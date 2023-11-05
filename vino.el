;;; vino.el --- Cellar tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2022 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (vulpea "0.1") (org-roam "2.0.0"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; License: GPLv3
;;
;; Created: 09 Jan 2021
;;
;; URL: https://github.com/d12frosted/vino
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; It's your cellar, your dear cantina.
;;
;;; Code:

(require 'vulpea)
(require 'org-roam)


;;; Configurations variables

;;;###autoload
(defvar vino-carbonation-types
  '(still
    sparkling)
  "List of valid carbonation types.")

;;;###autoload
(defvar vino-colour-types
  '(red
    white
    rose)
  "List of valid colour types.

Orange wine is marked as white.")

;;;###autoload
(defvar vino-sweetness-levels
  (list 'still '(dry
                 semi-dry
                 semi-sweet
                 sweet)
        'sparkling '(brut-nature
                     extra-brut
                     brut
                     extra-dry
                     dry
                     demi-sec
                     doux))
  "List of valid sweetness levels per carbonation type.")

(defvar vino-db-location (expand-file-name
                          "vino.db"
                          user-emacs-directory)
  "The full path to file where the `vino' database is stored.

If this is non-nil, the `vino' sqlite database is saved here.")

(defvar vino-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `vino-db-sync', many
GC operations happen because of the large number of temporary
structures generated (e.g. parsed ASTs). Temporarily increasing
`gc-cons-threshold' will help reduce the number of GC operations,
at the cost of temporary memory usage.

This defaults to the original value of `gc-cons-threshold', but
tweaking this number may lead to better overall performance. For
example, to reduce the number of GCs, one may set it to a large
value like `most-positive-fixnum'.")


;;; Availability

(defvar vino-availability-fn nil
  "Function to check availability of `vino-entry'.

Function is called with ID of `vino-entry' and returns a cons of
acquired and consumed numbers.")

(defvar vino-availability-add-fn nil
  "Function to add AMOUNT of `vino-entry' to inventory.

Function is called with ID of `vino-entry', AMOUNT, SOURCE and
DATE arguments.")

(defvar vino-availability-sub-fn nil
  "Function to subtract AMOUNT of `vino-entry' from inventory.

Function is called with ID of `vino-entry', AMOUNT, ACTION and
DATE arguments.")

(defvar vino-sources-fn nil
  "Function to get the list of sources.

Function is called with ID of `vino-entry'.")


;;; Setup

(defun vino-setup ()
  "Setup `vino' library."
  (seq-each
   (lambda (x)
     (add-to-list 'org-tags-exclude-from-inheritance x))
   '("cellar"
     "rating"
     "producer"
     "grape"
     "region"
     "appellation"))

  ;; define tables
  (vulpea-db-define-table
   'cellar 1
   '([(id :unique :primary-key)
      (file :unique)
      (hash :not-null)
      (carbonation :not-null)
      (colour :not-null)
      (sweetness :not-null)
      (producer :not-null)
      (name :not-null)
      (vintage)
      (appellation)
      (region)
      (grapes)
      (alcohol :not-null)
      (sugar)
      (resources)
      (prices)
      (acquired :not-null)
      (consumed :not-null)
      (rating)
      (ratings)]
     (:foreign-key [id] :references nodes [id] :on-delete :cascade)))
  (vulpea-db-define-table
   'ratings 1
   '([(id :unique :primary-key)
      (file :unique)
      (hash :not-null)
      (wine :not-null)
      (date :not-null)
      (version :not-null)
      (score :not-null)
      (score-max :not-null)
      (total :not-null)
      (values :not-null)]
     (:foreign-key [id] :references nodes [id] :on-delete :cascade)))

  ;; setup required hooks
  (add-hook 'vulpea-db-insert-note-functions #'vino-db--insert-handle))


;;; Compat

;; Remove with support of Emacs 27.
(defmacro vino-dlet (binders &rest body)
  "Like `let*' but using dynamic scoping.

Dynamically bind the BINDERS and evaluate the BODY."
  (declare (indent 1) (debug let))
  ;; (defvar FOO) only affects the current scope, but in order for
  ;; this not to affect code after the `let*' we need to create a new
  ;; scope, which is what the surrounding `let' is for.
  ;; FIXME: (let () ...) currently doesn't actually create a new
  ;; scope, which is why we use (let (_) ...).
  `(let (_)
     ,@(mapcar (lambda (binder)
                 `(defvar ,(if (consp binder) (car binder) binder)))
               binders)
     (let* ,binders ,@body)))


;;; Rating

;;;###autoload
(cl-defstruct (vino-rating
               (:constructor
                make-vino-rating
                (&key wine date version values meta
                      &aux
                      (score (vino-rating--score values))
                      (score-max (vino-rating--score-max values))
                      (total (vino-rating--total score score-max)))))
  (wine
   nil
   :type vulpea-note
   :documentation "Associated wine note.")
  (date
   nil
   :type string
   :documentation "Rating date in format YYYY-MM-DD.")
  (version
   nil
   :type number
   :documentation "Rating version.")
  (score
   nil
   :type number
   :documentation "Resulting score of the rating based on VALUES.")
  (score-max
   nil
   :type number
   :documentation "Max score of the rating based on VALUES.")
  (total
   nil
   :type number
   :documentation "Normalised SCORE from 0 to 10.")
  (values
   nil
   :type list
   :documentation "Rating values based on `vino-rating-props'.")
  (meta
   nil
   :type list
   :documentation "Extra meta defined by `vino-rating-extra-meta'."))

(defun vino-rating--score (values)
  "Calculate score of a rating from VALUES."
  (seq-reduce
   #'+
   (seq-map
    (lambda (value)
      (nth 1 value))
    values)
   0))

(defun vino-rating--score-max (values)
  "Calculate max score of a rating from VALUES."
  (seq-reduce
   #'+
   (seq-map
    (lambda (value)
      (nth 2 value))
    values)
   0))

(defun vino-rating--total (score score-max)
  "Calculate total score of a rating from SCORE and SCORE-MAX."
  (* 10.0 (/ (float score)
             (float score-max))))

;;;###autoload
(defvar vino-rating-template
  (list :file-name "wine/rating/${id}.org")
  "Capture template for rating entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and rating

See `vulpea-create' for more information.")

(defvar vino-rating-props nil
  "Rating properties per version.

`vino-entry-rate' uses the latest element from list to read the
rating. Each rating contains the rating system version, so when
the rating is updated/refreshed `vino' knows what properties to
use and how to use them.

This variable is a list of all rating systems, stating with the
first version up to the current one. This variable has the
following format:

  ((1 . PROPS)
   (2 . PROPS)
   (3 . PROPS)
   ...)

And PROPS defines a specific version of rating system:

  ((\"PROP_1\" . PROP)
   (\"PROP_2\" . PROP)
   (\"PROP_3\" . PROP)
   ...)

Each PROP can be of one of the following types:

- NUMBER - then the property value is a number between 0 (inclusively)
  and PROP (exclusively), user is prompted for a number using
  `read-number' during `vino-entry-rate';

  Example: (\"prop_number\" . 10)

- LIST - then the property value is a number between 0 (inclusively)
  and the `length' of PROP (exclusively), user is prompted to select
  one element from the list `car's using `completing-read' during
  `vino-entry-rate' and the `cdr' of selected element is used as
  value;

  Example: (\"prop_list\" . ((\"max\" . 2)
                             (\"avg\" . 1)
                             (\"min\" . 0)))

- FUNCTION - then the property value is a number between 0 and `cdr'
  of PROP result, function is called with without arguments during
  `vino-entry-rate' and `car' of the result is used as value.

  Example: (\"prop_function\" . (lambda () (cons 42 100))")

(defvar vino-rating-extra-meta nil
  "Extra `vulpea-meta' associated with rating.

The value is a list, with each element being a plist of the
following form:

  (:name :read-fn :mode :type)

where

  :mode can be either single or multiple.

  :type is any type supported by `vulpea-meta-get'

This is not stored in `vino-db', but can be retrieved from
`vulpea-db' if you properly configured meta persistence.")

;;;###autoload
(defun vino-rating-get-by-id (note-or-id)
  "Get `vino-rating' represented by NOTE-OR-ID."
  (unless vino-rating-props
    (user-error "Expected vino-rating-props to be non-nil"))
  (let ((note (if (stringp note-or-id)
                  (vulpea-db-get-by-id note-or-id)
                note-or-id)))
    (when (and note (vino-rating-note-p note))
      (let*
          ((wine (vulpea-note-meta-get note "wine" 'note))
           (date (vulpea-note-meta-get note "date"))
           (version (vulpea-note-meta-get note "version" 'number))
           (info (seq-find
                  (lambda (x) (equal (car x) version))
                  vino-rating-props))
           (_ (unless info
                (user-error
                 "Could not find ratings props of version %s"
                 version)))
           (props (cdr info))
           (values (seq-map
                    (lambda (cfg)
                      (let ((name (downcase (car cfg))))
                        (list
                         name
                         (vulpea-note-meta-get
                          note name 'number)
                         (vulpea-note-meta-get
                          note (concat name "_max") 'number))))
                    props))
           (extra-meta (vino-rating--meta note)))
        (make-vino-rating
         :wine wine
         :date date
         :version version
         :values values
         :meta extra-meta)))))

;;;###autoload
(defun vino-rating-note-p (note)
  "Return non-nil if NOTE represents `vino-rating'."
  (when-let* ((tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "rating"))))

;;;###autoload
(defun vino-rating-update (note-or-id)
  "Refresh rating represented by NOTE-OR-ID."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id))
         (rating (vino-rating-get-by-id note)))
    (vulpea-meta-set note "score"
                     (vino-rating-score rating) 'append)
    (vulpea-meta-set note "score_max"
                     (vino-rating-score-max rating) 'append)
    (vulpea-meta-set note "total"
                     (vino-rating-total rating) 'append)))

(defun vino-rating--read-value (prop)
  "Read a rating value defined by PROP.

See `vino-rating-props' for specification.

Return a list (name score score-max)."
  (cond
   ((functionp (cdr prop))
    (let ((res (funcall (cdr prop))))
      (list (car prop)
            (car res)
            (cdr res))))

   ((numberp (cdr prop))
    (list (car prop)
          (min
           (cdr prop)
           (max
            0
            (read-number
             (format "%s: (0 to %i): "
                     (vino--format-prop (car prop))
                     (cdr prop)))))
          (cdr prop)))

   ((listp (cdr prop))
    (let* ((ans (completing-read
                 (concat (vino--format-prop (car prop))
                         ": ")
                 (seq-map #'car (cdr prop))))
           (res (assoc ans (cdr prop))))
      (list (car prop)
            (cdr res)
            (- (length (cdr prop)) 1))))))

(defun vino-rating--meta (note)
  "Extract meta from NOTE according to `vino-rating-extra-meta'."
  (seq-filter
   #'identity
   (seq-map
    (lambda (x)
      (let* ((name (plist-get x :name))
             (type (or (plist-get x :type)))
             (mode (or (plist-get x :mode)
                       'single))
             (value (pcase mode
                      (`single
                       (when-let ((v (vulpea-note-meta-get
                                      note name type)))
                         (list v)))
                      (`multiple
                       (vulpea-note-meta-get-list
                        note name type)))))
        (when value
          (cons name value))))
    vino-rating-extra-meta)))

(defun vino-rating--read-meta ()
  "Read extra meta defined by `vino-rating-extra-meta'."
  (seq-map
   (lambda (x)
     (let ((name (plist-get x :name))
           (read-fn (plist-get x :read-fn))
           (mode (or (plist-get x :mode)
                     'single)))
       (cons
        name
        (pcase mode
          (`single (list (funcall read-fn)))
          (`multiple (vino--collect-while read-fn nil))))))
   vino-rating-extra-meta))

(defun vino-rating--create (rating &optional id)
  "Create a note for RATING.

ID is generated unless passed."
  (when-let*
      ((wine-note (vino-rating-wine rating))
       (vino-entry (vino-entry-get-by-id wine-note))
       (producer (vino-entry-producer vino-entry))
       (date-str (vino-rating-date rating))
       (version (vino-rating-version rating))
       (values (vino-rating-values rating))
       (title (format "%s %s %s - %s"
                      (vulpea-note-title producer)
                      (vino-entry-name vino-entry)
                      (or (vino-entry-vintage vino-entry) "NV")
                      date-str))
       (body (with-temp-buffer
               ;; TODO: performance of multiple `vulpea-meta-set'
               (vulpea-buffer-meta-set "wine" wine-note 'append)
               (vulpea-buffer-meta-set "date" date-str 'append)
               (vulpea-buffer-meta-set "version" version 'append)
               (seq-do (lambda (data)
                         (vulpea-buffer-meta-set
                          (downcase (nth 0 data))
                          (nth 1 data)
                          'append)
                         (vulpea-buffer-meta-set
                          (downcase (concat (nth 0 data) "_MAX"))
                          (nth 2 data)
                          'append))
                       values)
               (vulpea-buffer-meta-set
                "score" (vino-rating-score rating) 'append)
               (vulpea-buffer-meta-set
                "score_max" (vino-rating-score-max rating) 'append)
               (vulpea-buffer-meta-set
                "total" (vino-rating-total rating) 'append)
               (seq-do (lambda (meta)
                         (vulpea-buffer-meta-set
                          (car meta) (cdr meta) 'append))
                       (vino-rating-meta rating))
               (buffer-substring (point-min)
                                 (point-max))))
       (note (vulpea-create
              title
              (plist-get vino-rating-template :file-name)
              :id id
              :tags (seq-union (plist-get vino-rating-template :tags)
                               '("wine" "rating"))
              :head (plist-get vino-rating-template :head)
              :body (concat (plist-get vino-rating-template :body)
                            body)
              :context (plist-get vino-rating-template :context)
              :properties (plist-get vino-rating-template :properties)
              :unnarrowed t
              :immediate-finish t)))
    (vulpea-meta-set
     wine-note
     "ratings"
     (cons note
           (vulpea-meta-get-list wine-note "ratings" 'note))
     'append)
    (vino-db-insert-note (vulpea-db-get-by-id (vulpea-note-id note)))
    (vino-entry-update wine-note)
    note))


;;; Entry

;;;###autoload
(defvar vino-entry-template
  (list :file-name "wine/cellar/${id}.org")
  "Capture template for wine entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and cellar

See `vulpea-create' for more information.")

;;;###autoload
(cl-defstruct vino-entry
  carbonation
  colour
  sweetness
  producer
  name
  vintage
  appellation
  region
  grapes
  alcohol
  sugar
  resources
  price
  acquired
  consumed
  rating
  ratings)

;;;###autoload
(defun vino-entry-find-file ()
  "Select and find vino note."
  (interactive)
  (find-file (vulpea-note-path (vino-entry-note-select))))

;;;###autoload
(defun vino-entry-read ()
  "Read a `vino-entry'."
  (let* ((producer (vino-producer-select))
         (name (vino--repeat-while
                #'read-string
                #'string-empty-p
                "Name: "))
         (vintage (vino--repeat-while
                   #'read-number
                   (lambda (v) (< v 1900))
                   "Vintage (C-g for NV): "))
         (rora (vino-region-select))
         (region (when (seq-contains-p (vulpea-note-tags rora)
                                       "region")
                   rora))
         (appellation (when (seq-contains-p (vulpea-note-tags rora)
                                            "appellation")
                        rora))
         (grapes (vino--collect-while #'vino-grape-select nil))
         (alcohol (vino--repeat-while
                   #'read-number
                   (lambda (v) (< v 0))
                   "Alcohol: "))
         (sugar (vino--repeat-while
                 #'read-number
                 (lambda (v) (< v 0))
                 "Sugar g/l (C-g for N/A): "))
         (colour (intern
                  (completing-read
                   "Colour: "
                   vino-colour-types
                   nil
                   'require-match)))
         (carbonation (intern
                       (completing-read
                        "Carbonation: "
                        vino-carbonation-types
                        nil
                        'require-match)))
         (sweetness (intern
                     (completing-read
                      "Sweetness:"
                      (plist-get vino-sweetness-levels
                                 carbonation)
                      nil
                      'require-match)))
         (resources (vino--collect-while
                     #'read-string
                     (lambda (v) (not (string-empty-p v)))
                     "Resource: "))
         (price (read-string "Price: ")))
    (make-vino-entry
     :carbonation carbonation
     :colour colour
     :sweetness sweetness
     :producer producer
     :name name
     :vintage vintage
     :appellation appellation
     :region region
     :grapes grapes
     :alcohol alcohol
     :sugar sugar
     :resources resources
     :price price
     :acquired 0
     :consumed 0
     :rating nil
     :ratings nil)))

;;;###autoload
(defun vino-entry-get-by-id (note-or-id)
  "Get `vino-entry' by NOTE-OR-ID."
  (let ((note (if (stringp note-or-id)
                  (vulpea-db-get-by-id note-or-id)
                note-or-id)))
    (when (and note (vino-entry-note-p note))
      (let ((meta (vulpea-meta note)))
        (make-vino-entry
         :carbonation (vulpea-buffer-meta-get!
                       meta "carbonation" 'symbol)
         :colour (vulpea-buffer-meta-get! meta "colour" 'symbol)
         :sweetness (vulpea-buffer-meta-get!
                     meta "sweetness" 'symbol)
         :producer (if-let ((producer (vulpea-buffer-meta-get!
                                       meta "producer" 'note)))
                       producer
                     (lwarn 'vino :error
                            "Producer is not an existing note in entry %s"
                            (vulpea-note-id note)))
         :name (vulpea-buffer-meta-get! meta "name" 'string)
         :vintage (vino--parse-opt-number
                   (vulpea-buffer-meta-get! meta "vintage")
                   "NV")
         :appellation (vulpea-buffer-meta-get!
                       meta "appellation" 'note)
         :region (vulpea-buffer-meta-get! meta "region" 'note)
         :grapes (vulpea-buffer-meta-get-list! meta "grapes" 'note)
         :alcohol (vulpea-buffer-meta-get! meta "alcohol" 'number)
         :sugar (vulpea-buffer-meta-get! meta "sugar" 'number)
         :acquired (vulpea-buffer-meta-get! meta "acquired" 'number)
         :consumed (vulpea-buffer-meta-get! meta "consumed" 'number)
         :resources (vulpea-buffer-meta-get-list!
                     meta "resources" 'link)
         :price (vulpea-buffer-meta-get-list! meta "price" 'string)
         :rating (vino--parse-opt-number
                  (vulpea-buffer-meta-get! meta "rating" 'string)
                  "NA")
         :ratings (vulpea-buffer-meta-get-list!
                   meta "ratings" 'note))))))

;;;###autoload
(defun vino-entry-create ()
  "Create a `vino-entry'."
  (interactive)
  (let ((note (vino-entry--create (vino-entry-read))))
    (when (y-or-n-p "Acquire? ")
      (vino-entry-acquire note))))

(defun vino-entry--create (vino &optional id)
  "Create an entry for VINO.

ID is generated unless passed."
  (let* ((producer (vino-entry-producer vino))
         (producer (if (vulpea-note-p producer)
                       producer
                     (vulpea-db-get-by-id producer)))
         (vintage (vino-entry-vintage vino))
         (title (concat (vulpea-note-title producer)
                        " "
                        (vino-entry-name vino)
                        " "
                        (if (numberp vintage)
                            (number-to-string vintage)
                          vintage)))
         (body
          (with-temp-buffer
            ;; TODO: optimize multiple calls
            (vulpea-buffer-meta-set
             "carbonation" (vino-entry-carbonation vino) 'append)
            (vulpea-buffer-meta-set
             "colour" (vino-entry-colour vino) 'append)
            (vulpea-buffer-meta-set
             "sweetness" (vino-entry-sweetness vino) 'append)
            (vulpea-buffer-meta-set
             "producer" (vino-entry-producer vino) 'append)
            (vulpea-buffer-meta-set
             "name" (vino-entry-name vino) 'append)
            (when-let ((vintage (vino-entry-vintage vino)))
              (vulpea-buffer-meta-set "vintage" vintage 'append))
            (when-let ((appellation (vino-entry-appellation vino)))
              (vulpea-buffer-meta-set
               "appellation" appellation 'append))
            (when-let ((region (vino-entry-region vino)))
              (vulpea-buffer-meta-set "region" region 'append))
            (vulpea-buffer-meta-set
             "grapes" (vino-entry-grapes vino) 'append)
            (let ((alcohol (vino-entry-alcohol vino)))
              (when (and alcohol
                         (> alcohol 0))
                (vulpea-buffer-meta-set "alcohol" alcohol 'append)))
            (let ((sugar (vino-entry-sugar vino)))
              (when (and sugar (>= sugar 0))
                (vulpea-buffer-meta-set "sugar" sugar 'append)))
            (when (vino-entry-price vino)
              (vulpea-buffer-meta-set
               "price" (vino-entry-price vino) 'append))
            (let ((acquired (or (vino-entry-acquired vino) 0))
                  (consumed (or (vino-entry-consumed vino) 0)))
              (vulpea-buffer-meta-set "acquired" acquired 'append)
              (vulpea-buffer-meta-set "consumed" consumed 'append)
              (vulpea-buffer-meta-set
               "available" (- acquired consumed) 'append))
            (vulpea-buffer-meta-set
             "resources" (vino-entry-resources vino) 'append)
            (vulpea-buffer-meta-set
             "rating" (or (vino-entry-rating vino) "NA") 'append)
            (vulpea-buffer-meta-set
             "ratings" (vino-entry-ratings vino) 'append)
            (buffer-substring (point-min)
                              (point-max))))
         (note
          (vulpea-create
           title
           (plist-get vino-entry-template :file-name)
           :id id
           :tags (seq-union (plist-get vino-entry-template :tags)
                            '("wine" "cellar"))
           :head (plist-get vino-entry-template :head)
           :body (concat (plist-get vino-entry-template :body)
                         body)
           :context (plist-get vino-entry-template :context)
           :properties (plist-get vino-entry-template :properties)
           :unnarrowed t
           :immediate-finish t)))
    (vino-db-insert-note note)
    note))

;;;###autoload
(defun vino-entry-set-grapes (&optional note-or-id grapes)
  "Set GRAPES to `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (grapes (or grapes
                     (vino--collect-while #'vino-grape-select nil))))
    (vulpea-meta-set note "grapes" grapes 'append)))

;;;###autoload
(defun vino-entry-set-region (&optional note-or-id region)
  "Set REGION to `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

REGION may be either region or appellation."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (region (cond
                  ((stringp region) (vulpea-db-get-by-id region))
                  ((vulpea-note-p region) region)
                  (t (vino-region-select)))))
    (when (seq-contains-p (vulpea-note-tags region) "region")
      (vulpea-meta-remove note "appellation")
      (vulpea-meta-set note "region" region 'append))
    (when (seq-contains-p (vulpea-note-tags region) "appellation")
      (vulpea-meta-remove note "region")
      (vulpea-meta-set note "appellation" region 'append))
    ;; TODO: sort metadata
    ))

;;;###autoload
(defun vino-entry-update (&optional note-or-id)
  "Update `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

The following things are updated:

- total score of each linked ratings;
- rating of the `vino-entry';
- availability of `vino-entry'."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id)))
    (vino-entry-update-rating note)
    (vino-entry-update-availability note)))

;;;###autoload
(defun vino-entry-update-title (&optional note-or-id)
  "Update title of `vino-entry'..

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

The following things are updated:

- link description of producer;
- title of the `vino-entry';
- title of every linked rating."
  (interactive)
  ;; TODO: vulpea-meta performance
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (meta (vulpea-meta note))
         (title (format
                 "%s %s %s"
                 (vulpea-note-title
                  (vulpea-buffer-meta-get! meta "producer" 'note))
                 (vulpea-buffer-meta-get! meta "name")
                 (or (vulpea-buffer-meta-get! meta "vintage") "NV"))))
    (vulpea-utils-with-note note
      (vulpea-buffer-title-set title)
      (save-buffer))
    (vulpea-db-update note)
    (setq note (vulpea-db-get-by-id (vulpea-note-id note)))
    (vulpea-meta-set
     note
     "ratings"
     (seq-map
      (lambda (rn)
        (vulpea-utils-with-note rn
          (vulpea-buffer-title-set
           (format "%s - %s"
                   title
                   (vulpea-meta-get rn "date")))
          (save-buffer))
        (vulpea-db-update rn)
        (vulpea-meta-set rn "wine" note)
        (vulpea-db-get-by-id (vulpea-note-id rn)))
      (vulpea-meta-get-list note "ratings" 'note)))))

;;;###autoload
(defun vino-entry-update-rating (note-or-id)
  "Update rating metadata of `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

- total score of each linked ratings;
- rating of the `vino-entry'."
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (ratings (vulpea-meta-get-list note "ratings" 'note))
         (values (seq-map
                  (lambda (rn)
                    (vino-rating-update rn)
                    (vulpea-meta-get rn "total" 'number))
                  ratings))
         (rating (if (null values)
                     "NA"
                   (/ (apply #'+ values)
                      (float (length values))))))
    (vulpea-meta-set note "rating" rating 'append)
    (vulpea-meta-set
     note
     "ratings"
     (seq-sort-by #'vulpea-note-title
                  #'string<
                  ratings)
     'append)))

;;;###autoload
(defun vino-entry-update-availability (note-or-id)
  "Update available metadata of `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly."
  (unless vino-availability-fn
    (user-error "`vino-availability-fn' is nil"))
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (res (funcall vino-availability-fn (vulpea-note-id note)))
         (in (car res))
         (out (cdr res))
         (cur (- in out)))
    (vulpea-meta-set note "acquired" in 'append)
    (vulpea-meta-set note "consumed" out 'append)
    (vulpea-meta-set note "available" cur 'append)))

;;;###autoload
(defun vino-entry-acquire (&optional note-or-id
                                     amount source price date)
  "Acquire AMOUNT of `vino-entry' from SOURCE for PRICE at DATE.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (id (vulpea-note-id note))
         (vino (vino-entry-get-by-id id))
         (source (or source
                     (if vino-sources-fn
                         (completing-read
                          "Source: "
                          (funcall vino-sources-fn id))
                       (read-string "Source: "))))
         (amount (or amount (read-number "Amount: " 1)))
         (price (or price (vino-price-read vino)))
         (date (or date (org-read-date nil t))))
    (funcall vino-availability-add-fn
             id amount source date)
    (let ((prices (vino-entry-price vino)))
      (unless (seq-contains-p prices price)
        (vulpea-meta-set note "price" (cons price prices))))
    (vino-entry-update-availability id)))

;;;###autoload
(defun vino-entry-consume (&optional note-or-id amount action date)
  "Consume AMOUNT of `vino-entry' because of ACTION at DATE.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (id (vulpea-note-id note))
         (action (or action (read-string "Action: " "consume")))
         (amount (or amount
                     (read-number
                      "Amount: "
                      (or (min 1.0 (vulpea-meta-get
                                    note "available" 'number))
                          1))))
         (date (or date (org-read-date nil t))))
    (funcall vino-availability-sub-fn
             id amount action date)
    (vino-entry-update-availability id)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate? "))
      (vino-entry-rate id date))))

;;;###autoload
(defun vino-entry-rate (&optional note-or-id date)
  "Rate a `vino-entry' on DATE.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly."
  (interactive)
  (when-let* ((note (vino-entry-note-get-dwim note-or-id))
              (date (or date (org-read-date nil t)))
              (info (car (last vino-rating-props)))
              (version (car info))
              (props (cdr info))
              (values (seq-map #'vino-rating--read-value props))
              (extra-meta (vino-rating--read-meta)))
    (vino-rating--create
     (make-vino-rating
      :wine note
      :date (format-time-string "%Y-%m-%d" date)
      :version version
      :values values
      :meta extra-meta))))


;;; Vino entry note

;;;###autoload
(defun vino-entry-note-p (note)
  "Return non-nil if NOTE represents vino entry."
  (when-let ((tags (vulpea-note-tags note))
             (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "cellar"))))

;;;###autoload
(defun vino-entry-note-select ()
  "Select and return a `vulpea-note' representing `vino-entry'."
  (vulpea-select-from
   "Wine"
   (vulpea-db-query-by-tags-every '("wine" "cellar"))
   :require-match t))

;;;###autoload
(defun vino-entry-note-get-dwim (&optional note-or-id)
  "Get a `vulpea-note' representing `vino-entry' in a dwim style.

If NOTE-OR-ID is an ID, then get a note by that ID. Throws error
if extracted note does not represent a `vino-entry'.

If NOTE-OR-ID is a note, then return it. Throws error
if extracted note does not represent a `vino-entry'.

If NOTE-OR-ID is nil, try to extract a note from current buffer
or ask for user to select a note. Extractions happens from
`vino-entry' buffer or from `vino-rating' buffer. In the latter
case, linked `vino-entry' is extracted."
  (cond
   ((null note-or-id)
    (if (eq major-mode 'org-mode)
        (let* ((id (save-excursion
                     (goto-char (point-min))
                     (org-id-get)))
               (note (vulpea-db-get-by-id id)))
          (cond
           ((vino-entry-note-p note) (vino-entry-note-get-dwim note))
           ((vino-rating-note-p note)
            (vino-rating-wine (vino-rating-get-by-id id)))
           (t (vino-entry-note-select))))
      (vino-entry-note-select)))
   ((stringp note-or-id)
    (let ((note (vulpea-db-get-by-id note-or-id)))
      (if (and note (vino-entry-note-p note))
          note
        (user-error
         "Note with id %s does not represent a vino entry"
         note-or-id))))
   ((vulpea-note-p note-or-id)
    (if (vino-entry-note-p note-or-id)
        note-or-id
      (user-error
       "Note %s does not represent a vino entry"
       note-or-id)))
   (t
    (user-error "Unsupported type of %s" note-or-id))))


;;; Regions and appellations

;;;###autoload
(defvar vino-region-template
  (list :file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}.org")
  "Capture template for region entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and region

See `vulpea-create' for more information.")

;;;###autoload
(defvar vino-appellation-template
  (list :file-name "wine/appellation/%<%Y%m%d%H%M%S>-${slug}.org")
  "Capture template for appellation entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and
  appellation

See `vulpea-create' for more information.")

;;;###autoload
(defun vino-region-create (&optional title)
  "Create a region note using `vino-region-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Region: "))))
    (vulpea-create
     title
     (plist-get vino-region-template :file-name)
     :tags (seq-union (plist-get vino-region-template :tags)
                      '("wine" "region"))
     :head (plist-get vino-region-template :head)
     :body (plist-get vino-region-template :body)
     :context (plist-get vino-region-template :context)
     :properties (plist-get vino-region-template :properties)
     :unnarrowed t
     :immediate-finish t)))

;;;###autoload
(defun vino-appellation-create (&optional title)
  "Create a appellation note using `vino-appellation-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Appellation: "))))
    (vulpea-create
     title
     (plist-get vino-appellation-template :file-name)
     :tags (seq-union (plist-get vino-appellation-template :tags)
                      '("wine" "appellation"))
     :head (plist-get vino-appellation-template :head)
     :body (plist-get vino-appellation-template :body)
     :context (plist-get vino-appellation-template :context)
     :properties (plist-get vino-appellation-template :properties)
     :unnarrowed t
     :immediate-finish t)))

;;;###autoload
(defun vino-region-find-file ()
  "Select and find region note."
  (interactive)
  (find-file (vulpea-note-path (vino-region-select))))

;;;###autoload
(defun vino-region-select ()
  "Select a wine region or appellation note.

When region or appellation note does not exist, it is created
using `vino-region-create' or `vino-appellation-create' depending
on user decision.

Return `vulpea-note'."
  (let ((note
         (vulpea-select-from
          "Region"
          (seq-filter
           (lambda (note)
             (seq-contains-p (vulpea-note-tags note) "wine"))
           (vulpea-db-query-by-tags-some
            '("appellation" "region"))))))
    (if (vulpea-note-id note)
        note
      (pcase (completing-read
              (format "Region %s does not exist. What to do?"
                      (vulpea-note-title note))
              '("Create region"
                "Create appellation"
                "Ignore"))
        (`"Create region"
         (vino-region-create (vulpea-note-title note)))
        (`"Create appellation"
         (vino-appellation-create (vulpea-note-title note)))
        (_ note)))))


;;; Grapes

;;;###autoload
(defvar vino-grape-template
  (list :file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}.org")
  "Capture template for grape entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and grape

See `vulpea-create' for more information.")

;;;###autoload
(defun vino-grape-create (&optional title)
  "Create a grape note using `vino-grape-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Grape: "))))
    (vulpea-create
     title
     (plist-get vino-grape-template :file-name)
     :tags (seq-union (plist-get vino-grape-template :tags)
                      '("wine" "grape"))
     :head (plist-get vino-grape-template :head)
     :body (plist-get vino-grape-template :body)
     :context (plist-get vino-grape-template :context)
     :properties (plist-get vino-grape-template :properties)
     :unnarrowed t
     :immediate-finish t)))

;;;###autoload
(defun vino-grape-find-file ()
  "Select and find grape note."
  (interactive)
  (find-file (vulpea-note-path (vino-grape-select))))

;;;###autoload
(defun vino-grape-select ()
  "Select a grape note.

When grape note does not exist, it may be created using
`vino-grape-create' or added as synonym to other grape if user
decides to do so.

Return `vulpea-note'."
  (let ((note
         (vulpea-select-from
          "Grape"
          (vulpea-db-query-by-tags-every '("wine" "grape")))))
    (if (vulpea-note-id note)
        note
      (pcase (completing-read
              (format "Grape %s does not exist. What to do?"
                      (vulpea-note-title note))
              '("Create new grape"
                "Add a synonym to existing grape"
                "Ignore"))
        (`"Create new grape"
         (vino-grape-create (vulpea-note-title note)))
        (`"Add a synonym to existing grape"
         (let ((base (vulpea-select-from
                      "Original grape"
                      (vulpea-db-query-by-tags-every
                       '("wine" "grape")))))
           (vulpea-utils-with-note base
             (org-roam-alias-add (vulpea-note-title note))
             (org-roam-db-update-file
              (buffer-file-name (buffer-base-buffer))))
           (setf (vulpea-note-title base) (vulpea-note-title note))
           base))
        (_ note)))))


;;; Producers

;;;###autoload
(defvar vino-producer-template
  (list :file-name "wine/producer/%<%Y%m%d%H%M%S>-${slug}.org")
  "Capture template for producer entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `org-roam-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and producer

See `vulpea-create' for more information.")

;;;###autoload
(defun vino-producer-create (&optional title)
  "Create a producer note using `vino-producer-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Producer: "))))
    (vulpea-create
     title
     (plist-get vino-producer-template :file-name)
     :tags (seq-union (plist-get vino-producer-template :tags)
                      '("wine" "producer"))
     :head (plist-get vino-producer-template :head)
     :body (plist-get vino-producer-template :body)
     :context (plist-get vino-producer-template :context)
     :properties (plist-get vino-producer-template :properties)
     :unnarrowed t
     :immediate-finish t)))

;;;###autoload
(defun vino-producer-find-file ()
  "Select and find producer note."
  (interactive)
  (find-file (vulpea-note-path (vino-producer-select))))

;;;###autoload
(defun vino-producer-select ()
  "Select a producer note.

When producer note does not exist, it is created using
`vino-producer-create' if user decides to do so.

Return `vulpea-note'."
  (let ((note (vulpea-select-from
               "Producer"
               (vulpea-db-query-by-tags-every '("wine" "producer")))))
    (if (vulpea-note-id note)
        note
      (if (y-or-n-p
           (format "Producer %s does not exist. Create it? "
                   (vulpea-note-title note)))
          (vino-producer-create (vulpea-note-title note))
        note))))


;;; Price

;;;###autoload
(defun vino-price-read (vino)
  "Read the price for VINO entry."
  (if-let ((price (car (vino-entry-price vino))))
      (read-string (format "Price (default %s): " price)
                   nil nil price)
    (read-string "Price: ")))


;;; Database

(defalias 'vino-db-query #'org-roam-db-query)

(defun vino-db-get-entry (id)
  "Get `vino-entry' by ID from db."
  (when-let
      ((row (car-safe
             (vino-db-query
              [:select [carbonation     ; 0
                        colour          ; 1
                        sweetness       ; 2
                        producer        ; 3
                        name            ; 4
                        vintage         ; 5
                        appellation     ; 6
                        region          ; 7
                        grapes          ; 8
                        alcohol         ; 9
                        sugar           ; 10
                        resources       ; 11
                        prices          ; 12
                        acquired        ; 13
                        consumed        ; 14
                        rating          ; 15
                        ratings]        ; 16
               :from cellar
               :where (= id $s1)]
              id))))
    (make-vino-entry
     :carbonation (nth 0 row)
     :colour (nth 1 row)
     :sweetness (nth 2 row)
     :producer (vulpea-db-get-by-id (nth 3 row))
     :name (nth 4 row)
     :vintage (nth 5 row)
     :appellation (vulpea-db-get-by-id (nth 6 row))
     :region (vulpea-db-get-by-id (nth 7 row))
     :grapes (seq-map #'vulpea-db-get-by-id (nth 8 row))
     :alcohol (nth 9 row)
     :sugar (nth 10 row)
     :resources (nth 11 row)
     :price (nth 12 row)
     :acquired (nth 13 row)
     :consumed (nth 14 row)
     :rating (nth 15 row)
     :ratings (seq-map #'vulpea-db-get-by-id (nth 16 row)))))

(defun vino-db-get-rating (id)
  "Get `vino-rating' by ID from db."
  (when-let
      ((row (car-safe
             (vino-db-query
              [:select [wine            ; 0
                        date            ; 1
                        version         ; 2
                        values]         ; 3
               :from ratings
               :where (= id $s1)]
              id))))
    (let* ((wine-note (vulpea-db-get-by-id (nth 0 row)))
           (rating-note (vulpea-db-get-by-id id))
           (meta (vino-rating--meta rating-note)))
      (make-vino-rating
       :wine wine-note
       :date (nth 1 row)
       :version (nth 2 row)
       :values (nth 3 row)
       :meta meta))))

(defun vino-db-insert-note (note)
  "Insert NOTE to `vino' tables in `vulpea-db'."
  (cond
   ((vino-entry-note-p note)
    (vino-db--clear-note 'cellar note)
    (vino-db-query
     [:insert :into cellar
      :values $v1]
     (list
      (vector
       (vulpea-note-id note)
       (vulpea-note-path note)
       (vulpea-utils-note-hash note)
       (vulpea-note-meta-get note "carbonation" 'symbol)
       (vulpea-note-meta-get note "colour" 'symbol)
       (vulpea-note-meta-get note "sweetness" 'symbol)
       (vulpea-note-meta-get note "producer" 'link)
       (vulpea-note-meta-get note "name")
       (vino--parse-opt-number (vulpea-note-meta-get note "vintage") "NV")
       (vulpea-note-meta-get note "appellation" 'link)
       (vulpea-note-meta-get note "region" 'link)
       (vulpea-note-meta-get-list note "grapes" 'link)
       (vulpea-note-meta-get note "alcohol" 'number)
       (vulpea-note-meta-get note "sugar" 'number)
       (vulpea-note-meta-get-list note "resources" 'link)
       (vulpea-note-meta-get-list note "price")
       (vulpea-note-meta-get note "acquired" 'number)
       (vulpea-note-meta-get note "consumed" 'number)
       (vino--parse-opt-number (vulpea-note-meta-get note "rating") "NA")
       (vulpea-note-meta-get-list note "ratings" 'link)))))

   ((vino-rating-note-p note)
    (vino-db--clear-note 'ratings note)
    (let* ((version (vulpea-note-meta-get note "version" 'number))
           (info (seq-find
                  (lambda (x) (equal (car x) version))
                  vino-rating-props))
           (_ (unless info
                (user-error
                 "Could not find ratings props of version %s"
                 version)))
           (props (cdr info))
           (values (seq-map
                    (lambda (cfg)
                      (let ((name (downcase (car cfg))))
                        (list
                         name
                         (vulpea-note-meta-get
                          note name 'number)
                         (vulpea-note-meta-get
                          note (concat name "_max") 'number))))
                    props))
           (score (vino-rating--score values))
           (score-max (vino-rating--score-max values)))
      (vino-db-query
       [:insert :into ratings
        :values $v1]
       (list
        (vector
         (vulpea-note-id note)
         (vulpea-note-path note)
         (vulpea-utils-note-hash note)
         (vulpea-note-meta-get note "wine" 'link)
         (vulpea-note-meta-get note "date")
         version
         score
         score-max
         (vino-rating--total score score-max)
         values)))))))

(defun vino-db--insert-handle (note)
  "Handle NOTE insertion to `vulpea-db'."
  (vino-db-insert-note note))

(defun vino-db--clear-note (table note)
  "Remove any db information from TABLE related to NOTE."
  (when-let ((id (if (stringp note) note (vulpea-note-id note))))
    (vino-db-query
     `[:delete :from ,table
       :where (= id $s1)]
     id)))


;;; Utilities

;;;###autoload
(defun vino-resources-template ()
  "Query for resource URL and return it as a meta string."
  (seq-reduce
   (lambda (r a)
     (concat r "- resources :: " a "\n"))
   (vino--collect-while
    (lambda ()
      (let ((resource (read-string "Resource: ")))
        (when (not (string-empty-p resource))
          (vulpea-buffer-meta-format resource))))
    (lambda (a) (not (null a))))
   ""))

(defun vino--format-prop (prop)
  "Create a pretty prompt from PROP."
  (capitalize (replace-regexp-in-string "_" " " prop)))

(defun vino--parse-opt-number (str nil-str)
  "Parse an optional number from STR.

If STR is equal to NIL-STR, then nil is the result."
  (when (and str (not (string-equal str nil-str)))
    (string-to-number str)))



(defun vino--collect-while (fn filter &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

If FILTER is nil, it does not affect repeat cycle.

If FILTER returns nil, the computed value is not added to result.

ARGS are passed to FN."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (if (and filter
                 (null (funcall filter value)))
            (setq continue nil)
          (setq result (cons value result)))))
    (setq quit-flag nil)
    (seq-reverse result)))

(defun vino--repeat-while (fn filter &rest args)
  "Repeat FN and return the first unfiltered result.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

ARGS are passed to FN."
  (let (value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (when (null (funcall filter value))
          (setq continue nil))))
    (setq quit-flag nil)
    (when (null continue)
      value)))



(provide 'vino)
;;; vino.el ends here
