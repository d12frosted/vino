;;; vino.el --- Cellar tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (vulpea "0.1") (org-roam "1.2.3"))
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
(require '+fun)


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
During large, heavy operations like `vino-db-build-cache', many
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


;;; Setup

(defun vino-setup ()
  "Setup `vino' library."
  (advice-add 'org-roam-db-build-cache
              :after
              #'vino-db-build-cache))


;;; Rating

;;;###autoload
(cl-defstruct (vino-rating
               (:constructor
                make-vino-rating
                (&key wine date version values
                      &aux
                      (score
                       (seq-reduce
                        #'+
                        (seq-map
                         (lambda (value)
                           (nth 1 value))
                         values)
                        0))
                      (score-max
                       (seq-reduce
                        #'+
                        (seq-map
                         (lambda (value)
                           (nth 2 value))
                         values)
                        0))
                      (total (* 10.0
                                (/ (float score)
                                   (float score-max)))))))
  (wine
   nil
   :type vino-entry
   :documentation "Associated `vino-entry'.")
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
   :documentation "Rating values based on `vino-rating-props'."))

;;;###autoload
(defvar vino-rating-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/rating/${id}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for rating entry.

Variables in the capture context are provided by
`vulpea-create'.")

(defvar vino-rating-props nil
  "Rating properties per version.

`vino-entry-rate' uses the latest element from list to read the
rating. Each rating contains the rating system version, so when
the rating is updated/refreshed `vino' knows what properties to
use and how to use them.

This variable is a list of all rating systems, stating with the
first version up to the current one. This variable has the
following format:

  '((1 . PROPS)
    (2 . PROPS)
    (3 . PROPS)
    ...)

And PROPS defines a specific version of rating system:

  ((\"PROP_1\" . PROP)
   (\"PROP_2\" . PROP)
   (\"PROP_3\" . PROP)
   ...)

Each PROP can be of one of the following types:

- NUMBER - then the property value is a number inclusively
  between 0 and PROP, user is prompted for a number using
  `read-number' during `vino-entry-rate';

- LIST - then the property value is a number inclusively between
  0 and the `length' of PROP, user is prompted to select one
  element from the list `car's using `completing-read' during
  `vino-entry-rate' and the `cdr' of selected element is used as
  value;

- FUNCTION - then the property value is a number between 0 and
  `cdr' of PROP result, function is called with without arguments
  during `vino-entry-rate' and `car' of the result is used as
  value.")

;;;###autoload
(defun vino-rating-get-by-id (note-or-id)
  "Get `vino-rating' represented by NOTE-OR-ID."
  (let ((note (if (stringp note-or-id)
                  (vulpea-db-get-by-id note-or-id)
                note-or-id)))
    (when (and note (vino-rating-note-p note))
      (when-let*
          ((meta (vulpea-meta note))
           (wine (vulpea-meta-get! meta "wine" 'note))
           (date (vulpea-meta-get! meta "date"))
           (version (vulpea-meta-get! meta "version" 'number))
           (info (seq-find
                  (lambda (x) (equal (car x) version))
                  vino-rating-props))
           (props (cdr info))
           (values (seq-map
                    (lambda (cfg)
                      (let ((name (downcase (car cfg))))
                        (list
                         name
                         (vulpea-meta-get!
                          meta name 'number)
                         (vulpea-meta-get!
                          meta (concat name "_max") 'number))))
                    props)))
        (make-vino-rating
         :wine wine
         :date date
         :version version
         :values values)))))

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

(defun vino-rating--read (props)
  "Read rating values from PROPS.

Each value is a list (name score score-max)."
  (seq-map
   (lambda (cfg)
     (cond
      ((functionp (cdr cfg))
       (let ((res (funcall (cdr cfg))))
         (list (car cfg)
               (car res)
               (cdr res))))

      ((numberp (cdr cfg))
       (list (car cfg)
             (read-number
              (format "%s: (0 to %i): "
                      (vino--format-prop (car cfg))
                      (cdr cfg)))
             (cdr cfg)))

      ((listp (cdr cfg))
       (let* ((ans (completing-read
                    (concat (vino--format-prop (car cfg))
                            ": ")
                    (seq-map #'car (cdr cfg))))
              (res (assoc ans (cdr cfg))))
         (list (car cfg)
               (cdr res)
               (- (length (cdr cfg)) 1))))))
   props))

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
       (note (vulpea-create
              title
              vino-rating-template
              (when id (list (cons 'id id))))))
    ;; TODO: performance of multiple `vulpea-meta-set'
    (vulpea-meta-set
     wine-note
     "ratings"
     (cons note
           (vulpea-meta-get-list wine-note "ratings" 'note))
     'append)
    (vulpea-meta-set note "wine" wine-note 'append)
    (vulpea-meta-set note "date" date-str 'append)
    (vulpea-meta-set note "version" version 'append)
    (seq-do (lambda (data)
              (vulpea-meta-set note
                               (downcase (nth 0 data))
                               (nth 1 data)
                               'append)
              (vulpea-meta-set note
                               (downcase (concat (nth 0 data) "_MAX"))
                               (nth 2 data)
                               'append))
            values)
    (vulpea-meta-set note "score" (vino-rating-score rating) 'append)
    (vulpea-meta-set
     note "score_max" (vino-rating-score-max rating) 'append)
    (vulpea-meta-set note "total" (vino-rating-total rating) 'append)
    (vino-db-update-rating rating note)
    (vino-entry-update wine-note)
    note))


;;; Entry

;;;###autoload
(defvar vino-entry-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/cellar/${id}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for wine entry.

Variables in the capture context are provided by
`vulpea-create'.")

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
         (name (+fun-repeat-while
                #'read-string
                #'string-empty-p
                "Name: "))
         (vintage (+fun-repeat-while
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
         (grapes (+fun-collect-while #'vino-grape-select nil))
         (alcohol (+fun-repeat-while
                   #'read-number
                   (lambda (v) (< v 0))
                   "Alcohol: "))
         (sugar (+fun-repeat-while
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
         (resources (+fun-collect-while
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
         :carbonation (vulpea-meta-get! meta "carbonation" 'symbol)
         :colour (vulpea-meta-get! meta "colour" 'symbol)
         :sweetness (vulpea-meta-get! meta "sweetness" 'symbol)
         :producer (vulpea-meta-get! meta "producer" 'note)
         :name (vulpea-meta-get! meta "name" 'string)
         :vintage (vino--parse-opt-number
                   (vulpea-meta-get! meta "vintage")
                   "NV")
         :appellation (vulpea-meta-get! meta "appellation" 'note)
         :region (vulpea-meta-get! meta "region" 'note)
         :grapes (vulpea-meta-get-list! meta "grapes" 'note)
         :alcohol (vulpea-meta-get! meta "alcohol" 'number)
         :sugar (vulpea-meta-get! meta "sugar" 'number)
         :acquired (vulpea-meta-get! meta "acquired" 'number)
         :consumed (vulpea-meta-get! meta "consumed" 'number)
         :resources (vulpea-meta-get-list! meta "resources" 'link)
         :price (vulpea-meta-get-list! meta "price" 'string)
         :rating (vino--parse-opt-number
                  (vulpea-meta-get! meta "rating" 'string)
                  "NA")
         :ratings (vulpea-meta-get-list! meta "ratings" 'note))))))

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
         (note (vulpea-create title
                              vino-entry-template
                              (when id
                                (list (cons 'id id))))))
    ;; TODO: optimize multiple calls
    (vulpea-meta-set
     note "carbonation" (vino-entry-carbonation vino) 'append)
    (vulpea-meta-set
     note "colour" (vino-entry-colour vino) 'append)
    (vulpea-meta-set
     note "sweetness" (vino-entry-sweetness vino) 'append)
    (vulpea-meta-set
     note "producer" (vino-entry-producer vino) 'append)
    (vulpea-meta-set
     note "name" (vino-entry-name vino) 'append)
    (when-let ((vintage (vino-entry-vintage vino)))
      (vulpea-meta-set note "vintage" vintage 'append))
    (when-let ((appellation (vino-entry-appellation vino)))
      (vulpea-meta-set note "appellation" appellation 'append))
    (when-let ((region (vino-entry-region vino)))
      (vulpea-meta-set note "region" region 'append))
    (vulpea-meta-set
     note "grapes" (vino-entry-grapes vino) 'append)
    (let ((alcohol (vino-entry-alcohol vino)))
      (when (and alcohol
                 (> alcohol 0))
        (vulpea-meta-set note "alcohol" alcohol 'append)))
    (let ((sugar (vino-entry-sugar vino)))
      (when (and sugar (>= sugar 0))
        (vulpea-meta-set note "sugar" sugar 'append)))
    (when (vino-entry-price vino)
      (vulpea-meta-set
       note "price" (vino-entry-price vino) 'append))
    (let ((acquired (or (vino-entry-acquired vino) 0))
          (consumed (or (vino-entry-consumed vino) 0)))
      (vulpea-meta-set note "acquired" acquired 'append)
      (vulpea-meta-set note "consumed" consumed 'append)
      (vulpea-meta-set
       note "available" (- acquired consumed) 'append))
    (vulpea-meta-set
     note "resources" (vino-entry-resources vino) 'append)
    (vulpea-meta-set
     note "rating" (or (vino-entry-rating vino) "NA") 'append)
    (vulpea-meta-set
     note "ratings" (vino-entry-ratings vino) 'append)
    (vino-db-update-entry vino note)
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
                     (+fun-collect-while #'vino-grape-select nil))))
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
                  (vulpea-meta-get! meta "producer" 'note))
                 (vulpea-meta-get! meta "name")
                 (vulpea-meta-get! meta "vintage"))))
    (vulpea-utils-with-note note
      (org-roam--set-global-prop "TITLE" title)
      (save-buffer))
    (vulpea-db-update note)
    (setq note (vulpea-db-get-by-id (vulpea-note-id note)))
    (vulpea-meta-set
     note
     "ratings"
     (seq-map
      (lambda (rn)
        (vulpea-utils-with-note rn
          (org-roam--set-global-prop
           "TITLE"
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
                     0
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
         (source (or source (read-string "Source: ")))
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
              (values (vino-rating--read props)))
    (vino-rating--create
     (make-vino-rating
      :wine note
      :date (format-time-string "%Y-%m-%d" date)
      :version version
      :values values))))


;;; Vino entry note

;;;###autoload
(defun vino-entry-note-p (note)
  "Return non-nil if NOTE represents vino entry."
  (when-let* ((tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "cellar"))))

;;;###autoload
(defun vino-entry-note-select ()
  "Select and return a `vulpea-note' representing `vino-entry'."
  (vulpea-select
   "Wine"
   :require-match t
   :filter-fn
   (lambda (note)
     (let ((tags (vulpea-note-tags note)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "cellar"))))))

;;;###autoload
(defun vino-entry-note-get-dwim (&optional note-or-id)
  "Get a `vulpea-note' representing `vino-entry' in a dwim style.

If NOTE-OR-ID is an ID, then get a note by that ID. Throws error
if extracted note does not represent a `vino-entry'.

If NOTE-OR-ID is a note, then return it. Throws error
if extracted note does not represent a `vino-entry'.

If NOTE-OR-ID is nil, try to extract a note from current buffer
or ask for user to select a note."
  (cond
   ((null note-or-id)
    (if (eq major-mode 'org-mode)
        (let* ((id (save-excursion
                     (goto-char (point-min))
                     (org-id-get)))
               (note (ignore-errors (vino-entry-note-get-dwim id))))
          (if note
              note
            (vino-entry-note-select)))
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
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for region entry.

Variables in the capture context are provided by
`vulpea-create'.")

;;;###autoload
(defvar vino-appellation-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/appellation/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for appellation entry.

Variables in the capture context are provided by
`vulpea-create'.")

;;;###autoload
(defun vino-region-create (&optional title)
  "Create a region note using `vino-region-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Region: "))))
    (vulpea-create title vino-region-template)))

;;;###autoload
(defun vino-appellation-create (&optional title)
  "Create a appellation note using `vino-appellation-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Appellation: "))))
    (vulpea-create title vino-appellation-template)))

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
         (vulpea-select
          "Region"
          :filter-fn
          (lambda (note)
            (let ((tags (vulpea-note-tags note)))
              (and (seq-contains-p tags "wine")
                   (or (seq-contains-p tags "appellation")
                       (seq-contains-p tags "region"))))))))
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
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for grape entry.

Variables in the capture context are provided by
`vulpea-create'.")

;;;###autoload
(defun vino-grape-create (&optional title)
  "Create a grape note using `vino-grape-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Grape: "))))
    (vulpea-create title vino-grape-template)))

;;;###autoload
(defun vino-grape-find-file ()
  "Select and find grape note."
  (interactive)
  (find-file (vulpea-note-path (vino-grape-select))))

;;;###autoload
(defun vino-grape-select ()
  "Select a grape note.

When grape note does not exist, it is created using
`vino-grape-create' if user decides to do so.

Return `vulpea-note'."
  (let ((note
         (vulpea-select
          "Grape"
          :filter-fn
          (lambda (note)
            (let ((tags (vulpea-note-tags note)))
              (and (seq-contains-p tags "wine")
                   (seq-contains-p tags "grape")))))))
    (if (vulpea-note-id note)
        note
      (if (y-or-n-p
           (format "Grape %s does not exist. Create it? "
                   (vulpea-note-title note)))
          (vino-grape-create (vulpea-note-title note))
        note))))


;;; Producers

;;;###autoload
(defvar vino-producer-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "wine/producer/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for producer entry.

Variables in the capture context are provided by
`vulpea-create'.")

;;;###autoload
(defun vino-producer-create (&optional title)
  "Create a producer note using `vino-producer-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Producer: "))))
    (vulpea-create title vino-producer-template)))

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
  (let ((note (vulpea-select
               "Producer"
               :filter-fn
               (lambda (note)
                 (let ((tags (vulpea-note-tags note)))
                   (and (seq-contains-p tags "wine")
                        (seq-contains-p tags "producer")))))))
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

(defvar vino-db--connection (make-hash-table :test #'equal)
  "Database connection to `vino' database.")

(defconst vino-db--version 1
  "Version of `vino' database.")

(defconst vino-db--schemata
  '((cellar
     [(id :unique :primary-key)
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
      (ratings)])
    (ratings
     [(id :unique :primary-key)
      (file :unique)
      (hash :not-null)
      (wine :not-null)
      (date :not-null)
      (version :not-null)
      (score :not-null)
      (score-max :not-null)
      (total :not-null)
      (values :not-null)])))

(defun vino-db-query (sql &rest args)
  "Run SQL query on `vino' database with ARGS.

SQL can be either the emacsql vector representation, or a
string."
  (if (stringp sql)
      (emacsql (vino-db) (apply #'format sql args))
    (apply #'emacsql (vino-db) sql args)))

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
    (make-vino-rating
     :wine (vulpea-db-get-by-id (nth 0 row))
     :date (nth 1 row)
     :version (nth 2 row)
     :values (nth 3 row))))

(defun vino-db ()
  "Entrypoint to the `vino' sqlite database.

Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (vino-db--get-connection)
               (emacsql-live-p (vino-db--get-connection)))
    (let ((init-db (not (file-exists-p vino-db-location))))
      (make-directory (file-name-directory vino-db-location) t)
      (let ((conn (emacsql-sqlite3 vino-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name vino-db-location)
                 conn
                 vino-db--connection)
        (when init-db
          (vino-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (vino-db--upgrade-maybe conn version)))
          (cond
           ((> version vino-db--version)
            (emacsql-close conn)
            (user-error
             "The database was created with a newer vino version.  "
             "You need to update the Org-roam package"))
           ((< version vino-db--version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  (vino-db--get-connection))

(defun vino-db--init (db)
  "Initialize DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) vino-db--schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s"
                        vino-db--version))))

(defun vino-db--get-connection ()
  "Return the database connection, if any."
  (gethash (expand-file-name vino-db-location)
           vino-db--connection))

(defun vino-db--close (&optional db)
  "Closes the database connection for database DB."
  (unless db
    (setq db (vino-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun vino-db--upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version vino-db--version)
        (progn
          (user-error "Not implemented"))))
  version)

(defun vino-db-build-cache (&optional force)
  "Build the cache for `vino'.

If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (vino-dlet ((agenda-files nil)
              (gc-cons-threshold vino-db-gc-threshold))
    (let ((t1 (current-time)))
      (when force (delete-file vino-db-location))
      (vino-db--close)
      (vino-db)
      (let* ((notes (vulpea-db-query
                     (lambda (n)
                       (let ((tags (vulpea-note-tags n)))
                         (and
                          (seq-contains-p tags "wine")
                          (or
                           (seq-contains-p tags "cellar")
                           (seq-contains-p tags "rating")))))))
             (entries-res (vino-db--build-cache
                           'cellar
                           (seq-filter
                            (lambda (n)
                              (seq-contains-p
                               (vulpea-note-tags n)
                               "cellar"))
                            notes)))
             (ratings-res (vino-db--build-cache
                           'ratings
                           (seq-filter
                            (lambda (n)
                              (seq-contains-p
                               (vulpea-note-tags n)
                               "rating"))
                            notes))))
        (message
         (concat
          "(vino)"
          " Cellar (total: %s, modified: %s, deleted: %s)"
          " Ratings (total: %s, modified: %s, deleted: %s)"
          " in %s ms")
         (plist-get entries-res :total)
         (plist-get entries-res :modified)
         (plist-get entries-res :deleted)
         (plist-get ratings-res :total)
         (plist-get ratings-res :modified)
         (plist-get ratings-res :deleted)
         (car (time-convert
               (time-subtract (current-time) t1)
               1000)))))))

;; TODO: remove with Emacs 27 support.
(if (fboundp 'dlet)
    (defalias 'vino-dlet #'dlet)
  (defmacro vino-dlet (binders &rest body)
    "Like `let*' but using dynamic scoping.

Dynamically bind the BINDERS and evaluate the BODY."
    (declare (indent 1) (debug let))
    ;; (defvar FOO) only affects the current scope, but in order for
    ;; this not to affect code after the `let*' we need to create a
    ;; new scope, which is what the surrounding `let' is for. FIXME:
    ;; (let () ...) currently doesn't actually create a new scope,
    ;; which is why we use (let (_) ...).
    `(let (_)
       ,@(mapcar (lambda (binder)
                   `(defvar ,(if (consp binder) (car binder) binder)))
                 binders)
       (let* ,binders ,@body))))

(defun vino-db--build-cache (table notes)
  "Build cache for TABLE from NOTES.

Return a property list (:total NUM :modified NUM :deleted NUM)"
  (let* ((current-notes (vino-db--get-current-notes table))
         (modified-notes nil)
         (deleted-count 0))
    (dolist (note notes)
      (let* ((contents-hash (vulpea-utils-note-hash note)))
        (unless (string=
                 (gethash (vulpea-note-id note)
                          current-notes)
                 contents-hash)
          (push (cons note contents-hash) modified-notes)))
      (remhash (vulpea-note-id note) current-notes))
    (dolist (note (hash-table-keys current-notes))
      ;; These notes are no longer around, remove from cache...
      (vino-db--clear-note table note)
      (setq deleted-count (1+ deleted-count)))
    (vino-db--update-notes table modified-notes)
    (list :total (length notes)
          :modified (length modified-notes)
          :deleted deleted-count)))

(defun vino-db--get-current-notes (table)
  "Return a hash-table of note id to the hash of its content.

Data is retrieved from TABLE."
  (let* ((rows (vino-db-query `[:select [id hash] :from ,table]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row rows)
      (puthash (nth 0 row) (nth 1 row) ht))
    ht))

(defun vino-db--update-notes (table notes)
  "Update TABLE cache for a list of modified NOTES.

Notes is a list of (note . hash) pairs."
  (pcase-dolist (`(,note . _) notes)
    (vino-db--clear-note table note))
  (let ((modified-count 0))
    (pcase-dolist (`(,note . ,hash) notes)
      (message "(vino) Processed %s/%s modified notes from %s..."
               modified-count
               (length notes)
               table)
      (vino-db--update-note table hash note)
      (setq modified-count (1+ modified-count)))))

(defun vino-db--update-note (table hash note)
  "Update TABLE cache for a NOTE with HASH."
  (let ((id (vulpea-note-id note)))
    (pcase table
      ('cellar (vino-db--update-entry
                (vino-entry-get-by-id id)
                note
                hash))
      ('ratings (vino-db--update-rating
                 (vino-rating-get-by-id id)
                 note
                 hash)))))

(defun vino-db-update-entry (entry note)
  "Update cache for ENTRY stored in NOTE."
  (vino-db--update-entry
   entry
   note
   (vulpea-utils-note-hash note)))

(defun vino-db--update-entry (entry note hash)
  "Update `vino' cache for ENTRY stored in NOTE.

HASH is SHA1 of NOTE file."
  (vino-db-query
   [:insert :into cellar
    :values $v1]
   (list
    (vector
     (vulpea-note-id note)
     (vulpea-note-path note)
     hash
     (vino-entry-carbonation entry)
     (vino-entry-colour entry)
     (vino-entry-sweetness entry)
     (vulpea-note-id (vino-entry-producer entry))
     (vino-entry-name entry)
     (vino-entry-vintage entry)
     (when-let ((n (vino-entry-appellation entry)))
       (vulpea-note-id n))
     (when-let ((n (vino-entry-region entry)))
       (vulpea-note-id n))
     (seq-map #'vulpea-note-id (vino-entry-grapes entry))
     (vino-entry-alcohol entry)
     (vino-entry-sugar entry)
     (vino-entry-resources entry)
     (vino-entry-price entry)
     (vino-entry-acquired entry)
     (vino-entry-consumed entry)
     (vino-entry-rating entry)
     (seq-map #'vulpea-note-id (vino-entry-ratings entry))))))

(defun vino-db-update-rating (rating note)
  "Update cache for RATING stored in NOTE."
  (vino-db--update-rating
   rating
   note
   (vulpea-utils-note-hash note)))

(defun vino-db--update-rating (rating note hash)
  "Update `vino' cache for RATING stored in NOTE.

HASH is SHA1 of NOTE file."
  (vino-db-query
   [:insert :into ratings
    :values $v1]
   (list
    (vector
     (vulpea-note-id note)
     (vulpea-note-path note)
     hash
     (vulpea-note-id (vino-rating-wine rating))
     (vino-rating-date rating)
     (vino-rating-version rating)
     (vino-rating-score rating)
     (vino-rating-score-max rating)
     (vino-rating-total rating)
     (vino-rating-values rating)))))

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
   (+fun-collect-while
    (lambda ()
      (let ((resource (read-string "Resource: ")))
        (when (not (string-empty-p resource))
          (vulpea-meta-format resource))))
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



(provide 'vino)
;;; vino.el ends here
