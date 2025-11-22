;;; vino.el --- Cellar tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2022 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1") (vulpea "2.0.0") (dash "2.19.1") (s "1.13.0"))
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

;;; Configurations variables
;;

;;;###autoload
(defvar vino-carbonation-types
  '(still
    sparkling)
  "List of valid carbonation types.")

;;;###autoload
(defvar vino-carbonation-methods
  '(traditional
    charmat
    ancestral
    transfer
    injection
    unknown)
  "List of valid carbonation methods.")

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

;;; Hooks
;;

(defvar vino-entry-create-handle-functions nil
  "Abnormal hooks to run after vino entry is created.

Each function accepts a `vulpea-note'.")

(defvar vino-entry-update-handle-functions nil
  "Abnormal hooks to run after vino rating is updated.

Each function accepts a `vulpea-note'.")

(defvar vino-rating-create-handle-functions nil
  "Abnormal hooks to run after vino rating is created.

Each function accepts a `vulpea-note' and extra-data passed to
`vino-entry-rate'.")

;;; Selection config
;;

(defvar vino-origin-select-fn #'vino-origin-select-by-country
  "Interactive function to select wine's origin.

The function doesn't receive any arguments and must return an alist. The
expectation is that resulting alist contains country and optionally it
may include region and/or appellation. The function may return other
keys as well, but they don't bear any meaning for vino library - this is
used only for custom extensions.

Vino comes with two functions that can be used as the value:

- `vino-origin-select-flat' - it just lists union of all regions and
  appellations, and when user picks

    region, it returns (country region) where country is country
    meta value of the selected region

    appellation, it returns (country appellation) where country is
    country meta value of the selected appellation;

- `vino-origin-select-by-country' - this is a multi-step selection,
   where user is asked to pick a country first; and then is presented
   with a union of all regions and appellations that have country meta
   value link to selected country.

You can implement your own, more sophisticated selection function based
on your situation. For example, you can pick extra routes based on the
country.")

;;; Setup
;;

(defun vino-setup ()
  "Setup `vino' library."
  (seq-each
   (lambda (x)
     (add-to-list 'org-tags-exclude-from-inheritance x))
   '("cellar"
     "rating"
     "producer"
     "grape"
     "country"
     "region"
     "appellation")))

;;; Rating
;;

;;;###autoload
(cl-defstruct (vino-rating
               (:constructor
                make-vino-rating
                (&key wine date version values
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
   :documentation "Rating values based on `vino-rating-props'."))

(defvar vino-rating-scale 5.0
  "Scale used for rating calculations.

There are a few commonly used scales:

- 5.0 - as used in applications like Vivino;
- 10.0 - as used in Italian schools;
- 20.0 - as used by Jancis Robinson;
- 100 - as used by many professionals.")

(defvar vino-rating-precision 1
  "Precision used for rating calculations.

When set to number, all ratings are __rounded__ to N digits after
point.

When the value is nil, no rounding happens.")

(defvar vino-rating-precision--fmt nil)

(defun vino-rating--round (num)
  "Round NUM using `vino-rating-precision'."
  (when (numberp vino-rating-precision)
    (unless vino-rating-precision--fmt
      (setq vino-rating-precision--fmt
            (concat "%." (number-to-string vino-rating-precision) "f"))))
  (if vino-rating-precision--fmt
      (string-to-number (format vino-rating-precision--fmt num))
    num))

(defun vino-rating--score (values)
  "Calculate score of a rating from VALUES."
  (vino-rating--round
   (seq-reduce
    #'+
    (seq-map
     (lambda (value)
       (nth 1 value))
     values)
    0)))

(defun vino-rating--score-max (values)
  "Calculate max score of a rating from VALUES."
  (vino-rating--round
   (seq-reduce
    #'+
    (seq-map
     (lambda (value)
       (nth 2 value))
     values)
    0)))

(defun vino-rating--total (score score-max)
  "Calculate total score of a rating from SCORE and SCORE-MAX."
  (vino-rating--round
   (* vino-rating-scale
      (/ (float score)
         (float score-max)))))

;;;###autoload
(defvar vino-rating-template
  '(:file-name "wine/rating/${id}.org"
    :tags ("wine" "rating"))
  "Capture template for rating entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and rating

See `vulpea-create' for more information.")

(defvar vino-rating-props `((1 . (("SCORE" . ,vino-rating-scale))))
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
  and PROP (inclusively), user is prompted for a number using
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
                    props)))
        (make-vino-rating
         :wine wine
         :date date
         :version version
         :values values)))))

(defun vino-rating-note-p (note)
  "Return non-nil if NOTE represents `vino-rating'."
  (when-let* ((tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "rating"))))

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

(defun vino-rating--create (rating &optional id extra-data)
  "Create a note for RATING.

ID is generated unless passed.

EXTRA-DATA is passed to `vino-rating-create-handle-functions'."
  (when-let*
      ((wine-note (vino-rating-wine rating))
       (date-str (vino-rating-date rating))
       (version (vino-rating-version rating))
       (values (vino-rating-values rating))
       (title (format "%s - %s" (vulpea-note-title wine-note) date-str))
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
    (vulpea-utils-with-note wine-note
      (vulpea-buffer-meta-set
       "ratings"
       (cons note (vulpea-note-meta-get-list wine-note "ratings" 'note)))
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))
    ;; pass ID so vino-entry-update can get the note from file/buffer
    (vino-entry-update (vulpea-note-id wine-note))
    (run-hook-with-args 'vino-rating-create-handle-functions note extra-data)
    note))


;;; Entry

;;;###autoload
(defvar vino-entry-template
  '(:file-name "wine/cellar/${id}.org"
    :tags ("wine" "cellar"))
  "Capture template for wine entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and cellar

See `vulpea-create' for more information.")

;;;###autoload
(defvar vino-entry-rating-average-method 'amean
  "Method to calculate rating of vino entry.

All ratings are averaged using selected method and then the result is
rounded to respect `vino-rating-precision'.

Supported methods:

- amean (arithmetic mean)
- min
- max
- oldest - when multiple ratings share the date, result is not defined
- latest - when multiple ratings share the date, result is not defined

The value can be function that takes a list of ratings (numbers) and
returns a single number. Even if a custom function is used, the
resulting rating is still rounded to respect `vino-rating-precision'.")

(defvar vino-entry-meta-props-order
  '("carbonation"
    "carbonation method"
    "colour"
    "sweetness"
    "producer"
    "name"
    "vintage"
    "base"
    "sur lie"
    "degorgee"
    "volume"
    "country"
    "region"
    "appellation"
    "grapes"
    "alcohol"
    "sugar"
    "price"
    "acquired"
    "consumed"
    "available"
    "rating"
    "ratings"))

;;;###autoload
(defun vino-entry-find-file ()
  "Select and find vino note."
  (interactive)
  (find-file (vulpea-note-path (vino-entry-note-select))))

(defun vino-entry-insert ()
  "Select and insert vino note."
  (interactive)
  (unwind-protect
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq
                     beg (set-marker
                          (make-marker) (region-beginning))
                     end (set-marker
                          (make-marker) (region-end))
                     region-text
                     (org-link-display-format
                      (buffer-substring-no-properties
                       beg end)))))
               (note (vino-entry-note-select region-text))
               (description (or region-text (vulpea-note-title note))))
          (unless (vulpea-note-id note)
            (setq note (vino-entry-create)))
          (when region-text
            (delete-region beg end)
            (set-marker beg nil)
            (set-marker end nil))
          (insert (org-link-make-string
                   (concat "id:" (vulpea-note-id note))
                   description))
          (run-hook-with-args 'vulpea-insert-handle-functions note)))
    (deactivate-mark)))

;;;###autoload
(defun vino-entry-create ()
  "Create a `vino-entry'.

The `vulpea-insert-handle-functions' are called with the created
note as the only argument."
  (interactive)
  (let* ((producer (vino-producer-select))
         (name (vino--repeat-while
                #'read-string
                #'string-empty-p
                "Name: "))
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
         (carbonation-method (when (eq carbonation 'sparkling)
                               (intern
                                (completing-read
                                 "Carbonation Method: "
                                 vino-carbonation-methods
                                 nil
                                 'require-match))))
         (sweetness (intern
                     (completing-read
                      "Sweetness:"
                      (plist-get vino-sweetness-levels
                                 carbonation)
                      nil
                      'require-match)))
         (vintage (vino--repeat-while
                   #'read-number
                   (lambda (v) (< v 1900))
                   "Vintage (C-g for NV): "))
         (base-vintage (when (and (eq carbonation-method 'traditional)
                                  (not vintage))
                         (vino--repeat-while
                          #'read-number
                          (lambda (v) (< v 1900))
                          "Base vintage (C-g for NV): ")))
         (sur-lie (when (eq carbonation-method 'traditional)
                    (let ((x (read-string "Sur lie: ")))
                      (if (string-empty-p x)
                          "N/A"
                        x))))
         (degorgee (when (eq carbonation-method 'traditional)
                     (let ((x (read-string "Degorgee: ")))
                       (if (string-empty-p x)
                           "N/A"
                         x))))
         (volume (vino--repeat-while
                  #'read-number
                  (lambda (v) (< v 1))
                  "Volume mL: " 750))
         (origin (funcall-interactively vino-origin-select-fn))
         (grapes (vino-grapes-select))
         (alcohol (vino--repeat-while
                   #'read-number
                   (lambda (v) (< v 0))
                   "Alcohol: "))
         (sugar (vino--repeat-while
                 #'read-number
                 (lambda (v) (< v 0))
                 "Sugar g/l (C-g for N/A): "))
         (price (s-presence (read-string "Price: ")))

         (title (format "%s %s %s"
                        (vulpea-note-title producer)
                        name
                        (or vintage "NV")))
         (note (vulpea-create
                title
                (plist-get vino-entry-template :file-name)
                :tags (seq-union (plist-get vino-entry-template :tags)
                                 '("wine" "cellar"))
                :head (plist-get vino-entry-template :head)
                :body (plist-get vino-entry-template :body)
                :context (plist-get vino-entry-template :context)
                :properties (plist-get vino-entry-template :properties)
                :meta (-filter
                       #'cdr
                       (append
                        `(("carbonation" . ,carbonation)
                          ("carbonation method" . ,carbonation-method)
                          ("colour" . ,colour)
                          ("sweetness" . ,sweetness)
                          ("producer" . ,producer)
                          ("name" . ,name)
                          ("vintage" . ,vintage)
                          ("base" . ,base-vintage)
                          ("sur lie" . ,sur-lie)
                          ("degorgee" . ,degorgee)
                          ("volume" . ,volume))
                        origin
                        `(("grapes" . ,grapes)
                          ("alcohol" . ,alcohol)
                          ("sugar" . ,sugar)
                          ("price" . ,price)
                          ("acquired" . 0)
                          ("consumed" . 0)
                          ("available" . 0)
                          ("rating" . "NA"))))
                :unnarrowed t
                :immediate-finish t)))
    (run-hook-with-args 'vino-entry-create-handle-functions note)
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
         (grapes (or grapes (vino-grapes-select))))
    (vulpea-utils-with-note note
      (vulpea-buffer-meta-set "grapes" grapes 'append)
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))))

;;;###autoload
(defun vino-entry-set-region (&optional note-or-id)
  "Set REGION to `vino-entry'.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

REGION may be either region or appellation."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (origin (funcall-interactively vino-origin-select-fn)))
    (vulpea-utils-with-note note
      (--each origin
        (vulpea-buffer-meta-set (car it) (cdr it)))
      (vulpea-buffer-meta-sort vino-entry-meta-props-order)
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))))

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
- title of the `vino-entry';
- description of the linked producer and ratings;
- title of all linked ratings."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note-or-id))
         (producer (vulpea-note-meta-get note "producer" 'note))
         (title (format
                 "%s %s %s"
                 (vulpea-note-title producer)
                 (vulpea-note-meta-get note "name")
                 (or (vulpea-note-meta-get note "vintage") "NV")))

         ;; update linked ratings
         rating-title rating
         (ratings (->>
                   (vulpea-note-meta-get-list note "ratings" 'note)
                   (--map
                    (vulpea-utils-with-note it
                      ;; ensure rating file is synced to database before reading
                      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer)))

                      ;; set title
                      (setq rating-title (format "%s - %s" title (vulpea-note-meta-get it "date")))
                      (vulpea-buffer-title-set rating-title)
                      (vulpea-buffer-meta-set "wine" (vulpea-utils-link-make-string note title))

                      ;; update rating
                      (setq rating (vino-rating-get-by-id it))
                      (vulpea-buffer-meta-set "score" (vino-rating-score rating) 'append)
                      (vulpea-buffer-meta-set "score_max" (vino-rating-score-max rating) 'append)
                      (vulpea-buffer-meta-set "total" (vino-rating-total rating) 'append)

                      ;; save buffer
                      (save-buffer)
                      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer)))

                      ;; return note id, so we can reload latest state from db;
                      ;; we could optimize title, but meta is a little bit more messy
                      (vulpea-note-id it)))
                   (vulpea-db-query-by-ids)
                   (-remove #'vulpea-note-primary-title)
                   (seq-sort-by #'vulpea-note-title #'string<)))

         ;; calculate rating
         (values (--map (vulpea-meta-get it "total" 'number) ratings))
         (rating (if (functionp vino-entry-rating-average-method)
                     (funcall vino-entry-rating-average-method values note)
                   (when values
                     (pcase vino-entry-rating-average-method
                       (`amean (/ (apply #'+ values)
                                  (float (length values))))
                       (`min (-min values))
                       (`max (-max values))
                       (`oldest (car values))
                       (`latest (car (reverse values)))
                       (_ (user-error "Unexpected value of `vino-entry-rating-average-method': %S"
                                      vino-entry-rating-average-method))))))
         (rating (if rating (vino-rating--round rating) "NA")))

    ;; update wine entry note
    (vulpea-utils-with-note note
      (vulpea-buffer-title-set title)
      (vulpea-buffer-meta-set "producer" producer)
      (vulpea-buffer-meta-set "ratings" ratings)
      (vulpea-buffer-meta-set "rating" rating 'append)
      ;; we need to save buffer to make sure that db is synced, so hooks can use this data
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer)))

      ;; run hook
      (run-hook-with-args
       'vino-entry-update-handle-functions
       (vulpea-db-get-by-id (vulpea-note-id note)))

      (vulpea-buffer-meta-sort vino-entry-meta-props-order)
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))))

;;;###autoload
(defun vino-entry-rate (&optional note-or-id date extra-data)
  "Rate a `vino-entry' on DATE.

When NOTE-OR-ID is non-nil, it is used to get `vino-entry'.
Otherwise if current buffer is visiting `vino-entry', it used
instead. Otherwise user is prompted to select a `vino-entry'
explicitly.

EXTRA-DATA is passed to `vino-rating-create-handle-functions'."
  (interactive)
  (when-let* ((note (vino-entry-note-get-dwim note-or-id))
              (date (or date (org-read-date nil t)))
              (info (car (last vino-rating-props)))
              (version (car info))
              (props (cdr info))
              (values (seq-map #'vino-rating--read-value props)))
    (vino-rating--create
     (make-vino-rating
      :wine note
      :date (format-time-string "%Y-%m-%d" date)
      :version version
      :values values)
     nil
     extra-data)))


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
(defun vino-entry-note-select (&optional initial-prompt)
  "Select and return a `vulpea-note' representing `vino-entry'.

Optionally provide INITIAL-PROMPT."
  (vulpea-select-from
   "Wine"
   (vulpea-db-query-by-tags-every '("wine" "cellar"))
   :require-match t
   :initial-prompt initial-prompt))

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
           ((vino-entry-note-p note) note)
           ((vino-rating-note-p note) (vulpea-note-meta-get note "wine" 'note))
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


;;; Country, region and appellation

;;;###autoload
(defvar vino-country-template
  '(:file-name "wine/country/${timestamp}-${slug}.org"
    :tags ("wine" "country"))
  "Capture template for region entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :meta (optional) - metadata (associative list)

  :properties (optional) - extra properties to put in PROPERTIES block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and country

See `vulpea-create' for more information.")

;;;###autoload
(defvar vino-region-template
  '(:file-name "wine/region/${country}/${timestamp}-${slug}.org"
    :tags ("wine" "region"))
  "Capture template for region entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :meta (optional) - metadata (associative list)

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and region

See `vulpea-create' for more information.")

;;;###autoload
(defvar vino-appellation-template
  '(:file-name "wine/appellation/${country}/${timestamp}-${slug}.org"
    :tags ("wine" "appellation"))
  "Capture template for appellation entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :meta (optional) - metadata (associative list)

  :properties (optional) - extra properties to put in PROPERTIES
  block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to wine and
  appellation

See `vulpea-create' for more information.")

;;;###autoload
(cl-defun vino-country-create (&key title)
  "Create a country note using `vino-country-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (read-string "Country: "))))
    (vulpea-create
     title
     (plist-get vino-country-template :file-name)
     :tags (seq-union (plist-get vino-country-template :tags)
                      '("wine" "country"))
     :head (plist-get vino-country-template :head)
     :meta (plist-get vino-country-template :meta)
     :body (plist-get vino-country-template :body)
     :context (plist-get vino-country-template :context)
     :properties (plist-get vino-country-template :properties)
     :unnarrowed t
     :immediate-finish t)))

;;;###autoload
(cl-defun vino-region-create (&key title country parent capture-properties)
  "Create a region note using `vino-region-template'.

Unless TITLE is provided, user is prompted to provide one.

Unless COUNTRY note is provided, user is prompted to provide one.

Unless PARENT is provided, user is prompted to provide one or none
explicitly.

CAPTURE-PROPERTIES are passed to `vulpea-create'.

Return `vulpea-note'."
  (interactive)
  (let* ((title (or title (read-string "Region title: ")))
         (country (or country (vino-country-select)))
         (parent (or parent (vino--repeat-while
                             #'vino-region-select
                             #'null
                             :country country
                             :prompt "Parent region (C-g for none)"))))
    (vulpea-create
     title
     (plist-get vino-region-template :file-name)
     :tags (seq-union (plist-get vino-region-template :tags)
                      '("wine" "region"))
     :head (plist-get vino-region-template :head)
     :meta (append
            `(("country" . ,country))
            (when parent
              `(("parent" . ,parent)))
            (plist-get vino-region-template :meta))
     :body (plist-get vino-region-template :body)
     :context (append
               (list :country (vulpea--title-to-slug (vulpea-note-title country)))
               (plist-get vino-region-template :context))
     :properties (plist-get vino-region-template :properties)
     :unnarrowed t
     :immediate-finish t
     :capture-properties capture-properties)))

;;;###autoload
(cl-defun vino-appellation-create (&key title country parent capture-properties)
  "Create a appellation note using `vino-appellation-template'.

Unless TITLE is provided, user is prompted to provide one.

Unless COUNTRY note is provided, user is prompted to provide one.

Unless PARENT is provided, user is prompted to provide one or none
explicitly.

CAPTURE-PROPERTIES are passed to `vulpea-create'.

Return `vulpea-note'."
  (interactive)
  (let* ((title (or title (read-string "Appellation: ")))
         (country (or country (vino-country-select)))
         (parent (or parent (vino--repeat-while
                             #'vino-region-select
                             #'null
                             :country country
                             :prompt "Parent region (C-g for none)"))))
    (vulpea-create
     title
     (plist-get vino-appellation-template :file-name)
     :tags (seq-union (plist-get vino-appellation-template :tags)
                      '("wine" "appellation"))
     :head (plist-get vino-appellation-template :head)
     :meta (append
            `(("country" . ,country))
            (when parent
              `(("parent" . ,parent)))
            (plist-get vino-appellation-template :meta))
     :body (plist-get vino-appellation-template :body)
     :context (append
               (list :country (vulpea--title-to-slug (vulpea-note-title country)))
               (plist-get vino-appellation-template :context))
     :properties (plist-get vino-appellation-template :properties)
     :unnarrowed t
     :immediate-finish t
     :capture-properties capture-properties)))

;;;###autoload
(defun vino-country-find-file ()
  "Select and find country note."
  (interactive)
  (find-file (vulpea-note-path (vino-country-select))))

(defun vino-country-select ()
  "Select a country note.

When country note does not exist, it is created using
`vino-country-create'.

Return `vulpea-note'."
  (let ((note (vulpea-select-from
               "Country"
               (vulpea-db-query-by-tags-every '("wine" "country")))))
    (if (vulpea-note-id note)
        note
      (vino-country-create :title (vulpea-note-title note)))))

;;;###autoload
(defun vino-region-find-file ()
  "Select and find region note."
  (interactive)
  (let* ((country (vino-country-select))
         (region (vino-region-select :country country)))
    (find-file (vulpea-note-path region))))

(cl-defun vino-region-select (&key country prompt)
  "Select a region note.

Optionally, list of candidates can be limited by COUNTRY.

Optionally, PROMPT can be provided.

When region note does not exist, it is created using
`vino-region-create'.

Return `vulpea-note'."
  (let* ((candidates (vulpea-db-query-by-tags-every '("wine" "region")))
         (candidates (if country
                         (--filter (string-equal (vulpea-note-id country)
                                                 (vulpea-note-meta-get it "country" 'link))
                                   candidates)
                       candidates))
         (note (vulpea-select-from (or prompt "Region") candidates)))
    (if (vulpea-note-id note)
        note
      (vino-region-create
       :title (vulpea-note-title note)
       :country country))))

;;;###autoload
(defun vino-appellation-find-file ()
  "Select and find appellation note."
  (interactive)
  (let* ((country (vino-country-select))
         (appellation (vino-appellation-select :country country)))
    (find-file (vulpea-note-path appellation))))

(cl-defun vino-appellation-select (&key country prompt)
  "Select a appellation note.

Optionally, list of candidates can be limited by COUNTRY.

Optionally, PROMPT can be provided.

When appellation note does not exist, it is created using
`vino-appellation-create'.

Return `vulpea-note'."
  (let* ((candidates (vulpea-db-query-by-tags-every '("wine" "appellation")))
         (candidates (if country
                         (--filter (string-equal (vulpea-note-id country)
                                                 (vulpea-note-meta-get it "country" 'link))
                                   candidates)
                       candidates))
         (note (vulpea-select-from (or prompt "Appellation") candidates)))
    (if (vulpea-note-id note)
        note
      (vino-appellation-create
       :title (vulpea-note-title note)
       :country country))))



(defun vino-origin-select-flat ()
  "Select origin of a wine using flat strategy.

When region or appellation note does not exist, it is created
using `vino-region-create' or `vino-appellation-create' depending
on user decision.

Return list of country, region and appellation depending on the user
selection.

If region is selected, then country is _country_ meta value of the
selected region; appellation is nil.

If appellation is selected, the country is _country_ meta value of the
selected appellation; and region is _parent_ meta value of the selected
appellation.

When the function tries to access meta value which is not set, it raises
an error."
  (let ((note
         (vulpea-select-from
          "Region or appellation"
          (seq-filter
           (lambda (note)
             (seq-contains-p (vulpea-note-tags note) "wine"))
           (vulpea-db-query-by-tags-some
            '("appellation" "region")))))
        country region appellation)
    (unless (vulpea-note-id note)
      (setq note (pcase (completing-read
                         (format "%s does not exist. What to do?"
                                 (vulpea-note-title note))
                         '("Create region"
                           "Create appellation"
                           "Abort"))
                   (`"Create region"
                    (vino-region-create :title (vulpea-note-title note)))
                   (`"Create appellation"
                    (vino-appellation-create :title (vulpea-note-title note)))
                   (_ (error "Abort")))))
    (setq country (vulpea-note-meta-get note "country" 'note))
    (unless country
      (error "No country is set in %s" (vulpea-note-title note)))
    (if (vulpea-note-tagged-any-p note "region")
        (setq region note)
      (setq appellation note))

    (-filter
     #'cdr
     `(("country" . ,country)
       ("region" . ,region)
       ("appellation" . ,appellation)))))

(defun vino-origin-select-by-country ()
  "Select origin of a wine using country-based strategy.

Selection is done in two ways. First, the user must pick a country, and
then select a region or appellation from a list of those that are part
of the selected country.

When country, region or appellation note does not exist, it is created
using respective function.

Return list of country, region and appellation depending on the user
selection.

If region is selected, appellation is nil.

If appellation is selected, region is _parent_ meta value of the
selected appellation.

When the function tries to access meta value which is not set, it raises
an error."
  (let ((country (vulpea-select-from
                  "Country"
                  (vulpea-db-query-by-tags-every '("wine" "country"))))
        rora region appellation)
    (unless (vulpea-note-id country)
      (when (y-or-n-p "Country %s doesn not exist. Would you like to create it?")
        (setq country (vino-country-create :title (vulpea-note-title country)))))
    (when (vulpea-note-id country)
      (setq rora (vulpea-select-from
                  "Region or appellation"
                  (->> (append (vulpea-db-query-by-tags-every '("wine" "region"))
                               (vulpea-db-query-by-tags-every '("wine" "appellation")))
                       (--filter (string-equal (vulpea-note-id country)
                                               (vulpea-note-meta-get it "country" 'link))))))
      (unless (vulpea-note-id rora)
        (setq rora (pcase (completing-read
                           (format "%s does not exist. What to do?"
                                   (vulpea-note-title rora))
                           '("Create region"
                             "Create appellation"
                             "Abort"))
                     (`"Create region"
                      (vino-region-create :title (vulpea-note-title rora)
                                          :country country))
                     (`"Create appellation"
                      (vino-appellation-create :title (vulpea-note-title rora)
                                               :country country))
                     (_ (error "Abort")))))
      (if (vulpea-note-tagged-any-p rora "region")
          (setq region rora)
        (setq appellation rora))
      (-filter
       #'cdr
       `(("country" . ,country)
         ("region" . ,region)
         ("appellation" . ,appellation))))))


;;; Grapes

;;;###autoload
(defvar vino-grape-template
  '(:file-name "wine/grape/${timestamp}-${slug}.org"
    :tags ("wine" "grape"))
  "Capture template for grape entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

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
  (let* ((title (or title (read-string "Grape: ")))
         (note (vulpea-create
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
    ;; sync to database for future queries
    (vulpea-db-update-file (vulpea-note-path note))
    note))

;;;###autoload
(defun vino-grape-find-file ()
  "Select and find grape note."
  (interactive)
  (find-file (vulpea-note-path (vino-grape-select))))

;;;###autoload
(defun vino-grape-select (&optional notes)
  "Select a grape note from NOTES.

When NOTES are nil, database is queried to get a list of notes.

When grape note does not exist, it may be created using
`vino-grape-create' or added as synonym to other grape if user
decides to do so.

Return `vulpea-note'."
  (let* ((notes (or notes (vulpea-db-query-by-tags-every '("wine" "grape"))))
         (note (vulpea-select-from "Grape" notes)))
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
             (vulpea-buffer-alias-add (vulpea-note-title note))
             (save-buffer)
             (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))
           (setf (vulpea-note-title base) (vulpea-note-title note))
           base))
        (_ note)))))

;;;###autoload
(defun vino-grapes-select ()
  "Select multiple grape notes.

When grape note does not exist, it may be created using
`vino-grape-create' or added as synonym to other grape if user
decides to do so.

Return a list, where each entry is a `vulpea-note'."
  (vulpea-select-multiple-from
   "Grape" (vulpea-db-query-by-tags-every '("wine" "grape"))
   :select-fn
   (lambda (_ notes &rest _)
     (vino-grape-select notes))))


;;; Producers

;;;###autoload
(defvar vino-producer-template
  '(:file-name "wine/producer/${timestamp}-${slug}.org"
    :tags ("wine" "producer"))
  "Capture template for producer entry.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

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
  (let* ((title (or title (read-string "Producer: ")))
         (note (vulpea-create
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
    ;; sync to database for future queries
    (vulpea-db-update-file (vulpea-note-path note))
    note))

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

;;;###autoload;
(defun vino-set-price (&optional note price)
  "Interactively set a new PRICE for a wine NOTE."
  (interactive)
  (let* ((note (or note (vino-entry-note-get-dwim)))
         (price-new (or price (read-string "Price: ")))
         (price-old (vulpea-note-meta-get note "price")))
    (unless (string-equal price-new price-old)
      (when price-old
        (vulpea-buffer-meta-set
         "price private"
         (-uniq (cons price-old (vulpea-note-meta-get-list note "price private")))))
      (vulpea-buffer-meta-set "price" price-new)
      (vulpea-buffer-meta-set "price date" (format-time-string "%F"))
      (vulpea-buffer-meta-sort vino-entry-meta-props-order))))


;;; Utilities

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
