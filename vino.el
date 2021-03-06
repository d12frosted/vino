;;; vino.el --- Cellar tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
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
  (advice-add 'org-roam-db--update-files
              :after
              #'vino-db--update-files))


;;; Rating

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
(defun vino-rating-update (note-or-id)
  "Refresh rating represented by NOTE-OR-ID."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id))
         (meta (vulpea-meta note))
         (version (vulpea-meta-get! meta "version" 'number))
         (info (seq-find (lambda (x) (equal (car x) version))
                         vino-rating-props))
         (props (cdr info))
         (score
          (seq-reduce
           #'+
           (seq-map
            (lambda (x)
              (vulpea-meta-get! meta (downcase x) 'number))
            (seq-map #'car props))
           0))
         (score-max
          (seq-reduce
           #'+
           (seq-map
            (lambda (x)
              (vulpea-meta-get!
               meta
               (downcase (concat x "_MAX"))
               'number))
            (seq-map #'car props))
           0))
         (total (* 10.0 (/ (float score) (float score-max)))))
    (vulpea-meta-set note "score" score 'append)
    (vulpea-meta-set note "score_max" score-max 'append)
    (vulpea-meta-set note "total" total 'append)))

(defun vino-rating--read (props)
  "Read rating values from PROPS."
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

(defun vino-rating--create (id date version values)
  "Rate a rating note for `vino-entry' with ID.

The process is simple:

1. Create a new rating note.
2. Set wine ID meta.
3. Set DATE meta.
4. Set VERSION meta.
5. Set VALUES"
  (when-let*
      ((vino (vino-entry-get-by-id id))
       (producer (vino-entry-producer vino))
       (date-str (if (stringp date)
                     date
                   (format-time-string "%Y-%m-%d" date)))
       (title (format "%s %s %s - %s"
                      (vulpea-note-title producer)
                      (vino-entry-name vino)
                      (or (vino-entry-vintage vino) "NV")
                      date-str))
       (note (vulpea-create title vino-rating-template)))
    ;; TODO: performance of multiple `vulpea-meta-set'
    (vulpea-meta-set
     id
     "ratings"
     (cons note
           (vulpea-meta-get-list id "ratings" 'note))
     'append)
    (vulpea-meta-set note "wine" id 'append)
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
    (vino-entry-update id)
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
(defun vino-entry-get-by-id (id)
  "Get `vino-entry' by ID."
  (let ((note (vulpea-db-get-by-id id)))
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
     (vulpea-note-id note) date version values)))


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
    "%(vino-resources-template)%?"
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
      (prices)
      (acquired :not-null)
      (consumed :not-null)
      (rating)
      (ratings)])))

(defun vino-db-query (sql &rest args)
  "Run SQL query on `vino' database with ARGS.

SQL can be either the emacsql vector representation, or a
string."
  (if (stringp sql)
      (emacsql (vino-db) (apply #'format sql args))
    (apply #'emacsql (vino-db) sql args)))

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
  (when force (delete-file vino-db-location))
  (vino-db--close)
  (vino-db)
  (let* ((notes (vulpea-db-query #'vino-entry-note-p))
         (current-entries (vino-db--get-current-entries))
         (modified-entries nil)
         (deleted-count 0))
    (dolist (note notes)
      (let* ((contents-hash
              (org-roam-db--file-hash
               (vulpea-note-path note))))
        (unless (string=
                 (gethash (vulpea-note-id note)
                          current-entries)
                 contents-hash)
          (push (cons note contents-hash) modified-entries)))
      (remhash (vulpea-note-id note) current-entries))
    (dolist (note (hash-table-keys current-entries))
      ;; These notes are no longer around, remove from cache...
      (vino-db--clear-note note)
      (setq deleted-count (1+ deleted-count)))
    (vino-db--update-notes modified-entries)
    (message "(vino) total: Δ%s, modified: Δ%s, deleted: Δ%s"
             (length notes)
             (length modified-entries)
             deleted-count)))

(defun vino-db--get-current-entries ()
  "Return a hash-table of entry id to the hash of its content."
  (let* ((entries (vino-db-query [:select * :from cellar]))
         (current-files (org-roam-db--get-current-files))
         (ht (make-hash-table :test #'equal)))
    (dolist (row entries)
      (puthash (car row) (gethash (cadr row) current-files) ht))
    ht))

(defun vino-db--update-notes (modified-notes)
  "Update `vino' cache for a list of MODIFIED-NOTES.

Notes is a list of (note . hash) pairs."
  (pcase-dolist (`(,note . _) modified-notes)
    (vino-db--clear-note note))
  (let ((modified-count 0))
    (pcase-dolist (`(,note . _) modified-notes)
      (message "(vino) Processed %s/%s modified notes..."
               modified-count
               (length modified-notes))
      (vino-db--update-note note)
      (setq modified-count (1+ modified-count)))))

(defun vino-db--update-files (modified-files)
  "Update `vino' cache for a list of MODIFIED-FILES.

Files is a list of (file . hash) pairs."
  (vino-db--update-notes
   (seq-map
    (lambda (x)
      (cons
       (vulpea-db-get-by-id
        (vulpea-db-get-id-by-file (car x)))
       (cdr x)))
    modified-files)))

(defun vino-db--update-note (note)
  "Update `vino' cache for a NOTE."
  (let* ((id (vulpea-note-id note))
         (entry (vino-entry-get-by-id id)))
    (vino-db--update-entry id (vulpea-note-path note) entry)))

(defun vino-db--update-entry (id file entry)
  "Update `vino' cache for an ENTRY with ID in FILE."
  (vino-db-query
   [:insert :into cellar
    :values $v1]
   (list
    (vector
     id
     file
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
     (vino-entry-price entry)
     (vino-entry-acquired entry)
     (vino-entry-consumed entry)
     (vino-entry-rating entry)
     (seq-map #'vulpea-note-id (vino-entry-ratings entry))))))

(defun vino-db--clear-note (note)
  "Remove any db information related to NOTE."
  (when-let ((id (vulpea-note-id note)))
    (vino-db-query
     [:delete :from cellar
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
