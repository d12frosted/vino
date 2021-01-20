;;; vino.el --- Cellar tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 09 Jan 2021
;;
;; URL: https://github.com/d12frosted/vino
;;
;; License: GPLv3
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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
       (producer (vulpea-db-get-by-id (vino-entry-producer vino)))
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
           (vulpea-meta-get-list id "ratings" 'link))
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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
         :producer (vulpea-meta-get! meta "producer" 'link)
         :name (vulpea-meta-get! meta "name" 'string)
         :vintage (vulpea-meta-get! meta "vintage" 'number)
         :appellation (vulpea-meta-get! meta "appellation" 'link)
         :region (vulpea-meta-get! meta "region" 'link)
         :grapes (vulpea-meta-get-list! meta "grapes" 'link)
         :alcohol (vulpea-meta-get! meta "alcohol" 'number)
         :sugar (vulpea-meta-get! meta "sugar" 'number)
         :acquired (vulpea-meta-get! meta "acquired" 'number)
         :consumed (vulpea-meta-get! meta "consumed" 'number)
         :resources (vulpea-meta-get-list! meta "resources" 'link)
         :price (vulpea-meta-get-list! meta "price" 'string)
         :rating (vino--parse-opt-number
                  (vulpea-meta-get! meta "rating" 'string)
                  "NA")
         :ratings (vulpea-meta-get-list! meta "ratings" 'link))))))

;;;###autoload
(defun vino-entry-create ()
  "Create a `vino-entry'."
  (interactive)
  (vino-entry--create (vino-entry-read)))

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
         (note (vulpea-create title vino-entry-template id)))
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
                 ;; TODO use 'note type from vulpea-meta
                 (vulpea-note-title
                  (vulpea-db-get-by-id
                   (vulpea-meta-get! meta "producer" 'link)))
                 (vulpea-meta-get! meta "name")
                 (or (vulpea-meta-get! meta "vintage" 'number)
                     "NV"))))
    (vulpea-utils-with-file (vulpea-note-path note)
      (org-roam--set-global-prop "TITLE" title)
      (save-buffer))
    (vulpea-db-update note)
    (setq note (vulpea-db-get-by-id (vulpea-note-id note)))
    (vulpea-meta-set
     note
     "ratings"
     (seq-map
      (lambda (rn)
        (vulpea-utils-with-file (vulpea-note-path rn)
          (org-roam--set-global-prop
           "TITLE"
           (format "%s - %s"
                   title
                   (vulpea-meta-get rn "date")))
          (save-buffer))
        (vulpea-db-update rn)
        (vulpea-meta-set rn "wine" note)
        (vulpea-db-get-by-id (vulpea-note-id rn)))
      (seq-map
       #'vulpea-db-get-by-id
       (vulpea-meta-get-list note "ratings" 'link))))))

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
         ;; TODO use 'note type from vulpea-meta
         (ratings (seq-map
                   #'vulpea-db-get-by-id
                   (vulpea-meta-get-list note "ratings" 'link)))
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
                      (or (vulpea-meta-get note "available" 'number)
                          1))))
         (date (or date (org-read-date nil t))))
    (funcall vino-availability-sub-fn
             id amount action date)
    (vino-entry-update-availability id)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate? "))
      (vino-entry-rate id date))))

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
   nil nil
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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
(defun vino-region-select ()
  "Select a wine region or appellation note.

See `vulpea' documentation for more information on note
structure."
  (vulpea-select
   "Region"
   nil nil
   (lambda (note)
     (let ((tags (vulpea-note-tags note)))
       (and (seq-contains-p tags "wine")
            (or (seq-contains-p tags "appellation")
                (seq-contains-p tags "region")))))))


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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
(defun vino-grape-select ()
  "Select a grape note.

Return `vulpea-note'."
  (let ((result
         (vulpea-select
          "Grape"
          nil nil
          (lambda (note)
            (let ((tags (vulpea-note-tags note)))
              (and (seq-contains-p tags "wine")
                   (seq-contains-p tags "grape")))))))
    (if (vulpea-note-id result)
        result
      (vino-grape-create (vulpea-note-title result)))))


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
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n\n")
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
(defun vino-producer-select ()
  "Select a producer note.

See `vulpea' documentation for more information on note
structure."
  (vulpea-select
   "Producer"
   nil nil
   (lambda (note)
     (let ((tags (vulpea-note-tags note)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "producer"))))))


;;; Price

;;;###autoload
(defun vino-price-read (vino)
  "Read the price for VINO entry."
  (if-let ((price (car (vino-entry-price vino))))
      (read-string (format "Price (default %s): " price)
                   nil nil price)
    (read-string "Price: ")))


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
