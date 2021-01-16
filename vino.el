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


;; Entry

;; TODO: inventory
;;;###autoload
(cl-defstruct vino
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
  consumed)

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
  "Capture template for grape entry.")

;;;###autoload
(defun vino-entry-create ()
  "Create a `vino` entry."
  (interactive)
  (vino--entry-create (vino-entry-read)))

(defun vino-entry-read ()
  "Read a `vino` entry."
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
    (make-vino
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
     :consumed 0)))

(defun vino--entry-create (vino)
  "Create an entry for VINO."
  (let* ((producer (vino-producer vino))
         (producer (if (vulpea-note-p producer)
                       producer
                     (vulpea-db-get-by-id producer)))
         (vintage (vino-vintage vino))
         (title (concat (vulpea-note-title producer)
                        " "
                        (vino-name vino)
                        " "
                        (if (numberp vintage)
                            (number-to-string vintage)
                          vintage)))
         (id (vulpea-create title vino-entry-template)))
    (org-roam-db-update-file
     (expand-file-name (concat "wine/cellar/" id ".org")
                       org-roam-directory))
    ;; TODO: optimize multiple calls
    (vulpea-meta-set id "carbonation" (vino-carbonation vino) 'append)
    (vulpea-meta-set id "colour" (vino-colour vino) 'append)
    (vulpea-meta-set id "sweetness" (vino-sweetness vino) 'append)
    (vulpea-meta-set id "producer" (vino-producer vino) 'append)
    (vulpea-meta-set id "name" (vino-name vino) 'append)
    (when-let ((vintage (vino-vintage vino)))
      (vulpea-meta-set id "vintage" vintage 'append))
    (when-let ((appellation (vino-appellation vino)))
      (vulpea-meta-set id "appellation" appellation 'append))
    (when-let ((region (vino-region vino)))
      (vulpea-meta-set id "region" region 'append))
    (vulpea-meta-set id "grapes" (vino-grapes vino) 'append)
    (let ((alcohol (vino-alcohol vino)))
      (when (and alcohol
                 (> alcohol 0))
        (vulpea-meta-set id "alcohol" alcohol 'append)))
    (let ((sugar (vino-sugar vino)))
      (when (and sugar (>= sugar 0))
        (vulpea-meta-set id "sugar" sugar 'append)))
    (when (vino-price vino)
      (vulpea-meta-set id "price" (vino-price vino) 'append))
    (let ((acquired (or (vino-acquired vino) 0))
          (consumed (or (vino-consumed vino) 0)))
      (vulpea-meta-set id "acquired" acquired 'append)
      (vulpea-meta-set id "consumed" consumed 'append)
      (vulpea-meta-set id "available" (- acquired consumed) 'append))
    (vulpea-meta-set id "resources" (vino-resources vino) 'append)
    id))

(defun vino-entry-get-by-id (id)
  "Get `vino' entry by ID."
  (let ((note (vulpea-db-get-by-id id)))
    (when (and note (vino-note-p note))
      ;; TODO: optimise multiple calls
      (make-vino
       :carbonation (vulpea-meta-get id "carbonation" 'symbol)
       :colour (vulpea-meta-get id "colour" 'symbol)
       :sweetness (vulpea-meta-get id "sweetness" 'symbol)
       :producer (vulpea-meta-get id "producer" 'link)
       :name (vulpea-meta-get id "name" 'string)
       :vintage (vulpea-meta-get id "vintage" 'number)
       :appellation (vulpea-meta-get id "appellation" 'link)
       :region (vulpea-meta-get id "region" 'link)
       :grapes (vulpea-meta-get-list id "grapes" 'link)
       :alcohol (vulpea-meta-get id "alcohol" 'number)
       :sugar (vulpea-meta-get id "sugar" 'number)
       :acquired (vulpea-meta-get id "acquired" 'number)
       :consumed (vulpea-meta-get id "consumed" 'number)
       :resources (vulpea-meta-get-list id "resources" 'link)
       :price (vulpea-meta-get-list id "price" 'string)))))


;;; Note

;;;###autoload
(defun vino-note-p (note)
  "Return non-nil if NOTE represents vino entry."
  (when-let* ((tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "cellar"))))

;;;###autoload
(defun vino-note-select ()
  "Select a wine note.

See `vulpea' documentation for more information on note
structure."
  (vulpea-select
   "Wine"
   nil nil
   (lambda (note)
     (let ((tags (vulpea-note-tags note)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "cellar"))))))

;;;###autoload
(defun vino-note-get (&optional note-or-id)
  "Get a note representing `vino' entry in a dwim style.

If NOTE-OR-ID is an ID, then get a note by that ID. Throws error
if extracted note does not represent a `vino' entry.

If NOTE-OR-ID is a note, then return it. Throws error
if extracted note does not represent a `vino' entry.

If NOTE-OR-ID is nil, try to extract a note from current buffer
or ask for user to select a note."
  (cond
   ((null note-or-id)
    (if (eq major-mode 'org-mode)
        (let* ((id (save-excursion
                     (goto-char (point-min))
                     (org-id-get)))
               (note (ignore-errors (vino-note-get id))))
          (if note
              note
            (vino-note-select)))
      (vino-note-select)))
   ((stringp note-or-id)
    (let ((note (vulpea-db-get-by-id note-or-id)))
      (if (and note (vino-note-p note))
          note
        (user-error
         "Note with id %s does not represent a vino entry"
         note-or-id))))
   ((vulpea-note-p note-or-id)
    (if (vino-note-p note-or-id)
        note-or-id
      (user-error
       "Note %s does not represent a vino entry"
       note-or-id)))
   (t
    (user-error "Unsupported type of %s" note-or-id))))


;;; Regions and appellations

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
  "Capture template for grape entry.")

;;;###autoload
(defun vino-grape-select ()
  "Select a grape note.

See `vulpea' documentation for more information on note
structure."
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
      (let* ((title (vulpea-note-title result))
             (id (vulpea-create title vino-grape-template)))
        (org-roam-db-build-cache)
        (vulpea-db-get-by-id id)))))


;;; Producers

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
  (if-let ((price (car (vino-price vino))))
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


;;; Availability

(defvar vino-availability-fn nil
  "Function to check availability of `vino' entry.

Function is called with ID of `vino' entry and returns a cons of
acquired and consumed numbers.")

(defvar vino-availability-add-fn nil
  "Function to add AMOUNT of `vino' to inventory.

Function is called with ID of `vino' entry, AMOUNT, SOURCE and
DATE arguments.")

(defvar vino-availability-sub-fn nil
  "Function to subtract AMOUNT of `vino' from inventory.

Function is called with ID of `vino' entry, AMOUNT, ACTION and
DATE arguments.")

(defun vino-acquire (&optional id amount source price date)
  "Acquire AMOUNT of vine with ID from SOURCE for PRICE at DATE."
  (interactive)
  (let* ((note (vino-note-get id))
         (id (vulpea-note-id note))
         (vino (vino-entry-get-by-id id))
         (source (or source (read-string "Source: ")))
         (amount (or amount (read-number "Amount: " 1)))
         (price (or price (vino-price-read vino)))
         (date (or date (org-read-date nil t))))
    (funcall vino-availability-add-fn
             id amount source date)
    (let ((prices (vino-price vino)))
      (unless (seq-contains-p prices price)
        (vulpea-meta-set id "PRICE" (cons price prices) 'append)))
    (vino-availability-update id)))

(defun vino-consume (&optional id amount action date)
  "Consume AMOUNT of ID because of ACTION at DATE."
  (interactive)
  (let* ((note (vino-note-get id))
         (id (vulpea-note-id note))
         (action (or action (read-string "Action: " "consume")))
         (amount (or amount
                     (read-number
                      "Amount: "
                      (or (vulpea-meta-get id "available" 'number)
                          1))))
         (date (or date (org-read-date nil t))))
    (funcall vino-availability-sub-fn
             id amount action date)
    (vino-availability-update id)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate? "))
      ;; (vino-rate date)
      (message "not implemented"))))

(defun vino-availability-update (id)
  "Update availability metadata of `vino' with ID."
  (unless vino-availability-fn
    (user-error "`vino-availability-fn' is nil"))
  (let* ((res (funcall vino-availability-fn id))
         (in (car res))
         (out (cdr res))
         (cur (- in out)))
    (vulpea-meta-set id "acquired" in 'append)
    (vulpea-meta-set id "consumed" out 'append)
    (vulpea-meta-set id "available" cur 'append)))

(provide 'vino)
;;; vino.el ends here
