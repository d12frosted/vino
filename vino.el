;;; vino.el --- Wine tracking in Emacs -*- lexical-binding: t; -*-
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
;;

;; TODO: validation
;; TODO: inventory
;; TODO: prices
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
  resources)

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
(defun vino-entry-p (&optional id)
  "Return non-nil if ID represents vino entry.

When ID is omitted, ID of the heading at point is taken."
  (when-let* ((id (or id (org-id-get)))
              (note (vulpea-db-get-by-id id))
              (tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "wine")
         (seq-contains-p tags "cellar"))))

(defun vino--entry-create (vino)
  "Create an entry for VINO."
  (let* ((producer (vulpea-db-get-by-id (vino-producer vino)))
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
    (vulpea-meta-set id "vintage" (vino-vintage vino) 'append)
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
      (when (and sugar
                 (>= sugar 0))
        (vulpea-meta-set id "sugar" sugar 'append)))
    (vulpea-meta-set id "resources" (vino-resources vino) 'append)
    id))

;;; Regions and appellations
;;

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
;;

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

;;; Utilities

;;;###autoload
(defun vino-resources-template ()
  "Query for resource URL and return it as a meta string."
  (seq-reduce
   (lambda (r a)
     (concat r "- resources :: " a "\n"))
   (+repeat-fn
    (lambda ()
      (let ((resource (read-string "Resource: ")))
        (when (not (string-empty-p resource))
          (vulpea-meta-format resource))))
    (lambda (a) (not (null a))))
   ""))

(provide 'vino)
;;; vino.el ends here
