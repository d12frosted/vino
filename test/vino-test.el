;;; vino-test.el --- Test `vino' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Test `vino' module.
;;
;;; Code:

(require 'buttercup)
(require 'vino)

(defvar vino-test-directory (expand-file-name "test/note-files")
  "Directory containing test notes.")

(defun vino-test--abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (expand-file-name file-path org-roam-directory))

(defun vino-test--map-file (fn file)
  "Execute FN with buffer visiting FILE."
  (let* ((fname (vino-test--abs-path file))
         (buf (find-file-noselect fname)))
    (with-current-buffer buf
      (funcall fn fname))))

(defun vino-test--init ()
  "Initialize testing environment."
  (let ((original-dir vino-test-directory)
        (new-dir (expand-file-name (make-temp-name "note-files") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir
          org-roam-tag-sources '(prop all-directories))
    (org-roam-mode +1)
    (sleep-for 2)))

(defun vino-test--teardown ()
  "Teardown testing environment."
  (org-roam-mode -1)
  (delete-file org-roam-db-location)
  (org-roam-db--close))

(buttercup-define-matcher :to-contain-exactly (file value)
  (cl-destructuring-bind
      ((file-expr . file) (value-expr . value))
      (mapcar #'buttercup--expr-and-value (list file value))
    (let* ((content (vino-test--map-file
                     (lambda (_)
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))
                     file))
           (spec (format-spec-make
                  ?F (format "%S" file-expr)
                  ?f (format "%S" file)
                  ?V (format "%S" value-expr)
                  ?v (format "%S" value)
                  ?c (format "%S" content))))
      (if (string-equal content value)
          (cons t (buttercup-format-spec
                   "Expected `%F' not to have content equal to `%v'"
                   spec))
        (cons nil (buttercup-format-spec
                   "Expected `%F' to have content equal to `%v', but instead `%F' has content equal to `%c'"
                   spec))))))

(describe "vino-entry-p"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns non-nil when used on wine entry"
    (expect (vino-entry-p "c9937e3e-c83d-4d8d-a612-6110e6706252")
            :to-be t))

  (it "returns nil when used on some heading inside wine entry"
    (expect (vino-entry-p "71715128-3d6f-4e36-8d70-d35fcb057609")
            :to-be nil))

  (it "returns nil when used on other entry"
    (expect (vino-entry-p "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
            :to-be nil)))

(describe "vino--entry-create"
  :var (vino id)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new entry with all information"
    (setq vino (make-vino :carbonation "still"
                          :colour "red"
                          :sweetness "dry"
                          :producer "9462dfad-603c-4094-9aca-a9042cec5dd2"
                          :name "Grotte Alte"
                          :vintage 2014
                          :appellation "6a0819f3-0770-4481-9754-754ca397800b"
                          :grapes '("cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
                                    "3b38917f-6065-42e8-87ca-33dd39a92fc0")
                          :alcohol 13
                          :sugar 0))
    (setq id (vino--entry-create vino))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Grotte Alte 2014
#+TIME-STAMP: <>

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Grotte Alte
- vintage :: 2014
- appellation :: [[id:6a0819f3-0770-4481-9754-754ca397800b][Cerasuolo di Vittoria DOCG]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- grapes :: [[id:3b38917f-6065-42e8-87ca-33dd39a92fc0][Nero d'Avola]]
- alcohol :: 13
- sugar :: 0


"
             id))))

(describe "vino-grape-select"
          :var (generated-id)
          (before-all
           (vino-test--init))

          (after-all
           (vino-test--teardown))

          (it "returns full information about selected grape"
              (spy-on 'org-roam-completion--completing-read
                      :and-return-value "(wine,grape) Frappato")
              (expect (vino-grape-select)
                      :to-equal
                      (list :path (expand-file-name "wine/grape/frappato.org" org-roam-directory)
                            :title "Frappato"
                            :tags '("wine" "grape")
                            :level 0
                            :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")))

          (it "creates a new grape note when selecting non-existing name"
              (setq generated-id (org-id-new))
              (spy-on 'org-id-new :and-return-value generated-id)
              (spy-on 'org-roam-completion--completing-read :and-return-value "Slarina")
              (spy-on 'read-string :and-return-value nil)
              (expect (vino-grape-select)
                      :to-equal
                      (list :path (expand-file-name (format "wine/grape/%s-slarina.org"
                                                            (format-time-string "%Y%m%d%H%M%S"
                                                                                (current-time)))
                                                    org-roam-directory)
                            :title "Slarina"
                            :tags '("wine" "grape")
                            :level 0
                            :id generated-id))))

(describe "vino-producer-select"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected producer"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,producer) Arianna Occhipinti")
    (expect (vino-producer-select)
            :to-equal
            (list :path (expand-file-name "wine/producer/arianna_occhipinti.org" org-roam-directory)
                  :title "Arianna Occhipinti"
                  :tags '("wine" "producer")
                  :level 0
                  :id "9462dfad-603c-4094-9aca-a9042cec5dd2"))))

(describe "vino-region-select"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected region"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,region) Central Otago")
    (expect (vino-region-select)
            :to-equal
            (list :path (expand-file-name "wine/region/central_otago.org" org-roam-directory)
                  :title "Central Otago"
                  :tags '("wine" "region")
                  :level 0
                  :id "f9ef759b-f39e-4121-ab19-9ab3daa318be")))

  (it "returns full information about selected appellation"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,appellation) Cerasuolo di Vittoria DOCG")
    (expect (vino-region-select)
            :to-equal
            (list :path (expand-file-name "wine/appellation/cerasuolo_di_vittoria_docg.org" org-roam-directory)
                  :title "Cerasuolo di Vittoria DOCG"
                  :tags '("wine" "appellation")
                  :level 0
                  :id "6a0819f3-0770-4481-9754-754ca397800b"))))

(describe "vino--resource-format"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "formats an URL"
    (expect (vino--resource-format "https://www.wikipedia.org/")
            :to-equal "[[https://www.wikipedia.org/][wikipedia.org]]"))

  (it "formats a note ID"
    (expect (vino--resource-format "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
            :to-equal "[[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]"))

  (it "throw user-error for unknown note"
    (expect (vino--resource-format "d36125b3-39e1-4bc3-8f7d-126159d8d60e")
            :to-throw 'user-error '("Note with id \"d36125b3-39e1-4bc3-8f7d-126159d8d60e\" does not exist")))

  (it "throw user-error for unsupported resource type"
    (expect (vino--resource-format "123")
            :to-throw 'user-error '("123 is not a valid resource"))))

(provide 'vino-test)
;;; vino-test.el ends here
