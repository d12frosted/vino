;;; vino-test.el --- Test `vino' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Jan 2021
;;
;; URL:
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

(describe "vino-grape-select"
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
                  :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))))

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
                  :id "f9ef759b-f39e-4121-ab19-9ab3daa318be")))

  (it "returns full information about selected appellation"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,appellation) Cerasuolo di Vittoria DOCG")
    (expect (vino-region-select)
            :to-equal
            (list :path (expand-file-name "wine/appellation/cerasuolo_di_vittoria_docg.org" org-roam-directory)
                  :title "Cerasuolo di Vittoria DOCG"
                  :tags '("wine" "appellation")
                  :id "6a0819f3-0770-4481-9754-754ca397800b"))))

(provide 'vino-test)
;;; vino-test.el ends here
