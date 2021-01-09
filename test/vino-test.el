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

(provide 'vino-test)
;;; vino-test.el ends here
