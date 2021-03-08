;;; vino-test-utils.el --- Vino test utils -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 08 Mar 2021
;;
;; URL: https://github.com/d12frosted/vino
;;
;; License: GPLv3
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
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Utilities for testing `vino'.
;;
;;; Code:

(require 'vino)
(require 'buttercup)



(defvar vino-test-directory (expand-file-name "test/note-files")
  "Directory containing test notes.")

(defun vino-test-abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (expand-file-name file-path org-roam-directory))

(defun vino-test-map-file (fn file)
  "Execute FN with buffer visiting FILE."
  (let* ((fname (vino-test-abs-path file))
         (buf (find-file-noselect fname)))
    (with-current-buffer buf
      (funcall fn fname))))



(defun vino-test-init ()
  "Initialize testing environment."
  (let ((original-dir vino-test-directory)
        (new-dir (expand-file-name (make-temp-name "note-files") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir
          org-roam-tag-sources '(prop all-directories))
    (org-roam-mode +1)
    (vulpea-setup)
    (vino-setup)
    (org-roam-db-build-cache)))

(defun vino-test-teardown ()
  "Teardown testing environment."
  (org-roam-mode -1)
  (delete-file org-roam-db-location)
  (org-roam-db--close)
  (vino-db--close))



(buttercup-define-matcher :to-be-note-like (a b)
  (cl-destructuring-bind
      ((a-expr . a)
       (b-expr . b))
      (mapcar #'buttercup--expr-and-value (list a b))
    (let* ((spec (format-spec-make
                  ?A (format "%S" a-expr)
                  ?a (format "%S" a)
                  ?B (format "%S" b-expr)
                  ?b (format "%S" b))))
      (let ((o (copy-vulpea-note a)))
        (setf (vulpea-note-meta o) nil)
        (if (equal o b)
            (cons t (buttercup-format-spec
                     "Expected `%A' not to be like `'%b, but it was."
                     spec))
          (cons nil (buttercup-format-spec
                     "Expected `%A' to be like `%b', but instead it was `%a'."
                     spec)))))))

(buttercup-define-matcher :to-contain-exactly (file value)
  (cl-destructuring-bind
      ((file-expr . file) (value-expr . value))
      (mapcar #'buttercup--expr-and-value (list file value))
    (let* ((content (vino-test-map-file
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



(provide 'vino-test-utils)
;;; vino-test-utils.el ends here
