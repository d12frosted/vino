;;; vino-test-utils.el --- Vino test utils -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2022 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
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
(require 'dash)

(when (version<= "29" emacs-version)
  (defun buttercup-format-spec (format specification)
    "Return a string based on FORMAT and SPECIFICATION.

This is a wrapper around `format-spec', which see. This also adds
a call to `save-match-data', as `format-spec' modifies that."
    (save-match-data
      (format-spec format (--map
                           (cons (car it) (lambda () (cdr it)))
                           specification)))))



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
    (vino-test-init-in new-dir)))

(defun vino-test-init-in (dir)
  "Initialize testing environment in DIR."
  (setq org-roam-directory dir
        org-roam-db-location (expand-file-name "org-roam.db" dir)
        vino-db-gc-threshold most-positive-fixnum)
  (vulpea-db-autosync-enable)
  (org-roam-db-autosync-enable)
  (vino-setup)
  (vino-db-sync))

(defun vino-test-teardown ()
  "Teardown testing environment."
  (vulpea-db-autosync-disable)
  (org-roam-db-autosync-disable)
  (delete-file org-roam-db-location)
  (delete-file vino-db-location)
  (vino-db--close))



(cl-defun mk-vulpea-note (&key type id title basename category tags meta links)
  "Constructor of `vulpea-note' for `vino' testing.

It handles boilerplate of note creation that is actually
irrelevant for `vino' library. We just care about few slots.

TYPE (mandatory) is one of: cellar, rating, grape, producer,
appellation, region.

ID (mandatory) is id slot of the note.

TITLE (mandatory) is title slot of the note.

BASENAME (optional) is basename of note file. E.g. without
directory and extension. When omitted, BASENAME is calculated as
`org-roam-node-slug` of TITLE.

CATEGORY (optional) is category property inside properties slot
of the note. When omitted, equals to BASENAME.

TAGS (optional) is tags slot of the future note. When omitted,
equals to (TYPE wine) list. Unfortunately, since tags list order
is fixed, but unpredictable, there is no generic solution for
now.

META (optional) is meta slot of the future note.

LINKS (optional) is list of (type . link) pairs."
  (let* ((basename (or basename
                       (org-roam-node-slug
                        (org-roam-node-create :title title))))
         (category (or category basename))
         (path (expand-file-name
                (format "wine/%s/%s.org" type basename)
                org-roam-directory)))
    (make-vulpea-note
     :path path
     :title title
     :tags (or tags (list "wine" type))
     :level 0
     :id id
     :links links
     :properties (list
                  (cons "CATEGORY" category)
                  (cons "ID" id)
                  (cons "BLOCKED" "")
                  (cons "FILE" path)
                  (cons "PRIORITY" "B"))
     :meta meta
     :attach-dir (expand-file-name
                  (format "wine/%s/data/%s/%s"
                          type
                          (s-left 2 id)
                          (s-chop-prefix (s-left 2 id) id))
                  org-roam-directory))))

(cl-defun mock-vulpea-note (&key type title tags)
  "Prepare system for note creation.

This function is needed when you want to create a note, but want
to be able to fix 'random' stuff - id generation and clock to be
able to compare result.

This function mocks whatever is needed for proper note creation
and returns a note that would be generated for a given TYPE,
TITLE and TAGS.

TYPE (mandatory) is one of: cellar, rating, grape, producer,
appellation, region.

TITLE (mandatory) is title slot of the future note.

TAGS (optional) is tags slot of the future note. When omitted,
equals to (wine TYPE) list. Unfortunately, since tags list order
is fixed, but unpredictable, there is no generic solution for
now."
  (let* ((id (org-id-new))
         (ts (current-time))
         (slug (org-roam-node-slug (org-roam-node-create :title title)))
         (basename (format "%s-%s"
                           (format-time-string "%Y%m%d%H%M%S" ts)
                           slug)))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (mk-vulpea-note
     :type type
     :id id
     :title title
     :basename basename
     :tags (or tags (list "wine" type)))))



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



(defmacro vino-spy--return-values (res values orig-initform)
  "Create a function for :and-return-values spy.

Resulting function is interactive depending on ORIG-INITFORM.

Basically, it sets the value of RES variable to `car' of VALUES,
and replaces VALUES value with `cdr' of VALUES."
  (let ((tempres (make-symbol "res"))
        (tempvalues (make-symbol "values")))
    `(let ((,tempres ,res)
           (,tempvalues ,values))
       (lambda (&rest _)
         ,orig-initform
         (setq ,tempres (car ,tempvalues)
               ,tempvalues (cdr ,tempvalues))
         ,tempres))))

(defun vino-spy-on (original symbol &optional keyword arg)
  "Create a spy (mock) for the function SYMBOL.

KEYWORD can have one of the following values:
  :and-call-through -- Track calls, but call the original
      function.
  :and-return-value -- Track calls, but return ARG instead of
      calling the original function.
  :and-return-values -- Track calls, but return elements of ARG
      list in turn instead of calling the original function. Once
      list is depleted, nil is returned.
  :and-call-fake -- Track calls, but call ARG instead of the
      original function.
  :and-throw-error -- Signal ARG as an error instead of calling
      the original function.
  nil -- Track calls, but simply return nil instead of calling
      the original function.

If the original function was a command, the generated spy will
also be a command with the same interactive form, unless
`:and-call-fake' is used, in which case it is the caller's
responsibility to ensure ARG is a command.

In the nutshell, it's an advice around `spy-on', which is passed
as ORIGINAL."
  (pcase keyword
    (:and-return-values
     (let* ((orig (and (fboundp symbol) (symbol-function symbol)))
            (replacement (vino-spy--return-values nil arg (interactive-form orig))))
       (unless (buttercup--spy-on-and-call-replacement symbol replacement)
         (error "Spies can only be created in `before-each'"))
       ))
    (_ (funcall original symbol keyword arg))))

(advice-add #'spy-on :around #'vino-spy-on)



(cl-defun completion-for (&key title tags)
  "Return completion for TITLE and TAGS matchers."
  (when-let ((note
              (seq-find
               (lambda (note)
                 (let ((res (and (or (null title) (string-equal title (vulpea-note-title note)))
                                 (or (null tags)
                                     (seq-every-p
                                      (lambda (x)
                                        (seq-contains-p (vulpea-note-tags note) x))
                                      tags)))))
                   res))
               (vulpea-db-query))))
    (vulpea-select-describe note)))



(provide 'vino-test-utils)
;;; vino-test-utils.el ends here
