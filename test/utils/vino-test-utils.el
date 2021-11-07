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
(require 'dash)



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
     :tags (or tags (list type "wine"))
     :level 0
     :id id
     :links links
     :properties (list
                  (cons "CATEGORY" category)
                  (cons "ID" id)
                  (cons "BLOCKED" "")
                  (cons "FILE" path)
                  (cons "PRIORITY" "B"))
     :meta meta)))

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



(defvar random-valid-chars
  (split-string "abcdefghijklmnopqrstuvwxyz" "" t))

(defvar random-valid-chars-length
  (length random-valid-chars))

(defun random-string (length)
  "Return random string of LENGTH."
  (let* (s)
    (while (< (length s) length)
      (setq s (concat s (nth (random random-valid-chars-length)
                             random-valid-chars))))
    s))

(defun random-name ()
  "Generate random name."
  (concat
   (capitalize (random-string (+ 3 (random 6))))
   (when (= (random 2) 1)
     (concat
      " "
      (capitalize (random-string (+ 3 (random 6))))))))



(defun swap (list el1 el2)
  "In LIST swap indices EL1 and EL2 in place."
  (let ((tmp (elt list el1)))
    (setf (elt list el1) (elt list el2))
    (setf (elt list el2) tmp)))

(defun shuffle (list)
  "Shuffle the elements in LIST.

Shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length list))))
           do (let ((j (random (+ i 1))))
                (swap list i j)))
  list)



(defun print-to-file (filename data)
  "Print DATA to FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  "Read data from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))



(defun vino-test-generate-data (dir &optional verbose)
  "Generate test data in DIR.

When VERBOSE is non-nil, lots of garbage will be printed.

DIR will be populated like:

.
├── data
│   ├── appellations
│   ├── cellar
│   ├── grapes
│   ├── producers
│   ├── ratings
│   └── regions
└── notes
    └── wine
        ├── appellation
        │   │   20210313121752-anm.org
        │   └── ...
        ├── cellar
        │   │   58324762-802c-11eb-a46e-93bf426c7c38.org
        │   └── ...
        ├── grape
        │   │   20210313121752-caroelit.org
        │   └── ...
        ├── producer
        │   │   20210313121752-ddhfe.org
        │   └── ...
        ├── rating
        │   │   554f9c34-802c-11eb-8904-6fb6e370bbb9.org
        │   └── ...
        └── region
            │   20210313121752-etwjis_btqdxlny.org
            └── ...

DIR/notes can be used as `org-roam-directory'."
  (let ((notes-dir (expand-file-name "notes" dir))
        (producers-file (expand-file-name "data/producers" dir))
        (grapes-file (expand-file-name "data/grapes" dir))
        (regions-file (expand-file-name "data/regions" dir))
        (appellations-file (expand-file-name "data/appellations" dir))
        (cellar-file (expand-file-name "data/cellar" dir))
        (ratings-file (expand-file-name "data/ratings" dir)))
    (make-directory dir)
    (make-directory (expand-file-name "notes" dir))
    (make-directory (expand-file-name "data" dir))
    (setq org-id-uuid-program
          "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
    (setq vino-rating-props
          '((1 . (("score" 20)))
            (4 . (("property_1" 3)
                  ("property_2" 4)
                  ("property_3" 2)
                  ("property_4" 5)
                  ("property_5" 6)))))
    (vino-test-init-in notes-dir)
    (let* ((producers (vino-test--generate-producers 10 producers-file))
           (grapes (vino-test--generate-grapes 20 grapes-file))
           (regions (vino-test--generate-regions 10 regions-file))
           (appellations (vino-test--generate-appellations 10 appellations-file))
           (resources
            (list (vulpea-note-id (nth (random (length producers)) producers))
                  (vulpea-note-id (nth (random (length producers)) grapes))
                  (vulpea-note-id (nth (random (length producers)) regions))
                  (vulpea-note-id (nth (random (length producers)) appellations))
                  "http://www.agricolaocchipinti.it/it/vinicontrada"
                  "wikipedia"
                  "duckduckgo")))
      (vino-test--generate-cellar
       cellar-file
       ratings-file
       producers
       grapes
       regions
       appellations
       resources
       verbose))))

(defun vino-test--generate-producers (n filename)
  "Generate N producers and store info in FILENAME."
  (let ((producers
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-producer-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'vino-test--relativize-note
                    producers))
    (save-some-buffers t)
    producers))

(defun vino-test--generate-grapes (n filename)
  "Generate N grapes and store info in FILENAME."
  (let ((grapes
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-grape-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'vino-test--relativize-note
                    grapes))
    (save-some-buffers t)
    grapes))

(defun vino-test--generate-regions (n filename)
  "Generate N regions and store info in FILENAME."
  (let ((regions
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-region-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'vino-test--relativize-note
                    regions))
    (save-some-buffers t)
    regions))

(defun vino-test--generate-appellations (n filename)
  "Generate N appellations and store info in FILENAME."
  (let ((appellations
         (cl-loop
          repeat n
          for name = (random-name)
          for note = (vino-appellation-create name)
          collect note)))
    (print-to-file filename
                   (seq-map
                    #'vino-test--relativize-note
                    appellations))
    (save-some-buffers t)
    appellations))

(defun vino-test--generate-cellar (cellar-file
                                   ratings-file
                                   producers
                                   grapes
                                   regions
                                   appellations
                                   resources
                                   verbose)
  "Generate cellar entries and ratings for them.

Cellar entries are stored in CELLAR-FILE and ratings are stored
in RATINGS-FILE.

PRODUCERS, GRAPES, REGIONS, APPELLATIONS and RESOURCES are used
for generation.

When VERBOSE is non-nil, lots of garbage will be printed."
  (let* ((entries-with-ratings
          (seq-map
           (lambda (spec)
             (pcase-let ((`(,carbonation
                            ,sweetness
                            ,colour
                            ,vintage?
                            ,rora?
                            ,grapes-count
                            ,sugar?
                            ,resources-count
                            ,prices-count
                            ,ratings-count)
                          spec))
               (vino-test--generate-entry
                :producers producers
                :grapes grapes
                :regions regions
                :appellations appellations
                :resources resources
                :verbose verbose
                :carbonation carbonation
                :sweetness sweetness
                :colour colour
                :vintage? vintage?
                :rora? rora?
                :grapes-count grapes-count
                :sugar? sugar?
                :resources-count resources-count
                :prices-count prices-count
                :ratings-count ratings-count)))
           ;; Generate a Cartesian product of all required values.
           (-table-flat
            #'-snoc
            (cl-loop
             for carbonation in vino-carbonation-types
             nconc
             (cl-loop
              for sweetness in (plist-get vino-sweetness-levels carbonation)
              collect
              (list carbonation sweetness)))
            vino-colour-types
            '(nv random)
            '(appellation region)
            '(0 1 2)
            '(na random)
            '(0 1)
            '(0 1)
            '(1 0))))
         (entries (seq-map #'car entries-with-ratings))
         (ratings (seq-mapcat #'cdr entries-with-ratings)))
    (seq-each entries (lambda (x) (message "%s" x)))
    (print-to-file
     cellar-file
     (seq-map #'vino-test--relativize-entry entries))
    (print-to-file
     ratings-file
     (seq-map #'vino-test--relativize-rating ratings))
    entries))

(cl-defun vino-test--generate-entry (&key
                                     producers
                                     grapes
                                     regions
                                     appellations
                                     resources
                                     verbose
                                     carbonation
                                     sweetness
                                     colour
                                     vintage?
                                     rora?
                                     grapes-count
                                     sugar?
                                     resources-count
                                     prices-count
                                     ratings-count)
  "Generate a cellar entry and associated ratings.

Return a list of (`vino-entry' . (`vino-rating')).

Generation is based on CARBONATION, SWEETNESS, COLOUR, VINTAGE?,
RORA?, GRAPES-COUNT, SUGAR?, RESOURCES-COUNT, PRICES-COUNT,
RATINGS-COUNT.

Producer is randomly selected from PRODUCERS list.

Grapes are selected randomly from GRAPES list based on
GRAPES-COUNT value.

Region or appellation is selected based on RORA? value from
either REGIONS list or APPELLATIONS list.

Resources are randomly selected from RESOURCES list based on
RESOURCES-COUNT value.

When VERBOSE is non-nil, lots of garbage will be printed."
  (let ((t1 (current-time)))
    (when verbose
      (message (string-join
                '("creating cellar for:"
                  "    carbonation:     %s"
                  "    sweetness:       %s"
                  "    colour:          %s"
                  "    vintage:         %s"
                  "    rora:            %s"
                  "    grapes-count:    %s"
                  "    sugar:           %s"
                  "    resources-count: %s"
                  "    prices-count:    %s"
                  "    ratings-count:   %s")
                "\n")
               carbonation
               sweetness
               colour
               vintage?
               rora?
               grapes-count
               sugar?
               resources-count
               prices-count
               ratings-count))
    (let* ((acquired (random 10))
           (consumed (random (+ 1 acquired)))
           (vino-availability-fn (lambda (_) (cons acquired consumed)))
           (entry (make-vino-entry
                   :carbonation carbonation
                   :colour colour
                   :sweetness sweetness
                   :producer (nth (random (length producers)) producers)
                   :name (random-name)
                   :vintage (pcase vintage?
                              (`random (+ 1900 (random 121))))
                   :appellation (pcase rora?
                                  (`appellation (nth (random (length appellations)) appellations)))
                   :region (pcase rora?
                             (`region (nth (random (length regions)) regions)))
                   :grapes (seq-take (shuffle grapes) grapes-count)
                   :alcohol (+ 9.0 (/ (random 500) 100.0))
                   :sugar (pcase sugar?
                            (`random (/ (random 2400) 100.0)))
                   :resources (seq-take (shuffle resources) resources-count)
                   :price (cl-loop
                           repeat prices-count
                           collect (format
                                    "%.2f EUR"
                                    (+ 3.0 (/ (random 10000) 100.0))))
                   :acquired acquired
                   :consumed consumed
                   :rating nil
                   :ratings nil))
           (_ (when verbose (message "  ... entry = %s" entry)))
           (entry-note (vino-entry--create entry))
           (_ (when verbose (message "  ... entry id = %s" (vulpea-note-id entry-note))))
           (ratings (cl-loop
                     repeat ratings-count
                     collect
                     (make-vino-rating
                      :wine entry-note
                      :date (format "%s-%s-%s"
                                    (+ 1990 (random 35))
                                    (+ 1 (random 12))
                                    (+ 1 (random 28)))
                      :version 1
                      :values (list
                               (list "property_1" (random 4) 3)
                               (list "property_2" (random 5) 4)
                               (list "property_3" (random 3) 2)
                               (list "property_4" (random 6) 5)
                               (list "property_5" (random 7) 6)))))
           (ratings-notes (seq-map #'vino-rating--create ratings)))
      (when (> ratings-count 0)
        (setf (vino-entry-rating entry)
              (/ (seq-reduce (lambda (r x) (+ r (vino-rating-total x))) ratings 0)
                 (float (length ratings))))
        (setf (vino-entry-ratings entry)
              ratings-notes))
      (when verbose (message "  ... saving and killing remaining buffers"))
      (save-some-buffers 'ignore)
      (seq-each
       (seq-filter (lambda (b)
                     (and (buffer-file-name b)
                          (string-suffix-p ".org" (buffer-file-name b))))
                   (buffer-list))
       #'kill-buffer)
      (message "  ... entry generated in %s ms"
               (car (time-convert
                     (time-subtract (current-time) t1)
                     1000)))
      (cons entry ratings))))

(defun vino-test--relativize-note (n0)
  "Make `vulpea-note' N0 relative to `org-roam-directory'."
  (when n0
    (let ((n (copy-vulpea-note n0)))
      (setf (vulpea-note-path n)
            (string-remove-prefix org-roam-directory
                                  (vulpea-note-path n)))
      n)))

(defun vino-test--relativize-rating (r0)
  "Make `vino-rating' R0 relative to `org-roam-directory'."
  (let ((r (copy-vino-rating r0)))
    (setf (vino-rating-wine r)
          (vino-test--relativize-note (vino-rating-wine r)))
    r))

(defun vino-test--relativize-entry (e0)
  "Make `vino-entry' E0 relative to `org-roam-directory'."
  (let ((e (copy-vino-entry e0)))
    (setf (vino-entry-producer e)
          (vino-test--relativize-note (vino-entry-producer e)))
    (setf (vino-entry-appellation e)
          (vino-test--relativize-note (vino-entry-appellation e)))
    (setf (vino-entry-region e)
          (vino-test--relativize-note (vino-entry-region e)))
    (setf (vino-entry-grapes e)
          (seq-map #'vino-test--relativize-note (vino-entry-grapes e)))
    (setf (vino-entry-ratings e)
          (seq-map #'vino-test--relativize-note (vino-entry-ratings e)))
    e))



(provide 'vino-test-utils)
;;; vino-test-utils.el ends here
