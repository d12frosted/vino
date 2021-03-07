;;; vino-test.el --- Test `vino' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
    (vulpea-setup)
    (vino-setup)
    (org-roam-db-build-cache)))

(defun vino-test--teardown ()
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

(describe "vino-entry-note-p"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns non-nil when used on of wine entry"
    (expect
     (vino-entry-note-p
      (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
     :to-be t))

  (it "returns nil when used on some heading inside wine entry"
    (expect
     (vino-entry-note-p
      (vulpea-db-get-by-id "71715128-3d6f-4e36-8d70-d35fcb057609"))
     :to-be nil))

  (it "returns nil when used on other entry"
    (expect
     (vino-entry-note-p
      (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
     :to-be nil)))

(describe "vino-entry-note-select"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected wine"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,cellar) Arianna Occhipinti Bombolieri BB 2017")
    (expect (vino-entry-note-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name "wine/cellar/c9937e3e-c83d-4d8d-a612-6110e6706252.org" org-roam-directory)
             :title "Arianna Occhipinti Bombolieri BB 2017"
             :tags '("wine" "cellar")
             :level 0
             :id "c9937e3e-c83d-4d8d-a612-6110e6706252"))))

(describe "vino-entry-note-get-dwim"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns a note when used on id of wine entry"
    (expect
     (vino-entry-note-get-dwim "c9937e3e-c83d-4d8d-a612-6110e6706252")
     :to-equal
     (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))

  (it "returns a note when used on wine note"
    (expect
     (vino-entry-note-get-dwim
      (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
     :to-equal
     (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))

  (it "returns a note when used inside a wine note"
    (let* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
           (note (vulpea-db-get-by-id id)))
      (expect
       (vulpea-utils-with-file (vulpea-db-get-file-by-id id)
         (goto-char (point-max))
         (vino-entry-note-get-dwim))
       :to-equal note)))

  (it "calls `vino-entry-note-select' when called from non org-mode buffer"
    (let ((note (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))
      (spy-on 'vino-entry-note-select :and-return-value note)
      (expect (vino-entry-note-get-dwim) :to-equal note)))

  (it "calls `vino-entry-note-select' when current buffer does not represent wine"
    (let* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
           (note (vulpea-db-get-by-id id)))
      (spy-on 'vino-entry-note-select :and-return-value note)
      (expect
       (vulpea-utils-with-file (vulpea-db-get-file-by-id id)
         (vino-entry-note-get-dwim))
       :to-equal note)))

  (it "throws an error when used on id of some heading inside wine entry"
    (let ((id "71715128-3d6f-4e36-8d70-d35fcb057609"))
      (expect (vino-entry-note-get-dwim id)
              :to-throw
              'user-error
              (list
               (format "Note with id %s does not represent a vino entry"
                       id)))))

  (it "throws an error when used on some heading inside wine entry"
    (let ((id "71715128-3d6f-4e36-8d70-d35fcb057609"))
      (expect (vino-entry-note-get-dwim (vulpea-db-get-by-id id))
              :to-throw
              'user-error
              (list
               (format "Note %s does not represent a vino entry"
                       (vulpea-db-get-by-id id))))))

  (it "throws an error when used on id of other note"
    (let ((id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
      (expect (vino-entry-note-get-dwim id)
              :to-throw
              'user-error
              (list
               (format "Note with id %s does not represent a vino entry"
                       id)))))

  (it "throws an error when used on other note"
    (let ((id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
      (expect (vino-entry-note-get-dwim (vulpea-db-get-by-id id))
              :to-throw
              'user-error
              (list
               (format "Note %s does not represent a vino entry"
                       (vulpea-db-get-by-id id)))))))

(describe "vino-entry--create"
  :var (note vino)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new entry with all information"
    (setq vino (make-vino-entry
                :carbonation 'still
                :colour 'red
                :sweetness 'dry
                :producer (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2")
                :name "Grotte Alte"
                :vintage 2014
                :appellation (vulpea-db-get-by-id "6a0819f3-0770-4481-9754-754ca397800b")
                :grapes (list (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
                              (vulpea-db-get-by-id "3b38917f-6065-42e8-87ca-33dd39a92fc0"))
                :alcohol 13
                :sugar 0
                :acquired 0
                :consumed 0
                :resources '("http://www.agricolaocchipinti.it/it/grotte-alte"
                             "https://www.bowlerwine.com/wine-or-spirit/grotte-alte-cerasuolo-di-vittoria-riserva")
                :price '("50.00 EUR")))
    (setq note (vino-entry--create vino))
    (expect (vino-entry-get-by-id (vulpea-note-id note)) :to-equal vino)
    (expect (expand-file-name (vulpea-note-path note))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Grotte Alte 2014

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
- price :: 50.00 EUR
- acquired :: 0
- consumed :: 0
- available :: 0
- resources :: [[http://www.agricolaocchipinti.it/it/grotte-alte][agricolaocchipinti.it]]
- resources :: [[https://www.bowlerwine.com/wine-or-spirit/grotte-alte-cerasuolo-di-vittoria-riserva][bowlerwine.com]]
- rating :: NA


"
             (vulpea-note-id note)))))

(describe "vino-entry-get-by-id"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns an existing `vino'"
    (expect (vino-entry-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")
            :to-equal
            (make-vino-entry
             :carbonation 'still
             :colour 'red
             :sweetness 'dry
             :producer (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2")
             :name "Bombolieri BB"
             :vintage 2017
             :appellation (vulpea-db-get-by-id "8353e2fc-8034-4540-8254-4b63fb5a421a")
             :grapes (list (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
             :alcohol 13
             :sugar 1
             :acquired 2
             :consumed 1
             :resources '("http://www.agricolaocchipinti.it/it/vinicontrada")
             :price '("50.00 EUR"))))

  (it "returns nil for non-wine note id"
    (expect (vino-entry-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
            :to-equal nil))

  (it "returns nil for non-existing id"
    (expect (vino-entry-get-by-id (org-id-new))
            :to-equal nil)))

(describe "vino-grape-create"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new grape note"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (spy-on 'read-string :and-return-value "")
    (expect (vino-grape-create "Slarina")
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/grape/%s-slarina.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Slarina"
             :tags '("wine" "grape")
             :level 0
             :id id))))

(describe "vino-grape-select"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected grape"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,grape) Frappato")
    (expect (vino-grape-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name "wine/grape/frappato.org" org-roam-directory)
             :title "Frappato"
             :tags '("wine" "grape")
             :level 0
             :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")))

  (it "creates a new grape note when selecting non-existing name"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (spy-on 'org-roam-completion--completing-read :and-return-value "Slarina")
    (spy-on 'y-or-n-p :and-return-value t)
    (spy-on 'read-string :and-return-value "")
    (expect (vino-grape-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/grape/%s-slarina.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Slarina"
             :tags '("wine" "grape")
             :level 0
             :id id))))

(describe "vino-producer-create"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new producer note"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (expect (vino-producer-create "Vino di Anna")
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/producer/%s-vino_di_anna.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Vino di Anna"
             :tags '("wine" "producer")
             :level 0
             :id id))))

(describe "vino-producer-select"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected producer"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,producer) Arianna Occhipinti")
    (expect (vino-producer-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name "wine/producer/arianna_occhipinti.org" org-roam-directory)
             :title "Arianna Occhipinti"
             :tags '("wine" "producer")
             :level 0
             :id "9462dfad-603c-4094-9aca-a9042cec5dd2")))

  (it "creates a new producer note when selecting non-existing name"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (spy-on 'org-roam-completion--completing-read :and-return-value "Vino di Anna")
    (spy-on 'y-or-n-p :and-return-value t)
    (expect (vino-producer-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/producer/%s-vino_di_anna.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Vino di Anna"
             :tags '("wine" "producer")
             :level 0
             :id id))))

(describe "vino-region-create"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new region note"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (expect (vino-region-create "Codru")
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/region/%s-codru.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Codru"
             :tags '("wine" "region")
             :level 0
             :id id))))

(describe "vino-appellation-create"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new appellation note"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (expect (vino-appellation-create "Gattinara DOCG")
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/appellation/%s-gattinara_docg.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Gattinara DOCG"
             :tags '("wine" "appellation")
             :level 0
             :id id))))

(describe "vino-region-select"
  :var (id ts)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns full information about selected region"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,region) Central Otago")
    (expect (vino-region-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name "wine/region/central_otago.org" org-roam-directory)
             :title "Central Otago"
             :tags '("wine" "region")
             :level 0
             :id "f9ef759b-f39e-4121-ab19-9ab3daa318be")))

  (it "returns full information about selected appellation"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(wine,appellation) Cerasuolo di Vittoria DOCG")
    (expect (vino-region-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name "wine/appellation/cerasuolo_di_vittoria_docg.org" org-roam-directory)
             :title "Cerasuolo di Vittoria DOCG"
             :tags '("wine" "appellation")
             :level 0
             :id "6a0819f3-0770-4481-9754-754ca397800b")))

  (it "creates a new region note when selecting non-existing name"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (spy-on 'org-roam-completion--completing-read :and-return-value "Codru")
    (spy-on 'completing-read :and-return-value "Create region")
    (spy-on 'read-string :and-return-value "")
    (expect (vino-region-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/region/%s-codru.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Codru"
             :tags '("wine" "region")
             :level 0
             :id id)))

  (it "creates a new appellation note when selecting non-existing name"
    (setq id (org-id-new)
          ts (current-time))
    (spy-on 'org-id-new :and-return-value id)
    (spy-on 'current-time :and-return-value ts)
    (spy-on 'org-roam-completion--completing-read :and-return-value "Gattinara DOCG")
    (spy-on 'completing-read :and-return-value "Create appellation")
    (spy-on 'read-string :and-return-value "")
    (expect (vino-region-select)
            :to-be-note-like
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/appellation/%s-gattinara_docg.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Gattinara DOCG"
             :tags '("wine" "appellation")
             :level 0
             :id id))))

(describe "vino-entry-consume"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        (initial-in 5)
        (initial-out 2)
        (extra-out 0)
        vino)
  (before-all
    (vino-test--init)
    (setq vino-availability-sub-fn (lambda (_id amount _source _date)
                                     (setq  extra-out amount))
          vino-availability-fn (lambda (_) (cons initial-in (+ initial-out extra-out)))))

  (after-all
    (vino-test--teardown)
    (setq vino-availability-sub-fn nil
          vino-availability-fn nil))

  (it "updates availability based on vino-availability-sub-fn result"
    (spy-on 'y-or-n-p :and-return-value nil)
    (vino-entry-consume id 3 "consume" (current-time))
    (setq vino (vino-entry-get-by-id id))
    (expect 'y-or-n-p :to-have-been-called-times 1)
    (expect (vino-entry-acquired vino) :to-equal 5)
    (expect (vino-entry-consumed vino) :to-equal 5)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 5
- consumed :: 5
- available :: 0
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id))))

(describe "vino-entry-acquire"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        (initial-in 2)
        (extra-in 0)
        vino)
  (before-all
    (vino-test--init)
    (setq vino-availability-add-fn (lambda (_id amount _source _date)
                                     (setq  extra-in amount))
          vino-availability-fn (lambda (_) (cons(+ initial-in extra-in) 2))))

  (after-all
    (vino-test--teardown)
    (setq vino-availability-add-fn nil
          vino-availability-fn nil))

  (it "updates availability based on vino-availability-add-fn result"
    (vino-entry-acquire id 3 "some source" "50.00 EUR" (current-time))
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-acquired vino) :to-equal 5)
    (expect (vino-entry-consumed vino) :to-equal 2)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 5
- consumed :: 2
- available :: 3
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id))))

(describe "vino-entry-set-grapes"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "replace grapes metadata with new data"
    (vino-entry-set-grapes id '("1c436b3b-ad14-4818-896d-1b7755f10fa1"
                                "3b38917f-6065-42e8-87ca-33dd39a92fc0"))
    (expect (vino-entry-grapes (vino-entry-get-by-id id))
            :to-equal
            (list (vulpea-db-get-by-id "1c436b3b-ad14-4818-896d-1b7755f10fa1")
                  (vulpea-db-get-by-id "3b38917f-6065-42e8-87ca-33dd39a92fc0")))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
- grapes :: [[id:1c436b3b-ad14-4818-896d-1b7755f10fa1][Nerello Mascalese]]
- grapes :: [[id:3b38917f-6065-42e8-87ca-33dd39a92fc0][Nero d'Avola]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 2
- consumed :: 1
- available :: 1
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id))))

(describe "vino-entry-set-region"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        vino)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "replace region metadata"
    (vino-entry-set-region id "f9ef759b-f39e-4121-ab19-9ab3daa318be")
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-region vino) :to-equal (vulpea-db-get-by-id "f9ef759b-f39e-4121-ab19-9ab3daa318be"))
    (expect (vino-entry-appellation vino) :to-equal nil)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 2
- consumed :: 1
- available :: 1
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA
- region :: [[id:f9ef759b-f39e-4121-ab19-9ab3daa318be][Central Otago]]

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id)))

  (it "replace appellation metadata"
    (vino-entry-set-region id "860f5505-d83c-4305-bc20-cb6a92f5d0be")
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-region vino) :to-equal nil)
    (expect (vino-entry-appellation vino) :to-equal (vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be"))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 2
- consumed :: 1
- available :: 1
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA
- appellation :: [[id:860f5505-d83c-4305-bc20-cb6a92f5d0be][Etna DOC]]

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id))))

(describe "vino-entry-update-availability"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        vino)
  (before-all
    (vino-test--init)
    (setq vino-availability-fn (lambda (_) (cons 10 8))))

  (after-all
    (vino-test--teardown)
    (setq vino-availability-fn nil))

  (it "updates availability based on vino-availability-fn result"
    (vino-entry-update-availability id)
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-acquired vino) :to-equal 10)
    (expect (vino-entry-consumed vino) :to-equal 8)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 10
- consumed :: 8
- available :: 2
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: NA

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id))))

(describe "vino-entry-update"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
  (before-all
    (vino-test--init)
    (setq vino-rating-props '((1 . (("score" 20))))
          vino-availability-fn (lambda (_) (cons 5 2))))

  (after-all
    (vino-test--teardown)
    (setq vino-rating-props nil
          vino-availability-fn nil))

  (it "should update rating to average of ratings"
    (vulpea-meta-set id "ratings" '("f1ecb856-c009-4a65-a8d0-8191a9de66dd"
                                    "be7777a9-7993-44cf-be9e-0ae65297a35d"))
    (vino-entry-update id)
    (expect (vino-entry-get-by-id id)
            :to-equal
            (make-vino-entry
             :carbonation 'still
             :colour 'red
             :sweetness 'dry
             :producer (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2")
             :name "Bombolieri BB"
             :vintage 2017
             :appellation (vulpea-db-get-by-id "8353e2fc-8034-4540-8254-4b63fb5a421a")
             :grapes (list (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
             :alcohol 13
             :sugar 1
             :acquired 5
             :consumed 2
             :resources '("http://www.agricolaocchipinti.it/it/vinicontrada")
             :price '("50.00 EUR")
             :rating 8.0
             :ratings (list (vulpea-db-get-by-id "be7777a9-7993-44cf-be9e-0ae65297a35d")
                            (vulpea-db-get-by-id "f1ecb856-c009-4a65-a8d0-8191a9de66dd"))))))

(describe "vino-rating--create"
  :var* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
         (date (current-time))
         (date-str (format-time-string "%Y-%m-%d" date))
         note)
  (before-all
    (vino-test--init)
    (setq vino-rating-props '((4 . (("property_1" 3)
                                    ("property_2" 4)
                                    ("property_3" 2)
                                    ("property_4" 5)
                                    ("property_5" 6))))
          vino-availability-fn (lambda (_) (cons 5 2))))

  (after-all
    (vino-test--teardown)
    (setq vino-rating-props nil
          vino-availability-fn nil))

  (it "should create rating note and update vino note"
    (setq note (vino-rating--create
                id date 4
                '(("property_1" 3 3)
                  ("property_2" 3 4)
                  ("property_3" 0 2)
                  ("property_4" 5 5)
                  ("property_5" 5 6))))
    (expect (vino-entry-get-by-id id)
            :to-equal
            (make-vino-entry
             :carbonation 'still
             :colour 'red
             :sweetness 'dry
             :producer (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2")
             :name "Bombolieri BB"
             :vintage 2017
             :appellation (vulpea-db-get-by-id "8353e2fc-8034-4540-8254-4b63fb5a421a")
             :grapes (list (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
             :alcohol 13
             :sugar 1
             :acquired 5
             :consumed 2
             :resources '("http://www.agricolaocchipinti.it/it/vinicontrada")
             :price '("50.00 EUR")
             :rating 8.0
             :ratings (list note)))
    (expect (vulpea-db-get-file-by-id id)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- appellation :: [[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 5
- consumed :: 2
- available :: 3
- resources :: [[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]
- rating :: 8.0
- ratings :: [[id:%s][Arianna Occhipinti Bombolieri BB 2017 - %s]]

#+begin_quote
Il Frappato stems from a dream which I had when I was a girl to make a wine that
knows the land that I work, the air I breath, and my own thoughts. It is bitter,
bloody and elegant. That is Vittoria and the Iblei Mountains. It is the wine
that most resembles me, brave, original and rebellious. But not only. It has
peasant origins, for this it loves its roots and the past that it brings in;
but, at the same time, it is able to fight to improve itself. It knows
refinement without forgetting itself.

Arianna Occhipinti
#+end_quote

* Additional information
:PROPERTIES:
:ID:                     71715128-3d6f-4e36-8d70-d35fcb057609
:END:

Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tincidunt urna id
consequat pulvinar. Nullam ac dapibus arcu. Phasellus ornare tincidunt justo in
tincidunt. Vestibulum dignissim arcu erat, in viverra ligula tristique vel.
Etiam ac euismod lacus. Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Phasellus nec urna sit amet arcu laoreet sagittis ac et dolor. Sed molestie mi
dui, eu posuere diam faucibus eget. Nullam fringilla ante in laoreet
scelerisque. Nam feugiat neque id odio accumsan sodales. Quisque eu nibh diam.
Aliquam varius, nibh vel pretium molestie, velit lorem consectetur erat, quis
pretium eros dui eu eros. Vestibulum at turpis lacus. Donec tempor nec ipsum sed
dictum. Quisque suscipit neque dui, in efficitur quam interdum ut.
"
             id
             (vulpea-note-id note)
             date-str))
    (expect (vulpea-note-path note)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:                     %s
:END:
#+TITLE: Arianna Occhipinti Bombolieri BB 2017 - %s

- wine :: [[id:%s][Arianna Occhipinti Bombolieri BB 2017]]
- date :: %s
- version :: 4
- property_1 :: 3
- property_1_max :: 3
- property_2 :: 3
- property_2_max :: 4
- property_3 :: 0
- property_3_max :: 2
- property_4 :: 5
- property_4_max :: 5
- property_5 :: 5
- property_5_max :: 6
- score :: 16
- score_max :: 20
- total :: 8.0


"
             (vulpea-note-id note)
             date-str
             id
             date-str))))

(provide 'vino-test)
;;; vino-test.el ends here
