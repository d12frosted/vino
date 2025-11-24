;;; vino-test.el --- Test `vino' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2022 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;;
;; Created: 09 Jan 2021
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
;; Test `vino' module.
;;
;;; Code:

(require 'buttercup)
(require 'vino)
(require 'vino-test-utils)

(describe "vino-entry-note-p"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

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
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns full information about selected wine"
    (spy-on
     'completing-read
     :and-return-value
     (completion-for :title "Arianna Occhipinti Bombolieri BB 2017"
                     :tags '("cellar")))
    (expect (vino-entry-note-select)
            :to-equal
            (mk-vulpea-note
             :type "cellar"
             :id "c9937e3e-c83d-4d8d-a612-6110e6706252"
             :title "Arianna Occhipinti Bombolieri BB 2017"
             :basename "c9937e3e-c83d-4d8d-a612-6110e6706252"
             :links '((:dest "9462dfad-603c-4094-9aca-a9042cec5dd2" :type "id")
                      (:dest "8353e2fc-8034-4540-8254-4b63fb5a421a" :type "id")
                      (:dest "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa" :type "id"))
             :meta '(("rating" "NA")
                     ("available" "1")
                     ("consumed" "1")
                     ("acquired" "2")
                     ("price" "50.00 EUR")
                     ("sugar" "1")
                     ("alcohol" "13")
                     ("grapes" "[[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]")
                     ("appellation" "[[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]")
                     ("vintage" "2017")
                     ("name" "Bombolieri BB")
                     ("producer" "[[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]")
                     ("sweetness" "dry")
                     ("colour" "red")
                     ("carbonation" "still"))))))

(describe "vino-entry-note-get-dwim"
  (before-all
    (setq vino-rating-props '((1 . (("score" 20)))))
    (vino-test-init))

  (after-all
    (vino-test-teardown)
    (setq vino-rating-props nil))

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

  (it "returns a note when used inside a rating note"
    (let* ((id "be7777a9-7993-44cf-be9e-0ae65297a35d")
           (note (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))
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

(describe "vino-grape-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new grape note"
    (let ((expected (mock-vulpea-note :type "grape" :title "Slarina"))
          (actual (vino-grape-create "Slarina")))
      ;; Compare notes without attach-dir (set asynchronously in Vulpea V2)
      (expect (vulpea-note-id actual) :to-equal (vulpea-note-id expected))
      (expect (vulpea-note-path actual) :to-equal (vulpea-note-path expected))
      (expect (vulpea-note-title actual) :to-equal (vulpea-note-title expected))
      (expect (vulpea-note-tags actual) :to-equal (vulpea-note-tags expected)))))

(describe "vino-grape-select"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns full information about selected grape"
    (spy-on
     'completing-read
     :and-return-value
     (completion-for :title "Frappato"
                     :tags '("grape")))
    (expect (vino-grape-select)
            :to-equal
            (mk-vulpea-note
             :type "grape"
             :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
             :title "Frappato")))

  (it "creates a new grape note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Slarina" "Create new grape"))
    (let ((expected (mock-vulpea-note :type "grape" :title "Slarina"))
          (actual (vino-grape-select)))
      ;; Compare notes without attach-dir (set asynchronously in Vulpea V2)
      (expect (vulpea-note-id actual) :to-equal (vulpea-note-id expected))
      (expect (vulpea-note-path actual) :to-equal (vulpea-note-path expected))
      (expect (vulpea-note-title actual) :to-equal (vulpea-note-title expected))
      (expect (vulpea-note-tags actual) :to-equal (vulpea-note-tags expected))))

  (it "adds a synonym when selecting non-existing name"
    (spy-on 'completing-read
            :and-return-values
            (list
             "Frappato di Vittoria"
             "Add a synonym to existing grape"
             (completion-for :title "Frappato" :tags '("grape"))))
    (expect (vino-grape-select)
            :to-equal
            (mk-vulpea-note :type "grape"
                            :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
                            :title "Frappato di Vittoria"
                            :basename "frappato"))
    (expect (expand-file-name "wine/grape/frappato.org"
                              vulpea-default-notes-directory)
            :to-contain-exactly
            ":PROPERTIES:
:ID:       cb1eb3b9-6233-4916-8c05-a3a4739e0cfa
:ALIASES:  \"Frappato di Vittoria\"
:END:
#+title: Frappato
#+filetags: :wine:grape:
")))

(describe "vino-producer-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new producer note"
    (let ((expected (mock-vulpea-note :type "producer" :title "Vino di Anna"))
          (actual (vino-producer-create "Vino di Anna")))
      ;; Compare notes without attach-dir (set asynchronously in Vulpea V2)
      (expect (vulpea-note-id actual) :to-equal (vulpea-note-id expected))
      (expect (vulpea-note-path actual) :to-equal (vulpea-note-path expected))
      (expect (vulpea-note-title actual) :to-equal (vulpea-note-title expected))
      (expect (vulpea-note-tags actual) :to-equal (vulpea-note-tags expected)))))

(describe "vino-producer-select"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns full information about selected producer"
    (spy-on
     'completing-read
     :and-return-value
     (completion-for :title "Arianna Occhipinti"
                     :tags '("producer")))
    (expect (vino-producer-select)
            :to-equal
            (mk-vulpea-note
             :type "producer"
             :id "9462dfad-603c-4094-9aca-a9042cec5dd2"
             :title "Arianna Occhipinti")))

  (it "creates a new producer note when selecting non-existing name"
    (spy-on
     'completing-read
     :and-return-value
     "Vino di Anna")
    (spy-on 'y-or-n-p :and-return-value t)
    (let ((expected (mock-vulpea-note :type "producer" :title "Vino di Anna"))
          (actual (vino-producer-select)))
      ;; Compare notes without attach-dir (set asynchronously in Vulpea V2)
      (expect (vulpea-note-id actual) :to-equal (vulpea-note-id expected))
      (expect (vulpea-note-path actual) :to-equal (vulpea-note-path expected))
      (expect (vulpea-note-title actual) :to-equal (vulpea-note-title expected))
      (expect (vulpea-note-tags actual) :to-equal (vulpea-note-tags expected)))))

(describe "vino-country-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new country note"
    (expect (mock-vulpea-note :type "country" :title "Vino Republic")
            :to-equal
            (vino-country-create :title "Vino Republic"))))

(describe "vino-region-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new region note"
    (expect (mock-vulpea-note
             :type "region"
             :title "Codru"
             :basename-prefix "new_zealand/"
             :links '((:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id")
                      (:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id"))
             :meta '(("parent" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")
                     ("country" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")))
            :to-equal
            (vino-region-create
             :title "Codru"
             :country (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9")
             :parent (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9")))))

(describe "vino-appellation-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new appellation note"
    (expect (mock-vulpea-note
             :type "appellation"
             :title "Gattinara DOCG"
             :basename-prefix "new_zealand/"
             :links '((:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id")
                      (:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id"))
             :meta '(("parent" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")
                     ("country" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")))
            :to-equal
            (vino-appellation-create
             :title "Gattinara DOCG"
             :country (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9")
             :parent (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9")))))

(describe "vino-region-select"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns full information about selected region"
    (spy-on 'completing-read
            :and-return-value
            (completion-for :title "Central Otago" :tags '("region")))
    (expect (vino-region-select)
            :to-equal
            (mk-vulpea-note
             :type "region"
             :id "f9ef759b-f39e-4121-ab19-9ab3daa318be"
             :title "Central Otago")))

  (it "returns full information about selected appellation"
    (spy-on 'completing-read
            :and-return-value
            (completion-for :title "Cerasuolo di Vittoria DOCG" :tags '("appellation")))
    (expect (vino-appellation-select)
            :to-equal
            (mk-vulpea-note
             :type "appellation"
             :id "6a0819f3-0770-4481-9754-754ca397800b"
             :title "Cerasuolo di Vittoria DOCG"
             :links '((:dest "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa" :type "id")
                      (:dest "3b38917f-6065-42e8-87ca-33dd39a92fc0" :type "id")))))

  (it "creates a new region note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Codru" "Create region"))
    (spy-on 'read-string :and-return-value "")
    (spy-on 'vino--repeat-while :and-return-value (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))
    (expect (mock-vulpea-note
             :type "region"
             :title "Codru"
             :basename-prefix "new_zealand/"
             :links '((:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id")
                      (:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id"))
             :meta '(("parent" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")
                     ("country" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")))
            :to-equal
            (vino-region-select
             :country (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))))

  (it "creates a new appellation note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Gattinara DOCG" "Create appellation"))
    (spy-on 'read-string :and-return-value "")
    (spy-on 'vino--repeat-while :and-return-value (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))
    (expect (mock-vulpea-note
             :type "appellation"
             :title "Gattinara DOCG"
             :basename-prefix "new_zealand/"
             :links '((:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id")
                      (:dest "437298dc-39d9-42e6-8d0f-1838e9a007f9" :type "id"))
             :meta '(("parent" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")
                     ("country" "[[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]")))
            :to-equal
            (vino-appellation-select
             :country (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9")))))

(describe "vino-entry-set-grapes"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "replace grapes metadata with new data"
    (vino-entry-set-grapes id '("1c436b3b-ad14-4818-896d-1b7755f10fa1"
                                "3b38917f-6065-42e8-87ca-33dd39a92fc0"))
    (expect (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "grapes" 'note)
            :to-equal
            (list (vulpea-db-get-by-id "1c436b3b-ad14-4818-896d-1b7755f10fa1")
                  (vulpea-db-get-by-id "3b38917f-6065-42e8-87ca-33dd39a92fc0")))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") vulpea-default-notes-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Arianna Occhipinti Bombolieri BB 2017
#+filetags: :wine:cellar:

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
:ID:       71715128-3d6f-4e36-8d70-d35fcb057609
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
        note)
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "replace region metadata"
    (setq vino-origin-select-fn
          (lambda ()
            `(("country" . ,(vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))
              ("region" . ,(vulpea-db-get-by-id "f9ef759b-f39e-4121-ab19-9ab3daa318be")))))

    ;; clean old values
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (vulpea-buffer-meta-remove "appellation")
      (vulpea-buffer-meta-remove "region")
      (save-buffer))

    ;; set region/appellation
    (vino-entry-set-region id)
    (setq note (vulpea-db-get-by-id id))
    (expect (vulpea-note-meta-get note "region" 'note)
            :to-equal
            (vulpea-db-get-by-id "f9ef759b-f39e-4121-ab19-9ab3daa318be"))
    (expect (vulpea-note-meta-get note "appellation" 'note) :to-equal nil)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") vulpea-default-notes-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Arianna Occhipinti Bombolieri BB 2017
#+filetags: :wine:cellar:

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- country :: [[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]
- region :: [[id:f9ef759b-f39e-4121-ab19-9ab3daa318be][Central Otago]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 2
- consumed :: 1
- available :: 1
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
:ID:       71715128-3d6f-4e36-8d70-d35fcb057609
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
    (setq vino-origin-select-fn
          (lambda ()
            `(("country" . ,(vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))
              ("appellation" . ,(vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be")))))

    ;; clean old values
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (vulpea-buffer-meta-remove "appellation")
      (vulpea-buffer-meta-remove "region")
      (save-buffer))

    ;; set region/appellation
    (vino-entry-set-region id)
    (setq note (vulpea-db-get-by-id id))
    (expect (vulpea-note-meta-get note "region" 'note) :to-equal nil)
    (expect (vulpea-note-meta-get note "appellation" 'note)
            :to-equal
            (vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be"))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") vulpea-default-notes-directory)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Arianna Occhipinti Bombolieri BB 2017
#+filetags: :wine:cellar:

- carbonation :: still
- colour :: red
- sweetness :: dry
- producer :: [[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]
- name :: Bombolieri BB
- vintage :: 2017
- country :: [[id:437298dc-39d9-42e6-8d0f-1838e9a007f9][New Zealand]]
- appellation :: [[id:860f5505-d83c-4305-bc20-cb6a92f5d0be][Etna DOC]]
- grapes :: [[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]
- alcohol :: 13
- sugar :: 1
- price :: 50.00 EUR
- acquired :: 2
- consumed :: 1
- available :: 1
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
:ID:       71715128-3d6f-4e36-8d70-d35fcb057609
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
    (vino-test-init))

  (after-all
    (setq vino-rating-props nil)
    (vino-test-teardown))

  (it "should update rating to average of ratings"
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (vulpea-buffer-meta-set "ratings" '("f1ecb856-c009-4a65-a8d0-8191a9de66dd"
                                          "be7777a9-7993-44cf-be9e-0ae65297a35d"))
      (save-buffer)
      (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))
    (vino-entry-update id)
    (expect (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "ratings" 'note)
            :to-equal
            (list (vulpea-db-get-by-id "be7777a9-7993-44cf-be9e-0ae65297a35d")
                  (vulpea-db-get-by-id "f1ecb856-c009-4a65-a8d0-8191a9de66dd")))
    (expect (vulpea-note-meta-get (vulpea-db-get-by-id id) "rating" 'number) :to-equal 4.0)))

(describe "vino-rating--read-value"
  (it "should support number"
    (spy-on #'read-number :and-return-value 4)
    (expect (vino-rating--read-value (cons "prop_number" 10))
            :to-equal '("prop_number" 4 10)))

  (it "should clip numbers - left"
    (spy-on #'read-number :and-return-value -100)
    (expect (vino-rating--read-value (cons "prop_number" 10))
            :to-equal '("prop_number" 0 10)))

  (it "should clip numbers - right"
    (spy-on #'read-number :and-return-value 1000)
    (expect (vino-rating--read-value (cons "prop_number" 10))
            :to-equal '("prop_number" 10 10)))

  (it "should support list"
    (spy-on #'completing-read :and-return-value "avg")
    (expect (vino-rating--read-value (cons "prop_list" '(("max" . 2) ("avg" . 1) ("min" . 0))))
            :to-equal '("prop_list" 1 2)))

  (it "should support function"
    (expect (vino-rating--read-value (cons "prop_function" (lambda () (cons 42 100))))
            :to-equal '("prop_function" 42 100))))

(describe "vino-rating--create"
  :var* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
         (date (current-time))
         (date-str (format-time-string "%Y-%m-%d" date))
         rating
         note)
  (before-each
    (vino-test-init)
    (setq vino-rating-props
          '((1 . (("score" 20)))
            (4 . (("property_1" 3)
                  ("property_2" 4)
                  ("property_3" 2)
                  ("property_4" 5)
                  ("property_5" 6))))))

  (after-each
    (setq vino-rating-props nil)
    (vino-test-teardown))

  (it "should create rating note and update vino note"
    (setq rating (make-vino-rating
                  :wine (vulpea-db-get-by-id id)
                  :date date-str
                  :version 4
                  :values '(("property_1" 3 3)
                            ("property_2" 3 4)
                            ("property_3" 0 2)
                            ("property_4" 5 5)
                            ("property_5" 5 6))))
    (setq note (vino-rating--create rating))
    (setf (vino-rating-wine rating) (vulpea-db-get-by-id id))
    (expect note :not :to-be nil)
    (expect (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "ratings" 'note) :to-equal (list note))
    (expect (vulpea-note-meta-get (vulpea-db-get-by-id id) "rating" 'number) :to-equal 4.0)
    (expect (vino-rating-get-by-id (vulpea-note-id note)) :to-equal rating)
    (expect (vulpea-db-get-file-by-id id)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Arianna Occhipinti Bombolieri BB 2017
#+filetags: :wine:cellar:

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
- acquired :: 2
- consumed :: 1
- available :: 1
- rating :: 4.0
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
:ID:       71715128-3d6f-4e36-8d70-d35fcb057609
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
:ID:       %s
:END:
#+title: Arianna Occhipinti Bombolieri BB 2017 - %s
#+filetags: :wine:rating:

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
- score :: 16.0
- score_max :: 20.0
- total :: 4.0
"
             (vulpea-note-id note)
             date-str
             id
             date-str))))

(describe "vino--collect-while"
  (it "repeats a function until filter returns nil"
    (let ((n 0))
      (expect (vino--collect-while
               (lambda () (setq n (+ 1 n)))
               (lambda (v) (< v 5)))
              :to-equal '(1 2 3 4)))))

(describe "vino--repeat-while"
  (it "repeats a function until filter returns nil"
    (let ((n 0))
      (expect (vino--repeat-while
               (lambda () (setq n (+ 1 n)))
               (lambda (v) (< v 5)))
              :to-equal 5))))

(provide 'vino-test)
;;; vino-test.el ends here
