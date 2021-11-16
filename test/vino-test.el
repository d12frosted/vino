;;; vino-test.el --- Test `vino' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
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
            :to-be-note
            (mk-vulpea-note
             :type "cellar"
             :id "c9937e3e-c83d-4d8d-a612-6110e6706252"
             :title "Arianna Occhipinti Bombolieri BB 2017"
             :basename "c9937e3e-c83d-4d8d-a612-6110e6706252"
             :links '(("id" . "9462dfad-603c-4094-9aca-a9042cec5dd2")
                      ("id" . "8353e2fc-8034-4540-8254-4b63fb5a421a")
                      ("id" . "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
                      ("http" . "http://www.agricolaocchipinti.it/it/vinicontrada"))
             :meta '(("carbonation" "still")
                     ("colour" "red")
                     ("sweetness" "dry")
                     ("producer" "[[id:9462dfad-603c-4094-9aca-a9042cec5dd2][Arianna Occhipinti]]")
                     ("name" "Bombolieri BB")
                     ("vintage" "2017")
                     ("appellation" "[[id:8353e2fc-8034-4540-8254-4b63fb5a421a][IGP Terre Siciliane]]")
                     ("grapes" "[[id:cb1eb3b9-6233-4916-8c05-a3a4739e0cfa][Frappato]]")
                     ("alcohol" "13")
                     ("sugar" "1")
                     ("price" "50.00 EUR")
                     ("acquired" "2")
                     ("consumed" "1")
                     ("available" "1")
                     ("resources" "[[http://www.agricolaocchipinti.it/it/vinicontrada][agricolaocchipinti.it]]")
                     ("rating" "NA"))))))

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

(describe "vino-entry--create"
  :var (note vino)
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

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
    (expect (vino-db-get-entry (vulpea-note-id note)) :to-equal vino)
    (expect (expand-file-name (vulpea-note-path note))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Arianna Occhipinti Grotte Alte 2014
#+filetags: :wine:cellar:

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
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

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
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new grape note"
    (expect (mock-vulpea-note :type "grape" :title "Slarina")
            :to-be-note
            (vino-grape-create "Slarina"))))

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
            :to-be-note
            (mk-vulpea-note
             :type "grape"
             :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
             :title "Frappato")))

  (it "creates a new grape note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Slarina" "Create new grape"))
    (expect (mock-vulpea-note :type "grape" :title "Slarina")
            :to-be-note
            (vino-grape-select)))

  (it "adds a synonym when selecting non-existing name"
    (spy-on 'completing-read
            :and-return-values
            (list
             "Frappato di Vittoria"
             "Add a synonym to existing grape"
             (completion-for :title "Frappato" :tags '("grape"))))
    (expect (vino-grape-select)
            :to-be-note
            (mk-vulpea-note :type "grape"
                            :id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
                            :title "Frappato di Vittoria"
                            :basename "frappato"))
    (expect (expand-file-name "wine/grape/frappato.org"
                              org-roam-directory)
            :to-contain-exactly
            ":PROPERTIES:
:ID:       cb1eb3b9-6233-4916-8c05-a3a4739e0cfa
:ROAM_ALIASES: \"Frappato di Vittoria\"
:END:
#+title: Frappato
#+filetags: :wine:grape:
")))

(describe "vino-producer-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new producer note"
    (expect (mock-vulpea-note :type "producer" :title "Vino di Anna")
            :to-be-note
            (vino-producer-create "Vino di Anna")
            )))

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
            :to-be-note
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
    (expect (mock-vulpea-note :type "producer" :title "Vino di Anna")
            :to-be-note
            (vino-producer-select))))

(describe "vino-region-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new region note"
    (expect (mock-vulpea-note :type "region" :title "Codru")
            :to-be-note
            (vino-region-create "Codru"))))

(describe "vino-appellation-create"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "creates a new appellation note"
    (expect (mock-vulpea-note :type "appellation" :title "Gattinara DOCG")
            :to-be-note
            (vino-appellation-create "Gattinara DOCG"))))

(describe "vino-region-select"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns full information about selected region"
    (spy-on 'completing-read
            :and-return-value
            (completion-for :title "Central Otago" :tags '("region")))
    (expect (vino-region-select)
            :to-be-note
            (mk-vulpea-note
             :type "region"
             :id "f9ef759b-f39e-4121-ab19-9ab3daa318be"
             :title "Central Otago")))

  (it "returns full information about selected appellation"
    (spy-on 'completing-read
            :and-return-value
            (completion-for :title "Cerasuolo di Vittoria DOCG" :tags '("appellation")))
    (expect (vino-region-select)
            :to-be-note
            (mk-vulpea-note
             :type "appellation"
             :id "6a0819f3-0770-4481-9754-754ca397800b"
             :title "Cerasuolo di Vittoria DOCG"
             :links '(("id" . "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
                      ("id" . "3b38917f-6065-42e8-87ca-33dd39a92fc0")))))

  (it "creates a new region note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Codru" "Create region"))
    (spy-on 'read-string :and-return-value "")
    (expect (mock-vulpea-note :type "region" :title "Codru")
            :to-be-note
            (vino-region-select)))

  (it "creates a new appellation note when selecting non-existing name"
    (spy-on 'completing-read :and-return-values '("Gattinara DOCG" "Create appellation"))
    (spy-on 'read-string :and-return-value "")
    (expect (mock-vulpea-note :type "appellation" :title "Gattinara DOCG")
            :to-be-note
            (vino-region-select))))

(describe "vino-entry-consume"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        (initial-in 5)
        (initial-out 2)
        (extra-out 0)
        vino)
  (before-all
    (setq vino-availability-sub-fn (lambda (_id amount _source _date)
                                     (setq  extra-out amount))
          vino-availability-fn (lambda (_) (cons initial-in (+ initial-out extra-out))))
    (vino-test-init))

  (after-all
    (setq vino-availability-sub-fn nil
          vino-availability-fn nil)
    (vino-test-teardown))

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

(describe "vino-entry-acquire"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        (initial-in 2)
        (extra-in 0)
        vino)
  (before-all
    (setq vino-availability-add-fn (lambda (_id amount _source _date)
                                     (setq  extra-in amount))
          vino-availability-fn (lambda (_) (cons(+ initial-in extra-in) 2)))
    (vino-test-init))

  (after-all
    (setq vino-availability-add-fn nil
          vino-availability-fn nil)
    (vino-test-teardown))

  (it "updates availability based on vino-availability-add-fn result"
    (vino-entry-acquire id 3 "some source" "50.00 EUR" (current-time))
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-acquired vino) :to-equal 5)
    (expect (vino-entry-consumed vino) :to-equal 2)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
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

(describe "vino-entry-set-grapes"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

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
        vino)
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "replace region metadata"
    (vino-entry-set-region id "f9ef759b-f39e-4121-ab19-9ab3daa318be")
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-region vino) :to-equal (vulpea-db-get-by-id "f9ef759b-f39e-4121-ab19-9ab3daa318be"))
    (expect (vino-entry-appellation vino) :to-equal nil)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
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
    (vino-entry-set-region id "860f5505-d83c-4305-bc20-cb6a92f5d0be")
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-region vino) :to-equal nil)
    (expect (vino-entry-appellation vino) :to-equal (vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be"))
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
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

(describe "vino-entry-update-availability"
  :var ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
        vino)
  (before-all
    (setq vino-availability-fn (lambda (_) (cons 10 8)))
    (vino-test-init))

  (after-all
    (setq vino-availability-fn nil)
    (vino-test-teardown))

  (it "updates availability based on vino-availability-fn result"
    (vino-entry-update-availability id)
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-entry-acquired vino) :to-equal 10)
    (expect (vino-entry-consumed vino) :to-equal 8)
    (expect (expand-file-name (concat "wine/cellar/" id ".org") org-roam-directory)
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
    (setq vino-rating-props '((1 . (("score" 20))))
          vino-availability-fn (lambda (_) (cons 5 2)))
    (vino-test-init))

  (after-all
    (setq vino-rating-props nil
          vino-availability-fn nil)
    (vino-test-teardown))

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
    (setq vino-rating-props
          '((1 . (("score" 20)))
            (4 . (("property_1" 3)
                  ("property_2" 4)
                  ("property_3" 2)
                  ("property_4" 5)
                  ("property_5" 6))))
          vino-availability-fn (lambda (_) (cons 5 2))
          vino-rating-extra-meta (list
                                  (list
                                   :name "location"
                                   :mode 'single
                                   :type 'string)
                                  (list
                                   :name "convive"
                                   :mode 'multiple
                                   :type 'string)))
    (vino-test-init))

  (after-each
    (setq vino-rating-props nil
          vino-availability-fn nil
          vino-rating-extra-meta nil)
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
                            ("property_5" 5 6))
                  :meta '(("location" . ("some distant location"))
                          ("convive" . ("person 1" "person 2")))))
    (setq note (vino-rating--create rating))
    (expect note :not :to-be nil)
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
    (expect (vino-rating-get-by-id (vulpea-note-id note)) :to-equal rating)
    (expect (vino-db-get-rating (vulpea-note-id note)) :to-equal rating)
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
- score :: 16
- score_max :: 20
- total :: 8.0
- location :: some distant location
- convive :: person 1
- convive :: person 2
"
             (vulpea-note-id note)
             date-str
             id
             date-str)))

  (it "should accept unregistered meta"
    (setq rating (make-vino-rating
                  :wine (vulpea-db-get-by-id id)
                  :date date-str
                  :version 4
                  :values '(("property_1" 3 3)
                            ("property_2" 3 4)
                            ("property_3" 0 2)
                            ("property_4" 5 5)
                            ("property_5" 6 6))
                  :meta '(("weather" . ("good"))
                          ("thoughts" . ("thought 1" "thought 2" "thought 3")))))
    (setq note (vino-rating--create rating))
    (expect note :not :to-be nil)

    ;; and now all get methods should ignore meta
    (setf (vino-rating-meta rating) nil)

    (expect (vino-rating-get-by-id (vulpea-note-id note)) :to-equal rating)
    (expect (vino-db-get-rating (vulpea-note-id note)) :to-equal rating)
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
- property_5 :: 6
- property_5_max :: 6
- score :: 17
- score_max :: 20
- total :: 8.5
- weather :: good
- thoughts :: thought 1
- thoughts :: thought 2
- thoughts :: thought 3
"
             (vulpea-note-id note)
             date-str
             id
             date-str))))

(describe "vino-db"
  (before-all
    (setq vino-rating-props
          '((1 . (("score" 20)))
            (4 . (("property_1" 3)
                  ("property_2" 4)
                  ("property_3" 2)
                  ("property_4" 5)
                  ("property_5" 6)))))
    (vino-test-init))

  (after-all
    (vino-test-teardown)
    (setq vino-rating-props nil))

  (it "subsequent call of vino-db-sync is fast when the cache is warm"
    (let ((duration (benchmark-run 1
                      (vino-db-sync))))
      (message "duration = %s" duration)
      (expect (car duration) :to-be-less-than 1.0)))

  (it "reading entries from file leads to the same result as reading from db"
    (let* ((notes (vulpea-db-query #'vino-entry-note-p))
           (ids (seq-map #'vulpea-note-id notes)))
      (expect (seq-map #'vino-entry-get-by-id ids)
              :to-equal
              (seq-map #'vino-db-get-entry ids))))

  (it "reading ratings from file leads to the same result as reading from db"
    (let* ((notes (vulpea-db-query #'vino-rating-note-p))
           (ids (seq-map #'vulpea-note-id notes)))
      (expect (seq-map #'vino-rating-get-by-id ids)
              :to-equal
              (seq-map #'vino-db-get-rating ids)))))

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
