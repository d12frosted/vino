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

(describe "vino-note-p"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns non-nil when used on of wine entry"
    (expect (vino-note-p (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
            :to-be t))

  (it "returns nil when used on some heading inside wine entry"
    (expect (vino-note-p (vulpea-db-get-by-id "71715128-3d6f-4e36-8d70-d35fcb057609"))
            :to-be nil))

  (it "returns nil when used on other entry"
    (expect (vino-note-p (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
            :to-be nil)))

(describe "vino-note-get"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns a note when used on id of wine entry"
    (expect (vino-note-get "c9937e3e-c83d-4d8d-a612-6110e6706252")
            :to-equal
            (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))

  (it "returns a note when used on wine note"
    (expect (vino-note-get (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252"))
            :to-equal
            (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))

  (it "returns a note when used inside a wine note"
    (let* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
           (note (vulpea-db-get-by-id id)))
      (expect
       (vulpea-utils-with-file (vulpea-db-get-file-by-id id)
         (goto-char (point-max))
         (vino-note-get))
       :to-equal note)))

  (it "calls `vino-note-select' when called from non org-mode buffer"
    (let ((note (vulpea-db-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")))
      (spy-on 'vino-note-select :and-return-value note)
      (expect (vino-note-get) :to-equal note)))

  (it "calls `vino-note-select' when current buffer does not represent wine"
    (let* ((id "c9937e3e-c83d-4d8d-a612-6110e6706252")
           (note (vulpea-db-get-by-id id)))
      (spy-on 'vino-note-select :and-return-value note)
      (expect
       (vulpea-utils-with-file (vulpea-db-get-file-by-id id)
         (vino-note-get))
       :to-equal note)))

  (it "throws an error when used on id of some heading inside wine entry"
    (let ((id "71715128-3d6f-4e36-8d70-d35fcb057609"))
      (expect (vino-note-get id)
              :to-throw
              'user-error
              (list
               (format "Note with id %s does not represent a vino entry"
                       id)))))

  (it "throws an error when used on some heading inside wine entry"
    (let ((id "71715128-3d6f-4e36-8d70-d35fcb057609"))
      (expect (vino-note-get (vulpea-db-get-by-id id))
              :to-throw
              'user-error
              (list
               (format "Note %s does not represent a vino entry"
                       (vulpea-db-get-by-id id))))))

  (it "throws an error when used on id of other note"
    (let ((id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
      (expect (vino-note-get id)
              :to-throw
              'user-error
              (list
               (format "Note with id %s does not represent a vino entry"
                       id)))))

  (it "throws an error when used on other note"
    (let ((id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
      (expect (vino-note-get (vulpea-db-get-by-id id))
              :to-throw
              'user-error
              (list
               (format "Note %s does not represent a vino entry"
                       (vulpea-db-get-by-id id)))))))

(describe "vino--entry-create"
  :var (vino id)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "creates a new entry with all information"
    (setq vino (make-vino :carbonation 'still
                          :colour 'red
                          :sweetness 'dry
                          :producer "9462dfad-603c-4094-9aca-a9042cec5dd2"
                          :name "Grotte Alte"
                          :vintage 2014
                          :appellation "6a0819f3-0770-4481-9754-754ca397800b"
                          :grapes '("cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"
                                    "3b38917f-6065-42e8-87ca-33dd39a92fc0")
                          :alcohol 13
                          :sugar 0
                          :acquired 0
                          :consumed 0
                          :resources '("http://www.agricolaocchipinti.it/it/grotte-alte"
                                       "https://www.bowlerwine.com/wine-or-spirit/grotte-alte-cerasuolo-di-vittoria-riserva")
                          :price '("50.00 EUR")))
    (setq id (vino--entry-create vino))
    (expect (vino-entry-get-by-id id) :to-equal vino)
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
- price :: 50.00 EUR
- acquired :: 0
- consumed :: 0
- available :: 0
- resources :: [[http://www.agricolaocchipinti.it/it/grotte-alte][agricolaocchipinti.it]]
- resources :: [[https://www.bowlerwine.com/wine-or-spirit/grotte-alte-cerasuolo-di-vittoria-riserva][bowlerwine.com]]


"
             id))))

(describe "vino-entry-get-by-id"
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "returns an existing `vino'"
    (expect (vino-entry-get-by-id "c9937e3e-c83d-4d8d-a612-6110e6706252")
            :to-equal
            (make-vino :carbonation 'still
                       :colour 'red
                       :sweetness 'dry
                       :producer "9462dfad-603c-4094-9aca-a9042cec5dd2"
                       :name "Bombolieri BB"
                       :vintage 2017
                       :appellation "8353e2fc-8034-4540-8254-4b63fb5a421a"
                       :grapes '("cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
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
            :to-equal
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
    (spy-on 'read-string :and-return-value "")
    (expect (vino-grape-select)
            :to-equal
            (make-vulpea-note
             :path (expand-file-name
                    (format "wine/grape/%s-slarina.org"
                            (format-time-string "%Y%m%d%H%M%S" ts))
                    org-roam-directory)
             :title "Slarina"
             :tags '("wine" "grape")
             :level 0
             :id id))))

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
            (make-vulpea-note
             :path (expand-file-name "wine/producer/arianna_occhipinti.org" org-roam-directory)
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
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "wine/appellation/cerasuolo_di_vittoria_docg.org" org-roam-directory)
             :title "Cerasuolo di Vittoria DOCG"
             :tags '("wine" "appellation")
             :level 0
             :id "6a0819f3-0770-4481-9754-754ca397800b"))))

(describe "vino-availability-update"
  :var (id vino)
  (before-all
    (vino-test--init))

  (after-all
    (vino-test--teardown))

  (it "updates availability based on vino-availability-fn result"
    (setq vino-availability-fn (lambda (_) (cons 10 8))
          id "c9937e3e-c83d-4d8d-a612-6110e6706252")
    (vino-availability-update id)
    (setq vino (vino-entry-get-by-id id))
    (expect (vino-acquired vino) :to-equal 10)
    (expect (vino-consumed vino) :to-equal 8)
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

(provide 'vino-test)
;;; vino-test.el ends here
