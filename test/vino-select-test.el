;;; vino-select-test.el --- Test wine selection -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;;
;; Created: 18 Aug 2026
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
;; Test annotation of wine entries during selection.
;;
;;; Code:

(require 'buttercup)
(require 'vino)
(require 'vino-test-utils)



(defun vino-select-test--country (title)
  "Create a country note with TITLE."
  (vulpea-create title "wine/country/${id}.org" :tags '("wine" "country")))

(defun vino-select-test--wine (title meta)
  "Create a wine entry note with TITLE and META."
  (vulpea-db-get-by-id
   (vulpea-note-id
    (vulpea-create title "wine/cellar/${id}.org"
                   :tags '("wine" "cellar")
                   :meta (-filter #'cdr meta)))))



(describe "vino-entry-annotate"
  :var (italy)

  (before-all
    (vino-test-init)
    (setq italy (vino-select-test--country "Italy")))
  (after-all (vino-test-teardown))

  (it "shows colour, country, rating and available bottles"
    (expect (vino-entry-annotate
             (vino-select-test--wine
              "Full Wine" `(("colour" . red)
                            ("country" . ,italy)
                            ("rating" . "4.5")
                            ("available" . 2))))
            :to-equal " red Italy 4.5 x2"))

  (it "shows an unrated wine as NA"
    (expect (vino-entry-annotate
             (vino-select-test--wine
              "Unrated Wine" `(("colour" . white)
                               ("country" . ,italy)
                               ("rating" . "NA")
                               ("available" . 1))))
            :to-equal " white Italy NA x1"))

  (it "omits the count when no bottle is available"
    (expect (vino-entry-annotate
             (vino-select-test--wine
              "Drunk Wine" `(("colour" . red)
                             ("country" . ,italy)
                             ("rating" . "4.5")
                             ("available" . 0))))
            :to-equal " red Italy 4.5"))

  (it "omits the count when availability is not tracked"
    (expect (vino-entry-annotate
             (vino-select-test--wine
              "Untracked Wine" `(("colour" . red)
                                 ("country" . ,italy)
                                 ("rating" . "4.5"))))
            :to-equal " red Italy 4.5"))

  (it "omits the country when the wine has none"
    (expect (vino-entry-annotate
             (vino-select-test--wine
              "Nowhere Wine" '(("colour" . red)
                               ("rating" . "4.5"))))
            :to-equal " red 4.5"))

  (it "returns an empty string when there is nothing to show"
    (expect (vino-entry-annotate
             (vino-select-test--wine "Bare Wine" nil))
            :to-equal ""))

  (it "reads the country from the context when there is one"
    (let ((wine (vino-select-test--wine
                 "Context Wine" `(("colour" . red)
                                  ("country" . ,italy)
                                  ("rating" . "4.5"))))
          (context (make-hash-table :test 'equal)))
      (puthash (vulpea-note-id italy) "Italia" context)
      (expect (vino-entry-annotate wine context)
              :to-equal " red Italia 4.5"))))



(describe "vino-entry-dyncontext"
  :var (italy france wines)

  (before-all
    (vino-test-init)
    (setq italy (vino-select-test--country "Italy")
          france (vino-select-test--country "France")
          wines (list (vino-select-test--wine "One" `(("country" . ,italy)))
                      (vino-select-test--wine "Two" `(("country" . ,italy)))
                      (vino-select-test--wine "Three" `(("country" . ,france)))
                      (vino-select-test--wine "Four" nil))))
  (after-all (vino-test-teardown))

  (it "maps country ids to titles"
    (let ((context (vino-entry-dyncontext wines)))
      (expect (gethash (vulpea-note-id italy) context) :to-equal "Italy")
      (expect (gethash (vulpea-note-id france) context) :to-equal "France")))

  (it "resolves every country with a single query"
    (spy-on 'vulpea-db-query-by-ids :and-call-through)
    (vino-entry-dyncontext wines)
    (expect 'vulpea-db-query-by-ids :to-have-been-called-times 1))

  (it "does not query when no wine has a country"
    (spy-on 'vulpea-db-query-by-ids :and-call-through)
    (expect (hash-table-count
             (vino-entry-dyncontext
              (list (vino-select-test--wine "Countryless" nil))))
            :to-equal 0)
    (expect 'vulpea-db-query-by-ids :not :to-have-been-called)))



(describe "vino-entry-select-from"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "annotates candidates as wine entries"
    (let (annotate-fn dyncontext-fn)
      (spy-on 'vulpea-select-from :and-call-fake
              (lambda (&rest _)
                (setq annotate-fn vulpea-select-annotate-fn
                      dyncontext-fn vulpea-select-dyncontext-fn)
                nil))
      (vino-entry-select-from "Wine" nil)
      (expect annotate-fn :to-equal vino-entry-annotate-fn)
      (expect dyncontext-fn :to-equal vino-entry-dyncontext-fn)))

  (it "leaves the global annotation alone once done"
    (let ((before vulpea-select-annotate-fn))
      (spy-on 'vulpea-select-from :and-return-value nil)
      (vino-entry-select-from "Wine" nil)
      (expect vulpea-select-annotate-fn :to-equal before))))



(provide 'vino-select-test)
;;; vino-select-test.el ends here
