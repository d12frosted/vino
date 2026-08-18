;;; vino-schema-test.el --- Test `vino-schema' module -*- lexical-binding: t; -*-
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
;; Test `vino-schema' module.
;;
;; Notes used here are created by the tests themselves rather than taken
;; from `test/note-files', so that the expectations stay readable and
;; independent of the shared fixtures.
;;
;;; Code:

(require 'buttercup)
(require 'vino)
(require 'vino-schema)
(require 'vino-test-utils)



(defun vino-schema-test--create (type title &optional meta)
  "Create a note tagged wine and TYPE with TITLE and META."
  (vulpea-create title
                 (format "wine/%s/${id}.org" type)
                 :tags (list "wine" type)
                 :meta (-filter #'cdr meta)))

(defun vino-schema-test--meta (defaults overrides)
  "Merge OVERRIDES over DEFAULTS, dropping keys whose value is nil."
  (let ((result (copy-alist defaults)))
    (dolist (override overrides)
      (setf (alist-get (car override) result nil nil #'equal)
            (cdr override)))
    (-filter #'cdr result)))

(defun vino-schema-test--teardown ()
  "Teardown testing environment and unregister vino schemas.

The schema registry is global, so leaving vino schemas behind makes
every later suite validate its notes on sync."
  (--each (--filter (string-prefix-p "vino-" (symbol-name it))
                    (vulpea-schema-list))
    (vulpea-schema-unregister it))
  (vino-test-teardown))


(defun vino-schema-test--violations (note schema)
  "Validate NOTE against SCHEMA and return (FIELD . TYPE) pairs.

NOTE is re-read from the database first, so it carries the metadata
as extracted from the file."
  (--map (cons (vulpea-violation-field it)
               (vulpea-violation-type it))
         (vulpea-schema-validate
          (vulpea-db-get-by-id (vulpea-note-id note))
          schema)))



(describe "vino-schema-setup"
  (before-all (vino-test-init))
  (after-all (vino-schema-test--teardown))

  (it "registers a schema for every vino note type"
    (vino-schema-setup)
    (expect (--filter (string-prefix-p "vino-" (symbol-name it))
                      (vulpea-schema-list))
            :to-have-same-items-as
            '(vino-entry vino-rating vino-producer vino-grape
              vino-country vino-region vino-appellation)))

  (it "is idempotent"
    (vino-schema-setup)
    (vino-schema-setup)
    (expect (length (--filter (string-prefix-p "vino-" (symbol-name it))
                              (vulpea-schema-list)))
            :to-equal 7)))



(describe "vino note predicates"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "recognises a country note"
    (expect (vino-country-note-p
             (vulpea-db-get-by-id "437298dc-39d9-42e6-8d0f-1838e9a007f9"))
            :to-be t))

  (it "recognises a region note"
    (expect (vino-region-note-p
             (vulpea-db-get-by-id "f9ef759b-f39e-4121-ab19-9ab3daa318be"))
            :to-be t))

  (it "recognises an appellation note"
    (expect (vino-appellation-note-p
             (vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be"))
            :to-be t))

  (it "recognises a grape note"
    (expect (vino-grape-note-p
             (vulpea-db-get-by-id "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa"))
            :to-be t))

  (it "recognises a producer note"
    (expect (vino-producer-note-p
             (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2"))
            :to-be t))

  (it "does not confuse one note type for another"
    (expect (vino-region-note-p
             (vulpea-db-get-by-id "860f5505-d83c-4305-bc20-cb6a92f5d0be"))
            :to-be nil)
    (expect (vino-grape-note-p
             (vulpea-db-get-by-id "9462dfad-603c-4094-9aca-a9042cec5dd2"))
            :to-be nil))

  (it "ignores heading level notes"
    (expect (vino-producer-note-p
             (vulpea-db-get-by-id "71715128-3d6f-4e36-8d70-d35fcb057609"))
            :to-be nil)))



(describe "vino-entry schema"
  :var (producer country grape wine-defaults)

  (before-all
    (vino-test-init)
    (vino-schema-setup)
    (setq producer (vino-schema-test--create "producer" "Schema Producer")
          country (vino-schema-test--create "country" "Schema Country")
          grape (vino-schema-test--create "grape" "Schema Grape")
          wine-defaults `(("carbonation" . still)
                          ("colour" . red)
                          ("sweetness" . dry)
                          ("producer" . ,producer)
                          ("name" . "Schema Wine")
                          ("vintage" . 2017)
                          ("volume" . 750)
                          ("country" . ,country)
                          ("grapes" . ,grape)
                          ("alcohol" . 13))))
  (after-all (vino-schema-test--teardown))

  (defun vino-schema-test--wine (&rest overrides)
    "Create a wine entry note with OVERRIDES applied to valid defaults."
    (vino-schema-test--create
     "cellar" "Schema Wine"
     (vino-schema-test--meta wine-defaults overrides)))

  (it "applies to a wine entry note"
    (expect (vulpea-schema-applies-p (vino-schema-test--wine) 'vino-entry)
            :to-be-truthy))

  (it "does not apply to other wine notes"
    (expect (vulpea-schema-applies-p producer 'vino-entry) :to-be nil))

  (it "reports no violations for a complete entry"
    (expect (vino-schema-test--violations (vino-schema-test--wine) 'vino-entry)
            :to-be nil))

  (it "reports a missing required field"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("producer"))
             'vino-entry)
            :to-equal '(("producer" . missing-required))))

  (it "reports a non numeric alcohol"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("alcohol" . "quite a lot"))
             'vino-entry)
            :to-equal '(("alcohol" . wrong-type))))

  (it "reports a colour outside of `vino-colour-types'"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("colour" . blue))
             'vino-entry)
            :to-equal '(("colour" . disallowed-value))))

  (it "reports a sweetness that does not match carbonation"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("sweetness" . brut))
             'vino-entry)
            :to-equal '(("sweetness" . disallowed-value))))

  (it "accepts a sparkling sweetness on a sparkling wine"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("carbonation" . sparkling)
                                     '("carbonation method" . traditional)
                                     '("sweetness" . brut))
             'vino-entry)
            :to-be nil))

  (it "requires carbonation method for sparkling wines"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("carbonation" . sparkling)
                                     '("sweetness" . brut))
             'vino-entry)
            :to-equal '(("carbonation method" . missing-required))))

  (it "does not require carbonation method for still wines"
    (expect (vino-schema-test--violations (vino-schema-test--wine) 'vino-entry)
            :to-be nil))

  (it "reports a producer that is not a producer note"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine `("producer" . ,grape))
             'vino-entry)
            :to-equal '(("producer" . invalid-target))))

  (it "reports a grape that is not a grape note"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine `("grapes" . ,country))
             'vino-entry)
            :to-equal '(("grapes" . invalid-target))))

  (it "accepts NA as a rating"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("rating" . "NA"))
             'vino-entry)
            :to-be nil))

  (it "accepts a numeric rating"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("rating" . "4.2"))
             'vino-entry)
            :to-be nil))

  (it "reports a rating that is neither NA nor a number"
    (expect (vino-schema-test--violations
             (vino-schema-test--wine '("rating" . "quite nice"))
             'vino-entry)
            :to-equal '(("rating" . invalid-value)))))



(describe "vino-rating schema"
  :var (producer wine rating-defaults)

  (before-all
    (vino-test-init)
    (vino-schema-setup)
    (setq producer (vino-schema-test--create "producer" "Rating Producer")
          wine (vino-schema-test--create
                "cellar" "Rating Wine"
                `(("carbonation" . still)
                  ("colour" . red)
                  ("sweetness" . dry)
                  ("producer" . ,producer)
                  ("name" . "Rating Wine")
                  ("volume" . 750)
                  ("alcohol" . 13)))
          rating-defaults `(("wine" . ,wine)
                            ("date" . "2021-01-14")
                            ("version" . 1)
                            ("score" . 18)
                            ("score_max" . 20)
                            ("total" . 4.5))))
  (after-all (vino-schema-test--teardown))

  (defun vino-schema-test--rating (&rest overrides)
    "Create a rating note with OVERRIDES applied to valid defaults."
    (vino-schema-test--create
     "rating" "Rating Wine - 2021-01-14"
     (vino-schema-test--meta rating-defaults overrides)))

  (it "applies to a rating note"
    (expect (vulpea-schema-applies-p (vino-schema-test--rating) 'vino-rating)
            :to-be-truthy))

  (it "reports no violations for a complete rating"
    (expect (vino-schema-test--violations (vino-schema-test--rating) 'vino-rating)
            :to-be nil))

  (it "reports a missing total"
    (expect (vino-schema-test--violations
             (vino-schema-test--rating '("total"))
             'vino-rating)
            :to-equal '(("total" . missing-required))))

  (it "reports a date that is not YYYY-MM-DD"
    (expect (vino-schema-test--violations
             (vino-schema-test--rating '("date" . "14 Jan 2021"))
             'vino-rating)
            :to-equal '(("date" . invalid-value))))

  (it "reports a wine that is not a wine entry"
    (expect (vino-schema-test--violations
             (vino-schema-test--rating `("wine" . ,producer))
             'vino-rating)
            :to-equal '(("wine" . invalid-target)))))



(describe "vino-region and vino-appellation schemas"
  :var (country region)

  (before-all
    (vino-test-init)
    (vino-schema-setup)
    (setq country (vino-schema-test--create "country" "Origin Country")
          region (vino-schema-test--create
                  "region" "Origin Region"
                  `(("country" . ,country)))))
  (after-all (vino-schema-test--teardown))

  (it "reports no violations for a region with a country"
    (expect (vino-schema-test--violations region 'vino-region) :to-be nil))

  (it "requires a country on a region"
    (expect (vino-schema-test--violations
             (vino-schema-test--create "region" "Countryless Region")
             'vino-region)
            :to-equal '(("country" . missing-required))))

  (it "requires a country on an appellation"
    (expect (vino-schema-test--violations
             (vino-schema-test--create "appellation" "Countryless Appellation")
             'vino-appellation)
            :to-equal '(("country" . missing-required))))

  (it "accepts a region as the parent of an appellation"
    (expect (vino-schema-test--violations
             (vino-schema-test--create
              "appellation" "Child Appellation"
              `(("country" . ,country)
                ("parent" . ,region)))
             'vino-appellation)
            :to-be nil))

  (it "reports a parent that is not a region"
    (expect (vino-schema-test--violations
             (vino-schema-test--create
              "appellation" "Odd Appellation"
              `(("country" . ,country)
                ("parent" . ,country)))
             'vino-appellation)
            :to-equal '(("parent" . invalid-target)))))



(provide 'vino-schema-test)
;;; vino-schema-test.el ends here
