;;; vino-inv-test.el --- Test `vino-inv' module -*- lexical-binding: t; -*-
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
;; Test `vino-inv' module.
;;
;;; Code:

(require 'buttercup)
(require 'emacsql)
(require 'vino-inv)
(require 'dash)
(require 'vino-test-utils)

(defconst vino-inv-test--wine "c9937e3e-c83d-4d8d-a612-6110e6706252"
  "Arianna Occhipinti Bombolieri BB 2017.")

(defun vino-inv-test--acquire (wine amount)
  "Purchase AMOUNT bottles of WINE, returning the first one.

A bottle needs a location and a source, both `not-null' in the schema."
  (let ((location (vino-inv-add-location "Cellar"))
        (source (vino-inv-add-source "Shop")))
    (car (--map (vino-inv-add-bottle :wine wine
                                     :date "2021-01-01"
                                     :price "10 EUR"
                                     :price-usd "12 USD"
                                     :location-id (vino-inv-location-id location)
                                     :source-id (vino-inv-source-id source))
                (-iota amount)))))

(defun vino-inv-test--reload (note)
  "Re-read NOTE from the database after its file was written."
  (vulpea-db-update-file (vulpea-note-path note))
  (vulpea-db-get-by-id (vulpea-note-id note)))

(defun vino-inv-test--tables (db)
  "Return list of table names in DB."
  (-map #'car
        (emacsql db [:select [name] :from sqlite_master
                     :where (= type 'table)])))

(defmacro vino-inv-test--with-fresh-db (&rest body)
  "Evaluate BODY with `vino-inv-db-file' pointing at a fresh location."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "vino-inv-test-" 'dir))
          (vino-inv-db-file (expand-file-name "wine.db" dir))
          (vino-inv-db--connection nil))
     (unwind-protect
         (progn ,@body)
       (when vino-inv-db--connection
         (vino-inv-db-close))
       (delete-directory dir t))))

(describe "vino-inv-db"
  (it "bootstraps schema when database file does not exist"
    (vino-inv-test--with-fresh-db
      (expect (-intersection (vino-inv-test--tables (vino-inv-db))
                             '(location source bottle transaction))
              :to-have-same-items-as
              '(location source bottle transaction)))))

(describe "vino-inv-db--setup"
  (it "is idempotent"
    (vino-inv-test--with-fresh-db
      (let ((db (vino-inv-db)))
        (expect (vino-inv-db--setup db) :not :to-throw)
        (expect (-intersection (vino-inv-test--tables db)
                               '(location source bottle transaction))
                :to-have-same-items-as
                '(location source bottle transaction))))))

(describe "vino-inv-count-bottles-for"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "counts purchased and consumed bottles in one go"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (vino-inv-test--acquire wine 3)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id bottle)
                                 :date "2021-02-01")
        (expect (vino-inv-count-bottles-for (vulpea-note-id wine))
                :to-equal '(3 . 1)))))

  (it "counts nothing for a wine with no bottles"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-count-bottles-for vino-inv-test--wine)
              :to-equal '(0 . 0))))

  (it "agrees with the single counters"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (vino-inv-test--acquire wine 2)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id bottle)
                                 :date "2021-02-01")
        (expect (vino-inv-count-purchased-bottles-for (vulpea-note-id wine))
                :to-equal 2)
        (expect (vino-inv-count-consumed-bottles-for (vulpea-note-id wine))
                :to-equal 1)))))

(describe "vino-inv-update-availability"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "writes acquired, consumed and available"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (vino-inv-test--acquire wine 3)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id bottle)
                                 :date "2021-02-01")
        (vino-inv-update-availability wine)
        (let ((note (vino-inv-test--reload wine)))
          (expect (vulpea-note-meta-get note "acquired" 'number) :to-equal 3)
          (expect (vulpea-note-meta-get note "consumed" 'number) :to-equal 1)
          (expect (vulpea-note-meta-get note "available" 'number) :to-equal 2)))))

  (it "leaves the order of the other metadata alone"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (before (-map #'car (vulpea-note-meta wine))))
        (vino-inv-test--acquire wine 1)
        (vino-inv-update-availability wine)
        (expect (-map #'car (vulpea-note-meta (vino-inv-test--reload wine)))
                :to-equal before)))))


(describe "vino-inv-add-price"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records a public price on the wine note"
    (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
      (vino-inv-add-price wine "42.00 EUR" "public")
      (expect (vulpea-note-meta-get-list (vino-inv-test--reload wine) "price")
              :to-equal '("42.00 EUR" "50.00 EUR"))))

  (it "records a private price on the wine note"
    (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
      (vino-inv-add-price wine "42.00 EUR" "private")
      (expect (vulpea-note-meta-get-list (vino-inv-test--reload wine)
                                         "price private")
              :to-equal '("42.00 EUR"))))

  (it "does not record the same price twice"
    (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
      (vino-inv-add-price wine "42.00 EUR" "public")
      (vino-inv-add-price (vino-inv-test--reload wine) "42.00 EUR" "public")
      (expect (vulpea-note-meta-get-list (vino-inv-test--reload wine) "price")
              :to-equal '("42.00 EUR" "50.00 EUR"))))

  (it "records nothing when the price is skipped"
    (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
      (vino-inv-add-price wine "42.00 EUR" "skip")
      (vino-inv-add-price wine "42.00 EUR" nil)
      (expect (vulpea-note-meta-get-list (vino-inv-test--reload wine) "price")
              :to-equal '("50.00 EUR"))))

  (it "refuses an unknown kind"
    (expect (vino-inv-add-price (vulpea-db-get-by-id vino-inv-test--wine)
                                "42.00 EUR" "secret")
            :to-throw 'user-error)))

(defun vino-inv-test--face-at (string substring)
  "Return the `face' property at the start of SUBSTRING within STRING."
  (when-let* ((pos (string-match (regexp-quote substring) string)))
    (get-text-property pos 'face string)))

(defun vino-inv-test--bottle ()
  "Return a bottle to describe, built without touching any database."
  (make-vino-inv-bottle
   :id 12
   :wine (vulpea-db-get-by-id vino-inv-test--wine)
   :purchase-date "2021-01-01"
   :price "10 EUR"
   :price-usd "12 USD"
   :location (make-vino-inv-location :id 1 :name "Cellar")
   :source (make-vino-inv-source :id 1 :name "Shop")))

(describe "vino-inv-bottle-describe"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "names the bottle, when it was bought, where it is, and from whom"
    (expect (substring-no-properties
             (vino-inv-bottle-describe (vino-inv-test--bottle)))
            :to-equal "12 #12 [2021-01-01] @Cellar - 10 EUR from Shop"))

  (it "hides the leading id that identifies the selected candidate"
    (let ((described (vino-inv-bottle-describe (vino-inv-test--bottle))))
      (expect (get-text-property 0 'invisible described) :to-be t)
      (expect (string-to-number described) :to-equal 12)))

  (it "renders the bottle number with a vino face"
    (expect (vino-inv-test--face-at
             (vino-inv-bottle-describe (vino-inv-test--bottle)) "#12")
            :to-equal 'vino-inv-annotation-id))

  (it "renders the separators with a vino face"
    (let ((described (vino-inv-bottle-describe (vino-inv-test--bottle))))
      (--each '(" [" "] @" " - " " from ")
        (expect (vino-inv-test--face-at described it)
                :to-equal 'vino-inv-annotation-separator))))

  (it "leaves the values themselves unstyled"
    (let ((described (vino-inv-bottle-describe (vino-inv-test--bottle))))
      (--each '("2021-01-01" "Cellar" "10 EUR" "Shop")
        (expect (vino-inv-test--face-at described it) :to-be nil)))))

(provide 'vino-inv-test)
;;; vino-inv-test.el ends here
