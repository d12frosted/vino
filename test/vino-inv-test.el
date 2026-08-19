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

(defun vino-inv-test--location-id (name)
  "Return the id of location NAME, creating it when it does not exist.
Location names are unique, so a second `vino-inv-add-location' with the
same name fails."
  (if-let ((existing (--find (string-equal (vino-inv-location-name it) name)
                             (vino-inv-query-locations))))
      (vino-inv-location-id existing)
    (vino-inv-location-id (vino-inv-add-location name))))

(defun vino-inv-test--source-id (name)
  "Return the id of source NAME, creating it when it does not exist."
  (if-let ((existing (--find (string-equal (vino-inv-source-name it) name)
                             (vino-inv-query-sources))))
      (vino-inv-source-id existing)
    (vino-inv-source-id (vino-inv-add-source name))))

(cl-defun vino-inv-test--acquire (wine amount &key (location "Cellar")
                                              (source "Shop")
                                              (date "2021-01-01")
                                              (price "10 EUR"))
  "Purchase AMOUNT bottles of WINE, returning them oldest first.

A bottle needs a LOCATION and a SOURCE, both `not-null' in the schema;
each is created on first use.  DATE and PRICE are recorded on every
bottle."
  (let ((location-id (vino-inv-test--location-id location))
        (source-id (vino-inv-test--source-id source)))
    (--map (vino-inv-add-bottle :wine wine
                                :date date
                                :price price
                                :price-usd "12 USD"
                                :location-id location-id
                                :source-id source-id)
           (-iota amount))))

(defun vino-inv-test--wine-note (title)
  "Create a wine entry note titled TITLE."
  (vulpea-create title "wine/cellar/${id}.org" :tags '("wine" "cellar")))

(defun vino-inv-test--bottle-ids (bottles)
  "Return sorted ids of BOTTLES."
  (sort (-map #'vino-inv-bottle-id bottles) #'<))

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
             (bottle (car (vino-inv-test--acquire wine 3))))
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
             (bottle (car (vino-inv-test--acquire wine 2))))
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
             (bottle (car (vino-inv-test--acquire wine 3))))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id bottle)
                                 :date "2021-02-01")
        (vino-inv-update-availability wine)
        (let ((note (vino-inv-test--reload wine)))
          (expect (vulpea-note-meta-get note "acquired" 'number) :to-equal 3)
          (expect (vulpea-note-meta-get note "consumed" 'number) :to-equal 1)
          (expect (vulpea-note-meta-get note "available" 'number) :to-equal 2)))))

  (it "adds the counters to a wine that carries none"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-create "Bare Wine" "wine/cellar/${id}.org"
                                 :tags '("wine" "cellar")
                                 :meta '(("colour" . red)
                                         ("name" . "Bare")))))
        (vino-inv-test--acquire wine 2)
        (vino-inv-update-availability wine)
        (let ((note (vino-inv-test--reload wine)))
          (expect (vulpea-note-meta-get note "acquired" 'number) :to-equal 2)
          ;; a counter the wine did not have opens the block, the way any
          ;; newly set property does
          (expect (-map #'car (vulpea-note-meta note))
                  :to-equal '("acquired" "consumed" "available"
                              "colour" "name"))))))

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

(describe "vino-inv locations and sources"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "returns what was added"
    (vino-inv-test--with-fresh-db
      (vino-inv-add-location "Cellar")
      (vino-inv-add-location "Fridge")
      (vino-inv-add-source "Shop")
      (expect (-map #'vino-inv-location-name (vino-inv-query-locations))
              :to-have-same-items-as '("Cellar" "Fridge"))
      (expect (-map #'vino-inv-source-name (vino-inv-query-sources))
              :to-equal '("Shop"))))

  (it "has nothing to return on a fresh database"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-query-locations) :to-be nil)
      (expect (vino-inv-query-sources) :to-be nil)))

  (it "reads a location back by id"
    (vino-inv-test--with-fresh-db
      (let ((id (vino-inv-location-id (vino-inv-add-location "Cellar"))))
        (expect (vino-inv-location-name (vino-inv-get-location id))
                :to-equal "Cellar"))))

  (it "reads a source back by id"
    (vino-inv-test--with-fresh-db
      (let ((id (vino-inv-source-id (vino-inv-add-source "Shop"))))
        (expect (vino-inv-source-name (vino-inv-get-source id))
                :to-equal "Shop"))))

  (it "returns nothing for a location that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-get-location 404) :to-be nil)))

  (it "returns nothing for a source that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-get-source 404) :to-be nil))))

(describe "vino-inv-get-bottle"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "reads a bottle back with its wine, location and source resolved"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (added (car (vino-inv-test--acquire wine 1 :location "Fridge"
                                                 :source "Auction"
                                                 :date "2021-03-04"
                                                 :price "25 EUR")))
             (bottle (vino-inv-get-bottle (vino-inv-bottle-id added))))
        (expect (vulpea-note-id (vino-inv-bottle-wine bottle))
                :to-equal vino-inv-test--wine)
        (expect (vino-inv-location-name (vino-inv-bottle-location bottle))
                :to-equal "Fridge")
        (expect (vino-inv-source-name (vino-inv-bottle-source bottle))
                :to-equal "Auction")
        (expect (vino-inv-bottle-purchase-date bottle) :to-equal "2021-03-04")
        (expect (vino-inv-bottle-price bottle) :to-equal "25 EUR"))))

  (it "returns nothing for a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-get-bottle 404) :to-be nil))))

(describe "vino-inv-consume-bottle"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records the consumption of a bottle that exists"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id bottle)
                                 :date "2021-02-01")
        (expect (vino-inv-count-bottles-for vino-inv-test--wine)
                :to-equal '(1 . 1)))))

  (it "refuses a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-consume-bottle :bottle-id 404 :date "2021-02-01")
              :to-throw 'user-error)))

  (it "records nothing when it refuses"
    (vino-inv-test--with-fresh-db
      (ignore-errors
        (vino-inv-consume-bottle :bottle-id 404 :date "2021-02-01"))
      (expect (caar (emacsql (vino-inv-db)
                             [:select (funcall count *) :from transaction]))
              :to-equal 0))))

(describe "vino-inv-query-available-wines"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "names a wine once however many bottles are held"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 3)
        (expect (-map #'vulpea-note-id (vino-inv-query-available-wines))
                :to-equal (list vino-inv-test--wine)))))

  (it "drops a wine once its last bottle is consumed"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 2)))
        (--each bottles
          (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id it)
                                   :date "2021-02-01"))
        (expect (vino-inv-query-available-wines) :to-be nil))))

  (it "keeps a wine that still has one bottle left"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 2)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id (car bottles))
                                 :date "2021-02-01")
        (expect (-map #'vulpea-note-id (vino-inv-query-available-wines))
                :to-equal (list vino-inv-test--wine)))))

  (it "has nothing to return before anything is acquired"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-query-available-wines) :to-be nil))))

(describe "vino-inv-query-available-bottles"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "returns the bottles still held, resolved"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 3)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id (car bottles))
                                 :date "2021-02-01")
        (let ((available (vino-inv-query-available-bottles)))
          (expect (vino-inv-test--bottle-ids available)
                  :to-equal (vino-inv-test--bottle-ids (cdr bottles)))
          (expect (-map (lambda (b)
                          (vino-inv-location-name (vino-inv-bottle-location b)))
                        available)
                  :to-equal '("Cellar" "Cellar"))
          (expect (-map (lambda (b) (vulpea-note-id (vino-inv-bottle-wine b)))
                        available)
                  :to-equal (list vino-inv-test--wine vino-inv-test--wine))))))

  (it "has nothing to return before anything is acquired"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-query-available-bottles) :to-be nil))))

(describe "vino-inv-query-available-bottles-for-many"
  :var (other)

  (before-each
    (vino-test-init)
    (setq other (vino-inv-test--wine-note "Another Wine 2019")))
  (after-each (vino-test-teardown))

  (it "groups bottles by the wine they belong to"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (mine (vino-inv-test--acquire wine 2))
             (theirs (vino-inv-test--acquire other 1))
             (table (vino-inv-query-available-bottles-for-many
                     (list vino-inv-test--wine (vulpea-note-id other)))))
        (expect (vino-inv-test--bottle-ids (gethash vino-inv-test--wine table))
                :to-equal (vino-inv-test--bottle-ids mine))
        (expect (vino-inv-test--bottle-ids
                 (gethash (vulpea-note-id other) table))
                :to-equal (vino-inv-test--bottle-ids theirs)))))

  (it "leaves out a wine with no bottles left"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 1)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id (car bottles))
                                 :date "2021-02-01")
        (vino-inv-test--acquire other 1)
        (let ((table (vino-inv-query-available-bottles-for-many
                      (list vino-inv-test--wine (vulpea-note-id other)))))
          (expect (gethash vino-inv-test--wine table) :to-be nil)
          (expect (length (gethash (vulpea-note-id other) table))
                  :to-equal 1)))))

  (it "asks for nothing and gets nothing"
    (vino-inv-test--with-fresh-db
      (expect (hash-table-count
               (vino-inv-query-available-bottles-for-many nil))
              :to-equal 0)))

  (it "agrees with the single wine query"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--acquire other 1)
        (expect (vino-inv-test--bottle-ids
                 (vino-inv-query-available-bottles-for vino-inv-test--wine))
                :to-equal
                (vino-inv-test--bottle-ids
                 (gethash vino-inv-test--wine
                          (vino-inv-query-available-bottles-for-many
                           (list vino-inv-test--wine))))))))

  (it "resolves the wine on every bottle it returns"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (expect (--map (vulpea-note-title (vino-inv-bottle-wine it))
                       (gethash vino-inv-test--wine
                                (vino-inv-query-available-bottles-for-many
                                 (list vino-inv-test--wine))))
                :to-equal '("Arianna Occhipinti Bombolieri BB 2017"))))))

(provide 'vino-inv-test)
;;; vino-inv-test.el ends here
