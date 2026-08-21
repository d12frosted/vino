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

(defun vino-inv-test--wine-note (title &optional meta)
  "Create a wine entry note titled TITLE carrying META."
  (vulpea-create title "wine/cellar/${id}.org"
                 :tags '("wine" "cellar")
                 :meta meta))

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

(cl-defun vino-inv-test--bottle (&key (id 12)
                                      (wine (vulpea-db-get-by-id
                                             vino-inv-test--wine))
                                      (purchase-date "2021-01-01")
                                      (price "10 EUR")
                                      (price-usd "12 USD")
                                      (location "Cellar")
                                      (source "Shop")
                                      comment)
  "Return a bottle to render, built without touching any database.

ID, WINE, PURCHASE-DATE, PRICE, PRICE-USD, LOCATION, SOURCE and COMMENT
fill the slots of the same name; LOCATION and SOURCE are names."
  (make-vino-inv-bottle
   :id id
   :wine wine
   :purchase-date purchase-date
   :price price
   :price-usd price-usd
   :location (make-vino-inv-location :id 1 :name location)
   :source (make-vino-inv-source :id 1 :name source)
   :comment comment))

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


;; * inventory ui

(defun vino-inv-test--goto-first-entry ()
  "Move point to the first entry of the inventory buffer.

The inventory keeps the column header in the buffer rather than in the
header line, which is taken by the totals, so the first line holds no
entry."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (tabulated-list-get-id)))
    (forward-line)))

(defmacro vino-inv-test--with-ui (&rest body)
  "Render the inventory in a throwaway buffer and evaluate BODY in it."
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer "*vino-inventory-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (vino-inv-ui-mode)
           (vino-inv-ui-update)
           (vino-inv-test--goto-first-entry)
           ,@body)
       (kill-buffer buffer))))

(defun vino-inv-test--column (name)
  "Return the definition of column NAME in `vino-inv-ui-columns'."
  (seq-find (lambda (col) (string-equal (car col) name))
            vino-inv-ui-columns))

(defun vino-inv-test--columns-idx ()
  "Return the column index alist the inventory UI builds."
  (seq-map-indexed (lambda (col idx) (cons (car col) idx))
                   vino-inv-ui-columns))

(defun vino-inv-test--entry (&rest cells)
  "Return a Tabulated List entry whose columns hold CELLS."
  (list "id" (apply #'vector cells)))

(defun vino-inv-test--cell (name)
  "Return the value of column NAME in the entry under point."
  (substring-no-properties
   (elt (tabulated-list-get-entry)
        (alist-get name vino-inv-ui--columns-idx nil nil #'string-equal))))

(describe "vino-inv-ui--column-sort-fn"
  (it "compares the values of the column it names"
    (let* ((vino-inv-ui--columns-idx '(("ID" . 0) ("Location" . 1)))
           (fn (vino-inv-ui--column-sort-fn "Location" #'string-lessp)))
      (expect (funcall fn
                       (vino-inv-test--entry "2" "Cellar")
                       (vino-inv-test--entry "1" "Fridge"))
              :to-be-truthy)
      (expect (funcall fn
                       (vino-inv-test--entry "1" "Fridge")
                       (vino-inv-test--entry "2" "Cellar"))
              :to-be nil))))

(describe "vino-inv-ui-columns"
  (it "sorts each price column by its own values"
    ;; the two price columns hold different numbers, so a column sorting by
    ;; its neighbour orders the list by a value the reader cannot see
    (let* ((vino-inv-ui--columns-idx (vino-inv-test--columns-idx))
           (fn (nth 2 (vino-inv-test--column "Price Public")))
           (cheap (vino-inv-test--entry "1" "P" "W" "2017" "10.00 EUR"
                                        "99.00 EUR" "99.00" "d" "Cellar" ""))
           (dear (vino-inv-test--entry "2" "P" "W" "2017" "20.00 EUR"
                                       "99.00 EUR" "99.00" "d" "Cellar" "")))
      (expect (funcall fn cheap dear) :to-be-truthy)
      (expect (funcall fn dear cheap) :to-be nil)))

  (it "sorts prices in the same currency by amount"
    (let* ((vino-inv-ui--columns-idx (vino-inv-test--columns-idx))
           (fn (nth 2 (vino-inv-test--column "Price")))
           (cheap (vino-inv-test--entry "1" "P" "W" "2017" "" "9.00 EUR"
                                        "9.00" "d" "Cellar" ""))
           (dear (vino-inv-test--entry "2" "P" "W" "2017" "" "10.00 EUR"
                                       "10.00" "d" "Cellar" "")))
      (expect (funcall fn cheap dear) :to-be-truthy)
      (expect (funcall fn dear cheap) :to-be nil)))

  (it "sorts prices in different currencies by currency"
    (let* ((vino-inv-ui--columns-idx (vino-inv-test--columns-idx))
           (fn (nth 2 (vino-inv-test--column "Price")))
           (eur (vino-inv-test--entry "1" "P" "W" "2017" "" "99.00 EUR"
                                      "99.00" "d" "Cellar" ""))
           (usd (vino-inv-test--entry "2" "P" "W" "2017" "" "10.00 USD"
                                      "10.00" "d" "Cellar" "")))
      (expect (funcall fn eur usd) :to-be-truthy)
      (expect (funcall fn usd eur) :to-be nil)))

  (it "sorts a wine that carries no public price without complaining"
    (let* ((vino-inv-ui--columns-idx (vino-inv-test--columns-idx))
           (fn (nth 2 (vino-inv-test--column "Price Public")))
           (priced (vino-inv-test--entry "1" "P" "W" "2017" "10.00 EUR"
                                         "10.00 EUR" "12.00" "d" "Cellar" ""))
           (bare (vino-inv-test--entry "2" "P" "W" "2017" ""
                                       "10.00 EUR" "12.00" "d" "Cellar" "")))
      (expect (funcall fn priced bare) :not :to-throw)
      (expect (funcall fn bare priced) :not :to-throw)))

  (it "sorts prices in USD by amount"
    (let* ((vino-inv-ui--columns-idx (vino-inv-test--columns-idx))
           (fn (nth 2 (vino-inv-test--column "Price USD")))
           (cheap (vino-inv-test--entry "1" "P" "W" "2017" "" "9.00 EUR"
                                        "9.00" "d" "Cellar" ""))
           (dear (vino-inv-test--entry "2" "P" "W" "2017" "" "10.00 EUR"
                                       "10.00" "d" "Cellar" "")))
      (expect (funcall fn cheap dear) :to-be-truthy)
      (expect (funcall fn dear cheap) :to-be nil))))

(describe "vino-inv-ui-render-cell"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "renders the bottle id"
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "ID" (vino-inv-test--bottle)))
            :to-equal "12"))

  (it "renders the producer name carried by the link"
    (expect (vino-inv-ui-render-cell "Producer" (vino-inv-test--bottle))
            :to-equal "Arianna Occhipinti"))

  (it "renders a producer that is not a link as it is"
    ;; a failed `string-match' leaves behind the match data of whatever ran
    ;; before it, so the cell has to check the match instead of trusting it
    (string-match "\\(a\\)\\(b\\)" "ab")
    (let ((wine (vino-inv-test--reload
                 (vino-inv-test--wine-note "Plain Wine 2019"
                                           '(("producer" . "Plain Producer"))))))
      (expect (vino-inv-ui-render-cell "Producer"
                                       (vino-inv-test--bottle :wine wine))
              :to-equal "Plain Producer")))

  (it "renders an empty cell for a wine that names no producer"
    (let ((wine (vino-inv-test--reload
                 (vino-inv-test--wine-note "Bare Wine 2019"))))
      (expect (vino-inv-ui-render-cell "Producer"
                                       (vino-inv-test--bottle :wine wine))
              :to-equal "")))

  (it "renders the name of the wine"
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "Wine" (vino-inv-test--bottle)))
            :to-equal "Bombolieri BB"))

  (it "renders an empty cell for a wine that carries no name"
    (let ((wine (vino-inv-test--reload
                 (vino-inv-test--wine-note "Bare Wine 2019"))))
      (expect (substring-no-properties
               (vino-inv-ui-render-cell "Wine" (vino-inv-test--bottle :wine wine)))
              :to-equal "")))

  (it "renders the vintage"
    (expect (vino-inv-ui-render-cell "Vintage" (vino-inv-test--bottle))
            :to-equal "2017"))

  (it "calls a wine with no vintage NV"
    (let ((wine (vino-inv-test--reload
                 (vino-inv-test--wine-note "Bare Wine"))))
      (expect (vino-inv-ui-render-cell "Vintage"
                                       (vino-inv-test--bottle :wine wine))
              :to-equal "NV")))

  (it "renders the public price the wine carries"
    (expect (vino-inv-ui-render-cell "Price Public" (vino-inv-test--bottle))
            :to-equal "50.00 EUR"))

  (it "renders an empty public price for a wine that carries none"
    (let ((wine (vino-inv-test--reload
                 (vino-inv-test--wine-note "Bare Wine 2019"))))
      (expect (vino-inv-ui-render-cell "Price Public"
                                       (vino-inv-test--bottle :wine wine))
              :to-equal "")))

  (it "renders what was paid for the bottle"
    (expect (vino-inv-ui-render-cell "Price" (vino-inv-test--bottle))
            :to-equal "10 EUR")
    (expect (vino-inv-ui-render-cell "Price USD" (vino-inv-test--bottle))
            :to-equal "12 USD"))

  (it "renders the purchase date, the location and the source"
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "Date" (vino-inv-test--bottle)))
            :to-equal "2021-01-01")
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "Location" (vino-inv-test--bottle)))
            :to-equal "Cellar")
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "Source" (vino-inv-test--bottle)))
            :to-equal "Shop"))

  (it "renders the comment"
    (expect (substring-no-properties
             (vino-inv-ui-render-cell
              "Comment" (vino-inv-test--bottle :comment "corked")))
            :to-equal "corked"))

  (it "renders an empty comment for a bottle without one"
    (expect (substring-no-properties
             (vino-inv-ui-render-cell "Comment" (vino-inv-test--bottle)))
            :to-equal ""))

  (it "reads the key without caring about its case"
    (expect (vino-inv-ui-render-cell "VINTAGE" (vino-inv-test--bottle))
            :to-equal (vino-inv-ui-render-cell "vintage" (vino-inv-test--bottle))))

  (it "renders every column the UI asks for"
    (--each (append vino-inv-ui-columns nil)
      (expect (vino-inv-ui-render-cell (car it) (vino-inv-test--bottle))
              :not :to-throw)))

  (it "refuses a key it does not know"
    (expect (vino-inv-ui-render-cell "Colour" (vino-inv-test--bottle))
            :to-throw 'user-error)))

(describe "vino-inv-ui-render-header"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "counts the bottles, the wines and what they cost"
    (expect (substring-no-properties
             (vino-inv-ui-render-header
              (list (vino-inv-test--bottle :id 1)
                    (vino-inv-test--bottle :id 2))))
            :to-equal "Total 2 bottles of 1 wines purchased for 24.00 USD"))

  (it "counts a wine once however many bottles are held"
    (let ((other (vino-inv-test--wine-note "Another Wine 2019")))
      (expect (substring-no-properties
               (vino-inv-ui-render-header
                (list (vino-inv-test--bottle :id 1)
                      (vino-inv-test--bottle :id 2)
                      (vino-inv-test--bottle :id 3 :wine other))))
              :to-equal "Total 3 bottles of 2 wines purchased for 36.00 USD")))

  (it "has nothing to count for an empty inventory"
    (expect (substring-no-properties (vino-inv-ui-render-header nil))
            :to-equal "Total 0 bottles of 0 wines purchased for 0.00 USD"))

  (it "renders the numbers in bold"
    (let ((header (vino-inv-ui-render-header
                   (list (vino-inv-test--bottle :id 1)))))
      (--each '("1 bottles" "1 wines" "12.00 USD")
        (expect (vino-inv-test--face-at header it) :to-equal 'bold)))))

(describe "vino-inv-ui entry ids"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "carries the wine and the bottle of an entry"
    (let ((bottle (vino-inv-test--bottle :id 7)))
      (expect (vino-inv-ui-entry-id bottle)
              :to-equal (concat vino-inv-test--wine ":7"))))

  (it "reads the wine and the bottle back"
    (let ((id (vino-inv-ui-entry-id (vino-inv-test--bottle :id 7))))
      (expect (vino-inv-ui-entry-wine-id id) :to-equal vino-inv-test--wine)
      (expect (vino-inv-ui-entry-bottle-id id) :to-equal 7)))

  (it "reads back a wine id that carries a colon itself"
    ;; `org-id-prefix' puts a prefix and a colon in front of every id
    (let ((id (vino-inv-ui-entry-id
               (vino-inv-test--bottle
                :id 7
                :wine (make-vulpea-note :id "vino:c9937e3e" :title "W")))))
      (expect (vino-inv-ui-entry-wine-id id) :to-equal "vino:c9937e3e")
      (expect (vino-inv-ui-entry-bottle-id id) :to-equal 7))))

(describe "vino-inv-ui-update"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "lists every bottle that is still held"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 3)))
        (vino-inv-consume-bottle :bottle-id (vino-inv-bottle-id (car bottles))
                                 :date "2021-02-01")
        (vino-inv-test--with-ui
          (expect (-map #'car tabulated-list-entries)
                  :to-have-same-items-as
                  (--map (concat vino-inv-test--wine ":"
                                 (number-to-string (vino-inv-bottle-id it)))
                         (cdr bottles)))))))

  (it "orders the bottles by the title of their wine"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine))
            (other (vino-inv-test--wine-note "AAA Wine 2019")))
        (vino-inv-test--acquire wine 1)
        (vino-inv-test--acquire other 1)
        (vino-inv-test--with-ui
          (expect (--map (vino-inv-ui-entry-wine-id (car it))
                         tabulated-list-entries)
                  :to-equal (list (vulpea-note-id other) vino-inv-test--wine))))))

  (it "renders a row for every bottle"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--with-ui
          (expect (vino-inv-test--cell "Wine") :to-equal "Bombolieri BB")
          (expect (vino-inv-test--cell "Location") :to-equal "Cellar")
          (expect (vino-inv-test--cell "Price") :to-equal "10 EUR")))))

  (it "counts the inventory in the header line"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--with-ui
          (expect (substring-no-properties header-line-format)
                  :to-equal
                  "Total 2 bottles of 1 wines purchased for 24.00 USD")))))

  (it "knows where each column sits"
    (vino-inv-test--with-fresh-db
      (vino-inv-test--with-ui
        (expect vino-inv-ui--columns-idx :to-equal (vino-inv-test--columns-idx))
        (expect tabulated-list-format :to-equal vino-inv-ui-columns))))

  (it "has nothing to list for an empty inventory"
    (vino-inv-test--with-fresh-db
      (vino-inv-test--with-ui
        (expect tabulated-list-entries :to-be nil)))))

(describe "vino-inv-ui"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "opens the inventory in its own buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (unwind-protect
            (progn
              (vino-inv-ui)
              (expect (buffer-name) :to-equal "*vino-inventory*")
              (expect major-mode :to-equal 'vino-inv-ui-mode)
              (expect (length tabulated-list-entries) :to-equal 1))
          (kill-buffer "*vino-inventory*")))))

  (it "reuses the buffer it already opened"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (unwind-protect
            (let ((buffer (progn (vino-inv-ui) (current-buffer))))
              (vino-inv-test--acquire wine 1)
              (vino-inv-ui)
              (expect (current-buffer) :to-be buffer)
              (expect (length tabulated-list-entries) :to-equal 2))
          (kill-buffer "*vino-inventory*"))))))

(describe "vino-inv-ui-quit"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "leaves the inventory buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (unwind-protect
            (progn
              (vino-inv-ui)
              (vino-inv-ui-quit)
              (expect (buffer-name) :not :to-equal "*vino-inventory*"))
          (kill-buffer "*vino-inventory*"))))))

(describe "vino-inv-ui-visit"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "visits the wine of the bottle under point"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'vulpea-visit)
        (vino-inv-test--with-ui
          (vino-inv-ui-visit)
          (expect 'vulpea-visit :to-have-been-called-with
                  vino-inv-test--wine nil)))))

  (it "visits in another window when asked to"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'vulpea-visit)
        (vino-inv-test--with-ui
          (vino-inv-ui-visit 'other-window)
          (expect 'vulpea-visit :to-have-been-called-with
                  vino-inv-test--wine 'other-window))))))

(describe "vino-inv-ui-get-bottle-id"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "reads the bottle and the wine under point"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-test--with-ui
          (expect (vino-inv-ui-get-bottle-id)
                  :to-equal (vino-inv-bottle-id bottle))
          (expect (vino-inv-ui-get-wine-id) :to-equal vino-inv-test--wine)))))

  (it "says so when there is no bottle under point"
    (vino-inv-test--with-fresh-db
      (vino-inv-test--with-ui
        (expect (vino-inv-ui-get-bottle-id) :to-throw 'user-error)
        (expect (vino-inv-ui-get-wine-id) :to-throw 'user-error)))))

(describe "vino-inv-ui-mark"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "marks the bottle under point"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--with-ui
          (let ((start (point)))
            (vino-inv-ui-mark)
            (expect (char-after start) :to-equal ?*))))))

  (it "moves on to the next bottle"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--with-ui
          (let ((first (vino-inv-ui-get-bottle-id)))
            (vino-inv-ui-mark)
            (expect (vino-inv-ui-get-bottle-id) :not :to-equal first))))))

  (it "takes the mark back off"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 2)
        (vino-inv-test--with-ui
          (let ((start (point)))
            (vino-inv-ui-mark)
            (goto-char start)
            (vino-inv-ui-unmark)
            (expect (char-after start) :to-equal ?\s)))))))

(describe "vino-inv-ui-dispatch-action"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "acts on the bottle under point when nothing is marked"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 3))
             (seen nil))
        (vino-inv-test--with-ui
          (vino-inv-ui-dispatch-action (lambda (b) (push b seen)))
          (expect (vino-inv-test--bottle-ids seen)
                  :to-equal (list (vino-inv-bottle-id (car bottles))))))))

  (it "acts on every marked bottle"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 3))
             (seen nil))
        (vino-inv-test--with-ui
          (vino-inv-ui-mark)
          (vino-inv-ui-mark)
          (goto-char (point-max))
          (vino-inv-ui-dispatch-action (lambda (b) (push b seen)))
          (expect (vino-inv-test--bottle-ids seen)
                  :to-equal (vino-inv-test--bottle-ids (-take 2 bottles)))))))

  (it "hands over resolved bottles"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine))
            (seen nil))
        (vino-inv-test--acquire wine 1)
        (vino-inv-test--with-ui
          (vino-inv-ui-dispatch-action (lambda (b) (push b seen)))
          (expect (vino-inv-location-name (vino-inv-bottle-location (car seen)))
                  :to-equal "Cellar"))))))

(describe "vino-inv-ui-read-location"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "returns the location that was picked"
    (vino-inv-test--with-fresh-db
      (let ((added (vino-inv-add-location "Cellar")))
        (spy-on 'completing-read :and-return-value "Cellar")
        (expect (vino-inv-location-id (vino-inv-ui-read-location))
                :to-equal (vino-inv-location-id added)))))

  (it "creates a location that does not exist yet"
    (vino-inv-test--with-fresh-db
      (vino-inv-add-location "Cellar")
      (spy-on 'completing-read :and-return-value "Fridge")
      (expect (vino-inv-location-name (vino-inv-ui-read-location))
              :to-equal "Fridge")
      (expect (-map #'vino-inv-location-name (vino-inv-query-locations))
              :to-have-same-items-as '("Cellar" "Fridge"))))

  (it "offers the locations that exist"
    (vino-inv-test--with-fresh-db
      (vino-inv-add-location "Cellar")
      (vino-inv-add-location "Fridge")
      (spy-on 'completing-read :and-return-value "Cellar")
      (vino-inv-ui-read-location)
      (expect (nth 1 (spy-calls-args-for 'completing-read 0))
              :to-have-same-items-as '("Cellar" "Fridge")))))

(describe "vino-inv-move-bottle"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "moves the bottle to its new location"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1)))
             (fridge (vino-inv-add-location "Fridge")))
        (vino-inv-move-bottle :bottle-id (vino-inv-bottle-id bottle)
                              :location-id (vino-inv-location-id fridge)
                              :date "2021-05-05")
        (expect (vino-inv-location-name
                 (vino-inv-bottle-location
                  (vino-inv-get-bottle (vino-inv-bottle-id bottle))))
                :to-equal "Fridge"))))

  (it "records where the bottle went and when"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1)))
             (fridge (vino-inv-add-location "Fridge")))
        (vino-inv-move-bottle :bottle-id (vino-inv-bottle-id bottle)
                              :location-id (vino-inv-location-id fridge)
                              :date "2021-05-05")
        (expect (emacsql (vino-inv-db)
                         [:select [transaction-date destination-location-id]
                          :from transaction
                          :where (= transaction-type 'move)])
                :to-equal
                (list (list "2021-05-05" (vino-inv-location-id fridge)))))))

  (it "leaves the bottle available"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1)))
             (fridge (vino-inv-add-location "Fridge")))
        (vino-inv-move-bottle :bottle-id (vino-inv-bottle-id bottle)
                              :location-id (vino-inv-location-id fridge)
                              :date "2021-05-05")
        (expect (length (vino-inv-query-available-bottles)) :to-equal 1)
        (expect (vino-inv-count-bottles-for vino-inv-test--wine)
                :to-equal '(1 . 0)))))

  (it "moves the bottle today when no date is given"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1)))
             (fridge (vino-inv-add-location "Fridge")))
        (vino-inv-move-bottle :bottle-id (vino-inv-bottle-id bottle)
                              :location-id (vino-inv-location-id fridge))
        (expect (caar (emacsql (vino-inv-db)
                               [:select [transaction-date] :from transaction
                                :where (= transaction-type 'move)]))
                :to-equal (format-time-string "%Y-%m-%d")))))

  (it "refuses a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (let ((fridge (vino-inv-add-location "Fridge")))
        (expect (vino-inv-move-bottle :bottle-id 404
                                      :location-id (vino-inv-location-id fridge)
                                      :date "2021-05-05")
                :to-throw 'user-error)
        (expect (caar (emacsql (vino-inv-db)
                               [:select (funcall count *) :from transaction]))
                :to-equal 0)))))

(describe "vino-inv-set-bottle-price"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records what the bottle cost"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-set-bottle-price :bottle-id (vino-inv-bottle-id bottle)
                                   :price "42.00 EUR"
                                   :price-usd "45.00 USD")
        (let ((updated (vino-inv-get-bottle (vino-inv-bottle-id bottle))))
          (expect (vino-inv-bottle-price updated) :to-equal "42.00 EUR")
          (expect (vino-inv-bottle-price-usd updated) :to-equal "45.00 USD")))))

  (it "leaves the other bottles alone"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 2)))
        (vino-inv-set-bottle-price :bottle-id (vino-inv-bottle-id (car bottles))
                                   :price "42.00 EUR"
                                   :price-usd "45.00 USD")
        (expect (vino-inv-bottle-price
                 (vino-inv-get-bottle (vino-inv-bottle-id (nth 1 bottles))))
                :to-equal "10 EUR"))))

  (it "refuses a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-set-bottle-price :bottle-id 404
                                         :price "42.00 EUR"
                                         :price-usd "45.00 USD")
              :to-throw 'user-error))))

(describe "vino-inv-set-bottle-date"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "moves the purchase date of the bottle"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-set-bottle-date :bottle-id (vino-inv-bottle-id bottle)
                                  :date "2022-05-06")
        (expect (vino-inv-bottle-purchase-date
                 (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                :to-equal "2022-05-06"))))

  (it "moves the purchase transaction along with it"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-set-bottle-date :bottle-id (vino-inv-bottle-id bottle)
                                  :date "2022-05-06")
        (expect (caar (emacsql (vino-inv-db)
                               [:select [transaction-date] :from transaction
                                :where (= transaction-type 'purchase)]))
                :to-equal "2022-05-06"))))

  (it "leaves the other bottles alone"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 2)))
        (vino-inv-set-bottle-date :bottle-id (vino-inv-bottle-id (car bottles))
                                  :date "2022-05-06")
        (expect (vino-inv-bottle-purchase-date
                 (vino-inv-get-bottle (vino-inv-bottle-id (nth 1 bottles))))
                :to-equal "2021-01-01")
        (expect (-map #'car (emacsql (vino-inv-db)
                                     [:select [transaction-date]
                                      :from transaction
                                      :where (= transaction-type 'purchase)]))
                :to-have-same-items-as '("2022-05-06" "2021-01-01")))))

  (it "refuses a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-set-bottle-date :bottle-id 404 :date "2022-05-06")
              :to-throw 'user-error)))

  (it "refuses a bottle with more than one purchase transaction"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (emacsql (vino-inv-db)
                 [:insert :into transaction [bottle-id
                                             transaction-type
                                             transaction-date]
                  :values $v1]
                 `([,(vino-inv-bottle-id bottle) purchase "2021-06-06"]))
        (expect (vino-inv-set-bottle-date :bottle-id (vino-inv-bottle-id bottle)
                                          :date "2022-05-06")
                :to-throw 'user-error)
        (expect (vino-inv-bottle-purchase-date
                 (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                :to-equal "2021-01-01")))))

(describe "vino-inv-set-bottle-comment"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records the comment"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (vino-inv-set-bottle-comment :bottle-id (vino-inv-bottle-id bottle)
                                     :comment "gift from Anna")
        (expect (vino-inv-bottle-comment
                 (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                :to-equal "gift from Anna"))))

  (it "refuses a bottle that does not exist"
    (vino-inv-test--with-fresh-db
      (expect (vino-inv-set-bottle-comment :bottle-id 404 :comment "nope")
              :to-throw 'user-error))))

(describe "vino-inv-ui-edit-location"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "moves the bottle under point"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 2))))
        (spy-on 'completing-read :and-return-value "Fridge")
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-location)
          (expect (vino-inv-location-name
                   (vino-inv-bottle-location
                    (vino-inv-get-bottle (vino-inv-bottle-id bottle))))
                  :to-equal "Fridge")))))

  (it "moves every marked bottle"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 3)))
        (spy-on 'completing-read :and-return-value "Fridge")
        (vino-inv-test--with-ui
          (vino-inv-ui-mark)
          (vino-inv-ui-mark)
          (vino-inv-ui-edit-location)
          (expect (--map (vino-inv-location-name
                          (vino-inv-bottle-location
                           (vino-inv-get-bottle (vino-inv-bottle-id it))))
                         bottles)
                  :to-equal '("Fridge" "Fridge" "Cellar"))))))

  (it "tells the hooks about every bottle it moved"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottles (vino-inv-test--acquire wine 2))
             (seen nil)
             (vino-inv-edit-location-handle-functions
              (list (lambda (b) (push b seen)))))
        (spy-on 'completing-read :and-return-value "Fridge")
        (vino-inv-test--with-ui
          (vino-inv-ui-mark)
          (vino-inv-ui-mark)
          (vino-inv-ui-edit-location)
          (expect (vino-inv-test--bottle-ids seen)
                  :to-equal (vino-inv-test--bottle-ids bottles))))))

  (it "shows the new location in the buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'completing-read :and-return-value "Fridge")
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-location)
          (vino-inv-test--goto-first-entry)
          (expect (vino-inv-test--cell "Location") :to-equal "Fridge"))))))

(describe "vino-inv-ui-edit-price"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records the new price on the bottle"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (spy-on 'completing-read :and-return-value "50.00 EUR")
        (spy-on 'read-number :and-return-value 60)
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-price)
          (let ((updated (vino-inv-get-bottle (vino-inv-bottle-id bottle))))
            (expect (vino-inv-bottle-price updated) :to-equal "50.00 EUR")
            (expect (vino-inv-bottle-price-usd updated)
                    :to-equal "60.00 USD"))))))

  (it "keeps a price already given in USD as it is"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (spy-on 'completing-read :and-return-values '("30.00 USD" "public"))
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-price)
          (expect (vino-inv-bottle-price-usd
                   (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                  :to-equal "30.00 USD")))))

  (it "records a price the wine did not carry yet"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'completing-read :and-return-values '("42.00 EUR" "public"))
        (spy-on 'read-number :and-return-value 45)
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-price))
        (expect (vulpea-note-meta-get-list (vino-inv-test--reload wine) "price")
                :to-have-same-items-as '("42.00 EUR" "50.00 EUR")))))

  (it "does not ask where to record a price the wine already carries"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'completing-read :and-return-value "50.00 EUR")
        (spy-on 'read-number :and-return-value 60)
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-price)
          (expect (spy-calls-count 'completing-read) :to-equal 1)))))

  (it "shows the new price in the buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'completing-read :and-return-value "50.00 EUR")
        (spy-on 'read-number :and-return-value 60)
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-price)
          (vino-inv-test--goto-first-entry)
          (expect (vino-inv-test--cell "Price") :to-equal "50.00 EUR"))))))

(describe "vino-inv-ui-edit-date"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "moves the bottle and its purchase transaction to the new date"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (spy-on 'org-read-date :and-return-value (date-to-time "2022-05-06"))
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-date)
          (expect (vino-inv-bottle-purchase-date
                   (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                  :to-equal "2022-05-06")
          (expect (caar (emacsql (vino-inv-db)
                                 [:select [transaction-date] :from transaction
                                  :where (= transaction-type 'purchase)]))
                  :to-equal "2022-05-06")))))

  (it "offers the date the bottle carries as the default"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1 :date "2021-03-04")
        (spy-on 'org-read-date :and-return-value (date-to-time "2022-05-06"))
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-date)
          (expect (nth 4 (spy-calls-args-for 'org-read-date 0))
                  :to-equal (date-to-time "2021-03-04"))))))

  (it "shows the new date in the buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'org-read-date :and-return-value (date-to-time "2022-05-06"))
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-date)
          (vino-inv-test--goto-first-entry)
          (expect (vino-inv-test--cell "Date") :to-equal "2022-05-06"))))))

(describe "vino-inv-ui-edit-comment"
  (before-each (vino-test-init))
  (after-each (vino-test-teardown))

  (it "records the comment on the bottle under point"
    (vino-inv-test--with-fresh-db
      (let* ((wine (vulpea-db-get-by-id vino-inv-test--wine))
             (bottle (car (vino-inv-test--acquire wine 1))))
        (spy-on 'vino--read-string :and-return-value "gift from Anna")
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-comment)
          (expect (vino-inv-bottle-comment
                   (vino-inv-get-bottle (vino-inv-bottle-id bottle)))
                  :to-equal "gift from Anna")))))

  (it "shows the comment in the buffer"
    (vino-inv-test--with-fresh-db
      (let ((wine (vulpea-db-get-by-id vino-inv-test--wine)))
        (vino-inv-test--acquire wine 1)
        (spy-on 'vino--read-string :and-return-value "gift from Anna")
        (vino-inv-test--with-ui
          (vino-inv-ui-edit-comment)
          (vino-inv-test--goto-first-entry)
          (expect (vino-inv-test--cell "Comment")
                  :to-equal "gift from Anna"))))))

(provide 'vino-inv-test)
;;; vino-inv-test.el ends here
