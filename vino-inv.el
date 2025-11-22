;;; vino-inv.el --- Inventory solution for Vino -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 06 Nov 2023
;;
;; URL: https://github.com/d12frosted/
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
;;; Code:

(require 'vulpea)
(require 'vino)
(require 'dash)
(require 's)

;; * hooks

(defvar vino-inv-acquire-handle-functions nil
  "Abnormal hooks to run after a bottle is acquired.

The hook is called with single argument - `vino-inv-bottle'.")

(defvar vino-inv-consume-handle-functions nil
  "Abnormal hooks to run after a bottle is consumed.

The hook is called with three arguments - `vino-inv-bottle',
action (string) and date (internal time).")

(defvar vino-inv-edit-location-handle-functions nil
  "Abnormal hooks to run after a bottle is moved.

The hook is called with single arguments - `vino-inv-bottle'.")

;; * inv setup

;;;###autoload
(defun vino-inv-setup ()
  "Setup `vino-inv' module."
  (add-hook 'vino-entry-update-handle-functions #'vino-inv-update-availability))

(defun vino-inv-update-availability (note)
  "Update available metadata in wine NOTE."
  (let* ((in (vino-inv-count-purchased-bottles-for (vulpea-note-id note)))
         (out (vino-inv-count-consumed-bottles-for (vulpea-note-id note)))
         (cur (- in out)))
    (vulpea-meta-set note "acquired" in 'append)
    (vulpea-meta-set note "consumed" out 'append)
    (vulpea-meta-set note "available" cur 'append)))

;; * commands

;;;###autoload
(defun vino-inv-find-file-available ()
  "Select and visit available wine."
  (interactive)
  (let* ((available (vino-inv-query-available-wines))
         (res (vulpea-select-from "Wine" available)))
    (if (vulpea-note-id res)
        (find-file (vulpea-note-path res))
      (user-error
       "Can not visit vino entry that does not exist: %s"
       (vulpea-note-title res)))))

;;;###autoload
(defun vino-inv-acquire (&optional note)
  "Acquire wine represented as NOTE."
  (interactive)
  (let* ((note (or note (vino-entry-note-get-dwim)))

         ;; source
         (sources (vino-inv-query-sources))
         (source (completing-read "Source: " (-map #'vino-inv-source-name sources)))
         (source-id (vino-inv-source-id
                     (if-let ((s (--find (string-equal (vino-inv-source-name it) source) sources)))
                         s (vino-inv-add-source source))))

         ;; location
         (locations (vino-inv-query-locations))
         (location (completing-read "Initial location: " (-map #'vino-inv-location-name locations)))
         (location-id (vino-inv-location-id
                       (if-let ((s (--find (string-equal (vino-inv-location-name it) location) locations)))
                           s (vino-inv-add-location location))))


         ;; price
         (prices-public (vulpea-note-meta-get-list note "price"))
         (prices-private (vulpea-note-meta-get-list note "price private"))
         (prices (-uniq (-concat prices-public prices-private '("0 XXX"))))
         (price (if prices
                    (completing-read "Price: " prices)
                  (read-string "Price: ")))
         (price-usd (cond
                     ((s-suffix-p "USD" price) price)
                     ((= 0 (string-to-number price)) "0 USD")
                     (t (format "%.2f USD" (read-number (format "Convert %s to USD: " price))))))
         (price-add-as (cond
                        ((seq-contains-p prices-public price) nil)
                        ((seq-contains-p prices-private price) nil)
                        ((string-prefix-p "0 " price) nil)
                        (t (completing-read "Add this price as: "
                                            '(private public skip) nil t))))

         ;; etc
         (amount (read-number "Amount: " 1))
         (date (format-time-string "%Y-%m-%d" (org-read-date nil t))))

    ;; add price if needed
    (pcase price-add-as
      (`"public" (vulpea-meta-set note "price" (cons price prices-public) 'append))
      (`"private" (vulpea-meta-set note "price private" (cons price prices-private) 'append)))

    (--each (-iota amount)
      (let ((bottle (vino-inv-add-bottle
                     :wine note
                     :date date
                     :price price
                     :price-usd price-usd
                     :location-id location-id
                     :source-id source-id)))
        (run-hook-with-args 'vino-inv-acquire-handle-functions bottle)))

    (vino-inv-update-availability note)))

;;;###autoload
(defun vino-inv-consume (&optional note)
  "Consume wine represented as NOTE."
  (interactive)
  (let* ((note (or note (vino-entry-note-get-dwim)))
         (bottles (vino-inv-query-available-bottles-for (vulpea-note-id note)))
         (_ (unless bottles (user-error "There are no bottles to consume")))
         (bottle (completing-read
                  "Bottle: "
                  (--map
                   (concat
                    (propertize (concat (number-to-string (vino-inv-bottle-id it)) " ")
                                'invisible t)
                    (propertize (concat "#" (number-to-string (vino-inv-bottle-id it)))
                                'face 'barberry-theme-face-salient)
                    (propertize " [" 'face 'barberry-theme-face-faded)
                    (vino-inv-bottle-purchase-date it)
                    (propertize "] @" 'face 'barberry-theme-face-faded)
                    (vino-inv-location-name (vino-inv-bottle-location it))
                    (propertize " - " 'face 'barberry-theme-face-faded)
                    (vino-inv-bottle-price it)
                    (propertize " from " 'face 'barberry-theme-face-faded)
                    (vino-inv-source-name (vino-inv-bottle-source it)))
                   bottles)
                  nil t))
         ;; we use invisible part as a hack
         (bottle-id (string-to-number bottle))
         (bottle (vino-inv-get-bottle bottle-id))
         (action (read-string "Action: " "consume"))
         (date (org-read-date nil t)))
    (vino-inv-consume-bottle :bottle-id bottle-id :date (format-time-string "%Y-%m-%d" date))
    (vino-inv-update-availability note)
    (run-hook-with-args 'vino-inv-consume-handle-functions bottle action date)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate? "))
      (vino-entry-rate note date `((bottle-id . ,bottle-id))))))

;; * database

(defvar vino-inv-db-file (expand-file-name "wine.db" org-directory)
  "Location of inventory database file.

This file is source of truth, so keep it somewhere safe.")

(defvar vino-inv-db--connection nil)

(defun vino-inv-db ()
  "Return connection to inventory database.

In case database doesn't exist yet, bootstrap it.

The connection is cached. Use `vino-inv-db--close' to reset it."
  (let* ((file vino-inv-db-file)
         (exists (file-exists-p file))
         (db (or vino-inv-db--connection (emacsql-sqlite-open file))))
    (setq vino-inv-db--connection db)
    (unless exists
      (vino-inv-db--setup db))
    db))

(defun vino-inv-db-close ()
  "Close connection to inventory database."
  (emacsql-close vino-inv-db--connection)
  (setq vino-inv-db--connection nil))

(defun vino-inv-db--setup (db)
  "Setup inventory database DB."
  (emacsql db [:create-table location :if :not :exists
                             ([(location-id integer :primary-key :autoincrement)
                               (name :not-null :unique)])])
  (emacsql db [:create-table source :if :not :exists
                             ([(source-id integer :primary-key :autoincrement)
                               (name :not-null :unique)])])
  (emacsql db [:create-table bottle :if :not :exists
                             ([(bottle-id integer :primary-key :autoincrement)
                               (wine-id :not-null)
                               (purchase-date :not-null)
                               (price :not-null)
                               (price-usd :not-null)
                               (location-id :not-null)
                               (source-id integer :not-null)
                               (comment)]
                              (:foreign-key [location-id] :references location [location-id])
                              (:foreign-key [source-id] :references source [source-id]))])
  (emacsql db [:create-table transaction :if :not :exists
                             ([(transaction-id integer :primary-key :autoincrement)
                               (bottle-id integer :not-null)
                               ;; purchase, consume, move
                               (transaction-type :not-null)
                               (transaction-date :not-null)
                               (destination-location-id integer)]
                              (:foreign-key [bottle-id] :references bottle [bottle-id])
                              (:foreign-key [destination-location-id] :references location [location-id]))]))

;; * types

(cl-defstruct vino-inv-source id name)
(cl-defstruct vino-inv-location id name)

;; materialized
(cl-defstruct vino-inv-bottle id wine purchase-date price price-usd location source comment)

;; materialized
(cl-defstruct vino-inv-txn id bottle type date dest-location)

;; * queries

(defun vino-inv-get-source (id)
  "Get source by ID."
  (let ((row (car (emacsql (vino-inv-db)
                           [:select [source-id name]
                                    :from source
                                    :where (= source-id $s1)]
                           id))))
    (make-vino-inv-source
     :id (nth 0 row)
     :name (nth 1 row))))

(defun vino-inv-get-location (id)
  "Get location by ID."
  (let ((row (car (emacsql (vino-inv-db)
                           [:select [location-id name]
                                    :from location
                                    :where (= location-id $s1)]
                           id))))
    (make-vino-inv-location
     :id (nth 0 row)
     :name (nth 1 row))))

(defun vino-inv-get-bottle (id)
  "Get bottle by ID."
  (let* ((rows (->> (emacsql
                     (vino-inv-db)
                     [:select
                      [bottle-id     ; 0
                       wine-id       ; 1
                       purchase-date ; 2
                       price         ; 3
                       price-usd     ; 4
                       location-id   ; 5
                       source-id     ; 6
                       comment]      ; 7
                      :from [bottle]
                      :where (= bottle:bottle-id $s1)]
                     id)))
         (row (car rows))
         (wine (vulpea-db-get-by-id (nth 1 row)))
         (location (vino-inv-get-location (nth 5 row)))
         (source (vino-inv-get-source (nth 6 row))))
    (make-vino-inv-bottle
     :id (nth 0 row)
     :wine wine
     :purchase-date (nth 2 row)
     :price (nth 3 row)
     :price-usd (nth 4 row)
     :location location
     :source source
     :comment (nth 7 row))))

(defun vino-inv-query-sources ()
  "Return list of sources."
  (->> (emacsql (vino-inv-db) [:select [source-id name] :from source])
       (--map (make-vino-inv-source
               :id (nth 0 it)
               :name (nth 1 it)))))

(defun vino-inv-query-locations ()
  "Return list of locations."
  (->> (emacsql (vino-inv-db) [:select [location-id name] :from location])
       (--map (make-vino-inv-location
               :id (nth 0 it)
               :name (nth 1 it)))))

(defun vino-inv-query-available-wines ()
  "Return list of all available wines.

This is an efficient function to get list of wines without
duplicates."
  (->> (emacsql
        (vino-inv-db)
        [:select
         [bottle:wine-id]
         :from [bottle]
         :left-join (as [:select
                         [bottle-id
                          (as
                           (funcall sum
                                    [:case :when (= transaction-type 'purchase) :then 1
                                           :when (= transaction-type 'consume) :then -1
                                           :else 0
                                           :end])
                           total-amount)]
                         :from [transaction]
                         :group-by bottle-id]
                        t)
         :on (= bottle:bottle-id t:bottle-id)
         :where (> (funcall coalesce t:total-amount 0) 0)])
       (-flatten-n 1)
       (-uniq)
       (vulpea-db-query-by-ids)))

(defun vino-inv-query-available-bottles ()
  "Return list of all available bottles."
  (let* ((rows (->> (emacsql
                     (vino-inv-db)
                     [:select
                      [bottle:bottle-id ; 0
                       bottle:wine-id   ; 1
                       bottle:purchase-date ; 2
                       bottle:price         ; 3
                       bottle:price-usd     ; 4
                       bottle:location-id   ; 5
                       bottle:source-id     ; 6
                       comment]             ; 7
                      :from [bottle]
                      :left-join (as [:select
                                      [bottle-id
                                       (as
                                        (funcall sum
                                                 [:case :when (= transaction-type 'purchase) :then 1
                                                        :when (= transaction-type 'consume) :then -1
                                                        :else 0
                                                        :end])
                                        total-amount)]
                                      :from [transaction]
                                      :group-by bottle-id]
                                     t)
                      :on (= bottle:bottle-id t:bottle-id)
                      :where (> (funcall coalesce t:total-amount 0) 0)])))
         (wines-tbl (let ((tbl (make-hash-table :test 'equal)))
                      (--each (->> rows
                                   (--map (nth 1 it))
                                   (-uniq)
                                   (vulpea-db-query-by-ids))
                        (puthash (vulpea-note-id it) it tbl))
                      tbl))
         (locations-tbl (let ((tbl (make-hash-table :test 'equal)))
                          (--each (vino-inv-query-locations)
                            (puthash (vino-inv-location-id it) it tbl))
                          tbl))
         (sources-tbl (let ((tbl (make-hash-table :test 'equal)))
                        (--each (vino-inv-query-sources)
                          (puthash (vino-inv-source-id it) it tbl))
                        tbl)))
    (--map
     (make-vino-inv-bottle
      :id (nth 0 it)
      :wine (gethash (nth 1 it) wines-tbl)
      :purchase-date (nth 2 it)
      :price (nth 3 it)
      :price-usd (nth 4 it)
      :location (gethash (nth 5 it) locations-tbl)
      :source (gethash (nth 6 it) sources-tbl)
      :comment (nth 7 it))
     rows)))

(defun vino-inv-query-available-bottles-for (wine-id)
  "Return list of all available bottles for WINE-ID."
  (let* ((rows (->> (emacsql
                     (vino-inv-db)
                     [:select
                      [bottle:bottle-id     ; 0
                       bottle:purchase-date ; 1
                       bottle:price         ; 2
                       bottle:price-usd     ; 3
                       bottle:location-id   ; 4
                       bottle:source-id     ; 5
                       comment]             ; 6
                      :from [bottle]
                      :left-join (as [:select
                                      [bottle-id
                                       (as
                                        (funcall sum
                                                 [:case :when (= transaction-type 'purchase) :then 1
                                                        :when (= transaction-type 'consume) :then -1
                                                        :else 0
                                                        :end])
                                        total-amount)]
                                      :from [transaction]
                                      :group-by bottle-id]
                                     t)
                      :on (= bottle:bottle-id t:bottle-id)
                      :where (and (> (funcall coalesce t:total-amount 0) 0)
                                  (= bottle:wine-id $s1))]
                     wine-id)))
         (wine (vulpea-db-get-by-id wine-id))
         (locations-tbl (let ((tbl (make-hash-table :test 'equal)))
                          (--each (vino-inv-query-locations)
                            (puthash (vino-inv-location-id it) it tbl))
                          tbl))
         (sources-tbl (let ((tbl (make-hash-table :test 'equal)))
                        (--each (vino-inv-query-sources)
                          (puthash (vino-inv-source-id it) it tbl))
                        tbl)))
    (--map
     (make-vino-inv-bottle
      :id (nth 0 it)
      :wine wine
      :purchase-date (nth 1 it)
      :price (nth 2 it)
      :price-usd (nth 3 it)
      :location (gethash (nth 4 it) locations-tbl)
      :source (gethash (nth 5 it) sources-tbl)
      :comment (nth 6 it))
     rows)))

(defun vino-inv-count-purchased-bottles-for (wine-id)
  "Total amount of purchased bottles of wine with WINE-ID."
  (caar
   (emacsql (vino-inv-db)
            [:select (funcall count *)
                     :from [transaction]
                     :join bottle :on (= bottle:bottle-id transaction:bottle-id)
                     :where (= bottle:wine-id $s1)
                     :and (= transaction-type 'purchase)]
            wine-id)))

(defun vino-inv-count-consumed-bottles-for (wine-id)
  "Total amount of consumed bottles of wine with WINE-ID."
  (caar
   (emacsql (vino-inv-db)
            [:select (funcall count *)
                     :from [transaction]
                     :join bottle :on (= bottle:bottle-id transaction:bottle-id)
                     :where (= bottle:wine-id $s1)
                     :and (= transaction-type 'consume)]
            wine-id)))

;; * location operations

(defun vino-inv-add-location (name)
  "Create a new location with NAME."
  (let ((db (vino-inv-db)))
    (emacsql db [:insert :into location [name] :values $v1]
             `([,name]))
    (make-vino-inv-location
     :id (caar (emacsql db [:select (funcall last_insert_rowid)]))
     :name name)))

;; * source operations

(defun vino-inv-add-source (name)
  "Create a new source with NAME."
  (let ((db (vino-inv-db)))
    (emacsql db [:insert :into source [name] :values $v1]
             `([,name]))
    (make-vino-inv-source
     :id (caar (emacsql db [:select (funcall last_insert_rowid)]))
     :name name)))

;; * bottle operations

(cl-defun vino-inv-add-bottle (&key wine
                                    date
                                    price
                                    price-usd
                                    location-id
                                    source-id
                                    comment)
  "Purchase a bottle.

- WINE is a `vulpea-note'.
- DATE is purchase date.
- PRICE is price of the purchase in any currency.
- PRICE-USD is price of the purchase in USD.
- LOCATION-ID is id of the initial location.
- SOURCE-ID is id of the source.
- COMMENT is optional."
  (let ((db (vino-inv-db))
        (bottle-id))
    (emacsql-with-transaction db
      (emacsql db
               [:insert
                :into bottle [wine-id
                              purchase-date
                              price
                              price-usd
                              location-id
                              source-id
                              comment]
                :values $v1]
               `([,(vulpea-note-id wine)
                  ,date
                  ,price
                  ,price-usd
                  ,location-id
                  ,source-id
                  ,comment]))
      (setq bottle-id (caar (emacsql db [:select (funcall last_insert_rowid)])))
      (emacsql db
               [:insert :into transaction [bottle-id
                                           transaction-type
                                           transaction-date]
                        :values $v1]
               `([,bottle-id purchase ,date])))
    (make-vino-inv-bottle
     :id bottle-id
     :wine wine
     :purchase-date date
     :price price
     :price-usd price-usd
     :location (vino-inv-get-location location-id)
     :source (vino-inv-get-source source-id)
     :comment comment)))

(cl-defun vino-inv-consume-bottle (&key bottle-id date)
  "Consume BOTTLE-ID on a DATE."
  (emacsql (vino-inv-db)
           [:insert :into transaction [bottle-id
                                       transaction-type
                                       transaction-date]
                    :values $v1]
           `([,bottle-id consume ,date])))

;; * inventory ui

(defvar-local vino-inv-ui--columns-idx nil)

(defun vino-inv-ui--column-sort-fn (name comp)
  "Return a sort function for column with NAME using COMP.

COMP is called with two arguments, which are the values of the column
itself."
  (lambda (a b)
    (let ((idx (alist-get name vino-inv-ui--columns-idx nil nil #'string-equal)))
      (funcall comp (elt (elt a 1) idx) (elt (elt b 1) idx)))))

(defvar vino-inv-ui-columns
  `[("ID" 5 t)
    ("Producer" 26 t . (:pad-right 2))
    ("Wine" 44 t . (:pad-right 2))
    ("Vintage" 8 t . (:right-align t))
    ("Price Public" 12
     ,(vino-inv-ui--column-sort-fn
       "Price"
       (lambda (a b)
         (let ((c1 (nth 1 (s-split " " a)))
               (c2 (nth 1 (s-split " " b))))
          (if (string-equal c1 c2)
              (< (string-to-number a) (string-to-number b))
            (string-lessp c1 c2)))))
     . (:right-align t))
    ("Price" 12
     ,(vino-inv-ui--column-sort-fn
       "Price"
       (lambda (a b)
         (let ((c1 (nth 1 (s-split " " a)))
               (c2 (nth 1 (s-split " " b))))
          (if (string-equal c1 c2)
              (< (string-to-number a) (string-to-number b))
            (string-lessp c1 c2)))))
     . (:right-align t))
    ("Price USD" 12
     ,(vino-inv-ui--column-sort-fn
       "Price USD"
       (lambda (a b) (< (string-to-number a) (string-to-number b))))
     . (:right-align t))
    ("Date" 10 t)
    ("Location" 16 t)
    ("Comment" 20 t)])

;; ** mode definition

(defvar vino-inv-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'vino-inv-ui-quit)
      (define-key map "g" #'vino-inv-ui-update)
      (define-key map "m" #'vino-inv-ui-mark)
      (define-key map "u" #'vino-inv-ui-unmark)
      (define-key map (kbd "el") #'vino-inv-ui-edit-location)
      (define-key map (kbd "ep") #'vino-inv-ui-edit-price)
      (define-key map (kbd "ed") #'vino-inv-ui-edit-date)
      (define-key map (kbd "ec") #'vino-inv-ui-edit-comment)
      (define-key map (kbd "<RET>") #'vino-inv-ui-visit)))
  "Keymap for `vino-inv-ui-mode'.")

(define-derived-mode vino-inv-ui-mode tabulated-list-mode "vino-inventory"
  "Major mode for listing inventory entries."
  (setq tabulated-list-printer #'tabulated-list-print-entry)
  (setq tabulated-list-padding 2))

;; ** rendering

(defun vino-inv-ui-render-cell (key bottle)
  "Render KEY cell for BOTTLE."
  (let ((wine (vino-inv-bottle-wine bottle)))
    (pcase (s-downcase key)
      (`"id" (propertize (number-to-string (vino-inv-bottle-id bottle))
                         'face 'font-lock-comment-face))
      (`"producer" (let ((str (vulpea-note-meta-get wine "producer")))
                     (string-match org-link-bracket-re str)
                     (match-string 2 str)))
      (`"wine" (propertize (vulpea-note-meta-get wine "name")
                           'face 'link))
      (`"vintage" (or (vulpea-note-meta-get wine "vintage") "NV"))
      (`"price public" (or (vulpea-note-meta-get wine "price") ""))
      (`"price" (vino-inv-bottle-price bottle))
      (`"price usd" (vino-inv-bottle-price-usd bottle))
      (`"date" (propertize (vino-inv-bottle-purchase-date bottle)
                           'face 'font-lock-comment-face))
      (`"location" (propertize (vino-inv-location-name (vino-inv-bottle-location bottle))
                               'face 'font-lock-comment-face))
      (`"source" (propertize (vino-inv-source-name (vino-inv-bottle-source bottle))
                             'face 'font-lock-comment-face))
      (`"comment" (propertize (or (vino-inv-bottle-comment bottle) "")
                              'face 'font-lock-comment-face))
      (k (user-error "Unexpected cell key '%s'" k)))))

(defun vino-inv-ui-render-header (bottles)
  "Render header line for BOTTLES."
  (concat
   "Total "
   (propertize (format "%d" (seq-length bottles)) 'face 'bold)
   " bottles of "
   (propertize (format "%d" (->> bottles
                                 (-map #'vino-inv-bottle-wine)
                                 (-map #'vulpea-note-id)
                                 (-uniq)
                                 (seq-length)))
               'face 'bold)
   " wines purchased for "
   (propertize (format "%.2f USD" (->> bottles
                                       (-map #'vino-inv-bottle-price-usd)
                                       (-map #'string-to-number)
                                       (-sum)))
               'face 'bold)))

;;;###autoload
(defun vino-inv-ui ()
  "Open inventory UI."
  (interactive)
  (let ((buffer (get-buffer-create "*vino-inventory*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'vino-inv-ui-mode)
      (vino-inv-ui-mode))
    (vino-inv-ui-update)))

(defun vino-inv-ui-update ()
  "Update inventory entries."
  (interactive)
  (let* ((bottles (vino-inv-query-available-bottles)))
    (setq tabulated-list-format vino-inv-ui-columns
          tabulated-list-entries
          (->> bottles
               (--sort (string< (vulpea-note-title (vino-inv-bottle-wine it))
                                (vulpea-note-title (vino-inv-bottle-wine other))))
               (--map
                (list
                 (concat (vulpea-note-id (vino-inv-bottle-wine it))
                         ":"
                         (number-to-string (vino-inv-bottle-id it)))
                 (apply
                  #'vector
                  (-map (lambda (col)
                          (vino-inv-ui-render-cell (car col) it))
                        vino-inv-ui-columns)))))
          tabulated-list-sort-key nil
          tabulated-list-use-header-line nil
          header-line-format (vino-inv-ui-render-header bottles)
          vino-inv-ui--columns-idx (seq-map-indexed (lambda (c idx) (cons (car c) idx))
                                                    vino-inv-ui-columns)))
  (tabulated-list-init-header)
  (tabulated-list-print 'rembember-pos))

;; ** utils

(defsubst vino-inv-ui-get-wine-id (&optional pos)
  "Return the wine ID of the Tabulated List entry at POS.

POS, if omitted or nil, defaults to point."
  (let ((id (tabulated-list-get-id pos)))
    (car (s-split ":" id))))

(defsubst vino-inv-ui-get-bottle-id (&optional pos)
  "Return the bottle ID of the Tabulated List entry at POS.

POS, if omitted or nil, defaults to point."
  (let ((id (tabulated-list-get-id pos)))
    (string-to-number (nth 1 (s-split ":" id)))))

(defun vino-inv-ui-read-location (&optional require-match)
  "Read and return location.

If REQUIRE-MATCH is non nil and the user select a non-existing
location, it will be created automatically."
  (let* ((locations (vino-inv-query-locations))
         (location (completing-read "Initial location: "
                                    (-map #'vino-inv-location-name locations)
                                    nil require-match)))
    (if-let ((s (--find (string-equal (vino-inv-location-name it) location) locations)))
        s (vino-inv-add-location location))))

;; ** actions

(defun vino-inv-ui-quit ()
  "Quit from inventory UI."
  (interactive)
  (quit-window))

(defun vino-inv-ui-visit (&optional other-window)
  "Visit cellar entry at point.

If OTHER-WINDOW, visit the NOTE in another window."
  (interactive)
  (let ((id (vino-inv-ui-get-wine-id)))
    (org-roam-node-visit
     (org-roam-node-from-id id)
     (or current-prefix-arg other-window))))

(defun vino-inv-ui-edit-location ()
  "Edit location of the bottle at point."
  (interactive)
  (let ((location-id (vino-inv-location-id (vino-inv-ui-read-location)))
        (db (vino-inv-db))
        (date (format-time-string "%Y-%m-%d")))
    (vino-inv-ui-dispatch-action
     (lambda (bottle)
       (let ((bottle-id (vino-inv-bottle-id bottle)))
         (emacsql-with-transaction db
           (emacsql db [:update bottle
                                :set (= location-id $s2)
                                :where (= bottle-id $s1)]
                    bottle-id location-id)
           (emacsql db
                    [:insert :into transaction [bottle-id
                                                transaction-type
                                                transaction-date
                                                destination-location-id]
                             :values $v1]
                    `([,bottle-id move ,date ,location-id])))
         (run-hook-with-args 'vino-inv-edit-location-handle-functions bottle))))
    (vino-inv-ui-update)))

(defun vino-inv-ui-edit-price ()
  "Edit price of the bottle at point."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (wine-id (vino-inv-ui-get-wine-id))
         (note (vulpea-db-get-by-id wine-id))

         (prices-public (vulpea-note-meta-get-list note "price"))
         (prices-private (vulpea-note-meta-get-list note "price private"))
         (prices (-uniq (-concat prices-public prices-private)))
         (price (if prices
                    (completing-read "Price: " prices)
                  (read-string "Price: ")))
         (price-usd (cond
                     ((s-suffix-p "USD" price) price)
                     ((= (string-to-number price) 0) "0 USD")
                     (t (format "%.2f USD" (read-number (format "Convert %s to USD: " price))))))
         (price-add-as (cond
                        ((seq-contains-p prices-public price) nil)
                        ((seq-contains-p prices-private price) nil)
                        (t (completing-read "Add this price as: "
                                            '(private public skip) nil t)))))

    (emacsql (vino-inv-db) [:update bottle
                                    :set [(= price $s2) (= price-usd $s3)]
                                    :where (= bottle-id $s1)]
             bottle-id price price-usd)

    (when (and price-add-as (not (string-equal "skip" price-add-as)))
      (vulpea-meta-set
       note
       (pcase price-add-as
         (`"public" "price")
         (`"private" "price private"))
       (cons price (pcase price-add-as
                     (`"public" prices-public)
                     (`"private" prices-private)))
       'append))

    (vino-inv-ui-update)))

(defun vino-inv-ui-edit-date ()
  "Edit date of the bottle at point."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (bottle (vino-inv-get-bottle bottle-id))
         (date (format-time-string
                "%Y-%m-%d"
                (org-read-date nil t nil nil
                               (date-to-time (vino-inv-bottle-purchase-date bottle)))))
         (db (vino-inv-db))
         (txn-ids (emacsql db [:select [transaction-id]
                                       :from transaction
                                       :where (and (= bottle-id $s1)
                                                   (= transaction-type 'purchase))]
                           bottle-id))
         (txn-id (car txn-ids)))
    (unless (= 1 (seq-length txn-ids))
      (user-error "The bottle has multiple purchase transactions"))
    (emacsql-with-transaction db
      (emacsql db [:update bottle
                           :set [(= purchase-date $s2)]
                           :where (= bottle-id $s1)]
               bottle-id date)
      (emacsql db [:update transaction
                           :set [(= transaction-date $s2)]
                           :where (= transaction-id $s1)]
               txn-id date))
    (vino-inv-ui-update)))

(defun vino-inv-ui-edit-comment ()
  "Edit comment of the bottle at point."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (comment (read-string "Comment: ")))
    (emacsql (vino-inv-db)
             [:update bottle
                      :set [(= comment $s2)]
                      :where (= bottle-id $s1)]
             bottle-id comment)
    (vino-inv-ui-update)))

(defun vino-inv-ui-mark ()
  "Mark entry at point."
  (interactive)
  (tabulated-list-put-tag "*" t))

(defun vino-inv-ui-unmark ()
  "Mark entry at point."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun vino-inv-ui-dispatch-action (fn)
  "Execute action FN over bottle under point or marked bottles.

If there are marked bottles, FN is executed sequentially on each marked
bottle. Otherwise FN is executed on bottled under point.

FN is called with `vino-inv-bottle' as its only argument."
  (let (print-list bottle cmd)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (when (eq cmd ?\*)
          ;; This is the key PKG-DESC.
          (setq bottle (tabulated-list-get-id))
          (push bottle print-list))
        (forward-line)))
    (-each (->> (or (--map (string-to-number (nth 1 (s-split ":" it))) print-list)
                    (list (vino-inv-ui-get-bottle-id)))
                (-map #'vino-inv-get-bottle))
      fn)))

(provide 'vino-inv)
;;; vino-inv.el ends here
