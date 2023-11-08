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
         (db (or vino-inv-db--connection (emacsql-sqlite file))))
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
                 (volume integer :not-null)
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
(cl-defstruct vino-inv-bottle id wine volume purchase-date price price-usd location source comment)

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
                       volume        ; 2
                       purchase-date ; 3
                       price         ; 4
                       price-usd     ; 5
                       location-id   ; 6
                       source-id     ; 7
                       comment]      ; 8
                      :from [bottle]
                      :where (= bottle:bottle-id $s1)]
                     id)))
         (row (car rows))
         (wine (vulpea-db-get-by-id (nth 1 row)))
         (location (vino-inv-get-location (nth 6 row)))
         (source (vino-inv-get-source (nth 7 row))))
    (make-vino-inv-bottle
     :id (nth 0 row)
     :wine wine
     :volume (nth 2 row)
     :purchase-date (nth 3 row)
     :price (nth 4 row)
     :price-usd (nth 5 row)
     :location location
     :source source
     :comment (nth 8 row))))

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
                       bottle:volume    ; 2
                       bottle:purchase-date ; 3
                       bottle:price         ; 4
                       bottle:price-usd     ; 5
                       bottle:location-id   ; 6
                       bottle:source-id     ; 7
                       comment]             ; 8
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
      :volume (nth 2 it)
      :purchase-date (nth 3 it)
      :price (nth 4 it)
      :price-usd (nth 5 it)
      :location (gethash (nth 6 it) locations-tbl)
      :source (gethash (nth 7 it) sources-tbl)
      :comment (nth 8 it))
     rows)))

(defun vino-inv-query-available-bottles-for (wine-id)
  "Return list of all available bottles for WINE-ID."
  (let* ((rows (->> (emacsql
                     (vino-inv-db)
                     [:select
                      [bottle:bottle-id     ; 0
                       bottle:volume        ; 1
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
      :volume (nth 2 it)
      :purchase-date (nth 3 it)
      :price (nth 4 it)
      :price-usd (nth 5 it)
      :location (gethash (nth 6 it) locations-tbl)
      :source (gethash (nth 7 it) sources-tbl)
      :comment (nth 8 it))
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

(cl-defun vino-inv-add-bottle (&key wine-id
                                    volume
                                    date
                                    price
                                    price-usd
                                    location-id
                                    source-id
                                    comment)
  "Purchase a bottle.

- WINE-ID is a id of relevant `vulpea-note'.
- VOLUME is measured in milliliters (750 default).
- DATE is purchase date.
- PRICE is price of the purchase in any currency.
- PRICE-USD is price of the purchase in USD.
- LOCATION-ID is id of the initial location.
- SOURCE-ID is id of the source.
- COMMENT is optional."
  (let ((db (vino-inv-db)))
    (emacsql-with-transaction db
      (emacsql db
               [:insert
                :into bottle [wine-id
                              volume
                              purchase-date
                              price
                              price-usd
                              location-id
                              source-id
                              comment]
                :values $v1]
               `([,wine-id
                  ,(or volume 750)
                  ,date
                  ,price
                  ,price-usd
                  ,location-id
                  ,source-id
                  ,comment]))
      (let ((bottle-id (caar (emacsql db [:select (funcall last_insert_rowid)]))))
        (emacsql db
                 [:insert :into transaction [bottle-id
                                             transaction-type
                                             transaction-date]
                  :values $v1]
                 `([,bottle-id purchase ,date]))))))

(cl-defun vino-inv-consume-bottle (&key bottle-id date)
  "Consume BOTTLE-ID on a DATE."
  (emacsql (vino-inv-db)
           [:insert :into transaction [bottle-id
                                       transaction-type
                                       transaction-date]
            :values $v1]
           `([,bottle-id consume ,date])))

;; * inventory ui

(defvar vino-inv-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'vino-inv-ui-quit)
      (define-key map "g" #'vino-inv-ui-update)))
  "Keymap for `vino-inv-ui-mode'.")

(define-derived-mode vino-inv-ui-mode tabulated-list-mode "vino-inventory"
  "Major mode for listing inventory entries."
  (setq tabulated-list-printer #'vino-inv-ui--list-printer))

(defun vino-inv-ui--list-printer (id cols)
  "Propertize entries.

Consult with `tabulated-list-printer' for information about ID
and COLS."
  (setf (aref cols 0) (propertize (aref cols 0) 'face 'barberry-theme-face-faded))
  (setf (aref cols 1) (propertize (aref cols 1) 'face 'barberry-theme-face-faded))
  (setf (aref cols 6) (propertize (aref cols 6) 'face 'barberry-theme-face-faded))
  (tabulated-list-print-entry id cols))

(defun vino-inv-ui-quit ()
  "Quit from inventory UI."
  (interactive)
  (quit-window))

(defun vino-inv-ui-update ()
  "Update inventory entries."
  (interactive)
  (let* ((bottles (emacsql
                   (vino-inv-db)
                   [:select
                    [bottle:bottle-id
                     bottle:wine-id
                     bottle:volume
                     bottle:purchase-date
                     bottle:price
                     (as location:name location-name)]
                    :from [bottle]
                    :join location :on (= bottle:location-id location:location-id)
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
                    :where (> (funcall coalesce t:total-amount 0) 0)]))
         (wines-all (->> bottles
                         (--map (nth 1 it))
                         (-uniq)
                         (vulpea-db-query-by-ids)))
         (wines-tbl (let ((tbl (make-hash-table :test 'equal)))
                      (--each wines-all
                        (puthash (vulpea-note-id it) it tbl))
                      tbl)))
    (setq tabulated-list-entries
          (->> bottles
               (--sort (string< (vulpea-note-title (gethash (nth 1 it) wines-tbl))
                                (vulpea-note-title (gethash (nth 1 other) wines-tbl))))
               (--map
                (list
                 (number-to-string (nth 0 it))
                 (let ((wine (gethash (nth 1 it) wines-tbl)))
                   (vector
                    (number-to-string (nth 0 it))
                    (number-to-string (nth 2 it))
                    (let ((str (vulpea-note-meta-get wine "producer")))
                      (string-match org-link-bracket-re str)
                      (match-string 2 str))
                    (vulpea-note-meta-get wine "name")
                    (or (vulpea-note-meta-get wine "vintage") "NV")
                    (nth 4 it)
                    (nth 3 it))))))))
  (tabulated-list-print 'rembember-pos))

;;;###autoload
(defun vino-inv-ui ()
  "Open inventory UI."
  (interactive)
  (let ((buffer (get-buffer-create "*vino-inventory*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'vino-inv-ui-mode)
      (vino-inv-ui-mode))
    (setq tabulated-list-format [("ID" 5 t)
                                 ("Volume" 6 t . (:right-align t))
                                 ("Producer" 26 t . (:pad-right 2))
                                 ("Wine" 44 t . (:pad-right 2))
                                 ("Vintage" 8 t . (:right-align t))
                                 ("Price" 10 t . (:right-align t))
                                 ("Date" 10 t)])
    (setq tabulated-list-sort-key nil)
    (tabulated-list-init-header)
    (vino-inv-ui-update)))

(provide 'vino-inv)
;;; vino-inv.el ends here
