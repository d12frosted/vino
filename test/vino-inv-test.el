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

(provide 'vino-inv-test)
;;; vino-inv-test.el ends here
