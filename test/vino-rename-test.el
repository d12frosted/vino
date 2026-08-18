;;; vino-rename-test.el --- Test producer renaming -*- lexical-binding: t; -*-
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
;; Test that renaming a producer carries through to the wines it made.
;;
;;; Code:

(require 'buttercup)
(require 'vino)
(require 'vino-test-utils)

(defconst vino-rename-test--producer "9462dfad-603c-4094-9aca-a9042cec5dd2"
  "Arianna Occhipinti, the producer of the wine fixture.")

(defconst vino-rename-test--other-producer "424696f0-0e9f-4f82-9504-89b59305d9db"
  "Pyramid Valley, a producer with no wines.")

(defconst vino-rename-test--wine "c9937e3e-c83d-4d8d-a612-6110e6706252"
  "Arianna Occhipinti Bombolieri BB 2017.")

(defconst vino-rename-test--ratings
  '("be7777a9-7993-44cf-be9e-0ae65297a35d"
    "f1ecb856-c009-4a65-a8d0-8191a9de66dd")
  "Ratings of the wine fixture, oldest first.")

(defun vino-rename-test--link-ratings ()
  "Link the wine fixture to its ratings, as `vino-entry-rate' would."
  (vulpea-utils-with-note (vulpea-db-get-by-id vino-rename-test--wine)
    (vulpea-buffer-meta-set "ratings" vino-rename-test--ratings)
    (save-buffer)
    (vulpea-db-update-file (buffer-file-name (buffer-base-buffer)))))



(describe "vino-producer-wines"
  (before-all (vino-test-init))
  (after-all (vino-test-teardown))

  (it "returns the wines made by a producer"
    (expect (-map #'vulpea-note-id
                  (vino-producer-wines vino-rename-test--producer))
            :to-equal (list vino-rename-test--wine)))

  (it "returns nothing for a producer with no wines"
    (expect (vino-producer-wines vino-rename-test--other-producer)
            :to-be nil))

  (it "accepts a note as well as an id"
    (expect (-map #'vulpea-note-id
                  (vino-producer-wines
                   (vulpea-db-get-by-id vino-rename-test--producer)))
            :to-equal (list vino-rename-test--wine))))



(describe "vino-producer-rename"
  :var (producer-path)

  (before-each
    (vino-test-init)
    (vino-rename-test--link-ratings)
    (setq producer-path
          (vulpea-note-path (vulpea-db-get-by-id vino-rename-test--producer)))
    (vino-producer-rename vino-rename-test--producer "Occhipinti"))

  (after-each (vino-test-teardown))

  (it "gives the producer its new title"
    (expect (vulpea-note-title (vulpea-db-get-by-id vino-rename-test--producer))
            :to-equal "Occhipinti"))

  (it "retitles every wine made by that producer"
    (expect (vulpea-note-title (vulpea-db-get-by-id vino-rename-test--wine))
            :to-equal "Occhipinti Bombolieri BB 2017"))

  (it "refreshes the producer link in the wine entry"
    (expect (vulpea-note-meta-get (vulpea-db-get-by-id vino-rename-test--wine)
                                  "producer")
            :to-equal
            (format "[[id:%s][Occhipinti]]" vino-rename-test--producer)))

  (it "retitles the ratings of those wines"
    (expect (-map (lambda (id) (vulpea-note-title (vulpea-db-get-by-id id)))
                  vino-rename-test--ratings)
            :to-equal
            '("Occhipinti Bombolieri BB 2017 - 2021-01-14"
              "Occhipinti Bombolieri BB 2017 - 2021-01-15")))

  (it "leaves the producer file where it is"
    (expect (vulpea-note-path (vulpea-db-get-by-id vino-rename-test--producer))
            :to-equal producer-path))

  (it "returns the wines it updated"
    (expect (-map #'vulpea-note-id
                  (vino-producer-rename vino-rename-test--producer "Occhipinti II"))
            :to-equal (list vino-rename-test--wine))))



(describe "vino-rename"
  :var ((grape "cb1eb3b9-6233-4916-8c05-a3a4739e0cfa")
        (appellation "6a0819f3-0770-4481-9754-754ca397800b")
        country region)

  (before-each
    (vino-test-init)
    (setq country (vulpea-create "Sicily" "wine/country/${id}.org"
                                 :tags '("wine" "country")))
    (setq region (vulpea-create "Vittoria" "wine/region/${id}.org"
                                :tags '("wine" "region")
                                :meta `(("country" . ,country)))))
  (after-each (vino-test-teardown))

  (it "gives the note its new title"
    (vino-rename grape "Frappato di Vittoria")
    (expect (vulpea-note-title (vulpea-db-get-by-id grape))
            :to-equal "Frappato di Vittoria"))

  (it "refreshes the link description in a wine that references it"
    (vino-rename grape "Frappato di Vittoria")
    (expect (vulpea-note-meta-get (vulpea-db-get-by-id vino-rename-test--wine)
                                  "grapes")
            :to-equal
            (format "[[id:%s][Frappato di Vittoria]]" grape)))

  (it "refreshes link descriptions in prose too"
    (vino-rename grape "Frappato di Vittoria")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id appellation)
              (buffer-string))
            :to-match (regexp-quote
                       (format "[[id:%s][Frappato di Vittoria]]" grape))))

  (it "leaves a description that reads differently alone"
    (let ((note (vulpea-create
                 "Tasting" "wine/note/${id}.org"
                 :tags '("wine")
                 :body (format "Mostly [[id:%s][the local grape]].\n" grape))))
      (vino-rename grape "Frappato di Vittoria")
      (expect (vulpea-utils-with-note (vulpea-db-get-by-id (vulpea-note-id note))
                (buffer-string))
              :to-match (regexp-quote (format "[[id:%s][the local grape]]" grape)))))

  (it "refreshes a country referenced by a region"
    (vino-rename country "Sicilia")
    (expect (vulpea-note-meta-get (vulpea-db-get-by-id (vulpea-note-id region))
                                  "country")
            :to-equal
            (format "[[id:%s][Sicilia]]" (vulpea-note-id country))))

  (it "retitles the wines when the note is a producer"
    (vino-rename vino-rename-test--producer "Occhipinti")
    (expect (vulpea-note-title (vulpea-db-get-by-id vino-rename-test--wine))
            :to-equal "Occhipinti Bombolieri BB 2017"))

  (it "does nothing when the title has not changed"
    (let ((before (vulpea-note-meta-get
                   (vulpea-db-get-by-id vino-rename-test--wine) "grapes")))
      (vino-rename grape "Frappato")
      (expect (vulpea-note-meta-get (vulpea-db-get-by-id vino-rename-test--wine)
                                    "grapes")
              :to-equal before)))

  (it "refuses a wine entry, whose title is derived"
    (expect (vino-rename vino-rename-test--wine "Something Else")
            :to-throw 'user-error))

  (it "refuses a rating, whose title is derived"
    (expect (vino-rename (car vino-rename-test--ratings) "Something Else")
            :to-throw 'user-error)))


(provide 'vino-rename-test)
;;; vino-rename-test.el ends here
