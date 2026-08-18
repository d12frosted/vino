;;; vino-schema.el --- Schemas for vino notes -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Created: 18 Aug 2026
;;
;; URL: https://github.com/d12frosted/vino
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Vulpea schemas describing the vino note types.
;;
;; Until now the shape of a wine entry lived only inside
;; `vino-entry-create', as the order and validity of its prompts.  This
;; module states it declaratively instead, so the same knowledge can be
;; used to check notes that already exist.
;;
;; Call `vino-schema-setup' to register the schemas:
;;
;;   (require 'vino-schema)
;;   (vino-schema-setup)
;;
;; Afterwards every vulpea validation entry point works on wine notes,
;; e.g. `vulpea-schema-validate-all' for a single schema,
;; `vulpea-schema-note-violations' for the note at hand, or
;; `vulpea-schema-collection-health' for the whole cellar.  With
;; vulpea-ui installed, `vulpea-ui-schema-dashboard' renders the same
;; data.
;;
;; Each schema takes its fields from a variable, so a setup that carries
;; extra metadata can add fields without redefining the predicates:
;;
;;   (add-to-list 'vino-schema-entry-fields
;;                '(:key "drink from" :type number)
;;                'append)
;;   (vino-schema-setup)
;;
;; Required fields are the ones `vino-entry-create' always writes.  Two
;; of them are worth knowing about: `volume' arrives with v0.5.0 and
;; `country' is written by both origin selection strategies, so entries
;; created before those land are reported as incomplete.
;;
;;; Code:

(require 'vulpea)
(require 'vino)

;;; Field predicates
;;

(defun vino-schema-sparkling-p (note)
  "Return non-nil when NOTE is a sparkling wine."
  (eq (vulpea-note-meta-get note "carbonation" 'symbol) 'sparkling))

(defun vino-schema-sweetness-levels (note)
  "Return sweetness levels allowed for NOTE.

The levels depend on the carbonation of the wine, see
`vino-sweetness-levels'.  When NOTE carries no valid carbonation, the
result is nil, which vulpea reads as \"any value goes\" - the carbonation
itself is reported instead."
  (plist-get vino-sweetness-levels
             (vulpea-note-meta-get note "carbonation" 'symbol)))

(defun vino-schema-rating-value-p (value _note)
  "Return t when VALUE is a valid rating of a wine entry.

A wine entry carries the average of its ratings, or \"NA\" when it has
none."
  (if (or (string-equal value "NA")
          (string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
      t
    (format "Expected a number or \"NA\", got %S" value)))

(defun vino-schema-date-p (value _note)
  "Return t when VALUE is a date in YYYY-MM-DD format."
  (if (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" value)
      t
    (format "Expected a date in YYYY-MM-DD format, got %S" value)))

;;; Fields
;;

(defvar vino-schema-entry-fields
  `((:key "carbonation" :type symbol :required t
     :one-of ,(lambda (_note) vino-carbonation-types))
    (:key "carbonation method" :type symbol
     :required vino-schema-sparkling-p
     :one-of ,(lambda (_note) vino-carbonation-methods))
    (:key "colour" :type symbol :required t
     :one-of ,(lambda (_note) vino-colour-types))
    (:key "sweetness" :type symbol :required t
     :one-of vino-schema-sweetness-levels)
    (:key "producer" :type note :required t :target-tags ("wine" "producer"))
    (:key "name" :type string :required t)
    (:key "vintage" :type number)
    (:key "base" :type number)
    (:key "sur lie" :type string)
    (:key "degorgee" :type string)
    (:key "volume" :type number :required t)
    (:key "country" :type note :required t :target-tags ("wine" "country"))
    (:key "region" :type note :target-tags ("wine" "region"))
    (:key "appellation" :type note :target-tags ("wine" "appellation"))
    (:key "grapes" :type note :multiple t :target-tags ("wine" "grape"))
    (:key "alcohol" :type number :required t)
    (:key "sugar" :type number)
    (:key "price" :type string :multiple t)
    (:key "price private" :type string :multiple t)
    (:key "price date" :type string)
    (:key "acquired" :type number)
    (:key "consumed" :type number)
    (:key "available" :type number)
    (:key "rating" :type string :validate vino-schema-rating-value-p)
    (:key "ratings" :type note :multiple t :target-tags ("wine" "rating")))
  "Fields of the `vino-entry' schema.

Vintage is absent on a non vintage wine, and base vintage, sur lie and
degorgee are only meaningful for a traditional method sparkling, so none
of them is required.  Acquisition counters and the average rating are
maintained by vino itself and are optional as well, since a wine that
was never rated or acquired simply lacks them.

See the Commentary of `vulpea-schema' for the field spec format.")

(defvar vino-schema-rating-fields
  '((:key "wine" :type note :required t :target-tags ("wine" "cellar"))
    (:key "date" :type string :required t :validate vino-schema-date-p)
    (:key "version" :type number :required t)
    (:key "score" :type number :required t)
    (:key "score_max" :type number :required t)
    (:key "total" :type number :required t))
  "Fields of the `vino-rating' schema.

Only the fields shared by every rating version are listed.  The values
that make up the score depend on `vino-rating-props' and are named by
the rating system in use, so they are not described here.")

(defvar vino-schema-country-fields nil
  "Fields of the `vino-country' schema.

A country carries no metadata of its own.  The schema exists so that
countries are covered by collection health, and as a place to declare
fields for a setup that keeps more than a name.")

(defvar vino-schema-region-fields
  '((:key "country" :type note :required t :target-tags ("wine" "country"))
    (:key "parent" :type note :target-tags ("wine" "region")))
  "Fields of the `vino-region' schema.")

(defvar vino-schema-appellation-fields
  '((:key "country" :type note :required t :target-tags ("wine" "country"))
    (:key "parent" :type note :target-tags ("wine" "region")))
  "Fields of the `vino-appellation' schema.

An appellation sits inside a region, so its parent is a region and not
another appellation.")

(defvar vino-schema-grape-fields nil
  "Fields of the `vino-grape' schema.

See `vino-schema-country-fields' on why this is empty.")

(defvar vino-schema-producer-fields nil
  "Fields of the `vino-producer' schema.

See `vino-schema-country-fields' on why this is empty.")

;;; Setup
;;

;;;###autoload
(defun vino-schema-setup ()
  "Register schemas for all vino note types.

Defining a schema replaces the one registered under the same name, so
calling this again picks up changes to the field variables and is
otherwise a no-op."
  (interactive)
  (vulpea-schema-define 'vino-country
    :predicate #'vino-country-note-p
    :fields vino-schema-country-fields)
  (vulpea-schema-define 'vino-region
    :predicate #'vino-region-note-p
    :fields vino-schema-region-fields)
  (vulpea-schema-define 'vino-appellation
    :predicate #'vino-appellation-note-p
    :fields vino-schema-appellation-fields)
  (vulpea-schema-define 'vino-grape
    :predicate #'vino-grape-note-p
    :fields vino-schema-grape-fields)
  (vulpea-schema-define 'vino-producer
    :predicate #'vino-producer-note-p
    :fields vino-schema-producer-fields)
  (vulpea-schema-define 'vino-entry
    :predicate #'vino-entry-note-p
    :fields vino-schema-entry-fields)
  (vulpea-schema-define 'vino-rating
    :predicate #'vino-rating-note-p
    :fields vino-schema-rating-fields))

(provide 'vino-schema)
;;; vino-schema.el ends here
