;;; +fun-test.el --- Test `+fun' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Jan 2021
;;
;; URL: https://github.com/d12frosted/vino
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `+fun' module/
;;
;;; Code:

(require 'buttercup)
(require '+fun)

(describe "+fun-collect-while"
  (it "repeats a function until filter returns nil"
    (let ((n 0))
      (expect (+fun-collect-while
               (lambda () (setq n (+ 1 n)))
               (lambda (v) (< v 5)))
              :to-equal '(1 2 3 4)))))

(provide '+fun-test)
;;; +fun-test.el ends here
