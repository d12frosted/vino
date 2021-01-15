;;; +fun.el --- Function extensions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1"))
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
;; Extra functions one might hate to use.
;;
;;; Code:

;;;###autoload
(defun +fun-collect-while (fn filter &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

If FILTER is nil, it does not affect repeat cycle.

If FILTER returns nil, the computed value is not added to result.

ARGS are passed to FN."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (if (and filter
                 (null (funcall filter value)))
            (setq continue nil)
          (setq result (cons value result)))))
    (setq quit-flag nil)
    (seq-reverse result)))

;;;###autoload
(defun +fun-repeat-while (fn filter &rest args)
  "Repeat FN and return the first unfiltered result.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

ARGS are passed to FN."
  (let (value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (when (null (funcall filter value))
          (setq continue nil))))
    (setq quit-flag nil)
    (when (null continue)
      value)))

(provide '+fun)
;;; +fun.el ends here
