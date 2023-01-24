;;; flycheck-eglot.el --- Flycheck support for eglot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Sergey Firsov

;; Author: Sergey Firsov <intramurz@gmail.com>
;; Maintainer: Sergey Firsov <intramurz@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/intramurz/flycheck-eglot
;; Keywords: tools, flycheck, eglot


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'flymake)
(require 'eglot)


(defvar-local flycheck-eglot--current-errors nil)


(defun flycheck-eglot--start (checker callback)
  "Start function for generic checker definition."
  (when (equal checker 'eglot-check)
    (funcall callback
             'finished
             flycheck-eglot--current-errors)))


(defun flycheck-eglot--report-eglot-diagnostics (diags &rest _)
  "Get Eglot diagnostics DIAGS, prepare it and pass to Flycheck.
Report function for `eglot-flymake-backend'."
  (cl-flet
      ((diag-to-err (diag)
                    "Flymake diagnostics to flycheck errors."
                    (with-current-buffer (flymake--diag-locus diag)
                      (flycheck-error-new-at-pos
                       (flymake--diag-beg diag) ; POS
                       (pcase (flymake--diag-type diag) ; LEVEL
                         ('eglot-note 'info)
                         ('eglot-warning 'warning)
                         ('eglot-error 'error)
                         (_ (error "Unknown diagnostic type: %S" diag)))
                       (flymake--diag-text diag)  ; MESSAGE
                       :end-pos (flymake--diag-end diag)
                       :checker 'eglot-check
                       :buffer (current-buffer)
                       :filename (buffer-file-name)))))

    (setq flycheck-eglot--current-errors
          (mapcar #'diag-to-err diags))

    (flycheck-buffer-automatically '(idle-change new-line))))


(defun flycheck-eglot--eglot-available-p ()
  "Is Eglot available."
  (bound-and-true-p eglot--managed-mode))


(flycheck-define-generic-checker 'eglot-check
  "Report Eglot diagnostics with Flycheck."
  :start #'flycheck-eglot--start
  :predicate #'flycheck-eglot--eglot-available-p
  :modes '(prog-mode text-mode))


(provide 'flycheck-eglot)
;;; flycheck-eglot.el ends here
