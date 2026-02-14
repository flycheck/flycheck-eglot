;;; flycheck-eglot.el --- Flycheck support for eglot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Sergey Firsov

;; Author: Sergey Firsov <intramurz@gmail.com>
;; Maintainer: Sergey Firsov <intramurz@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (eglot "1.9") (flycheck "32"))
;; URL: https://github.com/flycheck/flycheck-eglot
;; Keywords: convenience language tools


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

;; This software has its origin in a piece of Doom Emacs code
;; which was distributed under the following terms:

;;  The MIT License (MIT)

;;  Copyright (c) 2014-2022 Henrik Lissner.

;;  Permission is hereby granted, free of charge, to any person obtaining
;;  a copy of this software and associated documentation files (the "Software"),
;;  to deal in the Software without restriction, including
;;  without limitation the rights to use, copy, modify, merge, publish,
;;  distribute, sublicense, and/or sell copies of the Software, and to
;;  permit persons to whom the Software is furnished to do so, subject to
;;  the following conditions:

;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.

;;; Commentary:

;; A simple "glue" minor mode that allows Flycheck and Eglot to work together.
;;
;; You just need to enable `global-flycheck-eglot-mode'.
;; Put the following in your init file:
;;
;;      (require 'flycheck-eglot)
;;      (global-flycheck-eglot-mode 1)
;;
;; By default, the Flycheck-Eglot considers the Eglot to be the only provider
;; of syntax checks.  Other Flycheck checkers are ignored.
;; There is a variable `flycheck-eglot-exclusive' that controls this.
;; You can override it system-wide or for some major modes.

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'flymake)
(require 'eglot)


(defgroup flycheck-eglot nil
  "Flycheck-Eglot compatibility customizations."
  :prefix "flycheck-eglot-"
  :group 'flycheck)


(defcustom flycheck-eglot-exclusive t
  "Is the flycheck-eglot checker exclusive or in a chain of others."
  :type 'boolean
  :local t
  :group 'flycheck-eglot)


(defcustom flycheck-eglot-enable-diagnostic-tags t
  "Enable display of diagnostic tags."
  :type 'boolean
  :group 'flycheck-eglot)


(defvar flycheck-eglot-tag-labels
  '((deprecated . "*")
    (unnecessary . "?"))
  "Diagnostic tag labels.")


(defvar flycheck-eglot-level-tag-separator
  ":"
  "Separator between the level name and diagnostic tag labels.")


(defvar flycheck-eglot-tag-separator
  ""
  "Diagnostic tag label separator.")


(defvar-local flycheck-eglot--current-errors nil)

(defvar-local flycheck-eglot--current-diags nil)


(defun flycheck-eglot--start (checker callback)
  "Start function for generic checker definition.
CHECKER is the current checker (assuming eglot-check).
CALLBACK is a callback function provided by Flycheck."
  (when (eq checker 'eglot-check)
    (funcall callback
             'finished
             flycheck-eglot--current-errors)))


(flymake--diag-accessor flymake-diagnostic-overlay-properties
                        flymake--diag-overlay-properties overlay-properties)


(defun flycheck-eglot--get-error-level (diag)
  "Select or create (if necessary) a flycheck error level.
DIAG is the Eglot diagnostics in Flymake format."
  (let ((level (pcase (flymake-diagnostic-type diag)
                 ('eglot-note 'info)
                 ('eglot-warning 'warning)
                 ('eglot-error 'error)
                 (_ (error "Unknown diagnostic type: %S" diag))))
        (overlay-props (flymake-diagnostic-overlay-properties diag)))
    (if (and flycheck-eglot-enable-diagnostic-tags
             overlay-props)
        (let* ((faces (alist-get 'face overlay-props))
               (tags (mapcar
                      (lambda (face)
                        (pcase face
                          ('eglot-diagnostic-tag-unnecessary-face 'unnecessary)
                          ('eglot-diagnostic-tag-deprecated-face 'deprecated)
                          (_ (error "Unknown eglot face: %S" face))))
                      faces))
               (name (format "%s%s%s"
                             level
                             flycheck-eglot-level-tag-separator
                             (mapconcat (lambda (tag)
                                          (alist-get tag flycheck-eglot-tag-labels))
                                        tags flycheck-eglot-tag-separator))))

          (or (intern-soft name)
              (let* ((new-level (intern name))
                     (face (get (flycheck-error-level-overlay-category level)
                                'face))
                     (faces (append faces
                                    (list face)))
                     (priority (get (flycheck-error-level-overlay-category level)
                                    'priority))
                     (bitmaps (cons (flycheck-error-level-fringe-bitmap level)
                                    (flycheck-error-level-fringe-bitmap level t)))
                     (category (intern (format "%s-category" name))))

                (setf (get category 'face) faces)
                (setf (get category 'priority) priority)

                (flycheck-define-error-level new-level
                  :severity (flycheck-error-level-severity level)
                  :compilation-level (flycheck-error-level-compilation-level level)
                  :overlay-category category
                  :fringe-bitmap bitmaps
                  :fringe-face (flycheck-error-level-fringe-face level)
                  :margin-spec (flycheck-error-level-margin-spec level)
                  :error-list-face (flycheck-error-level-error-list-face level))

                new-level)))
      level)))


(defun flycheck-eglot--report-eglot-diagnostics (diags &rest args)
  "Report function for the `eglot-flymake-backend'.
DIAGS is the Eglot diagnostics list in Flymake format."
  (when-let* ((reg (plist-get args :region))
              (beg (car reg))
              (end (cdr reg)))

    (setq flycheck-eglot--current-diags
          (cond ((and (= beg (point-min)) (= end (point-max)))
                 (append diags nil))

                ((and (= beg (point-min)) (= end (point-min)))
                 (append flycheck-eglot--current-diags diags))

                ;; The default case never seems to appear in the context of interactions with eglot.
                ;; Most likely, this is dead code.
                ;; But the :region parameter specification for flymake report functions
                ;; requires this behavior. So be it.
                (t (append (cl-remove-if (lambda (diag)
                                           (let* (
                                                  (diag-beg (flymake-diagnostic-beg diag))
                                                  (diag-end (or (flymake-diagnostic-end diag)
                                                                diag-beg)))

                                             (or (> beg diag-beg)
                                                 (< end diag-end))))
                                         flycheck-eglot--current-diags)
                           diags))))

    (setq flycheck-eglot--current-errors (mapcar (lambda (diag)
                                                   ;; Translate flymake--diag to flycheck-error struct
                                                   (with-current-buffer (flymake-diagnostic-buffer diag)
                                                     (flycheck-error-new-at-pos
                                                      (flymake-diagnostic-beg diag) ; POS
                                                      (flycheck-eglot--get-error-level diag) ; LEVEL
                                                      (flymake-diagnostic-text diag)  ; MESSAGE
                                                      :end-pos (flymake-diagnostic-end diag)
                                                      :checker 'eglot-check
                                                      :buffer (current-buffer)
                                                      :filename (buffer-file-name))))
                                                 flycheck-eglot--current-diags)))

  (flycheck-buffer-automatically '(idle-change new-line)))


(defun flycheck-eglot--eglot-available-p ()
  "Is Eglot available."
  (and (fboundp 'eglot-managed-p)
       (eglot-managed-p)))


(flycheck-define-generic-checker 'eglot-check
  "Reports Eglot-provided diagnostics with Flycheck."
  :start #'flycheck-eglot--start
  :predicate #'flycheck-eglot--eglot-available-p
  :modes '(prog-mode text-mode))


(defun flycheck-eglot--flymake-diagnostics-wrapper (orig &optional beg end)
  "Does the job of the `flymake-diagnostic' when it can't.
ORIG is the original function, (BEG END) is the range"
  (if (not flycheck-eglot-mode)
      (funcall orig beg end)
    (cl-remove-if-not (lambda (s)
                        (cond (end (<= beg
                                       (flymake-diagnostic-beg s)
                                       (flymake-diagnostic-end s)
                                       end))
                              (beg (= beg (flymake-diagnostic-beg s)))
                              (t t)))
                      flycheck-eglot--current-diags)))


(defun flycheck-eglot--setup ()
  "Setup flycheck-eglot."
  (when (flycheck-eglot--eglot-available-p)
    (flycheck-eglot--register-eglot-checker major-mode)
    (setq flycheck-checker 'eglot-check)
    (eglot-flymake-backend #'flycheck-eglot--report-eglot-diagnostics)
    (advice-add #'flymake-diagnostics :around #'flycheck-eglot--flymake-diagnostics-wrapper)
    (flymake-mode -1)
    (flycheck-mode 1)))

(defun flycheck-eglot--register-eglot-checker (mode)
  "Register `eglot-check' for major mode MODE."
  (add-to-list 'flycheck-checkers 'eglot-check t)
  (unless (member mode (flycheck-checker-get 'eglot-check 'modes))
    (flycheck-add-mode 'eglot-check mode))
  (if flycheck-eglot-exclusive
      (setf (flycheck-checker-get 'eglot-check 'next-checkers) nil)
    (when-let ((checker (cl-find-if (lambda (checker)
                                      (and (not (eq checker 'eglot-check))
                                           (flycheck-checker-supports-major-mode-p checker mode)))
                                    flycheck-checkers)))
      (flycheck-add-next-checker 'eglot-check checker))))

(defun flycheck-eglot--teardown ()
  "Teardown flycheck-eglot."
  (when (flycheck-eglot--eglot-available-p)
    (eglot-flymake-backend #'ignore)
    (setq flycheck-checker nil)
    (setq flycheck-eglot--current-errors nil)
    (flycheck-buffer-deferred)))


;;;###autoload
(define-minor-mode flycheck-eglot-mode
  "Minor mode for using Flycheck with Eglot."
  :init-value nil
  :lighter nil
  (if flycheck-eglot-mode
      (flycheck-eglot--setup)
    (flycheck-eglot--teardown)))


;;;###autoload
(define-globalized-minor-mode global-flycheck-eglot-mode
  flycheck-eglot-mode
  (lambda ()
    (when (flycheck-eglot--eglot-available-p)
      (flycheck-eglot-mode 1)))
  :group 'flycheck-eglot
  (if global-flycheck-eglot-mode
      (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode)
    (remove-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode)
    (setq flycheck-checkers
          (remove 'eglot-check
                  flycheck-checkers))))


(provide 'flycheck-eglot)
;;; flycheck-eglot.el ends here
