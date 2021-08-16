;; infix-notation-calculator.el --- Infix Notation Calculator -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 14 Aug 2021
;; Modified: 16 Aug 2021
;; Version: 1.0.0
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-infix-notation-calculator

;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Calculate selected region, use the calculator mode or calculate text in the mini-buffer.  All calculations are stored in history buffer.
;;
;; GENERATE PARSER
;; Run "make admin" from terminal.
;;
;; RUN TESTS
;; Run "make test" from terminal.
;;
;; ACTIVATE CALCULATOR MODE
;; Call `(infix-notation-calculator-mode)'.
;; In this mode press `C-return' to calculate the line you are at.
;;
;; CALCULATE SELECTED REGION
;; Call `(infix-notation-calculator-on-region)'.
;;
;; CALCULATE TEXT IN THE MINI-BUFFER
;; Call `(infix-notation-calculator-on-minibuffer)'.
;;
;; SEE THE LOG OF CALCULATIONS
;; Just visit the buffer `*InfixCalc History*'.
;;
;; TRANSLATE ANY STRING (ADVANCED)
;; Just call the function `(infix-notation-calculator--translate-string)'.
;;

;;; Code:


(require 'infix-notation-calculator-parser)

(defvar infix-notation-calculator-font-face
  '(
    ("\\([0-9]+\\.[0-9]+\\|[0-9]+\\)" . font-lock-constant-face)
    ("\\()\\|(\\)" . font-lock-function-name-face)
    ("\\(+\\|-\\|*\\|\\/\\|\\^\\|=\\)" . font-lock-variable-name-face)
    )
  "The default font-face for mode.")

(defvar infix-notation-calculator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") #'infix-notation-calculator-on-current-line)
    map)
  "Keymap for `infix-notation-calculator-mode'.")

(define-derived-mode infix-notation-calculator-mode fundamental-mode "InfixCalc"

  ;; Skip comments when navigating via syntax-table
  (setq-local parse-sexp-ignore-comments t)

  ;; Font lock
  (setq-local
   font-lock-defaults
   '(infix-notation-calculator-font-face))

  )

;;;###autoload
(defun infix-notation-calculator-on-minibuffer ()
  "Calculate results of mini-buffer and output result in messages."
  (interactive)
  (let ((minibuffer-string (read-string "Calculate:")))
    (setq
     minibuffer-string
     (infix-notation-calculator--adjust-string
      minibuffer-string))
    (let ((translation
           (infix-notation-calculator--translate-string
            minibuffer-string)))
      (infix-notation-calculator--log-calculation
       minibuffer-string
       translation)
      (message "%S" translation)
      translation)))

(defun infix-notation-calculator-on-current-line ()
  "Calculate results of current line and output results on next line."
  (interactive)
  (let ((start)
        (end))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (let ((line (buffer-substring-no-properties start end)))

      ;; When we have a trailing equals symbol, delete it
      (when (string-match-p "^.+=$" line)
        (delete-char -1)
        (setq line (substring line 0 -1)))

      (setq
       line
       (infix-notation-calculator--adjust-string
        line))
      (let ((translation
             (infix-notation-calculator--translate-string
              line)))
        (infix-notation-calculator--log-calculation
         line
         translation)
        (insert (format "\n=%S" translation))
        translation))))

;;;###autoload
(defun infix-notation-calculator-on-selected-region ()
  "Calculate results of selected region."
  (interactive)
  (let ((selected-text (buffer-substring-no-properties (mark) (point))))
    (setq
     selected-text
     (infix-notation-calculator--adjust-string
      selected-text))
    (let ((translation
           (infix-notation-calculator--translate-string
            selected-text)))
      (infix-notation-calculator--log-calculation
       selected-text
       translation)
      (message "%s" translation)
      translation)))

(defun infix-notation-calculator--translate-string (string)
  "Translate STRING, return value."
  (unless (get-buffer "*infix-notation-calculator-buffer*")
    (generate-new-buffer "*infix-notation-calculator-buffer*"))
  (with-current-buffer "*infix-notation-calculator-buffer*"
    (kill-region (point-min) (point-max))
    (insert string)
    (let ((translation (infix-notation-calculator-parser-translate)))
      (kill-buffer)
      translation)))

(defun infix-notation-calculator--log-calculation (query result)
  "Log calculation QUERY and RESULT in buffer *InfixCalc History*."
  ;; Should not switch buffer
  (unless (get-buffer "*InfixCalc History*")
    (generate-new-buffer "*InfixCalc History*"))
  (with-current-buffer "*InfixCalc History*"
    (goto-char (point-max))
    (insert (format "\n%s=%s" query result))))

(defun infix-notation-calculator--adjust-string (string)
  "Adjust STRING for parser."
  ;; Remove trailing equals sign
  (when (string-match-p "^.+=$" string)
    (setq string (substring string 0 -1)))

  ;; Add trailing newline if missing
  (unless (string-match-p "^.+\n$" string)
    (setq string (concat string "
")))

  string)


(provide 'infix-notation-calculator)

;;; infix-notation-calculator.el ends here
