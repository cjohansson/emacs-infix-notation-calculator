;; infix-notation-calculator.el --- Infix Notation Calculator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'infix-notation-calculator-parser)

(defvar infix-notation-calculator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") #'infix-notation-calculator-on-current-line)
    map)
  "Keymap for `infix-notation-calculator-mode'.")

(define-derived-mode infix-notation-calculator-mode fundamental-mode "InfixCalc"

  ;; Skip comments when navigating via syntax-table
  (setq-local parse-sexp-ignore-comments t)

  ;; Font lock
  ;; This makes it possible to have full control over syntax coloring from the lexer
  (setq-local font-lock-keywords-only nil)
  (setq-local font-lock-defaults '(nil t))

  ;; TODO Perhaps fix syntax-coloring?

  )

(defun infix-notation-calculator-on-current-line ()
  "Calculate results of current line and output results on next line."

  ;; TODO
  )

;;;###autoload
(defun infix-notation-calculator-on-selected-region ()
  "Calculate results of selected region."
  (interactive)
  (let ((selected-text (buffer-substring-no-properties (mark) (point))))
    (setq
     selected-text
     (infix-notation-calculator--adjust-string
      selected-text))
    (infix-notation-calculator--translate-string
     selected-text)))

(defun infix-notation-calculator--translate-string (string)
  "Translate STRING, return value."
  (unless (get-buffer "*infix-notation-calculator-buffer*")
    (generate-new-buffer "*infix-notation-calculator-buffer*"))
  (switch-to-buffer "*infix-notation-calculator-buffer*")
  (kill-region (point-min) (point-max))
  (insert string)
  (let ((translation (infix-notation-calculator-parser-translate)))
    (kill-buffer)
    translation))

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
