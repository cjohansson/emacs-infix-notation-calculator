;; infix-notation-calculator.el --- Infix Notation Calculator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'infix-notation-calculator-parser)

;; TODO Create mode here based on fundamental mode

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
