;; infix-notation-calculator-test.el --- Test for Infix Notation Calculator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'infix-notation-calculator)
(require 'ert)

(defun infix-notation-calculator-test ()
  "Test mode and interactive functions."

  (should
   (equal
    "abc\n"
    (infix-notation-calculator--adjust-string "abc=")))
  (should
   (equal
    "abc\n"
    (infix-notation-calculator--adjust-string "abc\n")))
  (message "Passes tests for (infix-notation-calculator--adjust-string)")

  (should
   (equal
    3.0
    (infix-notation-calculator--translate-string "-5+8\n")))
  (should
   (equal
    14.0
    (infix-notation-calculator--translate-string "2+3*4\n")))
  (should
   (equal
    10.0
    (infix-notation-calculator--translate-string "2*3+4\n")))
  (message "Passes tests for (infix-notation-calculator--translate-string)")

  (with-temp-buffer
    (insert "-5-1")
    (should
     (equal
      -6.0
      (infix-notation-calculator-on-current-line)))

    (goto-char (point-max))
    (insert "\n")
    (insert "10+30/5=")
    (should
     (equal
      16.0
      (infix-notation-calculator-on-current-line)))

    (let ((buffer-contents
           (buffer-substring-no-properties
            (point-min)
            (point-max))))
      (should
       (equal
        "-5-1=\n-6.0\n10+30/5=\n16.0"
        buffer-contents))))
  (message "Passes tests for (infix-notation-calculator-on-current-line)")

  ;; TODO Make this work
  (with-temp-buffer
    (infix-notation-calculator-mode)
    (insert "3*(1+1)=")
    (execute-kbd-macro (kbd "<C-return>"))
    (insert "\n5+3*3")
    (execute-kbd-macro (kbd "<C-return>"))
    (let ((buffer-contents
           (buffer-substring-no-properties
            (point-min)
            (point-max))))
      (should
       (equal
        "3*(1+1)=\n6.0\n5+3*3=\n14.0"
        buffer-contents))))
  (message "Passes tests for (infix-notation-calculator-mode)")

  )


(provide 'infix-notation-calculator-test)

;;; infix-notation-calculator-test.el ends here
