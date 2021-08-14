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

  ;; TODO Test (infix-notation-calculator-on-current-line)

  ;; TODO Test mode (infix-notation-calculator-mode)

  )


(provide 'infix-notation-calculator-test)

;;; infix-notation-calculator-test.el ends here
