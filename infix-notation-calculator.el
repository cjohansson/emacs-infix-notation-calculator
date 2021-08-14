;; infix-notation-calculator.el --- Infix Notation Calculator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(defun infix-notation-calculator-generate-parser ()
  "Generate parser for plug-in."
  (unless (or
           (fboundp 'parser-generator-lr-export-to-elisp)
           (boundp 'parser-generator--e-identifier)
           )
    (error "Parser Generator is not available!"))

  (setq
   parser-generator--e-identifier
   '%empty)
  (parser-generator-set-look-ahead-number 1)
  (setq
   parser-generator--global-attributes
   '(%left %precedence %right))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (setq
   parser-generator-lr--global-precedence-attributes
   '(%left %precedence %right))
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
  (setq
   parser-generator--global-declaration
   '(
     (%left "-" "+")
     (%left "*" "/")
     (%precedence NEG)
     (%right "^")))
  (setq
   parser-generator-lr--precedence-comparison-function
   (lambda(a-type a-value _b-type b-value)
     (cond

      ((and
        a-value
        b-value)
       (cond
        ((> a-value b-value)
         t)

        ((< a-value b-value)
         nil)

        ((= a-value b-value)

         (cond
          ((equal a-type '%left)
           t)

          ((equal a-type '%right)
           nil)

          ((equal a-type '%precedence)
           t))

         )))

      ((and
        a-value
        (not b-value))
       t)

      ((and
        (not a-value)
        (not b-value))
       nil)

      )))

  (parser-generator-set-grammar
   '(
     (start input line exp)
     ("+" "-" "*" "/" "^" "(" ")" "\n" NUM)
     (
      (start input)
      (input
       %empty
       (input line (lambda(args) (nth 1 args))))
      (line
       "\n"
       (exp "\n" (lambda(args) (nth 0 args))))
      (exp
       NUM
       (exp "+" exp (lambda(args) (+ (float (nth 0 args)) (nth 2 args))))
       (exp "-" exp (lambda(args) (- (float (nth 0 args)) (nth 2 args))))
       (exp "*" exp (lambda(args) (* (float (nth 0 args)) (nth 2 args))))
       (exp "/" exp (lambda(args) (/ (float (nth 0 args)) (nth 2 args))))
       ("-" exp %prec NEG (lambda(args) (- (float (nth 1 args)))))
       (exp "^" exp (lambda(args) (expt (float (nth 0 args)) (nth 2 args))))
       ("(" exp ")" (lambda(args) (nth 1 args)))))
     start))

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (with-current-buffer "*buffer*"
       (let ((token))
         (when
             (<
              index
              (point-max))
           (goto-char
            index)

           ;; Skip white-space(s)
           (when (looking-at-p "[\t ]+")
             (when
                 (search-forward-regexp "[^\t ]" nil t)
               (forward-char -1)))

           (cond
            ((looking-at "\\([0-9]+\\.[0-9]+\\|[0-9]+\\)")
             (setq
              token
              `(NUM ,(match-beginning 0) . ,(match-end 0))))
            ((looking-at "\\(\\+\\|-\\|*\\|/\\|\\^\\|)\\|(\\|\n\\)")
             (let ((symbol
                    (buffer-substring-no-properties
                     (match-beginning 0)
                     (match-end 0))))
               (setq
                token
                `(,symbol ,(match-beginning 0) . ,(match-end 0)))))
            (t (error "Unexpected input at %d!" index))))
         token))))

  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (with-current-buffer "*buffer*"
       (let ((start (car (cdr token)))
             (end (cdr (cdr token))))
         (when (<= end (point-max))
           (let ((symbol
                  (buffer-substring-no-properties start end)))
             (when
                 (string-match-p "^\\([0-9]+\\.[0-9]+\\|[0-9]+\\)$" symbol)
               (setq
                symbol
                (string-to-number symbol)))
             symbol))))))

  (let ((export (parser-generator-lr-export-to-elisp "infix-notation-calculator-parser")))
    (with-temp-buffer
      (insert export)
      (write-file "infix-notation-calculator-parser.el")
      (kill-buffer))
    )

  (message "Parser Generator at infix-notation-calculator-parser.el"))


;;; infix-notation-calculator.el ends here
