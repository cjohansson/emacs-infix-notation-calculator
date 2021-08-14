;;; infix-notation-calculator-parser.el --- Exported Emacs Parser Generator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; Constants:


(defconst
  infix-notation-calculator-parser--action-tables
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (0 0 1 1 2 2 3 3 4 3 5 5 6 6 7 7 8 8 9 3 10 3 11 3 12 3 13 3 14 14 15 15 16 16 17 17 18 18 19 19 20 3 21 3 22 22 23 23 24 24 25 3 26 3 27 3 28 3 29 3 30 30 31 31 32 32 33 33 34 34 35 35 36 23 37 37))
  "The generated action-tables.")

(defconst
  infix-notation-calculator-parser--distinct-action-tables
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (0 ((("
") reduce 1) (($) reduce 1) (("(") reduce 1) (("-") reduce 1) ((NUM) reduce 1)) 1 ((("
") shift) (($) accept) (("(") shift) (("-") shift) ((NUM) shift)) 2 ((("
") reduce 3) (($) reduce 3) (("(") reduce 3) (("-") reduce 3) ((NUM) reduce 3)) 3 ((("(") shift) (("-") shift) ((NUM) shift)) 5 ((("
") reduce 5) (("*") reduce 5) (("+") reduce 5) (("-") reduce 5) (("/") reduce 5) (("^") reduce 5)) 6 ((("
") shift) (("*") shift) (("+") shift) (("-") shift) (("/") shift) (("^") shift)) 7 ((("
") reduce 2) (($) reduce 2) (("(") reduce 2) (("-") reduce 2) ((NUM) reduce 2)) 8 ((("
") reduce 4) (($) reduce 4) (("(") reduce 4) (("-") reduce 4) ((NUM) reduce 4)) 14 ((("
") reduce 11) (("*") reduce 11) (("+") reduce 11) (("-") reduce 11) (("/") reduce 11) (("^") shift)) 15 ((("
") reduce 9) (("*") reduce 9) (("+") reduce 9) (("-") reduce 9) (("/") reduce 9) (("^") shift)) 16 ((("
") reduce 7) (("*") shift) (("+") reduce 7) (("-") reduce 7) (("/") shift) (("^") shift)) 17 ((("
") reduce 6) (("*") shift) (("+") reduce 6) (("-") reduce 6) (("/") shift) (("^") shift)) 18 ((("
") reduce 8) (("*") reduce 8) (("+") reduce 8) (("-") reduce 8) (("/") reduce 8) (("^") shift)) 19 ((("
") reduce 10) (("*") reduce 10) (("+") reduce 10) (("-") reduce 10) (("/") reduce 10) (("^") shift)) 22 (((")") reduce 5) (("*") reduce 5) (("+") reduce 5) (("-") reduce 5) (("/") reduce 5) (("^") reduce 5)) 23 (((")") shift) (("*") shift) (("+") shift) (("-") shift) (("/") shift) (("^") shift)) 24 ((("
") reduce 12) (("*") reduce 12) (("+") reduce 12) (("-") reduce 12) (("/") reduce 12) (("^") reduce 12)) 30 (((")") reduce 11) (("*") reduce 11) (("+") reduce 11) (("-") reduce 11) (("/") reduce 11) (("^") shift)) 31 (((")") reduce 9) (("*") reduce 9) (("+") reduce 9) (("-") reduce 9) (("/") reduce 9) (("^") shift)) 32 (((")") reduce 7) (("*") shift) (("+") reduce 7) (("-") reduce 7) (("/") shift) (("^") shift)) 33 (((")") reduce 6) (("*") shift) (("+") reduce 6) (("-") reduce 6) (("/") shift) (("^") shift)) 34 (((")") reduce 8) (("*") reduce 8) (("+") reduce 8) (("-") reduce 8) (("/") reduce 8) (("^") shift)) 35 (((")") reduce 10) (("*") reduce 10) (("+") reduce 10) (("-") reduce 10) (("/") reduce 10) (("^") shift)) 37 (((")") reduce 12) (("*") reduce 12) (("+") reduce 12) (("-") reduce 12) (("/") reduce 12) (("^") reduce 12))))
  "The generated distinct action-tables.")

(defconst
  infix-notation-calculator-parser--goto-tables
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (0 0 1 1 2 2 3 3 4 4 5 2 6 5 7 2 8 2 9 6 10 7 11 8 12 9 13 10 14 11 15 11 16 11 17 11 18 11 19 11 20 12 21 13 22 2 23 14 24 2 25 15 26 16 27 17 28 18 29 19 30 20 31 20 32 20 33 20 34 20 35 20 36 21 37 2))
  "The generated goto-tables.")

(defconst
  infix-notation-calculator-parser--distinct-goto-tables
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (0 ((input 1)) 1 (("
" 2) ("(" 3) ("-" 4) (NUM 5) (exp 6) (line 7)) 2 nil 3 (("(" 20) ("-" 21) (NUM 22) (exp 23)) 4 (("(" 3) ("-" 4) (NUM 5) (exp 19)) 5 (("
" 8) ("*" 9) ("+" 10) ("-" 11) ("/" 12) ("^" 13)) 6 (("(" 3) ("-" 4) (NUM 5) (exp 18)) 7 (("(" 3) ("-" 4) (NUM 5) (exp 17)) 8 (("(" 3) ("-" 4) (NUM 5) (exp 16)) 9 (("(" 3) ("-" 4) (NUM 5) (exp 15)) 10 (("(" 3) ("-" 4) (NUM 5) (exp 14)) 11 (("*" 9) ("+" 10) ("-" 11) ("/" 12) ("^" 13)) 12 (("(" 20) ("-" 21) (NUM 22) (exp 36)) 13 (("(" 20) ("-" 21) (NUM 22) (exp 35)) 14 ((")" 24) ("*" 25) ("+" 26) ("-" 27) ("/" 28) ("^" 29)) 15 (("(" 20) ("-" 21) (NUM 22) (exp 34)) 16 (("(" 20) ("-" 21) (NUM 22) (exp 33)) 17 (("(" 20) ("-" 21) (NUM 22) (exp 32)) 18 (("(" 20) ("-" 21) (NUM 22) (exp 31)) 19 (("(" 20) ("-" 21) (NUM 22) (exp 30)) 20 (("*" 25) ("+" 26) ("-" 27) ("/" 28) ("^" 29)) 21 ((")" 37) ("*" 25) ("+" 26) ("-" 27) ("/" 28) ("^" 29))))
  "The generated distinct goto-tables.")

(defconst
  infix-notation-calculator-parser--table-productions-number-reverse
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (0 ((start) (input)) 1 ((input) (%empty)) 2 ((input) (input line)) 3 ((line) ("
")) 4 ((line) (exp "
")) 5 ((exp) (NUM)) 6 ((exp) (exp "+" exp)) 7 ((exp) (exp "-" exp)) 8 ((exp) (exp "*" exp)) 9 ((exp) (exp "/" exp)) 10 ((exp) ("-" exp)) 11 ((exp) (exp "^" exp)) 12 ((exp) ("(" exp ")"))))
  "The hash-table indexed by production-number and value is production.")

(defconst
  infix-notation-calculator-parser--table-look-aheads
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (("
") t ($) t ("(") t (")") t ("*") t ("+") t ("-") t ("/") t (NUM) t ("^") t))
  "The hash-table of valid look-aheads.")

(defconst
  infix-notation-calculator-parser--table-terminal-p
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("+" t "-" t "*" t "/" t "^" t "(" t ")" t "
" t NUM t))
  "The hash-table of valid terminals.")

(defconst
  infix-notation-calculator-parser--table-non-terminal-p
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (start t input t line t exp t))
  "The hash-table of valid non-terminals.")

(defconst
  infix-notation-calculator-parser--table-translations
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (2 (lambda (args) (nth 1 args)) 4 (lambda (args) (nth 0 args)) 6 (lambda (args) (+ (float (nth 0 args)) (nth 2 args))) 7 (lambda (args) (- (float (nth 0 args)) (nth 2 args))) 8 (lambda (args) (* (float (nth 0 args)) (nth 2 args))) 9 (lambda (args) (/ (float (nth 0 args)) (nth 2 args))) 10 (lambda (args) (- (float (nth 1 args)))) 11 (lambda (args) (expt (float (nth 0 args)) (nth 2 args))) 12 (lambda (args) (nth 1 args))))
  "The hash-table of translations.")

(defconst
  infix-notation-calculator-parser-lex-analyzer--get-function
  (lambda (token) (save-current-buffer (set-buffer "*infix-notation-calculator-buffer*") (let ((start (car (cdr token))) (end (cdr (cdr token)))) (if (<= end (point-max)) (progn (let ((symbol (buffer-substring-no-properties start end))) (if (string-match-p "^\\([0-9]+\\.[0-9]+\\|[0-9]+\\)$" symbol) (progn (setq symbol (string-to-number symbol)))) symbol))))))
  "The lex-analyzer get function.")

(defconst
  infix-notation-calculator-parser-lex-analyzer--function
  (lambda (index) (save-current-buffer (set-buffer "*infix-notation-calculator-buffer*") (let ((token)) (if (< index (point-max)) (progn (goto-char index) (if (looking-at-p "[	 ]+") (progn (if (search-forward-regexp "[^	 ]" nil t) (progn (forward-char -1))))) (cond ((looking-at "\\([0-9]+\\.[0-9]+\\|[0-9]+\\)") (setq token (cons 'NUM (cons (match-beginning 0) (match-end 0))))) ((looking-at "\\(\\+\\|-\\|*\\|/\\|\\^\\|)\\|(\\|
\\)") (let ((symbol (buffer-substring-no-properties (match-beginning 0) (match-end 0)))) (setq token (cons symbol (cons (match-beginning 0) (match-end 0)))))) (t (error "Unexpected input at %d!" index))))) token)))
  "The lex-analyzer function.")

(defconst
  infix-notation-calculator-parser-lex-analyzer--reset-function
  nil
  "The lex-analyzer reset function.")

(defconst
  infix-notation-calculator-parser--e-identifier
  '%empty
  "The e-identifier.")

(defconst
  infix-notation-calculator-parser--eof-identifier
  '$
  "The end-of-file-identifier.")

(defconst
  infix-notation-calculator-parser--look-ahead-number
  1
  "The look-ahead number.")


;;; Variables:

(defvar-local
  infix-notation-calculator-parser-lex-analyzer--index
  0
  "The current index of the lex-analyzer.")


;;; Functions:


;;; Lex-Analyzer:

(defun
  infix-notation-calculator-parser-lex-analyzer--get-function (token)
  "Get information about TOKEN."
  (let ((meta-information))
    (condition-case
      error
      (progn
        (setq
          meta-information
          (funcall
            infix-notation-calculator-parser-lex-analyzer--get-function
            token)))
      (error (error
        "Lex-analyze failed to get token meta-data of %s, error: %s"
        token
        (car (cdr error)))))
    (unless meta-information
      (error "Could not find any token meta-information for: %s" token))
    meta-information))

(defun
  infix-notation-calculator-parser-lex-analyzer--reset
  ()
  "Reset Lex-Analyzer."
  (setq
    infix-notation-calculator-parser-lex-analyzer--index
    1)
  (when
    infix-notation-calculator-parser-lex-analyzer--reset-function
    (funcall
      infix-notation-calculator-parser-lex-analyzer--reset-function)))

(defun
  infix-notation-calculator-parser-lex-analyzer--peek-next-look-ahead
  ()
  "Peek next look-ahead number of tokens via lex-analyzer."
  (let ((look-ahead)
        (look-ahead-length 0)
        (index infix-notation-calculator-parser-lex-analyzer--index)
        (k (max
            1
            infix-notation-calculator-parser--look-ahead-number)))
    (while (<
            look-ahead-length
            k)
      (condition-case error
          (progn
            (let ((next-look-ahead
                   (funcall
                    infix-notation-calculator-parser-lex-analyzer--function
                    index)))
              (if next-look-ahead
                  (progn
                    (unless (listp (car next-look-ahead))
                      (setq next-look-ahead (list next-look-ahead)))
                    (dolist (next-look-ahead-item next-look-ahead)
                      (when (<
                             look-ahead-length
                             k)
                        (push next-look-ahead-item look-ahead)
                        (setq look-ahead-length (1+ look-ahead-length))
                        (setq index (cdr (cdr next-look-ahead-item))))))
                (push (list infix-notation-calculator-parser--eof-identifier) look-ahead)
                (setq look-ahead-length (1+ look-ahead-length))
                (setq index (1+ index)))))
        (error
         (error
          "Lex-analyze failed to peek next look-ahead at %s, error: %s"
          index
          error))))
    (nreverse look-ahead)))

(defun
  infix-notation-calculator-parser-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (let ((iteration 0)
        (tokens))
    (while (< iteration 1)
      (condition-case error
          (progn
            (let ((token
                   (funcall
                    infix-notation-calculator-parser-lex-analyzer--function
                    infix-notation-calculator-parser-lex-analyzer--index)))
              (when token
                (unless (listp (car token))
                  (setq token (list token)))
                (let ((first-token (car token)))
                  (setq
                   infix-notation-calculator-parser-lex-analyzer--index
                   (cdr (cdr first-token)))
                  (push first-token tokens)))))
        (error (error
                "Lex-analyze failed to pop token at %s, error: %s"
                infix-notation-calculator-parser-lex-analyzer--index
                (car (cdr error)))))
      (setq iteration (1+ iteration)))
    (nreverse tokens)))


;;; Syntax-Analyzer / Parser:


(defun
  infix-notation-calculator-parser--get-grammar-production-by-number
  (production-number)
  "If PRODUCTION-NUMBER exist, return it's production."
  (gethash
   production-number
   infix-notation-calculator-parser--table-productions-number-reverse))

(defun
  infix-notation-calculator-parser--valid-symbol-p
  (symbol)
  "Return whether SYMBOL is valid or not."
  (let ((is-valid t))
    (unless (or
             (infix-notation-calculator-parser--valid-e-p symbol)
             (infix-notation-calculator-parser--valid-eof-p symbol)
             (infix-notation-calculator-parser--valid-non-terminal-p symbol)
             (infix-notation-calculator-parser--valid-terminal-p symbol))
      (setq is-valid nil))
    is-valid))

(defun
  infix-notation-calculator-parser--valid-e-p
  (symbol)
  "Return whether SYMBOL is the e identifier or not."
  (eq
   symbol
   infix-notation-calculator-parser--e-identifier))

(defun
  infix-notation-calculator-parser--valid-eof-p
  (symbol)
  "Return whether SYMBOL is the EOF identifier or not."
  (eq
    symbol
    infix-notation-calculator-parser--eof-identifier))

(defun
  infix-notation-calculator-parser--valid-non-terminal-p (symbol)
  "Return whether SYMBOL is a non-terminal in grammar or not."
  (gethash
   symbol
   infix-notation-calculator-parser--table-non-terminal-p))

(defun
  infix-notation-calculator-parser--valid-terminal-p (symbol)
  "Return whether SYMBOL is a terminal in grammar or not."
  (gethash
   symbol
   infix-notation-calculator-parser--table-terminal-p))

(defun
  infix-notation-calculator-parser--get-grammar-translation-by-number
  (production-number)
  "If translation for PRODUCTION-NUMBER exist, return it."
  (gethash
    production-number
    infix-notation-calculator-parser--table-translations))

(defun
  infix-notation-calculator-parser--parse
  (&optional
    input-tape-index
    pushdown-list
    output
    translation
    translation-symbol-table-list
    history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION, TRANSLATION-SYMBOL-TABLE-LIST and HISTORY."
  (unless input-tape-index
    (setq input-tape-index 1))
  (unless pushdown-list
    (push 0 pushdown-list))
  (let ((translation-symbol-table
         (make-hash-table :test 'equal)))
    (when translation-symbol-table-list
      (dolist
          (item translation-symbol-table-list)
        (puthash
         (nth 0 item)
         (nth 1 item)
         translation-symbol-table)))

    (if (and
         input-tape-index
         (> input-tape-index 1))
        (setq
         infix-notation-calculator-parser-lex-analyzer--index
         input-tape-index)
      (infix-notation-calculator-parser-lex-analyzer--reset))

    ;; Make sure tables exists
    (unless infix-notation-calculator-parser--action-tables
      (error "Missing action-tables for grammar!"))
    (unless infix-notation-calculator-parser--distinct-action-tables
      (error "Missing distinct GOTO-tables for grammar!"))
    (unless infix-notation-calculator-parser--goto-tables
      (error "Missing GOTO-tables for grammar!"))
    (unless infix-notation-calculator-parser--distinct-goto-tables
      (error "Missing distinct GOTO-tables for grammar!"))

    (let ((accept)
          (pre-index 0))
      (while (not accept)

        ;; Save history when index has changed to enable incremental parsing / translating
        (when
            (>
             infix-notation-calculator-parser-lex-analyzer--index
             pre-index)
          ;; We make a copy of the hash-table here to avoid passing same
          ;; hash-table every-time with pointer
          (let ((translation-symbol-table-list))
            (maphash
             (lambda (key value)
               (push
                `(,key ,value)
                translation-symbol-table-list))
             translation-symbol-table)
            (push
             `(,infix-notation-calculator-parser-lex-analyzer--index
               ,pushdown-list
               ,output
               ,translation
               ,translation-symbol-table-list)
             history)
            (setq
             pre-index
             infix-notation-calculator-parser-lex-analyzer--index)))

        ;; (1) The look-ahead string u, consisting of the next k input symbols, is determined.
        (let ((look-ahead
               (infix-notation-calculator-parser-lex-analyzer--peek-next-look-ahead))
              (look-ahead-full))

          ;; Save token stream indexes in separate variable if needed later
          (setq look-ahead-full look-ahead)

          ;; Create simplified look-ahead for logic below
          (setq look-ahead nil)
          (dolist (look-ahead-item look-ahead-full)
            (if (listp look-ahead-item)
                (push (car look-ahead-item) look-ahead)
              (push look-ahead-item look-ahead)))
          (setq look-ahead (nreverse look-ahead))

          (let ((table-index
                 (car pushdown-list)))
            (let ((action-table-distinct-index
                   (gethash
                    table-index
                    infix-notation-calculator-parser--action-tables)))
              (let ((action-table
                     (gethash
                      action-table-distinct-index
                      infix-notation-calculator-parser--distinct-action-tables)))
              (unless action-table
                (error
                 "Action-table with index %s is empty! Push-down-list: %s"
                 table-index
                 pushdown-list))
              (let ((action-match nil)
                    (action-table-length (length action-table))
                    (action-index 0)
                    (possible-look-aheads))

                ;; (2) The parsing action f of the table on top of the pushdown list is applied to the lookahead string u.
                (while (and
                        (not action-match)
                        (< action-index action-table-length))
                  (let ((action (nth action-index action-table)))
                    (let ((action-look-ahead (car action)))
                      (push
                       action-look-ahead
                       possible-look-aheads)
                      (when
                          (equal
                           action-look-ahead
                           look-ahead)
                        (setq
                         action-match
                         (cdr action)))
                      (when
                          (and
                           (=
                            infix-notation-calculator-parser--look-ahead-number
                            0)
                           (not
                            action-look-ahead))
                        ;; LR(0) reduce actions occupy entire row
                        ;; and is applied regardless of look-ahead
                        (setq
                         action-match
                         (cdr action))))
                    (setq
                     action-index
                     (1+ action-index))))

                (unless action-match
                  ;; (c) If f(u) = error, we halt parsing (and, in practice
                  ;; transfer to an error recovery routine).
                  (error
                   (format
                    "Invalid syntax! Expected one of %s found %s at %s"
                    possible-look-aheads
                    look-ahead
                    infix-notation-calculator-parser-lex-analyzer--index)
                   possible-look-aheads
                   look-ahead
                   infix-notation-calculator-parser-lex-analyzer--index))

                (cond

                 ((equal action-match '(shift))
                  ;; (a) If f(u) = shift, then the next input symbol, say a
                  ;; is removed from the input and shifted onto the pushdown list.
                  ;; The goto function g of the table on top of the pushdown list
                  ;; is applied to a to determine the new table to be placed on
                  ;; top of the pushdown list. We then return to step(1). If
                  ;; there is no next input symbol or g(a) is undefined, halt
                  ;; and declare error.

                  (let ((a (list (car look-ahead)))
                        (a-full (list (car look-ahead-full))))
                      (let ((goto-table-distinct-index
                             (gethash
                              table-index
                              infix-notation-calculator-parser--goto-tables)))
                        (let ((goto-table
                               (gethash
                                goto-table-distinct-index
                                infix-notation-calculator-parser--distinct-goto-tables)))
                      (let ((goto-table-length (length goto-table))
                            (goto-index 0)
                            (searching-match t)
                            (next-index)
                            (possible-look-aheads))

                        (while (and
                                searching-match
                                (< goto-index goto-table-length))
                          (let ((goto-item (nth goto-index goto-table)))
                            (let ((goto-item-symbol (list (car goto-item)))
                                  (goto-item-next-index (car (cdr goto-item))))
                              (push goto-item-symbol possible-look-aheads)

                              (when (equal
                                     goto-item-symbol
                                     a)
                                (setq next-index goto-item-next-index)
                                (setq searching-match nil))))

                          (setq goto-index (1+ goto-index)))
                        (unless next-index
                          (error
                           "In shift, found no GOTO-item for %s at %s, expected one of %s"
                           a
                           infix-notation-calculator-parser-lex-analyzer--index
                           possible-look-aheads))

                        (push (car a-full) pushdown-list)
                        (push next-index pushdown-list)
                        (infix-notation-calculator-parser-lex-analyzer--pop-token))))))

                 ((equal (car action-match) 'reduce)
                  ;; (b) If f(u) = reduce i and production i is A -> a,
                  ;; then 2|a| symbols are removed from the top of the pushdown
                  ;; list, and production number i is placed in the output
                  ;; buffer. A new table T' is then exposed as the top table
                  ;; of the pushdown list, and the goto function of T' is applied
                  ;; to A to determine the next table to be placed on top of the
                  ;; pushdown list. We place A and this new table on top of the
                  ;; the pushdown list and return to step (1)

                  (let ((production-number (car (cdr action-match))))

                    (let ((production
                           (infix-notation-calculator-parser--get-grammar-production-by-number
                            production-number)))
                      (let ((production-lhs (car production))
                            (production-rhs (car (cdr production)))
                            (popped-items-contents))
                        (unless (equal
                                 production-rhs
                                 (list infix-notation-calculator-parser--e-identifier))
                          (let ((pop-items (* 2 (length production-rhs)))
                                (popped-items 0)
                                (popped-item))
                            (while (< popped-items pop-items)
                              (setq popped-item (pop pushdown-list))
                              (when (and
                                     (listp popped-item)
                                     (infix-notation-calculator-parser--valid-symbol-p
                                      (car popped-item)))
                                (push
                                 popped-item
                                 popped-items-contents))
                              (setq popped-items (1+ popped-items)))))
                        (push production-number output)

                        (let ((popped-items-meta-contents))
                          (setq
                           popped-items-contents
                           (reverse popped-items-contents))
                          ;; Collect arguments for translation
                          (dolist (popped-item popped-items-contents)
                            (if (and
                                 (listp popped-item)
                                 (cdr popped-item))
                                ;; If item is a terminal, use it's literal value
                                (push
                                 (infix-notation-calculator-parser-lex-analyzer--get-function
                                  popped-item)
                                 popped-items-meta-contents)

                              ;; If item is a non-terminal
                              (let ((temp-hash-key
                                     (format
                                      "%S"
                                       popped-item)))

                              ;; If we have a translation for symbol, pop one
                              ;; otherwise push nil on translation argument stack
                              (if (gethash
                                       temp-hash-key
                                       translation-symbol-table)
                                      (let ((symbol-translations
                                             (gethash
                                              temp-hash-key
                                              translation-symbol-table)))
                                        (let ((symbol-translation
                                               (pop symbol-translations)))
                                          (push
                                           symbol-translation
                                           popped-items-meta-contents)
                                          (puthash
                                           temp-hash-key
                                           symbol-translations
                                           translation-symbol-table)))
                                    (push
                                     nil
                                     popped-items-meta-contents)))))

                            ;; If we just have one argument, pass it as a instead of a list
                            (when (= (length popped-items-meta-contents) 1)
                              (setq
                               popped-items-meta-contents
                               (car popped-items-meta-contents)))

                            ;; Perform translation at reduction if specified
                            (if
                                (infix-notation-calculator-parser--get-grammar-translation-by-number
                                 production-number)
                                (let ((partial-translation
                                       (funcall
                                        (infix-notation-calculator-parser--get-grammar-translation-by-number
                                         production-number)
                                        popped-items-meta-contents)))
                                  (let ((temp-hash-key
                                         (format
                                          "%S"
                                          production-lhs)))
                                    (let ((symbol-translations
                                           (gethash
                                            temp-hash-key
                                            translation-symbol-table)))
                                      (push
                                       partial-translation
                                       symbol-translations)
                                      (puthash
                                       temp-hash-key
                                       symbol-translations
                                       translation-symbol-table)
                                      (setq
                                       translation
                                       partial-translation))))

                              ;; When no translation is specified just use popped contents as translation
                              (let ((partial-translation
                                     popped-items-meta-contents))
                                (let ((temp-hash-key
                                       (format
                                        "%S"
                                        production-lhs)))
                                  (let ((symbol-translations
                                         (gethash
                                          temp-hash-key
                                          translation-symbol-table)))
                                    (push
                                     partial-translation
                                     symbol-translations)
                                    (puthash
                                     temp-hash-key
                                     symbol-translations
                                     translation-symbol-table)
                                    (setq
                                     translation
                                     partial-translation))))))

                          (let ((new-table-index (car pushdown-list)))
                            (let ((goto-table-distinct-index
                                   (gethash
                                    new-table-index
                                    infix-notation-calculator-parser--goto-tables)))
                              (let ((goto-table
                                     (gethash
                                      goto-table-distinct-index
                                      infix-notation-calculator-parser--distinct-goto-tables)))
                                (let ((goto-table-length
                                       (length goto-table))
                                      (goto-index 0)
                                      (searching-match t)
                                      (next-index))

                                  (while (and
                                          searching-match
                                          (< goto-index goto-table-length))
                                    (let ((goto-item (nth goto-index goto-table)))
                                      (let ((goto-item-symbol (list (car goto-item)))
                                            (goto-item-next-index (car (cdr goto-item))))

                                        (when (equal
                                               goto-item-symbol
                                               production-lhs)
                                          (setq next-index goto-item-next-index)
                                          (setq searching-match nil))))

                                    (setq goto-index (1+ goto-index)))

                                  (when next-index
                                    (push production-lhs pushdown-list)
                                    (push next-index pushdown-list))))))))))

                   ((equal action-match '(accept))
                    ;;    (d) If f(u) = accept, we halt and declare the string
                    ;;    in the output buffer to be the right parse of the original
                    ;;    input string.

                    (setq accept t))
                   (t (error
                       "Invalid action-match: %s!"
                       action-match)))))))))
      (unless accept
        (error
         "Parsed entire string without getting accepting! Output: %s"
         (reverse output)))
      (when history
        (setq history (reverse history)))
      (when output
        (setq output (reverse output)))
      (let ((translation-symbol-table-list))
        (when translation-symbol-table
          (maphash
           (lambda (key value)
             (push
              `(,key ,value)
              translation-symbol-table-list))
           translation-symbol-table))
        (list
         output
         translation
         translation-symbol-table-list
         history)))))
(defun infix-notation-calculator-parser-parse
    (&optional
     input-tape-index
     pushdown-list
     output
     translation
     history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION and HISTORY."
  (let ((result
         (infix-notation-calculator-parser--parse
          input-tape-index
          pushdown-list
          output
          translation
          history)))
    (nth 0 result)))

(defun infix-notation-calculator-parser-translate
    (&optional
     input-tape-index
     pushdown-list
     output
     translation
     history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION and HISTORY."
  (let ((result
         (infix-notation-calculator-parser--parse
          input-tape-index
          pushdown-list
          output
          translation
          history)))
    (nth 1 result)))

(provide 'infix-notation-calculator-parser)

;;; infix-notation-calculator-parser.el ends here
