EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L . -L test/

EL  := infix-notation-calculator.el infix-notation-calculator-parser.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -f batch-byte-compile $(EL)

.PHONY: test
test:
	$(EMACS_CMD) -l test/infix-notation-calculator-test.el -f "infix-notation-calculator-test"

.PHONY: admin
admin:
	$(EMACS_CMD) -L ../emacs-parser/ -l admin/infix-notation-calculator-admin.el -eval "(progn (require 'parser-generator-lr-export)(infix-notation-calculator-admin))"
