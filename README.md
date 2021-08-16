# Infix Notation Calculator - Emacs plugin

Calculate selected region, use the calculator mode or calculate text in the mini-buffer. All calculations are stored in history buffer.

## Generate parser

Run `make admin`.

## Run tests

Run `make test`.

## Activate calculator mode

Run `(infix-notation-calculator-mode)`.

Press `C-return` to calculate the line you are at.

## Calculate selected region

Run `(infix-notation-calculator-on-region)`.

## Calculate text in mini-buffer

Run `(infix-notation-calculator-on-minibuffer)`.

## See log of calculations

Just visit the buffer `*InfixCalc History*`.

## Translate any string (Advanced)

Just call the function `(infix-notation-calculator--translate-string)`.
