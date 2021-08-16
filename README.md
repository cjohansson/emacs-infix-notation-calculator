# Infix Notation Calculator - Emacs plugin

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://api.travis-ci.com/cjohansson/emacs-infix-notation-calculator.svg?branch=master)](https://api.travis-ci.com/cjohansson/emacs-infix-notation-calculator.svg?branch=master)

Calculate selected region, use the calculator mode or calculate text in the mini-buffer. All calculations are stored in history buffer.

## Generate parser

Run `make admin` from terminal.

## Run tests

Run `make test` from terminal.

## Activate calculator mode

Call `infix-notation-calculator-mode`.

In this mode press `C-return` to calculate the line you are at.

## Calculate selected region

Call `(infix-notation-calculator-on-region)`.

## Calculate text in mini-buffer

Call `(infix-notation-calculator-on-minibuffer)`.

## See log of calculations

Just visit the buffer `*InfixCalc History*`.

## Translate any string (Advanced)

Just call the function `(infix-notation-calculator--translate-string)`.
