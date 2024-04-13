;;; test-casual.el --- Tests for casual  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test suite for Casual package.

;; Note that tests are focused on verifying UI behavior and not intended to test
;; the `calc' package.

;; * Running the test suite

;; ** Command Line
;; Casual uses `ert' to manage its test suite. The test suite is intended to be
;; run in batch mode via a Makefile in the same directory as this file.
;; Invoke the following command in a shell to run the test suite.

;;     $ make tests

;; ** Interactive
;; Tests can be interactively run via `ert' by loading the following two files:
;; - casual-test-utils.el
;; - test-casual.el

;; Refer to `ert' documentation for more detail on how use it.

;;; Code:
(require 'ert)
(require 'casual)
(require 'casual-test-utils)

;;; Tests

(ert-deftest test-casual-main-menu ()
  (casualt-setup)
  (let ((test-vectors '(("&" . casual-calc-inv)
                        ("Q" . casual-calc-sqrt)
                        ("n" . casual-calc-change-sign)
                        ("^" . casual-calc-power)
                        ("=" . casual-calc-evaluate)
                        ("A" . casual-calc-abs)
                        ("!" . casual-calc-factorial)
                        ("%" . casual-calc-percent)
                        ("D" . casual-calc-percent-change)
                        ("p" . casual-calc-pi)
                        ("e" . casual--e-constant)
                        ("m" . casual-modes-menu)
                        ("ó" . casual-stack-display-menu)
                        ("ô" . casual-trail-menu)
                        ("o" . casual-rounding-menu)
                        ("c" . casual-conversions-menu)
                        ("T" . casual-time-menu)
                        ("i" . casual-complex-number-menu)
                        ("R" . casual-random-number-menu)
                        ("t" . casual-trig-menu)
                        ("l" . casual-logarithmic-menu)
                        ("b" . casual-binary-menu)
                        ("v" . casual-vector-menu)
                        ("u" . casual-units-menu)
                        ("f" . casual-financial-menu)
                        ("g" . casual-plot-menu)
                        ("s" . casual--stack-swap)
                        ("r" . casual--stack-roll-all)
                        ("d" . casual--stack-drop)
                        ("C" . casual--stack-clear)
                        ("L" . casual--stack-last)
                        ("w" . casual-calc-copy-as-kill)
                        ("z" . casual-variable-crud-menu))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-main-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-main-menu-last ()
  (casualt-setup)
  (calc-push-list '(2 3))
  (funcall 'casual-main-menu)
  (execute-kbd-macro "^")
  (funcall 'casual-main-menu)
  (execute-kbd-macro "L")
  (should (and (= (calc-top) 3)
               (= (calc-top-n 2) 2)))
  (casualt-breakdown t))

(provide 'test-casual)
;;; test-casual.el ends here
