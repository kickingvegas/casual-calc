;;; test-casual-symbolic.el --- Casual Symbolic Tests  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'ert)
(require 'casual-test-utils)
(require 'casual-symbolic)

(ert-deftest test-casual-symbolic-menu ()
  (casualt-setup)
  (let ((test-vectors '(("Eq" . casual-calc-alg-evaluate)
                        ("=q" . casual-calc-evaluate)
                        ("m" . casual-symbolic-manipulation-menu)
                        ("F" . casual-subformula-menu)
                        ("fq" . casual-calc-factor)
                        ("eq" . casual-calc-expand)
                        ("p" . casual-polynomial-menu)
                        ("dq" . casual-calc-derivative)
                        ("iq" . casual-calc-integral)
                        ("c" . casual-calculus-menu)
                        ("s" . casual-solve-symbolic-menu)
                        ("n" . casual-solve-numeric-menu)
                        ("C" . casual-curve-fit-menu)
                        ("S" . casual-summations-menu)
                        ("l" . casual-symbolic-logic-menu)
                        ("g" . casual-plot-menu)
                        ;;("A" . calc-algebraic-mode)
                        ;;("M" . calc-symbolic-mode)
                        ("aq" . casual-angle-measure-menu))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-symbolic-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-subformula-menu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc-select-here)
                        ("oq" . casual-calc-select-once)
                        ("mq" . casual-calc-select-more)
                        ("pq" . casual-calc-select-previous)
                        ("uq" . casual-calc-unselect)
                        ("cq" . casual-calc-clear-selections)
                        ("lq" . casual-calc-select-less)
                        ("nq" . casual-calc-select-next)
                        ("bq" . casual-calc-commute-left)
                        ("dq" . casual-calc-sel-distribute)
                        ("iq" . casual-calc-sel-isolate)
                        ("Nq" . casual-calc-sel-negate)
                        ("eq" . casual-calc-sel-jump-equals)
                        ("fq" . casual-calc-commute-right)
                        ("Mq" . casual-calc-sel-merge)
                        ("&q" . casual-calc-sel-invert)
                        ("=q" . casual-calc-sel-evaluate)
                        ("`" . casual-calc-edit-selection)
                        ("Cq" . casual-calc-copy-selection)
                        ("'" . casual-calc-enter-selection)
                        ("Dq" . casual-calc-del-selection))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-subformula-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-symbolic-manipulation-menu ()
  (casualt-setup)
  (let ((test-vectors '(("Eq" . casual-calc-alg-evaluate)
                        ("=q" . casual-calc-evaluate)
                        ("eq" . casual-calc-expand-formula)
                        ("mq" . casual-calc-map-equation)
                        ("sq" . casual-calc-substitute))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-symbolic-manipulation-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-polynomial-menu ()
  (casualt-setup)
  (let ((test-vectors '(("fq" . casual-calc-factor)
                        ("eq" . casual-calc-expand)
                        ("cq" . casual-calc-collect)
                        ("aq" . casual-calc-apart)
                        ("nq" . casual-calc-normalize-rat)
                        ("\\q" . casual-calc-poly-div)
                        ("%q" . casual-calc-poly-rem)
                        ("/q" . casual-calc-poly-div-rem)
                        ("gq" . casual-calc-poly-gcd)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-polynomial-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-calculus-menu ()
  (casualt-setup)
  (let ((test-vectors '(("nq" . casual-calc-num-integral)
                        ("tq" . casual-calc-taylor)
                        ("dq" . casual-calc-derivative)
                        ("iq" . casual-calc-integral))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calculus-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-solve-symbolic-menu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc-solve-for)
                        ("pq" . casual-calc-poly-roots))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-solve-symbolic-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-solve-numeric-menu ()
  (casualt-setup)
  (let ((test-vectors '(("rq" . casual-calc-find-root)
                        ("mq" . casual-calc-find-minimum)
                        ("xq" . casual-calc-find-maximum)
                        ("hq" . casual-calc-head)
                        ("wq" . casual-calc-why))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-solve-numeric-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-curve-fit-menu ()
  (casualt-setup)
  (let ((test-vectors '(("cq" . casual-calc-curve-fit)
                        ("pq" . casual-calc-poly-interp))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-curve-fit-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-summations-menu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc-summation)
                        ("aq" . casual-calc-alt-summation)
                        ("pq" . casual-calc-product)
                        ("tq" . casual-calc-tabulate))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-summations-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-symbolic-logic-menu ()
  (casualt-setup)
  (let ((test-vectors '(("=q" . casual-calc-equal-to)
                        ("lq" . casual-calc-less-than)
                        ("gq" . casual-calc-greater-than)
                        ("nq" . casual-calc-not-equal-to)
                        ("Lq" . casual-calc-less-equal)
                        ("Gq" . casual-calc-greater-equal)
                        ("xq" . casual-calc-remove-equal)
                        ("!q" . casual-calc-logical-not)
                        ("&q" . casual-calc-logical-and)
                        ("|q" . casual-calc-logical-or)
                        ("eq" . casual-calc-in-set)
                        ("iq" . casual-calc-logical-if))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-symbolic-logic-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(provide 'test-casual-symbolic)
