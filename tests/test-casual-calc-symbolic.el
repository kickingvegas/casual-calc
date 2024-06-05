;;; test-casual-calc-symbolic.el --- Casual Symbolic Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-test-utils)
(require 'casual-calc-symbolic)

(ert-deftest test-casual-calc-symbolic-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("Eq" . casual-calc--calc-alg-evaluate)
                        ("=q" . casual-calc--calc-evaluate)
                        ("m" . casual-calc-symbolic-manipulation-tmenu)
                        ("F" . casual-calc-subformula-tmenu)
                        ("fq" . casual-calc--calc-factor)
                        ("eq" . casual-calc--calc-expand)
                        ("p" . casual-calc-polynomial-tmenu)
                        ("dq" . casual-calc--calc-derivative)
                        ("iq" . casual-calc--calc-integral)
                        ("c" . casual-calc--calculus-tmenu)
                        ("s" . casual-calc-solve-symbolic-tmenu)
                        ("n" . casual-calc-solve-numeric-tmenu)
                        ("C" . casual-calc-curve-fit-tmenu)
                        ("S" . casual-calc-summations-tmenu)
                        ("l" . casual-calc-symbolic-logic-tmenu)
                        ("g" . casual-calc-plot-tmenu)
                        ;;("A" . calc-algebraic-mode)
                        ;;("M" . calc-symbolic-mode)
                        ("aq" . casual-calc-angle-measure-tmenu))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-calc-subformula-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc--calc-select-here)
                        ("oq" . casual-calc--calc-select-once)
                        ("mq" . casual-calc--calc-select-more)
                        ("pq" . casual-calc--calc-select-previous)
                        ("uq" . casual-calc--calc-unselect)
                        ("cq" . casual-calc--calc-clear-selections)
                        ("lq" . casual-calc--calc-select-less)
                        ("nq" . casual-calc--calc-select-next)
                        ("bq" . casual-calc--calc-commute-left)
                        ("dq" . casual-calc--calc-sel-distribute)
                        ("iq" . casual-calc--calc-sel-isolate)
                        ("Nq" . casual-calc--calc-sel-negate)
                        ("eq" . casual-calc--calc-sel-jump-equals)
                        ("fq" . casual-calc--calc-commute-right)
                        ("Mq" . casual-calc--calc-sel-merge)
                        ("&q" . casual-calc--calc-sel-invert)
                        ("=q" . casual-calc--calc-sel-evaluate)
                        ("`" . casual-calc--calc-edit-selection)
                        ("Cq" . casual-calc--calc-copy-selection)
                        ("'" . casual-calc--calc-enter-selection)
                        ("Dq" . casual-calc--calc-del-selection))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-subformula-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-symbolic-manipulation-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("Eq" . casual-calc--calc-alg-evaluate)
                        ("=q" . casual-calc--calc-evaluate)
                        ("eq" . casual-calc--calc-expand-formula)
                        ("mq" . casual-calc--calc-map-equation)
                        ("sq" . casual-calc--calc-substitute))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-manipulation-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-polynomial-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("fq" . casual-calc--calc-factor)
                        ("eq" . casual-calc--calc-expand)
                        ("cq" . casual-calc--calc-collect)
                        ("aq" . casual-calc--calc-apart)
                        ("nq" . casual-calc--calc-normalize-rat)
                        ("\\q" . casual-calc--calc-poly-div)
                        ("%q" . casual-calc--calc-poly-rem)
                        ("/q" . casual-calc--calc-poly-div-rem)
                        ("gq" . casual-calc--calc-poly-gcd)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-polynomial-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-calc--calculus-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("nq" . casual-calc--calc-num-integral)
                        ("tq" . casual-calc--calc-taylor)
                        ("dq" . casual-calc--calc-derivative)
                        ("iq" . casual-calc--calc-integral))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc--calculus-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-solve-symbolic-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc--calc-solve-for)
                        ("pq" . casual-calc--calc-poly-roots))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-solve-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-solve-numeric-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("rq" . casual-calc--calc-find-root)
                        ("mq" . casual-calc--calc-find-minimum)
                        ("xq" . casual-calc--calc-find-maximum)
                        ("hq" . casual-calc--calc-head)
                        ("wq" . casual-calc--calc-why))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-solve-numeric-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-curve-fit-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("cq" . casual-calc--calc-curve-fit)
                        ("pq" . casual-calc--calc-poly-interp))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-curve-fit-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-summations-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc--calc-summation)
                        ("aq" . casual-calc--calc-alt-summation)
                        ("pq" . casual-calc--calc-product)
                        ("tq" . casual-calc--calc-tabulate))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-summations-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-symbolic-logic-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("=q" . casual-calc--calc-equal-to)
                        ("lq" . casual-calc--calc-less-than)
                        ("gq" . casual-calc--calc-greater-than)
                        ("nq" . casual-calc--calc-not-equal-to)
                        ("Lq" . casual-calc--calc-less-equal)
                        ("Gq" . casual-calc--calc-greater-equal)
                        ("xq" . casual-calc--calc-remove-equal)
                        ("!q" . casual-calc--calc-logical-not)
                        ("&q" . casual-calc--calc-logical-and)
                        ("|q" . casual-calc--calc-logical-or)
                        ("eq" . casual-calc--calc-in-set)
                        ("iq" . casual-calc--calc-logical-if))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-logic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(provide 'test-casual-calc-symbolic)
