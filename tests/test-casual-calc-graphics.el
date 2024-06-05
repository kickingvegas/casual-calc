;;; test-casual-calc-graphics.el --- Casual Graphics Tests  -*- lexical-binding: t; -*-

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

;; Note that these are UI tests. Testing wrapped functions means effectively
;; testing Calc behavior which is out of scope for testing here.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-graphics)

(ert-deftest test-casual-calc--push-natural-interval-0-100 ()
  (casualt-setup)
  (casual-calc--push-natural-interval-0-100)
  (should (equal '(intv 3 0 100) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-natural-interval-0-360 ()
  (casualt-setup)
  (casual-calc--push-natural-interval-0-360)
  (should (equal '(intv 3 0 360) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-float-interval-0-100 ()
  (casualt-setup)
  (casual-calc--push-float-interval-0-100)
  (should (equal '(intv 3 (float 0 0) (float 1 2)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-float-interval-1-symmetric ()
  (casualt-setup)
  (casual-calc--push-float-interval-1-symmetric)
  (should (equal '(intv 3 (float -1 0) (float 1 0)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-sin ()
  (casualt-setup)
  (casual-calc--push-sin)
  (should (equal '(calcFunc-sin (var x var-x)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-cos ()
  (casualt-setup)
  (casual-calc--push-cos)
  (should (equal '(calcFunc-cos (var x var-x)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-tan ()
  (casualt-setup)
  (casual-calc--push-tan)
  (should (equal '(calcFunc-tan (var x var-x)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-ln ()
  (casualt-setup)
  (casual-calc--push-ln)
  (should (equal '(calcFunc-ln (var x var-x)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-e-raised-to-x ()
  (casualt-setup)
  (casual-calc--push-e-raised-to-x)
  (should (equal '(^ (var e var-e) (var x var-x)) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-polynomial-order-2 ()
  (casualt-setup)
  (casual-calc--push-polynomial-order-2)
  (should (equal '(+ (^ (var x var-x) 2) 1) (calc-top)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--push-polynomial-order-3 ()
  (casualt-setup)
  (casual-calc--push-polynomial-order-3)
  (should (equal '(+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1) (calc-top)))
  (casualt-breakdown t))


;; TODO: figure out how to mock functions called by Transient prefix.
;; At current, unknown how to write a test without exercising Gnuplot which is
;; _not_ desired.

;; (ert-deftest test-casual-calc-plot-tmenu ())
;; (ert-deftest test-casual-calc-plot-options-tmenu ())
;; (ert-deftest test-casual-calc-graph-examples-tmenu ())
;; (ert-deftest test-casual-calc-graph-settings-tmenu ())
;; (ert-deftest test-casual-calc-curve-style-tmenu ())

(provide 'test-casual-calc-graphics)
;;; test-casual-calc-graphics.el ends here
