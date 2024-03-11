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

;; Casual uses `ert' to manage its test suite. The test suite is intended to be
;; run in batch mode via a Makefile in the same directory as this file.
;; Invoke the following command in a shell to run the test suite.

;;     $ make tests

;; Tests can be interactively run via `ert' by loading this file test-casual.el
;; via the `load-file' command. Refer to `ert' documentation for more detail on
;; how use it.

;;; Code:

(require 'ert)
(require 'casual)

(defun casualt-setup ()
  "Casual menu test setup function."
  (calc-create-buffer))

(defun casualt-breakdown (&optional clear)
  "Casual menu test breakdown function, if CLEAR is non-nil then clear stack."
  (if clear
      (calc-pop-stack (calc-stack-size)))
  (calc-quit nil))

(defun casualt-menu-input-testcase (menu keyseq init value)
  "Testcase that compares VALUE with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
VALUE - value to be compared with top of the stack"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (should (equal (calc-top) value)))

(defun casualt-menu-assert-testcase (menu keyseq init assert)
  "Testcase to call ASSERT with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
ASSERT - assert function that calls `should'"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (funcall assert))

(defun casualt-run-menu-input-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instancesg\n
A testcase is a list with the following schema - (keyseq init value)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
value - value to be compared with top of the stack\n
A testcase is used as input to `casualt-menu-input-testcase'."
  (mapc (lambda (x)
          (casualt-menu-input-testcase menu
                                       (car x)
                                       (nth 1 x)
                                       (nth 2 x)))
        testcases))

(defun casualt-run-menu-assert-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instances\n
A testcase is a list with the following schema - (keyseq init assert)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
assert - assert function that calls `should'\n
A testcase is used as input to `casualt-menu-assert-testcase'."
  (mapc (lambda (x)
          (casualt-menu-assert-testcase menu
                                        (car x)
                                        (nth 1 x)
                                        (nth 2 x)))
        testcases))

;;; Tests

(ert-deftest test-casual-main-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-main-menu
   '(("&" (2) (float 5 -1))
     ("Q" (9) 3)
     ("n" (5) -5)
     ("^" (2 3) 8)
     ("A" (-10) 10)
     ("!" (7) 5040)
     ("%" (8) (calcFunc-percent 8))
     ("d" (100 20) (calcFunc-percent -80))
     ("p" nil (float 314159265359 -11))
     ("e" nil (float 271828182846 -11))))
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

(ert-deftest test-casual-rounding-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-rounding-menu
   '(("r" ((float 27 -1)) 3)
     ("r" ((float 22 -1)) 2)
     ("f" ((float 25 -1)) 2)
     ("c" ((float 25 -1)) 3)
     ("t" ((float 25 -1)) 2)))
  (casualt-breakdown t))

(ert-deftest test-casual-conversions-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-conversions-menu
   '(("d" ((float 785398163397 -12)) (float 45 0))
     ("r" ((float 45 0)) (float 785398163397 -12))
     ("h" ((float 150833333333 -11)) (hms 1 30 (float 3 1)))
     ("f" ((float 15 -1)) (frac 3 2))
     ("F" ((frac 3 2)) (float 15 -1))
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-time-menu ()
  (casualt-setup)
  (casualt-run-menu-assert-testcases
   'casual-time-menu
   '(("n" () (lambda () (should (math-floatp (calc-top)))))))

  (casualt-run-menu-input-testcases
   'casual-time-menu
   '(("i" ((date (float 738944382662 -6))) (date (float 738973382662 -6)))
     ("u" ((date (float 738973382662 -6))) 1711642262)
     ("+" ((date (float 738973382662 -6)) 14) (date (float 738993382662 -6)))
     ("-" ((date (float 738973382662 -6)) 14) (date (float 738953382662 -6)))))

  (casualt-breakdown t))

(ert-deftest test-casual-first-day-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-first-day-menu
   '(("w" ((date (float 738953382662 -6))) (date 738948))
     ("m" ((date (float 738953382662 -6))) (date 738946))
     ("y" ((date (float 738953382662 -6))) (date 738886))))
  (casualt-breakdown t))

(ert-deftest test-casual-complex-number-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-complex-number-menu
   '(("r" ((cplx 2 3)) 2)
     ("i" ((cplx 2 3)) 3)
     ("c" ((cplx 2 3)) (cplx 2 -3))
     ("a" ((cplx 2 3)) (hms 56 18 (float 35756906 -6)))))
  (casualt-breakdown t))


(ert-deftest test-casual-random-number-menu ()
  (casualt-setup)
  (casualt-run-menu-assert-testcases
   'casual-random-number-menu
   '(
     ;; default m=10
     ("r" ()
      (lambda ()
        (message (format "m=10, n: %d" (calc-top)))
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (< (calc-top) 10)))))
     ("mr" ((intv 3 0 100))
      (lambda ()
        (message (format "stack [0..100], n: %d" (calc-top)))
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))
     ("a" ()
      (lambda ()
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))
     ("a" ()
      (lambda ()
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))

     ("c" ()
      (lambda () ; TODO: Figure out bounds check for xfloat
        (should (math-floatp (calc-top)))))))
  (casualt-breakdown t))

(ert-deftest test-casual-variable-crud-menu ()
  (casualt-setup)

  (calc-push-list '(25))
  (funcall 'casual-variable-crud-menu)
  ;; test (s) calc-store
  (execute-kbd-macro "sfoo")
  (should (= (calc-var-value 'var-foo) 25))
  (calc-pop-stack (calc-stack-size))

  ;; test (r) calc-recall
  (execute-kbd-macro "rfoo")
  (should (= (calc-top) 25))

  ;; test (o) calc-copy-variable
  (execute-kbd-macro "ofoojane")
  (should (= (calc-var-value 'var-jane) 25))

  ;; test (c) calc-unstore
  (execute-kbd-macro "cfoo")
  (should (not (calc-var-value 'var-foo)))

  ;; test (x) calc-store-exchange
  (calc-push-list '(32))
  (execute-kbd-macro "xjane")
  (should (= (calc-var-value 'var-jane) 32))

  ;; TODO: punting on calc-edit-variable
  ;; TODO: punting on calc-permanent-variable
  ;; TODO: punting on calc-insert-variables
  (casualt-breakdown t))


(ert-deftest test-casual-binary-menu ()
  (casualt-setup)
  (calc-word-size 8)
  (casualt-run-menu-input-testcases
   'casual-binary-menu
   '(("a" (11 12) 8)
     ("o" (11 12) 15)
     ("x" (11 12) 7)
     ("-" (11 12) 3)
     ("!" (11) 244)
     ("l" (12) 24)
     ("r" (24) 12)
     ([134217836] (12) 24) ; (read-kbd-macro "M-l") evals to [134217836]
     ([134217842] (24) 12) ; (read-kbd-macro "M-r") evals to [134217842]
     ([18] (24) 48) ; (read-kbd-macro "C-r") evals to [18]
     ))
  (calc-word-size 32)
  (casualt-breakdown t))

(ert-deftest test-casual-vector-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-vector-menu
   '(
     ("b|" (2 3) (vec 2 3))
     ;; TODO: define remaining testcases.
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-vector-menu-extract ()
  (casualt-setup)
  (calc-push-list '((vec (vec 1 4) (vec 2 3))))
  (funcall 'casual-vector-menu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) '(vec 2 3)))

  (calc-push-list '((vec (vec 9 2) (vec 7 14))))
  (funcall 'casual-vector-menu)
  (execute-kbd-macro "c2")
  (should (equal (calc-top) '(vec 2 14)))

  (calc-push-list '((vec 7 9)))
  (funcall 'casual-vector-menu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) 9))

  (calc-push-list '((vec 2 3)))
  (funcall 'casual-vector-menu)
  (execute-kbd-macro "c1")
  (should (= (calc-top) 2))
  (casualt-breakdown t))

(ert-deftest test-casual-vector-building-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-vector-building-menu
   '(
     ;; Build
     ("|" (2 3) (vec 2 3))
     ;; TODO: "i" requires handling Emacs prefix convention to override prompt.
     ("e" ((intv 3 0 5)) (vec 0 1 2 3 4 5))
     ;; TODO: "I" requires handling Emacs prefix convention to override prompt.
     ;; TODO: "d" requires handling Emacs prefix convention to override prompt.
     ;; TODO: "b" requires handling Emacs prefix convention to override prompt.

     ;; Transpose
     ("t" ((vec 1 2)) (vec (vec 1) (vec 2)))
     ("r" ((vec 1 2 3)) (vec 3 2 1))
     ;; TODO: "a"
     ("s" ((vec 2 1 3)) (vec 1 2 3))
     ("p" ((vec 2 1 3 3)) (vec 1 2 3))

     ;; Miscellaneous
     ("l" ((vec 2 1 3 3)) 4)
     ("c" ((vec 2 1 3 3)) 4)
     ("f" ((vec 2 1 3 5) 3) 3)
     ;; TODO: "h"
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-vector-arithmetic-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-vector-arithmetic-menu
   '(
     ("t" ((vec 2 3)) (vec (vec 2) (vec 3)))
     ("A" ((vec 2 3)) (float 360555127546 -11))
     ("r" ((vec 2 3)) 3)
     ("c" ((vec 2 3)) 5)
     ("p" ((vec 2 4 1) (vec 8 9 2)) (vec -1 4 -14))
     ("k"
      ((vec (vec 2 3) (vec 8 7))
       (vec (vec 9 14) (vec -4 -16)))
      (vec
       (vec 18 28 27 42)
       (vec -8 -32 -12 -48)
       (vec 72 112 63 98)
       (vec -32 -128 -28 -112)))

     ("&"
      ((vec (vec 2 4 1) (vec 8 9 2) (vec 7 12 3)))
      (vec (vec -3 0 1) (vec 10 1 -4) (vec -33 -4 14)))

     ("d" ((vec (vec 2 4 1) (vec 8 9 2) (vec 7 12 3))) -1)
     ("l"
      ((vec (vec 2 4 ) (vec 8 9)))
      (vec
       (vec (vec 0 1) (vec 1 0))
       (vec (vec 1 0) (vec (float 25 -2) 1))
       (vec (vec 8 9) (vec 0 (float 175 -2)))))

     ("T" ((vec (vec 2 4 ) (vec 8 9))) 11)))
  (casualt-breakdown t))

(ert-deftest test-casual-statistics-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-statistics-menu
   '(("c" ((vec 1 2)) 2)
     ("c" ((vec 1 2 (vec 3))) 3)
     ("c" ((vec (vec 1 2) (vec 3))) 3)
     ("s" ((vec 3 5)) 8)
     ("x" ((vec 3 5)) 5)
     ("m" ((vec 5 5)) 5)
     ("e" ((vec 1 2 3 4 5 6 7 8 9)) (sdev 5 (float 912870929175 -12)))
     ("M" ((vec 1 2 3 4 5 6 7 8 9)) 5)
     ("h" ((vec 1 2 3 4 5 6 7 8 9)) (float 318137186141 -11))
     ("g" ((vec 1 2 3 4 5 6 7 8 9)) (float 41471662744 -10))
     ("r" ((vec 1 2 3 4 5 6 7 8 9)) (float 562731433871 -11))
     ("1" ((vec 1 2 3 4 5 6 7 8 9)) (float 273861278753 -11))
     ("2" ((vec 1 2 3 4 5 6 7 8 9)) (float 258198889747 -11))
     ("3" ((vec 1 2 3 4 5 6 7 8 9)) (float 75 -1))
     ("4" ((vec 1 2 3 4 5 6 7 8 9)) (float 666666666667 -11))
     ("5" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -175 -2))
     ("6" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -14 -1))
     ("7" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -746202507245 -12))))
  (casualt-breakdown t))


(ert-deftest test-casual-set-operations-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-set-operations-menu
   '(("d" ((vec 5 3 3 4 1)) (vec 1 3 4 5))
     ("u" ((vec 1 2) (vec 3 4)) (vec 1 2 3 4))
     ("i" ((vec 1 2 3) (vec 3 4)) (vec 3))
     ("-" ((vec 1 2 3) (vec 3 4)) (vec 1 2))
     ("x" ((vec 1 2 3) (vec 3 4)) (vec 1 2 4))
     ("~" ((vec 3 10)) (vec
                        (intv 2 (neg (var inf var-inf)) 3) (intv 0 3 10)
                        (intv 1 10 (var inf var-inf))))
     ("#" ((vec 5 3 3 4 1)) 4)))
  (casualt-breakdown t))

(ert-deftest test-casual-units-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-units-menu
   '(("ckg" ((* 2 (var lb var-lb))) (* (float 90718474 -8) (var kg var-kg)))
     ("tdegC" ((* 57 (var degF var-degF)))
      (* (float 138888888889 -10) (var degC var-degC)))
     ("b" ((var km var-km)) (* 1000 (var m var-m)))
     ("r" ((* 100 (var km var-km))) 100)
     ("x" ((* 100 (var km var-km))) (var km var-km))))
  ;; TODO: test "v"
  (casualt-breakdown t))

(ert-deftest test-casual-map-and-reduce-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-map-and-reduce-menu
   '(
     ("m+" ((vec 1 2) 1) (vec 2 3))
     ("r+" ((vec 1 2)) 3)
     ("a+" ((vec 2 4)) 6)
     ("A+" ((vec 1 3 8)) (vec 1 4 12))))
  (casualt-breakdown t))

(ert-deftest test-casual-logarithmic-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-logarithmic-menu
   '(
     ("l" ((float 109663315843 -8)) (float 7 0))
     ("e" ((float 7 0)) (float 109663315843 -8))
     ("L" ((float 100 0)) (float 2 0))
     ;; (read-kbd-macro "M-l") evals to [134217836]
     ([134217836] (8 2) 3)
     ;; (read-kbd-macro "M-e") evals to [134217829]
     ([134217829] (3) (float 190855369232 -10))))
  (casualt-breakdown t))

;; TODO: implement test for modes menu
;; (ert-deftest test-casual-modes-menu ())

(ert-deftest test-casual-complex-format-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-complex-format-menu
   '(("i" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'i)))))
     ("j" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'j)))))
     ("c" () (lambda () (calc-slow-wrapper (should (not calc-complex-format)))))))
  (casualt-breakdown t))

(ert-deftest test-casual-angle-measure-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-angle-measure-menu
   '(("r" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'rad)))))
     ("h" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'hms)))))
     ("d" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'deg)))))))
  (casualt-breakdown t))

(ert-deftest test-casual-radix-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-radix-menu
   '(("2" () (lambda () (should (= calc-number-radix 2))))
     ("8" () (lambda () (should (= calc-number-radix 8))))
     ("6" () (lambda () (should (= calc-number-radix 16))))
     ;; TODO "n" case
     ("0" () (lambda () (should (= calc-number-radix 10))))))
  (casualt-breakdown t))

(ert-deftest test-casual-float-format-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-float-format-menu
   '(("s" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'sci)))))
     ("f3" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'fix)))))
     ("e" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'eng)))))
     ("n" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'float)))))
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-trig-menu ()
  (casualt-setup)
  (calc-degrees-mode 1)
  (casualt-run-menu-input-testcases
   'casual-trig-menu
   '(("s" (90) 1)
     ("c" (0) 1)
     ("t" (45) (float 1 0))
     ("S" (1) 90)
     ("C" (1) 0)
     ("T" (1) 45)))
  (casualt-breakdown t))

(ert-deftest test-casual-hyperbolic-trig-menu ()
  (casualt-setup)
  (calc-degrees-mode 1)
  (casualt-run-menu-input-testcases
   'casual-hyperbolic-trig-menu
   '(("s" ((float 88137358702 -11)) (float 1 0))
     ("c" ((float 0 0)) (float 1 0))
     ("t" ((float 33 -2)) (float 318520776903 -12))
     ("S" ((float 1 0)) (float 88137358702 -11))
     ("C" ((float 1 0)) (float 0 0))
     ("T" ((float 318520776903 -12)) (float 33 -2))))
  (casualt-breakdown t))

;;; Labels
(ert-deftest test-casual-cmplx-or-polar-label ()
  (casualt-setup)
  (setq calc-complex-mode 'polar)
  (should (equal (casual-cmplx-or-polar-label)
                 "Change to Complex Mode (now Polar)"))
  (setq calc-complex-mode 'cmplx)
  (should (equal (casual-cmplx-or-polar-label)
                 "Change to Polar Mode (now Complex)"))
  (casualt-breakdown t))

(ert-deftest test-casual-symbolic-mode-label ()
  (casualt-setup)
  (setq calc-symbolic-mode t)
  (should (equal (casual-symbolic-mode-label)
                 "Change to Numeric Mode (now Symbolic)"))
  (setq calc-symbolic-mode nil)
  (should (equal (casual-symbolic-mode-label)
                 "Change to Symbolic Mode (now Numeric)"))
  (casualt-breakdown t))

(ert-deftest test-casual-prefer-frac-label ()
  (casualt-setup)
  (setq calc-prefer-frac t)
  (should (equal (casual-prefer-frac-label)
      "Change to Floating Point Results (now Fractional)"))
  (setq calc-prefer-frac nil)
  (should (equal (casual-prefer-frac-label)
    "Change to Fractional Results (now Floating Point)"))
  (casualt-breakdown t))

(ert-deftest test-casual-number-radix-label ()
  (casualt-setup)
  (setq calc-number-radix '2)
  (should (equal (casual-number-radix-label) "Binary"))
  (setq calc-number-radix '8)
  (should (equal (casual-number-radix-label) "Octal"))
  (setq calc-number-radix '16)
  (should (equal (casual-number-radix-label) "Hexadecimal"))
  (setq calc-number-radix '7)
  (should (equal (casual-number-radix-label) "7"))
  (setq calc-number-radix '10)
  (should (equal (casual-number-radix-label) "Decimal"))
  (casualt-breakdown t))

(ert-deftest test-casual-matrix-mode-label ()
  (casualt-setup)
  (setq calc-matrix-mode 'matrix)
  (should (equal (casual-matrix-mode-label) "Matrix"))
  (setq calc-matrix-mode 'sqmatrix)
  (should (equal (casual-matrix-mode-label) "Square Matrix"))
  (setq calc-matrix-mode 'scalar)
  (should (equal (casual-matrix-mode-label) "Scalar"))
  (setq calc-matrix-mode 7)
  (should (equal (casual-matrix-mode-label) "7x7"))
  (setq calc-matrix-mode nil)
  (should (equal (casual-matrix-mode-label) "No assumptions"))
  (casualt-breakdown t))

(ert-deftest test-casual-angle-mode-label ()
  (casualt-setup)
  (setq calc-angle-mode 'deg)
  (should (equal (casual-angle-mode-label) "Degrees"))
  (setq calc-angle-mode 'rad)
  (should (equal (casual-angle-mode-label) "Radians"))
  (setq calc-angle-mode 'hms)
  (should (equal (casual-angle-mode-label) "hms"))
  (casualt-breakdown t))

(ert-deftest test-casual-complex-format-label ()
  (casualt-setup)
  (setq calc-complex-format 'i)
  (should (equal (casual-complex-format-label) "x + yi"))
  (setq calc-complex-format 'j)
  (should (equal (casual-complex-format-label) "x + yj"))
  (setq calc-complex-format nil)
  (should (equal (casual-complex-format-label) "(x, y)"))
  (casualt-breakdown t))

(ert-deftest test-casual-float-format-label ()
  (casualt-setup)
  (setq calc-float-format (list 'sci 0))
  (should (equal (casual-float-format-label) "Scientific"))
  (setq calc-float-format (list 'eng 0))
  (should (equal (casual-float-format-label) "Engineering"))
  (setq calc-float-format (list 'fix 0))
  (should (equal (casual-float-format-label) "Fixed Point"))

  (setq calc-float-format (list 'sci 4))
  (should (equal (casual-float-format-label t) "Scientific 4"))
  (setq calc-float-format (list 'eng 5))
  (should (equal (casual-float-format-label t) "Engineering 5"))
  (setq calc-float-format (list 'fix 7))
  (should (equal (casual-float-format-label t) "Fixed Point 7"))

  (setq calc-float-format (list 'float 0))
  (should (equal (casual-float-format-label) "Normal"))
  (casualt-breakdown t))

(ert-deftest test-casual--prefix-label ()
  (should (equal (casual--prefix-label "fred" "jane")
                 "jane fred")))

(ert-deftest test-casual--suffix-label ()
  (should (equal (casual--suffix-label "fred" "jane")
                 "fred jane")))

(ert-deftest test-casual--checkbox-label ()
  (let ((var t))
    (should (equal (casual--checkbox-label var "mary")
                   "[x] mary"))
    (setq var nil)
    (should (equal (casual--checkbox-label var "min")
                   "[ ] min"))))

;; Predicates
(ert-deftest test-casual-matrixp ()
  (casualt-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (casual-matrixp))
  (casualt-breakdown t))

(ert-deftest test-casual-square-matrixp ()
  (casualt-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (casual-square-matrixp))
  (casualt-breakdown t))

(ert-deftest test-casual-vectorp ()
  (casualt-setup)
  (calc-push-list '((vec 8 2 9)))
  (should (casual-vectorp))
  (casualt-breakdown t))

(ert-deftest test-casual-crossp ()
  (casualt-setup)
  (calc-push-list '((vec 8 2 9) (vec 1 5 3)))
  (should (casual-crossp))
  (casualt-breakdown t))

(ert-deftest test-casual-matrixmultp ()
  (casualt-setup)
  (calc-push-list '((vec (vec 8 2)) (vec (vec 7) (vec 4))))
  (should (casual-matrixmultp))
  (casualt-breakdown t))


(provide 'test-casual)
;;; test-casual.el ends here
