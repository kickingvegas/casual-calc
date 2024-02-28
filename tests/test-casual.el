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



(ert-deftest test-casual-binary-menu ()
  (casualt-setup)
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

;;; 50%
;; (ert-deftest test-casual-statistics-menu ())
;; (ert-deftest test-casual-set-operations-menu ())
;; (ert-deftest test-casual-units-menu ())
;; (ert-deftest test-casual-map-and-reduce-menu ())
;; (ert-deftest test-casual-logarithmic-menu ())
;; (ert-deftest test-casual-modes-menu ())

;;; 75%
;; (ert-deftest test-casual-angle-measure-menu ())
;; (ert-deftest test-casual-complex-format-menu ())
;; (ert-deftest test-casual-radix-menu ())
;; (ert-deftest test-casual-float-format-menu ())
;; (ert-deftest test-casual-trig-menu ())
;; (ert-deftest test-casual-hyperbolic-trig-menu ())

;;; Labels
;; (ert-deftest test-casual-cmplx-or-polar-label ()
;; (ert-deftest test-casual-symbolic-mode-label ()
;; (ert-deftest test-casual-prefer-frac-label ()
;; (ert-deftest test-casual-number-radix-label ()
;; (ert-deftest test-casual-matrix-mode-label ()

(ert-deftest test-casual-angle-mode-label ()
  (casualt-setup)
  (setq calc-angle-mode 'deg)
  (should (equal (casual-angle-mode-label) "Degrees"))
  (setq calc-angle-mode 'rad)
  (should (equal (casual-angle-mode-label) "Radians"))
  (setq calc-angle-mode 'hms)
  (should (equal (casual-angle-mode-label) "hms"))
  (casualt-breakdown t))

;; (ert-deftest test-casual-complex-format-label ()
;; (ert-deftest test-casual-float-format-label (&optional include-precision)
;; (ert-deftest test-casual--prefix-label (label prefix)
;; (ert-deftest test-casual--suffix-label (label prefix)
;; (ert-deftest test-casual--checkbox-label (v label)

;; Predicates
;; (ert-deftest test-casual-matrixp ())
;; (ert-deftest test-casual-square-matrixp ())
;; (ert-deftest test-casual-vectorp ())
;; (ert-deftest test-casual-crossp ())
;; (ert-deftest test-casual-matrixmultp ())

(provide 'test-casual)
;;; test-casual.el ends here
