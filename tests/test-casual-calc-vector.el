;;; test-casual-calc-vector.el --- Casual Vector/Matrix Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-vector)

(ert-deftest test-casual-calc-vector-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-tmenu
   '(
     ("b|" (2 3) (vec 2 3))
     ;; TODO: define remaining testcases.
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-vector-tmenu-extract ()
  (casualt-setup)
  (calc-push-list '((vec (vec 1 4) (vec 2 3))))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) '(vec 2 3)))

  (calc-push-list '((vec (vec 9 2) (vec 7 14))))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "c2")
  (should (equal (calc-top) '(vec 2 14)))

  (calc-push-list '((vec 7 9)))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) 9))

  (calc-push-list '((vec 2 3)))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "c1")
  (should (= (calc-top) 2))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-vector-building-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-building-tmenu
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

(ert-deftest test-casual-calc-vector-arithmetic-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-arithmetic-tmenu
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

(ert-deftest test-casual-calc-statistics-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-statistics-tmenu
   '(("c" ((vec 1 2)) 2)
     ("c" ((vec 1 2 (vec 3))) 3)
     ("c" ((vec (vec 1 2) (vec 3))) 3)
     ("s" ((vec 3 5)) 8)
     ("x" ((vec 3 5)) 5)
     ("m" ((vec 5 5)) 5)
     ("e" ((vec 1 2 3 4 5 6 7 8 9)) (sdev 5 (float 912870929175 -12)))
     ("M" ((vec 1 2 3 4 5 6 7 8 9)) 5)
     ("H" ((vec 1 2 3 4 5 6 7 8 9)) (float 318137186141 -11))
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

(ert-deftest test-casual-calc-set-operations-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-set-operations-tmenu
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

(ert-deftest test-casual-calc-map-and-reduce-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-map-and-reduce-tmenu
   '(
     ("m+" ((vec 1 2) 1) (vec 2 3))
     ("r+" ((vec 1 2)) 3)
     ("a+" ((vec 2 4)) 6)
     ("A+" ((vec 1 3 8)) (vec 1 4 12))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-histogram ()
  (casualt-setup)
  (calc-push '(vec 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
                   19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
                   35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
                   51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66
                   67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82
                   83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
                   99 100))

  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "sh[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]")
  (should (equal (calc-top) '(vec 5 10 10 10 10 10 10 10 10 10 5)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-vector-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors '(("b" . casual-calc-vector-building-tmenu)
                        ("a" . casual-calc-vector-arithmetic-tmenu)
                        ("s" . casual-calc-statistics-tmenu)
                        ("S" . casual-calc-set-operations-tmenu)
                        ("m" . casual-calc-map-and-reduce-tmenu)
                        ;; TODO: seems like calc functions can not be advised, need to wrap
                        ;; ("lq" . calc-vlength)
                        ;; ("tq" . calc-transpose)
                        ;; ("v" . calc-reverse-vector)
                        ;; ("o" . calc-sort)
                        ;; ("d" . calc-remove-duplicates)
                        ;; ("r" . calc-mrow)
                        ;; ("c" . calc-mcol)
                        ;; ("p" . calc-pack)
                        ;; ("u" . calc-unpack)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-vector-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-statistics-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors '(("h" . casual-calc--calc-histogram)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-statistics-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(provide 'test-casual-calc-vector)
;;; test-casual-calc-vector.el ends here
