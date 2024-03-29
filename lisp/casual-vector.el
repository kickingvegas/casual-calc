;;; casual-vector.el --- Casual Vector Menu          -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'transient)
(require 'casual-predicates)

(transient-define-prefix casual-vector-menu ()
  "Casual vector and matrix functions top-level menu."
  ["Vector & Matrix Functions (index is 1-offset)\n"
   ["Categories"
   ("b" "Building›" casual-vector-building-menu :transient nil)
   ("a" "Arithmetic›" casual-vector-arithmetic-menu :transient nil)
   ("s" "Statistics›" casual-statistics-menu :transient nil)
   ("S" "Set Operations›" casual-set-operations-menu :transient nil)
   ("m" "Map, Reduce, Apply›" casual-map-and-reduce-menu :transient nil)]

   ["Manipulate"
    :pad-keys t
    ("l" "Length" calc-vlength :transient t)
    ("t" "Transpose" calc-transpose :transient t)
    ("v" "Reverse" calc-reverse-vector :transient t)
    ("o" "Sort" calc-sort :transient t)
    ("d" "Deduplicate" calc-remove-duplicates :transient t)]

   ["Extract and Pack"
    ("r" "Extract Row…" calc-mrow :transient nil)
    ("c" "Extract Column…" calc-mcol :transient nil)
    ("p" "Pack (𝑛)" calc-pack :transient nil)
    ("u" "Unpack" calc-unpack :transient nil)]]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-vector-building-menu ()
  "Casual vector building functions menu."
  ["Vector Building (index is 1-offset, 𝑛 is a prompt value)\n"
   ["Build"
    ("|" "Concat" calc-concat :transient nil)
    ("i" "index (1..𝑛)…" calc-index :transient nil)
    ("e" "Enumerate Interval" calc-set-enumerate :transient nil)
    ("I" "Identity 𝑛…" calc-ident :transient nil)
    ("d" "Diagonal (𝟣:)" calc-diag :transient nil)
    ("b" "Build Vector 𝑛…" calc-build-vector :transient nil)]

   ["Manipulate"
    ("t" "Transpose" calc-transpose :transient nil)
    ("r" "Reverse" calc-reverse-vector :transient nil)
    ("a" "Vector Arrange" calc-arrange-vector :transient nil)
    ("s" "Sort" calc-sort :transient nil)
    ("p" "Deduplicate" calc-remove-duplicates :transient nil)]

   ["Miscellaneous"
    ("l" "Length" calc-vlength :transient nil)
    ("c" "Vector Count" calc-vector-count :transient nil)
    ("f" "Vector Find (𝟣:)" calc-vector-find :transient nil)
    ("h" "Histogram" calc-histogram :transient nil)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-vector-arithmetic-menu ()
  "Casual vector arithmetic functions menu."
  [["Arithmetic (index is 1-offset)\n"
    ("t" "Conjugate Transpose" calc-conj-transpose :transient nil)
    ("A" "Frobenius Norm (|𝑛|)" calc-abs :transient nil)
    ("r" "Row Norm" calc-rnorm :transient nil)
    ("c" "Column Norm" calc-cnorm :transient nil)
    ("p" "RH Cross Product" calc-cross :inapt-if-not casual-crossp :transient nil)
    ("k" "Kronecker Product" calc-kron :inapt-if-not casual-matrixmultp :transient nil)]
   ["Square Matrix"
    ("&" "Inverse" calc-inv :inapt-if-not casual-square-matrixp :transient nil)
    ("d" "Determinant" calc-mdet :inapt-if-not casual-square-matrixp  :transient nil)
    ("l" "LU Decomposition" calc-mlud :inapt-if-not casual-square-matrixp :transient nil)
    ("T" "Trace" calc-mtrace :inapt-if-not casual-square-matrixp :transient nil)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

;; TODO: add Transient prefix arguments n
(transient-define-prefix casual-statistics-menu ()
  "Casual statistic functions menu."
  ["Statistics (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
   ["Mean and Error"
    ("c" "Vector Count" calc-vector-count :transient nil)
    ("s" "Sum" calc-vector-sum :transient nil)
    ("x" "Max" calc-vector-max :transient nil)
    ("m" "Mean" calc-vector-mean :transient nil)
    ("h" "Histogram…" casual-calc-histogram :transient nil)
    ("e" "Mean Error" calc-vector-mean-error :transient nil)
    ("M" "Median" calc-vector-median :transient nil)
    ("H" "Harmonic Mean" calc-vector-harmonic-mean :transient nil)
    ("g" "Geometric Mean" calc-vector-geometric-mean :transient nil)]

   ["Deviation and Variance"
    ("r" "Root Mean Square" calc-vector-rms :transient nil)
    ("1" "Standard Deviation" calc-vector-sdev :transient nil)
    ("2" "Population Standard Deviation" calc-vector-pop-sdev :transient nil)
    ("3" "Variance" calc-vector-variance :transient nil)
    ("4" "Population Variance" calc-vector-pop-variance :transient nil)]

   ["Paired-Sample Statistics" ; predicate for two vectors of the same size
    ("5" "Covariance" calc-vector-covariance :transient nil)
    ("6" "Population Covariance" calc-vector-pop-covariance :transient nil)
    ("7" "Correlation" calc-vector-correlation :transient nil)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-set-operations-menu ()
  "Casual set functions menu."
  ["Set Operations"
    ("d" "Deduplicate" calc-remove-duplicates :transient nil)
    ("u" "Union" calc-set-union :transient nil)
    ("i" "Intersect" calc-set-intersect :transient nil)
    ("-" "Difference" calc-set-difference :transient nil)
    ("x" "xor" calc-set-xor :transient nil)
    ("~" "Complement" calc-set-complement :transient nil)
    ("#" "Cardinality" calc-set-cardinality :transient nil)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-map-and-reduce-menu ()
  "Casual functional operations (map, reduce, apply) menu."
  ["Functional Operators"
   ("m" "map" calc-map :transient nil)
   ("r" "reduce" calc-reduce :transient nil)
   ("a" "apply" calc-apply :transient nil)
   ("A" "accumulate" calc-accumulate :transient nil)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

;;; Wrapped Functions

(defun casual-calc-histogram ()
  "Build histogram of (1:).
\nGiven a vector data set in (1:), this command will prompt the
user for a bin specification vector, where each element of the
vector is a center point of a bin. For example, if the entered
bin vector is '[a, b, c, …]' then the bin ranges will be computed
as (-inf, (a+b)/2], ((a+b)/2, (b+c)/2], …

* Example

Start with the following data set of integer numbers from 1 to 100.

1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
    67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
    83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
    99, 100]

A histogram of the above data set where each bin is every 10
units requires this bin vector. Enter this when prompted:

[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

The result on the stack is:

1: [5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5]

* References - info node `(calc) Manipulating Vectors' -
`calc-histogram'"
  (interactive)
  (call-interactively #'calc-histogram))

(provide 'casual-vector)
;;; casual-vector.el ends here
