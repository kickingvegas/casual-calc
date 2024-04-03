;;; casual-algebra.el --- Casual Algebra Menu        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
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
(require 'casual-labels)

(transient-define-prefix casual-algebra-menu ()
  "Casual algebra functions menu."
  [:description
   (lambda ()
     (format "Algebraic Functions\nAlgebraic Mode %s, Symbolic Mode %s\n"
             (casual--variable-to-checkbox calc-algebraic-mode)
             (casual--variable-to-checkbox calc-symbolic-mode)))
   ["General"
    ("v" "evaluate" calc-alg-evaluate :transient t)
    ("=" "evaluate with values" calc-evaluate :transient t)
    ("\"" "expand formula" calc-expand-formula :transient t)
    ("b" "substitute" calc-substitute :transient t)
    ("s" "simplify" calc-simplify :transient t)
    ("e" "simplify, extended" calc-simplify-extended :transient t)]
   ["Polynomials"
    ("f" "factor" calc-factor :transient t)
    ("P" "polynomial roots" calc-poly-roots :transient t)
    ("c" "collect" calc-collect :transient t)
    ("x" "expand" calc-expand :transient t)
    ("n" "normalize rational" calc-normalize-rat :transient t)
    ("\\" "polynomial division" calc-poly-div :transient t)
    ("%" "polynomial remainder" calc-poly-rem :transient t)
    ("/" "polynomial div with remainder" calc-poly-div-rem :transient t)
    ("g" "polynomial gcd" calc-poly-gcd :transient t)
    ("p" "polynomial interpolation" calc-poly-interp :transient t)]
   ["Calculus"
    ("d" "derivative" calc-derivative :transient t)
    ("i" "integral" calc-integral :transient t)
    ("I" "numerical integration" calc-num-integral :transient t)
    ("t" "taylor series" calc-taylor :transient t)]
   ["Solve/Optimise"
    ("S" "solve for" calc-solve-for :transient t)
    ("R" "find root" calc-find-root :transient t)
    ("N" "find minimum" calc-find-minimum :transient t)
    ("X" "find maximum" calc-find-maximum :transient t)
    ("F" "fit curve" calc-curve-fit :transient t)]
   ["Summations"
    ("+" "sum" calc-summation :transient t)
    ("-" "alternating sum" calc-alt-summation :transient t)
    ("*" "product" calc-product :transient t)
    ("T" "tabulate" calc-tabulate :transient t)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual-algebra)
;;; casual-algebra.el ends here
