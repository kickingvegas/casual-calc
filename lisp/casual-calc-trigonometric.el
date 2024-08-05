;;; casual-calc-trigonometric.el --- Casual Trigonometric Menus  -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)
(require 'casual-calc-labels)
(require 'casual-calc-angle-measure)

(transient-define-prefix casual-calc-trig-tmenu ()
  "Casual trigonometric functions menu."
  ;; ["Arguments"
  ;;  ("i" "inverse" "-inverse")
  ;;  ("h" "hyperbolic" "-hyperbolic")]
  [["Trig"
    ("s" "sin" calc-sin :transient t)
    ("c" "cos" calc-cos :transient t)
    ("t" "tan" calc-tan :transient t)]
   ["Inverse"
    ("S" "arcsin" calc-arcsin :transient t)
    ("C" "arccos" calc-arccos :transient t)
    ("T" "arctan" calc-arctan :transient t)]

   ["Misc"
    ("p" "𝜋" casual-calc--pi :transient t)
    ("d" "To Degrees" calc-to-degrees :transient t)
    ("r" "To Radians" calc-to-radians :transient t)
    ("a" casual-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-calc-angle-mode-label)))
     :transient t)
    ("h" "Hyperbolic›" casual-calc-hyperbolic-trig-tmenu)]
   casual-calc-operators-group]

  casual-calc-navigation-group)


(transient-define-prefix casual-calc-hyperbolic-trig-tmenu ()
  "Casual hyperbolic trigonometric functions menu."
  [["Hyperbolic"
    ("s" "sinh" calc-sinh :transient t)
    ("c" "cosh" calc-cosh :transient t)
    ("t" "tanh" calc-tanh :transient t)]
   ["Inverse Hyperbolic"
    ("S" "arcsinh" calc-arcsinh :transient t)
    ("C" "arccosh" calc-arccosh :transient t)
    ("T" "arctanh" calc-arctanh :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(provide 'casual-calc-trigonometric)
;;; casual-calc-trigonometric.el ends here
