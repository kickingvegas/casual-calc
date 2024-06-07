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
(require 'calc)
(require 'transient)
(require 'casual-calc-labels)
(require 'casual-calc-angle-measure)

(transient-define-prefix casual-calc-trig-tmenu ()
  "Casual trigonometric functions menu."
  ;; ["Arguments"
  ;;  ("i" "inverse" "-inverse")
  ;;  ("h" "hyperbolic" "-hyperbolic")]
  [["Trig"
    ("s" "sin" calc-sin :transient nil)
    ("c" "cos" calc-cos :transient nil)
    ("t" "tan" calc-tan :transient nil)]
   ["Inverse"
    ("S" "arcsin" calc-arcsin :transient nil)
    ("C" "arccos" calc-arccos :transient nil)
    ("T" "arctan" calc-arctan :transient nil)]

   ["Angle Measure"
    ("a" casual-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-calc-angle-mode-label)))
     :transient nil)]]
  [("h" "Hyperbolic›" casual-calc-hyperbolic-trig-tmenu :transient nil)]
  [:class transient-row
          (casual-calc-quit-one)
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])

(transient-define-prefix casual-calc-hyperbolic-trig-tmenu ()
  "Casual hyperbolic trigonometric functions menu."
  [["Hyperbolic"
    ("s" "sinh" calc-sinh :transient nil)
    ("c" "cosh" calc-cosh :transient nil)
    ("t" "tanh" calc-tanh :transient nil)]
   ["Inverse Hyperbolic"
    ("S" "arcsinh" calc-arcsinh :transient nil)
    ("C" "arccosh" calc-arccosh :transient nil)
    ("T" "arctanh" calc-arctanh :transient nil)]]
  [:class transient-row
          (casual-calc-quit-one)
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])


(provide 'casual-calc-trigonometric)
;;; casual-calc-trigonometric.el ends here
