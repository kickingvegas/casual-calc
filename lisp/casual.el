;;; casual.el --- Transient UI for Calc              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools
;; Version: 1.7.0
;; Package-Requires: ((emacs "29.1"))

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

;; Casual Calc is an opinionated Transient-based porcelain for Emacs Calc.

;; INSTALLATION
;; (require 'casual)
;; (define-key calc-mode-map (kbd "C-o") #'casual-calc-tmenu)

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)
(require 'casual-calc-utils)
(require 'casual-calc--calc)
(require 'casual-calc-version)
(require 'casual-calc-binary)
(require 'casual-calc-complex)
(require 'casual-calc-conversion)
(require 'casual-calc-logarithmic)
(require 'casual-calc-random)
(require 'casual-calc-rounding)
(require 'casual-calc-settings)
(require 'casual-calc-time)
(require 'casual-calc-trigonometric)
(require 'casual-calc-units)
(require 'casual-calc-vector)
(require 'casual-calc-graphics)
(require 'casual-calc-trail)
(require 'casual-calc-stack)
(require 'casual-calc-financial)
(require 'casual-calc-symbolic)
(require 'casual-calc-variables)

;; Menus
;;;###autoload (autoload 'casual-calc-tmenu "casual" nil t)
(transient-define-prefix casual-calc-tmenu ()
  "Casual main menu."
  [["Casual"
    :pad-keys t
    ("&" "1/𝑥" casual-calc--calc-inv :transient nil)
    ("Q" " √" casual-calc--calc-sqrt :transient nil)
    ("n" "+∕− " casual-calc--calc-change-sign :transient nil)
    ("^" "𝑦^𝑥" casual-calc--calc-power :transient nil)
    ("=" " =" casual-calc--calc-evaluate :transient nil)]
   [""
    ("A" "|𝑥|" casual-calc--calc-abs :transient nil)
    ("!" " !" casual-calc--calc-factorial :transient nil)
    ("%" " ٪" casual-calc--calc-percent :transient nil)
    ("D" " Δ%" casual-calc--calc-percent-change :transient nil)]
   ["Constants"
    ("p" "𝜋" casual-calc--calc-pi :transient nil)
    ("e" "𝑒" casual-calc--e-constant :transient nil)]
   ["Settings"
    :pad-keys t
    ("m" "Modes, Displays, Angles›" casual-calc-modes-tmenu :transient nil)
    ("M-s" "Stack›" casual-calc-stack-display-tmenu :transient nil)
    ("M-t" "Trail›" casual-calc-trail-tmenu :transient nil)]]

  [["Arithmetic"
    :pad-keys t
    ("o" "Rounding›" casual-calc-rounding-tmenu :transient nil)
    ("c" "Conversion›" casual-calc-conversions-tmenu :transient nil)
    ("T" "Time›" casual-calc-time-tmenu :transient nil)
    ("i" "Complex›" casual-calc-complex-number-tmenu :transient nil)
    ("R" "Random›" casual-calc-random-number-tmenu :transient nil)]

   ["Functions" ; test if anything is on the stack calc-stack-size 0
    ("t" "Trigonometric›" casual-calc-trig-tmenu :transient nil)
    ("l" "Logarithmic›" casual-calc-logarithmic-tmenu :transient nil)
    ("b" "Binary›" casual-calc-binary-tmenu :transient nil)
    ("v" "Vector/Matrix›" casual-calc-vector-tmenu :transient nil)
    ("u" "Units›" casual-calc-units-tmenu :transient nil)
    ("f" "Financial›" casual-calc-financial-tmenu :transient nil)
    ("g" "Graphics›" casual-calc-plot-tmenu :transient nil)
    ("a" "Algebra›" casual-calc-symbolic-tmenu :transient nil)]

   ["Stack"
    :pad-keys t
    ("s" "Swap" casual-calc--stack-swap :transient t)
    ("r" "Roll" casual-calc--stack-roll-all :transient t)
    ("d" "Drop" casual-calc--stack-drop :transient t)
    ("C" "Clear" casual-calc--stack-clear :transient t)
    ("L" "Last" casual-calc--stack-last :transient t)
    ("w" "Copy" casual-calc--calc-copy-as-kill :transient nil)
    ("z" "Variables›" casual-calc-variable-crud-tmenu :transient nil)]]

  [:class transient-row
          ;; Note: no need to C-g for main menu
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)
          ("q" "Quit Calc" calc-quit)])

;;;###autoload (autoload 'casual-main-menu "casual" nil t)
(transient-define-prefix casual-main-menu ()
  "OBSOLETE: Use `casual-calc-tmenu' instead."

  [:class transient-row
          ;; Note: no need to C-g for main menu
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])

(define-obsolete-function-alias #'casual-main-menu #'casual-calc-tmenu
  "v1.6.0"
  "Naming changed to conform with Casual Suite.")

(provide 'casual)
;;; casual.el ends here
