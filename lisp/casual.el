;;; casual.el --- Transient UI for Calc              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools
;; Version: 1.4.0
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

;; Casual is an opinionated Transient-based porcelain for Emacs Calc.

;; INSTALLATION
;; (require 'casual)
;; (define-key calc-mode-map (kbd "C-o") 'casual-main-menu)

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)
(require 'casual-calc)
(require 'casual-version)
(require 'casual-binary)
(require 'casual-complex)
(require 'casual-conversion)
(require 'casual-logarithmic)
(require 'casual-random)
(require 'casual-rounding)
(require 'casual-settings)
(require 'casual-time)
(require 'casual-trigonometric)
(require 'casual-units)
(require 'casual-vector)
(require 'casual-graphics)
(require 'casual-trail)
(require 'casual-stack)
(require 'casual-financial)

;; Menus
;;;###autoload (autoload 'casual-main-menu "casual" nil t)
(transient-define-prefix casual-main-menu ()
  "Casual main menu."
  [["Casual"
    :pad-keys t
    ("&" "1/𝑥" casual-calc-inv :transient nil)
    ("Q" " √" casual-calc-sqrt :transient nil)
    ("n" "+∕− " casual-calc-change-sign :transient nil)
    ("^" "𝑦^𝑥" casual-calc-power :transient nil)
    ("=" " =" casual-calc-evaluate :transient nil)]
   [""
    ("A" "|𝑥|" casual-calc-abs :transient nil)
    ("!" " !" casual-calc-factorial :transient nil)
    ("%" " ٪" casual-calc-percent :transient nil)
    ("D" " Δ%" casual-calc-percent-change :transient nil)]
   ["Constants"
    ("p" "𝜋" casual-calc-pi :transient nil)
    ("e" "𝑒" casual--e-constant :transient nil)]
   ["Settings"
    :pad-keys t
    ("m" "Modes, Displays, Angles›" casual-modes-menu :transient nil)
    ("M-s" "Stack›" casual-stack-display-menu :transient nil)
    ("M-t" "Trail›" casual-trail-menu :transient nil)]]

  [["Arithmetic"
    :pad-keys t
    ("o" "Rounding›" casual-rounding-menu :transient nil)
    ("c" "Conversion›" casual-conversions-menu :transient nil)
    ("T" "Time›" casual-time-menu :transient nil)
    ("i" "Complex›" casual-complex-number-menu :transient nil)
    ("a" "Random›" casual-random-number-menu :transient nil)]

   ["Functions" ; test if anything is on the stack calc-stack-size 0
    ("t" "Trigonometric›" casual-trig-menu :transient nil)
    ("l" "Logarithmic›" casual-logarithmic-menu :transient nil)
    ("b" "Binary›" casual-binary-menu :transient nil)
    ("v" "Vector/Matrix›" casual-vector-menu :transient nil)
    ("u" "Units›" casual-units-menu :transient nil)
    ("f" "Financial›" casual-financial-menu :transient nil)
    ("g" "Graphics›" casual-plot-menu :transient nil)]
   ["Stack"
    :pad-keys t
    ("s" "Swap" casual--stack-swap :transient t)
    ("r" "Roll" casual--stack-roll-all :transient t)
    ("d" "Drop" casual--stack-drop :transient t)
    ("C" "Clear" casual--stack-clear :transient t)
    ("L" "Last" casual--stack-last :transient t)
    ("w" "Copy" casual-calc-copy-as-kill :transient nil)
    ("z" "Variables›" casual-variable-crud-menu :transient nil)]]
  [:class transient-row
          ;; Note: no need to C-g for main menu
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-variable-crud-menu ()
  "Stored variable operations menu.
Operations to store, recall, clear, and edit variables are provided by this
menu."
  ["Variable Operations"
   ("s" "Store (𝟣:)…" casual-calc-store :transient t)
   ("r" "Recall…" casual-calc-recall :transient t)
   ("c" "Clear…" casual-calc-unstore :transient t)
   ("e" "Edit…" casual-calc-edit-variable :transient nil)
   ("o" "Copy to other variable…" casual-calc-copy-variable :transient t)
   ("x" "Exchange (𝟣:) to variable…" casual-calc-store-exchange :transient t)
   ("p" "Persist…" casual-calc-permanent-variable :transient t)
   ("i" "Insert variables into buffer…" casual-calc-insert-variables :transient t)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual)
;;; casual.el ends here
