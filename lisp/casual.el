;;; casual.el --- Transient UI for Calc              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools
;; Version: 1.2.1
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
(require 'casual-predicates)
(require 'casual-labels)
(require 'casual-angle-measure)
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

;; Private functions to avoid using anonymous functions in Transients
(defun casual--e-constant ()
  "Constant 𝑒."
  (interactive)
  (calc-hyperbolic)
  (calc-pi))

(defun casual--stack-roll-all ()
  "Roll entire stack."
  (interactive)
  (calc-roll-down (calc-stack-size)))

(defun casual--stack-clear ()
  "Clear entire stack."
  (interactive)
  (calc-pop-stack (calc-stack-size)))

;; Menus
(transient-define-prefix casual-main-menu ()
  "Casual main menu."
  [["Calc"
    :pad-keys t
    ("&" "1/𝑥" calc-inv :transient nil)
    ("Q" " √" calc-sqrt :transient nil)
    ("n" "+∕− " calc-change-sign :transient nil)
    ("^" "𝑦^𝑥" calc-power :transient nil)
    ("=" " =" calc-evaluate :transient nil)]
   [""
    ("A" "|𝑥|" calc-abs :transient nil)
    ("!" " !" calc-factorial :transient nil)
    ("%" " ٪" calc-percent :transient nil)
    ("D" " Δ%" calc-percent-change :transient nil)]
   ["Constants"
    ("p" "𝜋" calc-pi :transient nil)
    ("e" "𝑒" casual--e-constant :transient nil)]
   ["Settings"
    ("m" "Modes, Displays, Angles›" casual-modes-menu :transient nil)]]

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
    ("g" "Graphics›" casual-plot-menu :transient nil)]
   ["Stack"
    :pad-keys t
    ("s" "Swap" calc-roll-down :transient t)
    ("r" "Roll" casual--stack-roll-all :transient t)
    ("d" "Drop" calc-pop :transient t)
    ("C" "Clear" casual--stack-clear :transient t)
    ("L" "Last" calc-last-args :transient t)
    ("y" "Copy to Buffer" calc-copy-to-buffer :transient nil)
    ("z" "Variables›" casual-variable-crud-menu :transient nil)]]
  [:class transient-row
          ;; Note: no need to C-g for main menu
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-variable-crud-menu ()
  "Casual variable CRUD menu."
  ["Variable Operations"
   ("s" "Store (𝟣:)…" calc-store :transient t)
   ("r" "Recall…" calc-recall :transient t)
   ("c" "Clear…" calc-unstore :transient t)
   ("e" "Edit…" calc-edit-variable :transient nil)
   ("o" "Copy to other variable…" calc-copy-variable :transient t)
   ("x" "Exchange (𝟣:) to variable…" calc-store-exchange :transient t)
   ("p" "Persist…" calc-permanent-variable :transient t)
   ("i" "Insert variables into buffer…" calc-insert-variables :transient t)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual)
;;; casual.el ends here
