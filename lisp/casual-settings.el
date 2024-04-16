;;; casual-settings.el --- Casual Settings Menu      -*- lexical-binding: t; -*-

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
(require 'casual-labels)
(require 'casual-version)
(require 'casual-angle-measure)

;; = Menus =
(transient-define-prefix casual-modes-menu ()
  "Casual modes menu."
  [["Modes"
    :pad-keys t
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual--checkbox-label calc-algebraic-mode
                                            "Algebraic Mode"))
     :transient t)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros
                                            "Leading Zeroes"))
     :transient t)

    ("F" calc-frac-mode :description casual-prefer-frac-label :transient t)
    ("s" calc-symbolic-mode :description casual-symbolic-mode-label :transient t)
    ("p" calc-polar-mode :description casual-cmplx-or-polar-label :transient t)
    ("c" "Complex Number Format›" casual-complex-format-menu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (casual-complex-format-label)))
     :transient t)
    ;; ("m" calc-matrix-mode :description casual-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("P" calc-precision
     :description (lambda ()
                    (format "Precision (now %d)" calc-internal-prec))
     :transient t)
    ("I" "Infinite Mode" casual-calc-infinite-mode
     :description (lambda ()
                    (casual--checkbox-label calc-infinite-mode
                                            "Infinite Mode"))
     :transient t)]

   ["Angular Measure"
    ("a" casual-angle-measure-menu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-angle-mode-label)))
     :transient t)]]

  [["Display"
    ("R" casual-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-number-radix-label)))
     :transient t)
    ("f" casual-float-format-menu
     :description (lambda ()
                    (format "Float Formats (now %s)›"
                            (casual-float-format-label)))
     :transient t)
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (casual--checkbox-label calc-group-digits
                                            "Show Thousands Separators"))
     :transient t)
    ("," "Thousands Separator…" calc-group-char
     :description (lambda ()
                    (format "Set Thousands Separator (now %s)…" calc-group-char))
     :transient t)
    ("." "Decimal Separator…" calc-point-char
     :description (lambda ()
                    (format "Set Decimal Separator (now %s)…" calc-point-char))
     :transient t)
    ("H" "ℎ𝑚𝑠 Format" calc-hms-notation
     :description (lambda ()
                    (format
                     "Set ℎ𝑚𝑠 Format (now %s)"
                     (format calc-hms-format "" "" "")))
     :transient t)]

   ["Settings"
    ("S" "Save Calc Settings" calc-save-modes :transient t)
    ("O" "Open Calc Settings File" casual-open-settings-file :transient nil)
    ("C-M-r" "Calc Reset" calc-reset :transient t)]]

  [""
   :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("v" "Version" casual-version :transient nil)
   ("M-a" "About" casual-about :transient nil)])

(transient-define-prefix casual-complex-format-menu ()
  "Casual complex formats menu."
  ["Complex Number Format"
   :description (lambda ()
                  (format "Complex Number Format (now %s)"
                          (casual-complex-format-label)))
   ("c" calc-complex-notation
    :description "complex (rectangular) notation"
    :transient nil)

   ("i" calc-i-notation
    :description "𝑖 notation"
    :transient nil)

   ("j" calc-j-notation
    :description "𝑗 notation"
    :transient nil)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])


(transient-define-prefix casual-float-format-menu ()
  "Casual float formats menu."
  ["Float Format (𝑛 is 𝟣: on stack)"
   ("n" "Normal" calc-normal-notation :transient nil)
   ("f" "Fixed Point 𝑛" calc-fix-notation :transient nil)
   ("s" "Scientific" calc-sci-notation :transient nil)
   ("e" "Engineering" calc-eng-notation :transient nil)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])


;; = Functions =
(defun casual-about-casual ()
  "Casual is an opinionated porcelain for Emacs Calc.

Learn more about using Casual at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/Casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/Casual/issues'

If you enjoy using Casual, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual was conceived and crafted by Charles Choi in San Francisco, California.

Thank you for using Casual and always choose love."
  (ignore))

(defun casual-about ()
  "About information for Casual."
  (interactive)
  (describe-function 'casual-about-casual))

(defun casual-calc-infinite-mode ()
  "Toggle infinite mode on or off.

Divide-by-zero (e.g. ‘1 / 0’) results are normally treated as
errors; formulas like this are left in unsimplified form. An
alternate behavior is to treat a divide-by-zero condition as an
infinite result. This command toggles this behavior.

This function is a wrapper over `calc-infinite-mode'.

* References
- info node `(calc) Infinite Mode'
- `calc-infinite-mode'"
  (interactive)
  (call-interactively #'calc-infinite-mode))

(provide 'casual-settings)
;;; casual-settings.el ends here
