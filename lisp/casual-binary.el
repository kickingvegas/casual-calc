;;; casual-binary.el --- Casual Binary Menu          -*- lexical-binding: t; -*-

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
(require 'casual-radix)

(transient-define-prefix casual-binary-menu ()
  "Casual binary functions menu."
  ["Binary Functions"
   ["Operators"
    ("a" "and" calc-and :transient nil)
    ("o" "or" calc-or :transient nil)
    ("x" "xor" calc-xor :transient nil)
    ("-" "diff" calc-diff :transient nil)
    ("!" "not" calc-not :transient nil)]
   ["Shift"
    :pad-keys t
    ("l" "binary left" calc-lshift-binary :transient t)
    ("r" "binary right" calc-rshift-binary :transient t)
    ("M-l" "arithmetic left" calc-lshift-arith :transient t)
    ("M-r" "arithmetic right" calc-rshift-arith :transient t)
    ("C-r" "rotate binary" calc-rotate-binary :transient t)]
   ["Utils"
    ("R" casual-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-number-radix-label)))
     :transient nil)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros "Leading Zeroes"))
     :transient nil)
    ("w" "Set Word Size…" calc-word-size :transient nil)
    ("u" "Unpack Bits" calc-unpack-bits :transient nil)
    ("p" "Pack Bits" calc-pack-bits :transient nil)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual-binary)
;;; casual-binary.el ends here
