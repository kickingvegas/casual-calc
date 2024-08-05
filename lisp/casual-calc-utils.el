;;; casual-calc-utils.el --- Casual Calc Utils       -*- lexical-binding: t; -*-

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
(require 'casual-calc--calc)
(require 'transient)
(require 'casual-lib)

;; Transient Navigation
(transient-define-suffix casual-calc-undo-suffix ()
  "Undo stack."
  :transient t
  :key "U"
  :description "Undo ≣"
  (interactive)
  (call-interactively #'calc-undo))

(transient-define-suffix casual-calc-algebraic-entry ()
  "Algebraic entry."
  :transient t
  :key "'"
  :description "Entry"
  (interactive)
  (call-interactively #'calc-algebraic-entry))

(transient-define-suffix casual-calc-pop ()
  "Pop."
  :transient t
  :key "DEL"
  :description "Pop"
  (interactive)
  (call-interactively #'calc-pop))

(transient-define-suffix casual-calc-enter ()
  "Enter/Return."
  :transient t
  :key "RET"
  :description "Enter"
  (interactive)
  (call-interactively #'calc-enter))

(transient-define-suffix casual-calc-edit ()
  "Enter/Return."
  :transient nil
  :key "`"
  :description "Edit"
  (interactive)
  (call-interactively #'calc-edit))

(defconst casual-calc-operators-group
  ["Operators"
    ("+" "add" casual-calc--plus :transient t)
    ("-" "sub" casual-calc--minus :transient t)
    ("*" "mul" casual-calc--times :transient t)
    ("/" "div" casual-calc--divide :transient t)
    ("%" "mod" casual-calc--mod :transient t)])

(defconst casual-calc-basic-operators-group
  ["Operators"
    ("+" "add" casual-calc--plus :transient t)
    ("-" "sub" casual-calc--minus :transient t)
    ("*" "mul" casual-calc--times :transient t)
    ("/" "div" casual-calc--divide :transient t)])

(defconst casual-calc-navigation-group
  [:class transient-row
          (casual-lib-quit-one)
          (casual-calc-algebraic-entry)
          (casual-calc-enter)
          (casual-calc-pop)
          (casual-calc-undo-suffix)
          (casual-lib-quit-all)])

(provide 'casual-calc-utils)
;;; casual-calc-utils.el ends here
