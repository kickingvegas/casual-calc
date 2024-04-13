;;; casual-variables.el --- Casual Variable Menu     -*- lexical-binding: t; -*-

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
(require 'casual-calc)

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
   ("O" "Open Calc Settings File" casual-open-settings-file :transient nil)
   ("i" "Insert variables into buffer…" casual-calc-insert-variables :transient t)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual-variables)
;;; casual-variables.el ends here
