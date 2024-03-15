;;; casual-units.el --- Casual Units                 -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-units-menu ()
  "Casual unit conversions menu."
  ["Unit Conversions"
   ("c" "Convert" calc-convert-units :transient nil)
   ("t" "Convert Temperature" calc-convert-temperature :transient nil)
   ("b" "Convert to Base Unit" calc-base-units :transient nil)
   ;; TODO: display current autorange state
   ;;("a" "Autorange" calc-autorange-units :transient nil)
   ("r" "Remove Units" calc-remove-units :transient nil)
   ("x" "Extract Units" calc-extract-units :transient nil)
   ("v" "View Units" calc-view-units-table :transient nil)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])


(provide 'casual-units)
;;; casual-units.el ends here
