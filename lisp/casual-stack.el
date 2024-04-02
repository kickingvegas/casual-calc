;;; casual-stack.el --- Casual Stack Menu            -*- lexical-binding: t; -*-

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

(defun casual-customize-kill-line-numbering ()
  "Customize Calc kill line numbering behavior.
Customize the variable `calc-kill-line-numbering'.
Set `calc-kill-line-numbering' to nil to exclude line numbering
from kill-ring operations."
  (interactive)
  (customize-variable 'calc-kill-line-numbering))

(transient-define-prefix casual-stack-display-menu ()
  "Casual stack display menu."
  ["Justification"
   :class transient-row
   ("l" "Left" calc-left-justify :transient t)
   ("c" "Center" calc-center-justify :transient t)
   ("r" "Right" calc-right-justify :transient t)]

  [["Truncation"
    ("." "At Point" calc-truncate-stack :transient t)
    ("p" "Previous" calc-truncate-up :transient t)
    ("n" "Next" calc-truncate-down :transient t)]

   ["Misc"
    ("R" "Refresh" calc-refresh :transient t)
    ("l" "Customize kill line numbering"
     casual-customize-kill-line-numbering :transient nil)]]
  [""
   :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("s" "Save Settings" calc-save-modes :transient t)])

(provide 'casual-stack)
;;; casual-stack.el ends here
