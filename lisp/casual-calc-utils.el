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
(require 'transient)

;; Transient Navigation
(transient-define-suffix casual-calc-quit-all ()
  "Dismiss all menus."
  :transient nil
  :key "C-q"
  :description "Dismiss"
  (interactive)
  (transient-quit-all))

(transient-define-suffix casual-calc-quit-one ()
  "Go back to previous menu."
  :transient nil
  :key "C-g"
  :description "‹Back"
  (interactive)
  (transient-quit-one))

(transient-define-suffix casual-calc-undo-suffix ()
  "Undo stack."
  :transient t
  :key "U"
  :description "Undo Stack"
  (interactive)
  (call-interactively #'calc-undo))

(provide 'casual-calc-utils)
;;; casual-calc-utils.el ends here
