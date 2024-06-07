;;; casual-calc-rounding.el --- Casual Rounding Menu      -*- lexical-binding: t; -*-

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
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-rounding-tmenu ()
  "Casual rounding functions menu."
  ["Rounding Functions"
   ("r" "Round" calc-round :transient nil)
   ("f" "Floor" calc-floor :transient nil)
   ("c" "Ceiling" calc-ceiling :transient nil)
   ("t" "Truncate" calc-trunc :transient nil)]
  [:class transient-row
          (casual-calc-quit-one)
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])

(provide 'casual-calc-rounding)
;;; casual-calc-rounding.el ends here
