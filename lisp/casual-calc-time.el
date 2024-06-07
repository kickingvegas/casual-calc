;;; casual-calc-time.el --- Casual Time Menu              -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-calc-time-tmenu ()
  "Casual time functions menu."
  ["Time"
   ("n" "Now" calc-now :transient nil)
   ("f" "First Day of›" casual-calc-first-day-tmenu :transient nil)
   ("i" "Increment Month" calc-inc-month :transient nil)
   ("u" "To Unix Time" calc-unix-time :transient nil)
   ("+" "Add Business Days" calc-business-days-plus :transient nil)
   ("-" "Subtract Business Days" calc-business-days-minus :transient nil)]
  [:class transient-row
          (casual-calc-quit-one)
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])

(transient-define-prefix casual-calc-first-day-tmenu ()
  "Casual time first day of menu."
  ["First Day Of"
   ("w" "Week" calc-new-week :transient nil)
   ("m" "Month" calc-new-month :transient nil)
   ("y" "Year" calc-new-year :transient nil)]
  [:class transient-row
          (casual-calc-quit-one)
          (casual-calc-quit-all)
          (casual-calc-undo-suffix)])

(provide 'casual-calc-time)
;;; casual-calc-time.el ends here
