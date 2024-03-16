;;; test-casual-conversion.el --- Casual Conversion Menu Tests  -*- lexical-binding: t; -*-

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
(require 'ert)
(require 'casual-test-utils)
(require 'casual-conversion)

(ert-deftest test-casual-conversions-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-conversions-menu
   '(("d" ((float 785398163397 -12)) (float 45 0))
     ("r" ((float 45 0)) (float 785398163397 -12))
     ("h" ((float 150833333333 -11)) (hms 1 30 (float 3 1)))
     ("F" ((float 15 -1)) (frac 3 2))
     ("f" ((frac 3 2)) (float 15 -1))
     ))
  (casualt-breakdown t))

(provide 'test-casual-conversion)
;;; test-casual-conversion.el ends here
