;;; test-casual-rounding.el --- Casual Rounding Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-rounding)

(ert-deftest test-casual-rounding-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-rounding-menu
   '(("r" ((float 27 -1)) 3)
     ("r" ((float 22 -1)) 2)
     ("f" ((float 25 -1)) 2)
     ("c" ((float 25 -1)) 3)
     ("t" ((float 25 -1)) 2)))
  (casualt-breakdown t))

(provide 'test-casual-rounding)
;;; test-casual-rounding.el ends here
