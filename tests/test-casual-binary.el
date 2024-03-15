;;; test-casual-binary.el --- Casual Binary Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-binary)

(ert-deftest test-casual-binary-menu ()
  (casualt-setup)
  (calc-word-size 8)
  (casualt-run-menu-input-testcases
   'casual-binary-menu
   '(("a" (11 12) 8)
     ("o" (11 12) 15)
     ("x" (11 12) 7)
     ("-" (11 12) 3)
     ("!" (11) 244)
     ("l" (12) 24)
     ("r" (24) 12)
     ([134217836] (12) 24) ; (read-kbd-macro "M-l") evals to [134217836]
     ([134217842] (24) 12) ; (read-kbd-macro "M-r") evals to [134217842]
     ([18] (24) 48) ; (read-kbd-macro "C-r") evals to [18]
     ))
  (calc-word-size 32)
  (casualt-breakdown t))

(provide 'test-casual-binary)
;;; test-casual-binary.el ends here
