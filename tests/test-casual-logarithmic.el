;;; test-casual-logarithmic.el --- Casual Logarithmic Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-logarithmic)

(ert-deftest test-casual-logarithmic-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-logarithmic-menu
   '(
     ("l" ((float 109663315843 -8)) (float 7 0))
     ("e" ((float 7 0)) (float 109663315843 -8))
     ("L" ((float 100 0)) (float 2 0))
     ;; (read-kbd-macro "M-l") evals to [134217836]
     ([134217836] (8 2) 3)
     ;; (read-kbd-macro "M-e") evals to [134217829]
     ([134217829] (3) (float 190855369232 -10))))
  (casualt-breakdown t))





(provide 'test-casual-logarithmic)
;;; test-casual-logarithmic.el ends here
