;;; test-casual-angle-measure.el --- Casual Angle Measure Tests  -*- lexical-binding: t; -*-

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
(require 'casual-angle-measure)

(ert-deftest test-casual-angle-measure-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-angle-measure-menu
   '(("r" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'rad)))))
     ("h" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'hms)))))
     ("d" () (lambda () (calc-slow-wrapper (should (eq calc-angle-mode 'deg)))))))
  (casualt-breakdown t))

(provide 'test-casual-angle-measure)
;;; test-angle-measure.el ends here
