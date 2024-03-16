;;; test-casual-settings.el --- Casual Settings Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-settings)

(ert-deftest test-casual-complex-format-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-complex-format-menu
   '(("i" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'i)))))
     ("j" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'j)))))
     ("c" () (lambda () (calc-slow-wrapper (should (not calc-complex-format)))))))
  (casualt-breakdown t))


(ert-deftest test-casual-float-format-menu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-float-format-menu
   '(("s" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'sci)))))
     ("f3" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'fix)))))
     ("e" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'eng)))))
     ("n" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'float)))))
     ))
  (casualt-breakdown t))


(provide 'test-casual-settings)
;;; test-casual-settings.el ends here
