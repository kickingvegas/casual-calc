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

(ert-deftest test-casual-modes-menu ()
  (casualt-setup)
  (let ((test-vectors '(
                        ;; ("Aq" . calc-algebraic-mode)
                        ;; ("zq" . calc-leading-zeros)
                        ;; ("Fq" . calc-frac-mode)
                        ;; ("sq" . calc-symbolic-mode)
                        ;; ("pq" . calc-polar-mode)
                        ("cq" . casual-complex-format-menu)
                        ;; ("Pq" . calc-precision)
                        ("Iq" . casual-calc-infinite-mode)
                        ;; ("Sq" . calc-save-modes)
                        ("aq" . casual-angle-measure-menu)
                        ("Rq" . casual-radix-menu)
                        ("fq" . casual-float-format-menu)
                        ;; ("gq" . calc-group-digits)
                        ;; (",q" . calc-group-char)
                        ;; (".q" . calc-point-char)
                        ;; ("Hq" . calc-hms-notation)
                        ;; ("C-M-r" . calc-reset)
                        ("Oq" . casual-open-settings-file)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-modes-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

;; TODO: stubbed in for later
;; (ert-deftest casual-complex-format-menu ()
;;   (casualt-setup)
;;   (let ((test-vectors '(
;;                         ;; ("c" . calc-complex-notation)
;;                         ;; ("i" . calc-i-notation)
;;                         ;; ("j" . calc-j-notation)
;;                         )))
;;     (casualt-suffix-testbench-runner test-vectors
;;                                      #'casual-complex-format-menu
;;                                      '(lambda () (random 5000))))
;;   (casualt-breakdown t))

;; TODO: stubbed in for later
;; (ert-deftest casual-float-format-menu ()
;;   (casualt-setup)
;;   (let ((test-vectors '(
;;                         ;; ("n" . calc-normal-notation)
;;                         ;; ("f" . calc-fix-notation)
;;                         ;; ("s" . calc-sci-notation)
;;                         ;; ("e" . calc-eng-notation)]
;;                         )))
;;     (casualt-suffix-testbench-runner test-vectors
;;                                      #'casual-float-format-menu
;;                                      '(lambda () (random 5000))))
;;   (casualt-breakdown t))

(provide 'test-casual-settings)
;;; test-casual-settings.el ends here
