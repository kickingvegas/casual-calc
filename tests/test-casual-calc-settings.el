;;; test-casual-calc-settings.el --- Casual Settings Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-test-utils)
(require 'casual-calc-settings)

(ert-deftest test-casual-calc-complex-format-tmenu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-calc-complex-format-tmenu
   '(("i" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'i)))))
     ("j" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'j)))))
     ("c" () (lambda () (calc-slow-wrapper (should (not calc-complex-format)))))))
  (casualt-breakdown t))


(ert-deftest test-casual-calc-float-format-tmenu ()
  (casualt-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-calc-float-format-tmenu
   '(("s" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'sci)))))
     ("f3" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'fix)))))
     ("e" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'eng)))))
     ("n" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'float)))))
     ))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-modes-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(
                        ;; ("Aq" . calc-algebraic-mode)
                        ;; ("zq" . calc-leading-zeros)
                        ;; ("Fq" . calc-frac-mode)
                        ;; ("sq" . calc-symbolic-mode)
                        ;; ("pq" . calc-polar-mode)
                        ("cq" . casual-calc-complex-format-tmenu)
                        ;; ("Pq" . calc-precision)
                        ("Iq" . casual-calc--calc-infinite-mode)
                        ;; ("Sq" . calc-save-modes)
                        ("aq" . casual-calc-angle-measure-tmenu)
                        ("Rq" . casual-calc-radix-tmenu)
                        ("fq" . casual-calc-float-format-tmenu)
                        ;; ("gq" . calc-group-digits)
                        ;; (",q" . calc-group-char)
                        ;; (".q" . calc-point-char)
                        ;; ("Hq" . calc-hms-notation)
                        ;; ("C-M-r" . calc-reset)
                        ("Oq" . casual-calc-open-settings-file)
                        )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-modes-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

;; TODO: stubbed in for later
;; (ert-deftest casual-calc-complex-format-tmenu ()
;;   (casualt-setup)
;;   (let ((test-vectors '(
;;                         ;; ("c" . calc-complex-notation)
;;                         ;; ("i" . calc-i-notation)
;;                         ;; ("j" . calc-j-notation)
;;                         )))
;;     (casualt-suffix-testbench-runner test-vectors
;;                                      #'casual-calc-complex-format-tmenu
;;                                      '(lambda () (random 5000))))
;;   (casualt-breakdown t))

;; TODO: stubbed in for later
;; (ert-deftest casual-calc-float-format-tmenu ()
;;   (casualt-setup)
;;   (let ((test-vectors '(
;;                         ;; ("n" . calc-normal-notation)
;;                         ;; ("f" . calc-fix-notation)
;;                         ;; ("s" . calc-sci-notation)
;;                         ;; ("e" . calc-eng-notation)]
;;                         )))
;;     (casualt-suffix-testbench-runner test-vectors
;;                                      #'casual-calc-float-format-tmenu
;;                                      '(lambda () (random 5000))))
;;   (casualt-breakdown t))

(provide 'test-casual-calc-settings)
;;; test-casual-calc-settings.el ends here
