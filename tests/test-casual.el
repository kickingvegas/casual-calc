;;; test-casual.el --- Tests for casual  -*- lexical-binding: t; -*-

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

;; Test suite for Casual package.

;; Note that tests are focused on verifying UI behavior and not intended to test
;; the `calc' package.

;; * Running the test suite

;; ** Command Line
;; Casual uses `ert' to manage its test suite. The test suite is intended to be
;; run in batch mode via a Makefile in the same directory as this file.
;; Invoke the following command in a shell to run the test suite.

;;     $ make tests

;; ** Interactive
;; Tests can be interactively run via `ert' by loading the following two files:
;; - casual-test-utils.el
;; - test-casual.el

;; Refer to `ert' documentation for more detail on how use it.

;;; Code:
(require 'ert)
(require 'casual)
(require 'casual-test-utils)

;;; Tests

(ert-deftest test-casual-main-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-main-menu
   '(("&" (2) (float 5 -1))
     ("Q" (9) 3)
     ("n" (5) -5)
     ("^" (2 3) 8)
     ("=" ((var pi var-pi)) (float 314159265359 -11))
     ("A" (-10) 10)
     ("!" (7) 5040)
     ("%" (8) (calcFunc-percent 8))
     ("D" (100 20) (calcFunc-percent -80))
     ("p" nil (float 314159265359 -11))
     ("e" nil (float 271828182846 -11))))
  (casualt-breakdown t))

(ert-deftest test-casual-main-menu-last ()
  (casualt-setup)
  (calc-push-list '(2 3))
  (funcall 'casual-main-menu)
  (execute-kbd-macro "^")
  (funcall 'casual-main-menu)
  (execute-kbd-macro "L")
  (should (and (= (calc-top) 3)
               (= (calc-top-n 2) 2)))
  (casualt-breakdown t))

(ert-deftest test-casual-variable-crud-menu ()
  (casualt-setup)

  (calc-push-list '(25))
  (funcall 'casual-variable-crud-menu)
  ;; test (s) calc-store
  (execute-kbd-macro "sfoo")
  (should (= (calc-var-value 'var-foo) 25))
  (calc-pop-stack (calc-stack-size))

  ;; test (r) calc-recall
  (execute-kbd-macro "rfoo")
  (should (= (calc-top) 25))

  ;; test (o) calc-copy-variable
  (execute-kbd-macro "ofoojane")
  (should (= (calc-var-value 'var-jane) 25))

  ;; test (c) calc-unstore
  (execute-kbd-macro "cfoo")
  (should (not (calc-var-value 'var-foo)))

  ;; test (x) calc-store-exchange
  (calc-push-list '(32))
  (execute-kbd-macro "xjane")
  (should (= (calc-var-value 'var-jane) 32))

  ;; TODO: punting on calc-edit-variable
  ;; TODO: punting on calc-permanent-variable
  ;; TODO: punting on calc-insert-variables
  (casualt-breakdown t))

(provide 'test-casual)
;;; test-casual.el ends here
