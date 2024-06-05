;;; test-casual-calc-variables.el --- Test Casual Variables  -*- lexical-binding: t; -*-

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
(require 'casual-calc-variables)

(ert-deftest test-casual-calc-variable-crud-tmenu ()
  (casualt-setup)

  (calc-push-list '(25))
  (funcall 'casual-calc-variable-crud-tmenu)
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

(ert-deftest test-casual-calc-variable-crud-tmenu2 ()
  (casualt-setup)
  (let ((test-vectors '(("sq" . casual-calc--calc-store)
                        ("rq" . casual-calc--calc-recall)
                        ("cq" . casual-calc--calc-unstore)
                        ("e" . casual-calc--calc-edit-variable)
                        ("oq" . casual-calc--calc-copy-variable)
                        ("xq" . casual-calc--calc-store-exchange)
                        ("pq" . casual-calc--calc-permanent-variable)
                        ("iq" . casual-calc--calc-insert-variables))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-variable-crud-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(provide 'test-casual-calc-variables)
;;; test-casual-calc-variables.el ends here
