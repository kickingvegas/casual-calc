;;; casual-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'transient)

(defun casualt-setup ()
  "Casual menu test setup function."
  (calc-create-buffer))

(defun casualt-breakdown (&optional clear)
  "Casual menu test breakdown function, if CLEAR is non-nil then clear stack."
  (if clear
      (calc-pop-stack (calc-stack-size)))
  (calc-quit nil))

(defun casualt-menu-input-testcase (menu keyseq init value)
  "Testcase that compares VALUE with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
VALUE - value to be compared with top of the stack"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (should (equal (calc-top) value)))

(defun casualt-menu-assert-testcase (menu keyseq init assert)
  "Testcase to call ASSERT with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
ASSERT - assert function that calls `should'"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (funcall assert))

(defun casualt-run-menu-input-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instancesg\n
A testcase is a list with the following schema - (keyseq init value)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
value - value to be compared with top of the stack\n
A testcase is used as input to `casualt-menu-input-testcase'."
  (mapc (lambda (x)
          (casualt-menu-input-testcase menu
                                       (car x)
                                       (nth 1 x)
                                       (nth 2 x)))
        testcases))

(defun casualt-run-menu-assert-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instances\n
A testcase is a list with the following schema - (keyseq init assert)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
assert - assert function that calls `should'\n
A testcase is used as input to `casualt-menu-assert-testcase'."
  (mapc (lambda (x)
          (casualt-menu-assert-testcase menu
                                        (car x)
                                        (nth 1 x)
                                        (nth 2 x)))
        testcases))

(provide 'casual-test-utils)
;;; casual-test-utils.el ends here
