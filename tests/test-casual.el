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

;;

;;; Code:

(require 'ert)
(require 'casual)

(ert-deftest test-casual-angle-mode-label ()
  "Test "
  (setq calc-angle-mode 'deg)
  (should (equal (casual-angle-mode-label) "Degrees"))
  (setq calc-angle-mode 'rad)
  (should (equal (casual-angle-mode-label) "Radians"))
  (setq calc-angle-mode 'hms)
  (should (equal (casual-angle-mode-label) "hms")))

(defun casualt-setup ()
  "Casual menu test setup function."
  (calc-create-buffer))

(defun casualt-breakdown (&optional clear)
  "Casual menu test breakdown function, if CLEAR is non-nil then clear stack."
  (if clear
      (calc-pop-stack (calc-stack-size))))

(defun casualt-menu-testcase (menu keyseq init value)
  (funcall menu)
  (if (and init (listp init))
      (calc-push-list init))
  (execute-kbd-macro keyseq)
  (should (equal (calc-top) value)))

(defun casualt-menu-input-testcases (menu testcases)
  ""
  (mapc (lambda (x)
          (casualt-menu-testcase menu
                                 (car x)
                                 (nth 1 x)
                                 (nth 2 x)))
        testcases))

(ert-deftest test-casual-main-menu ()
  (casualt-setup)

  (casualt-menu-input-testcases
   'casual-main-menu
   '(("&" (2) (float 5 -1))
     ("Q" (9) 3)
     ("n" (5) -5)
     ("^" (2 3) 8)
     ("A" (-10) 10)
     ("!" (7) 5040)
     ("%" (8) (calcFunc-percent 8))
     ("d" (100 20) (calcFunc-percent -80))
     ("p" nil (float 314159265359 -11))
     ("e" nil (float 271828182846 -11))))
  (casualt-breakdown t))


(provide 'test-casual)
;;; test-casual.el ends here
