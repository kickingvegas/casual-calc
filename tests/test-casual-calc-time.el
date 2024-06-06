;;; test-casual-calc-time.el --- Casual Time Menu Tests   -*- lexical-binding: t; -*-

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
(require 'casual-calc-time)

(ert-deftest test-casual-calc-time-tmenu ()
  (casualt-setup)
  (casualt-run-menu-assert-testcases
   'casual-calc-time-tmenu
   '(("n" () (lambda () (should (math-floatp (calc-top)))))))

  (casualt-run-menu-input-testcases
   'casual-calc-time-tmenu
   '(("i" ((date (float 738944382662 -6))) (date (float 738973382662 -6)))
     ("u" ((date (float 738973382662 -6))) 1711642262)
     ("+" ((date (float 738973382662 -6)) 14) (date (float 738993382662 -6)))
     ("-" ((date (float 738973382662 -6)) 14) (date (float 738953382662 -6)))))

  (casualt-breakdown t))

(ert-deftest test-casual-calc-first-day-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-first-day-tmenu
   '(("w" ((date (float 738953382662 -6))) (date 738948))
     ("m" ((date (float 738953382662 -6))) (date 738946))
     ("y" ((date (float 738953382662 -6))) (date 738886))))
  (casualt-breakdown t))


(provide 'test-casual-calc-time)
;;; test-casual-calc-time.el ends here
