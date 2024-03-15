;;; test-casual-time.el --- Casual Time Menu Tests   -*- lexical-binding: t; -*-

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
(require 'casual-time)

(ert-deftest test-casual-time-menu ()
  (casualt-setup)
  (casualt-run-menu-assert-testcases
   'casual-time-menu
   '(("n" () (lambda () (should (math-floatp (calc-top)))))))

  (casualt-run-menu-input-testcases
   'casual-time-menu
   '(("i" ((date (float 738944382662 -6))) (date (float 738973382662 -6)))
     ("u" ((date (float 738973382662 -6))) 1711642262)
     ("+" ((date (float 738973382662 -6)) 14) (date (float 738993382662 -6)))
     ("-" ((date (float 738973382662 -6)) 14) (date (float 738953382662 -6)))))

  (casualt-breakdown t))

(ert-deftest test-casual-first-day-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-first-day-menu
   '(("w" ((date (float 738953382662 -6))) (date 738948))
     ("m" ((date (float 738953382662 -6))) (date 738946))
     ("y" ((date (float 738953382662 -6))) (date 738886))))
  (casualt-breakdown t))


(provide 'test-casual-time)
;;; test-casual-time.el ends here
