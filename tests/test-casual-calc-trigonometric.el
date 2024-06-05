;;; test-casual-calc-trigonometric.el --- Casual Trig Menu Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords:

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
(require 'casual-calc-trigonometric)

(ert-deftest test-casual-calc-trig-tmenu ()
  (casualt-setup)
  (calc-degrees-mode 1)
  (casualt-run-menu-input-testcases
   'casual-calc-trig-tmenu
   '(("s" (90) 1)
     ("c" (0) 1)
     ("t" (45) (float 1 0))
     ("S" (1) 90)
     ("C" (1) 0)
     ("T" (1) 45)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-hyperbolic-trig-tmenu ()
  (casualt-setup)
  (calc-degrees-mode 1)
  (casualt-run-menu-input-testcases
   'casual-calc-hyperbolic-trig-tmenu
   '(("s" ((float 88137358702 -11)) (float 1 0))
     ("c" ((float 0 0)) (float 1 0))
     ("t" ((float 33 -2)) (float 318520776903 -12))
     ("S" ((float 1 0)) (float 88137358702 -11))
     ("C" ((float 1 0)) (float 0 0))
     ("T" ((float 318520776903 -12)) (float 33 -2))))
  (casualt-breakdown t))


(provide 'test-casual-calc-trigonometric)
;;; test-casual-calc-trigonometric.el ends here
