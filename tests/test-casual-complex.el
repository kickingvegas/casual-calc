;;; test-casual-complex.el --- Casual Complex Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-complex)

(ert-deftest test-casual-complex-number-menu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-complex-number-menu
   '(("r" ((cplx 2 3)) 2)
     ("i" ((cplx 2 3)) 3)
     ("c" ((cplx 2 3)) (cplx 2 -3))
     ("a" ((cplx 2 3)) (float 56309932474 -9))))
  (casualt-breakdown t))

(provide 'test-casual-complex)
;;; test-casual-complex.el ends here
