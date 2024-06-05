;;; test-casual-calc-units.el --- Casual Unit Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-units)


(ert-deftest test-casual-calc-units-tmenu ()
  (casualt-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-units-tmenu
   '(("ckg" ((* 2 (var lb var-lb))) (* (float 90718474 -8) (var kg var-kg)))
     ("tdegC" ((* 57 (var degF var-degF)))
      (* (float 138888888889 -10) (var degC var-degC)))
     ("b" ((var km var-km)) (* 1000 (var m var-m)))
     ("r" ((* 100 (var km var-km))) 100)
     ("x" ((* 100 (var km var-km))) (var km var-km))))
  ;; TODO: test "v"
  (casualt-breakdown t))


(provide 'test-casual-calc-units)
;;; test-casual-calc-units.el ends here
