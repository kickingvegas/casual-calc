;;; casual-zh-cn-complex.el --- Casual Complex Menu        -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'transient)

(transient-define-prefix casual-zh-cn-complex-number-menu ()
  "Casua 复数功能菜单。"
  ["复数"
   ("r" "实部" calc-re :transient nil)
   ("i" "虚部" calc-im :transient nil)
   ("c" "复共轭" calc-conj :transient nil)
   ("a" "幅角" calc-argument :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])


(provide 'casual-zh-cn-complex)
;;; casual-zh-cn-complex.el ends here
