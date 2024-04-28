;;; casual-zh-cn-trigonometric.el --- Casual Trigonometric Menus  -*- lexical-binding: t; -*-

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
(require 'casual-labels)
(require 'casual-zh-cn-angle-measure)

(transient-define-prefix casual-zh-cn-trig-menu ()
  "三角函数菜单。"
  [["三角函数"
    ("s" "sin" calc-sin :transient nil)
    ("c" "cos" calc-cos :transient nil)
    ("t" "tan" calc-tan :transient nil)]
   ["反三角函数"
    ("S" "arcsin" calc-arcsin :transient nil)
    ("C" "arccos" calc-arccos :transient nil)
    ("T" "arctan" calc-arctan :transient nil)]

   ["角度单位"
    ("a" casual-zh-cn-angle-measure-menu
     :description (lambda ()
                    (format "角度单位 (当前 %s)›"
                            (casual-angle-mode-label)))
     :transient nil)]]
  [("h" "双曲函数›" casual-zh-cn-hyperbolic-trig-menu :transient nil)]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-leave)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-hyperbolic-trig-menu ()
  "双曲三角函数菜单。"
  [["双曲函数"
    ("s" "sinh" calc-sinh :transient nil)
    ("c" "cosh" calc-cosh :transient nil)
    ("t" "tanh" calc-tanh :transient nil)]
   ["反双曲函数"
    ("S" "arcsinh" calc-arcsinh :transient nil)
    ("C" "arccosh" calc-arccosh :transient nil)
    ("T" "arctanh" calc-arctanh :transient nil)]]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-leave)
   ("U" "撤销堆栈" calc-undo :transient t)])



(provide 'casual-zh-cn-trigonometric)
;;; casual-zh-cn-trigonometric.el ends here
