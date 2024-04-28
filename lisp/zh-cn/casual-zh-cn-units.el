;;; casual-zh-cn-units.el --- Casual Units                 -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-units-menu ()
  "单位转换菜单。"
  ["单位转换"
   ("c" "转换" calc-convert-units :transient nil)
   ("t" "温度转换" calc-convert-temperature :transient nil)
   ("b" "转换为基本单位" calc-base-units :transient nil)
   ;; TODO: 显示当前的自动范围状态
   ;;("a" "自动范围" calc-autorange-units :transient nil)
   ("r" "移除单位" calc-remove-units :transient nil)
   ("x" "提取单位" calc-extract-units :transient nil)
   ("v" "查看单位表" calc-view-units-table :transient nil)]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])



(provide 'casual-zh-cn-units)
;;; casual-zh-cn-units.el ends here
