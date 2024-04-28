;;; casual-zh-cn-variables.el --- Casual Variable Menu     -*- lexical-binding: t; -*-

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
(require 'casual-calc)

(transient-define-prefix casual-zh-cn-variable-crud-menu ()
  "存储变量操作菜单。
此菜单提供存储、调用、清除和编辑变量的操作。"
  ["变量操作"
   ("s" "存储 (𝟣:)…" casual-calc-store :transient t)
   ("r" "调用…" casual-calc-recall :transient t)
   ("c" "清除…" casual-calc-unstore :transient t)
   ("e" "编辑…" casual-calc-edit-variable :transient nil)
   ("o" "复制到其他变量…" casual-calc-copy-variable :transient t)
   ("x" "交换 (𝟣:) 和变量…" casual-calc-store-exchange :transient t)
   ("p" "持久化…" casual-calc-permanent-variable :transient t)
   ("O" "打开计算设置文件" casual-open-settings-file :transient nil)
   ("i" "插入变量到缓冲区…" casual-calc-insert-variables :transient t)]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])


(provide 'casual-zh-cn-variables)
;;; casual-zh-cn-variables.el ends here
