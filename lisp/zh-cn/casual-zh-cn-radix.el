;;; casual-zh-cn-radix.el --- Casual Radix Menu            -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-radix-menu ()
  "进制转换菜单。"
  ["进制转换 (𝑛 是栈顶元素)"
   ("0" "十进制" calc-decimal-radix :transient nil)
   ("2" "二进制" calc-binary-radix :transient nil)
   ("8" "八进制" calc-octal-radix :transient nil)
   ("6" "十六进制" calc-hex-radix :transient nil)
   ("n" "其他进制 𝑛" calc-radix :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销栈" calc-undo :transient t)])


(provide 'casual-zh-cn-radix)
;;; casual-zh-cn-radix.el ends here
