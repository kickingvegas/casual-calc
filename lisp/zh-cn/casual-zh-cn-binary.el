;;; casual-zh-cn-binary.el --- Casual Binary Menu          -*- lexical-binding: t; -*-

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
(require 'casual-zh-cn-radix)

(transient-define-prefix casual-zh-cn-binary-menu ()
  "Casua 二进制功能菜单。"
  ["二进制功能"
   ["操作符"
    ("a" "与" calc-and :transient nil)
    ("o" "或" calc-or :transient nil)
    ("x" "异或" calc-xor :transient nil)
    ("-" "取反" calc-diff :transient nil)
    ("!" "非" calc-not :transient nil)]
   ["位移"
    :pad-keys t
    ("l" "二进制左移" calc-lshift-binary :transient t)
    ("r" "二进制右移" calc-rshift-binary :transient t)
    ("M-l" "算术左移" calc-lshift-arith :transient t)
    ("M-r" "算术右移" calc-rshift-arith :transient t)
    ("C-r" "循环二进制" calc-rotate-binary :transient t)]
   ["实用工具"
    ("R" casual-zh-cn-radix-menu
     :description (lambda ()
                    (format "进制（当前 %s）›" (casual-number-radix-label)))
     :transient nil)
    ("z" "前导零" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros "前导零"))
     :transient nil)
    ("w" "设置字大小…" calc-word-size :transient nil)
    ("u" "解包位" calc-unpack-bits :transient nil)
    ("p" "打包位" calc-pack-bits :transient nil)]]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])


(provide 'casual-zh-cn-binary)
;;; casual-zh-cn-binary.el ends here
