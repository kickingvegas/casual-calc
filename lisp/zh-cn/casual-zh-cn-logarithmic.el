;;; casual-zh-cn-logarithmic.el --- Casual Logarithmic Menu  -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-logarithmic-menu ()
  "对数函数。"
  ["对数函数"
   :pad-keys t
   ("l" "𝑙𝑛" calc-ln :transient nil)
   ("e" "𝑒^𝑥" calc-exp :transient nil)
   ("L" "𝑙𝑜𝑔𝟣𝟢" calc-log10 :transient nil)
   ("M-l" "𝑙𝑜𝑔" calc-log :transient nil)
   ("M-e" "𝑒^𝑥 - 𝟣" calc-expm1 :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销栈" calc-undo :transient t)])


(provide 'casual-zh-cn-logarithmic)
;;; casual-zh-cn-logarithmic.el ends here
