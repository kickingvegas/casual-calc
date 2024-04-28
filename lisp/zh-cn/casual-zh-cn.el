;;; casual-zh-cn.el --- Transient UI for Calc              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools
;; Version: 1.5.0
;; Package-Requires: ((emacs "29.1"))

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

;; Casual is an opinionated Transient-based porcelain for Emacs Calc.

;; INSTALLATION
;; (require 'casual-zh-cn)
;; (define-key calc-mode-map (kbd "C-o") 'casual-zh-cn-main-menu)

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)
(require 'casual-calc)
(require 'casual-version)
(require 'casual-zh-cn-binary)
(require 'casual-zh-cn-complex)
(require 'casual-zh-cn-conversion)
(require 'casual-logarithmic)
(require 'casual-zh-cn-random)
(require 'casual-zh-cn-rounding)
(require 'casual-zh-cn-settings)
(require 'casual-zh-cn-time)
(require 'casual-zh-cn-trigonometric)
(require 'casual-zh-cn-units)
(require 'casual-zh-cn-vector)
(require 'casual-zh-cn-graphics)
(require 'casual-zh-cn-trail)
(require 'casual-zh-cn-stack)
(require 'casual-zh-cn-financial)
(require 'casual-zh-cn-symbolic)
(require 'casual-zh-cn-variables)
(require 'casual-zh-cn-logarithmic)

;; Menus
;;;###autoload (autoload 'casual-zh-cn-main-menu "casual" nil t)
(transient-define-prefix casual-zh-cn-main-menu ()
  "Casua 主菜单。"
  [["Casua "
    :pad-keys t
    ("&" "倒数 1/𝑥" casual-calc-inv :transient nil)
    ("Q" "平方根 √" casual-calc-sqrt :transient nil)
    ("n" "正负 +∕−" casual-calc-change-sign :transient nil)
    ("^" "乘方 𝑦^𝑥" casual-calc-power :transient nil)
    ("=" "等于 =" casual-calc-evaluate :transient nil)]
   [""
    ("A" "绝对值 |𝑥|" casual-calc-abs :transient nil)
    ("!" "阶乘 !" casual-calc-factorial :transient nil)
    ("%" "百分比 ٪" casual-calc-percent :transient nil)
    ("D" "百分比变化 Δ%" casual-calc-percent-change :transient nil)]
   ["常数"
    ("p" "𝜋" casual-calc-pi :transient nil)
    ("e" "𝑒" casual--e-constant :transient nil)]
   ["设置"
    :pad-keys t
    ("m" "模式、显示、角度›" casual-zh-cn-modes-menu :transient nil)
    ("M-s" "堆栈›" casual-zh-cn-stack-display-menu :transient nil)
    ("M-t" "轨迹›" casual-zh-cn-trail-menu :transient nil)]]

  [["算术"
    :pad-keys t
    ("o" "舍入›" casual-zh-cn-rounding-menu :transient nil)
    ("c" "转换›" casual-zh-cn-conversions-menu :transient nil)
    ("T" "时间›" casual-zh-cn-time-menu :transient nil)
    ("i" "复数›" casual-zh-cn-complex-number-menu :transient nil)
    ("R" "随机数›" casual-zh-cn-random-number-menu :transient nil)]

   ["函数" ; 测试栈上是否有任何东西 calc-stack-size 0
    ("t" "三角函数›" casual-zh-cn-trig-menu :transient nil)
    ("l" "对数函数›" casual-zh-cn-logarithmic-menu :transient nil)
    ("b" "二进制›" casual-zh-cn-binary-menu :transient nil)
    ("v" "向量/矩阵›" casual-zh-cn-vector-menu :transient nil)
    ("u" "单位›" casual-zh-cn-units-menu :transient nil)
    ("f" "金融›" casual-zh-cn-financial-menu :transient nil)
    ("g" "图形›" casual-zh-cn-plot-menu :transient nil)
    ("a" "代数›" casual-zh-cn-symbolic-menu :transient nil)]

   ["堆栈"
    :pad-keys t
    ("s" "交换" casual--stack-swap :transient t)
    ("r" "滚动" casual--stack-roll-all :transient t)
    ("d" "删除" casual--stack-drop :transient t)
    ("C" "清除" casual--stack-clear :transient t)
    ("L" "最后" casual--stack-last :transient t)
    ("w" "复制" casual-calc-copy-as-kill :transient nil)
    ("z" "变量›" casual-zh-cn-variable-crud-menu :transient nil)]]

  [:class transient-row
          ;; 注意：主菜单不需要 C-g
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])


(provide 'casual-zh-cn)
;;; casual-zh-cn.el ends here
