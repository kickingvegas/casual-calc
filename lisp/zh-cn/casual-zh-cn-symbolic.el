;;; casual-zh-cn-symbolic.el --- Casual Symbolic Menu      -*- lexical-binding: t; -*-

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
(require 'casual-zh-cn-graphics)
(require 'casual-calc)
(require 'casual-calc-algebra)
(require 'casual-zh-cn-variables)
(require 'casual-fileio)

(transient-define-prefix casual-zh-cn-symbolic-menu ()
  "计算代数菜单。
\n在此处可以进行以下操作：
- 操纵代数表达式
- 操纵多项式表达式
- 执行微积分
- 符号或数值求解表达式
- 拟合数据曲线
- 执行求和"
  ["计算代数"
   ["表达式操纵"
    :pad-keys t
    ("E" "简化" casual-calc-alg-evaluate :transient t)
    ("=" "求解变量" casual-calc-evaluate :transient t)
    ("m" "⋯›" casual-zh-cn-symbolic-manipulation-menu :transient nil)]
   [""
    :pad-keys t
    ("F" "公式›" casual-zh-cn-subformula-menu :transient nil)]]

  [["多项式"
    :pad-keys t
    ("f" "因式分解" casual-calc-factor :transient t)
    ("e" "展开" casual-calc-expand :transient t)
    ("p" "⋯›" casual-zh-cn-polynomial-menu :transient nil)]

   ["微积分"
    :pad-keys t
    ("d" "导数…" casual-calc-derivative :transient t)
    ("i" "积分…" casual-calc-integral :transient t)
    ("c" "⋯›" casual-zh-cn-calculus-menu :transient nil)]

   ["求解"
    ("s" "符号求解›" casual-zh-cn-solve-symbolic-menu :transient nil)
    ("n" "数值求解›" casual-zh-cn-solve-numeric-menu :transient nil)]]

  [""
   ["其他"
    ("C" "曲线拟合›" casual-zh-cn-curve-fit-menu :transient nil)
    ("S" "求和›" casual-zh-cn-summations-menu :transient nil)
    ("l" "等式与逻辑›" casual-zh-cn-symbolic-logic-menu :transient nil)
    ("g" "图形›" casual-zh-cn-plot-menu :transient nil)
    ("z" "变量›" casual-zh-cn-variable-crud-menu :transient nil)]

   ["设置"
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual--checkbox-label calc-algebraic-mode
                                            "代数模式"))
     :transient t)
    ("M" calc-symbolic-mode :description casual-symbolic-mode-label :transient t)
    ("a" casual-zh-cn-angle-measure-menu
     :description (lambda ()
                    (format "角度测量单位（当前设置为 %s）›"
                            (casual-angle-mode-label)))
     :transient t)]]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-subformula-menu ()
  "子公式菜单。
\n选择和编辑子公式的命令。"
  ["选择与导航"
   [("s" "选择" casual-calc-select-here :transient t)
    ("o" "选择一次" casual-calc-select-once :transient t)
    ("m" "更多" casual-calc-select-more :transient t)
    ("p" "上一个" casual-calc-select-previous :transient t)]

   [("u" "取消选择" casual-calc-unselect :transient t)
    ("c" "清除选择" casual-calc-clear-selections :transient t)
    ("l" "减少选择" casual-calc-select-less :transient t)
    ("n" "下一个" casual-calc-select-next :transient t)]]

  ["操纵"
   [("b" "← 交换" casual-calc-commute-left :transient t)
    ("d" "分配" casual-calc-sel-distribute :transient t)
    ("i" "隔离" casual-calc-sel-isolate :transient t)
    ("N" "否定" casual-calc-sel-negate :transient t)
    ("e" "⇄" casual-calc-sel-jump-equals :transient t)]

   [("f" "→ 交换" casual-calc-commute-right :transient t)
    ("M" "合并" casual-calc-sel-merge :transient t)
    ("&" "反转" casual-calc-sel-invert :transient t)
    ("=" "=" casual-calc-sel-evaluate :transient t)]]

  ["编辑"
   [("`" "编辑" casual-calc-edit-selection :transient nil)
    ("C" "复制" casual-calc-copy-selection :transient t)]

   [("'" "替换" casual-calc-enter-selection :transient nil)
    ("D" "删除" casual-calc-del-selection :transient t)]]

  ;; ["两侧"
  ;;  ("*" "乘以…" casual-calc-sel-mult-both-sides :transient t)
  ;;  ("/" "除以…" casual-calc-sel-div-both-sides :transient t)
  ;;  ("+" "加上…" casual-calc-sel-add-both-sides :transient t)
  ;;  ("-" "减去…" casual-calc-sel-sub-both-sides :transient t)
  ;;  ]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-symbolic-manipulation-menu ()
  "符号操纵菜单。
用于操作符号表达式的命令。"
  ["符号操纵"
   ("E" "简化" casual-calc-alg-evaluate :transient t)
   ("=" "评估变量" casual-calc-evaluate :transient t)
   ("e" "展开公式" casual-calc-expand-formula :transient t)
   ("m" "映射方程" casual-calc-map-equation :transient t)
   ("s" "替换" casual-calc-substitute :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])


(transient-define-prefix casual-zh-cn-polynomial-menu ()
  "多项式菜单。
用于操作多项式表达式的命令。"
  ["多项式"
   ("f" "因式分解" casual-calc-factor :transient t)
   ("e" "展开" casual-calc-expand :transient t)
   ("c" "收集…" casual-calc-collect :transient t)
   ("a" "部分分式分解" casual-calc-apart :transient t)
   ("n" "归一化比值" casual-calc-normalize-rat :transient t)
   ("\\" "多项式除法" casual-calc-poly-div :transient t)
   ("%" "多项式求余" casual-calc-poly-rem :transient t)
   ("/" "多项式除法及求余" casual-calc-poly-div-rem :transient t)
   ("g" "多项式最大公因式" casual-calc-poly-gcd :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-calculus-menu ()
  "微积分菜单。
执行微积分的命令。"
  ["微积分"
   ("n" "数值积分…" casual-calc-num-integral :transient t)
   ("t" "泰勒展开…" casual-calc-taylor :transient t)
   ("d" "导数…" casual-calc-derivative :transient t)
   ("i" "定积分…" casual-calc-integral :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-solve-symbolic-menu ()
  "符号解菜单。
用于符号解代数表达式的命令。"
  ["符号解"
   ("s" "解…" casual-calc-solve-for :transient t)
   ("p" "多项式根…" casual-calc-poly-roots :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-solve-numeric-menu ()
  "数值解菜单。
用于数值解代数表达式的命令。"
  ["数值解"
   ("r" "找根" casual-calc-find-root :transient t)
   ("m" "找最小值…" casual-calc-find-minimum :transient t)
   ("x" "找最大值…" casual-calc-find-maximum :transient t)
   ("h" "Head" casual-calc-head :transient t)
   ("w" "Why" casual-calc-why :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-curve-fit-menu ()
  "曲线拟合菜单。
用于曲线拟合的命令。"
  ["曲线拟合"
   ("c" "拟合曲线" casual-calc-curve-fit :transient t)
   ("p" "多项式插值" casual-calc-poly-interp :transient t)
   ("o" "打开拟合数据…" casual-read-curvefit-data :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-summations-menu ()
  "求和菜单。
求和命令。"
  ["求和"
   ("s" "∑" casual-calc-summation :transient t)
   ("a" "交错求和" casual-calc-alt-summation :transient t)
   ("p" "∏" casual-calc-product :transient t)
   ("t" "制表" casual-calc-tabulate :transient t)]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-symbolic-logic-menu ()
  "符号逻辑菜单。
符号逻辑命令。"
  ["等式"
   [("=" "=" casual-calc-equal-to :transient t)
    ("l" "<" casual-calc-less-than :transient t)
    ("g" ">" casual-calc-greater-than :transient t)]

   [("n" "≠" casual-calc-not-equal-to :transient t)
    ("L" "≤" casual-calc-less-equal :transient t)
    ("G" "≥" casual-calc-greater-equal :transient t)]

   [("x" "移除比较符" casual-calc-remove-equal :transient t)]]

  [["运算符"
    ("!" "非 (!)" casual-calc-logical-not :transient t)
    ("&" "∧ (&&)" casual-calc-logical-and :transient t)
    ("|" "∨ (||)" casual-calc-logical-or :transient t)]

   ["杂项"
    ("e" "∈" casual-calc-in-set :transient t)
    ("i" "如果" casual-calc-logical-if :transient t)]]

  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销" calc-undo :transient t)])


(provide 'casual-zh-cn-symbolic)
;;; casual-zh-cn-symbolic.el ends here
