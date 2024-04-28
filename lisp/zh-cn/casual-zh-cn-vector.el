;;; casual-zh-cn-vector.el --- Casual Vector Menu          -*- lexical-binding: t; -*-

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
(require 'casual-predicates)

(transient-define-prefix casual-zh-cn-vector-menu ()
  "向量和矩阵函数顶层菜单。"
  ["向量和矩阵函数（索引偏移为 1）\n"
   ["类别"
    ("b" "构建›" casual-zh-cn-vector-building-menu :transient nil)
    ("a" "算术›" casual-zh-cn-vector-arithmetic-menu :transient nil)
    ("s" "统计›" casual-zh-cn-statistics-menu :transient nil)
    ("S" "集合运算›" casual-zh-cn-set-operations-menu :transient nil)
    ("m" "映射、约简、应用›" casual-zh-cn-map-and-reduce-menu :transient nil)]

   ["操作"
    :pad-keys t
    ("l" "长度" calc-vlength :transient t)
    ("t" "转置" calc-transpose :transient t)
    ("v" "反转" calc-reverse-vector :transient t)
    ("o" "排序" calc-sort :transient t)
    ("d" "去重" calc-remove-duplicates :transient t)]

   ["提取和打包"
    ("r" "提取行…" calc-mrow :transient nil)
    ("c" "提取列…" calc-mcol :transient nil)
    ("p" "打包（𝑛）" calc-pack :transient nil)
    ("u" "解包" calc-unpack :transient nil)]]

  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-vector-building-menu ()
  "向量构建函数菜单。"
  ["向量构建（索引偏移为 1，𝑛 是提示值）\n"
   ["构建"
    ("|" "连接" calc-concat :transient nil)
    ("i" "索引（1..𝑛）…" calc-index :transient nil)
    ("e" "枚举区间" calc-set-enumerate :transient nil)
    ("I" "单位矩阵 𝑛…" calc-ident :transient nil)
    ("d" "对角矩阵（𝟣:）" calc-diag :transient nil)
    ("b" "构建向量 𝑛…" calc-build-vector :transient nil)]

   ["操作"
    ("t" "转置" calc-transpose :transient nil)
    ("r" "反转" calc-reverse-vector :transient nil)
    ("a" "向量排列" calc-arrange-vector :transient nil)
    ("s" "排序" calc-sort :transient nil)
    ("p" "去重" calc-remove-duplicates :transient nil)]

   ["其他"
    ("l" "长度" calc-vlength :transient nil)
    ("c" "向量计数" calc-vector-count :transient nil)
    ("f" "查找向量（𝟣:）" calc-vector-find :transient nil)
    ("h" "直方图" calc-histogram :transient nil)]]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-vector-arithmetic-menu ()
  "向量算术函数菜单。"
  [["算术（索引偏移为 1）\n"
    ("t" "共轭转置" calc-conj-transpose :transient nil)
    ("A" "Frobenius 范数（|𝑛|）" calc-abs :transient nil)
    ("r" "行范数" calc-rnorm :transient nil)
    ("c" "列范数" calc-cnorm :transient nil)
    ("p" "RH 叉乘" calc-cross :inapt-if-not casual-crossp :transient nil)
    ("k" "Kronecker 乘积" calc-kron :inapt-if-not casual-matrixmultp :transient nil)]
   ["方阵"
    ("&" "求逆" calc-inv :inapt-if-not casual-square-matrixp :transient nil)
    ("d" "行列式" calc-mdet :inapt-if-not casual-square-matrixp  :transient nil)
    ("l" "LU 分解" calc-mlud :inapt-if-not casual-square-matrixp :transient nil)
    ("T" "迹" calc-mtrace :inapt-if-not casual-square-matrixp :transient nil)]]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

;; TODO: add Transient prefix arguments n
(transient-define-prefix casual-zh-cn-statistics-menu ()
  "统计函数菜单。"
  ["统计（索引偏移为 1，𝑛 为栈上的值）\n"
   ["平均值和误差"
    ("c" "向量计数" calc-vector-count :transient nil)
    ("s" "求和" calc-vector-sum :transient nil)
    ("x" "最大值" calc-vector-max :transient nil)
    ("m" "平均值" calc-vector-mean :transient nil)
    ("h" "直方图…" casual-calc-histogram :transient nil)
    ("e" "平均误差" calc-vector-mean-error :transient nil)
    ("M" "中位数" calc-vector-median :transient nil)
    ("H" "调和平均数" calc-vector-harmonic-mean :transient nil)
    ("g" "几何平均数" calc-vector-geometric-mean :transient nil)]

   ["偏差和方差"
    ("r" "均方根" calc-vector-rms :transient nil)
    ("1" "标准差" calc-vector-sdev :transient nil)
    ("2" "总体标准差" calc-vector-pop-sdev :transient nil)
    ("3" "方差" calc-vector-variance :transient nil)
    ("4" "总体方差" calc-vector-pop-variance :transient nil)]

   ["配对样本统计" ; 两个大小相同的向量的断言
    ("5" "协方差" calc-vector-covariance :transient nil)
    ("6" "总体协方差" calc-vector-pop-covariance :transient nil)
    ("7" "相关系数" calc-vector-correlation :transient nil)]]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-set-operations-menu ()
  "集合函数菜单。"
  ["集合运算"
    ("d" "去重" calc-remove-duplicates :transient nil)
    ("u" "并集" calc-set-union :transient nil)
    ("i" "交集" calc-set-intersect :transient nil)
    ("-" "差集" calc-set-difference :transient nil)
    ("x" "xor" calc-set-xor :transient nil)
    ("~" "补集" calc-set-complement :transient nil)
    ("#" "基数" calc-set-cardinality :transient nil)]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-map-and-reduce-menu ()
  "函数式操作（map、reduce、apply）菜单。"
  ["函数操作"
   ("m" "map" calc-map :transient nil)
   ("r" "reduce" calc-reduce :transient nil)
   ("a" "apply" calc-apply :transient nil)
   ("A" "accumulate" calc-accumulate :transient nil)]
  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

;;; Wrapped Functions

(defun casual-calc-histogram ()
  "Build histogram of (1:).
\nGiven a vector data set in (1:), this command will prompt the
user for a bin specification vector, where each element of the
vector is a center point of a bin. For example, if the entered
bin vector is '[a, b, c, …]' then the bin ranges will be computed
as (-inf, (a+b)/2], ((a+b)/2, (b+c)/2], …

* Example

Start with the following data set of integer numbers from 1 to 100.

1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
    67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
    83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
    99, 100]

A histogram of the above data set where each bin is every 10
units requires this bin vector. Enter this when prompted:

[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

The result on the stack is:

1: [5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5]

* References - info node `(calc) Manipulating Vectors' -
`calc-histogram'"
  (interactive)
  (call-interactively #'calc-histogram))

(provide 'casual-zh-cn-vector)
;;; casual-zh-cn-vector.el ends here
