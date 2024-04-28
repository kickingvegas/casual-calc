;;; casual-zh-cn-graphics.el --- Casual Graphics Functions  -*- lexical-binding: t; -*-

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
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)
(require 'casual-fileio)

;; Push Example Functions
(defun casual--push-natural-interval-0-100 ()
  "Push inclusive natural interval [0..100] onto stack."
  (interactive)
  (calc-push-list '((intv 3 0 100))))

(defun casual--push-natural-interval-0-360 ()
  "Push inclusive natural interval [0..360] onto stack.
Convert interval to radians to obtain 2𝜋."
  (interactive)
  (calc-push-list '((intv 3 0 360))))

(defun casual--push-float-interval-0-100 ()
  "Push inclusive float interval 0.0 to 100.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float 0 0) (float 1 2)))))

(defun casual--push-float-interval-1-symmetric ()
  "Push inclusive float interval -1.0 to 1.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float -1 0) (float 1 0)))))

(defun casual--push-sin ()
  "Push sin(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-sin (var x var-x)))))

(defun casual--push-cos ()
  "Push cos(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-cos (var x var-x)))))

(defun casual--push-tan ()
  "Push tan(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-tan (var x var-x)))))

(defun casual--push-ln ()
  "Push ln(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-ln (var x var-x)))))

(defun casual--push-e-raised-to-x ()
  "Push e^x onto stack."
  (interactive)
  (calc-push-list '((^ (var e var-e) (var x var-x)))))

(defun casual--push-polynomial-order-2 ()
  "Push x^2 + 1 onto stack.
This can be used as a template formula to edit an order 2 polynomial."
  (interactive)
  (calc-push-list '((+ (^ (var x var-x) 2) 1))))

(defun casual--push-polynomial-order-3 ()
  "Push x^3 + x^2 + 1 onto stack.
This can be used as a template formula to edit an order 3 polynomial."
  (interactive)
  (calc-push-list '((+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1))))

;; Wrapped Calc Graphics Functions

(defun casual--graph-refresh-plot ()
  "Internal function to refresh the Gnuplot canvas.
This function is intended to be called after a calc-graph command
that changes the internal state of Gnuplot is called.  This way
the canvas is updated to support interactive usage.  Invokes
`calc-graph-plot' to do the actual work."
  (call-interactively #'calc-graph-plot))

(defun casual--graph-add ()
  "Add 2D curve to Gnuplot canvas.
\This function adds the curve specified by the stack
arguments (2:) and (1:), prompting for a name to assign the
curve.

2: x-axis specification
1: y-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification can be either a vector or an algebraic formula with a
single independent variable, typically 𝑥.

Invoking this multiple times will for each time generate a new curve on the same
canvas.  The last curve generated is referred to as the current curve.  The user
can invoke `casual--graph-juggle' to rotate to an arbitrary curve to make it
current.

The number of sample points used to plot a curve can be set by
calling `casual--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b] [c d] [e f]] where [a b],
  ;; [c d], [e f] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))


(defun casual--graph-add-equation ()
  "Add 2D algebraic equation to Gnuplot canvas.
\nThis function adds the curve specified in (1:), prompting the
user for an interval and then a name to assign the curve.

1: y-axis specification, typically an equation

The y-axis specification can be either a vector or an algebraic formula with a
single independent variable, typically 𝑥.

Invoking this multiple times will for each time generate a new curve on the same
canvas.  The last curve generated is referred to as the current curve.  The user
can invoke `casual--graph-juggle' to rotate to an arbitrary curve to make it
current.

The number of sample points used to plot a curve can be set by
calling `casual--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b] [c d] [e f]] where [a b],
  ;; [c d], [e f] are three separate points to plot.
  (interactive)

  (calc-push (math-read-expr (read-string "Plot Interval: ")))
  (calc-roll-down 2)
  (call-interactively #'calc-graph-add)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-add-3d ()
  "Add 3D curve to Gnuplot canvas.
This function adds the curve specified by the stack arguments 2: and 1:.

3: x-axis specification
2: y-axis specification
1: z-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification is typically a vector or an interval.
The z-axis specification can be either a vector or an algebraic formula with two
independent variables, typically 𝑥 and 𝑦.

The number of sample points used to plot a curve can be set by
calling `casual--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b c] [d e f] [g h i]] where
  ;; [a b c], [d e f], and [g h i] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add-3d)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-delete ()
  "Delete the current curve."
  (interactive)
  (call-interactively #'calc-graph-delete)
  (casual--graph-refresh-plot))

(defun casual--graph-hide ()
  "Hide the current curve."
  ;; TODO: for some reason this doesn't work as expected.
  (interactive)
  (call-interactively #'calc-graph-hide)
  (casual--graph-refresh-plot))

(defun casual--graph-num-points ()
  "Define number of sample points to plot for a curve.
Note: on some terminals (aqua, X11) setting this value is only
effective on a curve for the first time; subsequent changes to
this value are not honored."
  ;; TODO: apparently with aquaterm, when sample points are defined for a curve,
  ;; it can not be changed. Need to investigate if this is the same with other
  ;; terminals.
  (interactive)
  (call-interactively #'calc-graph-num-points)
  (casual--graph-refresh-plot))

;; (defun casual--graph-line-style ()
;;   (interactive)
;;   (let* ((linestyle (transient-arg-value "--linestyle=" (transient-args transient-current-command))))
;;          ;;(current-prefix-arg (if linestyle (string-to-number linestyle) nil)))
;;     ;;(call-interactively #'calc-graph-line-style)
;;     (if (not linestyle)
;;         (call-interactively #'calc-graph-line-style)
;;       (calc-graph-line-style (string-to-number linestyle)))
;;     (casual--graph-refresh-plot)))

(defun casual--graph-toggle-line-style ()
  "Toggle whether a line is rendered for the current curve.
Defining a line style is not yet supported in Casual."
  (interactive)
  (call-interactively #'calc-graph-line-style)
  (casual--graph-refresh-plot))

(defun casual--graph-toggle-point-style ()
  "Toggle whether points are rendered for the current curve.
Defining a point style is not yet supported in Casual."
  (interactive)
  (call-interactively #'calc-graph-point-style)
  (casual--graph-refresh-plot))


(defun casual--graph-juggle ()
  "Change the current curve by rotating through the set of added curves.
Note that direct selection of a curve is not yet supported in Casual."
  ;; OBSERVATION (kickingvegas): Calc's notion of juggling is an unfortunate
  ;; design decision because it imposes an unnatural means to access a curve.
  ;; More ideal would a curve abstraction that supports direct manipulation.
  (interactive)
  (call-interactively #'calc-graph-juggle)
  (casual--graph-refresh-plot))

(defun casual--graph-grid ()
  "Toggle canvas grid."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-grid)
  (casual--graph-refresh-plot))

(defun casual--graph-key ()
  "Toggle canvas legend of defined curves."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-key)
  (casual--graph-refresh-plot))

(defun casual--graph-border ()
  "Toggle canvas border."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-border)
  (casual--graph-refresh-plot))

(defun casual--graph-header ()
  "Set string for canvas title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-header)
  (casual--graph-refresh-plot))

(defun casual--graph-name ()
  "Set string for current curve name.
This string name is used in the canvas legend (key)."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-title-x ()
  "Set string for x-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-x)
  (casual--graph-refresh-plot))

(defun casual--graph-title-y ()
  "Set string for y-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-y)
  (casual--graph-refresh-plot))

(defun casual--graph-title-z ()
  "Set string for z-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-z)
  (casual--graph-refresh-plot))

(defun casual--graph-zero-x ()
  "Toggle solid line for y=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-x)
  (casual--graph-refresh-plot))

(defun casual--graph-zero-y ()
  "Toggle solid line for x=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-y)
  (casual--graph-refresh-plot))

(defun casual--graph-log-x ()
  "Toggle linear/log scaling for x-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-x)
  (casual--graph-refresh-plot))

(defun casual--graph-log-y ()
  "Toggle linear/log scaling for y-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-y)
  (casual--graph-refresh-plot))

(defun casual--graph-log-z ()
  "Toggle linear/log scaling for z-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-z)
  (casual--graph-refresh-plot))

;; Menus

(transient-define-prefix casual-zh-cn-plot-menu ()
  "常规绘图菜单。"
  ["图形"
   ["曲线"
    :pad-keys t
    ("a" "添加二维曲线" casual--graph-add :transient t)
    ("e" "添加二维方程" casual--graph-add-equation :transient t)
    ("A" "添加三维" casual--graph-add-3d :transient t)
    ("d" "删除" casual--graph-delete :transient t)
    ("N" "命名…" casual--graph-name :transient t)
    ("j" "折叠" casual--graph-juggle :transient t)
    ("s" "样式›" casual-zh-cn-curve-style-menu :transient nil)
    ("o" "打开绘图数据…" casual-read-plot-data :transient t)]
   ["画布"
    ("r" "重绘" calc-graph-plot :transient t)
    ("c" "清除" calc-graph-clear :transient t)
    ("n" "数据点数…" casual--graph-num-points :transient t)
    ("S" "样式›" casual-zh-cn-plot-options-menu :transient t)]
   ["工具"
    ("g" "设置›" casual-zh-cn-graph-settings-menu :transient nil)
    ("p" "打印" calc-graph-print :transient nil)
    ("C" "原始命令…" calc-graph-command :transient nil)]]

  ["数据"
   :pad-keys t
   ("E" "示例›" casual-zh-cn-graph-examples-menu :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-plot-options-menu ()
  "常规绘图选项菜单。"
  ["绘图样式"
   ["画布"
    ("t" "设置标题…" casual--graph-header :transient t)
    ("k" "关键字" casual--graph-key :transient t)
    ("g" "网格" casual--graph-grid :transient t)
    ("b" "边框" casual--graph-border :transient t)]

   ["曲线"
    ("n" "命名…" casual--graph-name :transient t)
    ("l" "切换线条" casual--graph-toggle-line-style :transient t)
    ("p" "切换点" casual--graph-toggle-point-style :transient t)
    ("j" "折叠" casual--graph-juggle :transient t)]]

  ["轴"
   ["标题"
    ("x" "设置 𝑥…" casual--graph-title-x :transient t)
    ("y" "设置 𝑦…" casual--graph-title-y :transient t)
    ("z" "设置 𝑧…" casual--graph-title-z :transient t)]

   ["对数/线性"
    ("X" "切换 𝑥" casual--graph-log-x :transient t)
    ("Y" "切换 𝑦" casual--graph-log-y :transient t)
    ("Z" "切换 𝑧" casual--graph-log-z :transient t)]

   ["零轴"
    ("1" "切换 𝑥" casual--graph-zero-x :transient t)
    ("2" "切换 𝑦" casual--graph-zero-y :transient t)]]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-graph-examples-menu ()
  "绘图示例菜单。"
  ["区间"
   ["自然数"
    :pad-keys t
    ("a" "插入 [0..100]" casual--push-natural-interval-0-100 :transient t)
    ("b" "插入 [0..360]" casual--push-natural-interval-0-360 :transient t)]
   ["浮点数"
    :pad-keys t
    ("c" "插入 [0.0 .. 100.0]" casual--push-float-interval-0-100 :transient t)
    ("d" "插入 [-1.0 .. 1.0]" casual--push-float-interval-1-symmetric :transient t)]]

  ["曲线"
   ["三角函数"
    :pad-keys t
    ("1" "𝑠𝑖𝑛(𝑥)" casual--push-sin :transient t)
    ("2" "𝑐𝑜𝑠(𝑥)" casual--push-cos :transient t)
    ("3" "𝑡𝑎𝑛(𝑥)" casual--push-tan :transient t)]

   ["对数函数"
    :pad-keys t
    ("4" "𝑙𝑛(𝑥)" casual--push-ln :transient t)
    ("5" "𝑒^𝑥" casual--push-e-raised-to-x :transient t)]

   ["多项式函数"
    :pad-keys t
    ("6" "𝑥² + 𝟣" casual--push-polynomial-order-2 :transient t)
    ("7" "𝑥³ + 𝑥² + 𝟣" casual--push-polynomial-order-3 :transient t)]]

  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("U" "撤销栈" calc-undo :transient t)])


(transient-define-prefix casual-zh-cn-graph-settings-menu ()
  "图形设置菜单。"
  ["图形设置"
   ("d" "设置设备…" calc-graph-device
                                        ;:description (lambda () (format "设备 (%s)…" calc-gnuplot-default-device)) 这个变量只是初始化。
    :transient nil)
   ("o" "设置输出文件…" calc-graph-output
                                        ;:description (lambda () (format "输出 (%s)…" calc-gnuplot-default-output))
    :transient nil)
   ("Q" "退出 Gnuplot 会话" calc-graph-quit :transient nil)
   ]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)])


(transient-define-prefix casual-zh-cn-curve-style-menu ()
  "样式曲线菜单。"
  ["曲线样式"
   ["线条"
    ("l" "切换线条样式" casual--graph-toggle-line-style :transient t)]
   ["点"
    ("p" "切换点样式" casual--graph-toggle-point-style :transient t)]]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)])



;; (transient-define-prefix casual-zh-cn-curve-style-menu ()
;;   "hey there"
;;   ;;:value '("--linestyle=1")
;;   [
;;    ["Line"
;;     ("L" "linestyle [1..6]" "--linestyle="
;;      :choices ("1" "2" "3" "4" "5"))
;;     ("l" "Toggle Line Style" casual--graph-line-style :transient t) ;; set prefix
;;     ]
;;    ["Point"
;;     ("p" "Toggle Point Style" casual--graph-point-style :transient t) ;; set prefix ]
;;     ]
;;    ]

;;   [:class transient-row
;;           ("C-g" "‹Back" ignore :transient transient--do-return)
;;           ("q" "Dismiss" ignore :transient transient--do-exit)
;;           ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual-zh-cn-graphics)
;;; casual-zh-cn-graphics.el ends here
