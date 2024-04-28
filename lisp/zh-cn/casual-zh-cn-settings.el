;;; casual-zh-cn-settings.el --- Casual Settings Menu      -*- lexical-binding: t; -*-

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
(require 'casual-version)
(require 'casual-zh-cn-angle-measure)

;; = Menus =
(transient-define-prefix casual-zh-cn-modes-menu ()
  "Casual modes menu."
  [["模式"
    :pad-keys t
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual--checkbox-label calc-algebraic-mode
                                            "代数模式"))
     :transient t)
    ("z" "前导零" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros
                                            "前导零"))
     :transient t)
    ("F" calc-frac-mode :description casual-prefer-frac-label :transient t)
    ("s" calc-symbolic-mode :description casual-symbolic-mode-label :transient t)
    ("p" calc-polar-mode :description casual-cmplx-or-polar-label :transient t)
    ("c" "复数格式›" casual-zh-cn-complex-format-menu
     :description (lambda ()
                    (format "复数格式 (当前 %s)›"
                            (casual-complex-format-label)))
     :transient t)
    ;; ("m" calc-matrix-mode :description casual-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("P" calc-precision
     :description (lambda ()
                    (format "精度 (当前 %d)" calc-internal-prec))
     :transient t)
    ("I" "无穷模式" casual-calc-infinite-mode
     :description (lambda ()
                    (casual--checkbox-label calc-infinite-mode
                                            "无穷模式"))
     :transient t)]
   ["角度度量"
    ("a" casual-zh-cn-angle-measure-menu
     :description (lambda ()
                    (format "角度度量 (当前 %s)›"
                            (casual-angle-mode-label)))
     :transient t)]]
  [["显示"
    ("R" casual-zh-cn-radix-menu
     :description (lambda ()
                    (format "基数 (当前 %s)›" (casual-number-radix-label)))
     :transient t)
    ("f" casual-zh-cn-float-format-menu
     :description (lambda ()
                    (format "浮点格式 (当前 %s)›"
                            (casual-float-format-label)))
     :transient t)
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (casual--checkbox-label calc-group-digits
                                            "显示千位分隔符"))
     :transient t)
    ("," "千位分隔符…" calc-group-char
     :description (lambda ()
                    (format "设置千位分隔符 (当前 %s)…" calc-group-char))
     :transient t)
    ("." "小数点…" calc-point-char
     :description (lambda ()
                    (format "设置小数点 (当前 %s)…" calc-point-char))
     :transient t)
    ("H" "ℎ𝑚𝑠 格式" calc-hms-notation
     :description (lambda ()
                    (format
                     "设置 ℎ𝑚𝑠 格式 (当前 %s)"
                     (format calc-hms-format "" "" "")))
     :transient t)]
   ["设置"
    ("S" "保存 Calc 设置" calc-save-modes :transient t)
    ("O" "打开 Calc 设置文件" casual-open-settings-file :transient nil)
    ("C-M-r" "Calc 重置" calc-reset :transient t)]]
  [""
   :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("v" "版本" casual-version :transient nil)
   ("M-a" "关于" casual-about :transient nil)])

(transient-define-prefix casual-zh-cn-complex-format-menu ()
  "Casual complex formats menu."
  ["复数格式"
   :description (lambda ()
                  (format "复数格式 (当前 %s)"
                          (casual-complex-format-label)))
   ("c" calc-complex-notation
    :description "复数（直角坐标）表示法"
    :transient nil)

   ("i" calc-i-notation
    :description "𝑖 表示法"
    :transient nil)

   ("j" calc-j-notation
    :description "𝑗 表示法"
    :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])


(transient-define-prefix casual-zh-cn-float-format-menu ()
  "Casual float formats menu."
  ["浮点数格式 (𝑛 是栈中的第一个值)"
   ("n" "普通" calc-normal-notation :transient nil)
   ("f" "定点数 𝑛" calc-fix-notation :transient nil)
   ("s" "科学计数法" calc-sci-notation :transient nil)
   ("e" "工程计数法" calc-eng-notation :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])



;; = Functions =
(defun casual-about-casual ()
  "Casual is an opinionated porcelain for Emacs Calc.

Learn more about using Casual at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/Casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/Casual/issues'

If you enjoy using Casual, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual was conceived and crafted by Charles Choi in San Francisco, California.

Thank you for using Casual and always choose love."
  (ignore))

(defun casual-about ()
  "About information for Casual."
  (interactive)
  (describe-function 'casual-about-casual))

(defun casual-calc-infinite-mode ()
  "Toggle infinite mode on or off.

Divide-by-zero (e.g. ‘1 / 0’) results are normally treated as
errors; formulas like this are left in unsimplified form. An
alternate behavior is to treat a divide-by-zero condition as an
infinite result. This command toggles this behavior.

This function is a wrapper over `calc-infinite-mode'.

* References
- info node `(calc) Infinite Mode'
- `calc-infinite-mode'"
  (interactive)
  (call-interactively #'calc-infinite-mode))

(provide 'casual-zh-cn-settings)
;;; casual-zh-cn-settings.el ends here
