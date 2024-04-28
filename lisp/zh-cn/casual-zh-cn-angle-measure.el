;;; casual-zh-cn-angle-measure.el --- Casual Angle Measure Menu  -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-angle-measure-menu ()
  "Casua 角度测量功能菜单。"
  ["角度测量"
   :description (lambda ()
                  (format "当前角度单位：%s›"
                          (casual-angle-mode-label)))
   ("d" "度数" calc-degrees-mode :transient nil)
   ("r" "弧度" calc-radians-mode :transient nil)
   ("h" "度-分-秒" calc-hms-mode :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])


(provide 'casual-zh-cn-angle-measure)
;;; casual-zh-cn-angle-measure.el ends here
