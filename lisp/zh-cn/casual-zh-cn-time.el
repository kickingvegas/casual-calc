;;; casual-zh-cn-time.el --- Casual Time Menu              -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-time-menu ()
  "时间菜单。"
  ["时间"
   ("n" "当前时间" calc-now :transient nil)
   ("f" "首日是›" casual-zh-cn-first-day-menu :transient nil)
   ("i" "增加一个月" calc-inc-month :transient nil)
   ("u" "转换为 Unix 时间" calc-unix-time :transient nil)
   ("+" "增加工作日" calc-business-days-plus :transient nil)
   ("-" "减去工作日" calc-business-days-minus :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-first-day-menu ()
  "时间首日菜单。"
  ["首日是"
   ("w" "周" calc-new-week :transient nil)
   ("m" "月" calc-new-month :transient nil)
   ("y" "年" calc-new-year :transient nil)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销" calc-undo :transient t)])


(provide 'casual-zh-cn-time)
;;; casual-zh-cn-time.el ends here
