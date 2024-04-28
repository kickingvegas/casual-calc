;;; casual-zh-cn-trail.el --- Casual Trail Menu            -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-zh-cn-trail-menu ()
  "轨迹菜单。"
  ["轨迹命令"
   ["显示"
    ("d" "切换显示" calc-trail-display :transient t)
    ("t" "焦点切换至轨迹" calc-trail-in :transient t)
    ("c" "焦点切换至计算" calc-trail-out :transient t)
    ("<" "左滚动" calc-trail-scroll-left :transient t)
    (">" "右滚动" calc-trail-scroll-right :transient t)]

   ["导航"
    ("p" "上一个" calc-trail-previous :transient t)
    ("n" "下一个" calc-trail-next :transient t)
    ("[" "第一个" calc-trail-first :transient t)
    ("]" "最后一个" calc-trail-last :transient t)]

   ["搜索与编辑"
    ("m" "插入标记…" calc-trail-marker :transient t)
    ("C-r" "向后搜索" calc-trail-isearch-backward :transient t)
    ("C-s" "向前搜索" calc-trail-isearch-forward :transient t)
    ("y" "复制到栈" calc-trail-yank :transient t)
    ("k" "删除所选" calc-trail-kill :transient t)]]

  [:class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-rebturn)
   ("q" "关闭" ignore :transient transient--do-exit)
   ("s" "保存设置" calc-save-modes :transient t)])


(provide 'casual-zh-cn-trail)
;;; casual-zh-cn-trail.el ends here
