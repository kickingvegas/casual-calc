;;; casual-zh-cn-random.el --- Casual Random Menu          -*- lexical-binding: t; -*-

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
(require 'calc-comb)
(require 'transient)

(defun casual--random-interval-0-to-m ()
  "Generate natural random number [0..m)."
  (interactive)
  (let* ((m (transient-arg-value "-m=" (transient-args transient-current-command))))
    (if m (calc-push (string-to-number m)))
    (calc-random nil)))

(transient-define-prefix casual-zh-cn-random-number-menu ()
  "随机数生成菜单。"
  :value '("-m=10")
  ["随机数生成\n"
   ["自然数"
    ("m" "𝑚" "-m=" :prompt "𝑚: "
     :reader transient-read-number-N+)
    ("r" "在 [𝟢..𝑚) 之间的自然数" casual--random-interval-0-to-m :transient t)]

   ["实数"
    ("c" "在 [𝟢.𝟢..𝟣.𝟢) 之间的实数" calc-rrandom :transient t)]]

  ;;("r" "Random number within [0..𝑛)" calc-random :transient nil)

  [("a" "再次生成随机数" calc-random-again :transient t)]
  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "关闭" ignore :transient transient--do-exit)
          ("U" "撤销栈" calc-undo :transient t)])



(provide 'casual-zh-cn-random)
;;; casual-zh-cn-random.el ends here
