;;; casual-random.el --- Casual Random Menu          -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-random-number-menu ()
  "Casual random number functions menu."
  :value '("-m=10")
  ["Random Number Generation\n"
   ["Natural Number"
    ("m" "𝑚" "-m=" :prompt "𝑚: "
     :reader transient-read-number-N+)
    ("r" "Natural within [𝟢..𝑚)" casual--random-interval-0-to-m :transient t)]

   ["Real Number"
    ("c" "Real within [𝟢.𝟢..𝟣.𝟢)" calc-rrandom :transient t)]]

  ;;("r" "Random number within [0..𝑛)" calc-random :transient nil)

  [("a" "Random number again" calc-random-again :transient t)]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])


(provide 'casual-random)
;;; casual-random.el ends here
