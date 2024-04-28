;;; casual-zh-cn-financial.el --- Casual Financial Menu    -*- lexical-binding: t; -*-

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
(require 'calc-ext)
(require 'transient)
(require 'casual-zh-cn-stack)

(transient-define-prefix casual-zh-cn-financial-menu ()
  "Casua sua 金融菜单。
\n金融功能的顶层菜单。"
  [["复利计算"
    ("f" "未来/现值›" casual-zh-cn-fin-pv-fv-menu :transient nil)
    ("p" "周期性支付›" casual-zh-cn-fin-periodic-payments-menu :transient nil)
    ("n" "支付次数›" casual-zh-cn-fin-number-of-payments-menu :transient nil)
    ("t" "达到目标的期数›" casual-zh-cn-fin-periods-to-target-menu :transient nil)
    ("r" "收益率›" casual-zh-cn-fin-rate-of-return-menu :transient nil)]

   ["百分比"
    ("%" "%" calc-percent :transient t)
    ("c" "→%" calc-convert-percent :transient t)
    ("D" "Δ%" calc-percent-change :transient t)
    ("=" "=" calc-evaluate :transient t)]

   ["显示"
    ("M-s" "栈" casual-stack-display-menu :transient t)
    ("R" "刷新" calc-refresh :transient t)
    ("L" "上次参数" calc-last-args :transient t)]]

  ["贴现现金流分析"
   ("v" "净现值›" casual-zh-cn-fin-npv-menu :transient nil)
   ("i" "内部收益率" casual-zh-cn-fin-irr-menu :transient nil)]

  ["折旧"
   ("d" "折旧" casual-zh-cn-fin-depreciation-menu :transient nil)]

  ["" :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "退出" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-npv-menu ()
  "净现值计算Casua 菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-npv'"
  ["不规则支付的净现值"
   :class transient-row
   ("r" "利率 (%)" "--rate=" :prompt "利率: ")
   ("b" "初始" "--beginning")]

  [("n" "净现值 (1:)" casual--fin-npv :transient nil)]

  ["" :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "退出" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-pv-fv-menu ()
  "周期性现值或未来值函数的Casua 菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-fv-periodic'
- `casual--fin-fv-lump'
- `casual--fin-pv-periodic'
- `casual--fin-pv-lump'"
  :value '("--rate=5.4" "--periods=5" "--amount=1000")

  ["未来/现值"
   [("r" "利率 (%)" "--rate=" :prompt "利率: ")
    ("n" "时间期数" "--periods=" :prompt "时间期数: ")]
   [("a" "金额" "--amount=" :prompt "金额: ")
    ("b" "初始" "--beginning")]]

  ["未来值"
   ("f" "周期性" casual--fin-fv-periodic :transient t)
   ("F" "总额" casual--fin-fv-lump :transient t)]

  ["现值"
    ("p" "周期性" casual--fin-pv-periodic :transient t)
    ("P" "总额" casual--fin-pv-lump :transient t)]

  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-periodic-payments-menu ()
  "Casua 周期性支付菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-periodic-payment'"
  ["周期性支付"
   ["参数"
    ("a" "贷款金额" "--amount=" :prompt "贷款金额: ")
    ("r" "每期利率 (%)" "--rate=" :prompt "每期利率 (%): ")
    ("n" "贷款期数" "--periods=" :prompt "贷款期数: ")
    ("b" "初始" "--beginning")]]

  ["每期预期支付额"
    ("p" "支付额" casual--fin-periodic-payment :transient t)]

  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-number-of-payments-menu ()
  "Casua 周期性支付菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-number-of-payments'"
  ["支付期数"
   ["参数"
    ("a" "贷款金额" "--amount=" :prompt "贷款金额: ")
    ("r" "每期利率 (%)" "--rate=" :prompt "每期利率 (%): ")
    ("p" "支付额" "--payment=" :prompt "支付额: ")
    ("b" "初始" "--beginning")]]

  ["预期支付期数"
    ("n" "支付期数" casual--fin-number-of-payments :transient t)]

  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-rate-of-return-menu ()
  "Casua 收益率菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-rate-of-return'"
  ["计算现值的收益率"
   ["参数"
    :class transient-row
    ("n" "期数" "--periods=" :prompt "期数: ")
    ("p" "支付" "--payment=" :prompt "支付: ")
    ("v" "现值" "--pv=" :prompt "现值: ")]]
  ;; TODO: 注意，因为 `calc-fin-rate' 函数有问题，所以没有 `--beginning' 参数。

  ["收益率"
   ("r" "支付在期末的情况" casual--fin-rate-of-return :transient t)]

  [:class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-irr-menu ()
  "Casua 内部收益率菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-irr'"
  ["不规则支付的内部收益率"
   ["参数"
    ("b" "初始" "--beginning")]]

  [("i" "内部收益率 (1:)" casual--fin-irr :transient nil)]

  ["" :class transient-row
          ("C-g" "‹返回" ignore :transient transient--do-return)
          ("q" "退出" ignore :transient transient--do-exit)
          ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-periods-to-target-menu ()
  "Casua 达到目标的期数菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-periods-to-reach-target'"
  ["达到投资目标所需的期数"
   ["参数"
    :class transient-row
    ("t" "目标" "--target=" :prompt "目标金额: ")
    ("r" "利率" "--rate=" :prompt "利率 (%): ")
    ("d" "存款" "--deposit=" :prompt "存款: ")]]

  ["达到目标的期数"
   ("n" "期数" casual--fin-periods-to-reach-target :transient t)]

  ["" :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "退出" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])

(transient-define-prefix casual-zh-cn-fin-depreciation-menu ()
  "Casua 折旧菜单。
\n为以下函数提供菜单参数支持：
- `casual--fin-depreciation-straight-line'
- `casual--fin-depreciation-sum-of-years'
- `casual--fin-depreciation-double-declining-balance'"
  :value '("--period=1")

  ["参数"
   [("c" "成本" "--cost=" :prompt "成本: ")
    ("s" "残值" "--salvage=" :prompt "残值: ")]
   [("l" "使用年限" "--life=" :prompt "使用年限: ")
    ("p" "期数" "--period=" :prompt "期数 (1..life)): ")]]

  ["折旧"
   ("1" "直线法 (c, s, l)" casual--fin-depreciation-straight-line :transient t)
   ("2" "年数总和法 (c, s, l, p)" casual--fin-depreciation-sum-of-years :transient t)
   ("3" "双倍余额递减法 (c, s, l, p)" casual--fin-depreciation-double-declining-balance :transient t)]

  ["" :class transient-row
   ("C-g" "‹返回" ignore :transient transient--do-return)
   ("q" "退出" ignore :transient transient--do-exit)
   ("U" "撤销堆栈" calc-undo :transient t)])


;; Wrapped Functions -----

(defun casual--fin-npv ()
  "Net present value of irregular investments.
\nCompute the net present value of a series of irregular investments.
\nMenu argument:\n
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nThis command requires the stack to be populated with the argument:\n
1: vector of irregular investments

* References
- info node `(calc) Present Value'
- `calc-fin-npv'"
  (interactive)
  (let ((rate (math-read-number-simple
               (transient-arg-value
                "--rate="
                (transient-args transient-current-command))))

        (beginning (transient-arg-value
                    "--beginning"
                    (transient-args transient-current-command))))

    (calc-push rate)
    (calc-percent)
    (call-interactively #'calc-roll-down)
    (if beginning
        (calc-inverse))
    (call-interactively #'calc-fin-npv)))

(defun casual--fin-irr ()
  "Internal rate of return for irregular payments.
\nThe internal rate of return is defined as the discount rate
that results in a net present value (NPV) of zero. Internal rate
of return analysis is a financial technique that is often used to
assess the likely profitability of a project or investment.
\nIRR assumes that cash flows occur regularly, in equal length time
periods. Cash flow values may vary between periods and represent
either income (positive) or expenditure (negative). At least one
value must be negative and at least one value must be positive.
\n\nMenu argument:\n
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nThis command requires the stack to be populated with the argument:\n
1: vector of irregular payments

* References
- info node `(calc) Related Financial Functions'
- URL `https://wiki.documentfoundation.org/Documentation/Calc_Functions/IRR'
- `calc-fin-irr'"
  (interactive)
  (if (transient-arg-value "--beginning" (transient-args transient-current-command))
      (calc-inverse))
  (call-interactively #'calc-fin-irr)
  (calc-convert-percent))

(defun casual--fin-fv-periodic ()
  "Future value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit amount per time period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Future Value'.
- `calc-fin-fv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "fvb" (cons #'calcFunc-fvb (calc-top-list-n 3)))
       (calc-enter-result 3 "fv" (cons #'calcFunc-fv (calc-top-list-n 3)))))))

(defun casual--fin-pv-periodic ()
  "Present value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit amount per time period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Present Value'.
- `calc-fin-pv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "pvb" (cons #'calcFunc-pvb (calc-top-list-n 3)))
       (calc-enter-result 3 "pv" (cons #'calcFunc-pv (calc-top-list-n 3)))))))

(defun casual--fin-fv-lump ()
  "Future value, lump sum.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : deposit amount
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Future Value'.
- `calc-fin-fv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command))))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (calc-enter-result 3 "fvl" (cons #'calcFunc-fvl (calc-top-list-n 3))))))

(defun casual--fin-pv-lump ()
  "Present value, lump sum.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : deposit amount
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Present Value'
- `calc-fin-pv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command))))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (calc-enter-result 3 "pvl" (cons #'calcFunc-pvl (calc-top-list-n 3))))))

(defun casual--fin-periodic-payment ()
  "Amount of periodic payment necessary to amortize a loan.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : loan amount
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-pmt'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "pmtb" (cons #'calcFunc-pmtb (calc-top-list-n 3)))
       (calc-enter-result 3 "pmt" (cons #'calcFunc-pmt (calc-top-list-n 3)))))))

(defun casual--fin-number-of-payments ()
  "Number of regular payments to amortize a loan.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--amount=a  : loan amount
--rate=r    : % interest rate per period of time
--payment=p : payment per period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: p
1: a

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-nper'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (payment (math-read-number-simple
                   (transient-arg-value "--payment=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push payment)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "nprb" (cons #'calcFunc-nperb (calc-top-list-n 3)))
       (calc-enter-result 3 "nper" (cons #'calcFunc-nper (calc-top-list-n 3)))))))

(defun casual--fin-periods-to-reach-target ()
  "Number of periods to reach an investment target.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--target=t  : investment target amount
--rate=r    : interest rate per period
--deposit=d : deposit amount per period

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-nper'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (target (math-read-number-simple
                  (transient-arg-value "--target=" current-command)))
         (deposit (math-read-number-simple
                   (transient-arg-value "--deposit=" current-command))))
    (calc-push rate)
    (calc-percent)
    (calc-push target)
    (calc-push deposit)

    (calc-slow-wrapper
     (calc-enter-result 3 "nprl" (cons #'calcFunc-nperl (calc-top-list-n 3))))))

(defun casual--fin-rate-of-return ()
  "Compute rate of return on investment.
\nMenu arguments:\n
--periods=n    : number of periods
--payment=p    : payment per period
--pv=v         : present value of asset

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-rate'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (payment (math-read-number-simple
                   (transient-arg-value "--payment=" current-command)))
         (pv (math-read-number-simple
              (transient-arg-value "--pv=" current-command))))
    (calc-push periods)
    (calc-push payment)
    (calc-push pv)

    ;; TODO: inverse calc-fin-rate is broken
    (calc-slow-wrapper
     (calc-pop-push-record 3
                           "rate"
                           (calc-to-percentage
                            (calc-normalize
                             (cons #'calcFunc-rate (calc-top-list-n 3))))))))

(defun casual--fin-depreciation-straight-line ()
  "Compute depreciation using “straight-line” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-sln'"
  (interactive)
  (casual--fin-process-depreciation #'calc-fin-sln))

(defun casual--fin-depreciation-sum-of-years ()
  "Compute depreciation using “sum-of-years-digits” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life
--period=p  : specific period (year) of life, must be within [1..l]

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-syd'"
  (interactive)
  (casual--fin-process-depreciation #'calc-fin-syd))

(defun casual--fin-depreciation-double-declining-balance ()
  "Compute depreciation using “double-declining balance” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life
--period=p  : specific period (year) of life, must be within [1..l]

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-ddb'"
  (interactive)
  (casual--fin-process-depreciation #'calc-fin-ddb))

(defun casual--fin-process-depreciation (fn)
  "Process financial prefix with MODIFIER, FN.
\nProcess Transient prefix arguments and invoke function using said arguments.
FN: calc function to invoke"
  (let* ((arg1 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "cost")
                 (transient-args transient-current-command))))
         (arg2 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "salvage")
                 (transient-args transient-current-command))))
         (arg3 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "life")
                 (transient-args transient-current-command))))
         (arg4 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "period")
                 (transient-args transient-current-command)))))

    (calc-push arg1)
    (calc-push arg2)
    (calc-push arg3)
    (if (not (eq fn #'calc-fin-sln))
        (calc-push arg4))

    (funcall fn)))

(provide 'casual-zh-cn-financial)
;;; casual-zh-cn-financial.el ends here
