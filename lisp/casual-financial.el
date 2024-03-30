;;; casual-financial.el --- Casual Financial Menu    -*- lexical-binding: t; -*-

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
(require 'casual-stack)

(transient-define-prefix casual-financial-menu ()
  "Casual financial menu.
\nTop-level menu of financial functions."
  [["Compound Interest"
    ("f" "Future/Present Value›" casual-fin-pv-fv-menu :transient nil)
    ("p" "Periodic Payment›" casual-fin-periodic-payments-menu :transient nil)
    ("n" "# of Payments›" casual-fin-number-of-payments-menu :transient nil)
    ("t" "# of Periods To Reach Target›" casual-fin-periods-to-target-menu :transient nil)
    ("r" "Rate of Return›" casual-fin-rate-of-return-menu :transient nil)]

   ["Percentages"
    ("%" "%" calc-percent :transient t)
    ("c" "→%" calc-convert-percent :transient t)
    ("D" "Δ%" calc-percent-change :transient t)
    ("=" "=" calc-evaluate :transient t)]

   ["Display"
    ("M-s" "Stack" casual-stack-display-menu :transient t)
    ("R" "Refresh" calc-refresh :transient t)
    ("L" "Last" calc-last-args :transient t)]]

  ["Discounted Cash Flow Analysis"
   ("v" "Net Present Value›" casual-fin-npv-menu :transient nil)
   ("i" "Internal Rate of Return" casual-fin-irr-menu :transient nil)]

  ["Depreciation"
   ("d" "Depreciation" casual-fin-depreciation-menu :transient nil)]

  ["" :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-npv-menu ()
  "Casual menu to calculate net present value.
\nProvides menu argument support for the following function:
- `casual--fin-npv'"
  ["Net Present Value with Irregular Deposits"
   :class transient-row
   ("r" "Interest Rate (%)" "--rate=" :prompt "Interest Rate: ")
   ("b" "Beginning" "--beginning")]

  [("n" "Net Present Value (1:)" casual--fin-npv :transient nil)]

  ["" :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-pv-fv-menu ()
  "Casual menu for periodic present or future value functions.
\nMenu with argument support for the following functions:
- `casual--fin-fv-periodic'
- `casual--fin-fv-lump'
- `casual--fin-pv-periodic'
- `casual--fin-pv-lump'"
  :value '("--rate=5.4" "--periods=5" "--amount=1000")

  ["Future/Present Value"
   [("r" "Interest Rate (%)" "--rate=" :prompt "Interest Rate: ")
    ("n" "Number of Periods" "--periods=" :prompt "Number of Time Periods: ")]
   [("a" "Amount" "--amount=" :prompt "Amount: ")
    ("b" "Beginning" "--beginning")]]

  ;; TODO: investigate why setting :transient t triggers a fatal Transient error.
  ["Future Value"
   ("f" "Periodic" casual--fin-fv-periodic :transient nil)
   ("F" "Lump Sum" casual--fin-fv-lump :transient nil)]

  ["Present Value"
    ("p" "Periodic" casual--fin-pv-periodic :transient nil)
    ("P" "Lump Sum" casual--fin-pv-lump :transient nil)]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-periodic-payments-menu ()
  "Casual periodic payments menu.
\nMenu with argument support for the following functions:
- `casual--fin-periodic-payment'"
  ["Periodic Payments"
   ["Parameters"
    ("a" "Loan Amount" "--amount=" :prompt "Loan Amount: ")
    ("r" "Interest Rate per Period (in %)" "--rate=" :prompt "Interest Rate (%): ")
    ("n" "Number of Periods" "--periods=" :prompt "Number of periods in loan: ")
    ("b" "Beginning" "--beginning")]]

  ["Projected payment per period"
    ("p" "Payment" casual--fin-periodic-payment :transient nil)]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-number-of-payments-menu ()
  "Casual periodic payments menu.
\nMenu with argument support for the following functions:
- `casual--fin-number-of-payments'"
  ["Number of Payments"
   ["Parameters"
    ("a" "Loan Amount" "--amount=" :prompt "Loan Amount: ")
    ("r" "Interest Rate per Period (in %)" "--rate=" :prompt "Interest Rate (%): ")
    ("p" "Payment" "--payment=" :prompt "Payment: ")
    ("b" "Beginning" "--beginning")]]

  ["Projected number of payments"
    ("n" "Number of Payments" casual--fin-number-of-payments :transient nil)]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-rate-of-return-menu ()
  "Casual rate of return menu.
\nMenu with argument support for the following functions:
- `casual--fin-rate-of-return'"
  ["Compute Rate of Return on Present Value"
   ["Parameters"
    :class transient-row
    ("n" "periods" "--periods=" :prompt "Number of Periods: ")
    ("p" "payment" "--payment=" :prompt "Payment: ")
    ("v" "present value" "--pv=" :prompt "Present value: ")]]
  ;; TODO: note no --beginning argument because I `calc-fin-rate' is broken.

  ["Rate of Return"
   ("r" "Where payment made at end of period" casual--fin-rate-of-return :transient nil)]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-irr-menu ()
  "Casual internal rate of return menu.
\nMenu with argument support for the following functions:
- `casual--fin-irr'"
  ["Internal Rate of Return for Irregular Payments"
   ["Parameters"
    ("b" "Beginning" "--beginning")]]

  [("i" "Internal Rate of Return (1:)" casual--fin-irr :transient nil)]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-periods-to-target-menu ()
  "Casual periods to target menu.
\nMenu with argument support for the following functions:
- `casual--fin-periods-to-reach-target'"
  ["Number of Periods to Reach Investment Target"
   ["Parameters"
    :class transient-row
    ("t" "target" "--target=" :prompt "target amount: ")
    ("r" "rate" "--rate=" :prompt "rate (%): ")
    ("d" "deposit" "--deposit=" :prompt "deposit: ")]]

  ["Periods to Target"
   ("n" "Periods" casual--fin-periods-to-reach-target :transient nil)]

  ["" :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-fin-depreciation-menu ()
  "Casual depreciation menu.
\nMenu with argument support for the following functions:
- `casual--fin-depreciation-straight-line'
- `casual--fin-depreciation-sum-of-years'
- `casual--fin-depreciation-double-declining-balance'"
  :value '("--period=1")

  ["Parameters"
   [("c" "cost" "--cost=" :prompt "cost: ")
    ("p" "period" "--period=" :prompt "period (1..life)): ")]
   [("s" "salvage" "--salvage=" :prompt "salvage: ")]
   [("l" "life" "--life=" :prompt "life: ")]]

  ;; TODO: investigate why setting :transient t does /not/ trigger a fatal Transient error.
  ["Depreciation"
   ("1" "Straight Line (c, s, l)" casual--fin-depreciation-straight-line :transient t)
   ("2" "Sum-of-Years Digits (c, s, l, p)" casual--fin-depreciation-sum-of-years :transient t)
   ("3" "Double Declining Balance (c, s, l, p)" casual--fin-depreciation-double-declining-balance :transient t)]

  ["" :class transient-row
   ("C-g" "‹Back" ignore :transient transient--do-return)
   ("q" "Dismiss" ignore :transient transient--do-exit)
   ("U" "Undo Stack" calc-undo :transient t)])

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
  (call-interactively #'calc-fin-irr))

(defun casual--fin-fv-periodic ()
  "Future value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit added at the end of each time period
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
  (let ((modifier (if (transient-arg-value "--beginning" (transient-args transient-current-command)) :inverse nil)))
    (casual--fin-process-triplet-with-rate modifier #'calc-fin-fv '("rate" "periods" "amount"))))

(defun casual--fin-pv-periodic ()
  "Present value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit added at the end of each time period
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
  (let ((modifier (if (transient-arg-value "--beginning" (transient-args transient-current-command)) :inverse nil)))
    (casual--fin-process-triplet-with-rate modifier #'calc-fin-pv '("rate" "periods" "amount"))))

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
  (casual--fin-process-triplet-with-rate :hyper #'calc-fin-fv '("rate" "periods" "amount")))

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
  (casual--fin-process-triplet-with-rate :hyper #'calc-fin-pv '("rate" "periods" "amount")))

(defun casual--fin-periodic-payment ()
  "Amount of periodic payment necessary to amortize a loan.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-pmt'"
  (interactive)
  (let ((modifier (if (transient-arg-value "--beginning" (transient-args transient-current-command)) :inverse nil)))
    (casual--fin-process-triplet-with-rate modifier #'calc-fin-pmt '("rate" "periods" "amount"))))

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
  (let ((modifier (if (transient-arg-value "--beginning" (transient-args transient-current-command)) :inverse nil)))
    (casual--fin-process-triplet-with-rate modifier #'calc-fin-nper '("rate" "payment" "amount"))))

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
  (casual--fin-process-triplet-with-rate :hyper #'calc-fin-nper '("rate" "target" "deposit")))

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
  ;; TODO: inverse calc-fin-rate is broken
  (let ((modifier (if (transient-arg-value "--beginning" (transient-args transient-current-command)) :inverse nil)))
    (casual--fin-process-triplet modifier #'calc-fin-rate '("periods" "payment" "pv"))))

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
  (casual--fin-process-depreciation nil #'calc-fin-sln))

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
  (casual--fin-process-depreciation nil #'calc-fin-syd))

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
  (casual--fin-process-depreciation nil #'calc-fin-ddb))

(defun casual--fin-process-triplet-with-rate (modifier fn argnames)
  "Process financial prefix with MODIFIER, FN, and ARGNAMES.
\nProcess Transient prefix arguments and invoke function using said arguments.
MODIFIER : key value (either :hyper, :inverse, or nil)
FN: calc function to invoke
ARGNAMES: list of argument names
\nThe Transient argument specified by the first element in ARGNAMES is
converted to a percent value."
  (let* ((arg1 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 0 argnames))
                 (transient-args transient-current-command))))
         (arg2 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 1 argnames))
                 (transient-args transient-current-command))))
         (arg3 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 2 argnames))
                 (transient-args transient-current-command)))))

    (calc-push arg1)
    (calc-percent)
    (calc-push arg2)
    (calc-push arg3)

    (cond
     ((equal modifier :hyper) (calc-hyperbolic))
     ((equal modifier :inverse) (calc-inverse))
     (t (ignore))))

  (funcall fn))

(defun casual--fin-process-triplet (modifier fn argnames)
  "Process financial prefix with MODIFIER, FN, and ARGNAMES.
\nProcess Transient prefix arguments and invoke function using said arguments.
MODIFIER : key value (either :hyper, :inverse, or nil)
FN: calc function to invoke
ARGNAMES: list of argument names"
  (let* ((arg1 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 0 argnames))
                 (transient-args transient-current-command))))
         (arg2 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 1 argnames))
                 (transient-args transient-current-command))))
         (arg3 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" (nth 2 argnames))
                 (transient-args transient-current-command)))))

    (calc-push arg1)
    (calc-push arg2)
    (calc-push arg3)

    (cond
     ((equal modifier :hyper) (calc-hyperbolic))
     ((equal modifier :inverse) (calc-inverse))
     (t (ignore))))

  (funcall fn))

(defun casual--fin-process-depreciation (modifier fn)
  "Process financial prefix with MODIFIER, FN.
\nProcess Transient prefix arguments and invoke function using said arguments.
MODIFIER : key value (either :hyper, :inverse, or nil)
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

    (cond
     ((equal modifier :inverse) (calc-inverse))
     ((equal modifier :hyper) (calc-hyperbolic))
     (t (ignore))))

  (call-interactively fn))

(provide 'casual-financial)
;;; casual-financial.el ends here
