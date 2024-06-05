;;; test-casual-calc-financial.el --- Casual Financial Menu Tests  -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-financial)

;; Functions -----

(ert-deftest test-casual-calc--fin-npv ()
  (casualt-setup)

  (calc-push '(vec 2000 2000 2000 2000))
  (casualt-testbench-calc-fn 'casual-calc--fin-npv
                             '("--rate=9")
                             '(float 647943975411 -8))

  (calc-push '(vec 2000 2000 2000 2000))
  (casualt-testbench-calc-fn 'casual-calc--fin-npv
                             '("--rate=9" "--beginning")
                             '(float 706258933198 -8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-irr ()
  (casualt-setup)

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (casualt-testbench-calc-fn 'casual-calc--fin-irr
                             '()
                             '(calcFunc-percent
	                       (float 974404201651 -11)))

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (casualt-testbench-calc-fn 'casual-calc--fin-irr
                             '("--beginning")
                             '(calcFunc-percent
	                       (float 974404201651 -11)))
  ;; TODO: same value, does inverse calc-fin-irr really work?
  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-fv-periodic ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 9146258 -3))

  (casualt-testbench-calc-fn 'casual-calc--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 996942122 -5))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-pv-periodic ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 647943975411 -8))

  (casualt-testbench-calc-fn 'casual-calc--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 706258933198 -8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-fv-lump ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-fv-lump
                             '("--rate=5.4" "--periods=5" "--amount=5000")
                             '(float 650388807223 -8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-pv-lump ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-pv-lump
                             '("--rate=9" "--periods=4" "--amount=8000")
                             '(float 566740168852 -8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-periodic-payment ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-periodic-payment
                             '("--rate=5.4" "--periods=5" "--amount=1000")
                             '(float 233534637575 -9))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-number-of-payments ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000")
                             '(float 125222398371 -10))

  (casualt-testbench-calc-fn 'casual-calc--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000" "--beginning")
                             '(float 119976962243 -10))

  (casualt-breakdown t))


(ert-deftest test-casual-calc--fin-periods-to-reach-target ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual-calc--fin-periods-to-reach-target
                             '("--rate=9" "--target=20000" "--deposit=2000")
                             '(float 267190374474 -10))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-rate-of-return ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual-calc--fin-rate-of-return
                             '("--periods=4" "--payment=2000" "--pv=6479.44")
                             '(calcFunc-percent
	                       (float 899999827105 -11)))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-depreciation-straight-line ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-depreciation-straight-line
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=0")
                             2000)

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-depreciation-sum-of-years ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual-calc--fin-depreciation-sum-of-years
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             '(float 333333333333 -8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--fin-depreciation-double-declining-balance ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual-calc--fin-depreciation-double-declining-balance
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             4800)
  (casualt-breakdown t))

;; Menus ----
(ert-deftest test-casual-calc-financial-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("f" . casual-calc-fin-pv-fv-tmenu)
                        ("p" . casual-calc-fin-periodic-payments-tmenu)
                        ("n" . casual-calc-fin-number-of-payments-tmenu)
                        ("t" . casual-calc-fin-periods-to-target-tmenu)
                        ("r" . casual-calc-fin-rate-of-return-tmenu)

                        ("%q" . calc-percent)
                        ("cq" . calc-convert-percent)
                        ("Dq" . calc-percent-change)
                        ;; ("=q" . calc-evaluate)  ; dunno why this doesn't work

                        ("v" . casual-calc-fin-npv-tmenu)
                        ("i" . casual-calc-fin-irr-tmenu)
                        ("d" . casual-calc-fin-depreciation-tmenu))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-financial-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-fin-npv-tmenu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-calc-fin-npv-tmenu
                                      "n"
                                      #'casual-calc--fin-npv
                                      (random 5000))
  (casualt-breakdown t))


(ert-deftest test-casual-calc-fin-pv-fv-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("f" . casual-calc--fin-fv-periodic)
                        ("F" . casual-calc--fin-fv-lump)
                        ("p" . casual-calc--fin-pv-periodic)
                        ("P" . casual-calc--fin-pv-lump))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-fin-pv-fv-tmenu
                                     '(lambda () (random 5000)))
    (casualt-breakdown t)))

(ert-deftest test-casual-calc-fin-periodic-payments-tmenu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-calc-fin-periodic-payments-tmenu
                                      "p"
                                      #'casual-calc--fin-periodic-payment
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-fin-number-of-payments-tmenu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-calc-fin-number-of-payments-tmenu
                                      "n"
                                      #'casual-calc--fin-number-of-payments
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-fin-rate-of-return-tmenu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-calc-fin-rate-of-return-tmenu
                                      "r"
                                      #'casual-calc--fin-rate-of-return
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-fin-periods-to-target-tmenu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-calc-fin-periods-to-target-tmenu
                                      "n"
                                      #'casual-calc--fin-periods-to-reach-target
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-calc-fin-depreciation-tmenu ()
  (casualt-setup)
  (let ((test-vectors '(("1" . casual-calc--fin-depreciation-straight-line)
                        ("2" . casual-calc--fin-depreciation-sum-of-years)
                        ("3" . casual-calc--fin-depreciation-double-declining-balance))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-fin-depreciation-tmenu
                                     '(lambda () (random 5000)))
    (casualt-breakdown t))

  (casualt-breakdown t))

;; (provide 'test-casual-calc-financial)
;;; test-casual-calc-financial.el ends here
