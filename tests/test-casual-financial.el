;;; test-casual-financial.el --- Casual Financial Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-test-utils)
(require 'casual-financial)

;; Functions -----

(ert-deftest test-casual--fin-npv ()
  (casualt-setup)

  (calc-push '(vec 2000 2000 2000 2000))
  (casualt-testbench-calc-fn 'casual--fin-npv
                             '("--rate=9")
                             '(float 647943975411 -8))

  (calc-push '(vec 2000 2000 2000 2000))
  (casualt-testbench-calc-fn 'casual--fin-npv
                             '("--rate=9" "--beginning")
                             '(float 706258933198 -8))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-irr ()
  (casualt-setup)

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (casualt-testbench-calc-fn 'casual--fin-irr
                             '()
                             '(float 974404201651 -13))

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (casualt-testbench-calc-fn 'casual--fin-irr
                             '("--beginning")
                             '(float 974404201651 -13))
  ;; TODO: same value, does inverse calc-fin-irr really work?
  (casualt-breakdown t))

(ert-deftest test-casual--fin-fv-periodic ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 9146258 -3))

  (casualt-testbench-calc-fn 'casual--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 996942122 -5))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-pv-periodic ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 647943975411 -8))

  (casualt-testbench-calc-fn 'casual--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 706258933198 -8))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-fv-lump ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-fv-lump
                             '("--rate=5.4" "--periods=5" "--amount=5000")
                             '(float 650388807223 -8))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-pv-lump ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-pv-lump
                             '("--rate=9" "--periods=4" "--amount=8000")
                             '(float 566740168852 -8))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-periodic-payment ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-periodic-payment
                             '("--rate=5.4" "--periods=5" "--amount=1000")
                             '(float 233534637575 -9))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-number-of-payments ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000")
                             '(float 125222398371 -10))

  (casualt-testbench-calc-fn 'casual--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000" "--beginning")
                             '(float 119976962243 -10))

  (casualt-breakdown t))


(ert-deftest test-casual--fin-periods-to-reach-target ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual--fin-periods-to-reach-target
                             '("--rate=9" "--target=20000" "--deposit=2000")
                             '(float 267190374474 -10))
  (casualt-breakdown t))

(ert-deftest test-casual--fin-rate-of-return ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual--fin-rate-of-return
                             '("--periods=4" "--payment=2000" "--pv=6479.44")
                             '(calcFunc-percent
	                       (float 899999827105 -11)))
  (casualt-breakdown t))

(ert-deftest test-casual--fin-depreciation-straight-line ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-depreciation-straight-line
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=0")
                             2000)

  (casualt-breakdown t))

(ert-deftest test-casual--fin-depreciation-sum-of-years ()
  (casualt-setup)

  (casualt-testbench-calc-fn 'casual--fin-depreciation-sum-of-years
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             '(float 333333333333 -8))

  (casualt-breakdown t))

(ert-deftest test-casual--fin-depreciation-double-declining-balance ()
  (casualt-setup)
  (casualt-testbench-calc-fn 'casual--fin-depreciation-double-declining-balance
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             4800)
  (casualt-breakdown t))

;; Menus ----
(ert-deftest test-casual-financial-menu ()
  (casualt-setup)
  (let ((test-vectors '(("f" . casual-fin-pv-fv-menu)
                        ("p" . casual-fin-periodic-payments-menu)
                        ("n" . casual-fin-number-of-payments-menu)
                        ("t" . casual-fin-periods-to-target-menu)
                        ("r" . casual-fin-rate-of-return-menu)

                        ("%q" . calc-percent)
                        ("cq" . calc-convert-percent)
                        ("Dq" . calc-percent-change)
                        ;; ("=q" . calc-evaluate)  ; dunno why this doesn't work

                        ("v" . casual-fin-npv-menu)
                        ("i" . casual-fin-irr-menu)
                        ("d" . casual-fin-depreciation-menu))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-financial-menu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-fin-npv-menu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-fin-npv-menu
                                      "n"
                                      #'casual--fin-npv
                                      (random 5000))
  (casualt-breakdown t))


(ert-deftest test-casual-fin-pv-fv-menu ()
  (casualt-setup)
  (let ((test-vectors '(("f" . casual--fin-fv-periodic)
                        ("F" . casual--fin-fv-lump)
                        ("p" . casual--fin-pv-periodic)
                        ("P" . casual--fin-pv-lump))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-fin-pv-fv-menu
                                     '(lambda () (random 5000)))
    (casualt-breakdown t)))

(ert-deftest test-casual-fin-periodic-payments-menu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-fin-periodic-payments-menu
                                      "p"
                                      #'casual--fin-periodic-payment
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-fin-number-of-payments-menu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-fin-number-of-payments-menu
                                      "n"
                                      #'casual--fin-number-of-payments
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-fin-rate-of-return-menu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-fin-rate-of-return-menu
                                      "r"
                                      #'casual--fin-rate-of-return
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-fin-periods-to-target-menu ()
  (casualt-setup)
  (casualt-testbench-transient-suffix #'casual-fin-periods-to-target-menu
                                      "n"
                                      #'casual--fin-periods-to-reach-target
                                      (random 5000))
  (casualt-breakdown t))

(ert-deftest test-casual-fin-depreciation-menu ()
  (casualt-setup)
  (let ((test-vectors '(("1" . casual--fin-depreciation-straight-line)
                        ("2" . casual--fin-depreciation-sum-of-years)
                        ("3" . casual--fin-depreciation-double-declining-balance))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-fin-depreciation-menu
                                     '(lambda () (random 5000)))
    (casualt-breakdown t))

  (casualt-breakdown t))

;; (provide 'test-casual-financial)
;;; test-casual-financial.el ends here
