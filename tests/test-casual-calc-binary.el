;;; test-casual-calc-binary.el --- Casual Binary Menu Tests  -*- lexical-binding: t; -*-

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
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-binary)

(ert-deftest test-casual-calc-binary-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors '(("a" . casual-calc--calc-and)
                        ("o" . casual-calc--calc-or)
                        ("x" . casual-calc--calc-xor)
                        ("-" . casual-calc--calc-diff)
                        ("!" . casual-calc--calc-not)

                        ("lq" . casual-calc--calc-lshift-binary)
                        ("rq" . casual-calc--calc-rshift-binary)

                        ("ìq" . casual-calc--calc-lshift-arith)
                        ("òq" . casual-calc--calc-rshift-arith)
                        ("q" . casual-calc--calc-rotate-binary)

                        ("Rq" . casual-calc-radix-tmenu)
                        ("wq" . casual-calc--calc-word-size)
                        ("u" . casual-calc--calc-unpack-bits)
                        ("p" . casual-calc--calc-pack-bits))))

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-binary-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))


(ert-deftest test-casual-calc--calc-and ()
  (casualt-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-and)
  (should (= (calc-top) 8))

  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-or ()
  (casualt-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-or)
  (should (= (calc-top) 15))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-xor ()
  (casualt-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-xor)
  (should (= (calc-top) 7))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-diff ()
  (casualt-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-diff)
  (should (= (calc-top) 3))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-not ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 11)
  (call-interactively #'casual-calc--calc-not)
  (should (= (calc-top) 244))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-lshift-binary ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-lshift-binary)
  (should (= (calc-top) 24))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-rshift-binary ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--calc-rshift-binary)
  (should (= (calc-top) 12))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-lshift-arith ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'casual-calc--calc-lshift-arith)
  (should (= (calc-top) 24))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-rshift-arith ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--calc-rshift-arith)
  (should (= (calc-top) 12))
  (casualt-breakdown t))

(ert-deftest test-casual-calc--calc-rotate-binary ()
  (casualt-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--calc-rotate-binary)
  (should (= (calc-top) 48))
  (casualt-breakdown t))

;; (ert-deftest test-casual-calc--calc-word-size ()
;;   (casualt-setup)
;;   (casualt-breakdown t))

;; (ert-deftest test-casual-calc--calc-unpack-bits ()
;;   (casualt-setup)
;;   (casualt-breakdown t))

;; (ert-deftest test-casual-calc--calc-pack-bits ()
;;   (casualt-setup)
;;   (casualt-breakdown t))

(provide 'test-casual-calc-binary)
;;; test-casual-calc-binary.el ends here
