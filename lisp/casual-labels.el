;;; casual-labels.el --- Casual Labels               -*- lexical-binding: t; -*-

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
(require 'calc-math)

;; Labels
(defun casual-cmplx-or-polar-label ()
  "Label for either complex or polar mode."
  (if (eq calc-complex-mode 'polar)
      "Change to Complex Mode (now Polar)"
    "Change to Polar Mode (now Complex)"))

(defun casual-symbolic-mode-label ()
  "Label for symbolic mode."
  (if calc-symbolic-mode
      "Change to Numeric Mode (now Symbolic)"
    "Change to Symbolic Mode (now Numeric)"))

(defun casual-prefer-frac-label ()
  "Label for fractional or floating point mode."
  (if calc-prefer-frac
      "Change to Floating Point Results (now Fractional)"
    "Change to Fractional Results (now Floating Point)"))

(defun casual-number-radix-label ()
  "Label for number radix."
  (cond
   ((= calc-number-radix 10) "Decimal")
   ((= calc-number-radix 2) "Binary")
   ((= calc-number-radix 8) "Octal")
   ((= calc-number-radix 16) "Hexadecimal")
   (t (format "%d" calc-number-radix))))

(defun casual-matrix-mode-label ()
  "Label for matrix mode."
  (cond
   ((eq calc-matrix-mode 'matrix) "Matrix")
   ((eq calc-matrix-mode 'sqmatrix) "Square Matrix")
   ((eq calc-matrix-mode 'scalar) "Scalar")
   ((eq calc-matrix-mode 'nil) "No assumptions")
   ((integerp calc-matrix-mode) (format "%dx%d"
                                        calc-matrix-mode
                                        calc-matrix-mode))))

(defun casual-angle-mode-label ()
  "Label for angle mode."
  (cond
   ((eq calc-angle-mode 'deg) "Degrees")
   ((eq calc-angle-mode 'rad) "Radians")
   ((eq calc-angle-mode 'hms) "hms")))

(defun casual-complex-format-label ()
  "Label for complex format mode."
  (cond
   ((eq calc-complex-format 'i) "x + yi")
   ((eq calc-complex-format 'j) "x + yj")
   ((not calc-complex-format) "(x, y)")))

(defun casual-float-format-label (&optional include-precision)
  "Label for Calc float mode.
If INCLUDE-PRECISION is non-nil, then add precision to label."
  (let ((mode (pcase (car calc-float-format)
                ('float "Normal")
                ('fix "Fixed Point")
                ('sci "Scientific")
                ('eng "Engineering")))
        (precision (nth 1 calc-float-format)))

    (if include-precision
        (format "%s %d" mode precision)
      (format "%s" mode))))

(defun casual--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if (display-graphic-p)
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

(defun casual--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual--suffix-label (label suffix)
  "Label constructed with LABEL and SUFFIX separated by a space."
  (format "%s %s" label suffix))

(defun casual--checkbox-label (v label)
  "Casual checkbox label using variable V and LABEL."
  (casual--prefix-label label (casual--variable-to-checkbox v)))

(provide 'casual-labels)
;;; casual-labels.el ends here
