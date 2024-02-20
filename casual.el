;;; casual.el --- Transient UI for Calc              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; Casual is an opinionated Transient-based porcelain for Emacs Calc.

;; INSTALLATION
;; (require 'casual)
;; (define-key calc-mode-map (kbd "C-o") 'casual-calc-main-menu)

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)

;; Predicates
(defun casual-matrixp ()
  "Predicate if top of stack is a matrix."
  (if (> (calc-stack-size) 0)
      (math-matrixp (calc-top-n 1))))

(defun casual-square-matrixp ()
  "Predicate if top of stack is a square matrix."
  (if (> (calc-stack-size) 0)
      (math-square-matrixp (calc-top-n 1))))

(defun casual-vectorp ()
  "Predicate if top of stack is a vector."
  (if (> (calc-stack-size) 0)
      (math-vectorp (calc-top-n 1))))

(defun casual-crossp ()
  "Predicate if top two stack items supports a cross product."
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-vectorp arg1)
             (math-vectorp arg2)
             (not (math-matrixp arg1))
             (not (math-matrixp arg2))
             (eq 3 (calcFunc-vlen arg1))
             (eq 3 (calcFunc-vlen arg2))))
    nil))

(defun casual-matrixmultp ()
  "Predicate if top two stack items support matrix multiplication."
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-matrixp arg1)
             (math-matrixp arg2)))
    nil))

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


(defun casual--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if v
      "☑︎"
    "◻︎"))

(defun casual--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual--suffix-label (label prefix)
  "Label constructed with LABEL and SUFFIX separated by a space."
  (format "%s %s" prefix label))

(defun casual--checkbox-label (v label)
  "Casual checkbox label using variable V and LABEL."
  (casual--prefix-label label (casual--variable-to-checkbox v)))


;; Menus
(transient-define-prefix casual-main-menu ()
  "Transient main menu for calc."
  [["Calc"
    ("&" "1/𝑥" calc-inv :transient nil)
    ("Q" " √" calc-sqrt :transient nil)
    ("n" "+∕− " calc-change-sign :transient nil)
    ("^" "𝑦^𝑥" calc-power :transient nil)]
   [""
    ("A" "|𝑥|" calc-abs :transient nil)
    ("!" " !" calc-factorial :transient nil)
    ("%" " ٪" calc-percent :transient nil)
    ("d" " Δ%" calc-percent-change :transient nil)]
   ["Constants"
    ("p" "𝜋" calc-pi :transient nil)
    ("e" "𝑒" (lambda () (interactive) (calc-hyperbolic) (calc-pi)) :transient nil)]
   ["Modes"
    ("m" "Modes›" casual-modes-menu :transient nil)]]
  [["Functions" ; test if anything is on the stack calc-stack-size 0
    ("t" "Trig›" casual-trig-menu :transient nil)
    ("l" "Logarithmic›" casual-logarithmic-menu :transient nil)
    ("b" "Binary›" casual-binary-menu :transient nil)
    ("v" "Vector/Matrix›" casual-vector-menu :transient nil)
    ("c" "Conversions›" casual-conversions-menu :transient nil)
    ("u" "Units›" casual-units-menu :transient nil)]
   ["Stack"
    ;;("s" "Roll Up" calc-roll-up :transient nil)
    ("s" "Swap" calc-roll-down :transient nil)
    ("P" "Pack" calc-pack :transient nil)
    ("U" "Unpack" calc-unpack :transient nil)]])

(transient-define-prefix casual-conversions-menu ()
  ["Conversions"
   ("d" "To Degrees" calc-to-degrees :transient nil)
   ("r" "To Radians" calc-to-radians :transient nil)
   ("h" "To HMS" calc-to-hms :transient nil)])

(transient-define-prefix casual-binary-menu ()
  [["Operators"
    ("a" "and" calc-and :transient nil)
    ("o" "or" calc-or :transient nil)
    ("x" "xor" calc-xor :transient nil)
    ("-" "diff" calc-diff :transient nil)
    ("!" "not" calc-not :transient nil)]
   ["Shift"
    :pad-keys t
    ("l" "binary left" calc-lshift-binary :transient nil)
    ("r" "binary right" calc-rshift-binary :transient nil)
    ("M-l" "arithmetic left" calc-lshift-arith :transient nil)
    ("M-r" "arithmetic right" calc-rshift-arith :transient nil)
    ("C-r" "rotate binary" calc-rotate-binary :transient nil)]
   ["Utils"
    ("R" casual-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-number-radix-label)))
     :transient nil)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros "Leading Zeroes"))
     :transient nil)
    ("w" "Set Word Size" calc-word-size :transient nil)
    ("u" "Unpack Bits" calc-unpack-bits :transient nil)
    ("p" "Pack Bits" calc-pack-bits :transient nil)]])

(transient-define-prefix casual-vector-menu ()
  [
   ["Manipulation"
    ("l" "Length" calc-vlength :transient nil)
    ("f" "Vector Find" calc-vector-find :transient nil) ; requires target N, not a vector
    ("a" "Vector Arrange" calc-arrange-vector :transient nil)
    ("s" "Sort" calc-sort :transient nil) ;; need to use inverse for rsort
    ;; ("h" "Histogram" calc-histogram :transient nil) ; requires prefix
    ("t" "Transpose" calc-transpose :transient nil)
    ("I" "Identity" calc-ident :transient nil)
    ("r" "Reverse" calc-reverse-vector :transient nil)
    ("b" "build vector sequence" calc-index :transient nil)
    ("|" "Concat" calc-concat :transient nil)]
   ["Arithmetic"
    ("T" "Conjugate Transpose" calc-conj-transpose :transient nil)
    ("A" "Frobenius Norm (abs)" calc-abs :transient nil)
    ("R" "Row Norm" calc-rnorm :transient nil)
    ("c" "Column Norm" calc-cnorm :transient nil)
    ("p" "RH Cross Product" calc-cross :if casual-crossp :transient nil)
    ("L" "LU Decomposition" calc-mlud :if casual-matrixp :transient nil)
    ("k" "Kronecker Product" calc-kron :if casual-matrixmultp :transient nil)]
   ["Square Matrix"
    :if casual-square-matrixp
    ("&" "Inverse" calc-inv :transient nil)
    ("d" "Determinant" calc-mdet :transient nil)
    ("t" "Trace" calc-mtrace :transient nil)]
   ["Set"
    ("D" "Deduplicate" calc-remove-duplicates :transient nil)
    ("u" "Union" calc-set-union :transient nil)
    ("i" "Intersect" calc-set-intersect :transient nil)
    ("-" "Difference" calc-set-difference :transient nil)
    ("x" "xor" calc-set-xor :transient nil)
    ("~" "Complement" calc-set-complement :transient nil)
    ("#" "Cardinality" calc-set-cardinality :transient nil)]]

  [;;"⦿ Single Variable Statistics"
   ["Mean and Error"
    ("C" "Vector Count" calc-vector-count :transient nil)
    ("S" "Sum" calc-vector-sum :transient nil)
    ("X" "Max" calc-vector-max :transient nil)
    ("m" "Mean" calc-vector-mean :transient nil)
    ("E" "Mean Error" calc-vector-mean-error :transient nil)
    ("M" "Median" calc-vector-median :transient nil)
    ("h" "Harmonic Mean" calc-vector-harmonic-mean :transient nil)
    ("g" "Geometric Mean" calc-vector-geometric-mean :transient nil)]
   ["Deviation and Variance"
    ("q" "Root Mean Square" calc-vector-rms :transient nil)
    ("1" "Standard Deviation" calc-vector-sdev :transient nil)
    ("2" "Population Standard Deviation" calc-vector-pop-sdev :transient nil)
    ("3" "Variance" calc-vector-variance :transient nil)
    ("4" "Population Variance" calc-vector-pop-variance :transient nil)]
   ["Paired-Sample Statistics" ; predicate for two vectors of the same size
    ("5" "Covariance" calc-vector-covariance :transient nil)
    ("6" "Population Covariance" calc-vector-pop-covariance :transient nil)
    ("7" "Correlation" calc-vector-correlation :transient nil)]])

(transient-define-prefix casual-units-menu ()
  ["Unit Conversions"
   ("c" "Convert" calc-convert-units :transient nil)
   ("t" "Convert Temperature" calc-convert-temperature :transient nil)
   ("b" "Convert to Base Unit" calc-base-units :transient nil)
   ("a" "Autorange" calc-autorange-units :transient nil)
   ("r" "Remove Units" calc-remove-units :transient nil)
   ("x" "Extract Units" calc-extract-units :transient nil)
   ("v" "View Units" calc-view-units-table :transient nil)])

(transient-define-prefix casual-logarithmic-menu ()
  ["Logarithmic Functions"
   :pad-keys t
    ("l" "𝑙𝑛" calc-ln :transient nil)
    ("e" "𝑒^𝑥" calc-exp :transient nil)
    ("L" "𝑙𝑜𝑔𝟣𝟢" calc-log10 :transient nil)
    ("M-l" "𝑙𝑜𝑔" calc-log :transient nil)
    ("M-e" "𝑒^𝑥 - 𝟣" calc-expm1 :transient nil)])

(transient-define-prefix casual-modes-menu ()
  [["Modes"
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual--checkbox-label calc-algebraic-mode
                                            "Algebraic Mode"))
     :transient nil)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros
                                            "Leading Zeroes"))
     :transient nil)
    ("F" calc-frac-mode :description casual-prefer-frac-label :transient nil)
    ("s" calc-symbolic-mode :description casual-symbolic-mode-label :transient nil)
    ("p" calc-polar-mode :description casual-cmplx-or-polar-label :transient nil)
    ;; ("m" calc-matrix-mode :description casual-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("c" "Complex Number Format›" casual-complex-format-menu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (casual-complex-format-label)))
     :transient nil)
    ("S" "Save Calc Settings" calc-save-modes :transient nil)]
   ["Angular Measure"
    ("a" casual-angle-measure-menu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-angle-mode-label)))
     :transient nil)]]
  [["Display"
    ("R" casual-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-number-radix-label)))
     :transient nil)
    ;; TODO show current value float formats
    ("f" "Float Formats›" casual-float-format-menu :transient nil)
    ;; TODO show current value thousands separators
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (casual--checkbox-label calc-group-digits
                                            "Thousands Separators"))
     :transient nil)
    ("," "Set Thousands Separator" calc-group-char :transient nil)
    ("P" "Decimal Separator" calc-point-char :transient nil)
    ("H" "ℎ𝑚𝑠 Format" calc-hms-notation
     :description (lambda ()
                    (format
                     "ℎ𝑚𝑠 Format (%s)"
                     (format calc-hms-format "" "" "")))
     :transient nil)]

   ["Reset"
    ("C-M-r" "Calc Reset" calc-reset :transient nil)]])

(transient-define-prefix casual-angle-measure-menu ()
  ["Angle Measure"
   :description (lambda ()
                  (format "Angle Measure (now %s)›"
                          (casual-angle-mode-label)))
   ("d" "Degrees" calc-degrees-mode :transient nil)
   ("r" "Radians" calc-radians-mode :transient nil)
   ("h" "Degrees-Minutes-Seconds" calc-hms-mode :transient nil)])


(transient-define-prefix casual-complex-format-menu ()
  ["Complex Number Format"
   :description (lambda ()
                  (format "Complex Number Format (now %s)"
                          (casual-complex-format-label)))
   ("c" calc-complex-notation
    :description "complex (rectangular) notation"
    :transient nil)

   ("i" calc-i-notation
    :description "𝑖 notation"
    :transient nil)

   ("j" calc-i-notation
    :description "𝑗 notation"
    :transient nil)])

(transient-define-prefix casual-radix-menu ()
  [["Radix"
    ("0" "Decimal" calc-decimal-radix :transient nil)
    ("2" "Binary" calc-binary-radix :transient nil)
    ("8" "Octal" calc-octal-radix :transient nil)
    ("6" "Hexadecimal" calc-hex-radix :transient nil)
    ("n" "Other" calc-radix :transient nil)]])

(transient-define-prefix casual-float-format-menu ()
  [["Float Format"
    ("n" "Normal" calc-normal-notation :transient nil)
    ("f" "Fixed Point" calc-fix-notation :transient nil)
    ("s" "Scientific" calc-sci-notation :transient nil)
    ("e" "Engineering" calc-eng-notation :transient nil)]])


(transient-define-prefix casual-trig-menu ()
  ;; ["Arguments"
  ;;  ("i" "inverse" "-inverse")
  ;;  ("h" "hyperbolic" "-hyperbolic")]
  [["Trig"
    ("s" "sin" calc-sin :transient nil)
    ("c" "cos" calc-cos :transient nil)
    ("t" "tan" calc-tan :transient nil)]
   ["Inverse"
    ;;("S" "arcsin" calc-arcsin :transient nil)
    ;;(calc-fancy-prefix 'calc-inverse-flag msg n)

    ("S" "arcsin"
     (lambda ()
       (interactive)
       (calc-inverse)
       (calc-sin (calc-top-n 1)))
     :transient nil)

    ("C" "arccos" calc-arccos :transient nil)
    ("T" "arctan" calc-arctan :transient nil)]
   ["Hyperbolic"
    ("M-s" "sinh" calc-sinh :transient nil)
    ("M-c" "cosh" calc-cosh :transient nil)
    ("M-t" "tanh" calc-tanh :transient nil)]
   ["Inverse Hyperbolic"
    ("M-S" "arcsinh" calc-arcsinh :transient nil)
    ("M-C" "arccosh" calc-arccosh :transient nil)
    ("M-T" "arctanh" calc-arctanh :transient nil)]
   ["Angle Measure"
    ("a" casual-angle-measure-menu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-angle-mode-label)))
     :transient nil)]])

;; (defun casual-trig-command (trig prefix-args)

;;   (cond
;;    ((eq trig :sin)
;;     (cond
;;      ((member "-inverse" prefix-args) (calc-arcsin))
;;      ((member "-hyperbolic" prefix-args) (calc-sinh))
;;      (t (calc-sin)))


;;     ))
;;     )


(provide 'casual)
;;; casual.el ends here
