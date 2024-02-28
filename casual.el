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
;; (define-key calc-mode-map (kbd "C-o") 'casual-main-menu)

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

(defun casual-float-format-label (&optional include-precision)
  "Label for Calc float mode.
If INCLUDE-PRECISION is non-nil, then add precision to label."
  (let* ((mode (pcase (car calc-float-format)
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
  (if v "☑︎" "◻︎"))

(defun casual--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual--suffix-label (label prefix)
  "Label constructed with LABEL and PREFIX separated by a space."
  (format "%s %s" prefix label))

(defun casual--checkbox-label (v label)
  "Casual checkbox label using variable V and LABEL."
  (casual--prefix-label label (casual--variable-to-checkbox v)))

;; Menus
(transient-define-prefix casual-main-menu ()
  "Casual main menu."
  [["Calc"
    :pad-keys t
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

  [["Arithmetic"
    :pad-keys t
    ("M-r" "Rounding›" casual-rounding-menu :transient nil)
    ("c" "Conversion›" casual-conversions-menu :transient nil)
    ("T" "Time›" casual-time-menu :transient nil)
    ("i" "Complex›" casual-complex-number-menu :transient nil)
    ("R" "Random›" casual-random-number-menu :transient nil)]

   ["Functions" ; test if anything is on the stack calc-stack-size 0
    ("t" "Trigonometric›" casual-trig-menu :transient nil)
    ("l" "Logarithmic›" casual-logarithmic-menu :transient nil)
    ("b" "Binary›" casual-binary-menu :transient nil)
    ("v" "Vector/Matrix›" casual-vector-menu :transient nil)
    ("u" "Units›" casual-units-menu :transient nil)]
   ["Stack"
    :pad-keys t
    ("s" "Swap" calc-roll-down :transient t)
    ("r" "Roll" (lambda ()
                    (interactive (calc-roll-down (calc-stack-size))))
     :transient t)
    ("P" "Pack" calc-pack :transient nil)
    ("u" "Unpack" calc-unpack :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-rounding-menu ()
  "Casual rounding functions menu."
  ["Rounding Functions"
   ("r" "Round" calc-round :transient nil)
   ("f" "Floor" calc-floor :transient nil)
   ("c" "Ceiling" calc-ceiling :transient nil)
   ("t" "Truncate" calc-trunc :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-conversions-menu ()
  "Casual conversion functions menu."
  ["Conversions"
   ("d" "To Degrees" calc-to-degrees :transient nil)
   ("r" "To Radians" calc-to-radians :transient nil)
   ("h" "To HMS" calc-to-hms :transient nil)
   ("f" "To Fraction" calc-fraction :transient nil)
   ("F" "To Float" calc-float :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-time-menu ()
  "Casual time functions menu."
  ["Time"
   ("n" "Now" calc-now :transient nil)
   ("f" "First Day of›" casual-first-day-menu :transient nil)
   ("i" "Increment Month" calc-inc-month :transient nil)
   ("u" "To Unix Time" calc-unix-time :transient nil)
   ("+" "Add Business Days" calc-business-days-plus :transient nil)
   ("-" "Subtract Business Days" calc-business-days-minus :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-first-day-menu ()
  "Casual time first day of menu."
  ["First Day Of"
   ("w" "Week" calc-new-week :transient nil)
   ("m" "Month" calc-new-month :transient nil)
   ("y" "Year" calc-new-year :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-complex-number-menu ()
  "Casual complex number functions menu."
  ["Complex Number"
   ("r" "Real Part" calc-re :transient nil)
   ("i" "Imaginary Part" calc-im :transient nil)
   ("c" "Complex Conjugate" calc-conj :transient nil)
   ("a" "Argument" calc-argument :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-random-number-menu ()
  "Casual random number functions menu."
  ["Random Number (𝑛 is 𝟣: on stack)\n"
   ("r" "Random number within [0..𝑛)" calc-random :transient nil)
   ("a" "Random number again" calc-random-again :transient nil)
   ("c" "Random real number (0 ≤ 𝑛 < 1.0)" calc-rrandom :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-binary-menu ()
  "Casual binary functions menu."
  ["Binary Functions"
   ["Operators"
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
    ("p" "Pack Bits" calc-pack-bits :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-vector-menu ()
  "Casual vector and matrix functions top-level menu."
  ["Vector & Matrix Functions (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
   ["Categories"
   ("b" "Building›" casual-vector-building-menu :transient nil)
   ("a" "Arithmetic›" casual-vector-arithmetic-menu :transient nil)
   ("s" "Statistics›" casual-statistics-menu :transient nil)
   ("S" "Set Operations›" casual-set-operations-menu :transient nil)
   ("m" "Map, Reduce, Apply›" casual-map-and-reduce-menu :transient nil)]

   ["Pack"
    ("p" "Pack (𝑛)" calc-pack :transient nil)
    ("u" "Unpack" calc-unpack :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-vector-building-menu ()
  "Casual vector building functions menu."
  ["Vector Building (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
   ["Build"
    ("|" "Concat" calc-concat :transient nil)
    ("i" "index(1..𝑛)" calc-index :transient nil)
    ("e" "Enumerate (𝑛: interval)" calc-set-enumerate :transient nil)
    ("I" "Identity" calc-ident :transient nil)
    ("d" "Diagonal" calc-diag :transient nil)
    ("b" "Build Vector" calc-build-vector :transient nil)]

   ["Manipulate"
    ("t" "Transpose" calc-transpose :transient nil)
    ("r" "Reverse" calc-reverse-vector :transient nil)
    ("a" "Vector Arrange" calc-arrange-vector :transient nil)
    ("s" "Sort" calc-sort :transient nil)
    ("p" "Deduplicate" calc-remove-duplicates :transient nil)]

   ["Miscellaneous"
    ("l" "Length" calc-vlength :transient nil)
    ("c" "Vector Count" calc-vector-count :transient nil)
    ("f" "Vector Find (𝑛)" calc-vector-find :transient nil)
    ("h" "Histogram" calc-histogram :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])


(transient-define-prefix casual-vector-arithmetic-menu ()
  "Casual vector arithmetic functions menu."
  [["Arithmetic (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
    ("t" "Conjugate Transpose" calc-conj-transpose :transient nil)
    ("A" "Frobenius Norm (|𝑛|)" calc-abs :transient nil)
    ("r" "Row Norm" calc-rnorm :transient nil)
    ("c" "Column Norm" calc-cnorm :transient nil)
    ("p" "RH Cross Product" calc-cross :inapt-if-not casual-crossp :transient nil)
    ("l" "LU Decomposition" calc-mlud :inapt-if-not casual-matrixp :transient nil)
    ("k" "Kronecker Product" calc-kron :inapt-if-not casual-matrixmultp :transient nil)]
   ["Square Matrix"
    ("&" "Inverse" calc-inv :inapt-if-not casual-square-matrixp :transient nil)
    ("d" "Determinant" calc-mdet :inapt-if-not casual-square-matrixp  :transient nil)
    ("t" "Trace" calc-mtrace :inapt-if-not casual-square-matrixp :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])


(transient-define-prefix casual-statistics-menu ()
  "Casual statistic functions menu."
  ["Statistics (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
   ["Mean and Error"
    ("c" "Vector Count" calc-vector-count :transient nil)
    ("s" "Sum" calc-vector-sum :transient nil)
    ("x" "Max" calc-vector-max :transient nil)
    ("m" "Mean" calc-vector-mean :transient nil)
    ("e" "Mean Error" calc-vector-mean-error :transient nil)
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
    ("7" "Correlation" calc-vector-correlation :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-set-operations-menu ()
  "Casual set functions menu."
  ["Set Operations"
    ("d" "Deduplicate" calc-remove-duplicates :transient nil)
    ("u" "Union" calc-set-union :transient nil)
    ("i" "Intersect" calc-set-intersect :transient nil)
    ("-" "Difference" calc-set-difference :transient nil)
    ("x" "xor" calc-set-xor :transient nil)
    ("~" "Complement" calc-set-complement :transient nil)
    ("#" "Cardinality" calc-set-cardinality :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-units-menu ()
  "Casual unit conversions menu."
  ["Unit Conversions"
   ("c" "Convert" calc-convert-units :transient nil)
   ("t" "Convert Temperature" calc-convert-temperature :transient nil)
   ("b" "Convert to Base Unit" calc-base-units :transient nil)
   ("a" "Autorange" calc-autorange-units :transient nil)
   ("r" "Remove Units" calc-remove-units :transient nil)
   ("x" "Extract Units" calc-extract-units :transient nil)
   ("v" "View Units" calc-view-units-table :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-map-and-reduce-menu ()
  "Casual functional operations (map, reduce, apply) menu."
  ["Functional Operators"
   ("m" "map" calc-map :transient nil)
   ("r" "reduce" calc-reduce :transient nil)
   ("a" "apply" calc-apply :transient nil)
   ("A" "accumulate" calc-accumulate :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-logarithmic-menu ()
  "Casual logarithmic functions."
  ["Logarithmic Functions"
   :pad-keys t
    ("l" "𝑙𝑛" calc-ln :transient nil)
    ("e" "𝑒^𝑥" calc-exp :transient nil)
    ("L" "𝑙𝑜𝑔𝟣𝟢" calc-log10 :transient nil)
    ("M-l" "𝑙𝑜𝑔" calc-log :transient nil)
    ("M-e" "𝑒^𝑥 - 𝟣" calc-expm1 :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-modes-menu ()
  "Casual modes menu."
  [["Modes"
    :pad-keys t
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual--checkbox-label calc-algebraic-mode
                                            "Algebraic Mode"))
     :transient t)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual--checkbox-label calc-leading-zeros
                                            "Leading Zeroes"))
     :transient t)
    ("F" calc-frac-mode :description casual-prefer-frac-label :transient t)
    ("s" calc-symbolic-mode :description casual-symbolic-mode-label :transient t)
    ("p" calc-polar-mode :description casual-cmplx-or-polar-label :transient t)
    ;; ("m" calc-matrix-mode :description casual-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("c" "Complex Number Format›" casual-complex-format-menu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (casual-complex-format-label)))
     :transient t)
    ("M-p" calc-precision
     :description (lambda ()
                    (format "Precision (now %d)" calc-internal-prec))
     :transient t)
    ("S" "Save Calc Settings" calc-save-modes :transient t)]
   ["Angular Measure"
    ("a" casual-angle-measure-menu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-angle-mode-label)))
     :transient t)]]
  [["Display"
    ("R" casual-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-number-radix-label)))
     :transient t)
    ("f" casual-float-format-menu
     :description (lambda ()
                    (format "Float Formats (now %s)›"
                            (casual-float-format-label)))
     :transient t)
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (casual--checkbox-label calc-group-digits
                                            "Thousands Separators"))
     :transient t)
    ;; TODO show current value thousands separators
    ("," "Set Thousands Separator" calc-group-char :transient t)
    ("P" "Decimal Separator" calc-point-char :transient t)
    ("H" "ℎ𝑚𝑠 Format" calc-hms-notation
     :description (lambda ()
                    (format
                     "ℎ𝑚𝑠 Format (%s)"
                     (format calc-hms-format "" "" "")))
     :transient t)]

   ["Reset"
    ("C-M-r" "Calc Reset" calc-reset :transient t)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-angle-measure-menu ()
  "Casual angle measure functions menu."
  ["Angle Measure"
   :description (lambda ()
                  (format "Angle Measure (now %s)›"
                          (casual-angle-mode-label)))
   ("d" "Degrees" calc-degrees-mode :transient nil)
   ("r" "Radians" calc-radians-mode :transient nil)
   ("h" "Degrees-Minutes-Seconds" calc-hms-mode :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-complex-format-menu ()
  "Casual complex formats menu."
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
    :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-radix-menu ()
  "Casual radix functions menu."
  ["Radix (𝑛 is 𝟣: on stack)"
   ("0" "Decimal" calc-decimal-radix :transient nil)
   ("2" "Binary" calc-binary-radix :transient nil)
   ("8" "Octal" calc-octal-radix :transient nil)
   ("6" "Hexadecimal" calc-hex-radix :transient nil)
   ("n" "Other base 𝑛" calc-radix :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-float-format-menu ()
  "Casual float formats menu."
  ["Float Format (𝑛 is 𝟣: on stack)"
   ("n" "Normal" calc-normal-notation :transient nil)
   ("f" "Fixed Point 𝑛" calc-fix-notation :transient nil)
   ("s" "Scientific" calc-sci-notation :transient nil)
   ("e" "Engineering" calc-eng-notation :transient nil)]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-trig-menu ()
  "Casual trigonometric functions menu."
  ;; ["Arguments"
  ;;  ("i" "inverse" "-inverse")
  ;;  ("h" "hyperbolic" "-hyperbolic")]
  [["Trig"
    ("s" "sin" calc-sin :transient nil)
    ("c" "cos" calc-cos :transient nil)
    ("t" "tan" calc-tan :transient nil)]
   ["Inverse"
    ("S" "arcsin" calc-arcsin :transient nil)
    ("T" "arctan" calc-arctan :transient nil)
    ("C" "arccos" calc-arccos :transient nil)]

   ["Angle Measure"
    ("a" casual-angle-measure-menu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-angle-mode-label)))
     :transient nil)]]
  [("h" "Hyperbolic›" casual-hyperbolic-trig-menu :transient nil)
   ("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(transient-define-prefix casual-hyperbolic-trig-menu ()
  "Casual hyperbolic trigonometric functions menu."
  [["Hyperbolic"
    ("s" "sinh" calc-sinh :transient nil)
    ("c" "cosh" calc-cosh :transient nil)
    ("t" "tanh" calc-tanh :transient nil)]
   ["Inverse Hyperbolic"
    ("S" "arcsinh" calc-arcsinh :transient nil)
    ("C" "arccosh" calc-arccosh :transient nil)
    ("T" "arctanh" calc-arctanh :transient nil)]]
  [("q" "Dismiss" (lambda () (interactive)) :transient transient--do-exit)])

(provide 'casual)
;;; casual.el ends here
