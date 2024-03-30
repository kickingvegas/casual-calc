;;; casual-graphics.el --- Casual Graphics Functions  -*- lexical-binding: t; -*-

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
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)

;; Push Example Functions
(defun casual--push-natural-interval-0-100 ()
  "Push inclusive natural interval [0..100] onto stack."
  (interactive)
  (calc-push-list '((intv 3 0 100))))

(defun casual--push-natural-interval-0-360 ()
  "Push inclusive natural interval [0..360] onto stack.
Convert interval to radians to obtain 2𝜋."
  (interactive)
  (calc-push-list '((intv 3 0 360))))

(defun casual--push-float-interval-0-100 ()
  "Push inclusive float interval 0.0 to 100.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float 0 0) (float 1 2)))))

(defun casual--push-float-interval-1-symmetric ()
  "Push inclusive float interval -1.0 to 1.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float -1 0) (float 1 0)))))

(defun casual--push-sin ()
  "Push sin(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-sin (var x var-x)))))

(defun casual--push-cos ()
  "Push cos(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-cos (var x var-x)))))

(defun casual--push-tan ()
  "Push tan(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-tan (var x var-x)))))

(defun casual--push-ln ()
  "Push ln(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-ln (var x var-x)))))

(defun casual--push-e-raised-to-x ()
  "Push e^x onto stack."
  (interactive)
  (calc-push-list '((^ (var e var-e) (var x var-x)))))

(defun casual--push-polynomial-order-2 ()
  "Push x^2 + 1 onto stack.
This can be used as a template formula to edit an order 2 polynomial."
  (interactive)
  (calc-push-list '((+ (^ (var x var-x) 2) 1))))

(defun casual--push-polynomial-order-3 ()
  "Push x^3 + x^2 + 1 onto stack.
This can be used as a template formula to edit an order 3 polynomial."
  (interactive)
  (calc-push-list '((+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1))))

;; Wrapped Calc Graphics Functions

(defun casual--graph-refresh-plot ()
  "Internal function to refresh the Gnuplot canvas.
This function is intended to be called after a calc-graph command
that changes the internal state of Gnuplot is called.  This way
the canvas is updated to support interactive usage.  Invokes
`calc-graph-plot' to do the actual work."
  (call-interactively #'calc-graph-plot))

(defun casual--graph-add ()
  "Add 2D curve to Gnuplot canvas.
This function adds the curve specified by the stack arguments 2: and 1:.

2: x-axis specification
1: y-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification can be either a vector or an algebraic formula with a
single independent variable, typically 𝑥.

Invoking this multiple times will for each time generate a new curve on the same
canvas.  The last curve generated is referred to as the current curve.  The user
can invoke `casual--graph-juggle' to rotate to an arbitrary curve to make it
current.

The number of sample points used to plot a curve can be set by
calling `casual--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b] [c d] [e f]] where [a b],
  ;; [c d], [e f] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-add-3d ()
  "Add 3D curve to Gnuplot canvas.
This function adds the curve specified by the stack arguments 2: and 1:.

3: x-axis specification
2: y-axis specification
1: z-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification is typically a vector or an interval.
The z-axis specification can be either a vector or an algebraic formula with two
independent variables, typically 𝑥 and 𝑦.

The number of sample points used to plot a curve can be set by
calling `casual--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b c] [d e f] [g h i]] where
  ;; [a b c], [d e f], and [g h i] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add-3d)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-delete ()
  "Delete the current curve."
  (interactive)
  (call-interactively #'calc-graph-delete)
  (casual--graph-refresh-plot))

(defun casual--graph-hide ()
  "Hide the current curve."
  ;; TODO: for some reason this doesn't work as expected.
  (interactive)
  (call-interactively #'calc-graph-hide)
  (casual--graph-refresh-plot))

(defun casual--graph-num-points ()
  "Define number of sample points to plot for a curve.
Note: on some terminals (aqua, X11) setting this value is only
effective on a curve for the first time; subsequent changes to
this value are not honored."
  ;; TODO: apparently with aquaterm, when sample points are defined for a curve,
  ;; it can not be changed. Need to investigate if this is the same with other
  ;; terminals.
  (interactive)
  (call-interactively #'calc-graph-num-points)
  (casual--graph-refresh-plot))

;; (defun casual--graph-line-style ()
;;   (interactive)
;;   (let* ((linestyle (transient-arg-value "--linestyle=" (transient-args transient-current-command))))
;;          ;;(current-prefix-arg (if linestyle (string-to-number linestyle) nil)))
;;     ;;(call-interactively #'calc-graph-line-style)
;;     (if (not linestyle)
;;         (call-interactively #'calc-graph-line-style)
;;       (calc-graph-line-style (string-to-number linestyle)))
;;     (casual--graph-refresh-plot)))

(defun casual--graph-toggle-line-style ()
  "Toggle whether a line is rendered for the current curve.
Defining a line style is not yet supported in Casual."
  (interactive)
  (call-interactively #'calc-graph-line-style)
  (casual--graph-refresh-plot))

(defun casual--graph-toggle-point-style ()
  "Toggle whether points are rendered for the current curve.
Defining a point style is not yet supported in Casual."
  (interactive)
  (call-interactively #'calc-graph-point-style)
  (casual--graph-refresh-plot))


(defun casual--graph-juggle ()
  "Change the current curve by rotating through the set of added curves.
Note that direct selection of a curve is not yet supported in Casual."
  ;; OBSERVATION (kickingvegas): Calc's notion of juggling is an unfortunate
  ;; design decision because it imposes an unnatural means to access a curve.
  ;; More ideal would a curve abstraction that supports direct manipulation.
  (interactive)
  (call-interactively #'calc-graph-juggle)
  (casual--graph-refresh-plot))

(defun casual--graph-grid ()
  "Toggle canvas grid."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-grid)
  (casual--graph-refresh-plot))

(defun casual--graph-key ()
  "Toggle canvas legend of defined curves."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-key)
  (casual--graph-refresh-plot))

(defun casual--graph-border ()
  "Toggle canvas border."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-border)
  (casual--graph-refresh-plot))

(defun casual--graph-header ()
  "Set string for canvas title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-header)
  (casual--graph-refresh-plot))

(defun casual--graph-name ()
  "Set string for current curve name.
This string name is used in the canvas legend (key)."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-name)
  (casual--graph-refresh-plot))

(defun casual--graph-title-x ()
  "Set string for x-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-x)
  (casual--graph-refresh-plot))

(defun casual--graph-title-y ()
  "Set string for y-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-y)
  (casual--graph-refresh-plot))

(defun casual--graph-title-z ()
  "Set string for z-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-z)
  (casual--graph-refresh-plot))

(defun casual--graph-zero-x ()
  "Toggle solid line for y=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-x)
  (casual--graph-refresh-plot))

(defun casual--graph-zero-y ()
  "Toggle solid line for x=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-y)
  (casual--graph-refresh-plot))

(defun casual--graph-log-x ()
  "Toggle linear/log scaling for x-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-x)
  (casual--graph-refresh-plot))

(defun casual--graph-log-y ()
  "Toggle linear/log scaling for y-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-y)
  (casual--graph-refresh-plot))

(defun casual--graph-log-z ()
  "Toggle linear/log scaling for z-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-z)
  (casual--graph-refresh-plot))

;; Menus

(transient-define-prefix casual-plot-menu ()
  "Casual plot menu."
  ["Graphics"
   ["Curve"
    :pad-keys t
    ("a" "Add 2D" casual--graph-add :transient t)
    ("A" "Add 3D" casual--graph-add-3d :transient t)
    ("d" "Delete" casual--graph-delete :transient t)
    ("j" "Juggle" casual--graph-juggle :transient t)
    ("s" "Style›" casual-curve-style-menu :transient nil)]
   ["Canvas"
    ;;("f" "Plot 2D Fast" calc-graph-fast :transient t) ;; obsolete
    ;;("F" "Plot 3D Fast" calc-graph-fast-3d :transient t) ;; obsolete
    ("r" "Redraw" calc-graph-plot :transient t)
    ("c" "Clear" calc-graph-clear :transient t)
    ;;("h" "Toggle Hide" casual--graph-hide :transient t) ; does this even work?
    ("n" "# Data Points…" casual--graph-num-points :transient t)
    ;; force redraw by setting current-prefix-arg to -1 and call calc-graph-plot
    ("S" "Style›" casual-plot-options-menu :transient t)]
   ["Utils"
    ("g" "Settings›" casual-graph-settings-menu :transient nil)
    ("p" "Print" calc-graph-print
     ;:description (lambda () (format "Print (%s)…" calc-gnuplot-print-device))
     :transient nil)
    ("C" "Raw Command…" calc-graph-command :transient nil)]]

  ["Data"
   :pad-keys t
   ("e" "Examples›" casual-graph-examples-menu :transient nil)]
   ;;("v" "Vector/Matrix›" casual-vector-menu :transient nil)
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-plot-options-menu ()
  "Casual plot options menu."
  ["Plot Styles"
   ["Canvas"
    ("t" "Set Title…" casual--graph-header :transient t)
    ("k" "Key" casual--graph-key :transient t)
    ("g" "Grid" casual--graph-grid :transient t)
    ("b" "Border" casual--graph-border :transient t)]

   ["Curve"
    ("n" "Name…" casual--graph-name :transient t)
    ("l" "Toggle Line" casual--graph-toggle-line-style :transient t)
    ("p" "Toggle Point" casual--graph-toggle-point-style :transient t)
    ("j" "Juggle" casual--graph-juggle :transient t)]]

  ["Axes"
   ["Title"
    ("x" "Set 𝑥…" casual--graph-title-x :transient t); refactor to new axis menu
    ("y" "Set 𝑦…" casual--graph-title-y :transient t)
    ("z" "Set 𝑧…" casual--graph-title-z :transient t)]

   ["Log/Linear"
    ("X" "Toggle 𝑥" casual--graph-log-x :transient t)
    ("Y" "Toggle 𝑦" casual--graph-log-y :transient t)
    ("Z" "Toggle 𝑧" casual--graph-log-z :transient t)]

   ["Zero Axis"
    ("1" "Toggle 𝑥" casual--graph-zero-x :transient t); refactor to new menu
    ("2" "Toggle 𝑦" casual--graph-zero-y :transient t)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-graph-examples-menu ()
  "Menu of plot examples."
  ["Intervals"
   ["Natural"
    :pad-keys t
    ("a" "insert [0..100]" casual--push-natural-interval-0-100 :transient t)
    ("b" "insert [0..360]" casual--push-natural-interval-0-360 :transient t)]
   ["Float"
    :pad-keys t
    ("c" "insert [0.0 .. 100.0]"
     casual--push-float-interval-0-100 :transient t)
    ("d" "insert [-1.0 .. 1.0]"
     casual--push-float-interval-1-symmetric :transient t)]]

  ["Curves"
   ["Trigonometric"
    :pad-keys t
    ("1" "𝑠𝑖𝑛(𝑥)" casual--push-sin :transient t)
    ("2" "𝑐𝑜𝑠(𝑥)" casual--push-cos :transient t)
    ("3" "𝑡𝑎𝑛(𝑥)" casual--push-tan :transient t)]

   ["Logarithmic"
    :pad-keys t
    ("4" "𝑙𝑛(𝑥)" casual--push-ln :transient t)
    ("5" "𝑒^𝑥" casual--push-e-raised-to-x :transient t)]

   ["Polynomial"
    :pad-keys t
    ("6" "𝑥² + 𝟣" casual--push-polynomial-order-2 :transient t)
    ("7" "𝑥³ + 𝑥² + 𝟣" casual--push-polynomial-order-3 :transient t)]]

  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)
          ("U" "Undo Stack" calc-undo :transient t)])

(transient-define-prefix casual-graph-settings-menu ()
  "Graphics settings menu."
  ["Graphics Settings"
    ("d" "Set Device…" calc-graph-device
     ;:description (lambda () (format "Device (%s)…" calc-gnuplot-default-device)) this variable only initializes.
     :transient nil)
    ("o" "Set Output File…" calc-graph-output
     ;:description (lambda () (format "Output (%s)…" calc-gnuplot-default-output))
     :transient nil)
    ("Q" "Quit Gnuplot Session" calc-graph-quit :transient nil)
   ]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-curve-style-menu ()
  "Style curve menu."
  ["Style Curve"
   ["Line"
    ("l" "Toggle Line" casual--graph-toggle-line-style :transient t)]
   ["Point"
    ("p" "Toggle Point" casual--graph-toggle-point-style :transient t)]]
  [:class transient-row
          ("C-g" "‹Back" ignore :transient transient--do-return)
          ("q" "Dismiss" ignore :transient transient--do-exit)])


;; (transient-define-prefix casual-curve-style-menu ()
;;   "hey there"
;;   ;;:value '("--linestyle=1")
;;   [
;;    ["Line"
;;     ("L" "linestyle [1..6]" "--linestyle="
;;      :choices ("1" "2" "3" "4" "5"))
;;     ("l" "Toggle Line Style" casual--graph-line-style :transient t) ;; set prefix
;;     ]
;;    ["Point"
;;     ("p" "Toggle Point Style" casual--graph-point-style :transient t) ;; set prefix ]
;;     ]
;;    ]

;;   [:class transient-row
;;           ("C-g" "‹Back" ignore :transient transient--do-return)
;;           ("q" "Dismiss" ignore :transient transient--do-exit)
;;           ("U" "Undo Stack" calc-undo :transient t)])

(provide 'casual-graphics)
;;; casual-graphics.el ends here
