
(in-package :cl-user)

(defpackage :sudoku
  (:use :cl)
  (:export
   ;; functions
   :make-multiple-sudoku-table
   :make-sudoku-mask
   :blank-cells
   :check-sudoku
   :empty-cell-p
   :empty-cell))

(defpackage :sudoku-mcclim
  (:use :clim :clim-lisp :clim-internals :sudoku :sudoku.system))
