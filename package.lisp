
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

(defpackage :sudoku-control
  (:use :cl :sudoku :sudoku.system)
  (:export
   ;; classes, methods, functions
   :sudoku-game
   :size
   :set-cell
   :get-cell
   :check
   :undo
   :redo
   :make-sudoku-game
   :add-memo
   :clean-memo
   :game-to-list
   :make-sudoku-game-from-list
   :sudoku-game-record
   :game-exists-p
   :create-new-games
   :move-game
   :to-list
   :make-sudoku-game-record-from-list
   :save-sudoku-game-record
   :load-sudoku-game-record
   :valid-sudoku-game-record-p
   :pick-game-to-play
   ;; accessors of classes
   :ans
   :history
   :history-pointer
   :level
   :mask
   :memo
   :nc
   :nr
   :rec-done
   :rec-new
   :rec-playing
   :table))

(defpackage :sudoku-mcclim
  (:use :clim :clim-lisp :clim-internals
        :sudoku :sudoku-control :sudoku.system))
