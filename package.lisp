
(in-package :cl-user)

(defpackage :sudoku-control
  (:use :cl :sudoku)
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
   :game-ans
   :game-history
   :game-history-pointer
   :game-level
   :game-mask
   :game-memo
   :game-nc
   :game-nr
   :game-table
   :rec-nc
   :rec-nr
   :rec-done
   :rec-new
   :rec-level
   :rec-playing))

(defpackage :sudoku-mcclim
  (:use :clim :clim-lisp :clim-internals
        :sudoku :sudoku-control)
  (:export
   :run
   :add-image))
