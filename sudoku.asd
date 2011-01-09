
(in-package :cl-user)

(defpackage :sudoku-asd
  (:use :cl :asdf))

(in-package :sudoku-asd)

(defsystem sudoku
  :name "sudoku"
  :author "Toru Takaishi <tortkis@mtlhc.com>"
  :version "0.1"
  :serial t
  :components ((:file "sudoku")))



