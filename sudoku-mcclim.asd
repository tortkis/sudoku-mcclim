
(in-package :cl-user)

(defpackage :sudoku-mcclim-asd
  (:use :cl :asdf))

(in-package :sudoku-mcclim-asd)

(defsystem :sudoku-mcclim
  :name "sudoku-mcclim"
  :author "Toru Takaishi <tortkis@mtlhc.com>"
  :version "0.1"
  :depends-on (:mcclim
               :sudoku)
  :components ((:file "package")
               (:file "sudoku-control" :depends-on ("package"))
               (:file "sudoku-ui-mcclim" :depends-on ("package" "sudoku-control"))))

