
(in-package :cl-user)

(defpackage :sudoku.system
  (:use :cl :asdf)
  (:export
   :*images-path*))

(in-package :sudoku.system)

(defsystem :sudoku-mcclim
  :depends-on (:mcclim)
  :author "Toru Takaishi <torutakaishi@comcast.net>"
  :version "0.1"
  :components ((:file "package")
               (:file "sudoku" :depends-on ("package"))
               (:file "sudoku-control" :depends-on ("package" "sudoku"))
               (:file "sudoku-ui" :depends-on ("package" "sudoku" "sudoku-control"))))

(defvar *images-path* (asdf:system-relative-pathname 'sudoku-mcclim "images/"))
