

;; create executable binary
(sb-ext:save-lisp-and-die "/Users/torutakaishi/sudoku"
                          :executable t
                          :toplevel (lambda ()
                                      (sudoku-mcclim::run)
                                      (sb-ext:quit :unix-status 0)))