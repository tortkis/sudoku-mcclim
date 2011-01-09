;;; sudoku

;;; Copyright (C) 2010, Toru Takaishi <torutakaishi@comcast.net>
;;; All rights reserved.

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :sudoku
  (:use :cl)
  (:export
   :stbl
   ;; functions
   :make-multiple-sudoku-table
   :make-sudoku-mask
   :blank-cells
   :getcell
   :setcell
   :stbl-nr
   :stbl-nc
   :stbl-dimension
   :compare-table
   :count-masked-cells
   :check-sudoku
   :empty-cell-p
   :empty-cell))

(in-package :sudoku)
  
(defconstant empty-cell 0)

(defun empty-cell-p (x) (eql x empty-cell))

(defvar *sudoku-use-random* t
  "For debugging, set this to nil to get repeatable results.")

(defstruct stbl
  (table nil)
  (nr 3)
  (nc 3)
  (dimension 9))

(defun make-sudoku-table (nr nc &optional tbl)
  "Make a table with the size of (nr x nc)^2"
  (let* ((size (* nr nc)))
    (make-stbl
     :table (if tbl
                (make-array (list size size)
                            :displaced-to
                            (copy-seq
                             (make-array (expt size 2)
                                         :displaced-to (if (eql (type-of tbl) 'stbl)
                                                           (stbl-table tbl)
                                                           tbl))))
                ;; slow?
                ;;(adjust-array (make-array (list size size) :displaced-to tbl)
                ;;              (list size size))
                (make-array (list size size)
                            :element-type 'integer
                            :initial-element empty-cell))
     :nr nr
     :nc nc
     :dimension size)))

(defun getcell (tbl row col)
  (aref (stbl-table tbl) row col))

(defun setcell (tbl row col num)
  (setf (aref (stbl-table tbl) row col) num))

(defun compare-table (tbl1 tbl2)
  (let ((dim (stbl-dimension tbl1)))
    (and (= dim (stbl-dimension tbl2))
         (= (stbl-nr tbl1) (stbl-nr tbl2))
         (= (stbl-nc tbl1) (stbl-nc tbl2))
         (every #'eval (map 'vector #'eql
                            (make-array (* dim dim) :displaced-to (stbl-table tbl1))
                            (make-array (* dim dim) :displaced-to (stbl-table tbl2)))))))

;;

(defun num-exists-row (tbl row num)
  (let ((if-exists nil))
    (dotimes (col (stbl-dimension tbl))
      (when (eql (getcell tbl row col) num)
        (setf if-exists t)))
    if-exists))

(defun num-exists-col (tbl col num)
  (let ((if-exists nil))
    (dotimes (row (stbl-dimension tbl))
      (when (eql (getcell tbl row col) num)
        (setf if-exists t)))
    if-exists))

(defun filter-table (tbl filter-fn)
  (let ((lis '()))
    (dotimes (r (stbl-dimension tbl))
      (dotimes (c (stbl-dimension tbl))
        (when (funcall filter-fn (getcell tbl r c))
          (push (list r c) lis))))
    lis))

(defun blank-cells (tbl)
  (filter-table tbl #'(lambda (x) (empty-cell-p x))))

(defun random-sort (lis)
  (if *sudoku-use-random* 
      (labels ((random-pop (lis out)
                 (cond ((null lis) out)
                       (t (let ((elt (nth (random (length lis)) lis)))
                            (random-pop (remove elt lis) (cons elt out)))))))
        (random-pop lis '()))
      lis))

(defun set-cell-in-block (tbl blk-row blk-col row col val)
  (let ((nr (stbl-nr tbl)) (nc (stbl-nc tbl)))
    (cond ((find-if #'(lambda (x) (>= x (* nr nc))) (list blk-row blk-col row col))
           nil)
          (t (let ((tbl-new (make-sudoku-table nr nc tbl)))
               (setcell tbl-new (+ (* nr blk-row) row) (+ (* nc blk-col) col) val)
               tbl-new)))))

(defun get-sub-block (tbl blk-row blk-col)
  (let ((nr (stbl-nr tbl)) (nc (stbl-nc tbl))
        (lis '()))
    (when (and (< blk-row nc) (< blk-col nr))
      (dotimes (row nr)
        (dotimes (col nc)
          (push (getcell tbl (+ (* nr blk-row) row) (+ (* nc blk-col) col)) lis)))
      (nreverse lis))))

(defun blank-cells-in-block (tbl blk-row blk-col &optional excl-num)
  "Return a list of (row col) of empty cells in the block of blk-row & blk-col,
   if excl-num is specified, exclude the rows and columns in which excl-num exists."
  (let ((nr (stbl-nr tbl)) (nc (stbl-nc tbl)))
    (unless (or (>= blk-row nc) (>= blk-col nr))
      (let ((blank-cells '()))
        (dotimes (row nr)
          (dotimes (col nc)
            (let ((abs-row (+ (* nr blk-row) row))
                  (abs-col (+ (* nc blk-col) col)))
              (when (and (empty-cell-p (getcell tbl abs-row abs-col))
                         (not (and excl-num
                                   (or (num-exists-row tbl abs-row excl-num)
                                       (num-exists-col tbl abs-col excl-num)
                                       (find excl-num (get-sub-block tbl blk-row blk-col))))))
                (push (list row col) blank-cells)))))
        blank-cells))))

(defun make-num-blk-seq (nr nc)
  (let ((num-blk-seq nil))
    (dotimes (num (* nr nc))
      (dotimes (blk-row nc)
        (dotimes (blk-col nr)
          (push (list (1+ num) blk-row blk-col) num-blk-seq))))
    (nreverse num-blk-seq)))

(defun possible-placement-all (tbl)
  (let ((num-blk-seq (make-num-blk-seq (stbl-nr tbl) (stbl-nc tbl))))
    (sort
     (remove-if #'null
                (mapcar #'(lambda (blk)
                            (list (subseq blk 0 3)
                                  (blank-cells-in-block
                                   tbl (second blk) (third blk) (first blk))))
                        num-blk-seq)
             :key #'second)
     #'< :key #'(lambda (x) (length (second x))))))

(defun possible-placement-remove-invalid-1 (tbl)
  (labels ((rem-invalid (plc-lis)
             (cond ((null plc-lis) nil)
                   (t (let ((plc-num (length plc-lis))
                            (plc (car plc-lis))
                            (valid-sel nil))
                        (dolist (sel (cadr plc))
                          (let ((tmp-tbl (make-sudoku-table (stbl-nr tbl) (stbl-nc tbl) tbl)))
                            (setcell tmp-tbl
                                     (+ (* (second (car plc)) (stbl-nr tbl)) (first sel))
                                     (+ (* (third (car plc)) (stbl-nc tbl)) (second sel))
                                     (first (car plc)))
                            (when (= (1- plc-num)
                                     (length (possible-placement-all tmp-tbl)))
                              (push sel valid-sel))))
                        (if valid-sel
                            (list (car plc) valid-sel)
                            (rem-invalid (cdr plc-lis))))))))
    (rem-invalid (possible-placement-all tbl))))

(defun fill-uniq-all (tbl)
  (let* ((placement (possible-placement-all tbl))
         (placement-uniq (remove-if-not #'(lambda (x) (= (length (second x)) 1))
                                        placement))
         (tbl-new (make-sudoku-table (stbl-nr tbl) (stbl-nc tbl) tbl))
         (failed nil))
    (cond ((null placement-uniq)
           (let ((next-placement (possible-placement-remove-invalid-1 tbl)))
             (if (and (null next-placement) (blank-cells tbl))
                 (list :failed nil)
                 (list tbl next-placement))))
          (t
           (dolist (plc placement-uniq)
             (let* ((blk-row (second (first plc)))
                    (blk-col (third (first plc)))
                    (rel-row (first (car (second plc))))
                    (rel-col (second (car (second plc))))
                    (row (+ (* blk-row (stbl-nr tbl)) rel-row))
                    (col (+ (* blk-col (stbl-nc tbl)) rel-col))
                    (num (caar plc)))
               (if (or (num-exists-col tbl-new col num)
                       (num-exists-row tbl-new row num))
                   (setf failed t)
                   (setcell tbl-new row col num))))
           (if failed
               (list :failed nil)
               (fill-uniq-all tbl-new))))))
                      
(defun make-sudoku-table-1 (nr nc &key initial-tbl-lis random)
  "Return a list with a valid sudoku table and partial (incomplete) sudoku tables.
   If initial-tbl-lis is specified, it is used as the initial table,
   starting with the first one."
  (labels
      ((next-fill (tbl-lis)
         ;; tbl-lis item: table idx blk-seq
         (cond ((null tbl-lis) nil)
               (t
                (let* ((tbl-start-set (car tbl-lis))
                       (tbl-start (first tbl-start-set))
                       (num-blk-next (or (second tbl-start-set)
                                         (if random
                                             (car (random-sort (possible-placement-all tbl-start)))
                                             (possible-placement-remove-invalid-1 tbl-start)))))
                  (cond ((= 0 (length (blank-cells tbl-start)))
                         tbl-lis)
                        ((null num-blk-next)
                         (next-fill (cdr tbl-lis)))
                        (t
                         (let* ((num (first (first num-blk-next)))
                                (blk-row (second (first num-blk-next)))
                                (blk-col (third (first num-blk-next)))
                                (tbl-lis-add '()))
                           (dolist (avail-cell (if random
                                                   (random-sort (second num-blk-next))
                                                   (second num-blk-next)))
                             (let ((tbl-new
                                    (fill-uniq-all
                                     (set-cell-in-block
                                      tbl-start blk-row blk-col
                                      (first avail-cell) (second avail-cell) num))))
                               (when (not (eql (car tbl-new) :failed))
                                 (push tbl-new tbl-lis-add))))
                           (next-fill (if tbl-lis-add
                                          (append tbl-lis-add (cdr tbl-lis))
                                          (cdr tbl-lis)))))))))))
    (next-fill (or initial-tbl-lis
                   (list (list (make-sudoku-table nr nc) nil))))))

(defun make-multiple-sudoku-table (c tbl-or-rc &key random)
  "Make a list of c tables of (nr x nc)^2 sudoku, starting with initial-tbl if specified.
   If c is less than 0, make sudoku tables as many as possible."
  (let (nr nc initial-tbl-lis)
    (cond ((eql (type-of tbl-or-rc) 'stbl)
           (setf nr (stbl-nr tbl-or-rc)
                 nc (stbl-nc tbl-or-rc))
           (setf initial-tbl-lis (list (fill-uniq-all tbl-or-rc))))
          ((and (listp tbl-or-rc) (= (length tbl-or-rc) 2) (every #'numberp tbl-or-rc))
           (setf nr (first tbl-or-rc)
                 nc (second tbl-or-rc))
           (setf initial-tbl-lis (list (list (make-sudoku-table nr nc) nil))))
          (t (error "make-multiple-sudoku-table: table or '(r c)")))
    (labels ((rec (tbl-lis count)
               (cond
                 ((= count 0) '())
                 (t (let ((tbl-lis-new (make-sudoku-table-1
                                        nr nc :initial-tbl-lis tbl-lis :random random)))
                      (cond
                        ((null tbl-lis-new) '())
                        ((= (length tbl-lis-new) 1) (list (caar tbl-lis-new)))
                        (t (cons (caar tbl-lis-new)
                                 (rec (random-sort (cdr tbl-lis-new)) (1- count))))))))))
      (rec initial-tbl-lis c))))

;; check

(defun get-col (tbl col)
  (when (< col (stbl-dimension tbl))
    (let ((lis '()))
      (dotimes (row (stbl-dimension tbl))
        (push (getcell tbl row col) lis))
      (nreverse lis))))

(defun get-row (tbl row)
  (when (< row (stbl-dimension tbl))
    (let ((lis '()))
      (dotimes (col (stbl-dimension tbl))
        (push (getcell tbl row col) lis))
      (nreverse lis))))

(defun check-sudoku-group (lis)
  (and (equal lis (remove-duplicates lis))
       (eql (apply #'min lis) 1)
       (eql (apply #'max lis) (length lis))))

(defun position-all (elt lis &optional (start 0))
  (let ((pos (position elt lis :start start)))
    (cond ((null pos) '())
          ((< pos (1- (length lis)))
           (cons pos (position-all elt lis (1+ pos))))
          (t (list pos)))))

(defun check-duplicates (lis)
  (let ((dup-lis '()))
    (dotimes (i (length lis))
      (let ((elt (nth i lis)))
        (unless (or (empty-cell-p elt)
                    (find-if #'(lambda (x) (find i x)) dup-lis))
          (push (position-all elt lis) dup-lis))))
    (remove-if #'(lambda (x) (= (length x) 1)) dup-lis)))

(defmacro if-let (c then &optional else)
  `(let ,c
     (if (and ,@(mapcar #'car c))
         ,then
         ,else)))
  
(defun check-sudoku (tbl)
  "Check if the sudoku table is valid. Return 'CORRECT if there are no errors.
   Otherwise, return a list of two lists: a list of (row col)
   and a list of invalid number, block, row, column:
   (INVALID row col invalid-num),
   (BLK block-row block-col list-of-duplicated-block-row-col),
   (ROW row list-of-duplicated-row),
   (COL column list-of-duplicated-col)"
  (if (null tbl)
      'INVALID
      (let ((nr (stbl-nr tbl)) (nc (stbl-nc tbl)))
        (let* ((nmax (* nr nc))
               (fail-rc '())
               (fail-desc '()))
          (dotimes (col nmax)
            (if-let ((dup-lis (check-duplicates (get-col tbl col))))
              (progn (mapc #'(lambda (x) (push (list x col) fail-rc))
                           (apply #'append dup-lis))
                     (push (list 'COL col dup-lis) fail-desc))))
          (dotimes (row nmax)
            (if-let ((dup-lis (check-duplicates (get-row tbl row))))
              (progn (mapc #'(lambda (x) (push (list row x) fail-rc))
                           (apply #'append dup-lis))
                     (push (list 'ROW row dup-lis) fail-desc))))
          (dotimes (blk-row nc)
            (dotimes (blk-col nr)
              (if-let ((dup-lis (check-duplicates (get-sub-block tbl blk-row blk-col))))
                (progn (mapc #'(lambda (x)
                                 (push (list (+ (* blk-row nr) (truncate (/ x nc)))
                                             (+ (* blk-col nc) (mod x nc)))
                                       fail-rc))
                             (apply #'append dup-lis))
                       (push (list 'BLK blk-row blk-col dup-lis) fail-desc)))))
          (dotimes (row nmax)
            (dotimes (col nmax)
              (let ((num (getcell tbl row col)))
                (when (or (< num 1) (> num nmax))
                  (push (list row col) fail-rc)
                  (push (list 'INVALID row col num) fail-desc)))))
          (let ((fail-lis (remove-duplicates
                           fail-rc :test #'(lambda (x y)
                                             (and (eql (first x) (first y))
                                                  (eql (second x) (second y)))))))
            (if (and (null fail-lis) (null fail-desc))
                'CORRECT
                (list fail-lis fail-desc)))))))


(defun max-table (tbl)
  (let ((max-num nil))
    (dotimes (row (stbl-dimension tbl))
      (dotimes (col (stbl-dimension tbl))
        (when (numberp (getcell tbl row col))
          (if (null max-num)
              (setf max-num (getcell tbl row col))
              (setf max-num (max (getcell tbl row col) max-num))))))
    max-num))

(defun count-table (tbl)
  (let* ((max-num (max-table tbl))
         (cnt-lis (when max-num (make-list (1+ max-num) :initial-element 0))))
    (dotimes (row (stbl-dimension tbl))
      (dotimes (col (stbl-dimension tbl))
        (when (numberp (getcell tbl row col))
          (incf (nth (getcell tbl row col) cnt-lis)))))
    cnt-lis))
              
(defun count-table-if (filter-fn tbl)
  (let ((cnt 0))
    (dotimes (row (stbl-dimension tbl))
      (dotimes (col (stbl-dimension tbl))
        (when (funcall filter-fn (getcell tbl row col))
          (incf cnt))))
    cnt))

;; mask

(defun make-random-num-seq (n)
  (let ((seq '()))
    (do ((i 1 (1+ i)))
        ((> i (expt n 2)) (random-sort seq))
      (push i seq))))

(defun mask-sudoku (tbl mask-tbl)
  (let* ((nr (stbl-nr tbl))
         (nc (stbl-nc tbl))
         (nmax (* nr nc))
         (tbl-new (make-sudoku-table nr nc)))
    (dotimes (row nmax)
      (dotimes (col nmax)
        (setcell tbl-new row col
                 (if (> (getcell mask-tbl row col) 0)
                     0
                     (getcell tbl row col)))))
    tbl-new))

(defun solve-sudoku (s)
  (let* ((res (make-multiple-sudoku-table 2 s)))
    (cond ((null res) nil)
          ((= (length res) 1) (car res))
          (t 'MULTIPLE-SOLUTIONS))))

(defun make-sudoku-mask (s-tbl mask-n)
  (let* ((nr (stbl-nr s-tbl))
         (nc (stbl-nc s-tbl))
         (m-tbl (make-sudoku-table nr nc)))
    ;; try to find cells which can be masked as many as possible
    (let ((rc-lis (random-sort (filter-table m-tbl #'(lambda (x) (eql x 0))))))
      (dolist (rc rc-lis)
        (when (< (count-table-if #'(lambda (x) (eql x 1)) m-tbl) mask-n)
          (setcell m-tbl (car rc) (cadr rc) 1)
          (when (< 1 (length (make-multiple-sudoku-table
                              2 (mask-sudoku s-tbl m-tbl))))
            (setcell m-tbl (car rc) (cadr rc) 0)))))
    (if (eql (solve-sudoku (mask-sudoku s-tbl m-tbl)) 'MULTIPLE-SOLUTIONS)
        'FAIL
        (values (mask-sudoku s-tbl m-tbl) s-tbl m-tbl))))

(defun count-masked-cells (m-tbl)
  (length (filter-table m-tbl #'(lambda (x) (eql x 1)))))

(defun print-sudoku (tbl &key (mask-tbl nil) (stream t))
  (let* ((nr (stbl-nr tbl))
         (nc (stbl-nc tbl))
         (nmax (* nr nc)))
    (dotimes (row nmax)
      (when (= (mod row nr) 0)
        (dotimes (i nr) (format stream "+") (dotimes (j nc) (format stream "-")))
        (format stream "+~%"))
      (dotimes (col nmax)
        (when (= (mod col nc) 0)
          (format stream "|"))
        (format stream "~A" (if (or (= (getcell tbl row col) 0)
                                    (and mask-tbl (> (getcell mask-tbl row col) 0)))
                                " "
                                (getcell tbl row col))))
      (format stream "|~%"))
    (dotimes (i nr) (format stream "+") (dotimes (j nc) (format stream "-")))
    (format stream "+~%")))

;; grid format (for (3x3)^2 only)

(defun parse-grid (grid)
  (let* ((nr (truncate (sqrt (sqrt (length grid)))))
         (nc nr)
         (len (* nr nc))
         (tbl (make-sudoku-table nr nc)))
    (dotimes (row (* nr nc))
      (dotimes (col (* nr nc))
        (let ((cell (char grid (+ (* row len) col))))
          (setcell tbl row col
                   (cond ((equal cell #\.) 0)
                         ((digit-char-p cell) (digit-char-p cell))
                         (t (error "grid format error '~A' at ~A"
                                   cell (+ (* row len) col))))))))
    tbl))
                                         
(defun solve-grids (&rest grids)
  (dolist (grid grids)
    (time (princ (sudoku:make-multiple-sudoku-table
                  1 (sudoku::parse-grid grid))))))
  
(defun print-grid (grid)
  (dotimes (n 9)
    (format t "~A~%" (subseq grid (* n 9) (* (1+ n) 9)))))

(defun read-grid-file (fn)
  (let ((grids '()))
    (with-open-file (str fn)
      (do ((line (read-line str nil :eof)
                 (read-line str nil :eof)))
          ((eql line :eof)
           (nreverse grids))
        (if (string-equal (subseq line 0 4) "Grid")
            (let ((g ""))
              (dotimes (i 9)
                (setf g (concatenate 'string g (subseq (read-line str nil :eof) 0 9))))
              (push (parse-grid g) grids))
            (push (parse-grid line) grids))))))

(defun tbl-to-grid (tbl)
  (let ((s (make-string (expt (stbl-dimension tbl) 2))))
    (dotimes (col (stbl-dimension tbl))
      (dotimes (row (stbl-dimension tbl))
        (setf (char s (+ (* row (stbl-dimension tbl)) col))
              (char (write-to-string (getcell tbl row col)) 0))))
    s))

(defun solve-grid-file (fn &optional sp ep)
  (let ((solve-time nil)
        (solved 0))
    (mapc #'(lambda (tbl)
              (let* ((start-time (/ (get-internal-real-time) internal-time-units-per-second))
                     (res (solve-sudoku tbl))
                     (run-time (- (/ (get-internal-real-time) internal-time-units-per-second) start-time)))
                (format t "~8,3,,f ~A~%" run-time
                        (if (eql (type-of res) 'stbl)
                            (progn (incf solved) (tbl-to-grid res))
                            res))
                (push run-time solve-time)))
          (subseq (read-grid-file fn) (or sp 0) ep))
    (format t "Solved ~A of ~A in ~A puzzles (total ~8,3,,f sec, avg ~8,3,,f sec, max ~8,3,,f sec, min ~5,4,,f sec)~%"
            solved (length solve-time) fn
            (reduce #'+ solve-time) (/ (reduce #'+ solve-time) (length solve-time))
            (apply #'max solve-time) (apply #'min solve-time))))

;; example

(defun sudoku-example (nr nc)
  (let* ((s (car (make-multiple-sudoku-table 1 (list nr nc) :random t)))
         (v (make-sudoku-mask s (* (expt (* nr nc) 2) (/ 5 8))))
         (w (make-multiple-sudoku-table 2 v))
         (c (count-table v)))
    (format t "problem:~%")
    (print-sudoku v)
    (format t "# masked cells (total): ~A~%" (car c))
    (format t "# shown cells for each number: ~A~%" (cdr c))
    (format t "solution(s)=~A~%" (length w))
    (mapc #'(lambda (x)
              (print-sudoku x))
          w)
    t))

