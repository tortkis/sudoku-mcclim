;;; sudoku

(in-package :sudoku)
  
(defconstant empty-cell 0)

(defvar *sudoku-use-random* t
  "For debugging, set this to nil to get repeatable results.")

(defun empty-cell-p (x) (eql x empty-cell))

(defun empty-cell () empty-cell)

(defun make-sudoku-table (nr nc &optional tbl)
  "Make a table with the size of (nr x nc)^2"
  (let* ((size (* nr nc)))
    (if tbl
        (make-array (list size size)
                    :displaced-to (copy-seq (make-array (expt size 2)
                                                        :displaced-to tbl)))
        ;; slow?
        ;;(adjust-array (make-array (list size size) :displaced-to tbl)
        ;;              (list size size))
        (make-array (list size size)
                    :element-type 'integer
                    :initial-element empty-cell))))

(defun num-exists-row (tbl row num)
  (let ((if-exists nil))
    (dotimes (col (array-dimension tbl 1))
      (when (eql (aref tbl row col) num)
        (setf if-exists t)))
    if-exists))

(defun num-exists-col (tbl col num)
  (let ((if-exists nil))
    (dotimes (row (array-dimension tbl 0))
      (when (eql (aref tbl row col) num)
        (setf if-exists t)))
    if-exists))

(defun filter-table (tbl filter-fn)
  (let ((lis '()))
    (dotimes (r (array-dimension tbl 0))
      (dotimes (c (array-dimension tbl 1))
        (when (funcall filter-fn (aref tbl r c))
          (push (list r c) lis))))
    lis))

(defun blank-cells (tbl)
  (filter-table tbl #'(lambda (x) (eql x empty-cell))))

(defun blank-cells-in-block (tbl nr nc blk-row blk-col &optional excl-num)
  "Return a list of (row col) of empty cells in the block of blk-row & blk-col,
   if excl-num is specified, exclude the rows and columns in which excl-num exists."
  (unless (or (>= blk-row nc) (>= blk-col nr))
    (let ((blank-cells '()))
      (dotimes (row nr)
        (dotimes (col nc)
          (let ((abs-row (+ (* nr blk-row) row))
                (abs-col (+ (* nc blk-col) col)))
            (when (and (eql (aref tbl abs-row abs-col) empty-cell)
                       (not (and excl-num
                                 (or (num-exists-row tbl abs-row excl-num)
                                     (num-exists-col tbl abs-col excl-num)))))
              (push (list row col) blank-cells)))))
      blank-cells)))

(defun random-sort (lis)
  (if *sudoku-use-random* 
      (labels ((random-pop (lis out)
                 (cond ((null lis) out)
                       (t (let ((elt (nth (random (length lis)) lis)))
                            (random-pop (remove elt lis) (cons elt out)))))))
        (random-pop lis '()))
      lis))

(defun set-cell-in-block (tbl nr nc blk-row blk-col row col val)
  (cond ((find-if #'(lambda (x) (>= x (* nr nc))) (list blk-row blk-col row col))
         nil)
        (t (let ((tbl-new (make-sudoku-table nr nc tbl)))
             (setf (aref tbl-new (+ (* nr blk-row) row) (+ (* nc blk-col) col)) val)
             tbl-new))))

(defun get-sub-block (tbl nr nc blk-row blk-col)
  (let ((lis '()))
    (when (and (< blk-row nc) (< blk-col nr))
      (dotimes (row nr)
        (dotimes (col nc)
          (push (aref tbl (+ (* nr blk-row) row) (+ (* nc blk-col) col)) lis)))
      (nreverse lis))))

(defun make-num-blk-seq (nr nc)
  (let ((num-blk-seq nil))
    (dotimes (num (* nr nc))
      (dotimes (blk-row nc)
        (dotimes (blk-col nr)
          (push (list (1+ num) blk-row blk-col) num-blk-seq))))
    (nreverse num-blk-seq)))
                      
(defun make-sudoku-table-1 (nr nc &optional initial-tbl-lis)
  "Return a list with a valid sudoku table and partial (incomplete) sudoku tables.
   If initial-tbl-lis is specified, it is used as the initial table,
   starting with the first one."
  (let ((num-blk-seq (make-num-blk-seq nr nc)))
    (labels ((next-fill (tbl-lis idx)
               ;; tbl-lis item: table num blk-row blk-col
               (cond ((null tbl-lis) nil)
                     ((>= idx (length num-blk-seq)) tbl-lis)
                     (t (let* ((tbl-start-set (car tbl-lis))
                               (tbl-start (car tbl-start-set))
                               (idx-prev (position (subseq tbl-start-set 1 4)
                                                   num-blk-seq :test 'equal))
                               (tbl-lis-add '()))
                          (when (numberp idx-prev)
                            (setf idx (1+ idx-prev)))
                          (let* ((num-blk-next (nth idx num-blk-seq))
                                 (num (car num-blk-next))
                                 (blk-row (cadr num-blk-next))
                                 (blk-col (caddr num-blk-next)))
                            (cond
                              ((find num (get-sub-block tbl-start nr nc blk-row blk-col))
                               (next-fill (cons (list tbl-start num blk-row blk-col)
                                                (cdr tbl-lis))
                                          (1+ idx)))
                              (t (dolist (avail-cell (random-sort
                                                      (blank-cells-in-block
                                                       tbl-start nr nc blk-row blk-col num)))
                                   (push (list (set-cell-in-block tbl-start
                                                                  nr nc blk-row blk-col
                                                                  (first avail-cell) (second avail-cell) num)
                                               num blk-row blk-col)
                                         tbl-lis-add))
                                 (next-fill (if tbl-lis-add
                                                (append tbl-lis-add (cdr tbl-lis))
                                                (cdr tbl-lis))
                                            (1+ idx))))))))))
      (next-fill (or initial-tbl-lis
                     (list (list (make-sudoku-table nr nc) -1 -1 -1)))
                 0))))

(defun make-multiple-sudoku-table (nr nc c &optional initial-tbl-lis)
  "Make a list of c tables of (nr x nc)^2 sudoku, starting with initial-tbl-lis if specified.
   If c is less than 0, make sudoku tables as many as possible."
  (labels ((rec (tbl-lis count)
             (cond
               ((= count 0) '())
               (t (let ((tbl-lis-new (make-sudoku-table-1 nr nc tbl-lis)))
                    (cond
                      ((null tbl-lis-new) '())
                      ((= (length tbl-lis-new) 1) (list (caar tbl-lis-new)))
                      (t (cons (caar tbl-lis-new)
                               (rec (random-sort (cdr tbl-lis-new)) (1- count))))))))))
    (rec initial-tbl-lis c)))

;; check

(defun get-col (tbl col)
  (when (< col (array-dimension tbl 1))
    (let ((lis '()))
      (dotimes (row (array-dimension tbl 0))
        (push (aref tbl row col) lis))
      (nreverse lis))))

(defun get-row (tbl row)
  (when (< row (array-dimension tbl 0))
    (let ((lis '()))
      (dotimes (col (array-dimension tbl 1))
        (push (aref tbl row col) lis))
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
        (unless (or (eql elt empty-cell)
                    (find-if #'(lambda (x) (find i x)) dup-lis))
          (push (position-all elt lis) dup-lis))))
    (remove-if #'(lambda (x) (= (length x) 1)) dup-lis)))

(defmacro if-let (c then &optional else)
  `(let ,c
     (if (and ,@(mapcar #'car c))
         ,then
         ,else)))
  
(defun check-sudoku (tbl nr nc)
  "Check if the sudoku table is valid. Return 'CORRECT if there are no errors.
   Otherwise, return a list of two lists: a list of (row col)
   and a list of invalid number, block, row, column:
   (INVALID row col invalid-num),
   (BLK block-row block-col list-of-duplicated-block-row-col),
   (ROW row list-of-duplicated-row),
   (COL column list-of-duplicated-col)"
  (if (null tbl)
      'INVALID
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
            (if-let ((dup-lis (check-duplicates (get-sub-block tbl nr nc blk-row blk-col))))
              (progn (mapc #'(lambda (x)
                               (push (list (+ (* blk-row nr) (truncate (/ x nc)))
                                           (+ (* blk-col nc) (mod x nc)))
                                     fail-rc))
                           (apply #'append dup-lis))
                     (push (list 'BLK blk-row blk-col dup-lis) fail-desc)))))
        (dotimes (row nmax)
          (dotimes (col nmax)
            (let ((num (aref tbl row col)))
              (when (or (< num 1) (> num nmax))
                (push (list row col) fail-rc)
                (push (list 'INVALID row col num) fail-desc)))))
        (let ((fail-lis (remove-duplicates
                         fail-rc :test #'(lambda (x y)
                                           (and (eql (first x) (first y))
                                                (eql (second x) (second y)))))))
          (if (and (null fail-lis) (null fail-desc))
              'CORRECT
              (list fail-lis fail-desc))))))


(defun max-table (tbl)
  (let ((max-num nil))
    (dotimes (row (array-dimension tbl 0))
      (dotimes (col (array-dimension tbl 1))
        (when (numberp (aref tbl row col))
          (if (null max-num)
              (setf max-num (aref tbl row col))
              (setf max-num (max (aref tbl row col) max-num))))))
    max-num))

(defun count-table (tbl)
  (let* ((max-num (max-table tbl))
         (cnt-lis (when max-num (make-list (1+ max-num) :initial-element 0))))
    (dotimes (row (array-dimension tbl 0))
      (dotimes (col (array-dimension tbl 1))
        (when (numberp (aref tbl row col))
          (incf (nth (aref tbl row col) cnt-lis)))))
    cnt-lis))
              
(defun count-table-if (filter-fn tbl)
  (let ((cnt 0))
    (dotimes (row (array-dimension tbl 0))
      (dotimes (col (array-dimension tbl 1))
        (when (funcall filter-fn (aref tbl row col))
          (incf cnt))))
    cnt))

;; mask

(defun make-random-num-seq (n)
  (let ((seq '()))
    (do ((i 1 (1+ i)))
        ((> i (expt n 2)) (random-sort seq))
      (push i seq))))

(defun mask-sudoku (tbl mask-tbl nr nc)
  (let* ((nmax (* nr nc))
         (tbl-new (make-array (list nmax nmax))))
    (dotimes (row nmax)
      (dotimes (col nmax)
        (setf (aref tbl-new row col)
              (if (> (aref mask-tbl row col) 0)
                  0
                  (aref tbl row col)))))
    tbl-new))

(defun solve-sudoku (s nr nc)
  (let ((res (make-multiple-sudoku-table
              nr nc 2 (list (list s -1 -1 -1)))))
    (cond ((null res) nil)
          ((= (length res) 1) (car res))
          (t 'MULTIPLE-SOLUTIONS))))

(defun make-sudoku-mask (s-tbl nr nc mask-n)
  (let* ((size (array-dimension s-tbl 0))
         (m-tbl (make-array (list size size)
                            :initial-element 0)))
    ;; try to find cells which can be masked as many as possible
    (let ((rc-lis (random-sort (filter-table m-tbl #'(lambda (x) (eql x 0))))))
      (dolist (rc rc-lis)
        (when (< (count-table-if #'(lambda (x) (eql x 1)) m-tbl) mask-n)
          (setf (apply #'aref m-tbl rc) 1)
          (when (< 1 (length (make-multiple-sudoku-table
                              nr nc 2 (list (list (mask-sudoku s-tbl m-tbl nr nc) -1 -1 -1)))))
            (setf (apply #'aref m-tbl rc) 0)))))
    (if (eql (solve-sudoku (mask-sudoku s-tbl m-tbl nr nc) nr nc) 'MULTIPLE-SOLUTIONS)
        'FAIL
        (values (mask-sudoku s-tbl m-tbl nr nc) s-tbl m-tbl))))

(defun print-sudoku (tbl nr nc &key (mask-tbl nil) (stream t))
  (let ((nmax (* nr nc)))
    (dotimes (row nmax)
      (when (= (mod row nr) 0)
        (dotimes (i nr) (format stream "+") (dotimes (j nc) (format stream "-")))
        (format stream "+~%"))
      (dotimes (col nmax)
        (when (= (mod col nc) 0)
          (format stream "|"))
        (format stream "~A" (if (or (= (aref tbl row col) 0)
                                    (and mask-tbl (> (aref mask-tbl row col) 0)))
                                " "
                                (aref tbl row col))))
      (format stream "|~%"))
    (dotimes (i nr) (format stream "+") (dotimes (j nc) (format stream "-")))
    (format stream "+~%")))




;; test

(when nil

(let* ((nr 3)
       (nc 3)
       (s (car (make-multiple-sudoku-table nr nc 1))))
  (dotimes (i 10)
    (let* ((v (make-sudoku-mask s nr nc 51))
           (w (make-multiple-sudoku-table nr nc 2 (list (list v -1 -1 -1)))))
      (format t "~A(~A) ~A~%" i (length w) (count-table v))
      (when (= (length w) 1)
        (format t "~%~A~%" v)
        (format t "==> ~A~%" (count-table v))
        (format t "~A~%" w)
        (return)))))

)
