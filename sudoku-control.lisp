
(in-package :sudoku-mcclim)

;;; game controller

(defclass sudoku-game ()
  ((level :accessor level :initarg :level :initform 0.5) ; 1/3, 1/2, 51/81
   (nr :accessor nr :initarg :nr :initform 2)
   (nc :accessor nc :initarg :nc :initform 2)
   (table :accessor table :initarg :table :initform nil)
   (ans :accessor ans :initarg :ans :initform nil)
   (mask :accessor mask :initarg :mask :initform nil)
   (history :accessor history :initarg :history :initform nil)
   (history-pointer :accessor history-pointer :initform 0)))

(defmethod size ((s sudoku-game))
  (* (nr s) (nc s)))

(defmethod set-cell ((s sudoku-game) row col val)
  (setf (history s)
        (cons (list row col val (aref (table s) row col))
              (if (>= (history-pointer s) (length (history s)))
                  '()
                  (subseq (history s) (history-pointer s)))))
  (setf (history-pointer s) 0)
  (setf (aref (table s) row col) val))

(defmethod get-cell ((s sudoku-game) row col)
  (aref (table s) row col))

(defmethod check ((s sudoku-game))
  (check-sudoku (table s) (nr s) (nc s)))

(defmethod undo ((s sudoku-game))
  (cond ((or (null (history s))
             (>= (history-pointer s) (length (history s))))
         nil)
        (t
         (let ((c (nth (history-pointer s) (history s))))
           (setf (aref (table s) (first c) (second c)) (fourth c))
           (incf (history-pointer s))))))

(defmethod redo ((s sudoku-game))
  (cond ((zerop (history-pointer s))
         nil)
        (t
         (decf (history-pointer s))
         (let ((c (nth (history-pointer s) (history s))))
           (setf (aref (table s) (first c) (second c)) (third c))))))

(defmethod make-sudoku-game ((s sudoku-game))
  (setf (history s) nil)
  (multiple-value-bind (table ans mask)
      (make-sudoku-mask
       (car (make-multiple-sudoku-table (nr s) (nc s) 1))
       (nr s) (nc s)
       (truncate (* (expt (* (nr s) (nc s)) 2) (level s))))
    (setf (table s) table)
    (setf (ans s) ans)
    (setf (mask s) mask)
    s))

(defmethod to-list ((s sudoku-game))
  `((LEVEL ,(level s))
    (NR ,(nr s))
    (NC ,(nc s))
    (TABLE ,(table s))
    (ANS ,(ans s))
    (MASK ,(mask s))
    (HISTORY ,(history s))))

(defun make-sudoku-game-from-list (lis)
  (make-instance 'sudoku-game
                 :level (second (assoc 'LEVEL lis))
                 :nr (second (assoc 'NR lis))
                 :nc (second (assoc 'NC lis))
                 :table (second (assoc 'TABLE lis))
                 :ans (second (assoc 'ANS lis))
                 :mask (second (assoc 'MASK lis))
                 :history (second (assoc 'HISTORY lis))))

;;; game records

(defclass sudoku-game-record ()
  ((new :accessor rec-new :initform '())
   (done :accessor rec-done :initform '())
   (playing :accessor rec-playing :initform '())
   (level :accessor level :initarg :level :initform 0.5) ; 1/3, 1/2, 51/81
   (nr :accessor nr :initarg :nr :initform 2)
   (nc :accessor nc :initarg :nc :initform 2)))

(defmethod game-exists-p ((r sudoku-game-record) tbl)
  (let ((nr (array-dimension tbl 0))
        (nc (array-dimension tbl 1)))
    (find-if #'(lambda (x)
                 (and (= (array-dimension x 0) nr)
                      (= (array-dimension x 1) nc)
                      (every #'eval (map 'vector #'eql
                                         (make-array (* nr nc) :displaced-to x)
                                         (make-array (* nr nc) :displaced-to tbl)))))
             (mapcar #'ans (append (rec-playing r) (rec-new r) (rec-done r))))))

(defmethod create-new-games ((r sudoku-game-record) &key nr nc level (game-n 1))
  (when nr (setf (nr r) nr))
  (when nc (setf (nc r) nc))
  (when level (setf (level r) level))
  (do ((i 0 i))
      ((>= i game-n) r)
    (let ((g (make-sudoku-game (make-instance
                                'sudoku-game
                                :nr (nr r) :nc (nc r) :level (level r)))))
      (unless (game-exists-p r (ans g))
        (push g (rec-new r))
        (incf i)))))

(defmacro move-game (r from-rec to-rec)
  `(unless (null (,from-rec ,r))
     (push (car (,from-rec ,r)) (,to-rec ,r))
     (setf (,from-rec ,r) (cdr (,from-rec ,r)))
     (car (,to-rec ,r))))

(defmethod to-list ((r sudoku-game-record))
  `((LEVEL ,(level r))
    (NR ,(nr r))
    (NC ,(nc r))
    (NEW ,(mapcar #'to-list (rec-new r)))
    (DONE ,(mapcar #'to-list (rec-done r)))
    (PLAYING ,(mapcar #'to-list (rec-playing r)))))

(defun make-sudoku-game-record-from-list (lis)
  (let ((r (make-instance 'sudoku-game-record
                          :level (second (assoc 'LEVEL lis))
                          :nr (second (assoc 'NR lis))
                          :nc (second (assoc 'NC lis)))))
    (setf (rec-new r)
          (mapcar #'make-sudoku-game-from-list (second (assoc 'NEW lis))))
    (setf (rec-done r)
          (mapcar #'make-sudoku-game-from-list (second (assoc 'DONE lis))))
    (setf (rec-playing r)
          (mapcar #'make-sudoku-game-from-list (second (assoc 'PLAYING lis))))
    r))

(defmethod save-sudoku-game-record ((r sudoku-game-record) fn)
  (with-open-file (str fn :direction :output :if-exists :supersede)
    (write (to-list r) :stream str)))

(defun load-sudoku-game-record (fn)
  (with-open-file (str fn :if-does-not-exist nil)
    (when str
      (make-sudoku-game-record-from-list (read str)))))