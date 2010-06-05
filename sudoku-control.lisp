
(in-package :sudoku-control)

;;; game controller

(defclass sudoku-game ()
  ((level :accessor level :initarg :level :initform 0.5) ; 1/3, 1/2, 51/81
   (nr :accessor nr :initarg :nr :initform 2)
   (nc :accessor nc :initarg :nc :initform 2)
   (table :accessor table :initarg :table :initform nil)
   (ans :accessor ans :initarg :ans :initform nil)
   (mask :accessor mask :initarg :mask :initform nil)
   (memo :accessor memo :initarg :memo :initform nil)
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
    (setf (memo s) (make-array (list (size s) (size s)) :initial-element nil))
    s))

(defmethod add-memo ((s sudoku-game) row col m)
  (let ((size (* (nr s) (nc s))))
    (when (or (null (memo s))
              (not (equal (array-dimensions (memo s)) (list size size))))
      (setf (memo s) (make-array (list size size) :initial-element nil))))
  (setf (aref (memo s) row col) m))

(defmethod clean-memo ((s sudoku-game))
  (dolist (r (array-dimension (memo s) 0))
    (dolist (c (array-dimension (memo s) 1))
      (setf (aref (memo s) r c) nil))))

(defmethod game-to-list ((s sudoku-game) &optional (save-memo nil))
  `((LEVEL ,(level s))
    (NR ,(nr s))
    (NC ,(nc s))
    (TABLE ,(table s))
    (ANS ,(ans s))
    (MASK ,(mask s))
    ,(when save-memo `(MEMO ,(memo s)))
    (HISTORY ,(history s))))

(defun make-sudoku-game-from-list (lis)
  (let* ((level (second (assoc 'LEVEL lis)))
         (nr (second (assoc 'NR lis)))
         (nc (second (assoc 'NC lis)))
         (table (second (assoc 'TABLE lis)))
         (ans (second (assoc 'ANS lis)))
         (mask (second (assoc 'MASK lis)))
         (memo (second (assoc 'MEMO lis)))
         (history (second (assoc 'HISTORY lis)))
         (s (make-instance
             'sudoku-game
             :level level
             :nr nr
             :nc nc
             :table table
             :ans ans
             :mask (or mask (make-array (list nr nc) :initial-element nil))
             :memo memo
             :history history)))
    (if (and (numberp level) (numberp nr) (numberp nc)
             (every #'(lambda (x) (and (arrayp x)
                                       (= (length (array-dimensions x)) 2)
                                       (= (array-dimension x 0) (* nr nc))
                                       (= (array-dimension x 1) (* nr nc))))
                    (list table ans mask))
             (string-equal (symbol-name (check-sudoku ans nr nc)) "CORRECT"))
        s
        nil)))

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
    (NEW ,(mapcar #'(lambda (x) (game-to-list x nil)) (rec-new r)))
    (DONE ,(mapcar #'(lambda (x) (game-to-list x nil)) (rec-done r)))
    (PLAYING ,(mapcar #'(lambda (x) (game-to-list x t)) (rec-playing r)))))

(defun make-sudoku-game-record-from-list (lis)
  (let ((r (make-instance 'sudoku-game-record
                          :level (second (assoc 'LEVEL lis))
                          :nr (second (assoc 'NR lis))
                          :nc (second (assoc 'NC lis)))))
    (setf (rec-new r)
          (remove-if #'null (mapcar #'make-sudoku-game-from-list (second (assoc 'NEW lis)))))
    (setf (rec-done r)
          (remove-if #'null (mapcar #'make-sudoku-game-from-list (second (assoc 'DONE lis)))))
    (setf (rec-playing r)
          (remove-if #'null (mapcar #'make-sudoku-game-from-list (second (assoc 'PLAYING lis)))))
    r))

(defmethod save-sudoku-game-record ((r sudoku-game-record) fn)
  (with-open-file (str fn :direction :output :if-exists :supersede)
    (write (to-list r) :stream str)))

(defun load-sudoku-game-record (fn)
  (with-open-file (str fn :if-does-not-exist nil)
    (when str
      (let ((lis (read str)))
        (make-sudoku-game-record-from-list lis)))))

(defmethod valid-sudoku-game-record-p ((r sudoku-game-record))
  (every #'(lambda (x) (not (null x)))
         (list (level r) (nr r) (nc r))))

(defmethod pick-game-to-play ((r sudoku-game-record))
  (cond ((<= (length (rec-new r)) 0)
         (create-new-games r)
         (move-game r rec-new rec-playing))
        (t (labels
               ((pick (new-games)
                  (cond ((null new-games)
                         (create-new-games r)
                         (move-game r rec-new rec-playing)
                         nil)
                        (t (let ((g (car new-games)))
                             (cond ((and (eql (nr g) (nr r))
                                         (eql (nc g) (nc r))
                                         (eql (level g) (level r)))
                                    (push g (rec-playing r))
                                    (cdr new-games))
                                   (t (cons g (pick (cdr new-games))))))))))
             (setf (rec-new r) (pick (rec-new r)))))))
                        
