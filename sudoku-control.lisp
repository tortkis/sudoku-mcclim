;;; sudoku game controller

(in-package :sudoku-control)

(defclass sudoku-game ()
  ((level :accessor game-level :initarg :level :initform 0.5) ; 1/3, 1/2, 51/81
   (nr :accessor game-nr :initarg :nr :initform 2)
   (nc :accessor game-nc :initarg :nc :initform 2)
   (table :accessor game-table :initarg :table :initform nil)
   (ans :accessor game-ans :initarg :ans :initform nil)
   (mask :accessor game-mask :initarg :mask :initform nil)
   (memo :accessor game-memo :initarg :memo :initform nil)
   (history :accessor game-history :initarg :history :initform nil)
   (history-pointer :accessor game-history-pointer :initform 0)))

(defmethod size ((s sudoku-game))
  (stbl-dimension (game-table s)))

(defmethod set-cell ((s sudoku-game) row col val)
  (setf (game-history s)
        (cons (list row col val (getcell (game-table s) row col))
              (if (>= (game-history-pointer s) (length (game-history s)))
                  '()
                  (subseq (game-history s) (game-history-pointer s)))))
  (setf (game-history-pointer s) 0)
  (setcell (game-table s) row col val))

(defmethod get-cell ((s sudoku-game) row col)
  (getcell (game-table s) row col))

(defmethod check ((s sudoku-game))
  (check-sudoku (game-table s)))

(defmethod undo ((s sudoku-game))
  (cond ((or (null (game-history s))
             (>= (game-history-pointer s) (length (game-history s))))
         nil)
        (t
         (let ((c (nth (game-history-pointer s) (game-history s))))
           (setcell (game-table s) (first c) (second c) (fourth c))
           (incf (game-history-pointer s))))))

(defmethod redo ((s sudoku-game))
  (cond ((zerop (game-history-pointer s))
         nil)
        (t
         (decf (game-history-pointer s))
         (let ((c (nth (game-history-pointer s) (game-history s))))
           (setcell (game-table s) (first c) (second c) (third c))))))

(defmethod make-sudoku-game ((s sudoku-game))
  (setf (game-history s) nil)
  (multiple-value-bind (table ans mask)
      (make-sudoku-mask
       (car (make-multiple-sudoku-table 1 (list (game-nr s) (game-nc s)) :random t))
       (truncate (* (expt (* (game-nr s) (game-nc s)) 2) (game-level s))))
    (setf (game-table s) table)
    (setf (game-ans s) ans)
    (setf (game-mask s) mask)
    (setf (game-memo s) (make-array (list (size s) (size s)) :initial-element nil))
    s))

(defmethod add-memo ((s sudoku-game) row col m)
  (let ((size (* (game-nr s) (game-nc s))))
    (when (or (null (game-memo s))
              (not (equal (array-dimensions (game-memo s)) (list size size))))
      (setf (game-memo s) (make-array (list size size) :initial-element nil))))
  (setf (aref (game-memo s) row col) m))

(defmethod clean-memo ((s sudoku-game))
  (dolist (r (array-dimension (game-memo s) 0))
    (dolist (c (array-dimension (game-memo s) 1))
      (setf (aref (game-memo s) r c) nil))))

(defmethod game-to-list ((s sudoku-game) &optional (save-memo nil))
  `((LEVEL ,(game-level s))
    (NR ,(game-nr s))
    (NC ,(game-nc s))
    (TABLE ,(game-table s))
    (ANS ,(game-ans s))
    (MASK ,(game-mask s))
    ,(when save-memo `(MEMO ,(game-memo s)))
    (HISTORY ,(game-history s))))

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
             (every #'(lambda (x) (and (eql (type-of x) 'stbl)
                                       (= (stbl-dimension x) (* nr nc))))
                    (list table ans mask))
             (string-equal (symbol-name (check-sudoku ans)) "CORRECT"))
        s
        nil)))

;;; game records

(defclass sudoku-game-record ()
  ((new :accessor rec-new :initform '())
   (done :accessor rec-done :initform '())
   (playing :accessor rec-playing :initform '())
   (level :accessor rec-level :initarg :level :initform 0.5) ; 1/3, 1/2, 51/81
   (nr :accessor rec-nr :initarg :nr :initform 2)
   (nc :accessor rec-nc :initarg :nc :initform 2)))

(defmethod game-exists-p ((r sudoku-game-record) tbl)
  (some #'(lambda (x)
            (compare-table tbl x))
        (mapcar #'game-ans (append (rec-playing r) (rec-new r) (rec-done r)))))

(defmethod create-new-games ((r sudoku-game-record) &key nr nc level (game-n 1))
  (when nr (setf (rec-nr r) nr))
  (when nc (setf (rec-nc r) nc))
  (when level (setf (rec-level r) level))
  (do ((i 0 i))
      ((>= i game-n) r)
    (let ((g (make-sudoku-game (make-instance
                                'sudoku-game
                                :nr (rec-nr r) :nc (rec-nc r) :level (rec-level r)))))
      (unless (game-exists-p r (game-ans g))
        (push g (rec-new r))
        (incf i)))))

(defmacro move-game (r from-rec to-rec)
  `(unless (null (,from-rec ,r))
     (push (car (,from-rec ,r)) (,to-rec ,r))
     (setf (,from-rec ,r) (cdr (,from-rec ,r)))
     (car (,to-rec ,r))))

(defmethod record-to-list ((r sudoku-game-record))
  `((LEVEL ,(rec-level r))
    (NR ,(rec-nr r))
    (NC ,(rec-nc r))
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
    (write (record-to-list r) :stream str)))

(defun load-sudoku-game-record (fn)
  (with-open-file (str fn :if-does-not-exist nil)
    (when str
      (let ((lis (read str)))
        (make-sudoku-game-record-from-list lis)))))

(defmethod valid-sudoku-game-record-p ((r sudoku-game-record))
  (every #'(lambda (x) (not (null x)))
         (list (rec-level r) (rec-nr r) (rec-nc r))))

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
                             (cond ((and (eql (game-nr g) (rec-nr r))
                                         (eql (game-nc g) (rec-nc r))
                                         (eql (game-level g) (rec-level r)))
                                    (push g (rec-playing r))
                                    (cdr new-games))
                                   (t (cons g (pick (cdr new-games))))))))))
             (setf (rec-new r) (pick (rec-new r)))))))
                        
