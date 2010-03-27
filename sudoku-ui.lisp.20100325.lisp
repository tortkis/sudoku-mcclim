
(in-package :cl-user)

(defpackage :sudoku-mcclim
  (:use :clim :clim-lisp :clim-internals :sudoku))

(in-package :sudoku-mcclim)

;;; game object

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

;;; game record

(defvar *game-record* nil)
   
;;; GUI parameters

(defparameter *sudoku-frame* nil)
(defparameter *board-size* 500)
(defparameter *board-margin* 20)
(defparameter *cell-gap* 5)
(defvar *game-output-record* nil)
(defvar *game-output-record-cells* nil) ; list of ((row col) record)
(defvar *game-output-record-board* nil)
(defvar *game-output-record-tile-board* nil)
(defvar *selected-cell* nil) ; (row col) of selected cell
(defvar *use-tile* t)
(defvar *selected-input-val* nil)
(defvar *sudoku-record-file* (merge-pathnames (make-pathname 
                                               :name ".sudoku"
                                               :type "record")
                                              (user-homedir-pathname)))

(defmacro debug-msg (format &rest args)
  `(format (get-frame-pane *sudoku-frame* 'sudoku-debug-pane) ,format ,@args))

(defun rgb-color (rgb)
  (make-rgb-color (/ (truncate (/ rgb (expt #X100 2))) #XFF)
                  (/ (mod (truncate (/ rgb #X100)) #X100) #XFF)
                  (/ (mod rgb #X100) #XFF)))

(defclass sudoku-board-pane (application-pane) ())

(defclass debug-display-pane (clim-stream-pane) ())

(define-application-frame sudoku-frame ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (sudoku-pane (make-pane 'sudoku-board-pane
                           :background (rgb-color #X4682b4)
                           :display-time nil
                           :display-function 'init-sudoku))
                           ;;:display-function 'display-sudoku-pane
                           ;;:incremental-redisplay '(t :check-overlapping t)))
   (sudoku-debug-pane (make-pane 'debug-display-pane
                                 :display-time nil)))
  (:layouts
   (default
       (vertically (:height (+ (* *board-margin* 6) (* (/ 5 4) *board-size*)
                               (* (/ 1 4) *board-size*))
                    :width (+ (* *board-margin* 4) *board-size*))
         (5/6 (spacing (:thickness *board-margin*) sudoku-pane))
         (+fill+ (scrolling (:scroll-bars :vertical) sudoku-debug-pane))))
   (layout-without-tile
       (vertically (:height (+ (* *board-margin* 6) (* (/ 5 4) *board-size*))
                            :width (+ (* *board-margin* 4) *board-size*))
         (4/5 (spacing (:thickness *board-margin*) sudoku-pane))
         (+fill+ (scrolling (:scroll-bars :vertical) sudoku-debug-pane))))))

(defclass cell ()
  ((row :accessor row :initarg :row)
   (col :accessor col :initarg :col)
   (val :accessor val :initarg :val))
  (:default-initargs :val 0))

(defclass cell-fixed (cell) ())

(defclass cell-blank (cell) ())

(defun coordinates-to-rowcol (x y)
  (let ((game (car (rec-playing *game-record*))))
    (if (some #'(lambda (n) (or (<= n *board-margin*)
                                (>= n (+ *board-margin* *board-size*))))
              (list x y))
        nil
        (values
         (truncate (* (/ (- y *board-margin*) *board-size*) (size game)))
         (truncate (* (/ (- x *board-margin*) *board-size*) (size game)))))))

(defun mask-p (row col)
  (> (aref (mask (car (rec-playing *game-record*))) row col) 0))

(defun make-cell (row col &optional err)
  (let* ((game (car (rec-playing *game-record*)))
         (val (get-cell game row col))
         (cell-size (/ *board-size* (size game)))
         (rec
          (with-new-output-record (*standard-output*)
            (with-output-as-presentation
                (t (make-instance (if (mask-p row col) 'cell-blank 'cell-fixed)
                                  :row row :col col :val val)
                   (if (mask-p row col) 'cell-blank 'cell-fixed))
              (draw-rectangle*
               *standard-output*
               (+ *board-margin* (* col cell-size) *cell-gap*)
               (+ *board-margin* (* row cell-size) *cell-gap*)
               (+ *board-margin* (* col cell-size) (- cell-size *cell-gap*))
               (+ *board-margin* (* row cell-size) (- cell-size *cell-gap*))
               :filled t
               :ink (cond ((and err (not (eql (get-cell game row col) empty-cell)))
                           (if (mask-p row col) +red+ +magenta+))
                          ((and (= row (first *selected-cell*))
                                (= col (second *selected-cell*)))
                           (if (mask-p row col) +white+ (rgb-color #X40e0d0)))
                          (t (if (mask-p row col)
                                 +gray80+ ; (rgb-color #X40e0d0)
                                 (rgb-color #X00ced1)))))
              (when (> val 0)
                (if *use-tile*
                    (draw-tile val
                               (+ *board-margin* (* col cell-size))
                               (+ *board-margin* (* row cell-size)))
                    (draw-text* *standard-output* (format nil "~A" val)
                                (+ *board-margin* (* col cell-size) (/ cell-size 2))
                                (+ *board-margin* (* row cell-size) (/ cell-size 2))
                                :align-x :center :align-y :center
                                :text-size (truncate (/ cell-size 2))
                                :ink (if (mask-p row col) +blue+ +black+))))))))
    (push (list (list row col) rec) *game-output-record-cells*)))

;;(defparameter *tile-image-names*
;;  '("RedApple2x" "Strawberry" "Orange" "Pear"))

(defparameter *tile-image-names*
  '("Woof2x" "RedDog" "Pointy" "Doggie"))

(defvar *tile-images* nil)

;;(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *tile-images* '())
  (dotimes (idx (length *tile-image-names*))
    (multiple-value-bind (array design)
        (climi::xpm-parse-file (make-pathname
                                ;;:directory "/Users/torutakaishi/cl/sudoku/"
                                :directory "/Users/torutakaishi/cl/sudoku/images/func-animals/"
                                ;;:directory `(:relative "images/func-animals/")
                                :name (nth idx *tile-image-names*)
                                :type "xpm"))
      (push (list idx array design) *tile-images*)))

(defclass tile ()
  ((val :accessor val :initarg :val)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defun draw-tile (val x y &optional (in-tile-sel nil))
  (let* ((cell-size (/ *board-size* (size (car (rec-playing *game-record*)))))
         (img (cdr (assoc (1- val) *tile-images*)))
         (img-array (first img))
         (img-design (second img))) 
    (draw-rectangle* *standard-output*
                     (+ x *cell-gap*)
                     (+ y *cell-gap*)
                     (+ x (- cell-size *cell-gap*))
                     (+ y (- cell-size *cell-gap*))
                     :filled (if (and in-tile-sel (eql val *selected-input-val*)) t nil)
                     :ink (if (and in-tile-sel (eql val *selected-input-val*)) +red+ nil))
    (draw-pattern* *standard-output*
                   (make-pattern img-array img-design)
                   (+ x (* 0.5 (- cell-size (array-dimension img-array 1))))
                   (+ y (* 0.5 (- cell-size (array-dimension img-array 0)))))))

(defun make-tile (val)
  (let* ((cell-size (/ *board-size* (size (car (rec-playing *game-record*)))))
         (x (+ *board-margin*  (* (1- val) cell-size)))
         (y (+ (* 2 *board-margin*) *board-size*)))
    (with-output-as-presentation
        (*standard-output* (make-instance 'tile :val val :x x :y y) 'tile)
      (draw-tile val x y t))))

(defun make-tile-all ()
  (when *game-output-record-tile-board*
    (erase-output-record *game-output-record-tile-board* *standard-output* nil))
  (setf *game-output-record-tile-board*
        (updating-output (*standard-output*)
          (dotimes (i (size (car (rec-playing *game-record*))))
            (make-tile (1+ i))))))

;; TODO: drag-and-drop does not redraw image?
;;(define-sudoku-frame-command (com-dnd-tile)
;;    ((tile 'tile))
;;  (let ((val (val tile)))
;;    (multiple-value-bind (final-x final-y)
;;        (dragging-output (t :finish-on-release t)
;;          (draw-tile val (x tile) (y tile)))
;;      (multiple-value-bind (row col)
;;          (coordinates-to-rowcol final-x final-y)
;;        (when (and row col (mask-p row col))
;;          (set-cell (car (rec-playing *game-record*)) row col val)))
;;      (redraw-cells-all)
;;      (redisplay *game-output-record-tile-board* *standard-output*))))

(define-sudoku-frame-command (com-dnd-tile)
    ((tile 'tile)
     (x 'real) (y 'real))
  (declare (ignore x y))
  (if (eql *selected-input-val* (val tile))
      (setf *selected-input-val* nil)
      (setf *selected-input-val* (val tile)))
  (make-tile-all))
;;  (redisplay *game-output-record-tile-board* *standard-output*))

(define-presentation-to-command-translator translator-dnd-tile
    (tile com-dnd-tile sudoku-frame)
    (object x y)
  `(,object ,x ,y))

(define-sudoku-frame-command (com-click-cell :name t)
    ((object 'cell-blank)
     (x 'real) (y 'real))
  (declare (ignore x y))
  (let* ((prev-row-col *selected-cell*)
         (row (row object))
         (col (col object))
         (game (car (rec-playing *game-record*))))
    (setf *selected-cell* (list row col))
    (when prev-row-col
      (apply #'erase-cell prev-row-col)
      (apply #'make-cell prev-row-col))
    (when *selected-input-val*
      (set-cell game row col *selected-input-val*))
    ;;(redraw-cells-all)
    (erase-cell row col)
    (make-cell row col)
    (debug-msg "[history] ~A: ~A~%" (history-pointer game) (history game))))
    
(defun redraw-cells-all ()
  (let ((game (car (rec-playing *game-record*))))
    (dotimes (row (size game))
      (dotimes (col (size game))
        (erase-cell row col)
        (make-cell row col)))))
  
(defun erase-cell (row col)
  (let ((new-rec '()))
    (mapc #'(lambda (x)
              (let ((pos (first x)))
                (if (and (= (first pos) row) (= (second pos) col))
                    (erase-output-record (second x)
                                         (get-frame-pane *sudoku-frame* 'sudoku-pane) nil)
                    (push x new-rec))))
          *game-output-record-cells*)
    (setf *game-output-record-cells* new-rec)))

(define-presentation-to-command-translator translator-click-cell
    (cell-blank com-click-cell sudoku-frame)
    (object x y)
  `(,object ,x ,y))

;; commands

(define-sudoku-frame-command com-quit-frame ()
  (save-sudoku-game-record *game-record* *sudoku-record-file*)
  (frame-exit *application-frame*))

(define-sudoku-frame-command com-start (&key (fresh 'boolean :default t)
                                             (rotate 'boolean :default nil))
  (let ((playing (car (rec-playing *game-record*))))
    (when (and (not (null playing))
               (eql (check playing) 'CORRECT))
      (move-game *game-record* rec-playing rec-done))
    (cond ((or fresh (null playing))
           (let ((next-game (car (rec-new *game-record*))))
             (when (or (null next-game)
                       (not (eql (nr next-game) (nr *game-record*)))
                       (not (eql (nc next-game) (nc *game-record*)))
                       (not (eql (level next-game) (level *game-record*))))
               (create-new-games *game-record*)))
           (move-game *game-record* rec-new rec-playing))
          (rotate
           (setf (rec-playing *game-record*)
                 (append (cdr (rec-playing *game-record*))
                         (list (car (rec-playing *game-record*))))))))
  (let ((game (car (rec-playing *game-record*)))
        (stream (get-frame-pane *sudoku-frame* 'sudoku-pane)))
    (unless (<= (size game) 4)
      (setf *use-tile* nil))
    (display-sudoku-pane *sudoku-frame* stream)
    (when *game-output-record*
      (erase-output-record *game-output-record* stream nil))
    (mapc #'(lambda (x) (erase-output-record (second x) stream nil))
          *game-output-record-cells*)
    (setf *game-output-record-cells* '())
    (setf *selected-cell* (car (reverse (blank-cells (table game)))))
    (setf *game-output-record*
          (with-new-output-record (stream)
            (dotimes (row (size game))
              (dotimes (col (size game))
                (make-cell row col)))
            (when *use-tile*
              (make-tile-all))))))

(define-sudoku-frame-command com-redraw ()
  (redraw-cells-all))

(define-sudoku-frame-command com-reset ()
  (let ((game (car (rec-playing *game-record*))))
    (when (mask game)
      (dotimes (row (size game))
        (dotimes (col (size game))
          (when (eql (aref (mask game) row col) 1)
            (set-cell game row col empty-cell))))
      (redraw-cells-all))))

(define-sudoku-frame-command com-check ()
  (let* ((game (car (rec-playing *game-record*)))
         (chk (check game)))
    (debug-msg "~A~%" (table game))
    (debug-msg "[CHECK] ~A~%" chk)
    (when (listp chk)
      (redraw-cells-all)
      (mapc #'(lambda (rc) (let ((r (first rc)) (c (second rc)))
                             (erase-cell r c)
                             (make-cell r c t)))
            chk))))

(define-sudoku-frame-command com-size
    ((nr 'integer :default 2 :prompt "Block Rows")
     (nc 'integer :default 2 :prompt "Block Columns"))
  (setf (nr *game-record*) nr)
  (setf (nc *game-record*) nc)
  (setf *selected-input-val* nil)
  (when *game-output-record-tile-board*
    (erase-output-record *game-output-record-tile-board* *standard-output* nil))
  (unless (<= (* nr nc) 4)
    (setf *use-tile* nil))
  (if *use-tile*
      (setf (frame-current-layout *sudoku-frame*) 'default)
      (setf (frame-current-layout *sudoku-frame*) 'layout-without-tile)))

(define-sudoku-frame-command com-level
    ((level 'interger
            :default 0.5
            :prompt "Level"))
  (setf (level *game-record*) level)
  (debug-msg "Set level ~A~%" level))

(define-sudoku-frame-command com-style
    ((style 'interger
            :default 2
            :prompt "Style"))
  (cond ((eql style 1)
         (setf *use-tile* nil))
        (t (setf *use-tile* t)))
  (debug-msg "Set Style ~A~%" style))

(define-sudoku-frame-command com-undo ()
  (let ((game (car (rec-playing *game-record*))))
    (undo game)
    (com-redraw)
    (debug-msg "[history] ~A: ~A~%" (history-pointer game) (history game))))

(define-sudoku-frame-command com-redo ()
  (let ((game (car (rec-playing *game-record*))))
    (redo game)
    (com-redraw)
    (debug-msg "[history] ~A: ~A~%" (history-pointer game) (history game))))

(make-command-table 'size-command-table
                   :errorp nil
                   :menu '(("4x4" :command (com-size 2 2))
                           ("2x3" :command (com-size 2 3))
                           ("9x9" :command (com-size 3 3))))

(make-command-table 'level-command-table
                   :errorp nil
                   :menu '(("Easy" :command (com-level 1/3))
                           ("Medium" :command (com-level 0.5))
                           ("Difficult" :command (com-level 51/81))
                           ("Very Difficult (slow)" :command (com-level 1))))

(make-command-table 'style-command-table
                   :errorp nil
                   :menu '(("Number" :command (com-style 1))
                           ("Cartoon" :command (com-style 2))))

(make-command-table 'menubar-command-table
                    :errorp nil
                    :menu '(("Quit" :command com-quit-frame)
                            ("New" :command com-start)
                            ("Other" :command (com-start :fresh nil :rotate t))
                            ("Reset" :command com-reset)
                            ("Redraw" :command com-redraw)
                            ("Check" :command com-check)
                            ("Size" :menu size-command-table)
                            ("Level" :menu level-command-table)
                            ("Style" :menu style-command-table)
                            ("Undo" :command com-undo)
                            ("Redo" :command com-redo)))

(defmethod handle-event ((pane sudoku-board-pane) (event keyboard-event))
  (let* ((game (car (rec-playing *game-record*)))
         (key (keyboard-event-character event))
         (key-name (keyboard-event-key-name event))
         (val (if (characterp key)
                  (parse-integer (coerce (list key) 'string) :junk-allowed t)
                  nil))
         (prev-row (first *selected-cell*))
         (prev-col (second *selected-cell*)))
    (if (and (numberp val) (>= val 0) (<= val (size game))
             (eql (aref (mask game) prev-row prev-col) 1))
        (progn
          (set-cell game prev-row prev-col val)
          (debug-msg "~A~%" (table game))
          (erase-cell prev-row prev-col)
          (make-cell prev-row prev-col))
        (let ((dir (position (symbol-name key-name) '("UP" "DOWN" "LEFT" "RIGHT")
                             :test 'equal)))
          (when dir
            (let ((col (+ prev-col
                          (cond ((= dir 2) -1)
                                ((= dir 3) +1)
                                (t 0))))
                  (row (+ prev-row
                          (cond ((= dir 0) -1)
                                ((= dir 1) +1)
                                (t 0)))))
              (when (and (<= 0 row (1- (size game)))
                         (<= 0 col (1- (size game))))
                (setf *selected-cell* (list row col))
                (erase-cell prev-row prev-col)
                (make-cell prev-row prev-col)
                (erase-cell row col)
                (make-cell row col))))))))

(defmethod display-sudoku-pane ((frame sudoku-frame) stream)
  (let* ((game (car (rec-playing *game-record*)))
        (cell-size (/ *board-size* (size game))))
    (when *game-output-record-board*
      (erase-output-record *game-output-record-board* stream nil))
    (setf *game-output-record-board* 
          (with-new-output-record (stream)
            (dotimes (i (1+ (size game)))
              (let ((x1 *board-margin*)
                    (x2 (+ *board-margin* (* i cell-size)))
                    (x3 (+ *board-margin* (* (size game) cell-size)))
                    (th-h (if (zerop (mod i (nr game))) 5 1))
                    (th-v (if (zerop (mod i (nc game))) 5 1)))
                (draw-line* stream x1 x2 x3 x2 :line-thickness th-h)
                (draw-line* stream x2 x1 x2 x3 :line-thickness th-v)))
            (when *use-tile*
              (with-translation (stream 0 (+ *board-margin*
                                             (* (size game) cell-size)))
                (draw-line* stream
                            *board-margin* *board-margin*
                            (+ *board-margin* (* (size game) cell-size))
                            *board-margin*
                            :line-thickness 3)
                (draw-line* stream
                            *board-margin* (+ *board-margin* cell-size)
                            (+ *board-margin* (* (size game) cell-size))
                            (+ *board-margin* cell-size)
                            :line-thickness 3)
                (dotimes (i (1+ (size game)))
                  (let ((x1 *board-margin*)
                        (x2 (+ *board-margin* (* i cell-size)))
                        (x3 (+ *board-margin* cell-size)))
                    (draw-line* stream x2 x1 x2 x3 :line-thickness 1)))))))))

(defmethod init-sudoku ((frame sudoku-frame) stream)
  (com-start :fresh nil))

(defun sudoku-mcclim ()
  (setf *game-output-record* nil)
  (setf *game-output-record-cells* nil)
  (setf *game-output-record-board* nil)
  (setf *game-output-record-tile-board* nil)
  (setf *selected-cell* nil)
  (setf *use-tile* t)
  (setf *selected-input-val* nil)
  (setf *game-record* (load-sudoku-game-record *sudoku-record-file*))
  (if (null *game-record*)
      (setf *game-record* (make-instance 'sudoku-game-record)))
  (setf *sudoku-frame* (make-application-frame 'sudoku-frame))
  (run-frame-top-level *sudoku-frame*))

