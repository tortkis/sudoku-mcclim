
(in-package :sudoku-mcclim)

;;; game record

(defvar *game-record* nil)
(defun sudoku-record-file ()
  (merge-pathnames (make-pathname :name ".sudoku" :type "record")
                   (user-homedir-pathname)))
   
;;; GUI parameters

(defparameter *sudoku-frame* nil)
(defparameter *board-size* 500)
(defparameter *board-margin* 20)
(defparameter *cell-gap* 5)
(defparameter *info-height* 25)
(defvar *game-output-record* nil)
(defvar *game-output-record-cells* nil) ; list of ((row col) record)
(defvar *game-output-record-board* nil)
(defvar *game-output-record-tile-board* nil)
(defvar *selected-cell* nil) ; (row col) of selected cell
(defvar *selected-input-val* nil)
(defvar *keep-playing-record* nil)
(defvar *debug-output-p* nil)

;;; images

(defparameter *tile-themes*
  '(("shape1" ("circle1" "star2" "square1" "triangle2") ("msg-correct" "msg-incorrect"))
    ("func-animals" ("Woof2x" "RedDog" "Pointy" "Doggie"))
    ("fruits" ("RedApple2x" "Strawberry" "Orange" "Pear"))))
(defvar *tile-images* nil)
(defvar *use-tile* t)
(defvar *msg-images* nil)

(defun load-images (theme)
  (let* ((theme-set (assoc theme *tile-themes* :test 'equal))
         (tile-image-names (second theme-set))
         (msg-image-names (third theme-set)))
    (when tile-image-names
      (setf *tile-images* '())
      (dotimes (idx (length tile-image-names))
        (multiple-value-bind (array design)
            (climi::xpm-parse-file (merge-pathnames
                                    (make-pathname
                                     :directory `(:relative ,theme)
                                     :name (nth idx tile-image-names)
                                     :type "xpm")
                                    sudoku.system::*images-path*))
          (push (list idx array design) *tile-images*))))
    (when msg-image-names
      (dolist (msg-name msg-image-names)
        (multiple-value-bind (array design)
            (climi::xpm-parse-file (merge-pathnames
                                    (make-pathname
                                     :directory `(:relative ,theme)
                                     :name msg-name
                                     :type "xpm")
                                    sudoku.system::*images-path*))
          (push (list msg-name array design) *msg-images*))))))

;; functions

(defmacro debug-msg (format &rest args)
  `(when *debug-output-p*
     (format (get-frame-pane *sudoku-frame* 'sudoku-debug-pane) ,format ,@args)))

(defun rgb-color (rgb)
  "create a MCCLIM color object from 6-digit hexadecimal code"
  (make-rgb-color (/ (truncate (/ rgb (expt #X100 2))) #XFF)
                  (/ (mod (truncate (/ rgb #X100)) #X100) #XFF)
                  (/ (mod rgb #X100) #XFF)))

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

(defun erase-all-outputs ()
  (let ((str (get-frame-pane *sudoku-frame* 'sudoku-pane)))
    (mapc #'(lambda (x) (erase-output-record (second x) str nil))
          *game-output-record-cells*)
    (setf *game-output-record-cells* nil)
    (dolist (rec (list *game-output-record*
                       *game-output-record-board*
                       *game-output-record-tile-board*))
      (when (output-record-p rec)
        (erase-output-record rec str nil)))))

;; classes

(defclass sudoku-board-pane (application-pane) ())
(defclass sudoku-info-pane (clim-stream-pane) ())
(defclass debug-display-pane (clim-stream-pane) ())

(defclass cell ()
  ((row :accessor row :initarg :row)
   (col :accessor col :initarg :col)
   (val :accessor val :initarg :val))
  (:default-initargs :val 0))

(defclass cell-fixed (cell) ())

(defclass cell-blank (cell) ())

(defclass tile ()
  ((val :accessor val :initarg :val)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

;; panes

(define-application-frame sudoku-frame ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (sudoku-pane (make-pane 'sudoku-board-pane
                           :background (rgb-color #X4682b4)
                           :display-time nil
                           :display-function 'init-sudoku))
   (info-pane (make-pane 'sudoku-info-pane
                         :display-function 'display-info
                         :display-time nil))
   (sudoku-debug-pane (make-pane 'debug-display-pane
                                 :display-time nil)))
  (:layouts
   (default
       (vertically (:height (+ (* *board-margin* 5) (* (/ 4 4) *board-size*)
                               (* (/ 1 4) *board-size*)
                               *info-height*)
                    :width (+ (* *board-margin* 4) *board-size*))
         `(,*info-height* ,info-pane)
         (+fill+ (spacing (:thickness *board-margin*) sudoku-pane))))
   (debug
       (vertically (:height (+ (* *board-margin* 6) (* (/ 5 4) *board-size*)
                               (* (/ 1 4) *board-size*)
                               *info-height*)
                    :width (+ (* *board-margin* 4) *board-size*))
         `(,*info-height* ,info-pane)
         (5/6 (spacing (:thickness *board-margin*) sudoku-pane))
         (+fill+ (scrolling (:scroll-bars :vertical) sudoku-debug-pane))))))

;; display

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
                     :ink (if (and in-tile-sel (eql val *selected-input-val*)) +blue+ nil))
    (draw-pattern* *standard-output*
                   (make-pattern img-array img-design)
                   (+ x (* 0.5 (- cell-size (array-dimension img-array 1))))
                   (+ y (* 0.5 (- cell-size (array-dimension img-array 0)))))))

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
                                 (rgb-color #X4682b4) ; +gray80+ ; (rgb-color #X40e0d0)
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

(defun redraw-cells-all ()
  (let ((game (car (rec-playing *game-record*))))
    (dotimes (row (size game))
      (dotimes (col (size game))
        (erase-cell row col)
        (make-cell row col)))))
  
(defun make-tile (val)
  (let* ((cell-size (/ *board-size* (size (car (rec-playing *game-record*)))))
         (x (+ *board-margin*  (* (1- val) cell-size)))
         (y (+ (* 2 *board-margin*) *board-size*
               (/ *board-size* 4 2) (- (/ cell-size 2)))))
    (with-output-as-presentation
        (*standard-output* (make-instance 'tile :val val :x x :y y) 'tile)
      (if *use-tile*
          (draw-tile val x y t)
          (progn
            (draw-rectangle* *standard-output*
                             (+ x *cell-gap*)
                             (+ y *cell-gap*)
                             (+ x (- cell-size *cell-gap*))
                             (+ y (- cell-size *cell-gap*))
                             :filled nil)
            (draw-text* *standard-output* (format nil "~A" val)
                        (+ x (/ cell-size 2))
                        (+ y (/ cell-size 2))
                        :align-x :center :align-y :center
                        :ink (if (eql val *selected-input-val*) +blue+ +black+)
                        :text-size (truncate (/ cell-size 2))))))))

(defun make-tile-all ()
  (when (output-record-p *game-output-record-tile-board*)
    (erase-output-record *game-output-record-tile-board* *standard-output* nil))
  (setf *game-output-record-tile-board*
        (with-new-output-record (*standard-output*)
          (dotimes (i (size (car (rec-playing *game-record*))))
            (make-tile (1+ i))))))

(defmethod display-sudoku-board ((frame sudoku-frame) stream)
  (let* ((game (car (rec-playing *game-record*)))
        (cell-size (/ *board-size* (size game))))
    (when (output-record-p *game-output-record-board*)
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
            (with-translation (stream 0 (+ *board-margin*
                                           ;;(* (size game) cell-size)
                                           ;;(/ *board-size* 4 2)
                                           (* *board-size* (+ 1 (/ 1 4 2)))
                                           (- (/ cell-size 2))))
              (draw-line* stream
                          *board-margin* *board-margin*
                          (+ *board-margin* (* (size game) cell-size))
                          *board-margin*
                          :line-thickness 1)
              (draw-line* stream
                          *board-margin* (+ *board-margin* cell-size)
                          (+ *board-margin* (* (size game) cell-size))
                          (+ *board-margin* cell-size)
                          :line-thickness 1)
              (dotimes (i (1+ (size game)))
                (let ((x1 *board-margin*)
                      (x2 (+ *board-margin* (* i cell-size)))
                      (x3 (+ *board-margin* cell-size)))
                  (draw-line* stream x2 x1 x2 x3 :line-thickness 1))))))))

(let ((info-output-record nil))
  ;;(defun display-info ()
  (defmethod display-info ((frame sudoku-frame) stream)
    (let (;;(stream (get-frame-pane *sudoku-frame* 'info-pane))
          (game (car (rec-playing *game-record*))))
      (when (output-record-p info-output-record)
        (erase-output-record info-output-record stream nil))
      (setf info-output-record
            (with-new-output-record (stream)
              (draw-text* stream (format nil "size: ~Ax~A    level: ~A"
                                         (nr game) (nc game)
                                         (cond ((<= (level game) 0.4) "easy")
                                               ((<= (level game) 0.6) "medium")
                                               ((<= (level game) 0.74) "difficult")
                                               (t "very difficult")))
                          0 (truncate (/ *info-height* 2))
                          :align-x :left :align-y :center
                          :text-size (truncate (* 0.8 *info-height*))))))))

;; actions

(define-sudoku-frame-command (com-sel-tile)
    ((tile 'tile)
     (x 'real) (y 'real))
  (declare (ignore x y))
  ;;(if (eql *selected-input-val* (val tile))
  ;;    (setf *selected-input-val* nil)
  ;;    (setf *selected-input-val* (val tile)))
  (setf *selected-input-val* (val tile))
  (make-tile-all))

(define-presentation-to-command-translator translator-dnd-tile
    (tile com-sel-tile sudoku-frame)
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
    
(define-presentation-to-command-translator translator-click-cell
    (cell-blank com-click-cell sudoku-frame)
    (object x y)
  `(,object ,x ,y))

;; commands

(define-sudoku-frame-command com-quit-frame ()
  (let ((playing (car (rec-playing *game-record*))))
    (when (and playing
               (symbolp (check playing))
               (equal (symbol-name (check playing)) "CORRECT"))
      (move-game *game-record* rec-playing rec-done)))
  (save-sudoku-game-record *game-record* (sudoku-record-file))
  (frame-exit *application-frame*))

(define-sudoku-frame-command com-start (&key (fresh 'boolean :default t)
                                             (rotate 'boolean :default nil))
  (let ((playing (car (rec-playing *game-record*)))
        (stream (get-frame-pane *sudoku-frame* 'sudoku-pane)))
    (when (and playing
               (symbolp (check playing))
               (equal (symbol-name (check playing)) "CORRECT"))
      (move-game *game-record* rec-playing rec-done)
      (setf playing nil))
    (cond ((or fresh (null playing))
           (unless *keep-playing-record*
             (setf (rec-playing *game-record*) nil))
           (let ((next-game (car (rec-new *game-record*))))
             (when (or (null next-game)
                       (not (eql (nr next-game) (nr *game-record*)))
                       (not (eql (nc next-game) (nc *game-record*)))
                       (not (eql (level next-game) (level *game-record*))))
               (create-new-games *game-record*)))
           (move-game *game-record* rec-new rec-playing)
           (setf playing (car (rec-playing *game-record*))))
          (rotate
           (setf (rec-playing *game-record*)
                 (append (cdr (rec-playing *game-record*))
                         (list (car (rec-playing *game-record*)))))
           (setf playing (car (rec-playing *game-record*)))))
    (unless (<= (size playing) 4)
      (setf *use-tile* nil))
    (display-sudoku-board *sudoku-frame* stream)
    (when (output-record-p *game-output-record*)
      (erase-output-record *game-output-record* stream nil))
    (mapc #'(lambda (x) (erase-output-record (second x) stream nil))
          *game-output-record-cells*)
    (setf *game-output-record-cells* '())
    (setf *selected-cell* (or (car (reverse (blank-cells (table playing))))
                              '(0 0)))
    (setf *game-output-record*
          (with-new-output-record (stream)
            (dotimes (row (size playing))
              (dotimes (col (size playing))
                (make-cell row col)))
            (make-tile-all)))
    (display-info *sudoku-frame* (get-frame-pane *sudoku-frame* 'info-pane))))

(define-sudoku-frame-command com-other ()
  (com-start :fresh nil :rotate t))

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
         (chk (check game))
         (rec nil))
    (debug-msg "~A~%" (table game))
    (debug-msg "[CHECK] ~A~%" chk)
    (when (listp chk)
      (redraw-cells-all)
      (mapc #'(lambda (rc) (let ((r (first rc)) (c (second rc)))
                             (erase-cell r c)
                             (make-cell r c t)))
            (car chk)))
    (let* ((msg-img (assoc (if (and (symbolp chk)
                                    (equal (symbol-name chk) "CORRECT"))
                               "msg-correct"
                               "msg-incorrect")
                           *msg-images* :test 'equal))
           (img-array (second msg-img))
           (img-design (third msg-img)))
      (when (and img-array img-design)
        (setf rec (with-new-output-record (*standard-output*)
                    (draw-pattern* *standard-output*
                                   (make-pattern img-array img-design)
                                   (+ *board-margin*
                                      (/ *board-size* 2)
                                      (- (/ (array-dimension img-array 1) 2)))
                                   (+ (* 2 *board-margin*)
                                      *board-size*
                                      (/ *board-size* 4 2)
                                      (- (/ (array-dimension img-array 0) 2))))))))
    (notify-user *sudoku-frame* (if (and (symbolp chk)
                                         (equal (symbol-name chk) "CORRECT"))
                               "CORRECT!"
                               "INCORRECT"))
    (when (output-record-p rec)
      (erase-output-record rec *standard-output* nil))
    (redraw-cells-all)))

(define-sudoku-frame-command com-size
    ((nr 'integer :default 2 :prompt "Block Rows")
     (nc 'integer :default 2 :prompt "Block Columns"))
  (setf (nr *game-record*) nr)
  (setf (nc *game-record*) nc)
  (setf *selected-input-val* nil)
  (unless (<= (* nr nc) 4)
    (setf *use-tile* nil))
  (erase-all-outputs)
  (com-start))

(define-sudoku-frame-command com-level
    ((level 'interger
            :default 0.5
            :prompt "Level"))
  (setf (level *game-record*) level)
  (debug-msg "Set level ~A~%" level)
  (erase-all-outputs)
  (com-start))

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
                   :menu '(("(2x2)^2" :command (com-size 2 2))
                           ("(2x3)^2" :command (com-size 2 3))
                           ("(4x2)^2" :command (com-size 4 2))
                           ("(3x3)^2" :command (com-size 3 3))
                           ("(4x4)^2" :command (com-size 4 4))))

(make-command-table 'level-command-table
                   :errorp nil
                   :menu '(("Easy" :command (com-level 1/3))
                           ("Medium" :command (com-level 0.5))
                           ("Difficult" :command (com-level 51/81))
                           ("Very Difficult" :command (com-level 1))))

(make-command-table 'style-command-table
                   :errorp nil
                   :menu '(("Number" :command (com-style 1))
                           ("Image" :command (com-style 2))))

(make-command-table 'menubar-command-table
                    :errorp nil
                    :menu '(("Quit" :command com-quit-frame)
                            ("New" :command com-start)
                            ("Other" :command com-other)
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

(defmethod init-sudoku ((frame sudoku-frame) stream)
  (com-start :fresh nil))

(defun run ()
  (setf *game-output-record* nil)
  (setf *game-output-record-cells* nil)
  (setf *game-output-record-board* nil)
  (setf *game-output-record-tile-board* nil)
  (setf *selected-cell* nil)
  (setf *use-tile* t)
  (setf *selected-input-val* nil)
  (setf *game-record* (load-sudoku-game-record (sudoku-record-file)))
  (if (null *game-record*)
      (setf *game-record* (make-instance 'sudoku-game-record)))
  (setf *sudoku-frame* (make-application-frame 'sudoku-frame))
  (unless *keep-playing-record*
    (setf (command-enabled 'com-other *sudoku-frame*) nil))
  (if *debug-output-p*
      (setf (frame-current-layout *sudoku-frame*) 'debug)
      (setf (frame-current-layout *sudoku-frame*) 'default))
  (run-frame-top-level *sudoku-frame*))

(eval-when (:load-toplevel)
  (load-images "shape1"))


