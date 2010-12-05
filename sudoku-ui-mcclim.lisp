
(in-package :sudoku-mcclim)

;; auxiliary functions

(defmacro debug-msg (format &rest args)
  `(when *debug-output-p*
     (format (get-frame-pane *sudoku-frame* 'sudoku-debug-pane) ,format ,@args)))

(defun rgb-color (rgb)
  "create a MCCLIM color object from a 6-digit hexadecimal code"
  (make-rgb-color (/ (truncate (/ rgb (expt #X100 2))) #XFF)
                  (/ (mod (truncate (/ rgb #X100)) #X100) #XFF)
                  (/ (mod rgb #X100) #XFF)))

;; game record

(defvar *game-record* nil)
(defun sudoku-record-file ()
  (merge-pathnames (make-pathname :name ".sudoku" :type "record")
                   (user-homedir-pathname)))
   
;; GUI parameters

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
(defvar *making-memo-p* nil)

;; colors

(defstruct colors
  background
  board
  cell-text-open
  cell-text-mask
  cell-text-memo
  cell-open
  cell-open-sel
  cell-open-err
  cell-mask
  cell-mask-sel
  cell-mask-err
  tile-sel)

(defparameter *colors*
  (make-colors
   :background (rgb-color #X4682b4)
   :board (rgb-color #X87ceeb)
   :cell-text-open +black+
   :cell-text-mask +blue+
   :cell-text-memo +blue+
   :cell-open (rgb-color #X87ceeb) ; (rgb-color #X00ced1)
   :cell-open-sel (rgb-color #X40e0d0)
   :cell-open-err +magenta+
   :cell-mask (rgb-color #X4682b4) ; (rgb-color #X87ceeb)
   :cell-mask-sel +white+
   :cell-mask-err +red+
   :tile-sel +blue+))

;; images

(defparameter *tile-themes*
  '(("shape1" ("circle1" "star2" "square1" "triangle2"))
    ("shape2" ("ball-red" "ball-yellow" "ball-green"
               "ball-blue" "ball-magenta" "ball-orange"
               "ball-purple" "ball-gray" "ball-green2"))
    ("number1" ("number-comic-1" "number-comic-2" "number-comic-3"
                "number-comic-4" "number-comic-5" "number-comic-6"
                "number-comic-7" "number-comic-8" "number-comic-9"))
    ("k66" ("k66" "g66" "tmm" "966"))
    ;;("func-animals" ("Woof2x" "RedDog" "Pointy" "Doggie"))
    ("fruits" ("RedApple" "Strawberry" "Orange" "Pear"))))
(defparameter *msg-names* '("msg-correct2" "msg-incorrect2"))
(defvar *tile-images* nil)
(defvar *msg-images* nil)
(defvar *use-tile* t)
(defvar *selected-tile-theme* "shape1")

(defun load-images (theme-name)
  (let ((tile-images '()))
    (dolist (path (directory
                   (merge-pathnames
                    (pathname (format nil "~A/s?-*.xpm" theme-name))
                    sudoku.system::*images-path*)))
      (let ((img-name (pathname-name path)))
        (multiple-value-bind (array design)
            (climi::xpm-parse-file path)
          (push (list img-name array design) tile-images))))
    (cons theme-name (nreverse tile-images))))

(defun load-msg-images ()
  (let ((msg-images '()))
    (dolist (msg-name *msg-names*)
      (multiple-value-bind (array design)
          (climi::xpm-parse-file (merge-pathnames
                                  (make-pathname
                                   :directory `(:relative "messages")
                                   :name msg-name
                                   :type "xpm")
                                  sudoku.system::*images-path*))
        (push (list msg-name array design) msg-images)))
    msg-images))

(defun load-images-all (themes)
  (let ((images-all '()))
    (dolist (theme themes)
      (push (load-images theme) images-all))
    images-all))

(defun pick-image (val)
  (let ((img-name (nth (1- val)
                       (second (assoc *selected-tile-theme* *tile-themes* :test 'equal))))
        (game-size (size (car (rec-playing *game-record*)))))
    (cdr (find-if #'(lambda (x) (equal (car x)
                                       (format nil "s~A-~A" game-size img-name)))
                  (cdr (assoc *selected-tile-theme*
                              *tile-images* :test 'equal))))))

;; functions

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
(defclass sudoku-memo-input-pane (clim-stream-pane) ())
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
                           :background (colors-background *colors*)
                           :display-time nil
                           :display-function 'init-sudoku))
   (info-pane (make-pane 'sudoku-info-pane
                         :display-function 'display-info
                         :display-time nil))
   (memo-pane (make-pane 'sudoku-memo-input-pane
                         :display-time nil))
   (sudoku-debug-pane (make-pane 'debug-display-pane
                                 :display-time nil)))
  (:layouts
   (default
       (vertically (:height (+ (* *board-margin* 3)
                               *board-size*
                               (* (/ 1 4) *board-size*)
                               *info-height*)
                            :width (+ (* *board-margin* 2) *board-size*))
         `(,*info-height* ,(horizontally ()
                                         (2/3 info-pane)
                                         (+fill+ memo-pane)))
         ;;(+fill+ (spacing (:thickness *board-margin*) sudoku-pane))))
         (+fill+ sudoku-pane)))
   (debug
    (vertically (:height (+ (* *board-margin* 6)
                            *board-size*
                            (* (/ 1 4) *board-size*)
                            (* (/ 1 4) *board-size*)
                            *info-height*)
                         :width (+ (* *board-margin* 4) *board-size*))
      `(,*info-height* ,(horizontally () (2/3 info-pane) (+fill+ memo-pane)))
      (5/6 (spacing (:thickness *board-margin*) sudoku-pane))
      (+fill+ (scrolling (:scroll-bars :vertical) sudoku-debug-pane))))))

(define-application-frame check-frame ()
  ()
  (:panes
   (check-pane :clim-stream
               :background +white+
               :display-time nil
               :display-function 'display-check)
   (exit-button (make-pane 'push-button
                           :label "OK" :width 400
                           :activate-callback #'(lambda (button)
                                                  (declare (ignore button))
                                                  (redraw-cells-all)
                                                  (frame-exit *application-frame*)))))
  (:layouts
   (default
       (vertically (:height 150 :width 400)
         (100 check-pane)
         (+fill+ (horizontally ()
                   (+fill+ (spacing (:thickness *board-margin*) exit-button))))))))

;; display

(defun draw-tile (val x y &key (in-tile-sel nil) (mask-p nil))
  (let* ((game-size (size (car (rec-playing *game-record*))))
         (cell-size (/ *board-size* game-size))
         (img (pick-image val))
         (img-array (first img))
         (img-design (second img)))
    (debug-msg "[draw-tile] ~A ~A ~A ~A~%" game-size *selected-tile-theme*
               (null img-array) (null img-design))
    (cond
      ((and *use-tile* img-array img-design)
       (draw-rectangle* *standard-output*
                        (+ x *cell-gap*)
                        (+ y *cell-gap*)
                        (+ x (- cell-size *cell-gap*))
                        (+ y (- cell-size *cell-gap*))
                        :filled (if (and in-tile-sel (eql val *selected-input-val*)) t nil)
                        :ink (if (and in-tile-sel (eql val *selected-input-val*))
                                 (colors-tile-sel *colors*) nil))
       (draw-pattern* *standard-output*
                      (make-pattern img-array img-design)
                      (+ x (* 0.5 (- cell-size (array-dimension img-array 1))))
                      (+ y (* 0.5 (- cell-size (array-dimension img-array 0))))))
      (t
       (draw-text* *standard-output* (format nil "~A" val)
                   (+ x (/ cell-size 2))
                   (+ y (/ cell-size 2))
                   :align-x :center :align-y :center
                   :text-size (truncate (/ cell-size 2))
                   :ink (if mask-p
                            (colors-cell-text-mask *colors*)
                            (colors-cell-text-open *colors*)))))))

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
               :ink (cond ((and err (not (empty-cell-p (get-cell game row col))))
                           (if (mask-p row col)
                               (colors-cell-mask-err *colors*)
                               (colors-cell-open-err *colors*)))
                          ((and (= row (first *selected-cell*))
                                (= col (second *selected-cell*)))
                           (if (mask-p row col)
                               (colors-cell-mask-sel *colors*)
                               (colors-cell-open-sel *colors*)))
                          (t (if (mask-p row col)
                                 (colors-cell-mask *colors*)
                                 (colors-cell-open *colors*)))))
              (when (> val 0)
                (draw-tile val
                           (+ *board-margin* (* col cell-size))
                           (+ *board-margin* (* row cell-size))
                           :mask-p (mask-p row col)))
              (when (memo game)
                (let ((m (aref (memo game) row col)))
                  (when m
                    (draw-text* *standard-output* m
                                (+ *board-margin* (* col cell-size) (* 1 *cell-gap*))
                                (+ *board-margin* (* row cell-size) (* 1 *cell-gap*))
                                :align-x :left :align-y :top
                                :text-size (truncate (/ *board-size* 4 10))
                                :ink (colors-cell-text-memo *colors*)))))))))
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
          (draw-tile val x y :in-tile-sel t)
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
                        :ink (if (eql val *selected-input-val*)
                                 (colors-cell-text-mask *colors*)
                                 (colors-cell-text-open *colors*))
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
            (draw-rectangle* stream
                             0 0
                             (+ (* 2 *board-margin*) *board-size*)
                             (+ (* 3/2 *board-margin*) *board-size*)
                             :filled t :ink (colors-board *colors*))
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

(defmethod display-info ((frame sudoku-frame) stream)
  (let ((game (car (rec-playing *game-record*))))
    (window-clear stream)
    (draw-text* stream (format nil "level: [~A]  games: [~A]"
                               (cond ((<= (level game) 0.4) "easy")
                                     ((<= (level game) 0.6) "medium")
                                     ((<= (level game) 0.74) "difficult")
                                     (t "very difficult"))
                               (length (remove-if-not
                                        #'(lambda (x) (and (eql (nr game) (nr x))
                                                           (eql (nc game) (nc x))
                                                           (eql (level game) (level x))))
                                        (rec-new *game-record*))))
                0 (truncate (/ *info-height* 2))
                :align-x :left :align-y :center
                :text-size (truncate (* 0.8 *info-height*)))))

(defun display-check (frame stream)
  (declare (ignore frame))
  (let* ((game (car (rec-playing *game-record*)))
         (chk (check game)))
    (when (listp chk)
      (redraw-cells-all)
      (mapc #'(lambda (rc) (let ((r (first rc)) (c (second rc)))
                             (erase-cell r c)
                             (make-cell r c t)))
            (car chk)))
    (let* ((msg-img (assoc (if (and (symbolp chk)
                                    (equal (symbol-name chk) "CORRECT"))
                               "msg-correct2"
                               "msg-incorrect2")
                           *msg-images*
                           :test 'equal))
           (img-array (second msg-img))
           (img-design (third msg-img)))
      (when (and img-array img-design)
        (draw-pattern* stream
                       (make-pattern img-array img-design)
                       0 0)))
    ))

;; actions

(define-sudoku-frame-command (com-sel-tile)
    ((tile 'tile)
     (x 'real) (y 'real))
  (declare (ignore x y))
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
           (pick-game-to-play *game-record*)
           (setf playing (car (rec-playing *game-record*))))
          (rotate
           (setf (rec-playing *game-record*)
                 (append (cdr (rec-playing *game-record*))
                         (list (car (rec-playing *game-record*)))))
           (setf playing (car (rec-playing *game-record*)))))
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
  (redraw-cells-all)
  (make-tile-all))

(define-sudoku-frame-command com-create
    ((num 'integer :default 10 :prompt "Number of new games"))
  (create-new-games *game-record* :game-n num))

(define-sudoku-frame-command com-reset ()
  (let ((game (car (rec-playing *game-record*))))
    (when (mask game)
      (dotimes (row (size game))
        (dotimes (col (size game))
          (when (eql (aref (mask game) row col) 1)
            (set-cell game row col (empty-cell)))))
      (redraw-cells-all))))

(let ((checking-p nil))
  (define-sudoku-frame-command com-check ()
    (unless checking-p
      (setf checking-p t)
      (run-frame-top-level
       (make-application-frame 'check-frame
                               :calling-frame *sudoku-frame*))
      (setf checking-p nil))))

(defun check-level ()
  (let ((level (level *game-record*))
        (game-size (* (nr *game-record*) (nc *game-record*))))
    (setf (command-enabled 'com-level-very-difficult *sudoku-frame*) (<= game-size 6)
          (command-enabled 'com-level-difficult *sudoku-frame*) (<= game-size 9)
          (command-enabled 'com-level-medium *sudoku-frame*) (<= game-size 9)
          (command-enabled 'com-level-easy *sudoku-frame*) t)
    (setf (level *game-record*)
          (cond ((and (> level 51/81) (> game-size 6))
                 51/81)
                ((and (> level 1/3) (> game-size 9))
                 1/3)
                (t level)))))

(define-sudoku-frame-command com-size
    ((nr 'integer :default 2 :prompt "Block Rows")
     (nc 'integer :default 2 :prompt "Block Columns"))
  (unless (and (eql (nr *game-record*) nr)
               (eql (nc *game-record*) nc))
    (move-game *game-record* rec-playing rec-new)
    (setf (nr *game-record*) nr)
    (setf (nc *game-record*) nc)
    (setf *selected-input-val* nil)
    (check-level)
    (erase-all-outputs)
    (com-start)))

(define-sudoku-frame-command com-level
    ((level 'interger :default 0.5 :prompt "Level"))
  (let ((orig-level (level *game-record*)))
    (setf (level *game-record*) level)
    (check-level)
    (unless (eql orig-level (level *game-record*))
      (move-game *game-record* rec-playing rec-new)
      (erase-all-outputs)
      (com-start))))

(define-sudoku-frame-command com-level-easy () (com-level 1/3))
(define-sudoku-frame-command com-level-medium () (com-level 0.5))
(define-sudoku-frame-command com-level-difficult () (com-level 51/81))
(define-sudoku-frame-command com-level-very-difficult () (com-level 1))

(define-sudoku-frame-command com-style
    ((style 'interger
            :default 2
            :prompt "Style")
     (img-name 'string :default "shape1"))
  (cond ((eql style 2)
         (setf *use-tile* t)
         (when img-name
           (setf *selected-tile-theme* img-name)))
        (t (setf *use-tile* nil)))
  (com-redraw)
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
                    :menu '(("Easy" :command com-level-easy)
                            ("Medium" :command com-level-medium)
                            ("Difficult" :command com-level-difficult)
                            ("Very Difficult" :command com-level-very-difficult)))

(make-command-table 'style-command-table
                    :errorp nil
                    :menu '(("Number" :command (com-style 1 ""))
                            ("Number(comic)" :command (com-style 2 "number1"))
                            ("Image(shape1)" :command (com-style 2 "shape1"))
                            ("Image(ball)" :command (com-style 2 "shape2"))
                            ("Image(k66)" :command (com-style 2 "k66"))
                            ;;("Image(animals)" :command (com-style 2 "func-animals"))
                            ("Image(fruits)" :command (com-style 2 "fruits"))))

(make-command-table 'game-command-table
                    :errorp nil
                    :menu '(("New" :command com-start)
                            ("Redraw" :command com-redraw)
                            ("Other" :command com-other)
                            ("Reset" :command com-reset)
                            ("Create 10 New Games" :command (com-create 10))
                            ("Quit" :command com-quit-frame)))

(make-command-table 'menubar-command-table
                    :errorp nil
                    :menu '(("Game" :menu game-command-table)
                            ("Size" :menu size-command-table)
                            ("Level" :menu level-command-table)
                            ("Style" :menu style-command-table)
                            ("Check" :command com-check)
                            ("Undo" :command com-undo)
                            ("Redo" :command com-redo)))

(defmethod handle-event ((pane sudoku-board-pane) (event keyboard-event))
  (let* ((game (car (rec-playing *game-record*)))
         (key (keyboard-event-character event))
         (key-name (keyboard-event-key-name event))
         (val (if (characterp key)
                  (parse-integer (coerce (list key) 'string) :junk-allowed t)
                  nil))
         (dir (position (symbol-name key-name) '("UP" "DOWN" "LEFT" "RIGHT")
                        :test 'equal))
         (prev-row (first *selected-cell*))
         (prev-col (second *selected-cell*))
         (memo nil))
    (cond ((equal (symbol-name key-name) "ESCAPE")
           (let ((str (get-frame-pane *sudoku-frame* 'memo-pane)))
             (setf *making-memo-p* nil)
             (window-clear str))
           (setf *selected-input-val* nil)
           (make-tile-all))
          ((equal (symbol-name key-name) "BACKSPACE")
           (set-cell game prev-row prev-col 0)
           (erase-cell prev-row prev-col)
           (make-cell prev-row prev-col))
          (*making-memo-p*
           nil)
          ((and (numberp val) (>= val 0) (<= val (size game))
                (eql (aref (mask game) prev-row prev-col) 1))
           (set-cell game prev-row prev-col val)
           (debug-msg "~A~%" (table game))
           (erase-cell prev-row prev-col)
           (make-cell prev-row prev-col)
           (if (= val 0)
               (setf *selected-input-val* nil)
               (setf *selected-input-val* val))
           (make-tile-all))
          (dir
           (let ((next-col (+ prev-col (cond ((= dir 2) -1)
                                             ((= dir 3) +1)
                                             (t 0))))
                 (next-row (+ prev-row (cond ((= dir 0) -1)
                                             ((= dir 1) +1)
                                             (t 0)))))
             (when (and (<= 0 next-row (1- (size game)))
                        (<= 0 next-col (1- (size game))))
               (setf *selected-cell* (list next-row next-col))
               (erase-cell prev-row prev-col)
               (make-cell prev-row prev-col)
               (erase-cell next-row next-col)
               (make-cell next-row next-col))))
          ;; memo
          ((and (string-equal (symbol-name key-name) "m")
                (eql (aref (mask game) prev-row prev-col) 1))
           (let ((str (get-frame-pane *sudoku-frame* 'memo-pane)))
             (setf *making-memo-p* t)
             (window-clear str)
             (setf memo (accept 'string :stream str :prompt "Memo"))
             (setf *making-memo-p* nil))
           (when memo
             (add-memo game prev-row prev-col memo)
             (erase-cell prev-row prev-col)
             (make-cell prev-row prev-col)))
          ;; deleting memo
          ((string-equal (symbol-name key-name) "d")
           (setf (aref (memo game) prev-row prev-col) nil)
           (erase-cell prev-row prev-col)
           (make-cell prev-row prev-col)))))

(defmethod init-sudoku ((frame sudoku-frame) stream)
  (com-start :fresh nil))


;;

(defun run ()
  (setf *game-output-record* nil)
  (setf *game-output-record-cells* nil)
  (setf *game-output-record-board* nil)
  (setf *game-output-record-tile-board* nil)
  (setf *selected-cell* nil)
  (setf *use-tile* t)
  (setf *selected-input-val* nil)
  (setf *making-memo-p* nil)
  (setf *game-record* (load-sudoku-game-record (sudoku-record-file)))
  (if (not (and *game-record*
                (valid-sudoku-game-record-p *game-record*)))
      (setf *game-record* (make-instance 'sudoku-game-record)))
  (setf *sudoku-frame* (make-application-frame 'sudoku-frame))
  (unless *keep-playing-record*
    (setf (command-enabled 'com-other *sudoku-frame*) nil))
  (if *debug-output-p*
      (setf (frame-current-layout *sudoku-frame*) 'debug)
      (setf (frame-current-layout *sudoku-frame*) 'default))
  (check-level)
  (run-frame-top-level *sudoku-frame*))

(eval-when (:load-toplevel)
  (setf *tile-images* (load-images-all (mapcar #'car *tile-themes*)))
  (setf *msg-images* (load-msg-images)))


