

(defparameter *color-chars* (concatenate 'string "+@#$%&*=-;>,')!~{]^/(_:<[}|"
                                         "1234567890abcdefghijklmnopqrstuvwxyz"
                                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defclass color-set ()
  ((idx :accessor idx :initarg :idx :initform 0)
   (colors :accessor colors :initarg :colors :initform '())))

(defmethod init ((cs color-set))
  (setf (idx cs) 0
        (colors cs) '()))

(defmethod add-color ((cs color-set) rgb)
  (let ((c (find-if #'(lambda (x) (eql (cdr x) rgb)) (colors cs))))
    (cond (c
           (car c))
          (t
           (let* ((chnum (length *color-chars*))
                  (cpp (if (<= (idx cs) 0) 1 (1+ (truncate (log (idx cs) chnum)))))
                  (ch "")
                  (ch-idx nil))
             (dotimes (k cpp)
               (setf ch-idx (mod (truncate (/ (idx cs) (expt chnum k)))
                                 (expt chnum (1+ k))))
               (setf ch (concatenate 'string
                                     (string (char *color-chars* ch-idx))
                                     ch)))
             (when (> (length ch) (length (caar (colors cs))) 0)
               (let ((co-ch (string (char *color-chars* (1- ch-idx)))))
                 (setf (colors cs)
                       (mapcar #'(lambda (x)
                                   (cons (concatenate 'string co-ch (car x))
                                         (cdr x)))
                               (colors cs)))))
             (push (cons ch rgb) (colors cs))
             (incf (idx cs))
             ch)))))


(defclass xpm ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (ncolors :accessor ncolors :initarg :ncolors)
   (chars-per-pixel :accessor cpp :initarg :cpp)
   (colors :accessor colors :initarg :colors :initform '())
   (pixels :accessor pixels :initarg :pixels :initform '())))

(defmethod check ((x xpm))
  (not (some #'null (list (width x) (height x) (ncolors x) (cpp x)
                          (colors x) (pixels x)))))

(defmethod read-xpm ((xpm xpm) file)
  (with-open-file (str file)
    (do ((line (read-line str nil :eof)
               (read-line str nil :eof)))
        ((or (eql line :eof)
             (search "static char" line))))
    (let* ((xpm-values (read-line str nil nil))
           (p0 (position #\" xpm-values))
           (p1 (when p0 (position #\Space xpm-values :start (1+ p0))))
           (p2 (when p1 (position #\Space xpm-values :start (1+ p1))))
           (p3 (when p2 (position #\Space xpm-values :start (1+ p2))))
           (p4 (when p3 (position #\" xpm-values :start (1+ p3)))))
      (setf (width xpm) (when p4 (parse-integer (subseq xpm-values (1+ p0) p1))))
      (setf (height xpm) (when p4 (parse-integer (subseq xpm-values (1+ p1) p2))))
      (setf (ncolors xpm) (when p4 (parse-integer (subseq xpm-values (1+ p2) p3))))
      (setf (cpp xpm) (when p4 (parse-integer (subseq xpm-values (1+ p3) p4))))
      (setf (colors xpm) '())
      (setf (pixels xpm) (make-array (list (height xpm) (width xpm))
                                     :initial-element nil)))
    (dotimes (i (ncolors xpm))
      (let* ((line (read-line str nil nil))
             (p0 (position #\" line))
             (p1 (when p0 (position-if-not #'(lambda (x) (or (eql x #\Space)
                                                             (eql x #\Tab)))
                                           line :start (+ p0 (cpp xpm) 1))))
             (p2 (when p1 (position #\Space line :start p1)))
             (p3 (when p2 (position #\" line :start p2)))
             (ckey (when p3 (subseq line (+ p0 1) (+ p0 1 (cpp xpm)))))
             (cval (when p3 (subseq line (1+ p2) p3))))
        (when (and ckey cval)
          (push (cons ckey (if (eql (char cval 0) #\#)
                               (read-from-string (concatenate
                                                  'string
                                                  "#X"
                                                  (subseq cval 1)))
                               -1))
                (colors xpm)))))
    (dotimes (i (height xpm))
      (let* ((line (read-line str nil nil))
             (p0 (position #\" line))
             (p1 (when p0 (position #\" line :start (1+ p0))))
             (line-image (subseq line (1+ p0) p1))
             (line-image-lis '()))
        (do ((j 0 (+ j (cpp xpm))))
            ((>= j (length line-image)))
          (push (subseq line-image j (+ j (cpp xpm))) line-image-lis))
        (dotimes (j (length line-image-lis))
          (setf (aref (pixels xpm) i (- (length line-image-lis) j 1))
                (nth j line-image-lis)))))))

(defmethod write-xpm ((xpm xpm) file)
  (with-open-file (str file :direction :output :if-exists :supersede)
    (format str "/* XPM */~%")
    (format str "static char * ball_red_xpm[] = {~%")
    (format str "\"~A ~A ~A ~A\""
            (width xpm) (height xpm) (ncolors xpm) (length (caar (colors xpm))))
    (dolist (color (colors xpm))
      (if (< (cdr color) 0)
          (format str ",~%\"~A c None\"" (car color))
          (format str ",~%\"~A c #~6,'0X\"" (car color) (cdr color))))
    (dotimes (row (height xpm))
      (format str ",~%\"")
      (dotimes (col (width xpm))
        (format str "~A" (aref (pixels xpm) row col)))
      (format str "\""))
    (format str "};~%")))

(defun split-rgb (rgb)
  (if (< rgb 0)
      (list nil nil nil)
      (list (truncate (/ rgb (expt #X100 2)))
            (mod (truncate (/ rgb #X100)) #X100)
            (mod rgb #X100))))

(defmethod resize ((xpm xpm) x-ratio y-ratio)
  (let* ((width-new (truncate (* (width xpm) x-ratio)))
         (height-new (truncate (* (height xpm) y-ratio)))
         (c-d1) (c-r1) (c-d2) (c-r2)
         (r-d1) (r-r1) (r-d2) (r-r2)
         (pixels-new (make-array (list height-new width-new)))
         (cs (make-instance 'color-set)))
    (dotimes (row height-new)
      (dotimes (col width-new)
        (multiple-value-bind (d1 r1)
            (truncate (/ (* col (width xpm)) width-new))
          (multiple-value-bind (d2 r2)
              (truncate (/ (* (1+ col) (width xpm)) width-new))
            (setf c-d1 d1
                  c-r1 r1
                  c-d2 (if (>= d2 (width xpm)) (1- d2) d2)
                  c-r2 (if (>= d2 (width xpm)) 1 r2))))
        (multiple-value-bind (d1 r1)
            (truncate (/ (* row (height xpm)) height-new))
          (multiple-value-bind (d2 r2)
              (truncate (/ (* (1+ row) (height xpm)) height-new))
            (setf r-d1 d1
                  r-r1 r1
                  r-d2 (if (>= d2 (height xpm)) (1- d2) d2)
                  r-r2 (if (>= d2 (height xpm)) 1 r2))))
        (let ((ratio 0)
              (color-accum '(0 0 0))
              (sum 0)
              (color nil)
              (rgb-lis '())
              (rlen (- (1+ r-d2) r-d1))
              (clen (- (1+ c-d2) c-d1)))
          (dotimes (r rlen)
            (dotimes (c clen)
              (setf ratio (* (if (= c 0) (- 1 c-r1) 1.0)
                             (if (= c (1- clen)) c-r2 1.0)
                             (if (= r 0) (- 1 r-r1) 1.0)
                             (if (= r (1- rlen)) r-r2 1.0)))
              (setf rgb-lis (split-rgb
                             (cdr (assoc
                                   (aref (pixels xpm) (+ r r-d1) (+ c c-d1))
                                   (colors xpm)
                                   :test 'equal))))
              (setf color-accum
                    (mapcar #'+
                            color-accum
                            (mapcar #'(lambda (x) (if (null x) 0 (* x ratio)))
                                    rgb-lis)))
              (unless (some #'null rgb-lis)
                (incf sum ratio))))
          (setf color
                (if (zerop sum)
                    -1
                    (+ (* (mod (truncate (/ (first color-accum) sum)) #x100) #x10000)
                       (* (mod (truncate (/ (second color-accum) sum)) #x100) #x100)
                       (mod (truncate (/ (third color-accum) sum)) #x100))))
          (add-color cs color)
          (setf (aref pixels-new row col) color))))
    (dotimes (row height-new)
      (dotimes (col width-new)
        (setf (aref pixels-new row col)
              (car (find (aref pixels-new row col) (colors cs) :key #'cdr)))))
    (make-instance 'xpm
                   :width width-new
                   :height height-new
                   :ncolors (length (colors cs))
                   :cpp (length (caar (colors cs)))
                   :colors (colors cs)
                   :pixels pixels-new)))
                   

;;(dolist (path (directory (pathname "/Users/torutakaishi/cl/sudoku/images/*/s4-*.xpm")))
;;  (let ((xpm (make-instance 'xpm))
;;        (xpm-resized nil))
;;    (read-xpm xpm path)
;;    (dolist (size '(6 8 9))
;;      (let ((path-new (make-pathname
;;                       :directory (pathname-directory path)
;;                       :name (format nil "s~A-~A" size (subseq (pathname-name path) 3))
;;                       :type (pathname-type path))))
;;        (unless (probe-file path-new)
;;          (setf xpm-resized (resize xpm (/ 4 size) (/ 4 size)))
;;          (write-xpm xpm-resized path-new))))))