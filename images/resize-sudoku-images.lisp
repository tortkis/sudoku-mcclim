#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#

(unless (> (length sb-ext:*posix-argv*) 1)
  (format t "Usage: ~A [--all] | image-directory ...~%" (car sb-ext:*posix-argv*))
  (quit))

(load "resize-xpm.lisp")

(let ((path-all (if (equal (cadr sb-ext:*posix-argv*)  "--all")
                    (directory (pathname (format nil "~A*/s4-*.xpm" *default-pathname-defaults*)))
                    (apply #'append
                           (mapcar #'(lambda (x)
                                       (directory (pathname (format nil "~A~A/s4-*.xpm" *default-pathname-defaults* x))))
                                   (cdr sb-ext:*posix-argv*))))))
  (dolist (path path-all)
    (format t "converting ~A~%" path)
    (let ((xpm (make-instance 'xpm))
          (xpm-resized nil))
      (read-xpm xpm path)
      (dolist (size '(6 8 9))
        (let ((path-new (make-pathname
                         :directory (pathname-directory path)
                         :name (format nil "s~A-~A" size (subseq (pathname-name path) 3))
                         :type (pathname-type path))))
          (unless (probe-file path-new)
            (setf xpm-resized (resize xpm (/ 4 size) (/ 4 size)))
            (write-xpm xpm-resized path-new)))))))