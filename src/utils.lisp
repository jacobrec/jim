(defpackage :jim-utils
  (:use :common-lisp)
  (:export stty
           shell
           shell-stream))

(defun stty (args)
  (shell-stream "/usr/bin/stty" args))

(defun shell (cmd args)
    (read-line (process-output (shell-stream cmd args))))

(defun shell-stream (cmd args)
  (let ((process (sb-ext:run-program cmd args
                                    :output :stream
                                    :input t
                                    :wait t)))
    (process-output process)))


(let ((w 0) (h 0))
  (defun term-width ()
    (when (= w 0)
      (let ((strm (stty '("size"))))
        (read strm) ; ignore height
        (let ((nw (read strm)))
          (when nw
            (setf w nw)))))
    w)

  (defun term-height ()
    (when (= h 0)
      (let ((nh (read (stty '("size")))))
        (when nh
          (setf h nh))))
    h))
