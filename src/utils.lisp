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

