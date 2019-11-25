(defpackage :jedit-app
  (:use :common-lisp)
  (:export run-app))

(defun run-app ()
  (trivial-shell:shell-command "stty -echo && stty raw")
  (let ((state `(tabs ("file 1" "file 2" "file 3")
                     selected 0
                     mode "Normal"
                     buffer ,(buffer-new)
                     cmd nil)))
    (draw-screen state)
    (loop while t do
      (let ((c (read-char)))
        (setf (getf state 'buffer)
              (buffer-set
                (getf state 'buffer)
                (concatenate 'string
                             (getf state 'buffer)
                             (list c))))
        (draw-screen state))))

  (trivial-shell:shell-command "stty sane"))



