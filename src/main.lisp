(defpackage :jim-app
  (:use :common-lisp)
  (:export run-app))

(defvar *running* t)

(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (command "?1049" #\h)
  (let ((state `(tabs ("file 1" "file 2" "file 3")
                     selected 0
                     mode normal
                     buffer ,(make-array 0 :adjustable t :fill-pointer 0)
                     cmd nil)))
    (draw-screen state)
    (loop while *running* do
      (let ((c (read-char)))
        (do-input state c)
        (draw-screen state))))
  (command "?1049" #\l)
  (stty '("sane")))

(defun do-input (state ch)
  (case (getf state 'mode)
    ((normal)
     (cond
       ((char= #\: ch)
        (setf (getf state 'cmd) nil)
        (setf (getf state 'mode) 'cmd))
       ((char= #\i ch) (setf (getf state 'mode) 'insert))))
    ((cmd)
     (cond
       ((char= #\escape ch) (setf (getf state 'mode) 'normal))
       ((char= #\rubout ch) (setf (getf state 'cmd)
                                  (cdr (getf state 'cmd))))
       ((char= #\return ch)
        (do-command state)
        (setf (getf state 'mode) 'normal))
       (t (setf (getf state 'cmd) (cons ch (getf state 'cmd))))))
    ((insert)
     (cond
       ((char= #\escape ch) (setf (getf state 'mode) 'normal))
       ((char= #\rubout ch) (vector-pop (getf state 'buffer)))
       (t (vector-push-extend ch (getf state 'buffer)))))))

(defun do-command (state)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (getf state 'cmd))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
