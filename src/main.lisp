(defpackage :jim-app
  (:use :common-lisp)
  (:export run-app))

(defvar *running* t)

(defstruct cursor
  index
  line
  col)

(defun move-to-cursor (r c state)
  (let ((cur (getf state 'cur)))
    (setf (cursor-line cur) (max 1 r))
    (setf (cursor-col cur) (max 1 c))
    (setf (cursor-index cur)
          (jbrope:coord-to-idx
           (jbedit:buffer-head (getf state 'buffer))
           (1- (cursor-line cur))
           (1- (cursor-col cur))))))

(defun move-cursor (r c state)
  (move-to-cursor
    (+ r (cursor-line (getf state 'cur)))
    (+ c (cursor-col (getf state 'cur)))
    state))


(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (command "?1049" #\h)
  (let ((state `(tabs ("tmp.txt")
                     selected 0
                     mode normal
                     buffer ,(buffer-new "tmp.txt")
                     cur ,(make-cursor
                            :index 0
                            :line 1
                            :col 1)
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
       ((char= #\i ch) (setf (getf state 'mode) 'insert))
       ((char= #\h ch) (move-cursor 0 -1 state))
       ((char= #\j ch) (move-cursor 1 0 state))
       ((char= #\k ch) (move-cursor -1 0 state))
       ((char= #\l ch) (move-cursor 0 1 state))))
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
       (t (jbedit:insert (getf state 'buffer)
                         (make-string 1 :initial-element ch)
                         (cursor-index (getf state 'cur))))))))

(defun do-command (state)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (getf state 'cmd))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
