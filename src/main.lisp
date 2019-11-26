(defpackage :jim-app
  (:use :common-lisp :jim-utils)
  (:export run-app))

(defvar *running* t)

(defstruct cursor
  index
  line
  col)

(defun move-to-cursor (r c state)
  (let ((cur (getf state 'cur)))
    (setf (cursor-line cur) (min (1- (term-height)) (max 1 r)))
    (setf (cursor-col cur) (min (term-width) (max 1 c)))
    (set-dirty state 'status)
    (setf (cursor-index cur)
          (jbrope:coord-to-idx
           (jbedit:buffer-head (getf state 'buffer))
           (1- (cursor-line cur))
           (1- (cursor-col cur))))))

(defun set-dirty (state place)
  (setf (getf state 'redraw) (cons place (getf state 'redraw))))

(defun set-mode (state mode)
  (set-dirty state 'status)
  (set-dirty state 'cmd)
  (setf (getf state 'mode) mode))


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
                 buffer ,(jbedit:open-buff "tmp.txt")
                 cur ,(make-cursor
                        :index 0
                        :line 1
                        :col 1)
                 redraw (status buffer tabs cmd)
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
        (set-mode state 'cmd))
       ((char= #\i ch) (set-mode state 'insert))
       ((char= #\h ch) (move-cursor 0 -1 state))
       ((char= #\j ch) (move-cursor 1 0 state))
       ((char= #\k ch) (move-cursor -1 0 state))
       ((char= #\l ch) (move-cursor 0 1 state))
       ((char= #\0 ch) (move-cursor 0 -1000000 state))
       ((char= #\$ ch) (move-cursor 0 1000000 state))))
    ((cmd)
     (cond
       ((char= #\escape ch) (set-mode state 'normal))
       ((char= #\rubout ch) (setf (getf state 'cmd)
                                  (cdr (getf state 'cmd)))
                            (set-dirty state 'cmd))
       ((char= #\return ch)
        (do-command state)
        (set-mode state 'normal)
        (set-dirty state 'cmd))
       (t (setf (getf state 'cmd) (cons ch (getf state 'cmd)))
          (set-dirty state 'cmd))))
    ((insert)
     (cond
       ((char= #\escape ch) (set-mode state 'normal))
       ((char= #\rubout ch) (vector-pop (getf state 'buffer)))
       (t (setf (getf state 'buffer)
                (jbedit:insert (getf state 'buffer)
                             (make-string 1 :initial-element ch)
                             (cursor-index (getf state 'cur))))
          (move-cursor 0 1 state)
          (set-dirty state 'buffer))))))

(defun do-command (state)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (getf state 'cmd))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
