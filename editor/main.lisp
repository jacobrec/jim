(defpackage :jim-app
  (:use :common-lisp :jim-utils)
  (:export run-app))
(in-package :jim-app)

(defvar *running* t)

(defun move-to-cursor (r c state &optional (propegate t))
  (let ((cur (getf state :cur)))
    (setf (cursor-line cur) (min (1- (term-height)) (max 0 r)))
    (setf (cursor-col cur) (min (1- (term-width)) (max 0 c)))
    (set-dirty state :status)
    (when propegate
      (setf (cursor-index cur)
            (jbrope:coord-to-idx
             (jbedit:buffer-head (getf state :buffer))
             (cursor-line cur)
             (cursor-col cur)))
      (refresh-cursor state))))

(defun refresh-cursor (state)
  (let ((loc (jbrope:idx-to-coord
               (jbedit:buffer-head
                 (getf state :buffer))
               (cursor-index (getf state :cur)))))
    (move-to-cursor (car loc) (cdr loc) state nil)))

(defun slide-cursor (state amount)
  (incf (cursor-index (getf state :cur)) amount)
  (refresh-cursor state))


(defun set-dirty (state place)
  (setf (getf state :redraw) (cons place (getf state :redraw))))

(defun set-mode (state mode)
  (set-dirty state :status)
  (set-dirty state :cmd)
  (setf (getf state :mode) mode))


(defun move-cursor (r c state)
  (move-to-cursor
    (+ r (cursor-line (getf state :cur)))
    (+ c (cursor-col (getf state :cur)))
    state))


(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (jim-utils:command "?1049" #\h)
  (let ((state (list
                 :tabs (list "tmp.txt")
                 :selected 0
                 :mode :normal
                 :buffer (jbedit:open-buff "tmp.txt")
                 :cur (jim-utils:make-cursor
                        :index 0
                        :line 0
                        :col 0)
                 :redraw (list :status :buffer :tabs :cmd)
                 :lastchar #\space
                 :cmd nil)))
    (jim-io:draw-screen state)
    (loop while *running* do
      (let ((c (read-char)))
        (do-input state c)
        (jim-io:draw-screen state))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (state ch)
  (setf (getf state :lastchar) ch)
  (case (getf state :mode)
    ((:normal)
     (cond
       ((char= #\: ch)
        (setf (getf state :cmd) nil)
        (set-mode state :cmd))
       ((char= #\i ch) (set-mode state :insert))
       ((char= #\h ch) (move-cursor 0 -1 state))
       ((char= #\j ch) (move-cursor 1 0 state))
       ((char= #\k ch) (move-cursor -1 0 state))
       ((char= #\l ch) (move-cursor 0 1 state))
       ((char= #\0 ch) (move-cursor 0 -1000000 state))
       ((char= #\$ ch) (move-cursor 0 1000000 state))
       ((char= #\x ch)
        (setf (getf state :buffer)
              (jbedit:del-from (getf state :buffer)
                    (cursor-index (getf state :cur))
                    (1+ (cursor-index (getf state :cur)))))
        (set-dirty state :buffer))
       ((char= #\u ch) (setf (getf state :buffer)
                             (jbedit:undo (getf state :buffer)))
                       (set-dirty state :buffer))))
    ((:cmd)
     (cond
       ((char= #\escape ch) (set-mode state :normal))
       ((char= #\rubout ch) (setf (getf state :cmd)
                                  (cdr (getf state :cmd)))
                            (set-dirty state :cmd))
       ((char= #\return ch)
        (do-command state)
        (set-mode state :normal)
        (set-dirty state :cmd))
       (t (setf (getf state :cmd) (cons ch (getf state :cmd)))
          (set-dirty state :cmd))))
    ((:insert)
     (cond
       ((char= #\escape ch) (set-mode state :normal))
       ((char= #\rubout ch)
        (setf (getf state :buffer)
              (jbedit:del-from (getf state :buffer)
                             (1- (cursor-index (getf state :cur)))
                             (cursor-index (getf state :cur))))
        (slide-cursor state -1)
        (set-dirty state :buffer))
       ((char= #\return ch)
        (add-char state #\newline))
       (t (add-char state ch))))))

(defun add-char (state ch)
  (setf (getf state :buffer)
        (jbedit:insert (getf state :buffer)
                       (make-string 1 :initial-element ch)
                       (cursor-index (getf state :cur))))
  (slide-cursor state 1)
  (set-dirty state :buffer))

(defun do-command (state)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (getf state :cmd))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))