(defpackage :jim-app
  (:use :common-lisp :jim-utils :jim-editor)
  (:export run-app))
(in-package :jim-app)

(defvar *running* t)

(defun set-dirty (edit place)
  (setf (editor-redraw edit) (cons place (editor-redraw edit))))

(defun set-mode (edit mode)
  (set-dirty edit :status)
  (set-dirty edit :cmd)
  (setf (editor-mode edit) mode))

(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (jim-utils:command "?1049" #\h)
  (let ((edit (new-editor nil)))
    (loop while *running* do
      (jim-io:draw-screen edit)
      (do-input edit (read-char))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (edit ch)
  (set-dirty edit :status)
  (case (editor-mode edit)
    ((:normal)
     (cond
       ((char= #\: ch)
        (setf (editor-cmd edit) nil)
        (set-mode edit :cmd))
       ((char= #\i ch) (set-mode edit :insert))
       ((char= #\h ch) (move-cursor edit 0 -1))
       ((char= #\j ch) (move-cursor edit 1 0))
       ((char= #\k ch) (move-cursor edit -1 0))
       ((char= #\l ch) (move-cursor edit 0 1))
       ((char= #\0 ch) (move-cursor edit 0 -1000000))
       ((char= #\$ ch) (move-cursor edit 0 1000000))
       ((char= #\x ch) (delete-from edit
                                   (editor-index edit)
                                   (1+ (editor-index edit)))
        (set-dirty edit :buffer))
       ((char= #\u ch) (undo edit 1)
                       (set-dirty edit :buffer))))
    ((:cmd)
     (cond
       ((char= #\escape ch) (set-mode edit :normal))
       ((char= #\rubout ch) (setf (editor-cmd edit)
                                  (cdr (editor-cmd edit)))
                            (set-dirty edit :cmd))
       ((char= #\return ch)
        (do-command edit)
        (set-mode edit :normal)
        (set-dirty edit :cmd))
       (t (setf (editor-cmd edit) (cons ch (editor-cmd edit)))
          (set-dirty edit :cmd))))
    ((:insert)
     (cond
       ((char= #\escape ch) (set-mode edit :normal))
       ((char= #\rubout ch) (delete-from edit (1- (editor-index edit))
                                              (editor-index edit))
        (slide-cursor edit -1)
        (set-dirty edit :buffer))
       ((char= #\return ch)
        (add-char edit #\newline))
       (t (add-char edit ch))))))

(defun add-char (edit ch)
  (insert edit (make-string 1 :initial-element ch)
               (cursor-index (editor-cur edit)))
  (slide-cursor edit 1)
  (set-dirty edit :buffer))

(defun do-command (edit)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (editor-cmd edit))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
