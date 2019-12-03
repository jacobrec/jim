(defpackage :jim-app
  (:use :common-lisp :jim-utils :jim-editor)
  (:export run-app))
(in-package :jim-app)

(defvar *running* t)


(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (jim-utils:command "?1049" #\h)
  (vim:use-vim-bindings)
  (jim-user-util:load-jimrc)
  (let ((jim.api:*editor* (new-editor nil)))
    (loop while *running* do
      (jim-io:draw-screen jim.api:*editor*)
      (do-input jim.api:*editor* (read-char))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (edit ch)
  (set-dirty edit :status)
  (case (editor-mode edit)
    ((:cmd)
     (cond
       ((char= #\escape ch) (set-mode edit :normal)
                            (setf (editor-cmdcur edit) nil))
       ((char= #\rubout ch) (setf (editor-cmd edit)
                                  (cdr (editor-cmd edit)))
                            (setf (editor-cmdcur edit)
                                  (1- (editor-cmdcur edit)))
                            (set-dirty edit :cmd))
       ((char= #\return ch)
        (do-command edit)
        (setf (editor-cmdcur edit) nil)
        (set-mode edit :normal)
        (set-dirty edit :cmd))
       (t (setf (editor-cmd edit) (cons ch (editor-cmd edit)))
          (setf (editor-cmdcur edit) (1+ (editor-cmdcur edit)))
          (set-dirty edit :cmd))))
    (otherwise (jim.bindings:do-keypress ch))))

(defun do-command (edit)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (editor-cmd edit))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
