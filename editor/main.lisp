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
  (let ((*editor* (new-editor nil)))
    (loop while *running* do
      (jim-io:draw-screen *editor*)
      (do-input *editor* (read-char))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (edit ch)
  (set-dirty edit :status)
  (case (editor-mode edit)
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
    (otherwise (jim.bindings:do-keypress ch))))

(defun do-command (edit)
  (let ((cmd (string-trim '(#\space #\return #\linefeed)
                          (concatenate
                            'string
                            (reverse (editor-cmd edit))))))
    (cond
      ((string= "q" cmd) (setf *running* nil)))))
