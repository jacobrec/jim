(defpackage :jim-app
  (:use :common-lisp :jim-utils :jim-editor :sb-ext)
  (:export run-app))
(in-package :jim-app)


(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (jim-utils:command "?1049" #\h)
  (vim:use-vim-bindings)
  (jim-user-util:load-jimrc)
  (let ((jim.api:*editor* (new-editor (uiop:command-line-arguments))))
    (loop while (jim.api:is-running) do
      (jim-io:draw-screen jim.api:*editor*)
      (do-input jim.api:*editor* (read-char))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (edit ch)
  (set-dirty edit :status)
    (jim.bindings:do-keypress ch))
