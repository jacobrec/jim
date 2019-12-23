(defpackage :jim-app
  (:use :common-lisp :jim-utils :jim-editor :sb-ext)
  (:export run-app))
(in-package :jim-app)


(defun run-app ()
  (stty '("-echo"))
  (stty '("raw"))
  (jim-utils:command "?1049" #\h)
  (jim-user-util:load-jimrc)
  (let ((jim.api:*editor* (new-editor (uiop:command-line-arguments))))
    (vim:use-vim-bindings)
    (loop while (jim.api:is-running) do
      (jim-io:draw-screen jim.api:*editor*)
      (let ((bind:*key-bindings* (or (editor-keybindings jim.api:*editor*)
                                     bind:*key-bindings*)))
        (do-input jim.api:*editor* (read-char)))))
  (jim-utils:command "?1049" #\l)
  (stty '("sane")))

(defun do-input (edit ch)
  (set-dirty edit :status)
    (jim.bindings:do-keypress ch))
