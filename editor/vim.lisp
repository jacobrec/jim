;;;; jim's default lisp bindings
(defpackage :jim.vim
  (:nicknames vim)
  (:use :cl :jim.bindings)
  (:export
    use-vim-bindings
    normal-mode
    insert-mode
    bind-normal
    bind-insert
    ))

(in-package :jim.vim)

(defvar *normal-bindings* (make-trie))
(defvar *insert-bindings* (make-trie))
(defvar *commmand-bindings* (make-trie))

(defun use-vim-bindings ()
  "sets the global editor bindings to the vim ones"
  (setf *key-bindings* *normal-bindings*))

(defun normal-mode ()
  "enter normal mode"
  (jim-editor:set-mode jim-editor:*editor* :normal))

(defun insert-mode ()
  "enter insert mode"
  (jim-editor:set-mode jim-editor:*editor* :insert))

(defun command-mode ()
  "enter insert mode"
  (jim-editor:set-mode jim-editor:*editor* :cmd))


(defmacro bind-normal ((&rest keys) &rest body)
  `(let ((*key-bindings* *normal-bindings*))
    (bind (,@keys) ,@body)))

(defmacro bind-insert ((&rest keys) &rest body)
  `(let ((*key-bindings* *insert-bindings*))
    (bind (,@keys) ,@body)))

;;; normal mode bindings
(bind-normal (":")
  (command-mode))

(bind-normal ("i")
  (insert-mode))

(bind-normal ("h")
  (jim-editor:move-cursor jim-editor:*editor* 0 -1))

(bind-normal ("j")
  (jim-editor:move-cursor jim-editor:*editor* 1 0))

(bind-normal ("k")
  (jim-editor:move-cursor jim-editor:*editor* -1 0))

(bind-normal ("l")
  (jim-editor:move-cursor jim-editor:*editor* 0 1))

(bind-normal ("0")
  (jim-editor:move-cursor jim-editor:*editor* 0 -1000000))

(bind-normal ("$")
  (jim-editor:move-cursor jim-editor:*editor* 0 1000000))

(bind-normal ("x")
  (jim-editor:delete-from jim-editor:*editor* (jim-editor:editor-index jim-editor:*editor*)
               (1+ (jim-editor:editor-index jim-editor:*editor*)))
  (set-dirty edit :buffer))

(bind-normal ("u")
  (undo jim-editor:*editor* 1)
  (set-dirty jim-editor:*editor* :buffer))

;;; insert mode bindings
(bind-insert (<C-c>)
  (normal-mode))

(bind-insert ('*)
  (jim-editor:add-char jim-editor:*editor* *last-key*))

(bind-insert (#\rubout)
  (jim-editor:backspace jim-editor:*editor*))

(bind-insert (#\return)
  (jim-editor:add-char jim-editor:*editor* #\newline))
