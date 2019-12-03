;;;; jim's default lisp bindings
(defpackage :jim.vim
  (:nicknames vim)
  (:use :cl :jim.bindings :jim.api)
  (:export
    use-vim-bindings
    normal-mode
    insert-mode
    bind-normal
    bind-insert))


(in-package :jim.vim)

(defvar *normal-bindings* (make-trie))
(defvar *insert-bindings* (make-trie))
(defvar *commmand-bindings* (make-trie))

(defun use-vim-bindings ()
  "sets the global editor bindings to the vim ones"
  (setf *key-bindings* *normal-bindings*))

(defun normal-mode ()
  "enter normal mode"
  (setf *key-bindings* *normal-bindings*)
  (set-mode :normal))

(defun insert-mode ()
  "enter insert mode"
  (setf *key-bindings* *insert-bindings*)
  (set-mode :insert))

(defun command-mode ()
  "enter command mode"
  (setf (jim-editor:editor-cmd *editor*) nil) ;TODO: add to api
  (set-mode :cmd))

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

(bind-normal ("I")
  (cursor->line-start)
  (insert-mode))

(bind-normal ("a")
  (cursor-right)
  (insert-mode))

(bind-normal ("A")
  (cursor->line-end)
  (insert-mode))

(bind-normal ("h")
  (cursor-left))

(bind-normal ("j")
  (cursor-down))

(bind-normal ("k")
  (cursor-up))

(bind-normal ("l")
  (cursor-right))

(bind-normal ("0")
  (cursor->line-start))

(bind-normal ("$")
  (cursor->line-end))

(bind-normal ("x")
  (del))

(bind-normal ("u")
  (undo))

;;; insert mode bindings
(bind-insert (<C-c>)
  (normal-mode))

(bind-insert ('*)
  (when (char>= *last-key* #\ )
    (insert-char *last-key*)))

(bind-insert (#\rubout)
  (backspace))

(bind-insert (#\return)
  (enter))
