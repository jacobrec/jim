;;;; jim's default lisp bindings
(defpackage :jim.vim
  (:nicknames vim)
  (:use :cl :jim.bindings)
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
  (jim-editor:set-mode jim-editor:*editor* :normal))

(defun insert-mode ()
  "enter insert mode"
  (setf *key-bindings* *insert-bindings*)
  (jim-editor:set-mode jim-editor:*editor* :insert))

(defun command-mode ()
  "enter command mode"
  (setf (jim-editor:editor-cmd jim-editor:*editor*) nil)
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
  (jim-editor:move-cursor-col jim-editor:*editor* -1))

(bind-normal ("j")
  (jim-editor:move-cursor-row jim-editor:*editor* 1))

(bind-normal ("k")
  (jim-editor:move-cursor-row jim-editor:*editor* -1))

(bind-normal ("l")
  (jim-editor:move-cursor-col jim-editor:*editor* 1))

(bind-normal ("0")
  (jim-editor:move-cursor-col jim-editor:*editor* -1000000))

(bind-normal ("$")
  (jim-editor:move-cursor-col jim-editor:*editor* 1000000))

(bind-normal ("x")
  (jim-editor:delete-from jim-editor:*editor* (jim-editor:editor-index jim-editor:*editor*)
               (1+ (jim-editor:editor-index jim-editor:*editor*)))
  (jim-editor:set-dirty jim-editor:*editor* :buffer))

(bind-normal ("u")
  (jim-editor:undo jim-editor:*editor* 1)
  (jim-editor:set-dirty jim-editor:*editor* :buffer))

;;; insert mode bindings
(bind-insert (<C-c>)
  (normal-mode))

(bind-insert ('*)
  (when (char>= *last-key* #\ )
    (jim-editor:add-char jim-editor:*editor* *last-key*)))

(bind-insert (#\rubout)
  (jim-editor:backspace jim-editor:*editor*))

(bind-insert (#\return)
  (jim-editor:add-char jim-editor:*editor* #\newline))
