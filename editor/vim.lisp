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
(defvar *command-bindings* (make-trie))

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

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun command-mode ()
  "enter command mode"
  (set-cmd (make-adjustable-string ":"))
  (set-cmd-cur 1)
  (setf *key-bindings* *command-bindings*)
  (set-mode :cmd))

(defmacro bind-normal ((&rest keys) &rest body)
  `(let ((*key-bindings* *normal-bindings*))
    (bind (,@keys) ,@body)))

(defmacro bind-insert ((&rest keys) &rest body)
  `(let ((*key-bindings* *insert-bindings*))
    (bind (,@keys) ,@body)))

(defmacro bind-command ((&rest keys) &rest body)
  `(let ((*key-bindings* *command-bindings*))
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

(vim:bind-normal ("r" '*)
  (del)
  (insert-char *last-key*)
  (cursor-left))

(vim:bind-normal (<Left>)
  (cursor-left))

(vim:bind-normal (<Down>)
  (cursor-down))

(vim:bind-normal (<Up>)
  (cursor-up))

(vim:bind-normal (<Right>)
  (cursor-right))

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

(vim:bind-insert (<Left>)
  (cursor-left))

(vim:bind-insert (<Down>)
  (cursor-down))

(vim:bind-insert (<Up>)
  (cursor-up))

(vim:bind-insert (<Right>)
  (cursor-right))

;;; command mode bindings
(bind-command ('*)
  (when (char>= *last-key* #\ )
    (vector-push-extend *last-key* (cmd))
    (flush-cmd)
    (set-cmd-cur (1+ (cmd-cur)))))

(bind-command (<C-c>)
  (set-cmd-cur nil)
  (set-cmd "")
  (normal-mode))

(bind-command (#\rubout)
  (when (> (length (cmd)) 1)
    (vector-pop (cmd))
    (flush-cmd)
    (set-cmd-cur (1- (cmd-cur)))))

(bind-command (#\return)
  (exit-jim))
