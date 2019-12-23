;;;; jim's default lisp bindings
(defpackage :jim.vim
  (:nicknames vim)
  (:use :cl :jim.bindings :jim.api)
  (:export
    use-vim-bindings
    normal-mode
    insert-mode
    command-mode
    bind-normal
    bind-insert
    defcmd
    reexport))

(in-package :jim.vim)

(defvar *normal-bindings* (make-trie))
(defvar *insert-bindings* (make-trie))

(defun use-vim-bindings ()
  "sets the global editor bindings to the vim ones"
  (set-key-bindings *normal-bindings*))

(defun normal-mode ()
  "enter normal mode"
  (commit-scratch)
  (set-bindings *normal-bindings*)
  (cursor-style :default)
  (set-mode :normal))

(defun insert-mode ()
  "enter insert mode"
  (begin-scratch)
  (set-bindings *insert-bindings*)
  (cursor-style :i-beam)
  (set-mode :insert))

(defun command-mode ()
  "enter command mode"
  (commit-scratch)
  (let ((old-mode (mode))
        (buff (current-buffer)))
    (set-mode :cmd)
    (prompt ":"
        (lambda (command)
          (eval (read-from-string
                 (concatenate 'string "( vim-cmd:" command ")")))
          (when (equal (mode buff) :cmd)
            (set-mode old-mode buff)))
        (lambda ()
          (when (equal (mode buff) :cmd)
            (set-mode old-mode buff))))))

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
  (insert-mode)
  (cursor-right))

(bind-normal ("A")
  (insert-mode)
  (cursor->line-end))

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

(bind-normal ("gg")
  (cursor-to 0 0))

(bind-normal ("G")
  (cursor-to 0 999999999))

(bind-normal ("r" '*)
  (del)
  (insert-char *last-key*)
  (cursor-left))

(bind-normal (<Left>)
  (cursor-left))

(bind-normal (<Down>)
  (cursor-down))

(bind-normal (<Up>)
  (cursor-up))

(bind-normal (<Right>)
  (cursor-right))

(bind-normal ("gt")
  (next-buffer))

(bind-normal ("gT")
  (previous-buffer))

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

;;; vim commands

(defpackage :vim-cmd
  (:use :cl :vim :jim.api))

(defmacro defcmd (name &rest args)
  `(let ((old-pkg *package*)
         (old-rtable *readtable*))
     (in-package :vim-cmd)
     (defun ,(intern (symbol-name name) :vim-cmd) ,@args)
     (export ',(intern (symbol-name name) :vim-cmd))
     (setf *package* old-pkg)
     (setf *readtable* old-rtable)))

(defmacro reexport (sym)
  `(let ((old-pkg *package*)
         (old-rtable *readtable*))
     (in-package :vim-cmd)
     (import ',sym)
     (export ',sym)
     (setf *package* old-pkg)
     (setf *readtable* old-rtable)))

(defun quit-buff (&optional (buff (current-buffer)))
  (if (> (num-buffers) 1)
      (close-buffer buff)
      (exit-jim)))

(defun quit-all ()
  (cond
    ((buffer-dirty)
     (set-cmd "no write since last change (add ! to override)"))
    ((<= (num-buffers) 1) (exit-jim))
    (t (close-buffer)
       (quit-all))))

(defcmd q ()
  (if (buffer-dirty)
      (set-cmd "no write since last change (add ! to override)")
      (quit-buff)))

(defcmd q! ()
  (quit-buff))

(defcmd qa ()
  (quit-all))

(defcmd qa! ()
  (exit-jim))

(defcmd w (&optional name)
  (write-buffer :name name))

(defcmd wq ()
  (write-buffer)
  (quit-buff))

(defcmd wqa ()
  (map nil
       (lambda (buff)
         (write-buffer :buff buff))
       (buffers))
  (exit-jim))

(defcmd e (name)
  (new-buffer name))

(reexport set-param)
(reexport set-default-param)
