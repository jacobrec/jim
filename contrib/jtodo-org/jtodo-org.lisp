(defpackage :jtodo-org
  (:use :cl :jim.api :jim.bindings :jtodo)
  (:export
   ))

(in-package :jtodo-org)

(defun redraw-list ()
  (set-content (with-output-to-string (out)
		 (let ((*standard-output* out))
		   (display-todo *todo-list*)))))

(defun jtodo-set-cur ()
  (cursor-to 6 (1+ *todo-item*)))

(vim:defcmd :jtodo ()
  (jtodo-mode))

(defvar *jtodo-bindings* (make-trie))

(defvar *todo-list*)
(defvar *todo-item*)

(defun jtodo-mode ()
  "configures jtodo mode"
  (set-key-bindings *jtodo-bindings*)
  (setf *todo-list* (file-read-list *default-list))
  (setf *todo-item* 0)
  (set-mode :jtodo)
  (redraw-list)
  (jtodo-set-cur))

(defmacro bind-jtodo ((&rest keys) &rest body)
  `(let ((*key-bindings* *jtodo-bindings*))
     (bind (,@keys) ,@body)))

(bind-jtodo ("j")
  (setf *todo-item* (min (1+ *todo-item*)
                         (1- (length (cdr *todo-list*)))))
  (jtodo-set-cur))

(bind-jtodo ("k")
  (setf *todo-item* (max (1- *todo-item*) 0))
  (jtodo-set-cur))

(bind-jtodo (#\return)
  (setf *todo-list* (list-toggle *todo-list* (1+ *todo-item*)))
  (redraw-list))


(bind-jtodo (":")
  (vim:command-mode))
