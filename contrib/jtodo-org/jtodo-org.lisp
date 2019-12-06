(defpackage :jtodo-org
  (:use :cl :jim.api :jim.bindings :jtodo)
  (:export
   ))

(in-package :jtodo-org)

(defun redraw-list ()
  (set-content (with-output-to-string (out)
		 (let ((*standard-output* out))
		   (display-todo *todo-list*)))))

(vim:defcmd :jtodo ()
  (jtodo-mode))

(defvar *jtodo-bindings* (make-trie))

(defvar *todo-list*)

(defun jtodo-mode ()
  "configures jtodo mode"
  ;(set-key-bindings *jtodo-bindings*)
  (setf *todo-list* (file-read-list *default-list))
  (set-mode :jtodo)
  (redraw-list))

(defmacro bind-jtodo ((&rest keys) &rest body)
  `(let ((*key-bindings* *jtodo-bindings*))
     (bind (,@keys) ,@body)))
