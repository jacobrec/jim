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
  (new-buffer)
  (goto-buffer (1- (num-buffers))) ;TODO fix goto-buffer
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
  (file-write-list *todo-list*)
  (redraw-list))

(bind-jtodo ("n")
  (prompt "new item: "
          (lambda (item)
            (setf *todo-list* (list-add *todo-list* item))
            (file-write-list *todo-list*)
            (redraw-list))
          (lambda () t)))

(bind-jtodo ("c")
  (setf *todo-list* (list-clear-done *todo-list*))
  (file-write-list *todo-list*)
  (redraw-list))
 
(bind-jtodo (":")
  (vim:command-mode))
