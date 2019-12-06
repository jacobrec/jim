;;;; editor operations on immutable ropes
;;;; author: Peter Elliott
;;;; licence: LGPL-v3

(defpackage :jbuffer-editor
  (:nicknames jbedit)
  (:use :cl)
  (:export
    #:make-buffer
    #:buffer-head
    #:open-buff
    #:write-buff
    #:buffer-dirty
    #:insert-coord
    #:insert
    #:del-from-coord
    #:del-from
    #:del-coord-n
    #:undo
    #:redo
    #:buffer
    #:buffer-fname))


(in-package :jbuffer-editor)


(defstruct buffer
  stack  ; the stack of buffer snapshots (terminated when car is not list)
  redo   ; the stack of undone snapshots
  dirty  ; whether or not the buffer has been updated
  fname) ; the file the buffer is associated with


(defun buffer-head (buf)
  (let ((stack (buffer-stack buf)))
    (if (listp stack)
      (car stack)
      stack)))


(defun fname-to-string (fname)
  (with-open-file (strm fname
                     :if-does-not-exist nil)
    (if strm
      (let ((contents (make-string (file-length strm))))
        (read-sequence contents strm)
        contents)
      "")))


(defun open-buff (fname)
  (make-buffer
    :stack (jbrope:str-to-rope (fname-to-string fname))
    :redo nil
    :dirty nil
    :fname fname))


(defun write-buff (buff &optional fname)
  (let ((dirty (not (null fname))))
    (if (null fname) (setf fname (buffer-fname buff)))

    (with-open-file (f fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (mapcar
        (lambda (chunk)
          (princ chunk f))
        (jbrope:chunks (buffer-head buff))))

    (make-buffer
      :stack (buffer-stack buff)
      :redo  (buffer-redo buff)
      :dirty dirty
      :fname (buffer-fname buff))))


(defun insert-coord (buff str lines &optional (cols 0))
  (insert buff str (jbrope:coord-to-idx lines cols)))


(defun insert (buff str i)
  (make-buffer
    :stack (cons
             (jbrope:insert
               (buffer-head buff)
               (jbrope:str-to-rope str)
               i)
             (buffer-stack buff))
    :redo nil
    :dirty t
    :fname (buffer-fname buff)))


(defun del-from-coord (buff s-line e-line &optional (s-cols 0) (e-cols 0))
  (del-from buff
            (jbrope:coord-to-idx s-line s-cols)
            (jbrope:coord-to-idx e-line e-cols)))


(defun del-from (buff start end)
  (make-buffer
    :stack (cons
             (jbrope:del-from (buffer-head buff) start end)
             (buffer-stack buff))
    :redo nil
    :dirty t
    :fname (buffer-fname buff)))


(defun del-coord-n (buff sline scol n)
  (let ((i (jbrope:coord-to-idx sline scol)))
    (if (> n 0)
      (del-from buff i (+ i n))
      (del-from buff (+ i n) i))))


(defun undo (buff)
  (if (listp (buffer-stack buff))
    (make-buffer
      :stack (cdr (buffer-stack buff))
      :redo  (cons
               (car (buffer-stack buff))
               (buffer-redo buff))
      :dirty t  ;TODO: attach dirtyness to snapshot
      :fname (buffer-fname buff))
    buff))


(defun redo (buff)
  (if (buffer-redo buff)
    (make-buffer
      :stack (cons
               (car (buffer-redo buff))
               (buffer-stack buff))
      :redo  (cdr (buffer-redo buff))
      :dirty t ;TODO: attatch dirtyness to snapshot
      :fname (buffer-fname buff))
    buff))
