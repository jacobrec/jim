;;;; editor operations on immutable ropes
;;;; author: Peter Elliott
;;;; licence: LGPL-v3

(defpackage :jbuffer-editor
  (:nicknames jbedit)
  (:use :cl)
  (:export
    #:commit-scratch
    #:begin-scratch
    #:make-buffer
    #:buffer-contents
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
  stack       ; the stack of buffer snapshots (terminated when car is not list)
  redo        ; the stack of undone snapshots
  dirty       ; whether or not the buffer has been updated
  scratch     ; the scratch buffer
  scratch-idx ; the index the scratch buffer starts at
  fname)      ; the file the buffer is associated with

(defun merge-scratch (rope scratch idx)
  (if (or (null scratch) (zerop (length scratch)))
      rope
      (let* ((ropes (jbrope:split rope idx))
             (left (car ropes))
             (right (cdr ropes)))
        (jbrope:concat left (jbrope:str-to-rope scratch) right))))

(defun commit-scratch (buffer)
  (if (buffer-scratch buffer)
    (make-buffer
     :stack (cons (merge-scratch (buffer-head buff)
                                 (buffer-scratch buff)
                                 (buffer-scratch-idx buff))
                  (buffer-stack buff))
     :redo nil
     :dirty t
     :scratch nil
     :scratch-idx nil
     :fname (buffer-fname buff))
    buffer))

(defun begin-scratch (buff idx)
  (make-buffer
   :stack (buffer-stack buff)
   :redo  (buffer-redo buff)
   :dirty (buffer-dirty dirty)
   :scratch ""
   :scratch-idx idx
   :fname (buffer-fname buff)))


(defun buffer-head (buf)
  (let ((stack (buffer-stack buf)))
    (if (listp stack)
      (car stack)
      stack)))

(defun buffer-contents (buff)
  (merge-scratch (buffer-head buff)
                 (buffer-scratch buff)
                 (buffer-scratch-idx buff)))

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
    :scratch nil
    :scratch-idx nil
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
      :scratch (buffer-scratch buff)
      :scratch-idx (buffer-scratch-idx buff)
      :fname (buffer-fname buff))))


(defun insert-coord (buff str lines &optional (cols 0))
  (insert buff str (jbrope:coord-to-idx lines cols)))

(defun insert-scratch (buff str)
  (make-buffer
   :stack (buffer-stack buff)
   :redo  (buffer-redo buff)
   :dirty t
   :scratch (concatenate 'string (buffer-scratch buff) str)
   :scratch-idx (+ (buffer-scratch-idx buff) (length str))
   :fname (buffer-fname buff)))

(defun insert (buff str i)
  (if (equal i (buffer-scratch-idx buff))
      (insert-scratch buff str)
      (progn
        (setq buff (commit-scratch buff))
        (make-buffer
         :stack (cons
                 (jbrope:insert
                  (buffer-head buff)
                  (jbrope:str-to-rope str)
                  i)
                 (buffer-stack buff))
         :redo nil
         :dirty t
         :fname (buffer-fname buff)))))


(defun del-from-coord (buff s-line e-line &optional (s-cols 0) (e-cols 0))
  (setq buff (commit-scratch buff))
  (del-from buff
            (jbrope:coord-to-idx s-line s-cols)
            (jbrope:coord-to-idx e-line e-cols)))


(defun del-from (buff start end)
  (setq buff (commit-scratch buff))
  (make-buffer
    :stack (cons
             (jbrope:del-from (buffer-head buff) start end)
             (buffer-stack buff))
    :redo nil
    :dirty t
    :fname (buffer-fname buff)))


(defun del-coord-n (buff sline scol n)
  (setq buff (commit-scratch buff))
  (let ((i (jbrope:coord-to-idx sline scol)))
    (if (> n 0)
      (del-from buff i (+ i n))
      (del-from buff (+ i n) i))))


(defun undo (buff)
  (setq buff (commit-scratch buff))
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
