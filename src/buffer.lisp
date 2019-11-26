(defpackage :jim-buffer
  (:use :common-lisp)
  (:export buffer-new
           buffer-set))

(defun buffer-new (filename)
  (jbedit:open-buff filename))

(defun buffer-set (buffer data)
  data)


