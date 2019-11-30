;;;; immutable strings with O(1) substrings
;;;; author: Peter Elliott
;;;; licence: LGPL-v3

(defpackage :jbuffer-istring
  (:nicknames jbstring)
  (:use :cl)
  (:export
    #:make-istring
    #:istring-p))

(in-package :jbuffer-istring)


(defclass istring (sequence standard-object)
  ((base  :accessor istring-base
          :initarg  :base)
   (start :accessor istring-start
          :initarg  :start)
   (end   :accessor istring-end
          :initarg  :end)))


(defun make-istring (str)
  (make-instance 'istring
                 :base  str
                 :start 0
                 :end   (length str)))

(defmethod istring-p ((x istring))
  t)
(defmethod istring-p ((x t))
  nil)



(defmethod sb-sequence:elt ((seq istring) index)
  (elt (istring-base seq) (+ (istring-start seq) index)))


(defmethod sb-sequence:length ((seq istring))
  (- (istring-end seq) (istring-start seq)))


(defmethod sb-sequence:subseq ((seq istring) start &optional end)
  (make-instance 'istring
                 :base  (istring-base seq)
                 :start (+ start (istring-start seq))
                 :end   (if end
                          (+ end (istring-start seq))
                          (istring-end seq))))


(defmethod print-object ((obj istring) stream)
  (cond
    (*print-escape* (progn
                      (princ #\" stream)
                      (istring-princ obj stream)
                      (princ #\" stream)))
    (t (istring-princ obj stream))))


(defun istring-princ (istr stream)
  (map nil (lambda (ch) (princ ch stream)) istr))

