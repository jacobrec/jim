(defpackage :jim.bindings.bindings
  (:use :cl :jim.bindings.keys :jim.bindings.trie)
  (:export
    *key-bindings*
    *key-state*
    *last-key*
    set-key-bindings
    fbind
    unbind
    do-keypress
    bind))

(in-package :jim.bindings.bindings)

(defvar *key-bindings* (make-trie))
(defvar *key-state* *key-bindings*)
(defvar *last-key*)

(defun set-key-bindings (bindings)
  (setf *key-bindings* bindings)
  (setf *key-state* *key-bindings*))

(defun fbind (seq fun)
  "binds seq to apply fun when pressed"
  (trie-insert *key-bindings* seq fun))

(defun unbind (seq)
  "unbinds seq"
  (trie-insert *key-bindings* seq nil))

(defmacro bind ((&rest keys) &rest body)
  "like fbind ex: (bind (<CR> <F2>) (write 'hi) (terpri))"
  `(fbind (kseq ,@keys)
          (lambda () ,@body)))

(defun do-keypress (ch)
  "execute a single character of a keysequence"
  (let ((next-state (trie-inc *key-state* ch)))
    (cond
      ((null next-state) (setf *key-state* *key-bindings*))
      ((functionp next-state)
       (let ((*last-key* ch)) (funcall next-state))
       (setf *key-state* *key-bindings*))
      (t (setf *key-state* next-state)))))
