(defpackage :jim.bindings.trie
  (:use :cl)
  (:export
    make-trie
    trie-insert
    trie-p
    trie-inc
    trie-ref))

(in-package :jim.bindings.trie)

(defstruct trie
  vec)

(defun trie-insert (trie chars value)
  "inserts value into trie destructively"
  (setf (trie-vec trie)
    (inner-trie-insert (trie-vec trie) chars value))
  trie)


(defun inner-trie-insert (node chars value)
  (cond
    ((null chars) value)
    ((null node)
       (inner-trie-insert
         (make-array 256 :initial-element nil)
         chars value))
    ((consp node)
     (cons (inner-trie-insert (car node) chars value) (cdr node)))
    ((equal '* (car chars))
     (unless (null (cdr chars))
       (error "wildcards can only ocur at the end of a binding"))
     (cons node value))
    (t (setf (elt node (char-code (car chars)))
             (inner-trie-insert
               (elt node (char-code (car chars)))
               (cdr chars) value))
       node)))


(defun trie-inc (trie ch)
  "returs either a terminal value or a trie one level down from the previous"
  (cond
    ((vectorp (trie-vec trie))
     (let ((node (elt (trie-vec trie) (char-code ch))))
       (if (or (vectorp node) (consp node))
         (make-trie :vec node)
         node)))
    ((consp (trie-vec trie))
     (or (trie-inc (make-trie :vec (car (trie-vec trie))) ch)
         (cdr (trie-vec trie))))
    (t (trie-vec trie))))

(defun trie-ref (trie chars)
  "returns the element contained in the trie for chars"
  (cond
    ((null chars) (unless (trie-p trie) trie))
    ((trie-p trie) (trie-ref (trie-inc trie (car chars)) (cdr chars)))
    (t nil)))
