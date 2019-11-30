;;;; immutable rope data structure
;;;; author: Peter Elliott
;;;; licence: LGPL-v3

(defpackage :jbuffer-rope
  (:nicknames jbrope)
  (:use :cl)
  (:export
    #:str-to-rope
    #:rope-to-string
    #:print-rope
    #:coord-to-idx
    #:idx-to-coord
    #:rope-ref
    #:line-ref
    #:split
    #:rope-len
    #:rope-lines
    #:concat
    #:insert
    #:del-from
    #:chunks))

(in-package :jbuffer-rope)

(defun find-lines (str)
  (coerce (find-lines-list str) 'vector))


(defun find-lines-list (str &optional (i 0))
  (cond
    ((= i (- (length str) 1)) nil)
    ((equal (elt str i) #\newline)
     (cons (+ i 1) (find-lines-list str (+ i 1))))
    (t (find-lines-list str (+ i 1)))))


(defstruct rope
  l-len   ; length of left branch
  left    ; left branch
  right)   ; right branch


(defun str-to-rope (str)
  (jbstring:make-istring str))

(defun rope-to-string (rope)
  (concatenate 'string (rope-to-string-internal rope)))

(defun rope-to-string-internal (rope)
  (if (jbstring:istring-p rope) rope
      (format nil "~a~a" (rope-to-string-internal (rope-left rope))
                         (rope-to-string-internal (rope-right rope)))))


;;;;; TODO: write coord-to-idx and idx-to-coord
(defun coord-to-idx (rope line col)
  0)
(defun idx-to-coord (rope idx)
  '(0 . 0))


(defun rope-ref (rope i)
  "gets the char in the rope at index i"
  (cond
    ((jbstring:istring-p rope) (aref rope i))
    ((< i (rope-l-len rope)) (rope-ref (rope-left rope) i))
    (t (rope-ref (rope-right rope) (- i (rope-l-len rope))))))

(defun split (rope i)
  "splits a rope at i into a list of two ropes"
  (cond
    ((jbstring:istring-p rope) (list (subseq rope 0 i) (subseq rope i)))
    ((= i (rope-l-len rope)) (list (rope-left rope) (rope-right rope)))
    ((< i (rope-l-len rope))
     (let ((left (split (rope-left rope) i)))
       (list (first left) (concat (second left) (rope-right rope)))))
    ((> i (rope-l-len rope))
     (let ((right (split (rope-right rope) (- i (rope-l-len rope)))))
       (list (concat (rope-left rope) (first right)) (second right))))))

(defun bsearch (seq i &optional (lo 0) hi)
  (let ((len (length seq)))
    (if (null hi) (setf hi len))

    (let ((mid (+ lo (floor (- hi lo) 2))))
      (cond
        ((> lo hi) lo)
        ((>= mid len) (bsearch seq i lo (- mid 1)))
        ((= (elt seq mid) i) mid)
        ((> (elt seq mid) i) (bsearch seq i lo (- mid 1)))
        ((< (elt seq mid) i) (bsearch seq i (+ mid 1) hi))))))

(defun rope-len (rope)
  "gets the length of the rope"
  (if (jbstring:istring-p rope)
    (length rope)
    (+ (rope-l-len rope)
       (rope-len (rope-right rope)))))

(defun empty-leaf-p (rope)
  (and (jbstring:istring-p rope)
       (= (length rope) 0)))


(defun concat (rope1 &rest ropes)
  "concatenates two ropes"
  (cond
    ((not ropes) rope1)
    ((empty-leaf-p rope1) (apply #'concat ropes))
    (t (apply #'concat
              (make-rope :l-len (rope-len rope1)
                        :left rope1
                        :right (car ropes))
              (cdr ropes)))))


(defun insert (rope str i)
  "returns a rope with str inserted into rope at i"
  (let ((ropes (split rope i)))
    (concat (first ropes) str (second ropes))))


(defun del-from (rope start end)
  "delete a subsequence from a rope"
  (concat
    (first (split rope start))
    (second (split rope end))))


(defun chunks (rope)
  "linearizes the rope into a list"
  (if (rope-p rope)
    (append
      (chunks (rope-l rope))
      (chunks (rope-r rope)))
    (list rope)))
