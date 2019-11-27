;;;; immutable rope data structure
;;;; author: Peter Elliott
;;;; licence: LGPL-v3

(defpackage :jbuffer-rope
  (:nicknames jbrope)
  (:use :cl)
  (:export
    #:str-to-rope
    #:print-rope
    #:coord-to-idx
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


(defstruct leaf
  str   ; the sequence the leaf is based on
  lvec) ; the vector of newlines


(defun find-lines (str)
  (coerce (find-lines-list str) 'vector))


(defun find-lines-list (str &optional (i -1))
  (cond
    ((= i (- (length str) 1)) nil)
    ((= i -1) (cons 0 (find-lines-list str (+ i 1))))
    ((equal (elt str i) #\newline)
     (cons (+ i 1) (find-lines-list str (+ i 1))))
    (t (find-lines-list str (+ i 1)))))


(defstruct rope
  nl   ; length of left branch
  nnl  ; number of newlines in left branch
  l    ; left branch
  r)   ; right branch


(defun str-to-rope (str)
  (make-leaf
    :str (jbstring:make-istring str)
    :lvec (find-lines str)))


(defun print-rope (rope)
  "prints a rope via princ"
  (if (leaf-p rope)
    (princ (leaf-str rope))
    (progn
      (print-rope (rope-l rope))
      (print-rope (rope-r rope)))))


(defun coord-to-idx (rope lines &optional (cols 0))
  "turns a lines/cols index into a linear one"
  (min
    (+ (line-ref rope lines) cols)
    (1- (line-ref rope (+ lines 1)))))


(defun rope-ref (rope i)
  "gets the char in the rope at index i"
  (cond
    ((leaf-p rope) (aref (leaf-str rope) i))
    ((< i (rope-nl rope)) (rope-ref (rope-l rope) i))
    (t (rope-ref (rope-r rope) (- i (rope-nl rope))))))


(defun line-ref (rope i)
  "gets the index of the line at i"
  (cond
    ((leaf-p rope)
     (if (< i (length (leaf-lvec rope)))
       (aref (leaf-lvec rope) i)
       (rope-len rope)))
    ((< i (rope-nnl rope)) (line-ref (rope-l rope) i))
    (t (line-ref (rope-r rope) (- i (rope-nnl rope))))))


(defun split (rope i)
  "splits a rope at i into a list of two ropes"
  (cond
    ((leaf-p rope) (split-leaf rope i))
    ((= i (rope-nl rope)) (list (rope-l rope) (rope-r rope)))
    ((< i (rope-nl rope))
     (let ((left (split (rope-l rope) i)))
       (list (first left) (concat (second left) (rope-r rope)))))
    ((> i (rope-nl rope))
     (let ((right (split (rope-r rope) (- i (rope-nl rope)))))
       (list (concat (rope-l rope) (first right)) (second right))))))


(defun split-leaf (leaf i)
  "splits a string in to two strings at i"
  (let ((nl-split (bsearch (leaf-lvec leaf) i)))
    (list
      (make-leaf
        :str (subseq (leaf-str leaf) 0 i)
        :lvec (subseq (leaf-lvec leaf) 0 nl-split))
      (make-leaf
        :str (subseq (leaf-str leaf) i)
        :lvec (vsub (subseq (leaf-lvec leaf) nl-split) i)))))


(defun vsub (vec i)
  (map 'vector
    (lambda (e)
      (- e i))
    vec))


(defun bsearch (seq i &optional (lo 0) hi)
  (if (null hi) (setf hi (length seq)))

  (let ((mid (+ lo (floor (- hi lo) 2))))
    (cond
      ((> lo hi) lo)
      ((= (elt seq mid) i) mid)
      ((> (elt seq mid) i) (bsearch seq i lo (- mid 1)))
      ((< (elt seq mid) i) (bsearch seq i (+ mid 1) hi)))))


(defun rope-len (rope)
  "gets the length of the rope"
  (if (leaf-p rope)
    (length (leaf-str rope))
    (+
      (rope-nl rope)
      (rope-len (rope-r rope)))))


(defun rope-lines (rope)
  "gets the number of lines in a rope"
  (if (leaf-p rope)
    (length (leaf-lvec rope))
    (+
      (rope-nnl rope)
      (rope-lines (rope-r rope)))))


(defun empty-leaf-p (rope)
  (and
    (leaf-p rope)
    (= (length (leaf-str rope)) 0)))


(defun concat (rope1 rope2)
  "concatenates two ropes"
  (cond
    ((empty-leaf-p rope1) rope2)
    ((empty-leaf-p rope2) rope1)
    (t (make-rope :nl (rope-len rope1)
                  :nnl (rope-lines rope1)
                  :l rope1
                  :r rope2))))


(defun insert (rope str i)
  "returns a rope with str inserted into rope at i"
  (let ((ropes (split rope i)))
      (concat (first ropes)
              (concat str (second ropes)))))


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
    (list (leaf-str rope))))
