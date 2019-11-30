(defpackage rope-test
  (:use :cl :prove :jbrope))
(in-package :rope-test)


(plan nil)

;; Test rope building
(let* ((s (format nil "hello"))
       (rope (jbrope:str-to-rope s)))
  (is (jbrope:rope-to-string rope)
      (format nil "hello") :test #'string=)
  (is (format nil "~a" (car (jbrope::split rope 3)))
      (format nil "~a" (subseq rope 0 3)) :test #'equal)

  (is (jbrope::empty-leaf-p (jbrope:str-to-rope "")) t)
  (is (jbrope::empty-leaf-p (jbrope:str-to-rope "a")) nil)
  (is (jbrope::empty-leaf-p (jbrope:str-to-rope "ah")) nil)

  (is (format nil "~a" (cadr (jbrope::split rope 3)))
      (format nil "~a" (subseq rope 3)) :test #'equal)

  (is (jbrope:rope-to-string (jbrope:insert rope (jbrope:str-to-rope "i") 3))
      "helilo" :test #'string=))
(let* ((s (format nil "Hello World")) (rope (jbrope:str-to-rope s)))
  (is (jbrope:rope-to-string rope) s :test #'string=))

(let* ((s (format nil "Hello~%World")) (rope (jbrope:str-to-rope s)))
  (is (jbrope:rope-to-string rope) s :test #'string=))

(let* ((s (format nil "abcdefghijklmnopqrstuvwxyz~%abcdefghijklmnopqrstuvwxyz")) (rope (jbrope:str-to-rope s)))
  (is (jbrope:rope-to-string rope) s :test #'string=)
  (is (coord-to-idx rope 0 0) 0)
  (is (coord-to-idx rope 0 1) 1)
  (is (coord-to-idx rope 0 5) 5)
  (is (coord-to-idx rope 1 0) 27)
  (is (coord-to-idx rope 1 20) 47)
  (let ((rope2 (jbrope:insert rope (jbrope:str-to-rope "Hi") 15)))
    (is (jbrope:rope-to-string rope2)
        (format nil "abcdefghijklmnoHipqrstuvwxyz~%abcdefghijklmnopqrstuvwxyz") :test #'string=)
    (is (coord-to-idx rope2 0 0) 0)
    (is (coord-to-idx rope2 0 1) 1)
    (is (coord-to-idx rope2 0 5) 5)
    (is (coord-to-idx rope2 1 0) 29)
    (is (coord-to-idx rope2 1 20) 49)))

(let* ((s (format nil "hello~%world~%goodbye"))
       (rope (jbrope:str-to-rope s))
       (rope2 (jbrope:insert rope (jbrope:str-to-rope "1") 3))
       (rope3 (jbrope:insert rope2 (jbrope:str-to-rope "2") 10))
       (rope4 (jbrope:insert rope3 (jbrope:str-to-rope "3") 16)))
  (is (jbrope:rope-to-string rope4)
      (format nil "hel1lo~%wor2ld~%go3odbye") :test #'string=)
  (is (coord-to-idx rope4 0 0) 0)
  (is (coord-to-idx rope4 0 1) 1)
  (is (coord-to-idx rope4 1 0) 7)
  (is (coord-to-idx rope4 1 1) 8)
  (is (coord-to-idx rope4 2 0) 14)
  (is (coord-to-idx rope4 2 1) 15)
  (is (coord-to-idx rope4 0 3) 3)
  (is (jbrope:idx-to-coord rope2 0) '(0 . 0))
  (is (jbrope:idx-to-coord rope2 1) '(0 . 1))
  (is (jbrope:idx-to-coord rope2 2) '(0 . 2))
  (is (jbrope:idx-to-coord rope2 3) '(0 . 3))
  (is (jbrope:idx-to-coord rope4 3) '(0 . 3))
  (is (jbrope:idx-to-coord rope4 4) '(0 . 4))
  (is (jbrope:idx-to-coord rope4 5) '(0 . 5))
  (is (jbrope:idx-to-coord rope4 6) '(0 . 6))
  (is (jbrope:idx-to-coord (jbrope:str-to-rope (format nil "lo~%wor")) 3) '(1 . 0))
  (is (jbrope:idx-to-coord rope4 7) '(1 . 0))
  (is (jbrope:idx-to-coord rope4 10) '(1 . 3))
  (is (jbrope:idx-to-coord rope4 14) '(2 . 0))
  (is (jbrope:idx-to-coord rope4 15) '(2 . 1)))


(let ((test #(1 4 7 9 10 11 100 1000)))
  (is (jbrope::bsearch test 11) 5)
  (is (jbrope::bsearch test 12) 6)
  (is (jbrope::bsearch test 13) 6)
  (is (jbrope::bsearch test 14) 6)
  (is (jbrope::bsearch test 10000) 8)
  (is (jbrope::bsearch #() 100) 0)
  (is (jbrope::bsearch #() 10) 0)
  (is (jbrope::bsearch #() 0) 0))

(let ((rope (jbrope:str-to-rope (format nil "12345~%67890"))))
  (is (jbrope:idx-to-coord rope 0) '(0 . 0))
  (is (jbrope:idx-to-coord rope 1) '(0 . 1))
  (is (jbrope:idx-to-coord rope 3) '(0 . 3))
  (is (jbrope:idx-to-coord rope 6) '(1 . 0))
  (is (jbrope:idx-to-coord rope 7) '(1 . 1))
  (is (jbrope:idx-to-coord rope 8) '(1 . 2))
  (is (jbrope:idx-to-coord rope 10) '(1 . 4)))

(defun idx-idx-test (rope idx)
  (is
    (let ((loc (jbrope:idx-to-coord rope idx)))
      (jbrope:coord-to-idx rope (car loc) (cdr loc)))
    idx))
(defun idx-coord-test (rope idx line col)
  (is (jbrope:idx-to-coord rope idx) (cons line col))
  (is (jbrope:coord-to-idx rope line col) idx))

(let* ((s (format nil "12345~%~%~%67890")) (rope (jbrope:str-to-rope s)))
  (loop for i from 0 to (1- (length s)) do
    (idx-idx-test rope i)))
(let* ((s (format nil "hello~%world~%goodbye"))
       (rope (jbrope:str-to-rope s))
       (rope2 (jbrope:insert rope (jbrope:str-to-rope "1") 3))
       (rope3 (jbrope:insert rope2 (jbrope:str-to-rope "2") 10))
       (rope4 (jbrope:insert rope3 (jbrope:str-to-rope "3") 16))
       (rope5 (jbrope:insert rope4 (jbrope:str-to-rope (format nil "~%")) 0))
       (rope6 (jbrope:insert rope5 (jbrope:str-to-rope (format nil "~%")) 0))
       (rope7 (jbrope:insert rope6 (jbrope:str-to-rope (format nil "~%")) 0)))
  (is (jbrope:rope-to-string rope7)
      (format nil "~%~%~%hel1lo~%wor2ld~%go3odbye") :test #'string=)
  (loop for i from 0 to (1- (rope-len rope4)) do
    (idx-idx-test rope4 i))
  (loop for i from 0 to (1- (rope-len rope7)) do
    (idx-idx-test rope7 i))

  (idx-coord-test rope4 7 1 0)
  (idx-coord-test rope4 10 1 3)
  (idx-coord-test rope4 14 2 0)
  (idx-coord-test rope4 15 2 1)

  (idx-coord-test rope7 0 0 0)
  (idx-coord-test rope7 1 1 0)
  (idx-coord-test rope7 2 2 0)
  (idx-coord-test rope7 3 3 0)
  (idx-coord-test rope7 4 3 1))

(let* ((s (format nil "hello~%"))
       (rope (jbrope:str-to-rope s))
       (rope2 (jbrope:insert rope (jbrope:str-to-rope (format nil "~%")) 0))
       (rope3 (jbrope:insert rope2 (jbrope:str-to-rope (format nil "~%")) 0)))
  (idx-coord-test rope3 0 0 0)
  (idx-coord-test rope3 1 1 0)
  (idx-coord-test rope3 2 2 0))

(let* ((s (format nil "hello"))
       (rope (jbrope:str-to-rope s))
       (rope2 (jbrope:insert rope (jbrope:str-to-rope (format nil "~%")) 5))
       (rope3 (jbrope:insert rope2 (jbrope:str-to-rope (format nil "~%")) 6))
       (rope4 (jbrope:insert rope3 (jbrope:str-to-rope (format nil "~%")) 7)))
  (idx-coord-test rope4 0 0 0)
  (idx-coord-test rope4 1 0 1)
  (idx-coord-test rope4 2 0 2)

  (diag (jbrope:rope-to-string rope4))
  (idx-coord-test rope4 6 1 0)
  (idx-coord-test rope4 7 2 0))


(is (jbrope::find-lines-list (format nil "a~%")) '(1))
(is (jbrope::find-lines-list (format nil "~%~%~%")) '(0 1 2))

(let* ((s (format nil "hello~%world~%goodbye"))
       (rope (jbrope:str-to-rope s))
       (rope2 (jbrope:insert rope (jbrope:str-to-rope "1") 3))
       (rope3 (jbrope:insert rope2 (jbrope:str-to-rope "2") 10))
       (rope4 (jbrope:insert rope3 (jbrope:str-to-rope "3") 16)))
  (let ((a-str (format nil "hel1lo~%wor2ld~%go3odbye")))
    (is (jbrope:rope-to-string rope4) a-str :test #'string=)
    (let ((i 0))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i))))
    (let ((i 3))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i)) i 10))
    (let ((i 8))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i)) i 10))
    (let ((i 12))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i)) i 10))
    (let ((i 12))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i)) i 21))
    (let ((i 3))
      (jbrope::iterate rope4 (lambda (c) (is c (char a-str i)) (incf i)) i 21))
    (let ((i 0) (lines #("hel1lo" "wor2ld" "go3odbye")))
      (jbrope::iterate-lines rope4
        (lambda (c)
          (is (string-trim '(#\newline) c) (elt lines i) :test #'string=)
          (incf i))))))



(finalize)
