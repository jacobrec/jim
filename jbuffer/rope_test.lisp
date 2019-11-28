(defpackage rope-test
  (:use :cl :prove :jbrope))
(in-package :rope-test)

(plan nil)


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
  (is (jbrope::bsearch test 10000) 8))

(let ((rope (jbrope:str-to-rope (format nil "12345~%67890"))))
  (is (jbrope:idx-to-coord rope 0) '(0 . 0))
  (is (jbrope:idx-to-coord rope 1) '(0 . 1))
  (is (jbrope:idx-to-coord rope 3) '(0 . 3))
  (is (jbrope:idx-to-coord rope 6) '(1 . 0))
  (is (jbrope:idx-to-coord rope 7) '(1 . 1))
  (is (jbrope:idx-to-coord rope 8) '(1 . 2))
  (is (jbrope:idx-to-coord rope 10) '(1 . 4)))



(finalize)
