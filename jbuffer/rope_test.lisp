(defpackage rope-test
  (:use :cl :prove :jbrope))
(in-package :rope-test)

(plan 21)


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
  (is (coord-to-idx rope2 0 0) 0)
  (is (coord-to-idx rope2 0 1) 1)
  (is (coord-to-idx rope2 1 0) 7)
  (is (coord-to-idx rope2 1 1) 8)
  (is (coord-to-idx rope2 2 0) 13)
  (is (coord-to-idx rope2 2 1) 14))


(finalize)
