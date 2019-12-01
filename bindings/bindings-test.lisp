(defpackage jim.bindings.bindings-test
  (:use :cl :prove :jim.bindings))

(in-package :jim.bindings.bindings-test)

(plan nil)

(let* ((*key-bindings* (make-trie))
      (*key-state* *key-bindings*)
      (status nil))

  (bind (<C-c> <C-x>)
    (setf status 0))
  (bind ("gqq" <CR>)
    (setf status 1))
  (bind ("gqg" <CR>)
    (setf status 2))

  (do-keypress <C-c>)
  (is status nil)
  (do-keypress <C-x>)
  (is status 0)

  (do-keypress #\g)
  (is status 0)
  (do-keypress #\q)
  (is status 0)
  (do-keypress #\q)
  (is status 0)
  (do-keypress <CR>)
  (is status 1)

  (do-keypress #\g)
  (is status 1)
  (do-keypress #\q)
  (is status 1)
  (do-keypress #\g)
  (is status 1)
  (do-keypress <CR>)
  (is status 2)

  (do-keypress <C-c>)
  (is status 2)
  (do-keypress <C-c>)
  (is status 2)
  (do-keypress <C-c>)
  (is status 2)
  (do-keypress <C-x>)
  (is status 0)

  )







(finalize)
