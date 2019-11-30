(defpackage jim.bindings.trie-test
  (:use :cl :prove :jim.bindings))

(in-package :jim.bindings.trie-test)

(plan nil)

(let ((trie (make-trie)))
  (trie-insert trie '(#\a) 7)
  (is (trie-ref trie '(#\a)) 7)
  (is (trie-ref trie '(#\b)) nil)
  (is (trie-ref trie '(#\a #\b)) nil)
  (is (trie-ref trie '(#\b #\a)) nil))

(let ((trie (make-trie)))
  (trie-insert trie '(#\a #\a #\a) 7)
  (trie-insert trie '(#\a #\a #\b) 8)
  (trie-insert trie '(#\a #\a #\c) 9)
  (trie-insert trie '(#\a #\b #\a) 10)
  (trie-insert trie '(#\a #\c #\a) 11)
  (trie-insert trie '(#\b #\a #\a) 12)
  (trie-insert trie '(#\c #\a #\a) 13)

  (is (trie-ref trie '(#\a #\a #\a)) 7)
  (is (trie-ref trie '(#\a #\a #\b)) 8)
  (is (trie-ref trie '(#\a #\a #\c)) 9)
  (is (trie-ref trie '(#\a #\b #\a)) 10)
  (is (trie-ref trie '(#\a #\c #\a)) 11)
  (is (trie-ref trie '(#\b #\a #\a)) 12)
  (is (trie-ref trie '(#\c #\a #\a)) 13)

  (is (trie-ref trie '(#\a #\a)) nil)
  (is (trie-ref trie '(#\a #\a #\d)) nil)
  (is (trie-ref trie '()) nil))

(let ((trie (make-trie)))
  (is (trie-inc trie '(#\a)) nil))

(finalize)
