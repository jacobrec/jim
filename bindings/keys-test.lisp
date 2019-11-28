(defpackage jim.bindings.keys-test
  (:use :cl :prove :jim.bindings))

(in-package :jim.bindings.keys-test)

(plan nil)

(is (kseq) nil)
(is (kseq #\a) '(#\a))
(is (kseq <Up>) '(#\esc #\[ #\A))
(is (kseq <Up> #\a #\x #\y) '(#\esc #\[ #\A #\a #\x #\y))
(is (kseq <Up> #\a #\x #\y) '(#\esc #\[ #\A #\a #\x #\y))
(is (kseq <Up> "axy") '(#\esc #\[ #\A #\a #\x #\y))
(is (kseq '("a" "b") '("c" "d")) '(#\a #\b #\c #\d))


(finalize)
