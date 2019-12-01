(defpackage :jim.bindings.keys
  (:use :cl)
  (:export
    kseq
    <CR> <Esc>
    <F2> <F3> <F4> <F5> <F6> <F7> <F8> <F9> <F10> <F12>
    <Insert> <Del> <Backspace>
    <Up> <Down> <Right> <Left>
    <C-a> <C-b> <C-c> <C-d> <C-e> <C-f> <C-g> <C-h> <C-i> <C-j> <C-k> <C-l>
    <C-m> <C-n> <C-o> <C-p> <C-q> <C-r> <C-s> <C-t> <C-u> <C-v> <C-w> <C-x>
    <C-y> <C-z>))

(in-package :jim.bindings.keys)


(defun kseq (&rest args)
  "produces a list of chars from arbitrallity deeply nested lists and strings"
  (kseq-lst args))

(defun kseq-lst (arg)
  (cond
    ((null arg) nil)
    ((stringp arg) (coerce arg 'list))
    ((characterp arg) (list arg))
    ((atom arg) (error "invalid keybinding sequence"))
    (t (mapcan #'kseq-lst arg))))


(defvar <CR> #\newline)
(defvar <Esc> #\escape)

(defvar <F2>  '(#\esc #\O #\Q))
(defvar <F3>  '(#\esc #\O #\R))
(defvar <F4>  '(#\esc #\O #\S))
(defvar <F5>  '(#\esc #\[ #\1 #\5 #\~))
(defvar <F6>  '(#\esc #\[ #\1 #\7 #\~))
(defvar <F7>  '(#\esc #\[ #\1 #\8 #\~))
(defvar <F8>  '(#\esc #\[ #\1 #\9 #\~))
(defvar <F9>  '(#\esc #\[ #\2 #\0 #\~))
(defvar <F10> '(#\esc #\[ #\2 #\1 #\~))
(defvar <F12> '(#\esc #\[ #\2 #\4 #\~))

(defvar <Insert> '(#\esc #\[ #\2 #\~))
(defvar <Del> '(#\esc #\[ #\3 #\~))
(defvar <Backspace> #\backspace)

(defvar <Up> '(#\esc #\[ #\A))
(defvar <Down> '(#\esc #\[ #\B))
(defvar <Right> '(#\esc #\[ #\C))
(defvar <Left> '(#\esc #\[ #\D))

(defvar <C-a> (code-char 1))
(defvar <C-b> (code-char 2))
(defvar <C-c> (code-char 3))
(defvar <C-d> (code-char 4))
(defvar <C-e> (code-char 5))
(defvar <C-f> (code-char 6))
(defvar <C-g> (code-char 7))
(defvar <C-h> (code-char 8))
(defvar <C-i> (code-char 9))
(defvar <C-j> (code-char 10))
(defvar <C-k> (code-char 11))
(defvar <C-l> (code-char 12))
(defvar <C-m> (code-char 13))
(defvar <C-n> (code-char 14))
(defvar <C-o> (code-char 15))
(defvar <C-p> (code-char 16))
(defvar <C-q> (code-char 17))
(defvar <C-r> (code-char 18))
(defvar <C-s> (code-char 19))
(defvar <C-t> (code-char 20))
(defvar <C-u> (code-char 21))
(defvar <C-v> (code-char 22))
(defvar <C-w> (code-char 23))
(defvar <C-x> (code-char 24))
(defvar <C-y> (code-char 25))
(defvar <C-z> (code-char 26))
