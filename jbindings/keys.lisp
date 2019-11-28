(defpackage :jbindings-keys
  (:nicknames :jkeys)
  (:use :cl :asdf)
  (#:export
    <CR> <Esc>
    <F2> <F3> <F4> <F5> <F6> <F7> <F8> <F9> <F10> <F12>
    <Insert> <Del> <Backspace>
    <Up> <Down> <Right> <Left>
    <C-a> <C-b> <C-c> <C-d> <C-e> <C-f> <C-g> <C-h> <C-i> <C-j> <C-k> <C-l>
    <C-m> <C-n> <C-o> <C-p> <C-q> <C-r> <C-s> <C-t> <C-u> <C-v> <C-w> <C-x>
    <C-y> <C-z>))

(in-package :jbindings-keys)

(defconstant <CR> #\newline)
(defconstant <Esc> #\escape)

(defconstant <F2> (list #\esc #\O #\Q))
(defconstant <F3> (list #\esc #\O #\R))
(defconstant <F4> (list #\esc #\O #\S))
(defconstant <F5> (list #\esc #\[ #\1 #\5 #\~))
(defconstant <F6> (list #\esc #\[ #\1 #\7 #\~))
(defconstant <F7> (list #\esc #\[ #\1 #\8 #\~))
(defconstant <F8> (list #\esc #\[ #\1 #\9 #\~))
(defconstant <F9> (list #\esc #\[ #\2 #\0 #\~))
(defconstant <F10> (list #\esc #\[ #\2 #\1 #\~))
(defconstant <F12> (list #\esc #\[ #\2 #\4 #\~))

(defconstant <Insert> (list #\esc #\[ #\2 #\~))
(defconstant <Del> (list #\esc #\[ #\3 #\~))
(defconstant <Backspace> #\backspace)

(defconstant <Up> (list #\esc #\[ #\A))
(defconstant <Down> (list #\esc #\[ #\B))
(defconstant <Right> (list #\esc #\[ #\C))
(defconstant <Left> (list #\esc #\[ #\D))

(defconstant <C-a> (code-char 1))
(defconstant <C-b> (code-char 2))
(defconstant <C-c> (code-char 3))
(defconstant <C-d> (code-char 4))
(defconstant <C-e> (code-char 5))
(defconstant <C-f> (code-char 6))
(defconstant <C-g> (code-char 7))
(defconstant <C-h> (code-char 8))
(defconstant <C-i> (code-char 9))
(defconstant <C-j> (code-char 10))
(defconstant <C-k> (code-char 11))
(defconstant <C-l> (code-char 12))
(defconstant <C-m> (code-char 13))
(defconstant <C-n> (code-char 14))
(defconstant <C-o> (code-char 15))
(defconstant <C-p> (code-char 16))
(defconstant <C-q> (code-char 17))
(defconstant <C-r> (code-char 18))
(defconstant <C-s> (code-char 19))
(defconstant <C-t> (code-char 20))
(defconstant <C-u> (code-char 21))
(defconstant <C-v> (code-char 22))
(defconstant <C-w> (code-char 23))
(defconstant <C-x> (code-char 24))
(defconstant <C-y> (code-char 25))
(defconstant <C-z> (code-char 26))
