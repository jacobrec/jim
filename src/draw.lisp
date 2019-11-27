(defpackage :jim-io
  (:use :common-lisp :jim-utils)
  (:export draw-screen))

(in-package :jim-io)

(defconstant BLK 0)
(defconstant RED 1)
(defconstant GRN 2)
(defconstant YEL 3)
(defconstant BLU 4)
(defconstant MAG 5)
(defconstant CYN 6)
(defconstant WHT 7)
(defconstant RST 9)


(defun set-color (color side &optional brightness)
  (let ((ground (if (eq 'bg side) 40 30)))
    (command (+ (if (eq 'bright brightness)
                    60 0)
                color ground) #\m)))

(defun save-cursor ()
  (command #\s))

(defun restore-cursor ()
  (command #\u))

(defun move-to (x y)
  (command y x #\H))

(defun draw-tabs (tabs selected)
  (set-color BLK 'bg 'bright)
  (set-color YEL 'fg 'bright)
  (move-to 1 1)
  (draw-blank-line)
  (move-to 1 1)
  (let ((c 0))
    (loop for x in tabs do
      (unless (= c 0)
        (format t "|"))
      (when (= selected c)
        (command 1 4 #\m)); Set underline
      (format t " ~a " x)
      (command 0 #\m); Reset all styles
      (set-color YEL 'fg 'bright)
      (set-color BLK 'bg 'bright)
      (incf c)))
  (set-color RST 'fg)
  (set-color RST 'bg))

(defun draw-status (left center right)
  (set-color BLK 'bg 'bright)
  (set-color YEL 'fg 'bright)
  (move-to 1 (- (term-height) 1))
  (draw-blank-line)

  (move-to 1 (- (term-height) 1))
  (format t "~a" left)

  (move-to (floor (- (term-width) (length center)) 2) (- (term-height) 1))
  (format t "~a" center)

  (move-to (+ 1 (- (term-width) (length right))) (- (term-height) 1))
  (format t "~a" right)

  (set-color RST 'fg)
  (set-color RST 'bg))

(defun draw-blank-line ()
  (format t "~a" (make-string (term-width) :initial-element #\ )))

(defun draw-command (screen)
  (move-to 1 (term-height))
  (draw-blank-line)
  (when (equal :cmd (getf screen :mode))
    (move-to 1 (term-height))
    (let ((cmd (getf screen :cmd)))
      (format t ":~a" (concatenate 'string (reverse cmd))))))

(defun draw-buffer (rbuf)
  (move-to 1 2)
  (loop for i from 0 to 20 do
    (draw-blank-line)
    (format t "~c~c" #\newline #\return))
  (move-to 1 2)
  (loop for chunk in (jbrope:chunks rbuf) do
    (map nil
         (lambda (x)
           (cond ((char= x #\newline)
                  (format t "~C~C" #\newline #\return))
                 (t (format t "~C" x))))
         chunk))
  (loop for c across (format nil "~a" rbuf) do
    (cond ((char= c #\newline)
           (format t "~C~C" #\newline #\return))
          (t (format t "~C" c)))))

(defun is-dirty (screen place)
  (let ((res (find place
               (getf screen :redraw))))
    (delete place (getf screen :redraw))
    res))

(defun draw-screen (screen)
  ;(command 2 #\J) ; clear screeen
  (command "?25" #\l) ; Set cursor invisible

  (when (is-dirty screen :tabs)
    (draw-tabs (getf screen :tabs) (getf screen :selected)))

  ; Draw buffer
  (when (is-dirty screen :buffer)
    (draw-buffer (jbedit:buffer-head (getf screen :buffer))))

  (when (is-dirty screen :status)
    (draw-status
      (getf screen :mode)
      (format nil "~a" (getf screen :cur))
      ;(nth (getf screen :selected) (getf screen :tabs))
      (format nil "R:~a C~a"
              (cursor-line (getf screen :cur))
              (cursor-col (getf screen :cur)))))
  (when (is-dirty screen :cmd)
    (draw-command screen))

  (move-to (cursor-col (getf screen :cur))
           (1+ (cursor-line (getf screen :cur))))
  (command "?25" #\h) ; Set cursor visible
  (finish-output))


