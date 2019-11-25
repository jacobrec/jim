(defconstant BLK 0)
(defconstant RED 1)
(defconstant GRN 2)
(defconstant YEL 3)
(defconstant BLU 4)
(defconstant MAG 5)
(defconstant CYN 6)
(defconstant WHT 7)
(defconstant RST 9)

(defpackage :jedit-io
  (:use :common-lisp)
  (:export draw-screen))

(defun command (&rest args)
  (let* ((m (reverse args)) (e (car m)) (a (reverse (cdr m))))
    (format t "~a[~{~a~^;~}~a" #\escape a e)))

(defun set-color (color side &optional brightness)
  (let ((ground (if (eq 'bg side) 40 30)))
    (command (+ (if (eq 'bright brightness)
                    60 0)
                color ground) #\m)))

(defun term-width ()
  (parse-integer
    (string-trim
      '(#\newline)
      (trivial-shell:shell-command "tput cols"))))

(defun term-height ()
  (parse-integer
    (string-trim
      '(#\newline)
      (trivial-shell:shell-command "tput lines"))))

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
  (format t "~a" (make-string (term-width) :initial-element #\ ))
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
  (set-color RST 'bg)
  (format t "~%"))


(defun draw-status (left center right)
  (set-color BLK 'bg 'bright)
  (set-color YEL 'fg 'bright)
  (move-to 1 (- (term-height) 1))
  (format t "~a" (make-string (term-width) :initial-element #\ ))

  (move-to 1 (- (term-height) 1))
  (format t "~a" left)

  (move-to (floor (- (term-width) (length center)) 2) (- (term-height) 1))
  (format t "~a" center)

  (move-to (+ 1 (- (term-width) (length right))) (- (term-height) 1))
  (format t "~a" right)

  (set-color RST 'fg)
  (set-color RST 'bg)
  (format t "~%"))

(defun draw-command (cmd)
  (when cmd
    (move-to 1 (term-height))
    (format t ":~a" cmd)))


(defun draw-screen (screen)
  (command 2 #\J) ; clear screeen
  (save-cursor)
  (command 6 #\p) ; Set cursor invisible

  (draw-tabs (getf screen 'tabs) (getf screen 'selected))
  (format t "~a" (getf screen 'buffer))
  (draw-status
    (getf screen 'mode)
    (nth (getf screen 'selected) (getf screen 'tabs))
    "row/lines : col") ; derive this last one from buffer
  (draw-command (getf screen 'cmd))

  (command 7 #\p) ; Set cursor visible
  (restore-cursor))


