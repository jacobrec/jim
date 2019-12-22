(defpackage :jim-io
  (:use :common-lisp :jim-utils :jim-editor)
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
      (format t " ~a~a " (tab-name x) (if (tab-dirty x) "*" ""))
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

(defun draw-command (cmd mode)
  (move-to 1 (term-height))
  (draw-blank-line)
  (move-to 1 (term-height))
  (format t "~a" cmd))

(defun clear-lines (start end)
  (loop for i from start to end do
       (move-to 1 i)
       (command 2 #\K)))

(defun draw-buffer (rbuf &optional (start 0) (end -1))
  (clear-lines 2 (- (term-height) 2))
  (move-to 1 2)
  (draw-blank-line)
  (jbrope::iterate-lines rbuf
    (lambda (x)
      (format t "~C~a" #\return x)
      (set-color BLU 'fg)
      (format t "~C~C~C" #\↵ #\newline #\return)
      (set-color RST 'fg)
      (draw-blank-line))
    start end))

(defun is-dirty (edit place)
  (let ((res (find place (editor-redraw edit))))
    (delete place (editor-redraw edit))
    res))

(defun draw-screen (edit)
  ;(command 2 #\J) ; clear screeen
  (command "?25" #\l) ; Set cursor invisible

  ; Draw tab
  (when (is-dirty edit :tabs)
    (draw-tabs (editor-tabs edit) (editor-selected-tab edit)))

  ; Draw buffer
  (when (is-dirty edit :buffer)
    (draw-buffer (jbedit:buffer-contents (editor-buffer edit))
                 (tab-line (editor-tab edit))
                 (+ (tab-line (editor-tab edit))
                    (term-height)
                    -3))) ; TODO: anything but this

  ; Draw status bar
  (when (or t (is-dirty edit :status))
    (draw-status
      (editor-mode edit)
      (format nil "~a" (editor-cur edit))
      (format nil "R:~a C~a || ~a"
              (cursor-line (editor-cur edit))
              (cursor-col (editor-cur edit))
              (jbrope:rope-len (jbedit:buffer-contents (editor-buffer edit))))))

  ; Draw command propmt
  (when (is-dirty edit :cmd)
    (draw-command (editor-cmd edit) (editor-mode edit)))

  (if (editor-cmdcur edit)
    (move-to (1+ (editor-cmdcur edit)) (term-height))
    (move-to (+ 1 (cursor-col (editor-cur edit)))
             (+ 2 (- (cursor-line (editor-cur edit))
                     (tab-line (editor-tab edit))))))
  (command "?25" #\h) ; Set cursor visible
  (finish-output))
