;;;; jim's api for keybindings an configuration
(defpackage :jim.api
  (:nicknames api)
  (:use :cl)
  (:export
    *editor*
    set-mode
    undo
    exit-jim
    is-running
    cursor-left
    cursor-right
    cursor-up
    cursor-down
    cursor->line-start
    cursor->line-end
    del
    backspace
    enter
    insert
    insert-char
    set-cmd-cur
    cmd-cur
    set-cmd
    flush-cmd
    cmd))

(in-package :jim.api)

; the global editor state
(defvar *editor*)

;; editor state

(defun set-mode (mode)
  (jim-editor:set-mode *editor* mode))

(defun undo ()
  (jim-editor:undo *editor* 1)
  (jim-editor:set-dirty *editor* :buffer))

(defvar running t)

(defun exit-jim ()
  (setf running nil))

(defun is-running ()
  running)

;; cursor

(defun cursor-left ()
  (jim-editor:move-cursor-col *editor* -1))

(defun cursor-right ()
  (jim-editor:move-cursor-col *editor* 1))

(defun cursor-up ()
  (jim-editor:move-cursor-row *editor* -1))

(defun cursor-down ()
  (jim-editor:move-cursor-row *editor* 1))

(defun cursor->line-start ()
  (jim-editor:move-cursor-col *editor* -1000000))

(defun cursor->line-end ()
  (jim-editor:move-cursor-col *editor* 1000000))

;; editing

(defun del ()
  (jim-editor:delete-from *editor* (jim-editor:editor-index *editor*)
               (1+ (jim-editor:editor-index *editor*)))
  (jim-editor:set-dirty *editor* :buffer))

(defun backspace ()
  (jim-editor:delete-from *editor* (1- (jim-editor:editor-index *editor*))
                          (jim-editor:editor-index *editor*))
  (jim-editor:slide-cursor *editor* -1)
  (jim-editor:set-dirty *editor* :buffer))


(defun insert (str &optional loc)
  (jim-editor:insert *editor*
    str (or loc (jim-editor:cursor-index
                  (jim-editor:editor-cur *editor*))))
  (jim-editor:slide-cursor *editor* 1)
  (jim-editor:set-dirty *editor* :buffer))

(defun insert-char (ch &optional loc)
  (insert (make-string 1 :initial-element ch) loc))

(defun enter ()
  (insert-char #\newline))

;; command line

(defun set-cmd-cur (n)
  "sets the command cursor, if (null n) then the regular cursor will be drawn"
  (setf (jim-editor:editor-cmdcur *editor*) n))

(defun cmd-cur ()
  (jim-editor:editor-cmdcur *editor*))

(defun set-cmd (str)
  (setf (jim-editor:editor-cmd *editor*) str)
  (jim-editor:set-dirty *editor* :cmd))

(defun flush-cmd ()
  (jim-editor:set-dirty *editor* :cmd))

(defun cmd ()
  (jim-editor:editor-cmd *editor*))
