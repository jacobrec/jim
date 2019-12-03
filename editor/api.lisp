;;;; jim's api for keybindings an configuration
(defpackage :jim.api
  (:nicknames api)
  (:use :cl)
  (:export
    *editor*
    set-mode
    undo
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
    insert-char))

(in-package :jim.api)

; the global editor state
(defvar *editor*)

;; editor state

(defun set-mode (mode)
  (jim-editor:set-mode *editor* mode))

(defun undo ()
  (jim-editor:undo *editor* 1)
  (jim-editor:set-dirty *editor* :buffer))

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
