;;;; jim's api for keybindings an configuration
(defpackage :jim.api
  (:nicknames api)
  (:use :cl :jim.bindings)
  (:export
    *editor*
    set-mode
    mode
    set-content
    undo
    current-buffer
    write-buffer
    buffer-dirty
    exit-jim
    is-running
    cursor-left
    cursor-right
    cursor-up
    cursor-down
    cursor-to
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
    cmd
    prompt))

(in-package :jim.api)

; the global editor state
(defvar *editor*)

;; editor state

(defun set-mode (mode)
  (jim-editor:set-mode *editor* mode))

(defun mode ()
  (jim-editor:editor-mode *editor*))

(defun set-content (str)
  (setf (jim-editor:tab-buffer (nth (jim-editor:editor-selected-tab *editor*)
                                    (jim-editor:editor-tabs *editor*)))
	(jbedit:make-buffer :stack (jbrope:str-to-rope str)
			    :redo nil
			    :dirty nil
			    :fname "jtodo"))
  (jim-editor:set-dirty *editor* :buffer))

(defun undo ()
  (jim-editor:undo *editor* 1)
  (jim-editor:set-dirty *editor* :buffer))

(defun current-buffer ()
  (jim-editor:editor-buffer *editor*))

(defun write-buffer (&key (buff (current-buffer)) name)
  ; TODO: write different buffers
  (jim-editor:write-buff *editor* name))

(defun buffer-dirty (&optional (buff (current-buffer)))
  (jbedit:buffer-dirty buff))

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

(defun cursor-to (x y)
  ; TODO: fix this hack
  (jim-editor:move-cursor-row *editor* -1000000)
  (jim-editor:move-cursor-col *editor* -1000000)
  (jim-editor:move-cursor-row *editor* x)
  (jim-editor:move-cursor-col *editor* y))

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

;; prompt api -- a higher level command interface

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defvar *prompt-bindings* (make-trie))
(defvar *old-bindings*)
(defvar *prompt-len*)
(defvar *prompt-callback*)
(defvar *prompt-cancel*)

(defun prompt (pr fn cancel)
  "prompt with pr and callback to fn with the entered command
   restores bindings when run or canceled"
  (setf *prompt-len* (length pr))
  (set-cmd (make-adjustable-string pr))
  (set-cmd-cur *prompt-len*)
  (setf *old-bindings* *key-bindings*)
  (set-key-bindings *prompt-bindings*)
  (setf *prompt-callback* fn)
  (setf *prompt-cancel* cancel))

(defmacro bind-prompt ((&rest keys) &rest body)
  `(let ((*key-bindings* *prompt-bindings*))
    (bind (,@keys) ,@body)))

(bind-prompt ('*)
  (when (char>= *last-key* #\ )
    (vector-push-extend *last-key* (cmd))
    (flush-cmd)
    (set-cmd-cur (1+ (cmd-cur)))))

(bind-prompt (<C-c>)
  (set-cmd-cur nil)
  (set-cmd "")
  (set-key-bindings *old-bindings*)
  (funcall *prompt-cancel*))

(bind-prompt (#\rubout)
  (if (> (length (cmd)) *prompt-len*)
    (progn
      (vector-pop (cmd))
      (flush-cmd)
      (set-cmd-cur (1- (cmd-cur))))
    (progn
      (set-cmd-cur nil)
      (set-cmd "")
      (set-key-bindings *old-bindings*)
      (funcall *prompt-cancel*))))

(bind-prompt (#\return)
  (let ((str (string-trim '(#\space #\return #\linefeed)
                          (subseq (cmd) *prompt-len*))))
    (set-cmd-cur nil)
    (set-cmd "")
    (set-key-bindings *old-bindings*)
    (funcall *prompt-callback* str)))
