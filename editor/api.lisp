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
    get-buffer
    num-buffers
    goto-buffer
    next-buffer
    previous-buffer
    new-buffer
    close-buffer
    set-bindings
    cursor-left
    cursor-right
    cursor-up
    cursor-down
    cursor-to
    cursor->line-start
    cursor->line-end
    cursor-index
    cursor-line
    cursor-col
    del-range
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

(defun set-mode (mode &optional (buff (current-buffer)))
  (setf (jim-editor:tab-mode buff) mode))

(defun undo (&optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:undo (jim-editor:tab-buffer buff)))
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :tabs))

(defun mode (&optional (buff (current-buffer)))
  (jim-editor:tab-mode buff))

(defun set-content (str &optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
	(jbedit:make-buffer :stack (jbrope:str-to-rope str)
			    :redo nil
			    :dirty nil
			    :fname "jtodo"))
  (jim-editor:set-dirty *editor* :buffer))

(defun current-buffer ()
  (nth (jim-editor:editor-selected-tab *editor*)
       (jim-editor:editor-tabs *editor*)))

(defun write-buffer (&key (buff (current-buffer)) name)
  (setf (jim-editor:tab-buffer buff)
        (jbedit:write-buff (jim-editor:tab-buffer buff) name))
  (jim-editor:set-dirty *editor* :tabs))

(defun buffer-dirty (&optional (buff (current-buffer)))
  (jim-editor:tab-dirty buff))

(defvar running t)

(defun exit-jim ()
  (setf running nil))

;TODO: exit tab

(defun is-running ()
  running)

;; buffer switching

(defun get-buffer (n)
  (nth n (jim-editor:editor-tabs *editor*)))

(defun num-buffers ()
  (length (jim-editor:editor-tabs *editor*)))

(defun goto-buffer (n)
  (setf (jim-editor:editor-selected-tab *editor*)
        (mod n (num-buffers)))
  (jim-editor:set-dirty *editor* :tabs)
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :status))

(defun next-buffer ()
  (goto-buffer (1+ (jim-editor:editor-selected-tab *editor*))))

(defun previous-buffer ()
  (goto-buffer (1- (jim-editor:editor-selected-tab *editor*))))

(defun new-buffer (&optional (fname "/dev/null"))
  (prog1
    (jim-editor:open-new-tab *editor* fname)
    (jim-editor:set-dirty *editor* :tabs)))

(defun close-buffer (&optional (buff (current-buffer)))
  (setf (jim-editor:editor-tabs *editor*)
        (remove (if (integerp buff)
                    (get-buffer buff)
                    buff)
                (jim-editor:editor-tabs *editor*)))
  (goto-buffer (max (1- (num-buffers))
                    (jim-editor:editor-selected-tab *editor*)))
  (jim-editor:set-dirty *editor* :tabs)
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :status))


;; keybindings

(defun set-bindings (bindings &optional (buff (current-buffer)))
  (set-key-bindings bindings)
  (setf (jim-editor:tab-keybindings buff) bindings))

;; cursor

;TODO: cursor movements for buffers
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
  (jim-editor:move-cursor-row *editor* y)
  (jim-editor:move-cursor-col *editor* x))

(defun cursor->line-start ()
  (jim-editor:move-cursor-col *editor* -1000000))

(defun cursor->line-end ()
  (jim-editor:move-cursor-col *editor* 1000000))

(defun cursor-index (&optional (buff (current-buffer)))
  (jim-utils:cursor-index (jim-editor:tab-cur buff)))

(defun cursor-line (&optional (buff (current-buffer)))
  (jim-utils:cursor-line (jim-editor:tab-cur buff)))

(defun cursor-col (&optional (buff (current-buffer)))
  (jim-utils:cursor-col (jim-editor:tab-cur buff)))

;; editing

(defun del-range (start end &optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:del-from (jim-editor:tab-buffer buff)
                         (max 0 start) (max 0 end))))

(defun del (&optional (buff (current-buffer)))
  (del-range (cursor-index buff) (1+ (cursor-index buff)))
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :tabs))

(defun backspace (&optional (buff (current-buffer)))
  (jim-editor:slide-cursor *editor* -1) ; TODO: this is not buffer specific
  (del buff))

(defun insert (str &key (loc (cursor-index)) (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:insert (jim-editor:tab-buffer buff) str loc))
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :tabs))

(defun insert-char (ch &key (loc (cursor-index)) (buff (current-buffer)))
  (let ((for-newline (char= ch #\newline)))
    (insert (make-string 1 :initial-element ch)
            :loc loc :buff buff)
    ;TODO: this is not buffer specific
    (jim-editor:slide-cursor *editor* 1 for-newline)))

(defun enter (&key (loc (cursor-index)) (buff (current-buffer)))
  (insert-char #\newline :loc loc :buff buff))

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
  (set-bindings *prompt-bindings*)
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
  (set-bindings *old-bindings*)
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
      (set-bindings *old-bindings*)
      (funcall *prompt-cancel*))))

(bind-prompt (#\return)
  (let ((str (string-trim '(#\space #\return #\linefeed)
                          (subseq (cmd) *prompt-len*))))
    (set-cmd-cur nil)
    (set-cmd "")
    (set-bindings *old-bindings*)
    (funcall *prompt-callback* str)))
