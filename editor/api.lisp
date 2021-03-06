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
    set-buffer-name
    buffers
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
    begin-scratch
    commit-scratch
    slide-cursor
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
    set-default-cursor
    cursor-style
    prompt
    get-param
    get-default-param
    set-param
    set-default-param))

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
                            :fname "/dev/null"))
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

(defun set-buffer-name (name &optional (buff (current-buffer)))
  (setf (jim-editor:tab-dname buff) name))

(defun buffers ()
  (jim-editor:editor-tabs *editor*))

(defun get-buffer (n)
  (nth n (buffers)))

(defun num-buffers ()
  (length (buffers)))

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

(defun begin-scratch (&optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:begin-scratch (jim-editor:tab-buffer buff)
                              (jim-utils:cursor-index
                               (jim-editor:tab-cur buff)))))

(defun commit-scratch (&optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:commit-scratch (jim-editor:tab-buffer buff))))

(defun del-range (start end &optional (buff (current-buffer)))
  (setf (jim-editor:tab-buffer buff)
        (jbedit:del-from (jim-editor:tab-buffer buff)
                         (max 0 start) (max 0 end))))

(defun del (&optional (buff (current-buffer)))
  (del-range (cursor-index buff) (1+ (cursor-index buff)))
  (jim-editor:set-dirty *editor* :buffer)
  (jim-editor:set-dirty *editor* :tabs))

(defun backspace (&optional (buff (current-buffer)))
  (jim-editor:slide-cursor *editor* -1 nil t) ; TODO: this is not buffer specific
  (del buff))

(defun slide-cursor (amount &optional (buff (current-buffer))
                              (for-newline nil) (for-delete nil))
  (jim-editor:slide-cursor *editor* amount for-newline for-delete)) ; TODO: this is not buffer specific

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

;; cursor style

(defvar *default-cursor* nil)

(defun cursor-form (form)
  (cond
    ((null form) 0)
    ((equal form '(:block t))       1)
    ((equal form '(:block nil))     2)
    ((equal form '(:underline t))   3)
    ((equal form '(:underline nil)) 4)
    ((equal form '(:i-beam t))      5)
    ((equal form '(:i-beam nil))    6)
    (t (error "invalid cursor specifier"))))

(defun set-default-cursor (style &optional (blinking t))
  "configures jim's default cursor style. if unset,
   the terminal's default will be used"
  (setf *default-cursor*
        (if style
            (list style blinking)
            nil)))

(defun cursor-style (style &optional (blinking t))
  "set the cursor style. :default will reset to the default"
  (jim-utils:command (cursor-form (if (eq style :default)
                                      *default-cursor*
                                      (list style blinking)))
                     " q"))

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

;; params

(defun get-param (key &optional (buff (current-buffer)))
  (jim-editor:tab-param buff key))

(defun get-default-param (key)
  (cdr (assoc key jim-editor:*default-params*)))

(defun set-param (key value &optional (buff (current-buffer)))
  (setf (jim-editor:tab-params buff)
        (acons key value (jim-editor:tab-params buff)))
  (jim-editor:set-dirty *editor* :buffer))

(defun set-default-param (key value)
  (setf jim-editor:*default-params*
        (acons key value jim-editor:*default-params*)))
