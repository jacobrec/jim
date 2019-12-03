(defpackage :jim-editor
  (:use :common-lisp :jim-utils)
  (:export *editor*
           open-new-tab
           new-editor
           editor-buffer
           editor-cur
           editor-cmd
           editor-index
           editor-redraw
           editor-mode
           editor-tabs
           editor-selected-tab
           tab-name
           tab-dirty

           insert
           undo
           delete-from

           move-cursor-row
           move-cursor-col
           slide-cursor
           active-cursor-index

           add-char
           set-dirty
           set-mode
           backspace))

(in-package :jim-editor)

; the global editor state
(defvar *editor*)

; an individual tab
(defstruct tab
  buffer
  cur)

(defun tab-name (tab)
 (jbedit:buffer-fname (tab-buffer tab)))
(defun tab-dirty (tab)
 (jbedit::buffer-dirty (tab-buffer tab)))

; contains the entire editor
(defstruct editor
  mode
  selected-tab
  tabs
  redraw
  cmd)

(defun new-editor (files)
  (make-editor
    :mode :normal
    :selected-tab 0
    :tabs (mapcar (lambda (filename) (open-tab filename))
                  (or files '("/tmp/jimscratch")))
    :redraw (list :tabs :status :buffer :cmd)
    :cmd ""))

(defun set-editor-buffer (edit buf)
  (let ((tab (nth (editor-selected-tab edit) (editor-tabs edit))))
    (setf (tab-buffer tab) buf)))

(defun editor-buffer (edit)
  (tab-buffer (nth (editor-selected-tab edit) (editor-tabs edit))))

(defun editor-rope (edit)
  (jbedit:buffer-head (editor-buffer edit)))

(defun editor-cur (edit)
  (tab-cur (nth (editor-selected-tab edit) (editor-tabs edit))))

(defun editor-index (edit)
  (cursor-index (tab-cur (nth (editor-selected-tab edit) (editor-tabs edit)))))


;; Edit functionalities
(defun delete-from (edit start end)
  (set-editor-buffer edit (jbedit:del-from (editor-buffer edit)
                                           (max 0 start) (max 0 end))))

(defun undo (edit amount)
  (loop as i from 0 to amount do
    (set-editor-buffer edit (jbedit:undo (editor-buffer edit)))))

(defun insert (edit str loc)
  (set-editor-buffer edit (jbedit:insert (editor-buffer edit) str loc)))


;; tab manipualtion
(defun open-tab (filename)
  (make-tab
    :buffer (jbedit:open-buff filename)
    :cur (make-cursor :index 0 :line 0 :col 0)))

(defun open-new-tab (edit filename)
  (setf (editor-tabs edit) (append (editor-tabs edit) (list (open-tab filename)))))

;; TODO: cursor movements are really ineffecient
(defun slide-cursor (edit amount)
  (move-cols edit amount t))
(defun move-cursor-col (edit amount)
  (move-cols edit amount nil))
(defun move-cursor-row (edit amount)
  (let ((c (cursor-col (editor-cur edit))))
    (move-rows edit amount)
    (move-cols edit c nil)))

(defun move-cols (edit c wrap)
  (let* ((cur (editor-cur edit))
         (i (cursor-index cur))
         (rope (editor-rope edit)))
    (cond  ((and (> c 0) (< (cursor-index cur) (jbrope:rope-len rope))
                 (char= #\newline (jbrope:rope-ref rope (1+ i))))
            ;; Forward hits line end
            (when wrap
              (unless (= (+ (cursor-index cur) 2) (jbrope:rope-len rope))
                (incf (cursor-index cur) 2)
                (incf (cursor-line cur))
                (setf (cursor-col cur) 0)
                (move-cols edit (1- c) wrap))))
           ((and (> c 0) (< (cursor-index cur) (jbrope:rope-len rope)))
            ;; Forward no line end
            (incf (cursor-index cur))
            (incf (cursor-col cur))
            (move-cols edit (1- c) wrap))
           ((and (< c 0) (or (= i 0)
                             (char= #\newline (jbrope:rope-ref rope (1- i)))))
            ;; Backwards line end
            (when (and wrap (> (cursor-line cur) 0))
              (decf (cursor-index cur) 2)
              (decf (cursor-line cur))
              (setf (cursor-col cur)
                    (- (or (jbrope:next rope (cursor-index cur) '(#\newline))
                           (jbrope:rope-len rope))
                       (or (jbrope:prev rope (cursor-index cur) '(#\newline)) -1)))
              (decf (cursor-col cur) 2)
              (move-cols edit (1+ c) wrap)))
           ((< c 0)
            ;; Backwards no line end
            (decf (cursor-index cur))
            (decf (cursor-col cur))
            (move-cols edit (1+ c) wrap)))))

(defun move-rows (edit r)
  (let* ((cur (editor-cur edit))
         (i (cursor-index cur))
         (rope (editor-rope edit)))
    (when (> r 0)
      ; moving down (incrementing row)
      (decf r 1)
      (decf (cursor-index cur) (cursor-col cur))
      (let ((eol (jbrope:next rope (cursor-index cur) '(#\newline))))
        (when eol
          (setf (cursor-index cur) eol)
          (incf (cursor-index cur))
          (setf (cursor-col cur) 0)
          (incf (cursor-line cur))
          (move-rows edit r))))
    (when (and (< 0 (cursor-line cur)) (> 0 r))
      ; moving up (decrementing row)
      (incf r 1)
      (let* ((loc (or (jbrope:prev rope
                            (or (jbrope:prev rope i '(#\newline)) 0)
                           '(#\newline)) 0))
             (dif (- i loc)))
        (decf (cursor-index cur) dif)
        (unless (= 0 loc)
          (incf (cursor-index cur)))
        (setf (cursor-col cur) 0)
        (decf (cursor-line cur)))
      (move-rows edit r))))

(defun add-char (edit ch)
  (insert edit (make-string 1 :initial-element ch)
               (cursor-index (editor-cur edit)))
  (slide-cursor edit 1)
  (set-dirty edit :buffer))


(defun set-dirty (edit place)
  (setf (editor-redraw edit) (cons place (editor-redraw edit))))

(defun set-mode (edit mode)
  (set-dirty edit :status)
  (set-dirty edit :cmd)
  (setf (editor-mode edit) mode))

(defun backspace (edit)
  (delete-from edit (1- (editor-index edit)) (editor-index edit))
  (slide-cursor edit -1)
  (set-dirty edit :buffer))
