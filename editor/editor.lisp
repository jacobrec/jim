(defpackage :jim-editor
  (:use :common-lisp :jim-utils)
  (:export open-new-tab
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

           insert
           undo
           delete-from

           move-to-cursor
           move-cursor
           slide-cursor
           active-cursor-index))

(in-package :jim-editor)

; an individual tab
(defstruct tab
  buffer
  cur)

(defun tab-name (tab)
 (jbedit:buffer-fname (tab-buffer tab)))

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
;; TODO: editor crashes if you go down too far (infinite recursion in these methods?)
;;; Cursor movements
(defun move-to-cursor (edit r c &optional (propegate t))
  (let ((cur (editor-cur edit)))
    (setf (cursor-line cur) (max 0 (min (1- (term-height)) r)))
    (setf (cursor-col cur) (max 0 (min (1- (term-width)) c)))
    (when propegate
      (setf (cursor-index cur)
            (jbrope:coord-to-idx
             (jbedit:buffer-head (editor-buffer edit))
             (cursor-line cur)
             (cursor-col cur)))
      (refresh-cursor edit))))

(defun refresh-cursor (edit)
  (when (> 0 (cursor-index (editor-cur edit)))
    (setf (cursor-index (editor-cur edit)) 0))
  (if (char= #\newline (jbrope:rope-ref (jbedit:buffer-head
                                          (editor-buffer edit))
                                        (cursor-index (editor-cur edit))))
      (move-cursor edit 0 -1)
      (let ((loc (jbrope:idx-to-coord
                   (jbedit:buffer-head
                     (editor-buffer edit))
                   (cursor-index (editor-cur edit)))))
        (move-to-cursor edit (car loc) (cdr loc) nil))))

(defun slide-cursor (edit amount)
  (incf (cursor-index (editor-cur edit)) amount)
  (refresh-cursor edit))

(defun move-cursor (edit r c)
  (move-to-cursor
    edit
    (+ r (cursor-line (editor-cur edit)))
    (+ c (cursor-col (editor-cur edit)))))

