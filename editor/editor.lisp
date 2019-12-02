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
  (move edit 0 amount t))

(defun move-cursor (edit r c)
  (move edit r c nil))

(defun move (edit r c wrap)
  (let ((cur (editor-cur edit)))
    (cond ((and (= 0 r) (< c 0) (>= (+ (cursor-col cur) c) 0))
           ;; Moving left and will not underflow column
           (incf (cursor-index (editor-cur edit)) c)
           (incf (cursor-col (editor-cur edit)) c))
          ((and (= 0 r) (< c 0))
           ;; Moving left and will underflow column
           (when wrap
            (when (> (cursor-line cur) 0)
              (incf (cursor-line (editor-cur edit)) -1)
              (let* ((eol (jbrope:prev (editor-rope edit)
                                       (1+ (cursor-index cur)) '(#\newline)))
                     (sol (jbrope:prev (editor-rope edit)
                                       (or eol 0) '(#\newline))))
                (setf (cursor-col (editor-cur edit)) (- (or eol 0) (or sol 0))))
              (incf (cursor-index (editor-cur edit)) -1))))
          ((and (= 0 r) (> c 0) (not (char= (jbrope:rope-ref
                                               (editor-rope edit)
                                               (cursor-index cur))
                                            #\newline))
                                (> (jbrope:rope-len (editor-rope edit))
                                   (1+ (cursor-index cur))))
           ;; Moving right, and will not overflow column
           (incf (cursor-index (editor-cur edit)) 1)
           (incf (cursor-col (editor-cur edit)) 1)
           (move edit r (- c 1) wrap))
          ((and (= 0 r) (> c 0) (> (jbrope:rope-len (editor-rope edit))
                                   (1+ (cursor-index cur))))
           ;; Moving right, and will overflow column
           (when wrap
            (incf (cursor-line (editor-cur edit)) 1)
            (setf (cursor-col (editor-cur edit)) 0)
            (incf (cursor-index (editor-cur edit)) 1))))))
