;;;; the package that .jimrc is loaded in
(defpackage :jim-user
  (:use :cl :jim.bindings :jim.api))

(defpackage :jim-user-util
  (:use :cl))

(in-package :jim-user-util)

(defun jim-load (file)
  "loads a file in the :jim-user package"
  (let ((old-pkg *package*)
        (old-rtable *readtable*))
    (in-package :jim-user)
    (load file)
    (setf *package* old-pkg)
    (setf *readtable* old-rtable)))
