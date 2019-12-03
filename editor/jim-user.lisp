;;;; the package that .jimrc is loaded in
(defpackage :jim-user
  (:use :cl :jim.bindings :jim.api))

(defpackage :jim-user-util
  (:use :cl)
  (:export
    jim-user-load
    load-jimrc))

(in-package :jim-user-util)

(defun jim-user-load (&rest args)
  "loads a file in the :jim-user package, forwards all args to load"
  (let ((old-pkg *package*)
        (old-rtable *readtable*))
    (in-package :jim-user)
    (apply #'load args)
    (setf *package* old-pkg)
    (setf *readtable* old-rtable)))

(defun load-jimrc ()
  "loads ~/.jimrc into the :jim-user package"
  (jim-user-load "~/.jimrc" :if-does-not-exist nil))
