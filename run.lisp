#!/bin/sbcl --script
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :jim)

(jim-app:run-app)
