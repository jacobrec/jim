(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :jedit)

(run-app)
