(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :jim)

(run-app)
