(defpackage :jtodo-org-asd
  (:use :cl :asdf))

(in-package :jtodo-org-asd)

(defsystem :jtodo-org
  :version "0.0"
  :author  "Peter Elliott"
  :license "GPLv3"
  :depends-on (:jim.bindings :jim)
  :components ((:file "jtodo")
	       (:file "jtodo-org"))
  :description "like org-mode, but for jim and jtodo")
