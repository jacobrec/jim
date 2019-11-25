(defpackage :jim-asd
  (:use :cl :asdf))

(in-package :jim-asd)

(defsystem jim
  :version "0.0"
  :author  "Jacob Reckhard"
  :license "LGPLv3"
  :depends-on ("trivial-shell")
  :components ((:module "src"
                :components
                ((:file "buffer")
                 (:file "draw" :depends-on ("buffer"))
                 (:file "main" :depends-on ("buffer" "draw")))))
  :description "A pretty bad vi like editor")
