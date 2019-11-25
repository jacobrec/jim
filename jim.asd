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
                 (:file "utils")
                 (:file "draw" :depends-on ("buffer" "utils"))
                 (:file "main" :depends-on ("buffer" "utils" "draw")))))
  :description "A pretty bad vi like editor")
