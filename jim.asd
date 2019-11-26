(defpackage :jim-asd
  (:use :cl :asdf))

(in-package :jim-asd)

(defsystem jim
  :version "0.0"
  :author  "Jacob Reckhard"
  :license "LGPLv3"
  :depends-on ("jbuffer")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "draw" :depends-on ("utils"))
                 (:file "main" :depends-on ("utils" "draw")))))
  :description "A pretty bad vi like editor")
