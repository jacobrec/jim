(defpackage :jedit-asd
  (:use :cl :asdf))

(in-package :jedit-asd)

(defsystem jedit
  :version "0.0"
  :author  "Jacob Reckhard"
  :depends-on ("trivial-shell")
  :components ((:module "src"
                :components
                ((:file "buffer")
                 (:file "draw" :depends-on ("buffer"))
                 (:file "main" :depends-on ("buffer" "draw")))))
  :description "A pretty bad vi like editor")
