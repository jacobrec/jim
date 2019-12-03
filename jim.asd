(defpackage :jim-asd
  (:use :cl :asdf))

(in-package :jim-asd)

(defsystem jim
  :version "0.0"
  :author  "Jacob Reckhard"
  :license "LGPLv3"
  :depends-on ("jbuffer" :jim.bindings)
  :components ((:module "editor"
                :components
                ((:file "utils")
                 (:file "editor" :depends-on ("utils"))
                 (:file "draw" :depends-on ("utils" "editor"))
                 (:file "api")
                 (:file "vim")
                 (:file "jim-user")
                 (:file "main" :depends-on ("utils" "draw" "editor")))))
  :description "A pretty bad vi like editor"
  :build-operation program-op
  :build-pathname "jim"
  :entry-point "jim-app:run-app")

(defsystem jim/test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:jim :prove)
  :components ((:module "editor"
                :components
                ((:test-file "movement_test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
