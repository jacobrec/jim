
(defpackage :jbuffer-asd
  (:use :cl :asdf))

(in-package :jbuffer-asd)

(defsystem jbuffer
  :version "0.0"
  :author  "Peter Elliott"
  :license "LGPLv3"
  :components ((:module "jbuffer"
                :components
                ((:file "istring")
                 (:file "rope")
                 (:file "editor"))))
  :in-order-to ((test-op (test-op jbuffer-test)))
  :description "jim's text editor buffer")

(defsystem jbuffer-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:jbuffer :prove)
  :components ((:test-file "rope_test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

