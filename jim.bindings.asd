(defpackage :jim.bindings-asd
  (:use :cl :asdf))

(in-package :jim.bindings-asd)

(defsystem jim.bindings
  :version "0.0"
  :author  "Peter Elliott"
  :license "LGPLv3"
  :depends-on (:cl-reexport)
  :components ((:module "bindings"
                :components
                ((:file "keys")
                 (:file "trie")
                 (:file "package"))))
  :in-order-to ((test-op (test-op :jim.bindings/test)))
  :description "jim's keybinding system")

(defsystem jim.bindings/test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:jim.bindings :prove)
  :components ((:module "bindings"
                :components
                ((:test-file "keys-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
