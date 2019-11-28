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
  :description "jim's keybinding system")
