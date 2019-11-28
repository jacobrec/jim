(defpackage :jbindings-asd
  (:use :cl :asdf))

(in-package :jbindings-asd)

(defsystem jbindings
  :version "0.0"
  :author  "Peter Elliott"
  :license "LGPLv3"
  :components ((:module "jbindings"
                :components
                ()))
  :description "jim's keybinding system")
