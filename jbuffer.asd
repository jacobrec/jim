
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
  :description "jim's text editor buffer")
