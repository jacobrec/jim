(defpackage rope-test
  (:use :cl :prove))
(in-package :rope-test)

(plan 2)
(is 4 4)
(is 4 5)
(finalize)
