(ql:quickload :prove)

(asdf:load-system :jbuffer)
(prove:run :jbuffer-test)

(asdf:load-system :jim.bindings)
(prove:run :jim.bindings/test)

(asdf:load-system :jim)
(prove:run :jim/test)
