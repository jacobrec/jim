(ql:quickload :prove)

(prove:run :jbuffer)
(prove:run :jbuffer-test)

(asdf:load-system :jim.bindings)
(prove:run :jim.bindings/test)
