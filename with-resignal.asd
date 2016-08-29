; vim: ft=lisp et
(in-package :asdf)
(defsystem :with-resignal
  :in-order-to((test-op(test-op :with-resignal-test)))
  :depends-on (:closer-mop :nitch-util)
  :components ((:file "with-resignal")))

(defsystem :with-resignal-test
  :depends-on (:jingoh :with-resignal)
  :components ((:file "design"))
  :perform(test-op(o s)
            (uiop:symbol-call :jingoh 'report)))
