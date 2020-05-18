; vim: ft=lisp et
(in-package :asdf)
(defsystem :resignal-bind.test
 :version "0.1.2"
 :depends-on (:jingoh "resignal-bind")
 :components ((:file "resignal-bind")) :perform
 (test-op (o c)
          (declare(special args))
          (apply #'symbol-call :jingoh :examine :resignal-bind args)))
