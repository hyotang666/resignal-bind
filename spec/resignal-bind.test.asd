; vim: ft=lisp et
(in-package :asdf)
(defsystem :resignal-bind.test :depends-on (:jingoh "resignal-bind")
 :components ((:file "resignal-bind")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :resignal-bind)))
