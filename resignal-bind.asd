; vim: ft=lisp et
(in-package :asdf)
(defsystem :resignal-bind
  :description "Tiny signal capturing facility."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :licence "MIT"
  :depends-on
  (
   "closer-mop" ; wrapper for meta object protocols.
   )
  :components ((:file "resignal-bind")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "resignal-bind"))))
  (append (call-next-method)'((test-op "resignal-bind.test"))))
