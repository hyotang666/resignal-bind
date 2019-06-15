; vim: ft=lisp et
(in-package :asdf)
(defsystem :resignal-bind
  :version "0.0.1"
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
(defmethod operate :around(o (c (eql (find-system "resignal-bind")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
