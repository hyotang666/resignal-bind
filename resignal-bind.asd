; vim: ft=lisp et
(in-package :asdf)
(defsystem :resignal-bind
  :description "Tiny signal capturing facility."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname "README.md" *load-pathname*))
  :author "Shinich Sato"
  :depends-on (:closer-mop)
  :components ((:file "resignal-bind")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "resignal-bind"))))
 (test-system :resignal-bind.test))
