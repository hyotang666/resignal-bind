(defpackage :resignal-bind.spec(:use :cl :jingoh :resignal-bind)
  (:import-from #:resignal-bind #:pprint-resignal-bind))
(in-package :resignal-bind.spec)
(setup :resignal-bind)

(requirements-about cl)

;;;; Description:
;;; CL:SIGNAL
; do only signaling.
#?(signal 'warning) :signals warning
#?(signal 'error) :signals error
; CLHS say "If the condition is not handled, signal returns nil."
#-ecl
#?(signal 'error) :invokes-debugger NOT
; NOTE!
; CLHS never say no handlers in top level.
; ECL has serious-condition handler in top level.
#+ecl
#?(signal 'error) :invokes-debugger error
#+ecl
#?(signal 'serious-condition) :invokes-debugger serious-condition
#+ecl
#?(signal 'condition) => NIL

;;; Condition handler
; When handler is found call it.
#?(handler-bind ((error (lambda (c) (print (type-of c)))))
  (signal 'error))
:outputs "
ERROR "
,:ignore-signals error

; When control flow is not changed, nested some handlers are called.
#?(handler-bind ((error (lambda (c) (declare (ignore c)) (princ :outer))))
  (handler-bind ((error (lambda (c) (declare (ignore c)) (princ :inner))))
    (signal 'error)))
:outputs "INNEROUTER"
, :ignore-signals error

;;; CL:WARN
; searches handler then print message on *ERROR-OUTPUT* then return nil.
#?(warn 'warning) :signals warning

; does not invoke debugger.
#?(warn "foo")
:invokes-debugger NOT
, :ignore-signals warning

;; NOTE!
; code below is causes TYPE-ERROR.
; Because CL:WARN accept only subtype of WARNING.
#?(warn 'program-error) :signals type-error
#?(warn 'condition) :signals type-error

;;; CL:ERROR
; searches handler then invoke debugger.
#?(error 'error) :signals error

;; NOTE!
; code below is signalling WARNING, then invoke debugger.
#?(error 'warning) :signals warning
#?(error 'warning) :invokes-debugger warning

;;; CL:HANDLER-BIND
;; NOTE!
; CLHS say "type -- a type specifier", not limited only condition.
#?(handler-bind ((integer #'print))
    (concatenate 'string
		 "above"
		 "integer"
		 "is"
		 "valid"))
=> "aboveintegerisvalid"
,:test equal

(requirements-about resignal-bind :doc-type function)

;;;; Description:
; capturing condition.
; Then resignaling another condition.
#?(resignal-bind ((error nil 'program-error))
    (error 'error))
:signals program-error

;; NOTE!
; Just SIGNALING condition become invoking debugger.
#?(resignal-bind ((error nil 'program-error))
    (signal 'error))
:invokes-debugger program-error

; Purpose is making better error message.
#?(handler-case
      (resignal-bind ((error nil 'simple-error :format-control "foo"))
        (error "bar"))
    (error (c)
      (princ c)))
:outputs "foo"

; it is invalid that downgrade error to warning,
; but upgrade warning to error.
#?(resignal-bind ((warning nil 'program-error)) (warn 'warning))
:invokes-debugger program-error

#?(resignal-bind ((error nil 'warning)) (error 'error))
:invokes-debugger warning
, :ignore-signals warning

; same with CL:HANDLER-BIND, without signaling, nothing to do.
#?(resignal-bind ((error nil 'simple-error :format-control "why?"))
    (+ 1 2))
=> 3

; same with CL:HANDLER-BIND, no binds is valid form.
#?(resignal-bind () (+ 1 2))
=> 3

; same with CL:HANDLER-BIND, compound type specifier is valid.
#?(resignal-bind (((not arithmetic-error) () 'program-error))
    (/ 1 (parse-integer "0")))
:signals arithmetic-error
, :lazy t

; like CL:HANDLER-CASE's clause,
; we can access captured condition when var is specified.
#?(handler-case
    (resignal-bind
        ((error (c) 'simple-error
            :format-control (concatenate 'string
                                         (simple-condition-format-control c)
                                         " added string")))
      (error "error"))
    (error (c)
      (princ c)))
:outputs "error added string"

; Resignal-bind support status inheritance.
; When new condition has same slot with old condition,
; such value is inherited unless specified explicitly.
#?(handler-case
      (resignal-bind ((error nil 'simple-error :format-arguments '(1 2)))
        (error "~S-~S" :a :b))
    (error (c)
      (princ c)))
:outputs "1-2" ; in this case, format-control is inherited.
               ; But format-arguments is superseded.

(requirements-about PPRINT-RESIGNAL-BIND :doc-type function
                    :around (let((*print-pretty* t))
                              (call-body)))

;;;; Description:

#+syntax
(PPRINT-RESIGNAL-BIND stream exp &rest noise) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; noise := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(pprint-resignal-bind nil '(resignal-bind))
:outputs "(RESIGNAL-BIND)"

#?(pprint-resignal-bind nil '(resignal-bind nil))
:outputs "(RESIGNAL-BIND ())"

#?(pprint-resignal-bind nil '(resignal-bind nil nil))
:outputs "(RESIGNAL-BIND ()
  NIL)"

#?(pprint-resignal-bind nil '(resignal-bind nil hoge))
:outputs "(RESIGNAL-BIND ()
  HOGE)"

#?(pprint-resignal-bind nil '(resignal-bind (a)))
:outputs "(RESIGNAL-BIND (A))"

#?(pprint-resignal-bind nil '(resignal-bind (nil)))
:outputs "(RESIGNAL-BIND (NIL))"

#?(pprint-resignal-bind nil '(resignal-bind ((nil))))
:outputs "(RESIGNAL-BIND ((NIL)))"

#?(pprint-resignal-bind nil '(resignal-bind ((a nil))))
:outputs "(RESIGNAL-BIND ((A ())))"

#?(pprint-resignal-bind nil '(RESIGNAL-BIND ((ERROR () 'SIMPLE-ERROR
                                               :FORMAT-CONTROL "Missing initform.~%~S"
                                               :FORMAT-ARGUMENTS(LIST SLOT hoge)))
                               form))
:outputs
"(RESIGNAL-BIND ((ERROR () 'SIMPLE-ERROR
                  :FORMAT-CONTROL \"Missing initform.~%~S\"
                  :FORMAT-ARGUMENTS (LIST SLOT HOGE)))
  FORM)"
