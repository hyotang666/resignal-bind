(defpackage :resignal-bind.spec(:use :cl :jingoh :resignal-bind))
(in-package :resignal-bind.spec)
(setup :resignal-bind)

(requirements-about cl)

;;;; Description:
;;; CL:SIGNAL
; do only signaling.
#?(signal 'warning) :signals warning
#?(signal 'error) :signals error
#-ecl
#?(signal 'error) :invokes-debugger NOT

;;; Condition handler
; When handler is found call it.
#?(handler-bind((warning(lambda(c)
			  (print(type-of c)))))
    (signal 'warning))
:outputs "
WARNING "
, :ignore-signals warning

; When control flow is not changed, nested some handlers are called.
#?(handler-bind((warning(lambda(c)
			  (declare(ignore c))
			  (princ :outer))))
    (handler-bind((warning(lambda(c)
			    (declare(ignore c))
			    (princ :inner))))
      (signal 'warning)))
:outputs "INNEROUTER"
, :ignore-signals warning

;;; CL:WARN
; searches handler then print message on *ERROR-OUTPUT* then return nil.
#?(warn 'warning) :signals warning

; does not invoke debugger.
#?(warn "foo")
:invokes-debugger NOT
, :ignore-signals warning

#?(with-output-to-string(*error-output*)
     (warn "foo"))
:satisfies #`(& (stringp $result)
		(< 0 (length $result)))
,:ignore-signals warning

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

(requirements-about resignal-bind)

;;;; Description:
; capturing condition.
; Then resignaling another condition.
#?(resignal-bind((error()'program-error))
    (error 'error))
:signals program-error

;; NOTE!
; Just SIGNALING condition become invoking debugger.
#-ecl
#?(resignal-bind((error()'program-error))
    (signal 'error))
:invokes-debugger program-error

#?(with-output-to-string(*error-output*)
    (resignal-bind((warning()'warning))
      (signal 'warning)))
:satisfies #`(& (stringp $result)
		(< 0 (length $result)))
, :ignore-signals warning

; Purpose is making better error message.
#?(handler-case(resignal-bind((error()'simple-error
				    :format-control "foo"))
		 (error "bar"))
    (error(c)(princ c)))
:outputs "foo"

; it is invalid that downgrade error to warning,
; but upgrade warning to error.
#-ecl
#?(resignal-bind((warning()'program-error))
    (warn 'warning))
:invokes-debugger program-error

#?(resignal-bind((error()'warning))
    (error 'error))
:invokes-debugger warning
, :ignore-signals warning

; same with CL:HANDLER-BIND, without signaling, nothing to do.
#?(resignal-bind((error()'simple-error :format-control "why?"))
    (+ 1 2))
=> 3

; same with CL:HANDLER-BIND, no binds is valid form.
#?(resignal-bind()(+ 1 2))
=> 3

; same with CL:HANDLER-BIND, compound type specifier is valid.
#?(resignal-bind(((not arithmetic-error)()'program-error))
     (/ 1 (parse-integer "0")))
:signals arithmetic-error
, :lazy t

; like CL:HANDLER-CASE's clause,
; we can access captured condition when var is specified.
#?(handler-case(resignal-bind((error(c)'simple-error
				   :format-control(concatenate 'string
							       (simple-condition-format-control c)
							       " added string")))
		 (error "error"))
    (error(c)(princ c)))
:outputs "error added string"

; Resignal-bind support status inheritance.
; When new condition has same slot with old condition,
; such value is inherited unless specified explicitly.
#?(handler-case(resignal-bind((error()'simple-error
				:format-arguments'(1 2)))
		 (error "~S-~S":a :b))
    (error(c)(princ c)))
:outputs "1-2" ; in this case, format-control is inherited.
               ; But format-arguments is superseded.
