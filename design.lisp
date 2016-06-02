(defpackage :with-resignal.spec(:use :cl :jingoh :with-resignal))
(in-package :with-resignal.spec)

(setup :with-resignal)

(requirements-about cl)

#|
CL:SIGNAL just do signaling.
|#
#?(signal 'warning) :signals warning
#?(signal 'error) :signals error
#?(signal 'error) :never-invoke-debugger T

#|
When handler is found call it.
|#
#?(handler-bind((warning(lambda(c)
			  (print(type-of c)))))
    (signal 'warning))
:output "
WARNING "
, :ignore-warning t

#|
When control frow is not changed, some handler is called.
|#
#?(handler-bind((warning(lambda(c)
			  (declare(ignore c))
			  (princ :outer))))
    (handler-bind((warning(lambda(c)
			    (declare(ignore c))
			    (princ :inner))))
      (signal 'warning)))
:output "INNEROUTER"
, :ignore-warning t

#|
CL:WARN searches handler then print message on *ERROR-OUTPUT* then return nil.
|#
#?(warn 'warning) :signals warning

#?(let((*error-output*(make-broadcast-stream)))
    (warn "foo"))
:never-invoke-debugger t

#?(with-output-to-string(*error-output*)
     (warn "foo"))
:satisfies #`(& (stringp $result)
		(< 0 (length $result)))
, :ignore-warning t

#|
CL:ERROR searches handler then invoke debugger.
|#
#?(error 'error) :signals error

#|
NOTE! - code below is signalling WARNING, then invoke debugger.
|#
#?(error 'warning) :signals warning
#?(error 'warning) :invoke-debugger-with warning

#|
NOTE! - code below is causes TYPE-ERROR.
	Because CL:WARN accept only subtype of WARNING.
|#
#?(warn 'program-error) :signals type-error
#?(warn 'condition) :signals type-error

(requirements-about with-resignal)

#|
capturing condition.
Then resignaling another condition.
|#
#?(with-resignal((error()'program-error))
    (error 'error))
:signals program-error

#|
NOTE! - Just SIGNALING condition become actual behavior.
|#
#?(with-resignal((error()'program-error))
    (signal 'error))
:invoke-debugger-with program-error

#?(with-output-to-string(*error-output*)
    (with-resignal((warning()'warning))
      (signal 'warning)))
:satisfies #`(& (stringp $result)
		(< 0 (length $result)))
, :ignore-warning t

#|
Porpose is making better error message.
|#
#?(handler-case(with-resignal((error()'simple-error
				    :format-control "foo"))
		 (error "bar"))
    (error(c)(princ c)))
:output "foo"

#|
it is invalid that downgrade error to warning,
but upgrade warning to error.
|#
#?(with-resignal((warning()'program-error))
    (warn 'warning))
:signals program-error

#?(with-resignal((error()'warning))
    (error 'error))
:invoke-debugger-with warning
, :ignore-warning t

#|
same with CL:HANDLER-BIND, without signaling, nothing to do.
|#
#?(with-resignal((error()'simple-error :format-control "why?"))
    (+ 1 2))
=> 3

#|
same with CL:HANDLER-BIND, no binds is valid form.
|#
#?(with-resignal()(+ 1 2))
=> 3

#|
same with CL:HANDLER-BIND, compound type specifier is valid.
|#
#?(with-resignal(((not arithmetic-error)()'program-error))
     (/ 1 (parse-integer "0")))
:signals arithmetic-error
, :lazy t

#|
like CL:HANDLER-CASE's clause,
we can access captured condition when var is specified.
|#
#?(handler-case(with-resignal((error(c)'simple-error
				   :format-control(concatenate 'string
							       (simple-condition-format-control c)
							       " added string")))
		 (error "error"))
    (error(c)(princ c)))
:output "error added string"
