# [Macro] WITH-RESIGNAL

## Syntax:

(WITH-RESIGNAL (bind\*) &body body) => result

## Arguments and Values:

bind := (condition-name ([var]) successor-condition-name &rest initargs)

condition-name := condition type specifier

var := symbol

successor-condition-name := lisp form which produce symbol names condition name.

initargs := initargs for constructing successor-condition-name

body := implicit progn

result := result of body

## Description:
Capturing condition like CL:HANDLER-BIND, then signaling better condition.

VAR is used when you want to access captured condition in initargs.
(e.g. want to concatenate original format-control.)
This is similar with CL:HANDLER-CASE's cluase.

## Example:
```lisp
;; define simple example.
(defun enstrings(list)
  (labels((rec(list &optional acc)
	    (if(endp list)
	      (nreverse acc)
	      (rec(cdr list)(push(princ-to-string(car list))acc)))))
    (rec list)))
=> ENSTRINGS

;; behavior
(enstrings '(1 2 3))
=> ("1" "2" "3")

;; bad case.
(enstrings '(1 2 . 3))
=> ERROR
#+clisp
*** - ENDP: A proper list must not end with 3
#+ccl
> Error: The value 3 is not of the expected type LIST.
#+ecl
Condition of type: SIMPLE-TYPE-ERROR
In function ENDP, the value of the only argument is
  3
which is not of the expected type LIST
#+sbcl
The value 3 is not of type LIST.

;; Error message is not user friendly, because user used enstrings not endp.

;; for example,
(defun enstrings(list)
  (labels((rec(seq &optional acc)
	    (if(with-resignal((error()'simple-error
			      :format-control "~S: Accepts only proper list, but ~S"
			      :format-arguments (list 'enstrings list)))
		 (endp seq))
	      (nreverse acc)
	      (rec(cdr seq)(push(princ-to-string(car seq))acc)))))
    (rec list)))
=> ENSTRINGS

(enstrings '(1 2 . 3))
=> ERROR
ENSTRINGS: Accepts only proper list, but (1 2 . 3)

;; TIPS! - If you feel with-resignal in the source code is annoying to read,
;;         MACROLET allows you to more pretty source code.
;; e.g.
(macrolet((!(form)
	    `(WITH-RESIGNAL((ERROR()'SIMPLE-ERROR
			    :FORMAT-CONTROL "~S: Accepts only proper list, but ~S"
			    :FORMAT-ARGUMENTS(LIST 'ENSTRINGS LIST)))
	       ,form)))

  (defun enstrings(list)
    (labels((rec(seq &optional acc)
	      (if(!(endp seq))
		(nreverse acc)
		(rec(cdr seq)(push(princ-to-string(car seq))acc)))))
      (rec list))))

;; Why don't you use with-resignal at refactoring stage.
```

## Affected-By:

## Side-Effects:
Condition may signaled.

## Notes:
Even if just SIGNALing, WITH-RESIGNAL upgrade it actual ERROR or WARNING.

WITH-RESIGNAL can NOT downgrade error to warning, but upgrade warning to error.

## See-Also:

## Exceptional-Situations:
When downgrading error to warning is occur, an error of type type-error is signaled.
