(in-package :cl-user)
(defpackage :with-resignal(:use :cl)
  (:export
    ;;;; main api
    #:with-resignal
    ))
(in-package :with-resignal)

(eval-when(:compile-toplevel :load-toplevel)
  (defun doc(system namestring)
    (uiop:read-file-string
      (uiop:subpathname(asdf:system-source-directory(asdf:find-system system))
	namestring))))

(defmacro with-resignal(binds &body body)
  #.(doc :with-resignal "with-resignal.M.md")
  (if binds
    `(HANDLER-BIND,(mapcar #'bind-form binds)
       ,@body)
    `(PROGN ,@body)))

(defun bind-form(bind)
  (destructuring-bind(old (&optional var) new . args)bind
    (let((var(or var
		 (gensym"CONDITION")))
	 (gnew new))
      `(,old(LAMBDA(,var)
	      ,@(if(not(subtypep old 'warning))
		  ;; old is ERROR or CONDITION.
		  `((ERROR(INHERIT-CONDITION ,var ,gnew ,@args)))
		  ;; old is WARNING.
		  `((IF(NOT(SUBTYPEP ,gnew 'WARNING))
		      ;; to be ERROR or CONDITION.
		      (ERROR(INHERIT-CONDITION ,var ,gnew ,@args))
		      ;; to be WARNING.
		      (PROGN (WARN(INHERIT-CONDITION ,var ,gnew ,@args))
			     (WHEN(FIND-RESTART 'MUFFLE-WARNING ,var)
			       (MUFFLE-WARNING ,var)))))))))))

(defun inherit-condition(condition tobe &rest args)
  (loop :with instance = (make-condition tobe)
	:with args = (append args (slot-status condition))
	:for slot :in (closer-mop:class-slots(class-of instance))
	:do (setf(slot-value instance (closer-mop:slot-definition-name slot))
	      (getf args (car(closer-mop:slot-definition-initargs slot))))
	:finally (return instance)))

(defun slot-status(instance)
  (loop :for slot :in (closer-mop:class-slots(class-of instance))
	:for slot-name = (closer-mop:slot-definition-name slot)
	:when (slot-boundp instance slot-name)
	:collect(car(closer-mop:slot-definition-initargs slot))
	:and :collect(slot-value instance slot-name)))
