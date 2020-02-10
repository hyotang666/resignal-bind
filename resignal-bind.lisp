(defpackage :resignal-bind(:use :cl)
  (:export
    ;;;; main api
    #:resignal-bind
    ))
(in-package :resignal-bind)

#|
Don't use HANDLER-CASE.
Consider about when capturing warning then resignal new warning.
HANDLER-CASE can not keep control flow.
|#
(defmacro resignal-bind(binds &body body)
  (if(null binds)
    `(PROGN ,@body)
    (let((tag (gensym "TAG"))
         (var (gensym "NEW-CONDITION")))
      `(PROG(,var)
         (HANDLER-BIND,(loop :for bind :in binds
                             :collect (bind-form bind tag var))
           (RETURN(PROGN ,@body))) ; without signaling, return it.
         ,tag
         (ERROR ,var))))) ; when signaling, resignal new condition.

(defun bind-form(bind tag var)
  (destructuring-bind(old (&optional condition) new . args)bind
    (let((condition(or condition
                       (gensym"CONDITION")))
         (gnew (gensym"NEW")))
      (check-error old)
      `(,old(LAMBDA(,condition)
              (LET((,gnew ,new))
                ,@(if(not(subtypep old 'warning))
                    ;; old is ERROR or CONDITION.
                    `((CHECK-ERROR ,gnew)
                      (SETF ,var (INHERIT-CONDITION ,condition ,gnew ,@args))
                      (GO ,tag))
                    ;; old is WARNING.
                    `((IF(NOT(PROGN (CHECK-ERROR ,gnew)
                                    (SUBTYPEP ,gnew 'WARNING)))
                        ;; to be ERROR or CONDITION.
                        (PROGN (SETF ,var (INHERIT-CONDITION ,condition ,gnew ,@args))
                               (GO ,tag))
                        ;; to be WARNING.
                        (PROGN (WARN(INHERIT-CONDITION ,condition ,gnew ,@args))
                               (WHEN(FIND-RESTART 'MUFFLE-WARNING ,condition)
                                 (MUFFLE-WARNING ,condition))))))))))))

(define-condition unknown-condition(program-error type-error) ()
  (:report(lambda(c *standard-output*)
            (format t "~S: Unknown condition specified. ~S~%Typo?~:[~; or defined later?~]"
                    'resignal-bind (type-error-datum c)*compile-file-pathname*))))

(defun check-error(condition)
  (handler-case(typep '#:dummy condition)
    (error()(error 'unknown-condition :datum condition))))

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

(defun pprint-resignal-bind(stream exp &rest noise)
  (declare(ignore noise))
  (pprint-logical-block(stream exp :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream) ; operator
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :block 3 stream)
    (pprint-newline :fill stream)
    (let((bind(pprint-pop)))
      (cond
        ((atom bind)
         (format stream "~:S" bind))
        ((some #'atom bind)
         (write bind :stream stream))
        (t
          (format stream "~:<~@{~:<~^~W~^~3I ~@_~:[()~;~:*~W~]~^ ~W~^~1I~:@_~@{~W~^ ~:_~}~:>~}~:>"
                  bind))))
    (pprint-indent :block 0 stream)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :mandatory stream)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (write (pprint-pop):stream stream))))

;;; [CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)
;;; > The Lisp Pretty Printer implementation is not perfect yet.
#-clisp
(set-pprint-dispatch '(cons(member resignal-bind)) 'pprint-resignal-bind)
