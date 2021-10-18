(defpackage :resignal-bind
  (:use :cl)
  (:export ;;;; main api
           #:resignal-bind))

(in-package :resignal-bind)

(declaim (optimize speed))

#|
Don't use HANDLER-CASE.
Consider about when capturing warning then resignal new warning.
HANDLER-CASE can not keep control flow.
|#

(defmacro resignal-bind (binds &body body)
  (declare (type list binds))
  (if (null binds)
      `(progn ,@body)
      (let ((tag (gensym "TAG"))
            (var (gensym "NEW-CONDITION"))
            (handlers
             (loop :repeat (length binds)
                   :collect (gensym "HANDLER"))))
        `(prog (,var)
           (flet ,(loop :for bind :in binds
                        :for ?handler in handlers
                        :collect (<handler-def> bind ?handler var tag))
             (declare (dynamic-extent (function ,@handlers))) ; SBCL needs.
             (handler-bind ,(loop :for bind :in binds
                                  :for ?handler :in handlers
                                  :collect `(,(car bind) #',?handler))
               (return (progn ,@body)))) ; without signaling, return it.
          ,tag
           (error ,var))))) ; when signaling, resignal new condition.

(defun <handler-def> (bind ?handler var tag)
  (destructuring-bind
      (old (&optional condition) new . args)
      bind
    (let ((condition (or condition (gensym "CONDITION"))) (gnew (gensym "NEW")))
      (check-error old)
      `(,?handler (,condition) (declare (optimize (safety 0)))
        (let ((,gnew ,new))
          ,@(if (not (subtypep old 'warning))
                ;; old is ERROR or CONDITION.
                `((check-error ,gnew)
                  (setf ,var (inherit-condition ,condition ,gnew ,@args))
                  (go ,tag))
                ;; old is WARNING.
                `((if (not
                        (progn (check-error ,gnew) (subtypep ,gnew 'warning)))
                      ;; to be ERROR or CONDITION.
                      (progn
                       (setf ,var (inherit-condition ,condition ,gnew ,@args))
                       (go ,tag))
                      ;; to be WARNING.
                      (progn
                       (warn (inherit-condition ,condition ,gnew ,@args))
                       (when (find-restart 'muffle-warning ,condition)
                         (muffle-warning ,condition)))))))))))

(define-condition unknown-condition (program-error type-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (format t
             "~S: Unknown condition specified. ~S~%Typo?~:[~; or defined later?~]"
             'resignal-bind (type-error-datum c) *compile-file-pathname*))))

(defun check-error (condition)
  (handler-case
      (locally ; Due to type is known in runtime.
       (declare (optimize (speed 1)))
       (typep '#:dummy condition))
    (error ()
      (error 'unknown-condition :datum condition))))

(defun inherit-condition (condition tobe &rest args)
  (loop :with instance = (make-condition tobe)
        :with args = (append args (slot-status condition))
        :with missing = '#:missing
        :for slot :in (closer-mop:class-slots (class-of instance))
        :for inherit
             = (getf args (car (closer-mop:slot-definition-initargs slot))
                     missing)
        :unless (eq missing inherit)
          :do (setf (slot-value instance
                                (closer-mop:slot-definition-name slot))
                      inherit)
        :finally (return instance)))

(defun slot-status (instance)
  (loop :for slot :in (closer-mop:class-slots (class-of instance))
        :for slot-name = (closer-mop:slot-definition-name slot)
        :when (slot-boundp instance slot-name)
          :collect (car (closer-mop:slot-definition-initargs slot))
          :and :collect (slot-value instance slot-name)))

(defun pprint-resignal-bind (stream exp &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream) ; operator
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :block 3 stream)
    (pprint-newline :miser stream)
    (let ((bind (pprint-pop)))
      (cond ((atom bind) (funcall (formatter "~:S") stream bind))
            ((some #'atom bind) (write bind :stream stream))
            (t
             (funcall
               (formatter
                "~:<~@{~:<~^~W~^~3I ~@_~:<~^~@{~W~^ ~}~:>~^ ~W~^~1I ~_~@{~W~^ ~:_~W~^ ~_~}~:>~}~:>")
               stream bind))))
    (pprint-indent :block 0 stream)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :mandatory stream)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (write (pprint-pop) :stream stream))))

;;; [CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)
;;; > The Lisp Pretty Printer implementation is not perfect yet.

#-clisp
(set-pprint-dispatch '(cons (member resignal-bind)) 'pprint-resignal-bind)
