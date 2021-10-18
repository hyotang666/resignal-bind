# RESIGNAL-BIND 0.0.7

## Alternatives and differences

|     | [more-conditions] | resignal-bind |
| --- | ----------------- | ------------- |
| API | 38 symbols        | 1 symbol      |

[more-conditions]: https://github.com/scymtym/more-conditions

## Usage

```lisp
;; define simple example.
(defun enstrings (list)
  (labels ((rec (list &optional acc)
             (if (endp list)
                 (nreverse acc)
                 (rec (cdr list) (push (princ-to-string (car list)) acc)))))
    (rec list)))
=> ENSTRINGS

;; behavior
(enstrings '(1 2 3))
=> ("1" "2" "3")

;; bad case.
(enstrings '(1 2 . 3))
=> ERROR
#+clisp        *** - ENDP: A proper list must not end with 3
#+ccl          > Error: The value 3 is not of the expected type LIST.
#+ecl          Condition of type: SIMPLE-TYPE-ERROR
               In function ENDP, the value of the only argument is
                  3
               which is not of the expected type LIST
#+sbcl         The value 3 is not of type LIST.

;; Error message is not user friendly, because user used enstrings not endp.

;; for example,
(defun enstrings (list)
  (labels ((rec (seq &optional acc)
             (if (resignal-bind
                   ((error nil 'simple-error
                           :format-control \"~S: Accepts only proper list, but ~S\"
                           :format-arguments (list 'enstrings list)))
                   (endp seq))
                 (nreverse acc)
                 (rec (cdr seq) (push (princ-to-string (car seq)) acc)))))
    (rec list)))
=> ENSTRINGS

(enstrings '(1 2 . 3))
=> ERROR
ENSTRINGS: Accepts only proper list, but (1 2 . 3)

;; TIPS! - If you feel resignal-bind in the source code is annoying to read,
;;         MACROLET allows you to more pretty source code.
;; e.g.
(macrolet ((! (form)
             `(resignal-bind
               ((error nil 'simple-error
                       :format-control \"~S: Accepts only proper list, but ~S\"
                       :format-arguments (list 'enstrings list)))
               ,form)))
  (defun enstrings (list)
    (labels ((rec (seq &optional acc)
               (if (! (endp seq))
                   (nreverse acc)
                   (rec (cdr seq) (push (princ-to-string (car seq)) acc)))))
      (rec list))))

;; Why don't you use resignal-bind at refactoring stage.
```

## From developer

### Product's goal
Maybe already
### License
MIT

### Tested
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49 ; Failed.
* ECL/16.1.3
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0 ; Failed.

### Known issues.
#### CLISP
[CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

Works fine, but S-expression printed as not pretty.

#### ABCL
Works fine, but S-expression printed as not pretty.
