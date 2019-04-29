(in-package :alexandria)

(defun %reevaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                  (ignore ()
                    :report "Retain the current value."
                    old)
                  (continue ()
                    :report "Try to redefine the constant."
                    new)))))))



(defmacro define-constant (name initial-value &key (type t) (test ''eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST is a
/function designator/ that defaults to EQL. If DOCUMENTATION is given, it
becomes the documentation string of the constant. The type may be specified with
TYPE.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(progn ,(if type `(declaim (type ,type ,name)))
          (defconstant ,name (%reevaluate-constant ',name (the ,type ,initial-value)
                                                   ,test)
            ,@(when documentation `(,documentation)))))

(defmacro definline (name lambda-list &body body)
  "Declare an inline defun, saves a line of code."
  `(progn (declaim (inline ,name))
          (defun ,name ,lambda-list ,@body)))

(defmacro defcustom (name type initial-value documentation)
  "Define a typed global variable of TYPE with INITIAL-VAUE and docstring
DOCUMENTATION."
  `(progn (declaim (type ,type ,name))
          (defvar ,name (the ,type ,initial-value) ,documentation)))
