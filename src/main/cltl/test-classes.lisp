;; test-classes.lisp - protocol classes [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defgeneric test-predicate (test))

(defgeneric test-predicate-function (test))
;; ^ FIXME: Remove?

(defgeneric test-name (test))

(defgeneric test-object (test))

(defgeneric test-setup-function (test)
  (:documentation "Functional test setup protocol.

Syntax:
>    TEST-SETUP-FUNCTION TEST => OBJECT, BOUND-P
* OBJECT: a FUNCTION object or NIL
* BOUND-P: a BOOLEAN value

TEST-SETUP-FUNCTION provides an interface for a test developer to
define an anonymous lambda function for purpose of performing specific
procedures to configure a test's testing environment, prior to a
test's main form.

The system supplied primary method `TEST-SETUP-FUNCTION (TEST)` first
checks whether the SETUP-FUNCTION slot is bound on the TEST object. If
the slot is bound, the method returns the value of that slot as the
OBJECT and a 'true' value for BOUND-P. If the slot is not bound, the
method returns 'false' in both of the OBJECT and BOUND-P values.

For a function OBJECT, the function must accept two arguments:
* a TEST-GOAL
* a TEST object. 

Within the method `DO-TEST :AROUND (LISP-TEST-GOAL T)`, the function
will be evaluated with the TEST-GOAL and the TEST provided ot the
method. The values returned by the function will then be stored as the
TEST-SETUP-VALUES for the TEST-RECORD returned by the containing
method.

See also: `DO-TEST-SETUP'; `DO-TEST'; `TEST-CLEANUP-FUNCTION'"))


(defgeneric test-cleanup-function (test)
  (:documentation "Functional test cleanup protocol

Syntax:
>    TEST-SETUP-FUNCTION TEST => OBJECT, BOUND-P
* OBJECT: a FUNCTION object or NIL
* BOUND-P: a BOOLEAN value

TEST-CLEANUP-FUNCTION provides an interface for a test developer to 
define an anonymous lambda function for purpose of performing specific
procedures as to \"reset\" and \"clean up\" objects defined in a
test's testing environment, following a test's main form.

The system supplied primary method `TEST-CLEANUP-FUNCTION (TEST)` first
checks whether the CLEANUP-FUNCTION slot is bound on the TEST
object. If the slot is bound, the method returns the value of that
slot as the OBJECT and a 'true' value for BOUND-P. If the slot is not
bound, the method returns 'false' in both of the OBJECT and BOUND-P
values.

For a function OBJECT, the function must accept two arguments:
* a TEST-GOAL
* a TEST object. 

Within the method `DO-TEST :AROUND (LISP-TEST-GOAL T)`, the function
will be evaluated with the TEST-GOAL and the TEST provided ot the
method. The values returned by the function will then be stored as the
TEST-CLEANUP-VALUES for the TEST-RECORD returned by the containing
method.

See also: `DO-TEST-CLEANUP'; `DO-TEST'; `TEST-SETUP-FUNCTION'")
  )

(defvar %unbound-slot-label% 
  ;; FIXME: #I18N
  ;; FIXME: Stack allocate this binding and its string value
  (coerce "{Unbound}" 'simple-base-string))

;;;; Test

(defclass test (associative-object pretty-printable-object)
  ((object
    :initarg :object
    :accessor test-object)
   ;; NOTE: The following slots are defined with non-conventional
   ;; accessors, below.
   (setup-function
    ;; FIXME: Define SETUP-FUNCTION-FORM additionally?
    ;; FIXME: Document the usage/non-usage of this slot value.
    ;;        See also: `DO-TEST-SETUP'
    :initarg :setup-function
    :accessor %test-setup-function)
   (cleanup-function
    ;; FIXME: Define CLEANUP-FUNCTION-FORM additionally?
    ;; FIXME: Document the usage/non-usage of this slot value.
    ;;        See also: `DO-TEST-CLEANUP'
    :initarg :cleanup-function
    :accessor %test-cleanup-function)
   ))

(defgeneric test-setup-function (test)
  ;;
  ;; (values (or null function) boolean)
  ;;
  ;; if a function, must accept two arguments:
  ;;  1) list of parameters provided to the test
  ;;  2) test object
  ;;
  ;; function would be called before the test's primary method is
  ;; evaluated within DO-TEST
  (:method ((test test))
    (cond
      ((slot-boundp test 'setup-function)
       (values (%test-setup-function test) t))
      (t (values nil nil)))))

(defgeneric test-cleanup-function (test)
  ;; 
  ;;
  ;; (values (or null function) boolean)
  ;;
  ;; if a function, must accept two arguments:
  ;;  1) list of parameters provided to the test
  ;;  2) test object
  ;;
  ;; function would be called within the cleanup forms of an
  ;; unwind-protect form within DO-TEST
  (:method ((test test))
    (cond
      ((slot-boundp test 'cleanup-function)
       (values (%test-cleanup-function test) t))
      (t (values nil nil)))))

(defgeneric (setf test-setup-function) (new-value test)
  (:method ((new-value function) (test test))
    (setf (%test-cleanup-function test) new-value))
  (:method ((new-value list) (test test))
    (let ((fn (compile* new-value)))
      (setf (test-setup-function test) fn))))

(defgeneric (setf test-cleanup-function) (new-value test)
  (:method ((new-value function) (test test))
    (setf (%test-cleanup-function test) new-value))
  (:method ((new-value list) (test test))
    (let ((fn (compile* new-value)))
      (setf (test-cleanup-function test) fn))))

(declaim (type string %unbound-slot-label%))

(defmethod format-test-label ((test test) (stream stream))
  ;; FIXME: Refactor onto UTILS:PRETTY-PRINTABLE-OBJECT
  (format stream "[~A] ~A" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)))

(defmethod label ((object test))
  ;; FIXME: Refactor onto UTILS:PRETTY-PRINTABLE-OBJECT
  (with-output-to-string (s)
    (format-test-label object s)))

(defmethod print-object ((test test) stream)
  ;; FIXME: Refactor onto UTILS:PRETTY-PRINTABLE-OBJECT
  (print-unreadable-object (test stream :type t :identity t)
    (format-test-label test stream)))

;;;; Functional-Test [FIXME: Rename to FUNCTIONAL-TEST]


;; FIXME/TO-DO:


#+AFFTA-2.0
(defclass application-test (test)
  ...)

#+AFFTA-2.0
(defclass lisp-application-test (application-test lisp-test)
  ...)

#+AFFTA-3.0
(defclass rootfs-test (test)
  ...)


(defclass lisp-test (test)
  ((lambda-form
    :initarg :lambda
    ;; NOTE: A CLtL compiler typically evaluates (LAMBDA () ...)
    ;; as a functional expression, rather than a list expression.
    ;; If the (LAMBA () ...) form is _quoted_ however, then the form
    ;; will be interpreted as a _list_ and may be provided for
    ;; subsequent system diagnostics.
    :accessor test-lambda-form)
   (lambda-function
    :accessor test-lambda-function)))

(defmethod shared-initialize :after ((instance lisp-test)
                                     slot-names 
                                     &rest initargs
                                     &key &allow-other-keys)
  (declare (ignore initargs))
  (when-slot-init (instance lambda-function slot-names)
                  (setf (test-lambda-function instance)
                        (compile nil (test-lambda-form instance)))))

#+NIL
(let ((inst 
       (make-instance 
        'lisp-test 
        :lambda '(lambda () (+ 2 2)))))
  (funcall (test-lambda-function  inst)))
;; => 4


(defmethod format-test-label ((test lisp-test) (stream stream))
  ;; FIXME: Refactor onto UTILS:PRETTY-PRINTABLE-OBJECT
  (multiple-value-bind (fn boundp)
      (slot-value* test 'object)
    (when boundp
      (format stream "[~A] ~A" 
              (class-name (class-of test))
              (function-name (test-object test))))))


(defgeneric %test-suite-tests (suite))

(defgeneric test-suite-default-test-class (suite))
(defgeneric (setf test-suite-default-test-class) (new-value suite))

(defclass test-suite (associative-object pretty-printable-object)
  ((%tests
    :reader %test-suite-tests
    :initform (make-hash-table :test #'eq))
   (default-test-class
       :initarg :default-test-class
     :initform (find-class 'lisp-test)
     :type class-designator 
     :accessor test-suite-default-test-class)))

(defmethod shared-initialize :around ((instance test-suite)
                                      slot-names &rest initargs
                                      &key 
                                        (print-name nil pnp)
                                        (print-label nil plp)
                                        (name nil namep))
  (let (args-changedp)
    (when namep
      (unless pnp
        (setf args-changedp t
              (getf initargs :print-name)
              (symbol-name name)))
      (unless plp
        (setf args-changedp t
              (getf initargs :print-label)
              (symbol-name name))))
    (cond
      (args-changedp (apply #'call-next-method instance slot-names initargs))
      (t (call-next-metod)))))

(defmethod print-label ((object test-suite) (stream stream))
  (let ((name (ignore-errors (object-name object)))
        (%tests (ignore-errors (%test-suite-tests object))))
    (format stream "~A (~A tests)" name 
            (when %tests
              (hash-table-count %tests)))))

(defmethod print-object ((object test-suite) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-label object stream)))

(defgeneric register-test-suite (instance container))

(defgeneric find-test-suite (name container))

(defgeneric remove-test-suite (instance container))


(defgeneric map-tests (function suite)
  (:method (function (suite test-suite))
    (map-tests (coerce function 'function) suite))
  (:method ((function function) (suite test-suite))
    (let ((tests (%test-suite-tests suite)))
      (labels ((fncall (k v)
                 (declare (ignore k))
                 (funcall function v)))
        (maphash #'fncall tests)))))

(defgeneric add-test (test suite))

(defgeneric remove-test (test suite))
