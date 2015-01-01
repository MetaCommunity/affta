;; test-classes.lisp - protocol classes [AFFTA]

;; FIXME: Once AFFTA test-interfaces is defined,
;;        then define tests for:
;;  1. ASSOCIATIVE-INDEX and subclasses
;;  2. TEST-SUITE and TEST registry protocol (presently mirroring ASSOCIATIVE-INDEX protocol)

(in-package #:mcicl.test)

(defgeneric test-object (test))
(defgeneric (setf test-object) (new-value test))

(defgeneric test-predicate (test))
(defgeneric (setf test-predicate) (new-value test))

(defgeneric test-summary (test))
(defgeneric (setf test-summary) (new-value test))

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

(defclass test (associative-object)
  ;; FIXME: add slot SUITE (?)
  ((object
    :initarg :object
    :accessor test-object)
   (predicate
    :initarg :predicate
    :type function
    :accessor test-predicate)
   (summary
    :initarg :summary
    :initform nil
    :accessor test-summary
    :type (or string null))
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

(define-condition test-not-found (container-condition entity-not-found)
  ()
  (:report format-condition))

(defmethod format-condition ((condition test-not-found)
                             (stream stream))
  (format stream
          "No test found for name ~S within suite ~S"
          (entity-condition-name condition)
          (object-name (container-condition-container condition))))



(defmethod test-setup-function ((test test))
  ;;
  ;; (values (or null function) boolean)
  ;;
  ;; if a function, must accept two arguments:
  ;;  1) list of parameters provided to the test
  ;;  2) test object
  ;;
  ;; function would be called before the test's primary method is
  ;; evaluated within DO-TEST
  (cond
    ((slot-boundp test 'setup-function)
     (values (%test-setup-function test) t))
    (t (values nil nil))))

(defmethod test-cleanup-function ((test test))
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
  (cond
    ((slot-boundp test 'cleanup-function)
     (values (%test-cleanup-function test) t))
    (t (values nil nil))))


(defgeneric (setf test-setup-function) (new-value test)
  (:method ((new-value function) (test test))
    (setf (%test-setup-function test) new-value))
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

(defmethod print-label ((test test) (stream stream))
  (format stream "~A ~<[~A]~>" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (print-label test stream)))

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


(defmethod print-label ((test lisp-test) (stream stream))
  (format stream "~A ~<~<[~A]~> ~<[~A]~>~>" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)
          (slot-value* test 'lambda-function)))


(defgeneric %test-suite-tests (suite))

(defgeneric test-suite-default-test-class (suite))
(defgeneric (setf test-suite-default-test-class) (new-value suite))

(defclass test-suite (simple-associative-index associative-object)
  ((default-test-class
       :initarg :default-test-class
     :initform (find-class 'lisp-test)
     :type class-designator 
     :accessor test-suite-default-test-class))
  (:metaclass associative-class)
  (:key-slot . mcicl.utils::name)
  (:default-initargs :key-function #'object-name))

(define-condition test-suite-not-found (entity-not-found)
  ()
  (:report format-condition))

(defmethod format-condition ((condition test-suite-not-found)
                             (stream stream))
  (format stream
          "No test suite found for name ~S"
          (entity-condition-name condition)))


(defmethod print-label ((object test-suite) (stream stream))
  (let ((name (ignore-errors (object-name object)))
        (%tests (ignore-errors (object-table object))))
    (format stream "~<~A (~A tests)~>" name 
            (when %tests
              (hash-table-count %tests)))))

(defmethod print-object ((object test-suite) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-label object stream)))

(defgeneric register-test-suite (instance)
  (:method ((instance test-suite))
    (register-object instance (find-class 'test-suite))))

(defgeneric find-test-suite (name &optional errorp)
  (:method ((name symbol) &optional (errorp t))
    (let  ((instance
            (find-object name (find-class 'test-suite) nil)))
      (cond
        (instance (values instance))
        (errorp (error 'test-suite-not-found :name name))
        (t (values nil))))))

(defgeneric remove-test-suite (instance)
  (:method ((instance test-suite))
    (remove-object (object-name instance) 
                   (find-class 'test-suite))))

(defgeneric map-test-suites (function)
  (:method (function)
    (map-test-suites (coerce function 'function)))
  (:method ((function function))
    (map-objects function (find-class 'test-suite))))


(defgeneric add-test (test suite)
  (:method ((test test) (suite test-suite))
    (register-object test suite)))

(defgeneric remove-test (test suite)
  (:method ((test test) (suite test-suite))
    (remove-object test suite)))

(defgeneric find-test (name suite &optional errorp)
  (:method (name (suite symbol) &optional (errorp t))
    (let ((suite (find-test-suite suite)))
      (find-test name suite errorp)))
  (:method ((name symbol) (suite test-suite) &optional (errorp t))
    (let ((test (find-object name suite nil)))
      (cond
        (test (values test))
          (errorp (error 'test-not-found
                         :name name :container suite))
          (t (values nil))))))

(defgeneric map-tests (function suite)
  (:method (function (suite test-suite))
    (map-objects function suite)))
