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

See also: `DO-TEST-SETUP'; `DO-TEST'"))


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

See also: `DO-TEST-CLEANUP'; `DO-TEST'")
  )

(defvar %unbound-slot-label% 
  ;; FIXME: #I18N
  ;; FIXME: Stack allocate this binding and its string value
  (coerce "{Unbound}" 'simple-base-string))

;;;; Test

(defclass test (labeled-object)
  ((object
    :initarg :object
    :accessor test-object)
   ;; NOTE: The following slots are defined with non-conventional
   ;; accessors, below.
   (setup-function
    :initarg :setup-function)
   (cleanup-function
    :initarg :cleanup-function)
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
       (values (test-setup-function test) t))
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
       (values (test-cleanup-function test) t))
      (t (values nil nil)))))


(declaim (type string %unbound-slot-label%))

(defmethod format-test-label ((test test) (stream stream))
  (format stream "[~A] ~A" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)))

(defmethod label ((object test))
  (with-output-to-string (s)
    (format-test-label object s)))

(defmethod print-object ((test test) stream)
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

(defclass functional-test (lisp-test)
  ;; in which the TEST-OBJECT is assumed to designate a function
  ())


(defclass functional-setf-test (functional-test)
  ;; in which the TEST-OBJECT is assumed to designate a setf form
  ;; FIXME: Develop a use case, e.g. onto CLIM multiple-value-setf
  ())


(defclass class-protocol-test (lisp-test)
  ;; in which the TEST-OBJECT is assumed to designate a class
  ;; see also: UTILS:COMPUTE-CLASS
  ;; FIXME: Develop a use case, e.g. towards a portable READ-WRITE-LOCK
  ())


