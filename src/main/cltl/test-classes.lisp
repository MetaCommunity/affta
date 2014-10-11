;; test-classes.lisp - protocol classes [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defgeneric test-predicate (test))

(defgeneric test-predicate-function (test))

(defgeneric test-name (test))

(defgeneric test-object (test))

(defvar %unbound-slot-label% 
  ;; FIXME: #I18N
  ;; FIXME: Stack allocate this binding and its string value
  (coerce "{Unbound}" 'simple-base-string))

;;;; Test

(defclass test (labeled-object)
  ((object
    :initarg :object
    :accessor test-object)))

(declaim (type string %unbound-slot-label%))

(defmethod format-test-label ((test test) (stream stream))
  (format stream "[~A] ~A" 
          (class-of test)
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
  ((lambda-body
    :initarg :lambda
    ;; NOTE: A CLtL compiler typically evaluates (LAMBDA ()...)
    ;; as a functional expression, rather than a list expression
    :accessor test-lambda-body)
   (lambda-function
    :accessor test-lambda-function)))


(defclass functional-test (lisp-test)
  ())


(defclass class-protocol-test (lisp-test)
  ((class 
    ;; NOTE: This slot and its accessor may be renamed within AFFTA-1.3
    ;; subsequent to development of an exacting use case for this
    ;; class' definition - such as c.f READ-WRITE-LOCK
    :initarg :class
    :accessor test-class)))



(defmethod format-test-label ((test class-protocol-test)
                              (stream stream))
  (princ-label test stream)
  (princ #\Space stream)
  (princ (test-class test) stream))
