;; test-classes.lisp - protocol classes [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defgeneric test-predicate (test))

(defgeneric test-predicate-function (test))

(defgeneric test-name (test))

(defgeneric test-object (test))


;;;; Test

(defclass test (labeled-object)
  ())

(defmethod format-test-label ((test test) (stream stream))
  (princ-label test stream))

(defmethod label ((object test))
  (with-output-to-string (s)
    (format-test-label object s)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format-test-label test stream)))

;;;; Values-Test [FIXME: Rename to FUNCTIONAL-TEST]

(declaim (type function %default-equivalence-function%))
(defvar %default-equivalence-function% #'equalp)
(defgeneric test-predicate (test)
  (:method ((test function))
    (values %default-equivalence-function%)))
;; ^ FIXME : Remove (?) or move into test-record.lisp

;; FIXME/TO-DO:
#+AFFTA-1.3
(defclass functional-test (test)
  ((function
    :initarg :function
    :accessor test-function)))
#+AFFTA-1.3
(defclass class-protocol-test (test)
  ((class 
    :initarg :calss
    :accessor test-protocol-class)))

#+AFFTA-2.0
(defclass application-test (test)
...)

#+AFFTA-3.0
(defclass rootfs-test (test)
...)


(defclass values-test (test)
  ((lambda-body
    :initarg :lambda
    :accessor test-body)
   (lambda-function
    :accessor test-lambda-function))

   (expect-values
    ;; FIXME: overlap onto TEST-RECORD [Remove]
    :initarg :expect
    :accessor test-expect-values)
   (predicate
    ;; FIXME: overlap onto TEST-RECORD [Remove]
    :initarg :predicate
    :initform %default-equivalence-function%
    :accessor test-predicate)))


(defmethod format-test-label :around ((test values-test) (stream stream))
  (call-next-method*)
  (format stream " => (~{ ~A~} ) ~A"
          (test-expect-values test)
          (test-predicate test)))


;;;; Diadic-Values-Test

(defclass diadic-values-test (values-test)
  ())

(defmethod format-test-label ((test diadic-values-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A ~A ~A) => (~{ ~A~} ) ~A"
          (test-predicate test)
          ;; FIXME: Update for revised TEST classes, test recording
          (test-datum-a test)
          (test-datum-b test)
          (test-expect-values test)
          (test-predicate test)))


;;;; Monadic-Values-Test

(defclass monadic-values-test (values-test)
  ())

(defmethod format-test-label ((test monadic-values-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A ~A) => (~{ ~A~} ) ~A"
          (test-predicate test)
          ;; FIXME: Update for revised TEST classes, test recording
          (test-datum test)
          (test-expect-values test)
          (test-predicate test)))


;;;; Variadic-Values-Test

(defclass variadic-values-test (values-test)
  ())

(defmethod format-test-label ((test monadic-values-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A~{ ~A~}) => (~{ ~A~} ) ~A"
          (test-predicate test)
          ;; FIXME: Update for revised TEST classes, test recording
          (test-data test)
          (test-expect-values test)
          (test-predicate test)))

