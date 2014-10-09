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
    (format-test-label test s)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format-test-label test stream)))

;;;; Values-Test

(declaim (type function %default-equivalence-function%))

(defvar %default-equivalence-function% #'equalp)

(defgeneric test-predicate (test)
  (:method ((test function))
    (values %default-equivalence-function%)))

(defclass values-test (test)
  ((expect-values
    :initarg :expect
    :accessor test-expect-values)
   (predicate
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
          (test-data test)
          (test-expect-values test)
          (test-predicate test)))

