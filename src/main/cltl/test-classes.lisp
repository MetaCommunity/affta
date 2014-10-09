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
  (let ((label (labeled-object-label test)))
    (princ (or label %unnamed%)
           stream)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format-test-label test stream)))

;; test application

#+NIL
(defgeneric run-test (test &rest data &key &allow-other-keystest)
  ;; FIXME: Move documentation; remove this function; use test-protocol.lisp

  ;; FIXME: Encapsulating the test data wtihin a test object
  ;; may not be "the best approach". Test data, alternately,
  ;; may be encapsulated within a TEST-CONDITION, thus allowing for a
  ;; TEST to simply represent a functional interface independent
  ;; of test data.
  ;;
  ;; Furthermore, it may be not unwise to allow for a test's
  ;; PRE-TEST and CLEANUP procedures to be parameterized for
  ;; individual test data.
  ;;
  ;; Both of those concerns may be resolved simply with an
  ;; application of generic functions.
  ;;  1) Revise DEFTEST such that it effectively provides an interface
  ;;     onto DEFMETHOD <FOO>
  ;;  2) Ensure that a test interface's 'setup' and 'cleanup' forms
  ;;     will be evaluated witin an :AROUND method onto <FOO>
  ;;     with the actual test form being evaluted within UNWIND-PROTECT
  (:method :around ((test test) &rest data &key &allow-other-keys)
           (declare (ignore data))
     (let ((pre-test-form (test-pre-test-form test)))
       (when pre-test-form
         (funcall (test-pre-test-function test))))
     (unwind-protect
          (when (next-method-p (call-next-method)))
       (let ((cleanup-form (test-cleanup-form test)))
         (when cleanup-form
           (funcall (test-cleanup-function test)))))))


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

