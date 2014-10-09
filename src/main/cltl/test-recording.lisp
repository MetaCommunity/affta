;; test-record.lisp - test data encapsulation [AFFTA]


(in-package #:info.metacommunity.cltl.test)

(defgeneric test-record-test (test))

(defgeneric test-record-parameters (test))

(defgeneric test-record-expected-values (test))
;; ^ FIXME: This applies only to a VALUES-TEST

(defgeneric test-record-condition (test))

(defgeneric test-record-results (test))

(defgeneric test-record-setup-results (test))

(defgeneric test-record-setup-cleanup-results (test))


(defclass test-record ()
  ((test
    :initarg :test
    :accessor test-record-test)
   (parameters
    :initarg :parameters
    :accessor test-record-parameters)
   (expected-values ;; FIXME: This applies only to a VALUES-TEST
    :initarg :expect
    :accessor test-record-expected-values)
   (condition
    :initarg :condition
    :accessor test-record-condition)
   (results
    :initarg :results
    :accessor test-record-results)
   (setup-results
    :initarg :setup-results
    :initform nil
    :accessor test-record-setup-results)
   (cleanup-results
    :initarg :cleanup-results
    :initform nil
    :accessor test-record-cleanup-results)
   ))

