;; test-reporting.lisp - Test result reporting [AFFTA]

(in-package #:info.metacommunity.cltl.test)


(define-condition test-condition ()
  ((test
    :initarg :test
    :reader test-condition-test)))

(defgeneric format-test-label (test stream))

(defgeneric format-test-results (condition test stream))

(defmethod format-condtion ((condition test-condition) (stream stream))
  (let ((test (test-condition-test condition)))
    (format-test-label test stream)
    (format-test-results condition test stream)))

(define-condition test-result (test-condition)
  ((record
    :initarg :record
    :accessor test-result-record)))


(define-condition test-failed (test-result)
  ()
  (:report format-test-condition))


(defmethod format-test-condition ((condition test-failed)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test failed" stream))


(define-condition test-succeeded (test-result)
  ()
  (:report format-test-condition))


(defmethod format-test-condition ((condition test-succeeded)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test succeeded" stream))


