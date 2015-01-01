;; test-reporting.lisp - Test result reporting [AFFTA]

(in-package #:mcicl.test)

;; FIXME: Merge into file test-recording.lisp

(define-condition test-condition ()
  ((test
    :initarg :test
    :reader test-condition-test)))

(defgeneric format-test-results (condition stream))

(define-condition test-result (test-condition)
  ;; The TEST-RESULT class was developed both to serve as a
  ;; condition class for test failure/success and furthermore to serve
  ;; as an effective "bucket" for test result values, as the latter
  ;; being encapsulated within a TEST-RECORD object
  ((record
    :initarg :record
    :accessor test-result-record)))

(defmethod format-condition ((condition test-result) (stream stream))
  (let ((test (test-condition-test condition)))
    (princ (class-name (class-of condition)) stream)
    (write-char #\Space stream)
    (print-label test stream)
    (write-char #\Space stream)
    (format-test-results condition stream)))

(defmethod format-test-results ((condition test-result) (stream stream))
  (let ((record (test-result-record condition)))
    (print-label (ignore-errors (test-goal record)) stream)
    (write-string " | " stream)
    (princ (ignore-errors (test-main-values record))
           stream)))


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


