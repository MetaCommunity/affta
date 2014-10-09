(in-package #:test)


(defgeneric do-test (params expect test)
  (:method :around ((params list)
                    (expect list)
                    (test values-test))
           (let ((results (multiple-value-list
                           (call-next-method)))
                 (pred (test-predicate-function test)))
             (signal (cond
                       ((funcall pred results expect)
                        (find-class 'test-succeeded))
                       (t (find-class 'test-failed)))
                     :test test
                     :parameters (list a b)
                     :results results)))

  (:method ((params list) (expect list)
            (test diadic-parameters-test))
    (declare (ignore expect))
    (destructuring-bind (a b) params
      (funcall (test-main-function test)
               a b)))

  (:method ((params list) (expect list)
            (test monadic-parameters-test))
    (declare (ignore expect))
    (destructuring-bind (a) params
      (funcall (test-main-function test)
               a)))

  (:method ((params list) (expect list)
            (test variadic-parameters-test))
    (declare (ignore expect))
    (apply (test-main-function test) params)))
