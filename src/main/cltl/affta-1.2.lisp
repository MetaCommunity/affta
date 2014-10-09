(in-package #:test)


(defgeneric do-test-setup (params test)
  (:method :around (params (test test))
    ;; a trivial system-supplied primary method
    ;; dispatching to a seperate function
    ;; only when test has a SETUP-FUNCTION

    ;; FIXME: Alternately, ensure that DEFTEST
    ;; defines a primary method onto DO-TEST-SETUP
    ;; when a :SETUP form is provided
    (let ((fn (test-setup-function test)))
      (when fn
        (funcall fn params test)))))

(defgeneric do-test-cleanup (params test)
  (:method :around (params (test test))
    ;; a trivial system-supplied primary method
    ;; dispatching to a seperate function
    ;; only when test has a CLEANUP-FUNCTION

    ;; FIXME: Alternately, ensure that DEFTEST
    ;; defines a primary method onto DO-TEST-CLEANUP
    ;; when a :cleanup form is provided
    (let ((fn (test-cleanup-function test)))
      (when fn
        (funcall fn params test)))))


(defgeneric do-test (params expect test)
  (:method :around ((params list)
                    (expect list)
                    (test values-test))
           (do-test-setup params test) ;; frobbed :BEFORE method
           (let ((results
                  (unwind-protect
                       (multiple-value-list (call-next-method))
                    ;; Note that method dispatching, by defualt,
                    ;; does not encapsulate CALL-NEXT-METHOD within
                    ;; an UNWIND-PROTECT form
                    (do-test-cleanup params test)))
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


;; prototype for test eval:
;;
;; (do-test '(2 2) '(4) (mktest #'expt))
