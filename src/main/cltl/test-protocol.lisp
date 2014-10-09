(in-package #:test)


(defgeneric test-setup-function (test)
  ;; trivial test setup protocol
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
      (values nil nil))))

(defgeneric test-cleanup-function (test)
  ;; trivial test cleanup protocol
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
      (values nil nil))))

(defgeneric do-test-setup (params test)
  (:method (params (test function))
    (declare (ignore params test))
    (values nil))

  (:method (params (test test))
    ;; a trivial system-supplied primary method
    ;; dispatching to a seperate function
    ;; only when test has a SETUP-FUNCTION

    ;; FIXME: Alternately, ensure that DEFTEST
    ;; defines a primary method onto DO-TEST-SETUP
    ;; when a :SETUP form is provided
    (multiple-value-bind (fn setup-p)
        (test-setup-function test)
      (when setup-p
        (funcall fn params test)))))

(defgeneric do-test-cleanup (params test)
  (:method (params (test function))
    (declare (ignore params test))
    (values nil))

  (:method (params (test test))
    ;; a trivial system-supplied primary method
    ;; dispatching to a seperate function
    ;; only when test has a CLEANUP-FUNCTION

    ;; FIXME: Alternately, ensure that DEFTEST
    ;; defines a primary method onto DO-TEST-CLEANUP
    ;; when a :cleanup form is provided
    (multiple-value-bind (fn cleanup-p)
        (test-cleanup-function test)
      (when cleanup-p
        (funcall fn params test)))))


(defgeneric do-test (params expect test)
  (:method :around (params expect test)
           (do-test-setup params test) ;; frobbed :BEFORE method
           (let ((results
                  (unwind-protect
                       (multiple-value-list (call-next-method))
                    ;; Note that method dispatching, by defualt,
                    ;; does not encapsulate CALL-NEXT-METHOD within
                    ;; an UNWIND-PROTECT form
                    (do-test-cleanup params test)))
                 (pred (test-predicate test)))
             (signal 
              ;; FIXME [CLtL3?] SIGNAL should accept a CLASS object
              (cond
                ;; FIXME: Also store the results of the predicate function
                ;; in the TEST-RECORD (TO DO) object
                ((funcall pred results expect) (quote test-succeeded))
                (t (quote test-failed)))
              :test test
              :parameters params
              :results results)))

  (:method ((params list) (expect list)
            (test function))
    (declare (ignore expect))
    (apply test params))

  (:method ((params list) (expect list)
            (test diadic-values-test))
    (declare (ignore expect))
    (destructuring-bind (a b) params
      (funcall (test-main-function test)
               a b)))

  (:method ((params list) (expect list)
            (test monadic-values-test))
    (declare (ignore expect))
    (destructuring-bind (a) params
      (funcall (test-main-function test)
               a)))

  (:method ((params list) (expect list)
            (test variadic-values-test))
    (declare (ignore expect))
    (apply (test-main-function test) params))


  #+TO-DO
  (:method ((params list) (expect symbol)
            (test expect-condition-test))

    )

  )


;; prototype for test eval:
;;
#+PROTOTYPE
(handler-case
    (do-test '(2 2) '(4) #'expt)
  (test-succeeded (c)
    (format* "OK ~S" c))
  (test-failed (c)
    (format* "NOT OK ~S" c)))
      
