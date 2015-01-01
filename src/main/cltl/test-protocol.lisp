;; test-protocol.lisp - AFFTA

(in-package #:mcicl.test)


(defgeneric do-test-setup (goal test)
  (:method (goal (test function))
    (declare (ignore goal test))
    (values))

  (:method (goal (test test))
    (multiple-value-bind (fn setup-p)
        (test-setup-function test)
      (when setup-p
        (funcall fn goal test)))))

(defgeneric do-test-cleanup (goal test)
  (:method (goal (test function))
    (declare (ignore goal test))
    (values))

  (:method (goal (test test))
    (multiple-value-bind (fn cleanup-p)
        (test-cleanup-function test)
      (when cleanup-p
        (funcall fn goal test)))))

(defgeneric do-test  (goal test)
  (:method ((goal lisp-test-goal) (test lisp-test))
    (apply (test-lambda-function test)
           (funcall (the function (test-parameters-function goal)))))

  (:method ((goal list) (test function))
    "initialize a TEST-GOAL and a TEST for the FUNCTION and
GOAL specification, then call DO-TEST with the new TEST-GOAL and
TEST objects.

Syntax of GOAL list:

    ({ARG}*) ({VALUE*} [PREDICATE]

ARG, VALUE: An object; will not be evaluated
PREDICATE: A function

The GOAL list will be evaluated as to initialize a new TEST-GOAL
object. Furthemore, a FUNCTIONAL-TEST object will be initialized as to  
represent the TEST object for purpose of functional testing.

Within the method DO-TEST (LISP-TEST-GOAL FUNCTIONAL-TEST)

The TEST function will be evaluated with the set of ARG objects
providing parameters to the lambda list of the TEST function. The
values returned by the TEST function will be captured within a
multiple-value list, and the multiple value list then compared to the
list of VALUE objects. The multiple-value list of values returned by
the TEST function will be provided as the first argument to the
PREDICATE function, and the list of VALUE objects, as specified in
GOAL, will be provided as the second argument to the PREDICATE
function. 

The PREDICATE function should return a 'true' value if the
test, as represented of {GOAL, TEST}, is to be denoted as a
'successful test'. When the PREDICATE function returns a 'false'
value,  effectively it serves to indicate that the test is a 'failed
test'.

It is expected that the method dispatched to will return a TEST-RECORD
object representative of the test's evaluation.

This function is provided primarily as a convenience, as to allow a 
develope to specify a functional test effectively \"inline\" with a 
source code form to which the functional test would be applied. 

Example:

 (defun geom-sum (a b)
   (sqrt (+ (expt a 2) (expt b 2))))

 (do-test '((3 4) (5) every=) #'geom-sum)
 => #<TEST-RECORD TEST-SUCCEEDED GEOM-SUM ANONYMOUS | (5.0)>, #<TEST-SUCCEEDED {1008EF50E3}>"
    ;; FIXME ^ Include that example in the regression tests
    (destructuring-bind (params expect
                                &optional 
                                (predicate %default-equivalence-function%))
        goal
      (declare (type function-designator predicate))
      (setq predicate (coerce predicate 'function))
      (let* ((params-list (mapcar #'(lambda (form)
                                 (declare (ignore form))
                                 (gensym "parameter-"))
                             params))
             
             (test (make-instance 'lisp-test
                                  :name 
                                  (intern-formatted "Anonymous Test ~A"
                                                    (function-name predicate))
                                  :object test
                                  :predicate predicate
                                  :lambda
                                  `(lambda (,@params-list)
                                     (funcall ,test ,@params-list))))
             (g (make-instance 'lisp-test-goal
                               :test test
                               :name
                               (intern-formatted "Anonymous Test Goal ~A =[~A]=> ~A"
                                                    params 
                                                    (function-name predicate)
                                                    expect)
                               :params params
                               :params-function (coerce `(lambda ()
                                                           (list ,@params))
                                                        'function)
                               :expect  expect
                               :expect-function (coerce `(lambda ()
                                                           (list ,@expect))
                                                        'function))))
        (do-test g test)))))


(defmethod  do-test :around ((goal lisp-test-goal) test)
  ;; FIXME: Add more handling for errors, BREAK, CERROR, etc
  (macrolet ((recorded-run (phase test goal record)
               (let ((app (intern (format* "~A-~A"
                                           (quote #:do-test)
                                           phase)))
                     (rec (intern (format* "~A-~A-~A"
                                           (quote #:test)
                                           phase
                                           (quote #:values)))))
                 (with-gensym (%test %goal %record results)
                 `(let* ((,%test ,test)
                         (,%goal ,goal)
                         (,%record ,record)
                         (,results))
                    (unwind-protect
                         (setq ,results
                               (with-flagged-eval ,%record
                                 (multiple-value-list 
                                  (,app ,%goal ,%test))))
                      (setf (,rec ,%record) ,results))))))

             (with-flagged-eval (record &body body)
               (with-gensym (c bounce %record)
                 `(macrolet
                      ((process-c (,c ,bounce ,%record)
                         `(progn 
                            (setf (test-condition ,,%record) ,,c)
                            (,,bounce ,,c))))
                    (handler-case 
                        (progn ,@body)
                      (error (,c) (process-c ,c error ,record))
                      (warning (,c) (process-c ,c warn ,record))
                      (condition (,c) (process-c ,c signal ,record)))))))

    (let ((record (ensure-test-record goal test)))
      ;; FIXME: Note test setup function return values stored in RECORD
      (recorded-run #:setup test goal record)
      (let ((results
             (unwind-protect
                  (with-flagged-eval record
                    (multiple-value-list (call-next-method)))
               ;; Note that method dispatching, by defualt,
               ;; would not encapsulate CALL-NEXT-METHOD within
               ;; an UNWIND-PROTECT form.
               ;;
               ;; Note also that the TEST-CLEANUP-VALUES property will
               ;; be set onto the RECORD object, before the
               ;; TEST-RESULTS property is set 
               
               ;; FIXME: Note test cleanup function return values stored in RECORD
               (recorded-run #:cleanup test goal record)))
            (expect (funcall (the function (test-expect-state-function goal))))
            (pred (test-predicate test)))

        (setf (test-main-values record) results)
        
        (let ((state (make-instance 
                      (cond
                        ;; FIXME: Document the syntax and order of arguments 
                        ;; to the test-predicate function
                        ((funcall (the function pred) results expect)
                         (quote test-succeeded))
                        (t (quote test-failed)))
                      :test test
                      :record record)))
          (setf (test-condition record) state)
          (signal state)
          (values record state))))))


