;; test-protocol.lisp - AFFTA

(in-package #:test)


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
    (values nil))

  (:method (goal (test test))
    (multiple-value-bind (fn cleanup-p)
        (test-cleanup-function test)
      (when cleanup-p
        (funcall fn goal test)))))


(defgeneric do-test  (goal test)
  (:method ((goal lisp-test-goal) (test functional-test))
    (apply (test-lambda-function test)
           (test-parameters goal)))

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
 => #<TEST-RECORD TEST-SUCCEEDED [FUNCTIONAL-TEST] GEOM-SUM (3 4) =?=> (5) (EVERY=) (5.0)>
"
    (destructuring-bind (params expect 
                                &optional 
                                (predicate
                                 %default-equivalence-function%))
        goal
      (let* ((g (make-instance 'lisp-test-goal
                              :parameters params
                              :expect  expect
                              :predicate predicate))
             (p-list (mapcar #'(lambda (form)
                                 (declare (ignore form))
                                 (gensym "parameter-"))
                             params))
             (test (make-instance 'functional-test 
                                  :object test
                                  :lambda
                                  `(lambda (,@p-list)
                                     (funcall ,test ,@p-list)))))
        (do-test g test)))))


(defmethod  do-test :around ((goal lisp-test-goal) test)
  (macrolet ((record-at-phase (phase test goal record)
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
      (record-at-phase #:setup test goal record)
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
               (record-at-phase #:cleanup test goal record)))
            (expect (test-expect-state goal))
            (pred (test-predicate goal)))

        (setf (test-main-values record) results)
        
        (let ((state (make-instance 
                      (cond
                        ((funcall pred results expect) 
                         (quote test-succeeded))
                        (t (quote test-failed)))
                      :test test
                      :record record)))
          (setf (test-condition record) state)
          (signal state)
          (values record))))))



#+PROTOTYPE ;; from AFFTA-1.2 [Batch Testing] (?) ;; see README.md
(progn

  ;; setup forms

  (defun radians-to-degrees (theta)
    (* theta #.(/ 180 pi)))

  ;; test forms - deftest*

  (defsuite geometry-test-suite-1
      (:class test-suite))
  
  (deftest* radians-to-degrees-1 geometry-test-suite-1
    (:object #'radians-to-degrees)
    (:summary "Ensure...")
    (:setup-lamba ()) ;; no-op
    (:cleanup-lamba ()) ;; no-op    
    (:lambda (theta)
      (radians-to-degrees theta)))

  (defgoals radians-to-degrees-1
      (:documentation "Ensure...")
    ("Pi Radians => 180 Degrees"
     (:params-form pi)
     (:expect-form (values (coerce 180 (type-of pi))))
     (:predicate #'eq))
    ("-Pi Radians => -180 Degrees"
     (:params-form (- pi))
     (:expect-form (values (coerce -180 (type-of pi))))
     (:predicate #'eq))
    )

  (run-test (quote |Radians => Degrees (180)|)
            geometry-test-suite-1)

  (run-test-suite geometry-test-suite-1)


  ;; furthermore, to do:

  (defsuite utils-test-suite-1
      ;; TO DO: define TEST-SUITE as a sublcass of ASDF:SYSTEM
      ;; but do not ADSF/DEFSYSTEM:REGISTER-SYSTEM-DEFINITION
      ;; of a TEST-SUITE defined within DEFSUITE

      ;; support a syntax similar to ASDF:DEFSYSTEM
      (:class test-suite)
    (:default-component-class functional-test)
    (:depends-on #:info.metacommunity.cltl.utils))
  
  (in-test-suite utils-test-suite-1)

  (deftest compute-class-1 (ident)
    "Ensure..."
    (compute-class ident))
  
  (defgoals compute-class-1 
      (:documentation "Ensure...")
    ("Symbol => Class" 
     (:params-form 'string)
     (:expect-form (values (find-class 'string))))
    ("Class => Class" 
     (:params-form (find-class 'ratio))
     (:expect-form (values (find-class 'ratio)))))
  
  (run-test compute-class-1) ;; use current-suite

  (run-test-suite  utils-test-suite-1)

  )



;; Protocol for source-inline instance tests [AFFTA 1.3+]

#+PROTOTYPE
(do-test (mktest #'EXPT (2 2) (4)))

#+AFFTA-1.3
(defmacro mktest (function (&rest args) (&rest expected-values)
                  &key
                    (predicate %default-equivalence-function%)
                    (context (get-current-test-suite)))
  (with-gensym (test record)
    `(when-test-load ;; ...
      (let* ((,test (make-instance 'functional-test
                                   :function ,function))
             (,record (.... :test ,test
                            .... (quote ,args) 
                            .... (quote ,expected-values))))
        (when-test-eval ;; ...
         (do-recorded-test ,record *standard-output*))))))
                    
