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

(defgeneric do-test-setup 
    #+AFFTA-1.3 (goal test)
    #-AFFTA-1.3 (params test)
  ;; FIXME: Specialize this GF to use TEST-RECORD (wrapper for bare PARAMS methods)
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

(defgeneric do-test-cleanup
    #+AFFTA-1.3 (goal test)
    #-AFFTA-1.3 (params test)
  ;; FIXME: Specialize this GF to use TEST-RECORD (wrapper for bare PARAMS methods)
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


(defgeneric do-test 
    #+AFFTA-1.3 (goal test)
    #-AFFTA-1.3 (params expect test)
  ;; FIXME: Specialize this GF to use TEST-RECORD (wrapper for bare
  ;; PARAMS methods)  
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
              ;; FIXME: revise this to use a (new) TEST-RESULT
              ;; condition, storing the PARAMS rather as a TEST-RECORD
              ;; to the respective TEST-RESULT condition
              :record 
              #-AFFTA-1.3 (list params expect results test)
              #+AFFTA-1.3 (make-test-record params expect results test)
              
              )))

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


;; prototype for test eval [AFFTA 1.2]
;;
#+PROTOTYPE
(handler-case
    (do-test '(2 2) '(4) #'expt)
  ;; ^ FIXME: use default NULL-TEST-SUITE with that DO-TEST eval
  (test-succeeded (c)
    (format* "OK ~S" c))
  (test-failed (c)
    (format* "NOT OK ~S" c)))

#+PROTOTYPE
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


(declaim (type class-designator %rest-record-class))

(defvar %test-record-class% 'test-record)

      
(defgeneric do-recorded-test (record 
                              #-AFFTA-1.4 test
                              #+AFFTA-1.4 output
                              )
  #+AFFTA-1.4
  (:method :around ((record test-record) (output synchronized-stdio-stream))
    ;; cf. Franz Inc. "Running tests in multiple threads (Lisp
    ;; processes)". The Allegro CL Test Harness

           
           ;; #+SBCL (require :sb-simple-streams)
           ;;
           ;; TO DO : xref SB-SIMPLE-STREAMS:TERMINAL-SIMPLE-STREAM
           ;;
           ;; TO DO : determine whether CCL provides a simple strams
           ;;         implementation 
           ;;
           ;; TO DO : determine whether there's an available
           ;;         portability layer for simple-streams
           ;;         implementations



           ;; BIG TO DO: Define UNION-STREAM, ANNOTATED-UNION-STREAM
           ;;
           ;; NOTE: This entails development specifically in MCi AFFTA
           ;;       and in the MCi APPLICATION system
           ;;
           ;; Goal: define a single class of "union stream" w/ a
           ;; related i/o control backend, such that can encapsulate
           ;; all of  the "Streams in CLtL2" and filter those in a
           ;; time-ordered manner onto standard POSIX file
           ;; descriptors or onto one or more files. That stream
           ;; classes, then, may be used during DO-RECORDED-TEST. 
           ;; 
           ;; Additional caveats:
           ;;  1. The "union stream" should publish an interface
           ;;     allowing a program to access each of the individual
           ;;     "Streams in CLtL2" seperately for input and/or
           ;;     output
           ;;
           ;;  2. The "union stream" class may be extended with an
           ;;     "annotated union stream" class, such that would
           ;;     annotate each line printed onto one of the
           ;;     "component streams", similar to syslog annotations
           ;;     (minus timestamps. See also: Daemontools//Multilog)
           ;;
           ;; 3. The "annotated union stream" class should allow for a
           ;;    Common Lisp application to direct all annotated
           ;;    output to a single file
           ;;
           ;; 4. In application within DO-RECORDED-TEST, the
           ;;     "union stream"  and its subclass, "annotated union
           ;;     stream" should allow for an application to
           ;;     effectively "tee" any of the "component streams"
           ;;     and/or the combined "union stream" onto an
           ;;     individual file
           ;;
           ;; 5. The test framework should "prune out" any zero-length
           ;;    files.
           ;;
           ;; 6. The "union streams" implementation shall extend of
           ;;    Allegro CL's Simple Streams framework
           ;;
           ;; 7. The "union streams" implementation should be extended
           ;;    for thread-local application within graphical Common
           ;;    Lisp programs on a Common Lisp desktop

           ;; For purpose of supporting interaction with the
           ;; developer, regardless of host platform, AFFTA may be
           ;; developed together with the MCI Application system

           ;; A "headless test session" application may be defined,
           ;; such that would:
           ;; 1. Either
           ;;    1.A Prevent that any of the input streams would be 
           ;;        interactive, xor....
           ;;    1.B Allow for the developer to <conveniently> provide a
           ;;        string or file for input to any of the respective
           ;;        input streams
           ;; 2. Record any entries to the debugger and abort tests
           ;; 3. Save all output in a file
           ;; 4. Notify the developer once a test session is concluded
           ;; 5. Provide a convenient batch test configuration syntax
           ;; 6. Be capable of running as a "stand alone" application
           ;;    in Hudson or Taylor (CI) 

           ;; Streams in CLtL2
           ;;   "POSIX friendly streams"
           ;;     *standard-input*
           ;;     *standard-output*
           ;;     *error-output*
           ;;  "Other CLtL2 output streams"
           ;;     *trace-output*
           ;;  "СLtL2 IO Streams"
           ;;     *debug-io*
           ;;     *query-io*
           ;;     *terminal-io*
           ;;
           ;; Referencing the CLHS, certainly *QUERY-IO* would have been
           ;; an interactive stream, possibly implemented onto CLIM,
           ;; on the LispM desktop.
           ;;
           ;; Orthogonally, *DEBUG-IO* may be used with an interactive
           ;; systems diagnostics application, as when the containing
           ;; Lisp host is "running in debug mode"

           ;; Streams in POSIX
           ;;   * File Descriptors
           ;;   * Terminals, Pseudoterminals, ...
           ;;  ...

           ;; Streams in a SLIME REPL [Existing precedents in CLtL2/IO]
           ;;
           ;; (type-of *STANDARD-OUTPUT*)
           ;; => SWANK-BACKEND::SLIME-OUTPUT-STREAM
           ;;
           ;; (type-of *STANDARD-INPUT*)
           ;; => SWANK-BACKEND::SLIME-INPUT-STREAM
           ;;
           ;; (eq *STANDARD-OUTPUT* *ERROR-OUTPUT*)
           ;; => t ;; "unlike a POSIX PTY", as "by default" i.e. w/o IO redirection
           ;;
           ;; (every #'(lambda (s) (typep s 'two-way-stream)) (list *debug-io* *query-io*))
           ;; => T
           ;; 
           ;; (eq *debug-io* *query-io*)
           ;; => T
           ;;
           ;; (eq *debug-io* *terminal-io*)
           ;; => T

           ;; Commentary:
    (let ((*stadnardaasdad-asdf-adsfouputopuat* (stdout-stream output))
          (*stderr---adserereerrrr-outpututut* (stderr-st
          streamoutputoutput))
          (*debug--ad-a-d--d--d--d-d-d-* (the-other-stream output))
          (888*****adfasdfasdfadfdf* (foo output)
            ))
      (call-the-next-method-!)))
    
  #-AFFTA-1.4
  (:method ((record test-record) test)
    (let ((parameters (test-record-parameters record))
          (expect (test-record-expected-values record))
          (test (test-record-test record)))
      (handler-case
          (do-test parameters expect test)
        (t (c)
          (setf (test-record-condition record) c)
          (notify (current-application) test c))))))



;; Protocol for source-inline instance tests [AFFTA 1.3+]

#+PROTOTYPE
(do-test (mktest #'EXPT (2 2) (4)))

#+AFFTA-1.3
(defmacro mktest (function (&rest args) (&rest expected-values)
                  &key
                    (predicate #'equalp)
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
                    
