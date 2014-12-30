;; test-interface.lisp - interface [AFFTA]

(in-package #:info.metacommunity.cltl.test)


#+PROTOTYPE ;; from AFFTA-1.2 [Batch Testing] (?) ;; see README.md
(progn

  ;; setup forms

  (defun radians-to-degrees (theta)
    (* theta #.(/ 180 pi)))

  ;; test forms - deftest*

  (defsuite geometry-test-suite-1
      (:class test-suite))
  
  (deftest radians-to-degrees-1 geometry-test-suite-1
    (:object #'radians-to-degrees)
    (:summary "Ensure...")
    (:setup-lamba ()) ;; no-op
    (:cleanup-lamba ()) ;; no-op    
    (:lambda (theta)
      (radians-to-degrees theta)))

  (in-test-suite geometry-test-suite-1) ;; X

  (defgoals radians-to-degrees-1 (...)
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
    (:default-test-class lisp-test)
    (:depends-on #:info.metacommunity.cltl.utils) ;;;; ?
   )
  
  (in-test-suite utils-test-suite-1) ;; X

  (deftest compute-class-1 (ident)
    "Ensure..."
    (compute-class ident))
  
  (defgoals simple-goals-1 (compute-class-1  )
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
      (let* ((,test (make-instance 'lisp-test
                                   :function ,function))
             (,record (.... :test ,test
                            .... (quote ,args) 
                            .... (quote ,expected-values))))
        (when-test-eval ;; ...
         (do-recorded-test ,record *standard-output*))))))
                    


(defmacro defsuite (name &rest properties
                           &aux (default-class 'test-suite)
                             (default-test-class 'lisp-test))
  (let ((p (copy-list properties)))
    (labels ((format-spec (spec)
               (destructuring-bind (name . value) spec
                 (declare (ignore name))
                 (cond
                   ((cdr value) (values value))
                   (t (values (car value))))))
             (find-spec (name &optional default)
               (let ((spec (assoc name p :test #'eq)))
                 (cond 
                   (spec (setf p (delete spec p :test #'eq))
                         (values (format-spec spec) t))
                   (t (values default nil)))))
             (format-properties ()
               (mapcan (lambda (spec)
                         (multiple-value-bind (name . value) spec
                           (cond
                             ((cdr value) spec)
                             (t (list name (car value))))))
                       p)))
      (with-gensym (class instance dtc)
        `(let* ((,class (quote ,(find-spec :class (find-class default-class))))
                ;; note that all initarg values except for :CLASS, :NAME,
                ;; :DEFAULT-TEST-CLASS  will be evaluated
                ;; (FIXME: This is "cheap")
                (,dtc (quote ,(find-spec :default-test-class 
                                         (find-class default-test-class))))
                (,instance (make-instance ,class 
                                          :name (quote ,name)
                                          :default-test-class ,dtc
                                          ,@(format-properties))))
           (register-test-suite ,instance)
           (values ,instance))))))


;; (defsuite foo-suite)
;; (defsuite foo-suite (:default-test-class lisp-test))

;; (defclass foo-test-suite (test-suite) ())
;; (defsuite foo-suite (:class foo-test-suite))


(declaim (type test-suite *test-suite*))
(defvar *test-suite*)
(setf (documentation *test-suite* 'variable)
      "Default test suite, within the active lexical environment.

See also: 
* `TEST-SUITE' [Class]
* `DEFSUITE' [Macro]
* `IN-TEST-SUITE' [Macro]
* `FIND-TEST-SUITE' [Function]")


(defmacro in-test-suite (name)
  "Specify that the test suite named NAME will be bound to
`*TEST-SUITE*' within the active lexical environment.

NAME will not be evaluated

See also: 
* `TEST-SUITE' [Class]
* `DEFSUITE' [Macro]
* `IN-TEST-SUITE' [Macro]
* `FIND-TEST-SUITE' [Function]"
  ;; FIXME: Try to ensure IN-TEST-SUITE will be applied local to:
  ;; 1. evaluation of a file
  ;; 2. evaluation of forms from an input stream
  ;;
  ;; Consider defining *TEST-SUITE* as a  thread-local variable (TO DO)
  ;; for this purpose
  `(setq *test-suite* (find-test-suite (quote ,name))))


#+TO-DO
(defmacro deftest (name (&optional (suite *test-suite*))
                   &rest properties)
  ;; mirror DEFSUITE FORMAT-SPEC, FIND-SPEC, FORMAT-PROPERTIES here
  
  ;; ...
  )

#+TO-DO
(defmacro defgoals (name (test &optional (suite *test-suite*))
                    &rest properties)
  ;; mirror DEFSUITE FORMAT-SPEC, FIND-SPEC, FORMAT-PROPERTIES here  

  )
