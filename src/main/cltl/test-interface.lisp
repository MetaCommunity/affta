;; test-interface.lisp - interface [AFFTA]

(in-package #:mcicl.test)

#+NIL ;; Regression test onto ASSOCIATIVE-INDEX
(progn

(defsuite frob)

(deftest frob-1 (frob) (:lambda () (break "FROB")))

;; test integration with instance indexing protocol
(compute-key (deftest frob-1 (frob) (:lambda () (break "FROB")))
             (find-test-suite 'frob))
;; => FROB-1 ;; OK

;; test registry for test object in test suite
(let ((suite (find-test-suite 'frob))
      (tests))
  (register-object  (deftest frob-1 (frob) (:lambda () (break "FROB")))
                    suite)
  (map-tests #'(lambda (test) (setq tests (push test tests))) suite)
  (values (length tests) (and tess (object-name (car tests))))
  )
;; => 1, FROB-1 ;; OK

(hash-table-count (object-table (find-test-suite 'frob)))
;; => 1


)

#+PROTOTYPE ;; from AFFTA-1.2 [Batch Testing] (?) ;; see README.md
(progn

  ;; setup forms

  (defun radians-to-degrees (theta)
    (* theta #.(/ 180 pi)))

  ;; test forms - deftest*

  (defsuite geometry-test-suite-1
      (:class test-suite))
            

 (macroexpand-1 (quote
  (deftest radians-to-degrees-1 (geometry-test-suite-1)
    (:object #'radians-to-degrees)
    (:summary "Verify conversion of radians to degrees")
    ;; (:setup-lamba ()) ;; no-op
    ;; (:cleanup-lamba ()) ;; no-op    
    (:lambda (theta)
      (radians-to-degrees theta))
    (:predicate #'set=)
    )
 ))

  (describe (find-test 'radians-to-degrees-1 'geometry-test-suite-1))

  (in-test-suite geometry-test-suite-1) ;; X

  (defgoals radians-to-degrees-1.1 (radians-to-degrees-1 geometry-test-suite-1)
    (:summary "Verify conversion onto factors of PI")
    (:goal pluspi-180
           (:summary "Pi Radians => 180 Degrees")
           (:params pi)
           (:expect (values (coerce 180 (type-of pi)))))
    (:goal minuspi-minus180
           (:summary "-Pi Radians => -180 Degrees")
           (:params (- pi))
           (:expect (values (coerce -180 (type-of pi))))))

  (map-goals #'print (find-goal 'radians-to-degrees-1.1
                                'geometry-test-suite-1))

  (let ((g (find-goal 'pluspi-180 (find-goal 'radians-to-degrees-1.1 
                                             'geometry-test-suite-1))))
    (describe g)
    (funcall (test-parameters-function g ))
    #+TO-DO (run-test g)
    )

  (run-test '(geometry-test-suite-1 pluspi-180))
  (run-test '(geometry-test-suite-1 minuspi-minus180 )

  (run-test-suite 'geometry-test-suite-1)

  ;; FIXME: Define a *BREAK-ON-FAILURE* flag 
  ;; and implement within DO-TEST (LISP-TEST-GOAL T)

  ;; second prototype:

  (defsuite utils-test-suite-1
      ;; TO DO: define TEST-SUITE as a sublcass of ASDF:SYSTEM
      ;; but do not ADSF/DEFSYSTEM:REGISTER-SYSTEM-DEFINITION
      ;; of a TEST-SUITE defined within DEFSUITE

      ;; support a syntax similar to ASDF:DEFSYSTEM
      (:class test-suite)
    ;; N/A handle system dependencies in system definition :
    #+NIL (:depends-on #:info.metacommunity.cltl.utils)
   )

   (find-test-suite 'utils-test-suite-1)  

  (in-test-suite utils-test-suite-1) ;; X

 (deftest compute-class-1 (utils-test-suite-1)
    (:object #'compute-class)
    (:summary "Test evaluation of COMPUTE-CLASS")
    (:lambda (ident)
      (values (compute-class ident) ident))
    (:predicate #'set-eq))

  (find-test 'compute-class-1 'utils-test-suite-1)

  (defgoals simple-goals-1 (compute-class-1 utils-test-suite-1)
    (:summary "Test evaluation of COMPUTE-CLASS")
    (:goal symbol-to-class
           (:class lisp-test-goal)
           (:summary "Compute class for symbol class name" )
           (:params 'string)
           (:expect (values (find-class 'string) 'string)))
    (:goal class-as-class
           (:class lisp-test-goal)
           (:summary "Ensure identity operation for class arg")
           (:params (find-class 'ratio))
           (:expect (let ((c (find-class 'ratio)))
                      (values c c)))))
  
  (run-test 'compute-class-1) ;; use current-suite (?)

  (run-test-suite  'utils-test-suite-1)

  )
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
                    

(defun find-property (name set &optional default)
  "Return the first property element named NAME from the associative
property list SET. Comparison of NAME onto SET will be performed
with `EQ'

If no such property exists, return DEFAULT"
  (declare (type symbol name) (type list set))
  (or (assoc name set :test #'eq)
      default))


(defun nfind-property (name set &optional default)
"Return the first property element named NAME from associative
property list SET. Comparison of NAME onto SET will be performed
with `EQ'. If not found, return the value of DEFAULT.

If found, this function will destructively modify SET as to delete the
property element. The second return value will then be the value of
SET, destructively modified if NAME was found

If not found, the value SET will be returned as the second return
value, unmodified.

See also: `nget-property', `nget-property*'"

  (declare (type symbol name) (type list set)
           (values t list))
  (let ((it (assoc name set :test #'eq)))
    (cond
      (it (values it (delete it set :test #'eq)))
      (t (values default set)))))

(defun format-property (spec)
  "Format a variably atomic/list type property list element.

This function returns the formatted element and the name of the
element.

Examples:

 (format-property (:foo)) => nil, :foo
 (format-property (:foo a)) => a, :foo
 (format-property (:foo . a)) => a, :foo
 (format-property (:foo a b)) => (a b), :foo
 (format-property (:foo . a b)) => (a b), :foo

See also: `format-property*', `map-properties'" 
  (declare (type list spec)
           (values t symbol))
  (destructuring-bind (name . value) spec
    (cond
      ((and (consp value) (cdr value))
       (values value name))
      ((consp value) 
       (values (car value) name))
      (t (values value name)))))

(defun format-property* (spec)
  "Format a list type propert list element

This function returns the formatted element and the name of the
element.

Examples:

 (format-property (:foo)) => nil, :foo
 (format-property (:foo a)) => (a), :foo
 (format-property (:foo . a)) => a, :foo
 (format-property (:foo a b)) => (a b), :foo
 (format-property (:foo . a b)) => (a b), :foo


See also: `format-property', `map-properties*'" 
  (declare (type list spec)
           (values t symbol))
  (destructuring-bind (name . value) spec
    (values value name)))
    
(defun map-properties (set)
  (mapcan #'(lambda (spec)
              (multiple-value-bind (value name)
                  (format-property spec)
                (list name value)))
          set))

(defun map-properties* (set)
  (mapcan #'(lambda (spec)
              (multiple-value-bind (value name)
                  (format-property* spec)
                (list name value)))
          set))

(defmacro nget-property (indicator where &optional default)
  "Return a plist formatted property element named NAME from the
associative property list denoted by WHERE, destructively mofidying
the property list for the removed element.

The property element will be formatted with `format-property'

If no such property exists, return DEFAULT

See also: `nfind-property'"

  (with-gensym (prop %where)
    `(multiple-value-bind (,prop ,%where)
         (nfind-property ,indicator ,where)
       (cond
         (,prop
          (setf ,where ,%where)
          (values (format-property ,prop)))
         (t (values ,default))))))

(defmacro nget-property* (indicator where &optional default)
  "Return a plist formatted property element named NAME from the
associative property list denoted by WHERE, destructively mofidying
the property list for the removed element.

The property element will be formatted with `format-property'

If no such property exists, return DEFAULT

See also: `nfind-property'"

  (with-gensym (prop %where)
    `(multiple-value-bind (,prop ,%where)
         (nfind-property ,indicator ,where)
       (cond 
         (,prop
          (setf ,where ,%where)
          (values (format-property* ,prop)))
         (t (values ,default))))))

;; instance tests (associative property list model) -- nget-property
;; (let ((p '((:a 1) (:b 2)))) (values (nget-property :a p) p))
;; => 1, ((:B 2))

;; instance tests (associative property list model) -- nget-property*
;; (let ((p '((:a 1) (:b 2)))) (values (nget-property* :a p) p))
;; => (1), ((:B 2))


;;; DEFSUITE

(defmacro defsuite (name &rest properties
                    &aux (default-suite-class 'test-suite)
                     (default-test-class 'lisp-test) 
                    &environment env)
  (setq properties (copy-list properties))
  (with-gensym (class instance dtc)
    `(let* ((,class (quote ,(nget-property :class properties
                                           (find-class default-suite-class env))))
            ;; note that all initarg values except for :CLASS, :NAME,
            ;; :DEFAULT-TEST-CLASS  will be evaluated
            ;; (FIXME: This is "cheap")
            (,dtc (quote ,(nget-property :default-test-class  
                                         properties
                                         (find-class default-test-class))))

            (,instance (make-instance (compute-class ,class)
                                      :name (quote ,name)
                                      :default-test-class ,dtc
                                      ,@(map-properties properties))))
       (register-test-suite ,instance)
       (values ,instance))))


;; (defsuite foo-suite)
;; (defsuite foo-suite (:default-test-class lisp-test))

;; (defclass foo-test-suite (test-suite) ())
;; (defsuite foo-suite (:class foo-test-suite))


;;; *TEST-SUITE*

(declaim (type test-suite *test-suite*))
(defvar *test-suite*)
(setf (documentation '*test-suite* 'variable)
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

;;; DEFTEST

(defconstant* %unspecified% (make-symbol "%UNSPECIFIED%"))

(defmacro deftest (name (&optional (suite *test-suite* suitep))
                   &rest properties &environment env)
  (setq properties (copy-list properties))
  (let* ((lp (nget-property :lambda properties %unspecified%))
         (ob (nget-property :object properties)) ;; will be evaluted
         (c (nget-property :class properties %unspecified%))
         (pred (nget-property :predicate properties %unspecified%)))
    (when (eq lp %unspecified%)
      (error "DEFTEST ~S absent of :LAMBDA property" name))
    (with-gensym (%suite class test object predicate)
      `(let* ((,%suite ,(if suitep 
                            `(find-test-suite (quote ,suite) t)
                            `(values *test-suite*)))
              ;; FIXME: "Chicken and egg" issue for defaulting of TEST-CLASS
              ;;
              ;; Observe:
              ;;   1. A TEST-SUITE may make reference to one or more tests
              ;;   2. If a TEST must be defined as deriving its
              ;;      TEST-CLASS from a TEST-SUITE, there is a
              ;;      bootstrapping issue.   
              ;; 
              ;; Therefore.
              ;;  1. If a TEST is to be defined newly and without an
              ;;     explicit :CLASS specifier in DEFTEST, then at
              ;;     least one TEST-SUITE must be defined, intially,
              ;;     from which the DEFTEST macroexpansion may then
              ;;     derive the test's class.
              ;;
              ;;  2. For DEFTEST forms with explicit :CLASS
              ;;     specifiers, the containing TEST-SUITE need not be
              ;;     referenced for teremining the class of the test
              ;;     definition in DEFTEST
              ;;
              ;; Moreover: When a TEST-GOAL is initialized, the class
              ;; of the TEST-GOAL is computed per the test object and
              ;; the container object in which the test is to be
              ;; defined. See also: `ENSURE-GOAL', `COMPUTE-GOAL-CLASS'
             
              ;;     ... however FIXME: If the :CLASS speciifed within
              ;;     a DEFTEST form for a named test is not EQ to the
              ;;     existing class of the test, then the test must be
              ;;     updated for its new class, via CHANGE-CLASS etc.
              ;;     
              ;;     The similar must be done, for test goal objects
              (,class ,(if (eq c %unspecified%)
                           `(default-test-class ,%suite)
                           `(find-class ,c t ,env)))
              (,object ,(values ob))
              (,predicate ,(if (eq pred %unspecified%)
                               `(function eql)
                               `(values ,pred)))
              ;; FIXME: :LAMBDA only applicable for LISP-TEST
              (,test (make-instance ,class
                                    :name (quote ,name)
                                    :object ,object
                                    :predicate ,predicate
                                    :lambda (lambda ,@lp)
                                    ,@(map-properties properties))))
         (add-test ,test ,%suite)
         (values ,test ,%suite)))))

(defmacro defgoals (name (test &optional (suite *test-suite* suitep))
                    &rest properties)
#+NIL  "Define a GOAL-SET with name NAME 

FIXME: Update documentation string:
* TEST
   * Must name an initialized TEST object within the specified SUITE
   * Will not be evaluated
* SUITE
    * Symbol, a test suite name
    * May name be a TEST-SUITE or a GOAL-SET (VERIFY). 
    * Will not be evaluated
* :GOALS syntax, within PROPERTIES
* Other PROPERTIES syntax - :CLASS, etc.
"
  (setq properties (copy-list properties))
  (let ((c (nget-property :class properties %unspecified%)) ;; ? class of ..?
        (forms-cache))
    (with-gensym (%suite goal-set %test class)
      `(let* ((,%suite ,(if suitep 
                            `(find-test-suite (quote ,suite) t)
                            `(values *test-suite*)))
              (,%test (find-test (quote ,test) ,%suite))
              (,class ,(if (eq c %unspecified%)
                           ;; FIXME: No interface for defining
                           ;; GOAL-SET instances
                           `(default-goal-set-class ,%suite)
                           `(compute-class (quote ,c)))))
           
         ;; FIXME: Record source locations (portably) - see DEFINITION
         ,@(loop
              (let ((g (nget-property :goal properties %unspecified%)))
                (cond
                  ((eq g %unspecified%)
                   (return))
                  (t
                   (destructuring-bind (name . args) g
                     ;; NB: This (FIXME:
                     ;; 1. ensures that all elements of ARGS will be evaluated 
                     ;; 2. does not coerce ARGS from property format 
                     ;; 3. does not define a PARAMS-FUNCTION for
                     ;;    evaluating :PARAMS initarg
                       ;; 4. does not define an :EXPECT-FUNCTION for
                     ;;    evaluating :EXPECT initarg
                     (let* ((expect (nget-property* :expect args))
                            (expect-fn `(lambda () (list ,@expect)))
                            (params (nget-property* :params args))
                            (params-fn `(lambda () (list ,@params)))
                            (class (nget-property :class args %unspecified%)))

                       (cond
                         ((eq class %unspecified%)
                          (setq class 
                                `(compute-goal-class ,%test ,goal-set)))
                         ((symbolp class) (setq class `(quote,class))))
                         
                       
                       (setq forms-cache
                             (append forms-cache
                                     `((ensure-goal (quote ,name) ,goal-set
                                                    :expect (quote ,expect)
                                                    :expect-function ,expect-fn
                                                    :params (quote ,params)
                                                    :params-function ,params-fn
                                                    :class (compute-class ,class)
                                                    ,@(map-properties args)))))))))))
                
         (let ((,goal-set (ensure-goal-set (quote ,name) ,%suite
                                           :test ,%test
                                           :class (compute-class  ,class)
                                           ,@(map-properties properties))))

           
           ,@forms-cache
           
           )))))


(defun run-test-suite (suite &rest recording-params)
  (declare (type (or symbol test-suite) suite)
           (ignore recording-params))
  ;; FIXME: pass the RECORDING-PARAMS through to RUN-TESTS
  (let ((%suite (etypecase suite
                  (symbol (find-test-suite suite))
                  (test-suite suite))))
    (run-tests %suite)))
