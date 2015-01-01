;; test-classes.lisp - protocol classes [AFFTA]

;; FIXME: Once AFFTA test-interfaces is defined,
;;        then define tests for:
;;  1. ASSOCIATIVE-INDEX and subclasses
;;  2. TEST-SUITE and TEST registry protocol (presently mirroring ASSOCIATIVE-INDEX protocol)

(in-package #:mcicl.test)

(defgeneric test-object (test))
(defgeneric (setf test-object) (new-value test))

(defgeneric test-predicate (test))
(defgeneric (setf test-predicate) (new-value test))

(defgeneric test-summary (test))
(defgeneric (setf test-summary) (new-value test))

(defgeneric test-setup-function (test)
  (:documentation "Functional test setup protocol.

Syntax:
>    TEST-SETUP-FUNCTION TEST => OBJECT, BOUND-P
* OBJECT: a FUNCTION object or NIL
* BOUND-P: a BOOLEAN value

TEST-SETUP-FUNCTION provides an interface for a test developer to
define an anonymous lambda function for purpose of performing specific
procedures to configure a test's testing environment, prior to a
test's main form.

The system supplied primary method `TEST-SETUP-FUNCTION (TEST)` first
checks whether the SETUP-FUNCTION slot is bound on the TEST object. If
the slot is bound, the method returns the value of that slot as the
OBJECT and a 'true' value for BOUND-P. If the slot is not bound, the
method returns 'false' in both of the OBJECT and BOUND-P values.

For a function OBJECT, the function must accept two arguments:
* a TEST-GOAL
* a TEST object. 

Within the method `DO-TEST :AROUND (LISP-TEST-GOAL T)`, the function
will be evaluated with the TEST-GOAL and the TEST provided ot the
method. The values returned by the function will then be stored as the
TEST-SETUP-VALUES for the TEST-RECORD returned by the containing
method.

See also: `DO-TEST-SETUP'; `DO-TEST'; `TEST-CLEANUP-FUNCTION'"))


(defgeneric test-cleanup-function (test)
  (:documentation "Functional test cleanup protocol

Syntax:
>    TEST-SETUP-FUNCTION TEST => OBJECT, BOUND-P
* OBJECT: a FUNCTION object or NIL
* BOUND-P: a BOOLEAN value

TEST-CLEANUP-FUNCTION provides an interface for a test developer to 
define an anonymous lambda function for purpose of performing specific
procedures as to \"reset\" and \"clean up\" objects defined in a
test's testing environment, following a test's main form.

The system supplied primary method `TEST-CLEANUP-FUNCTION (TEST)` first
checks whether the CLEANUP-FUNCTION slot is bound on the TEST
object. If the slot is bound, the method returns the value of that
slot as the OBJECT and a 'true' value for BOUND-P. If the slot is not
bound, the method returns 'false' in both of the OBJECT and BOUND-P
values.

For a function OBJECT, the function must accept two arguments:
* a TEST-GOAL
* a TEST object. 

Within the method `DO-TEST :AROUND (LISP-TEST-GOAL T)`, the function
will be evaluated with the TEST-GOAL and the TEST provided ot the
method. The values returned by the function will then be stored as the
TEST-CLEANUP-VALUES for the TEST-RECORD returned by the containing
method.

See also: `DO-TEST-CLEANUP'; `DO-TEST'; `TEST-SETUP-FUNCTION'")
  )

(defvar %unbound-slot-label% 
  ;; FIXME: #I18N
  ;; FIXME: Stack allocate this binding and its string value
  (coerce "{Unbound}" 'simple-base-string))

;;;; Test

(defclass test (definition associative-object)
  ;; FIXME: add slot SUITE (?)
  ((object
    :initarg :object
    :accessor test-object)
   (predicate
    :initarg :predicate
    :type function
    :accessor test-predicate)
   ;; NOTE: The following slots are defined with non-conventional
   ;; accessors, below.
   (setup-function
    ;; FIXME: Define SETUP-FUNCTION-FORM additionally?
    ;; FIXME: Document the usage/non-usage of this slot value.
    ;;        See also: `DO-TEST-SETUP'
    :initarg :setup-function
    :accessor %test-setup-function)
   (cleanup-function
    ;; FIXME: Define CLEANUP-FUNCTION-FORM additionally?
    ;; FIXME: Document the usage/non-usage of this slot value.
    ;;        See also: `DO-TEST-CLEANUP'
    :initarg :cleanup-function
    :accessor %test-cleanup-function)
   ))

#+NIL ;; regression test - test initialization
(make-instance  'test)


(define-condition test-not-found (container-condition entity-not-found)
  ()
  (:report format-condition))

(defmethod format-condition ((condition test-not-found)
                             (stream stream))
  (format stream
          "No test found for name ~S within suite ~S"
          (entity-condition-name condition)
          (object-name (container-condition-container condition))))



(defmethod test-setup-function ((test test))
  ;;
  ;; (values (or null function) boolean)
  ;;
  ;; if a function, must accept two arguments:
  ;;  1) list of parameters provided to the test
  ;;  2) test object
  ;;
  ;; function would be called before the test's primary method is
  ;; evaluated within DO-TEST
  (cond
    ((slot-boundp test 'setup-function)
     (values (%test-setup-function test) t))
    (t (values nil nil))))

(defmethod test-cleanup-function ((test test))
  ;; 
  ;;
  ;; (values (or null function) boolean)
  ;;
  ;; if a function, must accept two arguments:
  ;;  1) list of parameters provided to the test
  ;;  2) test object
  ;;
  ;; function would be called within the cleanup forms of an
  ;; unwind-protect form within DO-TEST
  (cond
    ((slot-boundp test 'cleanup-function)
     (values (%test-cleanup-function test) t))
    (t (values nil nil))))


(defgeneric (setf test-setup-function) (new-value test)
  (:method ((new-value function) (test test))
    (setf (%test-setup-function test) new-value))
  (:method ((new-value list) (test test))
    (let ((fn (compile* new-value)))
      (setf (test-setup-function test) fn))))

(defgeneric (setf test-cleanup-function) (new-value test)
  (:method ((new-value function) (test test))
    (setf (%test-cleanup-function test) new-value))
  (:method ((new-value list) (test test))
    (let ((fn (compile* new-value)))
      (setf (test-cleanup-function test) fn))))

(declaim (type string %unbound-slot-label%))

(defmethod print-label ((test test) (stream stream))
  (format stream "~A ~<[~A]~>" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (print-label test stream)))

;;;; Functional-Test [FIXME: Rename to FUNCTIONAL-TEST]


;; FIXME/TO-DO:


#+AFFTA-2.0
(defclass application-test (test)
  ...)

#+AFFTA-2.0
(defclass lisp-application-test (application-test lisp-test)
  ...)

#+AFFTA-3.0
(defclass rootfs-test (test)
  ...)


(defclass lisp-test (test)
  ((lambda-form
    :initarg :lambda
    ;; NOTE: A CLtL compiler typically evaluates (LAMBDA () ...)
    ;; as a functional expression, rather than a list expression.
    ;; If the (LAMBA () ...) form is _quoted_ however, then the form
    ;; will be interpreted as a _list_ and may be provided for
    ;; subsequent system diagnostics.
    :accessor test-lambda-form)
   (lambda-function
    :accessor test-lambda-function)))

(defmethod shared-initialize :after ((instance lisp-test)
                                     slot-names 
                                     &rest initargs
                                     &key &allow-other-keys)
  (declare (ignore initargs))
  (when-slot-init (instance lambda-function slot-names)
    (when (slot-boundp instance 'lambda-form)
      (setf (test-lambda-function instance)
            (compile nil (test-lambda-form instance))))))

#+NIL ;; regression test - lisp-test initialization
;; Ensure initialization oif LAMBDA-FUNCTION slot from LAMBA-FORM initarg
(let ((inst 
       (make-instance 
        'lisp-test 
        :lambda '(lambda () (+ 2 2)))))
  (funcall (test-lambda-function  inst)))
;; => 4


(defmethod print-label ((test lisp-test) (stream stream))
  (format stream "~A ~<~<[~A]~> ~<[~A]~>~>" 
          (class-name (class-of test))
          (slot-value* test 'object %unbound-slot-label%)
          (slot-value* test 'lambda-function)))


;;; % Test Goals Container - Protocol Class

(defgeneric test-reference-test (instance))
(defgeneric (setf test-reference-test) (new-value instance))

(defclass test-reference () ;; Mixin - TEST reference
  ((test
    :initarg :test
    :type test
    :accessor test-reference-test)))


(defgeneric default-goal-set-class (container))
(defgeneric (setf default-goal-set-class) (new-value container))

(defgeneric container-goals-index (container))
(defgeneric (setf container-goals-index) (new-value container))

(defclass test-goal-container (test-reference definition associative-object)
  ;; Proocol class. 
  ;; Effective implementation classes:
  ;;  * GOAL-SET
  ;;  * TEST-SUITE 

  ;; FIXME: Document "Inheritance of TEST object" for
  ;; GOAL-SET, TEST-GOAL, and TEST-SUITE 
  ;; ^ cf TEST-REFERENCE-TEST
  ((default-goal-set-class
       :initarg :default-goal-set-class
     :initform (find-class 'goal-set)
     :type class-designator
     :accessor default-goal-set-class)
   (goals-index
    :initarg :goals-index
    :initform (make-instance 'simple-associative-index
                             ;; :element-type 'test-goal-container
                             ;; FIXME: Define ELEMENT-TYPE checking
                             ;; for SIMPLE-ASSOCIATIVE-INDEX
                             :key-function #'object-name)
    :type associative-index
    :accessor container-goals-index)))


(defgeneric register-goal (goal container)
  (:method ((goal associative-object) (container test-goal-container))
    (register-object goal  (container-goals-index container))))


(defgeneric find-goal (name container &optional errorp)
  (:method ((name symbol) (container test-goal-container) 
            &optional (errorp t))
    (find-object name  (container-goals-index container) errorp)))

(defgeneric remove-goal (name container)
  (:method ((name symbol) (container test-goal-container))
    (remove-object name  (container-goals-index container) )))


(defgeneric map-goals (function container)
  (:method (function (container test-goal-container))
    (map-objects function (container-goals-index container))))


(defgeneric compute-goal-class (test container &rest initargs
                                &key &allow-other-keys)
  ;; NOTE: INITARGS is interpreted as initialization arguments for a
  ;; TEST-GOAL 
  
  ;; FIXME: Try to apply a "reverse" method combination 
  ;; then refedine the (LISP-TEST GOAL-SET) method to 
  ;; only (find-class 'lisp-test-goal) 
  (:method ((test lisp-test) (container test-goal-container) &rest initargs
            &key (class nil cp) &allow-other-keys)
    (declare (ignore  initargs))
    (cond
      (cp (values (compute-class class)))
      (t (find-class 'lisp-test-goal))))
  
  (:method ((test test) (container test-goal-container) &rest initargs
            &key (class nil cp) &allow-other-keys)
    (declare (ignore initargs))
    (cond
      (cp (values (compute-class class)))
      (t (call-next-method)))))

;; (compute-goal-class (make-instance 'lisp-test) (make-instance 'goal-set))
;; => #<STANDARD-CLASS LISP-TEST-GOAL>

;; (compute-goal-class (make-instance 'lisp-test) (make-instance 'goal-set) :CLASS (FIND-CLASS 'TEST-GOAL))
;; => #<STANDARD-CLASS TEST-GOAL>


(defgeneric ensure-goal (name container &rest initargs
                         &key &allow-other-keys)
  (:method ((name symbol) (container test-goal-container)
            &rest initargs &key (test nil tp)
                             (class nil cp) 
                             &allow-other-keys)
    (when cp (remf initargs :class))
    (let ((instance (find-goal name container nil)))
      (unless tp
        (setq test (test-reference-test container)))
      (cond
        ((and instance cp (not (eq class (class-of instance))))
         (apply #'change-class instance class 
                :test test
                :name name
                initargs))
        (instance
         (values (apply #'reinitialize-instance instance
                        :test test
                        :name name 
                        initargs)
                 test))
        (t
         (let ((c (if cp 
                      (compute-class class)
                      (apply #'compute-goal-class 
                             test container))))
           (setq instance (apply #'make-instance c
                                 :test test
                                 :name name 
                                 initargs))
           ;; NB: ^ This may be the only form that would differentiate 
           ;; this method from a method onto a hypothetical
           ;; ENSURE-GOAL-CONTAINER function
           (register-goal instance container)
           (values instance test)))))))


;;; % Test Suite

(defgeneric %test-suite-tests (suite))

(defgeneric default-test-class (container))
(defgeneric (setf default-test-class) (new-value container))


(defclass test-suite (test-goal-container 
                      simple-associative-index ;; applied for TEST indexing
                      )

  ;; Essentially, a TEST-SUITE represents a "Root container" for
  ;; tests, goal sets, and test goals. 
  ;;
  ;; Insofar as application design: TEST-SUITE objects are indexed 
  ;; uniquely -- as according to the OBJECT-NAME of each -- within the 
  ;; class, TEST-SUITE
  ;;
  ;; Insofar as "recommended appliation" :
  ;;  0. Ensure that an appropriate TEST class is defined for the test
  ;;  1. Define a TEST-SUITE A - cf. DEFSUITE
  ;;  2. Define a TEST B - cf. DEFTEST - onto A
  ;;  3. Define a TEST-GOAL C - cf. DEFGOALS - onto B
  ;;  4. (TBD: "Test running" interface - onto A, B, or )

  ;; NB: The effective 'root' DEFAULT-TEST-CLASS is defined within the
  ;; macro definition for DEFSUITE

  ;; NB: Differentiation between TEST-SUITE and GOAL-SET [Design]
  ;;
  ;;   A TEST-SUITE contains an index of TEST objects, as well as a
  ;;   corresponding index of GOAL-SET and TEST-GOAL objects. 
  ;;
  ;;  Of course, a TEST-GOAL must make refernce to a TEST object, for
  ;;  purpose of application. For purpose of inheritance by TEST-GOAL
  ;;  object, a GOAL-SET will also make reference to a TEST
  ;;  object. 
  ;;
  ;;  A GOAL-SET contains only other GOAL-SET and TEST
  ;;  objects

  ;; FIXME: Rather than defining tests within a TEST-SUITE by way of
  ;; inheritence, instead define by way of encapsulation - parallel
  ;; to the GOALS-INDEX slot, a TESTS-INDEX slot.
  ;; 
  ;; See also: FIND-TEST, etc.

  ((default-test-class
        :initarg :default-test-class
      :initform (find-class 'lisp-test)
      :type class-designator 
      :accessor default-test-class))

  (:metaclass simple-associative-class)
  ;; The following KEY-SLOT is applied for global TEST-SUITE indexing
  ;; within the TEST-SUITE class. The NAME slot is defined onto
  ;; ASSOCIATIVE-OBJECT 
  (:key-slot . mcicl.utils::name) 
  ;; The :KEY-FUNCTION initialization argument provides a utility for
  ;; TEST indexing within a TEST-SUITE, cf. SIMPLE-ASSOCIATIVE-INDEX
  (:default-initargs :key-function #'object-name))

(define-condition test-suite-not-found (entity-not-found)
  ()
  (:report format-condition))

(defmethod format-condition ((condition test-suite-not-found)
                             (stream stream))
  (format stream
          "No test suite found for name ~S"
          (entity-condition-name condition)))


(defmethod print-label ((object test-suite) (stream stream))
  (let ((name (ignore-errors (object-name object)))
        (%tests (ignore-errors (object-table object))))
    (format stream "~<~A (~A tests)~>" name 
            (when %tests
              (hash-table-count %tests)))))

(defmethod print-object ((object test-suite) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-label object stream)))

(defgeneric register-test-suite (instance)
  (:method ((instance test-suite))
    (register-object instance (find-class 'test-suite))))

(defgeneric find-test-suite (name &optional errorp)
  (:method ((name symbol) &optional (errorp t))
    (let  ((instance
            (find-object name (find-class 'test-suite) nil)))
      (cond
        (instance (values instance))
        (errorp (error 'test-suite-not-found :name name))
        (t (values nil))))))

(defgeneric remove-test-suite (instance)
  (:method ((instance test-suite))
    (remove-object (object-name instance) 
                   (find-class 'test-suite))))

(defgeneric map-test-suites (function)
  (:method (function)
    (map-test-suites (coerce function 'function)))
  (:method ((function function))
    (map-objects function (find-class 'test-suite))))


(defgeneric add-test (test suite)
  (:method ((test test) (suite test-suite))
    (register-object test suite)))

(defgeneric remove-test (test suite)
  (:method ((test test) (suite test-suite))
    (remove-object test suite)))

(defgeneric find-test (name suite &optional errorp)
  (:method (name (suite symbol) &optional (errorp t))
    (let ((suite (find-test-suite suite)))
      (find-test name suite errorp)))
  (:method ((name symbol) (suite test-suite) &optional (errorp t))
    (let ((test (find-object name suite nil)))
      (cond
        (test (values test))
          (errorp (error 'test-not-found
                         :name name :container suite))
          (t (values nil))))))

(defgeneric map-tests (function suite)
  (:method (function (suite test-suite))
    (map-objects function suite)))


;;; % Test Goal Containment - Test Goals and Goal Sets

;; %% Goals for Test Definition

(defclass test-goal (definition test-reference associative-object)
  ;; effectively, a test goal encapsulates a set of _parameters_ and
  ;; an object representative of an _expected state_, as provided the
  ;; assigned parameters, for applicaiton onto a test's _primary test
  ;; function_. 
  ;;
  ;; A _test predicate_ would be defined -- as referenced within a
  ;; test object -- as to verify that the _expected state_ of a _test
  ;; goal_ matches the _end state_ of a test's _primary test
  ;; function_.
  ())

(defgeneric test-parameters (instance))
(defgeneric (setf test-parameters) (new-value instance))

(defgeneric test-parameters-function (instance))
(defgeneric (setf test-parameters-function) (new-value instance))

(defgeneric test-expect-state (instance))
(defgeneric (setf test-expect-state) (new-value instance))

(defgeneric test-expect-state-function (instance))
(defgeneric (setf test-expect-state-function) (new-value instance))

(defclass lisp-test-goal (test-goal)
  ((parameters
    :initarg :params
    :accessor test-parameters)
   (parameters-function
    :initarg :params-function
    :type function
    :accessor test-parameters-function)
   (expect-state
    :initarg :expect
    :accessor test-expect-state)
   (expect-state-function
    :initarg :expect-function
    :accessor test-expect-state-function)
   ))

(defmethod print-label ((goal lisp-test-goal) (stream stream))
  (format stream "~<~A =?=> ~A~> ~<(~A)~>"
          (test-parameters goal)
          (test-expect-state goal)
          (function-name (test-predicate goal))))

#+TO-DO
(defclass application-test-goal (test-goal)
  ()
  (:default-initargs
   ;; apply PARAMETERS as {command, arg+, env-var-spec*, rootfs-spec?, ...}
   ;; apply EXPECT-STATE as shell return value (?)
   :expect 0 ))

#+TO-DO
(defclass rootfs-test-goal (test-goal)
  ()
  ;; define PARAMETERS as filesystem pathame of filesystem directory OR {filesystem image and optional partition number} containing rootfs
  ;; define EXPECT-STATE as ...?
  )

;;; % GOAL-SET


(defclass goal-set (test-goal-container)
  ;; A GOAL-SET represents a "last branch" container for TEST-GOAL
  ;; objects. Respectively, TEST-GOAL objects serve as effective "leaf
  ;; nodes" onto a common TEST object -- as would be identified within
  ;; a containing  GOAL-SET, within a common TEST-SUITE.
  ;;
  ;; This system defines a singularly linked list, from TEST-SUITE to
  ;; GOAL-SET and TEST-GOAL objects. This system does not allocate
  ;; storage for static back-references from GOAL-SET or TEST-GOAL
  ;; objects to containing TEST-SUITE objects. 
  ())


(defgeneric ensure-goal-set (name container &rest initargs
                                          &key &allow-other-keys)
  (:method ((name symbol) (container test-goal-container)
            &rest initargs
            &key (class nil cp) (test nil tp) &allow-other-keys)
    (let ((instance (find-goal name container nil))
          (test (if tp test (test-reference-test container))))
      ;; FIXME: Check class of INSTANCE to insure it is TYPEP GOAL-SET
      (when cp (remf initargs :class))
      (cond
        ((and instance cp (not (eq class (class-of instance))))
         (values (apply #'change-class instance class
                        :test test
                        :name name 
                        initargs)
                 test))
        (instance
         (values  (apply #'reinitialize-instance instance
                         :test test
                         :name name 
                         initargs)
                  test))
        (t 
         (let ((c (if cp class
                      (default-goal-set-class container))))
           (setq instance (apply #'make-instance c
                                 :test test
                                 :name name 
                                 initargs))
           (register-goal instance container)
           (values instance test)))))))
                 
           

