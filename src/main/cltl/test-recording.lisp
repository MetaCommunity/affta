;; test-record.lisp - test data encapsulation [AFFTA]


(in-package #:info.metacommunity.cltl.test)

(defgeneric test-record-test (test))

(defgeneric test-record-parameters (test))

(defgeneric test-record-expect-state (test))
;; ^ FIXME: This applies only to a VALUES-TEST

(declaim (type function %default-equivalence-function%))
(defvar %default-equivalence-function% #'equalp)

(defgeneric test-predicate (test)
  (:method ((test functional-test))
    (values %default-equivalence-function%)))
;; ^ FIXME : Remove (?) or move into test-record.lisp


(defgeneric test-record-condition (test))

(defgeneric test-record-results (test))

(defgeneric test-record-setup-results (test))

(defgeneric test-record-cleanup-results (test))

;; FIXME: Implement TEST-GOAL



(defclass test-record ()
  ;; In the design of AFFTA 1.2, the TEST-RECORD class serves as both
  ;; a means for specifying parameters to a test and recording the
  ;; results of a test for later evaluation. Though it's semantically
  ;; a bit of a mashup, but in a sense it may serve towards a sense of
  ;; convenience as to keep the parameters and expected values close
  ;; together with the test result values.
  ((test ;; GOAL, RECORD
    :initarg :test
    :accessor test-record-test)
   (parameters ;; GOAL
    :initarg :parameters
    :accessor test-record-parameters)
   (expect-state ;; GOAL
    :initarg :expect
    :accessor test-record-expect-state)
   (condition ;; RESULT
    ;; If the test completed successfully, this slot's value should be
    ;; a TEST-RECORD-CONDITION. If the test did not complete
    ;; successfully, this slot's value should contain a CONDITION
    ;; object, such that would be representative of an exceptional
    ;; situation (i.e. ERROR, WARNING, or signaled CONDITION)
    ;; such that intercepted -- namely with HANDLER-CASE -- during the
    ;; evaluation of the TEST for the specified PARAMETERS.
    :initarg :condition
    :accessor test-record-condition)
   (results ;; RESULT
    ;; For a values test:
    ;;
    ;; This slot's value should hold a multiple-value list of the
    ;; values returned by "the primary DO-TEST method"
    ;;
    ;; (FIXME: In AFTA 1.2 the concept of "the primary DO-TEST method" 
    ;;  is regrettably vague in its definition - proceeding to a
    ;;  revision of the DO-TEST function, as well as DO-TEST-SETUP and
    ;;  DO-TEST-CLEANUP. See notes, below)
    :initarg :results
    :accessor test-record-results)
   (setup-results ;; RESULT
    ;; This slot's value should hold a multiple-value-list of any
    ;; values returned by the effective DO-TEST-SETUP method for this
    ;; TEST-RECORD  - recorded in the TEST-RECORD primarily for
    ;; information to the developer
    :initarg :setup-results
    :initform nil
    :accessor test-record-setup-results)
   (cleanup-results ;; RESULT
    ;; This slot's value should hold a multiple-value-list of any
    ;; values returned by the effective DO-TEST-CLEANUP method for
    ;; this TEST-RECORD - recorded in the TEST-RECORD primarily for
    ;; information to the developer
    :initarg :cleanup-results
    :initform nil
    :accessor test-record-cleanup-results)
   )

  #+AFFTA-1.4
  (stream-controller
   :initarg :stream-controller
   :intiform (make-instance 'union-stream))

  )




#| NOTE: [Design decision] Encapslation of test form, test parameter, and
test expected values forms


In defining an instance test such as the following:

  (or (= 4 (expt 2 2)) (error "Failed"))

... or such as within AFFTA 1.2:

#+PROTOTYPE  ;; (AFFTA 1.2)
 (handler-case
     (do-test '(2 2) '(4) #'expt)
   (test-succeeded (c)
     (format* "OK ~S" c))
   (test-failed (c)
     (format* "NOT OK ~S" c)))

... the developer is then able to write a convenient test form, within
the source code itself. Such an 'instance test' may serve a use for
immediate debugging. Complimentary with the convenience of such an
inline, functional testing methodology, however, there's a concern
that the instance test methodology, as such, serves to bury a system's
functional unit tests within comments in the system's source
code. 


Although such a "Commented instance test" methodology may certainly
not be of an immediate concern -- as when a system's unit tests are
all effectively pocketed away within comments in the source code --
and to the contrary, those instance tests may serve to present
convenient illustrations of the intended applications of a top-level
form to which such an instance test would be defined, moreover to
provide such an illustration in some proximity to the source code
itself -- however, over time, it may seem to interfere with the
overall maintainability of a system. A "Commented instance test"
methodology, as such, effectively removes those functional unit tests
from within the domain of anything that may be evaluated in "batch
mode," as when a system is being compiled within a continuous
integration server.


Ideally, the design of the AFFTA functional testing system would allow
for both of:

 1) Development of a rigorous, functional unit testing framework for
    Common Lisp applications

 2) Definition of convenient instance tests, to the developer's
    interest

Considering such mechanism as must be defined for effective
coordination of the first of those goals: It may seem to interfere
with the second of those goals, in this system's development, that
this sytem would be developed likewise for support of a rigorous,
functional unit testing framework.



See also:

 Inline tests: MKTEST
 Batch tests: [TBD] Refer to notes in ./test-protocol.lisp


FIXME: move this text to 
  project-file:affta;doc;md;design-decision.test-encapsulation.md

|#
