;; test-classes.lisp - protocol classes [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defgeneric test-predicate (test))

(defgeneric test-predicate-function (test))

(defgeneric test-name (test))

(defgeneric test-object (test))


;;;; Test

(defclass test (labeled-object)
  ((environment
    :initarg :environment
    :accessor test-environment)

   (body
    :initarg :body
    :accessor test-body)
   (body-function
    :type function
    :initarg :body-function
    :accessor test-body-function)

   (pre-test-form
    :initarg :setup-form
    :initform nil
    :accessor test-pre-test-form)
   (pre-test-function
    :type function
    :initarg :setup-function
    :initform nil
    :accessor test-pre-test-function)

   (cleanup-form
    :initarg :cleanup-form
    :initform nil
    :accessor test-cleanup-form)
   (cleanup-function
    :type function
    :initarg :cleanup-form
    :initform nil
    :accessor test-cleanup-function)
   ))


(defmacro deftest (name (&rest args
                               &key (class 'diadic-predicate-test)
                               &allow-other-keys)
                                 body
                   &environment env)
    (macrolet ((pop-arg (kwd &optional defv)
                 (with-gensym (v default)
                   `(let ((,v (getf args ,kwd (quote ,default))))
                      (cond
                        ((eq ,v (quote ,default))
                         (values ,defv nil))
                        (t
                         (multiple-value-prog1 (values ,v t)
                           (remf args ,kwd))))))))
      (labels ((mk-kwd (a b) (intern (format nil "~A-~A" a b)
                                     (quote #:keyword)))
               (args-if (arg)
                   (multiple-value-bind (form foundp)
                       (pop-arg arg)
                     (when foundp
                       (let ((form-arg (mk-kwd arg (quote #:form)))
                             (fn-arg (mk-kwd arg (quote #:function))))
                         (list form-arg form
                               fn-arg `(lambda () ,form)))))))
        (with-gensym (test)
          `(let ((,test (make-instance ,(pop-arg :class class)
                                       ,@(args-if :setup)
                                       ,@(args-if :cleanup)
                                       ,@args
                                       :label (quote ,name)
                                       :environment ,env
                                       :body (quote ,body)
                                       :body-function
                                       (lambda () ,body)
                                       )))
             #+NIL (register-test ,test)
             (values ,test))))))

#+NIL
(let ((foo-interface-active-p)
      (*break-on-signals* t))
;;  (macroexpand-1
;;   (quote
    (deftest #:deftest-test
        (:class 'diadic-predicate-test
                :setup (setq foo-interface-active-p t)
                :predicate (quote =)
                :expect 4 ;; FIXME: move into test data
                )
      (cond
        (foo-interface-active-p
         (expt 2 2) ;; FIXME: move into test data
         )
        (t (quote #:fail)))))
;;   ))

;; test output

(defmethod format-test-label ((test test) (stream stream))
  (let ((label (labeled-object-label test)))
    (princ (or label %unnamed%)
           stream)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format-test-label test stream)))

;; test application

(defgeneric run-test (test &rest data &key &allow-other-keystest)
  ;; FIXME: Encapsulating the test data wtihin a test object
  ;; may not be "the best approach". Test data, alternately,
  ;; may be encapsulated within a TEST-CONDITION, thus allowing for a
  ;; TEST to simply represent a functional interface independent
  ;; of test data.
  ;;
  ;; Furthermore, it may be not unwise to allow for a test's
  ;; PRE-TEST and CLEANUP procedures to be parameterized for
  ;; individual test data.
  ;;
  ;; Both of those concerns may be resolved simply with an
  ;; application of generic functions.
  ;;  1) Revise DEFTEST such that it effectively provides an interface
  ;;     onto DEFMETHOD <FOO>
  ;;  2) Ensure that a test interface's 'setup' and 'cleanup' forms
  ;;     will be evaluated witin an :AROUND method onto <FOO>
  ;;     with the actual test form being evaluted within UNWIND-PROTECT
  (:method :around ((test test) &rest data &key &allow-other-keys)
           (declare (ignore data))
     (let ((pre-test-form (test-pre-test-form test)))
       (when pre-test-form
         (funcall (test-pre-test-function test))))
     (unwind-protect
          (when (next-method-p (call-next-method)))
       (let ((cleanup-form (test-cleanup-form test)))
         (when cleanup-form
           (funcall (test-cleanup-function test)))))))


;;;; Values-Test

(defclass values-test (test)
  ((expect-values
    :initarg :expect
    :accessor test-expect-values)
   (results-test
    :initarg :results-test
    :initform #'equalp
    :accessor test-results-test)))


(defmethod format-test-label :around ((test values-test) (stream stream))
  (call-next-method*)
  (format stream " => (~{ ~A~} ) ~A"
          (test-expect-values test)
          (test-results-test test)))


;;;; Predicate-Test

(defclass predicate-test (values-test)
  ;; NOTE: The definition of monadic, diadic, and variadic predicate
  ;; tests is developed primarily around the matter of how the
  ;; TEST-PREDICATE function would be FUNCALL'ed
  ((predicate
   ;; FIXME: PREDICATE is redundant onto RESULTS-TEST
    :type function
    :initarg :predicate
    :accessor test-predicate)))


(defmethod format-test-label :around ((test predicate-test) (stream stream))
  (call-next-method*)
  (princ #\Space stream)
  (princ (test-predicate test) stream))

(defmethod run-test :around ((test predicate-test)
                             &rest data &key &allow-other-keys)
  (let ((nmp (next-method-p)))
    (cond
      (nmp
       ;; FIXME: The semantics of predicates and results-tests
       ;; may need description
       (let* ((result-values (multiple-value-list (call-next-method)))
              (expect-values (test-expect-values test))
              (result-okidoke-p
               (funcall (test-results-test test)
                        result-values expect-values)))
         (cond
           (result-okiedoke-p
            (signal 'test-succeeded :test test :parameters data))
           (t (signal 'test-failed :test test :parameters data)))))
      (t
       (simple-program-error "No primary test metod defined for ~S"
                             test)))))


;;;; Diadic-Predicate-Test

(defclass diadic-predicate-test (predicate-test)
  ((datum-a
    :initarg :datum-a
    :accessor test-datum-a)
   (datum-b
    :initarg :datum-b
    :accessor test-datum-b)))

(defmethod format-test-label ((test diadic-predicate-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A ~A ~A) => (~{ ~A~} ) ~A"
          (test-predicate test)
          (test-datum-a test)
          (test-datum-b test)
          (test-expect-values test)
          (test-results-test test)))



(defmethod run-test ((test diadic-predicate-test)
                     &rest data &key &allow-other-keys)
  (let ((a (test-datum-a test))
        (b (test-datum-b test)))
    (funcall (the function (test-predicate test))
             a b)))


;;;; Monadic-Predicate-Test

(defclass monadic-predicate-test (predicate-test)
  ((datum
    :initarg :datum
    :accessor test-datum)))


(defmethod format-test-label ((test monadic-predicate-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A ~A) => (~{ ~A~} ) ~A"
          (test-predicate test)
          (test-datum test)
          (test-expect-values test)
          (test-results-test test)))

(defmethod run-test ((test monadic-predicate-test)
                     &rest data &key &allow-other-keys)
  (let ((a (test-datum test)))
    (funcall (the function (test-predicate test))
             a)))

;;;; Variadic-Predicate-Test

(defclass variadic-predicate-test (predicate-test)
  ((data
    :initarg :datum
    :accessor test-data)))

(defmethod format-test-label ((test monadic-predicate-test)
                              (stream stream))
  (princ-label test stream)
  (format stream "(~A~{ ~A~}) => (~{ ~A~} ) ~A"
          (test-predicate test)
          (test-data test)
          (test-expect-values test)
          (test-results-test test)))


(defmethod run-test ((test variadic-predicate-test)
                     &rest data &key &allow-other-keys)
  (let ((test-data (test-data test)))
    (apply (the function (test-predicate test))
           test-data)))
