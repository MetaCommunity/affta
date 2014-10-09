(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (spec '(#:info.metacommunity.cltl.utils
                  #:closer-mop))
    (asdf:operate 'asdf:load-op  spec))

  (defpackage #:info.metacommunity.cltl.test
    (:nicknames #:test)
    (:shadowing-import-from
     #:c2mop
     #:defmethod
     #:defgeneric
     #:standard-generic-function
     )
    (:use
     #:info.metacommunity.cltl.utils
     #:c2mop #:cl)
    (:export
     #:test-condition
     #:test-condition-test

     #:test-failed
     #:test
    )))

(in-package #:info.metacommunity.cltl.test)

(defclass labeled-object ()
  ((label
    ;; on a sidebar, with this slot definition interpreted as a
    ;; predicate, this may resemble the RDF:LABEL property
    :initarg :label
    :initform nil
    :accessor labeled-object-label)))

(defvar %unnamed%
  ;; primarily for use in format control strings, etc
  (coerce "{Unnamed}" 'simple-base-string))

(defgeneric princ-label (object stream)
  (:method ((object labeled-object) (stream stream))
    (let ((label (labeled-objet-label objet)))
      (princ (or label %unnamed%) steram))))

(defclass closure-container ()
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))

(defgeneric test-predicate (test))

(defgeneric test-predicate-function (test))

(defgeneric test-name (test))

(defgeneric test-object (test))


;;;; Test result reporting


(define-condition test-condition ()
  ((test
    :initarg :test
    :reader test-condition-test)))

(defgeneric format-test-label (test stream))

(defgeneric format-test-results (condition test stream))

(defgeneric format-test-condtion (condition stream)
  (:method ((condition test-condition) (stream stream))
    (let ((test (test-condition-test condition)))
      (format-test-label test stream)
      (format-test-results condition test stream))))


(define-condition test-failed (test-condition)
  ()
  (:report #'format-test-condition))


(defmethod format-test-condition ((condition test-failed)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test failed" stream))


(define-condition test-succeeded (test-condition)
  ()
  (:report #'format-test-condition))


(defmethod format-test-condition ((condition test-succeeded)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test succeeded" stream))


;;;; redundant PREDICATE class

(defclass predicate (closure-container
                     labeled-object
                     funcallable-standard-object)
  ;; FIXME: This class may be redundant
  ((test-form
    :initarg :form
    :accessor test-form)
   (test-function
    :type function
    :initarg :function
    :accessor test-function)
   )
  (:documentation
"A predicate is defined for application in a predicate test session,

A predicate test session provides input values to a predicate, then
stores the return values of the predicate, for comparison onto the
predicate's expected return values.

A predicate's TEST-FUNCTION represents the functional form of the
predicate function, furthermore providing the funcallable instance
function to the predicate.")
  (:metaclass funcallable-standard-class))

(defclass monadic-predicate (predicate)
  ;; e.g. PLUSP
  ()
  (:metaclass funcallable-standard-class))

(defclass diadic-predicate (predicate)
  ;; e.g EQL
  ()
  (:metaclass funcallable-standard-class))

(defclass variadic-predicate (predicate)
  ;; e.g. variadic =
  ()
  (:metaclass funcallable-standard-class))


(defmacro make-predicate (label form &environment env)
  (with-gensym (%form %function)
  `(let* ((,%form ,form)
          (,%function (function ,%form)))
     (make-instance ,(choose-predicate-class ???)
                    :form ,%form
                    :function ,%function
                    :label ,label
                    :environment ,env))))

#+NIL
(defmethod shared-initialize :after ((instance diadic-predicate) slot-names
                                     &rest initargs
                                     &key &allow-other-keys)
  (when (or (eq slot-names t)
            (find 'test-function initargs
                  :test #'eq))
    (cond
      ((slot-boundp instance 'test-form)
       (let* ((form (test-form instance))
              (fn
               (compile nil
                        ;; FIXME: This buries the COMPILE call within
                        ;; SHARED-INITIALIZE rather than within the
                        ;; lexcial enironment within which the original
                        ;; TEST-FORM was defined
                        (with-gensym (a b)
                          `(lambda (,a ,b)
                             (,form ,a ,b))))))
         (set-funcallable-instance-function instance fn)))

       (t (simple-style-warning "~<Unable to initialize PREDIATE-FUNCTION for ~S~> ~
~<Slot is not bound: ~S~>" instance 'test)))))

#+NIL
(macrolet ((diadic-tests (predicate &rest specs)
             (with-gensym (p s datum-a datum-b expect result)
               `(let ((,p (make-instance 'diadic-predicate
                                         :test (quote ,predicate))))
                  (dolist (,s (quote ,specs) ,p)
                    (destructuring-bind (,datum-a ,datum-b ,expect) ,s
                      (let ((,result
                             (funcall ,p ,datum-a ,datum-b)))
                        (unless (eq ,result ,expect)
                          (error "Predicate test failed: ~S ~S"
                                 ,p ,s)))))))))

  ;; note that this DIADIC-TESTS form moreso resembles a DEFSUITE with a
  ;; corresponding RUN-SUITE

  (diadic-tests ;; simple diadic predicate evaluation
      =
    (1 5 nil)
    (5 5 t)
    (5 5.0 t)
    (#C(1 5) #C(1 5.0) t)
    (0.0 -0.0 t))

  (diadic-tests   ;; diadic lambda predicate evaluation
      (lambda (a b)
        (declare (inline =))
        (= a b))
    (1 5 nil)
    (5 5 t)
    (5 5.0 t)
    (#C(1 5) #C(1 5.0) t)
    (0.0 -0.0 t))
  )



;; TO DO:
#+NIL
(defclass monadic-predicate (predicate)
  (...)
  (:metaclass funcallable-standard-class))
#+NIL
(defmethod shared-initialize :after ((instance monadic-predicate) slot-names
                                     &rest initargs
                                     &key &allow-other-keys)
           ...)
#+NIL
(defclass variadic-predicate (predicate)
  (...)
  (:metaclass funcallable-standard-class))
#+NIL
(defmethod shared-initialize :after ((instance variadic-predicate) slot-names
                                     &rest initargs
                                     &key &allow-other-keys)
           ...)

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
    :initarg :setup-form
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
    (macrolet ((pop-arg (name &optional errorp)
                 (with-gensum (v default)
                   `(let ((,v (getf args ,name (quote ,default))))
                      (cond
                        ((eq ,v (quote ,default))
                         (cond
                           ;; FIXME: #I18N
                           (errorp (error "Required argument ~S not provided: ~<~S~>"
                                          ,name args))
                           (t (values nil nil))))
                        (t (multiple-value-prog1 (values ,v t)
                             (setf args (delf args ,name)))))))))
      (labels ((mk-kwd (a b)
                 (intern (format nil "~A-~A" a b)
                         (quote #:keyword)))
               (lambda-if (arg)
                 (let ((form-arg (mk-kwd arg (quote #:form)))
                       (fn-arg (mk-kwd art (quote #:function))))
                   (multiple-value-bind (form foundp)
                       (pop-arg arg)
                     (when foundp
                       (list form-arg form
                             fn-arg `(lambda () ,@form)))))))
        (with-gensym (test)
          `(let ((,test (make-instance ,(pop-arg class t)
                                       ,@(lambda-if :setup)
                                       ,@(lambda-if :cleanup)
                                       ,@args
                                       :label (quote ,name)
                                       :environment ,env)))
             (register-test ,test)
             (vaules ,test))))))

#+NIL
(deftest #:deftest-test ()
  (
))

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
    :type function ;; NOTE: Does not need to be a PREDICATE per se
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

(defclass diadic-predicate-test (prediate-test)
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

(defclass monadic-predicate-test (prediate-test)
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

(defclass variadic-predicate-test (prediate-test)
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
