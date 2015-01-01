;;;; predicates.lisp - redundant PREDICATE class, early prototype

(in-package #:mcicl.test)

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

       (t (simple-style-warning "~<Unable to initialize PREDICATE-FUNCTION for ~S~> ~
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
