;; test-utils.lisp - utility forms [AFFTA]

(in-package #:mcicl.test)

(defclass closure-container () ;; cf. PREDICATE (?)
  ;; FIXME: Reevaluate this class' application
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))


(defun every= (a b)
  (every #'= a b))

(defun every-eql (a b)
  (every #'eql a b))

(defun every-eq (a b)
  (every #'eq a b))

(defgeneric definition-summary (object)
  (:documentation 
   "Provide a succinct summary about a definition object"))
(defgeneric (setf definition-summary) (new-value object)
  (:documentation 
   "Establish a succinct summary about a definition object"))


(defgeneric definition-source-location (object))
(defgeneric (setf definition-source-location) (new-value object))
;; ^ FIXME/TO-DO : Define a portable source locations library (stand-alone)

(defclass definition ()
  ((summary
    :initarg :summary
    :accessor definition-summary
    :type simple-string)))

;; Trivial regression test
;; (make-instance 'definition)
;; (make-instance 'definition :summary "FOO")
