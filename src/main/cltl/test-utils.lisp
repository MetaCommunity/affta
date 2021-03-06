;; test-utils.lisp - utility forms [AFFTA]

(in-package #:mcicl.test)

(defclass closure-container () ;; cf. PREDICATE (?)
  ;; FIXME: Reevaluate this class' application
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))

;;; % Set Equivalence Operations


(defmacro do-seo (op (a b))
  ;; do-set-equivalence-op
  (with-gensym (%a %b len-a len-b n)
    `(let ((,%a ,a)
           (,%b ,b)
           (,len-a (length a))
           (,len-b (length b)))
       (declare (type unsigned-byte ,len-a ,len-b)
                (inline ,op))
       ;; NB: This might avert some compiler transformations, in using
       ;; DOTIMES instead of EVERY. Notably, SBCL converts an EVERY
       ;; call to a MAP call. Perhaps there may be not much of an
       ;; optimization lost, in this alternate approach of using
       ;; DOTIMES instead of EVERY -- with LEN-A and LEN-B already
       ;; available, as needed for this set/elements form.
       ;;
       ;; NB: A compiler optimization may be defined for the LENGTH
       ;; and ELT calls in this form, for when A and/or B can be
       ;; determined to be type VECTOR or SIMPLE-ARRAY [SBCL
       ;; 1.2.6]. Perhaps that may behoove a design for a method for
       ;; dispatching optimizations. Alternately, this may be
       ;; implemented within a generic function in which the types of
       ;; A and B would be more specificall defined
       (and (= ,len-a ,len-b)
            (dotimes (,n ,len-a t)
              (unless (funcall (function ,op) (elt ,%a ,n) (elt ,%b ,n))
                (return nil)))))))


(defun set= (a b)
  (declare (type sequence a b)
           (values boolean))
  (do-seo = (a b)))

;; (set= '(1.0 2.0) '(1 2.0d0))
;; => T
;; (set= '(1.0 2.0) '(1 2.0d0 3))
;; => NIL
;; (set= '(1.0 3.0) '(1 2.0d0))
;; => NIL


(defun set-eql (a b)
  (declare (type sequence a b)
           (values boolean))
  (do-seo eql (a b)))

(defun set-eq (a b)
  (declare (type sequence a b)
           (values boolean))
  (do-seo eq (a b)))

(defun set-equal (a b)
  (declare (type sequence a b)
           (values boolean))
  (do-seo equal (a b)))

(defun set-equalp (a b)
  (declare (type sequence a b)
           (values boolean))
  (do-seo equalp (a b)))

;;; % Class DEFINITION

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
