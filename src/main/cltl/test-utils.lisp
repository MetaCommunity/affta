;; test-utils.lisp - utility forms [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defgeneric label (object))

(defclass labeled-object ()
  ((label
    ;; [sidebar] with this slot definition interpreted as a
    ;; predicate, this slot may resemble an RDF:LABEL property
    :initarg :label
    :initform nil
    :accessor label)))

(defvar %unnamed%
  ;; primarily for use in format control strings, etc
  ;; FIXME: #I18N
  (coerce "{Unnamed}" 'simple-base-string))

(defgeneric princ-label (object stream)
  (:method ((object labeled-object) (stream stream))
    (let ((label (label object)))
      (princ (or label %unnamed%) stream))))

(defclass closure-container ()
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))
