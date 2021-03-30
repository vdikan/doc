(uiop:define-package #:40ants-doc/locatives/method
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/document
                #:document-object)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:swank-backend)
  (:import-from #:swank-mop)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package 40ants-doc/locatives/method)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-locative-type method (method-qualifiers method-specializers)
  "See CL:FIND-METHOD for the description of the arguments.
  To refer to the default method of the three argument generic
  function FOO:

      (foo (method () (t t t)))")

(defmethod locate-object (symbol (locative-type (eql 'method))
                          locative-args)
  (unless (= 2 (length locative-args))
    (locate-error "Reference to method ~S should include qualifiers and specializers like:

(~S method ((:before) (some-arg type) t t))"
                  symbol
                  symbol))
  
  (destructuring-bind (qualifiers specializers) locative-args
    (or (ignore-errors
         (find-method (symbol-function symbol) qualifiers
                      (loop for specializer in specializers
                            collect (typecase specializer
                                      ;; SPECIALIZER can be a cons
                                      ;; like (:EQL :SOME-VALUE) ...
                                      (cons specializer)
                                      ;; or a type specifier denoting
                                      ;; a class:
                                      (t (find-class specializer))))))
        (locate-error))))

(defmethod canonical-reference ((method method))
  (40ants-doc/reference::make-reference (swank-mop:generic-function-name
                                         (swank-mop:method-generic-function method))
                                        `(method ,(swank-mop:method-qualifiers method)
                                                 ,(method-specializers-list method))))

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defmethod document-object ((method method) stream)
  (let ((arglist (rest (method-for-inspect-value method))))
    (40ants-doc/builder/bullet::print-bullet method stream)
    (write-char #\Space stream)
    (40ants-doc/render/args::print-arglist arglist stream)
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (40ants-doc/args::with-dislocated-symbols ((40ants-doc/args::function-arg-names arglist))
      (40ants-doc/render/print::maybe-print-docstring method t stream))))

;;;; These were lifted from the fancy inspector contrib and then
;;;; tweaked.

(defun method-specializers-for-inspect (method)
  """Return a "pretty" list of the method's specializers. Normal
  specializers are replaced by the name of the class, eql specializers
  are replaced by `(eql ,object)."""
  (mapcar (lambda (name spec)
            (let ((name (if (listp name) (first name) name)))
              (if (eq spec t)
                  name
                  (list name spec))))
          (swank-mop:method-lambda-list method)
          (method-specializers-list method)))

(defun method-for-inspect-value (method)
  """Returns a "pretty" list describing METHOD. The first element of
  the list is the name of generic-function method is specialized on,
  the second element is the method qualifiers, the rest of the list is
  the method's specialiazers (as per
  METHOD-SPECIALIZERS-FOR-INSPECT)."""
  (append (list (swank-mop:generic-function-name
                 (swank-mop:method-generic-function method)))
          (swank-mop:method-qualifiers method)
          (method-specializers-for-inspect method)))
