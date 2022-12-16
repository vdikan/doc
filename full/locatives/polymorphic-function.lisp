(uiop:define-package #:40ants-doc-full/locatives/polymorphic-function
  (:use #:cl)
  (:import-from #:polymorphic-functions
                #:polymorphic-function)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc-full/args)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown))
(in-package #:40ants-doc-full/locatives/polymorphic-function)

(define-locative-type 40ants-doc/locatives::polymorphic-function ())

(defmethod locate-object (symbol (locative-type (eql '40ants-doc/locatives::polymorphic-function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (symbol-function symbol)))
    function))

(defmethod canonical-reference ((function polymorphic-function))
  (let ((name (polymorphic-functions::polymorphic-function-name function)))
    (40ants-doc/reference::make-reference name
                                          '40ants-doc/locatives::polymorphic-function)))

(defmethod 40ants-doc-full/commondoc/builder::to-commondoc ((obj polymorphic-function))
  (let* ((arglist (swank-backend:arglist obj))
         (function-symbol (polymorphic-functions::polymorphic-function-name obj))
         (docstring (40ants-doc/docstring:get-docstring function-symbol 'function))
         (children (when docstring      ;FIXME: ad-hoc formatting of the polymorphic function docstring
                     (remove-if (lambda (s) (string= "Documentation:" s))
                                (mapcar (lambda (s) (string-trim " " s))
                                        (cl-ppcre:split #\newline docstring)))))
         (children (when children
                     (mapcar '40ants-doc-full/commondoc/markdown:parse-markdown children)))
         (reference (canonical-reference obj))
         (dislocated (40ants-doc-full/args::function-arg-names arglist)))

    (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                  :arglist arglist
                                                  :children children
                                                  :dislocated-symbols dislocated)))

