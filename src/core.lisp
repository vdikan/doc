(uiop:define-package 40ants-doc
  (:documentation "See 40ants-doc:@index.")
  (:nicknames 40ants-doc/core)
  (:use #:common-lisp)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/locatives)
  (:export #:define-package
           #:defsection
           #:exportable-locative-type-p
           #:locative-args
           #:locative-type
           #:make-reference
           #:reference
           #:reference-object
           #:reference-locative
           #:section
           #:section-name
           #:section-package
           #:section-readtable
           #:section-title
           #:section-link-title-to
           #:section-entries
           ;; TODO:
           ;; Locatives for these symbols will be defined
           ;; in the full 40ants-doc/full package:
           #:reader
           #:writer
           #:accessor
           #:macro))
(in-package 40ants-doc)


;;; Should this remove docstrings of referenced things?
(defvar *discard-documentation-p* nil
  "The default value of DEFSECTION's DISCARD-DOCUMENTATION-P argument.
  One may want to set *DISCARD-DOCUMENTATION-P* to true before
  building a binary application.")

(defmacro defsection (name (&key (package-symbol '*package*)
                                 (readtable-symbol '*readtable*)
                                 (export t)
                                 title
                                 link-title-to
                                 (discard-documentation-p *discard-documentation-p*))
                      &body entries)
  "Define a documentation section and maybe export referenced symbols.
  A bit behind the scenes, a global variable with NAME is defined and
  is bound to a [SECTION][class] object. By convention, section names
  start with the character `@`. See @40ANTS-DOC-TUTORIAL for an example.

  ENTRIES consists of docstrings and references. Docstrings are
  arbitrary strings in markdown format, references are defined in the
  form:

      (symbol locative)

  For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
  SECTION)` says that `@BAR` is a subsection of this
  one. `(BAZ (METHOD () (T T T)))` refers to the default method of the
  three argument generic function `BAZ`. `(FOO FUNCTION)` is
  equivalent to `(FOO (FUNCTION))`.

  A locative in a reference can either be a symbol or it can be a list
  whose CAR is a symbol. In either case, the symbol is the called the
  type of the locative while the rest of the elements are the locative
  arguments. See @LOCATIVE-TYPES for the list of locative
  types available out of the box.

  The same symbol can occur multiple times in a reference, typically
  with different locatives, but this is not required.

  The references are not looked up (see RESOLVE in the
  @EXTENSION-API) until documentation is generated, so it is
  allowed to refer to things yet to be defined.

  If EXPORT is true (the default), the referenced symbols and NAME are
  candidates for exporting. A candidate symbol is exported if

  - it is accessible in PACKAGE (it's not `OTHER-PACKAGE:SOMETHING`)
    and

  - there is a reference to it in the section being defined with a
    locative whose type is approved by EXPORTABLE-LOCATIVE-TYPE-P.

  See DEFINE-PACKAGE if you use the export feature. The idea with
  confounding documentation and exporting is to force documentation of
  all exported symbols.

  TITLE is a non-marked-up string or NIL. If non-NIL, it determines
  the text of the heading in the generated output. LINK-TITLE-TO is a
  reference given as an
  (OBJECT LOCATIVE) pair or NIL, to which the heading will link when
  generating HTML. If not specified, the heading will link to its own
  anchor.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, ENTRIES will not be recorded to save memory."
  ;; Let's check the syntax as early as possible.
  (setf entries
        (transform-locative-symbols
                entries))
  
  (transform-entries entries)
  (transform-link-title-to link-title-to)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (when ,export
         (export-some-symbols ',name ',entries ,package-symbol)))
     (defparameter ,name
       (make-instance 'section
                      :name ',name
                      :package ,package-symbol
                      :readtable ,readtable-symbol
                      :title ,title
                      :link-title-to (transform-link-title-to ',link-title-to)
                      :entries ,(if discard-documentation-p
                                    ()
                                    `(transform-entries ',entries))))))

(defclass section ()
  ((name
    :initarg :name :reader section-name
    :documentation "The name of the global variable whose value is
    this SECTION object.")
   (package
    :initarg :package :reader section-package
    :documentation "*PACKAGE* will be bound to this package when
    generating documentation for this section.")
   (readtable
    :initarg :readtable :reader section-readtable
    :documentation "*READTABLE* will be bound to this when generating
    documentation for this section.")
   (title
    :initarg :title :reader section-title
    :documentation "STRING or NIL. Used in generated documentation.")
   (link-title-to
    :initform nil
    :initarg :link-title-to :reader section-link-title-to
    :documentation "A REFERENCE or NIL. Used in generated documentation.")
   (entries
    :initarg :entries :reader section-entries
    :documentation "A list of strings and REFERENCE objects in the
    order they occurred in DEFSECTION."))
  (:documentation "DEFSECTION stores its NAME, TITLE, PACKAGE,
  READTABLE and ENTRIES in [SECTION][class] objects."))

(defmethod print-object ((section section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~a" (section-name section))))


;; This function is from alexandria, to not
;; introduce any dependencies to 40ants-doc/core
(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun locative-equal (locative-1 locative-2)
  (equal (ensure-list locative-1)
         (ensure-list locative-2)))

(defun transform-entries (entries)
  (mapcar (lambda (entry)
            (if (stringp entry)
                entry
                (entry-to-reference entry)))
          entries))

(defun entry-to-reference (entry)
  (destructuring-bind (symbol locative &key export) entry
    (declare (ignore export))
    (assert (symbolp symbol) ()
            "~S is not a valid reference its first element is not a ~
            symbol." entry)
    (40ants-doc/reference::make-reference symbol locative)))

(defun transform-link-title-to (link-title-to)
  (when link-title-to
    (if (typep link-title-to '40ants-doc/reference::reference)
        link-title-to
        (apply #'40ants-doc/reference::make-reference link-title-to))))

;;;; Exporting

(defun export-some-symbols (name entries package)
  (when (symbol-accessible-in-package-p name package)
    (export name package))
  (dolist (entry entries)
    (when (listp entry)
      (destructuring-bind (symbol locative) entry
        (when (and (symbol-accessible-in-package-p symbol package)
                   (exportable-locative-type-p (40ants-doc/locatives/base::locative-type locative)))
          (export symbol package))))))

(defun transform-locative-symbols (entries &aux (locatives-package (find-package "40ANTS-DOC/LOCATIVES")))
  (labels ((transform (locative)
           (etypecase locative
             (symbol (intern (symbol-name locative) locatives-package))
             (cons (cons (transform (car locative))
                         (cdr locative))))))
    
    (loop for entry in entries
          if (listp entry)
          collect (destructuring-bind (symbol locative) entry
                    (list symbol
                          (transform locative)))
          else collect entry)))

(defun symbol-accessible-in-package-p (symbol package)
  (eq symbol (find-symbol (symbol-name symbol) package)))

(defgeneric exportable-locative-type-p (locative-type)
  (:documentation "Return true iff symbols in references with
  LOCATIVE-TYPE are to be exported by default when they occur in a
  DEFSECTION. The default method returns T, while the methods for
  PACKAGE, ASDF:SYSTEM and METHOD return NIL.

  DEFSECTION calls this function to decide what symbols to export when
  its EXPORT argument is true.")
  (:method (locative-type)
    (declare (ignore locative-type))
    t))

;;;; These methods must be defined here else the DEFSECTION forms in
;;;; pax.lisp will export too much.

(defmethod exportable-locative-type-p ((locative-type (eql 'asdf:system)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'package)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'method)))
  nil)
