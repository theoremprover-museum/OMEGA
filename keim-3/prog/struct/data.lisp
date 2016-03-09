;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1998 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@ags.uni-sb.de                                    ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;



;;; Nov-1998: Schemata werden endgueltig eingefuehrt
;;;
;;; Noch nicht ausgefreift: Kopieren und ersetzten 



(in-package "KEIM")


(mod~defmod DATA
            :uses (bind doc keim pos pp tree)
            :documentation "Fundamental data structures"
            :exports (
		      data+abstr
		      data+appl
		      data+complex
		      data+constant
		      data+object
		      data+primitive
		      data+schema
		      data+struct
		      data+top
		      data+variable
		      
		      data~reader
		      data~defgeneric
		      
		      ;; Creating and access
		      
		      data~abstr-binder
		      data~abstr-bound-var
		      data~abstr-c-binder
		      data~abstr-c-domain
		      data~abstr-c-range
		      data~abstr-create
		      data~abstr-domain
		      data~abstr-n-domain
		      data~abstr-n-range
		      data~abstr-n-scope
		      data~abstr-p
		      data~abstr-range
		      data~abstr-scope
		      data~annotated-p
		      data~annotation
		      data~appl-arguments
		      data~appl-c-argument
		      data~appl-c-function
		      data~appl-create
		      data~appl-function
		      data~appl-last-argument
		      data~appl-n-arguments
		      data~appl-n-function
		      data~appl-p
		      data~args
		      data~binding
		      data~c-domain
		      data~c-range
		      data~complex-p
		      data~constant-create
		      data~constant-origin
		      data~constant-p
		      data~general-struct-p
		      data~label
		      data~linear-p
		      data~n-range
		      data~n-domain
		      data~plist
		      data~primitive-p
		      data~object-p 
		      data~schema-create
		      data~schema-domain
		      data~schema-p
		      data~schema-range
		      data~struct-p
		      data~top
		      data~variable-create
		      data~variable-p
		      
		      ;; Comparing
		      
		      data~alpha-equal
		      data~alpha-match
		      data~equal
		      data~equal-p
		      data~schema-equal

		      ;; Copying
		      data~alpha-copy
		      data~copy
		      
		      ;; Normalization
		      
		      ;;data~c-normalize
		      data~c-normalized-p
		      data~n-normalize
		      data~n-normalized-p
		      data~normalize
		      

		      
		      ;; Others
		      
		      data~all-unbound-symbols
		      data~all-variables
		      data~assoc
		      data~ground-p
		      data~free-variables
		      data~renaming-for
		      
		      ;; Replacing
		      
		      data~replace-at-position
		      data~replace-at-positions
		      data~replace-free-variables
		      data~replace-free-variables-and-rename
		      data~replace-free-variables-ntc    ;; Problemkind
		      data~replace-struct
		      data~replace-structs
		      data~replacement-check-p
		     
		      ;; Substructs and Positions

		      data~struct-at-position
		      data~position
		      data~positions
		      data~substruct-p
		      data~substruct-positions
		      data~substructs
		      data~all-substructs
		      
		      
		      data*all-classes-keyword
		      data*proto-types

		      ))

(defvar data*all-classes-keyword
        (intern 'all-classes (find-package 'keyword)))

;;; The fundamental classes in keim-3.1 are:
;;; =========================================

;;;                             data+struct
;;;                                  |
;;;                 --------------------------------
;;;                |                                |
;;;          data+primitive                     data+complex
;;;                |                                |
;;;         ---------------                  --------------------------
;;;        |               |                |             |           |
;;; data+constant    data+variable      data+appl     data+abstr  data+schema


;;; The data~defgeneric construction:
;;; =================================

#{
\subsection{Generalized Data Structures and Data Generic Functions}

The following may not be obvious on first view and may be skipped during
cursory reading.

Generalized data structures are KEIM objects that contain data structures,
which can be acessed by a generic function {\tt data$\sim$reader}. They have
to be subclasses of the class
#}

;; =============================== DATA+TOP ==============================
(eval-when (load compile eval)
(defclass data+top () 
  ()
  (:documentation "This class is the class of generalized data structures. All
 objects which could be data structures are subclasses of this one.")
  ))


(defgeneric data~reader (object)
  (declare
   (authors nesmith)
   (input "An object which represents a logical data structure.")
   (value "The concrete data structure that the input represents."
          " Data Structures may come in various forms: normal data"
          " structures, definitions, lines of a proof, etc.  This function"
          "should return the concrete data structure that the input "
          " represents, or else an error."))
  (:method (x)
           (error "~S does not represent a data structure." x))
  (:documentation "Returns a concrete data structure of an rep-object."))


#{which returns the concrete data object that is represented by the
generalized data structure.  Data generic functions are CLOS generic
functions with methods for subclasses of {\tt data+struct} and one specialized
method for {\tt data+top}, which applies the generic function itself to the
data structure obtained by {\tt data$\sim$reader}. In our example of named
data structures we would use the new mechanism by declaring a new subclass of
{\tt data+top}, that provides two slots, a name slot and a slot for a data
structure. The generic function {\tt data$~$reader} then simply consists of
the reader for the data structure slot. This way we would automatically
inherit all the functionality from the {\tt data+struct} hierarchy via {\tt
data+top}. And we would only have to provide the functionality for handling
names. 

The mechanism for data generic functions is adequate for our example, since
naming does not interact with the structure operations, therefore the
specialized subclasses of a new data category are not needed. The decision
which of the two mechanism for inheritance from the{\tt data+struct} hierarchy
depends on the concrete situation.

The definition of data generic functions is automated in KEIM by the following
specialized macro. It can be used exactly like the {\tt defgeneric} macro, but
instead of a regular CLOS generic function yields a data generic function.

#}

(defmacro data~defgeneric (name arglist &rest options)
  (declare 
   (authors nesmith)
   (input "Same syntax as DEFGENERIC. Exception: Some of the required"
          "parameters of the argument list may be enclosed in parens,"
          "signifying that they are always data structures.  No &optional"
          "or &keyword parameters may be enclosed in a list.")
   (effect "Expands into a DEFGENERIC form, with one difference.  A default"
           "method will be created, specializing for the class DATA+TOP on"
           "each parameter which was in a list.  In this new method, the"
           "function DATA~READER will be called on each of the data arguments"
           ", and if any of them are different afterwards, the function will"
           "be called again, replacing the original data arguments with the"
           "value of DATA~READER on them.  To avoid infinite loops, the"
           "function recurses only if some argument has changed. If no"
           "argument is enclosed in a list, you will get a warning."
           "One danger is that if the methods YOU define are not as"
           "specialized as the default method that is created, it will be"
           "used instead of yours.")
   (value "Same as DEFGENERIC"))
  (let* ((lambda-list-keyword-pos
          (position-if #'(lambda (x) (member x lambda-list-keywords))
                       arglist))
	 (required-args (if lambda-list-keyword-pos
                            (subseq arglist 0 lambda-list-keyword-pos)
                          arglist))
         (tail-args (if lambda-list-keyword-pos
                        (subseq arglist lambda-list-keyword-pos)
                      nil))
         (tail-args-minus-keywords
          (let ((tail-args tail-args)
                (new nil))
            (loop 
             (when (null tail-args) (return (nreverse new)))
             (cond ((eq (car tail-args) '&rest)
                    (setq tail-args (cddr tail-args)))
                   ((member (car tail-args) lambda-list-keywords)
                    (setq tail-args (cdr tail-args)))
                   (t (push (car tail-args) new)
                      (setq tail-args (cdr tail-args)))))))
         (tail-args-for-lambda-list
          (mapcar #'(lambda (x)
                      (if (member x tail-args-minus-keywords)
                          (list x nil (gensym))
                        x))
                  tail-args))
         (tail-args-for-funcall
          (let ((new nil)
                (optional-p nil))
            (dolist (x tail-args-for-lambda-list (nreverse new))
              (cond ((eq x '&optional)
                     (setq optional-p t))
                    ((symbolp x) 
                     (setq optional-p nil))
                    (t (let ((var (car x))
                             (kvar (intern (string (car x))
                                           (find-package :keyword)))
                             (svar (third x)))
                         (push
                          (if optional-p
                              `(if ,svar (list ,var) nil)
                            `(if ,svar (list ,kvar ,var) nil))
                          new)))))))
         )
    (if (every #'symbolp required-args)
        `(progn
           (warn "DATA~~DEFGENERIC used for ~A, but no data arguments were ~ declared." ',name)
           (defgeneric ,name ,arglist ,@options))
      `(defgeneric ,name ,(append (mapcar #'(lambda (x)
                                              (if (listp x) (car x) x))
                                          required-args) tail-args)
         ,@options
         (:method ,(append (mapcar #'(lambda (x) 
                                       (if (listp x)
                                           (list (car x) 'data+top)
                                         x))
                                   required-args)
                           tail-args-for-lambda-list
                           )
                  (let ((req-args (list ,@(mapcar #'(lambda (x) (if (listp x)
                                                                    (car x)
                                                                  x))
                                                  required-args)))
                        (new-required-args 
                         (list ,@(mapcar 
                                  #'(lambda (x)
                                      (if (listp x) 
                                          (list 'data~reader (car x))
                                        x))
                                  required-args)))
                        (optional-and-keyword-args
                         ,(if tail-args-for-funcall
                              (cons 'nconc tail-args-for-funcall)
                            nil)))
                    (if (every #'eq req-args new-required-args)
                        (error "Default method defined by DATA~~DEFGENERIC makes no progress using DATA~~READER: function is ~S, arguments are ~S"
                               ',name 
                               (append req-args optional-and-keyword-args))
                      (apply #',name ,(if tail-args-minus-keywords
                                          `(nconc new-required-args
                                                  optional-and-keyword-args)
                                        'new-required-args)))))))))

(defmethod doc~parse-def ((type (eql 'data~defgeneric)) def)
  (multiple-value-bind (name desc args doc-items)
      (doc~parse-def 'defgeneric def)
    (declare (ignore desc))
    (values 
     name 
     "Data Generic Function" 
     args
     (let* ((data-args (remove-if-not #'listp (third def)))
            (exactly-one-data-arg (null (cdr data-args))))
       doc-items))))

(pp~modify-style doc+latex-interface
  ((cons (member data~defgeneric) doc+interface)
   doc~latex-lispdef
   15))

(pp~modify-style doc+latex-complete
  ((cons (member data~defgeneric))
   doc~latex-lispdef
   20))



#{
\subsection{The Classes of Concrete Data Structures}

The top class of the data structure hierarchy is the class #} 

(eval-when (load compile eval)
  (defclass data+struct (data+top keim+object)
    ((binding       :initform nil
                    :initarg :binding
                    :accessor data=binding)
     (annotation    :initform nil
                    :initarg :annotation
                    :accessor data=annotation)
     (plist         :accessor data=plist)
     (binding-table :initform nil
                    :initarg :binding-table
                    :accessor data~binding-table)
     (label         :initform nil
                    :accessor data=label)
     (status        :accessor data=status
                    :initarg :status
                    :documentation "purely internal: normalization status"))
     ;;; >>> Kappa-Slot entfaellt komplett
    (:documentation "This is the uppermost class for concrete data structs")))

;;; the status info is relevant for all procedures that expect data in a
;;; certain form. It is kept in order to allow the user to explicitly manage
;;; normal forms and  keep control over when things are normalized.
;;; For the default case (n-normalization) is handled properly if no keywords\;;; are given.
;;; The meaning of the status info is:
;;; :n-normalized / :c-normalized  (self explanatory)
;;; nil   nothing known, i.e. procedures depending on normalization do have
;;;       to normalize
;;; T     n- AND c-normalized (for primitives)

#{ of which all KEIM data structures are instances. Parallel to the
{\tt data+struct} hierarchy there is the hierarchy of concrete data objects,
which is defined by its top class #}


(eval-when (load compile eval)
  (defclass data+object (keim+object)
    ()
    (:documentation "The superclass of concrete data obj. categories in KEIM")
    ))


#{ It is very important to note that even though all KEIM data structures
inherit the functionality from the {\tt data+struct} hierarchy, they will not
be direct instances of this class. Instead, they are instances of specialized
concrete subclasses in the {\tt data+object} hierarchy that specialize the
functionality inherited from the {\tt data+struct} hierarchy to the concrete
object category at hand. For a more detailed discussion on this
see~\ref{data:objects}. #}

#{ In KEIM data structures are divided into primitive and complex data
structures. #}

(eval-when (load compile eval)
  (defclass data+primitive (data+struct keim+name)
    ((status :initform T))
    (:documentation "Primitive data structures (always have a name).")))

#{Primitive data structures are the base case of the inductive construction
of data structures, since they cannot be further subdivided into data
structures. This class can further be subdivided in to the classses #}

(eval-when (load compile eval)
  (defclass data+constant (data+primitive)
    ((origin    :initform nil
                :accessor data=constant-origin
                :initarg :origin)
     ;;(instances :initform nil
     ;;           :accessor data=constant-inst
     ;;           :initarg :instances)
     )
    (:documentation "Constant primitive data structures.")))

#{for constant data structures (data structures that cannnot be instantiated
by substitutions) and #}
 
(eval-when (load compile eval)
  (defclass data+variable (data+primitive)
    ((context :initform nil
              :initarg :context
              :accessor data=variable-context))
    (:documentation "Variable primitive data structures.")))

#{for variable data structures (data structures that can be instantiated by
substitutions).#} 

#{The complex data structures #}

(eval-when (load compile eval)
  (defclass data+complex (data+struct)
    ((status :initform nil))
    (:documentation "Compound data structs,ie. abstraction and applications.")
    ))

#{consist of applications and abstractions both of which are pairs $\langle
A,(B^1\ldots B^n)\rangle$ consisting of a data structure $A$ and a list
$B=(B^1\ldots B^n)$ of data structures. In the case of applications#}

(eval-when (load compile eval)
  (defclass data+appl (data+complex)
    ((function  :initarg :function
                :accessor data=appl-function)
     (arguments :initarg :arguments
                :accessor data=appl-arguments))
    (:documentation "Data structures, which are applications.")))

#{ the data structure $A$ is called the {\bf function} and $B$ the
{\bf list of arguments} of $A$. In the case fo abstractions #}

(eval-when (load compile eval)
  (defclass data+abstr (data+complex)
    ((domain :initarg :domain
             :accessor data=abstr-domain)
     (range  :initarg :range
             :accessor data=abstr-range))
    (:documentation "Data structures, which are abstractions.")))


(eval-when (load compile eval)
  (defclass data+schema (data+complex)
    ((kappa  :initarg :domain
             :accessor data=schema-domain)
     (datum  :initarg :datum
             :accessor data=schema-range))
    (:documentation "Data structs, which are able to realize polymorphism.")))

#{$A$ is called the {\bf range} and $B$ the {\bf domain}. 
The class of local variables is subdivided into the classes for {\bf local}
and {\bf global} variables. The former only appear dominated by an abstraction
, only have a meaning there and are invisible outside of it. Moreover local
variables may not be instantiated, but are subject to alphabetic renaming
(cf.~\ref{data:alphabeta}), while global ones may be instantiated and but not
renamed.#}

#{\subsection{Test Predicates}
\subsubsection{Determining the Class of Objects}
#}

(defun data~general-struct-p (object)
  (declare (edited  "23-JAN-1995")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a generalized data structure"
                    "i.e. in data+top. Else: nil."))
  (typep object 'data+top))

(defun data~struct-p (object)
  (declare (edited  "19-NOV-1994")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a concrete data struct, nil else."))
  (typep object 'data+struct))

(defun data~object-p (object)
  (declare (edited  "19-NOV-1994")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a data object, nil else."))
  (typep object 'data+object))

(defun data~primitive-p (object)
  (declare (edited  "18-JAN-1995")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a primitive data-struct, nil else"))
  (typep object 'data+primitive))

(defun data~complex-p (object)
  (declare (edited  "18-JAN-1995")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a compound data-struct, nil else"))
  (typep object 'data+complex))

(defun data~constant-p (object)
  (declare (edited  "18-JAN-1995")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a constant data-struct, nil else"))
  (typep object 'data+constant))

(defun data~variable-p (object)
  (declare (edited  "18-JAN-1995")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "T, if the object is a variable data-struct, nil else"))
  (typep object 'data+variable))

(defun data~appl-p (object)
  (declare (edited  "17-NOV-1994")
           (authors Fehrer RICHTS)
           (input   "An object.")
           (effect  "None.")
           (value   "True iff object is an application."))
  (typep object 'data+appl))

(defun data~abstr-p (object)
  (declare (edited  "17-NOV-1994")
           (authors Fehrer )
           (input   "An object.")
           (effect  "None.")
           (value   "True iff object is an abstraction."))
  (typep object 'data+abstr))

(defun data~schema-p (object)
  (declare (edited  "17-NOV-1994")
           (authors Fehrer )
           (input   "An object.")
           (effect  "None.")
           (value   "True iff object is a schema."))
  (typep object 'data+schema))


#{
\subsection{Selectors and Destructive Modifiers for all data structures}
\subsubsection{Selectors}
#}

(data~defgeneric data~binding ((datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a data structure")
           (effect  "none")
           (value   "the contents of the binding-slot"))
  (:method ((datum data+struct)) (data=binding datum)))

(data~defgeneric data~plist ((datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a data structure")
           (effect  "none")
           (value   "the property list of the datum"))
  (:method ((datum data+struct))
           (data=plist datum)))

(data~defgeneric data~label ((datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a data structure")
           (effect  "none")
           (value   "the label of the datum"))
  (:method ((datum data+struct))
           (data=label datum)))

#{\subsubsection{Modifiers}#}

(data~defgeneric (setf data~binding) (new-binding (datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a new binding and a data structure")
           (effect  "sets the binding slot of the datum to new binding")
           (value   "the new contents of the binding-slot"))
  (:method (new-binding (datum data+struct))
           (setf (data=binding datum) new-binding)))

(data~defgeneric (setf data~plist) (new-plist (datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a new property list and a data structure")
           (effect  "sets the property list of the datum to the new one")
           (value   "the new property list"))
  (:method ((new-plist list) (datum data+struct))
           (setf (data=plist datum) new-plist)))

(data~defgeneric (setf data~label) (new-label (datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a new label and a data structure")
           (effect  "sets the label of the datum to new label")
           (value   "the new label"))
  (:method (new-label (datum data+struct))
           (setf (data=label datum) new-label)))

#{
\subsection{Selectors and Constructors for Particular Structures}
\subsubsection{Primitive Data Structures}
Creating primitive data structures is somewhat tricky. All the creation
procedures have as first argument a so called {\em reference term}, which
simply is an existing structure of the same type, which serves as a reference
to tell the creation function what particular class the new structure belongs
to (e.g. {\tt term+variable} instead of {\tt data+variable}). In some cases
there are no reference terms available, e.g. when creating the first structure
of a given type. Therefore there exists the possibility to replace the
reference term by a {\em CLOS class} or a symbol representing that class.
Example: if {\tt term-1} is of class {\tt term+constant}, then
{\tt (data$\sim$constant-create term-1 :name 'hugo :annotation (type~i))} and
{\tt (data$\sim$constant-create 'term+constant :name 'hugo :annotation
(type~i))} mean the same. A hint for programmers: only the reference term
versions need to be specialized!#}

(defvar data*proto-types nil)
;;; contains one generic reference object for each structure class used so far

(data~defgeneric data~constant-create ((refterm) &key name)  
  (declare (edited  "22-MAR-1995")
           (authors Fehrer)
           (input   "a reference term (or a class), optionally a name.")
           (effect  "creates a new constant")
           (value   "the constant"))
  (:method ((refterm data+struct) &key name)
	   (declare (ignore name))
           (error "The ref-term must be a subtype of data+constant"))
  (:method ((refterm data+constant) &key (name (gensym)))
           (make-instance (class-of refterm) :name name))
  (:method ((refterm symbol) &key (name (gensym)))
           (data~constant-create (find-class refterm) :name name))
  (:method ((refterm standard-class) &key (name (gensym)))
           (if (or (eq refterm (find-class 'data+constant))
                   (subtypep refterm 'data+constant))
               ;necessary because contrary to Common Lisp Definition subtypep
               ;is not reflexive!  DEF
               (let ((found (some #'(lambda (x)
                                      (if (eq (class-of x) refterm) x))
                                  data*proto-types)))
                 (if found
                     (data~constant-create found :name name)
                   (let ((new (make-instance refterm :name name)))
                     (progn (push new data*proto-types)
                            new))))            
             (error "the class given for a data~~constant-create must be a subclass of data+constant"))))

(data~defgeneric data~constant-origin ((datum))
  (declare (edited  "22-JAN-1998")
           (authors Gebhard)
           (input   "A data-structure")
           (effect  "None")
           (value   "If datum is a constant, its origin"))
  (:method ((datum data+constant))
           (data=constant-origin datum)))


(data~defgeneric (setf data~constant-origin) (new-origin (datum))
  (declare (edited  "22-JAN-1998")
           (authors Gebhard)
           (input   "A data structure")
           (effect  "The the origin slot of an costant to new-origin.")
           (value   "The chenged datum"))
  (:method (new-origin (datum data+constant))
           (setf (data=constant-origin datum) new-origin)))


(data~defgeneric data~variable-create ((refterm) &key name)  
  (declare (edited  "22-MAR-1995")
           (authors Fehrer)
           (input   "a reference term (or a class), optionally a name.")
           (effect  "creates a new variable")
           (value   "the variable"))
  (:method ((refterm data+struct) &key name)
	   (declare (ignore name))
           (error "the ref-term has to be of a subtype of data+variable!"))
  (:method ((refterm data+variable) &key (name (gensym)))
           (make-instance (class-of refterm) :name name))
  (:method ((refterm symbol) &key (name (gensym)))
           (data~variable-create (find-class refterm) :name name))
  (:method ((refterm standard-class) &key (name (gensym)))
           (if (or (eq refterm (find-class 'data+variable))
                   (subtypep refterm 'data+variable))
               ;necessary because contrary to Common Lisp Definition subtypep
               ;is not reflexive!  DEF
               (let ((found (some #'(lambda (x)
                                      (if (eq (class-of x) refterm) x))
                                  data*proto-types)))
                 (if found
                     (data~variable-create found :name name)
                   (let ((new (make-instance refterm :name name)))
                     (progn (push new data*proto-types)
                            new))))            
             (error "the class given for a data~~variable-create must be a subclass of data+variable"))))


#{
\subsubsection{Complex Data Structures}
#}

#{
In order to understand the rationale behind the following selector functions,
you should have some knowledge of type theory. If you are only used to first
order terms, you will probably only need the functions
{\tt data$\sim$appl-arguments}, {\tt data$\sim$appl-function},
{\tt data$\sim$abstr-bvars} and {\tt data$\sim$abstr-range} and not bother
about the rest.
For type theorists:
By default, within KEIM data structures are internally kept in {\em
n-normal-form}. Therefore primitive slot-access is guaranteed only if you
view at data that very way, and anything involving c-normal-forms is bound to
be costly.
However, there are possibilities to restructure (convert) data on demand, but
anybody using these features does so at his own risk (e.g. positions within
terms once recorded will not be valid after a restructure operation).
Because of the normalization, the following are no genuine selector/
constructor pairs. However, since no automatic $\beta$-conversion or similar
actions take place, the result of an {\tt data$\sim$abstraction-create} is
guaranteed to always be an abstraction, as well as the result of an
{\tt data$\sim$application-create} is always an application.
Nevertheless, if A is itself an application, the first component (domain) of
{\tt (application-create B A)} is {\em not} B, but a list with B as first
element!
An example may serve to clarify this:
Let $A = (f a b)$.
Then
\begin{tabular}{l@{$=$}ll}
{\tt data$\sim$appl-n-function}$(A)$  & $f$     & f is guaranteed to be
{\em no} application!\\
{\tt data$\sim$appl-n-arguments}$(A)$ & $(a b)$ & This is a list!\\
{\tt data$\sim$appl-c-function}$(A)$  & $(f a)$\\
{\tt data$\sim$appl-c-argument}$(A)$  & $b$     & The last argument
\end{tabular}
In analogy to this, let A = $\lambda{x_1}\ldots{x_n}$.B.
Then
\begin{tabular}{l@{$=$}ll}
{\tt data$\sim$abstr-domain}$(A)$ & ($\lambda{x_1}\ldots{x_n}$)  &  a list!\\
{\tt data$\sim$abstr-range}$(A)$  & $B$                            & B is guaranteed to be no abstraction!\\
{\tt data$\sim$abstr-c-domain}$(A)$ & $x_1$                          &``the'' bound variable\\
{\tt data$\sim$abstr-c-range}$(A)$  & $\lambda{x_2}\ldots{x_n}$.B
\end{tabular}
#}
#{
In contrast to the case of primitive data structures the creation procedures
for the complex one do not need a reference term, for the class of the
compound structure can be determined by looking at the components. This has
to be done within the methods to be included in the respective modules (e.g.
{\tt type+type} or {\tt term+term}). The same holds for the handling of
annotations, which is peculiar to the respective term systems. There also the
check for applicability belongs.
The resulting objects all share parts of their structure with the original
arguments. If this is not intended, an explicit {\tt data~copy} has to be
performed!
The creation procedures all have (optional) {\em mode} and {\em destructive}
arguments, with the following meaning:
\begin{itemize}
\item The {\bf mode} argument may be {\tt :n-normalize} (which is the default), {\tt :c-normalize},
or {\tt :conserve}. This means, that the structure delivered by the creator function is either
{\em n-normalized} (every application has as many arguments as possible, the binder of abstractions
is as long as possible), {\em c-normalized} (argument lists and binder always consist of single
items), or the structure of the input components is simply taken over unaltered.
\item If {\bf destructive} is non-nil, parts of the input arguments are destructively modified (e.g.
renormalized) in order to construct the new structure. In the {\tt nil} case (the default)
everything is copied.
\end{itemize}
#}

(data~defgeneric data~appl-create ((function) arguments &key destructive mode)
  (declare (edited  "05-JAN-1998")
           (authors Gebhard)
           (input   "a function and a list of arguments")
           (effect  "creates an application, details depend on keyword args.")
           (value   "the new application")
           (comment "see above for keyword arguments"))
  (:method ((function data+struct) arguments &key destructive
                                                  (mode :n-normalize))
           (data~appl-create function (list arguments)
                             :destructive destructive
                             :mode mode))
  (:method ((function data+struct) (arguments list) &key destructive
                                                         (mode :n-normalize))
           (data~normalize (make-instance 'data+appl
                                          :function function
                                          :arguments arguments)
                           :mode mode
                           :destructive destructive
                           :share-vars t)))

(data~defgeneric data~abstr-create (domain (range) &key destructive mode)
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "A binder (struct or list of structs) and a scope")
           (effect  "Creates an abstraction, details depend on keyword args")
           (value   "A new abstraction")
           (comment "See above for keyword arguments"))
  (:method ((domain data+struct) (range data+struct) &key destructive
                                                          (mode :n-normalize))
           (data~abstr-create (list domain)
                              range
                              :destructive destructive
                              :mode mode))
  (:method ((domain list) (range data+struct) &key destructive
                                                   (mode :n-normalize))
           (data~normalize (make-instance 'data+abstr
                                          :range range
                                          :domain domain)
                           :mode mode
                           :destructive destructive
                           :share-vars t)))

(data~defgeneric data~schema-create ((datum) (kappa) &key destructive mode) 
  (declare (edited  "28-OCT-1998")
           (authors Gebhard)
           (input   "")
           (effect  "")
           (value   ""))
  (:method ((datum data+struct) (kappa data+struct) &key destructive
                                                        (mode :n-normalize))
          (data~schema-create (list kappa)
                              datum
                              :destructive destructive
                              :mode mode))
  (:method ((datum data+struct) (kappa list) &key destructive
                                                  (mode :n-normalize))
           (data~normalize (make-instance 'data+schema
                                          :domain kappa
                                          :datum datum)
                           :mode mode
                           :destructive destructive
                           :share-vars t)))
                              

#{
There are a lot of selector functions, which come in to different categories.
Some are generally fast, because they simply look up the requested
component according to the structure given and don't involve any
renormalizations.
Some may take some time, because they may have to reorganize things. They give
back the requested value how it would be like, if the structure were in a
particular normal form. They do, however {\em not} modify the structure
itself, but create a copy. This is done every time they are used!
If you want to keep forms in different normal forms you should keep a
restructured version for yourself.
Note: the result itself is {\em not} submitted to transformation. So e.g.
{\tt data$\sim$appl-c-function} yields the function part corresponding
to a view on the structure according to c-normal-form, but this structure
itself may of course be in n-normal-form or unnormalized.
#}


(data~defgeneric data~appl-function ((datum))
  (declare (edited  "17-JAN-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the function of the application"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (data=appl-function datum)))

(data~defgeneric data~appl-arguments ((datum))
  (declare (edited  "17-JAN-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the argument list of the application"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (data=appl-arguments datum)))

(data~defgeneric data~appl-n-function ((datum))
  (declare (edited  "02-FEB-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the function of the n-normal form, i.e. no application")
           (comment "but can be an abstraction not in n-normal form"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (let ((fun (data=appl-function datum)))
             (if (data~appl-p fun)
                 (data~appl-n-function fun)
               fun))))

(data~defgeneric data~appl-n-arguments ((datum))
  (declare (edited  "02-FEB-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the argument list of the n-normal form")
           (comment "the items themselves are not normalized!"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (let ((fun (data=appl-function datum)))
             (if (data~appl-p fun)
                 (append (data~appl-n-arguments fun)
                         (data=appl-arguments datum))
               (data=appl-arguments datum)))))

(data~defgeneric data~appl-c-function ((datum))
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the function of the c-normal form, thought of as with a"
                    "singular argument list")
           (comment "but may itself be not in c-normal form"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (if (null (cdr (data=appl-arguments datum)))
               (data=appl-function datum)
             (data~appl-create (data=appl-function datum)
                               (butlast (data=appl-arguments datum))
                               :mode :conserve))))

(data~defgeneric data~appl-c-argument ((datum))
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "an application")
           (effect  "none")
           (value   "the argument of the c-normal form, i.e. a single"
		    "argument (not a list!)")
           (comment "but may itself be not in c-normal form"))
  (:method ((datum data+struct))
           (error "argument must be an application"))
  (:method ((datum data+appl))
           (car (last (data=appl-arguments datum)))))

;; ------------------------------------------------------------------------

(data~defgeneric data~abstr-domain ((datum))
  (declare (edited  "17-JAN-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "The domain (the binder!), a list!"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (data=abstr-domain datum)))

(data~defgeneric data~abstr-range ((datum))
  (declare (edited  "17-JAN-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "The range (the scope!)"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (data=abstr-range datum)))

(data~defgeneric data~abstr-n-range ((datum))
  (declare (edited  "02-FEB-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "the range of the n-normal form, i.e. no abstraction")
           (comment "but can be an application not in n-normal form"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (let ((ran (data=abstr-range datum)))
             (if (data~abstr-p ran)
                 (data~abstr-n-range ran)
               ran))))

(data~defgeneric data~abstr-n-domain ((datum))
  (declare (edited  "02-FEB-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "the domain (binder) of the n-normal form")
           (comment "the items themselves are not normalized!"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (let ((ran (data=abstr-range datum)))
             (if (data~abstr-p ran)
                 (append (data=abstr-domain datum) (data~abstr-n-domain ran))
               (data=abstr-domain datum)))))

(data~defgeneric data~abstr-c-domain ((datum))
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "the domain (binder) of the c-normal-form, i.e. a single variable (not a list!)"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (car (data=abstr-domain datum))))

(data~defgeneric data~abstr-c-range ((datum))
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "an abstraction")
           (effect  "none")
           (value   "the range (scope) of the c-normal-form, thought of as with a singular bound variable")
           (comment "but may itself be not in c-normal-form"))
  (:method ((datum data+struct))
           (error "argument must be an abstraction"))
  (:method ((datum data+abstr))
           (if (null (cdr (data=abstr-domain datum)))
               (data=abstr-range datum)
             (data~abstr-create (cdr (data=abstr-domain datum))
                                (data=abstr-range datum) 
                                :mode :conserve))))

;; ------------------------------------------------------------------------

(data~defgeneric data~schema-domain ((datum))
  (declare (edited  "28-OCT-1998")
           (authors gebhard)
           (input   "a schema")
           (effect  "none")
           (value   "The kappas of the schema, a list!"))
  (:method ((datum data+struct))
           (error "argument must be a schema"))
  (:method ((datum data+schema))
           (data=schema-domain datum)))

(data~defgeneric data~schema-range ((datum))
  (declare (edited  "28-OCT-1998")
           (authors gebhard)
           (input   "a schema")
           (effect  "none")
           (value   "The datum of the schema"))
  (:method ((datum data+struct))
           (error "argument must be a schema"))
  (:method ((datum data+schema))
           (data=schema-range datum)))

;; ------------------------------------------------------------------------

(defgeneric data~n-range (datum)
  (declare (edited  "22-MAR-1996 14:18")
           (authors GKLEIN)
           (input   "A datum.")
           (effect  "None.")
           (value   "The data~abstr-n-range of DATUM if DATUM is an "
                    "abstraction,"
                    "DATUM if datum is primitive, and NIL if datum is an"
                    "application."))
  (:method ((datum data+primitive))
           datum)
  (:method ((datum data+abstr))
           (data~abstr-n-range datum))
  (:method ((datum data+appl))
           datum))

(defgeneric data~n-domain (datum)
  (declare (edited  "22-MAR-1996 14:19")
           (authors GKLEIN)
           (input   "A datum.")
           (effect  "None.")
           (value   "The data~abstr-n-domain of DATUM if DATUM is an"
                    "abstraction,"
                    "NIL if datum is primitive or an application."))
  (:method ((datum data+primitive))
           nil)
  (:method ((datum data+abstr))
           (data~abstr-n-domain datum))
  (:method ((datum data+appl))
           nil))

#{
In order to make things easier for those who are not used to type theory and
lambda calculus, there are a bunch of macros that give other versions of
access to data structures, some of which are particularly interesting for
those who want to only deal with first order terms and are used to the t
erminology found there.
#}


(defun data~appl-last-argument (datum)
  (data~appl-c-argument datum))

(defun data~abstr-c-binder (datum)
  (data~abstr-c-domain datum))

(defun data~abstr-bound-var (datum)
  (data~abstr-c-domain datum))

(defun data~abstr-scope (datum)
  (data~abstr-c-range datum))

(defun data~abstr-binder (datum)
  (data~abstr-domain datum))

(defun data~abstr-n-scope (datum)
  (data~abstr-range datum))

(defun data~c-domain (datum)
  (data~abstr-c-domain datum))

(defun data~c-range (datum)
  (data~abstr-c-range datum))

#{
As destructive slot access is only for insiders, these functions are only
available in one form. As the normalization status of structures above the
modified level may change, all these functions issue warnings if this occurs.
#}

(data~defgeneric data=computed-status ((struct))
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "a data structure or a list of data structures")
           (effect  "none")
           (value   "the normalization status of struct"))
  (:method ((struct data+struct))
           (data=status struct))
  (:method ((list list))
           (cond
            ((every #'(lambda (x) (eq (data=status x) T)) list) T)
            ((every #'(lambda (x) (eq (data=status x) :n-normalized)) list)
             :n-normalized)
            ((every #'(lambda (x) (eq (data=status x) :c-normalized)) list)
             :c-normalized)
            (T nil))))

(defun data=compatible-status (new-stat old-stat)
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "two status")
           (effect  "none")
           (value   "T, if a structure with new-stat can safely replace one"
                    "with old-stat"))
  (or (eq new-stat T) (null old-stat) (eq new-stat old-stat)))

(defun data=incompatibility-warn (struct)
  (declare (edited  "23-MAR-1995")
           (authors Fehrer)
           (input   "a data structure")
           (effect  "issues a warning concerning superstructures of struct")
           (value   "nil"))
  (declare (ignore struct))
  ;; (warn "POSSIBLE SOURCE OF INCONSISTENCY!~%"))
  nil
  )
;;oder (warn "The norm-status of structs containing ~S may not be valid any more!" struct))

#{\subsubsection{Now the modifiers for complex data structures}
#}

(data~defgeneric (setf data~appl-function) ((function) (datum)) 
  (declare (edited  "17-NOV-1994" )
	   (authors Fehrer )
	   (input   "A function and a datum" )
	   (effect  "the function slot of the application is set to the new"
		    "function" )
	   (value   "the new data structure"))
  (:method ((function data+struct) (datum data+appl))
           (progn (setf (data=appl-function datum) function)
                  (let ((ostat (data=status datum)))
                    (if (or
                         (and (eq ostat :n-normalized)
                              (or (data~appl-p function)
                                  (not (data~n-normalized-p function))))    
                         (and (eq ostat :c-normalized)
                              (not (data~c-normalized-p function))))
                        (setf (data=status datum)
			      (data=incompatibility-warn datum)
			      )))
                  datum))
  (:method ((function data+schema) (datum data+appl))
           (error "You can't insert schemas into an application")))

(data~defgeneric (setf data~appl-arguments) (arguments (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "A list of data and a datum." )
           (effect  "the arguments of the datum are set to the new args" )
           (value   "the changed datum"))
  (:method (arguments (datum data+appl))
           (error "~A is not a list of arguments" arguments))
  (:method ((arguments list) (datum data+appl))
           (if (and (every #'data~struct-p arguments)
                    (not (some #'data~schema-p arguments))) 
               (progn (setf (data=appl-arguments datum) arguments)
                      (let ((ostat (data=status datum))
                            (nstat (data=computed-status arguments)))
                        (if (or
                             (and (eq ostat :n-normalized)
                                  (member nstat (list :c-normalized nil)))
                             (and (eq ostat :c-normalized)
                                  (or (member nstat (list :n-normalized nil))
                                      (not (null (cdr arguments))))))
                            (setf (data=status datum)
                                  (data=incompatibility-warn datum))))
                      datum)
             (error "the list ~S contains non-data-structures or schemas!"
                    arguments))))

;; --------------------------------------------------------------------------

(data~defgeneric (setf data~abstr-domain) (domain (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "A list of data and a datum." )
           (effect  "the binder of the datum is set to the new domain(list)" )
           (value   "the changed datum"))
  (:method (domain (datum data+abstr))
           (error "~A is not a list of arguments" domain))
  (:method ((domain list) (datum data+abstr))
           (if (and (every #'data~struct-p domain)
                    (not (some #'data~schema-p domain)))
               (progn (setf (data=abstr-domain datum) domain)
                      (let ((ostat (data=status datum))
                            (nstat (data=computed-status domain)))
                        (if (or
                             (and (eq ostat :n-normalized)
                                  (member nstat (list :c-normalized nil)))
                             (and (eq ostat :c-normalized)
                                  (or (member nstat (list :n-normalized nil))
                                      (not (null (cdr domain))))))
                            (setf (data=status datum)
                                  (data=incompatibility-warn datum))))
                      datum)
             (error "the list ~S contains non-data-structures or schemas!"
                    domain))))

(data~defgeneric (setf data~abstr-range) ((range) (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "a scope and an abstraction" )
           (effect  "the scope of the abstraction is set to the new range" )
           (value   "the new data structure"))
  (:method ((range data+struct) (datum data+abstr))
           (progn (setf (data=abstr-range datum) range)
                  (let ((ostat (data=status datum)))
                    (when (or
			   (and (eq ostat :n-normalized)
				(or (data~abstr-p range)
				    (not (data~n-normalized-p range))))  
			   (and (eq ostat :c-normalized)
				(not (data~c-normalized-p range))))
		      (setf (data=status datum)
			    (data=incompatibility-warn datum))))
                  datum))
  (:method ((range data+schema) (datum data+abstr))
           (error "You can't insert a schema ino an abstraction.")))

;; --------------------------------------------------------------------------

(data~defgeneric (setf data~schema-domain) (newkappa (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "A list of data and a datum." )
           (effect  "the kappas of the datum are set to newkappa")
           (value   "the changed datum"))
  (:method (newkappa (datum data+schema))
           (error "~A is not a list of arguments" newkappa))
  (:method ((newkappa list) (datum data+schema))
           (if (every #'data~struct-p newkappa)
               (progn (setf (data=schema-domain datum) newkappa)
                      (let ((ostat (data=status datum))
                            (nstat (data=computed-status newkappa)))
                        (when (or
			       (and (eq ostat :n-normalized)
				    (member nstat (list :c-normalized nil)))
			       (and (eq ostat :c-normalized)
				    (member nstat (list :n-normalized nil))))
			  (setf (data=status datum)
				(data=incompatibility-warn datum))))
                      datum)
             (error "the list ~S contains non-data-structures!" newkappa))))

(data~defgeneric (setf data~schema-range) ((newdatum) (datum)) 
  (declare (edited  "17-NOV-1994" )
           (authors Fehrer )
           (input   "a datum and an schema" )
           (effect  "the datum of the schema is set to newdatum" )
           (value   "the new data structure"))
  (:method ((newdatum data+struct) (datum data+schema))
           (progn (setf (data=schema-range datum) newdatum)
                  (let ((ostat (data=status datum)))
                    (when (or
			   (and (eq ostat :n-normalized)
				(not (data~n-normalized-p newdatum)))          
			   (and (eq ostat :c-normalized)
				(not (data~c-normalized-p newdatum))))
		      (setf (data=status datum)
			    (data=incompatibility-warn datum))))
		  datum)))
  
#{\subsection{Normalization Stuff}
\subsubsection{Checking the Normalization Status of Objects}

The following predicates have to be used with some caution. In order to avoid
repeated traversal of huge data structures, the normalization status is
internally kept in certain labels which simply are looked up when needed. All
the access functions correctly update this information. If you use your own
access functions (which you shouldn't do!) or if you destructively modify
substructures of bigger structures, the respective information in the
structures above may not be correct afterwards and the predicates below give 
the wrong answers! It is your responsibility in those cases to explicitly
renormalize your structures using the {\tt nobelieve} parameter in the
renormalization functions or to use that same parameter in the test functions
to enforce checking the complete structure.
If you always use the same normal forms in all circumstances you need not
bother about all this stuff.
#}

(data~defgeneric data~n-normalized-p ((datum) &key nobelieve)
  (declare (edited  "26-JAN-1995")
           (authors Fehrer)
           (input   "a datum")
           (effect  "none")
           (value   "non-nil, if the datum is in n-normal-form"))
  (:method ((datum data+primitive) &key nobelieve)
           (declare (ignore nobelieve))
           T)
  (:method ((datum data+appl) &key nobelieve)
           (if nobelieve
               (and (not (data~appl-p (data~appl-function datum)))
                    (data~n-normalized-p (data~appl-function datum) nobelieve)
                    (every #'(lambda (x) (data~n-normalized-p x nobelieve))
                           (data~appl-arguments datum)))
             (member (data=status datum) (list T :n-normalized))))
  (:method ((datum data+abstr) &key nobelieve)
           (if nobelieve
               (and (not (data~abstr-p (data~abstr-range datum)))
                    (data~n-normalized-p (data~abstr-range datum) nobelieve)
                    (every #'(lambda (x) (data~n-normalized-p x nobelieve))
                           (data~abstr-domain datum)))
             (member (data=status datum) (list T :n-normalized))))
  (:method ((datum data+schema) &key nobelieve)
           (if nobelieve
               (data~n-normalized-p (data=schema-range datum) nobelieve)
             (member (data=status datum) (list T :n-normalized)))))

(data~defgeneric data~c-normalized-p ((datum) &key nobelieve)
  (declare (edited  "26-JAN-1995")
           (authors Fehrer)
           (input   "a datum")
           (effect  "none")
           (value   "T, if the datum is in c-normal-form"))
  (:method ((datum data+primitive) &key nobelieve)
           (declare (ignore nobelieve))
           T)
  (:method ((datum data+appl) &key nobelieve)
           (if nobelieve
               (and (null (cdr (data=appl-arguments datum)))
                    (data~c-normalized-p (data=appl-function datum) nobelieve)
                    (every #'(lambda (x) (data~c-normalized-p x nobelieve))
                           (data~appl-arguments datum)))
             (member (data=status datum) (list T :c-normalized))))
  (:method ((datum data+abstr) &key nobelieve)
           (if nobelieve
               (and (null (cdr (data=abstr-domain datum)))
                    (data~c-normalized-p (data=abstr-range datum) nobelieve)
                    (every #'(lambda (x) (data~c-normalized-p x nobelieve))
                           (data~abstr-domain datum)))
             (member (data=status datum) (list T :c-normalized))))
  (:method ((datum data+schema) &key nobelieve)
           (if nobelieve
               (data~c-normalized-p (data=schema-range datum) nobelieve)
             (member (data=status datum) (list T :c-normalized)))))

#{\subsubsection{Restructuring}

The following functions return a given datum in a restructured form, i.e.
either normalized or with a particular strategy of structure sharing applied.
This can be done constructively (the default), i.e. a copy is created with
the desired properties (this copy however may share parts with the original).
By giving a non-nil value to the {\tt destructive} parameter, the original is
destructively modified and then returned. This concerns {\em only the top
level} of the structure, however. Care must be taken when destructively
modifying subterms of a bigger structure, as the status information of the
containing term may no longer be valid!
#}

;; Ueberlegungnen zum normalisieren (sharing und destructiv parameter)
;; Prinzipiell kann man konstruktiv und destruktive normalisieren
;; Macht man es konstruktiv, dann komplett konstruktiv
;; Denn: Werden Unterterme geshared, d.h. ab einer bestimmten Ebene wird
;; destruktiv gearbeitet, so wuerden diese in den anderen Termen (d.h. denen
;; die sich diesen Unterterm mit dem neuen normalisierten Teilen) destruktiv
;; veraendert, insbesondere wuerde sich der Normalisierungsstatus des
;; Unterermes aendern und verfaelschen. 
;; Macht man es destructiv, so kann man bestimmen, ob nicht doch tiefere
;; ebenen vervielfaeltigt werden sollen, wegen dem gleichen Argument wie oben.

;; Die Parameter haben also folgende Absichten:
;; destructive: Ja oder nein
;;
;; constructive-from: Im dem Fall, dass destructive t ist kann mit diesen
;; Parameter eine Liste von Klassen angegeben werden, ab deren
;; Erscheinen construcitv fortgefahren wird
;;
;; share-vars: Im Fall des Konstruktiven arbeitens kann man bestimmen,
;; ob Variablen beibehalten werden sollen oder nicht, d.h. durch neue ersetzt
;; werden sollen


(defun data~normalize (datum &key mode
                                  destructive
                                  constructive-from
                                  share-vars)
  (declare (edited  "28-OCT-1998")
           (authors Gebhard)
           (input   "A datum, a mode, destructive keyword combination.")
           (effect  "If destructive, the struct will be changed.")
           (value   "The mode and keywords directed normalized struct."))
  (ecase mode
    ((:conserve)
     (if destructive datum
       (data~copy datum :downto (list 'data+variable))))
    ((:n-normalize)
     (data~n-normalize datum
                       :destructive destructive
                       :constructive-from constructive-from
                       :share-vars share-vars))
    ((:c-normalize) (error "c-normalization not yet implemented."))))

(defun data~n-normalize (datum &key destructive
                                    constructive-from
                                    share-vars)
  (declare (edited  "28-OCT-1998")
           (authors Gebhard)
           (input   "A datum, and a destructive keyword combination.")
           (effect  "Depends on destructive: Changing the datum.")
           (value   "The n-normalized datum"))
  (car (data=n-normalize datum destructive constructive-from nil share-vars)))

(data~defgeneric data=n-normalize
                 ((datum) destr constr-from renaming share-vars)
  (declare (edited  "28-OCT-1998")
           (authors Gebhard)
           (input   "A Parameter set of an data~~n-norm... function call")
           (effect  "Dependig on the parameters")
           (value   "A list with: The resulting datum and the realized"
                    "renaming"))
   (:method ((datum data+constant) constr from renaming share-vars)
           (declare (ignore constr from share-vars))
           (list datum renaming))
   (:method ((datum data+variable) destr constr-from renaming share-vars)
           (cond ((and destr
                       (some #'(lambda (x) (typep datum x)) constr-from))
                  (data=n-normalize datum
                                    nil
                                    constr-from
                                    renaming
                                    share-vars))
                 ((or destr
                      share-vars)
                  (if (assoc datum renaming)
                      (list (cdr (assoc datum renaming)) renaming)
                    (list datum renaming)))
                 (t
                  (if (assoc datum renaming)
                      (list (cdr (assoc datum renaming)) renaming)
                    (let* ((new-type (if (data~annotation datum)
                                         (list (data~copy
                                                (data~annotation datum)
                                                :downto (list 'data+variable))
                                               renaming)
                                       (list nil renaming)))
                           (new-var (make-instance (class-of datum)
                                                   :name (gensym "nn")
                                                   :binding
                                                    (data~binding datum)
                                                   :binding-table
                                                    (data~binding-table datum)
                                                   :annotation
                                                    (car new-type)
                                                   :status T)))
                      (list new-var (acons datum new-var (cadr new-type)))
                    )))))
   (:method ((datum list) constr constr-from renaming share-vars)
            (if datum
                (let* ((first-el (data=n-normalize (car datum)
                                                   constr
                                                   constr-from
                                                   renaming
                                                   share-vars))
                       (rest-els (data=n-normalize (cdr datum)
                                                   constr
                                                   constr-from
                                                   (cadr first-el)
                                                   share-vars)))
                  (list (append (list (car first-el)) (car rest-els))
                        (cadr rest-els)))
              (list nil renaming)))
   (:method ((datum data+appl) destr constr-from renaming share-vars)
            (cond ((data~n-normalized-p datum) (list datum renaming))
		  ((and destr
                        (some #'(lambda (x) (typep datum x)) constr-from))
                   (data=n-normalize datum nil constr-from
                                     renaming share-vars)) 
                  (destr
                   (let* ((newfun
                           (data=n-normalize (data=appl-function  datum)
                                             t
                                             constr-from
                                             renaming
                                             share-vars))
                          (newargs
                           (data=n-normalize (data=appl-arguments datum)
                                             t
                                             constr-from
                                             (cadr newfun)
                                             share-vars)))
                     (if (data~appl-p (car newfun))
                         (progn
                           (setf (data=appl-function datum)
                                 (data=appl-function (car newfun)))
                           (setf (data=appl-arguments datum)
                                 (append
                                  (data=appl-arguments (car newfun))
                                  (car newargs)))
                           (when (data~annotation datum)
                             (setf (data~annotation datum)
                                   (data~n-normalize
                                    (data~annotation datum)
                                    :share-vars t)))
                           (setf (data=status datum) :n-normalized)
                           (list datum (cadr newargs)))
                       (progn
                         (setf (data=appl-function datum) (car newfun))
                         (setf (data=appl-arguments datum) (car newargs))
                         (when (data~annotation datum)
                           (setf (data~annotation datum)
                                 (data~n-normalize (data~annotation datum)
                                                   :share-vars t)))
                         (setf (data=status datum) :n-normalized)
                         (list datum (cadr newargs))))))
                  (t 
                   (let* ((newfun  (data=n-normalize
                                    (data=appl-function datum)
                                    nil nil
                                    renaming
                                    share-vars))
                          (newargs (data=n-normalize
                                    (data=appl-arguments datum)
                                    nil nil
                                    (cadr newfun)
                                    share-vars)))
                     (if (data~appl-p (car newfun))
                         (list
                          (make-instance (class-of (car newfun))
                                         :function (data=appl-function
                                                    (car newfun))
                                         :arguments
                                          (append (data=appl-arguments
                                                   (car newfun))
                                                  (car newargs))
                                          :status :n-normalized
                                          :annotation
                                           (when (data~annotation datum)
                                             (data~n-normalize
                                              (data=annotation datum)
                                              :share-vars t)))
                          (cadr newargs))
                       (list
                        (make-instance (class-of datum)
                                       :function (car newfun)
                                       :arguments (car newargs)
                                       :status :n-normalized
                                       :annotation
                                        (when (data~annotation datum)
                                          (data~n-normalize
                                           (data=annotation datum)
                                           :share-vars t)))
                        (cadr newargs)))))))
   (:method ((datum data+schema) destr constr-from renaming share-vars)
            (cond ((data~n-normalized-p datum) (list datum renaming))
		  ((and destr
                        (some #'(lambda (x) (typep datum x)) constr-from))
                   (data=n-normalize datum
                                     nil
                                     constr-from
                                     renaming
                                     share-vars))
                  (destr
                   (let* ((newdat (data=n-normalize
                                   (data=schema-range datum)
                                   t
                                   constr-from
                                   renaming
                                   share-vars))
                          (newkap (data=n-normalize
                                   (data=schema-domain datum)
                                   t
                                   constr-from
                                   (cadr newdat)
                                   share-vars)))
                     (setf (data=schema-range datum) (car newdat))
                     (setf (data=schema-domain datum) (car newkap))
                     (list datum (cadr newkap))))
                  (t
                   (let* ((newdat (data=n-normalize
                                   (data=schema-range  datum)
                                   nil nil
                                   renaming
                                   share-vars))
                          (newkap (data=n-normalize
                                   (data=schema-domain datum)
                                   nil nil
                                   (cadr newdat)
                                   share-vars)))
                     (list
                      (make-instance (class-of datum)
                                     :domain (car newkap)
                                     :datum (car newdat)
                                     :status :n-normalized
                                     :annotation
                                     (when (data~annotation datum)
                                       (data~n-normalize
                                        (data=annotation datum)
                                        :share-vars t))))))))
   (:method ((datum data+abstr) destr constr-from renaming share-vars)
            (cond ((data~n-normalized-p datum) (list datum renaming))
		  ((and destr
                        (some #'(lambda (x) (typep datum x)) constr-from))
                   (data=n-normalize datum
                                     nil
                                     constr-from
                                     renaming
                                     share-vars))
                  (destr
                   (let* ((newran (data=n-normalize
                                   (data=abstr-range  datum)
                                   t
                                   constr-from
                                   renaming
                                   share-vars))
                          (newdom (data=n-normalize
                                   (data=abstr-domain datum)
                                   t constr-from
                                   (cadr newran)
                                   share-vars)))
                     (if (data~abstr-p (car newran))
                         (progn
                           (setf (data=abstr-range datum)
                                 (data=abstr-range (car newran)))
                           (setf (data=abstr-domain datum)
                                 (append (car newdom)
                                         (data=abstr-domain (car newran))))
                           (when (data~annotation datum)
			     (setf (data~annotation datum)
                                   (data~n-normalize (data~annotation datum)
                                                     :share-vars t)))
                           (setf (data=status datum) :n-normalized)
                           (list datum (cadr newdom)))
                       (progn
                         (setf (data=abstr-domain datum) (car newdom))
                         (setf (data=abstr-range datum) (car newran))
                         (when (data~annotation datum)
                           (setf (data~annotation datum)
                                 (data~n-normalize (data~annotation datum)
                                                   :share-vars t)))
                         (setf (data=status datum) :n-normalized)
                         (list datum (cadr newdom))))))
                  (t
                   (let* ((newran (data=n-normalize
                                   (data=abstr-range  datum)
                                   nil nil
                                   renaming
                                   share-vars))
                          (newdom (data=n-normalize
                                   (data=abstr-domain datum)
                                   nil nil
                                   (cadr newran)
                                   share-vars)))
                     (if (data~abstr-p (car newran))
                         (list (make-instance
                                (class-of datum)
                                :range (data=abstr-range (car newran))
                                :domain (append (car newdom)
                                                (data=abstr-domain
                                                 (car newran)))
                                :status :n-normalized
                                :annotation (when (data~annotation datum)
                                              (data~n-normalize
                                               (data=annotation datum)
                                               :share-vars t)))
                               (cadr newdom))
                       (list (make-instance
                              (class-of datum)
                              :range (car newran)
                              :domain (car newdom)
                              :status :n-normalized
                              :annotation (when (data~annotation datum)
                                            (data~n-normalize
                                             (data=annotation datum)
                                             :share-vars t)))
                             (cadr newdom))))))))
   
   
;; C-Normalisieren sollte insgesammt ueberarbeitet werden !
;; Keine Garantie was das unten treibt !
;; AMEIER + HGEBHARD


(data~defgeneric data~c-normalize ((datum))
  (declare (edited  "24-JAN-1995")
           (authors Fehrer)
           (input   "a datum")
           (effect  "none")
           (value   "the c-normal-form of datum"))
  (:method ((datum t))
           (error "The c normalization should be completly be reimplemented. Or use the old version of keim3.0.")))

#{\subsubsection{Ground data objects}
#}

(defun data~ground-p (datum)
  (declare (edited  "21-MAR-1996 16:29")
           (authors GKLEIN)
           (input   "A datum.")
           (effect  "None.")
           (value   "True iff the datum is ground, i.e. it doesn't contain "
                    "any free variable."))
  (not (data~free-variables datum)))


(defgeneric data~free-variables (datum)
  (declare (edited  "25-MAR-1996" "21-MAR-1996 16:38")
           (authors Fehrer GKLEIN)
           (input   "A datum.")
           (effect  "None.")
           (value   "A list with all free occuring variables in DATUM."))
  (:method ((datum data+variable))
           (list datum))
  (:method ((datum data+constant))
           nil)
  (:method ((datum data+appl))
           (let ((function (data~appl-n-function datum ))
                 (args (data~appl-n-arguments datum)))
             (remove-duplicates (mapcan #'data~free-variables
                                        (cons function args)))))
  (:method ((datum data+abstr))
           (let ((binder (data~abstr-n-domain datum))
                 (free-scope (data~free-variables
                              (data~abstr-n-range datum))))
             (remove-duplicates (set-difference free-scope binder))))
  (:method ((datum data+schema))
           (let ((bound-vars (data=schema-domain datum))
                 (free-in-dat (data~free-variables
                               (data=schema-range datum))))
             (remove-duplicates (set-difference free-in-dat bound-vars))))
  (:method ((datum list))
           (remove-duplicates (mapcan #'data~free-variables datum))))

(defgeneric data~all-unbound-symbols (datum)
  (declare (edited  "25-MAR-1996" "21-MAR-1996 16:38")
           (authors Fehrer GKLEIN)
           (input   "A datum.")
           (effect  "None.")
           (value   "A list with all free variables and constants in DATUM."))
  (:method ((datum data+variable))
           (list datum))
  (:method ((datum data+constant))
           (list datum))
  (:method ((datum data+appl))
           (let ((function (data~appl-n-function datum ))
                 (args (data~appl-n-arguments datum)))
             (mapcan #'data~all-unbound-symbols (cons function args))))
  (:method ((datum data+abstr))
           (let ((binder (data~abstr-n-domain datum))
                 (free-scope (data~all-unbound-symbols
                              (data~abstr-n-range datum))))
             (set-difference free-scope binder)))
  (:method ((datum data+schema))
           (let ((bound-syms (data=schema-domain datum))
                 (free-in-dat (data~all-unbound-symbols (data~schema-range datum))))
             (set-difference free-in-dat bound-syms)))
  (:method ((datum list))
           (mapcan #'data~all-unbound-symbols datum)))

(defgeneric data~all-variables ((datum))
  (declare (edited  "06-NOV-1998")
	   (authors Gebhard)
	   (input   "A datum")
	   (effect  "None")
	   (value   "The list of all variables occuring -bound or not- in the datum."))
  (:method ((datum data+variable)) (list datum))
  (:method ((datum data+constant)) nil)
  (:method ((datum data+appl))
	   (remove-duplicates
	    (mapcan 'data~all-variables (cons (data~appl-function datum)
					      (data~appl-arguments datum)))))
  (:method ((datum data+abstr))
	   (remove-duplicates
	    (mapcan 'data~all-variables (cons (data~abstr-range datum)
					      (data~abstr-domain datum)))))
  (:method ((datum data+schema))
	   (remove-duplicates
	    (mapcan 'data~all-variables (cons (data~schema-range datum)
					      (data~schema-domain datum))))))


#{\subsection{Semantic Annotations}
Some types of data, e.g. terms, are supposed to possess a {\em semantical
annotation}. This annotation helps to classify data and thus will allow
various algorithms (e.g. unification) to perform in a way better suited to
the intended meaning.
There are various possibilities to annotate a datum. Perhaps the most well
known of these are Church types, used to circumvent paradoxa like the Russell
anomaly by making it impossible even to write down such beasts. Others are
various types of sort systems.
The advantage of semantical annotations is, that there is no necessity of
introducing different classes for, say, typed terms, unsorted terms and sorted
terms. All of them belong to the same {\tt term+term} class and can simply be
distinguished (if needed) by looking at their annotation, which then must be
of a particular subclass of {\tt data+object}, e.g. {\tt sort+sort} resp.
{\tt type+type}.
The default case of annotation for terms should be {\em Church types}.
Algorithms like unification should operate on arbitrary terms, and it is
planned to even supply combination methods, thus that one can for example
unify an unsorted term and a sorted one. #}

(data~defgeneric data~annotated-p ((datum))
  (declare (edited  "19-NOV-1994")
           (authors Fehrer)
           (input   "A data structure")
           (effect  "none")
           (value   "T, if it is annotated, if applied to a non data "
                    "structure object, an error"))
  (:method ((datum data+struct)) (data=annotation datum)))

#|(data~defgeneric data~annotation ((datum))
  (declare (edited  "15-NOV-1994")
           (authors Fehrer)
           (input  "a data structure")
           (effect "none")
           (value  "the semantical annotation of the datum, or nil, if none"
                   "exists"))
  (:method ((datum data+struct)) (data=annotation datum)))|#

;; Replaced because of type-defs, AMeier
(data~defgeneric data~annotation ((datum))
  (declare (edited  "15-NOV-1994")
           (authors Fehrer)
           (input  "a data structure")
           (effect "none")
           (value  "the semantical annotation of the datum, or nil, if none"
                   "exists"))
  (:method ((datum data+struct))
	   (let* ((anno (data=annotation datum)))
	     (if (type~p anno)
		 (type~expand-type-def anno)
	       anno))))

(data~defgeneric (setf data~annotation) (new-annotation (datum))
  (declare (edited  "19-JAN-1995")
           (authors Fehrer)
           (input   "a new annotation and a data structure")
           (effect  "sets the annotation slot of the datum to new annotation")
           (value   "the new contents of the annotation-slot"))
  (:method (new-annotation (datum data+struct))
           (setf (data=annotation datum) new-annotation)))


;;; Print functions:
;;; ================
;;; These print functions are for testing purposes only.
;;; That's the reason why they are that verbose (e.g. print out all the
;;; annotations). They need not be removed, however, as actually appearing
;;; objects are instances of more special classes.

(defvar print*appearance 'short "affects all the print functions; recognized values are: 'short, 'verbose, 'discriminate, 'short&discriminate")

;;; The meaning is the following:
;;; short:        only the names of primitives are given
;;; verbose:      additional information (e.g. variable, constant ...) is
;;;               supplied
;;; discriminate: variables and constants are discriminated by case
;;;               (variables in caps, constants small)
;;; short&discriminate: variables and constants are discriminated by case and
;;;               types represented in short syntax
;;; type:         yet to be defined!!!

(defmethod print-object ((datum data+object) stream)
  (format stream "A totally unspecified data object"))

(defmethod print-object ((datum data+struct) stream)
  (format stream "A totally unspecified data struct~@[ with annotation ~S~]"
          (data=annotation datum)))

(defmethod print-object ((primitive data+primitive) stream)
  (declare (edited  "19-JAN-1995")
           (authors Fehrer))
  (case print*appearance
    ((verbose) (format stream "~:[unnamed (!) primitive data struct~;~:*primitive data ;;structure called ;~S~]"
                       (keim~name primitive)))
    (t (format stream "~S" (keim~name primitive)))))

(defmethod print-object ((primitive data+variable) stream)
  (declare (edited  "19-JAN-1995")
           (authors Sorge Fehrer))
  (case print*appearance
    ((verbose) (format stream "~:[unnamed (!) Variable~;~:*Variable ~S~]"
                       (keim~name primitive)))
    ((or short&discriminate discriminate) (format stream "~:@(~S~)"
                                                  (keim~name primitive)))
    (t (format stream "~S" (keim~name primitive)))))

(defmethod print-object ((primitive data+constant) stream)
  (declare (edited  "19-JAN-1995")
           (authors Sorge Fehrer))
  (case print*appearance
    ((verbose) (format stream "~:[unnamed (!) Constant~;~:*Constant ~S~]"
                       (keim~name primitive)))
    ((or short&discriminate discriminate) (format stream "~(~S~)"
                                                  (keim~name primitive)))
    (t (format stream "~S" (keim~name primitive)))))

(defmethod print-object ((abstr data+abstr) stream)
  (declare (edited  "17-NOV-1994")
           (authors Fehrer RICHTS))
  (case print*appearance
    (t (format stream "[")
       (map nil #'(lambda (x) (format stream " ~A" x))
            (data~abstr-domain abstr))
       (format stream "] . ~A" (data~abstr-range abstr)))))

(defmethod print-object ((appl data+appl) stream)
  (declare (edited  "17-NOV-1994")
           (authors Fehrer RICHTS)
           (special *print-length*))
  (case print*appearance
    (t (cond    ((or (null *print-length*) 
                     (> *print-length* (length (data~appl-arguments appl))))
                 (format stream "(~A~{ ~A~})" (data~appl-function appl) 
                         (data~appl-arguments appl)))
                (t (format stream "(~A~{ ~A~} ...)"
                           (data~appl-function appl)
                           (subseq (data~appl-arguments appl)
                                   0
                                   (1- *print-length*))))))))

(defmethod print-object ((scheme data+schema) stream)
  (declare (edited  "28-OCT-1998")
           (authors Gebhard))
  (case print*appearance
    (t (format stream "<")
       (map nil #'(lambda (x) (format stream " ~A" x))
            (data=schema-domain scheme))
       (format stream "> . ~A" (data=schema-range scheme)))))

#{\subsection{Copy Functions, Equality and Destructive Restructuring}

\section{Data Objects}\label{data:objects}

The primary application of the {\tt data+struct} hierarchy is to define
hierarchies of concrete data structures as subclasses of {\tt data+object}.
We call these hierarchies of data objects {\em categories}. The two most
important categories for KEIM are those of terms and types, and indeed it is
their structural similarity that has lead to the data+struct hierarchy.

We will use this subsection to give an overview of how the general framework
of data structures can be used to represent terms and (polymorphic) types and
obtain most of the relevant functionality by inheritance from the
{\tt data+struct} hierarchy. This example is particularly interesting in KEIM,
since this describes the actual implementation of basic terms and types. The
example itself is a simplification in that we only consider unary applications
and abstractions.
Traditionally types consist of a set of base types, and a set of complex types
that is inductively built up from function types $\alpha\rightarrow\beta$,
where $\alpha$ and $\beta$ are types. Since we want to have a polymorphic type
system we subdivide the base types into type variables and constants.
Typed $\lambda$-Terms also consist of variables, constants, and furthermore of
applications (pairs $(A B)$ of terms) and $\lambda$-abstractions (pairs of the
form $\lambda X\sdot M$, where $X$ is a term variable, and $A$ is a term). All
$\lambda$-terms are typed, i.e. they have a semantic annotation, which is a
type; and a term is considered legal, iff it is well-typed (in all
applications $(A B)$ the function $A$ must have type $\alpha\rightarrow\beta$
for some $\alpha$ if $B$ has type $\beta$). For this example we will sometimes
index terms with their types.
Since in many applications there are constants like the equality constant
$=^\alpha$ which are parametric in the type
$\alpha\rightarrow(\alpha\rightarrow o)$, we give the constant $=$ the type
$\gamma\rightarrow(\gamma\rightarrow o)$, where $\gamma$ is a type variable.
In the application $(= A_\alpha A_\alpha)$ the type is instantiated to
$\alpha\rightarrow(\alpha\rightarrow o)$ by unification of the domain type of
$=$ with the type $\alpha$. This mechanism allows KEIM to handle a simple form
of type polymorphism for parsing terms.
We model type constants as instances of {\tt data+constant}, type variables
as {\tt data+global-variable}, and complex types as {\tt data+abstraction}.
To illustrate this we will sometimes write $\lambda\alpha.\beta$ for the type
$\alpha\rightarrow\beta$ if we want to refer to the KEIM representation. For
the term category we have a full copy of the structural hierarchy, with the
only restriction that the domain of abstractions is restricted to variables of
the class {\tt term+variable}, which is a subclass of
{\tt data+local-variable}.
For well-typedness we use the semantic annotations of data structures. Terms
have semantic annotations that are types. More specifically: Term constants
and variables can have arbitrary annotations, whereas a term abstraction
$\lambda X_\alpha. A_\beta$ has the annotation $\lambda \alpha.\beta$ or in
other words the type $\alpha\rightarrow\beta$. 
We could use this scheme even further and introduce type applications
$(\lambda \alpha.\beta)\gamma$ as annotations for term application
$(\lambda X_\alpha. A_\beta)C_\gamma$. In fact this would be perfectly
sensible from a semantic point of view, but not adequate, since it does not
generate well-typedness constraints and wastes valuable space. Therefore in
KEIM we compute (and store) the type that would be obtained by immediately
$\beta$-normalizing the type application $(\lambda\alpha.\beta)\gamma$. If
this type application reduces to a type that is not an application (in our
example to $\beta$, provided that $\alpha=\gamma$ or one of $\alpha$ or
$\gamma$ is a type variable), then we call the term application well-typed
and of type $\beta$. In general the procedure of $\beta$-reduction involves
{\em unification} (wrt. an appropriate theory), $(\lambda\alpha.\beta)\gamma$
being $\sigma(\beta)$ with $\sigma={\rm mgu}(\alpha,\gamma)$.
Note that in the case of terms the $\beta$-reduction functionality of
{\tt data+struct} reduces to classical $\beta$-reduction in
$\lambda$-calculus $(\lambda X.A)B=[B/X]A$, since $[B/X]$ is the most general
unifier of $B$ and $X$.
It can easily be checked that this behaviour of types and terms (that is
entirely inherited from the {\tt data+struct} hierarchy) is exactly the
desired behaviour for our simple polymorphic $\lambda$-calculus. The
correct (polymorphic) handling of type variables came from the functionality
of {\tt data+global-variable} and $\beta$-reduction, whereas the correct
handling of bound variables in $\lambda$-terms is a consequence of
restricting the domain of term abstractions to local variables.
#}
;;; Additional things required for HOTAB

(defgeneric data~top (data)
  (declare (edited  "21-APR-1995" "3-4-1995")
           (authors Fehrer kohlhase)
           (input   "An application or primitive data structure.")
           (effect  "None.")
           (value   "If {\vb DATA} is primitive then {\vb DATA}, else the"
                    "n-function symbol of {\vb DATA}"))
  (:method ((data data+appl))
           (data~top (data~appl-n-function data)))
  (:method ((prim data+primitive))
           prim)
  (:method ((abstraction data+abstr))
           (data~top (data~abstr-scope abstraction))))

(defgeneric data~args (data)
  (declare (edited  "21-APR-1995" "3-4-1995")
           (authors Fehrer kohlhase)
           (input   "An application or primitive data structure.")
           (effect  "None.")
           (value   "If {\vb DATA} is primitive then nil, else the list of"
                    "n-arguments of {\vb DATA}"))
  (:method ((data data+appl))
           (data~appl-n-arguments data))
  (:method ((prim data+primitive))
           nil))

;;; Es folgen die aus Top uebernommenen Labelling-Sachen. Die meisten werden
;;; ueberhaupt nicht gebraucht und koennten gestrichen werden.  DEF

; lokaler Speicher fuer Labelmanipulationen
(defvar data*label-list nil)

(defun data=label-structs (structs labels)
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "A list of structs and a list of LISP-objects of equal"
                    "length.")
           (effect  "The label cells of TERMS are set to LABELS.")
           (value   "Undefined."))
  (setf data*label-list (append structs data*label-list))
  (mapc #'(lambda (struct label)
            (setf (data~label struct) label))
        structs labels))

(defun data=label-struct (struct label)
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "A struct and an object.")
           (effect  "The label of STRUCT is set to LABEL.")
           (value   "LABEL."))
  (push struct data*label-list)
  (setf (data~label struct) label))

(defun data=clear-struct-labels (struct)
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "A struct or list of structs.")
           (effect  "The labels of all substructs of STRUCT are recursively"
                    "set to NIL.")
           (value   "Undefined."))
  (if (listp struct)
      (mapc #'(lambda (substruct) (top=clear-struct-labels substruct)) struct)
      (progn (setf (data~label struct) nil)
             (if (data~primitive-p struct)
                 (data=label-struct struct nil)
                 (mapc #'(lambda (substruct)
                           (top=clear-struct-labels substruct))
                       (data~substructs struct))))))

(defun top=clear-struct-labels (struct)
  (data=clear-struct-labels struct))

(defun data=unlabel-label-list ()
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "None.")
           (effect  "The labels of all structs in the current"
                    "(data=label-list) are set to NIL. (data=label-list)"
                    "itself is also set to NIL.")
           (value   "Undefined."))
  (data=clear-struct-labels data*label-list)
  ;; vorher (data=unlabel-structs data*label-list)
  (setf data*label-list nil))

(defun data=label-list ()
  data*label-list)

(defun data~linear-p (struct)
   (declare (edited  "19-JAN-1998")
            (authors Fehrer)
            (input   "A struct or nested list of structs.")
            (effect  "None.")
            (value   "True iff each free variable in {\vb STRUCT} occurs only"
                     "once.")
            (example "(P x y) --> T"
                     "(P x x) --> NIL"))
   (unwind-protect
       (data=linear-p struct)
     (data=unlabel-label-list)))

(data~defgeneric data=linear-p ((struct))
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "A struct or nested list of structs.")
           (effect  "None.")
           (value   "True iff each free variable in STRUCT occurs only once"))
  (:method ((structlist list))
   (every #'data=linear-p structlist))
  (:method ((variable data+variable))
   (let ((label (data~label variable)))
     (unless label (data=label-struct variable t))
     (not (eq label t))))
  (:method ((constant data+constant))
   t)
  (:method ((application data+appl))
   (every #'data=linear-p (data~substructs application)))
  (:method ((abstraction data+abstr))
   (let* ((bound-variables (data~abstr-domain abstraction))
          (old-labels (mapcar #'data~label bound-variables)))
     (unwind-protect
         (progn
           (mapc #'(lambda (x) (data=label-struct x :bound)) bound-variables)
           (every #'data=linear-p (data~substructs abstraction)))
       (data=label-structs bound-variables old-labels))))
  (:method ((scheme data+schema))
   (let* ((bound-variables (data~schema-domain scheme))
          (old-labels (mapcar #'data~label bound-variables)))
     (unwind-protect
         (progn
           (mapc #'(lambda (x) (data=label-struct x :bound)) bound-variables)
           (every #'data=linear-p (data~substructs scheme)))
       (data=label-structs bound-variables old-labels)))))
  
#{\subsection{Equalitiy, Copy Functions and Destructive Restructuring}

Within KEIM a bunch of highly different data structures is kept. One of the
most used operations on data structures is copying. Since KEIM is thought to
support various algorithms, we cannot force users to adhere to a particular
strategy, for {\em efficient} implementations often depend on adequate
representations (e.g. DAG representations for unification algorithms).

Thus we have to offer not only copy-functions but also restructuring operators
that supply possibilities to fine-tune the representation of data
concerning different aspects. Many of the respective functions thus have
lots of parameters, all of which however are keyword parameters and thus
optional so that less experienced users
(or those who simply want to implement prototypes where efficiency is of
minor importance) do not need to know of their existence at all.

The first aspect to consider is the degree of recursion exhibited
by copy functions. The mere word ``copy'' itself strongly suggests that the
default be to recursively copy down to the leaves of a structure. But there
are severe objections against always doing this: Structure sharing not only
saves space, but  accelerates the copy process by blocking the need to
descend into certain subtrees. Besides, some algorithms perform notably
better if structures are maximally shared.

In order to control the depth down to which true copying should occur
we decided against the version to supply different functions (different
function names to remember!) for ``copy only top level'', ``copy down two
levels'', ``copy everything'' and so forth, because that is really a clumsy
way to do such a task. We also did not like to include a level argument,
for very often the user does not know the exact locations. Instead we
chose a more {\em semantics oriented} way by giving the user the opportunity
to name CLOS-classes that should be excluded from copying.

KEIM data structures can all  be looked upon as {\em DAG's} (directed
acyclic graphs). In principle they can even be viewed upon as trees, but
because of {\em structure sharing} (cf. below) this seldom reflects reality.
The KEIM approach to destructive reuse of structure exploits the fact that
every KEIM object belongs to a CLOS class. The optional {\tt :downto} argument
to copy functions simply says: Copy, and if you come across an object in the
named class, stop copying and instead make a link to the existing structure.

The second aspect to consider is the structure sharing we already mentioned.
The term $f(g(a,a),g(a,a))$ can in principle be represened using 2 $g$'s and
4 $a$'s, with only one of each (multiple links), and anything in between.

A na{\"\i}ve copy would exactly duplicate the structure found in the original.

Let us digress drom copying for a moment. Suppose we are given a function
{\tt restructure}, which destructively modifies the variant of strucure
sharing found in the argument.

The extrema can be described as {\tt explode} and {\tt share}, saying that
everything has to be kept apart or as much as possible is shared.
#}

#{\subsubsection{Comparing Data Structures}

Two data structures are considered {\tt data$\sim$eq}, if they are built
isomorphically. This means in particular, that different normal forms are
{\em not} {\tt data$\sim$eq}! However, if the default normalization (or some
other) is used consistently, this predicate should return the expected values
in every case.
If structures in different normal forms have to be compared, you have to use
{\tt data$\sim$equal}. This is costly, however, as normalization is executed
for both structures. Per default, n-normal-forms are obtained, but this can be
changed via the mode parameter.#}

(defgeneric data~schema-equal ((datum1)(datum2) &key (test #'data~equal))
  (:method ((datum1 data+top)(datum2 data+top) &key (test #'data~equal))
	   (funcall test datum1 datum2))
  (:method ((datum1 data+schema)(datum2 data+top) &key (test #'data~equal))
	   (let ((range1 (data~schema-range datum1)))
	     (funcall test range1 datum2)))
  (:method ((datum1 data+top)(datum2 data+schema) &key (test #'data~equal))
	   (let ((range2 (data~schema-range datum2)))
	     (funcall test datum1 range2)))
  (:method ((datum1 data+schema)(datum2 data+schema) &key (test #'data~equal))
	   (let ((range1 (data~schema-range datum1))
		 (range2 (data~schema-range datum2)))
	     (funcall test range1 range2))))

(data~defgeneric data~equal ((datum1) (datum2))
  (declare (edited  "31-JAN-1995")
           (authors Fehrer)
           (input   "two data structures")
           (effect  "none")
           (value   "true, if datum1 and datum2 represent the same structure "
                    "(including normalization!Variables have to be eq)"))
  (:method ((datum1 data+abstr) (datum2 data+abstr))
           (or (eq datum1 datum2)
               (let ((dom1 (data=abstr-domain datum1))
                     (dom2 (data=abstr-domain datum2)))
                 (and (data~equal (data=abstr-range datum1)
                                  (data=abstr-range datum2))
                      (= (length dom1) (length dom2))
                      (every #'data~equal dom1 dom2)))))
  (:method ((datum1 data+appl) (datum2 data+appl))
           (or (eq datum1 datum2)
               (let ((args1 (data=appl-arguments datum1))
                     (args2 (data=appl-arguments datum2)))
                 (and (data~equal (data=appl-function datum1)
                                  (data=appl-function datum2))
                      (= (length args1) (length args2))
                      (every #'data~equal args1 args2)))))
  (:method ((datum1 data+schema) (datum2 data+schema))
           (or (eq datum1 datum2)
               (let ((kaps1 (data=schema-domain datum1))
                     (kaps2 (data=schema-domain datum2)))
                 (and (data~equal (data=schema-range datum1)
                                  (data=schema-range datum2))
                      (= (length kaps1) (length kaps2))
                      (every #'data~equal kaps1 kaps2)))))
  (:method ((datum1 data+constant) (datum2 data+constant))
           (eq (data~constant-origin datum1)
               (data~constant-origin datum2)))
  (:method ((datum1 data+struct) (datum2 data+struct))
           (eq datum1 datum2))
  (:method (datum1 datum2)
	   nil))

(data~defgeneric data~equal-p ((datum1) (datum2) &key mode destructive)
  (declare (edited  "31-JAN-1995")
           (authors Fehrer)
           (input   "two data structs, optionally mode (default :n-normalize)"
                    "and destructive (default nil)")
           (effect  "if destructive is non-nil, the structures are modified "
                    "to normal form")
           (value   "true, if datum1 and datum2 represent the same structure "
                    "(considering normalization!not kapaslots)"))
  (:method ((datum1 data+struct) (datum2 data+struct)
            &key (mode :n-normalize) destructive)
           (or (eq datum1 datum2)
               (data~equal (data~normalize datum1
                                           :mode mode
                                           :destructive destructive)
                           (data~normalize datum2
                                           :mode mode
                                           :destructive destructive))))
  (:method ((datum1 data+constant) (datum2 data+constant)
            &key (mode :n-normalize) destructive)
           (declare (ignore mode destructive))
           (eq (data~constant-origin datum1)
               (data~constant-origin datum2)))
  (:method ((datum1 data+variable) (datum2 data+variable)
            &key (mode :n-normalize) destructive)
           (declare (ignore mode destructive))
           (eq datum1 datum2)))

;; --------------------  Anfang ALPHA-GLEICHHEIT ----------------------------
;; Nur als Hilfe context reset

(defun data=context-reset ()
  (setf bind*current-stack (bind=context-stack-create nil)))  

;(data~defgeneric data~alpha-equal ((datum1) (datum2) bound1 bound2 bindables)
;  (:method ((datum1 data+struct) (datum2 data+struct) bound1 bound2 bindables)
;           (data=alpha-equal-p datum1 datum2 nil nil bindable-vars))
;  (:method ((datum1 list) (datum2 list) bound1 bound2 bindables)
;           (cond ((and datum1 (typep (car datum1) 'type+type))
;                  (type~alpha-equal-intern-access datum1 datum2 nil nil bindables))
;                 ((and datum1 (typep (car datum1) 'term+term))
;                  (term~alpha-equal-intern-access datum1 datum2 nil nil bindables))
;                 (t
;                  (error "data=alpha-switch: start ohne arguments und reste sind keine Typen")))))

(data~defgeneric data~alpha-equal ((datum1) (datum2) bound1 bound2 bindables)
  (declare (edited  "04-APR-1997")
	   (authors Gebhard)
	   (input   "2 structures, 2 lists of bound variables"
		    "and a list of additional bindable variables.")
	   (effect  "None")
	   (value   "If the structures are alpha-equal t, else nil"))
  (:method ((datum1 data+struct) (datum2 data+struct) bound1 bound2 bindables)
	   (declare (ignore bound1 bound2))
	   (error ";;; Error data~~alpha-equal: Please work on terms and types~%"))
  (:method ((datum1 list) (datum2 list) bound1 bound2 bindables)
	   (declare (ignore bound1 bound2))
	   (cond ((and datum1 (typep (car datum1) 'type+type))
		  (type=alpha-equal datum1 datum2 nil nil bindables))
		 ((and datum1 (typep (car datum1) 'term+term))
		  (term=alpha-equal datum1 datum2 bindables))
		 (t
		  (error "data~~alpha-equal: start ohne arguments und reste sind keine Typen")))))

;; Die Notwendigen Methoden fuer Terme und Typen sollten in den etnsprechenden Files
;; direkt auf term/type~alpha-equal reduziert werden.
(data~defgeneric data~alpha-match ((datum1) (datum2))
  (declare (edited  "06-JAN-1999")
	   (authors Gebhard)
	   (input   "Two datastructures.")
	   (effect  "None")
	   (value   "The substitution if the structs are matchable, else nil."))
  (:method ((datum1 data+struct) (datum2 data+struct))
	   (error ";;; Error in data~~alpha-match: Please work on terms and types.~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substruct Things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric data~substructs (data)
  (declare (edited  "22-APR-1995")
           (authors Fehrer)
           (input   "a data structure")
           (effect  "none")
           (value   "a list of the substructures of data"))
  (:method ((data data+primitive))
           nil)
  (:method ((data data+appl))
           (cons (data~appl-function data) (data~appl-arguments data)))
  (:method ((data data+abstr)) 
           (list (data~abstr-range data)))
  (:method ((data data+schema))
           (list (data=schema-range data))))

(defgeneric data~all-substructs (data)
  (declare (edited  "26-APR-1998" "22-APR-1995")
           (authors Lassaad Fehrer)
           (input   "a data structure")
           (effect  "none")
           (value   "a list of the all substructures of data"))
  (:method ((data data+primitive))
           (list data))
  (:method ((data-list list))
           (when data-list
             (append (data~all-substructs (first data-list))
                     (data~all-substructs (rest data-list)))))
  (:method ((data data+appl))
           (cons data
                 (append (data~all-substructs (data~appl-function data))
                         (data~all-substructs (data~appl-arguments data)))))
  (:method ((data data+abstr))
           (cons data (data~all-substructs (data~abstr-range data))))
  (:method ((data data+schema))
           (cons data (data~all-substructs (data=schema-range data)))))

(defun data~substruct-p (object struct &optional (test #'data~equal))
  (declare (edited  "20-APR-1998")
           (authors Gebhard)
           (input   "A datum and a (sub)datum. Via key a testfunction.")
           (effect  "None")
           (value   "The position of the first occurence of struct in object"
                    "where test is true if exists, else nil."))
  (data~position object (lambda (x) (funcall (eval test) struct x))))



(data~defgeneric data~struct-at-position ((object) position)
  (declare (edited  "24-JUN-1992 10:06" )
	   (authors AMEIER KOHLHASE)
	   (input   "A struct or list of structs and a position.")
	   (effect  "None.")
	   (value   "The (list) of subtems of OBJECT at the position POSITION.")
	   (example "(QQ Y Y) [2] --> Y"))
  (:method ((object-list list) position)
	   (if (pos~empty-p position)
	       object-list
	     (data~struct-at-position (elt object-list (pos~first position)) (pos~rest position))))
  (:method ((struct data+struct) position)
	   (if (pos~empty-p position)
	       struct
	     (let ((number (pos~first position))
		   (substructs (data~substructs struct)))
	       (if (>= number (length substructs))
		   (error "Position ~A not in struct ~A." position struct)
		 (data~struct-at-position (elt substructs number) (pos~rest position)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions Things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(data~defgeneric data~position ((object) test)
  (declare (edited  "20-AUG-1991 14:11")
           (authors AMEIER RICHTS)
           (input   "A struct or a list of structs and a test-function.")
           (effect  "None.")
           (value   "The position of the first subobject in OBJECT where"
                    "TEST evaluates to true.")
           (example "(f a (f a a))     #'fo~constant-p ->    (1)."
                    "(f a (f a a))     #'fo~function-p ->    (0)."))
  (:method ((object-list list) test)
           (if (funcall test object-list)
               (pos~empty)
             (let ((i -1))
               (some #'(lambda (object)
                         (let ((subposition (data~position object test)))
                           (incf i)
                           (if subposition
                               (pos~add-front i subposition)
                             nil)))
                     object-list))))
  (:method ((struct data+struct) test)
           (if (funcall test struct)
               (pos~empty)
             (let ((i -1))
               (some #'(lambda (substruct)
                         (let ((subposition (data~position substruct test)))
                           (incf i)
                           (if subposition
                               (pos~add-front i subposition)
                             nil)))
                     (data~substructs struct))))))

(data~defgeneric data~positions ((object) test)
  (declare (edited  "20-AUG-1991 14:11")
           (authors AMEIER RICHTS)
           (input   "A struct or a list of structs and a test-function.")
           (effect  "None.")
           (value   "The positions of all substructs in STRUCT where TEST"
                    "evaluates to true.")
           (example "(f a (f a a))     #'fo~constant-p -> ((1) (2 1) (2 2))."
                    "(f a (f a a))     #'fo~function-p -> ((0) (2 0))."
                    "(ALL [x].(P x))   #'abstr~p    ->    ((1))."
                    "[x].(P x)         #'appl~p     ->    ((0))."))
  (:method ((object-list list) test)
           (let* ((i -1)
                  (subpositions
                   (mapcan #'(lambda (object)
                               (incf i)
                               (mapcar #'(lambda (position)
                                           (pos~add-front i position))
                                       (data~positions object test)))
                           object-list)))
             subpositions))
  (:method ((struct data+struct) test)
           (let ((positions (data~positions (data~substructs struct) test)))
             (if (funcall test struct)
                 (cons (pos~empty) positions)
               positions))))

(defun data~substruct-positions (substruct object &key (test #'keim~equal))
  (declare (edited  "20-AUG-1991 14:11")
           (authors AMEIER RICHTS)
           (input   "Two structs and a test-function.")
           (effect  "None.")
           (value   "The positions of all occurrences of SUBSTRUCT in OBJ.")
           (example "a      (f a (f a a))     ->    ((1) (2 1) (2 2))."
                    "f      (f a (f a a))     ->    ((0) (2 0))."
                    "P      [x].(P x)         ->    ((0 0))."
                    "ALL    (ALL [x].(P a x)) ->    ((0))."
                    "a      (ALL [x].(P a x)) ->    ((1 0 1))."))
  (data~positions object #'(lambda (struct) (funcall test substruct struct))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The replacement functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun data=class-checker (thing)
  (declare (edited  "18-FEB-1998")
           (authors Ameier)
           (input   "Something")
           (effect  "None.")
           (value   "The input if THING is a list, that consists only of"
                    "classes or if thing is :all-classes,"
                    "otherwise an error will occur."))
  (when thing     
    (if (symbolp thing)
        (when (null (eq (intern 'all-classes (find-package 'keyword)) thing))
          (error "Wrong input in class-list argument ~A" thing))
      (mapcar #'(lambda (thing-arg)
                  (when (null (find-class thing-arg nil))
                    (error "Wrong input in class-list argument ~A, ~A is not a CLOS-CLASS." thing thing-arg)))
              thing)))
  thing)

(data~defgeneric data~replacement-check-p ((struct1) (struct2))
  (declare (edited  "29-JAN-1998")
           (authors Ameier)
           (input   "Two structs.")
           (effect  "None.")
           (value   "T if it is ok if struct1 is as subterm replaced by"
                    "struct2, otherwise nil."))
  (:method (struct1 struct2)
	   (declare (ignore struct1 struct2))
           nil))

(defun data~replace-at-position (datum position struct
                                       &key
                                       (destructive nil)
                                       (replacers-downto '(data+struct))
                                       (downto '(data+primitive)))
  (declare (edited  "28-JAN-1998")
           (authors Gebhard Ameier)
           (input   "A datum, a position and a struct.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
                    "If DESTRUCTIVE is true the input datum is changed"
                    "destructively. If DESTRUCTIVE is nil, but DOWNTO is a"
                    "list of some Clos-Classes all objects of this classes are"
                    "shared. So if position describes a position, that the a"
		    "struct above this position is of one of the shared"
		    "classes, the replacement is also destructive.")
           (value   "A structure in that the struct of DATUM at POSITION is"
                    "replaced by the STRUCT."
                    "Remark: Which structs are shared depends also on the"
                    "keyword argument REPLACERS-DOWNTO."
                    "        A copy of STRUCT is made one time, by data~copy"
                    "        with REPLACERS-DOWNTO as downto argument and"
                    "        :all-classes as preserve argument."
                    "        This (possibly new created) struct is then used"
                    "        for the replacement."))
  
  (data=class-checker downto)
  (data=class-checker replacers-downto)
  
  (let ((new-struct (data~copy struct
                               :downto (if (data~schema-p struct)
					   nil
					 replacers-downto)
                               :explode nil
			       :preserve :all-classes)))
    (if (pos~empty-p position)
	new-struct
      (data=replace-at-position datum
				position
				new-struct
				destructive
				downto))))

(data~defgeneric data=replace-at-position ((datum) position struct destructive downto)
  (declare (edited  "12-JAN-1998" "28-JAN-1998")
           (authors Gebhard Ameier)
           (input   "look at data~replace-at-position.")
           (effect  "look at data~replace-at-position.")
           (value   "look at data~replace-at-position."))
  (:method ((datum data+appl) position struct destructive downto)
           (let ((index (pos~first position))
                 (appl-length (list-length (data~appl-arguments datum))))
             (cond ((or (minusp index) (> index appl-length))
                    (error "data=replace-at..: The given position is not valid for datum ~A" datum))
                   ((and (zerop index) (pos~empty-p (pos~rest position)))
                    (if (data~replacement-check-p (data~appl-function datum) struct)
                        (if (or destructive
                                (eq downto data*all-classes-keyword)
                                (some #'(lambda (x) (typep datum x)) downto))
                            (progn
                              (setf (data~appl-function datum) struct)
                              datum)
                          (make-instance (class-of datum)
                                         :function  struct 
                                         :arguments (data~copy (data~appl-arguments datum)
                                                               :downto downto)
                                         :annotation (data~copy (data~annotation datum)
                                                                :downto (list 'data+variable))
                                         :status (data=status datum)))
                      (error "Some of the structs are incompatible, so they can't be replaced: ~A ~A."
                             (data~appl-function datum) struct)))
                   ((zerop index)
                    (if (or destructive
                            (some #'(lambda (x) (typep datum x)) downto))
                        (progn
                          (setf (data~appl-function datum)
                                (data=replace-at-position (data~appl-function datum)
                                                          (pos~rest position)
                                                          struct
                                                          't
                                                          downto))
                          datum)
                      (make-instance (class-of datum)
                                     :function (data=replace-at-position
                                                (data~appl-function datum)
                                                (pos~rest position)
                                                struct
                                                destructive
                                                downto)
                                     :arguments (data~copy (data~appl-arguments datum)
                                                           :downto downto)
                                     :annotation (data~copy (data~annotation datum)
                                                            :downto (list 'data+variable))
                                     :status (data=status datum))))
                   (t
                    (if (or destructive
                            (eq downto data*all-classes-keyword)
                            (some #'(lambda (x) (typep datum x)) downto))
                        (progn 
                          (setf (data~appl-arguments datum)
                                (data=replace-at-position
                                 (data~appl-arguments datum)
                                 (pos~add-front
                                  (- index 1)
                                  (pos~rest position))
                                 struct 't downto))
                          datum)
                      (make-instance (class-of datum)
                                     :function (data~copy (data~appl-function datum) :downto downto)
                                     :arguments (data=replace-at-position
                                                 (data~appl-arguments datum)
                                                 (pos~add-front (- index 1) (pos~rest position))
                                                 struct destructive downto)
                                     :annotation (data~copy (data~annotation datum)
                                                            :downto (list 'data+variable))
                                     :status (data=status datum)))))))
  (:method ((datum data+abstr) position struct destructive downto)
           (cond ((not (zerop (pos~first position)))
                  (error "data=replace-at..: The given position is not valid for datum ~A" datum))
                 ((pos~empty-p (pos~rest position))
                  (if (data~replacement-check-p struct (data~abstr-range datum))
                      (if (or destructive
                              (eq downto data*all-classes-keyword)
                              (some #'(lambda (x) (typep datum x)) downto))
                          (progn
                            (setf (data~abstr-range datum) struct)
                            datum)
                        (make-instance (class-of datum)
                                       :range struct 
                                       :domain (data~copy (data~abstr-domain datum)
                                                          :downto downto)
                                       :annotation (data~copy (data~annotation datum)
                                                              :downto (list 'data+variable))
                                       :status (data=status datum)))
                    (error "Some of the structs are incompatible, so they can't be replaced: ~A ~A."
                           struct (data~abstr-range datum))))
                 (t (if (or destructive
                            (eq downto data*all-classes-keyword)
                            (some #'(lambda (x) (typep datum x)) downto))
                      (progn
                        (setf (data~abstr-range datum)
                              (data=replace-at-position
                               (data~abstr-range datum) (pos~rest position) struct 't downto))
                        datum)
                    (make-instance (class-of datum)
                                   :range (data=replace-at-position
                                           (data~abstr-range datum)
                                           (pos~rest position)
                                           struct
                                           destructive
                                           downto)
                                   :domain (data~copy (data~abstr-domain datum)
                                                      :downto downto)
                                   :annotation (data~copy (data~annotation datum)
                                                          :downto (list 'data+variable))
                                   :status (data=status datum))))))
  (:method ((datum data+schema) position struct destructive downto)
           (cond ((not (zerop (pos~first position)))
                  (error "data=replace-at..: The given position is not valid for datum ~A" datum))
                 ((pos~empty-p (pos~rest position))
                  (if (data~replacement-check-p struct (data~schema-range datum))
                      (if (or destructive
                              (eq downto data*all-classes-keyword)
                              (some #'(lambda (x) (typep datum x)) downto))
                          (progn
                            (setf (data~schema-range datum) struct)
                            datum)
                        (make-instance (class-of datum)
                                       :datum struct 
                                       :domain (data~copy (data~schema-domain datum)
                                                          :downto downto)
                                       :annotation (data~copy (data~annotation datum)
                                                              :downto (list 'data+variable))
                                       :status (data=status datum)))
                    (error "Some of the structs are incompatible, so they can't be replaced: ~A ~A."
                           struct (data~schema-range datum))))
                 (t (if (or destructive
                            (eq downto data*all-classes-keyword)
                            (some #'(lambda (x) (typep datum x)) downto))
                      (progn
                        (setf (data~schema-range datum)
                              (data=replace-at-position
                               (data~schema-range datum) (pos~rest position) struct 't downto))
                        datum)
                    (make-instance (class-of datum)
                                   :datum (data=replace-at-position
                                           (data~schema-range datum)
                                           (pos~rest position)
                                           struct
                                           destructive
                                           downto)
                                   :domain (data~copy (data~schema-range datum)
                                                      :downto downto)
                                   :annotation (data~copy (data~annotation datum)
                                                          :downto (list 'data+variable))
                                   :status (data=status datum))))))
  (:method ((datum list) position struct destructive downto)
	   (let* ((index (pos~first position))
		  (the-ele (nth index datum)))
	     (cond ((>= index (length datum))
		    (error "data=replace-at..: The given position is not valid for datum ~A" datum))
		   ((pos~empty-p (pos~rest position))
		    ;; mache replacement hier
		    (mapcar #'(lambda (x)
				(decf index)
				(cond ((zerop (+ index 1))
				       (if (data~annotation x)
					   (let* ((matcher (type~alpha-match (if (term~schema-p struct)
										 (data~annotation (data~schema-range struct))
									       (data~annotation struct))
									     (term~type x))))
					     (if matcher
						 (subst~apply matcher (if (term~schema-p struct)
									  (data~schema-range struct)
									struct))
					       (error "Some of the structs are incompatible, so they can't be replaced: ~A ~A." struct x)))
					 struct))
				      (destructive
				       x)
				      (t
				       (data~copy x :downto downto))))
			    datum ))
		   (t
		    (mapcar #'(lambda (x)
				(decf index)
				(cond ((zerop (+ index 1))
				       (data=replace-at-position the-ele (pos~rest position) struct destructive downto))
				      (destructive
				       x)
				      (t
				       (data~copy x :downto downto))))
			    datum))))))




(defun data~replace-at-positions (datum position struct
                                       &key destructive (replacers-downto '(data+struct)) (downto '(data+primitive)))
  (declare (edited  "28-JAN-1998")
           (authors Gebhard Ameier)
           (input   "look at data~replace-at-position.")
           (effect  "look at data~replace-at-position.")
           (value   "look at data~replace-at-position."))
           
  ;(when (typep datum 'type+type)
  ;  (error "data~~replace-at-positions is not implemented yet for types."))
  
  (data=class-checker downto)
  (data=class-checker replacers-downto)

   
  (let ((new-struct (data~copy struct
                               :downto replacers-downto)))
    (cond (destructive (error "data~~replace-at-positions: destructive case not yet implemented!!!"))
          ((null position) datum)
          (t
           (data=replace-at-positions datum position new-struct (pos~empty) downto)))))

(data~defgeneric data=replace-at-positions ((datum) positions struct act-pos downto)
  (declare (edited  "06-MAR-1998")
           (authors Gebhard)
           (input   "Datum, positions, struct, the actual-position downto")
           (effect  "None")
           (value   "Datum where struct is inserted at every position."))
  (:method ((datum data+primitive) positions struct act-pos downto)
	   (declare (ignore downto))
           (if (find act-pos positions :test #'keim~equal)
               (if (data~alpha-equal (data~annotation struct)
                                     (data~annotation datum) nil nil nil)
                   struct
                 (error "data=replace-at-positions: Cant replace objects of diffent types"))
             datum))
  (:method ((datum data+appl) positions struct act-pos downto)
           (cond ((or (eq downto :all-classes)
                      (some #'(lambda (x) (typep datum x)) downto))
                  datum)
                 ((find act-pos positions)
                  (if (data~alpha-equal (data~annotation struct)
                                        (data~annotation datum))
                   struct
                 (error "data=replace-at-positions: Cant replace objects of diffent types")))
                 (t (let ((i 0))
                      (make-instance (class-of datum)
                                     :function  (data=replace-at-positions
                                                 (data~appl-function datum)
                                                 positions
                                                 struct
                                                 (pos~add-end 0 act-pos)
                                                 downto)
                                     :arguments (mapcar #'(lambda (arg)
                                                            (incf i)
                                                            (data=replace-at-positions
                                                             arg positions struct
                                                             (pos~add-end i act-pos)
                                                             downto))
                                                        (data~appl-arguments datum))
                                     :annotation (data~copy (data~annotation datum)
                                                            :downto (list 'data+variable))
                                     :status (data=status datum))))))
  (:method ((datum data+abstr) positions struct act-pos downto)
           (cond ((or (eq downto :all-classes)
                      (some #'(lambda (x) (typep datum x)) downto))
                  datum)
                 ((find act-pos positions)
                  (if (data~alpha-equal (data~annotation struct) (data~annotation datum))
                   struct
                 (error "data=replace-at-positions: Cant replace objects of diffent types")))
                 (t (let ((i 0))
                      (make-instance (class-of datum)
                                     :range (data=replace-at-positions
                                             (data~abstr-range datum)
                                             positions
                                             struct
                                             (pos~add-end 0  act-pos)
                                             downto)
                                     :domain (mapcar #'(lambda (arg)
                                                         (incf i)
                                                         (data=replace-at-positions
                                                          arg positions struct
                                                          (pos~add-end i act-pos)
                                                          downto))
                                                     (data~abstr-domain datum))
                                     :annotation (data~copy (data~annotation datum)
                                                            :downto (list 'data+variable))
                                     :status (data=status datum))))))
  (:method ((datum data+schema) positions struct act-pos downto)
           (cond ((or (eq downto :all-classes)
                      (some #'(lambda (x) (typep datum x)) downto))
                  datum)
                 ((find act-pos positions)
                  (if (data~alpha-equal (data~annotation struct)
                                        (data~annotation datum))
                   struct
                 (error "data=replace-at-positions: Cant replace objects of diffent types")))
                 (t (let ((i 0))
                      (make-instance (class-of datum)
                                     :datum (data=replace-at-positions
                                             (data~schema-range datum)
                                             positions
                                             struct
                                             (pos~add-end 0 act-pos)
                                             downto)
                                     :domain (mapcar #'(lambda (arg)
                                                        (incf i)
                                                        (data=replace-at-positions
                                                         arg positions struct
                                                         (pos~add-end i act-pos)
                                                         downto))
                                                    (data~schema-domain datum))
                                     :annotation (data~copy (data~annotation datum)
                                                            :downto (list 'data+variable))
                                     :status (data=status datum)))))))

(defun data~assoc (object domain codomain test)
  (declare (edited  "26-JUN-1992 11:45")
	   (authors KOHLHASE )
	   (input   "An term, two lists of terms and a function to compare terms.")
	   (effect  "None.")
	   (value   "The term-lists can be seen as the domain and co-domain of a table defining a function."
		    "The value is the image of OBJECT in this table, where OBJECT is compared to the terms in DOMAIN by TEST."))
  (some #'(lambda (object1 object2)
	    (if (funcall test object object1)
		object2))
	domain codomain))

(defun data~replace-struct (datum struct1 struct2
                                  &key (destructive nil)
                                       (downto '(data+primitive))
                                       (replacers-downto '(data+struct))
                                       (test #'keim~equal))
  (declare (edited  "27-JAN-1998")
           (authors Ameier)
           (input   "A datum and two structs.")
           (effect  "look at data~replace-structs.")
           (value   "look at data~replace-structs."))
  (data~replace-structs datum (list struct1) (list struct2)
                        :destructive destructive
                        :downto downto
                        :replacers-downto replacers-downto
                        :test test))

(defun data~replace-structs (datum struct-list1 struct-list2
                                   &key (destructive nil)
                                        (replacers-downto '(data+struct))
                                        (downto '(data+primitive))
                                        (test #'keim~equal))
  (declare (edited  "12-JAN-1998" "27-JAN-1998") 
           (authors Gebhard AMEIER)
           (input   "A datum and two lists of structs.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
                    "If DESTRUCTIVE is true the input datum is changed destructively"
                    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
                    "all objects of this classes are shared. So if some substructs of DATUM"
                    "are of one of this shared classes the replacement is also done destructively"
                    "on this substructs.")
           (value   "A structure similar the input DATUM, but all occurrences of a struct of STRUCT-LIST1"
                    "(tested by keyword argument TEST) are replaced by the according structs of"
                    "STRUCT-LIST2."
                    "Remark: Which structs are shared depends also on the keyword argument REPLACERSDOWNTO."
                    "        A copy of all structs of STRUCT-LIST2 is made one time, using data~copy with"
                    "        REPLACERS-DOWNTO as downto argument and :all-classes as preserve argument."
                    "        This (possibly new created) structs are then used for the replacement."))
  (data=class-checker downto)
  (data=class-checker replacers-downto)
  
  (if (not (= (length struct-list1) (length struct-list2)))
      (error "Domain ~A and Codomain ~A of replacement should have the same length."
             struct-list1
             struct-list2)
    (data=replace-structs datum
                          struct-list1
                          struct-list2
                          destructive
                          downto
                          replacers-downto
                          test)))

(data~defgeneric data=replace-structs ((datum) struct-list1 struct-list2 destructive downto replacers-downto test)
  (declare (edited  "12-JAN-1998" "27-JAN-1998")
           (authors Gebhard AMEIER)
           (input   "look at data~replace-structs.")
           (effect  "look at data~replace-structs.")
           (value   "look at data~replace-structs."))
  (:method ((datum data+primitive) struct-list1 struct-list2 destructive downto replacers-downto test)
           (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test)))
             (if assoc-struct 
                 (data~copy assoc-struct
                            :downto replacers-downto
                            :explode nil
                            :preserve :all-classes)
               (if (or destructive
                       (eq downto data*all-classes-keyword)
                       (some #'(lambda (x) (typep datum x)) downto)) 
                   datum
                 (data~copy datum :downto downto)))))
  (:method ((datum data+appl) struct-list1 struct-list2 destructive downto replacers-downto test)
           (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test)))
             (if assoc-struct 
                 (data~copy assoc-struct
                            :downto replacers-downto
                            :explode nil :preserve :all-classes)
               (if (or destructive
                       (eq downto data*all-classes-keyword)
                       (some #'(lambda (x) (typep datum x)) downto))
                   (progn
                     (setf (data~appl-function datum)
                           (data=replace-structs (data~appl-function datum)
                                                 struct-list1 struct-list2 't downto replacers-downto test))
                     (setf (data~appl-arguments datum)
                           (data=replace-structs (data~appl-arguments datum)
                                                 struct-list1 struct-list2 't downto replacers-downto test))
                     datum)
                 (make-instance (class-of datum)
                                :function (data=replace-structs (data~appl-function datum)
                                                                struct-list1 struct-list2
                                                                destructive downto replacers-downto test)
                                :arguments (data=replace-structs (data~appl-arguments datum)
                                                                 struct-list1 struct-list2
                                                                 destructive downto replacers-downto test)
                                :annotation (data~copy (data~annotation datum)
                                                       :downto (list 'data+variable))
                                :status (data=status datum))))))
  (:method ((datum data+abstr) struct-list1 struct-list2 destructive downto replacers-downto test)
           (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test)))
             (if assoc-struct 
                 (data~copy assoc-struct
                            :downto replacers-downto
                            :explode nil :preserve :all-classes)                                            
               (if (or destructive
                       (eq downto data*all-classes-keyword)
                       (some #'(lambda (x) (typep datum x)) downto))
                   (progn
                     (setf (data~abstr-range datum)
                           (data=replace-structs (data~abstr-range datum)
                                                 struct-list1 struct-list2 't
                                                 downto replacers-downto test)) 
                     (setf (data~abstr-domain datum)
                           (data=replace-structs (data~abstr-domain datum)
                                                 struct-list1 struct-list2 't
                                                 downto replacers-downto test))
                     datum)
                 (make-instance (class-of datum)
                                :range (data=replace-structs (data~abstr-range datum)
                                                             struct-list1 struct-list2
                                                             destructive downto replacers-downto test)
                                :domain (data=replace-structs (data~abstr-domain datum)
                                                              struct-list1 struct-list2
                                                              destructive downto replacers-downto test)
                                :annotation (data~copy (data~annotation datum)
                                                       :downto (list 'data+variable))
                                :status (data=status datum))))))
  (:method ((datum data+schema) struct-list1 struct-list2 destructive downto replacers-downto test)
           (let* ((assoc-struct (data~assoc datum struct-list1 struct-list2 test)))
             (if assoc-struct 
                 (data~copy assoc-struct
                            :downto replacers-downto
                            :explode nil :preserve :all-classes)                                            
               (if (or destructive
                       (eq downto data*all-classes-keyword)
                       (some #'(lambda (x) (typep datum x)) downto))
                   (progn
                     (setf (data~schema-range datum)
                           (data=replace-structs (data~schema-range datum)
                                                 struct-list1 struct-list2 't
                                                 downto replacers-downto test)) 
                     (setf (data~schema-domain datum)
                           (data=replace-structs (data~schema-domain datum)
                                                 struct-list1 struct-list2 't
                                                 downto replacers-downto test))
                     datum)
                 (make-instance (class-of datum)
                                :datum (data=replace-structs (data~schema-range datum)
                                                             struct-list1 struct-list2
                                                             destructive downto replacers-downto test)
                                :domain (data=replace-structs (data~schema-range datum)
                                                              struct-list1 struct-list2
                                                              destructive downto replacers-downto test)
                                :annotation (data~copy (data~annotation datum)
                                                       :downto (list 'data+variable))
                                :status (data=status datum))))))
  (:method ((datum list) struct-list1 struct-list2 destructive downto replacers-downto test)
           (mapcar #'(lambda (dat)
                       (let* ((assoc-struct (data~assoc dat struct-list1 struct-list2 test))
                              (assoc-copy (if assoc-struct
                                              (data~copy assoc-struct
                                                         :downto (if (data~schema-p assoc-struct)
								     nil
								   replacers-downto)
                                                         :explode nil
                                                         :preserve :all-classes)
                                            nil)))
                         (if assoc-copy
			     (if (data~annotation dat)
				 (let* ((matcher (type~alpha-match (if (data~schema-p assoc-copy)
								       (data~annotation (data~schema-range assoc-copy))
								     (data~annotation assoc-copy))
								   (data~annotation dat))))
				   (if matcher
				       (subst~apply matcher (if (data~schema-p assoc-copy)
								(data~schema-range assoc-copy)
							      assoc-copy))
				     (error "Some structs are incompatible, they can't be replaced: ~A ~A"
					    assoc-copy
					    dat))))
                           (data=replace-structs dat struct-list1 struct-list2
                                                 destructive downto replacers-downto test))))
                   datum)))

(defun data=filter-type-substitution (domain codomain)
  (do* ((rest-domain domain (rest rest-domain))
        (rest-codomain codomain (rest rest-codomain))
        (type-domain nil)
        (type-codomain nil)
        (other-domain nil)
        (other-codomain nil))
      ((null rest-domain)
       (values type-domain type-codomain other-domain other-codomain))
    (let* ((head-domain (first rest-domain))
           (head-codomain (first rest-codomain)))
      (cond ((type~variable-p head-domain)
             (setq type-domain (append type-domain (list head-domain)))
             (setq type-codomain (append type-codomain (list head-codomain))))
            (t
             (setq other-domain (append other-domain (list head-domain)))
             (setq other-codomain (append other-codomain (list head-codomain))))))))

;; Diese Funktion muss spaeter am laufenden Object angepasst werden, da man jetzt noch nicht alle
;; Auswirkungen der SChemata auf typen und Terme abschaetzen kann.

(defun data~replace-free-variables (datum domain codomain
                                          &key (destructive nil)
                                               (replacers-downto '(data+struct))
                                               (downto '(data+primitive))
                                               (test #'eq))
  (declare (edited  "12-JAN-1998" "27-JAN-1998")
           (authors Gebhard Ameier)
           (input   "A datum and a replacement mapping in form of domain and codomain."
                    "It is also possible to insert the mapping of domain and codomain only"
                    "in the domain in form of mapping pairs (a b). In this moment the codomain"
                    "has to be nil.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
                    "If DESTRUCTIVE is true the input datum is changed destructively."
                    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
                    "all objects of this classes are shared. So if some substructs are"
                    "of one of this shared objects the replacement is also done destructively on them.")
           (value   "Multiple-value:"
                    "First: The datum, where the replacement mapping is applied, remark, that only"
                    "       free variable occurrences are replaced. The equality test can be given"
                    "       by keyword argument TEST."
                    "Second: The (possibly new created) codomain of the replacement mapping."
                    "        (If length of domain is bigger as the length of codomain, new variables"
                    "         are created and added to the codomain.)"
                    "Remark: Which structs are shared depends also on the keyword argument REPLACERSDOWNTO."
                    "        A copy of all structs of CODOMAIN is made one time, using data~copy with"
                    "        REPLACERS-DOWNTO as downto argument and :all-classes as preserve argument."
                    "        This (possibly new created) structs are then used for the replacement."))

  (data=class-checker downto)
  (data=class-checker replacers-downto)
  
  (cond ((and domain (consp (car domain)))
         (let ((dom (mapcar 'car domain))
               (codo (mapcar #'(lambda (struct)
                                 (let* ((back-struct (data~copy struct
								:downto (if (data~schema-p struct)
									    nil
									  replacers-downto)
								:explode nil :preserve :all-classes)))
				   (if (data~schema-p back-struct)
				       (data~schema-range back-struct)
				     back-struct)))
                             (mapcar 'cdr domain))))
           (multiple-value-bind
               (type-domain type-codomain other-domain other-codomain)
               (data=filter-type-substitution dom codo)
             (if (eval (cons 'and (mapcar #'(lambda (st1 st2)
                                              (or (keim~equal st1 st2)
                                                  (keim~equal (data~n-normalize st1) st2)))
                                          ;; was nicht n-normalisiertes durch was n-normalisiertes geht
                                          ;; aber numgekehrt geht nicht !!
                                          (mapcar #'(lambda (type)
                                                      (data~replace-free-variables type type-domain type-codomain
                                                                                   :downto (list 'data+primitive)))
                                                  (mapcar #'term~type other-domain))
                                          (mapcar #'(lambda (type)
                                                      (data~replace-free-variables type type-domain type-codomain
                                                                                   :downto (list 'data+primitive)))
                                                  (mapcar #'term~type other-codomain)))))
                 (values (data=replace-fv datum nil dom codo destructive downto test)
                         codo)
               (error "Some of the structs are type incompatible, so they can't be replaced 1: ~A ~A" dom codo)))))
        ((or domain (and (null domain) (null codomain)))
         (cond ((= (length domain) (length codomain))
		(let* ((codo (mapcar #'(lambda (struct)
					 (let* ((back-struct (data~copy struct
									:downto (if (data~schema-p struct)
										    nil
										  replacers-downto)
									:explode nil :preserve :all-classes)))
					   (if (data~schema-p back-struct)
					       (data~schema-range back-struct)
					     back-struct)))
				     codomain)))   
		  
		  (multiple-value-bind
		      (type-domain type-codomain other-domain other-codomain)
		      (data=filter-type-substitution domain codo)

		    (if (eval
			 (cons 'and
			       (mapcar #'(lambda (st1 st2) (or (keim~equal st1 st2)
							       (keim~equal (data~n-normalize st1) st2)))
				       ;; was nicht n-normalisiertes durch was n-normalisiertes geht
				       ;; aber numgekehrt geht nicht !!
				       (mapcar #'(lambda (type)
						   (data~replace-free-variables type
										type-domain
										type-codomain
										:downto (list 'data+primitive)
										))
					       (mapcar #'term~type other-domain))
				       (mapcar #'(lambda (type)
						   (data~replace-free-variables type
										type-domain
										type-codomain
										:downto (list 'data+primitive)))
					       (mapcar #'term~type other-codomain)))))
			
			(values (data=replace-fv datum nil domain codo
						 destructive downto test)
				codomain)
		      (error "Some of the structs are type incompatible, so they can't be replaced 2: ~A ~A." domain codomain)))))
               ((> (length domain) (length codomain))
                (let* ((codo (mapcar #'(lambda (struct)
					 (let* ((back-struct (data~copy struct
									:downto (if (data~schema-p struct)
										    nil
										  replacers-downto)
									:explode nil :preserve :all-classes)))
					   (if (data~schema-p back-struct)
					       (data~schema-range back-struct)
					     back-struct)))
				     (do* ((rest-domain domain (rest rest-domain))
					   (rest-codomain codomain (rest rest-codomain)))
					 ((null rest-codomain)
					  (append codomain (mapcar #'(lambda (x)
								       (data~copy x
									:downto (if (data~schema-p x)
										    nil
										  (list 'data+constant 'type+primitive))
									:explode nil :preserve :all-classes))
								   rest-domain)))))))
		  (setf co codo)
		  (setf do domain)
			
                  (multiple-value-bind
                      (type-domain type-codomain other-domain other-codomain)
                      (data=filter-type-substitution domain codo)
                    (if (eval (cons 'and (mapcar #'(lambda (st1 st2)
                                                     (or (keim~equal st1 st2)
                                                         (keim~equal (data~n-normalize st1) st2)))
                                                 ;; was nicht n-normalisiertes durch was n-normalisiertes geht
                                                 ;; aber numgekehrt geht nicht !!
                                                 (mapcar #'(lambda (type)
                                                             (data~replace-free-variables type type-domain type-codomain
                                                                                          :downto (list 'data+primitive)))
                                                         (mapcar #'term~type other-domain))
                                                 (mapcar #'(lambda (type)
							     (data~replace-free-variables type type-domain type-codomain
                                                                                          :downto (list 'data+primitive)))
                                                         (mapcar #'term~type other-codomain)))))
                        
                        (values (data=replace-fv datum nil domain codo
                                                 destructive downto test)
                                codo)
                      (error "Some of the structs are type incompatible, so they can't be replaced 3: ~A ~A." domain codo)))))
               (t
                (error "Codomain ~A is longer as Domain ~A" codomain domain))))
        (t
         (error "data~~replace-free-variables: Something wrong with ~A and ~A" domain codomain))))

(defun data~name-generator (x &key prefix)
  (declare (edited  "22-APR-1998")
           (authors Gebhard)
           (input   "If wanted a number, evt a constant prefix to be indexed.")
           (effect  "None")
           (value   "A function, taking a symbol and creating indexed symbols"
                    "beginning with x."))
  (if prefix
      (lambda () (intern (format nil "~A~A" prefix (- (incf x) 1))))
    (lambda (y) (intern (format nil "~A~A" y (- (incf x) 1))))))



(data~defgeneric data=replace-fv ((datum) bound domain codomain destructive downto test)
    (declare (edited  "13-JAN-1998" "27-JAN-1998")
             (authors Gebhard Ameier)
             (input   "look at data~replace-free-variables")
             (effect  "look at data~replace-free-variables")
             (value   "look at data~replace-free-variables"))
    (:method ((datum list) bound domain codomain destructive downto test)
             (if datum
                 (mapcar #'(lambda (dat)
                             (data=replace-fv dat
                                              bound
                                              domain
                                              codomain
                                              destructive
                                              downto
                                              test))
                         datum)
               nil)))





(defun data~replace-free-variables-and-rename (datum domain codomain &key (destructive nil) (replacers-downto '(data+struct))
                                                     (downto '(data+primitive)) (test #'eq))
  (declare (edited  "27-JAN-1998")
           (authors Ameier)
           (input   "A datum and a replacement mapping in form of domain and codomain."
                    "It is also possible to insert the mapping of domain and codomain only"
                    "in the domain in form of mapping pairs (a b). In this moment the codomain"
                    "has to be nil.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
                    "If DESTRUCTIVE is true the input datum is changed destructively."
                    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
                    "all objects of this classes are shared. So if some substructs"
                    "are of one of this shared objects the replacement is also done"
                    "destructively on them.")
           (value   "In a first step the free-variables in datum that occur also in the domain are"
                    "replaced by the according structs in codomain. Then in a second step the free"
                    "variables of this structure are renamed."
                    "Multiple-Value:"
                    "First: the structure after renaming"
                    "Second: the free variables that are renamed in the second step."
                    "Third: the codomain of the renaming in the second step."
                    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
                    "        A copy of all structs of CODOMAIN is made one time, using data~copy with"
                    "        REPLACERS-DOWNTO as downto argument and :all-classes as preserve argument."
                    "        This (possibly new created) structs are then used for the replacement."))
  (data=class-checker downto)
  (data=class-checker replacers-downto)

  (let* ((new-object (data~replace-free-variables datum domain codomain
						  :destructive destructive
                                                  :replacers-downto replacers-downto
                                                  :downto downto
                                                  :test test))
         (free-variables (remove-duplicates (data~free-variables new-object))))
    ;; (format t "~%~% FREE_S: ~A" free-variables)
    (multiple-value-bind
        (new-new-object codomain)
        (data~replace-free-variables new-object
                                     free-variables 
                                     nil
                                     :destructive destructive
                                     :replacers-downto '(data+struct)
                                     :downto downto
                                     :test test)
      (values new-new-object free-variables codomain))))


#{\subsubsection{Copying}
The function {\tt data$\sim$copy} recursively copies data structures. The keyword argument {\tt :downto}
expects a {\em list of classnames or classes}. The effect of supplying this list is that the recursion
is broken at places where an item which falls in one of the named classes is encountered. Instead a
link to the original is made ({\em structure sharing}). There is one exception to the strategy that
classes not explicitly mentioned are always copied: Primitives (i.e. members of {\tt data+primitive})
are always shared!

{\tt data$\sim$copy} also works on lists of data structures.#}

(defun data=copy-list-conflict (list1 list2 list3)
  (declare (edited  "10-FEB-1999")
	   (authors Gebhard)
	   (input   "Listen")
	   (effect  "Unbekannt")
	   (value   "Tested ob bei data~copy ob die eingegebenen Classenlisten Sinn machen."))
  (cond ((or (and (eq list1 :all-classes) (eq list2 :all-classes))
             (and (eq list2 :all-classes) (eq list3 :all-classes))
             (and (eq list1 :all-classes) (eq list3 :all-classes))
             (and (eq list1 :all-classes)(eq list2 :all-classes) (eq list3 :all-classes))
             (and (listp list1) (listp list2) (listp list3)
                  (or (intersection list1 list2) (intersection list2 list3) (intersection list1 list3))))
         (error "data~~copy: the given explode share preserve combi isn't valid"))
        ((and (null list2) (null list3))
         :fix)
        (t :ex)))


(defmethod keim~copy ((struct data+struct) &key (explode :all-classes) share preserve downto)
  (declare (edited  "27-JAN-1998")
           (authors Ameier)
           (input   "A struct.")
           (effect  "None.")
           (value   "A copy of the struct."
                    "Warning: Depending of the values of downto some substruct are may shared."))
  (data~copy struct
             :explode explode
             :share share
             :preserve preserve
             :downto downto))


(defgeneric data~alpha-copy (datum renaming)
  (declare (edited  "14-JUL-1998")
	   (authors Ameier)
	   (input   "A datum and the current renaming (former bound vars and their image)")
	   (effect  "Nil")
	   (value   "The alpha-renamed term."))
  (:method ((datum list) renaming)
	   (if datum
	       (multiple-value-bind
		   (back-first-element back-renaming)
		   (data~alpha-copy (first datum) renaming)
		 (multiple-value-bind
		     (back-rest-elements back-rest-renaming)
		     (data~alpha-copy (rest datum) back-renaming)

		   (values
		    (cons back-first-element back-rest-elements)
		    back-rest-renaming)))
	     (values
	      nil
	      renaming))))	
;; die MEthoden fuer Terme und Typen siehe dort

(defun data~copy (datum &key (explode :all-classes) share preserve downto)
  (declare (edited  "07-JAN-1998")
           (authors Gebhard)
           (input   "A datum, and some keywordparamters"
                    "downto:  A list of classes. If the function arrives at a subterm of a class contained"
                    "         in the downto list. this subterm will be shared between copy and original."
                    "explode, share, preserve: The copy function works in different modes:  "
                    "Every keyword contains a list of classes, activating ths mode."
                    "Everytime when a subterm is in a list not of an other mode, the is changed."
                    "These keywords manage the datum intern sharing behaviour."
                    )
           (effect  "None")
           (value   "A copy of datum, with respect to the keyword args."))
  (tree~with-fresh-hash
   (let ((mode (data=copy-list-conflict explode share preserve)))
     (if mode
         (car (data=copy datum nil mode explode share preserve downto))
       (error "data~~copy: There are Problems with the parameters")))))

;; 1. downto: parameter von klassen oder klassennamen an denen die rekursion abgebrochen
;;            wird und damit auch der folgende unterterm geshared wird.
;; 2. am    : aktueller mode, der besagt ob gerade explodoert oder geshared wird,
;;            unterschied zum downto parameter ist, dass hier nicht die rekursion
;;            abgebrochen wird, sondern lediglich die konzeption des Zieltermes bzg
;;            Sharing festgelegt wird
;; 3. die uebrigen Listen dienen lediglich als Schalter zum momentan angebrachten
;;    anpassen des aktuellen Shraing modus'. 
;; d.h. downto und am haengen nur begrentzt miteinander zusammen.
;;
;; Ueberlegungen fuehrten zu folgender kleinen Taktik (traversieren):
;;
;; Zunaechst ueberpruefen des downto Argumentes wird dies aktiv rekursion hier abbrechen
;; sonst:
;;   Markieren des aktuellen Standpunktes
;;   aktualisieren des am parameters
;;   dann kopiervorgang fortsetzten gemaess am
;;
;;
;; Die Funktion wird aber trotzdem dinge produzieren, die anders aussehen als das was man erwartet

(defun data=copy-new-mode (datum am explode share preserve)
  (declare (edited  "07-JAN-1998")
           (authors Gebhard)
           (input   "a Datum, the actual mode and the mode managing lists.")
           (effect  "None")
           (value   "The following actual mode"))
  (cond ((equal ':fix am) nil)
        ((or (eq explode :all-classes)
             (some #'(lambda (x) (typep datum x)) explode))
         (unless (eq am :ex) :ex))
        ((or (eq share :all-classes) (some #'(lambda (x) (typep datum x)) share))
         (unless (eq am :sha) :sha))
        ((or (eq preserve :all-classes) (some #'(lambda (x) (typep datum x)) preserve))
         (unless (eq am :pre) :pre))))

(setf (symbol-function 'data=copy-new-name)
      (data~name-generator 0 :prefix 'dc))


(defun data~name-generator (x &key prefix)
  (declare (edited  "22-APR-1998")
	   (authors Gebhard)
	   (input   "If wanted a number, evt a constant prefix to be indexed.")
	   (effect  "None")
	   (value   "A function, taking a symbol and creating indexed symbols beginning with x."))
  (if prefix
      (lambda () (intern (format nil "~A-~A" prefix (- (incf x) 1))))
    (lambda (y) (intern (format nil "~A-~A" y (- (incf x) 1))))))


(setf (symbol-function 'data=copy-new-name) (data~name-generator 0 :prefix 'dc))

(data~defgeneric data=copy ((datum) renaming am explode share preserve downto)
  (declare (edited  "23-JUL-1998" "05-JAN-1998" "08-FEB-1995")
           (authors Sorge Gebhard Fehrer)
           (input   "a datastruct, optional :downto (classes or classnames)")
           (effect  "none")
           (value   "A recursively copied version of datum, and the realized"
		    "renaming.")
           (comments "for the meaning of the downto parameter see above"))
  (:method ((datum list) renaming am explode share preserve downto)
           (if datum
               (let* ((first-el (data=copy (car datum) renaming am
					   explode share preserve downto))
                      (rest-els (data=copy (cdr datum) (cadr first-el)
					   am explode share preserve downto)))
                 (list (append (list (car first-el)) (car rest-els))
                       (cadr rest-els)))
             (list nil renaming)))
  (:method ((datum data+constant) renaming am explode share preserve downto)
           (cond ((or (eq downto :all-classes)
                      (some #'(lambda (x) (typep datum x)) downto))
                  (list datum renaming))
                 ((data~annotation datum)
		  
		  ;;    (intersection (mapcar #'car renaming)
		  ;;                  (data~all-variables (data~annotation datum))))
		  ;;       ;; eine Fkt data~all-variables wie type~type-variables
                  (let* ((new-type (data=copy (data~annotation datum)
					      renaming :ex
					      explode share preserve downto)))
		    (if (data~equal (car new-type) (data~annotation datum))
			(list datum renaming)
		      (let ((new-const (make-instance
					(class-of datum)
					:name (keim~name datum)
					:annotation (car new-type)
					:status T
					:origin (data~constant-origin datum))))
			(list new-const (cadr new-type))))))
                 (t (list datum renaming))))
     (:method ((datum data+variable) renaming am explode share preserve downto)
         (cond ((or (eq downto :all-classes)
		    (some #'(lambda (x) (typep datum x)) downto))
		(if (assoc datum renaming)
		    (list (cdr (assoc datum renaming)) renaming)
		  (list datum renaming)))
	       ((assoc datum renaming)                         ;; Uebernehmen
		(list (cdr (assoc datum renaming)) renaming))
	       (t                                              ;; Kopieren
		(let* ((new-type (if (data~annotation datum)
				     (data=copy (data~annotation datum)
						renaming :ex
						explode share preserve
						downto)
				   (list nil renaming)))
		       (new-var (make-instance (class-of datum)
					       :name (data=copy-new-name)
					       :binding (data~binding datum)
					       :binding-table
					         (data~binding-table datum)
					       :annotation (car new-type)
					       :status T)))
		  (list new-var (acons datum new-var (cadr new-type)))))))
     (:method ((datum data+appl) renaming am explode share preserve downto)
	      (cond ((or (eq downto :all-classes)
			 (some #'(lambda (x) (typep datum x)) downto))
		     (list datum renaming))
		    ((data=copy-new-mode datum am explode share preserve)
		     (data=copy datum renaming
				(data=copy-new-mode
				 datum am explode share preserve)
				explode share preserve downto))
		    (t
		     (let* ((new-ann (data=copy
				      (data~annotation datum)
				      renaming ':ex explode share
				      preserve downto))
			    (new-fun (data=copy
				      (data~appl-function datum)
				      (cadr new-ann) am explode share
				      preserve downto))
			    (new-args (data=copy
				       (data=appl-arguments datum)
				       (cadr new-fun) am explode share
				       preserve downto))
			    (new-val (make-instance
				      (class-of datum)
				      :function (car new-fun)
				      :arguments (car new-args)
				      :annotation (car new-ann)
				      :status (data=status datum)))
			    (print-rep (prin1-to-string new-val)))
		       (ecase am
			 ((:ex)
			  (progn (tree~hash print-rep new-val)
				 (tree~hash datum new-val)
				 (list new-val (cadr new-args))))
			 ((:fix)
			  (list new-val (cadr new-args)))
			 ((:sha)
			  (if (tree~hashed datum)
			      (list (tree~hashed datum) renaming)
			    (let* ((print-rep (prin1-to-string datum))
				   (hash-val (tree~hashed print-rep)))
			      (if hash-val
				  (list (tree~hash print-rep hash-val)
					renaming)
				(progn (tree~hash print-rep new-val)
                                       (tree~hash datum new-val)
                                       (list new-val (cadr new-args)))))))
			 ((:pre)
			  (if (tree~hashed datum)
			      (list (tree~hashed datum) renaming)
			    (progn (tree~hash (prin1-to-string datum) new-val)
				   (tree~hash datum new-val)
				   (list new-val (cadr new-args))))))))))
    (:method ((datum data+abstr) renaming am explode share preserve downto)
	     (cond ((or (eq downto :all-classes)
			(some #'(lambda (x) (typep datum x)) downto))
		    (list datum renaming))
		   ((data=copy-new-mode datum am explode share preserve)
		    (data=copy datum renaming
			       (data=copy-new-mode datum am explode
						   share preserve)
			       explode share preserve downto))
		   (t
		    (let* ((new-ann (data=copy
				     (data~annotation datum)
				     renaming ':ex explode share preserve
				     downto))
			   (new-dom (data=copy
				     (data~abstr-domain datum) (cadr new-ann)
				     am explode share preserve downto))
			   (new-ran (data=copy
				     (data~abstr-range datum) (cadr new-dom)
				     am explode share preserve downto))
			   (new-val (make-instance
				     (class-of datum)
				     :range (car new-ran)
				     :domain (car new-dom)
				     :annotation (car new-ann)
				     :status (data=status datum)))
			   (print-rep (prin1-to-string new-val)))
		      (ecase am
			((:ex)
			 (progn (tree~hash print-rep new-val)
				(tree~hash datum new-val)
				(list new-val (cadr new-ran))))
		      ((:fix)
		       (list new-val (cadr new-ran)))
		      ((:sha)
		       (if (tree~hashed datum)
			   (list (tree~hashed datum) renaming)
			 (let* ((print-rep (prin1-to-string datum))
				(hash-val (tree~hashed print-rep)))
			   (if hash-val
			       (list (tree~hash print-rep hash-val) renaming)
			     (progn (tree~hash print-rep new-val)
				    (tree~hash datum new-val)
				    (list new-val (cadr new-ran)))))))
		      ((:pre)
		       (if (tree~hashed datum)
			   (list (tree~hashed datum) renaming)
			 (progn (tree~hash (prin1-to-string datum) new-val)
				(tree~hash datum new-val)
				(list new-val (cadr new-ran))))))))))
    (:method ((datum data+schema) renaming am explode share preserve downto)
	     (cond ((or (eq downto :all-classes)
			(some #'(lambda (x) (typep datum x)) downto))
		    (list datum renaming))
		   ((data=copy-new-mode datum am explode share preserve)
		    (data=copy datum renaming
			       (data=copy-new-mode datum am explode
						   share preserve)
			       explode share preserve downto))
		   (t
		    (let* ((new-kappas (if (or (find 'data+primitive downto)
					       (find 'data+variable downto)
					       (find 'term+primitive downto)
					       (find 'type+primitive downto)
					       (find 'type+variable downto)
					       (find 'type+constant downto)
					       (find 'type+type downto)
					       (find 'term+variable downto)
					       (find 'term+constant downto))
					   ;; kein Renaming der Typ-Variablen, da sonst eventuell auch andere Teile veranedert werden
					   ;; muessten
					   (data=schema-domain datum)
					 (mapcar #'(lambda (x)
						     (if (assoc x renaming)
							 (cdr (assoc x renaming))
						       (make-instance
							(class-of x)
							:name (gensym))))
						 (data=schema-domain datum))))
			   (new-ren (pairlis (data=schema-domain datum)
			   		     new-kappas
			   		     renaming))
			   (new-type (when (data=annotation datum)
			   	       (data=copy (data=annotation datum)
			   			  new-ren ':ex explode share
			   			  preserve downto))))
		      
		      (list (make-instance (class-of datum)
					   :domain new-kappas
					   :datum
					   (car (data=copy
						 (data~schema-range datum)
						 new-ren am explode share
						 preserve downto))
					   :annotation (car new-type)
					   :status (data=status datum))
			    new-ren))))))


;; ==========================================================================
;; |                                                                        |
;; |              S O N S T I G E       F U N K T I O N E N                 | 
;; |                                                                        | 
;; ==========================================================================

(defun data~assoc (object domain codomain test)
  (declare (edited  "26-JUN-1992 11:45")
           (authors KOHLHASE )
           (input   "An term, two lists of terms and a function to compare"
                    "terms.")
           (effect  "None.")
           (value   "The term-lists can be seen as the domain and co-domain"
                    "of a table defining a function."
                    "The value is the image of OBJECT in this table, where "
                    "OBJECT is compared to the terms in DOMAIN by TEST."))
  (some #'(lambda (object1 object2)
            (if (funcall test object object1)
                object2))
        domain codomain))

(defmethod keim~equal ((struct1 data+struct) (struct2 data+struct))
  (declare (edited  "20-JAN-1998")
	   (authors Fehrer)
	   (input   "Two structs.")
	   (value "True if the structs are DATA~EQUAL."))
  (data~equal struct1 struct2))



(setf (symbol-function 'data~new-name)
      (data~name-generator 0 :prefix 'dt))

(defun data~renaming-for (primlist)
 (if (every 'data~primitive-p primlist)
      (mapcar #'(lambda (x)
                  (make-instance (class-of x)
                                 :name (data~new-name)
                                 :annotation
                                 (when (data~annotation x)
                                   (data~copy (data~annotation x)))))
	      primlist)
    (error "Only rename primtives")))

(defun data~replace-free-variables-ntc (datum domain codomain
                                          &key (destructive nil) (replacers-downto '(data+struct))
                                          (downto '(data+primitive)) (test #'eq))
  (declare (edited  "10-FEB-1999")
	   (authors Ameier GEBHARD)
	   (input   "siehe data~replace-free-variables")
	   (effect  "siehe data~replace-free-variables")
	   (value   "Macht im prinzip das gleiche wie data~replace-free-variables, nur dass uber den zu ersetzenden"
		    "Structs kein Typ-check gemacht wird."))
  
  (data=class-checker downto)
  (data=class-checker replacers-downto)

  (cond ((and domain (consp (car domain)))
         (let ((dom (mapcar 'car domain))
               (codo (mapcar #'(lambda (struct)
                                (data~copy struct
                                            :downto replacers-downto
                                            :explode nil
                                            :preserve :all-classes))
                             (mapcar 'cdr domain))))
           (values (data=replace-fv-nso datum nil dom codo destructive downto test)
                   codo)))
        ((or domain (and (null domain) (null codomain)))
         (cond ((= (length domain) (length codomain))
                (values (data=replace-fv-nso datum nil domain  (mapcar #'(lambda (struct)
                                                                       (data~copy struct
                                                                                  :downto replacers-downto
                                                                                  :explode nil :preserve :all-classes))
 codomain)   
                                             destructive downto test)
                            codomain))
               ((> (length domain) (length codomain))
                (let* ((codo (do* ((rest-domain domain (rest rest-domain))
                                   (rest-codomain codomain (rest rest-codomain)))
                                 ((null rest-codomain)
                                  (append codomain (mapcar #'(lambda (x) (data~copy x)) rest-domain))))))
                  (values (data=replace-fv-nso datum nil domain (mapcar #'(lambda (struct)
                                                                        (data~copy struct
                                                                                   :downto replacers-downto
                                                                                   :explode nil :preserve :all-classes))
                                                                    codo)     
                                           destructive downto test)
                              codo)))
               (t
                (error "Codomain ~A is longer as Domain ~A" codomain domain))))
        (t
         (error "data~~replace-free-variables-ntc: Something wrong with ~A and ~A" domain
                codomain))))












