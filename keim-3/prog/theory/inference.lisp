;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
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

(in-package "KEIM")

(mod~defmod infer :uses ( keim mod inter arg )
	    :documentation  "Datastructures and basic functionality for generic methods."
	    :exports (
		      infer+inference
		      infer+dummy
		      infer+open
		      infer+rule
		      infer+tactic
		      infer+method
		      infer+supermethod
		      infer+black-box

		      infer~dummy-create
		      infer~open-create
		      infer~rule-create
		      infer~method-create
		      infer~tactic-create
		      infer~bbox-create
		      infer~read-file

		      infer~p
		      infer~dummy-p
		      infer~open-p
		      infer~rule-p
		      infer~tactic-p
		      infer~method-p
		      infer~supermethod-p
		      infer~bbox-p

		      infer~find-method
		      infer~supermethod
		      infer~expansion-function
		      infer~parameter-types
		      infer~compute-outline
		      infer~compute-outline-pattern
		      infer~apply-expansion-function
		      infer~apply-test-function
		      infer~type-test-list
		      infer~pattern-check

		      infer~deftactic
		      infer~defbbox
		      infer~defrule
		      infer~defmethod
		      infer~defsupermethod
		      infer~defdummy
		      infer~defopen
		      
		      infer~find-outline-mappings
		      infer~find-outline-methodnames
		      infer~application2outline-pattern
		      infer~outline-pattern2application
		      infer~def-outline-mappings
		      infer~add-outline-mapping
		      infer~delete-outline-mapping
		      infer~relate-mappings2method
		      infer~print-outline-mappings
		      infer~copy-outline-mappings
		      infer~remove-outline-mappings
		      infer~closed-pattern-p 
		      infer~existent-pattern-p
		      infer~nonexistent-pattern-p
		      infer~list-pattern-p

		      infer~outline-function
		      infer~test-function

		      infer~theory
		      infer~find-arbitrary-application-name

		      ;;; stuff for wild tactics   ---   VS
		      infer+wild-tactic

		      infer~defwild-tactic
		      infer~wild-tactic-create
		      infer~wild-tactic-p
		      infer~passkey

		      infer*closed
		      infer*existent
		      infer*list 
		      infer*non-existent 

		      )
	    )

#{\section{Inference}\label{mod:infer}

An important novelity created for the plan data structure is the concept of inference method or abstract method.
It generalises the notion of inference rule by allowing not only calculus level steps as justification for deductions, but
more abstract arguments like tactics, planning methods, etc. In order to ensure the correctness of such abstract
deductions, it is necessary to provide the possibility of expanding those steps to calculus level.
So far we have thought of four different types of abstract methods:
\begin{itemize}
\item Calculus level rules, which have no expansion function.
\item Tactics, that apply sequences of calculus level steps.
\item Declarative methods (not to confuse with abstract methods!) are used for planning and consist of tactics with specifications.
\item Black box systems: These can be ATP or CAS, for instance.
\end{itemize}

An abstract method consists of four attributes: a name, a list of parametertypes, a set of outline functions and an
expansion function. When grounding a node the method is applied to a given set of nodes, the outline. According to this
outline the corresponding outline-function is called which then returns the complete outline including the grounded node.
For expanding the justification an expansion function must be supplied. This expansion function, as well as all outline
functions must have two arguments: a list of nodes as the outline, and a list of parameters.
#}

;;; Classes

(eval-when (load compile eval)

(defclass infer+inference (help+help)
  ((expansion-function :initarg :expansion-function
		       :initform nil
		       :accessor infer=expansion-function
		       :documentation "The expansion-function expands the method to the next lower level of abstraction")
   (parameter-types :initarg :parameter-types
		    :initform nil
		    :accessor infer=parameter-types
		    :documentation "An ARG+TYPE list corresponding to additional arguments of the expansion- and outline-functions"))
  (:documentation "The basic class for all methods of inference. It has no instances of its own."))

(defclass infer+dummy (infer+inference)
  ((expansion-function :initform nil
		       :allocation :class)
   (parameter-types :initform nil
		    :allocation :class))
  (:default-initargs :expansion-function nil :parameter-types nil)
  (:documentation "The class for dummy-methods, i.e. justifications like HYP, AXIOM, etc."))

(defclass infer+open (infer+dummy)
  ()
  (:documentation "The subclass of dummy methods which justify unproved lines,"
		  "i.e. OPEN, PLANNED, NOT-PROVEN-YET, or whatever."))

(defclass infer+rule (infer+inference)
  ((expansion-function :initform nil
		       :allocation :class))
  (:default-initargs :expansion-function nil)
  (:documentation "The class of all basic nd-rules."))

(defclass infer+tactic (infer+inference)
  ()
  (:documentation "The class for all procedural tactics."))

(defclass infer+method (infer+inference)
  ()
  (:documentation "The class computed and expanded by (non-abstract) methods."))

(defclass infer+black-box (infer+inference)
  ((outline-function :initarg :outline-function
		       :initform nil
		       :accessor infer~outline-function
		       :documentation "A single outline-function is enough, as black-box methods are always applied in one direction.")
   (test-function :initarg :test-function
		       :initform nil
		       :accessor infer~test-function
		       :documentation "A test-function to check whether computed outlines are correct."))
  (:documentation "The class of tactics using some black-box for proving."))
)

(eval-when (load compile eval)
(defclass infer+supermethod (infer+inference)
  ((supermethod :initarg :supermethod
		:initform nil
		:accessor infer~supermethod
		:documentation "The corresponding supermethod.")
   )
  (:documentation "The class computed and expanded by (non-abstract) supermethods."))
)
(defvar infer*method-hash-table (make-hash-table :test #'equal)
  "A hash table, that keeps track of all the existing abstract methods at any time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some constants that define the values in outline-patterns:
;; - non-existent means no node was provided,
;; - existent means some node was provided
;; - closed specifies the provided node is justified, i.e.
;;   closed nodes are a real subsent of existent nodes.
;; - and finally list indicates that a list of nodes was given.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant infer*closed "CLOSED")
(defconstant infer*existent "EXISTENT")
(defconstant infer*non-existent "NONEXISTENT")
(defconstant infer*list "LIST")

#{\subsection{Constructors}#}

(defun infer~dummy-create (name help)
  (declare (edited  "23-MAY-1997 17:56")
	   (authors SORGE)
	   (input   "A string and a help string.")
	   (effect  "Creates an instance of INFER+DUMMY.")
	   (value   "The new instance."))
  (make-instance 'infer+dummy
		 :name (infer=read-string name)
		 :help help))

(defun infer~open-create (name help)
  (declare (edited  "23-MAY-1997 17:57")
	   (authors SORGE)
	   (input   "A string and a help string.")
	   (effect  "Creates an instance of INFER+OPEN.")
	   (value   "The new instance."))
  (make-instance 'infer+open
		 :name (infer=read-string name)
		 :help help))

(defun infer~rule-create (name parameter-types help)
  (declare (edited  "24-JAN-1996 16:07")
	   (authors SORGE)
	   (input   "A string, a list of ARG+TYPES and a help string.")
	   (effect  "Creates an instance of INFER+RULE.")
	   (value   "The new instance."))
  (make-instance 'infer+rule
		 :name (infer=read-string name)
		 :parameter-types parameter-types
		 :help help))

(defun infer~method-create (name exp-func parameter-types help)
  (declare (edited  "24-JAN-1996 16:07")
	   (authors SORGE)
	   (input   "A string, a function-symbol, a list of ARG+TYPES and a help string.")
	   (effect  "Creates an instance of INFER+METHOD.")
	   (value   "The new instance."))
  (make-instance 'infer+method
		 :name (infer=read-string name)
		 :expansion-function exp-func
		 :parameter-types parameter-types
		 :help help))

(defun infer~tactic-create (name exp-func parameter-types help)
  (declare (edited  "24-JAN-1996 16:07")
	   (authors SORGE)
	   (input   "A string, a function-symbol, a list of ARG+TYPES and a help string.")
	   (effect  "Creates an instance of INFER+TACTIC.")
	   (value   "The new instance."))
  (make-instance 'infer+tactic
		 :name (infer=read-string name)
		 :expansion-function exp-func
		 :parameter-types parameter-types
		 :help help))

(defun infer~bbox-create (name exp-func parameter-types help)
  (declare (edited  "24-JAN-1996 16:07")
	   (authors SORGE)
	   (input   "A string, a function-symbol, a list of ARG+TYPES and a help string.")
	   (effect  "Creates an instance of INFER+BLACK-BOX.")
	   (value   "The new instance."))
  (make-instance 'infer+black-box
		 :name (infer=read-string name)
		 :expansion-function exp-func
		 :parameter-types parameter-types
		 :help help))

(defun infer~read-file (file)
  (declare (edited  "24-JAN-1996 16:34")
	   (authors SORGE)
	   (input   "A filename.")
	   (effect  "Reads and creates abstract inference methods from a file containing some INFER~DEFRULE, INFER~DEFTACTIC etc.")
	   (value   "T if everything went allright."))
  (when (open file :if-does-not-exist nil)
    (with-open-file (in file :direction :input)
		    (do ((x (read in nil) (read in nil)))
			((null x) t)
		      (eval x)))
    t))

#{\subsection{Predicates}#}

(defun infer~p (obj)
  (declare (edited  "24-JAN-1996 16:31")
	   (authors SORGE)
	   (input   "A lisp object.")
	   (value   "T if object is an abstract method, nil otherwise"))
  (typep obj 'infer+inference))

(defun infer~dummy-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+DUMMY."))
  (typep obj 'infer+dummy))

(defun infer~open-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+OPEN."))
  (typep obj 'infer+open))

(defun infer~rule-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+RULE."))
  (typep obj 'infer+rule))

(defun infer~tactic-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+TACTIC."))
  (typep obj 'infer+tactic))

(defun infer~method-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+METHOD."))
  (typep obj 'infer+method))

(defun infer~supermethod-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+SUPERMETHOD."))
  (typep obj 'infer+supermethod))


(defun infer~bbox-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+BLACK-BOX."))
  (typep obj 'infer+black-box))

#{\subsection{Accessors}#}

(defgeneric infer~expansion-function (method)
  (declare (edited  "24-JAN-1996 16:45")
	   (authors SORGE)
	   (input   "An abstract method.")
	   (value   "The expansion-function of the abstract method."))
  (:method (obj)
	   (error "~A must be an abstract method." obj))
  (:method ((method symbol))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~expansion-function tac)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method string))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~expansion-function tac)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method infer+inference))
	   (infer=expansion-function method)))

(defgeneric infer~parameter-types (method)
  (declare (edited  "24-JAN-1996 16:45")
	   (authors SORGE)
	   (input   "An abstract method.")
	   (value   "A list of the parameter-types of abstract method."))
  (:method (obj)
	   (error "~A must be an abstract method." obj))
  (:method ((method symbol))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~parameter-types tac)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method string))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~parameter-types tac)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method infer+inference))
	   (infer=parameter-types method)))

#{\subsection{Auxiliary Functions}#}

(defgeneric infer~find-method (name)
  (declare (edited  "24-JAN-1996 16:52")
	   (authors SORGE)
	   (input   "A name of an abstract method.")
	   (value   "The theory with this name or NIL if none exists."))
  (:method (name)
	   (gethash (infer=read-string name) infer*method-hash-table))
  (:method ((name infer+inference))
	   (gethash (infer=read-string (keim~name name)) infer*method-hash-table)))

;;; Auxiliary functions


(defgeneric infer~compute-outline (method outline parameters)
  (declare (edited  "25-JAN-1996 15:16")
	   (authors SORGE)
	   (input   "An abstract method, an outline and a parameter-list.")
	   (effect  "Applies the corresponding outline-function of the abstract method to both lists.")
	   (value   "The new outline and T if it was computed successfully,"
		    "The incomplete outline and NIL otherwise."))
  (:method (obj1 obj2 obj3)
	   (error "~A must be of type INFER+INFERENCE, ~A and ~A both lists." obj1 obj2 obj3))
  (:method (obj (outline list) (parameters list))
	   (error "~A must be a tactic." obj))
  (:method ((method symbol) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~compute-outline tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method string) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~compute-outline tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method :before ((tactic infer+inference) (outline list) (parameters list))
	   (unless (every #'(lambda (x) (or (node~p x)
					    (and (consp x) (every #'node~p x))
					    (null x))) outline)
	     (error "The members of ~A should be nodes, list of nodes, or nil." outline))
	   (unless (infer~type-test-list parameters (infer~parameter-types tactic))
	     (error "The members of ~A are not of the right type." parameters)))
  (:method :around ((dummy infer+dummy) (outline list) (parameters list))
	   (warn "No outline can be computed for the dummy-method ~A!" dummy)
	   (values outline nil))
  (:method ((tactic infer+inference) (outline list) (parameters list))
	   (warn ";;;INFER~~APPLY-OUTLINE-FUNCTION: No method specified for dealing with object ~A of type ~A."
		 tactic (type-of tactic)))
  #+old(:method ((tactic infer+inference) (outline list) (parameters list))
	   (let* ((outline-pattern (infer~compute-outline-pattern outline))
		  (outline-function (infer~outline-pattern2application tactic outline-pattern)))
	     (if (and (not outline-function)
		      (not (warn ";;; Underspecified! No outline-function for pattern ~A" outline-pattern)))
		 (values outline nil)
	       (let ((new-outline (apply outline-function (list outline parameters))))
		 (if (and (null new-outline)
			  (not (warn ";;; Something went wrong while applying the outline-function to ~A" outline)))
		     (values outline nil)
		   (values new-outline t))))))
  )

(defun infer~compute-outline-pattern (node-list)
  (declare (edited  "06-JUN-1997 16:25")
	   (authors SORGE)
	   (input   "A list of nodes.")
	   (value   "The outline-pattern for NODE-LIST."))
  (mapcar '(lambda (x) (cond ((null x) infer*non-existent)
			     ((consp x) infer*list)
			     ((infer~open-p (node~just-method x)) infer*existent)
			     (t infer*closed)))
	  node-list))

(defgeneric infer~apply-expansion-function (method outline parameters)
  (declare (edited  "25-JAN-1996 15:16")
	   (authors SORGE)
	   (input   "An abstract method, an outline and a parameter-list")
	   (effect  "Applys the expansion-function of the abstract method to both lists.")
	   (value   "The value of the expansion-function applied to outline and parameters."))
  (:method (obj1 obj2 obj3)
	   (error "~A must be a tactic, ~A and ~A both lists." obj1 obj2 obj3))
  (:method (obj (outline list) (parameters list))
	   (error "~A must be a tactic." obj))
  (:method ((method symbol) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~apply-expansion-function tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method string) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~apply-expansion-function tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method :before ((tactic infer+inference) (outline list) (parameters list))
	   (unless (every #'(lambda (x) (or (node~p x) (null x))) outline)
	     (error "The members of ~A should be nodes or nil." outline))
	   (unless (infer~type-test-list parameters (infer~parameter-types tactic))
	     (error "The members of ~A are not of the right type." parameters)))
  (:method :around ((rule infer+rule) (outline list) (parameters list))
	   (warn "Rules like ~A do NOT have expansion-functions!" rule)
	   nil)
  (:method :around ((dummy infer+dummy) (outline list) (parameters list))
	   (warn "Dummy-methods like ~A do NOT have expansion-functions!" dummy)
	   nil)
  (:method ((tactic infer+tactic) (outline list) (parameters list))
	   (apply (symbol-function (infer~expansion-function tactic)) (list outline parameters)))
  (:method ((tactic infer+black-box) (outline list) (parameters list))
	   (apply (symbol-function (infer~expansion-function tactic)) (list outline parameters)))
  )

(defgeneric infer~apply-test-function (method outline parameters)
  (declare (edited  "25-JAN-1996 15:16")
	   (authors SORGE)
	   (input   "An abstract method, an outline and a parameter-list")
	   (effect  "Applys the test-function of the abstract method to both lists.")
	   (value   "The value of the test-function applied to outline and parameters."))
  (:method (obj1 obj2 obj3)
	   (error "~A must be a tactic, ~A and ~A both lists." obj1 obj2 obj3))
  (:method (obj (outline list) (parameters list))
	   (error "~A must be a tactic." obj))
  (:method ((method symbol) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~apply-test-function tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method ((method string) (outline list) (parameters list))
	   (let ((tac (infer~find-method method)))
	     (if tac
		 (infer~apply-test-function tac outline parameters)
	       (error "Abstract method ~A does not exist" method))))
  (:method :before ((tactic infer+inference) (outline list) (parameters list))
	   (unless (every #'(lambda (x) (or (node~p x) (null x))) outline)
	     (error "The members of ~A should be nodes or nil." outline))
	   (unless (infer~type-test-list parameters (infer~parameter-types tactic))
	     (error "The members of ~A are not of the right type." parameters)))
  (:method ((infer infer+inference) (outline list) (parameters list))
	   (warn "There exist no test-functions for objects of type ~A." (type-of infer))
	   nil)
  (:method ((tactic infer+black-box) (outline list) (parameters list))
	   (apply (symbol-function (infer~test-function tactic)) (list outline parameters)))
  )

(defun infer~type-test-list (list types)
  (declare (edited  "25-JAN-1996 16:00")
	   (authors SORGE)
	   (input   "A list and a list of arg+type symbols")
	   (value   "T if the members of list are of the right type,"
		    "otherwise NIL."))
  (unless (= (length list) (length types))
    (error "~A and ~A must have equal length." list types))
  (every #'(lambda (x y) (arg~test-type x y)) types list))


;;; Internal auxiliary functions
(defgeneric infer=read-string (name)
  (declare (edited  "24-JAN-1996 16:56")
	   (authors SORGE)
	   (input   "A name (symbol or string).")
	   (effect  "None.")
	   (value   "NAME as an uppercase string."))
  (:method (name)
	   (error "~A has to be of type symbol or string" name))
  (:method ((name symbol))
	   (symbol-name name))
  (:method ((name string))
	   (string-upcase name)))

(defun infer=print-hashtable (stream table)
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       (format stream "~A " key))
	   table))

(defmethod print-object ((obj (eql infer*method-hash-table)) stream)
  (format stream "#<INFERENCE ")
  (infer=print-hashtable stream obj)
  (format stream ">")
)

(defun infer==res ()
  (declare (edited  "24-JAN-1996 19:36")
	   (authors SORGE)
	   (remark "This little thingy resets the hash-table storing"
		   "the tactics. It's only for testing purposes!"))
  (maphash #'(lambda (x y) (declare (ignore y)) (remhash x infer*method-hash-table))
	   infer*method-hash-table))


#{\subsection{\post\ Interface}#}

(defmacro infer=definference (caller name attribs block instance)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "The calling macro, name and attribs as in the calling macro,"
		    "one additional block for testing and a make-instance.")
           (effect  "Same as the calling macro.")
           (value   "Same as the calling macro."))
  `(let ((name (quote ,name))
	 (attribs (quote ,attribs))
	 (outline-mappings)
	 (outline-function)
	 (supermethod)
	 (test-function)
	 (passkey :formula)
	 (expansion-function) (parameter-types)  (help ""))
     (when (infer~find-method name)
       (inter~print-warning (asi~create)
			    (format nil ";;;infer~~def~A: Redefining inference method ~A!" ,caller name)))
     (do ((attribs (cdr attribs) (cdr attribs))
	  (attrib (car attribs) (car attribs)))
	 ((and (null attrib) (null attribs)))
       (if (consp attrib)
	   (let ((carattrib (if (symbolp (car attrib))
				(symbol-name (car attrib))
			      (car attrib))))
	     (cond 
	      ((string-equal carattrib :outline-mappings) (setq outline-mappings (cadr attrib)))
	      ((string-equal carattrib :outline-function) (setq outline-function (cadr attrib)))
	      ((string-equal carattrib :supermethod) (setq supermethod (cdr attrib)))
	      ((string-equal carattrib :test-function) (setq test-function (cadr attrib)))
	      ((string-equal carattrib :passkey) (setq passkey (cadr attrib)))
	      ((string-equal carattrib :expansion-function) (setq expansion-function (cadr attrib)))
	      ((string-equal carattrib :parameter-types) (setq parameter-types (cdr attrib)))
	      ((string-equal carattrib :help) (setq help (cadr attrib)))
	      (t (error ";;;infer~~def~A: Not expecting ~S" ,caller attrib))))
	 (error ";;;infer~~def~A ~A: ~A is not a valid attribute specification" ,caller name attrib)))
     (unless (symbolp expansion-function)
       (error ";;;infer~~def~A: Expansion-function ~S must be a symbol." ,caller expansion-function))
     (let ((badtype (find-if-not #'arg~find-argtype parameter-types)))
       (when badtype
	 (error ";;;infer~~def~A ~A: Argtype ~S does not refer to an argument type."
		,caller name badtype)))
     ,block
     (let ((new-tactic ,instance))
       (when outline-mappings (infer=om-create-outline-mappings name outline-mappings))
       (setf (gethash (infer=read-string name) infer*method-hash-table) new-tactic)
       )))

(defmacro infer~defbbox (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an INFER+BLACK-BOX."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defbbox otter
	        (outline-function compute-atp-lines)
                (test-function call-otter)
	        (expansion-function call-otter-and-insert)
	        (parameter-types integer boolean)
	        (help \"Prove by OTTER\"))
		    \\end{codebox}"
		    "\\newline Any argtype used in parameter-types must already be defined."
		    "The parameters for outline- and expansion-functions must be a symbol representing"
		    "A function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the tactic, construct a real INFER+BLACK-BOX.")
           (value   "The new tactic."))
  `(infer=definference "bbox" ,name ,attribs
		       (unless (and (symbolp test-function) (symbolp outline-function))
			 (error ";;;infer~~defbbox: Both outline- and test-function for ~A must be a symbol." name))
		       (make-instance 'infer+black-box
				      :name name
				      :test-function test-function
				      :outline-function outline-function
				      :expansion-function expansion-function
				      :parameter-types parameter-types
				      :help help)))


(defmacro infer~deftactic (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an INFER+TACTIC."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~deftactic poly-add
	          (outline-mappings (((nil t) poly-add-forw)
                                     ((t nil) poly-add-back)))
	          (expansion-function poly-add-exp)
	          (parameter-types position)
	          (help \"Apply basic polynomial addition\"))
		    \\end{codebox}"
		    "\\newline Any argtype used in both outline- and parameter-types must already be defined."
		    "The parameters for outline- and expansion-functions must be a symbol representing"
		    "A function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the tactic, construct a real INFER+TACTIC.")
           (value   "The new tactic."))
  `(infer=definference "tactic" ,name ,attribs nil
		  (make-instance 'infer+tactic
				 :name name 
				 :expansion-function expansion-function
				 :parameter-types parameter-types
				 :help help)))


(defmacro infer~defrule (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an INFER+RULE."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defrule forall-e
	      (outline-mappings (((t nil) forall-e-back)
                                 ((t t) forall-e-connect)
                                 ((nil t) forall-e-forw)))
	      (parameter-types symbol)
	      (help \"Apply forall-elimination.\"))
                    \\end{codebox}"
		    "\\newline Any argtype used in both outline- and parameter-types must already be defined."
		    "The defaults slot should have the following syntax:"
		    "The parameters for the outline-mappings must be a symbol representing a"
		    "function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the rule, construct a real INFER+RULE.")
           (value   "The new rule."))
  `(infer=definference "rule" ,name ,attribs
		  (unless (null expansion-function)
		    (warn ";;;infer~~defrule ~A: There are no expansion functions for rules. ~S is discarded."
			  name expansion-function))
		  (make-instance 'infer+rule
				 :name name 
				 :parameter-types parameter-types
				 :help help)))

(defmacro infer~defmethod (name &rest attribs)
  (declare (edited  "27-JUN-1997")
           (authors Lassaad)
           (input   "A written representation of an INFER+METHOD."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defmethod andi-m
	      (outline-mappings (((existent nonexistent nonexistent) andi-m-b)
                                 ((existent existent exsitent) andi-m-a)
                                 ((nonexistent existent existent) andi-m-f)))
	      (help \"Apply and-introduction.\"))
                    \\end{codebox}"
		    "\\newline Any argtype used in both outline- and parameter-types must already be defined."
		    "The defaults slot should have the following syntax:"
		    "The parameters for the outline-mappings must be a symbol representing a"
		    "function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the method, construct a real INFER+METHOD.")
           (value   "The new method."))
  `(infer=definference "method" ,name ,attribs nil
		       (make-instance 'infer+method
				      :name name 
				      :parameter-types parameter-types
				      :help help)))

(defmacro infer~defsupermethod (name &rest attribs)
  (declare (edited  "07-JAN-1998")
	   (authors Jzimmer)
           (input   "A written representation of an INFER+SUPERMETHOD."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defsupermethod normal-s \\
          (supermethod normal-s-b)
	      (help \"The normal inference.\"))
                    \\end{codebox}"
		    "\\newline Any argtype used in both outline- and parameter-types must already be defined."
		    "The defaults slot should have the following syntax:"
		    "The parameters for the outline-mappings must be a symbol representing a"
		    "function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the method, construct a real INFER+SUPERMETHOD.")
           (value   "The new supermethod."))
  `(infer=definference "supermethod" ,name ,attribs NIL
		       (make-instance 'infer+supermethod
				      :name name
				      :supermethod (second (first attribs))
				      :parameter-types parameter-types
				      :help help
				      )))

(defmacro infer~defdummy (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an INFER+DUMMY."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defdummy hyp 
	      (help \"Dummy for hypotheses.\"))
                    \\end{codebox}")
           (effect  "Read the dummy-method, construct a real INFER+DUMMY.")
           (value   "The new dummy-method."))
  `(infer=definference "dummy" ,name ,attribs
		       (cond (expansion-function 
			      (warn ";;;infer~~defdummy ~A: There are no expansion functions for dummys. ~S is discarded."
				    name expansion-function))
			     (outline-mappings
			      (warn ";;;infer~~defdummy ~A: You need no outline-mappings for dummys. ~S is discarded."
				    name outline-mappings))
			     (parameter-types
			      (warn ";;;infer~~defdummy ~A: You need no parameter-types for dummys. ~S is discarded."
				    name parameter-types)))
		       (make-instance 'infer+dummy
				      :name name 
				      :help help)))

(defmacro infer~defopen (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an INFER+OPEN."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defdummy open 
	      (help \"Dummy for open lines.\"))
                    \\end{codebox}")
           (effect  "Read the dummy-method, construct a real INFER+OPEN.")
           (value   "The new dummy-method."))
  `(infer=definference "open" ,name ,attribs
		  (cond (expansion-function 
			 (warn ";;;infer~~defopen ~A: There are no expansion functions for dummys. ~S is discarded."
			       name expansion-function))
			(outline-mappings
			 (warn ";;;infer~~defopen ~A: You need no outline-mappings for dummys. ~S is discarded."
			       name outline-mappings))
			(parameter-types
			 (warn ";;;infer~~defopen ~A: You need no parameter-types for dummys. ~S is discarded."
			       name parameter-types)))
		  (make-instance 'infer+open
				 :name name 
				 :help help))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally we define some crash test dummys....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defdummy hyp (help "Dummy for hypotheses."))

(infer~defopen open (help "Dummy for open lines."))

(infer~defopen planned (help "Dummy for planned lines."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the stuff for them outline-functions....
;; So what do we all need:
;; Datastructure with a mapping facility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline-mappings can be created without direct reference to a method.
;; Outline-mappings can be related to a method afterwards. 
;; Then this outline-mapping might destroy some other outline-mapping corresponding to the method so far.
;; Example: ....
;; creation of an outline-mapping with the name of an existing method will always result in the destruction
;; of any outline-mappings existing for that method so far.

#{\subsection{Outline-Mappings}
The outline-mappings of an abstract method provide the possibility to specifiy different outline-functions for different
outlines, for instance to apply the method forward or backward in a proof plan. 
Outline-mappings are usually created with and related to some abstract method. But they can also be built
independently which gives an implementor the liberty of playing around with different sets of outline-mappings for the
same abstract method.

An outline is always a list of nodes (usually incomplete), where the first node corresponds to the one that will be
grounded with the abstract method. This outline can be translated into an outline-pattern, a list of booleans, where T
means this node was actually provided. According to this pattern the corresponding outline-function is selected and
applied to both the outline and the list of parameters.

Let us take a look at our example once more. Suppose the abstract method DEF has the following outline mappings
... that's all going to change .... VS
\begin{center}
\begin{codebox}
(T T T) --> DEF-CONNECT
(NIL T T) --> DEF-E
(T T NIL) --> DEF-I
\end{codebox}
\end{center}
\noindent and our proof plan is the following:
\begin{center}
  \begin{footnotesize}
    \begin{tabular}{p{.7cm}p{1cm}@{$\,\vdash\,$}p{4cm}p{2cm}}
      {A1}&{A1}&{$\forall x \lambdot \forall y \lambdot P(x,y) \Rightarrow Q(y,x)$}&{(Hyp)}\\
      {A2}&{A2}&{$P(c,d)$}&{(Hyp)}\\
      {Thm}&{A1, A2}&{$Q(c,d)$}&{(Planned)}
  \end{tabular}\end{footnotesize}\end{center}
Applying the abstract method DEF to outline (THM A1 A2) would result in a call to the {\tt DEF-CONNECT}-function and return the same outline, but with a differently justified THM:
\begin{center}
  \begin{footnotesize}
    \begin{tabular}{p{.7cm}p{1cm}@{$\,\vdash\,$}p{4cm}p{2cm}}
      {Thm}&{A1, A2}&{$Q(c,d)$}&{(Def A1 A2)}
  \end{tabular}\end{footnotesize}\end{center}
If we would provide only (NIL A1 A2) as outline the {\tt DEF-E}-function would be called and the line
\begin{center}
  \begin{footnotesize}
    \begin{tabular}{p{.7cm}p{1cm}@{$\,\vdash\,$}p{4cm}p{2cm}}
      {L1}&{A1, A2}&{$Q(c,d)$}&{(Def A1 A2)}
  \end{tabular}\end{footnotesize}\end{center}
would be added to the proof. This line could of course be closed with THM and thus would lead to the same proof.
The third possible outline is (THM A1 NIL); any other outline is underspecified and thus would not change the proof.

#}

(defclass infer=om-outline-mappings (keim+name)
  ((mappings :initform (make-hash-table :test #'equal)
	     :accessor infer=om-mappings
	     :documentation "A list for single mappings."))
  (:documentation "This thingy keeps track of all mappings from outline-patterns to actual outline-functions."))

(defvar infer*outline-mappings-hash-table (make-hash-table :test #'equal)
  "A hash table, that keeps track of all the existing outline-mappings at any time.")

(defmethod print-object ((obj infer=om-outline-mappings) stream)
  (format stream "#<OUTLINE-MAPPINGS for ~A>" (keim~name obj))) 

(defun infer=om-create-outline-mappings (name mappings)
  (declare (edited  "30-MAY-1996 11:05")
	   (authors SORGE MELIS)
	   (input   "A name and a list of mappings.")
	   (effect  "An instance of an INFER=OM-OUTLINE-MAPPINGS.")
	   (value   "The newly created instance."))
  (when (listp mappings)
    (let* ((instance (make-instance 'infer=om-outline-mappings
		     :name name))
	   (hash-table (infer=om-mappings instance)))
      (mapc #'(lambda (x)
		(if (and (listp x) (not (null (cdr x))))
		    (infer=om-mapping-table-entry hash-table (car x) (cadr x))
		  (warn ";;; ~A is an invalid function mapping." x)))
	    mappings)
      (setf (gethash (infer=read-string name) infer*outline-mappings-hash-table) instance)
      (values)
      )))

(defun infer=om-copy-outline-mappings (mappings newname)
  (declare (edited  "17-JUN-1996 22:46")
	   (authors SORGE)
	   (input   "Outline-mappings and a new-name.")
	   (effect  "Copys the outline-mappings.")
	   (value   "The copy of the mappings."))
  (let* ((instance (make-instance 'infer=om-outline-mappings
				  :name (infer=read-string newname)))
	 (new-hash (infer=om-mappings instance))
	 (old-hash (infer=om-mappings mappings)))
    (maphash #'(lambda (x y)
		 (setf (gethash x new-hash) y))
	     old-hash)
    (setf (gethash (infer=read-string newname) infer*outline-mappings-hash-table) instance)
    instance
    ))
  
(defun infer=om-find-outline-mappings (name)
  (declare (edited  "17-JUN-1996 12:24")
	   (authors SORGE)
	   (input   "A name of a outline-mapping.")
	   (value   "The outline-mappings with this name."))
  (gethash (infer=read-string name) infer*outline-mappings-hash-table))

(defun infer=om-mapping-table-entry (hashtable pattern object)
  (declare (edited  "30-MAY-1996 11:04")
	   (authors SORGE MELIS)
	   (input   "A hashtable, a pattern and a symbol.")
	   (effect  "Pattern and object are inserted in the hash table.")
	   (value   "Undefined."))
  (let ((real-pat (mapcar #'infer=read-string pattern)))
    (if (infer~pattern-check real-pat)
      (setf (gethash real-pat hashtable) object)
    (warn ";;; ~A is an invalid pattern." pattern))))

(defun infer=om-delete-table-entry (hashtable pattern)
  (declare (edited  "17-JUN-1996 14:11")
	   (authors SORGE)
	   (input   "A hashtable and a pattern.")
	   (effect  "The function for this pattern is deleted from the hash table.")
	   (value   "Undefined."))
  (if (infer~pattern-check pattern)
      (remhash pattern hashtable)
    (warn ";;; ~A is an invalid pattern." pattern)))

(defun infer=om-add-outline-mapping (name mapping)
  (declare (edited  "17-JUN-1996 16:53")
	   (authors SORGE)
	   (input   "A name and a mapping.")
	   (effect  "Adds the mapping to the outline-mappings.")
	   (value   "Undefined."))
  (let ((mappings (infer=om-find-outline-mappings name)))
    (infer=om-mapping-table-entry (infer=om-mappings mappings)
				  (car mapping)
				  (cadr mapping))))

(defun infer=om-delete-outline-mapping (name pattern)
  (declare (edited  "17-JUN-1996 16:53")
	   (authors SORGE)
	   (input   "A name and a outline-pattern.")
	   (effect  "Removes the mapping for that pattern from the outline-mappings.")
	   (value   "T if an entry has been successfully removed."))
  (let ((mappings (infer=om-find-outline-mappings name)))
    (infer=om-delete-table-entry (infer=om-mappings mappings) pattern)))

(defun infer=om-print-outline-mappings (mappings &optional (stream t))
  (declare (edited  "30-MAY-1996 17:24")
	   (authors SORGE)
	   (input   "Outline-mappings.")
	   (effect  "Prints the outline-mappings.")
	   (value   "Undefined."))
  (let ((maps (infer=om-find-outline-mappings mappings)))
    (if maps (infer=om-pom-help (infer=om-mappings maps) stream)
      (warn ";;; Outline-mappings for ~A do not exist!" mappings)))
  (values))

(defun infer=om-pom-help (table &optional (stream t))
  (declare (edited  "30-MAY-1996 17:28")
	   (authors SORGE)
	   (input   "A hash table of outline-mappings.")
	   (effect  "Prints the mappings.")
	   (value   "Undefined."))
  (terpri stream)
  (maphash #'(lambda (x y)
	      (format stream "~A --> ~A" x y)
	      (terpri stream))
	   table))	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is a bit more complicated....
;; We are trying to map a given outline-pattern to its corresponfing
;; object in the mappings. But as we have :closed lines as a subset
;; of :existent lines, we will have to do some kind of dispatch
;; in order to determine the mapped object.
;; A little example:
;; We get something like (:nonexistent :closed :closed) as pattern
;; if there is no corresponding mapping, we try to match
;; (:nonexistent :closed :existent), then (:nonexistent :existent :closed)
;; and finally (:nonexistent :existent :existent).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun infer=om-get-closed-positions (pattern count)
  (cond ((null pattern) pattern)
	((string-equal (car pattern) infer*closed)
	 (cons count (infer=om-get-closed-positions (cdr pattern) (1+ count))))
	(t (infer=om-get-closed-positions (cdr pattern) (1+ count)))))

(defun infer=om-permute-t-nil (count)
  (when (> count 0)
    (let ((rec-res (infer=om-permute-t-nil (1- count))))
      (if rec-res
	  (append
	   (mapcar #'(lambda (x) (cons T x)) rec-res)
	   (mapcar #'(lambda (x) (cons NIL x)) rec-res))
	(list '(T) '(NIL))))))

(eval-when (load compile eval)
(let ((closure nil))          ;;; specification: input: a pattern consisting of :existent :nonexistent :closed
                              ;;;                value: a pattern permutating the :closed with :existent, nil if we are done  
  (defun infer=om-start-pattern-perm (pattern)
    (let* ((poslist (infer=om-get-closed-positions pattern 0))
	   (perm (infer=om-permute-t-nil (length poslist)))
	   (pat pattern))
      (if (find infer*closed pattern :test #'string-equal)
	  (progn
	    (setf closure
		  #'(lambda ()
		    (when perm
		      (let ((fperm (car perm)))
			(setf perm (cdr perm))
			(mapcar #'(lambda (x y)
				    (if y (setf (nth x pat) infer*closed)
				      (setf (nth x pat) infer*existent)))
				poslist fperm))
		      pat)))
	    (funcall closure))
	pattern)))

  (defun infer=om-perm-pattern ()
    (when closure (funcall closure)))
  
))
  
(defun infer=om-pattern-2-object (mapping pattern)
  (declare (edited  "30-MAY-1996 15:10")
	   (authors SORGE)
	   (input   "An outline-mapping and an outline-pattern.")
	   (value   "The object corresponding to the outline-pattern."))
  (let* ((mappings (infer=om-mappings (infer=om-find-outline-mappings mapping))))
    (do* ((pat (infer=om-start-pattern-perm pattern) (infer=om-perm-pattern))
	  (object (gethash pat mappings) (gethash pat mappings)))
	((or object (null pat)) object)
      )))

(defun infer=om-object-2-pattern (mapping application)
  (declare (edited  "11-MAY-2000")
	   (authors SORGE)
	   (input   "An outline-mapping and an application.")
	   (value   "The outline-pattern corresponding to the application."))
  (let* ((mappings (infer=om-mappings (infer=om-find-outline-mappings mapping))))
    (maphash #'(lambda (key value)
		 (when (string-equal application value)
		   (return-from infer=om-object-2-pattern key)))
	     mappings)))

(defun infer=om-relate-mappings-2-method (name mappings)
  (declare (edited  "17-JUN-1996 22:22")
	   (authors SORGE)
	   (input   "A name of an inference-method and a name of mappings.")
	   (effect  "Copys the mappings to the name of the inference-method."
		    "Any old mappings with that name are deleted.")
	   (value   "T."))
  (let ((maps (infer=om-copy-outline-mappings (infer=om-find-outline-mappings mappings) name)))
    (setf (gethash name infer*outline-mappings-hash-table) maps))
  t)

(defun infer~pattern-check (pattern)
  (declare (edited  "30-MAY-1996 11:03")
	   (authors SORGE MELIS)
	   (input   "A pattern.")
	   (value   "T if the list consists of valid keywords for patterns, i.e. EXISTENT, NONEXISTENT, and CLOSED."))
  (and (listp pattern)
       (every #'(lambda (x) (or (infer~existent-pattern-p x)
				(infer~nonexistent-pattern-p x)
				(infer~closed-pattern-p x)
				(infer~list-pattern-p x)))
	      pattern)))

(defun infer~find-outline-mappings (name)
  (declare (edited  "24-JAN-1996 16:52")
	   (authors SORGE MELIS)
	   (input   "A name of an inference-method.")
	   (value   "T if outline-function mappings exists otherwise nil."))
  (let ((method-name (infer=inference-name name)))
    (multiple-value-bind (mapping bool)
	(infer=om-find-outline-mappings method-name)
      (declare (ignore mapping))
      bool)))

(defgeneric infer~find-outline-methodnames (name)
  (declare (edited  "29-AUG-2001" )
	   (authors Bracz )
	   (input   "An inference-method.")
	   (value   "line-function mappings exists otherwise nil."))
  (:method ((name infer+inference))
	   (infer~methods (infer=inference-name name)))
  (:method ((name symbol))
	   (let (result)
	     (maphash #'(lambda (x y) (push y result))
		      (infer=om-mappings
			 (infer=om-find-outline-mappings name)))
	   result)))

(defun infer~outline-pattern2application (method pattern)
  (declare (edited  "24-JAN-1996 16:52")
	   (authors SORGE MELIS)
	   (input   "A name of a method and an outline-pattern.")
	   (value   "The funcallable outline-function for this pattern if it exists, otherwise nil."))
  (let ((method-name (infer=inference-name method)))
    (unless (infer~pattern-check pattern)
      (error ";;; ~A is an invalid outline-pattern." pattern))
    (if (infer~find-outline-mappings method-name)
	(infer=om-pattern-2-object method-name pattern)
      (warn ";;; ~A does not have any outline-mappings!" method-name))))
  
(defun infer~application2outline-pattern (method application)
  (declare (edited  "11-MAY-2000")
	   (authors SORGE)
	   (input   "A name of a method and an application")
	   (value   "The funcallable outline-function for this pattern if it exists, otherwise nil."))
  (let ((method-name (infer=inference-name method)))
    (if (infer~find-outline-mappings method-name)
	(infer=om-object-2-pattern method-name application)
      (warn ";;; ~A does not have any outline-mappings!" method-name))))
  
(defmacro infer~def-outline-mappings (name &rest attribs)
  (declare (edited  "24-JAN-1996 17:05")
           (authors SORGE)
           (input   "A written representation of an OUTLINE-MAPPINGS."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~def-outline-mappings and-i
                             (((existent nonexistent nonexistent) and-i-back)
                              ((existent closed closed) and-i-connect)
                              ((nonexistent existent existent) and-i-forw)
                              ((existent closed nonexistent) and-i-forw-l)
                              ((existent nonexistent closed) and-i-forw-r)))
		    \\end{codebox}")
           (effect  "Constructs OUTLINE-MAPPINGS which can be referred to by name.")
           (value   "The new OUTLINE-MAPPINGS."))
  (when (infer~find-outline-mappings name)
    (inter~print-warning (asi~create)
			 (format nil ";;;infer~~def-outline-mappings: Redefining outline-mappings for ~A!" name)))
  (infer=om-create-outline-mappings name (car attribs)))

(defun infer~add-outline-mapping (method mapping)
  (declare (edited  "17-JUN-1996 11:18")
	   (authors SORGE)
	   (input   "A method and a mapping.")
	   (effect  "The mapping is added to the outline-mappings of the method.")
	   (value   "Undefined."))
  (let ((method-name (infer=inference-name method)))
    (unless (and (listp mapping) (not (null (cdr mapping))))
      (error ";;; ~A is an invalid function mapping." mapping))
    (if (infer~find-outline-mappings method-name)
	(infer=om-add-outline-mapping method-name mapping)
      (infer=om-create-outline-mappings method-name mapping))))

(defun infer~delete-outline-mapping (method pattern)
  (declare (edited  "17-JUN-1996 11:18")
	   (authors SORGE)
	   (input   "A method and an outline-pattern.")
	   (effect  "The mapping for this pattern is deleted in the outline-mappings of the mathod.")
	   (value   "T if successfully removed, otherwise nil."))
  (let ((method-name (infer=inference-name method)))
    (unless (infer~pattern-check pattern)
      (error ";;; ~A is an invalid outline pattern."pattern))
    (if (infer~find-outline-mappings method-name)
	(infer=om-delete-outline-mapping method-name pattern)
      (warn ";;; ~A does not have any outline-mappings. Nothing deleted!" method-name))))

(defun infer~relate-mappings2method (method mappings)
  (declare (edited  "17-JUN-1996 11:24")
	   (authors SORGE)
	   (input   "A method and a name of outline-mappings or nil.")
	   (effect  "Relates the method with these outline-mappings or, in case of NIL, with none.")
	   (value   "T if successful, o/w nil."))
  (let ((method-name (infer=inference-name method)))
    (if (infer~find-method method-name)
	(progn
	  (when (null mappings) (remhash method-name infer*outline-mappings-hash-table))
	  (if (infer~find-outline-mappings mappings)
	      (infer=om-relate-mappings-2-method method-name mappings)
	    (warn ";;; ~A does not have any outline-mappings. No relations have been set!" mappings)))
      (warn ";;; ~A is not an existing method!" method))))
    
(defun infer~print-outline-mappings (method &optional (stream t))
  (declare (edited  "17-JUN-1996 12:14")
	   (authors SORGE)
	   (input   "A method or its name.")
	   (effect  "Prints the outline-mappings.")
	   (value   "Undefined."))
  (infer=om-print-outline-mappings (infer=inference-name method) stream))

(defun infer~copy-outline-mappings (name newname)
  (declare (edited  "21-JUN-1996 14:59")
	   (authors SORGE)
	   (input   "The name of an abstract method and a new name for the outline-mappings.")
	   (effect  "Copies the outline-mappings for name to newname.")
	   (value   "T if the operatin was successful."))
  (let ((method-name (infer=inference-name name)))
    (if (infer~find-outline-mappings method-name)
	(progn 
	  (infer=om-copy-outline-mappings (infer=om-find-outline-mappings method-name) newname)
	  t)
      nil)))
      
(defun infer~remove-outline-mappings (method)
  (declare (edited  "21-JUN-1996 15:16")
	   (authors SORGE)
	   (input   "An abstract method or its name.")
	   (effect  "Removes the outline-mappings of this method.")
	   (value   "Undefined."))
  (let ((method-name (infer=inference-name method)))
    (when (infer~find-outline-mappings method-name)
      (remhash method-name infer*outline-mappings-hash-table))))

(defgeneric infer=inference-name (method)
  (declare (edited  "17-JUN-1996 19:49")
	   (authors SORGE)
	   (input   "An abstract method or its name.")
	   (value   "The correct and useful name of the method."))
  (:method (method)
	   (error ";;; ~A is neither an inference-method nor a name of one." method))
  (:method ((method symbol))
	   (infer=read-string method))
  (:method ((method string))
	   (infer=read-string method))
  (:method ((method infer+inference))
	   (keim~name method)))
  
(defun infer~closed-pattern-p (pattern)
  (declare (edited  "11-JUN-1997 14:45")
	   (authors SORGE)
	   (input   "A pattern.")
	   (value   "T if it corresponds to the closed-pattern."))
  (string-equal pattern infer*closed))

(defun infer~existent-pattern-p (pattern)
  (declare (edited  "11-JUN-1997 14:45")
	   (authors SORGE)
	   (input   "A pattern.")
	   (value   "T if it corresponds to the existent-pattern."))
  (string-equal pattern infer*existent))

(defun infer~nonexistent-pattern-p (pattern)
  (declare (edited  "11-JUN-1997 14:45")
	   (authors SORGE)
	   (input   "A pattern.")
	   (value   "T if it corresponds to the non-existent-pattern."))
  (string-equal pattern infer*non-existent))

(defun infer~list-pattern-p (pattern)
  (declare (edited  "29-JAN-1999 12:42")
	   (authors SORGE)
	   (input   "A pattern.")
	   (value   "T if it corresponds to the list-pattern."))
  (string-equal pattern infer*list))


;;; von Armin

(defun infer~find-arbitrary-application-name (method)
  (declare (edited  "20-OCT-1998")
	   (authors Afiedler)
	   (input   "An inference method.")
	   (value   "The name of an arbitrary application in METHOD's mappings."))
  (with-hash-table-iterator (entry (infer=om-mappings (infer=om-find-outline-mappings
						       (infer=inference-name method))))
			    (multiple-value-bind (bool key val) (entry)
			      (declare (ignore bool key))
			      val)))

(defgeneric infer~theory (inference)
  (declare (edited  "20-OCT-1998")
	   (authors Afiedler)
	   (input   "An inference method.")
	   (value   "The theory in which INFERENCE was defined, if one exists, otherwise"
		    "NIL."))
  (:method ((inference infer+inference))
	   (warn ";;; There is no theory for ~A" inference)
	   nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Stuff for wild tactics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)
(defclass infer+wild-tactic (infer+inference)
  ((passkey :initarg :passkey
	    :initform :formula
	    :accessor infer~passkey
	    :documentation "A symbol specifying what is used for computing the tactic (examples: :formula, :node).")
   (outline-function :initarg :outline-function
		     :initform nil
		     :accessor infer~outline-function
		     :documentation "A single outline-function is enough, as black-box methods are always applied in one direction."))
  (:documentation "The class of tactics with no fixed outline."))
)

(defmacro infer~defwild-tactic (name &rest attribs)
  (declare (edited  "27-JAN-1999 21:03")
           (authors SORGE)
           (input   "A written representation of an INFER+WILD-TACTIC."
		    "Here is an example of the syntax:\\newline
		    \\begin{codebox}
		    \\vspace{.1cm}
 (infer~defwild-tactic andi*
	        (outline-mappings (((nonexistent list) andi*-f)
                                   ((existent nonexistent) andi*-b)
                                   ((existent list) andi*-a)))
	        (parameter-types )
	        (help \"Simplification of a conjunctive goal.\"))
		    \\end{codebox}"
		    "\\newline Any argtype used in parameter-types must already be defined."
		    "The parameters for outline- and expansion-functions must be a symbol representing"
		    "A function that either exists or is defined prior to the first use of the tactic.")
           (effect  "Read the tactic, construct a real INFER+WILD-TACTIC.")
           (value   "The new tactic."))
  `(infer=definference "wild-tactic" ,name ,attribs
		       (unless (symbolp outline-function)
			 (error ";;;infer~~defwild-tactic: The outline-function for ~A must be a symbol." name))
		       (make-instance 'infer+wild-tactic
				      :name name
				      :passkey passkey
				      :outline-function outline-function
				      :expansion-function expansion-function
				      :parameter-types parameter-types
				      :help help)))

(defun infer~wild-tactic-create (name outline-func exp-func parameter-types help pass-key)
  (declare (edited  "27-JAN-1999 21:08")
	   (authors SORGE)
	   (input   "A string, two function-symbols, a list of ARG+TYPES, a help string, and a symbol.")
	   (effect  "Creates an instance of INFER+WILD-TACTIC.")
	   (value   "The new instance."))
  (make-instance 'infer+wild-tactic
		 :name (infer=read-string name)
		 :outline-function outline-func
		 :expansion-function exp-func
		 :parameter-types parameter-types
		 :passkey pass-key
		 :help help))

(defun infer~wild-tactic-p (obj)
  (declare (edited  "25-MAY-1997 19:45")
	   (authors SORGE)
	   (input   "An object.")
	   (value   "T if OBJ is of type INFER+WILD-TACTIC."))
  (typep obj 'infer+wild-tactic))



