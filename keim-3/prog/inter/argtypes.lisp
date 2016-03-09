;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)

(mod~defmod arg :uses (mod keim sys help doc)
            :documentation "Argument types"
	    :exports (
		      arg+type
		      arg~read-function
		      arg~predicate
		      arg~deftype
		      arg~find-argtype
		      arg~argtype-p
		      arg~read-type
		      arg~test-type
		      arg~deflisttype
		      arg~def-or-type
		      arg+input-error
		      arg~input-error-received
		      arg~input-error-expected
		      arg+cannot-read-error
		      arg+wrong-type-error
		      arg~signal-wrong-type
		      )
	    )

#{
\section{Argument types}
\label{mod:arg}
 An argument type (often abbreviated argtype) is an object of the class
 {\vb arg+type}.  Each such type denotes a class of \lisp\ objects, in
 particular, a class which could be required by some command.  That
 is, these classes are used to make sure we apply commands only to
 arguments of the proper type.

 Each argtype defines a read function. This is a generic function, which
 given a Lisp object and perhaps other arguments, determines if the
 object does indeed represent an element of the desired type.  If
 so, it returns it, otherwise it should signal an error.

 Each argtype also has a predicate function, which returns {\vb t} or {\vb nil}
 appropriately if the object tested is a member of the desired class.
#}

;; We'll use this hash table to keep track of all the argtypes that
;; exist at any given time 
(defvar arg*argtype-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by argtype name, that holds all existing arg types.")

(eval-when (load compile eval)
(defclass arg+type (help+help keim+name)
  ((read-function :initarg :read-function :accessor arg=read-function
		  :documentation "A function to read in representations of
this argument type.  Should be a function that takes an input source as
argument (perhaps stream, string, ..., can specialize itself on the type) and
return two values.  If an object of the proper type is read in the object
is returned, other a condition of type {\\vb arg+input-error} is signaled.
A generic function with this name will automatically be defined.")
   (predicate :initarg :predicate :accessor arg=predicate
	      :documentation "A function of one argument, that given a Lisp
object, returns T if the object fits this argument type and NIL otherwise."))
  (:documentation "The class of argument types for inputting arguments to
commands.")))

(defgeneric arg~read-function (argtype)
  (declare (authors nesmith)
	   (input "An argtype.")
	   (value "The argtype's read-function."))
  (:method ((argtype arg+type))
	   (arg=read-function argtype))
  (:documentation "Returns the read function for argument type ARGTYPE."))

(defgeneric arg~predicate (argtype)
  (declare (authors nesmith)
	   (input "An argtype.")
	   (value "The argtype's predicate."))
  (:method ((argtype arg+type))
	   (arg=predicate argtype))
  (:documentation "Returns the predicate function for argument type ARGTYPE."))


;; How to read in an argtype
(eval-when (load compile eval)
(defmacro arg~deftype (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an arg+type. NAME
should be a symbol naming the argtype.  The ATTRIBS should have the form
 (read-function <read-function-name>) (predicate <predicate-name>) (help
<help-string>), and the ATTRIBS can appear in any order.")
           (effect "A new arg+type with the given NAME will be constructed.
<read-function-name> (should be a symbol), will be defined as a
generic function whose default method signals an error of type
arg+cannot-read-error.  <predicate-name> (also a symbol) is not
defined, and must be defined by the argtype implementor.")
	   (value "The new argtype class."))
  (let ((read-function nil)
	(predicate nil)
	(doc nil))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (let ((carattrib (if (symbolp (car attrib))
			       (symbol-name (car attrib)))))
	    (cond ((string-equal carattrib :read-function)
		   (setq read-function (cadr attrib)))
		  ((string-equal carattrib :predicate)
		   (setq predicate (cadr attrib)))
		  ((string-equal carattrib :help)
		   (setq doc (cadr attrib)))
		  (t
		   (error ";;;arg~~deftype: Not expecting ~S" attrib))))))
    (dolist (attrib (mapcar #'cons 
			    (list 'read-function 
				  'predicate
				  'help)
			    (list read-function
				  predicate
				  doc)))
    (unless (cdr attrib)
      (error ";;;arg~~deftype ~A: Must specify ~A." name (car attrib))))
  `(block nil
     (when (arg~find-argtype ',name)
       )
     (let* ((new-argtype
	     (make-instance 'arg+type
	       :name ',name 
	       :read-function ',read-function
	       :predicate ',predicate
	       :help ',doc)))
       (setf (gethash ',(symbol-name name) arg*argtype-hash-table)
	 new-argtype))
    (eval-when (load eval)
     (defgeneric ,read-function (obj &rest others)
       (:documentation 
         ,(format nil "Given a Lisp object, attempts to return ~A." doc)))
     (defmethod ,read-function ((obj t) &rest others)
       (declare (ignore others))
       (sys~signal (sys~make-condition 'arg+cannot-read-error
				       :expected (arg~find-argtype ',name)
				       :received obj))))
     )))
)



(defmethod print-object ((argtype arg+type) stream)
  (format stream "#<arg+type ~A>" (keim~name argtype)))


(defun arg~find-argtype (string-or-symbol)
  (declare 
   (authors nesmith)
   (input "A string or symbol (error if otherwise).")
   (value "The arg+type whose name matches the input, or nil."))
  (gethash (etypecase string-or-symbol 
	     (string string-or-symbol)
	     (symbol (symbol-name string-or-symbol)))
	   arg*argtype-hash-table))

(defun arg~argtype-p (object)
  (declare 
   (authors nesmith)
   (input "Any Lisp object.")
   (value "T if the object is an arg+type, otherwise NIL."))
  (typep object 'arg+type))


(defmacro arg~read-type (type obj &rest others)
  (declare
   (authors nesmith)
   (input "An argtype (object or name) and a Lisp object, optional other 
arguments.")
   (effect "Tries to read the object as a member of the given argtype."))
  `(funcall (arg~read-function 
	     (if (or (symbolp ,type) (stringp ,type))
		 (arg~find-argtype ,type)
	       ,type))
	     ,obj ,@others)
  )

(defmacro arg~test-type (type obj)
  (declare
   (authors nesmith)
   (input "An argtype (object or name) and an object.")
   (effect "Tests the object to see if it actually of the given type."))
  `(funcall (arg~predicate 	     
	     (if (or (symbolp ,type) (stringp ,type))
		 (arg~find-argtype ,type)
	       ,type)) 
	    ,obj)
  )

#{
\subsection{List types}
 Often we will want to define an argument type that is merely a list
 of objects, all of which are of a particular type.  To make this easier,
 we define the macro {\vb arg~deflisttype}.
#}

(defmacro arg~deflisttype (name single-type)
  (declare 
   (authors nesmith)
   (input "A NAME and an existing {\\vb arg+type}, SINGLE-TYPE.")
   (effect "Completely defines a new {\\vb arg+type} of name NAME, such that
the new {\\vb arg+type} denotes lists of type SINGLE-TYPE.  A read-function and
predicate are automatically defined, as well as a doc string.")
   (value "The new argtype."))
  (let ((single-type* (arg~find-argtype single-type)))
     (unless single-type*
       (error "~S does not refer to a valid argument type." single-type))
     (let* ((single-read-fn-name
	     (arg=read-function single-type*))
	    (list-read-fn-name 
	     (gensym (format nil "~A-list" single-read-fn-name)))
	    (single-predicate-name
	     (arg=predicate single-type*))
	    (list-predicate-name 
	     (gensym (format nil "~A-list" single-predicate-name)))
	    (doc
	     (format nil "A list of elements of type ~A." single-type)))
	`(progn
	   (arg~deftype ,name
	     (read-function ,list-read-fn-name)
	     (predicate ,list-predicate-name)
	     (help ,doc))
	   (defmethod ,list-read-fn-name ((obj list) &rest others)
	     (mapcar #'(lambda (x) (,single-read-fn-name x others))
			 obj))
	  (defun ,list-predicate-name (obj)
	     (if (listp obj)
		 (every #',single-predicate-name obj)
		 nil))))))

(pp~modify-style doc+latex-interface
		 ((cons (member arg~deflisttype))
		  (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					    "A list of elements of type ~A."
					    (caddr obj)))))
			   :stream str))
		  15))

(pp~modify-style doc+latex-complete
		 ((cons (member arg~deflisttype))
		  (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					      "A list of elements of type ~A."
					      (caddr obj)))))
			   :stream str))
		  15))

(pp~modify-style doc+latex-documentation
		 ((cons (member arg~deflisttype))
		  (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					      "A list of elements of type ~A."
					      (caddr obj)))))
			   :stream str))
		  15))


#{
 \subsection{Disjunctive types}
 We may want to define an argument type that is a disjunction
 of other types.  To make this easier,
 we define the macro {\vb arg~def-or-type}.
#}

(defmacro arg~def-or-type (name &rest single-types)
  (declare 
   (authors nesmith)
   (input "A NAME and any number (greater than 1) of single types.")
   (effect "Completely defines a new {\\vb arg+type} of name NAME, such that
the new ARG+TYPE denotes a disjunction of the single types given. That is,
an object is of the new argtype NAME iff it is of at least one of the
single types.
A read-function and
predicate are automatically defined, as well as a doc string.")
   (value "The new argtype."))
  (when (< (length single-types) 2)
    (error "Must specify two or more argument types"))
  (let ((single-types* (mapcar #'arg~find-argtype single-types)))
    (when (member nil single-types*)
      (error "~S does not refer to a valid argument type."
	     (nth (position nil single-types*) single-types)))
    (let* ((single-read-fn-names
	    (mapcar #'arg=read-function single-types*))
	   (or-read-fn-name 
	    (gensym (format nil "~A-read-fn" name)))
	   (single-predicate-names
	    (mapcar #'arg=predicate single-types*))
	   (or-predicate-name 
	    (gensym (format nil "~A-pred-fn" name)))
	   (doc
	    (format nil "An element which is a member of one of the types ~A."
		    single-types)))
      `(progn
	 (arg~deftype ,name
		      (read-function ,or-read-fn-name)
		      (predicate ,or-predicate-name)
		      (help ,doc))
	 (defmethod ,or-read-fn-name ((obj t) &rest others)
	   (dolist (read-fn ',single-read-fn-names
		     (arg~signal-wrong-type ',name obj))
	     (sys~handler-case
		 (funcall (symbol-function read-fn) obj others)
	       (arg+wrong-type-error () nil)
	       (arg+cannot-read-error () nil)
	       (:no-error (val) 
		 (return-from ,or-read-fn-name val)))))
	   (defun ,or-predicate-name (obj)
	     (some #'(lambda (x) (funcall (symbol-function x) obj)) 
		   ',single-predicate-names))
	   (arg~find-argtype ',name)))))


(pp~modify-style doc+latex-interface
		 ((cons (member arg~def-or-type))
		   (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					    "An element which is a member of one of the types ~A."
					    (cddr obj)))))
			   :stream str))
		  15))

(pp~modify-style doc+latex-complete
		 ((cons (member arg~def-or-type))
		   (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					      "An element which is a member of one of the types ~A."
					    (cddr obj)))))
			   :stream str))
		  15))

(pp~modify-style doc+latex-documentation
		 ((cons (member arg~def-or-type))
		   (lambda (str obj)
		    (write `(arg~deftype 
			     ,(cadr obj)
			     (help ,(let ((*print-pretty* nil))
				      (format nil 
					    "An element which is a member of one of the types ~A."
					    (cddr obj)))))
			   :stream str))
		  15))



#{
\subsection{Error handling}

 We set up some condition types to be used by read-functions.

 {\vb arg+input-error} is the most general argument input error, using 
{\vb sys+error}.
 Each instance contains two slots: EXPECTED, which should be the
 {\vb arg+type} that was expected; and RECEIVED, which should be the \lisp\ 
 object that was actually received by the read-function.

#}
(sys~define-condition arg+input-error (sys+error)
  ((received nil) (expected nil))
  (lambda (condition stream)
	     (let ((received (arg+input-error-received condition))
		   (expected (arg+input-error-expected condition)))
	       (format stream "Expecting ~A, but got ~S."
		       (help~help-string expected) received))))

(defmacro arg~input-error-received (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an arg+input-error, its RECEIVED slot will be
returned. Otherwise an error will result."))
  `(arg+input-error-received ,err))

(defmacro arg~input-error-expected (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an arg+input-error, its EXPECTED slot will be
returned. Otherwise an error will result."))
  `(arg+input-error-expected ,err))

#{
 {\vb arg+cannot-read-error} is to be used when the read-function for an
 argument type has no method for the object to be read.  It is a subclass
 of {\vb arg+input-error}.
#}

(sys~define-condition arg+cannot-read-error (arg+input-error)
  () 		      
  (lambda (condition stream)
	     (let ((received (arg+input-error-received condition))
		   (expected (arg+input-error-expected condition)))
	       (format stream "Expecting ~A, but got ~S, which I don't
 know to read." (help~help-string expected) received))))

#{
 {\vb arg+wrong-type-error} is to be used when the read-function for an
 argument type determines that the object it is reading cannot be
 of the expected type.   It is a subclass
 of {\vb arg+input-error}.
#}

(sys~define-condition arg+wrong-type-error (arg+input-error)
		      () 		      
		      (lambda (condition stream)
			(let ((received (arg+input-error-received condition))
			      (expected (arg+input-error-expected condition)))
			  (format stream "Expecting ~A, but got ~S, which is the wrong type." (help~help-string expected) received))))



(defun arg~signal-wrong-type (expected received)
  (declare 
   (authors nesmith)
   (input "An expected argument type (string or symbol) and the object
that was received by the read-function.")
   (effect "Signals a condition of type {\\vb arg+wrong-type-error} with the
given arguments.")
   (value "The new argtype."))
  (sys~signal (sys~make-condition 'arg+wrong-type-error
				   :expected (arg~find-argtype expected)
				   :received received)))



;(in-package :ags)

(eval-when (load compile eval)

(defmethod doc~parse-def ((type (eql 'arg~deftype)) obj)
  (values (cadr obj) "Argument Type" nil
	  (remove-if 
	   #'(lambda (x) (null (cdr x)))
	   `((remark ,(cadr (assoc 'help (cddr obj) :test #'string=)))
	     (misc 
	      ,@(remove 
		 nil
		 (let ((pred (cadr (assoc 'predicate (cddr obj) :test #'string=)))
		       (reader (cadr 
				(assoc 'read-function (cddr obj) :test #'string=))))
		 (list
		   (when (and pred
			      (or (typep pred 'doc+interface)
				  (eq (find-package "LISP")
				      (symbol-package pred))))
			   (format nil "The predicate function for this argument type is ~A." pred))
		   (when (and reader 
			      (or (typep reader 'doc+interface)
				  (eq (find-package "LISP")
				      (symbol-package reader))))
		     (format nil "The reader function for this argument type is ~A." reader))))))))))

(pp~modify-style doc+latex-interface
		 ((cons (member arg~deftype))
		  doc~latex-lispdef
		  15))

(pp~modify-style doc+latex-complete
		 ((cons (member arg~deftype))
		  doc~latex-lispdef
		  15))

(pp~modify-style doc+latex-documentation
		 ((cons (member arg~deftype))
		  doc~latex-lispdef
		  15))

)
