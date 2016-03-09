;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(mod~defmod env :uses (mod keim help)
	    :documentation
	    "Functions which define the environment interface."
	    :exports
	    (env+environment
	     env~p
	     env~lookup-object
	     env~classes-keys
	     env~class-keys
	     env~enter
	     env~remove
	     env~remove-plural
	     env~empty-p
	     env~query
	     env~create
	     env~post-print
	     env~copy
	     )
	    )

#{\section{Environments}

In any \keim\ application it is important to keep track of the actual context of defined objects and to have a
mapping from internal objects to external names, the {\em keys}. This service is brought to you by \keim\
environments.  Since environments are used by most other modules in \keim\ they must be very basic objects and
cannot have any semantic concern for what they are storing. 

It turns out that many higher-level \keim\ objects like proofs, problems, inference rules and the like carry 
their own environment with them, so that in a typical \keim\ application there will be a multitude of
environments that differ only in small parts. In order conserve space and reduce copying \keim\ supports a
simple inheritance mechanism for environments. An environment may have a single parent environment from which
it inherits all objects and keys. Thus an application can have a tree-like inheritance structure of environments.#}

#{\subsection{General}#}

(eval-when (load compile eval)
  (defclass env+environment (help+help)
    ((locals :initarg :locals :accessor env=locals
	     :initform nil
	     :documentation "association list.")
     (direct-parents :initarg :direct-parents :accessor env=direct-parents
		     :initform nil
		     :documentation "list of parent environments.")
     (trans-parents  :initarg :trans-parents :accessor env=trans-parents
		     :initform nil
		     :documentation "list of transitive clpsure of the parent relation."))
    (:documentation "An environment is a way of providing a mapping
between a key and a KEIM object.  The environment just does lookups and
stores, without any semantic concern for what it is storing.")))

;;; The trans-parents slot contains the transitive closure of the parent relation.
;;; It contains redundant information only for efficiency and should not be accesible
;;; from outside this module.   DEF

(defun env~create (&optional (parents nil) (name nil) (help ""))
  (declare (edited  "16-SEP-1996")
	   (authors Fehrer Nesmith)
	   (input   "(optionally) a single parent environment or a list of such, a symbol
as a name and a help string.")
	   (effect  "Creates a new environment.")
	   (value   "the new environment."))
   (etypecase parents
    ((or env+environment list)
     (etypecase name
      (symbol
       (etypecase help
        (string 
	 (let* ((parlist (if (listp parents) parents (list parents)))
		(t-parlist (env=compatible-list nil parlist)))
	   (make-instance 'env+environment :name name
			       :help help
			       :direct-parents parlist
			       :trans-parents t-parlist)))))))))

(defun env=compatible-list (list-of-envs new-envs-list)
  (declare (edited  "17-SEP-1996")
	   (authors Fehrer)
	   (input   "a list of environments supposed to be transitively closed (!) and a second list of environments.")
	   (effect  "none")
	   (value   "if all the environments are compatible, the transitive closure of both lists."))
  (if (null new-envs-list) list-of-envs
    (let ((candidate (car new-envs-list)))
      (etypecase candidate
	(env+environment
	 (if (null list-of-envs)
	     (env=compatible-list (cons candidate (env=trans-parents candidate))
				  (cdr new-envs-list))
	   (env=compatible-list (env=compatible list-of-envs candidate) (cdr new-envs-list))))))))

(defun env=compatible (list-of-envs new)
  (declare (edited  "17-SEP-1996")
	   (authors Fehrer)
	   (input   "a list of environments supposed to be transitively closed (!) and a new environment.")
	   (effect  "none")
	   (value   "if the new environment is compatible, the transitive closure after adding it."))
  (if (member new list-of-envs) list-of-envs
    (env=locally-compatible (env=compatible-list list-of-envs (env=direct-parents new)) new)))

(defun env=locally-compatible (list-of-envs new)
  (declare (edited  "17-SEP-1996")
	   (authors Fehrer)
	   (input   "a list of environments supposed to be transitively closed and to contain the parents of the new environment given as second argument.")
	   (effect  "none")
	   (value   "same list with new environment added if locally compatible."))
  (if (some #'(lambda (x) (some #'(lambda (y) (env=simple-lookup-pair y x)) (mapcar #'car (env=locals new))))
	    list-of-envs)
      (error "environment ~A not compatible with one of (~{~A~^, ~})" (keim~name new) (mapcar #'keim~name list-of-envs))
    (cons new list-of-envs)))


(defun env~copy (env)
  (declare (edited  "18-SEP-1996")
	   (authors Fehrer)
	   (input   "An environment to be copied.")
	   (effect  "creates a new environment that equals the original, but without its name and help property.")
	   (value   "the new environment."))
  (make-instance 'env+environment
		 :direct-parents (copy-list (env=direct-parents env))
		 :trans-parents (copy-list (env=trans-parents env))
		 :locals (copy-alist (env=locals env))))

(defun env~p (env)
  (declare 
   (authors NESMITH)
   (input   "An object.")
   (effect  "None.")
   (value   "T if this object is an environment, NIL otherwise."))
  (typep env 'env+environment))

#{\subsection{Retrieving objects from environments}#}


(defgeneric env~query (key env &key test)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an environment ENV. The keyword TEST, if given, should 
be a predicate which determines when two keys are equal.")
   (effect  "None.")
   (value   "The CLOS class of the object associated with KEY in ENV if any, NIL otherwise."))
  (:method ((key t) (env env+environment) &key (test #'eql))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj))))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj))))
  (:method ((key string) (env env+environment) &key (test #'string=))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj)))))
  
(defgeneric env~lookup-object (key env &key test)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an environment ENV. The keyword TEST, if given, should 
be a predicate which determines when two keys are equal.")
   (effect  "None.")
   (value   "The object associated with KEY in ENV if any, NIL otherwise."))
  (:method ((key t) (env env+environment) &key (test #'eql))
	   (env=simple-lookup key env :test test))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
	   (env=simple-lookup key env :test test))
  (:method ((key string) (env env+environment) &key (test #'string=))
	   (env=simple-lookup key env :test test)))

(defgeneric env~classes-keys (env classlist &optional (recursive T))
  (declare 
   (authors NESMITH)
   (input   "An environment ENV and a list of CLOS classes CLASSLIST.  
Each class is either an actual CLOS class object or a symbol which names the class.")
   (effect  "None.")
   (value   "The keys of all objects in the ENV whose class is among those those
in CLASSLIST."))
  (:method  ((env env+environment) (classlist list) &optional (recursive T))
	    (mapcan #'(lambda (class) (env~class-keys env class recursive))
		    classlist)))

(defgeneric env~class-keys (env class &optional (recursive T))
  (declare 
   (authors RICHTS NESMITH)
   (input   "An environment ENV and a CLOS class CLASS, which is either an
actual class object or a symbol which names it.")
   (effect  "None.")
   (value   "The keys of all objects in the ENV whose class is CLASS."))
  (:method ((env env+environment) (class class) &optional (recursive T))
	   (if recursive
	       (let ((res nil))
		 (dolist (pair (env=locals env) 
			       (remove-duplicates (append (nreverse res) 
							  (mapcan #'(lambda (x)
								      (env~class-keys x class)) (env=direct-parents env)))
						  :from-end t))
		   (when (typep (cdr pair) class)
		     (push (car pair) res))))
	     (mapcan #'(lambda (x) (if (typep (cdr x) class) (list (car x)))) (env=locals env))))
  (:method ((env env+environment) (class symbol) &optional (recursive T))
	   (env~class-keys env (find-class class) recursive)))

(defun env~empty-p (environment)
  (declare (edited  "18-SEP-1996" "05-AUG-1993 13:49")
	   (authors Fehrer ACSEHN)
	   (input "An environment."  )
	   (effect "none." )
	   (value "T, iff the environment is empty."  ))
  (and (null (env=locals environment))
       (every #'env~empty-p (env=direct-parents environment))))


#{\subsection{Manipulating Environments}

Once created empty, environments grow when objects are entered into them by the {\vb ENV~ENTER} function.
This takes as arguments a key (a \lisp\ symbol or a string) 
and a \keim\ object. Since environments do not
care about the semantic content of the objects the association of a key for the object is entirely left to the
user. Note that in deduction system applications it is not customary and sensible to enter variables
into the environment, since members of an environment are not open to \lisp\ garbage collection. Indeed \keim\
variables are stored in their binding occurrences (quantors or abstractions).#}

(defgeneric env~enter (key object env)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an object OBJECT, an environment ENV.")
   (effect   "The object is stored in ENV, associated with KEY.")
   (value  "The changed ENV."))
    (:method ((key symbol) (object keim+object) (env env+environment))
	   (env=simple-enter key object env))
    (:method ((key string) (object keim+object) (env env+environment))
	   (env=simple-enter key object env)))
  

(defgeneric env~remove (key env &key test)
  (declare 
    (authors NESMITH)
    (input   "A KEY and an environment ENV. The keyword TEST, if given, should be"
	     "a predicate which determines when two keys are equal.")
    (effect  "The first object found which is stored under KEY in ENV is removed if it is not in a parent of ENV."
	     "A warning is singnaled if the object is in a parent."
	     "No error is signaled if the KEY has no association in ENV.")
    (value   "The changed ENV."))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
   (env=simple-remove key env :test test))
  (:method ((key string) (env env+environment) &key (test #'string=))
   (env=simple-remove key env :test test)))

(defgeneric env~remove-plural (list-of-keys env &key test)
  (declare 
    (authors NESMITH)
    (input   "A list of KEYS and an environment ENV. The keyword TEST, if given, should be"
	     "a predicate which determines when two keys are equal.")
    (effect  "The objects stored under the KEYS are removed sequentially from ENV."
	     "That is, two occurrences of the same KEY can result in two objects"
	     "being removed.  No error is signaled if the KEY has no association in ENV.")
    (value   "The changed ENV."))
  (:method ((list-of-keys list) (env env+environment) &key (test #'string=))
   (dolist (key list-of-keys env)
     (env~remove key env :test test))))
 
(defun env=simple-lookup-pair (key env &key (test #'eql))
  (declare (edited  "18-SEP-1996")
	   (authors Fehrer)
	   (input   "A key, an environment and keyword TEST predicate (defaults to EQL).")
	   (effect  "none")
	   (value   "Looks up the KEY in the ENVIRONMENT using the given predicate, returns a cons of key and object if found, or else NIL."))
  (or (assoc key (env=locals env) :test test)
      (some #'(lambda (x) (env=simple-lookup-pair key x :test test)) (env=direct-parents env))))

(defun env=simple-lookup (key env &key (test #'eql))
  (declare (edited  "18-SEP-1996")
	   (authors Fehrer)
	   (input   "A key, an environment and keyword TEST predicate (defaults to EQL).")
	   (effect  "None")
	   (value   "Looks up the KEY in the ENVIRONMENT using the given predicate, returns the object associated or NIL if none.  A second value is a boolean which indicates if the key was found or not."))
  (let ((pair (env=simple-lookup-pair key env :test test)))
    (if pair (values (cdr pair) t)
      (values nil nil))))

(defun env=simple-enter (key object env)
  (declare (edited  "17-SEP-1996")
	   (authors Fehrer)
	   (input   "A key, an object and an environment.")
	   (effect  "The environment is changed to add the association between the KEY
and the OBJECT.")
	   (value   "The changed environment."))
  (push (cons key object) (env=locals env)))

;;; ohne Probe auf Enthaltensein!


(defun env=simple-remove (key env &key (test #'eql))
  (declare 
   (authors NESMITH)
   (input   "A key, an environment and a keyword predicate TEST (defaults 
to EQL.")
   (value "The (potentially changed) environment.")
   (effect "The environment is changed to remove the most recent association 
which this key has in the environment, but only if this is a local association,
 i.e. not in a parent environment."))
  (let ((loc (assoc key (env=locals env) :test test)))
    (if loc (setf (env=locals env) (remove key (env=locals env) :key #'car :test test
			      :count 1))
      (if (env=simple-lookup key env :test test)
	  (warn "The key ~A which shall be removed is inherited from a parent environment. It is not removed." key)))
    env))

#{\subsection{Miscellaneous}#}


(defmethod print-object ((env env+environment) stream)
  (declare (edited  "16-SEP-1996")
	   (authors Fehrer)
	   (input   )
	   (effect  )
	   (value   ))
  (format stream "~%Environment ~A~%direct parents: ~{~A~^, ~}~%trans. closure: ~{~A~^, ~}~%objects: ~{~A~^, ~}"
	  (keim~name env)
	  (mapcar #'keim~name (env=direct-parents env))
	  (mapcar #'keim~name (env=trans-parents env))
	  (mapcar #'car (env=locals env))))


;(defmethod print-object ((env env+environment) stream)
;  (format stream "#<ENV ~{~S ~} ~A>" 
;          (mapcar #'car (env=storage env))
;          (if (env=parent env) (env=parent env) "")))

#{The next function is simply a result of the module conventions of \keim, it carries no significance for the
user. The {\vb post} module must use environments, but environments must have a \post\ representation, So here
it is.#}

(defgeneric env~post-print (key object stream)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an OBJECT with which this key is associated and a STREAM.")
   (effect  "Outputs, to STREAM, a suitable environment declaration for this
key and object type.")
   (value   "Undefined.")))



;
;
;#{\subsection{Example session}
;We give an example of a \keim\ session that uses some of the interface functions in this module.
;
;\begin{code}
;KEIM(1): (setq env1 (env~create))
;#<ENV  >
;KEIM(2): (env~empty-p env1)
;T
;{\em we create an environment and see that it is created empty, then we create and enter some named \keim\ objects. 
;Note that the choice of keys for the objects is fully at the whim of the user.}
;KEIM(3):  (setq test (make-instance 'keim+name :name 'test))
;#<KEIM+NAME @ #xbc492e>
;KEIM(4): (env~enter 'test test env1)
;#<ENV TEST  >
;KEIM(5): (env~enter 'tast test env1)
;#<ENV TAST TEST  >
;{\em now we create another environment that inherits everything from the first one, this time we choose to
;give it a name and a help string, then we make and enter a \keim\ object into the new environment.}  
;KEIM(7): (setq env2 (env~create env1 `son "Help String"))
;#<ENV  #<ENV TAST TEST  >>
;KEIM(8): (env~query 'tast env2)
;#<KEIM+NAME TEST>
;KEIM(9): (setq help1 (make-instance 'help+help :name 'tust :help "Help object for testing"))
;#<HELP+HELP TUST>
;KEIM(10): (env~enter 'tust help1 env2)
;#<ENV TUST  #<ENV TAST TEST  >>
;KEIM(11): (env~lookup-object 'tust env2)
;#<HELP+HELP TUST>
;T
;KEIM(111): (env~query 'tust env1)
;NIL
;{\em {\vb TUST} is a member of {\vb ENV2} but not of {\vb ENV1}}
;KEIM(113): (env~remove 'tast env2)
;Warning: The key TAST which shall be removed is only in a parent of 
;the environment. It is not removed.
;#<ENV TUST  #<ENV TAST TEST  >>
;KEIM(114): (env~remove 'tast env1)
;#<ENV TEST  >
;KEIM(115): env2
;#<ENV TUST  #<ENV TEST  >>
;KEIM(117): (env~class-keys env2 'help+help)
;(TUST)
;\end{code}
;#}
;
