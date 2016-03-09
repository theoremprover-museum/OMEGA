;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
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
 
;;;;;;;;;;;;
;;; Still missing:
;;;  - some of the predicates

(mod~defmod DFLT 
            :uses (com foci help keim node pdsn)
            :documentation "The definition of the basic functionality for argument agents."
            :exports (
                      dflt+agent
                      dflt+bb-entry
                      dflt+blackboard
                      dflt+c-pred-agent
                      dflt+function-agent
                      dflt+predicate-agent
                      dflt+s-pred-agent
                      
                      dflt~agent-applicable-p
                      dflt~agent-p
		      dflt~all-agents
                      dflt~apply-agent
                      dflt~bb-agents
                      dflt~bb-command
                      dflt~bb-entries
                      dflt~bb-entry-create
                      dflt~bb-level
                      dflt~bb-ordering
                      dflt~blackboard-create
                      dflt~c-pred-agent-p
                      dflt~command
                      dflt~defagent
                      dflt~default-computation
		      dflt~defmatrix
                      dflt~dependence
                      dflt~enter-bb-entries
                      dflt~enter-bb-entry
                      dflt~entry-mapping
                      dflt~entry-status
                      dflt~entry-visited
                      dflt~find-agent
                      dflt~find-agents
                      dflt~fragment
                      dflt~function
                      dflt~function-agent-p
                      dflt~get-agent
                      dflt~goal
                      dflt~initialize-agent
                      dflt~initialize-blackboard
                      dflt~level
                      dflt~predicate
                      dflt~predicate-agent-p
                      dflt~s-pred-agent-p
                      dflt~sort-bb-entries
                      dflt~survey-blackboard
		      dflt~output

		      dflt*output
                      dflt*agent-hash-table
                      dflt*blackboard-hash-table
                      dflt*command2agent-hash-table
                      dflt*complexity-level))


;;#{
;;\subsection{Default Computations for Commands}
;;#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for the default-mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)

  (defclass dflt+agent (help+help)
    ((command :accessor dflt~command
	      :initarg :command
	      :initform nil
	      :documentation "The command the argument agent belongs to.")
     (fragment :accessor dflt~fragment
	       :initarg :fragment
	       :initform nil
	       :documentation "The fragment the argument agent belongs to.")
     (goal :accessor dflt~goal
	   :initarg :goal
	   :initform nil
	   :documentation "The argument the agent is defined for.")
     (dependence :accessor dflt~dependence
		 :initarg :dependence
		 :initform nil
		 :documentation "The additional arguments required for the agent.")
     (level :accessor dflt~level
	    :initarg :level
	    :initform 1
	    :documentation "The level of complexity of the agent."))
    (:documentation "The basic agent class."))

  (defclass dflt+predicate-agent (dflt+agent)
    ((predicate :accessor dflt~predicate
		:initarg :predicate
		:initform nil
		:documentation "The predicate the agent uses."))
    (:documentation "The superclass of predicate agents."))

  (defclass dflt+s-pred-agent (dflt+predicate-agent)
    ()
    (:documentation "Predicate agent for support lines."))

  (defclass dflt+c-pred-agent (dflt+predicate-agent)
    ()
    (:documentation "Predicate agent for open lines."))

  (defclass dflt+function-agent (dflt+agent)
    ((function :accessor dflt~function
	       :initarg :function
	       :initform nil
	       :documentation "The function the agent uses."))
    (:documentation "The superclass of function agents."))
  )

(defvar dflt*agent-hash-table (make-hash-table :test #'equal))
(defvar dflt*command2agent-hash-table (make-hash-table :test #'equal))
(defvar dflt*output t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for the Blackboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

  (defclass dflt+blackboard (keim+name)
    ((command :accessor dflt~bb-command
	      :initarg :bb-command
	      :initform nil
	      :documentation "A backward-reference to the command the blackboard is initialized for.")
     (ordering :accessor dflt~bb-ordering
	       :initarg :bb-ordering
	       :initform nil
	       :documentation "The arguments as reference-ordering of the blackboard entries.")
     (level :accessor dflt~bb-level
	    :initarg :bb-level
	    :initform 1
	    :documentation "The level of complexity of the agent.")
     (agents :accessor dflt~bb-agents
	     :initarg :bb-agents
	     :initform nil
	     :documentation "A list of agents associated with the balck-board.")
     (entries :accessor dflt~bb-entries
	      :initarg :bb-entries
	      :initform nil
	      :documentation "A list of blackboard entries."))
    (:documentation "The class of blackboards."))

  (defclass dflt+bb-entry (keim+object)
    ((mapping :accessor dflt~entry-mapping
	      :initarg :mapping
	      :initform nil
	      :documentation "A association-list mapping a paremeter to a value")
     (status :accessor dflt~entry-status
	     :initarg :status
	     :initform nil
	     :documentation "The status of the entry, i.e. completed or not.")
     (visited :accessor dflt~entry-visited
	      :initarg :visited
	      :initform nil
	      :documentation "A list of agents that have already visited the bb-entry."))
    (:documentation "The class of blackboard entries."))

  )

(defvar dflt*blackboard-hash-table (make-hash-table :test #'equal)
  "A hash-table keeping track of all existing blackboards. This might be useful some day in the future.")

(defvar dflt*complexity-level 100
  "The level of complexity for which agents are being invoked. (100 should be rather high.)")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The default-mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dflt~agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type DFLT+AGENT."))
  (typep obj 'dflt+agent))

(defun dflt~function-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type DFLT+FUNCTION-AGENT."))
  (typep obj 'dflt+function-agent))

(defun dflt~predicate-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type DFLT+PREDICATE-AGENT."))
  (typep obj 'dflt+predicate-agent))

(defun dflt~s-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type DFLT+S-PRED-AGENT."))
  (typep obj 'dflt+s-pred-agent))

(defun dflt~c-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type DFLT+C-PRED-AGENT."))
  (typep obj 'dflt+c-pred-agent))

(defgeneric dflt~find-agent (command goal depend)
  (declare (edited  "13-APR-1998")
	   (authors Sorge)
	   (input   "A command or its name, and two symbols.")
	   (effect  "None.")
	   (value   "The argument agent belonging to the specification if it exists."))
  (:method ((command com+command) goal depend)
	   (dflt~find-agent (keim~name command) goal depend))
  (:method (command goal depend)
	   (gethash
	    (dflt=unique-agent-name command goal depend)
	    dflt*agent-hash-table)))
	   
(defun dflt~get-agent (agent-name)
  (declare (edited  "13-APR-1998")
	   (authors Sorge)
	   (input   "The name of an agent.")
	   (value   "The corresponding agent, if it exists."))
  (let ((name (etypecase agent-name
		     (string (string-upcase agent-name))
		     (symbol (symbol-name agent-name)))))
    (gethash name dflt*agent-hash-table)))

(defun dflt~find-agents (command)
  (declare (edited  "13-APR-1998")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "None.")
	   (value   "All agents associated with the command."))
  (let* ((command-name (if (com~command-p command)
			   (keim~name command)
			 command))
	 (com-name (etypecase command-name
		     (string (string-upcase command-name))
		     (symbol (symbol-name command-name)))))
    (mapcar #'cdr (gethash com-name dflt*command2agent-hash-table))))

(defgeneric dflt~find-blackboard (blackboard)
  (declare (edited  "16-APR-1998")
	   (authors Sorge)
	   (input   "A blackboard or its name or a command.")
	   (value   "The corresponding blackboard, if it exists."))
  (:method ((command com+command))
	   (dflt~find-blackboard (keim~name command)))
  (:method ((blackboard symbol))
	   (dflt~find-blackboard (symbol-name blackboard)))
  (:method ((blackboard dflt+blackboard))
	   (dflt~find-blackboard (keim~name blackboard)))
  (:method ((blackboard string))
	   (gethash (string-upcase blackboard) dflt*blackboard-hash-table)))
  
  
(defmacro dflt~defagent (command fragment specification &rest attribs) 
  (declare (edited  "08-APR-1998")
	   (authors Sorge)
	   (input   "An agent for default computation. It is either an argument-FUNCTION or"
		    "-Support or -Conclusion-PREDicate."
		    "It has to refer to both a valid command and a list of command-parameters, syntactically"
		    "equal to those defined in the command. Thus an agent can only be created after the"
		    "corresponding command has been defined."
		    "Example:
 \\begin{code}
 (dflt~defagent andi rules
   s-predicate
   (for LConj)
   (uses Conjunction)
   (definition (and (logic~conjunction-p Conjunction)
                    (data~equal LConj (first (data~appl-arguments Conjunction)))))
   (level 1)
   (help \"Predicate for the left conjunct of a given conjunction.\"))
 \\end{code}")
	   (effect  "Defines an agent using this specification.")
	   (value   "Unspecified."))
  `(let* ((fragment (let ((frag (com~find-fragment ',fragment)))
		     (unless frag
		       (error ";;;DFLT~~DEFAGENT: The fragment ~A does not exist." ',fragment))
		     frag))
	 (command (let ((com (com~find-command ',command fragment)))
		    (unless com
		      (error ";;;DFLT~~DEFAGENT: The command ~A does not exist." ',command))
		    com))
	 (spec ',specification))
     (unless (find spec '(function c-predicate s-predicate) :test #'string-equal)
       (error ";;;DFLT~~DEFAGENT: Unexpected agent type ~A." spec))
     (dflt=defagent command fragment (cons spec ',attribs))))

     
(defun dflt=defagent (command fragment agent &optional (meta-functions nil))
  (let ((spec (car agent))
	(attribs (cdr agent))
	(level 1) (help "") 
	(goal) (depend) (definition))
    (unless (find spec '(function c-predicate s-predicate) :test #'string-equal)
      (error ";;;Agent-definition: Unexpected agent type ~A." spec))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (cond 
	   ((string-equal (car attrib) :uses)        (setq depend      (cdr attrib)))
	   ((string-equal (car attrib) :for)         (setq goal        (cadr  attrib)))
	   ((string-equal (car attrib) :level)       (setq level       (cadr  attrib)))
	   ((string-equal (car attrib) :definition)  (setq definition  (cadr  attrib)))
	   ((string-equal (car attrib) :help)        (setq help (cadr attrib)))
	   (t (error ";;;Agent-definition: Not expecting ~A" (car attrib))))
	(error ";;;Agent-definition: Not expecting ~A" attrib)))
    (when (dflt=test-arguments (cons goal depend) command)
      (let ((real-def (dflt=build-function&predicate definition goal depend meta-functions)))
	(if real-def
	    (let ((new-agent (dflt=make-agent spec
					      real-def
					      :name (dflt=unique-agent-name (keim~name command) goal depend)
					      :command command
					      :fragment fragment
					      :goal goal
					      :dependence depend
					      :help help
					      :level (if (numberp level) level 1))))
	      (dflt=register-new-agent new-agent command)
	      new-agent)
	  (error ";;;Agent-definition: The following things could not be referenced to: ~A" real-def)))))) 
                                                                  ;;; hier noch Fehlermeldung genauer...
(defun dflt=register-new-agent (agent command)
  (let* ((com-name (etypecase (keim~name command)
		     (string (string-upcase (keim~name command)))
		     (symbol (symbol-name (keim~name command)))))
	 (agent-name (keim~name agent))
	 (agent-list (gethash com-name dflt*command2agent-hash-table)))
    (if (assoc agent-name agent-list :test #'string-equal)
	(rplacd (assoc agent-name agent-list :test #'string-equal) agent)
      (setf (gethash com-name dflt*command2agent-hash-table)
	      (acons agent-name agent agent-list)))
    (setf (gethash agent-name dflt*agent-hash-table) agent)))

(defun dflt=make-agent (spec def &rest slots)
  (let ((class (cond ((string-equal spec :function)
		      (list (quote dflt+function-agent) :function def))
		     ((string-equal spec :s-predicate)
		      (list (quote dflt+s-pred-agent) :predicate def))
		     ((string-equal spec :c-predicate)
		      (list (quote dflt+c-pred-agent) :predicate def)))))
    (apply #'make-instance (append class slots))))

(defmacro dflt~defmatrix (command fragment &rest attribs) 
  (declare (edited  "08-APR-1998")
	   (authors Sorge)
	   (input   "A matrix specifying a set of agents for default computation."
		    "Example:
 \\begin{code}
  (dflt~defmatrix andi rules
   (agents (c-predicate (for Conjunction)
                        (uses )
                        (definition (logic~conjunction-p Conjunction)))
           (c-predicate (for Conjunction)
                        (uses LConj)
                        (definition (pred1 Conjunction LConj)))
           (c-predicate (for Conjunction)
                        (uses RConj)
                        (definition (pred2 Conjunction RConj)))
           (c-predicate (for Conjunction)
                        (uses LConj RConj)
                        (definition (and (pred1 Conjunction LConj)
                                         (pred2 Conjunction RConj))))
           (s-predicate (for RConj)
                        (uses Conjunction)
                        (definition (pred2 Conjunction RConj)))
	   (s-predicate (for LConj)
                        (uses Conjunction)
                        (definition (pred1 Conjunction LConj))
                        (help \"Predicate for the left conjunct of a given conjunction.\")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))
 \\end{code}")
	   (effect  "Several agents using this specification.")
	   (value   "Unspecified."))
  `(let* ((fragment (let ((frag (com~find-fragment ',fragment)))
		     (unless frag
		       (error ";;;DFLT~~DEFMATRIX: The fragment ~A does not exist." ',fragment))
		     frag))
	 (command (let ((com (com~find-command ',command fragment)))
		    (unless com
		      (error ";;;DFLT~~DEFMATRIX: The command ~A does not exist." ',command))
		    com))
	 (attribs ',attribs)
	 (agents) (functions) (predicates))
     (do ((attribs (cdr attribs) (cdr attribs))
	  (attrib (car attribs) (car attribs)))
	 ((and (null attrib) (null attribs)))
       (if (consp attrib)
	   (cond 
	    ((string-equal (car attrib) :agents)     (setq agents      (cdr attrib)))
	    ((string-equal (car attrib) :functions)  (setq functions   (cdr attrib)))
	    ((string-equal (car attrib) :predicates) (setq predicates  (cdr attrib)))
	    (t (error ";;;DFLT~~DEFMATRIX: Not expecting ~A" (car attrib))))
	 (error ";;;DFLT~~DEFMATRIX: Not expecting ~A" attrib)))
     (format t "~% Defining ~A default agents for command ~A" (length agents) command)
     (mapcar #'(lambda (agent)
		 (dflt=defagent command fragment agent (append functions predicates)))
	     agents)))

(defgeneric dflt~initialize-agent (agent)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "An agent-structure and a assoc-list with arguments.")
	   (effect  "Creates a function or predicate.")
	   (value   "A function or predicate if the necessary parameters for the agent"
		    "are instantiated in the argument list. If not NIL is returned."))
  (:method (agent)
	   (error "~A should be an agent structure." agent))
  (:method ((agent dflt+predicate-agent))
	   (unless (functionp (dflt~predicate agent))
	     (let* ((depend (dflt~dependence agent))
		    (goal (dflt~goal agent))
		    (func-name (read-from-string (keim~name agent)))
		    (func (list 'defun func-name depend
				(list 'lambda (list goal)
				      (dflt~predicate agent)))))
	       (compile
		(eval func))
	       (setf (dflt~predicate agent) (symbol-function func-name))))
	   (dflt~predicate agent))
  (:method ((agent dflt+function-agent))
	   (unless (functionp (dflt~function agent))
	     (let* ((depend (dflt~dependence agent))
		    (func-name (read-from-string (keim~name agent)))
		    (func (list 'defun func-name depend
				(dflt~function agent))))
	     (compile 
	      (eval func))
	     (setf (dflt~function agent) (symbol-function func-name))))
	   (dflt~function agent)))
		  
(defgeneric dflt~apply-agent (agent bb-entry &optional (proof-context foci*proof-context))
  (declare (edited  "12-APR-1998")
	   (authors Sorge)
	   (input   "An agent, a mapping of a blackboard entry and optional a proof-plan.")
	   (effect  "None.")
	   (value   "A list of updated blackboard entris for predicate agents and a single"
		    "updated entry in case of function agents. Results are provided, if the"
		    "agent could search with respect to BB-ENTRY. If not, NIL is returned."))
  (:method (agent (bb-entry list) &optional proof-context)
	   (declare (ignore proof-context))
	   (error "~A should be an agent structure." agent))
  (:method (agent bb-entry &optional proof-context)
	   (declare (ignore agent proof-context))
	   (error "~A is not a valid blackboard-entry." bb-entry))
  (:method (agent (bb-entry dflt+bb-entry) &optional (proof-context foci*proof-context))
	   (dflt~apply-agent agent (dflt~entry-mapping bb-entry) proof-context))
  (:method ((agent dflt+c-pred-agent) (bb-entry list) &optional (proof-context foci*proof-context))
	   (let* ((depend (dflt~dependence agent))
		  (goal (dflt~goal agent))
		  (args (dflt=get-arguments depend bb-entry))
		  (pred (dflt~initialize-agent agent)))
	     (if (or (and depend (not args))
		     (dflt=lookup-goal goal bb-entry))
		 bb-entry
	       (progn (dflt=output-agent agent bb-entry)
		      (let ((new-values (foci~find-open (apply pred args) proof-context)))
			(when new-values
			  (let ((new-bb (dflt~enter-bb-entries (mapcar #'(lambda (new-value)
									   (cons goal new-value))
								       new-values)
							       bb-entry
							       (dflt~command agent))))
			    (dflt=output-agent agent new-bb t)
			    new-bb)))))))
  (:method ((agent dflt+s-pred-agent) (bb-entry list) &optional (proof-context foci*proof-context))
	   (let* ((depend (dflt~dependence agent))
		  (goal (dflt~goal agent))
		  (args (dflt=get-arguments depend bb-entry))
		  (pred (dflt~initialize-agent agent)))
	     (if (or (and depend (not args))
		     (dflt=lookup-goal goal bb-entry))
		 bb-entry
	       (progn (dflt=output-agent agent bb-entry)
		      (let ((new-values (foci~find-support (apply pred args) proof-context)))
			(when new-values
			  (let ((new-bb (dflt~enter-bb-entries (mapcar #'(lambda (new-value)
									   (cons goal new-value))
								       new-values)
							       bb-entry (dflt~command
									 agent))))
			    (dflt=output-agent agent new-bb t)
			    new-bb)
			  ))))))
  (:method ((agent dflt+function-agent) (bb-entry list) &optional proof-context)
	   (declare (ignore proof-context))
	   (let* ((depend (dflt~dependence agent))
		  (goal (dflt~goal agent))
		  (args (dflt=get-arguments depend bb-entry))
		  (pred (dflt~initialize-agent agent)))
	     (if (or (and depend (not args))
		     (dflt=lookup-goal goal bb-entry))
		 bb-entry
	       (progn (dflt=output-agent agent (list bb-entry))
		      (let ((new-value (apply pred args)))
			(when new-value
			    (let ((new-bb (dflt~enter-bb-entry (cons goal new-value)
							       bb-entry (dflt~command
									 agent))))
			      (dflt=output-agent agent (list new-bb) t)
			      new-bb)
			    )))))))

(defun dflt=output-agent (agent bb-entry &optional (completed nil))
  (when dflt*output
    (let ((mapping (mapcar #'(lambda (x)
			       (concatenate 'string
					    (format nil "~A: " (car x))
					    (cond ((pdsn~p (cdr x))
						   (format nil "~A |- ~A"
							   (keim~name (cdr x))
							   (node~formula (cdr x))))
						  (t (format nil "~A" (cdr x))))))
			   (if (listp (caar bb-entry))
			       (car bb-entry)
			     bb-entry))))
      (let ((interface (comint~interface comint*current-comint)))
	(if completed
	    (inter~print-message interface  
				 (format nil "~%Agent ~A completed PAI:~%~{    ~A~%~}" (keim~name agent)  mapping))
	  (inter~print-message interface
			       (format nil "~%Agent ~A trying to complete PAI:~%~{    ~A~%~}" (keim~name agent)	mapping))))))
  )

(defun dflt~enter-bb-entries (new-entries bb-entry command)
  (mapcar #'(lambda (new-entry)
	      (dflt~enter-bb-entry new-entry bb-entry command))
	  new-entries))


(defun dflt~enter-bb-entry (new-entry bb-entry command)   ;;;; schoener machen VS.!!!
  (declare (edited  "12-APR-1998")
	   (authors Sorge)
	   (input   "A new parameter-entry, a blackboard-entry and a command.")
	   (effect  "None.")
	   (value   "A blackboard-entry containing the new parameter-entry."))
  (let ((args (com~argnames command)))
    (remove-if #'null
	       (mapcar #'(lambda (x)
			   (if (string-equal (car new-entry) x)
			       new-entry
			     (assoc x bb-entry :test #'string-equal)))
		       args))))
	
(defgeneric dflt~agent-applicable-p (agent bb-entry)
  (declare (edited  "13-APR-1998")
	   (authors Sorge)
	   (input   "An agent and a blackboard entry.")
	   (value   "T if the agent can be applied to the entry, o/w NIL."))
  (:method (agent bb-entry)
	   (error "~A should be an agent and ~A a balckboard entry." agent bb-entry))
  (:method ((agent dflt+agent) (bb-entry dflt+bb-entry))
	   (and (not (find (keim~name agent)
			   (dflt~entry-visited bb-entry)
			   :test #'string-equal))
		(dflt~agent-applicable-p agent (dflt~entry-mapping bb-entry))))
  (:method ((agent dflt+agent) (bb-entry list))
	   (let ((depend (dflt~dependence agent))
		 (goal (dflt~goal agent)))
	     (and (or (not depend)
		      (dflt=get-arguments depend bb-entry))
		  (not (dflt=lookup-goal goal bb-entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions  for Argument Agents.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dflt=unique-agent-name (command goal depend)
  (declare (edited  "08-APR-1998")
	   (authors Sorge Chris)
	   (input   "A command name, a goal name and a list of other parameter names.")
	   (effect  "None.")
	   (value   "A string representing a unique name for an agent."))
  (flet ((name2string (name)
		      (etypecase name
			(symbol (symbol-name name))
			(string (string-upcase name)))))
    (let ((com-name (name2string command))
	  (goal-name (name2string goal))
	  (depend-names (sort (mapcar #'name2string depend) #'string<)))
      (concatenate 'string
		   com-name "-"
		   goal-name
		   (when depend
		     (concatenate 'string
				  "="
				  (let ((depend-string (car depend-names)))
				    (dolist (el (cdr depend-names) depend-string)
				      (setf depend-string (concatenate 'string depend-string "*" el))))))))))
			      
  
(defun dflt=test-arguments (args command)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A list of arguments and a command.")
	   (effect  "None.")
	   (value   "T if the argument-names in the list are syntactically equal with"
		    "arguments defined for the command."))
  (let ((com-args (com~argnames command)))
    (every #'(lambda (x) (or (find x com-args :test #'string-equal)
			     (warn "Specified argument ~A does not exist!." x)))
	   args)))

(defun dflt=build-function&predicate (definition goal depend &optional meta-functions)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "The definition of a predicate, the goal argument and a list of"
		    "dependency arguments. Optionally a list of meta-function definitions.")
	   (effect  "None.")
	   (value   "A funcallable expression."))
  (cond ((functionp definition) definition)
	((dflt=find-metafunc definition meta-functions)
	 (dflt=build-function&predicate (dflt=match-metafunc definition meta-functions)
					goal depend meta-functions))
	(t (dflt=rewrite-definition definition goal depend meta-functions))))

(defun dflt=rewrite-definition (def goal depend &optional meta-functions)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "An expression of a predicate or function and a list of arguments."
		    "Optionally a list of meta-function definitions.")
	   (effect  "None.")
	   (value   "The expression with certain keywords substituted."))
  (if (atom def) (if (find def (cons goal depend))
		     (list 'node~formula def)   
		   def)
    (let* ((function (car def))
	   (arguments (if (string-equal function :lambda)
			  (dflt=rename-bound-vars (cadr def) (cddr def))
			(cdr def))))
      (multiple-value-bind (func error)
	  (ignore-errors (symbol-function function))
	(declare (ignore func))
	(if error
	    (case function
;;;	      (:no-func (append (first arguments) (dflt=rewrite-definition (cdr arguments) goal depend)))
	      (:protect arguments)
	      (:param (first arguments))
	      (:node (first arguments))
	      (:hyps (list 'pdsn~hyps (first arguments)))
	      (:just (list 'node~justification (first arguments)))
	      (t (error "~A does not have a function definition..." function)))
	  (cons function
		(mapcar #'(lambda (x) (dflt=build-function&predicate x goal depend meta-functions)) arguments)))))))

(defun dflt=rename-bound-vars (binder scope)
  (declare (edited  "04-SEP-1998")
	   (authors Sorge)
	   (input   "A list of lambda bound variables and the scope containing the functionality.")
	   (effect  "None.")
	   (value   "The binder with the scope, where all variables are uniquely renamed."))
  (let* ((old-vars (if (equal (car binder) :protect)
		       (cdr binder)
		     binder))
	 (new-vars (mapcar #'(lambda (x)
			       (etypecase x
				 (string (gensym x))
				 (symbol (gensym (symbol-name x)))))
			   old-vars))
	 (subst (pairlis old-vars new-vars)))
    (cons (cons :protect new-vars)
	  (sublis subst scope))))
    

(defun dflt=find-metafunc (func meta-funcs)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A function definition and a list of metafunction definitions.")
	   (effect  "None.")
	   (value   "T if func is in the meta-function list. O/w nil."))
  (when (listp func)
    (find (car func) meta-funcs :test #'(lambda (x y)
					  (string-equal x (car y))))))

(defun dflt=match-metafunc (func meta-funcs)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A function definition, a list of metafunction definitions, a goal argument"
		    "and a lsit of dependency arguments.")
	   (effect  "None.")
	   (value   "The rewriten function definition where the apropriate metafunction has been"
		    "substituted."))
  (let* ((meta-func (dflt=find-metafunc func meta-funcs))
	 (old-args (second meta-func))
	 (new-args (cdr func)))
    (sublis (pairlis old-args new-args) (caddr meta-func))))

(defun dflt=get-arguments (args a-list)
  (let ((new-alist (mapcar #'(lambda (x)
			       (assoc x a-list :test #'string-equal))
			   args)))
    (unless (some #'null new-alist)
      (mapcar #'cdr new-alist))))

(defun dflt=lookup-goal (goal a-list)
  (cdr (assoc goal a-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Parameter Blackboard....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dflt~blackboard-create (name command ordering level agents entries)
  (declare (edited  "14-APR-1998")
	   (authors Sorge)
	   (input   "A name, a command, a list of arguments of COMMAND, a number, a list of agents and"
		    "a list of blackboard entries.")
	   (effect  "Creates an instance of DFLT+BLACKBOARD.")
	   (value   "The newly created instance."))
  (make-instance 'dflt+blackboard
		 :name name
		 :bb-command command
		 :bb-ordering ordering
		 :bb-level level
		 :bb-agents agents
		 :bb-entries entries))


(defun dflt~bb-entry-create (mapping status visited)
  (declare (edited  "14-APR-1998")
	   (authors Sorge)
	   (input   "A association-list, a status indicator, and a list of agent-names.")
	   (effect  "Creates an instance of DFLT+BB-ENTRY.")
	   (value   "The newly created instance."))
  (make-instance 'dflt+bb-entry
		 :mapping mapping
		 :status status
		 :visited visited))
		 
(defun dflt~initialize-blackboard (command default &optional (level dflt*complexity-level))  
  (declare (edited  "14-APR-1998")
	   (authors Sorge)
	   (input   "A command, a default argument setting and a number.")
	   (effect  "A blackboard for this command is created and registered.")
	   (value   "The new blackboard if possible for the command."))
  (let ((name (keim~name command))
	(args (com~argnames command))
	(agents (remove-if-not #'(lambda (x) (<= (dflt~level x) level))
			   (dflt~find-agents command))))
    (when agents
      (let ((new-obj (dflt~blackboard-create name
					     command
					     args
					     level
					     agents
					     (list (dflt~bb-entry-create
						    (remove-if #'(lambda (x) (null (cdr x)))
							       (pairlis args default))
						    (notany #'null default)
						    nil)))))
	(setf (gethash (etypecase name
			(symbol (symbol-name name))
			(string (string-upcase name)))
		       dflt*blackboard-hash-table)
	      new-obj)
	new-obj))))

(defun dflt~survey-blackboard (blackboard &optional (proof-context foci*proof-context))
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "The function contains the functionality of the surveying agent."
		    "New blackboard-entries of type DFLT+BB-ENTRY might be created and the"
		    "corresponding slots updated.")
	   (value   "The updated blackboard."))
  (let* ((agents (dflt~bb-agents blackboard))
	 (agent-names (mapcar #'keim~name agents)))
    (flet ((update-visited (entries)
			   (mapc #'(lambda (x)
				     (setf (dflt~entry-visited x)
					   (union (dflt~entry-visited x) agent-names)))
				 entries)
			   ))
      (let ((entry-list
	     (do* ((entries (dflt~bb-entries blackboard) new-entries)
		   (new-entries (dflt=iterate-entries entries agents)
				(dflt=iterate-entries entries agents))
		   (all-entries (remove-duplicates (append entries new-entries)
						   :test #'(lambda (x y) (equalp (dflt~entry-mapping x)
										 (dflt~entry-mapping y))))
				(remove-duplicates (append all-entries new-entries)
						   :test #'(lambda (x y) (equalp (dflt~entry-mapping x)
										 (dflt~entry-mapping y))))))
		 ((null new-entries) (progn (update-visited entries)
					    all-entries))
	       (update-visited entries))))
	(terpri)
	(setf (dflt~bb-entries blackboard)
	      (dflt~sort-bb-entries entry-list proof-context))))))
				     
	  

(defun dflt=iterate-agents (agents bb-entry)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A list of agents and a bb-entry."))
  (when agents
    (append (dflt=update-bb-entry bb-entry (car agents))
	    (dflt=iterate-agents (cdr agents) bb-entry))))

(defun dflt=iterate-entries (bb-entries agents)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A list of blackboard entries and a list of agents."))
  (when bb-entries
    (append (dflt=iterate-agents agents (car bb-entries))
	    (dflt=iterate-entries (cdr bb-entries) agents))))
  
(defgeneric dflt=update-bb-entry (bb-entry agent)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A blackboard entry and an agent.")
	   (effect  "New blackboard entries are created.")
	   (value   "The newly created blackboard entries."))
  (:method ((bb-entry dflt+bb-entry) (agent dflt+function-agent))
	   (when (and (not (dflt~entry-status bb-entry))
		      (dflt~agent-applicable-p agent bb-entry))
	     (let ((new-mapping (dflt~apply-agent agent bb-entry)))
	       (if new-mapping
		   (list bb-entry
			 (dflt~bb-entry-create new-mapping
					       (= (length new-mapping)
						  (length (com~argnames (dflt~command agent))))
					       (list (keim~name agent))))
		 (list bb-entry)))))
  (:method ((bb-entry dflt+bb-entry) (agent dflt+predicate-agent))
	   (when (and (not (dflt~entry-status bb-entry))
		      (dflt~agent-applicable-p agent bb-entry))
	     (let ((new-mappings (dflt~apply-agent agent bb-entry)))
	       (cons bb-entry
		     (mapcar #'(lambda (mapping)
				 (dflt~bb-entry-create mapping
						       (= (length mapping)
							  (length (com~argnames (dflt~command agent))))
						       (list (keim~name agent))))
			     new-mappings))))))




;; ------ CAUTION! CONSTRUCTION AHEAD ------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here should go a nice little mechanism to dynamically specify
;; sorting criteria. Moreover information from the chronological
;; focus should be used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dflt~sort-bb-entries (bb-entries &optional (proof-context foci*proof-context))
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A list of blackboard entries.")
	   (effect  "The list is sorted with respect to some heuristics.")
	   (value   "The sorted list."))
  (let ((bbs (remove-duplicates
	      (copy-tree bb-entries)
	      :test  #'dflt=bb-entry-equal-p)))
     (stable-sort bbs
		  #'(lambda (x y)
		      (or (dflt~entry-status x)
			  (> (length (dflt~entry-mapping x))
			     (length (dflt~entry-mapping y))))))
    )) ;;; sorting criteria with respect to the chronological focus



(defgeneric dflt=bb-entry-equal-p (bbe1 bbe2)
  (:method ((bbe1 dflt+bb-entry) (bbe2 dflt+bb-entry))
	   (let ((map1 (dflt~entry-mapping bbe1))
		 (map2 (dflt~entry-mapping bbe2)))
	     (and (= (length map1) (length map2))
		  (every #'(lambda (x y)
			     (or (equal x y)
				 (keim~equal x y)))
			 map1 map2))))
  (:method ((map1 list) (map2 list))
	     (and (= (length map1) (length map2))
		  (every #'(lambda (x y)
			     (or (equal x y)
				 (keim~equal x y)))
			 map1 map2))))


;; ------ END OF CONSTRUCTION ------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The generic default function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| old-version with iterative default-computation....
(let ((command-defaults)
      (counter 0))

  (defmethod com~general-default-computation ((command com+command) (defaults list))
    (when (= counter 0)
      (let ((defaults (dflt~default-computation command defaults)))
	(setf command-defaults
	      (if defaults (car defaults)
		(make-list (length (com~argnames command)) :initial-element (com~unspecified))))))
    (if (= counter (length command-defaults))
	(setf counter 0)
      (incf counter))
    command-defaults)
  
  )
|#

(defmethod com~general-default-computation ((command com+command) (defaults list))
  (let ((agents (dflt~default-computation command defaults)))
    agents))
  
(defun dflt~default-computation (command defaults &optional (proof-context foci*proof-context))
  (declare (edited  "16-APR-1998")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "Starts the argument agents and completes all blackboard entries to"
		    "full parameter lists.")
	   (value   "A list of parameter lists."))
  (let ((blackboard (dflt~initialize-blackboard command defaults)))
    (if blackboard
	(progn 
	  (dflt~survey-blackboard blackboard proof-context)
;;;    (format t "~{~A~%~}" (dflt=complete-bb-entries (dflt~bb-entries blackboard) (com~argnames command)))
	  (dflt=complete-bb-entries (dflt~bb-entries blackboard) (com~argnames command)))
      (list (mapcar #'(lambda (argument)
			(if argument argument (com~unspecified)))
		    defaults)))))
      

(defun dflt=complete-bb-entries (bb-entries argnames)
  (declare (edited  "16-APR-1998")
	   (authors Sorge)
	   (input   "A list of bb-entries and a list of argnames.")
	   (value   "A list of parameter instantiations for the command."))
  (mapcar #'(lambda (bb-entry)
	      (let ((mapping (dflt~entry-mapping bb-entry)))
		(mapcar #'(lambda (argument)
			    (let ((map (assoc argument mapping :test #'string-equal)))
			      (if map (cdr map)
				(com~unspecified))))
			argnames)))
	  bb-entries))
	      

(defmethod print-object ((obj dflt+bb-entry) stream)
  (let ((mapping (apply #'append (mapcar #'(lambda (x)
					     (list (car x)
						   (if (pdsn~p (cdr x))
						       (format nil "~A: ~A"
							       (keim~name (cdr x))
							       (node~formula (cdr x)))
						     (cdr x))))
					 (dflt~entry-mapping obj)))))
    (format stream "<~{[~A: ~A]~}>" mapping)))


(defun dflt~all-agents ()
  (let ((agents))
    (maphash #'(lambda (x y)
		 (declare (ignore x))
		 (push y agents))
	     dflt*agent-hash-table)
    agents))



(defun dflt~output (&optional (status (not dflt*output)))
  (setf dflt*output status))
