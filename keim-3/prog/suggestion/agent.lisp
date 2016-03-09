;;
;; To Do: Removal functions for agents (important when debugging!)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic reimplementation of the suggestion mechanism
;; using concurrent lisp facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Agent Module
;;
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains everything necessary to work with agents.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :keim)


(mod~defmod AGENT 
            :uses (bb com foci heur keim let node pdsn proc rsrc sugg sys)
            :documentation "All that is needed for agents."
            :exports (agent+agent
                      agent+agent
                      agent+c-pred-agent
		      agent+c-ext-pred-agent
                      agent+classifier
                      agent+command-agent
		      agent+dummy-agent
                      agent+function-agent
                      agent+parameter-agent
                      agent+predicate-agent
                      agent+s-pred-agent
                      agent+p-pred-agent
                      agent+a-pred-agent
                      agent+suggestion-agent
                      agent+surveyor
		      agent+resource-agent
                      
                      agent~associate-agent-2-command
                      agent~blackboard
                      agent~c-pred-agent-p
                      agent~change-log-flag
                      agent~check-global-activation-level
                      agent~check-reset-state
                      agent~check-resources
                      agent~change-short-term-act-level
                      agent~classification-function
                      agent~classifier-p
                      agent~command
                      agent~command-agent-p
                      agent~command2command-agent
                      agent~command2parameter-agents
		      agent~completeness-sens-p
                      agent~create-classifier
                      agent~create-command-agent
		      agent~create-resource-agent
                      agent~create-suggestion-agent
                      agent~defagent
                      agent~defclassifier
                      agent~defmatrix
                      agent~dependence
                      agent~distribute-resources
                      agent~eval-fun
                      agent~exclude
                      agent~find-parameter-agent
                      agent~function
                      agent~function-agent-p
                      agent~get-agent
                      agent~get-agents
                      agent~get-classifier
                      agent~get-classifiers
                      agent~get-command-agents
                      agent~get-parameter-agent
                      agent~get-parameter-agents
                      agent~get-surveyor
                      agent~goal
                      agent~help
                      agent~information
                      agent~init-resources
                      agent~initialize
		      agent~level
                      agent~list-turned-off-agents
                      agent~make-command-agent
                      agent~multiple
                      agent~name
                      agent~old-information
                      agent~output-function
                      agent~p
                      agent~parameter-agent-p
                      agent~predicate
                      agent~predicate-agent-p
                      agent~process
		      agent~proc-id
                      agent~register-new-agent
                      agent~remove-agent
                      agent~remove-agents-of-command
                      agent~remove-all-agents
		      agent~reset-ext-provers
		      agent~reset-external-prover
		      agent~resets
                      agent~reset-resources
                      agent~reset-state
                      agent~resources
                      agent~s-pred-agent-p
                      agent~p-pred-agent-p
                      agent~a-pred-agent-p
                      agent~set-reset-state
                      agent~suggestion-agent-p
                      agent~surveyed-bb
                      agent~surveyor-p
                      agent~turn-off-subagents
                      agent~write-logs
                      agent~write-ls
                      agent~write-res-log

		      atptop~kill
                      atptop~get-pids-of-children
		      
                      agent*classifier-hash-table
                      agent*command2command-agent-hash-table
                      agent*command2parameter-agent-hash-table
		      agent*external-prover-agents
                      agent*parameter-agent-hash-table
                      agent*processes
                      agent*surveyor-hash-table))

;;; The following functions are internal in other modules and should not be used:
;;; (agent=actualize-res-slots agent=reset-bb-resources heur=save-div ohlp=make-directory)


#{
\section{Agents}
This module provides elementary functions for agents.
\subsection{Class hierarchy for the agents}

\begin{description}
\item[agent+agent] Superclass for all agents.
  \begin{description}
   \item[Name:] The name of the agent.
  \end{description}
  \begin{description}
  \item[Blackboard:] The blackboard the agent works for.
  \end{description}
  \begin{description}
  \item[Resources:] The resource information for the agent. E.g. a number or an object
    indicating the resources available for the agent.
  \end{description}
  \begin{description}
  \item[Information:] A slot for miscellanious information about the agent.
  \end{description}
  \begin{description}
  \item[Process:] The process running the agent.
  \end{description}
  \begin{description}
  \item[Reset:] Reset state of the agent. A number; can be compared with the global reset state.
  \end{description}
\end{description}

\begin{description}
\item[agent+classifier (agent+agent)] The class of classifying agents.
  \begin{description}
  \item[Name:]
  \end{description}
  \begin{description}
  \item[Classification-function:] The classification-function of the agent.
  \end{description}
  \begin{description}
  \item[Old-information:] A slot to store previous information supplied by the agent.
  \end{description}
\end{description}

\begin{description}
\item[agent+surveyor (agent+agent)] The class of surveyor-agents. These agents work for some blackboard by surveying another.
  \begin{description}
  \item[Surveyed-bb:] The blackboard the agent surveys.
  \end{description}
\end{description}

\begin{description}
\item[agent+command-agent (agent+surveyor)] The class agents surveying command blackboards.
  \begin{description}
    \item[Command:] The command the agent belongs to.
    \end{description}
\end{description}


\begin{description}
\item[agent+suggestion-agent (agent+surveyor)] The class of agents surveying suggestion blackboards.
  \begin{description}
  \item[Blackboard:] 
  \end{description}
  \begin{description}
  \item[Output-function:] The function specifying where the suggestion agent is presenting its output to.
  \end{description}
\end{description}

\begin{description}
\item[agent+parameter-agent (agent+agent)] The basic parameter agent class.
  \begin{description}
  \item[Command:] The command the argument agent belongs to.
  \end{description}
  \begin{description}
  \item[Goal:] The argument the agent is defined for (function agents can have multiple
    goals). Coresspondents with the for-slot.
  \end{description}
  \begin{description}
  \item[Dependence:] The additional arguments required for the agent.
  \end{description}
  \begin{description}
  \item[Exclude:] The parameters that should not yet be suggested.
  \end{description}
  \begin{description}
  \item[Multiple:] The goal parameters which get multiple suggestions computed within a single suggestion run.
  \end{description}
  \begin{description}
  \item[Level:] 
  \end{description}
\end{description}


\begin{description}
\item[agent+predicate-agent] The superclass of predicate agents.
  \begin{description}
  \item[Predicate:] The predicate the agent uses.
  \end{description}
\end{description}

\begin{description}
\item[agent+s-pred-agent (agent+predicate-agent)] Predicate agent for support lines.
\end{description}

\begin{description}
\item[agent+c-pred-agent (agent+predicate-agent)] Predicate agent for open lines.
\end{description}

\begin{description}
\item[agent+function-agent (agent+parameter-agent)] The superclass of function agents.
  \begin{description}
  \item[Function:] The function the agent uses.
  \end{description}
\end{description}
\end{description}



#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for agents 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)

  (defclass agent+agent ()
    ((name :accessor agent~name
	   :initarg :name
	   :initform ""
	   :documentation "The name of the agent.")
     (blackboard :accessor agent~blackboard
		:initarg :blackboard
		:initform nil
		:documentation "The blackboard the agent works for.")
     (resources :accessor agent~resources
		:initarg :resources
		:initform nil
		:documentation "The resource information for the agent.")
     (information :accessor agent~information
		  :initarg :information
		  :initform nil
		  :documentation "A slot for miscellanious information about the agent.")
     (process :accessor agent~process
	      :initarg :process
	      :initform nil
	      :documentation "The process running the agent.")
     (reset :accessor agent~reset-state
	    :initarg :reset
	    :initform nil
	    :documentation "Reset state of the agent.")
     (help :accessor agent~help
	   :initarg :help
	   :initform ""
	   :documentation "The help string of the agent."))
    (:documentation "The basic class of agents."))

  (defclass agent+classifier (agent+agent)
    ((classification-function :accessor agent~classification-function
			      :initarg :classification-function
			      :initform nil
			      :documentation "The classification-function of the agent.")
     (initial-duty-switch :accessor agent=initial-duty-switch
			      :initarg :initial-duty-switch
			      :initform nil
			      :documentation "A switch to store the execution of the initial duty.")
     (old-information :accessor agent~old-information
		      :initarg :old-information
		      :initform nil
		      :documentation "A slot to store previous information supplied by the agent."))
    (:documentation "The class of classifying agents."))
  
  (defclass agent+surveyor (agent+agent)
    ((surveyed-bb :accessor agent~surveyed-bb
		   :initarg :surveyed-bb
		   :initform nil
		   :documentation "The blackboard the agent surveys."))
    (:documentation "The class of surveyor-agents. These agents work for some blackboard by surveying another."))

  (defclass agent+command-agent (agent+surveyor)
    ((command :accessor agent~command
	      :initarg :command
	      :initform nil
	      :documentation "The command the command agent belongs to."))
    (:documentation "The class agents surveying command blackboards."))

  (defclass agent+suggestion-agent (agent+surveyor)
    ((blackboard :allocation :class
		 :initform nil)
     (output-function :accessor agent~output-function
		      :initarg :output-function
		      :initform #'(lambda (x) (format t "~A" x))
		      :documentation "The function specifying where the suggestion agent is presenting its output to."))
    (:documentation "The class of agents surveying suggestion blackboards."))

  (defclass agent+parameter-agent (agent+agent)
    ((command :accessor agent~command
	      :initarg :command
	      :initform nil
	      :documentation "The command the argument agent belongs to.")
     (goal :accessor agent~goal
	   :initarg :goal
	   :initform nil
	   :documentation "The argument the agent is defined for (function agents can have multiple goals).")
     (dependence :accessor agent~dependence
		 :initarg :dependence
		 :initform nil
		 :documentation "The additional arguments required for the agent.")
     (exclude :accessor agent~exclude
	      :initarg :exclude
	      :initform nil
	      :documentation "The parameters that should not yet be suggested.")
     (multiple :accessor agent~multiple
	       :initarg :multiple
	       :initform nil
	       :documentation "The goal parameters which get multiple suggestions computed within a single suggestion run.")
     (level :accessor agent~level
            :initarg :level
            :initform 1
            :documentation "The level of complexity of the agent."))
    (:documentation "The basic parameter agent class."))

  (defclass agent+predicate-agent (agent+parameter-agent)
    ((predicate :accessor agent~predicate
		:initarg :predicate
		:initform nil
		:documentation "The predicate the agent uses."))
    (:documentation "The superclass of predicate agents."))

  (defclass agent+pred-list-agent (agent+predicate-agent) ;;under construction
     ()
     (:documentation "The superclass of list-predicate agents."))

  (defclass agent+s-pred-list-agent (agent+pred-list-agent) ;;under construction
     ()
     (:documentation "Predicate agent for lists of support lines"))

  (defclass agent+c-pred-list-agent (agent+pred-list-agent) ;;under construction
    ()
    (:documentation "Predicate agent for lists of conclusion lines"))
  
  (defclass agent+s-pred-agent (agent+predicate-agent)
    ()
    (:documentation "Predicate agent for support lines."))

  (defclass agent+p-pred-agent (agent+predicate-agent)
    ()
    (:documentation "Predicate agent for all premise (closed) lines."))

  (defclass agent+a-pred-agent (agent+predicate-agent)
    ()
    (:documentation "Predicate agent for all lines."))
  
  (defclass agent+c-pred-agent (agent+predicate-agent)
    ()
    (:documentation "Predicate agent for open lines."))

  (defclass agent+c-ext-pred-agent (agent+c-pred-agent)
    ((proc-id :accessor agent~proc-id
	      :initarg :proc-id
	      :initform NIL
	      :documentation "The pid from the process for the ext. Prover"))
    (:documentation "Predicate agent for open lines, which should be tackled by ext. Prover."))

  ;; this agent is only to keep the concourrency running MH
  (defclass agent+dummy-agent (agent+classifier)
    ((dummy-flag :accessor agent~dummy-flag
		 :initform NIL)))

  (defclass agent+function-agent (agent+parameter-agent)
    ((function :accessor agent~function
	       :initarg :function
	       :initform nil
	       :documentation "The function the agent uses."))
    (:documentation "The superclass of function agents."))

  (defclass agent+resource-agent (agent+surveyor)
    ((last-execution-interval :accessor agent~last-exec-time
			      :initarg last-exec-time
			      :initform 0
			      :documentation "The time between last two resets in ms.")
     (last-time-stamp :accessor agent~last-time-stamp
		      :initarg :last-time-stamp
		      :initform 0
		      :documentation "System time at last reset.")
     (total-resets :accessor agent~resets
		   :initarg :total-resets
		   :initform 0
		   :documentation "Indicating how often agent was resetted.")
     (resets-since-last-update :accessor agent~resets-since-last-update
			       :initarg :resets-since-last-update
			       :initform 0
			       :documentation "Number of resets since last update of
global activation level.")
     (time-since-last-update :accessor agent~time-since-last-update
			     :initarg :time-since-last-update
			     :initform 0
			     :documentation "Time since last update of global act. level.")
     (average-n-int :accessor agent~average-n-int
		    :initarg average-n-int
		    :initform 0
		    :documentation "The average time between two command executions over the
last n runs.")
     (average-prev-n-int :accessor agent~average-prev-n-int
			 :initarg average-prev-n-int
			 :initform 0
			 :documentation "The average time between two command executions over the
previous n runs.")))

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors for agents 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun agent~create-classifier (name blackboard resources information old-information class-func help)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "Slot values for classifier agents.")
	   (effect  "Creates an object of type AGENT+CLASSIFIER.")
	   (value   "The newly created object."))
  (make-instance 'agent+classifier
		 :name name
		 :blackboard blackboard
		 :resources resources
		 :information information
		 :old-information old-information
		 :classification-function class-func
		 :help help))

(defun agent~create-command-agent (name command blackboard resources information surveyed-bb help)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "Slot values for command agents.")
	   (effect  "Creates an object of type AGENT+COMMAND-AGENT.")
	   (value   "The newly created object."))
  (make-instance 'agent+command-agent
		 :name name
		 :command command
		 :blackboard blackboard
		 :resources resources
		 :information information
		 :surveyed-bb surveyed-bb
		 :help help))

(defgeneric agent~make-command-agent (command)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "A command agent and its associated blackboard are created.")
	   (value   "The newly created command agent."))
  (:method ((command string))
	   (let ((com (bb~find-command command)))
	     (if com
		 (agent~make-command-agent com)
	       (sugg~output nil 0 "Command ~A does not exist." command))))	     
  (:method ((command symbol))
	   (agent~make-command-agent (symbol-name command))))

(defun agent~create-suggestion-agent (name surveyed-bb resources information output-function help)
  (declare (edited  "20-NOV-1999")
           (authors Sorge)
           (input   "Slot values for suggestion agents.")
           (effect  "Creates an object of type AGENT+SUGGESTION-AGENT.")
           (value   "The newly created object."))
  (make-instance 'agent+suggestion-agent
                 :name name
                 :surveyed-bb surveyed-bb
                 :resources resources
                 :information information
                 :output-function output-function
                 :help help))

(defun agent~create-resource-agent (name)
  (make-instance 'agent+resource-agent
		 :name name
		 :last-time-stamp (rsrc~get-time-stamp)))

;;; Parameter Agents
;;; There are two macros to specify and create parameter agents.

(defmacro agent~defagent (command specification &rest attribs) 
  (declare (edited  "08-APR-1998")
	   (authors Sorge)
	   (input   "An agent for default computation. It is either an argument-FUNCTION or"
		    "-Support or -Conclusion-PREDicate."
		    "It has to refer to both a valid command and a list of command-parameters, syntactically"
		    "equal to those defined in the command. Thus an agent can only be created after the"
		    "corresponding command has been defined."
		    "Example:
 \\begin{code}
 (agent~defagent andi
   s-predicate
   (for LConj)
   (uses Conjunction)
   (exclude )
   (multiple )
   (definition (and (logic~conjunction-p Conjunction)
                    (data~equal LConj (first (data~appl-arguments Conjunction)))))
   (level 1)
   (help \"Predicate for the left conjunct of a given conjunction.\"))
 \\end{code}")
	   (effect  "Defines an agent using this specification.")
	   (value   "Unspecified."))
  `(let* ((command (bb~find-command ',command))
	  (spec ',specification))
     (cond ((and command (find spec '(function c-predicate s-predicate p-predicate a-predicate c-pred-list s-pred-list c-ext-pred)
			  :test #'string-equal)) ;; modified for pred-list-agents
	    (sugg~output nil 2 "Defining 1 default agents for command ~A." command)
	    (agent=defagent command (cons spec ',attribs)))
	   (command (sugg~output nil 0 "Agent-definition: Unexpected agent type ~A." spec))
	   (t (sugg~output nil 0 "Agent-definition: The command ~A does not exist." ',command)))))
     
(defun agent=defagent (command agent &optional (meta-functions nil))
  (let ((spec (car agent))
	(attribs (cdr agent))
	(level 1) (help "") 
	goal depend exclude definition information multiple)
    (unless (find spec '(function c-predicate s-predicate p-predicate a-predicate c-ext-pred c-pred-list s-pred-list) :test #'string-equal)
    ;; modified for pred-list-agents  
      (return-from agent=defagent (sugg~output nil 0 "Agent-definition: Unexpected agent type ~A." spec)))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (cond 
       ((not (consp attrib)) (sugg~output nil 0 "Agent-definition: Not expecting ~A." attrib))
       ((string-equal (car attrib) :uses)        (setq depend      (cdr  attrib)))
       ((string-equal (car attrib) :for)         (setq goal        (cdr  attrib)))
       ((string-equal (car attrib) :exclude)     (setq exclude     (cdr  attrib)))
       ((string-equal (car attrib) :multiple)    (setq multiple    (cdr  attrib)))
       ((string-equal (car attrib) :level)       (setq level       (cadr attrib)))
       ((string-equal (car attrib) :definition)  (setq definition  (cadr attrib)))
       ((string-equal (car attrib) :information) (setq information (append information (cdr  attrib))))
       ((string-equal (car attrib) :help)        (setq help (cadr attrib)))
       (t (sugg~output nil 0 "Agent-definition: Not expecting ~A." (car attrib)))))
    (when (agent=test-arguments (append goal depend) command)
      ;;;; MALTE
      (let ((real-def (agent=build-function&predicate definition goal depend meta-functions))
	    (agenten-name  (agent=unique-parameter-agent-name
			    (keim~name command) goal depend exclude)))
        (if real-def
            (let ((new-agent (agent=make-agent spec
                                              real-def
                                              :name agenten-name
					      :command command
					      :resources (make-instance 'rsrc+param-agent-resource :name agenten-name)
					      ;;;; MALTE : Ist das hier an der richtigen Stelle ?
					      :goal goal
					      :dependence depend
                                              :exclude exclude
                                              :multiple multiple
                                              :help help
                                              :information information
                                              :level (if (numberp level) level 1))))
              (agent~register-new-agent new-agent)
	      (rsrc~add-data (bb~resources (agent~blackboard new-agent)) (agent~resources new-agent))
              (sugg~output nil 3 "Parameter agent ~A of type ~A created." (agent~name new-agent) spec)
              new-agent)
          (sugg~output nil 0 "Agent-definition: The following things could not be referenced to: ~A." real-def)))))) 
                                                                  ;;; hier noch Fehlermeldung genauer...
(defun agent=make-agent (spec def &rest slots)
  (let ((class (cond ((string-equal spec :function)
		      (list (quote agent+function-agent) :function def))
		     ((string-equal spec :s-predicate)
		      (list (quote agent+s-pred-agent) :predicate def))
		     ((string-equal spec :p-predicate)
		      (list (quote agent+p-pred-agent) :predicate def))
		     ((string-equal spec :a-predicate)
		      (list (quote agent+a-pred-agent) :predicate def))
		     ((string-equal spec :c-ext-pred)
		      (list (quote agent+c-ext-pred-agent) :predicate def))
		     ((string-equal spec :c-predicate)
		      (list (quote agent+c-pred-agent) :predicate def))
		     ((string-equal spec :c-pred-list) ;; Achim
		      (list (quote agent+c-pred-list-agent) :predicate def))
		     ((string-equal spec :s-pred-list) ;; Achim
		      (list (quote agent+s-pred-list-agent) :predicate def))
	       )
	))
    (apply #'make-instance (append class slots))))

(defmacro agent~defmatrix (command &rest attribs) 
  (declare (edited  "08-APR-1998")
	   (authors Sorge)
	   (input   "A matrix specifying a set of agents for default computation."
		    "Example:
 \\begin{code}
  (agent~defmatrix andi
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
	   (value   "Unspecified.")
	   (remark  "The syntax of predicates and functions is restricted to relatively simple"
		    "lisp expressions. Functions and predicates can be used recursively in order"
		    "of declarations (i.e. pred2 could call pred1 but not vice versa)."
		    "The arguments are always considered to be formulas of the input nodes unless"
		    "stated otherwise with the help of the keywords: :node, :just, :hyps, :param."
		    "The keyword :param simply indicates that the parameter is passed on without"
		    "any modification."
		    ""
		    "The meta-predicates and -functions have to be of the form:"
		    "(NAME (Arg1... Argn) BODY)"
		    "The body has to be a single sexpression. This can always be ensured by enveloping"
		    "the body in a progn form."
		    ""
		    "Besides for recursive calls to defined predicates we allow for calls to simple"
		    "Lisp functions, i.e. functions that have the syntax: (Func Arg1 ... Argn)"
		    "Moreover, one can use lambda abstractions and let, respectively let*, environments."
		    "Note, that the initial parameters of the predicates should not be redefined in"
		    "these environments, neither should be the goal and dependencty parameters of the"
		    "agents."
		    ""
		    "Conventions for results of agent's predicates and functions:"
		    "\\begin{enumerate}"
		    "\\item only one parameter-name given in the for slot (for a1):\\newline"
		    "For predicate agents any non-nil value will indicate success of the predicate for"
		    "the particular node which is subsequently returned as instantiation for a1\\newline"
		    "For functions the returned value is directly instantiated for a1."
		    "\\item several parameter-names are given (for a1 a2 $\\ldots$ an) :\\newline"
		    "For predicate agents either nil or a list of return values is expected."
		    "If the result is non-nil the particular node will be instantiated for a1 and"
		    "the single elements of the returned list are sequentially bound to a2, $\\ldots$, an."
		    "For funciton agents we expect n values returned with multiple-values, where each value"
		    "is bound to one of the ai."
		    "\\item several parameter-names plus multiple parameters are given (for a1 a2 $\\ldots$ an)"
		    "(multiple ai aj):\\newline"
		    "The return values are expected to be as in the preceding point, however for each argument"
		    "indicated to be of multiple values, a list of single values is expected. For each of the single"
		    "values a cartesian product of partial argument instantiations is constructed."
		    "\\end{enumerate}"))
  `(let* ((command (bb~find-command ',command))
	  (attribs ',attribs))
     (if command
	 (let (agents functions predicates information)
	   (do ((attribs (cdr attribs) (cdr attribs))
		(attrib (car attribs) (car attribs)))
	       ((and (null attrib) (null attribs)))
	     (cond
	      ((not (consp attrib)) (sugg~output nil 0 "Agent-definition-matrix: Not expecting ~A." attrib))
	      ((string-equal (car attrib) :agents)     (setq agents      (cdr attrib)))
	      ((string-equal (car attrib) :functions)  (setq functions   (cdr attrib)))
	      ((string-equal (car attrib) :predicates) (setq predicates  (cdr attrib)))
	      ((string-equal (car attrib) :information)(setq information attrib))
	      (t (sugg~output nil 0 "Agent-definition-matrix: Not expecting ~A." (car attrib)))))
	   (sugg~output nil 2 "Defining ~A default agents for command ~A." (length agents) command)
	   (mapcar #'(lambda (agent)
		       (agent=defagent command (append agent (list information)) (append functions predicates)))
		   agents))
       (sugg~output nil 0 "Agent-definition-matrix: The command ~A does not exist." ',command))))


;;; Classifier: there is one function for specifying classifying agents

(defmacro agent~defclassifier (name &rest attribs)
  (declare (edited  "03-DEC-1999")
	   (authors Sorge)
	   (input   "A classifier agent. It can have the following syntax:
 \\begin{code}
 (agent~defclassifier fo
   (function #'(lambda (x) (logic~fo-formula-p (node~formula x))))
   (information fo no-ho pl0)
   (help \"Classifies whether a goal is first order.\"))
 \\end{code}"
		    "The information slot can have the following values:"
		    "NIL - when predicate is T, the classifier adds its own name to the"
		    "      information slot of the blackboard."
		    "A list of strings or symbols - the list is added as information."
		    "T - the result of the predicate is added to the information slot.")
	   (effect  "Creates a classifier agent and registers it.")
	   (value   "The newly created agent."))
  `(let* ((attribs ',attribs)
	  (name ',name)
	  function help information)
     (do ((attribs (cdr attribs) (cdr attribs))
	  (attrib (car attribs) (car attribs)))
	 ((and (null attrib) (null attribs)))
       (cond
	((not (consp attrib)) (sugg~output nil 0 "Agent-definition-matrix: Not expecting ~A." attrib))
	((string-equal (car attrib) :function)   (setq function    (cadr attrib)))
	((string-equal (car attrib) :help)       (setq help        (cadr attrib)))
	((string-equal (car attrib) :information)(setq information (cdr attrib)))
	(t (sugg~output nil 0 "Agent-definition-classifier: Not expecting ~A." (car attrib)))))
     (let* ((real-name (etypecase name
			(string (string-upcase name))
			(symbol (symbol-name name))))
	    (info (if (and (length information 1) (eq (car information) t))
		      (car information)
		    information))
	    (new-agent (agent~create-classifier real-name nil nil info nil function help)))
       (agent~register-new-agent new-agent)
       (sugg~output nil 2 "Defining classifier agent ~A." real-name)
       new-agent)))

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates for agents 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun agent~p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+AGENT."))
  (typep obj 'agent+agent))

(defun agent~classifier-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+CLASSIFIER."))
  (typep obj 'agent+classifier))

(defun agent~dummy-agent-p (obj)
  (typep obj 'agent+dummy-agent))

(defun agent~surveyor-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+SURVEYOR."))
  (typep obj 'agent+agent))

(defun agent~command-agent-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+COMMAND-AGENT."))
  (typep obj 'agent+agent))

(defun agent~suggestion-agent-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+SUGGESTION-AGENT."))
  (typep obj 'agent+agent))

(defun agent~parameter-agent-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+PARAMETER-AGENT."))
  (typep obj 'agent+agent))

(defun agent~function-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+FUNCTION-AGENT."))
  (typep obj 'agent+function-agent))

(defun agent~predicate-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+PREDICATE-AGENT."))
  (typep obj 'agent+predicate-agent))

(defun agent~s-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+S-PRED-AGENT."))
  (typep obj 'agent+s-pred-agent))

(defun agent~p-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+S-PRED-AGENT."))
  (typep obj 'agent+p-pred-agent))

(defun agent~a-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+S-PRED-AGENT."))
  (typep obj 'agent+a-pred-agent))

(defun agent~c-pred-agent-p (obj)
  (declare (edited  "15-APR-1998")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type AGENT+C-PRED-AGENT."))
  (typep obj 'agent+c-pred-agent))

(defun agent~pred-list-agent-p (obj)
  (declare (edited "07-FEB-2001")
	   (authors Achim)
	   (input "A lisp object")
	   (value "T if the object is of type AGENT+PRED-LIST-AGENT."))
  (typep obj 'agent+pred-list-agent))

(defun agent~c-pred-list-agent-p (obj)
  (declare (edited "07-FEB-2001")
	   (authors Achim)
	   (input "A lisp object")
	   (value "T if the object is of type AGENT+C-PRED-LIST-AGENT."))
  (typep obj 'agent+c-pred-list-agent))

(defun agent~s-pred-list-agent-p (obj)
  (declare (edited "07-FEB-2001")
	   (authors Achim)
	   (input "A lisp object")
	   (value "T if the object is of type AGENT+S-PRED-LIST-AGENT."))
  (typep obj 'agent+s-pred-list-agent))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General hash-tables to store agents by types
(defvar agent*classifier-hash-table (make-hash-table :test #'equal))
(defvar agent*surveyor-hash-table (make-hash-table :test #'equal))
(defvar agent*parameter-agent-hash-table (make-hash-table :test #'equal))

;;; Hash-tables to relate commands to relevant agents
(defvar agent*command2parameter-agent-hash-table (make-hash-table :test #'equal))
(defvar agent*command2command-agent-hash-table (make-hash-table :test #'equal))

;;; Miscellaneous 
(defvar agent*processes nil "A list with all active processes of agents")

;;; Handling the external provers
(defvar agent*external-prover-agents nil
  "A list with all agents running for an external prover.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; getting all agents of some type

(defun agent~get-agents ()
  (declare (edited  "07-JAN-2000")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "All currently available agents."))
  (append (sugg~hash2list agent*surveyor-hash-table)
	  (agent~get-classifiers)
	  (agent~get-parameter-agents)))

(defun agent~get-parameter-agents ()
  (declare (edited  "22-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "All currently available parameter agents."))
  (sugg~hash2list agent*parameter-agent-hash-table))
	     
(defun agent~get-classifiers ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "All currently available classifier agents."))
  (sugg~hash2list agent*classifier-hash-table))
	     
(defun agent~get-command-agents ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "All currently available command agents."))
  (sugg~hash2list agent*command2command-agent-hash-table))
	     

;;; getting certain agents by name

(defgeneric agent~get-agent (agent-name)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "The name of an agent.")
	   (value   "The corresponding agent, if it exists."))
  (:method ((agent-name string))
	   (let ((name (string-upcase agent-name)))
	     (or (gethash name agent*parameter-agent-hash-table)
		 (gethash name agent*classifier-hash-table)
		 (gethash name agent*surveyor-hash-table))))
  (:method ((agent-name symbol))
	   (agent~get-agent (symbol-name agent-name))))

(defgeneric agent~get-parameter-agent (agent-name)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "The name of an agent.")
	   (value   "The corresponding parameter agent, if it exists."))
  (:method ((agent-name string))
	   (gethash (string-upcase agent-name) agent*parameter-agent-hash-table))
  (:method ((agent-name symbol))
	   (gethash (symbol-name agent-name) agent*parameter-agent-hash-table)))

(defgeneric agent~get-classifier (agent-name)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "The name of an agent.")
	   (value   "The corresponding classifier agent, if it exists."))
  (:method ((agent-name string))
	   (gethash (string-upcase agent-name) agent*classifier-hash-table))
  (:method ((agent-name symbol))
	   (gethash (symbol-name agent-name) agent*classifier-hash-table)))

(defgeneric agent~get-surveyor (agent-name)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "The name of an agent.")
	   (value   "The corresponding surveyor agent, if it exists."))
  (:method ((agent-name string))
	   (gethash (string-upcase agent-name) agent*surveyor-hash-table))
  (:method ((agent-name symbol))
	   (gethash (symbol-name agent-name) agent*surveyor-hash-table)))

;;; getting agents by specification

(defgeneric agent~find-parameter-agent (command goal depend &optional exclude)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "A command or its name, and two symbols.")
	   (effect  "None.")
	   (value   "The argument agent belonging to the specification if it exists."))
  (:method (command (goal cons) depend &optional exclude)
	   (gethash
	    (agent=unique-parameter-agent-name command goal depend exclude)
	    agent*parameter-agent-hash-table))
  (:method (command goal depend &optional exclude)
	   (gethash
	    (agent=unique-parameter-agent-name command (list goal) depend exclude)
	    agent*parameter-agent-hash-table)))
	   
(defgeneric agent~command2parameter-agents (command)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "A command.")
	   (effect  "None.")
	   (value   "All parameter agents associated with the command."))
  (:method ((command string))
	   (mapcar #'cdr (gethash (string-upcase command) agent*command2parameter-agent-hash-table)))
  (:method ((command symbol))
	   (mapcar #'cdr (gethash (symbol-name command) agent*command2parameter-agent-hash-table))))

(defgeneric agent~command2command-agent (command)
  (declare (edited  "19-NOV-1999" "13-APR-1998")
	   (authors Sorge Sorge)
	   (input   "A command.")
	   (effect  "None.")
	   (value   "The command agent associated with the command."))
  (:method ((command string))
	   (gethash (string-upcase command) agent*command2command-agent-hash-table))
  (:method ((command symbol))
	   (gethash (symbol-name command) agent*command2command-agent-hash-table)))

;;; inserting agents into appropriate hash-tables

(defgeneric agent~register-new-agent (agent)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "The agent is registered in the appropriate hash-table.")
	   (value   "Undefined."))
  (:method :before ((agent agent+command-agent))
	   (let ((command (agent~command agent)))
	     (when command (agent~associate-agent-2-command agent command))))
  (:method :before ((agent agent+parameter-agent))
	   (let ((command (agent~command agent)))
	     (when command
	       (let ((command-agent (agent~command2command-agent command)))
		 (if command-agent
		     (let* ((bb (agent~surveyed-bb command-agent)))
		       (bb~enter-agent bb agent)
		       (setf (agent~blackboard agent) bb)
		       (agent~associate-agent-2-command agent command))
		   (let* ((com-agent (agent~make-command-agent command))
			  (bb (agent~surveyed-bb com-agent)))
		     (bb~enter-agent bb agent)
		     (setf (agent~blackboard agent) bb)
		     (agent~associate-agent-2-command agent command)))))))
  (:method :before ((agent agent+suggestion-agent))
	   (let* ((name (agent~name agent))
		  (new-name (etypecase name
			      (symbol (symbol-name name))
			      (string (string-upcase name)))))
	     (setf (agent~name agent) new-name)))
  (:method ((agent agent+surveyor))
	   (setf (gethash (agent~name agent) agent*surveyor-hash-table) agent))
  (:method ((agent agent+parameter-agent))
	   (setf (gethash (agent~name agent) agent*parameter-agent-hash-table) agent))
  (:method ((agent agent+classifier))
	   (setf (gethash (agent~name agent) agent*classifier-hash-table) agent)))
	   
(defgeneric agent~associate-agent-2-command (agent command)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "An agent and a command.")
	   (effect  "Enters the agent in a hash-table that relates it to its command.")
	   (value   "Undefined."))
  (:method (agent (command symbol))
	   (agent~associate-agent-2-command agent (symbol-name command)))
  (:method ((agent agent+agent) (command string))
	   (error ";;;AGENT~~ASSOCIATE-AGENT-2-COMMAND: The agent ~A cannot be associated with a command." agent))
  (:method ((agent agent+parameter-agent) (command string))
	   (let* ((com-name (string-upcase command))
		  (agent-name (agent~name agent))
		  (agent-list (gethash com-name agent*command2parameter-agent-hash-table)))
	     (if (assoc agent-name agent-list :test #'string-equal)
		 (rplacd (assoc agent-name agent-list :test #'string-equal) agent)
	       (setf (gethash com-name agent*command2parameter-agent-hash-table)
		     (acons agent-name agent agent-list)))))
  (:method ((agent agent+command-agent) (command string))
	   (setf (gethash (string-upcase command) agent*command2command-agent-hash-table) agent)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removal of agents 
;;
;; When agents need to be removed (for instance for debugging
;; purposes), they need to be CLEANLY removed. That is, they have
;; to be removed from all the hashtables and blackboards they are
;; enregistered as well as from the dependency of other agents.
;;
;; To be done: clear processes when an agent is removed!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric agent~remove-agent (agent)
  (declare (edited  "27-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "The agent is removed from the ")
	   (value   "T if successful, o/w NIL."))
  (:method ((agent agent+surveyor))
	   (bb~remove-agent (agent~surveyed-bb agent) agent)
	   (when (agent=remove-from-hash-table (agent~name agent) agent*surveyor-hash-table)
	     (sugg~output agent 2 "Removed!")
	     t))
  (:method :before ((agent agent+command-agent))
	   (bb~remove-agent (agent~blackboard agent) agent)
	   (agent=remove-from-hash-table (agent~command agent) agent*command2command-agent-hash-table))
  (:method ((agent agent+classifier))
	   (bb~remove-agent (agent~blackboard agent) agent)
	   (when (agent=remove-from-hash-table (agent~name agent) agent*classifier-hash-table)
	     (sugg~output agent 2 "Removed!")
	     t))
  (:method ((agent agent+parameter-agent))
	   (bb~remove-agent (agent~blackboard agent) agent)
	   (when (agent=remove-from-hash-table (agent~name agent) agent*parameter-agent-hash-table)
	     (agent=remove-parameter-agent agent)
	     (sugg~output agent 2 "Removed!")
	     t))
  (:method ((agent string))
	   (let ((real-agent (agent~get-agent agent)))
	     (when real-agent (agent~remove-agent real-agent))))
  (:method ((agent symbol))
	   (let ((real-agent (agent~get-agent agent)))
	     (when real-agent (agent~remove-agent real-agent))))
  )

(defun agent~remove-agents-of-command (command)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "Removes all agents related to the command from the mechanism.")
	   (value   "T if successful, o/w nil."))
  (let ((pagents (agent~command2parameter-agents command))
	(cagent (agent~command2command-agent command)))
    (dolist (pagent pagents)
      (agent~remove-agent pagent))
    (agent~remove-agent cagent)))

(defun agent~remove-all-agents ()
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Removes all agents from the mechanism.")
	   (value   "Undefined."))
  (let ((surveyor-list (sugg~hash2list agent*surveyor-hash-table)))
    (dolist (surveyor surveyor-list)
      (when (agent~surveyed-bb surveyor)
	;; exclude resource agent
	(bb~remove-all-agents (agent~surveyed-bb surveyor))))
    (clrhash agent*surveyor-hash-table)
    (sugg~output nil 2 "Surveyor hash-table cleared.")
    (clrhash agent*parameter-agent-hash-table)
    (sugg~output nil 2 "Parameter agent hash-table cleared.")
    (clrhash agent*classifier-hash-table)
    (sugg~output nil 2 "Classifier hash-table cleared.")
    (clrhash agent*command2command-agent-hash-table)
    (sugg~output nil 2 "Association table for command agents cleared.")
    (clrhash agent*command2parameter-agent-hash-table)
    (sugg~output nil 2 "Association table for parameter agents cleared.")
    (values)))


(defgeneric agent=remove-from-hash-table (key table)
  (declare (edited  "27-NOV-1999")
	   (authors Sorge)
	   (input   "A key and hash-table.")
	   (effect  "Removes the entry with KEY from the hash-table.")
	   (value   "T if successful, o/w NIL."))
  (:method ((key string) (table hash-table))
	   (remhash (string-upcase key) table))
  (:method ((key symbol) (table hash-table))
	   (remhash (symbol-name key) table))
  (:method (key (table hash-table))
	   (remhash key table))
  (:method (key table)
	   (declare (ignore key))
	   (error "~A should be a hash-table!" table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maintaining the reset state of agents
;;
;; Agents maintain their own reset state with respect to the reset
;; state of the whole suggestion mechanism. That way we can query
;; from outside, whether an agent has already been reset in a given
;; run state of the mechanism or not.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun agent~set-reset-state (agent)
  (declare (edited  "07-JAN-2000")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "Sets the value of the agent's reset-state to SUGG*RESET.")
	   (value   "The new values of reset-state."))
  (setf (agent~reset-state agent) sugg*reset))



(defun agent~check-reset-state (agent)
  (declare (edited  "07-JAN-2000")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "None.")
	   (value   "T if the agent's reset-state is the same as that of the overall system."))
  (let ((reset-state (agent~reset-state agent)))
    (and (numberp reset-state)
	 (= reset-state sugg*reset))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric agent~initialize (agent)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "The agent is initialized and a new process is started to run it.")
	   (value   "The new process."))
  (:method (agent)
	   (sugg~output t 0 "Agent-initialization: ~A is not an agent." agent))
  (:method :before ((agent agent+classifier))
	   (unless (functionp (agent~classification-function agent))
	     (agent=compile-function agent)))
  (:method :before ((agent agent+predicate-agent))
	   (unless (functionp (agent~predicate agent))
	     (agent=compile-function agent)))
  (:method :before ((agent agent+function-agent))
	   (unless (functionp (agent~function agent))
	     (agent=compile-function agent)))
  (:method ((agent agent+agent))
	   (let ((proc (proc~create :name (agent~name agent)
			:function #'agent=run-agent
			:args ( agent))))
	     (sugg~output agent 2 "Initialized.")
	     (setf (agent~process agent) proc)
	     (agent=register-process proc)
	     proc))
  (:method :after ((agent agent+classifier))
	   (setf  (agent=initial-duty-switch agent) t)
; To set some existing slot containing a property list to something that is
; neither a property list nor a list at all is not a good idea. This is the
; where the scheduler problems with 'attempted to take the cdr of T' came from.	   
; I added a slot to agent+classifier and changed all places. MP	   
;	   (setf (proc~property-list (agent~process agent)) t)
	   (agent~process agent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent run functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro agent=loop (&key wait wait-msg reset reset-msg body)
  `(progn
     (agent~set-reset-state agent)
     (loop
      (sys~handler-case                                 ;;; this is ags specific  VS
       (progn                                           ;;; could go a regular handler-case
	 (sugg~output agent 5 "Waiting.")
	 (proc~wait ,wait-msg
		    #'(lambda () (or (not (agent~check-reset-state agent))
				     sugg*quit
				     ,wait)))
	 (cond (sugg*quit
		(agent=un-register-process agent)
		(return (sugg~output agent 2 "Quitting job!")))
	       ((not (agent~check-reset-state agent))
		(sugg~output agent 5 "Recommencing work.")
		,reset
		,reset-msg
		(agent~set-reset-state agent))
	       (t (sugg~output agent 5 "Recommencing work.")
		  ,body)))
       (simple-error (c) (return (sugg~output agent 0 "An interrupt has occured:~%        ~A" c)))
       (error (c) (return (sugg~output agent 0 "An interrupt has occured:~%        ~A " c)))))
     (agent=un-register-process agent)))


(defgeneric agent=print-if-ext (agent text)
  (:method ((agent agent+c-ext-pred-agent) text)
	   (sugg~output agent 2 text))
  (:method (agent text)
	   NIL))

(defmacro agent=reset-with-resources ()
  `(unless (agent~check-reset-state agent)
     (when rsrc*use-resources
       (progn (agent~set-reset-state agent)
	      (setf (rsrc~serious-reset (agent~resources agent)) 1)
	      ;; reset all data for interaction interval
	      ;; DAS IST HIER DOCH BLOEDSINN !!
	      ;; (rsrc~reset-activation-level)
	      ;; at this point there has been an reset occured: incrementing the reset-counter of
	      ;; the resource object MALTE
	      (agent=print-if-ext agent "Resetting with resources.")
	      (return (progn (sugg~output agent 5 "Reseting.")
			     (agent~eval-fun agent)))))))  ;; <---- evaluate performance
			    

(defgeneric agent=evaluate-resources (agent)
  (:method ((agent agent+command-agent))
	   (agent~eval-fun agent)
	   ;(agent=print-res-info agent)
	   (sugg~output agent 3 "Evaluating performance."))
  (:method ((agent agent+resource-agent))
	   (agent~eval-fun agent)
	   (sugg~output agent 3 "Evaluating performance."))
  (:method ((agent agent+suggestion-agent))
	   (agent~eval-fun agent)
	   ;(agent=print-res-info agent)
	   (sugg~output agent 3 "Evaluating performance."))
  (:method ((agent agent+c-ext-pred-agent))
	   (agent~eval-fun agent)
	   ;(agent=print-res-info agent)
	   (sugg~output agent 2 "Evaluating performance."))
  (:method ((agent agent+parameter-agent))
	   (agent~eval-fun agent)
	   ;(agent=print-res-info agent)
	   (sugg~output agent 3 "Evaluating performance.")))
 
  ;;VS: here will go the resource evaluation  <---- to be done.

(defun agent=check-interaction-interval-dummy (agent)
  (sugg~output agent 2 "Checking interaction interval ...")
  NIL)

(defgeneric agent=run-agent (agent)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "This is the run function that will keep an agent and its process alive until it quits.")
	   (value   "Undefined."))
  (:method ((agent agent+classifier))
	   (agent=loop
	    :wait (agent=initial-duty-switch agent)
; see above	    
;	    :wait (proc~property-list (agent~process agent))
	    :wait-msg "Waiting for reset."
	    :reset (agent=apply-classifier agent)
	    :reset-msg "Checking active goal."
	    :body (agent=classifier-initial-duty agent)))
  (:method ((agent agent+dummy-agent))
	    (agent=loop
	    :wait rsrc*use-resources
	    :body (if (agent~dummy-flag agent) (setf (agent~dummy-flag agent) NIL)
		    (setf (agent~dummy-flag agent) T))))
  (:method ((agent agent+resource-agent))
	   (agent=loop
	    :wait (when rsrc*use-resources (agent=check-interaction-interval agent))
	    :reset (agent=evaluate-resources agent)
	    :reset-msg "Resetting Resource agent."
	    :body (agent~change-short-term-act-level agent)))
  (:method ((agent agent+command-agent))
           (let* ((com-bb (agent~surveyed-bb agent))
                  (sug-bb (agent~blackboard agent)))
             (agent=loop
              :wait (and (agent~check-resources agent)
			 (or (agent=new-information? agent sug-bb)
			     (bb~new-entries com-bb)))
	      ;; agent works only if it has enough resources MALTE
	      :wait-msg "Waiting for new entry."
              :reset (progn (when rsrc*use-resources (agent=evaluate-resources agent))
			    (bb~reset (agent~surveyed-bb agent)))
	      :reset-msg (sugg~output agent 3 "Reseting blackboard ~A." (bb~name com-bb))
              :body
              (if (agent=new-information? agent sug-bb)
                  (agent=propagate-information agent sug-bb com-bb)
                (let ((new-first (bb~merge-new-entries com-bb)))
                  (cond ((and new-first (bb~entry-empty-p new-first))
                         (sugg~output agent 5 "Empty best suggestion not passed on"))
                        ((and new-first (bb~p sug-bb))
                         (sugg~trace-output agent "Passing on new best suggestion: ~A" new-first)
                         (bb~add-new-entry sug-bb
                                           (bb~create-command-suggestion (agent~command agent) new-first (list agent))))
                        (new-first (sugg~output agent 1 "Don't have a blackboard assigned to write to!"))))))))
  (:method ((agent agent+suggestion-agent))
           (let* ((sug-bb (agent~surveyed-bb agent)))
             (agent=loop
              :wait (bb~new-entries sug-bb)
              :wait-msg "Waiting for new entry"
              :reset (progn (when rsrc*use-resources (agent=evaluate-resources agent))
			    (bb~reset (agent~surveyed-bb agent)))
              :reset-msg (sugg~output agent 3 "Reseting blackboard ~A" (bb~name sug-bb))
              :body
	      (let ((new-list (bb~merge-new-entries sug-bb)))
		    (when new-list (agent=suggest-new-suggestions agent new-list))))))
  (:method ((agent agent+predicate-agent))
           (let (app-entries)
             (agent=loop
              :wait-msg "Waiting for useful entry"
              :wait (and (agent=applicable-on-information-p agent)
                         (setf app-entries (agent=get-applicable-entries agent))
			 (agent~check-resources agent))
			 ;; agent works only if it has enough resources MALTE
	      ;; app-entries will be get bound to all entries on blackboard on wich agent can be applied
              :reset-msg (sugg~output agent 5 "Resetting.")
              :reset (when rsrc*use-resources (agent=evaluate-resources agent))
	      :body
              (progn
		(sugg~output agent 7 "New applicable entries: ~A" app-entries)
		(dolist (entry app-entries)
		  (let (t1 t2)
		    (setf t1 (rsrc~get-time-stamp))
		  ;;; generating new entries, using entry
                  (multiple-value-bind (new-values rest error args)
                      (agent=apply-function agent entry)
                    (declare (ignore rest))      ;;; there we could do something with the passive foci.
		    (setf t2 (rsrc~get-time-stamp))
		    (incf (rsrc~last-entries (agent~resources agent)) 1)
                    (cond (error (incf (rsrc~errors (agent~resources agent)) 1)
				 (sugg~output agent 0 "Predicate could not be applied to arguments ~A!~%>>>>  ~A" args error))
                          (new-values (incf (rsrc~last-used (agent~resources agent)) 1)
				      ;; the following statement is only a FAKE MH
				      (unless (agent~check-reset-state agent)
					(progn
					  (bb~agent-visited-entry agent entry)
					  (setf (rsrc~run-time-last (agent~resources agent)) (rsrc~time-diff t1
													     t2))
					  (sugg~output agent 2 "Updated time")
					  (agent=reset-with-resources)))
				      (agent=add-new-entries agent entry new-values))
			  (t (incf (rsrc~last-bad (agent~resources agent)) 1)
			     (sugg~output agent 11 "Could not find anything."))) ;;VS: here can go a penalty 
                                                                            ;;; on the resource  <---- to be done.
                    (bb~agent-visited-entry agent entry)
		    (setf (rsrc~run-time-last (agent~resources agent)) (rsrc~time-diff t1
										       t2))
		    (agent=reset-with-resources))))))))
                    ;;; prueft nur, ob reset von aussen MH
  (:method ((agent agent+function-agent))
           (let (app-entries)
             (agent=loop
              :wait-msg "Waiting for useful entry"
              :wait (and (agent=applicable-on-information-p agent)
                         (setf app-entries (agent=get-applicable-entries agent))
			 (agent~check-resources agent))
              :reset-msg (sugg~output agent 5 "Reseting.")
              :reset (when rsrc*use-resources (agent=evaluate-resources agent))    
              :body
              (progn
                (sugg~output agent 7 "New applicable entries: ~A" app-entries)
                (dolist (entry app-entries)
		   (let ((t1) (t2))
		    (setf t1 (rsrc~get-time-stamp))
                  (multiple-value-bind (new-values rest error args)
                      (agent=apply-function agent entry)
                    (declare (ignore rest))
		     (setf t2 (rsrc~get-time-stamp))
		     (incf (rsrc~last-entries (agent~resources agent)) 1)
		     (cond (error (incf (rsrc~errors (agent~resources agent)) 1)
				  (sugg~output agent 0 "Function could not be applied to arguments ~A!~%>>>>  ~A" args error))
			   (new-values (incf (rsrc~last-used (agent~resources agent)) 1) ;; actualizing
				       ;; the number of entries the agent used for
				       ;; generation new suggestion
				       (agent=add-new-entries agent entry new-values))
			   (t (incf (rsrc~last-bad (agent~resources agent)) 1) 
			      (sugg~output agent 1 "This case should not occur for function agents!"))))
                  (bb~agent-visited-entry agent entry)
		  (setf (rsrc~run-time-last (agent~resources agent)) (rsrc~time-diff t1 t2))
		  (agent=reset-with-resources))))))))
           
(defun agent=classifier-initial-duty (agent)
  (declare (edited  "05-DEC-1999")
	   (authors Sorge)
	   (input   "A classifier agent.")
	   (effect  "Function is executed the very first time the agent's process is resumed."
		    "It then removes itself from the agents property list.")
	   (value   "Undefined."))
  (when (foci~active-pc)
    (let* ((process (agent~process agent)))
      (agent=apply-classifier agent)
      (sugg~output agent 6 "Executing my initial duty.")
      (setf  (agent=initial-duty-switch agent) nil)
; see above
;      (setf (proc~property-list process) nil)
	   )))

(defun agent=apply-classifier (agent)
  (declare (edited  "03-DEC-1999")
	   (authors Sorge)
	   (input   "A classifier agent.")
	   (effect  "Checks the open line of the focus and enters or removes information from the suggestion blackboard.")
	   (value   "Undefined."))
  (sugg~output agent 5 "Applying my classification function.")
  (let* ((sugg-bb (agent~blackboard agent))
	 (function (agent~classification-function agent))
	 (information (agent~information agent))
	 (old-info (agent~old-information agent)))
    (multiple-value-bind (result error)
	(ignore-errors (funcall function (foci~focus-line (foci~active-pc))))
      (cond (error
	     (sugg~output agent 0 "Classification function could not be applied~%>>>>  ~A" error))
	    ((null result)
	     (bb~remove-information sugg-bb old-info)
	     (setf (agent~old-information agent) result)
	     (sugg~trace-output agent "Removing information: ~A" old-info))
	    ((eq information t)
	     (bb~remove-information sugg-bb old-info)
	     (bb~enter-information sugg-bb result)
	     (setf (agent~old-information agent) result)
	     (sugg~trace-output agent "New information: ~A" result))
	    (information
	     (unless (equal old-info information)
	       (bb~enter-information sugg-bb information)
	       (setf (agent~old-information agent) information)
	       (sugg~trace-output agent "New information: ~A" information)))
	    (t (let ((name (agent~name agent)))
		 (unless (string-equal old-info name)
		   (bb~enter-information sugg-bb name)
		   (setf (agent~old-information agent) name)
		   (sugg~trace-output agent "New information: ~A" name))))))))

  

(defgeneric agent=apply-function (agent entry)
  (declare (edited  "24-NOV-1999")
	   (authors Sorge)
	   (input   "An agent and a blackboard entry.")
	   (effect  "Applys the function of the agent to the entry.")
	   (value   "Returns up to four values:"
		    "1 - Results from applying the agent's function."
		    "2 - Additional results from applying the function to passive foci."
		    "3 - A value if an error occured when building the function."
		    "4 - The arguments the function was applied to, when the error occured."))
  (:method ((agent agent+c-pred-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-open new-pred NIL)))))
  (:method ((agent agent+p-pred-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-premise new-pred)))))
  (:method ((agent agent+a-pred-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-line new-pred)))))
  (:method ((agent agent+s-pred-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-support new-pred)))))
  (:method ((agent agent+s-pred-list-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-supportlist new-pred)))))
  (:method ((agent agent+c-pred-list-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (pred (agent~predicate agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (new-pred error)
		 (ignore-errors (apply pred args))
	       (if error (values nil nil error args) (foci~find-openlist new-pred)))))
  (:method ((agent agent+function-agent) (entry bb+parameter-suggestion))
	   (let* ((mapping (bb~entry-mapping entry))
		  (depend (agent~dependence agent))
		  (args (agent=get-arguments depend mapping)))
	     (multiple-value-bind (result error)
		 (ignore-errors (multiple-value-list (apply (agent~function agent) args)))
	       (values result nil error args)))))

(defgeneric agent=add-new-entries (agent old-entry new-entries);;;MP
  (declare (edited  "24-NOV-1999")
	   (authors Sorge)
	   (input   "An agent, and old parameter suggestion and a list of new entries.")
	   (effect  "Adds the entries to the agents blackboard.")
	   (value   "Undefined."))
  (:method ((agent agent+parameter-agent) (old-entry bb+parameter-suggestion) new-entries)
	   (sugg~output agent 6 "Computed new values ~A" new-entries)
	   (let* ((bb (agent~blackboard agent))
		  (alist (agent=associate-value-with-arg (agent~goal agent) (agent~multiple agent) new-entries))
		  (new-bb-entries (bb~add-list-of-list-entries old-entry alist (bb~com-args bb))))
	     (sugg~output agent 7 "Computed new blackboard entries ~A." new-bb-entries)
	     (bb~add-new-entry bb new-bb-entries)
	     (sugg~trace-output agent "New suggestions: ~{~%~A~}" new-bb-entries)
	     (bb~agent-visited-entry agent new-bb-entries)))
    (:method ((agent agent+p-pred-agent) (old-entry bb+parameter-suggestion) new-entries)
	   (sugg~output agent 6 "Computed new values ~A" new-entries)
	   (let* ((bb (agent~blackboard agent))
		  (alist (mapcan #'(lambda (new-entry)
				     (agent=associate-value-with-arg (agent~goal agent)
								     (agent~multiple
								      agent) new-entry))
				 new-entries))
		  (new-bb-entries (bb~add-list-of-list-entries old-entry alist (bb~com-args bb))))
	     (sugg~output agent 7 "Computed new blackboard entries ~A." new-bb-entries)
	     (bb~add-new-entry bb new-bb-entries)
	     (sugg~trace-output agent "New suggestions: ~{~%~A~}" new-bb-entries)
	     (bb~agent-visited-entry agent new-bb-entries)))
    (:method ((agent agent+a-pred-agent) (old-entry bb+parameter-suggestion) new-entries)
	   (sugg~output agent 6 "Computed new values ~A" new-entries)
	   (let* ((bb (agent~blackboard agent))
		  (alist (mapcan #'(lambda (new-entry)
				     (agent=associate-value-with-arg (agent~goal agent)
								     (agent~multiple
								      agent) new-entry))
				 new-entries))
		  (new-bb-entries (bb~add-list-of-list-entries old-entry alist (bb~com-args bb))))
	     (sugg~output agent 7 "Computed new blackboard entries ~A." new-bb-entries)
	     (bb~add-new-entry bb new-bb-entries)
	     (sugg~trace-output agent "New suggestions: ~{~%~A~}" new-bb-entries)
	     (bb~agent-visited-entry agent new-bb-entries)))
    (:method ((agent agent+s-pred-agent) (old-entry bb+parameter-suggestion) new-entries)
	   (sugg~output agent 6 "Computed new values ~A" new-entries)
	   (let* ((bb (agent~blackboard agent))
		  (alist (mapcan #'(lambda (new-entry)
				     (agent=associate-value-with-arg (agent~goal agent)
								     (agent~multiple
								      agent) new-entry))
				 new-entries))
		  (new-bb-entries (bb~add-list-of-list-entries old-entry alist (bb~com-args bb))))
	     (sugg~output agent 7 "Computed new blackboard entries ~A." new-bb-entries)
	     (bb~add-new-entry bb new-bb-entries)
	     (sugg~trace-output agent "New suggestions: ~{~%~A~}" new-bb-entries)
	     (bb~agent-visited-entry agent new-bb-entries))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; administration of agent processes

(defun agent=register-process (process)
  (pushnew process agent*processes))

(defun agent=un-register-process (agent)
  (setf agent*processes
	(delete (agent~process agent) agent*processes)))

;;; handling information

(defun agent=new-information? (agent bb &optional (compare #'equal))
  (declare (edited  "02-DEC-1999")
	   (authors Sorge)
	   (input   "A command agent, a blackboard and a binary predicate.")
	   (effect  "None.")
	   (value   "T if the information on the blackboard is new for the agent."))
  (if (functionp compare)
      (let ((new? (not (funcall compare (agent~information agent) (bb~information bb)))))
	(when new? (sugg~output agent 5 "Found new information on blackboard ~A" bb))
	new?)
    (sugg~output t 0 "AGENT=NEW-INFORMATION?: ~A is not a binary function!" compare)))

(defun agent=propagate-information (agent bb1 bb2)
  (declare (edited  "02-DEC-1999")
	   (authors Sorge)
	   (input   "An agent and two blackboards.")
	   (effect  "The agent propagates the information of BB1 to BB2 and stores it in its own information slot.")
	   (value   "Undefined."))
  (let ((information (bb~information bb1)))
    (sugg~output agent 5 "Passing information ~A from ~A to ~A." information bb1 bb2)
    (setf (agent~information agent) information)
    (sugg~output agent 8 "New information: ~A" information)
    (setf (bb~information bb2) information)
    (sugg~output bb2 8 "New information: ~A" information)))

(defun agent=applicable-on-information-p (agent)
  (declare (edited  "02-DEC-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "None.")
	   (value   "T if an element the agents information list is contained"
		    "in the information list of the agents blackboard."))
  (labels ((compare-lists (list1 list2)
			  (cond ((null list1) nil)
				((find (car list1) list2 :test #'string-equal) t)
				(t (compare-lists (cdr list1) list2)))))
    (let ((agent-info (agent~information agent))
	  (bb-info (bb~information (agent~blackboard agent))))
      (if (and agent-info bb-info)
	  (compare-lists agent-info bb-info)
	t))))

;;; to be moved later

(defgeneric agent=associate-value-with-arg (arg multiple value)
  (:method (arg (multiple list) value)
	   (list (list (cons arg value))))
  (:method ((arg list) (multiple list) value)
	   (list (list (cons (car arg) value))))
  (:method (arg (multiple list) (values list))
	   (if (find arg multiple :test #'string-equal)
	       (mapcar #'(lambda (new-value)
			   (list (cons arg new-value)))
		       values)
	     (list (list (cons arg values)))))
  (:method ((args list) (multiple list) (values list))
	   (do* ((restargs args (cdr restargs))
		 (restvalues values (cdr restvalues))
		 (arg (car args) (car restargs))
		 (value (car values) (car restvalues))
		 (result nil))
	       ((or (null restargs) (null restvalues)) result)
	     (let ((found (find arg multiple :test #'string-equal)))
	       (setf result
		     (cond ((and found result (consp value))
			    (mapcan #'(lambda (old-value)
					(mapcar #'(lambda (val)
						    (append old-value (list (cons arg val))))
						value))
				    result))
			   ((and found result)
			    (mapcar #'(lambda (old-value)
					(append old-value (list (cons arg value))))
				    result))
			   ((and found (consp value))
			    (mapcar #'(lambda (val) (list (cons arg val))) value))
			   (found
			    (list (list (cons arg value))))
			   (result
			    (mapcar #'(lambda (old-value)
					(append old-value (list (cons arg value))))
				    result))
			   (t (list (list (cons arg value))))))))))
;;;					(format t "old-value ~A~%" old-value)


(defun agent=suggest-new-suggestions (agent new-suggestions)
  (declare (edited  "19-NOV-1999")
	   (authors Sorge)
	   (input   "An agent and a list of suggestions.")
	   (effect  "Passes on the suggestions with the appropriate suggestion function.")
	   (value   "Undefined."))
  (when (and new-suggestions (agent~suggestion-agent-p agent))
    (let ((function (agent~output-function agent)))
      (cond ((functionp function)
	     (multiple-value-bind (value error)
		 (ignore-errors (funcall function new-suggestions))
	       (declare (ignore value))
	       (when error
		 (sugg~output agent 0 "Couldn't apply output function ~A to arguments ~A!"
			      function new-suggestions))))
	    ((symbolp function)
	     (multiple-value-bind (func error)
		 (symbol-function function)
	       (if error
		   (sugg~output agent 0 "My output function ~A doesn't have a function definition!" function)
		 (multiple-value-bind (value error)
		     (ignore-errors (apply func new-suggestions))
		   (declare (ignore value))
		   (when error
		     (sugg~output agent 0 "Couldn't apply output function ~A to arguments ~A!"
				  func new-suggestions))))))
	    (t (sugg~output agent 0 "My output function ~A doesn't make any sense!" function))))))


(defun agent=unique-parameter-agent-name (command goal depend &optional exclude)
  (declare (edited  "08-APR-1998")
	   (authors Sorge Chris)
	   (input   "A command name, a list of goal names and two lists of other parameter names.")
	   (effect  "None.")
	   (value   "A string representing a unique name for an agent."))
  (flet ((name2string (name)
		      (etypecase name
			(symbol (symbol-name name))
			(string (string-upcase name))))
	 (namelist2string (list)
			  (concatenate 'string
				       (let ((string (car list)))
					 (dolist (el (cdr list) string)
					   (setf string (concatenate 'string string "*" el)))))))
    (let ((com-name (name2string command))
	  (goal-names (sort (mapcar #'name2string goal) #'string<))
	  (depend-names (sort (mapcar #'name2string depend) #'string<))
	  (exclude-names (sort (mapcar #'name2string exclude) #'string<)))
      (concatenate 'string
		   com-name "-"
		   (namelist2string goal-names)
		   (when depend
		     (concatenate 'string "=" (namelist2string depend-names)))
		   (when exclude
		     (concatenate 'string "!" (namelist2string exclude-names)))))))
			      
(defun agent=test-arguments (args command)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A list of arguments and a command.")
	   (effect  "None.")
	   (value   "T if the argument-names in the list are syntactically equal with"
		    "arguments defined for the command."))
  (let ((com-args (agent=command-argnames command)))
    (or (every #'(lambda (x) (or (find x com-args :test #'string-equal)
			     (sugg~output nil 1 "Specified argument ~A does not exist!" x)))
	   args)
	T))) ;;;MP always build agents, even when argument does not exist (for hidden agents)

(defgeneric agent=compile-function (agent)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "The predicate function of the agent is compiled. This is done very carefully,"
		    "i.e. we evaluate and compile the functions within an error handler and give"
		    "information if something went wrong, without letting the whole mechanism crash.")
	   (value   "The new compiled function."))
  (:method (agent)
	   (sugg~output t 0 "~A is not a parameter agent or classifier." agent))
  (:method ((agent agent+classifier))
	   (let ((func (agent~classification-function agent)))
	     (unless (functionp func)
	       (let* ((evalfun (ignore-errors
				 (if (symbolp func)
				     (symbol-function func)
				   (eval func))))
		      (compfun (ignore-errors (compile evalfun))))
		 (cond (compfun
			 (setf (agent~classification-function agent) compfun)
			 (sugg~output agent 4 "Classification function successfully compiled."))
		       (evalfun
			(setf (agent~classification-function agent) evalfun)
			(sugg~output agent 8 "Classification function could not be compiled.")
			(sugg~output agent 4 "Classification function successfully evaluated."))
		       (t (sugg~output agent 0 "Invalid classification function!")))))))
  (:method ((agent agent+predicate-agent))
	   (unless (functionp (agent~predicate agent))
	     (let* ((depend (agent~dependence agent))
		    (goal (agent~goal agent))
		    (func-name (read-from-string (agent~name agent)))
		    (func (list 'defun func-name depend
				(list 'lambda (list (car goal))
				      (agent~predicate agent))))
		    (evfun (ignore-errors (eval func)))
		    (compfun (ignore-errors (compile evfun))))
	       (cond (compfun 
		      (setf (agent~predicate agent) (symbol-function func-name))
		      (sugg~output agent 4 "Predicate successfully compiled."))
		     (evfun
		      (sugg~output agent 4 "Predicate successfully evaluated.")
		      (sugg~output agent 0 "Something went wrong when compiling my predicate."))
		     (t
		      (sugg~output agent 0 "Something went wrong when evaluating my predicate.")))))
	   (agent~predicate agent))
  (:method ((agent agent+function-agent))
	   (unless (functionp (agent~function agent))
	     (let* ((depend (agent~dependence agent))
		    (func-name (read-from-string (agent~name agent)))
		    (func (list 'defun func-name depend
				(agent~function agent)))
		    (evfun (ignore-errors (eval func)))
		    (compfun (ignore-errors (compile evfun))))
	       (cond (compfun 
		      (setf (agent~function agent) (symbol-function func-name))
		      (sugg~output agent 4 "Function successfully compiled."))
		     (evfun
		      (sugg~output agent 4 "Function successfully evaluated.")
		      (sugg~output agent 0 "Something went wrong when compiling my function."))
		     (t
		      (sugg~output agent 0 "Something went wrong when evaluating my function.")))))
	   (agent~function agent)))
		  

;;; Facility to build metafunctions
;;;
;;; The following functions are needed to rewrite the given function definitions of
;;; parameter agents and to construct meta-functions that can be compiled to
;;; predicates later on.

(defun agent=build-function&predicate (definition goal depend &optional meta-functions)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "The definition of a predicate, the goal argument and a list of"
		    "dependency arguments. Optionally a list of meta-function definitions.")
	   (effect  "None.")
	   (value   "A funcallable expression."))
  (cond ((functionp definition) definition)
	((agent=find-metafunc definition meta-functions)
	 (agent=build-function&predicate (agent=match-metafunc definition meta-functions)
					goal depend meta-functions))
	(t (agent=rewrite-definition definition goal depend meta-functions))))

(defun agent=rewrite-definition (def goal depend &optional meta-functions)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "An expression of a predicate or function and a list of arguments."
		    "Optionally a list of meta-function definitions.")
	   (effect  "None.")
	   (value   "The expression with certain keywords substituted."))
  (if (atom def) (if (find def (append goal depend))
		     (list 'node~formula def)   
		   def)
    (let* ((function (car def))
	   (arguments (cond ((string-equal function :lambda)
			     (agent=rename-lambda-vars (cadr def) (cddr def)))
			    ((string-equal function :let)
			     (agent=rename-let-vars (second def) (cddr def)))
			    ((string-equal function :let*)
			     (agent=rename-let*-vars (second def) (cddr def)))
			    (t (cdr def)))))
      (multiple-value-bind (func error)
	  (ignore-errors (symbol-function function))
	(declare (ignore func))
	(if error
	    (case function
;;;	      (:no-func (append (first arguments) (agent=rewrite-definition (cdr arguments) goal depend)))
	      (:protect arguments)
	      (:param (first arguments))
	      (:node (first arguments))
	      (:hyps (list 'pdsn~hyps (first arguments)))            ;;; KEIM dependencies    VS
	      (:just (list 'node~justification (first arguments)))
	      (:let-declaration
	       (mapcar #'(lambda (decl)
			   (let ((var (first decl))
				 (rest (second decl)))
			     (list var
				   (agent=build-function&predicate rest goal depend meta-functions))))
		       arguments))
	      (t (sugg~output nil 0 "~A does not have a function definition..." function)))
	  (cons function
		(mapcar #'(lambda (x) (agent=build-function&predicate x goal depend meta-functions)) arguments)))))))

(defun agent=rename-bound-vars (vars scope)
  (declare (edited  "12-DEC-1999" "04-SEP-1998")
	   (authors Sorge Sorge)
	   (input   "A list of bound variables and the scope containing the functionality.")
	   (effect  "None.")
	   (value   "1. The list of new variables."
		    "2. The scope, where all variables are uniquely renamed."))
  (let* ((new-vars (mapcar #'(lambda (x)
			       (etypecase x
				 (string (gensym x))
				 (symbol (gensym (symbol-name x)))))
			   vars))
	 (subst (pairlis vars new-vars)))
    
    (values new-vars (sublis subst scope))))
    
(defun agent=rename-lambda-vars (binder scope)
  (declare (edited  "12-DEC-1999")
	   (authors Sorge)
	   (input   "A list of bound variables and the scope containing the functionality.")
	   (effect  "None.")
	   (value   "The binder with the scope, where all variables are uniquely renamed."))
  (multiple-value-bind (new-vars new-scope)
      (agent=rename-bound-vars binder scope)
    (cons (cons :protect new-vars) new-scope)))

(defun agent=rename-let-vars (decls scope)
  (declare (edited  "12-DEC-1999")
	   (authors Sorge)
	   (input   "A list of let-declarations and the scope of the let binding.")
	   (effect  "None.")
	   (value   "The declarations and the scope, where all variables are uniquely renamded."))
  (let ((vars (mapcar #'car decls)))
    (multiple-value-bind (new-vars new-scope)
	(agent=rename-bound-vars vars scope)
      (cons
       (cons :let-declaration
	     (mapcar #'(lambda (var decl)
			 (list var (cadr decl)))
		    new-vars  decls))
       new-scope))))

(defun agent=rename-let*-vars (decls scope)
  (declare (edited  "12-DEC-1999")
	   (authors Sorge)
	   (input   "A list of let*-declarations and the scope of the let binding.")
	   (effect  "None.")
	   (value   "The declarations and the scope, where all variables are uniquely renamded."))
  (flet ((union-let-declaration (scope)
		(let ((decl1 (first scope))
		      (decl2 (second scope))
		      (rest (cddr scope)))
		  (if (and (consp decl2)
			   (string-equal (car decl2) :let-declaration))
		      (cons
		       (cons :let-declaration (append (cdr decl1) (cdr decl2)))
		       rest)
		    scope))))
    (if (null decls)
	scope
      (let* ((old-decls (car decls))
	     (old-scope (agent=rename-let*-vars (cdr decls) scope))
	     (new-scope (agent=rename-let-vars (list old-decls) old-scope)))
	(union-let-declaration new-scope)))))

(defun agent=find-metafunc (func meta-funcs)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A function definition and a list of metafunction definitions.")
	   (effect  "None.")
	   (value   "T if func is in the meta-function list. O/w nil."))
  (when (listp func)
    (find (car func) meta-funcs :test #'(lambda (x y)
					  (string-equal x (car y))))))

(defun agent=match-metafunc (func meta-funcs)
  (declare (edited  "11-APR-1998")
	   (authors Sorge)
	   (input   "A function definition and a list of metafunction definitions.")
	   (effect  "None.")
	   (value   "The rewriten function definition where the apropriate metafunction has been"
		    "substituted."))
  (let* ((meta-func (agent=find-metafunc func meta-funcs))
	 (old-args (second meta-func))
	 (new-args (cdr func)))
    (sublis (pairlis old-args new-args) (caddr meta-func))))


;;; checking for applicability of agents on entries

(defgeneric agent=get-arguments (args a-list &optional (some nil))
  (:method (args (a-list list) &optional (some nil))
	   (let ((new-alist (mapcar #'(lambda (x)
					(assoc x a-list :test #'string-equal))
				    args)))
	     (cond (some (mapcar #'cdr (remove-if #'null new-alist)))
		   ((some #'null new-alist) nil)
		   (t (mapcar #'cdr new-alist)))))
  (:method (args (entry bb+parameter-suggestion) &optional (some nil))
	   (agent=get-arguments args (bb~entry-mapping entry) some)))

(defgeneric agent=lookup-goal (goal a-list)
  (:method (goal (a-list list))
	   (cdr (assoc goal a-list)))
  (:method (goal (bb-entry bb+parameter-suggestion))
	   (assoc goal (bb~entry-mapping bb-entry))))

(defgeneric agent=applicable-on-entry-p (agent entry)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A parameter agent and a blackboard entry.")
	   (effect  "None.")
	   (value   "T if the agent is applicable on the entry. O/w NIL."))
;  (:method ((agent agent+predicate-agent) (entry bb+parameter-suggestion))
;           (and (not (bb~entry-visited-p entry agent))
;                (let ((depend (agent~dependence agent))
;                      (goal (agent~goal agent)))
;                  (and (or (not depend)
;                           (agent=get-arguments depend entry))
;                       (not (agent=lookup-goal goal entry))))))
  (:method ((agent agent+parameter-agent) (entry bb+parameter-suggestion))
	   (and (not (bb~entry-visited-p entry agent))
		(let ((depend (agent~dependence agent))
		      (goals (agent~goal agent))
		      (exclude (agent~exclude agent)))
		  (and (or (not depend)
			   (agent=get-arguments depend entry))
		       (or (not exclude)
			   (not (agent=get-arguments exclude entry t)))
		       (notany #'(lambda (goal)
				   (agent=lookup-goal goal entry))
			       goals))))))

(defun agent=get-applicable-entries (agent)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "An agent.")
	   (effect  "None.")
	   (value   "A list of blackboard entries from the agent's blackboard,"
		    "the agent can be applied to."))
    (let ((entries (bb~entries (agent~blackboard agent))))
    (when entries
      (progn
	(labels ((get-entries (entries)
			      (cond ((null entries) nil)
				    ((agent=applicable-on-entry-p agent (car entries))
				     (cons (car entries) (get-entries (cdr entries))))
				    (t
				     (bb~agent-visited-entry agent (car entries))
				     (get-entries (cdr entries))))))
	(get-entries entries))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spcializations for methods from other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Blackboard Module

(defmethod bb~enter-agent ((bb bb+blackboard) (agent agent+agent))
  (sugg~output bb 8 "Registering agent ~A." bb)
  (let* ((old-agents (bb~agents bb))
	 (repl-agent (find-if #'(lambda (x)
				  (string-equal (agent~name x) (agent~name agent)))
			      old-agents))
	 (new-agents (if repl-agent
			 (cons agent (remove repl-agent old-agents))
		       (cons agent old-agents))))
    (sugg~output bb 10 "Old agents: ~A" bb old-agents)
    (when repl-agent (sugg~output bb 9 "Agent is replaced."))
    (setf (bb~agents bb) new-agents)
    (sugg~output bb 10 "New agents: ~A" new-agents)))

(defmethod bb~remove-agent ((bb bb+command-blackboard) (agent agent+surveyor))
  (sugg~output bb 10 "Old surveyor agent: ~A." (bb~surveyor bb))
  (if (eq agent (bb~surveyor bb))
      (setf (bb~surveyor bb) nil)
    (sugg~output bb 1 "Cannot remove agent ~A. It's not my surveyor!" agent))
  (sugg~output bb 10 "New surveyor agent: ~A." (bb~surveyor bb)))
  
(defmethod bb~remove-agent ((bb bb+suggestion-blackboard) (agent agent+command-agent))
  (sugg~output bb 10 "Old command agents: ~A." (bb~agents bb))
  (setf (bb~agents bb) (remove agent (bb~agents bb)))
  (sugg~output bb 10 "New command agents: ~A." (bb~agents bb)))

(defmethod bb~remove-agent ((bb bb+suggestion-blackboard) (agent agent+classifier))
  (sugg~output bb 10 "Old classifying agents: ~A." (bb~classifier bb))
  (setf (bb~agents bb) (remove agent (bb~agents bb)))
  (sugg~output bb 10 "New classifying agents: ~A." (bb~classifier bb)))

(defmethod bb~remove-agent ((bb bb+command-blackboard) (agent agent+parameter-agent))
  (sugg~output bb 10 "Old parameter agents: ~A." (bb~agents bb))
  (setf (bb~agents bb) (remove agent (bb~agents bb)))
  (sugg~output bb 10 "New parameter agents: ~A." (bb~agents bb)))

(defmethod bb~find-agent ((bb bb+blackboard) (agent agent+agent))
  (find agent (bb~agents bb)))

(defmethod bb~find-agent ((bb bb+blackboard) (agent string))
  (find agent (bb~agents bb)
	:test #'(lambda (x y) (string-equal (agent~name x) (agent~name y)))))

(defmethod bb~find-agent ((bb bb+blackboard) (agent symbol))
  (find agent (bb~agents bb)
	:test #'(lambda (x y) (string-equal (agent~name x) (agent~name y)))))

(defmethod bb~entry-visited-p ((entry bb+entry) (agent agent+agent))
  (find (agent~name agent) (bb~entry-visited entry) :test #'string-equal))

(defmethod bb~agent-visited-entry ((agent agent+agent) (entry bb+entry))
  (let ((name (agent~name agent)))
    (unless (find name (bb~entry-visited entry) :test #'string-equal)
      (sugg~output agent 4 "Just visited entry ~A" entry)
      (pushnew (string-upcase name) (bb~entry-visited entry)))
    (bb~entry-visited entry)))

;;; Suggestion Module

(defmethod sugg~output  ((obj agent+agent) level string &rest args)
  (declare (ignore string args))
  (when (<= level sugg*output)
    (format t ";;;Agent ~A[~A]: " (agent~name obj) level)))

(defmethod sugg~output ((obj agent+command-agent) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Command Agent ~A [~A]: " (agent~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output  ((obj agent+parameter-agent) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Parameter Agent ~A [~A]: " (agent~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output  ((obj agent+suggestion-agent) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Suggestion Agent ~A [~A]: " (agent~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output  ((obj agent+classifier) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Classifier ~A [~A]: " (agent~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output  ((obj agent+resource-agent) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Resource Agent ~A [~A]: " (agent~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output  ((obj agent+dummy-agent) level string &rest args)
  NIL)


(defmethod sugg~trace-output ((obj agent+command-agent) string &rest args)
  (format t "Command Agent ~A: ~A~%" (agent~name obj)
	  (apply #'format (cons nil (cons string args)))))
	  
(defmethod sugg~trace-output ((obj agent+parameter-agent) string &rest args)
  (format t "Parameter Agent ~A: ~A~%" (agent~name obj)
	  (apply #'format (cons nil (cons string args)))))
	  
(defmethod sugg~trace-output ((obj agent+classifier) string &rest args)
  (format t "Classifier Agent ~A: ~A~%" (agent~name obj)
	  (apply #'format (cons nil (cons string args)))))

(defmethod sugg~trace-output ((obj agent+suggestion-agent) string &rest args)
  (format t "Suggestion Agent ~A: ~A~%" (agent~name obj)
	  (apply #'format (cons nil (cons string args)))))
	  
(defmethod sugg~reset-trace-object ((obj agent+surveyor))
  (agent~get-surveyor (agent~name obj)))
	  
(defmethod sugg~reset-trace-object ((obj agent+parameter-agent))
  (agent~get-parameter-agent (agent~name obj)))

(defmethod sugg~reset-trace-object ((obj agent+classifier))
  (agent~get-classifier (agent~name obj)))

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keim and OMEGA specific functionality
;; (this will need to be replaced, if the mechanism is ported)
;; We try to do as much as possible without Keim specific functions.
;; Where this cannot be avoided, we try to define generic functions 
;; and implement specific methods for particular Keim objects.
;; Where all this fails, the functions will have to be reimplemented.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Methods specializing on KEIM objects

(defmethod agent~find-parameter-agent ((command com+command) goal depend &optional exclude)
  (agent~find-parameter-agent (keim~name command) goal depend exclude))

(defmethod agent~command2parameter-agents ((command com+command))
  (agent~command2parameter-agents (keim~name command)))

(defmethod agent~command2command-agent ((command com+command))
  (agent~command2command-agent (keim~name command)))

(defmethod agent~associate-agent-2-command (agent (command com+command))
  (agent~associate-agent-2-command agent (keim~name command)))

(defmethod agent~make-command-agent ((command com+command))  ;;; This function could go within one of the
  (let* ((com-name (keim~name command))                      ;;; other methods. However it is more efficient
         (name (etypecase com-name                           ;;; to implement it like this,             VS
                 (string (string-upcase com-name))
                 (symbol (symbol-name com-name))))
         (old-bb (bb~find-blackboard command))
         (bb (if old-bb old-bb (bb~make-command-blackboard command)))
	 ;; MALTE
	 (res-obj (make-instance 'rsrc+bb-agent-resource :name command)) 
         (agent (agent~create-command-agent name command nil res-obj nil bb
                                            (format nil "Command agent for command ~A" name))))
    (agent~register-new-agent agent)
   ; (format t "Got a blackboard now: ~A Blackboard: ~A~%" (agent~name agent) (agent~blackboard agent))
   ;(format t "Created command agent with resources: ~A" res-obj)
   ; (rsrc~add-data (bb~resources (agent~blackboard agent)) (agent~resources agent))
    (sugg~output nil 2 "Command agent for command ~A created" name)
    (setf (bb~surveyor bb) agent)
    agent))
  
(defmethod agent=remove-from-hash-table ((com com+command) (table hash-table))
  (agent=remove-from-hash-table (keim~name com) table))

;;; Functions that need to be KEIM specific

(defgeneric agent=command-argnames (command)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "None.")
	   (value   "A list of names containing the argument names of the command."))
  (:method ((command com+command))
	   (com~argnames command)))

(defun agent=remove-parameter-agent (agent)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "A parameter agent.")
	   (effect  "The parameter agent is removed from the agent*commmand2parameter-agent-hash-table.")
	   (value   "Undefined."))
  (let* ((command (keim~name (agent~command agent)))
	 (name (etypecase command
		 (symbol (symbol-name command))
		 (string (string-upcase command))))
	 (agent-list (gethash name agent*command2parameter-agent-hash-table)))
    (setf (gethash name agent*command2parameter-agent-hash-table)
	  (remove (rassoc agent agent-list) agent-list))))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Object Methods (to be cleaned up)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object ((obj agent+a-pred-agent) stream)
  (format stream "A-Pred-Agent for ~A |Goals: {~{~A~^,~}} |Depends on: {~{~A~^,~}} |Exclude: {~{~A~^,~}}~% Resource-Information ~A"
          (agent~command obj)
          (agent~goal obj)
          (agent~dependence obj)
          (agent~exclude obj)
	  (agent~resources obj)
          ))
(defmethod print-object ((obj agent+p-pred-agent) stream)
  (format stream "P-Pred-Agent for ~A |Goals: {~{~A~^,~}} |Depends on: {~{~A~^,~}} |Exclude: {~{~A~^,~}}~% Resource-Information ~A"
          (agent~command obj)
          (agent~goal obj)
          (agent~dependence obj)
          (agent~exclude obj)
	  (agent~resources obj)
          ))

(defmethod print-object ((obj agent+s-pred-agent) stream)
  (format stream "S-Pred-Agent for ~A |Goals: {~{~A~^,~}} |Depends on: {~{~A~^,~}} |Exclude: {~{~A~^,~}}~% Resource-Information ~A"
          (agent~command obj)
          (agent~goal obj)
          (agent~dependence obj)
          (agent~exclude obj)
	  (agent~resources obj)
          ))

(defmethod print-object ((obj agent+c-pred-agent) stream)
  (format stream "C-Pred-Agent for ~A |Goals: {~{~A~^,~}} |Depends on: {~{~A~^,~}} |Exclude: {~{~A~^,~}}~% Resource-Information ~A"
          (agent~command obj)
          (agent~goal obj)
          (agent~dependence obj)
          (agent~exclude obj)
	  (agent~resources obj)
          ))

(defmethod print-object ((obj agent+function-agent) stream)
  (format stream "Function-Agent for ~A |Goals: {~{~A~^,~}} |Depends on: {~{~A~^,~}} |Exclude: {~{~A~^,~}}"
	  (agent~command obj)
	  (agent~goal obj)
	  (agent~dependence obj)
	  (agent~exclude obj)
	  ))

(defmethod print-object ((obj agent+resource-agent) stream)
  (format stream "Resource Agent"))

(defmethod print-object ((obj agent+classifier) stream)
  (format stream "Classifier ~A   ~A"
	  (agent~name obj) (agent~help obj)))

(defmethod print-object ((obj agent+command-agent) stream)
  (let ((bb (agent~blackboard obj)))
    (if bb
	(format stream "Command Agent ~A working for blackboard ~A" (agent~name obj) (bb~name bb))
      (format stream "Command Agent ~A working for no blackboard" (agent~name obj)))))

(defmethod print-object ((obj agent+suggestion-agent) stream)
  (format stream "Suggestion Agent ~A" (agent~name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some useful functions MH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun agent~list-turned-off-agents ()
  (declare (edited  "29-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A list of agents which currently have not enough resources"
		    "to run."))
  (mapcar #'(lambda (x)
	      (unless (rsrc~running-state (agent~resources x))
		(format t "~A~%" (agent~name x)))) (agent~get-agents)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing the information of a certain agents resources to a
;; file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


(defun agent~change-log-flag (agent)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Changes the log-flag of the agent. This function determines"
		    "if the agent records his resourcres.") 
	   (value   "None."))
  (if (rsrc~log-flag (agent~resources agent))
      (progn (setf (rsrc~log-flag (agent~resources agent)) NIL)
	     (sugg~output agent 2 "Agent ~A stopped recording resources"
			  (agent~name agent)))
    (progn
      (setf (rsrc~log-flag (agent~resources agent))
	    (format nil "~A~%--------------------------------------------~%" (agent~resources agent)))
      (sugg~output agent 2 "Agent ~A begin recording resources"
		   (agent~name agent)))))

(defun agent~write-res-log (agent)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Writes the information of the agents resources to a file.")
	   (value   "None."))
  (when (agent~resources agent)
    (progn 
      (ohlp=make-directory "~/omega-logs")
      (let ((path (format nil "~A~A.log" "~/omega-logs/" (agent~name agent))))
	(when (probe-file path) (sys~call-system (format nil "\rm ~A" path)))
	(with-open-stream (str (open path :direction :output :if-exists :supersede))
			  (format str "~A" (rsrc~log-flag (agent~resources agent))))))))

(defun agent~write-ls (agent-list)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "A list of agents.")
	   (effect  "Writes the informations of all agents in the list to a file.")
	   (value   "None."))
  (mapcar #'(lambda (x) (agent~write-res-log x)) agent-list))


(defun agent~write-logs (sym)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "A symbol.")
	   (effect  "If input    'commands   --> write resource information of command"
		    "                            agents to file"
		    "            'parameters --> write resource information of parameter"
		    "                            agents to file."
		    "            'all        --> write resource information of all"
		    "                            agents to file.")
	   (value   "None."))
  (cond ((string-equal sym 'all) (agent~write-ls (agent~get-agents)))
	((string-equal sym 'parameters) (agent~write-ls (agent~get-parameter-agents)))
	((string-equal sym 'commands) (agent~write-ls (agent~get-command-agents)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maltes methods

(defgeneric agent~check-resources (agent)
  (declare (edited  "28-APR-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Checks wether agent has enough resources to run and sets"
		    "rsrc~running-state.")
	   (value   "The new rsrc~running-state."))
  (:method ((agent agent+agent))
	   (if rsrc*use-resources
	       (if (not (agent~completeness-sens-p agent))
		   ;; HIER GEFAHR
		   (let ((res (agent~resources agent)))
		     (if (< (rsrc~act-resources res) rsrc*activation-level)
			 (progn (setf (rsrc~running-state res) NIL) NIL)
		       (progn (setf (rsrc~running-state res) T) T))) ;;; return T
		 T) T))) 


(defgeneric agent=evaluate-last-run (agent)
  (declare (edited  "28-APR-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Uses the appropiate evaluation heuristic."
		    "to evaluate the agents performance in the last run.")
	   (value   "NIL."))
  (:method ((agent agent+parameter-agent))
	   (setf (rsrc~res3 (agent~resources agent))
			    (heur~parameter-agent-evaluate-fun (agent~resources agent)
							       heur*param-agent-eval-fun))
	   ;; check wheter resource object has to be
	   ;; resetted
	   (heur~reset-resource-obj (agent~resources agent)))
  (:method ((agent agent+surveyor))
	   (setf (rsrc~res3 (agent~resources agent))
			   (heur~bb-agent-evaluate-fun (agent~resources agent)
						       heur*bb-agent-eval-fun))))
	  


(defun agent~check-global-activation-level (agent)
  (declare (edited  "07-JUN-2000")
	   (authors Mth)
	   (input   "The resource-agent.")
	   (effect  "Checks wether the global activation level has to be changed.")
	   (value   "NIL"))
      (let ((time-diff (- (agent~average-n-int agent)
			  (agent~average-prev-n-int agent))))
	(sugg~output agent 2 "New average user interval exceeded old interval by ~A sec."
		     (float (/ time-diff 1000)))
	(cond ((< time-diff (- (* heur*delta-exec-time 1000)))
	       ;; if the average command execution interval has fallen then
	       ;; increase global activation level
	       (incf rsrc*activation-level heur*delta-activation-level)
	       (sugg~output agent 2 "Increased global activation-level by ~A New act level: ~A"
			    heur*delta-activation-level
			    rsrc*activation-level))
	      ((> time-diff (* heur*delta-exec-time 1000))
	       ;; if the average command execution interval has raised then
	       ;; lower global activation level
	       (decf rsrc*activation-level heur*delta-activation-level)
	       (sugg~output agent 2 "Lowered global activation-level by ~A"
			    heur*delta-activation-level)))) NIL)

(defun agent=check-interaction-interval (agent)
  (declare (edited  "04-JUL-2000")
	   (authors Huebner)
	   (input   "The resource agent.")
	   (effect  "None.")
	   (value   "T if the user exceeded his average interaction interval,"
		    "NIL otherwise."))
  (when (agent=greater (agent~resets agent) heur*wait-for-n-resets)
    (when (and (agent=greater (float (agent~average-prev-n-int agent)) 0) ; no div by zero
	       (not (= (agent~last-exec-time agent) 0)))
      (let* ((act-time-diff (- (rsrc~get-time-stamp) (agent~last-time-stamp agent)))
	     (r (/ act-time-diff (agent~average-prev-n-int agent))))
	(if (agent=greater (float r) (+ 1.0 heur*act-diff))
	    (progn 
	      (sugg~output agent 2 "The average interaction interv. is ~A ms~% Relation:~
~A~% Treshold: ~A~%" (float (agent~average-prev-n-int agent)) (float r) (+ 1.0 heur*act-diff)) T)
	  NIL)))))
	 ; (agent=greater (float r) (+ 1.0 heur*act-diff)))))))


(defun agent=easy-div (a b)
  (/ a b))

(defun agent=greater (a b) (> a b))

(defun agent~change-short-term-act-level (agent)
  (declare (edited  "07-JUN-2000")
	   (authors Mth)
	   (input   "The resource agent.")
	   (effect  "Checks wether the activation level has to be changed"
		    "temporarily.")
	   (value   "None."))
  (decf rsrc*activation-level heur*delta-activation-level)
  (incf heur*act-diff heur*act-diff-help)
  (sugg~output agent 2 "Temporary lowered activation level to: ~A" rsrc*activation-level))

;;; Die koennte man auch in agent.lisp packen !
(defgeneric agent=reset-agent-resources (agent)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Reinstantiates the agents resource object.")
	   (value   "None."))
  (:method ((agent agent+suggestion-agent))
	   (setf (agent~resources agent) (make-instance 'rsrc+sugg-agent-resource :name
						      (agent~name agent)))
	   (sugg~output agent 5 "Agent ~A resetted resource object" (agent~name agent)))
  (:method ((agent agent+resource-agent))
	   (reinitialize-instance agent))
  (:method ((agent agent+dummy-agent))
	   (reinitialize-instance agent)
	   (sugg~output agent 5 "Resetted resource object."))
  (:method ((agent agent+command-agent))
	   (setf (agent~resources agent) (make-instance 'rsrc+bb-agent-resource :name
						      (agent~name agent)))
	   (rsrc~add-data (bb~resources (agent~blackboard agent)) (agent~resources agent))
	   (sugg~output agent 5 "Agent ~A resetted resource object" (agent~name agent)))
  (:method ((agent agent+parameter-agent))
	   (setf (agent~resources agent) (make-instance 'rsrc+param-agent-resource :name
						      (agent~name agent)))
	   (rsrc~add-data (bb~resources (agent~blackboard agent)) (agent~resources agent))
	   (sugg~output agent 5 "Agent ~A resetted resource object" (agent~name agent))))

;; functions for initialize agents resources


(defun agent=init-parameter-agent-resources (init-value)
  (declare (edited  "03-MAY-2000")
	   (authors Mth)
	   (input   "An integer.")
	   (effect  "Initializes all paramter agent resources with the integer given as input.")
	   (value   "None."))
  (mapcar #'(lambda (x) (setf (rsrc~act-resources (agent~resources x)) init-value))
	      (agent~get-parameter-agents)))

(defgeneric agent=get-sum-of-subagent-resources (agent)
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "A surveyor agent.")
	   (effect  "None.")
	   (value   "The sum of all the subagent resources."))
  (:method ((agent agent+surveyor))
	   (let ((res-sum 0))
	     (mapcar #'(lambda (x)
			 (incf res-sum (rsrc~act-resources (agent~resources x))))
		     (bb~agents (agent~surveyed-bb agent)))
	   res-sum)))

(defun agent=calculate-comm-agent-resources ()
  (declare (edited  "03-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Initializes all command agent resources with the average of the"
		    "resources of its parameter agents.")
	   (value   "None."))
  (mapcar #'(lambda (x)
		(let ((sum (agent=get-sum-of-subagent-resources x))
		      (sub-agents (bb~agents (agent~surveyed-bb x))))
		  (setf (rsrc~act-resources (agent~resources x)) (/ sum (length sub-agents)))))
	    (agent~get-command-agents)))


(defun agent=set-agent-resources (resource-value)
  (declare (edited  "18-MAY-2000")
	   (authors Mth)
	   (input   "An integer.")
	   (effect  "Initilializes resource slots form all agents with argument given.")
	   (value   "None."))
  (mapcar #'(lambda (x) (setf (rsrc~act-resources (agent~resources x)) resource-value))
	  (agent~get-agents)))
 
(defun agent~reset-resources ()
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Resets all resource objects in the system.")
	   (value   "None."))
  (declare (edited  "08-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Resets the resource objects of the entire system.")
	   (value   "None."))
  ;; resetting the resource-slots of each blackboard
  (mapcar #'(lambda (x) (agent=reset-bb-resources x)) (bb~get-blackboards))
  ;; resetting the resource objects of the agents
  (mapcar #'(lambda (x) (agent=reset-agent-resources x)) 
	  (remove-if-not #'(lambda (ag) (agent~blackboard ag)) (agent~get-agents)))
  ;; reset gloabal activation-level etc.
  (rsrc~reset-activation-level)
  ;; initialize heuristic again
  (agent~init-resources)
  (setf agent*external-prover-agents nil))



(defgeneric agent~distribute-resources (agent resources)
  (declare (edited  "29-APR-2000")
	   (authors Mth)
	   (input   "An agent and an integer, indicating the amount of"
		    "resources to be distributed to the subagents."
		    "Method also treats parameter agents.")
	   (effect  "Distributes the resources to the subagents.")
	   (value   "None."))
  (:method ((agent agent+surveyor) resources)
	   ;; sorting list according to heur~sort-p
	   (let ((res-list (sort (rsrc~agent-data (bb~resources (agent~surveyed-bb
								 agent)))  #'heur~sort-p)))
	     (setf   (rsrc~agent-data (bb~resources (agent~surveyed-bb agent))) res-list) 
	     ;; now distribute resources
	     (when res-list     ;; avoiding division by zero
	       (let ((res-per-obj (floor (/ resources (length res-list))))
		     (rest-resources (mod resources (length res-list)))
		     (counter 0))
		 ;; does this also work with negative resources ???
		 ;; res1(subagent) := res-per-obj
		 (mapcar #'(lambda (agent-res) (if (< counter rest-resources)
						   (progn (setf (rsrc~res1 agent-res)
								(+ res-per-obj 1))
		 					  (incf counter))
		 				 (setf (rsrc~res1 agent-res)
						       res-per-obj))) res-list))))))


(defgeneric agent~eval-fun (agent)
  (declare (edited  "31-MAY-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "This function evaluates the agents performance in the last run."
		    "If agent is a blackboard agent then the average performance of"
		    "it's subagents will be calculated and a readjustment of the"
		    "subagents resources may take place."
		    "This function serves as a shell for all the other functions"
		    "involved in this process.")
	   (value   "None."))
  (:method ((agent agent+resource-agent))
	   (incf (agent~resets agent))
	   ;; reset all data for interaction interval
	   (rsrc~reset-activation-level) 
	   ;; first update user interaction interval
	   (let* ((time (rsrc~get-time-stamp))
		  (delta-time (- time (agent~last-time-stamp agent))))
	     (setf (agent~last-exec-time agent) delta-time) ;; update last user interval
	     (setf (agent~last-time-stamp agent) time)      ;; save new time
	     (incf (agent~time-since-last-update agent) delta-time)  ;; update time since
								     ;; last update
	   (sugg~output agent 2 "Time since last reset ~A sec." (float (/ delta-time 1000))))
	   ;; then evaluate data
	   (incf (agent~resets-since-last-update agent))
	   (when (eql rsrc*exec-intervals (agent~resets-since-last-update agent))
	     (progn
	       (setf (agent~average-prev-n-int agent) (agent~average-n-int agent))
	       (setf (agent~average-n-int agent)
		     (/ (agent~time-since-last-update agent) rsrc*exec-intervals))
	       (when (eql (agent~average-prev-n-int agent) 0)
		 (setf (agent~average-prev-n-int agent) (agent~average-n-int agent)))
	       (setf (agent~time-since-last-update agent) 0)
	       (setf (agent~resets-since-last-update agent) 0)
	       (agent~check-global-activation-level agent))))
  (:method ((agent agent+suggestion-agent))
	   ;; reading information about subagents from blackboard and get resources
	   ;; which subagents gave themselfs after eval.
	   (rsrc~update-resources (agent~resources agent) (bb~resources (agent~surveyed-bb
									 agent)))
	   (sugg~output agent 5 "Suggestion agent ~A has eval. resources" (agent~name agent)))
  (:method ((agent agent+command-agent))
	   ;; reading information about subagents from blackboard and get resources
	   ;; which subagents gave themselfs after eval.
	   (rsrc~update-resources (agent~resources agent) (bb~resources (agent~surveyed-bb
										agent)))
	     ;; now the command agents knows the aggregated values from the last
	     ;; run and the new average values.

	     (when (> (rsrc~resets (agent~resources agent)) heur*wait-for-n-resets)
	       ;; give penalty and get the new resources from upper layer
	       (agent=actualize-res-slots agent)
	       (let ((running-state (rsrc~running-state (agent~resources agent)))
		     (new-state (agent~check-resources agent)))
		 (when (and running-state (not new-state))
		   (sugg~output agent 2 "Command agent ~A has given up working" (agent~name agent)))
		 (when (and (not running-state) new-state)
		   (sugg~output agent 2 "Command agent ~A turned on again" (agent~name agent)))))
	     (sugg~output agent 5 "Comand agent ~A has eval. resources" (agent~name agent)))
  (:method ((agent agent+parameter-agent))
	   (incf (rsrc~resets (agent~resources agent)) 1)
	   (if (> (rsrc~resets (agent~resources agent)) heur*wait-for-n-resets)
	       (progn 
		 ;; give penalty and get the new resources from upper layer
		 (agent=actualize-res-slots agent)
		 ;; setting the agents running state
		 (let ((running-state (rsrc~running-state (agent~resources agent)))
		       (new-state (agent~check-resources agent)))
		   (when (and running-state (not new-state))
		     (sugg~output agent 2 "Parameter agent ~A has given up working" (agent~name
										     agent)))
		   (when (and (not running-state) new-state)
		     (sugg~output agent 2 "Parameter agent ~A turned on again" (agent~name
										agent)))))
	     ;; else, only update the data
	     (heur~reset-resource-obj (agent~resources agent)))
	     (sugg~output agent 5 "Parameter agent ~A has eval. resources" (agent~name
									  agent))))





(defmethod agent~turn-off-subagents ((agent agent+surveyor))
  (declare (edited  "04-MAY-2000")
	   (authors Mth)
	   (input   "A surveyor agent.")
	   (effect  "Sets the resources of each subagent to its activation level.")
	   (value   "None."))
  (mapcar #'(lambda (x) (unless (agent~completeness-sens-p x)
			  (setf (rsrc~act-resources x) (- rsrc*activation-level 1))))
	  (rsrc~agent-data (bb~resources (agent~surveyed-bb agent))))
  NIL)


(defun agent~init-resources ()
  (agent=init-parameter-agent-resources heur*init-resource-value)
  (agent=calculate-comm-agent-resources)
  (format t "Initialized heuristic"))


(defgeneric agent=actualize-res-slots (agent)
  (declare (edited  "04-MAY-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Agent evaluates itself by calling the evaluate function"
		    "and sets res slots according to the result of the evaluation.")
	   (value   "NIL."))
  ; HOFFENTLICH STIMMT DAS !!!!
  (:method ((agent agent+agent))
	   (let ((res-obj (agent~resources agent)))
	     (agent=evaluate-last-run agent) ;; Penalty ?
	     (setf (rsrc~act-resources res-obj) (+ (rsrc~act-resources res-obj)
						   (rsrc~res1 res-obj)
						   (rsrc~res3 res-obj)))
	     (setf (rsrc~res3 res-obj) 0)      ;; reset penalty slot
	     (setf (rsrc~res1 res-obj) 0))))   ;; reset deltaR slot

(defun agent~completeness-sens-p (agent)
  (declare (edited  "14-JUN-2000")
	   (authors Mth)
	   (input   "An agent.")
	   (effect  "Inspecting the information slot.")
	   (value   "T, if the agent is needed to ensure completeness, NIL otherwise."))
  (if (find-if #'(lambda (x) (string-equal x ':c-sens)) (agent~information agent)) T NIL))



;; To be called before agent=reset-agent-resources !!!
(defun agent=reset-bb-resources (bb)
  (setf (rsrc~agent-data (bb~resources bb)) NIL)) 



;; ----------------------------------------------------------------------------
;; Handling the processes of the external provers, MH
;; ----------------------------------------------------------------------------

(defmethod atptop~kill ((arg (eql nil))) NIL)
(defmethod atptop~get-pids-of-children ((arg-1 (eql nil)) (arg-2 (eql nil))) NIL)


(defun agent~reset-ext-provers ()
  (sugg~output t 2 "~A provers have to be killed~%" (length agent*external-prover-agents))
  (mapcar #'agent~reset-external-prover agent*external-prover-agents)
  (setf agent*external-prover-agents NIL))

(defmethod agent~reset-external-prover ((agent agent+c-ext-pred-agent))
  (dolist (proc (agent~proc-id agent))
    (let ((pids (atptop~get-pids-of-children (car proc)
					     (cadr proc))))
      (if pids
	  (progn
	    (atptop~kill pids)
	    (sugg~output agent 2 "Killed my process."))
	(sugg~output agent 2 "No process to kill."))))
  (setf (agent~proc-id agent) nil))


;; ------------------------------------------------
;; other stuff
;; ------------------------------------------------


(defun agent~number-of-working-agents ()
  (declare (edited  "17-AUG-2000")
	   (authors Huebner)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The number of agents currently working on bb entries."))
  (let ((total-agents (+ (length (agents~get-parameter-agents))
			 (length (agents~get-command-agents)) 1))
	(active-agents 0))
    (mapcar #'(lambda (bb)
		(incf active-agents (length (bb~agents-done-p bb))))
	    (remove-if-not #'bb~command-blackboard-p (bb~get-blackboards)))
    (/ active-agents total-agents)))
  
		
    










		
    
	

