
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic reimplementation of the suggestion mechanism
;; using concurrent lisp facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Command Suggestion Module
;;
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The file contains the functionality to initialize, run and maintain
;; the multi-agent based suggestion mechanism.
;; It depends on all other files of the suggestion system (directly
;; or indirectly), i.e. it needs the modules:
;; process (PROC), suggestion (SUGG), foci (FOCI),
;; blackboard (BB), and agent (AGENT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :keim)

(mod~defmod CSM 
            :uses (agent bb com foci keim proc sugg)
            :documentation "The command suggestion mechanism."
            :exports (
                      
                      csm~active-p
                      csm~apply-conventional-defaults

                      csm~considered-classifiers
                      csm~considered-commands
                      csm~erase
                      csm~erase-command
                      csm~initialize
                      csm~quit
                      csm~reset-p
                      csm~resume
                      csm~set-considered-classifiers
                      csm~set-considered-commands
                      csm~set-default-heuristics
                      csm~start
                      csm~status
                      csm~suspend
                      
                      csm*active
                      csm*considered-classifiers
                      csm*considered-commands
                      csm*suggestion-agent

		      csm~add-agents))


#{
\section{Mechanism}
This module contains the functionality for the agent-based suggestion mechanism. It assembles the mechanism by using
the functionality of the preceding modules.
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar csm*suggestion-agent nil "Anchor for the suggestion agent.")

(defvar csm*active nil)

(defvar csm*considered-commands t
  "A list containing the command-blackboards with which the mechanism will be initialized. If the value is T all available command-blackboards are considered.")
(defvar csm*considered-classifiers t
  "A list containing the classifier agents that are considered for when mechanism will be initialized. If the value is T all available classifiers are considered.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of the Overall Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun csm~initialize (&optional (output-function #'csm=default-output))
  (declare (edited  "23-NOV-1999")
	   (authors Sorge)
	   (input   "An output function for the suggestion agent.")
	   (effect  "Newly initializes the suggestion mechanism.")
	   (value   "Undefined."))
  (sugg~output nil 3 "Initializing suggestion mechanism!")
  (sugg~start)
  (multiple-value-bind (coms blackboards command-agents)
      (csm~considered-commands)
    (let* ((classifiers  (remove-if #'agent~dummy-agent-p (csm~considered-classifiers)))   ;only one dummyagent MP!
	   (date (multiple-value-bind (sec min hour day month year)
		     (get-decoded-time)
		   (format nil "~A:~A:~A ~A.~A.~A" hour min sec day month year)))
	   (res-agent (agent~create-resource-agent "RESAGENT"))
	   (sugg-bb (bb~make-suggestion-blackboard :name "SuggBB"
						   :agents command-agents
						   :classifiers classifiers))
	   (dummy-agent (make-instance 'agent+dummy-agent :name "DUMMYAGENT")) 
	   (sugg-agent (agent~create-suggestion-agent
			"SuggAgent"
			sugg-bb
			(make-instance `rsrc+sugg-agent-resource :name "SuggAgent")
			nil
			output-function
			(format nil "Created at ~A" date)))
	   ;; initializing resources of the suggestion agent
	   (sum 0))
      (mapcar #'(lambda (x) (incf sum (rsrc~act-resources x)))
	      (rsrc~agent-data (bb~resources (agent~surveyed-bb sugg-agent))))
      (setf (rsrc~act-resources (agent~resources sugg-agent)) sum)
      (agent~change-log-flag sugg-agent)
      (sugg~reset-trace)
      (agent~register-new-agent sugg-agent)
      (agent~register-new-agent res-agent)
      (agent~register-new-agent dummy-agent)
      (setf (bb~surveyor sugg-bb) sugg-agent)
      (setf csm*suggestion-agent sugg-agent)
      (setf (bb~agents sugg-bb) command-agents)
      (dolist (agent classifiers)
	(setf (agent~blackboard agent) sugg-bb)
	(setf (agent~old-information agent) nil)
	(agent~initialize agent))
      (dolist (agent command-agents)
	(setf (agent~blackboard agent) sugg-bb)
	(agent~initialize agent))
      (dolist (bb blackboards)
	(bb~erase bb)
	(dolist (agent (bb~agents bb))
	  (agent~initialize agent)))
      (agent~initialize sugg-agent)
      (agent~initialize res-agent)
      (agent~initialize dummy-agent)
      (if (foci~active-pc)
	  (dolist (bb blackboards) (bb~reset bb))
	(sugg~output nil 1 "No active focus, mechanism not yet reset."))
      (setf csm*active t)
      (sugg~output nil 2 "Suggestion mechanism initialized for commands:~%~{~A ~}" coms)
    ;;;(setf (proc~resume-hook process) #'csm=first-listener-reschedule)
      (values)
      )))


;;; old but left for future reference...
(defun csm=first-listener-reschedule ()
  (sugg~output nil 2 "Reseting mechanism the first time")
  (sugg~reset)
  (setf (proc~resume-hook (proc~actual-process)) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling Functions for the Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun csm~start (&optional (output-function #'csm=default-output))
  ;;;(csm~set-default-heuristics)
  (csm~initialize output-function)
  )

(defun csm=suspend-process (proc)
  (sugg~output t 3 "Suspending process ~A" (proc~name proc))
  (proc~add-arrest-reason proc :suspended))

(defun csm~suspend ()
  (when (csm~active-p)
    (mapc #'csm=suspend-process agent*processes)))

(defun csm=resume-process (proc)
  (when (proc~arrest-reasons proc)
    (sugg~output t 3 "Resuming process ~A" (proc~name proc))
    (proc~revoke-arrest-reason proc :suspended)))

(defun csm~resume ()
  (when (csm~active-p)
    (mapc #'csm=resume-process agent*processes)))

(defun csm~quit ()
  (sugg~quit)
  (setf csm*active nil))

(defun csm~erase ()
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Erases all parts of the suggestion mechanism.")
	   (value   "None."))
  (sugg~output nil 3 "Erasing suggestion mechanism!")
  (agent~remove-all-agents)
  (bb~remove-all-blackboards)
  (setf csm*considered-commands t)
  (sugg~output nil 2 "Suggestion mechanism erased!")
  (values))

(defun csm~erase-command (command)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "A command or its name.")
	   (effect  "All agents and blackboards associated with this command are erased.")
	   (value   "T if successful, o/w NIL."))
  (let ((bb (bb~find-blackboard command))
	(success (and (agent~remove-agents-of-command command) (bb~remove-blackboard command))))
    (if success
	(progn
	  (csm=remove-considered-command bb)
	  (sugg~output nil 2 "Facilities for command ~A erased!" command))
      (sugg~output nil 1 "Something went wrong while erasing facilities for commmand ~A!" command))
    success))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying the Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General query functions
(defun csm~active-p ()
  csm*active)

(defun csm~reset-p ()
  (multiple-value-bind (coms bbs agents)
      (csm~considered-commands)
    (declare (ignore coms))
    (and
     (every #'(lambda (x) (= (bb~reset-state x) sugg*reset)) (csm=complete-bbs bbs))
     (every #'agent~check-reset-state (csm=complete-agents agents)))))

(defun csm=complete-bbs (bbs)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A list of command blackboards.")
	   (effect  "None.")
	   (value   "The list completed by the suggestion blackboard."))
  (let ((sugg (bb~find-blackboard 'suggbb)))
    (if sugg
	(cons sugg bbs)
      bbs)))

(defun csm=complete-agents (agents)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A list of command agents.")
	   (effect  "None.")
	   (value   "The list completed by all parameter agents associated with the commands and the suggestion agent."))
  (let ((sugg (agent~get-agent 'suggagent))
	(rest (apply #'append
		     (mapcar #'(lambda (agent)
				 (bb~agents (agent~surveyed-bb agent)))
			     agents))))
    (if sugg
	(cons sugg (append agents rest (csm~considered-classifiers)))
      (append agents rest (csm~considered-classifiers)))))

;;; General status information
(defun csm~status (&optional (silence nil))
  (declare (edited  "01-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Prints information on the activation status of the mechanism.")
	   (value   "T if the mechanism is completely active, o/w NIL."
		    "In case of T additional values specify the number of active processes."))
  (flet ((output (&rest args)
		 (unless silence (apply #'sugg~output args))))	  
  (if (not csm*suggestion-agent)
      (output nil 2 "Mechanism needs to be initialized. Suggestion agent not set.")
    (flet ((processes (agents)
		      (mapcar #'agent~process agents))
	   (active (agents)
		   (mapcar #'proc~is-active agents))
	   (count-nil (list)
		      (count-if #'null list))
	   (count-t (list)
		    (count-if-not #'null list)))
      (let* ((sugg-bb (agent~surveyed-bb csm*suggestion-agent))
	     (command-agents (bb~agents sugg-bb))
	     (classifier-agents (bb~classifier sugg-bb))
	     (parameter-agents (apply #'append
				      (mapcar #'(lambda (x) (bb~agents (agent~surveyed-bb x)))
					      command-agents)))
	     (sugg-proc (agent~process csm*suggestion-agent))
	     (com-proc (processes command-agents))
	     (cla-proc (processes classifier-agents))
	     (par-proc (processes parameter-agents)))
	(if (or (null sugg-proc) (some #'null com-proc)
		(some #'null cla-proc) (some #'null par-proc))
	    (progn
	      (output nil 2 "Mechanism needs to be initialized. Some agents are without process.")
	      (if (null sugg-proc)
		  (output nil 3 "Suggestion agent has no process.")
		(output nil 3 "Suggestion agent has a process."))
	      (output nil 3 "~A command agents are with process; ~A without."
			   (count-t com-proc) (count-nil com-proc))
	      (output nil 3 "~A classifying agents are with process; ~A without."
			   (count-t cla-proc) (count-nil cla-proc))
	      (output nil 3 "~A parameter agents are with process; ~A without."
			   (count-t par-proc) (count-nil par-proc)))
	  (let ((sugg-act (proc~is-active sugg-proc))
		(com-act (active com-proc))
		(cla-act (active cla-proc))
		(par-act (active par-proc)))
	    (cond ((and (null sugg-act) (every #'null com-act)
			(every #'null cla-act) (every #'null par-act))
		   (output nil 2 "Mechanism needs to be initialized. All ~A agents are inactive."
				(+ 1 (length com-act) (length cla-act) (length par-act)))
		   (output nil 3 "The Suggestion agent is inactive.")
		   (output nil 3 "~A command agents are inactive." (length com-act))
		   (output nil 3 "~A classifying agents are inactive." (length cla-act))
		   (output nil 3 "~A parameter agents are inactive." (length par-act)))
		  ((or (null sugg-act) (some #'null com-act)
		       (some #'null cla-act) (some #'null par-act))
		   (output nil 2 "Mechanism is in a weird state. Some agents are active, others are not.")
		   (if (null sugg-act)
		       (output nil 3 "Suggestion agent is not active.")
		     (output nil 3 "Suggestion agent is active."))
		   (output nil 3 "~A command agents are active; ~A are not."
				(count-t com-act) (count-nil com-act))
		   (output nil 3 "~A classifying agents are active; ~A are not."
				(count-t cla-act) (count-nil cla-act))
		   (output nil 3 "~A parameter agents are active; ~A are not."
				(count-t par-act) (count-nil par-act)))
		  (t
		   (let* ((com-act-num (count-t com-act))
			  (cla-act-num (count-t cla-act))
			  (par-act-num (count-t par-act)))
		     (output nil 2 "Mechanism is completely active with ~A agents."
				  (+ 1 com-act-num cla-act-num par-act-num))
		     (output nil 3 "Suggestion agent is active.")
		     (output nil 3 "~A command agents are active." com-act-num)
		     (output nil 3 "~A classifying agents are active." cla-act-num)
		     (output nil 3 "~A parameter agents are active." par-act-num)
		     (values t (+ 1 com-act-num cla-act-num par-act-num))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some default functions
(defun csm=default-output (suggestion)
  (format t "Suggestions: ~{       ~A~%~}~%" suggestion))

(defun csm~set-default-heuristics ()           ;;;  <----- might need modification VS.
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Sets some global variables to default values.")
	   (value   "None."))
  (bb~set-command-suggestion-ordering '(:rating :null-positions :position-weights))
  (bb~set-parameter-suggestion-ordering '(:status :length))
  (bb~set-command-suggestion-equality '(:command))
  (bb~set-parameter-suggestion-equality '(:mapping))
  (values))

;;; Considering only certain commands
(defun csm~considered-commands ()
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "Three values:"
		    "1. A list of commands that are considered for command suggestion."
		    "2. A list of blackboards corresponding to the commands."
		    "3. A list of command agents for the blackboards."))
  (if (listp csm*considered-commands)
      (values
       (mapcar #'bb~command csm*considered-commands)
       csm*considered-commands
       (mapcar #'bb~surveyor csm*considered-commands))
    (let* ((agents (agent~get-command-agents))
	   (bbs (mapcar #'agent~surveyed-bb agents))
	   (commands (mapcar #'bb~command bbs)))
      (values commands bbs agents))))

(defgeneric csm~set-considered-commands (commands)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A command or a list of commands or T.")
	   (effect  "Specifies which commands are to be considered for command suggestion by the suggestion mechanism.")
	   (value   "The list of considered commands."))
  (:method ((commands list))
	   (setf csm*considered-commands nil)
	   (dolist (com commands)
	     (csm=add-considered-command com))
	   csm*considered-commands)
  (:method ((commmands (eql t)))
	   (setf csm*considered-commands (remove-if-not #'bb~command-blackboard-p (bb~get-blackboards))))
  (:method ((commands null))
	   (setf csm*considered-commands nil))
  (:method (commands)
	   (declare (ignore commands))
	   (setf csm*considered-commands t)))

(defgeneric csm=add-considered-command (command)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A command, blackboard or agent.")
	   (effect  "The appropriate command blackboard is added to the list of considered commands.")
	   (value   "The added blackboard, if it exists. Otherwise NIL."))
  (:method ((bb bb+command-blackboard))
	   (if (listp csm*considered-commands)
	       (pushnew bb csm*considered-commands)
	     (setf csm*considered-commands (list bb))))
  (:method ((agent agent+command-agent))
	   (csm=add-considered-command (agent~surveyed-bb agent)))
  (:method ((command com+command))                                 ;;; <---   Keim specific !!!  VS
	   (csm=add-considered-command (bb~find-blackboard (keim~name command))))
  (:method ((name string))
	   (csm=add-considered-command (bb~find-blackboard name)))
  (:method ((name symbol))
	   (csm=add-considered-command (bb~find-blackboard name)))
  (:method ((nothing null)))
  (:method (something)
	   (declare (ignore something))))

(defgeneric csm=remove-considered-command (command)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A command, blackboard or agent.")
	   (effect  "The appropriate command blackboard is removed from the list of considered commands.")
	   (value   "The removed blackboard, if it exists. Otherwise NIL."))
  (:method ((bb bb+command-blackboard))
	   (when (listp csm*considered-commands)
	     (remove bb csm*considered-commands)))
  (:method ((agent agent+command-agent))
	   (csm=remove-considered-command (agent~surveyed-bb agent)))
  (:method ((command com+command))                                 ;;; <---   Keim specific !!!  VS
	   (csm=remove-considered-command (bb~find-blackboard (keim~name command))))
  (:method ((name string))
	   (csm=remove-considered-command (bb~find-blackboard name)))
  (:method ((name symbol))
	   (csm=remove-considered-command (bb~find-blackboard name)))
  (:method (something)
	   (declare (ignore something))))

;;; Considering only certain classifiers
(defun csm~considered-classifiers ()
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A list of classifiers that are considered for command suggestion."))
  (if (listp csm*considered-classifiers)
      csm*considered-classifiers
    (agent~get-classifiers)))

(defgeneric csm~set-considered-classifiers (classifiers)
  (declare (edited  "19-JAN-2000")
	   (authors Sorge)
	   (input   "A list of classifiers or T.")
	   (effect  "Specifies which classifiers are to be considered by the suggestion mechanism.")
	   (value   "The list of considered classifiers."))
  (:method ((classifiers list))
	   (setf csm*considered-classifiers
		 (remove-if #'null (mapcar #'agent~get-classifier classifiers))))
  (:method ((commmands (eql t)))
	   (setf csm*considered-classifiers (agent~get-classifiers)))
  (:method ((classifiers null))
	   (setf csm*considered-classifiers nil))
  (:method (classifiers)
	   (declare (ignore classifiers))
	   (setf csm*considered-classifiers t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the default function of the command module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun com~apply-defaults (command arglist)
  (if (csm~active-p)
      (let ((bb (bb~find-blackboard command)))
	(if bb
	    (bb~pop-entry bb)
	  (csm~apply-conventional-defaults command arglist)))
    (csm~apply-conventional-defaults command arglist)))

(defun csm~apply-conventional-defaults (command arglist)
  (let ((default-fn (com~default-fn command)))
    (if default-fn
	(apply default-fn arglist)
      arglist)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff for MIPPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun csm~add-agents (agents)
  (sugg~output nil 3 "Adding agents ~{~A~} to the running suggestion mechanism!" agents)
					;  (sugg~start)
  (multiple-value-bind (coms blackboards command-agents)
      (csm=get-com-bb-ag agents)
    (let* ((classifiers (remove-if #'agent~dummy-agent-p (csm~considered-classifiers)))
	   (date (multiple-value-bind (sec min hour day month year)
		     (get-decoded-time)
		   (format nil "~A:~A:~A ~A.~A.~A" hour min sec day month year)))
	   (sugg-bb (bb~find-blackboard "SuggBB"))
					;	   (dummy-agent (make-instance 'agent+dummy-agent :name "DUMMYAGENT")) 
	   (sugg-agent (bb~surveyor sugg-bb))
	   ;; initializing resources of the suggestion agent
	   (sum (rsrc~act-resources (car (rsrc~agent-data (bb~resources (agent~surveyed-bb sugg-agent))))))
	   )
      (mapc #'(lambda (x) (incf sum (rsrc~act-resources x)))
       	      (rsrc~agent-data (bb~resources (agent~surveyed-bb sugg-agent))))
      (setf (rsrc~act-resources (agent~resources sugg-agent)) sum)
      ;(agent~change-log-flag sugg-agent)zerop
      (sugg~reset-trace)
      ;(agent~register-new-agent sugg-agent)
      ;(agent~register-new-agent res-agent)
      ;(agent~register-new-agent dummy-agent)
      ;(setf (bb~surveyor sugg-bb) sugg-agent)
      ;(setf csm*suggestion-agent sugg-agent)
      (mapc  #'(lambda (ag) (push ag (bb~agents sugg-bb))) command-agents)
      ;(dolist (agent classifiers)
      ;(setf (agent~blackboard agent) sugg-bb)
      ;(setf (agent~old-information agent) nil)
      ;(agent~initialize agent))
      (dolist (agent command-agents)
	(setf (agent~blackboard agent) sugg-bb)
	(agent~initialize agent))
      (dolist (bb blackboards)
	(bb~erase bb)
	(dolist (agent (bb~agents bb))
	  (agent~initialize agent)))
;      (agent~initialize sugg-agent)
;      (agent~initialize res-agent)
;      (agent~initialize dummy-agent)
      (if (foci~active-pc)
	  (dolist (bb blackboards) (bb~reset bb))
	(sugg~output nil 1 "No active focus, mechanism not yet reset."))
      (setf csm*active t)
      (sugg~output nil 2 "Added the following commands:~%~{~A ~}" coms)
    ;;;(setf (proc~resume-hook process) #'csm=first-listener-reschedule)
      (values)
      )))



(defun  csm=get-com-bb-ag (agents)
  (declare (edited  "19-JAN-2000")
	   (input   "A list of agents")
	   (value   "Three values:"
		    "1. A list of commands that are considered for command suggestion."
		    "2. A list of blackboards corresponding to the commands."
		    "3. A list of command agents for the blackboards."))
  (let ((commands (mapcar #'agent~command agents))
	(bbs (mapcar #'agent~surveyed-bb agents)))
    (values     commands     bbs     agents)))
