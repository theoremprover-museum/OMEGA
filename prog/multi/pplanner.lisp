;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                         ;;
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

(in-package :omega)


;; THIS MODUL CONTAINS ALL THINGS FOR THE PARTIAL ORDER PLANNER PPLANNER OF MULTI

#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ------------------------------------------------------ Global Variables ----------------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#

(defvar pplan*verbose nil)
;; if set the whole system becomes a little bit verbose ...

(defvar pplan*very-verbose nil)
;; if set the whole system becomes very verbose ...

(defvar pplan*methods nil)
;; the methods which the planner is allowed to use currently 

(defvar pplan*restriction-methods nil)
;; the methods the planner can currently use for restricting the goal

(defvar pplan*normalization-methods nil)
;; the methods the planner can currently use for normalizing assumptions

(defvar pplan*crules nil)
;; the control rules the planner can currently use 

(defvar pplan*roc-state-description nil)
;; the current state description (continously maintained and updated)

(defvar pplan*normalize-goals-and-supps nil)
;; to store whether goals and assumptions are normalized 

(defvar pplan*loop-detection-depth 0)

(defvar pplan*update-agents 't)
;; global variable to determin whether the agents in the VIL mode should be updated or not!

(defvar pplan*check-backtrack 't)
;; global variable to determin whether during the method matching already applied matchings should be removed (t) or not (nil)
;; the default is t as needed for automatic search!
;; When using MULTI in VIL-MODE it is necessary to set it to nil

(defvar pplan*VIL-on nil)
;; global variable to determin whether VIL-mode is currently on or not (the point is that the button auto desactivates
;; the vil mode locally, whereas the button interactive reactivates it!

(defvar pplan*last-time nil)
;; a global variable to store (from time to time) the global time in!
;; Can be used to measure the time intervall between to plan steps
;; The difference is then added to the total time spend by the strategy!

(defvar pplan*models nil)
;; a global variable to store the relevant models
;; (Work of Seungyeob Choi)

(defvar pplan*fired-control-rules nil)
;; the control rules which did fire during the creation of a mmatching

(defvar pplan*user-comments nil)
;; the control rules which did fire during the creation of a mmatching


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ------------------------------------------- PPLANNER ALS REFINEMENT ALGORITHM ----------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#

;; In the following we define the functionalities of PPlanner as refinement algorithm.
;; That is we define a invokation and a reinvokation Funktion.


;; The invokation function
(defun pplan~pplanner-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using PPlanner as refinement algorithm) and the task.")
	   (effect  "PPlanner is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; new PPLANNER ROC-State-Description 
	 (new-pplanner-roc-state-description (roc~fresh-pplanner-state-description strategy-ks task nil parameters))
	 ;; New Start-strategy-ks Justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-pplanner-roc-state-description)))
	 ;; New Strategy-step from this justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))
    
    ;; Roc gets start-step as entry
    (setf (roc~start-step new-pplanner-roc-state-description) 
	  start-step)

    ;; ROC becomes an entry in ROCS-list 
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-pplanner-roc-state-description)
    
    ;; Strategy-step is added to Strategy-steps 
    (sod~introduce-new-strategy-ks-step! start-step)
    
    ;; If there are Randomization Rules a new random-seed is created and attached to the ROC
    (when (gethash 'randomization-rules (strat~strategy-ks-hash-table strategy-ks))
      (setf (roc~pplanner-random-seed new-pplanner-roc-state-description)
	    (make-random-state T)))
    
    (when pplan*verbose
      (omega~message "~%Invoking PPLANNER WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (pplan=calling-pplanner new-pplanner-roc-state-description)))


;; the Reinvokation function
(defun pplan~pplanner-reinvokation-function (roc-stades parameters)
  (declare (edited  "26-MAR-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for PPlaner (the topmost on the stack).")
	   (effect  "Planning with the strategy in the state-des (and what ever this will"
		    "cause)...")
	   (value   "Messages returned from the invokation of the strategy."))

  (let* (;; tasks with state-des
	 (tasks (remove-if-not #'(lambda (task)
				   (and (agenda~goal-or-goal-schema-task-p task)
					(find roc-stades (agenda~task-rocs task))))
			       (agenda~all-tasks (pds~agenda omega*current-proof-plan))))
	 ;; New Reinvoke-strategy-ks Justification
	 (new-reinvoke-strategy-ks-application-just (strat~reinvoke-strategy-ks-application-create-just (list roc-stades
													      (roc~copy-state-description roc-stades)
													      tasks)))
	 ;; New Strategy-step from this Justification
	 (reinvoke-step (scon~create-scon-step nil nil new-reinvoke-strategy-ks-application-just)))

    ;; when there are still demands for this roc-stades then delete them NOW!
    (when (null (roc~demands-are-fulfilled-p roc-stades))
      (pplan=delete-old-demands! roc-stades))
    
    (when parameters
      (setf (roc~parameters roc-stades) parameters))
    
    ;; Strategy-step is inserted in strategy-steps
    (sod~introduce-new-strategy-ks-step! reinvoke-step)
    
    (when pplan*verbose
      (omega~message "~%ReInvoking PPLANNER WITH ROC STATE Description ~A" roc-stades)
      (omega~message "~%DEMANDS IN ROC STATE Description are set to nil"))
    
    (setf (roc~demands roc-stades)
	  nil)
    
    (pplan=calling-pplanner roc-stades :reinvokation 't)))

(defun pplan=delete-old-demands! (roc-stades)
  (declare (edited  "28-JAN-2003")
	   (authors Ameier)
	   (input   "A roc with demands.")
	   (effect  "None.")
	   (value   "Deletes all demands from the blackboard and the roc."))
  (let* ((demands (roc~demands roc-stades)))

    (setf (roc~demands roc-stades) nil)
    
    (store~remove-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
			    demands)))
  

(defun pplan=calling-pplanner (roc &key (reinvokation nil))
  (declare (edited  "26-MAR-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for PPlanner.")
	   (effect  "Calls PPlanner on it (what ever this may cause) ...")
	   (value   "Messages returned from PPlanner."))
  
  (let* ((strategy-ks (roc~strategy-ks roc))
	 (strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 (meths (mapcar #'meth~find-method
			(gethash 'methods strat-parameter-hashtable)))
	 (restriction-meths (mapcar #'meth~find-method
				    (gethash 'restriction-methods strat-parameter-hashtable)))
	 (normalization-meths (mapcar #'meth~find-method
				      (gethash 'normalization-methods strat-parameter-hashtable)))
	 (crules (gethash 'control-rules strat-parameter-hashtable))
	 (randomization-rules (mapcar #'rand~find-rand-rule
				      (gethash 'randomization-rules strat-parameter-hashtable)))
	 (loop-detection-depth (gethash 'loop-detection strat-parameter-hashtable)))

    (setf pplan*last-time (get-universal-time))
    
    ;; Setting Methods + Control Rules + Randomization Rules
    (setf pplan*methods meths)
    (setf pplan*restriction-methods restriction-meths)
    (setf pplan*normalization-methods normalization-meths)
    (setf pplan*crules crules)
    (setf rand*current-rand-rules randomization-rules)

    ;; If there are randomization rules the *random-state* variable is set to the random seed of the ROC
    (when randomization-rules
      (setq *random-state* (roc~pplanner-random-seed roc)))
    
    ;; resetting of the Control-Rules for CRI
    (cri~remove-all-used-control-rules)
    (cri~set-used-control-rules! crules)

    ;; seting of the current ROC state-description 
    (setf pplan*roc-state-description roc)
    
    ;; How deep should the planner search for Loops?
    (setf pplan*loop-detection-depth loop-detection-depth)

    ;; Should restriction and normalization be applied?
    (setf pplan*normalize-goals-and-supps (if (or pplan*restriction-methods pplan*normalization-methods) 't nil))

    ;; Normalizing + Restriction
    (when pplan*normalize-goals-and-supps
      (pplan=restrict-and-normalize-at-beginning :reinvokation reinvokation))

    ;; CALLING THE MAIN PLANNER
    (when pplan*verbose
      (omega~message "~%~%CALLING PPLANNER!"))
    (when (and sod*VIL
	       pplan*VIL-on)
      (pplan=initialize-VIL!))
    
    (let* ((execution-message (catch 'leave-pplanner
				(if (and sod*VIL
					 pplan*VIL-on)
				    (pplan~plan-VIL)
				  (pplan~plan)))))
      
      (when pplan*verbose
	(omega~message "~%Receiving the following message from termination of PPLANNER: ~A" execution-message))
      
      ;; THE EXECUTION MESSAGE IS RETURNED
      execution-message
      )))

(defun pplan=initialize-VIL! ()
  (declare (edited  "09-AUG-2000")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Initializes the agents for VIL-MODE.")
	   (value   "UNdefined."))
  (let* ((method-names (mapcar #'keim~name pplan*methods))
	 (strat-names  (mapcar #'keim~name strat*current-strategy-ks))
	 (blackboards (apply #'append (mapcar #'(lambda (name)
						  (let* ((blacki (bb~find-blackboard name)))
						    (if blacki
							(list blacki)
						      nil)))
					      (append method-names strat-names))))
	 (commands (mapcar #'bb~command blackboards)))
    (progn
      (oc=use-standard-heuristics)
      (setf foci*in-use t)
      (csm~set-considered-commands commands)
      (csm~start
       #'(lambda (x)
	   (mapc #'(lambda (whatandlist)
		     (socket~write (format nil "updateInterActive(~A ~A)"
					   (first whatandlist)
					   (rest whatandlist))
				   :inout))
		 (list (cons "actions"
			     (parse~list-of-strings (mapcar #'(lambda (entry)
					 (keim~name (bb~entry-command entry)))
							    x)))
		       (cons "goals"
			     (format nil "[~{~A~}]" (mapcar #'(lambda (foc)
					 (parse~list-of-strings (cons (keim~name (foci~focus-line foc))
								      (mapcar #'keim~name (foci~supports foc)))))
				     (cons (foci~active-pc)
					   (foci~passive-pcs)))))
		       (cons "variables"
			     (parse~list-of-strings (mapcar #'(lambda (inst-task)
					 (agenda~inst-task-meta-var inst-task))
				     (remove-if-not #'agenda~inst-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan))))))
;                       (cons "backtrack"
;                              (parse~list-of-strings
;                               (append (remove
;                                        (keim~name (prob~proof-root omega*current-proof-plan))
;                                        (mapcar #'(lambda (foc)
;                                                    (keim~name (foci~focus-line foc)))
;                                                (cons (foci~active-pc)
;                                                      (foci~passive-pcs))))
;                                       (let* ((cstr-pool (pds~constraint-pool omega*current-proof-plan)))
;                                         (if (and cstr-pool
;                                                  (pds~cstrpool-bindings cstr-pool))
;                                             (subst~domain (pds~cstrpool-bindings cstr-pool))
;                                           nil))
;                                       (if (pds~last-plan-step omega*current-proof-plan)
;                                           (list "OPEN")
;                                         nil))))
                      (cons "backtrack"
                            (format nil "[~{~A~}~{~A~}~{~A~}]"
                                    (remove
                                     (keim~name (prob~proof-root omega*current-proof-plan))
                                     (mapcar #'(lambda (foc)
                                                 (parse~string (keim~name (foci~focus-line foc))))
                                             (cons (foci~active-pc)
                                                   (foci~passive-pcs))))
                                    (let* ((cstr-pool (pds~constraint-pool omega*current-proof-plan)))
                                      (if (and cstr-pool
                                               (pds~cstrpool-bindings cstr-pool))
                                          (mapcar #'(lambda (x y) (parse~list-of-strings (list (keim~name x)(format nil "~A" y))))
                                                  (subst~domain (pds~cstrpool-bindings cstr-pool))
                                                  (subst~codomain (pds~cstrpool-bindings cstr-pool)))
                                        nil))
                                    (if (pds~last-plan-step omega*current-proof-plan)
                                        (list (parse~string "OPEN"))
                                      nil)))
		       )))))))

#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| -------------------------------------------------- Main Planning Algorithm -------------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#

;;
;; Here starts the Main Planning Algorithm of PPlanner!
;; In this moment the following global variables are already set:
;; 1.) omega*current-proof-plan    -> current PDS, which is also the current pds on the solution blackboard
;; 2.) pplan*methods               -> current methods
;; 3.) pplan*crules                -> current control-rules
;; 4.) pplan*roc-state-description -> ROC state-description of the currently running process
;;
;;
;; The top-level planning algorithm looks as follows:
;; 1.) If the termination condition is satisfied then stop with execution message: proper-termination.
;; 2.) If an interruption control-rule fires, then stop with the execution message produced by the control-rule
;; 3.) If there a no more local goal (marked with the current ROC) then stop with execution message: proper-termination
;;     (since then the local problem is solved)
;; 4.) If there are local goals but none of these goals are first; that is, other goals are first and bloack the local goals,
;;     then compute these goals which block the local goals and stop with an execution messahe that demands to tackle these
;;     other goals first.
;; 5.) If the are local goals which are first -> Plan a step
;;            -> If this was successful start again at 1.)
;;            -> Otherwise stop with a failure  execution message



(defun pplan~plan ()
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Plans with the possible methods (in pplan*methods) and crules (in pplan*crules) on the"
		    "tasks marked with the current ROC (in pplan*roc-state-description)."
		    "Thereby the PDS will be changed.")
	   (value   "An execution message."))

  (setf pplan*check-backtrack 't) ;; automatic search -> set backtrack on
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (strategy-ks (roc~strategy-ks pplan*roc-state-description))
	 (termination-check (gethash 'termination-check (strat~strategy-ks-hash-table strategy-ks)))
	 )

    ;; UPDATETING the run-time of the roc:
    (setf (roc~pplanner-run-time pplan*roc-state-description)
	  (+ (roc~pplanner-run-time pplan*roc-state-description)
	     (- (get-universal-time) pplan*last-time)))
    (setf pplan*last-time (get-universal-time))

    (when pplan*verbose
      (omega~message "~%~%Time spend by strategy is: ~A" (roc~pplanner-run-time pplan*roc-state-description)))
    
    (multiple-value-bind
	(free-tasks unblocked-tasks)
	(if (agenda~empty-p agenda)
	    (values nil nil)
	  (pds~first-tasks! pds agenda))
      ;; Note: pds~first-tasks! does already update the current-formulas in the tasks!
      ;;       Hence, no further extra up-to-date necessary for the tasks!
      
      (multiple-value-bind
	  (result applied-crules)
	  (cri~call '(interrupt)
		    :kind 'strategy-interruption
		    :tasks  (if (agenda~empty-p agenda)
				nil
			      (remove-if-not #'(lambda (task)
						 (find pplan*roc-state-description (agenda~task-rocs task)))
					     (append free-tasks unblocked-tasks)))
		    :pds pds)
	
	(let* ((first-tasks (append free-tasks unblocked-tasks))
	       (termination-check-results (apply termination-check nil)))
	  
	  (cond (applied-crules
		 
		 ;; A strategy-interuption rule fired -> Interruption

		 (let* ((cleared-result (remove-if #'(lambda (item)
						       (and (symbolp item)
							    (string-equal item 'interrupt)))
						   result))
			(demands (mapcar #'(lambda (uninterpreted-demand)
					     (let* ((strategy-ks-entry (first uninterpreted-demand))
						    (strategy-ks (if strategy-ks-entry
								     (strat~find-strategy-ks strategy-ks-entry)
								   nil))
						    (task-entry (second uninterpreted-demand))
						    (parameters (if (listp (third uninterpreted-demand))
								    (third uninterpreted-demand)
								  (list (third uninterpreted-demand))))
						    (new-demand 
						     (cond ((and strategy-ks-entry task-entry)
							    (demand~create-strategy-task-demand strategy-ks task-entry parameters))
							   ((and (null strategy-ks-entry) task-entry)
							    (demand~create-task-demand task-entry parameters))
							   (t
							    (omega~error "Demand not correctly defined.")))))
					       
					       new-demand))
					 cleared-result))
			(new-interup-exmes (exmes~create-interruption-message pplan*roc-state-description demands applied-crules)))
		   
		   (when pplan*verbose
		     (omega~message "**PPLANNER**: Termination because of interruption by control rules => Interrupted Termination with demands ~A"
				    new-interup-exmes))
		   new-interup-exmes))
		
		(termination-check-results 
		 
		 ;; If the termination-check of the strategy-ks is satisfied:
		 ;; -> create proper-termination execution message
		 
		 (let* ((demands (if (listp termination-check-results)
				     termination-check-results
				   nil)))
		   
		   (when pplan*verbose
		     (omega~message "**PPLANNER**: Termination by strategy-termination-predicat => Proper Termination with additional demands ~A" demands))
		   
		   (exmes~create-termination-message pplan*roc-state-description
						     demands)))
		
		((null (find pplan*roc-state-description (agenda~all-tasks agenda)
			     :test #'(lambda (roc task)
				       (and (find roc (agenda~task-rocs task))
					    (agenda~goal-or-goal-schema-task-p task)))))
		 
		 ;; If there are no local goals (no goals marked with the current ROC)
		 ;; -> all local goals closed 
		 ;; -> create proper-termination execution message
		 
		 (when pplan*verbose
		   (omega~message "**PPLANNER**: Termination because no further local-goals => Proper Termination with no Demands!"))
		 
		 (exmes~create-termination-message pplan*roc-state-description
						   nil))
		
		(;; after the last case there are still local goal-tasks (tasks with the current roc)
		 (null (find pplan*roc-state-description first-tasks
			     :test #'(lambda (roc task)
				       (and (find roc (agenda~task-rocs task))
					    (agenda~goal-or-goal-schema-task-p task)))))
		 
		 ;; -> There are no tasks with the current ROC among the first tasks
		 ;; -> interrupt and demand that local tasks get unblocked
		 
		 (let* ((demands (mapcar #'(lambda (task)
					     (demand~create-task-demand task nil))
					 first-tasks)))
		   
		   (when pplan*verbose
		     (omega~message "**PPLANNER**: Termination because of ordering demands => Interrupted Termination with demands ~A"
				    demands))
		   
		   (exmes~create-interruption-message pplan*roc-state-description
						      demands
						      nil)))
		
	        (;; continue in interactive mode ...
		 (and sod*VIL (pplan=interactive-mode? strategy-ks)) 
		 (setf pplan*VIL-on 't)
		 (csm~resume)
		 (foci~compute-pcs :pds pds)
		 (sugg~reset)
		 (pplan~plan-VIL))

		(t
		 
		 (when pplan*verbose
		   (omega~message "**PPLANNER**: Continue to plan ..."))
		 
		 ;; there are local tasks (with current roc) among the first tasks

		 ;; TRY TO PLAN A STEP!!
		 (let* ((success (pplan=plan-step pds agenda)))
		   
		   (opr~signal-interrupt success)                ;;; Possible interrupt point for infinite loops
		   (cond ((or (agenda~p success)
			      (agenda~empty-p success))
			  
			  (when pplan*verbose
			    (omega~message "**PPLANNER**: Sucessfully applied a Method, new resulting agenda: ~A!" success))
			  
			  ;; Application of plan-step was successful -> New agenda for PDS
			  (setf (pds~agenda omega*current-proof-plan) success)
			  
			  (when inac*strategy-ks-execution-interactive
			    (omega~message "~%~%**INTERACTIVE**")
			    (omega~message "~%Type STEP to continue")
			    (inac~stepper))

			  (pplan~plan))
			 
			 (t
			  
			  (when pplan*verbose
			    (omega~message "**PPLANNER**: Found no applicable method for TASK ~A" success))

			  ;; A task was returned. That is, a task on which no method could be applied!
			  ;; -> return failure message 
			  ;; (in early years a backtrack-demand was produced directly here, but meanwhile this is handled
			  ;;  differently)
			  
			  (let* ((report (list 'pplanner 'no-applicable-method success))
				 )
			    
			    (when pplan*verbose
			      (omega~message "**PPLANNER**: Termination because a task ~A was found on which no method of this strategy is applicable -> Failed interruption"
					     success))

			    ;; On the task-node is marked that the current strategy failed already on it!
			    ;; this forbids an invokation of the strategy on this task!
			    ;; But it does not forbis a reinvokation of the strategy (even on this task).
			    (setf (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node success))))
				  (remove-duplicates (cons (list (roc~strategy-ks pplan*roc-state-description)
								 (roc~parameters pplan*roc-state-description))
							   (keim::pdsc~already-applied-strategy-ks
							    (pdsj~control (node~justification (agenda~task-node success)))))
						     :test #'keim~equal))
			    
			    (exmes~create-failure-message pplan*roc-state-description
							  report)))
			 )))))))))

(defun pplan=plan-step (pds agenda)
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A pds and an Agenda.")
	   (effect  "Searches for an applicable method and applies it." )
	   (value   "The new agenda if there was applied a method, or if not, the task for which it was not"
		    "possible to apply a method (to pose back-track demands)."))

  (when (socket~find-socket :Service)
    (maple~leave)
    (maple~enter))
  
  (when pplan*verbose
    (omega~message "**PLAN-STEP**: Planning a step ..."))

  ;; (setq glo*glo agenda)
  ;; Searching for the next Matching to be applied ...
  (multiple-value-bind
      (task mmatching)
      (pplan=choose-task-and-mmatching pds agenda)
    
    (if (null mmatching)

	;; if no matching was found, the task is returned!
	task
      
      (let* ((task-node (agenda~task-node task))
	     (node-formula (pds~task-formula task pds))
	     (node-supports (pds~node-supports task-node pds)))

	(when pplan*verbose
	  (omega~message "**PLAN-STEP**: Selected matching: ~A, Applying now this matching." mmatching))

	(multiple-value-bind
	    (new-agenda new-opens new-supps)
	    (pplan=apply-plan-step! task mmatching pds agenda)

	  (if pplan*normalize-goals-and-supps
	      (pplan=restrict-and-normalize-during-planning
	       new-agenda
	       pds
	       (if (pdsn~open-node-p task-node)
		   (cons task-node new-opens)   ;; if task still open -> normalise with respect to the new supports
		 new-opens)                     ;; otherwise restrict only the new-opens
	       new-supps)
	    new-agenda))))))

(defun pplan=update-models! (new-hyps)
  (declare (edited  "02-AUG-2001")
	   (authors Ameier)
	   (input   "A list of new hypothesis nodes.")
	   (effect  "The currently relevant models in pplan*models are changed.")
	   (value   "Undefined."))

  ;; This function is a connection point with the work of Seungyeob Choi
  ;; he will fill in its own stuff, when finished we will integrate it.  
  nil)

(defun pplan=apply-plan-step! (task mmatching pds agenda &key (kind 'standard) (normalizing-tasks nil))
  (declare (edited  "16-AUG-1999")
	   (authors Ameier)
	   (input   "A task, a mmatching, a pds and an agenda.")
	   (effect  "Applies the mmatching.")
	   (value   "Multiple-Value:"
		    "First: A new agenda."
		    "Second: the new open-lines."
		    "Third: The new support-lines."))
  
  ;; if matching was found:
  ;; 1.) Apply Matching!
  ;;     This executes Method, creates new lines, inserts new-lines, sets last-plan-step,
  ;;     sets constraint-store, supports and open-lines etc.
  
  (multiple-value-bind
      (mmatching new-opens new-supps additional-returns)
      (pplan=apply-mmatching! task mmatching pds :kind kind :normalizing-tasks normalizing-tasks)

    ;; update the models after the application of an mmatching
    (pplan=update-models! (remove-if-not #'pdsn~hypothesis-node-p new-supps))
    
    (let* ((applied-method (pplan~matched-method mmatching))
	   (resulted-cstrpool (pplan=mmatch-cstr-state mmatching))
	   (new-agenda 
	    ;; carries out the remaining things such as setiing the up-to-date flags in the nodes, creating the new tasks
	    ;; creating the new agenda etc.
	    (cond ((or (equal kind 'standard)
		       (equal kind 'restricting))
		   (if (eq (agenda~task-node task) (pplan~mmatch-goal mmatching))
		       ;; TASK was solved by a backward method
		       (pplan=finish-application-of-backward-method! task applied-method mmatching
								     pds agenda resulted-cstrpool
								     new-opens new-supps)
		     
		     ;; Forward Method was applied!
		     (pplan=finish-application-forward-method! task applied-method mmatching
							       pds agenda resulted-cstrpool
							       new-opens new-supps)))
		  (t
		   ;; normalizing is applied
		   (pplan=finish-application-forward-normalizing-method! applied-method mmatching
									 pds agenda resulted-cstrpool
									 new-opens new-supps)))))
      
      (values
       (pplan=remove-inst-tasks new-agenda resulted-cstrpool)
       new-opens
       new-supps))))

(defun pplan=remove-inst-tasks (agenda resulted-cstrpool)
  (declare (edited  "28-APR-2000")
	   (authors Ameier)
	   (input   "The agenda and the constraint pool resulting from the application of a method"
		    "(the constraint pool is nil if this step does not change the constraint pool).")
	   (effect  "None.")
	   (value   "The set S of meta-variables which are instantiated within this step is computed."
		    "An agenda is returned in which the insttasks to these meta-variables are removed."))
  (let* ((previous-cstrpool (if resulted-cstrpool
				(pds~cstrpool-previous resulted-cstrpool)
			      nil)))
    
    (if (and resulted-cstrpool
	     (pds~cstrpool-bindings resulted-cstrpool)
	     (or (null previous-cstrpool)
		 (not (eq (pds~cstrpool-bindings resulted-cstrpool)
			  (pds~cstrpool-bindings previous-cstrpool)))))
	;; The resulted-cstrpool exists and the last step introduces new bindings!
	(let* ((curr-bindings (pds~cstrpool-bindings resulted-cstrpool))
	       (prev-bindings (if previous-cstrpool
				  (pds~cstrpool-bindings previous-cstrpool)
				nil))
	       (curr-mvs (subst~domain curr-bindings))
	       (prev-mvs (if prev-bindings
			     (subst~domain prev-bindings)
			   nil))
	       (new-mvs (set-difference curr-mvs prev-mvs))
	       ;; Variable new-mvs contains the set of all meta-variables which are bound within the application of the last method
	       (inst-tasks-to-new-mvs (remove-if-not #'(lambda (task)
							 (and (agenda~inst-task-p task)
							      (find (agenda~inst-task-meta-var task) new-mvs)))
						     (agenda~all-tasks agenda)))
	       ;; Variable inst-tasks-to-new-mvs contains the Insttasks to the meta-variables in new-mvs
	       )

	  ;;(format t "~%FOUND THE FOLLOWING NEW BOUNDED MVS: ~A" new-mvs)
	  ;;(format t "~%CORRESPONDING INST-TASKS ARE REMOVED: ~A" inst-tasks-to-new-mvs)
	  
	  (do* ((rest-inst-tasks inst-tasks-to-new-mvs (rest rest-inst-tasks))
		(new-agenda agenda))
	      ((null rest-inst-tasks)
	       new-agenda)
	    (setf new-agenda (agenda~replace-task (first rest-inst-tasks) nil nil nil new-agenda))))
      agenda)))	  
    
(defun pplan=finish-application-forward-normalizing-method! (applied-method mmatching pds agenda resulted-cstrpool new-opens new-supps)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The applied method, the corresponding mmatching, the pds, the current agenda,"
		    "the resulted cstrpool, the new-opens, the new-supps.")
	   (effect  "The up-to-date falgs of the goals and supports are updated (are set to nil if meta-variables"
		    "were bound during application of method), the new tasks arising by method application are"
		    "created. The ROCS from the old task are inherited to the new tasks. Then a new agenda is"
		    "produced with this new tasks.")
	   (value   "The new agenda."))

  (if new-opens
      (omega~error "~%In function pplan=finish-application-forward-normalizing-method! the application of normalizing method ~A creates new open nodes ~A. It is not allowed for a normalizing method to create new open-nodes!"
		   applied-method
		   new-opens)

    (progn
      
      ;; 1. If some meta-variables are bound in this planning step:
      ;;    -> updaten der schematic-nodes in der PDS
      (when (and resulted-cstrpool
		 (pds~cstrpool-bindings resulted-cstrpool)
		 (or (null (pds~cstrpool-previous resulted-cstrpool))
		     (not (eq (pds~cstrpool-bindings resulted-cstrpool)
			      (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))))
	
	(multiple-value-bind
	    (previously-clsd-goals newly-clsd-goals)
	    (pds~update-schematic-nodes! pds)))
      
      ;; 2. Carry out the matched method actions.
      (pplan=execute-outline-actions (find-if-not #'pdsn~hypothesis-p new-supps) new-opens mmatching pds)
      
      ;; 3.) Create new tasks: New Expansion Tasks + new INST-TASKS
      (let* ((new-exp-tasks (if (meth~non-reliability applied-method)
				(mapcar #'agenda~create-pseudo-goal
					(remove-if #'pdsn~hypotheses-p new-supps))
	      		      nil))
	     (new-inst-tasks (pplan=new-inst-tasks agenda pds resulted-cstrpool new-supps))
	     (new-tasks (append new-exp-tasks new-inst-tasks)))
	
	;; ATTENTION: normalizing steps are entried in NO ROCS!
	
	;; 4.) Insert the NEW-TASKS
	(multiple-value-bind
	    (first-task orderings)
	    (pplan=create-agenda-orderings (meth~outline-orderings applied-method)
					   (pplan~mmatch-mmapp mmatching)
					   (remove-if-not #'agenda~goal-or-goal-schema-task-p new-tasks))
	  (let* ((new-agenda (agenda~insert-tasks nil 
						  first-task
						  (remove first-task new-tasks)  ;; or simply only new-tasks?
						  orderings
						  agenda
						  :where 'before)))
	    
	    ;; (format t "~% THE NEW AGENDA has TASKS: ~A" (agenda~all-tasks new-agenda))
	    new-agenda))))))
  

(defun pplan=finish-application-forward-method! (task applied-method mmatching pds agenda resulted-cstrpool new-opens new-supps)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "A task, the applied method, the corresponding mmatching, the pds, the current agenda,"
		    "the resulted cstrpool, the new-opens, the new-supps.")
	   (effect  "The up-to-date falgs of the goals and supports are updated (are set to nil if meta-variables"
		    "were bound during application of method), the new tasks arising by method application are"
		    "created. The ROCS from the old task are inherited to the new tasks. Then a new agenda is"
		    "produced with this new tasks.")
	   (value   "The new agenda."))

  (let* ((task-node (agenda~task-node task)))

    ;; 1. If some meta-variables are bound in this planning step:
    ;;    -> updaten der schematic-nodes in der PDS
    (when (and resulted-cstrpool
	       (pds~cstrpool-bindings resulted-cstrpool)
	       (or (null (pds~cstrpool-previous resulted-cstrpool))
		   (not (eq (pds~cstrpool-bindings resulted-cstrpool)
			    (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))))
      
      (multiple-value-bind
	  (previously-clsd-goals newly-clsd-goals)
	  (pds~update-schematic-nodes! pds)))
    
    ;; 2. Carry out the matched method actions.
    (pplan=execute-outline-actions (find-if-not #'pdsn~hypothesis-p new-supps) new-opens mmatching pds)
    
    ;; 3.) Create new tasks: new gaol tasks + new expansion tasks + new inst-tasks
    (let* ((new-goal-tasks (mapcar #'pplan=create-goal-task new-opens))
	   (new-exp-tasks (if (meth~non-reliability applied-method)
			      ;; METHOD is non-reliable, we have to consider
			      ;; a new pseudo tasks for the new supps:
			      (mapcar #'agenda~create-pseudo-goal
				      (remove-if #'pdsn~hypothesis-p new-supps))
			    nil))
	   (new-inst-tasks (pplan=new-inst-tasks agenda pds resulted-cstrpool (append new-opens new-supps)))
	   (new-tasks (append new-goal-tasks new-exp-tasks new-inst-tasks)))
      
      ;; 4.) Inherit the ROCS of task to the new tasks
      (mapcar #'(lambda (new-task)
		  (pplan=inherit-rocs! task new-task))
	      new-tasks)

      ;; 5.) The PPlanner-Rocs get updated!
      (pplan=update-rocs-with-new-lines-forward! task new-opens new-supps)
      
      ;; 6.) Replace TASK by the NEW-TASKS
      (multiple-value-bind
	  (first-task orderings)
	  (pplan=create-agenda-orderings (meth~outline-orderings applied-method)
					 (pplan~mmatch-mmapp mmatching)
					 (remove-if-not #'agenda~goal-or-goal-schema-task-p new-tasks))
	(let* ((new-agenda (agenda~insert-tasks task 
						first-task
						(remove first-task new-tasks)  ;; oder einfach nur new-tasks?
						orderings
						agenda
						:where 'before)))
	  
	  ;; (format t "~% THE NEW AGENDA has TASKS: ~A" (agenda~all-tasks new-agenda))
	  new-agenda)))))


(defun pplan=finish-application-of-backward-method! (task applied-method mmatching pds agenda resulted-cstrpool new-opens new-supps)    
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "A task, the applied method, the corresponding mmatching, the pds, the current agenda,"
		    "the resulted cstrpool, the new-opens, the new-supps.")
	   (effect  "The up-to-date falgs of the goals and supports are updated (are set to nil if meta-variables"
		    "were bound during application of method), the new tasks arising by method application are"
		    "created. The ROCS from the old task are inherited to the new tasks. then a new agenda is"
		    "produced with this new tasks.")
	   (value   "The new agenda."))
  
  (let* ((task-node (agenda~task-node task)))

    ;; 1. If some meta-variables are bound in this planning step:
    ;;    -> updaten der schematic-nodes in der PDS
    (when (and resulted-cstrpool
	       (pds~cstrpool-bindings resulted-cstrpool)
	       (or (null (pds~cstrpool-previous resulted-cstrpool))
		   (not (eq (pds~cstrpool-bindings resulted-cstrpool)
			    (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))))
      
      (multiple-value-bind
	  (previously-clsd-goals newly-clsd-goals)
	  (pds~update-schematic-nodes! pds task new-opens)))
    
    ;; 2. Carry out the method actions first, because these can make some
    ;; changes to the supports of the involved nodes. For instance, by applying
    ;; a method M we get a new hypothesis (H1 (and a b)) which occurs in a subgoal
    ;; Li, hence during the application of M H1 is added to the supports of Li.
    ;; Suppose M has the outline actions [Li (unsponsor H1) (sponsor Lj)] where
    ;; Lj corresponds to (Lj (H1) a ("AndEL" () (H1))). To prevent that H1 will
    ;; be splitted into (Ln a) and (Ln+1 b) and these are added as supports to
    ;; Li, we have to carry out the actions of M before normalizing the new
    ;; closed nodes and we must prohibit to normalize new closed nodes which does 
    ;; not support any node.
    
    (pplan=execute-outline-actions task-node new-opens mmatching pds)
    
    ;; 3.) Create new TAKS: NEW GOAL TASKS, NEW EXPANDIERUNGS-TASKS, NEW INST-TASKS
    (let* ((new-goal-tasks (mapcar #'pplan=create-goal-task new-opens))
	   (new-exp-tasks (if (meth~non-reliability applied-method)
			      ;; METHOD is non-reliable, we have to consider
			      ;; a new pseudo task for TASK-NODE:
			      (list (agenda~create-pseudo-goal task-node)) 
			    nil))
	   (new-inst-tasks (pplan=new-inst-tasks agenda pds resulted-cstrpool (append new-opens new-supps)))
	   (new-tasks (append new-goal-tasks new-exp-tasks new-inst-tasks)))
      
      ;; 4.) Inherit the ROCS of task to the new tasks
      (mapcar #'(lambda (new-task)
		  (pplan=inherit-rocs! task new-task))
	      new-tasks)

      ;; 5.) The PPLANNER ROCS get updated!
      (pplan=update-rocs-with-new-lines-backward! task new-opens new-supps)
      
      ;; 6.) Replace TASK by the NEW-TASKS
      (multiple-value-bind
	  (first-task orderings)
	  (pplan=create-agenda-orderings (meth~outline-orderings applied-method)
					 (pplan~mmatch-mmapp mmatching)
					 (remove-if-not #'agenda~goal-or-goal-schema-task-p new-tasks))
	(agenda~replace-task task
			     first-task
			     (remove first-task new-tasks)
			     orderings
			     agenda
			     :where 'before)))))


(defun pplan=create-agenda-orderings (meth-orderings mmapp tasks)
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "Method orderings, a binding of the method meta-variables,"
		    "and a list of tasks which can be referenced by the"
		    "METH-ORDERINGS.")
	   (effect  "Signals an error when the method orderings are inconsistent.")
	   (value   "A pair: the first task, and a list of agenda orderings when"
		    "the method orderings are consistent."))
  (when meth-orderings
    (multiple-value-bind (first-task agenda-orderings)
	(pplan=create-agenda-orderings (rest meth-orderings) mmapp tasks)
      (let* ((meth-ordering1 (first meth-orderings))
	     (key-word (keim~name meth-ordering1))
	     (meth-nodes (meth~ordering-ordered-nodes meth-ordering1)))
	(cond ((string-equal key-word 'first)
	       (let* ((meth-var (if (meth~node-p (first meth-nodes)) (keim~name (first meth-nodes))
				  (first meth-nodes)))
		      (pds-node (meth~mapp-get-component meth-var mmapp :mapp)))
		 (if pds-node
		     (let ((the-task (find-if #'(lambda (task) (eq (agenda~task-node task) pds-node))
					      tasks)))
		       (if the-task
			   (if (or (null first-task) (eq the-task first-task))
			       (values the-task agenda-orderings)
			     (return-from pplan=create-agenda-orderings
			       (omega~error ";;; Specifying two different tasks ~A and ~A to be considered first!"
					    the-task first-task)))
			 (values first-task agenda-orderings)))
		   (return-from pplan=create-agenda-orderings
		     (omega~error ";;; No associated pds node for ~A in ~A" (first meth-nodes) mmapp)))))
	      ((string-equal key-word 'before)
	       (let* ((meth-vars (mapcar #'(lambda (mnode) (if (meth~node-p mnode) (keim~name mnode)
							     mnode))
					 meth-nodes))
		      (pds-nodes (meth~mapp-get-component meth-vars mmapp :mapp)))
		 (if (some #'null pds-nodes)
		     (return-from pplan=create-agenda-orderings
		       (omega~error ";;; No associated pds nodes for ~A in ~A" meth-nodes mmapp))
		   (let ((the-tasks
			  (mapcar #'(lambda (x)
				      (if (listp x)
					  (remove-if
					   #'null
					   (mapcar #'(lambda (y)
						       (find-if #'(lambda (task)
								    (eq (agenda~task-node task) y))
							       tasks))
						   x))
					(find-if #'(lambda (task) (eq (agenda~task-node task) x))
						 tasks)))
				  pds-nodes)))
		     (if (some #'null the-tasks)
			 (values first-task agenda-orderings)
		       (values first-task
			       (let ((tasks1 (first the-tasks))
				     (tasks2 (second the-tasks)))
				 (cons (agenda~ordering-create
					(if (listp tasks1) tasks1 (list tasks1))
					(if (listp tasks2) tasks2 (list tasks2)))
				       agenda-orderings))))))))
	      (T
	       (return-from pplan=create-agenda-orderings
		 (omega~error ";;; Unkown ordering key word ~A!" key-word))))
	))))
	            

(defun pplan=inherit-rocs! (old-task new-task)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The old task and a new task.")
	   (effect  "The rocs entry of the new task is set to the rocs entry of"
		    "the old task.")
	   (value   "Undefined."))
  (setf (agenda~task-rocs new-task)
	(agenda~task-rocs old-task)))

(defun pplan=new-inst-tasks (agenda pds cstr-state new-nodes)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The current agenda, the pds, the current cstr-state and the new nodes.")
	   (effect  "None.")
	   (value   "Computes all new inst-meta goals, by checking which meta-variables"
		    "are in the new-nodes and not bound in the constraint-state binding or"
		    "already tasks in the agenda."))
  (let* ((meta-vars-in-new-nodes (remove-duplicates (apply 'append
							   (mapcar #'(lambda (new-node)
								       (if (pdsn~schematic-p new-node)
									   (remove-if-not #'meta~p
											  (data~free-variables (node~formula new-node)))
									 nil))
								   new-nodes))))
	 (meta-vars-in-inst-tasks (mapcar #'agenda~inst-task-meta-var
					  (remove-if-not #'agenda~inst-task-p
							 (agenda~all-tasks agenda))))
	 (bound-meta-vars-in-current-cstr-binding (if cstr-state
						      (subst~domain (pds~cstrpool-bindings cstr-state))
						    nil))
	 (old-meta-vars (append meta-vars-in-inst-tasks bound-meta-vars-in-current-cstr-binding))
	 (new-meta-vars (remove-if #'(lambda (mv-in-new-nodes)
				       (find mv-in-new-nodes old-meta-vars))
				   meta-vars-in-new-nodes)))

    ;;(format t "~%meta-vars-in-new-nodes: ~A" meta-vars-in-new-nodes)
    ;;(format t "~%meta-vars-in-inst-tasks: ~A" meta-vars-in-inst-tasks)
    ;;(format t "~%bound-meta-vars-in-current-cstr-binding: ~A" bound-meta-vars-in-current-cstr-binding)
    ;;(format t "~%old-meta-vars: ~A" old-meta-vars)
    ;;(format t "~%new-meta-vars: ~A" new-meta-vars)
    
    (mapcar #'(lambda (new-meta-var)
		(let* ((new-inst-task (agenda~inst-task-create new-meta-var (pds~last-plan-step pds))))
		  ;; all arising inst-tasks are stored in a separate list: sod*inst-tasks
		  (setf sod*inst-tasks (cons new-inst-task sod*inst-tasks))
		  new-inst-task))		  
	    new-meta-vars)))

(defun pplan=mmatch-cstr-state (mmatch)
  (let ((cstr-state (meth~mapp-constraint (pplan~mmatch-mmapp mmatch))))
    (when (pds~constraint-pool-p cstr-state) cstr-state)
    ))

(defun pplan=create-goal-task (open-node)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "An open-node.")
	   (effect  "None.")
	   (value   "If the open-node is schematic a new goal-schema-task is created, otherwise a"
		    "goal-task is created."))
  (if (pdsn~schematic-p open-node)
      (agenda~create-goal-schema open-node)
    (agenda~create-goal open-node)))


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ----------------------------------------------------------- Plan-Method-Matching ------------------------------------------------ |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


;; Taken from Lassaad's stuff: Method Matching!

(defclass pplan+meth-matching (keim+object)
  ((method :initarg :method
	   :initform nil
	   :accessor pplan~matched-method
	   :documentation "The matched method.")
   (parameters :initarg :parameters
	       :initform nil
	       :accessor pplan~mmatch-parameters
	       :documentation "The matched method parameters.")
   (mmapp :initarg :mmapp
	  :accessor pplan~mmatch-mmapp
	  :documentation "The binding of the method meta-variables to pds-objects.")
   (goal :initarg :goal
	 :initform nil
	 :accessor pplan~mmatch-goal
	 :documentation "The goal to be closed: just for OUTLINE-PAT.")
   (exist-prems :initarg :exist-prems
		:initform nil
		:accessor pplan~mmatch-exist-prems
		:documentation "The unsigned premises: just for OUTLINE-PAT.")
   (clsed-prems :initarg :clsed-prems
		:initform nil
		:accessor pplan~mmatch-clsed-prems
		:documentation "The premises to be deleted: just for OUTLINE-PAT.")
   (cstr-state-before :initarg :cstr-state-before
		      :initform nil
		      :accessor pplan~mmatch-cstr-state-before
		      :documentation "The constraint pool before the method was executed, that is the costraint pool wrt. which this method matching was computed..")
   (state-description :initarg :state-description
		      :initform nil
		      :accessor pplan~mmatch-state-description
		      :documentation "The strategy application in which the method matching was created.")
   )
  (:documentation "Some information to the matching of a method."))

(defmethod print-object ((mmatch pplan+meth-matching) stream)
  (format stream "<<Method-Matching for METHOD ~A GOAL ~A with MAPPING ~A>>"
	  (pplan~matched-method mmatch)
	  (pplan~mmatch-goal mmatch)
	  (pplan~mmatch-mmapp mmatch)))

(defun pplan~mmatch-cstr-state (mmatch)
  (let ((cstr-state (meth~mapp-constraint (pplan~mmatch-mmapp mmatch))))
    (when (pds~constraint-pool-p cstr-state) cstr-state)
    ))

(defun pplan~meth-matching-p (object)
  (typep object 'pplan+meth-matching))

(defun pplan=mmatching-create (method params mmapp cstr-state-before state-description &optional goal exist-prems clsed-prems)
  (make-instance 'pplan+meth-matching
		 :method method
		 :parameters params
		 :mmapp mmapp
		 :cstr-state-before cstr-state-before
		 :goal goal
		 :exist-prems exist-prems
		 :clsed-prems clsed-prems
		 :state-description state-description))

;; I have added the slot state-description, because of the following problem:
;; If a task T from a strategy S has become the start-task of a strategy S', its already-applied-mmatchings have been deleted up to now!
;; This was validly, since S' could start from scratch then!
;; The only problem was, that, if S' failed and T returned to S, then the already-applied-mmatchings of S had been deleted!
;; Therfore:
;; mmatchings have an additional slot state-description, in which the strategy-ks application is stored, in that the mmatching is made.
;; If a task becomes the new start task of a strategy, its already-applied-mmatchings are no longer deleted!
;; Instead the mmatching comparison test is extended to the request, whether the state descriptions are equal!


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| -------------------------------------------------------- COMPARISON OF MATCHINGS -------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


;; Preliminary HACK:
;; The function below checks, whether a method matching corresponds to an already tried and backtracked method matching
;; and therefore should not be tried again.
;; Later this function rebuilt, that this function is a PARAMETER OF THE PLANERS!!!!!
;; AMEIER


;; FURTHERMORE:
;; The order in which the new goals are inserted in the next-tasks list can be crucial:
;; -> before already existing goals
;; -> after already existing goals
;;
;; The key point is, that, if new goals are inserted after already existing goals, then the planner does not proceed on one branch,
;; but every time on an other branch, for example at the goals G1, G2, G3,
;; G1 is reduced to G1' at first (only if not any control rule requires something other), thereby we get
;; the list G2,G3,G1', then G2 would be reduced to G2', resulting in the list G3,G1',G2', then G3 is reduced to G3', leading to G1',G2',G3' etc. ...
;;
;; In the case of at the beginning inserted steps it would look like:
;; G1,G2,G3, G1 is reduced to G1', this ends in the list G1',G2,G3, then G1' is reduced to G1'', resulting in the list G1'',G2,G3 reduziert etc. ...
;;
;; The question, whether new goals are inserted before or behind, can be crucial amoung others: Take the case 
;; that G1, G2, G3 are rather independent, but they introduce meta variables each and they reason about meta variables.
;; Then new goals should be inserted at the front, because then one goal is processed completely without influencing the others. 
;; If new goals are inserted at the end, then the parts under G1,G2,G3 are completely mixed leading thereto,
;; that on backtracking there have to be retracted parts (if CONSTRAINT STORES HAVE CHANGED), that have nothing to do with it.
;; Such a case is Isomorphis, because the following happens there: there are three goals
;; homomorphis, surjectivem and injective. Thereby it has to be reasoned about meta variables at surjective
;; (for exists!). Therefore it should be processed one after another without interleaving!
;;
;; Currently the default behaviour 'before is set, but that should be a parameter of the planer in fact!


(defun pplan=matching-is-subsumed-p (partial-mmatching complete-mmatching)
  (let* ((complete-method (pplan~matched-method complete-mmatching))
	 (complete-mmapp (pplan~mmatch-mmapp complete-mmatching))
	 (complete-mapp-mapp (meth~mapp-mapp complete-mmapp))
	 (complete-cstr-pool-before (pplan~mmatch-cstr-state-before complete-mmatching))
	 (complete-state-des (pplan~mmatch-state-description complete-mmatching))
	 (partial-method (meth~inference (pplan~matched-method partial-mmatching)))
	 (partial-mmapp (pplan~mmatch-mmapp partial-mmatching))
	 (partial-mapp-mapp (meth~mapp-mapp partial-mmapp))
	 (partial-cstr-pool-before (pplan~mmatch-cstr-state-before partial-mmatching))
	 (partial-state-des (pplan~mmatch-state-description partial-mmatching))
	 )
    
    (and (pplan=equal-state-des complete-state-des partial-state-des)
	 (eq complete-method partial-method)
	 (pplan=subst-is-subsumed-p partial-mapp-mapp complete-mapp-mapp)
	 ;;(null (pplan=bindings-different-for-nodes-p (subst~codomain partial-mapp-mapp)
	 ;;					     (if partial-cstr-pool-before
	 ;;						 (pds~cstrpool-bindings partial-cstr-pool-before)
	 ;;					       (subst~create nil nil))
	 ;;					     (if complete-cstr-pool-before
	 ;;						 (pds~cstrpool-bindings complete-cstr-pool-before)
	 ;;					       (subst~create nil nil)))))))
	 (null (pplan=bindings-different-p (if partial-cstr-pool-before
					       (pds~cstrpool-bindings partial-cstr-pool-before)
					     (subst~create nil nil))
					   (if complete-cstr-pool-before
					       (pds~cstrpool-bindings complete-cstr-pool-before)
					     (subst~create nil nil)))))))

(defun pplan=equal-state-des (complete-state-des partial-state-des)
  (if (and complete-state-des
	   partial-state-des
	   (eq complete-state-des partial-state-des))
      't
    nil))


;; Note:
;; This function checks (among other things), whether two bindings are differnt wrt. to the mvs in the outline of a method.
;; If this is the case it is sencefull (or at least it can be sencefull) to say that two mmatchings are not equal.
;; This is necessary, for instance, when searching over metavariables.
;; Assume you have to goals G1 and G2. Both depend on a meta-variable MV. You close G1 first. Thereby MV is bound on term t.
;; Then you try to close G2, but you fail, since the binding of MV on t is not appropriate for closing G2. Hence, you backtrack,
;; until you get MV unbound in G1. You close G1 (or the tree below G1) and instantiate thereby MV by t'. Then you close G2.
;; Thereby, you want to apply the same steps as you tried in the first attempt. But now with the binding MV -> t'.
;; Therefore, the application of these steps should not be forbidden.
;; Example: Isomorphic, ..., resclass-examples
;;
;; Addition:
;; To be really complete, it is even not enough to compare only the bindings of these mv which are relevant for the nodes in the
;; outlines (the mvs which occur in the formulas of the nodes), but ALL mvs.
;; Assume the following scenario:
;; You have three goals: G1:(mv1=t1 or mv1=t1'), G2:(mv2=t2 or mv2=t2'), G3
;; You apply first ORIL on G1, then bind mv1=t1 in the resulting equation (by reflex-m) 
;; then you do the same for G2, binding mv2=t2
;; then you try to prove G3, but you fail and backtrack.
;; You decide to unbind mv2 and to reopen G2, since ORIL is now already applied, you apply ORIR on G2 and bind mv2=t2'
;; then you try again G2, but you fail again.
;; Now you backtrack again
;;
;; The following would be good: G2 open again (i.e. retraction of mv2=t2' and orir), then retraction of mv1=t1 + oril
;; then orir on G1, then again oril on G2 (now there are new bindings -> oril is possible again!)
;; This behaviour can only be garanteed, if CPool-Backtrack id prefered in general before step-backtrack!
;; then G2 will not be backtracked further! (WHAT COULD HAPPEN OTHERWISE -> error)
;;
;; BUT: CPOOL IN GENERAL (i.e. the last step introducing bindings is retracted, not the last step, that introduced bindings, that
;; are relevant for the meta variables of the node!) THATS DANGEROUS TOO:
;; EXAMPLE:
;; Two GOALS: G1 + G2
;; First we process G1 -> thereby MV1 is bound (there are several possibilities), but a rest-goal G1' remains
;; But now we go on with goal G2, which we close, so that MV2 becomes bound. G2 gets completely closed!
;; Now we can not close G1' and have to backtrack! Because the closing of G2 introduced bindings subsequent
;; (which have nothing to do with G1'), we retract this step leaving a goal G2' below G2!
;; If we continue to backtrack on G1' and bind MV1 newly, then later we can go on with G2' and again insert the correct binding for MV2.
;; BUT: Take the case we go on with G2' and try other instantiations for MV2, that do not work and finally retract G2' until G2.
;;      Before G2 would be retracted, the steps for G1 would be retracted first, that created the binding
;;      After that G2 should be solvable quite normal ...
;; ...
;; ...
;;
;; 
;; On second thought I concluded the following:
;; 1. Backtrack Check: Two matchings are unequal if the bindings are arbitrarily DIFFERENT! (i.e. not only in respect to the meta variables,
;;                     that occur in the OUTLINES of the step!)
;; 2. CPOOL-BACKTRACK: The last (not the last for the in the node occuring meta variable relevant bindings!)
;;                     binding changing step is retracted

(defun pplan=bindings-different-p (binding1 binding2)
  (declare (edited  "30-MAY-2000")
	   (authors Ameier)
	   (input   "Two bindings.")
	   (effect  "None.")
	   (value   "T, if the bindings contain different bindings for meta-variables."))
  (let* ((binding1-domain (subst~domain binding1))
	 (binding2-domain (subst~domain binding2))
	 (union-mvs (union binding1-domain binding2-domain))          ;; --> all mvs bound in one of the bindings
	 (inter-mvs (intersection binding1-domain binding2-domain))   ;; --> all mvs bound in both bindings
	 (diff1 (set-difference union-mvs inter-mvs))                 ;; --> all mvs bound in one of the bindings but not in both
	 (diff2 (remove-if #'(lambda (mv)
			       (keim~equal (subst~apply binding1 mv) (subst~apply binding2 mv)))
			   inter-mvs))                                ;; --> all mvs bound in both bindings but on different terms
	 (diff-mvs (append diff1 diff2))                              ;; --> all mvs bound differently in the bindings
	 )

    (if diff-mvs
	't
      nil)))

(defun pplan=mapping-is-subsumed-p (partial-mmapping complete-mmapping)
  (let* ((complete-subst (meth~mapp-subst complete-mmapping))
	 (complete-mapp (meth~mapp-mapp complete-mmapping))
	 (complete-extension (meth~mapp-extension complete-mmapping))
	 (complete-constraint (meth~mapp-constraint complete-mmapping))
	 (partial-subst (meth~mapp-subst partial-mmapping))
	 (partial-mapp (meth~mapp-mapp partial-mmapping))
	 (partial-extension (meth~mapp-extension partial-mmapping))
	 (partial-constraint (meth~mapp-constraint partial-mmapping))
	 (complete-binding (if (pds~constraint-pool-p complete-constraint)
			       (pds~cstrpool-bindings complete-constraint)
			     (subst~create nil nil)))
         (partial-binding (if (pds~constraint-pool-p partial-constraint)
			      (pds~cstrpool-bindings partial-constraint)
			     (subst~create nil nil))))
    
    (and ;; (keim~equal complete-constraint partial-constraint)
     (pplan=subst-is-subsumed-p (pplan=apply-binding partial-subst partial-binding)
				(pplan=apply-binding complete-subst complete-binding))
     (pplan=subst-is-subsumed-p (pplan=apply-binding partial-mapp partial-binding)
				(pplan=apply-binding complete-mapp complete-binding))
     ;; (keim~equal extension1 extension2) ;; what exactly is Extension???
     )))


(defgeneric pplan=apply-binding (subst binding)
  (:method ((subst subst+substitution) binding)
	   (let* ((domain (subst~domain subst))
		  (codomain (subst~codomain subst))
		  (new-codomain (mapcar #'(lambda (codo)
					    (if (term~p codo)
						(subst~apply binding codo)
					      codo))
					codomain))
		  (new-subst (subst~create nil nil)))
	     (setf (subst~domain new-subst) domain)
	     (setf (subst~codomain new-subst) new-codomain)
	     new-subst))
  (:method ((subst mapp+mapping) binding)
	   (let* ((domain (mapp~domain subst))
		  (codomain (mapp~codomain subst))
		  (new-codomain (mapcar #'(lambda (codo)
					    (if (term~p codo)
						(subst~apply binding codo)
					      codo))
					codomain)))
	     (mapp~create domain new-codomain))))

(defun pplan=subst-is-subsumed-p (partial-subst complete-subst)
  (declare (edited  "29-JUN-1999")
	   (authors Ameier)
	   (input   "Two substitutions.")
	   (effect  "None.")
	   (value   "T if all elements of the first substitition are also in the"
		    "second substitution."))
  
  (let* ((partial-domain (subst~domain partial-subst))
	 (partial-codomain (subst~codomain partial-subst))
	 (complete-domain (subst~domain complete-subst))
	 (complete-codomain (subst~codomain complete-subst))
	 (partial-assoc-list (mapcar #'(lambda (in1 in2)
					 (list in1 in2))
				     partial-domain
				     partial-codomain))
	 (complete-assoc-list (mapcar #'(lambda (in1 in2)
					  (list in1 in2))
				      complete-domain
				      complete-codomain)))
    (do* ((rest-partial-domain partial-domain (rest rest-partial-domain))
	  (flag 't))
	((or (null rest-partial-domain)
	     (null flag))
	 flag)
      (let* ((head-partial-domain-element (first rest-partial-domain))
	     (acc-partial-codomain-element (second (assoc head-partial-domain-element partial-assoc-list)))
	     (acc-complete-codomain-element (second (assoc head-partial-domain-element complete-assoc-list))))
	(when (null (and (assoc head-partial-domain-element complete-assoc-list)
			 (pplan=subst-is-subsumed-test acc-complete-codomain-element acc-partial-codomain-element)))
	  (setf flag nil))))))

(defun pplan=subst-is-subsumed-test (it1 it2)
  (setq itti1 it1)
  (setq itti2 it2)

  (if (term~p it1)
      (term~alpha-equal it1 it2)
    (keim~equal it1 it2)))


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ----------------------------------------------------- CHOOSING AN APPLICABLE MATCHING --------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


;; Here is a possibility to plug in different ways to decide for the next
;; MethodMatching to be applied.
;;
;; So far we realized two possibilities:
;;
;; a) The Waterfall Way:
;;    First we decide for a task T
;;           |
;;           +-> Then we decide for a Method M to be applied to T
;;                     |
;;                     +-> Then for supports S + Parameters P
;;                           |
;;                           +-> Then we compute all MethodMatchings MMS for T with M and S + P
;;                               And decide for one MethodMatching of MMS
;;    (all choice points can be controled by Control Rules)
;;
;; b) The Parallel Way: 
;;    First we decide for a task T
;;           |
;;           +-> Then we compute all possible MethodMatchings MMS for T with all avaible Methods 
;;               And decide for one MethodMatching of MMS
;;    (again all choice points can be controled by Control Rules)

(defun pplan=choose-task-and-mmatching (pds agenda)
  (declare (edited  "30-JUL-2001")
	   (authors Ameier)
	   (input   "A PDS and an agenda.")
	   (effect  "None.")
	   (value   "Computes the next task and which MethodMatching should be applied to it"
		    "or with respect to it."
		    "Multiple-Value:"
		    "First: A task."
		    "Second: A MethodMatching with respect to this task."))
  (let* ((selection (gethash 'selection (strat~strategy-ks-hash-table (roc~strategy-ks pplan*roc-state-description)))))
    
    (cond ((string-equal selection 'waterfall)
	   (pplan=choose-task pds agenda :selection 'waterfall))
	  ((string-equal selection 'parallel)
	   (pplan=choose-task pds agenda :selection 'parallel))
	  (t
	   (omega~error "~%Current Strategy does not specify the selection parameter")))))

#| --------------------------------------------------------------------> Aussuchen TASK |#

;; 1.) Task selection: Remove all goals, that do not have the current ROC.
;;                     Apply control rules to determine the task
;;                         If crule fires -> set of task tupels to check (there can also be dropped tasks!)
;;                         If no crule fires, sort the goals with normal goals before schematic goals by default.
;;    ATTENTION: TASK SELECTION IS NO CHOICE POINT, i.e. if the first selected task fails, do not go on with the next task!!
;;
;;
;;
;; Little further remark:
;; Task-Tupel: Consists of a list with a task and a arbitrary number of method tuples (TASK (METH-TUPEL)*)
;; Method-Tupel: Consists of a list with first a method, second a list of supports and third a list of parameters.
;;               Note, that the method has to be specified, supports and parameters are optional,
;;               but the second argument is always interpreted as supports, the third then as parameters!


(defun pplan=merge-cri-task-tupel (cri-task-tupel)
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A list of outputs from the Control-rule intepreter.")
	   (effect  "None.")
	   (value   "The output list from the control-rule interpreter is megred to real task-tupel:"
		    "that means all tupel with the same TASKS are merged into one TASK-TUPEL,"
		    "e.g. (TASK1 blabla) (TASK1 bloblo) to: (TASK1 blabla bloblo)."))
  (let* ((tasks (reverse (remove-duplicates (reverse (mapcar #'(lambda (cri-tupel)
								 (if (listp cri-tupel)
								     (first cri-tupel)
								   cri-tupel))
							     cri-task-tupel))))))
    ;; remove-duplicates works from behind!!

    (do* ((rest-tasks tasks (rest rest-tasks))
	  (back-task-tupel nil))
	((null rest-tasks)
	 back-task-tupel)
      
      (let* ((head-task (first rest-tasks))
	     (cri-tupel-with-head-task (remove-if-not #'(lambda (cri-tupel)
							  (eq (if (listp cri-tupel)
								  (first cri-tupel)
								cri-tupel)
							      head-task))
						      cri-task-tupel))
	     (cri-tupel-after-cutting-head-task (mapcar #'(lambda (cri-tupel)
							    (if (listp cri-tupel)
								(rest cri-tupel)
							      nil))
							cri-tupel-with-head-task))
	     (remaining-method-tupels (remove-if #'(lambda (rest-tupel)
						     (or (null rest-tupel)
							 (null (first rest-tupel))))
						 cri-tupel-after-cutting-head-task)))
	;;(format t "~% HEAD-TASK: ~A" head-task)
	;;(format t "~% cri-tupel-with-head-task: ~A" cri-tupel-with-head-task)
	;;(format t "~% cri-tupel-after-cutting-head-task: ~A" cri-tupel-after-cutting-head-task)
	;;(format t "~% remaining-method-tupels: ~A" remaining-method-tupels)
	
	(setf back-task-tupel (append back-task-tupel (list (cons head-task remaining-method-tupels))))))))

(defun pplan=choose-task (pds agenda &key selection)
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A pds and an agenda.")
	   (effect  "Seeks for a possible matching for a task ...")
	   (value   "Multiple-Value:"
		    "First: The choosen Task."
		    "Second: The choosen applicable matching for it, or nil if no one was found."))

  (multiple-value-bind
      (free-tasks unblocked-tasks)
      (pds~first-tasks! pds agenda)
    ;; Remark: pds~first-tasks! already updates the current-formulas in the task-nodes!!!!!!
    ;; because of this the tasks do not have to be made uptodate later!
    
    (let* ((first-tasks (append free-tasks unblocked-tasks))
	   (first-goal-tasks-with-roc (remove-if-not #'(lambda (task)
							 (and (agenda~goal-or-goal-schema-task-p task)
							      (find pplan*roc-state-description (agenda~task-rocs task))))
						     first-tasks)))
      
      (when pplan*verbose
	(omega~message "**CHOOSE-TASK**: Possible local first tasks: ~A" first-goal-tasks-with-roc))
      
      (if (null first-goal-tasks-with-roc)
	  ;; in fact this case would have to be impossible by the stuff in pplan~plan!!
	  (omega~message "~%Something wrong in Function pplan=do-step !!")
	
	(multiple-value-bind
	    (task-tupelli fired-rules)
	    (cri~call first-goal-tasks-with-roc
		      :kind 'tasks
		      :tasks first-goal-tasks-with-roc
		      :pds pds)

	  (setf pplan*fired-control-rules fired-rules)
	  
	  (let* ((task-tupel (if fired-rules
				 (pplan=merge-cri-task-tupel task-tupelli)
			       (mapcar #'list first-goal-tasks-with-roc)
			       
			       ;; Problem: 1. This kind of knowledge is control knowledge in fact!
			       ;;             Do not code hard!
			       ;;          2. This stuff prevents a little bit that it is searched depth-first!
			       ;;             What is also control knowledge in principle ...
			       ;;(append
			       ;;	;; erst die non-schematic-goals Tupel
			       ;;	(mapcar #'list (remove-if-not #'(lambda (task)
			       ;;					  (or (agenda~goal-p task)
			       ;;					      (and (agenda~goal-schema-p task)
			       ;;						   (null (agenda~goal-schematic-p task)))))
			       ;;				      first-goal-tasks-with-roc))
			       ;;	;; dann die schematic-goals Tupel
			       ;;	(mapcar #'list (remove-if #'(lambda (task)
			       ;;				      (or (agenda~goal-p task)
			       ;;					  (and (agenda~goal-schema-p task)
			       ;;					       (null (agenda~goal-schematic-p task)))))
			       ;;				  first-goal-tasks-with-roc)))))
			       ))
		 (first-task-tupel (pplan=choose-task-interactively task-tupel first-goal-tasks-with-roc))
		 (first-task (first first-task-tupel))
		 (first-method-tupels (rest first-task-tupel)))
	    
	    (when pplan*verbose
	      (omega~message "**CHOOSE-TASK**: Deciding for task ~A, CRI: ~A" first-task fired-rules))
	    
	    (values
	     first-task
	     (cond ((string-equal selection 'waterfall)
		    (pplan=choose-method pds agenda first-task first-method-tupels))
		   ((string-equal selection 'parallel)
		    (pplan=choose-method-parallel pds agenda first-task first-method-tupels))
		   (t
		    (omega~error "~%MMatching Selection should be either waterfall or parallel"))))))))))

(defun pplan=choose-task-interactively (task-tupel legal-tasks)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "A list of task-tupel and a set of legal tasks.")
	   (effect  "None.")
	   (value   "If inac*strategy-ks-execution-interactive is nil simply the first task-tupel."
		    "Otherwise the user is asked interactively to choose a task or task-tupel."))

  (let* ((all-offers (append task-tupel legal-tasks))
	 (suggestion-by-cri (first (rand~call-randomizer all-offers
							 :kind 'tasks))))
    
    (if (null inac*strategy-ks-execution-interactive)
	suggestion-by-cri
      (let* ((default-number (if suggestion-by-cri
				 (position suggestion-by-cri all-offers)
			       nil))
	     (text (concatenate 'string
				(format nil "~%~%~%~%Choose the next Task/Task-Tupel:")
				(format nil "~%Auto-Mode suggests number ~A" (if (numberp default-number) (+ default-number 1) nil))
				(format nil "~%--------------------------------")
				(format nil "~%Legal possibilities are:")))
	     (number-of-task (omega~query-choice  text (append all-offers (list "Leave PPlanner")) default-number))
	     (task (nth number-of-task all-offers)))

	(cond (task
	       ;; a task was choosen
	       (if (listp task)
		   ;; already tupel
		   task
		 ;; not a tupel -> Tupelize
		 (list task)))
	      ((= number-of-task (length all-offers))
	       ;; Leave-PPlanner was choosen
	       (throw 'leave-pplanner
		      (exmes~create-interruption-message pplan*roc-state-description nil nil)))
	      (t
	       ;; a not specified option was choosen
	       ;; -> we take the suggestion from cri
	       (if (listp suggestion-by-cri)
		   ;; already tupel
		   suggestion-by-cri
		 ;; not a tupel -> Tupelize
		 (list suggestion-by-cri))))))))



#| -------------------------------------------------------------------------> CHOOSE METHOD PARALLEL |#

(defun pplan=choose-method-parallel (pds agenda task method-tupels &key (kind 'standard))
  (declare (ignore method-tupels))
  ;; currently we ignore everything that's possibly coming from top down
  
  (let* ((with-task (or (equal kind 'standard)
			(equal kind 'restricting)))
	 (all-mmatchings (if with-task
			     (progn
			       (setf (keim::pdsc~broken-matchings (pdsj~control (node~justification (agenda~task-node task))))
				     nil)
			       (apply #'append (mapcar #'(lambda (method)
							   (let* ((goal-matchings (pplan=matching-goals task method :kind kind)))
							     (if goal-matchings
								 (pplan=choose-supports-parallel pds agenda task method
												 goal-matchings
												 nil :kind kind)
							       nil)))
						       pplan*methods)))
			   (apply #'append (mapcar #'(lambda (method)
						       (pplan=choose-supports-parallel pds agenda task method
										       (list (meth~mapping-create (subst~create nil nil)
														  (mapp~create nil nil)))
										       nil :kind kind))
						   pplan*methods)))))
    
    (pplan=choose-matching pds agenda task nil all-mmatchings :kind kind)))

(defun pplan=choose-supports-parallel (pds agenda task method goal-matchings support-tupel &key (kind 'standard))
  (declare (ignore support-tupel))
  ;; currently we ignore everything that's possibly coming from top down

  (let* ((with-task (or (equal kind 'standard)
			(equal kind 'restricting)))
	 ;; -> task exists!
	 (task-node (if with-task
			(agenda~task-node task)
		      nil))
	 (support-nodes (if with-task
			    (pds~node-supports task-node pds)
			  nil))
	 (selected-support-tupel-list
	  (multiple-value-bind
	      (support-tupel-list applied-crules)
	      (cri~call (list (list support-nodes))
			:task task
			:kind 'supports
			:task-node task-node
			:task-formula (if task (node~formula task-node) nil)
			:method method
			:pds pds)

	    (setf pplan*fired-control-rules (append  pplan*fired-control-rules applied-crules))
	    
	    (when pplan*verbose
	      (omega~message "**CHOOSE-SUPPORTS+PARAMS**: Calling CRI for supports, receiving: ~A, CRI-FLAG: ~A"
			     support-tupel-list
			     applied-crules))
	    
	    (if applied-crules
		;; when crules applied -> try choosen ones:
		support-tupel-list
	      ;; if no crules applied -> try all support nodes:
	      (list (list support-nodes)))))
	 )
    
    (apply #'append (mapcar #'(lambda (support-tupel)				       
				(pplan=handling-support-tupel pds agenda task method (pplan=copy-matchings goal-matchings) support-tupel
							      :kind kind
							      :mmatchings 'all))
			    selected-support-tupel-list))))

(defun pplan=copy-matchings (matchings)
  (mapcar #'(lambda (matching)
	      
	      (let* ((subst (meth~mapp-subst matching))
		     (mapp (meth~mapp-mapp matching))
		     (constr (meth~mapp-constraint matching))
		     (ext (meth~mapp-extension matching))
		     (new-subst (subst~create () ())))
		
		(setf (subst~domain new-subst) (copy-list (subst~domain subst)))
		(setf (subst~codomain new-subst) (copy-list (subst~codomain subst)))
		;; This destructive setting of the new-subst is necessary since otherwise the creation of
		;; the substitution via (subst~create ) is rejected because of non-term entries ...

		(meth~mapping-create new-subst
				     (mapp~create (copy-list (mapp~domain mapp)) (copy-list (mapp~codomain mapp)))
				     ext
				     constr)))
	  matchings))
		 

#| -------------------------------------------------------------------------> Aussuchen Methode |#

;; 2.) Method selection: already given: Task + method-tupel (optional)
;;     If method-tupel given:
;;           -> Test in the given order at the method-tupels:
;;                             -> Match goal (if method is a backward method)
;;                             -> Test support tupel further
;;           -> if none works -> return fail               ^
;;     If no method-tupels given:                             \                    
;;     -> Call CRI on methods                                  \
;;        If a ruled was applied:                              |
;;                   -> new method tuple, same game as above   |
;;        if not test all methods:
;;                   -> Goal-Matching
;;                   -> Test supports further
;;     Remark: Method tuples are not merged! (tupel with the same method united!)
;:             Because the tupel contain the exact order, in which should be tested!

(defun pplan=methods-equal (meth1 meth2)
  (keim~equal meth1 meth2))
  
(defun pplan=handling-method-tupel (pds agenda task method-tupel-list &key (kind 'standard))
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, an agenda, a task and a method-tupel list.")
	   (effect  "None.")
	   (value   "Tries to find applicable matchings to the method-tupels"
		    "The first one witch is found is returned, nil if no allowed one is found."))
  
  (when pplan*verbose
    (omega~message "**CHOOSE-METHOD**: Checking the following method-tupel for applicable methods: ~A" method-tupel-list))

  (cond ((or (equal kind 'standard)
	     (equal kind 'restricting))
	 ;; -> Task given
	 
	 ;; The Broken Matchings of the task are reset (not necessary when dealing with restriction; however, it does no damage!)
	 (setf (keim::pdsc~broken-matchings (pdsj~control (node~justification (agenda~task-node task))))
	       nil)
	 
	 (let* ((task-node (agenda~task-node task))
		(just (node~justification task-node))
		(control (pdsj~control just))
		(excluded-methods nil) ;; (keim::pdsc~excluded-methods control)) this caused problems if MVS got substituted
		(remaining-method-tupel-list (remove-if #'(lambda (method-tupel)
							    (let* ((method (if (meth~method-p (first method-tupel))
									       (first method-tupel)
									     (meth~find-method (first method-tupel)))))
							      (find method excluded-methods)))
							method-tupel-list))
		(rejected-method-tupel-list (remove-if-not #'(lambda (method-tupel)
							       (let* ((method (if (meth~method-p (first method-tupel))
										  (first method-tupel)
										(meth~find-method (first method-tupel)))))
								 (find method excluded-methods)))
							   method-tupel-list)))
	   (when pplan*verbose
	     (omega~message "**CHOOSE-METHOD**: The method tupels ~A are removed, since the respective method is in the excluded method list of the task."
			    rejected-method-tupel-list))

	   (pplan=check-all-method-tupel pds agenda task remaining-method-tupel-list :kind kind)))
	(t
	 ;; normalizing -> no task found
	 ;;             -> no methods are expluded because of the tasks since no Goal-matching have to be made!
	 
	 (pplan=check-all-method-tupel pds agenda task method-tupel-list :kind kind))))


(defun pplan=check-all-method-tupel (pds agenda task remaining-method-tupel-list &key (kind 'standard))
  (declare (edited  "03-AUG-2001")
	   (authors Ameier)
	   (input   "A pds, the agenda of the pds, the current task and a list of method-tupels.")
	   (effect  "None.")
	   (value   "The method-tupels are successively checked for one that gives back a mmatching."
		    "The first mmatching that is found is returned. If no mmatching is found nil is returned."))
  
  (do* ((rest-method-tupel-list (if (null inac*strategy-ks-execution-interactive)
				    remaining-method-tupel-list
				  (pplan=choose-method-tupel-interactively remaining-method-tupel-list task))
				(cond ((null inac*strategy-ks-execution-interactive)
				       (rest rest-method-tupel-list))
				      (success
				       nil)
				      (t
				       (pplan=choose-method-tupel-interactively remaining-method-tupel-list task))))
	(success nil))
      ((or (null rest-method-tupel-list)
	   success)
       success)
    
    (let* ((head-method-tupel (first rest-method-tupel-list))
	   (method (if (meth~method-p (first head-method-tupel))
		       (first head-method-tupel)
		     (meth~find-method (first head-method-tupel))))
	   (method-goal (meth~goal method)))

      ;; (format t "~% CURRENT HEAD_METHOD-TUPEL: ~A" head-method-tupel)
       
      (when (and method-goal
		 (equal kind 'normalizing))
	(omega~error "~%In function pplan=handling-method-tupel with kind normalizing a backward-method ~A is used as normalizing method!"
		     method))
      
      (when (null (find method pplan*methods :test #'pplan=methods-equal))
	(omega~message "~%Method ~A not in pplan*methods" method))

      (let* ((support-tupel (if (or (second head-method-tupel) ;; either supports
				    (third head-method-tupel)) ;; or parameters already choosen!
				(rest head-method-tupel)
			      nil))
	     (goal-matchings (if (or (equal kind 'standard)
				     (equal kind 'restricting))
				 (pplan=matching-goals task method :kind kind)
			       (list (meth~mapping-create (subst~create nil nil)
							  (mapp~create nil nil))))))
	
	(when pplan*verbose
	  (if (or (equal kind 'standard)
		  (equal kind 'restricting))
	      (omega~message "**CHOOSE-METHOD**: method-tupel ~A resulted in goal-matchings ~A with task ~A proceeding to search for supports."
			     head-method-tupel
			     goal-matchings
			     task)
	    (omega~message "**CHOOSE-METHOD**: Normalizing -> no task given for method-tupel ~A, proceeding to search for supports with default goal-matchings ~A"
			   head-method-tupel
			   goal-matchings)))
	
	(when goal-matchings
	  (setf success (pplan=choose-supports pds agenda task method goal-matchings support-tupel :kind kind)))))))







(defun pplan=matching-goals (task method &key (kind 'standard))
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A goal task and a method.")
	   (effect  "None (to the best of my knowledge ...).")
	   (value   "If the method is a backward method, a list of matchings of the method-goal with"
		    "the open-node of the task, otherwise a list with an empty matching."))

  (setf meth*current-method method)

  (let* ((method-goal (meth~goal method))
	 (task-goal (agenda~task-node task)))

    (cond ((and method-goal ;; (method is backward method)
		task)       ;; task exists

	   (let* ((meth-matchings (meth~match-p method-goal
						task-goal
						(meth~mapping-create (subst~create nil nil)
								     (mapp~create nil nil))
						:one2one
						(pds~node-formula task-goal))))
	     
	     (if meth-matchings
		 meth-matchings
	       (let* ((just (node~justification task-goal))
		      (control (pdsj~control just)))
		 (when pplan*verbose
		   (omega~message "**CHOOSE-METHOD**: Method ~A not compatible with goal ~A, entering method as exluded in control"
				  method
				  task-goal))

		 ;; user comments output:
		 (when pplan*user-comments
		   (omega~message "The method ~A requires a goal of the form ~A." method (node~formula method-goal))
		   (omega~message "But the given goal is of the form ~A" (node~formula task-goal))
		   (omega~message "Look for a method that can tackle your goal!"))
		 
		 (when (null pplan*VIL-on)
		   ;; Excluded methods are only set if VIL mode is out!
		   (setf (pdsc~excluded-methods control)
			 (cons method (pdsc~excluded-methods control))))
		 (when (and pplan*VIL-on
			    (string-equal kind 'standard))
		   (omega~message "~%Sorry, but goal ~A matchs not with the goal ~A of method ~A" task-goal method-goal method))
		 nil))))
	  ((and method-goal    ;; (method is backward method)
		(null task))   ;; task does not exist
	   (omega~error "~%In function pplan=matching-goals: A backward method is called without a goal! May a failure with the normalizing things..."))
	  (t
	   (list (meth~mapping-create (subst~create nil nil)
				      (mapp~create nil nil)))))))

;;(defun pplan=already-applied-methods-with-bindings (task)
;;  (declare (edited  "06-MAR-2001")
;;	   (authors Ameier)
;;	   (input   "A Task.")
;;	   (effect  "None.")
;;	   (value   "A list of pairs consisting of methods that were already applied to the task (and backtracked afterwards),"
;;		    "and the constraint stores under which they were applied."))
;; (let* ((control (pdsj~control (node~justification (agenda~task-node task))))
;;	 (already-applied-mmatchings (keim::pdsc~already-applied-mmatchings control)))
;;    (mapcar #'(lambda (mmatching)
;;		(list (pplan~matched-method mmatching)
;;		      (pplan~mmatch-cstr-state-before mmatching)))
;;	    already-applied-mmatchings)))
;;
;;(defun pplan=find-restricting-method-with-same-binding-p (task)
;;  (declare (edited  "06-MAR-2001")
;;	   (authors Ameier)
;;	   (input   "A task.")
;;	   (effect  "None.")
;;	   (value   "T if in the already applied methods list there is an entry such:"
;;		    "1. The applied (and backtracked) method is in the restriction method list."
;;		    "2. The current binding of the current constraint store is the same as the"
;;		    "   binding in the backtracked mmatching."
;;		    "Otherwise nil."))
;;  (let* ((already-applied-methods-with-bindings (pplan=already-applied-methods-with-bindings task))
;;	 (already-applied-restriction-methods-with-bindings (remove-if #'(lambda (
;;
;; How do I find out, whether a method was applied backwards?
;; -> does not work at the moment!
;;


(defun pplan=choose-method (pds agenda task method-tupel-list &key (kind 'standard))
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, an agenda, a task, and a method-tupel-list (may nil).")
	   (effect  "Seeks for an applicable method for the given task, in the method-tupel"
		    "can stand possible selections already done by the task selection.")
	   (value   "The choosen matching, or nil if no one was found."))

  ;; set rand*current-task
  (setf rand*current-task task)
  
  (when pplan*verbose
    (omega~message "**CHOOSE-METHOD**: Incoming method-tupel: ~A" method-tupel-list))
  
  (if method-tupel-list
      (pplan=handling-method-tupel pds agenda task
				   (rand~call-randomizer (pplan=merge-method-tupels method-tupel-list)
							 :kind 'methods)
				   :kind kind)
    
    (let* ((with-task (or (equal kind 'standard)
			  (equal kind 'restricting)))
	   (method-names (mapcar #'keim~name pplan*methods)))
      
      (multiple-value-bind
	  (method-tupel-list2 applied-crules)
	  (cri~call ;;(copy-list pplan*methods) ;; copy, since cri~call has destructive effects on the list  
	            (mapcar #'keim~name pplan*methods)
		    :kind 'methods
		    :task task
		    :task-node (if with-task
				   (agenda~task-node task)
				 nil)
		    :task-formula (if with-task
				      (node~formula (agenda~task-node task))
				    nil)
		    :agenda agenda
		    :pds pds)

	(setf pplan*fired-control-rules (append pplan*fired-control-rules applied-crules))
		
	(when pplan*verbose
	  (omega~message "**CHOOSE-METHOD**: CRI on methods resulted in methode tupels: ~A, CRI: ~A" method-tupel-list2 applied-crules))
	
	(if applied-crules
	    (pplan=handling-method-tupel pds agenda task
					 (rand~call-randomizer (pplan=merge-method-tupels method-tupel-list2)
							       :kind 'methods)
					 :kind kind)
	  (pplan=handling-method-tupel pds agenda task
				       (rand~call-randomizer (pplan=merge-method-tupels pplan*methods)
							     :kind 'methods)
				       :kind kind))))))

(defun pplan=merge-method-tupels (in-list)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "A list where every element of the in-list which is not a list is listed again."))
  (apply 'append (mapcar #'(lambda (in-element)
			     (if (listp in-element)
				 (list in-element)
			       (list (list in-element))))
			 in-list)))

(defun pplan=choose-method-tupel-interactively (rest-method-tupel-list task)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "A list of possible method-tupels (this can be nil.")
	   (effect  "None.")
	   (value   "The user is asked to specify a method-tupel (this can also be nil)."
		    "Returned is a list of method-tuepls (this can also be nil)."))
  
  (let* ((task-node (agenda~task-node task))
	 (suggestion-by-cri (first rest-method-tupel-list))
	 (all-possibilities (append rest-method-tupel-list pplan*methods)))
    (let* ((default-number (if suggestion-by-cri
			       (position suggestion-by-cri all-possibilities)
			     nil))
	   (text (concatenate 'string
			      (format nil "~%Choose the next Method/Method-Tupel:")
			      (format nil "~%Current task is ~A with formula ~A"
				      task-node
				      (node~formula task-node))
			      (format nil "~%Auto-Mode suggests number ~A" (if (numberp default-number) (+ default-number 1) nil))
			      (format nil "~%------------------------------------")
			      (format nil "~%Legal possibilities are:")))
	   (number-of-method (omega~query-choice text (append all-possibilities (list nil) (list "Leave PPlanner")) default-number))
	   (method (nth number-of-method all-possibilities)))
      (cond (method
	     ;; a method was choosen
	     (if (listp method)
		 ;; already tupel -> list it to get list of method-tupels
		 (list method)
	       ;; not a tupel -> list it twice to tupelize and to get a list of method-tupels
	       (list (list method))))
	    ((= number-of-method (length all-possibilities))
	     ;; nil was choosen
	     nil)
	    ((= number-of-method (+ (length all-possibilities) 1))
	     ;; Leave PPlanner was choosen
	     (throw 'leave-pplanner
		    (exmes~create-interruption-message pplan*roc-state-description nil nil)))
	    (t
	     ;; a not specified option was choosen
	     ;; -> if suggestion-by-cri was given we take this, otherwise we take nil
	     (if suggestion-by-cri
		 (list suggestion-by-cri)
	       nil))))))

#| -------------------------------------------------------> Aussuchen Supports + Parameter |#

;; 3. Selection of supports + parameters
;;    Already given: pds, agends + TASK + METHOD + GOAL-MATCHINGS + (optional) ONE (and at most one) support tupel
;;
;;    -> If support-tupel given 
;;            -> compute all matchings, that result from the matching of the parameters with the goal matchings!
;;            -> compute all matchings, that result from the combination of these matchings with the supports!
;;            -> insert the mapping of the names + application condition + all rest parameter
;;            -> further all matchings to matching selection
;;            -> if none works fail
;;    -> If no support tuple given
;;            -> Call cri~call                              | 
;;                   -> If a rule has fired:                |     
;;                               -> Set of support-tupels   |
;;                               -> With each support-tupel |
;;                   -> If no rule has fired:
;;                               -> Do the above with the complete supports
;; 
;;    Remark: Selection of the parameters has to be done anyhow, anytime by the cri
;;    Remark: support tupel restricts the applicable supports! It says nothing about which support has to be testet with which premise
;;            -> this could be done by the cri over matchings!
;;

(defun pplan=consistent-premises (open-node mmapps existp-labels clsedp-labels &key (kind 'standard))
  (declare (edited  "16-MAR-1998")
	   (authors Lassaad)
	   (input   "A pds open node, a mmappitution list, and two lists of"
		    "method node labels: of existent nodes and of closed"
		    "nodes. These labels must be bound within the elements"
		    "of MMAPPS.")
	   (effect  "None.")
	   (value   "A list of triples: for each mmappitution, where the"
		    "associated premises are consistent with the OPEN-NODE,"
		    "i.e., each premise does not contain the goal OPEN-NODE"
		    "in its justifyings and has a hypothesis list which is"
		    "a subset of the goal hypotheses, this mmappitution, the"
		    "list of existent premises, and the list of closed premises"
		    "are returned together in a triple."))
  (when mmapps
    (let* ((mmapp (first mmapps))
	   (exist-prems (meth~mapp-get-component existp-labels mmapp :mapp))
	   (clsed-prems (meth~mapp-get-component clsedp-labels mmapp :mapp))
	   (goal-prems (append exist-prems clsed-prems))
	   (goal-hyps (if (or (equal kind 'standard)
			      (equal kind 'restricting))
			  (pdsn~hyps open-node)
			nil)))
      (cond ((or (equal kind 'standard)
		 (equal kind 'restricting))
	     ;; open-node exists -> you have to check whether compatible
	     (if (and (every #'(lambda (prem)
				 (subsetp (pdsn~hyps prem) goal-hyps))
			     goal-prems)
		      (not (find open-node (pdsn~justifying-nodes goal-prems))))
		 (cons (list mmapp exist-prems clsed-prems)
		       (pplan=consistent-premises open-node (rest mmapps) existp-labels clsedp-labels :kind kind))
	       (pplan=consistent-premises open-node (rest mmapps) existp-labels clsedp-labels :kind kind)))
	    (t
	     ;; -> normalizing case
	     ;; no open-node -> no checking nessessary!
	     (cons (list mmapp exist-prems clsed-prems)
		   (pplan=consistent-premises open-node (rest mmapps) existp-labels clsedp-labels :kind kind)))))))


(defun pplan=handling-support-tupel (pds agenda task method goal-matchings support-tupel
					 &key (kind 'standard) (mmatchings 'choose)) 
  
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, an agenda, the choosen task and method, the possible goal-matchings of"
		    "task-node and method, and the support-tupel, signing wich supports are allowed.")
	   (effect  "None.")
	   (value   "Tries to find applicable matchings to the support-tupel (that means using only"
		    "supports in the supports-tupel) with the given goal-matchings."
		    "When key word mmatchings is all then all found ones are returned."
		    "When the key word is choose then one is choosen and returned."))
  
  (setf meth*current-method method)

  ;; (when (socket~find-socket :Service)
  ;; (MAPLE~LEAVE)
  ;; (MAPLE~ENTER))
  
  (when pplan*verbose
    (omega~message "**CHOOSE-SUPPORTS+PARAMS**: Checking the following support-Tupel: ~A" support-tupel))
  
  (let* ((current-cstr-pool (pds~constraint-pool pds))
	 (with-task (or (equal kind 'standard)
			(equal kind 'restricting)))
	 (open-node (if with-task
			(agenda~task-node task)
		      nil))
	 (supports (first support-tupel))
	 (parameters (second support-tupel))
	 (meth-parameters (meth~parameters method))
	 (meth-appl-cond (meth~application-condition method))
	 (meth-goal (meth~goal method))
	 (mvar-subst (if current-cstr-pool
			 (pds~cstrpool-bindings current-cstr-pool)
		       nil))
	 (matchings-with-goal+parameters
	  (if meth-parameters
	      ;; Match the given PARAMETERS to the first associated METH-PARAMETERS, the rest METH-PARAMETERS
	      ;; will be bound later
	      (meth~match-p (subseq meth-parameters 0 (length parameters))
			    parameters
			    goal-matchings
			    :one2one)
	    (if parameters
		nil
	      goal-matchings))))
    
    ;; This thing with the parameters looks a little bit weird, but ... AMEIER
    (if (null matchings-with-goal+parameters)
	(progn
	  (when pplan*verbose
	    (omega~message ";;;Delivering inconsistent parameters ~A!~%There is something wrong with the parameters!" parameters))
	  (when (and pplan*VIL-on
		     (string-equal kind 'standard))
	    (omega~message "~%Sorry, but cannot match parameters ~A with method ~A and goal ~A"
			   parameters
			   method
			   (agenda~task-node task)))
	  nil)
      
      (progn
	(when pplan*verbose
	  (omega~message "**CHOOSE-SUPPORTS+PARAMS**: After parameter matching we have the matchings: ~A" matchings-with-goal+parameters))
	
	;;(format t "~%THE SUPPORTS ARE: ~A" supports)
	
	;; Update the current formulas in the supports
	(mapcar #'(lambda (node)
		    (when (and mvar-subst
			       (pdsn~schematic-p node)
			       (null (pdsn~up-to-date node)))
		      (setf (pdsn~current-formula node)
			    (subst~apply mvar-subst (node~formula node)))
		      (setf (pdsn~up-to-date node) 't)))
		supports)
	
	(setf meth*cause-of-failure nil)
	
	(let* ((exist-prems (meth~existent-premises method))
	       (clsed-prems (meth~closed-premises method))
	       (complete-matchings
		(if (or exist-prems clsed-prems)
		    ;; there are premises!
		    (let* ((matchings-goal+paramters+prems (meth~match-p (append exist-prems clsed-prems)
									 supports
									 matchings-with-goal+parameters
									 :many2many))
			   (matchings&prems-tripels (if matchings-goal+paramters+prems
							(pplan=consistent-premises open-node
										   matchings-goal+paramters+prems
										   (mapcar #'keim~name exist-prems)
										   (mapcar #'keim~name clsed-prems)
										   :kind kind)
						      nil)))
		      
		      (progn
			(when pplan*verbose
			  (omega~message "**CHOOSE-SUPPORTS+PARAMS**: After premises macthing we have the matchings: ~A"
					 matchings-goal+paramters+prems))
			
			(when pplan*user-comments
			  (pplan=analyse-missing-supports-for-user-comments method supports matchings-with-goal+parameters))
			
			(when (and (null matchings-goal+paramters+prems)
				   pplan*VIL-on
				   (string-equal kind 'standard))
			  (omega~message "~%Sorry, but cannot find suitable premises in the supports ~A for the application of method ~A on goal ~A"
					 supports
					 method
					 (agenda~task-node task)))	
			(apply 'append
			       (mapcar #'(lambda (mprem-tripel)
					   (let* ((mmapp (meth~check-condition meth-appl-cond (first mprem-tripel) pds)))
					     (if mmapp
						 (if (consp mmapp)
						     (mapcar #'(lambda (mm)
								 (pplan=mmatching-create method
											 (meth~compute-parameters meth-parameters mm)
											 mm
											 current-cstr-pool
											 pplan*roc-state-description
											 (if meth-goal
											     ;; backward-method
											     open-node
											   ;; forward method
											   nil)
											 (second mprem-tripel)
											 (third mprem-tripel)
											 ))
							     mmapp)
						   (list (pplan=mmatching-create method
										 (meth~compute-parameters meth-parameters mmapp)
										 mmapp
										 current-cstr-pool
										 pplan*roc-state-description
										 (if meth-goal
										     ;; backward-method
										     open-node
										   ;; forward method
										   nil)
										 (second mprem-tripel)
										 (third mprem-tripel)
										 )))
					       ;; no mapping found! -> if kind = standard and meth*cause-of-failure given
					       ;; -> place in the control infiormation meth*cause-of-failure an entry broken-matchings
					       (progn
						 
						 (when pplan*user-comments
						   (omega~message "Application condition ~A of method ~A not satisfied with ~A"
								  (list (first meth*cause-of-failure)
									(second meth*cause-of-failure))
								  method
								  (third meth*cause-of-failure))
						   (omega~message "Do you need other supports or other parameters?")
						   (omega~message "Or is the method ~A not suitable to tackle your goal?" method))
						 
						 (pplan=set-broken-matching-entry! task method :kind kind)
						 ))))
				       matchings&prems-tripels))))
		  ;; there are no premises!
		  (let ((mmapp (meth~check-condition meth-appl-cond matchings-with-goal+parameters pds)))
		    (if mmapp
			(if (consp mmapp)
			    (mapcar #'(lambda (mm)
					(pplan=mmatching-create method
								(meth~compute-parameters meth-parameters mm)
								mm
								current-cstr-pool
								pplan*roc-state-description 
								(if meth-goal
								    ;; backward-method		       
								    open-node
								  ;; forward method
								  nil)
								))
				    mmapp)
			  (list (pplan=mmatching-create method
							(meth~compute-parameters meth-parameters mmapp)
							mmapp
							current-cstr-pool
							pplan*roc-state-description 
							(if meth-goal
							    ;; backward-method
							    open-node
							  ;; forward method
							  nil))))
		      ;; no mapping found! -> if kind = standard and meth*cause-of-failure given
		      ;; -> place in the control infiormation meth*cause-of-failure an entry broken-matchings
		      (pplan=set-broken-matching-entry! task method :kind kind))))))
	  
	  (when pplan*verbose
	    (omega~message "**CHOOSE-SUPPORTS+PARAMS**: At the end of matching we have the following complete matchings: ~A, we proceed to choose one of them."
			   complete-matchings))

	  (cond ((string-equal mmatchings 'choose) 
		 (pplan=choose-matching pds agenda task method complete-matchings :kind kind))
		((string-equal mmatchings 'all)
		 complete-matchings)
		(t
		 (omega~error "~%Something went wrong in function pplan=handling-support-tupel."))))))))

(defun pplan=analyse-missing-supports-for-user-comments (method supports matchings-with-goal+parameters)
  (let* ((given-method-premises (append (meth~existent-premises method)
					(meth~closed-premises method))))

    (dolist (method-prem given-method-premises)
      (let* ((matchings-goal+paramters+prems (meth~match-p (list method-prem)
							   supports
							   matchings-with-goal+parameters
							   :many2many)))
	(when (null matchings-goal+paramters+prems)
	  (omega~message "Method ~A needs a support of form ~A." method (node~formula method-prem))
	  (omega~message "But the given supports are of the form:")
	  (dolist (supp supports)
	    (omega~message "~A" (node~formula supp)))
	  (omega~message "Look for supports suitable for the method ~A or try another method!" method))))))
	     

(defun pplan=set-broken-matching-entry! (task method &key (kind 'standard))
  (declare (edited  "30-JUL-2001")
	   (authors Ameier)
	   (input   "A task and a method.")
	   (effect  "If a cause of failure was created and stored in meth*cause-of-failure, then this entry"
		    "is add to the broken-matchings slot of the control information of the task node.")
	   (value   "Undefined."))
  
  ;; -> if kind = standard and meth*cause-of-failure given
  ;; -> place in the control infiormation meth*cause-of-failure an entry broken-matchings
  
  (when (and meth*cause-of-failure
	     (equal kind 'standard))
    (when (and pplan*VIL-on
	       (string-equal kind 'standard))
      (omega~message "~%Sorry, but application of method ~A fails because the condition ~A is not satisfied"
		     method meth*cause-of-failure))
    (let* ((control (pdsj~control (node~justification (agenda~task-node task)))))
      (setf (keim::pdsc~broken-matchings control)
	    (append (keim::pdsc~broken-matchings control)
		    (list (list method meth*cause-of-failure)))))
    nil))

(defun pplan=choose-supports (pds agenda task method goal-matchings support-tupel &key (kind 'standard))
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, an agenda, the choosen task and method, the found goal-matchings"
		    "and ONE support-tupel (may nil).")
	   (effect  "Seeks for supports and parameters for applicable goal-matchings.")
	   (value   "An applicable matching if one exists, nil otherwise."))

  (when pplan*verbose
    (omega~message "**CHOOSE-SUPPORTS+PARAMS**: Incoming support-tupel: ~A" support-tupel)) 

  (let* ((with-task (or (equal kind 'standard)
			(equal kind 'restricting)))
	 ;; -> task exists!
	 (task-node (if with-task
			(agenda~task-node task)
		      nil))
	 (support-nodes (if with-task
			    (pds~node-supports task-node pds)
			  nil))
	 (selected-support-tupel-list
	  (if support-tupel
	      ;; if already set -> try this!
	      ;; this is always the case when kind = normalize since then support-tupel always set!
	      (list support-tupel)
	    (multiple-value-bind
		(support-tupel-list applied-crules)
		(cri~call (list (list support-nodes))
			  :kind 'supports
			  :task-node task-node
			  :task-formula (if task (node~formula task-node) nil)
			  :method method
			  :pds pds)

	      (setf pplan*fired-control-rules (append pplan*fired-control-rules applied-crules))
	      
	      (when pplan*verbose
		(omega~message "**CHOOSE-SUPPORTS+PARAMS**: Calling CRI for supports, receiving: ~A, CRI-FLAG: ~A"
			       support-tupel-list
			       applied-crules))
	      
	      (if applied-crules
		  ;; when crules applied -> try choosen ones:
		  support-tupel-list
		;; if no crules applied -> try all support nodes:
		(list (list support-nodes)))))))


    ;; try all available support tupels or ask user to select or give one
    (do* ((rest-support-tupel-list (if (null inac*strategy-ks-execution-interactive)
				       selected-support-tupel-list
				     (pplan=choose-supports+parameters-interactively selected-support-tupel-list
										     task
										     method
										     support-nodes))
				   (cond ((null inac*strategy-ks-execution-interactive)
					  (rest rest-support-tupel-list))
					 (success
					  nil)
					 (t
					  (pplan=choose-supports+parameters-interactively selected-support-tupel-list
											  task
											  method
											  support-nodes))))
	  (success nil))
	((or (null rest-support-tupel-list)
	     success)
	 success)
      (let* ((head-support-tupel (first rest-support-tupel-list)))
	(setf success (pplan=handling-support-tupel pds agenda task method (pplan=copy-matchings goal-matchings) head-support-tupel :kind kind))))))

(defun pplan=choose-supports+parameters-interactively (rest-support-tupel-list task method support-nodes)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "The list of the remaining pre-produced (cri?) support-tupels, the current method and"
		    "the support-nodes of the current task-node.")
	   (effect  "None.")
	   (value   "The user is asked to choose or to provide a support+parameters tupel."
		    "A list with this tupel as only element is returned, or nil is returned"
		    "if the user did not provide a tupel."))
  
  (let* ((suggestion-by-cri (first rest-support-tupel-list))
	 (task-node (agenda~task-node task))
	 (default-number (if suggestion-by-cri
			     (position suggestion-by-cri rest-support-tupel-list)
			   nil))
	 (text (concatenate 'string
			    (format nil "~%Choose the next Support-Tupel:")
			    (format nil "~%Current task is ~A with formula ~A"
				    task-node
				    (node~formula task-node))
			    (format nil "~%Method is ~A" method)
			    (format nil "~%Auto-Mode suggests number ~A" (if (numberp default-number) (+ default-number 1) nil))
			    (format nil "~%------------------------------")
			    (format nil "~%Remaining possibilities are:")))
	 (number-of-support-tupel  (omega~query-choice text
						       (append rest-support-tupel-list
							       (list "Create a new one interactively")
							       (list nil)
							       (list "Leave PPlanner"))
						       default-number))
	 (support-tupel (nth number-of-support-tupel rest-support-tupel-list)))
    
    (cond (support-tupel
	   (list support-tupel))
	  ((= number-of-support-tupel (length rest-support-tupel-list))
	   ;; create a new one interactively was choosen
	   (list (pplan=create-supports+parameters-tupel-interactively method support-nodes)))
	  ((= number-of-support-tupel (+ (length rest-support-tupel-list) 1))
	   ;; nil was choosen
	   nil)
	  ((= number-of-support-tupel (+ (length rest-support-tupel-list) 2))
	   ;; Leave PPlanner was choosen
	   (throw 'leave-pplanner
		  (exmes~create-interruption-message pplan*roc-state-description nil nil)))
	  (t
	   ;; a not specified option was choosen
	   ;; -> if suggestion-by-cri was given we take this, otherwise we take nil
	   (if suggestion-by-cri
	       (list suggestion-by-cri)
	     nil)))))

(defun pplan=create-supports+parameters-tupel-interactively (method support-nodes)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "A method and a list of support-nodes.")
	   (effect  "None.")
	   (value   "A support-tupel, which is created by calling the user interactively to"
		    "specify support-nodes and parameters."))

  (omega~message "~%Choose first the support-nodes to use.")
  (omega~message "(a subset of the allowed support-nodes ~A" support-nodes)

  (let* ((interactive-read-things ;(inac~interactive-questions (list 'support-nodes)
				  ;			      (list "Choose support-supports nodes (a list of nodes)")
				  ;			      (list 'ndline-list)
				  ;			      (list nil)))
	  (omega~query (list "Choose support-supports nodes (a list of nodes)"
			     'ndline-list
			     nil)))
	 (choosen-lines (first interactive-read-things))
	 (accepted-lines (remove-if-not #'(lambda (ch-line)
					    (find ch-line support-nodes))
					choosen-lines))
	 (rejected-lines (remove-if #'(lambda (ch-line)
					(find ch-line support-nodes))
				    choosen-lines))
	 (method-parameters (meth~parameters method)))
	 
    (when rejected-lines
      (omega~message "WARNING: The following choosen nodes are not in the legal support-node set and are therefor rejected: ~A"
		     rejected-lines))

    (omega~message "~%Choose now the parameters to use.")

    (if (null method-parameters)
	(progn
	  (omega~message "~%No parameters to choose since the method ~A has none." method)
	  (list accepted-lines))
      (let* ((choosen-parameters
	      (do* ((rest-parameters method-parameters (rest rest-parameters))
		    (back-things nil)
		    (stop nil))
		  ((or stop
		       (null rest-parameters))
		   back-things)
		(let* ((head-parameter (first rest-parameters))
		       (sort (ssterm~sort head-parameter))
		       (type (pplan=compute-type-to-sort sort))
		       (interactive-read-thingsii ;(inac~interactive-questions (list 'proceed)
						  ;			      (list "Proceed?")
						  ;			      (list 'boolean)
						  ;			      (list t))))
			(omega~query  (list "Proceed?"
					    'boolean
					    t))))
		  (if (null (first interactive-read-thingsii))
		      (setf stop 't)
		    (let* ((interactive-read-thingsiii ;(inac~interactive-questions (list 'parameter)
						       ;				   (list (format nil "Specify parameter of type ~A" type))
						       ;				   (list type)
						       ;				   (list nil)))
			    (omega~query (list (format nil "Specify parameter of type ~A" type)
					       type
					       nil)))
			   (choosen-param (first interactive-read-thingsiii)))

		      (setf back-things (append back-things (list choosen-param)))))))))

	(list accepted-lines choosen-parameters)))))

(defun pplan=compute-type-to-sort (sort)
  (declare (edited  "09-AUG-1999")
	   (authors Ameier)
	   (input   "A ssort.")
	   (effect  "None.")
	   (value   "The symbol of the corresponding type."))
  
  (let* ((name (keim~name sort)))

    (cond ((string-equal name 'pos)
	   'position)
	  ((string-equal name 'prln)
	   'ndline)
	  ((string-equal name 'term)
	   'term)
	  ((string-equal name 'prlnlist)
	   'ndline-list)
	  ((string-equal name 'problem)
	   'problem)
	  (t
	   (error "Found a not specified type sort in pplan=compute-type-to-sort: ~A." name)))))

#| -------------------------------------------------------------------------> Auswahl Matching ! |# 

;; 4. Matchings selection
;;    Known are: TASK + METHOD + (permitted by the CRI) matchings
;;    If Matchings nil -> return nil
;;    If Matchings exist -> Call CRI for matchings selection
;;                          If CRI fires -> Take the first of the returned (can be nil, if the CRI bans things!)
;;                          otherwise -> Take the first of the input matchings                                    

(defun pplan=choose-matching (pds agenda task method complete-matchings &key (kind 'standard))
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "The pds, the agenda, the choosen task, the choosen method and a list of all computed"
		    "matchings.")
	   (effect  "None.")
	   (value   "Cri is called on matchings,"
		    "If cri evaluates a rule, the first of the results of cri is returned,"
		    "otherwise simply the first of the input matchings."
		    "Nil, if the input matchings was nil."))
  
  (when pplan*verbose
    (omega~message "**CHOOSE-MMATCHING**: Entering to choose a matching, possible matchings are: ~A" complete-matchings))
  
  (let* ((with-task (or (equal kind 'standard)
			(equal kind 'restricting)))
	 (task-node (when with-task (agenda~task-node task)))
	 (just (when with-task (node~justification task-node)))
	 (control (when with-task (pdsj~control just)))
	 (already-applied-mmatchings ;;;;(append
				      (when (and with-task pplan*check-backtrack)
					(keim::pdsc~already-applied-mmatchings control))
				      ;;;; things should go here, or not?
				      )
	 (above-mappings (when with-task
			   (pplan=get-above-mappings task-node method)))
	 (remaining-mmatchings0 (remove-if #'(lambda (mmatching)
					       (find (pplan~mmatch-mmapp mmatching) above-mappings
						     :test #'pplan=mapping-is-subsumed-p))
					   complete-matchings))
	 (remaining-mmatchings1 (remove-if #'(lambda (mmatching)
					       (find mmatching already-applied-mmatchings :test #'pplan=matching-is-subsumed-p))
					   remaining-mmatchings0))
	 (rejected-mmatchings1 (remove-if-not #'(lambda (mmatching)
						  (find mmatching already-applied-mmatchings :test #'pplan=matching-is-subsumed-p))
					      complete-matchings)))
    
    (multiple-value-bind
	(remaining-mmatchings2 rejected-mmatchings2)
	(if (and with-task
		 (equal kind 'standard))
	    ;; if kind is restricting then there is no loop-detection performed! 
	    (pplan=loop-detection pds task-node remaining-mmatchings1)
	  (values remaining-mmatchings1 nil))
      
      (when pplan*verbose
	(omega~message "**CHOOSE-MMATCHING**: The following already-applied-mmatchings were found in the task: ~A"
		       already-applied-mmatchings)
	(omega~message "**CHOOSE-MMATCHING**: Cause they was found in the already applied mmatchings control information of the task the following mmatchings was rejected: ~A"
		       rejected-mmatchings1)
	(when pplan*loop-detection-depth
	  (omega~message "**CHOOSE-MMATCHING**: Because of LOOP-DETECTION the following mmatchings are removed: ~A" rejected-mmatchings2)))
      
      
      (if (null remaining-mmatchings2)
	  nil
	(progn
	  (setf cri*current-mmatchings remaining-mmatchings2)
	  (multiple-value-bind
	      (matchings applied-crules)
	      (cri~call remaining-mmatchings2
			:kind 'mmatchings
			:task task
			:task-node task-node
			:task-formula (if with-task
					  (node~formula task-node)
					nil)
			:method method
			:pds pds
			:mmatchings remaining-mmatchings2)

	    (setf pplan*fired-control-rules (append pplan*fired-control-rules applied-crules))
	    
	    (if applied-crules
		(when pplan*verbose
		  (omega~message "**CHOOSE-MMATCHING**: CRI fired for matchings ~A, mmatchings are: ~A"
				 applied-crules
				 matchings))
	      (when pplan*verbose
		(omega~message "**CHOOSE-MMATCHING**: CRI did not fire for matchings, mmatchings are: ~A"
			       matchings)))
	    
	    (let* ((randomized-matchings (rand~call-randomizer matchings :kind 'mmatchings)))

	      (when pplan*verbose
		(omega~message "**CHOOSE-MMATCHING**: After Randomization matchings are: ~A"
			       matchings))
	      
	      (if (null inac*strategy-ks-execution-interactive)
		  (first randomized-matchings)
		(pplan=choose-mmatching-interactively remaining-mmatchings2 randomized-matchings task method)))))))))


(defun pplan=get-above-mappings (node method)
  (declare (edited  "11-DEC-2002")
	   (authors Vxs)
	   (input   "A NODE and a METHOD.")
	   (effect  "None.")
	   (value   "A list of mappings that are contained in the above justifications of NODE and for that"
		    "where created during the application of METHOD on a more abstract planning level."))
  (let* ((above-justs (pdsj~above-justs (node~justification node)))
	 (applied-methods (mapcar #'(lambda (just)
				     (pds~inference-application
				      (just~method just)
				      (exp~actual-outline-pattern node omega*current-proof-plan just)))
				 above-justs)))
    (mapcan #'(lambda (meth just)
		(when (eq meth method)
		  (list (pdsj~subst just))))
	    applied-methods above-justs)))


(defun pplan=choose-mmatching-interactively (remaining-mmatchings selected-matchings task method)
  (declare (edited  "10-AUG-1999")
	   (authors Ameier)
	   (input   "The list of all (remaining) possible matchings, a flag signing wether crules were applied"
		    "and a second list of matchings, selected by the crules.")
	   (effect  "None.")
	   (value   "If inac*strategy-ks-execution-interactive is nil the first of the selected-matchings"
		    "if crules were applied otherwise the first of all-matchings."
		    "Otherwise the user is asked to choose a matching."))
  
  ;; If no crule fires then all MMatchings are in the selected-matchings!
  
  (let* ((task-node (agenda~task-node task))
	 (suggestion-by-cri (if selected-matchings
				(first selected-matchings)
			      nil))
	 (default-number (if suggestion-by-cri
			     (position suggestion-by-cri remaining-mmatchings)
			   nil))
	 (text (concatenate 'string
			    (format nil "~%Choose the MMATCHING:")
			    (format nil "~%Current task is ~A with formula ~A"
				    task-node
				    (node~formula task-node))
			    (format nil "~%Method is ~A" method)
			    (format nil "~%Auto-Mode suggests number ~A" (if (numberp default-number) (+ default-number 1) nil))
			    (format nil "~%---------------------------")
			    (format nil "~%The remaining possibilities are:")))
	 (number-of-mmatching (omega~query-choice text (append remaining-mmatchings (list nil) (list "Leave PPlanner")) default-number))
	 (mmatching (nth number-of-mmatching remaining-mmatchings)))
    
    (cond (mmatching
	   mmatching)
	  ((= number-of-mmatching (length remaining-mmatchings))
	   ;; nil was choosen
	   nil)
	  ((= number-of-mmatching (+ (length remaining-mmatchings) 1))
	   ;; Leave PPlanner was choosen
	   (throw 'leave-pplanner
		  (exmes~create-interruption-message pplan*roc-state-description nil nil)))
	  (t
	   ;; a not specified option was choosen
	   ;; -> if suggestion-by-cri was given we take this, otherwise we take nil
	   (if suggestion-by-cri
	       suggestion-by-cri
	     nil)))))

	    

#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ------------------------------------------------------ Application of a MATCHING -------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


(defun pplan=execute-outline-actions (task new-opens mmatching &optional (pds omega*current-proof-plan))
  (declare (edited  "12-MAY-2000")
	   (authors Vxs)
	   (input   "A list of nodes, a matching, the applied method, a task and a PDS.")
	   (effect  "Executes the actions and thereby can change reasons.")
	   (value   "Undefined."))
  (let* ((applied-method (pplan~matched-method mmatching))
	 (actions (meth~outline-actions applied-method))
	 (proc-content (car (meth~procedural-content applied-method))))
    (when (and actions (not (pplan=tactic-application? proc-content)))
      (let ((reasons (when new-opens
		       (pdsj~reasons (node~justification (first new-opens))))))
	(dolist (action actions) 
	  (meth~execute-action applied-method action (pplan~mmatch-mmapp mmatching)
			       pds reasons (when task (pdsn~hyps task))))))))

(defun pplan=tactic-application? (proc-content)
  (declare (edited  "19-MAY-2000")
	   (authors Vxs)
	   (input   "A procedural content.")
	   (effect  "None.")
	   (value   "T if the procedural content indicates that the method is a tactic application. O/w NIL."))
  (and (or (stringp proc-content)
	   (symbolp proc-content))
       (string-equal proc-content 'apply-tactic)))

(defun pplan=apply-mmatching! (task mmatching pds &key (kind 'standard) (normalizing-tasks nil))
  (declare (edited  "01-APR-1998")
	   (authors Lassaad Ameier)
	   (input   "An agenda task, a method matching, a pds")
	   (effect  "Applies the method of MMATCHING to PDS.")
	   (value   "A tuple: MMATCHING, a list of new open nodes (method subgoals),"
		    "a list of new deduced nodes including new hypotheses, and"
		    "eventually additional returns, when this MMATCHING can be"
		    "applied, otherwise NIL."))

  (let* ((method (pplan~matched-method mmatching))
	 (mmapp (pplan~mmatch-mmapp mmatching))
	 (goal (pplan~mmatch-goal mmatching))
	 (just-parameters (pplan~mmatch-parameters mmatching))
	 (exist-prems (pplan=recreate-objects (meth~real-existent-premises method)
					      mmapp))
	 (clsed-prems (pplan=recreate-objects (meth~closed-premises method)
					      mmapp))
	 (cstr-state (pplan~mmatch-cstr-state mmatching))
	 (meth-prems (meth~premises method))

	 ;; The parameters, before applying meta-variable bindings to the MMAPP and
	 ;; before carrying out the computations.
	 ;; NOTE: Parameters which correspond to the state of input objects, e.g., a
	 ;; schematic goal or a meta-variable within such a goal, they must be bound
	 ;; at the latest during the evaluation of the application conditions. They
	 ;; may not be bound by an application computation.

	 )

    ;; 1.): when CSTR-STATE is not NIL and its binding empty (substitution), then
    ;; this means that the constraint evaluation of METHOD engenders a binding of some pds
    ;; meta-variables. Such meta-variables may occur in the associated pds objects to the method
    ;; variables stated in MMAPP. We have therefore to apply this BINDING to this terms in order
    ;; to update them, before to create new pds objects. Determining the PARAMETERS before taking
    ;; this changement of MMAPP ensures that this PARAMETERS remain unchanged even when they contain
    ;; bounded meta-variables:
    ;; Short: The binding of the constraint store (Attention: Here still the local binding! not the global)
    ;;        is applied on the whole method mappings!
    ;;        (later the binding of the constraint store is set global!)
    
    (when pplan*verbose
      (omega~message " The ~S is applied!!!!!!!!!!" method)
      (omega~message "=================================="))
      
    (when cstr-state
      (let ((binding (pds~cstrpool-bindings cstr-state)))
	(when (and binding (not (subst~empty-p binding)))
	  (let* ((mmapp-subst (meth~mapp-subst mmapp))
		 (updated-codom (meth~subst-apply binding (subst~codomain mmapp-subst)))
		 (updated-subst (meth~subst-create (subst~domain mmapp-subst) updated-codom)))
	    (meth~mapp-new-subst mmapp updated-subst)))))

    ;; 2.) carry out the outline-computations
    (meth~carry-out-computations (meth~outline-computations method) (meth~mapp-new-constraint mmapp T))
    (meth~mapp-new-constraint mmapp (if cstr-state cstr-state T))

    ;; 3.) Create and insert the non-existent-outlines
    (multiple-value-bind
	(mmatching new-opens new-supps additonal-returns)
	(pplan=create-and-insert-non-existent-outlines! task mmatching clsed-prems exist-prems just-parameters mmapp method meth-prems pds goal cstr-state :kind kind :normalizing-tasks normalizing-tasks)
      
      ;; 4.) Set the constraint store
      (pplan=insert-new-constraint-store! pds cstr-state)
      
      (values mmatching
	      new-opens
	      new-supps
	      additonal-returns))))

(defun pplan=recreate-objects (objects mmapp)
  (cond ((listp objects)
	 (apply #'append
		(mapcar #'(lambda (obj)
			    (pplan=recreate-objects obj mmapp))
			objects)))
	((meth~meta-node-p objects)
	 (let ((inst (meth~object-instance objects mmapp)))
	   (when inst inst)))
	((meth~node-p objects)
	 (let ((inst (meth~object-instance objects mmapp)))
	   (when inst
	     (list inst))))
	(T NIL)))

(defun pplan=create-and-insert-non-existent-outlines! (task mmatching clsed-prems exist-prems just-parameters mmapp method meth-prems pds goal cstr-state &key (kind 'standard) (normalizing-tasks nil))
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The task, the mmatching, the justification-parameters, the mapping, the method, the"
		    "premises of the method application, the pds, the goal and the (new) constraint-store"
		    "(may nil, if no new constraints was produced.")
	   (effect  "The new nodes are created and inserted into the pds (mit allem was dazu gehoert!).")
	   (value   "Multiple-Value:"
		    "First: The mmatching."
		    "Second: The new open-nodes."
		    "Third: The new closed nodes."
		    "Fourt: Additional-Return (was immer das ist ...)."))
  
  ;; 1: Completion of the outline
  (multiple-value-bind
      (concs prems new-nodes)
      (meth~complete-outlines (meth~conclusions method) meth-prems mmapp)

    ;; 2. Ina ???
    (when (and (fboundp 'ina~auto-add-goals)
	       goal)
      (ina~auto-add-goals (list goal)))

    ;; 3. Set hyps ???
    (mapcar #'(lambda (new-node)
		(let ((new-just (node~justification new-node)))
		  (when (and new-just
			     (infer~dummy-p (just~method new-just))
			     (not (infer~open-p (just~method new-just))))
		    (setf (pdsn~hyps new-node) (list new-node)))))
	    new-nodes)

    ;; Some output ...
    (when pplan*verbose
      (omega~message "deletes ...")
      (mapcar #'(lambda (prem)
		  (omega~message "   ~S : ~S"
				 (keim~name prem)
				 (node~formula prem)))
	      (append clsed-prems (when goal (list goal))))
      (omega~message "and adds ...")
      (mapcar #'(lambda (new-node)
		  (let ((new-just (node~justification new-node)))
		    (omega~message "   ~S : ~S |- ~S           ~S"
				   (keim~name new-node)
				   (mapcar #'keim~name (pdsn~hyps new-node))
				   (node~formula new-node)
				   (if (and (node~justification new-node)
					    (pdsn~open-node-p new-node))
				       'OPEN
				     '!))))
	      new-nodes))
    
    (let* ((concs-outline-pattern (mapcar #'keim~name concs))
	   
	   (new-hyps (remove-if-not #'pdsn~hypothesis-node-p
				    new-nodes))
	   (old-prems (append exist-prems clsed-prems))
	   ;; REMARK:
	   ;; IN AN ERALIER VERSION THERE WAS ONLY:
	   ;; (new-prems (pplan=set-difference prems old-prems))
	   ;; BUT IT COULD HAPPEN (E.G., method simplify-numerical-m-b) that in prems were closed premisses not
	   ;; mentioned in exist-prems or clsed-prems
	   ;; THEREFORE we added the remove-if-not #'pdsn~open-node-p part
	   ;; AMEIER (16.8.2000)
	   (new-prems (remove-if-not #'pdsn~open-node-p (pplan=set-difference prems old-prems)))
	   (open-exist-prems (remove-if-not #'pdsn~open-node-p exist-prems))
	   (open-prems (append new-prems open-exist-prems))
	   (new-concs (remove goal concs))
	   (supps&prems (append old-prems (pplan=set-difference new-nodes concs)))
	   (parameters (if (some #'null just-parameters)
			   (mapcar #'(lambda (pre-param mparam)
				       (if pre-param pre-param
					 (meth~mapp-get-component mparam mmapp :both)))
				   just-parameters (meth~parameters method))
			 just-parameters))
	   (proc-cont (car (meth~procedural-content method)))
	   )
      
      ;; 4. Critics handling
      (pplan=handle-critics! pds method parameters)

      ;; 5. Set the justification of the conclusion, extend the plan steps and set the corresponding reasons.
      (pplan=set-justs-and-plan-steps-and-constraint-store-and-reasons! pds concs concs-outline-pattern
									method meth-prems mmapp goal task
									cstr-state new-hyps prems
									parameters supps&prems
									(pplan~mmatch-cstr-state-before mmatching)
									:normalizing-tasks normalizing-tasks
									)
      
      ;; 6. Insert the new nodes into the PDS
      ;;(unless (pplan=tactic-application? proc-cont)

      (dolist (node new-nodes)
	(when (null (find node (prob~proof-steps pds)))
	  (pds~insert-node! node pds)))

      (when (pplan=tactic-application? proc-cont)
	(dolist (node new-nodes)
	  (pplan=update-formulas-by-mv-bindings! node)))    
      ;; if the proc-copntent is to apply-tactic
      ;; -> it can be happen that meta-variables have to be handled separately
      ;;    since the application of a tactic contains not, that the meta-variables are bound correctly!      
      
      ;;(mapcar #'(lambda (node)
      ;;	       (pds~insert-node! node pds))
      ;;	    new-nodes)
      
      ;; 7.) Inherit the supports
      (let* ((additional-returns (pplan=inheritance-of-supports task pds goal prems new-concs 
								open-prems clsed-prems old-prems
								new-prems open-exist-prems new-hyps
								proc-cont concs prems
								:kind kind)))

	
	
	;; It's already done by inserting it in pplan=inheritance-of-supports function
	;; (first case in the cond loop)
	;; (pplan=change-reason-structure-if-tactic-application! method concs prems)
	
	;; This is rather the same:
	;;(cond ((string-equal extra-return "PREMS-SUPPORTED-NODES")
	;;	     (values mmatching new-prems (append new-concs new-hyps) additional-returns))
	;; 	    (T
	;;   	     (values mmatching new-prems (append new-concs new-hyps) additional-returns)))

	(values mmatching
		new-prems
		(append new-concs new-hyps)
		additional-returns)))))

(defun pplan=update-formulas-by-mv-bindings! (node)
  (declare (edited  "31-JUL-2000")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "If the node contains some meta-variables that are bound in the current meta-variable binding,"
		    "then this binding is applied destructively on the formula of the node.")
	   (value   "The new formula."))
  (let* ((meta-vars-in-node (remove-if-not #'meta~p (data~all-substructs (node~formula node))))
	 (current-cs (pds~constraint-pool omega*current-proof-plan))
	 (current-binding (if (null current-cs)
			      (subst~create nil nil)
			    (pds~cstrpool-bindings current-cs)))
	 (mvs-in-current-binding (subst~domain current-binding))
	 (inter (intersection meta-vars-in-node mvs-in-current-binding)))
    (if inter
	(let* ((new-formula (if (remove-if-not #'data~abstr-p (subst~codomain current-binding))
				;; there are abstractions in the codomain of the mvar-subst
				;; -> we have to beta-normalize
				(beta~normalize (subst~apply current-binding (node~formula node)))
			      ;; otherwise we can spare to beta-normalize (whcih is a verry complex operation)
			      (subst~apply current-binding (node~formula node)))))
	  (setf (node~formula node)
		new-formula)
	  new-formula)
      nil)))

      
(defun pplan=change-reason-structure-if-tactic-application! (method concs prems)
  (declare (edited  "22-MAY-2000")
	   (authors Ameier)
	   (input   "A method, the conclusions, and the premises of an applied step.")
	   (effect  "When the applied-method is a tactic-application the reason structure of the"
		    "new nodes involved in this step is changed.")
	   (value   "Undefined."))
  (let* ((proc-cont (car (meth~procedural-content method))))

    (when (pplan=tactic-application? proc-cont)      ;;;  VS new stuff

      (let ((old-prems (just~premises (pdsj~below (node~justification (car concs)))))
	    (hyps (set-difference (tac~set-manage-list #'union (mapcar #'pdsn~hyps prems))
				  (tac~set-manage-list #'union (mapcar #'pdsn~hyps concs)))))
	(tac~change-pds-structure concs old-prems hyps)
	;; ADDS IN EACH HYP ALSO REASONS!
	;; THIS IS NECESSARY FOR DELETING THINGS! (with the delete command)
	;; I'm not sure how it affects backtracking ...
	(mapcar #'(lambda (conc)
		    (mapcar #'(lambda (hyp)
				(pdsn~insert-reason! hyp (pdsj~own-reason (node~justification conc))))
			    hyps))
		concs)
	))))

(defun pplan=set-difference (set1 set2)
  (remove-if #'(lambda (x) (find x set2)) set1))

(defun pplan=inheritance-of-supports (task pds goal prems new-concs open-prems clsed-prems old-prems new-prems open-exist-prems new-hyps  proc-cont concs prems &key (kind 'standard))
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The task, the pds, the goal, the new conclusions, the open premises,"
		    "the closed premsies and the old premises.")
	   (effect  "Setting of all global (in the pds) and local (in the open goals) supports.")
	   (value   "Additional-return (weiss erhlich nicht genau was das ist ..."))
  
  (let* ((additional-returns nil))
	    
    ;; 1.) Insert CONCS in the PDS supports and REMOVE them from the supports
    ;; of the open nodes which are not supported by OLD-PREMS:
    (unless (or goal open-prems)
      (when (and old-prems
		 (subsetp prems (pds~support-nodes pds)))
	;; If all premises are closed and they occur in the pds supports, then the
	;; conclusions should be inserted into this node list:
	(setf (pds~support-nodes pds) (append new-concs (pds~support-nodes pds)))
	(dolist (on (pds~open-nodes pds))
	  (let ((on-usps (pdsj~unsponsors (node~justification on))))
	    (when (and on-usps
		       (some #'(lambda (n) (find n on-usps)) old-prems))
	      (setf (pdsj~unsponsors (node~justification on))
		    (union on-usps new-concs)))))
	))
    
    ;; 2.) Set the local supports
    (cond ((plan=tactic-application? proc-cont)      ;;;  VS new stuff ;; NEW VOLKER ;; war vorher if
	   (let ((old-prems (just~premises (pdsj~below (node~justification (car concs)))))
		 (hyps (set-difference (tac~set-manage-list #'union (mapcar #'pdsn~hyps prems))
				       (tac~set-manage-list #'union (mapcar #'pdsn~hyps concs)))))
	     (tac~change-pds-structure concs old-prems hyps)
	     ;;(tac~change-reason-structure concs old-prems hyps)
	     (mapcar #'(lambda (conc)
			 (mapcar #'(lambda (hyp)
				     (pdsn~insert-reason! hyp (pdsj~own-reason (node~justification conc))))
				 hyps))
		     concs)
	     ;;(mapcar #'(lambda (conc)        ;;; that could be a source of confussion!!!!!  VS
	     ;;		(setf (pdsj~predecessor (node~justification conc)) nil)
	     ;;		(setf (pdsj~successor (node~justification conc)) nil))
	     ;;	    concs)))
	     ))
	  ((or (equal kind 'standard)
	       (equal kind 'restricting))
	   
	   ;; A planning step wrt. ONE TASK
	   (cond (open-prems
		  ;; A backward method or a forward method with open nodes:
		  ;; 1) Inheritance of the sponsors and unsponsors of the GOAL resp. the
		  ;; TASK node to the add-premises (NEW-PREMS) and then to the other open
		  ;; premises and deletion of the (-) premises from the supports of these nodes:
		  ;; 2) In the case of forward method, delete the (-) premises from the supports
		  ;; of the TASK node and add conclusions NEW-CONCS to these supports:
		  (let* ((goal-just (if goal
					(node~justification goal)
				      (node~justification (agenda~task-node task))))
			 (sponsors (pdsj~sponsors goal-just))
			 (unsponsors (pdsj~unsponsors goal-just)))
		    ;; 1)
		    (mapcar #'(lambda (prem)
				(let ((prem-just (node~justification prem)))
				  (setf (pdsj~sponsors prem-just) sponsors)
				  (setf (pdsj~unsponsors prem-just) (union unsponsors clsed-prems))))
			    new-prems)
		    (mapcar #'(lambda (prem)
				(let ((prem-just (node~justification prem)))
				  (setf (pdsj~sponsors prem-just)
					(union sponsors (pdsj~sponsors prem-just)))
				  (setf (pdsj~unsponsors prem-just)
					(union (intersection unsponsors (pdsj~unsponsors prem-just)) clsed-prems))))
			    open-exist-prems)
		    ;; 2)
		    (unless goal
		      (pds~delete-sponsors (agenda~task-node task) clsed-prems pds)
		      (pds~add-sponsors (agenda~task-node task) new-concs pds))))
		 (clsed-prems
		  ;; A forward method wrt. TASK without subgoals but with deletions:
		  ;; Delete the (-) premises from the supports of the TASK node and add
		  ;; conclusions NEW-CONCS to these supports:
		  (progn (pds~delete-sponsors (agenda~task-node task) clsed-prems pds)
			 (pds~add-sponsors (agenda~task-node task) new-concs pds)))
		 (t
		  ;; A forward method wrt. TASK without subgoals and without deletions:
		  ;; add the conclusions NEW-CONCS to the supports of all open nodes which are
		  ;; supported from the premises of this method:
		  (let ((prems-supported-nodes nil))
		    (mapcar #'(lambda (node)
				(when (subsetp old-prems (pds~node-supports node pds))
				  (setf prems-supported-nodes
					(cons node prems-supported-nodes))))
			    (pds~open-nodes pds))
		    (setf additional-returns prems-supported-nodes)
		    (mapcar #'(lambda (psn)
				(pds~add-sponsors psn new-concs pds))
			    prems-supported-nodes)))))
	  (t
	   ;; -> kind is normalizing
	   ;; -> A planning step wrt. ALL task
	   
	   (if (and goal new-prems)
	       (omega~error ";;;plan~~apply-mmatching: You have to inherit the supports of ~A to ~A."
			    goal new-prems)
	     ;; We have to consider the open nodes different from the open premises of the
	     ;; applied method and which are supported from the old premises of this method:
	     (let ((nodes (pplan=set-difference (pds~open-nodes pds) open-prems))
		   (prems-supported-nodes))
	       (mapcar #'(lambda (node)
			   (when (subsetp old-prems (pds~node-supports node pds))
			     (setf prems-supported-nodes
				   (cons node prems-supported-nodes))))
		       nodes)
	       
	       (setf additional-returns prems-supported-nodes)
	       
	       (when prems-supported-nodes
		 ;; Inherit the sponsors and unsponsors of the PREMS-SUPPORTED-NODES to the new (open) premises:
		 (let ((sps (pdsj~sponsors (node~justification (first prems-supported-nodes))))
		       (usps (pdsj~unsponsors (node~justification (first prems-supported-nodes)))))
		   (dolist (psn (rest prems-supported-nodes))
		     (setf sps (union sps (pdsj~sponsors (node~justification psn))))
		     (setf usps (intersection usps (pdsj~unsponsors (node~justification psn)))))
		   (dolist (np new-prems)
		     (setf (pdsj~sponsors (node~justification np)) sps
			   (pdsj~unsponsors (node~justification np)) usps)))
		 ;; Delete the (-) premises CLSED-PREMS from the supports of the PREMS-SUPPORTED-NODES
		 ;; and add the (+) conclusions NEW-CONCS to their supports:
		 (dolist (psn prems-supported-nodes)
		   (pds~delete-sponsors psn clsed-prems pds)
		   (pds~add-sponsors psn new-concs pds)))))))
    
    ;; LC: Each new hypothesis must sponsor each add-premise that depends on it.
    ;; IMPORTANT: the insertion of new hypotheses must occur after the inheritance
    ;; of the goal supports to the subgoals:
    
    (dolist (prem new-prems)
      (let ((sps (intersection new-hyps (pdsn~hyps prem))))
	(when sps
	  (pds~add-sponsors prem sps pds))))
    
    additional-returns))



(defun pplan=set-justs-and-plan-steps-and-constraint-store-and-reasons! (pds concs concs-outline-pattern method meth-prems mmapp goal task cstr-state new-hyps prems parameters supps&prems cstr-state-before &key (normalizing-tasks nil))
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The pds, the new conclusions, the conclusion outline pattern, the applied method,"
		    "the method premises, the matching, the goal and the constraint-store.")
	   (effect  "The justifications are set, the plan-steps entry is updated and the"
		    "reasons are set.")
	   (value   "UNdefined."))   

  (let* ((proc-cont (car (meth~procedural-content method))))
    
    (mapcar #'(lambda (conc)
		(let ((prems-outline-pattern (meth~prems-outline-pattern meth-prems mmapp)))
		  
		  ;; 1. Set the JUSTS + OPEN NODES etc.
		  (cond  ((and (pplan=tactic-application? proc-cont)
			       (eq conc goal))
			  ;; CONC was open and is now closed by METHOD:
			  (let* ((old-just (node~justification conc))
				 (new-just (pdsj~closed-just-create (meth~inference method)
								    prems parameters "expanded" mmapp
								    (append (substitute "EXISTENT" (keim~name conc)
											concs-outline-pattern)
									    prems-outline-pattern))))
			    (setf (node~justification conc) (pdsj~insert-just-above old-just new-just))
			    (setf (pds~open-nodes pds) (remove conc (pds~open-nodes pds)))
			    (setf (pdsj~reasons new-just) (append (pdsj~reasons new-just) (pdsj~reasons old-just)))
			    (setf (pdsj~reasons old-just) nil)
			    (setf (pdsj~sponsors new-just) (pdsj~sponsors old-just))       ;; NEW VOLKER
			    (setf (pdsj~unsponsors new-just) (pdsj~unsponsors old-just))   ;; NEW VOLKER
			    ))
			 ((pplan=tactic-application? proc-cont)
			  ;; CONC is deduced by a forward application of the method tactic:
			  (let* ((old-just (node~justification conc))
				 (new-just (pdsj~closed-just-create (meth~inference method)
								    prems parameters "expanded" mmapp
								    (append (substitute "NONEXISTENT" (keim~name conc)
											concs-outline-pattern)
									    prems-outline-pattern))))
			    (setf (node~justification conc) (pdsj~insert-just-above old-just new-just))
			    (setf (pdsj~reasons new-just) (append (pdsj~reasons new-just) (pdsj~reasons old-just)))
			    (setf (pdsj~reasons old-just) nil)
			    (setf (pdsj~sponsors new-just) (pdsj~sponsors old-just))       ;; NEW VOLKER
			    (setf (pdsj~unsponsors new-just) (pdsj~unsponsors old-just)))) ;; NEW VOLKER 
			 ((eq conc goal)
			  
			  ;; CONC was open and is now closed by METHOD:
			  (let* ((conc-outline-pattern (append (substitute "EXISTENT" (keim~name conc)
									   concs-outline-pattern)
							       prems-outline-pattern))
				 (new-just (pdsj~closed-just-create (meth~inference method)
								    prems parameters "unexpanded" mmapp
								    conc-outline-pattern))
				 (old-just (node~justification conc))
				 (new-control (keim~copy (pdsj~control old-just))))   ;; NEW
			    
			    ;;(setf (pdsj~control new-just) (pdsj~control old-just))
			    (setf (pdsj~control new-just) new-control)                ;; NEW
			    (setf (pdsc~predecessor (pdsj~control new-just)) nil)     ;; NEW
			    (setf (pdsc~successor (pdsj~control new-just)) nil)       ;; NEW

			    (when (keim~plist (pdsj~control old-just))
			      (omega~warn "~%~%~%PLIST IS NOT COPIED! SEE PPLANNER"))
			    
			    (setf (node~justification conc) (pdsj~replace-justification! old-just new-just))
			    (setf (pds~open-nodes pds) (remove conc (pds~open-nodes pds )))))
			 
			 (t
			  ;; CONC is deduced by a forward application of the method tactic:
			  (let* ((conc-outline-pattern (append (substitute "NONEXISTENT" (keim~name conc)
									   concs-outline-pattern)
							       prems-outline-pattern))
				 (old-just (node~justification conc))
				 (new-just (pdsj~closed-just-create (meth~inference method)
								    prems parameters "unexpanded" mmapp
								    conc-outline-pattern))
				 (conc-just (if old-just
						(pdsj~insert-just-below old-just new-just)
					      new-just)))
			    
			    (setf (node~justification conc) conc-just))))
		  
		  ;; 2. Set a new last plan steps! 
		  (let ((conc-reason (pds~change-last-plan-step! conc pds)))

		    ;; small Hack for testing:
		    ;; Put the cstr-pool before the application of the methot on the reason
		    (keim~put conc-reason
			      'cstr-state-before
			      cstr-state-before)
		    
		    (keim~put conc-reason
			      'goal-list
			      (if normalizing-tasks
				  (mapcar #'(lambda (item)
					      (if (node~p item)
						  item
						(agenda~task-node item)))
					  normalizing-tasks)
				(if goal
				    nil
				  (list (agenda~task-node task)))))
		    		    
		    (keim~put conc-reason
			      'applied-crules
			      pplan*fired-control-rules)


		    
		    
		    ;; 3. set the reasons in the nodes!
		    (mapcar #'(lambda (pds-node)
				(pdsn~insert-reason! pds-node conc-reason))
			    (cons conc supps&prems))
		    
		    ;; Add the step in the current roc
		    ;; ATTENTION!!!: STEPS ALWAYS ONLY IN THE LOCAL ROC!
		    (when pplan*roc-state-description ;; this is used in analogy, where it is set to nil, so there are no side effects
		      (setf (roc~pplanner-steps pplan*roc-state-description)
			    (cons conc-reason (roc~pplanner-steps pplan*roc-state-description))))
		    
		    ;; 4. It is annotated in the constraint pool, that this plan step ends in this constraint pool
		    (when cstr-state
		      ;; The cstrpool of pds is changed by this method application, therefore
		      ;; we mark this by registering the corresponding reasons.
		      (setf (pds~cstrpool-plansteps cstr-state)
			    (cons conc-reason (pds~cstrpool-plansteps cstr-state))))
		    )))
	    (pplan=set-difference concs new-hyps)
	    ;; only non-hyps get a new
	    )))

(defun pplan=handle-critics! (pds method parameters)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "The pds, the method and the parameters of the method application.")
	   (effect  "If the methos is a critic, we have to insert it together with"
		    "the parameters into the applied critics slot of all open nodes.")
	   (value   "Undefined."))
  (when (meth~critic-p method)
    (mapcar #'(lambda (open-node)
		(setf (pdsn~applied-critics open-node)
		      (cons (cons method parameters) (pdsn~applied-critics open-node))))
	    (pds~open-nodes pds))))

(defun pplan=insert-new-constraint-store! (pds cstr-state)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "A pds and a constraint store (may nil).")
	   (effect  "If the constraint store exists, it is introduced as the pds~constraint-pool."
		    "Thereby it can be happened, that the binding is updated.")
	   (value   "Undefined."
		    "Remark: After this function the constraint store bindings slot contains"
		    "        the global bindings substitution!."))
  (when cstr-state
    (cond ((not (subst~empty-p (pds~cstrpool-bindings cstr-state)))
	   ;; new bindings: Compose old and new bindings, when old bindings exist
	   ;; o/w. take old bindings
	   (let ((previous (pds~cstrpool-previous cstr-state)))
	     (when (and (pds~constraint-pool-p previous)
			(not (subst~empty-p (pds~cstrpool-bindings previous))))
	       ;; Set bindings of the constraint store on the global bindings!!
	       (setf (pds~cstrpool-bindings cstr-state)
		     (subst~compose-substitution (pds~cstrpool-bindings cstr-state)
						 (pds~cstrpool-bindings previous))))))
	  
	  ((pds~constraint-pool-p (pds~cstrpool-previous cstr-state))

	   (setf (pds~cstrpool-bindings cstr-state)
		 (pds~cstrpool-bindings (pds~cstrpool-previous cstr-state)))))

    ;; Change the PDS constraint state

    (setf (pds~constraint-pool pds) cstr-state)))



#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ---------------------------------------------------- UPDATING OF THE ROCS --------------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#



;; If a step is done, this can lead to changes in all affected ROCS.
;; Here I have entried the updates so far:
;; 1.) For all pplanner ROCS involved in the task:
;;     The new-lines + the outline-lines are updated!
;;     In the forward case:   All new lines are added to the new-lines
;;                          + all new open lines are added to the outline-lines
;;     In the backward case:   All new lines are added to the new-lines
;;                          + the task is removed from the outline
;;                          + all new open lines are added to the outline-lines

(defun pplan=update-rocs-with-new-lines-forward! (task new-opens new-supps)
  (declare (edited  "16-JUN-1999")
	   (authors Ameier)
	   (input   "A task, and the new open lines and the new closed lines (describing a forward step).")
	   (effect  "All ROCS in the ROCS slot of the task are updated.")
	   (value   "Undefined."))

  (let* ((rocs (agenda~task-rocs task)))
    
    (mapcar #'(lambda (roc)
		(pplan=update-roc-forward! roc task new-opens new-supps))
	    rocs)))

(defun pplan=update-rocs-with-new-lines-backward! (task new-opens new-supps)
  (declare (edited  "16-JUN-1999")
	   (authors Ameier)
	   (input   "A task, and the new open lines and the new closed lines (describing a backward step).")
	   (effect  "All ROCS in the ROCS slot of the task are updated.")
	   (value   "Undefined."))

  (let* ((rocs (agenda~task-rocs task)))
    
    (mapcar #'(lambda (roc)
		(pplan=update-roc-backward! roc task new-opens new-supps))
	    rocs)))

(defgeneric pplan=update-roc-forward! (roc task new-opens new-supps)
  (declare (edited  "16-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC, the current task, and new open lines and new closed lines (describing a forward plan step).")
	   (effect  "The Roc is updated.")
	   (value   "Undefined."))
  (:method ((roc roc+pplanner-state-description) task new-opens new-supps)

	   (setf (roc~pplanner-new-lines roc)
		 (append (roc~pplanner-new-lines roc) new-opens new-supps))

	   (setf (roc~pplanner-outline-lines roc)
		 (append (roc~pplanner-outline-lines roc) new-opens))

	   roc)
  (:method ((roc roc+lplanner-state-description) task new-opens new-supps)
	   nil))

(defgeneric pplan=update-roc-backward! (roc task new-opens new-supps)
  (declare (edited  "16-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC, the current task, and new open lines and new closed lines (describing a backward plan step).")
	   (effect  "The Roc is updated.")
	   (value   "Undefined."))
  (:method ((roc roc+pplanner-state-description) task new-opens new-supps)

	   (setf (roc~pplanner-new-lines roc)
		 (append (roc~pplanner-new-lines roc) new-opens new-supps))

	   (setf (roc~pplanner-outline-lines roc)
		 (remove (agenda~task-node task)
			 (append (roc~pplanner-outline-lines roc) new-opens)))

	   roc)
  (:method ((roc roc+lplanner-state-description) task new-opens new-supps)
	   nil))

#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ---------------------------------------------------------- Normalizing + Restricting ---------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


(defun pplan=restrict-goals (goals agenda pds)
  (declare (edited  "16-AUG-1999")
	   (authors Ameier)
	   (input   "A list of open-nodes (or tasks), the current agenda and the current pds.")
	   (effect  "Tries to apply task-restricting methods on the open-nodes.")
	   (value   "Multiple-value:"
		    "First: The new agenda."
		    "Second: The new open-lines."
		    "Third: The new support-lines."))
  
  (when pplan*verbose
    (omega~message"**RESTRICTING-GOALS**: ~A" goals))
  
  (let* ((restricting-methods pplan*restriction-methods))
    ;;   (remove-if-not #'(lambda(method)
    ;;	 (find :restricting (meth~reasoning method)))
    ;;  pplan*methods)))
    
    (if restricting-methods

	(progn
	  (when pplan*verbose
	    (omega~message "**RESTRICTING-GOALS**: Found the following restricting methods: ~A" restricting-methods))
	  
	  (do* ((rest-goals goals)
		(new-open-lines nil)
		(current-agenda agenda)
		(new-sup-lines nil))
	      ((null rest-goals)
	       (values current-agenda
		       new-open-lines
		       new-sup-lines))
	    
	    (let* ((new-rest-goals
		    (apply #'append
			   (mapcar #'(lambda (goal)
				       
				       (when pplan*verbose
					 (omega~message "**RESTRICTING-GOALS**: GOAL ~A" goal))
				       
				       (let* ((task (if (agenda~goal-or-goal-schema-task-p goal)
							goal
						      (find goal (agenda~all-tasks current-agenda)
							    :test #'(lambda (goal task)
								      (eq goal (agenda~task-node task))))))
					      (mmatching (pplan=choose-method pds agenda task (mapcar #'list restricting-methods)
									      :kind 'restricting)))
					 (if mmatching
					     (progn

					       (when pplan*verbose
						 (omega~message "**RESTRICTING-GOALS**: Found restricting matching ~A" mmatching))
					       
					       (multiple-value-bind
						   (new-agenda new-goals new-supps)
						   (pplan=apply-plan-step! task mmatching pds agenda
									   :kind 'restricting)
						 
						 (multiple-value-bind
						     (new-agendaii new-goals-from-norm new-supps-from-norm)
						     (pplan=normalize-supports new-supps new-goals new-agenda pds)
						   
						   (setf current-agenda new-agendaii)
						   (setf new-open-lines (append new-open-lines new-goals new-goals-from-norm))
						   (setf new-sup-lines (append new-sup-lines new-supps new-supps-from-norm))
						   
						   new-goals)))
					   (progn
					     (when pplan*verbose
					       (omega~message "**RESTRICTING-GOALS: No mmatching found for ~A" task))
					     nil))))
				   rest-goals))))
	      
	      (setf rest-goals new-rest-goals))))
      
      (progn
	(when pplan*verbose
	  (omega~message "**RESTRICTING-GOALS**: No RESTRICTION applied."))
	
	(values agenda nil nil)))))
   
(defun pplan=normalize-supports (supps goals agenda pds)
  (declare (edited  "16-AUG-1999")
	   (authors Ameier)
	   (input   "A list of support nodes, a list of goals, an agenda and a pds.")
	   (effect  "Applies normalizing methods on such supports which are in the support-list"
		    "of one of the goals.")
	   (value   "Multiple-Value:"
		    "First: The new agenda."
		    "Second: A list of new produced goals."
		    "Third: A list of new produced supports."))
  
  (when pplan*verbose
    (omega~message"**NORMALIZING-SUPPORTS**: ~A" supps))
  
  (let* ((normalizing-methods pplan*normalization-methods)
	 ;;(remove-if-not #'(lambda(method)
	 ;;			 (find :normalizing (meth~reasoning method)))
	 ;;		     pplan*methods))
	 (all-goals-supports (apply #'append (mapcar #'(lambda (goal)
							 (if (agenda~goal-or-goal-schema-task-p goal)
							     (pds~node-supports (agenda~task-node goal))
							   (pds~node-supports goal)))
						     goals)))
	 (remaining-supps (remove-if-not #'(lambda (supp)
					     (find supp all-goals-supports))
					 supps)))

    (if (and normalizing-methods
	     remaining-supps)

	(progn
	  (when pplan*verbose
	    (omega~message "**NORMALIZING-SUPPORTS**: Found the following normalizing methods: ~A" normalizing-methods))
	  
	  (do* ((rest-supps supps)
		(current-agenda agenda)
		(new-sup-lines nil)
		(new-open-lines nil))
	      ((null rest-supps)
	       (values current-agenda
		       new-open-lines
		       new-sup-lines))
	    
	    (let* ((new-rest-supps
		    (apply #'append
			   (mapcar #'(lambda (supp)
				       
				       (when pplan*verbose
					 (omega~message "**NORMALIZING-SUPP**: Normalizing Supp: ~A" supp))
				       
				       (let* (;;(goal-to-supp (find supp goals
					      ;;			  :test #'(lambda (supp goal)
					      ;;				    (find supp (if (agenda~goal-or-goal-schema-task-p goal)
					      ;;						   (pds~node-supports (agenda~task-node goal))
					      ;;						 (pds~node-supports goal))))))
					      ;;(task-to-supp (if (agenda~goal-or-goal-schema-task-p goal-to-supp)
					      ;;	 		goal-to-supp
					      ;;		      (find goal-to-supp (agenda~all-tasks current-agenda)
					      ;;			    :test #'(lambda (goal task)
					      ;;				      (eq goal (agenda~task-node task))))))
					      (restriction-list (mapcar #'(lambda (method)
									    (list method (list supp)))
									normalizing-methods))
					      (mmatching (pplan=choose-method pds agenda nil ;;task-to-supp
									      restriction-list
									      :kind 'normalizing)))
					 
					 (if mmatching
					     (progn
					       
					       (when pplan*verbose
						 (omega~message "**NORMALIZING-SUPPORTS**: Found normalizing mmatching ~A"
								mmatching))
					       
					       (multiple-value-bind
						   (new-agenda new-opens new-supps)
						   (pplan=apply-plan-step! nil ;;task-to-supp
									   mmatching pds agenda
									   :kind 'normalizing
									   :normalizing-tasks goals)
						 
						 (setf current-agenda new-agenda)
						 (setf new-open-lines (append new-open-lines new-opens))
						 (setf new-sup-lines (append new-sup-lines new-supps))
						 
						 new-supps))
					   
					   (progn
					     (when pplan*verbose
					       (omega~message "**NORMALIZING-SUPPORTS: No mmatching found for ~A" supp))
					     nil))))
				   rest-supps))))

	      ;; (format t "~%~%THE NEW REST-SUPPS ARE: ~A" new-rest-supps)
	      
	      (setf rest-supps new-rest-supps))))
      
      (progn
	(when pplan*verbose
	  (omega~message "**NORMALIZING-SUPPORTS**: No NORMALIZING applied."))
	
	(values agenda nil nil)))))

(defun pplan=restrict-and-normalize-during-planning (agenda pds opens supps)
  (declare (edited  "16-AUG-1999")
	   (authors Ameier)
	   (input   "An agenda, a pds, a list of open-nodes and a list of support-nodes.")
	   (effect  "Restrict and normalizes the nodes.")
	   (value   "A new agenda."))

  (when pplan*verbose
    (omega~message "**PPLANNER**: After applying a method: NORMALIZING AND RESTRICTING!"))

  (let* ((interactive inac*strategy-ks-execution-interactive))
    
    ;; interactive nil during normalization
    (setf inac*strategy-ks-execution-interactive nil)
    
    (multiple-value-bind
	(new-agenda new-open new-supps)
	(pplan=normalize-supports supps opens agenda pds)
      
      (multiple-value-bind
	  (new-agendaii new-open new-supps)
	  (pplan=restrict-goals opens new-agenda pds)

	(setf inac*strategy-ks-execution-interactive interactive)
	
	new-agendaii))))
      
(defun pplan=restrict-and-normalize-at-beginning (&key (reinvokation nil))
  (declare (edited  "16-AUG-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Normalizes and restricts the tasks of the roc and"
		    "all their supports.")
	   (value   "UNdefined."))

  (when pplan*verbose
	(omega~message "**PPLANNER**: AS FIRST: NORMALIZING AND RESTRICTING!"))

  (let* ((interactive inac*strategy-ks-execution-interactive))

    ;; interactive nil during normalization
    (setf inac*strategy-ks-execution-interactive nil)

    (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	   (steps (roc~pplanner-steps pplan*roc-state-description))
	   (agenda (pds~agenda pds))
	   (all-tasks (agenda~all-tasks agenda))
	   (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						   (and (agenda~goal-or-goal-schema-task-p task)
							(find pplan*roc-state-description (agenda~task-rocs task))))
					       all-tasks))
	   (all-supports (remove-duplicates (apply #'append (mapcar #'(lambda (task)
									(pds~node-supports (agenda~task-node task)))
								    goal-tasks-with-roc))))
	   (goal-tasks-with-roc-and-reason (remove-if #'(lambda (goal)
							  (intersection steps
									(pdsj~reasons
									 (node~justification
									  (agenda~task-node goal)))))
						      goal-tasks-with-roc))
	   (supports-with-reason (remove-if #'(lambda (supp)
						(intersection steps
							      (pdsj~reasons
							       (node~justification supp))))
					    all-supports)))
      
      (multiple-value-bind
	  (new-agenda new-open new-supps)
	  (pplan=normalize-supports (if reinvokation
					supports-with-reason
				      all-supports)
				    goal-tasks-with-roc agenda pds)
	
	(multiple-value-bind
	    (new-agendaii new-open new-supps)
	    (pplan=restrict-goals (if reinvokation
				      goal-tasks-with-roc-and-reason
				    goal-tasks-with-roc) new-agenda pds)
	  
	  (setf inac*strategy-ks-execution-interactive interactive)
	  
	  (setf (pds~agenda pds) new-agendaii))))))


#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| --------------------------------------------------------------- LOOP DETECTION ---------------------------------------------------- |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#


;; At all, Loop-Detection is so far very specialized!
;; In particular, the comparision function for matchings should be the same as for backtracking in pplan=matching-is-subsumed-p
;; with the addition that it should be possible to perform the comparison modulo some nodes that can be different.!

(defun pplan=loop-detection (pds task-node mmatchings)
  (declare (edited  "23-AUG-1999")
	   (authors Ameier)
	   (input   "A pds, the current task-node and a list of mmatchings.")
	   (effect  "None.")
	   (value   "If loop-detection is on, all the mmatchings are checked wether they"
		    "are a loop or not."
		    "Multiple-value:"
		    "First: The mmatchings not detected as loops."
		    "Second: The mmatchings detected as loops."))

  (let* ((current-cstr-pool (pds~constraint-pool pds)))
    
    (cond ((null task-node)
	   ;; no task -> no loops
	   (values mmatchings nil))
	  ((null pplan*loop-detection-depth)
	   ;; no loop-detection stopped
	   (values mmatchings nil))
	  ((null mmatchings)
	   ;; no mmatchings-> nothing to drop
	   (values mmatchings nil))
	  (t
	   ;; LOOP-Detection on,
	   ;; depth in pplan*loop-detection-depth
	   
	   (when pplan*verbose
	     (omega~message "**CHOOSE-MMATCHING**: Starting Loop Detection with mmatchings ~A" mmatchings))
	   
	   (do* ((current-depth 1 (+ current-depth 1))
		 (current-pred-steps (pplan=predecessors-steps-of-node task-node))
		 (remaining-mmatchings mmatchings)
		 (rejected-mmatchings nil))
	       ((or (null current-pred-steps)
		    (> current-depth pplan*loop-detection-depth))
		(values remaining-mmatchings
			rejected-mmatchings))
	     (let* ((current-pred-step-mmatchings (mapcar #'(lambda (pred-step)
							      (let* ((just (pdsc~an-just pred-step))
								     (mmapping (pdsj~subst just))
								     (parameters (pdsj~parameters just))
								     (method (just~method just))
								     (new-mmatching (pplan=mmatching-create method
													    parameters
													    mmapping
													    current-cstr-pool
													    pplan*roc-state-description 
													    )))
								new-mmatching))
							  current-pred-steps))
		    (mmatchings-to-reject (remove-if-not #'(lambda (mmatching)
							     (pplan=find-mmatching-modulo-task mmatching current-pred-step-mmatchings
											       task-node current-pred-steps))
							 remaining-mmatchings)))
	       
	       
	       (when pplan*verbose
		 (omega~message "**CHOOSE-MMATCHING**: Current regarded steps: ~A" current-pred-steps)
		 (omega~message "**CHOOSE-MMATCHING**: Found the following loop mmatchings: ~A" mmatchings-to-reject))
	       
	       (setf rejected-mmatchings (append rejected-mmatchings mmatchings-to-reject))
	       (setf remaining-mmatchings (remove-if #'(lambda (mmatching)
							 (find mmatching mmatchings-to-reject))
						     remaining-mmatchings))
	       (setf current-pred-steps (apply #'append (mapcar #'(lambda (pred-step)
								    (let* ((node (pdsc~an-node pred-step)))
								      (pplan=predecessors-steps-of-node node)))
								current-pred-steps)))))))))
  
;; The following is mayby not sufficient!
;; Indeed we would like to get here exactly the reason which created the node
(defun pplan=predecessors-steps-of-node (node)
  (let* ((just (node~justification node))
	 (other-reasons (remove (pdsj~own-reason just)
				(pdsj~reasons just))))
    other-reasons))

(defun pplan=find-mmatching-modulo-task (mmatching mmatching-set node steps-set)
  (do* ((rest-mmatchings mmatching-set (rest rest-mmatchings))
	(rest-steps steps-set (rest rest-steps))
	(flag nil))
      ((or flag
	   (null rest-mmatchings))
       flag)
    (let* ((head-mmatching (first rest-mmatchings))
	   (head-step (first rest-steps))
	   (head-node (pdsc~an-node head-step)))

      (when (pplan=matching-is-subsumed-modulo-nodes-p mmatching head-mmatching node head-node)
	(setf flag 't)))))


(defun pplan=matching-is-subsumed-modulo-nodes-p (partial-mmatching complete-mmatching node1 node2)
  (let* ((complete-method (pplan~matched-method complete-mmatching))
	 (complete-params (pplan~mmatch-parameters complete-mmatching))
	 (complete-mmapp (pplan~mmatch-mmapp complete-mmatching))
	 (partial-method (meth~inference (pplan~matched-method partial-mmatching)))
	 (partial-params (pplan~mmatch-parameters partial-mmatching))
	 (partial-mmapp (pplan~mmatch-mmapp partial-mmatching)))

    (and (eq complete-method partial-method)
	 (pplan=mapping-is-subsumed-modulo-nodes-p partial-mmapp complete-mmapp node1 node2)
	 (equal (length partial-params) (length complete-params))
	 (do* ((rest-params1 partial-params (rest rest-params1))
	       (rest-params2 complete-params (rest rest-params2)))
	     ((or (null rest-params1)
		  (null (keim~equal (first rest-params1)
				    (first rest-params2))))
	      (if (null rest-params1)
		  't
		nil))))))

(defun pplan=mapping-is-subsumed-modulo-nodes-p (partial-mmapping complete-mmapping node1 node2)
  (let* ((complete-subst (meth~mapp-subst complete-mmapping))
	 (complete-mapp (meth~mapp-mapp complete-mmapping))
	 (complete-extension (meth~mapp-extension complete-mmapping))
	 (complete-constraint (meth~mapp-constraint complete-mmapping))
	 (partial-subst (meth~mapp-subst partial-mmapping))
	 (partial-mapp (meth~mapp-mapp partial-mmapping))
	 (partial-extension (meth~mapp-extension partial-mmapping))
	 (partial-constraint (meth~mapp-constraint partial-mmapping)))

    (and ;; (keim~equal complete-constraint partial-constraint)
	 (pplan=subst-is-subsumed-modulo-nodes-p partial-subst complete-subst node1 node2)
	 (pplan=subst-is-subsumed-modulo-nodes-p partial-mapp complete-mapp node1 node2)
	 ;; (keim~equal extension1 extension2) ;; what exactly is Extension???
	 )))

(defun pplan=subst-is-subsumed-modulo-nodes-p (partial-subst complete-subst node1 node2)
  (declare (edited  "29-JUN-1999")
	   (authors Ameier)
	   (input   "Two substitutions.")
	   (effect  "None.")
	   (value   "T if all elements of the first substitition are also in the"
		    "second substitution."))

  (let* ((partial-domain (subst~domain partial-subst))
	 (partial-codomain (subst~codomain partial-subst))
	 (complete-domain (subst~domain complete-subst))
	 (complete-codomain (subst~codomain complete-subst))
	 (partial-assoc-list (mapcar #'(lambda (in1 in2)
					 (list in1 in2))
				     partial-domain
				     partial-codomain))
	 (complete-assoc-list (mapcar #'(lambda (in1 in2)
					  (list in1 in2))
				      complete-domain
				      complete-codomain)))
    (do* ((rest-partial-domain partial-domain (rest rest-partial-domain))
	  (flag 't))
	((or (null rest-partial-domain)
	     (null flag))
	 flag)
      (let* ((head-partial-domain-element (first rest-partial-domain))
	     (acc-partial-codomain-element (assoc head-partial-domain-element partial-assoc-list))
	     (acc-complete-codomain-element (assoc head-partial-domain-element complete-assoc-list)))

	(when (null (and acc-complete-codomain-element
			 (or (keim~equal (second acc-complete-codomain-element) (second acc-partial-codomain-element))
			     (and (eq (second acc-partial-codomain-element) node1)
				  (eq (second acc-complete-codomain-element) node2)))))
	  
	  (setf flag nil))))))



#| ----------------------------------------------------------------------------------------------------------------------------------- |#
#| ------------------------------------------------------------- VIL-MODE ------------------------------------------------------------ |#
#| ----------------------------------------------------------------------------------------------------------------------------------- |#

(defun pplan~plan-VIL ()
  (declare (edited  "09-AUG-2000")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Plans interactively.")
	   (value   "An execution message."))

  (setf pplan*check-backtrack nil) ;; interactive -> switch backtrack check off!
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 ;(agenda (pds~agenda pds))
	 (strategy-ks (roc~strategy-ks pplan*roc-state-description))
	 (termination-check (gethash 'termination-check (strat~strategy-ks-hash-table strategy-ks)))
	 (termination-check-results (apply termination-check nil)))

    (if pplan*update-agents
	(progn (foci~compute-pcs :pds pds)
	       (sugg~reset))
      (setf pplan*update-agents 't))
      
    (cond (termination-check-results 
	   
	   ;; If the termination check of the strategy-ks fires:
	   ;; -> create proper-termination execution message
	   
	   (let* ((demands (if (listp termination-check-results)
			       termination-check-results
			     nil)))
	     
	     (when pplan*verbose
	       (omega~message "**PPLANNER**: Termination by strategy-termination-predicat => Proper Termination with additional demands ~A" demands))
	     
	     (exmes~create-termination-message pplan*roc-state-description
					       demands)))
	  
	  (t
	   
	   (multiple-value-bind
	       (commando args)
	       (pplan=wait-until-command (append '(im.set-metavar im.set-goal im.multi im.stop im.backtrack
						   Start-Prex Leave-Prex Show-Presentation Interrupt-Prex Dialog show-step apply-instantiation get-construction-style-verbalization get-textbook-style-verbalization)
						 (mapcar #'keim~name pplan*methods)
						 (mapcar #'keim~name strat*current-strategy-ks)))
	     
	     (cond ((string-equal (keim~name commando) 'im.backtrack)

		    ;; User wishes to backtrack
		    (csm~suspend)
		    (pplan=interrupt-for-backtracking (first args)))
		   
		   ((string-equal (keim~name commando) 'im.multi)
		    
		    ;; User pressed the automatic Button -> continue with automatic Planning!
		    (csm~suspend)
		    (setf pplan*VIL-on nil)
		    (pplan=interactive-init 1)
		    (pplan~plan))
		   
		   ((string-equal (keim~name commando) 'im.set-goal)
		    
		    ;; User pressed one of the task-nodes -> set agent focus to this node (which is the first of the arguments)
		    (apply (com~function commando) args)
		    (setf pplan*update-agents nil)
		    (pplan~plan-VIL))
		   
		   ((string-equal (keim~name commando) 'im.stop)

		    ;; User pressed quit-button -> terminate Multi completely
		    (csm~quit)
		    (mapcar #'(lambda (x) (when (agent~resources x)
					    (when (rsrc~log-flag (agent~resources x))
					      (agent~change-log-flag x))))
			    (agent~get-agents))
		    (setf pplan*VIL-on nil)
		    (setf sod*VIL nil)
		    
		    ;;(return-from pplan~plan-VIL (exmes~create-termination-message pplan*roc-state-description
		    ;;								  nil)))

		    (throw 'break nil))
		   
		   ((string-equal (keim~name commando) 'im.set-metavar)
		    
		    ;; User whishes to instantiate a Meta-Variable (which is the first argument)
		    ;; -> interrupt with demand to instantiate interactively this meta-variable!

		    (csm~suspend)
		    (pplan~execute-strategy 'InstInteractively (first args) nil))
		   
		   (t
		    ;; User has chosen a method agent -> Do step!
		    (pplan=interactive-init 0)
		    (apply (com~function commando) args)  ;;MP: do whatever it is.
		    )))))))
		    

(defun pplan=interrupt-for-backtracking (arg)
  (cond ((pds~label2node arg)
	 ;; Backtracking to take back a step
	 (pplan~execute-strategy 'backtrack-step-to-task (pds~label2node arg) nil))

	((string-equal arg 'open)
	 ;; backtracking to re-open a node
	 (pplan~execute-strategy 'backtrack-to-open nil nil))

	(t
	 ;; backtracking to take back a mv instantiation
	 (let* ((current-binding (pds~cstrpool-bindings (pds~constraint-pool omega*current-proof-plan)))
		(bound-mvs (subst~domain current-binding))
		(mv (find arg bound-mvs :test #'(lambda (sym meta-var)
						  (string-equal sym (keim~name meta-var))))))
	   (if mv
	       (pplan~execute-strategy 'BackTrack-MV-Inst nil (list mv))
	     (progn
	       (omega~message "~%Could not find a meta-variable binding for meta-variable ~A" mv)
	       nil))))))

#| replaced by pplan~execute-strategy
(defun pplan=interrupt-for-mv-inst (mv)
  (declare (edited  "09-AUG-2000")
	   (authors Ameier)
	   (input   "A meta-variable.")
	   (effect  "None.")
	   (value   "A intrrupt execution message with the demand to call"
		    "the strategy InstInteractively on the task for the meta-variable."))
  (let* ((agenda (pds~agenda omega*current-proof-plan))
	 (all-tasks (agenda~all-tasks agenda))
	 (task-for-mv (first (remove-if-not #'(lambda (task)
						(and (agenda~inst-task-p task)
						     (eq (agenda~inst-task-meta-var task) mv)))
					    all-tasks))))
    (exmes~create-interruption-message pplan*roc-state-description
				       (list (demand~create-strategy-task-demand (strat~find-strategy-ks 'InstInteractively)
										 task-for-mv
										 nil))
				       nil)))
|#

;;todo: change-strategy additionally to execute-strategy

(defgeneric pplan~execute-strategy (strategy task params)
  (declare (edited  "15-DEC-2001")
	   (authors Pollet)
	   (input   "strategy, task, parameters.")
	   (effect  "None.")
	   (value   "An interrupt execution message with the demand to call"
		    "the strategy on the task with given parameters."
		    "The strategy will be executed automatically."))
  (:method ((strat strat+strategy-ks)(task agenda+task) params)
	   (pplan=interactive-store-strat strat)
	   (setf pplan*VIL-on nil)
	   (exmes~create-interruption-message pplan*roc-state-description
					      (list (demand~create-strategy-task-demand strat task params))
					      nil))
  (:method ((strat symbol) task params)
	   (pplan~execute-strategy (strat~find-strategy-ks strat) task params))
  (:method (strat (mv meta+variable) params)
	   (let* ((agenda (pds~agenda omega*current-proof-plan))
		  (all-tasks (agenda~all-tasks agenda))
		  (task-for-mv (first (remove-if-not #'(lambda (task)
							 (and (agenda~inst-task-p task)
							      (eq (agenda~inst-task-meta-var task) mv)))
						     all-tasks))))
	     (if task-for-mv (pplan~execute-strategy strat task-for-mv params)
	       (omega~warn "No task for meta-variable ~A to apply strategy ~A" mv strat))))
  (:method (strat (node node+node) params)
	   (let* ((agenda (pds~agenda omega*current-proof-plan))
		  (all-tasks (agenda~all-tasks agenda))
		  (task-for-node (first (remove-if-not #'(lambda (task)
							   (and (agenda~goal-or-goal-schema-task-p task)
								(eq (agenda~task-node task) node)))
						       all-tasks))))
	     (if task-for-node (pplan~execute-strategy strat task-for-node params)
	       (omega~warn "No task for node ~A to apply strategy ~A" node strat))))
  (:method (strat (nix null) params)
	   (declare (ignore nix))
	   (pplan~execute-strategy strat (first (agenda~all-tasks (pds~agenda omega*current-proof-plan))) params)))
	    
(defun pplan~execute-method (method goal supps params)
  (declare (edited  "15-DEC-2001" "09-AUG-2000")
	   (authors Pollet Ameier)
	   (input   "method, goal, supports, parameters.")
	   (effect  "Applies the method on the goal with the given supports and parameters."
		    "If this fails (why ever) it prints a message."
		    "If this is done within VIL-mode, calls plan-VIL.")
	   (value   "-"))
  
  (let* ((task-mmatching-pair (pplan~produce-mmatching method goal supps params))
	 (task (first task-mmatching-pair))
	 (mmatching (second task-mmatching-pair))
	 (pds omega*current-proof-plan)
	 (agenda (pds~agenda pds)))
    
    (if mmatching 

      (let* ((task-node (agenda~task-node task))
	     (node-formula (pds~task-formula task pds))
	     (node-supports (pds~node-supports task-node pds)))
	
	(when pplan*verbose
	  (omega~message "**PLAN-STEP**: Selected matching: ~A, Applying now this matching." mmatching))
	
	(multiple-value-bind
	    (new-agenda new-opens new-supps)
	    (pplan=apply-plan-step! task mmatching pds agenda)

	  (let ((success (if pplan*normalize-goals-and-supps
			     (pplan=restrict-and-normalize-during-planning
			      new-agenda
			      pds
			      (if (pdsn~open-node-p task-node)
				  (cons task-node new-opens)   ;; if task-node still open -> normalize it against the new supports too
				new-opens)                     ;; otherwise only the new opens
			      new-supps)
			   new-agenda))))))

      (omega~warn "~%~%NOT POSSIBLE TO APPLY METHOD ~A ON GOAL ~A WITH SUPPORTS ~A AND PARAM. ~A"
		  method goal supps params))
    (when pplan*vil-on  (pplan~plan-VIL))))  ;; command was choosen within IM/multi-vil -> continue


(defun pplan~produce-mmatching (method goal supports parameters)
  (declare (edited  "09-AUG-2000")
	   (authors Ameier)
	   (input   "A method-name, an open-node, a list of possible supports, and a list of parameters.")
	   (effect  "None.")
	   (value   "A list with two elements:"
		    "First: the task to the open node."
		    "Second: the method-matching if one is found (otherwise nil)."))
  
  (let* ((pds omega*current-proof-plan)
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (task (first (remove-if-not #'(lambda (task)
					 (and (agenda~goal-or-goal-schema-task-p task)
					      (eq (agenda~task-node task) goal)))
				     all-tasks))))

    (when task (list task
		     (pplan=choose-method pds agenda task (list (list method supports parameters)))))))
  
  
(defun pplan=wait-until-command (list-of-names)
  (loop
   ;(socket~write "indicateIdle" :inout)
   (opr~release-listener opr*loui-listener) 
   (opr~release-listener)
   (proc~wait "Waiting for a command" 
			#'opr~bb-command-queue opr*core-blackboard)	
   ;(socket~write (format nil "indicateBusy(\"ApplyingtheMethod\")") :inout)
   (let* ((queue    (opr~bb-command-queue opr*core-blackboard))
	  (command? (find-if #'(lambda (x)
					 (let ((com (car x)))
					   (and (com~command-p com)
						(some #'(lambda (name) (string-equal (keim~name com) name))
						      list-of-names))))
			     queue)))
     ;(omega~trace ";;;;current-queue ~A" queue)
     (setf (opr~bb-command-queue opr*core-blackboard) nil)
     (when command?
       (return (progn
		 ;(omega~trace ";;;;got the command ~A" command?)
		 (values (first command?) (second command?)))))
     (let ((request-command? (find-if #'(lambda (x)
					 (let ((com (car x))
					       (args (second x)))
					   (and (com~command-p com)
						(or
						 (string-equal (keim~name com) :exit)
						 (and (string-equal (keim~name com) :request-command)
							 (some #'(lambda (name) (string-equal
										 (keim~name (first args))
										 name))
							       list-of-names))
						    (and (string-equal (keim~name com) :check-arg-types)
							 (some #'(lambda (name) (string-equal
										 (first (first args))
										 name))
							       list-of-names))))))
					 queue)))
     (when request-command?
       ;(omega~trace ";;;;got the request ~A" request-command?)
       (opr~execute-command request-command?))))))

;A hack: to enable the automatic execution of a whole strategy,
;the strat is stored in the variable below will not be interrupted.
;This avoids two planners/ strategy declarations, but has the drawback
;that a strategy cannot be executed as a whole and interactively at
;the same time.  (MP/9.1.2002)
;Well it can, with change-strat instead of execute-strat (remove that
;strat from list ...)

(let (auto-strat (counter 0))

  (defun pplan=interactive-store-strat (strat)
    (unless (member strat auto-strat) (push strat auto-strat)))

  (defun pplan=interactive-set-autostrat (auto-strats)
    (let ((strats (mapcar #'(lambda (str) (if (strat~strategy-ks-p str) str (strat~find-strategy-ks str))) auto-strats))) 
      (setf auto-strat strats)))
  
  (defun  pplan=interactive-mode? (strat);;counter-mode
    (unless (member strat auto-strat)
      (setf counter (1- counter)) (minusp counter)))

  (defun pplan=interactive-init (numberofautosteps)
    (setq counter numberofautosteps))
  
					;   (defun  pplan=interactive-mode? (strat);;until user clicks on manual 
					;     (unless (member strat auto-strat)
					;       (opr~release-listener opr*loui-listener)
					;       (let* ((queue    (opr~bb-command-queue opr*core-blackboard))
					; 	     (command? (find-if #'(lambda (x)
					; 				    (let ((com (car x)))
					; 				      (and (com~command-p com)
					; 					   (string-equal (keim~name com) :im.agents))))
					; 				queue)))
					; 	(setf (opr~bb-command-queue opr*core-blackboard) nil)
					; 	(if  command? T nil))))
  )
	     
	     
