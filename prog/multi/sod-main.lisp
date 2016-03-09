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

(in-package :omega)

;;

(mod~defmod SOD 
            :uses (agenda black exmes inac job keim omega opr pds roc scon store strat)
            :documentation "THE MULTI SYSTEM"
            :exports (
		      sod~initialize-VIL!
		      sod~initialize!
		      sod~system-go
                      sod~introduce-new-strategy-ks-step!
                      
                      sod*control-blackboard
                      sod*current-legal-job-offers
                      sod*execution-message
                      sod*solution-blackboard
                      sod*startegy-application-bound
                      sod*verbose
		      sod*VIL))

;; This file contains the top level of SOD-SYS
;; The main cycle consists of:
;; 1.) Refining:      Some strategy-ks works on the solution blackboard (all strategies are listet in strat*current-strategy-ks,
;;                    the solution blackboard is found in sod*solution-blackboard)
;; 2.) Triggering:    The condition part of each strategy-ks is matched, all firing strategies write a job offer on the
;;                    control blackboard (the control blackbord is found in sod*control-blackboard)
;; 3.) Meta-Reaosner: The metareasoner reasons on the control blackboard using the strategic control rules (which can be found in
;;                    metar*current-strategic-control-rules) and changes the order of the job offers
;; 4.) Scheduling:    The scheduler takes the first job offer from the control blackboard and executes it.  

#| ------------------------------------------------------ global variables ---------------------------------------------------- |#

(defvar sod*solution-blackboard nil)
;; global variable containing the solution blackboard

(defvar sod*control-blackboard nil)
;; global variable containing the control blackboard

(defvar sod*verbose nil)
;; global variable; if set, the sod system outputs comments during work

(defvar sod*current-strategies nil)
;; global variable to specify a set of strategies

(defvar sod*current-strategic-control-rules nil)
;; global variable to specify a set of strategic control rules

(defvar sod*current-legal-job-offers nil)
;; global variable to store the set of current legal job-offers

(defvar sod*startegy-application-bound nil)
;; global variable to check how many strategies are applicable

(defvar sod*execution-message nil)
;; global variable to store the last execution message in!

(defvar sod*VIL nil)
;; global variable to store whether we are in the ida-interactive mode

(defvar sod*inst-tasks nil)

(defvar sod*backtrack-counter 0)

(defvar sod*initial-bound nil)
;; variable to store the initial bound in (to be apple to compare the remaining ressource with the initial one)

#| ------------------------------------------------------ main things --------------------------------------------------------- |#

(defun sod~initialize! (pds strategy-ks strat-crules interactive bound)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A PDS and a list of strategy names and a list of strategic control rule names."
		    "Furthermore a flag signing wether interactive should be used.")
	   (effect  "New solution and control blackboards are created and sod*solution-blackboard and"
		    "sod*control-blackboard are set on this new blackboards respectively. Furthermore"
		    "omega*current-proof-plan is set on the input PDS and strat*current-strategy-ks and"
		    "metar*current-strategic-control-rules are set on the input strategies (objects)"
		    "and control-rules (objects) respectively."
		    "inac*interactive is set on the value of interactive.")
	   (value   "The new solution blackboard."))
  
  (let* ((pds-name (keim~name pds))
	 (new-solution-blackboard (black~create-blackboard (make-symbol (format nil "~A-Solution-BlackBoard" pds-name))))
	 (new-control-blackboard (black~create-blackboard (make-symbol (format nil "~A-Control-BlackBoard" pds-name)))))

    ;; resetting the backtrack steps
    (setf sod*backtrack-counter 0)

    ;; resetting execution-message
    (setf sod*execution-message nil)
    
    ;; resetting of the inst-tasks
    (setf sod*inst-tasks nil)
    
    ;; setting interactive
    (setf inac*interactive interactive)
    (setf inac*strategy-ks-execution-interactive interactive) 
    
    ;; setting the Blackboards:
    (setf sod*solution-blackboard new-solution-blackboard)
    (ana~store-solution-bb new-solution-blackboard pds)
    (setf sod*control-blackboard new-control-blackboard)

    ;; setting the omega*current-proof-plan on the incomming PDS
    (setf omega*current-proof-plan pds)

    ;; setting the boundary for strategy applications:
    (setf sod*startegy-application-bound bound)
    (setf sod*initial-bound bound)
    
    ;; We place on the solution Blackboard:
    ;; 1.) the pds under Name PDS
    ;; 2.) A rocs-store (with an empty list of state-descriptions) under Name STORE
    ;; 3.) A rocs-list, die zunaechst mal nil ist
    ;; 4.) Strategy-steps list, die zunaechst mal nil ist
    (black~add-new-blackboard-object! 'pds pds 'pds new-solution-blackboard)
    (black~add-new-blackboard-object! 'store (store~create-rocs-store nil) 'dummyt new-solution-blackboard)
    (black~add-new-blackboard-object! 'rocs-list (store~create-rocs-list-store nil) 'dummyt new-solution-blackboard)
    (black~add-new-blackboard-object! 'ssteps-list (store~create-ssteps-list-store nil) 'dummyt new-solution-blackboard)
    ;; type dummyt is incorrect!
    
    ;; We place on the control blackboard:
    ;; 1.) A Job-offers store (at the moment empty) under name JOB-OFFERS
    ;; 2.) A demands store (at the moment empty) under name DEMANDS
    ;; 3.) An entry first-strategy-ks-step which is set to nil
    ;; 4.) An entry last-strategy-ks-step which is set to nil
    ;; The used bb-object types dummyt are not correct!
    (black~add-new-blackboard-object! 'job-offers (store~create-job-offers-store nil) 'dummyt new-control-blackboard)
    (black~add-new-blackboard-object! 'demands (store~create-demands-store nil) 'dummyt new-control-blackboard)
    (black~add-new-blackboard-object! 'first-strategy-ks-step nil 'dummyt new-control-blackboard)
    (black~add-new-blackboard-object! 'last-strategy-ks-step nil 'dummyt new-control-blackboard)
    
    ;; setting strat*current-strategy-ks to the strategies
    (setf strat*current-strategy-ks (mapcar #'strat~find-strategy-ks strategy-ks))
    
    (let* ((metar-crules (remove-if-not #'(lambda (crule)
					    (string-equal (cri~kind (cri~find-control-rule crule)) 'strategic))
					strat-crules))
	   (suggr-crules (remove-if-not #'(lambda (crule)
					    (string-equal (cri~kind (cri~find-control-rule crule)) 'suggestion))
					strat-crules)))
      
      ;; setting metar*current-strategic-control-rules
      (setf metar*current-strategic-control-rules metar-crules)
      ;; setting suggr*current-suggestion-control-rules
      (setf suggr*current-suggestion-control-rules suggr-crules)
      
      new-solution-blackboard)))

(defun sod~initialize-VIL! (pds strategy-ks strat-crules)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A PDS, a list of strategy names, and a list of strategic control rule names.")
	   (effect  "New solution and control blackboards are created and sod*solution-blackboard and"
		    "sod*control-blackboard are set on this new blackboards respectively. Furthermore"
		    "omega*current-proof-plan is set on the input PDS and strat*current-strategy-ks and"
		    "metar*current-strategic-control-rules are set on the input strategies (objects)"
		    "and control-rules (objects) respectively."
		    "Furthermore, sod*vil is set.")
	   (value   "The new solution blackboard."))

  (setf sod*VIL 't)
  (setf pplan*VIL-on 't)
  ;;(omega~trace "~%Hello ~A ~A" pplan*VIL-on sod*vil)
  (sod~initialize! pds strategy-ks strat-crules nil 1000)
  ;; currently we use 1000 as bound in the interactive mode ...
  )
  
(defun sod~introduce-new-strategy-ks-step! (new-step)
  (declare (edited  "22-JUN-1999")
	   (authors Ameier)
	   (input   "A scon strategy-step.")
	   (effect  "Introduces the step as new last strategy step.")
	   (value   "Undefined."))
  (let* ((current-last-step (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)))
    (if current-last-step
	;; there is already a last-step
	(progn
	  (scon~add-step-behind-step! new-step current-last-step)
	  (black~change-blackboard-object-content! 'last-strategy-ks-step new-step sod*control-blackboard))
      ;; there is no last step at all and so no first step
      (progn
	(scon~initialize-with-step! new-step)
	(black~change-blackboard-object-content! 'first-strategy-ks-step new-step sod*control-blackboard)
	(black~change-blackboard-object-content! 'last-strategy-ks-step new-step sod*control-blackboard)))))
	
(defun sod~system-go (&key (start-strategy nil))
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Starts to work (and continues to work) on the blackboard system.")
	   (value   "Undefined."))
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
    
    (when sod*verbose
      (omega~message "********** SOD:: SOD START ***********************************")
      (omega~message "**SOD:: SOD START**: Starting to work on PDS ~A." pds)
      (omega~message "*************** SOD:: SOD START ******************************"))
    
    (catch 'leave-multi
      (do* ((current-agenda (pds~agenda pds) (pds~agenda pds))
	    (current-tasks (if (agenda~empty-p current-agenda) nil (agenda~all-tasks current-agenda))
			   (if (agenda~empty-p current-agenda) nil (agenda~all-tasks current-agenda)))
	    (stop nil))
	  
	  (stop
	   (omega~message "~%*************** SOD:: SOD STOP ******************************")
	   (cond ((> sod*startegy-application-bound 0)
		  (omega~message "**SOD:: SOD STOP**: MULTI stops since there are no further applicable jobs **")
		  (omega~message "**SOD:: SOD STOP**: REST TASKS: ~A **" (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
		  (omega~message "**SOD:: SOD STOP**: REST RESOURCE ~A **" sod*startegy-application-bound))
		 (t
		  (omega~message "**SOD:: SOD STOP**: NO RESOURCES LEFT! **")
		  (omega~message "**SOD:: SOD STOP**: REST TASKS: ~A **" (agenda~all-tasks (pds~agenda omega*current-proof-plan)))))
	   (omega~message "*************** SOD:: SOD STOP ******************************")	
	   nil)
	
	(progn 
	  
	  ;; 0. Set the  job-offers on the control blackboard to nil!
	  (store~clean-store! (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	  
	  ;; 0.a Eliminate strategy applications who are meanwhile terminated from the active applications slot
	  (sod=check-active-rocs!)
	  
	  ;; 1. Let the strategy-ks trigger whether they are applicable!
	  ;;    Applicable ones will place a job-offer on the control-blackboard
	  (sod=trigger-job-offers! pds current-agenda)
	  
	  (when sod*verbose
	    (omega~message "~%**SOD:: TRIGGERING JOB OFFERS**:")
	    (omega~message "After triggering the strategy-KS we have the following job-offers on the control-blackboard:")
	    (store~print-job-offers (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	  
	  ;; set the legal applicable job-offers!
	  (setf sod*current-legal-job-offers
		(store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	  
	  ;; 2. The Meta-Reasoner reasons on the job-offers and the demands
	  ;;    -> If start-strategy is given (and bound is initial) then remove all job offers except those
	  ;;       which are using the start-strategy
	  (if (and start-strategy
		   (= sod*startegy-application-bound sod*initial-bound))
	      (store~remove-except-start-strategy! (black~get-blackboard-object-content 'job-offers sod*control-blackboard)
						   start-strategy)
	    (store~call-metareasoner-on-job-offers! (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	  
	  (when sod*verbose
	    (omega~message "~%**SOD:: CALLING METAREASONER**:")
	    (omega~message "After calling the meta-reasoner we have the following job-offers on the control-blackboard:")
	    (store~print-job-offers (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	  
	  ;; 3. Choose job-offer for execution
	  (let* ((first-job-offer (if sod*current-legal-job-offers
				      (sod=choose-job-offer)
				    nil)))
	    
	    ;; 4. Execute this job-offer
	    (if (and first-job-offer
		     (> sod*startegy-application-bound 0))
		(progn
		  
		  (setf sod*startegy-application-bound (- sod*startegy-application-bound 1))
		  
		  (when sod*verbose
		    (omega~message "~%**SOD:: CHOOSING JOB OFFER**:")
		    (omega~message (job~job-offer-print-string first-job-offer))
		    (omega~message "~%**SOD:: STARTING CHOOSEN JOB OFFER**"))
		  
		  (when inac*interactive
		    (omega~message "~%**INTERACTIVE**")
		    (let* ((interactive-read-things (omega~query (list "Should INTERACTIVE be active in next job?"
								       'boolean
								       nil))))
		      (setf inac*strategy-ks-execution-interactive (first interactive-read-things))))
		  
		  (let* ((execution-message (job~execute-offer! first-job-offer)))
		    (opr~signal-interrupt execution-message)
		    (when sod*verbose
		      (omega~message "~%*********************")
		      (omega~message "**SOD:: BACK ON SOD**")
		      (omega~message "*********************")
		      (omega~message "~%**SOD:: RECEIVING EXECUTION MESSAGE**: ~A" execution-message))
		    (setf sod*execution-message execution-message)
		    (sod=interpret-execution-message! execution-message)
		    ))

	      (progn
		(cond ((null first-job-offer)
		       (when sod*verbose
			 (omega~message "**SOD:: SOD STOP**: No further job-offer -> stop")))
		      (t
		       (when sod*verbose
			 (omega~message "**SOD:: SOD STOP**: BOUND ZERO, RESOURCES EMPTY"))))
		(setf stop 't)))))))))

(defun sod=choose-job-offer ()
  (declare (edited  "06-AUG-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "A job offer. If inac*interactive is off this will be the first job-offer"
		    "on the control blackboard othrwise the user can choose interactively one"
		    "of the legal job-offers (stored in sod*current-legal-job-offers)."))
  
  (if (null inac*interactive)
      ;; If interactive mode is out -> choose the first offer from the control blackboard
      (first (store~elements (black~get-blackboard-object-content 'job-offers
								  sod*control-blackboard)))
    ;; If interactive mode is on -> Ask user what to choose
    (let* ((all-jobs (remove-duplicates (append
					 (store~elements (black~get-blackboard-object-content 'job-offers
											      sod*control-blackboard))
					 sod*current-legal-job-offers)))
	   (all-possible-jobs (remove-if-not #'(lambda (job-offer)
						 (or (job~job-offers-parameters-are-consistent-p job-offer)
						     (equal (job~parameters job-offer) nil)))
					     all-jobs))
	   (suggestion-of-metareasoner (first (append
					       (store~elements (black~get-blackboard-object-content 'job-offers
												    sod*control-blackboard))
					       all-possible-jobs)))
	   (default-number (position suggestion-of-metareasoner all-possible-jobs))
	   (text (concatenate 'string
			      (format nil "~%~%~%Choose the next Job-Offer:")
			      (format nil "~%Current TASKS: ~A" (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
			      (format nil "~%Auto-Mode suggests number ~A" (if (numberp default-number) (+ default-number 1) nil))
			      (format nil "~%----------------------------")
			      (format nil "~%The possibilities are:")))
	   (text-jobs (append (mapcar #'(lambda (legal-job-offer)
					  (format nil "~A" (job~job-offer-print-string legal-job-offer)))
				      all-possible-jobs)
			      (list "Leave MULTI")))
	   (number-of-job (omega~query-choice text text-jobs default-number))
	   (job (nth number-of-job all-possible-jobs)))
      
      (when (and job
		 (null (job~parameters job))
		 (job~strategy-ks-offer-p job)
		 (strat~strategy-ks-parameter-types (job~strategy-ks job)))
	(sod=interactively-set-parameters-of-job! job))
      
      (cond (job
	     ;; a job was choosen
	     job)
	    ((= number-of-job (length all-possible-jobs))
	     ;; Leave MULTI was choosen
	     (omega~message "~%*************** SOD:: SOD STOP ******************************")
	     (omega~message "**SOD:: SOD STOP**: LEAVING MULTI **")
	     (omega~message "**SOD:: SOD STOP**: REST TASKS: ~A **" (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	     (omega~message "**SOD:: SOD STOP**: REST RESOURCE ~A **" sod*startegy-application-bound)
	     (omega~message "*************** SOD:: SOD STOP ********************************")
	     (omega~message "~%RESTART MULTI WITH THE COMMAND RESTART-MULTI")
	     (throw 'leave-multi nil))
	    (t
	     ;; a not specified option was choosen
	     ;; -> we take the suggestion from the metareasoner
	     suggestion-of-metareasoner)))))

(defun sod=interactively-set-parameters-of-job! (job)
  (declare (edited  "07-SEP-1999")
	   (authors Ameier)
	   (input   "A strategy-ks job offer with parameters nil but with parameter types in the strategy-ks.")
	   (effect  "Allows to choose interactively the parameters for the job and sets this parameters.")
	   (value   "The job offer."))
  (let* ((strategy-ks (job~strategy-ks job))
	 (parameter-types (strat~strategy-ks-parameter-types strategy-ks))
	 (choosen-parameters
	  (do* ((rest-parameter-types parameter-types (rest rest-parameter-types))
		(back nil))
	      ((null rest-parameter-types)
	       back)
	    (let* ((type (first rest-parameter-types))
		   (interactive-read-things ;(inac~interactive-questions (list 'parameter)
					    ;				(list (format nil "Specify parameter of type ~A" type))
					    ;				(list type)
					    ;				(list nil)))
		    (omega~query (list (format nil "Specify parameter of type ~A" type)
					type
					nil)))
		   (choosen-param (first interactive-read-things)))

	      (setq back (append back (list choosen-param)))))))

    (setf (job~parameters job)
	  choosen-parameters)
    job))

(defgeneric sod=interpret-execution-message! (exmes)
  (declare (edited  "27-AUG-1999")
	   (authors Ameier)
	   (input   "An execution message.")
	   (effect  "Depends on the message, and on the refinement operation which produced it.")
	   (value   "Undefined."))
  (:method ((exmes exmes+start-message))

	   ;; The following is the default treatment in case of a start message:
	   ;; NOTHING

	   nil
	   )
  (:method ((exmes exmes+interruption-message))
	   
	   ;; The following is the default treatment in case of an interruption message:
	   ;; The execution of strategy-ks was interrupted ->
	   ;; 1. demands are stored in demands store
	   ;; 2. demands are stored in state-description (-> i.e. the state-description depends from this demand)
	   ;; 3. the state description is stored (-> i.e. it can be reinvoked)
	   ;; 4. a new strategy interruption step is created
	   ;; 5. demands are also stored in this strategy interruption step
	   ;; Note: ROCS entries remain in TASKS!!!!!!! 

	   (let* ((state-des (exmes~state-description exmes)))
	   
	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF EXMES**: MESSAGE OF INTERRUPTED EXECUTION from strategy-ks ~A on start-task ~A"
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF EXMES**: Demands ~A and state-description are stored."
			      (exmes~interruption-message-demands exmes)))

	     (let* ((demands (exmes~interruption-message-demands exmes)))
	       
	       ;; 1. add demands in demands-store
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands)
	       
	       ;; 2. store demands in state-description
	       (setf (roc~demands state-des) demands)
	       
	       ;; 3. store state-description in store
	       (store~add-element! (black~get-blackboard-object-content 'store sod*solution-blackboard) state-des)
	       
	       ;; 4. create new strategy interruption step
	       (let* (;; new interrupt strategy-ks justification
		      (new-interrupt-strategy-ks-application-just
		       (strat~interrupt-strategy-ks-application-create-just (list state-des (roc~copy-state-description state-des) exmes)))
		      ;; new strategy step from this justification
		      (interrupt-step (scon~create-scon-step nil nil new-interrupt-strategy-ks-application-just)))
		 
		 ;; insert strategy step in strategy-steps
		 (sod~introduce-new-strategy-ks-step! interrupt-step))
	       
	       ;; 5. the demands are annotated to the last step (which should have an interruption just)
	       (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			 'posed-demands
			 demands))))
  (:method ((exmes exmes+termination-message))

	   ;; The follwing is the default treatment in case of a termination message
	   ;; Execution of a strategy-ks terminates proper ->
	   ;; 1. ROC has to be removed from TASKS (some other TASKS, exp ...)
	   ;; 2. A new strategy-step is created and inserted in strategy-steps
	   ;; 3. activ flag in state-des is set to nil
	   ;; 4. All fulfilled demands are removed from the demand store
	   ;; 5. Evtl. new demands are added

	   (let* ((state-des (exmes~state-description exmes))
		  (demands (exmes~termination-message-demands exmes)))

	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF EXMES**: MESSAGE OF TERMINATED EXECUTION from strategy-ks ~A on start-task ~A"
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF EXMES**: NEW Demands ~A are stored, the state description is deleted from all tasks and gets inactive."
			      demands))
	     
	     ;; 1. Remove the ROCS!
	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		    (all-tasks (agenda~all-tasks agenda)))
	       (mapcar #'(lambda (task)
			   (let* ((rocs (agenda~task-rocs task)))
			     (setf (agenda~task-rocs task) (remove state-des rocs))))
		       all-tasks))
	     
	     ;; 2. A new strategy-step is created and inserted in strategy-steps
	     (let* (;; An end-strategy-application-justification is created!
		    (new-justification (strat~end-strategy-ks-application-create-just (list state-des exmes)))
		    ;; A new scon step is created:
		    (strategy-end-step (scon~create-scon-step nil nil new-justification)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! strategy-end-step))
	     
	     ;; 3. activ flag in state-des is set to nil
	     (setf (roc~activ state-des) nil)
	     
	     
	     ;; 4. The fulfilled demands are removed!
	     (multiple-value-bind
		 (store2 rem del)
		 (store~demands-store-remove-fulfilled-demands! (black~get-blackboard-object-content 'demands sod*control-blackboard)
								exmes)
	       
	       ;; The removed demands are annotated to the last step (which should have a termination just)
	       (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			 'fulfilled-demands
			 del)
	       
	       (when sod*verbose
		 (omega~message "**SOD:: INTERPRETATION OF EXMES**: The following demands are fulfilled and thus removed: ~A" del)))

	     ;; 5. In case of new demands -> add to demands-store
	     (when demands
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands))))
  
  (:method ((exmes exmes+failure-message))

	   nil))


(defun sod=check-active-rocs! ()
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Searches for PPLANNER rocs in the roc-list which are activ but whose outline-lines are nil."
		    "Such rocs are terminated by and demands depending on them are removed.")
	   (value   "Undefined."))
  (let* ((rocs (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
	 (pplanner-rocs (remove-if-not #'roc~pplanner-state-description-p rocs))
	 (activ-but-empty-pplanner-rocs (remove-if-not #'(lambda (pplanner-roc)
							   (and (null (roc~pplanner-outline-lines pplanner-roc))
								(roc~activ pplanner-roc)))
						       pplanner-rocs)))
    (mapcar #'sod=terminate-roc! activ-but-empty-pplanner-rocs)))
  

(defun sod=terminate-roc! (state-des)
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "A stat-des (roc).")
	   (effect  "Terminates the roc by setting it to inactiv. Furthermore, all demands to this roc are"
		    "removed.")
	   (value   "Undefined."))
   (when sod*verbose
     (omega~message "~%**SOD:: Checking-ROCS**: Found that the following roc has no further tasks: ~A" state-des)
     (omega~message "~%**SOD:: Checking-ROCS**: TERMINATING THIS ROC:")
     (omega~message "~%**SOD:: Checking-ROCS**: SETTING STRATEGY-END-STEP, ACTIV-FLAG IS SET NIL, FULLFILLES DEMANDS ARE REMOVED!"))
   
   ;; 1. A new strategy-step is created and inserted in strategy-steps
   (let* (;; An end-strategy-application-justification is created!
	  (new-justification (strat~end-strategy-ks-application-create-just (list state-des)))
	  ;; A new scon step is created:
	  (strategy-end-step (scon~create-scon-step nil nil new-justification)))
     
     ;; Strategy-step is inserted in strategy-steps
     (sod~introduce-new-strategy-ks-step! strategy-end-step))

   ;; 2. activ flag in state-des is set to nil
   (setf (roc~activ state-des) nil)

   ;; 3. The fulfilled demands are removed!
   (multiple-value-bind
       (store2 rem del)
       (store~demands-store-remove-fulfilled-demands! (black~get-blackboard-object-content 'demands sod*control-blackboard)
						      state-des)
     
     ;; The removed demands are annotated to the last step (which should have a termination just)
     (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
	       'fulfilled-demands
	       del)
     
     (when sod*verbose
       (omega~message "**SOD:: Checking-ROCS**: The following demands are fulfilled and thus removed: ~A" del))))
   
(defun sod=trigger-job-offers! (pds current-agenda)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "The current pds and The current agenda of the pds.")
	   (effect  "All strategy-ks in strat*current-strategy-ks check their condition parts"
		    "whether they can work on a task in the agenda first-tasks."
		    "Each strategy-ks whose condition part fires places a job-offer on the control-blackboard."
		    "Furthermore the store also checks which of his state-descriptions could fire.")
	   (value   "Undefined."))
  
  (let* ((first-tasks (if (agenda~empty-p current-agenda)
			  nil
			(pds~first-tasks! pds current-agenda))))
    
    (when sod*verbose
      (omega~message "~%**SOD:: TRIGGERING JOB OFFERS**:")
      (omega~message "We have the following first-tasks: ~A" first-tasks)
      (omega~message "Whereas we have at all the following tasks: ~A" (agenda~all-tasks current-agenda)))

    (if (agenda~empty-p current-agenda)
	nil
      (progn
	
	(mapcar #'(lambda (strategy-ks)
		    (mapcar #'(lambda (task)
				(strat~trigger-condition-against-task! strategy-ks task))
			    first-tasks))
		strat*current-strategy-ks)
	
	(store~trigger-roc-state-descriptions! (black~get-blackboard-object-content 'store sod*solution-blackboard))))))
