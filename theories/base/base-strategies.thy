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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;    STRATEGIES FOR INSTANTIATING METAVARIABLES = STRATEGIES OF INSTMETA   ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InstFromParam Strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Instantiates a meta-variable by a parameter specified either by strategic control
;; or in a demand 

(defun answer-constraint-meta-p (task)
  (declare (edited  "21-JUN-1999")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "T if the task is an instantiation task for the meta-variable of the"
		    "answer constraint, nil otherwise."))
  (if (agenda~inst-task-p task)
      (let* ((meta-var (agenda~inst-task-meta-var task))
	     (plan-step (agenda~inst-task-plan-step task))
	     (plan-step-just (pdsc~an-just plan-step))
	     (plan-step-just-method (just~method plan-step-just))
	     (plan-step-just-method-name (keim~name plan-step-just-method)))
	
	(if (stringp plan-step-just-method-name)
	    (if (or (string= plan-step-just-method-name
			     "Initialize-CS-m")
		    (string= plan-step-just-method-name
			     "MYInitialize-CS-m"))
		't
	      nil)
	  (if (or (equal plan-step-just-method-name
			 'Initialize-CS-m)
		  (equal plan-step-just-method-name
			 'MYInitialize-CS-m))
	      't
	    nil)))
    nil))

(defun not-answer-constraint-meta-p (task)
  (and (agenda~inst-task-p task)
       (null (answer-constraint-meta-p task))))

(defun compute-instantiation-from-parameters (meta-var)
  (let* ((term (first (roc~parameters instmeta*roc-state-description))))
    term))

(defun instantiation-task-p (task)
  (if (not-answer-constraint-meta-p task)
      t
    nil))

(strat~define-strategy-ks
 (name InstFromParam)
 (refinement-algorithm InstMeta)
 (condition instantiation-task-p)
 (compute-instantiation-function compute-instantiation-from-parameters)
 (parameter-types (term))
 (print "Strategy-KS InstFromParam: Offer to instantiate meta-variable ~A from parameters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InstInteractively
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; THIS STRATEGY ASKS THE USER FOR AN INSTANTIATION FOR THE META-VARIABLE

(strat~define-strategy-ks
 (name InstInteractively)
 (refinement-algorithm InstMeta)
 (condition instantiation-task-p)
 (compute-instantiation-function ask-user)
 (parameter-types )
 (print "Strategy-KS InstInteractively: Offer to call user to instantiate mv ~A"))

(defun ask-user (meta-var)
  (omega~query (format nil "INSTANTIATION FOR ~A" (keim~name meta-var))
	       (arg~find-argtype 'term)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;   STRATEGIES FOR BACKTRACKING STUFF = STRATEGIES OF BACKTRACK   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backtrack-step Strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This strategy backtracks the step that introduced an open goal

(strat~define-strategy-ks
 (name backtrack-step-to-task)
 (refinement-algorithm backtrack)
 (condition backtrack-task-p)
 (compute-backtrack-steps-function steps-to-task)
 (print "Strategy-KS BackTrack-Step-to-Task: Offer to backtrack the step which creates task ~A"))

(defun backtrack-task-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (null (eq (prob~proof-root omega*current-proof-plan) (agenda~task-node task))))
      't
    nil))

(defun steps-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      ;; Nimm Reasons zu Task zurueck
      (let* ((node (agenda~task-node task))
	     (just (node~justification node))
	     (reasons (pdsj~reasons just)))
	(values
	 (reverse reasons)
	 nil))
    (values
     nil
     nil)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backtrack-chronological Strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This strategy backtracks the last step 

(strat~define-strategy-ks
 (name backtrack-chronological)
 (refinement-algorithm backtrack)
 (condition last-backtrack-step-p)
 (compute-backtrack-steps-function last-step)
 (print "Strategy-KS BackTrack-Step-to-Task: Offer to backtrack the last step"))

(defun last-backtrack-step-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (pds~last-plan-step omega*current-proof-plan))
      't
    nil))


(defun last-step (task)
  (let* ((last-plan-step (pds~last-plan-step omega*current-proof-plan)))
    (if last-plan-step
	(values (list last-plan-step)
		nil)
      (values nil
	      nil))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backtrack-last-strategy-to-task STrategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This strategy backtracks the whole strategy application which tackled as last one an open goal

(strat~define-strategy-ks
 (name backtrack-last-strategy-to-task)
 (refinement-algorithm backtrack)
 (condition strategy-backtrack-task-p)
 (compute-backtrack-steps-function last-strategy-to-task)
 (print "Strategy-KS BackTrack-Last-Strategy-to-Task: Offer to backtrack the last strategy applicated on task ~A"))

(defun strategy-backtrack-task-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (agenda~task-rocs task))
      ;; Task hat ROCS Eintrage
      't
    nil))

(defun last-strategy-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((task-rocs (rest (agenda~task-rocs task))) ;; The first one is the new backtrack ROC 
	     (all-rocs (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
	     (last-roc (find task-rocs (reverse all-rocs) :test #'(lambda (rocs-list roc)
								    (find roc rocs-list)))))
	
	(values (list (roc~start-step last-roc))
		nil))
    (values nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backtrack-creating-strategy-of-task
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This strategy backtracks the whole strategy application in which an open goal was created

(strat~define-strategy-ks
 (name backtrack-creating-strategy-of-task)
 (refinement-algorithm backtrack)
 (condition strategy-backtrack-task-p)
 (compute-backtrack-steps-function creating-strategy-to-task)
 (print "Strategy-KS BackTrack-Creating-strategy-of-task: Offer to backtrack the last strategy applicated on task ~A"))

(defun strategy-backtrack-task-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (agenda~task-rocs task))
      ;; Task hat ROCS Eintrage
      't
    nil))

(defun creating-strategy-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (other-reason (first (pdsj~other-reasons (node~justification node))))
	     (roc-with-step (first (remove-if-not #'(lambda (roc)
						      (find other-reason (roc~pplanner-steps roc)))
						  (remove-if-not #'roc~pplanner-state-description-p
								 (store~elements
								  (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))))
	(if (null roc-with-step)
	    (progn
	      (omega~warn "Could not find creating strategy in function creating-strategy-to-task")
	      (values nil nil))
	  (values (list (roc~start-step roc-with-step))
		  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra Backtrack-CPOOL Strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This strategy backtracks for a goal a step (planning step/instmeta step/ ...) that changed after the creation of the
;; goal the constraint store
;;
;;
;; A view further remarks:
;; 1.) To remove steps that changed the cosntraint store instead of backtracking a goal itself is sensible since then
;;     mayby meta-variables become unbound and can be bound in a different way such that then the goal can be closed!
;; 2.) In a first version this strategy backtracked ALL steps that changed after the creation of the goal the constraint store!
;;     This approach ended in a incomplete search
;;     HENCE: only the LAST step that changed the constraint store after the creation of the goal is backtracked!
;; 3.) In another early version we tried to backtrack only such steps that changed the constraint pool with respect to
;;     meta-variables that were also contained in the currently considered open goal. However, it turned out that this
;;     approach leaded to incompleteness (see also in file PPLANNER, diskussion at COMPARISON OF MATCHINGS).
;;     HENCE: only the LAST step that changed the constraint store after the creation of the goal is backtracked, what ever
;;            change it performes

(strat~define-strategy-ks
 (name backtrack-cpool-step-after-task)
 (refinement-algorithm backtrack)
 (condition backtrack-task-and-later-cpool-changes-p)
 (compute-backtrack-steps-function cpool-steps-to-task)
 (print "Strategy-KS backtrack-cpool-step-after-task: Offer to backtrack a step which changed the CPool for task ~A"))

;; Application Condition of backtrack-cpool-step-after-task Strategy
(defun backtrack-task-and-later-cpool-changes-p (task)
  (if (and (backtrack-task-p task)
	   (cpool-steps-to-task task))
      't
    nil))

;; Computing the steps to remove ...
;;
;; OLD VERSION: TAKES BACK EVERY STEP THAT CHANES THE CONSTRAINT POOL
;;(defun cpool-steps-to-task (task)
;;  (if (agenda~goal-or-goal-schema-task-p task)
;;     (let* ((steps-to-remove (later-constraint-pool-change-p task)))
;;	(if steps-to-remove
;;	    (values
;;	     (reverse steps-to-remove)
;;	     nil)
;;	  (values
;;	   nil
;;	   nil)))
;;   (values nil
;;	    nil)))
;;

;; NEW VERSION: TAKES BACK ONLY THE LAST STEP! (See above why we changed to this new version)

(defun cpool-steps-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((steps-to-remove (later-constraint-pool-change-p task)))
	(if steps-to-remove
	    (values
	     (list (first steps-to-remove))
	     nil)
	  (values
	   nil
	   nil)))
    (values nil
	    nil)))


(defun later-constraint-pool-change-p (task)
  (let* ((node (agenda~task-node task))
	 (step (first (pdsj~other-reasons (node~justification node))))
	 (after-steps (steps-after-step step))
	 (steps-changing-cstr-pool (do* ((current-constraint-pool (pds~constraint-pool omega*current-proof-plan))
					 (current-remove-steps nil))
				       ((null current-constraint-pool)
					current-remove-steps)
				     (let* ((current-cstr-pool-steps (pds~cstrpool-plansteps current-constraint-pool)))
				       (if (every #'scon~strategy-step-p current-cstr-pool-steps)
					   ;; we have here a constraint-pool changing step that was triggered by a meta-var-inst
					   (let* ((scon-just (scon~strategy-step-just (first current-cstr-pool-steps)))
						  (parameters (pdsj~parameters scon-just))
						  (roc (first parameters))
						  (last-plan-step (roc~instmeta-last-plan-step roc)))
					     (if (find last-plan-step (cons step after-steps))
						 ;; the inst-meta-step was done after the step we are interested in!
						 ;; -> add these instantiation step to the current-remove-steps
						 (progn
						   (setf current-remove-steps (append current-remove-steps current-cstr-pool-steps))
						   (setf current-constraint-pool (pds~cstrpool-previous current-constraint-pool)))
					       (setf current-constraint-pool nil)))
					 (let* ((inter (intersection current-cstr-pool-steps after-steps)))
					   (if inter
					       (progn
						 (setf current-remove-steps (append current-remove-steps inter))
						 (setf current-constraint-pool (pds~cstrpool-previous current-constraint-pool)))
					     (setf current-constraint-pool nil))))))))
    (if steps-changing-cstr-pool
	steps-changing-cstr-pool
      nil)))

(defun mvs-added-by-cpool (cpool)
  (declare (edited  "30-MAY-2000")
	   (authors Ameier)
	   (input   "A constraint-pool.")
	   (effect  "None.")
	   (value   "A list of all mvs added to the bindings in this constraint pool."))
  (if (null cpool)
      nil
    (let* ((previous-cpool (pds~cstrpool-previous cpool))
	   (bindings (pds~cstrpool-bindings cpool)))
      (if (null bindings)
	  nil
	(let* ((dom (subst~domain bindings)))
	  (if (null previous-cpool)
	      dom
	    (let* ((previous-bindings (pds~cstrpool-bindings previous-cpool)))
	      (if previous-bindings
		  (set-difference dom (subst~domain previous-bindings))
		dom))))))))
  

(defun steps-after-step (step)
  (let* ((last-step (pds~last-plan-step omega*current-proof-plan)))
    (do* ((curr-step step)
	  (after-steps nil))
	((eq curr-step last-step)
	 after-steps)
      (let* ((succ-step (pdsj~successor (pdsc~an-just curr-step))))
	(setf curr-step succ-step)
	(setf after-steps (cons succ-step after-steps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BackTrack-To-Open
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; THIS STRATEGY ASKS THE USER TO SPECIFY A NODE THAT SHOULD BE OPENED

(strat~define-strategy-ks
 (name BackTrack-To-Open)
 (refinement-algorithm BackTrack)
 (condition backtrack-task-and-VIL-p)
 (compute-backtrack-steps-function open-query)
 (print "Strategy-KS BackTrack-To-Open: Offer to call user to decide which node to open on ~A."))

(defun backtrack-task-and-VIL-p (task)
  (and sod*VIL  ;; Interactive mode is On
       (backtrack-task-p task)))
  
(defun always-true (&optional task)
  't)

(defun open-query (task)
  (let* ((node (omega~query "Node to open" (arg~find-argtype 'ndline)))
	 (own-reason (pdsj~own-reason (node~justification node))))
    (if own-reason
	(values
	 (list own-reason)
	 nil)
      (progn 
	(omega~message "Node ~A cannot be opened!" node)
	(values nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BackTrack-MV-INST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; THIS STRATEGY BACKTRACKS A MV INSTANTIATIONS

(strat~define-strategy-ks
 (name BackTrack-MV-Inst)
 (refinement-algorithm BackTrack)
 (condition mv-insts-exists-p)
 (compute-backtrack-steps-function compute-mv-back)
 (parameter-types (term))
 (print "Strategy-KS BackTrack-MV-Inst: Backtracks a meta-variable instantiation on ~A"))

(defun mv-insts-exists-p (task)
  (and sod*VIL
       (let* ((cpool (pds~constraint-pool omega*current-proof-plan))
	      (bindings (if cpool
			    (pds~cstrpool-bindings cpool)
			  (subst~create () ()))))
	 (if (subst~empty-p bindings)
	     nil
	   t))))
	 

	 
(defun compute-mv-back (task)
  (let* ((mv (first (roc~parameters back*roc-state-description)))
	 (cpool-with-mv (do* ((current-constraint-pool (pds~constraint-pool omega*current-proof-plan)
						       (pds~cstrpool-previous current-constraint-pool))
			      (back-pool nil))
			    ((or (null current-constraint-pool)
				 back-pool)
			     back-pool)
			  (when (find mv (mvs-added-by-cpool current-constraint-pool))
			    (setf back-pool current-constraint-pool)))))
    (if (null cpool-with-mv)
	(progn
	  (omega~message "~%Could not find constraint pool that contains entry for meta-variable ~A" mv)
	  (values nil
		  nil))
      (let* ((cpool-steps (pds~cstrpool-plansteps cpool-with-mv)))
	(values nil
		cpool-steps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    Strategic Control!!         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cri~def-control-rule PREFER-DEMAND-FULFILLING
		      (kind strategic)
		      (if (job-is-fulfilling-demand-modulo-parameterisation-p ("job" "parameters" "new-job")))
		      (then
		       (insert ("new-job")))) 
;; If a job fulfills a demand than it should be prefered
;; For everybody who is wondering why here stands 'insert' instead of 'prefer':
;; If an instantiations of a strategy with particular parameters is considered, then
;; the new job has to be INSERTED!


(cri~def-control-rule PREFER-OFFERS-FROM-STORE
		      (kind strategic)
		      (if (and (job-is-from-store-p "job")
			       (all-demands-satisfied-for-job "job")))
		      (then
		       (prefer ("job"))))
;; If a job comes from store it should be prefered


(cri~def-control-rule REJECT-ALREADY-APPLIED-STRATEGIES
		      (kind strategic)
		      (if (job-is-already-tried "job"))       ;; job wurde bereits auf STrategy ausprobiert
		      (then                                   ;; ->
		       (reject ("job"))))                     ;; Schmeisse Job raus!
;; If a job was already tried on a goal/task, then it should not be tried again


(cri~def-control-rule PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
                      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' on Task zurueck
				    (job-is BACKTRACK-STEP-TO-TASK "job1" "task"))    ;; Es gibt Angebote einen Step zu Backtracken
			       (job-is-not BACKTRACK-STEP-TO-TASK "job2" "task")))    ;; Es gibt andere Angebote auf TASK
 		      (then                                                           ;; ->
		       (prefer ("job1"))))
;; If during the application of a strategy the failure occured, that no further method was applicable then
;; backtracking the last step to the goal should be prefered


(cri~def-control-rule PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-START-TASK
                      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck 
				    (LAST-ROC-START-TASK "task"))                           ;; Task ist Starttask der letzten Strat.
			       (and (job-is BACKTRACK-LAST-STRATEGY-TO-TASK "job1" "task")  ;; Es gibt Angebot BACKTRACK-LAST-STRATEGY
				    (job-is-not BACKTRACK-LAST-STRATEGY-TO-TASK "job2" "task")))) ;; Es gibt noch andere Angebote Task
 		      (then                                                     ;; ->
		       (prefer ("job1"))))                                      ;; Ziehe BACKTRACK-LAST-STRATEGY-TO-TASK vor!
;; If during the application of a strategy the failure occured, that no further method was applicable and the
;; goal is the start task of the strategy application then backtracking the whole strategy should be prefered


(cri~def-control-rule DEFER-OFFERS-FROM-STORE-IF-NOT-SATISFIED-DEMANDS
		      (kind strategic)
		      (if (and (job-is-from-store-p "job")
			       (not (all-demands-satisfied-for-job "job"))))
		      (then
		       (defer ("job"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METAPREDS FOR THE STRATEGIC CONTROL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun all-demands-satisfied-for-job (job)
  (if (stringp job)
      nil
    (if (null (job~store-offer-p job))
	nil
      (let* ((roc (job~state-description job)))

	(if (roc~demands-are-fulfilled-p roc)
	    (list (list (cons T T)))
	  nil)))))

(defun job-is-fulfilling-demand-modulo-parameterisation-p (args)
  ;; das ertse von args muss job werden
  ;; zweites demand
  (let* ((job (first args))
	 (parameters (second args))
	 (new-job (third args))
	 (demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
	 (demands (store~elements demands-store))
	 (job-offers-store (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	 (job-offers (store~elements job-offers-store))
	 (job-offer-demand-pairs (do* ((rest-demands demands (rest rest-demands))
				       (back nil))
				     ((null rest-demands)
				      back)
				   (let* ((head-demand (first rest-demands))
					  (possible-jobs (remove-if-not
							  #'(lambda (job-offer)
							      (demand~demand-will-be-fulfilled-by-job-offer-modulo-parameters-p
							       head-demand
							       job-offer))
							  job-offers)))
				     
				     (when possible-jobs
				       (setf back (append back (mapcar #'(lambda (job)
									   (list job head-demand))
								       possible-jobs))))))))
				       
    (mapcar #'(lambda (pair)
		(let* ((jobi (first pair))
		       (paramsi (if (demand~parameters (second pair))
				    (demand~parameters (second pair))
				  nil))
		       (new-jobi (keim~copy jobi)))

		  (setf (job~parameters new-jobi) paramsi)
		  
		  (list (cons job jobi)
			(cons parameters paramsi)
			(cons new-job new-jobi))))
	    
	    job-offer-demand-pairs)))

(defun job-is-from-store-p (job)
  (let* ((job-offers-store (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	 (job-offers (store~elements job-offers-store))
	 (job-offers-from-store (remove-if-not #'job~store-offer-p job-offers)))

    (mapcar #'(lambda (jobi)
		(list (cons job jobi)))
	    job-offers-from-store)))


(defun job-is-already-tried (job)
  (cond ((stringp job)
	 (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
		(already-tried-job-offers (remove-if #'job~job-offer-is-not-already-tried-p all-job-offers)))
	   (if already-tried-job-offers
	       (mapcar #'(lambda (jobi)
			   (list (cons job jobi)))
		       already-tried-job-offers)
	     nil)))
	(t
	 ;; job is already bound
	 (if (job~job-offer-is-not-already-tried-p job)
	     nil
	   (list (list (cons t t)))))))

(defun job-is (signifer job task)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-signifer (cond ((strat~find-strategy-ks signifer)
				  (let* ((strat (strat~find-strategy-ks signifer)))
				    (remove-if-not #'(lambda (job)
						       (if (job~strategy-ks-offer-p job)
								  (let* ((strategy-ks (job~strategy-ks job)))
								    (if (eq strategy-ks strat)
									't
								      nil))))
						   all-job-offers)))
				 ((refalg~find-refinement-algorithm-object signifer)
				  (let* ((refalg (refalg~find-refinement-algorithm-object signifer)))
				    (remove-if-not #'(lambda (job)
						       (if (job~strategy-ks-offer-p job)
							   (let* ((strategy-ks (job~strategy-ks job))
								  (strat-refalg (strat~strategy-ks-refinement-algorithm strategy-ks)))
							     (if (eq refalg strat-refalg)
								 't
							       nil))))
						   all-job-offers)))
				 (t
				  nil))))

    ;;(format t "~% THE CURRENT JOB-OFFERS ARE:")
    ;;(mapcar #'(lambda (job)
    ;;		(format t "~%     ~A" job))
    ;;	    all-job-offers)
    
    (cond ((and (stringp job)
		(stringp task))
	   ;; -> both - job and task - are unbound!
	   
	   (if jobs-to-signifer
	       (mapcar #'(lambda (jobi)
			   (list (cons job jobi)
				 (cons task (job~task jobi))))
		       jobs-to-signifer)
	     nil))
	  
	  ((and (null (stringp job))
		(null (stringp task)))
	   ;; both - job and task - are bound!
	   
	   (if (and (find job jobs-to-signifer)
		    (eq (job~task job) task))
	       
	       (list (list (cons T T)))
	     nil))
	  
	  ((and (stringp job)
		(null (stringp task)))
	   ;; job is unbound, but task is bound!

	   (let* ((jobs-to-signifer-and-task (remove-if-not #'(lambda (job)
								(if (eq (job~task job) task)
								    't
								  nil))
							    jobs-to-signifer)))
	     (if jobs-to-signifer-and-task
		 (mapcar #'(lambda (jobi)
			     (list (cons job jobi)))
			 jobs-to-signifer-and-task)
	       nil)))

	  ((and (null (stringp job))
		(stringp task))
	   ;; job is bound, but task not!

	   (if (job~strategy-ks-offer-p job)
	       (list (list (cons task (job~task job))))
	     nil)))))	   


(defun job-is-not (signifer job task)
  (let* ((all-job-offers (remove-if #'job~store-offer-p
				    (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard))))
	 (not-jobs-to-signifer (cond ((strat~find-strategy-ks signifer)
				      (let* ((strat (strat~find-strategy-ks signifer)))
					(remove-if #'(lambda (job)
						       (if (job~strategy-ks-offer-p job)
							   (let* ((strategy-ks (job~strategy-ks job)))
							     (if (eq strategy-ks strat)
								 't
							       nil))))
						   all-job-offers)))
				     ((refalg~find-refinement-algorithm-object signifer)
				      (let* ((refalg (refalg~find-refinement-algorithm-object signifer)))
					(remove-if #'(lambda (job)
						       (if (job~strategy-ks-offer-p job)
							   (let* ((strategy-ks (job~strategy-ks job))
								  (strat-refalg (strat~strategy-ks-refinement-algorithm strategy-ks)))
							     (if (eq refalg strat-refalg)
								 't
							       nil))))
						   all-job-offers)))
				     (t
				      nil))))

    (cond ((and (stringp job)
		(stringp task))
	   ;; -> both - job and task - are unbound!
	   
	   (if not-jobs-to-signifer
	       (mapcar #'(lambda (jobi)
			   (list (cons job jobi)
				 (cons task (job~task jobi))))
		       not-jobs-to-signifer)
	     nil))
	  
	  ((and (null (stringp job))
		(null (stringp task)))
	   ;; both - job and task - are bound!
	   
	   (if (and (find job not-jobs-to-signifer)
		    (eq (job~task job) task))
	       
	       (list (list (cons T T)))
	     nil))
	  
	  ((and (stringp job)
		(null (stringp task)))
	   ;; job is unbound, but task is bound!

	   (let* ((not-jobs-to-signifer-and-task (remove-if-not #'(lambda (job)
								    (if (eq (job~task job) task)
									't
								      nil))
								not-jobs-to-signifer)))
	     (if not-jobs-to-signifer-and-task
		 (mapcar #'(lambda (jobi)
			     (list (cons job jobi)))
			 not-jobs-to-signifer-and-task)
	       nil)))
	  
	  ((and (null (stringp job))
		(stringp task))
	   ;; job is bound, but task not!
	   
	   (if (job~strategy-ks-offer-p job)
	       (list (list (cons task (job~task job))))
	     nil)))))	   

(defun LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE (strategy task)
  (if (and (exmes~failure-message-p sod*execution-message)
	   (string-equal (first (exmes~failure-message-report sod*execution-message)) 'pplanner)
	   (string-equal (second (exmes~failure-message-report sod*execution-message)) 'no-applicable-method)
	   (third (exmes~failure-message-report sod*execution-message)))
      (list (list (cons task (third (exmes~failure-message-report sod*execution-message)))
		  (cons strategy (roc~strategy-ks (exmes~state-description sod*execution-message)))))
    nil))

(defun LAST-ROC-START-TASK (task)
  (let* ((start-task-last-strat (roc~start-task (exmes~state-description sod*execution-message))))
    (cond ((stringp task)
	   ;; task noch nicht gebunden
	   (list (list (cons task start-task-last-strat))))
	  (t
	   ;; task schon gebunden
	   (if (cond ((and (agenda~goal-or-goal-schema-task-p task)
			   (agenda~goal-or-goal-schema-task-p start-task-last-strat))
		      (eq (agenda~task-node task) (agenda~task-node start-task-last-strat)))
		     ((and (agenda~inst-task-p task)
			   (agenda~inst-task-p start-task-last-strat))
		      (eq (agenda~inst-task-meta-var task)  (agenda~inst-task-meta-var start-task-last-strat)))
		     (t
		      (eq task start-task-last-strat)))
	       (list (list (cons T T)))
	     nil)))))




;;;;;;;;;;;;;;;;;;;;
;; EXPS-Strategy  ;;
;;;;;;;;;;;;;;;;;;;;

(defun pseudo-goal-p (task)
  (agenda~pseudo-goal-p task))

(strat~define-strategy-ks
 (name Exps)
 (refinement-algorithm Exp)
 (condition pseudo-goal-p)
 (parameter-types )
 (print "Strategy-KS Exps: Offer to expand pseudo-goal ~A."))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATP GENERAL STRATEGY ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-tasks-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      't
    nil))

(strat~define-strategy-ks
 (name FOATPS)
 (refinement-algorithm ATP)
 (condition line-tasks-p)
 (apply-atp-function call-foatps-on-task)
 (check-atp-out complete-res-proof-p)
 (parameter-types )
 (print "Strategy-KS call-foatps-on-task: Offer to call fo atps on a task ~A"))

(defun complete-res-proof-p (out)
  (if (and (res~proof-p out)
	   (remove-if-not #'cl~empty-p (res~proof-clauses out)))
      't
    nil))
	   
(defvar strathelp*otter-problem nil)
(defvar strathelp*bliksem-problem nil)
(defvar strathelp*spass-problem nil)

(defun call-foatps-on-task (task)
  (setf strathelp*otter-problem nil)
  (setf strathelp*bliksem-problem nil)
  (setf strathelp*spass-problem nil)
  
  (let* ((ressource 10)
	 (node (agenda~task-node task))
	 (time (get-universal-time))
	 (otter-process (proc~create :name "otter-process"
				     :priority 300
				     :function #'strathelp=call-otter-on-node 
				     :args (node ressource)))
	 (bliksem-process (proc~create :name "bliksem-process"
				       :priority 300
				       :function #'strathelp=call-bliksem-on-node 
				       :args (node ressource)))
	 (spass-process (proc~create :name "spass-process"
	                             :priority 300
	 			     :function #'strathelp=call-spass-on-node 
	 			     :args (node ressource)))
	 )

    (do* ((term-flag nil)
	  )
	(term-flag 
	 nil)
      (when (or (and strathelp*otter-problem
		     (atpprb~complete-p strathelp*otter-problem))
		(and strathelp*spass-problem
		     (atpprb~complete-p strathelp*spass-problem))
		(and strathelp*bliksem-problem
		     (atpprb~complete-p strathelp*bliksem-problem)))	
	(proc~kill otter-process)
	(proc~kill bliksem-process)
	(proc~kill spass-process)
	(setf term-flag 't)))
    
    (let* ((problems (remove-if #'null (list strathelp*otter-problem strathelp*spass-problem strathelp*bliksem-problem)))
	   (complete-problems (remove-if-not #'atpprb~complete-p problems)))
      
      ;;(format t "~%COMPLETE-PROBLEMS:")
      ;;(mapcar #'(lambda (prob)
      ;;		  (format t "~%      ~A" (atpprb~problem-type prob)))
      ;;	      complete-problems)
      
      (if complete-problems
	  (atpprb~problem-part-res-proof (first complete-problems))
	nil))))





(defun strathelp=call-otter-on-node (node ressource)
  (let* ((problem-name "OTTERM")
	 (otter-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) "/tmp/"))
	 (in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir))
	 (otter-problem (otter~generate-otter-problem node
						      (pds~node-supports node)
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof otter-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file otter-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring otter-problem-dir)))))
    
    ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (otter~add-in-string! otter-problem
			  'auto
			  (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
					"set(auto).")
				  (list "set(build_proof_object).")
				  otter*needed-flags)
			  (res~proof-clauses res-proof)
			  nil
			  't)
    
    (setq atptop*interactivity-allowed nil)
    
    ;; call-otter vot Ort -> schreibt otter.out file in den out-string des otter-problems
    (otter=call-otter! otter-problem otter-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des otter-beweises
    (otter~complete-otter-problem! otter-problem :parse-back 't)

    (setf strathelp*otter-problem otter-problem)

    ))

(defun strathelp=call-bliksem-on-node (node ressource)
  (let* ((problem-name "BLIKSEMM")
	 (bliksem-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) "/tmp/"))
	 (in-file (merge-pathnames "bliksem.in" bliksem-problem-dir))
	 (out-file (merge-pathnames "bliksem.out" bliksem-problem-dir))
	 (bliksem-problem (blik~generate-bliksem-problem node
							 (pds~node-supports node)
							 omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof bliksem-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file bliksem-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring bliksem-problem-dir)))))
    
    ;; erzeuge bliksem.in file in bliksem*in-string, fuege bliksem*in-string zum bliksem-problem hinzu (einschlieslich proof-object)
    (blik~add-in-string! bliksem-problem
			 (res~proof-clauses res-proof)
			 (format nil "~A~A~A~A~A"
				 "Set( totalproof, 1 )."
				 #\Newline
				 "Set( prologoutput, 1 )."
				 #\Newline
				 "Auto."
				 ))
        
    (setq atptop*interactivity-allowed nil)
    
    ;; call-bliksem vot Ort -> schreibt bliksem.out file in den out-string des bliksem-problems
    (blik=call-bliksem! bliksem-problem bliksem-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des otter-beweises
    (blik~complete-bliksem-problem! bliksem-problem :parse-back 't)

    (setf strathelp*bliksem-problem bliksem-problem)
    
    ))



(defun strathelp=call-spass-on-node (node ressource)
  (let* ((problem-name "SPASSM")
	 (spass-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) "/tmp/"))
	 (in-file (merge-pathnames "spass.cnf" spass-problem-dir))
	 (out-file (merge-pathnames "spass.dfg" spass-problem-dir))
	 (spass-problem (spass~generate-spass-problem node
						      (pds~node-supports node)
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof spass-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file spass-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring spass-problem-dir)))))
    
    ;; erzeuge spass.cnf file in spass*in-string, fuege spass*in-string zum spass-problem hinzu (einschlieslich proof-object)
    (spass~add-in-string! spass-problem 't 0)
    
    (setq atptop*interactivity-allowed nil)
    
    ;; call-spass vot Ort -> schreibt spass.out file in den out-string des spass-problems
    (spass=call-spass! spass-problem spass-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des spass-beweises
    (spass~complete-spass-problem! spass-problem :parse-back 't)

    (setf strathelp*spass-problem spass-problem)

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Analogy}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (arg~deftype any
	       (read-function ddummy)
	       (predicate always-true)
	       (help "Any object.")))

(defun ana~no-more-steps ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "True, iff all steps are transfered"))
  (let ((roc ana*roc-state-description))
    (null (ana~choose (ana~find-choice-control 'source-next-from-steps) (ana~matching-mapp-create roc) roc))))

(defun ana~only-tasks-open-in-source ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "True, iff the local tasks correspond to subgoals of the source steps"))
  (let ((roc ana*roc-state-description))
    (subsetp (ana~cor-set-of (roc~analogy-table roc) (mapcar 'agenda~task-node (ana~local-tasks)) 'ana=cor-target-to-sources)
	     (ana~step-add-premises (roc~analogy-source-steps roc)))))

;(defun ana~active-and-root-task-p (task)
;  (and ana*active
;       (ana~root-task-p task)))

(strat~define-strategy-ks
 (name ChronologicalAnalogy)
 (refinement-algorithm cplanner)
 (condition always-true)
 (parameter-types (any any any any))
 (parameters (source-plan steps superstep))
 (cycle-algs (
	      (search-dfs instmeta
			  (
			   source-next-from-steps
			   demand-strategy-from-source
			   metavar-from-source
			   demand-tasks-from-metavar
			   ))
	      (search-dfs pplanner-segment
			  (
			   source-next-from-steps
			   (demand-strategy ChronologicalAnalogy)
			   demand-tasks-from-source
			   source-strategy-from-source
			   start-condition-from-source
			   substeps-from-source
			   (demand-parameters (source-plan substeps source))
			   ))
	      (search-dfs method
			  (
			   source-next-from-steps
			   method-from-source
			   goal-from-source
			   supports-from-source
			   parameters-from-source
			   ))
	      ))
 (crules )
 (termination-condition ana~no-more-steps)
 (print "Strategy-KS ChronologicalAnalogy: Offer to close goal ~A by chronological analogy"))

(strat~define-strategy-ks
 (name TaskDirectedAnalogy)
 (refinement-algorithm cplanner)
 (condition always-true)
 (parameter-types (any any any any))
 (parameters (source-plan steps superstep))
 (cycle-algs (
	      (search-dfs instmeta
			  (
			   metavar-from-agenda
			   demand-tasks-from-metavar
			   source-from-metavar
			   demand-strategy-from-source
			   ))
	      (search-dfs pplanner-segment
			  (
			   goal-from-agenda
			   source-from-goal
			   (demand-strategy TaskDirectedAnalogy)
			   demand-tasks-from-source
			   source-strategy-from-source
			   start-condition-from-source
			   substeps-from-source
			   (demand-parameters (source-plan substeps source))
			   ))
	      (search-dfs method
			  (
			   goal-from-agenda
			   source-from-goal
			   method-from-source
			   supports-from-source
			   parameters-from-source
			   ))
	      ))
 (crules )
 (termination-condition ana~only-tasks-open-in-source)
 (print "Strategy-KS TaskDirectedAnalogy: Offer to close goal ~A by task directed analogy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{All Analogy Strategies}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf ana*strategies '(
		       ChronologicalAnalogy
		       TaskDirectedAnalogy
		       ))

(setf ana*strategic-control '(
			      PREFER-BACKTRACK-LAST-STEP-IF-NO-CHRONO-ANALOGY-MATCHING-FAILURE
			      PREFER-BACKTRACK-STEP-TO-TASK-IF-NO-TD-ANALOGY-MATCHING-FAILURE
			      PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-ANALOGY-MATCHING-FAILURE-AND-EMPTY-ROC-AND-START-TASK
			      ))
			      




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS                                                           ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf sod*current-strategies '(Exps
			       InstFromParam
			       InstInteractively
			       BackTrack-To-Open
			       BackTrack-MV-Inst
			       BACKTRACK-STEP-TO-TASK
			       BACKTRACK-CPOOL-STEP-AFTER-TASK
			       BACKTRACK-LAST-STRATEGY-TO-TASK
			       ))

(setf sod*current-strategic-control-rules '(PREFER-DEMAND-FULFILLING
					    PREFER-OFFERS-FROM-STORE
					    DEFER-OFFERS-FROM-STORE-IF-NOT-SATISFIED-DEMANDS
					    REJECT-ALREADY-APPLIED-STRATEGIES
					    PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
					    PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-START-TASK
					    ))

;; A view remarks about the implemented strategic control:
;; MULTI itself has no implicit control encoded!
;; The strategic control rules specified here provide a suitable default behavior.
;; For instance, PREFER-DEMAND-FULFILLING states that job-offers that fulfill a posed demand
;; should be prefered before others.
;; However, this default control can be changed without problems by specifying more specific control
;; rules.
;; Currently a control rule C1 is more specific than another control rule C2 if C1 is in the list (in the
;; call of MULTI) behind C2. This is the case since the strategic control rules are evaluated in the order
;; in which they are provided to MULTI.
;; Andreas Meier is currently thinking about a more appropriate concept to set an order among strategic
;; CRULES


