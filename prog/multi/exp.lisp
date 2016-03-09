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

;; This module contains the stuff for EXP!

#| ------------------------------------------------------ Global Variables ----------------------------------------------------------- |#

(defvar expexp*verbose nil)
;; if set the system talks to you!

(defvar expexp*roc-state-description nil)
;; The current state-description (always maintained)

#| ------------------------------------------- EXPAND AS REFINEMENT ALGORITHM ----------------------------------------------------- |#

(defun expexp~exp-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using Exp as refinement algorithm) and the task.")
	   (effect  "Exp is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; new Exp ROC-State-Description 
	 (new-exp-roc-state-description (roc~fresh-expansion-state-description strategy-ks task nil parameters))
	 ;; New start-strategy-ks justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-exp-roc-state-description)))
	 ;; New strategy-step from this justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Roc gets start-step as entry
    (setf (roc~start-step new-exp-roc-state-description) 
	  start-step)

    ;; ROC is entried in ROCS-list
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-exp-roc-state-description)
    
    ;; Strategy-step is inserted in Strategy-steps
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when expexp*verbose
      (omega~message "~%Invoking Exp WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (expexp=calling-exp new-exp-roc-state-description)))

;; The Reinvokation Function!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Since an interruption of Exp is not designated so far, a reinvokation function is omitted!                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expexp=calling-exp (roc)
  (declare (edited  "18-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for Exp.")
	   (effect  "Call Exp on it (ahwt ever this may cause) ...")
	   (value   "Messages returned from Exp."))
  
  ;; 1. Set the current ROC to the input ROC!
  (setf expexp*roc-state-description roc)

  ;; 2. Call the expansion algorithm itself
  (let* ((message (expexp~exp)))
    
    (when expexp*verbose
      (omega~message "~%Receiving the following message from termination of Exp: ~A" message))
    
    ;; 3. Process the returned message: -> now happens in suggestion-reasoner!
    ;;(expexp=interpret-execution-message! message)
    
    message
    ))


#| -------------------------------------------------- Expansion itself -------------------------------------------------------- |#

(defun expexp~exp ()
  (let* ((exp-task (roc~start-task expexp*roc-state-description))
	 (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds)))

    (multiple-value-bind
	(success new-agenda)
	(expexp=refine-task exp-task agenda pds)

      (if success

	  (progn
	    (when expexp*verbose
	      (omega~message "*** EXP: Expansion of exp-task ~A done." exp-task))
	    
	    (setf (pds~agenda pds) new-agenda)
	    
	    (exmes~create-termination-message expexp*roc-state-description
					      nil))

	;; I don't know exactly, waht can or has to happen in the case of a failed expansion
	;; I would have to see some examples, that clarify why an expansion fails.
	
	(progn
	  (when expexp*verbose
	    (omega~message "*** EXP: Expansion of exp-task ~A failed." exp-task))
	  
	  (exmes~create-failure-message expexp*roc-state-description
					nil))))))

;; THE FOLLOWING IS IN PRINCIPLE FROM LASSAADS STUFF in ~lassaad/tn-omega/prog/plan/lc-plan respectively add-plan
;;
;; New is the break up of the giant function exp=refine-task by outsourcing the fail case (which was never tested!)
;; New is further, that the newly created lines are checked, whether they are open and contain meta variables.
;; If nessessary goal and inst tasks are created then.

(defun expexp=refine-task (task agenda pds)
  (declare (edited  "17-MAR-1998")
	   (authors Lassaad)
	   (input   "A pseudo task, an agenda, and a pds.")
	   (effect  "Changes PDS by expanding the method in TASK.")
	   (value   "The changed agenda."))
  (let* ((task-node (agenda~task-node task))
	 (node-just (node~justification task-node))
	 (just-meth (just~method node-just)))
    (cond ((infer~method-p just-meth)

	   ;; expand the method application
	   (multiple-value-bind
	       (new-nodes success new-cstr-state)
	       (pexp~expand-method task-node pds)

	     (setf (roc~expansion-new-nodes expexp*roc-state-description)
		   new-nodes)
	     
	     (when expexp*verbose
	       (omega~message "*** EXP: Expansion of exp-task ~A resulted with new-nodes ~A, success ~A, and new constraint store ~A"
			      task
			      new-nodes
			      success
			      new-cstr-state))
	     
	     ;; - Check whether new-nodes was inserted in PDS
	     ;; - Check whether the new-nodes have the own reason of TASK, i.e., the reason
	     ;; which corresponds to the expanded method.
	     ;; - Check whether a new planning step is inserted 
	     ;; - Replace TASK with the NEW-TASKS which correspond to the nodes
	     ;; in NEW-NODES that are open or that are justified by a unreliable
	     ;; method.
	     ;; - In case of new open nodes, inheritance of supports?
	     ;; - In case of orderings, the orderings must be interpreted too
	     ;; - In case of NEW-CSTR-STATE with SUBST, i.e., bindings for
	     ;; meta-variables, the following should be done:
	     ;; A: SUBST should be composed with the previous SUBST
	     ;; B: SUBST should be applied to the pseudo-goals on AGENDA and
	     ;; the schematic supports of the goals on AGENDA must be considered
	     ;; as new supports, normalizing methods can be applied to the current
	     ;; instance of these supports. 
	     ;; IMPLEMENT the needed stuff for the expansion of BH-AndE

	     (if success
		 ;; --> Successful expansion:
		 (let ((expansion-step (pds~change-last-plan-step! task-node pds))
		       (an-agenda agenda))
		   
		   ;; EXPANSION-STEP is created only to note that the expansion is carried out during
		   ;; the planning phase. When the expansion step changes the PDS constraint state, then
		   ;; we use EXPANSION-STEP as the reason for this changement.
		   ;; The reson which we insert into the NEW-NODES corresponds to the step of the method
		   ;; application which led to this expansion. 

		   ;; add new nodes into pds
		   (dolist (node new-nodes)
		     (when (null (find node (prob~proof-steps pds)))
		       (pds~insert-node! node pds)))

		   ;; update the constraint store (and the up-to-date flags) when new constraints are created
		   (when new-cstr-state
		     (expexp=update-constraint-store! new-cstr-state expansion-step pds))

		   ;; create new tasks
		   (let* ((new-pseudo-nodes (remove-if-not #'expexp=pseudo-closed-p (cons task-node new-nodes)))
			  (new-pseudo-tasks (mapcar #'(lambda (node)
							(agenda~create-pseudo-goal node))
						    new-pseudo-nodes))
			  (new-inst-tasks (pplan=new-inst-tasks an-agenda pds (pds~constraint-pool pds) new-nodes))
			  (new-goal-tasks (apply #'append (mapcar #'(lambda (node)
								      (if (pdsn~open-node-p node)
									  (list (pplan=create-goal-task node))
									nil))
								  (cons task-node new-nodes)))))
		     (mapc #'(lambda (task)              ;; meta-variables in reopened nodes are not up-to-date MP
			       (let ((node (agenda~task-node task)))
				 (mapc #'(lambda (nod)
					   (when (pdsn~schematic-p nod)(setf (pdsn~up-to-date nod) nil)))
				       (cons node (pds~node-supports node)))))
			   new-goal-tasks)
		     (values 't
			     (agenda~replace-task task nil (append new-inst-tasks new-goal-tasks new-pseudo-tasks) nil an-agenda))))
	       ;; --> Expansion failed:
	       (omega~error "Don't know how to deal with a failure in the expansion!"))))
	  (t
	   (let ((new-nodes (infer~apply-expansion-function
			     just-meth
			     (oc~build-outline task-node
					       (pdsj~outline-pattern node-just)
					       (just~premises node-just))
			     (pdsj~parameters node-just)))
		 (expansion-step (pds~change-last-plan-step! task-node pds)))

	     (setf (roc~expansion-new-nodes expexp*roc-state-description)
		   new-nodes)
	     
	     (when expexp*verbose
	       (omega~message "*** EXP: Expansion of exp-task ~A resulted with new-nodes ~A"
			      task
			      new-nodes))

	     ;; add new nodes into pds
	     (dolist (node new-nodes)
	       (when (null (find node (prob~proof-steps pds)))
		 (pds~insert-node! node pds)))

	     ;; create new tasks
	     (let* ((new-pseudo-nodes (remove-if-not #'expexp=pseudo-closed-p (cons task-node new-nodes)))
		    (new-pseudo-tasks (mapcar #'(lambda (node)
						  (agenda~create-pseudo-goal node))
					      new-pseudo-nodes))							     
		    (new-inst-tasks (pplan=new-inst-tasks agenda pds (pds~constraint-pool pds) new-nodes))
		    (new-goal-tasks (apply #'append (mapcar #'(lambda (node)
								(if (pdsn~open-node-p node)
								    (list (pplan=create-goal-task node))
								  nil))
							    (cons task-node new-nodes)))))
	       (values 't
		       (agenda~replace-task task nil (append new-inst-tasks new-goal-tasks new-pseudo-tasks) nil agenda))))
	   
	   (error "~%Node ~A justified by ~A may not be considered as a pseudo task!" task-node just-meth)))
    ))



;; AMEIER:
;; Was not able to test the following:
(defun expexp=update-constraint-store! (new-cstr-state expansion-step pds)
  ;; when there are new constraints by the expansion, then:
  ;; a) add the expansion step as the reason to this constraint-store
  ;; b) add the new constraint state as the current one to the pds
  ;; c) update schematic lines, when new bindings are created
  
  (when expexp*verbose
    (omega~message "*** EXP: New constraint store ~A is introduced into the pds." new-cstr-state))
  
  (setf (pds~cstrpool-plansteps new-cstr-state) (list expansion-step))
  (setf	(pds~constraint-pool pds) new-cstr-state)
  
  (let ((new-mvar-subst (pds~cstrpool-bindings new-cstr-state)))
    
    (cond ((not (subst~empty-p new-mvar-subst))
	   ;; ---> NEW Meta-Variables Bindings
	   ;; - possibly composing its mvar-subst with the previous mvar-subst
	   ;; - associating the changement of the cstr-state to this expansion step
	   
	   (when expexp*verbose
	     (omega~message "New constraint state contains new bindings for meta-variables ~A" new-mvar-subst)
	     (omega~message "Therefore, schematic lines are updated."))
	   
	   (let ((previous (pds~cstrpool-previous new-cstr-state)))
	     ;; Application of new-subst to the old-subst:
	     ;; - Is there a non-empty old-susbt, then compose new-subst with old-subst
	     ;; - Is there no non-empty old-susbt, then take old-subst
	     (when (and (pds~constraint-pool-p previous)
			(not (subst~empty-p (pds~cstrpool-bindings previous))))
	       (setf (pds~cstrpool-bindings new-cstr-state)
		     (subst~compose-substitution (pds~cstrpool-bindings new-cstr-state)
						 (pds~cstrpool-bindings previous)))))
	   
	   ;; Reset the UP-TO-DATE flags of relevant nodes, since some meta-variables
	   ;; are bound during the expansion. The called function returns tasks which
	   ;; were bound before the expansion, and tasks which are bound due to the
	   ;; meta-variable bindings caused by the expansion:
	   (multiple-value-bind
	       (previously-bound-tasks newly-bound-tasks)
	       (pds~update-schematic-nodes! pds)
	     ))
	  
	  ((pds~cstrpool-previous new-cstr-state)
	   ;; No meta-variables are bound by this method expansion: take the previous MVAR-SUBST
	   
	   (setf (pds~cstrpool-bindings new-cstr-state)
		 (pds~cstrpool-bindings (pds~cstrpool-previous new-cstr-state)))))))



;; HOW TO DEAL WITH FAILURES IN THE EXPANSION?
;; MAYBE THIS HELPS:

;; ;; This function has simply been taken over from Lassaads stuff!!
;; ;; It has to be tested, adapted etc.
;;(defun expexp=expansion-fails (task task-node agenda pds)
;;  (omega~error "~%The case that an expanion fails is not yet checked, remark from function expexp=expansion-fails")
;;    
;;  ;; TASK-NODE cannot be expanded, since the expansion condition is evaluated to NIL.
;;  ;; This is the case when the expansion condition is not compatible with the current
;;  ;; constraint state. Therefore we must proceed as follows:
;;  ;; CASE A: JUST-METH applied to TASK-NODE in a planning step which preceeds the last
;;  ;; planning step that affected the constraint state, then:
;;  ;; - take back the last planning step that affected the constraint state
;;  ;; - call plan=refine-task recursively with the new agenda and new PDS
;;  ;; CASE B: The last planning step that affected the constraint pool is before the
;;  ;; application of JUST-METH, then
;;  ;; - take back JUST-METH
;;  (print "Expansion result:")
;;  (if (agenda~empty-p (pds~agenda pds))
;;      (format t "~%Empty Agenda!")
;;    (format t "~%Agenda:~%~A" (oc=show-the-agenda (pds~agenda pds))))
;;  (format T "~%The node ~A ~%The steps ~A" task-node (pds~plan-steps pds))
;;  (let ((own-reason (pdsj~own-reason node-just)))
;;    (cond ((not own-reason)
;;	   (omega~error ";;;plan=refine-task Something went wrong in the expansion of ~A closed in a previous expansion."
;;			task-node)
;;	   (error "Extend plan=refine-task"))
;;	  (T ;; The method to be expanded was applied to close TASK-NODE:
;;	   (let* ((pds-cstr (pds~constraint-pool pds))
;;		  (cstr-steps (when pds-cstr (pds~cstrpool-plansteps pds-cstr)))
;;		  (task-node-succs (pdsj~successors node-just)))
;;	     (cond ((and cstr-steps (intersection cstr-steps task-node-succs))
;;
;;		    ;; CASE A:
;;		    
;;		    (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds)))
;;		      (cond ((plan=expansion-step-p (first cstr-steps))
;;			     ;; CSTR-STP is an expansion step:
;;			     ;; - Remove this expansion --> expanded-node
;;			     ;; i) expanded-node is closed after TASK-NODE, then open this node, reset its
;;			     ;; control and order it after TASK
;;			     ;; ii) O/w: open TASK-NODE and exclude the applied method.
;;			     (let* ((expanded-node (plan=remove-expansion! (first cstr-steps) pds))
;;				    (exp-node-stp (pdsj~own-reason (node~justification expanded-node))))
;;			       (cond ((not exp-node-stp)
;;				      (omega~error
;;				       ";;;plan=refine-task must be extended to handle recursive expansions!")
;;				      ;; Principle: One applied method must be taken back either that of TASK-NODE
;;				      ;; or that of EXPANDED-NODE
;;				      (error "Extend plan=refine-task"))
;;				     ((find exp-node-stp task-node-succs)
;;				      (let ((exp-node (pdsc~an-node exp-node-stp))
;;					    (exp-just (pdsc~an-just exp-node-stp)))
;;					;; Pass the control information of EXP-NODE
;;					(unless (pdsn~alternative-mmatchings exp-node)
;;					  (multiple-value-bind (outln-pat)
;;					      (plan~actual-outline-pattern exp-node pds exp-just)
;;					    (let ((the-method (pds~inference-application
;;							       (just~method exp-just) outln-pat)))
;;					      (if (pdsj~parameters exp-just)
;;						  (setf
;;						   (pdsn~failed-methods exp-node) NIL
;;						   (pdsn~alternative-methods exp-node) NIL)	
;;						(setf
;;						 (pdsn~failed-methods exp-node) NIL
;;						 (pdsn~alternative-methods exp-node) NIL
;;						 )))))
;;					;; Open EXP-NODE and make PDS and AGENDA consistent
;;					(multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
;;					    (plan=consistent-opens! cstrsteps2succs
;;								    (cons (first cstr-steps)
;;									  (pds~open-node! exp-node pds))
;;								    NIL (list exp-node) NIL
;;								    pds (prob~proof-steps pds))
;;					  (let* ((the-meth-nodes
;;						  (remove-duplicates (union meth-nodes pseudog-nodes)))
;;						 (the-other-nodes
;;						  (remove-duplicates (set-difference other-nodes the-meth-nodes))))
;;					    (if the-other-nodes
;;						(let ((extra-supps))
;;						  (dolist (each-node the-other-nodes)
;;						    (setq extra-supps
;;							  (union extra-supps
;;								 (plan=set-difference
;;								  (pdsn~just-sponsors each-node)
;;								  (pdsn~just-unsponsors each-node)))))
;;						  (multiple-value-bind (the-agenda rest-opens)
;;						      (plan=normalize-supports extra-supps the-other-nodes agenda pds)
;;						    (let* ((root-new-task
;;							    (when pds-rootp (agenda~create-goal pds-rootp)))
;;							   (new-tasks
;;							    (if pds-rootp
;;								(cons
;;								 root-new-task
;;								 (append (mapcar #'agenda~create-task the-meth-nodes)
;;									 (mapcar #'agenda~create-goal rest-opens)))
;;							      (append (mapcar #'agenda~create-task the-meth-nodes)
;;								      (mapcar #'agenda~create-goal rest-opens))))
;;							   (new-agenda
;;							    (pds~reset-schematic-nodes!
;;							     pds
;;							     (if new-tasks
;;								 (agenda~insert-tasks
;;								  task nil new-tasks
;;								  (list (agenda~ordering-create
;;									 (list task) new-tasks))
;;								  the-agenda)
;;							       the-agenda))))
;;						      (setf (pds~agenda pds) new-agenda)
;;						      new-agenda)))
;;					      
;;					      (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
;;						     (new-tasks (if pds-rootp
;;								    (cons root-new-task
;;									  (mapcar #'agenda~create-task the-meth-nodes))
;;								  (mapcar #'agenda~create-task the-meth-nodes)))
;;						     (new-agenda (pds~reset-schematic-nodes!
;;								  pds
;;								  (if new-tasks
;;								      (agenda~insert-tasks
;;								       task nil new-tasks
;;								       (list (agenda~ordering-create
;;									      (list task) new-tasks)) agenda)
;;								    agenda))))
;;						(setf (pds~agenda pds) new-agenda)
;;						new-agenda))))))
;;				     (T
;;				      (omega~error
;;				       ";;;plan=refine-task must be extended!")
;;				      ;; Principle: Open TASK-NODE
;;				      (error "Extend plan=refine-task")))))
;;			    (T ;; CSTR-STP is a method application:
;;			     (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
;;				 (let ((to-open (pdsc~an-node (first cstr-steps))))
;;				   ;; Reset the control of TO-OPEN:
;;				   (setf (pdsn~alternative-mmatchings to-open) NIL
;;					 (pdsn~alternative-methods to-open) NIL)
;;				   (plan=consistent-opens! cstrsteps2succs (pds~open-node! to-open pds)
;;							   NIL (list to-open) NIL pds (prob~proof-steps pds)))
;;			       (let* ((the-meth-nodes (remove-duplicates (union meth-nodes pseudog-nodes)))
;;				      (the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
;;				 (if the-other-nodes
;;				     (let ((extra-supps))
;;				       (dolist (each-node the-other-nodes)
;;					 (setq extra-supps
;;					       (union extra-supps
;;						      (plan=set-difference (pdsn~just-sponsors each-node)
;;									   (pdsn~just-unsponsors each-node)))))
;;				       (multiple-value-bind (the-agenda rest-opens)
;;					   (plan=normalize-supports extra-supps the-other-nodes agenda pds)
;;					 (let* ((root-new-task
;;						 (when pds-rootp (agenda~create-goal pds-rootp)))
;;						(new-tasks
;;						 (if pds-rootp
;;						     (cons root-new-task
;;							   (append (mapcar #'agenda~create-task the-meth-nodes)
;;								   (mapcar #'agenda~create-goal rest-opens)))
;;						   (append (mapcar #'agenda~create-task the-meth-nodes)
;;							   (mapcar #'agenda~create-goal rest-opens))))
;;						(new-agenda
;;						 (pds~reset-schematic-nodes!
;;						  pds
;;						  (if new-tasks
;;						      (agenda~insert-tasks
;;						       task nil new-tasks
;;						       (list (agenda~ordering-create (list task) new-tasks))
;;						       the-agenda)
;;						    the-agenda))))
;;					   (setf (pds~agenda pds) new-agenda)
;;					   new-agenda)))
;;				   (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
;;					  (new-tasks (if pds-rootp
;;							 (cons root-new-task
;;							       (mapcar #'agenda~create-task the-meth-nodes))
;;						       (mapcar #'agenda~create-task the-meth-nodes)))
;;					  (new-agenda (pds~reset-schematic-nodes!
;;						       pds
;;						       (if new-tasks
;;							   (agenda~insert-tasks
;;							    task nil new-tasks
;;							    (list (agenda~ordering-create (list task)
;;											  new-tasks))
;;							    agenda)
;;							 agenda))))
;;				     (setf (pds~agenda pds) new-agenda)
;;				     new-agenda))))))))
;;		   (T ;; CASE B:
;;		    (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds)))
;;		      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
;;			  (plan=consistent-opens! cstrsteps2succs
;;						  (pds~open-node! task-node pds)
;;						  NIL NIL NIL pds (prob~proof-steps pds))
;;			(let* ((the-meth-nodes (remove-duplicates (union meth-nodes pseudog-nodes)))
;;			       (the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
;;			  (if the-other-nodes
;;			      (let ((extra-supps))
;;				(dolist (each-node the-other-nodes)
;;				  (setq extra-supps
;;					(union extra-supps
;;					       (plan=set-difference (pdsn~just-sponsors each-node)
;;								    (pdsn~just-unsponsors each-node)))))
;;				(multiple-value-bind (the-agenda rest-opens)
;;				    (plan=normalize-supports extra-supps the-other-nodes agenda pds)
;;				  (let* ((root-new-task
;;					  (when pds-rootp (agenda~create-goal pds-rootp)))
;;					 (new-tasks
;;					  (if pds-rootp
;;					      (cons root-new-task
;;						    (append (mapcar #'agenda~create-task the-meth-nodes)
;;							    (mapcar #'agenda~create-goal rest-opens)))
;;					    (append (mapcar #'agenda~create-task the-meth-nodes)
;;						    (mapcar #'agenda~create-goal rest-opens))))
;;					 (new-agenda
;;					  (pds~reset-schematic-nodes!
;;					   pds (agenda~replace-task task nil new-tasks nil the-agenda))))
;;				    (setf (pds~agenda pds) new-agenda)
;;				    new-agenda)))
;;			    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
;;				   (new-tasks (if pds-rootp
;;						  (cons root-new-task
;;							(mapcar #'agenda~create-task the-meth-nodes))
;;						(mapcar #'agenda~create-task the-meth-nodes)))
;;				   (new-agenda (pds~reset-schematic-nodes!
;;						pds (agenda~replace-task task nil new-tasks nil agenda))))
;;			      (setf (pds~agenda pds) new-agenda)
;;			      new-agenda))))))))))));;
;;
;;
;;
;;
;;

#| ---------------------------------------------------------------- Auxiliaries ----------------------------------------------------- |#

(defun expexp=pseudo-closed-p (node &optional (pds omega*current-proof-plan))
  ;;; Should verify whether the node would have been closed with a non-reliable method
  (let* ((node-just (node~justification node))
	 (inference (just~method node-just)))
    (when (infer~method-p inference)
      (multiple-value-bind
	  (outln-pat)
	  (expexp~actual-outline-pattern node pds node-just)
	(meth~non-reliability (pds~inference-application inference outln-pat))))
    ))

(defun expexp~actual-outline-pattern (node &optional (pds omega*current-proof-plan)
					 (just (node~justification node)) undone-steps)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "A node, a PDS, and the node current justification.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (prems (just~premises just))
	 (concs-l (- (length outln-pat) (length prems))))
    (if (> concs-l 1)
	;;More than one conclusion:
	(expexp=adapt-outline-pattern outln-pat node (- concs-l 1) pds undone-steps)
      (values outln-pat (list node)))
    ))

;; From Lassaads planning stuff, formerly plan=...
;;LC: A similar function must be given in the tactic system
(defun expexp=adapt-outline-pattern (pattern conc concs-l pds &optional undone-steps)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "An outline-pattern, a node, a positive integer, and a PDS:"
		    "CONC is justified by the same method application together"
		    "with CONCS-L other nodes in PDS.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (if (zerop concs-l)
      (if conc
	  (values pattern (list conc))
	(values pattern nil))
    (let ((first-pat (first pattern)))
      (if (or (infer~existent-pattern-p first-pat)
	      (infer~nonexistent-pattern-p first-pat))
	  ;;;FIRST-PAT is the outline of CONC:
	  (multiple-value-bind (patt concs)
	      (expexp=adapt-outline-pattern (rest pattern) nil concs-l pds undone-steps)
	    (values (cons first-pat patt)
		    (cons conc concs)))
	;;;FIRST-PAT is a label of another conclusion:
	(multiple-value-bind (patt concs)
	    (expexp=adapt-outline-pattern (rest pattern) conc (- concs-l 1) pds undone-steps)
	  (let* ((node (or (pds~label2node first-pat pds)
			   (let ((assoc-stp (find-if #'(lambda (stp)
							 (string-equal first-pat
								       (keim~name (pdsc~an-node stp))))
						     undone-steps)))
			     (when assoc-stp (pdsc~an-node assoc-stp)))))
		 (node-just (node~justification node))
		 (node-pat (pdsj~conclusion-outline node-just)))
	    (values (cons node-pat patt)
		    (cons node concs))))))
    ))

;; From Lassaads expansion stuff
(defun expexp~actual-outline-pattern2 (node &optional (pds omega*current-proof-plan)
					   (just (node~justification node)))
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "A node, a PDS, and the node current justification.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (prems (just~premises just))
	 (concs-l (- (length outln-pat) (length prems))))
    (if (> concs-l 1)
	;;More than one conclusion:
	(expexp=adapt-outline-pattern2 outln-pat node (- concs-l 1) pds)
      (values outln-pat (list node)))
    ))

;; From Lassaads expansion stuff
(defun expexp=adapt-outline-pattern2 (pattern conc concs-l pds)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "An outline-pattern, a node, a positive integer, and a PDS:"
		    "CONC is justified by the same method application together"
		    "with CONCS-L other nodes in PDS.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (if (zerop concs-l)
      (if conc
	  (values pattern (list conc))
	(values pattern nil))
    (let ((first-pat (first pattern)))
      (if (or (infer~existent-pattern-p first-pat)
	      (infer~nonexistent-pattern-p first-pat))
	  ;;;FIRST-PAT is the outline of CONC:
	  (multiple-value-bind (patt concs)
	      (expexp=adapt-outline-pattern2 (rest pattern) nil concs-l pds)
	    (values (cons first-pat patt)
		    (cons conc concs)))
	;;;FIRST-PAT is a label of another conclusion:
	(multiple-value-bind (patt concs)
	    (expexp=adapt-outline-pattern2 (rest pattern) conc (- concs-l 1) pds)
	  (let* ((node (pds~label2node first-pat pds))
		 (node-just (node~justification node))
		 (node-pat (pdsj~conclusion-outline node-just)))
	    (values (cons node-pat patt)
		    (cons node concs))))))
    ))


;;;;
;;;; New interactive expansion for Multi planned method applications.
;;;; 

(defmethod infer~apply-expansion-function ((inference infer+method) (outline list) parameters)
  (declare (ignore parameters))
  (let* ((conc (car outline))
	 (task (expexp~pseudo-goal-task conc)))
    (if task
	(let ((strat (strat~find-strategy-ks 'exps))
	      (old-tasks (remove task (agenda~all-tasks (pds~agenda omega*current-proof-plan))))) ;; test here
	  (if strat
		(let* ((outline-pattern
			(exp~actual-outline-pattern conc omega*current-proof-plan (node~justification conc)))
		       (real-method (pds~inference-application (node~just-method conc) outline-pattern)))
		  (expexp~exp-invokation-function strat task nil)
		  (when (meth~critical-p real-method)
		    (expexp~call-multi-on-new-tasks old-tasks)))
	    (omega~error ";;;METHOD EXPANSION: Strategy EXPS does not exist!!!")))

      (multiple-value-bind
	  (new-nodes success new-cstr-state)
	  (pexp~expand-method (first outline) omega*current-proof-plan)
	(when success
	  ;; --> Successful expansion:
	  (let ((expansion-step (pds~change-last-plan-step! (first outline) omega*current-proof-plan)))
	    ;; add new nodes into pds
	    (dolist (node new-nodes)
	      (when (null (find node (prob~proof-steps omega*current-proof-plan)))
		(pds~insert-node! node omega*current-proof-plan)))
	    ;; update the constraint store (and the up-to-date flags) when new constraints are created
	    (when new-cstr-state
	      (expexp=update-constraint-store! new-cstr-state expansion-step omega*current-proof-plan))))))))
	    
(defun expexp~pseudo-goal-task (node)
  (declare (edited  "05-JUN-2003")
	   (authors Vxs)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A pseudo task containing the node in the current agenda, if it exists. O/w NIL."))
  (let ((agenda (pds~agenda omega*current-proof-plan)))
    (find-if #'(lambda (x) (and (agenda~pseudo-goal-p x)
				(eq node (agenda~task-node x))))
	     (agenda~all-tasks agenda))))


(defun expexp~call-multi-on-new-tasks (old-tasks)
  (declare (edited  "05-JUN-2003")
	   (authors Vxs)
	   (input   "A list of tasks.")
	   (effect  "Calls Multi on the new tasks. i.e. those different from the task in OLD-TASKS."
		    "After calling Multi the agenda changed to contain both the task generated by the"
		    "call to Multi and the old-tasks.")
	   (value   "Undefined."))
  (print old-tasks)
  (print (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
  (let* ((new-tasks (set-difference (agenda~all-tasks (pds~agenda omega*current-proof-plan)) old-tasks))
	 (new-agenda (agenda~generate (list new-tasks)))
	 (real-old-tasks (set-difference old-tasks new-tasks)))
    (setf (pds~agenda omega*current-proof-plan) new-agenda)
    (sod~system-go)
    (setf (pds~agenda omega*current-proof-plan)
	  (agenda~generate
	   (list (agenda~all-tasks (pds~agenda omega*current-proof-plan)) real-old-tasks))))
  (print (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
  )
