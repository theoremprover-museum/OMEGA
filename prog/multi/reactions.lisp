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

(mod~defmod REAC 
            :uses (agenda black exmes keim omega pds roc scon sod store strat)
            :documentation "Stuff and data-structures for reactions"
            :exports (
                      reac+forbidalltaskstermination
                      reac+forbidtasktermination
                      reac+interrupt-reaction
                      reac+reaction
                      reac+simpletermination
                      reac+termination-reaction
                      
                      reac~create-forbidalltaskstermination
                      reac~create-forbidtasktermination
                      reac~create-interrupt
                      reac~create-reaction
                      reac~create-simpletermination
                      reac~create-termination-reaction
                      reac~forbidalltaskstermination-p
                      reac~forbidtasktermination-p
                      reac~interpret-reaction!
                      reac~interrupt
                      reac~reaction-demands
                      reac~reaction-execution-message
                      reac~reaction-p
                      reac~simpletermination-p
                      reac~termination-reaction-p
                      reac~termination-reaction-success
                      ))

;;
;;
;; With reactions can be controled, what should exactly happen with the last strategy.
;; At the moment this makes only sense in connection with a failure message, since the reactions to interrupts etc. are still hard coded
;; -> see Sod-main.lisp
;;
;; We have the following situation, a strategy, say a pplaner strategy S returns with a failure!
;; Subsequent the meta reasoner chooses a job offer!
;; The meta reasoner respectively the strategic control rules can (at the moment) create four different reactions and annotate these to the job offer:
;; 1.) SimpleTermination: S is simply terminated!
;; 2.) ForbidTaskTermination: S is terminated and the task, on which no method was applicable, is annotated with the entry,
;;                            that s should not be applied to it again.
;; 3.) ForbidAllTasksTermination: S is terminated and all tasks of S are annotated with the entry, that S should not be applied to them again.
;; 4.) Interrupt: S is treated as interrupted and this job offer, on which the reaction is annotated, is seen as demand!
;;
;;
;; Each of the termination reaction additionally has a success entry, that signals, whether the termination should be seen as success even though.
;; In this case, the demands, that are fulfilled by it, are deleted!
;;
;; Furthermore each reaction has a demands entry. These are stored in the demands!

#| --------------------------------------------------------- possible reactions ------------------------------------------------------ |#

(eval-when (load compile eval)
  (defclass reac+reaction (keim+object)
    ((execution-message :initform nil
			:initarg :execution-message
			:accessor reac~reaction-execution-message)
     (demands :initform nil
	      :initarg :demands
	      :accessor reac~reaction-demands))))
  
(defun reac~reaction-p (obj)
  (typep obj 'reac+reaction))

(defmethod print-object ((reac reac+reaction) stream)
  (format stream "A reaction on execution message ~A with demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~reaction-demands reac)))

(defun reac~create-reaction (execution-message demands)
  (make-instance 'reac+reaction
		 :execution-message execution-message
		 :demands demands))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Termination

(eval-when (load compile eval)
  (defclass reac+termination-reaction (reac+reaction)
    ((success :initform nil
	      :initarg :success
	      :accessor reac~termination-reaction-success))))
  
(defun reac~termination-reaction-p (obj)
  (typep obj 'reac+termination-reaction))

(defmethod print-object ((reac reac+termination-reaction) stream)
  (format stream "A termination reaction on execution message ~A with success ~A and demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~termination-reaction-success reac)
	  (reac~reaction-demands reac)))

(defun reac~create-termination-reaction (execution-message demands success)
  (make-instance 'reac+reaction
		 :execution-message execution-message
		 :demands demands
		 :success success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SimpleTermination

(eval-when (load compile eval)
  (defclass reac+simpletermination (reac+termination-reaction)
    ()))

(defun reac~simpletermination-p (obj)
  (typep obj 'reac+simpletermination))

(defmethod print-object ((reac reac+simpletermination) stream)
  (format stream "A SimpleTermination reaction on execution message ~A with success ~A and demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~termination-reaction-success reac)
	  (reac~reaction-demands reac)))

(defun reac~create-simpletermination (execution-message demands success)
  (make-instance 'reac+simpletermination
		 :execution-message execution-message
		 :demands demands
		 :success success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Forbidtasktermination

(eval-when (load compile eval)
  (defclass reac+forbidtasktermination (reac+termination-reaction)
    ()))

(defun reac~forbidtasktermination-p (obj)
  (typep obj 'reac+forbidtasktermination))

(defmethod print-object ((reac reac+forbidtasktermination) stream)
  (format stream "A Forbidtasktermination reaction on execution message ~A with success ~A and demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~termination-reaction-success reac)
	  (reac~reaction-demands reac)))

(defun reac~create-forbidtasktermination (execution-message demands success)
  (make-instance 'reac+forbidtasktermination
		 :execution-message execution-message
		 :demands demands
		 :success success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Forbidalltaskstermination

(eval-when (load compile eval)
  (defclass reac+forbidalltaskstermination (reac+termination-reaction)
    ()))

(defun reac~forbidalltaskstermination-p (obj)
  (typep obj 'reac+forbidalltaskstermination))

(defmethod print-object ((reac reac+forbidalltaskstermination) stream)
  (format stream "A Forbidalltaskstermination reaction on execution message ~A with success ~A and demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~termination-reaction-success reac)
	  (reac~reaction-demands reac)))

(defun reac~create-forbidalltaskstermination (execution-message demands success)
  (make-instance 'reac+forbidalltaskstermination
		 :execution-message execution-message
		 :demands demands
		 :success success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interrupt

(eval-when (load compile eval)
  (defclass reac+interrupt-reaction (reac+reaction)
    ()))

(defun reac~interrupt (obj)
  (typep obj 'reac+interrupt-reaction))

(defmethod print-object ((reac reac+interrupt-reaction) stream)
  (format stream "A Interrupt reaction on message ~A with demands ~A"
	  (reac~reaction-execution-message reac)
	  (reac~reaction-demands reac)))

(defun reac~create-interrupt (execution-message demands)
  (make-instance 'reac+interrupt-reaction
		 :demands demands
		 :execution-message execution-message))


#| -------------------------------------------------------- interpret reaction ------------------------------------------------------ |#


(defgeneric reac~interpret-reaction! (reaction)
  (declare (edited  "27-AUG-1999")
	   (authors Ameier)
	   (input   "A MULTI reaction.")
	   (effect  "Depends on the reaction (see the single cases).")
	   (value   "Undefined."))
  
  (:method ((reaction reac+termination-reaction))
  
	   ;; Execution of a strategy-ks is terminated
	   ;; 1. ROC has to be removed from TASKS (any other TASKS, exp ...)
	   ;; 2. New strategy-step is created and inserted in strategy-steps
	   ;; 3. activ flag in state-des is set to nil
	   ;; (the termination so far)
	   ;; 4. New demands are entried in the store.
	   ;; 5. If success: All fulfilled demands are removed from the demands store
	   ;; 6. Depending on the termination reaction it is annotated in the tasks, that have this ROC. in the strategy control,
	   ;;    that this strategy has failed on them!
	   
	   (let* ((demands (reac~reaction-demands reaction))
		  (exmes (reac~reaction-execution-message reaction))
		  (success (reac~termination-reaction-success reaction))
		  (state-des (exmes~state-description exmes)))

	     (when (null state-des)
	       (error "~%Can not terminate before start, see function reac=interpret-reaction"))
	     
	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF REACTION**: TERMINATE EXECUTION of strategy-ks ~A on task ~A."
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF REACTION**: The state description is deleted from all tasks and gets inactive.")
	       (omega~message "**SOD:: INTERPRETATION OF REACTION**: New demands ~A are stored."))
	     
	     ;; 1. + 6 Remove the ROCS! + Entry in control 
	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		    (strategy-ks (roc~strategy-ks state-des))
		    (all-tasks (agenda~all-tasks agenda)))
	       (mapcar #'(lambda (task)
			   (let* ((rocs (agenda~task-rocs task)))
			     (when (find state-des rocs)
			       ;; zu 1.
			       (setf (agenda~task-rocs task) (remove state-des rocs))
			       ;; zu 6.
			       (cond ((reac~forbidalltaskstermination-p reaction)
				      ;; If ForbidAllTaskstermination -> Each Task get entry that it was already tackled by this strategy
				      
				      ;;(setf (agenda~task-already-applied-strategy-ks task)
				      ;;	    (remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
				      ;;				     (agenda~task-already-applied-strategy-ks task))
				      ;; 			       :test #'keim~equal)))
				      ;; already-applied-strategy-ks are now stored at the node directly

				      (if (agenda~goal-or-goal-schema-task-p task)
					  (setf (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))
						(remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
									 (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task)))))    
								   :test #'keim~equal))
					(setf (agenda~task-already-applied-strategy-ks task)
					      (remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
								       (agenda~task-already-applied-strategy-ks task))
								 :test #'keim~equal))))
				     
				     
				     ((and (reac~forbidtasktermination-p reaction)
					   (string-equal (first (exmes~failure-message-report exmes)) 'pplanner)
					   (string-equal (second (exmes~failure-message-report exmes)) 'no-applicable-method)
					   (third (exmes~failure-message-report exmes))
					   (eq task (third (exmes~failure-message-report exmes))))
				      ;; If forbidtasktermination -> only failure task gets entry that it was already tackled by this
				      ;; strategy
				      
				      ;;(setf (agenda~task-already-applied-strategy-ks task)
				      ;;(remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
				      ;;					     (agenda~task-already-applied-strategy-ks task))
				      ;;			       :test #'keim~equal)))
				      ;; already-applied-strategy-ks are now stored at the node directly

				      (if (agenda~goal-or-goal-schema-task-p task)				      
					  (setf (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))
						(remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
									 (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task)))))    
								   :test #'keim~equal))
					(setf (agenda~task-already-applied-strategy-ks task)
					      (remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
								       (agenda~task-already-applied-strategy-ks task))
								 :test #'keim~equal))))
				     (t
				      nil)))))
		       all-tasks))
	     
	     ;; 2. New strategy-step is created and inserted in strategy-steps
	     (let* (;; an end-strategy-application-justification is created!
		    (new-justification (strat~end-strategy-ks-application-create-just (list state-des)))
		    ;; For thid purpose a new scon step is created:
		    (strategy-end-step (scon~create-scon-step nil nil new-justification)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! strategy-end-step))
	     
	     ;; 3. activ flag in state-des is set to nil
	     (setf (roc~activ state-des) nil)

	     ;; 4. If new demands -> add to demands-store
	     (when demands
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands))

	     ;; 5. If success -> delete fulfilled demands
	     (when success
	       (multiple-value-bind
		   (store2 rem del)
		   (store~demands-store-remove-fulfilled-demands! (black~get-blackboard-object-content 'demands sod*control-blackboard)
								  exmes)
		 
		 ;; The deleted demands are annotated to the last step (which therewith should have a termination just)
		 (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			   'fulfilled-demands
			   del)
		 
		 (when sod*verbose
		   (omega~message "**SOD:: INTERPRETATION OF REACTION**: The following demands are fulfilled and thus removed: ~A" del))))))

  (:method ((reaction reac+interrupt-reaction))

	   ;; Execution of a strategy-ks is interrupted!
	   ;; 1. store demands in demands store
	   ;; 2. store demands in state-description (-> i.e. state-description depends on these demands)
	   ;; 3. store state-description in store (-> i.e. can be reinvoked)
	   ;; 4. new strategy interruption step
	   ;; 5. demands are also annotated to this strategy interruption step.
	   ;; Note: ROCS entry stay in TASKS!!!!!!!

	   
	   (let* ((demands (reac~reaction-demands reaction))
		  (exmes (reac~reaction-execution-message reaction))
		  (state-des (exmes~state-description exmes)))

	     (when (null state-des)
	       (error "~% Handling of start message non awaited ... in function reac=interpret-reaction"))

	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF REACTION**: NON TERMINATE EXECUTION of strategy-ks ~A on task ~A."
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF REACTION**: NEW DEMANDS ~A are stored, the state description is stored and not deleted from tasks and stays active."
			      demands))
	     
	     
	     ;; 1. add demands to demands-store
	     (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				  demands)
	     
	     ;; 2. store demands in state-description
	     (setf (roc~demands state-des) demands)
	     
	     ;; 3. store state-description in store
	     (store~add-element! (black~get-blackboard-object-content 'store sod*solution-blackboard) state-des)
	     
	     ;; 4. New strategy interruption step
	     (let* (;; New interrupt-strategy-ks justification
		    (new-interrupt-strategy-ks-application-just
		     (strat~interrupt-strategy-ks-application-create-just (list state-des (roc~copy-state-description state-des))))
		    ;; New strategy-step from this justification
		    (interrupt-step (scon~create-scon-step nil nil new-interrupt-strategy-ks-application-just)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! interrupt-step))
	     
	     ;; 5. The posed demands are annotated to the last step (which should have an interruption just)
	     (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
		       'posed-demands
		       demands))))

