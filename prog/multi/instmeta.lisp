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

;; This module contains the stuff for IinstMeta!


;; In general: The meta variables are not replaced hardcore, but a new contraint pool is created, that contains the substitution in the bindings.

#| ------------------------------------------------------ Global Variables ----------------------------------------------------------- |#

(defvar instmeta*verbose nil)
;; if set the system talks to you!

(defvar instmeta*roc-state-description nil)
;; The current state-description (always maintained)

#| ------------------------------------------- InstMeta AS REFINEMENT ALGORITHM ----------------------------------------------------- |#

;; The Invokation Function:

(defun instmeta~instmeta-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using InstMeta as refinement algorithm) and the task.")
	   (effect  "InstMeta is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; new InstMeta ROC-State-Description 
	 (new-instmeta-roc-state-description (roc~fresh-instmeta-state-description strategy-ks task nil parameters))
	 ;; New start-strategy-ks justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-instmeta-roc-state-description)))
	 ;; New strategy-step form this justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Roc get start-step as entry
    (setf (roc~start-step new-instmeta-roc-state-description) 
	  start-step)

    ;; Roc gets last-plan-step as entry
    (setf (roc~instmeta-last-plan-step new-instmeta-roc-state-description)
	  (pds~last-plan-step omega*current-proof-plan))

    ;; Roc gets last-cpool as entry
    (setf (roc~instmeta-last-cpool new-instmeta-roc-state-description)
	  (pds~constraint-pool omega*current-proof-plan))
    
    ;; ROC is entried in ROCS-list
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-instmeta-roc-state-description)
    
    ;; Strategy-step is inserted in strategy-steps
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when instmeta*verbose
      (omega~message "~%Invoking InstMeta WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (instmeta=calling-instmeta new-instmeta-roc-state-description)))

;; The Reinvokation Function!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Since an interruption of InstMeta is not designated so far, a reinvokation function is omitted!                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instmeta=calling-instmeta (roc)
  (declare (edited  "18-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for InstMeta.")
	   (effect  "Call InstMeta on it (ahwt ever this may cause) ...")
	   (value   "Messages returned from InstMeta."))
  
  ;; 1. Set the current ROC to the input ROC!
  (setf instmeta*roc-state-description roc)

  ;; 2. Call the instantiating itself, in the sense of creating a new constraint pool with extended binding!
  (let* ((message (instmeta~instmeta)))

    (when instmeta*verbose
      (omega~message "~%Receiving the following message from termination of InstMeta: ~A" message))
    
    ;; 3. Process the returned message: -> now happens in suggestion-reasoner!
    ;;(instmeta=interpret-execution-message! message)
    
    message
    ))

;;
;; The functionality of instmeta=interpret-execution-message! now is in
;; MODULE suggestion-reasoner
;;
;;(defgeneric instmeta=interpret-execution-message! (message)
;;  (declare (edited  "18-JUN-1999")
;;	   (authors Ameier)
;;	   (input   "An execution-message of proper terminating InstMeta.")
;;	   (effect  "See below.")
;;	   (value   "Undefined."))
;; (:method ((message exmes+termination-message))
;;	   
;;	   ;; The execution of InstMeta terminates proper
;;	   
;;	   (when instmeta*verbose		 
;;	     (omega~message "~%InstMeta SENDS MESSAGE OF PROPER TERMINATION."))
;;
;;	   (let* (;; 1. An end-strategy-application-justification is created!
;;		  (new-justification (strat~end-strategy-ks-application-create-just (list instmeta*roc-state-description)))
;;		  ;; 2. Therefore a new scon step is created:
;;		  (strategy-end-step (scon~create-scon-step nil nil new-justification)))
;;
;;	     ;; 3. Strategy-step is inserted in strategy-steps
;;	     (sod~introduce-new-strategy-ks-step! strategy-end-step)
;;
;;	     ;; 4. active flag in instmeta*roc-state-description is set to nil
;;	     (setf (roc~activ instmeta*roc-state-description) nil)
;;	     
;;	     )))
;;

#| ----------------------------------------------The insertion in the PDS itself -------------------------------------------------------- |#

;;
;; 1.) The term to instantiate for the inst task is computed by the function compute-instantiation-function of the strategy-ks.
;; 2.) A new constraint pool is created, that contains the pair meta-var -> term in the bindings and gets the current constraint pool of the PDS
;;     as predecessor. Further a list with the start-reason of the instmeta strategy is entried as plan-steps.
;; 3.) The binding in the new constraint pool is updated to global
;; 4.) The new constraint pool is set current to the PDS.
;; 5.) All schematic tasks and all schematic supports of goal tasks are updated to that effect, that they are no longer uptodate
;;     (because of the new binding!).
;; 6.) Create a new agenda by removing of this task and set the current pds-agenda to this new agenda!
;; 7.) Create a proper termination message!
;; Therewith the instantiation is not directly entried in the lines (in the sense of replacing the meta-variable!), but it is represented
;; in the current constraint pool!
;;
;;

(defun instmeta~instmeta ()
  (let* ((strategy-ks (roc~strategy-ks instmeta*roc-state-description))
	 (strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 (compute-instantiation-function (gethash 'compute-instantiation-function strat-parameter-hashtable))
	 (inst-task (roc~start-task instmeta*roc-state-description))
	 (meta-var (agenda~inst-task-meta-var inst-task))
	 (term-for-meta-var (apply compute-instantiation-function (list meta-var)))
	 (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (current-constraint-pool (pds~constraint-pool pds))
	 (new-constraint-pool (pds~cstrpool-create (subst~create (list meta-var) (list term-for-meta-var))
						   nil
						   current-constraint-pool
						   (list (roc~start-step instmeta*roc-state-description)))))

    (when instmeta*verbose
      (omega~message "*** INST-META: Computed the term ~A for meta-variable ~A" term-for-meta-var meta-var))
    
    ;; Used function from PPlanner, perhaps it should be rearranged!
    (pplan=insert-new-constraint-store! pds
					new-constraint-pool)
    
    (pds~update-schematic-nodes! pds)

    (setf (pds~agenda pds)
	  (agenda~replace-task inst-task nil nil nil (pds~agenda pds)))

    (exmes~create-termination-message instmeta*roc-state-description
				      nil)))
