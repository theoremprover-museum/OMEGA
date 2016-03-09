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

;; This module contains the stuff for the ATP algorithm!


#| ------------------------------------------------------ Global Variables ----------------------------------------------------------- |#

(defvar atpa*verbose nil)
;; if set the system talks to you!

(defvar atpa*roc-state-description nil)
;; The current state-description (always maintained)

#| ------------------------------------------- ATP AS REFINEMENT ALGORITHM ----------------------------------------------------- |#

;; The Invokation Function:

(defun atpa~atpa-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using ATP as refinement algorithm) and the task.")
	   (effect  "ATP is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; new atpa ROC-State-Description 
	 (new-atp-roc-state-description (roc~fresh-atp-state-description strategy-ks task nil parameters))
	 ;; New start-strategy-ks justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-atp-roc-state-description)))
	 ;; New strategy-step form this justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Roc get start-step as entry
    (setf (roc~start-step new-atp-roc-state-description) 
	  start-step)

    ;; ROC is added to ROCS-list
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-atp-roc-state-description)
    
    ;; Strategy-step is inserted in strategy-steps
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when atpa*verbose
      (omega~message "~%Invoking ATP WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (atpa=calling-atpa new-atp-roc-state-description)))

;; The Reinvokation Function!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Since an interruption of ATP is not designated so far, a reinvokation function is omitted!                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atpa=calling-atpa (roc)
  (declare (edited  "18-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for ATP.")
	   (effect  "Call ATP on it (ahwt ever this may cause) ...")
	   (value   "Messages returned from ATP."))
  
  ;; 1. Set the current ROC to the input ROC!
  (setf atpa*roc-state-description roc)

  ;; 2. Call the instantiating itself, in the sense of creating a new constraint pool with extended binding!
  (let* ((message (atpa~atpa)))
    
    (when atpa*verbose
      (omega~message "~%Receiving the following message from termination of ATP: ~A" message))
    
    message
    ))
#| ---------------------------------------------- The application of the ATP -------------------------------------------------------- |#

(defun atpa~atpa ()
  (let* ((task (roc~start-task atpa*roc-state-description))
	 (strategy-ks (roc~strategy-ks atpa*roc-state-description))
	 (strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 (apply-atp-function (gethash 'apply-atp-function strat-parameter-hashtable))
	 (check-atp-out (gethash 'check-atp-out strat-parameter-hashtable))
	 (atp-task (roc~start-task atpa*roc-state-description))
	 (atp-output (apply apply-atp-function (list atp-task)))
	 (success (apply check-atp-out (list atp-output))))

    (setf (roc~atp-atp-out atpa*roc-state-description)
	  atp-output)
    
    (when atpa*verbose
      (omega~message "*** ATP: Computed the output ~A" atp-output)
      (omega~message "*** ATP: Check says ~A for output success" success))
    
    (if success
	;; application of ATP was successful
	(let* ((task-node (agenda~task-node task))
	       (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	       (agenda (pds~agenda pds))
	       (new-agenda (agenda~replace-task task nil nil nil agenda))
	       (atp-tactic (infer~find-method 'atp)))
	  
	  ;; set new agenda
	  (setf (pds~agenda pds) new-agenda)
	  
;;	  ;; close task-node by ...
;;	  (if (res~proof-p atp-output)
;;	      ;; there is a resolution proof from the atp output
;;	      ;; close node by atp just + status should be unexpanded + resolution-proof should be second argument in parameters
;;	      (progn
;;		(infer~compute-outline atp-tactic
;;				       (cons task-node (pds~node-supports task-node))
;;				       (list t atp-output))
;;		(setf (pdsj~status (node~justification task-node)) "unexpanded"))
;;	    ;; there is no resolution proof as successful atp output
;;	    ;; close node by atp just + status should be untested (this is automatically the case) + no second parameter
;;	    (infer~compute-outline atp-tactic
;;				   (cons task-node (pds~node-supports task-node))
;;				   (list t)))

	  (infer~compute-outline atp-tactic
				 (cons task-node (pds~node-supports task-node))
				 (list atp-output))
	  
	  ;; create termination-message
	  (exmes~create-termination-message atpa*roc-state-description
					    nil)
	  )
      ;; atp fails
      (exmes~create-termination-message atpa*roc-state-description
					nil))))


