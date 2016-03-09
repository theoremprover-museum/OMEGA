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

(mod~defmod BACK 
            :uses (agenda black exmes infer just keim omega pds pdsc pdsj refalg roc scon sod store strat)
            :documentation "The stuff for backtracking steps"
            :exports (
                      
                      back~backtrack-invokation-function
                      back~backtrack-reinvokation-function
                      back~track
                      
                      back*roc-state-description
                      back*verbose))

;;; The following functions are internal in other modules and should not be used:
;;; (pds=reason-nodes&justs pds=remove-plan-step! pdsc=actual-node)

#| ------------------------------------------------------ Global Variables ----------------------------------------------------------- |#

(defvar back*verbose nil)
;; if set, the system outputs some comments

(defvar back*roc-state-description nil)
;; the current state-description (always maintained)

#| ------------------------------------------- BackTrack as REFINEMENT ALGORITHM ----------------------------------------------------- |#

;; The Invokation Function:

(defun back~backtrack-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using BackTrack as refinement algorithm) and the task.")
	   (effect  "BackTrack is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; new backtrack ROC-state-description 
	 (new-backtrack-roc-state-description (roc~fresh-backtrack-state-description strategy-ks task nil parameters))
	 ;; new start-strategy-ks justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-backtrack-roc-state-description)))
	 ;; new strategy-step from this justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Count the backtrack tseps
    (setf sod*backtrack-counter (+ 1 sod*backtrack-counter))
    
    ;; Roc gets start-step as entry
    (setf (roc~start-step new-backtrack-roc-state-description) 
	  start-step)
    
    ;; Strategy-step is inserted in strategy-steps
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when back*verbose
      (omega~message "*** Back:: Start BackTrack ***: Invoking BackTrack WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (back=calling-backtrack new-backtrack-roc-state-description)))

;; The reinvokation function
;; Not used by backtrack!
(defun back~backtrack-reinvokation-function (roc)
  nil)

(defun back=calling-backtrack (roc)
  (declare (edited  "26-MAR-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for BackTrack.")
	   (effect  "Calls BackTrack on it (what ever this may cause) ...")
	   (value   "Messages returned from BackTrack."))
  
  ;; set the current ROC state-description 
  (setf back*roc-state-description roc)
  
  ;; call backtrack
  
  (when back*verbose
    (omega~message "*** Back:: Start ***: CALLING BackTrack!"))
  
  (let* ((execution-message (back~track)))
    
    (when back*verbose
      (omega~message "*** Back:: End *** Receiving the following message from termination of BackTrack: ~A" execution-message))
    
    ;; Process the returned messages: -> now happens in suggestion-reasoner!
    ;; (back=interpret-execution-message! execution-message)
    
    execution-message
    ))

;;
;; The functionality of back=interpret-execution-message! is now found in
;; MODUL suggestion-reasoner
;;
;;(defgeneric back=interpret-execution-message! (message)
;;  (declare (edited  "07-JUN-1999")
;;	   (authors Ameier)
;;	   (input   "An execution  message.")
;;	   (effect  "Some ... depending on the kind of message ... (see below).")
;;	   (value   "Undefined."))
;;  (:method ((message exmes+termination-message))
;;	   
;;	   (when pplan*verbose		 
;;	     (omega~message "~%BackTrack SENDS MESSAGE OF PROPER TERMINATION."))
;;	   	   
;;	   (let* (;; 1. A new end-strategy-application-justification is created!
;;		  (new-justification (strat~end-strategy-ks-application-create-just (list back*roc-state-description)))
;;		  ;; 2. A new scon step is created:
;;		  (strategy-end-step (scon~create-scon-step nil nil new-justification)))
;;	     
;;	     ;; 3. Strategy-step is inserted in strategy-steps
;;	     (sod~introduce-new-strategy-ks-step! strategy-end-step)
;;	     
;;	     ;; 4. activ flag in back*roc-state-description is set to nil
;;	     (setf (roc~activ back*roc-state-description) nil)
;;
;;	     ;; 5. ROCS is removed from TASKS
;;	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
;;		    (all-tasks (agenda~all-tasks agenda)))
;;	       (mapcar #'(lambda (task)
;;			   (let* ((rocs (agenda~task-rocs task)))
;;			     (setf (agenda~task-rocs task) (remove back*roc-state-description rocs))))
;;		       all-tasks))
;;	     
;;	     )))
;;

#| -------------------------------------------------- backtracker itself ---------------------------------------------------- |#

;; Here begins the backtracker itself!
;; It works the following way:
;; 1.) The strategy-ks is determined from the current Roc.
;; 2.) This has an entry: compute-backtrack-steps-function
;; 3.) The set of all steps to backtrack is computed with this function.
;;     This set consist of two parts: in the first part the control things are adapted, in the second they are not.
;; 4.) Then these steps are retracted!
;;
;; (This approach is described more exactly in READMES/BACK-TRACK

(defun back~track ()
  (declare (edited  "24-JUN-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "Backtracks the steps computed by the strategy-ks of the current back*roc-state-description.")
	   (value   "An execution message."))

  
  ;;(format t "~%~%~%THE PDS BEFORE BACKTRACKING:")
  ;;(oc=show-pds)
  ;;(format t "~%~%~%The ROCS OF LIST BEFORE BACKTRACKING:")
  ;;(mapcar #'(lambda (roc)
  ;;	       (format t "~% ~A" roc))
  ;;	  (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
			  
    
  (let* ((beginning-constraint-pool (pds~constraint-pool omega*current-proof-plan))
	 (strategy-ks (roc~strategy-ks back*roc-state-description))
	 (task (roc~start-task back*roc-state-description))
	 (compute-backtrack-steps-function (gethash 'compute-backtrack-steps-function (strat~strategy-ks-hash-table strategy-ks))))
    
    (multiple-value-bind
	(steps-to-remove-with-control-change steps-to-remove-without-control-change) 
	(apply compute-backtrack-steps-function (list task))
      
      
      (when back*verbose
	(omega~message "*** BackTrack ***: Computed the following steps to remove: (~A, ~A)"
		       steps-to-remove-with-control-change
		       steps-to-remove-without-control-change))
      
      (let* ((undone-steps (append (apply 'append
					  (mapcar #'(lambda (step)
						      (back=remove-step! step 't))
						   steps-to-remove-with-control-change))
				   (apply 'append
					  (mapcar #'(lambda (step)
						      (back=remove-step! step nil))
						  steps-to-remove-without-control-change)))))
	
	(when back*verbose
	  (omega~message "*** BackTrack ***: At all we had removed the steps: ~A" undone-steps))

	;; remove steps from rocs
	(setf (roc~backtrack-removed-steps back*roc-state-description)
	      undone-steps)

	;; Termination Execution Message
	(exmes~create-termination-message back*roc-state-description
					  nil)
	
	))))


;; The following has to be done at every step, the will be retracted:
;; 1. retract step (if it exists at all!) -> This creates a whole set M1 of retracted steps
;; 2. Adapt the constraint pool
;;    This leads to the necessity to retract another set M2 of steps,
;;    Given the following situation:
;;
;;     S1   S2   S3   S4   S5   S6   S7   S8   S9   S10
;;     |         |              |                   |  
;;     |         |              |                   | 
;;     V         V              V                   V
;;     C1        C2             C3                  C4
;;
;;    If S6 is retracted, S7,S8,S9,S10 have also to be retraced, because they all depend from the constraint store C3.
;;    C2 becomes the new actual constraint store. The control of the nodes concerning S7,S8,S9,S10 must not be adapted,
;;    (the corresponding steps will not be prohibited), but in fact the control of S6 has to be adapted.
;; 3. Adapt the control of the concrete input step that has been retracted (the other recursivly retracted steps
;;    already should have made the control adaption recursivly).
;; 4. The agenda has to be adapted by adding the new tasks respectively removing the deleted tasks.
;;
;;
;; In the following it is described for each kind of step how it can be retracted!

(defgeneric back=remove-step! (step control-flag)
  (declare (edited  "24-JUN-1999")
	   (authors Ameier)
	   (input   "A step which has to be removed and a flag, signing wether the corresponding control"
		    "information should be changed.")
	   (effect  "Removes the step and all other steps affected therefrom.")
	   (value   "A list of all steps which have been undone."))
  (:method ((step scon+strategy-step) control-flag)

	   ;; Strategy-KS Steps:
	   
	   (let* ((undone-steps (cond ((back=start-step-p step)
				       (back=remove-start-strategy-ks-step step control-flag))
				      ((back=end-step-p step)
				       (back=remove-end-strategy-ks-step step control-flag ))
				      ((back=interrupt-step-p step)
				       (back=remove-interrupt-strategy-ks-step step control-flag))
				      ((back=reinvoke-step-p step)
 				       (back=remove-reinvoke-strategy-ks-step step control-flag))
				      (t
				       (error "~%Something Wrong in function back=remove-step!: Unknown Strategy-ks-step to remove.")))))
	     
	     undone-steps))
  (:method ((step keim::pdsc=actual-node) control-flag)
	   
	   ;; Plan-Step Steps:
	   
	   (let* ((just (pdsc~an-just step))
		  (just-method (just~method just))
		  (undone-steps (cond ((infer~method-p just-method)
				       (back=remove-method-application-step step control-flag))
				      (t
				       (error "~%Something Wrong in function back=remove-step!: Unknown Plan-step to remove.")))))
	     
	     undone-steps)))

#| ---------------------------------------------------- Rectraction of Method Steps ---------------------------------------------- |#

(defun back=remove-method-application-step (step control-flag)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A plan-step which should be backtracked and a control-flag signing"
		    "whether the corresponding control information should be updated.")
	   (effect  "Backtracks the step and all steps influenced therefrom.")
	   (value   "A list of all steps which were removed at last..."))

  (if (null (find step (pds~plan-steps (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
      ;; -> Step is no longer actual
      ;; -> Forget it + Return nil, because no further step has to be deleted
      nil

    ;; Step still is actual
    (let* ((just (pdsc~an-just step))
	   (node (pdsc~an-node step))
	   (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	   (cstr-pools (back=constraint-pools pds)))
      
      ;; Prior to deleting the step itself, the following steps have to be deleted:
      ;; 1.) All justifications below this justification have to be removed
      ;;     (By the expansion of this justification node became open again and afterwards close again etc. -> below-justs)
      ;;     Thereby we obtain a set of steps, which were annotated to the below-justs and will be invalid as soon as
      ;;     node gets open. (Further we also obtain a set of steps, which are valid as well
      ;;     and therefore remain undeleted!)
      ;; 2.) All steps/reasons of this justification, which are no longer valid, if node gets open
      ;;     (Further we also obtain here a set of steps/reasons, which stay valid and therefore remain undeleted!)
      ;; 3.) All twin reasons of this step, which were created at the same application as the step itself.
      ;; 4.) If this step changes the constraint store: all following steps, that depended on this CS!

      ;; to 1.) All inconsistent steps on the level below
      (multiple-value-bind
	  (rest-steps1 steps-to-undo1)
	  (back=inconsistent-steps-below pds node just)

	;; to 2.) All inconsistent steps on this level
	(multiple-value-bind
	    (rest-steps2 steps-to-undo2)
	    (back=inconsistent-steps-of-just pds node just step)

	  (let* (;; to 3.) all twin steps to delete
		 (steps-to-undo3 (back=get-twin-steps pds node just step))
		 (cstr-pool-with-step (find step cstr-pools
					    :test #'(lambda (steppi cossi)
						      (find steppi (pds~cstrpool-plansteps cossi)))))
		 ;; to 4.) all steps to delete because of the constraint pool
		 (steps-to-undo4 (if cstr-pool-with-step
				     (back=all-plan-steps-after step)
				   nil))
		 (steps-to-undo (union steps-to-undo1 (union steps-to-undo2 (union steps-to-undo3 steps-to-undo4))))
		 (rest-steps (union rest-steps2 (set-difference rest-steps1 steps-to-undo))))
	    
	    (multiple-value-bind
		(steps-to-undo-before-step steps-to-undo-after-step)
		(back=before-and-after-steps step steps-to-undo)

	      (when back*verbose
		(omega~message "*** BackTrack ***: Removing the method-application step ~A" step)
		(omega~message "*** BackTrack ***: Removing first ~A" steps-to-undo-after-step)
		(omega~message "*** BackTrack ***: Removing later ~A" steps-to-undo-before-step))
	      
	      ;; Little Note: The names steps-to-undo-before-step steps-to-undo-after-step only refer to the
	      ;;              order of the steps in plan-steps and not to the order in which they have to be deleted.
	      ;;              In fact the after-steps have to be deleted at first, then the step itself and then the before-steps!
	      ;;              The twin reasons should be processed thereafter, because there could be loops otherwise.
	      ;; Little question: Can there be before-steps at all? Good question !!!!

	      (let* ((nodes-below-node (remove node (pdsn~justifying-nodes (list node)))))

		(append
		 ;; At first delete the newer steps (which are for example all belows)
		 (apply 'append 
			(mapcar #'(lambda (after-step)
				    (back=remove-step! after-step nil))
				steps-to-undo-after-step))
		 
		 ;; then delete the step itself
		 (back=undo-step! pds node just step rest-steps control-flag)
		 
		 ;; then delete the older steps + twin steps
		 (apply 'append 
			(mapcar #'(lambda (before-step)
				    (back=remove-step! before-step nil))
				steps-to-undo-before-step))
		 
		 ;; in the end delete the steps, that are found under node in the tree!
		 (back=undo-steps-below-node-on-branch! node nodes-below-node)))
	      
	      )))))))

(defun back=fringe-justifying-nodes (node)
  (declare (edited  "1-OCT-2000")
	   (authors AMeier)
	   (input  "A plan node.")
	   (effect "None." )
	   (value  "First alls nodes which justify this node, as well as the nodes which"
		   "justify them, ad infinitum, recursively are computed.  That is, all"
		   "nodes upon which the input node depend. Then all node which are themselves"
		   "justified by other nodes are removed, so only the fringe nodes remain." ))
  (let* ((all-justifying-nodes (pdsn~justifying-nodes (list node))))
    (remove-if-not #'(lambda (node)
		       (null (pdsn~just-ass&premises node)))
		   (remove node all-justifying-nodes))))

(defun back=undo-steps-below-node-on-branch! (node nodes-below-node-before)
  (declare (edited  "24-AUG-2000")
	   (authors Ameier)
	   (input   "A node and a list of nodes which were before backtracking on the branch of node"
		    "justifiying node.")
	   (effect  "The list of lines which is now after backtrikcung below node is computed."
		    "The difference between the input nodes and these nodes is computed."
		    "All own reasons of nodes of this difference set are computed, then all the"
		    "own reasons are removed.")
	   (value   "The list of removed steps."))
  (let* ((nodes-below-node-now (if (find node (prob~proof-steps omega*current-proof-plan))
				   (pdsn~justifying-nodes (list node))
				 nil))
	 (all-justiying-nodes (pdsn~justifying-nodes (list (prob~proof-root omega*current-proof-plan))))
	 (diff1 (if nodes-below-node-now
		   (set-difference nodes-below-node-before nodes-below-node-now)
		 nil))
	 (diff2-pre (remove-if #'(lambda (diff1-node)
				   (find diff1-node all-justiying-nodes))
			       diff1))



	 ;; what about nodes, which are created in a forward reasoning process and which
	 ;; depend just only from hypotheses or from nodes that have no premises themselves?
	 (forward-derived-nodes-below (remove-if-not #'(lambda (node)
							 (let* ((fringe-nodes (back=fringe-justifying-nodes node)))
							   (or (pdsn~hypothesis-node-p node)                     ;; node itself depends on nothing else
							       (null (just~premises (node~justification node)))  ;; node itself depends on nothing else
							       (and fringe-nodes
								    ;; every fringe node is a hypothesis or has a justification without premises
								    (every #'(lambda (node)
									       (or (pdsn~hypothesis-node-p node)
										   (and (null (pdsj~open-p (node~justification node)))
											(null (just~premises (node~justification node))))))
									   fringe-nodes)))))
						     diff2-pre))
	 ;; forward derived rules are not deleted, if one of their successors (i.e., another forwardly derived node in the list) is in the
	 ;; supports of a current task.
	 (all-current-supports (remove-duplicates (apply #'append (mapcar #'pds~node-supports
									  (mapcar #'agenda~task-node
										  (remove-if-not #'agenda~goal-or-goal-schema-task-p
												 (agenda~all-tasks (pds~agenda omega*current-proof-plan))))))))
	 (forward-derived-nodes-below-in-supports (intersection forward-derived-nodes-below all-current-supports))
	 (forward-derived-nodes-below-to-keep (remove-duplicates (apply #'append (mapcar #'(lambda (node)
											     (cons node (pdsn~justifying-nodes (list node))))
											 forward-derived-nodes-below-in-supports))))
	 (forward-derived-nodes-below-to-delete (remove-if #'(lambda (node)
							       (find node forward-derived-nodes-below-to-keep))
							   forward-derived-nodes-below))
	 (diff2 (remove-if #'(lambda (node)
			       (find node forward-derived-nodes-below-to-keep))
			   diff2-pre))	 
	 (own-reasons (apply #'append (mapcar #'(lambda (line)
						  (let* ((just (node~justification line))
							 (own-reason (pdsj~own-reason just)))
						    (if own-reason
							(list own-reason)
						      nil)))
					      diff2))))

    ;; only remaining problem, what about steps with twin-steps somewhere else?
    
    (format t "~%DIFF2-pre: ~A" diff2-pre)
    (format t "~%forward-derived-nodes-below: ~A" forward-derived-nodes-below)
    (format t "~%forward-derived-nodes-below-in-supports: ~A" forward-derived-nodes-below-in-supports)
    (format t "~%forward-derived-nodes-below-to-delete: ~A" forward-derived-nodes-below-to-delete)
    (format t "~%DIFF2: ~A" diff2)
    
    (when back*verbose
      (omega~message "~%** CHECKING BRANCH **: When opening ~A the we have to remove the following steps below: ~A" node own-reasons)) 
    
    (apply 'append 
	   (mapcar #'(lambda (step)
		       (back=remove-step! step nil))
		   own-reasons))))



#|
(defun back=undo-steps-below-node-on-branch! (node nodes-below-node-before)
  (declare (edited  "24-AUG-2000")
	   (authors Ameier)
	   (input   "A node and a list of nodes which were before backtracking on the branch of node"
		    "justifiying node.")
	   (effect  "The list of lines which is now after backtrikcung below node is computed."
		    "The difference between the input nodes and these nodes is computed."
		    "All own reasons of nodes of this difference set are computed, then all the"
		    "own reasons are removed.")
	   (value   "The list of removed steps."))
  (let* ((nodes-below-node-now (if (find node (prob~proof-steps omega*current-proof-plan))
				   (pdsn~justifying-nodes (list node))
				 nil))
	 (all-justiying-nodes (pdsn~justifying-nodes (list (prob~proof-root omega*current-proof-plan))))
	 (diff1 (if nodes-below-node-now
		   (set-difference nodes-below-node-before nodes-below-node-now)
		 nil))
	 (diff2-pre (remove-if #'(lambda (diff1-node)
				   (find diff1-node all-justiying-nodes))
			       diff1))

	 ;; Nodes which are created in a forward reasoning process and which depand thus only from
	 ;; hypothesis are note removed!
	 ;; By the way: It is not enough to remove the steps to only open nodes because
	 ;; a) This would then create again new open nodes ...
	 ;; b) what about steps that changed the constraint pool?
	 
	 (diff2 (remove-if #'(lambda (node)
			       (let* ((fringe-nodes (back=fringe-justifying-nodes node)))
				 (or (pdsn~hypothesis-node-p node)
				     (and fringe-nodes
					  ;; every fringe node is a hypothesis or has a justification without premises
					  (every #'(lambda (node)
						     (or (pdsn~hypothesis-node-p node)
							 (and (null (pdsj~open-p (node~justification node)))
							      (null (just~premises (node~justification node))))))
						 fringe-nodes)))))
			   diff2-pre))
	 
	 ;; if the node is used somewhere else -> its own reason is not deleted
	 (own-reasons (apply #'append (mapcar #'(lambda (line)
						  (let* ((just (node~justification line))
							 (own-reason (pdsj~own-reason just)))
						    (if own-reason
							(list own-reason)
						      nil)))
					      diff2))))

    ;;(format t "~%~%DIFF2-pre: ~A" diff2-pre)
    ;;(format t "~%DIFF2: ~A" diff2)
    
    (when back*verbose
      (omega~message "~%** CHECKING BRANCH **: When opening ~A the we have to remove the following steps below: ~A" node own-reasons)) 
    
    (apply 'append 
	   (mapcar #'(lambda (step)
		       (back=remove-step! step nil))
		   own-reasons))))
|#


(defun back=undo-step! (pds node just reason rest-steps control-flag)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "The pds, one of its nodes, a justification of the node and the own reason of the"
		    "justification. Furthermore a list of other reasons (from below and this justification)"
		    "which stay consistent, if the node gets open. And last but not least a flag to sign"
		    "wheter on the backtracked node a control information should be assigned.")
	   (effect  "Undo the step.")
	   (value   "A list with all steps which were removed."))
  
  (when (pdsj~below-reasons just)
    ;;To prevent this error message, you must remove the remaining below reasons
    ;;while undoing below justifications of JUST.
    (omega~message "Problem in Function back=undo-step!!: ~A may not have below reasons." just))

  ;; Added to handle backtracking of steps from methods which call tactics
  ;; The current justification contains already an below just - the applied tactic.
  ;; To ignore these tactic application - which has no own reason - we delete the below justification 
  (back=remove-all-below-just-without-reasons! just)
  
  (multiple-value-bind
      (nodes&justs new-hyps)
      (keim::pds=reason-nodes&justs node just reason)
    
    (let* (;; Delete reason from the reason list of the node; if then the reason list is empty, the node is deleted!
	   (deleted-nodes1 (back=remove-reason! pds node just reason))
	   ;; Delete reason from all nodes it is contained in!
	   (deleted-nodes2 (apply 'append (mapcar #'(lambda (n&j)
						      (when (and (null (find (first n&j) new-hyps))
								 ;; Had Problems, that some of the n&j had no justiications!
								 ;; This caused errors in the back=remove-reason! strategy
								 ;; Threfore, this additional conditions that both the node
								 ;; and the justification exist!
								 (first n&j)
								 (rest n&j))
							(back=remove-reason! pds (first n&j) (rest n&j) reason)))
						  nodes&justs))))
      
      (multiple-value-bind
	  (deleted-nodes3 further-steps-to-delete)
	  (back=delete-hyps! new-hyps reason)
	
	(let* ((deleted-nodes (append deleted-nodes1 deleted-nodes2 deleted-nodes3))
	       ;; Create new open justification (if necessary) + entry control information (if necessary)
	       (open-nodes (back=check-justification-of-node! pds node just reason rest-steps control-flag)))
	  
	  
	  (when back*verbose
	    (omega~message "*** BackTrack ***: While removing step ~A, the nodes ~A are removed and the nodes ~A are opened"
			   reason
			   deleted-nodes
			   open-nodes)
	    (omega~message "*** BackTrack ***: Furthermore the following steps have also to be removed: ~A"
			   further-steps-to-delete))
	  
	  ;; Removes step from the plan steps
	  (keim::pds=remove-plan-step! pds reason)
	  
	  ;; Update agenda
	  (back=update-agenda! reason deleted-nodes open-nodes)

	  ;; Backtrack in COSIE
	  (back=backtrack-cosie! reason)
	  
	  ;; Update ROCS
	  ;; Updating rocs has to be done after updating the agenda, since the updating of the ROCS needs an intact agenda.
	  ;; This updating of ROCS can result in deleting whole strategy applications, but thats OK,
	  ;; since all later steps must still have been deleted!
	  
	  (let* ((other-removed-steps1 (back=update-rocs! reason deleted-nodes open-nodes))
		 (other-removed-steps2 (mapcar #'(lambda (steppi)
						   (back=remove-step! steppi nil))
					       further-steps-to-delete))
		 ;; Remark: Since the further-steps-to-delete are after the current step for sure, they can just be removed!
		 (cstr-pools (back=constraint-pools pds))
		 (cstr-pool-with-step (find reason cstr-pools
					    :test #'(lambda (steppi cossi)
						      (find steppi (pds~cstrpool-plansteps cossi))))))
	    ;; Remark: Here we have to look again for the constraint pools (we can not take it from above),
	    ;; since the pools could be changed in between
	    
	    ;; Sets the current constraint-pool to the predecessor, if it exists
	    (when cstr-pool-with-step
	      (let* ((mvs-bound-here (back=mvs-added-by-cpool cstr-pool-with-step)))
		
		(back=insert-int-tasks-to-mvs-in-agenda! mvs-bound-here)
		(setf (pds~constraint-pool pds)
		      (pds~cstrpool-previous cstr-pool-with-step))
		(back=all-update-to-nil)))
	    
	    (cons reason (append other-removed-steps1 other-removed-steps2))))))))

(defun back=backtrack-cosie! (reason)
  (let* ((node (pdsc~an-node reason))
	 (just (pdsc~an-just reason))
	 (method (just~method just)))
    (if (eq method (infer~find-method 'tellcs-m))
	(progn
	  (when back*verbose
	    (omega~message "~%The node ~A is removed from COSIE" node))
	  (CoSIE~backtrack node))
      nil)))
	

(defun back=insert-int-tasks-to-mvs-in-agenda! (mvs)
  (declare (edited  "28-AUG-2000")
	   (authors Ameier)
	   (input   "A list of meta-variables.")
	   (effect  "The inst-tasks for the meta-variables are created and inserted into the agenda.")
	   (value   "Undefined."))
  (let* ((inst-tasks (apply #'append
			    (mapcar #'(lambda (mv)
					(let* ((result (find mv sod*inst-tasks :test #'(lambda (mev inst-task)
											 (eq mev (agenda~inst-task-meta-var inst-task))))))
					  (if result
					      (list result)
					    ;; IT CAN HAPPEN THAT A META-VARIABLE WAS CREATED WITHIN A METHOD AND
					    ;; BOUND WITHIN THE SAME METHOD -> THERE EXISTED NEVER A INST-TASK
					    ;; FOR IT!!!
					    nil)))
				    mvs)))
	 (agenda (pds~agenda omega*current-proof-plan))
	 (new-agenda (if inst-tasks
			 (if (agenda~empty-p agenda)
			     (agenda~create nil inst-tasks nil agenda)
			   (let ((next-tasks (agenda~next-tasks agenda)))
			     (setf (agenda~next-tasks agenda) (append inst-tasks next-tasks))
			     agenda))
		       agenda)))
    
    (when back*verbose
      (omega~message "~%The following inst-tasks are inserted into the agenda: ~A" inst-tasks))
    
    (setf (pds~agenda omega*current-proof-plan) new-agenda)))

(defun back=mvs-added-by-cpool (cpool)
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

(defun back=remove-all-below-just-without-reasons! (just)
  (when (and (pdsj~below just)
	     (null (pdsj~reasons (pdsj~below just))))
    ;; -> just has a below justification without reasons
    (if (pdsj~below (pdsj~below just))
	;; below just has another below just
	(progn (setf (pdsj~below just) (pdsj~below (pdsj~below just)))
	       (setf (pdsj~above (pdsj~below (pdsj~below just))) just)
	       (back=remove-all-below-just-without-reasons! just))
      ;; below just has no below just
      (setf (pdsj~below just) nil))))
    
  

(defun back=all-update-to-nil ()
  (declare (edited  "18-AUG-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "All update date slots in all schematic-nodes are set to nil. This is necessary since after"
		    "a backtracking instantiaons can be deleted.")
	   (value   "Undefined."))
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (all-nodes (prob~proof-steps pds))
	 (all-schematic-nodes (remove-if-not #'pdsn~schematic-p all-nodes)))

    (mapcar #'(lambda (schematic-node)
		(setf (pdsn~up-to-date schematic-node) nil))
	    all-schematic-nodes)))
 
(defun back=delete-hyps! (hyps step)
  (declare (edited  "30-JUN-1999")
	   (authors Ameier)
	   (input   "A list of hyps (= Nodes) and a step which is currently removed.")
	   (effect  "The hyps are removed from the pds.")
	   (value   "Multiple-Value:"
		    "First: A list of realy deleted nodes (if they are not already deleted)."
		    "Second: A list of additionally steps using the hyps which have also to be"
		    "        deleted, except the input step."))

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
    
    (do* ((rest-hyps hyps (rest rest-hyps))
	  (deleted-nodes nil)
	  (steps-to-delete nil))
	((null rest-hyps)
	 (values deleted-nodes
		 steps-to-delete))
      (let* ((head-hyp (first rest-hyps)))
	(when (find head-hyp (prob~proof-steps pds))
	  
	  (let* ((all-nodes-with-hyp (remove-if-not #'(lambda (node)
							(find head-hyp (pdsn~hyps node)))
						    (prob~proof-steps pds)))
		 (further-steps-to-delete (remove step
						  (remove-duplicates
						   (apply #'append (mapcar #'(lambda (node)
									       (let* ((just (node~justification node)))
										 (append (pdsj~all-own-reasons just)
											 (pdsj~all-other-reasons just))))
									   all-nodes-with-hyp))))))
	    
	    (setf steps-to-delete (remove-duplicates (append further-steps-to-delete
							     steps-to-delete))) 
	    
	    (setf deleted-nodes (cons head-hyp deleted-nodes))
	    (keim::pds=remove-node! pds head-hyp (just~premises (node~justification head-hyp)))))))))

	  
#|	  (let* ((just (node~justification head-hyp))
		 (further-steps-to-delete (remove step (remove-duplicates (append (pdsj~all-own-reasons just)
										  (pdsj~all-other-reasons just))))))
	    
	    (setq steps-to-delete (remove-duplicates (append further-steps-to-delete
							     steps-to-delete)))  
	    
	    (when (null further-steps-to-delete)
	      (setq deleted-nodes (cons head-hyp deleted-nodes))
	      (keim::pds=remove-node! pds head-hyp (just~premises (node~justification head-hyp))))))))))|#
  
  
(defun back=check-justification-of-node! (pds node just reason rest-steps control-flag)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, one of its nodes, one of the node's justifications, the own reason of"
		    "the justification, a list of other-reasons and a control-flag.")
	   (effect  "If there are still reasons (above-reasons or other-reasons), the node is not deleted."
		    "Then a new open justification is created and added on the node.")
	   (value   "If nodes stays in the pds, a list with node, otherwise nil."))
  (if (or rest-steps
	  (pdsj~above-reasons just))
      ;; node has still reasons, it was therefore not deleted, open its justification:  
      (let* ((old-ctrl (pdsj~control just))
	     (new-ctrl (pdsc~create rest-steps
				    (pdsc~sponsors old-ctrl)
				    (pdsc~unsponsors old-ctrl))) 
	     (new-just (pdsj~open-just-create new-ctrl))
	     (above-just (pdsj~above just)))
	
	;;Inherit alternative-mmatchings, alternative-methods, and failed-methods:
	(setf (keim::pdsc~excluded-methods new-ctrl) (keim::pdsc~excluded-methods old-ctrl))
	(setf (keim::pdsc~already-applied-mmatchings new-ctrl) (keim::pdsc~already-applied-mmatchings old-ctrl))
	(setf (keim::pdsc~already-applied-strategy-ks new-ctrl) (keim::pdsc~already-applied-strategy-ks old-ctrl))
	
	(when control-flag
	  ;; Here new control information is entried
	  (back=enter-new-control-info-of-step! reason new-ctrl)
	  )
	
	;;Inherit the above justifications:
	(when above-just
	  (setf (pdsj~above new-just) above-just)
	  (setf (pdsj~below above-just) new-just))

	;; Set new justification
	(setf (node~justification node) new-just)

	;;Insert NODE into the open nodes of PDS and remove it from the PDS support nodes
	(setf (pds~open-nodes pds)
	      (union (list node) (pds~open-nodes pds)))
	(setf (pds~support-nodes pds)
	      (remove node (pds~support-nodes pds)))
	
	(list node))
    
    (if (equal node (prob~proof-root pds))
	;;NODE has no more reasons but it could not be deleted, because it corresponds to
	;;the conclusion of PDS; Open its justification:
	(let* ((old-ctrl (pdsj~control (pdsn~most-abstract-just node)))
	       (new-ctrl (pdsc~create NIL (pdsc~sponsors old-ctrl) (pdsc~unsponsors old-ctrl)))
	       (new-just (pdsj~open-just-create new-ctrl)))
	  
	  ;;Inherit alternative-mmatchings, alternative-methods, and failed-methods:
	  (setf (keim::pdsc~excluded-methods new-ctrl) (keim::pdsc~excluded-methods old-ctrl))
	  (setf (keim::pdsc~already-applied-mmatchings new-ctrl) (keim::pdsc~already-applied-mmatchings old-ctrl))
	  (setf (keim::pdsc~already-applied-strategy-ks new-ctrl) (keim::pdsc~already-applied-strategy-ks old-ctrl))
	  
	  (when control-flag
	    ;; Here new control information is entried
	    (back=enter-new-control-info-of-step! reason new-ctrl)
	    )

	  ;; Set new justification
	  (setf (node~justification node) new-just)
	
	  ;;Insert NODE into the open nodes of PDS and remove it from the PDS support nodes
	  (setf (pds~open-nodes pds)
		(union (list node) (pds~open-nodes pds)))
	  (setf (pds~support-nodes pds)
		(remove node (pds~support-nodes pds)))

	  (list node))
      ;; node has to be deleted!
      ;; make just the additional entries
      (progn
	(back=enter-new-control-info-into-forward-goals! reason)
	nil))))

;; ATTENTION!!
;; THIS IS NOT CORRECT FOR SCHEMATIC NODES AT LARGE!
;; IT IS POSSIBLE, THAT THE WHOLE CONTROL STUFF HAS TO BE SET TO NIL AGAIN

(defun back=enter-new-control-info-of-step! (reason new-ctrl)
  (declare (edited  "29-JUN-1999")
	   (authors Ameier)
	   (input   "A step and a control unit.")
	   (effect  "Updates the control unit by adding the information of the step.")
	   (value   "Undefined."))
  (let* ((just (pdsc~an-just reason))
	 (mmapping (pdsj~subst just))
	 (parameters (pdsj~parameters just))
	 (method (just~method just))
	 (state-description (back=find-roc-of-step reason))
	 (new-mmatching (pplan=mmatching-create method
						parameters
						mmapping
						(keim~get reason 'cstr-state-before)
						state-description)))
    
    (when back*verbose
      (omega~message "*** BackTrack ***: Updating the control-Unit ~A by new method-mapping ~A" new-ctrl mmapping))
    
    (setf (keim::pdsc~already-applied-mmatchings new-ctrl)
	  (cons new-mmatching (keim::pdsc~already-applied-mmatchings new-ctrl)))

    (back=enter-new-control-info-into-forward-goals! reason)
    ))


(defun back=enter-new-control-info-into-forward-goals! (reason)
  (declare (edited  "29-JUN-1999")
	   (authors Ameier)
	   (input   "A step.")
	   (effect  "Updates the control units of associated goals of a forward step.")
	   (value   "Undefined."))

  (let* ((just (pdsc~an-just reason))
	 (mmapping (pdsj~subst just))
	 (parameters (pdsj~parameters just))
	 (method (just~method just))
	 (state-description (back=find-roc-of-step reason))
	 (new-mmatching (pplan=mmatching-create method
						parameters
						mmapping
						(keim~get reason 'cstr-state-before)
						state-description)))

    (when back*verbose
      (omega~message "*** BackTrack ***: For Forward Methods: Updating the control-Units of the related goals ~A."
		     (keim~get reason 'goal-list))
      
      (mapcar #'(lambda (node)
		  (let* ((just (node~justification node))
			 (control (pdsj~control just)))
		    (setf (keim::pdsc~already-applied-mmatchings control)
			  (cons new-mmatching (keim::pdsc~already-applied-mmatchings control)))))
	      (keim~get reason 'goal-list)))))

(defun back=find-roc-of-step (step)
  (declare (edited  "24-AUG-2000")
	   (authors Ameier)
	   (input   "A planning step.")
	   (effect  "None.")
	   (value   "Looks up the rocs-store for a pplanner roc that contains this step in its steps list."))
  (let ((rocs (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard))))
    (or (find step (remove-if-not #'roc~pplanner-state-description-p rocs)
	      :test #'(lambda (st roc) (find st (roc~pplanner-steps roc))))
	(find step (remove-if-not #'roc~analogy-state-description-p rocs)
	      :test #'(lambda (st roc) (find st (roc~pplanner-steps roc)))))))

  

(defun back=update-agenda! (step deleted-nodes open-nodes)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A step, a list of deleted-nodes, a list of new open-nodes.")
	   (effect  "Deletes all tasks from agenda which are goal-tasks to one of the"
		    "deleted-nodes and inst-tasks to the step. Adds to the Agenda a task"
		    "for each open-node. Sets the agenda of PDS to the arising agenda.")
	   (value   "undefined."))

  (when back*verbose
    (omega~message "~%OLD AGENDA HAS ~%FIRST-TASKS: ~A" (pds~first-tasks! (black~get-blackboard-object-content 'pds sod*solution-blackboard)
									  (pds~agenda
									   (black~get-blackboard-object-content 'pds sod*solution-blackboard)))))

  ;; For each newly open node the up-to-date flag has to be set to nil
  ;; since it can happen that during the last visti of the node the bindings changed
  (dolist (open-node open-nodes)
    (when (pdsn~schematic-p open-node)
      (setf (pdsn~up-to-date open-node) nil))
    (dolist (supp (pds~node-supports open-node omega*current-proof-plan))
      (when (pdsn~schematic-p supp)
	(setf (pdsn~up-to-date supp) nil))))
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (tasks-to-remove (remove-if-not #'(lambda (task)
					     (or (and (agenda~goal-or-goal-schema-task-p task)
						      (find (agenda~task-node task) deleted-nodes))
						 (and (agenda~inst-task-p task)
						      (eq (agenda~inst-task-plan-step task) step))))
					 all-tasks))
	 (new-tasks (mapcar #'(lambda (open-node)
				(agenda~create-open-task open-node))
			    open-nodes))
	 (new-agenda1 (agenda~update agenda (prob~proof-steps pds)))
	 (new-agenda2 (progn
			
			(when back*verbose
			  (omega~message "~%Intermediate AGENDA HAS ~%FIRST-TASKS: ~A" (if (agenda~empty-p new-agenda1)
											   nil
											 (pds~first-tasks! pds new-agenda1))))
			
			(if (null new-tasks)
			    new-agenda1
			  (if (or (agenda~empty-p new-agenda1) (agenda~first-task new-agenda1))
			      (agenda~create nil new-tasks nil new-agenda1)
			    (let ((next-tasks (agenda~next-tasks new-agenda1)))
			      (setf (agenda~next-tasks new-agenda1) (append new-tasks next-tasks))
			      
			      ;; HACK: No idea, what that should be exactly!
			      ;; But it destroys  something! (introduces orderings....)
			      ;; Therefore I removed it again!
			      
			      ;;(when next-tasks
			      ;;  (setf (agenda~orderings new-agenda1)
			      ;;	    (cons (agenda~ordering-create new-tasks next-tasks)
			      ;;		  (agenda~orderings new-agenda1))))
			      new-agenda1))))))
    
    (when back*verbose
      (omega~message "~%NEW AGENDA HAS ~%FIRST-TASKS: ~A" (pds~first-tasks! pds new-agenda2)))
    
    (setf (pds~agenda pds) new-agenda2)))

(defun back=update-rocs! (step deleted-nodes open-nodes)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A step (which is removed), a list of nodes which are deleted, a list with"
		    "new open nodes.")
	   (effect  "All rocs in the rocs-list are updated.")
	   (value   "A list of removed steps."))
  
  ;; Rocs are updated in the following way:
  ;; 1.) If a start task of an strategy application disappears (i.e. its goal-node is under the deleted-nodes, if it is a goal-task,
  ;;     or its step is the input step, if it is an instantiation task), then the strategy application is deleted completely
  ;;     (delete start justification).
  ;; 2.) Especially for PPlaner strategies:
  ;;     -> All deleted-nodes are removed from all new-lines and from all Rocs, in which they occur.
  ;;     -> The step is removed from all new-lines and from all Rocs, in which it occurs.
  ;;     -> If some of the deleted nodes are found in the outline of a PPlaner strategy, then they are removed; if there is an open-node,
  ;;        then this is added for it and the end step of the rocs is deleted!

  (apply 'append 
	 (mapcar #'(lambda (roc)
		     (let* ((start-task (roc~start-task roc))
			    (further-removed-steps1 (if (back=start-task-is-removed-p start-task step deleted-nodes)
							(progn
							  (when back*verbose
							    (omega~message "*** BackTrack ***: While removing step ~A the Strategy application with start-step ~A"
									   step
									   (roc~start-step roc))
							    (omega~message "*** BackTrack ***:has to be removed, since its start-task was removed."))
							  
							  (back=remove-step! (roc~start-step roc) nil))
						      nil))
			    (further-removed-steps2 (back=further-update-roc! roc step deleted-nodes open-nodes)))
		       (append further-removed-steps1 further-removed-steps2)))
		 (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))

(defgeneric back=further-update-roc! (roc step deleted-nodes open-nodes)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A roc, a step (which is deleted), a list of nodes (which are deleted) and"
		    "a list of new open-nodes.")
	   (effect  "Updates the Roc.")
	   (value   "A list of removed steps."))
  (:method ((roc roc+pplanner-state-description) step deleted-nodes open-nodes)
	   ;; For PPlanner ROCS:
	   ;; -> Update new-lines:     All deleted-nodes are removed from all new-lines and from all Rocs, in which they occur.
	   ;; -> Update steps:         The step is removed from all new-lines and from all Rocs, in which it occurs.
	   ;; -> Update Outlines:      It is checked whether there is an intersection between the premises of the step and the outline
	   ;;                          or the step belongs to the steps of the ROC
	   ;;                          In this case, all premises are deleted and the new open nodes are added.
	   ;; -> Update-strategysteps: If there is an end-step for this Roc, it is deleted! If there is none, the task for the new open node
	   ;;                          is retrieved from the agenda (-> update agenda before updating the ROCS) and this task
	   ;;		               gets that ROC as entry
	   
	   (let* ((end-step-to-roc (back=end-step-to-roc roc))
		  (agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		  (all-tasks (agenda~all-tasks agenda))
		  (tasks-to-open-nodes (apply 'append
					      (mapcar #'(lambda (open-node)
							  (let* ((task (find open-node all-tasks
									     :test #'(lambda (nodi taski)
										       (cond ((agenda~goal-or-goal-schema-task-p taski)
											      (eq nodi (agenda~task-node taski)))
											     ;; HAE???
											     ;;((agenda~inst-task-p taski)
											     ;; (eq nodi (agenda~task-node taski)))
											     (t
											      nil))))))
							    (if task
								(list task)
							      nil)))
						      open-nodes)))
		  (just (pdsc~an-just step))
		  (premises (just~premises just))
		  (step-in-roc-steps (find step (roc~pplanner-steps roc))))
	     
	     (setf (roc~pplanner-new-lines roc)
		   (set-difference (roc~pplanner-new-lines roc) deleted-nodes))

	     (when step-in-roc-steps
	       (setf (roc~pplanner-steps roc)
		     (remove step (roc~pplanner-steps roc))))
	       
	     ;;(format t "~%~%UPDATING THE OUTLINES OF ROC ~A" roc)
	     ;;(format t "~%~%OUTLINES: ~A" (roc~pplanner-outline-lines roc))
	     ;;(format t "~%STEP-PREMISES: ~A" premises)
	     ;;(format t "~%DELETED-NODES: ~A" deleted-nodes)
	     ;;(format t "~%OPEN-NODES: ~A" open-nodes)
	     
	     (if (or step-in-roc-steps
		     (intersection (roc~pplanner-outline-lines roc) premises))
		 (progn
		   (setf (roc~pplanner-outline-lines roc)
			 (append open-nodes (set-difference (roc~pplanner-outline-lines roc) (append premises deleted-nodes))))
		   (if end-step-to-roc
		       (progn
			 (when back*verbose
			   (omega~message "*** BAckTrack ***: While removing step ~A, the end-step ~A is removed"
					  step
					  end-step-to-roc))
			 ;; Remark: The retraction of the end-steps automatically leads thereto, the ROCS are correctly set in the TASKS
			 ;; -> it must not be done here explicitly
			 (back=remove-step! end-step-to-roc nil))
		     (progn
		       ;; there is no step, that is retracted -> explicitly adding the ROC in the new TASKS
		       (mapcar #'(lambda (task)
				   (setf (agenda~task-rocs task)
					 (cons roc (agenda~task-rocs task))))
			       tasks-to-open-nodes)
		       (when back*verbose
			 (omega~message "*** BackTrack ***: While removing step ~A, the new tasks ~A get as roc ~A"
					step
					tasks-to-open-nodes
					roc))
		       nil)))
	       (progn
		 (setf (roc~pplanner-outline-lines roc)
		       (set-difference (roc~pplanner-outline-lines roc) deleted-nodes))
		 ;;(when back*verbose
		 ;;  (omega~message "*** BackTrack ***: WARNING SOMETHING STRANGE IN FUNCTION back=further-update-roc!"))
		 nil))))
  (:method ((roc roc+instmeta-state-description) step deleted-nodes open-nodes)
	   nil)
  (:method ((roc roc+backtrack-state-description) step deleted-nodes open-nodes)
	   nil))


(defun back=end-step-to-roc (roc)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A roc state description.")
	   (effect  "None.")
	   (value   "Seeks among the scon's for an end-step with this roc."))
  (let* ((start-step (roc~start-step roc)))

    (do* ((current-step start-step (back=later-step-with-equal-roc current-step)))
	((or (null current-step)
	     (back=end-step-p current-step))
	 current-step))))

(defgeneric back=start-task-is-removed-p (start-task step deleted-nodes)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A start-task, a step (which is removed) and a set of nodes (which are removed).")
	   (effect  "None.")
	   (value   "T if start-task is removed, that means:"
		    "if start-task is an inst-task, if its step is equal to the input step,"
		    "if start-task is a goal-task, if its node is among the deleted nodes."))
  (:method ((start-task agenda+inst-task) step deleted-nodes)
	   (eq step (agenda~inst-task-plan-step start-task)))
  (:method ((start-task agenda+goal) step deleted-nodes)
	   (find (agenda~task-node start-task) deleted-nodes))
  (:method ((start-task agenda+goal-schema) step deleted-nodes)
	   (find (agenda~task-node start-task) deleted-nodes)))
  

  
(defun back=get-twin-steps (pds node just reason)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, a node, a justification of this node and"
		    "the own reason of thi just.")
	   (effect  "None.")
	   (value   "A list of steps which are the twin-reasons of this reason (the twin"
		    "reason are that reasons which arised at the same application as"
		    "the input reason)."))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (conc-labels (remove-if #'(lambda (pat) (or (infer~nonexistent-pattern-p pat)
						     (infer~existent-pattern-p pat)
						     (infer~closed-pattern-p pat)))
				 outln-pat)))
    (apply 'append (mapcar #'(lambda (conc-label)
			       (let* ((conc (pds~label2node conc-label pds)))
				 (if conc
				     ;;The twin reason in CONC is not yet considered
				     (let* ((conc-just (node~justification conc)))
				       (if (pdsj~open-p conc-just)
					   ;; the twin step got already deleted
					   nil
					 (let* ((the-just (pdsj~get-twin-just conc-just (just~method just) outln-pat))
						(the-own-reason (pdsj~own-reason the-just)))
					   (if the-own-reason
					       ;;The twin reason is not yet deleted:
					       (list the-own-reason)
					     nil))))
				   nil)))
			   conc-labels))))


(defun back=remove-reason! (pds node just reason)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications, and"
		    "one of the justification reasons.")
	   (effect  "Deletes REASON from the reason list of JUST. If the reason"
		    "list of NODE becomes empty, NODE has to be removed from PDS.")
	   (value   "A list with node, if node is removed from PDS (because reason was the last"
		    "reason of node), nil otherwise."))
  (let ((new-reasons (remove reason (pdsj~reasons just)))
	(root-node (prob~proof-root pds)))

    (when (pdsj~below-reasons just)
      (omega~message "~%Something wrong in function back=remove-reason!!"))
    ;; Below-justifications are newer justs! These should have been deleted at this point!
    
    (setf (pdsj~reasons just) new-reasons)

    (if (null (or new-reasons
		  (pdsj~above-reasons just)
		  (pdsj~below-reasons just)
		  (keim~equal node root-node)
		  (find node (pdsn~hyps root-node))))
	(progn
	  (if (equal just (pdsc~an-just reason))
	      ;; REASON is the last own reason of NODE, then deletes the premises of
	      ;; JUST everywhere they occur as unsponsors when deleting NODE.
	      (keim::pds=remove-node! pds node (just~premises just))
	    (keim::pds=remove-node! pds node))
	  (list node))
      nil)))

(defun back=before-and-after-steps (step step-list)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A step and a list of steps.")
	   (effect  "None.")
	   (value   "Multiple-Value:"
		    "First: the list of steps from step-list which are before the step."
		    "Second: the list of steps from step-list which are after the step."))
  (let* ((all-steps-after (back=all-plan-steps-after step))
	 (all-steps-before (back=all-plan-steps-before step)))

    (values
     (intersection all-steps-before step-list)
     (intersection all-steps-after step-list))))
     
(defun back=all-plan-steps-after (step)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A plan step.")
	   (effect  "None.")
	   (value   "A list of all plan steps after the step (the transitive closure of"
		    "the successor relation)."))
  (let* ((just (pdsc~an-just step))
	 (control (pdsj~control just))
	 (succ (pdsc~successor control)))

    (if succ
	(cons succ (back=all-plan-steps-after succ))
      nil)))

(defun back=all-plan-steps-before (step)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A plan step.")
	   (effect  "None.")
	   (value   "A list of all plan steps before step (the transitive closure of"
		    "the successor relation)."))
  (let* ((just (pdsc~an-just step))
	 (control (pdsj~control just))
	 (pred (pdsc~predecessor control)))
    
    (if pred
	(append (back=all-plan-steps-before pred) (list pred))
      nil)))
	    
(defun back=constraint-pools (pds)
  (declare (edited  "28-JUN-1999")
	   (authors Ameier)
	   (input   "A pds.")
	   (effect  "None.")
	   (value   "The list of all constraint pools to this pds."))
  (let* ((constraint-pool (pds~constraint-pool pds)))

    (if constraint-pool
	(do* ((curr-cstrpool constraint-pool)
	      (back-pools (list constraint-pool)))
	    ((null curr-cstrpool)
	     back-pools)
	  (let* ((previous (pds~cstrpool-previous curr-cstrpool)))
	    (setq curr-cstrpool previous)
	    (when previous
	      (setq back-pools (cons previous back-pools)))))
      nil)))

(defun back=inconsistent-steps-below (pds node just)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, one of its nodes, one of the node justifications.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of steps which remain valid if just gets open,"
		    "Second: A list of steps which have to be removed (because they get invalid)"
		    "        if just gets open."))
  (let* ((below-just (pdsj~below just)))
    (when below-just
      (let* ((own-step (pdsj~own-reason below-just)))
	
	;; 1.) Compute all steps below
	(multiple-value-bind
	    (rest-steps1 steps-to-undo1)
	    (back=inconsistent-steps-below pds node below-just)
	  
	  ;; 2.) Compute all steps on this level
	  (multiple-value-bind
	      (rest-steps2 steps-to-undo2)
	      (back=inconsistent-steps-of-just pds node below-just own-step)

	    (values (union rest-steps2 (set-difference rest-steps1 (union (list own-step) steps-to-undo2)))
		    (union steps-to-undo1 (union (list own-step) steps-to-undo2)))))))))

;; OWN JUSTIFICATION OF BELOWER STEPS

(defun back=inconsistent-steps-of-just (pds node just reason)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A pds, one of its nodes, and one of the node justifications, and the"
		    "own reason of the justification.")
	   (effect  "Undo the reasons of JUST that become inconsistent when JUST becomes"
		    "open.")
	   (value   "Multiple-Value:"
		    "First: A list of reasons/steps which remain valid if just gets open,"
		    "Second: A list of steps which have to be removed (because they get invalid)"
		    "        if just gets open."))
  
  (let* ((other-reasons (pdsj~other-reasons just)))

    ;;  (values
    ;;   (remove-if #'(lambda (r)
    ;;		    (pdsj~clsd-premise-p (pdsc~an-just r) node))
    ;;		other-reasons)
    ;;    (remove-if-not #'(lambda (r)
    ;;			(pdsj~clsd-premise-p (pdsc~an-just r) node))
    ;;		    other-reasons))))
    
    ;; It turned out that we have currently problems with this function since the function pdsj~clsd-premise-p works not properly
    ;; if in the outline pattern there are lists!
    ;; Hence, I decided to remove this stuff!
    ;; Hence, this function returns currently that all reasons/steps stay valid
    
    (values
     other-reasons
     nil)))

#| --------------------------------------------------- RETRACTION OF STRATEGY-KS STEPS -------------------------------------------- |#

;; -------------------------------------------------------> Auxiliaries

(defun back=end-step-p (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step.")
	   (effect  "None.")
	   (value   "T if the justification of the step is a end-justification."))
  (let* ((just (scon~strategy-step-just step))
	 (just-method (just~method just)))

    (if (eq just-method (infer~find-method 'end-strategy-ks-application))
	't
      nil)))

(defun back=start-step-p (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step.")
	   (effect  "None.")
	   (value   "T if the justification of the step is a start-justification."))
  (let* ((just (scon~strategy-step-just step))
	 (just-method (just~method just)))

    (if (eq just-method (infer~find-method 'start-strategy-ks-application))
	't
      nil)))

(defun back=interrupt-step-p (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step.")
	   (effect  "None.")
	   (value   "T if the justification of the step is a interrupt-justification."))
  (let* ((just (scon~strategy-step-just step))
	 (just-method (just~method just)))

    (if (eq just-method (infer~find-method 'interrupt-strategy-ks-application))
	't
      nil)))

(defun back=reinvoke-step-p (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step.")
	   (effect  "None.")
	   (value   "T if the justification of the step is a reinvoke-justification."))
  (let* ((just (scon~strategy-step-just step))
	 (just-method (just~method just)))

    (if (eq just-method (infer~find-method 'reinvoke-strategy-ks-application))
	't
      nil)))

(defun back=later-step-with-equal-roc (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A step.")
	   (effect  "None.")
	   (value   "Seeks in the successors of the step for a step with the same ROC."))
  (let* ((just (scon~strategy-step-just step))
	 (roc (first (pdsj~parameters just))))

    (do* ((current-step step)
	  (current-succ (scon~strategy-step-successor step))
	  (step-found nil))
	((or (null current-succ)
	     step-found)
	 step-found)
      (let* ((just-of-succ (scon~strategy-step-just current-succ))
	     (roc-of-succ (first (pdsj~parameters just-of-succ))))

	(if (eq roc-of-succ roc)
	    (setq step-found current-succ)
	  (progn
	    (setq current-step current-succ)
	    (setq current-succ (scon~strategy-step-successor current-succ))))))))

(defun back=all-steps-after (step)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon-step.")
	   (effect  "None.")
	   (value   "A list of all scon-steps after this step (the transitive closure of its successors)."))
  (let* ((succ (scon~strategy-step-successor step)))
    (if succ
	(cons succ (back=all-steps-after succ))
      nil)))

(defun back=insert-task-at-the-end-of-agenda! (task agenda)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A task and an Agenda.")
	   (effect  "Introduces the task in the last agenda.")
	   (value   "The returned agenda."))
  (if (agenda~empty-p agenda)

      (omega~warning "~%Don't know how to handle empty agenda in funktion  back=insert-task-at-the-end-of-agenda!")
    
    (let* ((last-agenda (do* ((curr-agenda agenda (agenda~then-agenda curr-agenda)))
			    ((agenda~empty-p (agenda~then-agenda curr-agenda))
			     curr-agenda))))
      (setf (agenda~next-tasks last-agenda)
	    (cons task (agenda~next-tasks last-agenda)))
      
      agenda)))

;; ----------------------------------------------> END-STEP

;; Retracting end-step:
;; In case PPlanner:
;;        to 1.) -> write Roc in store (but no demands)
;;               -> Roc entries in all tasks of nodes of the outline
;;               -> Replace this end-step with an interrupt for this Roc in the strategy-steps
;;               -> All demands, that were fulfilled by this end-step (must be annotated to end-step) and that are interesting
;;                  for the current ROCs in the store, are written to the DEMANDS again.
;;               -> Annotation in ROC: aktiv t
;;        to 2.) Nothing
;;        to 3.) Nothing
;;        to 4.) Nothing
;; In Case InstMeta:
;;        to 1.) -> Delete end-step from steps
;;               -> Retract the corresponding start-step (directly before it)!
;;        to 2.) -> Nothing
;;        to 3.) -> Nothing
;;        to 4.) -> Nothing

(defun back=remove-end-strategy-ks-step (step control-flag)
  (declare (edited  "24-JUN-1999")
	   (authors Ameier)
	   (input   "A scon-step with an end-strategy-ks justificiation and a control flag stating"
		    "whether the corresponding control-information should be changed.")
	   (effect  "see above")
	   (value   "A list with the undone steps."))    

  (if (find step (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
      ;; Step already is actual
      ;; -> remove
      
      (let* ((just (scon~strategy-step-just step))
	     (roc (first (pdsj~parameters just)))
	     (strategy-ks (roc~strategy-ks roc))
	     (refinement-algorithm (strat~strategy-ks-refinement-algorithm strategy-ks)))

	(when back*verbose
	  (omega~message "*** BackTrack ***: Removing EndStep ~A" step))
	
	(cond ((or (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'pplanner))
		   (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'cplanner)))
	       (let* ((rocs-store (black~get-blackboard-object-content 'store sod*solution-blackboard))
		      (agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		      (all-tasks (agenda~all-tasks agenda))
		      (demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
		      (new-interrupt-jutification (strat~interrupt-strategy-ks-application-create-just
						   (list roc (roc~copy-state-description roc))))
		      (new-interrupt-step (scon~create-scon-step (scon~strategy-step-predecessor step)
								 (scon~strategy-step-successor step)
								 new-interrupt-jutification))
		      (fulfilled-demands (keim~get step 'fulfilled-demands))
		      (actual-rocs (store~elements rocs-store))
		      (actual-demands (store~elements demands-store))
		      (demands-of-actual-rocs (apply 'append (mapcar #'roc~demands actual-rocs)))
		      (demands-for-actual-store-rocs (remove-if-not #'(lambda (demand)
									(and
									 (find demand demands-of-actual-rocs :test #'eq)
									 (null (find demand actual-demands)))) 
								    demands-of-actual-rocs))
		      (outline-nodes (roc~pplanner-outline-lines roc)))
				     
		 (when (null (find roc (store~elements rocs-store)))
		   (store~add-element! rocs-store roc))
		 
		 (mapcar #'(lambda (outline-node)
			     (let* ((task-to-this-node (find outline-node all-tasks
							     :test #'(lambda (node task)
								       (if (and (agenda~goal-or-goal-schema-task-p task)
										(eq (agenda~task-node task) node))
									   't
									 nil)))))
			       (when task-to-this-node 
				 (setf (agenda~task-rocs task-to-this-node)
				       (remove-duplicates (cons roc (agenda~task-rocs task-to-this-node)))))))
			 outline-nodes)
		 
		 (scon~replace-step! new-interrupt-step step)
		 
		 (store~add-elements! demands-store demands-for-actual-store-rocs)
		 
		 (setf (roc~activ roc) 't)

		 ;; step deleted -> return the step
		 (list step)))

	      ((eq refinement-algorithm (refalg~find-refinement-algorithm-object 'backtrack))
	       
	       (let* ((pred (scon~strategy-step-predecessor step))
		      (pred-roc (if pred
				    (first (pdsj~parameters (scon~strategy-step-just pred)))
				  nil)))
		 
		 (scon~delete-step! step)
		 
		 ;; Step deleted -> return the step + evtl. further deleted steps
		 (cons step
		       (if (eq roc pred-roc)
			   ;; Predecessor step is StartBACKTRACK -> retract this step
			   (back=remove-step! pred control-flag)
			 ;; should actually not occur, but:
			 nil))))
	      
	      ((eq refinement-algorithm (refalg~find-refinement-algorithm-object 'instmeta))
	       (let* ((pred (scon~strategy-step-predecessor step))
		      (pred-roc (if pred
				    (first (pdsj~parameters (scon~strategy-step-just pred)))
				  nil)))
		 
		 (scon~delete-step! step)
		 
		 ;; Step deleted -> return the step + evtl. further deleted steps
		 (cons step
		       (if (eq roc pred-roc)
			   ;; Predecessor step is StartInstmeta -> retract this step
			   (back=remove-step! pred control-flag)
			 ;; should actually not occur, but:
			 nil))))
	      (t
	       (error "~%Something Wrong in Function back=remove-end-strategy-ks-step"))
	      
	      
	      ))
    
    ;; step was already deleted -> not deleted again -> return nil
    nil))

;; ---------------------------------------------------------------> Reinvoke Step 

;; Retract Reinvoke-Step:
;; In case PPlanner
;;      zu 1.) -> If there is a later step to the same Roc, delete it first
;;             -> Then delete reinvoke step from strategy-ks-steps
;;             -> Write Roc in store (if not already in!)
;;	zu 2.) -> Nothing
;;      zu 3.) -> Nothing
;;      zu 4.) -> Nothing

(defun back=remove-reinvoke-strategy-ks-step (step control-flag)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step with a reinvokation justification and a control flag stating"
		    "whether the corresponding control-information should be changed.")
	   (effect  "See above.")
	   (value   "A list of all deleted steps."))
  (if (find step (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
      ;; Step already is actual
      ;; -> remove
      
      (let* ((just (scon~strategy-step-just step))
	     (roc (first (pdsj~parameters just)))
	     (strategy-ks (roc~strategy-ks roc))
	     (refinement-algorithm (strat~strategy-ks-refinement-algorithm strategy-ks)))

	(when back*verbose
	  (omega~message "*** BackTrack ***: Removing ReinvokeStep ~A" step))
	
	(cond ((or (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'pplanner))
		   (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'cplanner)))
	       (let* ((rocs-store (black~get-blackboard-object-content 'store sod*solution-blackboard))
		      (later-step-with-equal-roc (back=later-step-with-equal-roc step))
		      (removed-steps1 (if later-step-with-equal-roc
					  (back=remove-step! later-step-with-equal-roc control-flag)
					nil))
		      (removed-steps2 (if (and later-step-with-equal-roc
					       (back=end-step-p later-step-with-equal-roc))
					  ;; A new interrupt was inserted for end-step -> it has to be removed
					  (let* ((later-step-with-equal-roc2 (back=later-step-with-equal-roc step)))
					    (back=remove-step! later-step-with-equal-roc2 control-flag))
					nil)))
		 
		 (scon~delete-step! step)
		 
		 (when (null (find roc (store~elements rocs-store)))
		   (store~add-element! rocs-store roc))

		 ;; Return the deleted steps
		 (append removed-steps1 removed-steps2 (list step))))
	      (t
	       (error "~%Something wrong in function  back=remove-reinvoke-strategy-ks-step"))))

    ;; step was already deleted -> not deleted again -> return nil
    nil))

;; --------------------------------------------------------------> Interrupt Step

;; Retract Interrupt-Step:
;; In case PPlanner:
;;        to 1.) -> if there is a later step to the same Roc, delete it first
;;               -> Then delete reinvoke step from strategy-ks-steps
;;               -> Write Roc in store (if not already in!)
;;               -> Retract the demands, that ware made by this interrupt (have to be annotated to the Interrupt-Step!) from the DEMANDS 
;;                  as far as they are still in!
;;        to 2.) -> Nothing
;;        to 3.) -> Nothing
;;        to 4.) -> Nothing

(defun back=remove-interrupt-strategy-ks-step (step control-flag)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step with a interrupt justification and a control flag stating"
		    "whether the corresponding control-information should be changed.") 
	   (effect  "See above.")
	   (value   "A list of all deleted steps."))
  (if (find step (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
      ;; Step already is actual
      ;; -> remove
      
      (let* ((just (scon~strategy-step-just step))
	     (roc (first (pdsj~parameters just)))
	     (strategy-ks (roc~strategy-ks roc))
	     (refinement-algorithm (strat~strategy-ks-refinement-algorithm strategy-ks)))

	(when back*verbose
	  (omega~message "*** BackTrack ***: Removing InterruptStep ~A" step))
	
	(cond ((or (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'pplanner))
		   (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'cplanner)))
	       (let* ((rocs-store (black~get-blackboard-object-content 'store sod*solution-blackboard))
		      (demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
		      (later-step-with-equal-roc (back=later-step-with-equal-roc step))
		      (removed-steps (if later-step-with-equal-roc
					 (back=remove-step! later-step-with-equal-roc control-flag)
	      			       nil))
		      (posed-demands (keim~get step 'posed-demands)))
		 
		 (scon~delete-step! step)

		 (setf (store~elements demands-store)
		       (remove-if #'(lambda (demand)
				      (find demand posed-demands :test #'eq))
				  (store~elements demands-store)))
		 
		 (when (null (find roc (store~elements rocs-store)))
		   (store~add-element! rocs-store roc))

		 ;; return the deleted steps
		 (append removed-steps (list step))))
	      (t
	       (error "~%Something wrong in function  back=remove-interrupt-strategy-ks-step"))))

    ;; step was already deleted -> not again deleted -> return nil
    nil))


;; --------------------------------------------------------------> Start Step

;; Retract Start-Step:
;; In case PPlanner:
;;        to 1.) -> If there is a later step to this Roc, delete it first
;;               -> Delete this step
;;	         -> Delete Roc from store (if in)	
;;               -> Delete all planning steps
;;               -> Remove ROC from ROCS list
;;        to 2.) -> Nothing
;;        to 3.) -> If adaption is required, annotate start-task (already-applied-strategy-ks) 
;;        to 4.) -> Nothing
;;        additional: All strategy-task-demands with this strategy and task are deleted! But they are not treated
;;                     as successfull!
;; In case InstMeta:
;;        to 1.) -> Delete this step
;;               -> if there are further steps with this ROC (say end-step), delete it
;;               -> Remove ROC from ROCS list
;;        to 2.) -> Delete all steps, that were made from this constraint store on + adapt constraint store
;;                  Concretly: -> Remove all steps, that were made after this Inst-Meta (all steps, that occur after the
;;                                last-plan-step of this step).
;;                             -> Look for the last constraint store before it -> Set the current constrain store to this!
;;        to 3.) -> If adaption is required, annotate the applied strategy to the task (already-applied-strategy-ks)
;;        to 4.) -> Instantiation-task again in the agenda (where ?)

(defun back=remove-start-strategy-ks-step (step control-flag)
  (declare (edited  "25-JUN-1999")
	   (authors Ameier)
	   (input   "A scon step with a start justification and a control flag stating"
		    "whether the corresponding control-information should be changed.")
	   (effect  "See above.")
	   (value   "A list of all deleted steps."))

  ;;(format t "~%THE STEP: ~A" step)
  ;;(setf steppi step)
  ;;(format t "~%THE SSTEPS: ~A" (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
  ;;(setf steppis (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
  
  (if (find step (store~elements (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))
      ;; Step already is actual
      ;; -> remove

      (let* ((just (scon~strategy-step-just step))
	     (roc (first (pdsj~parameters just)))
	     (strategy-ks (roc~strategy-ks roc))
	     (refinement-algorithm (strat~strategy-ks-refinement-algorithm strategy-ks)))
	
	(when back*verbose
	  (omega~message "*** BackTrack ***: Removing StartStep ~A" step))
	
	(cond ((or (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'pplanner))
		   (eq refinement-algorithm (refalg~find-refinement-algorithm-object 'cplanner)))
	       (let* ((rocs-store (black~get-blackboard-object-content 'store sod*solution-blackboard))
		      (rocs-list (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard))
		      (later-step-with-equal-roc (back=later-step-with-equal-roc step))
		      (removed-steps1 (if later-step-with-equal-roc
					 (back=remove-step! later-step-with-equal-roc control-flag)
					nil))
		      (plan-steps (roc~pplanner-steps roc))
		      (removed-steps2 (apply 'append (mapcar #'(lambda (plan-step)
								 (back=remove-step! plan-step control-flag))
							     plan-steps)))
		      (start-task (roc~start-task roc))
		      (strategy-ks (roc~strategy-ks roc)))
		 
		 
		 (scon~delete-step! step)
		 
		 (when (find roc (store~elements rocs-store))
		   (store~remove-element! rocs-store roc))
		 
		 ;;(when control-flag
		 ;;  (setf (agenda~task-already-applied-strategy-ks start-task)
		 ;;	 (remove-duplicates (cons (list strategy-ks (roc~parameters roc))
		 ;;				  (agenda~task-already-applied-strategy-ks start-task))
		 ;;			    :test #'keim~equal)))
		 ;; IT is not enough to store the already applied strategy-ks only on the task, they have to be
		 ;; stored at the NODE! ->

		 (when control-flag
		   (setf (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node start-task))))
			 ;; (remove-duplicates (cons (list strategy-ks (roc~parameters roc))
			 ;;                          (keim::pdsc~already-applied-strategy-ks
			 ;;    			     (pdsj~control (node~justification (agenda~task-node start-task)))))
			 ;; 		       :test #'keim~equal)))
			 ;; A removing of duplicates is not suitable if a strategy can be applied moew than once.
			 ;; Then it becomes important how often a strategy is applied!
			 (cons (list strategy-ks (roc~parameters roc))
			       (keim::pdsc~already-applied-strategy-ks
				(pdsj~control (node~justification (agenda~task-node start-task)))))))
		 
				
			 
		 ;; Note: A new task was created at the opening of the node
		 ;; now the control informatin from the start-task has to be entried in the task for the this node
		 (back=update-task-in-agenda! start-task roc)
		 
		 (store~remove-element! rocs-list roc)

		 ;; All demands which have the same strategy and task as roc are removed!
		 ;; Nevertheless they are not conted as successful!
		 (multiple-value-bind
		     (store2 rem del)
		     (store~demands-store-remove-fulfilled-demands!
		      (black~get-blackboard-object-content 'demands sod*control-blackboard)
		      roc)
		   (when back*verbose
		     (omega~message "*** BackTrack ***: The following demands are broken and thus removed: ~A" del)))
		 
		 ;; Rueckgabe der geloeschten Steps
		 (append removed-steps1 removed-steps2 (list step))))
	      
	      ((eq refinement-algorithm (refalg~find-refinement-algorithm-object 'backtrack))
	       
	       (let* ((rocs-store (black~get-blackboard-object-content 'store sod*solution-blackboard))
		      (rocs-list (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard))
		      (later-step-with-equal-roc (back=later-step-with-equal-roc step))
		      (removed-steps1 nil)
		      (start-task (roc~start-task roc))
		      (strategy-ks (roc~strategy-ks roc)))

		 (scon~delete-step! step)

		 (when later-step-with-equal-roc
		   (setf removed-steps1 (back=remove-step! later-step-with-equal-roc control-flag)))
		 
		 (when (find roc (store~elements rocs-store))
		   (store~remove-element! rocs-store roc))

		 (when control-flag
		   (setf (agenda~task-already-applied-strategy-ks start-task)
		 	 (remove-duplicates (cons (list strategy-ks (roc~parameters roc))
		 				  (agenda~task-already-applied-strategy-ks start-task))
		                         :test #'keim~equal)))
		 ;; In the PPLANNER CASE we store the already applied strategy-ks only on the node directly, otherwise they are
		 ;; stored at the TASK ->
		 
		 (store~remove-element! rocs-list roc)
		 
		 ;; Return the deleted steps
		 (append removed-steps1 (list step))))
	      
	      ((eq refinement-algorithm (refalg~find-refinement-algorithm-object 'instmeta))
	       (let* ((rocs-list (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard))
		      (all-scon-steps-after-this-step (back=all-steps-after step))
		      (start-task (roc~start-task roc))
		      (strategy-ks (roc~strategy-ks roc)))
		 
		 (scon~delete-step! step)

		 (store~remove-element! rocs-list roc)

		 ;; ATTENTION: ORDER IS IMPORTANT!
		 (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
			(cstr-pool (pds~constraint-pool pds))
			(previous-cstr-pool (roc~instmeta-last-cpool roc)) ;;(pds~cstrpool-previous cstr-pool))
			(removed-steps1 (apply 'append (mapcar #'(lambda (scon-step)
								   (back=remove-step! scon-step nil))
							       all-scon-steps-after-this-step)))
			;; it is not enough to remove only scon-stuff behind the instantiation step!
			;; it is also necessary to delete additionally all plan-steps after the last-plan-step that
			;; was done before the instantiation!
			(removed-steps2 (back=remove-plan-steps-after-inst! roc control-flag))
			(agenda (pds~agenda pds)))
		   
		   (when control-flag
		     (setf (agenda~task-already-applied-strategy-ks start-task)
		   	   (remove-duplicates (cons (list strategy-ks (roc~parameters roc))
		   				    (agenda~task-already-applied-strategy-ks start-task))
		   			      :test #'keim~equal)))
		   ;; In the PPLANNER CASE we store the already applied strategy-ks only on the node directly, otherwise they are
		   ;; stored at the TASK ->
		   
		   (setf (pds~constraint-pool pds) previous-cstr-pool)
		   (back=all-update-to-nil)
		   
		   ;; Instantiation-task normally not so important -> to the end of the agenda
		   ;; Ok , that is an in fact an a priori decision

		   ;; The inst task is added again to the agenda only if the step that created it is still in the
		   ;; pds plan-steps
		   (let* ((all-current-pds-plan-steps (back=all-pds-plan-steps omega*current-proof-plan)))
		     (when (find (agenda~inst-task-plan-step start-task) all-current-pds-plan-steps)
		       (setf (pds~agenda pds) (back=insert-task-at-the-end-of-agenda! start-task agenda)))) 
		   
		   ;; Return the deleted steps
		   (cons step (append removed-steps1 removed-steps2))
		   
		   )))
	      
	      (t
	       (error "~%Something Wrong in function back=remove-start-strategy-ks-step, the refinement algorithm is: ~A."
		      refinement-algorithm))))
    
    ;; step was already deleted -> not again deleted -> return nil
    nil))

(defun back=remove-plan-steps-after-inst! (roc control-flag)
  (declare (edited  "29-AUG-2000")
	   (authors Ameier)
	   (input   "A inst-meta state-description.")
	   (effect  "Removes all plan steps after the last-plan-step of the state-description.")
	   (value   "A list of all removed steps."))
  (let* ((last-plan-step (roc~instmeta-last-plan-step roc))
	 (all-steps-after (back=all-plan-steps-after last-plan-step))
	 (all-current-pds-plan-steps (back=all-pds-plan-steps omega*current-proof-plan))
	 (all-steps-to-remove (intersection all-steps-after all-current-pds-plan-steps)))

    (when back*verbose
      (omega~message "~%*** BACKTRACK INST-META ***: Computed the following steps to remove after a inst-meta: ~A"
		     all-steps-to-remove))

    (apply #'append (mapcar #'(lambda (step)
				(back=remove-step! step control-flag))
			    all-steps-to-remove))))


(defun back=all-pds-plan-steps (pds)
  (declare (edited  "18-APR-2000")
	   (authors Ameier)
	   (input   "A pds.")
	   (effect  "None.")
	   (value   "The set of all current pds-plan steps."))
  (let* ((first-step (pds~first-plan-step pds))
	 (last-step (pds~last-plan-step pds)))
    (cond ((or (null first-step)
	       (null last-step))
	   nil)
	  ((eq first-step last-step)
	   (list first-step))
	  (t
	   (do* ((curr-step first-step)
		 (back-steps nil))
	       ((eq curr-step last-step)
		back-steps)
	     (let* ((succ-step (pdsj~successor (pdsc~an-just curr-step))))
	       (setf curr-step succ-step)
	       (setf back-steps (cons succ-step back-steps))))))))
    

(defun back=update-task-in-agenda! (start-task roc)
  (declare (edited  "08-SEP-1999")
	   (authors Ameier)
	   (input   "A task and a roc")
	   (effect  "Seeks the task for the same node in the agenda and sets the already-applied-strategies"
		    "flag of this task to the content of this slot of the input task."
		    "Furthermore, the roc is removed from the rocs list of the found task.")
	   (value   "Undefined."))
  
  (let* ((all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (task-with-same-line (find start-task all-tasks :test #'(lambda (task1 task2)
								   (if (and (agenda~goal-or-goal-schema-task-p task2)
									    (eq (agenda~task-node task1)
										(agenda~task-node task2)))
								       't
								     nil)))))
    
    (when task-with-same-line
      ;; The following two lines should be not necessary since we decieded to store the already-applied-strategy-ks
      ;; directly on the nodes not on the tasks!
      (setf (agenda~task-already-applied-strategy-ks task-with-same-line)
      	    (agenda~task-already-applied-strategy-ks start-task))
      (setf (agenda~task-rocs task-with-same-line)
	    (remove roc (agenda~task-rocs task-with-same-line))))))

