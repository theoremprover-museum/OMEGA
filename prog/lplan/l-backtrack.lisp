;; -*- Syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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
 
(in-package "OMEGA")

(mod~defmod BACK
            :uses (pds pdsj pdsn pdsh)
            :documentation "The backtracking module"
            :exports (
		      back~retract-step
		      back~trace
		      back~track-task))

#{
This module deals with the backtracking process which has to be done when the first task on the
agenda cannot be solved. The algorithms, data structures and definitions in this module are developed
in a paper by Lassaad Cheikhrouhou. 
#}

(defvar back*trace nil "Flag for omega~trace outputs.")
(defvar back*messages t "Flag for omega~message outputs.")

(defun back~trace (format-string &rest args)
  (when back*trace
    (apply #'omega~trace (cons format-string args))))

(defun back~message (format-string &rest args)
  (when back*messages
    ;; (apply #'omega~message (cons format-string args))
    (apply 'format (cons t (cons (concatenate 'string format-string "~%") args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Backtracking Algorithm}
The process of backtracking is defined for both steps and tasks.
#}

(defun back~retract-step (rsn mode interacting-nodes pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn, a mode to take this reason back,"
		    "the interacting nodes to store in the failed steps and a pds.")
	   (effect  "Takes the refinement step associated to rsn back.")
	   (value   "A tuple:"
		    "- A boolean value, that is true iff a instantiating step was backtracked."
		    "- A list of added mor support nodes."
		    "- The consumer nodes for that new tasks have to be created and whose steps are followed by an"
		    "  instantiating step."
		    "- The other consumer nodes for that new tasks have to be created."))
  (if (pdsh~reason-p rsn)
      (if (or (or (eq mode 'retract) (eq mode 'reset)) (eq mode 'remove))
	  (progn 
	    (back~message "***********************************************************************************************************")
	    (back~message "*")
	    (cond ((eq mode 'retract) (back~message "* Retracting ~A..." rsn))
		  ((eq mode 'reset) (back~message "* Resetting ~A..." rsn))
		  ((eq mode 'remove) (back~message "* Removing ~A..." rsn)))
	    (back~message "* Interacting nodes: ~A" interacting-nodes)
	    (back~trace "")
	    (if (pdsh~reason-retracted rsn)
		(progn
		  (back~trace "Trying to retract an already retracted step")
		  (back~message "* ...backtracking of ~A completed." rsn)
		  (back~message "*")
		  (back~message "***********************************************************************************************************")
		  (values nil))
	      (multiple-value-bind (inst-step-p
				    added-mor-supports
				    new-task-nodes-followed-by-inst
				    new-task-nodes-other)
		  (back=retract-step rsn mode interacting-nodes pds)
		(setf (pdsh~reason-retracted rsn) t)
		(lagenda~delete-ordering! (pds~agenda pds) rsn)
		(back~trace "")
		(back~message "* ...backtracking completed.")
		(back~message "*")
		(back~message "***********************************************************************************************************")
		(values inst-step-p added-mor-supports new-task-nodes-followed-by-inst new-task-nodes-other))))
	(omega~error ";;; back~~retract-step: unknown mode ~A." mode))
    (omega~error ";;; back~~retract-step: Argument ~A is not a reason." rsn)))

(defgeneric back=retract-step (rsn mode interacting-nodes pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn, a mode to take this reason back,"
		    "the interacting nodes to store in the failed steps and a pds.")
	   (effect  "Takes the refinement step associated to rsn back.")
	   (value   "A tuple:"
		    "- A boolean value, that is true iff a instantiating step was backtracked."
		    "- A list of added mor support nodes."
		    "- The consumer nodes for that new tasks have to be created and whose steps are followed by an"
		    "  instantiating step."
		    "- The other consumer nodes for that new tasks have to be created."))
  (:method ((rsn pdsh+canonical-reason) mode interacting-nodes pds)
	   (back~trace "Step 1:  Remove inconsistent steps in the origination scope")
	   (let ((incons-inst-step-p (back=remove-inconsistent-steps rsn pds)))
	     (back~trace "Step 2:  Reset consumer steps")
	     (let ((cs-inst-step-p (back=reset-consumer-steps rsn pds)))
	       (back~trace "Step 3:  Delete nodes and justifications")
	       (let ((inference (back=delete-nodes-and-justs rsn pds)))
		 (multiple-value-bind (cn-inst-step-p
				       cn-added-mor-supports
				       cn-new-task-nodes-followed-by-inst
				       cn-new-task-nodes-other)
		     (back=process-consumer-nodes rsn mode interacting-nodes inference pds)
		   (back~trace "Step 5:  Reset influenced steps")
		   (multiple-value-bind (is-inst-step-p
					 is-added-mor-supports
					 is-new-task-nodes-followed-by-inst
					 is-new-task-nodes-other)
		       (back=reset-influenced-steps rsn pds)
		     (back~trace "Step 6:  Delete reason")
		     (back=delete-reason rsn pds)
		     (let* ((new-task-nodes (pdsh~new-task-nodes rsn))
			    (followed-p (pdsh~followed-by-instantiating-step-p rsn))
			    (new-task-nodes-followed-by-inst (when followed-p new-task-nodes))
			    (new-task-nodes-other (when (not followed-p) new-task-nodes)))
		       (values (or incons-inst-step-p cs-inst-step-p cn-inst-step-p is-inst-step-p
				   (pdsh~instantiating-step-p rsn))
			       (union cn-added-mor-supports is-added-mor-supports)
			       (union new-task-nodes-followed-by-inst
				      (union cn-new-task-nodes-followed-by-inst is-new-task-nodes-followed-by-inst))
			       (union new-task-nodes-other
				      (union cn-new-task-nodes-other is-new-task-nodes-other))))))))))
  (:method ((rsn pdsh+pr-reason) mode interacting-nodes pds)
	   (declare (ignore mode interacting-nodes pds))
	   ;;; call backtracking function of the refinement given by
	   ;;; (back~pr-reason-refinement rsn)
	   ))

(defun back~update-agenda (task new-task-nodes agenda pds)
  (back~trace "**")
  (let ((new-tasks (mapcar 'lagenda~create-task new-task-nodes)))
    ;; delete tasks
    (back~trace "** Deleting inconsistent tasks")
    (dolist (old-task (lagenda~tasks agenda))
	    (cond ((or (lagenda~goal-p old-task) (lagenda~goal-schema-p old-task))
		   (unless (pds~existent-p (keim~name (lagenda~task-node old-task)) pds)
		    (lagenda~delete-task! agenda old-task)))
		  ((and (lagenda~pseudo-goal-p old-task)
			(lagenda~pseudo-goal-step old-task))
		   (back=update-execution-task! old-task agenda pds))
		  (T ;;; expansion task
		   (when (pdsj~open-p (node~justification (lagenda~task-node old-task)))
		     (lagenda~delete-task! agenda old-task)))))
    ;; insert new tasks
    (back~trace "** Inserting tasks for nodes ~A" new-task-nodes)
    (setf (lagenda~tasks agenda) (append new-tasks (lagenda~tasks agenda)))
    (strat~update-control-link new-tasks)   
    ;; in case a forward strategic method was backtracked
    (when (and task
	       (or (lagenda~goal-p task) (lagenda~goal-schema-p task))
	       (pds~existent-p (keim~name (lagenda~task-node task)) pds))
      (strat~update-control-link (list task)))
    ))

(defun back=update-execution-task! (task agenda pds)
  ;;; should update an execution task according the updates of the PDS
  ;; This could depend on the refinement. In such a case we need to have a function for this purpose 
  )

(defun back~track-task (task agenda pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A task, an agenda and a pds.")
	   (effect  "Backtracks the task.")
	   (value   "Undefined."))
  (back~trace "** Backtracking task ~A" task)
  (back~trace "**")
  (let ((originator-step (pdsh~originator-step task))
	(latest-step (pdsh~latest-influencing-step task)))
    (if (or latest-step originator-step)
	(multiple-value-bind (inst-step-p add-mor-supports new-task-nodes-followed-by-inst new-task-nodes-other)
	    (if latest-step
		;; task is influenced by the mor step latest-step
		(back~retract-step latest-step 'retract (list (agenda~task-node task)) pds)
	      ;; task is not influenced by any mor step
	      (back~retract-step originator-step 'retract nil pds))
	  (let ((new-tasks (union new-task-nodes-followed-by-inst new-task-nodes-other)))
	    (back~update-agenda task new-tasks agenda pds)
	    (if inst-step-p
		;; an instantiating step was retracted
		(let ((mor-goals (pds~mor-goals pds))
		      (subst (when (pds~constraint-store pds) (pds~cstore-bindings (pds~constraint-store pds)))))
		  (multiple-value-bind (changed)
		      (pds~reset-current-formulas (pds~mor-nodes mor-goals) subst)
		    (pds~adapt-control-infos mor-goals changed)))
	      ;; no instantiating step was retracted
	      (let ((mor-goals (remove-if-not 'pdsn~schematic-p new-task-nodes-followed-by-inst))
		    (subst (when (pds~constraint-store pds) (pds~cstore-bindings (pds~constraint-store pds)))))
		(multiple-value-bind (changed unchanged)
		    (pds~reset-current-formulas (pds~mor-nodes mor-goals) subst)
		  (pds~adapt-control-infos mor-goals changed)
		  (pds~reset-current-formulas add-mor-supports subst changed unchanged))))
	    t))
      ;; task is root task and not influenced by any other task
      (progn
	(back~trace "**")
	(back~trace "** ~A is the root task and not influenced by any other task" task)
	(back~trace "")
	(back~trace "No proof plan can be found.")
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for investigation of steps for retraction used in step 1 and 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Investigation Of Steps}
If deleting or reopening a node which is involved in a proper refinement it is neccessary to
investigate the proper refinement for deletion or reopening of this node.
The data structure for this investigation is a list with four elements.
The reason which eventually has to be undone.
The mode in which this reason should be undone.
The node which is involved in the proper refinement.
The context in which the refinement should be undone, i.e. delete or reopen.
#}

(defgeneric back=investigate-for-in-context (rsn mode node context)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn and a mode, a node and a context (delete or reopen or unexpand).")
	   (effect  "None.")
	   (value   "The undoing mode if the refinement step associated to reason must be undone,"
		    "rsn otherwise."))
  (:method ((rsn pdsh+canonical-reason) mode node context)
	   (declare (ignore node))
	   (if (eq context 'delete) mode rsn))
  (:method ((rsn pdsh+ie-reason) mode node context)
	   (declare (ignore node))
	   (if (or (eq context 'unexpand) (eq context 'delete)) mode rsn))
  (:method ((rsn pdsh+pr-reason) mode node context)
	   (declare (ignore mode node context))
	   ;;; call the backtracking condition of the refinement
	   ;;; (pdsh~pr-reason-refinement rsn)
	   ;;; with arguments
	   ;;; (rsn mode node context)
	   ))

(defun back=investigate-and-retract (reasons pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A list of quadrupels with a reason, a mode, a node and a context."
		    "Second aregument is a pds.")
	   (effect  "Investigates every element for retraction and eventually retracts the step"
		    "with the mode the investigation delivers.")
	   (value   "True iff some instantiating step was retracted."))
  (when reasons
    (let* ((reason (first reasons))
	   (rsn (first reason))
	   (mode (second reason))
	   (node (third reason))
	   (context (fourth reason))
	   (result (back=investigate-for-in-context rsn mode node context)))
      (back~trace "         Investigate ~A" rsn)
      (back~trace "         for retraction of node ~A in context ~A..." node context)
      (let ((inst-step-p (if (not (eq rsn result))
			     (back~retract-step rsn result nil pds)
			   (progn
			     (back~trace "         ...retracting not neccessary")
			     nil)))
	    (rest-inst-step-p (back=investigate-and-retract (rest reasons) pds)))
	(or inst-step-p rest-inst-step-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 1 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 1 Of The Backtracking Algorithm}
#}

(defun back=remove-inconsistent-steps (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn and a pds.")
	   (effect  "Removes all steps in the originiation scope of rsn which are"
		    "inconsistent without rsn.")
	   (value   "True iff some instantiating step was retracted."))
  (let ((origin-scope (back=origination-scope rsn)))
    (back=investigate-and-retract origin-scope pds)))

(defgeneric back=origination-scope (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "A list of quadrupels with a reason, a mode, a node and a context in the"
		    "origination scope of rsn to be investigated."))
  (:method ((rsn pdsh+ie-reason))
	   (let ((orig-scope nil)
		 (expansion-nodes (pdsh~ie-expansion-nodes rsn))
		 (expansion-reasons (pdsh~ie-further-expansion-reasons rsn)))
	     (dolist (expansion-reason expansion-reasons)
	       (setf orig-scope (append orig-scope (list (list expansion-reason 'remove nil 'unexpand)))))
	     (dolist (node expansion-nodes)
	       (dolist (reason (pdsn~reasons node))
		 (when (not (eq reason rsn))
		   (setf orig-scope (append orig-scope (list (list reason 'remove node 'delete)))))))
	     orig-scope))
  (:method ((rsn pdsh+ia-reason))
	   (let ((orig-scope nil)
		 ;;(conclusion (first (pdsh~ia-conclusions rsn)))
		 (expansion-reason (pdsh~ia-expansion-reason rsn)))
	     (when expansion-reason
	       (setf orig-scope (append orig-scope (list (list expansion-reason 'remove nil 'unexpand)))))
	     (dolist (subgoal (pdsh~ia-subgoals rsn))
	       (dolist (reason (pdsn~reasons subgoal))
		 (when (not (eq reason rsn))
		   (setf orig-scope (append orig-scope (list (list reason 'remove subgoal 'delete)))))))
	     orig-scope)))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 2 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 2 Of The Backtracking Algorithm}
#}

(defun back=reset-consumer-steps (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn and a pds.")
	   (effect  "Resets the consumer steps of rsn")
	   (value   "True iff some instantiating step was retracted."))
  (let ((consumer-steps (back=consumer-steps rsn)))
    (back=investigate-and-retract consumer-steps pds)))

(defgeneric back=consumer-steps (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "The consumer steps for further investigation represented by"
		    "a list of quadrupels with a reason, a mode, a node and a context."))
  (:method ((rsn pdsh+ie-reason))
	   nil)
  (:method ((rsn pdsh+ia-reason))
	   (let ((consumer-steps nil))
	     (dolist (pg (pdsh~ia-primary-goals rsn))
	       (dolist (reason (pdsn~reasons pg))
		 (when (not (eq reason rsn))
		   (setf consumer-steps (append consumer-steps (list (list reason 'reset pg 'reopen)))))))
	     (dolist (ac (pdsh~ia-add-conclusions rsn))
	       (dolist (reason (pdsn~reasons ac))
		 (when (not (eq reason rsn))
		   (setf consumer-steps (append consumer-steps (list (list reason 'reset ac 'delete)))))))
	     consumer-steps)))
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 3 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 3 Of The Backtracking Algorithm}
#}

(defgeneric back=delete-nodes-and-justs (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason and a pds.")
	   (effect  "Deletes the nodes and justs created by rsn from the pds.")
	   (value   "The inference of rsn, if rsn represents an inference application."))
  (:method ((rsn pdsh+ie-reason) pds)
	   (pds~delete-nodes (pdsh~ie-expansion-nodes rsn) pds)
	   (dolist (expansion-origin (pdsh~ie-expansion-origins rsn))
	     (setf (node~justification (first expansion-origin)) (second expansion-origin))
	     (setf (pdsj~below (second expansion-origin)) nil)
	     (setf (pdsj~status (second expansion-origin)) "unexpanded"))
	   ;; the return value does not matter because only inference applications are stored as failed steps
	   nil)
  (:method ((rsn pdsh+ia-reason) pds)
	   (let ((add-conclusions (pdsh~ia-add-conclusions rsn))
		 (extra-hypotheses (pdsh~ia-extra-hypotheses rsn))
		 (subgoals (pdsh~ia-subgoals rsn))
		 (primary-goals (pdsh~ia-primary-goals rsn))
		 (inference (pdsh~ia-inference rsn)))
	     (when add-conclusions
	       (back~trace "         Deleting add conclusions ~A" add-conclusions)
	       (pds~delete-nodes add-conclusions pds))
	     (when extra-hypotheses
	       (back~trace "         Deleting extra hypotheses ~A" extra-hypotheses)
	       (pds~delete-nodes extra-hypotheses pds))
	     (when subgoals
	       (back~trace "         Deleting subgoals ~A" subgoals)
	       (pds~delete-nodes subgoals pds))
	     (when primary-goals
	       (back~trace "         Reopening primary goals ~A" primary-goals)
	       (pds~reopen-nodes primary-goals pds))
	     ;; return the inference because the justifiactions of the primary goals are
	     ;; changed destructivly
	     inference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 4 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 4 Of The Backtracking Algorithm}
#}

(defun back=process-consumer-nodes (rsn mode interacting-nodes inference pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason rsn, a mode to take this reason back, the interacting nodes and a pds.")
	   (effect  "Prevents the retry of the refinement step associated to rsn"
		    "and updates the supports of the consumer-nodes.")
	   (value   "A tuple:"
		    "- A boolean value, that is true iff a instantiating step was backtracked."
		    "- A list of added mor support nodes."
		    "- The consumer nodes for that new tasks have to be created and whose steps are followed by an"
		    "  instantiating step."
		    "- The other consumer nodes for that new tasks have to be created."))
  (if (eq mode 'remove) (values nil nil nil nil)
    (let* ((consumer-nodes (back=consumer-nodes rsn))
	   (inst-step-p nil)
	   (add-mor-supports nil)
	   (new-task-nodes-followed-by-inst nil)
	   (new-task-nodes-other nil))
      (back~trace "Step 4:  Consumer nodes: ~A" consumer-nodes)
      (dolist (node consumer-nodes)
	(when (not (eq mode 'reset))
	  (progn
	    (back~trace "Step 4a: Prevent retry of this step wrt. node ~A" node)
	    (multiple-value-bind (inst-step-add-p add-mor-supports-add new-task-nodes-followed-by-inst-add
						  new-task-nodes-other-add)
		(back=prevent-retry-of node rsn interacting-nodes inference pds)
	      ;; collect the return values
	      (setf inst-step-p (or inst-step-p inst-step-add-p)
		    add-mor-supports (union add-mor-supports add-mor-supports-add)
		    new-task-nodes-followed-by-inst (union new-task-nodes-followed-by-inst
							   new-task-nodes-followed-by-inst-add)
		    new-task-nodes-other (union new-task-nodes-other new-task-nodes-other-add)))))
	(back~trace "Step 4b: Update supports of the consumer node ~A" node)
	(let ((add-mor-supports-add (back=update-supports node rsn pds)))
	  (setf add-mor-supports (union add-mor-supports add-mor-supports-add))))
      (values inst-step-p add-mor-supports new-task-nodes-followed-by-inst new-task-nodes-other))))
;;      (if (eq mode 'reset)
;;	  (values nil)
;;	(values new-task-nodes)))))

(defgeneric back=consumer-subgoals (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "Return the subgoals relevant for updating the supports of."))
  (:method ((rsn pdsh+ie-reason))
	   nil)
  (:method ((rsn pdsh+ia-reason))
	   (pdsh~ia-subgoals rsn))
  (:method ((rsn pdsh+pr-reason))
	   ;;; How to determine the subgoals of a proper refinement?
	   ))

(defun back=iter-consumer-nodes (failed-rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "The failed reason rsn and a node.")
	   (effect  "None.")
	   (value   "Collects the nodes supported by the add conclusions of the failed reason"
		    "in a list. The search is a depth first search and begins from node."))
  (let ((add-concs (pdsh~ia-add-conclusions failed-rsn))
	(consumer-nodes nil))
    (dolist (reason (pdsh~reasons-later-in-list (pdsn~reasons node) failed-rsn))
      (dolist (subgoal (back=consumer-subgoals reason))
	(when (pds~node-supported-by-p subgoal add-concs)
	  (setf consumer-nodes
		(append consumer-nodes
			(cons subgoal (back=iter-consumer-nodes failed-rsn subgoal)))))))
    consumer-nodes))
  
(defgeneric back=consumer-nodes (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "Returns the consumer nodes of rsn."))
  (:method ((rsn pdsh+ie-reason))
	   (pdsh~ie-expansion-origin-nodes rsn))
  (:method ((rsn pdsh+ia-reason))
	   (let ((consumer-nodes (append (pdsh~ia-primary-goals rsn) (pdsh~ia-secondary-goals rsn))))
	     (dolist (sg (pdsh~ia-secondary-goals rsn))
	       (setf consumer-nodes (append consumer-nodes (back=iter-consumer-nodes rsn sg))))
	     consumer-nodes)))

(defgeneric back=prevent-retry-of (node rsn interacting-nodes inference pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node, a reason, the interacting nodes and a pds.")
	   (effect  "Prevents the retry of an inference application by storing the failed step in node"
		    "or recursively retracts steps in case of an inference expansion.")
	   (value   "A tuple:"
		    "- A boolean value, that is true iff a instantiating step was backtracked."
		    "- A list of added mor support nodes."
		    "- The consumer nodes for that new tasks have to be created and whose steps are followed by an"
		    "  instantiating step."
		    "- The other consumer nodes for that new tasks have to be created."))
  (:method (node (rsn pdsh+ie-reason) interacting-nodes inference pds)
	   (declare (ignore inference))
	   (let* ((orig-step (pdsh~originator-step node))
		  (influencing-steps (pdsh~influencing-steps (cons node interacting-nodes)))
		  (latest-influencing-step (when influencing-steps (first (last influencing-steps))))
		  (latest-interacting-step (pdsh~latest-reason-in-list (mapcar 'pdsh~originator-step
									       interacting-nodes))))
	     (cond (latest-influencing-step (back~retract-step latest-influencing-step 'retract
							       (union (list node) interacting-nodes) pds))
		   ((not latest-interacting-step) (back~retract-step orig-step 'retract nil pds))
		   ((pdsh~reason-later-p latest-interacting-step orig-step)
		    (back~retract-step orig-step 'retract interacting-nodes pds))
		   ((pdsh~reason-eq-or-later-p orig-step latest-interacting-step)
		    (back~retract-step latest-interacting-step 'retract
				       (remove-if #'(lambda (interacting-node)
						      (eq latest-interacting-step
							  (pdsh~originator-step interacting-node)))
						  (union (list node) interacting-nodes))
				       pds)))))
  (:method (node (rsn pdsh+ia-reason) interacting-nodes inference pds)
	   (declare (ignore pds))
	   (back~trace "         Add failed step to node ~A with interacting nodes ~A" node interacting-nodes)
	   ;; the inference can not be computed from the reason because the justification
	   ;; of the reason was changed destructivly in step 3
	   (pdsh~fs-add node rsn (mapcar 'pdsh~originator-step interacting-nodes) inference)
	   (values nil nil nil nil)))

(defgeneric back=update-supports (node rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node, a reason and a pds.")
	   (effect  "Updates the supports of node reestablishing the state before rsn.")
	   (value   "The mor nodes that are added as supports."))
  (:method (node (rsn pdsh+ie-reason) pds)
	   (declare (ignore node pds))
	   (back~trace "         Nothing to do.")
	   nil)
  (:method (node (rsn pdsh+ia-reason) pds)
	   (declare (ignore pds))
	   (let ((add-concs (pdsh~ia-add-conclusions rsn))
		 (delete-supps (pdsh~ia-delete-supports rsn)))
	     (when (subsetp add-concs (pdsn~supports node))
	       (back~trace "         Remove supports ~A" add-concs)
	       (pdsn~remove-supports node add-concs)
	       (back~trace "         Add supports ~A" delete-supps)
	       (pdsn~add-new-supports node delete-supps)
	       (remove-if-not 'pdsn~schematic-p delete-supps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 5 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 5 Of The Backtracking Algorithm}
#}

(defun back=reset-influenced-steps (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason and a pds.")
	   (effect  "Resets all influenced steps.")
	   (value   "A tuple:"
		    "- A boolean value, that is true iff a instantiating step was backtracked."
		    "- A list of added mor support nodes."
		    "- The consumer nodes for that new tasks have to be created and whose steps are followed by an"
		    "  instantiating step."
		    "- The other consumer nodes for that new tasks have to be created."))
  (misc~remove-from-slot (pds~constraint-store pds) (pdsh~reason-new-metavars rsn))
  (when (pdsh~mor-step-p rsn)
    (let ((inst-step-p nil)
	  (add-mor-supports nil)
	  (new-task-nodes-followed-by-inst nil)
	  (new-task-nodes-other nil))
      ;;; Achtung: Sortiere Steps nach Zeit, letzter zuerst
      (dolist (influenced-step (reverse (pdsh~sort-reasons (pdsh~influenced-steps rsn))))
	(multiple-value-bind (inst-step-add-p add-mor-supports-add new-task-nodes-followed-by-inst-add
					      new-task-nodes-other-add)
	    (back~retract-step influenced-step 'reset nil pds)
	  ;; collect the return values
	  (setf inst-step-p (or inst-step-p inst-step-add-p)
		add-mor-supports (union add-mor-supports add-mor-supports-add)
		new-task-nodes-followed-by-inst (union new-task-nodes-followed-by-inst
						       new-task-nodes-followed-by-inst-add)
		new-task-nodes-other (union new-task-nodes-other new-task-nodes-other-add))))
      (pds~set-ancestor-mor-scopes rsn pds)
      (values inst-step-p add-mor-supports new-task-nodes-followed-by-inst new-task-nodes-other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 6 of the backtracking algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step 6 Of The Backtracking Algorithm}
#}

(defgeneric back=delete-reason (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "Removes the reason from the pds history and all nodes involved in this reason.")
	   (value   "Undefined."))
  (:method ((rsn pdsh+ie-reason) pds)
	   (dolist (node (append (pdsh~ie-expansion-origin-nodes rsn)
				 (pdsh~ie-expansion-leaves rsn)))
	     (misc~remove-from-slot (pdsn~reasons node) rsn))
	   (pds~remove-reason-from-history rsn pds))
  (:method ((rsn pdsh+ia-reason) pds)
	   (dolist (node (append (append (pdsh~ia-primary-goals rsn)
					 (pdsh~ia-secondary-goals rsn))
				 (pdsh~ia-used-supports rsn)))
	     (misc~remove-from-slot (pdsn~reasons node) rsn))
	   (pds~remove-reason-from-history rsn pds)))

