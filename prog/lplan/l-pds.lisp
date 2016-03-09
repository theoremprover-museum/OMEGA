;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-

(in-package "KEIM")

(defun pds~all-reasons (pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A pds.")
	   (effect  "None.")
	   (value   "A list of all reasons in the pds."))
  (when (pds~first-plan-step pds)
    (cons (pds~first-plan-step pds)
	  (pdsh~reasons-later-than (pds~first-plan-step pds)))))

(defun pds~insert-reason-as-last (rsn pds)
  (setf (pds~last-plan-step pds) rsn)
  (let ((old-last-rsn (pdsh~reason-predecessor rsn)))
    (when old-last-rsn
      (setf (pdsh~reason-successor old-last-rsn) rsn))
    (when (not (pds~first-plan-step pds))
      (setf (pds~first-plan-step pds) rsn))
    rsn))
  
(defun pds~ia-create (conclusions secondary-goals method outline-patterns new-metavars new-nodes pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A task, a refinement and a pds.")
	   (effect  "Creates a reason and inserts it in the pds as last reason.")
	   (value   "The newly created reason."))
  (let ((rsn (make-instance 'pdsh+ia-reason
			    :conclusions conclusions
			    :secondary-goals secondary-goals
			    :ref method
			    :outline-patterns outline-patterns
			    :new-metavars new-metavars
			    :new-nodes new-nodes
			    :predecessor (pds~last-plan-step pds))))
    (pds~insert-reason-as-last rsn pds)))

(defun pds~ie-create (expansion-origin-nodes expanded-just new-nodes pds)
  (let ((rsn (make-instance 'pdsh+ie-reason
			    :expansion-origins (mapcar #'(lambda (node) (list node expanded-just))
						       expansion-origin-nodes)
			    :new-nodes new-nodes
			    :predecessor (pds~last-plan-step pds))))
    (pds~insert-reason-as-last rsn pds)))

(defun pds~pr-create (refinement input-nodes parameters mor-effect new-nodes pds)
  (let ((rsn (make-instance 'pdsh+pr-reason
			    :refinement refinement
			    :input-nodes input-nodes
			    :parameters parameters
			    :mor-effect mor-effect
			    :new-nodes new-nodes
			    :predecessor (pds~last-plan-step pds))))
    (pds~insert-reason-as-last rsn pds)))

(defun pds~remove-mor-scopes (mor-scopes pds)
  (misc~remove-from-slot (pds~constraint-store pds) mor-scopes))

(defun pds~add-mor-scopes (mor-scopes pds)
  (misc~append-to-slot (pds~constraint-store pds) mor-scopes))

(defun pds~insert-mor-scope (mor-scope new-metavars pds)
  (when mor-scope
    (pds~remove-mor-scopes (pdsh~ms-ancestor-scopes mor-scope) pds)
    (pds~add-mor-scopes (list mor-scope) pds))
  (pds~add-mor-scopes (set-difference new-metavars (pdsh~domain mor-scope)) pds)) 

(defun pds~set-ancestor-mor-scopes (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason and a pds.")
	   (effect  "Replaces the mor effect of rsn with the ancestor mor scopes of reason in the pds.")
	   (value   "Undefined."))
  (pds~remove-mor-scopes (list (pdsh~reason-mor-effect rsn)) pds)
  (pds~add-mor-scopes (pdsh~ancestor-mor-scopes (pdsh~reason-mor-effect rsn)) pds)
  (setf (pdsh~reason-mor-effect rsn) nil))

(defun pds~remove-reason-from-history (rsn pds)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "Removes the reason from the history connecting its predecessor"
		    "and successor directly.")
	   (value   "Undefined."))
  (when (pdsh~reason-predecessor rsn)
    (setf (pdsh~reason-successor (pdsh~reason-predecessor rsn)) (pdsh~reason-successor rsn)))
  (when (pdsh~reason-successor rsn)
    (setf (pdsh~reason-predecessor (pdsh~reason-successor rsn)) (pdsh~reason-predecessor rsn)))
  (when (eq rsn (pds~first-plan-step pds))
    (setf (pds~first-plan-step pds) (pdsh~reason-successor rsn)))
  (when (eq rsn (pds~last-plan-step pds))
    (setf (pds~last-plan-step pds) (pdsh~reason-predecessor rsn)))
  (setf (pdsh~reason-predecessor rsn) nil)
  (setf (pdsh~reason-successor rsn) nil))

(defun pds~delete-nodes (nodes pds)
  ;;; no set-difference, because th order of the pds nodes must stay the same
  (setf (prob~proof-steps pds) (remove-if #'(lambda (node) (find node nodes))
					  (prob~proof-steps pds)))
  (setf (pds~open-nodes pds) (set-difference (pds~open-nodes pds) nodes))
  (dolist (node nodes)
    (remhash (symbol-name (keim~name node)) (pds~label-node-hashtable pds))))

(defun pds~reopen-nodes (nodes pds)
  (dolist (node nodes)
    (setf (node~justification node) (pdsj~open-just-create (pdsj~control (node~justification node))))
    (nconc (pds~open-nodes pds) nodes)))

(defun pds~cstore-bindings (constraint-store)
  (let ((cstr-states (mapcar 'pdsh~ms-cstr-state
			     (remove-if-not 'pdsh~mor-scope-p constraint-store))))
    (when cstr-states
      (reduce 'subst~disjoint-compose-substitution-nso (mapcar 'pdsh~cs-bindings cstr-states)))))

; (defgeneric pds~node-formula (node &optional pds)
;   (declare (edited  "14-APR-1999")
; 	   (authors Lassaad)
; 	   (input   "A pds NODE, and optionally a PDS.")
; 	   (effect  "When NODE is schematic and its current formula is not up-to-date,"
; 		    "then updates the current formula of NODE using the current bindings"
; 		    "of the meta-variables in PDS.")
; 	   (value   "The up-to-date formula of node."))
;   (:method ((schematic-node pdsn+schematic-node) &optional (pds pds*current-proof-plan))
; 	   (unless (pdsn~up-to-date schematic-node)
; 	     (let ((subst (when (pds~constraint-store pds) (pds~cstore-bindings (pds~constraint-store pds)))))
; 	       (setf (pdsn~current-formula schematic-node)
; 		     (if subst 
; 			 (beta~normalize (subst~apply subst (node~formula schematic-node)))
; 		       (node~formula schematic-node))
; 		     (pdsn~up-to-date schematic-node) T)))
; 	   (pdsn~current-formula schematic-node))
;   (:method ((pdsn pdsn+node) &optional pds)
; 	   (declare (ignore pds))
; 	   (node~formula pdsn))
;   )

; (defun pds~update-schematic-nodes! (&optional (pds pds*current-proof-plan) solved-task new-opens)
;   (declare (edited  "14-APR-1999")
; 	   (authors Lassaad)
; 	   (input   "A pds, and optionally a solved task, and a list of new open nodes.")
; 	   (effect  "Sets the up-to-date flag in the schematic support nodes to NIL, and"
; 		    "adapts the slot schematic-p of the goal-schemas in the PDS agenda.")
; 	   (value   "A pair: - A list of tasks whose formulas did not contain meta-variables"
; 		    "          before the updating"
; 		    "        - A list of schematic tasks whose formulas become closed by the"
; 		    "          updating"))
;   (let* ((all-tasks (agenda~all-tasks (pds~agenda pds)))
; 	 (all-node-tasks (remove-if #'agenda~inst-task-p all-tasks))
; 	 (tasks (remove-if #'agenda~inst-task-p 
; 			   (remove solved-task all-tasks)))
; 	 (mvar-subst (when (pds~constraint-store pds) (pds~cstore-bindings (pds~constraint-store pds))))
; 	 (task-supports (apply #'append (mapcar #'(lambda (task) (pdsn~supports (agenda~task-node task)))
; 						(if new-opens all-node-tasks tasks)
; 						;; When there is new subgoals, i.e., NEW-OPENS is not NIL, then we have to reset
; 						;; the schematic supports of all tasks, since the supports of SOLVED-TASK are
; 						;; inherited to NEW-OPENS. Otherwise, only the schematic supports of the remaining
; 						;; tasks are reset
; 						))))
;     (when mvar-subst
;       ;; Sets the up-to-date flag in the schematic support nodes to NIL
;       (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
; 	    (remove-if-not #'pdsn~schematic-p task-supports))
;       ;; Fetch the closed tasks before applying the new meta-variable bindings:
;       (let* ((goal-schemas (remove-if-not #'agenda~goal-schema-p tasks))
; 	     (before-closed (append (remove-if-not #'agenda~goal-p (set-difference tasks goal-schemas))
; 				    (remove-if #'agenda~goal-schematic-p goal-schemas)))
; 	     (after-closed-candidates (remove-if-not #'agenda~goal-schematic-p goal-schemas))
; 	     after-closed)
; 	;; Sets the up-to-date flag in the schematic task nodes to NIL
; 	(mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
; 	      (mapcar #'agenda~task-node after-closed-candidates))
	
; 	(dolist (goal-schema after-closed-candidates)
; 	  ;; - Fetching some goal-schema SG: when goal-schematic-p(SG) is set to T, then consider the node(SG):
; 	  ;; When the up-to-date of this node is NIL, then compute the current-formula; set up-to-date to T, and
; 	  ;; set goal-schematic-p(SG) to NIL, when the resulted current-formula does not contain meta-variables
; 	  (let ((gs-node (agenda~task-node goal-schema)))
; 	    (unless (pdsn~up-to-date gs-node)
; 	      (misc~remove-from-slot (pdsn~s-inapplicable-methods gs-node) (pdsn~g-inapplicable-methods gs-node))
; 	      (setf (pdsn~g-inapplicable-methods gs-node) nil)
; 	      (setf (pdsn~current-formula gs-node)
; 		    (if (remove-if-not #'data~abstr-p (subst~codomain mvar-subst))
; 			;; there are abstractions in the codomain of the mvar-subst
; 			;; -> we have to beta-normalize
; 			(beta~normalize (subst~apply mvar-subst (node~formula gs-node)))
; 		      ;; otherwise we can spare to beta-normalize (whcih is a verry complex operation)
; 		      (subst~apply mvar-subst (node~formula gs-node))))
; 	      (setf (pdsn~up-to-date gs-node) T)
; 	      (unless (remove-if-not #'meta~p (data~free-variables (pdsn~current-formula gs-node)))
; 		(setf (agenda~goal-schematic-p goal-schema) NIL)
; 		(push goal-schema after-closed)))))
; 	(dolist (task all-tasks)
; 	  (let ((task-node (agenda~task-node task)))
; 	    (dolist (support (pdsn~normal-supports task-node))
; 	      (when (and (pdsn~s-inapplicable-methods task-node) (not (pdsn~up-to-date support)))
; 		(misc~remove-from-slot (pdsn~normal-supports task-node) support)
; 		(misc~append-to-slot (pdsn~extra-supports task-node) support)))))
; 	(values before-closed after-closed))
;       )))

(defgeneric pds~start-proof-plan (problem name)              ;;; a very ugly function!!!!!
  (declare (edited  "11-JUN-1997" "23-SEP-1996" "28-JUL-92 10:00")
           (authors Sorge Lassaad NESMITH)
           (input   "A problem and a name.")
           (effect "Start a new proof plan with name NAME for PROBLEM. POBLEM should be of type"
		   "PROB+PROBLEM; NAME is a symbol. Sets pds*current-proof-plan to new proof plan"
		   "and returns it.")
           (value   "The new proof plan."))
  (:method ((problem prob+problem) name)
	   (let* ((new-proof (pds~proof-plan-create name))
		  (conc (prob~conclusion problem))
		  (assumptions (mapcar #'pds=node2pdsn
				       (prob~assumptions problem)))
		  (root (pdsn~create (keim~name conc)
				     assumptions
				     (node~formula conc)
				     (pdsj~open-just-create)))
		  (type-var-subst (env~lookup-object 'type-var-subst (prob~environment problem))))
	     (setf (pds~agenda new-proof) (lagenda~initial-agenda root))
	     (setf pds*current-proof-plan new-proof)
	     (setf (prob~proof-problem new-proof) problem)
	     (setf (prob~proof-theory new-proof) (prob~theory problem))
	     (setf (prob~proof-root new-proof) root)
	     (setf (pds~open-nodes new-proof) (list root))
	     (setf (pds~environment new-proof) (env~create (prob~environment problem) name))
	     (env~enter 'type-var-subst
			(if type-var-subst
			    (keim~copy type-var-subst :downto '(data+struct))
			  (subst~create nil nil))
			(pds~environment new-proof))
	     (setf data*global-type-var-subst (env~lookup-object 'type-var-subst (pds~environment new-proof)))
	     (prob~add-proof problem new-proof)
	     (setf (prob~proof-steps new-proof) (append assumptions (list root)))
	     (setf (pds~support-nodes new-proof) assumptions)
	     (setf (pdsn~normal-supports root) assumptions)
	     (dolist (x (cons root assumptions))
	       (setf (gethash (symbol-name (keim~name x)) (pds~label-node-hashtable new-proof)) x))
	     new-proof)))

(defun pds~mor-goal-p (task)
  (let ((task-node (lagenda~task-node task)))
    (when (or (pdsn~schematic-p task-node)
	      (some 'pdsn~schematic-p (pdsn~supports task-node)))
      t)))

(defun pds~mor-goals (pds)
  (remove-if-not 'pds~mor-goal-p (lagenda~tasks (pds~agenda pds))))

(defgeneric pds~mor-nodes (goals)
  (:method ((goals list))
	   (when goals
	     (append (pds~mor-nodes (first goals)) (pds~mor-nodes (rest goals)))))
  (:method ((goal lagenda+task))
	   (pds~mor-nodes (lagenda~task-node goal)))
  (:method ((goal pdsn+node))
	   (append (when (pdsn~schematic-p goal) (list goal)) (remove-if-not 'pdsn~schematic-p (pdsn~supports goal)))))

(defgeneric pds~node-formula (node &optional pds)
  (:method ((node pdsn+schematic-node) &optional pds)
	   (if (pdsn~current-formula node) (pdsn~current-formula node) (node~formula node)))
  (:method ((node pdsn+node) &optional pds)
 	   (node~formula node)))

(defgeneric pds~adapt-control-infos (mor-goals changed)
  (:method ((mor-goals list) changed)
	   (when mor-goals
	     (pds~adapt-control-infos (first mor-goals) changed)
	     (pds~adapt-control-infos (rest mor-goals) changed)))
  (:method ((mor-goal lagenda+task) changed)
	   (pds~adapt-control-infos (lagenda~task-node mor-goal) changed))
  (:method ((mor-goal pdsn+node) changed)
	   (when (find mor-goal changed)
	     (misc~remove-from-slot (pdsn~s-inapplicable-methods mor-goal) (pdsn~g-inapplicable-methods mor-goal))
	     (setf (pdsn~g-inapplicable-methods mor-goal) nil))
	   (when (pdsn~s-inapplicable-methods mor-goal)
	     (dolist (support (pdsn~normal-supports mor-goal))
	       (when (find support changed)
		 (misc~remove-from-slot (pdsn~normal-supports task-node) support)
		 (misc~append-to-slot (pdsn~extra-supports task-node) support))))))
