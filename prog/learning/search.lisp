(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures for the search mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lea+step ()
  ((method     :initarg :method
	       :initform nil
	       :accessor lea=method)
   (parameter  :initarg :parameter
	       :initform nil
	       :accessor lea=parameter)
   (matching   :initarg :matching
	       :initform nil
	       :accessor lea=matching)
   (task       :initarg :task
	       :initform nil
	       :accessor lea=task))
  (:documentation "Class for applied methods, stored in the history of current sequence."))

(defvar lea*stack nil
  "A stack containing the alternatives of our search in form of (sequence . history)")

(defvar lea*pds nil
  "A the pds for the current search.")

(defvar lea*star-bound 5
  "An number that is the upper bound for the repetition of the star operator.")

(defvar lea*tolearn nil
  "A list containing a sequences method applications of proofs, that may be learned.")

(defvar lea*outlines (list infer*existent infer*non-existent infer*closed infer*list)
  "An list with the possible entries in outline-patterens, needed for the extraction.")

(defvar lea*trace nil
    "An flag enabling tracing messages.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea~search-all (mo &key (star-bound  lea*star-bound))
  (do ((result (lea~search mo :star-bound star-bound)
	       (lea~search nil :star-bound star-bound))
       (all nil (cons result all)))
      ((null result) all)))
	 
(defun lea~search (&optional mo &key (star-bound  lea*star-bound))
  (when mo (lea=start mo))
  (let ((remember-pds omega*current-proof-plan))
    (setf omega*current-proof-plan lea*pds     
	  keim::pds*current-proof-plan lea*pds)
    (let ((result (loop
		   ;; no solution, no patterns on the stack
			(when (null lea*stack) (return nil))
		   ;; found a solution, remove from stack, backtrack to the situation of the next stack entry
			(when (null (caar lea*stack)) (let ((hist (rest (pop lea*stack))))
						       (lea=backtrack hist (rest (car lea*stack)))
						       (return (reverse hist))))
			(lea=next star-bound))))
      (setf omega*current-proof-plan remember-pds
	    keim::pds*current-proof-plan remember-pds)
    result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the search mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea=start (mo)
  (setf meth*restricting-methods nil
	meth*normalizing-methods nil
	lea*pds (agplan~copy-pds omega*current-proof-plan)
	lea*stack nil)
  (mapc #'(lambda (step) ;;update the pds-constraint
	    (let ((constr (meth~mapp-constraint(pdsj~subst (pdsc~an-just step)))))
	      (when (pds~constraint-pool-p constr)(pplan=insert-new-constraint-store! lea*pds constr))))
	(pds~plan-steps lea*pds))
  (push (cons (list mo) nil) lea*stack))

(defun lea=next (&optional (star-bound lea*star-bound))
  (let* ((entry (pop lea*stack))
	 (seq (first entry))
	 (hist (rest entry)))
    (labels ((pushtoseq (thing)
			(push (cons (append thing (rest seq)) hist) lea*stack))
	     (pushtohist (new)
			 (push (cons (rest seq) (cons new hist)) lea*stack)))
      (let ((action (car seq)))
	(etypecase action
	  (lea+step ;;matching: apply the method-matching
	   (if (lea=apply-method action)
	       (pushtohist action)
	     (omega~error "It was not possible to apply the step ~A" action)))
	  (symbol ;;method-name: produce the matchings
	   (let ((mms (lea=produce-method+matchings action nil)))
	     (if mms
		 (mapc #'(lambda (mm)(pushtoseq (list mm))) mms)
	       (lea=backtrack hist (rest (car lea*stack))))))
	  (list ;;decompose pattern
	   (case (car action)
	     ('conj (pushtoseq (rest action)));;old
	     ('disj (mapc #'(lambda (act) (pushtoseq (list act))) (reverse (rest action))))
	     ('exp (pushtoseq (make-list (car (last action)) :initial-element (butlast (rest action)))))
	     ('star (dotimes (i (1+ star-bound));;MP take star(stuff) to the buttom of lea*stack would allow infinite search
		      (pushtoseq (make-list (- star-bound i) :initial-element (rest action)))))
	     ('method (pushtoseq (rest action)));;old
	     (T (pushtoseq action)))))));;list-in-list-case 
    (lea=trace-stack lea*stack))	;lea*stack
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the method+matching for the stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea=produce-method+matchings (meth controlrules)
  (let* ((crs (or controlrules
		  (lea=get-controlrules meth)))
	 (opennodes (pds~open-nodes lea*pds))
	 paras
	 (mm (if crs
		 (mapcan #'(lambda (cr)
			     (lea=trace "processing ~A of ~A" cr crs)
			     (mapcan #'(lambda (open)
					 (lea=trace "processing ~A of ~A" open opennodes)
					 (let* ((parainsts (remove-if #'(lambda (para) (member para paras :test #'lea=eq))
								      (lea=cri-call meth cr open)))
						(result (mapcan #'(lambda (inst)
								    (lea=produce-mmatching meth open inst))
								    parainsts)))
						(lea=trace "~%Found ~A~%" result)(setf bla paras bla2 parainsts)
						(mapc #'(lambda (par) (push par paras)) parainsts) result))
					 opennodes)) crs)
	       (mapcan #'(lambda (open)
			   (lea=trace "processing ~A of ~A" open opennodes)
			   (let ((result (lea=produce-mmatching meth open)))
			     (lea=trace "~%Found ~A~%" result) result))
		       opennodes))))
    mm))

(defun lea=produce-mmatching (meth open &optional inst)
  (let* ((method (or (meth~find-method meth)
		     (omega~error "Method ~A does not exist!" meth)))
	 (matches (when method (plan~match-method method
						  open (node~formula open)			       
						  (pds~node-supports open lea*pds) lea*pds
						  :parameters inst))))
	 (when matches (mapcar #'(lambda (mat)
				   (make-instance 'lea+step :method meth :parameter inst :matching mat
						  :task (plan~find-task-for-goal-in-agenda open (pds~agenda lea*pds))))
			       matches))))

(defun lea=get-controlrules (meth)
  (let (useful)
    (maphash #'(lambda (x cr)
		 ;(lea=trace "~A" x)
		 (declare (ignore x))
		 (some #'(lambda (to-do)
			   ;(lea=trace "~A" to-do)
			   (some #'(lambda (single)
				     ;(lea=trace "~A" single)
				     (when (and (consp single)
						(string-equal meth (first single))
						(not (null (third single))))
				       (push (keim~name cr) useful)))
				 (when  (listp (second to-do)) (second to-do))))
		       (cri~to-do-part cr)))
	     cri*control-rules-hashtable)
    useful))

(defun lea=cri-call (meth cr open)
  (mapcar #'third
  	  (remove-if #'(lambda (answer) (or (atom answer)
					    (not (eq (car answer) meth))))
		     (cri~call (list meth)
			       :kind 'methods
			       :task (plan~find-task-for-goal-in-agenda open (pds~agenda lea*pds))
			       :task-node open 
			       :task-formula (node~formula open)
			       :agenda (pds~agenda lea*pds)
			       :crules (list cr)
			       :pds lea*pds))))

(defun lea=eq (a b)
  (or (eq a b)
      (and (listp a)(listp b)
	   (eq (car a)(car b))
	   (lea=eq (rest a)(rest b)))))
	   
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backtrack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea=backtrack (history until-history)
  (let ((until-node (when until-history (agenda~task-node (lea=task (car until-history))))))
    (do* ((hist history (rest hist))
	  (step (car hist)(car hist)))
	((if until-node (eq (agenda~task-node (lea=task step)) until-node)(null step)))
      (lea=open-bloody-node-special (agenda~task-node (lea=task step)) lea*pds))))

(defun lea=open-bloody-node-special (node &optional (pds lea*pds))
  ;;; Redefinition of oc=open in /top/omega-com.lisp:
  (lea=trace ";;; Opening node ~A ..." node)
  (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	(undone-steps (pds~open-node! node pds))
	(agenda (pds~agenda pds)))
    ;; Foci stuff:
    (multiple-value-bind (meth-nodes other-nodes pseudo pds-rootp)  ;; 4! values: pseudo MP
	(lea=consistent-opens! cstrsteps2succs undone-steps nil nil nil pds)
      (lea=trace "opens!: meth-nodes ~A other-nodes ~A pseudo ~A pds-rootp" meth-nodes other-nodes pseudo pds-rootp)
      (if other-nodes
	  ;;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
	  (let ((extra-supps))
	    (dolist (each-node other-nodes)
	      (setq extra-supps (union extra-supps
				       (plan=set-difference (pdsn~just-sponsors each-node)
						       (pdsn~just-unsponsors each-node)))))
	    (multiple-value-bind (the-agenda rest-opens)
		(plan=normalize-supports extra-supps other-nodes agenda pds)
	      (lea=trace "normalize: agenda ~A rest-opens ~A" the-agenda rest-opens)
	      ;;; Creates new tasks
	      (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		     (new-tasks (if pds-rootp
				    (cons root-new-task
					  (append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
						  (mapcar #'agenda~create-goal rest-opens)))
				  (append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
					  (mapcar #'agenda~create-goal rest-opens)))))
		(when new-tasks
		  (if (or (agenda~empty-p the-agenda) (agenda~first-task the-agenda))
		      (setq the-agenda (agenda~create nil new-tasks nil the-agenda))
		    (let ((next-tasks (agenda~next-tasks the-agenda)))
		      (setf (agenda~next-tasks the-agenda) (append new-tasks next-tasks))
		      (when next-tasks
			(setf (agenda~orderings the-agenda)
			      (cons (agenda~ordering-create new-tasks next-tasks)
				    (agenda~orderings the-agenda)))))))
	        ;;; Adapt the PDS agenda:
		(setf (pds~agenda pds)
		      (agenda~update the-agenda (prob~proof-steps pds)))
	        ;;; Return root task and new agenda:
		(values root-new-task (pds~agenda pds)))))
	(if (or meth-nodes pseudo pds-rootp)
	    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		   (new-tasks (if pds-rootp
				  (cons root-new-task
					(mapcar #'agenda~create-open-task (append meth-nodes pseudo)))
				(mapcar #'agenda~create-open-task (append meth-nodes pseudo)))))
	      (if (or (agenda~empty-p agenda) (agenda~first-task agenda))
		  (setq agenda (agenda~create nil new-tasks nil agenda))
		(let ((next-tasks (agenda~next-tasks agenda)))
		  (setf (agenda~next-tasks agenda) (append new-tasks next-tasks))
		  (when next-tasks
		    (setf (agenda~orderings agenda)
			  (cons (agenda~ordering-create new-tasks next-tasks)
				(agenda~orderings agenda))))))
	      ;;; Adapt the PDS agenda:
	      (setf (pds~agenda pds)
		    (agenda~update agenda (prob~proof-steps pds)))
	      ;;; Return root task and new agenda:
	      (values root-new-task (pds~agenda pds)))
	  (progn
	    ;;; Adapt the PDS agenda:
	    (setf (pds~agenda pds)
		  (agenda~update agenda (prob~proof-steps pds)))
	    ;;; Return root task and new agenda:
	    (values nil (pds~agenda pds))))))
    ))

(defun lea=consistent-opens! (cstrstps2succs undone-stps redo-stps passed-onodes passed-cnodes pds
					      &optional (pds-nodes (prob~proof-steps pds)) 
					      (cstrpool (pds~constraint-pool pds)))
  ;;; The additional argument cstrstp2succ is needed because the successors of undone constraint steps are
  ;; lost during the deletion of the associated justifications and nodes.
  ;; For instance, when before opening/deleting some nodes the constraint pool had the following steps
  ;; (<s1> <s4> <s5> <s7>), where the last planning step in PDS was s9, cstrstps2succs must be 
  ;; (<s7 s8 s9> <s5 s6 s7> <s4 s5> <s1 s2 s3 s4>)
  (declare (edited  "18-APR-1998")
	   (authors Lassaad)
	   (input   "Two lists of backtracked plan steps, open nodes whose control informations were adapted"
		    "by the caller of this function, a pds, its current node list, and its current constraint"
		    "pool.")
	   (effect  "Changes the current CSTR-POOL of PDS when elements of UNDONE-STPS belong to CSTRPOOL-STPS,"
		    "updates the alternative lists of some open nodes in PDS, (for the associated nodes of"
		    "REDO-STPS, the control information list are set to NIL) and deletes some of the remaining"
		    "open nodes which may not have an empty alternative method list. A node may not have an empty"
		    "alternative list iff it was closed by a planning method and the associated step belongs to"
		    "UNDONE-STPS.")
	   (value   "A tuple: two list of open nodes which always belong to PDS and which have consistent control"
		    "informations. The first list contains the nodes which were closed by planning methods, and"
		    "the second list contains the nodes which were closed by tactics, rules, or bboxes."
		    "A third of list of nodes closed by unreliable methods."
		    "Moreover the pds root node as a third value when it is involved in the backtracking step."))
  (if undone-stps
      (if cstrpool
	  ;;; We have to check that no plan step on which CSTRPOOL depends belongs to UNDONE-STPS:
	  (let ((cstrstp&succs (find-if #'(lambda (stp-succs) (find (first stp-succs) undone-stps))
					cstrstps2succs)))
	    (if cstrstp&succs
		;;; At least one undone step corresponds to a constraint pool step, e.g., s5
		;; (cstrstp&succs is <s5 s6 s7>): This step must be undone with its successors <s9 s8 s7 s6 s5>
		;; and the rest of the cstrstps2succs (<s4 s5> <s1 s2 s3 s4>)
		(let* ((cstrstp&succs-pos (position cstrstp&succs cstrstps2succs :test #'equal))
		       (cstrstp&other-succs (subseq cstrstps2succs 0 cstrstp&succs-pos))
		       (all-to-undo-stps (append
					  (apply #'append (mapcar #'(lambda (stp-succs) (reverse (rest stp-succs)))
								  cstrstp&other-succs))
					  (reverse cstrstp&succs)))
		       (to-redo-stps (butlast all-to-undo-stps))
		       (to-pass-stp (first (last all-to-undo-stps)))
		       (rest-cstrstps2succs (subseq cstrstps2succs (+ 1 cstrstp&succs-pos))))
		  ;;; For the example, we must get:
		  ;; cstrstp&succs-pos = 1; cstrstp&other-succs = (<s7 s8 s9>)
		  ;; all-to-undo-stps = (s9 s8 s7 s6 s5); rest-cstrstps2succs = (<s4 s5> <s1 s2 s3 s4>)
		  ;; to-redo-stps = (s9 s8 s7 s6); to-pass-stp = s5
		  ;; The steps s9, s8, s7, and s6 must be taken back and in the recursive call of
		  ;; lea=consistent-opens! must be considered as redo-stps, which has to be NIL as
		  ;; control information. The step s5 must be taken back and must be considered as a
		  ;; passed node, the control information of s5 must be preserved.
		  (let ((new-redo-stps redo-stps)
			(new-undone-stps (remove to-pass-stp undone-stps)))
		    (dolist (to-redo-stp to-redo-stps)
		      (let ((to-redo-stp-node (pdsc~an-node to-redo-stp)))
			(when (find to-redo-stp-node (prob~proof-steps pds))
			  (cond ((pdsn~open-node-p to-redo-stp-node)
				 (push to-redo-stp new-redo-stps))
				((plan=expansion-step-p to-redo-stp)
				 ;; Method expansion: take back this expansion
				 (let ((back-stps (if (eq (node~justification (pdsc~an-node to-redo-stp))
							  (pdsc~an-just to-redo-stp))
						      (plan=take-back-expansion! to-redo-stp pds)
						    (list to-redo-stp))))
				   (push to-redo-stp new-redo-stps)
				   (setq new-undone-stps (plan=set-difference
							  (union new-undone-stps (remove to-redo-stp back-stps))
							  new-redo-stps))))
				(T ;; Method application: take back this application
				 (let ((back-stps (pds~open-node! to-redo-stp-node pds)))
				   (push to-redo-stp new-redo-stps)
				   (setq new-undone-stps (plan=set-difference
							  (union new-undone-stps (remove to-redo-stp back-stps))
							  new-redo-stps))))))
			(when (find to-redo-stp (pds~cstrpool-plansteps (pds~constraint-pool pds)))
			  (setf (pds~constraint-pool pds) (pds~cstrpool-previous (pds~constraint-pool pds))))))
		    ;; Change the constraint pool of PDS
		    (setf (pds~constraint-pool pds) (pds~cstrpool-previous (pds~constraint-pool pds)))
		    ;; Possibly change the control information of to-pass-stp, and recursively call
		    ;; lea=consistent-opens! after taking CSTR-STP-NODE to the PASSED-ONODES or PASSED-CNODES
		    (cond ((plan=expansion-step-p to-pass-stp)
			   (lea=consistent-opens! rest-cstrstps2succs
						   (remove-duplicates new-undone-stps)
						   (remove-duplicates new-redo-stps)
						   passed-onodes
						   (cons (pdsc~an-node to-pass-stp) passed-cnodes)
						   pds))
			  (T
			   (let ((cstr-stp-node (pdsc~an-node to-pass-stp)))
			     (cond ((find cstr-stp-node passed-onodes)
				    (lea=consistent-opens! rest-cstrstps2succs
							    (remove-duplicates new-undone-stps)
							    (remove-duplicates new-redo-stps)
							    passed-onodes passed-cnodes pds))
				   (T
				    (let ((cstr-stp-just (pdsc~an-just to-pass-stp)))
				      ;; LC: Changing the control of CSTR-STP-NODE (a node closed in a planning step 
				      ;; which affected the constraint pool and this planning step belongs to the undone
				      ;; steps, therefore we have to change the control of this node)
				      ;; 1) When alter-mmatchs is not empty:
				      ;;  The control remains the same 
				      ;; 2) When alter-mmatchs is NIL:
				      ;;  failed-methods(CSTR-STP-NODE) := failed-methods U {now-method}
				      ;;  alter-methods  and alter-mmatchs remain the same
				      ;; LC-REMARK1: There is no matter, if CSTR-STP-NODE cannot be closed, i.e, neither
				      ;; alternative mmatchings nor alternative methods are available. This situation is considered
				      ;; in the recursive call of the planner, where considering the associated task of CSTR-STP-NODE
				      ;; will invoke a backtracking step. There, the conflict is solved by either deleting the node
				      ;; CSTR-STP-NODE or taking another step which affected the constraint pool.
				      (unless (pdsn~alternative-mmatchings cstr-stp-node)
					(multiple-value-bind (outln-pat)
					    (plan=actual-outline-pattern cstr-stp-node pds cstr-stp-just)
					  (let ((the-method (pds~inference-application (just~method cstr-stp-just) outln-pat)))
					    (setf (pdsn~failed-methods cstr-stp-node)
						  (cons (if (pdsj~parameters cstr-stp-just)
							    (cons the-method (pdsj~parameters cstr-stp-just))
							  the-method)
							(pdsn~failed-methods cstr-stp-node))))))
				      (lea=consistent-opens! rest-cstrstps2succs
							      (remove-duplicates new-undone-stps)
							      (remove-duplicates new-redo-stps)
							      (cons (pdsc~an-node to-pass-stp) passed-onodes)
							      passed-cnodes pds)))))))))
	      ;;; No one of the backtracked steps affected the constraint pool, then make a recursive call
	      ;; with a nil as constraint pool
	      (lea=consistent-opens! cstrstps2succs undone-stps redo-stps passed-onodes passed-cnodes pds pds-nodes nil)))
	;;; No plan step on which CSTRPOOL depends, belongs to UNDONE-STPS:
	(let* ((stp (first undone-stps))
	       (stp-node (pdsc~an-node stp)))
	  (if (find stp-node pds-nodes)
	      (cond ((pdsn~open-node-p stp-node) 
		     ;; LC-REMARK2: The planning step which closed STP-NODE and which is now taken back, did not affect
		     ;; the constraint pool. Therefore, in case the node STP-NODE cannot be closed, i.e, neither
		     ;; alternative mmatchings nor alternative methods are available, this node must be deleted,
		     ;; in contrast to the case of a planning step affecting the constraint pool (see LC-REMARK1 above). 
		     (if (or (pdsn~alternative-mmatchings stp-node) (pdsn~alternative-methods stp-node))
			 ;; STP-NODE was closed by a method, it is now open and has some alternative methods:
			 (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
			     (lea=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps
						     passed-onodes passed-cnodes pds pds-nodes nil)
			   (if (pdsn~alternative-mmatchings stp-node)
			       ;; Because of alternative mmatchings, we dont have to adapt the control info
			       (values (cons stp-node meth-nodes) other-nodes pseudog-nodes pds-rootp)
			     (let ((stp-inference (just~method (pdsc~an-just stp))))
			       (multiple-value-bind (outln-pat)
				   (plan=actual-outline-pattern stp-node pds (pdsc~an-just stp))
				 (let ((the-method (pds~inference-application stp-inference outln-pat)))
				   (setf (pdsn~failed-methods stp-node)
					 (cons (if (pdsj~parameters (pdsc~an-just stp))
						   (cons the-method (pdsj~parameters (pdsc~an-just stp)))
						 the-method)
					       (pdsn~failed-methods stp-node))))
				 (progn (lea=trace "alternative matchings")
					(values (cons stp-node meth-nodes) other-nodes pseudog-nodes pds-rootp))))))
		       ;; STP-NODE has no alternative method:
		       (let ((stp-inference (just~method (pdsc~an-just stp))))
			 (if (infer~method-p stp-inference)
			     ;; STP-NODE was closed by a method, it is now open and has no alternative method,
			     ;; this must be deleted. When this node corresponds to the PDS root node then this
			     ;; node cannot be deleted, but we have to signal that the PDS root node was involved
			     ;; in this backtracking process by returning it as a third value of this function.
			     ;; Otherwise, after deleting this node, we have to consider the constraint pool in
			     ;; the recursive call, because of new undone steps:
			     (if (eq stp-node (prob~proof-root pds))
				 (multiple-value-bind (meth-nodes other-nodes pseudog-nodes)
				     (lea=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps
							     passed-onodes passed-cnodes pds pds-nodes nil)
				   ;; Before returning the pds root, we add the method taken back to its
				   ;; failed method list:
				   (let ((stp-inference (just~method (pdsc~an-just stp))))
				     (multiple-value-bind (outln-pat)
					 (plan=actual-outline-pattern stp-node pds (pdsc~an-just stp))
				       (let ((the-method (pds~inference-application stp-inference outln-pat)))
					 (setf (pdsn~failed-methods stp-node)
					       (cons (if (pdsj~parameters (pdsc~an-just stp))
							 (cons the-method (pdsj~parameters (pdsc~an-just stp)))
						       the-method)
						     (pdsn~failed-methods stp-node))))))
				   (progn (lea=trace "Method on root node")
					  (values meth-nodes other-nodes pseudog-nodes stp-node)))
			       ;(let ((additional-stps (pds~delete-node! stp-node nil pds)))
			       (progn (lea=trace "method, no matchings")
				      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					  (lea=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps
								passed-onodes passed-cnodes pds pds-nodes nil)
					(values  (cons stp-node meth-nodes) other-nodes pseudog-nodes pds-rootp))))
			   ;; STP was the application of a rule, a tactic, or a bbox: First try to apply
			   ;; task-restricting methods to STP-NODE with the supports: pds-supports and
			   ;; sponsors(STP-NODE). When STP-NODE cannot be closed, then return it:
			   (multiple-value-bind (a-mmatching)
			       (plan=apply-first-method pds stp-node (node~formula stp-node)
							(append (pds~support-nodes pds) (pdsn~just-sponsors stp-node))
							nil nil meth*restricting-methods)
			     (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
				 (lea=consistent-opens! cstrstps2succs (rest undone-stps)
							 redo-stps passed-onodes passed-cnodes pds pds-nodes nil)
			       (if a-mmatching
				   ;; STP-NODE is closed, no need to return it
				   (progn (lea=trace "No method, matching")
					  (values meth-nodes other-nodes pseudog-nodes pds-rootp))
				 (progn (lea=trace "No method, no matching")
					(values meth-nodes (cons stp-node other-nodes) pseudog-nodes pds-rootp)))))))))
		    (T ;; STP corresponds to a method expansion: return STP-NODE with the meth-nodes, when the method
		       ;; justifying STP-NODE is non-reliable.
		     (let ((exp-inference (just~method (node~justification stp-node))))
		       (cond ((infer~method-p exp-inference)
			      (multiple-value-bind (outln-pat)
				  (plan=actual-outline-pattern stp-node pds (node~justification stp-node))
				(let ((the-method (pds~inference-application exp-inference outln-pat)))
				  (if (meth~non-reliability the-method)
				      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					  (lea=consistent-opens! cstrstps2succs (rest undone-stps)
								  redo-stps passed-onodes passed-cnodes pds pds-nodes nil)
					(progn (lea=trace "Method expansion")
					       (values meth-nodes other-nodes (cons stp-node pseudog-nodes) pds-rootp)))
				    (lea=consistent-opens! cstrstps2succs (rest undone-stps)
							    redo-stps passed-onodes passed-cnodes pds pds-nodes nil)))))
			     (T
			      (lea=consistent-opens! cstrstps2succs (rest undone-stps)
						      redo-stps passed-onodes passed-cnodes pds pds-nodes nil))))))
	    ;;; STP-NODE occurs no more in PDS: 
	    (lea=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps passed-onodes passed-cnodes pds pds-nodes nil))))
    ;;; The first UNDONE-STPS are processed, now we have to process the second undone REDO-STPS:
    (if redo-stps
	(let ((stp-node (pdsc~an-node (first redo-stps))))
	  (if (find stp-node pds-nodes)
	      (cond ((pdsn~open-node-p stp-node)
		     (let ((stp-inference (just~method (pdsc~an-just (first redo-stps)))))
		       ;; Reset the control information of STP-NODE
		       (setf (pdsn~failed-methods stp-node) nil)
		       (setf (pdsn~alternative-mmatchings stp-node) nil)
		       (setf (pdsn~alternative-methods stp-node) nil)
		       (multiple-value-bind (meth-nodes other-nodes pseudog-nodes)
			   (lea=consistent-opens! cstrstps2succs nil (rest redo-stps) passed-onodes passed-cnodes pds pds-nodes nil)
			 (if (infer~method-p stp-inference)
			     (progn (lea=trace "redo, method")
				    (values (cons stp-node meth-nodes) other-nodes pseudog-nodes))
			     (progn (lea=trace "redo, no method")
				    (values meth-nodes (cons stp-node other-nodes) pseudog-nodes))))))
		    (T ;; STP corresponds to a method expansion: return STP-NODE with the meth-nodes, when the method
		       ;; justifying STP-NODE is non-reliable.
		     (let ((exp-inference (just~method (node~justification stp-node))))
		       (cond ((infer~method-p exp-inference)
			      (multiple-value-bind (outln-pat)
				  (plan=actual-outline-pattern stp-node pds (node~justification stp-node))
				(let ((the-method (pds~inference-application exp-inference outln-pat)))
				  (if (meth~non-reliability the-method)
				      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					  (lea=consistent-opens! cstrstps2succs nil (rest redo-stps)
								  passed-onodes passed-cnodes pds pds-nodes nil)
								     (progn (lea=trace "redo, expansion")
									    (values meth-nodes other-nodes (cons stp-node pseudog-nodes) pds-rootp)))
				    (lea=consistent-opens! cstrstps2succs nil (rest redo-stps)
							    passed-onodes passed-cnodes pds pds-nodes nil)))))
			     (T
			      (lea=consistent-opens! cstrstps2succs nil (rest redo-stps)
						      passed-onodes passed-cnodes pds pds-nodes nil))))))
	    (lea=consistent-opens! cstrstps2succs nil (rest redo-stps) passed-onodes passed-cnodes pds pds-nodes nil)))
      ;;; Now consider the passed-nodes:
      (progn (lea=trace "after processing")
      (values (intersection passed-onodes pds-nodes)
	      NIL
	      (set-difference (intersection passed-cnodes pds-nodes) passed-onodes))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply the method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea=repair-outlines (mmatching task open supp)
  (declare (ignore task))
  (let ((newclosed (remove-if #'pdsn~hypothesis-p supp)))
    (when (plan~mmatch-goal mmatching) (push (plan~mmatch-goal mmatching) newclosed))
    (dolist (new newclosed)
      (let* ((just (node~justification new))
	     (premnodes (just~premises just))
	     (premoutline (subseq (pdsj~outline-pattern just) (length newclosed))))
	(if (= (length premnodes)(length premoutline))
	    (lea=trace "~%right! ~A ~A" premnodes premoutline)
	  (let ((newoutline (maplist #'(lambda (prem)
					 (if (member (car prem) open) infer*non-existent
					   (nth (- (length premnodes) (length premnodes)) premoutline))) premnodes)))
	    (setf (pdsj~outline-pattern just)
		  (append (subseq premoutline 0 (length newclosed)) newoutline))
	    (lea=trace "~%wrong! ~A ~A -> ~A" premnodes premoutline newoutline)))))))

;;MP remove normalizing/restricting methods?
(defgeneric lea=apply-method (meth)
  (:method ((meth lea+step))
	   (let* ((pds lea*pds)
		  (agenda (pds~agenda pds))
		  (task ;;MP: we have a matching for an 'old' goal, get now the corresponding actual
		   (car (agenda~get-tasks agenda   
					  #'(lambda (task) (eq (agenda~task-node task)(agenda~task-node (lea=task meth)))))))
		  (task-node (agenda~task-node task)))
	     (multiple-value-bind (mmatching new-opens new-supps)
		 (plan~apply-mmatching task (lea=matching meth) lea*pds)
	       (lea=repair-outlines mmatching task new-opens new-supps)
	  ;;; One method was successfully applied to the TASK-NODE:
	       (let ((applied-method (plan~matched-method mmatching))
		     (resulted-cstrpool (plan~mmatch-cstr-state mmatching))
		;;; Remark to the change of cstr+state while applying and expanding methods:
		     ;; It must be dinstinguished between adding new meta-variable bindings and
		     ;; changing the constraint pool. When a method application (expansion) only
		     ;; changes the cstr+constraint of CSTR+STATE and does not add meta-variable
		     ;; bindings, then the current cstr+state gets an empty pds~cstrpool-bindings.
		     ;; This is needed to prevent unneeded updating schematic tasks, namely when
		     ;; the meta-variable bindings is not changed:
		     )
		 (if (eq task-node (plan~mmatch-goal mmatching))
		;;; TASK was solved by a backward method:
		     ;; 1- Carry out the method actions first, because these can make some
		     ;; changes to the supports of the involved nodes. For instance, by applying
		     ;; a method M we get a new hypothesis (H1 (and a b)) which occurs in a subgoal
		     ;; Li, hence during the application of M H1 is added to the supports of Li.
		     ;; Suppose M has the outline actions [Li (unsponsor H1) (sponsor Lj)] where
		     ;; Lj corresponds to (Lj (H1) a ("AndEL" () (H1))). To prevent that H1 will
		     ;; be splitted into (Ln a) and (Ln+1 b) and these are added as supports to
		     ;; Li, we have to carry out the actions of M before normalizing the new
		     ;; closed nodes and we must prohibit to normalize new closed nodes which does 
		     ;; not support any node.
		     ;; 2- Update the goal-schemas on the agenda but not TASK, because it was carried
		     ;; out, determine hereby the new-bound-tasks on the agenda.
		     ;; 3- Try to restrict some tasks and normalize the new supports.
		     ;; 4- Set the alternative mmatchings of TASK-NODE to REST-MMATCHINGS and
		     ;; alternative methods to REST-METHODS union the methods eliminated by the CRI.
		     ;; 5- Interpret the matched method orderings using the remaining tasks.
		     ;; 6- Replace TASK by the NEW-TASKS and eventually the associated pseudo-task
		     ;; when the applied method is non-reliable.
		     (let* ((cstr-bindings (when resulted-cstrpool
					     (pds~cstrpool-bindings resulted-cstrpool)))
			    (new-bound-tasks (when cstr-bindings
					       (agenda~update-goal-schemas! agenda cstr-bindings task)))
			    (mmatching-mmapp (plan~mmatch-mmapp mmatching)))
		       (plan=execute-outline-actions task-node new-opens mmatching pds)
		       (multiple-value-bind (new-agenda remaining-tasks)
			   (plan=restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
			 (let ((new-tasks (if (meth~non-reliability applied-method)
					 ;;; METHOD is non-reliable, we have to consider
					      ;; a new pseudo task for TASK-NODE:
					      (cons (agenda~create-pseudo-goal task-node) remaining-tasks)
					    remaining-tasks)))
;MP no handling of alternatives for our machinery			   
;                           (let ((matching-params (plan~mmatch-parameters mmatching))
;                                 (remain-matchings)
;                                 (remain-methods))
;                             (dolist (mmatch (remove mmatching all-mmatchings))
;                               (let ((mmatch-meth (plan~matched-method mmatch))
;                                     (mmatch-para (plan~mmatch-parameters mmatch)))
;                                 (if (eq applied-method mmatch-meth)
;                                ;;; Same method as APPLIED-METHOD:
;                                     (if matching-params
;                                         (when (equal matching-params mmatch-para)
;                                      ;;; Same parameters as those of APPLIED-METHOD:
;                                           (push mmatch remain-matchings))
;                                       (push mmatch remain-matchings))
;                                   (unless mmatch-para
;                                     (unless (find mmatch-meth remain-methods)
;                                       (push mmatch-meth remain-methods))))))
;			     (setf (pdsn~alternative-mmatchings task-node) remain-matchings
;				   (pdsn~alternative-methods task-node) remain-methods))
			   (multiple-value-bind (first-task orderings)
			       (plan=create-agenda-orderings (meth~outline-orderings applied-method)
							     mmatching-mmapp new-tasks)
			     (agenda~replace-task task first-task (remove first-task new-tasks)
						  orderings new-agenda)))))
	      ;;; A forward method was applied to contribute to the solving of TASK which is
		   ;; currently unsolved:
		   ;; 1- Carry out the matched method actions.
		   ;; 2- Update the goal-schemas on the agenda including TASK and determine hereby
		   ;; the new-bound-tasks on the agenda.
		   ;; 3- Try to restrict some tasks and normalize the new supports.
		   ;; 4- Dont change the alternative mmatchings and the alternative method slots
		   ;; of TASK-NODE.
		   ;; 5- Interpret the matched method orderings using the remaining tasks.
		   ;; 6- Insert the NEW-TASKS, including eventually pseudo-tasks when the applied
		   ;; method is non-reliable, into AGENDA:
		   (let* ((cstr-bindings (when resulted-cstrpool
					   (pds~cstrpool-bindings resulted-cstrpool)))
			  (new-bound-tasks (when cstr-bindings
					     (agenda~update-goal-schemas! agenda cstr-bindings)))
			  (mmatching-mmapp (plan~mmatch-mmapp mmatching))
			  (one-conc (find-if-not #'pdsn~hypothesis-p new-supps)))
		     (plan=execute-outline-actions one-conc new-opens mmatching pds)
		     (multiple-value-bind (new-agenda remaining-tasks)
			 (plan=restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
		       (let ((new-tasks (if (meth~non-reliability applied-method)
				       ;;; METHOD is non-reliable, we have to consider
					    ;; a new pseudo task for TASK-NODE:
					    (append (mapcar #'agenda~create-pseudo-goal
							    (remove-if #'pdsn~hypothesis-p new-supps))
						    remaining-tasks)
					  remaining-tasks)))
			 (multiple-value-bind (first-task orderings)
			     (plan=create-agenda-orderings (meth~outline-orderings applied-method)
							   mmatching-mmapp new-tasks)
			   (agenda~insert-tasks task first-task new-tasks orderings new-agenda))))))))))
  (:method (meth) (omega~error "~A not applicable with matchings ~A." meth (lea=matching meth))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj lea+step) stream)
  (format stream "~A with ~A on ~A" (lea=method obj)(lea=parameter obj)(lea=task obj)))

(defun lea=trace-stack (&optional (stack lea*stack))
  (when lea*trace 
    (format T "~%##############################STACK")
    (dolist (seq stack)
      (format T "~%[[~A~{,~%  ~A~}]]~%{{~A~{,~%  ~A~}}}" (caar seq)(cdar seq)(cadr seq)(cddr seq)))
    (format T "~%##############################STACK")))


(defun lea=trace (&rest args)
  (when lea*trace (apply #'omega~trace args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the stuff for Mateja's learning mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lea=extract-tolearn (&optional (pds omega*current-proof-plan))
  (let* ((justs (lea=extract-tolearn-rec (pds~plan-steps pds)))
	 (str (format nil "[~A~{, ~A~}]" (car justs)(rest justs))))
    (when (notany #'(lambda (stored)(string-equal str stored)) lea*tolearn)
      (push str lea*tolearn))
    (format T "[~A~{,~%~A~}]" (car lea*tolearn)(rest lea*tolearn))))

(defun lea=extract-tolearn-rec (steps)
  (when steps
    (let* ((just (pdsc~an-just (car steps)))
	   (meth (just~method just))
	   (outline (pdsj~outline-pattern just))
	   ;; special treatment of twin justification: insert 'nonexistent' and record them only once! 
	   (real-outline (mapcar #'(lambda (pat) (if (member pat lea*outlines :test #'string-equal) pat infer*non-existent)) outline))
	   (nodesinoutline (remove-if #'(lambda (pat)(member pat lea*outlines :test #'string-equal)) outline)))
      (cons  (or (infer~outline-pattern2application meth real-outline)
		 (keim~name (pds~inference-application meth real-outline))
		 (keim~name meth))
	    (lea=extract-tolearn-rec (remove-if #'(lambda (step)
						    (member (keim~name (pdsc~an-node step)) nodesinoutline :test #'string-equal))
						(rest steps)))))))

(com~defcommand pds2outline
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function lea=extract-tolearn)
  (defaults )
  (Log-p T) 
  (help "Shows the OUTLINE (in the sense of Mateja Learning project) of the
current pds."))


; more complicated ;)  
;   (sort l #'(lambda (x y)
;   	    (let ((a1 (read-from-string (string-left-trim "L" (string (keim~name x)))))
;   		  (a2 (read-from-string(string-left-trim "L" (string (keim~name y)))))) (print a1)(print a2)
;   	      (cond ((not (integerp a1)) T)
;   		    ((not (integerp a2)) nil)
;   		    ((<  a1 a2))))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repair/bug-fixes!!! (put this into corresponding files)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;method.lisp


(defmethod pds~inference-application ((inference infer+method) (outline-pat list))
  (let* ((an-appl (meth~find-method (infer~find-arbitrary-application-name inference)))
	 (tacappl? (and an-appl (string-equal (car (meth~procedural-content an-appl)) 'apply-tactic)))
	 (new-outline (if tacappl? (remove-if #'infer~nonexistent-pattern-p outline-pat) outline-pat)))
    (or (meth~find-method (infer~outline-pattern2application inference new-outline))
	(meth~find-method (infer~outline-pattern2application inference (remove-if #'infer~nonexistent-pattern-p outline-pat)))  ;;; caution (for methods with variable premises, will only work sometimes) VS
	(meth~find-method (infer~outline-pattern2application inference outline-pat))  ;;; for methods that apply a tactic MP
	(when (= (length (infer~find-outline-methodnames (keim~name inference))) 1)   ;;maybe we are lucky
	  (meth~find-method (car (infer~find-outline-methodnames (keim~name inference)))))
	(meth~find-method (keim~name inference))
	(omega~error ";;;pds~~inference-application: Inconsistent mapping of some method to the inference ~A"
		       (keim~name inference)))))

(defmethod pds~inference-application ((inference infer+inference) (outline-pat list))
  nil)


(defgeneric infer~find-outline-methodnames (name)
  (declare (edited  "29-AUG-2001" )
           (authors Bracz )
           (input   "An inference-method.")
           (value   "line-function mappings exists otherwise nil."))
  (:method ((name infer+inference))
           (infer~methods (infer=inference-name name)))
  (:method ((name symbol))
           (let (result)
             (maphash #'(lambda (x y) (push y result))
                      (infer=om-mappings
                         (infer=om-find-outline-mappings name)))
           result)))

;(defmethod meth~create-node ((meth-node null) (mmapp meth+mapping)
;                             &key ((:null-just null-just)) ((:hyps hyps)))
;  (omega~warn "meth-create-node with NIL and mapping ~A null-just ~A hyps ~A" mmapp null-just hyps))

;; openmath.lisp

(defun om~env-objects-of-class (env class) ;;was om=
  (let ((classlist (if (listp class) class (list class))))
    (mapcar #'(lambda (key) (env~lookup-object key env))
	(env~classes-keys env classlist nil))))

;; plan.lisp

(defun oc=delete (node &optional (pds omega*current-proof-plan))
  (declare (edited  "23-APR-1998")
	   (authors Lassaad)
	   (input   "A node and a pds.")
	   (effect  "deletes NODE and update PDS.")
	   (value   "see plan=backtrack-node!."))
  ;;; Redefinition of oc=delete in /top/omega-com.lisp:
  (if (eq node (prob~proof-root pds))
      (progn
	(plan~message ";;; Node ~A corresponds to the root of ~A, it may not be deleted!"
		      node pds)
	(values nil (pds~agenda)))
    (or (omega~trace ";;; Deleting node ~A ..." node)
	(let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	      (undone-steps (pds~delete-node! node nil pds))
	      (agenda (pds~agenda pds)))
	  ;; Foci stuff:
	  (when (foci~in-use) (foci~compute-pcs :pds pds :delete-case t))
	  (multiple-value-bind (meth-nodes other-nodes pseudo pds-rootp)   ;; 4! values MP 
	      (plan=consistent-opens! cstrsteps2succs undone-steps nil nil nil pds)
	    (if other-nodes
	        ;;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
		(let ((extra-supps))
		  (dolist (each-node other-nodes)
		    (setq extra-supps (union extra-supps
					     (plan=set-difference (pdsn~just-sponsors each-node)
								  (pdsn~just-unsponsors each-node)))))
		  (multiple-value-bind (the-agenda rest-opens)
		      (plan=normalize-supports extra-supps other-nodes agenda pds)
		    ;;; Creates new tasks
		    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
			   (new-tasks (if pds-rootp
					  (cons root-new-task
						(append (mapcar #'agenda~create-open-task meth-nodes)
							(mapcar #'agenda~create-goal rest-opens)))
					(append (mapcar #'agenda~create-open-task meth-nodes)
						(mapcar #'agenda~create-goal rest-opens)))))
		      (when new-tasks
			(if (or (agenda~empty-p the-agenda) (agenda~first-task the-agenda))
			    (setq the-agenda (agenda~create nil new-tasks nil the-agenda))
			  (let ((next-tasks (agenda~next-tasks the-agenda)))
			    (setf (agenda~next-tasks the-agenda) (append new-tasks next-tasks))
			    (when next-tasks
			      (setf (agenda~orderings the-agenda)
				    (cons (agenda~ordering-create new-tasks next-tasks)
					  (agenda~orderings the-agenda)))))))
		      ;;; Adapt the PDS agenda:
		      (setf (pds~agenda pds)
			    (agenda~update the-agenda (prob~proof-steps pds)))
		      ;;; Return root task and new agenda:
		      (values root-new-task (pds~agenda pds)))))
	      (if (or meth-nodes pds-rootp)
		  (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
			 (new-tasks (if pds-rootp
					(cons root-new-task
					      (mapcar #'agenda~create-open-task meth-nodes))
				      (mapcar #'agenda~create-open-task meth-nodes))))
		    (if (or (agenda~empty-p agenda) (agenda~first-task agenda))
			(setq agenda (agenda~create nil new-tasks nil agenda))
		      (let ((next-tasks (agenda~next-tasks agenda)))
			(setf (agenda~next-tasks agenda) (append new-tasks next-tasks))
			(when next-tasks
			  (setf (agenda~orderings agenda)
				(cons (agenda~ordering-create new-tasks next-tasks)
				      (agenda~orderings agenda))))))
		    ;;; Adapt the PDS agenda:
		    (setf (pds~agenda pds)
			  (agenda~update agenda (prob~proof-steps pds)))
		    ;;; Return root task and new agenda:
		    (values root-new-task (pds~agenda pds)))
		(progn
		  ;;; Adapt the PDS agenda:
		  (setf (pds~agenda pds)
			(agenda~update agenda (prob~proof-steps pds)))
		  ;; Return root task and new agenda:
		  (values nil (pds~agenda pds))))))))))


(defun oc=open (node &optional (pds omega*current-proof-plan))
  (declare (edited  "23-APR-1998")
	   (authors Lassaad)
	   (input   "A node and a pds.")
	   (effect  "Opens NODE and update PDS.")
	   (value   "see plan=backtrack-node!."))
  ;;; Redefinition of oc=open in /top/omega-com.lisp:
  (omega~trace ";;; Opening node ~A ..." node)
  (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	(undone-steps (pds~open-node! node pds))
	(agenda (pds~agenda pds)))
    ;; Foci stuff:
    (when (foci~in-use) (foci~compute-pcs :pds pds :delete-case t))
    (multiple-value-bind (meth-nodes other-nodes pseudo pds-rootp)  ;; 4! values MP 
	(plan=consistent-opens! cstrsteps2succs undone-steps nil nil nil pds)
      (if other-nodes
	  ;;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
	  (let ((extra-supps))
	    (dolist (each-node other-nodes)
	      (setq extra-supps (union extra-supps
				       (plan=set-difference (pdsn~just-sponsors each-node)
						       (pdsn~just-unsponsors each-node)))))
	    (multiple-value-bind (the-agenda rest-opens)
		(plan=normalize-supports extra-supps other-nodes agenda pds)
	      ;;; Creates new tasks
	      (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		     (new-tasks (if pds-rootp
				    (cons root-new-task
					  (append (mapcar #'agenda~create-open-task meth-nodes)
						  (mapcar #'agenda~create-goal rest-opens)))
				  (append (mapcar #'agenda~create-open-task meth-nodes)
					  (mapcar #'agenda~create-goal rest-opens)))))
		(when new-tasks
		  (if (or (agenda~empty-p the-agenda) (agenda~first-task the-agenda))
		      (setq the-agenda (agenda~create nil new-tasks nil the-agenda))
		    (let ((next-tasks (agenda~next-tasks the-agenda)))
		      (setf (agenda~next-tasks the-agenda) (append new-tasks next-tasks))
		      (when next-tasks
			(setf (agenda~orderings the-agenda)
			      (cons (agenda~ordering-create new-tasks next-tasks)
				    (agenda~orderings the-agenda)))))))
	        ;;; Adapt the PDS agenda:
		(setf (pds~agenda pds)
		      (agenda~update the-agenda (prob~proof-steps pds)))
	        ;;; Return root task and new agenda:
		(values root-new-task (pds~agenda pds)))))
	(if (or meth-nodes pds-rootp)
	    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		   (new-tasks (if pds-rootp
				  (cons root-new-task
					(mapcar #'agenda~create-open-task meth-nodes))
				(mapcar #'agenda~create-open-task meth-nodes))))
	      (if (or (agenda~empty-p agenda) (agenda~first-task agenda))
		  (setq agenda (agenda~create nil new-tasks nil agenda))
		(let ((next-tasks (agenda~next-tasks agenda)))
		  (setf (agenda~next-tasks agenda) (append new-tasks next-tasks))
		  (when next-tasks
		    (setf (agenda~orderings agenda)
			  (cons (agenda~ordering-create new-tasks next-tasks)
				(agenda~orderings agenda))))))
	      ;;; Adapt the PDS agenda:
	      (setf (pds~agenda pds)
		    (agenda~update agenda (prob~proof-steps pds)))
	      ;;; Return root task and new agenda:
	      (values root-new-task (pds~agenda pds)))
	  (progn
	    ;;; Adapt the PDS agenda:
	    (setf (pds~agenda pds)
		  (agenda~update agenda (prob~proof-steps pds)))
	    ;;; Return root task and new agenda:
	    (values nil (pds~agenda pds))))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lea~produce-method (pattern &optional &key (method nil) (inference nil) (theory 'base))
  (let* ((chars (list #\space #\tab #\newline #\) #\( ))
	 (meth (if method method
		 (read-from-string
		  (substitute-if #\_ #'(lambda (x) (member x chars))
				 (string-trim chars (format nil "~A" pattern))))))
	 (inf (if inference inference meth))
	 (docu (format nil "Applies the pattern ~A." pattern)))
    (eval
     `(infer~defmethod ,inf
		       (outline-mappings (((existent nonexistent nonexistent) ,meth)))
		       (help ,docu)))
    (eval
     `(meth~defmethod ,meth ,inf
		      (in ,theory)
		      (rating 1000)
		      (reasoning :planning :middle-out) 
		      (declarations
		       (sorted-meta-variables (hyps o prlnlist) (opens o prlnlist) (supps list prlnlist); (sig list termlist)
					      (dummy o)(phi o)))
		      (premises (+ opens))
		      (conclusions (- goal)(+ supps))
		      (application-condition (apply-pattern? ,(format nil "~A" pattern) goal opens supps hyps))
		      (outline-actions (opens (sponsor-if-hyp hyps)))
		      (decl-content (goal () phi ("apply-pattern" () opens)))
		      (proc-content schema-interpreter)
		      (manual (author "Martin Pollet")(examples "Learned methods")(documentation ,docu))
		      (remark ,docu)))))


(defun lea=add-to-env (newsig)
  (let ((realsig (remove-if-not #'data~primitive-p newsig)))
    (when realsig
      (mapc #'(lambda (elem)
		(unless (env~lookup-object (keim~name elem) (pds~environment omega*current-proof-plan))
		  (env~enter (keim~name elem) elem (pds~environment omega*current-proof-plan))))   realsig))))

(defun lea=true-node (goal)
  (pdsn~open-node-create (env~lookup-object 'true (th~env 'base))
			 (pdsn~hyps goal)
			 (pds~new-node-name)))			 
			 
(meth~defcond apply-pattern? (args cmapp)
	      (declare )
	      (let* ((pattern (read-from-string(car args)))
		     (goal (second args))
		     (opens (third args))
		     (supps (fourth args))
		     (hyps (fifth args))
		     (result (lea~condfunc-search-all pattern goal)))
		(if result
		    (destructuring-bind (newopens newsupps newhyps newsig newbinds newcstrs) result
		      (lea=add-to-env newsig)
		      (let ((cmapp-ext (meth~mapp-extension cmapp)))
			(mapcar #'(lambda (newopen newsupp newhyp newbind newcstr)
				    (if newcstr (omega~error "Constraints ~A happend !!!" newcstr)
				      (meth~mapping-create
				       (meth~mapp-subst cmapp)
				       (meth~mapp-mapp cmapp)
				       (mapp~create
					(append (list opens supps hyps)
						(when cmapp-ext (mapp~domain cmapp-ext)))
					(append (list (if newopen newopen (list (lea=true-node goal)))
							  newsupp (cons goal newhyp));;hack to avoid hyp-nil, inst with something unproblematic!
						(when cmapp-ext (mapp~codomain cmapp-ext))))
				       (if newbind
					   (cstr~conjunction
					    (cons (meth~mapp-constraint cmapp)
						  (mapcar #'(lambda (dom codom)
							      (cstr~binding-create (list dom codom)))
							  (subst~domain newbind)(subst~codomain newbind))))
					 (meth~mapp-constraint cmapp)))))
				newopens newsupps newhyps newbinds newcstrs)))
		  (meth~mapp-new-constraint cmapp nil))))

	     
(meth~new-relational-function 'apply-pattern?)

(defun lea~condfunc-search-all (mo goal &key (star-bound lea*star-bound))
  (lea=start mo)
  (let* ((remember-pds omega*current-proof-plan)
	 (leagoal (pds~label2node (keim~name goal) lea*pds))
	 (leasupps (mapcar #'(lambda (supp) (pds~label2node (keim~name supp) lea*pds)) (pds~node-supports goal remember-pds)))
	 (leahyps  (mapcar #'(lambda (hyp) (pds~label2node (keim~name hyp) lea*pds))
			   (remove-if-not #'pdsn~hypothesis-p (prob~proof-steps remember-pds))))
	 (leaopens (mapcar #'(lambda (node) (pds~label2node (keim~name node) lea*pds))
			   (pds~open-nodes remember-pds)))
	 (leasig (om~env-objects-of-class (pds~environment lea*pds) 'data+struct))
	 (leapool (when (pds~constraint-pool-p (pds~constraint-pool lea*pds)) (pds~constraint-pool lea*pds)))
	 (leabind (when leapool (pds~cstrpool-bindings leapool)))
	 (leacstr (when leapool (pds~cstrpool-constraint leapool))))
    (lea=trace "Init! open ~{~A ~}  supps ~{~A ~} hyps ~{~A ~} sig ~{~A ~} bind ~A cstr ~{~A ~}"
	       leaopens leasupps leahyps leasig leabind leacstr)	       
    (setf omega*current-proof-plan lea*pds 
	  keim::pds*current-proof-plan lea*pds)
    (labels ((searchy ()
		      (loop
		       ;; no solution, no patterns on the stack
		       (when (null lea*stack) (return nil))
		       ;; found a solution, remove from stack, backtrack to the situation of the next stack entry
		       (when (null (caar lea*stack))
			 (let* ((hist (rest (pop lea*stack)))
				(opens (set-difference (pds~open-nodes lea*pds) leaopens))
				(supps (set-difference (pds~node-supports leagoal lea*pds) leasupps))
				(hyps  (set-difference (remove-if-not #'pdsn~hypothesis-p
								      (prob~proof-steps lea*pds)) leahyps))
				(sig (set-difference
				      (om~env-objects-of-class (pds~environment lea*pds) 'data+struct)
				      leasig))
				(pool (when (pds~constraint-pool-p (pds~constraint-pool lea*pds))(pds~constraint-pool lea*pds)))
				(bind (when pool (lea=subst-difference (pds~cstrpool-bindings pool) leabind)))
				(cstr (when pool (set-difference (pds~cstrpool-constraint pool) leacstr))));TODO unify this with first let!!
			   (lea=backtrack hist (rest (car lea*stack)))
			   (return (list opens supps hyps sig bind cstr))))
		       (lea=next star-bound))))
      (do ((result (searchy)(searchy))
	   (newopen nil (cons (car result) newopen))
	   (newsupps nil (cons (second result) newsupps))
	   (newhyps nil (cons (third result) newhyps))
	   (newsig nil (cons (fourth result) newsig))
	   (newbind nil (cons (fifth result) newbind))
	   (newcstr nil (cons (sixth result) newcstr)));TODO this is getting ridiculous
	  ((null result) 
	   (progn      (setf omega*current-proof-plan remember-pds
			     keim::pds*current-proof-plan remember-pds)
		       (list newopen newsupps newhyps (remove-duplicates (apply #'append newsig)) newbind newcstr)));TODO sort the results
	(lea=trace "Step! open ~{~A ~} supps ~{~A ~} hyps ~{~A ~} sig ~{~A ~} bind ~A cstr ~{~A ~}"
		   (car result)(second result)(third result)(fourth result)(fifth result)(sixth result))))))


(defgeneric lea=subst-difference (subst1 subst2)
  (:method  ((subst1 subst+substitution)(subst2 subst+substitution))
	    (let ((vars (set-difference (subst~domain subst1)(subst~domain subst2))))
	      (subst~remove-components vars subst1)))
  (:method  ((subst1 T)(subst2 null))
	    subst1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf new 
      '((FORALLI*-M IMPI-M-B)
	(star  FORALLI-SORT-M-B )
	(star  ASSOC-R-M-B )
	(disj  INVL-I-M-B  INVR-I-M-B )
	(IDL-I-M-B REFLEX-M-B)  ))


(setf froc  '( DEFNEXP-M-B
	       (star   FORALLI-SORT-RESCLASS-M-B EXPAND-IN-M-F)
	       (disj   (CONVERT-RESCLASS-TO-NUM-M-B DEFNEXP-M-B)
		       CONVERT-RESCLASS-TO-NUM-M-B)
	       OR-E**-M-B
	       (star  SIMPLIFY-NUMERICAL-EXPR-M-B)))



(th~defproblem pp

	       (in group)
	       (type-constants abc)

	       (constants (a o)(b o)(c o)(d o)(e o)(g1 (o i))(g2 (o i))(e1 i)(e2 i) (Pred (all-types bb (o bb)))(op1 (i i i )))
	       (assumption a1 (or a b))
	       (assumption a2 (or c d))
	       (assumption a3 (g1 e1))
	       (assumption a4 (g2 e2))
	       (conclusion 
		(exists-sort (lam (y i)
				  (exists-sort (lam (x i) (and (g2 y)(g1 x))) g1)) g2)))




(lea~produce-method '(existsi-sort-m-b existsi-sort-m-b  weaken-m-c  weaken-m-c ) :method 'test-m-b :inference 'test-m :theory 'group)

(strat~define-strategy-ks
 (name test)
 (refinement-algorithm PPlanner)
 (condition (lambda (x) t))
 (methods (
	   test-m-b
	   weaken-m-c
	   andi-m-b
	   truei-m-b
	   ))
 
 (normalization-methods (
			 ))
 (restriction-methods (
		       ))
 (control-rules (
		 ))
 (loop-detection )
 (termination-check (lambda () nil))
 (selection waterfall)
 (print "TEST-Strat ~A"))




(infer~defmethod True-m
		 (outline-mappings (((existent) True-m-b))))

(meth~defmethod True-m-b True-m
		(in base)
		(rating 1001)
		(reasoning :middle-out :planning)
		
		(premises )
		(conclusions (- L1))
		
		(decl-content
		 (l1 () true       ("TrueI" ()()))
		 )
		)


