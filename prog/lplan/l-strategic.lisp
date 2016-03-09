;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "OMEGA")

(eval-when (load compile eval)
  (defclass strat+strategy (keim+name)
    ((ref     :initarg :ref
	      :accessor strat~ref 
	      :documentation "This is the refinement for which the strategie is defined.")
     (control :initarg :control
	      :accessor strat~control
	      :documentation "The list of control rules for the method.")
     )
    (:documentation "The datastructure for strategies.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of a strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro strat~def-strategic-refinement (methname stratname use-by &optional (control nil))
  (declare (edited  "21-DEZ-1998")
	   (authors Scholl)
	   (input   "A strategy specification: name of the method, list of control rules.")
	   (effect  "Creates an object of the strategy and replaces the method object with it.")
	   (value   "Unspecified."))
  `(block defmethod
     (let* ((methname ',methname)
	    (method (if (ref~find-ref methname)
			(ref~find-ref methname)
		      (omega~error ";;;METH~~DEF-STRATEGIC-REFINEMENT: The method ~A does not exists." methname)))
	    (stratname ',stratname)
	    (use-by (if (and (listp ',use-by) (equal (first ',use-by) 'use-by))
			(second ',use-by)
		      (progn 
			(omega~error ";;;METH~~DEF-STRATEGIC-REFINEMENT: Error in the syntax of the definition of ~A." stratname)
			nil)))
	    (control (if (listp ',control)
			 (if (and (null ',control) (equal use-by :default))
			     (progn
			       (omega~error ";;;METH~~DEF-STRATEGIC-REFINEMENT: The default strategic refinement must have control rules.")
			       :error)
			   ',control)
		       (let ((control-method (ref~find-ref ',control)))
			 (if (and control-method (strat~p control-method))
			     (strat~control control-method)
			   (progn
			     (omega~error ";;;METH~~DEF-STRATEGIC-REFINEMENT: The strategic method ~A, from which the control should be taken, does not exists." control-method)
			     :error))))))
       (when (and method use-by (not (equal control :error)))
	 (omega~message ";;; Defining strategic method ~A as ~A" stratname use-by)
	 (let* ((hashtable (if (ref~refinement-p method) ref*ref-hashtable meth*method-hashtable))
		(new-object (make-instance 'strat+strategy
					   :name stratname
					   :ref method
					   :control control)))
	   (setf (gethash (symbol-name stratname) hashtable) new-object)
	   (when (and (not (ref~refinement-p method)) (equal use-by :default))
	     (meth=replace-method new-object))
	   new-object)))))

(defmethod meth=replace-method ((strat strat+strategy))
  (declare (edited  "21-DEZ-1998")
	   (authors Scholl)
	   (input   "A strategy.")
	   (effect  "Replaces the method with its strategic method in the lists of methods.")
	   (value   "The strategic method."))
  (let ((ref (strat~ref strat)))
    (nsubstitute strat ref meth*planning-methods)
    (nsubstitute strat ref meth*MOReasoning-methods)
    (nsubstitute strat ref meth*normalizing-methods)
    (nsubstitute strat ref meth*restricting-methods)
    strat))

(defmethod meth~remove-method ((strat strat+strategy))
  (setf meth*planning-methods (remove strat meth*planning-methods))
  (setf meth*MOReasoning-methods (remove strat meth*MOReasoning-methods))
  (setf meth*normalizing-methods (remove strat  meth*normalizing-methods))
  (setf meth*restricting-methods (remove strat  meth*restricting-methods)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Shortcuts To The Underlying Method}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod meth~theory ((strategy strat+strategy))
  (meth~theory (strat~ref strategy)))

(defmethod meth~reasoning ((strategy strat+strategy))
  (meth~reasoning (strat~ref strategy)))

(defmethod meth~inference ((strategy strat+strategy))
  (meth~inference (strat~ref strategy)))

(defmethod meth~rating ((strategy strat+strategy))
  (meth~rating (strat~ref strategy)))

(defmethod meth~non-reliability ((strategy strat+strategy))
  (meth~non-reliability (strat~ref strategy)))

(defmethod meth~mor-method-p ((strategy strat+strategy))
  (meth~mor-method-p (strat~ref strategy)))

(defmethod meth~environment ((strategy strat+strategy))
  (meth~environment (strat~ref strategy)))

(defmethod meth~parameters ((strategy strat+strategy))
  (meth~parameters (strat~ref strategy)))

(defmethod meth~premises ((strategy strat+strategy))
  (meth~premises (strat~ref strategy)))

(defmethod meth~exist-premises ((strategy strat+strategy))
  (meth~exist-premises (strat~ref strategy)))

(defmethod meth~application-condition ((strategy strat+strategy))
  (meth~application-condition (strat~ref strategy)))

(defmethod meth~application-constraint ((strategy strat+strategy))
  (meth~application-constraint (strat~ref strategy)))

(defmethod meth~outline-computations ((strategy strat+strategy))
  (meth~outline-computations (strat~ref strategy)))

(defmethod meth~outline-actions ((strategy strat+strategy))
  (meth~outline-actions (strat~ref strategy)))

(defmethod meth~outline-orderings ((strategy strat+strategy))
  (meth~outline-orderings (strat~ref strategy)))

(defmethod meth~expansion-computations ((strategy strat+strategy))
  (meth~expansion-computations (strat~ref strategy)))

(defmethod meth~conclusions ((strategy strat+strategy))
  (meth~conclusions (strat~ref strategy)))

(defmethod meth~declarative-content ((strategy strat+strategy))
  (meth~declarative-content (strat~ref strategy)))

(defmethod meth~procedural-content ((strategy strat+strategy))
  (meth~procedural-content (strat~ref strategy)))

(defmethod meth~remark ((strategy strat+strategy))
  (meth~remark (strat~ref strategy)))

(defmethod meth~manual ((strategy strat+strategy))
  (meth~manual (strat~ref strategy)))

;(defmethod keim~name ((strategy strat+strategy))
;  (keim~name (strat~ref strategy)))

(defmethod ref~input-parameters ((strategy strat+strategy))
  (ref~input-parameters (strat~ref strategy)))

(defmethod ref~applicability-func ((strategy strat+strategy))
  (ref~applicability-func (strat~ref strategy)))

(defmethod ref~application-func ((strategy strat+strategy))
  (ref~application-func (strat~ref strategy)))

(defmethod ref~executability-func ((strategy strat+strategy))
  (ref~executability-func (strat~ref strategy)))

(defmethod ref~execution-func ((strategy strat+strategy))
  (ref~execution-func (strat~ref strategy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions Concerning Strategic Refinements}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strat~p (object)
  (typep object 'strat+strategy))

(defmethod print-object ((object strat+strategy) stream)
  (format stream "(strategic method ~A)" (keim~name object)))

(defun strat~control-link (task)
  (keim~get (first (lagenda~task-nodes task)) :control-link))

(defsetf strat~control-link (task) (control-link)
  `(dolist (node (lagenda~task-nodes ,task))
     (keim~put node :control-link ,control-link)))

(defun strat~strategic-reason-p (rsn)
  (and (pdsh~reason-p rsn)
       (or (and (pdsh~ia-reason-p rsn) (strat~p (pdsh~ia-ref rsn)))
	   (and (pdsh~pr-reason-p rsn) (strat~p (pdsh~pr-refinement rsn))))))

(defun strat~reason-control-rules (rsn)
  (when (strat~strategic-reason-p rsn)
    (strat~control (if (pdsh~ia-reason-p rsn) (pdsh~ia-ref rsn) (pdsh~pr-refinement rsn)))))

(defmethod strat~task-control-rules ((task lagenda+task))
  (when (strat~control-link task)
    (strat~reason-control-rules (first (strat~control-link task)))))

(defmethod strat~task-default-strat-p ((task lagenda+task))
  (or (not (strat~control-link task))
      (and (strat~control-link task) (null (strat~task-control-rules task)))))

;(defmethod strat~task-control-rules ((task lagenda+task))
;  (when (lagenda~task-control-link task)
;    (strat~control (lagenda~task-control-link task))))
;
;(defmethod strat~task-default-strat-p ((task lagenda+task))
;  (or (not (lagenda~task-control-link task))
;      (and (lagenda~task-control-link task) (not (strat~control (lagenda~task-control-link task))))))

(defun strat~last-direct-strategic-reason (node)
  ;;; falsch reason inference muss mit outline pattern kombiniert werden um method zu erhalten
  (pdsh~latest-reason-in-list (remove-if-not #'(lambda (rsn) (and (pdsh~ia-reason-p rsn)
								  (strat~p (pdsh~ia-ref rsn))))
					     (pdsn~reasons node))))

(defun strat~last-strategic-reason (node)
  (let ((last-direct-reason (strat~last-direct-strategic-reason node)))
    (if last-direct-reason
	last-direct-reason
      (pdsh~latest-reason-in-list (mapcar 'strat~last-strategic-reason (pdsh~super-goals node))))))

(defun strat~last-strategic-ref-containing (node)
  (let ((last-strategic-reason (strat~last-strategic-reason node)))
    (when last-strategic-reason
      (pdsh~ia-ref last-strategic-reason))))
      
(defun strat~update-control-link (new-tasks &key ((:super-task super-task)) ((:reason rsn)))
  (if (strat~strategic-reason-p rsn)
      (dolist (new-task new-tasks)
	(setf (strat~control-link new-task) (cons rsn (strat~control-link new-task))))
    (if super-task
	(dolist (new-task new-tasks)
	  (setf (strat~control-link new-task) (list (first (strat~control-link super-task)))))
      (dolist (new-task new-tasks)
	;; remove reasons that exist no more
	(setf (strat~control-link new-task) (remove-if-not #'(lambda (rsn) (find rsn (pds~all-reasons omega*current-proof-plan)))
							   (strat~control-link new-task)))))))
	 
;(defun strat~update-control-link (new-tasks &key ((:super-task super-task)) ((:refinement ref)))
;  (if (and ref (strat~p ref))
;      ;; new tasks are created by a strategic refinement
;      (dolist (new-task new-tasks)
;        (setf (lagenda~task-control-link new-task) ref))
;    ;; new tasks are not created by a strategic refinement
;    (if super-task
;        ;; super task available, so inherit the control link
;        (dolist (new-task new-tasks)
;          (setf (lagenda~task-control-link new-task) (lagenda~task-control-link super-task)))
;      ;; control link has to be computed from the reasons
;      (dolist (new-task new-tasks)
;        (setf (lagenda~task-control-link new-task)
;              (strat~last-strategic-ref-containing (lagenda~task-node new-task)))))))

(defun strat~latest-strategic-tasks (tasks pds)
  ;; all tasks are in the scope of strategic refinements
  (let ((latest-reason (pdsh~latest-reason-in-list (mapcar 'first (mapcar 'strat~control-link tasks)))))
    (remove-if-not #'(lambda (task) (eq latest-reason (first (strat~control-link task)))) tasks)))
  
(defun strat~strategic-conflict-tasks (tasks pds)
  (if (notevery 'strat~task-default-strat-p tasks)
      ;; there are tasks in the scope of a strategic refinement
      (strat~latest-strategic-tasks (remove-if 'strat~task-default-strat-p tasks) pds)
    ;; there are no tasks in the scope of a strategic refinement
    tasks))


(defmethod meth~execute-action ((method STRAT+STRATEGY) (action-pat meth+action-pattern)
				(mmapp meth+mapping) (pds pds+proof-plan) reasons default-hyps)
  (declare (edited  "10-APR-1998")
	   (authors Lassaad)
	   (input   "A method, one of its action patterns, a method mapping, a PDS,"
		    "a list of reasons to be inserted into new created nodes, and a"
		    "list of default hypotheses to be used for creating new open nodes.")
	   (effect  "Carry out the actions of ACTION-PAT by updating the supports of the"
		    "involved nodes.")
	   (value   "Unspecified."))
  (let* ((aps-nodes (meth=action-pattern-supported-nodes action-pat))
	 (supp-nodes (meth=flat-list
		      (cond ((listp aps-nodes)
			     (mapcar #'(lambda (n)
					 (meth=associated-node n mmapp method reasons pds default-hyps))
				     aps-nodes))
			    ((string-equal aps-nodes :all-opens) (pds~open-nodes pds)))))
	 (supported-nodes (if (every #'consp supp-nodes) (apply #'append supp-nodes) supp-nodes)))
    (if supported-nodes
	(let ((the-actions (meth=action-pattern-actions action-pat)))
	  (dolist (action the-actions)
	    (let ((supp-nodes (mapcar #'(lambda (n)
					  (meth=associated-node n mmapp method reasons pds default-hyps supported-nodes))
				      (meth=action-support-nodes action)))
		  (action-func (meth~sym2action (keim~name action))))
	      (dolist (supported-node supported-nodes)
		(funcall action-func supported-node supp-nodes pds)))))
      (omega~error ";;; Action pattern ~A without supported nodes!" action-pat))
    ))




