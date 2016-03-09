(in-package "OMEGA")

(setf meth*sym2action
  (list (cons 'sponsor #'pdsn~add-new-supports)
	(cons 'unsponsor #'pdsn~remove-supports)))

(defun meth~create-empty-mmapp ()
  (meth~mapping-create (subst~create nil nil)
		       (mapp~create nil nil)))		 

(defun meth~compute-meth-vars (meth-vars mmapp)
  (declare (edited  "15-FEB-1999")
	   (authors Lassaad)
	   (input   "Method parameters, and a method mapping.")
	   (effect  "None.")
	   (value   "The associated PDS objects for the elements of PARAMS."
		    "When MMAPP does not allow to compute the associated PDS"
		    "object for some element of PARAMS, then a NIL is returned"
		    "for this PDS object."))
  (mapcar #'(lambda (meth-var) (meth~pds-object meth-var mmapp nil))
	  meth-vars))

(defun meth~compute-parameters (method mmapp)
  (meth~compute-meth-vars (meth~parameters method) mmapp))

(defun meth~compute-existent-premises (method mmapp)
  (meth~compute-meth-vars (meth~existent-premises method) mmapp))

(defun meth~compute-closed-premises (method mmapp)
  (meth~compute-meth-vars (meth~closed-premises method) mmapp))

(defun meth~premises-to-match (method)
  (append (meth~existent-premises method) (meth~closed-premises method)))

(defun meth~compute-premises (method mmapp)
  (meth~compute-meth-vars (meth~premises-to-match method) mmapp))

(defun meth~compute-goal (method mmapp)
  (when (meth~goal method)
    (meth~pds-object (meth~goal method))))

(defun meth~compute-outline (method mmapp)
  (meth~compute-meth-vars (append (meth~conclusions method) (meth~premises method)) mmapp))

#{
(defun meth~carry-out-computations (meth-computations mmapp)
  (declare (edited  "01-APR-1998")
	   (authors Lassaad)
	   (input   "..")
	   (effect  "Extends MMAPP by binding the names of METH-COMPUTATIONS"
		    "with the values resulted from evaluating the associated"
		    "funcalls.")
	   (value   "Unspecified."))
  (dolist (computation meth-computations)
    (let ((meth-funcall (meth=computation-funcall computation)))
      (cond ((meth~funcall-p meth-funcall)
	     (let ((func-name (keim~name meth-funcall))
		   (func-args (meth=funcall-args meth-funcall)))
	       (if (string-equal func-name 'if)
		   (let ((result (meth~check-condition (first func-args) mmapp)))
		     (meth~mapping-extend mmapp (keim~name computation)
					  (meth~pds-object (if result (second func-args) (third func-args))
							   mmapp nil)))
		 (meth~mapping-extend mmapp (keim~name computation)
				      (apply (meth~sym2function func-name)
					     (meth=eval-arguments func-args mmapp))))))
	    ((meth~key-expression-p meth-funcall)
	     (meth~mapping-extend mmapp (keim~name computation)
				  (meth~pds-object meth-funcall mmapp nil)))
	    (t (omega~error "unknown computational object...."))))))

(defun meth=eval-arguments (args mmapp &optional unboundp)
  (declare (edited  "05-NOV-1998" "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of method objects, a method mapping, and a key word"
		    "UNBOUNDP stating whether some elements of ARGS are allowed to"
		    "be method variables without associated pds object in MMAPP.")
	   (effect  "Signals a method error, when UNBOUNDP is set to NIL and MMAPP"
		    "cannot be used or extended to evaluate each element of ARGS."
		    "Possibly extends MMAPP.")
	   (value   "When successful, the evaluation of the elements of ARGS."))
  ;;; For each argument in ARGS try to determine the associated PDS object. When 
  ;; this is not possible, then: if UNBOUNDP is set and the element of ARGS is a
  ;; method variable, then it is allowed to return the element self; otherwise
  ;; MMAPP may be extended by creating new PDS objects for unbound method variables
  ;; with the sorts CONST and METAVAR, when the type of these method variables
  ;; corresponds wrt. the substitution of MMAPP to a variable free type. 
  (when args
    (let ((arg1 (first args)))
      (cond ((ssterm~var-p arg1)
	     (if unboundp
		 ;;; Dont extend MMAPP:
		 (let ((the-arg1 (meth~pds-object arg1 mmapp nil)))
		   (if the-arg1
		       (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))
		     (cons arg1 (meth=eval-arguments (rest args) mmapp unboundp))))
	       ;;; Possibly extend MMAPP: 
	       (let ((the-arg1 (meth~pds-object arg1 mmapp nil T)))
		 (if the-arg1
		     (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))
		   (omega~error "~A does not correspond to a pds object.~%" arg1)))))
	    (T ;;; Possibly extend MMAPP
	     (let ((the-arg1 (meth~pds-object arg1 mmapp nil T)))
	       (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))))))
    ))

(defgeneric meth~pds-object (meth-obj mmapp indicator &optional extend-subst bvar-mapp)
  (declare (edited  "06-NOV-1998" "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A method object, a method mapping, an indicator stating when given to which the"
		    "method object corresponds, and optionally a key EXTENDP which states whether it"
		    "is allowed to extend MMAPP by bindings to default objects (new constants and new"
		    "meta-variables), and an additional mapping BVAR-MAPP which stores a mapping of"
		    "symbols belonging to an abstraction domain to variables (this mapping is needed"
		    "to determine the range of this abstraction).")
	   (effect  "Possibly extends MMAPP when EXTENDP is set.")
	   (value   "The correponding pds object to METH-OBJ."))
  (:method ((key-expr meth+key-expression) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator bvar-mapp))
	   (let ((pds-obj (meth~pds-object (meth=key-expression-args key-expr) mmapp (keim~name key-expr) extendp)))
	     (if (term~p pds-obj)
		 (beta~normalize pds-obj)
	       pds-obj)))
  (:method ((alist cons) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (let ((elt1 (first alist)))
	     (if (and (symbolp elt1) (string-equal elt1 'lam))
		 (let ((matrix (butlast (rest alist))))
		   (multiple-value-bind (bvars new-bvar-mapp)
		       (meth~pds-object matrix mmapp :lam-domain extendp bvar-mapp)
		     (when bvars
		       (let ((scope (meth~pds-object (first (last alist)) mmapp :term extendp new-bvar-mapp)))
			 (when scope
			   (term~abstr-create (meth=flat-list bvars) scope))))))
	       (let ((head (meth~pds-object elt1 mmapp :term extendp bvar-mapp)))
		 (when head
		   (let ((args (meth~pds-object (rest alist) mmapp :appl-args extendp bvar-mapp)))
		     (when args
		       (term~appl-create head (meth=flat-list args)))))))))
  (:method ((bvars cons) (mmapp meth+mapping) (indicator (eql :lam-domain)) &optional extendp bvar-mapp)
	   (let ((bvar1 (first bvars))
		 (the-bvar-mapp (if bvar-mapp bvar-mapp (mapp~create nil nil))))
	     (if (consp bvar1)
		 (let ((bvar1-name (first bvar1))
		       (bvar1-type (data~replace-free-variables (second bvar1)
								(remove-if-not #'type~p (subst~domain (meth~mapp-subst mmapp)))
								(remove-if-not #'type~p (subst~codomain (meth~mapp-subst mmapp))))))
		   (when (meth~mapp-extension mmapp)
		     (setq bvar1-type
			   (data~replace-free-variables bvar1-type
							(remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp)))
							(remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))))
		   (let* ((the-bvar1 (term~variable-create bvar1-name bvar1-type))
			  (new-bvar-mapp1 (mapp~insert-component bvar1-name the-bvar1 the-bvar-mapp)))
		     (if (rest bvars)
			 (multiple-value-bind (rest-bvars new-bvar-mapp2)
			     (meth~pds-object (rest bvars) mmapp :lam-domain extendp new-bvar-mapp1)
			   (when rest-bvars
			     (values (cons the-bvar1 rest-bvars) new-bvar-mapp2)))
		       (values (list the-bvar1) new-bvar-mapp1))))
	       (if (ssterm~var-p bvar1)
		   (let ((assoc-var (meth~pds-object bvar1 mmapp :term extendp)))
		     (when assoc-var
		       (if (rest bvars)
			   (multiple-value-bind (rest-bvars new-bvar-mapp)
			       (meth~pds-object (rest bvars) mmapp :lam-domain extendp the-bvar-mapp)
			     (when rest-bvars
			       (values (cons assoc-var rest-bvars) new-bvar-mapp)))
			 (values (list assoc-var) the-bvar-mapp))))
		 (omega~error ";;; Illegal bound variable ~A, an intance of ~A"
			      bvar1 (type-of bvar1))))))
  (:method ((args cons) (mmapp meth+mapping) (indicator (eql :appl-args)) &optional extendp bvar-mapp)
	   (let ((arg1 (meth~pds-object (first args) mmapp :term extendp bvar-mapp)))
	     (when arg1
	       (if (rest args)
		   (let ((rest-args (meth~pds-object (rest args) mmapp :appl-args extendp bvar-mapp)))
		     (when rest-args (cons arg1 rest-args)))
		 (list arg1)))))
  (:method ((schema term+schema) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   ;;LC(meth~pds-object (data~schema-range schema) mmapp indicator extendp bvar-mapp))
	   (meth~pds-object (data~schema-range (data~copy schema :downto nil)) mmapp indicator extendp bvar-mapp))
  (:method ((const term+constant) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   (meth~object-instance2 const mmapp))
  (:method ((var term+variable) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   var)
  (:method ((meth-obj symbol) (mmapp meth+mapping) (indicator (eql :symbol)) &optional extendp bvar-mapp)
	   (declare (ignore bvar-mapp))
	   meth-obj)
  (:method ((meth-obj symbol) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (when (mapp~p bvar-mapp)
	     (mapp~get-component meth-obj bvar-mapp)))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (declare (ignore bvar-mapp))
	   (let ((pds-obj (or (meth~mapp-get-component meth-var mmapp :subst)
			      (meth~mapp-get-component meth-var mmapp :ext))))
	     (if pds-obj pds-obj
	       (when extendp
		 (let ((sort (ssterm~sort meth-var))
		       (subst (meth~mapp-subst mmapp))
		       (ext (meth~mapp-extension mmapp)))
		   (cond ((eq sort (ssterm~get-ssort :const))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((const (term~generate-term-primitive-with-new-name
					      (keim~name meth-var)
					      (data~replace-free-variables type ext-type-vars
									   (remove-if-not #'type~p (mapp~codomain ext)))
					      'term+constant
					      (pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var const ext)
				  const)
			      (let ((const (term~generate-term-primitive-with-new-name
					    (keim~name meth-var) type 'term+constant
					    (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var const)
				const))))
			 ((eq sort (ssterm~get-ssort :metavar))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((metavar (term~generate-term-primitive-with-new-name
						(keim~name meth-var)
						(data~replace-free-variables type ext-type-vars
									     (remove-if-not #'type~p (mapp~codomain ext)))
						'meta+variable
						(pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var metavar ext)
				  metavar)
			      (let ((metavar (term~generate-term-primitive-with-new-name
					      (keim~name meth-var) type 'meta+variable
					      (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var metavar)
				metavar))))
			 ))))))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator bvar-mapp))
	   (let ((pds-obj (meth~mapp-get-component meth-var mmapp :all)))
	     (if pds-obj pds-obj
	       (when extendp
		 (let ((sort (ssterm~sort meth-var))
		       (subst (meth~mapp-subst mmapp))
		       (ext (meth~mapp-extension mmapp)))
		   (cond ((eq sort (ssterm~get-ssort :const))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((const (term~generate-term-primitive-with-new-name
					      (keim~name meth-var)
					      (data~replace-free-variables type ext-type-vars
									   (remove-if-not #'type~p (mapp~codomain ext)))
					      'term+constant
					      (pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var const ext)
				  const)
			      (let ((const (term~generate-term-primitive-with-new-name
					    (keim~name meth-var) type 'term+constant
					    (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var const)
				const))))
			 ((eq sort (ssterm~get-ssort :metavar))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((metavar (term~generate-term-primitive-with-new-name
						(keim~name meth-var)
						(data~replace-free-variables type ext-type-vars
									     (remove-if-not #'type~p (mapp~codomain ext)))
						'meta+variable
						(pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var metavar ext)
				  metavar)
			      (let ((metavar (term~generate-term-primitive-with-new-name
					      (keim~name meth-var) type 'meta+variable
					      (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var metavar)
				metavar))))
			 ))))))
  (:method ((meth-funcall meth+funcall) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   (let ((funcall-args (meth=funcall-args meth-funcall)))
	     (if funcall-args
		 (apply (meth~sym2function (keim~name meth-funcall))
			(meth=eval-arguments funcall-args mmapp))
	       (funcall (meth~sym2function (keim~name meth-funcall)) nil))))
  (:method ((meth-obj string) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   meth-obj)
  (:method (meth-obj (mmapp meth+mapping) (indicator t) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp meth-obj))
	   (omega~error ";;; meth~~pds-object must be implemented for key ~A!"
			indicator)
	   (error "IMPLEMENT IT."))
  (:method (meth-obj (mmapp meth+mapping) (indicator null) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (meth~object-instance2 meth-obj mmapp))
  (:method ((meth-obj type+type) (mmapp meth+mapping) (indicator (eql :type)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (meth~object-instance2 meth-obj mmapp))
  (:method ((meth-obj pos+position) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   meth-obj)
  (:method ((meth-obj list) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (if (null meth-obj)
	       (pos~empty)
	     (let ((first-pos (meth~pds-object (car meth-obj) mmapp indicator extendp bvar-mapp)))
	       (when first-pos
		 (pos~concatenate first-pos
				  (meth~pds-object (cdr meth-obj) mmapp indicator extendp bvar-mapp))))))
  (:method ((meth-obj integer) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (pos~list-position (list meth-obj)))
  (:method ((sym number) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (term~constant-create sym (env~lookup-object :num (pds~environment omega*current-proof-plan))))
  )

(defun meth~complete-outlines (meth-concs meth-prems mmapp &optional pds-nodes hyps &key ((:mvar-bindings mvar-bindings)))
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "Two lists of method nodes (the method conclusions, and the method"
		    "premises), a method mapping containing bindings of method objects,"
		    "and optionally a list of pds nodes, and a hypothesis list. PDS-NODES"
		    "is a collector for the new nodes created while evaluating this function."
		    "HYPS is also a collector for the hypotheses of the new nodes.")
	   (effect  "Extends MMAPP with new bindings when new nodes are created.")
	   (value   "A triple consisting of three lists of pds nodes: conclusions, premises,"
		    "and new created nodes while evaluating this function."))

  (if meth-concs
      (let* ((meth-conc (first meth-concs))
	     )
	(if (meth~meta-node-p meth-conc)
	    (let* ((metavar (meth~meta-node-metavar meth-conc))
		   (concs (meth~mapp-get-component metavar mmapp :both))
		   )
	      (multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		  (meth~complete-outlines (rest meth-concs)
					  meth-prems
					  mmapp
					  (union concs pds-nodes)
					  hyps
					  :mvar-bindings mvar-bindings)
		(values (union concs
			       pds-concs)
			pds-prems
			new-nodes
			metavars)))
	  (let ((pds-conc (meth~mapp-get-component (keim~name meth-conc) mmapp :mapp)))
	    (if pds-conc
	    ;;; PDS-CONC is existent
		(if hyps
		;;; A hypothesis list HYPS is specified to be used for new created
		    ;; nodes. The hyps of PDS-CONC must include the elements of HYPS. 
		    (if (subsetp hyps (pdsn~hyps pds-conc))
			(multiple-value-bind (pds-concs pds-prems new-nodes metavars)
			    (meth~complete-outlines (rest meth-concs) meth-prems mmapp pds-nodes hyps
						    :mvar-bindings mvar-bindings)
			  (values (cons pds-conc
					pds-concs)
				  pds-prems
				  new-nodes
				  metavars))
		      (omega~error "Inconsistent hypothesis list ~A.~%" hyps))
		  (multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		      (meth~complete-outlines (rest meth-concs) meth-prems mmapp pds-nodes (pdsn~hyps pds-conc)
					      :mvar-bindings mvar-bindings)
		    (values (cons pds-conc
				  pds-concs)
			    pds-prems
			    new-nodes
			    metavars)))
	  ;;; The associated node to METH-CONC must be created, without justification
	      ;; and normal hypotheses. The associated conclusion is the first element in
	      ;; the returned list CREATED-NODES which contains the new created nodes.
	      ;; Moreover SUBST is extended with new bindings for the new nodes. 
	      (multiple-value-bind (created-nodes add-metavarss)
		  (meth~create-node meth-conc mmapp :null-just T :mvar-bindings mvar-bindings)
		(multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		    (meth~complete-outlines (rest meth-concs) meth-prems mmapp 
					    (append pds-nodes created-nodes) hyps :mvar-bindings mvar-bindings)
		  (values (cons (first created-nodes) pds-concs) pds-prems new-nodes
			  (union metavars add-metavars))))))))
    ;;; Now, consider the premises:
    (if meth-prems
	(let* ((meth-prem (first meth-prems)))
	  (if (meth~meta-node-p meth-prem)
	      (let* ((metavar (meth~meta-node-metavar meth-prem))
		     (prems (meth~mapp-get-component metavar mmapp :both))
		     )
		(multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		    (meth~complete-outlines nil
					    (rest meth-prems)
					    mmapp
					    (if (meth~zero-node-p meth-prem)
						pds-nodes
					      (union prems pds-nodes))
					    hyps
					    :mvar-bindings mvar-bindings)
		  (values pds-concs (union prems pds-prems) new-nodes metavars)))
	    
	    (let ((pds-prem (meth~mapp-get-component (keim~name meth-prem) mmapp :mapp)))
	      (if pds-prem
	      ;;; PDS-PREM is existent, its eventually specified extra hyps are
		  ;; considered as normal hyps for the new nodes. 
		  (multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		      (meth~complete-outlines nil (rest meth-prems) mmapp
					      pds-nodes
					      (union (pdsn~hyps pds-prem) hyps :mvar-bindings mvar-bindings))
		    (values pds-concs (cons pds-prem pds-prems) new-nodes metavars))
	    ;;; The associated node to METH-PREM must be created with the justification
		;; of METH-PREM and without normal hyps.
		(multiple-value-bind (created-nodes add-metavarss)
		    (meth~create-node meth-prem mmapp :mvar-bindings mvar-bindings)
		  (multiple-value-bind (pds-concs pds-prems new-nodes metavars)
		      (meth~complete-outlines nil (rest meth-prems) mmapp 
					      (append created-nodes pds-nodes) hyps :mvar-bindings mvar-bindings)
		    (values pds-concs (cons (first created-nodes) pds-prems) new-nodes
			    (union metavars add-metavarss))))))))
      ;;; Now, we have to set the normal hyps of the new-nodes
      (dolist (new-node pds-nodes (values nil nil pds-nodes nil))
	(when (or (null (node~justification new-node))
		  (not (pdsn~hypothesis-p new-node)))
	  (setf (pdsn~hyps new-node)
		(union (pdsn~hyps new-node) hyps)))))
    ))

(defmethod meth~create-node ((meth-node meth+node) (mmapp meth+mapping)
			     &key ((:null-just null-just)) ((:hyps hyps)) ((:mvar-bindings mvar-bindings)))
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "A method node, a method mapping, which does not contain a binding for METH-NODE,"
		    "but it is suffisant to create a pds node to be associated to METH-NODE. Moreover"
		    "two key words stating whether the created node should have a justification"  
		    "(:NULL-JUST), and which are their normal hypotheses (:HYPS). When :NULL-JUST and"
		    ":HYPS are together NIL the hypothesis list of the new created node is determined"
		    "from its premises if this node is not open and not a hypothesis.")
	   (effect  "Extending MMAPP with bindings of new created nodes.")
	   (value   "A list of new created nodes, its first element is the pds node associated to"
		    "METH-NODE, if this is possible using the bindings in MMAPP, otherwise NIL."))
  (let ((node-formula (meth~object-instance2 (node~formula meth-node) mmapp nil :mvar-bindings mvar-bindings)))
    (if node-formula
	(let ((extra-hyps nil)
	      (new-hyps nil)
	      (metavars nil))
	  ;;; Determining the new and extra hypotheses
	  (dolist (xtra-hyp (pdsn~hyps meth-node))
	    (let ((pds-hyp (meth~mapp-get-component (keim~name xtra-hyp) mmapp :mapp)))
	      (if pds-hyp
		  (setq extra-hyps (cons pds-hyp extra-hyps))
		(if (ssterm~var-p xtra-hyp)
		    (let ((add-hyps (meth~mapp-get-component xtra-hyp mmapp :mapp))
			  )
		      (if add-hyps
			  (setq extra-hyps (union add-hyps extra-hyps))
			(return-from meth~create-node
			  (omega~error "Not possible to create ~A with the binding ~A.~%"
				       xtra-hyp mmapp))))
		  (multiple-value-bind (new-nodes add-metavarss)
		      (meth~create-node xtra-hyp mmapp :mvar-bindings mvar-bindings)
		    (if new-nodes
			(setq extra-hyps (append extra-hyps new-nodes)
			      new-hyps (append new-hyps new-nodes)
			      metavars (append metavars add-metavarss))
		      (return-from meth~create-node
			(omega~error "Not possible to create ~A with the binding ~A.~%" xtra-hyp mmapp))))))))
	  (if null-just
	      (multiple-value-bind (new-node add-metavars)
		  (pdsn~create (pds~new-node-name omega*current-proof-plan)
			       NIL node-formula NIL)
		(setf (pdsn~hyps new-node) (append extra-hyps hyps))
		(meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
		(values (cons new-node new-hyps) (union metavars add-metavars)))
	    (let ((node-just)
		  (node-hyps extra-hyps))
	      (if (listp hyps)
		  ;;; The hypotheses of the new created node is given by the caller of this function.
		  (setq node-hyps (append extra-hyps hyps)
			node-just (meth~object-instance2 (node~justification meth-node) mmapp hyps
							:mvar-bindings mvar-bindings))
		;;; The hypotheses of the new created node are the union of the normal hypotheses
		;; of its premises together with its extra-hyps:
		(multiple-value-bind (the-just the-hyps)
		    (meth~object-instance2 (node~justification meth-node) mmapp hyps :mvar-bindings mvar-bindings)
		  (setq node-hyps (union extra-hyps the-hyps)
			node-just the-just)))
	      (if node-just
		  (multiple-value-bind (new-node add-metavars)
		      (pdsn~create (pds~new-node-name omega*current-proof-plan)
					       NIL node-formula node-just)
		    (let ((just-method (just~method node-just)))
		      (if (and (infer~dummy-p just-method)
			       (not (infer~open-p just-method)))
			  ;; The created node is a hypothesis node:
			  (setf (pdsn~hyps new-node) (list new-node))
			(setf (pdsn~hyps new-node) node-hyps))
		      (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
		      (values (cons new-node new-hyps) (union metavars add-metavars))))
		(omega~error "Not possible to create ~A with the binding ~A.~%"
			     (node~justification meth-node) mmapp)))))
      (omega~error "Not possible to create ~A with the binding ~A.~%"
		   (node~formula meth-node) mmapp))
    ))

(defun meth~apply-bindings (term mvar-bindings)
  (if mvar-bindings
      (data~replace-free-variables term (subst~domain mvar-bindings) (subst~codomain mvar-bindings))
    term))
			       
(defgeneric meth~object-instance2 (meth-obj mmapp &optional hyps &key ((:mvar-bindings mvar-bindings)))
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "A method object, a method mapping, and optionally HYPS."
		    "When specified, this argument is either a non-empty list"
		    "of nodes to be used as normal premises for new created"
		    "nodes, or T signaling that hypothesis list of new created"
		    "nodes must be determined from its premises.")
	   (effect  "Might extend MMAPP with new created pds-objects.")
	   (value   "The associated pds object of METH-OBJ when MMAPP is"
		    "sufficient to create this object, if the pds object is"
		    "a justification and HYPS is set to T, then this pds object"
		    "is returned in a pair together with the union of the normal"
		    "hypotheses of the premises, otherwise NIL."))
  (:method ((meth-obj-list cons) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   ;;; METH-OBJ-LIST may not be empty
	   (let ((pds-obj (meth~object-instance2 (first meth-obj-list) mmapp hyps :mvar-bindings mvar-bindings)))
	     (when pds-obj
	       (if (rest meth-obj-list)
		   (let ((pds-obj-list (meth~object-instance2 (rest meth-obj-list) mmapp hyps :mvar-bindings mvar-bindings)))
		     (when pds-obj-list
		       (cons pds-obj pds-obj-list)))
		 (list pds-obj)))))
  (:method ((meth-node meth+node) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (let ((pds-node (or (meth~mapp-get-component (keim~name meth-node) mmapp :mapp)
			       (meth~mapp-get-component (keim~name meth-node) mmapp :ext))))
	     (if pds-node pds-node
	       (let ((node-formula (meth~object-instance2 (node~formula meth-node) mmapp nil :mvar-bindings mvar-bindings)))
		 ;;; LCh: hier muss man die Typ-variablen, die nicht in der pds-env vorkommen mit einer
		 ;; Kappa binden und auch bei Just-parametern.
		 (when node-formula
		   (let ((extra-hyps (pdsn~hyps meth-node)))
		     (if extra-hyps
			 ;;; Node with extra hyps:
			 (let ((pds-hyps (meth~object-instance2 extra-hyps mmapp nil :mvar-bindings mvar-bindings)))
			   (when pds-hyps
			     (let ((node-just)
				   (node-hyps pds-hyps))
			       (if (listp hyps)
				   ;;; HYPS is either NIL or a list of pds nodes to be used as normal hypotheses
				   (setq node-hyps (append pds-hyps hyps)
					 node-just (meth~object-instance2 (node~justification meth-node) mmapp
									  nil :mvar-bindings mvar-bindings))
				 ;;; HYPS is T, thus the normal hypotheses must be determined from the premises 
				 (multiple-value-bind (pds-just hyp-list)
				     (meth~object-instance2 (node~justification meth-node) mmapp hyps
							   :mvar-bindings mvar-bindings)
				   (setq node-hyps (union pds-hyps hyp-list)
					 node-just pds-just)))
			       (when node-just
				 (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
							      NIL node-formula node-just))
				       (just-method (just~method node-just)))
				   (if (and (infer~dummy-p just-method)
					    (not (infer~open-p just-method)))
		                       ;;; The created node is a hypothesis node:
				       (setf (pdsn~hyps new-node) (list new-node))
				     (setf (pdsn~hyps new-node) node-hyps))
				   (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
				   new-node)))))
		       ;;; Node without extra hyps:
		       (let ((node-just)
			     (node-hyps))
			 (if (listp hyps)
			     ;;; HYPS is either NIL or a list of pds nodes to be used as normal hypotheses
			     (setq node-hyps hyps
				   node-just (meth~object-instance2 (node~justification meth-node) mmapp
								    nil :mvar-bindings mvar-bindings))
			   ;;; HYPS is T, thus the normal hypotheses must be determined from the premises 
			   (multiple-value-bind (pds-just hyp-list)
			       (meth~object-instance2 (node~justification meth-node) mmapp hyps
						     :mvar-bindings mvar-bindings)
			     (setq node-hyps hyp-list 
				   node-just pds-just)))
			 (when node-just
			   (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
							NIL node-formula node-just))
				 (just-method (just~method node-just)))
			     (if (and (infer~dummy-p just-method)
				      (not (infer~open-p just-method)))
		                 ;;; The created node is a hypothesis node:
				 (setf (pdsn~hyps new-node) (list new-node))
			       (setf (pdsn~hyps new-node) node-hyps))
			     (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
			     new-node)))))))
	       )))
  (:method ((meth-obj meth+meta-node) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (meth~create-node meth-obj mmapp :hyps hyps :mvar-bindings mvar-bindings))
  
  (:method ((meth-just meth+just) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (let ((just-method (meth~object-instance2 (just~method meth-just)
						    mmapp nil :mvar-bindings mvar-bindings)))
             (unless just-method
               (return-from meth~object-instance2))
             (if (infer~open-p just-method)
		 (if (listp hyps) (pdsj~open-just-create)
		   (values (pdsj~open-just-create) nil))
               (let* ((meth-prems (just~premises meth-just))
                      (just-prems (when meth-prems
                                    (meth~object-instance2 meth-prems mmapp hyps :mvar-bindings mvar-bindings)))
                      (meth-params (if (and meth-prems (not just-prems))
                                       (return-from meth~object-instance2)
                                     (pdsj~parameters meth-just)))
                      (just-params (when meth-params
                                     (meth~object-instance2 meth-params mmapp nil :mvar-bindings mvar-bindings)))
                      (meth-status (if (and meth-params (not just-params))
                                       (return-from meth~object-instance2)
                                     (pdsj~status meth-just)))
                      (just-status (when meth-status
                                     (meth~object-instance2 meth-status mmapp nil :mvar-bindings mvar-bindings))))
                 (when (and meth-status (not just-status))
                   (return-from meth~object-instance2))
		 (if (listp hyps)
		     (pdsj~closed-just-create just-method just-prems just-params
					      (if just-status just-status
						(if (or (infer~dummy-p just-method)
							(infer~rule-p just-method))
						    "grounded" "unexpanded")))
		   (let ((prems-hyps))
		     (do ((rest-meth-prems meth-prems (rest rest-meth-prems))
			  (rest-just-prems just-prems (rest rest-just-prems)))
			 ((null rest-meth-prems)
			  (values (pdsj~closed-just-create just-method just-prems just-params
							   (if just-status just-status
							     (if (or (infer~dummy-p just-method)
								     (infer~rule-p just-method))
								 "grounded" "unexpanded")))
				  prems-hyps))
		       (let* ((meth-prem (first rest-meth-prems))
			      (meth-prem-xhyps (pdsn~hyps meth-prem))
			      (just-prem-xhyps (meth~mapp-get-component (mapcar #'keim~name meth-prem-xhyps)
									mmapp :all)))
			 (setq prems-hyps (union prems-hyps
						 (meth=set-difference (pdsn~hyps (first rest-just-prems))
								 just-prem-xhyps)))))))
		 ))))
  (:method ((meth-appl term+appl) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   (let ((func (meth~object-instance2 (data~appl-function meth-appl) mmapp nil :mvar-bindings mvar-bindings)))
	     (when func
	       (let ((args (meth~object-instance2 (data~appl-arguments meth-appl) mmapp nil :mvar-bindings mvar-bindings)))
		 (when args
		   (if (term~abstr-p func)
		       (beta~normalize (term~appl-create func (meth=flat-list args)))
		     (if (and (or (logic~existential-quantor-p func)
				  (logic~universal-quantor-p func))
			      (rest (data~abstr-domain (car (meth=flat-list args)))))
			 (let* ((vars2bind (data~abstr-domain (car (meth=flat-list args))))
				(arg-range (data~abstr-range (car (meth=flat-list args))))
				(result (dolist (var (reverse vars2bind) arg-range)
					  (setq arg-range
						(term~appl-create
						 ;;LC: unneeded
						 ;(data~copy (if (data~constant-p func)  ;; func can be an appl, MP
							;	(data~constant-origin func) func))
						 func
						 (list (term~abstr-create (list var)
									  arg-range)))))))
			   result)
		       (term~appl-create ;;LC: unneeded (data~copy (if (data~constant-p func)
							;(data~constant-origin func) func))
			                 func
					 (meth=flat-list args)))))))))
  (:method ((meth-abstr term+abstr) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   (let ((abstr-domain (meth~object-instance2 (data~abstr-domain meth-abstr) mmapp nil :mvar-bindings mvar-bindings)))
	     (when abstr-domain
	       (let ((abstr-range (meth~object-instance2 (data~abstr-range meth-abstr) mmapp nil :mvar-bindings mvar-bindings)))
		 (when abstr-range
		   (term~abstr-create (if (and (consp abstr-domain)
					       (every #'consp abstr-domain))
					       (apply #'append abstr-domain)
					       abstr-domain)
					  abstr-range))))))
  (:method ((meth-schema term+schema) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   ;;LC(meth~object-instance2 (data~schema-range meth-schema) mmapp hyps :mvar-bindings mvar-bindings))
	   (meth~object-instance2 (data~schema-range (data~copy meth-schema :downto nil)) mmapp hyps
				 :mvar-bindings mvar-bindings))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (let* ((subst (meth~mapp-subst mmapp))
		  (pds-obj (subst~get-component meth-var subst)))
	     (if pds-obj
		 ;; pds-obj is a term
		 ;; now substitute the metavars with mvar-bindings
		 (meth~apply-bindings pds-obj mvar-bindings)
	       (let ((pds-obj (or (mapp~get-component meth-var (meth~mapp-mapp mmapp))
				  (meth~mapp-get-component meth-var mmapp :ext))))
		 (if (or (listp hyps) (not (pdsj~justification-p pds-obj))) pds-obj
		   (let ((prems-hyps))
		     (dolist (prem (just~premises pds-obj) (values pds-obj prems-hyps))
		       (setq prems-hyps (union prems-hyps (pdsn~hyps prem))))))))))
  (:method ((const term+number) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   const)
  (:method ((const term+primitive) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   ;(omega~trace "~%meth~~object-instance of const (vorher) ~A~%" const)
	   ;;; Because of free type-variables
	   (let* ((mapp (meth~mapp-subst mmapp))
		  (map-dom (remove-if-not #'type~p (subst~domain mapp)))
		  (the-const (subst~apply (subst~restrict-substitution mapp map-dom)
					  const))
		  (const-type-vars (type~free-variables (term~type the-const)))
		  (ext-type-vars (when (meth~mapp-extension mmapp)
				   (remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp))))))
	     (if (intersection const-type-vars ext-type-vars)
		 (data~replace-free-variables the-const ext-type-vars
					      (remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))
	       the-const)))
  (:method ((type type+type) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   ;;; Because of free type-variables
	   (let* ((mapp (meth~mapp-subst mmapp))
		  (map-dom (remove-if-not #'type~p (subst~domain mapp)))
		  (the-type (subst~apply (subst~restrict-substitution mapp map-dom) type))
		  (type-vars (type~free-variables the-type))
		  (ext-type-vars (when (meth~mapp-extension mmapp)
				   (remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp))))))
	     (if (intersection type-vars ext-type-vars)
		 (data~replace-free-variables the-type ext-type-vars
					      (remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))
	       the-type)))
  (:method ((inference infer+inference) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   inference)
  (:method ((expr meth+key-expression) (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   (meth~pds-object expr mmapp nil))
  (:method (meth-obj (mmapp meth+mapping) &optional hyps &key ((:mvar-bindings mvar-bindings)))
	   (declare (ignore hyps))
	   (omega~error "Dont know how to determine ~A an instance of ~A using ~A.~%"
			meth-obj (type-of meth-obj) mmapp)))



(defgeneric meth=syntactic-sort-p (pds-obj ssort)
  (declare (edited  "02-APR-1998")
	   (authors Lassaad)
	   (input   "A pds object, and a syntactic sort.")
	   (effect  "None.")
	   (value   "T, iff PDS-OBJ corresponds to the sort SSORT."))
  (:method (pds-obj (ssort null))
	   (declare (ignore pds-obj))
	   T)
  (:method ((problem prob+problem) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :problem) ssort))
  (:method ((pds-node pdsn+node) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :prln) ssort))
  (:method ((pds-just pdsj+justification) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :just) ssort))
  (:method ((pds-metavar meta+variable) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :metavar) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-var term+variable) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :var) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-const term+constant) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :const) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-const term+term) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :term) ssort))
  (:method ((pos pos+position) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :pos) ssort))
  (:method ((pds-list list) (ssort ssterm+syntactic-sort))
	   (cond ((every #'term~p pds-list)
		  (eq (ssterm~get-ssort :termlist) ssort))
		 ((every #'pdsn~p pds-list)
		  (eq (ssterm~get-ssort :prlnlist) ssort))
		 ((every #'pos~p pds-list)
		  (eq (ssterm~get-ssort :poslist) ssort))
		 (T
		  (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
                               (keim~name ssort) pds-list (type-of pds-list)))))
  (:method ((sgn-tuple sgn+tuple) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :signs) ssort))
  (:method ((list string) (ssort ssterm+syntactic-sort))
	   T)
  (:method ((pds-obj T) (ssort ssterm+syntactic-sort)) 
	   (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
			(keim~name ssort) pds-obj (type-of pds-obj)))
  )

#}
