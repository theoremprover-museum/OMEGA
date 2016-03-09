

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameterized Inference Engine with O-ANTS
;;                            (chris|sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file enables the use of OANTS as a parameterized inference
;; engine for doing expansion of tactics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :omega)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make sure it only works for inference methods with one conclusion.

(defun part~expand-with-oants (outline commands)
  (declare (edited  "26-JUL-2001")
	   (authors Sorge)
	   (input   "An outline and a list of command names.")
	   (effect  "Inserts nodes and justifications into the current PDS.")
	   (value   "Undefined."))
;  (:method ((node pdsn+node) (commands list))
  (tacl~init outline)
  (let* ((old-commands (csm~considered-commands))
	 (new-problem (pds~make-problem-from-node (car outline)))
	 (old-pds omega*current-proof-plan)
	 (subpds (oc=prove-pre new-problem))
	 )
					;       (foci~compute-pcs)  ;;; done automaticall in prove-pre ?
    
    ;;(tacl~end :force t)
    ))
    ;;    (csm~set-considered-commands commands)
;;    (oc=stop-use-resources)
;;    (oc=use-standard-heuristics)
;;    (auto~set-auto-default-interval 10)
;;    (auto~prove)
;;    sub-pds))
  
;  (:method ((node string) (commands list))
;           (let ((pdsn (pds~label2node node)
;                       (if pdsn
;                           (part~expand-with-oants pdsn commands)
;                         (error ";;; PART~EXPAND-WITH-OANTS: node ~A is not in proof plan!" node))))))
;  (:method ((node symbol) (commands list))
;           (let ((pdsn (pds~label2node node)
;                       (if pdsn
;                           (part~expand-with-oants pdsn commands)
;                         (error ";;; PART~EXPAND-WITH-OANTS: node ~A is not in proof plan!" node))))))
;  (:method ((node null) (commands list))
;           (error ";;; PART~EXPAND-WITH-OANTS: node ~A is not a proof node!" node)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for creating subPDSs for independent subproofs.
;; (This should go somewhere else probably!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric pds~make-problem-from-node (node &optional (pds pds*current-proof-plan))
  (declare (edited  "27-JUL-2001")
	   (authors Sorge)
	   (input   "A node and a pds.")
	   (effect  "Creates a new problem.")
	   (value   "The newly created problem."))
  (:method ((node pdsn+node) &optional (pds pds*current-proof-plan))
	   (if (find node (prob~proof-steps pds))
	       (if (pdsn~open-node-p node)
		   (let ((hyps (mapcar #'(lambda (sup)
					   (pdsn~make-hypothesis (data~copy (node~formula sup))
								 (keim~name sup)))
				       (pds~node-supports node))))
		     (prob~create (read-from-string (format nil "~A-~A" (keim~name pds) (keim~name node)))
				  (prob~theory pds)
				  (pds~environment pds)
				  hyps
				  (pdsn~open-node-create (data~copy (node~formula node))
							 hyps
							 (keim~name node))
				  nil
				  'lemma
				  (format nil "Subproblem for node ~A in PDS ~A" node pds)))
		 (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: node ~A is not an open node!" node pds))
	     (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: node ~A is not in proof plan ~A!" node pds))
	   )
  (:method ((node string) &optional (pds pds*current-proof-plan))
	   (let ((pdsn (pds~label2node node pds)))
	     (if pdsn
		 (pds~make-problem-from-node pdsn pds)
	       (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: node ~A is not in proof plan ~A!" node pds))))
  (:method ((node symbol) &optional (pds pds*current-proof-plan))
	   (let ((pdsn (pds~label2node node pds)))
	     (if pdsn
		 (pds~make-problem-from-node pdsn pds)
	       (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: node ~A is not in proof plan ~A!" node pds))))
  (:method ((node null) &optional (pds pds*current-proof-plan))
	   (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: not a valid node in proof plan ~A!" pds))
  (:method :around (node &optional (pds pds*current-proof-plan))
	   (declare (ignore node))
	   (if (pds~proof-plan-p pds)
	       (call-next-method)
	     (error ";;; PDS~~MAKE-PROBLEM-FROM-NODE: ~A is not a valid proof plan!" pds))))
	   

(defun agenda~enqueue-then-agenda (agenda then-agenda)
  (declare (edited  "07-AUG-2001")
	   (authors Sorge)
	   (input   "Two agendas.")
	   (effect  "Enqueues THEN-AGENDA as the last possible then-agenda for AGENDA.")
	   (value   "The modified agenda."))
  (cond ((agenda~empty-p agenda) then-agenda)
	((agenda~empty-p (agenda~then-agenda agenda))
	 (setf (agenda~then-agenda agenda) then-agenda)
	 agenda)
	(t (agenda~enqueue-then-agenda (agenda~then-agenda agenda) then-agenda)
	   agenda)))
  

(defun spds~insert-pds (pds subpds &optional (node-mapping nil))
  (declare (edited  "27-JUL-2001")
	   (authors Sorge)
	   (input   "Two PDSs and an association list mapping nodes from PDS onto the hypotheses and"
		    "conclusion of subPDS.")
	   (effect  "Inserts the content of the subPDS into the PDS."
		    "The hypotheses and conclusions of subPDS are mapped to nodes in PDS according to NODE-MAPPING."
		    "If it is empty they are mapped with respect to the labels."
		    "If this is not possible the function fails."
		    "ATTENTION: some objects of subPDS might actually become objects of PDS."
		    "The subPDS is therefore subsequently discarded.")
	   (value   "The extended PDS."))
  (let* ((mapping (spds=complete-node-mapping pds subpds node-mapping)))
    (when (and mapping  ;;; open node thingy!!!!!!!!!!
	       (not (omega~warn "Here should go some stuff to check additional merging conditions.")))
      (omega~trace "Merging PDS ~A into PDS ~A!" pds subpds)
      (let* ((remove-hyps (spds=get-hyp-difference mapping))
	     (old-hyp-nodes (mapcar #'car (cdr mapping)))
	     (new-hyp-nodes (mapcar #'cdr (cdr mapping)))
	     (old-conc-node (caar mapping))
	     (new-conc-node (cdar mapping))
	     (new-nodes (set-difference (prob~proof-steps subpds) (cons new-conc-node new-hyp-nodes)))
	     (subst-list (mapcar #'(lambda (oh nh) (spds=merge-hyp-node oh nh mapping))
				 old-hyp-nodes new-hyp-nodes))
	     (substitution (spds=merge-conc-node old-conc-node new-conc-node mapping
						 (spds=compose-subst-list subst-list)))
	     )
	(dolist (x new-nodes)
	  (omega~trace "Inserting node ~A" x)
	  (spds=insert-node x pds mapping substitution remove-hyps))

	;; Check out open nodes (remove old open node and insert new open nodes).
	(setf (pds~open-nodes pds)
	      (remove-if-not #'pdsn~open-node-p (append (pds~open-nodes pds) new-nodes)))
	
	;; Check out plan steps
	(setf (pdsj~successor (pdsc~an-just (pds~last-plan-step pds))) (pds~first-plan-step subpds))
	(setf (pdsj~predecessor (pdsc~an-just (pds~first-plan-step subpds))) (pds~last-plan-step pds))
	(setf (pds~last-plan-step pds) (pds~last-plan-step subpds))

	;; Maybe also check out a new updated agenda and proof context
	
	;; Check out environmental things!
	;; to be done!
	
	(pds~remove-proof-plan subpds)
	pds
      ))))

(defun spds=compose-subst-list (subst-list)
  (declare (edited  "06-AUG-2001")
	   (authors Sorge)
	   (input   "A list of substitutions.")
	   (effect  "None.")
	   (value   "One substitution composed of all single substitutions."))
  (cond ((null (cdr subst-list)) (car subst-list))
	((null (cddr subst-list)) (subst~compose-substitution (car subst-list)
							      (cadr subst-list)))
	(t (subst~compose-substitution (car subst-list)
				       (spds=compose-subst-list (cdr subst-list))))))

;;; Check with alpha equality!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another problem we should keep in mind: Newly introduced constants
;; etc., i.e. stuff in the environment, of the subPDS have to be
;; introduced into the environment of the originial PDS.
;; Maybe we will need an environmental hack (see also below).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ------ CAUTION! CONSTRUCTION AHEAD ------

;;; do not forget to alpha substitute parameters! (done, I think, VS)

;;;------------------ Inserting different types of nodes -------------------------------

(defun spds=insert-node (node pds mapping substitution hyps)
  (declare (edited  "06-AUG-2001")
	   (authors Sorge)
	   (input   "A node, a PDS, an alist of node-mappings, a substitution,"
		    "and a list of hypothesis nodes to be removed.")
	   (effect  "Inserts the NODE into PDS and adjusts it accordingly.")
	   (value   "The newly inserted and adjusted node."))
  (keim~set-name! node (pds~new-node-name pds))
  (spds=map-object (node~justification node) mapping)
  (setf (pdsn~hyps node) (spds=map-object (set-difference (pdsn~hyps node) hyps) mapping))
  (spds=apply-substitution node substitution)
  (pds~only-insert-node! node pds)
  node
  )

(defun spds=merge-conc-node (old-node new-node mapping substitution)
  (declare (edited  "06-AUG-2001")
	   (authors Sorge)
	   (input   "Two conclusion nodes, an alist of node-mappings and a substitution.")
	   (effect  "Merges new-node into old-node.")
	   (value   "A substitution mapping elements of new-node's formula to the corresponding elements in old-node."))
  (let* ((old-ctrl (pdsj~control (node~justification old-node)))
	 (new-just (node~justification new-node))
	 (new-ctrl (pdsj~control new-just))
	 (new-subst (subst~compose-substitution substitution
						(term~alpha-equal (node~formula new-node)
								  (node~formula old-node)))))
    (setf (pdsc~reasons new-ctrl) (append (pdsc~reasons old-ctrl) (pdsc~reasons new-ctrl)))
    (setf (pdsc~sponsors new-ctrl) (append (pdsc~sponsors old-ctrl) (pdsc~sponsors new-ctrl)))
    (setf (pdsc~unsponsors new-ctrl) (append (pdsc~unsponsors old-ctrl) (pdsc~unsponsors new-ctrl)))
    (setf (node~justification old-node) (spds=apply-substitution 
					 (spds=map-object new-just mapping)
					 new-subst))
    new-subst))
  
(defun spds=merge-hyp-node (old-node new-node mapping)
  (declare (edited  "05-AUG-2001")
	   (authors Sorge)
	   (input   "Two hypothesis nodes and a alist of node-mappings.")
	   (effect  "Merges new-node into old-node.")
	   (value   "A substitution mapping elements of new-node's formula to the corresponding elements in old-node."))
  (setf (pdsj~reasons (node~justification old-node))
	(append (pdsj~reasons (node~justification old-node))
		(pdsj~reasons (node~justification new-node))))
  (setf (pdsj~control (node~justification old-node))
	(spds=map-object (pdsj~control (node~justification old-node)) mapping))
  (term~alpha-equal (node~formula new-node)
		    (node~formula old-node)))

;;;------------------- Mapping objects containing nodes ---------------------------------

(defgeneric spds=map-object (object mapping &key (assoc-function #'rassoc))
  (declare (edited  "05-AUG-2001")
	   (authors Sorge)
	   (input   "An object and a mapping.")
	   (effect  "Replaces occuring nodes in the object when they are mapped in the mapping.")
	   (value   "The altered object."))
  (:method (object mapping &key (assoc-function #'rassoc))
	   (declare (ignore mapping assoc-function))
	   object)
  (:method ((node pdsn+node) mapping &key (assoc-function #'rassoc))
	   (let ((new-node (funcall assoc-function node mapping)))
	     (or (car new-node) node)))
  (:method ((list list) mapping &key (assoc-function #'rassoc))
	   (mapcar #'(lambda (obj) (spds=map-object obj mapping :assoc-function assoc-function)) list))
  (:method ((anode keim::pdsc=actual-node) mapping &key (assoc-function #'rassoc))
	   (let* ((old-node (pdsc~an-node anode))
		  (new-node (funcall assoc-function old-node mapping)))
	     (if new-node
		 (pdsc~an-create new-node (pdsc~an-just anode))
	       ;;(spds=associate-justification new-node old-node (pdsc~an-just anode)))
	       anode)))
  (:method ((ctrl pdsc+control) mapping &key (assoc-function #'rassoc))
	   (setf (pdsc~reasons ctrl)
		 (spds=map-object (pdsc~reasons ctrl) mapping :assoc-function assoc-function))
	   (setf (pdsc~sponsors ctrl)
		 (spds=map-object (pdsc~sponsors ctrl) mapping :assoc-function assoc-function))
	   (setf (pdsc~unsponsors ctrl)
		 (spds=map-object (pdsc~unsponsors ctrl) mapping :assoc-function assoc-function))
	   (setf (pdsc~successor ctrl)
		 (spds=map-object (pdsc~successor ctrl) mapping :assoc-function assoc-function))
	   (setf (pdsc~predecessor ctrl)
		 (spds=map-object (pdsc~predecessor ctrl) mapping :assoc-function assoc-function))
	   ctrl)
  (:method ((just pdsj+justification) mapping &key (assoc-function #'rassoc))
	   (setf (pdsj~above just) (spds=map-object (pdsj~above just) mapping :assoc-function assoc-function))
	   (setf (pdsj~below just) (spds=map-object (pdsj~below just) mapping :assoc-function assoc-function))
	   (setf (just~premises just) (spds=map-object (just~premises just) mapping :assoc-function assoc-function))
	   just)
  )

;;; not sure whether the following is actually needed!
(defun spds=associate-justification (new-node old-node old-just)
  (declare (edited  "05-AUG-2001")
	   (authors Sorge)
	   (input   "Two nodes and a justification.")
	   (effect  "None.")
	   (value   "A justification of the first node which corresponds to the given justification of the"
		    "second node."))
  
  )
  
;;;------------------- Alpha substitution in objects ---------------------------------

(defgeneric spds=apply-substitution (obj subst)
  (declare (edited  "06-AUG-2001")
	   (authors Sorge)
	   (input   "An object and a substitution.")
	   (effect  "Applys the substitution to all occuring formulas.")
	   (value   "The possibly changed object."))
  (:method (obj subst)
	   (declare (ignore obj))
	   (error ";;;SPDS=APPLY-SUBSTITUTION: ~A should be a substitution!" subst))
  (:method (obj (subst subst+substitution))
	   obj)
  (:method ((obj list) (subst subst+substitution))
	   (mapcar #'(lambda (x) (spds=apply-substitution x subst)) obj))
  (:method ((term term+term) (subst subst+substitution))
	   (data~replace-structs term (subst~domain subst) (subst~codomain subst)))
  (:method ((node pdsn+node) (subst subst+substitution))
	   (setf (node~formula node) (spds=apply-substitution (node~formula node) subst))
	   (spds=apply-substitution (node~justification node) subst)
	   node)
  (:method ((just pdsj+justification) (subst subst+substitution))
	   (setf (pdsj~parameters just) (spds=apply-substitution (pdsj~parameters just) subst))
	   (spds=apply-substitution (pdsj~above just) subst)
	   (spds=apply-substitution (pdsj~below just) subst)
	   just)
  )



;; ------ END OF CONSTRUCTION ------  

	
(defun spds=get-hyp-difference (mapping)
  (declare (edited  "30-JUL-2001")
	   (authors Sorge)
	   (input   "An association list.")
	   (effect  "None.")
	   (value   "A list of hypotheses that does not occur in the conclusion of the first cons cell."))
  (let* ((old-conc (caar mapping))
	 (new-conc (cdar mapping))
	 (new-hyps (mapcar #'(lambda (x) (cdr (assoc x mapping))) (pdsn~hyps old-conc))))
    (set-difference (pdsn~hyps new-conc) new-hyps)))

(defun spds==alist2pairlist (list)
  (mapcar #'(lambda (x) (list (cdr x) (car x))) list))

;;;---------------- Completeing the node mapping as well as possible ------------------------

(defun spds=complete-node-mapping (pds subpds node-mapping)
  (declare (edited  "27-JUL-2001")
	   (authors Sorge)
	   (input   "Two PDSs and an association list mapping nodes from PDS onto the hypotheses"
		    "and conclusion of subPDS.")
	   (effect  "None.")
	   (value   "The completed or cleaned node-mapping, where the conclusion is followed by the hyps."))
  (flet ((pds-node (node plan)
		   (if (node~p node)
		       (find node (prob~proof-steps plan))
		     (pds~label2node node plan)))
	 (associate-node (node mapping)
			 (let ((old-node (rassoc node mapping)))
			   (if old-node old-node
			     (let ((pds-node (pds~label2node (keim~name node) pds)))
			       (when pds-node (cons pds-node node)))))))
    (let* ((mapping (mapcan #'(lambda (map)
				(let ((fnode (pds-node (car map) pds))
				      (snode (pds-node (cdr map) subpds)))
				  (when (and fnode snode)
				    (list (cons fnode snode)))))
			    node-mapping))
	   (subpds-hyps (mapcar #'(lambda (hyp) (pds~label2node (keim~name hyp) subpds))
				(prob~proof-assumptions subpds)))
	   (subpds-conc (prob~proof-root subpds))
	   (comp-mapping (mapcar #'(lambda (n) (associate-node n mapping))
				 (cons subpds-conc subpds-hyps))))
      (unless (or (some #'null comp-mapping)
		  (some #'(lambda (node-cons)
			    (not (term~alpha-equal (node~formula (cdr node-cons))
						   (node~formula (car node-cons))
						   ;; here should go a list of primitives that are additionally bindable
						   ;; by alpha matching from the environments of the problems and PDSs
						   ;; (For more see remark below)
						   )))
			comp-mapping))
		  comp-mapping))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is a little problem and its solution:
;; (the latter should be implemented one day...)
;; If we want to merge a subPDS into a PDS that proves the subproblem
;; correctly however is not a decendant from the original PDS but
;; instead from a new problem, we cannot simply merge the stuff
;; together since there might be problems with constants that are
;; distinct in the different problem and PDS environments.
;; 
;; One solution is an extended term~alpha-equal test and a subsequent
;; enlarged substitution of equalized terms. This can be done by
;; providing additional terms to the add-binding key of term~alpha-equal
;; However, we would have to access the environments of the PDSs and
;; their problems directly to get all the keys. In KEIMs current form
;; this will be a hack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting SubPDS as Expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun spds~insert-expansion-pds (pds subpds &optional (node-mapping nil))
  (declare (edited  "27-JUL-2001")
	   (authors Sorge)
	   (input   "Two PDSs and an association list mapping nodes from PDS onto the hypotheses and"
		    "conclusion of subPDS.")
	   (effect  "Inserts the content of the subPDS into the PDS."
		    "The hypotheses and conclusions of subPDS are mapped to nodes in PDS according to NODE-MAPPING."
		    "If it is empty they are mapped with respect to the labels."
		    "If this is not possible the function fails."
		    "ATTENTION: some objects of subPDS might actually become objects of PDS."
		    "The subPDS is therefore subsequently discarded.")
	   (value   "The extended PDS."))
  (let* ((mapping (spds=complete-node-mapping pds subpds node-mapping)))
    (when (and mapping  ;;; open node thingy!!!!!!!!!!
	       (not (omega~warn "Here should go some stuff to check additional merging conditions.")))
      (omega~trace "Merging PDS ~A into PDS ~A!" pds subpds)
      (let* ((remove-hyps (spds=get-hyp-difference mapping))
	     (old-hyp-nodes (mapcar #'car (cdr mapping)))
	     (new-hyp-nodes (mapcar #'cdr (cdr mapping)))
	     (old-conc-node (caar mapping))
	     (new-conc-node (cdar mapping))
	     (new-nodes (set-difference (prob~proof-steps subpds) (cons new-conc-node new-hyp-nodes)))
	     (subst-list (mapcar #'(lambda (oh nh) (spds=merge-hyp-node oh nh mapping))
				 old-hyp-nodes new-hyp-nodes))
	     (substitution (spds=merge-conc-node old-conc-node new-conc-node mapping
						 (spds=compose-subst-list subst-list)))
	     )
	(dolist (x new-nodes)
	  (omega~trace "Inserting node ~A" x)
	  (spds=insert-node x pds mapping substitution remove-hyps))

	;; Check out open nodes (remove old open node and insert new open nodes).
	(setf (pds~open-nodes pds)
	      (remove-if-not #'pdsn~open-node-p (append (pds~open-nodes pds) new-nodes)))
	
	;; Check out plan steps
	(setf (pdsj~successor (pdsc~an-just (pds~last-plan-step pds))) (pds~first-plan-step subpds))
	(setf (pdsj~predecessor (pdsc~an-just (pds~first-plan-step subpds))) (pds~last-plan-step pds))
	(setf (pds~last-plan-step pds) (pds~last-plan-step subpds))

	;; Maybe also check out a new updated agenda and proof context
	
	;; Check out environmental things!
	;; to be done!
	
	(pds~remove-proof-plan subpds)
	pds
      ))))

