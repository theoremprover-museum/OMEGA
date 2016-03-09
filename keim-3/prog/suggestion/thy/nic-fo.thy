;;; -*- syntax: common-lisp; package: omega; base: 10; mode: keim -*-

(in-package :omega)

(eval-when (compile)
           (error "This file should not be compiled."))

(defvar nic*substitution (subst~create nil nil)
  "The global substitution to be mantained for the NIC calculus")

(defvar nic*substitution-hashtable (make-hash-table :test #'equal)
  "The substitution hashtable for backtracking to be mantained for the NIC calculus. This hashtable is
indexed by pairs of timestamps and labels of proof nodes in the pds.")

(defun nic~substitution-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The substitution hashtable."))
  nic*substitution-hashtable)

(defun nic~initialiaze-substitution-hashtable ()
  (setf nic*substitution-hashtable (make-hash-table :test #'equal))
  (setf nic*substitution (subst~create nil nil))
  (nic~print-substitution-hashtable-info))


(defun nic~substitution-hashtable2list ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The substitution hashtable represented as a list."))
   (let ((index-subst-pairs nil))   ;;; das hier muss doch irgendwie effizienter gehen?
     (maphash #'(lambda (x y) (push (list x y) index-subst-pairs))
	      (nic~substitution-hashtable))
     index-subst-pairs))

(defun nic~print-substitution-hashtable-info ()
  (omega~message "Current substution-hashtable: ~A" (nic~substitution-hashtable2list))
  (omega~message "Represented substitution: ~A" nic*substitution))    


;; Just for the demo with Andrew Ireland, I will not use this here because of the write/read-pds failure
;
;(defmethod node~formula :around ((node pdsn+node))
;  (let ((formula (call-next-method)))
;    (if nic*substitution
;        (prog1 (subst~apply nic*substitution formula))
;      formula)))

(defun nic~index (node)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "a node (or a name of node)")
	   (effect  "None")
	   (value   "an index entry used for indexeing the substitution-hashtable"))
  (let ((name (if (stringp node) node (keim~name node))))
    (list (get-universal-time) (string-upcase name))))

(defun nic~index-stamp-= (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (= (first index1) (first index2)))

(defun nic~index-stamp-< (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (< (first index1) (first index2)))

(defun nic~index-stamp-<= (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (<= (first index1) (first index2)))


(defun nic~index-node-eq (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (equal (second index1) (second index2))) 
    

(defun nic~index-node-eq-special (node1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "A node (or a node-name) and a index entry")
	   (effect  "None")
	   (value   "T iff node1 and the node-part of the index-entry are the same"))
  (let ((node-name (if (node~p node1) (keim~name node1) node1)))
    (string-equal node-name (second index2))))


(defun nic~get-substs-for-node (node)
  (declare (edited  "24-MAY-2000")
	   (authors Ceb)
	   (input   "A node")
	   (effect  "None")
	   (value   "The list of all substitutions for node in the substitution hashtable"))
  (let* ((mapping-indices
	  (remove-if #'nic~backtracked-entry-p 
		     (remove-if-not #'(lambda (x)
					(nic~index-node-eq-special node x))
				    (nic~suggestion-hashtable-indices))))
	 (corresp-entries (mapcar #'(lambda (x)
				      (gethash x (nic~substitution-hashtable)))
				   mapping-indices)))
    corresp-entries))

(defun nic~backtracked-entry-p (entry)
  (equal 'backtracked (second entry)))

(defun nic~add-subst (subst node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A substitution and a node ")
	   (effect  "If subst is consistent with the substitution hashtable, i.e., the"
		    "the global substitution, then subst is appended to them."
		    "Furthermore the value the nic*substitution is updated")
	   (value   "The updated value of  nic*substitution, representing the idempotent"
		    "substitution described by the hashtable"))
  (when (and (nic~subst-consistent-p nic*substitution subst)
	     (<= (length (nic~get-substs-for-node node)) 1))
    (setf (gethash (nic~index node) (nic~substitution-hashtable)) subst)
    (setf nic*substitution (nic~compute-subst-represented-by-hashtable))
    (nic~print-substitution-hashtable-info)
    (nic~substitution-hashtable)))
	    

(defun nic~del-subst (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "a node")
	   (effect  "Deletes the substitution stored for node in the substitution hashtable together"
		    "with all other substitutions having a timestamp greater than that associated"
		    "with node.")
	   (value   "The updated value of  nic*substitution, representing the idempotent"
		    "substitution described by the hashtable"))
  (let* ((index-for-node (find-if #'(lambda (x) (nic~index-node-eq-special node x)) (nic~suggestion-hashtable-indices)))
	 (indices-to-del (when index-for-node
			   (remove-if #'(lambda (x) (nic~index-stamp-< x index-for-node))
				      (nic~suggestion-hashtable-indices)))))
    (print index-for-node)
    (print indices-to-del)
    (dolist (index indices-to-del nil)
      (setf (gethash index (nic~substitution-hashtable)) 'backtracked))
    (setf nic*substitution (nic~compute-subst-represented-by-hashtable))
    (nic~print-substitution-hashtable-info) ;;; for debugging
    (nic~substitution-hashtable)))


(defun nic~suggestion-hashtable-indices ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None")
	   (effect  "None.")
	   (value   "All indices of the substitution hashtable."))
  (let ((list nil))
    (maphash #'(lambda (x y) (declare (ignore y)) (push x list)) (nic~substitution-hashtable))
    list))


(defun nic~suggestion-hashtable-entries ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None")
	   (effect  "None.")
	   (value   "All entries of the substitution hashtable."))
    (let ((list nil))
      (maphash #'(lambda (x y) (declare (ignore x)) (push y list)) (nic~substitution-hashtable))
      list))


(defun  nic~subst-consistent-p (subst1 subst2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "Two substitutions")
	   (effect  "None")
	   (value   "T iff both substitutions are compatible."))
  (let* ((idem-subst1 (subst~idem subst1))
	 (idem-subst2 (subst~idem subst2))
	 (dom1 (subst~domain idem-subst1))
	 (dom2 (subst~domain idem-subst2)))
    (setf testvar1 (car dom1))
    (setf testvar2 (car dom2))
    (not (intersection dom1 dom2 :test #'term~alpha-equal))))


(defun nic~compute-subst-represented-by-hashtable ()
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "None")
	   (effect  "None")
	   (value   "The idempotent substitution represented by the substitution hashtable"))
  (let* ((index-subst-pairs
	  (remove-if #'nic~backtracked-entry-p (nic~substitution-hashtable2list)))
	 (sorted-index-subst-pairs (sort index-subst-pairs
					#'(lambda (pair1 pair2)
					    (nic~index-stamp-< (first pair1)
							       (first pair2)))))
	(subst (subst~create nil nil)))
    (mapc #'(lambda (pair)
	      (setf subst (subst~compose-substitution subst (second pair))))
	  sorted-index-subst-pairs)
    (setf subst (subst~idem subst))))
    
    
	   
	
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-FORALL-I

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FORALL-I
	       (outline-mappings (((closed closed) NIC-FORALL-I-c)
				  ((existent existent) NIC-FORALL-I-a)
				  ((existent nonexistent) NIC-FORALL-I-b)))
	       (parameter-types term)
	       (help "The NIC universal elimination rule."))

(rule~defrule NIC-FORALL-I-c NIC-FORALL-I (in base)
  (parameters (X term+term "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Checking NIC universal introduction."))

(rule~defrule NIC-FORALL-I-a NIC-FORALL-I (in base)
  (parameters (X term+constant "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Application of NIC universal introduction."))

(rule~defrule NIC-FORALL-I-b NIC-FORALL-I (in base)
  (parameters (X term+term "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "NIC universal introduction backwards."))


(com~defcommand NIC-FORALL-I
  (argnames univ-line parameter line)
  (argtypes ndline term ndline)
  (arghelps "A Universal line to prove" "New parameter" "A line")
  (function nic=NIC-FORALL-I)
  (frag-cats rules quantifier introduction backward)
  (log-p T)
  (help "Introduce a NIC universal quantifier."))

(defun nic=NIC-FORALL-I (univ param line)
  (infer~compute-outline 'NIC-FORALL-I (list univ line) (list param)))

(agent~defmatrix NIC-FORALL-I
   (information :pl1)		 
   (agents (c-predicate (for univ-line)
                        (uses )
			(exclude line parameter)
			(level 1)		
                        (definition (when (and (nic~introduction-p (:node univ-line))
					       (not (agplan~repeated-line-p univ-line)))
				      (logic~universal-quantification-p univ-line))))
           (c-predicate (for univ-line)
                        (uses line)
			(exclude parameter)
			(level 1)		
                        (definition (pred1 univ-line line)))
           (c-predicate (for univ-line)
                        (uses line parameter)
			(level 3)		
                        (definition (pred2 univ-line line (:param parameter))))
           (s-predicate (for line)
                        (uses univ-line)
			(exclude parameter)
			(level 1)		
                        (definition (pred1 univ-line line)))
	   (s-predicate (for line)
                        (uses univ-line parameter)
			(level 3)		
                        (definition (pred2 univ-line line (:param parameter))))
	   (function    (for parameter)
                        (uses univ-line)
			(level 1)		
                        (definition (func1 univ-line)))) ;; Skolemisierung
   (predicates
    (pred1 (univ-line line)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (agplan~matching-term (logic~quantification-scope univ-line) line)))
    (pred2 (univ-line line parameter)
	   (and
	    (pred1 univ-line line)
	    (term~alpha-equal (agplan~matching-term (logic~quantification-scope univ-line) line)
			parameter)))
    (func1 (univ-line)
	   (hocnf~new-skolem-term-create (logic~quantification-bound-variable univ-line)
					 (remove
					  (logic~quantification-bound-variable univ-line)
					  (foci~free-vars (foci~active-pc))
					  :test #'data~equal)
					 (pds~environment (agplan~current-proof-plan))))))


(defun nic=nic-forall-i-func2 (univ-line)
  (multiple-value-bind (form skolemterms)
      (hocnf~skolemize (logic~quantification-scope univ-line)
		       (list (logic~quantification-bound-variable univ-line))
		       (foci~free-vars (foci~active-pc))
		       (pds~environment (agplan~current-proof-plan)))
	       (declare (ignore form))
	       (car skolemterms)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-I
	       (outline-mappings (((closed closed) NIC-EXISTS-I-c)
				  ((existent existent) NIC-EXISTS-I-a)
				  ((existent nonexistent) NIC-EXISTS-I-b)))
	       (parameter-types term position-list)
	       (help "The NIC existential introduction rule."))

(rule~defrule NIC-EXISTS-I-c NIC-EXISTS-I (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Check of NIC existential introduction."))

(rule~defrule NIC-EXISTS-I-a NIC-EXISTS-I (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (pds~node-supported-by-p C P)
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Application of NIC existential introduction."))
  
(rule~defrule NIC-EXISTS-I-b NIC-EXISTS-I (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList))
  (computations
   (F (orules=replace-at-positions A PList X)))
  (description "Backward application of NIC existential introduction."))

(com~defcommand NIC-EXISTS-I
  (argnames exists-line prem witness positionlist)
  (argtypes ndline ndline term position-list)
  (arghelps "Line to justify" "Premise" "Witness term" "List of term positions")
  (function nic=NIC-EXISTS-I)
  (frag-cats rules quantifier introduction backward)
  (log-p T)
  (help "Justify an NIC existential line from a particular instance."))
 
(defun nic=NIC-EXISTS-I (plan prem term position)
  (infer~compute-outline 'NIC-EXISTS-I (list plan prem) (list term position)))

(agent~defmatrix NIC-EXISTS-I 	
   (agents (c-predicate (for exists-line witness positionlist)
                        (uses )
			(exclude prem)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node exists-line))
					       (logic~existential-quantification-p exists-line))
				      (list
				       (data~replace-free-variables-and-rename
					(logic~quantification-bound-variable
					 exists-line) nil nil)
				       (data~positions
					(logic~quantification-scope exists-line)
					#'(lambda (y) (data~equal y (logic~quantification-bound-variable
								     exists-line))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-E-i
    (outline-mappings (((closed closed closed) NIC-EXISTS-E-i-c)
                       ((existent existent nonexistent) NIC-EXISTS-E-i-b)))
    (parameter-types termsym)
	       (help "The NIC existential elimination-i rule."))

(rule~defrule NIC-EXISTS-E-i-c NIC-EXISTS-E-i (in base)
  (parameters (X term+term "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Checking NIC existential elimination-i."))

(rule~defrule NIC-EXISTS-E-i-b NIC-EXISTS-E-i (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~node-supported-by-p C P1)
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (actions (P2 (sponsor H) (unsponsor P1)))
  (description "NIC existential elimination-i backwards"))

(com~defcommand NIC-EXISTS-E-i
  (argnames conc exline parameter subgoal)
  (argtypes ndline ndline termsym ndline)
  (arghelps "A Line to be proved" "An existential line" "A term" "The second premise line")
  (function nic=NIC-EXISTS-E-i)
  (frag-cats rules structural elimination backward quantifier)
  (log-p T)
  (help "Eliminate an existential quantifier."))

(defun nic=NIC-EXISTS-E-i (conc ex param prem2)
  (infer~compute-outline 'NIC-EXISTS-E-i (list conc ex prem2) (list param)))

(agent~defmatrix NIC-EXISTS-E-i 	
   (agents
;          (s-predicate (for exline)
;                       (uses )
;                       (exclude conc parameter subgoal)
;                       (level 1)
;                       (definition (logic~existential-quantification-p exline)))
           (s-predicate (for exline)
                        (uses subgoal)
			(exclude conc parameter)
			(level 5)
                        (definition (pred1 exline subgoal)))
	   (s-predicate (for exline)
                        (uses conc)
			(exclude parameter subgoal)
			(level 5)
                        (definition (pred1 exline conc)))
	   (c-predicate (for conc)
                        (uses )
			(exclude parameter subgoal exline)
			(level 3)
                        (definition (when (and (nic~introduction-p (:node conc))
					       (not (agplan~repeated-line-p conc)))
				      (pred2 conc))))
	   (c-predicate (for conc)
                        (uses exline)
			(exclude parameter subgoal)
			(level 3)
                        (definition (pred3 exline conc)))
	   (c-predicate (for conc)
                        (uses subgoal)
			(exclude parameter exline)
			(level 1)
                        (definition  (and (not (equal (:node conc) (:node subgoal)))
					  (term~alpha-equal conc subgoal))))
;	   (s-predicate (for subgoal)
;                        (uses )
;                        (level 3)
;                        (definition (pred2 subgoal)))
           (s-predicate (for subgoal)
                        (uses conc)
			(level 1)
                        (definition (and (not (equal (:node conc) (:node subgoal)))
                                         (term~alpha-equal conc subgoal))))
;           (s-predicate (for subgoal)
;                        (uses exline)
;                        (level 3)
;                        (definition (pred3 exline subgoal)))
;           (function (for parameter)
;                     (uses exline)
;                     (exclude conc subgoal)
;                     (level 1)
;                     (definition (func0 exline))) 
	   (function (for parameter)
		     (uses exline conc)
		     (level 10)
		     (definition (func1 exline conc)))  ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
;	   (function (for parameter)
;                     (uses exline subgoal)
;                     (level 10)
;                     (definition (func1 exline subgoal))) ;;suche matchbarer exlinescope in
;                                                 ;;supports schlage ergebnis matching vor
	   
	   )
   (predicates
    (pred1 (exline subgoal)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support (:node subgoal) #'(lambda (x)
						       (term~alpha-match
							(logic~quantification-scope exline)
							x)))))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (x)
						    (logic~existential-quantification-p x))))
    (pred3 (conc exline)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (x)
						     (term~alpha-equal x exline)))))
    (func0 (exline) (when (logic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when
	       (logic~existential-quantification-p exline)
	     (agplan~matching-term
	      (logic~quantification-scope exline)
	      (or (car 
		   (pds~find-node-support (:node goal) #'(lambda (x)
							   (and (term~p x)
								(agplan~matching-term  
								 (logic~quantification-scope exline)
								 x)))))
		  (logic~negation-constant)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-FORALL-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FORALL-E
	       (outline-mappings (((closed closed) NIC-FORALL-E-c)
				  ((existent existent) NIC-FORALL-E-a)
				  ((existent nonexistent) NIC-FORALL-E-b)
				  ((nonexistent existent) NIC-FORALL-E-f)
				 ))
	       (parameter-types term)
	       (help "The NIC universal elimination rule."))

(rule~defrule NIC-FORALL-E-c NIC-FORALL-E (in base)
  (parameters (Y term+term "The universally quantified formula"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (logic~universal-quantification-p Y)
   (term~alpha-equal (logic~quantification-scope Y) A)
   (nic=special-sidecond1 AY A X Y))
  (description "Checking NIC universal elimination."))

(defun nic=special-sidecond1 (AY A X Y)
  (term~alpha-equal (logic~quantification-scope Y) A))
	 

(rule~defrule NIC-FORALL-E-a NIC-FORALL-E (in base)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (pds~node-supported-by-p C P)
   (logic~universal-quantification-p Y)
   (nic=special-sidecond1 AY A X Y))
  (description "Application of NIC universal elimination."))


;(rule~defrule NIC-FORALL-E-f NIC-FORALL-E (in base)
;  (parameters (Y term+term "A term"))
;  (declarations
;   (type-variables CC)
;   (meta-variables (Y O) (AY O) (X CC) (A O)))
;  (conclusion (C () AY))
;  (premises (P () (forall (lam (X CC :lookup) A))))
;  (computations
;   (AY (orules=substitute-for-in Y X A)))
;  (actions ((support P) (sponsor C)))
;  (description "NIC Universal elimination forwards."))

(rule~defrule NIC-FORALL-E-b NIC-FORALL-E (in base)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () A))
  (computations
   (A (nic=id Y)))
  (actions ((support P) (sponsor C)))
  (description "NIC Universal elimination forwards."))


(defun nic=id (x) x)

;(defun nic=compute-forall (AY Y)
;  (logic~quantification-create (logic~universal-quantor :name :forall)
;                               Y
;                               AY))

(com~defcommand NIC-FORALL-E
  (argnames  line univ-line term)
  (argtypes ndline ndline term)
  (arghelps "A line" "Universal line" "Term to substitute")
  (function nic=NIC-FORALL-E)
  (frag-cats rules quantifier elimination forward)
  (log-p T)
  (help "Instantiate a NIC universal quantifier."))

(defun nic=NIC-FORALL-E (line uni term)
  (infer~compute-outline 'NIC-FORALL-E (list line uni) (list term)))

(agent~defmatrix NIC-FORALL-E 
   (agents
           (s-predicate (for univ-line)
                        (uses term)
			(level 3)
                        (definition (term~alpha-equal univ-line (:param term))))
	   (c-predicate (for line term)
			(multiple term)
			(uses )
			(exclude univ-line)
			(level 1)
                        (definition (when (not (agplan~repeated-line-p line))
				      (pred3 (:node line))))))
   (predicates
    (pred3 (line)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~universal-quantification-p term)
			     (let ((subst
				    (term~alpha-match
				     (logic~quantification-scope
				      term)
				     (node~formula line))))
			       (when subst 
				 (list (subst~apply subst term))))))
		       (agplan~str-pos-subforms (pds~node-supports line)))))
	     (list res)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-E-e
    (outline-mappings (((closed closed closed) NIC-EXISTS-E-e-c)
                       ((existent existent nonexistent) NIC-EXISTS-E-e-b)))
    (parameter-types termsym)
	       (help "The NIC existential elimination-e rule."))

(rule~defrule NIC-EXISTS-E-e-c NIC-EXISTS-E-e (in base)
  (parameters (X term+term "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Checking NIC existential elimination-e."))

(rule~defrule NIC-EXISTS-E-e-b NIC-EXISTS-E-e (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~node-supported-by-p C P1)
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (actions (P2 (sponsor H) (unsponsor P1)))
  (description "NIC existential elimination-e backwards"))

(com~defcommand NIC-EXISTS-E-e
  (argnames conc exline parameter subgoal)
  (argtypes ndline ndline termsym ndline)
  (arghelps "A Line to be proved" "An existential line" "A term" "The second premise line")
  (function nic=NIC-EXISTS-E-e)
  (frag-cats rules structural elimination backward quantifier)
  (log-p T)
  (help "Eliminate an existential quantifier."))

(defun nic=NIC-EXISTS-E-e (conc ex param prem2)
  (infer~compute-outline 'NIC-EXISTS-E-e (list conc ex prem2) (list param)))

(agent~defmatrix NIC-EXISTS-E-e 	
   (agents
;           (s-predicate (for exline)
;                        (uses )
;                        (level 1)
;                        (definition (logic~existential-quantification-p exline)))
           (s-predicate (for exline)
                        (uses subgoal)
			(exclude conc)
			(level 5)
                        (definition (pred1 exline subgoal)))
	   (s-predicate (for exline)
                        (uses conc)
			(exclude subgoal)
			(level 5)
                        (definition (pred1 exline conc)))
	   (c-predicate (for conc)
                        (uses )
			(exclude exline subgoal parameter)  
			(level 3)
                        (definition (when (not (agplan~repeated-line-p conc))
				      (pred2 conc))))
	   (c-predicate (for conc)
                        (uses exline)
			(exclude subgoal)
			(level 3)
                        (definition (pred3 exline conc)))
	   (c-predicate (for conc)
                        (uses subgoal)
			(exclude exline)
			(level 1)
                        (definition  (and (not (equal (:node conc) (:node subgoal)))
					  (term~alpha-equal conc subgoal))))
;	   (s-predicate (for subgoal)
;                        (uses )
;                        (level 3)
;                        (definition (pred2 subgoal)))
           (s-predicate (for subgoal)
                        (uses conc)
			(exclude exline)
			(level 1)
                        (definition (and (not (equal (:node conc) (:node subgoal)))
                                         (term~alpha-equal conc subgoal))))
           (s-predicate (for subgoal)
                        (uses exline)
			(exclude conc)
			(level 3)
                        (definition (pred3 exline subgoal)))
	   (function (for parameter)
		     (uses exline)
		     (exclude conc subgoal)
		     (level 1)
		     (definition (func0 exline))) 
	   (function (for parameter)
		     (uses exline conc)
		     (exclude subgoal)
		     (level 10)
		     (definition (func1 exline conc)))  ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
	   (function (for parameter)
		     (uses exline subgoal)
		     (exclude conc)
		     (level 10)
		     (definition (func1 exline subgoal))) ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
	   )
   (predicates
    (pred1 (exline subgoal)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support (:node subgoal) #'(lambda (x)
						       (term~alpha-match
							(logic~quantification-scope exline)
							x)))))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (x)
						    (logic~existential-quantification-p x))))
    (pred3 (conc exline)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (x)
						     (term~alpha-equal x exline)))))
    (func0 (exline) (when (logic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when
	       (logic~existential-quantification-p exline)
	     (agplan~matching-term
	      (logic~quantification-scope exline)
	      (or (car 
		   (pds~find-node-support (:node goal) #'(lambda (x)
							   (and (term~p x)
								(agplan~matching-term  
								 (logic~quantification-scope exline)
								 x)))))
		  (logic~negation-constant)))))))




(infer~deftactic NICTAC-FOWEAKEN
	       (outline-mappings (((existent existent) NICTAC-FOWEAKEN-a)))
	       (expansion-function nictac=expand-NICTAC-FOWEAKEN)
	       (parameter-types anything)
	       (help "The NIC FOWEAKENing tactic."))

(tac~deftactic NICTAC-FOWEAKEN-a NICTAC-FOWEAKEN (in base)
   (parameters (uni subst+substitution "The unifier."))       
   (premises P)
   (conclusions C)
   (sideconditions (nictac=NICTAC-FOWEAKEN-a-sidecond P C uni))
   (description "Application of the NIC Waekening tactic."))

(defun nictac=NICTAC-FOWEAKEN-a-sidecond (P C uni)
  (let ((res (and (subst~p uni)
		  (term~alpha-equal (subst~apply uni (node~formula P))
			      (subst~apply uni (node~formula C))))))
    (when res (progn (nic~add-subst uni C)
		     t))))
  

(com~defcommand NICTAC-FOWEAKEN
  (argnames lowerline upperline uni)
  (argtypes ndline ndline anything)
  (arghelps "Line to justify" "Already-derived line" "Unifier")
  (function nictac=NICTAC-FOWEAKEN)
  (frag-cats rules structural elimination introduction)
  (log-p T)
  (help "Justify a line from an unifiable earlier-derived line."))

(defun nictac=NICTAC-FOWEAKEN (lower upper uni)
  (infer~compute-outline 'NICTAC-FOWEAKEN (list lower upper) (list uni)))

(agent~defmatrix NICTAC-FOWEAKEN  
   (agents (c-predicate (for lowerline upperline uni)
                        (uses )
			(level 1)
                        (definition (when (not (agplan~repeated-line-p (:node lowerline)))
				      (pred1 (:node lowerline))))))
   (predicates
    (pred1 (a1)
	   (some #'(lambda (x)
		     (let ((uni (term~alpha-unify (node~formula x) (node~formula a1))))
		       (when uni (list x uni))))
		 (pds~node-supports a1)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional Tactics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic set-ext-expand
		 (outline-mappings (((existent existent) set-ext-expand-a)
                                    ((nonexistent existent) set-ext-expand-f)
                                    ((existent nonexistent) set-ext-expand-b)
				   ))
                 (expansion-function batac=expand-set-ext-expand)
		 (help "Set-Extensionality."))

(defun batac=set-ext-expand (line1 line2)
  (infer~compute-outline 'set-ext-expand (list line1 line2) nil))

(com~defcommand set-ext-expand
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The expanded conclusion line" "The contracted premise line")
  (function batac=set-ext-expand)
  (frag-cats tactics base elimination nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Set-Extensionality"))

(tac~deftactic set-ext-expand-f set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=set-ext-expand-f (formula L1))))
   (sideconditions (batac~set-equation-p (formula L1)))
   (description "Set-Extensionality in forward direction."))

(defun batac~set-equation-p (formula)
  (when (logic~equality-p formula)
    (let ((type-left (data~annotation (first (data~appl-arguments formula)))))
      (and (type~func-p type-left) (type~o-p (data~c-range type-left))))))

(defun batac~set-ext-contract-ap (l1 l2)
  (term~alpha-equal l1 (batac=set-ext-contract-b l2)))

(defun batac=set-ext-expand-f (l1)
  (let* ((left (first (data~appl-arguments l1)))
	 (right (second (data~appl-arguments l1)))
	 (argtype (data~c-domain (data~annotation left)))
	 (var-of-argtype (term~variable-create (gensym) argtype))
	 (in (th~definition-constant
	      (th~find-assumption 'in (th~find-theory 'typed-set))))
	 )
    (logic~quantification-create
     (logic~universal-quantor :name :forall)
     var-of-argtype
     (term~appl-create
      (logic~equivalence-constant)
      (list 
       (term~appl-create in (list var-of-argtype left))
       (term~appl-create in (list var-of-argtype right)))))))
					      

(tac~deftactic set-ext-expand-b set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=set-ext-expand-b (formula L2))))
   (sideconditions (batac~expanded-set-extensionality-p (formula L2)))
   (description "Set-Ext-Extensionality in backward direction."))


(defun batac~expanded-set-extensionality-p (formula)
  (when (logic~universal-quantification-p formula)
    (let ((scope (logic~quantification-scope formula))
	  (bound-var (logic~quantification-bound-variable formula)))
      (when (logic~conjunction-p scope)
	(let ((left (first (data~appl-arguments scope)))
	      (right (second (data~appl-arguments scope))))
	  (when (and (data~appl-p left) (data~appl-p right))
	    (let ((head-left (data~appl-function left))
		  (head-right (data~appl-function right))
		  (in (data~schema-range (th~definition-constant
					  (th~find-assumption 'in (th~find-theory 'typed-set))))))
	      (when (and (term~alpha-equal in head-left)
			 (term~alpha-equal in head-right))
		(let ((pred-left (second (data~appl-arguments left)))
		      (pred-right (second (data~appl-arguments right)))
		      (arg-left (first (data~appl-arguments left)))
		      (arg-right (first (data~appl-arguments right))))
		  (when (and (term~alpha-equal arg-left bound-var)
			     (term~alpha-equal arg-right bound-var))
		    (values t pred-left pred-right)))))))))))
		      
	      
(defun batac=set-ext-expand-b (formula)
  (multiple-value-bind (flag pred-left pred-right)
      (batac~expanded-set-extensionality-p formula)
    (when flag (term~appl-create (logic~equality-constant) (list pred-left pred-right)))))

(tac~deftactic set-ext-expand-a set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=set-ext-expand-ap (formula L1) (formula L2)))
   (description "Set-Extensionality in test direction."))

(defun batac=set-ext-expand-ap (l1 l2)
  (term~alpha-equal l2 (batac=set-ext-expand-f l1)))



(defun batac=expand-set-ext-expand (outline parameters)
;  (let ((precond (second outline))  
;        (conc (first outline)))     
;    (tacl~init outline)
;    (tacl~sequence
;     (dummy ('exte (list conc precond) nil)))
;    (tacl~end)))
  (omega~message "Expansion function has still to be implemented ... Chris"))


(agent~defmatrix set-ext-expand
   (agents (c-predicate (for conclusion)
                        (uses premise)
			(level 1)
                        (definition (when (not (agplan~repeated-line-p (:node conclusion)))
				      (batac~set-ext-contract-ap premise conclusion))))	
           (c-predicate (for conclusion)
			(exclude premise)
			(level 1)
                        (definition  (when (not (agplan~repeated-line-p (:node
								      conclusion)))
				       (batac~expanded-set-extensionality-p conclusion))))
           (s-predicate (for premise)
                        (uses conclusion)
			(level 1)
                        (definition  (batac~set-ext-contract-ap premise conclusion)))
;           (s-predicate (for premise) not needed in nic because of backward seach
;                        (exclude conclusion)
;                        (level 1)
;                        (definition (batac~set-equation-p premise)))))
	   ))

;;;;;;  

(infer~deftactic set-ext-contract
		 (outline-mappings (((existent existent) set-ext-contract-a)
                                    ((nonexistent existent) set-ext-contract-f)
				    ((existent nonexistent) set-ext-contract-b)
				   ))
                 (expansion-function batac=contract-set-ext-expand)
		 (help "Set-Extensionality."))

(defun batac=set-ext-contract (line1 line2)
  (infer~compute-outline 'set-ext-contract (list line1 line2) nil))

(com~defcommand set-ext-contract
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The contracted conclusion line" "The expanded premise line")
  (function batac=set-ext-contract)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Set-Extensionality"))

(tac~deftactic set-ext-contract-b set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=set-ext-contract-b (formula L2))))
   (sideconditions (batac~set-equation-p (formula L2)))
   (description "Set-Extensionality in forward direction."))

(defun batac=set-ext-contract-b (l2)
  (let* ((left (first (data~appl-arguments l2)))
	 (right (second (data~appl-arguments l2)))
	 (argtype (data~c-domain (data~annotation left)))
	 (var-of-argtype (term~variable-create (gensym) argtype))
	 (in (th~definition-constant
	      (th~find-assumption 'in (th~find-theory 'typed-set))))
	 )
    (logic~quantification-create
     (logic~universal-quantor :name :forall)
     var-of-argtype
     (term~appl-create
      (logic~equivalence-constant)
      (list 
       (term~appl-create in (list var-of-argtype left))
       (term~appl-create in (list var-of-argtype right)))))))
					      
(tac~deftactic set-ext-contract-f set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=set-ext-expand-b (formula L1))))
   (sideconditions (batac~expanded-set-extensionality-p (formula L1)))
   (description "Set-Ext-Extensionality in backward direction."))


(tac~deftactic set-ext-contract-a set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac~set-ext-contract-ap (formula L1) (formula L2)))
   (description "Set-Extensionality in test direction."))


(defun batac=contract-set-ext-expand (outline parameters)
;  (let ((precond (second outline))  
;        (conc (first outline)))     
;    (tacl~init outline)
;    (tacl~sequence
;     (dummy ('exte (list conc precond) nil)))
;    (tacl~end)))
  (omega~message "Expansion function has still to be implemented ... Chris"))


(agent~defmatrix set-ext-contract
   (agents (c-predicate (for conclusion)
                        (uses premise)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~set-ext-contract-ap premise conclusion))))
           (c-predicate (for conclusion)
			(exclude premise)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~set-equation-p conclusion))))
           (s-predicate (for premise)
                        (uses conclusion)
			(level 1)
                        (definition  (batac~set-ext-contract-ap premise conclusion)))
;	   (s-predicate (for premise)  not needed in nic because of backward search
;                        (exclude conclusion)
;                        (level 1)
;                        (definition (batac~expanded-set-extensionality-p premise)))))
	   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Agents for BASE Tactics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; redefine commend to extend frac cats list, chris; additional farc cats 'definition is
;;; used in bb~...leq-p

(com~defcommand defn-expand*
  (argnames line definition position-list)
  (argtypes ndline thy-assumption position-list)
  (arghelps "Line to be rewritten" "Definition to be expanded" "Positions of occurrences")
  (function gentac=defn-expand*)
  (frag-cats tactics generic definition)
  (defaults gentac=defn-expand*-defaults)
  (log-p T)
  (level 5)
  (help "Expand multiple definition occurrences in a line."))

(agent~defmatrix defn-expand*
   (agents (s-predicate (for line definition)
                        (exclude position-list)
			(multiple definition)
			(level 3)		
                        (definition  (when (not (agplan~repeated-line-p (:node
								      line)))
				       (let ((defs (agplan~contained-definitions line)))
					 (when defs (list defs))))))
	   (function (for position-list)
		     (uses line definition)
		     (level 3)
		     (definition (pred1 line (:param definition)))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (let ((const (th~definition-constant definition)))
					    (data~equal (if (data~schema-p const)
							    (data~schema-range const)
							  const)
							x))))))))

;;; redefine commend to extend frac cats list, chris; additional farc cats 'definition is
;;; used in bb~...leq-p

(com~defcommand defn-contract*
  (argnames line definition position-list)
  (argtypes ndline thy-assumption position-list)
  (arghelps "Line to be rewritten" "Definition to be contracted" "Positions of occurrences")
  (function gentac=defn-contract*)
  (frag-cats tactics generic definition)
  (defaults gentac=defn-contract*-defaults)
  (log-p T)
  (level 5)
  (help "Contract multiple definition occurrences in a line."))

(agent~defmatrix defn-contract*
   (agents (c-predicate (for line definition)
                        (exclude position-list)
			(multiple definition)
			(level 3)
                        (definition (when (and (nic~introduction-p (:node line))
					       (not (agplan~repeated-line-p (:node
									  line))))
				      (let ((defs (agplan~contained-definitions line)))
					(when defs (list defs))))))
	   (function (for position-list)
		     (uses line definition)
		     (level 3)
		     (definition (pred1 line (:param definition)))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (let ((const (th~definition-constant definition)))
					    (data~equal (if (data~schema-p const)
							    (data~schema-range const)
							  const)
							x))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Additional Stuff that is needed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Malte; chris
(defmethod oc=use-nic-agents ()
;  (unless (find :nic-pl *features*)
;    (load (concatenate 'string (*user-top-dir*) "keim-3/prog/suggestion/nic-pl.thy")))
  (csm~set-considered-commands
   '(NICDUMMY-OR-E-R NICTAC-IMP-E NICTAC-NEG-E NICTAC-AND-E-L NIC-FORALL-E NICDUMMY-OR-E-L
		     NIC-NEG-I NIC-FORALL-I NIC-EXISTS-I NIC-FALSE-C NIC-OR-I-R NICTAC-AND-E-R
		     NICTAC-FOWEAKEN NIC-IMP-I NIC-EXISTS-E-I NICTAC-OR-E NICTAC-WEAKEN
		     NIC-OR-I-L NIC-EXISTS-E-E NIC-AND-I
		     ;additional tactics extending NIC
		     NICTAC-MODTOLL
		     ; SOLVED-BY-FO-ATP
		     ; SOLVED-OR-RESULT-BY-LEO
		     COUNTEREXAMPLE-BY-SATCHMO
		     SOLVED-BY-PL-ATP
		     ; SOLVED-BY-LEO-PL-ATP
		     SET-EXT-EXPAND SET-EXT-CONTRACT DEFN-EXPAND* DEFN-CONTRACT*
		     ; SIMPLIFY-GOAL-WITH-CAS
		     ))
  (csm~set-considered-classifiers nil)
  (auto~set-criteria)
  (agent~init-resources))




;;;;;;;;;;;;;;;;;;;;   For Mateja's Learning stuff; she needs tactics ;;;;;;;;;;;

(infer~deftactic NICTAC-FORALL-I
		 (outline-mappings (((existent nonexistent) NICTAC-FORALL-I-b)))
		 (expansion-function nictac=expand-NICTAC-FORALL-I)
		 (help "The NIC tactic for Conjunction elimination right."))


(tac~deftactic NICTAC-FORALL-I-b NICTAC-FORALL-I (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-FORALL-I-compute1 (formula L1))))
   (sideconditions (logic~universal-quantification-p (formula L2)))
   (description ""))

(defun nictac=compute-foralle-instance (formula term-list)
  (let ((new-term formula))
    (dolist (x term-list)
      (when (logic~universal-quantification-p new-term)
	(setq new-term (beta~contract
			(term~appl-create (car (data~appl-arguments new-term))
					  (list x))))))
    new-term))

(defun nictac=NICTAC-FORALL-I-compute1 (node)
  (nictac=compute-foralle-instance
   node
   (list (hocnf~new-skolem-term-create (logic~quantification-bound-variable node)
				       (remove
					(logic~quantification-bound-variable node)
					(foci~free-vars (foci~active-pc))
					:test #'data~equal)
				       (pds~environment (agplan~current-proof-plan))))))

(defun nictac=expand-NICTAC-FORALL-I (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-FORALL-I outline paramters)
  (tacl~end))

(com~defcommand NICTAC-FORALL-I
  (argnames conc prem)
  (argtypes ndline ndline)
  (arghelps "Conclusion" "Premise")
  (function nictac=NICTAC-FORALL-I)
  (frag-cats tactics base elimination)
  (log-p T)
  (help ""))

(defun nictac=NICTAC-FORALL-I (conc prem)
  (infer~compute-outline 'NICTAC-FORALL-I (list conc prem) nil))

