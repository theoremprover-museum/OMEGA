;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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


(mod~defmod DECOMP 
            :uses (cl data delta env extdelta integ keim lit logic node omega pds pdsn plco pos r2ntop ref split subst term)
            :documentation "The decomposition functions."
            :exports (
                      
                      decomp~all-subst-terms-of-quantified-var
                      decomp~get-conclusion-decomp
                      decomp~get-premise-decomp
                      decomp~new-decompose-unit
                      decomp~update-unit
                      ))

;;; The following functions are internal in other modules and should not be used:
;;; (pds=remove-node!)


#| ------------------------------------------------------- Main Functions  ----------------------------------------------------- |#


(defun decomp~get-conclusion-decomp (current-node current-unit workon-units update-units)
  (let* ((formula (node~formula current-node)))
    (cond ((logic~universal-quantification-p formula)
	   (decomp=decomposition-rule-e-forall current-node current-unit workon-units update-units)) 
	  ((logic~existential-quantification-p formula)
	   (decomp=choose-rule-for-e-exists current-node current-unit workon-units update-units))
	  ((logic~implication-p formula)
	   (decomp=choose-rule-for-e-implies current-node current-unit workon-units update-units))  
	  ((logic~conjunction-p formula)
	   (decomp=decomposition-rule-e-and current-node current-unit workon-units update-units))
	  ((logic~disjunction-p formula)
	   (decomp=choose-rule-for-e-or current-node current-unit workon-units update-units)) 
	  ((logic~negation-p formula)
	   (decomp=decomposition-rule-e-pushneg current-node current-unit workon-units update-units))
	  ((logic~equivalence-p formula)
	   (decomp=decomposition-rule-e-equiv current-node current-unit workon-units update-units))
	  (t ;; -> literal + ground + planned-node 
	   (decomp=remove-node-from-unit-decomposition-nodes! current-node (cons current-unit workon-units) update-units)))))

(defun decomp~get-premise-decomp (current-node current-unit workon-units update-units)
  (let* ((formula (node~formula current-node)))
    (cond ((logic~universal-quantification-p formula)
	   (decomp=decomposition-rule-forall-i current-node current-unit workon-units update-units))
	  ((logic~existential-quantification-p formula)
	   (decomp=decomposition-rule-m-choice current-node current-unit workon-units update-units))
	  ((logic~implication-p formula)
	   (decomp=choose-rule-for-implies-i current-node current-unit workon-units update-units))
	  ((logic~conjunction-p formula)
	   (decomp=decomposition-rule-and-i current-node current-unit workon-units update-units))
	  ((logic~disjunction-p formula)
	   (decomp=choose-rule-for-or-i current-node current-unit workon-units update-units))
	  ((logic~negation-p formula)
	   (decomp=decomposition-rule-pushneg-i current-node current-unit workon-units update-units))
	  ((logic~equivalence-p formula)
	   (decomp=decomposition-rule-equiv-i current-node current-unit workon-units update-units))
	  (t
	   (decomp=remove-node-from-unit-decomposition-nodes! current-node (cons current-unit workon-units) update-units)))))

#| --------------------------------- Check NODES TO DECOMPOSE + CREATE NEW DECOMPOSE-UNIT ------------------------------------------- |#


(defun decomp=check-nodes-to-decompose (nodes delta-relation conclusion-node)
  (declare (edited  "18-APR-1996")
	   (authors Ameier)
	   (input   "A list of (newly created) pdsnodes, the current delta-relation and the current"
		    "conclusion node.")
	   (effect  "None.")
	   (value   "A list of those nodes of the input-list which correspond to"
		    "unit clauses concerning the delta-relation (-> needed as unit-premises"
		    "in sspu) or which contain recursive existential quantifiers."
		    "Both findings necessitate further decomposition."))
  (if r2ntop*check-flag
      (if nodes
	  (let ((pairs (extdelta~relation-pairs delta-relation)))
	    (remove-duplicates
	     (append
	      ;; nodes which accord to unit-clauses
	      (remove-if-not #'(lambda (node)
				 (let ((clauses-of-node
					(remove-duplicates (mapcar #'extdelta~clause
								   (remove-if-not #'(lambda (pair)
										      (eq (extdelta~pdsnode pair) node))
										  pairs)))))
				   (remove-if-not #'cl~unit-p clauses-of-node)))
			     nodes)
	      ;; nodes which contains existential quantified Variables who really stand with skolems in the ref-graph
	      (decomp=get-rec-ex-nodes nodes delta-relation conclusion-node))))
	nil)
    nodes))

(defun decomp=get-rec-ex-nodes (list-of-nodes delta-relation conclusion-node)
  (declare (edited  "16-APR-1996")
	   (authors Ameier)
	   (input   "A list of nd-nodes, the current delta-relation and the current conclusion-node.")
	   (effect  "None.")
	   (value   "The list of nodes which contains a (possibly recursively nested) existential"
		    "subformula corresponding to literals wit skolem constants in ref-graph."
		    "That means the function looks for truly necessary (recursive) existential nodes."))
  (if list-of-nodes
      (let* ((head (first list-of-nodes))
	     (tail (rest list-of-nodes))
	     (flag (if (eq head conclusion-node) 
		       nil
		     't))
	     (pairs-to-node (remove-if-not #'(lambda (pair)
					       (eq (extdelta~pdsnode pair) head))
					   (extdelta~relation-pairs delta-relation))))
	(multiple-value-bind
	    (rec-ex bound)
	    (decomp=recursive-existential-p (node~formula head) (pos~list-position '()) flag nil pairs-to-node)
	  (declare (ignore bound))
	  (let ((back (decomp=get-rec-ex-nodes tail delta-relation conclusion-node)))
	    (if rec-ex
		(cons head back)
	      back))))
    nil))

(defun decomp=recursive-existential-p (formula pre-position flag bound pairs-with-old-pre-position)
  (declare (edited  "16-APR-1996")
	   (authors Ameier)
	   (input   "A formula, a position of the formulae history,"
		    "a flag: nil for planned node, t otherwise"
		    "and a flag bound nil and the delta-relation.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t if the formula contains recursive existential"
		    "       subformulas, otherwise nil."
		    "Second: t if the formula contains an arbitrary existential"
		    "        subformula  bound by a forall."))
  (let* ((pairs-with-pre-position (remove-if-not #'(lambda (old-pair)
						     (pos~prefix-p pre-position (extdelta~formula-position old-pair)))
						 pairs-with-old-pre-position)))
    (cond ((null pairs-with-pre-position)
	   (values nil bound))
	  ((logic~universal-quantification-p formula)
	   (if (null flag)
	       (values 't bound)
	     (multiple-value-bind
		 (rec-ex bound-ex)
		 (decomp=recursive-existential-p (data~abstr-scope (first (data~appl-arguments formula)))
						 (pos~add-end 0 (pos~add-end 1 pre-position))
						 flag 't pairs-with-pre-position)
	       (declare (ignore bound-ex))
	       (values rec-ex 't))))
	  ((logic~existential-quantification-p formula)
	   (if flag
	       (values 't bound)
	     (multiple-value-bind
		 (rec-ex bound-ex)
		 (decomp=recursive-existential-p (data~abstr-scope (first (data~appl-arguments formula)))
						 (pos~add-end 0 (pos~add-end 1 pre-position))    				    
						 flag 't pairs-with-pre-position)
	       (declare (ignore bound-ex))
	       (values rec-ex 't))))
	  ((logic~implication-p formula)
	   (multiple-value-bind
	       (rec-ex1 bound-ex1)
	       (decomp=recursive-existential-p (first (data~appl-arguments formula))
					       (pos~add-end 1 pre-position)
					       (not flag) bound pairs-with-pre-position)
	     (multiple-value-bind
		 (rec-ex2 bound-ex2)
		 (decomp=recursive-existential-p (second (data~appl-arguments formula))
						 (pos~add-end 2 pre-position)
						 flag bound pairs-with-pre-position)
	       (values (or rec-ex1 rec-ex2) (and bound-ex1 bound-ex2)))))
	  ((logic~conjunction-p formula)
	   (multiple-value-bind
	       (rec-ex1 bound-ex1)
	       (decomp=recursive-existential-p (first (data~appl-arguments formula))
					       (pos~add-end 1 pre-position)
					       flag bound pairs-with-pre-position)
	     (multiple-value-bind
		 (rec-ex2 bound-ex2)
		 (decomp=recursive-existential-p (second (data~appl-arguments formula))
					       (pos~add-end 2 pre-position)
					       flag bound pairs-with-pre-position)
	       (values (or rec-ex1 rec-ex2) (and bound-ex1 bound-ex2)))))
	  ((logic~disjunction-p formula)
	   (multiple-value-bind
	       (rec-ex1 bound-ex1)
	       (decomp=recursive-existential-p (first (data~appl-arguments formula))
					       (pos~add-end 1 pre-position)
					       flag bound pairs-with-pre-position)
	     (multiple-value-bind
		 (rec-ex2 bound-ex2)
		 (decomp=recursive-existential-p (second (data~appl-arguments formula))
						 (pos~add-end 2 pre-position)
						 flag bound pairs-with-pre-position)
	       (values (or rec-ex1 rec-ex2) (and bound-ex1 bound-ex2)))))
	  ((logic~negation-p formula)
	   (multiple-value-bind
	       (rec-ex bound-ex)
	       (decomp=recursive-existential-p (first (data~appl-arguments formula))
					       (pos~add-end 1 pre-position)
					       (not flag) bound pairs-with-pre-position)
	     (values rec-ex bound-ex)))
	  ((logic~equivalence-p formula)
	   (let* ((implies (env~lookup-object 'implies (pds~environment omega*current-proof-plan)))
		  (and (env~lookup-object 'and (pds~environment omega*current-proof-plan)))
		  (arguments (data~appl-arguments formula))
		  (and-formula (term~appl-create and (list (term~appl-create implies arguments)
							   (term~appl-create implies (reverse arguments))))))
	     (multiple-value-bind
	       (rec-ex bound-ex)
	       (decomp=recursive-existential-p and-formula
					       pre-position
					       flag bound pairs-with-pre-position)
	       (values rec-ex bound-ex))))
	  (t
	   (values nil bound)))))


(defun decomp~new-decompose-unit (conclusion-node ref-graph delta-relation &key (compute-plco 't) (pre-selected-nodes nil))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "Coherent conclusion-node, refutation graph and delta-relation.")
	   (effect  "None.")
	   (value   "A decompose-unit with given conclusion-node, extdelta-relation and refutataion graph,"
		    "and nodes-to-decompose computed from all nodes given by the extdelta-relation."
		    "Depending from some global and flag settings integral-formulas, plco-pairs and"
		    "pre-selctet-nodes are computed and set."))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Creating a new decompose unit of: ~A ~A ~A"
		   conclusion-node
		   ref-graph
		   delta-relation))
  
  (let* ((initial-clauses (ref~clause-nodes ref-graph))
	 (nodes-of-initial-clauses (remove-duplicates
				    (mapcar #'extdelta~pdsnode
					    (remove-if-not #'(lambda (x)
							       (member (extdelta~clause x) initial-clauses))
							   (extdelta~relation-pairs delta-relation)))))
	 (integral-formulas (if r2ntop*integral-formulas
				(integ~get-integral-formulas (remove conclusion-node nodes-of-initial-clauses))
			      nil))
	 (nodes-to-decompose (decomp=check-nodes-to-decompose nodes-of-initial-clauses delta-relation conclusion-node))
	 (plco-pairs (if compute-plco 
			 (plco~compute-compare-pair-list ref-graph)
		       compute-plco))
	 (new-unit (r2ntop~create-decompose-unit ref-graph
						 delta-relation
						 plco-pairs
						 nodes-to-decompose
						 conclusion-node
						 integral-formulas
						 pre-selected-nodes)))

    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: The new-unit: ~A"
		     new-unit))

    new-unit))

#| ----------------------------------------------------------- AUXILIARIES -------------------------------------------------------- |#

(defun decomp=remove-node-from-unit-nodes-to-decompose! (node unit)
  (declare (edited  "06-APR-1998")
	   (authors Ameier)
	   (input   "A pds node and a decompose unit.")
	   (effect  "The node is removed from the unit's nodes-to-decompose"
		    "slot.")
	   (value   "The updated decompose unit."))
  (let* ((nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit)))
    (when (find node nodes-to-decompose)
      (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (remove node nodes-to-decompose)))
    unit))

(defun decomp=remove-node-from-unit-decomposition-nodes! (node workon-units update-units)
  (declare (edited  "06-APR-1998")
	   (authors Ameier)
	   (input   "A node, a list of decompose units (the workon units) and a second list"
		    "of decompose units (the update units).")
	   (effect  "The node is removed from the unit's nodes-to-decompose slot.")
	   (value   "Undefined."))
  (mapcar #'(lambda (unit)
	      (decomp=remove-node-from-unit-nodes-to-decompose! node unit))
	  update-units)
  (mapcar #'(lambda (unit)
	      (decomp=remove-node-from-unit-nodes-to-decompose! node unit))
	  workon-units))

(defun decomp=delta-relation-contains-planned-node-p (delta-relation thm-node)
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A planned pdsnode and a second pdsnode.")
	   (effect  "None.")
	   (value   "T if second pdsnode is member of the planned-pdsnode's refutation Graph."
		    "Nil otherwise."))
    (find thm-node (extdelta~relation-pairs delta-relation) :test #'(lambda (node pair)
								      (eq node (extdelta~pdsnode pair)))))

(defun decomp=update-unit-by-subst-term (unit forall-node subst-term non-quantified-node)
  (declare (edited  "06-APR-1998")
	   (authors Ameier)
	   (input   "A decompose unit, a forall-node, a subst-term a the according non quantified node.")
	   (effect  "The unit is updated by replacing forall-node durch non-quantified-node (delta-relation+"
		    "ref-graph).")
	   (value   "The updated unit."
		    "REMARK: In the single unit, internal-node check is set to 't."))

  ;; Added the following if statement at 5.6.2000 after debugging 5 hours problem set002
  ;; The problem was, that one decomposition unit had a forall-quantified node (MEMBER-OF-UNION-IS-MEMBER-OF-ONE-SET) in its
  ;; nodes-to-decompose with instantiation c1. Another decomposition unit uses clauses to the same node and with the same
  ;; instantiation, but it has the node not in its nodes-to-decompose slot!
  ;; Hence, the first part of the updating was executed for this second unit (namely that the clause was instantiated with c1 and
  ;; the corresponding ground-substitution pairs were removed from the ground-substitutions of the clauses), but the second part
  ;; (namely the updating of the extdelta-relation) was not done correctly. Since the function decomp~update-unit checks whether
  ;; a node is in the node-to-decompose of a unit it did nothing! As result, the decompose unit was in a incorrect status, since the
  ;; clause was already changed, but not the extdelta-relation.
  ;; I try to avoid this problem by checking also in this function whteher the forall-node is in the nodes-to-decompose slot of the
  ;; unit. If this is not the case nothing happens for the unit.
  ;; AMeier

  (if (find forall-node (r2ntop~decompose-unit-nodes-to-decompose unit))
      ;; forall-node is in nodes-to-decompose -> Change the clauses and the extdelta-relation such that they correspond to the
      ;; instantiated line
      
      (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	     (ref-graph (r2ntop~decompose-unit-ref-graph unit))
	     (clause-var-pair-list (decomp=all-clauses-for-subst-term forall-node delta-relation subst-term))
	     (clauses-for-subst (mapcar #'first clause-var-pair-list)))
	
	;; update the clauses in ref-graph by applying instantiation on it
	(decomp=apply-instantiation ref-graph clause-var-pair-list)
	
	(when clauses-for-subst   ;; exitiieren ueberhaupt entsprechende Substitutiierte Klauseln ?
	  ;; Beachte: bei decomp~update-unit hat :clauses nil eine andere Bedeutung !
	  ;; update delta-relation + nodes-to-decompose + integral-formulas
	  (decomp~update-unit unit forall-node
			      (list (list non-quantified-node (list (list (pos~list-position '(1 0))
									  (pos~list-position '())))))
			      nil (list non-quantified-node)
			      :clauses clauses-for-subst
			      :remove-replace-node nil
			      :internal-node-check 't))
	unit)

    ;; forall-node is not in nodes-to-decompose -> Don't change the unit
    nil))

(defun decomp=update-units (list-of-units node-to-replace
					  pairs-of-nodes-to-add-and-del-pos-add-pos
					  nodes-to-remove-from-integrals
					  nodes-to-add-to-integrals
					  &key
					  (clauses nil)
					  (remove-replace-node 't))
  (declare (edited  "06-APR-1998")
	   (authors Ameier)
	   (input   "A list of decompose units, a node to be replaced"
		    "and a list of nodes for new, and an according list of same length"
		    "containing pairs of position-prefixes to be removed as well as position"
		    "prefixes to be added.")
	   (effect  "The unit is updated by replacing node-to-remove in the decomposition"
		    "nodes of unit by the nodes-to-add and the delta-relation is updated too,"
		    "if node-to-replace is in the unit decomposition-nodes."
		    "ATTENTION: The conclusion-node is not changed."
		    "REMARK: In the single units, internal-node check is set to 't.")
	   (value   "The updated units."))
  (mapcar #'(lambda (unit)
	      (decomp~update-unit unit
				  node-to-replace
				  pairs-of-nodes-to-add-and-del-pos-add-pos
				  nodes-to-remove-from-integrals
				  nodes-to-add-to-integrals
				  :clauses clauses
				  :remove-replace-node remove-replace-node
				  :internal-node-check 't))
	  list-of-units))

#| VORSICHT:

   Beim Updaten der Unit muss die conclusion-node bereits auf die NEUE conclusion-node gesetzt sein, da der check-to-decompose
   nicht mehr ueber open-node-p sondern ueber eq mit der conclusion-node laeuft (welche Knoten in DIESER Unit als offene Knoten
   behandelt werden muessen).
   Dies sollte NICHT kollidieren mit dem internal-node-check !!

|#

(defun decomp~update-unit (unit node-to-replace
				pairs-of-nodes-to-add-and-del-pos-add-pos
				nodes-to-remove-from-integrals
				nodes-to-add-to-integrals
				&key
				(clauses nil)
				(remove-replace-node 't)
				(internal-node-check nil)
				)
  (declare (edited  "30-JUL-1996")
	   (authors Ameier)
	   (input   "A unit to be updated, a node to be replaced"
		    "and a list of nodes for new, and an according list of same length"
		    "containing pairs of position-prefixes to be removed as well as position"
		    "prefixes to be added."
		    "REMARK: Keyword internal-node-check: if this keyword is true,the replacement"
		    "        is done only if the node-to-replace is not eq to the conclusion-node"
		    "        of the decompose unit but is in the nodes-to-decompose of the triple.")
	   (effect  "The unit is updated by replacing node-to-remove in the decomposition"
		    "nodes of unit by the nodes-to-add and the delta-relation is updated too,"
		    "if node-to-replace is in the unit decomposition-nodes."
		    "ATTENTION: The conclusion-node is not changed.")
	   (value   "The updated unit."
		    "WARNING: THE CONCLUSION NODE OF THE UNIT HAS TO BE ALREADY UPDATED IF THIS FUNCTION"
		    "         IS CALLED !!!"))
  ;; wenn internal-node-check nil ist oder falls es true ist und die node-to-replace != conclusion-node ist und die node-to-decompose
  ;;                                       in den nodes-to-decompose ist
  (when (or (null internal-node-check) (and (not (eq node-to-replace (r2ntop~decompose-unit-conclusion-node unit)))
					    (find node-to-replace (r2ntop~decompose-unit-nodes-to-decompose unit))))
    (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	   (nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit))
	   (nodes-to-add (mapcar #'first pairs-of-nodes-to-add-and-del-pos-add-pos)))
      (mapcar #'(lambda (list-of-new-node-and-pairs-of-add-and-del-pos)
		  (let* ((new-node (first list-of-new-node-and-pairs-of-add-and-del-pos))
			 (pairs-of-add-and-del-pos (second list-of-new-node-and-pairs-of-add-and-del-pos)))
		    (mapcar #'(lambda (del-pos-prefix add-pos-prefix)
				(decomp=replace-old-pdsnode-in-extdelta! node-to-replace new-node del-pos-prefix
									 delta-relation :new-prefix add-pos-prefix
									 :clauses clauses))
			    (mapcar #'first pairs-of-add-and-del-pos)
			    (mapcar #'second pairs-of-add-and-del-pos))))
	      pairs-of-nodes-to-add-and-del-pos-add-pos)
      (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
	    (append (remove-if-not #'(lambda (node-to-add)
				       (and (r2ntop~node-present-p node-to-add delta-relation)
					    (not (r2ntop~node-trivial-p node-to-add))))
				   (decomp=check-nodes-to-decompose nodes-to-add
								    delta-relation
								    (r2ntop~decompose-unit-conclusion-node unit)))
		    ;; |^ hier wird die conclusion-node gebraucht ! Muss vorher upgedated sein !!!
		    (if remove-replace-node
			(remove node-to-replace nodes-to-decompose)
		      nodes-to-decompose)))
      (when r2ntop*integral-formulas
	(integ~update-unit! unit
			    nodes-to-remove-from-integrals
			    nodes-to-add-to-integrals))))
  unit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Die folgednen beiden Funktionen kamen erst im Zuge der Lemmatisierungseinfuehrung hinzu!!

(defun decomp=all-clauses-for-empty-subst-term (pdsnode delta-relation)
  (declare (edited  "07-MAY-1998")
	   (authors Ameier)
	   (input   "A pdsnode with a universal or existential formula, a delta-relation.")
	   (effect  "None.")
	   (value   "A list of all clauses according (by the delta-relation) to the"
		    "the bound variable of pdsnode with an empty ground-instantiataion term"
		    "(for example (forall x C))."))
  (let* (;;(all-subst-terms (r2ntop~remove-list r2ntop*skolem-constants        ;; was soll das ?
	 ;;				      (decomp~all-subst-terms-of-forall-var pdsnode delta-relation)))
	 (all-subst-terms (remove-if #'(lambda (term)
					 (data~positions term #'sksym~p))
				     (decomp~all-subst-terms-of-quantified-var pdsnode delta-relation
									       :kind 'gamma)))
	 (all-clauses-for-node (remove-duplicates
				(mapcar #'extdelta~clause
					(remove-if-not #'(lambda (x)
							   (eq pdsnode (extdelta~pdsnode x)))
						       (extdelta~relation-pairs delta-relation))))))
    (do* ((rest-subst-terms all-subst-terms (rest rest-subst-terms))
	  (rest-clauses all-clauses-for-node))
	((null rest-subst-terms) rest-clauses)
      (let* ((head-subst-term (first rest-subst-terms))
	     (clause-var-pair-list (decomp=all-clauses-for-subst-term pdsnode delta-relation head-subst-term))
	     (clauses-for-subst-term (mapcar #'first clause-var-pair-list)))
	
	(setq rest-clauses (r2ntop~remove-list clauses-for-subst-term rest-clauses))))))

(defun decomp=update-unit-by-empty-subst-term (unit forall-node non-quantified-node)
  (declare (edited  "07-MAY-1998")
	   (authors Ameier)
	   (input   "A decompose unit, a forall-node and the according non quantified node.")
	   (effect  "The unit is updated by replacing forall-node by non-quantified-node (delta-relation+"
		    "ref-graph) modulo all clauses, that accords to a no subst-term (f.e. (forall x C)).")
	   (value   "The updated unit."
		    "REMARK: In the single unit, internal-node check is set to 't."))
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (clauses-for-subst-empty-subst (decomp=all-clauses-for-empty-subst-term forall-node delta-relation)))
    
    (when clauses-for-subst-empty-subst
      ;; exitiieren ueberhaupt entsprechende Substitutiierte Klauseln ?
      ;; Beachte: bei decomp~update-unit hat :clauses nil eine andere Bedeutung !
      ;; update delta-relation + nodes-to-decompose + integral-formulas
      (decomp~update-unit unit forall-node
			  (list (list non-quantified-node (list (list (pos~list-position '(1 0))
								      (pos~list-position '())))))
			  nil (list non-quantified-node)
			  :clauses clauses-for-subst-empty-subst
			  :remove-replace-node nil
			  :internal-node-check nil)))
  ;; internal-node-check nil -> ES WIRD IMMER ERSETZT !!!!! AUch wenn node garnicht in den nodes-to-decompose ist !!!!
  unit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun decomp=planned-existential-with-one-or-less-instantiation-p (exists-node delta-relation)
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A planned exists pdsnode, a delta-relation.")
	   (effect  "None.")
	   (value   "T if the exists of plannednode corresponds to the clauses given by delta-relation,"
		    "instantiated at most once. Nil otherwise."))
  (let* ((all-subst-terms (decomp~all-subst-terms-of-quantified-var exists-node delta-relation :kind 'gamma))
	 (laenge (length (remove-duplicates all-subst-terms :test 'keim~equal))))
    (or (= laenge 1) (= laenge 0))))


(defun decomp=replace-skolem-function-with-constant! (new-constant old-term ref-graph)
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A constant and an existing skolem term, a refutation graph and the delta-relation.")
	   (effect  "Every occurence of the skolem term is replaced by the new constant:"
		    "Directly in the clauses, in the ground-subst of the clauses,"
		    "and in the complete ground-substitution of the ref-graph.")
	   (value   "Undefined."))
  (let ((clause-nodes (ref~clause-nodes ref-graph)))

    ;;(format t "~%ALL EXAMINED CLAUSES BEFORE CHANGING: ~A" clause-nodes)
    
    (mapcar #'(lambda (clause)
		
		;; (format t "~%EXAMINING CLAUSE: ~A" clause)

		;;(when (typep clause 'r2ntop+flip-clause)
		;;  (setq global*clause clause)
		;;  (error))
		
		(if (or (typep clause 'r2ntop+flip-clause)
			(typep clause 'r2ntop+reflex-clause)
			(typep clause 'r2ntop+paramod-clause))
		    (mapcar #'(lambda (literal)
				(data~replace-struct literal old-term new-constant :destructive 't))
			    (cl~literals clause))
		  (progn
		    (mapcar #'(lambda (literal)
				(data~replace-struct literal old-term new-constant :destructive 't))
			    (cl~literals clause))
		    (let ((ground-subst (r2ntop~trans-clause-ground-subst clause)))
		      (setf (r2ntop~trans-clause-ground-subst clause)
			    (subst~create (subst~domain ground-subst)
					  (mapcar #'(lambda (codomain-term)
						      (data~replace-struct codomain-term old-term new-constant :destructive 't))
						  (subst~codomain ground-subst)))
			    ))))
		
		;; (format t "~%Checked-clause: ~A" clause)
		)
	    clause-nodes)
    
    ;; (format t "~%ALL EXAMINED CLAUSES AFTER CHANGING: ~A" clause-nodes)

    ;;(format t "~%THE REF-GRAPPH: ~A" ref-graph)
    
    ))

(defun decomp=apply-instantiation (ref-graph clause-var-pair-list)
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A refutation graph and a list of clauses-variable pairs (lists).")
	   (effect  "Takes for every clause of the clause-variables the ground-substitution of"
		    "the according variable and applies it to the clause.")
	   (value   "Undefined."))
  (declare (ignore ref-graph))
  (mapcar #'(lambda (clause-var-pair)
	      (let* ((clause (first clause-var-pair))
		     (var (second clause-var-pair))
		     (ground-subst (r2ntop~trans-clause-ground-subst clause))
		     (var-subst (subst~create (list var) (list (subst~apply ground-subst var)))))
		(mapcar #'(lambda (literal)
			    (setf (lit~atom literal) (subst~apply var-subst (lit~atom literal))))
			(cl~literals clause))
		(setf (r2ntop~trans-clause-ground-subst clause)
		      (subst~remove-component var ground-subst))))
	  clause-var-pair-list))	       


(defun decomp~all-subst-terms-of-quantified-var (pdsnode delta-relation &key (kind 'gamma))
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A quantified-pdsnode and a delta-relation and as keyword kind"
		    "gamma or delta to sign wether the node is a gamma formula or a delta-formula.")
	   (effect  "None.")
	   (value   "A list of all ground-instantiations of the bound variable of the"
		    "quantified-pdsnode."))
  (let* ((pairs (extdelta~relation-pairs delta-relation))
	 (acc-pairs (remove-if-not #'(lambda (x)
				       (eq (extdelta~pdsnode x) pdsnode))
				   pairs))
	 (formula (node~formula pdsnode))
	 (clauses (remove-duplicates (mapcar #'extdelta~clause acc-pairs)))
	 (variable (logic~quantification-bound-variable formula)))
        
    (apply 'append (mapcar #'(lambda (x)

			       (let* ((term-in-pdsnode (r2ntop~term-at-position formula (extdelta~formula-position x)))
				      (var-positions-in-term (data~substruct-positions variable
										       term-in-pdsnode :test 'keim~equal))
				      (clause-with-skolem-with-var (if (equal kind 'gamma)
								       (first (remove-if-not #'decomp=first-var-under-skolem
											     (list (extdelta~clause x))))
								     nil)))
				 (cond (var-positions-in-term
					(let* ((clause (extdelta~clause x))
					       (ground-subst (r2ntop~trans-clause-ground-subst clause))
					       (literal (r2ntop~term-at-position (cl~literals clause)
										 (extdelta~clause-position x))))
					  
					  (list (subst~apply ground-subst
							     (r2ntop~term-at-position (lit~atom literal)
										      (first var-positions-in-term))))))
				       (clause-with-skolem-with-var
					(let* ((first-var (decomp=first-var-under-skolem clause-with-skolem-with-var))
					       (ground-subst (r2ntop~trans-clause-ground-subst clause-with-skolem-with-var)))

					  
					  (list (subst~apply ground-subst first-var))))
				       
				       (t

					nil))))
			   
			   acc-pairs))))

(defun decomp=first-var-under-skolem (clause)
  (declare (edited  "20-OCT-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "Seeks for skolem-functions in the clause and returns if existing the"
		    "first variable under this skolem-functions."))
  (let* ((skolem-function-positions (data~positions clause #'sksym~p))
	 (poses-with-functions (remove-if-not #'(lambda (pos)
						  (equal (pos~first (pos~last pos)) 0))
					      skolem-function-positions))
	 (appl-poses (mapcar #'pos~butlast poses-with-functions))
	 (appls (mapcar #'(lambda (appl-pos)
			    (r2ntop~term-at-position clause appl-pos))
			appl-poses))
	 (appl-with-var (first (remove-if-not #'(lambda (appl)
						  (remove-if-not #'term~variable-p
								 (data~appl-arguments appl)))
					      appls)))
	 (first-var (if appl-with-var
			(first (remove-if-not #'term~variable-p
					      (data~appl-arguments appl-with-var)))
		      nil)))

    first-var))



;; im gegensatz zu decomp~all-subst-terms-of-quantified-var braucht man hier kein Kind, da diese Funktion nur bei
;; gamma Formeln benutzt wird -> immer gamma
(defun decomp=all-clauses-for-subst-term (pdsnode delta-relation subst-term)
  (declare (edited  "08-AUG-1996")
	   (authors Ameier)
	   (input   "A pdsnode with a universal or existential formula, a delta-relation, and a term.")
	   (effect  "None.")
	   (value   "A list of all clauses according (by the delta-relation) to the"
		    "the bound variable of pdsnode of the ground-instantiation term."))
  (let* ((pairs (extdelta~relation-pairs delta-relation))
	 (acc-pairs (remove-if-not #'(lambda (x)
				       (eq (extdelta~pdsnode x) pdsnode))
				   pairs))
	 (formula (node~formula pdsnode))
	 (variable (logic~quantification-bound-variable formula)))
    (remove-duplicates
     (apply 'append
	    (mapcar #'(lambda (x)
			(let* ((term-in-pdsnode (r2ntop~term-at-position formula (extdelta~formula-position x)))
			       (var-positions-in-term (data~substruct-positions variable
									      term-in-pdsnode :test 'keim~equal))
			       (clause (extdelta~clause x)))
			  (cond (var-positions-in-term
				 (let* ((literal (r2ntop~term-at-position (cl~literals clause) (extdelta~clause-position x)))
					(ground-subst (r2ntop~trans-clause-ground-subst clause))
					(var (r2ntop~term-at-position (lit~atom literal) (first var-positions-in-term))))
				   (if (keim~equal (subst~apply ground-subst var) subst-term)
				       (list (list clause var))
				     nil)))
				((remove-if-not #'decomp=first-var-under-skolem
						(list clause))
				 (let* ((first-var (decomp=first-var-under-skolem clause))
					(ground-subst (r2ntop~trans-clause-ground-subst clause)))
				   (if (keim~equal (subst~apply ground-subst first-var) subst-term)
				       (list (list clause first-var))
				     nil)))
				(t
				 nil))))
		    acc-pairs))
     :test #'(lambda (item1 item2) (eq (first item1) (first item2))))))


(defun decomp=cut-position-prefix (position prefix)
  (declare (edited  "11-MAR-1996")
	   (authors Ameier)
	   (input   "A position1 and a position2 which is a prefix of position1.")
	   (effect  "None.")
	   (value   "The position1 with cut off prefix position2."))
  (let ((numbers (pos~number-list prefix)))
    (do* ((rest-numbers numbers (rest rest-numbers))
	  (rest-position position (pos~rest rest-position)))
	((null rest-numbers) rest-position))))

(defun decomp=add-position-prefix (position new-prefix)
  (declare (edited  "17-APR-1996")
	   (authors Ameier)
	   (input   "A position and a position, that should become the new"
		    "prefix of position.")
	   (effect  "None.")
	   (value   "A new position consisting of new-prefix and position."))
  (let ((reverse-numbers (reverse (pos~number-list new-prefix))))
    (do ((rest-numbers reverse-numbers (rest rest-numbers))
	 (new-position position (pos~add-front (first rest-numbers) new-position)))
	((null rest-numbers) new-position))))

(defun decomp=replace-old-pdsnode-in-extdelta! (old-pdsnode new-pdsnode pos-prefix delta-relation &key (new-prefix nil) (clauses nil))
  (declare (edited  "11-MAR-1996")
	   (authors Ameier)
	   (input   "An nd-node old-node, an nd-node new-node, a position pos-prefix,"
		    "a delta-relation and keys: new-prefix and clauses.")
	   (effect  "Every relation-pair whose nd-node is old-node and whose formula-position"
		    "has the prefix pos-prefix is replaced by a pair with nd-node new-node"
		    "and formula-position cut off prefix position and add new-prefix at front.")
	   (value   "Undefined."))
  (let* ((new-formula (node~formula new-pdsnode))
	 (new-named-term (r2ntop~new-named-term new-formula))
	 (delta-pairs (extdelta~relation-pairs delta-relation))
	 (delta-pairs-of-old-pdsnode-and-pos-prefix
	  (remove-if-not #'(lambda (x)
			     (and
			      (eq (extdelta~pdsnode x) old-pdsnode)
			      (pos~prefix-p pos-prefix (extdelta~formula-position x))
			      (if clauses
				  (member (extdelta~clause x) clauses)
				't)))
			 delta-pairs))
	 (other-pairs (remove-if #'(lambda (x)
				     (and
				      (eq (extdelta~pdsnode x) old-pdsnode)
				      (pos~prefix-p pos-prefix (extdelta~formula-position x))
				      (if clauses
					  (member (extdelta~clause x) clauses)
					't)))
				 delta-pairs))
	 (new-delta-pairs (mapcar #'(lambda (x)
				      (extdelta~create-delta-pair
				       new-named-term
				       (if new-prefix
					   (decomp=add-position-prefix 
					    (decomp=cut-position-prefix (extdelta~formula-position x) pos-prefix)
					    new-prefix)
					 (decomp=cut-position-prefix (extdelta~formula-position x) pos-prefix))
				       new-pdsnode
				       (extdelta~clause x)
				       (extdelta~clause-position x)))
				  delta-pairs-of-old-pdsnode-and-pos-prefix)))
    (setf (delta~relation-pairs delta-relation) (append new-delta-pairs other-pairs))))

#| -------------------------------------------------------- DECOMPOSITIONS --------------------------------------------------------- |#

;; -------------------> DISJUNCTION-E

(defun decomp=choose-rule-for-e-or (or-node unit workon-units update-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing rule for e-or"))

  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (pairs-of-or-node (remove-if-not #'(lambda (pair)
					      (eq (extdelta~pdsnode pair) or-node))
					  (extdelta~relation-pairs delta-relation)))
	 (first-positions-of-or-node-pairs (mapcar #'pos~first (mapcar #'extdelta~formula-position pairs-of-or-node))))
    (cond ((and (find '1 first-positions-of-or-node-pairs)
		(find '2 first-positions-of-or-node-pairs))
	   (decomp=decomposition-rule-e-or-1 or-node unit workon-units update-units))
	  ((find '1 first-positions-of-or-node-pairs)
	   (decomp=decomposition-rule-e-or-left or-node unit workon-units update-units))
	  (t
	   (decomp=decomposition-rule-e-or-rigth or-node unit workon-units update-units)))))

(defun decomp=decomposition-rule-e-or-left (or-node unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-or-left."))
  
  (let* ((new-nodes (r2ntop~apply-tactic 'oril (list or-node nil) (list (second (data~appl-arguments (node~formula or-node))))))
	 (left-part (second new-nodes)))

    ;; update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node unit) left-part) 

    ;; update delta-relation + decomposition-nodes + integral-formulas
    (decomp~update-unit unit or-node 
			(list (list left-part (list (list (pos~list-position '(1))
							  (pos~list-position '())))))
			nil nil)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" unit))
    
    (cons unit workon-units)))

(defun decomp=decomposition-rule-e-or-rigth (or-node unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-or-right."))

  (let* ((new-nodes (r2ntop~apply-tactic 'orir (list or-node nil) (list (first (data~appl-arguments (node~formula or-node))))))
	 (rigth-part (second new-nodes)))

    ;; update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node unit) rigth-part)
    
    ;; update delta-relation + decomposition-nodes
    (decomp~update-unit unit or-node 
			(list (list rigth-part (list (list (pos~list-position '(2))
							   (pos~list-position '())))))
			nil nil)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" unit))
    
    (cons unit workon-units)))

(defun decomp=decomposition-rule-e-or-1 (disjunction-node unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-or-1."))

  (let* ((new-nodes-implies-as-or-backward (r2ntop~apply-tactic 'imp2or (list disjunction-node nil) nil))
	 (implication-node (second new-nodes-implies-as-or-backward))
	 (new-nodes-impi-backward (r2ntop~apply-tactic 'impi (list implication-node nil) nil))
	 (negated-lhs-hyp-node (third new-nodes-impi-backward))
	 (rhs-node (second new-nodes-impi-backward)))
    
    ;; update of conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node unit) rhs-node)
    
    (if (logic~negation-p (first (data~appl-arguments (node~formula disjunction-node))))
	;; abhaengig davon, wie sich das imp2or verhaelt auf termen (or (not ...) ...) bzw. (or ... ...)
	
	;; update of delta-relation + decomposition-nodes + integral-formulas
	(decomp~update-unit unit disjunction-node
			    (list (list negated-lhs-hyp-node (list (list (pos~list-position '(1 1))
									 (pos~list-position '()))))
				  (list rhs-node (list (list (pos~list-position '(2))
							     (pos~list-position '())))))
			    nil (list negated-lhs-hyp-node))
      
      ;; update of delta-relation + decomposition-nodes + integral-formulas
      (decomp~update-unit unit disjunction-node
			  (list (list negated-lhs-hyp-node (list (list (pos~list-position '(1))
								       (pos~list-position '(1)))))
				(list rhs-node (list (list (pos~list-position '(2))
							   (pos~list-position '())))))
			  nil (list negated-lhs-hyp-node)))
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" unit))
    
    (cons unit workon-units)))

;; ------------------> EXISTENTIAL-E

(defun decomp=choose-rule-for-e-exists (existential-node current-unit workon-units update-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing rule for e-exists"))

  (let* ((delta (r2ntop~decompose-unit-extdelta-relation current-unit)))
    (if (decomp=planned-existential-with-one-or-less-instantiation-p existential-node delta)
	(decomp=decomposition-rule-e-exists-direct existential-node current-unit workon-units update-units)
      (decomp=decomposition-rule-e-exists existential-node current-unit workon-units update-units))))


(defun decomp=decomposition-rule-e-exists-direct (existential-node current-unit workon-units update-units)
  (declare (ignore update-units))

   (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-exists-direct."))
   
  (let* ((abstr-ex-formula (first (data~appl-arguments (node~formula existential-node))))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (ref-graph (r2ntop~decompose-unit-ref-graph current-unit))

	 (all-subst-terms (remove-if #'(lambda (term)
					 (data~positions term #'sksym~p))
				     (decomp~all-subst-terms-of-quantified-var existential-node delta-relation
									       :kind 'gamma)))
	 ;; (r2ntop~remove-list r2ntop*skolem-constants
	 ;;     (decomp~all-subst-terms-of-quantified-var existential-node delta-relation
	 ;; :kind 'gamma)))
	 (subst-term (if all-subst-terms
			 (first all-subst-terms)
		       (r2ntop~new-const (first (data~abstr-domain
						 (first (data~appl-arguments (node~formula existential-node))))))))
	                                         ;; falls exists x. C oder so ?
	 (clause-var-pair-list (decomp=all-clauses-for-subst-term existential-node delta-relation subst-term))
	 ;; (clauses-for-subst (mapcar #'first clause-var-pair-list))
	 (pos-list (data~substruct-positions (data~abstr-bound-var abstr-ex-formula)
					     (data~abstr-range abstr-ex-formula)))
	 (new-nodes (r2ntop~apply-tactic 'existsi (list existential-node nil) (list subst-term pos-list)))
	 (non-quantified-node (second new-nodes)))
    
    ;; update the clauses by applying instantiation on it
    (decomp=apply-instantiation ref-graph clause-var-pair-list)

    ;;update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node current-unit) non-quantified-node)

    ;; update delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit current-unit existential-node 
			(list (list non-quantified-node (list (list (pos~list-position '(1 0))
								    (pos~list-position '())))))
			nil nil) ;;  :clauses clauses-for-subst) sollte nicht noetig sein, da sowieso hoechstens eine Instantiierung !
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))
    
    (cons current-unit workon-units)))


(defun decomp=decomposition-rule-e-exists (existential-node current-unit workon-units update-units)
  (declare (ignore update-units))

   (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-exists."))

  (let* ((new-nodes-indirect (r2ntop~apply-tactic 'indirect (list existential-node nil) nil))
	 (neg-ex-node (third new-nodes-indirect))
	 (new-thm-node (second new-nodes-indirect))
	 (new-nodes-pushneg (r2ntop~apply-tactic 'pushneg (list nil neg-ex-node) nil))
	 (forall-node (first new-nodes-pushneg)))

    ;; update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node current-unit) new-thm-node)

    ;; update of delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit current-unit existential-node
			(list (list forall-node (list (list (pos~list-position '(1 0))
							    (pos~list-position '(1 0 1))))))
			nil (list forall-node))
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))

    (cons current-unit workon-units)))

;; ------------------> UNIVERSAL-E

(defun decomp=decomposition-rule-e-forall (pdsnode current-unit workon-units update-units)
  (declare (ignore update-units))

   (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-forall."))
  
  (let* ((ref-graph (r2ntop~decompose-unit-ref-graph current-unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (all-subst-terms (decomp~all-subst-terms-of-quantified-var pdsnode delta-relation :kind 'delta))
	 (new-constant (if all-subst-terms
			   (r2ntop~new-const (first all-subst-terms))   ;; substterms exists -> new constant
			 (r2ntop~new-const (first (data~abstr-domain       ;; no substterms ,for example : (forall (x i) C)
						   (first (data~appl-arguments (node~formula pdsnode))))))))
	 (skolem-term (if all-subst-terms
			  (data~copy (first all-subst-terms))
			nil))
	 (new-nodes (r2ntop~apply-tactic 'foralli (list pdsnode nil) (list new-constant)))
	 (non-quantified-node (second new-nodes)))
    
    ;; update the refutation-graph -> only if subst-terms exists, otherwise there is nothing to change in ref-graph
    (when all-subst-terms
      (decomp=replace-skolem-function-with-constant! new-constant
						     skolem-term
						     ref-graph))

    ;; update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node current-unit) non-quantified-node)

    ;; update the-delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit current-unit pdsnode
			(list (list non-quantified-node (list (list (pos~list-position '(1 0))
								    (pos~list-position '())))))
			nil nil)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))

    (cons current-unit workon-units)))

;; -----------------> CONJUNCTION-E

(defun decomp=decomposition-rule-e-and (conjunction-node current-unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-and."))

  (let* ((pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes current-unit))
	 (new-nodes (r2ntop~apply-tactic 'andi (list conjunction-node nil nil) nil))
	 (lhs-node (second new-nodes))
	 (rhs-node (third new-nodes)))
    (multiple-value-bind
	(conc-ref-delta-list1 conc-ref-delta-list2)
	(split~splitting-ref-graph! (r2ntop~decompose-unit-ref-graph current-unit)
				    (r2ntop~decompose-unit-extdelta-relation current-unit)
				    conjunction-node
				    lhs-node rhs-node
				    lhs-node rhs-node)
      
      ;; update delta-relation + nodes-to-decompose + integral-formulas + conclusion-node done
      ;; by new-decompose-unit-from-conclusion-node
      (cons (decomp~new-decompose-unit (first conc-ref-delta-list1)
				       (second conc-ref-delta-list1)
				       (third conc-ref-delta-list1)
				       :pre-selected-nodes (rest pre-selected-nodes))   ;; falls dieser Knoten in pre-selected -> raus
	    (cons (decomp~new-decompose-unit  (first conc-ref-delta-list2)              ;; falls nicht -> preselcted=nil -> rest (nil)
					      (second conc-ref-delta-list2)
					      (third conc-ref-delta-list2)
					      :pre-selected-nodes (rest pre-selected-nodes))
		  workon-units)))))

;; ----------------> IMPLICATION-E

(defun decomp=choose-rule-for-e-implies (implies-node unit workon-units update-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing rule for e-implies"))

  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (pairs-of-implies-node (remove-if-not #'(lambda (pair)
						   (eq (extdelta~pdsnode pair) implies-node))
					       (extdelta~relation-pairs delta-relation)))
	 (first-positions-of-implies-node-pairs (mapcar #'pos~first (mapcar #'extdelta~formula-position pairs-of-implies-node))))
    (cond ((and (find '1 first-positions-of-implies-node-pairs) (find '2 first-positions-of-implies-node-pairs))
	   (decomp=decomposition-rule-e-implies implies-node unit workon-units update-units))
	  (t
	   (decomp=decomposition-rule-e-implies-to-or implies-node unit workon-units update-units)))))

(defun decomp=decomposition-rule-e-implies-to-or (implies-node unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-implies-to-or."))
  
  (let* ((new-nodes (r2ntop~apply-tactic 'or2imp (list implies-node nil) nil))
	 (or-node (second new-nodes)))

    ;; update of the conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node unit) or-node) 
    
    ;; update delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit unit implies-node
			(list (list or-node (list (list (pos~list-position '(1))
							(pos~list-position '(1 1)))
						  (list (pos~list-position '(2))
							(pos~list-position '(2))))))
			nil nil)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" unit))

    (cons unit workon-units)))

(defun decomp=decomposition-rule-e-implies (implication-node current-unit workon-units update-units)
  (declare (ignore update-units))
  
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-implies."))
  
  (let* ((new-nodes (r2ntop~apply-tactic 'impi (list implication-node nil) nil))
	 (rhs-node (second new-nodes))
	 (lhs-node (third new-nodes)))

    ;; update the conclusion node
    (setf (r2ntop~decompose-unit-conclusion-node current-unit) rhs-node) 

    ;; updating delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit current-unit implication-node
			(list (list lhs-node (list (list (pos~list-position '(1))
							 (pos~list-position '()))))
			      (list rhs-node (list (list (pos~list-position '(2))
							 (pos~list-position '())))))
			nil (list lhs-node))

    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))
    
    (cons current-unit workon-units)))

;; ---------------> Equiv-E

(defun decomp=decomposition-rule-e-equiv (equiv-node current-unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-equiv."))
  
  (let* ((pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes current-unit))
	 (new-nodes (r2ntop~apply-tactic 'equivi (list equiv-node nil nil) nil))
	 (left-node (second new-nodes))      ;; A -> B bei A <-> B
	 (right-node (third new-nodes)))     ;; B -> A bei A <-> B

    (multiple-value-bind
	(conc-ref-delta-list1 conc-ref-delta-list2)
	(split~splitting-ref-graph! (r2ntop~decompose-unit-ref-graph current-unit)
				    (r2ntop~decompose-unit-extdelta-relation current-unit)
				    equiv-node
				    left-node right-node
				    left-node right-node)
    
    ;; updating delta-relation + nodes-to-decompose + integral-formulas + conclusion-node
      (cons (decomp~new-decompose-unit (first conc-ref-delta-list1)
				       (second conc-ref-delta-list1)
				       (third conc-ref-delta-list1)
				       :pre-selected-nodes (rest pre-selected-nodes))
	    (cons (decomp~new-decompose-unit  (first conc-ref-delta-list2)
					      (second conc-ref-delta-list2)
					      (third conc-ref-delta-list2)
					      :pre-selected-nodes (rest pre-selected-nodes))
		  workon-units)))))

;; ---------------> NEGATION-E

(defun decomp=decomposition-rule-e-pushneg (negation-node current-unit workon-units update-units)
  (declare (ignore update-units))
  
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-pushneg."))
  
  (let* ((negated-term (car (data~appl-arguments (node~formula negation-node)))))
    (if (logic~atom-p negated-term)

	(progn
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: No further decomposition possible on node ~A"
			   negation-node))
	  
	  ;; atom -> pushneg isn't possible -> no new nodes
	  (append (decomp=remove-node-from-unit-decomposition-nodes! negation-node (list current-unit) nil)
		  workon-units))
      
      (let* ((new-nodes (r2ntop~apply-tactic 'pullneg (list negation-node nil) nil))
	     (pushed-node (second new-nodes)))

	(when r2ntop*trans-debug
	  (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule e-implies."))

	;; updating the conclusion-node
	(setf (r2ntop~decompose-unit-conclusion-node current-unit) pushed-node)

	;; updating delta-relation + nodes-to-decompose + integral-formulas
	(cond ((or (logic~universal-quantification-p negated-term)
		   (logic~existential-quantification-p negated-term))
	       (decomp~update-unit current-unit negation-node
				   (list (list pushed-node (list (list (pos~list-position '(1 1 0))
								       (pos~list-position '(1 0 1))))))
				   nil nil))
	      ((logic~implication-p negated-term)
	       (decomp~update-unit current-unit negation-node
				   (list (list pushed-node (list (list (pos~list-position '(1 1))
								       (pos~list-position '(1)))
								 (list (pos~list-position '(1 2))
								       (pos~list-position '(2 1))))))
				   nil nil))
	      ((or (logic~conjunction-p negated-term)
		   (logic~disjunction-p negated-term))
	       (decomp~update-unit current-unit negation-node
				   (list (list pushed-node (list (list (pos~list-position '(1 1))
								       (pos~list-position '(1 1)))
								 (list (pos~list-position '(1 2))
								       (pos~list-position '(2 1))))))
				   nil nil))
	      ((logic~negation-p negated-term)
	       (decomp~update-unit current-unit negation-node
				   (list (list pushed-node (list (list (pos~list-position '(1 1))
								       (pos~list-position '())))))
				   nil nil))
	      ((logic~equivalence-p negated-term)
	       (decomp~update-unit current-unit negation-node
				   (list (list pushed-node (list (list (pos~list-position '(1 1))
								       (pos~list-position '(1 1)))
								 (list (pos~list-position '(1 2))
								       (pos~list-position '(2 1))))))
				   nil nil)))
	
	(when r2ntop*trans-debug
	  (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))

	(cons current-unit workon-units)))))

      

#| ------------------------------------------------ END E-DECOMP RULES ------------------------------------------------ |#


#|
GROEEEEEEEEESSERE KNZEPTIONELLE UMSTELLUNG:

Wird durch die Anwendung einer internal Rule eine Zeile L1 ersetzt durch Zeilen (L2 ... Ln) in einer decompose Unit, so wird
diese Zeile L1 auch in jeder anderen decompose unit so ersetzt, sofern L1 in dieser Unit nicht die conclusion-node ist.
Etwas komplizierter ist es nur bei quantifizierten Formeln.

|#

#| ----------------------------------------------- I-Decomposition RULES ---------------------------------------------- |#


;; -----------------> NEGATION-I
(defun decomp=decomposition-rule-pushneg-i (negation-node current-unit workon-units update-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule pushneg-i."))
  
  (let* ((negated-term (car (data~appl-arguments (node~formula negation-node))))
	 (new-workon-units (cons current-unit workon-units)))
    (if (logic~atom-p negated-term)

	(progn
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: No further decomposition possible on node ~A"
			   negation-node))
	  ;; atom -> pushneg isn't possible -> no new nodes
	  (decomp=remove-node-from-unit-decomposition-nodes! negation-node new-workon-units update-units))
      
      (let* ((new-nodes (r2ntop~apply-tactic 'pushneg (list nil negation-node) nil))
	     (pushed-node (first new-nodes)))
	
	;; updating the delta-relation + decompose-nodes + integral-formulas
	(cond ((or (logic~universal-quantification-p negated-term)
		   (logic~existential-quantification-p negated-term))
	       (decomp=update-units (append (list current-unit) workon-units update-units)
				    negation-node 
				    (list (list pushed-node (list (list (pos~list-position '(1 1 0))
									(pos~list-position '(1 0 1))))))
				    (list negation-node) (list pushed-node)))
	      ((logic~implication-p negated-term)
	       (decomp=update-units (append (list current-unit) workon-units update-units)
				    negation-node
				    (list (list pushed-node (list (list (pos~list-position '(1 1))
									(pos~list-position '(1)))
								  (list (pos~list-position '(1 2))
									(pos~list-position '(2 1))))))
				    (list negation-node) (list pushed-node)))
	      ((or (logic~conjunction-p negated-term)
		   (logic~disjunction-p negated-term))
	       (decomp=update-units (append (list current-unit) workon-units update-units)
				    negation-node
				    (list (list pushed-node (list (list (pos~list-position '(1 1))
									(pos~list-position '(1 1)))
								  (list (pos~list-position '(1 2))
									(pos~list-position '(2 1))))))
				    (list negation-node) (list pushed-node)))
	      ((logic~equivalence-p negated-term)
	       (decomp=update-units (append (list current-unit) workon-units update-units)
				    negation-node
				    (list (list pushed-node (list (list (pos~list-position '(1 1))
									(pos~list-position '(1 1)))
								  (list (pos~list-position '(1 2))
									(pos~list-position '(2 1))))))
				    nil nil))
	      ((logic~negation-p negated-term)
	       (decomp=update-units (append (list current-unit) workon-units update-units)
				    negation-node
				    (list (list pushed-node (list (list (pos~list-position '(1 1))
									(pos~list-position '())))))
				    (list negation-node) (list pushed-node))))

	(when r2ntop*trans-debug
	  (omega~message "~%~%r2ntop*trans-debug: back-units")
	  (mapcar #'(lambda (unit)
		      (omega~message "~%~A" unit))
		  (append (list current-unit) workon-units update-units)))
		      
	(cons current-unit workon-units)))))


;; ----------------> CONJUNCTION-I

(defun decomp=decomposition-rule-and-i (conjunction-node current-unit workon-units update-units)
 
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule and-i."))

  (let* ((new-nodes (r2ntop~apply-tactic 'ande (list nil nil conjunction-node) nil))
	 (lhs-node (first new-nodes))
	 (rhs-node (second new-nodes)))
    
    ;; update delta-relation + nodes-to-decompose + integral-formulas
    (decomp=update-units (append (list current-unit) workon-units update-units)
			 conjunction-node
			 (list (list lhs-node (list (list (pos~list-position '(1))
							  (pos~list-position '()))))
			       (list rhs-node (list (list (pos~list-position '(2))
							  (pos~list-position '())))))
			 (list conjunction-node) (list rhs-node lhs-node))

    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-units")
      (mapcar #'(lambda (unit)
		  (omega~message "~%~A" unit))
	      (append (list current-unit) workon-units update-units)))
    
    (cons current-unit workon-units)))

;; ----------------> EQUIV-I

(defun decomp=decomposition-rule-equiv-i (equiv-node current-unit workon-units update-units)
 
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule equiv-i."))

  (let* ((new-nodes (r2ntop~apply-tactic 'equive (list nil nil equiv-node) nil))
	 (left-node (first new-nodes))         ;; A -> B if A <-> B
	 (right-node (second new-nodes)))        ;; B -> A if A <-> B
    
    
    ;; update delta-relation + nodes-to-decompose + integral-formulas
    (decomp=update-units (append (list current-unit) workon-units update-units)
			 equiv-node
			 (list (list left-node (list (list (pos~list-position '(1))
							   (pos~list-position '()))))
			       (list right-node (list (list (pos~list-position '(2))
							    (pos~list-position '())))))
			 (list equiv-node) (list left-node right-node))

    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-units")
      (mapcar #'(lambda (unit)
		  (omega~message "~%~A" unit))
	      (append (list current-unit) workon-units update-units)))
    
    (cons current-unit workon-units)))


;; ----------------> UNIVERSAL-I

#|
;; Ersetzte Version!
(defun decomp=decomposition-rule-forall-i (forall-node current-unit workon-units update-units)
 
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule forall-i."))
  
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (all-subst-terms (remove-if #'(lambda (term)
					 (data~positions term #'sksym~p))
				     (decomp~all-subst-terms-of-quantified-var forall-node delta-relation
									       :kind 'gamma)))
	 ;; (r2ntop~remove-list r2ntop*skolem-constants        ;; was soll das ?
	 ;;				      (decomp~all-subst-terms-of-quantified-var forall-node delta-relation
	 ;;										:kind 'gamma)))
	 (all-clauses-for-node (remove-duplicates
				(mapcar #'extdelta~clause
					(remove-if-not #'(lambda (x)
							   (eq forall-node (extdelta~pdsnode x)))
						       (extdelta~relation-pairs delta-relation)))))
	 (subst-term (first all-subst-terms)))

    (if subst-term
	(let* ((new-nodes (r2ntop~apply-tactic 'foralle (list nil forall-node) (list subst-term)))
	       (non-quantified-node (first new-nodes)))

	  
	  
	  ;; update delta-relation + nodes-to-decompose + integral-formulas
	  (mapcar #'(lambda (unit)
		      (decomp=update-unit-by-subst-term unit forall-node subst-term non-quantified-node))
		  (append (list current-unit) workon-units update-units))
	  
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: back-units")
	    (mapcar #'(lambda (unit)
			(omega~message "~%~A" unit))
		    (append (list current-unit) workon-units update-units)))
	  
	  (cons current-unit workon-units)) 
      ;; last found instantiation -> 
      (if (= (length all-clauses-for-node) 0)
	  ;; all clauses are abgearbeitet -> ;; delete Forall-node: NOT EARLIER
	  (progn

	    (when r2ntop*trans-debug
	      (omega~message "~%~%r2ntop*trans-debug: Last found instantiataion of forall term, deleteing it from the nodes-to-decompose."))
	    
	    (setf (r2ntop~decompose-unit-nodes-to-decompose current-unit)    
		  (remove forall-node (r2ntop~decompose-unit-nodes-to-decompose current-unit)))
	    (cons current-unit workon-units))
	
	;; clauses without subst-terms -> (f.e. (forall x (s a)))
	;; -> match all such clauses with one arbitrary instantiation of the node 
	(let* ((subst-term (r2ntop~new-const (first (data~abstr-domain
						     (first (data~appl-arguments (node~formula forall-node)))))))
	       (clauses-for-subst all-clauses-for-node)
	       (new-nodes (r2ntop~apply-tactic 'foralle (list nil forall-node) (list subst-term)))
	       (non-quantified-node (first new-nodes)))
	  
	  ;; update delta-relation + nodes-to-decompose + integral-formulas
	  (decomp~update-unit current-unit forall-node
			      (list (list non-quantified-node (list (list (pos~list-position '(1 0))
									  (pos~list-position '())))))
			      (list forall-node) (list non-quantified-node)
			      :clauses clauses-for-subst)

	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))
	  
	  (cons current-unit workon-units))))))
|#

;; Neuer Version, im ZUge der Lemma-Einfuehrung hinzugefuegt!
;; Neuerung: gibt es eine forall-line mit leere Susbtitution, so wird diese forall-line uber ALLEN Units nur einmal gemeinsam
;;           Instantiiert

(defun decomp=decomposition-rule-forall-i (forall-node current-unit workon-units update-units)
 
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule forall-i."))
  
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (all-subst-terms (remove-if #'(lambda (term)
					 (data~positions term #'sksym~p))
				     (decomp~all-subst-terms-of-quantified-var forall-node delta-relation
									       :kind 'gamma)))
	 (all-clauses-for-node (remove-duplicates
				(mapcar #'extdelta~clause
					(remove-if-not #'(lambda (x)
							   (eq forall-node (extdelta~pdsnode x)))
						       (extdelta~relation-pairs delta-relation)))))
	 (subst-term (first all-subst-terms)))
    
    (if subst-term
	(let* ((new-nodes (r2ntop~apply-tactic 'foralle (list nil forall-node) (list subst-term)))
	       (non-quantified-node (first new-nodes)))
	  
	  ;; update delta-relation + nodes-to-decompose + integral-formulas
	  (mapcar #'(lambda (unit)
		      (decomp=update-unit-by-subst-term unit forall-node subst-term non-quantified-node))
		  (append (list current-unit) workon-units update-units))
	  
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: back-units")
	    (mapcar #'(lambda (unit)
			(omega~message "~%~A" unit))
		    (append (list current-unit) workon-units update-units)))

	  ;;(decf glo*counter)
	  ;;(when (= glo*counter 0)
	  ;;  (setq glo*back (append (list current-unit) workon-units update-units))
	  ;;  (error "sdfsd"))
	  
	  (cons current-unit workon-units)) 
      ;; last found instantiation -> 
      (if (= (length all-clauses-for-node) 0)
	  ;; all clauses are abgearbeitet -> ;; delete Forall-node: NOT EARLIER
	  (progn

	    (when r2ntop*trans-debug
	      (omega~message "~%~%r2ntop*trans-debug: Last found instantiataion of forall term, deleteing it from the nodes-to-decompose."))
	    
	    (setf (r2ntop~decompose-unit-nodes-to-decompose current-unit)    
		  (remove forall-node (r2ntop~decompose-unit-nodes-to-decompose current-unit)))
	    (cons current-unit workon-units))
	
	;; clauses without subst-terms -> (f.e. (forall x (s a)))
	;; -> match all such clauses with one arbitrary instantiation of the node 
	(let* ((subst-term (r2ntop~new-const (first (data~abstr-domain
						     (first (data~appl-arguments (node~formula forall-node)))))))
	       (clauses-for-subst all-clauses-for-node)
	       (new-nodes (r2ntop~apply-tactic 'foralle (list nil forall-node) (list subst-term)))
	       (non-quantified-node (first new-nodes)))

	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: Found a foralle without subst-terms: using arbitrary and deleting then the node from the nodes-to-decompose."))	  
	  
	  ;; update delta-relations + nodes-to-decomposes + integral-formulas
	  (mapcar #'(lambda (unit)
		      (decomp=update-unit-by-empty-subst-term unit forall-node non-quantified-node))
		  (append (list current-unit) workon-units update-units))
	  
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))
	  
	  (cons current-unit workon-units))))))


;; ------------------------> DISJUNCTION-I

(defun decomp=choose-rule-for-or-i (disjunction-node current-unit workon-units update-units)
  (declare (ignore update-units))
  
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing rule for or-i"))

  (let* ((conclusion-node (r2ntop~decompose-unit-conclusion-node current-unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (lhs-dummy-node (pdsn~create nil nil (term~constant-create 'lhs-dummy type*o) nil))
	 (rhs-dummy-node (pdsn~create nil nil (term~constant-create 'rhs-dummy type*o) nil))
	 (dummy-planned-node1 (pdsn~open-node-create (term~constant-create 'planned-dummy1 type*o) nil nil nil))
	 (dummy-planned-node2 (pdsn~open-node-create (term~constant-create 'planned-dummy2 type*o) nil nil nil)))
    (multiple-value-bind
	(conc-ref-delta-list1 conc-ref-delta-list2)
	(split~splitting-ref-graph! (r2ntop~decompose-unit-ref-graph current-unit)
				    delta-relation
				    disjunction-node
				    lhs-dummy-node rhs-dummy-node
				    dummy-planned-node1 dummy-planned-node2)
      (decomp=decomposition-or-i disjunction-node
				 conclusion-node
				 current-unit
				 workon-units
				 lhs-dummy-node rhs-dummy-node
				 (first conc-ref-delta-list1) (first conc-ref-delta-list2)      ;; dummy-planned-nodes
				 (second conc-ref-delta-list1) (second conc-ref-delta-list2)    ;; ref-graphs
				 (third conc-ref-delta-list1) (third conc-ref-delta-list2)))))  ;; delta-relations


(defun decomp=decomposition-or-i (disjunction-node thm-node unit workon-units
						   lhs-dummy-node rhs-dummy-node
						   dummy-planned-node1 dummy-planned-node2
						   ref-graph1 ref-graph2
						   delta-relation1 delta-relation2
						   )
  (let ((pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes unit)))
    (cond ((data~equal (first (data~appl-arguments (node~formula disjunction-node)))
		       (second (data~appl-arguments (node~formula disjunction-node))))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: or-i: MERGE!! A OR A -> A"))
	   
	   ;; (OR A A) -> MERGE -> A
	   (let* ((new-nodes (r2ntop~apply-tactic 'idemor (list nil disjunction-node) nil))
		  (merge-node (first new-nodes)))
	     (cond ((decomp=delta-relation-contains-planned-node-p delta-relation2 thm-node)
		    ;; rechter Teil enthaelt anteile an der THM-Node -> waehle ihn um indirekten Beweis zu vermeiden

		    ;; update delta-relations
		    (decomp=replace-old-pdsnode-in-extdelta!
		     rhs-dummy-node
		     merge-node
		     (pos~list-position '())
		     delta-relation2)
		    
		    (cons (decomp~new-decompose-unit thm-node ref-graph2 delta-relation2
						     :pre-selected-nodes (rest pre-selected-nodes))
			  workon-units))
		   (t

		    ;; update delta-relations
		    (decomp=replace-old-pdsnode-in-extdelta!
		     lhs-dummy-node
		     merge-node
		     (pos~list-position '())
		     delta-relation1)
		    (cons (decomp~new-decompose-unit  thm-node ref-graph1 delta-relation1
						      :pre-selected-nodes (rest pre-selected-nodes))
			  workon-units)))))
	  ((not (decomp=delta-relation-contains-planned-node-p delta-relation1 thm-node))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: or-i: Left part contains no part of the conclusion-node: ORMP"))
	   
	   (let* ((new-planned-node1 (pdsn~open-node-create 
				      (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
							(list (first (data~appl-arguments (node~formula disjunction-node)))))
				      (pdsn~hyps thm-node)
				      (pds~new-node-name)
				      ))
		  (kill-shit (pds~only-insert-node! new-planned-node1 omega*current-proof-plan))
		  (new-nodes (r2ntop~apply-tactic 'ormp (list nil disjunction-node new-planned-node1) nil))
		  (or-elim-node (first new-nodes))
		  (new-planned-node2 thm-node)
		  (trivial-hyp-disj (keim~get disjunction-node 'trivial-hyp)))
	     (declare (ignore kill-shit))

	     (format t "~%~%TRIVIAL-HYP-DISJ is: ~A" trivial-hyp-disj)
	     
	     ;; It is possible that the disjunction node is a later inserted lemma-node of the kind: A v -A (TND).
	     ;; (in the plist trivial-hyp is 't)
	     ;; This node (and the according clause) is only inserted to do a better decomposition and splitting. Now it can be happens
	     ;; that you can realize after the splitting, that the node really isn't necessary, because the splitting and the
	     ;; decomposition is done in a way that we would get the following lines:
	     ;;             A v -A (TND)       (disjunction node)
	     ;;             (- A)  (blabla)    (new-planned-node1)
	     ;;             (- A)  (ormp)      (or-elim-node)
	     ;;              CONC  (blabla)    (new-planned-node2)
	     ;; It is obvious, that neither the lemma-node A v -A nor the second A node are necessary. So they can be deleted !
	     
	     ;; update delta-relations
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      new-planned-node1
	      (pos~list-position '())
	      delta-relation1
	      :new-prefix (pos~list-position '(1)))

	     (decomp=replace-old-pdsnode-in-extdelta!
	      rhs-dummy-node
	      (if trivial-hyp-disj
		  new-planned-node1
		or-elim-node)
	      (pos~list-position '())
	      delta-relation2)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation2)
	     
	     ;; delete unnecessary trivial-hyp things from omega*current-proof-plan
	     (when trivial-hyp-disj
	       (keim::pds=remove-node! omega*current-proof-plan disjunction-node nil t)
	       (keim::pds=remove-node! omega*current-proof-plan or-elim-node nil t))
	     	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit new-planned-node1 ref-graph1 delta-relation1
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph2 delta-relation2
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units))))
	  ((not (decomp=delta-relation-contains-planned-node-p delta-relation2 thm-node))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: or-i: Right part contains no part of the conclusion-node: ORMP"))

	   (let* ((new-planned-node1 (pdsn~open-node-create 
				      (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
							(list (second (data~appl-arguments (node~formula disjunction-node)))))
				      (pdsn~hyps thm-node)
				      (pds~new-node-name)))		  
		  (kill-shit (pds~only-insert-node! new-planned-node1 omega*current-proof-plan))
		  (new-nodes (r2ntop~apply-tactic 'ormp (list nil disjunction-node new-planned-node1) nil)) 
		  (or-elim-node (first new-nodes))
		  (new-planned-node2 thm-node)
		  (trivial-hyp-disj (keim~get disjunction-node 'trivial-hyp)))
	     (declare (ignore kill-shit))

	     ;; It is possible that the disjunction node is a later inserted lemma-node of the kind: A v -A (TND)
	     ;; (in the plist trivial-hyp is 't).
	     ;; This node (and the according clause) is only inserted to do a better decomposition and splitting. Now it can be happens
	     ;; that you can realize after the splitting, that the node really isn't necessary, because the splitting and the
	     ;; decomposition is done in a way that we would get the following lines:
	     ;;             A v -A     (TND)        (disjunction-node)
	     ;;           (- (- A))    (blabla)     (new-planned-node1)
	     ;;               A        (ormp)       (or-elim-node)
	     ;;              CONC      (blabla)     (new-planned-node2)
	     ;; It is obviously, that neither the lemma-node A v -A is needed, nor the complicated new-planned-node1
	     ;; So we delete them, make the or-elim-node open and use it instead of the new-planned-node1
	     ;; Notice: Dies ist nicht ganz so wie im links-Fall obenrueber, da dort die new-planned-node1 = der or-elim-node ist
	     ;;         jedenfalls Formelmaessig !

	     (format t "~%~%TRIVIAL-HYP-DISJ is: ~A" trivial-hyp-disj)
	     
	     ;; update delta-relations
	     (if trivial-hyp-disj
		 (decomp=replace-old-pdsnode-in-extdelta!
		  rhs-dummy-node
		  or-elim-node
		  (pos~list-position '())
		  delta-relation2)
	       (decomp=replace-old-pdsnode-in-extdelta!
		rhs-dummy-node
		new-planned-node1
		(pos~list-position '())
		delta-relation2
		:new-prefix (pos~list-position '(1))))
	     
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      or-elim-node
	      (pos~list-position '())
	      delta-relation1)
	     
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation1)

	     ;; mach die OR-ELIM-NODE open and delete unnecessary things from omega*current-proof-plan
	     (when trivial-hyp-disj
	       (let* ((open-just (pdsj~open-just-create)))
		 (setf (node~justification or-elim-node) open-just))

	       (keim::pds=remove-node! omega*current-proof-plan disjunction-node nil t)
	       (keim::pds=remove-node! omega*current-proof-plan new-planned-node1 nil t))
	     
	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit (if trivial-hyp-disj
						  or-elim-node
						new-planned-node1)
					      ref-graph2 delta-relation2
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph1 delta-relation1
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units))))	     
	  (t

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: or-i: Both parts contain parts of the conclusion-node: ORMP"))
	   
	   (let* ((new-nodes (r2ntop~apply-tactic 'ore (list thm-node disjunction-node nil nil) nil))
		  (new-planned-node1 (third new-nodes))
		  (new-planned-node2 (fourth new-nodes))
		  (hyp-disj-lhs-node (fifth new-nodes))
		  (hyp-disj-rhs-node (sixth new-nodes)))
	     
	     ;; update delta-relations
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node1
	      (pos~list-position '())
	      delta-relation1)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      hyp-disj-lhs-node
	      (pos~list-position '())
	      delta-relation1)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation2)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      rhs-dummy-node
	      hyp-disj-rhs-node
	      (pos~list-position '())
	      delta-relation2)
	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit new-planned-node1 ref-graph1 delta-relation1
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph2 delta-relation2
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units)))))))



;; -------------> IMPLICATION-I


(defun decomp=choose-rule-for-implies-i (implication-node current-unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing rule for implies-i"))

  (let* ((conclusion-node (r2ntop~decompose-unit-conclusion-node current-unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (lhs-dummy-node (pdsn~create nil nil (term~constant-create 'lhs-dummy type*o) nil))
	 (rhs-dummy-node (pdsn~create nil nil (term~constant-create 'rhs-dummy type*o) nil))
	 (dummy-planned-node1 (pdsn~open-node-create (term~constant-create 'planned-dummy1 type*o) nil nil nil))
	 (dummy-planned-node2 (pdsn~open-node-create (term~constant-create 'planned-dummy2 type*o) nil nil nil)))
    (multiple-value-bind
	(conc-ref-delta-list1 conc-ref-delta-list2)
	(split~splitting-ref-graph! (r2ntop~decompose-unit-ref-graph current-unit)
				    delta-relation
				    implication-node
				    lhs-dummy-node rhs-dummy-node
				    dummy-planned-node1 dummy-planned-node2
				    )
      (decomp=decomposition-implies-i implication-node conclusion-node
				      current-unit workon-units
				      lhs-dummy-node rhs-dummy-node
				      (first conc-ref-delta-list1) (first conc-ref-delta-list2)      ;; dummy-planned-nodes
				      (second conc-ref-delta-list1) (second conc-ref-delta-list2)    ;; ref-graphs
				      (third conc-ref-delta-list1) (third conc-ref-delta-list2)))))  ;; delta-relations


(defun decomp=decomposition-implies-i (implication-node thm-node unit workon-units
							lhs-dummy-node rhs-dummy-node
							dummy-planned-node1 dummy-planned-node2
							ref-graph1 ref-graph2
							delta-relation1 delta-relation2
							)
  (let ((pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes unit)))
    (cond ((not (decomp=delta-relation-contains-planned-node-p delta-relation1 thm-node))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: implies-i: Left part contains no part of the conclusion-node: IMPE"))

	   (let* ((new-planned-node1 (pdsn~open-node-create 
				      (first (data~appl-arguments (node~formula implication-node)))
				      (pdsn~hyps thm-node)
				      (pds~new-node-name)))
		  (kill-shit (pds~only-insert-node! new-planned-node1 omega*current-proof-plan))
		  (new-nodes (r2ntop~apply-tactic 'impe (list nil new-planned-node1 implication-node) nil)) 
		  (or-elim-node (first new-nodes))
		  (new-planned-node2 thm-node))
	     (declare (ignore kill-shit))
	     
	     ;; update delta-relations
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      new-planned-node1
	      (pos~list-position '())
	      delta-relation1
	      :new-prefix (pos~list-position '()))
	     (decomp=replace-old-pdsnode-in-extdelta!
	      rhs-dummy-node
	      or-elim-node
	      (pos~list-position '())
	      delta-relation2)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation2)
	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit new-planned-node1 ref-graph1 delta-relation1
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph2 delta-relation2
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units))))
	  ((not (decomp=delta-relation-contains-planned-node-p delta-relation2 thm-node))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: implies-i: Right part contains no part of the conclusion-node: IMPE"))

	   (let* ((new-planned-node1 (pdsn~open-node-create 
				      (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
							(list (second (data~appl-arguments (node~formula implication-node)))))
				      (pdsn~hyps thm-node)
				      (pds~new-node-name)))		  
		  (kill-shit (pds~only-insert-node! new-planned-node1 omega*current-proof-plan))
		  (new-nodes (r2ntop~apply-tactic 'modtoll (list nil implication-node new-planned-node1) nil)) 
		  (or-elim-node (first new-nodes))
		  (new-planned-node2 thm-node))
	     (declare (ignore kill-shit))
	     
	     ;; update delta-relations
	     (decomp=replace-old-pdsnode-in-extdelta!
	      rhs-dummy-node
	      new-planned-node1
	      (pos~list-position '())
	      delta-relation2
	      :new-prefix (pos~list-position '(1)))
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      or-elim-node
	      (pos~list-position '())
	      delta-relation1
	      :new-prefix (pos~list-position '(1)))
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation1)
	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit new-planned-node1 ref-graph2 delta-relation2
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph1 delta-relation1
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units))))
	  ((and (logic~negation-p (second (data~appl-arguments (node~formula implication-node))))
		(data~equal (first (data~appl-arguments (node~formula implication-node)))
			    (first (data~appl-arguments (second (data~appl-arguments (node~formula implication-node)))))))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: implies-i: (imp A -A) -> MERGE with IMP2OR"))
	   
	   ;; (imp A -A) -> (or -A -A) -> MERGE -> -A
	   (let* ((new-nodes-implies-as-or (r2ntop~apply-tactic 'imp2or (list nil implication-node) nil))
		  (disjunction-node (first new-nodes-implies-as-or))
		  (new-nodes (r2ntop~apply-tactic 'idemor (list nil disjunction-node) nil))
		  (merge-node (first new-nodes)))
	     (cond ((decomp=delta-relation-contains-planned-node-p delta-relation2 thm-node)
		    ;; rechter Teil enthaelt anteile an der THM-Node -> waehle ihn um indirekten Beweis zu vermeiden

		    ;; update delta-relations
		    (decomp=replace-old-pdsnode-in-extdelta!
		     rhs-dummy-node
		     merge-node
		     (pos~list-position '())
		     delta-relation2)
		    
		    (cons (decomp~new-decompose-unit thm-node ref-graph2 delta-relation2
						     :pre-selected-nodes (rest pre-selected-nodes))
			  workon-units))
		   (t
		    (setf (node~justification thm-node) (node~justification dummy-planned-node1))
		    
		    ;; update delta-relations
		    (decomp=replace-old-pdsnode-in-extdelta!
		     lhs-dummy-node
		     merge-node
		     (pos~list-position '())
		     delta-relation1
		     :new-prefix (pos~list-position '(1)))
		    (cons (decomp~new-decompose-unit thm-node ref-graph1 delta-relation1
						     :pre-selected-nodes (rest pre-selected-nodes))
			  workon-units)))))
	  
	  
	  (t

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: implies-i: Both parts contain parts of the conclusion-node -> imp2or, ore"))
	   
	   (let* ((new-nodes-implies-as-or (r2ntop~apply-tactic 'imp2or (list nil implication-node) nil))
		  (disjunction-node (first new-nodes-implies-as-or))
		  (new-nodes-ore-for (r2ntop~apply-tactic 'ore (list thm-node disjunction-node nil nil) nil))
		  (new-planned-node1 (third new-nodes-ore-for))
		  (new-planned-node2 (fourth new-nodes-ore-for))
		  (hyp-disj-lhs-node (fifth new-nodes-ore-for))
		  (hyp-disj-rhs-node (sixth new-nodes-ore-for)))
	     
	     
	     (setf (node~justification new-planned-node1) (node~justification dummy-planned-node1))
	     (setf (node~justification new-planned-node2) (node~justification dummy-planned-node2))
	     
	     ;; update delta-relations
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node1
	      (pos~list-position '())
	      delta-relation1)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      lhs-dummy-node
	      hyp-disj-lhs-node
	      (pos~list-position '())
	      delta-relation1
	      :new-prefix (pos~list-position '(1)))
	     (decomp=replace-old-pdsnode-in-extdelta!
	      thm-node
	      new-planned-node2
	      (pos~list-position '())
	      delta-relation2)
	     (decomp=replace-old-pdsnode-in-extdelta!
	      rhs-dummy-node
	      hyp-disj-rhs-node
	      (pos~list-position '())
	      delta-relation2)
	     
	     ;; new-decompose-unit-from-conclusion-node contains update of nodes-to-decompose + integral-formulas
	     (cons (decomp~new-decompose-unit new-planned-node1 ref-graph1 delta-relation1
					      :pre-selected-nodes (rest pre-selected-nodes))
		   (cons (decomp~new-decompose-unit new-planned-node2 ref-graph2 delta-relation2
						    :pre-selected-nodes (rest pre-selected-nodes))
			 workon-units)))))))

;; ----> EXISTENTIAL-I

(defun decomp=decomposition-rule-m-choice (existential-node current-unit workon-units update-units)
  (declare (ignore update-units))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Applying decomposition rule m-choice."))
  
  (let* ((conclusion-node (r2ntop~decompose-unit-conclusion-node current-unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation current-unit))
	 (all-subst-terms (decomp~all-subst-terms-of-quantified-var existential-node delta-relation :kind 'delta))
	 (new-constant (if all-subst-terms
			   ;; a subst-term exists -> take this to create a new constant
			   (r2ntop~new-const (first all-subst-terms))
			 ;; no subst-term exists -> (f.e. (forall x (s a))) -> create dummy constant
			 (r2ntop~new-const (first (data~abstr-domain
						   (first (data~appl-arguments (node~formula existential-node))))))))
	 (skolem-term (if all-subst-terms
			  (data~copy (first all-subst-terms))
			nil))
	 (ref-graph (r2ntop~decompose-unit-ref-graph current-unit))
	 (new-nodes (r2ntop~apply-tactic 'existse (list conclusion-node existential-node nil) (list new-constant)))
	 (hyp-node (fourth new-nodes))
	 (new-planned-node (third new-nodes))
	 (old-thm-node (first new-nodes)))
    
    ;; update the refutation-graph -> only necessary if a subst-term exists
    (when all-subst-terms
      (decomp=replace-skolem-function-with-constant! new-constant
						     skolem-term
						     ref-graph))
    ;; update conclusion-node
    (setf (r2ntop~decompose-unit-conclusion-node current-unit) new-planned-node)

    ;; update delta-relation + nodes-to-decompose + integral-formulas
    (decomp~update-unit current-unit existential-node
			(list (list hyp-node (list (list (pos~list-position '(1 0))
							 (pos~list-position '())))))
			(list existential-node) (list hyp-node))
    (decomp~update-unit current-unit old-thm-node
			(list (list new-planned-node (list (list (pos~list-position '())
								 (pos~list-position '())))))
			nil nil)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: back-unit: ~A" current-unit))
    
    (cons current-unit workon-units)))



#| ------------------------------------------------- END I-DECOMP-RULES ------------------------------------------------ |#

#|

LIST OF ALL DECOMPOSITION RULES AND THEIR DEFINITION:


or-il                       -> rule: oril - done                                      : omega-rules
or-ir                       -> rule: orir - done                                      : omega-rules
e-or-1                      -> Combination imp2or, impi - done                        : res2nd-rules (!!!!)
exists-i                    -> rule: existsi - done                                   : omega-rules
exists-e-indirect           -> Combination indirect, pushneg - done                   : res2nd-rules (!!!!) 
forall-i                    -> rule: foralli - done                                   : omega-rules
and-i                       -> rule: andi - done                                      : omega-rules
implies-rewrite-as-or       -> tactic: imp2or - done                                  : res2nd-rules (!!!!)    
implies-i                   -> rule: impi - done                                      : omega-rules
pullneg                     -> tactic: pullneg - done                                 : omega-rules
pushneg                     -> tactic: pushneg - done                                 : omega-rules
and-e                       -> tactic: andel -done                                    : omega-rules
forall-e                    -> rule: foralle -done                                    : omega-rules
equiv-e                     -> tactic: equive - done        
equiv-i                     -> tactic: equivi - done

or-trash-left               -> tactic: ormp -done                                     : res2nd-rules (!!!!)
or-trash-right              -> tactic: ormp -done                                     : res2nd-rules (!!!!)
imp-trash-left              -> rule: impe - done                                      : res2nd-rules (!!!!)
imp-trash-rigth             -> rule:                            : res2nd-rules (!!!!)

or-e                        -> rule: ore - done                                       : omega-rules
i-implies                   -> tactic: imp2or - done                                  : pres-rules   (!!)   
exists-e                    -> rule: existse - done                                   : omega-rules
indirect                    -> tactic: indirect,noti - done                           : omega-rules
indirect-alt                -> tactic: indirect,noti - done                           : pres-extend-omega (!!)  
not-e-alt                   -> rule: note - done                                      : pres-extend-omega (!!)
axiom                       -> theorem: tertium-non-datur - done                      : res2nd-rules
same                        -> rule: weaken - done                                    : omega-rules

=-I (reflex)                -> tactic: =ref - done                                    : omega-rules
=-neg-i                     -> tactic: neg=i - done                                   : res2nd-rules (!!!!)
=-subst-forw                -> tactic: =subst - done                                  : omega-rules
=-com-forw                  -> tactic: =sym - done                                    : omega-rules
=-com-forw-neg              -> tactic: neg=sym - done                                 : res2nd-rules

|#




