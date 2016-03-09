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


(mod~defmod INTEG 
            :uses (data env just logic node omega pds pdsn r2ntop tacl term)
            :documentation "Computing integral formulas."
            :exports (
                      integ+formula
                      integ+justification
                      
                      integ~check-decompose-unit!
                      integ~create-integral-formula
                      integ~create-integral-justification
                      integ~get-integral-formulas
                      integ~update-unit!
                      
                      integ*environment
                      integ*formula-counter
                      integ*maximal-depth
                      integ*var-counter))







(defvar integ*formula-counter 0)
(defvar integ*maximal-depth 2)

(defvar integ*environment (env~create))
(defvar integ*var-counter 0)

#| ---------------------------------------------- Classes + DEFS ------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass integ+formula (node+node)
    ((depth :initform nil
	     :initarg :depth
	     :reader integ=get-depth
	     :writer integ=set-depth!)
     (rec-parents :initform nil
		  :initarg :rec-parents
		  :reader integ=get-rec-parents
		  :writer integ=set-rec-parents!))))
  
(defun integ~create-integral-formula (formula justification depth rec-parents
					      &optional (name (format nil "INT-~A" (incf integ*formula-counter))))
  (make-instance 'integ+formula
		 :name name
		 :formula formula
		 :justification justification
		 :depth depth
		 :rec-parents rec-parents))

(defmethod print-object ((object integ+formula) stream)
  (format stream "<INT: ~A>" (node~formula object)))

(eval-when (load compile eval)
  (defclass integ+justification (just+justification)
    ()))

(defun integ~create-integral-justification (rule parents)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A rule and a list of parents.")
	   (effect  "None.")
	   (value   "A integral-formula justification with slot-rule rule and slot-parents parents."))
  (let ((new-just (make-instance 'integ+justification
				 :method rule)))
    (setf (just~premises new-just) parents)
    new-just))
  
(defmethod print-object ((object integ+justification) stream)
  (format stream "<INT-JUST: ~A at ~A>" (just~method object) (just~premises object)))

#| -------------------------------------------------------------- Auxiliaries ------------------------------------------------------- |#

(defun integ=pdsnode-to-integral-formula (pdsnode)
  (declare (edited  "29-JUL-1996")
	   (authors Ameier)
	   (input   "A pdsnode.")
	   (effect  "None.")
	   (value   "A new integral formula with the formula of pdsnode and"
		    "a initial justification ,depth 0 and empty rec-parents."))
  (integ~create-integral-formula (data~copy (node~formula pdsnode)
					    :downto '(data+primitive)
					    )
				 (integ~create-integral-justification 'initial-nd (list pdsnode))
				 0
				 (list pdsnode)))

(defun integ=usable-p (integral-formula)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A integral-formula.")
	   (effect  "None.")
	   (value   "T if the integral-formula is still usable for further inferrence"
		    "with it, that its depth is lesser than maximal depth."
		    "False otherwise."))
  (if (= (integ=get-depth integral-formula) integ*maximal-depth)
      nil
    t))

(defun integ=apply-tactic (tactic outline parameters)
  (tacl~apply tactic outline parameters))
  
#| ------------------------------------------ CREATING AND HANDLING INTEGRAL FORMULAS  ----------------------------------------------- |#

(defun integ~get-integral-formulas (list-of-pdsnodes)
  (declare (edited  "29-JUL-1996")
	   (authors Ameier)
	   (input   "A list of pdsnodes.")
	   (effect  "None.")
	   (value   "A list of all integral-formulas who can be inferrenced from the formulas of"
		    "this nodes till the depth integ*maximal-depth."))
  (integ=inferrence-list-list nil (mapcar #'integ=pdsnode-to-integral-formula list-of-pdsnodes)))

(defun integ=inferrence-list-list (old-formulas new-formulas)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "Two lists of integral-formulas.")
	   (effect  "None.")
	   (value   "A list of integral-formulas consisting of:"
		    "The first list (called old-integral-formulas) , the second list"
		    "(called new-integral-formulas and all possible inferrences between"
		    "the new-integral-formulas among themselves and between the"
		    "news and the olds. ATTENTION: No further inferrence seek between the old"
		    "themselves."))
  (if new-formulas
      (let* ((next (first new-formulas))
	     (inferrences-with-old (integ=inferrence-list-single old-formulas next))
	     (inferrences-self (integ=inferrence-self next)))
	(integ=inferrence-list-list (append old-formulas (list next)) (append (rest new-formulas) inferrences-with-old
									      inferrences-self)))
    old-formulas))

(defun integ=inferrence-list-single (old-formulas new-formula)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A list of integral-formulas (calles olds) and a single integral-formula"
		    "(called new).")
	   (effect  "None.")
	   (value   "A list of integral-formulas consisting of all possible inferrences"
		    "of the new with the olds."))
  (apply 'append (mapcar #'(lambda (old-formula)
			     (append
			      (integ=inferrence-formula-on-formula old-formula new-formula)
			      (integ=inferrence-formula-on-formula new-formula old-formula)))
			 old-formulas)))


#| In Justification always the first term is subterm of the second |#

(defun integ=inferrence-formula-on-formula (int-formula1 int-formula2)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "Two integral-formulas.")
	   (effect  "None.")
	   (value   "A list of inferrencable new integral-formulas in follwoing way:"
		    "The first is checked to be a subterm of the second."
		    "If this is the case it is tried to make a inferrence in the following"
		    "cases: 1) first = A  , second = (A => B)  ->  B (IMP-E)"
		    "       2) first = -B , second = (A => B)  ->  -A (IMP-E)"
		    "       3) first = -A , second = (A v B)   ->  B (OR-E-L)"
		    "       4) first = -B , second = (A v B)   ->  A (OR-E-R)"))
  (let* ((term1 (node~formula int-formula1))
	 (term2 (node~formula int-formula2))
	 (depth1 (integ=get-depth int-formula1))
	 (depth2 (integ=get-depth int-formula2))
	 (rec-parents1 (integ=get-rec-parents int-formula1))
	 (rec-parents2 (integ=get-rec-parents int-formula2))
	 (not-object (env~lookup-object 'not (pds~environment omega*current-proof-plan))))
    (if (or (not (integ=usable-p int-formula1)) (not (integ=usable-p int-formula2)) (logic~atom-p term2))
	nil
      (cond ((logic~disjunction-p term2)
	     (let* ((args (data~appl-arguments term2))
		    (arg1 (first args))
		    (arg2 (second args))
		    (neg-arg1 (term~appl-create not-object (list arg1)))
		    (neg-arg2 (term~appl-create not-object (list arg2))))
	       (cond ((data~equal  neg-arg1 term1)
		      (list (integ~create-integral-formula arg2 
							   (integ~create-integral-justification 'or-e-l
												(list int-formula1 int-formula2))
							   (+ (max depth1 depth2) 1)
							   (remove-duplicates (cons int-formula1
										    (cons int-formula2
											  (append rec-parents1 rec-parents2)))))))
		     ((data~equal neg-arg2 term1)		  
		      (list (integ~create-integral-formula arg1 
							   (integ~create-integral-justification 'or-e-r
												(list int-formula1 int-formula2))
							   (+ (max depth1 depth2) 1)
							   (remove-duplicates (cons int-formula1
										    (cons int-formula2
											  (append rec-parents1 rec-parents2))))))))))
	    ((logic~implication-p term2)
	     (let* ((args (data~appl-arguments term2))
		    (arg1 (first args))
		    (arg2 (second args))
		    (neg-arg2 (term~appl-create not-object (list arg2))))
	       (cond ((data~equal arg1 term1) 
		      (list (integ~create-integral-formula arg2 
							   (integ~create-integral-justification 'imp-e-l
												(list int-formula1 int-formula2)) 
							   (+ (max depth1 depth2) 1)
							   (remove-duplicates (cons int-formula1
										    (cons int-formula2
											  (append rec-parents1 rec-parents2)))))))
		     ((data~equal neg-arg2 term1)
		      (list (integ~create-integral-formula neg-arg2 
							   (integ~create-integral-justification 'imp-e-r
												(list int-formula1 int-formula2))
							   (+ (max depth1 depth2) 1)
							   (remove-duplicates (cons int-formula1
										    (cons int-formula2
											  (append rec-parents1 rec-parents2))))))))))
	    (t
	     nil)))))


(defun integ=inferrence-self (integ-formula)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A integral-formula.")
	   (effect  "None.")
	   (value   "A list of integral-formulas:"
		    "If formula of integral-formula is a conjunction or a equivalence a list of"
		    "integral-formulas of the two conjunctet subformulas."
		    "Otherwise nil."))
  (let ((term (node~formula integ-formula))
	(depth (integ=get-depth integ-formula))
	(rec-parents (integ=get-rec-parents integ-formula)))
    (if (= depth integ*maximal-depth)
	nil
      (cond ((logic~conjunction-p term)
	     (let* ((args (data~appl-arguments term))
		    (arg1 (first args))
		    (arg2 (second args)))
	       (list (integ~create-integral-formula arg1 
						    (integ~create-integral-justification 'and-e-l (list integ-formula))
						    (+ depth 1)
						    (remove-duplicates (cons integ-formula rec-parents)))
		     (integ~create-integral-formula arg2 
						    (integ~create-integral-justification 'and-e-r (list integ-formula))
						    (+ depth 1)
						    (remove-duplicates (cons integ-formula rec-parents))))))
	    ((logic~equivalence-p term)
	     (let* ((args (data~appl-arguments term))
		    (implies (env~lookup-object 'implies (pds~environment omega*current-proof-plan)))
		    (impl1 (term~appl-create implies args))
		    (impl2 (term~appl-create implies (reverse args))))
	       (list (integ~create-integral-formula impl1 
						    (integ~create-integral-justification 'equiv-e-l (list integ-formula))
						    (+ depth 1)
						    (remove-duplicates (cons integ-formula rec-parents)))
		     (integ~create-integral-formula impl2 
						    (integ~create-integral-justification 'equiv-e-r (list integ-formula))
						    (+ depth 1)
						    (remove-duplicates (cons integ-formula rec-parents))))))
	    (t
	     nil)))))

#| -------------------------------------------- INTERGAL-FORMULA-MATCHING -> PDSNODE-NODES ------------------------------------------ |#	 


(defun integ~check-decompose-unit! (unit)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A decompose unit.")
	   (effect  "It is checked if the conclusion-node of the unit is contained"
		    "in the integral-formulas of the unit."
		    "If so than the omega*current-proof-plan is updated by directly"
		    "proof the conclusion-node by using the justification informations"
		    "of the integral-formulas.")
	   (value   "T if the conclusion-node is proofed by integral-formulas."
		    "Otherwise nil."))
  (if r2ntop*integral-formulas
      (let* ((integral-formulas (r2ntop~decompose-unit-integral-formulas unit))
	     (conclusion-node (r2ntop~decompose-unit-conclusion-node unit)))
	(multiple-value-bind
	    (flag integral-formula)
	    (integ=member-in-integral-formulas-p conclusion-node integral-formulas)
	  (if flag
	      (progn
		(integ=conclusion+integral-formula-2-ndproof! integral-formula conclusion-node)
		t)
	    nil)))
    nil))
			    
(defgeneric integ=member-in-integral-formulas-p (object list-of-integral-formulas)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "Apdsnode or a term and a list-of-integral-formulas.")
	   (effect  "None.")
	   (value   "Multiple Value:"
		    "FIRST: T if the term (or the formula of pdsnode) is contained in the"
		    "       list of integral-formulas, that means there exists a intergal-formula"
		    "       whose formula is equal to the term."
		    "       Otherwise nil."
		    "SECOND: If first is t the equal integral-formula, otherwise nil."))
  (:method ((object pdsn+node) list-of-integral-formulas)
	   (multiple-value-bind
	       (flag integral-formula)
	       (integ=member-in-integral-formulas-p (node~formula object) list-of-integral-formulas)
	     (values flag integral-formula)))
  (:method (object list-of-integral-formulas)
	   (let ((equal-formula (first (remove-if-not #'(lambda (integral-formula)
							  (data~equal object (node~formula integral-formula)))
						      list-of-integral-formulas))))
	     (if equal-formula
		 (values t equal-formula)
	       (values nil nil)))))

(defun integ=conclusion+integral-formula-2-ndproof! (integral-formula conclusion-node)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "An integral-formula and a conlusion-node whose formulas are"
		    "equal.")
	   (effect  "The integral-formula is recursively (over her parents and so on ...)"
		    "translated in a sequance of justified pdsnodes whose last is a pdsnode"
		    "with same formula as conclusion-node, than the justification of"
		    "conclusion-node is set same as this last node.")
	   (value   "Undefined."))
  (let* ((new-pdsnode (integ=integral-formula-2-pdsnode integral-formula)))
    (integ=apply-tactic 'weaken (list conclusion-node new-pdsnode) nil)))

(defun integ=integral-formula-2-pdsnode (integral-formula)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "An integral-formula.")
	   (effect  "The omega*current-proof-plan is updated by a translation"
		    "of these integral-formula by recursively applying"
		    "nd-rules from the parents of the integral-formulas"
		    "according to the justification. This process is continued"
		    "recursively about all ancestors of the integral-formulas"
		    "to pdsnodes.")
	   (value   "The pdsnode who accords now to the integral-formula."))
  (let* ((just (node~justification integral-formula))
	 (rule (just~method just))
	 (parents (just~premises just)))
    (cond ((equal rule 'initial-nd)
	   (first parents))
	  ((equal rule 'or-e-l)
	   (let* ((or-node (integ=integral-formula-2-pdsnode (second parents)))
		  (left-part-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'ormp (list nil or-node left-part-node) nil))))
	  ((equal rule 'or-e-r)
	   (let* ((or-node (integ=integral-formula-2-pdsnode (second parents)))
		  (rigth-part-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'ormp (list nil or-node rigth-part-node) nil))))
	  ((equal rule 'imp-e-l)
	   (let* ((imp-node (integ=integral-formula-2-pdsnode (second parents)))
		  (left-part-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'impe (list nil left-part-node imp-node) nil))))
	  ((equal rule 'imp-e-r)
	   (let* ((imp-node (integ=integral-formula-2-pdsnode (second parents)))
		  (rigth-part-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'modtoll (list nil rigth-part-node imp-node) nil))))
	  ((equal rule 'and-e-l)
	   (let* ((and-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'andel (list nil and-node) nil)))) 
	  ((equal rule 'and-e-r)
	   (let* ((and-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'ander (list nil and-node) nil))))
	  ((equal rule 'equiv-e-l)
	   (let* ((equiv-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'equivel (list nil equiv-node) nil))))
	  ((equal rule 'equiv-e-r)
	   (let* ((equiv-node (integ=integral-formula-2-pdsnode (first parents))))
	     (first (integ=apply-tactic 'equiver (list nil equiv-node) nil)))))))



	  

#| -------------------------------------------------------- UPDATING UNIT ------------------------------------------------------ |#

(defun integ~update-unit! (unit nodes-to-remove nodes-to-add)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A decompose unit, a two lists of pdsnodes.")
	   (effect  "The first-list of pdsnodes is a list of nodes who aren't"
		    "anymore ready for integral-inferrence, so all integral-formulas"
		    "of unit who depend from one of them are deletet."
		    "The second list of pdsnodes are new pdsnodes to create"
		    "new integral-formulas and to do new inferrence with the"
		    "remainings from deleting the first list."
		    "The so generated integral-formula-list is put in the"
		    "integral-formula slot of the unit.")
	   (value   "Undefined."))
  (let* ((integ-formula-list (r2ntop~decompose-unit-integral-formulas unit)))
    (setf (r2ntop~decompose-unit-integral-formulas unit)
	  (integ=inferrence-list-list (integ=delete-nodes integ-formula-list nodes-to-remove)
				      (mapcar #'integ=pdsnode-to-integral-formula nodes-to-add))
	  )))

(defun integ=delete-nodes (integral-formulas nodes-to-remove)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A list of integral-formulas and a list of pdsnodes.")
	   (effect  "None.")
	   (value   "All integral-formulas who depend from one of the pdsnodes"
		    "is removed (pdsnode is in rec-parents of integral-formula),"
		    "the list of the rest integral-formulas is returned."))
  (remove-if #'(lambda (integral-formula)
		 (let* ((rec-parents (integ=get-rec-parents integral-formula)))
		   (if (intersection rec-parents nodes-to-remove)
		       t
		     nil)))
	     integral-formulas))























