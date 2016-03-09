;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

;; Agents for the natural deduction calculus for intercalation NIC.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;            NIC Agents for Predicate Logic               ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(agent~defmatrix NIC-IMP-I
		 (information :c-sens)
   (agents (c-predicate (for implication)	 
                        (uses )
			(level 1)
			(information :I)	
                        (definition (when (and (nic~introduction-p (:node implication))
					       (not (agplan~repeated-line-p (:node implication))))
				      (logic~implication-p implication))))))




(agent~defmatrix NICTAC-IMP-E
		 (information :c-sens)
 (agents
           (s-predicate (for implication)
                        (uses antecedent)
                        (exclude term)
                        (level 1)
                        (definition (pred1 implication antecedent)))
           (s-predicate (for implication)
                        (uses succedent)
                        (exclude term)
                        (level 1)
                        (definition (pred2 implication succedent)))
           (s-predicate (for implication)
                        (uses antecedent succedent)
			(exclude term)
                        (level 1)
                        (definition (and (pred1 implication antecedent)
                                         (pred2 implication succedent))))
	   (s-predicate (for implication)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal implication (:param term))))
           (c-predicate (for succedent term)
			(multiple term)
                        (uses )
                        (exclude antecedent implication)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node succedent)))
				      (pred3 (:node succedent)))))
	   (c-predicate (for succedent term)
			(multiple term)
                        (uses antecedent)
                        (exclude implication)
                        (level 1)
                        (definition (pred4 (:node succedent) (:node antecedent))))
           (c-predicate (for succedent)
                        (uses implication)
                        (exclude antecedent)
                        (level 1)
                        (definition (pred2 implication succedent)))
           (s-predicate (for antecedent)
                        (uses implication)
                        (level 1)
                        (definition (pred1 implication antecedent)))
	   (function    (for term)
			(uses implication)
			(level 1)
			(definition implication))
	   (function    (for term)
			(uses antecedent succedent)
			(exclude implication)
			(level 1)
			(definition (term~appl-create (logic~implication-constant)
						      (list antecedent succedent)))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~implication-p a1)
		(term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
           (and (logic~implication-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))
    (pred3 (a1)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~implication-p term)
			     (let ((uni (term~alpha-unify (second (data~appl-arguments term))
							  (node~formula a1))))
			       (when uni (list (subst~apply uni term))))))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))
    (pred4 (a1 a2)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~implication-p term)
			     (let ((uni (term~alpha-unify term
							  (term~appl-create
							   (logic~implication-constant)
							   (list (node~formula a1)
								 (node~formula a2))))))
			       (when uni (list (subst~apply uni term))))))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))))


(agent~defmatrix NIC-AND-I
   (information :pl1 :c-sens)
   (agents
           (c-predicate (for Conjunction)
                        (uses )
			(exclude LConj RConj)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node Conjunction))
					       (not (agplan~repeated-line-p (:node Conjunction))))
				      (logic~conjunction-p Conjunction))))
           (s-predicate (for RConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred2 Conjunction RConj))
			(help "Is Rconj the right conjunct of Conjunction."))
	   (s-predicate (for LConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred1 Conjunction LConj))
                        (help "Is Lconj the left conjunct of Conjunction.")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1) 
                (term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))))


(agent~defmatrix NICTAC-AND-E-L
		 (information :c-sens)
 (agents (s-predicate (for conj)
		      (uses term)
		      (level 1)
		      (definition (term~alpha-equal conj (:param term))))	   
	 (c-predicate (for conc term)
		      (multiple term)
		      (level 1)
		      (uses )
		      (definition (pred1 (:node conc)))))
 (predicates
  (pred1 (a1)
	 (let ((res (mapcan
		     #'(lambda (term)
			 (when (logic~conjunction-p term)
			   (let ((uni (term~alpha-unify (first (data~appl-arguments term))
							(node~formula a1))))
			     (when uni (list (subst~apply uni term))))))
		     (agplan~str-pos-subforms (pds~node-supports a1)))))
	   (when res (list res))))))


(agent~defmatrix NICTAC-AND-E-R
		 (information :c-sens)
 (agents   (s-predicate (for conj)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal conj (:param term))))	   
	   (c-predicate (for conc term)
			(multiple term)
                        (uses )
			(level 1)
                        (definition (when (not (agplan~repeated-line-p (:node conc)))
				      (pred1 (:node conc))))))
 (predicates
  (pred1 (a1)
	 (let ((res (mapcan
		     #'(lambda (term)
			 (when (logic~conjunction-p term)
			   (let ((uni (term~alpha-unify (second (data~appl-arguments term))
							(node~formula a1))))
			     (when uni (list (subst~apply uni term))))))
		     (agplan~str-pos-subforms (pds~node-supports a1)))))
	   (when res (list res))))))



(agent~defmatrix NIC-OR-I-l
		 (information :c-sens)
   (agents (c-predicate (for disjunction par)
                        (uses )
			(exclude ldisj par)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node disjunction))
					       (not (agplan~repeated-line-p (:node disjunction)))
					       (logic~disjunction-p disjunction))
				      (list (second (data~appl-arguments disjunction))))))
           (c-predicate (for disjunction)
                        (uses ldisj)
			(exclude par)
			(level 1)
                        (definition (pred1 disjunction ldisj)))
           (c-predicate (for disjunction)
                        (uses ldisj par)
			(level 1)
                        (definition (pred2 disjunction ldisj (:param par))))
           (s-predicate (for ldisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction ldisj))))
	   ;(function    (for par)
;                        (uses disjunction)
;			(level 1)
;                        (definition (second (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis ldis) (and (logic~disjunction-p dis)
				 (term~alpha-equal (first (data~appl-arguments dis)) ldis)))
    (pred2 (dis ldis par) (and (logic~disjunction-p ldis)
			       (term~alpha-equal (first (data~appl-arguments dis)) ldis)
			       (term~alpha-equal (second (data~appl-arguments dis)) par)))))

(agent~defmatrix NIC-OR-I-r
		 (information :c-sens)
   (agents (c-predicate (for disjunction par )
                        (uses )
			(exclude rdisj par)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node disjunction))
					       (not (agplan~repeated-line-p (:node disjunction)))
					       (logic~disjunction-p disjunction))
				      (list (first (data~appl-arguments disjunction))))))
           (c-predicate (for disjunction)
                        (uses rdisj)
			(exclude par)
			(level 1)
                        (definition (pred1 disjunction rdisj)))
           (c-predicate (for disjunction)
                        (uses rdisj par)
			(level 1)
                        (definition (pred2 disjunction rdisj (:param par))))
           (s-predicate (for rdisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction rdisj))))
	   ;(function    (for par)
;			(level 1)
;                        (uses disjunction)
;			(level 1)
;                        (definition (first (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis rdis) (and (logic~disjunction-p dis)
			   (term~alpha-equal (second (data~appl-arguments dis)) rdis)))
    (pred2 (dis rdis par) (and (logic~disjunction-p disjunction)
			       (term~alpha-equal (second (data~appl-arguments dis)) rdis)
			       (term~alpha-equal (first (data~appl-arguments dis)) par)))))


#|

(agent~defmatrix NICTAC-OR-E-e
		 (information :c-sens)
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (pred2 conc disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node conc)))
				      (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred2 (conc disjunction)
	   (when conc ;;; dummy, declare ignore does not work
	     (logic~disjunction-p disjunction)))
    (pred1 (a1)
	   (when (agplan~str-pos-subf-p (node~formula a1) (pds~node-supports a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~disjunction-p term)
			       (list term)))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))))
|#

#|
(agent~defmatrix NICTAC-OR-E-i
		 (information :c-sens)
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition  (when (not (agplan~repeated-line-p (:node conc)))
				       (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred1 (a1)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~disjunction-p term)
			     (list term)))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))))

|#

(agent~defmatrix NICDUMMY-OR-E-l
		  (information :c-sens)
    (agents (c-predicate (for conc term)
			 (multiple term)
			 (level 1)
			 (definition  (when (not (agplan~repeated-line-p (:node conc)))
					(pred1 (:node conc))))))
    (predicates
     (pred1 (a1)
	    (let ((res (mapcan
			#'(lambda (term)
			    (when (and (logic~disjunction-p term)
				       (term~alpha-equal (first (data~appl-arguments term))
						   (node~formula a1)))
			      (list term)))
			(agplan~str-pos-subforms (pds~node-supports a1))))
		  (given-entries (nic~lookup-or-e-info a1)))
	      (when res
		(if given-entries
		    (let ((new-res (nictac~reduce-or-e-l-termsuggestions res given-entries)))
		      (when new-res (list new-res)))
		  (list res)))))))





(agent~defmatrix NICDUMMY-OR-E-r
		 (information :c-sens)
    (agents (c-predicate (for conc term)
			 (multiple term)
			 (level 1)
			 (definition  (when (not (agplan~repeated-line-p (:node conc)))
					(pred1 (:node conc))))))
    (predicates
     (pred1 (a1)
	    (let ((res (mapcan
			#'(lambda (term)
			    (when (and (logic~disjunction-p term)
				       (term~alpha-equal (second (data~appl-arguments term))
						   (node~formula a1)))
			      (list term)))
			(agplan~str-pos-subforms (pds~node-supports a1))))
		  (given-entries (nic~lookup-or-e-info a1)))
	      (when res
		(if given-entries
		    (let ((new-res (nictac~reduce-or-e-r-termsuggestions res given-entries)))
		      (when new-res (list new-res)))
		  (list res)))))))








(agent~defmatrix NICTAC-or-e
		 (information :c-sens) 
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition  (when (not (agplan~repeated-line-p (:node conc)))
				       (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred1 (a1)
	   (let ((res (remove-if-not #'(lambda (x)
					 (agplan~str-pos-subf-p x (pds~node-supports a1)))
				     (nic~lookup-or-e-applicable-disjunctions a1))))
	     (when res (list res))))))


(agent~defmatrix NICTAC-NEG-E
		 (information :c-sens)
 (agents
           (s-predicate (for negated-form)
                        (uses unnegated-form)
                        (exclude term)
                        (level 1)
                        (definition (pred1 negated-form unnegated-form)))
	   (s-predicate (for negated-form)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal negated-form (:param term))))
           (c-predicate (for falsity-const term)
			(multiple term)
                        (uses )
                        (exclude unnegated-form negated-form)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node falsity-const)))
				      (pred3 (:node falsity-const)))))
	   (c-predicate (for falsity-const term)
			(multiple term)
                        (uses unnegated-form)
                        (exclude negated-form)
                        (level 1)
                        (definition (pred4 (:node falsity-const) (:node unnegated-form))))
           (s-predicate (for unnegated-form)
                        (uses negated-form)
                        (level 1)
                        (definition (pred1 negated-form unnegated-form)))
	   (function    (for term)
			(uses negated-form)
			(level 1)
			(definition negated-form))
	   (function    (for term)
			(uses unnegated-form)
			(exclude negated-form)
			(level 1)
			(definition (agplan~negate unnegated-form))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~negation-p a1)
		(term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
           (and (logic~implication-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))
    (pred3 (a1)
	   (when (logic~falsity-constant-p (node~formula a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~negation-p term)
			       (list term)))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))
    (pred4 (a1 a2)
	   (when (logic~falsity-constant-p (node~formula a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~negation-p term)
			       (let ((uni (term~alpha-unify term
							    (agplan~negate (node~formula a2)))))
				 (when uni (list (subst~apply uni term))))))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))))


(agent~defmatrix NIC-NEG-I
   (information :pl1 :c-sens)		  
   (agents
;           (s-predicate (for falsity)
;                        (uses )
;                        (exclude negation)
;                        (level 1)
;                        (definition (logic~falsity-constant-p falsity)))
;           (s-predicate (for falsity)   ;;; wie geht das hier ;;; aber das ist
;                                                ;;; doch eigentlich quatsch hier
;                        (uses negation)
;                        (level 3)
;                        (definition (pds~find-node-support (:node negation)
;                                                           #'(lambda (x) (keim~equal x falsity))))) 
	   (c-predicate (for negation)
                        (uses )
			(exclude negation)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node negation))
					       (not (agplan~repeated-line-p (:node negation))))
				      (logic~negation-p negation))))))




(agent~defmatrix NIC-FALSE-C
   (information :pl1 :c-sens)		  
   (agents (c-predicate (for conc)
                        (uses )
			(exclude falsity)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node conc))
					       (not (agplan~repeated-line-p conc)))
				      (and (not (logic~falsity-constant-p conc))
					   (or (logic~atom-p conc)
					       (logic~disjunction-p conc)
					       (logic~existential-quantification-p conc))))))))

#+notwendig(pushnew :nic-pl *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;           NIC Agents for First Order Logic              ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(agent~defmatrix NIC-FORALL-I
   (information :pl1)		 
   (agents (c-predicate (for univ-line)
                        (uses )
			(exclude line parameter)
			(level 1)		
                        (definition (when (and (nic~introduction-p (:node univ-line))
					       (not (agplan~repeated-line-p univ-line)))
				      (nic~universal-quantification-p univ-line))))
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
	    (nic~universal-quantification-p univ-line)
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



(agent~defmatrix NIC-EXISTS-I 	
   (agents (c-predicate (for exists-line witness positionlist)
                        (uses )
			(exclude prem)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node exists-line))
					       (nic~existential-quantification-p exists-line))
				      (let* ((var (logic~quantification-bound-variable exists-line))
					     (new-var (term~generate-term-primitive-with-new-name
						       (keim~name var) (term~type var) 'term+variable
						       (pds~environment omega*current-proof-plan)))
					     (positions (data~positions
							 (logic~quantification-scope exists-line)
							 #'(lambda (y) (data~equal y var)))))
					(list new-var positions)))))))

(agent~defmatrix NIC-EXISTS-E-i 	
   (agents
;          (s-predicate (for exline)
;                       (uses )
;                       (exclude conc parameter subgoal)
;                       (level 1)
;                       (definition (nic~existential-quantification-p exline)))
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
	   (nic~existential-quantification-p exline))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (x)
						    (nic~existential-quantification-p x))))
    (pred3 (conc exline)
	   (and
	    (nic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (x)
						     (term~alpha-equal x exline)))))
    (func0 (exline) (when (nic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when
	       (nic~existential-quantification-p exline)
	     (let ((var (logic~quantification-bound-variable exline)))
	       (term~generate-term-primitive-with-new-name
		(keim~name var)
		(term~type var)
		'term+constant
		(pds~environment omega*current-proof-plan)))))
    ))


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
			   (when (nic~universal-quantification-p term)
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


(agent~defmatrix NIC-EXISTS-E-e 	
   (agents
;           (s-predicate (for exline)
;                        (uses )
;                        (level 1)
;                        (definition (nic~existential-quantification-p exline)))
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
	   (nic~existential-quantification-p exline))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (x)
						    (nic~existential-quantification-p x))))
    (pred3 (conc exline)
	   (and
	    (nic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (x)
						     (term~alpha-equal x exline)))))
    (func0 (exline) (when (nic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when
	       (nic~existential-quantification-p exline)
	     (let ((var (logic~quantification-bound-variable exline)))
	       (term~generate-term-primitive-with-new-name
		(keim~name var)
		(term~type var)
		'term+constant
		(pds~environment omega*current-proof-plan)))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;        NIC Agents for Equality and Equivalence          ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Agent fuer IMP-E / Modus Tollens  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(agent~defmatrix NICTAC-MODTOLL
 		 (information :c-sens)
		 (agents
		  (s-predicate (for implication)
			       (uses negsucc)
			       (exclude term)
			       (level 1)
			       (definition (pred1 implication negsucc)))
		  (s-predicate (for implication)
			       (uses negantecedent)
			       (exclude term)
			       (level 1)
			       (definition (pred2 implication negantecedent)))
		  (s-predicate (for implication)
			       (uses negsucc negantecedent)
			       (exclude term)
			       (level 1)
			       (definition (and (pred1 implication negsucc)
						(pred2 implication negantecedent))))
		  (s-predicate (for implication)
			       (uses term)
			       (level 1)
			       (definition (term~alpha-equal implication (:param term))))
		  (c-predicate (for negantecedent term)
			       (multiple term)
			       (uses )
			       (exclude negsucc implication)
			       (level 1)
			       (definition (when (not (agplan~repeated-line-p (:node negantecedent)))
					     (pred3 (:node negantecedent)))))
		  (c-predicate (for negantecedent term)
			       (multiple term)
			       (uses negsucc)
			       (exclude implication)
			       (level 1)
			       (definition (pred4 (:node negantecedent) (:node negsucc))))
		  (c-predicate (for negantecedent)
			       (uses implication)
			       (exclude negsucc)
			       (level 1)
			       (definition (pred2 implication negantecedent)))
		  (s-predicate (for negsucc)
			       (uses implication)
			       (level 1)
			       (definition (pred1 implication negsucc)))
		  (function    (for term)
			       (uses implication)
			       (level 1)
			       (definition implication))
		  (function    (for term)
			       (uses negsucc negantecedent)
			       (exclude implication)
			       (level 1)
			       (definition (term~appl-create (logic~implication-constant)
							     (list negsucc negantecedent)))))
		 (predicates
		  (pred1 (a1 a2)
			 (and (logic~implication-p a1)
			      (term~alpha-equal a2 (nictac=logic-negate (second (data~appl-arguments a1))))))
		  (pred2 (a1 a2)
			 (and (logic~implication-p a1)
			      (term~alpha-equal a2 (nictac=logic-negate (first (data~appl-arguments a1))))))
		  (pred3 (a1)
			 (let ((res (mapcan
				     #'(lambda (term)
					 (when (logic~implication-p term)
					   (let ((uni (term~alpha-unify (first (data~appl-arguments term))
									(nictac=logic-negate (node~formula a1)))))
					     (when uni (list (subst~apply uni term))))))
				     (agplan~str-pos-subforms (pds~node-supports a1)))))
			   (when res (list res))))
		  (pred4 (a1 a2)
			 (let ((res (mapcan
				     #'(lambda (term)
					 (when (logic~implication-p term)
					   (let ((uni (term~alpha-unify term
									(term~appl-create
									 (logic~implication-constant)
									 (list (nictac=logic-negate (node~formula a1))
									       (nictac=logic-negate (node~formula a2)))))))
					     (when uni (list (subst~apply uni term))))))
				     (agplan~str-pos-subforms (pds~node-supports a1)))))
			   (when res (list res))))))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; And now ..... The MODBARB-AGENT !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; --------------------------------------------------------
; MODUS BARBARA
; -------------------------------------------------------

;         line2   line3
;         A=>B     B=>C
;       -----------------
;            A => C
;            conc

(agent~defmatrix NICTAC-modbarbara
		 (agents (c-predicate (for conc)
				      (uses )
				      (exclude line2 line3)
				      (level 1)
				      (definition (logic~implication-p conc)))
			 (s-predicate (for line2)
				      (uses )
				      (exclude conc line3)
				      (level 1)
				      (definition (logic~implication-p line2)))
			 (s-predicate (for line3)
				      (uses )
				      (exclude conc line2)
				      (level 1)
				      (definition (logic~implication-p line3)))
			 (c-predicate (for conc)
				      (uses line2)
				      (exclude line3)
				      (level 1)
				      (definition (pred1 conc line2)))
			 (c-predicate (for conc)
				      (uses line3)
				      (exclude line2)
				      (level 1)
				      (definition (pred2 conc line3)))
			 (s-predicate (for line2)
				      (uses line3)
				      (exclude conc)
				      (definition (pred3 line2 line3)))
			 (s-predicate (for line3)
				      (uses line2)
				      (exclude conc)
				      (definition (pred3 line2 line3)))
			 (c-predicate (for conc)
				      (uses line2 line3)
				      (exclude )
				      (definition (pred4 conc line2 line3))))
		 (predicates
		  (pred1 (a1 a2)
			(and (logic~implication-p a1)
			     (logic~implication-p a2)
			(term~alpha-equal (first (data~appl-arguments a1))
				    (first (data~appl-arguments a2)))))
		  (pred2 (a1 a2)
			(and (logic~implication-p a1)
			     (logic~implication-p a2)
			     (term~alpha-equal (second (data~appl-arguments a1))
				    (second (data~appl-arguments a2)))))
		  (pred3 (a1 a2)
			 (and (logic~implication-p a1)
			      (logic~implication-p a2)
			      (term~alpha-equal (second (data~appl-arguments a1))
					  (first (data~appl-arguments a2)))))

		  (pred4 (c a1 a2)
			 (and (logic~implication-p a1)
			      (logic~implication-p a2)
			      (logic~implication-p c)
			      (term~alpha-equal (first (data~appl-arguments c))
					  (first (data~appl-arguments a1)))
			      (term~alpha-equal (second (data~appl-arguments c))
					  (second (data~appl-arguments a2)))
			      (term~alpha-equal (second (data~appl-arguments a1))
					  (first (data~appl-arguments a2)))))))


|#


(agent~defmatrix NICTAC-EQUIV-I 	
   (agents (c-predicate (for equiv)
                        (uses )
			(level 1)
                        (definition (pred1 equiv)))
	   (s-predicate (for rimp)
                        (uses equiv)
			(level 1)
                        (definition (pred2 equiv rimp)))
	   (s-predicate (for limp)
                        (uses equiv)
			(level 1)
                        (definition (pred4 equiv limp))))
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (equiv rimp) (and (pred1 equiv)
			     (pred3 rimp)
			     (data~equal (batac=equiver-f-create equiv) rimp)))
    (pred3 (imp) (logic~implication-p imp))
    (pred4 (equiv limp) (and (pred1 equiv)
			     (pred3 limp)
			     (data~equal (batac=equivel-f-create equiv) limp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;              Special NIC Weaken Agents                  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(agent~defmatrix NICTAC-WEAKEN
		 (information :c-sens)
   (agents (c-predicate (for lowerline upperline)
                        (uses )
			(level 1)
                        (definition (pred1 (:node lowerline)))))
   (predicates
    (pred1 (a1)
	   (some #'(lambda (x)
		     (when (term~alpha-equal (node~formula x) (node~formula a1))
		       (list x)))
		 (pds~node-supports a1)))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;      Additional Agents for treating HO Concepts         ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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



;;;;;;  
(agent~defmatrix set-ext-contract*
   (agents 
 	   (c-predicate (for conclusion)
                        (uses premise)
 			(level 1)
                         (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~set-ext-contract*-ap premise conclusion))))
           (c-predicate (for conclusion)
			(exclude premise)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~contains-set-equations-p conclusion))))
           (s-predicate (for premise)
                        (uses conclusion)
 			(level 1)
			(definition  (batac~set-ext-contract*-ap premise conclusion)))
;	   (s-predicate (for premise)  not needed in nic because of backward search
;                        (exclude conclusion)
;                        (level 1)
;                        (definition ))
	   ))




;;;;;;  
(agent~defmatrix cnf-normalize
   (agents 
 	   (c-predicate (for conclusion)
                        (uses premise)
 			(level 2)
                         (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~cnf-normalize-ap premise conclusion))))
           (c-predicate (for conclusion)
			(exclude premise)
			(level 2)
                        (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~cnf-normalization-p conclusion))))
           (s-predicate (for premise)
                        (uses conclusion)
 			(level 2)
			(definition  (batac~cnf-normalize-ap premise conclusion)))
;	   (s-predicate (for premise)  not needed in nic because of backward search
;                        (exclude conclusion)
;                        (level 1)
;                        (definition ))
	   ))


;;;;;;  
(agent~defmatrix prenex-form
   (agents 
 	   (c-predicate (for conclusion)
                        (uses premise)
 			(level 2)
                         (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~prenex-form-ap premise conclusion))))
           (c-predicate (for conclusion)
			(exclude premise)
			(level 2)
                        (definition (when (and (nic~introduction-p (:node conclusion))
					       (not (agplan~repeated-line-p (:node
									  conclusion))))
				      (batac~simple-prenex-form-test conclusion))))
           (s-predicate (for premise)
                        (uses conclusion)
 			(level 2)
			(definition  (batac~prenex-form-ap premise conclusion)))
;	   (s-predicate (for premise)  not needed in nic because of backward search
;                        (exclude conclusion)
;                        (level 1)
;                        (definition ))
	   ))





(agent~defagent that-i c-predicate
		(for thati-line)
		(uses)
		(exclude)
		(definition
		  (let ((that (env~lookup-object :that (th~env 'post))))
		    (data~substruct-p thati-line that #'data~schema-equal))))

(agent~defagent that-e s-predicate
		(for thate-line)
		(uses)
		(exclude)
		(definition
		  (and (not (find (keim~name (:node thate-line)) nic*that-e-list :test #'string-equal))
		       (let ((that (env~lookup-object :that (th~env 'post))))
			 (data~substruct-p thate-line that #'data~schema-equal)))))


