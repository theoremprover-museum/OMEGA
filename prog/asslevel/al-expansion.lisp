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


(mod~defmod ALEXP 
            :uses (al alb alift alj altr alx data infer just keim logic meta misc mod node
		      pds pdsj pdsn sys tacl th) 
            :documentation "Expansion of assertion level steps."
            :exports (alexp+error
                      alexp+expand-warning
                      alexp+warning
                      
                      alexp~expand
                      ))


;################################################
;##                                            ##
;##                  Errors                    ##
;##                                            ##
;################################################

(sys~define-condition
 alexp+error (sys+error)
 ((tactic) (lines))
 (lambda (condition stream)
   (format stream "Cannot apply ~A with ~A"
	   (alexp+error-tactic condition)
	   (alexp+error-lines condition))))

(sys~define-condition
 alexp+warning (sys+warning)
 ((line) (type) (object))
 (lambda (condition stream)
   (format stream "Can't expand line ~A~%         ~:(~S~) ~A is unknown"
	   (alexp+warning-line condition)
	   (alexp+warning-type condition)
	   (alexp+warning-object condition))))

;;; Taktik
(infer~defbbox leaf
	       (outline-function)
	       (test-function)
	       (expansion-function)
	       (parameter-types)
	       (help "The justification method for the leaves of premise trees."))


(defun alexp=insert-between (con pre wff)
  (declare (edited  "28-MAY-1996")
	   (authors Afiedler)
	   (input   "Two premise nodes, where PRE must be a precondition of CON,"
		    "and a formula.")
	   (effect  "Inserts a new node with fomula WFF between CON and PRE.")
	   (value   "The new node."))
  (let ((preconds (altr~preconditions con)))
    (unless (member pre preconds)
      (error "Can't insert new node between ~A and ~A, since the second is not a ~
              precondition of the first." con pre))
    (when alx*tracer
      (omega~output "~%Inserting ~A~%  between ~A~%      and ~A"
		    wff
		    (node~formula con)
		    (node~formula pre)))
    (let ((new (altr~make-premise-node :formula wff
				       :next-node con
				       :preconditions (list pre))))
      (setf (altr~next-node pre) new)
      (setf (altr~preconditions con) (substitute new pre preconds))
      new)))

(defun alexp=insert-before (con wff)
  (declare (edited  "28-MAY-1996")
	   (authors Afiedler)
	   (input   "A premise node and a formula.")
	   (effect  "Inserts a new node with fomula WFF before CON, where before means"
		    "between CON and its preconditions.")
	   (value   "The new node."))
  (let ((preconds (altr~preconditions con)))
    (when alx*tracer
      (omega~output "~%Inserting ~A~%   before ~A"
		    wff
		    (node~formula con)))
    (let ((new (altr~make-premise-node :formula wff
				       :next-node con
				       :preconditions preconds)))
      (mapc #'(lambda (pre) (setf (altr~next-node pre) new)) preconds)
      (setf (altr~preconditions con) (list new))
      new)))

;### gehoert eher in al-expansion.lisp
(defun alexp=set-just! (conclusion paras tactical)
  (declare (edited  "30-JUN-1997")
	   (authors Afiedler)
	   (input   "An premise tree node, a list of parameters and a tactical.")
	   (effect  "Sets the justification of CONCLUSION to the inference rule for"
		    "TACTICAL.") 
	   (value   "Undefined."))
  (setf (node~justification conclusion)
	(pdsj~closed-just-create (cond ((infer~find-method tactical))
				       (t (th~require-completely 'base)
					  (infer~find-method tactical)))
				 nil paras "unexpanded")))

(defun alexp=justify0! (node)
  (declare (edited  "17-SEP-1997")
	   (authors Afiedler)
	   (input   "A leaf node in a premise tree.")
	   (effect  "If the formula in NODE is a double negation, its positive form is"
		    "added above NODE and NODE is justified by 'NotNotI. In any case, the"
		    "leaf is justified by 'Leaf.")
	   (value   "Undefined."))
  (let ((nodewff (node~formula node)))
    (if (alx~double-negation-p nodewff)
	(let ((linewff (node~formula (altr~premise-line node))))
	  (if (alx~match-p nodewff linewff)
	      (alexp=set-just! node nil 'Leaf)
	    (let ((wff (alx~negation-scope (alx~negation-scope nodewff))))
	      (if (alx~match-p wff linewff)
		  (let ((new (alexp=insert-before node wff)))
		    (setf (altr~premise-line new) (altr~premise-line node))
		    (setf (altr~premise-line node) nil)
		    (alexp=set-just! node nil 'NotNotI)
		    (alexp=set-just! new nil 'Leaf))
		(error "Something's wrong in alexp=justify0!")))))
      (alexp=set-just! node nil 'Leaf))))

(defun alexp=justify1! (conclusion premise)
  (declare (edited  "02-JUL-1997" "13-APR-1994 13:26")
	   (authors Afiedler AFIEDLER)
	   (input   "Two premise nodes.")
	   (effect  "Justifies CONCLUSION with PREMISE, possibly with inserting a new node"
		    "between them.")
	   (value   "Undefined."))
  (let ((conwff (node~formula conclusion))
	(prewff (node~formula premise)))
    (when alx*tracer
      (omega~output "~%~%Conclusion   : ~A~%Precondition : ~A" conwff prewff))
    (cond ((and (alx~double-negation-p prewff)
		(data~equal conwff (alx~negation-scope (alx~negation-scope prewff))))
	   (alx~fall "1.1; NotNotE")
	   (alexp=set-just! conclusion nil 'NotNotE))
	  ((and (alx~double-negation-p conwff)
		(data~equal prewff (alx~negation-scope (alx~negation-scope conwff))))
	   (alx~fall "1.2; NotNotI")
	   (alexp=set-just! conclusion nil 'NotNotI))
	  ((and (logic~negation-p prewff) (data~equal (pds~pushneg prewff) conwff))
	   (alx~fall "1.3; Pushneg")
	   (alexp=set-just! conclusion nil 'Pushneg))
	  ((and (logic~negation-p conwff) (data~equal (pds~pushneg conwff) prewff))
	   (alx~fall "1.4; Pullneg")
	   (alexp=set-just! conclusion nil 'Pullneg))
	  ((and (logic~disjunction-p conwff)
		(member prewff (data~appl-arguments conwff) :test #'data~equal))
	   (alx~fall "1.5; OrI (left or right)")
	   (case (position prewff (data~substructs conwff) :test #'data~equal)
	     (1 (alexp=set-just! conclusion nil 'OrIL))
	     (2 (alexp=set-just! conclusion nil 'OrIR))
	     (otherwise (error "Something's wrong in the disjunction case."))))
	  ((and (logic~disjunction-p conwff)
		(logic~implication-p prewff)
		(let* ((disargs (data~appl-arguments conwff))
		       (d1 (car disargs))
		       (d2 (cadr disargs))
		       (impargs (data~appl-arguments prewff))
		       (imp1 (car impargs))
		       (imp2 (cadr impargs)))
		  (and (data~equal d2 imp2)
		       (or (and (logic~negation-p d1)
				(data~equal (alx~negation-scope d1) imp1))
			   (and (logic~negation-p imp1)
				(data~equal d1 (alx~negation-scope imp1)))))))
	   (alx~fall "1.6; Imp2Or")
	   (alexp=set-just! conclusion nil 'Imp2Or))
	  ((and (logic~disjunction-p prewff)
		(logic~implication-p conwff)
		(let* ((disargs (data~appl-arguments prewff))
		       (d1 (car disargs))
		       (d2 (cadr disargs))
		       (impargs (data~appl-arguments conwff))
		       (imp1 (car impargs))
		       (imp2 (cadr impargs)))
		  (and (data~equal d2 imp2)
		       (or (and (logic~negation-p d1)
				(data~equal (alx~negation-scope d1) imp1))
			   (and (logic~negation-p imp1)
				(data~equal d1 (alx~negation-scope imp1)))))))
	   (alx~fall "1.7; Or2Imp")
	   (alexp=set-just! conclusion nil 'Or2Imp))
	  ((and (logic~conjunction-p prewff)
		(member conwff (data~substructs prewff) :test #'data~equal))
	   (alx~fall "1.8; AndE")
	   (alexp=set-just! conclusion nil 'AndE))
	  ((and (logic~equivalence-p prewff)
		(logic~implication-p conwff)
		(let* ((equivargs (data~appl-arguments prewff))
		       (e1 (car equivargs))
		       (e2 (cadr equivargs))
		       (impargs (data~appl-arguments conwff))
		       (i1 (car impargs))
		       (i2 (cadr impargs)))
		  (or (and (data~equal e1 i1) (data~equal e2 i2))
		      (and (data~equal e1 i2) (data~equal e2 i1)))))
	   (alx~fall "1.9; EquivE")
	   (alexp=set-just! conclusion nil 'EquivE))
	  ((logic~universal-quantification-p prewff)
	   (alx~fall "1.10; ForallE")
	   (let* ((var (logic~quantification-bound-variable prewff))
		  (bind (alx~match-p (logic~quantification-scope prewff)
				      conwff nil (list var)))
		  (term (data~assoc var (subst~domain bind) (subst~codomain bind)
				    #'data~equal)))
	     (if bind
		 (alexp=set-just! conclusion
				 (list (if term term var))
				 'ForallE)
	       (error "Something's wrong in the universal case."))))
	  ((logic~existential-quantification-p conwff)
	   (alx~fall "1.11; ExistsI")
	   (let* ((var (logic~quantification-bound-variable conwff))
		  (bind (alx~match-p (logic~quantification-scope conwff)
				      prewff nil (list var)))
		  (term (data~assoc var (subst~domain bind) (subst~codomain bind)
				    #'data~equal)))
	     (if bind
		 (alexp=set-just! conclusion
				 (list (if term term var))
				 'ExistsI)
	       (error "Something's wrong in the existential case."))))
	    
	  ((and (logic~negation-p prewff) (logic~negation-p conwff))
	   (alx~fall "1.12; Negations needing the insertion of a new node")
	   (let ((pre (alx~negation-scope prewff))
		 (con (alx~negation-scope conwff)))
	     (cond ((and (logic~disjunction-p pre)
			 (member con (data~substructs pre) :test #'data~equal))
		    (alx~fall "1.12.1; Pushneg on disjunction")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							   premise
							   (pds~pushneg prewff))))
		   ((and (logic~conjunction-p con)
			 (member pre (data~substructs con) :test #'data~equal))
		    (alx~fall "1.12.2; Pushneg on conjunction")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							   premise
							   (pds~pushneg conwff))))
		   ((and (logic~equivalence-p con)
			 (logic~implication-p pre)
			 (intersection (data~appl-arguments pre)
				       (data~appl-arguments con)))
		    (alx~fall "1.12.3a; Pushneg on equivalence")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							  premise
							  (pds~pushneg conwff)))
		    )
		   ((and (logic~equivalence-p pre)
			 (logic~implication-p con)
			 (intersection (data~appl-arguments pre) (data~appl-arguments
								  con)))
		    (alx~fall "1.12.3b; Pushneg on equivalence")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							  premise
							  (pds~pushneg prewff)))
		    )
		   ((and (logic~existential-quantification-p pre)
			 (alx~match-p (logic~quantification-scope pre)
				       con nil
				       (list (logic~quantification-bound-variable pre))))
		    (alx~fall "1.12.3; Pushneg on existential quantification")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							   premise
							   (pds~pushneg prewff))))
		   ((and (logic~universal-quantification-p con)
			 (alx~match-p (logic~quantification-scope con)
				       pre nil
				       (list (logic~quantification-bound-variable con))))
		    (alx~fall "1.12.4; Pushneg on universal quantification")
		    (alexp=justify1! conclusion
				     (alexp=insert-between conclusion
							   premise
							   (pds~pushneg conwff))))
		   ((and (logic~negation-p pre)
			 (data~equal con (pds~pushneg pre)))
		    (alx~fall "1.12.5; Pushneg-Special")
		    (alexp=set-just! conclusion nil 'Pushneg-Special))
		   ((and (logic~negation-p con)
			 (data~equal pre (pds~pushneg con)))
		    (alx~fall "1.12.6; Pullneg-Special")
		    (alexp=set-just! conclusion nil 'Pullneg-Special))
		   ((and (logic~implication-p pre)
			 (logic~disjunction-p con)
			 (let ((impargs (data~appl-arguments pre))
			       (disargs (data~appl-arguments con)))
			   (and (data~equal (car impargs) (logic~negate (car disargs)))
				(data~equal (cadr impargs) (cadr disargs)))))
		    (alx~fall "1.12.6; NotImp2NotOr")
		    (alexp=set-just! conclusion nil 'NotImp2NotOr))
		   ((and (logic~disjunction-p pre)
			 (logic~implication-p con)
			 (let ((disargs (data~appl-arguments pre))
			       (impargs (data~appl-arguments con)))
			   (and (data~equal (car impargs) (logic~negate (car disargs)))
				(data~equal (cadr impargs) (cadr disargs)))))
		    (alx~fall "1.12.7; NotOr2NotImp")
		    (alexp=set-just! conclusion nil 'NotImp2NotOr))

		   ((logic~negation-p con)
		    (alx~fall "1.12.9; NotNotE")
		    (alexp=justify1! conclusion
				     (alexp=insert-before conclusion
							  (alx~negation-scope con))))
		   (t (error "Can't justify ~A with ~A!" conclusion premise))
		   )))
	    
	  (t (error "Can't justify ~A with ~A!" conclusion premise)))))

(defun alexp=justify2! (conclusion premise1 premise2)
  (declare (edited  "02-JUL-1997" "13-APR-1994 14:17")
	   (authors Afiedler AFIEDLER)
	   (input   "Three premise nodes.")
	   (effect  "Justifies CONCLUSION with PREMISE1 and PREMISE2, possibly with"
		    "inserting a new node.")
	   (value   "Undefined."))
  (let ((conwff (node~formula conclusion))
	(prewff1 (node~formula premise1))
	(prewff2 (node~formula premise2)))
    (when alx*tracer
      (omega~output "~%~%Conclusion   : ~A~%Preconditions: ~A~%               ~A"
		    conwff prewff1 prewff2))
    (flet ((mymember (item seq) (member item seq :test #'data~equal)))
      (cond ((and (logic~conjunction-p conwff)
		  (let ((conargs (data~appl-arguments conwff)))
		    (and (mymember prewff1 conargs)
			 (mymember prewff2 conargs))))
	     (alx~fall "2.1; AndI")
	     (alexp=set-just! conclusion nil 'AndI))
	    ((and (logic~negation-p conwff)
		  (logic~disjunction-p (alx~negation-scope conwff))
		  (let ((disargs (data~appl-arguments (alx~negation-scope conwff))))
		    (and (or (and (logic~negation-p prewff1)
				  (mymember (alx~negation-scope prewff1) disargs))
			     (mymember (alx~negate-formula prewff1) disargs))
			 (or (and (logic~negation-p prewff2)
				  (mymember (alx~negation-scope prewff2) disargs))
			     (mymember (alx~negate-formula prewff2) disargs)))))
	     (alx~fall "2.1a; AndI with Pullneg on disjunction")
	     (alexp=justify1! conclusion
			      (alexp=insert-before conclusion
						   (pds~pushneg conwff))))
	    ((and (logic~negation-p conwff)
		  (logic~implication-p (alx~negation-scope conwff))
		  (let ((impargs (data~appl-arguments (alx~negation-scope conwff))))
		    (or (and (mymember prewff1 impargs)
			     (logic~negation-p prewff2)
			     (mymember (alx~negation-scope prewff2) impargs))
			(and (mymember prewff2 impargs)
			     (logic~negation-p prewff1)
			     (mymember (alx~negation-scope prewff1) impargs)))))
	     (alx~fall "2.1b; AndI with Pullneg on implication")
	     (alexp=justify1! conclusion
			      (alexp=insert-before conclusion
						   (pds~pushneg conwff))))
	    ((and (logic~negation-p conwff)
		  (logic~implication-p (alx~negation-scope conwff))
		  (let ((impargs (data~appl-arguments (alx~negation-scope conwff))))
		    (and (mymember prewff1 impargs)
			 (mymember (alx~negate-formula prewff2) impargs))))
	     (alx~fall "2.1c; AndI with Pullneg on Implication and negated conclusion")
	     (alexp=justify2! conclusion
			      premise1
			      (alexp=insert-between conclusion
						    premise2
						    (alx~negate-formula
						     (alx~negate-formula prewff2)))))
	    ((and (logic~negation-p conwff)
		  (logic~implication-p (alx~negation-scope conwff))
		  (let ((impargs (data~appl-arguments (alx~negation-scope conwff))))
		    (and (mymember prewff2 impargs)
			 (mymember (alx~negate-formula prewff1) impargs))))
	     (alx~fall "2.1d; AndI with Pullneg on Implication and negated conclusion")
	     (alexp=justify2! conclusion
			      (alexp=insert-between conclusion
						    premise1
						    (alx~negate-formula
						     (alx~negate-formula prewff1)))
			      premise2))

	    ((and (logic~equivalence-p conwff)
		  (logic~implication-p prewff1)
		  (logic~implication-p prewff2)
		  (let* ((equivargs (data~appl-arguments conwff))
			 (e1 (car equivargs))
			 (e2 (cadr equivargs))
			 (imp1args (data~appl-arguments prewff1))
			 (i11 (car imp1args))
			 (i12 (cadr imp1args))
			 (imp2args (data~appl-arguments prewff2))
			 (i21 (car imp2args))
			 (i22 (cadr imp2args)))
		    (or (and (data~equal e1 i11) (data~equal e1 i22)
			     (data~equal e2 i12) (data~equal e2 i21))
			(and (data~equal e1 i12) (data~equal e1 i21)
			     (data~equal e2 i11) (data~equal e2 i22)))))
	     (alx~fall "2.2; EquivI")
	     (alexp=set-just! conclusion nil 'EquivI))

	    ((or (and (logic~disjunction-p prewff1)
		      (logic~negation-p prewff2)
		      (let ((disargs (data~appl-arguments prewff1)))
			(and (mymember conwff disargs)
			     (mymember (alx~negation-scope prewff2) disargs))))
		 (and (logic~disjunction-p prewff2)
		      (logic~negation-p prewff1)
		      (let ((disargs (data~appl-arguments prewff2)))
			(and (mymember conwff disargs)
			     (mymember (alx~negation-scope prewff1) disargs)))))
	     (alx~fall "2.3; OrMP")
	     (alexp=set-just! conclusion nil 'OrMP))
	    ((and (logic~disjunction-p prewff1)
		  (let ((disargs (data~appl-arguments prewff1)))
		    (and (mymember conwff disargs)
			 (mymember (alx~negate-formula prewff2) disargs))))
	     (alx~fall "2.3a; OrMP with negated disjunct")
	     (alexp=justify2! conclusion
			      premise1
			      (alexp=insert-between conclusion
						    premise2
						    (alx~negate-formula
						     (alx~negate-formula prewff2)))))
	    ((and (logic~disjunction-p prewff2)
		  (let ((disargs (data~appl-arguments prewff2)))
		    (and (mymember conwff disargs)
			 (mymember (alx~negate-formula prewff1) disargs))))
	     (alx~fall "2.3b; OrMP with negated disjunct")
	     (alexp=justify2! conclusion
			      (alexp=insert-between conclusion
						    premise1
						    (alx~negate-formula
						     (alx~negate-formula prewff1)))
			      premise2))
	    ((and (logic~negation-p prewff1)
		  (logic~conjunction-p (alx~negation-scope prewff1))
		  (logic~negation-p conwff)
		  (let ((conargs (data~appl-arguments (alx~negation-scope prewff1))))
		    (and (mymember prewff2 conargs)
			 (mymember (alx~negation-scope conwff) conargs))))
	     (alx~fall "2.3c; OrMP with Pushneg")
	     (alexp=justify2! conclusion
			      (alexp=insert-between conclusion
						    premise1
						    (pds~pushneg prewff1))
			      premise2))
	    ((and (logic~negation-p prewff2)
		  (logic~conjunction-p (alx~negation-scope prewff2))
		  (logic~negation-p conwff)
		  (let ((conargs (data~appl-arguments (alx~negation-scope prewff2))))
		    (and (mymember prewff1 conargs)
			 (mymember (alx~negation-scope conwff) conargs))))
	     (alx~fall "2.3d; OrMP with Pushneg")
	     (alexp=justify2! conclusion
			      premise1
			      (alexp=insert-between conclusion
						    premise2
						    (pds~pushneg prewff2))))
	    ((or (and (logic~negation-p prewff1)
		      (logic~conjunction-p (alx~negation-scope prewff1))
		      (let ((conargs (data~appl-arguments (alx~negation-scope prewff1))))
			(and (mymember prewff2 conargs)
			     (mymember (alx~negate-formula conwff) conargs))))
		 (and (logic~negation-p prewff2)
		      (logic~conjunction-p (alx~negation-scope prewff2))
		      (let ((conargs (data~appl-arguments (alx~negation-scope prewff2))))
			(and (mymember prewff1 conargs)
			     (mymember (alx~negate-formula conwff) conargs)))))
	     (alx~fall "2.3e; OrMP with two Pushnegs")
	     (alexp=justify1! conclusion
			      (alexp=insert-before conclusion
						   (alx~negate-formula
						    (alx~negate-formula conwff)))))

	    ((or (and (logic~implication-p prewff1)
		      (let ((impargs (data~appl-arguments prewff1)))
			(and (mymember prewff2 impargs)
			     (mymember conwff impargs))))
		 (and (logic~implication-p prewff2)
		      (let ((impargs (data~appl-arguments prewff2)))
			(and (mymember prewff1 impargs)
			     (mymember conwff impargs)))))
	     (alx~fall "2.4; ImpE")
	     (alexp=set-just! conclusion nil 'ImpE))

	    ((or (and (logic~implication-p prewff1)
		      (logic~negation-p prewff2)
		      (logic~negation-p conwff)
		      (let ((impargs (data~appl-arguments prewff1)))
			(and (mymember (alx~negation-scope prewff2) impargs)
			     (mymember (alx~negation-scope conwff) impargs))))
		 (and (logic~implication-p prewff2)
		      (logic~negation-p prewff1)
		      (logic~negation-p conwff)
		      (let ((impargs (data~appl-arguments prewff2)))
			(and (mymember (alx~negation-scope prewff1) impargs)
			     (mymember (alx~negation-scope conwff) impargs)))))
	     (alx~fall "2.5; ModToll")
	     (alexp=set-just! conclusion nil 'ModToll))
	    ((or (and (logic~implication-p prewff1)
		      (logic~negation-p prewff2)
		      (let ((impargs (data~appl-arguments prewff1)))
			(and (mymember (alx~negation-scope prewff2) impargs)
			     (mymember (alx~negate-formula conwff) impargs))))
		 (and (logic~implication-p prewff2)
		      (logic~negation-p prewff1)
		      (let ((impargs (data~appl-arguments prewff2)))
			(and (mymember (alx~negation-scope prewff1) impargs)
			     (mymember (alx~negate-formula conwff) impargs)))))
	     (alx~fall "2.5a; ModToll with negated precondition")
	     (alexp=justify1! conclusion
			      (alexp=insert-before conclusion
						   (alx~negate-formula
						    (alx~negate-formula conwff)))))
	    ((and (logic~implication-p prewff1)
		  (logic~negation-p conwff)
		  (let ((impargs (data~appl-arguments prewff1)))
		    (and (mymember (alx~negate-formula prewff2) impargs)
			 (mymember (alx~negation-scope conwff) impargs))))
	     (alx~fall "2.5b; ModToll with negated postcondition")
	     (alexp=justify2! conclusion
					 prewff1
					 (alexp=insert-between conclusion
							      premise2
							      (alx~negate-formula
							       (alx~negate-formula
								prewff2)))))
	    ((and (logic~implication-p prewff2)
		  (logic~negation-p conwff)
		  (let ((impargs (data~appl-arguments prewff2)))
		    (and (mymember (alx~negate-formula prewff1) impargs)
			 (mymember (alx~negation-scope conwff) impargs))))
	     (alx~fall "2.5c; ModToll with negated postcondition")
	     (alexp=justify2! conclusion
					 (alexp=insert-between conclusion
							      premise1
							      (alx~negate-formula
							       (alx~negate-formula
								prewff1)))
					 prewff2))
	    
	    
	    
	    
	    ((and (logic~negation-p conwff)
		  (logic~negation-p (alx~negation-scope conwff)))
	     (alx~fall "2.6; NotNotI")
	     (alexp=justify1! conclusion
					(alexp=insert-before conclusion
							    (alx~negation-scope
							     (alx~negation-scope
							      conwff)))))
#||
	    ((and (logic~negation-p prewff1)
		  (not (data~equal (pds~pushneg prewff1) prewff1)))
	     (alx~fall "2.18")
	     (alexp=justify2! conclusion
			      (alexp=insert-between conclusion
						    premise1
						    (pds~pushneg prewff1))
			      premise2))
	    ((and (logic~negation-p prewff2)
		  (not (data~equal (pds~pushneg prewff2) prewff2)))
	     (alx~fall "2.19")
	     (alexp=justify2! conclusion
			      premise1
			      (alexp=insert-between conclusion
						    premise2
						    (pds~pushneg prewff2))))
	    ((and (logic~negation-p conwff)
		  (not (data~equal (pds~pushneg conwff) conwff)))
	     (alx~fall "2.20")
	     (alexp=justify1! conclusion
			      (alexp=insert-before conclusion
						   (pds~pushneg conwff))))
||#	    
	    (t (error "Can't justifiy ~A with ~A and ~A!" conclusion premise1
		      premise2))))))



(defun alexp=justify-premise-tree-help! (node)
  (declare (edited  "13-APR-1994 13:20")
	   (authors AFIEDLER)
	   (input   "A premise node.")
	   (effect  "Justifies each not yet justified node in the subtree with NODE as the"
		    "root, possibly with inserting new nodes.")
	   (value   "NODE."))
  (if (and (altr~or-node-p node) (< 1 (length (altr~preconditions node))))
      (setf (altr~preconditions node) (list (altr~find-or-node-premise node))))
  (if (and (altr~premise-line node) (not (altr~root-p node)))
      (setf (altr~preconditions node) nil))
  (let ((premises (altr~preconditions node)))
    (unless (node~justification node)
      (case (length premises)
	(0 (alexp=justify0! node))
	(1 (alexp=justify1! node (car premises)))
	(2 (alexp=justify2! node (car premises) (cadr premises)))
	(otherwise (error "Illegal number of preconditions in ~A" node))))
    (mapc #'alexp=justify-premise-tree-help! (altr~preconditions node))
    node))

(defun alexp=justify-premise-tree! (root)
  (declare (edited  "13-APR-1994 13:20")
	   (authors AFIEDLER)
	   (input   "A premise node.")
	   (effect  "Justifies each not yet justified node in the tree with ROOT as"
		    "root, possibly with inserting new nodes.")
	   (value   "The root of the tree."))
  (alexp=justify-premise-tree-help! root)
  root
  #+old(let ((preconds (altr~preconditions root))
	(line-wff (node~formula (altr~premise-line root))))
    (if (alx~match-p (node~formula root) line-wff)
	root
      (case (length preconds)
	(1 (cond ((alx~match-p (node~formula (car preconds)) line-wff)
		  (let ((actual (car preconds)))
		    (setf (altr~premise-line actual) (altr~premise-line root))
		    (setf (altr~premise-rule-node actual) (altr~premise-rule-node root))
		    actual))
		 (t (error "Somethings wrong with premise tree ~A" root))))
	(otherwise (error "Somethings wrong with premise tree ~A: no match with line."
			  root))))))

(defun alexp=insert-non-leaf-node! (node binding &optional conc)
  (declare (edited  "02-JUL-1997" "25-APR-1994 17:04")
	   (authors Afiedler AFIEDLER)
	   (input   "A premise node, the binding of metavariables, and optional"
		    "a proof line .")
	   (effect  "Inserts NODE before LINE and after its precondition lines in"
		    "{\\vb pds*current-proof-plan}, if it is not a leaf.")
	   (value   "Undefined."))
  (unless (altr~leaf-p node)
    (let* ((just (node~justification node))
	   (method (keim~name (just~method just)))
	   (prems (altr~preconditions node))
	   (prem-lines (mapcar #'altr~premise-line prems))
	   (conclusion (cond (conc)
			     (t
			      (let ((new (pdsn~open-node-create
					  (subst~apply binding (node~formula node))
					  (misc~union (mapcar #'pdsn~hyps prem-lines))
					  (pds~new-node-name))))
				(pds~only-insert-node-between! new
							       prem-lines
							       nil)
				new))))
	   (domain (subst~domain binding))
	   (codomain (subst~codomain binding)))
      (flet ((myerror ()
		      (error (sys~make-condition 'alexp+error
						 :tactic method
						 :lines (mapcar #'altr~premise-line
								prems)))))
					; eine Precondition und zwei Conclusions
	(cond ((string= method :AndE)
	       (let* ((prem (car prems))
		      (left (data~equal (node~formula node)
					(car (data~appl-arguments (node~formula prem)))))
		      (outline
		       (omega::tacl~apply method
					  (append (if left
						      (list conclusion nil)
						    (list nil conclusion))
						  (list (altr~premise-line prem)))
					  nil)))
		 (if outline
		     (setf (altr~premise-line node)
			   (if left (car outline) (cadr outline)))
		   (myerror))))
	      ((string= method :EquivE)
	       (let* ((prem (car prems))
		      (equivargs (data~appl-arguments (node~formula node)))
		      (impargs (data~appl-arguments (node~formula prem)))
		      (left (and (data~equal (car equivargs) (car impargs))
				 (data~equal (cadr equivargs) (cadr impargs))))
		      (outline
		       (omega::tacl~apply method
					  (append (if left
						      (list conclusion nil)
						    (list nil conclusion))
						  (list (altr~premise-line prem)))
					  nil)))
		 (if outline
		     (setf (altr~premise-line node)
			   (if left (car outline) (cadr outline)))
		   (myerror))))
					; zwei Preconditions und eine Conclusion
	      ((string= method :AndI)
	       (let* ((erster (car (data~appl-arguments (node~formula node))))
		      (outline
		       (omega::tacl~apply
			method
			(cons conclusion
			      (mapcar #'altr~premise-line
				      (cond ((data~equal erster
							 (node~formula (car prems)))
					     prems)
					    ((data~equal erster
							 (node~formula (cadr prems)))
					     (reverse prems))
					    (t (myerror)))))
			nil)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :EquivI)
	       (let* ((args (data~appl-arguments (node~formula node)))
		      (arg1 (car args))
		      (arg2 (cadr args))
		      (outline
		       (omega::tacl~apply
			method
			(cons conclusion
			      (mapcar #'altr~premise-line
				      (cond ((let* ((impargs (data~appl-arguments
							      (node~formula (car prems))))
						    (imp1 (car impargs))
						    (imp2 (cadr impargs)))
					       (and (data~equal arg1 imp1)
						    (data~equal arg2 imp2)))
					     prems)
					    ((let* ((impargs (data~appl-arguments
							      (node~formula (cadr prems))))
						    (imp1 (car impargs))
						    (imp2 (cadr impargs)))
					       (and (data~equal arg1 imp1)
						    (data~equal arg2 imp2)))
					     (reverse prems))
					    (t (myerror)))))
			nil)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :ImpE)
	       (let* ((imp (position-if #'(lambda (line)
					    (logic~implication-p (node~formula line)))
					prem-lines))
		      (outline
		       (omega::tacl~apply method
					  (cons conclusion
						(case imp
						  (0 (reverse prem-lines))
						  (1 prem-lines)
						  (otherwise (myerror))))
					  nil)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :ModToll)
	       (let* ((imp (position-if #'(lambda (line)
					    (logic~implication-p (node~formula line)))
					prem-lines))
		      (outline
		       (omega::tacl~apply method
					  (cons conclusion
						(case imp
						  (0 prem-lines)
						  (1 (reverse prem-lines))
						  (otherwise (myerror))))
					  nil)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :OrMP)
	       (let* ((disj (position-if #'(lambda (line)
					     (logic~disjunction-p (node~formula line)))
					 prem-lines))
		      (outline
		       (omega::tacl~apply method
					  (cons conclusion
						(case disj
						  (0 prem-lines)
						  (1 (reverse prem-lines))
						  (otherwise (myerror))))
					  nil)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
					; eine Precondition und eine Conclusion
	      ((string= method :OrIL)
	       (let ((outline
		      (omega::tacl~apply method
					 (list conclusion (altr~premise-line (car prems)))
					 (list (subst~apply
						binding
						(cadr (data~appl-arguments
						       (node~formula node))))))))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :OrIR)
	       (let ((outline
		      (omega::tacl~apply method
					 (list conclusion (altr~premise-line (car prems)))
					 (list (subst~apply
						binding
						(car (data~appl-arguments
						      (node~formula node))))))))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((member method
		       '(:NotNotE :NotNotI ; :OrIL :OrIR  
				  :Pushneg :Pushneg-Special :Pullneg :Pullneg-Special
				  :Or2Imp :Imp2Or :NotImp2NotOr :NotOr2NotImp
					;:AndEL :AndER :EquivE-R :EquivE-L
				  :ForallE ;:ExistsI
				  )
		       :test #'string=)
	       (let ((outline
		      (omega::tacl~apply
		       method
		       (list conclusion (altr~premise-line (car prems)))
		       (mapcar #'(lambda (par)
				   (cond ((data~assoc par domain codomain #'data~equal))
					 (t par)))
			       (pdsj~parameters just)))))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      ((string= method :ExistsI)
	       (let* ((par (car (pdsj~parameters just)))
		      (outline
		       (omega::tacl~apply
			method
			(list conclusion (car prem-lines))
			#+orig(mapcar #'(lambda (par)
					  (cond ((data~assoc par domain codomain #'data~equal))
						(t par)))
				      (pdsj~parameters just))
			(list (cond ((data~assoc par domain codomain #'data~equal))
				    (t par))
			      (data~positions (node~formula (car prems))
					      #'(lambda (sub) (data~equal sub par))))
			)))
		 (if outline
		     (setf (altr~premise-line node) (car outline))
		   (myerror))))
	      (t (myerror)))))))

(defun alexp=insert-premise-tree-help! (node binding)
  (declare (edited  "25-APR-1994 16:57")
	   (authors AFIEDLER)
	   (input   "A premise node, the conclusion line and optional the"
		    "bindings of metavariables.")
	   (effect  "Inserts the subtree with NODE as the root in {\\vb pds*current-proof-plan},"
		    "before CONCLUSION, with substituting the metavariables.")
	   (value   "Undefined."))
  (let ((preconditions (altr~preconditions node)))
    (when preconditions
      (mapc #'(lambda (pre)
		(alexp=insert-premise-tree-help! pre binding))
	    preconditions)
      (mapc #'(lambda (pre)
		(alexp=insert-non-leaf-node! pre binding))
	    preconditions))))

(defun alexp=insert-premise-tree! (node conclusion binding)
  (declare (edited  "03-JUL-1997" "28-MAY-1996")
	   (authors Afiedler Afiedler)
	   (input   "A premise node, the conclusion line and optional the"
		    "bindings of metavariables.")
	   (effect  "Inserts the subtree with NODE as the root in"
		    "{\\vb pds*current-proof-plan}, before CONCLUSION, with substituting"
		    "the metavariables.") 
	   (value   "Undefined."))
  (labels ((insert-tree (n b c)
			(alexp=insert-premise-tree-help! n b)
			(alexp=insert-non-leaf-node! n b c)))
    (cond ((alx~match-p (node~formula node) (node~formula conclusion) binding)
	   (insert-tree node binding conclusion))
	  (t (let ((prems (altr~preconditions node)))
	       (if (= (length prems) 1)
		   (alexp=insert-premise-tree! (car prems) conclusion binding)
		 (error "Cannot insert premise tree ~S" node)))))))

(defun alexp~expand (line)
  (declare (edited  "27-JAN-1994 15:58")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Expands the subproof justifying LINE.")
	   (value   "LINE."))
  (sys~handler-case
   (alb~proof-hash-tables pds*current-proof-plan)
   (alb+hash-error () (alb~make-proof-hash-tables pds*current-proof-plan)))
  (let* ((just (pdsj~above (node~justification line)))
	 (premises-lines (just~premises just))
	 #+old(hyps (pdsn~hyps line))
	 (assertion (if (alj~p just) (car premises-lines))))
    (if assertion
	(let* ((trees (sys~handler-case
		       (altr~get-premise-trees line)
		       (altr+tree-error ()
					(alift~apply! premises-lines assertion line)
					(altr~update-premise-trees! line)
					(altr~get-premise-trees line)))))
	  (let* ((tree (alexp=justify-premise-tree!
			(al~applied-tree trees premises-lines)))
		 (premises (al~find-best-premises tree)))
	    (altr~remove-detours! tree)
	    (altr~show-cond tree)
	    (let ((binding nil))
	      (dolist (pre (cons tree premises))
		; stimmt das?
		(let ((new-binding (alx~match-p (node~formula pre)
						(node~formula (altr~premise-line pre))
						binding)))
		  ;; (if new-binding (setq binding new-binding))

		  ;; Hallo Kleiner Armin !
		  ;; Ich habe den AUDRUCK ein obendrueber duch den Ausdruck eins untendrunter ersetzt !!
		  ;; Gruss
		  ;;   Der kleine Andreas

		  (setq binding
			(do* ((bind-dom (if binding
					    (subst~domain binding)
					  nil))
			      (bind-codom (if binding
					      (subst~codomain binding)
					    nil))
			      (rest-new-dom (if new-binding
						(subst~domain new-binding)
					      nil)
					    (rest rest-new-dom))
			      (rest-new-codom (if new-binding
						  (subst~codomain new-binding)
						nil)
					      (rest rest-new-codom)))
			    ((null rest-new-dom) (subst~create bind-dom bind-codom))
			  (let* ((head-new-dom (first rest-new-dom))
				   (head-new-codom (first rest-new-codom)))
			    (when (not (find head-new-dom bind-dom))
			      (setq bind-dom (append bind-dom (list head-new-dom)))
			      (setq bind-codom (append bind-codom (list
								   head-new-codom)))))))
		  )
	      
		  
		#+old(let ((new-binding (alx~match-p (node~formula pre)
						     (node~formula (altr~premise-line pre))
						     binding)))
		       (if (and new-binding (car new-binding))
			   (setq binding
				 (union binding new-binding
					:test #'(lambda (pair1 pair2)
						  (data~equal (car pair1) (car pair2))
						  (data~equal (cdr pair1) (cdr pair2))))))))
					;binding enthaelt jetzt die Instantiierungen der Meta-Variablen
	      #+old(unless (member assertion hyps)
		     (setf (pdsn~hyps line) (cons assertion hyps)))
	      (alexp=insert-premise-tree! tree line binding)
	      (setf (pdsj~status just) "expanded"))))
      (warn (sys~make-condition 'alexp+expand-warning
				:line line :type 'assertion :object (just~method just))))
    line))

