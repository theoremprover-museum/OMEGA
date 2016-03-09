;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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
 
(in-package "OMEGA")

(mod~defmod SBF 
            :uses (data logic term)
            :documentation "Help Functions for Assertion Application"
            :exports (sbf~subformula
		      ))

;;; Help Functions for Assertion Application 
;;  

(defun sbf~subformula (assertion ass-sign test-func sbf-sign &optional all-vars ex-vars)
  ;;; A formula and a predicate
  ;;; Returns: - the subformula of ASSERTION satisfying TEST-FUNC,
  ;;           - the premises that must be proven to apply ASSERTION for
  ;;           deducing the resulted subformula
  ;;           - Other subformulae of ASSERTION as side-consequences
  (let ((head (data~appl-function assertion)))
    (cond
     ((logic~negation-p assertion)
      (sbf~subformula (first (data~appl-arguments assertion)) (not ass-sign)
		      test-func sbf-sign all-vars ex-vars))
     ((and (logic~universal-quantor-p head)
	   (string-equal (keim~name head) 'forall-sort))
      (let* ((abstr (first (data~appl-arguments assertion)))
	     (subformula (data~abstr-range abstr))
	     (sort (second (data~appl-arguments assertion)))
	     (implication (term~appl-create (logic~implication-constant)
					    (list (term~appl-create sort
								    (data~abstr-domain abstr))
						  subformula))))
	
	(if (funcall test-func implication)
	    (when (eq ass-sign sbf-sign)
	      (if ass-sign
		  (values implication NIL NIL (append all-vars (data~abstr-domain abstr)) ex-vars)
		(values implication NIL NIL all-vars (append ex-vars (data~abstr-domain abstr)))))
	  (if ass-sign
	      (sbf~subformula implication ass-sign test-func sbf-sign
			      (append all-vars (data~abstr-domain abstr)) ex-vars)
	    (sbf~subformula implication ass-sign test-func sbf-sign
			    all-vars (append ex-vars (data~abstr-domain abstr)))))))
     ((logic~universal-quantor-p head)
      (let* ((abstr (first (data~appl-arguments assertion)))
	     (subformula (data~abstr-range abstr)))
	(if (funcall test-func subformula)
	    (when (eq ass-sign sbf-sign)
	      (if ass-sign
		  (values subformula NIL NIL (append all-vars (data~abstr-domain abstr)) ex-vars)
	      (values subformula NIL NIL all-vars (append ex-vars (data~abstr-domain abstr)))))
	  (if ass-sign
	      (sbf~subformula subformula ass-sign test-func sbf-sign
			      (append all-vars (data~abstr-domain abstr)) ex-vars)
	    (sbf~subformula subformula ass-sign test-func sbf-sign
			    all-vars (append ex-vars (data~abstr-domain abstr)))))))
     ((and (logic~existential-quantor-p head)
	   (string-equal (keim~name head) 'exists-sort))
      (let* ((abstr (first (data~appl-arguments assertion)))
	     (subformula (data~abstr-range abstr))
	     (sort (second (data~appl-arguments assertion)))
	     (conjunction (term~appl-create (logic~conjunction-constant)
					    (list (term~appl-create sort
								    (data~abstr-domain abstr))
						  subformula))))
	
	(if (funcall test-func conjunction)
	    (when (eq ass-sign sbf-sign)
	      (if ass-sign
		  (values conjunction NIL NIL all-vars (append ex-vars (data~abstr-domain abstr)))
		(values conjunction NIL NIL (append all-vars (data~abstr-domain abstr)) ex-vars)))
	  (if ass-sign
	      (sbf~subformula conjunction ass-sign test-func sbf-sign
			      all-vars (append ex-vars (data~abstr-domain abstr)))
	    (sbf~subformula conjunction ass-sign test-func sbf-sign
			    (append all-vars (data~abstr-domain abstr)) ex-vars)))))
     ((logic~existential-quantor-p head)
      (let* ((abstr (first (data~appl-arguments assertion)))
	     (subformula (data~abstr-range abstr)))
	(if (funcall test-func subformula)
	    (when (eq ass-sign sbf-sign)
	      (if ass-sign
		  (values subformula NIL NIL all-vars (append ex-vars (data~abstr-domain abstr)))
		(values subformula NIL NIL (append all-vars (data~abstr-domain abstr)) ex-vars)))
	  (if ass-sign
	      (sbf~subformula subformula ass-sign test-func sbf-sign
			      all-vars (append ex-vars (data~abstr-domain abstr)))
	    (sbf~subformula subformula ass-sign test-func sbf-sign
			    (append all-vars (data~abstr-domain abstr)) ex-vars)))))
     (;(logic~conjunction-connective-p head)
      (logic~conjunction-p assertion)
      (if ass-sign
	  ;;; conjunction: A and B
	  (let ((conjuncts (data~appl-arguments assertion)))
	    (if (funcall test-func (first conjuncts))
		(when sbf-sign
		  (values (first conjuncts) NIL (rest conjuncts) all-vars ex-vars))
	      (if (funcall test-func (second conjuncts))
		  (when sbf-sign
		    (values (second conjuncts) NIL (list (first conjuncts)) all-vars ex-vars))
		(multiple-value-bind (sbf1 prems1 other-sbfs1 new-allvs1 new-exvs1)
		    (sbf~subformula (first conjuncts) T test-func sbf-sign all-vars ex-vars)
		  (if sbf1 (values sbf1 prems1 (append other-sbfs1 (rest conjuncts)) new-allvs1 new-exvs1)
		    (multiple-value-bind (sbf2 prems2 other-sbfs2 new-allvs2 new-exvs2)
			(sbf~subformula (second conjuncts) T test-func sbf-sign all-vars ex-vars)
		      (when sbf2
			(values sbf2 prems2 (cons (first conjuncts) other-sbfs2)
				new-allvs2 new-exvs2))))))))
	;;; disjuncts: not (A and B) is not(A) or not(B):
	(let ((disjuncts (data~appl-arguments assertion)))
	  (if (funcall test-func (first disjuncts))
	      (unless sbf-sign
		(values (term~appl-create (logic~negation-constant :name 'not) (butlast disjuncts))
			(rest disjuncts) NIL all-vars ex-vars))
	    (if (funcall test-func (second disjuncts))
		(unless sbf-sign
		  (values (term~appl-create (logic~negation-constant :name 'not) (last disjuncts))
			  (butlast disjuncts) NIL all-vars ex-vars))
	      (multiple-value-bind (sbf1 prems1 other-sbfs1 new-allvs1 new-exvs1)
		  (sbf~subformula (first disjuncts) ass-sign test-func sbf-sign all-vars ex-vars)
		(if sbf1
		    (values sbf1 (cons (second disjuncts) prems1) other-sbfs1 new-allvs1 new-exvs1)
		  (multiple-value-bind (sbf2 prems2 other-sbfs2 new-allvs2 new-exvs2)
		      (sbf~subformula (second disjuncts) ass-sign test-func sbf-sign all-vars ex-vars)
		    (when sbf2
		      (values sbf2 (cons (second disjuncts) prems2) other-sbfs2 new-allvs2 new-exvs2))))))))))
     (;(logic~disjunction-connective-p head)
      (logic~disjunction-p assertion)
      (if ass-sign
	  (let ((disjuncts (data~appl-arguments assertion)))
	    (if (funcall test-func (first disjuncts))
		(when sbf-sign
		  (values (first disjuncts)
			  (list (term~appl-create (logic~negation-constant :name 'not)
						  (rest disjuncts)))
			  NIL all-vars ex-vars))
	      (if (funcall test-func (second disjuncts))
		  (when sbf-sign
		    (values (second disjuncts)
			    (list (term~appl-create (logic~negation-constant :name 'not)
						    (butlast disjuncts)))
			    NIL all-vars ex-vars))
		(multiple-value-bind (sbf1 prems1 other-sbfs1 new-alvs1 new-exvs1)
		    (sbf~subformula (first disjuncts) T test-func sbf-sign all-vars ex-vars)
		  (if sbf1
		      (values sbf1
			      (cons (term~appl-create (logic~negation-constant :name 'not)
						      (rest disjuncts))
				    prems1)
			      other-sbfs1 new-alvs1 new-exvs1)
		    (multiple-value-bind (sbf2 prems2 other-sbfs2 new-alvs2 new-exvs2)
			(sbf~subformula (second disjuncts) T test-func sbf-sign all-vars ex-vars)
		      (when sbf2
			(values sbf2
				(cons (term~appl-create (logic~negation-constant :name 'not)
							(butlast disjuncts))
				      prems2)
				other-sbfs2 new-alvs2 new-exvs2))))))))
	;;; not(A or B) is: not(A) and not(B)
	(let ((conjuncts (data~appl-arguments assertion)))
	  (if (funcall test-func (first conjuncts))
	      (unless sbf-sign
		(values (term~appl-create (logic~negation-constant :name 'not)
					  (butlast conjuncts))
			NIL
			(list (term~appl-create (logic~negation-constant :name 'not)
						(rest conjuncts)))
			all-vars ex-vars))
	    (if (funcall test-func (second conjuncts))
		(unless sbf-sign
		  (values (term~appl-create (logic~negation-constant :name 'not)
					    (last conjuncts))
			  NIL
			  (list (term~appl-create (logic~negation-constant :name 'not)
						  (butlast conjuncts)))
			  all-vars ex-vars))
	      (multiple-value-bind (sbf1 prems1 other-sbfs1 new-alvs1 new-exvs1)
		  (sbf~subformula (first conjuncts) ass-sign test-func sbf-sign all-vars ex-vars)
		(if sbf1
		    (values sbf1 prems1
			    (cons (term~appl-create (logic~negation-constant :name 'not)
						    (rest conjuncts))
				  other-sbfs1)
			    new-alvs1 new-exvs1)
		  (multiple-value-bind (sbf2 prems2 other-sbfs2 new-alvs2 new-exvs2)
		      (sbf~subformula (second conjuncts) ass-sign test-func sbf-sign all-vars ex-vars)
		    (when sbf2
		      (values sbf2 prems2
			      (cons (term~appl-create (logic~negation-constant :name 'not)
						      (butlast conjuncts))
				    other-sbfs2)
			      new-alvs2 new-exvs2))))))))))
     (;(logic~implication-connective-p head)
      (logic~implication-p assertion)
      (if ass-sign
	  ;;; A imp B
	  (let ((args (data~appl-arguments assertion)))
	    (if (funcall test-func (second args))
		;;; Possible to have p(t1 .. tn) imp p(s1 .. sn)
		;; here we have to consider both p(s1 .. sn) and not p(t1 .. tn)
		(cond (sbf-sign
		       ;;; B corresponds to the searched subformula
		       (values (second args) (list (first args)) NIL all-vars ex-vars))
		      ((and (funcall test-func (first args)) (null sbf-sign))
		       ;;; not(A) corresponds to the searched subformula
		       (values (term~appl-create (logic~negation-constant :name 'not)
						 (list (first args)))
			       (list (term~appl-create (logic~negation-constant :name 'not)
						       (rest args)))
			       NIL all-vars ex-vars))
		      (T
		       ;;; Check whether not(A) can assert the searched subformula:
		       (multiple-value-bind (sbf1 prems1 other-sbfs1 new-allvs1 new-exvs1)
			   (sbf~subformula (first args) NIL test-func sbf-sign all-vars ex-vars)
			 (when sbf1
			   (values sbf1 (cons (term~appl-create (logic~negation-constant :name 'not)
								(rest args)) prems1)
				   other-sbfs1 new-allvs1 new-exvs1)))))
	      (multiple-value-bind (sbf1 prems1 other-sbfs1 new-allvs1 new-exvs1)
		  (sbf~subformula (second args) T test-func sbf-sign all-vars ex-vars)
		(if sbf1
		    (values sbf1 (cons (first args) prems1) other-sbfs1 new-allvs1 new-exvs1)
		  (multiple-value-bind (sbf2 prems2 other-sbfs2 new-allvs2 new-exvs2)
		      (sbf~subformula (first args) NIL test-func sbf-sign all-vars ex-vars)
		    (when sbf2
		      (values sbf2
			      (cons (term~appl-create (logic~negation-constant :name 'not)
						      (rest args))
				    prems2)
			      other-sbfs2 new-allvs2 new-exvs2)))))))
	;;; not (A imp B) is: A and not(B)
	(let ((args (data~appl-arguments assertion)))
	  (if (funcall test-func (first args))
	      (when sbf-sign
		(values (first args) NIL
			(list (term~appl-create (logic~negation-constant :name 'not)
						(rest args)))
			all-vars ex-vars))
	    (if (funcall test-func (second args))
		(unless sbf-sign
		  (values (term~appl-create (logic~negation-constant :name 'not)
					    (rest args))
			  NIL (butlast args) all-vars ex-vars))
	      (multiple-value-bind (sbf1 prems1 other-sbfs1 new-allvs1 new-exvs1)
		  (sbf~subformula (first args) T test-func sbf-sign all-vars ex-vars)
		(if sbf1
		    (values sbf1 prems1
			    (cons (term~appl-create (logic~negation-constant :name 'not)
						    (rest args))
				  other-sbfs1)
			    new-allvs1 new-exvs1)
		  (multiple-value-bind (sbf2 prems2 other-sbfs2 new-allvs2 new-exvs2)
		      (sbf~subformula (second args) NIL test-func sbf-sign all-vars ex-vars)
		    (when sbf2
		      (values sbf2 prems2
			      (cons (term~appl-create (logic~negation-constant :name 'not)
						      (butlast args))
				    other-sbfs2)
			      new-allvs2 new-exvs2))))))))))
     ((logic~equivalence-p assertion)
      (if ass-sign
	  ;;; A eqv B:
	  (let ((args (data~appl-arguments assertion)))
	    (if (funcall test-func (first args))
		(if sbf-sign
		    (values (first args) (rest args) NIL all-vars ex-vars)
		  (values (term~appl-create (logic~negation-constant :name 'not)
					    (butlast args))
			  (list (term~appl-create (logic~negation-constant :name 'not)
						  (rest args)))
			  NIL all-vars ex-vars))
	      (if (funcall test-func (second args))
		  (if sbf-sign
		      (values (second args) (butlast args) NIL all-vars ex-vars)
		    (values (term~appl-create (logic~negation-constant :name 'not)
					      (rest args))
			    (list (term~appl-create (logic~negation-constant :name 'not)
						    (butlast args)))
			    NIL all-vars ex-vars))
		(multiple-value-bind (sbf1 prems1 other-sbfs1 allvs1 exvs1)
		    (sbf~subformula (first args) T test-func sbf-sign all-vars ex-vars)
		  (if sbf1
		      (values sbf1 (cons (second args) prems1) other-sbfs1 allvs1 exvs1)
		    (multiple-value-bind (sbf2 prems2 other-sbfs2 allvs2 exvs2)
			(sbf~subformula (second args) T test-func sbf-sign all-vars ex-vars)
		      (if sbf2
			  (values sbf2 (cons (first args) prems2) other-sbfs2 allvs2 exvs2)
			(multiple-value-bind (sbf3 prems3 other-sbfs3 allvs3 exvs3)
			    (sbf~subformula (first args) NIL test-func sbf-sign all-vars ex-vars)
			  (if sbf3
			      (values sbf3
				      (cons (term~appl-create (logic~negation-constant :name 'not)
							      (rest args))
					    prems3)
				      other-sbfs3 allvs3 exvs3)
			    (multiple-value-bind (sbf4 prems4 other-sbfs4 allvs4 exvs4)
				(sbf~subformula (second args) NIL test-func sbf-sign all-vars ex-vars)
			      (when sbf4
				(values sbf4
					(cons (term~appl-create (logic~negation-constant :name 'not)
								(butlast args))
					      prems4)
					other-sbfs4 allvs4 exvs4))))))))))))
	;;; not(A eqv B) is: (A and not(B)) or (not(A) and B)
	(let ((args (data~appl-arguments assertion)))
	  (if (funcall test-func (first args))
	      (if sbf-sign
		  (values (first args) (list (term~appl-create (logic~implication-constant :name 'implies)
							       (append (rest args) (butlast args))))
			  (list (term~appl-create (logic~negation-constant :name 'not)
						  (rest args)))
			  all-vars ex-vars)
		(values (term~appl-create (logic~negation-constant :name 'not)
					  (butlast args))
			(list (term~appl-create (logic~implication-constant :name 'implies)
						args))
			(rest args) all-vars ex-vars))
	    (if (funcall test-func (second args))
		(if sbf-sign
		    (values (second args)
			    (list (term~appl-create (logic~implication-constant :name 'implies)
						    args))
			    (list (term~appl-create (logic~negation-constant :name 'not)
						    (butlast args)))
			    all-vars ex-vars)
		  (values (term~appl-create (logic~negation-constant :name 'not)
					    (rest args))
			  (list (term~appl-create (logic~implication-constant :name 'implies)
						  (append (rest args) (butlast args))))
			  (butlast args) all-vars ex-vars))
	      (multiple-value-bind (sbf1 prems1 other-sbfs1 allvs1 exvs1)
		  (sbf~subformula (first args) T test-func sbf-sign all-vars ex-vars)
		(if sbf1
		    (values sbf1 (cons (term~appl-create (logic~implication-constant :name 'implies)
							 (append (rest args) (butlast args)))
				       prems1)
			    (cons (term~appl-create (logic~negation-constant :name 'not)
						    (rest args))
				  other-sbfs1)
			    allvs1 exvs1)
		  (multiple-value-bind (sbf2 prems2 other-sbfs2 allvs2 exvs2)
		      (sbf~subformula (first args) NIL test-func sbf-sign all-vars ex-vars)
		    (if sbf2
			(values sbf2
				(cons (term~appl-create (logic~implication-constant :name 'implies)
							args)
				      prems2)
				(cons (second args) other-sbfs2) allvs2 exvs2)
		      (multiple-value-bind (sbf3 prems3 other-sbfs3 allvs3 exvs3)
			  (sbf~subformula (second args) T test-func sbf-sign all-vars ex-vars)
			(if sbf3
			    (values sbf3
				    (cons (term~appl-create (logic~implication-constant :name 'implies)
							    args)
					  prems3)
				    (cons (term~appl-create (logic~negation-constant :name 'not)
							    (butlast args))
					  other-sbfs3)
				    allvs3 exvs3)
			  (multiple-value-bind (sbf4 prems4 other-sbfs4 allvs4 exvs4)
			      (sbf~subformula (second args) NIL test-func sbf-sign all-vars ex-vars)
			    (when sbf4
			      (values sbf4
				      (cons (term~appl-create (logic~implication-constant :name 'implies)
							      (append (rest args) (butlast args)))
					    prems4)
				      (cons (first args) other-sbfs4) allvs4 exvs4))))))))))))))
     (T
      (when (and (null ass-sign) (null sbf-sign)
		 (funcall test-func assertion))
	(values (term~appl-create (logic~negation-constant :name 'not)
				  (list assertion))
		NIL NIL all-vars ex-vars)))
     )))


	   
  






