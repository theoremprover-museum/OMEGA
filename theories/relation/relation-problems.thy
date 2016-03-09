;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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
(in-package :omega)


(th~defproblem thm136
	 (in relation)
	 (type-constants bb)
	 (conclusion
	  (forall (lam (R (O bb bb))
		       (transitive (transitive-closure R))))))




(th~defproblem thm262-sub1
	 (in relation)
	 (type-constants a)
	 (constants (p (O (O a))))
	 (conclusion
	  (=
	   (equivalence-classes
	    (lam (X a)
		 (lam (Y a)
		      (exists (lam (S (O a))
				   (and (p S)
					(and (S X)
					     (S Y))))))))
	   p)))



(th~defproblem thm262-sub2
	 (in relation)
	 (type-constants a)
	 (constants (p (O (O a))) (b (O a)))
	 (conclusion
	  (implies
	   (equivalence-classes
	     (lam (X a)
		  (lam (Y a)
		       (exists (lam (S (O a))
				    (and (p S)
					 (and (S X)
					      (S Y)))))))
	     b)
           (p b))))


(th~defproblem thm262
	 (in relation)
	 (type-constants a)
	 (conclusion
	  (forall (lam (P (O (O a)))
		       (implies
			(partition P)
			(exists (lam (Q ((O a) a))
				     (and (eqrel Q)
					  (= (equivalence-classes Q)
					     P)))))))))



(th~defproblem thm262-inst-1
	 (in relation)
	 (type-constants a)
	 (constants (Q ((O a) a)))
	 (conclusion
	  (forall (lam (P (O (O a)))
		       (implies
			(and (partition P)
			     (= Q (lam (x a)
				       (lam (y a)
					    (exists (lam (R (o a)) (and (in R P)
									(and (in x R) (in y R)))))))))
			(and (eqrel Q)
			     (= (equivalence-classes Q)
				P)))))))


(th~defproblem thm262-inst-2
	 (in relation)
	 (type-constants a)
	 (constants (Q ((O a) a)))
	 (conclusion
	  (forall (lam (P (O (O a)))
		       (implies
			(partition P)
			(and (eqrel (lam (x a)
				       (lam (y a)
					    (exists (lam (R (o a)) (and (in R P)
									(and (in x R) (in y R))))))))
			     (= (equivalence-classes (lam (x a)
				       (lam (y a)
					    (exists (lam (R (o a)) (and (in R P)
									(and (in x R) (in y R))))))))
				P)))))))



(th~defproblem thm262-lemma
        (in relation)
	(type-constants a)
	(conclusion
	(forall (lam (r ((O a) a))
		     (implies (eqrel r)
			      (forall (lam (c a)
					   (forall (lam (b a)
							(equiv (r c b)
							       (exists (lam (x (O a))
									    (and ((equivalence-classes r) x)
										 (and (x c) (x b))))))))))))))
			)				


(th~defproblem converse (in relation)
	 (type-constants bb)
	 (constants
	  (Rho (o bb bb)))

	 (assumption symm-rho (symmetric Rho))

	 (conclusion thm (symmetric (rel-converse Rho)))
	 )


;;;; problem by Chris & Volker

(th~defproblem synthesize-function (in base)
	       (constants
		(b0 i)
		(b1 i)
		(b2 i)
		(+ (o i i i))
		(* (o i i i)))
	       (assumption plusb0 (forall (lam (x i) (and (+ b0 x x) (+ x b0 x)))))
	       (assumption plus-rest (forall (lam (x i) (and (+ b1 b1 b2)
							      (and (+ b1 b2 b0)
								   (and (+ b2 b1 b0)
									(+ b2 b2 b1)))))))
	       (assumption multb0 (forall (lam (x i) (and (* b0 x b0) (* x b0 b0)))))
	       (assumption multb1 (forall (lam (x i) (and (* b1 x x) (* x b1 x)))))	 
	       (assumption mult-rest (forall (lam (x i) (* b2 b2 b2))))	 
	       (conclusion thm (exists (lam (op (o i i i))
					   (and (op b0 b0 b0)
						(and (op b1 b0 b2)
						     (and (op b2 b0 b1)
							  (and (op b0 b1 b1)
							       (and (op b1 b1 b0)
								    (and (op b2 b1 b2)
									 (and (op b0 b2 b2)
									      (and (op b1 b2 b1)
										   (op b2 b2 b0)))))))))))))
					  



;;  op b0 b1 b2
;;   b0 b0 b1 b2
;;   b1 b2 b0 b1
;;   b2 b1 b2 b0					  
					  

; symmetric function needs function.thy
;
;(th~defproblem symmetric-rel
;  (in relation)
;  (type-constants aa)
;  (conclusion thm
;   (forall (lam (R (o aa aa))
;    (equiv (symmetric R) (symmetric-function R)))))
;  (help "A relation is symmetric, iff it is a symmetric function."))



(th~defproblem set-770+4
	 (in relation)
	 (type-constants a)
	 (conclusion set-770+4
	  (forall (lam (R (o a a))
		       	  (forall (lam (Q (o a a))
;				       (implies (and (eqrel R)
;						     (eqrel Q))
						(or (= (equivalence-classes R)
						       (equivalence-classes Q))
						    (disjoint (equivalence-classes R)
							      (equivalence-classes Q)))))))
	  ))


;;;; Volker  13 Oct. 2004


(th~defproblem equiv-def-of-equivalence-classes
	       (in relation)
	       (conclusion thm (all-types aa
					  (forall (lam (R (o aa aa))
						       (implies (eqrel R) (= (equivalence-classes R)
									     (equivalence-classes-2 R)))))))
	       (help "Showing the equivalence between two definitions of Equivalence-Classes."))



(th~defproblem set-640+3
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R (o (tuple a b)))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (relation-like R X Y)
								(forall (lam (A (o (tuple a b)))
									     (implies (subset A R)
										      (subset A (cartesian-product X Y))))))))))))))
