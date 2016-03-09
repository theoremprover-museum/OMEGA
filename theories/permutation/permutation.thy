;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(th~deftheory permutation
              (uses group integer list)
	      (help "Theory of permutations"))

;; lists of type cyc

(th~deftype cyc
	    (in permutation)
	    (arguments 0)
	    (help "A simple type of permutations"))

(th~defconstant cnil
		(in permutation)
		(type  cyc)
		(help "The empty cycle."))

(th~defaxiom last-base-cyc
	     (in permutation)
	     (formula
	      (forall (lam (a num) (= (last (cons a cnil)) a))))
	     (help "Last element of a cycle."))

(th~defaxiom last-step-cyc
	     (in permutation)
	     (formula
	      (forall-sort (lam (L cyc)
				(forall (lam (a num)
					     (= (last (cons a L)) (last L)))))
			   (lam (x cyc)(not (= x cnil)))))							  
	     (help "Last element of a cycle."))

(th~defaxiom length-base-cyc
	     (in permutation)
	     (formula
	      (= (length cnil) 0))
	     (help "Length of the empty cycle is 0."))

(th~defaxiom length-step-cyc
	     (in permutation)
	     (formula
	      (all-types aa 
			 (forall-sort (lam (L cyc)
					   (forall (lam (a aa)
							(= (length (cons a L)) (plus (length L) 1)))))
				      (lam (x cyc)(not (= x cnil))))))							  
	     (help "Length of cycle (a L) = 1 + Length of cycle L."))

;; defi of cycle

(th~defconstant cycle-set   ;; replace by recursive defi
		(in permutation)
		(type ((o num) cyc))
		(help "The set of elements of a cycle"))


(th~defconstant cycle   ;; replace by recursive defi
		(in permutation)
		(type (o cyc))
		(help "Predicate for cycles."))

(th~deftheorem cycle-base
	       (in permutation)
	       (conclusion
		(forall (lam (a num) (cycle (cons a cnil)))))
	       (help "Cycle means a list of different elements."))

(th~deftheorem cycle-step
	       (in permutation)
	       (conclusion
		(forall-sort (lam (p cyc)
				  (forall (lam (a num)
					       (equiv (cycle (cons a p))
						      (and (not (in a (cycle-set p)))
							   (cycle p))))))
			     (lam (x cyc)(not (= x cnil)))))
	       (help "Cycle means a list of different elements."))

;; application of a cycle

(th~defconstant perm-apply
		(in permutation)
		(type (all-types aa (num num aa)))
		(help "The application of permutations."))

(th~deftheorem apply-cycle-first
	       (in permutation)
	       (conclusion
		(forall (lam (a num)
			     (forall-sort (lam (L cyc)
					       (implies (= (last L) a)
							(= (perm-apply L a) (first L))))
					  cycle))))
	       (help "Application of permutation: (a ... b) @ b = a"))
		 
(th~deftheorem apply-cycle-base
	       (in permutation)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall-sort (lam (L cyc)
							    (= (perm-apply (cons a (cons b L)) a) b))
						       (lam (x cyc)
							    (cycle (cons a (cons b x))))))))))
	       (help "Application of permutation: (a b ...) @ a = b"))
		 
(th~deftheorem apply-cycle-step
	       (in permutation)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall-sort (lam (L cyc)
							    (implies (and (not (= (last L) a))
									  (not (= b a)))
								     (= (perm-apply (cons b L) a) (perm-apply L a))))
						       cycle))))))
	       (help "Application of permutation: (b L) @ a = (L) @ a"))

(th~deftheorem apply-cycle-id
	       (in permutation)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (not (= a b))
						   (= (perm-apply (cons a cnil) b)
						      b)))))))
	       (help "Application of permutation: (a) @ b = b"))

(th~defconstant identity-cyc
		(in permutation)
		(type cyc)
		(help "Identity cycle."))

(th~defaxiom apply-identity
	     (in permutation)
	     (formula
	      (forall (lam (a num)
			   (= (perm-apply identity-cyc a) a))))
	     (help "Applying the identity permutation: (1) @ a = a."))


(th~deftheorem cycle-set-id
	       (in permutation)
	       (conclusion
		(= (cycle-set identity-cyc) emptyset))
	       (help "Set of elements of a cycle: cycle-set()  = {}"))

(th~deftheorem cycle-set-base
	       (in permutation)
	       (conclusion
		(forall (lam (a num) (= (cycle-set (cons a cnil)) (singleton a)))))
	       (help "Set of elements of a cycle: cycle-set(a)  = {a}"))

(th~deftheorem cycle-set-step
	     (in permutation)
	     (conclusion
	      (forall (lam (p cyc)
			   (forall (lam (a num)
					(= (cycle-set (cons a p))
					   (union (singleton a) (cycle-set p))))))))
	     (help "Set of elements of a cycle: cycle-set(a p)  = {a}Ucycle-set(p)"))

;; permutations as lists of disjoint cycles

(th~defdef fixes
	   (in permutation)
	   (definition
	     (lam (P cyc)
		  (lam (a num)
		       (= (perm-apply P a) a))))
	   (help "Permutation P fixes element a, if (P a) = a."))

;;;; The original version does not work in the proof. One cannot show anything concrete with it!!!
#+original(th~defdef cycle-disjoint    
	   (in permutation)
	   (definition
	     (lam (P cyc)
		  (lam (Q cyc)
		       (and (forall (lam (x num) (implies (not (fixes P x)) (fixes Q x))))
			    (forall (lam (y num) (implies (not (fixes Q y)) (fixes P y))))))))
	   (help "Two permutations P and Q are disjoint if all elements permutated by P are fixed by Q and vice versa."))

;;; This is a slightly weakened version of cycle-disjoint,
;;; talking about the actual sets of elements that are not fixed.
(th~defdef cycle-disjoint    
	   (in permutation)
	   (definition
	     (lam (P cyc)
		  (lam (Q cyc)
		       (and (forall-sort (lam (x num) (and (not (fixes P x)) (fixes Q x))) (cycle-set P))
			    (forall-sort (lam (y num) (and (not (fixes Q y)) (fixes P y))) (cycle-set Q))))))
	   (help "Two permutations P and Q are disjoint if all elements permutated by P are fixed by Q and vice versa."))


(th~defdef permutation 
	   (in permutation)
	   (definition
	     (lam (P (o cyc))
		  (forall-sort (lam (x cyc)
				    (forall-sort (lam (y cyc)
						      (implies (not (= x y))
							       (and (and (cycle x)  
									 (cycle y)) 
								    (cycle-disjoint x y))))
						 P))
			       P)))
	   (help "A permutation is a nonempty set of disjoint cycles."))

(th~defdef identity-perm
	   (in permutation)
	   (definition
	     ;;	     emptyset)
	     (lam (x cyc) (and false (not false))))
	   (help "The identity permutation is the empty set of cycles."))


;; application of permutations

(th~deftheorem apply-perm-base
	     (in permutation)
	     (conclusion
	      (forall (lam (a num)
			   (= (perm-apply ;;;;emptyset a) a))))
			       identity-perm a) a))))
	     (help "Application of permutation: {(...)} @ a = (...) @ a"))
		 
(th~deftheorem apply-perm-step
	     (in permutation)
	     (conclusion
	      (forall (lam (a num)
			   (forall-sort (lam (P (o cyc))
					     (exists-sort (lam (C cyc)
							       (= (perm-apply P a)
								  (perm-apply (setminus P (singleton C)) (perm-apply C a))))
							  P))
					permutation))))
	     (help "Application of permutation: {( ) ...} @ a = {...} @ ( ) @ b"))


;; Equality on cycles and permutations

(th~defaxiom =cycle
	     (in permutation)
	     (formula
	      (forall-sort (lam (p cyc)
				(forall-sort (lam (q cyc)
						  (implies (= (perm-apply p) (perm-apply q))
							   (= p q)))
					     cycle))
			   cycle))
	     (help "Cycles are identified, if they induce the same mapping."))

(th~defaxiom =permutation
	     (in permutation)
	     (formula
	      (forall-sort (lam (p (o cyc))
				(forall-sort (lam (q (o cyc))
						  (implies (= (perm-apply p) (perm-apply q))
							   (= p q)))
					     permutation))
			  permutation))
	     (help "Permutations are identified, if they induce the same mapping."))

;; Operations on permutations

(th~defconstant perm-compose
		(in permutation)
		(type ((o cyc)(o cyc)(o cyc)))
		(help "Composition operator for permutations."))

(th~deftheorem apply-permcompose
	     (in permutation)
	     (conclusion
	      (forall-sort (lam (P (o cyc))
				(forall-sort (lam (Q (o cyc))
						  (= (perm-apply (perm-compose P Q))
						     (compose-functions (perm-apply P)
									(perm-apply Q))))
							  permutation))
			   permutation))
	     (help "Application of permutation: { }o{ }@ = { }@ o { }@"))


(th~deftheorem cycle-compose-disjoint-perms
	       (in permutation)
	       (conclusion
		(forall-sort (lam (p (o cyc))
				  (forall-sort (lam (q (o cyc))
						    (implies
						     (forall-sort (lam (s cyc)
								       (forall-sort (lam (t cyc)
											 (cycle-disjoint s t))
										    P))
								  Q)
						     (= (perm-compose P Q) (union P Q))))
					       permutation))
			     permutation))
	       (help "The cycles of two disjoint permutations can be composed to the union of the union of the permutations."))

(th~defconstant perm-exp ;;replace by recursive-defi
		(in permutation)
		(type ((o cyc) num (o cyc)))
		(help "Exponentiation operator for permutations."))

(th~deftheorem perm-exp-base
	     (in permutation)
	     (conclusion
	      (forall-sort (lam (p (o cyc))
			   (= (perm-exp p zero) identity-perm))
			   permutation))
	     (help "The base case for exponentation of permutations: p^0 = i."))


(th~deftheorem perm-exp-step
	     (in permutation)
	     (conclusion
	      (forall-sort (lam (p (o cyc))
			   (forall-sort (lam (n num)
					     (= (perm-exp p (s n)) (perm-compose p (perm-exp p n))))
					nat))
			   permutation))
	     (help "The step case for exponentation of permutations: p^sn = p*p^n"))


;; negative exponent

(th~defdef perm-inverse
	   (in permutation)
	   (definition
	     (lam (p (o cyc))
		  (that (lam (q (o cyc))
			     (and (and (permutation q)(permutation p))
				  (= (perm-compose p q) identity-perm))))))
	   (help "Definition of inverse permutation."))

(th~defaxiom perm-exp-neg-base
	     (in permutation)
	     (formula
	      (forall-sort (lam (q (o cyc))
				(= (perm-exp q (p zero)) (perm-inverse q)))
			   permutation))
	     (help "The negative base case for exponentation of permutations: inverse of p is p^-1"))

(th~defaxiom perm-exp-neg-step
	     (in permutation)
	     (formula
	      (forall-sort (lam (q (o cyc))
			   (forall-sort (lam (n num)
					     (= (perm-exp q (p n )) (perm-compose (perm-inverse q) (perm-exp q n ))))
					neg-nnat))
			   permutation))
	     (help "The step case for exponentation of permutations: p^-pn = p*p^-n"))


;; generated set

;;(th~defdef generated-set
;;           (in permutation)
;;           (definition
;;             (lam (PS (o cyc))
;;                  (lam (P cyc)
;;                       (exists-sort (lam (P1 cyc)
;;                                         (exists-sort (lam (P2 cyc)
;;                                                           (= P (perm-compose P1 P2)))
;;                                                      PS))
;;                                    PS))))
;;           (help "A set of permutations generated by a generating set of permutations."))


(th~defconstant generated-set
		(in permutation)
		(type ((o (o cyc)) (o (o cyc))))
		(help "A generated set of permutations."))

(th~defaxiom generated-set-base
	     (in permutation)
	     (formula
	      (forall (lam (PS (o (o cyc)))
			   (forall-sort (lam (P (o cyc))
					     (and (permutation P)
						  (in P (generated-set PS))))
					PS))))
	     (help "PS is a subset of generated-set(PS)."))

(th~defaxiom generated-set-step
	     (in permutation)
	     (formula
	      (forall (lam (PS (o (o cyc)))
			   (forall-sort (lam (P1 (o cyc))
					     (forall-sort (lam (P2 (o cyc))
							       (in (perm-compose P1 P2) (generated-set PS)))
							  (generated-set PS)))
					(generated-set PS)))))
	     (help "generated-set(PS) is closed under perm-compose."))



;; stabiliser base 


(th~defconstant stabiliser-list
		(in permutation)
		(type  (all-types aa bb ((o aa ) list (bb bb aa) (o aa ))))
		(help "The empty list."))

(th~defaxiom stabiliser-list-base
	     (in permutation)
	     (formula
	      (all-types aa bb
			 (forall (lam (gset (o aa))
			 (forall (lam (gaction (bb bb aa ))
				      (= (stabiliser-list gset gaction nil)
					 gset)))))))
	     (help "Recursive definition of stabiliser-list"))

(th~defaxiom stabiliser-list-step
	     (in permutation)
	     (formula
	      (all-types aa bb
			 (forall (lam (gset (o aa))
			 (forall (lam (gaction (bb bb aa ))
			 (forall (lam (elems list)
			 (forall (lam (elem bb)
				      (= (stabiliser-list gset gaction (cons elem elems))
					 (stabiliser (stabiliser-list gset gaction elems) gaction elem))))))))))))
	     (help "Recursive definition of stabiliser-list"))

(th~defdef perm-stab-base
	   (in permutation)
	   (type-variables aa bb)
	   (definition
	     (lam (gset (o (o cyc)))
	     (lam (blist list)
			    (= (stabiliser-list gset perm-apply blist) (set identity-perm)))))
	   (help "A list of elements is a base if the stabiliser for all elements of the list is the trivial group."))
