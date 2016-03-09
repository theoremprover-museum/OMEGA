;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
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

(th~deftheory group
              (uses monoid loop)
	      (constants (left-coset (all-types aa ((o aa) (aa aa aa) (o aa) (o aa) aa)))
			 (right-coset (all-types aa ((o aa) (aa aa aa) (o aa) (o aa) aa))))
	      (help "Group theory"))


;;;
;;; Definition of GROUP
;;;

(th~defdef group
	   (in group)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (and (not-empty G)
			    (and (closed-under G op)
				 (and (associative G op)
				      (and (exists-sort (lam (e aa) (unit G op e)) G)
					   (inverse-exist G op (struct-unit G op)))))))))
	   (help "Definition of a Group."))

(th~defdef abelian-group
	   (in group)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (and (group G op)
			    (commutative G op)))))
	   (help "Definition of an Abelian Group."))

(th~defdef group-unit 
	   (in group)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (struct-unit G op))))
	   (help "The group-unit denotes the element of the group for which the unit property holds."))

(th~defdef group-inverse
	   (in group)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (elem bb)
			    (that (lam (inv bb)
				       (and (G inv)
					    (and (= (op elem inv) (group-unit g op))
						 (= (op inv elem) (group-unit g op))))))))))
	   (help "(group-inverse G op g) denotes the inverse of g in G."))

(th~defdef  subgroup  
           (in group)
           (type-variables se)
           (definition
             (lam (M (o se))
		  (lam (op (se se se))
             (lam (M1 (o se))
		  (lam (op1 (se se se))
		       (and  (and (= op op1) ;restriction!
				  (subset M M1))
			     (and (group M op)
				  (group M1 op1))))))))
	   (help "S is a subgroup of G if it is a subset of G and a group with respect to the same operation."))

(th~defdef g-action
           (in group)
           (type-variables aa bb)
           (definition
	     (lam (G (o aa))
	     (lam (op (aa aa aa))
	     (lam (action (bb bb aa))
      	     (lam (T (o bb))
		  (and (forall-sort (lam (x bb) (= (action (group-unit g op) x) x)) T)
		       (forall-sort (lam (x bb)
		       (forall-sort (lam (g1 aa)
		       (forall-sort (lam (g2 aa)
					 (= (action (op g1 g2) x)
					    (action g1 (action g2 x))))
				    G))
				    G))
				    T)))))))
	   (help "Group acting on a set."))


(th~defdef g-orbit
           (in group)
           (type-variables aa bb)
           (definition
             (lam (G (o aa))
 	     (lam (action (bb bb aa))
             (lam (elem  bb)
	     (lam (member bb)
		  (exists-sort (lam (gelem aa)
				    (= member (action gelem elem))) G))))))
	   (help "G-Orbit of an element x with respect to a group G is set {gx|g in G}."))

(th~defdef g-orbit-representation
           (in group)
           (type-variables aa bb)
           (definition
             (lam (G (o aa))
		  (lam (action (bb bb aa))
		       (lam (elem bb)
			    (lam (repr (aa bb))
				 (forall-sort (lam (x bb)
						   (= (action (repr x) elem) x))
						   (g-orbit G action elem)))))))
	   (help "A function f is g-orbit-representation, if it maps each member y of the g-orbit of x to the a group-element with g*x=y"))
	   

(th~defdef stabiliser
           (in group)
           (type-variables aa bb)
           (definition
             (lam (G (o aa))
		  (lam (action (bb bb aa))
             (lam (elem  bb)
		  (lam (gelem aa)
		       (and (in gelem G)
			    (= (action gelem elem) elem)))))))
	   (help "Stabiliser of an element x with respect to a group G is set {g in G|gx = x}."))



(th~defaxiom left-coset-axiom
	     (in group)
	     (type-variables aa)
	     (formula
	      (forall (lam (x aa)
		(forall (lam (G (o aa))
		  (forall (lam (A (o aa))
		    (forall (lam (op (aa aa aa))
				 (implies (and (in x G)
					       (and (group G op)
						    (subgroup A op G op)))
					  (= (left-coset x G A op)
					     (lam (y aa)
						  (exists-sort (lam (z aa)
								    (= y (op x z)))
							       A)))))))))))))
	     (help "The Axiom for the definition of left-coset."))


(th~defaxiom right-coset-axiom
	     (in group)
	     (type-variables aa)
	     (formula
	      (forall (lam (x aa)
		(forall (lam (G (o aa))
		  (forall (lam (A (o aa))
		    (forall (lam (op (aa aa aa))
				 (implies (and (in x G)
					       (and (group G op)
						    (subgroup A op G op)))
					  (= (right-coset x G A op)
					     (lam (y aa)
						  (exists-sort (lam (z aa)
								    (= y (op z x)))
							       A)))))))))))))
	     (help "The Axiom for the definition of right coset."))




;(th~defdef group
;           (in group)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                    (and (monoid S)
;                         (forall-sort
;                          (lam (x aa)
;                           (exists-sort
;                            (lam (y aa)
;                                 (= (struct-op S x y) (struct-neut S)))
;                            (struct-set S)))
;                          (struct-set S)))))
;           (help "The group definition. group  is true on a structure S, 
;                  iff S is a group, i.e. a monoid that has a unique 
;                  inverse function."))
;
;(th~defdef abelian-group
;           (in group)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                  (and (abelian-monoid S)
;                       (group S))))
;           (help "The predicate for abelian groups,  
;                  abelian-group  is true for a structure S, 
;                  iff S is a group and moreover its group operation 
;                  is commutative."))
;
;(th~defdef subgroup
;           (in group)
;           (type-variables aa)
;           (definition
;             (lam (S1 (struct aa))
;                    (lam (S2 (struct aa))
;                         (and (and (group S1)
;                                   (group S2))
;                              (subset (struct-set S1) 
;                                      (struct-set S2))))))
;           (help "The definition of the subgroup predicate. subgroup S G 
;                  is true, iff S is a subgroup of G."))
;
;(th~defdef kernel
;           (in group)
;           (definition 
;             (lam (F morphism)
;                  (that (lam (S struct)
;                             (and  (= (struct-set S)
;                                      (lam (z bb)
;                                           (= (morphism-function F z) 
;                                              (ring-zero (morphism-codomain F)))))
;                                   (= (struct-op S) (struct-op (morphism-codomain F))))))))
;           (help "The kernel of a ring homomorphism."))
;
;(th~defproblem kernel-subgroup
;           (in group)
;           (formula 
;            (forall (lam (F morphism)
;                         (subgroup (kernel F) (morphism-codomain F)))))
;           (help "The kernel of a group morhpism is a subgroup of the codomain."))
;

;ring-zero not declared 
;(th~defdef kernel
;           (in group)
;           (type-variables aa bb)
;           (definition 
;             (lam (F (morphism bb aa))
;                  (that (lam (S (struct aa))
;                             (and  (= (struct-set S)
;                                      (lam (z aa)
;                                           (= (morphism-function F z) 
;                                              (ring-zero (morphism-codomain F)))))
;                                   (= (struct-op S) (struct-op (morphism-codomain F))))))))
;           (help "The kernel of a ring homomorphism."))








