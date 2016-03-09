;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(th~deftheory RELATION
              (uses typed-set)
	      (help "Properties of binary Relations."))


(th~defdef relation-like
	   (in relation)
           (type-variables aa bb)
           (definition (lam (R (o (tuple aa bb))) 
			    (lam (U (o aa))
				 (lam (V (o bb))
				      (subset R (cartesian-product U V)))))))


(th~defdef transitive
	   (in relation)
           (type-variables bb)
	   (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (forall (lam (z bb)
 			    (implies (and (rel x y) (rel y z)) (rel x z))))))))))
	   (help "Definition of the predicate for transitivity. (transitive R) is true, iff Rxy and Ryz imply Rxz."))

(th~defdef transitivity
	   (in relation)
           (type-variables bb)
	   (definition
	     (lam (g (o bb))
		   (lam (rel (o bb bb))
			(forall-sort (lam (x bb)
				     (forall-sort (lam (y bb)
                                            (forall-sort (lam (z bb)
							      (implies (and (rel x y) (rel y z)) (rel x z))) g)) g)) g))))
	   (help "Definition of the predicate for transitivity. (transitive R) is true, iff Rxy and Ryz imply Rxz."))


(th~defdef sub-relation
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rela (o bb bb))
                  (lam (relb (o bb bb))
                       (forall (lam (x bb)
                                    (forall (lam (y bb)
                                                 (implies (rela x y)
                                                          (relb x y)))))))))
	   (help "Definition of the predicate for sub-relations.
                  (sub-relation R R') is true, iff Rxy implies R'xy."))

(th~defdef relation-union
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rela (o bb bb))
                  (lam (relb (o bb bb))
                       (lam (x bb)
                            (lam (y bb)
                                 (or (rela x y) (relb x y)))))))
	   (help "The union of two relations."))

(th~defdef relation-intersection
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rela (o bb bb))
                  (lam (relb (o bb bb))
                       (lam (x bb)
                            (lam (y bb)
                                 (and (rela x y) (relb x y)))))))
	   (help "The intersection of two relations."))

(th~defdef symmetric
	   (in relation)
           (type-variables bb)
           (definition
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (implies (rel x y)
                                                     (rel y x))))))))
	   (help "Definition of the predicate for symmetric relations.
                  (symmetric R) is true, iff Rxy implies Ryx."))


(th~defdef symm-closure
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (lam (x bb)
                       (lam (y bb)
                            (or (rel x y) (rel y x))))))
	   (help "Definition of the symmetric closure function for relations
                  (symmetrization R) is the relation S, such that Sxy iff Rxy or Ryx."))


(th~defdef trans-closure
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (lam (x bb)
                       (lam (y bb)
                            (or (rel x y)
                                (exists (lam (z bb)
                                             (and (rel x z) (rel z y)))))))))
	   (help "Definition of the transitive closure function for relations
                  (transitive-closure R) is the relation S, such that Sxy iff Rxy or
                  there is z such that Rxz and Rzy."))



(th~defdef anti-symmetric
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (implies (rel x y)
                                                     (not (rel y x)))))))))
	   (help "Definition of the predicate for anti-symmetric relations.
                  (symmetric R) is true, iff Rxy implies  that not Ryx."))


(th~defdef reflexive
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (rel x x)))))
; changed; DEF
;                          (implies (exists (lam (y bb) (rel x y)))
;                                   (rel x x)))))))
	   (help "Definition of the predicate for reflexive relations.
                  (reflexive R) is true, iff Rxy for some y implies Rxx."))

(th~defdef irreflexive
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (not (exists (lam (y bb) (rel x x))))))))
	   (help "Definition of the predicate for reflexive relations.
                  (reflexive R) is true, iff for all (not Rxx)."))


(th~defdef eqrel
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (and (and (reflexive rel)
                            (symmetric rel))
                       (transitive rel))))
	   (help "Definition of the predicate for equivalence relations."))

(th~defdef rel-domain
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (lam (x bb)
                       (exists (lam (y bb) (rel x y))))))
	   (help "Definition of the domain function relations.
                  (rel-domain R) is the set of x such that Rxy for some y."))



(th~defdef rel-codomain
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (lam (y bb)
                       (exists (lam (x bb) (rel x y))))))
	   (help "Definition of the codomain function relations.
                  (rel-domain R) is the set of y such that Rxy for some x."))




(th~defdef rel-converse
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (lam (x bb)
                       (lam (y bb)
                            (rel y x)))))
	   (help "Definition of the converse relations
                  (rel-converse R) is the set of y such that Rxy for some x."))

(th~defdef rel-composition
	   (in relation)
           (type-variables bb)
           (definition
             (lam (rela (o bb bb))
                  (lam (relb (o bb bb))
                       (lam (x bb)
                            (lam (y bb)
                                 (exists (lam (z bb)
                                              (and (rela x z) (relb z y)))))))))
(help "Definition of the composition of two relations
                  (rel-composition R T) is the relation C,
                  such that Cxy if Rxz and Tzy for some z."))

(th~defdef rel-total
	   (in relation)
           (type-variables bb)
           (definition
             (lam (rel (o bb bb))
                  (lam (G (o bb))
                       (forall (lam (x bb)
                                    (exists (lam (y bb) (rel x y))))))))
	   (help "Definition of the predicate for totality on the first argument
                  for binary relations.
                  (rel-total R G) is true, iff for all x in G Rxy for some y."))



(th~defdef eq-class
	   (in relation)
	   (type-variables bb)
           (definition
             (lam (x bb)
                  (lam (eq (o bb bb))
                       (lam (G (o bb))
                            (lam (y bb)
                                 (and (in y G) (eq x y)))))))
	   (help "The definition of an equivalence class of a an object.
                  (eq-class x R G) is the set of all y, such that y in G and xRy."))

(th~defdef quotient-set
           (in relation)
           (type-variables bb)
           (definition
             (lam (G (o bb))
                  (lam (eq (o bb bb))
                       (lam (z (o bb))
                            (exists (lam (w bb)
                                         (and (in w G)
                                              (= z (eq-class w eq G)))))))))
           (help "The definition of quotient set wrt. an equivalence relation.
                  (quotient-set G eq) is the quotient set of G over eq."))





;;; new by chris at 29.9.98

(th~defdef partition
           (in relation)
           (type-variables bb)
           (definition
             (lam (part (O (O bb)))
		  (and
		   (forall
		    (lam (set1 (O bb))
			 (implies
			  (part set1)
			  (exists (lam (Z bb) (set1 Z))))))
		   (forall
		    (lam (elem1 bb)
			 (exists
			  (lam (set2 (O bb))
			       (and (part set2)
				    (and (set2 elem1)
					 (forall
					  (lam (set3 (O bb))
					       (implies (and (Part set3)
							     (set3 elem1))
							(= set3 set2)))))))))))))
										  
           (help "The partition predicate."))


(th~defdef equivalence-classes-2
           (in relation)
           (type-variables bb)
           (definition
             (lam (rel1 (O bb bb))
		  (lam (set1 (O bb))
		       (exists (lam (elem1 bb)
				    (and (set1 elem1)
					 (forall (lam (elem2 bb)
						      (implies (set1 elem2)
							       (forall (lam (elem3 bb)
									    (equiv (set1 elem3)
										   (rel1 elem2 elem3)))))))))))))
	   (help "The equivalence-classes relation. Input: a relation. Result: The set of
all equivalence classes with respect to the input relation"))


(th~defdef equivalence-classes
           (in relation)
           (type-variables bb)
           (definition
             (lam (rel1 (O bb bb))
		  (lam (set1 (O bb))
		       (exists (lam (elem1 bb)
				    (and (set1 elem1)
					 (forall (lam (elem2 bb)
						      (equiv (set1 elem2)
							     (rel1 elem1 elem2))))))))))
	   (help "The equivalence-classes relation. Input: a relation. Result: The set of
all equivalence classes with respect to the input relation"))
			 
(th~defdef transitive-closure
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel1 (o bb bb))
                  (lam (elem1 bb)
                       (lam (elem2 bb)
                            (forall (lam (rel2 (o bb bb))
					 (implies (and (sub-relation rel1 rel2)
						       (transitive rel2))
						  (rel2 elem1 elem2))))))))
	   (help "Definition of the transitive closure as in TPS."))






