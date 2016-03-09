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

(th~deftheory poset
              (uses struct relation)
	      (help "Partially ordered Sets"))

#| Partially ordered sets are structures consisting of a set and an ordering relation.
   A pre-ordering is a binary relation that is transitive and reflexive.
   A strict ordering is a binary relation that is transitive and irreflexive.
   A partial ordering is transitive and trichomic |#

(th~defdef trichotomic
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (forall (lam (x bb)
			       (forall (lam (y bb)
					    (or (= x y)
						(or (rel x y) (rel y x)))))))))
	   (help "Definition of the predicate for trichotomy of relations
                  (trichotomic R) is true, iff Rxy or  Ryx or x=y."))

(th~defdef trichotomy
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (g (o bb))
		  (lam (rel (o bb bb))
		       (forall-sort (lam (x bb)
				    (forall-sort (lam (y bb)
						 (or (= x y)
						     (or (rel x y) (rel y x)))) g)) g))))
	   (help "Definition of the predicate for trichotomy of relations
                  (trichotomic R) is true, iff Rxy or  Ryx or x=y."))



(th~defdef pre-ordering
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (and (transitive rel)
		       (reflexive rel))))
	   (help "Definition of the pre-order predicate for relations
                  (pre-order R) is true, iff R is transitive and reflexive."))


(th~defdef strict-ordering
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (and (transitive rel)
		       (irreflexive rel))))
	   (help "Definition of the pre-order predicate for relations
                  (pre-order R) is true, iff R is transitive and irreflexive."))


(th~defdef partial-ordering
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (and (pre-ordering rel)
		       (trichotomic rel))))
	   (help "Definition of the predicate for partial ordering relations."))


(th~defdef strict-partial-ordering
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (and (strict-ordering rel)
		       (trichotomic rel))))
	   (help "Definition of the predicate for strict partial ordering relations."))

(th~defdef poset
	   (in poset)
	   (type-variables aa)
	   (definition
	     (lam (S (struct aa))
		  (partial-ordering (struct-ordering S))))
	   (help "The predicate for partially ordered sets."))


(th~defdef poset-strict-ordering
	   (in poset)
	   (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (x bb)
		       (lam (y bb)
			    (and (struct-ordering S x y)
				 (not (= x y)))))))
	   (help "The induced strict ordering of a poset."))

(th~defdef monotonic
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (lam (func (bb bb))
		       (lam (G (o bb))
			    (forall (lam (x bb)
					 (forall (lam (y bb)
						      (implies (and (and (in x G) (in y G))
								    (rel x y))
							       (rel (func x) (func y)))))))))))
	   (help "Definition of the predicate for monotonicity of a function on a set.
                  (monotonic R f G) is true, iff for all x,y in G Rxy implies R(fx)(fy)."))

(th~defdef monotone
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (rel (o bb bb))
		  (lam (op (bb bb bb))
		       (lam (G (o bb))
			    (forall-sort (lam (x bb)
 			    (forall-sort (lam (y bb)
			    (forall-sort (lam (z bb)
					      (implies (rel x y)
						       (rel (op z x) (op z y))))g )) g)) g)))))
	   (help "Definition of the predicate for monotonicity of a function on a set.
                  (monotone R f G) is true, iff for all x,y,z in G Rxy implies R(op z x)(op z y)."))

(th~defdef upper-bound
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (b bb)
		       (lam (G (o bb))
			    (forall (lam (x bb)
					 (implies (in x G)
						  (struct-ordering S x b))))))))
	   (help "In ordered structure S b is an upper bound of the set G
                  if (upper-bound S b G) is true."))

(th~defdef lower-bound
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (b bb)
		       (lam (G (o bb))
			    (forall (lam (x bb)
					 (implies (in x G)
						  (struct-ordering S b x))))))))
	   (help "In ordered structure S b is an upper bound of the set G
                  if (upper-bound S b G) is true."))

(th~defdef bounded-above
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (exists (lam (b bb)
				    (upper-bound S b G))))))
	   (help "A set is bounded above, iff there is an upper bound for it."))

(th~defdef bounded-below
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (exists (lam (b bb)
				    (lower-bound S b G))))))
	   (help "A set is bounded above, iff there is a lower bound for it."))

(th~defdef bounded
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (and (bounded-above S G)
			    (bounded-below S G)))))
	   (help "A set is bounded above, iff there are upper and 
                  lower bounds for it."))


(th~defdef supremum
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (that (lam (b bb)
				  (and (upper-bound S b G)
				       (forall (lam (x bb)
						    (implies (upper-bound S x G)
							     (struct-ordering S b x))))))))))
	   (help "The least upper bound of a set."))


(th~defdef infimum
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (that (lam (b bb)
				  (and (lower-bound S b G)
				       (forall (lam (x bb)
						    (implies (lower-bound S x G)
							     (struct-ordering S x b))))))))))
	   (help "The greatest lower bound of a set."))


(th~defdef maximum
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (that (lam (b bb)
				  (and (= b (supremum S G))
				       (in b (struct-set S))))))))
	   (help "The greatest lower bound of a set."))


(th~defdef minimum
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (lam (G (o bb))
		       (that (lam (b bb)
				  (and (= b (infimum S G))
				       (in b (struct-set S))))))))
	   (help "The greatest lower bound of a set."))

(th~defdef complete-poset
	   (in poset)
           (type-variables bb)
	   (definition
	     (lam (S (struct bb))
		  (and (poset S)
		       (forall (lam (G (o bb))
				    (implies (bounded-above S G)
					     (exists (lam (x bb)
							  (and (in x (struct-set S))
							       (= x (supremum S G)))))))))))
	   (help "A complete poset is a structure, 
                  where all bounded sets have suprema and infima."))
