;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

(th~deftheory TYPED-SET
	      (uses post)
	      (help "Simply typed sets and their operators."))

(th~deftype tuple
	    (in typed-set)
	    (arguments 2)
	    (help "The type of tuples, used for def of ordered pairs."))

(th~defconstant pair
  (in typed-set)
  (type (all-types aa bb ((tuple aa bb) bb aa)))
  (help "OMEGA constant for local definitions.")
  )

(th~defdef in
	   (in typed-set)
	   (type-variables bb)
           (definition
             (lam (x bb)
                  (lam (U (o bb))
                       (U x))))
	   (help "The element predicate, defined by application."))

;(th~defdef emptyset
;           (in typed-set)
;           (type-variables aa)
;           (definition (lam (x aa) false))
;           (help "The empty set, defined by falsehood."))

(th~defdef emptyset
	   (in typed-set)
	   (type-variables aa)
           (definition (lam (x aa) (and false (not false))))
	   (help "The empty set, defined by contradictory embedded formula, which is interpreted correctly by ext. systems.")) 

(th~defdef empty
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (M (o aa))
                  (forall (lam (x aa)
				    (not (M x))))))
	   (help "predicate, a set being empty."))

(th~defdef non-empty
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (M (o aa))
                  (exists (lam (x aa)
                               (M x)))))

	   (help "predicate, a set being not empty."))


(th~defdef subset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (O aa))
                       (forall (lam (x aa) (implies (U x) (V x)))))))
	   (help "Subset, defined by implication"))


(th~defdef subset2
	   (in typed-set)
	   (type-variables aa bb)
           (definition
             (lam (U (o aa bb))
                  (lam (V (O aa bb))
                       (forall (lam (x aa)
			(forall (lam (y bb)
			 (implies (U y x) (V y x)))))))))
	   (help "Subset, defined by implication"))

(th~defdef subsetp
	   (in typed-set)
	   (type-variables aa)
	   (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (forall-sort (lam (y aa) (in y V)) U))))
	   (help "Subset, defined with forall-sort and in"))
	   
;;; Added by Lassaad
(th~defdef proper-subset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
		       (and (subset U V)
			    (exists (lam (x aa) (and (V x) (not (U x)))))))))
	   (help "Proper-Subset, defined a.o. by subset"))

(th~defdef superset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (forall (lam (x aa) (implies (V x) (U x)))))))
           (help "Superset, defined by implication"))

(th~defdef powerset
	   (in typed-set)
	   (type-variables aa)
           (definition superset)
           (help "Powerset is the set of all subsets"))

(th~defdef powersetp
	   (in typed-set)
	   (type-variables aa)
           (definition
	     (lam (U (o aa))
                  (lam (V (o aa))
                       (subsetp V U))))
           (help "Powerset via subset."))


(th~defdef set=
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (O aa))
                  (lam (V (O aa))
                       (and (subset U V) (subset V U)))))
	   (help "Equality on sets, defined by subset"))

(th~defdef union
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (lam (x aa)
                            (or (U x) (V x))))))
           (help "Set union, defined by disjunction"))

(th~defdef union2
	   (in typed-set)
	   (type-variables aa bb)
           (definition
             (lam (U (o aa bb))
                  (lam (V (o aa bb))
                       (lam (x aa) (lam (y bb)
                            (or (U y x) (V y x)))))))
           (help "Set union, defined by disjunction"))

(th~defdef intersection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (O aa))
                  (lam (V (O aa))
                       (lam (x aa)
                            (and (U x) (V x))))))
	   (help "Set intersection, defined by conjunction."))

(th~defdef setminus
	   (in typed-set)
	   (type-variables aa)
           (definition 
             (lam (F (o aa))
                  (lam (G (o aa))
                       (lam (x aa)
                            (and (in x F)
                                 (not (in x G)))))))
	   (help "The set difference function. (setminus A B) is the set of all a in A that are not in B."))


(th~defdef set-complement
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (lam (x aa)
                            (and (U x) (not (V x)))))))
	   (help "(set-complement U V) is the set of all a in U that are not in V."))

(th~defdef singleton
	   (in typed-set)
	   (type-variables aa)
           (definition 
             (lam (x aa)
                  (lam (y aa)
                       (= x y))))
	   (help "The singleton function, (singleton x) is the set that only contains x."))
		    


(th~defdef union-over-collection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (Coll (o (o aa)))
                  (lam (x aa)
                       (exists-sort (lam (G (o aa))
                                         (in x G)) Coll))))
	   (help "Union over a collection of sets, defined by existential
quantification."))

(th~defdef intersection-over-collection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (Coll (o (o aa)))
                  (lam (x aa)
                       (forall-sort (lam (G (o aa))
					 (in x G))
				    Coll))))
           (help "Intersection over a collection of sets,
                  defined by universal quantification."))



(th~defdef meets
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa))
		  (lam (Y (o aa))
		       (exists (lam (Z aa) (and (in Z X) (in Z Y)))))))
	   (help "Predicate: There exists an element which is a member of both sets"))


(th~defdef misses
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa))
		  (lam (Y (o aa))
		       (not (exists (lam (Z aa) (and (in Z X) (in Z Y))))))))
	   (help "Predicate: There exists no element which is a member of both sets"))

(th~defdef exclunion
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa)) (lam (Y (o aa))
				  (union (setminus X Y)
					 (setminus Y X)))))
	   (help "The exclusive union of two sets."))




;;; for chris&volker from chris
   
(th~defdef add-one
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (x aa)
                  (lam (p (o aa))
                       (lam (t aa) (or (p t) (= t x))))))
	   (help "Add one element to a set"))


(th~defdef finite-set
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (p (o aa))
                  (forall (lam (w (o (o aa)))
		   (implies 
		    (and (w emptyset)
			 (forall (lam (r (o aa))
			  (forall (lam (x aa)
			   (implies (w r)
				    (w (add-one x r))))))))

		         (w p))))))
	   (help "Finite sets: A set p is finite iff it is in every collection w of sets
		 which contains the empty set and is closed under any application of add-one"))


;;; Introduced by chris (I don't know where else to put this def) sorry, MP
;
;(th~defdef pair
;           (in typed-set)
;           (type-variables aa)
;           (definition
;             (lam (x aa)
;                  (lam (y aa)
;                       (lam (p (o aa aa)) (p x y)))))
;           (help "Pairing function as introduced in Andrews86, p. 185"))
;
;(th~defdef cartesian-product
;           (in typed-set)
;           (type-variables aa)
;           (definition (lam (A (o aa))
;                            (lam (B (o aa))
;                                 (lam (P (o (o aa aa))) false))))  ;;; der Typ stimmt
;           ;;; aber wie sieht der Term aus?
;           (help "Cartesian Product"))
;
;

(th~defdef first-of-pair
   (in typed-set)
    (type-variables aa bb)
    (definition
      (lam (p (tuple aa bb))
	   (that (lam (x aa)
		      (exists (lam (y bb) (= (pair x y) p)))))))
 	   (help "First element of an ordered pair."))



(th~defdef second-of-pair
   (in typed-set)
    (type-variables aa bb)
    (definition
      (lam (p (tuple aa bb))
	   (that (lam (x bb)
		      (exists (lam (y aa) (= (pair y x) p)))))))
 	   (help "First element of an ordered pair."))




(th~defdef cartesian-product
           (in typed-set)
           (type-variables aa bb)
           (definition (lam (U (o aa))
			    (lam (V (o bb))
				(lam (p (tuple aa bb))
				     (exists-sort (lam (x aa)
							    (exists-sort (lam (y bb)
									      (= p (pair x y)))
									 V))
						       U)))))
	     (help "Cartesian Product"))



(th~defdef pair-operation
           (in typed-set)
           (type-variables aa bb)
           (definition (lam (op1 (aa aa aa))
		       (lam (op2 (bb bb bb))    
		       (lam (p1 (tuple aa bb))
		       (lam (p2 (tuple aa bb))
				     (pair (op1 (first-of-pair p1) (first-of-pair p2))
					   (op2 (second-of-pair p1) (second-of-pair p2))))))))
	     (help "Operation on ordered pairs defined by operations on each element."))

(th~defdef scalar-operation
           (in typed-set)
           (type-variables aa bb cc)
           (definition
	     (lam (op1 (aa aa cc))
		  (lam (op2 (bb bb cc))    
		       (lam (scalar cc)
			    (lam (p (tuple aa bb))
				 (pair (op1 scalar (first-of-pair p))
				       (op2 scalar (second-of-pair p))))))))
	   (help "Scalar operation on ordered pairs defined by operations on each element."))




(th~defdef not-empty
	   (in typed-set)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (exists (lam (a bb) (G a)))))
	   (help "Non-emptiness property of a set."))





(th~defdef strange-ho-abbr
	   (in typed-set)
           (type-variables bb)
	   (author cebrown)
           (definition
             (lam (S (o (o bb bb)))
                  (lam (x bb)
                       (lam (y bb)
                            (forall (lam (p (o bb bb))
                                    (implies (and (S p) (p x y))
                                             (p y x))))))))
           (help "Strange HO mapping from sets of relations to relations.
Used in strange-ho-example, where the 'p' is instantiated with
[strange-ho-abbr S]."))




;;;; (for chris: test)


(th~defdef has-fixpoint
	   (in typed-set)
           (type-variables bb)
	   (author chris)
           (definition
             (lam (F (bb bb))
		(exists (lam (X bb) (= (F X) X)))))
           (help "Predicate says: Function has a fix-point"))

(th~defdef is-constant-map
	   (in typed-set)
           (type-variables bb aa)
	   (author chris)
           (definition
             (lam (F (bb aa))
		(exists (lam (Y bb) (forall (lam (X aa) (= (F X) Y))))))) 
           (help "Predicate says: Function is a constant mapping"))

(th~defdef is-identity
	   (in typed-set)
           (type-variables bb)
	   (author chris)
           (definition
             (lam (F (bb bb))
		(forall (lam (X bb) (= (F X) X))))) 
           (help "Predicate says: Function is identity"))


;;; From VS

(th~defdef disjoint
	   (in typed-set)
	   (type-variables bb)
	   (definition
	     (lam (A (o bb))
		  (lam (B (o bb))
		       (= (intersection A B)
			  emptyset))))
	   (help "Two sets are disjoint if their intersection is empty."))



