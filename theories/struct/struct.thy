;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheory struct
              (uses typed-set)
	      (help "Theory for properties of algebraic structures."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Definitions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions of properties for operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(th~defdef closed-under
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (forall-sort (lam (a bb)
					 (forall-sort (lam (b bb)
							   (G (op a b)))
						      G))
				    G))))
	   (help "Closure property of an internal binary operation."))

(th~defdef associative
	   (in struct)
           (type-variables bb)
           (definition 
             (lam (G (o bb))
                  (lam (dot (bb bb bb))
                       (forall-sort (lam (x bb)
			(forall-sort (lam (y bb)
			 (forall-sort (lam (z bb)
			  (= (dot x (dot y z)) (dot (dot x y) z))) G)) G)) G))))
	   (help "Definition of the associativity predicate. (associative G *) is true, iff the operation * is associative on G."))

(th~defdef unit
	   (in struct) 
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (e bb)
			    (forall-sort (lam (a bb)
					      (and (= (op a e) a)
						   (= (op e a) a)))
					 G)))))
	   (help "Existence of a unit element with respect to an operation on a set."))

(th~defdef inverse-function
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (e bb)
			    (lam (inv (bb bb))
				 (forall-sort (lam (x bb)
						   (and (= (op x (inv x)) e)
							(= (op (inv x) x) e)))
					      G))))))
	   (help "The inverse function for a group G with operation op and unit element e."))

(th~defdef inverse-exist
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (e bb)
			    (forall-sort (lam (a bb)
					      (exists-sort (lam (x bb)
								(and (= (op a x) e)
								     (= (op x a) e)))
							   G))
					 G)))))
	   (help "Existence of inverse elements with respect to an operation on a set."))

(th~defdef left-unit
	   (in struct)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (lam (e aa)
			    (forall-sort (lam (a aa)
					      (= (op e a) a)) G)))))
	   (help "Existence of a left unit element with respect to an operation on a set."))

(th~defdef left-inverse-exist
	   (in struct)
	   (type-variables aa)
	   (definition
		  (lam (G (o aa))
		       (lam (op (aa aa aa))
			    (lam (e aa)
				 (forall-sort (lam (a aa)
						  (exists-sort (lam (x aa)
								    (= (op x a) e)) G)) G)))))
	   (help "Existence of left inverse elements with respect to an operation on a set."))

(th~defdef right-unit
	   (in struct)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (lam (e aa)
			    (forall-sort (lam (a aa)
					      (= (op a e) a)) G)))))
	   (help "Existence of a right element with respect to an operation on a set."))

(th~defdef right-inverse-exist
	   (in struct)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (lam (e aa)
			    (forall-sort (lam (a aa)
					      (exists-sort (lam (x aa)
								(= (op a x) e)) G)) G)))))
	   (help "Existence of right inverse elements with respect to an operation on a set."))


(th~defdef divisors-exist
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (forall-sort (lam (a bb)
					 (forall-sort (lam (b bb)
							   (and (exists-sort (lam (x bb)
										  (= (op a x) b))
									     G)
								(exists-sort (lam (y bb)
										  (= (op y a) b))
									     G)))
						      G))
				    G))))
	   (help "Existence of right and left divisors with respect to an operation on a set. "))

(th~defdef struct-left-unit 
	   (in struct)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (that (lam (neut bb)
				  (left-unit G op neut))))))
	   (help "The struct-left-unit is the element of the structures for which the left-unit property holds."))

(th~defdef struct-right-unit 
	   (in struct)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (that (lam (neut bb)
				  (right-unit G op neut))))))
	   (help "The struct-right-unit is the element of the structures for which the right-unit property holds."))

(th~defdef struct-unit 
	   (in struct)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (that (lam (neut bb)
				  (and (left-unit G op neut)
				       (right-unit G op neut)))))))
	   (help "The struct-unit is the element of the structures for which the unit property holds."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for Alternative Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defdef unique-inverse
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (inv (bb bb))
		       (forall-sort (lam (a bb)
					 (exists-sort-unique (lam (b bb) (= b (inv a)))
							     G))
				    G))))
	   (help "Unique property of the existence of an inverse."))

(th~defdef law-of-inverse
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (inv (bb bb))
			    (forall-sort (lam (a bb)
					      (forall-sort (lam (b bb)
								(= (op (inv a) (op a b))
								   (op (op b a) (inv a))))
							   G))
					 G)))))
	   (help "The law of inverses for groups."))
					 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defdef commutative
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (forall-sort (lam (a bb)
					 (forall-sort (lam (b bb)
							   (= (op a b) (op b a)))
						      G))
				    G))))
	   (help "Commutativity of a set G with respect to operation op."))

(th~defdef distributive
	   (in struct)
	   (type-variables bb)
	   (definition
	     (lam (G (o bb))
		  (lam (add (bb bb bb))
		       (lam (mul (bb bb bb))
			    (forall-sort (lam (a bb)
					      (forall-sort (lam (b bb)
								(forall-sort (lam (c bb)
										  (and (= (mul a (add b c))
											  (add (mul a b) (mul a c)))
										       (= (mul (add a b) c)
											  (add (mul a c) (mul b c)))))
									     G))
							   G))
					 G)))))
	   (help "Distributivity of a set G with respect to the operations add and mul."))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formalisations using the struct-type

(th~deftype struct
  (in struct)
  (arguments 1)
  (help "Parameterized by the type of the elements of the base set."))


(th~deftype bistruct
  (in struct)
  (arguments 2)
  (help "A structure that is paramterized by two type variables,
         e.g. a  metric space, where the measure-space may be
         of a different type than the structure itself."))

(th~deftype tristruct
  (in struct)
  (arguments 3)
  (help "A structure that is paramterized by three type variables,
         e.g. a  metric space, where the measure-space may be
         of a different type than the structure itself."))

(th~defconstant struct-set
  (in struct)
  (type (all-types cc (o cc (struct cc))))
  (help "The base set of a given structure."))

(th~defconstant struct-op
		(in struct)
		(type (all-types cc (cc cc cc (struct cc))))
		(help "The operation of a structure"))

(th~defdef struct-neut
          (in struct)
          (type-variables aa)
          (definition
            (lam (S (struct aa))
                   (that (lam (x aa)
                              (and (in x (struct-set S))
                                   (unit (struct-set S)
					 (struct-op S)
					 x))))))
          (help "The unique neutral element of a structure."))


(th~defdef struct-inv
          (in struct)
          (type-variables aa)
          (definition
            (lam (S (struct aa))
                   (that (lam (f (aa aa))
                            (forall-sort (lam (x aa)
					      (= ((struct-op S) x (f x))
						 (struct-neut S)))
					 (struct-set S))))))
          (help "The inverse function induced by the group operation, defined as right inverse. (struct-inv S) is the inverse function of S."))


(th~defconstant struct-ordering
		(in struct)
		(type (all-types bb (o bb bb (struct bb))))
		(help "The order of a structure"))

(th~defconstant struct-mul-sgroup
		 (in struct)
		 (type (all-types aa ((struct aa) (struct aa))))
		 (help "The feature for a the multiplicative subgroup of a ring"))




;; Accessinge sub-structs of a bi-struct and the possible operation between these substructs


(th~defconstant bistruct-struct1
  (in struct)
  (type (all-types aa bb ((struct aa) (bistruct aa bb))))
  (help "The first structure of a given bi-structure."))

(th~defconstant bistruct-struct2
  (in struct)
  (type (all-types aa bb ((struct bb) (bistruct aa bb))))
  (help "The second structure of a given bi-structure."))


(th~defconstant bistruct-ops1xs2=s1
  (in struct)
  (type (all-types aa bb ((aa bb aa) (bistruct aa bb))))
  (help "The operation from S1xS2 -> S1 of a given bi-structure."))

(th~defconstant bistruct-ops1xs2=s2
  (in struct)
  (type (all-types aa bb ((bb bb aa) (bistruct aa bb))))
  (help "The operation from S1xS2 -> S2 of a given bi-structure."))

(th~defconstant bistruct-ops2xs1=s1
  (in struct)
  (type (all-types aa bb ((aa aa bb) (bistruct aa bb))))
  (help "The operation from S2xS1 -> S1 of a given bi-structure."))

(th~defconstant bistruct-ops2xs1=s2
  (in struct)
  (type (all-types aa bb ((bb aa bb) (bistruct aa bb))))
  (help "The operation from S2xS1 -> S2 of a given bi-structure."))
