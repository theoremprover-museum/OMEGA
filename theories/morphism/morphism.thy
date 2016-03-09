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

(th~deftheory morphism
              (uses function struct)
	      (help "Definitions of/for morphisms between structures"))


(th~defdef homomorphism
	   (in morphism)
           (type-variables aa bb)
	   (definition
             (lam (G1 (o aa))
		  (lam (op1 (aa aa aa))
		       (lam (G2 (o bb))
			    (lam (op2 (bb bb bb))
				 (lam (phi (bb aa))
				      (forall-sort (lam (x aa)
							(forall-sort (lam (y aa)
									  (= (phi (op1 x y))
									     (op2 (phi x) (phi y))))
								     G1))
						   G1)))))))
	   (help "The predicate for homomorphism wrt. the structure operation."))
;; Here is missing an entry that phi(x),phi(y), and phi(op1(x,y)) in G2 

(th~defdef isomorphic
	   (in morphism)
	   (type-variables aa bb)
	   (definition
	     (lam (G1 (o aa))
		  (lam (op1 (aa aa aa))
		       (lam (G2 (o bb))
			    (lam (op2 (bb bb bb))
				 (exists-sort (lam (phi (bb aa))
						   (and (and (surjective G1 G2 phi)
							     (injective G1 phi))
							(homomorphism G1 op1 G2 op2 phi)
							))
					      (functions G1 G2)))))))
	   (help "Isomorphism between to pairs of sets and operations."))

(th~defdef kernel-for-elem  ; now in morphism
	   (in morphism)
	   (type-variables bb cc)
           (definition 
	     (lam (f (bb cc))
		  (lam (dom (o cc))
		       (lam (elem bb)
			    (lam (x cc)
				 (and (dom x)
				      (= (f x) elem)))))))
	   (help "The kernel for an element of a morphism."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formalisations using the struct-type



(th~deftype morphism
  (in morphism)
  (arguments 2)
  (help "Parameterized by the types of the elements of the base sets
	 of the domain and codomain structures."))




(th~defconstant morphism-function
		(in morphism)
		(type (all-types cc dd (cc dd (morphism cc dd))))
                (help "The function of a morphism."))

(th~defconstant morphism-domain
		  (in morphism)
		  (type (all-types cc dd ((struct dd) (morphism cc dd))))
                  (help "The domain of the morphism."))
(th~defconstant morphism-codomain
		(in morphism)
		(type (all-types cc dd ((struct cc) (morphism cc dd))))
                (help  "The codomain of a morphism."))

(th~defaxiom morphism-functional
	  (in morphism)
	  (type-variables aa bb)
	  (formula
	   (forall (lam (M (morphism aa bb))
	    (and (= (struct-set (morphism-domain M))
		    (domain (morphism-function M)))
		 (= (struct-set (morphism-codomain M))
		    (image (morphism-function M)))))))
	  (help "The (co)-domain of the function of a morphism is the set of its (co)-domain structure"))

(th~defdef struct-monomorphism
	   (in morphism)
	   (type-variables aa bb)
           (definition 
             (lam (F (morphism aa bb))
                  (injective (struct-set (morphism-domain F))
			     (morphism-function F))))
           (help "A morphism is a nonomorphism, iff it is injective."))

(th~defdef struct-epimorphism
	   (in morphism)
	   (type-variables aa bb)
           (definition 
             (lam (F (morphism aa bb))
                  (surjective (struct-set (morphism-domain F))
                              (struct-set (morphism-codomain F))
			      (morphism-function F))))
	   (help "A morphism is an epimorphism, iff it is surjective."))

(th~defdef struct-isomorphism
	   (in morphism)
	   (type-variables aa bb)
           (definition 
             (lam (F (morphism aa bb))
                  (bijective (struct-set (morphism-domain F))
                             (struct-set (morphism-codomain F))
			     (morphism-function F))))
	   (help "A morphism is an isomorphism, iff it is bijective."))

(th~defdef pfunc-struct
           (in morphism)
           (type-variables aa bb)
           (definition
             (lam (S (struct bb))
                  (lam (G (o aa))
                       (that (lam (T (struct (bb aa)))
                                  (= (struct-set T) 
				     (functions G (struct-set S))))))))
	   (help "The structure (pfunc-struct S G) of functions from a set G into a given structure S."))


