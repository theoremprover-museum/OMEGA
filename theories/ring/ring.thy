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

(th~deftheory ring
              (uses group)
	      (help "Ring theory"))



(th~defdef ring
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (o aa))
		  (lam (add (aa aa aa))
		       (lam (mul (aa aa aa))
			    (and (abelian-group S add)
				 (and (monoid S mul)
				      (and (distributive S add mul)
					   (not (= (struct-unit S add)
						   (struct-unit S mul))))))))))
	   (help "The ring predicate for structures."))

(th~defdef abelian-ring
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (o aa))
		  (lam (add (aa aa aa))
		       (lam (mul (aa aa aa))
			    (and (ring S add mul)
				 (commutative S mul))))))
	   (help "The abelian-ring predicate for structures."))

(th~defdef ring-with-one
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (o aa))
		  (lam (add (aa aa aa))
		       (lam (mul (aa aa aa))
			    (and (ring S add mul)
				 (exists-sort (lam (e aa)
						   (unit S mul e))
					      S))))))
	   (help "The ring-with-one predicate for structures."))

(th~defdef abelian-ring-with-one
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (o aa))
		  (lam (add (aa aa aa))
		       (lam (mul (aa aa aa))
			    (and (abelian-ring S add mul)
				 (exists-sort (lam (e aa)
						   (unit S mul e))
					      S))))))
	   (help "The abelian ring-with-one predicate for structures."))


 

(th~defdef ring-zero
	   (in ring)
	   ;; (type-variables bb) ;; struct-neut ist polymorph und gibt seinen Typ weiter
	   ;; an ring-zero. Eine Angabe in Form von type-variables sollte hier nicht
	   ;; sinnvoll - oder gar moeglich - sein.
           (definition struct-neut)
	   (help "The additive neutral element for rings."))

(th~defdef ring-one
	   (in ring)
           (definition (compose-functions struct-mul-sgroup struct-neut))
	   (help "The multiplicative neutral element for rings."))


(th~defdef ring-plus
	   (in ring)
           (definition struct-op)
	   (help "The additive operation for rings."))


(th~defdef ring-negative
	   (in ring)
           (definition struct-inv)
	   (help "The additive inversion operation for rings."))

(th~defdef ring-minus
           (in ring)
           (type-variables bb)
           (definition (lam (S (struct bb))
                            (lam (x bb)
                                 (lam (y bb)
                                      (ring-plus S x (struct-inv S y))))))
           (help "The difference function for rings."))


(th~defdef ring-times
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (struct-op (struct-mul-sgroup S))))
	   (help "The multiplicative operation for rings."))

(th~defdef ring-set-star
	   (in ring)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
		    (struct-set (struct-mul-sgroup S))))
	   (help "The set of the multiplicative subgroup of rings, 
                  i.e. the base set minus the zero."))



(th~defdef induced-homomorphism-mul-sgroup
	   (in ring)
	   (type-variables aa bb)
           (definition
             (lam (F (morphism aa bb))
                  (that (lam (G (morphism aa bb))
                             (and (and (= (morphism-domain G)
                                          (struct-mul-sgroup (morphism-domain F)))
                                       (= (morphism-codomain G)
                                          (struct-mul-sgroup (morphism-codomain F))))
                                  (= (morphism-function G) (morphism-function F)))))))
	   (help "The predicate for ring-homomorphism."))


;(th~defdef ring-homomorphism
;           (in ring)
;           (type-variables aa bb)
;           (definition
;             (lam (F (morphism  aa bb))
;                    (and (homomorphism F)
;                         (homomorphism (induced-homomorphism-mul-sgroup F)))))
;           (help "The predicate for ring-homomorphism."))

;geht nicht wegen fehelndem restrict-semigroup, und das nicht wegen fehlender polymorphie
;(th~defdef restrict-ring
;           (in ring)
;           (type-variables bb)
;           (definition
;             (lam (S (struct bb))
;                    (lam (G (o bb))
;                         (choose-from (lam (T (struct bb))
;                          (and (= T (restrict-semigroup S G))
;                               (= (struct-mul-sgroup T)
;                                  (restrict-semigroup 
;                                   (struct-mul-sgroup S)
;                                   (setminus G (singleton (ring-one S)))))))))))
;           (help "The restriction of a ring to a subset of the base set."))

(th~defdef ideal
	   (in ring)
	   (type-variables bb)
           (definition 
             (lam (S (o bb))
		    (lam (T (struct bb))
			 (and (and (subset S (struct-set T))
				   (non-empty  S))
			      (and (forall (lam (x bb)
						(forall (lam (y bb)
							     (implies (and (in x  S) (in y  S))
								      (in (ring-minus T x y) S))))))
				   (forall (lam (x bb)
						(forall (lam (y bb)
							     (implies (and (in x S) (in y (struct-set T)))
								      (and (in (ring-times T x y) S)
									   (in (ring-times T y x) S))))))))))))
	   (help "(ideal S T) is true, iff S is an ideal of T."))

