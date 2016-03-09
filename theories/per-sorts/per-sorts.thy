;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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


;; cebrown 30/7/01

(th~deftheory PER-SORTS
	      (uses nic relation) ; relation -> typed-set -> post - which is
					; unfortunate since but this doesn't
					; really depend on typed-set or post
					; but only the defns of transitive and symmetric
	      (constants )
	      (help "PER-SORTS - Using Partial Equivalence Relations
to sort higher order logic"))

;; Pers are transitive and symmetric binary relations on a type.
;; "Per Sorts" are simply pers, but I call them per sorts here
;; to emphasize that this notion of sorts extends to function
;; types without modifying the syntax of simply typed lambda calculus.
;; Actually, the domain of the per (in-per-dom) are what one would
;; usually think of as the sort, and the per plays the role of
;; equality on the sort.  We need the whole per (not just its domain)
;; to define the function per sort.
(th~defdef per
	   (in per-sorts)
	   (type-variables aa)
	   (definition
	     (lam (P (o aa aa))
		  (and (transitive P)
		       (symmetric P)))))

(th~defdef in-per-dom
	   (in per-sorts)
	   (type-variables aa)
	   (definition
	     (lam (x aa)
		  (lam (P (o aa aa))
		       (P x x))))
	   (help "An element x is in the domain of a per P if P x x
(x is related to itself by P).
One can prove that, given a per P, if x is related to anything by P, then
it must be related to itself by P."))

;; top per at a type is not what you think - it contains
;; everything of the type, but also thinks they are all the same.
(th~defdef top-per
	   (in per-sorts)
	   (type-variables aa)
	   (definition (lam (x aa) (lam (y aa) true)))
	   (help "The full binary relation at a type.
This is the greatest partial equivalence relation on the type."))

;; equality is what corresponds to the full sort:
;; everything of the type is in the domain, but it still distiguishes elements.
(th~defdef eq-per
	   (in per-sorts)
	   (type-variables aa)
	   (definition (lam (x aa) (lam (y aa) (= x y))))
	   (help "The equality relation at a type.
This is a partial equivalence relation whose domain
contains every element of the type, but still distiguishes
between elements."))

;; bottom per is just the empty relation
(th~defdef bot-per
	   (in per-sorts)
	   (type-variables aa)
	   (definition (lam (x aa) (lam (y aa) false)))
	   (help "The empty binary relation at a type.
This is the least partial equivalence relation on the type."))

;; pred-per gives a way of turning predicates into a per
;; by taking equality to be the necessary notion of equivalence
(th~defdef pred-per
	   (in per-sorts)
	   (type-variables aa)
	   (definition (lam (P (o aa))
			    (lam (x aa)
				 (lam (y aa)
				      (and (P x)
					   (= x y))))))
	   (help "Given a predicate P, return
the per which relates two elements when they
are in P and are equal."))

;; The per sorted quantifiers

(th~defdef forall-per-sort
	   (in per-sorts)
	   (type-variables aa)
	   #+logic-new(connective-type universal-quantor)
           (definition
             (lam (V (o aa))
                  (lam (U (O aa aa))
                       (forall (lam (x aa) (implies (in-per-dom x U) (V x)))))))
	   (help "The per sorted universal quantifier.
                  (forall-per-sort (lam (X aa) A) S) is an abbreviation for
(forall (lam (X aa) (implies (in-per-dom X S) A))"))

(th~defdef exists-per-sort
	   (in per-sorts)
	   (type-variables aa)
	   #+logic-new(connective-type existential-quantor)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa aa))
		       (exists (lam (X aa)
				    (and (in-per-dom X S) (T X)))))))
	   (help "The per sorted existential quantifier.
                  (exists-per-sort (lam (X aa) A) S) is an abbreviation for
(exists (lam (X aa) (and (in-per-dom S X) A)))"))

; this is genuinely different, because we want
; to consider elements in the per sort the same when
; they are related by the per.  That is, we don't use equality at
; the type.
(th~defdef exists-unique-per-sort
           (in per-sorts)
	   (type-variables aa)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa aa))
		       (exists
			(lam (X aa)
			     (and (and (in-per-dom X S)
				       (T X))
				  (forall
				   (lam (Y aa)
					(implies (and (in-per-dom Y S) (T Y))
						 (S Y X))))))))))
	   (help "The per sorted quantifier for unique existence.
This does not use equality to test for uniqueness,
but only equivalence up to the per sort.  So,
(exists-unique-per-sort (lam (X aa) A) S)
means there is an X in A with X in the domain of the per S,
and for every other Y in A with Y in the domain of S,
we have S X Y (X and Y related by S)."))
                 
(th~defdef exists-per-sort-unique
           (in per-sorts)
	   (type-variables aa)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa aa))
		       (exists-per-sort
			(lam (X aa)
			     (and (T X)
				  (forall-per-sort
				   (lam (Y aa) (implies (T Y)
							(S Y X))) ; S plays
					; the role
					; of equality
				   S)))
			S))))
	   (help "The per sorted quantifier for unique existence
defined via per sorted versions of the exists and forall quantifier."))

; this is where we really need pers:
(th~defdef fun-per-sort
  (in per-sorts)
  (type-variables cc bb)
  (definition
    (lam (Dom (o cc cc))
	 (lam (Im (o bb bb))
	      (lam (F (bb cc))
		   (lam (G (bb cc))
			(forall
			 (lam (x cc)
			      (forall
			       (lam (y cc)
				    (implies (Dom x y) ; related values
					     (Im (F x) (G y)))))))))))) ; go to related results
  (help "The functional per sort with domain Dom and range Im.
Checks not only that elements in (the domain of the per) Dom
go to elements in (the domain of the per) Im,
but that related values map to related results.")
  )

;; Relationships between pers

;; subper just means a per is a subrelation of another per
(th~defdef subper
  (in per-sorts)
  (type-variables aa)
  (definition
    (lam (P (o aa aa))
	 (lam (Q (o aa aa))
	      (and (and (per P) (per Q))
		   (sub-relation P Q)))))
  (help "P is a subper of Q if they are both pers
and P is a subrelation of Q."))


