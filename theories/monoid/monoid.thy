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

(th~deftheory monoid
              (uses semigroup)
	      (help "Monoid theory."))



;;;
;;; Definition of Monoid
;;;

(th~defdef monoid	
	   (in monoid)
	   (type-variables aa)
	   (definition
	     (lam (M (o aa))
		  (lam (op (aa aa aa))
		       (and (not-empty M)
			    (and (closed-under M op)
				 (and (associative M op)
				      (exists-sort (lam (e aa) (unit M op e)) M)))))))
	   (help "Definition of a Monoid."))


;(th~defdef monoid
;           (in monoid)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                    (and (semigroup S)
;                         (exists (lam (neut aa)
;                          (and (in neut (struct-set S))
;                               (neutral-in (struct-set S)
;                                           (struct-op S)
;                                           neut)))))))
;           (help "The monoid predicate.
;                  A structure is a monoid, iff it is a semigroup and has a neutral element."))
;
;(th~defdef ida-monoid
;           (in monoid)
;           (type-variables se)
;           (definition
;             (lam (M (o se))
;                  (lam (op (se se se))
;                       (lam (unit se)
;                            (and (ida-semigroup M op)
;                                 (neutral-in M op unit))))))
;             (help "The monoid predicate.
;                  A structure is a monoid, iff it is a semigroup and has a neutral element."))
;
;(th~defdef abelian-monoid
;           (in monoid)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                  (and (abelian-semigroup S)
;                       (monoid S))))
;           (help "The abelian monoid predicate. (abelian-monoid G * e) is true, iff G is a  monoid for the operation * and the neutral element e and * is commutative on G."))
;
;
;(th~defdef idempotent-monoid
;           (in monoid)
;           (type-variables bb)
;           (definition
;             (lam (S (struct bb))
;                  (and (monoid S)
;                       (forall (lam (X bb)
;                        (implies (in X (struct-set S))
;                                 (= (struct-op S X X) (struct-neut S))))))))
;           (help "The idempotent monoid predicate.
;                  (idempotent-monoid S) is true, iff G is a monoid and
;                  x*x is the neutral element of S for all x.")) 
;
;(th~defdef kernel-set
;           (in monoid)
;           (type-variables aa bb)
;           (definition
;             (lam (M (morphism aa bb))
;                    (lam (x bb)
;                         (and (in x (struct-set (morphism-domain M)))
;                              (= (morphism-function M x) (struct-neut (morphism-codomain M)))))))
;           (help "The kernel of a homomorphism is the pre-image of the neutral element in the codomain."))



