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

(th~deftheory semigroup
              (uses magma)
	      (help "Semi-group theory"))

;;;
;;; Definition of SEMIGROUP
;;;

(th~defdef semigroup
	   (in semigroup)
	   (type-variables aa)
	   (definition
	     (lam (S (o aa))
		  (lam (op (aa aa aa))
		       (and (not-empty S)
			    (and (closed-under S op)
				 (associative S op))))))
	   (help "Definition of a Semigroup."))

;(th~defdef semigroup
;           (in semigroup)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                    (and (and (defined (struct-set S))
;                              (defined (struct-op S)))
;                         (and (closed-under-2 (struct-set S) (struct-op S))
;                              (associative (struct-set S) (struct-op S))))))
;           (help "The semigroup predicate for structures.
;                  (semigroup S) is true, iff S has a base set G(S)
;                  and an operation *(S) such that G(S) is closed under *(S)
;                  and *(S) is associative."))


;(th~defdef abelian-semigroup
;           (in semigroup)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                  (and (semigroup S)
;                       (commutative (struct-set S) (struct-op S)))))
;           (help "The  predicate for abelian semigroups.
;                  (abelian-semigroup S) is true, iff S is a semi-group
;                   and *(S) is commutative on G(S)."))

;(th~defdef homomorphism
;           (in semigroup)
;           (type-variables aa bb)
;           (definition
;             (lam (F (morphism aa bb))
;                  (forall (lam (x bb)
;                               (forall (lam (y bb)
;                      (= (struct-op (morphism-codomain F)
;                                    (morphism-function F x)
;                                    (morphism-function F y))
;                         (morphism-function F (struct-op (morphism-domain F) x y)))))))))
;           (help "The predicate for homomorphism wrt. the structure operation."))

;(th~defdef restrict-semigroup
;           (in semigroup)
;           (type-variables aa)
;           (definition
;             (lam (S (struct aa))
;                    (lam (G (o aa))
;                         (choose-from (lam (T (struct aa))
;                                           (and (subset G (struct-set S))
;                                                (and (= (struct-set T) G)
;                                                     (= (struct-op T)
;                                                        (struct-op S)))))))))
;           (help "The restriction of a semigroup to a subset of the base set."))







