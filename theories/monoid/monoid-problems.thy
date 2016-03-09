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



;(th~defproblem left-neut-right
;               (in monoid)
;               (conclusion 
;                (all-types aa (forall (lam (S (struct aa))
;                                           (forall (lam (y aa)
;                                                        (forall (lam (x aa)
;                                                                     (implies (and (and (struct-set S x)
;                                                                                        (struct-set S y))
;                                                                                   (and (left-neutral-in (struct-set S)
;                                                                                                         (struct-op S)
;                                                                                                         x)
;                                                                                        (right-neutral-in (struct-set S)
;                                                                                                          (struct-op S)
;                                                                                                          y)))
;                                                                              (= x y))))))))))
;               (help "When there exists an element that is left-neutral and an element that is right-neutral, then
;                      they are equal."))
;
;(th~defproblem feature-thm-monoids
;               (in monoid)
;               (conclusion (total  monoid struct-neut))
;               (help "The feature structu-neut is total on monoids."))


;;;;;; relativ uninteressante Konsequenz des feature-thm
;(th~defproblem left-neut-unique
;               (in monoid)
;               (conclusion 
;                (all-types aa (forall (lam (S (struct aa))
;                                           (implies (monoid S)
;                                                    (forall (lam (y aa)
;                                                                 (implies (in y (struct-set S))
;                                                                          (implies (left-neutral-in (struct-set S) 
;                                                                                                    (struct-op S)
;                                                                                                    y)
;                                                                                   (= y (struct-neut S)))))))))))
;               (help "For any monoid, the unit is unique."))
;
;(th~defproblem right-neut-unique
;               (in monoid)
;               (conclusion 
;                (all-types aa (forall (lam (S (struct aa))
;                                           (implies (monoid S)
;                                                    (forall (lam (y aa)
;                                                                 (implies (in y (struct-set S))
;                                                                          (implies (right-neutral-in (struct-set S) 
;                                                                                                     (struct-op S)
;                                                                                                     y) 
;                                                                                   (= y (struct-neut S)))))))))))
;               (help "For any monoid, the unit is unique."))



;(th~defproblem idempotent-monoid-abelian
;               (in monoid)
;               (conclusion 
;                (all-types aa (forall (lam (S (struct aa))
;                                           (implies (idempotent-monoid S) (abelian-monoid S))))))
;               (help "Idempotent monoids are abelian."))
;
;(th~defproblem total-struct-neut
;               (in monoid)
;               (conclusion (total monoid struct-neut))
;               (help "Every monoid has a neutral element."))


;(th~defproblem neutral-pointwise
;               (in monoid)
;               (type-variables bb)
;               (conclusion 
;                (forall (lam (S struct)
;                             (forall (lam (F (bb bb))
;                                          (= F (apply-pointwise (struct-op S) F (lam (x bb) (struct-neut S)))))))))
;               (help "The function for the pointwise neutral element is a neutral element in the function space."))

;(th~defproblem pfunc-monoid
;               (in monoid)
;               (conclusion 
;                (forall (lam (S struct)
;                             (implies (monoid S) (monoid (pfunc-struct S))))))
;               (help "The structure of pointwise functions of a semigroup is again one."))



