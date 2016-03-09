;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

;; Die Theorie exl-nr soll grundlegende Elemente von Aussagenlogik enthalten
;; und die Moeglichkeit geben darauf aufbauende Sachverhalte adaptiv beschreiben zu
;; koennen


(th~deftheory exl-nr-th
              (uses resolution proof-theory)
	      (constants ;; Konstanten fuer die excess literal number
	                 (exl-nr           (num (o (o form))))
			 ;;(glist-length     (num glist))
			 (dip              (num form (o form) (o form) (o (o form))))
			 (wf-min-rel         (all-types rr ((o rr) (o rr rr) (o rr))))
			 (good-clause      ((o form) (o (o form))) )
			 (good-literal     (form     (o form))     )
			 ;; derivation-induction-parameter
			 ;; Dinge fuer noether`sche Induktion auf natuerlichen Zahlen.
			 ;(lesswfa          (o num num num))
			 ;(wellfound        (o (o rr rr)))
			 )
	      (help "A test theory for completeness proofs."))


;; Noether stuff
;; (wfMinRel         (all-types (rr) ((o rr) (o rr rr) (o rr))))
;; (lesswfa          (o num num num))
;; (wellfound        (o (o rr rr) (o rr)))
				
(th~defdef less-wf-nat-a
	   (in exl-nr-th)
	   (definition
	     (lam (a num) (lam (x num) (lam (y num)
		(and (less a y)
		     (less x y)))))))
		     

(th~defdef wellfound
	   (in exl-nr-th)
	   (type-variables rr)
	   (definition
	     (lam (M (o rr)) (lam (R (o rr rr))
		  true))))

(th~defaxiom wf-min-rel-a1
	     (in exl-nr-th)
	     (type-variables rr)
	     (formula (forall (lam (para num)
			(= (wf-min-rel Nat (less-wf-nat-a para))
			   (lam (x num) (and (leq zero x)
					     (leq x para))))))))


(th~defaxiom good-cl-ax
	     (in exl-nr-th)
	     (formula (forall (lam (klm (o (o form)))
		         (implies (not (empty klm))
				  (klm (good-clause klm)))))))

(th~defaxiom good-lit-ax
	     (in exl-nr-th)
	     (formula (forall (lam (kl (o form))
			 (implies (not (empty kl))
				  (kl (good-literal kl)))))))


;; Wenn mgl in natural einfuegen
;(th~defdef closed-under-s
;	   (in natural)
;           (definition 
;             (lam (G (o num))
;                  (lam (op (num num))
;                       (forall (lam (x num)
;                                    (implies (forall (lam (du num) (implies (leq du x) (in du G))))
;                                             (in (op x) G)))))))
;	   (help "Definition of the predicate for closure under an operation for sets of numbers."))


;(th~defaxiom nat-induction-strong
;	     (in natural)
;	     (formula (forall (lam (Q (o num))
;				   (implies (and (in zero Q)
;						 (closed-under-s Q s))
;					    (subset nat Q)))))
;	     (help "The induction axiom for natural numbers."))

;(th~defdef properties
;           (in exl-nr-th)
;           (type-variables rr)
;           (definition
;             (lam (afu (num rr)) true))
;           (help "A dummy predicate for applying a certain method."))

(th~defdef card>2
           (in exl-nr-th)
           (type-variables rr)
           (definition
             (lam (as (o rr))
		  (greater (cardinality as) one)))
           (help "The  assertion that a set ha more then 2 elemtns ."))

;
;
;;; ============ excess literal number  ==============================
(th~defaxiom exl-nr-base
             (in exl-nr-th)
             (formula (= (exl-nr emptyset) one)))

(th~defaxiom exl-nr-step
             (in exl-nr-th)
             (formula (forall (lam (S (o (o form))) (forall (lam (x (o form))
                         (equiv (not (in x S))
                                (= (exl-nr (union S (singleton x)))
                                   (minus (plus (exl-nr S)
                                                (cardinality x))
                                          one)))))))))


;(th~defaxiom cardinality-base
;              (in exl-nr-th)
;              (formula (= (cardinality emptyset) zero)))
;
;
;(th~defaxiom cardinality-step
;              (in exl-nr-th)
;              (type-variables bb)
;              (formula (forall (lam (M (o bb)) (forall (lam (x bb)
;                          (equiv (M x)
;                                 (= (cardinality M)
;                                    (s (cardinality (setminus M (singleton x))))))))))))

;(th~defaxiom dip-axiom1
;             (in exl-nr-th)
;             (formula (forall (lam (klm (o (o form))) (forall (lam (gkl (o form))
;                        (forall (lam (n num)  (forall (lam (R (o crule))
;                          (equiv (= (dip klm gkl) n)
;                                  (exists (lam (gdl glist)
;                                    (and (derivation-of klm R gdl gkl)
;                                         (and (= (glist-length gdl) n)
;                                              (forall (lam (gdl1 glist)
;                                                 (implies (derivation-of klm R gdl1 gkl)
;                                                          (leq (glist-length gdl)
;                                                               (glist-length
;                                                                gdl1))))))))))))))))))))
