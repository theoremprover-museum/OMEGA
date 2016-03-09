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
(in-package :omega)

;; cebrown 30/7/01

(th~deftheorem top-per-is-a-per
	       (in per-sorts)
	       (conclusion
		(per top-per))
	       (help "The full relation is a per."))

(th~deftheorem eq-per-is-a-per
	       (in per-sorts)
	       (conclusion
		(per eq-per))
	       (help "Equality is a per."))

(th~deftheorem bot-per-is-a-per
	       (in per-sorts)
	       (conclusion
		(per bot-per))
	       (help "The empty relation is a per."))

(th~deftheorem pred-pers-are-pers
	       (in per-sorts)
	       (conclusion
		(all-types
		 aa
		 (forall
		  (lam (P (o aa))
		       (per (pred-per P))))))
	       (help "A predicate P can be made into a per using equality"))

(th~deftheorem fun-pers-are-pers
	       (in per-sorts)
	       (conclusion
		(all-types
		 aa bb
		 (forall
		  (lam (P (o aa aa))
		       (implies (per P)
				(forall
				 (lam (Q (o bb bb))
				      (implies (per Q)
					       (per (fun-per-sort P Q))))))))))
	       (help "The definition of function (exponent) pers does give a per."))

;; I only used sub-relation in the conclusion so we don't have to show
;; both (fun-per-sort P2 Q1) and (fun-per-sort P1 Q2) are actually pers.
;; (The previous theorem shows this.)
(th~deftheorem per-contra-co-variance
	       (in per-sorts)
	       (conclusion
		(all-types
		 aa bb
		 (forall
		  (lam (P1 (o aa aa))
		       (forall
			(lam (P2 (o aa aa))
			     (implies (subper P1 P2)
				      (forall
				       (lam (Q1 (o bb bb))
					    (forall
					     (lam (Q2 (o bb bb))
						  (implies (subper Q1 Q2)
							   (sub-relation
							    (fun-per-sort P2 Q1)
							    (fun-per-sort P1 Q2)))
						  )))))))))))
	       (help "Contravariance and covariance for function pers."))
		
			
