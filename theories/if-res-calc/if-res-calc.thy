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

(th~deftheory if-res-calc
              (uses proof-theory resolution exl-nr-th)
	      (constants (res-start crule)
			 (res-res   crule)
			 (crule-set (o crule))
			 (form2item (item form))
			 (input-first  ((o form) glist))
			 (input-second ((o form) glist))
			 (input-third  (form     glist))
			 (input-fourth (form     glist))
			 )
	      (help "The all res.-calculus common stuff."))

(th~defaxiom crule-set
  (in if-res-calc)
  (formula (forall (lam (x (crule))
			(equiv (crule-set x)
			       (or (= x res-start)
				   (= x res-res)))))))

(th~defaxiom  start-applicable
	      (in if-res-calc)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			  (equiv (crule-applicable res-start seq S)
				 (in (item2cl (first seq)) S)))))))
	      (help "The res-start crule is applicable, if the premise clause is element of S."))


(th~defaxiom  start-application
	      (in if-res-calc)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			   (= (crule-application res-start seq S)
			      (item2cl (first seq))))))))
	      (help "The conclusion of the start crule is its premise. Only the side condition of membership in S, has to be fullfilled."))

(th~defaxiom start-result
	     (in if-res-calc)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result res-start seq S cl)
				(= (item2cl (first seq)) cl)))))))))
	     (help "The result of the start crule is is premise."))

