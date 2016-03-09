;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

;pplus,ptimes,polynomials, local-minimum-at are not defined! MP
;
;(th~defproblem continuous-pplus
;    (in calculus)
;    (conclusion 
;     (forall-sort  (lam (F (num num))
;      (forall-sort (lam (G (num num))
;       (continuous (pplus F G))) continuous)) continuous))
;    (help "The sum of continuous functions is continuous."))
;
;
;(th~defproblem continuous-ptimes
;    (in calculus)
;    (conclusion 
;     (forall-sort  (lam (F (num num))
;      (forall-sort  (lam (G (num num))
;       (continuous (ptimes F G))) continuous)) continuous))
;    (help "The product of continuous functions is continuous."))
;
;
;(th~defproblem continuous-polynomial
;    (in calculus)
;    (conclusion 
;     (forall (lam (P (num num))
;       (implies (polynomial P) (continuous P)))))
;    (help "The polynomials are continuous."))
;
;
;(th~defproblem chain-rule
;    (in calculus)
;    (conclusion 
;     (forall-sort (lam (F (num num))
;      (forall-sort (lam (G (num num))
;       (= (derivative (ptimes F G))
;          (pplus (ptimes (derivative F) G)
;                 (ptimes F (derivative G)))))  differentiable))  differentiable))
;    (help "The chain rule for differentiating products of functions."))
;
;
;(th~defproblem diff-minimum
;    (in calculus)
;    (conclusion 
;     (forall (lam (F (num num))
;      (forall (lam (x num)
;       (implies (and (= (derivative F x) zero)
;                     (greater (derivative (derivative F) x) zero))
;                (local-minimum-at x F)))))))
;    (help "A twice differentiable function function F has a minimum at x
;           if dF=0 and ddf>0"))
;



			     
