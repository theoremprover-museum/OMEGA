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


(th~defproblem mbase-example
	       (in natural)
	       (conclusion plus-comm (and
			      (forall-sort
			       (lam (x num) (= (plus x zero) (plus zero x))) nat)
			      (forall-sort 
			       (lam (x num)
				    (forall-sort
				     (lam (y num)
					  (implies
					   (= (plus x y) (plus y x))
					   (= (plus x (s y)) (plus (s y) x))))
					  nat)) nat))))



(th~defproblem 1-natural
	       (in natural)
	       (conclusion c (=
			      (plus (plus 2 (times 8 0)) (plus (times 3 1) (power 1 7)))
			      (plus (power 0 (times (power 3 2) 2)) (plus 2 (times 1 4))))))

(th~defproblem 2-natural
	       (in natural)
	       (conclusion c (less
			      (power 2 3)
			      (power 3 2))))

(th~defproblem 3-natural
	       (in natural)
	       (conclusion c (leq
			      (plus 3 4)
			      (plus 3 (times 2 2)))))

(th~defproblem 4-natural
	       (in natural)
	       (conclusion c (greater
			      (times 3 3)
			      (plus 3 3))))

(th~defproblem 5-natural
	       (in natural)
	       (conclusion c (geq
			      (plus 7 3)
			      (times 2 5))))

(th~defproblem 6-natural
	       (in natural)
	       (conclusion c (in (plus 3 4) nat)))

;;; lclams signature:
;;;has_otype arithmetic zero nat.
;;;has_otype arithmetic s (nat arrow nat).
;;;has_otype arithmetic plus ((tuple_type [nat, nat]) arrow nat).
;;;has_otype arithmetic minus ((tuple_type [nat, nat]) arrow nat).
;;;has_otype arithmetic otimes ((tuple_type [nat, nat]) arrow nat).
;;;has_otype arithmetic exp ((tuple_type [nat, nat]) arrow nat).
;;;has_otype arithmetic leq ((tuple_type [nat, nat]) arrow bool).
;;;has_otype arithmetic half (nat arrow nat).
;;;has_otype arithmetic double (nat arrow nat).
;;;has_otype arithmetic even (nat arrow bool).

;;;===================================
;;;  some LClam problems translated into POST, J.Zimmer Nov.2001
;;;===================================
(th~defproblem assp
               (in natural)
               (conclusion associativity
                           (forall (lam (x num)
                           (forall (lam (y num)
                           (forall (lam (z num)
                                   (= (plus (plus x y) z)
                                      (plus x (plus y z)))))))))))

(th~defproblem dist
               (in natural)
               (conclusion distributivity
                           (forall (lam (x num)
                           (forall (lam (y num)
                           (forall (lam (z num)
                                   (= (times x (plus y z))
                                      (plus (times x y) (times x z)))))))))))

(th~defproblem Times2Right
               (in natural)
               (conclusion Times2Right
                           (forall (lam (x num)
                           (forall (lam (y num)
                                   (= (times x (s y))
                                      (plus x (times x y)))))))))

