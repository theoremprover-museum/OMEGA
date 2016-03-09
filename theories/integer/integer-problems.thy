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



;Already in integer-theorems! MP
;
;(th~defproblem neutral-of-Nat
;           (in integer)
;           (conclusion
;            (= zero (struct-neut natplus)))
;           (help "The constant zero, is the neutral element of NatPlus."))
;
;(th~defproblem no-fix-succ 
;              (in integer)
;              (conclusion  (forall (lam (X num) 
;                                           (implies (nat x)
;                                                    (not (= (s X) X))))))
;              (help "The successor function has no fixed point."))
;
;(th~defproblem assoc-plus-Nat
;               (in integer)
;               (conclusion  (assoc nat plus)))
;
;(th~defproblem assoc-times-Nat
;               (in integer)
;               (conclusion  (assoc nat times)))
;
;(th~defproblem commutative-plus-Nat
;               (in integer)
;               (conclusion  (commutative nat plus)))
;
;(th~defproblem commutative-times-Nat
;               (in integer)
;               (conclusion  (commutative nat times)))
;
;(th~defproblem closed-plus-Nat
;               (in integer)
;               (conclusion  (closed-under nat plus)))
;
;(th~defproblem closed-times-Nat
;               (in integer)
;               (conclusion  (closed-under nat times)))
;
;(th~defproblem semigroup-NatPlus
;               (in integer)
;               (conclusion  (semigroup NatPlus)))
;
;(th~defproblem semigroup-times-Nat
;               (in integer)
;               (conclusion  (semigroup NatTimes)))
;
;(th~defproblem monoid-NatPlus
;               (in integer)
;               (conclusion  (monoid NatPlus)))
;
;(th~defproblem monoid-times-Nat
;               (in integer)
;               (conclusion  (monoid NatTimes)))
;
;(th~defproblem neutral-of-Int
;           (in integer)
;           (formula
;            (= zero (neutral-of int plus)))
;           (help "The constant zero, defined as the neutral element of the group of Integres."))
;
;(th~defproblem assoc-plus-Int
;               (in integer)
;               (conclusion  (assoc int plus)))
;
;(th~defproblem assoc-times-Int
;               (in integer)
;               (conclusion  (assoc int times)))
;
;(th~defproblem commutative-plus-Int
;               (in integer)
;               (conclusion  (commutative int plus)))
;
;(th~defproblem commutative-times-Int
;               (in integer)
;               (conclusion  (commutative int times)))
;
;(th~defproblem closed-plus-Int
;               (in integer)
;               (conclusion  (closed-under int plus)))
;
;(th~defproblem closed-times-Int
;               (in integer)
;               (conclusion  (closed-under int times)))
;
;(th~defproblem semigroup-plus-Int
;               (in integer)
;               (conclusion  (semigroup int plus)))
;
;(th~defproblem semigroup-times-Int
;               (in integer)
;               (conclusion  (semigroup int times)))
;
;(th~defproblem neutral-plus-Int
;               (in integer)
;               (conclusion  (neutral-in int plus zero)))
;
;(th~defproblem neutral-times-Int
;               (in integer)
;               (conclusion  (neutral-in int times (succ zero))))
;
;(th~defproblem monoid-plus-Int
;               (in integer)
;               (conclusion  (monoid int plus)))
;
;(th~defproblem monoid-times-Int
;               (in integer)
;               (conclusion  (monoid int times)))
;
;(th~defproblem inverse-plus-int
;               (in integer)
;               (conclusion  (inverse-in nat plus zero neg)))
;
;(th~defproblem group-plus-int
;               (in integer)
;               (conclusion  (group nat plus zero neg)))
;
;
;(th~defproblem neg-zero
;             (in integer)
;             (conclusion (= (neg zero) zero))
;             (help "Zero is the negative of zero."))
;
;(th~defproblem neg-nilpotent
;             (in integer)
;             (conclusion (nilpotent int neg))
;             (help "Zero is a natural number."))
;
;
;(th~defproblem div-mod-ass
;               (in integer)
;               (conclusion div-mod-thm
;                (forall (lam (x num)
;                 (forall (lam (y num)
;                  (= x (plus (times (div x y) y) (mod x y))))))))
;               (help "  x = (x div y) * y) + (x mod y)."))


(th~defproblem 1-integer
	       (in integer)
	       (conclusion c (=
			      (minus (power 3 2) (minus (times 1 1) (power -3 2)))
			      (plus (minus -4 -5) (times (power 2 3) (plus -3 5))))))

(th~defproblem 2-integer
	       (in integer)
	       (conclusion c (less
			      (lcm 4 6)
			      (times 6 4))))

(th~defproblem 3-integer
	       (in integer)
	       (conclusion c (leq
			      (gcd -12 8)
			      (gcd 8 -4))))

(th~defproblem 4-integer
	       (in integer)
	       (conclusion c (greater
			      (div 7 3)
			      (mod 7 3))))

(th~defproblem 5-integer
	       (in integer)
	       (conclusion c (geq
			      (times -3 -2)
			      (plus (div 11 3) (mod 11 3)))))

(th~defproblem 6-integer
	       (in integer)
	       (conclusion c (in (minus 3 10) int)))



 
(th~defproblem cas-test1a
	 (in integer)
         (author chris)
         (reference "Test")
	 (constants (nice-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption dummy (nice-numbers (union a b)))
	 (assumption ass1 (nice-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (nice-numbers (intersection
					 (lam (x num) (less x (plus 20 20)))
					 (lam (x num) (greater x (times 5 (plus 2 3))))))))

(th~defproblem cas-test1b
	 (in integer)
         (author chris)
         (reference "Test")
	 (constants (favourite-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption ass1 (favourite-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (favourite-numbers (lam (x num) (and (less x (plus 20 20))
					                       (greater x (times 5 (plus 2 3))))))))		



(th~defproblem cas-test1c
	 (in integer)
         (author chris)
         (reference "Test")
	 (conclusion conc (= (lam (x num) (and (greater x (gcd 10 8)) (less x (lcm 10
										   8))))
			     (intersection (lam (x num) (less x 40)) (lam (x num) (greater
										   x 2))))))

(th~defproblem cas-test2
	 (in integer)
	 (constants (ableit ((num num) (num num))) (f (num num)) (g (num num)) (log (num num)))
         (assumption ass1 (forall (lam (f (num num))
	                   (forall (lam (g (num num))
	                    (implies 
	                     (and (= (ableit f) (ableit g))
                                  (exists (lam (x num) (= (f x) (g x)))))
                            (= f g)))))))	
	 (conclusion conc (forall (lam (n num) 
                            (= (lam (x num) (times n (log x)))
                               (lam (x num) (log (power x n))))))))

