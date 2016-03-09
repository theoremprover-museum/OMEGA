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
(th~deftheorem neutral-of-Nat
	   (in integer)
	   (conclusion
	    (= zero (struct-neut nat-plus-struct)))
	   (help "The constant zero, is the neutral element of NatPlus."))

(th~defsimplifier neutral-of-Nat-simp
		  (in integer)
		  (status global)
                  (equation neutral-of-Nat)
                  (direction rl)
                  (help "Simplify the neutral of the nat-struct."))

(th~deftheorem no-fix-succ 
	      (in integer)
	      (conclusion THM (forall (lam (X num) 
					   (implies (nat x)
						    (not (= (s X) X))))))
	      (help "The successor function has no fixed point."))

(th~deftheorem assoc-plus-Nat
	       (in integer)
	       (conclusion THM (associative nat plus)))

(th~deftheorem assoc-times-Nat
	       (in integer)
	       (conclusion THM (associative nat times)))

(th~deftheorem commutative-plus-Nat
	       (in integer)
	       (conclusion THM (commutative nat plus)))

(th~deftheorem commutative-times-Nat
	       (in integer)
	       (conclusion THM (commutative nat times)))

(th~deftheorem closed-plus-Nat
	       (in integer)
	       (conclusion THM (closed-under-2 nat plus)))

(th~deftheorem closed-times-Nat
	       (in integer)
	       (conclusion THM (closed-under-2 nat times)))

;
;(th~deftheorem semigroup-NatPlus
;               (in integer)
;               (conclusion THM (semigroup Nat-Plus-struct)))
;
;(th~deftheorem semigroup-times-Nat
;               (in integer)
;               (conclusion THM (semigroup Nat-Times-struct)))
;
;(th~deftheorem monoid-NatPlus
;               (in integer)
;               (conclusion THM (monoid Nat-Plus-struct)))
;
;(th~deftheorem monoid-times-Nat
;               (in integer)
;               (conclusion THM (monoid Nat-Times-struct)))


(th~deftheorem assoc-plus-Int
	       (in integer)
	       (conclusion THM (associative int plus)))

(th~deftheorem assoc-times-Int
	       (in integer)
	       (conclusion THM (associative int times)))

(th~deftheorem commutative-plus-Int
	       (in integer)
	       (conclusion THM (commutative int plus)))

(th~deftheorem commutative-times-Int
	       (in integer)
	       (conclusion THM (commutative int times)))

(th~deftheorem closed-plus-Int
	       (in integer)
	       (conclusion THM (closed-under-2 int plus)))

(th~deftheorem closed-times-Int
	       (in integer)
	       (conclusion THM (closed-under-2 int times)))

;(th~deftheorem semigroup-plus-Int
;               (in integer)
;               (conclusion THM (semigroup int-struct)))
;
;(th~deftheorem semigroup-times-Int
;               (in integer)
;               (conclusion THM (semigroup int-struct)))

(th~deftheorem neutral-plus-Int
	       (in integer)
	       (conclusion THM (unit int plus zero)))

(th~deftheorem neutral-times-Int
	       (in integer)
	       (conclusion THM (unit int times (s zero))))
;(th~deftheorem monoid-plus-Int
;               (in integer)
;               (conclusion THM (monoid int-struct)))
;
;(th~deftheorem monoid-times-Int
;               (in integer)
;               (conclusion THM (monoid int-mul-struct)))
;
;(th~deftheorem inverse-plus-int
;               (in integer)
;               (conclusion THM (inverse-in nat plus zero change-sign)))
;
;(th~deftheorem group-plus-int
;               (in integer)
;               (conclusion THM (group nat-plus-struct)))


(th~deftheorem neg-zero
	     (in integer)
	     (conclusion (= (change-sign zero) zero))
	     (help "Zero is the negative of zero."))


(th~defsimplifier neg-zero
		  (in integer)
		  (status global)
                  (equation neg-zero)
                  (direction lr)
                  (help "Simplify  - 0 to 0."))

(th~deftheorem neg-nilpotent
	     (in integer)
	     (conclusion (nilpotent int change-sign))
	     (help "The function for changing signs of integers is nilpotent."))



(th~deftheorem plus-int-base
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (plus x zero) x))
		 int))
	       (help "The base case for recursive definition of addition."))

(th~deftheorem plus-int-base2
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (plus zero x) x))
		 int))
	       (help "Another base case for recursive definition of addition."))

(th~deftheorem plus-int-step-s
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (plus x (s y)) (s (plus x y))))
		       nat))
		 int))
	       (help "The step case for recursive definition of addition."))

(th~deftheorem plus-int-step-p
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (plus x (p y)) (p (plus x y))))
		       nnat))
		 int))
	       (help "Another step case for recursive definition of addition."))

(th~deftheorem plus-int-step2-s
               (in integer)
               (conclusion
                (forall-sort
                 (lam (y num)
                      (forall-sort
                       (lam (x num)
                            (= (plus (s x) y) (s (plus x y))))
                       nat))
                 int))
	       (help "Another step case for recursive definition of addition."))

(th~deftheorem plus-int-step2-p
               (in integer)
               (conclusion
                (forall-sort
                 (lam (y num)
                      (forall-sort
                       (lam (x num)
                            (= (plus (p x) y) (p (plus x y))))
                       nnat))
                 int))
	       (help "Another step case for recursive definition of addition."))

(th~deftheorem change-sign-base
	       (in integer)
	       (conclusion
		(= (change-sign zero) zero))
	       (help "The base case for recursive definition of unary minus."))

(th~deftheorem change-sign-s
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (change-sign (s x)) (p (change-sign x))))
		 int))
	       (help "The step case for recursive definition of unary minus."))

(th~deftheorem change-sign-p
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (change-sign (p x)) (s (change-sign x))))
		 int))
	       (help "Another step case for recursive definition of unary minus."))

(th~deftheorem change-sign-reverse
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (= (change-sign x) y) (= (change-sign y) x)))
		       int))
		 int))
	       (help "Simplification of unary minus."))

(th~deftheorem times-int-base
               (in integer)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (= (times x zero) zero))
		 int))
	       (help "The base case for recursive definition of multiplication."))

(th~deftheorem times-int-base2
               (in integer)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (= (times zero x) zero))
		 int))
	       (help "Another base case for recursive definition of multiplication."))

(th~deftheorem times-int-step-s
               (in integer)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (times x (s y)) (plus x (times x y))))
		       nat))
		 int))
	       (help "The step case for recursive definition of multiplication."))

(th~deftheorem times-int-step-p
               (in integer)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (times x (p y)) (plus (change-sign x) (times x y))))
		       nnat))
		 int))
	       (help "Another step case for recursive definition of multiplication."))

(th~deftheorem times-int-step2-s
               (in integer)
               (conclusion
                (forall-sort
		 (lam (y num)
		      (forall-sort
		       (lam (x num)
			    (= (times (s x) y) (plus y (times x y))))
		       nat))
		 int))
	       (help "Another step case for recursive definition of multiplication."))

(th~deftheorem times-int-step2-p
               (in integer)
               (conclusion
                (forall-sort
		 (lam (y num)
		      (forall-sort
		       (lam (x num)
			    (= (times (p x) y) (plus (change-sign y) (times x y))))
		       nnat))
		 int))
	       (help "Another step case for recursive definition of multiplication."))

(th~deftheorem div-int
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (forall-sort
			     (lam (z num)
				  (implies (less zero y)
					   (implies (and (leq (times z y) x)
							 (greater (times (plus z (s zero)) y) x))
						    (= (div x y) z))))
			     int))
		       nat))
		 int))
	       (help "Whole-numbered division."))

(th~deftheorem mod-int
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less zero y)
				     (= (mod x y)
					(minus x (times y (div x y))))))
		       nat))
		 int))
	       (help "Residue of whole-numbered division."))

(th~deftheorem power-int-base
               (in integer)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (= (power x zero) (s zero)))
                 int))
	       (help "The base case for recursive definition of exponentiation."))

(th~deftheorem power-int-step
               (in integer)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (forall-sort
                       (lam (y num)
                            (= (power x (s y)) (times x (power x y))))
                       nat))
                 int))
	       (help "The step case for recursive definition of exponentiation."))

(th~deftheorem gcd-left-arg-zero
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (gcd zero x)
			 x))
		 nat))
	       (help "A base case of the Euclidian algorithm."))

(th~deftheorem gcd-right-arg-zero
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (gcd x zero)
			 x))
		 nat))
	       (help "Another base case of the Euclidian algorithm."))

(th~deftheorem gcd-equal-args
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (gcd x x)
			 x))
		 nat))
	       (help "Another base case of the Euclidian algorithm."))

(th~deftheorem gcd-neg-left-arg
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (gcd x y)
			       (gcd (change-sign x) y)))
		       int))
		 nnat))
	       (help "Greatest common divisor with negative arguments."))

(th~deftheorem gcd-neg-right-arg
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (gcd x y)
			       (gcd x (change-sign y))))
		       nnat))
		 int))
	       (help "Greatest common divisor with negative arguments."))

(th~deftheorem gcd-diff-1
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (gcd x y)
			       (gcd (minus x y) y)))
		       nat))
		 nat))
	       (help "The step case of the Euclidian algorithm."))

(th~deftheorem gcd-diff-2
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (gcd x y)
			       (gcd x (minus y x))))
		       nat))
		 nat))
	       (help "Another step case of the Euclidian algorithm."))

(th~deftheorem lcm-left-arg-zero
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (lcm zero x)
			 zero))
		 int))
	       (help "A base case for the least common multiple."))

(th~deftheorem lcm-right-arg-zero
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (lcm x zero)
			 zero))
		 int))
	       (help "Another base case for the least common multiple."))

(th~deftheorem lcm-equal-args
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (lcm x x)
			 x))
		 nat))
	       (help "Another base case for the least common multiple."))

(th~deftheorem lcm-neg-left-arg
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (lcm x y)
			       (lcm (change-sign x) y)))
		       int))
		 nnat))
	       (help "Least common multiple with negative arguments."))

(th~deftheorem lcm-neg-right-arg
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (lcm x y)
			       (lcm x (change-sign y))))
		       nnat))
		 int))
	       (help "Least common multiple with negative arguments."))

(th~deftheorem lcm-by-gcd
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (or (less zero x)
					 (less zero y))
				     (= (lcm x y)
					(div (times x y)
					     (gcd x y)))))
		       nat))
		 nat))
	       (help "The least common multiple by the greatest common divisor."))

(th~deftheorem less-nnat-base
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (less (p x) zero))
		 nnat))
	       (help "The base case for recursive definition of less."))

(th~deftheorem less-nnat-step
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less x y) (less (p x) (p y))))
		       nnat))
		 nnat))
	       (help "The step case for recursive definition of less."))

(th~deftheorem less-implies-leq-int
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less x y) (leq x y)))
		       int))
		 int))
	       (help "Less implies less or equal."))

(th~deftheorem equal-implies-leq-int
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (= x y) (leq x y)))
		       int))
		 int))
	       (help "Equal implies less or equal."))

(th~deftheorem neg-less-pos-int
	       (in integer)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (less (p x) (s y)))
		       nat))
		 nnat))
	       (help "A negative integer is smaller than a positive integer."))

(th~deftheorem power-closed-int
	       (in integer)
	       (conclusion (forall-sort (lam (y num)
					     (forall-sort (lam (x num)
							       (int (power x y)))
							  int))
					nat))
	       (help "The power is closed on integers."))

(th~deftheorem even-on-integers
	       (in integer)	
	       (conclusion
		(forall-sort (lam (x num)
			     (equiv (evenp x)
				    (exists-sort (lam (y num)
						      (= x (times 2 y))) int))) int))
	       (help "An integer x is even, iff an integer y exists so that x=2*x."))

(th~deftheorem square-even
	       (in integer)	
	       (conclusion
		(forall-sort (lam (x num)
			     (equiv (evenp (power x 2))
				    (evenp x))) int))
	       (help "x is even, iff x^2 is even."))



(th~deftheorem even-common-divisor  
	       (in integer)	
	       (conclusion
		(forall-sort (lam (x num)
				  (forall-sort (lam (y num)
						    (implies (and (evenp x)
								  (evenp y))
							     (common-divisor x y 2))) int)) int))
	       (help "If x and y are even, then they have a common divisor."))


(th~deftheorem power-int-CLOSED 
(in integer)
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (INT (power Y X)))  INT))  INT))
(termdecl)
(help "The set of integers is closed under power."))

(th~deftheorem plus-int-closed-td
(in integer)
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (INT (plus Y X)))  INT))  INT))
(termdecl)
(help "The set of integers is closed under plus."))

(th~deftheorem times-int-closed-td
(in integer)
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (INT (times Y X)))  INT))  INT))
(termdecl)
(help "The set of integers is closed under times."))




(th~deftheorem PRIME-DIVISOR-POWER
	       (in integer) 
	       (conclusion (forall-sort (lam (n num)
					     (forall-sort (lam (d num)
							       (forall-sort (lam (x num)
										 (implies (prime-divisor d (power x n))
											  (prime-divisor d x)))
									    INT))
							  NAT))
					NAT)))

(th~deftheorem PRIME-DIV-INHERIT
	       (in integer) 
	       (conclusion (forall-sort (lam (x num)
					     (forall-sort (lam (y num)
							       (implies (prime-divisor x y)
									(exists-sort (lam (z num)
											  (and (= y (times x z))
											       (forall-sort (lam (u num)
														 (implies (and (prime-divisor u y)
															       (not (= u x)))
															  (prime-divisor u z)))
													    int)))
										     int)))
							  int))
					int)))
