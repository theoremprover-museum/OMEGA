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

(th~deftheorem rat-crit
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies
			     (and (less (s zero) y)
				  (= (gcd x y)
				     (s zero)))
			     (rat (frac x y))))
		       nat))
		 int))
	       (help "A criterion for a number being rational."))

(th~deftheorem cancel-fraction
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (forall-sort
			     (lam (z num)
				  (implies
				   (and (or (less zero z)
					    (less z zero))
					(less zero y))
				   (= (frac (times x z)
					    (times y z))
				      (frac x y))))
			     int))
		       nat))
		 int))
	       (help "A theorem for cancelling fractions."))

(th~deftheorem numerator-equals-zero
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (implies (less zero x)
			       (= (frac zero x)
				  zero)))
		 nat))
	       (help "A rational number with numerator zero is the integer zero."))
			 
(th~deftheorem int-to-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= x (frac x (s zero))))
		 int))
	       (help "Conversion of integers to fractions."))

(th~deftheorem rat-to-int
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (frac x (s zero)) x))
		 int))
	       (help "Conversion of fractions with denominator one to integers."))

(th~deftheorem numerator-of-frac
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies
			     (rat (frac x y))
			     (= (numerator (frac x y)) x)))
		       nat))
		 int))
	       (help "Extraction of the numerator of a rational number."))

(th~deftheorem denominator-of-frac
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies
			     (rat (frac x y))
			     (= (denominator (frac x y)) y)))
		       nat))
		 int))
	       (help "Extraction of the denominator of a rational number."))

(th~deftheorem numerator-of-int
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (numerator x)
			 x))
		 int))
	       (help "The numerator of an whole-numbered number is the number itself."))

(th~deftheorem denominator-of-int
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (denominator x)
			 (s zero)))
		 int))
	       (help "The denominator of an whole-numbered number is one."))

(th~deftheorem plus-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (plus x y)
			       (frac (plus (times (numerator x) (denominator y))
					   (times (numerator y) (denominator x)))
				     (times (denominator x) (denominator y)))))
		       rat))
		 rat))
	       (help "Brute force addition on rational numbers."))

(th~deftheorem plus-rat-equal-denoms
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies
			     (= (denominator x) (denominator y))
			     (= (plus x y)
				(frac (plus (numerator x) (numerator y))
				      (denominator x)))))
		       rat))
		 rat))
	       (help "Addition on rational numbers with equal denominators"))

(th~deftheorem plus-rat-expanded-fracs
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (forall-sort
			     (lam (z num)
				  (implies (less zero z)
					   (= (plus (frac x z)
						    (frac y z))
					      (frac (plus x y)
						    z))))
			     nat))
		       int))
		 int))
	       (help "Addition of expanded fractions."))

(th~deftheorem change-sign-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (change-sign x)
			 (frac (change-sign (numerator x))
			       (denominator x))))
		 rat))
	       (help "Unary minus on rational numbers."))

(th~deftheorem times-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (times x y)
			       (frac (times (numerator x) (numerator y))
				     (times (denominator x) (denominator y)))))
		       rat))
		 rat))
	       (help "Multiplication on rational numbers."))

(th~deftheorem power-rat-nat
               (in rational)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (forall-sort
                       (lam (y num)
                            (= (power x y)
			       (frac (power (numerator x) y)
				     (power (denominator x) y))))
                       nat))
                 rat))
	       (help "Natural powers of rational numbers."))

(th~deftheorem power-rat-nnat
               (in rational)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (forall-sort
                       (lam (y num)
                            (= (power x y)
			       (one-over (power x (change-sign y)))))
                       nnat))
                 rat))
	       (help "Negative whole-numbered powers of rational numbers."))

(th~deftheorem power-rat-base-one
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (power (s zero) x)
			 (s zero)))
		 int))
	       (help "Powers with base one."))

(th~deftheorem less-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less (times (numerator x) (denominator y))
					   (times (numerator y) (denominator x)))
				     (less x y)))
		       rat))
		 rat))
	       (help "Less on rational numbers."))

(th~deftheorem less-rat-neg-and-pos
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (and (less (numerator x)
						zero)
					  (less zero
						(numerator y)))
				     (less x y)))
		       rat))
		 rat))
	       (help "A negative rational number is smaller than a positive rational number."))

(th~deftheorem less-implies-leq-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less x y) (leq x y)))
		       rat))
		 rat))
	       (help "Less implies less or equal."))

(th~deftheorem equal-implies-leq-rat
	       (in rational)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (= x y) (leq x y)))
		       rat))
		 rat))
	       (help "Equal implies less or equal."))


