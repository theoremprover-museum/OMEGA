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

(th~deftheory RATIONAL
	      (uses integer)
	      (help "Peano Arithmetic for rationals."))

(th~defconstant frac
		(in rational)
		(type (num num num))
		(help "The fraction constructor for rational numbers"))

(th~defconstant rat-struct
		(in rational)
		(type (struct num))
		(help "The structure of rational numbers with addition as operation"))

(th~defconstant rat-mul-struct
		(in rational)
		(type (struct num))
		(help "The structure of non-zero rational numbers with multiplication as operation"))

(th~defdef rat
	   (in rational)
           (definition
             (lam (x num)
		  (exists-sort (lam (y num)
				    (exists-sort (lam (z num)
						      (= x (frac y z))) int\0)) int)))
	   (sort)
	   (help "The set of rationals, constructed as fractions a/b of integers."))

(th~defaxiom reduce-frac
	     (in rational)
	     (formula
	      (forall-sort (lam (x num)
	       (forall-sort (lam (y num)
		(forall-sort (lam (z num)
		 (implies (not (= z zero))
			  (= (frac (times x z)
				   (times y z))
			     (frac x y))))  rat))  rat))  rat))
	     (help "Reducing fractions by cancellation."))

(th~defdef numerator
	   (in rational)
           (definition 
	     (lam (x num)
		  (that (lam (y num)
			     (exists (lam (z num)
			      (= x (frac y z))))))))
	   (help "The numerator of a fraction x/y is x."))


(th~defdef denominator
	   (in rational)
           (definition 
	     (lam (x num)
		  (that (lam (y num)
			     (exists (lam (z num)
					  (= x (frac z y))))))))
	   (help "The numerator of a fraction x/y is y."))

  
;(th~defdef divide
;           (in rational)
;           (definition 
;             (lam (x num)
;                  (lam (y num)
;                       (frac (times (numerator x) (denominator y))
;                             (times (denominator x) (numerator y))))))
;           (help "The division operator of the rationals."))
;                                  
;(th~defdef one-over
;           (in rational)
;           (definition 
;             (lam (x num)
;                  (divide one x)))
;           (help "The multiplicative inversion operator of the rationals."))


(th~defdef one-over
	   (in rational)
	   (definition
	     (lam (x num)
		  (frac (denominator x)
			(numerator x))))
	   (help "The reciprocal value of a fraction."))

(th~defdef divide
	   (in rational)
	   (definition
	     (lam (x num)
		  (lam (y num)
		       (times x (one-over y)))))
	   (help "The division operator of the rational numbers."))


(th~defaxiom plus-frac
	     (in rational)
	     (formula
	      (forall-sort (lam (x num)
	       (forall-sort (lam (y num)
		(= (plus x y)
		   (frac (plus (times (numerator x) (denominator y))
			       (times (numerator y) (denominator x)))
			 (times (denominator x) (denominator y)))))  rat))  rat))
	     (help "The axiom for plus on the rationals."))



(th~defaxiom times-frac
	     (in rational)
	     (formula
	      (forall-sort (lam (x num)
	       (forall-sort (lam (y num)
		(= (times x y)
		   (frac (times (numerator x) (numerator y))
			 (times (denominator x) (denominator y))))) rat))  rat))
	     (help "The axiom for times on the rationals."))


(th~defaxiom rat-mul-struct
	     (in rational)
	     (formula
	      (and (= (struct-set rat-mul-struct) Rat)
		   (= (struct-op rat-mul-struct) times)))
	     (help "The group of Rationals with operation times."))



(th~defaxiom rat-struct
	     (in rational)
	     (formula
	      (and (and (= (struct-set rat-struct) Rat)
			(= (struct-op rat-struct) plus))
		   (and (= (struct-mul-sgroup rat-struct) Rat-mul-struct)
			(= (struct-ordering rat-struct) leq))))
	     (help "The ordered field of rationals with operation plus."))

(th~defdef pdivide
	   (in rational)
           (definition
	    (apply-pointwise-2 divide))
	   (help "The definition of pointwise addition of functions."))


(th~defaxiom int-rat
	     (in rational)
	     (formula
	      (forall-sort
	       (lam (x num)
		    (rat x))
	       int))
	     (help "An integer is rational."))
