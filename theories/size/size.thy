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

(th~deftheory SIZE
	      (uses polynomial )
	      (constants (sizeunit-conversion (o (num num)))
			 (sizeunit-factor (o num num num))
			 (sizeunit (o num))
			 (total-minimum-size (o (o num) (num num) num))
			 (sf-fun (num num (num num))))
	      (help "A simple theory of Sizes and Units,
                     where units are treated as special, user-specified numbers.
                     Sizes are products of real numbers with units,
                     i.e. special numbers."))


(th~defaxiom sizeunit-product
	     (in size)
	     (formula
	      (forall (lam (S num)
	       (forall (lam (T num)
		(implies (and (sizeunit S) (sizeunit T))
			 (sizeunit (times S T))))))))
	     (help "Products of units are units."))


(th~defaxiom sizeunit-fraction
	     (in size)
	     (formula
	      (forall (lam (S num)
	       (forall (lam (T num)
		(implies (and (sizeunit S) (sizeunit T))
			 (sizeunit (frac S T))))))))
	     (help "Quotients of units are units."))

(th~defdef size
	   (in size)
           (definition
             (lam (S num)
                  (exists (lam (y num)
                               (exists (lam (u num)
                                            (and (and (real y) (sizeunit u))
                                                 (= S (times y u)))))))))
	   (help "A number is a size, if it is the product of a real number and a unit."))

(th~defaxiom one-sizeunit
	     (in size)
	     (formula
	       (sizeunit one))
	     (help "One is a unit."))



(th~defaxiom one-and-times
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (implies (sizeunit u) (= (times one u) u)))))
	     (help "One is the neutral element of unit and times."))



(th~defaxiom one-and-frac
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (implies (sizeunit u) (= (frac u one) u)))))
	     (help "One is the neutral element of unit and frac."))


(th~defaxiom double-frac
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (implies (sizeunit u) (= (frac one (frac one u)) u)))))
	     (help "Reducing units by cancellation."))


(th~defaxiom sizeunit-reduction
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (implies (sizeunit u) (= (frac u u) one)))))
	     (help "Reducing units by cancellation."))

;; These axioms together with unit-times-and-frac defines the times-inverse 
;; of a unit as (frac one u), and so does the following:

(th~defdef inverse-sizeunit
	   (in size)
           (definition
             (lam (u num)
                  (frac one u)))
	   (help "The inverse of a unit."))



(th~defaxiom sizeunit-times-commutative
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (forall (lam (v num)
                   (implies (and (sizeunit u) (sizeunit v))
                     (= (times u v) (times v u))))))))
	     (help "Times is commutative on units."))



(th~defaxiom sizeunit-times-and-frac
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (forall (lam (v num)
                   (forall (lam (w num)
                     (forall (lam (x num)
                       (implies (and (and (sizeunit u) (sizeunit v))
				     (and (sizeunit w) (sizeunit x)))
                         (= (times (frac u w) (frac v x))
			    (frac (times u v) (times w x)))))))))))))
	     (help "Using frac and times together."))


(th~defaxiom sizeunit-double-frac
	     (in size)
	     (formula
	       (forall (lam (u num)
                 (forall (lam (v num)
                   (forall (lam (w num)
                       (implies (and (and (sizeunit u) (sizeunit v))
				     (sizeunit w))
                         (= (frac u (frac v w))
			    (times u (frac w v)))))))))))
	     (help "Dividing by a fraction."))



(th~defaxiom size-plus
	     (in size)
	     (formula
	       (forall (lam (a num)
                 (forall (lam (b num)
                   (forall (lam (u num)
                     (implies (and (sizeunit u)
				   (and (real a) (real b)))
                     (= (plus (times a u) (times b u))
			(times (plus a b) u))))))))))
	     (help "Adding two sizes."))


;; MiKo: Dies scheint mir eine konsequenz der Assoziativit"at und
;;       Kommutativit"at zu sein
(th~defaxiom size-times
	     (in size)
	     (formula
	       (forall (lam (a num)
                 (forall (lam (b num)
                   (forall (lam (u num)
                     (forall (lam (v num)
                     (implies (and (and (sizeunit u) (sizeunit v))
				   (and (real a) (real b)))
                     (= (times (times a u) (times b u)) 
                        (times (times a b) (times u v)))))))))))))
	     (help "Multiplying two sizes."))

;; MiKo: Dies auch
(th~defaxiom size-frac
	     (in size)
	     (formula
	       (forall (lam (a num)
                 (forall (lam (b num)
                   (forall (lam (u num)
                     (forall (lam (v num)
                     (implies (and (and (sizeunit u) (sizeunit v))
				   (and (real a) (real b)))
                     (= (frac (times a u) (times b u)) 
                        (times (frac a b) (frac u v)))))))))))))
	     (help "Dividing two sizes."))


(th~defdef size-function
	   (in size)
           (definition
             (lam (f (num num))
                  (lam (v num)
                       (lam (u num)
                            (lam (x num)
                                 (times (f (times x (inverse-sizeunit u))) v))))))
	   (help "Constructs a size function f': u -> v, f'(x) = f(x/u) * v."))

(th~defdef size-set
	   (in size)
           (definition
             (lam (theset (o num))
                  (lam (u num)
                       (lam (x num)
                            (theset (times x (inverse-sizeunit u)))))))
	   (help "Constructs a size set S' from a set S of numbers, where x in S; iff  (x/u) in S."))


(th~defdef compatible-sizes
  (in size)
  (definition
    (lam (s1 num)
         (lam (s2 num)
              (and (and (size s1) (size s2))
                   (exists (lam (r num)
                                (and (real r) (= s1 (times r s2)))))))))
  (help "Two sizes are compatible, iff they are real multiples."))

(th~defdef size-coefficient
	   (in size)
           (definition
             (lam (sz num)
                  (lam (u num)
                       (that (lam (z num)
                                  (and (= sz (times z u))
                                       (and (real z) (sizeunit u))))))))
           (help "The numerical part of a size, given a unit."))

#|
(th~defdef ss-set
	   (in size)
           (definition
             (lam (szs (o num))
                  (lam (u num)
                       (lam (z num)
                            (exists (lam (w num)
                                         (and (szs w)
                                              (= z (size-coefficient w u)))))))))
	   (help "The numeric part of a set of sizes given a unit."))

(th~defaxiom sf-fun
	   (in size)
	   (formula
	    (forall (lam (f (num num))
	     (forall (lam (u num)
	      (forall (lam (v num)
	       (= (sf-fun (size-function f u v)) f))))))))		  
	    (help "The numeric part of a size function."))
|#


(th~defaxiom conversion-axiom
	     (in size)
	     (formula
	      (forall (lam (f (num num))
			   (forall (lam (s num)
					(implies (sizeunit-conversion f) (= s (f s))))))))
	     (help "A size s and its equivalent defined by a price-function
                  f are exchangeable."))

(th~defaxiom size-factor-axiom
	     (in size)
	     (formula
	      (forall (lam (f num)
	       (forall (lam (u num)
	        (forall (lam (v num)
		(implies (sizeunit-factor f u v) (= v (times f u))))))))))
	     (help "if (sizeunit-factor f u v), then v = f*u."))

;; should be a theorem MiKo
(th~defaxiom sf-factor-axiom
	     (in size)
	     (formula
	      (forall (lam (f num)
	       (forall (lam (fun (num num))
		(forall (lam (u num)
		 (forall (lam (v num)
		  (forall (lam (w num)
		   (implies (sizeunit-factor f w v)
			    (= (size-function fun v u)
			       (size-function (s-times-r1 fun f) w u))))))))))))))
	     (help "A size function can obtain another unit by s-multiplication."))


(th~defaxiom sf-addition
	     (in size)
	     (formula
	      (forall (lam (f (num num))
	       (forall (lam (g (num num))
		(forall (lam (u num)
		 (forall (lam (v num)
			 (= (p-plus-r1 (size-function f u v) (size-function g u v))
			    (size-function (p-plus-r1 f g) u v)))))))))))
	     (help "Addition of size functions with equal denomination"))

(th~defaxiom total-minimum-size
	     (in size)
	     (formula
	      (forall (lam (F (num num))
	       (forall (lam (U num)
	        (forall (lam (V num)
	         (forall (lam (G (o num))
		  (forall (lam (H (o num))	        
		   (forall (lam (x num)
		    (implies (forall (lam (z num)
			      (= (G z) (H (times z u)))))
			     (= (total-minimum X F G)
				(total-minimum-size
				 (TIMES X V)
				 (size-function F u v)
				 H))))))))))))))))
	     (help "The total minimum of a size function is the total minimum
                    of its real part."))

