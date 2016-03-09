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

(th~deftheory INTEGER
	      (uses natural)
	      (help "Peano Arithmetic for integers."))


(th~defconstant int-struct 
  (in integer)
  (type (struct num))
  (help "The structure of integers together with addition as an operation"))

(th~defconstant int-mul-struct 
  (in integer)
  (type (struct num))
  (help "The structure of integers together with multiplication as an operation"))

(th~defdef int
	   (in integer)
	   (sort)
           (definition (union nat nnat))
	   (help "The set of integers, constructed as the naturals and their
         	  negatives."))

(th~defdef int\0
	   (in integer)
	   (sort)
           (definition (setminus int (singleton zero)))
	   (help "The set of integers without 0."))

#+sorts(th~deftheorem int-sortlike
    (in integer)
    (conclusion conc (forall-sort (lam (x num) (or (int x) (not (int x)))) defined)))
#+sorts(th~defsort int
    (in integer)
    (by int-sortlike))

;(th~defaxiom pred-succ
;             (in integer)
;             (formula (forall-sort (lam (x num)
;                       (= x (p (s x))))  int))
;             (help "The predecessor of the successor of x is x."))
;
;(th~defaxiom succ-pred
;             (in integer)
;             (formula (forall-sort (lam (x num) (= x (s (p x))))  int))
;             (help "The predecessor of the successor of x is x."))


(th~defaxiom nat-int
             (in integer)
             (formula
	      (forall-sort
	       (lam (x num)
		    (int x))
	       nat))
	     (termdecl)
	     (help "A natural number is whole-numbered."))

(th~defaxiom nnat-int
             (in integer)
             (formula
	      (forall-sort
	       (lam (x num)
		    (int x))
	       nnat))
	     (help "A nonpositive integer is whole-numbered."))

(th~defaxiom sp-int
	     (in integer)
	     (formula
	      (forall-sort
	       (lam (x num)
		    (= (s (p x)) x))
	       int))
	     (help "The successor of the predecessor of a number equals the number itself."))

(th~defaxiom ps-int
	     (in integer)
	     (formula
	      (forall-sort
	       (lam (x num)
		    (= (p (s x)) x))
	       int))
	     (help "The successor of the predecessor of a number equals the number itself."))


(th~defdef minus
	   (in integer)
           (definition 
             (lam (x num)
                  (lam (y num)
                       (plus x (change-sign y)))))
	   (help "The difference operators on natural numbers."))

(th~defaxiom plus-int
	     (in integer)
	     (formula
	      (forall-sort (lam (x num)
	       (forall-sort (lam (y num)
		(and (= (plus (change-sign x) (change-sign y))
			 (change-sign (plus x y)))
		     (and (= (plus x y)
			     (that (lam (z num)
					(= x (plus y z)))))
			  (= (plus x y)
			     (that (lam (z num)
					(= y (plus x z))))))))  nnat))  nnat))
	     (help "Extension of plus to the integers."))

(th~defaxiom times-int
	     (in integer)
	     (formula
	      (forall-sort (lam (x num)
	       (forall-sort (lam (y num)
		(= (times (change-sign x) y) (change-sign (times x y))))  int))  int))
	     (help "Extension of multiplication to the integers."))



(th~defaxiom int-mul-struct
	     (in integer)
	     (formula
	      (and (= (struct-set int-mul-struct) Int)
		   (= (struct-op int-mul-struct) times)))
	     (help "The monoid of integers with operation times."))



(th~defaxiom int-struct
	     (in integer)
	     (formula
	      (and (and (= (struct-set int-struct) Int)
			(= (struct-op int-struct) plus))
		   (and (= (struct-mul-sgroup int-struct) Int-mul-struct)
			(= (struct-ordering int-struct) leq))))
	     (help "The ordered ring of integers with operation plus."))

(th~defdef div
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (that (lam (z num)
                                  (and (leq (times z y) x)
                                       (greater (times (s z) y) x)))))))
	   (help "The div operator for natural numbers."))


;; put this into divi/prime later

(th~defdef divisor
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                    (and
                      (and (int x)
                           (int y))
                      (exists-sort (lam (z num)
					     (= y (times x z))) int)))))
	   (help "The predicate for integer divisibility."))

(th~defdef common-divisor
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (lam (z num)
                            (and (and (int x) (int y))
                                 (and (int z)
                                      (and (not (= 1 z))
					   (and (divisor z x)
						(divisor z y)))))))))
	   (help "The predicate for non-trivial common integer divisibility."))


(th~defdef prime
           (in integer)
           (definition
             (lam (p (num))
               (and (and (int p) (greater p 1))
                    (forall-sort (lam (n num)
                      (and
                        (greater n 0)
                        (implies
                          (divisor n p)
                          (or (= n 1) (= n p))
                        )
                      )
                    ) int )
                )
              )
            )
	   (help "Definition of a prime number."))            



(th~defdef prime-divisor
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                    (and
		     (divisor x y)
		     (prime x)))))
	   (help "The predicate for prime divisors."))


(th~defdef common-multiple
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (lam (z num)
                            (and (and (in x int) (in y int))
                                 (and (in z int)
                                      (and (divisor x z)
                                           (divisor y z))))))))
           (help "The predicate for common integer divisibility."))

(th~defdef gcd
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (maximum int-struct (lam (z num)
                                                (common-divisor z x y ))))))
	   (help "The predicate for common integer divisibility."))

(th~defdef lcm
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (minimum int-struct (lam (z num)
                                                (common-multiple z x y ))))))
           (help "The predicate for common integer divisibility."))


(th~defdef mod
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (that (lam (z num)
                                  (= x (plus z (times y (div x y)))))))))
           (help "The mod operator for natural numbers."))


(th~defdef pminus
	   (in integer)
           (definition
             (apply-pointwise-2 minus))
	   (help "The definition of pointwise subtraction of functions."))


(th~defdef integer-intervall
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (lam (elem num)
                                  (and (int elem)
				       (and (leq x elem)
					    (leq elem y)))))))
           (help "The set of all integers in the closed intervall from x to y."))

;;;;; To do:  even and odd
;;;;; 


(th~defdef evenp
	   (in integer)
	   (definition
	     (lam (x  num)
		  (exists-sort (lam (y num)
				    (= x (times 2 y))) int)))
	   (help "Definition of even."))

(th~defdef common-divisor-p
	   (in integer)
	   (definition
	     (lam (x  num)
		  (lam (y num)
		       (exists-sort (lam (z num)
					 (common-divisor x y z)) int))))
	   (help "Definition of the property of having a common divisor."))

