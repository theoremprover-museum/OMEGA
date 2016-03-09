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

(th~deftheory REAL
	      (uses rational sequences)
	      (constants (completion ((struct num)(struct num))))
	      (help "Peano Arithmetic for real numbers."))

(th~defconstant real
	   (in real)
	   (type (o num))
	   (sort))

(th~defdef real\0
	   (in real)
	   (sort)
           (definition (setminus real (singleton zero)))
	   (help "The set of reals without 0."))


(th~defdef real-struct
	   (in real)
           (definition
	    (completion rat-struct))
	   (help "The real numbers, defined as the completion of the rational
                  numbers."))

(th~defaxiom real-plus-closed
	     (in real)
	     (formula (closed-under real plus))
	     (help "Plus is closed."))

(th~defaxiom real-times-closed
	     (in real)
	     (formula (closed-under real times))
	     (help "Times is closed."))

(th~defaxiom real-plus-assoc
	     (in real)
	     (formula (associative real plus))
	     (help "Plus is assoc."))

(th~defaxiom real-times-assoc
	     (in real)
	     (formula (associative real times))
	     (help "Times is assoc."))

(th~defaxiom real-plus-commu
	     (in real)
	     (formula (commutative real plus))
	     (help "Plus is commu."))

(th~defaxiom real-times-commu
	     (in real)
	     (formula (commutative real times))
	     (help "Times is commu."))

(th~defaxiom real-plus-times-distrib
	     (in real)
	     (formula (distributive real plus times))
	     (help "Distributity for plus and times."))

(th~defaxiom real-plus-unit
	     (in real)
	     (formula (unit real plus zero))
	     (help "Zero is additive unit element."))

(th~defaxiom real-times-unit
	     (in real)
	     (formula (unit real times one))
	     (help "One is multiplicative unit element."))

(th~defaxiom real-plus-inv
	     (in real)
	     (formula (inverse-exist real plus zero))
	     (help "Existence of inverse elements."))

(th~defaxiom real-times-inv
	     (in real)
	     (formula (inverse-exist real\0 times one))
	     (help "Existence of inverse elements."))

(th~defaxiom real-trichotomy
	     (in real)
	     (formula (trichotomy real less))
	     (help "Trichotomy for reals."))

(th~defaxiom real-less-trichotomy
	     (in real)
	     (formula (trichotomy real less))
	     (help "Trichotomy for reals."))

(th~defaxiom real-less-transitive
	     (in real)
	     (formula (transitivity real less))
	     (help "Less is transitive for reals."))

(th~defaxiom real-less-times-mono
	     (in real)
	     (formula (monotone less times real\0))
	     (help "Less is monotone for times."))

(th~defaxiom real-less-plus-mono
	     (in real)
	     (formula (monotone less plus real))
	     (help "Less is monotone for plus."))

(th~defaxiom real-complete
	     (in real)
	     (formula
	      (forall (lam (xx (o num))
	      (forall (lam (yy (o num))
			   (implies (and (and (not (empty xx))(not (empty yy)))
					 (and (= real (union xx yy))
					      (forall-sort (lam (x num)
								(forall-sort (lam (y num)
										  (less x y)) yy)) xx)))
				    (exists-sort (lam (t num)
				    (forall-sort (lam (x num)
				    (forall-sort (lam (y num)
						      (and (leq x t)(leq t y))) yy))xx))real)))))))
	     (help "Completeness of the reals."))


(th~defaxiom rat-real
             (in real)
             (formula
	      (forall-sort
	       (lam (x num)
		    (real x))
	       rat))
	     (termdecl)
	     (help "All rational numbers are reals."))	     

(th~defdef closed-interval-with-bounds
	   (in real)
           (definition
             (lam (G (o num))
                  (lam (l num)
                       (lam (r num)
                            (forall (lam (x num)
                                         (equiv (and (leq x r) (geq x l))
                                                (in x G))))))))
	   (help "Predicate for closed intervals of real numbers."))


(th~defdef closed-interval
	   (in real)
           (definition
             (lam (G (o num))
                  (closed-interval-with-bounds
                   G
                   (supremum real-struct G)
                   (infimum real-struct G))))
	   (help "Predicate for closed intervals of real numbers."))



(th~defdef open-interval-with-bounds
	   (in real)
           (definition
             (lam (G (o num))
                  (lam (l num)
                       (lam (r num)
                            (forall (lam (x num)
                                         (forall (lam (x num)
                                                      (equiv (and
                                                              (less x r)
                                                              (greater x l))
                                                             (in x G))))))))))
	   (help "Predicate for open intervals of real numbers."))

(th~defdef open-interval
	   (in real)
           (definition
             (lam (G (o num))
                  (open-interval-with-bounds
                   G
                   (supremum real-struct G)
                   (infimum real-struct G))))
	   (help "Predicate for open intervals of real numbers."))

(th~defdef closed-interval-bounds
	   (in real)
           (definition
             (lam (x num)
                  (lam (y num)
                       (lam (z num)
                            (and (leq z x) (geq x y))))))
	   (help "The closed interval for given bounds."))


(th~defdef open-interval-bounds
	   (in real)
           (definition
             (lam (x num)
                  (lam (y num)
                       (lam (z num)
                            (and (less z x) (greater x y))))))
	   (help "The open interval for given bounds."))



(th~defdef interval-center
	   (in real)
	   (definition
	     (lam (I1 (o num))
		  (divide (minus (supremum real-struct I1)
				 (infimum real-struct I1))
			  (s one))))
	   (help "The center of an interval."))



(th~defdef sqrt 
	   (in real)
	   (definition
	     (lam (x  num)
		  (that (lam (y num) (= (power y 2) x)))))
	   (help "Definition of square root."))

(th~defdef root 
	   (in real)
	   (definition
	     (lam (n  num)
		  (lam (x  num)
		       (that (lam (y num) (= (power y n) x))))))
	   (help "Definition of the nth root of a number."))
