;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(th~deftheory divisibility
	      (uses integer)
	      (constants (product (all-types aa (aa (aa aa) aa aa))))
	      (help "Divisibility on integers"))

(th~defdef divisor
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
		       (and
			(and (int x)
			     (int y))
			(exists (lam (z num)
				     (and (int z)
					  (= y (times x z)))))))))
	   (help "The predicate for integer divisibility."))


(th~defdef UB
	   (in divisibility)
	   (definition
             (lam (S (o num))
               (lam (x (num))
                 (forall (lam (y (num))
                   (implies (in y S) (leq y x)))
                 )
               )
             )
           )
	   (help "In set S of type num, x is an upperbound of the set S if
                  (UB S x) is true.")
)

(th~defdef prime
           (in divisibility)
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
)            
               
(th~defdef rel-prime
           (in divisibility)
           (definition
             (lam (a (num))
               (lam (b (num))
                 (= (gcd a b) 1)
               )
             )
           )
)                 
               
;(th~defdef exists-unique-sort
;           (in divisibility)
;           (type-variables aa)
;           (definition
;             (lam (T (o aa))
;                  (lam (S (o aa))
;                       (exists-unique (lam (X aa)
;                                           (and (S X) (T X)))))))
;)                 

(th~defaxiom product-base
             (in divisibility)
             (formula
	      (forall (lam (b num)
			   (= (product b b) (lam (f (num num)) (f b)))))))
              
(th~defaxiom product-step
             (in divisibility)
             (formula
	      (forall (lam (b num)
			   (forall (lam (n num)
					(= (product b (s n))
					   (lam (f (num num)) (times (product b n f) (f (s n)))))))))))
(th~defdef mod
	   (in integer)
           (definition
             (lam (x num)
                  (lam (y num)
                       (that (lam (z num)
                                  (= x 
                                    (plus 
                                      z 
                                      (times
                                        y 
                                        (div x y)
                                      )
                                    )
                                  )
                              )
                        )
                   )
              )
            )
           (help "The mod operator for natural numbers."))



              
;(th~defaxiom product-base
;             (in divisibility)
;             (formula
;               (= (product emptyset) 1)
;             )
;)
;
;(th~defaxiom product-step
;             (in divisibility)
;             (formula
;               (forall (lam (S (o num))
;                 (forall (lam (a (num))
;                   (implies
;                     (not S a)
;                     (=
;                       (product (union S (lam (b num) (= b a))))
;                       (times a (product S))
;                     )
;                   )
;                 ))
;               ))
;             )
;)             
 
