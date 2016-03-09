;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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
(in-package :keim)

(th~deftheory calculus
	      (uses real)
	      (help "Theory of elementary real calculus."))
	   
(th~defdef limf
	   (in calculus)
	   (definition 
             (lam (F (num num))
                  (lam (a num)
                       (lam (L num)
                            (forall
			     (lam (eps num)
				  (implies (less zero eps)
					   (exists
					    (lam (del num)
						 (implies
						  (less zero del)
						  (forall
						   (lam (x num)
							(implies (less (absval (minus x a)) del)
								 (less (absval (minus (F x) L)) eps))))))))))))))
	   (help "Definition of the limit of a function at a point."))

(th~defdef continuous-at
	   (in calculus)
           (definition 
             (lam (F (num num))
		       (lam (X num)
			    (limf F X (F X)))))
	   (help "Definition of continuitiy at a point."))

(th~defdef continuous
	   (in calculus)
           (definition 
             (lam (F (num num))
                  (forall (lam (X num)
                               (implies (defined (F X))
                                        (continuous-at F X))))))
	   (help "Definition of continuitiy on the whole domain."))


(th~defdef derivative
	   (in calculus)
           (definition 
             (lam (F (num num))
                  (lam (X num)
                       (that (lam (lim num)
                                  (forall (lam (eps num)
                                               (exists (lam (del num)
                                                            (forall (lam (y num)
                                                                         (implies (leq (absval (minus x y)) eps)
                                                                                  (leq (absval (minus (div (minus (F X) (F Y))
                                                                                                           (minus x y ))
                                                                                                      lim))
                                                                                       del)))))))))))))
	   (help "Definition of the derivative of a function."))


(th~defdef nderivative
           (in calculus)
           (definition
             (iterate derivative))
           (help "Definition of the n-th derivative."))


(th~defdef differentiable-at
	   (in calculus)
           (definition 
             (lam (F (num num))
                  (lam (X num)
                       (defined (derivative F X)))))
	   (help "Derivative at a point."))



(th~defdef differentiable
	   (in calculus)
           (definition 
             (lam (F (num num))
                  (forall (lam (X num)
                               (implies (defined (F X))
                                        (differentiable-at F X))))))
	   (help "Definition of continuitiy on the whole domain."))

(th~defdef cdifferentiable
	   (in calculus)
           (definition 
             (lam (F (num num))
                  (and (differentiable F)
                       (continuous (derivative F)))))
           (help "Definition of continuously diefferentiable functions."))


(th~defdef total-minimum
	   (in calculus)
           (definition 
             (lam (X num)
                  (lam (F (num num))
                       (lam (G (o num))
                            (forall-sort (lam (y num)
                             (less (f x) (f y))) G)))))
	   (help "X is a total minimum of a function F on a set G,
                  iff for all y in G, F(X) leq F(Y)."))

(th~defdef local-minimum
	   (in calculus)
           (definition 
             (lam (X num)
                  (lam (F (num num))
                       (exists (lam (eps num)
                                    (and (greater eps zero)
                                         (forall (lam (y num)
                                                      (implies (leq (absval (minus x y)) eps)
                                                               (less (f x) (f y)))))))))))
	   (help "Definition of local minimum X of a function F:
                  X is a local minimum, iff it is minimal on an epsilon-environment."))



(th~defdef total-maximum
	   (in calculus)
           (definition 
             (lam (X num)
		(lam (F (num num))
		 (lam (G (o num))
		  (forall-sort  (lam (y num)
		   (greater (f x) (f y))) G)))))
	   (help "X is a total maximum of a function F on a set G,
                  iff for all y in G, F(X) greater F(Y)."))

(th~defdef local-maximum
	   (in calculus)
           (definition 
             (lam (X num)
	        (lam (F (num num))
		 (exists (lam (eps num)
		  (and (greater eps zero)
		       (forall (lam (y num)
			(implies (leq (absval (minus x y)) eps)
				 (greater (f x) (f y)))))))))))
	   (help "Definition of local minimum X of a function F:
                  X is a local minimum, iff it is minimal on an epsilon-environment."))





 

									
