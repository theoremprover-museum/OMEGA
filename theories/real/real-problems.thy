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

(th~defproblem hb-ass-l1
	       (in real)
	       (conclusion ass-l1
		(forall (lam (x num) 
                   (implies (in x Real) 
	                    (= x (divide x (power 2 0)))))))
	       (help "one of the assumptions for heine-Borel, should be subsumed by
                      CAS soon."))

(th~defproblem closed-interval-closed
	       (in real)
	       (conclusion ass-l3
		(forall (lam (x num)
		 (forall (lam (y num) 
		  (implies (and (in x Real) (in y Real)) 
			   (closed-interval (closed-interval-bounds x y))))))))
	       (help "The closed interval with given bounds is really a closed
                      Interval."))

(th~defproblem closed-interval-bound
               (in real)
               (conclusion ass-l4
                (forall (lam (x num)
                 (forall (lam (y num)
                  (implies (and (in x Real) (in y Real)) 
                           (and (= x (infimum real-struct (closed-interval-bounds x y))) 
                                (= y (supremum real-struct (closed-interval-bounds x y))))))))))
               (help "The closed interval with given bounds really has these bounds."))



(th~defproblem rationals-as-special-reals
	       (in real)
	       (assumption bla1 (forall (lam (x num) (implies (in x rat) (exists (lam (y num) (exists (lam (z num) (and (in y int) (and (in z int) (= x (divide y z))))))))))))
	       (assumption bla2 (forall (lam (x num) (forall (lam (y num) (forall (lam (z num) (equiv (= x (divide y z)) (= (times x z) y)))))))))
	       (conclusion chris
		(= Rat 
		   (lam (x num) 
                    (and (in x Real)
			 (exists (lam (n num)
				      (and (in n Int)
					   (and (not (= n 0))
						(in (times n x) Int))))))))))



(th~defproblem square-root-of-two-is-not-real  ;;??
	       (in real)
;               (assumption bla (forall (lam (x num) (implies (in x rat) (exists (lam (y num) (exists (lam (z num) (and (in y int) (and (in z int) (= x (divide y z))))))))))))
	       (conclusion 
		(not (exists (lam (x num)
				  (and (rat x)
				       (= (power x 2) 2)))))))
                      
(th~defproblem sqrt2-not-rat
	       (in real)
	       (constants (n num) (m num) (k num))
	       (conclusion
		(not (rat (sqrt 2))))
	       (help "sqrt 2 is not a rational number."))

(th~defproblem sqrt6-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt 6))))
	       (help "sqrt 6 is not a rational number."))

(th~defproblem sqrt3+3-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt (plus 3 3)))))
	       (help "sqrt 3+3 is not a rational number."))

(th~defproblem sqrt8-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt 8))))
	       (help "sqrt 8 is not a rational number."))

(th~defproblem sqrt9-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt 9))))
	       (help "sqrt 9 is not a rational number. Not a theorem, of course."))

(th~defproblem sqrt12-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt 12))))
	       (help "sqrt 12 is not a rational number."))

(th~defproblem 3root2-not-rat
	       (in real)	
	       (conclusion
		(not (rat (power 2 (div 1 3)))))
	       (help "Third root of 2 is not rational."))

(th~defproblem 8root2-not-rat
	       (in real)	
	       (conclusion
		(not (rat (power 2 (div 1 8)))))
	       (help "8th root of 2 is not rational."))

(th~defproblem sqrt24-not-rat
	       (in real)	
	       (conclusion
		(not (rat (sqrt 24))))
	       (help "sqrt 24 is not a rational number."))
