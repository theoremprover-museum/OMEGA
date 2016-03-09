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

;;**************************************************
;;       Theorems with coninuous functions
;;**************************************************

(th~defproblem cont-plus (in limit)
	       ;; Thm 5.2.1.a
	       ;; should work with MULTI
	       (constants
		(f (num num))
		(g (num num))
		(a num))
	       (assumption cont-g
		     (forall (lam (e2 num)
				  (exists (lam (d2 num)
					       (forall (lam (x2 num)
							    (implies (less 0 e2)
								     (and (less 0 d2)
									  (implies
									   (less (absval (minus x2 a)) d2)
									   (less (absval (minus(g x2) (g a)))
										 e2)))))))))))
	       (assumption cont-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (less (absval (minus x1 a)) d1)
										 (less (absval (minus (f x1) (f a)))
										       e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
					       (forall (lam (x num)
							    (implies (less 0 e)
								     (and (less 0 d)
									  (implies
									   (less (absval (minus x a)) d)
									    (less (absval (minus (plus (f x)
												       (g x))
												 (plus (f a)
												       (g a))))
										  e)))))))))))
	       )


(th~defproblem cont-minus (in limit)
	       ;; Thm 5.2.1.a
	       ;; should work with MULI
	       (constants
		(f (num num))
		(g (num num))
		(a num))
	       (assumption cont-g
		     (forall (lam (e2 num)
				  (exists (lam (d2 num)
					       (forall (lam (x2 num)
							    (implies (less 0 e2)
								     (and (less 0 d2)
									  (implies
									   (less (absval (minus x2 a)) d2)
									   (less (absval (minus(g x2) (g a)))
										 e2)))))))))))
	       (assumption cont-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (less (absval (minus x1 a)) d1)
										 (less (absval (minus (f x1) (f a)))
										       e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
					       (forall (lam (x num)
							    (implies (less 0 e)
								     (and (less 0 d)
									  (implies
									   (less (absval (minus x a)) d)
									    (less (absval (minus (minus (f x)
													(g x))
												 (minus (f a)
													(g a))))
										  e)))))))))))
	       )

(th~defproblem cont-times (in limit)
	       ;; Thm 5.2.1.a
	       ;; should work with MULTI, except that COSIE may crashes at the end
	       (constants
		(f (num num))
		(g (num num))
		(a num))
	 
	       (assumption cont-f
		     (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
						   (implies (less 0 e1)
							     (and (less 0 d1)
								  (implies
								   (less (absval (minus x1 a)) d1)
								   (less (absval (minus (f x1) (f a)))
									 e1)))))))))))
	 
	       (assumption cont-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
						  (implies (less 0 e2)
							    (and (less 0 d2)
								 (implies
								  (less (absval (minus x2 a)) d2)
								  (less (absval (minus (g x2) (g a)))
									e2)))))))))))
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
					  (forall (lam (x num)
						       (implies (less 0 e)
								(and (less 0 d)
								     (implies
								      (less (absval (minus x a)) d)
								      (less (absval (minus (times (f x)
												  (g x))
											   (times (f a)
												  (g a))))
									    e)))))))))))
	       )




(th~defproblem cont-timesconst (in limit)
	       ;; Thm 5.2.1.a
	       ;; should work with MULTI
	       (constants
		(f (num num))
		(b num)
		(a num))
	       
	       (assumption cont-f
		     (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
						   (implies (less 0 e1)
							     (and (less 0 d1)
								  (implies
								   (less (absval (minus x1 a)) d1)
								   (less (absval (minus (f x1) (f a)))
									 e1)))))))))))
	 
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
					  (forall (lam (x num)
						       (implies (less 0 e)
								(and (less 0 d)
								     (implies
								      (less (absval (minus x a)) d)
								      (less (absval (minus (times b (f x))
											   (times b (f a))))
									    e)))))))))))
	       )



(th~defproblem cont-div (in limit)
	       ;; Thm 5.2.1.b
	       ;; SHOULD WORK WITH COSIE EXCEPT GENERAL DIV PROBLEM!
	       (constants
		(f (num num))
		(h (num num))
		(limitf num)
		(a num))


	       (assumption hnoteq0
			   (forall (lam (x num)
					(or (less (h x) 0)
					    (greater (h x) 0)))))
	       
	       (assumption cont-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (less (absval (minus x1 a)) d1)
									     (less (absval (minus (f x1) (f a)))
										   e1)))))))))))

	       (assumption cont-h
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (less (absval (minus x1 a)) d1)
									     (less (absval (minus (h x1) (h a)))
										   e1)))))))))))
	       

	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (less (absval (minus x a)) d)
							     (less (absval (minus (div (f x) (h x)) (div (f a) (h a))))
								   e))))))))))))



#|(th~defproblem cont-div (in limit)
	       ;; Thm 5.2.1.b
	       (constants
		(f (num num))
		(limitf num)
		(a num))


	       (assumption notneg (not (= limitf 0)))
	       
	       (assumption cont-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (less (absval (minus x1 a)) d1)
									     (less (absval (minus (f x1) limitf))
										   e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (less (absval (minus x a)) d)
							     (and (less (absval (minus (div 1 (f x)) (div 1 limitf))) e)
								  (not (= (f x) 0)))))))))))))
	       )
|#




(th~defproblem cont-composite
	       ;; thm 5.2.7
	       ;; SHOULD WORK WITH MULTI
	       (in limit)
	       (constants
		(f (num num))
		(g (num num))
		(a num))
	       
	       (assumption cont-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
					     (implies (less 0 e2)
						       (and (less 0 d2)
							      (implies
							       (less (absval (minus x2 a)) d2)
							       (less (absval
								      (minus (g x2) (g a)))
								     e2)))))))))))
	       (assumption cont-f
		     (forall (lam (e1 num)
			     (exists (lam (d1 num)
					  (forall (lam (x1 num)
					     (implies (less 0 e1)
						       (and (less 0 d1)
						      (implies
						       (less (absval (minus x1 (g a))) d1)
						       (less (absval
							      (minus (f x1) (f (g a))))
							     e1)))))))))))
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (less (absval (minus x a)) d)
							    (less (absval
								   (minus (f (g x))
									  (f (g a))))
								  e)))))))))))
	       )




(th~defproblem cont-const (in limit)
	       ;; example 5.1.5 a
	       ;; SHOULD WORK WITH MULTI
	       (constants
		(a num)
		(b num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus b b))
									 e)))))))))))
	       )



(th~defproblem cont-var (in limit)
	       ;; example 5.1.5 b
	       ;; SHOULD WORK WITH MULTI
	       (constants  (a num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus x a))
									 e)))))))))))
	       )

(th~defproblem cont-var-seq (in limit)
	       ;; example 5.1.5 b, formulated with sequents
	       ;; SHOULD WORK WITH MULTI
	      
	       (constants  (a num)
			   (Xn (num num)))

	       (assumption XnConvergesToA
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less (absval (minus (Xn n) a)) e))))))))))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less (absval (minus (Xn n) a)) e))))))))))
	       )



(th~defproblem cont-square (in limit)
	       ;; example 5.1.5 c
	       ;; SHOULD WORK WITH MULTI
	       (constants  (a num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus (times x x)
											(times a a)))
									 e)))))))))))
	       )


(th~defproblem cont-square-seq (in limit)
	       ;; example 5.1.5 c formulated with sequents!
	       ;; SHOULD WORK WITH MULTI
	       (constants (a num)
			  (Xn (num num)))
	       

	       (assumption XnConvergesToA
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less (absval (minus (Xn n) a)) e))))))))))

	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less (absval (minus (times (Xn n) (Xn n)) (times a a))) e))))))))))
	       )


(th~defproblem cont-THM5.1.3a
	       (in limit)
	       ;; SHOULD WORK WITH MULTI

	       (constants (a num)
			  (Xn (num num))
			  (f (num num)))

	       (assumption fcontdelta
			   (forall (lam (e num)
					(implies (less 0 e)
						 (exists (lam (d num)
							      (and (less 0 d)
								   (forall (lam (x num)
										(implies (less (absval (minus x a)) d)
											 (less (absval (minus (f x) (f a))) e)))))))))))
	       
	       (conclusion thm
			   (implies (forall (lam (e num)
						 (exists (lam (k num)
							      (forall (lam (n num)
									   (implies (less 0 e)
										    (implies (leq k n)
											     (less (absval (minus (Xn n) a)) e)))))))))
				    
				    (forall (lam (e num)
						 (exists (lam (k num)
							      (forall (lam (n num)
									   (implies (less 0 e)
										    (implies (leq k n)
											     (less (absval (minus (f (Xn n)) (f a))) e))))))))))
			   ))


(th~defproblem cont-square-with-2
	       ;; SHOULD WORK WITH MULTI
	       (in limit)
	       (constants  (a num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x 2)) d)
								   (less (absval (minus (times x x)
											4))
									 e)))))))))))
	       )


(th~defproblem cont-x*x*x
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants  (a num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus (times (times x x) x)
											(times (times a a) a)))
									 e)))))))))))
	       )

 
 
(th~defproblem cont-x*x*x*x
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants  (a num)
			   (c num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus
				(times (times (times x x) x) x)
				(times (times (times a a) a) a)))
									 e)))))))))))
	       )



(th~defproblem cont-poly
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants  (a num)
			   (c num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus
				(plus (times 2 (times (times (times x x) x) x))  x)
				(plus (times 2 (times (times (times a a) a) a))  a)))
									 e)))))))))))
	       )

(th~defproblem cont-poly-with-numbers
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants  (a num)
			   (c num))
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (d num)
			   (forall (lam (x num)
					(implies (less 0 e)
						 (and (less 0 d)
						      (implies
						       (less (absval (minus x 2)) d)
						       (less (absval (minus
								      (plus (times 2 (times (times (times x x) x) x))  x)
								      34))
							     e)))))))))))
	       )


(th~defproblem cont-x^3
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants  (a num))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (less (absval (minus x a)) d)
								   (less (absval (minus (power x 3)
											(power a 3)))
									 e)))))))))))
	       )


(th~defproblem cont-absval
	       ;; exercise 5.1.10
	       ;; DOES NOT WORK WITH MULTI CURRENTLY!
	       (in limit) 
	       (constants
		(a num))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (less (absval (minus x a)) d)
										 (less (absval (minus (absval x) a))
										       e)))))))))))
	       )


(th~defproblem cont-exerc5.1.6
	       ;; exercise 5.1.6
	       ;; DOES NOT WORK WITH MULTI CURRENTLY --> MULTIPLE UNWRAPHYPS ARE NEEDED
	       (in limit) 
	       (constants
		(a num)
		(f (num num)))

	       (assumption cont-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (less (absval (minus x1 a)) d1)
										 (less (absval
											(minus (f x1) (f a)))
										       e1)))))))))))
	       (conclusion thm
			    (forall (lam (e num)
					 (implies (less 0 e)
						  (exists (lam (d num)
							       (and (less 0 d)
								    (forall (lam (x num)
										 (forall (lam (y num)
											      (implies (and (less (absval (minus x a)) d)
													    (less (absval (minus y a)) d))
												       (less (absval (minus (f x) (f y)))
													     e))))))))))))))




(th~defproblem cont-exerc5.1.11
	       ;; exercise 5.1.11
	       ;; SHOULD WORK WITH MULTI
	       (in limit) 
	       (constants
		(a num)
		(k num)
		(f (num num)))
	       
	       (assumption ass-k
			   (less 0 k))

	       (assumption ass-f
			   (forall (lam (x num)
					(forall (lam (y num)
						     (leq (absval (minus (f x) (f y)))
							  (times k (absval (minus x y)))))))))

	       (conclusion cont-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (less (absval (minus x1 a)) d1)
										 (less (absval
											(minus (f x1) (f a)))
										       e1)))))))))))
	       )

(th~defproblem contlim-composite
	       ;; exercise 5.2.6
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit)
	       (constants
		(f (num num))
		(g (num num))
		(limitg num)
		(a num))
	       
	       (assumption limit-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
					     (implies (less 0 e2)
						      (and (less 0 d2)
							   (implies
							    (and (less (absval (minus x2 a)) d2)
								 (greater (absval (minus x2 a)) 0))
							    (less (absval
								   (minus (g x2) limitg))
								  e2)))))))))))

	       (assumption cont-f
		     (forall (lam (e1 num)
			     (exists (lam (d1 num)
					  (forall (lam (x1 num)
					     (implies (less 0 e1)
						      (and (less 0 d1)
							   (implies
							    (less (absval (minus x1 limitg)) d1)
							    (less (absval
								   (minus (f x1) (f limitg)))
								  e1)))))))))))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval
								    (minus (f (g x))
									   (f limitg)))
								   e)))))))))))
	       )


(th~defproblem limimpcont
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit)
	       (constants
		(f (num num))
		(g (num num))
		(limitg num)
		(a num))
	       
	       (assumption limit-f
			   (forall (lam (e2 num)
					(exists (lam (d2 num)
						     (forall (lam (x2 num)
								  (implies (less 0 e2)
									   (and (less 0 d2)
										(implies
										 (and (less (absval (minus x2 a)) d2)
										      (greater (absval (minus x2 a)) 0))
										 (less (absval
											(minus (f x2) (f a)))
										       e2)))))))))))
	       
	       (conclusion cont-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (less (absval (minus x1 a)) d1)
										 (less (absval
											(minus (f x1) (f a)))
										       e1)))))))))))
	       )




;;**************************************************
;;       Theorems with limits of functions
;;**************************************************


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 1



(th~defproblem lim-xmp4.1.7.a
	       ;; example 4.1.7.a
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit) 

	       (constants
		(b num)
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus b b))
								   e)))))))))))	       
	       )


(th~defproblem lim-xmp4.1.7.b
	       ;; example 4.1.7.b
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit) 

	       (constants
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus x a))
								   e)))))))))))	       
	       )


(th~defproblem lim-xmp4.1.7.c
	       ;; example 4.1.7.c
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit) 

	       (constants
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus (times x x) (times a a)))
								   e)))))))))))	       
	       )



(th~defproblem lim-xmp4.1.7.c-variant
	       ;; variant of example 4.1.7.c
	       ;; SHOULD WORK WITH MULTI
	       
	       (in limit) 
	       (constants  
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								 (implies
								   (and (less (absval (minus x a)) d)
									(greater (absval (minus x a)) 0))
								   (less (absval (minus (power x 2)
											(power a 2)))
									 e)))))))))))
	       (help "Example 4.1.c (Variation)"))



(th~defproblem lim-xmp4.1.7.d
	       ;; example 4.1.7.d
	       ;; SHOULD WORK WITH MULTI EXCEPT THE INSTANTIATIONS ...
	       
	       (in limit) 
	       (constants
		(f (num num))
		(limitf num)
		(a num))

	       (assumption ass (greater a 0))
	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus (div 1  x) (div 1 a)))
								   e)
							     ))))))))))
	       )

(th~defproblem lim-xmp4.1.7.e
	       ;; example 4.1.7.e
	       ;; SHOULD WORK WITH MULTI EXCEPT THE INSTANTIATIONS (AND A COSIE CRASH ...)	       
	       
	       (in limit) 
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x 2)) d)
									(greater (absval (minus x 2)) 0))
								   (less (absval (minus
										  (div (minus (power x 3) 4)
										       (plus (power x 2) 1))
										  (div 4 5)))
									 e)))))))))))
	       (help "Example 4.1.7.e"))



(th~defproblem lim-Thm4.1.5
	       ;; Theorem 4.1.5
	       ;; does not work with MULTI currently!
	       
	       (in limit)
	       
	       (constants (c (num))
			  (f (num num))
			  (l1 num)
			  (l2 num))

	       (assumption liml1
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies
								   (less 0 e)
								   (and (less 0 d)
									(implies
									 (and
									  (less (absval (minus x c)) d)
									  (greater (absval (minus x c)) 0))
									 (less (absval
										(minus (f x) l1))
									       e)))))))))))

	       (assumption liml2
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies
								   (less 0 e)
								   (and (less 0 d)
									(implies
									 (and
									  (less (absval (minus x c)) d)
									  (greater (absval (minus x c)) 0))
									 (less (absval
										(minus (f x) l2))
									       e)))))))))))

	       (conclusion conc                         
                           (= l1 l2))
	       (help "Theorem 4.1.5")
	       )



(th~defproblem lim-div-1-x
	       ;; does not work with MULTI currently 
	       
	       (in limit)

	       (constants (c num))
	       
	       (assumption ass1 (greater c 0))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies
								   (less 0 e)
								   (and (less 0 d)
									(implies
									 (and
									  (less (absval (minus x c)) d)
									  (greater (absval (minus x c)) 0))
									 (less (absval (minus (div 1 x) (div 1 c))) e))))))))))))





(th~defproblem lim-not1divx
	       ;; example 4.1.10.a
	       ;; does not work with MULTI currently (COSIE CRASHES)!
	       
	       (in limit)
	       
	       (conclusion thm
			   (not (exists (lam (l num)
					     (lim (lam (x num)
						       (div 1 x))
						  0
						  l))))))





(th~defproblem lim-ex4.1.2.first
	       ;; exercise 4.1.2 first direction
	       ;; should work with MULTI
	       
	       (in limit)
	       
	       (constants  
		(f (num num))
		(a num)
		(b num)
		(c num)
		(l num))
	       
	       (assumption ass
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x c)) d)
										      (greater (absval (minus x c)) 0))
										 (less (absval (minus (absval (minus (f x) l)) 0))
										       e)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x c)) d)
										      (greater (absval (minus x c)) 0))
										 (less (absval (minus (f x) l))
										       e)))))))))))
	       (help "Exercise 4.1.2, first direction"))


(th~defproblem lim-ex4.1.2.second
	       ;; exercise 4.1.2 second direction
	       ;; should work with MULTI
	       
	       (in limit)
	       
	       (constants  
		(f (num num))
		(a num)
		(b num)
		(c num)
		(l num))
	       
	       (assumption ass
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x c)) d)
										      (greater (absval (minus x c)) 0))
										 (less (absval (minus (f x) l))
										       e)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x c)) d)
										      (greater (absval (minus x c)) 0))
										 (less (absval (minus (absval (minus (f x) l)) 0))
										       e)))))))))))
	       (help "Exercise 4.1.2, second direction"))

(th~defproblem lim-ex4.1.3.first 
	       ;; exercise 4.1.3 first direction
	       ;; should work with MULTI
	       
	       (in limit)
	       (constants  
		(f (num num))
		(a num)
		(b num)
		(c num)
		(l num))
	       
	       (assumption ass
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x 0)) d)
									(greater (absval (minus x 0)) 0))
								   (less (absval (minus (f (plus c x))  l))
									 e)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x c)) d)
									(greater (absval (minus x c)) 0))
								   (less (absval (minus (f x) l))
									 e)))))))))))
	       (help "Exercise 4.1.3, first direction"))


(th~defproblem lim-ex4.1.3.second
	       ;; exercise 4.1.3 second direction
	       ;; should work with MULTI
	       
	       (in limit)
	       (constants  
		(f (num num))
		(a num)
		(b num)
		(c num)
		(l num))

	       (assumption ass
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x c)) d)
									(greater (absval (minus x c)) 0))
								   (less (absval (minus (f x) l))
									 e)))))))))))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x 0)) d)
									(greater (absval (minus x 0)) 0))
								   (less (absval (minus (f (plus x c))  l))
									 e)))))))))))
	       
	       (help "Exercise 4.1.3, second direction"))


#|
Does not work in the way supposed by exercise 4.1.6
(th~defproblem lim-ex4.1.6.second    ;; zweiter Teil ohne Def. von g(x)
	 (in limit)
	 (constants
	  (g (num num))
	  (a num))
	 
	 (assumption g-c2-dominance
		     (forall (lam (x1 num)
		     (forall (lam (c1 num)
				  (implies             ;; x1 in (0,a)
				   (and (less 0 x1)
					(less x1 a))
				   (implies            ;; c1 in (0,a)
				    (and (less 0 c1)
					 (less c1 a))
				    (less (absval (minus (g x1) (times c1 c1)))
					  (times (times 2 a) (absval (minus x1 c1)))))))))))
	 (assumption a-bigger0
		     (greater a 0))
	 
	 (conclusion thm
		     (forall (lam (c num)			   
		     (forall (lam (e num)
		     (exists (lam (d num)
		     (forall (lam (x num)
				  (implies
				   (and (less 0 c)
					(less c a))
				   (implies (less 0 e)
					    (and (less 0 d)
						 (implies
						  (and (less (absval (minus x c)) d)
						       (greater (absval (minus x c)) 0))
						  (less (absval (minus (g x) (times c c)))
							e))))))))))))))
	 
	 (help "Exercice 4.1.6. (second part)"))
|#


(th~defproblem lim-exerc4.1.7
	       ;; exercise 4.1.7
	       ;; should work with MULTI
	       
	       (in limit)	       
	       (constants
		(a num)
		(k num)
		(f (num num))
		(l num))

	       (assumption ass-k
			   (greater k 0))
	       
	       (assumption ass-f
			   (forall (lam (x num)
					(forall (lam (y num)
						     (leq (absval (minus (f x) l))
							  (times k (absval (minus x a)))))))))

	       (conclusion lim-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (and (less (absval (minus x1 a)) d1)
										      (greater (absval (minus x1 a)) 0))
										 (less (absval (minus (f x1) l))
										       e1)))))))))))
	       )




(th~defproblem lim-x^3 
	       ;; exercise 4.1.8
	       ;; should work with MULTI

	       (in limit) 
	       (constants  
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								 (implies
								   (and (less (absval (minus x a)) d)
									(greater (absval (minus x a)) 0))
								   (less (absval (minus (power x 3)
											(power a 3)))
									 e)))))))))))
	       (help "Exercise 4.1.8"))

(th~defproblem lim-ex4.1.10.a
	       ;; excersice 4.1.10.a
	       ;; should work with MULTI

	       (in limit)

	       (conclusion thm ;; x > 1
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (less 1 x)
										 (implies
										  (and (less (absval (minus x 2)) d)
										       (greater (absval (minus x 2)) 0))
										  (less (absval (minus (div 1 (minus 1 x))
												       -1))
											e))))))))))))
	       (help "Exercise 4.1.(10).a"))


(th~defproblem lim-ex4.1.10.b
	       ;; excersice 4.1.10.b
	       ;; should work with MULTI
	       
	       (in limit)
	       
	       (conclusion thm ;; x <> 0
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies (less 0 x)
											 (implies
											  (and (less (absval (minus x 1)) d)
											       (greater (absval (minus x 1)) 0))
											  (less (absval
												 (minus (div x (plus 1 x))
													(div 1 2)))
												e))))))))))))
	       (help "Exercise 4.1.(10).b"))

(th~defproblem lim-ex4.1.10.c
	       ;; excersice 4.1.10.c
	       ;; should work with MULTI
	       
	       (in limit)
	       
	       (conclusion thm ;; x <> 0
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies (less 0 x)
											 (implies
											  (and (less (absval (minus x 1)) d)
											       (greater (absval (minus x 1)) 0))
											  (less (absval
												 (minus (div (times x x) (absval x))
													0))
												e))))))))))))
	       (help "Exercise 4.1.(10).c"))


(th~defproblem lim-ex4.1.10.d
	       ;; excersice 4.1.10.d
	       ;; should work with MULTI
	       
	       (in limit)
	       (conclusion thm ;; x <> 0
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x 1)) d)
										      (greater (absval (minus x 1)) 0))
										 (less (absval
											(minus
											 (div (plus (minus (power x 2) x) 1)
											      (plus x 1))
											 (div 1 2)))
										       e)))))))))))
	       (help "Exercise 4.1.(10).d"))


(th~defproblem lim-ex4.1.11.a
	       ;; exercise 4.1.11.a
	       (in limit)

	       (conclusion thm
			   (not (exists (lam (l num)
					     (lim (lam (x num)
						       (div 1 (times x x)))
						  0
						  l))))))


(th~defproblem lim-ex4.1.12
	       ;; exercise 4.1.12
	       ;; should work with MULTI except that one inequality is not accepted by COSIE for some reasons ...
	       
	       (in limit)
	       
	       (constants  (f (num num))
			   (a num) (l num))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (and (less (absval (minus x1 0)) d1)
										      (greater (absval (minus x1 0)) 0))
										 (less (absval
											(minus (f x1) l ))
										       e1)))))))))))
	       (assumption a-greater-0
			   (less 0 a))
	       
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x 0)) d)
										      (greater (absval (minus x 0)) 0))
										 (less (absval
											(minus (f (times x a))
											       l))
										       e)))))))))))
               (help "Exercise 4.1.12."))




(th~defproblem lim-ex4.1.12-reverse
	       ;; reverse of excersice 4.1.12
	       ;; should work with MULTI except that one inequality is not accepted by COSIE for some reasons ...
	       
	       (in limit)
	       
	       (constants  (f (num num))
			   (a num) (l num))
	       
	       
	       (assumption a-greater-0
			   (less 0 a))
	       
	       
	       (assumption limit-fax
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x 0)) d)
										      (greater (absval (minus x 0)) 0))
										 (less (absval
											(minus (f (times a x))
											       l))
										       e)))))))))))
	       
	       
	       (conclusion limit-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (and (less (absval (minus x1 0)) d1)
										      (greater (absval (minus x1 0)) 0))
										 (less (absval
											(minus (f x1) l ))
										       e1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 2


(th~defproblem lim-th4.2.2
	       ;; theorem 4.2.2
	       ;; should work with MULTI 
	       
	       (in limit)
	       
	       (constants  (f (num num))
			   (a num) (l num))
	       
	       (assumption limit-f
			   (forall (lam (e num)
					(exists (lam (d num)
						     (forall (lam (x num)
								  (implies (less 0 e)
									   (and (less 0 d)
										(implies
										 (and (less (absval (minus x a)) d)
										      (greater (absval (minus x a)) 0))
										 (less (absval
											(minus (f x) l))
										       e)))))))))))
	       (conclusion bounded 
			   (exists (lam (d1 num)
					(exists (lam (m num)
						     (and (less 0 d1)
							  (and (less 0 m)
							       (forall (lam (x1 num)
									    (implies (and (less (absval (minus x1 a)) d1)
											  (greater (absval (minus x1 a)) 0))
										     (less (absval (f x1)) m))))))))))))
											   


(th~defproblem lim-plus
	       ;; theorem 4.2.4.a first part
	       ;; should work with MULTI

	       (in limit)
	       ;; Juergen Zimmer:
	       ;; If f has a limit 'limit1' in 'a' and
	       ;; g has a limit 'limit2' in 'a' then
	       ;; f+g has the limit 'limit1+limit2' in a.
	       (constants
		(f (num num))
		(g(num num))
		(limit1 num)
		(limit2 num)
		(a num))
	       (assumption limit-g
			   (forall (lam (e2 num)
					(exists (lam (d2 num)
					       (forall (lam (x2 num)
							    (implies (less 0 e2)
								     (and (less 0 d2)
									  (implies
									   (and (less (absval (minus x2 a)) d2)
										(greater (absval (minus x2 a)) 0))
									   (less (absval (minus(g x2) limit2))
										 e2)))))))))))
	       (assumption limit-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
							     (implies (less 0 e1)
								      (and (less 0 d1)
									   (implies
									    (and (less (absval (minus x1 a)) d1)
										 (greater (absval (minus x1 a)) 0))
									    (less (absval (minus (f x1) limit1))
										  e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
					       (forall (lam (x num)
							    (implies (less 0 e)
								     (and (less 0 d)
									  (implies
									   (and (less (absval (minus x a)) d)
										(greater (absval (minus x a)) 0))
									   (less (absval (minus (plus (f x)
												      (g x))
												(plus limit1
												      limit2)))
										 e)))))))))))
	       )


(th~defproblem lim-minus
	       ;; theorem 4.2.4.a second part
	        ;; should work with MULTI

	       (in limit)
	       (constants
		(f (num num))
		(g(num num))
		(limit1 num)
		(limit2 num)
		(a num))
	       (assumption limit-g
			   (forall (lam (e2 num)
					(exists (lam (d2 num)
					       (forall (lam (x2 num)
							    (implies (less 0 e2)
								     (and (less 0 d2)
									  (implies
									   (and (less (absval (minus x2 a)) d2)
										(greater (absval (minus x2 a)) 0))
									   (less (absval (minus(g x2) limit2))
										 e2)))))))))))
	       (assumption limit-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
							     (implies (less 0 e1)
								      (and (less 0 d1)
									   (implies
									    (and (less (absval (minus x1 a)) d1)
										 (greater (absval (minus x1 a)) 0))
									    (less (absval (minus (f x1) limit1))
										  e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
					       (forall (lam (x num)
							    (implies (less 0 e)
								     (and (less 0 d)
									  (implies
									   (and (less (absval (minus x a)) d)
										(greater (absval (minus x a)) 0))
									   (less (absval (minus (minus (f x)
												      (g x))
												(minus limit1
												      limit2)))
										 e)))))))))))
	       )




(th~defproblem lim-times
	       ;; theorem 4.2.4.a third part
	        ;; should work with MULTI except that COSIE may breaks down at the end!

	       (in limit) 
	       ;; Juergen Zimmer:
	       ;; If f has a limit 'limit1' in 'a' and
	       ;; g has a limit 'limit2' in 'a' then
	       ;; f*g has the limit 'limit1*limit2' in a.
	       (type-constants  i o num) ;;num als Sorte anstelle (x in R)
	       (constants
		(f (num num))
		(g (num num))
		(limit1 num)
		(limit2 num)
		(a num))
	 
	       (assumption limit-f
		     (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
						   (implies (less 0 e1)
							     (and (less 0 d1)
								  (implies
								   (and (less (absval (minus x1 a)) d1)
									(greater (absval (minus x1 a)) 0))
								   (less (absval (minus (f x1) limit1))
									 e1)))))))))))
	 
	       (assumption limit-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
						  (implies (less 0 e2)
							    (and (less 0 d2)
								 (implies
								  (and (less (absval (minus x2 a)) d2)
								       (greater (absval (minus x2 a)) 0))
								  (less (absval (minus (g x2) limit2))
									e2)))))))))))
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
					  (forall (lam (x num)
						       (implies (less 0 e)
								(and (less 0 d)
								     (implies
								      (and (less (absval (minus x a)) d)
									   (greater (absval (minus x a)) 0))
								      (less (absval (minus (times (f x)
												  (g x))
											   (times limit1
												  limit2)))
									    e)))))))))))
	       )

(th~defproblem lim-times-const
	       ;; theorem 4.2.4.a fourth part
	       ;; should work with MULTI
	       
	       (in limit)
	       
	       (type-constants  i o num) ;;num als Sorte anstelle (x in R)
	       (constants
		(f (num num))
		(g (num num))
		(limit1 num)
		(limit2 num)
		(a num)
		(b num))
	 
	       (assumption limit-f
		     (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
						   (implies (less 0 e1)
							     (and (less 0 d1)
								  (implies
								   (and (less (absval (minus x1 a)) d1)
									(greater (absval (minus x1 a)) 0))
								   (less (absval (minus (f x1) limit1))
									 e1)))))))))))
	 
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
					  (forall (lam (x num)
						       (implies (less 0 e)
								(and (less 0 d)
								     (implies
								      (and (less (absval (minus x a)) d)
									   (greater (absval (minus x a)) 0))
								      (less (absval (minus (times b (f x))
											   (times b limit1)))
									    e)))))))))))
	       )


(th~defproblem lim-div
	       ;; theorem 4.2.4.b
	       ;; should work with MULTI EXCEPT INSTANTIATIONS OF COSIE!
	       
	       (in limit) 
	       (constants
		(f (num num))
		(g (num num))
		(limitf num)
		(limitg num)
		(a num))


	       (assumption not0g
			   (forall (lam (x num)
					(or (less (g x) 0)
					    (greater (g x) 0)))))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (and(less (absval (minus x1 a)) d1)
										 (greater (absval (minus x1 a)) 0))
									     (less (absval (minus (f x1) limitf))
										   e1)))))))))))

	       (assumption limit-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
						  (implies (less 0 e2)
							    (and (less 0 d2)
								 (implies
								  (and (less (absval (minus x2 a)) d2)
								       (greater (absval (minus x2 a)) 0))
								  (less (absval (minus (g x2) limitg))
									e2)))))))))))

	       
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus (div (f x) (g x)) (div limitf limitg))) e)
							     )))))))))))





(th~defproblem lim-ex4.2.5.b
	       ;; example 4.2.5.b
	       ;; should work with MULTI EXCEPT that COSIE CRASHES!
	       
	       (in limit)
	       
	       (conclusion (lim (lam (x num) (times (plus (times x x) 1) (minus (times x (times x x)) 4)))
				2
				20)))


(th~defproblem lim-ex4.2.5.d
	       ;; example 4.2.5.d
	       ;; don't know
	       
	       (in limit)
	       
	       (conclusion (lim (lam (x num) (div (minus (times x x) 4) (minus (times 3 x) 6)))
				2
				(div 4 3))))




(th~defproblem lim-sin
	       ;; example 4.2.8.b
	       ;; should work with MULTI with REDUCETOSPECIAL
	       
	       (in limit)

	       (conclusion (lim (lam (x num) (sin x))
				0
				0)))


(th~defproblem lim-cos
	       ;; example 4.2.8.c
	       ;; should work with MULTI with REDUCETOSPECIAL
	       
	       (in limit)

	       (conclusion (lim (lam (x num) (cos x))
				0
				1)))


(th~defproblem lim-sin2
	       ;; example 4.2.8.f
	       ;; should work with MULTI with REDUCETOSPECIAL if theorems are given!
	       
	       (in limit)

	       (conclusion (lim (lam (x num) (times x (sin (div 1 x))))
				0
				0)))



(th~defproblem lim-exerc4.2.1.a
	       (in limit)
	       ;; should work with MULTI

	       (conclusion
		(lim (lam (x num) (times (plus x 1) (plus (times 2 x) 3)))
		     1
		     10)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; section 3


(th~defproblem limr+liml-implies-lim
	       ;; theorem 4.3.3 first part
	       ;; SHOULD WORK WITH MULTI EXCEPT THAT COSIE DOES NOT ACCEPT A CONSTRAINT CURRENTLY!
	       
 	       (in limit)
 	       ;; if the right-hamd limit and the left-hand limit exist, then the limit exists
 	       
 	       (constants  (f (num num)) 
 			   (im num)
 			   (a num))
 	       
 	       (assumption limr
 			   (forall (lam (e1 num)
 					(implies (less 0 e1)
 						 (exists (lam (d1 num)
 							      (and (less 0 d1)
 								   (forall (lam (x1 num)
 										(implies
 										 (and (less (minus x1 a) d1)
 										      (greater (minus x1 a) 0))
 										 (less (absval (minus (f x1) im)) e1)))))))))))
 	       
 	       (assumption liml
 			   (forall (lam (e1 num)
 					(implies (less 0 e1)
 						 (exists (lam (d1 num)
 							      (and (less 0 d1)
 								   (forall (lam (x1 num)
 										(implies
 										 (and (greater (minus x1 a) (minus 0 d1))
 										      (less (minus x1 a) 0))
 										 (less (absval (minus (f x1) im)) e1)))))))))))
 	       
 	       (conclusion limit-f
 			   (forall (lam (e1 num)
 					(implies (less 0 e1)
 						 (exists (lam (d1 num)
 							      (and (less 0 d1)
 								   (forall (lam (x1 num)
 										(implies
 										 (and (less (absval (minus x1 a)) d1)
 										      (greater (absval (minus x1 a)) 0))
 										 (less (absval (minus (f x1) im)) e1)))))))))))
 	       )


(th~defproblem lim-implies-lim-left
	       ;; theorem 4.3.3 second part
	       ;; SHOULD WORK WITH MULTI EXCEPT THAT COSIE DOES NOT ACCEPT A CONSTRAINT CURRENTLY!
	       
 	       (in limit)
 	       ;; if the limit exist, then the left-hand limit exists
	       
 	       (constants  (f (num num)) 
			   (im num)
			   (a num))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
					(implies (less 0 e1)
						 (exists (lam (d1 num)
							      (and (less 0 d1)
								   (forall (lam (x1 num)
										(implies
										 (and (less (absval (minus x1 a)) d1)
										      (greater (absval (minus x1 a)) 0))
										 (less (absval (minus (f x1) im)) e1)))))))))))
	       
	       
	       (conclusion liml
			   (forall (lam (e1 num)
					(implies (less 0 e1)
						 (exists (lam (d1 num)
							      (and (less 0 d1)
								   (forall (lam (x1 num)
										(implies
										 (and (greater (minus x1 a) (minus 0 d1))
										      (less (minus x1 a) 0))
 									   (less (absval (minus (f x1) im)) e1)))))))))))
	       )


(th~defproblem lim-implies-lim-right
	       ;; theorem 4.3.3 third part
	       ;; SHOULD WORK WITH MULTI
	       
 	       (in limit)
 	       ;; if the limit exist, then the right-hand limit exists
	       
 	       (constants  (f (num num)) 
			   (im num)
			   (a num))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
					(implies (less 0 e1)
						 (exists (lam (d1 num)
							      (and (less 0 d1)
								   (forall (lam (x1 num)
										(implies
										 (and (less (absval (minus x1 a)) d1)
										      (greater (absval (minus x1 a)) 0))
										 (less (absval (minus (f x1) im)) e1)))))))))))
	       
	       
	       (conclusion limr
			   (forall (lam (e1 num)
					(implies (less 0 e1)
						 (exists (lam (d1 num)
							      (and (less 0 d1)
								   (forall (lam (x1 num)
										(implies
										 (and (less (minus x1 a) d1)
										      (greater (minus x1 a) 0))
										 (less (absval (minus (f x1) im)) e1)))))))))))
	       )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIVERSE ???

(th~defproblem lim-bound (in limit) 
		(type-constants  i o) ;;real als Sorte anstelle (x in R)
		(constants
		 (f (num num))
		 (g (num num))
		 (limit1 num)
		 (limit2 num)
		 (a num))
		
		(assumption bound-g
		      (exists (lam (c2 num)
				   (forall (lam (x2 num)
						(less (absval (g x2)) c2))))))
	  
		(assumption limit-f
		      (forall (lam (e1 num)
				   (exists (lam (d1 num)
				      (forall (lam (x1 num)
					      (implies (less 0 e1)
						       (and (less 0 d1)
							    (implies
							     (and (less (absval (minus x1 a)) d1)
								  (greater (absval (minus x1 a)) 0))
							     (less (absval (minus (f x1) 0))
								   e1)))))))))))
		
		(conclusion thm
			    (forall (lam (e num)
					 (exists (lam (d num)
						      (forall (lam (x num)
								   (implies (less 0 e)
									    (and (less 0 d)
										 (implies
										  (and (less (absval (minus x a)) d)
										       (greater (absval (minus x a)) 0))
										  (less (absval (minus (times (f x) (g x)) 0))
											e)))))))))))
		)

(th~defproblem lim-divWOzero (in limit) 
	       (constants
		(f (num num))
		(limitf num)
		(a num))


	       (assumption limit-not0 (not (= limitf 0)))
	       (assumption f-not0 (forall (lam (x2 num)
					       (not (= (f x2) 0)))))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (and(less (absval (minus x1 a)) d1)
										 (greater (absval (minus x1 a)) 0))
									     (less (absval (minus (f x1) limitf))
										   e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							     (less (absval (minus (div 1 (f x)) (div 1 limitf))) e)
								  ))))))))))
	       
	       )

(th~defproblem lim-div1 (in limit) 
	       (constants
		(f (num num))
		(limitf num)
		(a num))


	       (assumption not0func (forall (lam (x num) (not (= (f x) 0)))))
	       (assumption not0limit (not (= limitf 0)))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (and (less (absval (minus x1 a)) d1)
										  (greater (absval (minus x1 a)) 0))
									     (less (absval (minus (f x1) limitf))
										   e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (greater (absval (minus x a)) 0))
							      (and (less (absval (minus (div 1 (f x)) (div 1 limitf))) e)
								   (and (not (= (f x) 0))
									(not (= limitf 0))))))))))))))
	       )


;;**************************************************
;;       Theorems about real value sequents
;;**************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; section 1

(th~defproblem lim-seq-thm3.1.5
	       ;; theorem 3.1.5
	       ;; does currently not work with MULTI????????

	       (in limit)
                (constants (c (num))
                           (f (num num)))

 	       (conclusion thm
               
                  (forall (lam (l1 num)
                     (forall (lam (l2 num)
                        (implies
                       
                           (and
  	                     (forall (lam (e num)
                                 (exists (lam (d num)
                                    (forall (lam (x num)
                                       (implies
                                          (less 0 e)
                                          (and (less 0 d)
                                               (implies
          		                         (and
                                                     (less (absval (minus x c)) d)
              	                                    (greater (absval (minus x c)) 0))
                                                  (less (absval
                                                     (minus (f x) l1))
          		                         e))))))))))
                        
  	                     (forall (lam (e num)
                                 (exists (lam (d num)
                                    (forall (lam (x num)
                                       (implies
                                          (less 0 e)
                                          (and (less 0 d)
                                               (implies
          		                         (and
                                                     (less (absval (minus x c)) d)
              	                                    (greater (absval (minus x c)) 0))
                                                  (less (absval
                                                     (minus (f x) l2))
          		                         e)))))))))))
                         
                           (= l1 l2)))))))
	       (help "Uniqueness of Limits of Sequents."))


(th~defproblem lim-seq-xmp3.1.7.a
	       ;;  Example 3.1.7(a) $\lim (1/n)= 0$
	       ;; does currently not work with MULTI	       
	       
	       ;;noch ohne Relativierung  Nat(n), Nat(k) aber mit n >0, k >0
	       (in limit)
	       
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (less 0 k)
						      (implies (less 0 n)
							       (implies
								(greater n k)
								(less (absval (minus (div 1 n) 0))
								      e))))))))))))
	       (help "Example 3.1.7.a"))


(th~defproblem lim-seq-xmp3.1.7.b
	       ;;  Example 3.1.7(b) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (implies (less k n)
							  (less
							   (absval
							    (minus
							     (div 1
								  (times n n)) 0)) e))))))))))
	       (help "Example 3.1.7.b"))

(th~defproblem lim-seq-xmp3.1.7.d
	       ;;  Example 3.1.7(d) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (less k n)
										    (less
										     (absval
										      (minus
										       (div (plus (times 3 n) 2)
											    (plus n 1))
										       3)) e))))))))))
	       (help "Example 3.1.7.d"))

(th~defproblem lim-seq-xmp3.1.11.a
	       ;;  Example 3.1.11.a
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (constants (a num))
	       (assumption ass (greater a 0))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div 1
											    (plus 1 (times n a)))
										       0)) e))))))))))
	       (help "Example 3.1.11.a"))

(th~defproblem lim-seq-xmp3.1.11.b
	       ;;  Example 3.1.11(b) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
		     (forall (lam (e num)
				  (exists (lam (k num)
					       (forall (lam (n num)
							    (implies (less 0 e)
								     (implies (leq k n)
									      (less
									       (absval
										(minus
								(div 1
								     (power 2 n))
								0)) e))))))))))
	 (help "Example 3.1.11.b"))

;; Exercises

(th~defproblem lim-seq-ex3.1.4
	       ;; Exercise 3.1.4
	       ;; does currently not work with MULTI	

	       (in limit)
	       (constants (b num))
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div b n) 0))
										     e))))))))))
	       (help "Exercise 3.1.4"))

(th~defproblem lim-seq-ex3.1.5.a
	       ;; Exercise 3.1.5(a) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div 1 (plus (power n 2) 1)) 0))
										     e))))))))))
	       (help "Exercise 3.1.5.a"))

(th~defproblem lim-seq-ex3.1.5.b
	       ;; Exercise 3.1.5(b) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div (times 2 n) (plus n 1)) 2))
										     e))))))))))
	       (help "Exercise 3.1.5.b"))

(th~defproblem lim-seq-ex3.1.5.c
	       ;; Exercise 3.1.5(c) 
	       ;; does currently not work with MULTI	


	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div (plus (times 3 n) 1)
											    (plus (times 2 n) 5))
										       (div 3 2)))
										     e))))))))))
	       (help "Exercise 3.1.5.c"))

(th~defproblem lim-seq-ex3.1.5.d
	       ;; Exercise 3.1.5(d) 
	       ;; does currently not work with MULTI	
	       
	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div (minus (power n 2) 1)
											    (plus (times 2 (power n 2)) 3))
										       (div 1 2)))
										     e))))))))))
	       (help "Exercise 3.1.5.d"))

(th~defproblem lim-seq-ex3.1.6.b
	       ;; exercise 3.1.6.b
	       ;; does not work with MULTI

	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (div (times 2 n) (plus n 2)) 2))
										     e))))))))))
	       (help "Exercise 3.1.6.b"))

(th~defproblem lim-seq-ex3.1.7.partone
	       ;; exercise 3.1.7 part one
	       ;; should work with MULTI

	       (in limit)
	       (constants (Xn (num num)))

	       (assumption ass
			   (forall (lam (e num)
				   (exists (lam (k num)
				      (forall (lam (n num)
						   (implies (less 0 e)
							    (implies (leq k n)
								     (less
								      (absval
								       (minus
									(Xn n)
									0))
								      e))))))))))
	       
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (absval (Xn n))
										       0))
										     e)))))))))
			   ))

(th~defproblem lim-seq-ex3.1.7.parttwo
	       ;; exercise 3.1.7 part two
	       ;; does not work with MULTI

	       (in limit)
	       (constants (Xn (num num)))

	       (assumption ass
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (implies (leq k n)
										    (less
										     (absval
										      (minus
										       (absval (Xn n))
										       0))
										     e))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (k num)
				      (forall (lam (n num)
						   (implies (less 0 e)
							    (implies (leq k n)
								     (less
								      (absval
								       (minus
									(Xn n)
									0))
								      e))))))))))
	       )

(th~defproblem lim-seq-ex3.1.10
	       ;; Exercise 3.1.10
	       ;; does currently not work with MULTI

	       (in limit)
	       (conclusion thm
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
								  (implies (less 0 e)
									   (and (in k nat)
										(implies (in n nat)
											 (implies (leq k n)
												  (less (absval (minus (minus (div 1 n) (div 1 (plus n 1)))
														       0))
													e))))))))))))
	       (help "Exercise 3.1.(10)"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; section 2

(th~defproblem lim-seq-plus
	       ;; theorem 3.2.3.a part one
	       ;; should work with MULTI

	       (in limit)
	       ;; Juergen Zimmer:
	       ;; If f has a limit 'limit1' in 'a' and
	       ;; g has a limit 'limit2' in 'a' then
	       ;; f+g has the limit 'limit1+limit2' in a.
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (assumption limit-Yn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Yn n) l2))
								      e)))))))))))
			   )
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in  k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus
									       (plus
										(Xn n)
										(Yn n))
									       (plus l1 l2)))
									      e)))))))))))
			   )
	       )


(th~defproblem lim-seq-minus
	       ;; theorem 3.2.3.a part two
	       ;; should work with MULTI

	       (in limit)
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (assumption limit-Yn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Yn n) l2))
								      e)))))))))))
			   )
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in  k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus
									       (minus
										(Xn n)
										(Yn n))
									       (minus l1 l2)))
									      e)))))))))))
			   )
	       )


(th~defproblem lim-seq-times
	       ;; theorem 3.2.3.a part three
	       ;; should work with MULTI

	       (in limit)
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (assumption limit-Yn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Yn n) l2))
								      e)))))))))))
			   )
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in  k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus
									       (times
										(Xn n)
										(Yn n))
									       (times l1 l2)))
									      e)))))))))))
			   )
	       )


(th~defproblem lim-seq-timesconst
	       ;; theorem 3.2.3.a part four
	       ;; should work with MULTI

	       (in limit)
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in  k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus
									       (times
										a
										(Xn n))
									       (times a l1)))
								      e)))))))))))
			   )
	       )


(th~defproblem lim-seq-div
	       ;; theorem 3.2.3.b
	       ;; should work with MULTI except of instantiations

	       (in limit)
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))

	       (assumption ynnot0
			   (forall (lam (n num)
					(implies (in n nat)
						 (or (less (yn n) 0)
						     (greater (yn n) 0))))))
					 
	       
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (assumption limit-Yn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Yn n) l2))
								      e)))))))))))
			   )
	       (conclusion thm
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in  k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus
									       (div (Xn n) (Yn n)) (div l1 l2)))
								      e)))))))))))
			   )
	       )


(th~defproblem lim-seq-bounded
	       ;; theorem 3.2.2
	       ;; should work with MULTI

	       (in limit)
	       (constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))

	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )

	       (conclusion thm
			   (exists (lam (m num)
					(and (less 0 m)
					     (forall (lam (n num)
							  (implies (in n Nat)
								   (less (absval (Xn n)) m))))))))
	       )




(th~defproblem  lim-seq-thm3.2.4-old
		;; theorem 3.2.4
		;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)
		
		;; If $(x_n)$ is a convergent sequence and if for all $n$ $x_n \geq 0$, then$\lim(x_n) \geq 0$.
		;;ganz ohne Relativierung 
		;;l als constant damit assumptions extra formalisiert anstatt als Antecedent (logisch kein Unterschied)

		(in limit)
		(constants
		 (sx (num num))
		 (l num)
		 )
		
		(assumption all-geq0
			    (forall (lam (n2 num)
					 (geq (sx n2) 0))))
		
		
		(assumption x-convergent
			    (forall (lam (e1 num)
					 (exists (lam (k1 num)
						      (forall (lam (n1 num)
								   (implies (less 0 e1)
									    (implies (leq k1 n1)
										     (less (absval (minus (sx n1) l))
											   e1))))))))))
		
		(conclusion thm
			    (leq 0 l))
		
		)


(th~defproblem lim-seq-thm3.2.5-old
	       ;; theorem 3.2.5
	       ;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)

	       (in limit)
	       (constants (Xn (num num))
			  (Yn (num num))
			  (l1 num)
			  (l2 num))
	       ;; if Xn is a convergent sequence
	       (assumption ass1
			   (forall (lam (e num)
					(exists (lam (k num)
						     (forall (lam (n num)
							    (implies (less 0 e)
								     (implies (leq k n)
									      (less
									       (absval
										(minus
										 (Xn n)
										 l1))
									       e))))))))))
	       ;; and if Yn is a convergent sequence
	       (assumption ass2
			   (forall (lam (e num)
				  (exists (lam (k num)
					       (forall (lam (n num)
							    (implies (less 0 e)
								     (implies (leq k n)
									      (less
									       (absval
										(minus
										 (Yn n)
										 l2))
									       e))))))))))

	       ;; and Yn is always greater equal than Xn
	       (assumption ass3
			   (forall (lam (n num)
					(geq (Yn n) (Xn n)))))
	       ;; than l2 is also greater equal than l1
	       (conclusion thm (geq l2 l1)))

(th~defproblem lim-seq-thm3.2.6-old
	       ;; theorem 3.2.6
	       ;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)
	       
	       ;; If $(x_n)$ is a convergent sequence and if for all $n$ $a \leq x_n \geq b$, then$a \leq \lim(x_n) \geq b$.
	       ;;ganz ohne Relativierung 
	       ;;l als constant damit assumptions extra formalisiert anstatt als Antecedent (logisch kein Unterschied)
	       (in limit)
	       (constants
		(sx (num num))
		(l num)
		(a num)
		(b num)
		)
	       (assumption x-convergent
			   (forall (lam (e1 num)
					(exists (lam (k1 num)
						     (forall (lam (n1 num)
								  (implies (less 0 e1)
									   (implies (leq k1 n1)
										    (less (absval (minus (sx n1) l))
											  e1))))))))))
	       (assumption between-ab
			   (forall (lam (n2 num)
					(and (geq (sx n2) a)
					     (leq (sx n2) b)))))
	       
	       
	       (conclusion thm
			   (and (geq l a)
				(leq l b)))
	       
	       )



;;; Reformulation with Nat

(th~defproblem lim-seq-thm3.2.4 (in limit)
	       ;; theorem 3.2.4
	       ;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)
	       
	       (constants
		(Xn (num num))
		(l num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l))
								      e)))))))))))
			   )
		(assumption all-geq0
		   (forall (lam (n2 num)
				(geq (Xn n2) 0))))
    	       (conclusion thm
		      (leq 0 l)))
	       

(th~defproblem  lim-seq-thm3.2.5 (in limit)
		;; theorem 3.2.5
		;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)

		(constants
		(Xn (num num))
		(Yn (num num))
		(l1 num)
		(l2 num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l1))
								      e)))))))))))
			   )
	       (assumption limit-Yn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Yn n) l2))
								      e))))))))))))
	
	       (assumption Xn-geq-Yn
			   (forall (lam (n num)
					(geq (Yn n) (Xn n)))))

	       (conclusion thm (geq l2 l1)))

(th~defproblem lim-seq-thm3.2.6 (in limit)
	       ;; theorem 3.2.6
	       ;; should work with MULTI with the old ENVI methods or with Isolate (except that COSIE does not accept all constraints)
	       
	       (constants
		(Xn (num num))
		(l num) (b num)
		(a num))
	       (assumption limit-Xn
			   (forall (lam (e num)
			   (exists (lam (k num)
			   (forall (lam (n num)
					(implies (less 0 e)
						 (and (in k Nat)
						      (implies (in n Nat)
							       (implies
								(greater n k)
								(less (absval (minus (Xn n) l))
								      e)))))))))))
			   )
	       (assumption between-ab
		      (forall (lam (n2 num)
				   (and (geq (Xn n2) a)
					(leq (Xn n2) b)))))
	       (conclusion thm
		      (and (geq l a)
			   (leq l b)))   )


(th~defproblem lim-seq-thm3.2.7
	       ;; theorem 3.2.7
	       ;; does not work with MULTI
	       
	       ;; If $(x_n), (y_n)$, and $(z_n)$ are sequences of real numbers such that\\
	       ;;$x_n \leq y_n \leq z_n$ for all $ n\in\Nat$,\\
	       ;;and that $\lim(x_n) = \lim(z_n)$. Then $(y_n)$ is convergent and  $\lim(x_n) =
	       ;;\lim(y_n) = \lim(z_n)$
	       ;;PROBLEM: geandert von less auf (leq (sx n4) (sy n4), damit das jetzige Solve* es loesen kann
	       ;;ganz ohne Relativierung 
	       
	       (in limit)
	       (constants
		(sx (num num))
		(sy (num num))
		(sz (num num))
		(limitxs num)
		(limitzs num)
		)
	       (assumption x-convergent
			   (forall (lam (e1 num)
					(exists (lam (k1 num)
						     (forall (lam (n1 num)
								  (implies (less 0 e1)
									   (implies (leq k1 n1)
										    (less (absval (minus (sx n1) limitxs))
											  e1))))))))))
	       
	       
	       (assumption z-convergent
			   (forall (lam (e3 num)
					(exists (lam (k3 num)
						     (forall (lam (n3 num)
								  (implies (less 0 e3)
									   (implies (leq k3 n3)
										    (less (absval (minus (sz n3) limitzs))
											  e3))))))))))
	       (assumption sx<sy<sz
			   (forall (lam (n4 num)
					(and (leq (sx n4) (sy n4))
					     (leq (sy n4) (sz n4))))))
	       
	       (assumption lxs=lzs
			   (= limitxs limitzs))
	       
	       
	       (conclusion thm
			   (exists (lam (limitys num)
					(and (forall (lam (e num)
							  (exists (lam (k num)
								       (forall (lam (n num)
										    (implies (less 0 e)
											     (implies (leq k n)
												      (less (absval (minus (sy n) limitys))
													    e)))))))))
					     (= limitys limitxs)))))
	       )


;;**************************************************
;;           Derivative Examples
;;**************************************************

(th~defproblem derv-a*f
	       ;; thm 6.1.3.a
	       ;; Should work with MULTI except the instantiations ...
	       
	       (in limit)
	       
	       (constants (f (num num))
			  (g (num num))
			  (c num)
			  (a num)
			  (fstrich num)
			  (gstrich num))
	       
	       (assumption derivative-f
			   (lim (lam (x num)
				     (div (minus (f x) (f c)) (minus x c)))
				c
				fstrich))
	       
	       (conclusion der-a*f
			   (lim (lam (x num)
				     (div (minus (times a (f x)) (times a (f c))) (minus x c)))
				c
				(times a fstrich)))
	       )

(th~defproblem derv-plus
	       ;; thm 6.1.3.b
	       ;; Should work with MULTI except the instantiations ...
	       
	       (in limit)
	       
	       (constants (f (num num))
			  (g (num num))
			  (c num)
			  (fstrich num)
			  (gstrich num))
	       
	       (assumption derivative-f
			   (lim (lam (x num)
				     (div (minus (f x) (f c)) (minus x c)))
				c
				fstrich))
	       
	       (assumption derivative-g
			   (lim (lam (x num)
				     (div (minus (g x) (g c)) (minus x c)))
				c
				gstrich))
	       
	       (conclusion derv-plus
			   (lim (lam (x num)
				     (div (minus (plus (f x) (g x)) (plus (f c) (g c))) (minus x c)))
				c
				(plus fstrich gstrich)))
	       )


(th~defproblem derv-times
	       ;; thm 6.1.3.c
	       ;; Should work with MULTI except the instantiations ...
	       
	       (in limit)
	       
	       (constants (f (num num))
			  (g (num num))
			  (c num)
			  (fstrich num)
			  (gstrich num))
	       
	       (assumption derivative-f
			   (lim (lam (x num)
				     (div (minus (f x) (f c)) (minus x c)))
				c
				fstrich))
	       
	       (assumption derivative-g
			   (lim (lam (x num)
				     (div (minus (g x) (g c)) (minus x c)))
				c
				gstrich))
	       
	       (conclusion derv-plus
			   (lim (lam (x num)
				     (div (minus (times (f x) (g x)) (times (f c) (g c))) (minus x c)))
				c
				(times fstrich gstrich)))
	       )




 #|
(th~defproblem cont-derv (in limit) ;;Erica Melis
	       ;;if function has a derivative at a, then
	       ;;f is continuous in a.
	       ;;expanded without ex-quantifiers 
	       (type-constants  i o) 
	       (constants  (f (num num)) (g (num num))
			   (focus (o o))
			   (a num) (fstrich num))

	       (assumption absval-0
		      (forall (lam (z num)
				   (= (absval (minus z z))
				      0))))
	  
	       (assumption deriv
		      (forall (lam (e1 num)
			      (exists (lam (d1 num)
				      (forall (lam (x1 num)
					      (implies (less 0 e1)
							(and (less 0 d1)
							     (implies
							      (less (absval (minus x1 a)) d1)
							      (implies (not (= x1 a))
								       (less (absval (minus (div (minus (f x1) (f a))
												 (minus x1 a))
											    fstrich))
									     e1))))))))))))
	       (conclusion thm
		      (forall (lam (e num)
			      (exists (lam (d num)
					   (forall (lam (x num)
					      (implies (less 0 e)
							(and (less 0 d)
							     (implies
							      (less (absval (minus x a)) d)
							      (less (absval (minus (f x) (f a)))
								    e)))))))))))
	       )|#

(th~defproblem cont-derv
	       ;; thm 6.1.2
	       ;; SHOULD WORK WITH MULTI
	       
 	       (in limit)
	       (constants  (f (num num)) (g (num num))
  			   (focus (o o))
  			   (a num) (fstrich num))
 	       
  	       (assumption deriv
 			   (forall (lam (e1 num)
 					(exists (lam (d1 num)
 						     (forall (lam (x1 num)
 								  (implies (less 0 e1)
 									   (and (less 0 d1)
										(implies
 										 (and (less (absval (minus x1 a)) d1)
 										      (greater (absval (minus x1 a)) 0))
 										 (less (absval (minus (div (minus (f x1) (f a))
 													   (minus x1 a))
 												      fstrich))
 										       e1)))))))))))
  	       (conclusion thm
 			   (forall (lam (e num)
 					(exists (lam (d num)
 						     (forall (lam (x num)
 								  (implies (less 0 e)
 									   (and (less 0 d)
 										(implies
 										 (less (absval (minus x a)) d)
 										 (less (absval (minus (f x) (f a)))
 										       e)))))))))))
  	       )




;;;;;;;;;
;;; examples used for the analogy replay
;;;;;;;;;


(th~defproblem limit-analogy-source (in limit)
	 (constants (Xn (num num)))
	 (conclusion thm
		     (implies
		      (limseq Xn 0)
		      (limseq (lam (n num) (power (Xn n) 2)) 0)
		      ))
	 (help "Analogy example (source)"))

(th~defproblem limit-analogy-target (in limit)
	 (constants (Yn (num num)))
	 (conclusion thm
		      (limseq (lam (n num) (absval (Yn n))) 0)
		      )
	 (help "Analogy example (target)"))

;;;;;;;;;
;;; examples with easy names for analogy
;;;;;;;;;

;;analogy source 1
(th~defproblem lim-a1-xmp4.1.7.e (in limit) 
	       (type-constants  i o) 
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x 2)) d)
									(greater (absval (minus x 2)) 0))
								   (less (absval (minus
										  (div(minus(power x 3)4)
										      (plus(power x 2)1))
										  (div 4 5)))
									 e)))))))))))
	       (help "Example 4.1.7.e, for analogy"))

(th~defproblem lim-a1-ex4.1.10.a (in limit)
	       (conclusion thm ;; x > 1
			   (forall (lam (e num)
			   (exists (lam (d num)
			   (forall (lam (x num)
					(implies (less 0 e)
						 (and (less 0 d)
						      (implies
						       (less 1 x)
						       (implies
							(and (less (absval (minus x 2)) d)
							     (greater (absval (minus x 2)) 0))
							(less (absval (minus (div 1 (minus 1 x))
									     -1))
							      e))))))))))))
	       (help "Exercise 4.1.(10).a, for analogy"))


(th~defproblem lim-a1-ex4.1.10.b (in limit)
	       (conclusion thm ;; x <> 0
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								 (implies (less 0 x)
									  (implies
									   (and (less (absval (minus x 2)) d)
										(greater (absval (minus x 2)) 0))
									   (less (absval
										  (minus (div x (plus 1 x))
											 (div 1 2)))
										 e))))))))))))
	       (help "Exercise 4.1.(10).b, for analogy"))


(th~defproblem lim-a1-ex4.1.10.d (in limit)
	       (conclusion thm ;; x <> 0
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x 2)) d)
									(greater (absval (minus x 2)) 0))
								   (less (absval
									  (minus
									   (div (plus (minus (power x 2) x) 1)
										(plus x 1))
									   (div 1 2)))
									 e)))))))))))
	       (help "Exercise 4.1.(10).d, for analogy"))


#|
;;;;;;;;
;;;limes-variationen 
;;;;;;;;

(th~defproblem lim2-plus (in limit)
	       ;; Juergen Zimmer:
	       ;; If f has a limit 'limit1' in 'a' and
	       ;; g has a limit 'limit2' in 'a' then
	       ;; f+g has the limit 'limit1+limit2' in a.
	       (constants
		(f (num num))
		(g(num num))
		(limit1 num)
		(limit2 num)
		(a num))
	       (assumption limit-g
		     (forall (lam (e2 num)
				  (exists (lam (d2 num)
					       (forall (lam (x2 num)
							    (and (less 0 d2)
								 (implies
								  (and (less (absval (minus x2 a)) d2)
								       (not (= x2 0)))
								  (less (absval (minus(g x2) limit2))
									e2))))))))))
	       (assumption limit-f
			   (forall (lam (e1 num)
					(exists (lam (d1 num)
						     (forall (lam (x1 num)
								  (implies (less 0 e1)
									   (and (less 0 d1)
										(implies
										 (and (less (absval (minus x1 a)) d1)
										      (not (= x1 0)))
										 (less (absval (minus (f x1) limit1))
										       e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
					       (forall (lam (x num)
							    (implies (less 0 e)
								     (and (less 0 d)
									  (implies
									   (and (less (absval (minus x a)) d)
										(not (= x 0)))
									    (less (absval (minus (plus (f x)
												       (g x))
												 (plus limit1
												       limit2)))
										  e)))))))))))
	       )




(th~defproblem lim2-square (in limit) 
	       ;;Erica Melis  lim(x^2)= a^2 
	       ;;needs to be changed when Def-expansion methods are available in OMEGA
	       ;;ass:(= f (lam x (times x x))) conclusion: (lim f a (times a a))
	       ;;needed are 2 defn-expand methods: from theory and from assumption
	       
	       (type-constants  i o) ;;real als Sorte anstelle (x in R)
	       (constants  
		(a num))
	       
	       (conclusion thm
			   (forall (lam (e num)
				   (exists (lam (d num)
					   (forall (lam (x num)
						   (implies (less 0 e)
							    (and (less 0 d)
								  (implies
								   (and (less (absval (minus x a)) d)
									(not (= x 0)))
								   (less (absval (minus (times x x)
											(times a a)))
									 e)))))))))))
	       )
	  

(th~defproblem lim2-composite
	       (in limit)
	       ;;Erica Melis   if function f is continuous in g(a) and
	       ;;function g is continous in a and g(a), then f(g(x))
	       ;;is continuous in a.
	       
	       (type-constants  i o) 
	       (constants  (f (num num)) (g (num num))
		     (a num))
	       
	       (assumption limit-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
					     (implies (less 0 e2)
						       (and (less 0 d2)
							      (implies
							       (and (less (absval (minus x2 a)) d2)
								    (not (= x2 0)))
							       (less (absval
								      (minus (g x2) (g a)))
								     e2)))))))))))
	       (assumption limit-f
		     (forall (lam (e1 num)
			     (exists (lam (d1 num)
					  (forall (lam (x1 num)
					     (implies (less 0 e1)
						       (and (less 0 d1)
						      (implies
						       (and (less (absval (minus x1 (g a))) d1)
							    (not (= x1 0)))
						       (less (absval
							      (minus (f x1) (f (g a))))
							     e1)))))))))))
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (not (= x 0)))
							    (less (absval
								   (minus (f (g x))
									  (f (g a))))
								  e)))))))))))
	       )






(th~defproblem bound2 (in limit) 
                (type-constants  i o) ;;real als Sorte anstelle (x in R)
                (constants
                 (f (num num))
                 (g (num num))
                 (limit1 num)
                 (limit2 num)
                 (a num))
                
                (assumption bound-g
                      (exists (lam (c2 num)
                                   (forall (lam (x2 num)
                                                (less (absval (g x2)) c2))))))
          
                (assumption limit-f
                      (forall (lam (e1 num)
                                   (exists (lam (d1 num)
                                      (forall (lam (x1 num)
                                              (implies (less 0 e1)
                                                       (and (less 0 d1)
                                                            (implies
                                                             (and(less (absval (minus x1 a)) d1)
                                                                 (not (= x1 0)))
                                                             (less (absval (minus (f x1) 0))
                                                                   e1)))))))))))
                
                (conclusion thm
                            (forall (lam (e num)
                                         (exists (lam (d num)
                                                      (forall (lam (x num)
                                                                   (implies (less 0 e)
                                                                            (and (less 0 d)
                                                                                 (implies
                                                                                  (less (absval (minus x a)) d)
                                                                                  (less (absval (minus (times (f x) (g x)) 0))
                                                                                        e)))))))))))
;                )

(th~defproblem lim2-times (in limit) 
	       ;; Juergen Zimmer:
	       ;; If f has a limit 'limit1' in 'a' and
	       ;; g has a limit 'limit2' in 'a' then
	       ;; f*g has the limit 'limit1*limit2' in a.
	       (type-constants  i o num) ;;num als Sorte anstelle (x in R)
	       (constants
		(f (num num))
		(g (num num))
		(limit1 num)
		(limit2 num)
		(a num))
	 
	       (assumption limit-f
		     (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
						   (implies (less 0 e1)
							     (and (less 0 d1)
								  (implies
								   (and (less (absval (minus x1 a)) d1)
									(not (= x1 0)))
								   (less (absval (minus (f x1) limit1))
									 e1)))))))))))
	 
	       (assumption limit-g
		     (forall (lam (e2 num)
			     (exists (lam (d2 num)
				     (forall (lam (x2 num)
						  (implies (less 0 e2)
							    (and (less 0 d2)
								 (implies
								  (and (less (absval (minus x2 a)) d2)
								       (not (= x2 0)))
								  (less (absval (minus (g x2) limit2))
									e2)))))))))))
	       (conclusion thm
		     (forall (lam (e num)
			     (exists (lam (d num)
					  (forall (lam (x num)
						       (implies (less 0 e)
								(and (less 0 d)
								     (implies
								      (and (less (absval (minus x a)) d)
									   (not (= x 0)))
								      (less (absval (minus (times (f x)
												  (g x))
											   (times limit1
												  limit2)))
									    e)))))))))))
	       )


(th~defproblem lim2-div (in limit) 
	       (constants
		(f (num num))
		(limit num)
		(a num))


	       (assumption notneg (not (= limit 0)))
	       
	       (assumption limit-f
			   (forall (lam (e1 num)
				  (exists (lam (d1 num)
					       (forall (lam (x1 num)
							     (implies (less 0 e1)
								       (and (less 0 d1)
									    (implies
									     (and(less (absval (minus x1 a)) d1)
										 (not (= x1 0)))
									     (less (absval (minus (f x1) limit))
										   e1)))))))))))
	       (conclusion thm
			   (forall (lam (e num)
				  (exists (lam (d num)
				     (forall (lam (x num)
					     (implies (less 0 e)
						       (and (less 0 d)
							    (implies
							     (and (less (absval (minus x a)) d)
								  (not (= x 0)))
							     (and (less (absval (minus (div 1 (f x)) (div 1 limit))) e)
								  (not (= (f x) 0)))))))))))))
	       )

|#

