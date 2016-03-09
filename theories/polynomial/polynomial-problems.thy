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

(th~defproblem POL-ADD1
	       (in polynomial)
	       (conclusion (= (p-plus-r1 (lam (X num) (plus (times 3
								   (power X 2))
							    (plus (times 2
									 (power X 1))
								  (times 5 (power X 0)))))
					 (lam (X num) (plus (times 5
								   (power X 3))
							    (plus (times 1
									 (power X 1))
								  (times 2 (power X 0))))))
			      (lam (X num) (plus (times 5
							(power X 3))
						 (plus (times 3
							      (power X 2))
						       (plus (times 3
								    (power X 1))
							     (times 7 (power X 0)))))))))



(th~defproblem pol-add2 (in polynomial)
	 (conclusion THM (= (p-plus-r2 (lam (X num)
					    (lam (Y num)
						 (plus (times 1
							     (times (power X 2)
								   (power Y 1)))
						       (times 2
							     (times (power X 1)
								   (power Y 0))))))
				       (lam (X num)
					    (lam (Y num)
						 (plus (times 1
							     (times (power X 1)
								   (power Y 0)))
						       (plus (times 1
								   (times (power X 0)
									 (power Y 1)))
							     (times 1
								   (times (power X 0)
									 (power Y 0))))))))
			    (lam (X num)
				 (lam (Y num)
				      (plus (times 1
						  (times (power X 2)
							(power Y 1)))
					    (plus (times 3
							(times (power X 1)
							      (power Y 0)))
						  (plus (times 1
							      (times (power X 0)
								    (power Y 1)))
							(times 1
							      (times (power X 0)
								    (power Y 0))))))))))
	 )
							




(th~defproblem pol-add3 (in polynomial)
	 (conclusion (= (p-plus-r1 (lam (X num) (plus (times 3
							     (power X 2))
						      (plus (times 2
								   (power X 1))
							    (times 5 (power X 0)))))
				   (lam (X num) (plus (times 5
							     (power X 3))
						      (times 1
							     (power X 1))
							    )))
			(lam (X num) (plus (times 5
						  (power X 3))
					   (plus (times 3
							(power X 2))
						 (plus (times 3
							      (power X 1))
						       (times 5 (power X 0))))))))
	)





(th~defproblem pol-add4 (in polynomial)
	 (conclusion (= (p-plus-r1 (lam (X num) (plus (times 3
							     (power X 2))
						      (times 2
							     (power X 1))
						      ))
				   (lam (X num) (plus (times 5
							     (power X 3))
						      (plus (times 1
								   (power X 1))
							    (times 2 (power X 0))))))
			(lam (X num) (plus (times 5
						  (power X 3))
					   (plus (times 3
							(power X 2))
						 (plus (times 3
							      (power X 1))
						       (times 2 (power X 0))))))))
	)



(th~defproblem POL-ADD5
	       (in polynomial)
	       (conclusion (= ((p-plus-r1 (lam (X num) (plus (times 3
								   (power X 2))
							    (plus (times 2
									 (power X 1))
								  (times 5 (power X 0)))))
					 (lam (X num) (plus (times 5
								   (power X 3))
							    (plus (times 1
									 (power X 1))
								  (times 2 (power X 0)))))) 4)
			      25)))

(th~defproblem POL-ADD6
	       (in polynomial)
	       (assumption A0 (= (lam (z num)
				      (plus
				       ((lam (X num) (plus (times 3
								  (power X 2))
							   (plus (times 2
									(power X 1))
								 (times 5 (power X 0))))) z)
				       ((lam (X num) (plus (times 5
								  (power X 3))
							   (plus (times 1
									(power X 1))
								 (times 2 (power X 0))))) z)))
				 (lam (X num) (plus (times 5
							   (power X 3))
						    (plus (times 3
								 (power X 2))
							  (plus (times 3
								       (power X 1))
								(times 7 (power X 0))))))))
	       (conclusion (= (p-plus-r1 (lam (X num) (plus (times 3
								   (power X 2))
							    (plus (times 2
									 (power X 1))
								  (times 5 (power X 0)))))
					 (lam (X num) (plus (times 5
								   (power X 3))
							    (plus (times 1
									 (power X 1))
								  (times 2 (power X 0))))))
			      (lam (X num) (plus (times 5
							(power X 3))
						 (plus (times 3
							      (power X 2))
						       (plus (times 3
								    (power X 1))
							     (times 7 (power X 0)))))))))




(th~defproblem Totmin-test
	       (in polynomial)
	       (conclusion (exists (lam (x num)
					(total-minimum x
						       (lam (X num) (plus (times 3
										 (power X 2))
									  (plus (times -12
										       (power X 1))
										(times 9 (power X 0)))))
						       (closed-interval-bounds 1 7))))))


(th~defproblem pol-mult1 (in polynomial)
	  (conclusion THM (= (p-times-r1 (lam (X num) (plus (times 3
								 (power X 2))
							   (plus (times 2
								       (power X 1))
								 (times 5 (power X 0)))))
					(lam (X num) (plus (times 5
								 (power X 3))
							   (plus (times 1
								       (power X 1))
								 (times 2 (power X 0))))))
			     (lam (x num)                                                                  
				  (plus (times 15 (power x 5))                                              
					(plus (times 10 (power x 4))                                        
					      (plus (times 28 (power x 3))                                  
						    (plus                                                 
						     (times 8 (power x 2))                                  
						     (plus                                                
						      (times 9 (power x 1))                                 
						      (times 10 (power x 0))))))))))
	  )



(th~defproblem pol-mult2 (in polynomial)
	 (conclusion THM (= ((p-times-r1 (lam (X num) (plus (times 6
								  (power X 2))
							   (plus (times 2
									(power X 1))
								 (times 5 (power X 0)))))
					(lam (X num) (plus (times 5
								 (power X 3))
							   (plus (times 3
								       (power X 1))
								 (times -1 (power X 0))))))
			     2)
			    1485)))

(th~defproblem pol-stimes1 (in polynomial)
	 (conclusion THM (= (s-times-r1 (lam (X num) (plus (times 5
								  (power X 3))
							   (plus (times 1
									(power X 1))
								 (times 2 (power X 0)))))
					5)
			    (lam (x num)                                                                  
				 (plus (times 25 (power x 3))                                  
				       (plus (times 5 (power x 1))                                 
					     (times 10 (power x 0)))))))
	  )




(th~defproblem pol-diff1 (in polynomial)
	 (conclusion THM (= (p-deriv-r1 (lam (X num)
					     (plus (times 1
							 (power X 3))
						   (plus (times 3
							       (power X 2))
							 (plus (times 1
								     (power X 1))
							       (times 1 (power X 0)
								     )))))
					1)
			    (lam (X num) (plus (times 3 (power X 2))
					       (plus (times 6 (power X 1))
						     (times 1 (power X 0)))))))
	 )
							


(th~defproblem pol-diff2 (in polynomial)
	  (conclusion THM (= ((p-deriv-r1 (lam (X num)
					       (plus (times 1
							   (power X 3))
						     (plus (times -2
								    (power X 2))
							      (plus (times 1
									  (power X 1))
								    (times 1 (power X 0)
										)))))
					  1)
			      1)
			     0))
	  )


(th~defproblem pol-diff3 (in polynomial)
	 (conclusion THM (= (p-deriv-r1 (lam (X num)
					     (plus (times 1
							 (power X 3))
						   (plus (times 3
							       (power X 2))
							 (times 1
								(power X 1)))))
					1)
			    (lam (X num) (plus (times 3 (power X 2))
					       (plus (times 6 (power X 1))
						     (times 1 (power X 0)))))))
	 )
							








;(th~defproblem rolles-thm
;              (in polynomial)
;              (conclusion
;               (forall (lam (a num)
;                (forall (lam (b num)
;                 (forall-sort  (lam (F (num num))
;                  (implies (leq (degree F) 2)
;                   (forall (lam (min num)
;                    (implies (and (local-minimum min F)
;                                  (and (greater (F a) (F min))
;                                       (greater (F b) (F min))))
;                             (total-minimum min F (closed-interval-bounds a b))))))) polynomial))))))
;              (help "Rolles Theorem: For polynomials of degree leq 2, total minima on a 
;                     closed interval I must be  either on the bounds of I or at a local minimum."))

