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
(in-package :omega)


(th~defproblem cantor
	 (in function)
	 (conclusion
	  (forall (lam (G (o i))
		       (smaller-cardinality G (powerset G))))))

(th~defproblem cantor-simple
	 (in function)
	 (conclusion
	  (not (exists (lam (f ((o i) i))
			    (forall (lam (g (o i))
					 (exists (lam (y i)
						      (= (f y) g))))))))))

(th~defproblem thm15b
	 (in function)
	 (conclusion
	  (forall (lam (f (i i))
		       (implies
			(exists (lam (g (i i))
				     (and (iteratep+ f g)
					  (exists (lam (x i)
						       (and (= (g x) x)
							    (forall (lam (z i)
									 (implies
									  (= (g z) z)
									  (= z x))))))))))
			(exists (lam (y i) (= (f y) y))))))))

(th~defproblem thm47
	 (in function)
	 (conclusion
	  (forall (lam (X i)
	   (forall (lam (Y i)
            (equiv			
	     (forall (lam (Q (o i))
			  (implies (Q X) (Q Y))))
	     (forall (lam (R (o i i))
			  (forall (lam (Z i)
				       (implies (R Z Z) (R X Y)))))))))))))
			
(th~defproblem thm48
	 (in function)
	 (conclusion
	  (forall (lam (f (i i))
	   (forall (lam (g (i i))
            (implies
	     (and (injectivep f) (injectivep g))
	     (injectivep (compose-functions g f)))))))))

(th~defproblem thm134
	 (in function)
	 (conclusion
	  (forall (lam (Z i)
	   (forall (lam (G (i i))
			(implies
			 (iteratep+ (lam (X i) Z) G)
			 (forall (lam (U i)
				      (= (G U) Z))))))))))

(th~defproblem thm135
	 (in function)
	 (type-constants aa)
	 (conclusion
	  (forall (lam (f (aa aa))
	   (forall (lam (g1 (aa aa))
	    (forall (lam (g2 (aa aa))	
	     (implies
	      (and (iteratep f g1) (iteratep f g2))
	      (iteratep f (compose-functions g2 g1)))))))))))

(th~defproblem thm270
	 (in function)
	 (type-constants a c)
	 (constants (*1 (a a a))
		    (*2 (a a a))
		    (*3 (c c c)))
	 (conclusion 
	  (forall (lam (f (a a))
	    (forall (lam (g (c a))
	      (forall (lam (h (c a)) 
		 (implies  (and  
			    (and  
			     (and (forall (lam (x a) 
				    (= (h (f x)) (g x)))) 
				  (forall (lam (y a)
				    (exists (lam (x a) 
				      (= (f x) y)))))) 
			     (forall (lam (x a)
			       (forall (lam (y a) 
				 (=  (f  (*1 x y))
				     (*2  (f x)  (f y))))))))
			    (forall (lam (x a)
			      (forall (lam (y a) 
			        (=  (g  (*1 x y))  
				    (*3 (g x) (g y))))))))
			   (forall (lam (x a)
			     (forall (lam (y a) 
					  (=  (h (*2 x y))  
					      (*3 (h x) (h y))))))))))))))
))

(th~defproblem distrib-thm
(in function)
(conclusion 
(forall (lam (JOIN ( (i i) i))(forall (lam (MEET ( (i i) i)) (implies  (and  (and  (and  (and  (and  (and  (and (forall (lam (x i) (=  ( (JOIN x) x) x))) (forall (lam (x i) (=  ( (MEET x) x) x)))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (JOIN  ( (JOIN x) y)) z)  ( (JOIN x)  ( (JOIN y) z)))))))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (MEET  ( (MEET x) y)) z)  ( (MEET x)  ( (MEET y) z)))))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (JOIN x) y)  ( (JOIN y) x))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (MEET x) y)  ( (MEET y) x))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (JOIN  ( (MEET x) y)) y) y)))))) (forall (lam (x i)(forall (lam (y i) (=  ( (MEET  ( (JOIN x) y)) y) y))))))  (= (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (JOIN x)  ( (MEET y) z))  ( (MEET  ( (JOIN x) y))  ( (JOIN x) z))))))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (MEET x)  ( (JOIN y) z))  ( (JOIN  ( (MEET x) y))  ( (MEET x) z)))))))))))))))
))

(th~defproblem grp-comm2
(in function)
(constants (P (i i i))
	   (E i))
(conclusion 
 (implies  (and  (and  (and (forall (lam (x i) (=  ( (P E) x) x))) (forall (lam (y i) (=  ( (P y) E) y)))) (forall (lam (z i) (=  ( (P z) z) E)))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (P  ( (P x) y)) z)  ( (P x)  ( (P y) z)))))))))) (forall (lam (a i)(forall (lam (b i) (=  ( (P a) b)  ( (P b) a)))))))
))

(th~defproblem cd-lattice-thm
(in function)
(conclusion 
(forall (lam (JOIN ( (i i) i))(forall (lam (MEET ( (i i) i))(forall (lam (TOP i)(forall (lam (BOTTOM i) (implies  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and (forall (lam (x i) (=  ( (JOIN x) x) x))) (forall (lam (x i) (=  ( (MEET x) x) x)))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (JOIN  ( (JOIN x) y)) z)  ( (JOIN x)  ( (JOIN y) z)))))))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (MEET  ( (MEET x) y)) z)  ( (MEET x)  ( (MEET y) z)))))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (JOIN x) y)  ( (JOIN y) x))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (MEET x) y)  ( (MEET y) x))))))) (forall (lam (x i)(forall (lam (y i) (=  ( (JOIN  ( (MEET x) y)) y) y)))))) (forall (lam (x i)(forall (lam (y i) (=  ( (MEET  ( (JOIN x) y)) y) y)))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (MEET x)  ( (JOIN y) z))  ( (JOIN  ( (MEET x) y))  ( (MEET x) z)))))))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (=  ( (JOIN x)  ( (MEET y) z))  ( (MEET  ( (JOIN x) y))  ( (JOIN x) z))))))))))  (and (forall (lam (x i) (=  ( (MEET TOP) x) x))) (forall (lam (x i) (=  ( (JOIN TOP) x) TOP)))))  (and (forall (lam (x i) (=  ( (MEET BOTTOM) x) BOTTOM))) (forall (lam (x i) (=  ( (JOIN BOTTOM) x) x))))) (forall (lam (x i)(exists (lam (y i) (and  (=  ( (JOIN x) y) TOP)  (=  ( (MEET x) y) BOTTOM))))))) (forall (lam (x i)(forall (lam (y i)(forall (lam (z i) (implies  (and  (and  (=  ( (JOIN x) y) TOP)  (=  ( (MEET x) y) BOTTOM))  (and  (=  ( (JOIN x) z) TOP)  (=  ( (MEET x) z) BOTTOM)))  (= y z)))))))))))))))))
))



(th~defproblem set-474+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion set-474+4
			   (forall (lam (f (bb aa))
					(forall (lam (g (cc bb))
						     (forall (lam (ord1 (o aa aa))
								  (forall (lam (ord2 (o bb bb))
									       (forall (lam (ord3 (o cc cc))
		(implies
		 (and (increasing-wrt-ord f ord1 ord2)
		      (decreasing-wrt-ord g ord2 ord3))
		 (decreasing-wrt-ord (compose-functions f g) ord1 ord3))))))))))))))

(th~defproblem set-716+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion set-716+4
			   (forall (lam (f (bb aa))
					(forall (lam (g (aa bb))
						     (implies 
						      (and (injectivep f)
							   (injectivep g))
						      (injectivep (compose-functions f g)))))))))
							   


;;; Leo with Bliksem  (LEO SOS empty)
(th~defproblem set-724+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion set-724+4
			   (forall (lam (f (bb aa))
					(forall (lam (g (cc bb))
						     (forall (lam (h (cc bb))
								  (implies 
								   (and (= (compose-functions f g)
									   (compose-functions f h))
									(surjectivep-mateja-chris f))
								   
								   (= g h))))))))))


;;; Leo alone   (but also with Bliksem)
(th~defproblem set-747+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion set-747+4
			   (forall (lam (f (bb aa))
					(forall (lam (g (cc bb))
						     (forall (lam (ord1 (o aa aa))
								  (forall (lam (ord2 (o bb bb))
									       (forall (lam (ord3 (o cc cc))
		(implies
		 (and (increasing-wrt-ord f ord1 ord2)
		      (decreasing-wrt-ord g ord2 ord3))
		 (decreasing-wrt-ord (compose-functions f g) ord1 ord3))))))))))))))

(th~defproblem set-752+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion set-752+4
			   (forall (lam (X (o aa))
					(forall (lam (Y (o aa))
						     (forall (lam (F (bb aa))
                (= (function-domain-to-range F (union X Y))
		   (union (function-domain-to-range F X) (function-domain-to-range F Y)))))))))))

(th~defproblem set-753+4
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion (forall (lam (X (o aa))
					(forall (lam (Y (o aa))
						     (forall (lam (F (bb aa))
                (subset (function-domain-to-range F (intersection X Y))
			(intersection (function-domain-to-range F X) (function-domain-to-range F Y)))))))))))


;;(th~defproblem set-764+4
;;               (in function)
;;               (type-constants aa bb cc)
;;               (conclusion 
;;                (forall (lam (f ((o bb) (o aa)))
;;                             (f emptysetforall (lam (g (cc bb))
;;                                          (forall (lam (h (cc bb))
;;                                                       (implies 
;;                                                        (and (= (compose-functions f g)
;;                                                                (compose-functions f h))
;;                                                                        (surjectivep-mateja-chris f))
;;                                                        
;;                                                        (= g h))))))))))

