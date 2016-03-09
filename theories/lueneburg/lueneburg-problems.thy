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

(th~deftheorem JOERG (in lueneburg)
	  (conclusion Comm
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(= (* x y) (* y x)))))))
	  )


(th~deftheorem LUE1-1 (in lueneburg)
	       (conclusion theo1.1 (forall (lam (r ff)
						(forall (lam (s ff)
							     (implies (= (+ r s) r)
								      (= s null)))))))
	       )



(th~deftheorem LUE1-2 (in lueneburg)
	  (conclusion theo1.2 (forall (lam (r ff)
					   (forall (lam (t ff)
							(and (= (+ r (+ (- r) t)) t)
							     (forall (lam (s ff)
									  (implies (= (+ r s) t)
										   (= s (+ (- r) t)))))))))))
	  )




(th~deftheorem LUE1-3 (in lueneburg)
	  (conclusion theo1.3 (forall (lam (s ff)
					   (implies (forall (lam (r ff)
								 (and (= (* r s) r)
								      (= (* s r) r))))
						    (= s eins)))))
	  )
								 




(th~deftheorem LUE1-4 (in lueneburg)
	  (conclusion theo1.4 (forall (lam (r ff)
					   (and (= (* r null) null)
						(= (* null r) null)))))
	  )

	
(th~deftheorem LUE1-1i (in lueneburg)
	       (conclusion
		theo1.1
		(forall (lam (r ff)
			     (forall (lam (s ff)
					  (implies (and (and (ring r)
							     (ring s))
							(= (+ r s) r))
						   (= s null)))))))
	       )

(th~deftheorem LUE1-2 (in lueneburg)
	       (conclusion
		theo1.2
		(forall (lam (r ff)
			     (forall (lam (t ff)
					  (implies (and (ring r)
							(ring t))
						   (and (= (+ r (+ (- r) t)) t)
							(forall (lam (s ff)
								     (implies (ring s)
									      (implies (= (+ r s) t)
										       (= s (+ (- r) t)))))))))))))
	       )


(th~deftheorem LUE1-3i (in lueneburg)
	  (conclusion theo1.3 (forall (lam (s ff)
					   (implies (and (ring s)
							 (forall (lam (r ff)
								      (and (ring r)
									   (and (= (* r s) r)
										(= (* s r) r))))))
						    (= s eins)))))
	  )

(th~deftheorem LUE1-4i (in lueneburg)
	  (conclusion theo1.4 (forall (lam (r ff)
					   (implies (ring r)
						    (and (= (* r null) null)
							 (= (* null r) null))))))
	  )

(th~deftheorem SATZ-1-11-hin (in lueneburg)
	   (conclusion satz1.11.hin
		       (forall (lam (a ff)
				    (implies (< eins a)
					     (and (< null (inv a))
						  (< (inv a) eins))))))

	   )

(th~deftheorem  Satz-1.5
		(in lueneburg)
		(conclusion conc
		      (< null eins)))

(th~deftheorem  Satz-1.10a
		(in lueneburg)
		(conclusion conc
			    (forall (lam (x ff)
				   (implies (not (= x null))
					    (implies (< null x)
						     (< null (inv x))))))))

(th~deftheorem  Tricho-a
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(implies (= x y)
							 (not (< y x)))))))))

(th~deftheorem  Tricho-b
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(implies (< x y)
							 (not (< y x)))))))))

(th~deftheorem  Tricho-not=
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(implies (or (< y x) (< x y))
							 (not (= x y)))))))))

(th~deftheorem  Transitivity
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(forall (lam (z ff)
							     (implies (and (< x y)
									   (< y z))
								      (< x z))))))))))
(th~deftheorem  Monotony
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (forall (lam (y ff)
						(forall (lam (z ff)
							     (implies (and (< x y)
									   (< null z))
								      (< (* x z) (* y z)))))))))))

(th~deftheorem  Inverse-Mult
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (implies (not (= x null))
					    (= (* x (inv x)) eins))))))

(th~deftheorem  Neutral-Mult
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (= (* eins x) x)))))

(th~deftheorem  Neutrals
		(in lueneburg)
		(conclusion conc
		      (not (= null eins))))

(th~deftheorem  Uebung
		(in lueneburg)
		(conclusion conc
		      (forall (lam (x ff)
				   (= (* null x) null)))))

(th~deftheorem  ass-identity-rigth
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (= (* x e) x)))))
	 
(th~deftheorem  ass-identity-left
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (= (* e x) x)))))
	 
(th~deftheorem  ass-inverse-rigth
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (= (* x (inv x)) e)))))
	 
(th~deftheorem  ass-inverse-left
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (= (* (inv x) x) e)))))
	 
(th~deftheorem  ass-assozitivity
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* (* x y) z) (* x (* y z)))))))))))
	 
	  

		 

	  
(th~deftheorem GRP-10
	       (in lueneburg)
	       (conclusion
		theo
		(forall (lam (x ff)
			     (forall (lam (y ff)
					  (implies (= (inv x) y) (= (inv y) x))))))))


		



(th~defproblem GRP-12
	 (in lueneburg)
	 (conclusion theo (forall (lam (x ff)
				       (forall (lam (y ff)
						    (= (* (inv y) (inv x)) (inv (* x y)))))))))


		

	 
(th~defproblem  kom+
		(in lueneburg)
		(conclusion conc
			    (forall (lam (x ff)
					 (forall (lam (y ff)
						      (= (+ x y) (+ y x))))))))

(th~defproblem  ass+
		(in lueneburg)
		(conclusion conc
			     (forall (lam (x ff)
				       (forall (lam (y ff)
						    (forall (lam (z ff)
								 (= (+ (+ x y) z) (+ x (+ y z)))))))))))


(th~defproblem  kom*
		(in lueneburg)
		(conclusion conc
			     (forall (lam (x ff)
				       (forall (lam (y ff)
						    (= (* x y) (* y x))))))))


(th~defproblem  ass*
		(in lueneburg)
		(conclusion conc
			     (forall (lam (x ff)
				       (forall (lam (y ff)
						    (forall (lam (z ff)
								 (= (* (* x y) z) (* x (* y z)))))))))))

	 
(th~defproblem  distrib 
		(in lueneburg)
		(conclusion conc
			    (forall (lam (x ff)
					  (forall (lam (y ff)
						       (forall (lam (z ff)
								    (= (* x (+ y z)) (+ (* x y) (* x z)))))))))))


(th~defproblem  kon+
		(in lueneburg)
		(conclusion conc
			     (forall (lam (x ff)
					(forall (lam (y ff)
						     (= (- (+ x y)) (+ (- x) (- y)))))))))

(th~defproblem  --
		(in lueneburg)
		(conclusion conc
			    (forall (lam (x ff)
					 (= (- (- x)) x)))))

(th~defproblem  neut-element*
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (and 
				   (= (* x e) x)
				   (= x (* x e)))))))


(th~defproblem  inv-elememt+
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (and
				   (= (+ x (- x)) n)
				   (= (+ (- x) x) n))))))

(th~defproblem  kon-n-element*
		(in lueneburg)
		(conclusion conc
		     (forall (lam (x ff)
				  (and
				   (= (* x n) n)
				   (= (* n x) n))))))


(th~defproblem lin-komb+
	 (in lueneburg)
	 (conclusion conc
		     (exists (lam (x ff)
				  (exists (lam (y ff)
					       (= (+ (* x (+ fx (- l1)))
						     (* y (+ gx (- l2))))
						  (+ (+ fx gx)
						     (- (+ l1 l2))))))))))

(th~defproblem rng1
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* x (+ y (+inv z)))
							       (+ (* x y) (+inv (* x z))))))))))))

			 
					    
(th~defproblem rng2
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (forall (lam (z ff)
							    (= (* (+ x (+inv y)) z)
							       (+ (* x z) (+inv (* y z))))))))))))

			 
					    
(th~defproblem thm-2-1-2-a
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (implies (= (+ x y) y)
							(= x zero))))))))

			 
					    
(th~defproblem thm-2-1-3-a
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (x ff)
				  (forall (lam (y ff)
					       (implies (= (+ x y) zero)
							(= y (+inv x)))))))))

			 
					    
(th~defproblem thm-2-1-5-a
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (a ff)
				  (= (* a zero) zero)))))
			 
					    
(th~defproblem thm-2-1-5-b
	 (in lueneburg)
	 (conclusion theo
		     (forall (lam (a ff)
				  (= (* (+inv e) a) (+inv a))))))
			 
					    
(th~defproblem thm-2-1-5-c
	 (in lueneburg)

	 (conclusion theo
		     (forall (lam (a ff)
				  (= (+inv (+inv a)) a)))))

					    
(th~defproblem thm-2-1-5-d
	 (in lueneburg)
	 (conclusion theo
		     (= (* (+inv e) (+inv e)) e)))

					    
		 



				

	 


	 
	 
				







