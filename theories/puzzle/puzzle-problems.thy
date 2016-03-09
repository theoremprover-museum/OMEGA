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





(th~defproblem osterhase1
	       (in puzzle)
	       (conclusion conc (IMPLIES (AND (glaubt peter (exists (lam (x i) (osterhase x))))
					      (glaubt peter (exists (lam (x i) (weihnachtsmann x)))))
					 (glaubt peter (and (exists (lam (x i) (osterhase x)))
							    (exists (lam (x i) (weihnachtsmann x))))))))


(th~defproblem osterhase2
	       (in puzzle)
	       (conclusion conc (IMPLIES (AND (glaubt peter (exists (lam (x i) (osterhase x))))
					      (glaubt peter (implies (exists (lam (x i) (osterhase x)))
								     (exists (lam (x i) (weihnachtsmann x))))))
					 (glaubt peter (exists (lam (x i) (weihnachtsmann x)))))))
	 



(th~defproblem agathas-murderer
	       (in puzzle)
	       (assumption agatha (lives agatha))
	       (assumption butler (lives butler))
	       (assumption charles (lives charles))
	       (assumption agatha-hates-agatha (hates agatha agatha))
	       (assumption agatha-hates-charles (hates agatha charles))
	       (assumption butler-hates-poor
			   (forall (lam (X i) (implies (lives X)
						       (or (richer X agatha)
							   (hates butler X))))))
	       (assumption different-hates
			   (forall (lam (X i) (implies (hates agatha X)
						       (not (hates charles X))))))
	       (assumption killer-hates-victim
			   (forall (lam (x i)
					(forall (lam (Y i)
						     (implies (killed X Y)
							      (hates X Y)))))))
	       (assumption no-one-hates-everyone
			   (forall (lam (X i)
					(or (not (hates X agatha))
					    (or (not (hates X butler))
						(not (hates X charles)))))))
	       (assumption poorer-killer
			   (forall (lam (X i)
					(forall (lam (Y i)
						     (implies (killed X Y)
							      (not (richer X Y))))))))
	       (assumption same-hates
			   (forall (lam (X i) (implies (hates agatha X)
						       (hates butler X)))))
	       (conclusion theo
			   (and (not (killed butler agatha)) (not (killed charles agatha)))))



(th~defproblem STEAMROLLER (in puzzle)
	       (assumption wolves
			   (and (forall (lam (x i) (implies (wolf x) (animal x))))
				(exists (lam (x i) (wolf x)))))
	       (assumption foxes
			   (and (forall (lam (x i) (implies (fox x) (animal x))))
				(exists (lam (x i) (fox x)))))
	       (assumption birds
			   (and (forall (lam (x i) (implies (bird x) (animal x))))
				(exists (lam (x i) (bird x)))))
	       (assumption caterpillars
			   (and (forall (lam (x i) (implies (caterpillar x) (animal x))))
				(exists (lam (x i) (caterpillar x)))))
	       (assumption snails
			   (and (forall (lam (x i) (implies (snail x) (animal x))))
				(exists (lam (x i) (snail x)))))
	       (assumption grains
			   (and (forall (lam (x i) (implies (grain x) (plant x))))
				(exists (lam (x i) (grain x)))))
	       (assumption nature-of-animals
			   (forall (lam (x i)
					(implies (animal x)
						 (or (forall (lam (y i) (implies (plant y)
										 (eats x y))))
						     (forall (lam (z i) (implies (and (and (animal z)
											   (smaller z x))
										      (exists (lam (u i) (and (plant u)
													      (eats z u)))))
										 (eats
										  x
										  z)))))))))
	       (assumption smaller-relation
			   (and (and (forall (lam (x i)
						  (forall (lam (y i)
							       (implies (and (caterpillar x)
									     (bird y))
									(smaller x y))))))
				     (forall (lam (x i)
						  (forall (lam (y i)
							       (implies (and (snail x)
									     (bird y))
									(smaller x y)))))))
				(and (forall (lam (x i)
						  (forall (lam (y i)
							       (implies (and (bird x)
									     (fox y))
									(smaller x y))))))
				     (forall (lam (x i)
						  (forall (lam (y i)
							       (implies (and (fox x)
									     (wolf y))
									(smaller x
										 y)))))))))
	       (assumption food-birds-1
			   (forall (lam (x i)
					(forall (lam (y i)
						     (implies (and (bird x)
								   (caterpillar y))
							      (eats x y)))))))
	       (assumption food-caterpillars
			   (forall (lam (x i)
			   (implies (caterpillar x)
				    (exists (lam (y i)
						 (and (plant y)
						      (eats x y))))))))
	       (assumption food-snails
			   (forall (lam (x i)
					(implies (snail x)
						 (exists (lam (y i)
							      (and (plant y)
								   (eats x y))))))))
	       (assumption food-wolves
			   (forall (lam (x i)
					(implies (wolf x)
						 (and (forall (lam (y i)
								   (implies (fox y)
									    (not (eats x y)))))
						      (forall (lam (y i)
								   (implies (grain y)
									    (not (eats
										  x
										  y))))))))))
	       (assumption food-birds-2
			   (forall (lam (x i)
					(forall (lam (y i)
						     (implies (and (bird x)
								   (snail y))
							      (not (eats x y))))))))
	       (conclusion steamy
			   (exists (lam (x i)
					(exists (lam (y i)
						     (and (and (and (animal x)
								    (animal y))
							       (eats x y))
							  (exists (lam (z i)
								       (and (grain z)
									    (eats y z)))))))))))
				    



(th~defproblem ZWEISTEIN (in puzzle)
	       (conclusion zweistein
			   (and (and (and (FLUESSIGKEIT warsenplueff)
					  (MINERAL siesenwalps))
				     (and (MINERAL akrenfloeh)
					  (PFLANZE kuenzenhuff)))
				(and (and (KOERPERTEIL bulbenjolk)
					  (TIER prowenknor))
                                     (TIER deffenwulk))))
	       )

				
(th~defproblem wise-man
	       (in puzzle)
	       (conclusion conc (knows wmc (white wmc))))

				
(th~defproblem knight&Knaves1
	       (in puzzle)
	       (conclusion
		conc (IMPLIES (says asked (NOT (OR (knight asked) (knight other))))
			      (and (knave asked) (knight other)))))

(th~defproblem knight&Knaves2
	       (in puzzle)
	       (conclusion
		conc (IMPLIES (says asked (NOT (OR (knight asked) (knight other))))
			      (EXISTS (LAM (P (O I))
					   (EXISTS (LAM (Q (O I))
							(AND (OR (= P knight) (= P knave))
							     (AND (OR (= Q knight) (= Q knave))
								  (AND (P asked) (Q other)))))))))))
	 


	 
	 
				
(th~defproblem unicorn-is-horned
	       (in puzzle)
	       (constants (mo o) (my o) (ma o) (ho o))
	       (assumption Ass1 (implies my (not mo)))
	       (assumption Ass2 (implies (not my) (and mo ma)))
	       (assumption Ass3 (implies (or (not mo) ma) ho))
	       (assumption Ass4 (implies ho ma))
	       (conclusion Conc ho))



(th~defproblem unicorn-is-mythical
	       (in puzzle)
	       (constants (mo o) (my o) (ma o) (ho o))
	       (assumption Ass1 (implies my (not mo)))
	       (assumption Ass2 (implies (not my) (and mo ma)))
	       (assumption Ass3 (implies (or (not mo) ma) ho))
	       (assumption Ass4 (implies ho ma))
	       (conclusion Conc my))


(th~defproblem unicorn-is-mortal
	       (in puzzle)
	       (constants (mo o) (my o) (ma o) (ho o))
	       (assumption Ass1 (implies my (not mo)))
	       (assumption Ass2 (implies (not my) (and mo ma)))
	       (assumption Ass3 (implies (or (not mo) ma) ho))
	       (assumption Ass4 (implies ho ma))
	       (conclusion Conc mo))

(th~defproblem unicorn-is-mammal
	       (in puzzle)
	       (constants (mo o) (my o) (ma o) (ho o))
	       (assumption Ass1 (implies my (not mo)))
	       (assumption Ass2 (implies (not my) (and mo ma)))
	       (assumption Ass3 (implies (or (not mo) ma) ho))
	       (assumption Ass4 (implies ho ma))
	       (conclusion Conc ma))
