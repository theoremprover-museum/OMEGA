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

#-keim31-new(in-package :omega)

(th~deftheory puzzle
	      (uses base)
	      (constants (glaubt (o o i))
			 (peter i)
			 (osterhase (o i))
			 (weihnachtsmann (o i))
			 (agatha i)
			 (butler i)
			 (charles i)
			 (hates (o i i))
			 (killed (o i i))
			 (lives (o i))
			 (richer (o i i))
			 (wolf (o i))
			 (fox (o i))
			 (bird (o i))
			 (caterpillar (o i))
			 (snail (o i))
			 (animal (o i))
			 (grain (o i))
			 (plant (o i))
			 (eats (o i i))
			 (smaller (o i i))
			 (warsenplueff i)
			 (siesenwalps i)
			 (akrenfloeh i)
			 (kuenzenhuff i)
			 (bulbenjolk i)
			 (prowenknor i)
			 (deffenwulk i)
			 (FLUESSIGKEIT (o i))
			 (MINERAL (o i))
			 (PFLANZE (o i))
			 (KOERPERTEIL (o i))
			 (TIER (o i))
			 (wma I)
			 (wmb I)
			 (wmc I)
			 (knows (O O I))
			 (common-knowledge (O O))
			 (white (O I))
			 (asked I)
			 (other I)
			 (says (O O I))
			 (knight (O I))
			 (knave (O I))
			 (holds (O O)))
	      (help "The mother of all puzzle problems"))


(th~defaxiom agatha
	     (in puzzle)
	     (formula
	      (lives agatha))
	     (help "Agatha lives."))


(th~defaxiom butler
	     (in puzzle)
	     (formula
	      (lives butler))
	     (help "The butler lives."))

(th~defaxiom charles
	     (in puzzle)
	     (formula
	      (lives charles))
	     (help "Charles lives."))

(th~defaxiom agatha-hates-agatha
	     (in puzzle)
	     (formula
	      (hates agatha agatha))
	     (help "Agatha hates herself."))

(th~defaxiom agatha-hates-charles
	     (in puzzle)
	     (formula
	      (hates agatha charles))
	     (help "Agatha hates Charles."))

(th~defaxiom butler-hates-poor
	     (in puzzle)
	     (formula
	      (forall (lam (X i) (implies (lives X)
					  (or (richer X agatha)
					      (hates butler X))))))
	     (help "The butler hates all people who are not richer than Agatha."))

(th~defaxiom different-hates
	     (in puzzle)
	     (formula
	      (forall (lam (X i) (implies (hates agatha X)
					  (not (hates charles X))))))
	     (help "Charles hates nobody who is hated by Agatha."))

(th~defaxiom killer_hates_victim
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (forall (lam (Y i)
					(implies (killed X Y)
						 (hates X Y)))))))
	     (help "The killer hates his victim."))

(th~defaxiom no-one-hates-everyone
	     (in puzzle)
	     (formula
	      (forall (lam (X i)
			   (or (not (hates X agatha))
			       (or (not (hates X butler))
				   (not (hates X charles)))))))
	     (help "Nobody hates everyone else."))

(th~defaxiom poorer_killer
	     (in puzzle)
	     (formula
	      (forall (lam (X i)
			   (forall (lam (Y i)
					(implies (killed X Y)
						 (not (richer X Y))))))))
	     (help "The killer only killed people who are not richer than himself."))

(th~defaxiom same-hates
	     (in puzzle)
	     (formula
	      (forall (lam (X i) (implies (hates agatha X) (hates butler
								  X)))))
	     (help "The butler hates everybody who is hated by Agatha."))

(th~defaxiom wolves
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (wolf x) (animal x))))
		   (exists (lam (x i) (wolf x)))))
	     (help "Wolves are animals and there are wolves."))

(th~defaxiom foxes
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (fox x) (animal x))))
		   (exists (lam (x i) (fox x)))))
	     (help "Foxes are animals and there are foxes."))

(th~defaxiom birds
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (bird x) (animal x))))
		   (exists (lam (x i) (bird x)))))
	     (help "Birds are animals and there are birds."))


(th~defaxiom caterpillars
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (caterpillar x) (animal x))))
		   (exists (lam (x i) (caterpillar x)))))
	     (help "Caterpillare are animals and there are caterpillars."))

(th~defaxiom snails
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (snail x) (animal x))))
		   (exists (lam (x i) (snail x)))))
	     (help "Snails are animals and there are snails."))

(th~defaxiom grains
	     (in puzzle)
	     (formula
	      (and (forall (lam (x i) (implies (grain x) (plant x))))
		   (exists (lam (x i) (grain x)))))
	     (help "Grains are plants and there are grains."))

(th~defaxiom nature-of-animals
	     (in puzzle)
	     (formula
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
	     (help "Animals eat either plants or plant eating animals that are smaller
than themselves."))

(th~defaxiom smaller-relation
	     (in puzzle)
	     (formula
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
	     (help "Caterpillars and snails are smaller than birds. Birds are smaller than
foxes which are smaller than wolves."))

(th~defaxiom food-birds-1
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (forall (lam (y i)
					(implies (and (bird x)
						      (caterpillar y))
						 (eats x y)))))))
	     (help "Birds eat caterpillars."))

(th~defaxiom food-caterpillars
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (implies (caterpillar x)
				    (exists (lam (y i)
						 (and (plant y)
						      (eats x y))))))))
	     (help "Caterpillars eat plants."))

(th~defaxiom food-snails
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (implies (snail x)
				    (exists (lam (y i)
						 (and (plant y)
						      (eats x y))))))))
	     (help "Snails eat plants."))

(th~defaxiom food-wolves
	     (in puzzle)
	     (formula
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
	     (help "Wolves don't eat foxes or grains."))

(th~defaxiom food-birds-2
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (forall (lam (y i)
					(implies (and (bird x)
						      (snail y))
						 (not (eats x y))))))))
	     (help "Birds don't eat snails."))

(th~defaxiom ausschluss
	     (in puzzle)
	     (formula
	      (forall (lam (x i)
			   (and (and (and (implies (FLUESSIGKEIT x)
						   (not (or (or (MINERAL x)
								(PFLANZE x))
							    (or (KOERPERTEIL x)
								(TIER x)))))
					  (implies (MINERAL x)
						   (not (or (or (FLUESSIGKEIT x)
								(PFLANZE x))
							    (or (KOERPERTEIL x)
								(TIER x))))))
				     (and (implies (KOERPERTEIL x)
						   (not (or (or (MINERAL x)
								(PFLANZE x))
							    (or (FLUESSIGKEIT x)
								(TIER x)))))
					  (implies (PFLANZE x)
						   (not (or (or (FLUESSIGKEIT x)
								(MINERAL x))
							    (or (KOERPERTEIL x)
								(TIER x)))))))
				(implies (TIER x)
					 (not (or (or (FLUESSIGKEIT x)
						      (MINERAL x))
						  (or (KOERPERTEIL x)
						      (PFLANZE
						       x)))))))))
	     (help "Fluessigkeiten, Minerale, Pflanzen, Koerperteile und Tiere schliessen
sich gegenseitig aus."))

(th~defaxiom eins
	     (in puzzle)
	     (formula
	      (implies (not (TIER warsenplueff))
		       (MINERAL siesenwalps)))
	     (help "Wenn warsenplueff kein Tier ist, dann ist siesenwalps ein Mineral."))

(th~defaxiom zwei
	     (in puzzle)
	     (formula
	      (implies (KOERPERTEIL akrenfloeh)
		       (KOERPERTEIL siesenwalps)))
	     (help "Wenn akrenfloeh ein Koerperteil ist, dann auch siesenwalps."))

(th~defaxiom drei
	     (in puzzle)
	     (formula
	      (implies (not (TIER prowenknor))
		       (TIER bulbenjolk)))
	     (help "Wenn prowenknor kein Tier ist, dann aber bulbenjolk."))

(th~defaxiom vier
	     (in puzzle)
	     (formula
	      (implies (TIER warsenplueff)
		       (or (KOERPERTEIL siesenwalps)
			   (KOERPERTEIL kuenzenhuff))))
	     (help "Wenn warsenplueff ein Tier ist, dann ist siesenwalps oder kuenzenhuff
ein Koerperteil."))

(th~defaxiom fuenf
	     (in puzzle)
	     (formula
	      (implies (not (FLUESSIGKEIT warsenplueff))
		       (FLUESSIGKEIT siesenwalps)))
	     (help "Wenn warsenplueff keine Fluessigkeit ist, dann aber siesenwalps."))

(th~defaxiom sechs
	     (in puzzle)
	     (formula
	      (implies (not (PFLANZE prowenknor))
		       (TIER deffenwulk)))
	     (help "Wenn prowenknor keine Pflanze ist, dann ist deffenwulk ein Tier."))

(th~defaxiom acht
	     (in puzzle)
	     (formula (implies (not (PFLANZE kuenzenhuff))
			       (KOERPERTEIL warsenplueff)))
	     (help "Wenn kuenzenhuff keine Pflanze ist, dann ist warsenplueff ein Koerperteil."))
	     
(th~defaxiom neun
	     (in puzzle)
	     (formula
	      (implies (MINERAL akrenfloeh)
		       (KOERPERTEIL bulbenjolk)))
	     (help "Wenn akrenfloeh ein Mineral ist, dann ist bulbenjolk ein Mineral."))
	     
(th~defaxiom zehn
	     (in puzzle)
	     (formula
	      (implies (and (MINERAL siesenwalps)
			    (FLUESSIGKEIT warsenplueff))
		       (or (MINERAL akrenfloeh)
			   (KOERPERTEIL akrenfloeh))))
	     (help "Wenn siesenwalps ein Mineral ist und warsenplueff eine Fluessigkeit,
dann ist akrenfloeh ein Mineral oder ein Koerperteil."))
	     
(th~defaxiom ass1
	     (in puzzle)
	     (formula (AND (NOT (= wmb wma)) (AND (NOT (= wmc wma))
						  (NOT (= wmc wmb)))))
	     (help "specify!"))
	     
(th~defaxiom ass2
	     (in puzzle)
	     (formula (FORALL (LAM (P O) (IMPLIES (common-knowledge P)
						  (FORALL (LAM (X I)
							       (knows
								X
								P)))))))
	     (help "specify!"))
	     
(th~defaxiom ass3
	     (in puzzle)
	     (formula (common-knowledge (OR (white wma) (OR (white
							     wmb)
							    (white
							     wmc)))))
	     (help "specify!"))
	     
(th~defaxiom ass4
	     (in puzzle)
	     (formula (common-knowledge
		       (FORALL (LAM (X I)
				    (FORALL (LAM (Y I)
						 (IMPLIES (NOT (= X Y))
							  (IMPLIES (NOT (white
									 X))
								   (knows Y
									  (NOT
									   (white X)))))))))))
	     (help "specify!"))
	     
(th~defaxiom ass5
	     (in puzzle)
	     (formula (knows wmc (knows wmb (NOT (knows wma (white
							     wma))))))
	     (help "specify!"))
	     
(th~defaxiom ass6
	     (in puzzle)
	     (formula (knows wmc (NOT (knows wmb (white wmb)))))
	     (help "specify!"))

(th~defaxiom ass1for1and2
	     (in puzzle)
	     (formula(FORALL (LAM (X I) (OR (knight X) (knave X)))))
	     (help "Everything is a knight or a knave."))

(th~defaxiom ass2for1
	     (in puzzle)
	     (formula
	      (FORALL (LAM (P I)
			   (FORALL (LAM (S O)
					(IMPLIES (says P S)
						 (IMPLIES (knight P) S)))))))
	     (help "specify!"))

(th~defaxiom ass3for1
	     (in puzzle)
	     (formula
	      (FORALL (LAM (P I)
			   (FORALL (LAM (S O)
					(IMPLIES (says P S)
						 (IMPLIES (knave P) (not
								     S))))))))
	     (help "specify!"))

(th~defaxiom ass2for2
	     (in puzzle)
	     (formula
	      (FORALL (LAM (P I)
			   (FORALL (LAM (S O)
					(IMPLIES (says P S)
						 (IMPLIES (knight P) (holds
								      S))))))))
	     (help "specify!"))
(th~defaxiom ass3for2
	     (in puzzle)
	     (formula
	      (FORALL (LAM (P I)
			   (FORALL (LAM (S O)
					(IMPLIES (says P S)
						 (IMPLIES (knave P) (not (holds
									  S)))))))))
	     (help "specify!"))

	 
	 
