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
(in-package :omega)

 






(th~deftheorem EXISTS-UNIQUE-RL 
	       (in BASE) 
	       (category THEOREM) 
	       
	       (conclusion (all-types bb (forall (lam (Q (o bb))
						      (implies (exists-unique (lam (x bb) (Q x)))
							       (exists (lam (z bb)
									    (and (Q z)	    
										 (forall (lam (x bb) (implies (Q x) 
													      (= x z))))))))))))
	       (help ""))

(th~deftheorem EXISTS-UNIQUE-LR 
	       (in BASE) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (Q (o bb))
						      (implies  (exists (lam (z bb)
									     (and (Q z)	    
										  (forall (lam (x bb) (implies (Q x) 
													       (= x z)))))))
								(exists-unique (lam (x bb) (Q x))))))))
	       (help ""))



(th~deftheorem false-and1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (and x false) false))))
	     (help "FALSE + AND 1"))

(th~deftheorem false-and2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (and false x) false))))
	     (help "FALSE + AND 2"))

(th~deftheorem false-or1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (or x false) x))))
	     (help "FALSE + OR 1"))

(th~deftheorem false-or2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (or false x) x))))
	     (help "FALSE + OR 2"))

(th~deftheorem false-implies1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (implies x false) (not x)))))
	     (help "FALSE + IMPLIES 1"))

(th~deftheorem false-implies2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (implies false x) true))))
	     (help "FALSE + IMPLIES 2"))

(th~deftheorem false-equiv1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (equiv x false) (not x)))))
	     (help "FALSE + EQUIV 1"))

(th~deftheorem false-equiv2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (equiv false x) (not x)))))
	     (help "FALSE + EQUIV 2"))

(th~deftheorem false-forall
	     (in base)
	     (conclusion conc (all-types aa (= (forall (lam (x aa)
				      false))
			 false)))
	     (help "FALSE + FORALL"))

(th~deftheorem false-exists
	     (in base)
	     (conclusion conc (all-types aa (= (exists (lam (x aa)
				      false))
			 false)))
	     (help "FALSE + EXISTS"))

(th~deftheorem false-not
	     (in base)
	     (conclusion conc (= (not false)
			 true))
	     (help "FALSE + NOT"))

(th~defsimplifier false-and1-simp
		  (in base)
		  (status global)
		  (equation false-and1)
		  (direction lr)
		  (help "Simplify using axiom false-and1"))

(th~defsimplifier false-and2-simp
		  (in base)
		  (status global)
		  (equation false-and2)
		  (direction lr)
		  (help "Simplify using axiom false-and2"))

(th~defsimplifier false-or1-simp
		  (in base)
		  (status global)
		  (equation false-or1)
		  (direction lr)
		  (help "Simplify using axiom false-or1"))

(th~defsimplifier false-or2-simp
		  (in base)
		  (status global)
		  (equation false-or2)
		  (direction lr)
		  (help "Simplify using axiom false-or2"))

(th~defsimplifier false-implies1-simp
		  (in base)
		  (status global)
		  (equation false-implies1)
		  (direction lr)
		  (help "Simplify using axiom false-implies1"))

(th~defsimplifier false-implies2-simp
		  (in base)
		  (status global)
		  (equation false-implies2)
		  (direction lr)
		  (help "Simplify using axiom false-implies2"))


(th~defsimplifier false-equiv1-simp
		  (in base)
		  (status global)
		  (equation false-equiv1)
		  (direction lr)
		  (help "Simplify using axiom false-equiv1"))

(th~defsimplifier false-equiv2-simp
		  (in base)
		  (status global)
		  (equation false-equiv2)
		  (direction lr)
		  (help "Simplify using axiom false-equiv2"))

(th~defsimplifier false-forall-simp
		  (in base)
		  (status global)
		  (equation false-forall)
		  (direction lr)
		  (help "Simplify using axiom false-forall"))

(th~defsimplifier false-exists-simp
		  (in base)
		  (status global)
		  (equation false-exists)
		  (direction lr)
		  (help "Simplify using axiom false-exists"))

(th~defsimplifier false-not-simp
		  (in base)
		  (status global)
		  (equation false-not)
		  (direction lr)
		  (help "Simplify using axiom false-not"))











(th~deftheorem true-and1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (and x true) x))))
	     (help "TRUE + AND 1"))

(th~deftheorem true-and2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (and true x) x))))
	     (help "TRUE + AND 2"))

(th~deftheorem true-or1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (or x true) true))))
	     (help "TRUE + OR 1"))

(th~deftheorem true-or2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (or true x) true))))
	     (help "TRUE + OR 2"))

(th~deftheorem true-implies1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (implies x true) true))))
	     (help "TRUE + IMPLIES 1"))

(th~deftheorem true-implies2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (implies true x) x))))
	     (help "TRUE + IMPLIES 2"))

(th~deftheorem true-equiv1
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (equiv x true) x))))
	     (help "TRUE + EQUIV 1"))

(th~deftheorem true-equiv2
	     (in base)
	     (conclusion conc (forall (lam (x o)
				   (= (equiv true x) x))))
	     (help "TRUE + EQUIV 2"))

(th~deftheorem true-forall
	     (in base)
	     (conclusion conc (all-types aa (= (forall (lam (x aa)
				      true))
			 true)))
	     (help "TRUE + FORALL"))

(th~deftheorem true-exists
	     (in base)
	     (conclusion conc (all-types aa (= (exists (lam (x aa)
				      true))
			 true)))
	     (help "TRUE + EXISTS"))

(th~deftheorem true-not
	     (in base)
	     (conclusion conc (= (not true)
			 false))
	     (help "TRUE + NOT"))

(th~defsimplifier true-and1-simp
		  (in base)
		  (status global)
		  (equation true-and1)
		  (direction lr)
		  (help "Simplify using axiom true-and1"))

(th~defsimplifier true-and2-simp
		  (in base)
		  (status global)
		  (equation true-and2)
		  (direction lr)
		  (help "Simplify using axiom true-and2"))

(th~defsimplifier true-or1-simp
		  (in base)
		  (status global)
		  (equation true-or1)
		  (direction lr)
		  (help "Simplify using axiom true-or1"))

(th~defsimplifier true-or2-simp
		  (in base)
		  (status global)
		  (equation true-or2)
		  (direction lr)
		  (help "Simplify using axiom true-or2"))

(th~defsimplifier true-implies1-simp
		  (in base)
		  (status global)
		  (equation true-implies1)
		  (direction lr)
		  (help "Simplify using axiom true-implies1"))

(th~defsimplifier true-implies2-simp
		  (in base)
		  (status global)
		  (equation true-implies2)
		  (direction lr)
		  (help "Simplify using axiom true-implies2"))


(th~defsimplifier true-equiv1-simp
		  (in base)
		  (status global)
		  (equation true-equiv1)
		  (direction lr)
		  (help "Simplify using axiom true-equiv1"))

(th~defsimplifier true-equiv2-simp
		  (in base)
		  (status global)
		  (equation true-equiv2)
		  (direction lr)
		  (help "Simplify using axiom true-equiv2"))

(th~defsimplifier true-forall-simp
		  (in base)
		  (status global)
		  (equation true-forall)
		  (direction lr)
		  (help "Simplify using axiom true-forall"))

(th~defsimplifier true-exists-simp
		  (in base)
		  (status global)
		  (equation true-exists)
		  (direction lr)
		  (help "Simplify using axiom true-exists"))

(th~defsimplifier true-not-simp
		  (in base)
		  (status global)
		  (equation true-not)
		  (direction lr)
		  (help "Simplify using axiom true-not"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theorems Involving Description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~deftheorem that-ass1      
             (in base)
             (conclusion conc
              (all-types bb
			 (forall
			  (lam (Q (o bb))
			       (forall (lam (P (o bb))
					    (implies (and (exists-unique (lam (x bb) (P x)))
							  (forall
							   (lam (z bb)
								(implies (P z)
									 (Q z)))))
						   (Q (that P)))))))))
             (help "Assertion axiom  for description operator that."))

(th~deftheorem that-ass2
             (in base)
             (conclusion conc
              (all-types bb
			 (forall
			  (lam (Q (o bb))
			       (forall (lam (P (o bb))
					    (implies (Q (that P))
						     (implies
						      (exists (lam (x bb)
								   (implies (P x)
									    (forall (lam (y bb)
											 (implies (P y) (= x y)))))))
						      (forall
						       (lam (z bb)
							    (implies (P z)
								     (Q z))))))))))))
             (help "Assertion axiom  for description operator that."))

(th~deftheorem that-5310
             (in base)
             (conclusion 
              (all-types bb
			 (forall (lam (P (o bb))
			 (forall (lam (y bb)
				      (implies (forall (lam (z bb)
								   (equiv (P z)
									  (= y z))))
					       (= (that P) y))))))))
             (help "Andrews' theorem 5310 about the that operator."))

(th~deftheorem that-5311
             (in base)
             (conclusion 
              (all-types bb
			 (forall (lam (P (o bb))
				      (implies (exists-unique (lam (z bb)
								   (P z)))
					       (P (that P) ))))))
             (help "Andrews' theorem 5311 about the that operator."))

(th~deftheorem that-5312
             (in base)
             (conclusion 
              (all-types bb
			 (forall (lam (P (o bb))
				      (implies (exists-unique (lam (z bb)
								   (P z)))
					       (forall (lam (y bb) (equiv (P y)
									  (= y (that P) )))))))))
             (help "Andrews' theorem 5312 about the that operator."))



