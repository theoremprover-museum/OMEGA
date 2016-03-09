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


(th~defproblem thielscher-states
	 (in typed-set)
	 (type-constants person object action)
	 (constants (alive (o person))
		    (poisoned (o object))
		    (mix (action object object person))
		    (drink (action object person))
		    (do ((o o) (o o) action))
		    (nephew person)
		    (aunt person)
		    (tea object)
		    (milk object)
		    (s0 (o o)))
	 (assumption ass12a
		     (subset
		      (lam (P o) (or (= P (alive nephew))
				     (= P (alive aunt))))
		      s0))
	 (assumption ass12b
		     (not (in (poisoned tea) s0)))
	 (assumption ass15a
		     (forall (lam (S (o o)) (forall (lam (X object) (forall (lam (Y object) (forall (lam (P person)
		       (implies
			(and (in (poisoned X) S) (not (in (poisoned Y) S)))
			(= (do (mix P X Y) S)
			   (union S (lam (Q o) (= Q (poisoned Y)))))))))))))))
	 (assumption ass15b
		     (forall (lam (S (o o)) (forall (lam (X object) (forall (lam (Y object) (forall (lam (P person)
		       (implies
			(and (not (in (poisoned X) S)) (in (poisoned Y) S))
			(= (do (mix P X Y) S) S)))))))))))
	 (assumption ass17a
		     (forall (lam (S (o o)) (forall (lam (X object) (forall (lam (P person)
		       (implies
			(and (in (alive P) S) (in (poisoned X) S))
			(= (do (drink P X) S)
			   (setminus S (lam (Q o) (= Q (alive P)))))))))))))
	 (assumption ass17b
		     (forall (lam (S (o o)) (forall (lam (X object) (forall (lam (P person)
		       (implies
			(and (not (in (alive P) S)) (not (in (poisoned X) S)))
			(= (do (drink P X) S)
			   S)))))))))
	 (conclusion (implies
		      (not (in (alive aunt)
			       (do (drink aunt tea)
				   (do (mix nephew milk tea)
				       s0))))
		      (in (poisoned milk) s0))))
			   


(th~defproblem paulson1
	 (in typed-set)
	 (conclusion conc 
		     (all-types aa (forall (lam (X (o aa))
				  (implies
				   (forall (lam (Y (o aa))
						(subset X Y)))
				   (exists (lam (Z aa)
						(subset X (lam (u aa) (= u Z)))))))))))

(th~defproblem paulson2
	 (in typed-set)
	 (constants (A (o (o i) (o i))) (B (o (o i) (o i))))
	 (conclusion conc
		     (forall (lam (X (o i))
				     (= (intersection-over-collection
					 (intersection
					  (A X)
					  (B X)))
					(intersection
					 (intersection-over-collection
					  (A X))
					 (intersection-over-collection
					  (B X))))))))

(th~defproblem bool-prop-set10
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc
                     (all-types aa (forall (lam (X (o aa))
						(forall (lam (Y (o aa))
							     (forall (lam (Z aa)
									  (equiv
									   (in Z (setminus X Y))
									   (and (in Z X)
										(not (in Z Y)))))))))))))
	 
(th~defproblem bool-prop-set100
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa)) 
						     (forall (lam (Y (o aa)) 
								  (forall (lam (Z (o aa)) 
									       (equiv
										(meets X (union Y Z))
										(or (meets X Y)
										    (meets X Z))))))))))))

(th~defproblem bool-prop-set101
	       (in typed-set)
	       (author trybulec)
	       (reference "mizar.bool-prop-set")
	       (conclusion conc
			   (all-types aa (forall (lam (X (o aa)) 
						      (forall (lam (Y (o aa)) 
								   (forall (lam (Z (o aa)) 
										(implies
										 (and (meets X Y)
										      (subset Y Z))
										 (meets X Z)))))))))))

(th~defproblem bool-prop-set102 
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa)) 
						     (forall (lam (Y (o aa)) 
								  (forall (lam (Z (o aa)) 
									       (implies 
										(meets X (intersection Y Z))
										(meets X Y)))))))))))

(th~defproblem bool-prop-set104
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (misses X emptyset))))))

(th~defproblem bool-prop-set11
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z aa)
									       (equiv 	
										(in Z (setminus X Y))
										(and (in Z X)
										     (not (in Z Y)))))))))))))
	 
(th~defproblem bool-prop-set110
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (equiv (meets X X)
							    (not (= X emptyset))))))))
	 
(th~defproblem bool-prop-set111
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (misses 
								   (intersection X Y)
								   (setminus X Y)))))))))

(th~defproblem bool-prop-set112
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (misses (intersection X Y)
									  (exclunion X Y)))))))))
	 
(th~defproblem bool-prop-set113
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z (o aa))
									       (implies (meets X (setminus Y Z))
											(meets X Y)))))))))))

(th~defproblem bool-prop-set114
	       (in typed-set)
	       (author trybulec)
	       (reference "mizar.bool-prop-set")
	       (conclusion conc (all-types aa (forall (lam (X (o aa))
							   (forall (lam (Y (o aa))
									(forall (lam (Z (o aa))
										     (implies (and (subset X Y)
												   (and (subset X Z)
													(misses Y Z)))
											      (= X emptyset)))))))))))

(th~defproblem bool-prop-set115
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z (o aa))
									       (implies (and (subset (setminus X Y) Z)
											     (subset (setminus Y X) Z))
											(subset (exclunion X Y) Z)))))))))))

(th~defproblem bool-prop-set116
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z (o aa))
									       (= (intersection X (setminus Y Z))
										  (setminus (intersection X Y) Z)))))))))))
	 
(th~defproblem bool-prop-set117
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z (o aa))
									       (= (intersection X (setminus Y Z))
										  (setminus 
										   (intersection X Y)
										   (intersection X Z))))))))))))

(th~defproblem bool-prop-set118
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (equiv (misses X Y)
									 (= (intersection X Y)
									    emptyset)))))))))

(th~defproblem bool-prop-set119 
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa)) 
								  (equiv (meets X Y)
									 (not (= (intersection X Y) 
										 emptyset))))))))))

(th~defproblem bool-prop-set12
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z aa)
									       (implies (and (in Z X)
											     (misses X Y))
											(not (in Z Y))))))))))))

(th~defproblem bool-prop-set120
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (forall (lam (Z (o aa))
									       (implies (and
											 (subset X (union Y Z))
											 (= (intersection X Z) emptyset))
											(subset X Y)))))))))))

(th~defproblem bool-prop-set121
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types aa (forall (lam (X (o aa))
                           (forall (lam (Y (o aa)) 
                              (implies (and
                                        (subset Y X)
                                        (= (intersection X Y) emptyset))
                                       (= Y emptyset)))))))))

(th~defproblem bool-prop-set13
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z a)
                               (implies (and (in Z X)
                                             (in Z Y))
                                        (meets X Y)))))))))))

(th~defproblem bool-prop-set14
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z a)
                               (implies (and
                                         (in Z X)
                                         (in Z Y))
                                        (meets X Y)))))))))))

(th~defproblem bool-prop-set15
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (implies (meets X Y)
                                       (exists (lam (Z a)
                                          (and (in Z X)
                                               (in Z Y))))))))))))

(th~defproblem bool-prop-set17
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (implies (forall (lam (Z a) 
                                          (implies (in Z X)
                                                   (not (in Z Y)))))
                                       (misses X Y)))))))))

(th~defproblem bool-prop-set18
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies  (forall (lam (U a) 
                                             (equiv (in U X) 
                                                    (or (in U Y) 
                                                        (in U Z)))))
                                         (= X (union Y Z))))))))))))

(th~defproblem bool-prop-set19
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (forall (lam (U a) 
                                           (equiv (in U X) 
                                                  (and (in U Y)
                                                       (in U Z)))))
                                        (= X (intersection Y Z))))))))))))

(th~defproblem bool-prop-set20
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (forall (lam (U a) 
                                           (equiv (in U X) 
                                                  (and (in U Y) 
                                                       (not (in U Z))))))
                                        (= X (setminus Y Z))))))))))))

(th~defproblem bool-prop-set21
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (forall (lam (U a) 
                                           (equiv (in U X) 
                                                  (and (in U Y) 
                                                       (not (in U Z))))))
                                        (= X (setminus Y Z))))))))))))

(th~defproblem bool-prop-set23
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (U a)
 			       (equiv (in U (exclunion X Y))
                                      (equiv (in U X) 
                                             (not (in U Y)))))))))))))

(th~defproblem bool-prop-set24
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			   (forall (lam (Y (o a))
			    (forall (lam (U a)
                               (implies (and (in U X)
                                             (in U Y))
                                        (not (= emptyset
                                                (intersection X Y)))))))))))))

(th~defproblem bool-prop-set25
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
	       		       (implies (forall (lam (U a)
                                           (equiv (not (in U X)) 
                                                  (equiv (in U Y) 
                                                         (in U Z)))))
                                        (= X (exclunion Y Z))))))))))))

(th~defproblem bool-prop-set27
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (subset emptyset X))))))
	
(th~defproblem bool-prop-set28
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (implies (and (subset X Y)
                                            (subset Y X))
                                       (= X Y)))))))))

(th~defproblem bool-prop-set29
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
	  		       (implies (and (subset X Y)
                                             (subset Y Z))
                                        (subset X Z)))))))))))

(th~defproblem bool-prop-set30
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			     (implies (subset X emptyset)
                                      (= X emptyset)))))))
(th~defproblem bool-prop-set31
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (subset X (union X Y)))))))))
   
(th~defproblem bool-prop-set32
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (and (subset X Z)
                                             (subset Y Z))
                                        (subset (union X Y) Z)))))))))))

(th~defproblem bool-prop-set33
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (subset X Y)
                                        (subset (union X Z)
                                                (union Y Z))))))))))))


(th~defproblem bool-prop-set34
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
                           (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a)) 
			     (forall (lam (V (o a))
                                (implies (and (subset X Y)
                                              (subset Z V))
                                         (subset (union X Z)
                                                 (union Y V))))))))))))))

(th~defproblem bool-prop-set35
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (implies (subset X Y)
                                       (= (union  X Y) Y)))))))))

(th~defproblem bool-prop-set37
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (subset (intersection X Y) X))))))))

(th~defproblem bool-prop-set38
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (subset (intersection X Y)
                                       (union X Z)))))))))))

(th~defproblem bool-prop-set39
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies	(and (subset Z X)
                                             (subset Z Y))
                                        (subset Z (intersection X Y))))))))))))

(th~defproblem bool-prop-set40
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies (subset X Y)
                                        (subset (intersection X Z)
                                                (intersection Y Z))))))))))))

(th~defproblem bool-prop-set41
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
                            (forall (lam (Z (o a)) 
                             (forall (lam (V (o a))
                                (implies (and (subset X Y)
                                              (subset Z V))
                                         (subset (intersection X Z)
                                                 (intersection Y V))))))))))))))

(th~defproblem bool-prop-set42
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (implies	(subset X Y)
                                        (=  (intersection X Y) X)))))))))))
  
(th~defproblem bool-prop-set43
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a)) 
                             (forall (lam (V (o a))
                                (implies (and (subset X Y)
                                              (subset Z V))
                                         (subset (intersection X Z)
                                                 (intersection Y V))))))))))))))

(th~defproblem bool-prop-set44
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
                           (forall (lam (Y (o a)) 
                            (forall (lam (Z (o a))
                               (implies
                                  (subset X Z)
                                  (= (union X (intersection Y Z))
                                     (intersection (union X Y) Z))))))))))))

(th~defproblem bool-prop-set45 
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (equiv (= (setminus X Y) emptyset)
                                     (subset X Y)))))))))

(th~defproblem bool-prop-set46
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
			       (implies (subset X Y)
                                        (subset (setminus X Z)
                                                (setminus Y Z))))))))))))

(th~defproblem bool-prop-set47
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
                               (implies (subset X Y)
                                        (subset (setminus Z Y)
                                                (setminus Z X))))))))))))

(th~defproblem bool-prop-set48
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a)) 
			     (forall (lam (V (o a))
				(implies (and (subset X Y)
                                              (subset Z V))
                                         (subset (setminus X V)
                                                 (setminus Y Z))))))))))))))
(th~defproblem bool-prop-set49
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (subset (setminus X Y) X))))))))

(th~defproblem bool-prop-set50
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (implies (subset X (setminus Y X))
                                       (= X emptyset)))))))))

(th~defproblem bool-prop-set51	
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
                           (forall (lam (Y (o a)) 
                            (forall (lam (Z (o a))
                               (implies (and (subset X Y)
                                         (and (subset X Z)
                                              (= (intersection Y Z) emptyset)))
                                        (= X emptyset)))))))))))

(th~defproblem bool-prop-set52
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
		    	       (implies
                                  (subset X (union Y Z))
                                  (and (subset (setminus X Y) Z)
                                       (subset (setminus X Z) Y))))))))))))

(th~defproblem bool-prop-set53
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
                               (implies (= (union (intersection X Y)
                                                  (intersection X Z))
                                           X)
                                        (subset X (union Y Z))))))))))))

(th~defproblem bool-prop-set54
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (implies (subset X Y)
                                       (= Y (union X (setminus Y X)))))))))))
    
(th~defproblem bool-prop-set55
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
                               (implies
                                  (and (subset X Y)
                                       (= (intersection Y Z) emptyset))
                                  (= (intersection X Z) emptyset)))))))))))

(th~defproblem bool-prop-set56
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a)) 
			       (equiv (= X (union Y Z))
                                      (and (subset Y X)
                                       (and (subset Z X)
                                            (forall (lam (V (o a))
                                               (implies
                                                  (and (subset Y V)
                                                       (subset Z V))
                                                  (subset X V))))))))))))))))

(th~defproblem bool-prop-set57
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a)) 
                               (equiv
                                  (= X (union Y Z))
                                  (and (subset X Y)
                                   (and (subset X Z)
                                        (forall (lam (V (o a))
                                           (implies (and (subset V Y)
                                                         (subset V Z))
                                                    (subset V X))))))))))))))))

(th~defproblem bool-prop-set58
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a))
			      (subset (setminus X Y)
                                      (exclunion X Y)))))))))

(th~defproblem bool-prop-set59
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (equiv (= (union X Y) emptyset)
                                     (and (= X emptyset)
                                          (= Y emptyset))))))))))

(th~defproblem bool-prop-set60
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (D (o a))
                             (= (union D emptyset) D))))))

(th~defproblem bool-prop-set61
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                             (= (intersection X emptyset) emptyset))))))

(th~defproblem bool-prop-set62
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (=	(union X X) X))))))

(th~defproblem bool-prop-set64
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a))  
			    (forall (lam (Z (o a))
			       (= (union (union X Y) Z)
                                  (union X (union Y Z))))))))))))

(th~defproblem bool-prop-set65
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (= (intersection X X) X))))))
		      
(th~defproblem bool-prop-set67
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a))  
			    (forall (lam (Z (o a))
                               (= (intersection (intersection X Y) Z)
                                  (intersection X (intersection Y Z))))))))))))

(th~defproblem bool-prop-set68
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (= (intersection X (union X Y)) X))))))))
     		      
(th~defproblem bool-prop-set69
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (= (union X (intersection X Y)) X))))))))

(th~defproblem bool-prop-set70
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
			       (= (intersection X (union Y Z))
                                  (union (intersection X Y)
                                         (intersection X Z))))))))))))

(th~defproblem bool-prop-set71
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
			       (= (union X (intersection Y Z))
                                  (intersection (union X Y)
                                                (union X Z))))))))))))

(th~defproblem bool-prop-set72
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a))
                               (= (union (intersection X Y) 
                                   (union (intersection Y Z) 
                                          (intersection Z X)))
                                  (intersection (union X Y)
                                   (intersection (union Y Z)
                                                 (union Z X)))))))))))))

(th~defproblem bool-prop-set73
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (= (setminus X X) emptyset))))))

(th~defproblem bool-prop-set74
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (= (setminus X emptyset) X))))))

(th~defproblem bool-prop-set75
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
			     (= (setminus emptyset X) emptyset))))))

(th~defproblem bool-prop-set76
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (= (setminus X (union X Y)) emptyset))))))))

(th~defproblem bool-prop-set77
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (= (setminus X (intersection X Y))
                                 (setminus X Y)))))))))

(th~defproblem bool-prop-set78
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (= (intersection (setminus X Y) Y)
                                 emptyset))))))))

(th~defproblem bool-prop-set79
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (= (union X (setminus Y X))
                                 (union X Y)))))))))
   
(th~defproblem bool-prop-set8
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z a)
		 	       (equiv ((union X Y) Z)
                                      (or (X Z)
                                          (Y Z))))))))))))

(th~defproblem bool-prop-set80
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
			      (= (union (intersection X Y)
                                        (setminus X Y))
                                 X))))))))

(th~defproblem bool-prop-set81
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
			    (forall (lam (Z (o a))
                               (= (setminus X (setminus Y Z))
                                  (union (setminus X Y)
                                         (intersection X Z))))))))))))

(th~defproblem bool-prop-set82
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (= (setminus X (setminus X Y))
                                 (intersection X Y)))))))))

(th~defproblem bool-prop-set83
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (= (setminus (union X Y) Y)
                                 (setminus X Y)))))))))

(th~defproblem bool-prop-set84
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (equiv (= (intersection X Y) emptyset)
                                     (= (setminus X Y) X)))))))))
    
(th~defproblem bool-prop-set85
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			   (forall (lam (Y (o a)) 
                            (forall (lam (Z (o a)) 
                               (= (setminus X (union Y Z))
                                  (intersection (setminus X Y)
                                                (setminus X Z))))))))))))

(th~defproblem bool-prop-set86
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (= (setminus X (intersection Y Z))
                                  (union (setminus X Y) 
                                         (setminus X Z))))))))))))

(th~defproblem bool-prop-set87
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (= (setminus (union X Y)
                                           (intersection X Y))
                                 (union (setminus X Y)
                                        (setminus Y X))))))))))

(th~defproblem bool-prop-set88
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (= (setminus (setminus X Y) Z)
                                  (setminus X (union Y Z))))))))))))

(th~defproblem bool-prop-set89
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (= (setminus (union X Y) Z)
                                  (union (setminus X Z)
                                         (setminus Y Z))))))))))))

(th~defproblem bool-prop-set9
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z a)
		     	       (equiv (in Z (intersection X Y))   
                                      (and (in Z X)
                                           (in Z Y))))))))))))

(th~defproblem bool-prop-set90
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (implies (= (setminus X Y)
                                          (setminus Y X))
                                       (= X Y)))))))))

(th~defproblem bool-prop-set91
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (= (exclunion X Y)
                                 (union (setminus X Y)
                                        (setminus Y X))))))))))

(th~defproblem bool-prop-set92
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
			      (and (= (exclunion X emptyset) X)
                                   (= (exclunion emptyset X) X)))))))))

(th~defproblem bool-prop-set93
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a)) 
			     (= (exclunion X X) emptyset))))))

(th~defproblem bool-prop-set95
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (= (union X Y) 
                                 (union (exclunion X Y)
                                        (intersection X Y))))))))))

(th~defproblem bool-prop-set96
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (= (exclunion X Y)
                                 (setminus (union X Y)
                                           (intersection X Y))))))))))

(th~defproblem bool-prop-set97
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
		  	       (= (setminus (exclunion X Y) Z)
                                  (union (setminus X (union Y Z))
                                         (setminus Y (union X Z)))))))))))))

(th~defproblem bool-prop-set98
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (= (setminus X (exclunion Y Z))
                                  (union (setminus X (union Y Z))
                                         (intersection (intersection X Y) Z)))
                               )))))))))

(th~defproblem bool-prop-set99
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
			       (= (exclunion (exclunion X Y) Z)
                                  (exclunion X (exclunion Y Z))))))))))))

(th~defproblem basic-prop-set1
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
         (conclusion conc
                     (all-types aa (= (superset emptyset)
                        (lam (x (o aa))
                             (= x emptyset))))))

(th~defproblem basic-prop-set2 
         (in typed-set) 
         (conclusion conc
                     (= (union-over-collection emptyset) emptyset)))

(th~defproblem basic-prop-set6
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (conclusion conc (all-types aa (forall (lam (x i) (forall (lam (y i)  
		     (implies (= (lam (z i) (= z x))
				 (lam (z i) (= z y)))
			      (= x y)))))))))


(th~defproblem basic-prop-set8 
         (in typed-set) 
         (constants (x (all-types a a))
		    (y1 (all-types a a))
		    (y2 (all-types a a))) 
         (conclusion conc 
            (implies (= (lam (z i) (= z x)) (lam (z i) (or (= z y1) (= z y2))))
                     (and (= x y1) (= x y2)))))

(th~defproblem basic-prop-set9    (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
         (constants (x i) (y1 i) (y2 i))
         (conclusion conc 
                     (implies (= (lam (z i) (= z x))
                                 (lam (z i) (or (= z y1) (= z y2))))
                              (= y1 y2))))

(th~defproblem basic-prop-set10
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x1 i) (x2 i) (y1 i) (y2 i))
	 (conclusion conc 
		     (implies (= (lam (z i) (or (= z x1) (= z x2)))
				 (lam (z i) (or (= z y1) (= z y2))))
			      (or (= x1 y1)
				  (or (= x1 y2)
				      (or (= x2 y1)
					  (= x2 y2)))))))

(th~defproblem basic-prop-set14
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (= (union (lam (z aa) (= z x)) 
                               (lam (z aa) (or (= z x) (= z y))))
                        (lam (z aa) (or (= z x) (= z y)))))))))))

(th~defproblem basic-prop-set15
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (= (union (lam (z aa) (= z y)) 
                               (lam (z aa) (or (= z x) (= z y))))
                        (lam (z aa) (or (= z x) (= z y)))))))))))

(th~defproblem basic-prop-set16
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies (= (intersection (lam (z aa) (= z x)) 
                                               (lam (z aa) (= z y)))
                                 emptyset)
                              (not (= x y))))))))))

(th~defproblem basic-prop-set17
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies (not (= x y))
                              (= (intersection (lam (z aa) (= z x)) 
                                               (lam (z aa) (= z y)))
                                 emptyset)))))))))

(th~defproblem basic-prop-set18
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies (or (= (intersection (lam (z aa) (= z x)) 
                                                   (lam (z aa) (= z y)))
                                     (lam (z aa) (= z x)))
                                  (= (intersection (lam (z aa) (= z x)) 
                                                   (lam (z aa) (= z y)))
                                     (lam (z aa) (= z y))))
                              (= x y)))))))))

(th~defproblem basic-prop-set19
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (= (intersection (lam (z aa) (= z x)) 
                                      (lam (z aa) (or (= z x) (= z y))))
                        (lam (z aa) (= z x))))))))))

(th~defproblem basic-prop-set20
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (equiv (= (setminus (lam (z aa) (= z x)) 
                                         (lam (z aa) (= z y)))
                               (lam (z aa) (= z x)))
                            (not (= x y))))))))))

(th~defproblem basic-prop-set21
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies (= (setminus (lam (z aa) (= z x)) 
                                           (lam (z aa) (= z y)))
                                 emptyset)
                              (= x y)))))))))
         
(th~defproblem basic-prop-set22
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (and (= (setminus (lam (z aa) (= z x)) 
                                       (lam (z aa) (or (= z x) (= z y))))
                             emptyset)
                          (= (setminus (lam (z aa) (= z x)) 
                                       (lam (z aa) (or (= z x) (= z y))))
                             emptyset)))))))))
       
(th~defproblem basic-prop-set23
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies 
                      (not (= x y))
                      (and (= (setminus (lam (z aa) (= z x)) 
                                        (lam (z aa) (or (= z x) (= z y))))
                              (lam (z aa) (= z x)))
                           (= (setminus (lam (z aa) (= z x)) 
                                        (lam (z aa) (or (= z x) (= z y))))
                              (lam (z aa) (= z y))))))))))))
       
(th~defproblem basic-prop-set24
         (in typed-set)
         (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
                     (implies (subset (lam (z aa) (= z x))
                                      (lam (z aa) (= z y)))
                              (= (lam (z aa) (= z x))
                                 (lam (z aa) (= z y)))))))))))

(th~defproblem basic-prop-set25
         (in typed-set)
         (constants (x i) (y i) (z i))
         (conclusion conc 
                     (implies (subset (lam (u i) (= u z))
                                      (lam (u i) (or (= u x) (= u y))))
                              (or (= z x) (= z y)))))

(th~defproblem basic-prop-set26
         (in typed-set)
         (constants (x i) (y i) (z i))
         (conclusion conc 
                     (implies (subset (lam (u i) (or (= u x) (= u y)))
                                      (lam (u i) (= u z)))
                              (and (= x z) (= y z)))))

(th~defproblem basic-prop-set27
         (in typed-set)
         (constants (x i) (y i) (z i))
         (conclusion conc 
                     (implies (subset (lam (u i) (or (= u x) (= u y)))
                                      (lam (u i) (= u z)))
                              (= (lam (u i) (or (= u x) (= u y)))
                                 (lam (u i) (= u z))))))

(th~defproblem basic-prop-set28
         (in typed-set)
         (constants (x1 i) (x2 i) (y1 i) (y2 i))
         (conclusion conc
                     (implies (subset (lam (u i) (or (= u x1) (= u x2)))
                                      (lam (u i) (or (= u y1) (= u y2))))
                              (and (or (= x1 y1) (= x1 y2))
                                   (or (= x2 y1) (= x2 y2))))))

(th~defproblem basic-prop-set29
         (in typed-set)
         (constants (x i) (y i) (z i))
         (conclusion conc
                     (implies (not (= x y))
                              (= (exclunion (lam (u i) (= u x)) 
                                            (lam (u i) (= u y)))
                                 (lam (u i) (or (= u x) (= u y)))))))
         
(th~defproblem basic-prop-set30
         (in typed-set)
         (conclusion conc 
                     (all-types aa (forall (lam (x aa) (= (superset (lam (u aa) (= u x)))
                        (lam (u (o aa)) (or (= u emptyset)
                                           (= u (lam (v aa) (= v x)))))))))))

(th~defproblem basic-prop-set31
	       (in typed-set)
	       (conclusion conc
			   (all-types aa (forall (lam  (x (o aa))
						       (= (union-over-collection (lam (u (o aa)) (= u x)))
   x))))))

(th~defproblem basic-prop-set32
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
		     (= (union-over-collection
			 (lam (u (o aa)) (or (= u (lam (v aa) (= v x)))
					    (= u (lam (v aa) (= v y))))))
			(lam (u aa) (or (= u x) (= u y)))))))))))

(th~defproblem basic-prop-set33
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x1 i) (x2 i) (y1 i) (y2 i))
	 (conclusion conc
		     (implies (= (pair x1 x2)
                                 (pair y1 y2))
                              (and (= x1 y1)
                                   (= x2 y2)))))

(th~defproblem basic-prop-set34
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (y i) (x1 i) (x2 i) (y1 i) (y2 i))
	 (conclusion conc
		     (equiv (in (pair x y)
                                (cartesian-product (lam (u i) (= u x1))
                                                   (lam (u i) (= u y1))))
                            (and (= x x1)
                                 (= y y1)))))

(th~defproblem basic-prop-set35
  (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
  (constants (x i) (y i) (x1 i) (x2 i) (y1 i) (y2 i))
  (conclusion conc
              (= (cartesian-product (lam (u i) (= u x))
                                    (lam (u i) (= u y)))
                 (lam (p (tuple i i))
                      (= p (pair x y))))))

(th~defproblem basic-prop-set36    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
          (constants (x i) (y i) (z i))
         (conclusion conc
                     (= (cartesian-product
                         (lam (u i) (= u x))
                         (lam (u i) (or (= u y) (= u z))))
                        (lam (u (tuple i i))
                             (or (= u (pair x y))
                                 (= u (pair x z)))))))

(th~defproblem basic-prop-set37
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (Y (o i)))
	 (conclusion conc 
                     (equiv (subset (lam (u i) (= u x)) Y)
                            (in x Y))))

(th~defproblem basic-prop-set38
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x1 i) (x2 i) (Z (o i)))
	 (conclusion conc 
		     (equiv (subset (lam (u i) (or (= u x1) (= u x2))) Z)
			    (and (in x1 Z) (in x2 Z)))))

(th~defproblem basic-prop-set39
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (Y (o i)))
	 (conclusion conc 
		     (equiv (subset Y (lam (u i) (= u x)))
			    (or (= Y emptyset) (= Y (lam (u i) (= u x)))))))

(th~defproblem basic-prop-set40
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (Y (o i)) (Z (o i)))
	 (conclusion conc 
		     (implies (and (subset Y Z) (not (in x Y)))
			      (subset Y (setminus Z (lam (u i) (= u x)))))))

(th~defproblem basic-prop-set41
	 (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (Z (o i)))
	 (conclusion conc 
		     (implies (and (not (= Z (lam (u i) (= u x))))
				   (in x Z))
			      (exists (lam (y i) (and (in y Z) (not (= y x))))))))

(th~defproblem basic-prop-set42
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x1 i) (x2 i) (Z (o i)))
	 (conclusion conc 
		     (equiv (subset Z (lam (u i) (or (= u x1) (= u x2))))
			    (or (or (= Z emptyset)
                                    (= Z (lam (u i) (= u x1))))
				(or (= Z (lam (u i) (= u x2)))
                                    (= Z (lam (u i) (or (= u x1) (= u x2)))))))))

(th~defproblem basic-prop-set43
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (z i) (X (o i)) (Y (o i)))
	 (conclusion conc 
		     (implies (= (lam (u i) (= u z))
                                 (union X Y))
                              (or (and (= X (lam (u i) (= u z))) 
                                       (= Y (lam (u i) (= u z))))
                                  (or (and (= X emptyset)
                                           (= Y (lam (u i) (= u z))))
                                      (and (= X (lam (u i) (= u z)))
                                           (= Y emptyset)))))))

(th~defproblem basic-prop-set44
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (z i) (X (o i)) (Y (o i)))
	 (conclusion conc 
		     (implies (and (= (lam (u i) (= u z))
                                      (union X Y))
                                   (not (= X Y)))
                              (or (= X emptyset) (= Y emptyset)))))

(th~defproblem basic-prop-set45
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (z i) (X (o i)) (Y (o i)))
	 (conclusion conc 
		     (implies (= (union (lam (u i) (= u z)) X)
                                 X)
                              (in z X))))

(th~defproblem basic-prop-set46
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (z i) (X (o i)) (Y (o i)))
	 (conclusion conc 
		     (implies (in z X)
                              (= (union (lam (u i) (= u z)) X)
                                 X))))

(th~defproblem basic-prop-set47
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (z i) (X (o i)) (Y (o i)))
	 (conclusion conc 
		     (implies (= (union (lam (u i) (= u z)) X)
                                 X)
                              (in z X))))

(th~defproblem basic-prop-set48
	 (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies (and (in x Z) (in y Z))
                              (= (union (lam (u i) (or (= u x) (= u y))) Z)
                                 Z))))

(th~defproblem basic-prop-set49
	 (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (not (= (union (lam (u i) (= u x)) Z) emptyset))))

(th~defproblem basic-prop-set50
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (not (= (union (lam (u i) (or (= u x) (= u y))) Z)
                             emptyset))))

(th~defproblem basic-prop-set51
         (in typed-set)
         (reference "mizar-lib.basic-prop-sets")
         (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies (= (intersection Z (lam (u i) (= u x)))
                                 (lam (u i) (= u x)))
                              (in x Z))))

(th~defproblem basic-prop-set52
	       (in typed-set)
	       (reference "mizar-lib.basic-prop-sets")
	       (author Trybulec)
	       (constants (x i) (y i) (Z (o i)))
	       (conclusion conc 
			   (implies (in x Z)
                                    (= (intersection Z (lam (u i) (= u x)))
                                       (lam (u i) (= u x))))))

(th~defproblem basic-prop-set53
               (in typed-set)
	       (reference "mizar-lib.basic-prop-sets")
	       (author Trybulec)
	       (constants (x i) (y i) (Z (o i)))
	       (conclusion conc 
			   (implies
			    (and (in x Z) (in y Z))
			    (= (intersection (lam (u i) (or (= u x) (= u y))) 
					     Z)
			       (lam (u i) (or (= u x) (= u y)))))))

(th~defproblem basic-prop-set54
	       (in typed-set)
	       (reference "mizar-lib.basic-prop-sets")
	       (author Trybulec)
	       (constants (x i) (y i) (Z (o i)))
	       (conclusion conc 
			   (implies
			    (= (intersection (lam (u i) (= u x))
					     Z)
			       emptyset)
			    (not (in x Z)))))

(th~defproblem basic-prop-set55
	       (in typed-set)
	       (reference "mizar-lib.basic-prop-sets")
	       (author Trybulec)
	       (constants (x i) (y i) (Z (o i)))
	       (conclusion conc
			   (implies
			    (= (intersection (lam (u i) (or (= u x) (= u y)))
					     Z)
			       emptyset)
			    (and (not (in x Z))
				 (not (in y Z))))))

(th~defproblem basic-prop-set56
	       (in typed-set)
	       (constants (x (all-types aa aa))
			  (y (all-types aa aa))
			  (Z (all-types aa (o aa))))
	       (conclusion conc 
			   (all-types aa
				      (implies
				       (not (in x Z))
				       (= (intersection (lam (u aa) (= u x))
							Z)
					  emptyset)))))

(th~defproblem basic-prop-set57    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (and (not (in x Z))
			   (not (in y Z)))
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 emptyset))))

(th~defproblem basic-prop-set58    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (or
		      (= (intersection (lam (u i) (= u x))
				       Z)
			 emptyset)
		      (= (intersection (lam (u i) (= u x))
				       Z)
			 (lam (u i) (= u x))))))

(th~defproblem basic-prop-set59    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 (lam (u i) (= u x)))
		      (or (not (in y Z)) (= x y)))))

(th~defproblem basic-prop-set60
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (and (in x Z) 
			   (or (not (in y Z))
			       (= x y)))
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 (lam (u i) (= u x))))))

(th~defproblem basic-prop-set61
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 (lam (u i) (= u y)))
		      (or (not (in x Z))
			  (= x y)))))

(th~defproblem basic-prop-set62
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (and (in y Z) 
			   (or (not (in x Z))
			       (= x y)))
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 (lam (u i) (= u y))))))


(th~defproblem basic-prop-set63
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (implies
		      (= (intersection (lam (u i) (or (= u x) (= u y)))
				       Z)
			 (lam (u i) (or (= u x) (= u y))))
		      (and (in x Z)
			   (in y Z)))))


(th~defproblem basic-prop-set64
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (equiv
		      (in x (setminus Y (lam (u i) (= u x))))
		      (and (in z Y) (not (= z x))))))


(th~defproblem basic-prop-set65
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus Y (lam (u i) (= u x)))
			 Y)
		      (not (in x Y)))))


(th~defproblem basic-prop-set66
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus Y (lam (u i) (= u x)))
			 emptyset)
		      (or (= Y emptyset) (= Y (lam (u i) (= u x)))))))


(th~defproblem basic-prop-set67
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (= u x)) Y)
			 (lam (u i) (= u x)))
		      (not (in x Y)))))


(th~defproblem basic-prop-set68
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (= u x)) Y)
			 emptyset)
		      (in x Y))))


(th~defproblem basic-prop-set69
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (z i) (Y (o i)))
	 (conclusion conc 
		     (or
		      (= (setminus (lam (u i) (= u x)) Y)
			 emptyset)
		      (= (setminus (lam (u i) (= u x)) Y)
			 (lam (u i) (= u x))))))


(th~defproblem basic-prop-set70
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 (lam (u i) (= u x)))
		      (and (not (in x Z))
			   (or (in y Z)
			       (= x y))))))


(th~defproblem basic-prop-set71
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 (lam (u i) (= u y)))
		      (and (or (in x Z)
			       (= x y))
			   (not (in y Z))))))


(th~defproblem basic-prop-set72
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 (lam (u i) (or (= u x) (= u y))))
		      (and (not (in x Z))
			   (not (in y Z))))))


(th~defproblem basic-prop-set73
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 emptyset)
		      (and (in x Z)
			   (in y Z)))))


(th~defproblem basic-prop-set74
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (or
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 emptyset)
		      (or 
		       (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			  (lam (u i) (= u x)))
		       (or (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			      (lam (u i) (= u y)))
			   (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			      (lam (u i) (or (= u x) (= u y)))))))))


(th~defproblem basic-prop-set75
	    (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
	 (constants (x i) (y i) (Z (o i)))
	 (conclusion conc 
		     (equiv
		      (= (setminus (lam (u i) (or (= u x) (= u y))) Z)
			 emptyset)
		      (or (= Z emptyset)
			  (or (= Z (lam (u i) (= u x)))
			      (or (= Z (lam (u i) (= u y)))
				  (= Z (lam (u i) (or (= u x) (= u y))))))))))


    

(th~defproblem basic-prop-set87
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)))
         (conclusion conc 
                     (equiv
                      (in X (superset (exclunion A B)))
                      (and (subset X (union A B))
                           (misses X (intersection A B))))))

(th~defproblem basic-prop-set88
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)))
         (conclusion conc 
                     (implies
                      (and (in X (superset A)) (in Y (superset A)))
                      (in (union X Y) (superset A)))))

(th~defproblem basic-prop-set89
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)))
         (conclusion conc 
                     (implies
                      (or (in X (superset A)) (in Y (superset A)))
                      (in (intersection X Y) (superset A)))))

(th~defproblem basic-prop-set90
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)))
         (conclusion conc
                     (implies
                      (in X (superset A))
                      (in (setminus X Y) (superset A)))))
(th~defproblem basic-prop-set91
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)))
         (conclusion conc 
                     (implies
                      (and (in X (superset A)) (in Y (superset A)))
                      (in (exclunion X Y) (superset A)))))


(th~defproblem basic-prop-set92
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)))
         (conclusion conc 
                     (implies
                      (in X A)
                      (subset X (union-over-collection A)))))


(th~defproblem basic-prop-set93
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)))
         (conclusion conc 
                     (= (union-over-collection (lam (u (o i)) (or (= u X) (= u Y))))
                        (union X Y))))
   


(th~defproblem basic-prop-set94
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (implies (forall (lam (X (o i)) (implies (in X A) (subset X Z))))
                              (subset (union-over-collection A) Z))))
   


(th~defproblem basic-prop-set95
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (implies (subset A B)
                              (subset (union-over-collection A)
                                      (union-over-collection B)))))
   


(th~defproblem basic-prop-set96
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (= (union-over-collection (union A B))
                        (union (union-over-collection A)
                               (union-over-collection B)))))
   


(th~defproblem basic-prop-set97
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (subset (union-over-collection (intersection A B))
                             (intersection (union-over-collection A)
                                           (union-over-collection B)))))
   


(th~defproblem basic-prop-set98
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o i)) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (implies (forall (lam (X (o i)) (implies (in X A) (= (intersection X B) emptyset))))
                              (= (intersection (union-over-collection A) B)
                                 emptyset))))
   

(th~defproblem basic-prop-set99
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o i)) (B (o i)) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (= (union-over-collection (superset A))
                        A)))
   


   


(th~defproblem basic-prop-set101
            (in typed-set)
  (reference "mizar-lib.basic-prop-sets")
  (author Trybulec)
         (constants (A (o (o i))) (B (o (o i))) (X (o i)) (Y (o i)) (Z (o i)))
         (conclusion conc 
                     (implies (forall (lam (X (o i))
                                           (forall (lam (Y (o i))
                                                        (implies (and (not (= X Y))
                                                                      (and (in X (union A B))
                                                                           (in Y (union A B))))
                                                                 (= (intersection X Y) emptyset))))))
                              (= (union-over-collection (intersection A B))
                                 (intersection (union-over-collection A) 
                                               (union-over-collection B))))))
   

(th~defproblem basic-prop-set102
         (in typed-set) 
         (constants (x i) (y i) (z (tuple i i)) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (in z (cartesian-product X1 Y1))
                              (exists (lam (x i)
                                           (exists (lam (y i)
                                                        (= (pair x y) z))))))))

(th~defproblem basic-prop-set103
         (in typed-set) 
         (constants (x i) (y i) (z (tuple i i)) (A (o (tuple i i)))
		    (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (subset A (cartesian-product X1 Y1))
                                   (in z A))
                              (exists (lam (x i) 
                                           (exists (lam (y i)
                                                        (and (in x X1)
                                                             (and (in y Y1)
                                                                  (= z (pair x y)))))))))))

(th~defproblem basic-prop-set104
         (in typed-set) 
         (constants (x i) (y i) (z (tuple i i)) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (in z (intersection (cartesian-product X1 Y1) (cartesian-product X2 Y2)))
                              (exists (lam (x i) 
                                           (exists (lam (y i)
                                                        (and (= z (pair x y))
                                                             (and (in x (intersection X1 Y1))
                                                                  (in y (intersection X2 Y2)))))))))))

;(th~defproblem basic-prop-set105
;         (in typed-set) 
;         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
;         (conclusion conc 
;                     (subset (cartesian-product X1 Y1) (superset (superset (union X Y))))))

(th~defproblem basic-prop-set106
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (equiv (in (pair x y) (cartesian-product X1 Y1))
                            (and (in x X1) (in y Y1)))))

(th~defproblem basic-prop-set107
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (in (pair x y) (cartesian-product X1 Y1))
                              (in (pair y x) (cartesian-product Y1 X1)))))

(th~defproblem basic-prop-set108
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (forall (lam (x i) 
                                           (forall (lam (y i)
                                                        (equiv (in (pair x y) (cartesian-product X1 Y1))
                                                               (in (pair x y) (cartesian-product X2 Y2)))))))
                              (= (cartesian-product X1 Y1) (cartesian-product X2 Y2)))))

   


(th~defproblem basic-prop-set109
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (tuple i i)))
		    (B (o (tuple i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (subset A (cartesian-product X1 Y1))
                                   (forall (lam (x i) 
                                                (forall (lam (y i)
                                                             (implies (in (pair x y) A) (in (pair x y) B)))))))
				   (subset A B ))))

(th~defproblem basic-prop-set110
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (tuple i i)))
		    (B (o (tuple i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (subset A (cartesian-product X1 Y1))
                                   (and (subset B (cartesian-product X1 Y1))
                                        (forall (lam (x i) 
                                                     (forall (lam (y i)
                                                                  (equiv (in (pair x y) A) (in (pair x y) B))))))))
                              (= A B))))
         
(th~defproblem basic-prop-set111
         (in typed-set) 
         (constants (x i) (y i) (z (tuple i i)) (A (o (tuple i i)))
		    (B (o (tuple i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (forall (lam (z (tuple i i)) (implies (in z A) 
                                                                     (exists (lam (x i)
                                                                                  (exists (lam (y i) (= z (pair x y)))))))))
                                   (forall (lam (x i) 
                                                     (forall (lam (y i)
                                                                  (implies (in (pair x y) A) (in (pair x y) B)))))))
                              (subset A B))))
         
(th~defproblem basic-prop-set112
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (tuple i i)))
		    (B (o (tuple i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (forall (lam (z (tuple i i)) (implies (in z A) 
                                                                     (exists (lam (x i)
                                                                                  (exists (lam (y i) (= z (pair x y)))))))))
                                   (and (forall (lam (z (tuple i i)) (implies (in z B) 
                                                                     (exists (lam (x i)
                                                                                  (exists (lam (y i) (= z (pair x y)))))))))
                                        
                                        (forall (lam (x i) 
                                                     (forall (lam (y i) 
                                                                  (equiv (in (pair x y) A) (in (pair x y) B))))))))
                              (= A B))))
         
(th~defproblem basic-prop-set113
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (equiv (= (cartesian-product X1 Y1) emptyset)
                            (or (= X1 emptyset) (= Y1 emptyset)))))      

(th~defproblem basic-prop-set114
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (and (not (= X1 emptyset))
                                   (and (not (= Y1 emptyset))
                                        (= (cartesian-product X1 Y1) (cartesian-product Y1 X1))))
                              (= X Y))))
                              

(th~defproblem basic-prop-set115
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
         (conclusion conc 
                     (implies (= (cartesian-product X1 X1) (cartesian-product Y1 Y1))
                              (= X1 Y1))))

;(th~defproblem basic-prop-set116
;         (in typed-set) 
;         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i))) 
;         (conclusion conc 
;                     (implies (subset X1 (cartesian-product X1 X1))
;                              (= X1 emptyset))))

(th~defproblem basic-prop-set117
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))
		    (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (implies (and (not (= Z1 emptyset))
                                   (or (subset (cartesian-product X1 Z1) (cartesian-product Y1 Z1))
                                       (subset (cartesian-product Z1 X1) (cartesian-product Z1 Y1))))
                              (subset X1 Y1))))

(th~defproblem basic-prop-set118
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (implies (subset X1 Y1)
                              (and (subset (cartesian-product X1 Z1) (cartesian-product Y1 Z1))
                                   (subset (cartesian-product Z1 X1) (cartesian-product Z1 Y1))))))

(th~defproblem basic-prop-set119
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (implies (and (subset X1 Y1) (subset X2 Y2))
                              (subset (cartesian-product X1 X2) (cartesian-product Y1 Y2)))))

(th~defproblem basic-prop-set12
    (in typed-set)
    (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
        (and (subset (lam (z aa) (= z x)) (lam (z aa) (or (= z x) (= z y))))
             (subset (lam (z aa) (= z y)) (lam (z aa) (or (= z x) (= z y))))))))))))

(th~defproblem basic-prop-set120
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (and (= (cartesian-product (union X1 Y1) Z1)
                             (union (cartesian-product X1 Z1) (cartesian-product Y1 Z1)))
                           (= (cartesian-product Z1 (union X1 Y1))
                             (union (cartesian-product Z1 X1) (cartesian-product Z1 Y1))))))

(th~defproblem basic-prop-set121
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (= (cartesian-product (union X1 Y1) (union X2 Y2))
                        (union (cartesian-product X1 Y1)
                               (union (cartesian-product X1 Y2)
                                      (union (cartesian-product X2 Y1)
                                             (cartesian-product X2 Y2)))))))





(th~defproblem basic-prop-set122
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (and (= (cartesian-product (intersection X1 Y1) Z1)
                             (intersection (cartesian-product X1 Z1) (cartesian-product Y1 Z1)))
                           (= (cartesian-product Z1 (intersection X1 Y1))
                             (intersection (cartesian-product Z1 X1) (cartesian-product Z1 Y1))))))

(th~defproblem basic-prop-set123
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (= (cartesian-product (intersection X1 Y1) (intersection X2 Y2))
                        (intersection (cartesian-product X1 Y1) (cartesian-product X2 Y2)))))


(th~defproblem basic-prop-set124
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (implies (and (subset X1 Y1) (subset X2 Y2))
                              (= (intersection (cartesian-product X1 Y2) (cartesian-product Y1 X2))
                                 (cartesian-product X1 X2)))))

(th~defproblem basic-prop-set125
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (and (= (cartesian-product (setminus X1 Y1) Z1)
                             (setminus (cartesian-product X1 Z1) (cartesian-product Y1 Z1)))
                          (= (cartesian-product Z1 (setminus X1 Y1))
                             (setminus (cartesian-product Z1 X1) (cartesian-product Z1 Y1))))))

(th~defproblem basic-prop-set126
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc 
                     (= (setminus (cartesian-product X1 X2) (cartesian-product Y1 Y2))
                        (union (cartesian-product (setminus X1 Y1) X2)
                               (cartesian-product X1 (setminus X2 Y2))))))

(th~defproblem basic-prop-set127
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))
		    (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (implies (or (= (intersection X1 X2) emptyset)
                                  (= (intersection Y1 Y2) emptyset))
                              (= (intersection (cartesian-product X1 Y1) (cartesian-product X2 Y2)) emptyset))))

(th~defproblem basic-prop-set128
         (in typed-set) 
         (constants (x i) (y i) (z i) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (equiv (in (pair x y) (cartesian-product (lam (u i) (= u z)) Y1))
                            (and (= x z) (in y Y1)))))

(th~defproblem basic-prop-set129
         (in typed-set) 
         (constants (x i) (y i) (z i) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (equiv (in (pair x y) (cartesian-product X1 (lam (u i) (= u z))))
                            (and (in x X1) (= y z)))))

(th~defproblem basic-prop-set13
    (in typed-set)
    (conclusion conc (all-types aa (forall (lam (x aa) (forall (lam (y aa) 
      (implies (or (= (union (lam (z aa) (= z x)) 
                       (lam (z aa) (= z y)))
                (lam (z aa) (= z x)))
             (= (union (lam (z aa) (= z x)) 
                       (lam (z aa) (= z y)))
                (lam (z aa) (= z y))))
         (= x y)))))))))
             
(th~defproblem basic-prop-set130
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (implies (not (= X1 emptyset))
                              (and (not (= (cartesian-product (lam (u i) (= u x)) X1) emptyset))
                                   (not (= (cartesian-product X1 (lam (u i) (= u x))) emptyset))))))

(th~defproblem basic-prop-set131
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (implies (not (= x y))
                              (and (= (intersection (cartesian-product (lam (u i) (= u x)) X1) (cartesian-product (lam (u i) (= u y)) Y1))
                                      emptyset)
                                   (= (intersection (cartesian-product X1 (lam (u i) (= u x))) (cartesian-product Y1 (lam (u i) (= u y))))
                                      emptyset)))))
(th~defproblem basic-prop-set132
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o i)))
         (conclusion conc
                     (and (= (cartesian-product (lam (u i) (or (= u x) (= u y))) X1)
                             (union (cartesian-product (lam (u i) (= u x)) X1)
                                    (cartesian-product (lam (u i) (= u y)) X1)))
                          (= (cartesian-product X1 (lam (u i) (or (= u x) (= u y))))
                             (union (cartesian-product X1 (lam (u i) (= u x)))
                                    (cartesian-product X1 (lam (u i) (= u y))))))))
                          
(th~defproblem basic-prop-set133
         (in typed-set) 
         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))
		    (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o (tuple i i))))
         (conclusion conc
                     (equiv (= Z1 (cartesian-product X1 Y1))
                            (forall (lam (z (tuple i i)) 
                                         (equiv (in z Z1)
                                                (exists (lam (x i)
                                                             (exists (lam (y i)
                                                                          (and (in x X1) 
                                                                               (and (in y Y1) (= z (pair x y))))))))))))))

(th~defproblem basic-prop-set134
         (in typed-set) 
         (conclusion conc
		     (all-types aa (forall (lam (X1 (o aa))
				   (forall (lam (X2 (o aa))
				   (forall (lam (Y1 (o aa))
				   (forall (lam (Y2 (o aa))		
                     (implies (and (not (= X1 emptyset))
                                   (and (not (= Y1 emptyset))
                                        (= (cartesian-product X1 Y1) (cartesian-product X2 Y2))))
                              (and (= X1 Y1)
                                   (= X2 Y2))))))))))))))

;(th~defproblem basic-prop-set135
;         (in typed-set) 
;         (constants (x i) (y i) (z (o (o i i))) (A (o (o i i)))  (B (o (o i i))) (X1 (o i)) (Y1 (o i)) (X2 (o i)) (Y2 (o i)) (Z1 (o (o i i))))
;         (conclusion conc
;                     (implies (or (subset X1 (cartesian-product X1 Y1))
;                                  (subset X1 (cartesian-product Y1 X1)))
;                              (= X emptyset))))

(th~defproblem basic-prop-set136
         (in typed-set) 
         (conclusion  conc
                     (all-types aa
				(forall (lam (X1 (o aa))
						 (implies (not (= emptyset X1))
							  (exists (lam (x aa) (in x X1)))))))))





(th~defproblem set1
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
			    (implies (and (subset X Y) (subset Y X)) (= X Y))))))))

(th~defproblem set1a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
	 (constants (a (o i)) (b (o i)))
	 (assumption ass1 (subset a b))
	 (assumption ass2 (subset b a))
         (conclusion conc (= a b)))


(th~defproblem set2
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 commutativity of intersection)")
         (conclusion conc (forall (lam (X (o i))
				       (forall (lam (Y (o i)) 
						    (= (intersection X Y) (intersection Y X))))))))
			       

(th~defproblem set2a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 commutativity of intersection)")
         (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection a b) (intersection b a))))


(th~defproblem set3
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 associativity of intersection)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i)) 
			    (= (intersection (intersection X Y) Z) 
                               (intersection X (intersection Y Z)))))))))))

(th~defproblem set3a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 associativity of intersection)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (intersection (intersection a b) c) 
			     (intersection a (intersection b c)))))


(th~defproblem set4
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5)")
         (conclusion conc (forall (lam (x (o i)) (= (intersection x emptyset) emptyset)))))

(th~defproblem set4a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5)")
	 (constants (a (o i)))
         (conclusion conc (= (intersection a emptyset) emptyset)))

;;; Set-Difference

(th~defproblem set5
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))       
				       (= (intersection (setminus x y) y) emptyset)))))))

(th~defproblem set5a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection (setminus a b) b) emptyset)))

(th~defproblem set6
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))       
				       (= (intersection (setminus x y) y) emptyset)))))))

(th~defproblem set6a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection (setminus a b) b) emptyset)))

(th~defproblem set7
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))
			    (forall (lam (z (o i)) 	
				       (= (intersection (setminus x y) z)
					  (setminus (intersection x z)
						    (intersection y z)))))))))))

(th~defproblem set7a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (intersection (setminus a b) c)
			     (setminus (intersection a c) (intersection b c)))))


(th~defproblem set8
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))
					(= (setminus x (setminus x y))
					   (intersection x y))))))))

(th~defproblem set8a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (setminus a (setminus a b)) (intersection a b))))


(th~defproblem set9
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.4)")
         (conclusion conc (forall (lam (X (o (o i)))
			   (forall (lam (y (o i))
			    (= (intersection (intersection-over-collection X) y)
			       (intersection-over-collection
				(lam (w (o i))
				  (forall (lam (z (o i))
					       (implies (in z X)
							(= w (intersection z y))))))))))))))

(th~defproblem set9a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.4)")
	 (constants (A (o (o i))) (b (o i)))
         (conclusion conc (= (intersection (intersection-over-collection A) b)
			     (intersection-over-collection
			      (lam (w (o i))
				(forall (lam (z (o i))
					     (implies (in z A)
						      (= w (intersection z b))))))))))

(th~defproblem set10
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.6)")
         (conclusion conc (forall (lam (X (o (o i)))
			   (forall (lam (y (o i))
			    (= (setminus (intersection-over-collection X) y)
			       (intersection-over-collection
				(lam (w (o i))
				  (forall (lam (z (o i))
					       (implies (in z X)
							(= w (setminus z y))))))))))))))

(th~defproblem set10a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.6)")
	 (constants (A (o (o i))) (b (o i)))
         (conclusion conc (= (setminus (intersection-over-collection A) b)
			     (intersection-over-collection
			      (lam (w (o i))
				(forall (lam (z (o i))
					     (implies (in z A)
						      (= w (setminus z b))))))))))


;;;; Union

(th~defproblem set11
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 commutativity of union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
			    (= (union X Y) (union Y X))))))))

(th~defproblem set11a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 commutativity of union)")
         (constants (a (o i)) (b (o i)))
         (conclusion conc (= (union a b) (union b a))))


(th~defproblem set12
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 associativity of union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i)) 
			    (= (union (union X Y) Z) 
                               (union X (union Y Z)))))))))))

(th~defproblem set12a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 associativity of union)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (union (union a b) c) 
			     (union a (union b c)))))


(th~defproblem set13
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1)")
         (conclusion conc (forall (lam (x (o i)) (= (union x emptyset) x)))))

(th~defproblem set13a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1)")
	 (constants (a (o i)))
         (conclusion conc (= (union a emptyset) a)))


(th~defproblem set14
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
         (conclusion conc (= (union-over-collection emptyset) emptyset)))


(th~defproblem set15
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of union
and intersection)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (union (intersection X Z) (intersection Y Z)))))))))))

(th~defproblem set15a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of union
and intersection)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc  (= (intersection (union a b) c)
			      (union (intersection a c) (intersection b c)))))

(th~defproblem set16
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (union (intersection X Y) Z)
                               (intersection (union X Z) (union Y Z)))))))))))

(th~defproblem set16a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc  (= (union (intersection a b) c)
			      (intersection (union a c) (union b c)))))

(th~defproblem set17
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
         (conclusion conc (forall (lam (X (o (o i)))
			   (forall (lam (y (o i))
			    (= (intersection (union-over-collection X) y)
			       (union-over-collection
				(lam (w (o i))
				  (forall (lam (z (o i))
					       (implies (in z X)
							(= w (intersection z y))))))))))))))

(th~defproblem set17a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
	 (constants (A (o (o i))) (b (o i)))
         (conclusion conc (= (intersection (union-over-collection A) b)
			     (union-over-collection
			      (lam (w (o i))
				(forall (lam (z (o i))
					     (implies (in z A)
						      (= w (intersection z b))))))))))


(th~defproblem set18
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
         (conclusion conc (forall (lam (X (o (o i)))
			   (forall (lam (Y (o (o i)))
			    (= (union-over-collection (union X Y))
			       (union (union-over-collection X) (union-over-collection Y))))))))) 

(th~defproblem set18a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
	 (constants (A (o (o i))) (B (o (o i))))
         (conclusion conc (= (union-over-collection (union A B))
			     (union (union-over-collection A) (union-over-collection B)))))


(th~defproblem set-count1
	 (in typed-set)
         (author chris)
         (reference "Copunterexample")
	 (constants (A (o (o i))) (B (o (o i))))
         (conclusion conc (= (union A B) A)))
			     


(th~defproblem set-test1
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP?")
	 (constants (a o) (b o))
	 (assumption ass1 a)
	 (assumption ass2 b)
         (conclusion conc (and a b)))

(th~defproblem set-test2
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP after two times imp-i?")
	 (constants (a o) (b o))
         (conclusion conc (implies a (implies b (and a b)))))
	 
(th~defproblem set-test3
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP after two times imp-i?")
	 (constants (a o) (b o))
         (conclusion conc (and (implies a (implies b (and a b)))
			       (implies a (implies b (and a b))))))

(th~defproblem set-top1
	 (in base)
         (author chris)
         (reference "Schubert.Topologie")
	 (constants (A (o i)) (B (o i)) (Top (o (o i)))
		    (in (o (o (o i)) (o i))) (innere ((o i) (o i)))
		    (subset (o (o i) (o i))))
	 (assumption ass1 (forall (lam (M (o i)) (in (innere M) Top))))
	 (assumption ass2 (forall (lam (M (o i)) (subset (innere M) M))))
	 (assumption ass3 (forall (lam (M (o i))
				       (implies (in M Top)
						(forall (lam (N (o i))
							     (implies (subset M N)
								      (subset M (innere N)))))))))
	 (assumption ass4 (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (forall (lam (P (o i))		    
			     (implies (and (subset M N) (subset N P))
				      (subset M P)))))))))
         (conclusion conc (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (implies (subset M N) (subset (innere M) (innere N)))))))))

(th~defproblem set-top2
	 (in base)
         (author chris)
         (reference "Schubert.Topologie")
	 (constants (A (o i)) (B (o i)) (Top (o (o i)))
		    (in (o (o (o i)) (o i))) (innere ((o i) (o i)))
		    (subset (o (o i) (o i))) (intersection ((o i) (o i) (o i)))
	            (== (o (o i) (o i))))
	 (assumption ass1 (forall (lam (M (o i)) (in (innere M) Top))))
	 (assumption ass2 (forall (lam (M (o i)) (subset (innere M) M))))
	 (assumption ass3 (forall (lam (M (o i))
				       (implies (in M Top)
						(forall (lam (N (o i))
							     (implies (subset M N)
								      (subset M (innere N)))))))))
	 (assumption ass4 (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (forall (lam (P (o i))		    
			     (implies (and (subset M N) (subset N P))
				      (subset M P)))))))))
	 (assumption ass5 (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (subset (intersection M N) M))))))
         (assumption ass6 (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (subset (intersection M N) N))))))
	 (assumption ass7 (forall (lam (M (o i))
			   (forall (lam (N (o i))
                            (forall (lam (W (o i))
	                     (implies (and (subset W M) (subset W N))
                                      (subset W (intersection M N))))))))))
         (assumption ass8 (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (implies (and (subset M N) (subset N M)) (== M N)))))))
         (conclusion conc (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (== (innere (intersection M N)) (intersection (innere M) (innere N)))))))))




(th~defproblem set-top2a
	 (in base)
         (author chris)
         (reference "Schubert.Topologie")
	 (constants (A i) (B i) (Top (o i))
		    (in (o (o i) i)) (innere (i i))
		    (subset (o i i)) (intersection (i i i))
	            (== (o i i)))
	 (assumption ass1 (forall (lam (M i) (in (innere M) Top))))
	 (assumption ass2 (forall (lam (M i) (subset (innere M) M))))
	 (assumption ass3 (forall (lam (M i)
				       (implies (in M Top)
						(forall (lam (N i)
							     (implies (subset M N)
								      (subset M (innere N)))))))))
	 (assumption ass4 (forall (lam (M i)
			   (forall (lam (N i)
			    (forall (lam (P i)		    
			     (implies (and (subset M N) (subset N P))
				      (subset M P)))))))))
	 (assumption ass5 (forall (lam (M i)
			   (forall (lam (N i)
			    (subset (intersection M N) M))))))
         (assumption ass6 (forall (lam (M i)
			   (forall (lam (N i)
			    (subset (intersection M N) N))))))
	 (assumption ass7 (forall (lam (M i)
			   (forall (lam (N i)
                            (forall (lam (W i)
	                     (implies (and (subset W M) (subset W N))
                                      (subset W (intersection M N))))))))))
         (assumption ass8 (forall (lam (M i)
			   (forall (lam (N i)
			    (implies (and (subset M N) (subset N M)) (== M N)))))))
         (conclusion conc (forall (lam (M i)
			   (forall (lam (N i)
			    (== (innere (intersection M N)) (intersection (innere M) (innere N)))))))))



(th~defproblem set-top2b
	 (in typed-set)
         (author chris)
         (reference "Schubert.Topologie")
	 (constants (A (o i)) (B (o i)) (Top (o (o i)))
		    (innere ((o i) (o i))))
	 (assumption ass1 (forall (lam (M (o i)) (in (innere M) Top))))
	 (assumption ass2 (forall (lam (M (o i)) (subset (innere M) M))))
	 (assumption ass3 (forall (lam (M (o i))
				       (implies (in M Top)
						(forall (lam (N (o i))
							     (implies (subset M N)
								      (subset M (innere N)))))))))
         (conclusion conc (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (= (innere (intersection M N)) (intersection (innere M) (innere N)))))))))



(th~defproblem set-top-manfred
	 (in typed-set)
         (author chris)
         (reference "Manfred Kerbers Axiomatization")
	 (constants (A (o i)) (B (o i)) (Top (o (o i)))
		    (inner ((o i) (o i))) (subset+ (o (o i) (o i))) (intersection+ ((o i) (o i) (o i))))
         (assumption ass?1 (forall (lam (M (o i)) (= (inner (inner M)) (inner M)))))
         (assumption  ass?2 (forall (lam (M (o i)) (forall (lam (N (o i)) 
			     (equiv (and (in M Top) (in N Top)) (in (intersection+ M N) Top)))))))
	 (assumption ass?3 (forall (lam (M (o i)) (subset+ M M))))
	 (assumption ass?4 (forall (lam (M (o i)) (forall (lam (N (o i))
			    (implies (subset+ M N) (subset+ (inner M) (inner N))))))))			            (assumption ass?5 (forall (lam (M (o i)) (forall (lam (N (o i)) (forall (lam (P (o i))
			    (implies (and (subset+ M N) (subset+ N P)) (subset+ M P)))))))))
	 (assumption ass?6 (forall (lam (M (o i)) (forall (lam (N (o i)) 
			    (subset+ (intersection+ M N) M))))))
	 (assumption ass?7 (forall (lam (M (o i)) (forall (lam (N (o i)) 
			    (subset+ (intersection+ M N) N))))))
	 (assumption ass?8 (forall (lam (M (o i)) (forall (lam (N (o i)) (forall (lam (W (o i))
			    (implies (and (subset+ W M) (subset+ W N)) (subset+ W (intersection+ M N))))))))))
	 (assumption ass?9 (forall (lam (M (o i)) (forall (lam (V (o i)) (forall (lam (W (o i))
			    (implies (and (subset+ W M) (subset+ V M)) (subset+ (intersection+ V W) M)))))))))
	 (assumption ass?10 (forall (lam (M (o i)) (forall (lam (N (o i)) 
                             (implies (and (subset+ M N) (subset+ N M)) (= M N)))))))
	 (assumption ass1 (forall (lam (M (o i)) (in (inner M) Top))))
	 (assumption ass2 (forall (lam (M (o i)) (subset+ (inner M) M))))
	 (assumption ass3 (forall (lam (M (o i))
				       (implies (in M Top)
						(forall (lam (N (o i))
							     (implies (subset+ M N)
								      (subset+ M (inner N)))))))))
	 
         (conclusion conc (forall (lam (M (o i))
			   (forall (lam (N (o i))
			    (= (inner (intersection+ M N)) (intersection+ (inner M) (inner N)))))))))

(th~defproblem set-top-manfred-a
	 (in typed-set)
         (author chris)
         (reference "Manfred Kerbers Axiomatization")
	 (constants (A i) (B i) (Top (o i))
		    (inner (i i)) (subset+ (o i i)) (intersection+ (i i i)))
         (assumption ass?1 (forall (lam (M i) (= (inner (inner M)) (inner M)))))
         (assumption  ass?2 (forall (lam (M i) (forall (lam (N i) 
			     (equiv (and (in M Top) (in N Top)) (in (intersection+ M N) Top)))))))
	 (assumption ass?3 (forall (lam (M i) (subset+ M M))))
	 (assumption ass?4 (forall (lam (M i) (forall (lam (N i)
			    (implies (subset+ M N) (subset+ (inner M) (inner N))))))))			            (assumption ass?5 (forall (lam (M i) (forall (lam (N i) (forall (lam (P i)
			    (implies (and (subset+ M N) (subset+ N P)) (subset+ M P)))))))))
	 (assumption ass?6 (forall (lam (M i) (forall (lam (N i) 
			    (subset+ (intersection+ M N) M))))))
	 (assumption ass?7 (forall (lam (M i) (forall (lam (N i) 
			    (subset+ (intersection+ M N) N))))))
	 (assumption ass?8 (forall (lam (M i) (forall (lam (N i) (forall (lam (W i)
			    (implies (and (subset+ W M) (subset+ W N)) (subset+ W (intersection+ M N))))))))))
	 (assumption ass?9 (forall (lam (M i) (forall (lam (V i) (forall (lam (W i)
			    (implies (and (subset+ W M) (subset+ V M)) (subset+ (intersection+ V W) M)))))))))
	 (assumption ass?10 (forall (lam (M i) (forall (lam (N i) 
                             (implies (and (subset+ M N) (subset+ N M)) (= M N)))))))
	 (assumption ass1 (forall (lam (M i) (in (inner M) Top))))
	 (assumption ass2 (forall (lam (M i) (subset+ (inner M) M))))
	 (assumption ass3 (forall (lam (M i)
				       (implies (in M Top)
						(forall (lam (N i)
							     (implies (subset+ M N)
								      (subset+ M (inner N)))))))))
	 
         (conclusion conc (forall (lam (M i)
			   (forall (lam (N i)
			    (= (inner (intersection+ M N)) (intersection+ (inner M) (inner N)))))))))


(th~defproblem leo-otter1
	 (in base)
         (author chris)
         (reference "Test")
	 (constants (q1 (o i)) (q2 (o i)) (q3 (o i)) (q4 (o i)) (q5 (o i)) (q6 (o i)) (q7 (o i)) (q8 (o i)) (q9 (o i)) (q10 (o i)) (q11 (o i)) (q12 (o i)))
         (conclusion conc (= (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q1 x) (q2 x)) (q3 x)) (q4 x)) (q5 x)) (q6 x)) (q7 x)) (q8 x)) (q9 x)) (q10 x)) (q11 x)) (q12 x)))
                             (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q12 x) (q11 x)) (q10 x)) (q9 x)) (q8 x)) (q7 x)) (q6 x)) (q5 x)) (q4 x)) (q3 x)) (q2 x)) (q1 x))))))

(th~defproblem leo-otter2
	 (in base)
         (author chris)
         (reference "Test")
	 (constants (q1 (o i)) (q2 (o i)) (q3 (o i)) (q4 (o i)) (q5 (o i)) (q6 (o i)) (q7 (o i)) (q8 (o i)) (q9 (o i)) (q10 (o i)) (q11 (o i)) (q12 (o i)))
         (conclusion conc (not (= (lam (x i) 
                                      (or (and (and (and (and (and (and (and (and (and (and (q1 x) (q2 x)) (q3 x)) (q4 x)) (q5 x)) (q6 x)) (q7 x)) (q8 x)) (q9 x)) (q10 x)) (q11 x)) (q12 x)))
                             (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q12 x) (q11 x)) (q10 x)) (q9 x)) (q8 x)) (q7 x)) (q6 x)) (q5 x)) (q4 x)) (q3 x)) (q2 x)) (q1 x)))))))

(th~defproblem leo-otter3
	 (in typed-set)
         (author chris)
         (reference "Test")
	 (conclusion conc (implies (empty (intersection (lam (x o) true) (lam
									   (x o)
									   false)))
				   (empty (intersection (lam (x o) (or x (not x))) (lam
									   (x o)
									   (and
									    x    
									    (not
									     x))))))))

(th~defproblem cas-test1a
	 (in natural)
         (author chris)
         (reference "Test")
	 (constants (nice-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption dummy (nice-numbers (union a b)))
	 (assumption ass1 (nice-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (nice-numbers (intersection
					 (lam (x num) (less x (plus 20 20)))
					 (lam (x num) (greater x (times 5 (plus 2 3))))))))

(th~defproblem cas-test1b
	 (in natural)
         (author chris)
         (reference "Test")
	 (constants (favourite-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption ass1 (favourite-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (favourite-numbers (lam (x num) (and (less x (plus 20 20))
					                       (greater x (times 5 (plus 2 3))))))))		



(th~defproblem cas-test1c
	 (in integer)
         (author chris)
         (reference "Test")
	 (conclusion conc (= (lam (x num) (and (greater x (gcd 10 8)) (less x (lcm 10
										   8))))
			     (intersection (lam (x num) (less x 40)) (lam (x num) (greater
										   x 2))))))


(th~defproblem cas-test2
	 (in natural)
	 (constants (ableit ((num num) (num num))) (f (num num)) (g (num num)) (log (num num)))
         (assumption ass1 (forall (lam (f (num num))
	                   (forall (lam (g (num num))
	                    (implies 
	                     (and (= (ableit f) (ableit g))
                                  (exists (lam (x num) (= (f x) (g x)))))
                            (= f g)))))))	
	 (conclusion conc (forall (lam (n num) 
                            (= (lam (x num) (times n (log x)))
                               (lam (x num) (log (power x n))))))))


(th~defproblem set56
	 (in typed-set)
	 (type-constants a)
         (conclusion conc 
		     (forall (lam (X (o a)) 
				  (forall (lam (Y (o a)) 
					       (forall (lam (Z (o a)) 
							    (equiv (= X (union Y Z))
								   (and (subset Y X)
									(and (subset Z X)
									     (forall (lam (V (o a))
											  (implies
											   (and
											    (subset Y V)
											    (subset Z V))
											   (subset X V)))))))))))))))


(th~defproblem set-dist-counter
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (intersection (union  X Z) (union Y Z)))))))))))


(th~defproblem set-dist
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (union (intersection X Z) (intersection Y Z)))))))))))



(th~defproblem strange-ho-example
         (in typed-set) 
	 (author cebrown)
         (constants (S (o (o i i))) (x i) (y i))
         (conclusion conc 
                     (implies (and (S (strange-ho-abbr S)) (strange-ho-abbr S x y))
			      (strange-ho-abbr S y x)))
	 (help "A short higher-order example in which the proof contains
a formula which is a proper subformula of itself."))



;;;; (for chris: test)

(th~defproblem chris-test
         (in typed-set) 
	 (author chris)
         (conclusion conc
	  (= (lam (F (o o)) (has-fixpoint F))
	     (union (lam (G (o o)) (is-constant-map G))
		    (lam (H (o o)) (is-identity H)))))
	 (help "nice example"))


(th~defproblem set-dist-complex
           (in typed-set)
           (conclusion conc (forall (lam (X (o i))
                             (forall (lam (Y (o i))
                              (forall (lam (Z (o i))
                                 (= (union (intersection X Y)
                                     (union (intersection Y Z)
                                            (intersection Z X)))
                                    (intersection (union X Y)
                                     (intersection (union Y Z)
                                                   (union Z X))))))))))))

(th~defproblem set-dist-complex-b
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
			   (= (union (intersection b c)
                                     (union (intersection c d)
                                            (intersection d b)))
			      (intersection (union b c)
					    (intersection (union c d)
							  (union d b))))))

(th~defproblem dialog-ex1
	       (in typed-set)
	       (constants (A (o i)) (B (o i)) (C (o i)))
	       (conclusion conc (in (intersection A B)
				    (powerset (intersection (union A C)
							    (union B C))))))

(th~defproblem with-mbase
	       (in typed-set)
	       (constants (A (o i)) (B (o i)) (C (o i)))
	       (conclusion conc (in (intersection A B)
				    (powersetp (intersection (union A C)
							    (union B C))))))
(th~defproblem set171+3
           (in typed-set)
	   (conclusion
	    (forall  (lam (B (o i))
	     (forall (lam (C (o i)) 
	      (forall (lam (D (o i))
			   (= (union B (intersection C D))
			      (intersection (union B C) (union B D)))))))))))

(th~defproblem set014+4
           (in typed-set)
	   (conclusion
	    (forall  (lam (A (o i))
	     (forall (lam (X (o i)) 
	      (forall (lam (Y (o i))
			   (equiv
			    (and (subset X A) (subset Y A))
			    (subset (union X Y) A))))))))))




;;;; Test Chris

(th~defproblem test-bool-prop-set87
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a)) 
                              (set= (setminus (union X Y)
                                           (intersection X Y))
                                 (union (setminus X Y)
                                        (setminus Y X))))))))))

(th~defproblem test-bool-prop-set88
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (conclusion conc (all-types a (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (set= (setminus (setminus X Y) Z)
                                  (setminus X (union Y Z))))))))))))



;;; Test: set-ext-contract*


(th~defproblem set-complex-1
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
			   (implies (not (= b c))
				    (= (union (intersection b c)
					      (union (intersection c d)
						     (intersection d b)))
				       (intersection (union b c)
						     (intersection (union c d)
								   (union d b)))))))

(th~defproblem set-complex-2
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (= X Y)
						       (= (union X Y) X))))))))


(th~defproblem set-complex-3
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (not (= X Y))
						       (not (= (union X Y) X)))))))))

(th~defproblem set-complex-4
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (not (= Y emptyset))
						       (not (= (union X Y) X)))))))))


(th~defproblem set-complex-5
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (subset X Y)
						       (= (union X Y) Y))))))))

(th~defproblem set-complex-5
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (subset X Y)
						       (= (union X Y) Y))))))))



(th~defproblem set-complex-6
	       (in typed-set)
	       (constants (b (o i)) (c (o i)) (d (o i)))
	       (conclusion conc 
	                   (forall (lam (X (o i)) 
					(forall (lam (Y (o i))
						      (implies 
						       (equiv  (exists (lam (Z i) (in Z X))) (forall (lam (Z i) (in Z Y))))
						       (= (exists (lam (V i) (in V (union X Y)))) (exists (lam (V i) (in V Y)))))))))))