;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
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

;;;;; just in order to make sure that everything is well defined; should be removed later
(oc=remove-theories)
(csm~erase)
(oc=deac-agents)
(load "~/omega/keim-3/prog/suggestion/agplan.lisp")

(th~deftheory TYPED-SET
	      (uses post)
	      (help "Simply typed sets and their operators."))

(th~defdef in
	   (in typed-set)
	   (type-variables bb)
           (definition
             (lam (x bb)
                  (lam (U (o bb))
                       (U x))))
	   (help "The element predicate, defined by application."))

(th~defdef emptyset
	   (in typed-set)
	   (type-variables aa)
           (definition (lam (x aa) false)) ; (and false (not false))))
	   (help "The empty set, defined by an elemtary contradiction (this
                  particular contradiction here can even be used by external
                  systems which do not know about falsehood alone."))

(th~defdef empty
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (M (o aa))
                  (forall (lam (x aa)
				(not (in x M))))))
	   (help "predicate, a set being empty."))

(th~defdef non-empty
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (M (o aa))
                  (exists (lam (x aa)
                               (in x M)))))
	   (help "predicate, a set being not empty."))


(th~defdef subset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (O aa))
                       (forall (lam (x aa) (implies (in x U) (in x V)))))))
	   (help "Subset, defined by implication"))


(th~defdef proper-subset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
		       (and (subset U V)
			    (exists (lam (x aa) (and (in x V) (not (in x U)))))))))
	   (help "Proper-Subset, defined a.o. by subset"))

(th~defdef superset
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (forall (lam (x aa) (implies (V x) (U x)))))))
           (help "Superset, defined by implication"))

(th~defdef powerset
	   (in typed-set)
	   (type-variables aa)
           (definition superset)
           (help "Powerset is the set of all subsets"))


(th~defdef set=
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (O aa))
                  (lam (V (O aa))
                       (and (subset U V) (subset V U)))))
	   (help "Equality on sets, defined by subset"))

(th~defdef union
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (lam (x aa)
                            (or (in x U) (in x V))))))
           (help "Set union, defined by disjunction"))


(th~defdef intersection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (O aa))
                  (lam (V (O aa))
                       (lam (x aa)
                            (and (in x U) (in x V))))))
	   (help "Set intersection, defined by conjunction."))

(th~defdef setminus
	   (in typed-set)
	   (type-variables aa)
           (definition 
             (lam (F (o aa))
                  (lam (G (o aa))
                       (lam (x aa)
                            (and (in x F)
                                 (not (in x G)))))))
	   (help "The set difference function. (setminus A B) is the set of all a in A that are not in B."))


(th~defdef set-complement
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (U (o aa))
                  (lam (V (o aa))
                       (lam (x aa)
                            (and (in x U) (not (in x V)))))))
	   (help "(set-complement U V) is the set of all a in U that are not in V."))

(th~defdef singleton
	   (in typed-set)
	   (type-variables aa)
           (definition 
             (lam (x aa)
                  (lam (y aa)
                       (= x y))))
	   (help "The singleton function, (singleton x) is the set that only contains x."))
		    


(th~defdef union-over-collection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (Coll (o (o aa)))
                  (lam (x aa)
                       (exists (lam (G (o aa))
                              	     (and (in G Coll)
                                 	  (in x G)))))))
	   (help "Union over a collection of sets, defined by existential
quantification."))

(th~defdef intersection-over-collection
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (Coll (o (o aa)))
                  (lam (x aa)
                       (forall  (lam (G (o aa))
				     (implies (in G Coll)
					      (in x G)))))))
           (help "Intersection over a collection of sets,
                  defined by universal quantification."))



(th~defdef meets
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa))
		  (lam (Y (o aa))
		       (exists (lam (Z aa) (and (in Z X) (in Z Y)))))))
	   (help "Predicate: There exists an element which is a member of both sets"))


(th~defdef misses
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa))
		  (lam (Y (o aa))
		       (not (exists  (lam (Z aa) (and (in Z X) (in Z Y))))))))
	   (help "Predicate: There exists no element which is a member of both sets"))

(th~defdef exclunion
	   (in typed-set)
	   (type-variables aa)
           (definition 
	     (lam (X (o aa)) (lam (Y (o aa))
				  (union (setminus X Y)
					 (setminus Y X)))))
	   (help "The exclusive union of two sets."))


   
(th~defdef add-one
	   (in typed-set)
	   (type-variables aa)
           (definition
             (lam (x aa)
                  (lam (p (o aa))
                       (lam (t aa) (or (p t) (= t x))))))
	   (help "Add one element to a set"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problems ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem chris-set1
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
			    (implies (and (subset X Y) (subset Y X)) (= X Y))))))))

(th~defproblem chris-set1a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
	 (constants (a (o i)) (b (o i)))
	 (assumption ass1 (subset a b))
	 (assumption ass2 (subset b a))
         (conclusion conc (= a b)))


(th~defproblem chris-set2
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 commutativity of intersection)")
         (conclusion conc (forall (lam (X (o i))
				       (forall (lam (Y (o i)) 
						    (= (intersection X Y) (intersection Y X))))))))
			       

(th~defproblem chris-set2a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 commutativity of intersection)")
         (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection a b) (intersection b a))))


(th~defproblem chris-set3
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 associativity of intersection)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i)) 
			    (= (intersection (intersection X Y) Z) 
                               (intersection X (intersection Y Z)))))))))))

(th~defproblem chris-set3a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 associativity of intersection)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (intersection (intersection a b) c) 
			     (intersection a (intersection b c)))))


(th~defproblem chris-set4
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5)")
         (conclusion conc (forall (lam (x (o i)) (= (intersection x emptyset) emptyset)))))

(th~defproblem chris-set4a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5)")
	 (constants (a (o i)))
         (conclusion conc (= (intersection a emptyset) emptyset)))

;;; Set-Difference

(th~defproblem chris-set5
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))       
				       (= (intersection (setminus x y) y) emptyset)))))))

(th~defproblem chris-set5a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection (setminus a b) b) emptyset)))

(th~defproblem chris-set6
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))       
				       (= (intersection (setminus x y) y) emptyset)))))))

(th~defproblem chris-set6a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.7)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (intersection (setminus a b) b) emptyset)))

(th~defproblem chris-set7
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))
			    (forall (lam (z (o i)) 	
				       (= (intersection (setminus x y) z)
					  (setminus (intersection x z)
						    (intersection y z)))))))))))

(th~defproblem chris-set7a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (intersection (setminus a b) c)
			     (setminus (intersection a c) (intersection b c)))))


(th~defproblem chris-set8
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre")
         (conclusion conc (forall (lam (x (o i))
			   (forall (lam (y (o i))
					(= (setminus x (setminus x y))
					   (intersection x y))))))))

(th~defproblem chris-set8a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.8.3)")
	 (constants (a (o i)) (b (o i)))
         (conclusion conc (= (setminus a (setminus a b)) (intersection a b))))


(th~defproblem chris-set9
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

(th~defproblem chris-set9a
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

(th~defproblem chris-set10
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

(th~defproblem chris-set10a
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

(th~defproblem chris-set11
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 commutativity of union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
			    (= (union X Y) (union Y X))))))))

(th~defproblem chris-set11a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 commutativity of union)")
         (constants (a (o i)) (b (o i)))
         (conclusion conc (= (union a b) (union b a))))


(th~defproblem chris-set12
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 associativity of union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i)) 
			    (= (union (union X Y) Z) 
                               (union X (union Y Z)))))))))))

(th~defproblem chris-set12a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1 associativity of union)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc (= (union (union a b) c) 
			     (union a (union b c)))))


(th~defproblem chris-set13
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1)")
         (conclusion conc (forall (lam (x (o i)) (= (union x emptyset) x)))))

(th~defproblem chris-set13a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.1)")
	 (constants (a (o i)))
         (conclusion conc (= (union a emptyset) a)))


(th~defproblem chris-set14
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
         (conclusion conc (= (union-over-collection emptyset) emptyset)))


(th~defproblem chris-set15
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of union
and intersection)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (union (intersection X Z) (intersection Y Z)))))))))))

(th~defproblem chris-set15a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of union
and intersection)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc  (= (intersection (union a b) c)
			      (union (intersection a c) (intersection b c)))))

(th~defproblem chris-set16
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (union (intersection X Y) Z)
                               (intersection (union X Z) (union Y Z)))))))))))

(th~defproblem chris-set16a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
	 (constants (a (o i)) (b (o i)) (c (o i)))
         (conclusion conc  (= (union (intersection a b) c)
			      (intersection (union a c) (union b c)))))

(th~defproblem chris-set17
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

(th~defproblem chris-set17a
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


(th~defproblem chris-set18
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
         (conclusion conc (forall (lam (X (o (o i)))
			   (forall (lam (Y (o (o i)))
			    (= (union-over-collection (union X Y))
			       (union (union-over-collection X) (union-over-collection Y))))))))) 

(th~defproblem chris-set18a
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2)")
	 (constants (A (o (o i))) (B (o (o i))))
         (conclusion conc (= (union-over-collection (union A B))
			     (union (union-over-collection A) (union-over-collection B)))))


(th~defproblem chris-set-count1
	 (in typed-set)
         (author chris)
         (reference "Copunterexample")
	 (constants (A (o (o i))) (B (o (o i))))
         (conclusion conc (= (union A B) A)))
			     


(th~defproblem chris-test1
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP?")
	 (constants (a o) (b o))
	 (assumption ass1 a)
	 (assumption ass2 b)
         (conclusion conc (and a b)))

(th~defproblem chris-test2
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP after two times imp-i?")
	 (constants (a o) (b o))
         (conclusion conc (implies a (implies b (and a b)))))
	 
(th~defproblem chris-test3
	 (in typed-set)
         (author chris)
         (reference "Can this be solved by PL-ATP after two times imp-i?")
	 (constants (a o) (b o))
         (conclusion conc (and (implies a (implies b (and a b)))
			       (implies a (implies b (and a b))))))

(th~defproblem chris-top1
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

(th~defproblem chris-top2
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




(th~defproblem chris-top2a
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



(th~defproblem chris-top2b
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



(th~defproblem chris-top-manfred
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

(th~defproblem chris-top-manfred-a
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

(th~defproblem chris-leo-tps-test1
	 (in typed-set)
         (author chris)
         (reference "Test")
	 (constants (A i) (B i) (Top (o i)))
	 (assumption ass1 (top A))
         (conclusion conc (forall (lam (x i) (top x)))))

(th~defproblem chris-leo-tps-test2
	 (in base)
         (author chris)
         (reference "Test")
	 (constants (q1 (o i)) (q2 (o i)) (q3 (o i)) (q4 (o i)) (q5 (o i)) (q6 (o i)) (q7 (o i)) (q8 (o i)) (q9 (o i)) (q10 (o i)) (q11 (o i)) (q12 (o i)))
         (conclusion conc (= (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q1 x) (q2 x)) (q3 x)) (q4 x)) (q5 x)) (q6 x)) (q7 x)) (q8 x)) (q9 x)) (q10 x)) (q11 x)) (q12 x)))
                             (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q12 x) (q11 x)) (q10 x)) (q9 x)) (q8 x)) (q7 x)) (q6 x)) (q5 x)) (q4 x)) (q3 x)) (q2 x)) (q1 x))))))

(th~defproblem chris-leo-tps-test3
	 (in base)
         (author chris)
         (reference "Test")
	 (constants (q1 (o i)) (q2 (o i)) (q3 (o i)) (q4 (o i)) (q5 (o i)) (q6 (o i)) (q7 (o i)) (q8 (o i)) (q9 (o i)) (q10 (o i)) (q11 (o i)) (q12 (o i)))
         (conclusion conc (not (= (lam (x i) 
                                      (or (and (and (and (and (and (and (and (and (and (and (q1 x) (q2 x)) (q3 x)) (q4 x)) (q5 x)) (q6 x)) (q7 x)) (q8 x)) (q9 x)) (q10 x)) (q11 x)) (q12 x)))
                             (lam (x i) 
                                      (and (and (and (and (and (and (and (and (and (and (and (q12 x) (q11 x)) (q10 x)) (q9 x)) (q8 x)) (q7 x)) (q6 x)) (q5 x)) (q4 x)) (q3 x)) (q2 x)) (q1 x)))))))

(th~defproblem chris-leo-tps-test4
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

(th~defproblem chris-cas-test1a
	 (in natural)
         (author chris)
         (reference "Test")
	 (constants (nice-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption dummy (nice-numbers (union a b)))
	 (assumption ass1 (nice-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (nice-numbers (intersection
					 (lam (x num) (less x (plus 20 20)))
					 (lam (x num) (greater x (times 5 (plus 2 3))))))))

(th~defproblem chris-cas-test1b
	 (in natural)
         (author chris)
         (reference "Test")
	 (constants (favourite-numbers (o (o num))) (a (o num)) (b (o num)))
	 (assumption ass1 (favourite-numbers (lam (x num) (and (greater x (power 5 2)) (less x (times 2 20))))))
	 (conclusion conc (favourite-numbers (lam (x num) (and (less x (plus 20 20))
					                       (greater x (times 5 (plus 2 3))))))))		



(th~defproblem chris-cas-test1c
	 (in integer)
         (author chris)
         (reference "Test")
	 (conclusion conc (= (lam (x num) (and (greater x (gcd 10 8)) (less x (lcm 10
										   8))))
			     (intersection (lam (x num) (less x 40)) (lam (x num) (greater
										   x 2))))))


(th~defproblem chris-cas-test2
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


(th~defproblem chris-pl-test1
	 (in typed-set)
         (author chris)
         (reference "Test")
	 (constants (A i) (B i) (Top (o i)))
         (conclusion conc (implies (top a) (or (top a) (top b)))))


(th~defproblem chris-tpstrans-test1
	 (in natural)
         (author chris)
         (reference "Test")
	 (constants (Top (o num)))
         (conclusion conc (implies (top 20) (top (plus 10 10)))))


(th~defproblem chris-set-dist-counter
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (intersection (union  X Z) (union Y Z)))))))))))


(th~defproblem chris-set-dist
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (2.2 distributivity of intersection
and union)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
	                    (forall (lam (Z (o i))
			    (= (intersection (union X Y) Z)
                               (union (intersection X Z) (intersection Y Z)))))))))))


(th~defproblem hakim1
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
         (conclusion conc (forall (lam (X (o i))
			   (forall (lam (Y (o i)) 
			    (implies (and (subset X Y) (subset Y X)) (= X Y))))))))


(th~defproblem hakim2
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.1)")
         (conclusion conc (forall (lam (X (o))
			   (forall (lam (Y (o))
			     (forall (lam (Z (o)) 	
					(implies X (implies Z (implies Y (and X (and Y
										     Z)))))))))))))

(th~defproblem test-buf
	 (in typed-set)
         (author chris)
         (reference "Ebbinghaus.Einf.in.die.Mengenlehre (1.5 commutativity of
intersection)")
	 (constants (a o))
         (conclusion conc (and (forall (lam (X (o i))
					    (forall (lam (Y (o i)) 
							 (= (intersection X Y)
							    (intersection Y X))))))
			       (equiv a a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; note by chris: modified files: this one, nic-pl.thy (hashtable),  omega-com.lisp
;;;; (oc=use-nic-agents)

(load "~/omega/keim-3/prog/suggestion/thy/nic-pl.thy")
(load "~/omega/keim-3/prog/suggestion/thy/nic-fo.thy")
(load "~/omega/keim-3/prog/suggestion/thy/demo-extern.thy")
(oc=use-nic-agents)
(oc=stop-use-resources)
(set-auto-default-interval 10)
; (oc=set-agent-computation)






