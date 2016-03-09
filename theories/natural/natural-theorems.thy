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


;
; TODO:
;

;(th~deftheorem times-nat-base
;               (in natural)
;               (conclusion conc
;                (forall-sort
;                 (lam (x num)
;                      (= (times x zero) zero))
;                 nat))
;               (help "Base case for the recursive definition of multiplication."))
;
;(th~deftheorem times-nat-step
;               (in natural)
;               (conclusion conc
;                (forall-sort
;                 (lam (x num)
;                      (forall-sort
;                       (lam (y num)
;                            (= (times (s x) y) (plus x (times x y))))
;                       nat))
;                 nat))
;               (help "Step case for the recursive definition of multiplication."))

(th~deftheorem TIMES-NAT-CLOSED 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (NAT (TIMES Y X)))  NAT))  NAT))
(termdecl)
(help "The set of natural numbers is closed under plus."))

;(th~deftheorem power-nat-base
;               (in natural)
;               (conclusion conc
;                (forall-sort
;                 (lam (x num)
;                      (= (power x zero) one))
;                 nat))
;               (help "Base case for the recursive definition of exponentiation."))
;
;(th~deftheorem power-nat-step
;               (in natural)
;               (conclusion
;                (forall-sort
;                 (lam (x num)
;                      (forall-sort
;                       (lam (y num)
;                            (= (times (s x) y) (times x (power x y))))
;                       nat))
;                 nat))
;               (help "Step case for the recursive definition of exponentiation."))

(th~deftheorem POWER-NAT-CLOSED 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (NAT (POWER Y X)))  NAT))  NAT))
(termdecl)
(help "The set of natural numbers is closed under plus."))


(th~deftheorem nat-closed-s
               (in natural)
               (conclusion (closed-under-1 nat s))
               (help "The set of natural numbers is closed under successors."))

(th~deftheorem nat-plus-struct-set
               (in natural)
               (conclusion
                (= (struct-set nat-plus-struct) Nat))
               (help "The set of the nat-plus-struct is nat."))

(th~defsimplifier nat-plus-struct-set
                  (in natural)
                  (status global)
                  (equation nat-plus-struct-set)
                  (direction lr)
                  (help "Simplify the set of nat-plus-struct."))

(th~deftheorem nat-plus-struct-op
               (in natural)
               (conclusion
                (= (struct-op nat-plus-struct) plus))
             (help "The operation of nat-plus-struct is plus."))

(th~defsimplifier nat-plus-struct-op
                  (in natural)
                  (status global)
                  (equation nat-plus-struct-op)
                  (direction lr)
                  (help "Simplify the operation of nat-plus-struct."))

(th~deftheorem nat-plus-struct-ord
             (in natural)
             (conclusion
              (= (struct-ordering nat-plus-struct) leq))
             (help "The ordering of nat-plus-struct is leq."))

(th~defsimplifier nat-plus-struct-ord
                  (in natural)
                  (status global)
                  (equation nat-plus-struct-ord)
                  (direction lr)
                  (help "Simplify the ordering of nat-plus-struct."))
 
(th~deftheorem nat-times-struct-set
               (in natural)
               (conclusion
                (= (struct-set nat-times-struct) Nat))
               (help "The set of the nat-times-struct is nat."))

(th~defsimplifier nat-times-struct-set
                  (in natural)
                  (status global)
                  (equation nat-times-struct-set)
                  (direction lr)
                  (help "Simplify the set of nat-times-struct."))

(th~deftheorem nat-times-struct-op
               (in natural)
               (conclusion
                (= (struct-op nat-times-struct) times))
             (help "The operation of nat-times-struct is times."))

(th~defsimplifier nat-times-struct-op
                  (in natural)
                  (status global)
                  (equation nat-times-struct-op)
                  (direction lr)
                  (help "Simplify the operation of nat-times-struct."))

(th~deftheorem nat-times-struct-ord
             (in natural)
             (conclusion
              (= (struct-ordering nat-times-struct) leq))
             (help "The ordering of nat-times-struct is leq."))

(th~defsimplifier nat-times-struct-ord
                  (in natural)
                  (status global)
                  (equation nat-times-struct-ord)
                  (direction lr)
                  (help "Simplify the ordering of nat-times-struct."))




(th~deftheorem recursion-exists
       (in natural)
       (conclusion 
	  (forall (lam (h (num num num))
               (forall (lam (g num)
                      (and (= (recursion h g zero) g)
                           (forall-sort (lam (n num)
                                 (= (recursion h g (s n)) (h n (recursion h g n))))
					 Nat)))))))
(help "Existence of the recursion operator."))

(th~deftheorem RECURSION-UNIQ 
(in NATURAL) 
(category THEOREM) 
(conclusion
(all-types cc
	   (FORALL (lam (H (CC CC NUM))
			(FORALL (lam (G CC)
				     (FORALL (lam (R1 (CC NUM CC (CC CC NUM)))
						  (FORALL (lam (R2 (CC NUM CC (CC CC NUM)))
							       (IMPLIES (AND (AND (= (R1 H G ZERO) G)
										  (FORALL-SORT (lam (N NUM) (= (R1 H G (S N)) (H N (R1 H G N))))  NAT))
									     (AND (= (R2 H G ZERO) G)
										  (FORALL-SORT (lam (N NUM) (= (R2 H G (S N)) (H N (R2 H G N))))  NAT)))
									(FORALL-SORT (lam (N NUM) (= (R1 H G N) (R2 H G N))) NAT))))))))))))
(help "Uniqueness of the recursion operator."))



(th~deftheorem LEQ-REFL 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL (lam (X NUM) (LEQ X X))))
(help "Reflexivity of less-equal."))


(th~deftheorem LEQ-TRANS 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL (lam (X NUM)
			 (FORALL (lam (Y NUM)
				      (FORALL (lam (Z NUM)
						   (IMPLIES (AND (LEQ X Y)
								 (LEQ Y Z))
							    (LEQ X Z)))))))))
(help "Transitivity of less-equal."))


(th~deftheorem LEQ-ZERO-X 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM) (LEQ ZERO X))  NAT))
(help "Zero is less-equal than any natural number."))



(th~deftheorem LEQ-X-SX 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL (lam (X NUM) (LEQ X (S X)))))
(help "x is less-equal then the successor of x."))




;(th~deftheorem PLUS-NAT-BASE 
;(in NATURAL) 
;(category THEOREM) 
;(conclusion (FORALL-SORT (lam (X NUM) (= (PLUS X ZERO) X)) NAT))
;(help "Base case of the recursive definition of plus."))
;
;(th~deftheorem PLUS-NAT-BASE2 
;(in NATURAL) 
;(category THEOREM) 
;(conclusion (FORALL-SORT (lam (X NUM) (= (PLUS ZERO x) X)) NAT))
;(help "Base case of the recursive definition of plus."))
;
;
;(th~deftheorem PLUS-NAT-STEP 
;(in NATURAL) 
;(category THEOREM) 
;(conclusion
; (FORALL-SORT (lam (X NUM)
;                       (FORALL-SORT (lam (Y NUM)
;                                             (= (PLUS X (S Y)) (S (PLUS X Y))))  NAT))  NAT))
;(help "Step case of the recursive definition of plus."))



(th~deftheorem C-PLUS-NAT-BASE 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM) (= (PLUS X ZERO) (PLUS ZERO X)))  NAT))
(help "Base case of the commutative property of plus for natural numbers."))




;(th~deftheorem PLUS-NAT-STEP2 
;(in NATURAL) 
;(category THEOREM) 
;(conclusion (FORALL-SORT (lam (X NUM)
;                  (FORALL-SORT (lam (Y NUM)
;                         (= (PLUS (S X) Y) (S (PLUS X Y))))  NAT))  NAT))
;(help "Another step case of the recursive definition of plus."))

(th~deftheorem C-PLUS-NAT 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM)
				  (FORALL-SORT (lam (Y NUM)
							(= (PLUS X Y) (PLUS Y X)))  NAT))  NAT))
(help "Commutative property of addtition."))

(th~deftheorem PLUS-NAT-CLOSED 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM)
		     (FORALL-SORT (lam (Y NUM)
				   (NAT (PLUS Y X)))  NAT))  NAT))
(termdecl)
(help "The set of natural numbers is closed under plus."))

(th~deftheorem A-PLUS-NAT 
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (X NUM)
		(FORALL-SORT (lam (Y NUM)
		    (FORALL-SORT (lam (Z NUM)
			     (= (PLUS Z (PLUS Y X)) (PLUS (PLUS Z Y) X)))  NAT))  NAT))  NAT))
(help "Associative property of addition."))


(th~deftheorem plus-nat-base
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (plus x zero) x))
		 nat))
	       (help "The base case for recursive definition of addition."))

(th~deftheorem plus-nat-base2
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (plus zero x) x))
		 nat))
	       (help "Another base case for recursive definition of addition."))

(th~deftheorem plus-nat-step
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (plus x (s y)) (s (plus x y))))
		       nat))
		 nat))
	       (help "The step case for recursive definition of addition."))

(th~deftheorem plus-nat-step2
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (y num)
		      (forall-sort
		       (lam (x num)
			    (= (plus (s x) y) (s (plus x y))))
		       nat))
		 nat))
	       (help "Another step case for recursive definition of addition."))

(th~deftheorem times-nat-base
               (in natural)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (= (times x zero) zero))
		 nat))
	       (help "The base case for recursive definition of multiplication."))

(th~deftheorem times-nat-base2
               (in natural)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (= (times zero x) zero))
		 nat))
	       (help "Another base case for recursive definition of multiplication."))

(th~deftheorem times-nat-step
               (in natural)
               (conclusion
                (forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (= (times x (s y)) (plus x (times x y))))
		       nat))
		 nat))
	       (help "The step case for recursive definition of multiplication."))

(th~deftheorem times-nat-step2
               (in natural)
               (conclusion
                (forall-sort
		 (lam (y num)
		      (forall-sort
		       (lam (x num)
			    (= (times (s x) y) (plus y (times x y))))
		       nat))
		 nat))
	       (help "Another step case for recursive definition of multiplication."))

(th~deftheorem power-nat-base
               (in natural)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (= (power x zero) (s zero)))
                 nat))
	       (help "The base case for recursive definition of exponentiation."))

(th~deftheorem power-nat-step
               (in natural)
               (conclusion
                (forall-sort
                 (lam (x num)
                      (forall-sort
                       (lam (y num)
                            (= (power x (s y)) (times x (power x y))))
                       nat))
                 nat))
	       (help "The step case for recursive definition of exponentiation."))

(th~deftheorem power-nat-base-zero
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (implies (less zero x)
			       (= (power zero x)
				  zero)))
		 nat))
	       (help "Powers with base zero."))

(th~deftheorem power-nat-base-one
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (= (power (s zero) x)
			 (s zero)))
		 nat))
	       (help "Powers with base one."))

(th~deftheorem less-nat-base
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (less zero (s x)))
		 nat))
	       (help "The base case for recursive definition of less."))

(th~deftheorem less-nat-step
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less x y) (less (s x) (s y))))
		       nat))
		 nat))
	       (help "The step case for recursive definition of less."))

(th~deftheorem less-implies-leq-nat
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (less x y) (leq x y)))
		       nat))
		 nat))
	       (help "Less implies less or equal."))

(th~deftheorem equal-implies-leq-nat
	       (in natural)
	       (conclusion
		(forall-sort
		 (lam (x num)
		      (forall-sort
		       (lam (y num)
			    (implies (= x y) (leq x y)))
		       nat))
		 nat))
	       (help "Equal implies less or equal."))



;;**************************************************
;;  some theorems for the practical use in combining lime-economy and the limit-theorems
;;; THIS IS A HACK!
;;**************************************************


(th~deftheorem A-PLUS-NUM
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALl (lam (X NUM)
			 (FORALL (lam (Y NUM)
				      (FORALL (lam (Z NUM)
						   (= (PLUS Z (PLUS Y X)) (PLUS (PLUS Z Y) X)))))))))
(help "Associative property of addition."))

(th~deftheorem C-PLUS-NUM
(in NATURAL) 
(category THEOREM) 
(conclusion (FORALl (lam (X NUM)
			 (FORALL (lam (Y NUM)
				      (= (PLUS X Y) (PLUS Y X)))))))
(help "Associative property of addition."))





;;;Something about even numbers

(th~deftheorem subsort-even-nat
(in NATURAL) 
(conclusion (forall-sort (lam (x num) (Nat x)) Even))
(termdecl)
(help "Even numbers are a subset of natural numbers."))

(th~deftheorem even-from-nat
(in NATURAL) 
(conclusion (forall-sort (lam (x num) (Even (plus x x))) Nat))
(termdecl)
(help "The sum of the same two natural numbers is even."))

(th~deftheorem even-plus-closed
(in NATURAL) 
(conclusion (forall-sort (lam (y num) (forall-sort (lam (x num) (Even (plus y x))) even)) even))
(termdecl)
(help "The sum of the same two natural numbers is even."))








