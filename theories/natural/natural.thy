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

(th~deftheory NATURAL
	      (uses poset function struct)
	      (help "Peano Arithmetic for naturals."))

(th~deftype num
 (in natural)
 (arguments 0)
 (help "The type of number objects, like natural nubmers, rationals, reals, complex,..."))

(th~defconstant zero
 (in natural)
 (type num)
 (help "The zero of natural numbers"))


(th~defconstant s
 (in natural)
 (type (num num))
 (help "The successor function of the natural numbers"))

(th~defconstant Nat
 (in natural)
 (type (o num))
 (sort)
 (help "The set of natural numbers"))

(th~defconstant Even
 (in natural)
 (type (o num))
 (sort)
 (help "The set of even natural numbers"))

(th~defconstant pos-Nat
 (in natural)
 (type (o num))
 (sort)
 (help "The set of natural numbers"))

(th~defconstant NNat
 (in natural)
 (type (o num))
 (sort)
 (help "The set of negative natural numbers"))

(th~defconstant neg-NNat
 (in natural)
 (type (o num))
 (sort)
 (help "The set of negative natural numbers"))


(th~defconstant iterate-warg
 (in natural)
 (type (all-types bb (bb bb num (bb bb num))))
 (help "An interatiton Combinator for the natural numbers"))

(th~defconstant change-sign
 (in natural)
 (type (num num))
 (help "The unary minus operator of Integers"))

(th~defconstant nat-plus-struct
 (in natural)
 (type (struct num))
 (help "The structure of the natural numbers with plus as an operation."))

(th~defconstant nat-times-struct
 (in natural)
 (type (struct num))
 (help "The structure of natural numbers with times as an operation"))

;(th~defdef nat-iterate
;           (in natural)
;           (definition
;             (lam (n num)
;                  ((that (lam (P (o num))
;                    (and (and (and (and (P zero)
;                                        (forall-sort (lam (x num) (P (s X))) P ))
;                                   (injective P s))
;                              (forall-sort (lam (x num) (not (= zero (s x)))) P))
;                         (forall (lam (Q (o num))
;                          (implies (and (Q zero)
;                                        (forall-sort (lam (n num)
;                                                          (implies (Q n)
;                                                                   (Q (s n))))
;                                                     P))
;                                   (forall-sort (lam (n num) (Q n))
;                                                P)))))))
;                   n))))
;
;
; Peano's axioms for natural numbers
;


(th~defaxiom total-nat         ;;; from: (sort nat)
	     (in natural)
	     (formula (forall-sort (lam (x num) (defined (nat x))) defined))
	     (termdecl)
	     (help "The predicate Nat is defined everywhere."))

(th~deftheorem subsort-nat-defined         ;;;from sort-defined
	     (in natural)
	     (conclusion (forall-sort (lam (x num) (defined x)) nat))
	     (termdecl)
	     (help "The predicate Nat is defined everywhere."))


(th~defaxiom zero-nat
	     (in natural)
	     (formula
	      (nat zero))
	     (termdecl)
	     (help "Zero is a natural number."))

(th~defaxiom succ-nat
	     (in natural)
	     (formula
	      (forall-sort 
	       (lam (x num)
		    (nat (s x)))
	       nat))
	     (termdecl)
	     (help "The successor of a natural number is natural."))

(th~defaxiom nat-inj-succ
   	     (in natural)
	     (formula (injective nat s))
	     (help "The successor function is injective."))

(th~defaxiom nat-no-pred-zero
	     (in natural)
	     (formula (forall-sort
		       (lam (X num)
			    (not (= (s X) zero)))
		        Nat))
	     (help "Zero has no predecessor."))

(th~defaxiom nat-induction
	     (in natural)
	     (formula
	      (forall (lam (Q (o num))
                           (implies (and (subset Q Nat)   
			                 (and (in zero Q)
					      (closed-under-1 Q s)))
				    (= Nat Q)))))
	     (help "The induction axiom for natural numbers."))	     

(th~defaxiom nat-induct
	     (in natural)
	     (formula
	      (forall (lam (Q (o num))
                           (implies (and (Q zero)
					 (forall-sort (lam (n num)
					  (implies (Q n)
						   (Q (s n))))
					   Nat))
				    (forall-sort (lam (n num) (Q n))
						  Nat)))))
	     (help "The induction axiom."))	     

;
; Miscellaneous 
;

(th~defdef one
	   (in natural)
           (definition (s zero))
	   (help "The number 1 defined as the successor of 0."))

#-struct-3(th~defsimplifier one-simp
		  (in natural)
		  (status global)
                  (equation one)
                  (direction lr)
                  (help "Simplify the number one."))


(th~defdef two
	   (in natural)
           (definition (s one))
	   (help "The number 2 defined as the successor of 1."))

(th~defdef three
	   (in natural)
           (definition (s two))
	   (help "The number 3 defined as the successor of 2."))
(th~defdef four
	   (in natural)
           (definition (s three))
	   (help "The number 4 defined as the successor of 3."))
(th~defdef five
	   (in natural)
           (definition (s four))
	   (help "The number 5 defined as the successor of 4."))
(th~defdef six
	   (in natural)
           (definition (s five))
	   (help "The number 6 defined as the successor of 5."))
(th~defdef seven
	   (in natural)
           (definition (s six))
	   (help "The number 7 defined as the successor of 6."))
(th~defdef eight
	   (in natural)
           (definition (s seven))
	   (help "The number 8 defined as the successor of 7."))
(th~defdef nine
	   (in natural)
           (definition (s eight))
	   (help "The number 9 defined as the successor of 8."))
(th~defdef ten
	   (in natural)
           (definition (s nine))
	   (help "The number 10 defined as the successor of 9."))


(th~defdef p
           (in natural)
           (definition
              (lam (n num) (that (lam (m num) (= (s m) n)))))
           (help "Predecessor function."))

;
; Definitions of the order relations
;

(th~defdef leq
	   (in natural)
	   (definition
	      (lam (m num)
		   (lam (n num)
		   (forall (lam (Q (o num))
				(implies
				  (and (in m Q)
				       (forall (lam (l num)
					  (implies (in l Q)
						   (in (s l) Q)))))
				  (in n Q)))))))
      	   (help "The classical less-or-equal operator on the natural numbers."))				      

(th~defdef less
          (in natural)
           (definition
             (lam (x num)
                  (lam (y num)
                       (and (leq x y)
                            (not (= x y))))))
           (help "The less predicate."))

(th~defdef greater
           (in natural)
           (definition
             (lam (x num)
                  (lam (y num)
                       (less  y x))))
           (help "The greater predicate."))

(th~defdef geq
           (in natural)
           (definition
             (lam (x num)
                 (lam (y num)
		      (leq y x))))
           (help "The greater-or-equal predicate."))

;
; Arithmetic operations defined through the recursion operator
;

(th~defdef recursion-poly 
           (in natural)
	   (type-variables cc)
           (definition
                (lam (h (cc cc num))
                 (lam (g cc)
                  (lam (n num)
                   (that (lam (m cc)
                    (forall (lam (U (o cc num))
                         (implies
                          (and (U zero g)
                               (forall (lam (y cc)
                                 (forall (lam (x num)
                                            (implies (U x y)
                                                     (U (s x) (h x y))))))))
                          (U n m))))))))))
          (help "A polymorphic version of the recursion operator."))

(th~defdef recursion 
           (in natural)
           (definition
                (lam (h ((num num) num))
                 (lam (g num)
                  (lam (n num)
                   (that (lam (m num)
                    (forall (lam (U (o num num))
                         (implies
                          (and (U zero g)
                               (forall (lam (y num)
                                 (forall (lam (x num)
                                            (implies (U x y)
                                                     (U (s x) (h x y))))))))
                          (U n m))))))))))
          (help "The recursion operator."))

(th~defdef plus
           (in natural)
           (definition
	     (recursion (lam (x num) s)))
           (help "Addition defined as iterated application of successor."))

(th~defdef times
           (in natural)
           (definition
	     (lam (m num)
		  (recursion (lam (x num) (plus m)) zero)))
           (help "Multiplication defined as iterated addition."))

(th~defdef power
           (in natural)
           (definition
	     (lam (m num)
		  (recursion (lam (x num) (times m)) one)))
           (help "Exponentiation defined as iterated multiplication."))

(th~defdef iterate
           (in natural)
           (type-variables bb)
           (definition 
             (lam (F (bb bb))
                  (lam (n num)
                       (recursion-poly (lam (n num) (compose-functions F)) (lam (x bb) x) n)))) 
	   (help "The iteration operator."))


;
; MiKo's Defis
;

(th~defdef pos-nat
           (in natural)
           (definition 
             (lam (x num) (and (in x nat) (not (= x zero)))))
           (help "The set of positive natural numbers."))


(th~deftheorem total-pos-nat
	     (in natural)
	     (conclusion conc (forall (lam (x num) (defined (pos-nat x)))))
	     (help "The predicate Nat is defined everywhere."))


(th~defdef NNat
          (in natural)
           (definition 
             (lam (x num) (or (= x zero) (exists-sort (lam (y num) (= (s x) y))  NNat))))
           (help "The set of positive natural numbers."))

(th~deftheorem total-nnat
	     (in natural)
	     (conclusion conc (forall (lam (x num) (defined (nnat x)))))
	     (help "The predicate Nat is defined everywhere."))

(th~defaxiom zero-nnat
             (in natural)
             (formula
	      (nnat zero))
             (termdecl)
             (help "Zero is a negative natural number."))

(th~defaxiom pred-nnat
             (in natural)
             (formula
	      (forall-sort 
	       (lam (x num)
		    (nnat (p x)))
	       nnat))
             (termdecl)
	     (help "The predecessor of a negative natural number is a negative natural number."))

(th~defaxiom nnat-closed
             (in natural)
             (formula (closed-under-1 nnat p))
             (help "The set of neg-natural numbers is closed under successors."))


(th~defaxiom nnat-inj-pred
             (in natural)
             (formula (injective nnat p))
             (help "The successor function is injective."))

(th~defaxiom nnat-no-suc-zero
             (in natural)
             (formula (forall-sort 
		       (lam (X num)
			    (not (= x (s zero))))
		       Nnat))
             (help "Zero has no successor in NNat."))

(th~defaxiom nnat-induction
             (in natural)
             (formula (forall (lam (Q (o num))
                                   (implies (and (in zero Q)
                                                 (closed-under-1 Q p))
                                            (subset nnat Q)))))
             (help "The induction axiom for neg-natural numbers."))          


(th~defdef neg-NNat
           (in natural)
           (definition (lam (x num) (and (in x NNat) (not (= x zero)))))
           (help "The set of negative Nnats."))


(th~defaxiom change-sign
             (in natural)
             (formula
              (and (= (change-sign zero) zero)
                   (and (forall-sort 
                         (lam (x num)
                          (= (change-sign (s x)) (p (change-sign x))))
			  Nat)
                        (forall-sort 
                         (lam (x num)
                              (= (change-sign (p x)) (s (change-sign x))))
			  NNat))))
             (help "The negative operator on Natural numbers."))

;;#{Ordering properties of the natural numbers}#

(th~defaxiom leq-nat
             (in natural)
             (formula
              (and (and (forall-sort (lam (y num) (leq zero y)) Nat)
                        (forall-sort
                         (lam (x num)
                              (implies (in x pos-nat) (not (leq x zero))))
			  Nat ))
                   (forall-sort 
                    (lam (x num)
                     (forall-sort 
                      (lam (y num)
                           (implies (leq (s x) (s y))
                                    (leq x y)))
		      pos-nat))
		    pos-nat)))
             (help "The classical less-or-equal operator on the natural numbers."))


(th~defdef first-n-nats
           (in natural)
           (definition
             (lam (x num)
                  (lam (y num)
                       (less y x))))
           (help "The set of the first n natural numbers, i.e. the set {0,...,n-1}."))

                          
(th~defdef cardinality
           (in natural)
           (type-variables bb)
           (definition
             (lam (G (o bb))
                  (that (lam (x num)
                             (and (in x Nat)
                                  (exists (lam (F (num bb))
                                               (bijective G (first-n-nats x) F))))))))
           (help "Definition of the finite cardinalities of sets."))

(th~defdef finite-cardinality
           (in natural)
           (type-variables bb)
           (definition
             (lam (G (o bb))
                  (exists-sort
                   (lam (x num)
                        (exists (lam (F (num bb))
                                     (bijective G (first-n-nats x) F))))
		    Nat)))
           (help "Definition of the finite cardinalities of sets."))


(th~defdef finite
           (in natural)
           (type-variables bb)
           (definition
             (lam (G (o bb))
                  (exists-sort  (lam (n num)
                   (= n (cardinality G)))
			Nat	)))
           (help "Predicate for finiteness of sets."))
                          

(th~defdef finite-subset
           (in natural)
           (type-variables bb)
           (definition
             (lam (G (o bb))
                  (lam (H (o bb))
                       (and (subset G H)
                            (finite G)))))
           (help "Predicates for finite subsets."))


(th~defaxiom nat-plus-struct
             (in natural)
             (formula
              (and (and (= (struct-set nat-plus-struct) Nat)
                        (= (struct-op nat-plus-struct) plus))
                   (= (struct-ordering nat-plus-struct) leq)))
             (help "The structure of the natural numbers with plus."))


(th~defaxiom nat-times-struct
           (in natural)
           (formula
            (and (= (struct-set nat-times-struct) Nat)
                 (= (struct-op nat-times-struct) times)))
           (help "The structure of the natural numbers with times."))


(th~defdef absval
           (in natural)
           (definition
             (lam (x num) (ifthen (less x zero) (change-sign x) x)))
           (help "The absolute value on numbers."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Axioms for first-n-nats necessary to derive explicit sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defaxiom first-n-nats-base
	     (in natural)
	     (formula
	      (= (first-n-nats (s 0))
		 (lam (x num) (= x 0))))
	     (help ""))
	      
(th~defaxiom first-n-nats-step
	     (in natural)
	     (formula
	      (forall-sort (lam (n num)
				(= (first-n-nats (s n))
				   (lam (x num) (or (= x n) (in x (first-n-nats n))))))
			   nat))
	     (help ""))






