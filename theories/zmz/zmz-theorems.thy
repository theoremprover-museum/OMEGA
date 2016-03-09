;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theorems for Residue Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~deftheorem resclass-closed-constant
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
				 (forall (lam (rs (o (o num)))
					      (forall-sort (lam (c (o num))
								(implies (subset rs (resclass-set n))
									 (closed-under rs
										       (lam (x (o num))
											    (lam (y (o num))
												 c)))))
							   rs))))
			    int))
	       (help "A residue class is closed under a constant operation, provided the constant is in the residue class itself."))

(th~deftheorem resclass-closed-left-var
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
				 (forall (lam (rs (o (o num)))
					      (implies (subset rs (resclass-set n))
						       (closed-under rs (lam (x (o num))
									     (lam (y (o num))
										  x)))))))
			    int))
	       (help "A residue class is closed under the identity operation for the left operand."))
					      
(th~deftheorem resclass-closed-right-var
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
				 (forall (lam (rs (o (o num)))
					      (implies (subset rs (resclass-set n))
						       (closed-under rs
								     (lam (x (o num))
									  (lam (y (o num))
									       y)))))))
			    int))
	       (help "A residue class is closed under the identity operation for the right operand."))

(th~deftheorem resclass-closed-composition-plus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall (lam (a1 ((o num) (o num) (o num)))
					       (forall (lam (a2 ((o num) (o num) (o num)))
							    (implies (and (closed-under (resclass-set n) a1)
									  (closed-under (resclass-set n) a2))
								     (closed-under (resclass-set n)
										   (lam (x (o num)) (lam (y (o num))
													 (plus-resclass (a1 x y) (a2 x y)))))))))))
			     int))
	       (help "A residue class is closed under the composition of plus operation."))

(th~deftheorem resclass-closed-composition-times
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall (lam (a1 ((o num) (o num) (o num)))
					       (forall (lam (a2 ((o num) (o num) (o num)))
							    (implies (and (closed-under (resclass-set n) a1)
									  (closed-under (resclass-set n) a2))
								     (closed-under (resclass-set n)
										   (lam (x (o num)) (lam (y (o num))
													 (times-resclass (a1 x y) (a2 x y)))))))))))
			     int))
	       (help "A residue class is closed under the composition of times operation."))

(th~deftheorem resclass-closed-composition-minus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall (lam (a1 ((o num) (o num) (o num)))
					       (forall (lam (a2 ((o num) (o num) (o num)))
							    (implies (and (closed-under (resclass-set n) a1)
									  (closed-under (resclass-set n) a2))
								     (closed-under (resclass-set n)
										   (lam (x (o num)) (lam (y (o num))
													 (minus-resclass (a1 x y) (a2 x y)))))))))))
			     int))
	       (help "A residue class is closed under the composition of minus operation."))


(th~deftheorem associativity-plus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				 (forall (lam (rs (o (o num)))
					      (implies (subset rs (resclass-set n))
						       (associative rs plus-resclass)))))
			     int))	       
	       (help "Each residue class set is associative with respect to plus-resclass."))


(th~deftheorem associativity-times
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				 (forall (lam (rs (o (o num)))
					      (implies (subset rs (resclass-set n))
						       (associative rs times-resclass)))))
			     int))	       
	       (help "Each residue class set is associative with respect to times-resclass."))

(th~deftheorem unit-exists-plus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall (lam (rs (o (o num)))		
					       (implies (and (subset rs (resclass-set n))
							     (rs (resclass n 0)))
							(exists-sort (lam (e (o num))
									  (unit rs plus-resclass e))
								     rs)))))
			     int))	       	
	       (help "A residue class set containing the 0 element has an unit, namely 0, with respect to plus-resclass."))


(th~deftheorem unit-exists-times
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall (lam (rs (o (o num)))		
					       (implies (and (subset rs (resclass-set n))
							     (rs (resclass n 1)))
							(exists-sort (lam (e (o num))
									  (unit rs times-resclass e))
								     rs)))))
			     int))	       	
	       (help "A residue class set containing the 1 element has an unit, namely 1, with respect to times-resclass."))

(th~deftheorem inverse-exist-plus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (inverse-exist (resclass-set n) plus-resclass (resclass n 0)))
			     int))	       	
	       (help "In a complete residue class set inverses exists with respect to plus-resclass and the 0 element as unit."))

(th~deftheorem divisors-exist-plus
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (divisors-exist (resclass-set n) plus-resclass))
			     int))	       	
	       (help "In a complete residue class set divisors exists with respect to plus-resclass."))

(th~deftheorem divisors-exist-times
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (divisors-exist (resclass-set n) times-resclass))
			     int))	       	
	       (help "In a complete residue class set divisors exists with respect to times-resclass."))

(th~deftheorem not-isomorphic-when-different-cardinality
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n1 num)
			(forall-sort (lam (n2 num)
				(forall (lam (rset1 (o (o num)))
					(forall (lam (rset2 (o (o num)))
					        (forall (lam (op1 ((o num) (o num) (o num)))
							(forall (lam (op2 ((o num) (o num) (o num)))
								(implies (and (not (= (order rset1)
										      (order rset2)))
									 (and (subset rset1 (resclass-set n1))
									      (subset rset2 (resclass-set n2))))
									 (not (isomorphic rset1 op1 rset2 op2))))))))))))
				     int))
			     int)))
				
								      
(th~deftheorem cart-prod-assoc-when-items-assoc
	       (in zmz)

	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
					(forall (lam (set2 (o bb))
						     (forall (lam (op1 (aa aa aa))
								  (forall (lam (op2 (bb bb bb))
									       (equiv (and (associative set1 op1)
											   (associative set2 op2))
										      (associative (CARTESIAN-PRODUCT set1 set2)
												   (PAIR-OPERATION op1 op2))))))))))))))


(th~deftheorem cart-prod-closed-when-items-closed
	       (in zmz)
	       
	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
					(forall (lam (set2 (o bb))
						     (forall (lam (op1 (aa aa aa))
								  (forall (lam (op2 (bb bb bb))
									       (equiv (and (closed-under set1 op1)
											     (closed-under set2 op2))
											(closed-under (CARTESIAN-PRODUCT set1 set2)
												      (PAIR-OPERATION op1 op2))))))))))))))


(th~deftheorem cart-prod-invex-when-items-invex
	       (in zmz)

	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
				   (forall (lam (set2 (o bb))
					   (forall (lam (op1 (aa aa aa))
						   (forall (lam (op2 (bb bb bb))
							   (forall (lam (z (tuple aa bb))
									(equiv (and (inverse-exist set1 op1 (first-of-pair z))
										    (inverse-exist set2 op2 (second-of-pair z)))
									       (inverse-exist (CARTESIAN-PRODUCT set1 set2)
											      (PAIR-OPERATION op1 op2)
											      z)))))))))))))))


(th~deftheorem cart-prod-divex-when-items-divex
	       (in zmz)

	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
					(forall (lam (set2 (o bb))
						     (forall (lam (op1 (aa aa aa))
								  (forall (lam (op2 (bb bb bb))
									       (equiv (and (divisors-exist set1 op1)
											   (divisors-exist set2 op2))
										      (divisors-exist (CARTESIAN-PRODUCT set1 set2)
												      (PAIR-OPERATION op1 op2))
										      ))))))))))))


(th~deftheorem cart-prod-unit-when-items-unit
	       (in zmz)
	       
	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
				   (forall (lam (set2 (o bb))
					   (forall (lam (op1 (aa aa aa))
						   (forall (lam (op2 (bb bb bb))
							   (equiv (and (exists-sort (lam (e1 aa)
											 (unit set1 op1 e1))
										    set1)
								       (exists-sort (lam (e2 bb)
											 (unit set2 op2 e2))
										    set2))
								  (exists-sort (lam (e (tuple aa bb))
										    (unit (CARTESIAN-PRODUCT set1 set2)
											  (PAIR-OPERATION op1 op2)
											  e))
									       (cartesian-product set1 set2))))))))))))))

(th~deftheorem not-isomorphic-order-of-element1
	       (in zmz)
	       (conclusion
		(all-types aa
		  (forall (lam (S1 (o aa))
		  (forall (lam (op1 (aa aa aa))
		  (forall (lam (S2 (o aa))
		  (forall (lam (op2 (aa aa aa))
		     (implies
		      (exists-sort (lam (n num)
					(and
					 (exists-sort (lam (elem1 aa)
							   (order-of-element S1 op1 elem1 n))
						      S1)
					 (not (exists-sort (lam (elem2 aa)
								(order-of-element S2 op2 elem2 n))
							   S2))))
				   (integer-intervall 1 (order S1)))
		      (Not (isomorphic S1 op1 S2 op2))))))))))))))

(th~deftheorem not-isomorphic-order-of-element2
	       (in zmz)
	       (conclusion
		(all-types aa
		  (forall (lam (S1 (o aa))
		  (forall (lam (op1 (aa aa aa))
		  (forall (lam (S2 (o aa))
		  (forall (lam (op2 (aa aa aa))
		     (implies
		      (exists-sort (lam (n num)
					(and
					 (exists-sort (lam (elem2 aa)
							   (order-of-element S2 op2 elem2 n))
						      S2)
					 (not (exists-sort (lam (elem1 aa)
								(order-of-element S1 op1 elem1 n))
							   S1))))
				   (integer-intervall 1 (order S2)))
		      (Not (isomorphic S1 op1 S2 op2))))))))))))))

(th~deftheorem not-isomorphic-trace-of-element
	       (in zmz)
	       (conclusion
		(all-types aa
		  (forall (lam (S1 (o aa))
		  (forall (lam (op1 (aa aa aa))
		  (forall (lam (S2 (o aa))
		  (forall (lam (op2 (aa aa aa))
		     (implies  (exists-sort (lam (elem1 aa)
				  (forall-sort (lam (elem2 aa)
						       (not (= (order (trace-of-element op1 elem1))
							       (order (trace-of-element op2 elem2)))))
					       S2))
					    S1)
			      (Not (isomorphic S1 op1 S2 op2))))))))))))))


(th~deftheorem bij-by-surj-sameorder
	       (in zmz)
	       
	       (conclusion
		(all-types aa bb
			   (forall (lam (set1 (o aa))
			   (forall (lam (set2 (o bb))
			   (forall (lam (fun (aa bb))
					(implies (and (exists-sort (lam (n num) (= n (order set1))) nat)
						      (and (= (order set1)(order set2))
							   (surjective set2 set1 fun)))
						 (bijective set2 set1 fun)))))))))))

	
;;(th~deftheorem iso-by-same
;;	       (in zmz)
;;	       (conclusion
;;		(all-types aa bb
;;			   (forall (lam (set1 (o aa))
;;			   (forall (lam (set2 (o aa))
;;			   (forall (lam (op1 (aa aa aa))
;;			   (forall (lam (op2 (aa aa aa))		
;;					(implies (and (= set1 set2)
;;						      (forall-sort (lam (x aa)
;;						      (forall-sort (lam (y aa)
;;						       (= (op1 x y)(op2 x y)))
;;								   set1 ))
;;								   set1))
;;						 (isomorphic set1 op1 set2 op2)))))))))))))
;;
;;										      
;;(th~deftheorem ISOMORPHIC-IF-TWOHOMS-DIRONE
;;	       (in zmz) 
;;	       (category THEOREM) 
;;	       
;;	       (conclusion (forall (lam (set1 (o (o num)))
;;			    (forall (lam (op1 ((o num) (o num) (o num)))
;;			     (forall (lam (set2 (o (o num)))
;;			      (forall (lam (op2 ((o num) (o num) (o num)))
;;					   (implies (isomorphic set1 op1 set2 op2)
;;						    (exists (lam (h ((o num) (o num)))
;;								 (exists (lam (j ((o num) (o num)))
;;									      (and (and (and (forall-sort (lam (x (o num))
;;													       (set2 (h x)))
;;													  set1)
;;											     (forall-sort (lam (x (o num))
;;													       (set1 (j x)))
;;													  set2))
;;											(and (homomorphism set1 op1 set2 op2 h)
;;											     (homomorphism set2 op2 set1 op1 j)))
;;										   (and (forall-sort (lam (x (o num))
;;													  (= (j (h x)) x))
;;												     set1)
;;											(forall-sort (lam (x (o num))
;;													  (= (h (j x)) x))
;;												     set2))))))))))))))))))
;; this is a theorem stating the one direction of the theorem ISOMORPHIC-IF-TWOHOMS (in the morphisms theory),
;; this weaker version is needed to prove things with OTTER





