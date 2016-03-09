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

(th~deftheorem compose-identity-right
	       (in permutation)
	       (conclusion
		(forall-sort (lam (L (o cyc))
				  (= (perm-compose L identity-perm) L))
			     permutation))
	       (help "Application of indentity permutation right."))

(th~deftheorem compose-identity-left
	       (in permutation)
	       (conclusion
		(forall-sort (lam (L (o cyc))
			     (= (perm-compose identity-perm L) L))
			     permutation))
	       (help "Application of indentity permutation left."))

(th~deftheorem Singleton-cycle-is-identity
	       (in permutation)
	       (conclusion
		(forall (lam (a num)
			     (= (cons a cnil) identity-cyc))))
	       (help "All singleton cycles (1), (2),... are identity elements."))
 
(th~deftheorem Generated-set-closed-under-Composition
	       (in permutation)
	       (conclusion
		(forall (lam (PS (o (o cyc)))
			     (closed-under (generated-set PS) perm-compose))))
	       (help "The generated set of permutations is closed under composition of permutations."))

(th~deftheorem perm-times-inverse
	       (in permutation)
	       (conclusion
		(forall-sort (lam (p (o cyc))
			     (= (perm-compose p (perm-inverse p)) identity-perm))
			     permutation))
	       (help "p * p^-1 = i."))

(th~deftheorem perm-compose-exponents
	       (in permutation)
	       (conclusion
		(forall-sort (lam (p (o cyc))
			     (forall-sort (lam (n num)
					       (forall-sort (lam (m num)
								 (= (perm-exp p (plus n m))
								    (perm-compose (perm-exp p n)
										  (perm-exp p m))))
							    int))
					  int))
			     permutation))
	       (help "Composition of exponents of permutations: p^(n+m) = p^n + p^m."))
					       
								 
(th~deftheorem perm-disjoint-to-disjoint
	       (in permutation)
	       (conclusion
		(forall-sort (lam (p cyc)
				  (forall-sort (lam (q cyc)
						    (equiv (disjoint (cycle-set p) (cycle-set q))
							   (cycle-disjoint p q)))
					       cycle))
			     cycle))
	       (help "Relation between disjointness of permutations and the disjointness of their constituting sets."))

(th~deftheorem exponents-in-generated-set
	       (in permutation)
	       (conclusion
		(forall (lam (PS (o (o cyc)))
			     (forall-sort (lam (P (o cyc))
					       (forall-sort (lam (n num)
								 (in (perm-exp P n) (generated-set PS)))
							    int))
					  (generated-set PS)))))
	       (help "generated-set(PS) is closed under perm-exp."))
								 
(th~deftheorem inverses-in-generated-set
	       (in permutation)
	       (conclusion
		(forall (lam (PS (o (o cyc)))
			     (forall-sort (lam (P (o cyc))
					       (in (perm-inverse P) (generated-set PS)))
					  (generated-set PS)))))
	       (help "generated-set(PS) is closed under perm-inverse."))
								 

(th~deftheorem schreier-lemma
	       (in permutation)
	       (conclusion
		(forall (lam (PS (o (o cyc)))
	        (forall (lam  (elem num)
		(forall-sort (lam (t ((o cyc) num))
				  (= (stabiliser (generated-set PS) perm-apply elem)
				     (generated-set
				      (lam (schreierelem (o cyc))
							 (exists-sort (lam (pi (o cyc))
							 (exists-sort (lam (orb num)
									   (= schreierelem
									      (perm-compose
									       (perm-compose (t orb) pi)
									       (perm-inverse (perm-compose (t orb) pi)))))
								      (g-orbit (generated-set ps) perm-apply elem)))
								      PS)))))
			     (g-orbit-representation (generated-set PS) perm-apply elem)))))))
	       (help "The Schreier lemma gives the generating set of stab by the generating set of the group and a coset representation"))
		    

