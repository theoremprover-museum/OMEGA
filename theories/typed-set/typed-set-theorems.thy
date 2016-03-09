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

(th~deftheorem first-second-of-pair
           (in typed-set)
 (conclusion (all-types aa bb (forall  (lam (x aa)
					 (forall (lam (y bb)
						      (and
						       (= (first-of-pair (pair x y)) x)
						       (= (second-of-pair (pair y x)) x)))))))))
(th~deftheorem pair-equality
           (in typed-set)
 (conclusion (all-types aa bb
			(forall  (lam (x aa)
				      (forall (lam (y bb)
						   (forall (lam (a aa)
								(forall (lam (b bb)
						      (equiv
						       (= (pair x y)(pair a b))
						       (and (= a x)(= y b))))))))))))))

(th~deftheorem chris-test-1
           (in base)
 (conclusion (forall  (lam (x o)
	(or (= x (or x (not x)))
            (= x (and x (not x))))))))
;;;;;;;;;

(th~deftheorem commutativity-of-union
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (Y (o bb))
			     (= (union X Y) (union Y X))))))))
	       (help "Commutativity of union."))


(th~deftheorem commutativity-of-intersection
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (Y (o bb))
			     (= (intersection X Y) (intersection Y X))))))))
	       (help "Commutativity of intersection."))

(th~deftheorem reflexive-subsetp
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb))
			     (subsetp X X)))))
	       (help "Each set is a subset of itself."))

(th~deftheorem subsetp-of-union
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (A (o bb)) (forall (lam (B (o bb))
				(implies (or (subsetp X A)
					     (subsetp X B))
					 (subsetp X (union A B)))))))))))
	       (help "Subset of union of sets."))

(th~deftheorem subsetp-of-intersection
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (A (o bb)) (forall (lam (B (o bb))
				(implies (and (subsetp X A)
					      (subsetp X B))
					 (subsetp X (intersection A B)))))))))))
	       (help "Subset of intersection of sets."))

(th~deftheorem subsetp-of-union-simple
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (A (o bb)) 
					 (subsetp X (union A X))))))))
	       (help "Subset of union of sets, special case."))

(th~deftheorem union-is-subsetp
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types a (forall (lam (X (o a))
				        (forall (lam (Y (o a))
					(forall (lam (Z (o a))
						     (implies (and (subsetp X Z)
								   (subsetp Y Z))
							      (subsetp (union X Y) Z))))))))))
	       (help "Union of X Y is subset if each X and Y is subset."))

(th~deftheorem intersection-is-subsetp
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types a (forall (lam (X (o a))
				        (forall (lam (Y (o a))
					(forall (lam (Z (o a))
						     (implies (or (subsetp X Z)
								  (subsetp Y Z))
							      (subsetp (intersection X Y) Z))))))))))
	       (help "Intersection of X Y is subset if either X or Y is subset."))


(th~deftheorem intersection-is-subsetp-simple
	 (in typed-set)
         (conclusion  (all-types a
			   (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                              (subsetp (intersection X Y) X)))))))
	 (help "Intersection is subset, special case."))

(th~deftheorem union-of-itself
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) 
					 (= X (union X X))))))
	       (help "The union of the same sets S is equal to S."))

(th~deftheorem intersection-of-itself
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) 
					 (= X (intersection X X))))))
	       (help "The union of the same sets S is equal to S."))

(th~deftheorem distributivity-of-union-to-intersection
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (Y (o bb)) (forall (lam (Z (o bb))
			     (= (union (intersection X Y) Z)
				(intersection (union X Z) (union Y Z)))))))))))
	       (help "Distributivity of union and intersection."))

(th~deftheorem distributivity-of-intersection-to-union
	       (in typed-set) 
	       (category THEOREM)
	       (conclusion (all-types bb (forall (lam (X (o bb)) (forall (lam (Y (o bb)) (forall (lam (Z (o bb))
			     (= (intersection (union X Y) Z)
				(union (intersection X Z) (intersection Y Z)))))))))))
	       (help "Distributivity of union and intersection."))







