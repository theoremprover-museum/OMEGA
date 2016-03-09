;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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


(th~defproblem in-group-triv
	       (in permutation)
	       (conclusion
		(and (in  (perm-exp (set (cyc 1 3)) 2)
			  (generated-set
			   (set (set (cyc  1 2 3 4))(set (cyc 1 3)))))
		     (and
		      (in (set (cyc  1 ))
			  (generated-set
			   (set (set (cyc  1 2 3 4))(set (cyc 1 3)))))
		      (in identity-perm
			  (generated-set
			   (set (set (cyc  1 2 3 4))(set (cyc 1 3)))))))))

(th~defproblem in-group-1a
	       (in permutation)
	       (conclusion (in (lam (p cyc) (or (= p (cons 1 (cons 3 cnil)))
						    (= p (cons 2 (cons 4 cnil)))))
			       (generated-set
				(singleton
				 (lam (x cyc)
				      (= x (cons 1 (cons 2 (cons 3 (cons 4 cnil)))))))))))

(th~defproblem in-group-1b
	       (in permutation)
	       (conclusion (in (lam (p cyc) (or (= p (cons 1 (cons 3 cnil)))
						(= p (cons 2 (cons 4 cnil)))))
			       (generated-set
				(singleton (singleton (cons 1 (cons 2 (cons 3 (cons 4 cnil))))))))))

(th~defproblem in-group-2a
	       (in permutation)
	       (conclusion (in (lam (p cyc) (or (= p (cyc 1  3)) 
						(= p (cyc 2 4))))
			       (generated-set
				(lam (x (o cyc))(or (= x (singleton (cons 1 (cons 2 (cons 3 (cons 4 cnil))))))
						    (= x (singleton (cons 1 (cons 3 cnil))))))))))

(th~defproblem in-group-2b
	       (in permutation)
	       (conclusion ((generated-set
			     (lam (x (o cyc))(or (= x (singleton (cons 1 (cons 2 (cons 3 (cons 4 cnil))))))
						    (= x (singleton (cons 1 (cons 3 cnil)))))))
			    (lam (p cyc) (or (= p (cons 1 (cons 3 cnil)))
					     (= p (cons 2 (cons 4 cnil))))))))

(th~defproblem in-group-2c
	       (in permutation)
	       (conclusion (in (set (cyc 1 3)
				    (cyc 2 4))
			       (generated-set
				(set (set (cyc  1 2 3 4))(set (cyc 1 3)))))))

(th~defproblem in-group-2d
	       (in permutation)
	       (conclusion (in (perm-compose  (set (cyc 1 3) (cyc 2 4)) (set (cyc 4 3 2) (cyc 2 4)))
			       (generated-set
				(set (set (cyc  1 2 3 4))(set (cyc 1 3)))))))


(th~defproblem in-group-id
	       (in permutation)
	       (conclusion (in identity-perm
			       (generated-set
				(set (set (cyc  1 2 3 4))(set (cyc 1 3)))))))


(th~defproblem in-group-3
	       (in permutation)
	       (conclusion (in (singleton (cons 1 (cons 3 (cons 2 (cons 4 cnil)))))
			       (generated-set
				(lam (x (o cyc))
				     (or (= x (singleton (cons 1 (cons 2 (cons 3 cnil)))))
					 (= x (singleton (cons 1 (cons 4 cnil))))))))))

(th~defproblem in-group-4
	       (in permutation)
	       (conclusion (in (lam (x cyc)
				    (or (= x (cons 1 (cons 4 cnil)))
					(= x (cons 2 (cons 6 cnil)))))
			       (generated-set
				(lam (x (o cyc)) (or (= x (singleton (cons 1 (cons 2 (cons 3 cnil)))))
						     (or (= x (lam (t cyc) (or (= t (cons 1 (cons 4 cnil)))
									       (= t (cons 2 (cons 5 cnil))))))
							 (= x (lam (t cyc) (or (= t (cons 1 (cons 6 cnil)))
									       (= t (cons 3 (cons 5 cnil)))))))))))))

(th~defproblem in-group-5
	       (in permutation)
	       (conclusion (in (lam (x cyc)
				    (or (= x (cons 1 (cons 3 cnil)))
					(= x (cons 2 (cons 4 cnil)))))
			       (generated-set
				(lam (x (o cyc)) (or (= x (lam (t cyc) (or (= t (cons 1 (cons 2 cnil)))
									   (= t (cons 3 (cons 4 cnil))))))
						     (= x (singleton (cons 1 (cons 4 cnil))))))))))

(th~defproblem perm-mult-1
	       (in permutation)
	       (conclusion (= (perm-compose (singleton (cons 1 (cons 2 (cons 3 cnil))))
					    (singleton (cons 2 (cons 4 cnil))))
			      (singleton (cons 1 (cons 4 (cons 2 (cons 3 cnil))))))))

(th~defproblem perm-mult-2
	       (in permutation)
	       (conclusion (= (perm-compose (lam (x cyc) (or (= x (cons 1 (cons 2 (cons 3 cnil))))
							     (= x (cons 4 (cons 5 cnil)))))
					    (lam (x cyc) (or (= x (cons 2 (cons 4 cnil)))
							     (= x (cons 3 (cons 5 cnil))))))
			      (lam (x cyc) (or (= x (cons 1 (cons 4 (cons 3 cnil))))
					       (= x (cons 2 (cons 5 cnil))))))))

(th~defproblem perm-mult-3a
	       (in permutation)
	       (conclusion (= (perm-compose (lam (x cyc) (or (= x (cons 1 (cons 2 (cons 3 cnil))))
							   (or (= x (cons 4 (cons 5 cnil)))
							       (= x (cons 6 (cons 7 cnil))))))
					    (perm-compose (lam (x cyc) (or (= x (cons 2 (cons 4 cnil)))
									   (= x (cons 3 (cons 5 cnil)))))
							  (singleton (cons 1 (cons 3 (cons 5 (cons 7 cnil)))))))
			      (singleton (cons 1 (cons 4 (cons 5 (cons 2 (cons 7 (cons 6 cnil))))))))))

(th~defproblem perm-mult-3b
	       (in permutation)
	       (conclusion (= (perm-compose (set (cons 1 (cons 2 (cons 3 cnil)))
						 (cons 4 (cons 5 cnil))
						 (cons 6 (cons 7 cnil)))
					    (perm-compose (set (cons 2 (cons 4 cnil))
							       (cons 3 (cons 5 cnil)))
							  (set (cons 1 (cons 3 (cons 5 (cons 7 cnil)))))))
			      (set (cons 1 (cons 4 (cons 5 (cons 2 (cons 7 (cons 6 cnil))))))))))

(th~defproblem perm-mult-3c
	       (in permutation)
	       (conclusion (= (perm-compose (set (cyc 1 2 3)
						 (cyc 4 5)
						 (cyc 6 7))
					    (perm-compose (set (cyc 2 4)
							       (cyc 3 5))
							  (set (cyc 1 3 5 7))))
			      (set (cyc 1 4 3) (cyc 2 5 6 7)))))

(th~defproblem perm-mult-4
	       (in permutation)
	       (conclusion (= (perm-compose (set (cyc 2 3))
					    (set (cyc 1 2)))
			      (set (cyc 1 3 2)))))

(th~defproblem cycle-apply-0
	       (in permutation)
	       (conclusion (= (perm-apply identity-cyc 2)
			      2)))

(th~defproblem cycle-apply-1
	       (in permutation)
	       (conclusion (= (perm-apply (cyc 1 4 3 2 5) 3)
			      2)))

(th~defproblem cycle-apply-2
	       (in permutation)
	       (conclusion (= (perm-apply (cyc 1 3 2) 2)
			      1)))

(th~defproblem cycle-apply-3
	       (in permutation)
	       (conclusion (= (perm-apply (cyc 1 3 2) 5)
			      5)))

(th~defproblem perm-apply-0
	       (in permutation)
	       (conclusion (= (perm-apply identity-perm 3)
			      3)))

(th~defproblem perm-apply-1
	       (in permutation)
	       (conclusion (= (perm-apply (set (cyc 1 3 2)) 3)
			      2)))

(th~defproblem perm-apply-2
	       (in permutation)
	       (conclusion (= (perm-apply (set (cyc 1 3 2) (cyc 4 5 6)) 5)
			      6)))

(th~defproblem perm-apply-3
	       (in permutation)
	       (conclusion (= (perm-apply (set (cyc 1 3 2) (cyc 4 5 6) (cyc 7 8)) 3)
			      2)))

(th~defproblem set-test1
	       (in permutation)
	       (conclusion (= (set  1 4 5 2 7 6)(set  1 4 7 5 6 2))))


(th~defproblem set-test2
	       (in permutation)
	       (conclusion (in 1 (set  1 4 7 5 6 2))))

(th~defproblem group-generated-set1
	       (in permutation)
	       (conclusion (group
			       (generated-set
				(set (set (cyc  1 2 3 4))(set (cyc 1 3))))
			       perm-compose)))

(th~defproblem subgroup-generated-set1
	       (in permutation)
	       (conclusion (subgroup
			       (generated-set (set (set (cyc  1 2 3 4))(set (cyc 1 3))))
			       perm-compose
			       (generated-set (set (set (cyc  1 2 3 4))(set (cyc 1 3))))
			       perm-compose)))
			       


;; Orbit & subproblems

(th~defproblem orbit-equal1
	       (in permutation)
	       (conclusion (= (set 1 2 3 4)
			      (g-orbit 
			       (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
			       perm-apply 1))))

(th~defproblem orbit-equal2
	       (in permutation)
	       (conclusion (= (set 1 2 3 4 5 6 7 8 9 10 11)
			      (g-orbit 
			       (generated-set (set (set (cyc  1 10) (cyc 2 8) (cyc 3 11) (cyc 5 7))
						   (set (cyc 1 4 7 6) (cyc 2 11 10 9))))
			       perm-apply 1))))

(th~defproblem orbit-exists1
	       (in permutation)
	       (conclusion (exists (lam (orb (o num))
					(= orb
					   (g-orbit 
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
					    perm-apply 1))))))

(th~defproblem orbit-exists2
	       (in permutation)
	       (conclusion (exists (lam (orb (o num))
					(= orb
					   (g-orbit 
					    (generated-set (set (set (cyc  1 10)(cyc  2 8)(cyc  3 11)(cyc  5 7))
								(set (cyc 1 4 7 6)(cyc 2 11 10 9))))
					    perm-apply 1))))))


(th~defproblem orbit-subset1
	       (in permutation)
	       (conclusion (subsetp (set 1 2 3 4)
				    (g-orbit 
				     (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
				     perm-apply 1))))

(th~defproblem orbit-subset2
	       (in permutation)
	       (conclusion (subsetp (g-orbit 
				     (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
				     perm-apply 1)
				    (set 1 2 3 4))))

(th~defproblem orbit-invariant
	       (in permutation)
	       (conclusion (forall-sort (lam (x num)
					     (forall-sort (lam (y (o cyc))
							       (in (perm-apply y 1) 	(set 1 2 3 4)))
							  (set (set (cyc  1 2 3))(set (cyc 1 4)))))
					(set 1 2 3 4))))



(th~defproblem orbit-in1
	       (in permutation)
	       (conclusion 
		(in 2 (g-orbit 
			(generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
			perm-apply 1))))


(th~defproblem stab-exists1
	       (in permutation)
	       (conclusion (exists (lam (stab (o (o cyc)))
					(= 
					   stab
					   (stabiliser 
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
					    perm-apply 1))))))

(th~defproblem stab-exists2
	       (in permutation)
	       (conclusion (exists (lam (stab (o (o cyc)))
					(= 
					   stab
					   (stabiliser 
					    (generated-set (set (set (cyc  1 10)(cyc  2 8)(cyc  3 11)(cyc  5 7))
								(set (cyc 1 4 7 6)(cyc 2 11 10 9))))
					    perm-apply 1))))))

(th~defproblem stab-sub1
	       (in permutation)
	       (conclusion
		(SUBSETP                                                            
		 (STABILISER (GENERATED-SET (set (set (cyc 1 2 3)) (set (cyc 1 4)))) PERM-APPLY 1)
		 (GENERATED-SET (set (set (cyc 3 4)) (set (cyc 2 4 3)))))))

(th~defproblem stab-sub2
	       (in permutation)
	       (conclusion
		(SUBSETP                                                            
		 (GENERATED-SET (set (set (cyc 3 4)) (set (cyc 2 4 3))))
		 (STABILISER (GENERATED-SET (set (set (cyc 1 2 3)) (set (cyc 1 4)))) PERM-APPLY 1))))

(th~defproblem perm-apply-test
	       (in permutation)
	       (conclusion 
		(and (and (= 4 (perm-apply (perm-exp (set (cyc 1 2 3)) 3) 4))
			  (= 4 (perm-apply identity-perm 4)))
		     (and (and (nat  (perm-apply (perm-inverse (set (cyc  1 2 3))) 1))
			       (= 4 (perm-apply (set (cyc 1 2 3)) 4)))
			  (and (= perm-apply perm-apply)
			       (exists-sort (lam (x (o cyc))
						 (= (perm-apply x 3) 1))
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))))))))

(th~defproblem function-apply-test
	       (in permutation)
	       (conclusion 
		(and (= ((lam (x num)(ifthen (= x 1) 2 (ifthen (= x 2) 3 5))) 2) 3)
		     (= ((lam (x num)(ifthen (= x 1) 2 (ifthen (= x 2) 3 5))) 6) 5))))
						       


(th~defproblem in-set-test
	       (in permutation)
	       (conclusion 
		(and (exists-sort (lam (x num)
				       (= x 3))
				  (lam (y num)(in y (set 1 2 3))))
		     (in 2 (set 4 5 2 6)))))


(th~defproblem base1
	       (in permutation)
	       (conclusion
		(perm-stab-base
		 (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
		 (list 3 2 1))))
		 
(th~defproblem base-stab-list
	       (in permutation)
	       (conclusion
		(= (set identity-perm)
		   (stabiliser-list
		   (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))
		   perm-apply
		   (list 3 2 1)))))
		 

(th~defproblem not-in-1
	       (in permutation)
	       (conclusion
		(not (in (set (cyc 2 1))
			 (generated-set (set (set (cyc 1 2)(cyc 3 4))))))))


(th~defproblem not-in-2
	       (in permutation)
	       (conclusion
		(not (in (set (cyc 1 12  6  8  4  7)(cyc 2 10)(cyc 3  9 11))
			 (generated-set (set (set (cyc  1 10)(cyc  2 8)(cyc  3 11)(cyc  5 7))
					     (set (cyc 1 4 7 6)(cyc 2 11 10 9))))))))


(th~defproblem not-in-3
	       (in permutation)
	       (conclusion
		(not (in (set (cyc 1 2))
			 (generated-set (set (set (cyc  1 10)(cyc  2 8)(cyc  3 11)(cyc  5 7))
					     (set (cyc 1 4 7 6)(cyc 2 11 10 9))))))))

(th~defproblem order-exists1
	       (in permutation)
	       (conclusion (exists (lam (n num)
					(= n
					   (cardinality
					    (generated-set (set (set (cyc  1 2 3))(set (cyc 1 4))))))))))
