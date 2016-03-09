;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

;;(th~defproblem total-struct-inv
;;	       (in group)
;;	       (conclusion (total group struct-inv))
;;	       (help "Every group has an inversion function."))
;;
;
;(th~defproblem inv-pointwise
;               (in monoid)
;               (type-variables bb)
;               (conclusion 
;                (forall (lam (S struct)
;                             (forall (lam (F (bb bb))
;                                          (= (apply-pointwise (struct-op S) 
;                                                              (compose-funcs (struct-inv S) F)
;                                                              F)
;                                             (lam (x bb) (struct-neut S))))))))
;               (help "The function for the pointwise inverse is an inverse in the pointwise function space."))

(th~defproblem thm1
(in learn)
(conclusion conc
	    (forall
	     (lam (G (o i))
		  (forall
		   (lam (op (i i i))
			(forall
			 (lam (e i)
			      (forall
			       (lam (inv (i i))
				    (implies
				     (group G op e inv)
				     (forall-sort
				      (lam (a i)
					   (forall-sort
					    (lam (b i)
						 (= (inv (op a b))
						    (op (inv b) (inv a))))
					    G))
				      G))))))))))))
						  

(th~defproblem thm2
(in learn)
(conclusion conc
	    (forall
	     (lam (G (o i))
		  (forall
		   (lam (op (i i i))
			(forall
			 (lam (e i)
			      (forall
			       (lam (inv (i i))
				    (implies
				     (group G op e inv)
				     (forall-sort
				      (lam (a i)
					   (forall-sort
					    (lam (b i)
						 (forall-sort
						  (lam (c i)
						       (= (inv (op (op a (inv b)) c))
						    (op (op (inv c) b) (inv a))))
						  G))
					    G))
				      G))))))))))))
						  

(th~defproblem thm3
(in learn)
(constants (op (i i i)) (inv (i i)) (e i) (G (o i)) (a i) (b i))
(assumption ass1 (group G op e inv))
(conclusion conc
	    (= (op (inv a) (op a b))
	       b)))						  


(th~defproblem saracino.3.5.p31
(in learn)
(conclusion conc
	    (forall
	     (lam (G (o i))
		  (forall
		   (lam (op (i i i))
			(forall
			 (lam (e i)
			      (forall
			       (lam (inv (i i))
				    (implies
				     (group G op e inv)
				     (forall-sort
				      (lam (a i)
					   (forall-sort
					    (lam (b i)
						 (forall-sort
						  (lam (c i)
	    (and
	     (= (inv (op (op a b) c))
	       (op (op (inv c) (inv b)) (inv a)))
	     (= (inv (op a (op b c)))
	       (op (inv c) (op (inv b) (inv a))))
	     ))
						  G))
					    G))
				      G))))))))))))
						  

;
;(th~defproblem pfunc-group
;               (in group)
;               (conclusion 
;                (forall (lam (S struct)
;                             (implies (group S) (group (pfunc-struct S))))))
;               (help "The structure of pointwise functions of a group is again one."))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem test-assoc-l
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
#|
(assumption a1 (group G1 op1 e1 inv1))
(assumption a2 (forall-sort
		(lam (a i)
		     (forall-sort
		      (lam (b i)
			   (forall-sort
			    (lam (c i)(P (op1 a (op1 b c))))
			    G1))
						 ;
		      G1))
		G1))
;;;;(assumption a2 (P (op1 a1 (op1 b1 c1))))
(conclusion conc (forall-sort
		  (lam (a i)
		       (forall-sort
			(lam (b i)
			     (forall-sort
			      (lam (c i) (P (op1 (op1 a b) c)))
			    G1))
						 ;
		      G1))
		G1)))
|#
(conclusion conc 
       (forall
        (lam (G1 (o i))
             (forall 
              (lam (op1 (i i i))
                   (forall
                    (lam (e1 i)
                         (forall
                          (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
                                            (forall-sort
                                             (lam (c1 i)
                                                  (= (op1 (op1 a1 b1) c1)
                                                     (op1 (op1 a1 b1) c1)))
                                             G1))
                                       G1))
                                 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-assoc-r
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
;(assumption ass (group G1 op1 e1 inv1))
;(conclusion conc (forall-sort
;                                  (lam (a1 i)
;                                       (forall-sort
;                                        (lam (b1 i)
;                                             (forall-sort
;                                              (lam (c1 i)
;                                                   (= (op1 a1 (op1 b1 c1))
;                                                      e1))
;                                              G1))
;                                        G1))
;                                  G1)))

(conclusion conc 
       (forall
        (lam (G1 (o i))
             (forall 
              (lam (op1 (i i i))
                   (forall
                    (lam (e1 i)
                         (forall
                          (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
                                            (forall-sort
                                             (lam (c1 i)
                                                  (= (op1 a1 (op1 b1 c1))
                                                     (op1 a1 (op1 b1 c1))))
                                             G1))
                                       G1))
                                 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-inv-l
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
        (lam (G1 (o i))
             (forall 
              (lam (op1 (i i i))
                   (forall
                    (lam (e1 i)
                         (forall
                          (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
                                                  (= (op1 b1 (op1 (inv1 a1) a1))
                                                     b1))
                                       G1))
                                 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-inv-r
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (= (op1 b1 (op1 a1 (inv1 a1)))
					       b1))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-id-r
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (= (op1 b1 (op1 a1 e1))
					       (op1 b1 a1)))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-id-l
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (= (op1 b1 (op1 e1 a1))
					       (op1 b1 a1)))
				       G1))
				 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-ex1
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
					    (= (op1 a1 (op1 (op1 (inv1 a1) c1) b1))
					       (op1 c1 b1)))
					     G1))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-ex2
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (= (op1 (inv1 a1) (op1 a1 b1))
					       b1))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem test-ex1.1
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (= (op1 (op1 b1 a1) (op1 (inv1 a1) c1))
						     (op1 b1 c1)))
					     G1))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem frocos1
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (= (op1 a1 (op1 (op1 (inv1 a1) c1) b1))
						     (op1 c1 b1)))
					     G1))
				       G1))
				 G1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem frocos2
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (= (op1 (inv1 a1) (op1 a1 b1))
						     b1))
					     G1))
				       G1))
				 G1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem frocos3
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 a1 (op1 (op1 (op1 (inv1 a1) b1) (op1 c1 d1)) f1))
								 (op1 (op1 (op1 b1 c1) d1) f1)))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem frocos-new
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
 			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 (inv1 a1) (op1 (op1 (op1 a1 b1) (op1 c1 d1)) f1))
								 (op1 (op1 (op1 b1 c1) d1) f1)))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem recursive-simp1
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 b1 (op1 (inv1 a1) (op1 (op1 (op1 a1 (inv1 b1)) (op1 c1 d1)) f1)))
								 (op1 (op1 c1 d1) f1)))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem recursive-simp2
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   
						   (lam (d1 i)
							(= (op1 b1 (op1 (op1 (inv1 b1) (inv1 c1)) (op1 (inv1 a1) (op1 (op1 a1 c1) d1))))
								 d1))
							 G1))
						   G1))
				       G1))
				 G1))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defproblem recursive-with-assoc-l
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   
						   (lam (d1 i)
							(= (op1 b1 (op1 (op1 (op1 (inv1 b1) (inv1 c1)) (inv1 a1)) (op1 (op1 a1 c1) d1)))
								 d1))
							 G1))
						   G1))
				       G1))
				 G1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem complex-simp1
;;; recursive-simp1=recursive-simp2(d/c) o (recursive-simp2 o f)	      
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 b1 (op1 (inv1 a1) (op1 (op1 (op1 a1 (inv1 b1)) (op1 c1 d1)) f1)))
								 (op1
								  (op1 b1 (op1 (op1 (inv1 b1) (inv1 c1)) (op1 (inv1 a1) (op1 (op1 a1 c1) c1))))
								  (op1
								   (op1 b1 (op1 (op1 (inv1 b1) (inv1 c1)) (op1 (inv1 a1) (op1 (op1 a1 c1) d1))))
								   f1))))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem complex-simp2
;;CANNOT DO IT -- NEED BETTER CONTROL in STRATEGY WHY?????
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 b1 (op1 (inv1 a1) (op1 (op1 (op1 a1 (inv1 b1)) (op1 c1 d1)) f1)))
								 (op1
								  (op1
								   (op1 c1 (inv1 d1))
								   (op1 b1 (op1 (op1 (inv1 b1) (inv1 c1)) (op1 (inv1 a1) (op1 (op1 a1 c1) d1)))))
								  (op1 d1 f1))))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem complex-simp3
	       ;;CANNOT DO IT -- NEED BETTER CONTROL in STRATEGY WHY?????
;;; recursive-simp1=recursive-with-assoc-l(d/c) o (recursive-simp2 o f)	      
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 b1 (op1 (inv1 a1) (op1 (op1 (op1 a1 (inv1 b1)) (op1 c1 d1)) f1)))
								 (op1
								  (op1 b1 (op1 (op1 (op1 (inv1 b1) (inv1 c1)) (inv1 a1)) (op1 (op1 a1 c1) c1)))
								  (op1
								   (op1 b1 (op1 (op1 (inv1 b1) (inv1 c1)) (op1 (inv1 a1) (op1 (op1 a1 c1) d1))))
								   f1))))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem complex-simp4
	       ;;; WORKS ONLY If foralli-sort-m-b IS A RESTRICTION METHOD! WHY?????
(in learn)
(constants (op1 (i i i)) (inv1 (i i)) (e1 i) (G1 (o i)))
(conclusion conc 
       (forall
	(lam (G1 (o i))
	     (forall 
	      (lam (op1 (i i i))
		   (forall
		    (lam (e1 i)
			 (forall
			  (lam (inv1 (i i))
                               (implies
                                (group G1 op1 e1 inv1)
                                (forall-sort
                                 (lam (a1 i)
                                      (forall-sort
                                       (lam (b1 i)
					    (forall-sort
					     (lam (c1 i)
						  (forall-sort
						   (lam (d1 i)
							(forall-sort
							 (lam (f1 i)
							      (= (op1 (op1 c1 (op1 b1 (op1 (inv1 a1) (op1 a1 (inv1 b1)))))
								 (op1 (op1 (op1 d1 a1) (inv1 a1)) f1))
								 (op1 c1 (op1 d1 f1))))
							 G1))
						   G1))
					     G1))
				       G1))
				 G1))))))))))))


