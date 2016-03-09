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



(th~defproblem cantor-simple
	 (in function)
	 (conclusion
	  (not (exists (lam (f ((o i) i))
			    (forall (lam (g (o i))
					 (exists (lam (y i)
						      (= (f y) g))))))))))


	 

(th~defproblem los-thm
	       (in base)
	       (constants (p (o i i))
			  (q (o i i)))
	       (assumption Q-symmetric
			   (forall (lam (x i)
					(forall (lam (y i)
						     (implies (Q x y)
							      (Q y x)))))))
	       (assumption Q-transitive
			   (forall (lam (x i)
					(forall (lam (y i)
						     (forall (lam (z i)
								  (implies (and (Q x y)
										(Q y z))
									   (Q x z)))))))))
	       (assumption P-transitive
			   (forall (lam (x i)
					(forall (lam (y i)
						     (forall (lam (z i)
								  (implies (and (P x y)
										(P y z))
									   (P x z)))))))))
	       (assumption P-or-Q-related
			   (forall (lam (x i)
					(forall (lam (y i)
						     (or (P x y)
							 (Q x y)))))))
	       (conclusion conc (or
				 (forall (lam (x i)
					      (forall (lam (y i)
							   (P x y)))))
				 (forall (lam (x i)
					      (forall (lam (y i)
							   (Q x y))))))))



(th~defproblem church1
	(in base)
	(conclusion 
	 (exists (lam (G (I I (I I)))
          (exists (lam (mul ((I I (I I)) (I I (I I)) (I I (I I))))
           (and		       
	    (=
	     mul
	     (lam (U (I I (I I)))
		  (lam (V (I I (I I)))
		       (lam (Z (I I)) (U (V Z))))))
	    (=
	     (mul G G)
	     (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X Y)))))))))))))))))))


(th~defproblem church16
	(in base)
	(conclusion 
	 (exists (lam (G (I I (I I)))
	  (=
	   ((lam (U (I I (I I)))
		(lam (V (I I (I I)))
		     (lam (Z (I I)) (U (V Z))))) G G)
	   (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X (X (X (X (X (X (X Y)))))))))))))))))))))))



(th~defproblem normalize-test
	(in base)
	(constants
	 )

	(conclusion conc (= ((lam (x (i i))
				  (lam (y (i (i i)))
				       (x (y x))))
			     (lam (z i)
				  z))
			    (lam (y (i (i i)))
				 (y (lam (z i)
					 z))))))

		 
(th~defproblem prop-indirect (in base)
	       (constants (H1 o)
			  (H2 o))
	       
	       (assumption A1 (implies H1 H2))
	       (assumption A2 (implies H1 (not H2)))
	 
	       (conclusion CON (not H1)))

(th~defproblem prop-and-comm (in base)
	       (constants (H1 o)
			  (H2 o))
		    
	       (assumption A1 (and H1 H2))
	 
	       (conclusion CON (and H2 H1)))

	    
(th~defproblem primer-1 (in base)
	 (conclusion con
		     (exists (lam (R (o o o))
				  (forall (lam (x o)
					       (forall (lam (y o)
							    (equiv (r x y)
								   (and (implies x y)
									(implies y x)))))))))))

(th~defproblem embedded1 
	 (in base)
	 (constants (p (o o)) (a o) (b o))
	 (conclusion theo (IMPLIES (p (AND a b)) (p (AND b a)))))


(th~defproblem embedded2 
	 (in base)
	 (constants (p (o o)) (a o) (b o))
	 (conclusion theo (IMPLIES (AND (p a) (p b)) (p (AND a b)))))


(th~defproblem embedded3  ;;; diese Bsp. darf natuerlich nicht geloest werden.
	 (in base)
	 (constants (p (o i)) (q (o i)) (r (o (o i))))
	 (conclusion conc (IMPLIES (AND (r p) (r q)) (r (lam (X i) (AND (p X) (q X)))))))


(th~defproblem embedded4
(in base)
(constants (p (o o)) (a o) (b o) (c o))
(conclusion THEO (IMPLIES (p (or (AND a b) c)) (p (or c (AND b a))))))


				
(th~defproblem chad1
 (in base)
 (constants (h (i o))) 
 (conclusion THEO (= (h false) (h (= (h true) (h false))))))
	 

(th~defproblem leo1
 (in base)
 (constants (p (o o))) 
 (conclusion THEO (or (= p (lam (x o) x))
		      (or (= p (lam (x o) (not x)))
			  (or (= p (lam (x o) false))
			      (= p (lam (x o) true)))))))
	 


(th~defproblem chad-flex-flex-counter?-1
 (in base)
 (constants (a (o (i (o i)))) (h (i (o i))))
 (conclusion THEO
	     (exists (lam (F (i (o i)))
			  (exists (lam (G (i (o i) i))
				       (and (= F h)
					    (= (F (lam (X i) (a (G X))))
					       (F (lam (X i) false))))))))))




(th~defproblem test1
 (in base)
 (constants (a (o (i (o i)))) (h (i (o i))))
 (conclusion theo (exists (lam (G (i (o i) i))
			       (exists (lam (H (i i (o i)))
					    (= (lam (W i) (a (G (H (lam (Z i) (a (G Z))) W)))) (lam (Y i) false))))))))


(th~defproblem test2
 (in base)
 (constants (a o))
 (conclusion theo (= a false)))

(th~defproblem test3
 (in base)
 (constants (a (o (o i))) (h (i (o i))))
 (conclusion theo (exists (lam (X (o i)) (= (h (lam (Y i) (a X))) (h (lam (Y i) false)))))))


(th~defproblem chris-chad-test1
 (in base)
 (conclusion (exists (lam (X o)
	      (exists (lam (Y o)
	       (forall (lam (Z (o o o))
		(implies (Z X (not X)) (Z (not Y) Y))))))))))	  




(th~defproblem less-little
	       (in base)
	       (constants (p (o o)) (a (o i)) (b (o i)) (c (o i)) (m i))
	       (conclusion 
		(EXISTS (lam (Q (o (o i) (o i)))
			     (IMPLIES (p (AND (a m) (OR (b m) (c m)))) 
				      (p (AND (Q c b) (a m))))))))


(th~defproblem little
	       (in base)
	       (constants (p (o o)) (a (o i)) (b (o i)) (c (o i)) (m i))
	       (conclusion 
		(EXISTS (lam (Q (o (o i) (o i)))
			     (IMPLIES (p (AND (a m) (OR (b m) (c m)))) 
				      (p (AND (a m) (Q c b))))))))

(th~defproblem poly-a
	       (in base)
	       (conclusion 
		(EXISTS (lam (X o)
		  (EXISTS (lam (Y o)
		     (and (not (= X Y))
			  (FORALL (lam (Z o) (or (= Z X) (= Z Y)))))))))))
			       

(th~defproblem poly-b
	       (in base)
	       (conclusion 
		(FORALL (lam (X o)
		  (FORALL (lam (Y o)
		     (FORALL (lam (Z o) (or (= X Y) (or (= Y Z) (= Z X)))))))))))


(th~defproblem poly-c
	       (in base)
	       (conclusion 
		(EXISTS (lam (X o)
		  (EXISTS (lam (Y o)
		     (not (= X Y))))))))

(th~defproblem poly-d
	       (in base)
	       (conclusion 
		(EXISTS (lam (X o)
		  (EXISTS (lam (Y o)
		     (not (FORALL (lam (P (o o)) (implies  (P X) (P Y)))))))))))

(th~defproblem poly-e
	       (in base)
	       (conclusion 
		(EXISTS (lam (W (o o))
		  (EXISTS (lam (X (o o))
		    (EXISTS (lam (Y (o o))
		      (EXISTS (lam (Z (o o))     
			(and (not (= W X))
			     (and (not (= X Y))
				  (and (not (= Y Z))
				       (not (= Z W)))))))))))))))



(th~defproblem poly-f
	       (in base)
	       (conclusion 
		(EXISTS (lam (W (o o))
		  (EXISTS (lam (X (o o))
		    (EXISTS (lam (Y (o o))
		      (EXISTS (lam (Z (o o))     
			(and (not ((lam (U (o o)) (lam (V (o o)) (FORALL (lam (P (o (o o))) (implies (P U) (P V)))))) W X))
			     (and (not ((lam (U (o o)) (lam (V (o o)) (FORALL (lam (P (o (o o))) (implies (P U) (P V)))))) X Y))
				  (and (not ((lam (U (o o)) (lam (V (o o)) (FORALL (lam (P (o (o o))) (implies (P U) (P V)))))) Y Z))
				       (not ((lam (U (o o)) (lam (V (o o)) (FORALL (lam (P (o (o o))) (implies (P U) (P V)))))) Z W)))))))))))))))


(th~defproblem poly-g
	       (in base)
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
		       (FORALL (lam (X i)
			 (FORALL (lam (Y i) 
				      (and (= (Fst (Pair X Y)) X)
					   (= (Snd (Pair X Y)) Y))))))))))))))

(th~defproblem poly-g-alt
	       (in base)
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
				 (and  (FORALL (lam (X i) (FORALL (lam (Y i) (= (Fst (Pair X Y)) X)))))
				       (FORALL (lam (X i) (FORALL (lam (Y i) (= (Snd (Pair X Y)) Y))))))))))))))

(th~defproblem poly-g-fst
	       (in base)
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
		      (FORALL (lam (X i)
			 (FORALL (lam (Y i)
			       (= (Fst (Pair X Y)) X)))))))))))))
		       

(th~defproblem poly-g-snd
	       (in base)
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
		      (FORALL (lam (X i)
			 (FORALL (lam (Y i)
				    (= (Snd (Pair X Y)) Y)))))))))))))

(th~defproblem poly-h
	       (in base)
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
		      (FORALL (lam (X i)
			 (FORALL (lam (Y i)
			       (and (= Pair (lam (X i) (lam (Y i) (lam (F (i i i)) (F X Y)))))
				    (and (= Fst (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) X)))))
					 (and (= Snd  (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) Y)))))
					      (and (= (Fst (Pair X Y)) X)
						   (= (Snd (Pair X Y)) Y)))))))))))))))))

(th~defproblem poly-i
	       (in base)
	       (constants (a i) (b i))
	       (conclusion 
		(EXISTS (lam (Pair (i (i i i) i i))
		  (EXISTS (lam (Fst (i (i (i i i))))
		    (EXISTS (lam (Snd (i (i (i i i))))
				 (and (= Pair (lam (X i) (lam (Y i) (lam (F (i i i)) (F X Y)))))
				      (and (= Fst (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) X)))))
					   (and (= Snd  (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) Y)))))
						(and (= (Fst (Pair a b)) a)
						     (= (Snd (Pair a b)) b)))))))))))))

(th~defproblem poly-j
	       (in base)
	       (constants (Pair (i (i i i) i i)) (Fst (i (i (i i i)))) (Snd (i (i (i i i)))) (a i) (b i))
	       (conclusion 
		(implies (and (= Pair (lam (X i) (lam (Y i) (lam (F (i i i)) (F X Y)))))
			      (and (= Fst (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) X)))))
				   (= Snd  (lam (P (i (i i i))) (P (lam (X i) (lam (Y i) Y)))))))
			 (and (= (Fst (Pair a b)) a)
			      (= (Snd (Pair a b)) b)))))


(th~defproblem poly-k
	       (in base)
	       (constants (Pair (i (i i i) i i)) (Fst (i (i (i i i)))) (Snd (i (i (i i i)))) (a i) (b i))
	       (conclusion 
		(and (= ((lam (P (i (i i i))) (P (lam (X i) (lam (Y i) X)))) ((lam (X i) (lam (Y i) (lam (F (i i i)) (F X Y)))) a b)) a)
		     (= ((lam (P (i (i i i))) (P (lam (X i) (lam (Y i) Y)))) ((lam (X i) (lam (Y i) (lam (F (i i i)) (F X Y)))) a b)) b))))




(th~defproblem monoid
	       (in base)
	       (conclusion 
		(EXISTS (lam (Op ((I I (I I)) (I I (I I)) (I I (I I))))
                  (EXISTS (lam (One (I I (I I)))
		    (FORALL (lam (X (I I (I I)))
		      (FORALL (lam (Y (I I (I I)))
		        (FORALL (lam (Z (I I (I I)))
				     (and (= (op (op X Y) Z) (op X (op Y Z)))
					  (and (= (op One X) X)
					       (= (op X One) X)))))))))))))))


(th~defproblem table-I
	       (in base)
	       (conclusion 
		(EXISTS (lam (T o)
                  (EXISTS (lam (F o)
		    (EXISTS (lam (Op (o o))
                      (and (not (= T F))
			   (and (= (Op T) F)
				(= (Op F) T)))))))))))

(th~defproblem table-II
	       (in base)
	       (conclusion 
		(EXISTS (lam (T o)
                  (EXISTS (lam (F o)
		    (EXISTS (lam (Op (o o o))
                      (and (not (= T F))
			   (and (= (Op T T) T)
				(and (= (Op T F) F) 
				     (and  (= (Op F T) F)
					   (= (Op F F) F)))))))))))))


(th~defproblem table-III
	       (in base)
	       (conclusion 
		(EXISTS (lam (LT (o o))
                  (EXISTS (lam (LX (o o))
		    (EXISTS (lam (LNX (o o))
                      (EXISTS (lam (LF (o o))
			(EXISTS (lam (OP ((o o) (o o) (o o)))				   
			  (and (not (= LT LX))
			  (and (not (= LX LNX))
			  (and (not (= LNX LF))
			  (and (not (= LF LT))
			       (and (= (OP LT LT)  LT)
			       (and (= (OP LT LX)  LT)				   
			       (and (= (OP LT LNX) LT)
			       (and (= (OP LT LF)  LT)
			       (and (= (OP LX LT)  LT)
			       (and (= (OP LX LX)  LX)
			       (and (= (OP LX LNX) LNX)
			       (and (= (OP LX LF)  LF)
			       (and (= (OP LNX LT) LF)
			       (and (= (OP LNX LX) LNX)
			       (and (= (OP LNX LNX) LX)
			       (and (= (OP LNX LF) LT)
			       (and (= (OP LF LT)  LF)
			       (and (= (OP LF LX)  LF)
			       (and (= (OP LF LNX) LF)
				    (= (OP LF LF)  LF))))))))))))))))))))))))))))))))


(th~defproblem table-IV
	       (in base)
	       (conclusion 
		(EXISTS (lam (LT i)
                  (EXISTS (lam (LX i)
		    (EXISTS (lam (LNX i)
                      (EXISTS (lam (LF i)
			(EXISTS (lam (OP (i i i))				   
			  (and (not (= LT LX))
			  (and (not (= LX LNX))
			  (and (not (= LNX LF))
			  (and (not (= LF LT))
			       (and (= (OP LT LT)  LT)
			       (and (= (OP LT LX)  LT)				   
			       (and (= (OP LT LNX) LT)
			       (and (= (OP LT LF)  LT)
			       (and (= (OP LX LT)  LT)
			       (and (= (OP LX LX)  LX)
			       (and (= (OP LX LNX) LNX)
			       (and (= (OP LX LF)  LF)
			       (and (= (OP LNX LT) LF)
			       (and (= (OP LNX LX) LNX)
			       (and (= (OP LNX LNX) LX)
			       (and (= (OP LNX LF) LT)
			       (and (= (OP LF LT)  LF)
			       (and (= (OP LF LX)  LF)
			       (and (= (OP LF LNX) LF)
				    (= (OP LF LF)  LF))))))))))))))))))))))))))))))))


(th~defproblem test-sym
	       (in base)
	       (constants (a i) (b i))
	       (conclusion 
		(implies ((lam (X i) (lam (Y i) (forall (lam (P (o i)) (OR (NOT (P X)) (P Y))))))
			  a b)
			 ((lam (X i) (lam (Y i) (forall (lam (P (o i)) (OR (NOT (P X)) (P Y))))))
			  b a))))

(th~defproblem test-refl
	       (in base)
	       (constants (a i))
	       (conclusion 
		((lam (X i) (lam (Y i) (forall (lam (P (o i)) (OR (NOT (P X)) (P Y))))))
		 a a))) 


(th~defproblem leo-II-test-100
	(in base)
	(constants (p (o (I I (I I)))))
	(conclusion 
	 (p ((lam (U (I I (I I)))
		  (lam (V (I I (I I)))
		       (lam (Z (I I)) (U (V Z)))))
	     (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))
	     (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))))))

(th~defproblem leo-II-test-1000
	(in base)
	(constants (p (o (I I (I I)))))
	(conclusion 
	 (p ((lam (U (I I (I I)))
		  (lam (V (I I (I I)))
		       (lam (Z (I I)) (U (V Z)))))
	     (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))
	     ((lam (U (I I (I I)))
		   (lam (V (I I (I I)))
			(lam (Z (I I)) (U (V Z)))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y)))))))))))))))))


; (time (beta~normalize (node~formula (first (pds~open-nodes omega*current-proof-plan)))))

(th~defproblem leo-II-test-100x100
	(in base)
	(constants (p (o (I I (I I)))))
	(conclusion 
	 (p ((lam (U (I I (I I)))
		  (lam (V (I I (I I)))
		       (lam (Z (I I)) (U (V Z)))))
	     ((lam (U (I I (I I)))
		   (lam (V (I I (I I)))
			(lam (Z (I I)) (U (V Z)))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y)))))))))))))
	     ((lam (U (I I (I I)))
		   (lam (V (I I (I I)))
			(lam (Z (I I)) (U (V Z)))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y))))))))))))
	      (lam (X (I I)) (lam (Y I) (X (X (X (X (X (X (X (X (X (X Y)))))))))))))
	    ))))

; (time (beta~normalize (node~formula (first (pds~open-nodes omega*current-proof-plan)))))




(th~defproblem test-ATPHOL-1
	(in base)
	(constants (f (i i)))
	(conclusion
	 ((lam (X (i i)) (lam (Y (i i)) (forall (lam (P (o (i i))) (implies (P X) (P Y))))))
	  f
	  (lam (X i) (f X)))))


(th~defproblem test-ATPHOL-2
	(in base)
	(constants (a (i i)) (b i))
	(conclusion
	 (exists (lam (F (i (i i)))
		      ((lam (X i) (lam (Y i) (forall (lam (P (o i)) (implies (P X) (P Y))))))
		       (F a)
		       (a b))))))
		      


(th~defproblem test-ATPHOL-3
	(in base)
	(constants (red (o i)) (ball  (o i)))
	(conclusion
	 ((lam (X (o i)) (lam (Y (o i))
			      (forall (lam (P (o (o i)))
					   (implies (P X) (P Y))))))
		       (lam (X i) (and (red X) (ball X)))
		       (lam (X i) (and (ball X) (red X))))))
		      
(th~defproblem test-ATPHOL-4
	(in base)
	(constants (a o) (b o))
	(conclusion
	 (forall (lam (O (o o))
		      (implies (and (O a) (O b))
			       (O (and a b)))))))
		      

(th~defproblem inconsistency-test-1
	(in post)
	(constants (a o) (b o))
	(conclusion
         (implies
	  (and (not (defined undefined))
	       (and (forall (lam (X O)
			   (equiv (defined x) (or X (not X)))))
		    (and (not (= true undefined))
			 (and (not (= false undefined))
			      (and (= (not undefined) undefined)
				   (and (= (and true undefined) undefined)
					(and (= (and undefined true) undefined)
					     (and (= (and undefined undefined) undefined)
						  (and (= (or false undefined) undefined)
						       (and (= (or undefined false) undefined)
							    (and (= (or undefined undefined) undefined)
								 (and (= (implies undefined false) undefined)
								      (and (= (implies undefined undefined) undefined)
									   (= (or true undefined) undefined))))))))))))))
	 false)))

(th~defproblem inconsistency-test-2
	(in post)
	(constants (a o) (b o))
	(conclusion
         (implies
	  (and (not (= true undefined))
	       (not (= false undefined)))
	  false)))

(th~defproblem inconsistency-test-3
	(in post)
	(constants (a o) (b o))
	(conclusion
         (implies
	  (and (forall (lam (x o)
                           (forall (lam (y o)
                                        (implies (equiv x y) (= x y))))))
	       (and (not (= true undefined))
		    (not (= false undefined))))
	  false)))


(th~defproblem inconsistency-test-4
	(in post)
	(constants (everything-you-want-to-prove o))
	(conclusion
	 everything-you-want-to-prove))


;; (tacl~insert&return-assumption 'base 'ext-bool)
;; (tacl~insert&return-assumption 'post 'undefined-true)
;; (tacl~insert&return-assumption 'post 'undefined-false)


		      