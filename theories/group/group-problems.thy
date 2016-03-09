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
;;;
;;;  Problems   (to be theorems one day)
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equivalence statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Equivalences of group definitions
;;;

;;; Via different axiomatizations

(th~defproblem eq-group-and-left-only
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (equiv (exists (lam (op1 (i i i)) (group G op1)))
			(exists (lam (op2 (i i i))
			     (and (not-empty G)
				  (and (closed-under G op2)
				       (and (associative G op2)
					    (and (exists-sort (lam (e i) (left-unit G op2 e)) G)
						 (left-inverse-exist G op2 (struct-left-unit G op2))))))))))))
	   (help "Equivalence of the classic group definition with a definition restricted to the existence of
left inverses and a left unit element."))

(th~defproblem eq-group-and-right-only
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (equiv (exists (lam (op1 (i i i)) (group G op1)))
			(exists (lam (op2 (i i i))
			     (and (not-empty G)
				  (and (closed-under G op2)
				       (and (associative G op2)
					    (and (exists-sort (lam (e i) (right-unit G op2 e)) G)
						 (right-inverse-exist G op2 (struct-right-unit G op2))))))))))))
	   (help "Equivalence of the classic group definition with a definition restricted to the existence of
right inverses and a right unit element."))

(th~defproblem eq-group-and-divisors
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (equiv (exists (lam (op1 (i i i)) (group G op1)))
			(exists (lam (op2 (i i i))
			     (and (not-empty G)
				  (and (closed-under G op2)
				       (and (associative G op2)
					    (divisors-exist G op2))))))))))
	   (help "Equivalence of the classic group definition with a definition containing the axiom of existence of
left and right divisors."))


;;; Some exotic equivalences

(th~defproblem eq-group-and-unique-inverse
	       (in group)
	       (conclusion thm
		(forall (lam (G (o i))
		   (equiv (exists (lam (op1 (i i i)) (group G op1)))
			  (exists (lam (op2 (i i i))
				  (and (not-empty G)
				       (and (closed-under G op2)
					    (and (associative G op2)
						 (exists (lam (inv (i i))
							 (and (unique-inverse G inv)
							      (law-of-inverse G op2 inv)))))))))))))
	       (help "Equivalence of the classic group definition with a definition postulating the uniqueness of inverses."))


;;; Via more general algebras

(th~defproblem eq-group-and-quasi+semi
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (forall (lam (op (i i i))
		      (equiv (group G op)
			     (and (semigroup G op)
				  (quasigroup G op))))))))
	   (help "A set is group if it is a semigroup and a quasigroup."))

(th~defproblem eq-group-and-monoid+inverse
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (forall (lam (op (i i i))
		      (equiv (group G op)
			     (and (monoid G op)
				  (inverse-exist G op (struct-unit G op)))))))))
	   (help "A set is group if it is a monoind and there exist inverses."))

(th~defproblem eq-group-and-loop+associative
	   (in group)
	   (conclusion thm
	    (forall (lam (G (o i))
		 (forall (lam (op (i i i))
		      (equiv (group G op)
			     (and (loop G op)
				  (associative G op))))))))
	   (help "A set is group if it is a loop and its operation is associative."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some uniquness problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defproblem unit-unique
      (in group)
      (conclusion thm
	  (forall (lam (H (o i))
	       (forall (lam (op (i i i))
		    (implies (semigroup H op)
			     (exists-sort (lam (e i)
				       (implies 
					(forall-sort (lam (a i) (and (= (op a e) a) (= (op e a) a))) H)
					(forall-sort
					 (lam (f i)
					      (implies 
					       (forall-sort
						(lam (a i) (and (= (op a f) a) (= (op f a) a)))
						H)
					       (= e f)))
					 H)))
				  H)))))))
      (help "Uniqueness of a unit element in a semigroup."))

(th~defproblem inverses-unique
      (in group)
      (conclusion thm
	  (forall (lam (M (o i))
	       (forall (lam (op (i i i))
		    (implies (monoid M op)
			     (forall-sort (lam (a i)
					       (exists-sort (lam (b i) 
							 (implies 
							  (and (= (op a b) (struct-unit M op))
							       (= (op b a) (struct-unit M op)))
							  (forall-sort
							   (lam (c i)
								(implies 
								 (and (= (op a c) (struct-unit M op))
								      (= (op c a) (struct-unit M op)))
								 (= c b)))
							   M)))
						    M))
					  M)))))))
      (help "Uniqueness of a inverses in a monoid."))

(th~defproblem unique-trivial-subgroup
      (in group)
      (conclusion thm
	  (forall (lam (G (o i))
	       (forall (lam (U (o i))
		    (forall (lam (V (o i))
			 (forall (lam (op (i i i))
				      (implies (and (subgroup U op G op) (subgroup V op G op))
					       (in (struct-unit G op) (intersection U V))))))))))))
      (help "The unit element is in the intersection of all subgroups of a group."))


(th~defproblem idempotent-is-commutative
	       (in group)
	       (conclusion thm
		   (forall (lam (G (o i))
			(forall (lam (op (i i i))
			     (implies (and (group G op) 
					   (forall-sort (lam (x i) (= (op x x) (struct-unit G op))) G))
				      (commutative G op))))))))


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

;(th~defproblem kernel-subgroup
;           (in group)
;           (conclusion
;            (all-types aa bb
;                       (forall (lam (F (morphism aa bb))
;                         (subgroup (kernel F) (morphism-codomain F))))))
;           (help "The kernel of a group morhpism is a subgroup of the codomain."))

;(th~defproblem inv-pointwise
;               (in group)
;               (conclusion 
;                (all-types aa
;                       (forall (lam (S (struct aa))
;                             (forall (lam (F (aa aa))
;                                          (= (apply-pointwise (struct-op S) 
;                                                              (compose-funcs (struct-inv S) F)
;                                                              F)
;                                             (lam (x aa) (struct-neut S)))))))))
;               (help "The function for the pointwise inverse is an inverse in the pointwise function space."))




;(th~defproblem pfunc-group
;               (in group)
;               (conclusion 
;                (all-types aa bb
;                       (forall (lam (S (struct aa))
;                                    (implies (group S)
;                                             (group (pfunc-struct S)))))))
;               (help "The structure of pointwise functions of a group is again one."))






;(th~defproblem total-struct-inv
;               (in group)
;               (conclusion (total group struct-inv))
;               (help "Every group has an inversion function."))

(th~defproblem  image-finite
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (finite-set  G))
	       (assumption a2 (group G opg))
	       (assumption a3 (group H oph))
	       (assumption a4 (homomorphism g opg h oph  fun))
	       
	       (conclusion (subgroup (image-of-domain fun G) oph
					 H oph)))		    

(th~defproblem  image-subgroup
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       
	       (conclusion (subgroup (image-of-domain fun G) oph
					 H oph)))		    


(th~defproblem  image-closed
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(fun ((i i) i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism  g opg h oph  fun))
	       
	       (conclusion   (CLOSED-UNDER-2       
				  (IMAGE-OF-DOMAIN FUN G)                              
				  OPH)))                                                 



(th~defproblem  image-assoc
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       
	       (conclusion  (ASSOCIATIVE                                          
				 (IMAGE-OF-DOMAIN FUN G)                              
				 OPH)))                                                
(th~defproblem image-notempty
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       
	       (conclusion  (not-empty
				 (IMAGE-OF-DOMAIN FUN G)                              
				 )))                                                

(th~defproblem  image-neutinv
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       
	       (conclusion  
			   (exists-sort (lam (e i)
					     (and (unit (image-of-domain fun g) oph e)
						  (inverse-exist (IMAGE-OF-DOMAIN FUN G) oph e)))
					(IMAGE-OF-DOMAIN FUN G))))

(th~defproblem  image-commutative
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       (assumption a4 (forall-sort (lam (x i)
						(forall-sort (lam (y i)
								  (= (opg y x)(opg x y))) G)) G))
	       
	       (conclusion  
			   (forall-sort (lam (x i)
						(forall-sort (lam (y i)
								  (= (oph y x)(oph x y))) 
							     (IMAGE-OF-DOMAIN FUN G)))
					(IMAGE-OF-DOMAIN FUN G))
			   ))


(th~defproblem  image-neut-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (exists-sort (lam (e i)
					     (left-unit (image-of-domain fun g) oph e))
					(IMAGE-OF-DOMAIN FUN G))))


(th~defproblem  image-inv-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (left-inverse-exist (IMAGE-OF-DOMAIN FUN G) oph (fun (group-unit g opg)))))
				

(th~defproblem  image-neutinv-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (exists-sort (lam (e i)
					     (and (left-unit (image-of-domain fun g) oph e)
						  (left-inverse-exist (IMAGE-OF-DOMAIN FUN G) oph e)))
					(IMAGE-OF-DOMAIN FUN G))))



(th~defproblem  kernel-subgroup
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion (subgroup (kernel-for-elem fun G
							  (group-unit H oph))
					 opg
					 g opg))) 


(th~defproblem  kernel-neutinv-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (exists-sort (lam (e i)
					     (and (left-unit
						   (kernel-for-elem fun G (group-unit H oph))
						   opg e)
						  (left-inverse-exist
						   (kernel-for-elem fun G (group-unit H oph))
						   opg e)))
					(kernel-for-elem fun G (group-unit H oph)))))

(th~defproblem  kernel-neut-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (exists-sort (lam (e i)
					     (left-unit (kernel-for-elem FUN G (group-unit H oph)) opg e))
					(kernel-for-elem FUN G (group-unit H oph)))))

(th~defproblem  kernel-closed
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion   (CLOSED-UNDER-2       
				  (kernel-for-elem fun G 
						   (group-unit H oph))
				  OPg)))

(th~defproblem  kernel-assoc
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion   (ASSOCIATIVE                                          
				  (kernel-for-elem fun G
						   (group-unit H oph))
				  OPg)))

(th~defproblem  deussen-simple1
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(opk (i i i))
		(H ( o i))
		(k ( o i))
		(phi (i i))
		(funh (i i))
		(funk (i i)))
	    
	       (assumption a2 (group G opg))
	       (assumption a3 (group H oph))
	       (assumption a4 (homomorphism g opg h oph  funh))
	       (assumption a5 (homomorphism g opg k opk funk))
	       (assumption a1 (forall-sort (lam (x i)
						(= (funk x) (phi (funh x)) ))
					   g))
	       
	       (conclusion   (homomorphism
					       (IMAGE-OF-DOMAIN FUNh G)  oph
					       (IMAGE-OF-DOMAIN FUNk G)  opk  phi)))
								    

(th~defproblem  deussen-simple2
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(opk (i i i))
		(H ( o i))
		(k ( o i))
		(phi (i i))
		(funh (i i))
		(funk (i i)))
	    
	       (assumption a2 (group G opg))
	       (assumption a3 (group H oph))
	       (assumption a4 (homomorphism g opg h oph  funh))
	       (assumption a5 (homomorphism g opg k opk funk))
	       (assumption a1 (forall-sort (lam (x i)
						(= (funk x) (phi (funh x)) ))
					   g))
	       
	       (conclusion   (homomorphism
					       (IMAGE-OF-DOMAIN FUNh G)  oph
					       k  opk  phi)))

(th~defproblem  deussen
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(opk (i i i))
		(H ( o i))
		(k ( o i))
		(phi (i i))
		(funh (i i))
		(funk (i i)))
	    
	       (assumption a2 (group G opg))
	       (assumption a3 (group H oph))
	       (assumption a4 (homomorphism g opg h oph  funh))
	       (assumption a5 (homomorphism g opg k opk funk))
	       (assumption a1 (forall-sort (lam (x i)
						(= (funk x) (phi (funh x)) ))
					   g))
	       (assumption a6 (forall-sort (lam (x i)
						(exists-sort (lam (y i)
								  (= (funh y) x))
							     G))
					   H))
	       
	       (conclusion   (homomorphism
					       h  oph
					       k  opk  phi)))

(th~defproblem  composition-of-homo
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(opk (i i i))
		(H ( o ( i i)))
		(k ( o i))
		(phi (i i))
		(fungk (i i))
		(funkh ((i i) i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (group K opk))
	       (assumption a4 (homomorphism g opg k opk  fungk))
	       (assumption a5 (homomorphism k opk h oph  funkh))
	       
	       (conclusion   (homomorphism 
					       g  opg
					       h oph
					       (lam (x i) (funkh (fungk x))))))


(th~defproblem  image-inv-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (left-inverse-exist (IMAGE-OF-DOMAIN FUN G) oph (fun (group-unit g opg)))))

(th~defproblem test
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(a i)
		(b i)
		(H ( o i))
		(fun (i i)))

	    
	       
	       (assumption a4 (homomorphism g opg h oph fun))
	       (assumption a5 (image-of-domain fun g a))

	       
	       (conclusion (exists-sort (lam (x i) (h x))  (image-of-domain fun g ))))

				       	    
(th~defproblem  kernel-inv-simple
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion  
			   (left-inverse-exist
			    (kernel-for-elem fun G (group-unit H oph))
			    opg (group-unit g opg))))


(th~defproblem  kernel-iso
           (in group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       (assumption a4  (= (kernel-for-elem fun G (group-unit H oph))
				  (lam (x i) (= x (group-unit G opg)))))
	       (conclusion  
			   (injective G fun )))





(th~defproblem  singleton-closed
           (in  group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(e i)
		(g1 i)
		(g2 i)
		(fun ((i i) i)))
	       (conclusion   (CLOSED-UNDER-2       
				  (lam (x i) (= x e))
				  (lam (x i)(lam (y i) e)))))

(th~defproblem  singleton-assoc
           (in  group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(e i)
		(g1 i)
		(g2 i)
		(fun ((i i) i)))
	       (conclusion  (ASSOCIATIVE                                          
				  (lam (x i) (= x e))
				  (lam (x i)(lam (y i) e)))))

(th~defproblem  singleton-neut-left
           (in  group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(e i)
		(g1 i)
		(g2 i)
		(fun ((i i) i)))
	       (conclusion  
		(exists-sort (lam (y i)
				  (left-unit
				   (lam (x i) (= x e))
				   (lam (x i)(lam (y i) e))
				   y))
				   (lam (x i) (= x e)))))

(th~defproblem  singleton-neut-left
           (in  group)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(e i)
		(g1 i)
		(g2 i)
		(fun ((i i) i)))
	       (conclusion  
			   (exists-sort (lam (z i)
					     (and (left-unit (lam (x i) (= x e))
							     (lam (x i)(lam (y i) e)) z)
						  (left-inverse-exist (lam (x i) (= x e))
								      (lam (x i)(lam (y i) e)) z)))
				   (lam (x i) (= x e)))))


(th~defproblem  2set-closed
           (in  group)
	       (constants
		(G ( o i))
		(op (i i i))
		(H (o ( i i)))
		(g1 i)
		(g2 i))
	       (assumption a1 (= (op g1 g1) g1))
	       (assumption a2 (= (op g1 g2) g2))
	       (assumption a3 (= (op g2 g1) g2))
	       (assumption a4 (= (op g2 g2) g2))
	       (conclusion   (CLOSED-UNDER-2       
				  (lam (x i) (or (= x g1) (= x g2)))
				  op)))




(th~defproblem left-coset-theorem
	       (in  group)
	       (constants )
	       (conclusion
		(all-types aa
		 (forall (lam (G (o aa))
		   (forall (lam (A (o aa))
		     (forall (lam (op (aa aa aa))
		       (implies (and (group G op)
			  (subgroup A op G op))
				(forall-sort (lam (x aa)
						  (equiv (in x A)
							 (set= A (left-coset x G A op))))
					     G))))))))))
	       )
