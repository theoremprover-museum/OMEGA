;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
(in-package :omega)


(th~defproblem foralli-test (in frame)
	       (constants (p1 (o i))
			  (p3 (o i))
			  (f1 (i i))
			  (f2 (i i))
			  (p2 (o i)))
		    
	       (assumption A1 (and true false))
	 
	       (conclusion test (forall-sort (lam (x i)
						 (forall-sort (lam (y i)
								   (p1 x))
							      (image f1 p2)))
					    (image f2 p3))))


(th~defproblem prop-and-comm2 (in frame)
	       (constants (H1 o)
			  (H2 o))
		    
	       (assumption A1 (and H1 H2))
	 
	       (conclusion CON (and H1 H2)))


(th~defproblem prop-=-ref1 (in frame)
	       (constants (H1 o)
			  )
		    
	       (assumption A1 (= H1 H1))
	 
	       (conclusion CON H1))


(th~defproblem prop-=-ref2 (in frame)
	       (constants (H1 o)
			  )
		    
	       (assumption A1 H1)
	 
	       (conclusion CON (= H1 H1)))


(th~defproblem tu1 (in frame)
	       (constants (a i)
			  (f (i i i))
			  (b i)
			  )
		    
	       (assumption commutative
			   (forall (lam (x i)
					(forall (lam (y i)
						     (= (f x y) (f y x)))))))
	 
	       (conclusion CON (= (f a b) (f b a))))

(th~defproblem tu2 (in frame)
	       (constants (a i)
			  (f (i i i))
			  (b i)
			  (prop o)

			  )
		    
	       (assumption commutative
			   (forall (lam (x i)
					(forall (lam (y i)
						     (= (f x y) (f y x)))))))
	 
	       (assumption forget-me
			   prop)
	       
	       (conclusion CON (= (f a b) (f b a))))


(th~defproblem prop-and-comm3 (in frame)
	       (constants (H1 o)
			  (H2 o))
		    
	       (assumption A1 (and H1 H2))
	 
	       (conclusion CON H1))


(th~defproblem  ac-simple
           (in frame)
	       (constants
		(G ( o i))
		(h ( o i))
		(op (i i i))
		(a i)(b i)(c i))
	    
	       (assumption a1 (commutative G op))
	       (assumption a2 (associative G op))
	       
	       (conclusion (= (op a (op b c))
			      (op (op a c) b))))	    




(th~defproblem  image-finite
           (in frame)
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
	       
	       (conclusion (subgroup (image fun G)
				     H oph)))		    


(th~defproblem  image-subgroup
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph  fun))
	       
	       (conclusion (subgroup (image fun G) 
					 H oph)))		    


(th~defproblem  image-closed
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(fun ((i i) i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism  g opg h oph  fun))
	       
	       (conclusion   (CLOSED-UNDER       
				  (IMAGE FUN G)                              
				  OPH)))                                                 


(th~defproblem  image-closed-ring
           (in frame)
	       (constants
		(G ( o i))
		(opG+ (i i i))
		(opH+ ((i i)(i i)(i i)))
		(opG* (i i i))
		(opH* ((i i)(i i)(i i)))
		(H (o ( i i)))
		(fun ((i i) i)))
	    
	       (assumption a1 (ring G opg+ opg* ))
	       (assumption a2 (ring H oph+ oph*))
	       (assumption a3 (ring-homomorphism  g opg+ opg* h oph+ oph*  fun))
	       
	       (conclusion   (CLOSED-UNDER       
				  (IMAGE FUN G)                              
				  OPH+)))                                                 

(th~defproblem  image-closed-group4
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(eg i)
		(invg (i i))
		(opH ((i i)(i i)(i i)))
		(eh (i i))
		(invh ((i i)(i i)))
		(H (o ( i i)))
		(fun ((i i) i)))
	    
	       (assumption a1 (group4 G opg eg invg))
	       (assumption a2 (group4 H oph eh invh))
	       (assumption a3 (homomorphism  g opg h oph  fun))
	       
	       (conclusion   (CLOSED-UNDER       
				  (IMAGE FUN G)                              
				  OPH)))                                                 



(th~defproblem  image-assoc
           (in frame)
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
				 (IMAGE FUN G)                              
				 OPH)))                                                
(th~defproblem image-notempty
           (in frame)
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
				 (IMAGE FUN G)                              
				 )))                                                

(th~defproblem  image-neutinv
           (in frame)
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
					     (and (unit (image fun g) oph e)
						  (inverse-exist (IMAGE FUN G) oph e)))
					(IMAGE FUN G))))

(th~defproblem  image-commutative
           (in frame)
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
							     (IMAGE FUN G)))
					(IMAGE FUN G))
			   ))


(th~defproblem  image-neut-ring
           (in frame)
	       (constants
		(G ( o i))
		(opG+ (i i i))
		(opH+ ((i i)(i i)(i i)))
		(opG* (i i i))
		(opH* ((i i)(i i)(i i)))
		(H (o ( i i)))
		(fun ((i i) i)))
	    
	       (assumption a1 (ring G opg+ opg* ))
	       (assumption a2 (ring H oph+ oph*))
	       (assumption a3 (ring-homomorphism  g opg+ opg* h oph+ oph*  fun))
	       
	       (conclusion  
			   (exists-sort (lam (e ( i i))
					     (left-unit (image fun g) oph+ e))
					(IMAGE FUN G))))



(th~defproblem  image-neut-simple
           (in frame)
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
					     (left-unit (image fun g) oph e))
					(IMAGE FUN G))))


(th~defproblem  image-inv-simple
           (in frame)
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
			   (left-inverse-exist (IMAGE FUN G) oph (fun (group-unit g opg)))))
				

(th~defproblem  image-neutinv-simple
           (in frame)
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
					     (and (left-unit (image fun g) oph e)
						  (left-inverse-exist (IMAGE FUN G) oph e)))
					(IMAGE FUN G))))



(th~defproblem  kernel-subgroup
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion (subgroup (urbild fun G
							  (group-unit H oph))
					 g opg))) 

(th~defproblem  kernel-neutinv-simple
           (in frame)
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
						   (urbild fun G (group-unit H oph))
						   opg e)
						  (left-inverse-exist
						   (urbild fun G (group-unit H oph))
						   opg e)))
					(urbild fun G (group-unit H oph)))))

(th~defproblem  kernel-neut-simple
           (in frame)
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
					     (left-unit (urbild FUN G (group-unit H oph)) opg e))
					(urbild FUN G (group-unit H oph)))))

(th~defproblem  kernel-closed
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion   (CLOSED-UNDER       
				  (urbild fun G 
						   (group-unit H oph))
				  OPg)))

(th~defproblem  kernel-assoc
           (in frame)
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
				  (urbild fun G
						   (group-unit H oph))
				  OPg)))


(th~defproblem  deussen-simple1
           (in frame)
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
					       (IMAGE FUNh G)  oph
					       (IMAGE FUNk G)  opk  phi)))
								    

(th~defproblem  deussen-simple2
           (in frame)
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
					       (IMAGE FUNh G)  oph
					       k  opk  phi)))

(th~defproblem  deussen
           (in frame)
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
           (in frame)
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
           (in frame)
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
			   (left-inverse-exist (IMAGE FUN G) oph (fun (group-unit g opg)))))

(th~defproblem reflex-test2
           (in frame)
	       (constants
		(G ( o num))
		(opG (num num num))
		(opH (num num num))
		(a num)
		(b num)
		(H ( o num))
		(fun (num num)))

	    
	       
	       (assumption a4 (nat a))

	       
	       (conclusion  test (= (plus a one) one)))



(th~defproblem reflex-test1
           (in frame)
	       (constants
		(G ( o num))
		(opG (num num num))
		(opH (num num num))
		(a num)
		(b num)
		(H ( o num))
		(fun (num num)))

	    
	       
	       (assumption a4 (nat a))

	       
	       (conclusion test (= a a)))



(th~defproblem  kernel-inv-simple
           (in frame)
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
			    (urbild fun G (group-unit H oph))
			    opg (group-unit g opg))))

(th~defproblem  kernel-iso
           (in frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       (assumption a4  (= (urbild fun G (group-unit H oph))
				  (lam (x i) (= x (group-unit G opg)))))
	       (conclusion  
			   (injective G fun )))






(th~defproblem  singleton-closed
           (in  frame)
	       (constants
		(G ( o i))
		(opG (i i i))
		(opH ((i i)(i i)(i i)))
		(H (o ( i i)))
		(e i)
		(g1 i)
		(g2 i)
		(fun ((i i) i)))
	       (conclusion   (CLOSED-UNDER       
				  (lam (x i) (= x e))
				  (lam (x i)(lam (y i) e)))))

(th~defproblem  singleton-assoc
           (in  frame)
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
           (in  frame)
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
           (in  frame)
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
           (in  frame)
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
	       (conclusion   (CLOSED-UNDER       
				  (lam (x i) (or (= x g1) (= x g2)))
				  op)))




(th~defproblem  test-fgroup-neut
           (in frame)
	       (constants
		(G ( o i))
		(a i)
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion test
			   (= a (opg a (group-unit g opg)))))

(th~defproblem  test-fgroup-neut-mv
           (in frame)
	       (constants
		(G ( o i))
		(a i)
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion test
			   (exists (lam (y i)
			   (exists (lam (x i)
				   (= y (opg y x))))))))


(th~defproblem  test-fgroup-inv
           (in frame)
	       (constants
		(G ( o i))
		(a i)
		(opG (i i i))
		(opH (i i i))
		(H ( o i))
		(fun (i i)))
	    
	       (assumption a1 (group G opg))
	       (assumption a2 (group H oph))
	       (assumption a3 (homomorphism g opg h oph fun))
	       
	       (conclusion test
			   (= (opg (group-inverse g opg a) a)  (group-unit g opg))))
