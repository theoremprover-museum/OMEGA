(th~deftheory abelian-ring
	      (uses ring)
	      (constants (colon (all-types aa bb ((o bb) (o aa) (o aa) (struct bb))))))

(th~defdef prime-ideal
	   (in abelian-ring)
	   (type-variables bb)
           (definition 
             (lam (P (o bb))
		  (lam (R (o bb))
		       (and (and (ideal P R)
				 (not (= P R)))
			    (forall (lam (x bb)
					 (forall (lam (y bb)
						      (implies (and (and (in x R) (in y R))
								    (in (ring-times R x y) P))
							       (or (in x P)
								   (in y P)))))))))))
	   
	   
	   (help "(prime-ideal P R) is true, iff P is a prime-ideal of R."))



(th~defdef maximal-ideal
	   (in abelian-ring)
	   (type-variables bb)
           (definition 
             (lam (M (o bb))
		  (lam (R (struct bb))
		       (and (and (ideal M R)
				   (not (= M R)))
			    (forall (lam (N (o bb))
					 (implies (and (ideal N R)
						       (proper-subset M N))
						  (= N (struct-set R)))))))))
	   
	   (help "(maximal-ideal M R) is true, iff M is a maximal-ideal of R."))


(th~defdef cfield
	   (in abelian-ring)
	   (type-variables aa)
           (definition
	     (lam (S (o aa))
		  (lam (add (aa aa aa))
		       (lam (mul (aa aa aa))
			   (and (abelian-group S add)
				(abelian-group (setminus S (singleton (group-unit S add))) mul))))))
	   (help "The definition of a (commutative) field."))


(th~defaxiom colon
	     (in abelian-ring)
	     (formula 
	      (forall (lam (A (o aa)) (forall (lam (M (o aa)) (forall (lam (R (struct bb))
									   (implies (submodule A M R)
										    (= (colon A M R)
										       (lam (rr bb) (and (R rr) (forall (lam (mm aa) (impl (M mm) (A (rmmul rr mm))))))))))))))))
	     
	     (help "Definition of colon (A:M) with respect to ring R."))

