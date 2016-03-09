(in-package :omega)


(th~deftheorem maxmal-implies-prime-ideal
	       (in abelian-ring)
	       (conclusion
		(all-types aa bb
		 (forall (lam (M (o bb))
		  (forall (lam (P (o bb))
		   (forall (lam (R (o bb))
		    (implies (maximal-ideal M R)
                             (prime-ideal M R)))))))))))
