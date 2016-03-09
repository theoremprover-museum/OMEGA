(th~defdef pair2
           (in typed-set)
           (type-variables aa bb)
           (definition
             (lam (x aa)
                  (lam (y bb)
                       (lam (p (o bb aa)) (p x y)))))
           (help "Pairing function as introduced in Andrews86, p. 185 -- Andrews02, p. 237 (X5307)"))

(th~defdef cartesian-product2
           (in typed-set)
           (type-variables aa bb)
           (definition (lam (A (o aa))
			    (lam (B (o bb))
				 (lam (P (o (o bb aa)))
				     (exists-sort (lam (x aa)
						       (exists-sort (lam (y bb)
									 (= p (pair2 x y)))
								    B))
						  A)))))
           (help "Cartesian Product"))

(th~defdef relation-like2
	   (in relation)
           (type-variables aa bb)
           (definition (lam (R (o (o (o bb aa))))
			    (lam (U (o aa))
				 (lam (V (o bb))
				      (subset R (cartesian-product2 U V)))))))


(th~defdef rel-domain2
	   (in relation)
           (type-variables aa bb)
           (definition 
             (lam (rel (o (o (o bb aa))))
                  (lam (x aa)
                       (exists (lam (y bb) (rel (pair2 x y)))))))
	   (help "Definition of the domain function relations.
                  (rel-domain R) is the set of x such that Rxy for some y."))



(th~defdef rel-codomain2
	   (in relation)
           (type-variables aa bb)
           (definition 
             (lam (rel (o (o (o bb aa))))
                  (lam (y bb)
                       (exists (lam (x aa) (rel (pair2 x y)))))))
	   (help "Definition of the codomain function relations.
                  (rel-domain R) is the set of y such that Rxy for some x."))


(th~defdef equivalence-classes2
           (in relation)
           (type-variables bb)
           (definition
             (lam (rel1 (o (o (o bb bb))))
		  (lam (set1 (O bb))
		       (exists-sort (lam (elem1 bb)
					 (forall (lam (elem2 bb)
						      (equiv (set1 elem2)
							     (rel1 (pair2 elem1 elem2))))))
				    set1))))
	   (help "The equivalence-classes relation. Input: a relation. Result: The set of
all equivalence classes with respect to the input relation"))
			 

(th~defdef reflexive2
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o (o (o bb bb))))
                  (forall (lam (x bb)
                               (rel (pair2 x x))))))
	   (help "Definition of the predicate for reflexive relations.
                  (reflexive R) is true, iff Rxy for some y implies Rxx."))



(th~defdef symmetric2
	   (in relation)
           (type-variables bb)
           (definition
             (lam (rel (o (o (o bb bb))))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (implies (rel (pair2 x y))
                                                     (rel (pair2 y x)))))))))
	   (help "Definition of the predicate for symmetric relations.
                  (symmetric R) is true, iff Rxy implies Ryx."))

(th~defdef transitive2
	   (in relation)
           (type-variables bb)
	   (definition 
             (lam (rel (o (o (o bb bb))))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (forall (lam (z bb)
 			    (implies (and (rel (pair2 x y)) (rel (pair2 y z))) (rel (pair2 x z)))))))))))
	   (help "Definition of the predicate for transitivity. (transitive R) is true, iff Rxy and Ryz imply Rxz."))


(th~defdef eqrel2
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o (o (o bb bb))))
                  (and (and (reflexive2 rel)
                            (symmetric2 rel))
                       (transitive2 rel))))
	   (help "Definition of the predicate for equivalence relations."))

(th~defdef field2
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (R (o (o (o bb bb))))
		  (union (rel-domain2 R) (rel-codomain2 R))))
	   (help "Definition of the field of a relation."))


(th~defdef id2
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (X (o bb))
		  (that (lam (rel (o (o (o bb bb))))
			     (forall (lam (xs bb)
					  (forall (lam (ys bb)
						       (equiv (rel (pair2 xs
									 ys))
							      (and (X xs)
								   (= xs
								      ys)))))))))))
	   (help "Definition of the functor that returns the identity
relation defined over the input set X."))


(th~defdef restrict-rel-domain2
	   (in relation)
           (type-variables bb aa)
           (definition 
             (lam (rel (o (o (o bb aa))))
		  (lam (X (o aa))
		       (that (lam (rel2 (o (o (o bb aa))))
				  (forall (lam (xs aa)
					       (forall (lam (ys bb)
							    (equiv (rel2 (pair2 xs
									       ys))
								   (and (X xs)
									(rel (pair2 xs ys)))))))))))))
	   (help "Restriction of a relation wrt domain."))

(th~defdef restrict-rel-codomain2
	   (in relation)
           (type-variables bb aa)
           (definition 
	     (lam (rel (o (o (o bb aa))))
		  (lam (Y (o bb))
		       (that (lam (rel2 (o (o (o bb aa))))
				  (forall (lam (xs aa)
					       (forall (lam (ys bb)
							    (equiv (rel2 (pair2 xs
									       ys))
								   (and (Y ys)
									(rel (pair2 xs ys)))))))))))))
	   (help "Restriction of a relation wrt codomain."))


(th~defdef rel-composition2
	   (in relation)
           (type-variables cc bb aa)
           (definition 
	     (lam (rel1 (o (o (o bb aa))))
		  (lam (rel2 (o (o (o cc bb))))
		       (that (lam (rel3 (o (o (o cc aa))))
				  (forall (lam (xs aa)
					  (forall (lam (zs cc)
						       (equiv (rel3 (pair2
								     xs
								     zs))
							      (exists (lam
								       (ys bb)
								   (and
								    (rel1
								     (pair2
								      xs
								      ys))
								    (rel2
								     (pair2
								      ys zs)))))))))))))))
	   (help "Composition of relations."))



;;; solved with NIC Agents!
;;; standard setting!
;;; NICTAC-WEAKEN NICTAC-FOWEAKEN NIC-FORALL-I NIC-EXISTS-E-I NIC-EXISTS-E-E NIC-FORALL-E NIC-EXISTS-I SET-EXT-EXPAND SET-EXT-CONTRACT DEFN-CONTRACT* NICTAC-EQUIV-I NIC-IMP-I NIC-AND-I NIC-OR-I-L NIC-OR-I-R NIC-NEG-I THAT-I THAT-E NICDUMMY-OR-E-R NICDUMMY-OR-E-L NICTAC-AND-E-R NICTAC-AND-E-L NICTAC-IMP-E NICTAC-MODTOLL NICTAC-NEG-E NICTAC-OR-E DEFN-EXPAND* NIC-FALSE-C COUNTEREXAMPLE-BY-SATCHMO SOLVED-BY-PL-ATP 
(th~defproblem set-640+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (relation-like2 R X Y)
								(forall (lam (A  (o (o (o b a))))
									     (implies (subset A R)
										      (subset A (cartesian-product2 X Y)))))))))))))
	       (help "A a subset of R (X to Y) => A a subset of X x
Y. (Mizar: RELSET_1 (2))"))




;;; Leo + Bliksem
(th~defproblem set-646+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (X (o a))
			     (forall (lam (Y (o b))
					  (forall-sort (lam (xs a)
							    (forall-sort (lam (ys b)
									      (relation-like2 (singleton (pair2 xs ys)) X Y))
									 Y))
						       X))))))
	       (help "If x is in X and y is in Y then { (x,y) } is a relation from X to Y. (Mizar: RELSET_1 (8))"))



(th~defproblem set-647+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (and
								 (relation-like2 R X Y)
								 (subset
								  (rel-domain2 R) X))
								(relation-like2 R X (rel-codomain2 R)))))))))))

(th~defproblem set-648+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (and
								 (relation-like2 R X Y)
								 (subset
								  (rel-codomain2 R) Y))
								(relation-like2 R (rel-domain2 R) Y))))))))))

(th~defproblem set-649+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (and
								 (relation-like2 R X Y)
								 (and
								  (subset (rel-domain2 R) X)
								  (subset (rel-codomain2 R) Y)))
								(relation-like2 R (rel-domain2 R) (rel-codomain2 R)))))))))))



(th~defproblem set-651+3-2
	       (in relation)
	       (type-constants a b)
	       (constants (x1 (o a)))
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (implies (and
								 (relation-like2 R X Y)
								 (subset (rel-domain2 R) X1))
								(relation-like2 R X1 (rel-codomain2 R)))))))))))



(th~defproblem set-657+3-2
	       (in relation)
	       (type-constants a)
	       (conclusion
		(forall (lam (R  (o (o (o a a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o a))
						       (implies (relation-like2 R X Y)
								(subset
								 (field2 R)
								 (union X Y)))))))))))




(th~defproblem set-669+3-2
	       (in relation)
	       (type-constants a)
	       (conclusion
		(forall (lam (R  (o (o (o a a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o a))
						       (implies (and (relation-like2 R X Y)
								     (subset (id2 Y) R))
								(and
								 (subset Y
									 (rel-domain2 R))
								 (= Y
								    (rel-codomain2 R))))))))))))

(th~defproblem set-671+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (forall (lam (Z (o a))
								    (implies (and (relation-like2 R X Y)
										  (subset X Z))
									     (=
									      (restrict-rel-domain2 R Z) R))))))))))))


(th~defproblem set-673+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (X (o a))
					  (forall (lam (Y (o b))
						       (forall (lam (Z (o b))
								    (implies (and (relation-like2 R X Y)
										  (subset Y Z))
									     (=
									      (restrict-rel-codomain2 R Z) R))))))))))))


(th~defproblem set-680+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (D (o a))
					  (forall (lam (E (o b))
						       (implies (and (relation-like2 R D E)
								     (and
								      (not
								       (= D
									  emptyset))
								      (not
								       (= E
									  emptyset))))
								(forall-sort
								 (lam (xs a)
								      (equiv ((rel-domain2 R) xs)
									     (exists-sort (lam (ys b)
											       (R (pair2 xs ys)))
											  E)))
								 D))))))))))
											    
(th~defproblem set-683+3-2
	       (in relation)
	       (type-constants a b)
	       (conclusion
		(forall (lam (R  (o (o (o b a))))
			     (forall (lam (D (o a))
					  (forall (lam (E (o b))
						       (implies (and (relation-like2 R D E)
								     (and
								      (not
								       (= D
									  emptyset))
								      (not
								       (= E
									  emptyset))))
								(forall-sort
								 (lam (ys b)
								      (implies ((rel-codomain2 R) ys)
									       (exists-sort (lam (xs a)
												 ((rel-domain2 R) xs))
											    D)))
								 E))))))))))

;; None....
(th~defproblem set-770+4-2
	 (in relation)
	 (type-constants a)
	 (conclusion 
	  (forall (lam (R (o (o (o a a))))
		       	  (forall (lam (Q (o (o (o a a))))
				       (implies (and (eqrel2 R)
						     (eqrel2 Q))
						(or (= (equivalence-classes2 R)
						       (equivalence-classes2 Q))
						    (disjoint (equivalence-classes2 R)
							      (equivalence-classes2 Q)))))))
	  )))




