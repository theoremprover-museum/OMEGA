(oc=require-theory 'typed-set)
(oc=require-theory 'relation)
(oc=require-theory 'function)

(th~defdef un-ord-pair3
            (in typed-set)
            (type-variables aa)
            (definition
              (lam (x aa)
                   (lam (y aa)
                        (lam (u aa)
			     (or (= u x) (= u y))))))
            (help "Def unordered pairs."))

(th~defdef singleton3
            (in typed-set)
            (type-variables aa)
            (definition
              (lam (x aa)
		   (lam (u aa)
			(= u x))))
            (help "Def singleton set."))


(th~defdef pair-rel3
            (in typed-set)
            (type-variables aa bb)
            (definition
              (lam (x aa)
                   (lam (y bb)
                        (lam (u aa)
			     (lam (v bb) (and (= u x) (= v y)))))))
            (help "The relation {<x,y>}."))



(th~defdef cartesian-product3
	   (in relation)
           (type-variables aa bb)
           (definition 
             (lam (X (o aa))
		  (lam (Y (o bb))
		       (lam (xs aa) (lam (ys bb) (and (X xs) (Y ys)))))))
	   (help "Cartesian product of two sets."))

(th~defdef rel-domain3
	   (in relation)
           (type-variables aa bb)
           (definition 
             (lam (R (o bb aa))
		       (lam (x aa)
			    (exists (lam (y bb) (R x y))))))
	   (help "Definition of the domain of relations."))


(th~defdef rel-codomain3
	   (in relation)
           (type-variables aa bb)
           (definition 
             (lam (R (o bb aa))
		       (lam (y bb)
			    (exists (lam (x aa) (R x y))))))
	   (help "Definition of the codomain of relations."))



(th~defdef equivalence-classes3
           (in relation)
           (type-variables bb)
           (definition
             (lam (rel1 (o bb bb))
		  (lam (set1 (O bb))
		       (exists (lam (elem1 bb)
				    (and (set1 elem1)
					   (forall (lam (elem2 bb)
							(equiv (set1 elem2)
							       (rel1 elem1 elem2))))))))))
	   (help "The equivalence-classes relation. Input: a relation. Result: The set of
all equivalence classes with respect to the input relation"))
			 

(th~defdef reflexive3
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (rel x x)))))
	   (help "Definition of the predicate for reflexive relations."))



(th~defdef symmetric3
	   (in relation)
           (type-variables bb)
           (definition
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (implies (rel x y)
                                                     (rel y x))))))))
	   (help "Definition of the predicate for symmetric relations."))

(th~defdef transitive3
	   (in relation)
           (type-variables bb)
	   (definition 
             (lam (rel (o bb bb))
                  (forall (lam (x bb)
                               (forall (lam (y bb)
                                            (forall (lam (z bb)
							 (implies (and (rel x y) (rel y z)) (rel x z))))))))))
	   
	   (help "Definition of the predicate for transitivity."))


(th~defdef eqrel3
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (rel (o bb bb))
                  (and (and (reflexive3 rel)
                            (symmetric3 rel))
                       (transitive3 rel))))
	   (help "Definition of the predicate for equivalence relations."))

(th~defdef field3
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (R (o bb bb))
		  (union (rel-domain3 R) (rel-codomain3 R))))
	   (help "Definition of the field of a relation."))


(th~defdef id3
	   (in relation)
           (type-variables bb)
           (definition 
             (lam (X (o bb))
		  (lam (xs bb)
		       (lam (ys bb)
			    (and (X xs) (= xs ys))))))
	   (help "Definition of the functor that returns the identity relation defined over the input set X."))


(th~defdef restrict-rel-domain3
	   (in relation)
           (type-variables bb aa)
           (definition 
             (lam (rel (o bb aa))
		  (lam (X (o aa))
		       (lam (xs aa)
			    (lam (ys bb)
				 (and (X xs) (rel xs ys)))))))
	   (help "Restriction of a relation wrt domain."))

(th~defdef restrict-rel-codomain3
	   (in relation)
           (type-variables bb aa)
           (definition 
             (lam (rel (o bb aa))
		  (lam (Y (o bb))
		       (lam (xs aa)
			    (lam (ys bb)
				 (and (Y ys) (rel xs ys)))))))
	   (help "Restriction of a relation wrt codomain."))


(th~defdef rel-composition3
	   (in relation)
           (type-variables cc bb aa)
           (definition 
	     (lam (rel1 (o bb aa))
		  (lam (rel2 (o cc bb))
		       (lam (xs aa)
			    (lam (zs cc)
				 (exists (lam (ys bb)
					      (and (rel1 xs ys) (rel2 ys zs)))))))))
	   (help "Composition of relations."))


(th~defdef subrel3
	   (in relation)
           (type-variables bb aa)
           (definition 
	     (lam (rel1 (o bb aa))
		  (lam (rel2 (o bb aa))
		       (forall (lam (xs aa)
			    (forall (lam (ys bb)
					 (implies (rel1 xs ys) (rel2 xs ys)))))))))
	   (help "Definition of subrelation predicate."))


(th~defdef domain-of-codomain-restricted-rel3
	   (in relation)
           (type-variables bb aa)
           (definition 
	     (lam (rel (o bb aa))
		  (lam (Y (o bb))
		       (lam (xs aa)
			    (exists-sort (lam (ys bb) (rel xs ys))
					 Y)))))
	   (help "Definition of the functor domain-of-codomain-restricted-rel."))

(th~defdef is-rel-on3
	   (in relation)
           (type-variables bb aa)
           (definition 
	     (lam (rel (o bb aa))
		  (lam (X (o aa))
		       (lam (Y (o bb))
			    (forall (lam (xs aa)
					 (forall (lam (ys bb)
						      (implies (rel xs ys)
							       (and (X xs)
								    (Y ys)))))))))))
	   (help "Definition of the predicate is relation on."))

(th~defdef inverse-image-of-function3
	   (in function)
           (type-variables bb aa)
           (definition 
	     (lam (f (bb aa))
		  (lam (Y (o bb))
		       (lam (xs aa) (exists (lam (ys bb) (and (Y ys) (= ys (f xs)))))))))
	   (help "Inverse image of a function."))




(th~defproblem set-014+4-3
	       (in typed-set)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  (o aa))
			     (forall (lam (Y  (o aa))
					  (forall (lam (A (o aa))
						       (implies (and (subset X A)
								     (subset Y A))
								(subset (union X Y) A)))))))))
	       (help "Union of subsets is a subset."))




(th~defproblem set-017+1-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  aa)
			     (forall (lam (Y  aa)
					  (forall (lam (Z  aa)
						       (implies (= (un-ord-pair3 X Y) (un-ord-pair3 X Z))
								(= Y Z)))))))))
	       (help "Left cancellation in unordered pairs."))

(th~defproblem set-066+1-3 (in relation) (type-constants aa bb) (conclusion (forall (lam (X  aa)			     (forall (lam (Y  aa) (= (un-ord-pair3 X Y) (un-ord-pair3 Y X))))))))

(th~defproblem set-067+1-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  aa)
			     (forall (lam (Y  aa)
					  (subset (un-ord-pair3 X X) (un-ord-pair3 X Y))))))))


(th~defproblem set-076+1-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  aa)
			     (forall (lam (Y  aa)
					  (forall (lam (Z  (o aa))
						       (implies (and (in X Z) (in Y Z))
								(subset (un-ord-pair3 X Y) Z))))))))))


(th~defproblem set-086+1-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  aa)
			     (exists (lam (Y  aa)
					  ((singleton3 X) Y)))))))

(th~defproblem set-096+1-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X  (o aa))
			     (forall (lam (Y  aa)
					  (implies (subset X (singleton3 Y))
						   (or (= X emptyset)
						       (= X (singleton3 Y))))))))))





(th~defproblem set-143+3-3
	       (in typed-set)
	       (type-constants aa)
	       (conclusion
		(forall (lam (X  (o aa))
			     (forall (lam (Y  (o aa))
					  (forall (lam (Z (o aa))
						       (= (intersection (intersection X Y) Z)
							  (intersection X (intersection Y Z)))))))))))



(th~defproblem set-143+4-3
	       (in typed-set)
	       (type-constants aa)
	       (conclusion
		(forall (lam (X  (o aa))
			     (forall (lam (Y  (o aa))
					  (forall (lam (Z (o aa))
						       (= (intersection (intersection X Y) Z)
							  (intersection X (intersection Y Z)))))))))))



(th~defproblem set-171+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa)) 
			   (forall (lam (Y (o aa)) 
			    (forall (lam (Z (o aa))
			       (= (union X (intersection Y Z))
                                  (intersection (union X Y)
                                                (union X Z)))))))))))


(th~defproblem set-580+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
                            (forall (lam (U aa)
 			       (equiv (in U (exclunion X Y))
                                      (equiv (in U X) 
                                             (not (in U Y))))))))))))


(th~defproblem set-601+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
                            (forall (lam (Z (o aa))
                               (= (union (intersection X Y) 
                                   (union (intersection Y Z) 
                                          (intersection Z X)))
                                  (intersection (union X Y)
                                   (intersection (union Y Z)
                                                 (union Z X))))))))))))


(th~defproblem set-606+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
			      (= (setminus X (intersection X Y))
                                 (setminus X Y))))))))

(th~defproblem set-607+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
                              (= (union X (setminus Y X))
                                 (union X Y))))))))


(th~defproblem set-609+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa)) 
			   (forall (lam (Y (o aa)) 
			    (forall (lam (Z (o aa))
                               (= (setminus X (setminus Y Z))
                                  (union (setminus X Y)
                                         (intersection X Z)))))))))))

(th~defproblem set-611+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa)) 
                              (equiv (= (intersection X Y) emptyset)
                                     (= (setminus X Y) X))))))))


(th~defproblem set-612+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa)) 
			   (forall (lam (Y (o aa)) 
                            (forall (lam (Z (o aa)) 
                               (= (setminus X (union Y Z))
                                  (intersection (setminus X Y)
                                                (setminus X Z)))))))))))


(th~defproblem set-614+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
                            (forall (lam (Z (o aa)) 
                               (= (setminus (setminus X Y) Z)
                                  (setminus X (union Y Z)))))))))))


(th~defproblem set-615+3-3
         (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
         (conclusion (forall (lam (X (o aa))
                           (forall (lam (Y (o aa))
                            (forall (lam (Z (o aa)) 
                               (= (setminus (union X Y) Z)
                                  (union (setminus X Z)
                                         (setminus Y Z)))))))))))

(th~defproblem test
	 (in typed-set)
         (author trybulec)
	 (type-constants a)
         (reference "mizar.bool-prop-set")
         (conclusion conc (forall (lam (X (o a))
                           (forall (lam (Y (o a))
                            (forall (lam (Z (o a)) 
                               (= (setminus (union X Y) Z)
                                  (union (setminus X Z)
                                         (setminus Y Z)))))))))))



(th~defproblem set-623+3-3 (in typed-set) (type-constants aa) (conclusion (forall (lam (X (o aa)) (forall (lam (Y (o aa)) (forall (lam (Z (o aa)) (= (exclunion (exclunion X Y) Z) (exclunion X (exclunion Y Z)))))))))))




(th~defproblem set-624+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
	 (type-constants aa)
	 (conclusion (forall (lam (X (o aa)) 
						     (forall (lam (Y (o aa)) 
								  (forall (lam (Z (o aa)) 
									       (equiv
										(meets X (union Y Z))
										(or (meets X Y)
										    (meets X Z)))))))))))


(th~defproblem set-630+3-3
	 (in typed-set)
         (author trybulec)
         (reference "mizar.bool-prop-set")
         (type-constants aa)
	 (conclusion (forall (lam (X (o aa))
						     (forall (lam (Y (o aa))
								  (misses (intersection X Y)
									  (exclunion X Y))))))))


(th~defproblem set-640+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R1  (o bb aa))
			     (forall (lam (R2  (o bb aa))
					  (implies (subrel3 R1 R2)
						   (subrel3 R1
							    (cartesian-product3
							     (lam (us aa) true)
							     (lam (vs bb) true)))))))))
	       (help "R1 a subrelation of R2 (X x Y) => R1 a subrelation of X x
Y. (Mizar: RELSET_1 (2))"))


(th~defproblem set-646+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (X (o aa))
		 (forall (lam (Y (o bb))
		  (forall-sort (lam (xs aa)
				    (forall-sort (lam (ys bb)
						      (subrel3 (pair-rel3 xs ys)
							       (cartesian-product3
								(lam (us aa) true)
								(lam (vs bb) true))))
						 Y))
			       X))))))
	       (help "If x is in X and y is in Y then { (x,y) } is a relation from X to Y. (Mizar: RELSET_1 (8))"))


(th~defproblem set-647+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R (o bb aa))
			     (forall (lam (X (o aa))
					  (implies 
					   (subset (rel-domain3 R) X)
					   (subrel3 R (cartesian-product3 X (rel-codomain3 R)))))))))
	       (help "For every binary relation R such that domain(R) subset X holds R is a relation between X and codomain(R). (MIZAR RELSET_1 (9))")) 


(th~defproblem set-648+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R (o bb aa))
			     (forall (lam (Y (o bb) )
					  (implies 
					   (subset (rel-codomain3 R) Y)
					   (subrel3 R (cartesian-product3 (rel-domain3 R) Y))))))))
	       (help "For every binary relation R such that codomain(R) subset Y holds R is a relation between domain(R) and Y. (MIZAR RELSET_1 (10))")) 

(th~defproblem set-649+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R (o bb aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o bb))
						       (implies 
							(and (subset (rel-domain3 R) X)
							     (subset (rel-codomain3 R) Y))
							(subrel3 R (cartesian-product3 X Y))))))))))
	       (help "For every binary relation R such that domain(R) subset X and codomain(R) subset Y holds R is a relation between X and Y. (MIZAR RELSET_1 (11))")) 
		


(th~defproblem set-651+3-3
	       (in relation)
	       (type-constants aa bb)
	       (constants (x1 (o aa)))
	       (conclusion
		(forall (lam (R (o bb aa))
			     (implies (subset (rel-domain3 R) X1)
				      (subrel3 R (cartesian-product3 X1 (lam (xs bb) true)))))))
	       (help "For every binary relation R such that domain(R) subset X1 holds R is a relation between X1 and Y. (MIZAR RELSET_1 (13))")) 


(th~defproblem set-657+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R (o aa aa))
			     (subset (field3 R) (union (lam (xs aa) true)  (lam (ys aa) true))))))
	       (help "Field(R) subset X union Y. (MIZAR RELSET_1 (19))")) 




(th~defproblem set-669+3-3
	       (in relation)
	       (type-constants aa)
	       (conclusion
		(forall (lam (R  (o aa aa))
			     (implies (subrel3 (id3 (lam (ys aa) true)) R)
				      (and (subset (lam (ys aa) true) (rel-domain3 R))
					   (= (lam (ys aa) true) (rel-codomain3 R)))))))
	       (help "If identity-relation(Y) subset R then Y subset domain(R) and Y = codomain(R). (MIZAR RELSET_1 (32))")) 


(th~defproblem set-669+3-3-alt
	       (in relation)
	       (type-constants aa)
	       (conclusion
		(forall (lam (R  (o aa aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o aa))
						       (implies
							(and (is-rel-on3 R X Y)
							     (subrel3 (id3 Y) R))
							(and (subset Y (rel-domain3 R))
							     (= Y (rel-codomain3 R)))))))))))
	       (help "If identity-relation(Y) subset R then Y subset domain(R) and Y = codomain(R). (MIZAR RELSET_1 (32))"))


(th~defproblem set-670+3-3 (in relation) (type-constants aa bb) (constants (X1 (o aa))) (conclusion (forall (lam (R (o bb aa))		  (forall (lam (X (o aa))                    (forall (lam (Y (o bb))			(implies			 (is-rel-on3 R X Y)			 (is-rel-on3 (restrict-rel-domain3 R X1) X1 Y))))))))))


(th~defproblem set-671+3-3
	       (in relation)
	       (type-constants aa bb)
	       (constants (X1 (o aa)))
	       (conclusion
		(forall (lam (R (o bb aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o bb))
						       (implies
							(and (is-rel-on3 R X Y)
							     (subset X X1))
							(= (restrict-rel-domain3 R X1) R)))))))))
	       (help "If X subset X1 then restrict-relation-wrt-domain(R,X1) = R. (MIZAR RELSET_1 (34))"))


(th~defproblem set-672+3-3	       (in relation)  (type-constants aa bb)	       (constants (Y1 (o bb)))	       (conclusion		(forall (lam (R (o bb aa))		  (forall (lam (X (o aa))                    (forall (lam (Y (o bb))			(implies			 (is-rel-on3 R X Y)			 (is-rel-on3 (restrict-rel-codomain3 R Y1) X Y1))))))))))



(th~defproblem set-673+3-3
	       (in relation)
	       (type-constants aa bb)
	       (constants (Y1 (o bb)))
	       (conclusion
		(forall (lam (R (o bb aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o bb))
						       (implies (and
								 (is-rel-on3 R X Y)
								 (subset Y Y1))
								(= (restrict-rel-codomain3 R Y1) R)))))))))
	       (help "If Y subset Y1 then restrict-relation-wrt-codomain(R,Y1) = R. (MIZAR RELSET_1 (36))")) 


(th~defproblem set-680+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R  (o bb aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o bb))
						       (implies (is-rel-on3 R X Y)
								(forall-sort (lam (xs aa)
										  (equiv ((rel-domain3 R) xs)
											 (exists-sort (lam (ys bb)
													   (R xs ys))
												      Y)))
									     X)))))))))
	       (help "For every element xs of X holds x in domain(R) iff there exists an element ys of Y such that <xs, ys> in R. (MIZAR RELSET_1 (47)."))


(th~defproblem set-683+3-3
	       (in relation)
	       (type-constants aa bb)
	       (conclusion
		(forall (lam (R  (o bb aa))
			     (forall (lam (X (o aa))
					  (forall (lam (Y (o bb))
						       (implies
							(is-rel-on3 R X Y)
							(forall-sort (lam (ys bb)
									  (implies ((rel-codomain3 R) ys)
										   (exists-sort (lam (xs aa)
												     ((rel-domain3 R) xs))
												X)))
								     Y)))))))))
	       (help "For every element ys of Y holds ys in codomain(R) iff there exists an element xs of X such that xs in domain(R). (MIZAR RELSET_1 (50)."))




(th~defproblem set-684+3-3
	       (in relation)
	       (type-constants aa bb cc)
	       (conclusion
		(forall (lam (P  (o bb aa))
			     (forall (lam (R  (o cc bb))
					  (forall (lam (xs aa)
						       (forall (lam (zs cc)
								    (equiv ((rel-composition3 P R) xs zs)
									   (exists (lam (ys bb)
											(and (P xs ys)
											     (R ys zs))))))))))))))
	       (help "Let P be a relation between A and B, R be a relation between B and C, xs be an element of A, and zs be an element of C. Then <xs,yz> in compose-rel(P,R) if and only if there exists an element ys of B such that <xs,ys> in P and <ys,zs> in R. (MIZAR RELSET_1 (51)."))



(th~defproblem set-686+3-3
	       (in relation)
	       (type-constants aa bb)
	       (constants (D2 (o bb)))
	       (conclusion
		(forall (lam (R  (o bb aa))
			     (forall (lam (xs aa)
					  (equiv ((domain-of-codomain-restricted-rel3 R D2) xs)
						 (exists (lam (ys bb)
							      (and (R xs ys) (D2 ys))))))))))
	       (help "xs in domain-of-codomain-restricted-rel(R,D2) iff there there exists an element ys of Y such that <xs,ys> in R and ys in D2. MIZAR RELSET_1 (53)."))





(th~defproblem set-716+4-3
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion 
		(forall (lam (f (bb aa))
			     (forall (lam (g (cc bb))
					  (implies 
					   (and (injectivep f)
						(injectivep g))
					   (injectivep (compose-functions f g)))))))))
							   



(th~defproblem set-724+4-3       ;; interesting: Leo does not find proof, Bliksem does.
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion 
			   (forall (lam (f (bb aa))
					(forall (lam (g (cc bb))
						     (forall (lam (h (cc bb))
								  (implies 
								   (and (= (compose-functions f g)
									   (compose-functions f h))
									(surjectivep-mateja-chris f))
								   
								   (= g h))))))))))


(th~defproblem set-741+4-3       
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion 
		 (forall (lam (f (bb aa))
		   (forall (lam (g (cc bb))
		     (forall (lam (h (aa cc))
		       (implies 
			(and (injectivep (compose-functions
					  (compose-functions f g) h))
			     (and (surjectivep-mateja-chris
				   (compose-functions
					  (compose-functions g h) f))
				  (surjectivep-mateja-chris
				   (compose-functions
					  (compose-functions h f) g))))
			(bijectivep-mateja-chris h))))))))))
				  



(th~defproblem set-747+4-3
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion
			   (forall (lam (f (bb aa))
					(forall (lam (g (cc bb))
						     (forall (lam (ord1 (o aa aa))
								  (forall (lam (ord2 (o bb bb))
									       (forall (lam (ord3 (o cc cc))
		(implies
		 (and (increasing-wrt-ord f ord1 ord2)
		      (decreasing-wrt-ord g ord2 ord3))
		 (decreasing-wrt-ord (compose-functions f g) ord1 ord3))))))))))))))

(th~defproblem set-752+4-3       ;; interesting: Bliksem is faster than Leo alone....
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion
			   (forall (lam (X (o aa))
					(forall (lam (Y (o aa))
						     (forall (lam (F (bb aa))
                (= (function-domain-to-range F (union X Y))
		   (union (function-domain-to-range F X) (function-domain-to-range F Y)))))))))))

(th~defproblem set-753+4-3       ;; interesting: Bliksem is faster than Leo alone....
	       (in function)
	       (type-constants aa bb cc)
	       (conclusion (forall (lam (X (o aa))
					(forall (lam (Y (o aa))
						     (forall (lam (F (bb aa))
                (subset (function-domain-to-range F (intersection X Y))
			(intersection (function-domain-to-range F X) (function-domain-to-range F Y)))))))))))



(th~defproblem set-764+4-3
                (in function)
                (type-constants aa bb cc)
                (conclusion 
                 (forall (lam (f (bb aa))
			      (= (inverse-image-of-function3 f emptyset)
				 emptyset))))
		(help "The inverse image of empty set is empty."))

(th~defproblem set-770+4-3
	 (in relation)
	 (type-constants aa)
	 (conclusion 
	  (forall (lam (R (o aa aa))
		       (forall (lam (Q (o aa aa))
				    (implies (and (eqrel3 R)
						  (eqrel3 Q))
					     (or (= (equivalence-classes3 R)
						    (equivalence-classes3 Q))
						 (disjoint (equivalence-classes3 R)
							   (equivalence-classes3 Q)))))))))
	 (help "If R a nd Q are equivalence relations then the equivalence classes of R and Q are equal or disjoint."))



(th~defproblem chris1
                (in function)
                (type-constants aa bb cc)
                (conclusion 
                 (forall (lam (X (o aa))
			      (forall (lam (Y (o aa))
					   (exists (lam (P (o aa))
							(and (subset P X)
							     (and (subset P Y)
								  (and (subset (intersection X Y) P)
								       (not (= (intersection X Y) P))))))))))))
		(help "The inverse image of empty set is empty."))



;; ;;; A list of all problems:
;; ;;; (setq problems '(set-014+4-3 set-017+1-3 set-067+1-3 set-086+1-3 set-096+1-3 set-143+3-3 set-143+4-3 set-171+3-3 set-580+3-3 set-601+3-3 set-606+3-3 set-607+3-3 set-609+3-3 set-611+3-3 set-612+3-3 set-614+3-3 set-615+3-3 set-624+3-3 set-630+3-3 set-640+3-3 set-646+3-3 set-647+3-3 set-648+3-3 set-649+3-3 set-651+3-3 set-657+3-3 set-669+3-3 set-669+3-3-alt set-671+3-3 set-673+3-3 set-680+3-3 set-683+3-3 set-684+3-3 set-686+3-3 set-716+4-3 set-724+4-3 set-747+4-3 set-752+4-3 set-753+4-3 set-764+4-3 set-770+4-3 ))
;; ;;;

#|
;;;; don't do the following when you want Bliksem to tell us its time
(defun omega~message (&rest args) )

(defun blik~program ()
;;  "/bham/ums/solaris/pd/packages/omega-new/atp/bin/linux/bliksem-1.12")
  "/project/atp/bin/linux/bliksem-1.12")

(setf excl::*global-gc-behavior* nil)


;; (progn 
;;   (oc=require-complete-theory 'function)
;;   (oc=require-complete-theory 'relation)

;;   (defun omega~message (&rest args) )
  
;;   (defun blik~program ()
;;     "/bham/ums/solaris/pd/packages/omega-new/atp/bin/linux/bliksem-1.12")
  
;;   (defun leo~print-vars (varslist &optional (stream t)))
  
;; ;;;  Tactic leo-clauses leo-time fo-clauses fo-time cl-generated 
  
;;   (leo~set-special-output "/tmp/leo-results")



;;   (defun oc=prove (proof-plan)
;;     (when (mixin~activep)
;;       (let* ((name (keim~name proof-plan))
;; 	   (method (format nil "setProofName(~a)" (parse~atom name))))
;;       (socket~write method :inout)))
;;   (when (and view*on (pds~proof-plan-p omega*current-proof-plan))
;;     (view~hide-proof omega*current-proof-plan))
;; ; (when (and (not (mixin~activep))
;; ;	     view*on (pds~proof-plan-p omega*current-proof-plan))
;; ;    (view~hide-proof omega*current-proof-plan))
;;   (omega~trace "Changing to proof plan ~A" (keim~name proof-plan))
;;   (let ((oldtheory (when omega*current-proof-plan (prob~proof-theory omega*current-proof-plan))))
;;     (setq omega*current-proof-plan proof-plan
;; 	  omega*current-theory (prob~proof-theory proof-plan)
;; 	  logic*current-theory omega*current-theory
;; 	  keim::pds*current-proof-plan proof-plan)
;;     (alift~initialize omega*current-proof-plan) ;; KEIM-3 comment VS
;;     (setf foci*proof-context (pds~proof-context proof-plan))
;;     (setf foci*in-use t)
;;     (when (and (csm~active-p) (not auto*in-use))
;;       (oc=agents-restart :theo-default (unless (eq omega*current-theory oldtheory) omega*current-theory))))
;;   (when view*on 
;; ;    (unless (mixin~activep) (view~unhide-proof omega*current-proof-plan))
;;     (view~unhide-proof omega*current-proof-plan)
;;     (view~clean-proof view*view)
;;     (view~display-proof omega*current-proof-plan))
;;   (plan~reset-planner)
;;   (leo~reset-var-state)
;;   omega*current-proof-plan) 

;;   )




;; prove SET-014+4-3
;; call-leo-on-node SET-014+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-017+1-3
;; call-leo-on-node SET-017+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-066+1-3
;; call-leo-on-node SET-066+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-067+1-3
;; call-leo-on-node SET-067+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-076+1-3
;; call-leo-on-node SET-076+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-086+1-3
;; call-leo-on-node SET-086+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-096+1-3
;; call-leo-on-node SET-096+1-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-143+3-3
;; call-leo-on-node SET-143+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-143+4-3
;; call-leo-on-node SET-143+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-171+3-3
;; call-leo-on-node SET-171+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-580+3-3
;; call-leo-on-node SET-580+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-601+3-3
;; call-leo-on-node SET-601+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-606+3-3
;; call-leo-on-node SET-606+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-607+3-3
;; call-leo-on-node SET-607+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-609+3-3
;; call-leo-on-node SET-609+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-611+3-3
;; call-leo-on-node SET-611+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-612+3-3
;; call-leo-on-node SET-612+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-614+3-3
;; call-leo-on-node SET-614+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-615+3-3
;; call-leo-on-node SET-615+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-623+3-3
;; call-leo-on-node SET-623+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-624+3-3
;; call-leo-on-node SET-624+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-630+3-3
;; call-leo-on-node SET-630+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-640+3-3
;; call-leo-on-node SET-640+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-646+3-3
;; call-leo-on-node SET-646+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-647+3-3
;; call-leo-on-node SET-647+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-648+3-3
;; call-leo-on-node SET-648+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-649+3-3
;; call-leo-on-node SET-649+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-651+3-3
;; call-leo-on-node SET-651+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-657+3-3
;; call-leo-on-node SET-657+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-669+3-3
;; call-leo-on-node SET-669+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-669+3-3-ALT
;; call-leo-on-node SET-669+3-3-ALT fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-670+3-3
;; call-leo-on-node SET-670+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-671+3-3
;; call-leo-on-node SET-671+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-672+3-3
;; call-leo-on-node SET-672+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-673+3-3
;; call-leo-on-node SET-673+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-680+3-3
;; call-leo-on-node SET-680+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-683+3-3
;; call-leo-on-node SET-683+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-684+3-3
;; call-leo-on-node SET-684+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-686+3-3
;; call-leo-on-node SET-686+3-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-716+4-3
;; call-leo-on-node SET-716+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-724+4-3
;; call-leo-on-node SET-724+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-741+4-3
;; call-leo-on-node SET-641+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-747+4-3
;; call-leo-on-node SET-747+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-752+4-3
;; call-leo-on-node SET-752+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-753+4-3
;; call-leo-on-node SET-753+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-764+4-3
;; call-leo-on-node SET-764+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-770+4-3
;; call-leo-on-node SET-770+4-3 fo-atp-cooperation nil 100 (all) (= DEFINED EQUIV) nil


;; ;;;;;  direct OMEGA input


;; (progn

;; (oc=prove-pre (ot~read-proof-plan 'SET-014+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-014+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)
;; ;;(sleep 60)

;; (oc=prove-pre (ot~read-proof-plan 'SET-017+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-017+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-066+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-066+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-067+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-067+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-076+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-076+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-086+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-086+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-096+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-096+1-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-143+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-143+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-143+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-143+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-171+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-171+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-580+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-580+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-601+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-601+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-606+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-606+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-607+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-607+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-609+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-609+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-611+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-611+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-612+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-612+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-614+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-614+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-615+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-615+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-623+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-623+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-624+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-624+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-630+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-630+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-640+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-640+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-646+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-646+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-647+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-647+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-648+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-648+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-649+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-649+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-651+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-651+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-657+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-657+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-669+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-669+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-669+3-3-ALT))
;; (oc=call-leo-on-node (pds~label2node 'SET-669+3-3-ALT) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-670+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-670+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-671+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-671+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-672+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-672+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-673+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-673+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-680+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-680+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-683+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-683+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-684+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-684+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-686+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-686+3-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-716+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-716+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-724+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-724+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-741+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-741+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-747+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-747+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-752+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-752+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-753+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-753+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-764+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-764+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-770+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-770+4-3) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)

;; )



;; ;;;; LEO alone


;; prove SET-014+4-3
;; call-leo-on-node SET-014+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-017+1-3
;; call-leo-on-node SET-017+1-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-066+1-3
;; call-leo-on-node SET-066+1-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-067+1-3
;; call-leo-on-node SET-067+1-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-076+1-3
;; call-leo-on-node SET-076+1-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-086+1-3
;; call-leo-on-node SET-086+1-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-096+1-3
;; call-leo-on-node SET-096+1-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-143+3-3
;; call-leo-on-node SET-143+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-143+4-3
;; call-leo-on-node SET-143+4-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-171+3-3
;; call-leo-on-node SET-171+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-580+3-3
;; call-leo-on-node SET-580+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-601+3-3
;; call-leo-on-node SET-601+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-606+3-3
;; call-leo-on-node SET-606+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-607+3-3
;; call-leo-on-node SET-607+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-609+3-3
;; call-leo-on-node SET-609+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-611+3-3
;; call-leo-on-node SET-611+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-612+3-3
;; call-leo-on-node SET-612+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-614+3-3
;; call-leo-on-node SET-614+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-615+3-3
;; call-leo-on-node SET-615+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-623+3-3
;; call-leo-on-node SET-623+3-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-624+3-3
;; call-leo-on-node SET-624+3-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-630+3-3
;; call-leo-on-node SET-630+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-640+3-3
;; call-leo-on-node SET-640+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-646+3-3
;; call-leo-on-node SET-646+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-647+3-3
;; call-leo-on-node SET-647+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-648+3-3
;; call-leo-on-node SET-648+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-649+3-3
;; call-leo-on-node SET-649+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-651+3-3
;; call-leo-on-node SET-651+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-657+3-3
;; call-leo-on-node SET-657+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-669+3-3
;; call-leo-on-node SET-669+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-669+3-3-ALT
;; call-leo-on-node SET-669+3-3-ALT ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-670+3-3
;; call-leo-on-node SET-670+3-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-671+3-3
;; call-leo-on-node SET-671+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-672+3-3
;; call-leo-on-node SET-672+3-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-673+3-3
;; call-leo-on-node SET-673+3-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-680+3-3
;; call-leo-on-node SET-680+3-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-683+3-3
;; call-leo-on-node SET-683+3-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-684+3-3
;; call-leo-on-node SET-684+3-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-686+3-3
;; call-leo-on-node SET-686+3-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-716+4-3
;; call-leo-on-node SET-716+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-724+4-3
;; call-leo-on-node SET-724+4-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-741+4-3
;; call-leo-on-node SET-741+4-3 ext nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-747+4-3
;; call-leo-on-node SET-747+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-752+4-3
;; call-leo-on-node SET-752+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-753+4-3
;; call-leo-on-node SET-753+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-764+4-3
;; call-leo-on-node SET-764+4-3 ext-input-recursive nil 100 (all) (= DEFINED EQUIV) nil

;; prove SET-770+4-3
;; call-leo-on-node SET-770+4-3 standard nil 100 (all) (= DEFINED EQUIV) nil



;; ;;;; direct input as OMEGA functions

;; (progn

;; (oc=prove-pre (ot~read-proof-plan 'SET-014+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-014+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-017+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-017+1-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-066+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-066+1-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-067+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-067+1-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-076+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-076+1-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-086+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-086+1-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-096+1-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-096+1-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-143+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-143+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-143+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-143+4-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-171+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-171+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-580+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-580+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-601+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-601+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-606+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-606+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-607+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-607+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-609+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-609+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-611+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-611+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-612+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-612+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-614+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-614+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-615+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-615+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-623+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-623+3-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-624+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-624+3-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-630+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-630+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-640+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-640+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-646+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-646+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-647+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-647+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-648+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-648+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-649+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-649+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-651+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-651+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-657+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-657+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-669+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-669+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-669+3-3-ALT))
;; (oc=call-leo-on-node (pds~label2node 'SET-669+3-3-ALT) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-670+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-670+3-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-671+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-671+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-672+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-672+3-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-673+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-673+3-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-680+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-680+3-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-683+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-683+3-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-684+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-684+3-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-686+3-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-686+3-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-716+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-716+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-724+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-724+4-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-741+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-741+4-3) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-747+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-747+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-752+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-752+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-753+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-753+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-764+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-764+4-3) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)

;; (oc=prove-pre (ot~read-proof-plan 'SET-770+4-3))
;; (oc=call-leo-on-node (pds~label2node 'SET-770+4-3) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)

;; )



;; |#

;; (th~defproblem test
;; 	 (in typed-set)
;;  	(type-constants a)
;;          (author trybulec)
;;          (reference "???")
;;          (conclusion conc 
;; 	(exists (lam (P (o a)) 
;; 		(forall (lam (X (o a))
;;                            (forall (lam (Y (o a))	
;;         		      (and (subset P X)
;;                                    (and (subset P Y)
;; 					(and (subset (intersection X Y) P)
;; 				             (not (= P (intersection X Y)))))))))))))))



;; (th~defproblem checkerboard-huet-1996
;; 	       (in function)
;; 	       (type-constants bb ww)
;; 	       (conclusion 
;; 		(forall (lam (board ((o ww) (o bb)))
;; 			     (forall (lam (domino ((o ww) (o bb)))
;; 					  (implies
;; 					   (and 
;; 					    (and (injectivep board)
;; 						 (injectivep domino))
;; 					    (finite-set (lam (X bb) true)))
;; 					   (surjectivep-mateja-chris domino))))))))




;; (defun prove-wo-atp-eir (probname) 
;; 	(progn (oc=prove-pre (ot~read-proof-plan probname))
;;                (oc=call-leo-on-node (pds~label2node probname) :ext-input-recursive nil 100 '(all) '(= DEFINED EQUIV) nil)))

;; (defun prove-wo-atp-st (probname) 
;; 	(progn (oc=prove-pre (ot~read-proof-plan probname))
;;                (oc=call-leo-on-node (pds~label2node probname) :standard nil 100 '(all) '(= DEFINED EQUIV) nil)))

;; (defun prove-wo-atp-ext (probname) 
;; 	(progn (oc=prove-pre (ot~read-proof-plan probname))
;;                (oc=call-leo-on-node (pds~label2node probname) :ext nil 100 '(all) '(= DEFINED EQUIV) nil)))

;; (defun prove-wo-atp-eir-def (probname) 
;;   (progn (oc=prove-pre (ot~read-proof-plan probname))
;;	 (oc=call-leo-on-node (pds~label2node probname) :ext-input-recursive-def nil 100 '() '(= DEFINED EQUIV) nil)))


(defun prove-w-atp (probname) 
	(progn (oc=prove-pre (ot~read-proof-plan probname))
               (oc=call-leo-on-node (pds~label2node probname) :fo-atp-cooperation nil 100 '(all) '(= DEFINED EQUIV) nil)))

(defun prove-w-atp-def (probname) 
  (progn (oc=prove-pre (ot~read-proof-plan probname))
	 (oc=call-leo-on-node (pds~label2node probname) :fo-atp-cooperation-def nil 100 '() '(= DEFINED EQUIV) nil)))


(setq problemlist
(remove-duplicates 
'(
 SET-014+4-3 SET-014+4-3 SET-017+1-3 SET-017+1-3 SET-066+1-3 SET-066+1-3 SET-067+1-3 SET-067+1-3 SET-076+1-3 SET-076+1-3 SET-086+1-3 SET-086+1-3 SET-096+1-3 SET-096+1-3 SET-143+3-3 SET-143+3-3 SET-143+4-3 SET-143+4-3 SET-171+3-3 SET-171+3-3 SET-580+3-3 SET-580+3-3 SET-601+3-3 SET-601+3-3 SET-606+3-3 SET-606+3-3 SET-607+3-3 SET-607+3-3 SET-609+3-3 SET-609+3-3 SET-611+3-3 SET-611+3-3 SET-612+3-3 SET-612+3-3 SET-614+3-3 SET-614+3-3 SET-615+3-3 SET-615+3-3 SET-623+3-3 SET-623+3-3 SET-624+3-3 SET-624+3-3 SET-630+3-3 SET-630+3-3 SET-640+3-3 SET-640+3-3 SET-646+3-3 SET-646+3-3 SET-647+3-3 SET-647+3-3 SET-648+3-3 SET-648+3-3 SET-649+3-3 SET-649+3-3 SET-651+3-3 SET-651+3-3 SET-657+3-3 SET-657+3-3 SET-669+3-3 SET-669+3-3 SET-669+3-3-ALT SET-669+3-3-ALT SET-670+3-3 SET-670+3-3 SET-671+3-3 SET-671+3-3 SET-672+3-3 SET-672+3-3 SET-673+3-3 SET-673+3-3 SET-680+3-3 SET-680+3-3 SET-683+3-3 SET-683+3-3 SET-684+3-3 SET-684+3-3 SET-686+3-3 SET-686+3-3 SET-716+4-3 SET-716+4-3 SET-724+4-3 SET-724+4-3 SET-741+4-3 SET-741+4-3 SET-747+4-3 SET-747+4-3 SET-752+4-3 SET-752+4-3 SET-753+4-3 SET-753+4-3 SET-764+4-3 SET-764+4-3 SET-770+4-3 SET-770+4-3
	     )))

;(mapcar #'prove-w-atp problemlist)
;(mapcar #'prove-wo-atp-eir problemlist)
;(mapcar #'prove-wo-atp-st problemlist)
;(mapcar #'prove-wo-atp-ext problemlist)
;(mapcar #'prove-wo-atp-eir-def problemlist)
(mapcar #'prove-w-atp-def problemlist)

; For talk:

;5min: prove SET-171+3-3
; activate agents; show agent based proof with omega; activate leo agent; show automatic agent proof by leo
;in the background;



 

