;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
(in-package "OMEGA")

(infer~defmethod "AssTranscl3-m"
		 (outline-mappings (((existent existent nonexistent nonexistent) ass-transcl3-m)))
		 (help "Applying the ass-transcl3 assumption."))

(meth~defmethod ass-transcl3-m
		AssTranscl3-m
		(in base)
		(rating 2)
		(declarations
		 (type-variables aa)
		 (constants
		  ;;;(key objects from the special domain)
		  (transclosure ((o aa) (o aa)))
		  (subset (o (o aa) (o aa)))
		  (transitive (o (o aa)))
		  )
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  ;(s1 (o aa)) (s2 (o aa))
		  (U (o aa)) (V (o aa)))
		 )
		(premises l1 (+ l4) (+ l5))
		(conclusions (- l7))
		(decl-content
		 (l1 () (forall (lam (s1 (o aa))
				     (forall (lam (s2 (o aa))
						  (implies (and (subset s1 s2) (transitive s2))
							   (subset (transclosure s1)  s2)))))))
		 (l2 () (forall (lam (s2 (o aa))
				     (implies (and (subset U s2) (transitive s2))
					      (subset (transclosure U) s2))))         ("ForallE" (U) (l1)))
		 (l3 () (implies (and (subset U V) (transitive V))
				 (subset (transclosure U) V))             ("ForallE" (V) (l2)))
		 (l4 () (subset U V)                                      ("Open" () ()))
		 (l5 () (transitive V)                                    ("Open" () ()))
		 (l6 () (and (subset U V) (transitive V))                 ("AndI" () (l4 l5)))
		 (l7 () (subset (transclosure U) V)                       ("ImpE" () (l6 l3)))
		 )
		(proc-content schema-interpreter)
		(remark "applies the assertion: if the set s1 is a subset of a set s2 which is transitive then is the transclosure of s1 a subset of s2.")
		)

#|
The idea of the above method: (The logical meaning of the above method):
FROM:
 all a,b. a =< b & trans.(b) --> (transcl a) =< b
 A =< B
 trans.(B)
FOLLOWS:
 (transcl A) =< B
|#

(infer~defmethod "AssTranscl2-m"
		 (outline-mappings (((existent existent) ass-transcl2-m)))
		 (help "Applying the ass-transcl2 assumption."))


(meth~defmethod ass-transcl2-m AssTranscl2-m
		(in base)
		(rating 5)
		(declarations
		 (type-variables aa)
		 (constants
		  ;;;(key objects from the special domain)
		  (transclosure ((o aa) (o aa)))
		  (transitive (o (o aa)))
		  )
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (U (o aa)) ;(s (o aa))
		  )
		 )
		(premises l1)
		(conclusions (- l2))
		(decl-content
		 (l1 () (forall (lam (s (o aa))
				     (transitive (transclosure s)))))
		 (l2 () (transitive (transclosure U))           ("ForallE" (U) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "applies the assertion: the transclosure of each set is transitive."
			)
		)


(infer~defmethod "UnionSubset-m"
		 (outline-mappings (((existent existent existent nonexistent nonexistent) union-subset-m)))
		 (help "Applying the Subset definition to a Union set."))


(meth~defmethod union-subset-m UnionSubset-m
		(in base)
		(rating 2)
		(declarations
		 (type-variables cc ff)
		 (constants
		  (subset (o (o cc) (o cc)))
		  (union ((o ff) (o ff) (o ff))))
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  ;(s1 (o cc)) (s2 (o cc)) (s3 (o cc)) (s4 (o cc))
		  ;(x cc)
		  ;(y cc)
		  (U (o cc)) (V (o cc)) (W (o cc)) (C cc))
		 )
		(premises l1 l2 (+ l13) (+ l19))
		(expansion-computations
		 (C (var-newconst x))
		 (l3p (applargs (prlnformula l26)))
		 (l6p (mlist U V C))
		 (l11p (mlist U W))
		 (l17p (mlist V W))
		 )
		(conclusions (- l26)) 
		(decl-content
		 (l1  ()    (forall (lam (s1 (o cc))
					  (forall (lam (s2 (o cc))
						       (equiv (subset s1 s2)
							      (forall (lam (x cc var)
									   (implies (s1 x) (s2 x))))))))))
		 (l2  ()    (forall (lam (s3 (o cc))
					  (forall (lam (s4 (o cc))
						       (forall (lam (y cc)
								    (equiv ((union s3 s4) y)
									   (or (s3 y) (s4 y))))))))))
		 (l3 ()     (equiv (subset (union U V) W)
				   (forall (lam (x cc var)
						(implies ((union U V) x) (W x)))))
		                                                           ("ForallE*" (l3p) (l1)))
		 (l4 ()     (implies (forall (lam (x cc var)
						(implies ((union U V) x) (W x))))
				     (subset (union U V) W))               ("EquivER" () (l3)))
		 (l5 ()     ((union U V) C)                                ("Hyp" () ()))
		 (l6 ()     (equiv ((union U V) C) (or (U C) (V C)))       ("ForallE*" (l6p) (l2)))
		 (l7 ()     (implies ((union U V) C) (or (U C) (V C)))     ("EquivEL" () (l6)))
		 (l8 (l5)   (or (U C) (V C))                               ("ImpE" () (l5 l7)))
		 (l9  ()    (U C)                                          ("Hyp" () ()))
		 (l10 ()    (V C)                                          ("Hyp" () ()))
		 (l11 ()    (equiv (subset U W)
				   (forall (lam (x cc var)
						(implies (U x) (W x)))))   ("ForallE*" (l11p) (l1)))
		 (l12 ()    (implies (subset U W)
				     (forall (lam (x cc var)
						  (implies (U x) (W x))))) ("EquivEL" () (l11)))
		 (l13 ()    (subset U W)                                   ("Open" () ()))
		 (l14 ()    (forall (lam (x cc var)
					 (implies (U x) (W x))))           ("ImpE" () (l13 l12)))
		 (l15 ()    (implies (U C) (W C))                          ("ForallE" (C) (l14)))
		 (l16 (l9)  (W C)                                          ("ImpE" () (l9 l15) ))
		 (l17 ()    (equiv (subset V W)
				   (forall (lam (x cc var)
						(implies (V x) (W x)))))   ("ForallE*" (l17p) (l1)))
		 (l18 ()    (implies (subset V W)
				     (forall (lam (x cc var)
						  (implies (V x) (W x))))) ("EquivEL" () (l17)))
		 (l19 ()    (subset V W)                                   ("Open" () ()))
		 (l20 ()    (forall (lam (x cc var)
					 (implies (V x) (W x))))           ("ImpE" () (l19 l18)))
		 (l21 ()    (implies (V C) (W C))                          ("ForallE" (C) (l20)))
		 (l22 (l10) (W C)                                          ("ImpE" () (l10 l21) ))
		 (l23 (l5)  (W C)                                          ("OrE" () (l8 l16 l22)))
		 (l24 ()    (implies ((union U V) C) (W C))                ("ImpI" () (l23)))
		 (l25 ()    (forall (lam (x cc var)
					 (implies ((union U V) x) (W x)))) ("ForallI" (C) (l24)))
		 (l26 ()    (subset (union U V) W)                         ("ImpE" () (l25 l4)))
		 )
		(proc-content schema-interpreter)
		(remark "The union of two sets U and V is a subset of a set W, when each of them is a subset of the set W.")
		)


(infer~defmethod "Otter1-m"
		 (outline-mappings (((existent existent existent existent) otter1)))
		 (help "Calling otter."))

(meth~defmethod otter1 Otter1-m
		(in base)
		(rating 3)
		(declarations
		 (type-variables cc ff)
		 (constants 
		  (subset (o (o cc) (o cc)))
		  (union ((o ff) (o ff) (o ff)))
		  (transclosure ((o cc) (o cc))))
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  ;(s1 (o cc)) (s2 (o cc))
		  (psi o)
		  ;(s3 (o cc)) (s4 (o cc)) (x cc)
		  (phi o)
		  ;(s (o cc))
		  (U (o cc)) (V (o cc)) (W (o cc)))
		 )
		(premises l1 l2 l3 )
		(application-condition
		 (mor (subterm U V) (subterm U W)))
		(conclusions  (- l4))
		(decl-content
		 (l1 () (forall (lam (s1 (o cc))
				     (forall (lam (s2 (o cc))
						  (equiv (subset s1 s2) psi))))))		   
		 (l2 () (forall (lam (s3 (o cc))
				     (forall (lam (s4 (o cc))
						  (forall (lam (x cc)
							       (equiv ((union s3 s4) x) phi))))))))
		 (l3 () (forall (lam (s (o cc))
				     (subset s (transclosure s)))))

		 (l4 () (subset U (transclosure (union V W)))                ("Otter" () (l1 l2 l3) "untested"))
		 )
		(proc-content schema-interpreter)
		(remark "specifies a problem situation that OTTER could solve.")
		)


#| logical meaning of the above method:
(This method specifies a situation of calling an ATP (otter))
When having:
 1) subset definition: all s1,s2. s1 =< s2 <--> all x,y. s1((f x y)) --> s2((f x y))
 2) union definition: all u,v,z. (u + v)(z) <--> u(z) or v(z)
 3) for a set s and some function function g, (g s) is a superset of s: all s. s =< (g s)
 4) to prove: A =< (g (B + C))
 5) A is a subterm of B or C
then:
 call OTTER to prove 4) using 1), 2), and 3) as premises.
|#


;(infer~defmethod "Spass1-m"
;                 (outline-mappings (((existent existent existent existent) spass1)))
;                 (help "Calling Spass"))
;
;(meth~defmethod spass1 Spass1-m
;                (in base)
;                (rating 3)
;                (declarations
;                 (type-variables cc dd ff)
;                 (constants 
;                  (subset (o (o cc) (o cc)))
;                  (union ((o ff) (o ff) (o ff)))
;                  (transclosure ((o cc) (o cc)))
;                  )
;                 (meta-sort-symbols var const appl abstr term type prln just pos sub ir
;                                    varlist constlist appllist abstrlist termlist typelist
;                                    prlnlist justlist poslist sublist irlist list
;                                    )
;                 (meta-subsort-declarations
;                  (constlist termlist)
;                  (varlist list) (constlist list) (appllist list) (abstrlist list)
;                  (termlist list) (typelist list) (prlnlist list) (justlist list)
;                  (poslist list) (sublist list) (irlist list)
;                  (var term) (const term) (appl term) (abstr term))
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (l1 o prln) (l2 o prln) (l3 o prln) (l4 o prln)
;                  (h1 o prlnlist) (h2 o prlnlist) (h3 o prlnlist) (h4 o prlnlist)
;                  (s1 (o cc) var) (s2 (o cc) var)
;                  (psi o term)
;                  (s3 (o cc) var) (s4 (o cc) var) (x cc var)
;                  (phi o term) 
;                  (s (o cc) var)
;                  (h ((o cc) (o cc)) const)
;                  (U (o cc) term) (V (o cc) term) (W (o cc) term)
;                  )
;                 (sorted-meta-constants;;(meta-constant typ sort)
;                  (meval (aa cc bb bb) (o o o o))
;                  (mforall (aa bb cc aa) (o o list o))
;                  (mexists (aa bb cc aa) (o o list o))
;                  (mbind (o dd cc) (o term var))
;                  (mor (aa aa aa) (o o o))
;                  (mnot (aa aa) (o o))
;                  (listsubsetp (o bb bb) (o termlist termlist))
;                  (termoccs (cc dd dd) (poslist term term))
;                  (posemptylist cc poslist)
;                  (mand (aa aa aa) (o o o))
;                  (mequal (o dd dd) (o o o))
;                  )
;                 )
;                (premises l1 l2 l3 )
;                (outline-constraint
;                 (mand (listsubsetp h1 h4)
;                       (mand (listsubsetp h2 h4)
;                             (mand (listsubsetp h3 h4)
;                                   (mor (mnot (mequal (termoccs U V) posemptylist))
;                                        (mnot (mequal (termoccs U W) posemptylist))))))
;                 )
;                (conclusions  (- l4))
;                (decl-content
;                 (l1 h1 (forall (lam (s1 (o cc))
;                                     (forall (lam (s2 (o cc))
;                                                  (equiv (subset s1 s2) psi))))))                  
;                 (l2 h2 (forall (lam (s3 (o cc))
;                                     (forall (lam (s4 (o cc))
;                                                  (forall (lam (x cc)
;                                                               (equiv ((union s3 s4) x) phi))))))))
;                 (l3 h3 (forall (lam (s (o cc))
;                                     (subset s (transclosure s)))))
;
;                 (l4 h4  (subset U (transclosure (union V W)))                ("Spass" () (l1 l2 l3) "untested"))
;                 )
;                (proc-content schema-interpreter)
;                (remark "specifies a problem situation that SPASS could solve.")
;                )
;
;
;
;
;
;(infer~defmethod "Protein1-m"
;                 (outline-mappings (((existent existent existent existent) protein1)))
;                 (help "Calling Protein"))
;
;(meth~defmethod protein1 Protein1-m
;                (in base)
;                (rating 3)
;                (declarations
;                 (type-variables cc dd ff)
;                 (constants 
;                  (subset (o (o cc) (o cc)))
;                  (union ((o ff) (o ff) (o ff)))
;                  (transclosure ((o cc) (o cc)))
;                  )
;                 (meta-sort-symbols var const appl abstr term type prln just pos sub ir
;                                    varlist constlist appllist abstrlist termlist typelist
;                                    prlnlist justlist poslist sublist irlist list
;                                    )
;                 (meta-subsort-declarations
;                  (constlist termlist)
;                  (varlist list) (constlist list) (appllist list) (abstrlist list)
;                  (termlist list) (typelist list) (prlnlist list) (justlist list)
;                  (poslist list) (sublist list) (irlist list)
;                  (var term) (const term) (appl term) (abstr term))
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (l1 o prln) (l2 o prln) (l3 o prln) (l4 o prln)
;                  (h1 o prlnlist) (h2 o prlnlist) (h3 o prlnlist) (h4 o prlnlist)
;                  (s1 (o cc) var) (s2 (o cc) var)
;                  (psi o term)
;                  (s3 (o cc) var) (s4 (o cc) var) (x cc var)
;                  (phi o term) 
;                  (s (o cc) var)
;                  (h ((o cc) (o cc)) const)
;                  (U (o cc) term) (V (o cc) term) (W (o cc) term)
;                  )
;                 (sorted-meta-constants;;(meta-constant typ sort)
;                  (meval (aa cc bb bb) (o o o o))
;                  (mforall (aa bb cc aa) (o o list o))
;                  (mexists (aa bb cc aa) (o o list o))
;                  (mbind (o dd cc) (o term var))
;                  (mor (aa aa aa) (o o o))
;                  (mnot (aa aa) (o o))
;                  (listsubsetp (o bb bb) (o termlist termlist))
;                  (termoccs (cc dd dd) (poslist term term))
;                  (posemptylist cc poslist)
;                  (mand (aa aa aa) (o o o))
;                  (mequal (o dd dd) (o o o))
;                  )
;                 )
;                (premises l1 l2 l3 )
;                (outline-constraint
;                 (mand (listsubsetp h1 h4)
;                       (mand (listsubsetp h2 h4)
;                             (mand (listsubsetp h3 h4)
;                                   (mor (mnot (mequal (termoccs U V) posemptylist))
;                                        (mnot (mequal (termoccs U W) posemptylist))))))
;                 )
;                (conclusions  (- l4))
;                (decl-content
;                 (l1 h1 (forall (lam (s1 (o cc))
;                                     (forall (lam (s2 (o cc))
;                                                  (equiv (subset s1 s2) psi))))))                  
;                 (l2 h2 (forall (lam (s3 (o cc))
;                                     (forall (lam (s4 (o cc))
;                                                  (forall (lam (x cc)
;                                                               (equiv ((union s3 s4) x) phi))))))))
;                 (l3 h3 (forall (lam (s (o cc))
;                                     (subset s (transclosure s)))))
;
;                 (l4 h4  (subset U (transclosure (union V W)))                ("Protein" () (l1 l2 l3) "untested"))
;                 )
;                (proc-content schema-interpreter)
;                (remark "specifies a problem situation that PROTEIN could solve.")
;                )



















