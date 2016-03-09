(in-package :omega)

(infer~defmethod "ForallE-m"
		 (outline-mappings (((nonexistent closed) foralle-m-f)))
		 (parameter-types term)
		 (help "The method for FORALL elemination with a parameter."))


(meth~defmethod foralle-m-f ForallE-m
		(in hua)
		(rating 10)
		(declarations
		 (type-variables dd)
		 (sorted-meta-variables
		  (phi o) (phi1 o)
		  (c o term)

                  )
		 )
		(parameters c)
		(premises (- l1))
		(application-condition
		 ;(mnot (mequal (applfunc phi) forall))
		 )
		(outline-computations
		 (phi1 (subst-apply (subst-create (mlist x) (mlist c)) phi ))
		 )
		(conclusions (+ l2))
		(decl-content 
		 (l1 () (forall (lam (x dd var) phi)))
		 (l2 () phi1                      ("ForallE" (c) (l2)))

		 )
		(proc-content schema-interpreter)
		(remark "Forward application of the tactic ForallE*")
		)

(meth~defmethod ImpE-m-b ImpE-m
                (in base)
                (rating 1)
                (declarations
                 (sorted-meta-variables 
                  (F1 o) (F2 o))
                 )
                (premises (+ l1) l2)
                (conclusions (- l3))
                (decl-content
                 (l1 () (implies F1 F2) ("Open" () ()))
                 (l2 ()  F1)
                 (l3 ()  F2            ("ImpE" () (l2 l1)))
                 )
                (proc-content schema-interpreter)               
                (remark "ImpE-m-b is a backward working method that takes a goal F2 and an assumption F1 and produces a new subgoal F1-> F2.")
)

(infer~defmethod "Big-ImpE-m"
		 (outline-mappings (((existent existent nonexistent) Big-ImpE-m-b)))
		 (help "Forall and Implies-Elimination."))

(meth~defmethod Big-ImpE-m-b
		Big-ImpE-m
		(in hua)
		(rating 10)
		(declarations
		 (sorted-meta-variables
		  (left o)
		  (right o)
		  (inst-left o)
		  (inst-right o)
		  ))
		(premises ass (+ l1))
		(application-condition
		 (alpha-matcher right inst-right sub))
		(outline-computations
		 (inst-left (subst-apply sub left))
		 (cl (subst-apply sub xL))
		 )
		(conclusions (- goal))
		(decl-content
		 (ass  () (forall (lam (xL list varlist) (implies left right))))
		 (l2   () (implies inst-left inst-right)      ("ForallE*" (cL) (ass)))
		 (l1   () inst-left                           ("Open" () ()))
		 (goal () inst-right                          ("ImpE" () (l1 l2))))
		)

#|
;; kann noetig sein fuer repair:
;; A existiert, aber (and A B) wird gebraucht.

(infer~defmethod "AndE-with-parameter-m"
		 (outline-mappings (((existent existent nonexistent) ande-m-p)))
		 (help "The method for AND elimination"))


(meth~defmethod ande-m-s AndE-m
		(in base)
		(rating 0.4)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (+ l1))
		(conclusions (+ l2) (- l3))
		(decl-content
		 (l1 () (and phi psi))
		 (l2 () phi ("AndEL" () (l1))) 
		 (l3 () psi ("AndER" () (l1))) 
		 )
		(proc-content schema-interpreter)
		(remark "Sideward application of AND elimination")
		)

|#
#|
(infer~defmethod "EquivE-m"
		 (outline-mappings (((nonexistent nonexistent existent) EquivI-m-f)))
		 (help "The method for EQUIV elimination"))

(meth~defmethod EquivE-m-f EquivE-m
		(in base)
		(rating 10) 
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o) (psi o))
		 )
		(premises (- l3))
		(conclusions (+ l1) (+ l2))
		(decl-content 
		 (l1 () (implies phi psi)       ("AndEL" () (l4)))
		 (l2 () (implies psi phi)       ("AndER" () (l4)))
		 (l4 () (and (implies phi psi)
			    (implies psi phi)) ("EquivE" () (l3)))
		 (l3 () (equiv phi psi))
		 )
		(proc-content schema-interpreter)
		(remark "Forward applying EQUIV Elimination")
		)


(infer~defmethod "EqvAss2R-m"
		 (outline-mappings (((existent existent nonexistent) eqv-ass2r-rb)
				    ))
		 (help "A method for applying an equivalence assertion to its instantiated right side to conclude the instantiated left side"))

;;; def-elim with eqv
(meth~defmethod eqv-ass2r-rb
		EqvAss2r-m
		(in base)
		(rating 2)
		(declarations
		 ;;(type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (psi o) (psi1 o)
		  (tL list termlist) (p (o list) const)
		  ;;(p (o aa) term)
		  )
		 )
		(premises l1 (+ l5))
		(outline-computations
		 (psi1 (n-normalize (subst-apply (subst-create xL tL) psi)))
		 )
		(conclusions (- l4))
		(decl-content
		 (l1 () (forall (lam (xL list varlist) (equiv (p xL) psi))))
		 (l2 () (equiv (p tL) psi)                        ("ForallE*" (tL) (l1)))
		 (l3 () (implies psi1 (p tL))                      ("EquivER" () (l2)))
		 (l5 () psi1                                       ("Open" () ()))
		 (l4 () (p tL)                                     ("ImpE" () (l3 l5)))
		 )
		(proc-content schema-interpreter)
		(remark "Sideward application of an equivalence assertion to its right side to conclude the corresponding left side")
		)

|#
