(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLI-SORT-RESCLASS-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-Resclass-m"
		 (outline-mappings (((existent nonexistent) ForallI-Sort-Resclass-m-b)))
		 (help "The method for FORALL-sort-Resclass introduction"))


(meth~defmethod ForallI-Sort-Resclass-m-b ForallI-Sort-Resclass-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)  (phi1 o)  (phi2 o)  (num-hyp o)
		  (resclassset (o (o num)))
		  (c num const)
		  (cprime (o num) const)
		  (cexpr (o num) term)
		  (class-factor num)
		  (num-set (o num))
		  (mvs list metavarlist)
		  (mv aa metavar)
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (mand (mresclass-set-p resclassset)
		       (mbind c (type-newconst num))
		       (mbind mvs (metavars l3))
		       (mforall mv mvs
				(not-subterm c mv))))

		(outline-computations
		 (class-factor (mclass-factor-of resclassset))
		 (num-set (mnum-set-of resclassset))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist (appl-create (:term resclass) (mlist class-factor c)))) phi))
		 )
		
		(decl-content
		 (l1  ()      (in c num-set)                                                ("Hyp" () ()))
		 (l2  (l1)    phi1                                                          ("Open" () ()))
		 (l3  ()      (forall-sort (lam (x (o num) var) phi) resclassset)           ("ForallI-Sort" () ())))

;expansion 
;                (outline-actions
;                 (l2 (sponsor l1))
;                 (l2 (unsponsor l0))
;                 (l2 (unsponsor l12)))
;
;                (outline-computations
;                 (cprime (type-newconst (:type (o num))))
;                 (class-factor (mclass-factor-of resclassset))
;                 (num-set (mnum-set-of resclassset))
;                 (phi1 (subst-apply (subst-create (mlist x) (mlist (appl-create (:term resclass) (mlist class-factor c)))) phi))
;                 (num-hyp (beta-normalize (appl-create num-set (mlist c))))
;                 (phi2 (subst-apply (subst-create (mlist x) (mlist cprime)) phi)))
;                 
;                (decl-content
;                 (l0  ()         (resclassset cprime)                                          ("Hyp" () ()))
;                 (l11 (l0)       (exists (lam (y num)
;                                              (and (num-set y)
;                                                   (= cprime (resclass class-factor y)))))     ("TAKTIK" () ()))
;                 (l12 ()         (and (num-set c) (= cprime (resclass class-factor c)))        ("Hyp" () ()))
;                 (l13 (l12)   (num-set c)                                                   ("AndeL" () (l12)))
;                 (l14 (l12)   (= cprime (resclass class-factor c))                          ("AndeR" () (l12)))
;                 
;                 (l1  (l12)   num-hyp                                                       ("Beta-Normalize" () (l13)))
;
;                 (l2  (l0 l12)   phi1                                                          ("Open" () ()))
;
;                 (l15 (l0 l12)   phi2                                                          ("=Subst*" () (l2 l14)))    
;                 (l16 (l0)       phi2                                                          ("Existse" (c) (l11 l15)))
;                 (l3  ()         (forall-sort (lam (x (o num) var) phi) resclassset)           ("ForallI-Sort" (cprime) (l16))))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort-resclass."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;existsi-SORT-RESCLASS-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-Resclass-m"
		 (outline-mappings (((existent nonexistent nonexistent) ExistsI-Sort-Resclass-m-b)))
		 (help "The method for Exists-sort-Resclass introduction"))


(meth~defmethod ExistsI-Sort-Resclass-m-b ExistsI-Sort-Resclass-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (phi1 o)
		  (resclassset (o (o num)))
		  (num-hyp o)
		  (mv num metavar)
		  (class-factor num)
		  (num-set (o num))
		  )
		 )
		(premises (+ l1) (+ l2))
		(conclusions (- l3))

		(application-condition
		 (mresclass-set-p resclassset))
		(outline-computations
		 (mv (newmetavar (:symbol :M) num))
		 (class-factor (mclass-factor-of resclassset))
		 (num-set (mnum-set-of resclassset))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist (appl-create (:term resclass) (mlist class-factor mv)))) phi))
		 (num-hyp (beta-normalize (appl-create num-set (mlist mv))))
		 )
		
		(decl-content
		 (l1 ()   (in mv num-set)                                        ("Open" () ()))
		 (l2 ()   phi1                                                   ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x (o num) var) phi) resclassset)    ("ExistsI-Sort-Resclass" (mv) (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort-resclass"))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;existsi-SORT-RESCLASS-FUNCTION-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-Resclass-Function-m"
		 (outline-mappings (((existent nonexistent nonexistent) ExistsI-Sort-Resclass-Function-m-b)))
		 (help "The method for Exists-sort-Resclass introduction"))

(meth~defmethod ExistsI-Sort-Resclass-Function-m-b ExistsI-Sort-Resclass-Function-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (phi o) (phiprime o) 
		  (resclassset1 (o aa)) (resclassset2 (o bb))
		  (mvs-in-set2 o)
		  (function (bb aa)) (function-const (bb aa))
		  (func-equation o)
		  )
		 )
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (mand (mprod-of-resclass-sets-p resclassset1)
		       (mprod-of-resclass-sets-p resclassset2)))
		
		(outline-computations
		 (function-const (type-newconst (:type (bb aa))))
		 (terms (isomorphic-terms-exists-II resclassset1 resclassset2))
		 (function (mfirst terms))
		 (mvs-in-set2 (msecond terms))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist function-const)) phi))
		 (func-equation (appl-create (:term =) (mlist function-const function))))
		
		(decl-content
		 (l0 ()   func-equation                                        ("Hyp"  ()  ()))
		 (l1 ()   mvs-in-set2                                          ("Open" () ()))
		 (l2 (l0) phiprime                                             ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x (bb aa) var) phi)
				       (functions resclassset1 resclassset2))  ("ExistsI-Sort-Resclass-Function" (function) (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort-resclass-function"))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Foralli-SORT-RESCLASS-FUNCTION-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-Resclass-Function-m"
		 (outline-mappings (((existent nonexistent nonexistent) ForallI-Sort-Resclass-Function-m-b)))
		 (help "The method for Exists-sort-Resclass introduction"))


(meth~defmethod ForallI-Sort-Resclass-Function-m-b ForallI-Sort-Resclass-Function-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (phi o) (phiprime o)
		  (resclassset1 (o aa)) (resclassset2 (o bb))
		  (cs-in-set2 o)
		  (function (bb aa)) (function-const (bb aa))
		  (func-equation o)		  
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (mand (mprod-of-resclass-sets-p resclassset1)
		       (mprod-of-resclass-sets-p resclassset2)))

		(outline-computations
		 (function-const (type-newconst (:type (bb aa))))
		 (terms (isomorphic-terms-forall-II resclassset1 resclassset2))
		 (function (mfirst terms))
		 (cs-in-set2 (msecond terms))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist function-const)) phi))
		 (func-equation (appl-create (:term =) (mlist function-const function))))
		
		(decl-content
		 (l0 ()   func-equation                                           ("Hyp"  ()  ()))
		 (l1 ()   cs-in-set2                                              ("Hyp" () ()))
		 (l2 (l0 l1) phiprime                                                ("Open" () ()))
		 (l3 ()   (forall-sort (lam (x (bb aa) var) phi)
				       (functions resclassset1 resclassset2))     ("ForallI-Sort-Resclass-Function" (function) (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort-resclass-Function"))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;IsomorphicI-Function-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(infer~defmethod "IsomorphicI-Function-M"
;;		 (outline-mappings (((existent nonexistent nonexistent nonexistent) IsomorphicI-Function-M-b)))
;;		 (parameter-types term)
;;		 (help "The method for Introducing a function."))
;;
;;(meth~defmethod IsomorphicI-Function-M-b IsomorphicI-Function-M
;;		(in zmz)
;;		(rating 10)
;;		(reasoning :planning :middle-out)
;;		
;;		(declarations
;;		 (type-variables aa bb)
;;		 (sorted-meta-variables
;;		  (set1 (o aa)) (set2 (o bb))
;;		  (op1 (aa aa aa)) (op2 (bb bb bb))
;;		  (function (bb aa)) 
;;		  )
;;		 )
;;		
;;		(parameters function)
;;		
;;		(premises (+ l1) (+ l2) (+ l3))
;;		(conclusions (- l4))
;;		
;;		(decl-content
;;		 (l1 ()   (homomorphism set1 op1 set2 op2 function)   ("Open" () ()))
;;		 (l2 ()   (injective set1 function)                   ("Open" () ()))
;;		 (l3 ()   (surjective set1 set2 function)             ("Open" () ()))
;;		 (l4 ()   (isomorphic set1 op1 set2 op2)              ("Isomorphic-Function" (function) (l1 l2 l3)))
;;		 )
;;		(proc-content schema-interpreter)
;;		(manual (documentation "This methods has the same effect as the backward application
;;                                       of the tactic ExistsI-function"))
;;		)



(infer~defmethod "IsomorphicI-Function-M"
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) IsomorphicI-Function-M-b)))
		 ;;(parameter-types term)
		 (help "The method for Introducing a function."))

(meth~defmethod IsomorphicI-Function-M-b IsomorphicI-Function-M
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (set1 (o aa)) (set2 (o bb))
		  (op1 (aa aa aa)) (op2 (bb bb bb))
		  (function (bb aa)) 
		  )
		 )

		(outline-computations
		 (function (newmetavar (:symbol mv) (:type (bb aa)))))		 
		
		(premises (+ l1) (+ l2) (+ l3))
		(conclusions (- l4))
		
		(decl-content
		 (l1 ()   (homomorphism set1 op1 set2 op2 function)   ("Open" () ()))
		 (l2 ()   (injective set1 function)                   ("Open" () ()))
		 (l3 ()   (surjective set1 set2 function)             ("Open" () ()))
		 (l4 ()   (isomorphic set1 op1 set2 op2)              ("Isomorphic-Function" (function) (l1 l2 l3)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-function"))
		)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ModuleIsomorphicI-Function-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ModuleIsomorphicI-Function-M"
		 (outline-mappings (((existent nonexistent nonexistent nonexistent nonexistent) ModuleIsomorphicI-Function-M-b)))
		 ;;(parameter-types term)
		 (help "The method for Introducing a function."))

(meth~defmethod ModuleIsomorphicI-Function-M-b ModuleIsomorphicI-Function-M
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		
		(declarations
		 (type-variables aa bb1 bb2)
		 (sorted-meta-variables
		  (set1 (o bb1)) (set2 (o bb2))
		  (op1 (bb1 bb1 bb1)) (op2 (bb2 bb2 bb2))
		  (ringi (o aa))
		  (scop1 (bb1 bb1 aa)) (scop2 (bb2 bb2 aa))
		  (function (bb2 bb1)) 
		  )
		 )
		
		(outline-computations
		 (function (newmetavar (:symbol mv) (:type (bb2 bb1)))))		 
		
		(premises (+ l1) (+ l2) (+ l3) (+ l4))
		(conclusions (- l5))
		
		(decl-content
		 (l1 ()   (homomorphism set1 op1 set2 op2 function)               ("Open" () ()))
		 (l2 ()   (injective set1 function)                               ("Open" () ()))
		 (l3 ()   (surjective set1 set2 function)                         ("Open" () ()))
		 (l4 ()   (forall-sort (lam (rr aa)
					    (forall-sort (lam (mm bb1)
							      (= (function (scop1 rr mm))
								 (scop2 rr (function mm))))
							 set1))
				       ringi)                                      ("Open" () ()))
		 (l5 ()   (module-isomorphic set1 op1 set2 op2 ringi scop1 scop2)  ("ModuleIsomorphic-Function" (function) (l1 l2 l3)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;existsi-Sort-INTEGER-INTERVALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-Integer-Interval-m"
		 (outline-mappings (((existent nonexistent nonexistent) ExistsI-Sort-Integer-Interval-m-b)))
		 (help "The method for Exists-sort introduction over an integer intervall."))

(meth~defmethod ExistsI-Sort-Integer-Interval-m-b ExistsI-Sort-Integer-Interval-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o) (phi1 o)
		  (mv num metavar)
		  (num-hyp o)
		  (IntIntervall (o num))))
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		(application-condition
		 (mdetermined-intintervall-p IntIntervall))
		(outline-computations
		 (mv (newmetavar (:symbol :M) num))
		 (num-set (mnum-set-of IntIntervall))
		 (phi1 (subst-apply (subst-create (mlist n) (mlist mv)) phi))
		 (num-hyp (beta-normalize (appl-create num-set (mlist mv))))
		 )
		(decl-content
		 (l1 ()   num-hyp                                                        ("Open" () ()))
		 (l2 ()   phi1                                                           ("Open" () ()))
		 (l3 ()   (exists-sort (lam (n num var) phi) IntIntervall)    ("ExistsI-Sort-Integer-Intervall" (mv) (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-Sort-Integer-Intervall"))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Foralli-Sort-INTEGER-INTERVALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-Integer-Interval-m"
		 (outline-mappings (((existent nonexistent) ForallI-Sort-Integer-Interval-m-b)))
		 (help "The method for Forall-sort introduction over an integer intervall."))

(meth~defmethod ForallI-Sort-Integer-Interval-m-b ForallI-Sort-Integer-Interval-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o) (phi1 o)
		  (c num const)
		  (num-hyp o)
		  (IntIntervall (o num))))
		(premises (+ l2))
		(conclusions (- l3))
		(application-condition
		 (mdetermined-intintervall-p IntIntervall))
		(outline-computations
		 (c (type-newconst (:type num)))
		 (num-set (mnum-set-of IntIntervall))
		 (phi1 (subst-apply (subst-create (mlist n) (mlist c)) phi))
		 (num-hyp (beta-normalize (appl-create num-set (mlist c))))
		 )
		(decl-content
		 (l1 ()     num-hyp                                             ("Hyp" () ()))
		 (l2 (l1)   phi1                                                ("Open" () ()))
		 (l3 ()     (forall-sort (lam (n num var) phi) IntIntervall)    ("ForallI-Sort-Integer-Intervall" (c) (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-Sort-Integer-Intervall"))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Posnat-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "PosNat-m"
		 (outline-mappings (((existent) PosNat-m-b)))
		 (help "The method for PosNat closures."))

(meth~defmethod PosNat-m-b PosNat-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (n num)))
		(conclusions (- l1))
		(application-condition
		 (mposint-p n))
		(decl-content
		 (l1 ()     (pos-nat n)    ("PosNat" () ()))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic PosNat"))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RewritePowerOfOp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "RewritePowerOfOp-m"
		 (outline-mappings (((existent nonexistent) RewritePowerOfOp-m-b)))
		 (help "The method for rewriting power-of-operation expressions whose power value is specified"))

(meth~defmethod RewritePowerOfOp-m-b RewritePowerOfOp-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o) (phi1 o)
		  (poslist o positionlist)))
		(just-parameters poslist)
		(premises (+ l1))
		(conclusions (- l2))
		(application-condition
		 (mdetpowerofop-at-posses-p phi poslist))
		(outline-computations
		 (phi1 (mrewrite-detpowerofop-at-posses phi poslist)))
		(decl-content
		 (l1 ()   phi1                ("Open" () ()))
		 (l2 ()   phi                 ("RewritePowerOfOp" (poslist) (l2)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-Sort-Integer-Intervall"))
		)


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLI-SORT-CART-PRODUCT-RESCLASS-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-Cart-Product-Resclass-m"
		 (outline-mappings (((existent nonexistent) ForallI-Sort-Cart-Product-Resclass-m-b)))
		 (help "The method for FORALL-sort-Resclass introduction"))


(meth~defmethod ForallI-Sort-Cart-Product-Resclass-m-b ForallI-Sort-Cart-Product-Resclass-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (phiprime o)
		  (cartprodresclsets (o aa))
		  (cs-in-num-sets o)
		  (residue-class-sets list termlist)
		  (class-factors list termlist)
		  (num-sets-to-resclass-sets list termlist)
		  (new-consts list termlist)
		  (new-pair-term aa)
		  (phiprime o)
		  (cs-in-num-sets o)
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (mcart-product-of-resclasses-p cartprodresclsets))
		
		(outline-computations
		 (residue-class-sets (mget-residue-class-sets-from-cartesian-set cartprodresclsets))
		 (num-sets-to-resclass-sets (mnum-sets-of residue-class-sets))
		 (new-consts (type-new-consts (:type num) residue-class-sets))
		 (new-pair-term (mcreate-pair-term cartprodresclsets new-consts))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist new-pair-term)) phi))
		 (cs-in-num-sets (mcreate-items-in-numsets new-consts num-sets-to-resclass-sets)))
		
		(decl-content
		 (l1  ()   cs-in-num-sets                                                ("Hyp" () ()))
		 (l2  (l1)   phiprime                                                      ("Open" () ()))
		 (l3  ()     (forall-sort (lam (x aa var) phi) cartprodresclsets)          ("ForallI-Sort-Cart-product" () (l2))))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort-cart-product-resclass."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ExistsI-SORT-CART-PRODUCT-RESCLASS-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-Cart-Product-Resclass-m"
		 (outline-mappings (((existent nonexistent) ExistsI-Sort-Cart-Product-Resclass-m-b)))
		 (help "The method for FORALL-sort-Resclass introduction"))


(meth~defmethod ExistsI-Sort-Cart-Product-Resclass-m-b ExistsI-Sort-Cart-Product-Resclass-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (phiprime o)
		  (cartprodresclsets (o aa))
		  (cs-in-num-sets o)
		  (residue-class-sets list termlist)
		  (class-factors list termlist)
		  (num-sets-to-resclass-sets list termlist)
		  (new-mvs list termlist)
		  (new-pair-term aa)
		  (phiprime o)
		  (mvs-in-num-sets o)
		  )
		 )
		(premises (+ l2) (+ l1))
		(conclusions (- l3))
		
		(application-condition
		 (mcart-product-of-resclasses-p cartprodresclsets))
		
		(outline-computations
		 (residue-class-sets (mget-residue-class-sets-from-cartesian-set cartprodresclsets))
		 (num-sets-to-resclass-sets (mnum-sets-of residue-class-sets))
		 (new-mvs (type-new-mvs (:type num) residue-class-sets))
		 (new-pair-term (mcreate-pair-term cartprodresclsets new-mvs))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist new-pair-term)) phi))
		 (mvs-in-num-sets (mcreate-items-in-numsets new-mvs num-sets-to-resclass-sets)))
		
		(decl-content
		 (l1  ()   mvs-in-num-sets                                               ("Open" () ()))
		 (l2  ()   phiprime                                                      ("Open" () ()))
		 (l3  ()   (exists-sort (lam (x aa var) phi) cartprodresclsets)          ("ExistsI-Sort-Cart-product" () (l2))))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort-cart-product-resclass."))
		)
|#

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplify-That
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "SimplifyThat-m"
		 (outline-mappings (((existent nonexistent) SimplifyThat-m-b)))
		 (help "The method for the simplification of that expressions"))

(meth~defmethod SimplifyThat-m-b SimplifyThat-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (phi-prime o)
		  )
		 )
		
		(premises (+ l2))
		(conclusions (- l1))
		
		(application-condition
		 (mthat-expression-simplify-p phi phi-prime))
		
		(decl-content
		 (l2  () phi-prime   ("Open" () ()))     
		 (l1  () phi         ("Tac" () (l2))))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods reduces a goal with reducalbe that expressions."))
		)
|#

;; SimplifyThat-m lateron replaced by Apply-Def-function



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply-Def-Function-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "Apply-Def-Function-m"
		 (outline-mappings (((existent nonexistent existent) Apply-Def-Function-m-b)))
		 (help "The method for the simplification of function expressions"))

(meth~defmethod Apply-Def-Function-m-b Apply-Def-Function-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) (phi-prime o)
		  (function aa) (fc aa)
		  (poslist o positionlist)
		  ))
		
		(premises (+ l1) l0)
		(conclusions (- l2))
		
		(application-condition
		 (mand (abstr-p function)
		       (simplifiable-positions-p phi fc poslist)))
		
		(outline-computations
		 (phi-prime (simplify-at-positions phi poslist function)))
		
		(decl-content
		 (l0  () (= fc function)     )
		 (l1  () phi-prime          ("Open" () ()))     
		 (l2  () phi                ("Tac" () (l1 l2))))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods reduces a goal with reducalbe that expressions."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Or2Imp-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Or2Imp-m
                 (outline-mappings (((existent nonexistent) Or2Imp-m-b)))
                 (help "Tactic imp2or."))


(meth~defmethod Or2Imp-m-b Or2Imp-m
                (in base)
                (rating 0)
                (reasoning :planning)
                (declarations
                 (sorted-meta-variables
                  (phi o term)
                  (psi o term)))
		
                (premises (+ L1))
                (conclusions (- L2))
		
                (decl-content
                 (l1 () (or (not phi) psi)               ("open"   () ()))   
                 (l2 () (implies phi psi)                ("or2imp" () (l1)))
                 )
                (proc-content schema-interpreter)
		(manual (documentation "Applies or2imp backwards"))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert-Resclass-To-Num
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "Convert-Resclass-To-Num-m"
		 (outline-mappings (((existent nonexistent) Convert-Resclass-To-Num-m-b)))
		 (help "The method for Convertion of operations of residue classes to operations on natural numbers"))


(meth~defmethod Convert-Resclass-To-Num-m-b Convert-Resclass-To-Num-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (phi1 o)
		  )
		 )
		(premises (+ l1))
		(conclusions (- l2))

		(application-condition
		 (mand (mconvertable-residue-class-expressions-p phi)
		       (mbind phi1 (beta-normalize (mconvert-residue-class-expressions-pres phi)))
		       (mnot (mequal phi phi1))))
		
		;;(outline-computations
		;; (phi1 (beta-normalize (mconvert-residue-class-expressions phi)))
		;; )
		
		(decl-content
		 (l1 () phi1   ("Open" () ()))
		 (l2 () phi    ("Convertion" () (l1)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same converts backwards expressions with resclasses into expressions of natuaral numbers"))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMPLIFY-NUMERICAL-EXPR                                       ;;; expandable    VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Simplify-Numerical-Expr-m"
		 (outline-mappings (((existent nonexistent list) Simplify-Numerical-Expr-m-b)))
		 (help "The method for simplifying numerical expressions."))

(meth~defmethod Simplify-Numerical-Expr-m-b Simplify-Numerical-Expr-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (assumplist o prlnlist) (assumplist1 o prlnlist)
		  (poslist o positionlist) (poslistlist o positionlistlist)
		  )
		 )
		
		(just-parameters assumplist poslistlist)
		(premises  (+ l3) (0 assumplist))
		(conclusions (- l1))	
		
		(application-condition
		 (mnummerical-simplify phi phi-prime-prime (emptypos) l1 assumplist poslistlist))
		(expansion-computations
		 (phi-prime (apply-=subst**-b phi assumplist poslistlist))
		 (poslist (expand-num*-positions phi-prime))
		 (termlist (TermsAtPositions phi-prime poslist)))
		(decl-content
		 (l3 () phi-prime-prime    ("Open" () ()))
		 (l2 () phi-prime          ("expand-num*" (poslist termlist) (l3)))
		 (l1 () phi                ("=subst**" (poslistlist) (l2 assumplist)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This method evaluates numerical expressions and checks whether the formulas are true.")))


#| BRAUCHT MAN NICHT MEHR UND DAS IST GUT SO!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMPLIFY-RESCLASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Simplify-Resclass-m"
		 (outline-mappings (((existent nonexistent) Simplify-Resclass-m-b)))
		 (help "The method for simplifying numerical expressions."))

(meth~defmethod Simplify-Resclass-m-b Simplify-Resclass-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (goallist o prlnlist) 
		  (poslist o positionlist) 
		  )
		 )
		
		(just-parameters goallist poslist)
		(premises (+ l1) (+ goallist))
		(conclusions (- l2))	
		
		(application-condition
		 (msimplify-resclass phi phi-prime l2 goallist poslist))
		
		(decl-content
		 (l1 () phi-prime          ("Open" () ()))
		 (l2 () phi                ("Simplify-Resclass" (poslist) (l1 goallist)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This method evaluates numerical expressions and checks whether the formulas are true.")))
|#



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; DefnExp-m                                       ;;; expandable    VS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; copied from limit-methods! (regular definition expansion but with a parameter!)
;;
;;(infer~defmethod "DefnExp-m"
;;                 (outline-mappings (((existent nonexistent) DefnExp-m-b)))
;;		 (parameter-types term)
;;                (help "Applying the theory definition of a predicate or a function symbol."))
;;
;;(meth~defmethod DefnExp-m-b
;;               DefnExp-m
;;                (in zmz)
;;                (rating 0)
;;                (declarations
;;                ;;(type-variables aa)
;;                 (sorted-meta-variables
;;                  ;;(meta-var typ sort)
;;                  (phi o) (psi o) (zeta o)
;;		  (symbol o)
;;		  (symbol-term o term)
;;		  (thing-def o term)
;;                  )
;;                 )
;;		
;;                (premises (+ L1))
;;		(parameters symbol)
;;                (application-condition
;;		 (mand (bound-p symbol)
;;		       ;;(GetSymbolTerm symbol symbol-term)
;;		       (termoccs symbol phi occs)
;;		       (Th-Definition symbol thing-def)
;;		       ))
;;		
;;		(outline-computations
;;		 (zeta (beta-normalize (TermRplAtPos phi (mfirst occs) thing-def)))
;;		 )
;;                (conclusions (- L2))
;;                (decl-content
;;                 (l1 () zeta           ("OPEN" () ()))
;;                 (l2 () phi            ("DefnE" (symbol thing-def) (l1)))
;;                 
;;                 )
;;                (proc-content schema-interpreter)
;;                (remark "");Forward application of a theory definition.)
;;                )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reflex-m, NotReflex-m                                               ;;; Reflex-m expandable    VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; now in base, MP
;
;(infer~defmethod Reflex-m
;                 (outline-mappings (((existent) reflex-m-b)))
;                 (help "Closes a goal of the form 'a = a'"))
;
;(meth~defmethod reflex-m-b reflex-m
;                (in zmz)
;                (rating 50)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (type-variables aa)
;                 (sorted-meta-variables
;                  (phi aa term)
;                  (psi aa term)))
;                
;                (premises )
;                (conclusions (- L1))
;                
;                (application-condition
;                 ;;(mequal phi psi)
;                 (unify phi psi))
;                
;                (decl-content
;                 (l1 () (= phi psi) ("=ref" (phi) ()))))

;;;
;;; Inequality on numbers (a tactic needed in natural?)
;;;
		
(infer~defmethod NotReflex-m
		 (outline-mappings (((existent ) notreflex-m-b)))
		 (help "Closes a goal of the form '(not (n1 = n2))' where n1 and n2 are numbers that are unequal."))

(meth~defmethod notreflex-m-b notreflex-m
		(in zmz)
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi num term)
		  (psi num term)
		  (l2 aa prln)
		  (l3 aa prln)
		  ))
		
		(premises )
		(conclusions (- L1))
		
		(application-condition
		 (mand (number-p phi)
		       (number-p psi)
		       (mnot (number-equal-p phi psi))))
		(expansion-computations
		 (line (if (mgreater phi psi) l3 l2))
		 (less-symb (:symbol less))
		 (greater-symb (:symbol greater)))
		(decl-content
		 (l3 () (greater phi psi) ("Arith-Simplify" (greater-symb) ()))
		 (l2 () (less phi psi) ("Arith-Simplify" (less-symb) ()))
 		 (l1 () (not (= phi psi)) ("Assertion" () (line)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popmod
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Moves mod's to the outside of an equation:
;;; (c1 + (c2 + c3) mod n) mod n = (((c1 + c2) mod n) + c3) mod n -> (c1 + (c2 + c3)) mod n = ((c1 + c2) + c3) mod n
;;; (......) mod n = c1 + information that c1 < n -> (....) mod n = c1 mod n
;;; To be expanded with CAS

(infer~defmethod Popmod-m
		 (outline-mappings (((existent nonexistent) popmod-m-b)))
		 (help "Removes mods from a equational formula."))

(meth~defmethod popmod-m-b popmod-m
		(in zmz)
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi num term) (phiprime num term)
		  (psi num term) (psiprime num term)
		  (assump o prln)
		  (assumplist o prlnlist)
		  ))
		
		(premises (+ l2))
		(conclusions (- L1))
		
		(application-condition
		 (mmodpop-p phi psi phiprime psiprime l1 assump)
		 (mbind assumplist (mif (bound-p assump)
					(mlist assump)
					(mnil))))
		
		(decl-content
		 (l2 () (= phiprime psiprime)   ("OPEN" () ())) 
 		 (l1 () (= phi psi)             ("blabla" () ())))
		(proc-content schema-interpreter)
                (remark "")
                )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Embed plusgen, minusgen, timesges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod embedgen-m
		 (outline-mappings (((existent nonexistent) embedgen-m-b)))
		 (help "Removes mods from a equational formula."))

(meth~defmethod embedgen-m-b embedgen-m
		(in zmz)
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (f o term)
		  (fembedded o term)
		  (prems o prlnlist)
		  ))
		
		(premises (+ l2) (+ prems))
		(conclusions (- L1))
		
		(application-condition
		 (membed-generalized-function-p f l1 fembedded prems))
		
		(decl-content
		 (l2 () Fembedded                ("OPEN" () ())) 
 		 (l1 () F                        ("blabla" () ())))
		(proc-content schema-interpreter)
                (remark "")
                )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduce-StructUnit-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Reduce-StructUnit-m
		 (outline-mappings (((existent nonexistent) Reduce-StructUnit-m-b)))
		 (help "Reduces expressions with structunit."))

(meth~defmethod Reduce-StructUnit-m-b Reduce-StructUnit-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (f o term) (femb o term)		  
		  (structu aa)
		  (set (o bb))
		  (op (bb bb bb))
		  (poslist o positionlist)
		  (structuterm bb)
		  (assumplist o prlnlist)
		  (x bb)
		  ))
		
		(premises (+ l2))
		(conclusions (- L1))

		(just-parameters assumplist)
		(application-condition
		 (mand (mor ;;(mand (getsymbolterm "struct-unit" structu)
			    ;;	  (termoccs structu f poslist))
			    ;;(mand (getsymbolterm "group-unit" structu)
			    ;;	  (termoccs structu f poslist)))
			    (termoccs (:term struct-unit) f poslist)
			    (termoccs (:term group-unit) f poslist))
		       (structunitset+op f l1 poslist structuterm assumplist x)))
		(outline-computations
		 (femb (create-embedded-unit-term f structuterm x)))
		
		(decl-content
		 (l2 () femb                                         ("OPEN" () ()))
		 (l1 () f                                            ("blabla" () ())))
		(proc-content schema-interpreter)
                (remark "Solve equation.")
                )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduce-StructInverse-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Reduce-StructInverse-m
		 (outline-mappings (((existent nonexistent) Reduce-StructInverse-m-b)))
		 (help "Reduces expressions with structunit."))

(meth~defmethod Reduce-StructInverse-m-b Reduce-StructInverse-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (f o term) (femb o term)		  
		  (structu aa)
		  (set (o bb))
		  (op (bb bb bb))
		  (poslist o positionlist)
		  (structiterm bb)
		  ))
		
		(premises (+ l2))
		(conclusions (- L1))
		
		(application-condition
		 (mand ;;(mor (termoccs (:term struct-inverse) f poslist)
			    (termoccs (:term group-inverse) f poslist)
		       ;;	    )
		       (structinverse f poslist structiterm)))
		(outline-computations
		 (femb (create-embedded-inverse-term f structiterm)))
		
		(decl-content
		 (l2 () femb                                         ("OPEN" () ()))
		 (l1 () f                                            ("blabla" () ())))
		(proc-content schema-interpreter)
                (remark "Solve equation.")
                )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve-Equation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Solve-Equation-m
		 (outline-mappings (((existent) solve-equation-m-b)))
		 (help "Solves an equation with maple and might do some meta-variable bindings."))

(meth~defmethod solve-equation-m-b solve-equation-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi num term) (phiprime num term)
		  (psi num term) 
		  ))
		
		(premises )
		(conclusions (- L1))
		(application-condition
		 (equal-with-maple-p phi psi))
		(expansion-computations
		 (pos (:position ()))
		 (the-cas (:symbol mass)))

		(decl-content
		 (l2 () (= phi phi)             ("=ref" (phi) ())) 
 		 (l1 () (= phi psi)             ("cas" (pos the-cas) (l2))))
		(proc-content schema-interpreter)
                (remark "Solve equation.")
                )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve-Equation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Solve-Modulo-Equation-m
		 (outline-mappings (((existent) solve-modulo-equation-m-b)))
		 (help "Solves an equation involving modulo computations with maple and might do some meta-variable bindings."))

(meth~defmethod solve-modulo-equation-m-b solve-modulo-equation-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi num term) (phiprime num term)
		  (psi num term) (n num term)
		  ))
		
		(premises )
		(conclusions (- L1))
		(application-condition
		 (equal-modulo-with-maple-p phi psi n))
		(expansion-computations
		 (pos (:position ()))
		 (the-cas (:symbol mass)))

		(decl-content
		 (l2 () (= (mod phi n) (mod phi n))             ("=ref" (phi) ()))
 		 (l1 () (= (mod phi n) (mod psi n))             ("cas" (pos the-cas) (l2))))
		(proc-content schema-interpreter)
                (remark "Solve equation involving a modulo function.")
                )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DecompFunctionInEquation                                       ;;; expandable    VS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(infer~defmethod DecompFunctionInEquation-m
                (outline-mappings (((existent nonexistent) DecompFunctionInEquation-m-b)))
                (help "Decomposes an equation of the form op(a1 a2) = op(a'1 a2) to a equation a1=a'1"))


(meth~defmethod DecompFunctionInEquation-m-b DecompFunctionInEquation-m
               (in zmz)
               (rating 55)
               (reasoning :planning :middle-out)
               (declarations
                (sorted-meta-variables
                 (pos o pos)
                 (op (num num num) term)
                 (a num term)
                 (b num term)
                 (c num term)
                 (refterm o num)
                 ))
               
               (premises (+ l2))
               (conclusions (- l3))
               
               (application-condition
                (mor (mequal op (:term mod))
                     (mequal op (:term plus))
                     (mequal op (:term times))
                     (mequal op (:term minus))
                     (mequal op (:term div))))
               
               (expansion-computations
                (pos (:position (2 1)))
                (refterm (appl-create op (mlist a c))))
               
               (decl-content
                (l1 () (= (op a c) (op a c))       ("=ref" (refterm) ()))
                (l2 () (= a b)                     ("Open" () ()))
                (l3 () (= (op a c) (op b c))       ("=subst" (pos) (l1 l2)))
                )
               
               (proc-content schema-interpreter)
               (manual (documentation "This method decomposes an equation of the form op(a1 a2) = op(a'1 a2) to a equation a1=a'1"))
               )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpClosed-Under
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod OpClosedUnder-m
		 (outline-mappings (((existent nonexistent) OpClosedUnder-m-b))
				   (help "Reduces a goal (or (complex-expr = ...) (complex-expr = ...) ....) to a closed-under goal.")))

(meth~defmethod OpClosedUnder-m-b OpClosedUnder-m
		(in zmz)
		(rating 55)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (psi o)
		  (assumplist o prlnlist)
		  ))
		
		(premises (+ l1))
		(conclusions (- l2))

		(just-parameters assumplist)
		
		(application-condition
		 (mexpression-closed-under phi l2 psi assumplist))
		
		(outline-actions
		 (l1 (unsponsor assumplist)))
		
		(decl-content
		 (l1 () psi      ("Open" () ()))
		 (l2 () phi      ("Closed" () ()))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This method creates a closed-under goal."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ApplyAssertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "MAssertion-m"
		 (outline-mappings (((existent) MAssertion-m-b)))
		 (parameter-types problem))

(meth~deffun get-th-ass (ass)
  (declare (edited  "06-JUN-2001")
	   (authors Sorge)
	   (input   "A theory assertion.")
	   (effect  "Inserts the assertion into the proof.")
	   (value   "The newly created proof line."))
  (pds~add-thy-assertion ass))
	     

(meth~defmethod MAssertion-m-b MAssertion-m
		(in base)
		(rating 3)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (sym o)
		  (psi o term)
		  (phi o problem)
		  (pformulas list termlist)
		  (prems o prlnlist)))
		
		(parameters phi)
		(premises (+ prems))
		(conclusions (- l2))
		
		(application-condition
		 (mand ;;(literal-p psi)
		  (bound-p phi)
		  (myassert (theorem-formula phi) psi pformulas)))
		(outline-computations
		 (prems (if (mequal pformulas (mnil))
			    (mnil)
			  (premises-for l2 pformulas))))
		(expansion-computations
		 (new-prems (mcons (get-th-ass phi) prems))
		 (sym (:symbol t)))
		(decl-content
		 ;(l2 () psi ("Assertion" () new-prems))) Otter more reliable ->
		 (l2 () psi ("Otter" (sym) new-prems))) 
		(proc-content schema-interpreter ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ReductionClosed-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ReductionClosed-m"
		 (outline-mappings (((existent nonexistent nonexistent) ReductionClosed-m-b))
				   ))


(meth~defmethod ReductionClosed-m-b ReductionClosed-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (n num term)
		  (op ((o num) (o num) (o num)))
		  (op1 ((o num) (o num) (o num)))
		  (op2 ((o num) (o num) (o num)))
		  ))
		
		(premises (+ l0) (+ l1))
		(conclusions (- l2))
		
		(application-condition
		 (combindoperation op op1 op2))
		
		(decl-content
		 (l0 () (closed-under (resclass-set n) op1)   ("open" () ()))
		 (l1 () (closed-under (resclass-set n) op2)   ("open" () ()))
		 (l2 () (closed-under (resclass-set n) op)    ("ReductionClosed" () (l1 l0)))
		 )
		
		(proc-content schema-interpreter )		
		
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Int-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Int-m"
		 (outline-mappings (((existent) Int-m-b))
				   ))


(meth~deffun th-get-def (term)
  (declare (edited  "06-JUN-2001")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "Two values, the definiens and the definiendum."))
	     (let ((ass (th~find-assumption term omega*current-theory)))
	       (when (th~definition-p ass)
		 (list (th~definition-constant ass)
			 (th~ass-node ass)))))
			  
(meth~defmethod Int-m-b Int-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  ))
		
		(premises )
		(conclusions (- l1))

		(application-condition
		 (intnumber-p a))
		(expansion-computations
		 (in-symb (:symbol in))
		 (posi (:position ()))
		 ((def1 def2) (th-get-def :in))
		 )
		
		(decl-content
		 (l2 () (in a int) ("Arith-Simplify" (in-symb) ()))
		 (l1 () (int a) ("Defne" (def1 def2 posi) (l2))))
		
		(proc-content schema-interpreter ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SubsetResclass-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "SubsetResclass-m"
		 (outline-mappings (((existent) SubsetResclass-m-b))
				   ))


(meth~defmethod SubsetResclass-m-b SubsetResclass-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (rs1 (o (o num)) term)
		  (rs2 (o (o num)) term)
		  ))
		
		(premises )
		(conclusions (- l1))

		(application-condition
		 (subsetrcl-p rs1 rs2))
		
		(decl-content
		 (l1 () (subset rs1 rs2) ("SubsetRCL" () ())))
		
		(proc-content schema-interpreter ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IncludeTheorems-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "IncludeTheorems-m"
		 (outline-mappings (((existent) IncludeTheorems-m-b))
				   ))

(meth~defmethod IncludeTheorems-m-b IncludeTheorems-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (new-hyps o prlnlist)
		  ))
		
		(premises (+ l0))
		(conclusions (- l1))
		
		(application-condition
		 (theoremsintheory-p new-hyps))
		
		(outline-actions
		 (l0 (sponsor-if-hyp new-Hyps)))
		
		(decl-content
		 (l0 (new-hyps) phi ("open" () ()))
		 (l1 () phi ("Thinsert" () (l0))))
		
		(proc-content schema-interpreter ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InResclass-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "InResclass-m"
		 (outline-mappings (((existent) InResclass-m-b))
				   ))


(meth~defmethod InResclass-m-b InResclass-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  ))
		
		(premises )
		(conclusions (- l1))

		(application-condition
		 (inrsrc-p phi))
		
		(decl-content
		 (l1 () phi ("InRCL" () ())))
		
		(proc-content schema-interpreter ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SimplifyOrder-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "SimplifyOrder-m"
		 (outline-mappings (((existent nonexistent) SimplifyOrder-m-b))
				   ))


(meth~defmethod SimplifyOrder-m-b SimplifyOrder-m
		(in zmz)
		(rating 3)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (phiprime o term)
		  ))
		
		(premises (+ l2))
		(conclusions (- l1))
		
		(application-condition
		 (morder-expression-simplify-p phi phiprime))
		
		(decl-content
		 (l2 () phiprime     ("Open" () ()))
		 (l1 () phi          ("SimplifyOrder" () (l2))))
		
		(proc-content schema-interpreter))



;;; the following methods should go into typed-set one day.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DecompPairinEquation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod DecompPairinEquation-m                   
		 (outline-mappings (((existent nonexistent nonexistent) DecompPairinEquation-m-b)))
		 (help ""))


(meth~defmethod  DecompPairinEquation-m-b  DecompPairinEquation-m
		(in zmz)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (a aaa term)
		  (b aaa term)
		  (c bbb term)
		  (d bbb term)
		  ))
		
		(premises (+ l10)(+ l20))
		(conclusions (- l30))
		
		(application-condition)
				
		(outline-computations)
		
		(decl-content
		 (l10 () (= a b)                     ("Open" () ()))
		 (l20 () (= c d)                     ("Open" () ()))
		 (l30 () (= (pair a c) (pair b d))   ("funcequal" () (l10 l20)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DecompPairinInequation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod DecompPairinInequation-m                   
		 (outline-mappings (((existent nonexistent) DecompPairinInequation-m-b)))
		 (help ""))


(meth~defmethod  DecompPairinInequation-m-b  DecompPairinInequation-m
		(in zmz)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (a aaa term)
		  (b aaa term)
		  (c bbb term)
		  (d bbb term)
		  ))
		
		(premises (+ l10))
		(conclusions (- l30))
		
		(application-condition)
				
		(outline-computations)
		
		(decl-content
		 (l10 () (or (not (= a b))(not (= c d)))    ("Open" () ()))
		 (l30 () (not (= (pair a c) (pair b d)))   ("funcequal" () (l10)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DecompElementOfCartProd                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod DecompElementOfCartProd-m
		 (outline-mappings (((existent nonexistent nonexistent) DecompElementOfCartProd-m-b)))
		 (help ""))


(meth~defmethod DecompElementOfCartProd-m-b DecompElementOfCartProd-m
		(in zmz)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (a aaa term)
		  (b bbb term)
		  (U (o aaa) term)
		  (V (o bbb) term)
		  ))
		
		(premises (+ l10)(+ l20))
		(conclusions (- l30))
		
		(application-condition)
		
		(outline-computations)
		
		(decl-content
		 (l10 () (U a)                     ("Open" () ()))
		 (l20 () (V b)                     ("Open" () ()))
		 (l30 () ((Cartesian-Product U V) (pair a b))    ("defexpand" () (l10 l20)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DecompNotElementOfCartProd                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod DecompNotElementOfCartProd-m
		 (outline-mappings (((existent nonexistent) DecompNotElementOfCartProd-m-b)))
		 (help ""))


(meth~defmethod DecompNotElementOfCartProd-m-b DecompNotElementOfCartProd-m
		(in zmz)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aaa bbb)
		 (sorted-meta-variables
		  (a aaa term)
		  (b bbb term)
		  (U (o aaa) term)
		  (V (o bbb) term)
		  ))
		
		(premises (+ l10))
		(conclusions (- l30))
		
		(application-condition)
		
		(outline-computations)
		
		(decl-content
		 (l10 () (or (not (U a)) (not (V b)))                  ("Open" () ()))
		 (l30 () (not ((Cartesian-Product U V) (pair a b)))    ("defexpand" () (l10)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand-Pair-Operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Expand-Pair-Operation-m"
		 (outline-mappings (((existent) Expand-Pair-Operation-m-b)))
		 (help ""))

(meth~defmethod Expand-Pair-Operation-m-b Expand-Pair-Operation-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables ccc aaa bbb)
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (pairop ccc)
		  (poslist o positionlist)
		  ;;(new-Prem o prln)
		  )
		 )
		
		(premises (+ l0))
		(conclusions (- l100))	
		
		(application-condition
		 (mand (getsymbolterm "pair-operation" pairop)
		       (termoccs pairop phi poslist)
		       (th-definition (termatpos phi (mfirst poslist)) pairopdef)
		       ;;(test-tactic (:symbol defni*)
		       ;;			 (mlist l100 (mnil))
		       ;;	 		 (mlist pairop pairopdef poslist))
		       ))
		
		(outline-computations
		 ;;(new-prem (msecond (apply-tactic (:symbol defni*)
		 ;;				  (mlist l100 (mnil))
		 ;;				  (mlist pairop pairopdef poslist)))))
		 (phi-prime (apply-defni* phi pairop pairopdef poslist)))
		
		(decl-content
		 (l0   () phi-prime          ("Open" () ()))
		 (l100 () phi                ("defni*" (pairop pairopdef poslist) (l90)))
		 )

		(proc-content schema-interpreter)
		;; (proc-content apply-tactic)
		(manual (documentation "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewrite-First-Second-Pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Rewrite-First-Second-Pair-m"
		 (outline-mappings (((existent nonexistent) Rewrite-First-Second-Pair-m-b)))
		 (help ""))

(meth~defmethod Rewrite-First-Second-Pair-m-b Rewrite-First-Second-Pair-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables ccc aaa bbb)
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (pairop ccc const) (pos o position)(pos2 o position)
		  (poslist o positionlist)
		  )
		 )
		
		(premises (+ l10))
		(conclusions (- l100))	
		
		(application-condition
		 (mand (mbind phi-prime-prime (rewrite-first-second-pair phi))
		       (mnot (mequal phi phi-prime-prime))))
		      
		(outline-computations)
		
		(decl-content
		 (l10  () phi-prime-prime    ("open" ()()))
		 (l100 () phi                ("replace" ()(l10)))
		 )

		(proc-content  schema-interpreter)
		(manual (documentation "")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand-Scalar-Operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Expand-Scalar-Operation-m"
		 (outline-mappings (((existent) Expand-Scalar-Operation-m-b)))
		 (help ""))

(meth~defmethod Expand-Scalar-Operation-m-b Expand-Scalar-Operation-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables ccc aaa bbb)
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (scalarop ccc)
		  (poslist o positionlist)
		  ;;(new-Prem o prln)
		  )
		 )
		
		(premises (+ l0))
		(conclusions (- l100))	
		
		(application-condition
		 (mand (getsymbolterm "scalar-operation" scalarop)
		       (termoccs scalarop phi poslist)
		       (th-definition (termatpos phi (mfirst poslist)) scalaropdef)
		       ;;(test-tactic (:symbol defni*)
		       ;;			 (mlist l100 (mnil))
		       ;;	 		 (mlist pairop pairopdef poslist))
		       ))
		
		(outline-computations
		 ;;(new-prem (msecond (apply-tactic (:symbol defni*)
		 ;;				  (mlist l100 (mnil))
		 ;;				  (mlist pairop pairopdef poslist)))))
		 (phi-prime (apply-defni* phi scalarop scalaropdef poslist)))
		
		(decl-content
		 (l0   () phi-prime          ("Open" () ()))
		 (l100 () phi                ("defni*" (pairop pairopdef poslist) (l90)))
		 )
		
		(proc-content schema-interpreter)
		;; (proc-content apply-tactic)
		(manual (documentation "")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduce-Forall-To-Pairs-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Reduce-Forall-To-Pairs-m"
		 (outline-mappings (((existent nonexistent) Reduce-Forall-To-Pairs-m-b)))
		 (help "The method for Reducing a universal on a cartesian product to foralls on the pairs."))


(meth~defmethod Reduce-Forall-To-Pairs-m-b Reduce-Forall-To-Pairs-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (phi o)  (phiprime o)
		  (set1 (o aa)) (set2 (o bb))
		  (x1 aa var) (x2 bb var)))
		
		(premises (+ l0))
		(conclusions (- l1))
		
		(outline-computations
		 (x1 (type-newvar (:type aa)))
		 (x2 (type-newvar (:type bb)))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist (appl-create (:term pair) (mlist x1 x2)))) phi)))
		
		(decl-content
		 (l0 () (forall-sort (lam (x1 aa var)
					  (forall-sort (lam (x2 bb var)
							    phiprime)
						       set2))
				     set1)                                                              ("Open" () ()))
		 (l1 () (forall-sort (lam (x (tuple aa bb) var) phi) (cartesian-product set1 set2))     ("Reduce-Cart-Forall" () (l0))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduce-Exists-To-Pairs-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Reduce-Exists-To-Pairs-m"
		 (outline-mappings (((existent nonexistent) Reduce-Exists-To-Pairs-m-b)))
		 (help "The method for Reducing a universal on a cartesian product to existss on the pairs."))


(meth~defmethod Reduce-Exists-To-Pairs-m-b Reduce-Exists-To-Pairs-m
		(in zmz)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (phi o) (phiprime o)
		  (set1 (o aa)) (set2 (o bb))
		  (x1 aa var) (x2 bb var)))
		
		(premises (+ l0))
		(conclusions (- l1))
		
		(outline-computations
		 (x1 (type-newvar (:type aa)))
		 (x2 (type-newvar (:type bb)))
		 (phiprime (subst-apply (subst-create (mlist x) (mlist (appl-create (:term pair) (mlist x1 x2)))) phi)))
		
		(decl-content
		 (l0 () (exists-sort (lam (x1 aa var)
					  (exists-sort (lam (x2 bb var)
							    phiprime)
						       set2))
				     set1)                                                              ("Open" () ()))
		 (l1 () (exists-sort (lam (x (tuple aa bb) var) phi) (cartesian-product set1 set2))     ("Reduce-Cart-Exists" () (l0))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)


;;; the preceding methods should go into typed-set one day.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THE FOLLOWING ARE THE METHODS NEEDED IN THE NOTINJNOTISO STRATEGY
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NotInjNotIso-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "NotInjNotIso-m"
		 (outline-mappings (((existent nonexistent) NotInjNotIso-m-b)))
		 (help "The method for reducing not-iso to not-inj"))

(meth~defmethod NotInjNotIso-m-b NotInjNotIso-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (set1 (o aa)) (set2 (o bb))
		  (op1 (aa aa aa)) (op2 (bb bb bb))
		  (hprime (bb aa) const)
		  (assumplist o prlnlist)
		  )
		 )
		
		(premises (+ l7))
		(conclusions (- l10))
				
		(outline-computations
		 (hprime (type-newconst (:type (bb aa))))
		 )

		(outline-actions
		 (l7 (unsponsor l1)))
		
		(decl-content
		 (l1 ()      ((functions set1 set2) hprime)                                ("Hyp" () ()))
		 (l2 ()      (homomorphism set1 op1 set2 op2 hprime)                       ("Hyp" () ()))
		 (l7 (l1 l2) (exists-sort (lam (x1 aa)
					       (exists-sort (lam (x2 aa)
								 (and (not (= x1 x2))
								      (= (hprime x1) (hprime x2))))
							    set1))
					  set1)                                            ("Open" () ()))
		 (l8 (l1)    (implies (homomorphism set1 op1 set2 op2 hprime)
				      (exists-sort (lam (x1 aa)
							(exists-sort (lam (x2 aa)
									  (and (not (= x1 x2))
									       (= (hprime x1) (hprime x2))))
								     set1))
						   set1))                                  ("ImpliesI" () (l7)))
		 (l9 ()      (forall-sort (lam (h (bb aa))
					       (implies (homomorphism set1 op1 set2 op2 h)
							(exists-sort (lam (x1 aa)
									  (exists-sort (lam (x2 aa)
											    (and (not (= x1 x2))
												 (= (h x1) (h x2))))
										       set1))
								     set1)))
					  (functions set1 set2))                           ("ForallI-sort" (hprime) l8))
		 (l10 ()     (not (isomorphic set1 op1 set2 op2))                          ("Not-Inj-Not-Iso" () (l9)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort-resclass"))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InsertHomEqus-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "InsertHomEqus-m"
		 (outline-mappings (((nonexistent existent) InsertHomEqus-m-f)))
		 (help "Introduces the equations of the homomorphism"))

(meth~defmethod InsertHomEqus-m-f InsertHomEqus-m
		(in zmz)
		(rating 0.3)
		(reasoning :normalizing)
		(declarations
		 (sorted-meta-variables
		  (set1 (o (o num))) (set2 (o (o num))) 
		  (op1 ((o num) (o num) (o num))) (op2 ((o num) (o num) (o num)))
		  (h ((o num) (o num)))
		  (assumplist o prlnlist)
		  )
		 )
		
		(premises (- l1))
		(conclusions (+ assumplist))
		
		(outline-computations
		 (assumplist (compute-hom-lines set1 op1 op2 h l1)))
		
		(decl-content
		 (l1 () (homomorphism set1 op1 set2 op2 h))
		 )
		
		(proc-content schema-interpreter)
		(remark "")
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inequality on resclasses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod NotResclReflex-m
		 (outline-mappings (((existent) notresclreflex-m-b)))
		 (help ""))

(meth~defmethod notresclreflex-m-b notresclreflex-m
		(in zmz)
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi num term)
		  (psi num term)
		  (fac1 num term)
		  (fac2 num term)))
		
		(premises )
		(conclusions (- L1))
		
		(application-condition
		 (mand (number-p phi)
		       (number-p psi)
		       (number-p fac1)
		       (number-p fac2)
		       (mor (mnot (number-equal-p phi psi))
			    (mnot (number-equal-p fac1 fac2)))))
		
		(decl-content
 		 (l1 () (not (= (resclass fac1 phi) (resclass fac2 psi))) ("not=resclref" () ()))
		 )
		
		(proc-content schema-interpreter)
		(remark "")
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FiniteSetAsDisj-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod FiniteSetAsDisj-m
		 (outline-mappings (((existent nonexistent) FiniteSetAsDisj-m-b)
				    ((nonexistent existent) FiniteSetAsDisj-m-f)))
		 (help ""))

(meth~defmethod FiniteSetAsDisj-m-b FiniteSetAsDisj-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (disj o))
		 )
		
		(premises (+ l1))
		(conclusions (- l2))
		
		(application-condition
		 (in-finite-set-p phi)
		 )

		(outline-computations
		 (disj (rewrite-finite-set-to-disj phi)))
		
		(decl-content
		 (l1 () disj       ("Open" () ())) 
 		 (l2 () phi        ("RewriteFriniteSet" () (l1)))
		 )
		
		(proc-content schema-interpreter)
		(remark "")
		)

(meth~defmethod FiniteSetAsDisj-m-f FiniteSetAsDisj-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (disj o))
		 )
		
		(premises (- l1))
		(conclusions (+ l2))
		
		(application-condition
		 (in-finite-set-p phi)
		 )

		(outline-computations
		 (disj (rewrite-finite-set-to-disj phi)))
		
		(decl-content
		 (l1 () phi      )
 		 (l2 () disj     ("RewriteFriniteSet" () (l1)))
		 )
		
		(proc-content schema-interpreter)
		(remark "")
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve-Equation-Mod-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Solve-Equation-Mod-m
		 (outline-mappings (((existent) Solve-Equation-Mod-m-b)))
		 (help "Solves an equation involving modulo computations with maple."))

(meth~defmethod Solve-Equation-Mod-m-b Solve-Equation-Mod-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi (o num) term)
		  (psi (o num) term)
		  (class-factor num term)
		  (h ((o num) (o num)) term) 
		  ))
		
		(premises )
		(conclusions (- L1))
		
		(application-condition
		 (mand (hom-into-resclass phi psi l1 class-factor h)
		       (equal-modulo-maple phi psi class-factor h))
		 )
		
		(decl-content
		 (l1 () (= phi psi)                 ("cas" (pos the-cas) (l2)))
		 )
		
		(proc-content schema-interpreter)
		(remark "Solve equation involving a modulo function.")
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTISOSYM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "NotIsoSym-m"
		 (outline-mappings (((existent nonexistent) NotIsoSym-m-b)))
		 (help "The method for flipping the structures in a not-iso problem"))

(meth~defmethod NotIsoSym-m-b NotIsoSym-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (set1 (o aa)) (set2 (o bb))
		  (op1 (aa aa aa)) (op2 (bb bb bb))))
		
		(premises (+ l1))
		(conclusions (- l2))
		
		(decl-content
		 (l1 ()     (not (isomorphic set2 op2 set1 op1))                          ("Open" () ()))
		 (l2 ()     (not (isomorphic set1 op1 set2 op2))                          ("Not-Inj-Not-Iso" () (l1)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic NotIsoSym"))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-Solve-Equation-Mod-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod General-Solve-Equation-Mod-m
		 (outline-mappings (((existent) General-Solve-Equation-Mod-m-b)))
		 (help ""))

(meth~defmethod General-Solve-Equation-Mod-m-b General-Solve-Equation-Mod-m
		(in zmz)
		(rating 50)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi (o num) term)
		  (psi (o num) term)
		  (class-factor num term)
		  (h ((o num) (o num)) term)
		  (counter-node o prln)
		  (pos1 o position)
		  (pos2 o position)
		  ))
		
		(premises )
		(conclusions (- L1))

		(parameters counter-node pos1 pos2)
		
		(application-condition
		 (mand (hom-into-resclass phi psi l1 class-factor h)
		       (find-partner-in-chain l1 counter-node pos1 pos2))
		 )
		
		(decl-content
		 (l1 () (= phi psi)                 ("cas-over-node" (pos1 pos2) (counter-node)))
		 )
		
		(proc-content schema-interpreter)
		(remark "Solve equation involving a modulo function.")
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TWO ADDITIONAL METHODS FOR PRESENTATION THAT CONVERT (in ITEM SET) zu (beta-normalize (set item))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I need them -> moved to group. MP
;
;(infer~defmethod "Expand-in-m"
;                 (outline-mappings (((existent nonexistent) Expand-in-m-b)
;                                    ((nonexistent existent) Expand-in-m-f)))
;                 (help "The method for expanding the in."))
;
;
;(meth~defmethod Expand-in-m-b Expand-in-m
;                (in zmz)
;                (rating 10)
;                (reasoning :planning)
;                (declarations
;                 (type-variables aa)
;                 (sorted-meta-variables
;                  (phi o) 
;                  (set (o aa))
;                  (a aa)))
;                
;                (premises (+ l0))
;                (conclusions (- l1))
;                
;                (outline-computations
;                 (phi (beta-normalize (appl-create set (mlist a)))))
;                
;                (decl-content
;                 (l0 () phi                             ("Open" () ()))
;                 (l1 () (in a set)                      ("Defn-Expand" (l0) (in))))
;                
;                (proc-content schema-interpreter)
;                (manual (documentation ""))
;                )
;
;(meth~defmethod Expand-in-m-f Expand-in-m
;                (in zmz)
;                (rating 10)
;                (reasoning :planning)
;                (declarations
;                 (type-variables aa)
;                 (sorted-meta-variables
;                  (phi o) 
;                  (set (o aa))
;                  (a aa)))
;                
;                (premises (- l0))
;                (conclusions (+ l1))
;                
;                (outline-computations
;                 (phi (beta-normalize (appl-create set (mlist a)))))
;                
;                (decl-content
;                 (l0 () (in a set))
;                 (l1 () phi                             ("Defn-Expand" (l0) (in))))
;                
;                (proc-content schema-interpreter)
;                (manual (documentation ""))
;                )



(infer~defmethod "Expand-not-in-m"
		 (outline-mappings (((existent nonexistent) Expand-not-in-m-b)
				    ((nonexistent existent) Expand-not-in-m-f)))
		 (help "The method for expanding the in."))


(meth~defmethod Expand-not-in-m-b Expand-not-in-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) 
		  (set (o aa))
		  (a aa)))
		
		(premises (+ l0))
		(conclusions (- l1))
		
		(outline-computations
		 (phi (beta-normalize (appl-create set (mlist a)))))
		
		(decl-content
		 (l0 () (not phi)                             ("Open" () ()))
		 (l1 () (not (in a set))                      ("Defn-Expand" (l0) (in))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)

(meth~defmethod Expand-not-in-m-f Expand-not-in-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) 
		  (set (o aa))
		  (a aa)))
		
		(premises (- l0))
		(conclusions (+ l1))
		
		(outline-computations
		 (phi (beta-normalize (appl-create set (mlist a)))))
		
		(decl-content
		 (l0 () (not (in a set)))
		 (l1 () (not phi)                             ("Defn-Expand" (l0) (in))))
		
		(proc-content schema-interpreter)
		(manual (documentation ""))
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dist-Prop-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Dist-Prop-m"
		 (outline-mappings (((existent nonexistent nonexistent nonexistent nonexistent nonexistent) Dist-Prop-m-b)))
		 (help ""))

(meth~defmethod Dist-Prop-m-b Dist-Prop-m
		(in zmz)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa bb)
		 (sorted-meta-variables
		  (set1 (o (o num))) (set2 (o (o num)))
		  (op1 ((o num) (o num) (o num))) (op2 ((o num) (o num) (o num)))
		  (cset1 (o (o num))) (cset2 (o (o num)))
		  (cop1 ((o num) (o num) (o num))) (cop2 ((o num) (o num) (o num)))
		  (mvprop (o ((o num) (o num) (o num)) (o (o num))) metavar)
		  ))
		
		(premises (+ l0) (+ l1) (+ l3) (+ l4) (+ l17))
		(conclusions (- l20))
		
		(outline-computations
		 (mvprop (newmetavar (:symbol :MPROP) (:type (o ((o num) (o num) (o num)) (o (o num))))))
		 (cset1 (type-newconst (:type (o (o num)))))
		 (cset2 (type-newconst (:type (o (o num)))))
		 (cop1 (type-newconst (:type ((o num) (o num) (o num)))))
		 (cop2 (type-newconst (:type ((o num) (o num) (o num))))))

		(decl-content
		 (l0  ()     (mvprop set1 op1)                                                               ("Open" () ()))
		 (l1  ()     (not (mvprop set2 op2))                                                         ("Open" () ()))
		 (l2  ()     (and (mvprop set1 op1)
				  (not (mvprop set2 op2)))                                                   ("AndI" () (l0 l1)))
		 (l3  ()     (closed-under set1 op1)                                                         ("Open" () ()))
		 (l4  ()     (closed-under set2 op2)                                                         ("Open" () ()))
		 (l5  ()     (and (closed-under set1 op1) 
				  (closed-under set2 op2))                                                   ("AndI" () (l3 l4)))
		 (l6  ()     (and (and (closed-under set1 op1) 
				       (closed-under set2 op2))
				  (and (mvprop set1 op1)
				       (not (mvprop set2 op2))))                                             ("AndI" () (l5 l2)))
		 
		 (l17 ()     (forall (lam (set1 (o (o num)))
			      (forall (lam (op1 ((o num) (o num) (o num)))
			       (forall (lam (set2 (o (o num)))
				(forall (lam (op2 ((o num) (o num) (o num)))
				 (implies (and (and (forall-sort (lam (x1 (o num))
								      (forall-sort (lam (x2 (o num))
											(set1 (op1 x1 x2)))
										   set1))
								 set1)
						    (forall-sort (lam (y1 (o num))
								      (forall-sort (lam (y2 (o num))
											(set2 (op2 y1 y2)))
										   set2))
								 set2))
					       (and (mvprop set1 op1)
						    (not (mvprop set2 op2))))
					  (not (exists (lam (h ((o num) (o num)))
							    (exists (lam (j ((o num) (o num)))
									 (and (and (and (forall-sort (lam (x (o num))
													  (set2 (h x)))
												     set1)
											(forall-sort (lam (x (o num))
													  (set1 (j x)))
												     set2))
										   (and (forall-sort (lam (x (o num))
													  (forall-sort (lam (y (o num))
															    (= (h (op1 x y)) (op2 (h x) (h y))))
														       set1))
												     set1)
											(forall-sort (lam (x (o num))
													  (forall-sort (lam (y (o num))
															    (= (j (op2 x y)) (op1 (j x) (j y))))
														       set2))
												     set2)))
									      (and (forall-sort (lam (x (o num))
												     (= (j (h x)) x))
												set1)
										   (forall-sort (lam (x (o num))
												     (= (h (j x)) x))
												set2)))))))))))))))))    ("Open" () ()))
		 (l18 ()     (forall (lam (x1 (o (o num)))
			      (forall (lam (o1 ((o num) (o num) (o num)))
			       (forall (lam (x2 (o (o num)))
				(forall (lam (o2 ((o num) (o num) (o num)))
					     (implies (and (and (closed-under x1 o1)
								(closed-under x2 o2))
							   (and (mvprop x1 o1)
								(not (mvprop x2 o2))))
						      (not (isomorphic x1 o1 x2 o2)))))))))))                ("DefnI*" (closed-under) (l17)))
		 (l19 ()      (implies (and (and (closed-under set1 op1)
						(closed-under set2 op2))
					   (and (mvprop set1 op1)
						(not (mvprop set2 op2))))
				      (not (isomorphic set1 op1 set2 op2)))                                  ("ForallE*" (set1 op1 set2 op2) (l18)))
		 (l20 ()     (not (isomorphic set1 op1 set2 op2))                                            ("Impe" () (l19 l6)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application of the tactic ExistsI-sort-resclass"))
		)
