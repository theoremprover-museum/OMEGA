(in-package :omega)

(learn~compile-learned-stuff
      (append
       (learn~produce-method 'invr-i-m 'learn 'invr-i '((p position pos+position pos)))
       (learn~produce-method 'invl-i-m 'learn 'invl-i '((p position pos+position pos)))
       (learn~produce-method 'idr-i-m 'learn 'idr-i '((p position pos+position pos)))
       (learn~produce-method 'idl-i-m 'learn 'idl-i '((p position pos+position pos)))
       (learn~produce-method 'invr-e-m 'learn 'invr-e '((p position pos+position pos)))
       (learn~produce-method 'invl-e-m 'learn 'invl-e '((p position pos+position pos)))
       (learn~produce-method 'idr-e-m 'learn 'idr-e '((p position pos+position pos)))
       (learn~produce-method 'idl-e-m 'learn 'idl-e '((p position pos+position pos)))
       (learn~produce-method 'assoc-l-m 'learn 'assoc-l '((p position pos+position pos)))
       (learn~produce-method 'assoc-r-m 'learn 'assoc-r '((p position pos+position pos)))
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reflex-m, NotReflex-m                                               ;;; Reflex-m expandable    VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Reflex-m
		 (outline-mappings (((existent) reflex-m-b)))
		 (help "Closes a goal of the form 'a = a'"))

(meth~defmethod reflex-m-b reflex-m
		(in learn)
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi aa term)
		  (psi aa term)))
		
		(premises )
		(conclusions (- L1))
		
		(application-condition
		 ;;(mequal phi psi)
		 (unify phi psi))
		
		(decl-content
 		 (l1 () (= phi psi) ("=ref" (phi) ()))))

;
;;;******************************
;;;  Normalise Method
;;;******************************
;
;(infer~defmethod normalise-m
;                 (outline-mappings (((existent nonexistent existent) normalise-m-b)))
;                 (help "Applies ...."))
;
;(meth~defmethod normalise-m-b normalise-m
;                (in learn)
;                (rating 90)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (sorted-meta-variables
;                  (exp o term)
;                  (lhs-exp i term)
;                  (rhs-exp i term)
;                  (normal-exp o term)
;                  (lhs-normal-exp i term)
;                  (rhs-normal-exp i term)
;                  (oper (o i i) term)
;                  (termlist-lhs i termlist)
;                  (termlist-rhs i list)
;                  (op1 (i i i) term)
;                  (inv1 (i i) term)
;                  (e1 i term)
;                  (g1 (o i) term)
;;                 (param o termlist)
;                  )
;                 )
;                
;                (premises (+ l10) l20)
;                
;                (application-condition
;                 (mand
;                  (appl-p exp)
;                  (mbind oper (applfunc exp))
;                  (mequal oper (:term =))
;                  (mbind lhs-exp (mfirst (applargs exp)))
;                  (mbind rhs-exp (msecond (applargs exp)))
;                  (mnot(mequal lhs-exp rhs-exp))
;                  
;                  ;if
;                  (mif (appl-p lhs-exp)
;                       ;then
;                       (mand (mbind termlist-lhs
;                                    (arguments2list (mlist lhs-exp op1)))
;                             (mbind lhs-normal-exp
;                                    (list2arguments
;                                     (mlist
;                                      (remove-id
;                                       (mlist
;                                        (inverse-pairwise
;                                         (mlist
;                                          (remove-invinv
;                                           (mlist termlist-lhs inv1))
;                                          inv1))
;                                         e1))
;                                       op1 e1)) ))
;                            ;else
;                       (mbind lhs-normal-exp lhs-exp))
;                  ;if
;                  (mif (appl-p rhs-exp)
;                       ;then
;                       (mand (mbind termlist-rhs
;                                    (arguments2list (mlist rhs-exp op1)))
;                             (mbind rhs-normal-exp
;                                    (list2arguments
;                                     (mlist
;                                      (remove-id
;                                       (mlist
;                                        (inverse-pairwise
;                                         (mlist
;                                          (remove-invinv
;                                           (mlist termlist-rhs inv1))
;                                          inv1))
;                                         e1))
;                                       op1 e1)) ))
;                       ;else
;                       (mbind rhs-normal-exp rhs-exp))
;                  (mbind normal-exp (appl-create
;                                     oper (mlist lhs-normal-exp rhs-normal-exp)))
;                  (mnot(mequal exp normal-exp))
;                  )
;                 )
;                
;                (outline-computations
;                 
;                 )
;                
;                (conclusions (- l100))
;                
;                (decl-content
;                 (l10  () normal-exp        ("OPEN" () ()))
;                 (l20  () (group05 g1 op1 e1 inv1))
;                 (l100 () exp               ("SOLVED" () (l10 l20) ))
;                 )
;                
;                (manual (author "Mateja Jamnik")
;                        (examples "Group theory theorems")
;                        (documentation "")
;                )
;                (remark ""
;                )
;              )
;
;
;;;******************************
;;;  Inverse Method
;;;******************************
;
;(infer~defmethod inverse-m
;                 (outline-mappings (((existent nonexistent existent) inverse-m-b)))
;                 (help "Applies ...."))
;
;(meth~defmethod inverse-m-b inverse-m
;                (in learn)
;                (rating 10)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (sorted-meta-variables
;                  (lhs i term)
;                  (rhs i term)
;                  (inv (i i) term)
;                  (op (i i i) term)
;                  (e i term)
;                  (g (o i) term)
;                  )
;                 )
;                
;                (premises (+ l10) l20)
;                
;                (application-condition
;
;                 )
;                
;                (outline-computations
;                 
;                 )
;                
;                (conclusions (- l100))
;                
;                (decl-content
;                 (l10  () (= (op (inv lhs) lhs) (op (inv lhs) rhs)) ("OPEN" () ()))
;                 (l20  () (group05 g op e inv))
;                 (l100 () (= lhs rhs)                      ("SOLVED" () (l10 l20) ))
;                 )
;                
;                (manual (author "Mateja Jamnik")
;                        (examples "Group theory theorems")
;                        (documentation "")
;                )
;                (remark ""
;                )
;              )
;
;;;******************************
;;;  Compose with Significant element Method
;;;******************************
;
;(infer~defmethod comp-significant-m
;                 (outline-mappings (((existent existent nonexistent existent) comp-significant-m-b)))
;                 (help "Applies ...."))
;
;(meth~defmethod comp-significant-m-b comp-significant-m
;                (in learn)
;                (rating 20)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (sorted-meta-variables
;                  (lhs i term)
;                  (rhs i term)
;                  (inv (i i) term)
;                  (op (i i i) term)
;                  (e i term)
;                  (x i term)
;                  (g (o i) term)
;                  )
;                 )
;                
;                (premises (+ l10) l20 l30)
;                
;                (application-condition
;
;                 )
;                
;                (outline-computations
;                 
;                 )
;                
;                (conclusions (- l100))
;                
;                (decl-content
;                 (l10  () (= (op lhs lhs) (op lhs rhs)) ("OPEN" () ()))
;                 (l20  () (group05 g op e inv))
;                 (l30  () (forall-sort (lam (x i) (= e (op x x))) g))
;                 (l100 () (= lhs rhs)                      ("SOLVED"
;                                                            () (l10
;                                                                l20 l30) ))
;                 )
;                
;                (manual (author "Mateja Jamnik")
;                        (examples "Group theory theorems")
;                        (documentation "")
;                )
;                (remark ""
;                )
;              )
;
;;;******************************
;;;  Knuth-Bendix Procedure Method
;;;******************************
;
;(infer~defmethod knuth-bendix-m
;                 (outline-mappings (((existent nonexistent existent) normalise-m-b)))
;                 (help "Applies ...."))
;
;(meth~defmethod knuth-bendix-m-b knuth-bendix-m
;                (in learn)
;                (rating 90)
;                (reasoning :planning :middle-out)
;                (declarations
;                 (sorted-meta-variables
;                  (exp o term)
;                  (exp o term)
;                  (lhs i term)
;                  (rhs i term)
;                  (test i list)
;                  (lhs-list i termlist)
;                  (rhs-list i termlist)
;                  (new-lhs i term)
;                  (new-rhs i term)
;                  (op1 (i i i) term)
;                  (inv1 (i i) term)
;                  (e1 i term)
;;                 (t i term)
;                  (g1 (o i) term)
;;                 (remove-id (i o) term)
;;                 (param o termlist)
;                  )
;                 )
;                ;xxxxxxxxxxxxxxxxxxx check types with
;                ;repeat-while-change
;                ;trace these (FIRST CONS APPLY-MANY REMOVE-ID-AUX LIST2ARGUMENTS-AUX APPLY-MANY-AUX
;;       SECOND REPEAT-WHILE-CHANGE-AUX)
;                (premises (+ l10) l20)
;                
;                (application-condition
;                 (mand
;;                 (mbind t (mlist (:term test)))
;                  (mnot(mequal lhs rhs))
;                  (mbind lhs-list (arguments2list (mlist lhs op1)))
;                  (mbind rhs-list (arguments2list (mlist rhs op1)))
;                  (mbind new-lhs
;                         (list2arguments
;                          (mlist
;                           (repeat-while-change
;                            (mlist lhs-list
;                             (mlist
;                              (mlist
;                               (:symbol remove-id) (mlist e1))
;                              (mlist
;                               (:symbol remove-invinv) (mlist inv1))
;                              (mlist
;                               (:symbol inverse-pairwise) (mlist inv1))
;                              (mlist
;                               (:symbol remove-invid) (mlist inv1 e1))
;                              (mlist
;                               (:symbol rewrite-invcomp) (mlist op1 inv1))
;                              )))
;                           op1 e1)))
;                   (mbind new-rhs
;                         (list2arguments
;                          (mlist
;                           (repeat-while-change
;                            (mlist rhs-list
;                             (mlist
;                              (mlist
;                               (:symbol remove-id) (mlist e1))
;                              (mlist
;                               (:symbol remove-invinv) (mlist inv1))
;                              (mlist
;                               (:symbol inverse-pairwise) (mlist inv1))
;                              (mlist
;                               (:symbol remove-invid) (mlist inv1 e1))
;                              (mlist
;                               (:symbol rewrite-invcomp) (mlist op1 inv1))
;                              )))
;                           op1 e1)))
;                   (mor (mnot(mequal new-lhs lhs))
;                        (mnot(mequal new-rhs rhs)))
;;                  rewrite-invcomp lhs)
;                  )
;                 )
;                
;                (outline-computations
;                 
;                 )
;                
;                (conclusions (- l100))
;                
;                (decl-content
;                 (l10  () (= new-lhs new-rhs)    ("OPEN" () ()))
;                 (l20  () (group05 g1 op1 e1 inv1))
;                 (l100 () (= lhs rhs)            ("SOLVED" () (l10 l20) ))
;                 )
;                
;                (manual (author "Mateja Jamnik")
;                        (examples "Group theory theorems")
;                        (documentation "")
;                )
;                (remark ""
;                )
;              )
;
;
