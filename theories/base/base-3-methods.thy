;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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

;;;FORALLI*-M
(infer~defmethod "ForallI*-m"
		 (outline-mappings (((existent nonexistent) foralli*-m)))
		 (help "The method for iterated FORALL introduction"))

;;; Remark: In order to bind a lambda-variable within a method term, i.e., the
;; variable Y in (exists (lam (y aa var) (and (N y) (= D (F y))))) below, during
;; the matching of a method, you have to represent it as a triple (name type sort),
;; where sort can be either VAR or VARLIST. You dont need to declare such variables
;; in SORTED-META-VARIABLES, they will be automatically defined in the method
;; environment while constructing the terms containing them. For instance, the
;; declaration of (x (bb aa) var) is not necessary needed, compare the variable
;; (y aa var) in the formula of L3. If you dont want to get a binding of a lambda-variable,
;; you have to represent it in a pair as known:
#|
(meth~defmethod foralli*-m ForallI*-m
		(in base)
		(rating 10)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) (phi1 o) (x aa var)
                  )
		 )
		(premises (+ l1))
		(outline-computations
		 (vars (mcons x (first-uvars phi)))
		 (cL (vars-newconsts vars))
		 (phi1 (subst-apply (subst-create vars cL) phi ))
		 )
		(conclusions (- l2))
		(decl-content 
		 (l1 () phi1                      ("Open" () ()))
		 (l2 () (forall (lam (x aa) phi)) ("ForallI*" (cL) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward application of the tactic ForallI*")
		)
|#

;;; The above FORALLI*-M is more efficient and less declarative than the one below: 
(meth~defmethod foralli*-m ForallI*-m
		(in base)
		(rating 10)
		(declarations
		 ;(type-variables aa)
		 (sorted-meta-variables
		  ;;; LC: The declaration of these variables can be avoided. It is
		  ;; clear from their occurrences in the declarative content that
		  ;; they are terms of type o:
		  (phi o) (phi1 o)
                  )
		 )
		(premises (+ l1))
		(application-condition
		 (mnot (mequal (applfunc phi) forall)))
		(outline-computations
		 (cL (vars-newconsts xL))
		 (phi1 (subst-apply (subst-create xL cL) phi ))
		 )
		(conclusions (- l2))
		(decl-content 
		 (l1 () phi1                      ("Open" () ()))
		 (l2 () (forall (lam (xL list varlist) phi)) ("ForallI*" (cL) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward application of the tactic ForallI*")
		)

;;;FORALLE*-M
(infer~defmethod "ForallE*-m"
		 (outline-mappings (((existent existent) forallE*-m-a)))
		 (help "The method for iterated FORALL elemination"))

(meth~defmethod forallE*-m-a
		ForallE*-m
		(in base)
		(rating 20)
		(declarations
		 ;;(type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (phi1 o) )
		 )
		(premises l1)
		(application-condition
		 (alpha-matcher phi phi1 sub))
		(expansion-computations
		 (cL (subst-apply sub xL)))
		(conclusions (- l2))
		(decl-content 
		 (l1 () (forall (lam (xL list varlist) phi)))         
		 (l2 () phi1                               ("ForallE*" (cL) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Application of the tactic ForallE*")
		)


;;;DefnI-m
(infer~defmethod "DefnI-m"
		 (outline-mappings (((existent nonexistent) DefnI-m)))
		 (help "Reduction of an atom by substituting the predicate symbol by its the theory definition."))

;;;was def-intro
(meth~defmethod DefnI-m
		DefnI-m
		(in base)
		(rating 1)
		(declarations
		 ;;(type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (tL list termlist) (thing (o list) const)
		  )
		 )
		(premises (+ l1))
		(application-condition
		 (th-definition thing thing-def))
		(outline-computations
		 (phi (beta-normalize (:term (thing-def tL))))
		 )
		(conclusions (- l2))
		(decl-content
		 (l1 () phi            ("Open" () ()))
		 (l2 () (thing tL)     ("DefnI" (thing thing-def) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward application of a theory definition.")
		)

;;;DefnE-m
(infer~defmethod "DefnE-m"
		 (outline-mappings (((nonexistent closed) DefnE-m)))
		 (help "Unfolding an atom by applying the theory definition of its predicate symbol."))

;;;was def-intro
(meth~defmethod DefnE-m
		DefnE-m
		(in base)
		(rating 0)
		(declarations
		 ;;(type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (tL list termlist) (thing (o list) const)
		  )
		 )
		(premises (- l1))
		(application-condition
		 (th-definition thing thing-def))
		(outline-computations
		 (phi (beta-normalize (:term (thing-def tL))))
		 )
		(conclusions (+ l2))
		(decl-content
		 (l1 () (thing tL)         )
		 (l2 () phi            ("DefnE" (thing thing-def) (l1)))
		 
		 )
		(proc-content schema-interpreter)
		(remark "Forward application of a theory definition.")
		)



;;;EQV-ASS2R
(infer~defmethod "EqvAss2R-m"
		 (outline-mappings (((existent existent nonexistent) eqv-ass2r-b)))
		 (help "A method for applying an equivalence assertion to its instantiated right side to conclude the instantiated left side"))

;;;was def-intro
(meth~defmethod eqv-ass2r-b
		EqvAss2R-m
		(in base)
		(rating 5)
		(declarations
		 ;;(type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (psi o) (psi1 o)
		  (tL list termlist) (p (o list) const) ;;muss nicht unbedingt eine Konstante sein
		  ;;(p (o aa) term) Doch
		  )
		 )
		(premises l1 (+ l4))
		(outline-computations
		 (psi1 (n-normalize (subst-apply (subst-create xL tL) psi)))
		 )
		(conclusions (- l5))
		(decl-content
		 (l1 () (forall (lam (xL list varlist) (equiv (p xL) psi))))
		 (l2 () (equiv (p tL) psi1)                        ("ForallE*" (tL) (l1)))
		 (l3 () (implies psi1 (p tL))                      ("EquivER" () (l2)))
		 (l4 () psi1                                       ("Open" () ()))
		 (l5 () (p tL)                                     ("ImpE" () (l4 l3)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward application of an equivalence assertion to its right side to prove the corresponding left side")
		)


;;;EQV-ASS2L
(infer~defmethod "EqvAss2L-m"
		 (outline-mappings (((nonexistent existent existent) eqv-ass2l-f)))
		 (help "A method for applying an equivalence assertion to its instantiated left side to conclude the instantiated right side"))

;;; def-elim with eqv
(meth~defmethod eqv-ass2l-f
		EqvAss2L-m
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
		(premises l1 (- l4))
		(outline-computations
		 (psi1 (n-normalize (subst-apply (subst-create xL tL) psi)))
		 )
		(conclusions (+ l5))
		(decl-content
		 (l1 () (forall (lam (xL list varlist) (equiv (p xL) psi))))
		 (l2 () (equiv (p tL) psi1)                        ("ForallE*" (tL) (l1)))
		 (l3 () (implies (p tL) psi1)                      ("EquivEL" () (l2)))
		 (l4 () (p tL))
		 (l5 () psi1                                       ("ImpE" () (l4 l3)))
		 )
		(proc-content schema-interpreter)
		(remark "Forward application of an equivalence assertion to its left side to conclude the corresponding right side")
		)


;;;IMP-ASS2L
(infer~defmethod "ImpAss2L-m"
		 (outline-mappings (((nonexistent existent existent) imp-ass2l-f)))
		 (help "A method for applying an implication assertion to its instantiated left side to conclude the instantiated right side"))

(meth~defmethod imp-ass2l-f
		ImpAss2L-m
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
		(premises l1 (- l3))
		(outline-computations
		 (psi1 (n-normalize (subst-apply (subst-create xL tL) psi)))
		 )
		(conclusions (+ l4))
		(decl-content
		 (l1 () (forall (lam (xL list varlist) (implies (p xL) psi))))
		 (l2 () (implies (p tL) psi1)                        ("ForallE*" (tL) (l1)))
		 (l3 () (p tL))
		 (l4 () psi1                                         ("ImpE" () (l3 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Forward application of an equivalence assertion to its left side to conclude the corresponding right side")
		)


;;; ANDI-M
(infer~defmethod "AndI-m"
		 (outline-mappings (((existent nonexistent nonexistent) andi-m-b)
				    ((existent existent existent) andi-m-a)
				    ((nonexistent closed closed) andi-m-f)))
		 (help "The method for AND introduction"))

;;;was and-intro
(meth~defmethod ;;method name inference name
                andi-m-b AndI-m
		(in base)
		(rating 10)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		(decl-content
		 (l1 () phi ("Open" () ()))
		 (l2 () psi ("Open" () ()))
		 (l3 () (and phi psi) ("AndI" () (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward applying the AND introduction")
		)

(meth~defmethod ;;method name inference name
                andi-m-a AndI-m
		(in base)
		(rating 2)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o))
		 )
		(premises l1 l2)
                (conclusions (- l3))
		(decl-content
		 (l1 () phi)
		 (l2 () psi)
		 (l3 () (and phi psi) ("AndI" () (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Applying the AND introduction")
		)


(meth~defmethod andi-m-f AndI-m
		(in base)
		(rating 0.3)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o))
		 )
		(premises (- l1) (- l2))
		(conclusions (+ l3))
		(decl-content
		 (l1 () phi)
		 (l2 () psi)
		 (l3 () (and phi psi) ("AndI" () (l1 l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Forward applying the AND introduction")
		)


;;;ANDE-M:
(infer~defmethod "AndE-m"
		 (outline-mappings (((existent existent existent) ande-m-a)
				    ((nonexistent nonexistent existent) ande-m-f)))
		 (help "The method for AND elimination"))

;;; ande-m-a 
(meth~defmethod ande-m-a AndE-m
		(in base)
		(rating 2)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o))
		 )
		(premises l1)
		(conclusions (- l2) (- l3))
		(decl-content
		 (l1 () (and phi psi))
		 (l2 () phi ("AndEL" () (l1)))
		 (l3 () psi ("AndER" () (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Applying AND elimination")
		)

(meth~defmethod ande-m-f AndE-m
		(in base)
		(rating 0.4)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (- l1))
		(conclusions (+ l2) (+ l3))
		(decl-content
		 (l1 () (and phi psi))
		 (l2 () phi ("AndEL" () (l1))) 
		 (l3 () psi ("AndER" () (l1))) 
		 )
		(proc-content schema-interpreter)
		(remark "Forward application of AND elimination")
		)

(infer~defmethod "NotExistsI-m"
		 (outline-mappings (((existent nonexistent) NotExistsI-m)))
		 (help "The method for NOT-EXISTS introduction"))

(meth~defmethod NotExistsI-m NotExistsI-m
		(in base)
		(rating 10)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o) (phi1 o) (witness aa const)
		  )
		 )
		(premises (+ l3))
		(conclusions (- th))
		(outline-computations
		 (witness (var-newconst x))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist witness))
				    phi))
		 )
		(decl-content 
		 (l1 () (exists (lam (x aa var) phi))        ("Hyp" () ()))
		 (l2 () phi1                                 ("Hyp" () ()))
		 (l3 (l2) false                              ("Open" () ()))
		 (l4 (l1) false                              ("ExistsE" (witness) (l1 l3))) 
		 (th () (not (exists (lam (x aa var) phi)))  ("NotI" () (l4)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward applying of NOT-EXISTS introduction")
		)


(infer~defmethod "ImpI-m"
		 (outline-mappings (((existent nonexistent) ImpI-m-b)))
		 (help "The method for IMPLIES introduction"))

(meth~defmethod ImpI-m-b ImpI-m
		(in base)
		(rating 10)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o) (psi o)
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))
		(decl-content 
		 (l1 () phi ("Hyp" () ()))
		 (l2 (l1) psi ("Open" () ()))
		 (l3 () (implies phi psi) ("ImpI" () (l2)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward applying IMPLIES introduction")
		)


(infer~defmethod "EquivI-m"
		 (outline-mappings (((existent nonexistent nonexistent) EquivI-m-b)))
		 (help "The method for EQUIV introduction"))

(meth~defmethod EquivI-m-b EquivI-m
		(in base)
		(rating 10) 
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o) (psi o))
		 )
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		(decl-content 
		 (l1 () (implies phi psi)       ("Open" () ()))
		 (l2 () (implies psi phi)       ("Open" () ()))
		 (l4 () (and (implies phi psi)
			    (implies psi phi)) ("AndI" () (l1 l2)))
		 (l3 () (equiv phi psi)         ("EquivI" () (l4)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward applying EQUIV introduction")
		)


(infer~defmethod "ImpE-m"
		 (outline-mappings (((nonexistent closed nonexistent) ImpE-m-rd)
				    ((existent nonexistent existent) ImpE-m-b)))
		 (help "The method for IMPLIES elimination"))

(meth~defmethod ImpE-m-rd ImpE-m
		(in base) 
		(rating 1)
		(declarations
		 (sorted-meta-variables
		             ;;(meta-var type sort)
		  (F1 o) (F2 o))
		 )
		(premises (- l1) (+ l2)) 
		(conclusions (+ l3))
		(decl-content
		 (l1 () (implies F1 F2))
		 (l2 ()  F1            ("Open" () ()))
		 (l3 ()  F2            ("ImpE" () (l2 l1)))
		 )
		(proc-content schema-interpreter)		
		(remark "ImpE-m-rd is an essentially forward backchain method, i.e., works on assumptions F1 -> F2 and  produces a new subgoal F1 in order to provide a new assumption F2. Moreover, it deletes the assumption F1 -> F2 from the planning state.")
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

(infer~defmethod "Weaken-m"
		 (outline-mappings (((existent existent) Weaken-m-a)))
		 (help "The method for WEAKEN"))

(meth~defmethod Weaken-m-a Weaken-m 
                (in base)
		(rating 20)
		(declarations
		 (sorted-meta-variables
		  (F o))
		 )
		(premises l1)
		(conclusions (- l2))
		(decl-content
		 (l1 () F)
		 (l2 () F  ("Weaken" () (l1)))
		 )
;		(outline-actions
;                 ((sponsored l2) (unsponsor l2) (support l1))
;                 ;;; Selection {Action}
;                 ;; The lines sponsored by l2: delete l2 from the sponsors of
;                 ;; this lines and add l1 as a support of them.
;                 ;;; This actions are needed in Weaken to restrict the supports
;		  ;; of some nodes. It can happen that l1 is in supports(pds) and
;		  ;; consequently in the supports of each open node and if an open
;		  ;; node is sponsored by l2 too, then it will be sponsored by two
;		  ;; similar nodes l1 and l2.
;                 )
		(proc-content schema-interpreter )		
		(remark "This method closes a goal with one of its supports. The hypothesis list of this support node must be a subset of the goal hypotheses.")
		)






