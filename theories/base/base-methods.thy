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

;;****************************************************************************
;;                                                                           *
;;  The basic methods and supermethods for proof planning          .         *
;;___________________________________________________________________________*
;;                                                                           *
;; The names of low level methods start with a capital letter as             *
;; opposed to tactics. The names if supermethods (subplans) are completely   *
;; written in capital letters.                                               *
;;                                                                           *
;; The manual entry of the methods is extracted and presented in HTML        *
;; at http://www.ags.uni-sb.de/~omega/omegaindex/methods/methods-index.html  *
;;                                                                           *
;; Please send questions and comments to omega@ags.uni-sb.de                 *
;;****************************************************************************


;;*******************************************************
;;*    Methods for quanitifier elimination              *
;;*******************************************************
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

;;; The above FORALLI*-M is more efficient and less declarative than the one below: 
(meth~defmethod foralli*-m ForallI*-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
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
		(manual (documentation "This method has the same effect as the backward application of the tactic ForallI*"))
		)


;;;FORALLE*-M
(infer~defmethod "ForallE*-m"
		 (outline-mappings (((existent existent) Foralle*-m-a)))
		 (help "The method for iterated FORALL elemination"))

(meth~defmethod Foralle*-m-a
		ForallE*-m
		(in base)
		(rating 20)
		(reasoning :restricting  :middle-out)
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


;;*******************************************************
;;*    Methods for definition expansion                 *
;;*******************************************************
;;;DefnI-m
(infer~defmethod "DefnI-m"
                 (outline-mappings (((existent nonexistent) DefnI-m)))
                 (help "Reduction of an atom by substituting the predicate symbol by its the theory definition."))

;;;was def-intro
(meth~defmethod DefnI-m
                DefnI-m
                (in base)
                (rating 1)
		(reasoning :planning  :middle-out)
                (declarations
                 ;;(type-variables aa)
                 (sorted-meta-variables
                  ;;(meta-var typ sort)
                  (phi o) (tL list termlist) (thing (o list) const)
                  )
                 )
                (premises (+ l1))
                (application-condition
		 (th-restricted-definition (:symbol :base) thing thing-def))
;;;                 (th-definition thing thing-def))  ;;; too general especially for definitions in base-theory
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
;
;;;;;DefnE-m
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
		 (th-restricted-definition (:symbol :base) thing thing-def))
;;;                 (th-definition thing thing-def))  ;;; to general especially for definitions in base-theory
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

;;*******************************************************
;;*    Methods for the expansion of definitions         *
;;*******************************************************

(infer~defmethod "DefnExp-m"
                 (outline-mappings (((nonexistent closed) DefnExp-m-f)
				    ((existent nonexistent) DefnExp-m-b)))
		 (parameter-types term)
                 (help "Applying the theory definition of a predicate or a function symbol."))

(meth~defmethod DefnExp-m-b DefnExp-m
                (in base)
                (rating 0)
                (declarations
                ;;(type-variables aa)
                 (sorted-meta-variables
                  ;;(meta-var typ sort)
                  (phi o) (psi o) (zeta o)
		  (symbol o)
		  (symbol-term o term)
		  (thing-def o term)
                  )
                 )
		
                (premises (+ L1))
		(parameters symbol)
                (application-condition
		 (mand (bound-p symbol)
		       ;;(GetSymbolTerm symbol symbol-term)
		       (termoccs symbol phi occs)
		       (Th-Definition symbol thing-def)
		       ))
		
		(outline-computations
		 (zeta (beta-normalize (TermRplAtPos phi (mfirst occs) thing-def)))
		 )
                (conclusions (- L2))
                (decl-content
                 (l1 () zeta           ("OPEN" () ()))
                 (l2 () phi            ("DefnE" (symbol thing-def) (l1)))
                 
                 )
                (proc-content schema-interpreter)
		(remark ;local short, long
		 ("<U>DefnExp:</U><BR>
<TERM>symbol</TERM> is replaced by its definition."
		 "<U>DefnExp (detailed):</U><BR>
For <TERM>symbol</TERM> its definition<BR>
<TAB><TERM>thing-def</TERM><BR>
is inserted in<BR>
<TAB><TERM>phi</TERM><BR>
resulting in the new goal<BR>
<TAB><TERM>zeta</TERM>.")
	      ;global text, constr
		 ("Since<BR>
<TAB><TERM>zeta</TERM>,<BR>
it follows<BR>
<TAB><TERM>phi</TERM><BR>
by the definition of <TERM>symbol</TERM>.<BR>
<LISP>(verbalize-text-next l1)</LISP>"
		 "Since<BR>
<TAB><TERM>zeta</TERM>,<BR>
it follows<BR>
<TAB><TERM>phi</TERM><BR>
by the definition of <TERM>symbol</TERM>.<BR>
<LISP>(verbalize-cons-next l1)</LISP>")))



(meth~defmethod DefnExp-m-f DefnExp-m
                (in base)
                (rating 0)
                (declarations
                 ;;(type-variables aa)
                 (sorted-meta-variables
                  ;;(meta-var typ sort)
                  (phi o) (psi o) (zeta o)
		  (symbol o)
		  (symbol-term o term)
		  (thing-def o term)
		  (occs o sublist)
                  )
                 )
		
                (premises (- L1))
		(parameters symbol)
                (application-condition
		 (mand (bound-p symbol)
		       ;;(GetSymbolTerm symbol symbol-term)
		       (termoccs symbol phi occs)
		       (Th-Definition symbol thing-def)
		       ))
		(outline-computations
		 (zeta (beta-normalize (TermRplAtPos phi (mfirst occs) thing-def)))
		 )
		
		
                (conclusions (+ L2))
                (decl-content
                 (l1 () phi)
                 (l2 () zeta           ("DefnI" (symbol-term thing-def) (l1)))
                 
                 )
                (proc-content schema-interpreter)
                (remark "Forward application of a theory definition.")
                )


;;*******************************************************
;;*    Methods for (beta-) normalization                *
;;*******************************************************
(infer~defmethod "BetaNormalize-m"
		 (outline-mappings (((nonexistent existent) BetaNormalize-m-f)
				    ((existent nonexistent) BetaNormalize-m-b))))

(meth~defmethod BetaNormalize-m-f
		BetaNormalize-m
		(in base)
		(declarations
		 (sorted-meta-variables
		  (phi o) (psi o)))
		
		(premises (- l1))
		
		(application-condition
		 (mand (beta-redex-in phi)
		       (mbind psi (beta-normalize phi))
		       ))
		(conclusions (+ l2))
		(decl-content 
		 (l1 () phi)
		 (l2 () psi               ("beta-normalize-f" () (l1))))
		)

(meth~defmethod BetaNormalize-m-b
		BetaNormalize-m
		(in base)
		(declarations
		 (sorted-meta-variables
		  (phi o term) (psi o term)))
		
		(premises (+ l1))
		
		(application-condition
		 (mand (beta-redex-in phi)
		       (mbind psi (beta-normalize phi))
		       ))
		(conclusions (- l2))
		(decl-content
		 (l1 () psi                ("OPEN" () ()))
		 (l2 () phi                ("beta-normalize-b" () (l1))))
		)

;;*                   The NORMALIZE supermethod
(infer~defsupermethod NORMAL-S
		      (supermethod NORMAL-S-B)
		      (help "The NORMAL inference."))

(meth~defsupermethod NORMAL-S-B
		     NORMAL-S
		     (in base)
		     (rating 0)
		     (reasoning :planning :middle-out)
		     (declarations
		      (sorted-meta-variables
		       (Delta o prlnlist)
		       (psi o term)
		       (result o planlist)
		       (prems o prlnlist)
		       (concls o prlnlist)
		       (hyps o prlnlist)
		       )
		 
		      )
		     (premises (+ prems) )
		     (application-condition
		      (mand (meval result)
			    )
		      )
		     
		     (outline-computations
		      (hyps (mthird result))
		      (prems (mfourth result))
		      (concls (mfifth result))
		      )
		     
		     
		     (conclusions (- l1) (+ hyps) (+ concls))
		     (expansion-function (plan~expand-supermethod l1 result))
		     (decl-content
		      (l1 () psi)
		      )
		     (proc-content
		      (plan~iterate-planner l1
					    '()
					    '(ImpI-m-b
					      EquivI-m-b
					      AndI-m-b
					;BetaNormalize-m-b
					      PullNeg-m-b
					      )
					    '())
		      )
		     (manual (author "Juergen Zimmer")
			     (examples "Limit theorems")
			     (documentation "The supermethod NORMALIZE-S-B plans only with the methods
<A HREF=\"./impi-m-b.lml\">ImpI-m-b</A>, <A HREF=\"./equivi-m-b.lml\">EquivI-m-b</A>, and
<A HREF=\"./andi-m-b.lml\">AndI-m-b</A> to normalize the goal <FONT COLOR=\"#0000ff\">l1</FONT>
until none of these methods is applicable. <BR>
This supermethod uses no special control knowledge. It is applicable, if at least one of the given
methods is applicable. <BR>
The method is expanded by a special expansion-function.
"))
		     
		     (remark "psi is normalized.")
		     )



(infer~defmethod "Weaken-m"
		 (outline-mappings (((existent existent) Weaken-m-a)))
		 (help "The method for WEAKEN"))

(meth~defmethod Weaken-m-a Weaken-m 
                (in base)
		(rating 20)
		(reasoning :restricting)
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



(infer~defmethod "Weakenalpha-m"
		 (outline-mappings (((existent existent) Weakenalpha-m-a)))
		 (help "The method for WEAKEN with alpha equality"))

(meth~defmethod Weakenalpha-m-a Weakenalpha-m 
                (in base)
		(rating 20)
		(reasoning :restricting)
		(declarations
		 (sorted-meta-variables
		  (F o)
		  (Falpha o))
		 )
		(application-condition
		 (MTERMSALPHAEQUAL Falpha F))
		
		(premises l1)
		(conclusions (- l2))
		(decl-content
		 (l1 () Falpha     )
		 (l2 () F          ("Weaken" () (l1)))
		 )
		(proc-content schema-interpreter )		
		(remark "This method closes a goal with one of its supports. The hypothesis list of this support node must be a subset of the goal hypotheses.")
		)






;; new version: Assertion application with theorems given by control rule
(infer~defmethod "Assertion-m"
		 (outline-mappings (((existent) Assertion-m-b)))
		 (parameter-types problem))

(meth~defmethod Assertion-m-b Assertion-m
		(in base)
		(rating 3)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
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
                  (mnot (mequal phi (mnil)))
		  (myassert (theorem-formula phi) psi pformulas)))
		(outline-computations
		 (prems (if (mequal pformulas (mnil))
			    (mnil)
			  (premises-for l2 pformulas))))
		
		(decl-content
		 (l2 () psi ("Assertion" (phi) (prems))))
		(proc-content schema-interpreter ))


;;*******************************************************************
;;*  Some basic methods encapsulating ND-rules or simple tactics    *
;;*                                                                 *
;;*******************************************************************
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
		(reasoning :planning  :middle-out)
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
		(remark ("Backward applying EQUIV introduction")
			("In order to prove the equivalence, we first prove one direction:<BR>
<BLOCKQUOTE><LISP>(increase-depth)</LISP>
<LISP>(item)</LISP><TERM>(implies psi phi)</TERM>:
<BLOCKQUOTE>
<LISP>(increase-depth)</LISP><LISP>(item)</LISP>
<LISP>(verbalize-text-subproof l2)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP></BLOCKQUOTE>
Now we prove the second direction:<BR>
<BLOCKQUOTE>
<LISP>(item)</LISP><TERM>(implies phi psi)</TERM>:
<BLOCKQUOTE>
<LISP>(increase-depth)</LISP><LISP>(item)</LISP>
<LISP>(verbalize-text-subproof l1)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP></BLOCKQUOTE>"

			 "In order to prove the equivalence, we first prove one direction:<BR>
<BLOCKQUOTE><LISP>(increase-depth)</LISP>
<LISP>(item)</LISP><TERM>(implies psi phi)</TERM>
<BLOCKQUOTE>
<LISP>(increase-depth)</LISP><LISP>(item)</LISP>
<LISP>(verbalize-cons-subproof l2)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP></BLOCKQUOTE>
Now we prove the second direction:<BR>
<BLOCKQUOTE>
<LISP>(item)</LISP><TERM>(implies phi psi)</TERM>:<BR>
<BLOCKQUOTE>
<LISP>(increase-depth)</LISP><LISP>(item)</LISP>
<LISP>(verbalize-cons-subproof l1)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP></BLOCKQUOTE>"

			 )))

;;;EQV-ASS2R
(infer~defmethod "EqvAss2R-m"
		 (outline-mappings (((existent existent nonexistent) eqv-ass2r-b)))
		 (help "A method for applying an equivalence assertion to its instantiated right side to conclude the instantiated left side"))

;;;was def-intro
(meth~defmethod eqv-ass2r-b
		EqvAss2R-m
		(in base)
		(rating 5)
		(reasoning :planning  :middle-out) 
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

;;; ANDI*-M - makes nicer proof trees :)
(infer~defmethod "andi*-m"
		 (outline-mappings (((existent nonexistent) andi*-m-b)
				    ((existent list) andi*-m-b)))
		 (help "The method for multiple AND introduction"))

(meth~defmethod andi*-m-b andi*-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (phi o) (psi o)
		  (prems o prlnlist)
		  ))

		(premises prems)
		(conclusions (- l3))

		(application-condition
		 (test-tactic (:symbol :andi*) (mlist l3 (mnil)) (mnil)))

		(outline-computations
		 (prems (msecond (apply-tactic (:symbol :andi*) (mlist l3 (mnil)) (mnil)))))
		
		(decl-content
		 (l3 () (and phi psi) ("AndI*" () prems)))
		
		(proc-content schema-interpreter)
		(remark "The method for multiple AND introduction")
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
		(reasoning :planning :middle-out)
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



;;;ANDE-M:
(infer~defmethod "AndE-m"
		 (outline-mappings (((existent existent existent) ande-m-a)
				    ((nonexistent nonexistent existent) ande-m-f)
				    ((nonexistent nonexistent closed) ande-m-f)))
		 (help "The method for AND elimination"))


(meth~defmethod ande-m-f AndE-m
		(in base)
		(rating 0.4)
		(reasoning :normalizing)
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

(infer~defmethod TrueI-m
		 (outline-mappings (((existent) TrueI-m-b)))
		 (help "Deletes TRUE goal."))


(meth~defmethod TrueI-m-b TrueI-m
		(in base)
		(rating 45)
		(reasoning :planning)
		
		(premises )
		(conclusions (- L1))
		
		(decl-content
		 (l1 () true       ("TrueI" ()()))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method TrueI-m-b is a partial specification of the
<A HREF=\"../commands/truei.lml\">trueI</A> tactic."))
		
		(remark "The introduction of TRUE.")
		)

(infer~defmethod OrIL-m
		 (outline-mappings (((existent nonexistent) OrIL-m-b)))
		 (help "Tactic OrIL."))


(meth~defmethod OrIL-m-b OrIL-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
 		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  )
		 )
		
		(premises (+ L0))
		(conclusions (- L1))
		
		(decl-content
		 (L0 () phi               ("OPEN" () ()))
		 (l1 () (or phi psi)      ("oril" ()(L0)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method OrIL-m-b is a partial specification of the
<A HREF=\"../commands/oril.lml\">oril</A> tactic."))
		
		(remark "The introduction of TRUE.")
		)

(infer~defmethod OrIR-m
		 (outline-mappings (((existent nonexistent) OrIR-m-b)))
		 (help "Tactic orir."))


(meth~defmethod OrIR-m-b OrIR-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  )
		 )
			
		(premises (+ L0))
		(conclusions (- L1))
		
		(decl-content
		 (L0 () psi               ("OPEN" () ()))
		 (l1 () (or phi psi)      ("orir" ()(L0)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method OrIR-m-b is a partial specification of the
<A HREF=\"../commands/orir.lml\">orir</A> tactic."))
		
		(remark "The introduction of TRUE.")
		)

(infer~defmethod OrILHYP-m
		 (outline-mappings (((existent nonexistent) OrILHYP-m-b)))
		 (help "Tactic Or2imp combined with IMPI."))


(meth~defmethod OrILHYP-m-b OrILHYP-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
 		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  )
		 )
		
		(premises (+ L3))
		(conclusions (- L5))
		
		(decl-content
		 (L2 ()   (not psi)                ("Hyp" () ()))
		 (l3 (l2) phi                      ("OPEN" () ()))
		 (l4 ()   (implies (not psi) phi)  ("ImpI" () (l3))) 
		 (l5 ()   (or phi psi)             ("or2imp-b" () (L4)))
		 )
		(remark "")	
		)


(infer~defmethod OrIRHYP-m
		 (outline-mappings (((existent nonexistent) OrIRHYP-m-b)))
		 (help "Tactic Or2imp combined with IMPI."))


(meth~defmethod OrIRHYP-m-b OrIRHYP-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
 		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  )
		 )
		
		(premises (+ L3))
		(conclusions (- L5))
		
		(decl-content
		 (L2 ()   (not phi)                ("Hyp" () ()))
		 (l3 (l2) psi                      ("OPEN" () ()))
		 (l4 ()   (implies (not phi) psi)  ("ImpI" () (l3))) 
		 (l5 ()   (or phi psi)             ("or2imp-b" () (L4)))
		 )
		(remark "")	
		)



(infer~defmethod PushNeg-m
		 (outline-mappings (((nonexistent existent) PushNeg-m-f)))
		 (help "The pushneg tactic."))

(meth~defmethod PushNeg-m-f    ;; NOTE: THIS NAME IS CORRECT! (JZ)
		PushNeg-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (F o term)
		  (G o term)
		  )
		 )
		(premises (- L1))
		(application-condition
		 (mand (mbind G (pushneg* (currprlnformula L1)))
		       (mnot (mequal G (currprlnformula L1)))))
		(conclusions (+ L2))
		
		(decl-content
		 (L1 () (not F))
		 (L2 () G              ("pushneg" () (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method PushNeg-m-f is a partial specification of the
<A HREF=\"../commands/pushneg.lml\">pushneg</A> tactic."))
		(remark "")
		)

(infer~defmethod PullNeg-m
		 (outline-mappings (((existent nonexistent) PullNeg-m-b)))
		 (help "The pullneg tactic."))

(meth~defmethod PullNeg-m-b
		PullNeg-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (F o term)
		  (G o term)
		  )
		 )
		(premises (+ L1))
		(application-condition
		 (mand (mbind F (pushneg* (currprlnformula L2)))
		       (mnot (mequal F (currprlnformula l2)))))
		(conclusions (- L2))
		
		(decl-content
		 (L1 () F                    ("OPEN" () ()))
		 (L2 () (not G)              ("pullneg" () (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method PullNeg-m-b is a partial specification of the
<A HREF=\"../commands/pullneg.lml\">pushneg</A> tactic."))
		
		(remark "")
		)


(infer~defmethod "ImpE-Open-m"
		 (outline-mappings (((existent nonexistent closed nonexistent) ImpE-open-m-a)
				    ))
		 (help "The method for IMPLIES ELIMINATION ImpE modulo an open node."))

(meth~defmethod ImpE-open-m-a ImpE-Open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (thm o)
		  )
		 )
		(application-condition
		 )
		(outline-actions
		 (l4 (sponsor l3))
		 )
		(premises (+ l1) (- l2) (+ l4))
		(conclusions (- l5))		 
		(decl-content
		 (l1 () phi ("Open" () ()))
		 (l2 () (implies phi psi))
		 (l3 () psi ("ImpE" () (l1 l2)))
		 (l4 () thm ("Open" () ()))
		 (l5 () thm ("Weaken" () (l4))))
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose an implication assumption modulo an open line. As result the antecedent of the implication is an open line with all hps of the open line, and the succedent of the implication is available as assumption in the supports of the the open line."))		
		(proc-content schema-interpreter)
		(remark "Application of ImpE modulo an open line.")
		)


(infer~defmethod "OrEL-Open-m"
		 (outline-mappings (((existent nonexistent closed nonexistent) OrEL-open-m-a)
				    ))
		 (help "The method for IMPLIES ELIMINATION ImpE modulo an open node."))

(meth~defmethod OrEL-open-m-a OrEL-open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (thm o)
		  )
		 )
		(application-condition
		 )
		(outline-actions
		 (l4 (sponsor l3))
		 )
		(premises (+ l1) (- l0) (+ l4))
		(conclusions (- l5))		 
		(decl-content
		 (l0 () (or phi psi)  )
		 (l1 () (not phi)                ("Open" () ()))
		 (l2 () (implies (not phi) psi)  ("Imp2Or" () (l0)))
		 (l3 () psi ("ImpE" () (l1 l2)))
		 (l4 () thm ("Open" () ()))
		 (l5 () thm ("Weaken" () (l4))))
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose an implication assumption modulo an open line. As result the antecedent of the implication is an open line with all hps of the open line, and the succedent of the implication is available as assumption in the supports of the the open line."))		
		(proc-content schema-interpreter)
		(remark "Application of ImpE modulo an open line.")
		)

(infer~defmethod "OrER-Open-m"
		 (outline-mappings (((existent nonexistent closed nonexistent) OrER-open-m-a)
				    ))
		 (help "The method for IMPLIES ELIMINATION ImpE modulo an open node."))

(meth~defmethod OrER-open-m-a OrER-open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (thm o)
		  )
		 )
		(application-condition
		 )
		(outline-actions
		 (l4 (sponsor l3))
		 )
		(premises (+ l1) (- l0) (+ l4))
		(conclusions (- l5))		 
		(decl-content
		 (l0 () (or phi psi)  )
		 (l1 () (not psi)                ("Open" () ()))
		 (l2 () (implies (not psi) phi)  ("Imp2Or" () (l0)))
		 (l3 () phi                      ("ImpE" () (l1 l2)))
		 (l4 () thm ("Open" () ()))
		 (l5 () thm ("Weaken" () (l4))))
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose an implication assumption modulo an open line. As result the antecedent of the implication is an open line with all hps of the open line, and the succedent of the implication is available as assumption in the supports of the the open line."))		
		(proc-content schema-interpreter)
		(remark "Application of ImpE modulo an open line.")
		)


#| NEW VERSION:

;; also a little bit more elegant than the versions above, it turned out, that these versions of
;; ImpE-open-m-a, OrEL-open-m-a, and OrER-open-m-a caused problems when backtracking.
;; The problem was that also a step is perfromed with respect to a certain task (thm) this task was
;; not directly affected. So when this step was backtracked this was not noted at the task.
;; This caused endless circles.

(infer~defmethod "ImpE-Open-m"
		 (outline-mappings (((nonexistent nonexistent existent) ImpE-open-m-a)
				    ))
		 (parameter-types ndline)
		 (help "The method for IMPLIES ELIMINATION ImpE modulo an open node."))

(meth~defmethod ImpE-open-m-a ImpE-Open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (thm o prln)
		  (hyps o prlnlist)
		  ))
		(parameters thm)
		(application-condition
		 (bound-p thm))
		(outline-computations
		 (hyps (prlnhyps thm)))
		(outline-actions
		 (thm (sponsor l3))
		 (l1 (increasehyp hyps))
		 (l3 (increasehyp hyps)))
		(premises (+ l1) (- l2))
		(conclusions (+ l3))		 
		(decl-content
		 (l1 () phi ("Open" () ()))
		 (l2 () (implies phi psi))
		 (l3 () psi ("ImpE" () (l1 l2)))
		 )
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose an implication assumption modulo an open line. As result the antecedent of the implication is an open line with all hps of the open line, and the succedent of the implication is available as assumption in the supports of the the open line."))		
		(proc-content schema-interpreter)
		(remark "Application of ImpE modulo an open line.")
		)

(infer~defmethod "OrEL-Open-m"
		 (outline-mappings (((nonexistent nonexistent existent) OrEL-open-m-a)
				    ))
		 (parameter-types ndline)
		 (help "The method for implies a combination of imp2or and ImpE modulo an open node."))

(meth~defmethod OrEL-open-m-a OrEL-open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (hyps o prlnlist)
		  (thm o prln)))
		(parameters thm)
		(application-condition
		 (bound-p thm))
		(outline-computations
		 (hyps (prlnhyps thm)))
		(outline-actions
		 (thm (sponsor l3))
		 (l1 (increasehyp hyps))
		 (l3 (increasehyp hyps)))
		(premises (+ l1) (- l0))
		(conclusions (+ l3))		 
		(decl-content
		 (l0 () (or phi psi)  )
		 (l1 () (not phi)                ("Open" () ()))
		 (l2 () (implies (not phi) psi)  ("Imp2Or" () (l0)))
		 (l3 () psi                      ("ImpE" () (l1 l2)))
		 )
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose a disjunction assumption modulo an open line."))
		(proc-content schema-interpreter)
		(remark "Application of Imp2or and ImpE modulo an open line.")
		)


(infer~defmethod "OrER-Open-m"
		 (outline-mappings (((nonexistent nonexistent existent) OrER-open-m-a)
				    ))
		 (parameter-types ndline)
		 (help "The method for implies a combination of imp2or and ImpE modulo an open node."))

(meth~defmethod OrER-open-m-a OrER-open-m
		(in base)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o)
		  (psi o)
		  (hyps o prlnlist)
		  (thm o prln)))
		(parameters thm)
		(application-condition
		 (bound-p thm))
		(outline-computations
		 (hyps (prlnhyps thm)))
		(outline-actions
		 (thm (sponsor l3))
		 (l1 (increasehyp hyps))
		 (l3 (increasehyp hyps)))
		(premises (+ l1) (- l0))
		(conclusions (+ l3))		 
		(decl-content
		 (l0 () (or phi psi)  )
		 (l1 () (not psi)                ("Open" () ()))
		 (l2 () (implies (not psi) phi)  ("Imp2Or" () (l0)))
		 (l3 () phi                      ("ImpE" () (l1 l2)))
		 )
		(manual (author "Andreas Meier")
			(examples "all kind of problems")
			(documentation "Applicable to decompose a disjunction assumption modulo an open line."))
		(proc-content schema-interpreter)
		(remark "Application of Imp2or and ImpE modulo an open line.")
		)


|#

;;*******************************************************
;;*               The Same-methods                      *
;;*******************************************************
(infer~defmethod Same-m
		 (outline-mappings (((existent existent) Same-m-b))))
				    
(meth~defmethod Same-m-b Same-m 
                (in base)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (F o term)
		  )
		 )
		(premises L1)
		(conclusions (- L2))
		
		(decl-content
		 (L1 () F)
		 (L2 () F  ("weaken" () (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method Same-m-b is a partial specification of the
<A HREF=\"../commands/weaken.lml\">WEAKEN</A> ND-rule.
"))
		
		(remark "The two nodes contain exactly the same formula.")
		)

(infer~defmethod AlphaUnify-m
		 (outline-mappings (((existent closed closed) AlphaUnify-m-a)
				    ((existent existent nonexistent) AlphaUnify-m-b)))
		 )

(meth~defmethod AlphaUnify-m-a AlphaUnify-m 
                (in base)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (F o term)
		  (F1 o term)
		  (x o term)
		  (y o term)
		  (sigma o subst)
		  )
		 )
		(premises L1 L2)
		(application-condition
		 (mand (mor (mequal F1 (termrploccs y F x))
			    (mequal F1 (termrploccs x F y)))
		       )
		 )
		(conclusions (- L3))
		(decl-content
		 (L1 () F)
		 (L2 () (= x y))
		 (L3 () F1            ("subst-apply" () (L1 L2)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "This method closes a subgoal <FONT COLOR=\"#0000ff\">l3</FONT>,
if it is equal to the precondition <FONT COLOR=\"#0000ff\">l1</FONT> modulo the application of the
substitution given by the equation in the precondition <FONT COLOR=\"#0000ff\">l2</FONT>. <BR>
"))
		
		(remark "By applying the equation (= x y) to F, we obtain F1.")
		)

(meth~defmethod AlphaUnify-m-b AlphaUnify-m 
                (in base)
		(rating 60)
		(reasoning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o term)
		  (psi o term)
		  (=conjunct o term)
		  (sigma o subst)
		  )
		 )
		(premises L1 (+ L2))
		(application-condition
		 (mand (mnot (meta-p psi))
		       (mnot (meta-p phi))
		       (alphaunify phi psi sigma))              ;;termmgu  changed jzimmer 4.10.99
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )
		
		(conclusions (- L3))
		(decl-content
		 (L1 () psi)
		 (L2 () =conjunct    ("OPEN" () ()))
		 (L3 () phi          ("solved" () (L1 L2)));;statt "subst-apply*"
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "Like <A HREF=\"./AlphaUnify-m-a.lml\">AlphaUnify-m-a</A> this method closes
the subgoal <FONT COLOR=\"#0000ff\">l3</FONT>, if it is alpha-unifiable with the precondition
<FONT COLOR=\"#0000ff\">l1</FONT> that must not be a single meta-variable. The substitution
<FONT COLOR=\"#00aa10\">sigma</FONT> is introduced into the planning state as a conjunction of equations
(<FONT COLOR=\"#0000ff\">l2</FONT>).
"))
		
		(remark "By applying the equation(s) =conjunct to psi, we obtain phi.")
		)


;;*******************************************************
;;*               All the other methods                 *
;;*******************************************************
(infer~defmethod ExFalsoQuodlibet-m
		 (outline-mappings (((existent existent existent) ExFalsoQuodlibet-m-b)
				    ))
		 (help "Follow everything from a contradiction."))

(meth~defmethod ExFalsoQuodlibet-m-b ExFalsoQuodlibet-m
		(in base)
	        (rating 20)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (param o prln)
		  (phi o term)
		  (psi o term)
		  (Thm o term)
		  (positions o poslist)
		  )
		 
		 )
		
		(premises L1 L2)
		(application-condition
		 )
		
		(conclusions (- L5))
		
		(decl-content
		 (L1 () phi)
		 (L2 () (not phi))
		 
		 (L3 () false           ("NotE" () (L1 L2)))
		 (L5 () Thm             ("falsee" () (L3)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "")
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


(infer~defmethod notI-m
		 (outline-mappings (((existent nonexistent) notI-m-b)))
		 (help ""))

(meth~defmethod notI-m-b notI-m
		(in base)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a o term))
		 )
		
		(premises (+ L1))
		(conclusions (- l2))
		
		(decl-content
		 (L0 ()   A                     ("Hyp"))
		 (L1 (L0) FALSE                 ("Open" () ()))
		 (L2 ()  (not A)                ("notI" () (L1)))
		 )
		(manual (author "AM")
			(examples "All kinds of theorems, eg. sqrt.")
			(documentation "Applies ND rule notI, the standard method to show a negated formula."))
		)


(infer~defmethod Indirect-m 
		 (outline-mappings (((existent existent nonexistent) Indirect-m-b)))
		 (parameter-types ndline))

(meth~defmethod Indirect-m-b Indirect-m
		(in base)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 
		 (sorted-meta-variables
		  (assump o prln)
		  (assumplist o prlnlist)
		  (ass o term)
		  (goal o term)
		  ))

		(parameters assump)
		(just-parameters assumplist)
		
		(premises (- L20) (+ L40))
		(conclusions (- L60))
		
		(application-condition
		 (mand (bound-p assump)
		       (mequal (currprlnformula assump)
			       ass)))
		
		(outline-computations
		 (assumplist (mlist assump)))
		(outline-actions
		 (L40 (unsponsor L20)))

		 
		(decl-content
		 (L20 ()    ass                        ) 
		 (L30 ()    (not goal)                 ("Hyp" () ()))    
		 (L40 (l30) (not ass)                  ("OPEN" () ))
		 (L50 (l30) false                      ("note" () (L20 L40)))
		 (L60 ()    goal                       ("indirect" () (l50)))
		 )
		(remark 
		 ("<U>Indirect:</U><BR>
We prove the theorem indirectly by showing that if the theorem does not hold, then we can
derive <A HREF=\"lpt:get-local-verbalisation L40\"> <TERM>(not ass)</TERM> </A><BR>
which contradicts the assumption <TERM>ass</TERM>.")
		 ("We prove the theorem indirectly by showing that if the theorem does not hold, then we can
derive<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> <TERM>(not ass)</TERM></A>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof L40)</LISP></BLOCKQUOTE></BLOCKQUOTE>
That contradicts the assumption <TERM>ass</TERM>.<BR>
Therefore the theorem <TERM>goal</TERM> holds."
		  "We prove the theorem indirectly by showing that if the theorem does not hold, then we can
derive<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> <TERM>(not ass)</TERM></A>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof L40)</LISP></BLOCKQUOTE></BLOCKQUOTE>
That contradicts the assumption <TERM>ass</TERM>.<BR>
Therefore the theorem <TERM>goal</TERM> holds.")
		 )
		(manual (author "MP")
			(examples "Limit theorems")
			(documentation "")))


(infer~defmethod "ImpE-m"
		 (outline-mappings (((nonexistent closed nonexistent) ImpE-m-rd)
				    ))
		 (help "The method for IMPLIES elimination in UNWRAPHYP."))

(meth~defmethod ImpE-m-rd ImpE-m
                (in base) 
                (rating 1)
		(reasoning :planning)
                (declarations
                 (sorted-meta-variables
		  (F1 o)
		  (positions o poslist)
		  (F2 o))
                 )
                (premises (- l1) (+ l2)) 
                (conclusions (+ l3))
                (decl-content
                 (l1 () (implies F1 F2))
                 (l2 ()  F1            ("Open" () ()))
                 (l3 ()  F2            ("ImpE" () (l2 l1)))
                 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "<BR>ImpE-m-rd is an essentially forward backchain method, i.e.,
works on assumptions F1 -> F2 and  produces a new subgoal F1 in order to provide a new assumption F2.
Moreover, it deletes the assumption F1 -> F2 from the planning state.<BR>
The method ist applicable, iff there is a focus set on the consequent."))
		
                (remark "<BR>ImpE-m-rd is an essentially forward backchain method, i.e., works on assumptions F1 -> F2 and  produces a new subgoal F1 in order to provide a new assumption F2. Moreover, it deletes the assumption F1 -> F2 from the planning state.<BR>")
                )


;;*******************************************************
;;*       The Diagonalization-methods                   *
;;*                                      .              *
;;*******************************************************
(infer~defmethod "Diag-by"
		 (outline-mappings (((existent existent nonexistent nonexistent nonexistent) diag-by)))
                 (help "The diagonalization method"))

(infer~defmethod "MEC"
		 (outline-mappings (((existent existent) MEC)))
                 (parameter-types term)
		 (help "Make explicit contradiction"))



;;; Remark: In order to bind a lambda-variable within a method term, i.e., the
;; variable Y in (exists (lam (y aa var) (and (N y) (= D (F y))))) below, during
;; the matching of a method, you have to represent it as a triple (name type sort),
;; where sort can be either VAR or VARLIST. You dont need to declare such variables
;; in SORTED-META-VARIABLES, they will be automatically defined in the method
;; environment while constructing the terms containing them. For instance, the
;; declaration of (x (bb aa) var) is not necessary needed, compare the variable
;; (y aa var) in the formula of L3. If you dont want to get a binding of a lambda-variable,
;; you have to represent it in a pair as known:
(meth~defmethod diag-by diag-by
		(in base)
		(rating 1)
		(declarations
		 (type-variables aa bb)
                 (sorted-meta-variables
		  ;;(meta-var typ sort)
		  (x (bb aa) var) (phi1 (o (bb aa))) (phi3 o) (N (o aa) const) (F (bb aa aa))
                  (mv-D (bb aa) metavar) (idx aa const) (mv-IP (o (bb aa) aa (bb aa)) metavar)
		  (conj1 o) (subst2 o sub)
                  ))
		(premises (- l1) (+ l2) (+ l6) (+ l7))
		(conclusions (- th))
                (application-condition
		 (mand (mbind N-var (type-newvar (:type (o aa))))
                       (subterm-matcher conj1 N-var subst2)
		       (mbind N (subst-apply subst2 N-var)))
		 )
		;;(application-constraint
		;; (cand (cdiffer (:term (mv-D idx))
		;;		(:term ((F idx) idx)))
		;;       (csubterm idx (:term (mv-D idx)))
		;;       (cbind mv-IP (:term (lam (x (bb aa)) (y aa) (z (bb aa))
		;;				     (equiv (x y) (not (z y)))))))
		;; )
		(outline-computations
		 (phi3 (termrploccs conj1 y idx))
		 )
		(outline-actions
		 (l7 (sponsor l5))
		 (l6 (unsponsor l5) (sponsor l7))
		 )
		(outline-orderings
                 (before l2 l6) (before l6 l7) 
                 )
		(decl-content 
		 (l1 ()   (forall (lam (x (bb aa) var)
                           (implies (phi1 x) 
				    (exists (lam (y aa var) (and conj1 (= x (F y)))))))))
                 (l2 ()   (phi1 mv-D)                                        ("Open" () ())) 
                 (l3 ()   (exists (lam (y aa var)
				       (and conj1 (= mv-D (F y)))))         ("Assertion" () (l1 l2)))
                 (l5 ()   (and phi3 (= mv-D (F idx)))                        ("Hyp" () ()))
		 (l7 (l5) (N idx)                                            ("Open" () ()))
		 (l8 (l5) (= mv-D (F idx))                                   ("AndER" () (l5)))
                 (l6 (l5) (mv-IP mv-D idx (F idx))                           ("Open" () ()))
                 (l4 (l5) false                                              ("MEC" (mv-D) (l6 l8) "unexpanded"))
                 (th ()   false                                              ("ExistsE" (idx) (l3 l4))) 
		 )
		(proc-content schema-interpreter)
		(remark "")
		(manual (author "Lassaad")
			(examples "Cantor's Theorem")
			(documentation "Try to prove a contradiction by diagonalization wrt. some indexing property."))
		)





(infer~defmethod "BHImp1"
		 (outline-mappings (((existent) BHImp1)))
                 (parameter-types term-list)
		 (help "Applying BH ..."))

(meth~defmethod BHImp1 BHImp1 (in base) (rating 5)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
                 (sorted-meta-variables
		  (Mv-formulas list termlist)
                  (Mv (o aa) metavar) (arg aa) (phi o))
                 )
		(parameters Mv-formulas)
		;;(application-constraint
		;; (cbind Mv (bledsoe-instance Mv Mv-formulas))
		;;
		(expansion-computations
		 (I2 (if (conjunction-p (:term (Mv arg)))
			 (inference "AndEL") (inference "Weaken")))
		 )
		(conclusions (- l3))
                (decl-content
                 (l1 ()   (Mv arg)                 ("Hyp" () ()))
                 (l2 (l1) phi                      (I2 () (l1)))
		 (l3 ()   (implies (Mv arg) phi)   ("ImpI" () (l2)))
                 )
                (proc-content schema-interpreter)
                (remark "Bledsoe heuristic to an implication whose hypothesis is an application with 1 argument.")
                )


;;========================================
;; the methods for analogy
;;========================================


(infer~defsupermethod Internal-Analogy-S
		      (supermethod Internal-Analogy-S-B)
		      (help "The Internal-Analogy inference."))

(meth~defsupermethod Internal-Analogy-S-B Internal-Analogy-S
		(in base)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (Delta o prlnlist)
		  (psi o term)
		  (result o planlist)
		  (prems o prlnlist)
		  (concls o prlnlist)
		  (hyps o prlnlist)
		  (source o prln)
		  )
		 
		 )
		(parameters source)
		(premises (+ prems) )
		(application-condition
		 (mand (meval result)
		       )
		 )
		
		(outline-computations
		 (hyps (mthird result))
		 (prems (mfourth result))
		 (concls (mfifth result))
		 )
		
		
		(conclusions (- target) (+ hyps) (+ concls))
		(expansion-function (plan~expand-supermethod target result))
		(decl-content
		 (target () psi)
		 )
		(proc-content
		 (ana~ina-supermethod source target)
		 )
		
		(manual)
		(remark ("<U>Internal Analogy:</U><BR>
<TERM>psi</TERM><BR> is solved analogous to<BR><A HREF=\"ltp:get-local-verbalization source\">
<LISP>(verbalize-node source)</LISP></A>.")
			("This goal is solved analogous to<BR><LISP>(verbalize-node source)</LISP>."
			 "This goal is solved analogous to<BR><LISP>(verbalize-node source)</LISP>.")
			))


(infer~defsupermethod AnalogyRef-S-bw
		      (supermethod AnalogyRef-S-B)
		      (help "The analogy backward adaption."))

(meth~defsupermethod AnalogyRef-S-B
		     AnalogyRef-S-bw
		     (in base)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (Delta o prlnlist)
		  (psi o term)
		  (result o planlist)
		  (prems o prlnlist)
		  (concls o prlnlist)
		  (hyps o prlnlist)
		  )
		 
		 )
		(premises (+ prems) )
		(application-condition
		 (mand (meval result)
		       )
		 )
		
		(outline-computations
		 (hyps (mthird result))
		 (prems (mfourth result))
		 (concls (mfifth result))
		 )
		
		
		(conclusions (- l1) (+ hyps) (+ concls))
		(expansion-function (plan~expand-supermethod l1 result))
		(decl-content
		 (l1 () psi)
		 )
		(proc-content
		 (ana~try-ref l1
			      ())
		 )
		(manual (author "Carsten Ullrich")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "")
		)


(infer~defsupermethod AnalogyRef-S-fw
		      (supermethod AnalogyRef-S-F)
		      (help "The analogy forward adaption."))

(meth~defsupermethod AnalogyRef-S-F
		     AnalogyRef-S-fw
		     (in base)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (Delta o prlnlist)
		  (psi o term)
		  (result o planlist)
		  (prems o prlnlist)
		  (concls o prlnlist)
		  (hyps o prlnlist)
		  )
		 
		 )
		(premises (- L1) (+ prems))
		(application-condition
		 (mand (meval result)
		       )
		 )
		
		(outline-computations
		 (prec (msecond result))
		 (hyps (mthird result))
		 (prems (mfourth result))
		 (concls (mfifth result))
		 )
		
		
		(conclusions (+ hyps) (+ concls))
		(expansion-function (plan~expand-supermethod (first concls) result))
		(decl-content
		 (l1 () psi)
		 )
		(proc-content
		 (ana~try-ref ()
			      l1
			      )
		 )
		(manual (author "Carsten Ullrich")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "")
		)


(infer~defmethod "AnalogyClose-m"
		 (outline-mappings (((existent) AnalogyClose-m-b)))
		 (help "Needed for analogy demos. Internal use only."))

(meth~defmethod AnalogyClose-m-b AnalogyClose-m 
                (in base)
		(rating 0)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (F o))
		 )
		(premises )
		(conclusions (- l2))
		(decl-content
		 (l2 () F  ("solved" () ()))
		 )
		(proc-content schema-interpreter )		
		(remark "Needed for analogy demos. Internal use only.")
		)

(infer~defmethod "OrE-m" 
		 (outline-mappings (((existent existent nonexistent nonexistent) OrE-m-b)))
		 (parameter-types )
		 (help "Application of ORE rule."))

(meth~defmethod ORE-m-b ORE-m
		(in base)
		(rating 0)
		
		(reasoning :planning)
		
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  (F o term)))
		
		(premises (- l0) (+ l2) (+ l4))
		(conclusions (- l5))
		
		(decl-content
		 (l0 () (or phi psi))
		 (l1 () phi ("Hyp" () ()))
		 (l2 (l1) F ("Open" () ()))
		 (l3 () psi ("Hyp" () ()))
		 (l4 (l3) F ("Open" () ()))
		 (l5 () F ("Ore" ())))
		(proc-content schema-interpreter)
		(remark "Controlled application the ore** tactic.")
		)




(infer~defmethod "Or-E**-m" 
		 (outline-mappings (((existent nonexistent) Or-E**-m-b)))
		 (parameter-types ndline-list)
		 (help "Controlled application the ore** tactic."))


#|(meth~defmethod OR-E**-m-b OR-E**-m
		(in base)
		(rating 0)
		(parameters disjunctions)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (New-Lines o prlnlist)
		  (New-Prems o prlnlist)
		  (F o)
		  (disjunctions o prlnlist)))
		(premises (+ new-prems))
		(application-condition
		 (mand (bound-p disjunctions)
		       (nodes-disjunction-p disjunctions)
		       (test-tactic (:symbol :ore**) (mlist l1 disjunctions) (mnil))))
		(conclusions (- l1))
		(outline-computations
		 (New-Lines (apply-tactic (:symbol :ore**) (mlist l1 disjunctions) (mnil)))
		 (New-Prems (mset-difference (msecond New-Lines) disjunctions)))
		(outline-actions
		 (New-Prems (unsponsor disjunctions)))
		(decl-content
		 (l1 () F ("Ore**" ())))
		(proc-content apply-tactic)
		(manual (author "Volker Sorge")
			(examples "Group Theory")
			(documentation "The method Or-E**-m-b is a partial specification of the
<A HREF=\"../commands/ore**.lml\">ORE**</A> Tactic."))
		(remark "Controlled application the ore** tactic.")
		)
|#


(meth~defmethod OR-E**-m-b OR-E**-m
		(in base)
		(rating 0)
		(parameters disjunctions)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (New-Lines o prlnlist)
		  (New-Hyps o prlnlist)
		  (New-Prems o prlnlist)
		  (F o)
		  (disjunctions o prlnlist)
		  ))
		(premises (+ new-prems))
		;;; (just-parameters nilparam)
		(application-condition
		 (mand (bound-p disjunctions)
		       (nodes-disjunction-p disjunctions)))
		(conclusions (- l1))
		(outline-computations
		 (New-Lines (apply-tactic (:symbol :ore**) (mlist l1 disjunctions) (mnil)))
		 (New-Prems (mset-difference (msecond New-Lines) disjunctions))
		 (New-Hyps (mthird New-Lines))
		 )
		(outline-actions (New-Prems (sponsor-if-hyp New-Hyps) (unsponsor disjunctions)))
		(decl-content
		 (l1 () F ("Ore**" () New-Prems))
		 )
		(proc-content schema-interpreter)
		(manual (author "Volker Sorge")
			(examples "Group Theory")
			(documentation "The method Or-E**-m-b is a partial specification of the
<A HREF=\"../commands/ore**.lml\">ORE**</A> Tactic."))
		(remark "Controlled application the ore** tactic.")
		)

(infer~defmethod "=subst-m"
		 (outline-mappings (((existent nonexistent existent) =subst-m-b)
				    ((nonexistent existent existent) =subst-m-f)))
		 (parameter-types position)
		 (help "Controlled application the =subst tactic."))

#|

=subst-m-b Version with apply-tactic!
Problem: Instantiations of meta-variables are ignored by the tactic!

(meth~defmethod =subst-m-b =subst-m
		(in base)
		(rating 0)
		(parameters position)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (New-Lines o prlnlist)  (New-prem o prln)
		  (f o)
		  (phi aa)
		  (phi-prime aa)
		  (position o pos)
		  ))
		(premises (+ new-prem) l2)
		(application-condition
		 (mand (bound-p position)
		       (test-tactic (:symbol :=subst) (mlist l1 (mnil) l2) (mlist position))
		       ))
		(conclusions (- l1))
		(outline-computations
		 (new-lines (apply-tactic (:symbol :=subst) (mlist l1 (mnil) l2) (mlist position)))
		 (new-prem (msecond new-lines)))
		(decl-content 
		 (l2 () (= phi phi-prime))
		 (l1 () F ("=subst" () ())))
		(proc-content apply-tactic))

|#

(meth~defmethod =subst-m-f =subst-m
		(in base)
		(rating 0)
		(parameters position)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (New-Lines o prlnlist)  (New-prem o prln)
		  (f o) (f-prime o)
		  (phi aa) (phi-prime aa)
		  (position o pos)
		  ))

		(parameters position)
		(premises l0 l2)
		(conclusions (+ l1))
		
		(application-condition
		 (mand (bound-p position)
		       (equality-pos-exist F l2 position)
		       ))
		
		(outline-computations
		 (F-prime (compute-=subst-prem f l2 position)))
		
		(decl-content
		 (l2 () (= phi phi-prime)     )
		 (l0 () F                     )
		 (l1 () F-prime               ("=subst" (position) (l0 l2))))
		
		(proc-content schema-interpreter)
		(remark "")
		)

(meth~defmethod =subst-m-b =subst-m
		(in base)
		(rating 0)
		(parameters position)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (New-Lines o prlnlist)  (New-prem o prln)
		  (f o) (f-prime o)
		  (phi aa) (phi-prime aa)
		  (position o pos)
		  ))
		
		(premises (+ l0) l2)
		(conclusions (- l1))

		(application-condition
		 (mand (bound-p position)
		       (equality-pos-exist f l2 position)
		       ))
		
		(outline-computations
		 (F-prime (compute-=subst-prem f l2 position)))
		
		(decl-content
		 (l2 () (= phi phi-prime))
		 (l0 () F-prime                       ("Open" () ()))
		 (l1 () F                             ("=subst" (position) (l0 l2))))
		
		(proc-content schema-interpreter)
		(remark "")
		)

(infer~defmethod "=subst*-m"
		 (outline-mappings (((existent nonexistent nonexistent) =subst*-m-b)))
		 (parameter-types term-list position-list)
		 (help ""))

(meth~defmethod =subst*-m-b =subst*-m
		(in base)
		(rating 0)
		
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (f o) (f-prime o)
		  (position o pos)
		  (equation-nodes o prlnlist)
		  (positions o poslist)
		  (equations o termlist)
		  ))
		
		(parameters equations positions)
		
		(application-condition
		 (mand (bound-p positions)
		       (bound-p equations)
		       (equality-poses-exist f equations positions)
		       ))
		
		(outline-computations
		 (F-prime (compute-=subst*-prem f equations positions))
		 (equation-nodes (compute-open-nodes l1 equations)))
		;;(outline-actions
                ;; (l0 (sponsor equation-nodes)))
		
		(premises (+ l0) (+ equation-nodes))
		(conclusions (- l1))
		
		(decl-content
		 (l0 () F-prime                       ("Open" () ()))
		 (l1 () F                             ("=subst*" () ()))
		 )
		
		(proc-content schema-interpreter)
		(remark "")
		)





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

;(meth~defmethod ;;method name inference name
;                andi-m-a AndI-m
;                (in base)
;                (rating 2)
;                (declarations
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (psi o))
;                 )
;                (premises l1 l2)
;                (conclusions (- l3))
;                (decl-content
;                 (l1 () phi)
;                 (l2 () psi)
;                 (l3 () (and phi psi) ("AndI" () (l1 l2)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Applying the AND introduction")
;                )
;
;
;(meth~defmethod andi-m-f AndI-m
;                (in base)
;                (rating 0.3)
;                (declarations
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (psi o))
;                 )
;                (premises (- l1) (- l2))
;                (conclusions (+ l3))
;                (decl-content
;                 (l1 () phi)
;                 (l2 () psi)
;                 (l3 () (and phi psi) ("AndI" () (l1 l2)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Forward applying the AND introduction")
;                )


;(meth~defmethod ImpE-m-rd ImpE-m
;                (in base) 
;                (rating 1)
;                (declarations
;                 (sorted-meta-variables
;                             ;;(meta-var type sort)
;                  (F1 o) (F2 o))
;                 )
;                (premises (- l1) (+ l2)) 
;                (conclusions (+ l3))
;                (decl-content
;                 (l1 () (implies F1 F2))
;                 (l2 ()  F1            ("Open" () ()))
;                 (l3 ()  F2            ("ImpE" () (l2 l1)))
;                 )
;                (proc-content schema-interpreter)               
;                (remark "ImpE-m-rd is an essentially forward backchain method, i.e., works on assumptions F1 -> F2 and  produces a new subgoal F1 in order to provide a new assumption F2. Moreover, it deletes the assumption F1 -> F2 from the planning state.")
;                )

;(meth~defmethod ImpE-m-b ImpE-m
;                (in base)
;                (rating 1)
;                (declarations
;                 (sorted-meta-variables 
;                  (F1 o) (F2 o))
;                 )
;                (premises (+ l1) l2)
;                (conclusions (- l3))
;                (decl-content
;                 (l1 () (implies F1 F2) ("Open" () ()))
;                 (l2 ()  F1)
;                 (l3 ()  F2            ("ImpE" () (l2 l1)))
;                 )
;                (proc-content schema-interpreter)               
;                (remark "ImpE-m-b is a backward working method that takes a goal F2 and an assumption F1 and produces a new subgoal F1-> F2.")
;                )

;;; ande-m-a 
;(meth~defmethod ande-m-a AndE-m
;                (in base)
;                (rating 2)
;                (declarations
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (psi o))
;                 )
;                (premises l1)
;                (conclusions (- l2) (- l3))
;                (decl-content
;                 (l1 () (and phi psi))
;                 (l2 () phi ("AndEL" () (l1)))
;                 (l3 () psi ("AndER" () (l1)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Applying AND elimination")
;                )


;(meth~defmethod diag-by diag-by
;                (in base)
;                (rating 1)
;                (declarations
;                 (type-variables aa bb)
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (x (bb aa) var) (phi1 (o (bb aa))) (N (o aa)) (F (bb aa aa))
;                  (D (bb aa) metavar) (idx aa const) (IP (o (bb aa) aa (bb aa)) metavar) 
;                  ))
;                (premises (- l1) (+ l2) (+ l6))
;                (conclusions (- th))
;                (application-constraint
;                 (cand (cdiffer (:term (D idx))
;                                (:term ((F idx) idx)))
;                       (csubterm idx (:term (D idx)))
;                       (cbind IP (:term (lam (x (bb aa)) (y aa) (z (bb aa))
;                                             (equiv (x y) (not (z y)))))))
;                 )
;                (outline-actions
;                 (l6 (unsponsor l5) (sponsor l7))
;                 )
;                (outline-orderings
;                 (before l2 l6) 
;                 )
;                (decl-content 
;                 (l1 ()   (forall (lam (x (bb aa) var)
;                           (implies (phi1 x) 
;                                    (exists (lam (y aa var)
;                                     (and (N y) (= x (F y))))))))     )
;                 (l2 ()   (phi1 D)                                 ("Open" () ())) 
;                 (l3 ()   (exists (lam (y aa var)
;                           (and (N y) (= D (F y)))))               ("Assertion" () (l1 l2)))
;                 (l5 ()   (and (N idx) (= D (F idx)))              ("Hyp" () ()))
;                 (l7 ()   (N idx)                                  ("AndEL" () (l5)))
;                 (l8 ()   (= D (F idx))                            ("AndEL" () (l5)))
;                 (l6 (l5) (IP D idx (F idx))                       ("Open" () ()))
;                 (l4 (l5) false                                    ("MEC" (D) (l6 l8) "untested"))
;                 (th ()   false                                    ("ExistsE" (idx) (l3 l4))) 
;                 )
;                (proc-content schema-interpreter)
;                (remark "Try to prove a contradiction by diagonalization wrt. some indexing property.")
;                )




;;;; Methods for MOReasoning:

;(infer~defmethod "MOR-ForallI*"
;                 (outline-mappings (((existent nonexistent) MOR-foralli*)))
;                 (help "The method for iterated FORALL introduction"))
;
;(meth~defmethod mor-foralli* mor-ForallI*
;                (in base)
;                (rating 10)
;                (mor-method t)
;                (declarations
;                 ;(type-variables aa)
;                 (sorted-meta-variables
;                  ;;; LC: The declaration of these variables can be avoided. It is
;                  ;; clear from their occurrences in the declarative content that
;                  ;; they are terms of type o:
;                  (phi o) (phi1 o)
;                  )
;                 )
;                (premises (+ l1))
;                (application-condition
;                 (mnot (mequal (applfunc phi) forall)))
;                (outline-computations
;                 (cL (vars-newconsts xL))
;                 (phi1 (subst-apply (subst-create xL cL) phi ))
;                 )
;                (conclusions (- l2))
;                (decl-content 
;                 (l1 () phi1                      ("Open" () ()))
;                 (l2 () (forall (lam (xL list varlist) phi)) ("ForallI*" (cL) (l1)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Backward application of the tactic ForallI*")
;                )

;;;EQV-ASS2R
;(infer~defmethod "MOR-EqvAss2R"
;                 (outline-mappings (((existent existent nonexistent) mor-EqvAss2r)))
;                 (help "A method for applying an equivalence assertion to its instantiated right side to conclude the instantiated left side"))
;
;;;;was def-intro
;(meth~defmethod mor-eqvass2r
;                mor-EqvAss2R
;                (in base)
;                (rating 5)
;                (mor-method t) 
;                (declarations
;                 ;;(type-variables aa)
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (psi o) (psi1 o)
;                  (tL list termlist) (p (o list) const) ;;muss nicht unbedingt eine Konstante sein
;                  ;;(p (o aa) term) Doch
;                  )
;                 )
;                (premises l1 (+ l4))
;                (outline-computations
;                 (psi1 (n-normalize (subst-apply (subst-create xL tL) psi)))
;                 )
;                (conclusions (- l5))
;                (decl-content
;                 (l1 () (forall (lam (xL list varlist) (equiv (p xL) psi))))
;                 (l2 () (equiv (p tL) psi1)                        ("ForallE*" (tL) (l1)))
;                 (l3 () (implies psi1 (p tL))                      ("EquivER" () (l2)))
;                 (l4 () psi1                                       ("Open" () ()))
;                 (l5 () (p tL)                                     ("ImpE" () (l4 l3)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Backward application of an equivalence assertion to its right side to prove the corresponding left side")
;                )


;(infer~defmethod "MOR-EquivI"
;                 (outline-mappings (((existent nonexistent nonexistent) MOR-EquivI)))
;                 (help "The method for EQUIV introduction"))
;
;(meth~defmethod MOR-EquivI MOR-EquivI
;                (in base)
;                (rating 10)
;                (mor-method t)
;                (declarations
;                 (sorted-meta-variables
;                  ;;(meta-var type sort)
;                  (phi o) (psi o))
;                 )
;                (premises (+ l1) (+ l2))
;                (conclusions (- l3))
;                (decl-content 
;                 (l1 () (implies phi psi)       ("Open" () ()))
;                 (l2 () (implies psi phi)       ("Open" () ()))
;                 (l4 () (and (implies phi psi)
;                            (implies psi phi)) ("AndI" () (l1 l2)))
;                 (l3 () (equiv phi psi)         ("EquivI" () (l4)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Backward applying EQUIV introduction")
;                )

;;;DefnI-m
;(infer~defmethod "mor-DefnI"
;                 (outline-mappings (((existent nonexistent) mor-DefnI)))
;                 (help "Reduction of an atom by substituting the predicate symbol by its the theory definition."))
;
;;;;was def-intro
;(meth~defmethod mor-DefnI
;                mor-DefnI
;                (in base)
;                (mor-method t)
;                (rating 1)
;                (declarations
;                 ;;(type-variables aa)
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (tL list termlist) (thing (o list) const)
;                  )
;                 )
;                (premises (+ l1))
;                (application-condition
;;;;              (th-restricted-definition (:symbol :base) thing thing-def))
;                 (th-definition thing thing-def))  ;;; to general especially for definitions in base-theory
;                (outline-computations
;                 (phi (beta-normalize (:term (thing-def tL))))
;                 )
;                (conclusions (- l2))
;                (decl-content
;                 (l1 () phi            ("Open" () ()))
;                 (l2 () (thing tL)     ("DefnI" (thing thing-def) (l1)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "Backward application of a theory definition.")
;                )
(infer~defmethod NotnotE-m
		 (outline-mappings (((nonexistent existent) NotnotE-m-f)))
		 (help "Elimination of double-negation."))

(meth~defmethod NotnotE-m-f NotnotE-m
		(in base)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (F o term)
		  )
		 )
		(premises (- L1))
		(conclusions (+ L2))
		
		(decl-content
		 (L1 () (not (not F)))
		 (L2 ()  F             ("notnote" () (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method NotnotE-m-f is a partial specification of the
<A HREF=\"../commands/notnote.lml\">notnote</A> tactic."))
		
		(remark "The elimination of a double-negation.")
		)

|#


;MP: for experimental use
;(infer~defmethod Imp2or-m
;                 (outline-mappings (((existent nonexistent) Imp2or-m-b)))
;                 (help "Tactic imp2or."))
;
;
;(meth~defmethod imp2or-m-b imp2or-m
;                (in base)
;                (rating 0)
;                (reasoning :planning)
;                (declarations
;                 (sorted-meta-variables
;                  (phi o term)
;                  (psi o term)
;                  (alpha o term)
;                  (beta o term)))
;                
;                (premises (+ L00))
;                (conclusions (- L30))
;
;                (application-condition
;                  (mand (appl-p phi)
;                        (mequal (applfunc phi)(:term not))))
;                 
;                (outline-computations
;                 (alpha (appl-create (:Term implies) (mlist (mfirst (applargs phi)) psi))))
;                
;                (decl-content
;                 (l00 () alpha         ("open"   ()()))   
;                 (l30 () (or phi psi)                ("imp2or" ()(l00)))
;                 )
;                
;                (manual (author "MP")
;                        (examples "all kind of problems")
;                        (documentation ""))
;                
;                (remark "")
;                )
;





(infer~defmethod Reflex-m
                (outline-mappings (((existent) reflex-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod reflex-m-b reflex-m
               (in base)
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
                (mequal phi psi))
                ;;(unify phi psi))
               
               (decl-content
                (l1 () (= phi psi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "ZMZ, Homo")))


(infer~defmethod Reflexu-m
                (outline-mappings (((existent) reflexu-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod reflexu-m-b reflexu-m
               (in base)
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
                (unify phi psi))
               
               (decl-content
                (l1 () (= phi psi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "ZMZ, Homo")))



(infer~defmethod ReflexDirect-m
                (outline-mappings (((existent) reflexdirect-m-b)))
                (help "Closes a goal of the form 'a = a'"))

(meth~defmethod reflexdirect-m-b reflexdirect-m
               (in base)
               (rating 50)
               (reasoning :planning :middle-out)
               (declarations
                (type-variables aa)
                (sorted-meta-variables
                 (phi aa term)))
               
               (premises )
               (conclusions (- L1))
               
               (application-condition
                )
               
               (decl-content
                (l1 () (= phi phi) ("=ref" (phi) ())))
	       (manual (author "AMeier")
		       (examples "Limit")))

;;*******************************************************
;;*    Methods for assertion application                *
;;*******************************************************

(infer~defmethod "Assertion2-m"
		 (outline-mappings (((nonexistent existent nonexistent) Assertion2-m)))
		 ;; only the length of the slot parameter-types matters
		 (parameter-types position term term)
		 (help "..."))
	
(meth~defmethod Assertion2-m Assertion2-m (in base) (rating 33)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (pos o pos)
		  (signs list signs)
		  (tl list termlist)
		  (pi o term)
		  (pi1 o term)
		  (xi o term)
		  (xl list termlist)
		  ))
		(parameters pos signs tl xl)
		(premises l1 (+ l2))
		(conclusions (+ l3))
		(application-condition
		 (lassert phi pos signs tl pi xi xl)
		 )
                (outline-computations
                 (pi1 (beta-normalize pi))
                 )
		(decl-content
		 (l1 () phi)
		 (l2 () pi1            ("Open" () ()))
		 (l3 () xi             ("Assertion" () (l1 l2)))
		 )
		(proc-content schema-interpreter)		
                (manual (author "Lassaad Scholl")
                        (examples "")
                        (documentation "See Lassaads diss."))
		(remark "")
		)

(infer~defmethod "Assertion2P-m"
		 (outline-mappings (((nonexistent existent existent) Assertion2P-m)))
		 ;; only the length of the slot parameter-types matters
		 (parameter-types position term)
		 (help "..."))
	
(meth~defmethod Assertion2P-m Assertion2P-m (in base) (rating 3)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (pos o pos)
		  (signs list signs)
		  (tl list termlist)
		  (psi o term)
		  (xi o term)
		  (xi1 o term)
		  ))
		(parameters pos signs)
		(premises l1 (- l2))
		(conclusions (+ l3))
		(application-condition
		 (mand (literal-subterms psi tl)
		       (assert-with phi pos signs tl xi))
		 )
                (outline-computations
                 (xi1 (beta-normalize xi))
                 )
		(decl-content
		 (l1 () phi)
		 (l2 () psi)
		 (l3 () xi1            ("Assertion" () (l1 l2)))
		 )
		(proc-content schema-interpreter)		
                (manual (author "Lassaad Scholl")
                        (examples "")
                        (documentation "See Lassaads diss."))
		(remark "")
		)

;; note: strategic methods are only used in Lassaads planner
;; for all other planners a dummy macro is defined in omega/omega-3/prog/method/method.lisp
;; if there is an error reading this theory, make sure you checked out the file above

; (meth~defstrategicmethod Assertion2P-m
; 			 (
; 			  inputs-for-assertion-m
; 			  inputs-for-assertion2p-m
; 			  methods-after-assforw
; 			  supports-after-assforw))

; (meth~defstrategicmethod Assertion2-m
; 			 (
; 			  inputs-for-assertion-m
; 			  inputs-for-assertion2p-m
; 			  literals-before-composed
; 			  composed-after-literals
; 			  no-literal-for-assback
; 			  supports-for-assforw))

;; default strategy
(setq plan*control-rules '(
			   inputs-for-assertion-m
			   inputs-for-assertion2p-m
			   literals-before-composed
			   composed-after-literals
			   no-literal-for-assback
			   supports-for-assforw))


(infer~defmethod CaseSplit-m
		 (outline-mappings (((existent nonexistent nonexistent) CaseSplit-m-b)
				    ))
		 (parameter-types term)
		 (help "The Case-Split inference."))

(meth~defmethod CaseSplit-m-b CaseSplit-m
		(in base)
	        (rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (formula o term)
		  (Thm o term)
		  )
		 )
		
		(parameters formula)
		
		(premises (+ L4) (+ L5))
		(conclusions (- L7))
		
		(application-condition
		 (bound-p formula))
		
		(decl-content
		 (L1 () (or formula (not formula))                        ("HYP" () ()))
		 (L2 () formula                                           ("HYP" () ()))
		 (L3 () (not formula)                                     ("HYP" () ()))
		 
		 (L4 (L1 L2) Thm                                          ("OPEN" () ()))
		 (L5 (L1 L3) Thm                                          ("OPEN" () ()))
		 
		 (L6 (L1) Thm                                             ("Ore" () (L1 L4 L5)))
		 (L7 ()  Thm                                              ("TND" () (L6)))
		 )
		(manual (author "Andreas Meier")
			(examples "Limit theorems")
			(documentation ""))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DefnI*-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "DefnI*-m"
		 (outline-mappings (((existent nonexistent) DefnI*-m-b)))
		 (parameter-types term)
		 (help ""))

(meth~defmethod DefnI*-m-b DefnI*-m
		(in base)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (psi o) (phi o)
		  (symbol o)))
				
		(premises (+ l0))
		(conclusions (- l1))
		(parameters symbol)
		
		(application-condition
		 (mand (bound-p symbol)
		       (termoccs symbol phi occs)
		       (Th-Definition symbol thing-def)
		       ))
		
		(outline-computations
		 (psi (beta-normalize (iterated-expansion-of-def phi symbol thing-def occs)))) 
		
		(decl-content
		 (l0 ()      psi                                                           ("Open" () ()))
		 (l1 ()      phi                                                           ("DefnI*" (symbol thing-def occs) (l0))))
		
		(proc-content schema-interpreter)
		(manual (documentation "Expand all occurrences of a defined symbol."))
		)




;; Methods to call ATPS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Otter-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Call-Otter-m"
		 (outline-mappings (((existent) Call-Otter-m-b)))
		 (help ""))

(meth~defmethod Call-Otter-m-b Call-Otter-m
		(in base)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)))
		
		(premises )
		(conclusions (- l1))
		
		(application-condition	
		 (otter-proves-node-p l1))

		(expansion-computations
		 (just (get-otter-just l1)))
		
		;;(expansion-computations
		;; (supports (getsupports l1))
		;; (nillist (getnillist)))
		
		(decl-content
		 (l1 ()      phi                                                      just)) ;; ("Otter" nillist supports)))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods calls OTTER to close a goal."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Bliksem-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Call-Bliksem-m"
		 (outline-mappings (((existent) Call-Bliksem-m-b)))
		 (help ""))

(meth~defmethod Call-Bliksem-m-b Call-Bliksem-m
		(in base)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)))
		
		(premises )
		(conclusions (- l1))
		
		(application-condition	
		 (bliksem-proves-node-p l1))

		(expansion-computations
		 (just (get-bliksem-just l1)))
		
		;;(expansion-computations
		;; (supports (getsupports l1))
		;; (nillist (getnillist)))
		
		(decl-content
		 (l1 ()      phi                                                      just)) ;; ("Otter" nillist supports)))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods calls BLIKSEM to close a goal."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Tramp-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Call-Tramp-m"
		 (outline-mappings (((existent) Call-Tramp-m-b)))
		 (help ""))

(meth~defmethod Call-Tramp-m-b Call-Tramp-m
		(in base)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (atpvar o)))
		
		(premises )
		(conclusions (- l1))
		
		(application-condition	
		 (tramp-proves-node-p l1 atpvar))

		(expansion-computations
		 (just (get-tramp-just l1 atpvar)))
		
		(decl-content
		 (l1 ()      phi                  just))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods calls BLIKSEM to close a goal."))
		)


