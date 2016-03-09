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


(infer~defmethod not-equal-m 
		 (outline-mappings (((existent) not-equal-m-b)
				    )))

(meth~defmethod not-equal-m-b
		not-equal-m
		(in divisibility)
		(rating 60)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (phi num term)
		  (psi num term)
		  ))

		
		(premises )
		(conclusions (- L30))

		(application-condition
		 (mnot (mequal phi psi)))
		
		(outline-computations
		 )
		      
		(decl-content
		 (L30 ()    (not (= phi psi))                    ("not-eq" () ()))
		 )

		(remark "")
				)

(meth~defcond myequal (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	     ;;; args: obj1, obj2
	     ;;; Returns: <T, mmapp> when obj1 and obj2 are keim~equal
	      ;;          Otherwise, <NIL, mmapp>
	      (if (print  args)
		  cmapp
		(meth~mapp-new-constraint cmapp NIL)))



(infer~defmethod apply-induct-m
		 (outline-mappings (((existent nonexistent nonexistent) apply-induct-m-b)
				    )))

(meth~defmethod apply-induct-m-b
		apply-induct-m
		(in  divisibility)
		(rating 60)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		   (n num const)
		   (phi o)
		   (phi0 o)
		   (phin o)
		   (phisort o)
		   (phisn o)
		   (sort (o num) term)
		  ))

		
		(premises (+ L10) (+ L20))
		(conclusions (- L30))

		(application-condition
		 (mequal (:term nat) sort))
		

		(outline-computations
		  (phin (subst-apply (subst-create (mlist x) (mlist n)) phi))
		  (phisn (subst-apply (subst-create (mlist x) (mlist (appl-create (:term s) n))) phi))
		  (phi0 (subst-apply (subst-create (mlist x) (mlist (:term 0))) phi))
		  (phisort (appl-create (:term nat)(:term 0)))
		 )
		      
		(decl-content
		  (L30 ()    (forall-sort (lam (x num var) phi) sort)                    ("apply-induct" () ()))
		  (L20 ()    phi0               ("open" ))
		  (L10 (l5 l0)   phisn     ("open"))
		  (l5  ()    phin             ("hyp"))
		  (l0  ()    phisort               ("hyp"))
		  )

		(remark ""))
				

(meth~defcond myequal (args cmapp)
  (declare (edited  "11-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
	     ;;; args: obj1, obj2
	     ;;; Returns: <T, mmapp> when obj1 and obj2 are keim~equal
	     ;;          Otherwise, <NIL, mmapp>
	      (if (print  args)
		  cmapp
		(meth~mapp-new-constraint cmapp NIL)))

;;*******************************************************
;;*    Methods for quanitifier elimination              *
;;*******************************************************
(infer~defmethod "ForallIsort-m"
		 (outline-mappings (((existent nonexistent nonexistent) forallisort*-m)))
		 (help "The method for iterated FORALL introduction"))


;;; The above FORALLI*-M is more efficient and less declarative than the one below: 

(meth~defmethod forallisort-m ForallIsort-m
		(in  divisibility)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  ;;; LC: The declaration of these variables can be avoided. It is
		  ;; clear from their occurrences in the declarative content that
		  ;; they are terms of type o:
		  (phi o) (phi1 o) (srt (o aa)) (sort o)
                  )
		 )
		(premises (+ l1))
;;;		(application-condition
;;;		 (mnot (mequal (applfunc phi) forall-sort)))
		(outline-computations
                 (y (var-newconst x))
                 (sort (appl-create srt (mlist y)))
		 (phi1 (subst-apply (subst-create (mlist x)(mlist y)) phi ))
		 )
		(conclusions (- l3))
		(decl-content 
		 (l1 (l2) phi1                      ("Open" () ()))
                 (l2 () sort                   ("Hyp" () ()))
		 (l3 () (forall-sort (lam (x aa var) phi) srt) ("ForallI-sort" (y) (l1)))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort*"))
		)


;;*******************************************************
;;*    Methods for definition expansion                 *
;;*******************************************************
;;;
(infer~defmethod "defn-contract-m"
                 (outline-mappings (((existent nonexistent) defn-expand-m)))
                 (help "Reduction of an atom by substituting the predicate symbol by its the theory definition."))

;;;was def-intro
(meth~defmethod defn-contract-m
                defn-contract-m
                (in  divisibility)
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
              (th-restricted-definition (:symbol :generic) thing thing-def))
;;;                  (th-definition thing thing-def))  ;;; too general especially for definitions in base-theory
                (outline-computations
                 (phi (beta-normalize (:term (thing-def tL))))
                 )
                (conclusions (- l2))
                (decl-content
                 (l1 () phi            ("Open" () ()))
                 (l2 () (thing tL)     ("DefnI" (thing thing-def) (l1)))
                 )
                (proc-content schema-interpreter)
                (remark "");Backward application of a theory definition.)
                )



(infer~defmethod thati-m
		 (outline-mappings (((existent nonexistent nonexistent) thati-m-b)
				    )))

(meth~defmethod thati-m-b thati-m
                (in  divisibility)
		(rating 60)
		(reasoning :planning)
		(declarations
                 (type-variables aa)
		 (sorted-meta-variables
                   (Q (o aa)) (phi o)
                   (Pred (o aa))
		  ))

		
		(premises (+ L10) (+ L20))
		(conclusions (- L30))

		(application-condition
                   (mfalse)
                    )		

		(outline-computations
		 )
		      
		(decl-content
		  (L30 ()    phi                                                ("thati" () ()))
		  (L20 ()    (exists-unique (lam (x aa) (Pred x)))            ("open" ))
		  (L10 ()    (forall (lam (x aa) (implies (Pred x) (Q x))))   ("open"))
		  )

		(remark ""))
				

