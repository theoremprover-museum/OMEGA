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

(infer~defmethod LClam-m
		 (outline-mappings (
                                    ((existent) LClam-m-b)
				    ))
		 (help "Try to prove using the LClam system."))

(meth~defmethod LClam-m-b LClam-m 
		(in natural)
		(rating 40)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (G o term)
		  )
		 )
		
		(application-condition
                 (LClam-plan-problem L2 200)
                 )
		
		(conclusions (- l2))
		(decl-content
		 (l2 () G         ("solved" () (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Natural number stuff")
			(documentation "The method LClam-m-b tries to plan the problem."))
		
		(remark ("LClam solved the problem.")
			)
		)


(infer~defmethod Ripple-m
		 (outline-mappings (
                                    ((existent) Ripple-m-b)
				    ))
		 (help "Try to reduce the difference to a hypothesis using the ripple service of the LClam system."))

(meth~defmethod Ripple-m-b Ripple-m 
		(in natural)
		(rating 40)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (G o term)
                  (result o planlist)
                  (prec o prlnlist)
                  (prems o prlnlist)
		  )
		 )
                
                (premises (0 prec) (+ prems))
                
		(application-condition
                 (mand ;;(mnot (subterm (:term forall) G))
                  ;;(mnot (subterm (:term exists) G))
                  (meval result)
                  (mbind prems (mnil))
                  (mbind prec (mnil))
                  )
                 )
                (outline-computations
                 )
                
                (conclusions (- l2))
                ;;                (expansion-function (lclam~expand-rippling L1 result))
		(decl-content
		 (l2 () G         ("solved" () (L1)))
		 )
                (proc-content
                 (lclam~ripple L2
                               '(plus-nat-base2  ;; the wave rules...
                                 plus-nat-step2
                                 times-nat-base2
                                 times-nat-step2
                                 ;;power-nat-base
                                ;; power-nat-step
                                 )
                               20      ;; timeout in seconds
                               )
                 )
		
		(manual (author "Juergen Zimmer")
			(examples "Natural number stuff")
			(documentation "The method Ripple-m-b tries to reduce difference to a hypothesis using LClam's rippling service.."))
		
		(remark ("Rippling reduced the difference to a hypothesis.")
			)
		)


;;                 power-nat-base-zero
;;                 power-nat-base-one
(infer~defmethod Simplify-m
		 (outline-mappings (((existent nonexistent) Simplify-m-b)
				    ((nonexistent closed) Simplify-m-f)
				    ))
		 (help "Simplify with Maple."))

(meth~defmethod Simplify-m-b Simplify-m 
		(in natural)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (A num term)
		  (B num term)
		  (C num term)
		  (Ch num term)
		  (D num term)
		  (Dh num term)
		  )
		 )
		
		(premises (+ L1))
		(application-condition
		 (mand (mor
			(mequal (:term less) rel)
			(mequal (:term leq) rel)
			(mequal (:term =) rel)
			(mequal (:term greater) rel))
		       (mor (mand (cas-simplify A C)
				  (mbind D B)
				  (mbind position (:position (1))))
			    (mand (cas-simplify B D)
				  (mbind C A)
				  (mbind position (:position (2)))))))

		 
		(expansion-computations
		 (the-cas (:symbol mcas)))

		(conclusions (- l2))
		(decl-content
		 (L1 () (rel C D)                       ("OPEN" () ()))
		 (l2 () (rel A B)                       ("solved" (position the-cas) (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Simplify-m-b tries to simplify a term
<FONT COLOR=\"#00aa10\">B</FONT> on the left hand side of the inequality or an equation in the conclusion
<FONT COLOR=\"#0000ff\">l2</FONT>. It uses the term simplication algorithm of MAPLE(tm). <BR>
The method ist applicable, if the resulting term <FONT COLOR=\"#00aa10\">C</FONT> differs
from <FONT COLOR=\"#00aa10\">B</FONT>.<BR> The application of Simplify-m-b introduces
<FONT COLOR=\"#0000ff\">l1</FONT> as a new subgoal.
"))
		
		(remark ("<U>Simplify:</U><BR>
We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.")
			("We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-text-next l1)</LISP>"
			 "We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-cons-next l1)</LISP>"
			 ))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  cardinality-of-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~deffun cardinality-of-set (set)
  (declare (edited  "23-JAN-2003")
	   (authors Pollet)
	   (input   "A term+set constant")
	   (effect  "-")
	   (value   "The number of elements of set."))
  (when (term~set-p set) (post~read-object (length (term~normalform set)) (th~env 'natural) :existing-term)))

#|

(infer~defmethod Cardinality-of-set-m
		 (outline-mappings (((existent nonexistent) Cardinality-of-set-m-b)
				    ))
		 (help "Introduces the actual number, for the cardinality of a concrete set."))

(meth~defmethod Cardinality-of-set-m-b Cardinality-of-set-m 
		(in natural)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)		  (phiprime o term)
		  (set (o aa) term)	  (number num term)))
		
		(premises (+ L1))
		
		(conclusions (- l2))

		(application-condition
		 (mand
		  (termoccs (:term cardinality) phi position)
		  (mbind set (termatpos phi (posappend (posbutlast (mfirst position)) (:position (1)))))
		  (set-p set)))
		       
		 
		(outline-computations
		 (number (cardinality-of-set set))
		 (phiprime (TermRplAtPos phi (posbutlast (mfirst position)) number)))
		
		(decl-content
		 (L1 () phiprime                       ("OPEN" () ()))
		 (l2 () phi                            ("solved" () (L1)))
		 )
		
		(manual (author "Martin Pollet")
			(examples "with concrete sets")
			(documentation "Introduces the actual number, for the cardinality of a concrete set.")))

|#

		

(infer~defmethod Cardinality-of-set-m
		 (outline-mappings (((existent) Cardinality-of-set-m-b)
				    ))
		 (help "Introduces the actual number, for the cardinality of a concrete set."))

(meth~defmethod Cardinality-of-set-m-b Cardinality-of-set-m 
		(in natural)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (set (o aa) metavar) (number num metavar)
		  (phi o term) (sub o sub) (card num term)))
		
		(conclusions (- l2))

		(application-condition
		 (mand (mor (alpha-matcher (:term (= (cardinality set) number)) phi sub)
			    (alpha-matcher (:term (= number (cardinality set))) phi sub))
		       (set-p (subst-apply sub set))
		       (mbind card (cardinality-of-set (subst-apply sub set)))
		       (unify card (subst-apply sub number))))

		(decl-content
		 (l2 () phi                            ("solved" () (L1)))
		 )
		
		(manual (author "Martin Pollet")
			(examples "with concrete sets")
			(documentation "Introduces the actual number, for the cardinality of a concrete set.")))
