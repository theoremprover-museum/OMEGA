;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; not-in-set-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond not-elem-in-set-p (args cmapp)  
  (declare (edited  "18-SEP-2002")
	   (authors Pollet)
	   (input   "Takes an element and a set.")
	   (effect  "-")
	   (value   "True, if element is not an element of the set."
		    "(works only for some special terms)"))
	     (let* ((set-elems (and (term~set-p (second args))
				   (tyse=elements-of-set (second args) nil)))
		    (elem (and set-elems (peme=normalform (car args)))))
	       (meth~mapp-new-constraint cmapp
					 (and (term~special-p elem)
					      set-elems
					      (every #'(lambda (el)
							 ;(omega~trace "~A=~A " el elem)
							 (let ((normal  (peme=normalform el)))
							   (and normal
								(not (data~equal (peme=normalform el) elem))))) set-elems)))))



(defgeneric peme=normalform (term)
  (:method ((term term+constant))  ; a number or constant is already in normalform
	    term)
  (:method ((term term+set))     ; produce normalform for every element
	   (let ((setelems (mapcar #'peme=normalform (term~normalform term))))
	     (when (every #'term~p setelems) (post~read-object setelems  (th~env 'permutation) :set))))
  (:method ((term term+cyc))     ; consist of constants
	   (when (every	 #'term~constant-p (term~normalform term)) term))
  (:method ((term term+appl))    ; more compex objects
	   (when (perm~perm-type-p term)          ;case1 permutations, normalform by gap
	     (perm~gap-simplify term))))
	       
  
(infer~defmethod "not-in-set-m"
                 (outline-mappings (((existent) "not-in-set-m-c"))))

(meth~defmethod not-in-set-m-c not-in-set-m
               (in permutation)
               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		(type-variables aaa bbb)
		(sorted-meta-variables
		 (phi o  term)
		 (sym o)
		 (elem aaa  term)
		 (tl list termlist)
		 (set (o aaa)  term)))

	       (application-condition
		(not-elem-in-set-p elem set))
	       
	       (expansion-computations)
	       
               (conclusions (- l10))

               (decl-content
                (l10 () (not (in elem set))  ("open" () ()))
                ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Re-Represent-Permutation-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond perm-by-generators (args cmapp)
  (declare (edited  "24-JUL-2002")
	   (authors Pollet)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((old-perm (first args))
	 (gen-set (second args) nil)
	 (new-perm-mv (third args))
	 (new-perm (perm~gap-is-in-proof old-perm gen-set)))
    (if new-perm
	(meth~mapp-extend-subst cmapp new-perm-mv new-perm)
      (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'perm-by-generators)


(infer~defmethod "Re-Represent-with-generators-M"
		 (outline-mappings (((existent nonexistent nonexistent) Re-Represent-with-generators-m-b)))
		 (parameter-types term position)
		 (help "The method for re-representing a term with respect to a given term."))


(meth~defmethod Re-Represent-with-generators-m-b Re-Represent-with-generators-M
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (gen-set (o aa) term)
		  (phi o term)(phi-prime o term)
		  (g aa term) (g-prime aa term)
		  (position o pos)
		  )
		 )
		
		(parameters gen-set position)

		(application-condition
		 (mand (mand (bound-p gen-set)
			     (bound-p position))
		       (perm-by-generators (termatpos phi position) gen-set g-prime)))
		
		(outline-computations
		 (g (termatpos phi position))
		 (phi-prime (TermRplAtPos phi position g-prime))
		 )

		(premises (+ l10) (+ l20))
		(conclusions (- l30))

		(decl-content
		 (l10 () (= g g-prime)  ("Open" () ()))
		 (l20 () phi-prime  ("Open" () ()))
		 (l30 () phi  ("=subst" (position) (l10 l20)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods introduces a re-representation."))
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equal-With-GAP-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod Equal-With-GAP-m
		 (outline-mappings (((existent) Equal-With-GAP-m-b)))
		 (help "Showing the equality of two permutations with GAP."))


(meth~defcond equal-with-gap-p (args cmapp)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A list of two arguments (two permutations) and a mapping.")
	   (effect  "Calls GAP.")
	   (value   "<T,cmapp> if the permutations are actually equal, o/w <NIL,cmapp>"))
  (meth~mapp-new-constraint cmapp
			    (perm~gap-equal (first args) (second args))))
      
(meth~defmethod Equal-With-GAP-m-b Equal-With-GAP-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (phi (o cyc) term) (psi (o cyc) term)
		  )
		 )
		
		(application-condition
		 (equal-with-gap-p phi psi)
		 )
		 
		(conclusions (- l1))

		(decl-content
		 (l1 () (= phi psi)  ("Permutation-Equal" () ()))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods checks the equality of two given permutations with GAP."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Product-Of-Generators-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~new-relational-function 'Product-Of-Generators)

(meth~defcond Product-Of-Generators (args cmapp)
	      (declare (edited  "16-OCT-2002")
		       (authors Pollet)
		       (input   )
		       (effect  )
		       (value   ))
	      (let ((formula (car args))
		    (line (cadr args))
		    (premvar (caddr args)))
		(multiple-value-bind (concs prems hyps)
		    (product-of-generators-b (list formula) nil nil)
		  (if prems
		      (progn (meth~mapp-extend-mapp cmapp
						    premvar
						    (mapcar #'(lambda (prem)
								(pdsn~open-node-create prem (pdsn~hyps line) (pds~new-node-name omega*current-proof-plan)))
							    prems))
			     (meth~mapp-new-constraint cmapp T))
		    (meth~mapp-new-constraint cmapp NIL)))))

(infer~defmethod Product-Of-Generators-m
		 (outline-mappings (((existent nonexistent) Product-Of-Generators-m-b)))
		 (help "Showing the equality of two permutations with GAP."))


(meth~defmethod Product-Of-Generators-m-b Product-Of-Generators-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (pi (o cyc) metavar)
		  (sigma (o (o cyc)) metavar)
		  (New-Prems o prlnlist)
		  (phi o term)
		  (sub o sub)
		  )
		 )

		(premises (+ new-prems))

		(application-condition 
		 ;(test-tactic (:symbol :product-of-generators) (mlist l1 (mnil)) (mnil))
		 ;(Product-Of-Generators (:term (in pi (generated-set sigma))) l1 new-prems) ;this is SLOW!
		 (mand
		  (alpha-matcher (:term (in pi (generated-set sigma))) phi sub)
		  (Product-Of-Generators phi l1 new-prems))
		 )


		(outline-computations
		 ;(new-prems (msecond (apply-tactic (:symbol :product-of-generators) (mlist l1 (mnil)) (mnil))))
		 )
		 
		(conclusions (- l1))

		(decl-content
		 ;(l1 () (in pi (generated-set sigma))     ("Product-Of-Generators" () New-Prems))
		 (l1 () phi     ("Product-Of-Generators" () New-Prems))
		 )
		(proc-content schema-interpreter)
		(manual (documentation "This methods checks the equality of two given permutations with GAP."))
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generated-subgrp-by-subset-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This method and following deal with 'when is it possible to reduce the problems to the generators'
; A unified method would look like this:
; <A> sub(struct|set) B can be reduced to A subset B, if B is closed under perm-compose  MP

(infer~defmethod "generated-subgrp-by-subset-M"
		 (outline-mappings (((existent nonexistent) generated-subgrp-by-subset-m-b)))
		 (help "<A> subgroup <B>, it suffices to show that A is a subset of <B>."))


(meth~defmethod generated-subgrp-by-subset-m-b generated-subgrp-by-subset-M
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (A (o (o cyc)) term)
		  (B (o (o cyc)) term)
		  )
		 )
		
		(outline-computations
		 )

		(premises (+ l10))
		(conclusions (- l30))

		(decl-content
		 (l10 () (subsetp A (generated-set B))  ("Open" () ()))
		 (l30 () (subgroup (generated-set A) perm-compose (generated-set B) perm-compose)  ("Tactic"  (l10)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "<A> subgroup <B>, it suffices to show that A is a subset of <B>."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generated-subset-by-subset-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; look at comment above

(infer~defmethod "generated-subset-by-subset-M"
		 (outline-mappings (((existent nonexistent) generated-subset-by-subset-m-b)))
		 (help "<A> subset <B>, it suffices to show that A is a subset of <B>."))

(meth~defmethod generated-subset-by-subset-m-b generated-subset-by-subset-M
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (A (o (o cyc)) term)
		  (B (o (o cyc)) term)
		  (x num term)
		  )
		 )
		
		(outline-computations
		 )

		(premises (+ l10))
		(conclusions (- l30))

		(decl-content
		 (l10 () (subsetp A (stabiliser (generated-set B) perm-apply x))  ("Open" () ()))
		 (l30 () (subsetp (generated-set A) (stabiliser (generated-set B) perm-apply x))  ("Tactic"  (l10)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "<A> subgroup <B>, it suffices to show that A is a subset of <B>."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval-permutation-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond eval-permutation-application (args cmapp)
  (declare (edited  "24-JUL-2002")
	   (authors Pollet)
	   (input   "A term, a position, an unbound var for a new mv and an unbound var for the result.")
	   (effect  "-")
	   (value   "Returns true if the term contains the application of a permutation to an element"
		    "that can be evaluated, the vars will be bound to the evaluated term."))
  (let* ((term (first args))
	 (positions (peme=subterm-positions
			   (env~lookup-object 'perm-apply (pds~environment omega*current-proof-plan)) term (second args)))
	 (new (third args)))
   (if positions
       (multiple-value-bind (result position)
	   (do* ((pos positions (rest pos))
		 (res (peme=apply-perm term (pos~butlast (car pos)))
		      (peme=apply-perm term (pos~butlast (car pos)))))
	       ((or (null (rest pos)) res) (values res (car pos))))
;	 (omega~trace "~A ~A" result position)
;(reduce #'(lambda (trm pos)
;		 (or (peme=apply-perm trm (pos~butlast pos))
;	     trm))
;    positions :initial-value term)))
	 (if (and result position) ;(data~equal result term)
	   (progn
	     (meth~mapp-extend-subst
	      (meth~mapp-extend-mapp
	       cmapp (second args) (pos~butlast   position))
	      new result)
	     (meth~mapp-new-constraint cmapp t))
	     (meth~mapp-new-constraint cmapp nil)
	   ))
     (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'eval-permutation-application)

(defun peme=apply-perm (term pos)
  (let ((perm-appl (data~struct-at-position term pos)))
;    (omega~trace "apply1 ~A" perm-appl)
    (when (and (data~appl-p perm-appl)
	       (data~schema-equal (data~appl-function perm-appl)
				  (env~lookup-object 'perm-apply (pds~environment omega*current-proof-plan))))
      (let* ((args (data~appl-arguments perm-appl))
	     (perm (car args))
	     (arg (cadr args)))
;	(omega~trace "apply2 ~A~A" perm arg)
	(when (and (not (meta~p perm))(not (term~variable-p perm)) (term~number-p arg))
	  (let ((result (perm~gap-apply perm arg)))
;	    (omega~trace "apply3 ~A" result)
	    (when result
	       (data~replace-at-position term pos result))))))))

(defun peme=subterm-positions (subi term occ)
  (let ((sub (if (term~schema-p subi) (data~schema-range subi) subi)))
    (cond ((pos~p occ)
	   (when (term~alpha-equal (data~struct-at-position occ term) sub) (list occ)))
	  ((ssterm~var-p occ)
	   (data~substruct-positions sub term :test #'data~schema-equal))
	  (T nil))))

(infer~defmethod eval-permutation-m
		 (outline-mappings (((existent nonexistent) eval-permutation-m-b)))
;		 (parameter-types position)
		 (help "Eval application of permutations in terms."))

(meth~defmethod eval-permutation-m-b eval-permutation-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (phiprime o term)
		  (position o pos)))
		
	;	(parameters  position)

		(premises (+ l0))

		(application-condition
		 (eval-permutation-application phi position phiprime))
		  
		(outline-computations)
		 
		(conclusions (- l1))

		(decl-content
		 (l0 () phiprime  ("open" () ()))
		 (l1 () phi  ("eval-permutation-backward" (position) (l0)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods checks the equality of two given permutations with GAP."))
		)


#|
;;; 2. version

(meth~defcond eval-permutation-application (args cmapp)
  (declare (edited  "24-JUL-2002")
	   (authors Pollet)
	   (input   "A term, a position, an unbound var for a new mv and an unbound var for the result.")
	   (effect  "-")
	   (value   "Returns true if the term contains the application of a permutation to an element"
		    "that can be evaluated, the vars will be bound to the evaluated term."))
  (let* ((term (first args))
	 (positions (peme=subterm-positions
		    (env~lookup-object 'perm-apply (pds~environment omega*current-proof-plan)) term (second args)))
	 (new (third args)))
   (if positions
       (let ((results (mapcan #'(lambda (pos)
				  (multiple-value-bind (new-term constr)
				      (peme=apply-perm term (pos~butlast pos))
				    (cond ((and new-term constr)
					   (list (meth~mapping-create (meth~mapp-subst cmapp)
								(meth~mapp-mapp cmapp)
								(mapp~create (list new)(list new-term))
								constr)))
					  (new-term
					   (list (meth~mapping-create (meth~mapp-subst cmapp)
								      (meth~mapp-mapp cmapp)
								      (mapp~create (list new)(list new-term))
								      T)))
					  (T nil))))
			      positions)))
	     (cond ((null results) (meth~mapp-new-constraint cmapp nil))
		   ((rest results) results)
		   (T (car results))))
     (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'eval-permutation-application)

;; works only for 1 mv and primitive permutation!!
(defun peme=apply-perm (term pos)
  (let ((perm-appl (data~struct-at-position term pos)))
    (omega~trace "apply1 ~A" perm-appl)
    (when (and (data~appl-p perm-appl)
	       (data~schema-equal (data~appl-function perm-appl)
				  (env~lookup-object 'perm-apply (pds~environment omega*current-proof-plan))))
      (let* ((args (data~appl-arguments perm-appl))
	     (perm (car args))
	     (arg (cadr args)))
	(omega~trace "apply2 ~A~A" perm arg)
	(cond
;; 	 ((and (meta~p perm) (term~number-p arg) (logic~equality-p (data~struct-at-position term (pos~butlast pos))))
;; 	  (let* ((equality (data~appl-arguments (data~struct-at-position term (pos~butlast pos))))
;; 		 (value (if (term~number-p (car equality)) (car equality) (cadr equality)))
;; 		 (env (th~env 'permutation))) 
;; 	    (when (term~number-p value);;only construct this for concrete numbers
;; 	      (if (data~equal value arg)
;; 		  (values (data~replace-at-position term (pos~add-end 1 pos) (env~lookup-object 'identity-perm env))
;; 			  (cstr~binding-create (list perm (env~lookup-object 'identity-perm env))))
;; 		(let* ((mv (apply (meth~sym2function 'newmetavar)
;; 				  (list 'pi (post~read-object '(o cyc) env :existing-type))))
;; 		       (perm-compose (env~lookup-object 'perm-compose env))
;; 		       (new-perm (term~appl-create perm-compose
;; 						   (list (post~read-object (list 'set (list 'cyc (keim~name arg)(keim~name value)))
;; 									   env :existing-term)
;; 							 mv))))
;; 		  (values (data~replace-at-position term (pos~add-end 1 pos) new-perm)
;; 			  (cstr~conjunction
;; 			   (list (cstr~binding-create (list perm new-perm))
;; 				 (cstr~simple-create :subterm (list arg mv) nil nil)))))))))
	 ((and (not (meta~p perm))(not (term~variable-p perm)) (term~number-p arg))
	  (let ((result (perm~gap-apply perm arg)))
	    (omega~trace "apply3 ~A" result)
	    (when result
	      (values (data~replace-at-position term pos result)
		      T ))))
	 (T nil))))))


;;; 1. version

(newterm (reduce #'(lambda (term pos)
			      (let ((subterm (data~struct-at-position term (pos~butlast pos))))
				(when (and (data~appl-p subterm)
					   (term~number-p (cadr (data~appl-arguments subterm)))
					   (perm=perm-type-p (car (data~appl-arguments subterm)))) ;perm~permp would be good here
				  (let ((gap (perm~gap-apply (car (data~appl-arguments subterm))
							    (cadr (data~appl-arguments subterm)))))
				    (if gap (data~replace-at-position term (pos~butlast pos) gap)
				      term))))) poss :initial-value term)))

		      (find-if  #'(lambda (pos)
				    (let ((subterm (data~struct-at-position term (pos~butlast pos))))
				      (and (data~appl-p subterm)
					   (term~number-p (cadr (data~appl-arguments subterm)))
					   (perm=perm-type-p (car (data~appl-arguments subterm)))))) ;perm~perm-p would be good here??
				(data~substruct-positions (env~lookup-object 'perm-apply (pds~environment omega*current-proof-plan))
							  term :test #'data~schema-equal))))


     (meth=evaluate-disjuncts
					(mapcar #'(lambda (oc)
						    (make-instance 'meth+condition
								   :name 'mbind
								   :args (list occ
									       (make-instance 'meth+key-expression
											      :name :position
											      :args oc))))
						pds-occs)
					(if (cstr~constraint-p cmapp-cstr) 
					    (meth~mapp-new-constraint cmapp T)
					  cmapp)
					cmapp-cstr
					(meth~mapp-extension cmapp))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval-function-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;now in post-methods

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;fixpoint-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite with to a more general method!!

(infer~defmethod "fixpoint-m"
		 (outline-mappings (((existent nonexistent nonexistent) fixpoint-m-b)))
		 (help "Some kind of fixpoint criterion."))


(meth~defmethod fixpoint-m-b fixpoint-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (perms (o (o cyc)) term)
		  (set (o num) term)
		  (elem num term)
		  (action (num num (o cyc)) term)
		  )
		 )
		
		(application-condition)
		(outline-computations)

		(premises (+ l10) (+ l20))
		(conclusions (- l30))

		(decl-content
		 (l10 () (in elem set)                       ("Open" () ()))  ;;base case
		 (l20 () (forall-sort (lam (perm (o cyc))
			 (forall-sort (lam (y num) (in (action perm y) set))
				      set)) perms)           ("Open" () ()))   ;;invariance
		 (l30 () (forall-sort (lam (x num) (in x set)) (g-orbit (generated-set perms) action elem))  ("=subst" () (l10 l20)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "Some kind of fixpoint criterion."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;schreier-lemma-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite with to a more general method!!
;; But for now: since the method is explicit a situation is introduced that is suitable for
;; can be tackled by already existing methods, I hope it is also mathematically suitable


(infer~defmethod "schreier-lemma-m"
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) schreier-lemma-m-b)))
		 (help "The application of the schreier-lemma."))


(meth~defmethod schreier-lemma-m-b schreier-lemma-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (elem num term)
		  (G (o (o  cyc)))
		  (something (o (o  cyc)))
		  (action (num num (o cyc)) term)
		  (m_orb (o num) metavar)
		  (m_t ((o cyc) num) metavar)
		  )
		 )
		
		(application-condition)
		(outline-computations
		 (m_orb (newmetavar (:symbol orb) (:type (o num))))
		 (m_t (newmetavar (:symbol t) (:type ((o cyc) num))))
		 )
		      
		(outline-actions
		 (l20 (sponsor l10)))

		(premises (+ l10) (+ l20) (+ l30))
		(conclusions (- l40))

		(decl-content
		 (l10 () (= M_orb (g-orbit (generated-set G) action elem))               ("Open" () ()))
		 (l20 () (in M_t (g-orbit-representation (generated-set G) action elem)) ("Open" () ()))
		 (l30 () (forall-sort (lam (orb  num)
			 (forall-sort (lam (pi (o cyc))
					   (in 
					    (perm-compose
					     (perm-inverse (M_t (perm-apply pi orb)))
					     (perm-compose  pi (M_t orb)))
					    something))
				      G)) M_orb)		                  ("Open" () ()))
		 (l40 () (SUBSETP              
			  (STABILISER (GENERATED-SET G) action elem)                    
			  something)                                              ("Tactic" () (l10 l20 l30)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "The application of the schreier-lemma."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;decompose-stabiliserchain-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(meth~deffun decompose-list (list)
  (declare (edited  "28-DEC-2002")
	   (authors Pollet)
	   (input   "a constant LIST of type term+list")
	   (effect  )
	   (value   "A list with first element of LIST as first element and"
		    "the remaining elements of LIST as second element (if existent)"))
   (let ((elems (term~normalform list))
	 (env (th~env 'permutation)))
     (if (rest elems)
	 (list
	  (first elems)
	  (post~read-object (mapcar #'keim~name (rest elems)) env :list))
       (list (first elems)))))
	

(infer~defmethod "decompose-stabiliserchain-m"
		 (outline-mappings (((existent nonexistent nonexistent) decompose-stabiliserchain-m-b)))
		 (help "The application of the decompose-stabiliserchain."))


(meth~defmethod decompose-stabiliserchain-m-b decompose-stabiliserchain-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  ;(elems list var)             (gset (o (o  cyc)) var)              (gaction (num num (o cyc)) var)
		  (m_stab (o (o cyc)) metavar)
		  (stab (o (o  cyc)) term)           (phi o term)              (phiprime o term)
		  (subst o sub)           (position o pos))
		 )
		
		(application-condition
		 (mand (mbind gset (type-newvar (:type (o (o cyc)))))
		       (mbind gaction (type-newvar (:type (num num (o cyc)))))
		       (mbind elems (type-newvar (:type list)))
		       (match-and-bind-subterm  (:term (stabiliser-list gset gaction elems)) phi subst position)
		       (mbind m_stab (newmetavar (:symbol stab) (:type (o (o cyc)))))
		       (mbind decomp (decompose-list (subst-apply subst elems)))
		       (mbind stab   (appl-create
				      (:term stabiliser)
				      (mlist (subst-apply subst gset)
					     (subst-apply subst gaction)
					     (mfirst decomp))))
		       (mif (mequal (msecond decomp) (mnil))
			    (mbind phiprime (TermRplAtPos phi position m_stab))
			    (mbind phiprime
				   (TermRplAtPos
				    (TermRplAtPos phi (posappend position (:position (1))) m_stab)
				    (posappend position (:position (3))) (msecond decomp))))))
		 
		(outline-computations)

		      
		(premises (+ l10) (+ l20))
		(conclusions (- l40))

		(decl-content
		 (l10 () phiprime         ("Open" () ()))
		 (l20 () (= m_stab stab)  ("Open" () ()))
		 (l40 () phi              ("Open" () ()))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "Go through the elements of the stabiliser chain."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;notin-stab-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(meth~defcond first-elem-of-stabiliserbase (args cmapp)
  (declare (edited  "06-JAN-2003")
	   (authors Pollet)
	   (input   "")
	   (effect  )
	   (value   ""))
   (let* ((grp (car args))
	  (basevar (cadr args))
	  (elems (perm~gap-base-of-group grp))
	  (elem (when elems (car (term~normalform elems)))))
     (if elem
	 (meth~mapp-extend-subst cmapp basevar (car (term~normalform elems)))
       (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'first-elem-of-stabiliserbase)

(infer~defmethod "notin-stab-m"
		 (outline-mappings (((existent nonexistent nonexistent nonexistent nonexistent) notin-stab-m-b)))
		 (help "The application of the decompose-stabiliserchain."))

(meth~defmethod notin-stab-m-b notin-stab-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (m_stab (o (o cyc)) metavar) 		  (m_pi (o cyc) metavar)
		  (g (o cyc) term)  (bi num term)  (grp (o (o cyc)) term)
		  (bii num term)  (biii num term)
		  (position o pos)))
		
		
		(application-condition
		 (mand (first-elem-of-stabiliserbase grp bi)
		       (mbind biii (:term (perm-apply g bi)))
		       (eval-permutation-application biii position bii)))

		(outline-computations
		 (m_stab (newmetavar (:symbol stab) (:type (o (o cyc)))))
		 (m_pi (newmetavar (:symbol pi) (:type (o cyc)))))

		      
		(premises (+ l10) (+ l20) (+ l30) (+ l00))
		(conclusions (- l40))

		(decl-content
		 (l20 () (not (in (perm-compose g (perm-inverse M_pi)) M_stab)) ("Open" () ()))
		 (l00 () (= (perm-apply M_pi bi) bii)  ("Open" () ()))
		 (l10 () (in M_pi grp)                              ("Open" () ()))
		 (l30 () (= m_stab (stabiliser grp perm-apply bi)) ("Open" () ()))
		 (l40 () (not (in g grp))        ("Open" () (l00 l10 l20 l30)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "Go through the elements of the stabiliser chain."))
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;notin-orb-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "notin-orb-m"
		 (outline-mappings (((existent nonexistent nonexistent) notin-orb-m-b)))
		 (help "If b^g not in Orbit(M, b) then g not in M."))


(meth~defmethod notin-orb-m-b notin-orb-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (m_orb (o num) metavar) (g (o cyc) term)  (bi num term)  (grp (o (o cyc)) term)        
		 ))
		
		(application-condition
		 (first-elem-of-stabiliserbase grp bi))

		(outline-computations
		 (m_orb (newmetavar (:symbol orb) (:type (o num)))))

		      
		(premises (+ l10) (+ l20))
		(conclusions (- l40))

		(decl-content
		 (l10 () (not (in (perm-apply g bi) M_orb))          ("Open" () ()))
		 (l20 () (= M_orb (g-orbit grp perm-apply bi))       ("Open" () ()))
		 (l40 () (not (in g grp))        ("Open" () ()))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "If b^g not in Orbit(M, b) then g not in M."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;order-by-stab-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "order-by-stab-m"
		 (outline-mappings (((existent  nonexistent nonexistent nonexistent nonexistent) order-by-stab-m-b)))
		 (help "The order of a group is checked by successive application of |G|=|Orb G x|*|Stab G x|"))


(meth~defmethod order-by-stab-m-b order-by-stab-m
		(in permutation)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (m_orb (o num) metavar) (m_stab (o (o cyc)) metavar)
		  (m_num num metavar) (grp (o (o cyc)) metavar) (number num metavar)
		  (g (o cyc) term)  (bi num term)   (n num term) (genset (o (o cyc)) term)
 		  (phi o term)  (sub o sub)  
		 ))
		
		(application-condition
		 (mand (mor (alpha-matcher (:term (= (cardinality grp) number)) phi sub)
			    (alpha-matcher (:term (= number (cardinality grp))) phi sub))
		       (first-elem-of-stabiliserbase (subst-apply sub grp) bi)))
		       
		       

		(outline-computations
		 (m_orb (newmetavar (:symbol orb) (:type (o num))))
		 (m_stab (newmetavar (:symbol stab) (:type (o (o cyc)))))
		 (m_num (newmetavar (:symbol num) (:type num)))
		 (genset (subst-apply sub grp))
		 (n (subst-apply sub number)))
		      
		(premises  (+ l30) (+ l00) (+ l20) (+ l10))
		(conclusions (- l40))

		(decl-content
		 (l30 () (= (cardinality M_orb) M_num)          ("Open" () ()))
		 (l00 () (= (cardinality M_stab) (div n M_num))         ("Open" () ()))
		 (l10 () (= m_stab (stabiliser genset perm-apply bi))      ("Open" () ()))
		 (l20 () (= M_orb (g-orbit genset perm-apply bi))       ("Open" () ()))
		 (l30 () (= (cardinality M_orb) M_num)          ("Open" () ()))
		 (l40 () phi         ("Open" () ()))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "The order of a group is checked by successive application of |G|=|Orb G x|*|Stab G x|"))

		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here come some methods that are hiding proofs for subproblems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(meth~deffun plan-goal (conc premises parameters)
  (declare (edited  "10-DEC-2002")
	   (authors Vxs)
	   (input   "A conclusion, its premises and additional parameters.")
	   (effect  "Restarts the planner with on the conclusion.")
	   (value   "Undefined."))
  (let ((prems (just~premises (node~justification conc))))
    ;;;(tacl~init (cons conc premises))
    (pds~add-sponsors conc prems)
    (setf (pds~open-nodes omega*current-proof-plan)
	  (cons conc (pds~open-nodes omega*current-proof-plan)))
    (setf (pds~agenda omega*current-proof-plan)
	  (agenda~create (agenda~create-goal conc) nil nil (pds~agenda omega*current-proof-plan)))
;    (omega~trace "~A ~A" conc (node~justification conc))
    ;;;(sod~initialize! omega*current-proof-plan sod*current-strategies sod*current-strategic-control-rules nil 40)
    ;;;(setf sod*VIL nil)
    ;;;(setf pplan*VIL-on nil)
    ;; call meta-planer
    ;;;(sod~system-go)
    ;;;(tacl~end)
    ))
  
(defun meth~plan-subgoal (conc premises parameters)
  ;;;(sod=apply-instantiations)
  (let ((prems (just~premises (node~justification conc))))
    (mapc #'(lambda (nod) (when (pdsn~schematic-p nod)(setf (pdsn~up-to-date nod) nil))) (cons conc premises))
    (tacl~init (cons conc premises))
    (pds~add-sponsors conc prems)
    (setf (pds~open-nodes omega*current-proof-plan)
	  (cons conc (pds~open-nodes omega*current-proof-plan)))
    (setf (pds~agenda omega*current-proof-plan)
	  (agenda~create (agenda~create-goal conc) nil nil (pds~agenda omega*current-proof-plan)))
    (tacl~end))
  (sod~initialize! omega*current-proof-plan sod*current-strategies sod*current-strategic-control-rules nil 40)
  (sod~system-go)
  nil)
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In-Group-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod In-Group-M
		 (outline-mappings (((existent) in-group-m-b)))
		 (help "Hiding the subproof that g is a member of a Group."))

(meth~defmethod In-Group-M-B  In-Group-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (g (o cyc))
		  (Set (o (o cyc)))
		  ))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (in g (generated-set Set))    ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "<g> element of <<Set>>. The method hides the full subproof."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set-In-Group-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Set-In-Group-M
		 (outline-mappings (((existent) set-in-group-m-b)))
		 (help "Hiding the subproof that elements of a set {g1,g2,....} are members of a Group."))

(meth~defmethod Set-In-Group-M-B  Set-In-Group-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (comp (o cyc))
		  (Set1 (o (o cyc)))
		  (Set2 (o (o cyc)))
		  ))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (forall-sort
			 (lam (perm (o cyc))
			      (in comp (generated-set Set2)))
			 Set1)                                      ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "<Set1> are elements of <<Set2>>. The method hides the full subproof."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SubGroup-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod SubGroup-M
		 (outline-mappings (((existent) SubGroup-m-b)))
		 (help "Hiding the subproof that a generated permutation group is a subgroup of another."))

(meth~defmethod SubGroup-M-B  SubGroup-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (Set1 (o (o cyc)))
		  (Set2 (o (o cyc)))))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (subgroup (generated-set Set1) perm-compose
				  (generated-set Set2) perm-compose)    ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "<<Set2>> is subgroup of <<Set1>>. The method hides the full subproof."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In-Orbit-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod In-Orbit-M
		 (outline-mappings (((existent) in-orbit-m-b)))
		 (help "Hiding the subproof that n is a member of an Orbit."))

(meth~defmethod In-Orbit-M-B  In-Orbit-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (n num)  (m num)
		  (Set (o (o cyc)))
		  ))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (in n (g-orbit (generated-set Set) perm-apply m))    ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "<n> element of <<Set>_m>. The method hides the full subproof."))
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Orbit-Closed-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod Orbit-Closed-M
		 (outline-mappings (((existent) orbit-closed-m-b)))
		 (help "Hiding the subproof that an orbit is closed under one permutation."))

(meth~defmethod Orbit-Closed-M-B  Orbit-Closed-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (perm (o cyc))
		  (Set (o num))
		  ))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (forall-sort (lam (x num)
					  (in (perm-apply perm x) set))
				     set)                                ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "For all elements x of <Set> <perm>x is in <Set>. The method hides the full subproof."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In-Stabiliser-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod In-Stabiliser-M
		 (outline-mappings (((existent) in-stabiliser-m-b)))
		 (help "Hiding the subproof that g is a member of a Stabiliser."))

(meth~defmethod In-Stabiliser-M-B  In-Stabiliser-M
		(in permutation)
		(rating 1)
		(reasoning :restricting :critical)
		(non-reliability t)
		(declarations
		 (sorted-meta-variables
		  (g (o cyc))
		  (m num)
		  (Set (o (o cyc)))
		  ))
		(premises )
		(conclusions (- l1))
;		(expansion-function (meth~plan-subgoal l1 nil nil))
		(decl-content
		 (l1 () (in g (stabiliser (generated-set Set) perm-apply m))    ("Open" () () )))
		(proc-content schema-interpreter)
		(manual (documentation "<g> element of <<Set>_m>. The method hides the full subproof."))
		)

