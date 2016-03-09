;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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
(eval-when (load compile eval)
  (unless (com~find-category 'zmz)
    (com~defcategory zmz
		     (help "Tactics of the theory ZMZ."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Foralli-sort-resclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic foralli-sort-resclass
		 (outline-mappings (((existent nonexistent) foralli-sort-resclass-b)))
		 (parameter-types termsym)
	         (expansion-function zmztac=expand-foralli-sort-resclass)
		 (help "FORALL-SORT-Elimination on resclass quantifications."))


(tac~deftactic foralli-sort-resclass-b foralli-sort-resclass
	       (in zmz)
	       (parameters (X term+constant "A new constant."))
	       (conclusions C)
	       (premises P)
	       (hypotheses ((H1 P) "H1 is a hypothesis for P")
			   ;;((H2 P) "H2 is a hypothesis for P")
			   )
	       (sideconditions 
		(zmztac=forall-sort-resclass-p (formula C))
		(pds~not-free-in-nodes-or-hyps-p X C))
	       (computations 
		(P (zmztac=compute-forall-sort X (formula C)))
		(H1 (zmztac=compute-forall-sort-num-hyp X (formula C)))
		;;(H2 (zmztac=compute-forall-sort-resclass-hyp X (formula C)))
		)
	       (description "S-Universal introduction Resclass backwards"))

(defun zmztac=compute-forall-sort (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(forall-sort rc blablabla RESCLASS-SET).")
	   (effect  "None.")
	   (value   "A formula of the form 'blablabla\{rc -> (resclass c term)\}."))
  (let* ((args (data~appl-arguments formula))
	 (abstr (first args))
	 (resclass-set (second args))
	 (class-factor (zmztac=class-factor resclass-set))
	 (new-term (term~appl-create (env~lookup-object :resclass (pds~environment omega*current-proof-plan))
				     (list class-factor term))))
    (beta~normalize (term~appl-create abstr (list new-term)))))

(defun zmztac=compute-forall-sort-num-hyp (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(forall-sort rc blablabla RESCLASS-SET).")
	   (effect  "None.")
	   (value   "A formula of the form '(number-set term)' when number-set is the corresponding set of"
		    "natural numbers to RESCLASS-SET."))
  (let* ((args (data~appl-arguments formula))
	 (resclass-set (second args))
	 (number-set (zmztac=number-set-to-resclass-set resclass-set)))
    (beta~normalize                                ;;; Added beta-normalization!!!!       VS.
     (term~appl-create number-set (list term)))))

(defun zmztac=compute-forall-sort-resclass-hyp (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(forall-sort rc blablabla RESCLASS-SET) where RESCLASS-SET"
		    "represents a set of resclasses with a common factor n.")
	   (effect  "None.")
	   (value   "A formula of the form '(RESCLASS-SET (resclass n term))'."))
  (let* ((args (data~appl-arguments formula))
	 (resclass-set (second args))
	 (class-factor (zmztac=class-factor resclass-set))
	 (resclass-term (term~appl-create (env~lookup-object :resclass (pds~environment omega*current-proof-plan))
					   (list class-factor term))))
    (term~appl-create resclass-set (list resclass-term))))

(defun zmztac=forall-sort-resclass-p (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form '(forall-sort ... RESCLASS-SET)'."))
  (and
   (zmztac=forall-sort-appl-p formula)
   ;; so far we know that the formula is a forall-sort formula
   (zmztac=resclass-set-p (second (data~appl-arguments formula)))))



;;;
;;; Conclusion: Forall-sort quantified line with resclass set on n (can contain set-minus etc.)
;;; Premise:    The quantified variable replaced by expression: (resclass n c) (c is the new constant)
;;; Hyp:        Disjunction on the resclass set as single numerals (wrt. c)
;;;

(defun zmztac=expand-foralli-sort-resclass (outline parameters)
  )

(com~defcommand foralli-sort-resclass
  (argnames univ-line param line)
  (argtypes ndline termsym  ndline)
  (arghelps "Universal line to prove" "New (num) parameter" "A line" )
  (function zmztac=foralli-sort-resclass)
  (defaults zmztac=foralli-sort-resclass-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Introduce a sorted universal quantifier for a resclass."))

(defun zmztac=foralli-sort-resclass (univ-line param line )
  (infer~compute-outline 'foralli-sort-resclass (list univ-line line) (list param)))



(defun zmztac=foralli-sort-resclass-defaults (uni-line param line)
  (cond ((not (com~specified-arg-p uni-line))
	 (list (pds~find-open-node #'zmztac=forall-sort-appl-p)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p param))
	 (list uni-line
	       (or 
		(zmztac=generate-defaults-foralli-sort-resclass
		 (pds~environment omega*current-proof-plan))
		(com~unspecified))
	       (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list uni-line param  (oc~nil-argument)))
	(t (list uni-line param line))))

(defun zmztac=generate-defaults-foralli-sort-resclass (env &optional (name 'c))
  (term~generate-term-primitive-with-new-name name (env~lookup-object 'num env) 'term+constant env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Foralli-Sort-Resclass* Wild-Tactic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic foralli-sort-resclass*
		      (outline-mappings (((existent nonexistent) foralli-sort-resclass*-b)))
		      (parameter-types term-list)
		      (expansion-function batac=expand-foralli-sort-resclass*)
		      (passkey :node)
		      (help  "Iterated application of foralli-sort-resclass*."))

(defun foralli-sort-resclass*-b (concs prems parameters)
  (declare (ignore prems))
  (let ((conc (car concs))
	(terms (car parameters)))
    (when (and (zmztac=forall-sort-resclass-p (node~formula conc))
	       (every #'(lambda (x) (pds~not-free-in-nodes-or-hyps-p x conc)) terms))
      (multiple-value-bind (new-prem new-hyps)
	  (zmztac=compute-foralli-sort-resclass-recursively terms (node~formula conc))
	(when new-prem (values nil (list (cons new-prem new-hyps))))))))

(defun zmztac=compute-foralli-sort-resclass-recursively (terms formula)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "A list of terms.")
	   (effect  "None.")
	   (value   "Two values if successful:"
		    "- The formula with all forall-i-sorts elimintated by the memebers of terms."
		    "- The set of hypotheses that need to be newly introduced."
		    "If unsuccessful NIL is returned."))
  (cond ((null terms) (values formula nil))
	((and terms (not (zmztac=forall-sort-resclass-p formula))) nil)
	(t (let* ((fterm (car terms))
		  (new-formula (zmztac=compute-forall-sort fterm formula))
		  (num-hyp (zmztac=compute-forall-sort-num-hyp fterm formula))
		  ;;(resclass-hyp (zmztac=compute-forall-sort-resclass-hyp fterm formula))
		  )
	     (multiple-value-bind (prem hyps)
		 (zmztac=compute-foralli-sort-resclass-recursively (cdr terms) new-formula)
	       (when prem (values prem (cons num-hyp hyps))))))))
;; (cons resclass-hyp hyps)))))))))

(com~defcommand foralli-sort-resclass*
  (argnames univ-line param line)
  (argtypes ndline termsym-list  ndline)
  (arghelps "Universal line to prove" "A list of new (num) parameters" "A line" )
  (function zmztac=foralli-sort-resclass*)
  (defaults zmztac=foralli-sort-resclass*-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Introduce a series of sorted universal quantifier for a resclass."))

(defun zmztac=foralli-sort-resclass* (univ-line param line )
  (infer~compute-outline 'foralli-sort-resclass* (list univ-line line) (list param)))

(defun zmztac=foralli-sort-resclass*-defaults (uni-line param line)
  (cond ((not (com~specified-arg-p uni-line))
	 (list (pds~find-open-node #'zmztac=forall-sort-appl-p)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p param))
	 (list uni-line
	       (or 
		(zmztac=generate-defaults-foralli-sort-resclass*
		 (node~formula uni-line)
		 (pds~environment omega*current-proof-plan))
		(com~unspecified))
	       (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list uni-line param  (oc~nil-argument)))
	(t (list uni-line param line))))

(defun zmztac=generate-defaults-foralli-sort-resclass* (formula env)
  (do ((form formula (if (data~abstr-p (car (data~appl-arguments form)))
			 (logic~quantification-scope form)
		       form))
       (res nil))
      ((not (logic~universal-quantification-p form)) (nreverse res))
    (push (zmztac=generate-defaults-foralli-sort-resclass
	   env
	   (keim~name (logic~quantification-bound-variable form)))
	  res)))

;;; Expansion:
;;;  via foralli-sort-resclass........


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Existsi-sort-resclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic existsi-sort-resclass
		 (outline-mappings (((existent nonexistent nonexistent) existsi-sort-resclass-b)
				    ((existent nonexistent existent) existsi-sort-resclass-b1))
				   )
		 (parameter-types term position-list)
	         (expansion-function zmztac=expand-existsi-sort-resclass)
		 (help "Exists-SORT-Elimination on resclass quantifications."))

(tac~deftactic existsi-sort-resclass-b existsi-sort-resclass (in base)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P P1)
  (sideconditions
   (zmztac=exists-sort-resclass-p (formula C)))
  (computations
   (P (zmztac=compute-exists-sort X (formula C)))
   (P1 (zmztac=compute-exists-sort-num-prem X (formula C))))
  (description "S-Existential introduction backwards"))

(tac~deftactic existsi-sort-resclass-b1 existsi-sort-resclass (in base)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P P1)
  (sideconditions
   (zmztac=exists-sort-resclass-p (formula C))
   (zmztac=suitable-num-prem-p (formula P1) X (formula C)))
  (computations
   (P (zmztac=compute-exists-sort X (formula C))))
  (description "S-Existential introduction backwards"))

(defun zmztac=suitable-num-prem-p (num-prem1 term formula)
  (let* ((num-prem2 (zmztac=compute-exists-sort-num-prem term formula)))
    (term~alpha-equal num-prem1 num-prem2)))

(defun zmztac=suitable-resclass-prem-p (resclass-prem1 term formula)
  (let* ((resclass-prem2 (zmztac=compute-exists-sort-resclass-prem term formula)))
    (term~alpha-equal resclass-prem1 resclass-prem2)))

(defun zmztac=compute-exists-sort-num-prem (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(exists-sort rc blablabla RESCLASS-SET).")
	   (effect  "None.")
	   (value   "A formula of the form '(number-set term)' when number-set is the corresponding set of"
		    "natural numbers to RESCLASS-SET."))
  (let* ((args (data~appl-arguments formula))
	 (resclass-set (second args))
	 (number-set (zmztac=number-set-to-resclass-set resclass-set)))
    (beta~normalize                                ;;; Added beta-normalization!!!!       VS.
     (term~appl-create number-set (list term)))))

(defun zmztac=compute-exists-sort-resclass-prem (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(exists-sort rc blablabla RESCLASS-SET) where RESCLASS-SET"
		    "represents a set of resclasses with a common factor n.")
	   (effect  "None.")
	   (value   "A formula of the form '(RESCLASS-SET (resclass n term))'."))
  (let* ((args (data~appl-arguments formula))
	 (resclass-set (second args))
	 (class-factor (zmztac=class-factor resclass-set))
	 (resclass-term (term~appl-create (env~lookup-object :resclass (pds~environment omega*current-proof-plan))
					   (list class-factor term))))
    (term~appl-create resclass-set (list resclass-term))))

(defun zmztac=compute-exists-sort (term formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(exists-sort rc blablabla RESCLASS-SET).")
	   (effect  "None.")
	   (value   "A formula of the form 'blablabla\{rc -> (resclass c term)\}."))
  (let* ((args (data~appl-arguments formula))
	 (abstr (first args))
	 (resclass-set (second args))
	 (class-factor (zmztac=class-factor resclass-set))
	 (new-term (term~appl-create (env~lookup-object :resclass (pds~environment omega*current-proof-plan))
				     (list class-factor term))))
    (beta~normalize (term~appl-create abstr (list new-term)))))


(defun zmztac=exists-sort-resclass-p (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form '(exists-sort ... RESCLASS-SET)'."))
  (and
   (zmztac=exists-sort-appl-p formula)
   ;; so far we know that the formula is a exists-sort formula
   (zmztac=resclass-set-p (second (data~appl-arguments formula)))))


(com~defcommand existsi-sort-resclass
  (argnames exists-line witness num-prem inst-line positions)
  (argtypes ndline term ndline ndline position-list)
  (arghelps "Existential line to prove"
	    "Witness Term"
	    "Number Premisse Line" 
	    "Instantiated Line"
	    "Position List")
  (function zmztac=existsi-sort-resclass)
  (defaults zmztac=existsi-sort-resclass-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Introduce a witness term for a formula sorted existentially quantified on resclasses."))

(defun zmztac=existsi-sort-resclass (exists-line witness num-prem inst-line positions)
  (infer~compute-outline 'existsi-sort-resclass (list exists-line inst-line num-prem) (list witness positions)))

(defun zmztac=existsi-sort-resclass-defaults (exists-line witness num-prem inst-line positions)
  (cond ((not (com~specified-arg-p exists-line))
	 (list (pds~find-open-node #'zmztac=exists-sort-appl-p)
	       (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p witness))
	 (list exists-line (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p num-prem))
	 (let* ((formula (when (and witness exists-line)
			   (zmztac=compute-exists-sort-prem witness (node~formula exists-line))))
		(beta-formula (beta~normalize formula))
		(node (when formula
			(pds~find-support #'(lambda (x) (term~alpha-equal x beta-formula))))))
	   (list exists-line witness
		 (if node node (oc~nil-argument))
		 (com~unspecified) (com~unspecified))))
	((not (com~specified-arg-p inst-line))
	 (list exists-line witness num-prem (oc~nil-argument) (com~unspecified)))
	((not (com~specified-arg-p positions))
	 (list exists-line witness num-prem inst-line
	       (if (and witness inst-line)
		   (data~substruct-positions witness (node~formula inst-line))
		 (if (logic~existential-quantification-p (node~formula exists-line))
		     (data~substruct-positions (logic~quantification-bound-variable (node~formula exists-line))
					       (logic~quantification-scope (node~formula exists-line)))
		   (com~unspecified)))))
	(t (list exists-line witness num-prem inst-line positions))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existsi-Sort-Resclass* Wild-Tactic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic existsi-sort-resclass*
		      (outline-mappings (((existent nonexistent) existsi-sort-resclass*-b)))
		      (parameter-types term-list)
		      (expansion-function batac=expand-existsi-sort-resclass*)
		      (passkey :node)
		      (help  "Iterated application of existsi-sort-resclass."))


(defun existsi-sort-resclass*-b (concs prems parameters)
  (declare (ignore prems))
  (let ((conc (car concs))
	(terms (car parameters)))
    (when (zmztac=exists-sort-resclass-p (node~formula conc))
      (multiple-value-bind
	  (new-prem new-prems)
	  (zmztac=compute-existsi-sort-resclass-recursively terms (node~formula conc))
	(when new-prem
	  (values nil (mapcar #'list (cons new-prem new-prems))))))))

(defun zmztac=compute-existsi-sort-resclass-recursively (terms formula)
  (declare (edited  "01-MAR-2000")
	   (authors Sorge)
	   (input   "A list of terms.")
	   (effect  "None.")
	   (value   "Two values if successful:"
		    "- The formula with all exists-i-sorts elimintated by the members of terms."
		    "- The set of premises/new goals that need to be newly introduced."
		    "If unsuccessful NIL is returned."))
  (cond ((null terms)
	 (values formula
		 nil))
	((and terms
	      (not (zmztac=exists-sort-resclass-p formula)))
	 nil)
	(t (let* ((fterm (car terms))
		  (new-formula (zmztac=compute-exists-sort fterm formula))
		  (num-hyp (zmztac=compute-exists-sort-num-prem fterm formula))
		  ;;(resclass-hyp (zmztac=compute-forall-sort-resclass-prem fterm formula))
		  )
	     (multiple-value-bind
		 (prem hyps)
		 (zmztac=compute-existsi-sort-resclass-recursively (cdr terms) new-formula)
	       (when prem
		 (values prem (cons num-hyp hyps))))))))

(com~defcommand existsi-sort-resclass*
  (argnames exis-line param line)
  (argtypes ndline termsym-list  ndline)
  (arghelps "Existential line to prove" "A list of new (num) parameters" "Instantiated line" )
  (function zmztac=existsi-sort-resclass*)
  (defaults zmztac=existsi-sort-resclass*-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Introduce a series of sorted existential quantifier for a resclass."))

(defun zmztac=existsi-sort-resclass* (exis-line param line )
  (infer~compute-outline 'existsi-sort-resclass* (list exis-line line) (list param)))

(defun zmztac=existsi-sort-resclass*-defaults (exis-line param line)
  (cond ((not (com~specified-arg-p exis-line))
	 (list (pds~find-open-node #'zmztac=exists-sort-appl-p)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p param))
	 (list exis-line
	       (or 
		(zmztac=generate-defaults-existsi-sort-resclass*
		 (node~formula exis-line)
		 (pds~environment omega*current-proof-plan))
		(com~unspecified))
	       (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list exis-line param  (oc~nil-argument)))
	(t (list exis-line param line))))


(defun zmztac=generate-defaults-existsi-sort-resclass* (formula env)
  (do ((form formula (if (data~abstr-p (car (data~appl-arguments form)))
			 (logic~quantification-scope form)
		       form))
       (res nil))
      ((not (logic~existential-quantification-p form)) (nreverse res))
    (push (zmztac=generate-defaults-existsi-sort-resclass
	   env
	   (keim~name (logic~quantification-bound-variable form)))
	  res)))

(defun zmztac=generate-defaults-existsi-sort-resclass (env &optional (name 'c))
  (term~generate-term-primitive-with-new-name name (env~lookup-object 'num env) 'term+constant env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; composed-resclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic composed-resclass
		 (outline-mappings (((nonexistent existent existent existent) composed-resclass-f)))
		 (parameter-types term)
	         (expansion-function zmztac=expand-compose-resclass)
		 (help "A composed resclass is again a resclass."))


(tac~deftactic composed-resclass-f composed-resclass
	       (in zmz)
	       (parameters (X term+term "An operator on resclasses."))
	       (conclusions C)
	       (premises (P1 "The resclass r1 in resclass-set RS.")
			 (P2 "The resclass r2 in resclass-set RS.")
			 (P3 "Closure Theorem of operation."))
	       (sideconditions
		(zmztac=operator-is-binary-operator-on-resclasses-p X)
		(zmztac=closed-operation-theorem-p (formula P3) X)
		(zmztac=common-resclass-set-p (formula P1) (formula P2) (formula P3)))
	       (computations 
		(C (zmztac=compute-composed-resclass (formula P1) (formula P2) (formula P3) X)))
	       (description "Composed resclass computation."))



(defun zmztac=compute-composed-resclass (formula1 formula2 formula3 op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "Two formulas f1, f2 of the form '(RESCLASS-SET R1)' and '(RESCLASS-SET R2)', and"
		    "a opertor op on resclasses.")
	   (effect  "None.")
	   (value   "A formula of the form '(RESCLASS-SET (op R1 R2)'."))
  (let* ((resclass1 (first (last (data~appl-arguments formula1))))
	 (resclass2 (first (last (data~appl-arguments formula2))))
	 (resclass-set (zmztac=forall-sort-resclass-set formula3)))
    
    (term~appl-create resclass-set
		      (list (term~appl-create op (list resclass1 resclass2))))))


(defun zmztac=common-resclass-set-p (formula1 formula2 closed-theorem)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "Three formulas of the forms '(RESCLASS-SET1 R1)','(RESCLASS-SET2 R1)', and"
		    "(see zmztac=closed-operation-theorem-p with respect to a RESCLASS-SET3),"
		    "respectively.")
	   (effect  "None.")
	   (value   "T if RESCLASS-SET1 = RESCLASS-SET2 = RESCLASS-SET3."))
  (when (and (data~appl-p formula1)
	   (data~appl-p formula2))
      (let* ((resclass-set3 (zmztac=forall-sort-resclass-set closed-theorem))
	     (resclass-var (term~variable-create 'rcvar1 (post~read-object '(o num)
									    (pds~environment omega*current-proof-plan)
									    :existing-type)))
	     (match-term (beta~normalize (term~appl-create resclass-set3 (list resclass-var))))
	     (match1 (bind~with-bindings ((keim::term=alpha-equal match-term (beta~normalize formula1) (list resclass-var)))))
	     (match2 (bind~with-bindings ((keim::term=alpha-equal match-term (beta~normalize formula2) (list resclass-var))))))
	(when (and match1 match2)
	    't))))

(defun zmztac=closed-operation-theorem-p (formula op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula f and a operator on resclasses op.")
	   (effect  "None.")
	   (value   "T if f represent a theorem stating the that operation op is closed under a"
		    "resclass-set, this means that f has to be of one of the following forms:"
		    "1.) forall-sort r1:RS,r2:RS. (RS (op r1 r2)) where RS is a resclass-set."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (when (zmztac=forall-sort-appl-p formula)
	(let* ((args1 (data~appl-arguments formula))
	       (formula1 (data~abstr-range (first args1)))
	       (var1 (first (data~abstr-domain (first args1))))
	       (resclass-set1 (second args1)))
	  (when (and (zmztac=resclass-set-p resclass-set1)
		      (zmztac=forall-sort-appl-p formula1))
	      (let* ((args2 (data~appl-arguments formula1))
		     (formula2 (data~abstr-range (first args2)))
		     (var2 (first (data~abstr-domain (first args2))))
		     (resclass-set2 (second args2)))
		(when (and (zmztac=resclass-set-p resclass-set1)
			 (term~alpha-equal resclass-set1 resclass-set2))
		    (let* ((equal-term (beta~normalize
					(term~appl-create resclass-set2
							  (list (term~appl-create op
										  (list var1 var2)))))))
		      (when (term~alpha-equal equal-term (beta~normalize formula2))
			  't)))))))))

(defun zmztac=forall-sort-resclass-set (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula f.")
	   (effect  "None.")
	   (value   "If f has the form '(forall-sort r1 ... RS) and RS is a resclass-set, then RS is returned,"
		    "otherwise nil."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (when (zmztac=forall-sort-appl-p formula)
	(let* ((args1 (data~appl-arguments formula))
	       (formula1 (data~abstr-range (first args1)))
	       (var1 (data~abstr-domain (first args1)))
	       (resclass-set1 (second args1)))
	  (when (zmztac=resclass-set-p resclass-set1)
	      resclass-set1)))))


(com~defcommand composed-resclass
  (argnames resclass1 resclass2 operator closed-theorem composed-resclass)
  (argtypes ndline ndline term ndline ndline)
  (arghelps "Resclass-line1"
	    "Resclass-line2"
	    "The operation"
	    "Line with a closed theorem to operation"
	    "Comosed resclass line")
  (function zmztac=composed-resclass)
  (defaults ((com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
  (frag-cats tactics base) 
  (log-p T)
  (help "Compute a composed resclass."))

(defun zmztac=composed-resclass (resclass1 resclass2 operator closed-theorem composed-resclass)
  (infer~compute-outline 'composed-resclass (list composed-resclass resclass1 resclass2 closed-theorem) (list operator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convert-Resclass-Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic convert-resclass-equality
		 (outline-mappings (((existent nonexistent) convert-resclass-equality-b)))
		 (parameter-types position)
		 (expansion-function zmztac=expand-convert-resclass-equality)
		 (help "Converts the equality between resclasses into an equality between their class-residues."))

(tac~deftactic convert-resclass-equality-b convert-resclass-equality
	       (in zmz)
	       (parameters (Pos pos+position "A position of an equation subterm in C."))
	       (conclusions C)
	       (premises (P1 "The equation at position Pos replaced bey an equation between the class-residues."))
	       (sideconditions
		(zmztac=resclass-equation-at-pos-p (formula C) pos))
	       (computations 
		(P1 (zmztac=compute-corresponding-class-residue-equation-at-pos (formula C) pos)))
	       (description "Converts a goal-equation at a position between resclasses into a equation on the class-residues."))

(com~defcommand convert-resclass-equality
  (argnames resclass-equation-line position class-residue-equation-line)
  (argtypes ndline position ndline)
  (arghelps "Open line with subformula that is equation on resclasses."
	    "Position of subterm that is equation on resclasses."
	    "Corresponding line with equation on class-residues as subterm.")
  (function zmztac=convert-resclass-equality)
  (defaults zmztac=convert-resclass-equality-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Converts a equaltiy over resclasses into an equality over class-residues."))

(defun zmztac=convert-resclass-equality (resclass-equation-line position class-residue-equation-line)
  (infer~compute-outline 'convert-resclass-equality
			 (list resclass-equation-line class-residue-equation-line)
			 (list position)))

(defun zmztac=convert-resclass-equality-defaults (resclass-equation-line position class-residue-equation-line)
  (cond ((not (com~specified-arg-p resclass-equation-line))
	 (list (pds~find-open-node #'(lambda (x)
				       (data~positions x #'zmztac=resclass-equality-p )))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position))
	 (list resclass-equation-line
	       (first (when (node~p resclass-equation-line)
			(data~positions (node~formula resclass-equation-line)
					#'zmztac=resclass-equality-p)))
	       (com~unspecified)))
	((not (com~specified-arg-p class-residue-equation-line))
	 (list resclass-equation-line position (oc~nil-argument)))
	(t
	 (list resclass-equation-line position class-residue-equation-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convert-Resclass-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic convert-resclass-in
		 (outline-mappings (((existent nonexistent) convert-resclass-in-b)))
		 (parameter-types position)
		 (expansion-function zmztac=expand-convert-resclass-in)
		 (help "Converts a subformula '(Resclass-Set resclass)' at position into a formula '(NUMBER-SET (class-residue resclass)' at position"))


(tac~deftactic convert-resclass-in-b convert-resclass-in
	       (in zmz)
	       (parameters (Pos pos+position "A position of an resclass in resclass-set subterm in C."))
	       (conclusions C)
	       (premises P)
	       (sideconditions
		(zmztac=resclass-in-resclass-set-at-pos-p (formula C) pos))
	       (computations 
		(P (zmztac=compute-class-residue-in-number-set-at-pos (formula C) pos)))
	       (description "Converts a goal with subformula '(Resclass-Set resclass)' at position into a formula '(NUMBER-SET (class-residue resclass)' at position"))

(com~defcommand convert-resclass-in
  (argnames resclass-inresclass-set-line position class-residue-in-number-set-line)
  (argtypes ndline position ndline)
  (arghelps "Line with formula RESCLASS in RESCLASS-SET as subterm."
	    "Position of subformula RESCLASS in RESCLASS-SET."
	    "Corresponding line with (CLASS-RESIDUE RESCLASS) in NUMBER-SET at position.")
  (function zmztac=convert-resclass-in)
  (defaults zmztac=convert-resclass-in-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Converts a goal with formula '(Resclass-Set resclass)' into a formula '(NUMBER-SET (class-residue resclass)'"))

(defun zmztac=convert-resclass-in (resclass-inresclass-set-line position class-residue-in-number-set-line)
  (infer~compute-outline 'convert-resclass-in
			 (list resclass-inresclass-set-line class-residue-in-number-set-line)
			 (list position)))

(defun zmztac=convert-resclass-in-defaults (resclass-inresclass-set-line position class-residue-in-number-set-line)
  (cond ((not (com~specified-arg-p resclass-inresclass-set-line))
	 (list (pds~find-open-node #'(lambda (y)
				       (data~positions y #'(lambda (formula) 
							     (zmztac=resclass-in-resclass-set-p formula)))))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position))
	 (list resclass-inresclass-set-line
	       (first (data~positions (node~formula resclass-inresclass-set-line)
				      #'zmztac=resclass-in-resclass-set-p))
	       (com~unspecified)))
	((not (com~specified-arg-p class-residue-in-number-set-line))
	 (list resclass-inresclass-set-line position (oc~nil-argument)))
	(t
	 (list resclass-inresclass-set-line position class-residue-in-number-set-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Push-class-residue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic push-class-residue
		 ;;(outline-mappings (((existent nonexistent existent existent) push-class-residue-b)))
		 (outline-mappings (((existent nonexistent) push-class-residue-b)))
		 (parameter-types position term)
	         (expansion-function zmztac=expand-push-class-residue)
		 (help "Pushs class-residue at position through the term operation."))

(tac~deftactic push-class-residue-b push-class-residue
	       (in zmz)
	       (parameters (Pos pos+position "A position of a class-residue in C.")
			   (Op term+term "An operation on resclasses."))
	       (conclusions C)
	       (premises (P1 "The pushed line.")
			 ;;(P2 "The resclass r1 in resclass-set RS.")
			 ;;(P3 "The resclass r2 in resclass-set RS.")
			 )
	       (sideconditions
		;;(zmztac=resclass-in-resclass-set-p (formula P2))
		;;(zmztac=resclass-in-resclass-set-p (formula P3))
		;;(zmztac=resclasses-of-same-resclass-set-p (formula P2) (formula P3))
		(zmztac=convertable-operator-on-resclasses-p Op)
		;;(zmztac=class-residue-operator-and-args-at-position-p (formula C) (formula P2) (formula P3) Pos Op)
		(zmztac=class-residue-operator-and-args-at-position-p (formula C) Pos Op)
		)
	       (computations 
		;;(P1 (zmztac=compute-pushed-line (formula C) (formula P2) Pos Op))
		(P1 (zmztac=compute-pushed-line (formula C) Pos Op))
		)
	       (description "Composed resclass computation."))

(com~defcommand push-class-residue
		(argnames line-with-class-residue position operation pushed-line)
		(argtypes ndline position term ndline)
		(arghelps "Line with unpushed class-residue."
			  "Position of Subterm with class-residue as function."
			  "Operation in top of the argument of the class-residue term."
			  "Corresponding line with pushed class-residue.")
		(function zmztac=push-class-residue)
		(defaults zmztac=push-class-residue-defaults)
		(frag-cats tactics base) 
		(log-p T)
		(help "Pushs a class-residue."))

(defun zmztac=push-class-residue (line-with-class-residue position operation pushed-line)
  (infer~compute-outline 'push-class-residue
			 (list line-with-class-residue pushed-line)
			 (list position operation)))

(defun zmztac=push-class-residue-defaults (line-with-class-residue position operation pushed-line)
  (cond ((not (com~specified-arg-p line-with-class-residue))
	 (list (pds~find-open-node #'(lambda (x)
				       (data~positions x #'zmztac=class-resdue-resclass-appl-p)))
	       (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position))
	 (let* ((form (node~formula line-with-class-residue))
		(class-residue-position (first (data~positions form #'zmztac=class-resdue-resclass-appl-p))))	   
	   (list line-with-class-residue
		 (if class-residue-position
		     class-residue-position
		   (com~unspecified))
		 (com~unspecified) (com~unspecified))))
	((not (com~specified-arg-p operation))
	 (let* ((substruct-at-pos (data~struct-at-position (node~formula line-with-class-residue) position))
		(op (if (and (data~appl-p substruct-at-pos)
			     (data~appl-p (first (data~appl-arguments substruct-at-pos))))
			(data~appl-function (first (data~appl-arguments substruct-at-pos)))
		      nil)))
	   (list line-with-class-residue
		 position
		 (if op
		     op
		   (com~unspecified))
		 (com~unspecified))))
	((not (com~specified-arg-p pushed-line))
	 (list line-with-class-residue position operation (oc~nil-argument)))
	(t
	 (list line-with-class-residue position operation pushed-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Push-class-residue*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic push-class-residue*
		 (outline-mappings (((existent nonexistent) push-class-residue*-b)))
		 (parameter-types position)
	         (expansion-function zmztac=expand-push-class-residue*)
		 (help "Pushs class-residue at position through succesively as long as there are composed terms."))


(tac~deftactic push-class-residue*-b push-class-residue*
	       (in zmz)
	       (parameters (Pos pos+position "A position of a class-residue in C."))
	       (conclusions C)
	       (premises (P1 "The pushed line."))
	       (sideconditions
		(zmztac=class-residue-at-position-p (formula C) Pos))
	       (computations 
		(P1 (zmztac=compute-pushed*-line (formula C) Pos)))
	       (description "Pushs class-residue through terms."))

(defun zmztac=compute-pushed*-line (formula pos)
  (do* ((current-pos-list (list pos))
	(current-formula formula))
      ((null current-pos-list)
       current-formula)
    (let* ((head-pos (first current-pos-list))
	   (struct-at-pos (data~struct-at-position current-formula head-pos)))

      (if (and (zmztac=class-residue-appl-p struct-at-pos)
	       (= (length (data~appl-arguments struct-at-pos)) 2))
	  (let* ((op-arg (first (data~appl-arguments struct-at-pos))))
	    (if (and (data~appl-p op-arg)
		     (= (length (data~appl-arguments op-arg)) 2)
		     (zmztac=convertable-operator-on-resclasses-p (data~appl-function op-arg)))
		(let* ((op (data~appl-function op-arg))
		       (resclass-args (data~appl-arguments op-arg))
		       (resclass1 (first resclass-args))
		       (resclass2 (second resclass-args))
		       (class-factor (zmztac=class-factor-of-resclass resclass1))
		       (class-residue (env~lookup-object 'class-residue (pds~environment omega*current-proof-plan)))
		       (converted-operation (zmztac=convert-resclass-operation-to-num-operation op class-factor))
		       (new-subterm (term~appl-create converted-operation
						      (list (term~appl-create class-residue
									      (list resclass1 class-factor))
							    (term~appl-create class-residue
									      (list resclass2 class-factor))))))
		  (setf current-formula (data~replace-at-position current-formula head-pos new-subterm))
		  (setf current-pos-list (append (rest current-pos-list) (list (pos~add-end 1 head-pos)
									       (pos~add-end 2 head-pos)))))
	      (setf current-pos-list (rest current-pos-list))))
	(setf current-pos-list (rest current-pos-list))))))


(com~defcommand push-class-residue*
		(argnames line-with-class-residue position pushed-line)
		(argtypes ndline position ndline)
		(arghelps "Line with unpushed class-residue."
			  "Position of Subterm with class-residue as function."
			  "Corresponding line with pushed class-residue.")
		(function zmztac=push-class-residue*)
		(defaults zmztac=push-class-residue*-defaults)
		(frag-cats tactics base) 
		(log-p T)
		(help "Pushs a class-residue."))

(defun zmztac=push-class-residue* (line-with-class-residue position pushed-line)
  (infer~compute-outline 'push-class-residue*
			 (list line-with-class-residue pushed-line)
			 (list position)))


(defun zmztac=push-class-residue*-defaults (line-with-class-residue position pushed-line)
  (cond ((not (com~specified-arg-p line-with-class-residue))
	 (list (pds~find-open-node #'(lambda (x)
				       (data~positions x #'zmztac=class-resdue-resclass-appl-p)))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position))
	 (let* ((form (node~formula line-with-class-residue))
		(class-residue-position (first (data~positions form #'zmztac=class-resdue-resclass-appl-p))))
	   (list line-with-class-residue
		 (if class-residue-position
		     class-residue-position
		   (com~unspecified))
		 (com~unspecified))))
	((not (com~specified-arg-p pushed-line))
	 (list line-with-class-residue position (oc~nil-argument)))
	(t
	 (list line-with-class-residue position pushed-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Push-class-residue**
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic push-class-residue**
		 (outline-mappings (((existent nonexistent) push-class-residue**-b)))
	         (expansion-function zmztac=expand-push-class-residue**)
		 (help "Pushs all class-residues in formula through all possible terms."))

(tac~deftactic push-class-residue**-b push-class-residue**
	       (in zmz)
	       (conclusions C)
	       (premises (P1 "The pushed line."))
	       (sideconditions
		(zmztac=pushable-class-residue-positions-p (formula C)))
	       (computations 
		(P1 (zmztac=compute-pushed**-line (formula C))))
	       (description "Pushs all class-residue through terms."))

(defun zmztac=compute-pushed**-line (formula)
  (declare (edited  "03-MAR-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "The formula resulting by pushing in the input formula each occurence of class-residue"
		    "through all posiible operations."))
  (let* ((positions (reverse (zmztac=pushable-class-residue-positions-p formula))))
    (do* ((rest-positions positions (rest rest-positions))
	  (current-formula formula))
	((null rest-positions)
	 current-formula)
      (let* ((head-position (first rest-positions)))
	(setf current-formula (zmztac=compute-pushed*-line current-formula head-position))))))

(com~defcommand push-class-residue**
		(argnames line-with-class-residue pushed-line)
		(argtypes ndline ndline)
		(arghelps "Line with unpushed class-residue."
			  "Corresponding line with pushed class-residue.")
		(function zmztac=push-class-residue**)
		(defaults zmztac=push-class-residue**-defaults)
		(frag-cats tactics base) 
		(log-p T)
		(help "Pushs a class-residue."))

(defun zmztac=push-class-residue** (line-with-class-residue pushed-line)
  (infer~compute-outline 'push-class-residue**
			 (list line-with-class-residue pushed-line) nil))

(defun zmztac=push-class-residue**-defaults (line-with-class-residue pushed-line)
  (cond ((not (com~specified-arg-p line-with-class-residue))
	 (list (pds~find-open-node #'(lambda (x)
				       (data~positions x #'zmztac=class-resdue-resclass-appl-p)))
	       (com~unspecified)))
	((not (com~specified-arg-p pushed-line))
	 (list line-with-class-residue (oc~nil-argument)))
	(t
	 (list line-with-class-residue pushed-line))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Replace-classrest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic replace-class-residues
		 (outline-mappings (((existent nonexistent) replace-class-residues-b)))
		 (parameter-types position-list)
		 (expansion-function zmztac=expand-replace-class-residues)
		 (help "Removes the class-residue part of a term."))

(tac~deftactic replace-class-residues-b replace-class-residues
	       (in zmz)
	       (parameters (PList list "A list of positions of a class-residue term in C."))
	       (conclusions C)
	       (premises P)
	       (sideconditions 
		(zmztac=replace-class-residues-terms-at-positions-list-p (formula C) Plist))
	       (computations 
		(P (zmztac=compute-replaced-class-residues-term (formula C) Plist)))
	       (description "Removes classrest terms."))

(defun zmztac=compute-replaced-class-residues-term (formula pos-list)
  (declare (edited  "28-FEB-2000")
	   (authors Ameier)
	   (input   "A formula and a list of position such that the formula has at each position a subterm"
		    "of the form '(class-residue (resclass N C) N)'.")
	   (effect  "None.")
	   (value   "A formula that has at each position a corresponding term 'C'."))
  (do* ((rest-positions (reverse pos-list) (rest rest-positions))
	(current-formula formula))
      ((null rest-positions)
       current-formula)
    (let* ((pos (first rest-positions))
	   (struct-at-pos (data~struct-at-position current-formula pos))
	   (c (second (data~appl-arguments (first (data~appl-arguments struct-at-pos))))))
      
      (setf current-formula (data~replace-at-position current-formula pos c)))))
  
										  
(com~defcommand replace-class-residues
		(argnames line-with-class-residues position-list replaced-line)
		(argtypes ndline position-list ndline)
		(arghelps "Line with class-residues."
			  "List of positions of subterms with class-residue as function."
			  "Corresponding line with replaced class-residues.")
		(function zmztac=replace-class-residues)
		(defaults zmztac=replace-class-residues-defaults)
		(frag-cats tactics base) 
		(log-p T)
		(help "Removes a class-residues."))

(defun zmztac=replace-class-residues (line-with-class-residues position-list replaced-line)
  (infer~compute-outline 'replace-class-residues
			 (list line-with-class-residues replaced-line)
			 (list position-list)))

(defun zmztac=replace-class-residues-defaults (line-with-class-residues position-list replaced-line)
  (cond ((not (com~specified-arg-p line-with-class-residues))
	 (list (pds~find-open-node #'(lambda (x)
				       (data~positions x #'zmztac=replaceable-class-residue-p)))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p position-list))
	 (let* ((form (node~formula line-with-class-residues))
		(class-residue-position-list (data~positions form #'zmztac=replaceable-class-residue-p)))
	   (list line-with-class-residues
		 (if class-residue-position-list
		     class-residue-position-list
		   (com~unspecified))
		 (com~unspecified))))
	((not (com~specified-arg-p replaced-line))
	 (list line-with-class-residues position-list (oc~nil-argument)))
	(t
	 (list line-with-class-residues position-list replaced-line))))


	

