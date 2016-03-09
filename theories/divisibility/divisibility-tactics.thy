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
(eval-when (load compile eval)
  (unless (com~find-category 'typed-set)
    (com~defcategory typed-set
		     (help "Tactics of the divisibility theory."))))
                     
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; divisibility-foralli-sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic tintin-foralli-sort
		 (outline-mappings (((existent nonexistent) tintin-foralli-sort-b)
				    ((existent existent) tintin-foralli-sort-a)))
		 (parameter-types termsym)
	         (expansion-function batac=expand-tintin-foralli-sort)
		 (help "FORALL-SORT-Elimination."))

(tac~deftactic tintin-foralli-sort-b tintin-foralli-sort (in base)
  (parameters (X term+constant "A new constant."))
  (conclusions C)
  (premises P)
  (hypotheses ((H P) "H is a hypothesis for P"))
  (sideconditions 
   (batac=forall-sort-p (formula C))
   (pds~not-free-in-nodes-or-hyps-p X C))
  (computations 
   (P (batac=compute-forall-sort X (formula C)))
   (H (batac=compute-forall-sort-hyp X (formula C))))
  (description "S-Universal introduction backwards"))

(tac~deftactic tintin-foralli-sort-a tintin-foralli-sort (in base)
  (parameters (X term+constant "A new constant."))
  (conclusions C)
  (premises P)
  (hypotheses ((H P) "H is a hypothesis for P"))
  (sideconditions 
   (batac=forall-sort-p (formula C))
   (batac=tintin-foralli-sort-applicable-p (formula C) (formula P)  X (hyps P))
   (pds~not-free-in-nodes-or-hyps-p X C))
  (description "S-Universal introduction application"))


(defun batac=tintin-foralli-sort-applicable-p (con pre term hyp-list)
  (let ((hyp (batac=compute-forall-sort-hyp term con)))
    (and (term~alpha-equal pre (batac=compute-forall-sort term con))
	 (some #'(lambda (h) (term~alpha-equal hyp (node~formula h))) hyp-list))))
  

(defun batac=compute-forall-sort (term formula)
  (let* ((args (data~appl-arguments formula))
	 (abstr (first args)))
    (beta~normalize (term~appl-create abstr term))))

(defun batac=compute-forall-sort-hyp (term formula)
  (let* ((args (data~appl-arguments formula))
	 (sort (second args)))
     (term~appl-create sort term)))
    
(defun batac=expand-tintin-foralli-sort (outline parameters)
  (let* ((fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive)))
         (conc (car outline))
	 (prem (cadr outline))
	 (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc)))))
    (tacl~init outline)
    (let ((result 
	   (tacl~sequence
	    (defni-res ('defni (list conc nil) (list definiendum definiens (pos~add-front 0))))
	    (foralli-res ('foralli (list (cadr defni-res) nil) parameters))
	    (impi-res ('impi (list (cadr foralli-res) nil) nil)))))     
      (tac~forget&destroy-hyp (list (second result)) old-hyp (third result))
     (tacl~apply  'weaken (list (second result) prem) nil))
    (tacl~end)))

(com~defcommand tintin-foralli-sort
  (argnames univ-line param line)
  (argtypes ndline termsym  ndline)
  (arghelps "Universal line to prove" "New parameter" "A line" )
  (function batac=tintin-foralli-sort)
  (defaults batac=tintin-foralli-sort-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Introduce a sorted universal quantifier."))

(defun batac=tintin-foralli-sort (univ-line param line )
  (infer~compute-outline 'tintin-foralli-sort (list univ-line line) (list param)))


(defun batac=tintin-foralli-sort-defaults (uni-line param)
    (cond ((not (com~specified-arg-p uni-line))
           (list (pds~find-open-node #'(lambda (x) (data~equal
                                                    (data~appl-function x)
                                                    (env~lookup-object :forall-sort
                                                                       (pds~environment omega*current-proof-plan)))))
             (com~unspecified) (com~unspecified)))
          ((not (com~specified-arg-p param))
           (list uni-line
                 (or 
                  (batac=generate-defaults-tintin-foralli-sort
                         uni-line
                         (pds~environment omega*current-proof-plan))
                  (com~unspecified))
                 (com~unspecified)))
          ((not (com~specified-arg-p line))
           (list uni-line param  (oc~nil-argument)))
          (t (list uni-line param line))))

(defun batac=generate-defaults-tintin-foralli-sort (line env)  
  (when (node~p line)
    (let ((form (node~formula line)))
      (when (data~equal
             (data~appl-function form)
             (env~lookup-object :forall-sort
                                (pds~environment omega*current-proof-plan)))
        (let ((var (logic~quantification-bound-variable form)))
          (term~generate-term-primitive-with-new-name 
           (keim~name var) (term~type var) 'term+constant env))))))





;;; foralli-sort*  DOES NOT WORK

;(infer~deftactic foralli-sort*
;		 (outline-mappings (((existent nonexistent) foralli-sort*-b)))
;		 (parameter-types termsym-list)
;		 (expansion-function titac=expand-foralli-sort*)
;		 (help "Iterated FORALL-sort-Introduction."))
;
;(tac~deftactic foralli-sort*-b foralli-sort* (in divisibility)
;   (parameters (SL cons "A list of new constants."))
;   (premises P)
;   (conclusions C)
;   (hypotheses ((H P) "H is a hypothesis for P"))
;   (computations (P (titac=compute-iterated-foralli (formula C) SL)))
;   (sideconditions 
;    (batac=forall-sort-p (formula C))
;    (pds~not-free-in-nodes-or-hyps-p X C))
;    (description "Backward application of iterated FORALL-sort-Introduction."))
;
;(defun titac=compute-iterated-foralli (formula const-list)
;  (let ((new-term formula))
;    (dolist (const const-list)
;      (setq new-term (beta~contract
;                      (term~appl-create (car (data~appl-arguments new-term))
;                                   (list const)))))
;    new-term))
;
;(defun titac=expand-foralli-sort* (outline parameters)
;  (let* ((forall-line (first outline))
;	 (const-list (first parameters))
;	 (last-const (first (last const-list))))
;    (tacl~init outline)
;    (when (rest const-list)
;      (do ((rest-consts const-list (rest rest-consts)))
;	  ((null (rest rest-consts)) t)
;	(let* ((head-const (first rest-consts)))
;	  (setq forall-line 
;		(second (tacl~apply 'foralli (list forall-line nil) (list head-const)))))))
;    (tacl~apply 'foralli (list forall-line (second outline)) (list last-const))
;    (tacl~end)))
;  
;(com~defcommand foralli-sort*
;  (argnames univline line newconsts)
;  (argtypes ndline ndline termsym-list)
;  (arghelps "A universally quanitified srt line" "A line."  "A list of new constants wrt. the first line.")
;  (function titac=foralli-sort*)
;  (frag-cats tactics base)
;  (defaults orules=foralli-sort*-defaults)
;  (log-p T)
;  (help "Apply a series of Forall-sort-eliminations."))
;
;(defun titac=foralli-sort* (C P consts)
;  (infer~compute-outline 'foralli-sort* (list C P) (list consts)))
;
;(defun orules=foralli-sort*-defaults (planline line consts)
;    (cond ((not (com~specified-arg-p planline))
;	   (list (pds~find-open-node #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
;	  ((not (com~specified-arg-p line))
;	   (list planline nil (com~unspecified)))
;	  ((not (com~specified-arg-p consts))
;	   (list planline line
;		 (orules=generate-defaults-foralli planline
;						   (pds~environment omega*current-proof-plan))))
;	  (t (list planline line consts))))
;
;(defun orules=generate-defaults-foralli (line env)
;  (let ((form (node~formula line)))
;    (do ((form form (if (data~abstr-p (car (data~appl-arguments
;				       form)))
;			(logic~quantification-scope form)
;		      form))
;	 (res nil))
;	((not (logic~universal-quantification-p form))
;	 (nreverse res))
;      (let ((var (if (data~abstr-p (car (data~appl-arguments
;				    form)))
;		     (logic~quantification-bound-variable form))))
;	(if var
;	    (let ((var-name (concatenate 'string (string (keim~name var))
;					 (princ-to-string 1))))
;	      (if (env~lookup-object var-name env)
;		  (let ((n 2))
;		    (loop
;		     (setq var-name (concatenate 'string (string (keim~name var))
;						 (princ-to-string n)))
;		     (when (not (env~lookup-object var-name env))
;		       (return))
;		     (incf n))))
;	      (term~generate-term-primitive-with-new-name
;	       (keim~name var)
;	       (term~type var)
;	       'term+constant
;	       env)
;	      (push (env~lookup-object var-name env) res))
;	  (setq form nil)))))
;  )





(infer~deftactic eq-mult
		 (outline-mappings (((existent nonexistent nonexistent) eq-mult-b)))
		 (parameter-types termsym)
	         (expansion-function titac=expand-eq-mult)
		 (help "Multiply an open equation with some constant."))

(tac~deftactic eq-mult-b eq-mult (in base)
  (parameters (X term+term "A term."))
  (conclusions C)
  (premises P1 P2)
  (sideconditions 
   (titac=check-equal (formula C))
  )
  (computations 
   (P1 (titac=compute-multiplication X (formula C)))
   (P2 (titac=compute-condition X )))
  (description "Multiply an open equation with some constant."))

(defun titac=check-equal (line)
  (if (data~appl-p line)
      (let* ((head (data~appl-function line))
	     (args (data~appl-arguments line))
	     (lhs (first args))
	     (Rhs (second args)))
	(and (data~equal head (env~lookup-object := (pds~environment omega*current-proof-plan)))
	     (and (data~equal (term~type lhs) (env~lookup-object :num (pds~environment omega*current-proof-plan)))
		  (data~equal (term~type rhs) (env~lookup-object :num (pds~environment omega*current-proof-plan))))))
      nil))

(defun titac=compute-condition (term)
  (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
		    (list (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
					    (list term
						  (post~read-object 0 (pds~environment omega*current-proof-plan) :existing-term))))))
(defun titac=compute-multiplication (param line)
  (let* ((head (data~appl-function line))
	 (args (data~appl-arguments line))
	 (lhs (first args))
	 (Rhs (second args))
	 (mult  (env~lookup-object :times (pds~environment omega*current-proof-plan))))
    (term~appl-create head
		      (list (term~appl-create mult (list lhs param))
			    (term~appl-create mult (list rhs param))))))
			     
(defun titac=expand-eq-mult (outline parameters)
  (let* ((fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive)))
         (conc (car outline))
	 (prem (cadr outline))
	 (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc)))))
    (tacl~init outline)
    (let ((result 
	   (tacl~sequence
	    (defni-res ('defni (list conc nil) (list definiendum definiens (pos~add-front 0))))
	    (foralli-res ('foralli (list (cadr defni-res) nil) parameters))
	    (impi-res ('impi (list (cadr foralli-res) nil) nil)))))     
      (tac~forget&destroy-hyp (list (second result)) old-hyp (third result))
     (tacl~apply  'weaken (list (second result) prem) nil))
    (tacl~end)))

(com~defcommand eq-mult
  (argnames openline factor)
  (argtypes ndline termsym)
  (arghelps "Equation to prove" "Multiplication factor")
  (function titac=eq-mult)
  (defaults titac=eq-mult-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Multiply an open equation with some term."))

(defun titac=eq-mult (line param)
  (infer~compute-outline 'eq-mult (list line nil nil) (list param)))


(defun titac=eq-mult-defaults (line param ppp)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified)))
	(t (list line param))))





(infer~deftactic eq-add
		 (outline-mappings (((existent nonexistent) eq-add-b)))
		 (parameter-types termsym)
	         (expansion-function titac=expand-eq-add)
		 (help "Add some term to an open equation."))

(tac~deftactic eq-add-b eq-add (in base)
  (parameters (X term+term "A term."))
  (conclusions C)
  (premises P)
  (sideconditions 
   (titac=check-equal (formula C))
  )
  (computations 
   (P (titac=compute-addition X (formula C))))
  (description "Add some term to an open equation."))

(defun titac=compute-addition (param line)
  (let* ((head (data~appl-function line))
	 (args (data~appl-arguments line))
	 (lhs (first args))
	 (Rhs (second args))
	 (add  (env~lookup-object :plus (pds~environment omega*current-proof-plan))))
    (term~appl-create head
		      (list (term~appl-create add (list lhs param))
			    (term~appl-create add (list rhs param))))))
			     
(defun titac=expand-eq-add (outline parameters)
  (let* ((fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive)))
         (conc (car outline))
	 (prem (cadr outline))
	 (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc)))))
    (tacl~init outline)
    (let ((result 
	   (tacl~sequence
	    (defni-res ('defni (list conc nil) (list definiendum definiens (pos~add-front 0))))
	    (foralli-res ('foralli (list (cadr defni-res) nil) parameters))
	    (impi-res ('impi (list (cadr foralli-res) nil) nil)))))     
      (tac~forget&destroy-hyp (list (second result)) old-hyp (third result))
     (tacl~apply  'weaken (list (second result) prem) nil))
    (tacl~end)))

(com~defcommand eq-add
  (argnames openline factor)
  (argtypes ndline termsym)
  (arghelps "Equation to prove" "Addition term")
  (function titac=eq-add)
  (defaults titac=eq-add-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Add some term to an open equation."))

(defun titac=eq-add (line param)
  (infer~compute-outline 'eq-add (list line nil) (list param)))


(defun titac=eq-add-defaults (line param)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified)))
	(t (list line param))))

;;;;;;;;;;;;;;
;;Apply the natural induction axiom 2
;;;;;;;;;;;;;;

(infer~deftactic apply-induct-all
		 (outline-mappings (((existent nonexistent nonexistent) apply-induct-all-b)))
		 (parameter-types termsym termsym)
	         (expansion-function natac=expand-apply-induct-all)
		 (help "Make Induction."))

(tac~deftactic apply-induct-all-b apply-induct-all (in base)
  (parameters (X term+constant "A new constant.")
              (b term+constant "The base case."))
  (conclusions C)
  (premises Pbase Pstep)
  (hypotheses (Hstep Pstep)
	      (Hsort Pstep))
  (sideconditions 
   (batac=forall-sort-p (formula C)))
  (computations 
   (Hstep (natac=compute-apply-induct-all-hyp (formula C) X))
   (Hsort (natac=compute-apply-induct-all-hyp-sort X))
   (Pstep (natac=compute-apply-induct-all-step (formula C) X))
   (Pbase (natac=compute-apply-induct-all-base (formula C) b)))
  (description "S-Existential elimination backwards"))

(defun natac=get-induct-predicate (term)
  (first (data~appl-arguments term)))

(defun natac=compute-apply-induct-all-base (term b)
  (beta~normalize
   (term~appl-create (natac=get-induct-predicate term)
				   (list b))))

(defun natac=compute-apply-induct-all-step (term n)
  (beta~normalize 
   (term~appl-create (natac=get-induct-predicate term)
		     (list (term~appl-create
			    (env~lookup-object :s (pds~environment omega*current-proof-plan))
			    (list n))))))

(defun natac=compute-apply-induct-all-hyp (term n)
  (let* ((forall-sort (env~lookup-object :forall-sort (pds~environment omega*current-proof-plan)))
         (implies     (env~lookup-object :implies (pds~environment omega*current-proof-plan)))
         (nat         (env~lookup-object :nat (pds~environment omega*current-proof-plan)))
         (leq         (env~lookup-object :leq (pds~environment omega*current-proof-plan)))
         (i           (term~variable-create 'i (env~lookup-object :num (pds~environment omega*current-proof-plan))))
         (succ        (env~lookup-object :s (pds~environment omega*current-proof-plan))))
  (beta~normalize
   (term~appl-create forall-sort
                     (list
                     (term~abstr-create
                     (list i)
                     (term~appl-create implies
                     (list
                     (term~appl-create leq (list i n))
                     (term~appl-create (natac=get-induct-predicate term)
		     (list i)))))
                     nat))
)))

(defun natac=compute-apply-induct-all-hyp-sort (n)
  (term~appl-create (env~lookup-object :nat (pds~environment omega*current-proof-plan))
		    (list n)))

;(defun natac=expand-apply-induct-all (outline param)
;  (let* ((axiom  (tacl~insert&return-assumption 'natbural 'nat-induct))
;         (conc (car outline))
;         (Pbase  (cadr outline))
;         (Pbase (caddr outline))
;         (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc))))
;         (n (car param))
;         (pred (natac=get-induct-predicate (node~formula (pds~label2node conc))))
;         (para ((data~abstr-domain pred))
;    (tacl~init outline)
;    (let ((result 
;           (tacl~sequence
;            (foralli ('foralli-sort (list conc nil) (list para)))
;            (foralle ('foralle (list axiom nil) (list      
;       (tac~forget&destroy-hyp (list (third result)) old-hyp (fourth result) :test 'term~alpha-equal)
;       (tacl~apply 'weaken (list (third result) prem) nil))
;    (tacl~end)))
    

(com~defcommand apply-induct-all
  (argnames line b)
  (argtypes ndline termsym)
  (arghelps "An line to prove" "The base case")
  (function natac=apply-induct-all)
;  (defaults natac=apply-induct-all-defaults)
  (frag-cats tactics base) 
  (log-p T)
  (help "Apply the indcution axiom on a line."))

(defun natac=apply-induct-all (line b)
  (let* ((env (pds~environment omega*current-proof-plan))
	(const (term~generate-term-primitive-with-new-name
		'n
		(env~lookup-object :num env)
		'term+constant
		env)))
    (infer~compute-outline 'apply-induct-all (list line nil nil)
			   (list const b))))


(defun natac=apply-induct-all-defaults (line)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline)))
	(t (list line))))




