;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
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
  (unless (com~find-category 'post)
    (com~defcategory post
		     (help "Tactics of the theory POST."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the common tactics for the theory POST.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;todo: automation of the sort inference and more of the outline-cases in the equal-tactics.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sforalli   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;(infer~deftactic sforalli
;                 (outline-mappings (((existent nonexistent) sforalli-b)
;                                    ((existent existent) sforalli-a)))
;                 (parameter-types termsym)
;                 (expansion-function potac=expand-sforalli)
;                 (help "FORALL-SORT-Elimination."))
;
;(tac~deftactic sforalli-b sforalli (in post)
;  (parameters (X term+constant "A new constant."))
;  (conclusions C)
;  (premises P )
;  (sideconditions 
;   (potac=forall-sort-p (formula C))
;   (pds~not-free-in-nodes-or-hyps-p X C))
;  (computations 
;   (P (potac=compute-sforalli X (formula C))))
;  (description "S-Universal introduction backwards"))
;
;(tac~deftactic sforalli-a sforalli (in post)
;  (parameters (X term+constant "A new constant."))
;  (conclusions C)
;  (premises P )
;  (sideconditions 
;   (potac=forall-sort-p (formula C))
;   (potac=sforall-applicable-p (formula C) (formula P) X)
;   (pds~not-free-in-nodes-or-hyps-p X C))
;  (computations )
;  (description "S-Universal introduction application"))
;
;
;(defun potac=forall-sort-p (formula)
;  (and
;   (term~appl-p formula)
;   (data~schema-equal (data~appl-function formula)
;               (env~lookup-object :forall-sort (pds~environment omega*current-proof-plan)))))
;
;(defun potac=sforall-applicable-p (con pre term)
;  (term~alpha-equal pre (potac=compute-sforalli term con)))
;
;
;(defun potac=compute-sforalli (term formula)
;  (let* ((args (data~appl-arguments formula))
;         (sort (second args))
;         (abstr (first args)))
;    (term~appl-create
;     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
;     (list
;      (term~appl-create sort (list term))
;      (beta~normalize (term~appl-create abstr (list term)))))))
;
;    
;(defun potac=expand-sforalli (outline parameters)
;  (let* ((fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
;         (definiendum (th~definition-constant fs-def))
;         (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive))))
;    (tacl~init outline)
;    (tacl~sequence
;     (defni-res ('defni (list (car outline) nil) (list definiendum definiens (pos~add-front 0))))
;     (foralli-res ('foralli (list (cadr defni-res) (cadr outline)) parameters)))
;    (tacl~end)))
;
;(com~defcommand sforalli
;  (argnames univ-line param line)
;  (argtypes ndline termsym  ndline)
;  (arghelps "A universal line to prove" "New parameter" "A line")
;  (function potac=sforalli)
;  (defaults potac=sforalli-defaults)
;  (frag-cats tactics post) 
;  (log-p T)
;  (help "Introduce a sorted universal quantifier."))
;
;(defun potac=sforalli (univ-line param line)
;  (infer~compute-outline 'sforalli (list univ-line line) (list param)))
;
;
;(defun potac=sforalli-defaults (uni-line param line)
;    (cond ((not (com~specified-arg-p uni-line))
;           (list (pds~find-open-node #'(lambda (x) (data~schema-equal
;                                                    (data~appl-function x)
;                                                    (env~lookup-object :forall-sort
;                                                                       (pds~environment omega*current-proof-plan)))))
;             (com~unspecified) (com~unspecified)))
;          ((not (com~specified-arg-p param))
;           (list uni-line
;                 (or 
;                  (potac=generate-defaults-sforalli
;                         uni-line
;                         (pds~environment omega*current-proof-plan))
;                  (com~unspecified))
;                 (com~unspecified)))
;          ((not (com~specified-arg-p line))
;           (list uni-line param  (oc~nil-argument)))
;          (t (list uni-line param line))))
;
;(defun potac=generate-defaults-sforalli (line env)  
;  (when (node~p line)
;    (let ((form (node~formula line)))
;      (when (data~schema-equal
;             (data~appl-function form)
;             (env~lookup-object :forall-sort
;                                (pds~environment omega*current-proof-plan)))
;        (let ((var (logic~quantification-bound-variable form)))
;          (term~generate-term-primitive-with-new-name 
;           (keim~name var) (term~type var) 'term+constant env))))))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sforalle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;(infer~deftactic sforalle
;                 (outline-mappings (((nonexistent existent) sforalle-f)
;                                    ((existent existent) sforalle-a)))
;                 (parameter-types term)
;                 (expansion-function potac=expand-sforalle)
;                 (help "FORALL-SORT-Elimination."))
;
;(tac~deftactic sforalle-f sforalle (in post)
;  (parameters (X term+term "A  term."))
;  (conclusions C)
;  (premises P )
;  (sideconditions 
;   (potac=forall-sort-p (formula P)))
;  (computations 
;   (C (potac=compute-sforalle X (formula P))))
;  (description "S-Universal elimination forwards"))
;
;(tac~deftactic sforalle-a sforalle (in post)
;  (parameters (X term+constant "A new constant."))
;  (conclusions C)
;  (premises P )
;  (sideconditions 
;   (potac=forall-sort-p (formula P))
;   (potac=sforall-applicable-p (formula P) (formula C) X)
;   (pds~node-supported-by-p C P))
;  (computations )
;  (description "S-Universal elimination application"))
;
;(defun potac=compute-sforalle (term formula)
;  (let* ((args (data~appl-arguments formula))
;         (sort (second args))
;         (abstr (first args)))
;    (term~appl-create
;     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
;     (list
;      (term~appl-create sort (list term))
;      (beta~normalize (term~appl-create abstr (list term)))))))
; 
;    
;(defun potac=expand-sforalle (outline parameters)
;  (let* ((fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
;         (definiendum (th~definition-constant fs-def))
;         (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive))))
;    (tacl~init outline)
;    (tacl~sequence
;     (defne-res ('defne (list nil (cadr outline)) (list definiendum definiens (pos~add-front 0))))
;     (foralle-res ('foralle (list (car outline) (car defne-res)) parameters)))
;    (tacl~end)))
;
;(com~defcommand sforalle
;  (argnames univ-line line term)
;  (argtypes ndline ndline term)
;  (arghelps "Universal line" "A line" "Term to substitute")
;  (function potac=sforalle)
;  (defaults potac=sforalle-defaults)
;  (frag-cats tactics post) 
;  (log-p T)
;  (help "Eliminate a sorted universal quantifier."))
;
;(defun potac=sforalle (univ-line line param)
;  (infer~compute-outline 'sforalle (list line univ-line) (list param )))
;
; 
;(defun potac=sforalle-defaults (univ elim term)
;  (cond ((not (com~specified-arg-p univ))
;         (list (pds~find-support #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
;        ((not (com~specified-arg-p elim))
;         (list univ
;               (if (and univ (logic~universal-quantification-p (node~formula univ)))
;                   (let ((scope (logic~quantification-scope (node~formula univ))))
;                     (pds~find-open-node #'(lambda (x) (term~alpha-match scope x))))
;                 (oc~nil-argument))
;               (com~unspecified)))
;        ((not (com~specified-arg-p term))
;         (list univ
;               elim
;               (if (and univ
;                        (logic~universal-quantification-p (node~formula univ))
;                        elim)
;                   (let* ((subst (term~alpha-match (logic~quantification-scope (node~formula univ))
;                                                   (node~formula elim))))
;                     (if subst
;                         (car (subst~codomain subst))
;                       (com~unspecified)))
;                 (com~unspecified))))
;        (t (list univ elim term))))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foralle-sort  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic foralle-sort
		 (outline-mappings (((nonexistent existent existent) foralle-sort-f2)
				    ((nonexistent existent nonexistent) foralle-sort-f)
				    ((existent existent nonexistent) foralle-sort-s)
				    ((existent existent existent) foralle-sort-a)))
		 (parameter-types term)
	         (expansion-function potac=expand-foralle-sort)
		 (help "FORALL-SORT-Elimination."))


(tac~deftactic foralle-sort-s foralle-sort (in post)
  (parameters (X term+term "A  term."))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=forall-sort-formula? (formula P))
   (potac=foralle-sort-applicable-conc-p (formula P) (formula C) X))
  (computations 
   (S (potac=compute-forall-sort-hyp X (formula P))))
  (description "S-Universal elimination sideward"))

(tac~deftactic foralle-sort-f2 foralle-sort (in post)
  (parameters (X term+term "A  term."))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=forall-sort-formula? (formula P))
   (potac=foralle-sort-applicable-sort-p (formula p) (formula S) X))
  (computations 
   (C (potac=compute-forall-sort X (formula P))))
  (description "S-Universal elimination forward"))

(tac~deftactic foralle-sort-f foralle-sort (in post)
  (parameters (X term+term "A  term."))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=forall-sort-formula? (formula P)))
  (computations 
   (C (potac=compute-forall-sort X (formula P)))
   (S (potac=compute-forall-sort-hyp X (formula P))))
  (description "S-Universal elimination forwards"))

(tac~deftactic foralle-sort-a foralle-sort (in post)
  (parameters (X term+term "A term."))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=forall-sort-formula? (formula P))
   (potac=foralle-sort-applicable-p (formula P) (formula C) (formula S)  X))
  (computations )
  (description "S-Universal elimination application"))

(defun potac=foralle-sort-applicable-p (pre con sort term)
  (and (term~alpha-equal con (potac=compute-forall-sort term pre))
       (term~alpha-equal sort (potac=compute-forall-sort-hyp term pre))))

(defun potac=foralle-sort-applicable-sort-p (pre sort term)
  (term~alpha-equal sort (potac=compute-forall-sort-hyp term pre)))

(defun potac=foralle-sort-applicable-conc-p (pre con term)
  (term~alpha-equal con (potac=compute-forall-sort term pre)))

(defun potac=expand-foralle-sort (outline parameters)
  (let* ((quantorstring (string (keim~name (data~appl-function (node~formula (cadr outline))))))
	 (fs-def (th~find-assumption quantorstring (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive))))
    (tacl~init outline)
    (tacl~sequence
     (defne-res ('defne (list nil (cadr outline)) (list definiendum definiens (pos~add-front 0))))
     (foralle-res ('foralle (list nil (car defne-res)) parameters))
     (impe-res ('impe (list (car outline) (caddr outline) (car foralle-res)) nil)))
    (tacl~end)))

(com~defcommand foralle-sort
  (argnames univ-line line  term so-line)
  (argtypes ndline ndline term ndline)
  (arghelps "Universal line" "A line" "Term to substitute" "A line with sort")
  (function potac=foralle-sort)
  (defaults potac=foralle-sort-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Eliminate a sorted universal quantifier."))

(defun potac=foralle-sort (univ-line line param so-line)
  (infer~compute-outline 'foralle-sort (list line univ-line so-line) (list param )))

(defgeneric potac=forall-sort-formula? (form)
    (:method ((form node+node))
	     (potac=forall-sort-formula? (node~formula form)))
    (:method ((form term+appl))
	      (let ((func (data~appl-function form))
		    (env  (pds~environment omega*current-proof-plan)))
		(or (data~schema-equal func
				       (env~lookup-object :forall-sort env))
		    (data~schema-equal func
				       (env~lookup-object :forall-defined env)))))
    (:method ((form term+term)) nil))

(defun potac=foralle-sort-defaults (univ elim term so-line)
  (cond ((not (com~specified-arg-p univ))
	 (list (pds~find-support #'potac=forall-sort-formula?)
	       (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p elim))
	 (list univ
	       (if (and univ (logic~universal-quantification-p (node~formula univ)))
		   (let ((scope (logic~quantification-scope (node~formula univ))))
		     (pds~find-open-node #'(lambda (x) (term~alpha-match scope x))))
		 (oc~nil-argument))
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p term))
	 (list univ
	       elim
	       (if (and univ
			(logic~universal-quantification-p (node~formula univ))
			elim)
		   (let* ((subst (term~alpha-match (logic~quantification-scope (node~formula univ))
						   (node~formula elim))))
		     (if subst
			 (car (subst~codomain subst))
		       (com~unspecified)))
		 (com~unspecified))
	       (com~unspecified)))
	((not (com~specified-arg-p so-line))
	 (list univ
		elim
		term
                (if (and univ (= (length (data~appl-arguments (node~formula univ))) 2) term)
		(let* ((sort (second (data~appl-arguments (node~formula univ))))  
		       (sort-node (pds~find-support #'(lambda (x)
				      (and (data~appl-p x)
					   (term~alpha-equal sort (data~appl-function x))
					   (term~alpha-equal term (car (data~appl-arguments x))))))))
		  (if sort-node sort-node (oc~nil-argument))) (oc~nil-argument))))
	(t (list univ elim  term so-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foralli-sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic foralli-sort
		 (outline-mappings (((existent nonexistent) foralli-sort-b)
				    ((existent existent) foralli-sort-a)))
		 (parameter-types termsym)
	         (expansion-function potac=expand-foralli-sort)
		 (help "FORALL-SORT-Elimination."))

(tac~deftactic foralli-sort-b foralli-sort (in post)
  (parameters (X term+constant "A new constant."))
  (conclusions C)
  (premises P)
  (hypotheses ((H P) "H is a hypothesis for P"))
  (sideconditions 
   (potac=forall-sort-formula? (formula C))
   (pds~not-free-in-nodes-or-hyps-p X C))
  (computations 
   (P (potac=compute-forall-sort X (formula C)))
   (H (potac=compute-forall-sort-hyp X (formula C))))
  (description "S-Universal introduction backwards"))

(tac~deftactic foralli-sort-a foralli-sort (in post)
  (parameters (X term+constant "A new constant."))
  (conclusions C)
  (premises P)
  (hypotheses ((H P) "H is a hypothesis for P"))
  (sideconditions 
   (potac=forall-sort-formula? (formula C))
   (potac=foralli-sort-applicable-p (formula C) (formula P)  X (hyps P))
   (pds~not-free-in-nodes-or-hyps-p X C))
  (description "S-Universal introduction application"))


(defun potac=foralli-sort-applicable-p (con pre term hyp-list)
  (let ((hyp (potac=compute-forall-sort-hyp term con)))
    (and (term~alpha-equal pre (potac=compute-forall-sort term con))
	 (some #'(lambda (h) (term~alpha-equal hyp (node~formula h))) hyp-list))))
  

(defun potac=compute-forall-sort (term formula)
  (let* ((args (data~appl-arguments formula))
	 (abstr (first args)))
    (beta~normalize (term~appl-create abstr (list term)))))

(defun potac=compute-forall-sort-hyp (term formula)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (sort (if (data~schema-equal (data~appl-function formula)
				      (env~lookup-object :forall-sort env))
		   (second (data~appl-arguments formula))
		(env~lookup-object :defined env))))
     (term~appl-create sort (list term))))
    
(defun potac=expand-foralli-sort (outline parameters)
  (let* ((quantorstring (string (keim~name (data~appl-function (node~formula (car outline))))))
	 (fs-def (th~find-assumption quantorstring (prob~theory omega*current-proof-plan)))
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

(com~defcommand foralli-sort
  (argnames univ-line param line)
  (argtypes ndline termsym  ndline)
  (arghelps "Universal line to prove" "New parameter" "A line" )
  (function potac=foralli-sort)
  (defaults potac=foralli-sort-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Introduce a sorted universal quantifier."))

(defun potac=foralli-sort (univ-line param line )
  (infer~compute-outline 'foralli-sort (list univ-line line) (list param)))


(defun potac=foralli-sort-defaults (uni-line param line)
    (cond ((not (com~specified-arg-p uni-line))
           (list (pds~find-open-node #'potac=forall-sort-formula?)
		 (com~unspecified) (com~unspecified)))
          ((not (com~specified-arg-p param))
           (list uni-line
                 (or 
                  (potac=generate-new-constant
                         uni-line
                         (pds~environment omega*current-proof-plan))
                  (com~unspecified))
                 (com~unspecified)))
          ((not (com~specified-arg-p line))
           (list uni-line param  (oc~nil-argument)))
          (t (list uni-line param line))))

(defun potac=generate-new-constant (line env)  
  (when (node~p line)
    (let ((form (node~formula line)))
      (when (or (potac=forall-sort-formula? form)
		(potac=exists-sort-formula? form))
        (let ((var (logic~quantification-bound-variable form)))
          (term~generate-term-primitive-with-new-name 
           (keim~name var) (term~type var) 'term+constant env))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; existsi-sort  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic existsi-sort
		 (outline-mappings (((existent existent nonexistent) existsi-sort-r)
				    ((existent nonexistent existent) existsi-sort-l)
 				    ((existent nonexistent nonexistent) existsi-sort-b)
				    ((nonexistent existent existent) existsi-sort-f)
				    ((existent existent existent) existsi-sort-a)))
		 (parameter-types term position-list)
	         (expansion-function potac=expand-existsi-sort)
		 (help "EXISTS-SORT-Introduction."))

(tac~deftactic existsi-sort-r existsi-sort (in post)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=exists-sort-formula? (formula C))
   (potac=existsi-sort-applicable-prem-p (formula C) (formula P) X))
  (computations 
   (S (potac=compute-exists-sort-sort X (formula C))))
  (description "S-Existential introduction backwards"))

(tac~deftactic existsi-sort-f existsi-sort (in post)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P S)
  (sideconditions )
   ;(orules=equal-at-positions-p X P PList))
  (computations 
   (C (potac=compute-exists-sort-exists X (formula P) (formula S) PList)))
  (description "S-Existential introduction forward"))

(tac~deftactic existsi-sort-l existsi-sort (in post)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=exists-sort-formula? (formula C))
   (potac=existsi-sort-applicable-sort-p (formula C) (formula S) X))
  (computations 
   (P (potac=compute-exists-sort X (formula C))))
  (description "S-Existential introduction backwards"))

(tac~deftactic existsi-sort-b existsi-sort (in post)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=exists-sort-formula? (formula C)))
  (computations 
   (P (potac=compute-exists-sort X (formula C)))
   (S (potac=compute-exists-sort-sort X (formula C))))
  (description "S-Existential introduction backwards"))

(tac~deftactic existsi-sort-a existsi-sort (in post)
  (parameters (X term+term "The witness term.")
	      (PList list "positions of the witness term in the premise"))
  (conclusions C)
  (premises P S)
  (sideconditions 
   (potac=exists-sort-formula? (formula C))
   (potac=existsi-sort-applicable-p (formula C) (formula P) (formula S) X))
  (computations)
  (description "S-Existential introduction application"))

(defun potac=compute-exists-sort-exists (term prem sort  Pos-List)
  (let* ((new-var  (orules=new-variable (term~type term)))
	 (new-range (orules=replace-at-positions prem pos-list new-var)))
  (term~appl-create (env~lookup-object :exists-sort (pds~environment omega*current-proof-plan))
		    (list (term~abstr-create (list new-var) new-range)
			  (data~appl-function sort)))))
   
(defun potac=existsi-sort-applicable-p (con pre sort term)
  (and (term~alpha-equal pre (potac=compute-exists-sort term con))
       (term~alpha-equal sort (potac=compute-exists-sort-sort term con))))

(defun potac=existsi-sort-applicable-sort-p (con sort term)
       (term~alpha-equal sort (potac=compute-exists-sort-sort term con)))

(defun potac=existsi-sort-applicable-prem-p (con pre term)
       (term~alpha-equal pre (potac=compute-exists-sort term con)))

(defun potac=compute-exists-sort (term formula)
  (let* ((args (data~appl-arguments formula))
	 (abstr (first args)))
    (beta~normalize (term~appl-create abstr (list term)))))


(defun potac=compute-exists-sort-sort (term formula)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (sort (if (data~schema-equal (data~appl-function formula)
				      (env~lookup-object :exists-sort env))
		   (second (data~appl-arguments formula))
		(env~lookup-object :defined env))))
     (term~appl-create sort (list term))))
    
(defun potac=expand-existsi-sort (outline param)
  (let* ((quantorstring (string (keim~name (data~appl-function (node~formula (car outline))))))
	 (fs-def (th~find-assumption quantorstring (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive)))
	 (pos-list (append (mapcar #'(lambda (x) (pos~add-front 1 x))
				   (data~substruct-positions (first param) (node~formula (third outline))))
			   (mapcar #'(lambda (x) (pos~add-front 2 x))
				   (second param)))))
    (tacl~init outline)
    (tacl~sequence
	    (defni-res ('defni (list (car outline) nil) (list definiendum definiens (pos~add-front 0))))
	    (existsi-res ('existsi (list (cadr defni-res) nil) (list (car param) pos-list)))
	    (andi-res ('andi (list (cadr existsi-res) (third outline) (second outline)) nil)))     
    (tacl~end)))

(com~defcommand existsi-sort
  (argnames ex-line param line so-line pos-list)
  (argtypes ndline term  ndline ndline position-list)
  (arghelps "Existential line to prove" "Witness term" "A line" "A line with sort" "The position(s) of the witness term")
  (function potac=existsi-sort)
  (defaults potac=existsi-sort-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Introduce a sorted existential quantifier."))

(defun potac=existsi-sort (ex-line param line so-line pos-list)
  (infer~compute-outline 'existsi-sort (list ex-line line so-line) (list param pos-list)))

(defgeneric potac=exists-sort-formula? (form)
    (:method ((form node+node))
	     (potac=exists-sort-formula? (node~formula form)))
    (:method ((form term+appl))
	      (let ((func (data~appl-function form))
		    (env  (pds~environment omega*current-proof-plan)))
		(or (data~schema-equal func
				       (env~lookup-object :exists-sort env))
		    (data~schema-equal func
				       (env~lookup-object :exists-defined env)))))
    (:method ((form term+term)) nil))

(defun potac=existsi-sort-defaults (ex-line param line so-line pos-list)
    (cond ((not (com~specified-arg-p ex-line))
           (list (pds~find-open-node #'potac=exists-sort-formula?)
                 (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
          ((not (com~specified-arg-p param))
           (list ex-line (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
          ((not (com~specified-arg-p line))
           (list ex-line param (oc~nil-argument)(com~unspecified)(com~unspecified)))
	  ((not (com~specified-arg-p so-line))
           (list ex-line
		 param 
		 line
		 (if (and ex-line (= (length (data~appl-arguments (node~formula ex-line))) 2) param)
		     (let* ((sort (second (data~appl-arguments (node~formula ex-line))))  
		       (sort-node (pds~find-support #'(lambda (x)
				      (and
				       (term~appl-p x)
				       (term~alpha-equal sort (data~appl-function x))
				       (term~alpha-equal param (car (data~appl-arguments x))))))))
		  (if sort-node sort-node (oc~nil-argument))) (oc~nil-argument))
		 (com~unspecified)))
	  ((not (com~specified-arg-p pos-list))
	     (list ex-line
		   param 
		   line
		   so-line
		   (if (and param line) (data~substruct-positions param (node~formula line))
		     (if (logic~existential-quantification-p (node~formula ex-line))
			 (data~substruct-positions (logic~quantification-bound-variable (node~formula ex-line))
						   (logic~quantification-scope (node~formula ex-line)))
		       (com~unspecified))))) 
          (t (list ex-line param line so-line pos-list))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; existse-sort  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic existse-sort
		 (outline-mappings (((existent existent nonexistent) existse-sort-b)
				    ((existent existent existent) existse-sort-a)))
		 (parameter-types termsym)
	         (expansion-function potac=expand-existse-sort)
		 (help "EXISTS-SORT-Elimination."))


(tac~deftactic existse-sort-b existse-sort (in post)
  (parameters (X term+constant "The new term."))
  (conclusions C)
  (premises E P)
  (hypotheses (H P))
  (sideconditions 
   (potac=exists-sort-formula? (formula E))
   (pds~not-free-in-nodes-or-hyps-p X C E))
  (computations 
   (H (potac=compute-existse-sort-hyp X (formula E)))
   (P (batac=compute-existse-sort (formula C))))
  (description "S-Existential elimination backwards"))

(tac~deftactic existse-sort-a existse-sort (in post)
  (parameters (X term+constant "The new term."))
  (conclusions C)
  (premises E P)
  (hypotheses (H P))
  (sideconditions 
   (potac=exists-sort-formula? (formula E))
   (pds~not-free-in-nodes-or-hyps-p X C E)
   (potac=existse-sort-applicable-p (formula C) (formula E) (formula P) (hyps P)  X ))
  (description "S-Existential introduction application"))

(defun potac=existse-sort-applicable-p (con ex pre hyp-list term)
  (let ((hyp (potac=compute-existse-sort-hyp term ex)))
      (and (term~alpha-equal pre con)
	   (some #'(lambda (h) (term~alpha-equal hyp (node~formula h))) hyp-list))))    
	  
(defun potac=compute-existse-sort-hyp (term ex)
       (term~appl-create (env~lookup-object :and (pds~environment omega*current-proof-plan))
			 (list (potac=compute-exists-sort-sort term ex)
			       (potac=compute-exists-sort term ex))))

(defun potac=expand-existse-sort (outline param)
  (let* ((quantorstring (string (keim~name (data~appl-function (node~formula (second outline))))))
	 (fs-def (th~find-assumption quantorstring (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant fs-def))
	 (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive)))
	 (conc (car outline))
	 (exi  (cadr outline))
	 (prem (caddr outline))
	 (old-hyp (car (set-difference (pdsn~hyps prem) (pdsn~hyps conc)))))
    (tacl~init outline)
    (let ((result 
	   (tacl~sequence
	    (defne-res ('defne (list nil exi) (list definiendum definiens (pos~add-front 0))))
	    (existse-res ('existse (list conc (car defne-res) nil) param)))))     
       (tac~forget&destroy-hyp (list (third result)) old-hyp (fourth result) :test 'term~alpha-equal)
       (tacl~apply 'weaken (list (third result) prem) nil))
    (tacl~end)))
    

(com~defcommand existse-sort
  (argnames ex-line line param prem)
  (argtypes ndline ndline termsym ndline)
  (arghelps "An existential line" "A line to be proved" "A term" "The second premise line")
  (function potac=existse-sort)
  (defaults potac=existse-sort-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Eliminate a sorted existential quantifier."))

(defun potac=existse-sort (ex-line line param prem)
  (infer~compute-outline 'existse-sort (list  line ex-line prem) (list param)))


(defun potac=existse-sort-defaults (ex-line line param prem)
    (cond ((not (com~specified-arg-p ex-line))
           (list (pds~find-support #'potac=exists-sort-formula?) 
                 (com~unspecified)
		 (com~unspecified)
		 (com~unspecified)))
          ((not (com~specified-arg-p line))
           (list ex-line
		 (oc~default-current-planline)
		 (com~unspecified)
		 (com~unspecified)))
	  ((not (com~specified-arg-p param))
	   (list ex-line
		 line
                 (or 
                  (potac=generate-new-constant
                         ex-line
                         (pds~environment omega*current-proof-plan))
                  (com~unspecified))
		  (com~unspecified)))
	  ((not (com~specified-arg-p prem))
           (list ex-line
		 line
		 param
		 (oc~nil-argument)))
          (t (list ex-line line param prem))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wellsorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic wellsorted
		 (outline-mappings (((existent list) wellsorted-b)
				    ((existent nonexistent) wellsorted-b)))
		 (parameter-types anything-list)
	         (expansion-function potac=expand-wellsorted)
		 (passkey :node)
		 (help  "Proves that a formula is wellsorted"))

(defun wellsorted-b (conc prems parameters)
  (if (or prems (car parameters)) t  nil))


(defun potac=expand-numbers (node &key (forward nil))
  (typecase node
    (cons 
     (cons (potac=expand-numbers (car node) :forward forward)
	    (potac=expand-numbers (cdr node) :forward forward)))
    (null nil)
    (t (let*  ((env (pds~environment omega*current-proof-plan))
	       (theory (prob~theory omega*current-proof-plan))
	       (senv (th~senv theory))
	       (numbers (remove-duplicates (remove-if-not #'term~number-p (data~all-substructs (node~formula node))) :test #'data~equal)))
	 (dolist (num numbers node)
	   (let* ((numsym (make-symbol (format nil "~A" (keim~name num))))
		  (def (or (th~find-assumption numsym theory)
			   (make-instance 'th+def
					  :name numsym
					  :constant num
					  :theory theory
					  :node (natac=numbers-2-function num)
					  :help "")))
		  (definiens (data~copy (th~ass-node def) :downto '(term+constant
								    type+primitive)))
		  (poslist (data~substruct-positions num (node~formula node)
						     :test #'data~equal)))
	     (if forward
		 (setf node (car (tacl~apply 'defne* (list nil node) (list num definiens poslist))))
	       (setf node (cadr (tacl~apply 'defni* (list node nil) (list num definiens poslist)))))))))))


(defun potac=expand-wellsorted (concs prems params)
;  (mapc #'potac=node-formula!  concs)
;  (mapc #'potac=node-formula!  prems)
  (let* ((thms  (car params))
	 (env (pds~environment omega*current-proof-plan))
	 (theory (prob~theory omega*current-proof-plan))
	 (senv (th~senv theory)))
  (labels ((expand-node (nodes)
			(when nodes
			  (let* ((node (car nodes))
				 (form (node~formula node))
				 (weaken? (find-if #'(lambda (prem)
						       (term~alpha-equal (node~formula prem) form)) prems)))
			    ;(omega~trace "expand-node: node ~A node-formula ~A weaken? ~A" node (node~formula node) weaken?)
			    (if weaken?
				(progn
				  (tacl~apply 'weaken (list node weaken?) nil)
				  (expand-node (rest nodes)))
			      (let* ((new-node (potac=expand-numbers node))
				     (formula (node~formula new-node))
				     (sort (sort~sort-of-pred (data~appl-function formula) senv))
				     (term (car (data~appl-arguments formula)))
				     (theo? (find-if #'(lambda (th) 
							 (and
							  sort
							  (data~equal sort (second th))
							  (if (data~abstr-p term)
							      (term~alpha-match (data~abstr-range term)
										(car th))
							    (term~alpha-equal term
									      (car th)))))
									      thms)))
				;(omega~trace "expand-node: node ~A node-formula ~A theo? ~A" node (node~formula node) theo?)
				(if theo?
				    (progn
				      (setf thms (remove theo? thms))
				      (expand-node (append (rest nodes)
							   (second
							    (tacl~apply 'apply-theorem
									(list new-node nil)
									(rest (rest theo?)))))))
				  (progn (fun-case new-node) (expand-node (rest nodes)))))))))
	   (fun-case (node)
		     (let* ((funsort (th~find-assumption "fun-sort" theory))
			    (funsortdefiniendum (th~definition-constant funsort))
			    (funsortdefiniens (data~copy (th~ass-node funsort) :downto '(term+constant type+primitive)))
			    (dummy))
		       (tacl~sequence
			(deffun  ('defni (list node nil)
				  (list funsortdefiniendum funsortdefiniens (pos~list-position '(0)))))
			(alli    ('foralli-sort* (list (second deffun) nil)
						 (list  (orules=generate-defaults-foralli (second deffun) env))))
			(sorted  ('wellsorted
				    (progn
				      (setq dummy (potac=wellsorted-check-and-return-theorems (car (second alli))
							      (append (third alli) prems)))
				      (list (car (second alli)) (car dummy)))
				    (rest dummy))))))
	   (ugly-case (node)
		      (let* ((func (th~find-assumption "functions" theory))
			     (funcdefiniendum (th~definition-constant func))
			     (funcdefiniens (data~copy (th~ass-node func) :downto '(term+constant type+primitive)))
			     (total (th~find-assumption "total" theory))
			     (totaldefiniendum (th~definition-constant total))
			     (totaldefiniens (data~copy (th~ass-node total) :downto '(term+constant type+primitive)))
			     (image (th~find-assumption "image-of-domain" theory))
			     (imagedefiniendum (th~definition-constant image))
			     (imagedefiniens (data~copy (th~ass-node image) :downto '(term+constant type+primitive)))
			     (subset (th~find-assumption "subset" theory))
			     (subsetdefiniendum (th~definition-constant subset))
			     (subsetdefiniens (data~copy (th~ass-node subset) :downto '(term+constant type+primitive)))
			     (dummy)) ;; super bescheuert
		      (sys~handler-case
		       (tacl~sequence
			 (deffunc ('defni (list node nil)
				    (list funcdefiniendum funcdefiniens (pos~list-position '(0)))))
			 (deftot  ('defni (list (second deffunc) nil)
				    (list totaldefiniendum totaldefiniens (pos~list-position '(1 0)))))
			 (defsub  ('defni (list (second deftot) nil)
				    (list subsetdefiniendum subsetdefiniens (pos~list-position '(2 0)))))
			 (defimg  ('defni (list (second defsub) nil)
				    (list imagedefiniendum imagedefiniens (pos~list-position '(2 1 0 1 0)))))
			 (andi    ('andi (list (second defimg) nil nil) nil))
			 (lalli   ('foralli-sort (list (second andi) nil)
						 (list (potac=generate-new-constant (second andi) env))))
			 (lexi    ('existsi-sort (list (second lalli) nil nil)
						 (list (data~struct-at-position (node~formula (second lalli))
										(pos~list-position '(1 0 2)))
						       (list (pos~list-position '(1))))))
			 (lref    ('=ref (list (second lexi))
					 (list (data~struct-at-position (node~formula (second lexi))
									(pos~list-position '(1))))))
			 (lsort   ('wellsorted
				   (progn
				     (setq dummy (potac=wellsorted-check-and-return-theorems (third lexi)
											     (cons (third lalli) prems)))
				     (list (third lexi) (car dummy)))
				   (rest dummy)))						 
			 (ralli    ('foralli-sort (list (third andi) nil)
						  (list (potac=generate-new-constant (second andi) env))))
			 (rimp     ('impi (list (second ralli) nil) nil))
			 (rexe     ('existse-sort (list (second rimp) (third rimp) nil)
						  (list (potac=generate-new-constant (second andi) env))))
			 (rand     ('ande (list nil nil (fourth rexe)) nil))
			 (rsubst   ('=subst (list (third rexe) nil (second rand))
					    (list (pos~list-position '(1)))))
			 (rsort    ('wellsorted
				    (progn
				      (setq dummy (potac=wellsorted-check-and-return-theorems (second rsubst)
							      (cons (third ralli) (cons (first rand) prems))))
				      (list (second rsubst) (car dummy)))
				    (rest dummy))))
		       (error (cond) 
			(omega~error "\"~A\" in the expansion of WELLSORTED: call MP." cond))))))
      (tacl~init (append concs prems))
      (expand-node concs)))
      (tacl~end))

(com~defcommand wellsorted
  (argnames line premises)
  (argtypes ndline ndline-list)
  (arghelps "A line with sort" "A list of premises" )
  (function potac=wellsorted-outline)
  (defaults potac=wellsorted-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Prove that a formula is wellsorted."))

(defun potac=wellsorted-outline (line prems)
  (let* ((allthms   (potac=wellsorted-check-and-return-theorems line prems))
	 (newprems  (car allthms))
	 (param     (second allthms)))
    (infer~compute-outline 'wellsorted (list line (remove-duplicates newprems)) (list param))))
 
(defun potac=wellsorted-check-and-return-theorems (line prems)
  (let* ((senv (th~senv (prob~proof-theory omega*current-proof-plan)))
	 (sortprems (potac=wellsorted-prems prems senv))
	 (sortterm (potac=node-formula line))
	 (sort (when (data~appl-p sortterm)
		 (sort~sort-of-pred (data~appl-c-function sortterm)
				    senv)))
	 (term (when sort
		 (data~appl-c-argument sortterm)))
	 (newsenv (if sortprems
		      (sort~env-create :parents (list senv)
				       :unsortedenv (pds~environment omega*current-proof-plan))
		    senv)))
    (when sort 
      (mapc #'(lambda (node)
		(let ((premterm (potac=node-formula node)))
		  (sort~env-enter (list
				   (data~appl-c-argument premterm)
				   (sort~sort-of-pred (data~appl-c-function premterm) senv)
				   node)
				  newsenv :termdecl t)))
	    sortprems)
      (let* ((allthms  (suni~sortcheck term sort newsenv))
	     (newprems (mapcan #'(lambda (node) (when (node~p (third node)) (list node))) allthms))
	     (thms     (set-difference allthms newprems)))
	(list (mapcar #'third newprems) thms)))))

(defun potac=node-formula (node)
  (let ((formula (if (node~p node) (node~formula node) node)))
    (if (th~find-theory 'natural) (natac=numbers-2-function formula ) formula)))

(defun potac=node-formula! (node)
  (let ((formula (if (node~p node) (node~formula node) node)))
    (if (th~find-theory 'natural) (natac=numbers-2-function! formula) bformula)))

(defun potac=wellsorted-prems (premlist sorted-env)
  (mapcan #'(lambda (node)
	      (let ((form (node~formula node)))
		(when (and (data~appl-p form)
			   (sort~sort-of-pred
			    (data~appl-c-function form)
			    sorted-env))
		  (list node))))
	  premlist))
	  
(defun potac=wellsorted-defaults (line prems)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p prems))
	 (list line
	       (if line
		   (let* ((sorted-env (th~senv (prob~proof-theory omega*current-proof-plan)))
			  (sorted-nodes (potac=wellsorted-prems (pds~node-supports line) sorted-env)))
		     (if sorted-nodes sorted-nodes (oc~nil-argument)))
		 (oc~nil-argument))))
	(t (list line prems))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply-theorem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic apply-theorem
		 (outline-mappings (((existent list) apply-theorem-b)
				    ((existent nonexistent) apply-theorem-b)
				    ((nonexistent list) apply-theorem-f)))
		 (parameter-types symbol)
	         (expansion-function potac=expand-apply-theorem)
		 (passkey :formula)
		 (help  "Proves that a formula by apply-theorem"))

(defun apply-theorem-b (conc prems parameters)  ;;noch fehler 
  ;(omega~trace "apply-theorem-b: thm ~A on ~A with ~A" parameters conc prems)
   (let ((newpremsubst  (potac=apply-theorem-test (car conc) prems (car parameters))))
     ;(omega~trace "apply-theorem-b ~A" newpremsubst)
     (cond
      ((null newpremsubst)
       (omega~warn "The conlusion ~A didn't match the succedent of the theorem." (car conc)))
      ((null (second newpremsubst))
       (omega~warn "The premises didn't match with prems of the theorem."))
      ((consp (car newpremsubst))
	 (values nil  (remove-duplicates (mapcar #'(lambda (prem)
				  (subst~apply (second newpremsubst) prem))
					       (car newpremsubst)) :test #'data~equal)))
      (T T))))

(defun apply-theorem-f (conc prems parameters)  
  ;(omega~trace "apply-theorem-f: thm ~A on ~A with ~A" parameters conc prems)
   (let ((newpremsubst  (potac=apply-theorem-test (car conc) prems (car parameters))))
     ;(omega~trace "apply-theorem-f ~A" newpremsubst)
     (cond
      ((null newpremsubst)
       (omega~warn "The conlusion ~A didn't match the succedent of the theorem." (car conc)))
      ((null (second newpremsubst))
       (omega~warn "The premises didn't match with prems of the theorem."))
      ((null (third newpremsubst))
       (omega~warn "No conclusion from theorem."))
      ((consp (car newpremsubst))
	 (values (list (subst~apply (second newpremsubst) (third newpremsubst)))
		 (remove-duplicates (mapcar #'(lambda (prem)
						(subst~apply (second newpremsubst) prem))
					    (car newpremsubst)) :test #'data~equal)))
      (T T))))

(defun potac=expand-apply-theorem (conc prems params)
    (let* ((newpremsubst (potac=apply-theorem-test (node~formula (car conc))
						   (mapcar #'node~formula prems)
						   (car params)))
	   (subst (second newpremsubst))
	   (premises (first newpremsubst))
	   (kappatheo (tacl~insert&return-assumption
		       (prob~theory omega*current-proof-plan)
		       (car params)))
	   (fs-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
	   (definiendum (th~definition-constant fs-def))
	   (definiens (data~copy (th~ass-node fs-def) :downto '(term+constant type+primitive))))
      (tacl~init (append conc prems))
      (let* ((theo (if (data~schema-p (node~formula kappatheo))
		       (car (tacl~apply 'kappae (list nil kappatheo)
					(subst~apply subst (list (data~schema-domain (node~formula kappatheo))))))
		     kappatheo))
	     (pos (data~substruct-positions
		   definiendum (node~formula theo) :test #'data~schema-equal))
	     (defni (if pos (car (tacl~apply 'defne* (list nil theo)
					       (list definiendum definiens pos)))
		      theo))
	     (weak (when (term~alpha-equal (node~formula defni)(node~formula (car conc)))
		     (tacl~apply 'weaken (list (car conc) defni) nil))))
	(tacl~end)
	(unless weak (alift~apply-command! prems defni (car conc))))))


(defun potac=apply-theorem-get-formula-vars (thy)
  (let ((imp      (logic~implication-constant))
	(all      (env~lookup-object 'forall (pds~environment omega*current-proof-plan)))
	(all-sort (env~lookup-object 'forall-sort (pds~environment omega*current-proof-plan))))
  (labels ((get-forms-vars (conc &optional forms vars)
			   ;(omega~trace "get-formula-vars: conc ~A forms ~A vars ~A" conc forms vars)
	     (cond ((data~schema-p conc)
		    (let ((termcopy (data~alpha-copy (data~schema-range conc) nil)))
		    (get-forms-vars termcopy
				    forms (append (term~type-variables-rec termcopy) vars))))
		   ((data~appl-p conc)
		    (let ((fct (data~appl-function conc))
			  (args (data~appl-arguments conc)))
		      (cond ((data~schema-equal all fct)
			     (get-forms-vars (data~abstr-scope (car args))
					     forms
					     (append (data~abstr-domain (car args)) vars)))
			    ((data~schema-equal all-sort fct)
			     (get-forms-vars (data~abstr-scope (car args))
					     (cons (term~appl-create (second args)(data~abstr-domain (car args))) forms)
					     (append (data~abstr-domain (car args)) vars)))
			    ((data~equal imp fct)
			     (get-forms-vars (second args)
					     (cons (car args) forms) vars))
			    (T
			     (list (cons conc forms) vars)))))
		   (t (list (cons conc forms) vars)))))
    (let* ((thm  (th~find-assumption thy (prob~theory omega*current-proof-plan)))
	   (conc (th~ass-formula thm)))
      (get-forms-vars conc)))))

(defun potac=apply-theorem-test (conc premises thm)
  (declare (edited  "28-MAR-2000")
	   (authors Pollet)
	   (input   "A conclusion formula, a list of premise formulas, a theorem name.")
	   (effect  "-")
	   (value   "If PREMISES is a subset of the antecedents a list with two lists:"
		    "the first list contains all additional premise formulas,"
		    "the second list contains substitions for the variables of the theorem."
		    "else an error message and NIL when CONC and the succedent of THM are"
		    "unmatchable."))
  (let* ((formvarlist (potac=apply-theorem-get-formula-vars thm))
	 (thmvars (cadr formvarlist)))
    (labels ((check-prem (prem thmprems subst)
			;(omega~trace "check-prem: prem ~A thmprems ~A subst ~A" prem thmprems subst)
			(cond ((null prem) (list thmprems subst))
			      ((null thmprems) (omega~error "Premise ~A did not match with any premise of theorem ~A." prem thm))
			      (T (let ((match? (term~alpha-match (car thmprems) prem  :subst subst)))
				   (if match?
				       (list  (car thmprems) (subst~compose-substitution match? subst))
				     (check-prem prem (cdr thmprems) subst)))))))
	   (let* ((theconc (if conc conc (caar formvarlist)))
		  ;(concsubst  (uni~substitution (car (uni~unify (list (list theconc (caar formvarlist))) :match-only thmvars))))
		  (concsubst (term~alpha-match (caar formvarlist) theconc))
		  (theoremprems (cdar formvarlist)))
	     ;(setf bla concsubst)(setf term1 theconc)(setf term2 (caar formvarlist))
	     (if concsubst
		  (do* ((prems premises (rest prems))
			(result (check-prem (car prems) theoremprems concsubst)
				(check-prem (car prems) thyprems newsubst))
			(thyprems (remove (car result)  theoremprems)
				  (remove (car result) thyprems))
			(newsubst (second result) (second result)))
		      ((or (null result)(null prems)) (list thyprems newsubst theconc)))
	       nil)))))

	 

(defun potac=apply-theorem-outline (line thy prems)
    (infer~compute-outline 'apply-theorem (list line prems) (list (keim~name thy))))
 
	  
(defun potac=apply-theorem-defaults (line prems)
  (cond ((not (com~specified-arg-p line))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p prems))
	 (list line
	       (if line
		   (let* ((sorted-env (th~senv (prob~proof-theory omega*current-proof-plan)))
			  (sorted-nodes (potac=apply-theorem-prems (pds~node-supports line) sorted-env)))
		     (if sorted-nodes sorted-nodes (oc~nil-argument)))
		 (oc~nil-argument))))
	(t (list line prems))))


(com~defcommand apply-theorem
  (argnames line thm  premlist)
  (argtypes ndline thy-assumption ndline-list)
  (arghelps "An open line" "A theorem" "A list of premises" )
  (function potac=apply-theorem-outline)
  (defaults );potac=apply-theorem-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Prove a formula by apply-theorem."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(infer~deftactic equalref
		 (outline-mappings (((nonexistent existent) equalref-f)
				    ((nonexistent nonexistent) equalref-u)
                                    ((existent existent) equalref-a)
                                    ((existent nonexistent) equalref-b)
				    ))
		 (parameter-types term)
		 (expansion-function potac=expand-equalref)
		 (help "Reflexivity of equal."))


(tac~deftactic equalref-f equalref (in post)
   (parameters (Term term+term "A term."))
   (premises P)
   (conclusions L1)
   (computations (L1 (potac=equalref-create Term)))
   (sideconditions (potac=equalref-defined-create term (formula P)))
   (description "Forward application equal-reflexivity."))


(tac~deftactic equalref-b equalref (in post)
   (parameters (Term term+term "A term."))
   (premises P)
   (conclusions L1)
   (computations (P (potac=equalref-defined-create term)))
   (sideconditions (potac=equalref-p (formula L1) Term))
   (description "Backward equal-reflexivity."))

(tac~deftactic equalref-u equalref (in post)
   (parameters (Term term+term "A term."))
   (premises P)
   (conclusions L1)
   (computations  (P (potac=equalref-defined-create term))
		  (L1 (potac=equalref-create Term)))
   (sideconditions )
   (description "equal-reflexivity."))

(tac~deftactic equalref-a equalref (in post)
   (parameters (Term term+term "A term."))
   (premises P)
   (conclusions L1)
   (computations )
   (sideconditions (potac=equalref-p (formula L1) Term)
		   (potac=equalref-defined-create term (formula P)))
   (description "Closing equal-reflexivity."))

(defun potac=equalref-defined-create (term &optional form)
  (if form
      (and (data~appl-p form)
	   (data~schema-equal (data~appl-function form)
			      (env~lookup-object :defined (pds~environment omega*current-proof-plan)))
	   (data~equal (car (data~appl-arguments form))
		       term))
    (term~appl-create
     (env~lookup-object :defined (pds~environment omega*current-proof-plan))
     (list Term))))

(defun potac=equalref-create (Term)
  (term~appl-create
   (env~lookup-object :equal (pds~environment omega*current-proof-plan))
   (list Term Term)))

(defun potac=equalref-p (formula Term)
  (when (and (logic~equality-p formula)
	     (data~schema-equal (data~appl-function formula)
				(env~lookup-object :equal (pds~environment omega*current-proof-plan))))
    (and (data~equal (car (data~appl-arguments formula)) (cadr (data~appl-arguments formula)))
	 (data~equal (car (data~appl-arguments formula)) Term))))

(defun potac=expand-equalref (outline parameters)
   (tacl~init outline)
   (tacl~apply 'apply-theorem (list (car outline)(rest outline))  (list 'equal-reflexivity))
   (tacl~end))

(com~defcommand equalref
  (argnames equality-line term)
  (argtypes ndline term)
  (arghelps "A line with equality" "A term")
  (function potac=equalref)
  (frag-cats tactics post)
  (defaults potac=equalref-defaults)
  (log-p T)
  (help "Equality-reflexity."))

(defun potac=equalref (conc term)
  (infer~compute-outline 'equalref (list conc nil) (list term)))


(defun potac=equalref-defaults (equality-line term)
    (cond ((not (com~specified-arg-p equality-line))
	   (list (pds~find-open-node #'logic~equality-p)
		  (com~unspecified)(com~unspecified)))
	  ((not (com~specified-arg-p term))
	   (if (and equality-line (logic~equality-p (node~formula equality-line)))
	       (list equality-line
		     (car (data~appl-arguments (node~formula equality-line))))
	     (list equality-line (com~unspecified))))
	  (t (list equality-line  term))))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalsym
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(infer~deftactic equalsym
		 (outline-mappings (((nonexistent existent nonexistent nonexistent) equalsym-f)
                                    ((existent existent nonexistent nonexistent) equalsym-a)
                                    ((existent nonexistent nonexistent nonexistent) equalsym-b)
				    ))
		 (parameter-types)
		 (expansion-function potac=expand-equalsym)
		 (help "Commutativity of equality."))


(tac~deftactic equalsym-f equalsym (in post)
   (premises L1 defl defr)
   (conclusions L2)
   (computations (L2 (potac=equalsym-create (formula L1)))
		 (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (logic~equality-p (formula L1)))
   (description "Forward application equality-symmetry."))

(tac~deftactic equalsym-a equalsym (in post)
   (premises L1 defl defr)
   (conclusions L2)
   (computations (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (batac=equality-sym-p (formula L1) (formula L2)))
   (description "Closing equality."))

(tac~deftactic equalsym-b equalsym (in post)
   (premises L1 defl defr)
   (conclusions L2)
   (computations (L1 (potac=equalsym-create (formula L2)))
		 (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (logic~equality-p (formula L2)))
   (description "Backward equality-symmetry."))

(defun potac=equalsym-defined-create-l (equality)
  (potac=equalref-defined-create (first (data~appl-arguments equality))))

(defun potac=equalsym-defined-create-r (equality)
  (potac=equalref-defined-create (second (data~appl-arguments equality))))

(defun potac=equalsym-create (term)
  (term~appl-create
   (env~lookup-object :equal (pds~environment omega*current-proof-plan))
   (list (second (data~appl-arguments term)) (first (data~appl-arguments term)))))

(defun potac=equalsym-p (term1 term2)
  (let ((equal (env~lookup-object :equal (pds~environment omega*current-proof-plan))))
  (and (logic~equality-p term1) (data~schema-equal (data~appl-function term1) equal)
       (logic~equality-p term2) (data~schema-equal (data~appl-function term2) equal))
       (let ((args1 (data~appl-arguments term1))
             (args2 (data~appl-arguments term2)))
         (and (term~alpha-equal (first args1) (second args2))
              (term~alpha-equal (second args1) (first args2))))))

(defun potac=expand-equalsym (outline parameters)
   (tacl~init outline)
   (tacl~apply 'apply-theorem (list (car outline)(rest outline))  (list 'equal-symmetry))
   (tacl~end))

(com~defcommand equalsym
  (argnames equality-line1 equality-line2)
  (argtypes ndline ndline)
  (arghelps "A line with the conclusion equality"
	    "Another line with the premise equality")
  (function potac=equalsym)
  (frag-cats tactics post)
  (defaults potac=equalsym-defaults)
  (log-p T)
  (help "Equality-symmetry."))

(defun potac=equalsym (P P2)
  (infer~compute-outline 'equalsym (list P P2 nil nil) nil))

(defun potac=equalsym-defaults (equality-line1 equality-line2)
    (cond ((not (com~specified-arg-p equality-line1))
	   (list (pds~find-open-node #'logic~equality-p)
		  (com~unspecified)))
	  ((not (com~specified-arg-p equality-line2))
	   (list equality-line1
		 (if (and (pdsn~p equality-line1) (logic~equality-p (node~formula equality-line1)))
		     (pds~find-node-support
		      equality-line1
		      #'(lambda (p) (data~equal p (potac=equalsym-create (node~formula equality-line1)))))
		   (pds~find-support #'logic~equality-p))))
	  (t (list equality-line1 equality-line2))))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalsubst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(infer~deftactic equalsubst
		 (outline-mappings (((nonexistent existent existent nonexistent nonexistent) equalsubst-f)
                                    ((existent existent existent nonexistent nonexistent) equalsubst-a)
                                    ((existent existent nonexistent nonexistent nonexistent) equalsubst-l)
				    ((existent nonexistent existent nonexistent nonexistent) equalsubst-r)
                                    ))
		 (parameter-types position)
		 (expansion-function potac=expand-equalsubst)
		 (help "Replacement property of equality."))


(tac~deftactic equalsubst-f equalsubst (in post)
   (parameters (position pos+position "A position."))
   (premises L1 L2 defl defr)
   (conclusions L3)
   (computations (L3 (potac=equalsubst-create-f
                      (formula L1) (formula L2) position))
		 (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (potac=equalsubst-f-p
		    (formula L1) (formula L2) position))
   (description "Forward application equality substitution."))


(tac~deftactic equalsubst-a equalsubst (in post)
   (parameters (position pos+position "A position."))
   (premises L1 L2  defl defr)
   (conclusions L3)
   (computations (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (potac=equalsubst-a-p
                    (formula L3) (formula L1) (formula L2) position))
   (description "Closing equality substitution."))

(tac~deftactic equalsubst-l equalsubst (in post)
   (parameters (position pos+position "A position."))
   (premises L1 L2  defl defr)
   (conclusions L3)
   (computations (L2 (potac=equalsubst-create-l (formula L3) (formula L1) position))
		 (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2)))) 
   (sideconditions (potac=equalsubst-l-p (formula L3) (formula L1) position))
   (description "Creating equation for a substitution."))

(tac~deftactic equalsubst-r equalsubst (in post)
   (parameters (position pos+position "A position."))
   (premises L1 L2  defl defr)
   (conclusions L3)
   (computations (L1 (potac=equalsubst-create-f (formula L3) (formula L2) position))
		 (defr (potac=equalsym-defined-create-l (formula L2))) 
		 (defl (potac=equalsym-defined-create-r (formula L2))))
   (sideconditions (potac=equalsubst-f-p
                    (formula L3) (formula L2) position))
   (description "Creating equation for a substitution."))

(defun potac=equalsubst-f-p (term equal-term position)
  (when (logic~equality-p equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'term~alpha-equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'term~alpha-equal)))
	(or  (find position positions-of-arg1 :test 'keim~equal)
	     (find position positions-of-arg2 :test 'keim~equal)))))

(defun potac=equalsubst-create-f (term equal-term position)
  (let* ((term-at-position (data~struct-at-position term position))
	 (args (data~appl-arguments equal-term)))
    (cond ((term~alpha-equal term-at-position (first args))
	   (data~replace-at-position term position (second args)))
	  (t
	   (data~replace-at-position term position (first args))))))
	  

(defun potac=equalsubst-a-p (conclusion term equal-term position)
  (and (potac=equalsubst-f-p term equal-term position)
       (term~alpha-equal (potac=equalsubst-create-f term equal-term position) conclusion)))

(defun potac=equalsubst-l-p (conclusion term position)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (and (find position positions-of-conc :test 'keim~equal)
	       (find position positions-of-term :test 'keim~equal))
      (term~alpha-equal (data~replace-at-position conclusion
						  position
						  (data~struct-at-position term position))
			term))))
	     
(defun potac=equalsubst-create-l (conclusion term position)
  (term~appl-create (env~lookup-object :equal (pds~environment omega*current-proof-plan))
		    (list (data~struct-at-position conclusion position)
			  (data~struct-at-position term position))))

(defun potac=expand-equalsubst (outline parameters)
  (let* ((concform (node~formula (car outline)))
	 (equality (data~appl-arguments (node~formula (third outline))))
	 (newvar   (term~variable-create (gentemp 's) (term~type (data~struct-at-position
									concform (car parameters)))))
	 (pred     (term~abstr-create (list newvar)
				      (data~replace-at-position concform (car parameters) newvar)))
	 (newconc (term~appl-create pred (list (car equality))))
	 (newprem (term~appl-create pred (rest equality))))
  (tacl~init outline)
   (tacl~sequence
    (prem     ('denormalize (list nil (second outline)) (list newprem)))
    (conc     ('denormalize (list (car outline) nil) (list newconc)))
    (conj-res ('andi (list nil (third outline) (car prem)) nil)) 
    (thm      ('apply-theorem (list (second conc)(cons (car conj-res)(cdddr outline)))  (list 'equal-substitution))))
   (tacl~end)))
	    

(com~defcommand equalsubst
  (argnames line1 line2 equality-line position)
  (argtypes ndline ndline ndline position)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equation to be applied." "A position.")
  (function potac=equalsubst)
  (frag-cats tactics post)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution."))

(defun potac=equalsubst (P P2 P3 position)
  (infer~compute-outline 'equalsubst (list P P2 P3 nil nil) (list position)))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denormalize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic denormalize
		 (outline-mappings (((nonexistent existent) denormalize-f)
                                    ((existent existent) denormalize-a)
                                    ((existent nonexistent) denormalize-b)
                                    ))
		 (parameter-types term)
		 (expansion-function potac=expand-denormalize)
		 (help "Introduce a line with a term that is alpha/beta/eta-equal."))


(tac~deftactic denormalize-f denormalize (in post)
   (parameters (t term+term "A term."))
   (premises L1)
   (conclusions L2)
   (computations (L2 (potac=denormalize t (formula l1))))
   (description "Forward application denormalize."))

(tac~deftactic denormalize-b denormalize (in post)
   (parameters (t  term+term  "A term."))
   (premises L1)
   (conclusions L2)
   (computations (L1 (potac=denormalize t (formula l2))))
   (description "Backward application denormalize."))

(tac~deftactic denormalize-a denormalize (in post)
   (parameters (t term+term "A term."))
   (premises L1)
   (conclusions L2)
   (sideconditions  (lam~equal-p (formula l1)(formula l2)))
   (description "Closed application denormalize."))

(defun potac=denormalize (term term2)
  (when (lam~equal-p term term2) term))

(defun potac=expand-denormalize (outline parameters)
  (tacl~init outline)
  (tacl~apply 'lambda outline nil)
  (tacl~end))

;don't know if this is useful as a command
;(com~defcommand denormalize
;  (argnames line1 line2 term)
;  (argtypes ndline ndline term)
;  (arghelps "Conc" "Prem" "Term")
;  (function potac=normalize-outline)
;  (frag-cats tactics post)
;  (defaults)
;  (level 1)
;  (log-p T))
;
;(defun potac=normalize-outline (C P term)
;  (infer~compute-outline 'denormalize (list C P) (list term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foralle-sort*  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic foralle-sort*
		 (outline-mappings (((nonexistent list) foralle-sort*-f)
				    ((existent list) foralle-sort*-f)))
		 (parameter-types term-list)
	         (expansion-function potac=expand-foralle-sort*)
		 (passkey :formula)
		 (help "FORALL-SORT*-Elimination."))


(defun foralle-sort*-f (conc prems parameters)
  (let* ((all (potac=forall-sort*-prems (car parameters) (car prems)))
	 (allprems (butlast all))
	 (newconc (last all)))
    (if (subsetp (rest prems) allprems :test #'term~alpha-equal)
	(if conc
	    (when (term~alpha-equal (car conc) (car newconc))
	      (if (subsetp allprems (rest prems) :test #'term~alpha-equal)
		  t
	      (values nil (set-difference allprems prems :test #'term~alpha-equal))))
	  (values newconc (set-difference allprems prems :test #'term~alpha-equal)))
      (omega~warn "More premises are inserted than needed."))))



(defun potac=forall-sort*-prems (terms conc)
  (cond
   ((consp terms)
    (if (potac=forall-sort-formula? conc)
	(cons (potac=compute-forall-sort-hyp (car terms) conc)
	      (potac=forall-sort*-prems (rest terms)
					(potac=compute-forall-sort (car terms) conc)))
      (omega~warn "~A is not universal quantified" conc)))
   (t (list conc))))

(defun potac=expand-foralle-sort* (concs prems params)
  (let ((newline (car prems))
	(term-list (car params)))
    (tacl~init (append concs prems))
    (do* ((rest term-list (cdr rest))
	  (term (car rest) (car rest)))
	((null rest) t)
      (let* ((premterm (potac=compute-forall-sort-hyp term (node~formula newline)))
	     (prem (first (remove-if-not #'(lambda (node)
					     (term~alpha-equal premterm (node~formula node)))
					 (rest prems)))))
	
	;;(format t "~%For PREMTERM: ~A found node: ~A" premterm prem)
	
	(setf newline
	      (car (if (null (rest rest))
		       (tacl~apply 'foralle-sort (list (car concs) newline prem) (list term))
		     (tacl~apply 'foralle-sort (list nil newline prem) (list term)))))))
    (tacl~end)))


(com~defcommand foralle-sort*
  (argnames univ-line line  term so-line)
  (argtypes ndline ndline term-list ndline-list)
  (arghelps "Universal line" "A line" "A list with terms" "A list with lines that are contain the sort for the terms")
  (function potac=foralle-sort*)
  (defaults potac=foralle-sort*-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Eliminate a sorted universal quantifier."))

(defun potac=foralle-sort* (univ-line line param so-line)
  (infer~compute-outline 'foralle-sort* (list line (cons univ-line so-line)) (list param )))

(defun potac=foralle-sort*-defaults (univ elim term so-line)
  (cond ((not (com~specified-arg-p univ))
	 (list (pds~find-support #'potac=forall-sort-formula?)
	       (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p elim))
	 (list univ (oc~nil-argument) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p term))
	 (list univ elim (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p so-line))
	 (list univ elim term (oc~nil-argument)))
	(t (list univ elim  term so-line))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foralli-sort*  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic foralli-sort*
		 (outline-mappings (((existent nonexistent) foralli-sort*-b)))
		 (parameter-types termsym-list)
	         (expansion-function potac=expand-foralli-sort*)
		 (passkey :formula)
		 (help "FORALL-SORT-Elimination."))

(defun foralli-sort*-b (conc prem parameters)
  (let ((all (reverse (potac=forall-sort*-prems (car parameters) (car conc)))))     ;;termsyms free in hyps!!!
    (values nil  (list all))))

(defun potac=expand-foralli-sort* (concs prems params)
  (let* ((old-hyps (set-difference (pdsn~hyps (car prems))
				   (pdsn~hyps (car concs))))
	 (forall-line (first concs))
	 (const-list (first params))
	 (last-const (first (last const-list))))
    (tacl~init (append concs prems))
    (do ((rest-consts const-list (rest rest-consts)))
	((null rest-consts) t)
      (let* ((head-const (first rest-consts))
	     (newline (tacl~apply 'foralli-sort (list forall-line nil) (list head-const)))
	     (new-hyp (third newline))
	     (old-hyp (car (mapcan #'(lambda (node) (when (data~equal (node~formula node)
								 (node~formula new-hyp))
						(list node))) old-hyps))))
	(setq forall-line (second newline))
	(tac~forget&destroy-hyp (list forall-line) old-hyp new-hyp)))
    (tacl~apply 'weaken (cons forall-line prems) nil))
  (tacl~end))

(com~defcommand foralli-sort*
  (argnames univ-line param line)
  (argtypes ndline termsym-list  ndline)
  (arghelps "Universal line to prove" "A list of parameters" "A line" )
  (function potac=foralli-sort*)
  (defaults potac=foralli-sort*-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Introduce a sorted universal quantifier."))

(defun potac=foralli-sort* (univ-line param  line)
  (infer~compute-outline 'foralli-sort* (list univ-line line) (list param )))

(defun potac=foralli-sort*-defaults (univ term line)
  (cond ((not (com~specified-arg-p univ))
	 (list (pds~find-open-node #'potac=forall-sort-formula?)
	       (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p term))
	 (list univ
	       (if univ (orules=generate-defaults-foralli univ
							  (pds~environment omega*current-proof-plan))
		  (oc~nil-argument))
	       (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list univ term (oc~nil-argument)))
	(t (list univ term line))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; existse-sort*  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic existse-sort*
		 (outline-mappings (((existent existent nonexistent) existse-sort*-b)))
		 (parameter-types termsym-list)
	         (expansion-function potac=expand-existse-sort*)
		 (passkey :formula)
		 (help "EXISTS-SORT*-Elimination."))


(defun existse-sort*-b (concs prems parameters)
  (when (potac=exists-sort-formula? (first prems))
    (let* ((conc (first concs))
	   (exsort-formula (first prems))
	   (consts (first parameters))
	   (hyps (potac=get-hyps-recursive exsort-formula consts))
	   (counter 0)
	   (marked-hyps (mapcar #'(lambda (hyp)
				    (list hyp (incf counter)))
				hyps)))
      (values nil 
	      (list (cons conc marked-hyps))))))

;; The direct combination with ANDE Steps is nt possible!
;;      (values (apply #'append (mapcar #'(lambda (marked-hyp)
;;					  (list (list (first (data~appl-arguments (first marked-hyp))) marked-hyp)
;;						(list (second (data~appl-arguments (first marked-hyp))) marked-hyp)))
;;				      marked-hyps))
;;	      (list (cons conc marked-hyps))))))


(defun potac=expand-existse-sort* (conclusions premises parameters)
  (let* ((conc-line (first conclusions))
	 (ex-line (first premises))
	 (subgoal-line (second premises))
	 (consts (first parameters)))
    (tacl~init (append conclusions premises))

    (let* ((new-subgoal-line (do* ((current-ex-line ex-line)
				   (current-conc-line conc-line)
				   (current-consts consts (rest consts)))
				 ((or (null current-consts)
				      (null (potac=exists-sort-formula? current-ex-line)))
				  current-conc-line)
			       (let* ((result-exsort (tacl~apply 'existse-sort
								 (list current-conc-line current-ex-line nil)
								 (list (first current-consts))))
				      (new-subgoal (third result-exsort))
				      (new-hyp (fourth result-exsort))
				      (old-hyp (first (remove-if-not #'(lambda (hyp-line)
									 (term~alpha-equal (node~formula hyp-line)
											   (node~formula new-hyp)))
								     (pdsn~hyps subgoal-line)))))
				 
				 (tac~forget&destroy-hyp (list new-subgoal) old-hyp new-hyp :test #'term~alpha-equal)
				 
				 (let* ((result-ander (tacl~apply 'ander (list nil old-hyp) nil))
					(new-ex-line (first result-ander)))
				   
				   (setf current-conc-line new-subgoal)
				   (setf current-ex-line new-ex-line))))))
      
      (tacl~apply 'weaken (list new-subgoal-line subgoal-line) nil)
      
      (tacl~end))))


	


(defun potac=get-hyps-recursive (formula consts)
  (if (potac=exists-sort-formula? formula)
      (if consts
	  (let* ((args (data~appl-arguments formula))
		 (abstr (first args))
		 (sort (second args))
		 (bound-variable (first (data~abstr-domain abstr)))
		 (range (data~abstr-range abstr))
		 (head-const (first consts))
		 (sort-term (term~appl-create sort (list head-const)))
		 (range-term (data~replace-struct range bound-variable head-const))
		 (new-hyp (term~appl-create (env~lookup-object 'and (th~env 'base))
					    (list sort-term range-term))))
	    (cons new-hyp (potac=get-hyps-recursive range-term (rest consts))))
	nil)
    nil))



       
(com~defcommand existse-sort*
  (argnames concline exline subgoal parameter)
  (argtypes ndline ndline ndline termsym-list)
  (arghelps "Conclusion Line." "An existentially quanitified line" "Subgoal Line." "Termsym List.")
  (function potac=existse-sort*)
  (frag-cats tactics post)
  (defaults potac=existse-sort*-defaults)
  (log-p T)
  (help "Apply a series of Exists-Sort-Elminations."))

(defun potac=existse-sort* (C exline P param)
  (infer~compute-outline 'existse-sort* (list C exline P) (list param)))

(defun potac=existse-sort*-defaults (conc-line ex-line subgoal-line consts)
  (cond ((not (com~specified-arg-p conc-line))
	 (list (oc~default-current-planline)
	       (com~unspecified)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p ex-line))
	 (list conc-line
	       (pds~find-support #'potac=exists-sort-formula?)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p subgoal-line))
	 (list conc-line 
	       ex-line
	       (oc~nil-argument)
	       (com~unspecified)))
	((not (com~specified-arg-p consts))
	 (list conc-line 
	       ex-line
	       subgoal-line
	       (batac=generate-defaults-existse* ex-line)))
	(t
	 (list conc-line ex-line subgoal-line consts))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application of rewrite rules (i.e. a formula of the form
;; (forall (lam (x aa) (forall (lam (y bb) .....
;;        (implies (and (in x Set1) (and (in y Set2) ....
;;                 (=  (quack x y ...) 
;;                     (ruelps y x ...) ....) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic rewrite-with
		 (outline-mappings (((existent list) rewrite-with-b)
				    ((existent nonexistent) rewrite-with-b)
				    ((nonexistent list) rewrite-with-f)))
		 (parameter-types position symbol symbol)
		 (expansion-function potac=expand-rewrite-with)
		 (passkey :formula)
		 (help "Apply a rewrite rule at a given position in a certain direction."))

(defun rewrite-with-b (conc prems parameters)
  (multiple-value-bind (newconc newprems)
      (potac=rew-apply (car conc)
		       (second parameters)
		       (first parameters)
		       (third parameters))
    (when newconc (values nil (remove-duplicates (cons newconc newprems) :test #'data~equal)))))

(defun rewrite-with-f (conc prems parameters)
  (multiple-value-bind (newconc newprems)
      (potac=rew-apply (car prems)
		       (second parameters)
		       (first parameters)
		       (third parameters))
    (when newconc (values (list newconc) (remove-duplicates newprems :test #'data~equal)))))


(defun potac=rew-decompose (ax &optional vars prems)
  (cond ((data~schema-p ax)
	 (let ((termcopy (data~alpha-copy (data~schema-range ax) nil)))
	   (potac=rew-decompose termcopy
				(append (term~type-variables-rec termcopy) vars) prems)))
	((logic~universal-quantification-p ax)
	 (potac=rew-decompose (logic~quantification-scope ax)
			   (cons (logic~quantification-bound-variable ax) vars)
			   (if (data~schema-equal (env~lookup-object :forall-sort (pds~environment omega*current-proof-plan))
						  (data~appl-function ax))
			       (cons (data~appl-create (second (data~appl-arguments ax))
						       (list (logic~quantification-bound-variable ax))) prems)
			     prems)))
	((logic~implication-p ax)
	 (potac=rew-decompose (second (data~appl-arguments ax))
			   vars
			   (if (logic~conjunction-p (car (data~appl-arguments ax)))
			       (append (batac=split-on-and (car (data~appl-arguments ax))) prems)
			     (cons (car (data~appl-arguments ax)) prems))))
	((or (logic~equality-p ax)
	     (logic~equivalence-p ax))
	 (values (car (data~appl-arguments ax)) (second (data~appl-arguments ax)) vars prems))
	(T nil)))

(defun potac=rew-apply (goal ax pos direction)
  (declare (edited  "24-JUN-2002")
	   (authors Pollet)
	   (input   "A goal formula, a theorem, a position, a direction.")
	   (effect  "-")
	   (value   "If the theorem can be applied the rewritten formula and"
		    "the premises of the theorem, else NIL."))
  (let ((term-to-rew (data~struct-at-position goal pos)))
    (multiple-value-bind (subst prems rhs lhs)
	(potac=rew-apply-term term-to-rew ax direction)
      (when subst
	(values (subst~apply subst (data~replace-at-position goal pos (if (string-equal direction "rl") lhs rhs)))
		(mapcar #'(lambda (pre) (subst~apply subst pre)) prems))))))


(defgeneric potac=rew-apply-term (term ax direction)
  (declare (edited  "24-JUN-2002")
	   (authors Pollet)
	   (input   "A term, a theorem, a direction.")
	   (effect  "-")
	   (value   "If the theorem can be applied to term,"
		    "a substitution, the premises and the lhs and rhs of the theorem,"
		    "else NIL."))
  (:method (term (ax symbol) direction)
	   (potac=rew-apply-term term (th~find-assumption ax (prob~proof-theory omega*current-proof-plan)) direction))
  (:method (term (ax prob+problem) direction)
	   (potac=rew-apply-term term (node~formula (prob~conclusion	ax)) direction))
  (:method ((term term+term) (ax term+term) direction)
	     (multiple-value-bind (rhs lhs vars prems)
		 (potac=rew-decompose ax)
	       (when (and lhs rhs)
		 (let ((subst  (term~alpha-match (if (string-equal direction "lr") lhs rhs) term)))
		   (when subst (values subst prems rhs lhs)))))))

(com~defcommand rewrite-with
  (argnames oldline newline axiom position direction)
  (argtypes ndline ndline thy-assumption position symbol)
  (arghelps "An open line to apply rewrite rule" "A premise to apply rewrite rule"
	    "The rewrite rule" "A position for the application" "A direction")
  (function potac=rewrite-with)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Rewriting with a given equality rule."))

(defun potac=rewrite-with (C P Ax Pos direction)
  (if (or (string-equal direction "lr")
	  (string-equal direction "rl"))
      (infer~compute-outline 'rewrite-with (list C P) (list pos (keim~name ax) direction))
    (omega~error "~A is not a valid direction, use 'lr' or 'rl'" direction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'MIZAR style' proof construction:
;; introduction of statements/formula into the proof
;; now with sorts!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic assert
		      (outline-mappings (((nonexistent list) assert-f)
					 ((list nonexistent) assert-b)
					 ((list list) assert-s)))
		      (parameter-types term  thy-ass-list)
		      (expansion-function potac=expand-assert)
		      (help ""))


(defun assert-f (conc prems parameters)
  (declare (ignore conc))
  (let ((para (list (car parameters))))
    (when (potac=assert-check para prems (second parameters))
      (values para nil ))))

(defun assert-b (conc prems parameters)
  (declare (ignore prems))
  (let ((para (list (car parameters))))
    (when (potac=assert-check conc  para (second parameters))
      (values nil para))))

(defun assert-s (conc prems parameters)
  (let ((para (list (car parameters))))
    (when (potac=assert-check conc (append para prems) (second parameters))
      (values nil para))))


(com~defcommand assert
  (argnames formula proof-lines defis)
  (argtypes term ndline-list thy-ass-list)
  (arghelps "A formula" "Depends on proof lines" "A list of definitions that should be expanded")
  (function potac=assert)
  (frag-cats tactics base)
  (defaults potac=assert-defaults)
  (log-p T)
  (help ""))

(defun potac=assert (term lines defis)
  (let* ((prems (remove-if #'pdsn~open-node-p lines))
	 (openlines (set-difference lines prems)))
    (infer~compute-outline 'assert (list openlines prems) (list term defis))))

(defun potac=assert-defaults (form nodes defis)
  (cond ((not (com~specified-arg-p form))
	 (list (node~formula (oc~default-current-planline)) (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p nodes))
	 (list form (pds~support-nodes omega*current-proof-plan) (com~unspecified)))
	((not (com~specified-arg-p defis))
	 (list form nodes (when form (remove-if #'(lambda (defi)
						    (or (eq defi (th~find-assumption 'forall-sort (prob~theory omega*current-proof-plan)))
							(eq defi (th~find-assumption 'exists-sort (prob~theory omega*current-proof-plan)))))
						     (orules=contained-definitions form)))))
	(t (list form nodes defis))))

(defun potac=assert-sort-thms-for (nodes)
  (let*	 ((thy (prob~proof-theory omega*current-proof-plan))
	  (senv (th~senv thy))
	  (sort-preds (mapcan #'(lambda (node)
			     (remove-if-not #'(lambda (sub) (and (term~constant-p sub)
								 (sort~sort-of-pred sub senv)))
					    (data~all-substructs (node~formula node))))
			     nodes)))
	  (mapcan #'(lambda (pred)
		      (mapcar #'(lambda (td)
				   (th~find-assumption (keim::sort~td-theorem td) thy))
			      (sort~all-term-decls (sort~sort-of-pred pred senv) senv))) sort-preds)))
  

(defun potac=assert-check (conc prems defis &key (time 10)) 
  (let* ((new-name (intern (string-upcase (format nil "problem-~A"  (gentemp 'temp)))))
	 (pds omega*current-proof-plan)
	 (thy (prob~proof-theory pds))
	 (notexpand (potac=use-defis defis))     
	 (pds-env (pds~environment pds))
	 (pds-problem (prob~proof-problem pds))
	 (pds-problem-env (prob~environment pds-problem))
	 (new-env (env~create (list pds-problem-env)))
	 (new-assumptions (mapcar #'(lambda (supp)  ;;ass from the current proof
					      (node~create
					       (gentemp 'a0) 
					       (potac=num2func (gentac=substitute-defis supp notexpand))
					       (just~create (infer~find-method 'hyp) nil)))
					  prems))
	 (new-conclusion (node~create 'c
				      (potac=num2func (gentac=substitute-defis
						       (if (consp conc) (batac=assemble-conjunction conc)  conc)
						       notexpand))
				      (just~create (infer~find-method 'open) nil)))
	 (sort-thms (mapcar #'(lambda (thm)  ;;sort-thms 
				(let ((thmform  (th~ass-formula  thm)))
				  (node~create
				   (gentemp 't0) 
				   (gentac=substitute-defis thmform notexpand)
				   (just~create (infer~find-method 'hyp) nil))))
			    (potac=assert-sort-thms-for (cons new-conclusion new-assumptions))))
	 (new-problem (prob~create new-name (prob~theory pds) new-env
				   (append sort-thms new-assumptions) 
				    new-conclusion))                  
	 (all-type-vars (append (env~class-keys pds-env 'type+variable nil)
				(env~class-keys pds-problem-env 'type+variable nil)))
	 (all-type-constants (append (env~class-keys pds-env 'type+constant nil)
				     (env~class-keys pds-problem-env 'type+constant nil)))
	 (all-constants (append (env~class-keys pds-env 'term+constant nil)
				(env~class-keys pds-problem-env 'term+constant nil)))
	 (all-variables (append (env~class-keys pds-env 'term+variable nil)
				(env~class-keys pds-problem-env 'term+variable nil))))
    (mapcar #'(lambda (key)
		(let* ((obj (env~lookup-object key pds-env)))

		  (env~enter key obj new-env)))
	    (append all-type-vars
		    all-type-constants
		    all-constants
		    all-variables))

    (let ((new-pds (pds~start-proof-plan new-problem (ot~new-proof-plan-name new-problem))))
      (setf omega*current-proof-plan new-pds     
	    keim::pds*current-proof-plan new-pds)

    (setf testpds new-pds)    

    (let (
          (result '(spass~call-spass new-conclusion
                      new-pds
                      (atptop~default-directory)
                      time
                      T
                      0
                      nil))
;          (result (otter~call-otter new-conclusion  ;;for testing
;                      new-pds
;                      (atptop~default-directory)
;                      time
;                      'AUTO
;                      nil
;                      nil
;                      "" ""))
	  )
      (setf 
       keim::pds*current-proof-plan pds
       omega*current-proof-plan pds)
      result))))

(defun potac=use-defis (defis)
  (set-difference
       (th~definitions-recursed  (prob~theory omega*current-proof-plan))
       (cons (th~find-assumption 'forall-sort (prob~theory omega*current-proof-plan))
	     (cons (th~find-assumption 'exists-sort (prob~theory omega*current-proof-plan))
		   defis))))
	 
(defun potac=expand-assert (conclusions premises parameters)
  (tacl~init (append conclusions premises))
  (let* ((notexpand  (potac=use-defis  (second parameters)))
	 (prems (mapcar #'(lambda (prem)
			    (if (gentac=substituted-differs-p (node~formula prem) notexpand)
				(car (tacl~apply 'defse (list nil prem) (list notexpand)))
			      prem))
			premises)) ;potac=expand-numbers  ;already included in defse
	 (conc (cond ((and (consp conclusions) (> (length conclusions) 1))
		       (car (second (tacl~apply 'ande* (list conclusions nil) nil))))
		      ((consp conclusions)
		       (car conclusions))
		      (T conclusions)))  ;potac=expand-numbers ;already included in defsi
	 (node (if (gentac=substituted-differs-p (node~formula conc) notexpand)
		   (second (tacl~apply 'defsi
				       (list conc nil)
				       (list notexpand)))
		 conc))
	 (thmnodes   (mapcar #'(lambda (thm) (pds~add-thy-assertion thm omega*current-proof-plan))
			     (potac=assert-sort-thms-for (cons node prems))))
	 (thmprems  (mapcar #'(lambda (prem)
			    (if (gentac=substituted-differs-p (node~formula prem) notexpand)
				(car (tacl~apply 'defse (list nil prem) (list notexpand)))
			      prem))
			thmnodes))
	 )
    (setf (pds~node-supports node) (append thmprems prems))
      
    (setf (just~method (node~justification node)) (infer~find-method 'otter))  ;; replaced SPASS by OTTER AMEIER
    (setf (just~premises (node~justification node)) (append thmprems prems))
    (setf (pdsj~parameters (node~justification node)) (list t))
    (tacl~end)
      
    (setf (pdsj~status (node~justification node)) "untested")))

(defun potac=num2func (node)
  (if (th~find-theory 'natural) (natac=numbers-2-function node)  node))

#| expand every defi except =, this is not always useful
(defun potac=expand-assert (conclusions premises parameters)
  (declare (ignore parameters))
  (tacl~init (append conclusions premises))
  (let* ((notexpand  (list (th~find-assumption '= 'base)))
	 (prems (mapcar #'(lambda (prem)
			    (if (gentac=substituted-differs-p (node~formula prem) notexpand)
				(car (tacl~apply 'defse (list nil prem) (list notexpand)))
			      prem))
			premises))
	 (conc (cond ((and (consp conclusions) (> (length conclusions) 1))
		      (car (second (tacl~apply 'ande* (list conclusions nil) nil))))
		     ((consp conclusions)
		      (car conclusions))
		     (T conclusions)))
	 (node (if (gentac=substituted-differs-p (node~formula conc) notexpand)
		   (second (tacl~apply 'defsi
				       (list conc nil)
				       (list notexpand)))
		 conc)))
    (setf (pds~node-supports node) prems)
      
    (setf (just~method (node~justification node)) (infer~find-method 'spass))
    (setf (just~premises (node~justification node)) prems)
    (setf (pdsj~parameters (node~justification node)) (list t))
    (tacl~end)
      
    (setf (pdsj~status (node~justification node)) "untested")))

(defun potac=assert-check (conc prems &key (time 10)) 
  (let* ((new-name (intern (string-upcase (format nil "problem-~A"  (gentemp 'temp)))))
	 (notexpand (list (th~find-assumption '= 'base)))
	 (pds omega*current-proof-plan)
	 (pds-env (pds~environment pds))
	 (pds-problem (prob~proof-problem pds))
	 (pds-problem-env (prob~environment pds-problem))
	 (new-env (env~create (list pds-problem-env)))
	 (new-assumptions (mapcar #'(lambda (supp)
				      (node~create
				       (gentemp 'a0) 
				       (gentac=substitute-defis supp notexpand)
				       (just~create (infer~find-method 'hyp) nil)))
				  prems))
	 (new-conclusion (node~create 'c
				      (gentac=substitute-defis
				       (if (consp conc) (batac=assemble-conjunction conc)  conc)
				       notexpand)
				      (just~create (infer~find-method 'spass) new-assumptions)))
	 (new-problem (prob~create new-name (prob~theory pds) new-env new-assumptions new-conclusion))
	 (all-type-vars (append (env~class-keys pds-env 'type+variable nil)
				(env~class-keys pds-problem-env 'type+variable nil)))
	 (all-type-constants (append (env~class-keys pds-env 'type+constant nil)
				     (env~class-keys pds-problem-env 'type+constant nil)))
	 (all-constants (append (env~class-keys pds-env 'term+constant nil)
				(env~class-keys pds-problem-env 'term+constant nil)))
	 (all-variables (append (env~class-keys pds-env 'term+variable nil)
				(env~class-keys pds-problem-env 'term+variable nil))))
    (mapcar #'(lambda (key)
		(let* ((obj (env~lookup-object key pds-env)))

		  (env~enter key obj new-env)))
	    (append all-type-vars
		    all-type-constants
		    all-constants
		    all-variables))
    
    (setf test new-problem)    (setf co conc)    (setf pre prems)

    (let ((result (spass~call-spass new-conclusion
		      (pds~start-proof-plan new-problem (ot~new-proof-plan-name new-problem))
		      (atptop~default-directory)
		      time
		      T
		      0
		      nil))
	  (result1 '(otter~call-otter new-conclusion  ;;for testing
		      (pds~start-proof-plan new-problem (ot~new-proof-plan-name new-problem))
		      (atptop~default-directory)
		      time
		      'AUTO
		      nil
		      nil
		      "" "")))
      (setf 
       keim::pds*current-proof-plan
       omega*current-proof-plan)
      result)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; existse-sort*-special  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;better don't ask: the disjuncts are introduced as hyps of the new premise
;;in the expansion these hyps are converted to the result of andel/ander of the real hyps
;;and the dummy hyps have to replaced in the hyp-lists of the corresponding nodes.
;;Expansion of hypotheses!
;;
;; We have the following situation :
;;
;; ExistingPrem         Hyp_1 ... Hyp_n |- NewPrem
;; -----------------------------------
;; ExistingConc         Hyp_1 |- NewConc_1 ... Hyp_n |- NewConc_n
;;
;;
;; What would be cleaner?
;;1. Allow hyps for conclusions in wild-tactics 
;;2. I have the feeling that the plain execution would be much easier for simple tactics
;;   like this one. Additionally, some kind of inverse expansion after execution could restore
;;   it as more abstract inference step.

(infer~defwild-tactic existse-sort*-special
		 (outline-mappings (((existent list ) existse-sort*-special-b)))
		 (parameter-types termsym-list)
	         (expansion-function potac=existse-sort*-special-expand)
		 (help "Iterated EXISTS-SORT-Elimination."))


(defun existse-sort*-special-b (concs prems parameters)
  (let* ((ex-term (car prems))
	 (new-hyps (potac=compute-existse-sort*-special-sorts ex-term (car parameters) nil)))
      (when (> (length new-hyps 1))
	(values nil  (list (cons (car concs) new-hyps))))))

(defun potac=compute-existse-sort*-special-sorts (ex-term  params hyps)
  (if (and (potac=exists-sort-formula? ex-term) (consp params))
      (let* ((new-hyp (potac=compute-exists-sort-sort (car params) ex-term))
	     (new-ex (potac=compute-exists-sort (car params) ex-term)))
	(potac=compute-existse-sort*-special-sorts new-ex (rest params) (cons new-hyp hyps)))
    (cons ex-term hyps)))

(com~defcommand existse-sort*-special
  (argnames ex-line line param)
  (argtypes ndline ndline termsym-list)
  (arghelps "An existential line" "A line to be proved" "A list of new constants")
  (function potac=existse-sort*-special)
  (defaults potac=existse-sort*-special-defaults)
  (frag-cats tactics post) 
  (log-p T)
  (help "Iterated elimination of a sorted existential quantifier."))

(defun potac=existse-sort*-special (ex-line line params)
  (infer~compute-outline 'existse-sort*-special (list line (list ex-line)) (list params)))


(defun potac=existse-sort*-special-defaults (ex-line line param)
    (cond ((not (com~specified-arg-p ex-line))
           (list (pds~find-support #'potac=exists-sort-formula?) 
                 (com~unspecified)
		 (com~unspecified)))
          ((not (com~specified-arg-p line))
           (list ex-line
		 (oc~default-current-planline)
		 (com~unspecified)))
	  ((not (com~specified-arg-p param))
	   (list ex-line
		 line
                 (batac=generate-defaults-existse* ex-line)))
          (t (list ex-line line param))))

(defun potac=existse-sort*-special-expand (concs prems parameters)
    (let* ((conc-line  (car concs))
	   (exi-line (car prems))
	   (prem-line (second prems))
	   (const-list (first parameters))
	   (hyps (set-difference (pdsn~hyps prem-line) (pdsn~hyps conc-line))))
      (tacl~init (append concs prems))
      (potac=existse-sort*-special-expand-rec conc-line exi-line  prem-line hyps const-list)
      (tacl~end)))

(defun potac=existse-sort*-special-expand-rec (conc exi-prem prem hyps params &optional last-hyp)
  (if (and (potac=exists-sort-formula? exi-prem)
	   (consp params))
      (let* ((exe (tacl~apply 'existse-sort (list conc exi-prem nil) (list (first params))))
	     (newhyp (fourth exe))
	     (oldhyp (potac=hyp2open (find-if #'(lambda (hy) (data~equal (node~formula hy)
									 (car (data~appl-arguments (node~formula newhyp)))))
				     hyps) (list newhyp)))
	     (ander (tacl~apply 'ander (list nil newhyp) nil))
	     (andel (tacl~apply 'andel (list oldhyp newhyp) nil)))
	(potac=existse-sort*-special-expand-rec (third exe) (first ander) prem (remove oldhyp hyps) (rest params)  newhyp))
    (let ((oldhyp (potac=hyp2open (find-if #'(lambda (hy) (data~equal (node~formula hy) (node~formula  exi-prem))) hyps)
				  (list last-hyp))))
      (tacl~apply 'weaken (list oldhyp exi-prem) nil)
      (setf (pdsn~hyps prem)(pdsn~hyps conc))
      (tacl~apply 'weaken (list conc prem) nil))))

(defun potac=hyp2open (hyp newhyps)
  (setf (pdsn~hyps hyp) newhyps)
  (setf (node~justification hyp)(pdsj~open-just-create))
  (setf bla hyp)
  hyp)
