;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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


(eval-when (load  eval)
(com~defcategory forward
  (help "Forward application of nd rules"))

(com~defcategory backward
  (help "Backward application of nd rules"))

(com~defcategory introduction
  (help "Application of introduction rules"))

(com~defcategory elimination
  (help "Application of elimination rules"))

(com~defcategory propositional
  (help "Application of propositional rules"))

(com~defcategory quantifier
  (help "Application of quantifier rules"))

(com~defcategory lambda
  (help "Application of lambda expression rules"))

(com~defcategory structural
  (help "Application of structural rules"))
)

;;; RULES DefnE, DefnI (expansion and contraction of definitions) take definitions
;;; from the theory. They are handled as pure rewrite rules, i.e. none of the
;;; definitions themselves will show up in hypotheses or supports.

;;; In contrast to that, it is also possible to explicitly import definitions
;;; into the problem (by import-ass). One can use such lines like ordinary
;;; definitions by the RULES AsDefnE and AsDefnI.
;;; As such lines are not guaranteed to stem from the theory, but could as well
;;; have been included by the user, this time their use is tracked in
;;; hypotheses and supports.

;;; NOTE: the names of the respective COMMANDS are called
;;; defn-expand, defn-contract, defn-expand-line and defn-contract-line

(infer~defrule "DefnE"
               (outline-mappings (((closed closed) defne-c)
				  ((existent existent) defne-a)
				  ((nonexistent existent) defne-f)))
	       (parameter-types term term position)
               (help "Definiendum elimination using its theory definition"))

;;; DEF: in den folgenden Funktionen sind bewusst die Typ-Variablen von D und B
;;;      verschieden gewaehlt worden. Sie muessen ggf. in einer Sidecondition
;;;      auf Alpha-equality getestet werden (ist aber nicht noetig, sollte
;;;      bei korrekter Implementation von Definitionen immer stimmen.
;;;      (Dasselbe auch nei defni)

(rule~defrule defne-c defne (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (sideconditions
   (pds~term-equal-defn-expansion-p A D B F L))
  (description "checking definition expansion"))

(rule~defrule defne-a defne (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (sideconditions
   (pds~term-equal-defn-expansion-p A D B F L))
  (description "checking definition expansion"))

(rule~defrule defne-f defne (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (computations
   (F (pds~replace-and-contract-at-position D B A L)))
  (actions ((support P) (sponsor C) (unsponsor P)))
  (description "Eliminate a definition in a line."))

(com~defcommand defn-expand
  (argnames line definition position)
  (argtypes ndline thy-assumption position)
  (arghelps "Line to be rewritten" "Definition to be expanded" "Position of occurrence")
  (function orules=defn-expand)
  (frag-cats rules elimination forward definition)
  (defaults orules=defn-expand-defaults)
  (log-p T)
  (level 5)
  (help "Expand a definition in a line."))

(defun orules=defn-expand (line definition pos)
  (if (th~definition-p definition)
      (let ((definiendum (th~definition-constant definition))
	    (definiens (th~ass-node definition)))
	(if (data~schema-equal (data~struct-at-position (node~formula line) pos) definiendum)
	    (infer~compute-outline 'defne (list nil line) (list definiendum definiens pos))
	  (error "~A does not appear at given position" definiendum)))
    (error "~A is not a definition" (keim~name definition))))


(defun orules=defn-expand-defaults (line definition pos)
  (cond ((not (com~specified-arg-p line))
	 (list (pds~find-support #'orules=contained-definition) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p definition))
	 (list line (orules=contained-definition (node~formula line)) (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (if definition 
	     (list line definition
		   (let* ((definition (th~definition-constant definition))
			  (definition (if (Data~schema-p definition)
					  (data~schema-range definition)
					definition)))
		     (car
		      (data~substruct-positions 
		       definition
		       (node~formula line)))))
	   (list line definition (com~unspecified))))
	(t (list line definition pos))))


(infer~defrule "DefnI"
               (outline-mappings (((closed closed) defni-c)
				  ((existent existent) defni-a)
				  ((existent nonexistent) defni-b)))
	       (parameter-types term term position)
               (help "Definiendum introduction using its theory definition"))

(rule~defrule defni-c defni (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (sideconditions
   (pds~term-equal-defn-expansion-p F D B A L))
  (description "checking definition contraction"))
   
(rule~defrule defni-a defni (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (sideconditions
   (pds~term-equal-defn-expansion-p F D B A L))
  (description "checking definition contraction"))
   
(rule~defrule defni-b defni (in generic)
  (parameters (D term+schematic "the definiendum")
	      (B term+schematic "the definiens")
	      (L pos+position "the position"))
  (declarations
   (type-variables ee ff)
   (meta-variables (A O) (F O) (D ee) (B ff)))
  (conclusion (C () F))
  (premises (P () A))
  (computations
   (A (pds~replace-and-contract-at-position D B F L)))
  (description "Introduce a definition into a line."))

(com~defcommand defn-contract
  (argnames line definition pos)
  (argtypes ndline thy-assumption position)
  (arghelps "Line to be rewritten" "Definition to be contracted" "Position of occurrence")
  (function orules=defn-contract)
  (frag-cats rules introduction backward definition)
  (defaults orules=defn-contract-defaults)
  (log-p T)
  (level 5)
  (help "Contract a definition in a line."))

(defun orules=defn-contract-defaults (line definition pos)
  (cond ((not (com~specified-arg-p line))
	 (list (pds~find-open-node #'orules=contained-definition) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p definition))
	 (list line (orules=contained-definition (node~formula line)) (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (if definition 
	     (list line definition
		   (let* ((definition (th~definition-constant definition))
			  (definition (if (Data~schema-p definition)
					  (data~schema-range definition)
					definition)))
		     (car
		      (data~substruct-positions 
		       definition
		       (node~formula line)))))
	   (list line definition (com~unspecified))))
	(t (list line definition pos))))


(defun orules=contained-definition (formula)
  (declare (edited  "17-APR-1998")
	   (authors Chris)
	   (input   "A formula")
	   (effect  "None")
	   (value   "The definition of the first defined concept within term"))
  (let* ((defs (th~definitions-recursed (prob~theory omega*current-proof-plan)))
	 (poslist (data~positions formula #'(lambda (x) (term~constant-p x))))
	 (constants (mapcar #'(lambda (pos) (data~struct-at-position formula pos))
			    poslist)))
    (find 
     (find-if #'(lambda (const) (find-if #'(lambda (def) (keim~equal (keim~name const)
								     (keim~name def)))
					 defs))
	      constants)
     defs :test #'(lambda (x y) (keim~equal (keim~name x) (keim~name y))))))


(defun orules=contained-definitions (formula)
  (declare (edited  "17-APR-1998")
	   (authors Chris)
	   (input   "A formula")
	   (effect  "None")
	   (value   "All defined concept within term"))
  (let* ((defs (th~definitions-recursed (prob~theory omega*current-proof-plan)))
	 (poslist (data~positions formula #'(lambda (x) (term~constant-p x))))
	 (constants (mapcar #'(lambda (pos) (data~struct-at-position formula pos))
			    poslist)))
    (remove-duplicates
     (mapcan #'(lambda (def)
		 (when (find-if #'(lambda (const)
				    (keim~equal (keim~name const)
						(keim~name def)))
				constants)
		   (list def)))
	     defs))))


		     
;;(defun orules=contained-simplifier (formula)
;;  (declare (edited  "17-APR-1998")
;;           (authors Chris)
;;           (input   "A formula")
;;           (effect  "None")
;;           (value   "The simplifier of the first defined concept within term"))
;;  (let* ((defs (th~simplifiers-recursed (prob~theory omega*current-proof-plan)))
;;         (poslist (data~positions formula #'(lambda (x) (term~constant-p x))))
;;         (constants (mapcar #'(lambda (pos) (data~struct-at-position formula pos))
;;                            poslist)))
;;    (find 
;;     (find-if #'(lambda (const) (find-if #'(lambda (def) (keim~equal (keim~name const)
;;                                                                     (keim~name def)))
;;                                         defs))
;;              constants)
;;     defs :test #'(lambda (x y) (keim~equal (keim~name x) (keim~name y))))))

(defun orules=defn-contract (line definition pos)
  (if (th~definition-p definition)
      (let ((definiendum (th~definition-constant definition))
	    (definiens (th~ass-node definition)))
	(if (data~schema-equal (data~struct-at-position (node~formula line) pos) definiendum)
	    (infer~compute-outline 'defni (list line nil) (list definiendum definiens pos))
	  (error "~A does not appear at given position" definiendum)))
    (error "~A is not a definition" (keim~name definition))))

;;; OLD stuff --- kept for historical reasons  VS.

;(infer~defrule "AsDefnE"
;               (outline-mappings (((closed closed closed) asdefne-c)
;                                  ((nonexistent closed existent) asdefne-f)))
;               (help "Definiendum elimination using a node as definition"))
;

; da sich das Definitionsformat geaendert hat, geht Importieren als
; Lines nicht mehr. DEF

;(rule~defrule asdefne-c asdefne (in generic)
;  (declarations
;   (type-variables ee) 
;   (meta-variables (A O) (F O) (D ee) (B ee)))
;  (conclusion (C () F))
;  (premises (P () A)
;            (D () (=def D B)))
;  (sideconditions
;   (pds~term-equal-defn-expansion-p F D B A))
;  (description "checking definition expansion"))
;   
;(rule~defrule asdefne-f asdefne (in generic)
;  (declarations
;   (type-variables ee) 
;   (meta-variables (A O) (F O) (D ee) (B ee)))
;  (conclusion (C () F))
;  (premises (P1 () A)
;            (P2 () (=def D B)))
;  (sideconditions
;   (term~primitive-p D))
;  (computations
;   (F (pds~replace-and-contract D B A)))
;  (actions ((support P1 P2) (sponsor C) (unsponsor P1)))
;  (description "Eliminate a definition in a line."))
;   
;(com~defcommand defn-expand-line
;  (argnames line defline)
;  (argtypes ndline ndline)
;  (arghelps "Line to be rewritten" "Line to be used as definition")
;  (function orules=defn-expand-line)
;  (frag-cats rules elimination)
;  (defaults ((com~unspecified)(com~unspecified)))
;  (log-p T)
;  (help "Expand a definition in a line."))
;
;(defun orules=defn-expand-line (line definition)
;  (let ((def (node~formula definition)))
;    (if (and (appl~p def)
;                                        ;            (data~equal (appl~function def) '=)
;             (term~constant-p (car (data~appl-arguments def))))
;        (let* ((args (data~appl-arguments def))
;               (definiendum (car args))
;               (definiens (cadr args)))
;          (infer~compute-outline 'asdefne (list nil definition line) nil))
;      (error "~A is not a definition" (keim~name definition)))))



; s.o. DEF

;(infer~defrule "AsDefnI"
;               (outline-mappings (((closed closed closed) asdefni-c)
;                                  ((existent closed nonexistent) asdefni-b)))
;               (help "definition expansion"))
;
;(rule~defrule asdefni-c asdefni (in generic)
;  (declarations
;   (type-variables ee)
;   (meta-variables (A O) (F O) (D ee) (B ee)))
;  (conclusion (C () A))
;  (premises (P1 () F)
;            (P2 () (=def D B)))
;  (sideconditions
;   (pds~term-equal-defn-expansion-p F D B A))
;  (description "checking definition contraction"))
;   
;
;(rule~defrule asdefni-b asdefni (in generic)
;  (declarations
;   (type-variables ee)
;   (meta-variables (A O) (F O) (D ee) (B ee)))
;  (conclusion (C () A))
;  (premises (P1 () F)
;            (P2 () (=def D B)))
;  (sideconditions
;   (term~primitive-p D)
;   (pds~node-supported-by-p C P2))
;  (computations
;   (F (pds~replace-and-contract D B A)))
;  (description "Definition contraction"))
;
;(com~defcommand defn-contract-line
;  (argnames line defline)
;  (argtypes ndline ndline)
;  (arghelps "Line to be rewritten" "Equation line to be expanded as definition")
;  (function orules=defn-contract-line)
;  (frag-cats rules elimination)
;  (defaults ((oc~default-current-planline) (com~unspecified)))
;  (log-p T)
;  (help "Contract a definition in a line."))
;
;(defun orules=defn-contract-line (line definition)
;  (let ((def (node~formula definition)))
;    (if (and (appl~p def)
;                                        ;            (data~equal (appl~function def) '=)
;             (term~constant-p (car (data~appl-arguments def))))
;        (let* ((args (data~appl-arguments def))
;               (definiendum (car args))
;               (definiens (cadr args)))
;          (infer~compute-outline 'asdefni (list line definition nil) nil))
;      (error "~A is not a definition" (keim~name definition)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defn-contract-local-def
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule defn-contract-local-def
	       (outline-mappings (((existent nonexistent existent) defn-contract-local-def-b)
				  ((nonexistent existent existent) defn-contract-local-def-f)
				  ((existent existent existent) defn-contract-local-def-a)
				  ((closed closed closed) defn-contract-local-def-c)))
	       (parameter-types position)
	       (help "Introduces a local definition."))

(rule~defrule defn-contract-local-def-b defn-contract-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (computations (G (pds~replace-and-contract-at-position A B F POS)))
  (sideconditions (orules=def-p P2)
		  (orules=definiendum-at-position-p F A POS))
  (description "Backward application of defn-contract-local-def."))

(rule~defrule defn-contract-local-def-f defn-contract-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (computations (F (pds~replace-and-contract-at-position B A G POS)))
  (sideconditions (orules=def-p P2)
		  (orules=definiens-at-position-p G B POS))
  (description "Forward application of defn-contract-local-def."))
  
(rule~defrule defn-contract-local-def-a defn-contract-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (sideconditions (orules=def-p P2)
		  (pds~term-equal-defn-expansion-p F A B G POS)) 
  (description "Sideward application of defn-contract-local-def."))

(rule~defrule defn-contract-local-def-c defn-contract-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (sideconditions (orules=def-p P2)
		  (pds~term-equal-defn-expansion-p F A B G POS)) 
  (description "Check application of defn-contract-local-def."))

(defun orules=def-p (node)
  (let* ((just (node~justification node))
	 (method (just~method just)))
    (string-equal (keim~name method)
		  (keim~name (infer~find-method 'local-def)))))

(defun orules=definiendum-at-position-p (term definiendum pos)
  (let* ((term-at-pos (data~struct-at-position term pos)))
    (data~equal term-at-pos definiendum)))

(defun orules=definiens-at-position-p (term definiens pos)
  (let* ((term-at-pos (data~struct-at-position term pos)))
    (data~equal term-at-pos definiens)))


(com~defcommand defn-contract-local-def
  (argnames definiendum-line definiens-line definition pos)
  (argtypes ndline ndline ndline position)
  (arghelps "Line with definiendum" "Line with definiens" "Definition to be contracted" "Position of occurrence")
  (function orules=contract-local-def)
  (frag-cats rules introduction backward definition)
  (defaults nil nil nil nil) 
  (log-p T)
  (level 5)
  (help "Contract a local definition in a line."))

(defun orules=contract-local-def (definiendum-line definiens-line definition pos)
  (infer~compute-outline 'defn-contract-local-def (list definiendum-line definiens-line definition) (list pos)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defn-expand-local-def
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule defn-expand-local-def
	       (outline-mappings (((nonexistent existent existent) defn-expand-local-def-f)
				  ((existent nonexistent existent) defn-expand-local-def-b)
				  ((existent existent existent) defn-expand-local-def-a)
				  ((closed closed closed) defn-expand-local-def-c)))
	       (parameter-types position)
	       (help "Expands a local definition."))

(rule~defrule defn-expand-local-def-f defn-expand-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (computations (F (pds~replace-and-contract-at-position A B G POS)))
  (sideconditions (orules=def-p P2)
		  (orules=definiendum-at-position-p G A POS))
  (description "Forward application of defn-expand-local-def."))

(rule~defrule defn-expand-local-def-b defn-expand-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (computations (G (pds~replace-and-contract-at-position B A F POS)))
  (sideconditions (orules=def-p P2)
		  (orules=definiens-at-position-p F B POS))
  (description "Backward application of defn-expand-local-def."))

(rule~defrule defn-expand-local-def-a defn-expand-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (sideconditions (orules=def-p P2)
		  (pds~term-equal-defn-expansion-p G A B F POS))
  (description "Sideward application of defn-expand-local-def."))

(rule~defrule defn-expand-local-def-c defn-expand-local-def
  (in generic)
  (parameters (POS pos+position "the position"))
  (declarations
   (type-variables ee)
   (meta-variables (A ee) (B ee) (F o) (G o))
   )
  (premises
   (P1 () G)
   (P2 () (=def A B)))
  (conclusion (C () F))
  (sideconditions (orules=def-p P2)
		  (pds~term-equal-defn-expansion-p G A B F POS))
  (description "Check application of defn-expand-local-def."))

(com~defcommand defn-expand-local-def
  (argnames definiens-line definiendum-line definition pos)
  (argtypes ndline ndline ndline position)
  (arghelps "Line with definiens" "Line with definiendum" "Definition to be expanded" "Position of occurrence")
  (function orules=expand-local-def)
  (frag-cats rules introduction forward definition)
  (defaults nil nil nil nil) 
  (log-p T)
  (level 5)
  (help "Expands a local definition in a line."))

(defun orules=expand-local-def (definiens-line definiendum-line definition pos)
  (infer~compute-outline 'defn-expand-local-def (list definiens-line definiendum-line definition) (list pos)))
