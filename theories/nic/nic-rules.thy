;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

;; The following set of rules belong to the natural deduction calculus for intercalation
;; as presented in the PhD thesis of John Byrnes (Carnegie Mellon University, 1999).
;; In John's thesis they are presented and commented in chapter 4.1 (pages 93 ff.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;             NIC Rules  for Predicate Logic              ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-IMP-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-IMP-I
	       (outline-mappings (((closed closed) NIC-IMP-I-c)
				  ((existent nonexistent) NIC-IMP-I-b)))
	       (help "The IMPLIES-Introduction rule."))

(rule~defrule NIC-IMP-I-c NIC-IMP-I (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (description "Checking an application of the NIC implication introduction rule."))

(rule~defrule NIC-IMP-I-b NIC-IMP-I (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "Reducing the proof of an NIC implication to the proof of succedent using"
	       "the antecedent as an additional hypothesis."))

(com~defcommand NIC-IMP-I
  (argnames implication)
  (argtypes ndline)
  (arghelps "Implication to justify")
  (function nic=NIC-IMP-I)
  (frag-cats rules structural backward introduction propositional)
  (log-p T)
  (help "NIC Implication Introduction."))

(defun nic=NIC-IMP-I (imp)
  (infer~compute-outline 'NIC-IMP-I (list imp nil) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-IMP-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-IMP-E
	       (outline-mappings (((closed closed closed) NIC-IMP-E-c)
				  ((existent existent existent) NIC-IMP-E-a)
				  ))
	       (help "The NIC implication elimination rule."))

;; Remark: Only ...-a and ...-c versions are needed since for interactive and proof search
;; purposes the NIC tactic NICTAC-IMP-E should be employed.

(rule~defrule NIC-IMP-E-c NIC-IMP-E (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (description "Checking an application of the NIC implication elimination rule."))

(rule~defrule NIC-IMP-E-a NIC-IMP-E (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () (implies A B))
	    (P2 () A))
  (sideconditions
   (pds~node-supported-by-p C P2 P1))
  (description "Application of the NIC implication elimination rule."))

(com~defcommand NIC-IMP-E
  (argnames  succedent implication antecedent)
  (argtypes ndline ndline ndline)
  (arghelps  "Succedent of the implication" "Implication" "Antecedent of the implication")
  (function nic=implies-e)
  (frag-cats rules propositional elimination forward backward)
  (log-p T)
  (help "Use NIC implication elimination on two support lines."))
  
(defun nic=implies-e (succ imp ante)
  (infer~compute-outline 'NIC-IMP-E (list succ ante imp) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-AND-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-I
	       (outline-mappings (((closed closed closed) NIC-AND-I-c)
				  ((existent existent existent) NIC-AND-I-a)
				  ((existent nonexistent nonexistent) NIC-AND-I-b)
				  ((nonexistent existent existent) NIC-AND-I-f)
				  ((existent existent nonexistent) NIC-AND-I-ls)
				  ((existent nonexistent existent) NIC-AND-I-rs)
				  ))
	       (help "The AND-Introduction."))

(rule~defrule NIC-AND-I-c  NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Checking NIC conjunction introduction."))

(rule~defrule NIC-AND-I-a NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1 L2))
  (description "Justifying a NIC conjunction by its conjuncts."))

(rule~defrule NIC-AND-I-b NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O))) 
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Reducing the proof of a NIC conjunction to the proof of its conjuncts."))

(rule~defrule NIC-AND-I-f NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
            (L2 () B))
  (actions
   ((support L1 L2) (sponsor L3) (unsponsor L1 L2)))
  (description "Deducing a NIC conjunction from two formulae."))

(rule~defrule NIC-AND-I-ls NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1))
  (description "Given the left conjunct, reducing the proof of a NIC conjunction to the proof of the right conjunct."))

(rule~defrule NIC-AND-I-rs NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L2))
  (description "Given the right conjunct, reducing the proof of a NIC conjunction to the proof of the left conjunct."))

(com~defcommand NIC-AND-I
  (argnames conjunction lconj rconj)
  (argtypes ndline ndline ndline)
  (arghelps "Conjunction to justify" "The left conjunct" "The right conjunct")
  (function nic=NIC-AND-I)
  (frag-cats rules propositional introduction backward forward)
  (log-p T)
  (help "Justify a NIC conjunction by its two conjuncts."))

(defun nic=NIC-AND-I (conj lconj rconj)
  (infer~compute-outline 'NIC-AND-I (list conj lconj rconj) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-AND-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-E-l
	       (outline-mappings (((closed closed) NIC-AND-E-l-c)
				  ((existent existent) NIC-AND-E-l-a)
				  ((nonexistent existent) NIC-AND-E-l-f)
				  ))
	       (help "The NIC conjunction elemination left rule."))

(rule~defrule NIC-AND-E-l-c NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (description "Checking an application of the NIC conjunction elimination left rule."))

(rule~defrule NIC-AND-E-l-a NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the NIC conjunction elimination left rule."))

(rule~defrule NIC-AND-E-l-f NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a left conjunct from a NIC conjunction."))

(com~defcommand NIC-AND-E-l
  (argnames lconj conjunction)
  (argtypes ndline ndline)
  (arghelps "The left conjunct to justify" "A Conjunction")
  (function nic=NIC-AND-E-l)
  (frag-cats rules propositional elimination forward)
  (level 2)
  (log-p T)
  (help "Deducing a left conjunct from a NIC conjunction."))

(defun nic=NIC-AND-E-l (lconj conj)
  (infer~compute-outline 'NIC-AND-E-l (list lconj conj) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-AND-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-E-r
	       (outline-mappings (((closed closed) NIC-AND-E-r-c)
				  ((existent existent) NIC-AND-E-r-a)
				  ((nonexistent existent) NIC-AND-E-r-f)
				 ))
	       (help "The NIC conjunction elemination right rule."))

(rule~defrule NIC-AND-E-r-c NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (description "Checking an application of the NIC conjunction elimination right rule."))


(rule~defrule NIC-AND-E-r-a NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the NIC conjunction elimination right rule."))

(rule~defrule NIC-AND-E-r-f NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a right conjunct from a NIC conjunction."))

(com~defcommand NIC-AND-E-r
  (argnames rconj conjunction)
  (argtypes ndline ndline)
  (arghelps "The right conjunct to justify" "A Conjunction")
  (function nic=NIC-AND-E-r)
  (frag-cats rules propositional elimination forward)
  (level 2)
  (log-p T)
  (help "Deducing a right conjunct from a NIC conjunction."))

(defun nic=NIC-AND-E-r (rconj conj)
  (infer~compute-outline 'NIC-AND-E-r (list rconj conj) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-I-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-I-l
	       (outline-mappings (((closed closed) NIC-OR-I-l-c)
				  ((existent existent) NIC-OR-I-l-a)
				  ((nonexistent existent) NIC-OR-I-l-f)
				  ((existent nonexistent) NIC-OR-I-l-b)))
	       (parameter-types term)
	       (help "The NIC disjunction left introduction rule"))

(rule~defrule NIC-OR-I-l-c NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Checking an application of the NIC disjunction introduction left rule."))

(rule~defrule NIC-OR-I-l-a NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a NIC disjunction by its left disjunct."))

(rule~defrule NIC-OR-I-l-f NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Deducing a NIC disjunction from its left disjunct."))

(rule~defrule NIC-OR-I-l-b NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Reducing the proof of a NIC disjunction to the proof of its left disjunct."))

(com~defcommand NIC-OR-I-l
  (argnames disjunction ldisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Left disjunct" "Right disjunct as term")
  (function nic=NIC-OR-I-l)
  (frag-cats rules propositional introduction backward)
  (log-p T)
  (help "Justify a NIC disjunction by its left disjunct."))

(defun nic=NIC-OR-I-l (disj ldis par)
  (infer~compute-outline 'NIC-OR-I-l (list disj ldis) (list par)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-I-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-I-r
	       (outline-mappings (((closed closed) NIC-OR-I-r-c)
				  ((existent existent) NIC-OR-I-r-a)
				  ((nonexistent existent) NIC-OR-I-r-f)
				  ((existent nonexistent) NIC-OR-I-r-b)))
	       (parameter-types term)
	       (help "The NIC disjunction introduction right rule."))


(rule~defrule NIC-OR-I-r-c NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Checking an application of the NIC disjunction introduction right rule."))

(rule~defrule NIC-OR-I-r-a NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a NIC disjunction by its right disjunct."))

(rule~defrule NIC-OR-I-r-f NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Deducing a NIC disjunction from its right disjunct."))

(rule~defrule NIC-OR-I-r-b NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Reducing the proof of a NIC disjunction to the proof of its right disjunct."))

(com~defcommand NIC-OR-I-r
  (argnames disjunction rdisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Right disjunct" "Left disjunct as term")
  (function nic=NIC-OR-I-r)
  (frag-cats rules propositional introduction backward)
  (log-p T)
  (help "Justify a NIC disjunction by its right disjunct."))

(defun nic=NIC-OR-I-r (disj rdis par)
  (infer~compute-outline 'NIC-OR-I-r (list disj rdis) (list par)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule OR-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To Do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule OR-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To Do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-NEG-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-NEG-E
	       (outline-mappings (((closed closed closed) NIC-NEG-E-c)
				  ((existent existent existent) NIC-NEG-E-a)
				  ((existent existent nonexistent) NIC-NEG-E-l)
				  ((existent nonexistent existent) NIC-NEG-E-r)
				  ((nonexistent existent existent) NIC-NEG-E-f)
				 ))
	       (help "The NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-c NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (description "Checking NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-a NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1 P2))
  (description "Application of NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-f NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
            (P2 () (not A)))
  (actions
   ((support P1 P2) (sponsor C) (unsponsor P1 P2)))
  (description "NIC negation elimination forward."))

(rule~defrule NIC-NEG-E-r NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P2))
  (description "NIC negation elimination backwards using negated line."))

(rule~defrule NIC-NEG-E-l NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1))
  (description "NIC negation elimination backward using positive line."))


(com~defcommand NIC-NEG-E
  (argnames negation line falsehood)
  (argtypes ndline ndline ndline)
  (arghelps "A negated line" "The opposite line" "Line with falsehood to prove" )
  (function nic=NIC-NEG-E)
  (frag-cats rules propositional elimination forward)
  (log-p T)
  (help "Eliminate a NIC negation."))

(defun nic=NIC-NEG-E (neg line fal)
  (infer~compute-outline 'NIC-NEG-E (list fal line neg) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-E-e
	       (outline-mappings (((closed closed closed closed) NIC-OR-E-e-c)
				  ((existent nonexistent nonexistent nonexistent) NIC-OR-E-e-b)))
	       (help "The OR elimination."))

(rule~defrule NIC-OR-E-e-c NIC-OR-E-e (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (P1 () (or A B))
   	    (P2 (H1) F)
            (P3 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (description "Checking an application of the Disjunction Elimination rule."))

;(rule~defrule NIC-OR-E-e-b NIC-OR-E-e (in base)
;  (declarations 
;   (meta-variables (A O) (B O) (F O)))
;  (conclusion (C () F))
;  (premises (D () (or A B))
;            (P1 (H1) F)
;            (P2 (H2) F))
;  (extra-hyps (H1 () A)
;              (H2 () B))
;  (sideconditions
;   (pds~node-supported-by-p C D))
;  (actions (P1 (sponsor H1) (unsponsor D))
;           (P2 (sponsor H2) (unsponsor D)))
;  (description "Disjunction elimination"))

(com~defcommand NIC-OR-E-e
  (argnames goal disjunction)
  (argtypes ndline ndline)
  (arghelps "The goal"
	    "A disjunction")
  (function nic=NIC-OR-E-e)
  (frag-cats rules structural elimination propositional backward)
  (log-p T)
  (help "Apply rule of cases to prove goal."))

(defun nic=NIC-OR-E-e (goal disj)
  (infer~compute-outline 'NIC-OR-E-e (list goal disj nil nil) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-E-i
	       (outline-mappings (((closed closed closed closed) NIC-OR-E-i-c)
				  ((existent existent nonexistent nonexistent)
                                   NIC-OR-E-i-b)))
	       (help "The NIC disjunction elimination rule."))

(rule~defrule NIC-OR-E-i-c NIC-OR-E-i (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (P1 () (or A B))
   	    (P2 (H1) F)
            (P3 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (description "Checking an application of the NIC disjunction elimination rule."))

(rule~defrule NIC-OR-E-i-b NIC-OR-E-i (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (D () (or A B))
            (P1 (H1) F)
            (P2 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (sideconditions
   (pds~node-supported-by-p C D))
  (actions (P1 (sponsor H1) (unsponsor D))
	   (P2 (sponsor H2) (unsponsor D)))
  (description "NIC disjunction elimination."))

(com~defcommand NIC-OR-E-i
  (argnames goal disjunction)
  (argtypes ndline ndline)
  (arghelps "The goal"
	    "A disjunction")
  (function nic=NIC-OR-E-i)
  (frag-cats rules structural elimination propositional backward)
  (log-p T)
  (help "Apply NIC disjunction elimination rule."))

(defun nic=NIC-OR-E-i (goal disj)
  (infer~compute-outline 'NIC-OR-E-i (list goal disj nil nil) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rule NIC-NEG-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-NEG-I
	       (outline-mappings (((closed closed) NIC-NEG-I-c)
				  ((existent existent) NIC-NEG-I-a)
				  ((existent nonexistent) NIC-NEG-I-b)))
	       (help "The NIC negation introduction rule."))

(rule~defrule NIC-NEG-I-c NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Checking an application of the NIC negation Introduction rule."))

(rule~defrule NIC-NEG-I-a NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Application of the NIC negation introduction rule."))

(rule~defrule NIC-NEG-I-b NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "NIC negation introduction backwards."))

(com~defcommand NIC-NEG-I
  (argnames negation falsity)
  (argtypes ndline ndline)
  (arghelps "A negated line" "A falsity line")
  (function nic=NIC-NEG-I)
  (frag-cats rules structural introduction propositional backward)
  (log-p T)
  (help "Introduce a NIC negation."))

(defun nic=NIC-NEG-I (neg fals)
  (infer~compute-outline 'NIC-NEG-I (list neg fals) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-FALSE-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FALSE-C
	       (outline-mappings (((closed closed) NIC-FALSE-C-c)
				  ((existent existent) NIC-FALSE-C-a)
				  ((existent nonexistent) NIC-FALSE-C-b)))
	       (help "The NOT-Introduction rule."))

(rule~defrule NIC-FALSE-C-c NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (description "Checking an application of the NIC falsity introduction rule."))

(rule~defrule NIC-FALSE-C-a NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (description "Application of the NIC falsity introduction rule."))

(rule~defrule NIC-FALSE-C-b NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (actions (P (sponsor H)))
  (description "NIC falsity introduction backwards."))

(com~defcommand NIC-FALSE-C
  (argnames conc falsity)
  (argtypes ndline ndline)
  (arghelps "A unnegated line" "A falsity line")
  (function nic=NIC-FALSE-C)
  (frag-cats rules structural false propositional backward)
  (log-p T)
  (help "Introduce a NIC falsity."))

(defun nic=NIC-FALSE-C (form fals)
  (infer~compute-outline 'NIC-FALSE-C (list form fals) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;           NIC Rules  for First Order Logic              ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-FORALL-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FORALL-I
	       (outline-mappings (((closed closed) NIC-FORALL-I-c)
				  ((existent existent) NIC-FORALL-I-a)
				  ((existent nonexistent) NIC-FORALL-I-b)))
	       (parameter-types term)
	       (help "The NIC universal elimination rule."))

(rule~defrule NIC-FORALL-I-c NIC-FORALL-I (in nic)
  (parameters (X term+term "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Checking NIC universal introduction."))

(rule~defrule NIC-FORALL-I-a NIC-FORALL-I (in nic)
  (parameters (X term+constant "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Application of NIC universal introduction."))

(rule~defrule NIC-FORALL-I-b NIC-FORALL-I (in nic)
  (parameters (X term+term "the new Skolem term"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "NIC universal introduction backwards."))


(com~defcommand NIC-FORALL-I
  (argnames univ-line parameter line)
  (argtypes ndline term ndline)
  (arghelps "A Universal line to prove" "New parameter" "A line")
  (function nic=NIC-FORALL-I)
  (frag-cats rules quantifier introduction backward)
  (log-p T)
  (help "Introduce a NIC universal quantifier."))

(defun nic=NIC-FORALL-I (univ param line)
  (infer~compute-outline 'NIC-FORALL-I (list univ line) (list param)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-I
	       (outline-mappings (((closed closed) NIC-EXISTS-I-c)
				  ((existent existent) NIC-EXISTS-I-a)
				  ((existent nonexistent) NIC-EXISTS-I-b)))
	       (parameter-types term position-list)
	       (help "The NIC existential introduction rule."))

(rule~defrule NIC-EXISTS-I-c NIC-EXISTS-I (in nic)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Check of NIC existential introduction."))

(rule~defrule NIC-EXISTS-I-a NIC-EXISTS-I (in nic)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (pds~node-supported-by-p C P)
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Application of NIC existential introduction."))
  
(rule~defrule NIC-EXISTS-I-b NIC-EXISTS-I (in nic)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList))
  (computations
   (F (orules=replace-at-positions A PList X)))
  (description "Backward application of NIC existential introduction."))

(com~defcommand NIC-EXISTS-I
  (argnames exists-line prem witness positionlist)
  (argtypes ndline ndline term position-list)
  (arghelps "Line to justify" "Premise" "Witness term" "List of term positions")
  (function nic=NIC-EXISTS-I)
  (frag-cats rules quantifier introduction backward)
  (log-p T)
  (help "Justify an NIC existential line from a particular instance."))
 
(defun nic=NIC-EXISTS-I (plan prem term position)
  (infer~compute-outline 'NIC-EXISTS-I (list plan prem) (list term position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-E-i
    (outline-mappings (((closed closed closed) NIC-EXISTS-E-i-c)
                       ((existent existent nonexistent) NIC-EXISTS-E-i-b)))
    (parameter-types termsym)
	       (help "The NIC existential elimination-i rule."))

(rule~defrule NIC-EXISTS-E-i-c NIC-EXISTS-E-i (in nic)
  (parameters (X term+term "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Checking NIC existential elimination-i."))

(rule~defrule NIC-EXISTS-E-i-b NIC-EXISTS-E-i (in nic)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~node-supported-by-p C P1)
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (actions (P2 (sponsor H) (unsponsor P1)))
  (description "NIC existential elimination-i backwards"))

(com~defcommand NIC-EXISTS-E-i
  (argnames conc exline parameter subgoal)
  (argtypes ndline ndline termsym ndline)
  (arghelps "A Line to be proved" "An existential line" "A term" "The second premise line")
  (function nic=NIC-EXISTS-E-i)
  (frag-cats rules structural elimination backward quantifier)
  (log-p T)
  (help "Eliminate an existential quantifier."))

(defun nic=NIC-EXISTS-E-i (conc ex param prem2)
  (infer~compute-outline 'NIC-EXISTS-E-i (list conc ex prem2) (list param)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-FORALL-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FORALL-E
	       (outline-mappings (((closed closed) NIC-FORALL-E-c)
				  ((existent existent) NIC-FORALL-E-a)
				  ((existent nonexistent) NIC-FORALL-E-b)
				  ((nonexistent existent) NIC-FORALL-E-f)
				 ))
	       (parameter-types term)
	       (help "The NIC universal elimination rule."))

(rule~defrule NIC-FORALL-E-c NIC-FORALL-E (in nic)
  (parameters (Y term+term "The universally quantified formula"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (nic~universal-quantification-p Y)
   (term~alpha-equal (logic~quantification-scope Y) A)
   (nic=special-sidecond1 AY A X Y))
  (description "Checking NIC universal elimination."))

(defun nic=special-sidecond1 (AY A X Y)
  (term~alpha-equal (logic~quantification-scope Y) A))
	 

(rule~defrule NIC-FORALL-E-a NIC-FORALL-E (in nic)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (pds~node-supported-by-p C P)
   (nic~universal-quantification-p Y)
   (nic=special-sidecond1 AY A X Y))
  (description "Application of NIC universal elimination."))


;(rule~defrule NIC-FORALL-E-f NIC-FORALL-E (in nic)
;  (parameters (Y term+term "A term"))
;  (declarations
;   (type-variables CC)
;   (meta-variables (Y O) (AY O) (X CC) (A O)))
;  (conclusion (C () AY))
;  (premises (P () (forall (lam (X CC :lookup) A))))
;  (computations
;   (AY (orules=substitute-for-in Y X A)))
;  (actions ((support P) (sponsor C)))
;  (description "NIC Universal elimination forwards."))

(rule~defrule NIC-FORALL-E-b NIC-FORALL-E (in nic)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y O) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () A))
  (computations
   (A (nic=id Y)))
  (actions ((support P) (sponsor C)))
  (description "NIC Universal elimination forwards."))


(defun nic=id (x) x)

;(defun nic=compute-forall (AY Y)
;  (logic~quantification-create (logic~universal-quantor :name :forall)
;                               Y
;                               AY))

(com~defcommand NIC-FORALL-E
  (argnames  line univ-line term)
  (argtypes ndline ndline term)
  (arghelps "A line" "Universal line" "Term to substitute")
  (function nic=NIC-FORALL-E)
  (frag-cats rules quantifier elimination forward)
  (log-p T)
  (help "Instantiate a NIC universal quantifier."))

(defun nic=NIC-FORALL-E (line uni term)
  (infer~compute-outline 'NIC-FORALL-E (list line uni) (list term)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-EXISTS-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-EXISTS-E-e
    (outline-mappings (((closed closed closed) NIC-EXISTS-E-e-c)
                       ((existent existent nonexistent) NIC-EXISTS-E-e-b)))
    (parameter-types termsym)
	       (help "The NIC existential elimination-e rule."))

(rule~defrule NIC-EXISTS-E-e-c NIC-EXISTS-E-e (in nic)
  (parameters (X term+term "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Checking NIC existential elimination-e."))

(rule~defrule NIC-EXISTS-E-e-b NIC-EXISTS-E-e (in nic)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~node-supported-by-p C P1)
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (actions (P2 (sponsor H) (unsponsor P1)))
  (description "NIC existential elimination-e backwards"))

(com~defcommand NIC-EXISTS-E-e
  (argnames conc exline parameter subgoal)
  (argtypes ndline ndline termsym ndline)
  (arghelps "A Line to be proved" "An existential line" "A term" "The second premise line")
  (function nic=NIC-EXISTS-E-e)
  (frag-cats rules structural elimination backward quantifier)
  (log-p T)
  (help "Eliminate an existential quantifier."))

(defun nic=NIC-EXISTS-E-e (conc ex param prem2)
  (infer~compute-outline 'NIC-EXISTS-E-e (list conc ex prem2) (list param)))

