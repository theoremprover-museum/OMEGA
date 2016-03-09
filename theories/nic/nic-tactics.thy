;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: keim -*-
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

;; The following set of tactics are partially derived from the natural deduction 
;; calculus for intercalation as presented in the PhD thesis of John Byrnes 
;; (Carnegie Mellon University, 1999).
;; In John's thesis they are presented and commented in chapter 4.1 (pages 93 ff.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;            NIC Tactics for Predicate Logic              ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-IMP-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-IMP-E
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-IMP-E-b)
				    ((existent existent existent) NICTAC-IMP-E-a)
				    ((nonexistent existent existent) NICTAC-IMP-E-f)
				    ((existent existent nonexistent) NICTAC-IMP-E-r)
				    ((existent nonexistent existent) NICTAC-IMP-E-l)))
		 (expansion-function nictac=expand-NICTAC-IMP-E)
		 (parameter-types term)
		 (help "The NIC tactic for Implication elimination."))


(tac~deftactic NICTAC-IMP-E-b NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations 
                 (L3 (nictac=NICTAC-IMP-E-compute1 term))
		 (L2 (nictac=NICTAC-IMP-E-compute2 term)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond1 term L1))
   (description "Right application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute1 (term)
  (first (data~appl-arguments term)))

(defun nictac=NICTAC-IMP-E-compute2 (term)
  term)

(defun nictac=NICTAC-IMP-E-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-IMP-E-a NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-IMP-E-sidecond2 term L1 l2 l3))
   (description "Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-sidecond2 (term L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-IMP-E-f NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L1 (nictac=NICTAC-IMP-E-compute3 L2)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond3 term L2 L3))
   (description "Forward Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute3 (L2)
  (when (logic~implication-p (node~formula L2))
    (second (data~appl-arguments L2))))

(defun nictac=NICTAC-IMP-E-sidecond3 (term L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))))

(tac~deftactic NICTAC-IMP-E-l NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-IMP-E-compute4 L1 L3)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond4 term L1 L3))
   (description "Leftwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute4 (L1 L3)
  (term~appl-create (logic~implication-constant)
		    (list (node~formula L3) (node~formula L1))))

(defun nictac=NICTAC-IMP-E-sidecond4 (term L1 L3)
  (and (logic~implication-p term)
       (term~alpha-equal (first (data~appl-arguments term))
		   (node~formula L3))
       (term~alpha-equal (second (data~appl-arguments term))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-IMP-E-r NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-IMP-E-compute5 L2)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond5 term L1 L2))
   (description "Rightwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute5 (L2)
  (when (logic~implication-p (node~formula L2))
    (first (data~appl-arguments (node~formula L2)))))

(defun nictac=NICTAC-IMP-E-sidecond5 (term L1 L2)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-IMP-E (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-IMP-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-IMP-E
  (argnames succedent implication antecedent term)
  (argtypes ndline ndline ndline term)
  (arghelps "Succedent" "Implication" "Antecedent" "The implication-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-IMP-E)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Implication elimination."))


(defun nictac=NICTAC-IMP-E (succedent implication antecedent term)
  (infer~compute-outline 'NICTAC-IMP-E (list succedent implication antecedent) (list term)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-AND-E-L
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-AND-E-L
		 (outline-mappings (((existent nonexistent) NICTAC-AND-E-L-b)
				    ((existent existent) NICTAC-AND-E-L-a)))
		 (expansion-function nictac=expand-NICTAC-AND-E-L)
		 (parameter-types term)
		 (help "The NIC tactic for Conjunction elimination left."))


(tac~deftactic NICTAC-AND-E-L-b NICTAC-AND-E-L (in base)
   (parameters (term term+term "The conjunction term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-AND-E-L-compute1 term)))
   (sideconditions (nictac=NICTAC-AND-E-L-sidecond1 term L1))
   (description "Right application of NIC tactic AND-E-L."))

(defun nictac=NICTAC-AND-E-L-compute1 (term)
  term)

(defun nictac=NICTAC-AND-E-L-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-AND-E-L-a NICTAC-AND-E-L (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-AND-E-L-sidecond2 term L1 L2))
   (description "Application of NIC tactic AND-E-L."))

(defun nictac=NICTAC-AND-E-L-sidecond2 (term L1 L2)
  (and (logic~conjunction-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-AND-E-L (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-AND-E-L outline nil)
  (tacl~end))

(com~defcommand NICTAC-AND-E-L
  (argnames conc conj term)
  (argtypes ndline ndline term)
  (arghelps "Premise" "Conjunction" "The conjunction-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-AND-E-L)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Conjunction elimination left."))

(defun nictac=NICTAC-AND-E-L (conc conj term)
  (infer~compute-outline 'NICTAC-AND-E-L (list conc conj) (list term)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-AND-E-R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-AND-E-R
		 (outline-mappings (((existent nonexistent) NICTAC-AND-E-R-b)
				    ((existent existent) NICTAC-AND-E-R-a)))
		 (expansion-function nictac=expand-NICTAC-AND-E-R)
		 (parameter-types term)
		 (help "The NIC tactic for Conjunction elimination right."))


(tac~deftactic NICTAC-AND-E-R-b NICTAC-AND-E-R (in base)
   (parameters (term term+term "The conjunction term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-AND-E-R-compute1 term)))
   (sideconditions (nictac=NICTAC-AND-E-R-sidecond1 term L1))
   (description "Right application of NIC tactic AND-E-R."))

(defun nictac=NICTAC-AND-E-R-compute1 (term)
  term)

(defun nictac=NICTAC-AND-E-R-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-AND-E-R-a NICTAC-AND-E-R (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-AND-E-R-sidecond2 term L1 L2))
   (description "Application of NIC tactic AND-E-R."))

(defun nictac=NICTAC-AND-E-R-sidecond2 (term L1 L2)
  (and (logic~conjunction-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
			 (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-AND-E-R (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-AND-E-R outline nil)
  (tacl~end))

(com~defcommand NICTAC-AND-E-R
  (argnames conc conj term)
  (argtypes ndline ndline term)
  (arghelps "Premise" "Conjunction" "The conjunction-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-AND-E-R)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Conjunction elimination right."))

(defun nictac=NICTAC-AND-E-R (conc conj term)
  (infer~compute-outline 'NICTAC-AND-E-R (list conc conj) (list term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-NEG-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-NEG-E
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-NEG-E-b)
				    ((existent existent existent) NICTAC-NEG-E-a)
				    ((nonexistent existent existent) NICTAC-NEG-E-f)
				    ((existent existent nonexistent) NICTAC-NEG-E-r)
				    ((existent nonexistent existent) NICTAC-NEG-E-l)))
		 (expansion-function nictac=expand-NICTAC-NEG-E)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-NEG-E-b NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-NEG-E-compute1 term))
		 (L2 (nictac=NICTAC-NEG-E-compute2 term)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond1 term L1))
   (description "Right application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute1 (term)
  (when (logic~negation-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-NEG-E-compute2 (term)
  term)

(defun nictac=NICTAC-NEG-E-sidecond1 (term L1)
  (and (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-a NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-NEG-E-sidecond2 term L1 l2 l3))
   (description "Application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-sidecond2 (term L1 L2 L3)
  (and (logic~negation-p (node~formula L2))
       (logic~falsity-constant-p (node~formula L1))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-f NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L1 (nictac=NICTAC-NEG-E-compute3)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond3 term L2 L3))
   (description "Forward Application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute3 ()
  (logic~falsity-constant))

(defun nictac=NICTAC-NEG-E-sidecond3 (term L2 L3)
  (and (logic~negation-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))))

(tac~deftactic NICTAC-NEG-E-l NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-NEG-E-compute4 L3)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond4 term L1 L3))
   (description "Leftwards application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute4 (L3)
  (agplan~negate (node~formula L3)))

(defun nictac=NICTAC-NEG-E-sidecond4 (term L1 L3)
  (and (logic~negation-p term)
       (term~alpha-equal (first (data~appl-arguments term))
		   (node~formula L3))
       (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-r NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-NEG-E-compute5 L2)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond5 term L1 L2))
   (description "Rightwards application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute5 (L2)
  (when (logic~negation-p (node~formula L2))
    (first (data~appl-arguments (node~formula L2)))))

(defun nictac=NICTAC-NEG-E-sidecond5 (term L1 L2)
  (and (logic~negation-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-NEG-E (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-NEG-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-NEG-E
  (argnames falsity-const negated-form unnegated-form term)
  (argtypes ndline ndline ndline term)
  (arghelps "Falsity-Constant" "Negated-Formula" "Unnegated-Formula" "The negated formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-NEG-E)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Negated-Form elimination."))

(defun nictac=NICTAC-NEG-E (falsity-const negated-form unnegated-form term)
  (infer~compute-outline 'NICTAC-NEG-E (list falsity-const negated-form unnegated-form) (list term)))


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-OR-E-e
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-OR-E-e-b)
				    ((existent existent nonexistent nonexistent) NICTAC-OR-E-e-r)))
		 (expansion-function nictac=expand-NICTAC-OR-E-e)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-OR-E-e-b NICTAC-OR-E-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-OR-E-e-compute1 term))
		 (H1 (nictac=NICTAC-OR-E-e-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-e-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-e-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-e-compute4 L1)))   
   (sideconditions (nictac=NICTAC-OR-E-e-sidecond1 term L1))
   (description "Backward application of NIC tactic OR-E-e."))

(defun nictac=NICTAC-OR-E-e-compute1 (term)
  term)

(defun nictac=NICTAC-OR-E-e-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-e-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-e-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-OR-E-e-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p L1 (pds~node-supports L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-OR-E-e-r NICTAC-OR-E-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-OR-E-e-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-e-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-e-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-e-compute4 L1)))
   (sideconditions (nictac=NICTAC-OR-E-e-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic OR-E-e."))

(defun nictac=NICTAC-OR-E-e-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p L1 (pds~node-supports L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-OR-E-e (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-OR-E-e outline nil)
  (tacl~end))

(com~defcommand NICTAC-OR-E-e
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunctionula" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-OR-E-e)
  (frag-cats tactics base)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-OR-E-e (conc disjunction new-conc1 new-conc2 term)
  (infer~compute-outline 'NICTAC-OR-E-e (list conc disjunction new-conc1 new-conc2) (list term)))

|#

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-OR-E-i
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-OR-E-i-b)
				    ((existent existent nonexistent nonexistent) NICTAC-OR-E-i-r)))
		 (expansion-function nictac=expand-NICTAC-OR-E-i)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-OR-E-i-b NICTAC-OR-E-i (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-OR-E-i-compute1 term))
		 (H1 (nictac=NICTAC-OR-E-i-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-i-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-i-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-i-compute4 L1)))   
   (sideconditions (nictac=NICTAC-OR-E-i-sidecond1 term L1))
   (description "Backward application of NIC tactic OR-E-i."))

(defun nictac=NICTAC-OR-E-i-compute1 (term)
  term)

(defun nictac=NICTAC-OR-E-i-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-i-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-i-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-OR-E-i-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-OR-E-i-r NICTAC-OR-E-i (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-OR-E-i-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-i-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-i-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-i-compute4 L1)))
   (sideconditions (nictac=NICTAC-OR-E-i-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic OR-E-i."))

(defun nictac=NICTAC-OR-E-i-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-OR-E-i (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-OR-E-i outline nil)
  (tacl~end))

(com~defcommand NICTAC-OR-E-i
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunction" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-OR-E-i)
  (frag-cats tactics base)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-OR-E-i (conc disjunction new-conc1 new-conc2 term)
  (infer~compute-outline 'NICTAC-OR-E-i (list conc disjunction new-conc1 new-conc2) (list term)))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICDUMMY-OR-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand NICDUMMY-OR-E-l
  (argnames conc term)
  (argtypes ndline term)
  (arghelps "Conclusion" "The disjunction formula")
  (function nictac=NICDUMMY-OR-E-l)
  (frag-cats tactics base dummy)
  (log-p T)
  (help "Special dummy NIC tactic Disjunction elimination left."))


(defun nictac=NICDUMMY-OR-E-l (conc term)
  (when (logic~disjunction-p term)
    (let ((left-disj (first (data~appl-arguments term))))
      (auto=message "~A: Adding ~A to nodes ~{~A ~}." "NICDUMMY-OR-E-l" (list term left-disj)
		    (cons conc (foci~descendants (foci~active-pc))))
      (mapcar #'(lambda (x) (nic~add-or-e-info x term left-disj))
	      (cons conc (foci~descendants (foci~active-pc)))))
    (sugg~reset)))
    ;;    (proc~without-scheduling
;;     (sugg~reset)
;;     (setf (bb~reset-state (bb~find-blackboard 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~get-surveyor 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~find-parameter-agent 'nic-false-c '(conc) nil
;;                                                          '(falsity)))
;;           sugg*reset))))

(defun nictac~reduce-or-e-l-termsuggestions (termlist hash-entries)
  (let ((reslist))
    (dolist (disj termlist reslist)
      (when (not (find-if #'(lambda (x) (or (and (term~p x) (term~alpha-equal x disj))
					    (and (listp x)
						 (term~alpha-equal (first x) disj)
						 (term~alpha-equal (first (data~appl-arguments (first x)))
							     (second x)))))
		       ;;; dummy-or-e-l has already been executed
			  hash-entries))
	(push disj reslist)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICDUMMY-OR-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand NICDUMMY-OR-E-r
  (argnames conc term)
  (argtypes ndline term)
  (arghelps "Conclusion" "The disjunction formula")
  (function nictac=NICDUMMY-OR-E-r)
  (frag-cats tactics base dummy)
  (log-p T)
  (help "Special dummy NIC tactic Disjunction elimination right."))

(defun nictac=NICDUMMY-OR-E-r (conc term)
  (when (logic~disjunction-p term)
    (let ((right-disj (second (data~appl-arguments term))))
      (auto=message "~A: Adding ~A to nodes ~{~A ~}." "NICDUMMY-OR-E-r" (list term right-disj)
		   (cons conc (foci~descendants (foci~active-pc))))
      (mapcar #'(lambda (x) (nic~add-or-e-info x term right-disj))
	      (cons conc (foci~descendants (foci~active-pc)))))
    (sugg~reset)))
;;
;;    (proc~without-scheduling
;;     (sugg~reset)
;;     (setf (bb~reset-state (bb~find-blackboard 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~get-surveyor 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~find-parameter-agent 'nic-false-c '(conc) nil
;;							  '(falsity))) sugg*reset))))

(defun nictac~reduce-or-e-r-termsuggestions (termlist hash-entries)
  (let ((reslist))
    (dolist (disj termlist reslist)
      (when (not (find-if #'(lambda (x) (or (and (term~p x) (term~alpha-equal x disj))
					    (and (listp x)
						 (term~alpha-equal (first x) disj)
						 (term~alpha-equal (second (data~appl-arguments (first x)))
							     (second x)))))
		       ;;; dummy-or-e-r has already been executed
			  hash-entries))
	(push disj reslist)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-or-e
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-or-e-b)
				    ((existent existent nonexistent nonexistent) NICTAC-or-e-r)))
		 (expansion-function nictac=expand-NICTAC-or-e)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-or-e-b NICTAC-or-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-or-e-compute1 term))
		 (H1 (nictac=NICTAC-or-e-compute2 term))
		 (H2 (nictac=NICTAC-or-e-compute3 term))
		 (L3 (nictac=NICTAC-or-e-compute4 L1))
		 (L4 (nictac=NICTAC-or-e-compute4 L1)))   
   (sideconditions (nictac=NICTAC-or-e-sidecond1 term L1))
   (description "Backward application of NIC tactic or-e."))

(defun nictac=NICTAC-or-e-compute1 (term)
  term)

(defun nictac=NICTAC-or-e-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-or-e-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-or-e-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-or-e-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-or-e-r NICTAC-or-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-or-e-compute2 term))
		 (H2 (nictac=NICTAC-or-e-compute3 term))
		 (L3 (nictac=NICTAC-or-e-compute4 L1))
		 (L4 (nictac=NICTAC-or-e-compute4 L1)))
   (sideconditions (nictac=NICTAC-or-e-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic or-e."))

(defun nictac=NICTAC-or-e-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-or-e (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-or-e outline nil)
  (tacl~end))

(com~defcommand NICTAC-or-e
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunction" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-or-e)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-or-e (conc disjunction new-conc1 new-conc2 term)
  (auto=message "~% Apply NICTAC-or-e to node ~A with disj ~A" conc term)
  (infer~compute-outline 'NICTAC-or-e (list conc disjunction new-conc1 new-conc2) (list term))
  (nic~remove-or-e-info conc term))



(defun nictac=logic-negate (formula)
  (if (logic~negation-p formula)
      (car (data~appl-arguments formula))
    (logic~negate formula)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;           NIC Tactics for First Order Logic             ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-FORALL-I
		 (outline-mappings (((existent nonexistent) NICTAC-FORALL-I-b)))
		 (expansion-function nictac=expand-NICTAC-FORALL-I)
		 (help "The NIC tactic for Conjunction elimination right."))


(tac~deftactic NICTAC-FORALL-I-b NICTAC-FORALL-I (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-FORALL-I-compute1 (formula L1))))
   (sideconditions (nic~universal-quantification-p (formula L2)))
   (description ""))

(defun nictac=compute-foralle-instance (formula term-list)
  (let ((new-term formula))
    (dolist (x term-list)
      (when (nic~universal-quantification-p new-term)
	(setq new-term (beta~contract
			(term~appl-create (car (data~appl-arguments new-term))
					  (list x))))))
    new-term))

(defun nictac=NICTAC-FORALL-I-compute1 (node)
  (nictac=compute-foralle-instance
   node
   (list (hocnf~new-skolem-term-create (logic~quantification-bound-variable node)
				       (remove
					(logic~quantification-bound-variable node)
					(foci~free-vars (foci~active-pc))
					:test #'data~equal)
				       (pds~environment (agplan~current-proof-plan))))))

(defun nictac=expand-NICTAC-FORALL-I (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-FORALL-I outline paramters)
  (tacl~end))

(com~defcommand NICTAC-FORALL-I
  (argnames conc prem)
  (argtypes ndline ndline)
  (arghelps "Conclusion" "Premise")
  (function nictac=NICTAC-FORALL-I)
  (frag-cats tactics base elimination)
  (log-p T)
  (help ""))

(defun nictac=NICTAC-FORALL-I (conc prem)
  (infer~compute-outline 'NICTAC-FORALL-I (list conc prem) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;        NIC Tactics for Equality and Equivalence         ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-MODTOLL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic NICTAC-MODTOLL
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-MODTOLL-b)
				    ((existent existent existent) NICTAC-MODTOLL-a)
				    ((nonexistent existent existent) NICTAC-MODTOLL-f)
				    ((existent existent nonexistent) NICTAC-MODTOLL-r)
				    ((existent nonexistent existent) NICTAC-MODTOLL-l)))
		 (expansion-function nictac=expand-NICTAC-MODTOLL)
		 (parameter-types term)
		 (help "The NIC tactic for Implication elimination."))

;; backward application of Modus Tollens

(tac~deftactic NICTAC-MODTOLL-b NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations 
		(L3 (nictac=NICTAC-MODTOLL-compute1 term))
		(L2 (nictac=NICTAC-MODTOLL-compute2 term)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond1 term L1))
	       (description "Right application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute1 (term)
  (nictac=logic-negate (second (data~appl-arguments term))))

(defun nictac=NICTAC-MODTOLL-compute2 (term)
  term)

(defun nictac=NICTAC-MODTOLL-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ))) ;; test if neg(L1) = first (term) ?


;; closing Modus Tollens

(tac~deftactic NICTAC-MODTOLL-a NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations )
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond2 term L1 l2 l3))
	       (description "Application of NIC tactic IMP-E."))


(defun nictac=NICTAC-MODTOLL-sidecond2 (term L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L1)))
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L3)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

;; Applying Modus Tollens in forward direction

(tac~deftactic NICTAC-MODTOLL-f NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L1 (nictac=NICTAC-MODTOLL-compute3 L2)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond3 term L2 L3))
	       (description "Forward Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute3 (L2)
  (when (logic~implication-p (node~formula L2))
    (nictac=logic-negate (first (data~appl-arguments L2)))))

(defun nictac=NICTAC-MODTOLL-sidecond3 (term L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L3)))))

;; Applying Modus Tollen backwards in left direction


(tac~deftactic NICTAC-MODTOLL-l NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L2 (nictac=NICTAC-MODTOLL-compute4 L1 L3)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond4 term L1 L3))
	       (description "Leftwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute4 (L1 L3)
  (term~appl-create (logic~implication-constant)
		    (list (nictac=logic-negate (node~formula L1))
						  (nictac=logic-negate (node~formula L3)))))

(defun nictac=NICTAC-MODTOLL-sidecond4 (term L1 L3)
  (and (logic~implication-p term)
       (term~alpha-equal (second (data~appl-arguments term))
		   (nictac=logic-negate (node~formula L3)))
       (term~alpha-equal (first (data~appl-arguments term))
		   (nictac=logic-negate (node~formula L1)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

;; Backwards and rightwards, computing the negated succedent

(tac~deftactic NICTAC-MODTOLL-r NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L3 (nictac=NICTAC-MODTOLL-compute5 L2)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond5 term L1 L2))
	       (description "Rightwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute5 (L2)
  (when (logic~implication-p (node~formula L2))
    (nictac=logic-negate (second (data~appl-arguments (node~formula L2))))))

(defun nictac=NICTAC-MODTOLL-sidecond5 (term L1 L2)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L1)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-MODTOLL (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-IMP-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-MODTOLL
  (argnames negantecedent implication negsucc term)
  (argtypes ndline ndline ndline term)
  (arghelps "Negated antecedent" "Implication" "Negated succedent" "The implication-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-MODTOLL)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Modus tollens."))


(defun nictac=NICTAC-MODTOLL (succedent implication antecedent term)
  (infer~compute-outline 'NICTAC-MODTOLL (list succedent implication antecedent) (list term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; Transitivity of implication: Modus Barbara
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

#|
(infer~deftactic NICTAC-modbarbara
		 (outline-mappings (((existent existent existent) NICTAC-modbarbara-a)
                                    ((nonexistent existent existent) NICTAC-modbarbara-f)
				    ((existent nonexistent existent) NICTAC-modbarbara-l)
				    ((existent existent nonexistent) NICTAC-modbarbara-r)))
                 (expansion-function batac=expand-NICTAC-modbarbara)
		 (help "Transitivity of Implication."))


(tac~deftactic NICTAC-modbarbara-f NICTAC-modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations (C (nictac=modbarbara-f (formula L1) (formula L2))))
   (sideconditions (nictac=modbarbara-f-p (formula L1) (formula L2)))
   (description "Transitivity of implication in forward direction."))

(defun nictac=NICTAC-modbarbara-f-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (second (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun nictac=NICTAC-modbarbara-f (l1 l2)
  (let ((Opp (data~appl-function l1))
        (Aterm (first (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments l2))))
    ;; removing double negation
    (term~appl-create Opp (list Aterm Bterm))))

(tac~deftactic NICTAC-modbarbara-a NICTAC-modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations )
   (sideconditions (nictac=NICTAC-modbarbara-a-p (formula C) (formula L1) (formula L2)))
   (description "Transitivity of implication."))

(tac~deftactic NICTAC-modbarbara-l NICTAC-modbarbara (in base)
	       (premises L1 L2)
	       (conclusions C)
	       (computations (L1 (nictac=NICTAC-modbarbara-l (formula C) (formula L2))))
	       (sideconditions (nictac=NICTAC-modbarbara-l-p (formula C) (formula L2)))
	       (description "Transitivity of implication in leftward direction."))

(defun nictac=NICTAC-modbarbara-l (c l2)
  (let ((Opp (data~appl-function c))
        (Aterm (first (data~appl-arguments c)))
        (Bterm (first (data~appl-arguments l2))))
    (term~appl-create Opp (list Aterm Bterm))))


(defun nictac=NICTAC-modbarbara-l-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (second (data~appl-arguments form1))
		   (second (data~appl-arguments form2)))))


(tac~deftactic NICTAC-modbarbara-r NICTAC-modbarbara (in base)
	       (premises L1 L2)
	       (conclusions C)
	       (computations (L2 (nictac=NICTAC-modbarbara-r (formula C) (formula L1))))
	       (sideconditions (nictac=NICTAC-modbarbara-r-p (formula C) (formula L1)))
	       (description "Transitivity of implication in rightward direction."))

(defun nictac=NICTAC-modbarbara-r (c l1)
  (let ((Opp (data~appl-function c))
        (Aterm (second (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments c))))
    (term~appl-create Opp (list Aterm Bterm))))


(defun nictac=NICTAC-modbarbara-r-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (first (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun nictac=NICTAC-modbarbara-a-p (form1 form2 form3)
  (and (logic~implication-p form2)
       (logic~implication-p form2)
       (logic~implication-p form3)
       (term~alpha-equal (second (data~appl-arguments form2))
		   (first (data~appl-arguments form3)))
       (term~alpha-equal (nictac=NICTAC-modbarbara-f form2 form3)
		   form1)))


(defun nictac=expand-NICTAC-modbarbara (outline parameters)
  (let ((p1 (second outline))
	(p2 (third outline))
	(conc (first outline)))
    (tacl~init outline)
    (tacl~sequence
     (impi-res ('impi (list conc nil) nil))                ;;; conc  C hypA
     (BB ('impe (list nil (third impi-res) p1) nil))      ;;; B p1 hypA
     (CC ('impe (list (second impi-res) (car BB) p2) nil))) ;;; C p2 B
    (tacl~end)))

(defun nictac=NICTAC-modbarbara (line1 line2 line3)
  (infer~compute-outline 'NICTAC-modbarbara (list line1 line2 line3) nil))

(com~defcommand NICTAC-modbarbara
  (argnames conc line2 line3)
  (argtypes ndline ndline ndline)
  (arghelps "An implication line"
	    "Another one with the same antecedent"
	    "One with the same succedent as the first")
  (function nictac=NICTAC-modbarbara)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Transitivity of implication: modus barbara."))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equiv introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic NICTAC-EQUIV-I
		 (outline-mappings (((nonexistent existent existent) NICTAC-EQUIV-I-f)
				    ((existent nonexistent nonexistent) NICTAC-EQUIV-I-b)
				    ((existent nonexistent existent) NICTAC-EQUIV-I-l)
				    ((existent existent nonexistent) NICTAC-EQUIV-I-r)
				    ((existent existent existent) NICTAC-EQUIV-I-a)))
		 (expansion-function batac=expand-NICTAC-EQUIV-I)
		 (help "Two sided EQUIV-Introduction."))
;; Hier fehlt noch -a, -l, -r


(tac~deftactic NICTAC-EQUIV-I-f NICTAC-EQUIV-I (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=compute-equivalence (formula L1))))
   (sideconditions (batac=reversed-implications-p (formula L1) (formula L2)))
   (description "Forward application of EQUIV-Introduction."))

(tac~deftactic NICTAC-EQUIV-I-b NICTAC-EQUIV-I (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-left-implies (formula L3)))
                 (L2 (batac=compute-right-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic NICTAC-EQUIV-I-l NICTAC-EQUIV-I (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-left-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3))
		   (logic~implication-p (formula l2))
		   (batac=right-implies-p (formula L2) (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic NICTAC-EQUIV-I-r NICTAC-EQUIV-I (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=compute-right-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3))
		   (logic~implication-p (formula l1))
		   (batac=left-implies-p (formula L1) (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic NICTAC-EQUIV-I-a NICTAC-EQUIV-I (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (batac=equive-a-p (formula L2) (formula L3) (formula L1)))
   (description "Backward application of EQUIV-Elimination."))


(defun batac=expand-NICTAC-EQUIV-I (outline parameters)
  (let* ((LtR (second outline))
	 (RtL (third outline))
	 (equiv (first outline))
	 (=def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive))))
    
    (tacl~init outline)
    (let ((defni-res (tacl~apply 'defni (list equiv nil) (list definiendum definiens (pos~add-front 0)))))
      (if (and (term~alpha-equal (car (data~appl-arguments (node~formula (second defni-res))))
				 (node~formula LtR))
	       (term~alpha-equal (cadr (data~appl-arguments (node~formula (second defni-res))))
				 (node~formula RtL)))
	  (tacl~apply 'andi (list (second defni-res) LtR RtL) nil)
	(tacl~sequence	   
	 (defne1-res ('defne (list nil LtR) (list definiendum definiens (pos~add-front 0))))  ;; kann weg sobald
									;; defni&defne mit
									;; positionen  ---
									;; --> keim-3 
     
	 (defne2-res ('defne (list nil RtL) (list definiendum definiens (pos~add-front 0))))
	 (andi-res ('andi (list (second defni-res) (car defne1-res) (car defne2-res)) nil)))))      ;;; conj LtR RtL
    (tacl~end)))

(com~defcommand NICTAC-EQUIV-I
  (argnames equiv limp rimp)
  (argtypes ndline ndline ndline)
  (arghelps "Equivalence to split" "Left-to-right implication of an equivalence" "Right-to-left implication of an equivalence")
  (function batac=NICTAC-EQUIV-I)
  (frag-cats tactics introduction base)
  (defaults batac=NICTAC-EQUIV-I-defaults)
  (log-p T)
  (help "Split an equivalence into its two implications."))

(defun batac=NICTAC-EQUIV-I (equiv limp rimp)
  (infer~compute-outline 'NICTAC-EQUIV-I (list equiv limp rimp) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;              Special NIC Weaken tactics                 ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To do: Modify weaken in directed equality substitution

(infer~deftactic NICTAC-WEAKEN
	       (outline-mappings (((existent existent) NICTAC-WEAKEN-a)))
	       (expansion-function nictac=expand-NICTAC-WEAKEN)
	       (help "The NIC WEAKENing tactic."))

(tac~deftactic NICTAC-WEAKEN-a NICTAC-WEAKEN (in base)
   (premises P)
   (conclusions C)
   (sideconditions (nictac=NICTAC-WEAKEN-a-sidecond P C))
   (description "Application of the NIC Waekening tactic."))

(defun nictac=NICTAC-WEAKEN-a-sidecond (P C)
  (and 
   (term~alpha-equal (node~formula P) (node~formula C))
   (pds~node-supported-by-p C P)))

(com~defcommand NICTAC-WEAKEN
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "Line to justify" "Already-derived line")
  (function nictac=NICTAC-WEAKEN)
  (frag-cats rules structural elimination introduction)
  (log-p T)
  (help "Justify a line from an unifiable earlier-derived line."))

(defun nictac=NICTAC-WEAKEN (lower upper)
  (infer~compute-outline 'NICTAC-WEAKEN (list lower upper) nil))


(infer~deftactic NICTAC-FOWEAKEN
	       (outline-mappings (((existent existent) NICTAC-FOWEAKEN-a)))
	       (expansion-function nictac=expand-NICTAC-FOWEAKEN)
	       (parameter-types anything)
	       (help "The NIC FOWEAKENing tactic."))

(tac~deftactic NICTAC-FOWEAKEN-a NICTAC-FOWEAKEN (in base)
   (parameters (uni subst+substitution "The unifier."))       
   (premises P)
   (conclusions C)
   (sideconditions (nictac=NICTAC-FOWEAKEN-a-sidecond P C uni))
   (description "Application of the NIC Waekening tactic."))

(defun nictac=NICTAC-FOWEAKEN-a-sidecond (P C uni)
  (let ((res (and (subst~p uni)
		  (term~alpha-equal (subst~apply uni (node~formula P))
			      (subst~apply uni (node~formula C))))))
    (when res (progn (nic~add-subst uni C)
		     t))))
  

(com~defcommand NICTAC-FOWEAKEN
  (argnames lowerline upperline uni)
  (argtypes ndline ndline anything)
  (arghelps "Line to justify" "Already-derived line" "Unifier")
  (function nictac=NICTAC-FOWEAKEN)
  (frag-cats tactics structural elimination introduction)
  (log-p T)
  (help "Justify a line from an unifiable earlier-derived line."))

(defun nictac=NICTAC-FOWEAKEN (lower upper uni)
  (infer~compute-outline 'NICTAC-FOWEAKEN (list lower upper) (list uni)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;      Additional Tactics for treating HO Concepts        ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic set-ext-expand
		 (outline-mappings (((existent existent) set-ext-expand-a)
                                    ((nonexistent existent) set-ext-expand-f)
                                    ((existent nonexistent) set-ext-expand-b)
				   ))
                 (expansion-function batac=expand-set-ext-expand)
		 (help "Set-Extensionality."))

(defun batac=set-ext-expand (line1 line2)
  (infer~compute-outline 'set-ext-expand (list line1 line2) nil))

(com~defcommand set-ext-expand
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The expanded conclusion line" "The contracted premise line")
  (function batac=set-ext-expand)
  (frag-cats tactics base elimination nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Set-Extensionality"))

(tac~deftactic set-ext-expand-f set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=set-ext-expand-f (formula L1))))
   (sideconditions (batac~set-equation-p (formula L1)))
   (description "Set-Extensionality in forward direction."))

(defun batac~set-equation-p (formula)
  (when (logic~equality-p formula)
    (let ((type-left (data~annotation (first (data~appl-arguments formula)))))
      (and (type~func-p type-left) (type~o-p (data~c-range type-left))))))

(defun batac~set-ext-contract-ap (l1 l2)
  (term~alpha-equal l1 (batac=set-ext-contract-b l2)))

(defun batac=set-ext-expand-f (l1)
  (let* ((left (first (data~appl-arguments l1)))
	 (right (second (data~appl-arguments l1)))
	 (argtype (data~c-domain (data~annotation left)))
	 (var-of-argtype (term~variable-create (gensym) argtype))
	 (in (th~definition-constant
	      (th~find-assumption 'in (th~find-theory 'typed-set))))
	 )
    (logic~quantification-create
     (logic~universal-quantor :name :forall)
     var-of-argtype
     (term~appl-create
      (logic~equivalence-constant)
      (list 
       (term~appl-create in (list var-of-argtype left))
       (term~appl-create in (list var-of-argtype right)))))))
					      

(tac~deftactic set-ext-expand-b set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=set-ext-expand-b (formula L2))))
   (sideconditions (batac~expanded-set-extensionality-p (formula L2)))
   (description "Set-Ext-Extensionality in backward direction."))


(defun batac~expanded-set-extensionality-p (formula)
  (when (nic~universal-quantification-p formula)
    (let ((scope (logic~quantification-scope formula))
	  (bound-var (logic~quantification-bound-variable formula)))
      (when (logic~conjunction-p scope)
	(let ((left (first (data~appl-arguments scope)))
	      (right (second (data~appl-arguments scope))))
	  (when (and (data~appl-p left) (data~appl-p right))
	    (let ((head-left (data~appl-function left))
		  (head-right (data~appl-function right))
		  (in (data~schema-range (th~definition-constant
					  (th~find-assumption 'in (th~find-theory 'typed-set))))))
	      (when (and (term~alpha-equal in head-left)
			 (term~alpha-equal in head-right))
		(let ((pred-left (second (data~appl-arguments left)))
		      (pred-right (second (data~appl-arguments right)))
		      (arg-left (first (data~appl-arguments left)))
		      (arg-right (first (data~appl-arguments right))))
		  (when (and (term~alpha-equal arg-left bound-var)
			     (term~alpha-equal arg-right bound-var))
		    (values t pred-left pred-right)))))))))))
		      
	      
(defun batac=set-ext-expand-b (formula)
  (multiple-value-bind (flag pred-left pred-right)
      (batac~expanded-set-extensionality-p formula)
    (when flag (term~appl-create (logic~equality-constant) (list pred-left pred-right)))))

(tac~deftactic set-ext-expand-a set-ext-expand (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=set-ext-expand-ap (formula L1) (formula L2)))
   (description "Set-Extensionality in test direction."))

(defun batac=set-ext-expand-ap (l1 l2)
  (term~alpha-equal l2 (batac=set-ext-expand-f l1)))



(defun batac=expand-set-ext-expand (outline parameters)
;  (let ((precond (second outline))  
;        (conc (first outline)))     
;    (tacl~init outline)
;    (tacl~sequence
;     (dummy ('exte (list conc precond) nil)))
;    (tacl~end)))
  (omega~message "Expansion function has still to be implemented ... Chris"))



(infer~deftactic set-ext-contract
		 (outline-mappings (((existent existent) set-ext-contract-a)
                                    ((nonexistent existent) set-ext-contract-f)
				    ((existent nonexistent) set-ext-contract-b)
				   ))
                 (expansion-function batac=contract-set-ext-expand)
		 (help "Set-Extensionality."))

(defun batac=set-ext-contract (line1 line2)
  (infer~compute-outline 'set-ext-contract (list line1 line2) nil))

(com~defcommand set-ext-contract
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The contracted conclusion line" "The expanded premise line")
  (function batac=set-ext-contract)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Set-Extensionality"))

(tac~deftactic set-ext-contract-b set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=set-ext-contract-b (formula L2))))
   (sideconditions (batac~set-equation-p (formula L2)))
   (description "Set-Extensionality in forward direction."))

(defun batac=set-ext-contract-b (l2)
  (let* ((left (first (data~appl-arguments l2)))
	 (right (second (data~appl-arguments l2)))
	 (argtype (data~c-domain (data~annotation left)))
	 (var-of-argtype (term~variable-create (gensym) argtype))
	 (in (th~definition-constant
	      (th~find-assumption 'in (th~find-theory 'typed-set))))
	 )
    (logic~quantification-create
     (logic~universal-quantor :name :forall)
     var-of-argtype
     (term~appl-create
      (logic~equivalence-constant)
      (list 
       (term~appl-create in (list var-of-argtype left))
       (term~appl-create in (list var-of-argtype right)))))))
					      
(tac~deftactic set-ext-contract-f set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=set-ext-expand-b (formula L1))))
   (sideconditions (batac~expanded-set-extensionality-p (formula L1)))
   (description "Set-Ext-Extensionality in backward direction."))


(tac~deftactic set-ext-contract-a set-ext-contract (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac~set-ext-contract-ap (formula L1) (formula L2)))
   (description "Set-Extensionality in test direction."))


(defun batac=contract-set-ext-expand (outline parameters)
;  (let ((precond (second outline))  
;        (conc (first outline)))     
;    (tacl~init outline)
;    (tacl~sequence
;     (dummy ('exte (list conc precond) nil)))
;    (tacl~end)))
  (omega~message "Expansion function has still to be implemented ... Chris"))


(com~defcommand that-i
  (argnames thati-line)
  (argtypes ndline)
  (arghelps "A line with a description operator.")
  (function batac=thati)
  (frag-cats tactics that introduction)
  (defaults batac=thati-defaults) 
  (log-p T)
  (help "Eliminating the first description operator THAT"))


(com~defcommand that-E 
  (argnames thatE-line)
  (argtypes ndline)
  (arghelps "A line with a description operator.")
  (function nic=that-E)
  (frag-cats tactics that elimination)
  (defaults batac=thate-defaults) 
  (log-p T)
  (help "Eliminating the first description operator THAT"))

(defvar nic*that-e-list nil)

(defun nic=that-E (P)
  (multiple-value-bind (outline success)
      (infer~compute-outline 'thate (list nil nil P) nil)
    (declare (ignore outline))
    (print success)
    (when success (pushnew (keim~name p) nic*that-e-list))))




(infer~deftactic set-ext-contract*
		 (outline-mappings (
				    ((existent existent) set-ext-contract*-a)
;                                   ((nonexistent existent) set-ext-contract-f)
				    ((existent nonexistent) set-ext-contract*-b)
				   ))
                 (expansion-function batac=contract-set-ext-expand)
		 (help "Set-Extensionality (also for multiple, encapsulated occurrences)."))

(defun batac=set-ext-contract* (line1 line2)
  (infer~compute-outline 'set-ext-contract* (list line1 line2) nil))

(com~defcommand set-ext-contract*
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The contracted conclusion line" "The expanded premise line")
  (function batac=set-ext-contract*)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Set-Extensionality (also for multiple, encapsulated occurrences)"))

(tac~deftactic set-ext-contract*-b set-ext-contract* (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=set-ext-expand* (formula L2))))
   (sideconditions (batac~contains-set-equations-p (formula L2)))
   (description "Set-Extensionality in forward direction."))



(defun batac~contains-set-equations-p (formula)
  (data~positions formula
		  #'(lambda (X) (batac~set-equation-p X))))


(defun batac=set-ext-expand* (form)
  (let* ((formula (data~copy form))
	 (poslist (batac~contains-set-equations-p formula))
	 (termlist (mapcar #'(lambda (X) (data~struct-at-position formula X))
			   poslist))
	 (new-term-list (mapcar #'batac=set-ext-expand-f termlist)))
    (if (not poslist)
	formula
      (progn
	(mapcar #'(lambda (pos new-term) (data~replace-at-position
					  formula
					  pos
					  new-term
					  :destructive t))
		poslist
		new-term-list)
	formula))))
      
  

(defun batac~set-ext-contract*-ap (l1 l2)
  (when (batac~contains-set-equations-p l2)
    (term~alpha-equal l1 (batac=set-ext-contract* l2))))


(tac~deftactic set-ext-contract*-a set-ext-contract* (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac~set-ext-contract*-ap (formula L1) (formula L2)))
   (description "Set-Extensionality in test direction."))







(infer~deftactic cnf-normalize
		 (outline-mappings (
				    ((existent existent) cnf-normalize-a)
;                                   ((nonexistent existent) cnf-normalize-f)
				    ((existent nonexistent) cnf-normalize-b)
				   ))
                 (expansion-function batac=cnf-normalization-expand)
		 (help "CNF normalization."))

(defun batac=cnf-normalize (line1 line2)
  (infer~compute-outline 'cnf-normalize (list line1 line2) nil))

(com~defcommand cnf-normalize
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The contracted conclusion line" "The expanded premise line")
  (function batac=cnf-normalize)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "CNF normalization"))

(tac~deftactic cnf-normalize-b cnf-normalize (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=cnf-normalization (formula L2))))
   (sideconditions (batac~cnf-normalization-p (formula L2)))
   (description "CNF normalization backwards"))



(defun batac~cnf-normalization-p (formula)
;;   (data~positions formula #'(lambda (x) (and (term~constant-p x)
;; 					     (or 
;; 					      (keim~equal "FORALL" (agplan~name x))
;; 					      (keim~equal "EXISTS" (agplan~name x)))))))
  (not (term~alpha-equal formula (batac=cnf-normalization (batac=cnf-normalization formula)))))




(defun batac=cnf-normalization (form)
  (let ((formula (hocnf~transform-in-formula (hocnf~normalize form (pds~environment omega*current-proof-plan)))))
    (dolist (var (term~free-variables formula) formula)
      (setf formula
	    (logic~quantification-create
	     (logic~universal-quantor :name :forall)
	     var
	     formula)))
    ))
  

(defun batac~cnf-normalize-ap (l1 l2)
  (term~alpha-equal l1 (batac=cnf-normalization l2)))


(tac~deftactic cnf-normalize-a cnf-normalize (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac~cnf-normalize-ap (formula L1) (formula L2)))
   (description "CNF normalization application"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Prenex Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(infer~deftactic prenex-form
		 (outline-mappings (
				    ((existent existent) prenex-form-a)
;                                   ((nonexistent existent) prenex-form-f)
				    ((existent nonexistent) prenex-form-b)
				   ))
                 (expansion-function batac=cnf-normalization-expand)
		 (help "Prenex form transformation."))

(defun batac=comp-prenex-form (line1 line2)
  (infer~compute-outline 'prenex-form (list line1 line2) nil))

(com~defcommand prenex-form
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The line to prove." "The new line in prenex form.")
  (function batac=comp-prenex-form)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (level 10)
  (log-p T)
  (help "Prenex form transformation"))

(tac~deftactic prenex-form-b prenex-form (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac~prenex-form (formula L2))))
   (sideconditions (batac~prenex-form-p (formula L2)))
   (description "Prenex form transformation backwards"))



(defun batac~prenex-form-p (formula)
;;   (data~positions formula #'(lambda (x) (and (term~constant-p x)
;; 					     (or 
;; 					      (keim~equal "FORALL" (agplan~name x))
;; 					      (keim~equal "EXISTS" (agplan~name x)))))))
  (not (term~alpha-equal formula (batac~prenex-form formula))))


(defun batac~simple-prenex-form-test (formula)
  (if (or (logic~universal-quantification-p formula) (logic~existential-quantification-p formula))
      (batac~simple-prenex-form-test (logic~quantification-scope formula))
    (batac=simple-prenex-form-test formula)))

(defun batac=simple-prenex-form-test (formula)
  (cond ((logic~atom-p formula) nil)
	((or (logic~universal-quantification-p formula) (logic~existential-quantification-p formula))
	 t)
	((logic~negation-p formula)
	 (batac=simple-prenex-form-test (car (data~appl-arguments formula))))
	((or (logic~conjunction-p formula)
	     (logic~disjunction-p formula)
	     (logic~equivalence-p formula)
	     (logic~implication-p formula))
	 (or (batac=simple-prenex-form-test (car (data~appl-arguments formula)))
	     (batac=simple-prenex-form-test (cadr (data~appl-arguments formula)))))
	(t nil)))

  

(defun batac~prenex-form-ap (l1 l2)
  (term~alpha-equal l1 (batac~prenex-form l2)))



(tac~deftactic prenex-form-a prenex-form (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac~prenex-form-ap (formula L1) (formula L2)))
   (description "Prenex form transformation application"))




