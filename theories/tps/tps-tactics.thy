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

(eval-when (load compile eval)
  (unless (com~find-category 'tps)
    (com~defcategory tps
		     (help "Tactics of the theory tps ."))))


(defun tpstac=expansion-info ()
  (omega~warn "This TPS justification did not occur in the examples before.")
  (omega~warn "Please inform Christoph Benzmueller or Volker Sorge,")
  (omega~warn "they will add an expansion definition as soon as possible."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The mapping from the TPS rules/tactics to OMEGA rules/tactics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Miscellaneous Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic tps*Hyp
		 (outline-mappings (((existent) tps*Hyp-a)))
		 (expansion-function tpstac=expand-tps*Hyp)
		 (help "Introduce a new Hypothesis."))

(tac~deftactic tps*Hyp-a tps*Hyp (in tps)
	       )

(defun tpstac=expand-tps*Hyp (outline parameters)
   (let* ((hyp-node (car outline))
	  (just (node~justification hyp-node)))
     (setf (pdsn~hyps hyp-node) (list hyp-node))
     (setf (just~method just) (infer~find-method 'hyp))
     (setf (pdsj~status just) "grounded")
     (setf (pdsj~parameters just) nil)
     (setf (just~premises just) nil)
     ))


;;;


(infer~deftactic tps*Lemma
		 (outline-mappings (((existent) tps*Lemma-a)))
		 (expansion-function tpstac=expand-tps*Lemma)
		 (help "Introduce a Lemma."))

(tac~deftactic tps*Lemma-a tps*Lemma (in tps)
	       )

(defun tpstac=expand-tps*Lemma (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)
  (tpstac=expansion-info)
  (tacl~end))


;;;


(infer~deftactic tps*Defn
		 (outline-mappings (((existent) tps*Defn-a)))
		 (expansion-function tpstac=expand-tps*Defn)
		 (help "Introduce a Lemma."))

(tac~deftactic tps*Defn-a tps*Defn (in tps)
	       )

(defun tpstac=expand-tps*Defn (outline parameters)
  (tacl~init outline)
  (tacl~sequence
   (erg1  ('defsi (list (car outline) nil) (list nil)))
   (erg2  ('defse (list (cadr erg1) (cadr outline)) (list nil))))
  (tacl~end))



;;;


(infer~deftactic tps*Same-as
		 (outline-mappings (((existent existent) tps*Same-as-a)))
		 (expansion-function tpstac=expand-tps*Same-as)
		 (help "Use the fact that two lines are identical to justify a planned line."))

(tac~deftactic tps*Same-as-a tps*Same-as (in tps)
	       )

(defun tpstac=expand-tps*Same-as (outline parameters)
  (tacl~init outline)
   (tacl~apply 'weaken (list (first outline) (second outline)) nil)
  (tacl~end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Propositional Rules 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*Assoc
		 (outline-mappings (((existent existent) tps*Assoc-a)))
		 (expansion-function tpstac=expand-tps*Assoc)
		 (help "Rule to associate a support line leftwards. Use before calling
tps*cases3 or tps*cases4."))

(tac~deftactic tps*Assoc-a tps*Assoc (in tps)
	       )

(defun tpstac=expand-tps*Assoc (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)
   (tpstac=expansion-info)
  (tacl~end))

;;;


(infer~deftactic tps*cases
		 (outline-mappings (((existent existent existent existent) tps*cases-a)))
		 (expansion-function tpstac=expand-tps*cases)
		 (help "Rule of Cases."))


(tac~deftactic tps*cases-a tps*cases (in tps)
	       )

(defun tpstac=expand-tps*Cases (outline parameters)
  (tacl~init outline)
   (tacl~apply 'ore outline nil)
  (tacl~end))


;;;

(infer~deftactic tps*Cases3
		 (outline-mappings (((existent existent existent existent existent) tps*Cases4-a)))
		 (expansion-function tpstac=expand-tps*Cases3)
		 (help "Rule of Cases."))


(tac~deftactic tps*Cases3-a tps*Cases3 (in tps)
	       )

(defun tpstac=expand-tps*Cases3 (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)
  (tpstac=expansion-info)
  (tacl~end))


;;;

(infer~deftactic tps*Cases4
		 (outline-mappings (((existent existent existent existent existent) tps*Cases4-a)))
		 (expansion-function tpstac=expand-tps*Cases4)
		 (help "Rule of Cases."))


(tac~deftactic tps*Cases4-a tps*Cases4 (in tps)
	       )

(defun tpstac=expand-tps*Cases4 (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)
   (tpstac=expansion-info)
  (tacl~end))

;;;

(infer~deftactic tps*Deduct
		 (outline-mappings (((existent existent) tps*Deduct-a)))
		 (expansion-function tpstac=expand-tps*Deduct)
		 (help "The deduction rule."))


(tac~deftactic tps*Deduct-a tps*Deduct (in tps)
	       )



(defun tpstac=expand-tps*Deduct (outline parameters)
  (let* ((conc (car outline))     ;; A
	 (false (cadr outline))   ;; bot
	 (hyp   (car
		 (set-difference (pdsn~hyps false)
				 (pdsn~hyps conc))))) ;; hypnA
    (tacl~init outline)
    (let ((result
	   (tacl~apply 'impi (list (car outline) nil) nil)))
      (tac~forget&destroy-hyp (list (second result)) hyp (third result)
			      :test 'term~alpha-equal) ;;MP: instead of the default data~equal, e.g. thm270 in theory function
      (tacl~apply 'weaken (list (second result) false) nil))
    (tacl~end)))

;;;

(infer~deftactic tps*Disj-Imp
		 (outline-mappings (((existent existent) tps*Disj-Imp-a)))
		 (expansion-function tpstac=expand-tps*Disj-Imp)
		 (help "Rule to replace a disjunction by an implication."))


(tac~deftactic tps*Disj-Imp-a tps*Disj-Imp (in tps)
	       )

(defun tpstac=expand-tps*Disj-Imp (outline parameters)
  (tacl~init outline)
   (tacl~apply 'or2imp outline nil)
  (tacl~end))


;;;

(infer~deftactic tps*Conj
		 (outline-mappings (((existent existent existent) tps*Conj-a)))
		 (expansion-function tpstac=expand-tps*RuleP) ;	   (expansion-function tpstac=expand-tps*Conj)
		 (help "Rule to infer two conjuncts from a conjunction."))

(tac~deftactic tps*Conj-a tps*Conj (in tps)
	       )


(defun tpstac=expand-tps*Conj (outline parameters)    ;;;fix this for andi and ande
    (tacl~init outline)
    (tacl~apply 'ande outline nil)
    (tacl~end))

;;;

(infer~deftactic tps*EquivImp
		 (outline-mappings (((existent existent) tps*EquivImp-a)))
		 (expansion-function tpstac=expand-tps*RuleP) ; (expansion-function tpstac=expand-tps*EquivImp)
		 (help "Rule to convert an equivalence into twin implications."))


(tac~deftactic tps*EquivImp-a tps*EquivImp (in tps)
	       )

(defun tpstac=expand-tps*EquivImp (outline parameters)
  (tacl~init outline)
   (tacl~apply 'equive outline nil)
  (tacl~end))

;;;

(infer~deftactic tps*Idisj-L
		 (outline-mappings (((existent existent) tps*Idisj-L-a)))
		 (expansion-function tpstac=expand-tps*Idisj-L)
		 (help "Introduce a disjunction (left version)."))


(tac~deftactic tps*Idisj-L-a tps*Idisj-L (in tps)
	       )

(defun tpstac=expand-tps*Idisj-L (outline parameters)
  (tacl~init outline)
   (tacl~apply 'oril outline (cdr (data~appl-arguments (node~formula (car outline)))))
  (tacl~end))

;;;

(infer~deftactic tps*Idisj-R
		 (outline-mappings (((existent existent) tps*Idisj-R-a)))
		 (expansion-function tpstac=expand-tps*Idisj-R)
		 (help "Introduce a disjunction (right version)."))


(tac~deftactic tps*Idisj-R-a tps*Idisj-R (in tps)
	       )

(defun tpstac=expand-tps*Idisj-R (outline parameters)
  (tacl~init outline)
   (tacl~apply 'orir outline (list (car (data~appl-arguments (node~formula (car outline))))))
  (tacl~end))

;;;

(infer~deftactic tps*Imp-Disj
		 (outline-mappings (((existent existent) tps*Imp-Disj-a)))
		 (expansion-function tpstac=expand-tps*Imp-Disj)
		 (help "Rule to replace an implication by an disjunction."))


(tac~deftactic tps*Imp-Disj-a tps*Imp-Disj (in tps)
	       )

(defun tpstac=expand-tps*Imp-Disj (outline parameters)
  (tacl~init outline)
   (tacl~apply 'imp2or outline nil)
  (tacl~end))

;;;

(infer~deftactic tps*ImpEquiv
		 (outline-mappings (((existent existent) tps*ImpEquiv-a)))
		 (expansion-function tpstac=expand-tps*ImpEquiv)
		 (help "Rule to convert twin implications into an equivalence."))


(tac~deftactic tps*ImpEquiv-a tps*ImpEquiv (in tps)
	       )

(defun tpstac=expand-tps*ImpEquiv (outline parameters)
  (tacl~init outline)
  (tacl~sequence
    ((conj lconj rconj) ('equivi (list (car outline) nil nil) nil))
    (dummy ('ande (list lconj rconj (cadr outline)) nil)))
  (tacl~end))

;;;

(infer~deftactic tps*Indirect
		 (outline-mappings (((existent existent) tps*Indirect-a)))
		 (expansion-function tpstac=expand-tps*Indirect)
		 (help "Rule of Indirect Proof."))


(tac~deftactic tps*Indirect-a tps*Indirect (in tps)
	       )

(defun tpstac=expand-tps*Indirect (outline parameters)
  (let* ((conc (car outline))     ;; A
	 (false (cadr outline))   ;; bot
	 (hyp   (car
		 (set-difference (pdsn~hyps false)
				 (pdsn~hyps conc))))) ;; hypnA
    (tacl~init outline)
    (let ((result
	   (tacl~apply 'indirect (list (car outline) nil) nil)))
      (tac~forget&destroy-hyp (list (second result)) hyp (third result))
      (tacl~apply 'weaken (list (second result) false) nil))
    (tacl~end)))


;;;

(infer~deftactic tps*Assume-negation
		 (outline-mappings (((existent existent) tps*Assume-negation-a)))
		 (expansion-function tpstac=expand-tps*Hyp)
		 (help "Rule of Indirect Proof."))





;;;

(infer~deftactic tps*Indirect1
		 (outline-mappings (((existent existent) tps*Indirect1-a)))
		 (expansion-function tpstac=expand-tps*Indirect1)
		 (help "Rule of Indirect Proof using one contradictory line."))


(tac~deftactic tps*Indirect1-a tps*Indirect1 (in tps)
	       )

(defun tpstac=expand-tps*Indirect1 (outline parameters)
  (tacl~init outline)
  ;(tacl~apply '??? outline nil)
  (tpstac=expansion-info)
  (tacl~end))


;;;

(infer~deftactic tps*Indirect2
		 (outline-mappings (((existent existent) tps*Indirect2-a)))
		 (expansion-function tpstac=expand-tps*Indirect2)
		 (help "Rule of Indirect Proof using two contradictory lines."))


(tac~deftactic tps*Indirect2-a tps*Indirect2 (in tps)
	       )

(defun tpstac=expand-tps*Indirect2 (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)
  (tpstac=expansion-info)
  (tacl~end))

;;;

(infer~deftactic tps*MP
		 (outline-mappings (((existent existent existent) tps*MP-a)))
		 (expansion-function tpstac=expand-tps*MP)
		 (help "Modus Ponens."))


(tac~deftactic tps*MP-a tps*MP (in tps)
	       )

(defun tpstac=expand-tps*MP (outline parameters)
  (tacl~init outline)
   (tacl~apply 'impe outline nil)
  (tacl~end))


;;;

(infer~deftactic tps*Sub-equiv
		 (outline-mappings (((existent existent existent) tps*Sub-equiv-a)))
		 (expansion-function tpstac=expand-tps*Sub-equiv)
		 (help "Substitution of equivalence. Using if the two lines are equal modulo an equivalence."))


(tac~deftactic tps*Sub-equiv-a tps*Sub-equiv (in tps)
	       )

(defun tpstac=expand-tps*Sub-equiv (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline nil)   ;;; in OMEGA notwenig !!!
  (tpstac=expansion-info)
  (tacl~end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Negation Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*Absurd
		 (outline-mappings (((existent existent existent) tps*Absurd-a)))
		 (expansion-function tpstac=expand-tps*Absurd)
		 (help "Rule of Intuitionistic Absurdity."))


(tac~deftactic tps*Absurd-a tps*Absurd (in tps)
	       )

(defun tpstac=expand-tps*Absurd (outline parameters)
  (tacl~init outline)
   (tacl~apply 'falsee outline nil)
  (tacl~end))

;;;

(infer~deftactic tps*NegElim
		 (outline-mappings (((existent existent existent) tps*NegElim-a)))
		 (expansion-function tpstac=expand-tps*NegElim)
		 (help "Rule of Negation Elimination."))


(tac~deftactic tps*NegElim-a tps*NegElim (in tps)
	       )


;;; Achtung: geaendert am 12.3:  chris ---> keim3
(defun tpstac=expand-tps*NegElim (outline parameters)
  (let ((bot (car outline)))
    (multiple-value-bind (negform posform)
       (if (and (logic~negation-p (node~formula (second outline)))
		(term~alpha-equal (car (data~appl-arguments (node~formula (second outline))))
			       (node~formula (third outline))))
	   (values (second outline) (third outline))
	 (values (third outline) (second outline)))
       (tacl~init outline)
       (tacl~apply 'note (list bot posform negform) nil)
       (tacl~end))))

;;

(infer~deftactic tps*NegIntro
		 (outline-mappings (((existent existent) tps*NegIntro-a)))
		 (expansion-function tpstac=expand-tps*NegIntro)
		 (help "Rule of Negation Introduction."))


(tac~deftactic tps*NegIntro-a tps*NegIntro (in tps)
	       )

(defun tpstac=expand-tps*NegIntro (outline parameters)
  (tacl~init outline)
   (tacl~apply 'noti outline nil)
  (tacl~end))


;;;

(infer~deftactic tps*Neg
		 (outline-mappings (((existent existent) tps*Neg-a)))
		 (expansion-function tpstac=expand-tps*Neg)
		 (help "Pull out or Push in Negation."))


(tac~deftactic tps*Neg-a tps*Neg (in tps)
	       )

(defun tpstac=expand-tps*Neg (outline parameters)
  (tacl~init outline)
  (cond ((tpstac=pushneg-a-p (node~formula (car outline)) (node~formula (cadr outline)))
	 (tacl~apply 'pushneg outline nil))
	((tpstac=pullneg-a-p (node~formula (cadr outline)) (node~formula (car outline)))
	 (tacl~apply 'pullneg outline nil))
	(t (warn "Something went wrong while expanding justification tps*Neg")))
  (tacl~end))

(defun tpstac=pushneg-a-p (pushed negA)        ;;;MP jewels pds~pull/pushneg durch batac=... ersetzt
  (and (logic~negation-p negA)
       (term~alpha-equal (pds~pushneg negA) pushed)))

(defun tpstac=pullneg-a-p (A pulled)
  (and (logic~negation-p pulled)
       (term~alpha-equal (pds~pullneg A) pulled)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Quantifier Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*ab
		 (outline-mappings (((existent existent) tps*AB-a)))
		 (expansion-function tpstac=expand-tps*AB)
		 (help "Rule to alphabetically change embedded quantified variables."))


(tac~deftactic tps*AB-a tps*ab (in tps)
	       )

(defun tpstac=expand-tps*AB (outline parameters)
  (tacl~init outline)
   (tacl~apply 'lambda outline nil)
  (tacl~end))

;;;

(infer~deftactic tps*Egen
		 (outline-mappings (((existent existent) tps*Egen-a)))
		 (parameter-types term)
		 (expansion-function tpstac=expand-tps*Egen)
		 (help "Rule of Existential Generalization."))


(tac~deftactic tps*Egen-a tps*Egen (in tps)
	       )

(defun tpstac=expand-tps*Egen (outline parameters)
  (let ((pos (data~positions (node~formula (cadr outline))
			     #'(lambda (x) (term~alpha-equal x (car parameters))))))
    (tacl~init outline)
    (tacl~apply 'existsi outline (append parameters (list pos)))
    (tacl~end)))

;;;

(infer~deftactic tps*RuleC
		 (outline-mappings (((existent existent existent) tps*RuleC-a)))
		 (expansion-function tpstac=expand-tps*RuleC)
		 (parameter-types term)
		 (help "RuleC."))


(tac~deftactic tps*RuleC-a tps*RuleC (in tps)
	       )

(defun tpstac=expand-tps*RuleC (outline parameters)
  (print outline)
  
  (let* ((rulec-node (first outline))
	 (just-rulec-node (progn (print rulec-node)
				 (node~justification rulec-node)))
	 (false-node (third outline))
	 (prem-node (second outline))
	 (just-prem-node (node~justification prem-node))
	 (choose-node
	  (find-if #'(lambda (node) 
		       (and (find prem-node (node~just-premises node))
			    (eq (node~just-method node) (infer~find-method "tps*choose"))))
		   (prob~proof-steps omega*current-proof-plan)))
;          (find nil (pdsn~hyps false-node)
;                            :test #'(lambda (dummy node)
;                                      (declare (ignore dummy))
;                                      (and (equal 'tps*choose (keim~name (node~just-method node)))
;                                           (keim~equal (node~just-premises node)
;                                                       (list prem-node))))))
	 (just-choose-node
	  (progn
	    (print choose-node)
	    (node~justification choose-node)))
	 (inst-term (car (pdsj~parameters (node~justification choose-node)))))
    

    (setf (just~method just-choose-node) (infer~find-method 'hyp))
    (setf (pdsj~status just-choose-node) "grounded")
    (setf (pdsj~parameters just-choose-node) nil)
    (setf (just~premises just-choose-node) nil)
;;    (setf (pdsj~outline-pattern just-choose-node) '("existent"))
    (setf (pdsn~hyps choose-node) (list choose-node))
    
    (setf (just~method just-rulec-node) (infer~find-method 'existse))
    (setf (pdsj~status just-rulec-node) "grounded")
    (setf (pdsj~parameters just-rulec-node) (list inst-term))

    (view~update-step view*view rulec-node)
    (view~update-step view*view prem-node)
    (view~update-step view*view choose-node)
    (view~update-step view*view false-node)
    
;    (setf (node~justification rulec-node)
;	  (pdsj~closed-just-create (infer~find-method 'existse)
;				   (list hyp-node false-node)
;				   (list inst-term)
;				   "grounded"
;				   nil ;; substitution
;				   (make-list 3 :initial-element "existent")
;				   nil))
    ))
;;;

(infer~deftactic tps*Choose
		 (outline-mappings (((existent existent) tps*Choose-a)))
		 (expansion-function tpstac=expand-tps*Choose)
		 (parameter-types term)
		 (help "Choose."))


(tac~deftactic tps*Choose-a tps*Choose (in tps)
	       )

(defun tpstac=expand-tps*Choose (outline parameters)
  (warn "tps*Choose can not be expanded solely. Expand the corresponding tps*Rulec node
instead")
)

;;;

(infer~deftactic tps*Ugen
		 (outline-mappings (((existent existent) tps*Ugen-a)))
		 (expansion-function tpstac=expand-tps*Ugen)
		 (parameter-types term)
		 (help "Rule of Universial Generalization."))


(tac~deftactic tps*Ugen-a tps*Ugen (in tps)
	       )



(defun tpstac=expand-tps*Ugen (outline parameters)
  (tacl~init outline)
   (tacl~apply 'foralli outline parameters)
  (tacl~end))

;;;

(infer~deftactic tps*UI
		 (outline-mappings (((existent existent) tps*UI-a)))
		 (expansion-function tpstac=expand-tps*UI)
		 (parameter-types term)
		 (help "Rule of Universial Instantiation."))


(tac~deftactic tps*UI-a tps*UI (in tps)
	       )

(defun tpstac=expand-tps*UI (outline parameters)
  (if (term~alpha-equal (beta~contract (term~appl-create
					 (car (data~appl-arguments (node~formula (cadr
									     outline))))
					 parameters))
		     (node~formula (car outline)))
      (progn
	(tacl~init outline)
	(tacl~apply 'foralle outline parameters)
	(tacl~end))
    (progn
      (tacl~init outline)
      (tacl~sequence
       (res1 ('foralle (list nil (cadr outline)) parameters))
       (res2 ('lambda (list (car outline) (car res1)) nil)))
      (tacl~end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Substitution Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*Subst
		 (outline-mappings (((existent existent) tps*Subst-a)))
		 (expansion-function tpstac=expand-tps*Subst)
		 (parameter-types term)
		 (help "Rule to substitute a term for a variable."))


(tac~deftactic tps*Subst-a tps*Subst (in tps)
	       )

(defun tpstac=expand-tps*Subst (outline parameters)
  (tacl~init outline)
   ;(tacl~apply '??? outline parameters)  ;;; in OMEGA notwenig !!!
   (tpstac=expansion-info)
  (tacl~end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Equality Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*Equiv-EQ
		 (outline-mappings (((existent existent) tps*Equiv-EQ-a)))
		 (expansion-function tpstac=expand-tps*Equiv-EQ)
		 (help "Rule to infer a line from one which is equal up to definitions, lambda conversion, alphabetic change of bound variables and the Leibniz definition of equality."))


(tac~deftactic tps*Equiv-EQ-a tps*Equiv-EQ (in tps)
	       )

(defun tpstac=expand-tps*Equiv-EQ (outline parameters)
  (tacl~init outline)
;  (tacl~apply 'tps*Ext= outline parameters)  ;;; in OMEGA notwenig !!!
  (cond ((logic~universal-quantification-p (node~formula (car outline)))
	 (tacl~apply 'exti outline nil))
        ((logic~equivalence-p (node~formula (car outline)))
	 (tacl~apply '=2equiv outline (list (pos~empty))))
        ((logic~equality-p (node~formula (car outline)))
	 (tacl~apply 'equiv2= outline (list (pos~empty)))))
  (tacl~end))


(infer~deftactic tps*Ext=
		 (outline-mappings (((existent existent) tps*Ext=-a)))
		 (expansion-function tpstac=expand-tps*Ext=)
		 (help "Rule of Functional Extensionality."))


(tac~deftactic tps*Ext=-a tps*Ext= (in tps)
	       )

(defun tpstac=expand-tps*Ext= (outline parameters)
  (tacl~init outline)
  (cond ((logic~universal-quantification-p (node~formula (car outline)))
	 (tacl~apply 'exti outline nil))
        ((logic~equivalence-p (node~formula (car outline)))
	 (tacl~apply '=2equiv outline (list (pos~empty))))
        ((logic~equality-p (node~formula (car outline)))
	 (tacl~apply 'equiv2= outline (list (pos~empty)))))
  (tacl~end))

;;;

(infer~deftactic tps*Ext=O
		 (outline-mappings (((existent existent) tps*Ext=O-a)))
		 (expansion-function tpstac=expand-tps*Ext=O)
		 (help "Rule of Boolean Extensionality."))


(tac~deftactic tps*Ext=O-a tps*Ext=O (in tps)
	       )

(defun tpstac=expand-tps*Ext=O (outline parameters)
  (tacl~init outline)
   (tacl~apply 'equiv2= outline (list (pos~empty))) 
  (tacl~end))


;;;

(infer~deftactic tps*Subst=
		 (outline-mappings (((existent existent existent) tps*Subst=-a)))
		 (expansion-function tpstac=expand-tps*Subst=)
		 (help "Substitution of equality. Usable when R and P are the same modulo an equality."))


(infer~deftactic tps*Sub=
		 (outline-mappings (((existent existent existent) tps*Subst=-a)))
		 (expansion-function tpstac=expand-tps*Subst=)
		 (help "Substitution of equality. Usable when R and P are the same modulo an equality."))


(tac~deftactic tps*Subst=-a tps*Subst= (in tps)
	       )

(defun tpstac=expand-tps*Subst= (outline parameters)
  (tacl~init outline)
  (tacl~apply '=subst* outline (list (tpstac=compute-=subst-positions
				      (node~formula (car outline))
				      (node~formula (cadr outline))
				      (node~formula (caddr outline)))))
  (tacl~end))


(defun tpstac=compute-=subst-positions (subst unsubst eql)
  (when (logic~equality-p eql)
    (let ((pos1-left (data~positions unsubst #'(lambda (x)
					    (data~equal (first (data~appl-arguments eql))
							x))))
	  (pos1-right (data~positions subst #'(lambda (x)
					  (data~equal (second (data~appl-arguments eql))
						      x))))
	  (pos2-left (data~positions unsubst #'(lambda (x)
						 (data~equal (second (data~appl-arguments eql))
							     x))))
	  (pos2-right (data~positions subst #'(lambda (x)
						(data~equal (first (data~appl-arguments eql))
							    x)))))
      (append (intersection pos1-left pos1-right :test #'keim~equal)
	      (intersection pos2-left pos2-right :test #'keim~equal)))))
  

(infer~deftactic =subst*
		 (outline-mappings (((existent existent existent) =subst*-a)))
		 (parameter-types position-list)
		 (expansion-function tpstac=expand-=subst*)
		 (help "Substitution of equality with multiple positions. Usable when R and P are the same modulo an equality."))


(tac~deftactic =subst*-a =subst* (in tps)
   (parameters (Poslist cons "A list of positions"))
   (premises L1 EQL)
   (conclusions L2)
   (computations )
   (sideconditions )
   (description "Substitution of equality with multiple positions."))

(defun  tpstac=expand-=subst* (outline parameters)
  (let ((newline (list (second outline)))
	(eql (third outline))
	(poslist (car parameters)))
    (tacl~init outline)
    (do* ((rest poslist (cdr rest))
	  (pos (car rest) (car rest)))
	((null rest) t)
      (setf newline
	    (if (null (rest rest))
		(tacl~apply '=subst (list (car outline) (car newline) eql) (list pos))
	      (tacl~apply '=subst (list nil (car newline) eql) (list pos)))))
    (tacl~end))) 
;;;

(infer~deftactic tps*Sym=
		 (outline-mappings (((existent existent) tps*Sym=-a)))
		 (expansion-function tpstac=expand-tps*Sym=)
		 (help "Symmetrie of equality."))


(tac~deftactic tps*Sym=-a tps*Sym= (in tps)
	       )

(defun tpstac=expand-tps*Sym= (outline parameters)
   (tacl~init outline)
   (tacl~apply '=sym outline nil) 
  (tacl~end))


;;;

(infer~deftactic tps*EquivWffs
		 (outline-mappings (((existent existent) tps*EquivWffs-a)))
		 (expansion-function tpstac=expand-tps*EquivWffs)
		 (help "Rule to assert equivalence of lines up to definition."))


(tac~deftactic tps*EquivWffs-a tps*EquivWffs (in tps)
	       )

(defun tpstac=expand-tps*EquivWffs (outline parameters)
  (let* ((formula1 (node~formula (first outline)))
	 (formula2 (node~formula (second outline)))
	 (red-formula-1 (thtac~substitute-defis formula1 nil))
	 (red-formula-2 (thtac~substitute-defis formula2 nil)))
    (tacl~init outline)
    (cond ((keim~equal formula1 red-formula-1)
	   (tacl~sequence
	    (erg1  ('defse (list (first outline) (second outline)) (list nil)))))
	  ((keim~equal formula2 red-formula-2)
	   (tacl~sequence
	    (erg2  ('defsi (list (first outline) (second outline)) (list nil)))))
	  (t
	   (tacl~sequence
	    (erg1  ('defsi (list (car outline) nil) (list nil)))
	    (erg2  ('defse (list (cadr erg1) (cadr outline)) (list nil))))))
    (tacl~end)))

;;;

(infer~deftactic tps*Assert-REFL=
		 (outline-mappings (((existent) tps*Assert-REFL=-a)))
		 (expansion-function tpstac=expand-tps*Assert-REFL=)
		 (help "Introduce reflexifity of equality."))

(tac~deftactic tps*Assert-REFL=-a tps*Assert-REFL= (in tps)
	       )

(defun tpstac=expand-tps*Assert-REFL= (outline parameters)
  (let ((formula (car (data~appl-arguments (node~formula (car outline))))))
    (tacl~init outline)
    (tacl~apply '=ref outline (list formula))
    (tacl~end)))


(infer~deftactic tps*REFL=
		 (outline-mappings (((existent existent) tps*Refl=-a)))
		 (expansion-function tpstac=expand-tps*Refl=)
		 (help "Introduce reflexifity of equality."))

(tac~deftactic tps*Refl=-a tps*Refl= (in tps)
	       )

(defun tpstac=expand-tps*Refl= (outline parameters)
  (let* ((conc (car outline))
	 (prem (cadr outline))
	 (conc-form (node~formula conc))
	 (prem-form (node~formula prem)))
    (cond ((and (logic~negation-p prem-form) ;; conc: n(true), prem: n(A=A) 
		(logic~equality-p (car (data~appl-arguments prem-form))))
	   (let ((formula (car (data~appl-arguments (car (data~appl-arguments prem-form))))))
	     (tacl~init outline)
	     (tacl~sequence
	      (=ref-res ('=ref (list nil) (list formula)))
	      (note-res ('note (list nil (car =ref-res) prem) nil))
	      (dummy ('falsee (list conc (car note-res)) nil)))
	     (tacl~end)))
	  (t
	   (tpstac=expansion-info)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lambda Conversion Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic tps*Beta-Rule
		 (outline-mappings (((existent existent) tps*Beta-Rule-a)))
		 (expansion-function tpstac=expand-tps*Beta-Rule)
		 (help "Beta conversion."))


(tac~deftactic tps*Beta-Rule-a tps*Beta-Rule (in tps)
	       )

(defun tpstac=expand-tps*Beta-Rule (outline parameters)
   (tacl~init outline)
   (tacl~apply 'lambda outline nil) 
  (tacl~end))

;;;

(infer~deftactic tps*Eta-Rule
		 (outline-mappings (((existent existent) tps*Eta-Rule-a)))
		 (expansion-function tpstac=expand-tps*Eta-Rule)
		 (help "Eta conversion."))


(tac~deftactic tps*Eta-Rule-a tps*Eta-Rule (in tps)
	       )

(defun tpstac=expand-tps*Eta-Rule (outline parameters)
   (tacl~init outline)
   (tacl~apply 'lambda outline nil) 
  (tacl~end))

;;;

(infer~deftactic tps*Lambda=
		 (outline-mappings (((existent existent) tps*Lambda=-a)))
		 (expansion-function tpstac=expand-tps*Lambda=)
		 (help "Lambda conversion."))


(tac~deftactic tps*Lambda=-a tps*Lambda= (in tps)
	       )

(defun tpstac=expand-tps*Lambda= (outline parameters)
   (tacl~init outline)
   (tacl~apply 'lambda outline nil) 
  (tacl~end))


;;;

(infer~deftactic tps*Lambda
		 (outline-mappings (((existent existent) tps*Lambda-a)))
		 (expansion-function tpstac=expand-tps*Lambda)
		 (help "Lambda conversion."))


(tac~deftactic tps*Lambda-a tps*Lambda (in tps)
	       )

(defun tpstac=expand-tps*Lambda (outline parameters)
   (tacl~init outline)
   (tacl~apply 'lambda outline nil) 
  (tacl~end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rewriting Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*Rewrite
		 (outline-mappings (((existent existent) tps*Rewrite-a)))
		 (expansion-function tpstac=expand-tps*Rewrite)
		 (help "Rewriting."))


(tac~deftactic tps*Rewrite-a tps*Rewrite (in tps)
	       )

(defun tpstac=expand-tps*Rewrite (outline parameters)
   (tacl~init outline)
   (tacl~apply 'simplify outline nil) 
  (tacl~end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Special Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic tps*RuleP
		 (outline-mappings (((existent existent) tps*RuleP-a)))
		 (expansion-function tpstac=expand-tps*RuleP)
		 (help "Rewriting."))


(tac~deftactic tps*RuleP-a tps*RuleP (in tps)
	       )


;(defun tpstac=expand-tps*RuleP (outline parameters)
;  (declare (ignore parameters))
;  (setf (just~method (node~justification (car outline))) (infer~find-method 'otter))
;  (setf (pdsj~status (node~justification (car outline))) "untested")
;  (setf (pdsj~parameters (node~justification (car outline))) (list t)))

(defun tpstac=expand-tps*RuleP (outline parameters)
  (declare (ignore parameters))
  (let ((node (car outline))
	(premises (just~premises (node~justification (car outline)))))
    (tacl~init outline)
    (tpstac=call-external-atp node premises)
    (tacl~end)
    (setf (pdsj~status (node~justification node)) "untested")
    (view~update-step view*view node)))


(defun tpstac=call-external-atp (node premises)
  (setf (just~method (node~justification node)) (infer~find-method 'pl-atp))
  (setf (just~premises (node~justification node)) premises)
  (setf (pdsj~parameters (node~justification node)) (list t))
  (setf (pds~node-supports node) premises))
  
;;;


(infer~deftactic tps*RuleQ
		 (outline-mappings (((existent existent) tps*RuleQ-a)))
		 (expansion-function tpstac=expand-tps*RuleQ)
		 (help "Rewriting."))


(tac~deftactic tps*RuleQ-a tps*RuleQ (in tps)
	       )


(defun tpstac=expand-tps*RuleQ (outline parameters)
  (declare (ignore parameters))
  (let* ((node (car outline))
	 (premises (just~premises (node~justification node))))
    (tacl~init outline)
    (setf (just~method (node~justification node)) (infer~find-method 'protein))
    (setf (just~premises (node~justification node)) premises)
    (setf (pdsj~parameters (node~justification node)) (list t))
    (setf (pdsj~status (node~justification node)) "untested")
    (tacl~end)))


;;;; BUGS:

;; set10.post  tps*Ugen (short version) (trace PDS~TERM-EQUAL-APPLICATION-OF-TO-P)
;; set10.post  tps*Ugen (with lambda version) 

;; subset2.post tps*Deduct (trace TACL~APPLY INFER~COMPUTE-OUTLINE-PATTERN INFER~OUTLINE-PATTERN2APPLICATION INFER~PATTERN-CHECK INFER~FIND-OUTLINE-MAPPINGS RULE~FIND-RULE TAC~FIND-TACTIC )



;;; Hack: muss spaeter weg:

;(infer~defrule impi
;               (outline-mappings (((closed closed) impi-c)
;                                  ((existent existent) impi-a)
;                                  ((existent nonexistent) impi-b)))
;               (help "The IMPLIES-Introduction rule."))
;
;
;(rule~defrule impi-a impi (in base)
;  (declarations
;   (meta-variables (A O) (B O)))
;  (conclusion (C () (implies A B)))
;  (premises (P (H) B))
;  (extra-hyps (H () A))
;  (description "Application of the Implication Introduction rule."))





