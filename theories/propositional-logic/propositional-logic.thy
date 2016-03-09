;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(th~deftheory propositional-logic
              (uses base typed-set)
	      
              (type-constants form   )
	      (constants (atom (o form)) ;; Atomare Formeln sind die Grundlage der Aussagenlogik
			 (prop-form (o form))  ;; Darueber baut sich die Menge der
					       ;; aussagenlogischen Formeln auf
			 (Tsy  form)           ;; Symbole fuer Wahr und Falsch
			 (Fsy  form)			 
			 (nsy  (form form))    ;; Grundlegende Verknuepfungen der AL
			 (osy  (form (o form)));; not, or,  and
			 (asy  (form (o form)))
			 (evaluation (o (o form) form))
			 )
	      (help "Theory of objectimplementation of propositional logic."))


;; Wenn moeglich in typed-set einfuegen
(th~defdef setunit
	   (in propositional-logic)
	   (type-variables cc)
	   (definition 
	     (lam (aset (o cc))
		  (exists (lam (thing cc) (set= aset (singleton thing))))))
	   (help "Unit: A set has exactly one element."))

;; 


;; =============== Menge der Aussagelogischen Formeln: prop-form =====================
(th~defaxiom prop-form-const
	     (in propositional-logic)
	     (formula (and (prop-form Tsy) (prop-form Fsy)))
	     (help "The two constant formulae."))

(th~defaxiom prop-form-atom
	     (in propositional-logic)
	     (formula (forall (lam (f form) (implies (atom f) (in f prop-form)))))
	     (help "All atoms are propositional formulae"))

(th~defaxiom prop-form-nsy
	     (in propositional-logic)
	     (formula (forall (lam (f form) (equiv (in f prop-form) (in (nsy f) prop-form)))))
	     (help "The negation of a prop-form is a prop-form"))

(th~defaxiom prop-form-osy
	     (in propositional-logic)
	     (formula (forall (lam (fs (o form))
				   (equiv (forall (lam (fi form) 
						       (implies (in fi fs) (in fi prop-form))))
					  (in (osy fs) prop-form)))))
	     (help "The disjunction of propforms is a prop-form"))

(th~defaxiom prop-form-asy
	   (in propositional-logic)
	   (formula (forall (lam (fs (o form))
				 (equiv (forall (lam (fi form) 
						     (implies (in fi fs) (in fi prop-form))))
					(in (asy fs) prop-form)))))
	   (help "The conjunction of propforms is a prop-form"))

(th~defaxiom prop-form-ind
	   (in propositional-logic)
	   (formula (forall (lam (P (o form))
		      (implies (and (and (and (P Tsy) (P Fsy))
					 (and
					  (forall (lam (a form) (implies (atom a) (P a))))
					  (forall (lam (fm (o form))
						       (implies (forall (lam (me form) (implies (fm me) (P me))))
								(and (P (asy fm)) (P (osy fm))))))))
				    (forall (lam (n form) (implies (P n) (P (nsy n))))))
                               (set= P prop-form)))))
	   (help "Con-/Disjunctions, Negations, T, F, Atoms are all prop. formulae."))


;; ========== Literale ======================================

(th~defdef poslit
	   (in propositional-logic)
	   (definition 
	     (lam (f form) (atom f)))
	   (help "The positive literals are the atoms."))


(th~defdef neglit
	   (in propositional-logic)
	   (definition
	    (lam (f form)
		 (exists (lam (y form) (and (atom y) (= f (nsy y)))))))
	   (help "The negative literals are the negated atoms."))


(th~defdef literal
	   (in propositional-logic)
	   (definition
	    (lam (a form) (or (poslit a) (neglit a))))
	   (help "literal: a poitive or a negative literal."))

(th~defdef com-pair
	   (in propositional-logic)
	   (definition
	     (lam (li1 form) (lam (li2 form)
				  (and (and (literal li1) (literal li2))		  
				       (or (= li2 (nsy li1))
					   (= li1 (nsy li2)))))))
	   (help "2 Literals, where one is the negation of the other."))

;; =========== Disjunktionen sind zerlegbar in ihre Teilformeln

(th~defaxiom parsing-disjunction
	   (in propositional-logic)
	   (formula (forall (lam (f1 (o form)) (forall (lam (f2 (o form))
			(equiv (set= f1 f2) (= (osy f1) (osy f2))))))))
	   (help "A disjunction has unique subformulae."))

(th~defaxiom parsing-conjunction
	   (in propositional-logic)
	   (formula (forall (lam (f1 (o form)) (forall (lam (f2 (o form))
			 (equiv (set= f1 f2) (= (asy f1) (asy f2))))))))
	   (help "A conjunction has unique subformulae."))

(th~defaxiom parsing-negation
	   (in propositional-logic)
	   (formula (forall (lam (f form) (forall (lam (g form)
		      (equiv (= g f) (= (nsy g) (nsy f))))))))
	   (help "A negation has one unique subformula."))

;; =========== Formel Praedikate =============

(th~defaxiom false=osy-of-emptyset
	   (in propositional-logic)
	   (formula (= Fsy (osy emptyset)))
	   (help "osy maps the emptyset to the False-symbol."))

(th~defaxiom unit-disjunction
	   (in propositional-logic)
	   (formula (forall (lam (fs (o form))
		       (implies (setunit fs)
				(= (osy fs)
				   (that (lam (f form) (set= fs (singleton f)))))))))
	   (help "The disjunction of an singleton is the element of the singleton."))

(th~defdef disjunction
	   (in propositional-logic)
	   (definition
	     (lam (f form) (exists (lam (sf (o form))
					(and (= f (osy sf)) (not (setunit sf)))))))
	   (help "Is a formula a disjunction or Fsy?"))

;;---------

(th~defaxiom true=asy-of-emptyset
	   (in propositional-logic)
	   (formula (= Tsy (asy emptyset)))
	   (help "asy maps the emptyset to the True-symbol."))

(th~defaxiom unit-conjunction
	   (in propositional-logic)
	   (formula (forall (lam (fs (o form))
		       (implies (setunit fs)
				(= (asy fs)
				   (that (lam (f form) (set= fs (singleton f)))))))))
	   (help "The conjunction of an singleton is the element of the singleton."))

(th~defdef conjunction
	   (in propositional-logic)
	   (definition
	     (lam (f form) (exists (lam (sf (o form))
					(and (= f (asy sf)) (not (setunit sf)))))))
	   (help "Is a formula a conjunction or Tsy?"))

;; -------------

(th~defdef negation
	   (in propositional-logic)
	   (definition (lam (f form) (exists (lam (n form) (= f (nsy n))))))
	   (help "Is a formula a negation?"))

;; ========== Atomaritaet =====================

(th~defaxiom not-atom-p
	   (in propositional-logic)  
           (formula (forall (lam (f form)
			(implies (and (or (or (disjunction f) (conjunction f)) (negation f))
				      (and (not (= Tsy f)) (not (= Fsy f))))
				 (not (atom f))))))
	   (help "A formula is not atomic, if it is a negation or a con-/disjunction."))

(th~defaxiom atom-p
	   (in propositional-logic)  
           (formula (forall (lam (f form)
		    (implies (and (prop-form f) (atom f))
			     (or (or (= Tsy f) (= Fsy f))
				 (and (not (exists (lam (g form) (= f (nsy g)))))
				      (not (exists (lam (sf (o form)) 
					      (or (= f (osy sf)) (= f (asy sf))))))))))))
	   (help "A formula is atomic, if it is ist not a negation and not a con-/disjunction."))

;; ========= Teilformeln ======================

(th~defdef subform-set
	   (in propositional-logic)
	   (definition
	     (lam (f form)
		  (that (lam (fs (o form))
			     (or (and (negation f) (set= fs emptyset))
				 (or (and (disjunction f) (= f (osy fs)))
				     (or (and (conjunction f) (= f (asy fs)))
					 (and (literal f) (set= fs emptyset)))))))))
	   (help "The set of the direct subformulae of n-ary dis-/conjunction."))
           ;; theorem: (set= (subform-set (disjunction MENGE)) MENGE)

(th~defdef direct-subform
	   (in propositional-logic)
	   (definition
	     (lam (sf form) (lam (f form)
				 (or (and (negation f) (= f (nsy sf)))
				     (and (or (disjunction f) (conjunction f))
					  (in sf (subform-set f)))))))
	   (help "Is a formula subformula of another formula?"))

;(th~defdef subform
;           (in propositional-logic)
;           (definition
;             (lam (sf form) (lam (f form)
;                                 (or (= sf f)
;                                     (exists (lam (hf form) (and (direct-subform hf f)
;                                                                 (subform sf hf))))))))
;           (help "Predicate for the subformula-relation."))

;; ========== Klauseln und was dazugehoert =======

(th~defdef clause-form
	   (in propositional-logic)
	   (definition (lam (f form)
			    (or (or (literal f)(= Fsy f))
				(and (disjunction f)
				     (forall (lam (dsf form)
						  (implies (in dsf (subform-set f))
							   (literal dsf))))))))
	   (help "clause-form: disjunctions of literals."))
           ;; theorem: (clause-form Fsy)
	   ;; theorem: (set= (subform-set Fsy) empty-cl)

(th~defdef clause-set-form
	   (in propositional-logic)
	   (definition
	     (lam (f form)
		  (and (conjunction f)
		       (forall (lam (dsf form)
				    (implies (in dsf (subform-set f))
					     (clause-form dsf)))))))
	   (help "clause-set-form: predicate for formulae in cnf."))

(th~defdef cnf
	   (in propositional-logic)
	   (definition (lam (f form) (clause-set-form f)))
	   (help "cnf is the same like clause-set-form."))

;; ================================= S E M A N T I K =================================
;; Hier die Einschraenkung, dass nur Atome in Belegungen sein duerfen unnoetig
;; Also Belegungen irgendwelche Formelmengen, bei denen nur die Atomelemente von
;; Wichtigkeit sind

(th~defaxiom evaluation-axiom
           (in propositional-logic)
           (formula
                (forall (lam (f form) (forall (lam (b (o form))
	        (= (evaluation  f b)					   
                   (or (or (and (atom f) (b f))
                           (and (negation f)
                                (exists(lam (sf form)
                                   (and (direct-subform sf f) (not (evaluation sf b)))))))
                       (or (and (disjunction f)
                                (exists (lam (sf form)
                                   (and (in sf (subform-set f)) (evaluation sf b)))))
                           (and (conjunction f)
                                (forall(lam (sf form)
                                   (implies (in sf (subform-set f)) (evaluation sf b)))))))))))))
           (help "Evaluation of an formula under a valuation to T or F."))
           ;; theorem: (forall (lam (b (o form)) (evaluation Tsym)))
           ;; theorem: (forall (lam (b (o form)) (not (evaluation Fsy))))


(th~defdef unsat-form
	   (in propositional-logic)
	   (definition
	     (lam (f form)
		  (forall (lam (ev (o form)) (not (evaluation f ev))))))
	   (help "A formula is unsatifiable, iff ist's F under every valuation."))
           ;; theorem: (unsat Fsy)

