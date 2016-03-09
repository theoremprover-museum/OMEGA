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

#-keim31-new(in-package :omega)

(th~deftheory BASE
	      #+logic-new(uses generic)
	      ;;: The type constant list is introduced as a dummy type for cartesian products.
	      ;; It is needed to define and match methods. 
	      ;;: (type-variables aa)    TYPE-VARIABLEN NUR LOKAL ERLAUBT IN PROBLEMEN, DEFINITIONEN, METHODEN, usw.  
	      #-logic-new(type-constants o i list) 
	      (constants (void o)
			 (or (o o o))
			 (and (o o o))
			 (FALSE o)
			 (implies (o o o))
			 (not (o o))
			 (focus (o o))			 
					;(=def (all-types aa (o aa aa))) in generic
                         (forall (all-types aa (o (o aa))))
			 (exists (all-types aa (o (o aa))))
			 (choose-from (all-types aa (aa (o aa))))
			 (that (all-types aa (aa (o aa)))))
	      #+logic-new(connectives
			  (or disjunction)
			  (and conjunction)
			  (false falsity)
			  (implies implication)
			  (forall universal-quantor)
			  (exists existential-quantor)
			  (not negation))
	      (help "Basic higher order logic"))

(th~defaxiom tertium-non-datur
	     (in base)
	     (formula 
	      (forall (lam (X O) (or X (not X)))))
	     (help "The Axiom of the excluded middle."))

(th~defdef true
	   (in base)
	   (definition (not false))
	   #+logic-new(connective-type truth)
           (help "The constant for truth."))

(th~defdef =
	   (in base)
	   (type-variables bb)
           (definition
             (lam (X bb)
                  (lam (Y bb)
                       (forall (lam (P (o bb))
                                    (implies (P X) (P Y)))))))
	   #+logic-new(connective-type equality)
	   (help "The equality constant defined by the Leibniz definition."))

(th~defdef equiv
	   (in base)
           (definition
             (lam (A o)
                  (lam (B o)
                       (and (implies A B) (implies B A)))))
	   #+logic-new(connective-type equivalence)
	   (help "The equivalence connective."))

					      
(th~defdef atmost-one
	   (in base)
           (type-variables bb)
           (definition
             (lam (Q (o bb))
                  (forall
                   (lam (x bb)
                        (forall
                         (lam (y bb)
                              (implies (Q x)
                                       (implies (Q y)
                                                (= x y)))))))))
	   (help "The atmost quantifier."))

(th~defdef exists-unique
	   (in base)
           (type-variables bb)
           (definition
             (lam (Q (o bb))
                  (and (exists Q) (atmost-one Q))))
           (help "The quantifier for unique existence"))


(th~defaxiom ext-bool
             (in base)
             (formula
              (forall (lam (x o)
                           (forall (lam (y o)
                                        (implies (equiv x y) (= x y)))))))
             (help "Axiom for extensionality of propositions."))


(th~defaxiom ext-func
             (in base)
             (type-variables bb cc)
             (formula
              (forall (lam (f (bb cc))
                           (forall (lam (g (bb cc))
                                        (implies (forall (lam (z cc)
                                                              (= (f z) (g z))))
                                                 (= f g)))))))
             (help "Axiom for functional extensionality."))

(th~defaxiom choice-ax
             (in base)
             (type-variables bb)
             (formula
              (forall (lam (G (o bb))
                           (implies (exists (lam (x bb)  (G x)))
                                    (G (choose-from G))))))
             (help "Axiom of choice."))


;; Description operator

(th~defaxiom that-ax
             (in base)
             (type-variables bb)
             (formula (forall (lam (x bb)
				(= (that (= x)) x))))
             (help "Axiom for description operator that."))

;;(th~defsimplifier that-simp
;;                  (in base)
;;                  (equation that-ax)
;;                  (direction lr)
;;                  (help "Simplify using the axion of descriptions."))

