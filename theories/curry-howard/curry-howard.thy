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

(in-package :omega)

(th~deftheory CURRY-HOWARD
              (uses generic)
	      (help "A very simple set of constants for the ND Inference rules in
	      OMEGA, so that PDSes can be transformed to lambda-terms. There will
	      be one constant for any inference rule, and two for any tactic, see
	      the blue note on this."))

(th~deftype pds
  (in curry-howard)
  (arguments 0)
  (help "The type for Proof Plan data structures"))

(th~defconstant weaken
		(in curry-howard)
		(type (pds pds))
		(help "The weakening rule"))

(th~defconstant orir
		(in curry-howard)
		(type (pds pds))
		(help "The  rule for disjunction right introduction "))

(th~defconstant oril
		(in curry-howard)
		(type (pds pds))
		(help "The  rule for disjunction left introduction "))

(th~defconstant ore
		(in curry-howard)
		(type (pds (pds pds) (pds pds) pds))
		(help "The disjunction elemination rule"))

(th~defconstant andi
		(in curry-howard)
		(type (pds pds pds))
		(help "The conjunction introduction rule"))

(th~defconstant andel
		(in curry-howard)
		(type (pds pds))
		(help "The  rule for conjunction left elemination"))

(th~defconstant ander
		(in curry-howard)
		(type (pds pds))
		(help "The  rule for conjunction right elemination"))

(th~defconstant impi
		(in curry-howard)
		(type (pds (pds pds)))
		(help "The rule for implication introduction"))

(th~defconstant impe
		(in curry-howard)
		(type (pds pds pds))
		(help "The implication elimination rule, also known as modus ponens"))

(th~defconstant noti
		(in curry-howard)
		(type (pds (pds pds)))
		(help "The negation introduction rule"))

(th~defconstant note
		(in curry-howard)
		(type (pds pds pds))
		(help "The negation elimination  rule"))

(th~defconstant falsee
		(in curry-howard)
		(type (pds pds))
		(help "The falsity elimination rule"))

(th~defconstant existsi
		(in curry-howard)
		(type (pds pds))
		(help "The existential introduction rule"))

(th~defconstant existse
		(in curry-howard)
		(type (pds pds (pds pds)))
		(help "The existential elimination  rule"))

(th~defconstant foralle
		(in curry-howard)
		(type (pds pds))
		(help "The universal elimination rule"))

(th~defconstant foralli
		(in curry-howard)
		(type (pds pds))
		(help "The universal introduction rule"))

(th~defconstant lambda
		(in curry-howard)
		(type (pds pds))
		(help "The ND rule for lambda conversion"))

(th~defconstant exte
		(in curry-howard)
		(type (pds pds))
		(help "The extensionality elimination rule"))

(th~defconstant exti-neg
		(in curry-howard)
		(type (pds pds))
		(help "The ND rule for extensionality introduction under a negation"))

(th~defconstant tert-non-dat
		(in curry-howard)
		(type (pds pds))
		(help "The Axiom of the excluded middle"))

;(th~defdef notnote
;           (in curry-howard)
;           (definition 
;             (lam (Z pds)
;                  (ore (foralle tert-non-dat)
;                       weaken
;                       (lam (W pds) (falsee (note W Z))))))                         
;           (help "The tactic for double negation elimination"))
;
;(th~defdef notnoti
;           (in curry-howard)
;           (definition 
;             (lam (Z pds)
;                  (noti (note Z))))
;           (help "The tactic for double negation elimination"))

