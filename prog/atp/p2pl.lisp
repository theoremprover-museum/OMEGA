;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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



(mod~defmod P2PL 
            :uses (data keim logic res term termix type)
            :documentation "Translating higher-order res-proofs to propositional logic."
            :exports (
                      
                      p2pl~equal
                      p2pl~translate
                      
                      p2pl*codomain
                      p2pl*domain))




(defvar p2pl*domain nil)

(defvar p2pl*codomain nil)

(defun p2pl~translate (res-proof)
  (declare (edited  "21-APR-1998")
	   (authors Ameier)
	   (input   "A probably higher-order resolution proof with all its formulas"
		    "in n-normal-form.")
	   (effect  "The conclusion and the assumptions of the resolution proof are"
		    "translated into proposition logic, that means every subformula,"
		    "that starts not with a proposition logic connector is replaced"
		    "by one constant of the type o. Every new found such subformula"
		    "is compared with former found subformulas and is replaced by the"
		    "same new constant."
		    "Example: (forall x. (forall y (x y))) or (not (forall x. (forall y (x y)))"
		    "   -> A or (not A)")
	   (value   "The changed resolution proof."))

  (omega~message "Translating from higher- to propositional logic.")

  (setq p2pl*domain nil)
  (setq p2pl*codomain nil)
  
  (let* ((env (res~proof-environment res-proof)))

    (mapcar #'(lambda (node)
		(setf (termix~term node)
		      (p2pl=translate-term (termix~term node) env)))
	    (cons (res~proof-conclusion res-proof)
		  (res~proof-assumptions res-proof))))

  res-proof)

(defun p2pl=translate-term (term env)
  (declare (edited  "21-APR-1998")
	   (authors Ameier)
	   (input   "A formula and an environment.")
	   (effect  "Compare with p2pl~translate."
		    "The replaced subformulas and there replacements are added to the"
		    "global-variables p2pl*domain p2pl*codomain.")
	   (value   "The changed formula."))
  (cond ((term~appl-p term)
	 (if (or (logic~negation-p term)
		 (logic~conjunction-p term)
		 (logic~disjunction-p term)
		 (logic~equivalence-p term)
		 (logic~implication-p term))
	     (term~appl-create (data~appl-function term)
			       (mapcar #'(lambda (subterm)
					   (p2pl=translate-term subterm env))
				       (data~appl-arguments term)))
	   (p2pl=replace-term term env)))
	((term~abstr-p term)
	 (p2pl=replace-term term env))
	((or (logic~falsity-constant-p term)
	     (logic~truth-constant-p term))
	 term)
	(t
	 (p2pl=replace-term term env))))


(data~defgeneric p2pl~equal ((datum1) (datum2))
  (declare (edited  "31-JAN-1995")
           (authors AMEIER)
           (input   "two data structures")
           (effect  "none")
           (value   "true, if datum1 and datum2 represent the same structure "
		    "(including normalization! not viewing Kappa-slots), but for primitives"
		    "it is tested whether they are name-equal."))
  (:method ((datum1 data+abstr) (datum2 data+abstr))
           (or (eq datum1 datum2)
	       (let ((dom1 (data~abstr-domain datum1))
		     (dom2 (data~abstr-domain datum2)))
		 (and (p2pl~equal (data~abstr-range datum1) (data~abstr-range datum2))
		      (= (length dom1) (length dom2))
		      (every #'p2pl~equal dom1 dom2)))))
  (:method ((datum1 data+appl) (datum2 data+appl))
           (or (eq datum1 datum2)
	       (let ((args1 (data~appl-arguments datum1))
		     (args2 (data~appl-arguments datum2)))
		 (and (p2pl~equal (data~appl-function datum1) (data~appl-function datum2))
		      (= (length args1) (length args2))
		      (every #'p2pl~equal args1 args2)))))
  (:method ((datum1 data+constant) (datum2 data+constant))
	   (eq (data~constant-origin datum1)
	       (data~constant-origin datum2)))
  (:method ((datum1 data+variable) (datum2 data+variable))
	   (string= (string (keim~name datum1)) (string (keim~name datum2))))
  (:method ((datum1 data+struct) (datum2 data+struct))
	   nil))

(defun p2pl=replace-term (term env)
  (declare (edited  "21-APR-1998")
	   (authors Ameier)
	   (input   "A term and an environment.")
	   (effect  "It is checked, whether there exists a term p2pl~equal to this term in"
		    "p2pl*domain. If this is the case, the according assoc term of p2pl*codomain"
		    "is returned. Otherwise a new constant of type o is created and added into"
		    "p2pl*codomain and also the input term is added to p2pl*domain."
		    "The new created constant is then returned.")
	   (value   "Look at effect."))	
  (let* ((assoc-term (data~assoc term p2pl*domain p2pl*codomain #'p2pl~equal)))
    (if assoc-term
	assoc-term
      (let* ((new-constant (term~generate-term-primitive-with-new-name 'p2plc- (type~o) 'term+constant env)))
	(setq p2pl*domain (cons term p2pl*domain))
	(setq p2pl*codomain (cons new-constant p2pl*codomain))
	new-constant))))
	
		 
	     
