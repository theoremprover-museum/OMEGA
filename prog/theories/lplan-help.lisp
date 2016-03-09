; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

(in-package "OMEGA")

(defun help~literal-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a literal."))
  (or (logic~atom-p formula)
      (and (logic~negation-p formula)
	   (logic~atom-p (first (data~appl-arguments formula))))))

(defun help~composed-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a composed formula."))
  (not (help~literal-p formula)))
  
(defun help~literal-positive-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a positive literal."))
  (not (logic~negation-p formula)))

(defun help~literal-negative-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a negative literal."))
  (logic~negation-p formula))

(defgeneric help~flexible-p (term)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff the term is flexible, i.e. if it is a meta variable itself or has a meta variable as head."))
  (:method ((term meta+variable))
	   t)
  (:method ((term term+appl))
	   (meta~p (data~appl-function term)))
  (:method ((term t))
	   nil))

(defun help~rigid-p (term)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff the term is rigid, i.e. not flexible."))
  (not (help~flexible-p term)))

(defun help~rigid-literal-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a literal and rigid (as a term)."))
  (and (help~literal-p formula) (help~rigid-p formula)))

(defun help~flexible-literal-p (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is a literal and flexible (as a term)."))
  (and (help~literal-p formula) (help~flexible-p formula)))

(defgeneric help~atom-head (term)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The head symbol of the term."))
  (:method ((term term+primitive))
	   term)
  (:method ((term term+abstr))
	   (help~atom-head (data~abstr-range term)))
  (:method ((term term+appl))
	   (data~appl-function term)))

(defgeneric help~atom-subterms (term)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The subterms of the term."))
  (:method ((term term+primitive))
	   nil)
  (:method ((term term+abstr))
	   (help~atom-subterms (data~abstr-range term)))
  (:method ((term term+appl))
	   (data~appl-arguments term)))

(defun help~literal-predicate (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "The predicate of the literal."))
  (if (logic~negation-p formula)
      (help~atom-head (first (data~appl-arguments formula)))
    (help~atom-head formula)))

(defun help~literal-subterms (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "The arguments of the literal."))
  (if (logic~negation-p formula)
      (help~atom-subterms (first (data~appl-arguments formula)))
    (help~atom-subterms formula)))

(defun help~bind-vars-to-new-consts (vars binding)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of variables and a substitution.")
	   (effect  "None.")
	   (value   "Extends the substitution with new constants bound to the variables."))
  (if vars
      (let* ((var (first vars))
	     (new-name (term~generate-new-name (keim~name var) (pds~environment omega*current-proof-plan))))
	(subst~insert-component var (term~constant-create new-name (term~type var)) (help~bind-vars-to-new-consts (rest vars) binding)))
    binding))

(defun help~bind-vars-to-new-metavars (vars binding)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of variables and a substitution.")
	   (effect  "None.")
	   (value   "Extends the substitution with new meta variables bound to the variables."))
  (if vars
      (let* ((var (first vars))
	     (new-name (term~generate-new-name (keim~name var) (pds~environment omega*current-proof-plan))))
	(subst~insert-component var (meta~variable-create new-name (term~type var)) (help~bind-vars-to-new-metavars (rest vars) binding)))
    binding))

(defun help~create-forall (vars term)
  (if vars
      (term~appl-create
       (logic~universal-quantor)
       (list (term~abstr-create vars term)))
    term))
      
(defun help~create-exists (vars term)
  (if vars
      (term~appl-create
       (logic~existential-quantor)
       (list (term~abstr-create vars term)))
    term))

(defun help~create-not (term)
  (if (logic~negation-p term)
      (first (data~appl-arguments term))
    (term~appl-create (logic~negation-constant) (list term))))

(defun help~create-impl (term1 term2)
  (if (logic~truth-constant-p term1)
      term2
    (term~appl-create
     (logic~implication-constant)
     (list term1 term2)))) 

(defun help~build-conjunction (termlist)
  (if termlist
      (if (= (list-length termlist) 1)
	  (first termlist)
	(term~appl-create (logic~conjunction-constant) (list (first termlist) (help~build-conjunction (rest termlist)))))
    (logic~truth-constant)))

(defun help~build-implication (termlist)
  (if termlist
      (if (= (list-length termlist) 1)
	  (help~create-not (first termlist))
	(term~appl-create (logic~implication-constant) (list (first termlist) (help~build-implication (rest termlist)))))
    (logic~falsity-constant)))

(defun help~apply-subst-to-rest (binding termlist)
  (when termlist (cons (first termlist) (mapcar #'(lambda (term) (subst~apply binding term)) (rest termlist)))))

(defun help~apply-subst-to-all (binding termlist)
  (mapcar #'(lambda (term) (subst~apply binding term)) termlist))
