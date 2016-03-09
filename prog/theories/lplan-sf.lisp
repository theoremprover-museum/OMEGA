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

;; signed-formulas

(defclass sf+sf ()
  ((formula     :initarg :formula
		:initform nil
		:accessor sf~formula)
   (sign        :initarg :sign
		:initform sgn*plus
		:accessor sf~sign)))

(defmethod print-object ((object sf+sf) stream)
  (format stream "~A~A" (sf~formula object) (sf~sign object)))

(defun sf~create (formula sign)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula and a sign.")
	   (effect  "None.")
	   (value   "The signed formula."))
  (make-instance 'sf+sf :formula formula :sign sign))

(defun sf~plus-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the sign is positive."))
  (sgn~plus-p (sf~sign sf)))

(defun sf~minus-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the sign is negative."))
  (sgn~minus-p (sf~sign sf)))

(defun sf~p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A thing.")
	   (effect  "None.")
	   (value   "True iff the thing is a signed formula."))
  (and (listp sf) (term~p (first sf))))

(defun sf~list-p (sf-list)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A thing.")
	   (effect  "None.")
	   (value   "True iff the thing is a list of signed formulas."))
  (and (listp sf-list) (sf~p (first sf-list))))

(defun sf~to-formula (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "The formula if sign is positive, otherwise the negated formula."))
  (let ((formula (sf~formula sf))
	(sign (sf~sign sf)))
    (if (sgn~plus-p sign)
	formula
      (help~create-not formula))))

(defun sf~id (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "The signed formula."))
  sf)

(defun sf~negate (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "A signed formula with negated sign."))
  (if (sgn~plus-p (sf~sign sf))
      (sf~create (sf~formula sf) sgn*minus)
    (sf~create (sf~formula sf) sgn*plus)))

(defun sf~alpha-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the signed formula is an alpha formula."))
  (let ((formula (sf~formula sf))
	(sign (sf~sign sf)))
    (when (or (and (sgn~plus-p sign) (logic~implication-p formula))
	      (and (sgn~plus-p sign) (logic~disjunction-p formula))
	      (and (sgn~minus-p sign) (logic~conjunction-p formula)))
      t)))

(defun sf~alpha-1 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha formula.")
	   (effect  "None.")
	   (value   "The first signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (first (data~appl-arguments formula))))
    (cond ((or (logic~implication-p formula) (logic~conjunction-p formula))
	   (sf~create sub sgn*minus))
	  ((logic~disjunction-p formula)
	   (sf~create sub sgn*plus))
	  (t (omega~error "~A is not an alpha formula." formula)))))
  
(defun sf~alpha-2 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha formula.")
	   (effect  "None.")
	   (value   "The second signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (second (data~appl-arguments formula))))
    (cond ((or (logic~implication-p formula) (logic~disjunction-p formula))
	   (sf~create sub sgn*plus))
	  ((logic~conjunction-p formula)
	   (sf~create sub sgn*minus))
	  (t (omega~error "~A is not an alpha formula." formula)))))

(defun sf~alpha-subformulas (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha formula.")
	   (effect  "None.")
	   (value   "A list of the two alpha subformulas."))
  (list (sf~alpha-1 sf) (sf~alpha-2 sf)))

(defun sf~beta-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the signed formula is a beta formula."))
  (let ((formula (sf~formula sf))
	(sign (sf~sign sf)))
    (when (or (and (sgn~minus-p sign) (logic~implication-p formula))
	      (and (sgn~minus-p sign) (logic~disjunction-p formula))
	      (and (sgn~plus-p sign) (logic~conjunction-p formula)))
      t)))

(defun sf~beta-1 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A beta formula.")
	   (effect  "None.")
	   (value   "The first signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (first (data~appl-arguments formula))))
    (cond ((or (logic~implication-p formula) (logic~conjunction-p formula))
	   (sf~create sub sgn*plus))
	  ((logic~disjunction-p formula)
	   (sf~create sub sgn*minus))
	  (t (omega~error "~A is not a beta formula." formula)))))
  
(defun sf~beta-2 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A beta formula.")
	   (effect  "None.")
	   (value   "The second signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (second (data~appl-arguments formula))))
    (cond ((or (logic~implication-p formula) (logic~disjunction-p formula))
	   (sf~create sub sgn*minus))
	  ((logic~conjunction-p formula)
	   (sf~create sub sgn*plus))
	  (t (omega~error "~A is not a beta formula." formula)))))

(defun sf~beta-subformulas (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A beta formula.")
	   (effect  "None.")
	   (value   "A list of all recursive beta subformulas."))
  (cond ((sf~beta-p sf)
	 (append (sf~beta-subformulas (sf~beta-1 sf)) (sf~beta-subformulas (sf~beta-2 sf))))
	((and (sf~alpha-equiv-p sf) (sgn~plus-p sf))
	 (list (sf~alpha-equiv-beta-sub-1 sf) (sf~alpha-equiv-beta-sub-2 sf)))
	(t
	 (list sf))))

(defun sf~gamma-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the signed formula is a gamma formula."))
  (let* ((formula (sf~formula sf))
	 (head (data~appl-function formula))
	 (sign (sf~sign sf)))
    (when (or (and (sgn~minus-p sign) (logic~universal-quantor-p head))
	      (and (sgn~plus-p sign) (logic~existential-quantor-p head)))
      t)))

(defun sf~gamma-1-0 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A gamma formula.")
	   (effect  "None.")
	   (value   "The signed subformula."))
  (let* ((formula (sf~formula sf))
	 (head (data~appl-function formula))
	 (sub (data~abstr-range (first (data~appl-arguments formula)))))
    (cond ((logic~universal-quantor-p head)
	   (sf~create sub sgn*minus))
	  ((logic~existential-quantor-p head)
	   (sf~create sub sgn*plus))
	  (t (omega~error "~A is not a gamma formula." formula)))))

(defun sf~delta-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the signed formula is a delta formula."))
  (let* ((formula (sf~formula sf))
	 (head (data~appl-function formula))
	 (sign (sf~sign sf)))
    (when (or (and (sgn~plus-p sign) (logic~universal-quantor-p head))
	      (and (sgn~minus-p sign) (logic~existential-quantor-p head)))
      t)))

(defun sf~delta-1-0 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A delta formula.")
	   (effect  "None.")
	   (value   "The signed subformula."))
  (let* ((formula (sf~formula sf))
	 (head (data~appl-function formula))
	 (sub (data~abstr-range (first (data~appl-arguments formula)))))
    (cond ((logic~universal-quantor-p head)
	   (sf~create sub sgn*plus))
	  ((logic~existential-quantor-p head)
	   (sf~create sub sgn*minus))
	  (t (omega~error "~A is not a delta formula." formula)))))

(defun sf~gamma-delta-vars (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A gamma or delta formula.")
	   (effect  "None.")
	   (value   "The domain variables."))
  (let ((formula (sf~formula sf)))
    (data~abstr-domain (first (data~appl-arguments formula)))))

(defun sf~gamma-delta-sort (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A gamma or delta formula.")
	   (effect  "None.")
	   (value   "The sort of the first bound variable, nil if the first variable is not sorted."))
  (let* ((formula (sf~formula sf))
	 (args (data~appl-arguments formula)))
    (when (> (list-length args) 1)
      (second args))))

(defun sf~alpha-equiv-p (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A signed formula.")
	   (effect  "None.")
	   (value   "True iff the signed formula is an alpha equivalence formula."))
  (let ((formula (sf~formula sf)))
    (logic~equivalence-p formula)))

(defun sf~alpha-equiv-1 (sf sign)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha equivalence formula.")
	   (effect  "None.")
	   (value   "The first signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (first (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (cond ((and (sgn~plus-p sf-sign) (sgn~plus-p sign))
	   (sf~create sub sgn*plus))
	  ((and (sgn~plus-p sf-sign) (sgn~minus-p sign))
	   (sf~create sub sgn*minus))
	  ((and (sgn~minus-p sf-sign) (sgn~minus-p sign))
	   (sf~create sub sgn*plus))
	  ((and (sgn~minus-p sf-sign) (sgn~plus-p sign))
	   (sf~create sub sgn*minus))
	  (t (omega~error "One of the signs ~A and ~A is invalid." sign sf-sign)))))
  
(defun sf~alpha-equiv-2 (sf sign)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha equivalence formula.")
	   (effect  "None.")
	   (value   "The second signed subformula."))
  (let* ((formula (sf~formula sf))
	 (sub (second (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (cond ((and (sgn~plus-p sf-sign) (sgn~plus-p sign))
	   (sf~create sub sgn*plus))
	  ((and (sgn~plus-p sf-sign) (sgn~minus-p sign))
	   (sf~create sub sgn*minus))
	  ((and (sgn~minus-p sf-sign) (sgn~minus-p sign))
	   (sf~create sub sgn*plus))
	  ((and (sgn~minus-p sf-sign) (sgn~plus-p sign))
	   (sf~create sub sgn*minus))
	  (t (omega~error "One of the signs ~A and ~A is invalid." sign sf-sign)))))

(defun sf~alpha-equiv-beta-sub-1 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha equivalence formula.")
	   (effect  "None.")
	   (value   "The first beta-subformula."))
  (let* ((formula (sf~formula sf))
	 (first-sub (first (data~appl-arguments formula)))
	 (second-sub (second (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (sf~create (term~appl-create (logic~implication-constant) (list first-sub second-sub)) sf-sign)))

(defun sf~alpha-equiv-beta-sub-2 (sf)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An alpha equivalence formula.")
	   (effect  "None.")
	   (value   "The second beta-subformula."))
  (let* ((formula (sf~formula sf))
	 (first-sub (first (data~appl-arguments formula)))
	 (second-sub (second (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (sf~create (term~appl-create (logic~implication-constant) (list second-sub first-sub)) sf-sign)))

(defun sf~equality-p (sf)
  (let ((formula (sf~formula sf)))
    (logic~equality-p formula)))
  
(defun sf~equality-1 (sf)
  (let* ((formula (sf~formula sf))
	 (sub (first (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (sf~create sub sf-sign)))
    
(defun sf~equality-2 (sf)
  (let* ((formula (sf~formula sf))
	 (sub (second (data~appl-arguments formula)))
	 (sf-sign (sf~sign sf)))
    (sf~create sub sf-sign)))
