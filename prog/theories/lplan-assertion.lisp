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

;; signed-occurrences -> (po)*
;; po -> (p (occ)*)
;; occ -> ((sign)* pos)

;; occ/(occ)*

(defun ass~occ-create (signs pos)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign list and a position.")
	   (effect  "None.")
	   (value   "The occurrence."))
  (list signs pos))

(defun ass~occ-signs (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The associated sign list."))
  (first occ))

(defun ass~occ-pos (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The associated position."))
  (second occ))

(defun ass~occ-positive-p (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "True iff the sign list is positive."))
  (sgn~positive-p (ass~occ-signs occ)))

(defun ass~occ-negative-p (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "True iff the sign list is negative."))
  (sgn~negative-p (ass~occ-signs occ)))

(defun ass~occ-negation (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (not t), if occ is the occurrence of p in t."))
  (ass~occ-create (sgn~negate (ass~occ-signs occ)) (pos~add-front 1 (ass~occ-pos occ))))

(defun ass~occ-quantor (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (quantified t), if occ is the occurrence of p in t."))
  (ass~occ-create (ass~occ-signs occ) (pos~add-front 1 (pos~add-front 0 (ass~occ-pos occ)))))

(defun ass~occ-equiv-l (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (equiv tl tr), if occ is the occurrence of p in tl."))
  (ass~occ-create (sgn~add-poly-sign (ass~occ-signs occ)) (pos~add-front 1 (ass~occ-pos occ))))

(defun ass~occ-equiv-r (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (equiv tl tr), if occ is the occurrence of p in tr."))
  (ass~occ-create (sgn~add-poly-sign (ass~occ-signs occ)) (pos~add-front 2 (ass~occ-pos occ))))

(defun ass~occ-and-or-l (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (and/or tl tr), if occ is the occurrence of p in tl."))
  (ass~occ-create (ass~occ-signs occ) (pos~add-front 1 (ass~occ-pos occ))))

(defun ass~occ-and-or-r (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (and/or tl tr), if occ is the occurrence of p in tr."))
  (ass~occ-create (ass~occ-signs occ) (pos~add-front 2 (ass~occ-pos occ))))

(defun ass~occ-implies-l (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (implies tl tr), if occ is the occurrence of p in tl."))
  (ass~occ-create (sgn~negate (ass~occ-signs occ)) (pos~add-front 1 (ass~occ-pos occ))))

(defun ass~occ-implies-r (occ)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "An occurrence.")
	   (effect  "None.")
	   (value   "The occurrence of p in (implies tl tr), if occ is the occurrence of p in tr."))
  (ass~occ-create (ass~occ-signs occ) (pos~add-front 2 (ass~occ-pos occ))))

(defun ass~occs-change (occs function)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of occurrences and a function that takes an occurrence.")
	   (effect  "None.")
	   (value   "Applies function to all occurrences."))
  (mapcar function occs))

(defgeneric ass~occs-of-pred (formula p)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula and an atom (p tl).")
	   (effect  "None.")
	   (value   "A list of all occurrence of p in formula."))
  (:method ((formula term+primitive) p)
	   (declare (ignore p))
	   nil)
  (:method ((formula term+appl) p)
	   (let ((head (data~appl-function formula)))
	     (cond ((data~equal head p) (list (ass~occ-create (list sgn*plus) (pos~empty))))
		   ((logic~negation-p formula)
		    (ass~occs-change (ass~occs-of-pred (first (data~appl-arguments formula)) p) 'ass~occ-negation))
		   ((or (logic~universal-quantor-p head) (logic~existential-quantor-p head))
		    (ass~occs-change (ass~occs-of-pred (data~abstr-range (first (data~appl-arguments formula))) p)
				     'ass~occ-quantor))
		   ((logic~equivalence-p formula)
		    (append
		     (ass~occs-change (ass~occs-of-pred (second (data~appl-arguments formula)) p) 'ass~occ-equiv-r)
		     (ass~occs-change (ass~occs-of-pred (first (data~appl-arguments formula)) p) 'ass~occ-equiv-l)))
		   ((or (logic~conjunction-p formula) (logic~disjunction-p formula))
		    (append
		     (ass~occs-change (ass~occs-of-pred (first (data~appl-arguments formula)) p) 'ass~occ-and-or-l)
		     (ass~occs-change (ass~occs-of-pred (second (data~appl-arguments formula)) p) 'ass~occ-and-or-r)))
		   ((logic~implication-p formula)
		    (append
		     (ass~occs-change (ass~occs-of-pred (second (data~appl-arguments formula)) p) 'ass~occ-implies-r)
		     (ass~occs-change (ass~occs-of-pred (first (data~appl-arguments formula)) p) 'ass~occ-implies-l)))))))

;; po/(po)*

(defun ass~po-create (p occs)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A predicate and a list of occurrences.")
	   (effect  "None.")
	   (value   "The predicate-occurrences association."))
  (list p occs))

(defun ass~po-pred (pred-occs)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A predicate-occurrences association.")
	   (effect  "None.")
	   (value   "The associated predicate."))
  (first pred-occs))

(defun ass~po-occs (pred-occs)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A predicate-occurrences association.")
	   (effect  "None.")
	   (value   "The associated occurrences."))
  (second pred-occs))

(defun ass~pos-change (ps function)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of predicate-occurrences associations and a function that takes an occurrence.")
	   (effect  "None.")
	   (value   "Applies the function to all occurrences in the predicate-occurrences associations."))
  (mapcar #'(lambda (po) (list (ass~po-pred po) (ass~occs-change (ass~po-occs po) function))) ps))

(defun ass~pos-collect (ps1 ps2)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "Two lists of predicate-occurrences associations.")
	   (effect  "None.")
	   (value   "Unites the two list and collects thereby the occurrences of a predicate in one association."))
  (if ps1
      (let* ((po (first ps1))
	     (pred (ass~po-pred po))
	     (pos (position pred (mapcar 'ass~po-pred ps2))))
	(if pos 
	    (cons (ass~po-create pred (append (ass~po-occs (first ps1)) (ass~po-occs (nth pos ps2))))
		  (ass~pos-collect (rest ps1) (remove-if #'(lambda (po) (keim~equal (ass~po-pred po) pred)) ps2)))
	  (cons (first ps1) (ass~pos-collect (rest ps1) ps2))))
    ps2))

(defun ass~pos-find-occs-of-pred (ps pred)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of predicate-occurrences associations and a predicate.")
	   (effect  "None.")
	   (value   "The occurences of the predicate."))
  (let ((pos (position pred (mapcar 'ass~po-pred ps))))
    (when pos
      (ass~po-occs (nth pos ps)))))

;; signed-occurrences

(defun ass~signed-occurrences (formula)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "The complete list of all predicate-occurrences associations, i.e. all occurrences of all predicates."))
  (let ((head (help~atom-head formula)))
    (cond 
     ((logic~negation-p formula)
      (ass~pos-change (ass~signed-occurrences (first (help~atom-subterms formula))) 'ass~occ-negation))
     ((or (logic~universal-quantor-p head) (logic~existential-quantor-p head))
      (ass~pos-change (ass~signed-occurrences (data~abstr-range (first (help~atom-subterms formula)))) 'ass~occ-quantor))
     ((logic~equivalence-p formula)
      (ass~pos-collect
       (ass~pos-change (ass~signed-occurrences (second (help~atom-subterms formula))) 'ass~occ-equiv-r)
       (ass~pos-change (ass~signed-occurrences (first (help~atom-subterms formula))) 'ass~occ-equiv-l)))
     ((or (logic~conjunction-p formula) (logic~disjunction-p formula))
      (ass~pos-collect
       (ass~pos-change (ass~signed-occurrences (first (help~atom-subterms formula))) 'ass~occ-and-or-l)
       (ass~pos-change (ass~signed-occurrences (second (help~atom-subterms formula))) 'ass~occ-and-or-r)))
     ((logic~implication-p formula)
      (ass~pos-collect
       (ass~pos-change (ass~signed-occurrences (second (help~atom-subterms formula))) 'ass~occ-implies-r)
       (ass~pos-change (ass~signed-occurrences (first (help~atom-subterms formula))) 'ass~occ-implies-l)))
     (t (list (ass~po-create head (list (ass~occ-create (list sgn*plus) (pos~empty)))))))))

;(defgeneric ass~signed-occurrences (formula)
;  (declare (edited  "25-JUN-2001")
;           (authors Scholl)
;           (input   "A formula.")
;           (effect  "None.")
;           (value   "The complete list of all predicate-occurrences associations, i.e. all occurrences of all predicates."))
;  (:method ((formula term+primitive))
;           nil)
;  (:method ((formula term+appl))
;           (let ((head (data~appl-function formula)))
;             (cond 
;              ((logic~negation-p formula)
;               (ass~pos-change (ass~signed-occurrences (first (data~appl-arguments formula))) 'ass~occ-negation))
;              ((or (logic~universal-quantor-p head) (logic~existential-quantor-p head))
;               (ass~pos-change (ass~signed-occurrences (data~abstr-range (first (data~appl-arguments formula)))) 'ass~occ-quantor))
;              ((logic~equivalence-p formula)
;               (ass~pos-collect
;                (ass~pos-change (ass~signed-occurrences (second (data~appl-arguments formula))) 'ass~occ-equiv-r)
;                (ass~pos-change (ass~signed-occurrences (first (data~appl-arguments formula))) 'ass~occ-equiv-l)))
;              ((or (logic~conjunction-p formula) (logic~disjunction-p formula))
;               (ass~pos-collect
;                (ass~pos-change (ass~signed-occurrences (first (data~appl-arguments formula))) 'ass~occ-and-or-l)
;                (ass~pos-change (ass~signed-occurrences (second (data~appl-arguments formula))) 'ass~occ-and-or-r)))
;              ((logic~implication-p formula)
;               (ass~pos-collect
;                (ass~pos-change (ass~signed-occurrences (second (data~appl-arguments formula))) 'ass~occ-implies-r)
;                (ass~pos-change (ass~signed-occurrences (first (data~appl-arguments formula))) 'ass~occ-implies-l)))
;              (t (list (ass~po-create head (list (ass~occ-create (list sgn*plus) (pos~empty))))))))))




(defun ass=create-formula-for-gamma (evars uvars prems concs binding)
  (let ((prem (help~build-conjunction (help~apply-subst-to-all binding prems)))
	(conc (help~build-conjunction (help~apply-subst-to-rest binding concs))))
    (help~create-exists evars (help~create-forall uvars (help~create-impl prem conc)))))

(defun ass~create-formula-for-conc (uvars concs binding new-binding)
  (help~create-forall uvars (help~build-conjunction (help~apply-subst-to-rest binding (help~apply-subst-to-all new-binding concs)))))

(defun ass~match-subterms (subterms1 subterms2 uvars evars &optional cvars (ubinding (subst~create nil nil))
				     (cbinding (subst~create nil nil)) mvars cstrs)
  (ass=match-subterms subterms1 subterms2 uvars evars cvars ubinding cbinding mvars cstrs))

(defgeneric ass=match-subterms (subterms1 subterms2 uvars evars cvars ubinding cbinding mvars cstrs)
  (:method ((subterms1 list) (subterms2 list) uvars evars cvars ubinding cbinding mvars cstrs)
	   ;; -----------------------------------------------------------------------------------------------------------
	   ;; lists: recursion
	   (if (= (list-length subterms1) (list-length subterms2))
	       (if subterms1
		   (multiple-value-bind (first-success first-subterms first-cvars first-ubinding first-cbinding first-mvars first-cstrs)
		       (ass=match-subterms (first subterms1) (first subterms2) uvars evars cvars ubinding cbinding mvars cstrs)
		     (when first-success
		       (multiple-value-bind (rest-success rest-subterms rest-cvars rest-ubinding rest-cbinding rest-mvars rest-cstrs)
			   (ass=match-subterms (rest subterms1) (rest subterms2) uvars evars first-cvars first-ubinding
					       first-cbinding first-mvars first-cstrs)
			 (when rest-success
			   (values t (cons first-subterms rest-subterms) rest-cvars rest-ubinding rest-cbinding
				   rest-mvars rest-cstrs)))))
		 (values t nil cvars ubinding cbinding mvars cstrs))
	     (progn
	       (omega~error ";;;ass=match-subterms: The lists of subterms have different lenghts.")
	       nil)))
  (:method ((subterm1 t) (subterm2 t) uvars evars cvars ubinding cbinding mvars cstrs)
	   (when (keim~equal (term~type subterm1) (term~type subterm2))
	     (cond
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; simple equality
	      ((keim~equal subterm1 subterm2)
	       (values t subterm1 cvars ubinding cbinding mvars cstrs))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; applications
	      ((and (term~appl-p subterm1) (term~appl-p subterm2)
		    (data~constant-p (data~appl-function subterm1)) (data~constant-p (data~appl-function subterm2))
		    (not (keim~equal (data~appl-function subterm1) (data~appl-function subterm2))))
	       nil)
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; applications: recursion
	      ((and (term~appl-p subterm1) (term~appl-p subterm2)
		    (keim~equal (data~appl-function subterm1) (data~appl-function subterm2)))
	       (ass=match-subterms (data~appl-arguments subterm1) (data~appl-arguments subterm2) uvars evars cvars ubinding cbinding
				   mvars cstrs))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; flexible metavar right
	      ;; changes ubinding, cbinding, mvars, cstrs
	      ((meta~p subterm2)
	       (cond ((find subterm1 uvars)
		      ;; subterm1 is an element of uvars
		      (let ((term-for-subterm1 (subst~apply ubinding subterm1)))
			(if (eq subterm1 term-for-subterm1)
			    ;; subterm1 is not bound in ubinding
			    (values t subterm2 cvars (subst~insert-component subterm1 subterm2 ubinding) cbinding mvars cstrs)
			  ;; subterm1 is bound in ubinding
			  (values t subterm2 cvars ubinding cbinding mvars
				  (cons (cstr~binding-create (list subterm2 term-for-subterm1)) cstrs)))))
		     (T
		      (let* ((uvars-in-subterm1 (intersection uvars (term~variables subterm1)))
			     (new-ubinding (help~bind-vars-to-new-metavars uvars-in-subterm1 ubinding))
			     (subterm1-applied (subst~apply new-ubinding subterm1)))
			(if (intersection evars (term~variables subterm1))
			    ;; subterm1 contains a variable from evars
			    (values t subterm1-applied cvars new-ubinding cbinding
				    (union mvars subterm2) cstrs)
			  ;; subterm1 contains no variable from evars
			  (let* ((cvars-in-subterm1-applied (intersection cvars (term~variables subterm1-applied)))
				 (new-cbinding (help~bind-vars-to-new-consts cvars-in-subterm1-applied cbinding))
				 (new-cstr (cstr~unify (subst~apply new-cbinding subterm1-applied) (subst~apply new-cbinding subterm2))))
			    (values t subterm2 cvars new-ubinding new-cbinding mvars (append cstrs (list new-cstr)))))))))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; abstraction right: recursion
	      ;; locally changes cvars
	      ((term~abstr-p subterm2)
	       (multiple-value-bind (new-success new-subterm new-cvars new-ubinding new-cbinding new-mvars new-cstrs)
		   (ass=match-subterms (beta~normalize (term~appl-create subterm1 (data~abstr-domain subterm2)))
				       (data~abstr-range subterm2) uvars evars (append cvars (data~abstr-domain subterm2))
				       ubinding cbinding mvars cstrs)
		 (when new-success
		   (values t (term~abstr-create (data~abstr-domain subterm2) new-subterm) 
			   (set-difference new-cvars (data~abstr-domain subterm2)) new-ubinding new-cbinding new-mvars new-cstrs))))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; abstraction left: recursion
	      ;; locally changes cvars
	      ((term~abstr-p subterm1)
	       (multiple-value-bind (new-success new-subterm new-cvars new-ubinding new-cbinding new-mvars new-cstrs)
		   (ass=match-subterms (data~abstr-range subterm1)
				       (beta~normalize (term~appl-create subterm2 (data~abstr-domain subterm1)))
				       uvars evars (append cvars (data~abstr-domain subterm1))
				       ubinding cbinding mvars cstrs)
		 (when new-success
		   (values t (term~abstr-create (data~abstr-domain subterm1) new-subterm)
			   (set-difference new-cvars (data~abstr-domain subterm1)) new-ubinding new-cbinding new-mvars new-cstrs))))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; cvar left
	      ((find subterm1 cvars)
	       (when (keim~equal subterm1 subterm2)
		 (values t subterm1 uvars evars cvars ubinding cbinding mvars cstrs)))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; cvar right
	      ((and (find subterm2 cvars) (not (keim~equal subterm1 subterm2)))
	       nil)
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; evar left
	      ;; changes mvars
	      ((find subterm1 evars)
	       (when (help~flexible-p subterm2)
		 (values t subterm1 cvars ubinding cbinding
			 (union mvars (remove-if-not 'meta~p (term~free-variables subterm2))) cstrs)))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; uvar left
	      ;; changes ubinding, cstrs
	      ((find subterm1 uvars)
	       (if (find subterm1 (subst~domain ubinding))
		   (let* ((vars (append (term~variables subterm2) (term~variables (subst~apply ubinding subterm1))))
			  (cvars-not-bounded (set-difference (intersection vars cvars) (subst~domain cbinding)))
			  (new-cbinding (help~bind-vars-to-new-consts cvars-not-bounded cbinding))
			  (new-cstr (cstr~unify (subst~apply cbinding (subst~apply ubinding subterm1)) (subst~apply cbinding subterm2))))
		     (when new-cstr
		       (values t subterm2 cvars ubinding new-cbinding mvars (append cstrs (list new-cstr)))))
		 (values t subterm2 cvars (subst~insert-component subterm1 subterm2 ubinding) cbinding mvars cstrs)))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; application with head evar
	      ((and (term~appl-p subterm1) (find (data~appl-function subterm1) evars) (help~rigid-p subterm2))
	       nil)
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; application with head uvar
	      ;; changes ubinding, cbinding, mvars, cstrs
	      ((and (term~appl-p subterm1) (find (data~appl-function subterm1) uvars))
	       (let ((head (data~appl-function subterm1))
		     (arguments (data~appl-arguments subterm1)))
		 (if (find head (subst~domain ubinding))
		     (ass=match-subterms (term~appl-create (subst~apply ubinding head) arguments) subterm2 uvars evars cvars
					 ubinding cbinding mvars cstrs)
		   (let* ((uvars-in-subterm1 (intersection uvars (term~variables subterm1)))
			  (new-ubinding (help~bind-vars-to-new-metavars uvars-in-subterm1 ubinding))
			  (subterm1-applied (subst~apply new-ubinding subterm1)))
		     (if (intersection evars (apply 'append (mapcar 'term~variables arguments)))
			 ;; One of the arguments contains a variable from evars
			 (values t subterm1-applied cvars new-ubinding cbinding
				 (union mvars (remove-if-not 'meta~p (term~free-variables subterm2))) cstrs)
		       ;; None of the arguments contains a variable from evars
		       (let* ((cvars-in-subterm1-applied (intersection cvars (term~variables subterm1-applied)))
			      (new-cbinding (help~bind-vars-to-new-consts cvars-in-subterm1-applied cbinding))
			      (new-cstr (cstr~unify (subst~apply new-cbinding subterm1-applied) (subst~apply new-cbinding subterm2))))
			 (values t subterm2 cvars new-ubinding new-cbinding mvars (append cstrs (list new-cstr)))))))))
	      ;; -----------------------------------------------------------------------------------------------------------
	      ;; flexible application right
	      ;; changes ubinding, cbinding, mvars, cstrs
	      ((and (term~appl-p subterm2) (help~flexible-p subterm2))
	       (let* ((uvars-in-subterm1 (intersection uvars (term~variables subterm1)))
		      (new-ubinding (help~bind-vars-to-new-metavars uvars-in-subterm1 ubinding))
		      (subterm1-applied (subst~apply new-ubinding subterm1)))
		 (if (intersection evars (term~variables subterm1))
		     ;; subterm1 contains a variable from evars
		     (values t subterm1-applied cvars new-ubinding cbinding
			     (union mvars (remove-if-not 'meta~p (term~free-variables subterm2))) cstrs)
		   ;; subterm1 contains no variable from evars
		   (let* ((cvars-in-subterm1-applied (intersection cvars (term~variables subterm1-applied)))
			  (new-cbinding (help~bind-vars-to-new-consts cvars-in-subterm1-applied cbinding))
			  (new-cstr (cstr~unify (subst~apply new-cbinding subterm1-applied) (subst~apply new-cbinding subterm2))))
		     (values t subterm2 cvars new-ubinding new-cbinding mvars (append cstrs (list new-cstr)))))))
	      (t
	       (omega~error "Could not compare the two terms ~A and ~A." subterm1 subterm2)
	       nil)))))

(defun ass~determine-sequent (forw sf pos signs subterms &optional uvars evars)
  (ass=determine-sequent sf pos signs subterms uvars evars forw))

(defun ass=determine-sequent (sf pos signs subterms uvars evars forw)
  (let ((formula (sf~formula sf))
	(sign (sf~sign sf)))
    (cond ((logic~atom-p formula)
	   (when (not (pos~empty-p pos)) (omega~error "Inconsistent position: ~A" pos))
	   (multiple-value-bind (success new-subterms cvars ubinding cbinding mvars cstrs)
	       (ass~match-subterms (help~atom-subterms formula) subterms uvars evars)
	     (declare (ignore cvars cbinding))
	     (when success
	       (values t nil (list (sf~create (term~appl-create (help~atom-head formula) new-subterms) sign))
		       mvars (when cstrs (cstr~conjunction cstrs)) ubinding (set-difference uvars (subst~domain ubinding))))))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((and (logic~negation-p formula) (sgn~plus-p sign))
	   (ass=determine-sequent (sf~create (first (data~appl-arguments formula)) sgn*minus) (pos~rest pos)
				  signs subterms uvars evars forw))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((and (logic~negation-p formula) (sgn~minus-p sign))
	   (ass=determine-sequent (sf~create (first (data~appl-arguments formula)) sgn*plus) (pos~rest pos)
				  signs subterms uvars evars forw))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((sf~delta-p sf)
	   (ass=determine-sequent (sf~delta-1-0 sf) (pos~rest (pos~rest pos)) signs subterms
				  (union uvars (sf~gamma-delta-vars sf)) evars forw))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((sf~gamma-p sf)
	   (when (not forw)
	     (let ((gamma-vars (sf~gamma-delta-vars sf)))
	       (multiple-value-bind (success sf-prems sf-concs mvars c binding vars)
		   (ass=determine-sequent (sf~gamma-1-0 sf) (pos~rest (pos~rest pos)) signs subterms
					  uvars (union evars gamma-vars) forw)
		 (when success
		   (let ((prems (mapcar 'sf~to-formula sf-prems))
			 (concs (mapcar 'sf~to-formula sf-concs)))
		     (values t nil
			     (list (sf~create (ass=create-formula-for-gamma gamma-vars (set-difference vars uvars)
										prems concs binding) sgn*plus))
			     mvars c binding (intersection uvars vars))))))))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((sf~beta-p sf)
	   (let* ((i (pos~first pos))
		  (beta-i (if (= i 1) (sf~beta-1 sf) (sf~beta-2 sf)))
		  (beta-2/i (if (= i 1) (sf~beta-2 sf) (sf~beta-1 sf))))
	     (multiple-value-bind (success prems concs mvars c binding vars)
		 (ass=determine-sequent beta-i (pos~rest pos) signs subterms uvars evars forw)
	       (when success
		 (values t nil (if prems prems (append concs (if forw nil (list beta-2/i)))) mvars c binding vars)))))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((sf~alpha-p sf)
	   (let* ((i (pos~first pos))
		  (alpha-i (if (= i 1) (sf~alpha-1 sf) (sf~alpha-2 sf)))
		  (alpha-2/i (if (= i 1) (sf~alpha-2 sf) (sf~alpha-1 sf))))
	     (multiple-value-bind (success prems concs mvars c binding vars)
		 (ass=determine-sequent alpha-i (pos~rest pos) signs subterms uvars evars forw)
	       (when success
		 (values t (cons (sf~negate alpha-2/i) prems) concs mvars c binding vars)))))
	  ;; -----------------------------------------------------------------------------------------------------------
	  ((sf~alpha-equiv-p sf)
	   (let* ((i (pos~first pos))
		  (first-sign (sgn~tuple-first signs))
		  (alpha-equiv-i (if (= i 1) (sf~alpha-equiv-1 sf first-sign) (sf~alpha-equiv-2 sf first-sign)))
		  (alpha-equiv-2/i (if (= i 1) (sf~alpha-equiv-2 sf first-sign) (sf~alpha-equiv-1 sf first-sign))))
	     (multiple-value-bind (success prems concs mvars c binding vars)
		 (ass=determine-sequent alpha-equiv-i (pos~rest pos) (sgn~tuple-rest signs) subterms uvars evars forw)
	       (when success
		 (values t (cons (if (sgn~plus-p sign) alpha-equiv-2/i (sf~negate alpha-equiv-2/i)) prems) concs
			 mvars c binding vars)))))
	  ;; -----------------------------------------------------------------------------------------------------------
	  (t (omega~error "Formula of unknown type: ~A" formula)))))

