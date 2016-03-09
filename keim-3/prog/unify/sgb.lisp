;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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


(in-package "KEIM")

(mod~defmod sgb :uses (sort sctx)
            :documentation "Fundamental data structures."
            :exports (
		      sgb~construct

		      sgb~theorems
		      sgb~term
		      sgb~var-sctx

		      sgb~redundant-p
		      sgb~imit-gen-bindings
		      )
	    )

#{ \section{sorted general bindings}\label{mod:sgb}

A general binding of a sort A is a term which is most general and has the sort A.
General bindings are needed, for instance, by unification algorithms which
instantiate the variable head of a flexible term by general bindings of the same sort
as the sort of the variable head.\\
In the case of a sort system the sorts of the free variables of a general binding
are managed in a variable context which is associated to the general binding.
#}


(defclass sgb+gen-bin (term+term)
  ((var-sctx :initarg :var-sctx
	    :accessor sgb~var-sctx)
   (theorems     :initarg :theorems
		 :initform nil
		 :accessor sgb~theorems)
   (term :initarg :term
	 :accessor sgb~term)))


(defun sgb=create (term sctx &optional (thms nil))
  (declare (edited  "26-FEB-1996 15:12")
	   (authors GKLEIN)
	   (input   "A term TERM and a variable context SCTX")
	   (effect  "None.")
	   (value   "The term with variable context."))
  (make-instance 'sgb+gen-bin
		 :term term
		 :var-sctx sctx
		 :theorems thms))


;(defun sgb~join (general-bindings)
;  (declare (edited  "26-FEB-1996 15:33")
;	   (authors GKLEIN)
;	   (input   "A list of general bindings.")
;	   (effect  "None.")
;	   (value   "."))
;  (let ((terms (mapcar #'(lambda (gen-bin) (sgb~term gen-bin)) general-bindings))
;	(contexts (mapcar #'(lambda (gen-bin) (sgb~var-sctx gen-bin)) general-bindings)))
;    (sgb=create terms contexts)))

;;;
;;; All general bindings of a given sort and head symbol
;;;

(defgeneric sgb~construct (sort imithead env)
  (declare (edited  "27-FEB-1996 14:40")
	   (authors GKLEIN)
	   (input   "A sort SORT and a constant term IMITHEAD.")
	   (effect  "None.")
	   (value   "A list of all general bindigs of sort SORT which approximate the head IMITHEAD."
		    "If IMITHEAD is NIL then the general bindings do not approximate a certain head."))
  (:method ((sort sort+intersection) imithead (env sort+environment))
	   (sgb=gen-bin-combine (mapcar #'(lambda (sort1) (sgb~construct sort1 imithead env))
				       (sort~intersection-members sort))))
  (:method ((sort sort+sort) imithead (env sort+environment))
	   (let* ((thms nil)
		  (proj-bins (sgb=proj-bindings sort env thms))
		  (imit-term-decls (if imithead
				       (sort~top-term-decls sort imithead env)
				     (sort~all-imit-term-decls sort env)))
		  (proj-term-decls (sort~top-term-decls sort nil env))
		  (imit-td-bins (mapcar #'(lambda (term-decl)
					    (let* ((term (sort~td-term term-decl))
						   (termsort (sort~td-sort term-decl))
						   (termthm (sort~td-theorem term-decl))
						   (ma (sort~n-range-rel sort termsort
									 :test #'(lambda (x y)(data~equal x y)))))
					      (sgb~imit-gen-bindings term termsort ma sort (cons termthm thms))))
					imit-term-decls))
		  (proj-td-bins (mapcar #'(lambda (term-decl)
					    (let* ((term (sort~td-term term-decl))
						   (termsort (sort~td-sort term-decl))
						   (termthm (sort~td-theorem term-decl))
						   (ma (sort~n-range-rel sort termsort
									 :test #'(lambda (x y)(data~equal x y)))))
					      (sgb~imit-gen-bindings term termsort ma sort (cons termthm thms))))
					proj-term-decls)))
	     (when (member 'sgb (suni=trace)) (format t "~%~%SGB: imit-decls ~A proj-decls ~A" imit-term-decls proj-term-decls ))
	     (append proj-bins proj-td-bins imit-td-bins))))



;;;
;;; projection bindings
;;;


(defgeneric sgb=proj-bindings (sort env &optional thms)
  (declare (edited  "11-OCT-1995 13:41")
	   (authors GKLEIN)
	   (input   "A sort SORT.")
	   (effect  "None.")
	   (value   "A list of projection bindings without any respect to term declarations."))
  (:method ((sort data+abstr) (env sort+environment) &optional (thms nil))
	   (let ((n-range-sort (data~abstr-n-range sort))
		 (binder-sorts (data~abstr-n-domain sort)))
	     (labels
		 ((sgb=proj-bin (j sorts-to-conside)      ;consider thms for subsort-rels
	 		       (when sorts-to-conside
				 (let* ((sortj (car sorts-to-conside))
					(other-proj-bindings (sgb=proj-bin (+ j 1) (cdr sorts-to-conside)))
					(m (sort~n-range-rel n-range-sort sortj
							     :test #'(lambda (x y)(data~equal x y env)))))
				   (if m
				       (let* ((binder
					       (mapcar #'(lambda (sort)
							   (term~variable-create (gentemp "X-")
										 (sort~type-of-sort sort)))
							       binder-sorts))
					      (head-var (nth (- j 1) binder))
					      (head-sort-m-domain (sort~np-domain sortj m))
					      (h-sorts (mapcar #'(lambda (sort)
								   (data~abstr-create binder-sorts sort))
							       head-sort-m-domain))

					      (h-vars
                                               (mapcar #'(lambda (sort)
                                                                  (term~variable-create (gentemp "H-")
                                                                                        (sort~type-of-sort sort)))
                                                              h-sorts))
					      (h-arguments (mapcar #'(lambda (h-var)
								       (data~appl-create h-var binder))
								   h-vars))
					      (matrix (if h-arguments
							  (data~appl-create head-var h-arguments)
							head-var))
					      (proj-binding (data~abstr-create binder matrix))
					      (context (sctx~var-sctx-create h-vars h-sorts))
					      (gen-bin (sgb=create proj-binding context thms)))
					 (cons gen-bin other-proj-bindings))
				     other-proj-bindings)))))
	       (sgb=proj-bin 1 binder-sorts))))
  (:method ((sort data+constant) (env sort+environment) &optional (thms nil))
	   nil))

;;;
;;; imitation bindings
;;;

(defgeneric sgb~imit-gen-bindings (head headsort m sort &optional thms)
  (declare (edited  "27-FEB-1996 16:14")
	   (authors GKLEIN)
	   (input   "A head term HEAD of sort HEADSORT, a nonnegative number M, and a sort SORT.")
	   (effect  "None.")
	   (value   "A general binding of sort SORT with HEAD as head"
		    " so long as the M-th broken n-range sort of HEADSORT is eq to the n-range sort of SORT"))
  (:method (head headsort m (sort data+abstr) &optional (thms nil))
	   (let* ((binder-sorts (data~abstr-n-domain sort))
		  (binder (mapcar #'(lambda (sorti)
				       (term~variable-create (gentemp "X-")
							     (sort~type-of-sort sorti)))
				   binder-sorts))
		  (headsort-m-domain (sort~np-domain headsort m))
		  (h-sorts (mapcar #'(lambda (sortj)
				       (data~abstr-create binder-sorts sortj))
				   headsort-m-domain))
		  (h-vars
		   (mapcar #'(lambda (sort)
			       (term~variable-create (gentemp "H-") (sort~type-of-sort sort)))
				   h-sorts))
		  (h-arguments (mapcar #'(lambda (h-var)
					   (data~appl-create h-var binder))
				       h-vars))
		  (matrix (if h-arguments
			      (data~appl-create head h-arguments)
			    head))
		  (general-binding-tmp (data~abstr-create binder matrix))
		  (general-binding (beta~normalize general-binding-tmp))
		  (context (sctx~var-sctx-create h-vars h-sorts)))
	     (sgb=create general-binding context thms)))
  (:method (head headsort m (sort data+constant)  &optional (thms nil))
	   (let* ((headsort-m-domain (sort~np-domain headsort m))
		  (h-sorts headsort-m-domain)
		  (h-vars
		   (mapcar #'(lambda (sortj)
			       (term~variable-create (gentemp "H-")
						     (sort~type-of-sort sortj)))
			   h-sorts))
		  (h-arguments h-vars)
		  (matrix (if h-arguments
			      (data~appl-create head h-arguments)
			    head))
		  (general-binding-tmp matrix)
		  (general-binding (beta~normalize general-binding-tmp))
		  (context (sctx~var-sctx-create h-vars h-sorts)))
	     (sgb=create general-binding context thms))))




(defun sgb=gen-bin-combine (lists)
  (labels ((sgb=gen-bin-combine2 (list perms)
	      (if perms
		  (append (mapcar #'(lambda (elem) (cons elem (car perms))) list)
			  (sgb=gen-bin-combine2 list (cdr perms))))))
    (if (cdr lists)
	(sgb=gen-bin-combine2 (car lists) (sgb=gen-bin-combine (cdr lists)))
      (mapcar #'(lambda (elem) (list elem)) (car lists)))))


;;;
;;; print-object
;;;

(defmethod print-object ((gen-bin sgb+gen-bin) stream)
  (format stream "~A |- ~A " (sgb~var-sctx gen-bin) (sgb~term gen-bin)))





;;;;;;;;;;

(defun sgb~redundant-p (var sctx gen-bin)
  (unless (consp gen-bin)
    (let ((term (sgb~term gen-bin))
	  (sctx2 (sgb~var-sctx gen-bin)))
      (sgb~var-term-eta-alpha-equal-p var term sctx sctx2))))

(defgeneric sgb~var-term-eta-alpha-equal-p (var term sctx1 sctx2)
  (declare (edited  "13-MAR-1996 12:22")
	   (authors GKLEIN)
	   (input   "A variable VAR, a term TERM, and two variable contexts SCTX1 and SCTX2.")
	   (effect  "None.")
	   (value   "T iff TERM is eta-alpha equal to VAR in respect to the contexts SCTX1 and SCTX2."))
  (:method (var (term data+abstr) sctx1 sctx2)
	   (let ((head (data~top (data~n-range term))))
	     (when (and (sctx~var-sctx-get-component head sctx2) 
			(sctx~var-sctx-get-component var sctx1)
			(data~equal (sctx~var-sctx-get-component head sctx2) (sctx~var-sctx-get-component var sctx1)))
	       (data~variable-p (lam~eta-contract term)))))
  (:method (var (term data+variable) sctx1 sctx2)
	   (data~equal (sctx~var-sctx-get-component term sctx2) (sctx~var-sctx-get-component var sctx1)))
  (:method (var (term data+struct) sctx1 sctx2)
	   (declare (ignore var sctx1 sctx2))
	   nil)
  (:method (var (term data+struct) sctx1 sctx2)
	   (declare (ignore var sctx1 sctx2))
	   nil)
  (:method (var thing sctx1 sctx2)
	   (declare (ignore var thing sctx1 sctx2))
	   nil))

