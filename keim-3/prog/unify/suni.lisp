;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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


(mod~defmod suni :uses (sort sctx sgb subst)
            :documentation "Higher order sorted unification."
            :exports (
		      suni~unify
		      suni~sortcheck
		      suni~theorems
		      )
	    )

(eval-when (load compile eval)

  (defclass suni+unification-problem ()
    ((initial-problem :initform nil
		      :initarg :initial-problem
		      :accessor suni~initial-problem)
     (previous-solutions :initform nil
			 :initarg :previous-solutions
			 :accessor suni~previous-solutions))
    (:documentation "The class of unification problems."))

  (defclass suni+sorted-up (suni+unification-problem)
    ((context :initarg :context
	      :initform (sctx~empty-sctx)
	      :accessor suni~sctx)
     (solved-part :initarg :solved-part
		  :initform nil
		  :accessor suni~solved-part)
     (unsolved-part :initarg :unsolved-part
		    :initform nil
		    :accessor suni~unsolved-part)
     (pre-unsolved-part :initarg :pre-unsolved-part
			:initform nil
			:accessor suni~pre-unsolved-part)
     (prev-problem :initarg :prev-problem
		   :initform nil
		   :accessor suni~prev-problem)
     (theorems     :initarg :theorems
		   :initform nil
		   :accessor suni~theorems)
     (problemlist  :initarg :problemlist
		   :initform nil
		   :accessor suni=problemlist)
     (senv         :initarg :senv
		   :initform nil
		   :accessor suni=senv))
    (:documentation "This is the class of sorted unification problems")))

;;;
;;;
;;;

    
(defgeneric suni~sortcheck (term sort env)
  (:method ((term term+term)(sort sort+sort)(env sort+environment))
	   (let ((type-check (type~alpha-equal (term~type term)
						(sort~type-of-sort sort))))
	     (if (and type-check (subst~empty-p type-check))
		 (multiple-value-bind (theterm thesort thectx)
		     (suni=get-ctx-term-sort term sort)
		   (let* ((thevar (term~variable-create (gentemp "sv") (term~type theterm)))
			  (result  (suni~unify (list (list theterm thevar))
					     :mode 'gen
					     :sctx (cons (list thevar thesort) thectx)
					     :env env)))
		   (when result (suni~theorems result))))
	       (error ";;;sortcheck: term ~A and sort ~A must have the same type" term sort))))
  (:method ((term term+appl)(sort null)(env sort+environment))
	   (let ((args (data~appl-c-argument term)))
	     (suni~sortcheck (car args) (data~appl-c-function term) env)))
  (:method ((term term+term)(sort term+term)(env sort+environment))
	   (let ((predsort (sort~sort-of-pred sort env)))
	     (if predsort 
		 (suni~sortcheck term (sort~sort-of-pred sort env) env)
	       (error ";;;sortcheck: term ~A is not delcared as a sort" sort)))))


(defun suni=get-ctx-term-sort (term sort)
  (if (and (data~abstr-p term)
	   (data~abstr-p sort))
      (multiple-value-bind (newterm newsort newctx)
	  (suni=get-ctx-term-sort (data~abstr-c-range term)
				  (data~abstr-c-range sort))
	(values newterm newsort (cons (list (data~abstr-c-domain term)
					    (data~abstr-c-domain sort)) newctx)))
      (values term sort nil)))

(defgeneric suni~unify (up &key (mode 'pre) sctx env)
  (:method ((up suni+sorted-up) &key (mode 'pre) sctx env)
	   (declare (ignore sctx env))
	   (cond ((suni~previous-solutions up)
		  (when (suni=problemlist-start (suni=problemlist up))
		    (let ((unifier (suni=next-unifier (suni=problemlist-next) :mode mode)))
		      (if unifier
			  (progn
			    (setf (suni=problemlist up) (suni=problemlist-problems))
			    (push unifier (suni~previous-solutions up))
			    unifier)
			(setf (suni=problemlist up) nil)))))
		 (t
		  (suni=problemlist-start up) 
		  (let ((unifier (suni=next-unifier (suni=problemlist-next) :mode mode)))
		    (when unifier
		      (setf (suni=problemlist up) (suni=problemlist-problems))
		      (push unifier (suni~previous-solutions up))
		      unifier)))))
  (:method ((up cons) &key (mode 'pre) sctx env)
	   (let* ((vars     (mapcar #'first sctx))
		  (bindings (mapcar #'second sctx))
		  (makeup   (make-instance 'suni+sorted-up
					   :context (sctx~create (if (and vars bindings)
								    (sctx~var-sctx-create vars bindings)
								  (sctx~empty-var-sctx))
								(sctx~empty-var-sctx))
					   :senv env
					   :unsolved-part up)))
	     (setf (suni~initial-problem makeup) makeup)
	     (suni~unify makeup :mode mode))))

(defun suni~subst-create (sup)
	  (let ((solved-part (suni~solved-part sup))
		(pre-unsolved-part (suni~pre-unsolved-part sup))
		(sctx (suni~sctx sup)))
	    (if pre-unsolved-part
		(suni=prsub-substitution-create (suni=pairs-domain solved-part)
					   (suni=pairs-codomain solved-part)
					   pre-unsolved-part
					   sctx)
	      (suni=sosub-substitution-create (suni=pairs-domain solved-part)
					 (suni=pairs-codomain solved-part)
					 sctx))))

(defun suni~sorted-up-create (&key init-prob prev-sol sctx solved unsolved presolved
				   prev-up conob theorems senv)
  (make-instance 'suni+sorted-up
		 :initial-problem init-prob
		 :previous-solutions prev-sol
		 :context sctx
		 :theorems theorems
		 :solved-part solved
		 :unsolved-part unsolved
		 :pre-unsolved-part presolved
		 :prev-problem prev-up
		 :conob conob
		 :senv senv))


;;;
;;; presolved pairs
;;;

(defun suni=presolved-p (term1 term2 context)
  (declare (edited  "13-NOV-1995 16:27")
	   (authors GKLEIN)
	   (input   "Two terms and a variable context.")
	   (effect  "None.")
	   (value   "t iff the terms are in presolved form."))
  (cond ((data~abstr-p term1) nil)
	((data~abstr-p term2) nil)
	((not (member (data~top (data~n-range term1)) (sctx~domain context))) nil)
	((not (member (data~top (data~n-range term2)) (sctx~domain context))) nil)
	(t
	 (let* ((sort1 (sctx~var-sctx-get-component (data~top (data~n-range term1)) context))
		(sort2 (sctx~var-sctx-get-component (data~top (data~n-range term2)) context))
		(sortlen1 (sort~sort-length sort1))
		(sortlen2 (sort~sort-length sort2))
		(typlen1 (sort~type-length sort1))
		(typlen2 (sort~type-length sort2))
		(diff1 (- typlen1 sortlen1))
		(diff2 (- typlen2 sortlen2))
		(arglist1 (if (data~appl-p term1) (data~appl-n-arguments term1)))
		(arglist2 (if (data~appl-p term2) (data~appl-n-arguments term2))))
	   (if (>= diff1 diff2)
	       (and (<= 0 (- typlen2 diff1)) #+gklein(<= diff1 (- typlen2 diff1))
		    (every 'eq (subseq arglist1 sortlen1) (subseq arglist2 (- typlen2 diff1))))
	     (and (<= 0 (- typlen1 diff2)) #+gklein(<= diff2 (- typlen1 diff2))
		  (every 'eq (subseq arglist2 sortlen2) (subseq arglist1 (- typlen1 diff2)))))))))



;;;
;;;
;;;


(defun suni=simplify (uni-problem &key (mode 'pre))
  (declare (edited  "27-NOV-1995 21:42")
	   (authors GKLEIN)
	   (input   "An unification problem, and optionally a mode 'gen or 'pre (default).")
	   (effect  "None.")
	   (value   "The simplification-normalform of the unification problem."
		    "If the 'pre mode is on, then presolved pairs are moved from the unsolved part"
		    " into the presolved part."))
  (let* ((pos-sctx (sctx~sov (suni~sctx uni-problem)))
	 (loc-sctx (sctx~loc (suni~sctx uni-problem)))
	 (unsolved-part (suni~unsolved-part uni-problem))
	 (pair (car unsolved-part))
	 (lterm (first pair))
	 (rterm (second pair)))
    (cond
     ((null pair)
      uni-problem)
     ((or (data~abstr-p lterm) (data~abstr-p rterm))
      (let* ((type (if (data~abstr-p lterm)
		       (data~annotation (data~abstr-c-domain lterm))
		     (data~annotation (data~abstr-c-domain rterm))))
	     (new-var (term~variable-create (gentemp "X-") type))
	     (tmp-lterm (term~appl-create lterm (list new-var)))
	     (tmp-rterm (term~appl-create rterm (list new-var)))
	     (new-lterm (beta~normalize tmp-lterm))
	     (new-rterm (beta~normalize tmp-rterm))
	     (new-pair (list new-lterm new-rterm))
	     (new-loc-sctx (sctx~add-pair (list new-var type) loc-sctx))
	     (new-unsolved-part (cons new-pair (cdr unsolved-part)))
	     (new-sctx (sctx~create (sctx~sov (suni~sctx uni-problem))
				  new-loc-sctx))
	     (new-uni-problem (make-instance 'suni+sorted-up
					     :context new-sctx
					     :senv (suni=senv uni-problem)
					     :theorems (suni~theorems uni-problem)
					     :solved-part (suni~solved-part uni-problem)
					     :unsolved-part new-unsolved-part
					     :pre-unsolved-part (suni~pre-unsolved-part uni-problem)
					     :prev-problem (suni~prev-problem uni-problem)
					     :initial-problem (suni~initial-problem uni-problem))))
	(suni=simplify new-uni-problem :mode mode)))
     ((eq lterm rterm)
      (let ((new-uni-problem (make-instance 'suni+sorted-up
					    :context (suni~sctx uni-problem)
					    :solved-part (suni~solved-part uni-problem)
					    :senv (suni=senv uni-problem)
					    :theorems (suni~theorems uni-problem)
					    :unsolved-part (cdr unsolved-part)
					    :pre-unsolved-part (suni~pre-unsolved-part uni-problem)
					    :prev-problem (suni~prev-problem uni-problem)
					    :initial-problem (suni~initial-problem uni-problem))))
	(suni=simplify new-uni-problem :mode mode)))
     (t
      (let ((lsymbol (data~top (data~n-range lterm)))
	    (rsymbol (data~top (data~n-range rterm))))
	(setf lsym lsymbol)
	(setf rsym rsymbol)
	(setf psctx pos-sctx)
	(cond
	 ((and (not (member lsymbol (sctx~domain pos-sctx)))
	       (not (member rsymbol (sctx~domain pos-sctx))))   ;;; rigid-rigid pair ?
	  (if (eq lsymbol rsymbol)
	      (let* ((list1 (data~appl-n-arguments lterm))        ;;; decomposition
		     (list2 (data~appl-n-arguments rterm))
		     (new-uni-problem
		      (make-instance 'suni+sorted-up
				     :context (suni~sctx uni-problem)
				     :senv (suni=senv uni-problem)
				     :theorems (suni~theorems uni-problem)
				     :solved-part (suni~solved-part uni-problem)
				     :unsolved-part (append (mapcar #'(lambda (term1 term2)
									(list term1 term2))
								    list1 list2) (cdr unsolved-part))
				     :pre-unsolved-part (suni~pre-unsolved-part uni-problem)
				     :prev-problem (suni~prev-problem uni-problem)
				     :initial-problem (suni~initial-problem uni-problem))))
		(suni=simplify new-uni-problem :mode mode))
	    'bottom))   ;;; clash
	 (t
	  (let ((new-pair (if (not (member lsymbol (sctx~domain pos-sctx)))
			      (list rterm lterm)   ;;; we swap rigid-* pairs to *-rigid pairs
			    pair)))
	    (cond
	     ((and (suni=presolved-p lterm rterm pos-sctx) (eq mode 'pre))   ;;; we add flex-flex pair to pre-unsolved-part
	      (suni=simplify (make-instance 'suni+sorted-up
					   :context (suni~sctx uni-problem)
					   :theorems (suni~theorems uni-problem)
					   :senv (suni=senv uni-problem)
					   :solved-part (suni~solved-part uni-problem)
					   :unsolved-part (cdr unsolved-part)
					   :prev-problem (suni~prev-problem uni-problem)
					   :pre-unsolved-part (cons pair (suni~pre-unsolved-part uni-problem))
					   :initial-problem (suni~initial-problem uni-problem)) :mode mode))
	     (t ;;; flex-rigid or rigid-flex pair or not presolved flex-flex pair
	      (let ((new-uni-problem
		     (suni=simplify (make-instance 'suni+sorted-up
						  :context (suni~sctx uni-problem)
						  :theorems (suni~theorems uni-problem)
						  :senv (suni=senv uni-problem)
						  :solved-part (suni~solved-part uni-problem)
						  :unsolved-part (cdr unsolved-part)
						  :pre-unsolved-part (suni~pre-unsolved-part uni-problem)
						  :prev-problem (suni~prev-problem uni-problem)
						  :initial-problem (suni~initial-problem uni-problem)) :mode mode)))
		(if (eq new-uni-problem 'bottom)
		    'bottom
		  (make-instance 'suni+sorted-up
				 :context (suni~sctx new-uni-problem)
				 :theorems (suni~theorems uni-problem)
				 :senv (suni=senv uni-problem)
				 :solved-part (suni~solved-part new-uni-problem)
				 :unsolved-part (cons new-pair (suni~unsolved-part new-uni-problem))
				 :pre-unsolved-part (suni~pre-unsolved-part new-uni-problem)
				 :prev-problem (suni~prev-problem new-uni-problem)
				 :initial-problem (suni~initial-problem new-uni-problem))))))))))))))


;;;
;;;
;;;


(defun suni=next-unifier (uni-problem &key (mode 'pre))
  (declare (edited  "27-NOV-1995 21:07")
	   (authors GKLEIN)
	   (input   "An unification problem and optionally a mode 'gen or 'pre (default).")
	   (effect  "None.")
	   (value   "If there are no (more) solutions of the unifiction problem, nil is returned."
		    "Otherwise an pre-unificator if mode 'pre is used, "
		    "and an general unificator if mode 'gen is used."))
  (let ((sup (suni=simplify uni-problem :mode mode)))
    (when (suni=trace) (format t "~%~%Select the unification problem: ~%~A" sup))
    (if (eq sup 'bottom)
	(when (suni=problemlist-problems)
	  (suni=next-unifier (suni=problemlist-next) :mode mode))
      (let* ((unsolved-part (suni~unsolved-part sup))
	     (pair (first unsolved-part)))
	(if pair
	    (let* ((lterm (first pair))
		   (rterm (second pair))
		   (sctx (suni~sctx sup))
		   (psctx (sctx~sov sctx))
		   (ltop (data~top (data~n-range lterm)))  ;;; is a positive variable (by suni=simplify)
		   (ltop-sort (sctx~var-sctx-get-component ltop (sctx~sov (suni~sctx sup))))
		   (rtop (data~top (data~n-range rterm))))
	      (when (or (not (suni=presolved-p lterm rterm (sctx~sov (suni~sctx sup))))
			(eq mode 'gen))
	    ;;; a not presolved pair
		(let* ((all-general-bindings (sgb~construct ltop-sort rtop (suni=senv uni-problem)))
		       (general-bindings ;all-general-bindings
			                 (remove-if #'(lambda (gen-bin)
							(when (suni=trace) (format t "~%~A" gen-bin))
							(sgb~redundant-p ltop psctx gen-bin))
						    all-general-bindings))      
		       (new-uni-problems (mapcar #'(lambda (gen-binding)
						     (suni=gb-inst sup gen-binding :mode mode))
						 general-bindings)))
		  (when (suni=trace) (format t"~%~%There are ~D new unification-problems: " (length new-uni-problems)))
		  (mapc #'(lambda (new-uni-problem)
			    (cond (nil #+gklein(suni=alpha-cycle-check-p new-uni-problem)
				       (when (suni=trace) (progn (format t "~%CYCLE:~%") (suni=uprint new-uni-problem))))
				  (t
				   (when (suni=trace) (suni=uprint new-uni-problem))
				   (suni=problemlist-store nil
						  new-uni-problem))))
			new-uni-problems)))
	     ;;; flex-flex pair
	      (when (and (eq mode 'gen) (suni=flexflex-p pair (sctx~sov (suni~sctx sup))))
		(cond ((eq ltop rtop)
		       (let* ((list1 (data~appl-n-arguments lterm))   ;;; decomposition
			      (list2 (data~appl-n-arguments rterm))
			      (new-uni-problem (make-instance 'suni+sorted-up
							      :context (suni~sctx sup)
							      :theorems (suni~theorems sup)
							      :senv (suni=senv sup)
							      :solved-part (suni~solved-part sup)
							      :unsolved-part (append (mapcar #'(lambda (term1 term2)
												 (list term1 term2))
											     list1 list2)
										     (cdr unsolved-part))
							      :pre-unsolved-part (suni~pre-unsolved-part sup)
							      :prev-problem sup
							      :initial-problem (suni~initial-problem sup))))
			 (when (suni=trace) (progn (suni=uprint sup)
					      (format t "~%~%SUT-decomp leads to ")
					      (suni=uprint new-uni-problem)))
			 (suni=problemlist-store t
			  new-uni-problem)))
		      (t
		       (let ((weak-problem (suni=weak-problem ltop rtop sup)))
			 (when weak-problem
			   (mapc #'(lambda (wup)
				     (suni=problemlist-store nil
				     wup))
				 (suni=weak-elim weak-problem :position :right))))
		       (let ((weak-problem (suni=weak-problem rtop ltop sup)))
			 (when weak-problem
			   (mapc #'(lambda (wup)
				     (suni=problemlist-store nil
				     wup))
				 (suni=weak-elim weak-problem :position :left)))))))
	      	(when (suni=problemlist-problems)
		  (suni=next-unifier (suni=problemlist-next) :mode mode))
	      )
	  ;;; return the solved up
	  sup)))))
	  

;;;
;;; 
;;;

(defun suni=weak-elim (up &key (position :left))
  (declare (edited  "06-MAR-1996 15:05")
	   (authors GKLEIN)
	   (input   "A unification problem UP and optionally the position :left or :right.")
	   (effect  "None.")
	   (value   "A list of unification problems where the first unsolved weakenable pair of terms have been solved."))
  (let* ((pair (first (suni~unsolved-part up)))
	 (eterm (if (eq position :left)
		    (first pair)
		  (second pair)))
	 (oterm (if (eq position :left)
		    (second pair)
		  (first pair)))
	 (etop (data~top (data~n-range eterm)))
	 (otop (data~top (data~n-range oterm)))
	 (etop-sort (sctx~var-sctx-get-component etop (sctx~sov (suni~sctx up))))
	 (otop-sort (sctx~var-sctx-get-component otop (sctx~sov (suni~sctx up))))
	 (sort-B (data~n-range etop-sort))
	 (m (sort~n-range-rel sort-B otop-sort))
	 (gen-bins (if (sort~intersection-p otop-sort)
		       (list (sgb~imit-gen-bindings otop (first (sort~intersection-members otop-sort)) m etop-sort))
		     (list (sgb~imit-gen-bindings otop otop-sort m etop-sort)))))
    (when (suni=trace) (format t "~%~%w: There are ~A new uni-problems:" (length gen-bins)))
    (mapcar #'(lambda (gen-bin)
		(if (suni=trace) (format t "~%Imitation binding for ~A : ~A" etop gen-bin))
		(let* ((assoc-sctx (sgb~var-sctx gen-bin))
		       (binding-term (sgb~term gen-bin))
		       (new-solved-pair (when (suni=in-init-sctx-p etop up)
					  (list etop binding-term)))
		       (new-psctx (if new-solved-pair
				     (sctx~add assoc-sctx (sctx~sov (suni~sctx up)))
				   (sctx~add assoc-sctx (sctx~var-sctx-rem-pair etop (sctx~sov (suni~sctx up))))))
		       (new-solved-part (if new-solved-pair
					    (cons new-solved-pair (suni=var-replace-to-pairs binding-term etop
											     (suni~solved-part up)))
					  (suni=var-replace-to-pairs binding-term etop (suni~solved-part up))))
		       (new-sctx (sctx~create new-psctx  (sctx~loc (suni~sctx up))))
		       (new-up (make-instance 'suni+sorted-up
					      :context new-sctx
					      :theorems (append (list (cons oterm (cons otop-sort
									    (sgb~theorems gen-bin)))) 
								(suni~theorems up))
					      :senv (suni=senv up)
					      :solved-part new-solved-part
					      :unsolved-part (suni=var-replace-to-pairs binding-term etop
											(suni~unsolved-part up))
					      :pre-unsolved-part (suni~pre-unsolved-part up)
					      :prev-problem up
					      :initial-problem (suni~initial-problem up))))
		  (when (suni=trace) (format t "~%~%weak-elim leads to ~A"  new-up))
		  new-up))
	    gen-bins)))


;;;
;;;
;;;


(defun suni=gb-inst (up gen-bin &key (mode 'pre))
  (let* ((unsolved-part (suni~unsolved-part up))
	 (pair (first unsolved-part))
	 (lterm (first pair))
	 (rterm (second pair))
	 (sctx (suni~sctx up))
	 (psctx (sctx~sov sctx))
	 (ltop (data~top (data~n-range lterm)))  
	 (ltop-sort (sctx~var-sctx-get-component ltop (sctx~sov (suni~sctx up))))
	 (rtop (data~top (data~n-range rterm))))
    (let* ((bterms (when (listp gen-bin) (mapcar #'sgb~term gen-bin)))
	   (assoc-sctx (if bterms
			  (sctx~list-add (mapcar #'sgb~var-sctx gen-bin))
			(sgb~var-sctx gen-bin)))
	   (thms  (if bterms
			  (mapcan #'sgb~theorems gen-bin)
			(sgb~theorems gen-bin)))
	   (bterm (if bterms
		      (first bterms)
		    (sgb~term gen-bin)))
	   (pre-unsolved-part-tmp (when (eq mode 'pre)
				    (suni=var-replace-to-pairs bterm ltop (suni~pre-unsolved-part up))))
	   (new-pre-unsolved-part (when (eq mode 'pre)
				    (remove-if-not #'(lambda (pair)
						       (suni=presolved-p (first pair)
									(second pair)
									psctx))
						   pre-unsolved-part-tmp)))
	   (unsolved-part-tmp (if bterms
				  (append (suni=var-replace-to-pairs bterm ltop (suni~unsolved-part up))
					  (suni=pairs bterms))
				(suni=var-replace-to-pairs bterm ltop (suni~unsolved-part up))))
	   (new-unsolved-part (if (eq mode 'pre)
				  (append unsolved-part-tmp (remove-if #'(lambda (pair)
									   (suni=presolved-p (first pair)
											    (second pair)
											    psctx))
								       pre-unsolved-part-tmp))
				unsolved-part-tmp))
	   (new-solved-pair (when (suni=in-init-sctx-p ltop up)
			      (list ltop bterm)))
	   (new-psctx (if new-solved-pair
			 (sctx~add assoc-sctx (sctx~sov (suni~sctx up)))
		       (sctx~add assoc-sctx (sctx~var-sctx-rem-pair ltop (sctx~sov (suni~sctx up))))))
	   (new-sctx (sctx~create new-psctx (sctx~loc (suni~sctx up))))
	   (new-solved-part (if new-solved-pair
				(cons new-solved-pair (suni=var-replace-to-pairs bterm ltop (suni~solved-part up)))
			      (suni=var-replace-to-pairs bterm ltop (suni~solved-part up)))))
      (make-instance 'suni+sorted-up
		     :context new-sctx
		     :solved-part new-solved-part 
		     :unsolved-part new-unsolved-part
		     :pre-unsolved-part new-pre-unsolved-part
		     :prev-problem up
		     :theorems (append (list (cons rterm (cons ltop-sort thms))) (suni~theorems up))
		     :senv (suni=senv up)
		     :initial-problem (suni~initial-problem up)))))

;;;
;;;
;;;

(defun suni=weak-problem (wtop stop up)
  (declare (edited  "06-MAR-1996 14:31")
	   (authors GKLEIN)
	   (input   "A topsymbol WTOP whose sort has to been weaken and the other topsymbol"
		    "from the corresponding term of the regarded pair.")
	   (effect  "None.")
	   (value   "A weakened unification problem."))
  (let* ((wtop-sort (sctx~var-sctx-get-component wtop (sctx~sov (suni~sctx up))))
	 (stop-sort (sctx~var-sctx-get-component stop (sctx~sov (suni~sctx up))))
	 (sort-B (data~n-range stop-sort))
	 (m (sort~n-range-rel sort-B wtop-sort)))
    (when m
      (let ((sort-A (sort~j-c-range m wtop-sort)))
	  ;;;(format t "~%A: ~A, ~%B: ~A" sort-A sort-B)
	(cond ((data~equal sort-A sort-B)
	       up)
	      (t
	       (let* ((n-domain-sort-A (sort~np-domain wtop-sort m))
		      (new-range-sort (sort~intersection-create (list sort-A sort-B) (suni=senv up)))
		      (new-sort-r (if n-domain-sort-A
				      (data~abstr-create n-domain-sort-A new-range-sort)
				    new-range-sort))
		      (new-sort (if (sort~intersection-p new-sort-r)
				    (sort~intersection-create (cons wtop-sort (sort~intersection-members new-sort-r))
							      (suni=senv up))
				  (sort~intersection-create (list new-sort-r wtop-sort)
							    (suni=senv up))))
		      (new-var (term~variable-create (gentemp "H-")
						     (sort~type-of-sort new-sort)))
		      (new-solved-pair (if (suni=in-init-sctx-p wtop up)
					   (list wtop new-var)))
		      (new-pos-sctx-tmp (if new-solved-pair
					   (sctx~sov (suni~sctx up))
					 (sctx~var-sctx-rem-pair wtop (sctx~sov (suni~sctx up)))))
		      (new-pos-sctx (sctx~add-pair (list new-var new-sort) new-pos-sctx-tmp))
		      (new-solved-part (if new-solved-pair
					   (cons new-solved-pair (suni=var-replace-to-pairs new-var wtop
											  (suni~solved-part up)))
					 (suni=var-replace-to-pairs new-var wtop (suni~solved-part up))))
		      (new-sctx (sctx~create new-pos-sctx  (sctx~loc (suni~sctx up))))
		      (new-up (make-instance 'suni+sorted-up  
					     :context new-sctx
					     :theorems (suni~theorems up)
					     :senv (suni=senv up)
					     :solved-part new-solved-part
					     :unsolved-part (suni=var-replace-to-pairs new-var wtop
										     (suni~unsolved-part up))
					     :pre-unsolved-part (suni~pre-unsolved-part up)
					     :prev-problem up
					     :initial-problem (suni~initial-problem up))))
		 (when (suni=trace) (progn (suni=uprint up)
				      (format t "~%r2:There is the new weaker unification-problem: ")
				      (suni=uprint new-up)))
		 new-up)))))))

;;;
;;;
;;;

(defun suni=in-init-sctx-p (var up)
  (declare (edited  "06-MAR-1996 17:55")
	   (authors GKLEIN)
	   (input   "A variable VAR and an unification problem UP.")
	   (effect  "None.")
	   (value   "T iff the initial unification problem related to UP contains the variable VAR"
		    "in its positive variable context."))
  (if (sctx~var-sctx-get-component var (sctx~sov (suni~sctx (suni~initial-problem up))))
      t))

;;;
;;;
;;;



(defvar suni*trace nil)

(defun suni=trace ()
  suni*trace)

(defun suni=uprint (up)
  (format t "~%~A" up))

(defgeneric suni=var-replace (term var interm)
  (:method ((term term+term) var interm)
	   (beta~normalize (subst~apply (subst~create (list var) (list term)) interm)))
  (:method ((termlist list) varlist interm)
	   (if termlist
	       (suni=var-replace (car termlist) (car varlist) (suni=var-replace (cdr termlist) (cdr varlist) interm))
	     interm)))


(defun suni=var-replace-to-pairs (term var pairs)
  (mapcar #'(lambda (pair)
	      (list (suni=var-replace term var (first pair)) (suni=var-replace term var (second pair))))
	  pairs))

(defun suni=pairs-domain (pairs)
  (let ((pair (car pairs)))
    (when pair
      (cons (car pair) (suni=pairs-domain (cdr pairs))))))
	
(defun suni=pairs-codomain (pairs)
  (let ((pair (car pairs)))
    (when pair
      (cons (cadr pair) (suni=pairs-codomain (cdr pairs))))))


(defun suni=pairs (list)
  (cond ((= (length list) 2) (list list))
	((> (length list) 2) (cons (list (first list) (second list)) (suni=pairs (cdr list))))
	(t (error "wrong list length of ~S." list))))



(defun suni=flexflex-p (pair context)
  (let* ((lterm (first pair))
	 (rterm (second pair))
	 (ltop (data~top (data~n-range lterm)))
	 (rtop (data~top (data~n-range rterm))))
    (if (and (member ltop (sctx~domain context))
	     (member rtop (sctx~domain context)))
	t
      nil)))


;;;
;;;
;;;



(let ((problems))

  (defun suni=problemlist-start (prob)
    (setf problems
	  (if (listp prob) prob (list prob))))

  (defun suni=problemlist-next ()     ;breathfirstsearch
    (let ((probs problems))
      (setf problems (butlast probs))
    (car (last probs))))

; (defun suni=problemlist-next ()      ;depthfirstsearch
;   (let ((probs problems))
;     (setf problems (rest probs))
;     (car  probs)))

  (defun suni=problemlist-store (param prob)
    (setf problems (cons prob problems)))

 (defun suni=problemlist-problems ()
   problems)
 )
  

#{
\subsection{Sorted substitutions}
Sorted substitutions are substitutions as in the {\tt subst} module with the
additional property of having a context in which the variables occurring in the
substitution are assigned with sort information and variable conditions.
#}


(eval-when (load compile eval)
(defclass suni+sosub (subst+substitution)
  ((context :initform nil :initarg :pos-sctx :accessor suni=sosub-sctx))
  (:documentation "The class of sorted substitutions.")))


(defun suni=sosub-substitution-create (domain codomain context)
  (declare (edited  "16-NOV-1995 22:36")
	   (authors GKLEIN)
	   (input   "A domain DOMAIN, a codomain CODOMAIN and a context CONTEXT.")
	   (effect  "None.")
	   (value   "A substitution with a sorted context."))
  (let* ((new-subst (change-class (subst~create domain codomain) 'suni+sosub)))
    (setf (suni=sosub-sctx new-subst) context)
    new-subst))




(defgeneric suni=sosub-free-codomain-variables (subst vars)
  (declare (edited  "27-NOV-1995 20:38")
	   (authors GKLEIN)
	   (input   "A substitution SUBST and a list of variables VARS.")
	   (effect  "None.")
	   (value   "The free variables in SUBST(VARS)."))
  (:method ((subst subst+substitution) vars)
	   (let ((termlist (remove-if #'null (mapcar #'(lambda (var) (subst~get-component var subst)) vars))))
	     (mapcan #'data~free-variables termlist))))



(defmethod print-object :before ((subst suni+sosub) stream)
  (declare (edited  "16-NOV-1995 23:17")
	   (authors GKLEIN)
	   (input   )
	   (effect  )
	   (value   ))
  (format stream "context: ~A~%binding-pairs: " (suni=sosub-sctx subst)))



#{
\subsection{Sorted presubstitutions}
Sorted presubstitutions are sorted substitutions as in the {\tt sosub} module with the
additional property of having pairs of terms in a presolved form in respect to the sorted
context of the sorted substitution.
#}


(eval-when (load compile eval)
(defclass suni+prsub (suni+sosub)
  ((presolved-pairs :initform nil :initarg :presolved-pairs :accessor suni=prsub-presolved-pairs))
  (:documentation "The class of pre sorted substitutions.")))
  

(defun suni=prsub-substitution-create (domain codomain context pairs)
  (declare (edited  "16-NOV-1995 22:36")
	   (authors GKLEIN)
	   (input   "A domain DOMAIN, a codomain CODOMAIN, a context CONTEXT"
		    "and a list of presolved pairs PAIRS.")
	   (effect  "None.")
	   (value   "A pre substitution with a sorted context."))
  (let* ((new-subst (change-class (suni=sosub-substitution-create domain codomain context) 'suni+prsub)))
    (setf (suni=prsub-presolved-pairs new-subst) pairs)
    new-subst))



(defmethod print-object :after ((subst suni+prsub) stream)
  (declare (edited  "16-NOV-1995 23:17")
	   (authors GKLEIN)
	   (input   )
	   (effect  )
	   (value   ))
  (format stream "~%presoved pairs: ~A" (suni=prsub-presolved-pairs subst)))




;;;
;;; print-object
;;;


(defmethod print-object ((up suni+sorted-up) stream)
  (format stream "~A" (suni~sctx up))
  (format stream "~%Thms: ~A" (suni~theorems up))
  (format stream "~%Env: ~A" (suni=senv up))
  (format stream "~%solved: ~A"
	 ;(mapcar #'(lambda (pair)(concatenate 'string (format nil "~A" (car pair)) " == " (format nil "~A" (cadr pair))))
	  (suni~solved-part up))
					;)
  (format stream "~%unsolved: ~A"
	  ;(mapcar #'(lambda (pair)(concatenate 'string (format nil "~A" (car pair)) " == " (format nil "~A" (cadr pair))))
		  (suni~unsolved-part up))
					;)
  (format stream "~%presolved: ~A"
	 ;(mapcar #'(lambda (pair)(concatenate 'string (format nil "~A" (car pair)) " == " (format nil "~A" (cadr pair))))
(suni~pre-unsolved-part up)))
					;)



