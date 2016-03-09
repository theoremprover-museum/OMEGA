;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(mod~defmod crh :uses (term type data env keim mod post)
	    :documentation "Functions to be used for evaluating crules"
	    :exports (crh~leq-term-p
		      crh~term-less-or-eq
		      crh~sterm-leq
		      crh~sterm-leq2
		      )
	    )

(defun crh~term-less-or-eq (term1 term2)
  (declare (edited  "18-FEB-1999")
	   (authors Lassaad)
	   (input   "Two terms or two term lists.")
	   (effect  "None.")
	   (value   "Pair: - T, iff TERM1 is less than TERM2"
		    "      - T, iff TERM1 is equal to TERM2."))
  (crh=less-or-eq term1 term2))

(defgeneric crh=less-or-eq (term1 term2)
  (:method ((const1 term+constant) (const2 term+constant))
	   (values NIL (crh=data-equal const1 const2)))
  (:method ((const1 term+constant) (appl2 data+appl))
	   (values (some #'(lambda (arg)
			     (multiple-value-bind (less-arg eq-arg)
				 (crh=less-or-eq const1 arg)
			       (or less-arg eq-arg)))
			 (data~appl-arguments appl2))))
  (:method ((var1 term+variable) (var2 term+variable))
	   ;;; Comparing variables occurring in the scope of a lambda-expression
	   (values NIL (data~equal var1 var2)))
  (:method ((var1 term+variable) (appl2 data+appl))
	   (values (some #'(lambda (arg)
			     (multiple-value-bind (less-arg eq-arg)
				 (crh=less-or-eq var1 arg)
			       (or less-arg eq-arg)))
			 (data~appl-arguments appl2))))
  (:method ((appl1 term+appl) (appl2 term+appl))
	   (cond ((crh=data-equal (data~appl-function appl1) (data~appl-function appl2))
		  (crh=less-or-eq (data~appl-arguments appl1) (data~appl-arguments appl2)))
		 (T
		  (values (some #'(lambda (arg)
				    (multiple-value-bind (less-arg eq-arg)
					(crh=less-or-eq appl1 arg)
				      (or less-arg eq-arg)))
				(data~appl-arguments appl2))))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr))
	   (crh=less-or-eq (data~abstr-range abstr1)
			   (data~replace-free-variables (data~abstr-range abstr2)
							(data~abstr-domain abstr2)
							(data~abstr-domain abstr1))))
  (:method ((nix1 null) (nix2 null))
	   (values NIL T))
  (:method ((terms1 cons) (terms2 cons))
	   (multiple-value-bind (less1 eq1)
	       (crh=less-or-eq (first terms1) (first terms2))
	     (when (or less1 eq1)
	       (multiple-value-bind (less2 eq2)
		   (crh=less-or-eq (rest terms1) (rest terms2))
		 (if (and eq1 eq2)
		     (values NIL T)
		   (values (or less2 eq2)))))))
  (:method (object1 object2)
	   (declare (ignore object1 object2))
	   NIL)
  )


(defun crh~term-leq (term1 term2)
  (declare (edited  "18-FEB-1999")
	   (authors Lassaad)
	   (input   "Two terms or two term lists which may contain meta-variables.")
	   (effect  "None.")
	   (value   "T, iff TERM1 is less than or equal to TERM2."))
  (crh=term-leq term1 term2))

;;; Two variations of implementing CRH=TERM-LEQ
;; Principle-1: Judjing that two terms with meta-variables are leq by assuming that
;; functional variables are bound only by projection:
;;  X <= X 
;;  X <= f(t1 .. tn), iff X <= ti for some i
;;  a <= X(t1 .. tn), iff a <= ti for each i
;;  Y <= X(t1 .. tn), iff Y <= ti for each i
;;  X(t1 .. tn) <= f(s1 .. sm), iff X(t1 .. tn) <= si for some i
;;  X(t1 .. tn) <= X(s1 .. sn), iff ti <= si for each i
;;  f(t1 .. tn) <= X(s1 .. sm), iff f(t1 .. tn) <= si for each i
;;  X(t1 .. tn) <= Y(s1 .. sm), iff X(t1 .. tn) <= si for each i
;;
;; Principle-2: Judging that two terms t1 and t2 with meta-variables are leq, when  
;; t1 can be unified to a subterm of t2. Backtracking wrt. t2 would not affect the
;; completness. Here we have to reject only the methods which implement assertion
;; application. The Bind critic should be appplied, if we considered different
;; meta-variables with similar contexts as leq. In case meta-variables are eq
;; we reject the critic Bind too:
;; B-1: X = X
;; B-2: X < Y
;; B-3: X = f[X]
;; B-4: X < f[Y]
;; B-5: Y < X(t1 .. tn)
;; B-6: X(t1 .. tn) = f[X(t1 .. tn)]
;; B-7: X(t1 .. tn) < f[Y(s1 .. sm)]
;; B-8: X(t1 .. tn) = X(s1 .. sn), iff ti = si or ti leq si 
;; B-9: X(t1 .. tn) < Y(s1 .. sm)
;; B-10: Incomparable: <a, X[..]> and <f(t1 .. tn), X[..]>
;;; Implementation of a function which returns <eq less leq>
;; where eq (same meta-variables) and less (different meta-variables) for terms
;; with meta-variables; leq for terms without meta-variables

(defgeneric crh=term-leq (term1 term2)
  (:method ((mvar1 meta+variable) (mvar2 meta+variable))
	   (if (data~equal mvar1 mvar2)
	       ;; B-1
	       (values T)
	     ;; B-2
	     (values NIL T)))
  (:method ((prim1 term+primitive) (prim2 term+primitive))
	   ;; B-10 <a, X> and <X, a> are excluded
	   (values NIL NIL (crh=data-equal prim1 prim2)))
  (:method ((mvar1 meta+variable) (appl2 data+appl))
	   (cond ((meta~p (data~appl-function appl2))
		  ;; B-5
		  (values NIL T))
		 (T
		  (let ((free-vars (data~free-variables appl2)))
		    (if (find mvar1 free-vars :test #'data~equal)
			;; B-3
			(values T)
		      (when (find-if #'meta~p free-vars)
			;; B-4
			(values NIL T)))))))
  (:method ((prim1 term+primitive) (appl2 data+appl))
	   (unless (meta~p (data~appl-function appl2))
	     ;; B-10 <a, X(t1 .. tn)> is excluded
	     (values NIL NIL
		     (some #'(lambda (arg) (multiple-value-bind (eq less leq)
					       (crh=term-leq prim1 arg)
					     (declare (ignore eq less))
					     leq))
			   (data~appl-arguments appl2)))))
  (:method ((prim1 term+primitive) (abstr2 data+abstr))
	   (crh=term-leq prim1 (data~abstr-range abstr2)))
  (:method ((appl1 term+appl) (appl2 term+appl))
	   (cond ((and (meta~p (data~appl-function appl1)) (meta~p (data~appl-function appl2)))
		  (if (data~equal (data~appl-function appl1) (data~appl-function appl2))
		      ;; B-8
		      (multiple-value-bind (eq less leq)
			  (crh=term-leq (data~appl-arguments appl1) (data~appl-arguments appl2))
			(if (or eq leq)
			    (values T)
			  (when less
			    (values NIL T))))
		    ;; B-9
		    (values NIL T)))
		 ((meta~p (data~appl-function appl1))
		  ;; B-6 and B-7
		  (let ((flex-apps (crh=help-subterms (data~appl-arguments appl2)
						      #'(lambda (st) (and (data~appl-p st)
									  (meta~p (data~appl-function st)))))))
		    (when flex-apps
		      (let (res-eq)
			(dolist (flex-app flex-apps)
			  (when (data~equal (data~appl-function appl1) (data~appl-function flex-app))
			    (multiple-value-bind (eq less leq)
				(crh=term-leq (data~appl-arguments appl1) (data~appl-arguments flex-app))
			      (declare (ignore less))
			      (when (or eq leq)
				(setq res-eq T) (return)))))
			(values res-eq (not res-eq))))))
		 (T
		  (unless (meta~p (data~appl-function appl2))
		    ;; B-10 <f(t1 .. tn), X(s1 .. sn)> is excluded
		    (cond ((crh=data-equal (data~appl-function appl1) (data~appl-function appl2))
			   (multiple-value-bind (eq less eql)
			       (crh=term-leq (data~appl-arguments appl1) (data~appl-arguments appl2))
			     (cond ((or eq less eql) (values eq less eql))
				   (T
				    ;;; for instance: g1(idx1,idx1) and g1((g1 idx1 idx1),witness33)
				    ;; would not be captured by the first test:
				    (let ((same-fn-apps (crh=help-subterms
							 (data~appl-arguments appl2)
							 #'(lambda (st) (and (data~appl-p st)
									     (crh=data-equal (data~appl-function appl1)
											     (data~appl-function st)))))))
				      (let (res-eq res-less res-leq)
					(dolist (fn-app same-fn-apps)
					  (multiple-value-bind (eq less leq)
					      (crh=term-leq (data~appl-arguments appl1) (data~appl-arguments fn-app))
					    (if leq
						(progn (setq res-eq NIL res-less NIL res-leq T) (return))
					      (if eq (setq res-eq T)
						(setq res-less less)))))
					(if res-eq
					    (values T)
					  (values NIL res-less res-leq))))))))
			  (T
			   (let ((same-fn-apps (crh=help-subterms (data~appl-arguments appl2)
								  #'(lambda (st)
								      (and (data~appl-p st)
									   (crh=data-equal (data~appl-function appl1)
											   (data~appl-function st)))))))
			     (let (res-eq res-less res-leq)
			       (dolist (fn-app same-fn-apps)
				 (multiple-value-bind (eq less leq)
				     (crh=term-leq (data~appl-arguments appl1) (data~appl-arguments fn-app))
				   (if leq
				       (progn (setq res-eq NIL res-less NIL res-leq T) (return))
				     (if eq (setq res-eq T)
				       (setq res-less less)))))
			       (if res-eq
				   (values T)
				 (values NIL res-less res-leq))))))))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr))
	   (crh=term-leq (data~abstr-range abstr1)
			  (data~replace-free-variables (data~abstr-range abstr2)
						       (data~abstr-domain abstr2)
						       (data~abstr-domain abstr1))))
  (:method ((nix1 null) (nix2 null))
	   (values T))
  (:method ((terms1 cons) (terms2 cons))
	   (multiple-value-bind (eq1 less1 leq1)
	       (crh=term-leq (first terms1) (first terms2))
	     (when (or eq1 less1 leq1)
	       (multiple-value-bind (eq less leq)
		   (crh=term-leq (rest terms1) (rest terms2))
		 (cond ((or (and eq1 eq) (and eq1 leq) (and leq1 eq))
			(values T))
		       ((and leq1 leq)
			(values NIL NIL T))
		       ((or eq less leq)
			(values NIL T)))))))
  (:method (object1 object2)
	   (declare (ignore object1 object2))
	   NIL)
  )

(defgeneric crh=data-equal (obj1 obj2 &optional prob-env)
  (:method ((const1 term+constant) (const2 term+constant)
	    &optional (prob-env (prob~environment (prob~proof-problem omega*current-proof-plan))))
	   (or (data~equal const1 const2)
	       ;; Either CONST1 and CONST2 are the same, or both have the same type and are newly
	       ;; introduced as new constants in a ForallI step or ExistsE step.
	       (and (data~equal (term~type const1) (term~type const2))
		    (null (env~lookup-object (keim~name const1) prob-env))
		    (null (env~lookup-object (keim~name const2) prob-env)))))
  (:method ((nix1 null) (nix2 null) &optional prob-env)
	   (declare (ignore prob-env))
	   T)
  (:method ((list1 cons) (list2 cons) 
	    &optional (prob-env (prob~environment (prob~proof-problem omega*current-proof-plan))))
	   (and (crh=data-equal (first list1) (first list2) prob-env)
		(crh=data-equal (rest list1) (rest list2) prob-env)))
  (:method ((appl1 term+appl) (appl2 term+appl)
	    &optional (prob-env (prob~environment (prob~proof-problem omega*current-proof-plan))))
	   (and (crh=data-equal (data~appl-function appl1) (data~appl-function appl2) prob-env)
		(crh=data-equal (data~appl-arguments appl1) (data~appl-arguments appl2) prob-env)))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr)
	    &optional (prob-env (prob~environment (prob~proof-problem omega*current-proof-plan))))
	   (cond ((= (length (data~abstr-domain abstr1)) (length (data~abstr-domain abstr2)))
		  (and (crh=help-same-type (data~abstr-domain abstr1) (data~abstr-domain abstr2))
		       (crh=data-equal (data~abstr-range abstr1)
				       (data~replace-free-variables (data~abstr-range abstr2)
								    (data~abstr-domain abstr2)
								    (data~abstr-domain abstr1)))))
		 ((< (length (data~abstr-domain abstr1)) (length (data~abstr-domain abstr2)))
		  (let ((newran2 (term~abstr-create (last (data~abstr-domain abstr2)
							  (- (length (data~abstr-domain abstr2))
							     (length (data~abstr-domain abstr1)))) 
						    (data~abstr-range abstr2)))
			(newdom2 (subseq (data~abstr-domain abstr2) 0 (length (data~abstr-domain abstr1)))))
		    (and (crh=help-same-type (data~abstr-domain abstr1) newdom2)
			 (crh=data-equal (data~abstr-range abstr1)
					 (data~replace-free-variables newran2
								      newdom2
								      (data~abstr-domain abstr1))))))
		 ((< (length (data~abstr-domain abstr2)) (length (data~abstr-domain abstr1)))
		  (let ((newran1 (term~abstr-create (last (data~abstr-domain abstr1)
							  (- (length (data~abstr-domain abstr1))
							     (length (data~abstr-domain abstr2)))) 
						    (data~abstr-range abstr1)))
			(newdom1 (subseq (data~abstr-domain abstr1) 0 (length (data~abstr-domain abstr2)))))
		    (and (crh=help-same-type newdom1 (data~abstr-domain abstr2))
			 (crh=data-equal newran1
					 (data~replace-free-variables (data~abstr-range abstr2)
								      (data~abstr-domain abstr2)
								      newdom1)))))
		 (t nil)))
  (:method ((obj1 T) (obj2 T) &optional prob-env)
	   (declare (ignore prob-env))
	   (data~equal obj1 obj2))
  )

(defgeneric crh=help-same-type (obj1 obj2)
  (:method ((term1 term+term) (term2 term+term))
	   (data~equal (term~type term1) (term~type term2)))
  (:method ((list1 cons) (list2 cons))
	   (and (crh=help-same-type (first list1) (first list2))
		(crh=help-same-type (rest list1) (rest list2))))
  (:method ((nix1 null) (nix2 null))
	   T)
  (:method (obj1 obj2)
	   (declare (ignore obj1 obj2))
	   NIL)
  )

(defgeneric crh=help-subterms (term test-fn)
  ;;; Returns the least nested subterms of TERM which satisfy TEST-FN
  (:method ((nix1 null) test-fn)
	   (declare (ignore test-fn))
	   NIL)
  (:method ((terms cons) test-fn)
	   (append (crh=help-subterms (first terms) test-fn)
		   (crh=help-subterms (rest terms) test-fn)))
  (:method ((prim term+primitive) test-fn)
	   (when (funcall test-fn prim) (list prim)))
  (:method ((appl data+appl) test-fn)
	   (if (funcall test-fn appl) (list appl)
	     (crh=help-subterms (data~appl-arguments appl) test-fn)))
  (:method ((abstr data+abstr) test-fn)
	   (crh=help-subterms (data~abstr-range abstr) test-fn))
  )
	   


(defun crh~sterm-leq (term1 term2)
  (declare (edited  "18-FEB-1999")
	   (authors Lassaad)
	   (input   "Two terms or two term lists which may contain meta-variables.")
	   (effect  "None.")
	   (value   "T, iff TERM1 is less than or equal toTERM2."))
  (crh=sterm-leq term1 term2))

(defgeneric crh=sterm-leq (term1 term2)
  (:method ((prim1 term+primitive) (prim2 term+primitive))
	   (data~equal prim1 prim2))
  (:method ((prim1 term+primitive) (appl2 data+appl))
	   (some #'(lambda (arg) (crh=sterm-leq prim1 arg))
		 (data~appl-arguments appl2)))
  (:method ((prim1 term+primitive) (abstr2 data+abstr))
	   (crh=sterm-leq prim1 (data~abstr-range abstr2)))
  (:method ((appl1 term+appl) (appl2 term+appl))
	   (cond ((data~equal (data~appl-function appl1) (data~appl-function appl2))
		  (crh=sterm-leq (data~appl-arguments appl1) (data~appl-arguments appl2)))
		 (T
		  (some #'(lambda (arg) (crh=sterm-leq appl1 arg))
			(data~appl-arguments appl2)))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr))
	   (crh=sterm-leq (data~abstr-range abstr1)
			  (data~replace-free-variables (data~abstr-range abstr2)
						       (data~abstr-domain abstr2)
						       (data~abstr-domain abstr1))))
  (:method ((nix1 null) (nix2 null))
	   (values NIL T))
  (:method ((terms1 cons) (terms2 cons))
	   (and (crh=sterm-leq (first terms1) (first terms2))
		(crh=sterm-leq (rest terms1) (rest terms2))))
  (:method (object1 object2)
	   (declare (ignore object1 object2))
	   NIL)
  )


;;; A generalisation of crh~sterm-leq by interpreting meta-variables identical when
;; they have the same context, e.g., X[] = Y[] and X[t1 .. tn] <= Y[s1 .. sn] iff
;; (t1 .. tn) <= (s1 .. sn) in the sense of crh=sterm-leq:
(defun crh~sterm-leq2 (term1 term2)
  (declare (edited  "18-FEB-1999")
	   (authors Lassaad)
	   (input   "Two terms or two term lists which may contain meta-variables.")
	   (effect  "None.")
	   (value   "T, iff TERM1 is less than or equal toTERM2."))
  ;(crh=sterm-leq2 term1 term2 1))
  (crh=sterm-leq2 term1 term2 0))



(defgeneric crh=sterm-leq2 (term1 term2 &optional nest-factor)
  ;;; When nest-factor > 0, then X[..] = Y[..]
  (:method ((mvar1 meta+variable) (mvar2 meta+variable) &optional (nest-factor 0))
	   (cond ((zerop nest-factor) (data~equal mvar1 mvar2))
		 (T T)))
  (:method ((prim1 term+primitive) (prim2 term+primitive) &optional (nest-factor 0))
	   (declare (ignore nest-factor))
	   (data~equal prim1 prim2))
  (:method ((prim1 term+primitive) (appl2 data+appl) &optional (nest-factor 0))
	   (some #'(lambda (arg) (crh=sterm-leq2 prim1 arg nest-factor))
		 (data~appl-arguments appl2)))
  (:method ((prim1 term+primitive) (abstr2 data+abstr) &optional (nest-factor 0))
	   (crh=sterm-leq2 prim1 (data~abstr-range abstr2) nest-factor))
  (:method ((appl1 term+appl) (appl2 term+appl) &optional (nest-factor 0))
	   (cond ((data~equal (data~appl-function appl1) (data~appl-function appl2))
		  (crh=sterm-leq2 (data~appl-arguments appl1) (data~appl-arguments appl2) nest-factor))
		 ((and (meta~p (data~appl-function appl1)) (meta~p (data~appl-function appl2)))
		  (crh=sterm-leq2 (data~appl-arguments appl1) (data~appl-arguments appl2) (- nest-factor 1)))
		 (T
		  (some #'(lambda (arg) (crh=sterm-leq2 appl1 arg nest-factor))
			(data~appl-arguments appl2)))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr) &optional (nest-factor 0))
	   (crh=sterm-leq2 (data~abstr-range abstr1)
			   (data~replace-free-variables (data~abstr-range abstr2)
							(data~abstr-domain abstr2)
							(data~abstr-domain abstr1))
			   nest-factor))
  (:method ((nix1 null) (nix2 null) &optional (nest-factor 0))
	   (declare (ignore nest-factor))
	   T)
  (:method ((terms1 cons) (terms2 cons) &optional (nest-factor 0))
	   (and (crh=sterm-leq2 (first terms1) (first terms2) nest-factor)
		(crh=sterm-leq2 (rest terms1) (rest terms2) nest-factor)))
  (:method (object1 object2 &optional (nest-factor 0))
	   (declare (ignore object1 object2 nest-factor))
	   NIL)
  )

(defun crh~leq-term-p (term1 term2)
  (crh=leq-term-p2 term1 term2))
  ;(crh=leq-term-p term1 term2))

(defgeneric crh=leq-term-p2 (term1 term2)
  (:method ((prim1 term+primitive) (term2 term+term))
	   T)
  (:method ((appl1 term+appl) (appl2 term+appl))
	   (or (crh=leq-term-p2 (data~appl-arguments appl1) (data~appl-arguments appl2))
	       (some #'(lambda (x) (crh=leq-term-p2 appl1 x)) (data~appl-arguments appl2))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr))
	   (crh=leq-term-p2 (data~abstr-range abstr1) (data~abstr-range abstr2)))
  (:method ((nix1 null) (nix2 null))
	   T)
  (:method ((terms1 cons) (terms2 cons))
	   (and (crh=leq-term-p2 (first terms1) (first terms2))
		(crh=leq-term-p2 (rest terms1) (rest terms2))))
  (:method (object1 object2)
	   (declare (ignore object1 object2))
	   NIL)
  )

(defgeneric crh=leq-term-p (term1 term2)
  (declare (edited  "10-FEB-1999")
	   (authors Lassaad)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "T, when the term structure of TERM2 is greater than or equal to"
		    "the term structure of TERM1. TERM2 is greater than TERM1, when"
		    "TERM2 is built from TERM1 using additional functions (constructors)."
		    "Examples: f(i,i), f(g(i),i) -> T"
		    "          f(i,i), g(f(i,i),i) -> T"
		    "          f(MV) , MV' -> NIL"
		    "          f(i,i), MV -> NIL"
		    "          MV , f(MV') -> T"
		    "          MV, MV' -> T"
		    "          f(i,i), g(i) -> NIL"))
  (:method ((mvar1 meta+variable) (mvar2 meta+variable))
	   ;;; (MV <= MV')
	   T)
  (:method ((mvar1 meta+variable) (term2 term+term))
	   ;;; (MV1 <= f[MV2]) 
	   (find-if #'meta~p (data~free-variables term2)))
;  (:method ((prim1 term+primitive) (mvar2 meta+variable))
;           ;;; (cv <= MV) 
;           T) 
;           NOT ALLOWED
  (:method ((prim1 term+primitive) (prim2 term+primitive))
	   ;;; (cv <= cv) and (cv > cv')
	   (data~equal prim1 prim2))
  (:method ((prim1 term+primitive) (appl2 term+appl))
	   ;;; (cv <= f[cv]), where cv <= cv
	   (find-if #'(lambda (st) (crh=leq-term-p prim1 st))
		    (data~appl-arguments appl2)))
  (:method ((appl1 term+appl) (appl2 term+appl))
	   (cond ((or (meta~p (data~appl-function appl1)) (meta~p (data~appl-function appl2)))
		  ;;; F1(t1 .. tn) <= F2(s1 .. sn) ,iff F1 or F2 are meta-variables and ti <= si for each i
		  ;;; F1(t1 .. tn) <= F2(s1 .. sm) ,iff F1 or F2 are meta-variables and F1(t1 .. tn) <= sj for some j
		  (if (= (length (data~appl-arguments appl1)) (length (data~appl-arguments appl2)))
		      (crh=leq-term-p (data~appl-arguments appl1) (data~appl-arguments appl2))
		    (some #'(lambda (x) (crh=leq-term-p appl1 x))
			  (data~appl-arguments appl2))))
		 ((data~equal (data~appl-function appl1) (data~appl-function appl2))
		  ;;; f(t1 .. tn) <= f(s1 .. sn), iff each ti <= si
		  (crh=leq-term-p (data~appl-arguments appl1) (data~appl-arguments appl2)))
		 (T
		  ;;; f(t1 .. tn) <= g(s1 .. sn), when there is a subterm in (s1 .. sn) which 
		  ;; is greater or equal to f(t1 .. tn) 
		  (some #'(lambda (x) (crh=leq-term-p appl1 x))
			(data~appl-arguments appl2)))))
  	  
;                  (let ((appl1-prims (remove-if-not #'term~primitive-p (data~all-substructs appl1)))
;                        (appl2-args-prims (apply #'append
;                                                 (mapcar #'(lambda (arg)
;                                                             (remove-if-not #'term~primitive-p (data~all-substructs arg)))
;                                                         (data~appl-arguments appl2)))))
;                    (every #'(lambda (x) (find x appl2-args-prims)) appl1-prims)))))
  (:method ((abstr1 term+abstr) (abstr2 term+abstr))
	   (crh=leq-term-p (data~abstr-range abstr1)
			   (data~replace-free-variables (data~abstr-range abstr2)
							(data~abstr-domain abstr2)
							(data~abstr-domain abstr1))))
  (:method ((nix1 null) (nix2 null))
	   T)
  (:method ((terms1 cons) (terms2 cons))
	   (and (crh=leq-term-p (first terms1) (first terms2))
		(crh=leq-term-p (rest terms1) (rest terms2))))
  (:method (object1 object2)
	   (declare (ignore object1 object2))
	   NIL)
  )











