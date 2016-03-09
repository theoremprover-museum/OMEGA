;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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


(defun compute-instantiation-with-cas (meta-var)
  (let ((inst-task (roc~start-task instmeta*roc-state-description)))
    (crihelp=param-to-inst-task inst-task)))

(strat~define-strategy-ks
 (name InstFromCAS)
 (refinement-algorithm InstMeta)
 (condition instantiation-task-p)
 (compute-instantiation-function compute-instantiation-with-cas)
 (parameter-types )
 (print "Strategy-KS InstFromCAS: Offer to instantiate meta-variable ~A with a computer algebra system"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        InstIsoByPolyFromCAS                                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(strat~define-strategy-ks
 (name InstIsoByPolyFromCAS)
 (refinement-algorithm InstMeta)
 (condition iso-instantiation-task-p)
 (compute-instantiation-function compute-polynom-with-cas)
 (parameter-types )
 (print "Strategy-KS InstIsoByPolyFromCAS: Offer to instantiate meta-variable ~A with a computer algebra system"))


(defun iso-instantiation-task-p (task)
  (if (agenda~inst-task-p task)
      (let* ((plan-step (agenda~inst-task-plan-step task))
	     (just (pdsc~an-just plan-step))
	     (method (just~method just)))
	(if (or (string-equal (keim~name method) 'IsomorphicI-Function-M-b)
		(string-equal (keim~name method) 'IsomorphicI-Function-M)
		(string-equal (keim~name method) 'ModuleIsomorphicI-Function-M-b)
		(string-equal (keim~name method) 'ModuleIsomorphicI-Function-M))
	    't
	  nil))
    nil))

(defun compute-polynom-with-cas (meta-var)
  (let* ((inst-task (roc~start-task instmeta*roc-state-description))
	 (node (pdsc~an-node (agenda~inst-task-plan-step inst-task)))
	 (polynom (strathelp=find-polynom-function-from-node node)))
    polynom))

(defun strathelp=find-polynom-function-from-node (node)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node, which is open and has the formula (isomorphic set1 op1 set2 op2) or"
		    "(module-isomorphic set1 op1 set2 op2 ring scop1 scop2).")
	   (effect  "None.")
	   (value   "The polynom function, which is an isomorphismbetween set1, op1 and set2, op2)."))
  (let* ((formula (node~formula node)))
    (cond ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'isomorphic (pds~environment omega*current-proof-plan)))))
	   (let* ((tables (crihelp=compute-isomorphism-multiplication-table! node))
		  (table1 (first tables))
		  (table2 (second tables)))
	     (multiple-value-bind
		 (success function-pairs)
		 (crihelp=check-isomorphism table1 table2)
	       (if (null success)
		   nil
		 (let* ((functions (crihelp=remove-power-functions
				    (rcl~check-closed-homomorphism table1 table2 function-pairs))))
		   (if functions
		       (strathelp=parse-polynom-functions-from-node functions node)
		     nil))))))
	  ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'module-isomorphic (pds~environment omega*current-proof-plan)))))
	   (let* ((tables (crihelp=compute-module-isomorphism-multiplication-table! node))
		  (module1-table (first tables))
		  (module2-table (second tables))
		  (ring-module1-table (third tables))
		  (ring-module2-table (fourth tables)))
	     (multiple-value-bind
		 (success function-pairs)
		 (crihelp=check-module-isomorphism module1-table module2-table ring-module1-table ring-module2-table)
	       (if (null success)
		   nil
		 (let* ((functions (crihelp=remove-power-functions
				    (rcl~check-closed-homomorphism module1-table module2-table function-pairs))))
		   (if functions
		       (strathelp=parse-polynom-functions-from-node functions node)
		     nil))))))
	  (t
	   nil))))

(defun strathelp=parse-polynom-functions-from-node (functions node)
  (let* ((formula (node~formula node))
	 (set1 (first (data~appl-arguments formula)))
	 (set2 (third (data~appl-arguments formula)))
	 (class-factors (crihelp=pairs-facs set2))
	 (new-var (term~variable-create 'x (first (data~abstr-domain (term~type set1)))))
	 (pair-vars (crihelp=pairs-var set1 new-var))
	 (function-parts (mapcar #'(lambda (function class-factor)
				     (crihelp=parse-polynom-function function class-factor pair-vars))
				 functions class-factors))
	 (polynom (crihelp=combine-functions set2 function-parts)))
    (term~abstr-create (list new-var)
		       polynom)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InstDiscriminantWithHR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(strat~define-strategy-ks
 (name InstDiscriminantWithHR)
 (refinement-algorithm InstMeta)
 (condition Dist-Prop-Inst-Task-p)
 (compute-instantiation-function compute-discriminant-property)
 (parameter-types )
 (print "Strategy-KS InstWithDistinguishingProperty: Offer to instantiate meta-variable ~A as distinguishing property of two structures"))

(defun Dist-Prop-Inst-Task-p (inst-task)
  (if (agenda~inst-task-p inst-task)
      (let* ((plan-step (agenda~inst-task-plan-step inst-task))
	     (method (just~method (pdsc~an-just plan-step))))
	(if (string-equal (keim~name method) 'Dist-Prop-m)
	    ;; MV was created by the Dist-Prop-m-b Method
	    ;; next check whether SEM gives us a distiguishing property
	    't
	  nil))
    nil))

(defun compute-discriminant-property (meta-var)

  (setq hr*op (term~constant-create 'op (post~read-object '(i i i) (th~env 'base) :existing-type)))
  
  (let* ((inst-task (roc~start-task instmeta*roc-state-description))
	 (plan-step (agenda~inst-task-plan-step inst-task))
	 (node (pdsc~an-node plan-step))
	 (formula (node~formula node))
	 (args (data~appl-arguments (first (data~appl-arguments formula))))
	 (set1 (first args))
	 (set2 (third args))
	 (op1 (second args))
	 (op2 (fourth args))
	 
	 (class-factor1 (zmztac=class-factor set1))
	 (class-factor-number1 (keim~name class-factor1))
	 (number-list1 (crihelp=number-set-to-resclass-set set1))
	 (operation-on-nums1 (crihelp=convert-operation op1 class-factor1 nil))
	 (table1 (rcl~multiplication-table number-list1 class-factor-number1
					   :operation operation-on-nums1))
	 (row-lists1 (rcl~table table1))
	 (structure1 (do* ((rest-elems (apply #'append row-lists1) (rest rest-elems))
			   (back-string ""))
			 ((null rest-elems)
			  back-string)
		       (setf back-string (format nil "~A~A" back-string (first rest-elems)))))
	 (class-factor2 (zmztac=class-factor set2))
	 (class-factor-number2 (keim~name class-factor2))
	 (number-list2 (crihelp=number-set-to-resclass-set set2))
	 (operation-on-nums2 (crihelp=convert-operation op2 class-factor2 nil))
	 (table2 (rcl~multiplication-table number-list2 class-factor-number2
					   :operation operation-on-nums2))
	 (row-lists2 (rcl~table table2))
	 (structure2  (do* ((rest-elems (apply #'append row-lists2) (rest rest-elems))
			    (back-string ""))
			  ((null rest-elems)
			   back-string)
			(setf back-string (format nil "~A~A" back-string (first rest-elems)))))
	 (structures (list structure1 structure2))
	 (file-input (hr~prepare-input-file structures 'quasigroup))
	 (hr-output (hr~parse-output (hr~call-hr file-input)))
	 (prop (cadr (assoc structure1 hr-output :test #'string-equal)))
	 (zmz-prop (strathelp=generalize-prop prop)))
    (omega~message "HR found the property: ~A" zmz-prop)
    zmz-prop))

(defun strathelp=generalize-prop (prop)
  (let* ((env (th~env 'zmz))
	 (op hr*op)
	 (op-var (term~variable-create 'opv (post~read-object '((o num) (o num) (o num)) env :existing-type)))
	 (set-var (term~variable-create 'setv (post~read-object '(o (o num)) env :existing-type)))
	 (generalized-prop (strathelp=generalize-prop-rec prop op op-var set-var nil env)))
    (term~abstr-create (list set-var op-var)
		       generalized-prop)))

(defun strathelp=generalize-prop-rec (formula op op-var set-var replaced-vars env)
  (cond ((or (logic~universal-quantification-p formula)
	     (logic~existential-quantification-p formula))
	 (let* ((old-var (logic~quantification-bound-variable formula))
		(new-var (term~variable-create (make-symbol (format nil "~Arpl" (keim~name old-var)))
					       (post~read-object '(o num) env :existing-type))))
	   (term~appl-create (if (logic~universal-quantification-p formula)
				 (env~lookup-object 'forall-sort env)
			       (env~lookup-object 'exists-sort env))
			     (list (term~abstr-create (list new-var)
						      (strathelp=generalize-prop-rec (logic~quantification-scope formula) op op-var set-var
										     (cons (list old-var new-var) replaced-vars)
										     env))
				   set-var))))
	((term~appl-p formula)
	 (term~appl-create (strathelp=generalize-prop-rec (data~appl-function formula) op op-var set-var replaced-vars env)
			   (mapcar #'(lambda (arg)
				       (strathelp=generalize-prop-rec arg op op-var set-var replaced-vars env))
				   (data~appl-arguments formula))))
	((term~variable-p formula)
	 (cadr (assoc formula replaced-vars)))
	((keim~equal (data~schema-range (env~lookup-object '= env))
		     formula)
	 (env~lookup-object '= env))
	((eq formula op)
	 op-var)
	(t
	 formula)))

	 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRYANDERROR                                                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diese Strategie verfolgt auf den ResidueClass Sachen folgende Technik:
;; Fuer alle Forall-Quantifizierten Sachen erhaelt man Disjuntions-Hyp: (or (c1 = 0) (c1 = 1) ...) 
;; -> Mache case-Splitt darueber und probiere alle aus!
;; Fuer alle Exists-Quantifizierten Sachen erhaelt man Disjunction-Goal: (or (mv = 0) (mv = 1) ...)
;; -> Probiere fuer MV aus, ob eines passt! Beziehungsweise benuzte Hinweise - wenn moeglich - von Gap/Maple um MV 
;;    einfach passend zu bestimmen!

;; Termination Function of TryAndError Strategy
(defun no-further-goal-p ()
  nil)

;; Application Condition of TryAndError Strategy
(defun close-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (if (pdsn~current-formula node)
			  (pdsn~current-formula node)
			(node~formula node)))
	     (env (th~env 'zmz)))  ;;(pds~environment omega*current-proof-plan)))
	
	(cond ((and (logic~negation-p formula)
		    (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(data~schema-range (env~lookup-object 'isomorphic env))))
	       (let* ((iso-args (data~appl-arguments (first (data~appl-arguments formula))))
		      (set1 (first iso-args))
		      (set2 (third iso-args))
		      (card1 (strathelp=cardinality-of-residue-class-set set1))
		      (card2 (strathelp=cardinality-of-residue-class-set set2)))
		 (if (> (max card1 card2) 4)
		     nil
		   't)))
	      (t
	       (if (or (and (data~appl-p formula)
			    (or (keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'closed-under env)))
				;;(keim~equal (data~appl-function formula)
				;;	     (data~schema-range (env~lookup-object 'unit env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'divisors-exist env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'inverse-exist env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'associative env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'commutative env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'distributive env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'isomorphic env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'module-isomorphic env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'homomorphism env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'surjective env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'injective env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'module env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'submodule env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'prime-submodule env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'subset env)))
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'in env)))
				))
		       (and (logic~negation-p formula)
			    (data~appl-p (first (data~appl-arguments formula)))
			    (or (keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'closed-under env)))
				;;(keim~equal (data~appl-function (first (data~appl-arguments formula)))
				;;	     (data~schema-range (env~lookup-object 'unit env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'divisors-exist env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'inverse-exist env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'associative env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'commutative env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'distributive env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'isomorphic env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'module env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'submodule env)))
				(keim~equal (data~appl-function (first (data~appl-arguments formula)))
					    (data~schema-range (env~lookup-object 'prime-submodule env)))
				))
		       (and (logic~existential-quantification-p formula)
			    (data~appl-p (logic~quantification-scope formula))
			    (keim~equal (data~appl-function (logic~quantification-scope formula))
					(data~schema-range (env~lookup-object 'unit env))))
		       (and (logic~negation-p formula)
			    (logic~existential-quantification-p (first (data~appl-arguments formula)))
			    (data~appl-p (logic~quantification-scope (first (data~appl-arguments formula))))
			    (keim~equal (data~appl-function (logic~quantification-scope (first (data~appl-arguments formula))))
					(data~schema-range (env~lookup-object 'unit env))))
		       (strathelp=order-of-element-goal-p formula)
		       (strathelp=quantified-in-p formula)
		       (strathelp=task-from-dist-prop node)
		       )
		   't
		 nil))))
    nil))

(defun strathelp=task-from-dist-prop (node)
  (let* ((other-reason (first (pdsj~other-reasons (node~justification node))))
	 (just (if other-reason
		   (pdsc~an-just other-reason)
		 nil))
	 (method (if just
		     (just~method just)
		   nil)))
    (if (and method
	     (eq method (infer~find-method 'dist-prop-m)))
	't
      nil)))

		 
(defun strathelp=quantified-in-p (formula)
  (declare (edited  "20-MAY-2003")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form of an quantified in-formula."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (in-obj (data~schema-range (env~lookup-object 'in env)))
	 (forall-sort-obj  (data~schema-range (env~lookup-object 'forall-sort env))))
	 
    (do* ((current-formula formula)
	  (flag 't))
	((or (null current-formula)
	     (null flag))
	 flag)
      (cond ((and (data~appl-p current-formula)
		  (keim~equal (data~appl-function current-formula) in-obj))
	     (setf current-formula nil))
	    ((and (data~appl-p current-formula)
		  (keim~equal (data~appl-function current-formula) forall-sort-obj))
	     (setf current-formula (data~abstr-range (first (data~appl-arguments current-formula)))))
	    (t
	     (setf flag nil))))))

  

(defun strathelp=order-of-element-goal-p (formula)
  (declare (edited  "21-FEB-2001")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form of an order-goal."))
  (let* ((envi (env~create (pds~environment omega*current-proof-plan))))
    (post~read-object 'type-var envi :type-variable)
    (post~read-object '(;;(set1 (o (o num))) (set2 (o (o num)))
			;;(op1 ((o num) (o num) (o num))) (op2 ((o num) (o num) (o num)))
			(set1 (o type-var)) (set2 (o type-var))
			(op1 (type-var type-var type-var)) (op2 (type-var type-var type-var))
			(ibound num))
		      envi :variables-multiple)
    (term~alpha-match (post~read-object '(exists-sort (lam (n num)
							   (and (exists-sort (lam (el1 type-var)
										  (order-of-element set1 op1 el1 n))
									     set1)
								(not (exists-sort (lam (el2 type-var)
										       (order-of-element set2 op2 el2 n))
										  set2))))
						      (integer-intervall 1 ibound))
					envi
					:existing-term)
		      formula
		      )))

(defun strathelp=cardinality-of-residue-class-set (formula)
  (declare (edited  "20-FEB-2001")
	   (authors Ameier)
	   (input   "A formula, representing a residue class set.")
	   (effect  "None.")
	   (value   "The cardinality of the set."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (number-list (cond (;; 1.) (resclass-set n)
			     ;;     Formula is an application, starts with resclass-set, and has type o <- (o <- num)
			     
			     (zmz=resclass-set-formula-p formula)
			     (let* ((class-factor (first (data~appl-arguments formula))))
			       (zmztac=count-first-n-nats class-factor)))
			    (;; 2.) 2.1.: (setminus (resclass-set n) (singleton (resclass n m))) or
			     ;;     2.2.: (setminus (resclass-set n) (lam (x (o num)) (or (= x (resclass n m)) .... )))
			     ;;     Formula is an application, starts with setminus, has two arguments, the first argument satisfies the conditions of
			     ;;     the first case.
			     (and (data~appl-p formula)
				  (keim~equal (data~appl-function formula)
					      (data~schema-range (env~lookup-object :setminus
										    (pds~environment omega*current-proof-plan))))
				  (= (length (data~appl-arguments formula) 2)))
			     (multiple-value-bind
				 (flag1 class-factor1)
				 (zmz=resclass-set-formula-p (first (data~appl-arguments formula)))
			       ;; 2.1. (setminus (resclass-set n) (singleton (resclass n m)))
			       (multiple-value-bind
				   (flag2 class-factor2 residuum)
				   (zmz=resclass-singleton-p (second (data~appl-arguments formula)))
				 (if flag2
				     (remove residuum (zmztac=count-first-n-nats class-factor1) :test #'keim~equal)
				   (multiple-value-bind
				       (flag3 class-factor3 residues)
				       (zmz=or-set-of-resclass-p (second (data~appl-arguments formula)))
				     (if flag3
					 (set-difference (zmztac=count-first-n-nats class-factor1) residues :test #'keim~equal)
				       nil))))))
			    (;; 
			     (zmz=or-set-of-resclass-p formula)
			     
			     (multiple-value-bind
				 (flag class-factor residues)
				 (zmz=or-set-of-resclass-p formula)
			       
			       residues)))))
    (length number-list)))
    

(strat~define-strategy-ks
 (name TryAndError)
 (refinement-algorithm PPlanner)
 (condition close-goal-p)
 (methods (DefnExp-m-b
	   ForallI-Sort-Resclass-Function-m-b
	   ForallI-Sort-Resclass-m-b
	   ForallI-Sort-Integer-Interval-m-b
	   Convert-Resclass-To-Num-m-b
	   Or-E**-m-b
	   ExistsI-Sort-Resclass-Function-m-b
	   ExistsI-Sort-Resclass-m-b
	   ExistsI-Sort-Integer-Interval-m-b
	   OrIL-m-b OrIR-m-b
	   Expand-Pair-Operation-m-b
	   Expand-Scalar-Operation-m-b
	   Reduce-StructUnit-m-b
	   Reduce-StructInverse-m-b
	   embedgen-m-b
	   Expand-in-m-b
	   Expand-not-in-m-b
	   
	   ;; The following methods are all in the normalization and restriction methods
	   ;; Therefore, we placed them here after the other methods
	   ;; Notice that  Convert-Resclass-To-Num-m-b, Expand-Pair-Operation-m-b could be also in
	   ;; the restriction methods, but:
	   ;; Convert-resclass is VERRY VERRY complex hence, you will not always try to apply it!
	   ;; Expand-Pair-Operation-m-b caused some problems when in the restriction methods (i forgot the exact problem ...)
	   ;; Since Simplify-Resclass-m-b should be tried after Expand-Pair-Operation-m-b it is also not in the restriction methods

	   AndE-M-F PosNat-m-b RewritePowerOfOp-m-b
	   Expand-in-m-f Expand-not-in-m-f
	   PullNeg-m-b AndI-M-B Or2Imp-m-b
	   Reflexu-m-b NotReflex-m-b
	   Reduce-Exists-To-Pairs-m-b
	   Reduce-Forall-To-Pairs-m-b
	   DecompNotElementOfCartProd-m-b
	   DecompElementOfCartProd-m-b
	   DecompPairinInequation-m-b
	   DecompPairinEquation-m-b
	   Rewrite-First-Second-Pair-m-b
	   Apply-Def-Function-m-b               ;; replaces SimplifyThat-m-b
	   Simplify-Numerical-Expr-m-b
	   ))
 (normalization-methods (AndE-M-F Expand-in-m-f Expand-not-in-m-f))
 (restriction-methods (PullNeg-m-b AndI-M-B Or2Imp-m-b
		       Reflexu-m-b NotReflex-m-b
		       PosNat-m-b RewritePowerOfOp-m-b
		       Reduce-Forall-To-Pairs-m-b
		       Reduce-Exists-To-Pairs-m-b
		       DecompElementOfCartProd-m-b
		       DecompNotElementOfCartProd-m-b
		       DecompPairinEquation-m-b
		       DecompPairinInequation-m-b
		       embedgen-m-b
		       Expand-in-m-b
		       Reduce-StructUnit-m-b
		       Reduce-StructInverse-m-b
		       ;Expand-not-in-m-b
		       ;; Expand-Pair-Operation-m-b
		       ;; Expand-Scalar-Operation-m-b
		       ;; it was not possible to have Expand-Pair-Operation-m-b in the restrictions (i forgot the reason ...)
		       Rewrite-First-Second-Pair-m-b             ;; VERRY COMPLEX -> at the end of the list
		       Apply-Def-Function-m-b                    ;; ersetzt SimplifyThat-m-b ;; comlpex -> at the end of the list
		       Simplify-Numerical-Expr-m-b               ;; VERRY COMPLEX -> at the end of the list
		       ))
 (control-rules (zmz-standard-select
		 zmz-defnexp-select
		 zmz-prefer-metaors
		 zmz-interrupt-if-insttask
		 zmz-interrupt-for-other-strat
		 zmz-oril-select
		 zmz-orir-select
		 zmz-prefer-simple-parts
		 zmz-reject-methods-in-wrong-order
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is solved by try and error.")
 (print "Strategy-KS TryAndError: Offer to proof task ~A by trying each possibility"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CRULES OF TryAndError STRATEGY                                                                           
(cri~def-control-rule zmz-interrupt-for-other-strat
		      (kind strategy-interruption)
		      (if (task-suitable-for-strat-p "task" "strat"))
		      (then
		       (insert (("strat" "task")))))
		      
(cri~def-control-rule zmz-interrupt-if-insttask
		      (kind strategy-interruption)
		      (if (information-for-inst-task-p "insttask" "params"))
		      (then
		       (insert ((InstFromCAS "insttask")))))

(cri~def-control-rule zmz-prefer-metaors
		      (kind tasks)
		      (if (metaor-p "goal"))
		      (then
		       (prefer ("goal"))))

(cri~def-control-rule zmz-oril-select
		      (kind methods)
		      (if (and (goal-matches ("goal" (or "arg1" "arg2")))
			       (closable "arg1")))
		      (then
		       (select (ORIL-M-B))))

(cri~def-control-rule zmz-orir-select
		      (kind methods)
		      (if (and (goal-matches ("goal" (or "arg1" "arg2")))
			       (closable "arg2")))
		      (then
		       (select (ORIR-M-B))))

(cri~def-control-rule zmz-defnexp-select
		      (kind methods)
		      (if (and (or (goal-matches ("goal" ("rel" "arg1" "arg2")))
				   (goal-matches ("goal" (not ("rel" "arg1" "arg2")))))
			       (symbol-member "rel" (ORDER-OF-ELEMENT homomorphism isomorphic module-isomorphic injective surjective closed-under in associative unit inverse-exist divisors-exist commutative distributive group ring module abelian-group module-distributivity1 module-distributivity2 module-associativity module-unitary subset in))
			       ))
		      (then
		       (select ((DefnExp-m-b () ("rel"))
				))))

(defun special-representation (term)
  (term~special-p term))
		      
(cri~def-control-rule zmz-standard-select
		      (kind methods)
		      (if (or-supports ("or-supps")))
		      (then
		       (select (ForallI-Sort-Resclass-Function-m-b
				ForallI-Sort-Resclass-m-b
				;;;ForallI-Sort-Resclass-tuple-m-b
				Convert-Resclass-To-Num-m-b
				(Or-E**-m-b () ("or-supps"))
				ExistsI-Sort-Resclass-Function-m-b
				ExistsI-Sort-Resclass-m-b
				OrIL-m-b
				OrIR-m-b
				Expand-Pair-Operation-m-b
				Expand-Scalar-Operation-m-b
				Expand-in-m-b
				Expand-not-in-m-b
				))))

(cri~def-control-rule zmz-prefer-simple-parts
		      (kind methods)
		      (if (and (goal-matches ("goal" (or "arg1" "arg2")))
			       (simpler-p "arg2" "arg1")))			   
		      (then
		       (order-before ((OrIR-m-b OrIL-m-b)))))

(cri~def-control-rule zmz-reject-methods-in-wrong-order
		      (kind methods)
		      (if (and (already-applied-methods "applied-methods")
			       (less-ordered-method "less-method" "applied-methods")))
		      (then
		       (reject ("less-method"))))

;;(defun symbolofterm (rel srel)
;;  (cond ((and (null (stringp rel))
;;	      (null (stringp srel)))
;;	 ;; both arguments are bound
;;	 (if (string-equal srel (keim~name rel))
;;	     (list (list (cons T T)))
;;	   nil))
;;	((and (null (stringp rel))
;;	      (stringp srel))
;;	 ;; rel bound but srel unbound
;;	 (list (list (cons srel (keim~name rel)))))
;;	(t
;;	 (omega~warning "~%In Metapred symbolofterm: Not specified application!")
;;	 nil)))
	
(defun simpler-p (arg1 arg2)
  (declare (edited  "02-MAR-2001")
	   (authors Ameier)
	   (input   "Two terms.")
	   (effect  "NOne.")
	   (value   "Checks whether the fist arg is simpler than the second arg with respect to a few heuristics."
		    "If this is the case T, otherwise nil."))
  (if (and (term~p arg1)
	   (term~p arg2))
      (cond ((logic~disjunction-p arg2)
	     (let* ((iargs (data~appl-arguments arg2))
		    (iarg1 (first iargs))
		    (iarg2 (second iargs))
		    (unit-el (data~schema-range (env~lookup-object 'unit (pds~environment omega*current-proof-plan))))
		    (closed-under-el (data~schema-range (env~lookup-object 'closed-under (pds~environment omega*current-proof-plan)))))
	       (if (and (logic~negation-p iarg1)
			(logic~negation-p iarg2)
			(data~appl-p (first (data~appl-arguments iarg1)))
			(data~appl-p (first (data~appl-arguments iarg2)))
			(or (keim~equal (data~appl-function (first (data~appl-arguments iarg1)))
					unit-el)
			    (keim~equal (data~appl-function (first (data~appl-arguments iarg1)))
					closed-under-el))
			(or (keim~equal (data~appl-function (first (data~appl-arguments iarg2)))
					unit-el)
			    (keim~equal (data~appl-function (first (data~appl-arguments iarg2)))
					closed-under-el)))
		   (list (list (cons T T)))
		 nil)))
	    ((and (logic~negation-p arg2)
		  (logic~negation-p arg1))
	     (let* ((unit-el (data~schema-range (env~lookup-object 'unit (pds~environment omega*current-proof-plan))))
		    (closed-under-el (data~schema-range (env~lookup-object 'closed-under (pds~environment omega*current-proof-plan)))))
	       (if (and (data~appl-p (first (data~appl-arguments arg1)))
			(data~appl-p (first (data~appl-arguments arg2)))
			(keim~equal (data~appl-function (first (data~appl-arguments arg2)))
				    closed-under-el)
			(keim~equal (data~appl-function (first (data~appl-arguments arg1)))
				    unit-el))
		   (list (list (cons T T)))
		 nil)))
	    (t
	     nil))))


(defun task-suitable-for-strat-p (task strat)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (interrupt-goal-tasks-and-strats (apply #'append (mapcar #'crihelp=interrupt-for-strat-p
								  goal-tasks))))
    (mapcar #'(lambda (task-strat-pair)
		(let* ((itask (first task-strat-pair))
		       (istrat (second task-strat-pair)))
		  (list (cons task itask)
			(cons strat istrat))))
	    interrupt-goal-tasks-and-strats)))

	 
(defun crihelp=interrupt-for-strat-p (goal-task)
  (let* ((node (agenda~task-node goal-task))
	 (just (node~justification node))
	 (formula (node~formula node)))
    (cond ((and (null (crihelp=strat-already-applied-p goal-task 'equsolve))
		(data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'unit (pds~environment omega*current-proof-plan)))))
	   ;; -> task is unit task
	   ;; -> check whether the predecessor of this node was exists-sort e:RS ...
	   ;; if this was the case either this task should have been send to EQUSOLVE (another case in this cond loop)
	   ;; or it was already send to EQUSOLVE and failed.
	   (unless (eq goal-task (roc~start-task pplan*roc-state-description))
	     (let* ((other-reasons (pdsj~other-reasons just))
		    (pred-just (pdsc~an-just (first other-reasons)))
		    (method (just~method pred-just)))
	       (if (or (string-equal (keim~name method) 'ExistsI-Sort-Resclass-m)
		       (string-equal (keim~name method) 'ExistsI-Sort-Resclass-m-b))
		   nil
		 (list (list goal-task 'equsolve))))))
	  ((and (null (crihelp=strat-already-applied-p goal-task 'reducetospecial))
		(null (eq goal-task (roc~start-task pplan*roc-state-description)))
		(data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'closed-under (pds~environment omega*current-proof-plan)))))
	   ;; -> task is closed task and was not tried before by ReduceToSpecial and is not the start-task
	   (list (list goal-task 'reducetospecial)))
	  (t
	   nil))))

(defun crihelp=strat-already-applied-p (goal-task strat)
  (let* ((already-applied-strat-and-param-pairs (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node goal-task))))))
    (find (strat~find-strategy-ks strat) already-applied-strat-and-param-pairs
	  :test #'(lambda (equ pair)
		    (eq equ (first pair))))))


(defun already-applied-methods (meth-list)
  (declare (edited  "04-JUN-2000")
	   (authors Ameier)
	   (input   "A string, which is a meta-variable for a list of methods.")
	   (effect  "None.")
	   (value   "The list of all methods which are already applied on the current task (in cri*current-task)."
		    "If this list is nil, nil is returned. Otherwise the input string is bound on the list of"
		    "these methods, and a list with this binding is retruned (the binding itself is only a"
		    "assoc pair)."))
  (let* ((task cri*current-task)
	 (task-node (agenda~task-node task))
	 (task-just (node~justification task-node))
	 (already-applied-mmatchings (keim::pdsc~already-applied-mmatchings (pdsj~control task-just)))
	 (already-applied-methods (mapcar #'pplan~matched-method already-applied-mmatchings)))
    (if already-applied-methods
	(list (list (list meth-list already-applied-methods)))
      nil)))


(defun less-ordered-method (less-method selected-methods)
  (declare (edited  "04-JUN-2000")
	   (authors Ameier)
	   (input   "A string (which is a meta-variable for a method) and a list of methods.")
	   (effect  "None.")
	   (value   "Computes the set of less-methods to the selected methods list (wrt. the current pplanner strategy)."
		    "Then the input string is bound on each of these less methods and this lists of bindings is returned."))
  (let* ((current-strategy (roc~strategy-ks pplan*roc-state-description))
	 (less-methods (cond ((eq current-strategy (strat~find-strategy-ks 'TryAndError))
			      ;;(format t "~%~%~%~%~%~%~~%~%~%~% HERE")
			      (apply #'append (mapcar #'tryanderrr-less-methods-of-method selected-methods))
			      )
			     (t
			      nil))))
    (mapcar #'(lambda (less-meth)
		(list (cons less-method less-meth)))
	    less-methods)))


#|
(defun tryanderrr-less-methods-of-method (method)
  
  ;; Methods which are in the ranking higher forbid to use the methods in the ranking below them
  
  (cond ((or (eq method (infer~find-method 'Or-E**-m))
	     (eq method (meth~find-method 'Or-E**-m-b)))
	 
	 ;; Or-E**-m-b forbids:
	 
	 (list (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'Convert-Resclass-To-Num-m))
	     (eq method (meth~find-method 'Convert-Resclass-To-Num-m-b)))
	 
	 ;; Convert-Resclass-To-Num forbids:
	 
	 (list (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'ForallI-Sort-Resclass-m))
	     (eq method (meth~find-method 'ForallI-Sort-Resclass-m-b))
	     )
	 
	 ;; ForallI-Sort-Resclass-m-b forbids:
	 
	 (list (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'Simplify-Numerical-Expr-m))
	     (eq method (meth~find-method 'Simplify-Numerical-Expr-m-b)))
	 
	 ;; Simplify-Numerical-Expr-m forbids:

	 ;; apply-def-function-m forbids:
	 
	 (list (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'apply-def-function-m))
	     (eq method (meth~find-method 'apply-def-function-m-b)))

	  ;; apply-def-function-m forbids:
	 
	 (list (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'Rewrite-First-Second-Pair-m))
	     (eq method (meth~find-method 'Rewrite-First-Second-Pair-m-b)))
	 
	 ;; Rewrite-First-Second-Pair-m forbids:
	 
	 (list (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
		
	((or (eq method (infer~find-method 'DecompPairinInequation-m))
	     (eq method (meth~find-method 'DecompPairinInequation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'DecompPairinEquation-m))
	     (eq method (meth~find-method 'DecompPairinEquation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'DecompNotElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompNotElementOfCartProd-m-b)))
	 
	 ;; DecompNotElementOfCartProd-m forbids:
	 
	 (list (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	
	((or (eq method (infer~find-method 'DecompElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompElementOfCartProd-m-b)))
	 
	 ;; DecompElementOfCartProd-m-b forbids:
	 
	 (list (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))

	((or (eq method (infer~find-method 'Reduce-Exists-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Exists-To-Pairs-m-b)))
	 
	 ;; Reduce-Exists-To-Pairs-m-b forbids:
	 
	 (list (meth~find-method 'DecompElementOfCartProd-m-b)
	       (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))


	((or (eq method (infer~find-method 'Reduce-Forall-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Forall-To-Pairs-m-b)))
	 
	 ;; Reduce-Forall-To-Pairs-m-b forbids:
	 
	 (list (meth~find-method 'Reduce-Exists-To-Pairs-m-b)
	       (meth~find-method 'DecompElementOfCartProd-m-b)
	       (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))

	((or (eq method (infer~find-method 'Or2Imp-m))
	     (eq method (meth~find-method 'Or2Imp-m-b)))
	 
	 ;; Or2Imp-m-b forbids:
	 
	 (list (meth~find-method 'Reduce-Forall-To-Pairs-m-b)
	       (meth~find-method 'Reduce-Exists-To-Pairs-m-b)
	       (meth~find-method 'DecompElementOfCartProd-m-b)
	       (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))

	((or (eq method (infer~find-method 'AndI-M))
	     (eq method (meth~find-method 'AndI-M-B)))
	 
	 ;; AndI-M-B forbids:
	 
	 (list (meth~find-method 'Or2Imp-m-b)
	       (meth~find-method 'Reduce-Forall-To-Pairs-m-b)
	       (meth~find-method 'Reduce-Exists-To-Pairs-m-b)
	       (meth~find-method 'DecompElementOfCartProd-m-b)
	       (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))

	((or (eq method (infer~find-method 'PullNeg-m))
	     (eq method (meth~find-method 'PullNeg-m-b)))
	 
	 ;; PullNeg-m-b forbids:
	 
	 (list (meth~find-method 'AndI-M-B)
	       (meth~find-method 'Or2Imp-m-b)
	       (meth~find-method 'Reduce-Forall-To-Pairs-m-b)
	       (meth~find-method 'Reduce-Exists-To-Pairs-m-b)
	       (meth~find-method 'DecompElementOfCartProd-m-b)
	       (meth~find-method 'DecompNotElementOfCartProd-m-b)
	       (meth~find-method 'DecompPairinEquation-m-b)
	       (meth~find-method 'DecompPairinInequation-m-b)
	       (meth~find-method 'Rewrite-First-Second-Pair-m-b)
	       (meth~find-method 'Simplify-Numerical-Expr-m-b)
	       (meth~find-method 'apply-def-function-m-b)
	       (meth~find-method 'ForallI-Sort-Resclass-m-b)
	       (meth~find-method 'Convert-Resclass-To-Num-m-b)
	       (meth~find-method 'Or-E**-m-b)
	       (meth~find-method 'ExistsI-Sort-Resclass-m-b)
	       (meth~find-method 'OrIL-m-b)
	       (meth~find-method 'OrIR-m-b)
	       (meth~find-method 'Expand-Pair-Operation-m-b)
	       ))
	))
|#


(defun tryanderrr-less-methods-of-method (method)
  
  ;; Methods which are in the ranking higher forbid to use the methods in the ranking below them
  
  (cond ((or (eq method (infer~find-method 'Or-E**-m))
	     (eq method (meth~find-method 'Or-E**-m-b)))
	 
	 ;; Or-E**-m-b forbids:
	 
	 (list 'ExistsI-Sort-Resclass-m-b
	       'OrIL-m-b
	       'OrIR-m-b
	       'Expand-Pair-Operation-m-b
	       ))
	
	((or (eq method (infer~find-method 'Convert-Resclass-To-Num-m))
	     (eq method (meth~find-method 'Convert-Resclass-To-Num-m-b)))
	 
	 ;; Convert-Resclass-To-Num forbids:
	 
	 (list 'Or-E**-m-b
	       'ExistsI-Sort-Resclass-m-b
	       'OrIL-m-b
	       'OrIR-m-b
	       'Expand-Pair-Operation-m-b
	       ))
	
	((or (eq method (infer~find-method 'ForallI-Sort-Resclass-m))
	     (eq method (meth~find-method 'ForallI-Sort-Resclass-m-b))
	     )
	 
	 ;; ForallI-Sort-Resclass-m-b forbids:
	 
	 (list  'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Simplify-Numerical-Expr-m))
	     (eq method (meth~find-method 'Simplify-Numerical-Expr-m-b)))
	 
	 ;; Simplify-Numerical-Expr-m forbids:
	 
	 ;; apply-def-function-m forbids:
	 
	 (list  'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'apply-def-function-m))
	     (eq method (meth~find-method 'apply-def-function-m-b)))
	 
	 ;; apply-def-function-m forbids:
	 
	 (list  'Simplify-Numerical-Expr-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Rewrite-First-Second-Pair-m))
	     (eq method (meth~find-method 'Rewrite-First-Second-Pair-m-b)))
	 
	 ;; Rewrite-First-Second-Pair-m forbids:
	 
	 (list  'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompPairinInequation-m))
	     (eq method (meth~find-method 'DecompPairinInequation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list  'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompPairinEquation-m))
	     (eq method (meth~find-method 'DecompPairinEquation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list  'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompNotElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompNotElementOfCartProd-m-b)))
	 
	 ;; DecompNotElementOfCartProd-m forbids:
	 
	 (list  'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompElementOfCartProd-m-b)))
	 
	 ;; DecompElementOfCartProd-m-b forbids:
	 
	 (list  'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Reduce-Exists-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Exists-To-Pairs-m-b)))
	 
	 ;; Reduce-Exists-To-Pairs-m-b forbids:
	 
	 (list  'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	
	((or (eq method (infer~find-method 'Reduce-Forall-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Forall-To-Pairs-m-b)))
	 
	 ;; Reduce-Forall-To-Pairs-m-b forbids:
	 
	 (list  'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Or2Imp-m))
	     (eq method (meth~find-method 'Or2Imp-m-b)))
	 
	 ;; Or2Imp-m-b forbids:
	 
	 (list  'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'AndI-M))
	     (eq method (meth~find-method 'AndI-M-B)))
	 
	 ;; AndI-M-B forbids:
	 
	 (list  'Or2Imp-m-b
	        'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'PullNeg-m))
	     (eq method (meth~find-method 'PullNeg-m-b)))
	 
	 ;; PullNeg-m-b forbids:
	 
	 (list  'AndI-M-b
	        'Or2Imp-m-b
	        'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	))

(defun closable (term)
  (if (stringp term)
      ;; noch ungebunden
      nil
    (if (crihelp=closable-p term)
	(list (list (cons T T)))
      nil)))

(defun metaor-p (goaltask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (metaor-goal-tasks (remove-if-not #'(lambda (task)
					       (let* ((node (agenda~task-node task))
						      (formula (node~formula node)))
						 (crihelp=metaor-formula-p formula)))
					   goal-tasks)))
    (mapcar #'(lambda (mog)
		(list (cons goaltask mog)))
	    metaor-goal-tasks)))

(defun information-for-inst-task-p (insttask params) 
  (let* ((tasks cri*current-tasks)  ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
	 (start-task (roc~start-task pplan*roc-state-description)))
    
    (apply #'append (mapcar #'(lambda (inst-task)
				(let* ((param-to-inst-task (crihelp=param-to-inst-task inst-task)))
				  (if (and param-to-inst-task
					   (null (already-tried-p inst-task param-to-inst-task)))
				      (list (list (cons insttask inst-task)
						  (cons params (list param-to-inst-task))))
				    nil)))
			    inst-tasks))))

(defun already-tried-p (inst-task param)
  (declare (edited  "30-AUG-2000")
	   (authors Ameier)
	   (input   "A instantiation task and a parameter suggestion from the hint system, how to instantiate"
		    "this inst task.")
	   (effect  "None.")
	   (value   "T if in the already-applied-strategies of the inst-task there is already a entry of the"
		    "strategy InstFromCAS with this parameter."))
  (let* ((already-applied-strategies (agenda~task-already-applied-strategy-ks inst-task))
	 (with-param (remove-if-not #'(lambda (strat-param-pair)
					(and (eq (first strat-param-pair) (strat~find-strategy-ks 'InstFromCAS))
					     (keim~equal (second strat-param-pair) (list param))))
				    already-applied-strategies)))
    with-param))
					     
  
(defun or-supports (parameters)
  (declare (edited  "08-MAR-2000")
	   (authors Ameier)
	   (input   "A list of strings (here one string: \"or-supps\").")
	   (effect  "None.")
	   (value   "A binding of the string to the set of all hypotheses of the cri*current-task"
		    "that have disjuntion formulas."))
  (let* ((task cri*current-task)
	 (task-node (agenda~task-node task))
	 (supps (pds~node-supports task-node))
	 (or-supps (remove-if-not #'(lambda (node)
				      (logic~disjunction-p (node~formula node)))
				  supps)))

    (if or-supps
	(list (list (list (first parameters) or-supps)));; (crihelp=needed-or-supps or-supps task-node))))
      nil)))

;; also it was a good idea to reduce the number of or-supports used in a case split by the function crihelp=needed-or-supps;
;; it turned out to be problemetic when proving isomorphism proofs

(defun crihelp=needed-or-supps (or-supps task-node)
  (declare (edited  "26-JAN-2001")
	   (authors Ameier)
	   (input   "A list of support disjucntions and the corresponding task-node.")
	   (effect  "None.")
	   (value   "A reduced list of dijsunction nodes in which all disjunction that are of the form"
		    "(or (= c n1) (= c n2) ...) where n1, n2, ... are numbers such that c does not occur in"
		    "the task-node are removed."))
  (let* ((task-formula (node~formula task-node))
	 (constants-in-task-formula (remove-if-not #'term~constant-p
						   (data~all-substructs task-formula))))
    (remove-if #'(lambda (or-supp)
		   (let* ((const? (crihelp=disjunction-of-constant-p (node~formula or-supp))))
		     (if (and const?
			      (null (find const? constants-in-task-formula :test #'keim~equal)))
			 't
		       nil)))
	       or-supps)))	  

(defun crihelp=disjunction-of-constant-p (formula)
  (declare (edited  "26-JAN-2001")
	   (authors Ameier)
	   (input   "A foumula.")
	   (effect  "None.")
	   (value   "If the formula has the form (or (= c n1) (= c n2) ...) where n1, n2, ... are numbers"
		    "then the constant c is returned, otherwise nil."))
  (if (null (data~appl-p formula))
      nil
    (let* ((func (data~appl-function formula))
	   (args (data~appl-arguments formula))
	   (env (pds~environment omega*current-proof-plan)))
      (cond ((keim~equal func (env~lookup-object 'or env))
	     (let* ((ret1 (crihelp=disjunction-of-constant-p (first args)))
		    (ret2 (crihelp=disjunction-of-constant-p (second args))))
	       (if (and ret1
			ret2
			(keim~equal ret1 ret2))
		   ret1
		 nil)))
	    ((keim~equal func (data~schema-range (env~lookup-object '= env)))
	     (cond ((and (term~number-p (first args))
			 (term~constant-p (second args)))
		    (second args))
		   ((and (term~number-p (second args))
			 (term~constant-p (first args)))
		    (first args))
		   (t
		    nil)))
	    (t
	     nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equsolve Strategie                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The idea of this strategy is to reduce the task to an equation that can be closed by maple.
;; Hence, it is only applicable on positive associative,unit,inverse, and divisors Task

;; Application Condition of strategy Equsolve
(defun equsolve-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (if (pdsn~current-formula node)
			  (pdsn~current-formula node)
			(node~formula node)))
	     (env (th~env 'zmz)))  ;;(pds~environment omega*current-proof-plan)))
	
	(if (or (and (data~appl-p formula)
		     (or (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'unit env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'divisors-exist env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'inverse-exist env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'associative env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'commutative env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'isomorphic env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'module-isomorphic env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'surjective env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'homomorphism env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'distributive env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'module env)))
			 ))
		(and (logic~existential-quantification-p formula)
		     (data~appl-p (logic~quantification-scope formula))
		     (keim~equal (data~appl-function (logic~quantification-scope formula))
				 (data~schema-range (env~lookup-object 'unit env))))
		)
	    't
	  nil))
    nil))

;; Termination Condition of Strategy Equsolve
(defun no-further-goal-except-closed-under-p ()
  (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						 (and (agenda~goal-or-goal-schema-task-p task)
						      (find pplan*roc-state-description (agenda~task-rocs task))))
					     all-tasks))
	 (goal-tasks-with-roc-and-not-closed-under
	  (remove-if #'(lambda (task)
			 (let* ((formula (node~formula (agenda~task-node task))))
			   (and (data~appl-p formula)
				(keim~equal (data~appl-function formula)
					    (data~schema-range (env~lookup-object 'closed-under
										  (pds~environment omega*current-proof-plan)))))))
		     goal-tasks-with-roc)))
    (if goal-tasks-with-roc-and-not-closed-under
	nil
      't)))


(strat~define-strategy-ks
 (name equsolve)
 (refinement-algorithm PPlanner)
 (condition equsolve-goal-p)
 (methods (ForallI-Sort-Resclass-m-b
	   Convert-Resclass-To-Num-m-b
	   Popmod-m-b
	   solve-modulo-equation-m-b
	   DecompFunctionInEquation-m-b
	   embedgen-m-b
	   ;;solve-modulo-equation-m-b
	   solve-equation-m-b
	   ExistsI-Sort-Resclass-m-b
	   Reduce-StructUnit-m-b
	   DefnExp-m-b
	   ANDI-m-b
	   WEAKENALPHA-M-A  ;;Weaken-m-a
	   OrIL-m-b OrIR-m-b
	   OpClosedUnder-m-b
	   IsomorphicI-Function-M-b
	   ModuleIsomorphicI-Function-M-b
	   
	   ;; The following are methods necessary to handle cartesian products
	   Reduce-Exists-To-Pairs-m-b
	   Reduce-Forall-To-Pairs-m-b
	   Rewrite-First-Second-Pair-m-b
	   Expand-Pair-Operation-m-b
	   Expand-Scalar-Operation-m-b
	   DecompNotElementOfCartProd-m-b
	   DecompElementOfCartProd-m-b
	   DecompPairinInequation-m-b
	   DecompPairinEquation-m-b

	   Expand-in-m-f
	   Expand-not-in-m-f
	   Expand-in-m-b
	   Expand-not-in-m-b
	   ))
 (normalization-methods (
			 Expand-in-m-f
			 Expand-not-in-m-f
			 ))
 (restriction-methods (ANDI-m-b WEAKENALPHA-M-A ;; Weaken-m-a
		       Reduce-Exists-To-Pairs-m-b
		       Reduce-Forall-To-Pairs-m-b
		       Rewrite-First-Second-Pair-m-b
		       ;;Expand-Pair-Operation-m-b
		       ;;Expand-Scalar-Operation-m-b
		       embedgen-m-b
		       Reduce-StructUnit-m-b
		       DecompNotElementOfCartProd-m-b
		       DecompElementOfCartProd-m-b
		       DecompPairinInequation-m-b
		       DecompPairinEquation-m-b
		       Expand-in-m-b
		       Expand-not-in-m-b
		       ))
 (control-rules (zmz-defnexp-select
		 zmz-interrupt-if-iso-insttask
		 zmz-select-isofunc ;;zmz-find-polynom
		 zmz-prefer-notmetaors
		 zmz-reject-closed-unders
		 zmz-oril-select
		 zmz-orir-select
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-except-closed-under-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is closed by solving the equation.")
 (print "Strategy-KS Equsolve: Offer to prove task ~A by solving equation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ; CRULES OF EQUSolve STRATEGY

(cri~def-control-rule zmz-prefer-notmetaors
		      (kind tasks)
		      (if (notmetaor-p "goal"))
		      (then
		       (prefer ("goal"))))

(cri~def-control-rule zmz-reject-closed-unders
		      (kind tasks)
		      (if (closedunder-p "goal"))
		      (then
		       (reject ("goal"))))

;;(cri~def-control-rule zmz-find-polynom
	;;	      (kind methods)
	;;	      (if (and (goal-is-isomorphic-on-resclasses)
	;;		       (find-polynom-function-for-goal "polyfunc")))
	;;	      (then
	;;	       (select ((IsomorphicI-Function-M-b () ("polyfunc"))))))
			 

(cri~def-control-rule zmz-select-isofunc
		      (kind methods)
		      (if (goal-is-isomorphic-on-resclasses))
		      (then
		       (select ((IsomorphicI-Function-M-b () ())
				(ModuleIsomorphicI-Function-M-b () ())))))


(cri~def-control-rule zmz-interrupt-if-iso-insttask
		      (kind strategy-interruption)
		      (if (and (isoinsttask-p "insttask")
			       (isofunctionfor-isoinsttask-p "insttask")))
		      (then
		       (insert ((InstIsoByPolyFromCAS "insttask")))))


(defun isoinsttask-p (insttask)
  (let* ((tasks cri*current-tasks)  ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
	 (inst-tasks-from-ISO (remove-if-not #'(lambda (inst-task)
						 (let* ((plan-step (agenda~inst-task-plan-step inst-task))
							(just (pdsc~an-just plan-step))
							(method (just~method just)))
						   (if (or (string-equal (keim~name method) 'IsomorphicI-Function-M-b)
							   (string-equal (keim~name method) 'IsomorphicI-Function-M)
							   (string-equal (keim~name method) 'ModuleIsomorphicI-Function-M-b)
							   (string-equal (keim~name method) 'ModuleIsomorphicI-Function-M))
						       't
						     nil)))
					     inst-tasks)))
    (mapcar #'(lambda (inst-task)
		(list (cons insttask inst-task)))
	    inst-tasks-from-ISO)))

(defun isofunctionfor-isoinsttask-p (insttask)
  (if (null insttask)
      nil
    (let* ((node (pdsc~an-node (agenda~inst-task-plan-step insttask)))
	   (polynom-functions (strathelp=find-polynom-function-from-node node)))
      (if polynom-functions
	  (list (list (cons T T)))
	nil))))




(defun goal-is-isomorphic-on-resclasses ()
  (let* ((task cri*current-task)
	 (task-node (agenda~task-node task))
	 (formula (node~formula task-node))
	 (env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p formula)
	     (or (keim~equal (data~appl-function formula)
			     (data~schema-range (env~lookup-object 'isomorphic env)))
		 (keim~equal (data~appl-function formula)
			     (data~schema-range (env~lookup-object 'module-isomorphic env))))
	     (or (= (length (data~appl-arguments formula)) 4)
		 (= (length (data~appl-arguments formula)) 7)))
	(let* ((arg1 (first (data~appl-arguments formula)))
	       (arg2 (third (data~appl-arguments formula))))
	  (if (and (ZMZTAC=PROD-OF-RESCLASS-SETS-P arg1)
		   (ZMZTAC=PROD-OF-RESCLASS-SETS-P arg2))
	      (list (list (cons T T)))
	    nil))
      nil)))

(defun find-polynom-function-for-goal (polynom)
  (let* ((task cri*current-task)
	 (task-node (agenda~task-node task))
	 (formula (node~formula task-node))
	 (arg1 (first (data~appl-arguments formula)))
	 (arg2 (third (data~appl-arguments formula)))
	 (polynom-function (crihelp=find-polynom-function task)))
    (if polynom-function
	(list (list (cons polynom polynom-function)))
      nil)))

(defun goal-has-subterm (term-expr)
  (let* ((goal-formula (node~formula (agenda~task-node cri*current-task)))
	 (subterm (post~read-object term-expr (pds~environment omega*current-proof-plan) :existing-term)))
    (if (find subterm (data~all-substructs goal-formula) :test #'keim~equal)
	(list (list (cons T T)))
      nil)))

(defun notclosedunder-p (goaltask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (not-closed-goal-tasks (remove-if #'(lambda (task)
					       (let* ((node (agenda~task-node task))
						      (formula (node~formula node)))
						 (and (data~appl-p formula)
						      (keim~equal (data~appl-function formula)
								  (data~schema-range
								   (env~lookup-object 'closed-under
										      (pds~environment omega*current-proof-plan)))))))
					   goal-tasks)))
    (mapcar #'(lambda (nclgt)
		(list (cons goaltask nclgt)))
	    not-closed-goal-tasks)))

(defun closedunder-p (goaltask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (closed-goal-tasks (remove-if-not #'(lambda (task)
					       (let* ((node (agenda~task-node task))
						      (formula (node~formula node)))
						 (and (data~appl-p formula)
						      (keim~equal (data~appl-function formula)
								  (data~schema-range
								   (env~lookup-object 'closed-under
										      (pds~environment omega*current-proof-plan)))))))
					   goal-tasks)))
    (mapcar #'(lambda (clgt)
		(list (cons goaltask clgt)))
	    closed-goal-tasks)))

(defun notmetaor-p (goaltask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (notmetaor-goal-tasks (remove-if #'(lambda (task)
					      (let* ((node (agenda~task-node task))
						     (formula (node~formula node)))
						(crihelp=metaor-formula-p formula)))
					  goal-tasks)))
    (mapcar #'(lambda (nmog)
		(list (cons goaltask nmog)))
	    notmetaor-goal-tasks)))

(defun information-for-inst-task-from-equality-p (insttask params) 
  (let* ((tasks cri*current-tasks)  
	 (inst-tasks (remove-if-not #'(lambda (task)
					(and (agenda~inst-task-p task)
					     (find pplan*roc-state-description (agenda~task-rocs task))))
				    tasks))
	 (goal-tasks (remove-if-not #'(lambda (task)
					(and (agenda~goal-or-goal-schema-task-p task)
					     (find pplan*roc-state-description (agenda~task-rocs task))))
				    tasks))
	 (start-task (roc~start-task pplan*roc-state-description)))
    
    (apply #'append (mapcar #'(lambda (inst-task)
				(let* ((param-to-inst-task (crihelp=param-to-inst-task-from-equ inst-task start-task goal-tasks)))
				  (if (and param-to-inst-task
					   (null (already-tried-p inst-task param-to-inst-task)))
				      (list (list (cons insttask inst-task)
						  (cons params (list param-to-inst-task))))
				    nil)))
			    inst-tasks))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REDUCETOSPECIAL                                                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application Condition of ReduceToSpecial Strategy
(defun reducetospecial-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node))
	     (env (th~env 'zmz)))  ;;(pds~environment omega*current-proof-plan)))
	
	(if (or (and (data~appl-p formula)
		     (or (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'closed-under env)))
			 ;;(keim~equal (data~appl-function formula)
			 ;;	     (data~schema-range (env~lookup-object 'unit env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'divisors-exist env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'inverse-exist env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'associative env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'commutative env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'distributive env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'isomorphic env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'homomorphism env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'surjective env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'injective env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'module env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'submodule env)))
			 (keim~equal (data~appl-function formula)
				     (data~schema-range (env~lookup-object 'prime-submodule env)))
			 ))
		(and (logic~negation-p formula)
		     (data~appl-p (first (data~appl-arguments formula)))
		     (or (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'closed-under env)))
			 ;;(keim~equal (data~appl-function (first (data~appl-arguments formula)))
			 ;;	     (data~schema-range (env~lookup-object 'unit env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'divisors-exist env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'inverse-exist env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'associative env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'commutative env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'distributive env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'isomorphic env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'module env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'submodule env)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range (env~lookup-object 'prime-submodule env)))
			 ))
		(and (logic~existential-quantification-p formula)
		     (data~appl-p (logic~quantification-scope formula))
		     (keim~equal (data~appl-function (logic~quantification-scope formula))
				 (data~schema-range (env~lookup-object 'unit env))))
		(and (logic~negation-p formula)
		     (logic~existential-quantification-p (first (data~appl-arguments formula)))
		     (data~appl-p (logic~quantification-scope (first (data~appl-arguments formula))))
		     (keim~equal (data~appl-function (logic~quantification-scope (first (data~appl-arguments formula))))
				 (data~schema-range (env~lookup-object 'unit env)))))
	    't
	  nil))
    nil))

(strat~define-strategy-ks
 (name ReduceToSpecial)
 (refinement-algorithm PPlanner)
 (condition reducetospecial-p)
 (methods (MAssertion-m-b
	   ReductionClosed-m-b
	   InResclass-m-b
	   Int-m-b
	   SubsetResclass-m-b
	   ;;IncludeTheorems-m-b -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
	   WEAKENALPHA-M-A 
	   ANDI-m-b SimplifyOrder-m-b
	   reflexu-m-b NotReflex-m-b
	   ))
 (normalization-methods nil)
 (restriction-methods (Int-m-b
		       SubsetResclass-m-b
		       InResclass-m-b
		       ANDI-m-b
		       reflexu-m-b
		       NotReflex-m-b
		       SimplifyOrder-m-b
		       WEAKENALPHA-M-A 
		       ))
 (control-rules (;;redspec-include-theorems -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
		 redspec-standard-select
		 redspec-reject-meta-in-resclass-set
		 redspec-remove-order-theorem1-if-no-hint
		 redspec-remove-order-theorem2-if-no-hint
		 redspec-remove-submodule-theorem-if-no-submodule
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is solved by reducing it with theorem applications to a special case.")
 (print "Strategy-KS ReduceToSpecial: Offer to prove task ~A by reduce it with theorem applications to sepcial cases"))


;;;;;;;;;;;;;;;;;;;;;;;; CRULES OF REDUCETOSPECIAL


(cri~def-control-rule redspec-reject-meta-in-resclass-set
		      (kind tasks)
		      (if (task-is-not-meta-in-resclass-set "task"))
		      (then
		       (prefer ("task"))))

;; -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
;;(cri~def-control-rule redspec-include-theorems
;;		      (kind methods)
;;		      (if (goal-is-start-task))
;;		      (then
;;		       (select (IncludeTheorems-m-b))))

(cri~def-control-rule redspec-standard-select
		      (kind methods)
		      (if (theorem-of-thy-p "theorem"))
		      (then
		       (select (InResclass-m-b
				Int-m-b				
				SubsetResclass-m-b
				ReductionClosed-m-b
				(MAssertion-m-b () ("theorem"))))))


;; Order theorems are applied only if a hint is available, otherwise the whole
;; thing becomes to complex
;; The submodule theorem is applied only to submodule goals (and not in other weired directions!)

(cri~def-control-rule  redspec-remove-submodule-theorem-if-no-submodule
		      (kind methods)
		      (if (and (not (goal-matches ("goal" (submodule "set1" "set2" "op2" "set3" "op3" "op4" "op5"))))
			       (find-submodule-property-theorem "theorem")))
		      (then
		       (reject ((MAssertion-m-b () ("theorem"))))))

(cri~def-control-rule redspec-remove-order-theorem1-if-no-hint
		      (kind methods)
		      (if (and (and (goal-matches ("goal" (not (isomorphic "set1" "op1" "set2" "op2"))))
				    (no-order-hint "set1" "op1" "set2" "op2"))
			       (find-order-theorem1 "theorem")))
		      (then
		       (reject ((MAssertion-m-b () ("theorem"))))))

(cri~def-control-rule redspec-remove-order-theorem2-if-no-hint
		      (kind methods)
		      (if (and (and (goal-matches ("goal" (not (isomorphic "set1" "op1" "set2" "op2"))))
				    (no-order-hint "set2" "op2" "set1" "op1"))
			       (find-order-theorem2 "theorem")))
		      (then
		       (reject ((MAssertion-m-b () ("theorem"))))))


(defun find-submodule-property-theorem (theorem)
  (let* ((module-th (th~find-theory 'module)))
    (if module-th
	(let* ((theorems (th~theorems module-th))
	       (submodule-th (first (remove-if-not #'(lambda (theorem)
						       (string-equal (keim~name theorem) 'submodule-property))
						   theorems))))
	  (list (list (cons theorem submodule-th))))
      nil)))
    

(defun find-order-theorem1 (theorem)
  (let* ((current-th (prob~proof-theory omega*current-proof-plan))
	 (theorems (th~theorems current-th))
	 (order-th1 (first (remove-if-not #'(lambda (theorem)
					      (string-equal (keim~name theorem) 'not-isomorphic-order-of-element1))
					  theorems))))
    (list (list (cons theorem order-th1)))))

(defun find-order-theorem2 (theorem)
  (let* ((current-th (prob~proof-theory omega*current-proof-plan))
	 (theorems (th~theorems current-th))
	 (order-th2 (first (remove-if-not #'(lambda (theorem)
					      (string-equal (keim~name theorem) 'not-isomorphic-order-of-element2))
					  theorems))))
    (list (list (cons theorem order-th2)))))

(defun no-order-hint (set1 op1 set2 op2)
  (let* ((tables (crihelp=multiplication-tables-from-sets-and-ops set1 op1 set2 op2)))
    (multiple-value-bind
	(success pair)
	(crihelp=find-order-pair tables)
      (if success
	  nil
	(list (list (cons T T)))))))


;;(defun theorem-of-thy-p (theorem)
;;  (let* ((current-th (prob~proof-theory omega*current-proof-plan))
;;	 (theorems (th~theorems current-th)))
;;    (mapcar #'(lambda (th)
;;		(list (cons theorem th)))
;;	    theorems)))

;; take explicitly the theorems from ZMZ and MODULE
(defun theorem-of-thy-p (theorem)
  (let* ((zmz-th (th~find-theory 'zmz))
	 (module-th (th~find-theory 'module))
	 (theorems (append (if zmz-th
			       (th~theorems zmz-th)
			     nil)
			   (if module-th
			       (th~theorems module-th)
			     nil))))
    (mapcar #'(lambda (th)
		(list (cons theorem th)))
	    theorems)))


;; -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
;;(defun goal-is-start-task ()
;;  (if (eq cri*current-task (roc~start-task pplan*roc-state-description))
;;      (list (list (cons T T)))
;;    nil))

(defun task-is-not-meta-in-resclass-set (task)
  (if (stringp task)

      ;; -> Task not bound so far
      (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	     (meta-in-resclass-set-tasks
	      (remove-if-not #'(lambda (gota)
				 (let* ((node (agenda~task-node gota)))
				   (null (crihelp=meta-in-resclass-set-p node))))
			     goal-tasks)))
	(mapcar #'(lambda (taski)
		    (list (cons task taski)))
		meta-in-resclass-set-tasks))
    
    ;; -> Task already bound
    (let* ((node (agenda~task-node task)))
      (if (null (crihelp=meta-in-resclass-set-p node))
	  (list (list (cons T T)))
	nil))))
	  
(defun crihelp=meta-in-resclass-set-p (node)
  (if (null (pdsn~schematic-p node))
      nil
    (progn
      (when (null (pdsn~up-to-date node))
	(setf (pdsn~current-formula node)
	      (beta~normalize (subst~apply (pds~cstrpool-binding (pds~constraint-pool omega*current-proof-plan))
					   (node~formula node))))
	(setf (pdsn~up-to-date node) 't))
      (let* ((formula (pdsn~current-formula node))
	     (env (pds~environment omega*current-proof-plan)))
	(if (or (and (data~appl-p formula)
		     (keim~equal (data~appl-function formula)
				 (env~lookup-object 'resclass-set env))
		     (= (length (data~appl-arguments formula)) 2)
		     (meta~p (second (data~appl-arguments formula))))
		(crihelp=disjunction-of-mv=-p formula))
	    't
	  nil)))))

(defun crihelp=disjunction-of-mv=-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (do* ((rest-formulas (list formula))
	  (mv nil)
	  (flag t))
	((or (null rest-formulas)
	     (null flag))
	 flag)
      (let* ((head-formula (first rest-formulas)))
	(cond ((logic~disjunction-p head-formula)
	       (setf rest-formulas (append (rest rest-formulas) (data~appl-arguments head-formula))))
	      ((and (data~appl-p head-formula)
		    (keim~equal (data~appl-function head-formula)
				(data~schema-range (env~lookup-object '= env))))
	       (let* ((arg1 (first (data~appl-arguments head-formula)))
		      (arg2 (second (data~appl-arguments head-formula))))
		 (if (and (crihelp=resclass-item-p arg1)
			  (meta~p arg2))
		     (progn
		       (when (null mv)
			 (setf mv arg2))
		       (when (null (eq mv arg2))
			 (setf flag nil)))
		   (progn
		     (when (null mv)
		       (setf mv arg1))
		     (when (null (eq mv arg1))
		       (setf flag nil))))
		 (setf rest-formulas (rest rest-formulas))))
	      (t
	       (setf flag nil)))))))


(defun crihelp=resclass-item-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (and (data~appl-p formula)
	 (keim~equal (data~appl-function formula)
		     (env~lookup-object 'resclass env))
	 (= (length (data~appl-arguments formula)) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTINJNOTISO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Diese Strategie ist nur auf non-isomorphic Probleme anwendbar. Dabei versucht sie einen
;; Wiederspruchsbeweis zu fuehren: Aus der Annahme ein Isomorphismus H laege vor, wird ein
;; Wiederspruch konstruiert zuer Injektivitaet.
;;

;; Application Condition of NotInjNotIso Strategy

(defun notinjnotiso-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node))
	     (env (th~env 'zmz)))  ;;(pds~environment omega*current-proof-plan)))
	(if (and (logic~negation-p formula)
		 (data~appl-p (first (data~appl-arguments formula)))
		 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
			     (data~schema-range (env~lookup-object 'isomorphic env))))
	    't
	  nil))
    nil))

;;
;; Note by Ameier:
;; General-Solve-Equation-Mod-m-b is an even more general method than Solve-Equation-Mod-m-b
;; I commented it out for the Begehung since it is much harder to explain ...
;;
;; Yes, and directly after the Begehung I added it again and removed Solve-Equation-Mod-m-b ...
;;

(strat~define-strategy-ks
 (name NotInjNotIso)
 (refinement-algorithm PPlanner)
 (condition notinjnotiso-p)
 (methods (NotInjNotIso-m-b NotIsoSym-m-b
			    InsertHomEqus-m-f
	   notresclreflex-m-b reflexu-m-b
	   andi-m-b OrIR-m-b OrIL-m-b
	   FiniteSetAsDisj-m-b ExistsI-Sort-m-b
	   ;;Solve-Equation-Mod-m-b
	   General-Solve-Equation-Mod-m-b
	   =subst-m-b
	   ))
 (normalization-methods (InsertHomEqus-m-f))
 (restriction-methods (notresclreflex-m-b
		       ;;reflexu-m-b
		       FiniteSetAsDisj-m-b
		       andi-m-b
		       ;;Solve-Equation-Mod-m-b
		       General-Solve-Equation-Mod-m-b
		       ))
 (control-rules (;;zmz-apply-equation
		 zmz-remove-notisosym-double
		 zmz-apply-equation-II
		 zmz-prefer-unequations
		 zmz-prefer-metaors
		 zmz-oril-select
		 zmz-orir-select
		 zmz-interrupt-if-insttaskII
		 zmz-interrupt-if-time-exceeded
		 zmz-backtrack-if-circle
		 )) 
 (loop-detection nil)
 (randomization-rules (zmz-randomize-NotIsoSym
		       zmz-randomize-OrI
		       zmz-randomize-=subst-smallest-reduce
		       zmz-randomize-=subst-smallest-equal
		       zmz-randomize-=subst-smallest-add1
		       zmz-randomize-=subst-smallest-add2
		       zmz-randomize-=subst-not-smallest))
 (termination-check no-further-goal-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is solved by showing that no homomorphism is injective.")
 (print "Strategy-KS NotInjNotIso: Offer to proof task ~A by trying to show that no homomorphism is injectiv"))

;; Control-Rules of NOTINJNOTISO

(cri~def-control-rule zmz-remove-notisosym-double
		      (kind methods)
		      (if (current-goal-already-notisosym-p))
		      (then
		       (reject (NotIsoSym-m-b))))

(cri~def-control-rule zmz-interrupt-if-insttaskII
		      (kind strategy-interruption)
		      (if (information-for-inst-taskII-p "insttask" "params"))
		      (then
		       (insert ((InstFromParam "insttask" "params")))))

(cri~def-control-rule zmz-interrupt-if-time-exceeded
		      (kind strategy-interruption)
		      (if (notinjnotiso-time-exceeded-p "goal"))
		      (then
		       (insert ((backtrack-last-strategy-to-task "goal")))))
		      
(cri~def-control-rule zmz-prefer-unequations
		      (kind tasks)
		      (if (unequation-p "goal"))
		      (then
		       (prefer ("goal"))))

;;(cri~def-control-rule zmz-apply-equation
;;		      (kind tasks)
;;		      (if (suitable-assump-equation-for-position "goal" "assump" "pos"))
;;		      (then
;;		       (prefer (("goal" =subst-m-b ("assump") ("pos"))))))

(cri~def-control-rule zmz-apply-equation-II
		      (kind tasks)
		      (if (suitable-assump-equation-for-position-II "goal" "assump" "pos"))
		      (then
		       (prefer (("goal" =subst-m-b ("assump") ("pos"))))))

(cri~def-control-rule zmz-backtrack-if-circle
		      (kind strategy-interruption)
		      (if (equation-goal-with-circle "goal"))
		      (then
		       (insert ((backtrack-step-to-task "goal")))))

(defun current-goal-already-notisosym-p ()
  (let* ((task cri*current-task)
	 (node (agenda~task-node task))
	 (just (node~justification node))
	 (other-reasons (pdsj~other-reasons just)))
    (if (= (length other-reasons) 1)
	(let* ((pred-just (pdsc~an-just (first other-reasons)))
	       (method (just~method pred-just)))
	  ;;(format t "~%PRED JUST: ~A" pred-just)
	  (if (or (string-equal (keim~name method) 'notisosym-m)
		  (string-equal (keim~name method) 'notisosym-m-b))
	      't
	    nil))
      nil)))

(defun information-for-inst-taskII-p (insttask params) 
  (let* ((tasks cri*current-tasks)  ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
	 (start-task (roc~start-task pplan*roc-state-description)))
    
    (apply #'append (mapcar #'(lambda (inst-task)
				(let* ((param-to-inst-task (crihelp=param-to-inst-taskII inst-task start-task)))
				  (if (and param-to-inst-task
					   (null (already-tried-p inst-task param-to-inst-task)))
				      (list (list (cons insttask inst-task)
						  (cons params (list param-to-inst-task))))
				    nil)))
			    inst-tasks))))

(defun notinjnotiso-time-exceeded-p (goal)
  (let* ((start-task (roc~start-task pplan*roc-state-description))
	 (largest-mod-factor (crihelp=largest-mod-factor (node~formula (agenda~task-node start-task))))
	 (current-task (first (remove-if-not #'(lambda (task)
						 (and (agenda~goal-or-goal-schema-task-p task)
						      (find pplan*roc-state-description (agenda~task-rocs task))))
					     (agenda~all-tasks (pds~agenda omega*current-proof-plan))))))
    (if (null current-task)
	nil
      (let* ((run-time (roc~pplanner-run-time pplan*roc-state-description))
	     (time-exceeded? (cond ((= largest-mod-factor 2)
				    (if (> run-time 40)
					't
				      nil))
				   ((= largest-mod-factor 3)
				    (if (> run-time 55)
					't
				      nil))
				   ((= largest-mod-factor 4)
				    (if (> run-time 75)
					't
				      nil))
				   ((= largest-mod-factor 5)
				    (if (> run-time 110) 
					't
				      nil))
				   ((= largest-mod-factor 6)
				    (if (> run-time 140)
					't
				      nil))
				   ((> largest-mod-factor 6) ;; as long as we do not have any other results for factors larger than 6, we use this setting
				    (if (> run-time 140)
					't
				      nil)))))
	(if (null time-exceeded?)
	    nil
	  (list (list (cons goal current-task))))))))
    
(defun equation-goal-with-circle (goal)
  ;; Diese Funktion versucht herauszufinden, ob wir durch unsere Operationen einen Zirkel Schluss hervorgebracht haben
  ;; Die Funktion such nach einem Ziel, das eine equation ist. Nun wird daraus die ganze Kette von Schritten berechnet
  ;; die vor diesem Ziel lagen, die alle mit =subst-m geschlossen sind.
  ;; Wenn unser neues Ziel asu eienr =subst auf der linken Seite eines frueheren Ziels entstabnd, so wird die
  ;; linke Seite unserer Ziels mit jeder linken Seite der Kette von Vorgehern verglichen. Umgekehrt passiert das gleiche
  ;; wenn wenn unser Ziel durch einen =subst Schritt rechts entstabnd.
  ;; Wenn wir in der Kette vornedran einen Matcher finden, so wird das goalzurueckgegeben.
  (if (null 't)  ;; At this place the backtrack-probability-was-checked in the experiments (rand~flip-coin rand*backtrack-probability))
      nil
    (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	   (equation-task (first (remove-if-not #'(lambda (task)
						    (and (agenda~goal-or-goal-schema-task-p task)
							 (equation-p (node~formula (agenda~task-node task)))))
						all-tasks))))
      (if (null equation-task)
	  nil
	(let* ((node (agenda~task-node equation-task))
	       (chain (crihelp=compute-chain node)))
	  (if (null chain)
	      nil
	    (let* ((last-step-pos (first (pdsj~parameters (node~justification (first chain)))))
		   (side (if (= (pos~first last-step-pos) 1)
			     'left
			   'right))
		   (goal-side (if (string-equal side 'left)
				  (first (data~appl-arguments (crihelp=update-formula! node)))
				(second (data~appl-arguments (crihelp=update-formula! node)))))
		   (chain-sidesa (mapcar #'(lambda (chain-node)
					     (if (string-equal side 'left)
						 (first (data~appl-arguments (crihelp=update-formula! chain-node)))
					       (second (data~appl-arguments (crihelp=update-formula! chain-node)))))
					 chain))
		   (chain-sides (remove-duplicates chain-sidesa :test #'keim~equal)))
	      (multiple-value-bind
		  (h class-factor)
		  (methhelp=find-homomorphism (pdsn~hyps node))
		
		(let* ((matching (do* ((rest-sides chain-sides (rest rest-sides))
				       (flag nil))
				     ((or flag
					  (null rest-sides))
				      flag)
				   (let* ((head-side (first rest-sides))
					  (result (crihelp=matching-with-maple goal-side head-side h class-factor)))
				     (setf flag result)))))
		  (if matching
		      (list (list (cons goal equation-task)))
		    nil))))))))))
  	 
(defun unequation-p (task)
  (cond ((stringp task)
	 ;; task is not bound yet
	 (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
		(unequation-tasks (remove-if-not #'(lambda (task)
						     (let* ((node (agenda~task-node task))
							    (formula (node~formula node)))
						       (if (and (logic~negation-p formula)
								(equation-p (first (data~appl-arguments formula))))
							   't
							 nil)))
						 goal-tasks)))
	   (mapcar #'(lambda (ueqt)
		       (list (cons task ueqt)))
		   unequation-tasks)))
	(t
	 ;; task is already bound
	 (let* ((node (agenda~task-node task))
		(formula (node~formula node)))
	   (if (and (logic~negation-p formula)
		    (equation-p (first (data~appl-arguments formula))))
	       (list (list (cons T T)))
	     nil)))))

(defun equation-p (term)
  (if (and (data~appl-p term)
	   (keim~equal (data~appl-function term)
		       (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan)))))
      't
    nil))




;;(defun suitable-assump-equation-for-position (goal assumption position)
;;  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
;;	 (relevant-tasks (remove-if-not #'(lambda (task)
;;					    (let* ((node (agenda~task-node task))
;;						   (formula (node~formula node))
;;						   (current-formula (crihelp=update-formula! node)))
;;					      (if (and (equation-p formula)
;;						       (null (data~positions current-formula #'meta~p)))
;;						  't
;;						nil)))
;;					goal-tasks))
;;	 (equations-in-supports (mapcar #'crihelp=equation-supports-of-task relevant-tasks))
;;	 (merging-positions-left (mapcar #'(lambda (task equation-supps)
;;					     (mapcar #'(lambda (equation-supp)
;;							 (crihelp=positions-merging-left task equation-supp))
;;						     equation-supps))
;;					 relevant-tasks equations-in-supports))
;;	 (all-tupels
;;	  (apply #'append (mapcar #'(lambda (task equations-in-supports merging-positions-left)
;;				      (apply #'append (mapcar #'(lambda (equation poses-left)
;;								  (mapcar #'(lambda (pos)
;;									      (list (cons goal task)
;;										    (cons assumption equation)
;;										    (cons position pos)))
;;									  poses-left))
;;							      equations-in-supports merging-positions-left)))
;;				  relevant-tasks equations-in-supports merging-positions-left))))
;;    (crihelp=order-tupels all-tupels)))

(defun suitable-assump-equation-for-position-II (goal assumption position)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (relevant-tasks (remove-if-not #'(lambda (task)
					    (let* ((node (agenda~task-node task))
						   (formula (node~formula node))
						   (current-formula (crihelp=update-formula! node)))
					      (if (and (equation-p formula)
						       (null (data~positions current-formula #'meta~p)))
						  't
						nil)))
					goal-tasks))
	 (equations-in-supports (mapcar #'crihelp=equation-supports-of-task relevant-tasks))
	 (merging-positions-left (mapcar #'(lambda (task equation-supps)
					     (mapcar #'(lambda (equation-supp)
							 (crihelp=positions-merging-left task equation-supp))
						     equation-supps))
					 relevant-tasks equations-in-supports))
	 (all-tupels
	  (apply #'append (mapcar #'(lambda (task equations-in-supports merging-positions-left)
				      (apply #'append (mapcar #'(lambda (equation poses-left)
								  (mapcar #'(lambda (pos)
									      (list (cons goal task)
										    (cons assumption equation)
										    (cons position pos)))
									  poses-left))
							      equations-in-supports merging-positions-left)))
				  relevant-tasks equations-in-supports merging-positions-left))))
    (crihelp=order-tupels-II all-tupels)))

;; Randomization-Rules of NOTINJNOTISO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RANDOMIZE NOT-ISO-SYM

(rand~def-randomize-rule zmz-randomize-NotIsoSym
			 (kind methods)
			 (test-function sym-and-solve-p)
			 (set-weight-function set-1)
			 (action randomize-only))

(defun sym-and-solve-p (element)
  (let* ((check-thing (if (listp element)
			  (first element)
			element))
	 (check-symbol (cond ((symbolp check-thing)
			      check-thing)
			     ((meth~p check-thing)
			      (keim~name check-thing))
			     (t
			      (omega~error "~%Unawaited Situation in Function ori-method-p")))))
    (if (or (string-equal check-symbol 'NotInjNotIso-m-b)
	    (string-equal check-symbol 'NotIsoSym-m-b))
	't
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RANDOMIZE ORI

(rand~def-randomize-rule zmz-randomize-OrI
			 (kind methods)
			 (test-function ori-method-p)
			 (set-weight-function set-1)
			 (action randomize-only))

(defun set-1 (element)
  element)

(defun ori-method-p (element)
  (let* ((check-thing (if (listp element)
			  (first element)
			element))
	 (check-symbol (cond ((symbolp check-thing)
			      check-thing)
			     ((meth~p check-thing)
			      (keim~name check-thing))
			     (t
			      (omega~error "~%Unawaited Situation in Function ori-method-p")))))
    (if (or (string-equal check-symbol 'OrIL-m-b)
	    (string-equal check-symbol 'OrIR-m-b))
	't
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Randomize =subst Rules


(rand~def-randomize-rule zmz-randomize-=subst-smallest-reduce
			 (kind methods)
			 (test-function =subst-method-p-and-smallest-reduce)
			 (set-weight-function set-1)
			 (action randomize-only))

(rand~def-randomize-rule zmz-randomize-=subst-smallest-equal
			 (kind methods)
			 (test-function =subst-method-p-and-smallest-equal)
			 (set-weight-function set-1)
			 (action randomize-only))

(rand~def-randomize-rule zmz-randomize-=subst-smallest-add1
			 (kind methods)
			 (test-function =subst-method-p-and-smallest-add1)
			 (set-weight-function set-1)
			 (action randomize-only))

(rand~def-randomize-rule zmz-randomize-=subst-smallest-add2
			 (kind methods)
			 (test-function =subst-method-p-and-smallest-add2)
			 (set-weight-function set-1)
			 (action randomize-only))

(rand~def-randomize-rule zmz-randomize-=subst-not-smallest
			 (kind methods)
			 (test-function =subst-method-p-and-not-smallest)
			 (set-weight-function set-1)
			 (action randomize-only))

(defun =subst-method-p-and-smallest-reduce (element)
  (if (and (=subst-method-p element)
	   (listp element)
	   (= (length element) 3)
	   (randhelp=smallest-p element)
	   (= (randhelp=effect-of-tupel element) -1))
      't
    nil))

(defun =subst-method-p-and-smallest-equal (element)
  (if (and (=subst-method-p element)
	   (listp element)
	   (= (length element) 3)
	   (randhelp=smallest-p element)
	   (= (randhelp=effect-of-tupel element) 0))
      't
    nil))

(defun =subst-method-p-and-smallest-add1 (element)
  (if (and (=subst-method-p element)
	   (listp element)
	   (= (length element) 3)
	   (randhelp=smallest-p element)
	   (= (randhelp=effect-of-tupel element) 1))
      't
    nil))

(defun =subst-method-p-and-smallest-add2 (element)
  (if (and (=subst-method-p element)
	   (listp element)
	   (= (length element) 3)
	   (randhelp=smallest-p element)
	   (= (randhelp=effect-of-tupel element) 2))
      't
    nil))

(defun =subst-method-p-and-not-smallest (element)
  (if (and (=subst-method-p element)
	   (listp element)
	   (= (length element) 3)
	   (null (randhelp=smallest-p element)))
      't
    nil))


(defun randhelp=effect-of-tupel (element)
  (let* ((goal-node (agenda~task-node rand*current-task))
	 (assumption (first (second element)))
	 (position (first (third element)))
	 (goal-formula (crihelp=update-formula! goal-node))
	 (ass-formula (crihelp=update-formula! assumption))
	 (h (data~appl-function (first (data~appl-arguments ass-formula))))
	 (goal-h-expressions (crihelp=h-expressions-in-term goal-formula h)))
    (crihelp=effect-of-equation goal-h-expressions ass-formula h position)))

(defun randhelp=smallest-p (element)
  (let* ((goal-node (agenda~task-node rand*current-task))
	 (assumption (first (second element)))
	 (position (first (third element)))
	 (goal-formula (crihelp=update-formula! goal-node))
	 (ass-formula (crihelp=update-formula! assumption))
	 (h (data~appl-function (first (data~appl-arguments ass-formula))))
	 (goal-h-expressions (crihelp=h-expressions-in-term goal-formula h))
	 (left-side-of-equation (first (data~appl-arguments ass-formula)))
	 (smallest-occurrences (crihelp=check-for-smallest-number-of-occurrences goal-h-expressions)))
    (if (find left-side-of-equation smallest-occurrences :test #'keim~equal)
	't
      nil)))

(defun =subst-method-p (element)
  (let* ((check-thing (if (listp element)
			  (first element)
			element))
	 (check-symbol (cond ((symbolp check-thing)
			      check-thing)
			     ((meth~p check-thing)
			      (keim~name check-thing))
			     (t
			      (omega~error "~%Unawaited Situation in Function ori-method-p")))))
    (if (string-equal check-symbol '=subst-m-b)
	't
      nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISCRIMINANT STRATEGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stop-if-atp-success-p ()
  (let* ((last-step (pds~last-plan-step omega*current-proof-plan)))
    (if last-step
	(let* ((just (pdsc~an-just last-step))
	       (method (just~method just)))
	  (if (or (string-equal (keim~name method) 'call-tramp-m-b)
		  (string-equal (keim~name method) 'call-tramp-m)
		  (string-equal (keim~name method) 'call-otter-m-b)
		  (string-equal (keim~name method) 'call-otter-m)
		  (string-equal (keim~name method) 'call-bliksem-m-b)
		  (string-equal (keim~name method) 'call-bliksem-m))
	      't
	    nil))
      nil)))

(strat~define-strategy-ks
 (name DistProp)
 (refinement-algorithm PPlanner)
 (condition notinjnotiso-p)
 (methods (Dist-Prop-m-b
	   Call-tramp-m-b
	   DefnI*-m-b 
	   ))
 (normalization-methods ())
 (restriction-methods ())
 (control-rules (Expand-Forall-Sort-Def-In-Conclusion
		 Expand-Exists-Sort-Def-In-Conclusion
		 Prefer-ATP-Conc
		 Interrupt-After-Dist-Prop-Appl-And-Ask-For-Prop))
 (loop-detection nil)
 (randomization-rules ())
 (termination-check stop-if-atp-success-p)
 (selection waterfall)
 (print "Strategy-KS DistProp: Offer to proof task ~A by trying to show that there is a property that holds for the one residue class, but not for the other one."))

(cri~def-control-rule Prefer-ATP-Conc
		      (kind tasks)
		      (if (atp-concs "Task"))
		      (then
		       (prefer ("Task"))))

(defun atp-concs (task)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (atp-tasks (remove-if-not #'(lambda (task)
				       (let* ((conclusion (pdsn~current-formula (agenda~task-node task))))
					 (if (and (data~appl-p conclusion)
						  (term~constant-p (data~appl-function conclusion))
						  (string-equal (keim~name (data~appl-function conclusion)) 'forall))
					     't
					   nil)))
				   goal-tasks)))
    (mapcar #'(lambda (atp-task)
		(list (cons task atp-task)))
	    atp-tasks)))

(cri~def-control-rule Expand-Forall-Sort-Def-In-Conclusion
		      (kind methods)
		      (if (and (conclusion-contains-forall-sort)
			       (get-term "term" forall-sort)))
		      (then
		       (select ((DefnI*-m-b () ("term"))))))

(defun conclusion-contains-forall-sort ()
  (let* ((conclusion (pdsn~current-formula (agenda~task-node cri*current-task))))
    (if (data~positions conclusion
			#'(lambda (sub)
			    (and (term~constant-p sub)
				 (keim~equal sub
					     (data~schema-range (env~lookup-object 'forall-sort
										   (pds~environment omega*current-proof-plan)))))))
	(list (list (cons T T)))
      nil)))



(cri~def-control-rule Expand-Exists-Sort-Def-In-Conclusion
		      (kind methods)
		      (if (and (conclusion-contains-exists-sort)
			       (get-term "term" exists-sort)))
		      (then
		       (select ((DefnI*-m-b () ("term"))))))

(defun get-term (term name)
  (let* ((te (post~read-object name (pds~environment omega*current-proof-plan) :existing-term)))
    (if t
	(list (list (cons term te)))
      nil)))

(defun conclusion-contains-exists-sort ()
  (let* ((conclusion (pdsn~current-formula (agenda~task-node cri*current-task))))
    (if (data~positions conclusion
			#'(lambda (sub)
			    (and (term~constant-p sub)
				 (keim~equal sub
					     (data~schema-range (env~lookup-object 'exists-sort
										   (pds~environment omega*current-proof-plan)))))))
	(list (list (cons T T)))
      nil)))

(cri~def-control-rule Interrupt-After-Dist-Prop-Appl-And-Ask-For-Prop
		      (kind strategy-interruption)
		      (if (and (and (last-plan-step "Last-Plan-Step")
				    (applied-method "Last-Plan-Step" Dist-Prop-m))
			       (mv-task-of "Last-Plan-Step" "MVTask"))) 
		      (then
		       (insert ((InstDiscriminantWithHR "MVTask")))))


(defun last-plan-step (lps)
  (let* ((last-step (pds~last-plan-step omega*current-proof-plan)))
    (cond ((and (null (stringp lps))
		last-step)
	   (if (eq lps last-step)
	       't
	     nil))
	  ((and (stringp lps)
		last-step)
	   (list (list (cons lps last-step))))
	  (t
	   nil))))

(defun applied-method (plan-step method)
  (cond ((and (null (stringp plan-step))
	      (stringp method))
	 (list (list (cons method (just~method (pdsc~an-just plan-step))))))
	((and (null (stringp plan-step))
	      (null (stringp method)))
	 (let* ((applied-method (just~method (pdsc~an-just plan-step))))
	   (if (or (eq applied-method method)
		   (string-equal (keim~name applied-method) method))
	       (list (list (cons t t)))
	     nil)))
	(t
	 nil)))
		

(defun mv-task-of (plan-step mv-task)
  (cond ((and (null (stringp plan-step))
	      (stringp mv-task))
	 (let* ((tasks cri*current-tasks)                                 ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
		(inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
		(inst-tasks-of-plan-step (remove-if-not #'(lambda (inst-task)
							    (eq plan-step (agenda~inst-task-plan-step inst-task)))
							inst-tasks)))
	   (if inst-tasks-of-plan-step
	       (mapcar #'(lambda (inst-task)
			   (list (cons mv-task inst-task)))
		       inst-tasks-of-plan-step)
	     nil)))
	((and (null (stringp plan-step))
	      (null (stringp mv-task)))
	 (let* ((tasks cri*current-tasks)                                 ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
		(inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
		(inst-tasks-of-plan-step (remove-if-not #'(lambda (inst-task)
							    (eq plan-step (agenda~inst-task-plan-step inst-task)))
							inst-tasks)))
	   (if (find mv-task inst-tasks-of-plan-step :test #'eq)
	       (list (list (cons t t)))
	     nil)))
	(t
	 nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    Strategic Control!!         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cri~def-control-rule PREFER-BACKTRACK-CPOOL-FOR-NotInjNotIso
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is NotInjNotIso "strategy"))                  ;; Strategy war TRYANDERROR
			       (and (job-is BACKTRACK-CPOOL-STEP-AFTER-TASK "job1" "task")
				    ;; Es gibt Angebot von BACKTRACK-CPOOL-STEP-AFTER-TASK
				    (job-is BACKTRACK-STEP-TO-TASK "job2" "task")))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))

(cri~def-control-rule PREFER-REDUCETOSPECIAL-BEFORE-TRYANDERROR
		      (kind strategic)
		      (if (and (job-is REDUCETOSPECIAL "job1" "task")    ;; Es gibt Angebot von REDUCETOSPECIAL fuer Task 
			       (job-is TRYANDERROR "job2" "task")))      ;; Es gibt Angebot von TRYANDERROR fuer TASK
		      (then                                              ;; ->
		       (order-before (("job1" "job2")))))                ;; Preferiere REDUCETOSPECIAL Angebot before TRYANDERROR

(cri~def-control-rule PREFER-REDUCETOSPECIAL-BEFORE-NOTINJNOTISO
		      (kind strategic)
		      (if (and (job-is REDUCETOSPECIAL "job1" "task")    ;; Es gibt Angebot von REDUCETOSPECIAL fuer Task 
			       (job-is NOTINJNOTISO "job2" "task")))     ;; Es gibt Angebot von NOTINJNOTISO fuer TASK
		      (then                                              ;; ->
		       (order-before (("job1" "job2")))))                ;; Preferiere REDUCETOSPECIAL Angebot before NOTINJNOTISO

(cri~def-control-rule PREFER-NOTINJNOTISO-BEFORE-TRYANDERROR
		      (kind strategic)
		      (if (and (job-is NOTINJNOTISO "job1" "task")       ;; Es gibt Angebot von NOTINJNOTISO fuer Task 
			       (job-is TRYANDERROR "job2" "task")))      ;; Es gibt Angebot von TRYANDERROR fuer TASK
		      (then                                              ;; ->
		       (order-before (("job1" "job2")))))                ;; Preferiere NOTINJNOTISO Angebot before TRYANDERROR

(cri~def-control-rule PREFER-REDUCETOSPECIAL-BEFORE-EQUSOLVE
		      (kind strategic)
		      (if (and (job-is REDUCETOSPECIAL "job1" "task")    ;; Es gibt Angebot von REDUCETOSPECIAL fuer Task 
			       (job-is EQUSOLVE "job2" "task")))         ;; es gibt angebot von EQUSOLVE fuer TASK
		      (then                                              ;; ->
		       (order-before (("job1" "job2")))))                ;; Preferiere REDUCETOSPECIAL Angebot before EQUSOLVE

(cri~def-control-rule PREFER-EQUSOLVE-BEFORE-TRYANDERROR
		      (kind strategic)
		      (if (and (job-is EQUSOLVE "job1" "task")             ;; Es gibt Angebot von EQUSOLVE fuer Task 
			       (job-is TRYANDERROR "job2" "task")))        ;; Es gibt Angebot von TRYANDERROR fuer TASK
		      (then                                                ;; ->
		       (order-before (("job1" "job2")))))                               ;; Preferiere EQUSOLVE Angebot!

(cri~def-control-rule PREFER-BACKTRACK-CPOOL-FOR-TRYANDERROR
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is TRYANDERROR "strategy"))                  ;; Strategy war TRYANDERROR
			       (and (job-is BACKTRACK-CPOOL-STEP-AFTER-TASK "job1" "task")
				    ;; Es gibt Angebot von BACKTRACK-CPOOL-STEP-AFTER-TASK
				    (job-is BACKTRACK-STEP-TO-TASK "job2" "task")))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))                                           ;; Ziehe BACKTRACK-CPOOL-STEP-AFTER-TASK vor

(cri~def-control-rule PREFER-OTHER-STRATEGY-BEFORE-BACKTRACKING-IF-REDUCETOSPECIAL
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' on Task zurueck
				    (strategy-is REDUCETOSPECIAL "strategy")
				    ;; Last Strategy was REDUCETOSPECIAL
				    )
			       (or (job-is EQUSOLVE "job2" "task")
				   (job-is TRYANDERROR "job2" "task")
				   ;; Es gibt Angebote von TRYANDERROR oder EQUSOLVE auf TASK
				   )))
		      (then         ;; -> prefer job2
		       (prefer ("job2"))))
;; In opposite to the more general control rule  PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE which states
;; that backtracking should be done before something different is tried (the idea is to search first in one strategy,
;; then in the next) in the case of the REDUCETOSPECIAL it makes sence to prefer other job-offers before backtrcking
;; since it is the idea of REDUCETOSPECIAL to reduce a complex goal to simpler subgoals which may are not provable
;; by REDUCETOSPECIAL directly, but by other strategies

;; The same holds for Equsolve ...
(cri~def-control-rule PREFER-OTHER-STRATEGY-BEFORE-BACKTRACKING-IF-EQUSOLVE
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' on Task zurueck
				    (strategy-is EQUSOLVE "strategy")
				    ;; Last Strategy was EQUSOLVE
				    )
			       (or (job-is REDUCETOSPECIAL "job2" "task")
				   (job-is TRYANDERROR "job2" "task")
				   ;; Es gibt Angebote von REDUCETOSPECIAL oder EQUSOLVE auf TASK
				   )))
		      (then         ;; -> prefer job2
		       (prefer ("job2"))))


(cri~def-control-rule PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-REDUCETOSPECIAL-AND-INCLUDE-THEOREMS
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' on Task zurueck
				    (strategy-is REDUCETOSPECIAL "strategy")
				    ;; Last Strategy was REDUCETOSPECIAL
				    )
			       (and (task-created-by-method "task" IncludeTheorems-m)
				    (job-is BACKTRACK-LAST-STRATEGY-TO-TASK "job1" "task")  ;; Es gibt Angebot BACKTRACK-LAST-STRATEGY
				    )))
		      (then
		       (prefer ("job1"))))

(cri~def-control-rule INSERT-NOTINJNOTISO-IF-ALREADY-APPLIED-BUT-NOT-EXCEEDED
		      (kind strategic)
		      (if (notinjnotiso-already-applied-but-restarts-not-exceeded-p "task" "NOTINJNOTISO-JOB"))
		      (then
		       (insert ("NOTINJNOTISO-JOB"))))

(cri~def-control-rule PREFER-DistProp-BEFORE-ReduceToSpecial
		      (kind strategic)
		      (if (and (job-is DistProp "job1" "task")                 ;; Es gibt Angebot von DistProp fuer Task 
			       (job-is ReduceToSpecial "job2" "task")))        ;; Es gibt Angebot von ReduceToSpecial fuer TASK
		      (then                                                    ;; ->
		       (order-before (("job1" "job2")))))                      ;; Preferiere DistProp Angebot!

(cri~def-control-rule PREFER-DistProp-BEFORE-TryAndError
		      (kind strategic)
		      (if (and (job-is DistProp "job1" "task")                 ;; Es gibt Angebot von DistProp fuer Task 
			       (job-is TryAndError "job2" "task")))            ;; Es gibt Angebot von TryAndError fuer TASK
		      (then                                                    ;; ->
		       (order-before (("job1" "job2")))))                      ;; Preferiere DistProp Angebot!

(cri~def-control-rule PREFER-DistProp-BEFORE-NotInjNotIso
		      (kind strategic)
		      (if (and (job-is DistProp "job1" "task")                 ;; Es gibt Angebot von DistProp fuer Task 
			       (job-is NotInjNotIso "job2" "task")))           ;; Es gibt Angebot von NotInjNotIso fuer TASK
		      (then                                                    ;; ->
		       (order-before (("job1" "job2")))))                      ;; Preferiere DistProp Angebot!



;; metapreds fuer die CRULES

(defun notinjnotiso-already-applied-but-restarts-not-exceeded-p (task-str notinjnotiso-job-str)
  ;; checks whether the notinjnotiso strategy was already applied to the task.
  ;; if this is the case, it is checked how often notinjnotiso was already applied to the task, lets say a number of n tries
  ;; if n is smaller as the number of tries we allow for NOTINJNOTISO on the class coresponding to the problem (what are the
  ;; modulo factors of its residue class sets), then a new job-offer of notinjnotiso for the task is created and inserted
  ;; (by the control rule)

  (let* ((all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan)))))

    (apply #'append (mapcar #'(lambda (task)
				(let* ((job-offer (crihelp=check-task-for-notinjnotiso task)))
				  (if (null job-offer)
				      nil
				    (list (list (cons task-str task)
						(cons notinjnotiso-job-str job-offer))))))
			    all-goal-tasks))))

(defun crihelp=check-task-for-notinjnotiso (task)			    
  (let* ((notinjnotiso-strat (strat~find-strategy-ks 'notinjnotiso))
	 (already-applied-strat-and-param-pairs (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task)))))
	 (already-applied-notinjnotisos (remove-if-not #'(lambda (strat-and-param-pair)
							   (if (eq (first strat-and-param-pair) notinjnotiso-strat)
							       't
							     nil))
						       already-applied-strat-and-param-pairs))
	 (formula (node~formula (agenda~task-node task)))
	 (notiso-test (and (logic~negation-p formula)
			   (data~appl-p (first (data~appl-arguments formula)))
			   (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				       (data~schema-range (env~lookup-object 'isomorphic (pds~environment omega*current-proof-plan)))))))
    
    ;; (format t "~%Applied NOTINJNOTISO to Task ~A already ~A times" task (length already-applied-notinjnotisos))
    
    (if (and already-applied-notinjnotisos
	     notiso-test)
	(let* ((number-of-appls (length already-applied-notinjnotisos))
	       (largest-mod-fac (crihelp=largest-mod-factor (node~formula (agenda~task-node task))))
	       (new-job? (cond ((and (= largest-mod-fac 2)
				     (< number-of-appls 5))
				't)
			       ((and (= largest-mod-fac 3)
				     (< number-of-appls 20))
				't)
			       ((and (= largest-mod-fac 4)
				     (< number-of-appls 40))
				't)
			       ((and (= largest-mod-fac 5)
				     (< number-of-appls 50))
				't)
			       ((and (= largest-mod-fac 6)
				     (< number-of-appls 60))
				't)
			       ((and (> largest-mod-fac 6)  ;; as long as we have no results for mod-facs larger than 6, we take the setting for 6
				     (< number-of-appls 60))
				't)))
	       (new-job (if new-job?
			    (job~create-strategy-ks-offer notinjnotiso-strat task nil)
			  nil)))
	  new-job)
      nil)))      

(defun task-created-by-method (task method)
  (cond ((stringp task)
	 ;; task is unbound
	 nil)
	((agenda~goal-or-goal-schema-task-p task)
	 (let* ((node (agenda~task-node task))
		(just (node~justification node))
		(reason (first (pdsj~reasons just))))
	   (when reason
	     (let* ((reason-just (pdsc~an-just reason))
		    (method-name (keim~name (just~method reason-just))))
	       (when (string-equal method-name method)
		 (list (list (cons T T))))))))
	(t
	 nil)))

(defun strategy-is (signifer strategy)
  (declare (edited  "26-MAR-2000")
	   (authors Ameier)
	   (input   "A Symbol and a strategy.")
	   (effect  "None.")
	   (value   "If the strategy has the type strat+strategy-ks and has the symbol as name T, otherwise nil."))
  (if (and (strat~strategy-ks-p strategy)
	   (string-equal signifer (keim~name strategy)))
      (list (list (cons T T)))
    nil))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS                                                           ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf sod*current-strategies '(InstDiscriminantWithHR
			       DistProp
			       NotInjNotIso
			       ReduceToSpecial
			       EQUSOLVE
			       TRYANDERROR
			       INSTFROMPARAM
			       INSTFROMCAS
			       InstIsoByPolyFromCAS			       
			       BackTrack-To-Open
			       BackTrack-MV-Inst
			       BACKTRACK-STEP-TO-TASK
			       BACKTRACK-CPOOL-STEP-AFTER-TASK
			       BACKTRACK-LAST-STRATEGY-TO-TASK
			       InstInteractively
))
(setf sod*current-strategic-control-rules '(
					    PREFER-DEMAND-FULFILLING
					    PREFER-OFFERS-FROM-STORE
					    DEFER-OFFERS-FROM-STORE-IF-NOT-SATISFIED-DEMANDS
					    REJECT-ALREADY-APPLIED-STRATEGIES
					    INSERT-NOTINJNOTISO-IF-ALREADY-APPLIED-BUT-NOT-EXCEEDED
					    PREFER-EQUSOLVE-BEFORE-TRYANDERROR
					    PREFER-DistProp-BEFORE-ReduceToSpecial
					    PREFER-DistProp-BEFORE-TryAndError
					    PREFER-DistProp-BEFORE-NotInjNotIso
					    PREFER-REDUCETOSPECIAL-BEFORE-TRYANDERROR
					    PREFER-REDUCETOSPECIAL-BEFORE-EQUSOLVE
					    PREFER-REDUCETOSPECIAL-BEFORE-NOTINJNOTISO
					    PREFER-NOTINJNOTISO-BEFORE-TRYANDERROR
					    PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
					    PREFER-OTHER-STRATEGY-BEFORE-BACKTRACKING-IF-REDUCETOSPECIAL
					    PREFER-OTHER-STRATEGY-BEFORE-BACKTRACKING-IF-EQUSOLVE
					    PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-START-TASK
					    PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-REDUCETOSPECIAL-AND-INCLUDE-THEOREMS
					    PREFER-BACKTRACK-CPOOL-FOR-TRYANDERROR
					    PREFER-BACKTRACK-CPOOL-FOR-NotInjNotIso
					    ))


