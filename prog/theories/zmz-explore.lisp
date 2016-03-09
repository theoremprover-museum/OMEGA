;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(mod~defmod EXPL 
            :uses (agenda com data env keim logic node oc omega pds pdsj post prob rcl sod strat term th)
            :documentation "Exploration of residueclass sets"
            :exports (expl+table
                      expl+table
                      expl+table-entry
                      
                      expl~count-file-length
                      expl~create-table
                      expl~create-table-entry
                      expl~delete-result-file
                      expl~explore-znz
                      expl~explore-znz-command
                      expl~find-table
                      expl~generate-examples
                      expl~generate-operation
                      expl~generate-resclass
                      expl~generate-set
                      expl~set-with-operations
                      expl~table-entries
                      expl~table-entry-proof
                      expl~table-entry-strategy
                      expl~table-entry-theorem
                      expl~table-operator
                      expl~table-result
                      expl~table-set
                      expl~table-status
                      
                      expl*counter
                      expl*current-table
                      expl*example-files
                      expl*hashtable
                      expl*multiplication-table
                      expl*result-file
                      expl*testbed-dir))

;;; The following functions are internal in other modules and should not be used:
;;; (crihelp=convert-operation crihelp=decompose-cartproduct-sets-and-ops crihelp=number-set-to-resclass-set oc=prove-pre sod=make-strategic-justifications sod=system-work th=read-next-sexp th=stop-reading-sexp zmztac=class-factor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar expl*multiplication-table nil)

(defvar expl*counter 0)

(defvar expl*hashtable (make-hash-table :test #'equal))

(defvar expl*current-table nil)

(defvar expl*current-strategies
  '(NotInjNotIso
    ReduceToSpecial
    EQUSOLVE
    TRYANDERROR
    INSTFROMPARAM
    INSTFROMCAS
    BackTrack-To-Open
    BackTrack-MV-Inst
    BACKTRACK-STEP-TO-TASK
    BACKTRACK-CPOOL-STEP-AFTER-TASK
    BACKTRACK-LAST-STRATEGY-TO-TASK
    InstInteractively
    ))
  
(defvar expl*current-strategic-control-rules
  '(PREFER-DEMAND-FULFILLING
    PREFER-OFFERS-FROM-STORE
    REJECT-ALREADY-APPLIED-STRATEGIES
    INSERT-NOTINJNOTISO-IF-ALREADY-APPLIED-BUT-NOT-EXCEEDED
    PREFER-EQUSOLVE-BEFORE-TRYANDERROR
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (defclass expl+table (keim+name)
    ((set :initarg :set
	  :initform nil
	  :accessor expl~table-set)
     (operator :initarg :operator
	       :initform nil
	       :accessor expl~table-operator)
     (entries :initarg :entries
	      :initform nil
	      :accessor expl~table-entries)
     (status :initarg :status
	     :initform nil
	     :accessor expl~table-status)
     (result :initarg :result
	      :initform nil
	      :accessor expl~table-result
	      :documentation "The result of an exploration process, i.e. what type of algebra it is.")
     )))

  
(defun expl~create-table (name set operator entries)
  (make-instance 'expl+table
		 :name name
		 :set set
		 :operator operator
		 :entries entries))

(defmethod print-object ((table expl+table) stream)
  (format stream "~%Table for Expl-Problem ~A with ~%SET: ~A ~%OPERATOR: ~A and ~%Entries:"
	  (keim~name table)
	  (post~print (expl~table-set table) nil)
	  (post~print (expl~table-operator table) nil))
  (mapcar #'(lambda (entry)
	      (format stream "~%  Theorem: ~A~%  Proof-Status: ~A~%  Strategy: ~A~%"
		      (expl~table-entry-theorem entry)
		      (if (pds~proof-plan-p (expl~table-entry-proof entry))
			  "Proof"
			"Unknown")
		      (if (strat~strategy-ks-p (expl~table-entry-strategy entry))
			  (keim~name (expl~table-entry-strategy entry))
			"Unknown")))
	  (expl~table-entries table)))

(eval-when (load compile eval)
  (defclass expl+table-entry (keim+object)
    ((theorem :initarg :theorem
	      :initform nil
	      :accessor expl~table-entry-theorem)
     (proof :initarg :proof
	    :initform nil
	    :accessor expl~table-entry-proof)
     (strategy :initarg :strategy
	       :initform nil
	       :accessor expl~table-entry-strategy))))

(defun expl~create-table-entry (theorem proof strategy)
  (make-instance 'expl+table-entry
		 :theorem theorem
		 :proof proof
		 :strategy strategy))

(defmethod print-object ((entry expl+table-entry) stream)
  (format stream "~%Table-Entry with:")
  (format stream "~%  Theorem: ~A~%  Proof-Status: ~A~%  Strategy: ~A~%"
	  (expl~table-entry-theorem entry)
	  (if (pds~proof-plan-p (expl~table-entry-proof entry))
	      "Proof"
	    "Unknown")
	  (if (strat~strategy-ks-p (expl~table-entry-strategy entry))
	      (keim~name (expl~table-entry-strategy entry))
	    "Unknown")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Kleines Commando um Table zu momentanem Ziel zu bekommen!            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (com~defcommand table
;;               (frag-cats planning)
;;               (function expl=multi-table)
;;               (help "Prints the multiplication table of the residue class currently explored."))

;; (defun expl=multi-table ()
;;   (let* ((tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan))))
;;     (crihelp=compute-multiplication-table! (first tasks))
;;     (omega~output "~%~A~%" crihelp*multiplication-table)))


(defun expl=show-expl-table-default (name)
  (cond ((not (com~specified-arg-p name))
	 (if expl*current-table
	     (list (keim~name expl*current-table))
	   (list (oc~nil-argument))))
	(t (list name))))

(defun expl=show-table (table)
  (let ((table (expl~find-table table)))
    (expl=exploration-message table)
    (omega~message "~A" table)))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric expl~find-table (name)
  (:method ((name string))
	   (gethash (string-upcase name) expl*hashtable))
  (:method ((name symbol))
	   (gethash (symbol-name name) expl*hashtable)))

(defun expl~explore-znz-command (post-set post-op)
  (let* ((prob (car (th~theorems (th~require-completely 'zmz))))
	 (proof (or (car (prob~proofs prob))
		    (pds~start-proof-plan prob
					  'test)))
	set op)
  (sys~handler-case
   (progn
     (setf set (post~read-object post-set (pds~environment proof) :existing-term))
     (setf op  (post~read-object post-op (pds~environment proof) :existing-term)))
   (error (c) (omega~error "~A: either ~A or ~A is not a correct term."
			   c post-set post-op)))
  (setq omega*current-proof-plan proof)
  (when (and set op) (expl~explore-znz set op))))
   
(defun expl~explore-znz (set operation)

  (incf expl*counter)
  
  (let* ((expl-name (format nil "EXPL-PROBLEM-~A" expl*counter))
	 (expl-table (expl~create-table expl-name set operation nil)))

    (setf (gethash expl-name expl*hashtable) expl-table)
    (setf expl*current-table expl-table)
    
    (when
	;; 0. Check closure
	(expl=check! 'closed expl-table set operation)

      ;; 1. Check commutativity for all closed structs
      (expl=check! 'commu expl-table set operation)
  
      (if
	  ;; 2a. Check associativity
	  (expl=check! 'assoc expl-table set operation)
	  (and 
	   ;; 3. Check Unit-Element
	   (expl=check! 'unit expl-table set operation)
	   ;; 4. Check Inverse
	   (expl=check! 'inverse expl-table set operation)
	   )
	(and 
	 ;;2b. Check divisors 
	 (expl=check! 'divisors expl-table set operation)
	 ;; 3. Check unit
	 (expl=check! 'unit expl-table set operation)
	 )))

    
    (expl=evaluate-exploration expl-table)
    (expl=exploration-message expl-table)
    expl-table))


(defun expl=check! (signifier expl-table set operation)
  (declare (edited  "27-MAR-2000")
	   (authors Ameier)
	   (input   "A signifier (closed,assoc,unit,inverse,divisors), a exploration table, the set, and"
		    "the operation.")
	   (effect  "The exploration table is changed by adding new entries, representing the results of"
		    "proving the problem corresponding to the signifier, the set, and the operation.")
	   (value   "Multiple-Value-Bind:"
		    "First: T/Nil, T if we were able to find a proof of the theorem corresponding to"
		    "       '(signifier set operation)', Nil otherwise."
		    "Second: T/Nil/Error, T if we were able to find a proof of '(signifier set operation)'."
		    "        Nil, if we were able to find a proof of '(not (signifier set operation)'."
		    "        Error, if we were not able to find a proof."
		    "Third: The new created exploration table entry (which is added to the exploration table)."))
  
  (expl=compute-multi-table set operation)

  (let* ((expl-name (keim~name expl-table))
	 (success (cond ((string-equal signifier 'closed)
			 (rcl~check-closure expl*multiplication-table))
			((string-equal signifier 'assoc)
			 (rcl~check-associativity expl*multiplication-table))
			((string-equal signifier 'unit)
			 (rcl~check-unit-element expl*multiplication-table))
			((string-equal signifier 'inverse)
			 (rcl~check-inverses expl*multiplication-table))
			((string-equal signifier 'divisors)
			 (rcl~check-divisors expl*multiplication-table))
			((string-equal signifier 'commu)
			 (rcl~check-commutativity expl*multiplication-table))))
	 (problem (expl=compute-problem signifier set operation success expl-name))
	 (theorem (node~formula (prob~conclusion problem))))
    
    (if success
	(omega~message "~%Trying to prove the property ~A." signifier)
      (omega~message "~%Trying to refute the property ~A." signifier))
    (omega~message "~%Computed Problem ~A." problem)
    (omega~message "~%Start with proving this problem.")
    
    (oc=prove-pre problem)

    ;; Calling Multi!
    (sod=system-work expl*current-strategies expl*current-strategic-control-rules nil 10000)
    ;; Introduce Strategic Justifications
    (sod=make-strategic-justifications)

    (let* ((remaining-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p
						(agenda~all-tasks (pds~agenda omega*current-proof-plan))))
	   (new-expl-table-entry (if remaining-goal-tasks
				     ;; Multi was not able to find a proof
				     (expl~create-table-entry theorem 'error nil)
				   (let* ((strategy (strat~find-strategy-ks
						     (first (pdsj~parameters (node~justification
									      (prob~proof-root omega*current-proof-plan)))))))
				     (expl~create-table-entry theorem omega*current-proof-plan strategy)))))
      (setf (expl~table-entries expl-table)
	    (append (expl~table-entries expl-table) (list new-expl-table-entry)))
      
      (values (if (and success (null remaining-goal-tasks)) 't nil)
	      (cond ((and success (null remaining-goal-tasks)) 't)
		    ((and (null success) (null remaining-goal-tasks))  nil)
		    (t 'error))
	      new-expl-table-entry))))
		     
      
(defun expl=compute-problem (signifier set operation flag expl-name)
  (let* ((env (env~create (th~env 'zmz)))
	 (pre-conc-formula (cond ((string-equal signifier 'closed)
				  (term~appl-create (env~lookup-object 'closed-under env) (list set operation)))
				 ((string-equal signifier 'assoc)
				  (term~appl-create (env~lookup-object 'associative env) (list set operation)))
				 ((string-equal signifier 'unit)
				  (let* ((unit-var (term~variable-create 'e
									 (first (data~abstr-domain (term~type set))))))
				    (term~appl-create (env~lookup-object 'exists-sort env)
						      (list 
						       (term~abstr-create (list unit-var)
									  (term~appl-create (env~lookup-object 'unit env)
											    (list set operation unit-var)))
						       set))))
				 ((string-equal signifier 'inverse)
				  (term~appl-create (env~lookup-object 'inverse-exist env)
						    (list set operation (expl=get-unit-element set))))
				 ((string-equal signifier 'divisors)
				  (term~appl-create (env~lookup-object 'divisors-exist env) (list set operation)))
				 ((string-equal signifier 'commu)
				  (term~appl-create (env~lookup-object 'commutative env) (list set operation)))))

	 (conc-formula (if (null flag)
			   (term~appl-create (env~lookup-object 'not env) (list pre-conc-formula))
			 pre-conc-formula))
	 (prop-name (make-symbol (if (null flag)
				     (format nil "~A-NOT-~A" expl-name signifier)
				   (format nil "~A-~A" expl-name signifier)))))
    (post~read-object (list prop-name '(in zmz)
			    (list 'conclusion
				  (read-from-string (post~print conc-formula nil))))
		      (env~create) :problem)))

(defun expl=get-unit-element (set)
  ;; we assume, that the multiple-cation table is the right one!
  ;; And that we have already a proof that there is an unit element e!

  (let* ((env (th~env 'zmz)))
    
    (multiple-value-bind
	(success unit)
	(rcl~check-unit-element expl*multiplication-table)
      (declare (ignore success))
      
      (if (null (listp unit))
      	  (let* ((class-factor (zmztac=class-factor set)))
	    (term~appl-create (env~lookup-object 'resclass env)
			      (list class-factor (term~constant-create unit (env~lookup-object 'num env)))))
	;; -> cartesian product
	(expl=compute-unit-of-cart set unit)))))

(defun expl=compute-unit-of-cart (rest-set rest-unit-elements)
  (let* ((env (th~env 'zmz)))
    
    (cond ((and (data~appl-p rest-set)
		(keim~equal (data~appl-function rest-set)
			    (data~schema-range (env~lookup-object 'cartesian-product env))))
	   
	   (multiple-value-bind
	       (unit-part1 rest-unit-elements1)
	       (expl=compute-unit-of-cart (first (data~appl-arguments rest-set)) rest-unit-elements)
	     (multiple-value-bind
		 (unit-part2 rest-unit-elements2)
		 (expl=compute-unit-of-cart (second (data~appl-arguments rest-set)) rest-unit-elements1)
	       
	       (values (term~appl-create (env~lookup-object 'pair env)
					 (list unit-part1 unit-part2))
		       rest-unit-elements2))))
	  (t
	   (let* ((class-factor (zmztac=class-factor rest-set)))
	     (values (term~appl-create (env~lookup-object 'resclass env)
				       (list class-factor
					     (term~constant-create (first rest-unit-elements)
								   (env~lookup-object 'num env))))
		     (rest rest-unit-elements)))))))



(defun expl=compute-multi-table (set op)

  (multiple-value-bind
      (sets ops)
      (crihelp=decompose-cartproduct-sets-and-ops set op)
    
    (let* ((class-factors (mapcar #'zmztac=class-factor sets))
	   (class-factor-numbers (mapcar #'keim~name class-factors))
	   (number-lists (mapcar #'crihelp=number-set-to-resclass-set sets))
	   (operations-on-nums (mapcar #'(lambda (op class-factor)
					   (crihelp=convert-operation op class-factor nil))
				       ops class-factors))
	   (table (if (= (length sets) 1)
		      (rcl~multiplication-table (first number-lists) (first class-factor-numbers)
						:operation (first operations-on-nums))
		    (rcl~product-multiplication-table number-lists
						      :modulo class-factor-numbers
						      :operation operations-on-nums))))
      (setf expl*multiplication-table table))))
  
    
;;  (let* ((class-factor (zmztac=class-factor set))
;;	 (class-factor-number (keim~name class-factor))
;;	 (number-list (crihelp=number-set-to-resclass-set set))
;;	 (operation-on-num (crihelp=convert-operation op class-factor nil))
;;	 (table (rcl~multiplication-table number-list class-factor-number :operation operation-on-num)))
;;    (setf expl*multiplication-table table)))

(defun expl=evaluate-exploration (table)
  (declare (edited  "29-MAR-2000")
	   (authors Sorge)
	   (input   "An exploration table.")
	   (effect  "Adds meaningful content to the slots status and result.")
	   (value   "Undefined."))
  (let* ((entries (expl~table-entries table))
	 (proofs (mapcar #'expl~table-entry-proof entries)))
    (cond ((notevery #'pds~proof-plan-p proofs) (setf (expl~table-status table) :error))
	  ((some #'pds~open-nodes proofs) (setf (expl~table-status table) :open))
	  (t (setf (expl~table-status table) :closed)))
    (setf (expl~table-result table)
	  (expl=evaluate-single-property proofs))))

(defun expl=evaluate-single-property (proofs)
  (declare (edited  "29-MAR-2000")
	   (authors Sorge)
	   (input   "A list of proof plans.")
	   (effect  "None.")
	   (value   "The algebra the properties indicate."))
  (flet ((get-property (formula)
		       (cond ((and (logic~negation-p formula)
				   (logic~existential-quantification-p (data~appl-function (car (data~appl-arguments formula)))))
			      (list 'not 'unit))
			     ((logic~negation-p formula)
			      (list 'not (keim~name (data~appl-function (car (data~appl-arguments formula))))))
			     ((logic~existential-quantification-p formula)
			      'unit)
			     (t (keim~name (data~appl-function formula))))))
    (if (every #'pds~proof-plan-p proofs)
      (let ((properties (mapcar #'(lambda (proof) (get-property (node~formula (prob~proof-root proof)))) proofs)))
	(let ((what	(cond ((find 'inverse-exist properties) :group)
			      ((and (find 'unit properties) (find 'associative properties)) :monoid)
			      ((find 'unit properties) :loop)
			      ((find 'associative properties) :semi-group)
			      ((find 'divisors-exist properties) :quasi-group)
			      ((find 'closed-under properties) :magma)
			      (t :algebra))))
	  (cond ((and (find 'commutative properties) what)
		 (read-from-string (concatenate 'string ":abelian-" (string what))))
		(what what)
		(T :error)))))))
	     

(defun expl=exploration-message (table)
  (declare (edited  "29-MAR-2000")
	   (authors Sorge)
	   (input   "An exploration table.")
	   (effect  "Prints some information on the table.")
	   (value   "Undefined."))
  (let ((status (expl~table-status table)))
    (cond ((string-equal status :error)
	   (omega~message "~%Errors occurred during the exploration of problem ~A!" (keim~name table))
	   (omega~message "~%A preliminary result is that the set ~A~%together with the operation ~A~% forms a ~A."
			  (expl~table-set table) (expl~table-operator table) (expl~table-result table)))
	  ((string-equal status :open)
	   (omega~message "~%Some proofs could not be concluded during the exploration of problem ~A!" (keim~name table))
	   (omega~message "~%A preliminary result is that the set ~A~%together with the operation ~A~% forms a ~A."
			  (expl~table-set table) (expl~table-operator table) (expl~table-result table)))
	  (t 
	   (omega~message "~%The set ~A~%together with the operation ~A~%forms a ~A."
			  (expl~table-set table) (expl~table-operator table) (expl~table-result table))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facilities to automatically generate sets and operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lots of the following functions are no longer needed....

(defun expl=make-operation-lambda-expression (operation)
  (list 'lam '(x (o num))
	(list 'lam '(y (o num))
	      operation)))

(defun expl=make-operation-combinations (x y)
  (mapcar #'(lambda (op) (list op x y))
	  '(plus-resclass times-resclass minus-resclass)))

(defun expl=permute-lists2pairs (list1 list2)
  (declare (edited  "31-MAR-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list containing the cartesian product of the two lists."))
  (labels ((permute (l1 l2)
		    (cond ((and (null l1) (null l2)) nil)
			  ((null l1) nil)
			  ((and l1 (null l2)) (permute (cdr l1) list2))
			  ((and l1 l2)
			   (cons (list (car l1) (car l2)) (permute l1 (cdr l2)))))))
    (permute list1 list2)))
  
(defun expl=special-permute-lists2pairs (l1 l2 &optional (list l2))
  (declare (edited  "31-MAR-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list containing the cartesian product of the two lists."))
  (cond ((and (null l1) (null l2)) nil)
	((null l1) nil)
	((and l1 (null l2)) (expl=special-permute-lists2pairs (cdr l1) list list))
	((and l1 l2)
         (let ((l11 (car l1))
               (l21 (car l2)))
           (if (and (listp l11) (listp l21)
                    (string-equal (car l11) 'resclass)
                    (string-equal (car l21) 'resclass))
               (expl=special-permute-lists2pairs l1 (cdr l2) list)
	     (cons (list (car l1) (car l2)) (expl=special-permute-lists2pairs l1 (cdr l2) list)))))))
  
(defun expl=special-permute-lists2pairs-iterative (l1 l2 )
  (declare (edited  "31-MAR-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list containing the cartesian product of the two lists."))
  (let (result)
    (dolist (x l1)
      (dolist (y l2)
	(unless (and (listp x) (listp y)
		     (string-equal (car x) 'resclass)
		     (string-equal (car y) 'resclass))
	  (push (list x y) result))))
    result))
      
(defun expl=remove-commuting-pairs (list)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A list of pairs.")
	   (effect  "None.")
	   (value   "A list where all commuting pairs are removed."))
  (when list
    (let ((farg (caar list))
	  (sarg (cadar list)))
      (if (find (list sarg farg) (cdr list) :test #'tree-equal)
	  (expl=remove-commuting-pairs (cdr list))
	(cons (car list) (expl=remove-commuting-pairs (cdr list)))))))

(defun expl=remove-equal-pairs (list)
  (declare (edited  "06-APR-2000")
	   (authors Sorge)
	   (input   "A list of pairs.")
	   (effect  "None.")
	   (value   "A list where all pairs containing two equal elements are removed."))
  (when list
    (let ((farg (caar list))
	  (sarg (cadar list)))
      (if (equal farg sarg)
	  (expl=remove-equal-pairs (cdr list))
	(cons (car list) (expl=remove-equal-pairs (cdr list)))))))


(defun expl=remove-commuting-operations (list)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A list of operations.")
	   (effect  "None.")
	   (value   "A list where all commuting operation are removed. For example for"
		    "((plus-resclass a b) (plus resclass b a)) the result were ((plus-resclass b a))."))
  (when list
    (let* ((operation (car list))
	   (function (car operation))
	   (farg (cadr operation))
	   (sarg (caddr operation)))
      (if (and (not (string-equal function 'minus-resclass))
	       (find (list sarg farg) (cdr list) :test #'tree-equal))
	  (expl=remove-commuting-operations (cdr list))
	(cons (car list) (expl=remove-commuting-pairs (cdr list)))))))

(eval-when (load compile eval)
(let (level0-vars level0-constants level1 leveln)
  
  (defun expl~generate-operation (set factor)
    (declare (edited  "30-MAR-2000")
	     (authors Sorge)
	     (input   "A list of non-negative integers and two numbers.")
	     (effect  "Changes some locally bound variables.")
	     (value   "A list of operations that combines the standard operations up to a given depths."))
    (let* ((level0 (expl=generate-level0-operations set factor))
	   (level1 (expl=generate-leveln-operations-new (append level0-vars level0-constants)))
	   (level2 (expl=generate-leveln-operations-new (append level0-vars level0-constants level1)))
	   )
      (append level0
	      (mapcar #'expl=make-operation-lambda-expression level2))))
      
    ;;(setf levelnum nil levels nil)

  (defun expl=generate-level0-operations (set factor)
    (declare (edited  "31-MAR-2000")
	     (authors Sorge)
	     (input   "A list of non-negative integers and a number.")
	     (effect  "Changes the level0-vars and level0-constants variables.")
	     (value   "A list of operations with respect to SET and FACTOR."))
    (setf level0-vars '(x y))
    (setf level0-constants
	  (mapcar #'(lambda (element) (list 'resclass factor element)) set))
    (mapcar #'expl=make-operation-lambda-expression (append level0-vars level0-constants)))

  (defun expl=generate-leveln-operations-new (list)
    (let* ((pairs (expl=special-permute-lists2pairs-iterative list list))
	   (com-pairs (expl=remove-commuting-pairs pairs))
	   (min-pairs (expl=remove-equal-pairs pairs)))
      (append
       (mapcar #'(lambda (pair) (list 'plus-resclass (first pair) (second pair))) com-pairs)
       (mapcar #'(lambda (pair) (list 'times-resclass (first pair) (second pair))) com-pairs)
       (mapcar #'(lambda (pair) (list 'minus-resclass (first pair) (second pair))) min-pairs))))

  (defun expl=generate-level1-operations ()
    (declare (edited  "31-MAR-2000")
	     (authors Sorge)
	     (input   "A list of non-negative integers and a number.")
	     (effect  "Changes the level1 and leveln variables.")
	     (value   "A list of first level operations."))
    (let* ((pairs (append (expl=permute-lists2pairs level0-vars level0-vars)
			  (expl=permute-lists2pairs level0-vars level0-constants)
			  (expl=permute-lists2pairs level0-constants level0-vars)))
	   (com-pairs (expl=remove-commuting-pairs pairs)))
    (setf level1
	  (append
	   (mapcar #'(lambda (pair) (list 'plus-resclass (first pair) (second pair))) com-pairs)
	   (mapcar #'(lambda (pair) (list 'times-resclass (first pair) (second pair))) com-pairs)
	   (mapcar #'(lambda (pair) (list 'minus-resclass (first pair) (second pair))) pairs)))
    (setf leveln level1)
    (mapcar #'expl=make-operation-lambda-expression level1)))

  (defun expl==l0 ()
    (append level0-vars level0-constants))
  
  (defun expl=generate-leveln-operations (depth)
    (declare (edited  "31-MAR-2000")
	     (authors Sorge)
	     (input   "A number.")
	     (effect  "Changes the leveln variable.")
	     (value   "A list of nth-level operations."))
    (labels ((build-operations (operations)
			       (when operations
				 (union (expl=remove-commuting-operations
					 (remove-duplicates
					  (expl=make-operation-depth (car operations) depth) :test #'tree-equal))
					 (build-operations (cdr operations)) :test #'tree-equal))))
      (setf leveln (build-operations leveln))
      (mapcar #'expl=make-operation-lambda-expression leveln)))

  (defun expl=make-operation-depth (operation depth)
    (declare (edited  "31-MAR-2000")
	     (authors Sorge)
	     (input   "An operation and a number and a list to avoid duplicates.")
	     (effect  "None.")
	     (value   "A list operations where first-level operations are replaced at the"
		      "given depth of the operation trees."))
    (format t ".")
    (if (= depth 2)
	(let ((func (first operation))
	      (arg1 (second operation))
	      (arg2 (third operation)))
	  (append (mapcar #'(lambda (op) (list func op arg2)) level1)
		  (mapcar #'(lambda (op) (list func arg1 op)) level1)))
      (let* ((func (first operation))
	     (arg1 (second operation))
	     (arg2 (third operation))
	     (farg1? (find arg1 (append level0-constants level0-vars) :test #'tree-equal))
	     (farg2? (find arg2 (append level0-constants level0-vars) :test #'tree-equal)))
	(cond (farg1? (mapcar #'(lambda (x) (list func arg1 x))
			      (expl=make-operation-depth arg2 (1- depth))))
	      (farg2? (mapcar #'(lambda (x) (list func x arg2))
			      (expl=make-operation-depth arg1 (1- depth))))
	      (t (append 
		  (mapcar #'(lambda (x) (list func arg1 x))
			  (expl=make-operation-depth arg2 (1- depth)))
		  (mapcar #'(lambda (x) (list func x arg2))
			  (expl=make-operation-depth arg1 (1- depth)))))))))

	
    
  
  (defun expl==print-variables ()
    (print level0-vars)
    (print level0-constants)
    (print level1)
    (print leveln)
    )
  ))
  
(defun expl=generate-set-permutations (set)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A list representing a set.")
	   (effect  "None.")
	   (value   "A list of sets representing all possible permutations of the input set."))
    (do* ((sets (mapcar #'list set)
		(expl=permute-elements-with-sets set sets))
	  (ressets sets
		   (remove-duplicates sets
				      :test #'(lambda (x y) (and (not (set-difference x y)) (not (set-difference y x))))))
	  (result ressets (append result ressets)))
	((null ressets) (mapcar #'(lambda (x) (sort x #'<)) (copy-tree result)))
      ))

(defun expl=permute-elements-with-sets (elements sets)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A list and a list of sets.")
	   (effect  "None.")
	   (value   "A list of sets where the single elements are added to the sets."))
  (when (and sets elements)
    (append (expl=cons2sets (car elements) sets)
	    (expl=permute-elements-with-sets (cdr elements) (cdr sets)))))

(defun expl=cons2sets (nelem sets)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "An elements and a list of sets.")
	   (effect  "None.")
	   (value   "A list of sets where the element is merged with each set."))
  (when sets
    (if (find nelem (car sets))
	(expl=cons2sets nelem (cdr sets))
      (cons (cons nelem (car sets))
	    (expl=cons2sets nelem (cdr sets))))))

(defun expl=generate-complement-sets (sets set)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A list of sets and a set.")
	   (effect  "None.")
	   (value   "A list of sets representing the complements to the given sets."))
  (mapcar #'(lambda (x) (sort (copy-list x) #'<)) 
	  (mapcar #'(lambda (x) (set-difference set x)) sets)))

(defun expl=translate-to-resclass-set (complement factor)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A set and a number.")
	   (effect  "None.")
	   (value   "A POST representation of the given set as residue class with respect to FACTOR."))
  (labels ((make-set (set)
		     (if (> (length set) 1)
			 (list 'or
			       (list '= 'z (list 'resclass factor (car set)))
			       (make-set (cdr set)))
		       (list '= 'z (list 'resclass factor (car set))))))
    (cond ((null complement) (list 'resclass-set factor))
	  ((= (length complement) 1) (list 'setminus
					   (list 'resclass-set factor)
					   (list 'singleton (list 'resclass factor (car complement)))))
	  (t (list 'setminus
		   (list 'resclass-set factor)
		   (list 'lam '(z (o num)) (make-set complement)))))))

(defgeneric expl~generate-set (set)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A set or a number.")
	   (effect  "None.")
	   (value   "A list of set representation for all possible permutations of the set."))
  (:method ((order number))
	   (expl~generate-set (expl=make-number-list order)))
  (:method ((set cons))
	   (let* ((permutations (expl=generate-set-permutations set))
		  (complements (expl=generate-complement-sets permutations set))
		  (factor (length set)))
	     (mapcar #'(lambda (x) (expl=translate-to-resclass-set x factor)) complements))
	   ))

(defun expl=make-number-list (number)
  (let (list)
    (dotimes (x number) (push x list))
    (reverse list)))

(defun expl~generate-resclass (factor)
  (declare (edited  "03-APR-2000")
	   (authors Sorge)
	   (input   "A number.")
	   (effect  "Creates several terms.")
	   (value   "A list of pairs with term for sets and operations."))
  (let* ((set (expl=make-number-list factor))
	 (operations (expl~generate-operation set factor))
	 (sets (expl~generate-set set))
	 (sets-file (format nil "examples-znz~A-sets.post" factor))
	 (ops-file (format nil "examples-znz~A-ops.post" factor)))
    (with-open-file (stream sets-file
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
		    (dolist (x sets)
		      (format stream "~A~%" x)))
    (with-open-file (stream ops-file
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
		    (dolist (x operations)
		      (format stream "~A~%" x)))
    ))
  ;;     (mapcar #'(lambda (set) (post~read-object set (th~env 'zmz) :existing-term)) sets)
  ;;     (mapcar #'(lambda (op) (post~read-object op (th~env 'zmz) :existing-term))  operations))))

(defun expl~generate-examples (&optional (numbers '(2 3 4 5 6 7 8 9 10)))
  (mapcar #'expl~generate-resclass numbers))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and Executing Examples from the Testbed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expl=read-example ()
  )

(defun expl~count-file-length (file)
  (declare (edited  "07-APR-2000")
	   (authors Sorge)
	   (input   "A filename.")
	   (effect  "Reads from the given file.")
	   (value   "A number corresponding to the number of sexpressions in the given file."))
  (do ((result (keim::th=read-next-sexp file) (keim::th=read-next-sexp))
       (i 0 (1+ i)))
      ((null result) i)))

(defmacro expl=read-n-sexp (file n)
  (declare (edited  "07-APR-2000")
	   (authors Sorge)
	   (input   "A filename and an integer.")
	   (effect  "Reads from the given file.")
	   (value   "Two values: a list containing the read sexpressions and T if the end of"
		    "the file was not yet reached. NIL o/w."))
  `(if (> ,n 0)
       (do* ((result (keim::th=read-next-sexp ,file) (keim::th=read-next-sexp))
	     (reslist (list result) (append reslist (list result)))
	     (i 1 (1+ i)))
	   ((or (= i ,n)
		(null result))
	    (if result
		(values reslist t)
	      (values (butlast reslist) nil))))
     (values nil t)))


(defun expl=read-sexp-to-sexp (file begin &optional (end begin))
  (declare (edited  "07-APR-2000")
	   (authors Sorge)
	   (input   "A filename and two numbers.")
	   (effect  "Reads from the given file.")
	   (value   "Two values: a list containing the read sexpressions and T if the end of"
		    "the file was not yet reached. NIL o/w."))
  (when (and (typep begin '(integer 1))
	     (>= end begin))
    (multiple-value-bind (result success)
	(expl=read-n-sexp file (1- begin))
      (declare (ignore result))
      (multiple-value-prog1
	  (when success
	    (expl=read-n-sexp file (- end (1- begin))))
	(keim::th=stop-reading-sexp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some global variable for the location of the testbed, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar expl*testbed-dir "/home/sorge/diss/group/testbed/")
(defvar expl*result-file "results")
(defvar expl*example-files '(2 3 4 5 6 7 8 9))

(defun expl~delete-result-file (&optional (file expl*result-file))
  (let ((resfile (concatenate 'string expl*testbed-dir file)))
    (when (probe-file resfile)
      (delete-file resfile))))


(defun expl=write-result2file (table &optional (res-file expl*result-file))
  (declare (edited  "07-APR-2000")
	   (authors Sorge)
	   (input   "An exploration table.")
	   (effect  "Writes the results of the exploration table to the result file.")
	   (value   "Undefined."))
  (with-open-file (file res-file
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
		  (format file "~%~A ~A ~A ~A ~{ ~A~}"
			  (post~print (expl~table-set table) nil)
			  (post~print (expl~table-operator table) nil)
			  (expl~table-result table) (expl~table-status table)
			  (mapcar #'(lambda (entry)
				      (keim~name (expl~table-entry-strategy entry)))
				  (expl~table-entries table)))))

(defun expl~set-with-operations (set which from to &optional
				     (file (concatenate 'string expl*testbed-dir
							(format nil "results-~A-~A" set which))))
  (declare (edited  "07-APR-2000")
	   (authors Sorge)
	   (input   "Four integers.")
	   (effect  "Triggers proofs.")
	   (value   "????"))
  (let ((sets-file (concatenate 'string expl*testbed-dir (format nil "examples-znz~A-sets.post" set)))
	(ops-file (concatenate 'string expl*testbed-dir (format nil "examples-znz~A-ops.post" set))))
    (when (and (probe-file sets-file) (probe-file ops-file))
      (multiple-value-bind (set more-sets)
	  (expl=read-sexp-to-sexp sets-file which)
	(multiple-value-bind (ops more-ops)
	    (expl=read-sexp-to-sexp ops-file from to)
	  (when (and set ops)
	    (let* ((env (th~env 'zmz))
		   (set-term (post~read-object set env :existing-term)))
	    (mapcar #'(lambda (op)
			(expl=write-result2file
			 (expl~explore-znz
			  set-term
			  (post~read-object op env :existing-term))
			 file))
		    ops)))
	  )))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The classification of isomorphic structs (expl2class)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defconstant expl*latex-integer-set "\\Int")
(defconstant expl*latex-macros '("\\def\\lambdot{\\rule{0.6mm}{0.6mm}\\hspace{0.4ex}}"
				 "\\def\\lamb#1{\\lambda #1\\lambdot}"
				 "\\def\\vbar#1{\\ensuremath #1}"
				 "\\def\\resclassoperation#1#2#3{(#2 #1 #3)}"
				 "\\def\\Int{{\\mathchoice{\\displaystyle\\rm \\rm Z\\hskip-0.32em Z}{\\textstyle\\rm \\rm Z\\hskip-0.32em Z}{\\scriptstyle\\rm \\rm Z\\hskip-0.22em Z}{\\scriptscriptstyle\\rm \\rm Z\\hskip-0.22em Z}}}"))



(defun expl~latex-results (in-file out-file)
  (declare (edited  "16-DEC-2001")
	   (authors Sorge)
	   (input   "Two filenames.")
	   (effect  "Writes a latex file.")
	   (value   "Undefined."))
  ;;  (let ((result-list (mapcar #'expl=latex-read-from-string (remove "" (expl=latex-read-file in-file) :test #'string-equal))))
  (with-open-file (file out-file
			:direction :output
			:if-does-not-exist :create
			:if-exists :overwrite)
		  (write-string "\\documentclass{article}" file)
		  (terpri file)
		  (write-string "\\usepackage{longtable}" file)
		  (terpri file)
		  (terpri file)
		  (write-string "\\begin{document}" file)
		  (terpri file)
		  (terpri file)
		  (write-string (format nil "~{~A~%~}" expl*latex-macros) file)
		  (terpri file)
		  (terpri file)
		  (write-string "\\begin{longtable}{lll}" file)
		  (terpri file)
		  (write-string "Set & Operation & Structure\\\\\\hline\\hline\\vspace{0cm}\\endhead" file)  ;;; & Closure & Commutativity
		  (terpri file)
		  (expl=latex-results2table in-file file)
		  (write-string "\\end{longtable}" file)
		  (terpri file)
		  (terpri file)
		  (write-string "\\end{document}" file)
		  (terpri file)
		  ))


(let (last-set)
  
  (defun expl=latex-results2table (in-file out-file)
    (declare (edited  "04-AUG-2002")
	     (authors Sorge)
	     (input   "A file name and an open output stream.")
	     (effect  "Writes to OUT-FILE.")
	     (value   "Undefined."))
    (with-open-file (file in-file
			  :direction :input
			  :if-does-not-exist :error)
		    (do ((x (read-line file nil) (read-line file nil)))
			((null x))
		      (omega~output ".")
		    (unless (equal x "")
		      (let* ((result (expl=latex-read-from-string x))
			     (set (car result)))
			(print set)
			(print last-set)
			(unless (equal set last-set)
			  (setf last-set set)
			  (write-string "$" out-file)
			  (write-string (expl=latex-set set) out-file)
			  (write-string "$" out-file)
			  )
			(write-string " & $" out-file)
			(write-string (expl=latex-operation (second result)) out-file)
			(write-string "$ & " out-file)
			(write-string (format nil "~:(~A~)" (third result)) out-file)
			(write-string "\\\\" out-file)
			(terpri out-file)
			)))))

  )


(defun expl=latex-read-from-string (string)
  (declare (edited  "15-DEC-2001")
	   (authors Sorge)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "A list of expressions contained in the string."))
  (do ((rest-string string (subseq rest-string length))
       fstr length
       (result nil (cons fstr result)))
      ((equal rest-string "") (reverse result))
    (multiple-value-setq (fstr length)
      (read-from-string rest-string))))

(defun expl=latex-set (set)
  (declare (edited  "15-DEC-2001")
	   (authors Sorge)
	   (input   "A list representing a residue-class set.")
	   (effect  "None.")
	   (value   "A string containing the latex representation of the set."))
  (labels ((extract-resclass-set (set)
				 (cond ((atom set) nil)
				       ((string-equal (car set) 'resclass-set) set)
				       (t (do* ((rest-set set (cdr rest-set))
						(test (extract-resclass-set (car rest-set))
						      (extract-resclass-set (car rest-set))))
					      ((or (null rest-set) test) test))))))
    (let* ((integer (second (extract-resclass-set set)))
	   (term (post~read-object set (th~env 'zmz) :existing-term))
	   (number-set (crihelp=number-set-to-resclass-set term))
	   (integer-set (rcl=make-element-list integer t))
	   (exclude-set (sort (set-difference integer-set number-set) #'<)))
      (cond ((null exclude-set)
	     (concatenate 'string
			  expl*latex-integer-set
			  (format nil "_{~A}" integer)))
	    ((= (length exclude-set) 1)
	     (concatenate 'string
			  expl*latex-integer-set
			  (format nil "_{~A}" integer)
			  "\\setminus\\{"
			  (expl=latex-congruence-class (car exclude-set) integer)
			  "\\}"))
	    (t (let ((latex-exclude-set
		      (mapcar #'(lambda (element)
				  (expl=latex-congruence-class element integer))
			      exclude-set)))
		 (concatenate 'string
			      expl*latex-integer-set
			      (format nil "_{~A}" integer)
			      "\\setminus\\{"
			      (format nil "~{~A~^,~}" latex-exclude-set)
			      "\\}")))))))

(defun expl=latex-congruence-class (int mod)
  (declare (edited  "15-DEC-2001")
	   (authors Sorge)
	   (input   "Two integers.")
	   (effect  "None.")
	   (value   "The LaTeX representation of the congruence class INT modulo MOD."))
  (format nil "\\bar{~A}_{~A}" int mod))
  
(defun expl=latex-operation (operation)
  (declare (edited  "15-DEC-2001")
	   (authors Sorge)
	   (input   "A list containing a residue class operation.")
	   (effect  "None.")
	   (value   "A string containing the LaTeX representation of the operation."))
  (flet ((constant-congclass (var)
			     (format nil "\\vbar{~(~A~)}" var))
	 (congclass-p (exp)
		      (string-equal (car exp) 'resclass))
	 (resclass-operation (op)
			     (format nil "\\bar{~A}" op))
	 )
    (labels ((op2op (op)
		    (cond ((string-equal op 'minus-resclass) (resclass-operation '-))
			  ((string-equal op 'plus-resclass) (resclass-operation '+))
			  ((string-equal op 'times-resclass) (resclass-operation '*))
			  (t (omega~warn "Don't know this residue class operation")
			     op)))
	     (trans-operation (op)
			      (cond ((atom op)
				     (constant-congclass op))
				    ((congclass-p op)
				     (expl=latex-congruence-class (third op) (second op)))
				    (t (format nil
					       "\\resclassoperation{~A}{~A}{~A}"
					       (op2op (first op))
					       (trans-operation (second op))
					       (trans-operation (third op)))))))
      (let ((op (trans-operation (fourth operation))))
	(concatenate 'string
		     (format nil "\\lamb{~A~A}"
			     (constant-congclass 'x)
			     (constant-congclass 'y))
		     op)))))




(defgeneric expl~latex-exploration-results (directory)
  (declare (edited  "04-AUG-2002")
	   (authors Sorge)
	   (input   "One or a list of directory names.")
	   (effect  "Reads files from the directory.")
	   (value   "A list of file-names containing exploration results."))
  (:method ((directories list))
	   (apply #'append
		  (mapcar #'expl~latex-exploration-results directories)))
  (:method ((directory pathname))
	   (directory directory))
  (:method ((directory string))
	   (directory directory))
  (:method (directory)
	   (omega~error "~A is not of the right type of a directory." directory)))
  

;;; (expl~latex-exploration-results '("/home/sorge/diss/group/results/results-zmz-misc/" "/home/sorge/diss/group/results/results-z6z/results-algebras/" "/home/sorge/diss/group/results/results-z5z/results-algebras/" "/home/sorge/diss/group/results/results-z10z/results-algebras/"))
