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

(th~require-completely 'zmz) ;; for instfromcas
(th~require-completely 'limit) ;; for existsi-meta-m-b
;;; The stategies for permutation problems


(strat~define-strategy-ks
 (name permutation-groups)
 (refinement-algorithm PPlanner)
 (condition  not-pseudo-goal)
 (methods (

;; permutation simple	   
	   eval-permutation-m-b
	   eval-function-m-b
	   Re-Represent-with-generators-m-b
	   Product-Of-Generators-m-b
	   Equal-With-GAP-m-b

;; permutation complex	   
	   fixpoint-m-b
	   schreier-lemma-m-b
	   decompose-stabiliserchain-m-b
	   notin-orb-m-b
	   notin-stab-m-b
	   order-by-stab-m-b

;; group theory	   
	   generated-subgrp-by-subset-m-b
	   generated-subset-by-subset-m-b


;; general CAS
	   simplify-m-b
	   
;; natural
	   cardinality-of-set-m-b
	   
;; set theory
	   subset-equal-m-b
;	   ReRepresent-Set-m-b  loop! 
	   In-Set-m-c
	   not-In-Set-m-c
	   foralli-finite-sort-m-b
	   
;; base 
	   existsi-in-sort-m-b
	   Existsi-meta-m-b
	   andi*-m-b
	   =subst-m-b
	   DefnExp-m-b
	   Reflex-m-b
	   

;; Hiding subproofs
	   In-Group-M-B
	   SubGroup-M-B
	   Set-In-Group-M-B
	   In-Orbit-M-B
	   Orbit-Closed-M-B
	   In-Stabiliser-M-B

	   ))
 (normalization-methods (	   
			 ))
 (restriction-methods (
		       Reflex-m-b
		       In-Set-m-c
		       not-In-Set-m-c

		       Equal-With-GAP-m-b
;		       eval-permutation-m-b

		       ;;; Hiding subproofs
		       In-Group-M-B
		       SubGroup-M-B
		       Set-In-Group-M-B
		       In-Orbit-M-B
		       Orbit-Closed-M-B
		       In-Stabiliser-M-B
		       		       
		       ))
 (control-rules (
		 ;;;recursive-expansion

		 perm-subst-concrete
		 ;prefer-equation-tasks 
		 permutation-defnexp-replace
		 perm-by-generators
		 gap-interrupt-if-insttask
		 prefer-eval

		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 ;;;(remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is closed by solving the equation.")
 (print "Strategy-KS Permutation-Groups: Offer to prove task ~A for permutation groups."))

(defun not-pseudo-goal (task) (unless (agenda~pseudo-goal-p task) t))

(defun no-further-goal-p () nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hint mechanism
;;; Instantiation with the help of GAP

;; some changes:

(defclass crihelp+case ()
  ((node                 :initform nil
			 :accessor crihelp~case-node
			 :initarg :node
			 :documentation "The node the case is created from.")
   (mv                   :initform nil
			 :accessor crihelp~case-mv
			 :initarg :mv
			 :documentation "The MV to be instantiated.")
   (processed            :initform nil
			 :accessor crihelp~case-processed
			 :initarg :processed
			 :documentation "A flag whether the hint has already be computed.")
   (success              :initform nil
			 :accessor crihelp~case-success
			 :initarg :success
			 :documentation "The success flag.")
   (hint                 :initform nil
			 :accessor crihelp~case-hint
			 :initarg :hint
			 :documentation "The computed hint.")
   (output               :initform nil
			 :accessor crihelp~case-output
			 :initarg :output
			 :documentation "An association list mapping instatiation tasks to there instantiations.")
   )
  (:documentation "The base class for all cases."))


;; new stuff

(defclass crihelp+case-simple-equation (crihelp+case)
  ((term            :initform nil
		    :accessor crihelp~case-equ-term
		    :initarg :term
		    :documentation "the term of an equation"))
  (:documentation "The base class for all equations of the form MV=TERM."))

(defclass crihelp+case-equation (crihelp+case-simple-equation)
  ((mv-term            :initform nil
		       :accessor crihelp~case-equ-mv-term
		       :initarg :term
		       :documentation "the side of an equation containing a MV"))
  (:documentation "The base class for all equations of the form TERM1(MV)=TERM2."))


;; the instanttiation in the general case should be clear, for orbits we compute the concrete set

(defmethod crihelp=case-compute-multtable ((case crihelp+case-simple-equation))
    (let* ((form (crihelp~case-formula case))
	   ;(form (logic~quantification-scope formula))
	   (lhs (car (data~appl-arguments form)))
	   (rhs (cadr (data~appl-arguments form)))
	   (term (if (term~variable-p lhs) rhs lhs)))
      (setf (crihelp~case-equ-term case) term)))

(defmethod crihelp=case-compute-multtable ((case crihelp+case-equation))
    (let* ((form (crihelp~case-formula case))
	   ;(form (logic~quantification-scope formula))
	   (lhs (car (data~appl-arguments form)))
	   (rhs (cadr (data~appl-arguments form))))
      (multiple-value-bind (mv-term term)
	  (if (some #'term~variable-p (data~all-substructs rhs)) (values rhs lhs) (values lhs rhs))
	(setf (crihelp~case-equ-mv-term case) mv-term)
	(setf (crihelp~case-equ-term case) term))))

(defclass crihelp+case-orbit (crihelp+case-simple-equation crihelp+case-pos)
   ()
  (:documentation "The base class for orbit computation."))


(defmethod crihelp=case-test ((case crihelp+case-orbit))
    (let* ((form (crihelp~case-formula case))
	   ;(form (when (logic~existential-quantification-p formula) (logic~quantification-scope formula)))
	   (lhs (when (and form (logic~equality-p form)) (car (data~appl-arguments form))))
	   (rhs (when (and form (logic~equality-p form)) (cadr (data~appl-arguments form)))))
      (and lhs rhs
	   (or (and (term~variable-p lhs)(crihelp=applfunc-equals 'g-orbit rhs))
	       (and (term~variable-p rhs)(crihelp=applfunc-equals 'g-orbit lhs))))))

(defmethod crihelp=case-compute-hint ((case crihelp+case-orbit))
  (let ((result (perm~gap-set-of-orbit (crihelp~case-equ-term case))))
    (when result (values T result))))

(defmethod crihelp=case-compute-output ((case crihelp+case-orbit) inst-task)
  (crihelp~case-hint case))

(defclass crihelp+case-stabiliser (crihelp+case-simple-equation crihelp+case-pos)
   ()
  (:documentation "The base class for stabiliser computation."))

(defmethod crihelp=case-test ((case crihelp+case-stabiliser))
    (let* ((form (crihelp~case-formula case))
	   ;(form (when (logic~existential-quantification-p formula) (logic~quantification-scope formula)))
	   (lhs (when (and form (logic~equality-p form)) (car (data~appl-arguments form))))o
	   (rhs (when (and form (logic~equality-p form)) (cadr (data~appl-arguments form)))))
      (and lhs rhs
	   (or (and (term~variable-p lhs)(crihelp=applfunc-equals 'stabiliser rhs))
	       (and (term~variable-p rhs)(crihelp=applfunc-equals 'stabiliser lhs))))))

(defmethod crihelp=case-compute-hint ((case crihelp+case-stabiliser))
  (let ((result (perm~gap-set-of-stabiliser (crihelp~case-equ-term case))))
    (when result (values T result))))

(defmethod crihelp=case-compute-output ((case crihelp+case-stabiliser) inst-task)
  (crihelp~case-hint case))


(defclass crihelp+case-perm-apply (crihelp+case-equation crihelp+case-pos)
  ((grp            :initform nil
		   :accessor crihelp~case-equ-qrp
		   :initarg :term
		    :documentation "the term of an equation"))
  (:documentation "The base class for permutation application where the permutation is unknown."))


;; working on new goals and not old goals
(defmethod crihelp=case-compute-multtable ((case crihelp+case-perm-apply))
  (let* ((form (crihelp~case-formula case))
	 (lhs (car (data~appl-arguments form)))
	 (rhs (cadr (data~appl-arguments form))))
    (multiple-value-bind (mv-term term)
	(if (some #'meta~p (data~all-substructs rhs)) (values rhs lhs) (values lhs rhs))
      (setf (crihelp~case-equ-mv-term case) mv-term)
      (setf (crihelp~case-equ-term case) term)
      (let ((grp (find-if #'(lambda (node)                  ;; and there exists a set perm is an element of!
			      (let ((form (node~formula node)))
				(and (data~appl-p form)
				     (crihelp=applfunc-equals 'in form)
				     (eq (car (data~appl-arguments form)) (car (data~appl-arguments mv-term))))))
		(prob~proof-steps omega*current-proof-plan))))
	(setf (crihelp~case-equ-qrp case) (when grp (second (data~appl-arguments (node~formula grp)))))))))
	   

(defmethod crihelp=case-test ((case crihelp+case-perm-apply))
    (let* ((form (crihelp~case-formula case))
	   (lhs (when (logic~equality-p form) (car (data~appl-arguments form))))
	   (rhs (when (logic~equality-p form) (cadr (data~appl-arguments form)))))
      (and lhs rhs
	   (or (and (term~number-p lhs)                              ;; n = perm-apply perm m
		    (crihelp=applfunc-equals 'perm-apply rhs)
		    (term~number-p (second (data~appl-arguments rhs)))
		    (meta~p (car (data~appl-arguments rhs))))
	       (and (term~number-p rhs)                          
		    (crihelp=applfunc-equals 'perm-apply lhs)
		    (term~number-p (second (data~appl-arguments lhs)))
		    (meta~p (car (data~appl-arguments lhs))))))))
	       

(defmethod crihelp=case-compute-hint ((case crihelp+case-perm-apply))
  (when (crihelp~case-equ-qrp case)
    (let* ((results (perm~gap-orbit-proof
		     (term~appl-create perm*g-orbit
				     (list  (crihelp~case-equ-qrp case)
					    perm*perm-apply
					    (second (data~appl-arguments (crihelp~case-equ-mv-term case)))))))
	   (result (find-if #'(lambda (pair)
				(data~equal (car pair) (crihelp~case-equ-term case))) results)))
    (when result (values T (second result))))))

(defun crihelp=case-perm-apply-reverse (perm)
  (if (and (data~appl-p perm)
	   (eq perm*perm-compose (data~appl-function perm)))
      (term~appl-create perm*perm-compose
			(list	(crihelp=case-perm-apply-reverse (second (data~appl-arguments perm)))
				(crihelp=case-perm-apply-reverse (first (data~appl-arguments perm)))))
    perm))

(defmethod crihelp=case-compute-output ((case crihelp+case-perm-apply) inst-task)
  (crihelp~case-hint case))

(defclass crihelp+case-predicate (crihelp+case)
  ((pred            :initform nil
		    :accessor crihelp~case-pred
		    :initarg :pred
		    :documentation "the predicate"))
  (:documentation "The base class formulas of the form (in MV pred)."))

(defmethod crihelp=case-compute-multtable ((case crihelp+case-predicate))
  (setf (crihelp~case-pred case) (cadr (data~appl-arguments (crihelp~case-formula case)))))


(defclass crihelp+case-orbit-representation (crihelp+case-predicate crihelp+case-pos)
   ()
  (:documentation "The base class for orbit-representation computation."))


(defmethod crihelp=case-test ((case crihelp+case-orbit-representation))
    (let* ((form (crihelp~case-formula case))
	   ;(form (when (logic~existential-quantification-p formula) (logic~quantification-scope formula)))
	   (elem (when (crihelp=applfunc-equals 'in form) (car (data~appl-arguments form))))
	   (pred (when (crihelp=applfunc-equals 'in form) (cadr (data~appl-arguments form)))))
      (and elem pred
	   (crihelp=applfunc-equals 'g-orbit-representation pred)
	   (term~variable-p elem))))

(defmethod crihelp=case-compute-hint ((case crihelp+case-orbit-representation))
  (let ((result (perm~gap-g-orbit-representation (crihelp~case-pred case))))
    (when result (values T result))))

(defmethod crihelp=case-compute-output ((case crihelp+case-orbit-representation) inst-task)
  (crihelp~case-hint case))

(defclass crihelp+case-perm-order (crihelp+case-simple-equation crihelp+case-pos)
   ()
  (:documentation "The base class for order computation."))

(defmethod crihelp=case-test ((case crihelp+case-perm-order))
  (labels ((applfunc-equals (prop formula)
			    (and (data~appl-p formula)
				 (data~equal (data~appl-function formula)
					     (env~lookup-object prop (pds~environment omega*current-proof-plan))))))
    (let* ((form (crihelp~case-formula case))
	   ;(form (when (logic~existential-quantification-p formula) (logic~quantification-scope formula)))
	   (lhs (when (and form (logic~equality-p form)) (car (data~appl-arguments form))))
	   (rhs (when (and form (logic~equality-p form)) (cadr (data~appl-arguments form)))))
      (and lhs rhs
	   (or (and (term~variable-p lhs)
		    (crihelp=applfunc-equals 'cardinality rhs)
		    (applfunc-equals 'generated-set (car (data~appl-arguments rhs))))
	       (and (term~variable-p rhs)
		    (crihelp=applfunc-equals 'cardinality lhs)
		    (applfunc-equals 'generated-set (car (data~appl-arguments lhs)))))))))


(defmethod crihelp=case-compute-hint ((case crihelp+case-perm-order))
  (let ((result (perm~gap-order (crihelp~case-equ-term case))))
    (when result (values T result))))

(defmethod crihelp=case-compute-output ((case crihelp+case-perm-order) inst-task)
  (crihelp~case-hint case))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strategic control



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(setf sod*current-strategies '(;;exps
			       permutation-groups
			       InstFromGAP
			       backtrack-step-to-task))

(mapc #'(lambda (class)
	  (unless (member class crihelp*case-classes)
	    (push class crihelp*case-classes)))
      '(crihelp+case-orbit crihelp+case-orbit-representation crihelp+case-stabiliser
			   crihelp+case-perm-apply crihelp+case-perm-order))

(setf sod*current-strategic-control-rules '(
					    PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
					    PREFER-DEMAND-FULFILLING
					    PREFER-OFFERS-FROM-STORE
					    ))



#|
(strat~define-strategy-ks
 (name test)
 (refinement-algorithm PPlanner)
 (condition always)
 (methods (
	   	   andi-m-b
		   reflex-m-b
		   in-set-m-c
		   existsi-sort-m-b
	   ))
 (normalization-methods ())
 (restriction-methods ())
 (control-rules ()) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (print "testStrategy: Offer to proof task ~A with"))



(cri~def-control-rule PREFER-BACKTRACK-CPOOL-FOR-test
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is test "strategy"))                  ;; Strategy war 
			       (and (job-is BACKTRACK-CPOOL-STEP-AFTER-TASK "job1" "task")
				    ;; Es gibt Angebot von BACKTRACK-CPOOL-STEP-AFTER-TASK
				    (job-is BACKTRACK-STEP-TO-TASK "job2" "task")))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))
|#
