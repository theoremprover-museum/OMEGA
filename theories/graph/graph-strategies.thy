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

(th~require-completely 'limit) ;; for existsi-meta-m-b

(strat~define-strategy-ks
 (name graph-problems)
 (refinement-algorithm PPlanner)
 (condition  not-pseudo-goal)
 (methods (
;; set theory
	   foralli-finite-sort-m-b
	   existsi-finite-sort-m-b
;; post
	   an-check-m-c
	   an-check-not-prem-m-c
	   an-eval-m-b	
	   eval-function-m-b
;;basics	   
	   assertion-m-b
	   existsi-meta-m-b
	   ImpI-m-b
	   andi*-m-b
	   DefnExp-m-b
	   Reflex-m-b
	   ))
 (normalization-methods (	   
			 ))
 (restriction-methods (
		       Reflex-m-b
		       ))
 (control-rules (
		 graph-theorem-select
		 graph-defnexp-replace
		 graph-prefer
		 gap-interrupt-if-insttask
		 )) 
 (loop-detection 5)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 ;;;(remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is closed by solving the equation.")
 (print "Strategy-KS Permutation-Groups: Offer to prove task ~A for permutation groups."))

(defun not-pseudo-goal (task) (unless (agenda~pseudo-goal-p task) t))

(defun no-further-goal-p () nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass crihelp+case-graphiso (crihelp+case-2 crihelp+case-pos)
  ()
  (:documentation "The isomorphic case."))

(defmethod crihelp=case-test ((case crihelp+case-graphiso))
  (declare (edited  "7-JUL-2004")
	   (authors Pollet)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is an isomorphic formula."))
  (let* ((form (crihelp~case-formula case)))
    (when (crihelp=applfunc-equals 'graph-isomorphism form)
      (let ((args (data~appl-arguments form)))
	    (and (term~set-p (first args))
		 (term~set-p (second args))
		 (every #'term~set-p (term~normalform (second args)))
		 (term~set-p (third args))
		 (term~set-p (fourth args))
		 (every #'term~set-p (term~normalform (fourth args)))
		 (meta~p (fifth args)))))))

(defmethod crihelp=case-compute-multtable ((case crihelp+case-graphiso))
  (declare (edited  "7-JUL-2004")
	   (authors Pollet)
	   (input   "A case object.")
	   (effect  "Computes the multiplication tables and stores it in the slot of the case.")
	   (value   "Undefined."))
)

(defmethod crihelp=case-compute-hint ((case crihelp+case-graphiso))
  (declare (edited  "7-JUL-2004")
	   (authors Pollet)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (let ((result (apply #'graph~gap-isomorphism (butlast (data~appl-arguments (crihelp~case-formula case))))))
    (when result (values T result))))



(defmethod crihelp=case-compute-output ((case crihelp+case-graphiso) inst-task)
  (declare (edited  "7-JUL-2004")
	   (authors Pollet)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (crihelp~case-hint case))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf sod*current-strategies '(;;exps
			       graph-problems
			       InstFromgap
			       backtrack-step-to-task))

(mapc #'(lambda (class)
	  (unless (member class crihelp*case-classes)
	    (push class crihelp*case-classes)))
      '(crihelp+case-graphiso))

(setf sod*current-strategic-control-rules '(
					    PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
					    PREFER-DEMAND-FULFILLING
					    PREFER-OFFERS-FROM-STORE
					    ))

