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




(strat~define-strategy-ks
 (name an-simplify)
 (refinement-algorithm PPlanner)
 (condition (lambda (x) t)) ;; replace: goal contains annotated constants
 (methods (reflexu-m-b
	   reflex-m-b
	   weaken-m-c
	   an-check-m-c
	   an-eval-m-b
	   ))
 (normalization-methods (
	))
 (restriction-methods (	
	       ))
 (control-rules (
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check (lambda () nil)) ;;replace: no method applicable
 (selection waterfall)
 (remark "")
 (print "Strategy-KS: Simplification ~A for annotated constants"))


(cri~def-control-rule change-task-if-no-method-applicable
		      (kind tasks)
		      (if (an-simplify-NO-METHOD-APPLICABLE-FAILURE  "task"))
		      (then
		       (reject ("task"))))

(defun an-simplify-NO-METHOD-APPLICABLE-FAILURE (task)
  (if (and (exmes~failure-message-p sod*execution-message)
	   (string-equal (first (exmes~failure-message-report sod*execution-message)) 'pplanner)
	   (string-equal (second (exmes~failure-message-report sod*execution-message)) 'no-applicable-method)
	   (third (exmes~failure-message-report sod*execution-message))
	   (string-equal  "an-simplify" (keim~name (roc~strategy-ks (exmes~state-description sod*execution-message)))))
      (list (list (cons task (third (exmes~failure-message-report sod*execution-message)))))
    nil))


;;;;;;;

(setf sod*current-strategies '(;;exps
			       an-simplify))

;(setf crihelp*case-classes  '())

(setf sod*current-strategic-control-rules '(REJECT-ALREADY-APPLIED-STRATEGIES))
