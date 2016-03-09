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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;multi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(strat~define-strategy-ks
 (name homo-assoc)
 (refinement-algorithm PPlanner)
 (condition (lambda (x) t))
 (methods (

	   group-closed-m-b
           group-assoc-m-b
           group-inv-m-b
           group-neut-m-b

	   reflexu-m-b
	   weaken-m-c
	   apply-assertion-m-b
	   equal-func-m-b
	   equal-subst-m-b

	   neutral-in-group-m-c
	   inverse-in-group-m-b

	   SUBSET-IMAGE-RANGE-M-b
	   ;subset-kernel-domain-m-b

	   EXISTSI-SORT-M-B
	   FORALLI-SORT-M-B
	   ANDI-M-B
	   ;IMPI-M-B
	   DefnExp-m-b

	   image-of-neut-m-b
	   image-of-inv-m-b
	   inv-of-neut-m-b


	   ;;;;auto methods
	   element-of-kernel-m-f
	   element-of-domain-m-f
	   ;existse-sort-m-f
	   ande-m-f
	   andi-m-b
	   foralli-sort-m-b
	   Expand-in-m-b
		
	   ))
 
 (normalization-methods (
			 element-of-kernel-m-f
			 ande-m-f
			 ))
 (restriction-methods (
		       element-of-domain-m-f
		       foralli-sort-m-b
		       andi-m-b
		       Expand-in-m-b
		       ;existse-sort-m-f
		       ;foralle-sort-m-f
		       ))
 (control-rules (
		 homo-defnexp-select
		 prefer-equation-tasks
		 group-closed-only-once
		 homo1
		 homo2
		 rewrite-to-unit
		 ))
 (loop-detection )
 (termination-check (lambda () nil))
 (selection waterfall)
 (print "Strategy-KS Homo ~A"))


(strat~define-strategy-ks
 (name homo)
 (refinement-algorithm PPlanner)
 (condition (lambda (x) t))
 (methods (
	   HOMOMORPHISM-M-B
	   homomorphism-on-domain-m-b

	   reflexu-m-b
	   weaken-m-c
	   apply-assertion-m-b
	   equal-func-m-b
	   equal-subst-m-b


	   neutral-in-group-m-c
	   inverse-in-group-m-b

	   ;SUBSET-IMAGE-RANGE-M-b
	   ;subset-kernel-domain-m-b


	   group-closed-m-b
           group-assoc-m-b
           group-inv-m-b
           group-neut-m-b

	    
	   EXISTSI-SORT-M-B
	   FORALLI-SORT-M-B
	   ANDI-M-B
	   ;IMPI-M-B
	   DefnExp-m-b

	   image-of-neut-m-b
	   image-of-inv-m-b
	   inv-of-neut-m-b


	   ;;;;auto methods
	   element-of-kernel-m-f
	   element-of-domain-m-f
	   ;existse-sort-m-f
	   ande-m-f
	   andi-m-b
	   foralli-sort-m-b
	   Expand-in-m-b


	   ;or-e**-m-b
	   ;oril-m-b
	   ;orl-m-b

	   ))
 
 (normalization-methods (
			 element-of-kernel-m-f
			 ande-m-f
			 ))
 (restriction-methods (
		       element-of-domain-m-f
		       foralli-sort-m-b
		       andi-m-b
		       Expand-in-m-b
		       ;existse-sort-m-f
		       ;foralle-sort-m-f
		       ))
 (control-rules (
		 homo-defnexp-select
		 prefer-equation-tasks
		 group-closed-only-once
		 homo1
		 homo2
		 rewrite-to-unit
		 ))
 (loop-detection )
 (termination-check (lambda () nil))
 (selection waterfall)
 (print "Strategy-KS Homo ~A"))


;(strat~define-strategy-ks
; (name homo-f-b)
; (refinement-algorithm PPlanner)
; (condition (lambda (x) t))
; (methods (
;           
;           HOMOMORPHISM-M-B
;           homomorphism-on-domain-m-b
;           homomorphism-on-inverse-m-b
;           homomorphism-on-unit-m-b
;           
;           EQUAL-REF-M-c 
;           equal-to-m-c
;           equal-func-m-b
;
;
;           UNPACK-S-B
;;           EXISTSI-SORT-M-B
;;           FORALLI-SORT-M-B
;;           ANDI-M-B
;;           IMPI-M-B
;           DEFNI-M
;
;           MYAssertion-m-b
;
;            ))
; (normalization-methods ())
; (restriction-methods (
;                       ))
; (control-rules (
;                 prefer-equation-tasks
;                 reject-defni
;                 homo1
;                 homo2))
; (loop-detection nil)
; (termination-check (lambda () nil))
; (print "Strategy-KS Homo with forward backward ~A"))
;

;;;;;;;


;(defun no-unpacked-ass-p ()
;  (let* ((pds omega*current-proof-plan)
;         (all-tasks (agenda~all-tasks (pds~agenda pds)))
;         (supps (mapcan #'(lambda (task)
;                            (pds~node-supports (agenda~task-node task)))
;                        all-tasks)))
;    (every #'(lambda (supp)
;               (and
;                (not (and (data~appl-p supp)
;                          (th~find-assumption (keim~name (data~appl-function supp))
;                                              (prob~theory pds))))
;                (not (logic~universal-quantification-p  supp))
;                (not (logic~conjunction-p supp))))
;           (mapcar #'node~formula supps))))
;  
;  
;(strat~define-strategy-ks
; (name unpack-ass)
; (refinement-algorithm PPlanner)
; (condition (lambda (x) t))
; (methods (
;           EXISTSE-SORT-M-f
;           defnE-M
;           ande-M-f
;           ))
; (normalization-methods ())
; (restriction-methods (
;                       ))
; (control-rules (
;                 reject-defne
;                 ))
; (loop-detection nil)
; (termination-check no-unpacked-ass-p)
; (print "unpack-ass ~A"))
;
;



(strat~define-strategy-ks
 (name xhomo)
 (refinement-algorithm PPlanner)
 (condition (lambda (x) t))
 (methods (
	   xHOMOMORPHISM-M-B
	   ;homomorphism-on-domain-m-b

	   reflexu-m-b
	   weaken-m-c
	   ;apply-assertion-m-b
	   equal-func-m-b
	   equal-subst-m-b

	   xapply-assert-m-b

	   XEXISTSI-SORT-M-B
	   XFORALLI-SORT-M-B
	   ANDI-M-B
	   ;IMPI-M-B
	   DefnExp-m-b

	   ;image-of-neut-m-b
	   ;image-of-inv-m-b
	   inv-of-neut-m-b


	   ;;;;auto methods
	   ;element-of-kernel-m-f
	   ;element-of-domain-m-f
	   ;existse-sort-m-f
	   ande-m-f
	   andi-m-b
	   Expand-in-m-b
		
	   ))
 
 (normalization-methods (
			 ;element-of-kernel-m-f
			 ;ande-m-f
			 ))
 (restriction-methods (
		       ;element-of-domain-m-f
		       ;foralli-sort-m-b
		       ;andi-m-b
		       Expand-in-m-b
		       ;existse-sort-m-f
		       ;foralle-sort-m-f
		       ))
 (control-rules (
		 homo-defnexp-select
		 prefer-equation-tasks
		 group-closed-only-once
		 homo1
		 homo2
		 rewrite-to-unit
		 ))
 (loop-detection 2)
 (termination-check (lambda () nil))
 (selection waterfall)
 (print "Strategy-KS XHomo ~A"))

;(push  'homo-assoc sod*current-strategies)
;(push  'homo sod*current-strategies)
;(push  'xhomo sod*current-strategies)



(cri~def-control-rule defer-ALREADY-APPLIED-STRATEGIES
		      (kind strategic)
		      (if  (job-is-already-tried "job"))
		      (then                                   
		       (defer ("job"))))                     

(cri~def-control-rule reject-instjob
		      (kind strategic)
		      (if  (job-is-insttask "job"))
		      (then                                  
		       (reject ("job"))))                    






(defun job-is-insttask (job)
  (cond ((stringp job)
	 (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
		(already-tried-job-offers (remove-if-not #'(lambda (j)
							     (and (job~strategy-ks-offer-p j)
								  (agenda~inst-task-p (job~task j)))) all-job-offers)))
	   (if already-tried-job-offers
	       (mapcar #'(lambda (jobi)
			   (list (cons job jobi)))
		       already-tried-job-offers)
	     nil)))
	(t
	 ;; job is already bound
	 (if  (and (job~strategy-ks-offer-p job)
		   (agenda~inst-task-p (job~task job)))
	     (list (list (cons t t)))
	   nil))))



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


(cri~def-control-rule BACKTRACK-FOR-homo
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is homo "strategy"))                  ;; Strategy war 
			       (job-is BACKTRACK-STEP-TO-TASK "job1" "task"))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))

(cri~def-control-rule PREFER-BACKTRACK-CPOOL-FOR-homo
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is homo "strategy"))                  ;; Strategy war 
			       (and (job-is BACKTRACK-CPOOL-STEP-AFTER-TASK "job1" "task")
				    ;; Es gibt Angebot von BACKTRACK-CPOOL-STEP-AFTER-TASK
				    (job-is BACKTRACK-STEP-TO-TASK "job2" "task")))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))


#|


(defun sod=system-work (strategy-symbols strategic-crules-symbols interactive bound &key (start-strategy nil))
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A list of usable strategy-symbols and a list of usable strategic crules symbols.")
	   (effect  "...")
	   (value   "..."))
  
  ;; initialize
(time
 (progn
   (sod~initialize! omega*current-proof-plan strategy-symbols strategic-crules-symbols interactive bound)

   (setf sod*VIL nil)
   (setf pplan*VIL-on nil)

   ;; restart maple
   (when (socket~find-socket :Service)
     (maple~leave)
     (maple~enter))
  
   ;; call meta-planer
   (sod~system-go :start-strategy start-strategy)
   )))


|#
