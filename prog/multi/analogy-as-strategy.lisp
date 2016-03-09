;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                         ;;
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



;; This Module contains the stuff for applying analogy as strategy


#| ----------------------------------------------------------- Global Variables ------------------------------------------------ |#

(defvar anastrat*verbose nil)
;; wenn gesetzt ist das ganze System etwas gespraechig!

(defvar anastrat*roc-state-description nil)
;; Die momentane State-description (st"andig gemaintaint)

#| ----------------------------------------------------- Analogy ALS REFINEMENT ALGORITHM -------------------------------------- |# 

;; Die Invokation Funktion:

(defun anastrat~anastrat-invokation-function (strategy-ks task parameters)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The strategy-ks (using Analogy as refinement algorithm) and the task.")
	   (effect  "Analogy is invoked (what ever this may affects ...")
	   (value   "Message returned from the invokation of the strategy.")) 

  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; neue Analogy ROC-State-Description 
	 (new-analogy-roc-state-description (roc~fresh-analogy-state-description strategy-ks task nil parameters))
	 ;; Neue Start-strategy-ks Justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just
						  (list new-analogy-roc-state-description)))
	 ;; Neuer Strategy-step aus dieser Justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Roc bekommt start-step als Eintrag
    (setf (roc~start-step new-analogy-roc-state-description) 
	  start-step)

    ;; ROC wird in ROCS-list eingetragen
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)
			new-analogy-roc-state-description)
    
    ;; Strategy-step wird in Strategy-steps eingefuegt
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when anastrat*verbose
      (omega~message "~%Invoking ANALOGY WITH STRATEGY ~A on TASK ~A" strategy-ks task))
    
    (anastrat=calling-anastrat new-analogy-roc-state-description)))

;; Die Reinvokation Funktion!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Da eine Unterbrechung von ANALOGY bisher jedenfalls noch nicht vorgesehen ist, wird hier auch auf eine Reinvokation  ;;
;; Funktion verzichtet!                                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anastrat=calling-anastrat (roc)
  (declare (edited  "18-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state-description for Analogy.")
	   (effect  "Call Analogy on it (what ever this may cause) ...")
	   (value   "Messages returned from Analogy."))
  
  ;; Setzten des momentanen ROCS auf den input ROC!
  (setf anastrat*roc-state-description roc)

  (let* ((parameters (roc~parameters roc))
	 (source-node (first parameters))
	 (goal-node (agenda~task-node (roc~start-task roc))))
    (cond ((find source-node (prob~proof-steps omega*current-proof-plan))
	   ;; -> Internal Analogy

	   (ana~ina source-node goal-node)

	   ;; Was sind moegliche Rueckgaben von Analogy?
	   ;; Was ist mit Outline?
	   ;; Was ist mit Schritten innerhalb der Analogy?
	   )
	  ((eq source-node (prob~proof-root ana*source-plan))
	   ;; -> external Analogy

	   (ana~exa-strategy goal-node)

	   ;; Was sind moegliche Rueckgaben von Analogy?
	   ;; Was ist mit Outline?
	   ;; Was ist mit Schritten innerhalb der Analogy?
	   )
	  (t
	   (error "~%Don't know how to apply analogy to source node ~A" source-node)))

    (exmes~create-termination-message anastrat*roc-state-description
				      nil)))


	  


