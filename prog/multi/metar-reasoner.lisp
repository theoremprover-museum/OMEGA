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


(mod~defmod METAR 
            :uses (black cri job keim sod)
            :documentation "THE METAREASONER"
            :exports (
                      
                      metar~reasoner
                      
                      metar*current-strategic-control-rules))

;; This module contains the stuff for the MetarReasoner. The MetaReasoner is confronted with the following situation:
;; There is a set of job offers on the control blackboard in the job offer store. Now the Meta-Reasoner has to evaluate the strategic
;; control rules and to change the order of the job offers (and evtl. to delete some ...)

(defvar metar*current-strategic-control-rules nil)
;; To store the current strategic control rules in 

(defun metar~reasoner (job-offers)
  (declare (edited  "11-JUN-1999")
	   (authors Ameier)
	   (input   "A list of job-offers.")
	   (effect  "None.")
	   (value   "The strategic control rules are called on the job offers. Returned is the resulting list"
		    "of job-offers."))

  ;; set the currently used control rules to the permitted strategic ones!
  (cri~remove-all-used-control-rules)
  (cri~set-used-control-rules! metar*current-strategic-control-rules)
  
  (multiple-value-bind
      (results applied-crules)
      (cri~call job-offers
		:kind 'strategic
		:pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
    
    
    ;; if parameter is set, a copy of the job offer is made and annotated with the parameters!
    (mapcar #'(lambda (result)
		(if (listp result)
		    (let* ((job (first result))
			   (parameters (second result)))
		      
		      (if (not (equal parameters 'dummy))
			  ;; dummy stands for nil ....
			  (let* ((job-offer-copy (keim~copy job)))
			    (setf (job~parameters job-offer-copy) parameters)
			    job-offer-copy)
			job))
		  result))
	    results)))

