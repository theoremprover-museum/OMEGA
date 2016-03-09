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


(mod~defmod JOB 
            :uses (agenda arg black exmes keim omega reac refalg roc sod store strat)
            :documentation "Stuff and data-structures for job-offers"
            :exports (
                      job+offer
                      job+store-offer
                      job+strategy-ks-offer
                      
                      job~create-store-offer
                      job~create-strategy-ks-offer
                      job~execute-offer!
                      job~job-offer-is-not-already-tried-p
                      job~job-offer-print-string
                      job~job-offers-parameters-are-consistent-p
                      job~offer-p
                      job~parameters
                      job~reaction
                      job~state-description
                      job~store-offer-p
                      job~strategy-ks
                      job~strategy-ks-offer-p
                      job~task
                      ))


;; This module contains the things for the job-offers

#| ------------------------------------------------------- Job+offer ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass job+offer (keim+object)
    ((parameters :initform nil
		 :initarg :parameters
		 :accessor job~parameters)
     (reaction :initform nil
	       :initarg :reaction
	       :accessor job~reaction))))

(eval-when (load compile eval)
  (defclass job+strategy-ks-offer (job+offer)
    ((strategy-ks :initform nil
		  :initarg :strategy-ks
		  :accessor job~strategy-ks)
     (task :initform nil
	   :initarg :task
	   :accessor job~task))))

(eval-when (load compile eval)
  (defclass job+store-offer (job+offer)
    ((state-description :initform nil
			:initarg :state-description
			:accessor job~state-description))))

(defun job~strategy-ks-offer-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a job+strategy-ks-offer, nil otherwise."))
  (typep obj 'job+strategy-ks-offer))

(defun job~store-offer-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a job+store-offer, nil otherwise."))
  (typep obj 'job+store-offer))

(defun job~offer-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a job+offer, nil otherwise."))
  (typep obj 'job+offer))

(defmethod print-object ((job-offer job+strategy-ks-offer) stream)
  (format stream "JOB-OFFER: Calling Strategy-ks ~A on task ~A with parameters ~A and reaction ~A"
	  (job~strategy-ks job-offer)
	  (job~task job-offer)
	  (job~parameters job-offer)
	  (job~reaction job-offer)
	  ))

(defmethod print-object ((job-offer job+store-offer) stream)
  (format stream "JOB-OFFER: Reinvoking state-description ~A from store with parameters ~A and reaction ~A"
	  (job~state-description job-offer)
	  (job~parameters job-offer)
	  (job~reaction job-offer)))

#| ------------------------------------------------------ Create + copy job-offers ---------------------------------------- |#

(defun job~create-strategy-ks-offer (strategy-ks task parameters &optional reaction)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a task, parameters and optional a reaction.")
	   (effect  "None.")
	   (value   "A new strategy-ks job offer."))
  (make-instance 'job+strategy-ks-offer
		 :strategy-ks strategy-ks
		 :task task
		 :parameters parameters
		 :reaction reaction))

(defun job~create-store-offer (state-description parameters &optional reaction)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A state description, parameters, and optional a reaction.")
	   (effect  "None.")
	   (value   "A new store job offer."))
  (make-instance 'job+store-offer
		 :state-description state-description
		 :parameters parameters
		 :reaction reaction))

(defmethod keim~copy ((job job+strategy-ks-offer) &key (explode :all-classes) share preserve downto)
  (job~create-strategy-ks-offer (job~strategy-ks job)
				(job~task job)
				(job~parameters job)
				(job~reaction job)))

(defmethod keim~copy ((job job+store-offer) &key (explode :all-classes) share preserve downto)
  (job~create-store-offer (job~state-description job)
			  (job~parameters job)
			  (job~reaction job)))

#| ---------------------------------------------------- Execute job-offers ------------------------------------------------ |# 

(defgeneric job~execute-offer! (job-offer)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A job-offer.")
	   (effect  "Executing the job-offer.")
	   (value   "Some messages from the execution.")) 
  
  (:method ((job-offer job+strategy-ks-offer))

	   (cond ((job~reaction job-offer)
		  ;; the job carries a reaction
		  (reac~interpret-reaction! (job~reaction job-offer)))
		 ((and sod*execution-message
		       (exmes~failure-message-p sod*execution-message))
		  ;; last Strategy failed + there is no Reaction
		  ;; -> Default -> Interpretiere Strategy as interrupted!
		  (let* ((demand (demand~create-strategy-task-demand (job~strategy-ks job-offer)
								     (job~task job-offer)
								     (job~parameters job-offer)))
			 (reaction (reac~create-interrupt sod*execution-message (list demand))))
		    (reac~interpret-reaction! reaction))))
	   
	   ;; Executed job is an application of a strategy-ks on a task:
	   
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (task (job~task job-offer))
		  (parameters (job~parameters job-offer))
		  (strat-refinement-algorithm (strat~strategy-ks-refinement-algorithm strategy-ks))
		  (ref-alg-invokation-function (refalg~invokation-function strat-refinement-algorithm)))

	     (when (null (job~job-offers-parameters-are-consistent-p job-offer))
	       (omega~error "~%Since its parameters are not consistent the job offer ~A is not executable" job-offer))
	     
	     (apply ref-alg-invokation-function (list strategy-ks task parameters))))
  
  (:method ((job-offer job+store-offer))
	   
	   (when (job~reaction job-offer)
	     ;; the job carries a reaction
	     (reac~interpret-reaction! (job~reaction job-offer) job-offer))

	   ;; Executed job is reinvokation of a computation from the store
	   
	   (let* ((state-description (job~state-description job-offer))
		  (parameters (job~parameters job-offer))
		  (strategy-ks (roc~strategy-ks state-description))		  
		  (ref-alg (strat~strategy-ks-refinement-algorithm strategy-ks))
		  (reinvokation-function (refalg~reinvokation-function ref-alg)))

	     ;; Delete the state-description from the store
	     
	     (store~remove-element! (black~get-blackboard-object-content 'store sod*solution-blackboard)
				    state-description)
	     
	     (apply reinvokation-function (list state-description parameters)))))


#| -------------------------------------------------- Print Job-Offers ----------------------------------------------------- |#

(defgeneric job~job-offer-print-string (job-offer)
  (declare (edited  "06-AUG-1999")
	   (authors Ameier)
	   (input   "A job-offer.")
	   (effect  "None.")
	   (value   "A string which gives a description of the job-offers job."))
  (:method ((job-offer job+strategy-ks-offer))
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (task (job~task job-offer))
		  (parameters (job~parameters job-offer))
		  (print-string (format nil "~A with parameters ~A and reaction ~A"
					(format nil (strat~strategy-ks-print strategy-ks) task)
					parameters
					(job~reaction job-offer))))
	     
	     print-string))
  (:method ((job-offer job+store-offer))
	   (let* ((state-description (job~state-description job-offer))
		  (strategy-ks (roc~strategy-ks state-description))
		  (task (roc~start-task state-description))
		  (print-string (format nil "Reinvoking interrupted computation of Strategy-KS ~A on Task ~A with reaction ~A"
					(keim~name strategy-ks)
					task
					(job~reaction job-offer))))
	     
	     print-string)))

	     
#| ----------------------------------------------------------- TESTS -------------------------------------------------------------- |#

(defgeneric job~job-offers-parameters-are-consistent-p (job-offer)
  (declare (edited  "07-SEP-1999")
	   (authors Ameier)
	   (input   "A job-offer.")
	   (effect  "None.")
	   (value   "If the job-offer is a strategy-ks job-offer:"
		    "T if the job-offers parameters slot contains for each parameter-type in the strategy-ks"
		    "a consistent parameter, nil otherwise."
		    "If the job-offer is a reinvoking job offer: always t"))
  (:method ((job-offer job+strategy-ks-offer))
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (parameter-types (strat~strategy-ks-parameter-types strategy-ks))
		  (parameters (job~parameters job-offer)))

	     (do* ((rest-parameters parameters (rest rest-parameters))
		   (rest-parameter-types parameter-types (rest rest-parameter-types))
		   (back 't))
		 ((or (null back)
		      (null rest-parameters)
		      (null rest-parameter-types))
		  (cond ((and (null rest-parameters)
			      rest-parameter-types)
			 (setf back nil))
			((and rest-parameters
			      (null rest-parameter-types))
			 (setf back nil)))
		  back)
	       
	       (let* ((head-param-type (first rest-parameter-types))
		      (argtype (arg~find-argtype head-param-type))
		      (predicate (arg~predicate argtype))
 		      (head-param (first rest-parameters)))
		 
		 (when (null (funcall predicate head-param))
		   (setf back nil))))))
  (:method ((job-offer job+store-offer))
	   't))



(defgeneric job~job-offer-is-not-already-tried-p (job-offer)
  (declare (edited  "08-SEP-1999")
	   (authors Ameier)
	   (input   "A job offer.")
	   (effect  "None.")
	   (value   "If the job-offer is a reinvoking job offer: always t"
		    "If the job-offer is a strategy-ks job-offer:"
		    "T if the task contains in its already-applied-strategy-ks no strategy-ks/parameters-pair"
		    "keim-equal to the strategy-ks and parameters of this job, nil otherwise."))
  (:method ((job-offer job+store-offer))
	   't)
  (:method ((job-offer job+strategy-ks-offer))
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (parameters (job~parameters job-offer))
		  (task (job~task job-offer))
		  ;;(already-applied-strategy-ks-and-parameter-pairs (agenda~task-already-applied-strategy-ks task))
		  ;; already-applied-strategy-ks are now stored at the node directly
		  (already-applied-strategy-ks-and-parameter-pairs
		   (if (agenda~goal-or-goal-schema-task-p task)
		       (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))
		     (agenda~task-already-applied-strategy-ks task))))
	     
	     (if (find (list strategy-ks parameters)
		       already-applied-strategy-ks-and-parameter-pairs
		       :test #'keim~equal)
		 nil
	       't))))
;; In an earlier Version the function store~call-metareasoner-on-job-offers! tested whether a job-offer was already tried (by using
;; this function) on a task. In the newer Version this is declared as a control rule!

