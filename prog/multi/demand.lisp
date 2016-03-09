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


(mod~defmod DEMAND 
            :uses (exmes job keim roc)
            :documentation "Stuff and data-structures for demands"
            :exports (
                      demand+demand
                      demand+strategy-task-demand
                      demand+task-demand
                      
                      demand~create-demand
                      demand~create-strategy-task-demand
                      demand~create-task-demand
                      demand~demand-is-fulfilled-by-termination-with-message-p
                      demand~demand-p
                      demand~demand-will-be-fulfilled-by-job-offer-modulo-parameters-p
                      demand~demand-will-be-fulfilled-by-job-offer-p
                      demand~parameters
                      demand~strategy-task-demand-p
                      demand~strategy-task-demand-strategy-ks
                      demand~strategy-task-demand-task
                      demand~task-demand-p
                      demand~task-demand-task
                      ))







;; If the execution of a sterategy-ks can not be continued before some other strategy-ks are performed, the execution can be interrupted
;; and the demands for other things can be placed on the control blackboard. This module contains the data-strctutures for demands.
;;
;; Do you need perhaps several kinds of demands?
;; -> as a precaution it is implemented open!
;;

#| --------------------------------------------------------DEMANDS in general ------------------------------------------------------ |#

(eval-when (load compile eval)
  (defclass demand+demand (keim+object)
    ((parameters :initform nil
		 :initarg :parameters
		 :accessor demand~parameters))))

(defmethod print-object ((demand demand+demand) stream)
  (format stream "<A demand with parameters ~A>"
	  (demand~parameters demand)))

(defun demand~demand-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a demand+demand, nil otherwise."))
  (typep obj 'demand+demand))

(defun demand~create-demand (parameters)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of parameters.")
	   (effect  "None.")
	   (value   "A new demand."))
  
  (make-instance 'demand+demand
		 :parameters parameters))

#| ---------------------------------------------------- Strategy+TASK demand --------------------------------------------------------- |#

;; This demand requires to apply an specified strategy to a task

(eval-when (load compile eval)
  (defclass demand+strategy-task-demand (demand+demand)
    ((strategy-ks :initform nil
		  :initarg :strategy-ks
		  :accessor demand~strategy-task-demand-strategy-ks)
     (task :initform nil
	   :initarg :task
	   :accessor demand~strategy-task-demand-task))))

(defmethod print-object ((demand demand+strategy-task-demand) stream)
  (format stream "<A STRATEGY-TASK-demand for strategy-ks ~A on task ~A with parameters ~A>"
	  (keim~name (demand~strategy-task-demand-strategy-ks demand))
	  (demand~strategy-task-demand-task demand)
	  (demand~parameters demand)))

(defun demand~strategy-task-demand-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a demand+strategy-task-demand, nil otherwise."))
  (typep obj 'demand+strategy-task-demand))

(defun demand~create-strategy-task-demand (strategy-ks task parameters)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A strategy-ks and a task and parameters.")
	   (effect  "None.")
	   (value   "A new strategy-task-demand."))

  (make-instance 'demand+strategy-task-demand
		 :strategy-ks strategy-ks
		 :task task
		 :parameters parameters))

#| -------------------------------------------------------------- TASK DEMANDS -------------------------------------------------------- |#

;; Task demands only require to work on a task (in comparison withg Strategy+Task demands) einen Task zu bearbeiten.

(eval-when (load compile eval)
  (defclass demand+task-demand (demand+demand)
    ((task :initform nil
	   :initarg :task
	   :accessor demand~task-demand-task))))

(defmethod print-object ((demand demand+task-demand) stream)
  (format stream "<A TASK-demand for task ~A with parameters ~A>"
	  (demand~task-demand-task demand)
	  (demand~parameters demand)))

(defun demand~task-demand-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a demand+task-demand, nil otherwise."))
  (typep obj 'demand+task-demand))

(defun demand~create-task-demand (task parameters) 
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "A new Task-demand."))

  (make-instance 'demand+task-demand
		 :task task
		 :parameters parameters))

;; Removed again!
;; #| --------------------------------------------------------- BackTrack Demands ------------------------------------------------------- |#
;;
;; ;; backtrack demands are treated in principle as normal task demands, but we need a strategic control rule, that prescribes,
;; ;; that if there are backtracking demands, backdtacking should be prefered on those.
;;
;; (eval-when (load compile eval)
;;  (defclass demand+backtrack-demand (demand+demand)
;;    ((task :initform nil
;;	   :initarg :task
;;	   :accessor demand~backtrack-demand-task))))
;;
;;(defmethod print-object ((demand demand+backtrack-demand) stream)
;; (format stream "<A BackTrack-demand for task ~A with parameters ~A>"
;;	  (demand~backtrack-demand-task demand)
;;	  (demand~parameters demand)))
;;
;;(defun demand~backtrack-demand-p (obj)
;;  (declare (edited  "19-MAR-1999")
;;	   (authors Ameier)
;;	   (input   "Something.")
;;	   (effect  "None.")
;;	   (value   "T if input is a demand+backtrack-demand, nil otherwise."))
;;  (typep obj 'demand+backtrack-demand))
;;
;;(defun demand~create-backtrack-demand (task parameters)
;;  (declare (edited  "07-JUN-1999")
;;	   (authors Ameier)
;;	   (input   "A task and parameters.")
;;	   (effect  "None.")
;;	   (value   "A new backtrack-demand."))
;;  (make-instance 'demand+backtrack-demand
;;		 :task task
;;		 :parameters parameters))
;;

#| -------------------------------------------------------- General functions ---------------------------------------------------- |#

(defgeneric demand~demand-is-fulfilled-by-termination-with-message-p (demand message)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A demand and a termination-message.")
	   (effect  "None.")
	   (value   "T if the demand is fulfilled by the termination message,"
		    "otherwise nil."))

  (:method ((demand demand+strategy-task-demand) message)
	   (let* ((state-des (exmes~state-description message))
		  (strategy-ks (roc~strategy-ks state-des))
		  (task (roc~start-task state-des))
		  (parameters (roc~parameters state-des))
		  (demand-strategy-ks (demand~strategy-task-demand-strategy-ks demand))
		  (demand-task (demand~strategy-task-demand-task demand))
		  (demand-parameters (demand~parameters demand)))
	     

	     (if (and (eq demand-strategy-ks strategy-ks)
		      (eq demand-task task)
		      (or (keim~equal demand-parameters parameters)
			  (null demand-parameters)))  ;; non parameterized demand gets fulfilled by parameterized EXMES
		 't
	       nil)))
  (:method ((demand demand+task-demand) (message exmes+termination-message))
	   (let* ((state-des (exmes~state-description message))
		  (task (roc~start-task state-des))
		  (parameters (roc~parameters state-des))
		  (demand-task (demand~task-demand-task demand))
		  (demand-parameters (demand~parameters demand)))
	     
	     (if (and (eq demand-task task)
		      (or (keim~equal demand-parameters parameters)
			  (null demand-parameters)))  ;; non parameterized demand gets fulfilled by parameterized EXMES
		 't
	       nil))))

(defgeneric demand~demand-is-fulfilled-by-termination-of-state-des-p (demand state-des)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A demand and a state-description.")
	   (effect  "None.")
	   (value   "T if the demand is fulfilled by the termination of the state-description,"
		    "otherwise nil."))
  (:method ((demand demand+strategy-task-demand) state-des)
	   (let* ((strategy-ks (roc~strategy-ks state-des))
		  (task (roc~start-task state-des))
		  (parameters (roc~parameters state-des))
		  (demand-strategy-ks (demand~strategy-task-demand-strategy-ks demand))
		  (demand-task (demand~strategy-task-demand-task demand))
		  (demand-parameters (demand~parameters demand)))
	     

	     (if (and (eq demand-strategy-ks strategy-ks)
		      (eq demand-task task)
		      (or (keim~equal demand-parameters parameters)
			  (null demand-parameters)))  ;; non parameterized demand gets fulfilled by parameterized EXMES
		 't
	       nil)))
  (:method ((demand demand+task-demand) state-des)
	   (let* ((task (roc~start-task state-des))
		  (parameters (roc~parameters state-des))
		  (demand-task (demand~task-demand-task demand))
		  (demand-parameters (demand~parameters demand)))
	     

	     (if (and (eq demand-task task)
		      (or (keim~equal demand-parameters parameters)
			  (null demand-parameters)))  ;; non parameterized demand gets fulfilled by parameterized EXMES
		 't
	       nil))))

(defgeneric demand~demand-will-be-fulfilled-by-job-offer-p (demand job-offer)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A demand and a job-offer.")
	   (effect  "None.")
	   (value   "T if the demand is fulfilled by the job-offer,"
		    "otherwise nil."))
  (:method ((demand demand+strategy-task-demand) (job-offer job+strategy-ks-offer))
	   
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (task (job~task job-offer))
		  (demand-strategy-ks (demand~strategy-task-demand-strategy-ks demand))
		  (demand-task (demand~strategy-task-demand-task demand))
		  (parameters-job-offer (job~parameters job-offer))
		  (parameters-demand (demand~parameters demand)))
	     
	     (cond ((and (eq demand-strategy-ks strategy-ks)
			 (eq demand-task task)
			 (or (keim~equal parameters-job-offer parameters-demand)
			     (null parameters-demand)))  ;; non parameterized demand gets fulfilled by parameterized job
		    't)
		   (t
		    nil))))
  (:method ((demand demand+task-demand) (job-offer job+strategy-ks-offer))
	   (let* ((task (job~task job-offer))
		  (demand-task (demand~task-demand-task demand))
		  (parameters-job-offer (job~parameters job-offer))
		  (parameters-demand (demand~parameters demand)))
	     
	     (cond ((and (eq demand-task task)
			 (or (keim~equal parameters-job-offer parameters-demand)
			     (null parameters-demand)))  ;; non parameterized demand gets fulfilled by parameterized job
		    't)
		   (t
		    nil))))
  (:method (demand (job-offer job+store-offer))
	   ;; JOBS in the store could no longer satisfy new potential demands!
	   nil))


(defgeneric demand~demand-will-be-fulfilled-by-job-offer-modulo-parameters-p (demand job-offer)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A demand and a job-offer.")
	   (effect  "None.")
	   (value   "T if the demand is fulfilled by the job-offer,"
		    "otherwise nil modulo the parameters."))
  (:method ((demand demand+strategy-task-demand) (job-offer job+strategy-ks-offer))
	   
	   (let* ((strategy-ks (job~strategy-ks job-offer))
		  (task (job~task job-offer))
		  (demand-strategy-ks (demand~strategy-task-demand-strategy-ks demand))
		  (demand-task (demand~strategy-task-demand-task demand))
		  (parameters-job-offer (job~parameters job-offer))
		  (parameters-demand (demand~parameters demand)))
	     
	     (cond ((and (eq demand-strategy-ks strategy-ks)
			 (eq demand-task task)
			 (or (keim~equal parameters-job-offer
					 parameters-demand)      ;; parameter equal 
			     (null parameters-demand)            ;; non parameterized demand gets fulfilled by parameterized job
			     (null parameters-job-offer)))       ;; job-offer not parameterized -> can still get parameterized
		    't)
		   (t
		    nil))))
  (:method ((demand demand+task-demand) (job-offer job+strategy-ks-offer))
	   (let* ((task (job~task job-offer))
		  (demand-task (demand~task-demand-task demand))
		  (parameters-job-offer (job~parameters job-offer))
		  (parameters-demand (demand~parameters demand)))
	     
	     (cond ((and (eq demand-task task)
			 (or (keim~equal parameters-job-offer
					 parameters-demand)      ;; parameter equal 
			     (null parameters-demand)            ;; non parameterized demand gets fulfilled by parameterized job
			     (null parameters-job-offer)))       ;; job-offer not parameterized -> can still get parameterized
		    't)
		   (t
		    nil))))
  (:method (demand (job-offer job+store-offer))
	   ;; JOBS in the store could no longer satisfy new potential demands!
	   nil))
