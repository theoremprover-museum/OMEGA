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

(mod~defmod STORE 
            :uses (black demand job keim metar omega roc sod)
            :documentation "Stuff and data-structures for stores"
            :exports (
                      store+demands-store
                      store+job-offers-store
                      store+rocs-list-store
                      store+rocs-store
                      store+ssteps-list-store
                      store+store
                      
                      store~add-element!
                      store~add-elements!
                      store~call-metareasoner-on-job-offers!
                      store~clean-store!
                      store~create-demands-store
                      store~create-job-offers-store
                      store~create-rocs-list-store
                      store~create-rocs-store
                      store~create-ssteps-list-store
                      store~create-store
                      store~demands-store-p
                      store~demands-store-remove-fulfilled-demands!
                      store~demands-which-will-be-fulfilled-by-job
                      store~elements
                      store~elements-with-test
                      store~job-offers-store-p
                      store~print-job-offers
                      store~remove-element!
                      store~remove-elements!
                      store~rocs-list-store-p
                      store~rocs-store-p
                      store~ssteps-list-store-p
                      store~store-p
                      store~trigger-roc-state-descriptions!
                      ))


;; We first define a store as data-structe with a single slot: elements and some functions therefor!
;; This meands the store defined here consists simply of a list of things (not of a stack or whatever)! Nevertheless this can be
;; changed if necessary!
;; Then we define three subsorts of stores: roc-stores (to hold refinement operation call state description)
;;                                          demand-stores (to hold demands)
;;                                          job-offers-stores (to hold job-offers)
;;                               and relevant functions to these stores

#| ----------------------------------------------------------- Stores in general  ------------------------------------------------------ |#

(eval-when (load compile eval)
  (defclass store+store (keim+object)
    ((elements :initform nil
	       :initarg :elements
	       :accessor store=elements))))

(defun store~elements (store)
  (copy-list (store=elements store)))

(defsetf store~elements (store) (elements)
  `(setf (store=elements ,store) (copy-list ,elements)))

(defun store~store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+store, nil otherwise."))
  (typep obj 'store+store))


(defmethod print-object ((store store+store) stream)
  (format stream "[Store with the following elements: ~A]"
	  (store~elements store)))


(defun store~create-store (elements)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of elements.")
	   (effect  "None.")
	   (value   "A new store with elements."))
  (make-instance 'store+store
		 :elements elements))

(defun store~clean-store! (store)
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A store.")
	   (effect  "The elements slot of the store is set to nil.")
	   (value   "The store."))

  (setf (store~elements store) nil)
  store)

(defun store~add-element! (store element)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A store and an element.")
	   (effect  "Adds the elment at the end of the store's elements list.")
	   (value   "The store."))
  (setf (store~elements store) (append (store~elements store) (list element)))
  store)

(defun store~add-elements! (store element-list)
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A store and a list of new elements.")
	   (effect  "Adds the elements at the end of the store's elements list.")
	   (value   "The store."))
  (setf (store~elements store) (append (store~elements store) element-list))
  store)

(defun store~remove-element! (store element &key (test #'eq))
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A store, an element and a (binary) test function.")
	   (effect  "Removes all elements from store which fire with the input element and the test-function.")
	   (value   "The store."))

  (setf (store~elements store)
	(remove-if #'(lambda (item)
		       (apply test (list item element)))
		   (store~elements store)))

  store)

(defun store~remove-elements! (store element-list &key (test #'eq))
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A store, an element-list and a (binary) test function.")
	   (effect  "Removes all elements from store which fire with the input element-list and the test-function.")
	   (value   "The store."))
  
  (setf (store~elements store)
	(remove-if #'(lambda (item)
		       (some #'(lambda (ele)
				 (apply test (list item ele)))
			     element-list))
		   (store~elements store)))
  
  store)

(defun store~elements-with-test (store &key (test nil))
  (declare (edited  "14-JUN-1999")
	   (authors Ameier)
	   (input   "A store and a test function.")
	   (effect  "None.")
	   (value   "A list of all elements in the store wich fulfill the test function."))

  (let* ((elements (store~elements store)))

    (remove-if-not #'(lambda (element)
		       (apply test (list element)))
		   elements)))

#| ------------------------------------------------------------- ROCS STORES ---------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass store+rocs-store (store+store)
    ()))

(defun store~rocs-store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+rocs-store, nil otherwise."))
  (typep obj 'store+rocs-store))

(defmethod print-object ((store store+rocs-store) stream)
  (format stream "[A ROCS Store with the following ROCS: ~A]"
	  (store~elements store)))

(defun store~create-rocs-store (rocs)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of ROCS.")
	   (effect  "None.")
	   (value   "A new ROCS-store with ROCS."))
  (make-instance 'store+rocs-store
		 :elements rocs))

#|
(defgeneric store~trigger-roc-state-descriptions! (store)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A ROCS store.")
	   (effect  "For every ROC state-description of the store is checked whether its demands are"
		    "fulfilled (= are not longer on the control blackboard) and one of the first"
		    "tasks in the agenda carries this ROC description. For every such"
		    "state-description a job-offer is placed on the control-blackboard.")
	   (value   "Undefined."))
  (:method ((store store+rocs-store))
	   (let* ((rocs (store~elements store)))
	     
	     (mapcar #'(lambda (roc-stades)
			 (let* ((fulfilled (and (roc~demands-are-fulfilled-p roc-stades)
						(roc~first-task-in-agenda-carry-roc-p roc-stades))))
			   
			   (when fulfilled
			     (let* ((new-job-offer (job~create-store-offer roc-stades nil)))
			       (store~add-element! (black~get-blackboard-object-content 'job-offers sod*control-blackboard)
						   new-job-offer)))))
		     (reverse rocs)
		     ;; ROCS should be reverted because of the following reason:
		     ;; When adding elements to a store, younger ones are added at the end of the store's list
		     ;; That is without reverting the order of the rocs the oldest ROCS are triggered first, what
		     ;; means that they are in front of the job-offers. But typically (yes i know that's encoding
		     ;; of control knowledge) you want to continue on younger rocs first!
		     ))))|#

;; It turned out that the above version of store~trigger-roc-state-descriptions! is too restrictive. It forbids to reinvoke
;; job-offers whose demands are not satisfied.
;; The version below allows this!

(defgeneric store~trigger-roc-state-descriptions! (store)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A ROCS store.")
	   (effect  "For every ROC state-description of the store is checked whether one of the first"
		    "tasks in the agenda carries this ROC description. For every such"
		    "state-description a job-offer is placed on the control-blackboard.")
	   (value   "Undefined."))
  (:method ((store store+rocs-store))
	   (let* ((rocs (store~elements store)))
	     
	     (mapcar #'(lambda (roc-stades)
			 (let* ((new-job-offer (job~create-store-offer roc-stades nil)))
			   
			   (when (roc~first-task-in-agenda-carry-roc-p roc-stades)
			     (store~add-element! (black~get-blackboard-object-content 'job-offers sod*control-blackboard)
						 new-job-offer))))
		     (reverse rocs)
		     ;; ROCS should be reverted because of the following reason:
		     ;; When adding elements to a store, younger ones are added at the end of the store's list
		     ;; That is without reverting the order of the rocs the oldest ROCS are triggered first, what
		     ;; means that they are in front of the job-offers. But typically (yes i know that's encoding
		     ;; of control knowledge) you want to continue on younger rocs first!
		     ))))

#| ----------------------------------------------------------- DEMANDS STORES --------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass store+demands-store (store+store)
    ()))

(defun store~demands-store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+demands-store, nil otherwise."))
  (typep obj 'store+demands-store))

(defmethod print-object ((store store+demands-store) stream)
  (format stream "[A DEMANDS Store with the following DEMANDS: ~A]"
	  (store~elements store)))

(defun store~create-demands-store (demands)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of demands.")
	   (effect  "None.")
	   (value   "A new demands-store with demands."))
  (make-instance 'store+demands-store
		 :elements demands))

(defgeneric store~demands-store-remove-fulfilled-demands! (store obj)
  (declare (edited  "09-JUN-1999")
	   (authors Ameier)
	   (input   "A demands store, and either a termination-message or a state-description.")
	   (effect  "All demands which are fulfilled by the strategy-ks application"
		    "(stated by this proper termination-message or the state-description) are removed from the"
		    "elements list.")
	   (value   "Multiple-Value:"
		    "First: The store."
		    "Second: The remaning demands."
		    "Third: The deleted demands."))
  (:method ((store store+demands-store) (message exmes+exmes))
	   (let* ((demands (store~elements store))
		  (remaining-demands (remove-if #'(lambda (demand)
						    (demand~demand-is-fulfilled-by-termination-with-message-p demand message))
						demands))
		  (deleted-demands (remove-if-not #'(lambda (demand)
						      (demand~demand-is-fulfilled-by-termination-with-message-p demand message))
						  demands)))
	     
	     (setf (store~elements store) remaining-demands)
	     
	     (values store remaining-demands deleted-demands)))
  (:method ((store store+demands-store) (state-des roc+state-description))
	   (let* ((demands (store~elements store))
		  (remaining-demands (remove-if #'(lambda (demand)
						    (demand~demand-is-fulfilled-by-termination-of-state-des-p demand state-des))
						demands))
		  (deleted-demands (remove-if-not #'(lambda (demand)
						      (demand~demand-is-fulfilled-by-termination-of-state-des-p demand state-des))
						  demands)))
	     
	     (setf (store~elements store) remaining-demands)
	     
	     (values store remaining-demands deleted-demands))))
  
(defgeneric store~demands-which-will-be-fulfilled-by-job (store job-offer)
  (declare (edited  "14-JUN-1999")
	   (authors Ameier)
	   (input   "A demands store and a job-offer.")
	   (effect  "None.")
	   (value   "The list of all demands which will be fulfilled by the"
		    "job-offer (if terminated!)."))

  (:method ((store store+demands-store) job-offer)

	   (let* ((demands (store~elements store)))

	     (remove-if-not #'(lambda (demand)
				(demand~demand-will-be-fulfilled-by-job-offer-p demand job-offer))
			    demands))))

#| ----------------------------------------------------------- Job-offer STores ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass store+job-offers-store (store+store)
    ()))

(defun store~job-offers-store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+job-offers-store, nil otherwise."))
  (typep obj 'store+job-offers-store))

(defmethod print-object ((store store+job-offers-store) stream)
  (format stream "[A JOB-OFFERS Store with the following JOB-OFFERS: ~A]"
	  (store~elements store)))

(defun store~create-job-offers-store (job-offers)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of job-offers.")
	   (effect  "None.")
	   (value   "A new job-offers-store with job-offers."))
  (make-instance 'store+job-offers-store
		 :elements job-offers))

(defgeneric store~remove-except-start-strategy! (store strategy-symbol)
  (declare (edited  "08-AUG-2000")
	   (authors Ameier)
	   (input   "A job offer store and a strategy (symbol).")
	   (effect  "If the strategy-symbol corresponds to a strategy, all job-offers which are"
		    "using this strategy are removed from the store.")
	   (value   "Undefined."))
  (:method ((store store+job-offers-store) strategy-symbol)
	   (let* ((strategy (strat~find-strategy-ks strategy-symbol))
		  (job-offers (store~elements store))
		  (remaining-job-offers (remove-if-not #'(lambda (job-offer)
							   (if (and (job~strategy-ks-offer-p job-offer)
								    (eq (job~strategy-ks job-offer) strategy))
							       't
							     nil))
						       job-offers)))
	     (setf (store~elements store) remaining-job-offers))))


(defgeneric store~call-metareasoner-on-job-offers! (store)
  (declare (edited  "11-JUN-1999")
	   (authors Ameier)
	   (input   "A job offer store.")
	   (effect  "Calls the metareasoner on the job-offers in the store. Sets the elements slot of the store"
		    "on the result of the call of the metareasoner. Further all jobs are checked whether they"
		    "contain the parameters of the types demanded from the respective strategy-ks.")
	   (value   "Undefined."))
  
  (:method ((store store+job-offers-store))
	   (let* ((job-offers (store~elements store))
		  (new-job-offers (metar~reasoner job-offers))
		  (filtered-job-offers (remove-if-not #'(lambda (job-offer)
							  (let* ((test1 (job~job-offers-parameters-are-consistent-p job-offer))
								 ;; (test2 (job~job-offer-is-not-already-tried-p job-offer))
								 )
							    (when (and (null test1)
								       sod*verbose)
							      (omega~message "~%~A is rejected since its parameters are inconsistent with the parameter-types specified in its strategy-ks."
									     job-offer))
							    ;;(when (and (null test2)
							    ;;	       sod*verbose)
							    ;;  (omega~message "~%~A is rejected since it is contained in the already-apllied-strategy-ks slot of the task."
							    ;;		     job-offer))
							    
							    ;;(and test1 test2)))
							    test1))
						      new-job-offers)))    
	     
	     (setf (store~elements store)
		   filtered-job-offers))))
;; In an earlier Version the function store~call-metareasoner-on-job-offers! also tested whether a job-offer was already tried on a
;; task. In the newer Version this is declared as a control rule!


(defgeneric store~print-job-offers ((store store+job-offers-store))
  (declare (edited  "06-AUG-1999")
	   (authors Ameier)
	   (input   "A job-offer store.")
	   (effect  "Prints the job-offers on the store.")
	   (value   "Undefined."))
  (:method ((store store+job-offers-store))
	   (let* ((job-offers (store~elements store))
		  (job-strings (mapcar #'job~job-offer-print-string job-offers)))

	     (mapcar #'omega~message job-strings))))
  

#| ----------------------------------------------------------- ROCS-List Stores ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass store+rocs-list-store (store+store)
    ()))

(defun store~rocs-list-store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+rocs-list-store, nil otherwise."))
  (typep obj 'store+rocs-list-store))

(defmethod print-object ((store store+rocs-list-store) stream)
  (format stream "[A ROCS-List Store with the following ROCS: ~A]"
	  (store~elements store)))

(defun store~create-rocs-list-store (rocs-list)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of ROCS.")
	   (effect  "None.")
	   (value   "A new rocs-list-store with rocs-list."))
  (make-instance 'store+rocs-list-store
		 :elements rocs-list))

#| ----------------------------------------------------------- ROCS-List Stores ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass store+ssteps-list-store (store+store)
    ()))

(defun store~ssteps-list-store-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a store+ssteps-list-store, nil otherwise."))
  (typep obj 'store+ssteps-list-store))

(defmethod print-object ((store store+ssteps-list-store) stream)
  (format stream "[A Ssteps-list Store with the following ROCS: ~A]"
	  (store~elements store)))

(defun store~create-ssteps-list-store (ssteps-list)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of Strategy-ks steps.")
	   (effect  "None.")
	   (value   "A new ssteps-list store with ssteps-list."))
  (make-instance 'store+ssteps-list-store
		 :elements ssteps-list))
