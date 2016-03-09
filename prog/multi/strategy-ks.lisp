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



(mod~defmod STRAT 
            :uses (arg black job keim omega refalg sod store)
            :documentation "STuff and data-structures for strategy knowledge sources"
            :exports (
                      strat+strategy-ks
                      
                      strat~all-strategy-ks-in-hash-table
                      strat~all-strategy-ks-names-in-hash-table
                      strat~create-new-strategy-ks
                      strat~define-strategy-ks
                      strat~find-strategy-ks
                      strat~strategy-ks-condition
                      strat~strategy-ks-hash-table
                      strat~strategy-ks-p
                      strat~strategy-ks-parameter-types
                      strat~strategy-ks-print
                      strat~strategy-ks-refinement-algorithm
		      strat~strategy-ks-remark
                      strat~trigger-condition-against-task!
                      
                      strat*current-strategy-ks
                      strat*strategy-ks-hash-table))






;; This module contains the things for the strategy-ks. 


#| ------------------------------------------------------ global vars ---------------------------------------------------------------- |#

(defvar strat*strategy-ks-hash-table (make-hash-table))
;; A hash table to store all strategy-ks in

(defvar strat*current-strategy-ks nil)
;; To store the set currently usable strategy-ks in 

#| ------------------------------------------------------ Strategy-KS ----------------------------------------------------------------- |#

;; A strategy-KS consists in principle of a condition part and an action part (and a name under which it is stored in the
;; strat*strategy-ks-hash-table). In practice we not divide between the condition part and the action part, but specify directly the
;; parameters: a condition, and then the solts for the action part, namely a refinement-algorithm slot, in which the refinement-algorithm
;; object is stored, this strategy uses, and a hashtable in which the parameters and their values are entered the strategy specifies
;; for the refinement algorithm (only such parameters are allowed which are also given in the parameters-list of the
;; refinement-algorithm).

(eval-when (load compile eval)
  (defclass strat+strategy-ks (keim+name keim+object)
    ((condition :initform nil
		:initarg :condition
		:accessor strat~strategy-ks-condition)
     (refinement-algorithm :initform nil
			   :initarg :refinement-algorithm
			   :accessor strat~strategy-ks-refinement-algorithm)
     (print :initform nil
	    :initarg :print
	    :accessor strat~strategy-ks-print)
     (parameter-hashtable :initform nil
			  :initarg :parameter-hashtable
			  :accessor strat~strategy-ks-hash-table
			  :documentation "Here the parameters of the refinement algorithm are specified.")
     (parameter-types :initform nil
		      :initarg :parameter-types
		      :accessor strat~strategy-ks-parameter-types
		      :documentation "Here the args of the allowed parameters for a specific call of this strategy are specified.")
     (remark :initform nil
	     :initarg :remark
	     :accessor strat~strategy-ks-remark
	     :documentation "Here the remark for the lml browser is specified."))))

(defun strat~strategy-ks-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a strat+strategy-ks, nil otherwise."))
  (typep obj 'strat+strategy-ks))

(defmethod print-object ((strategy-ks strat+strategy-ks) stream)
  (format stream "><Strategy-ks ~A for Refinement Algorithm ~A with parameters of types ~A"
	  (keim~name strategy-ks)
	  (strat~strategy-ks-refinement-algorithm strategy-ks)
	  (strat~strategy-ks-parameter-types strategy-ks)))

(defun strat~find-strategy-ks (name)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The name of the strategy-ks.")
	   (effect  "None.")
	   (value   "Looks up the strat*strategy-ks-hash-table whether there exists an entry with this name"
		    "If this is the case this entry is returned, otherwise nil."))
  (gethash name strat*strategy-ks-hash-table))

(defgeneric strat~create-new-strategy-ks (name condition refinement-algorithm parameter-pair-list print parameter-types remark)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name, a condition, the refinement-algorithm and a list of pairs of parameter-names and contents"
		    "a print string for a strategy-ks and the parameter-types of the strategy.")
	   (effect  "A new strategy-ks with the given inputs is created and an entry is inserted into the"
		    "strat*strategy-ks-hash-table (existing ones with the same name are overwritten.)."
		    "Errors are procuced if the given refinement-algorithm does not exists or the names"
		    "of the parameters are not specified for the given refinement-algorithm.")
	   (value   "The new strategy-ks."))
  (:method (name condition (refinement-algorithm symbol) parameter-pair-list print parameter-types remark)
	   (let* ((refinement-algorithm-obj (refalg~find-refinement-algorithm-object refinement-algorithm)))
	     (if refinement-algorithm-obj
		 (strat~create-new-strategy-ks name condition refinement-algorithm-obj parameter-pair-list print parameter-types remark)
	       (omega~error "There exists no refinement algorithm with name ~A" refinement-algorithm))))
  (:method (name condition (refinement-algorithm refalg+refinement-algorithm) parameter-pair-list print parameter-types remark)
	   (let* ((allowed-parameter-names (refalg~parameter-list refinement-algorithm))
		  (new-hash-table (make-hash-table))
		  (new-strategy-ks (make-instance 'strat+strategy-ks
						  :name name
						  :condition condition
						  :refinement-algorithm refinement-algorithm
						  :parameter-hashtable new-hash-table
						  :print print
						  :parameter-types parameter-types
						  :remark remark
						  ))
		  (old-value (gethash name strat*strategy-ks-hash-table))) 
	     
	     (mapcar #'(lambda (parameter-pair)
			 (let* ((parameter-name (first parameter-pair))
				(parameter-content (second parameter-pair)))
			   (if (find parameter-name allowed-parameter-names)
			       (progn
				 (setq allowed-parameter-names (remove parameter-name allowed-parameter-names))
				 (setf (gethash parameter-name new-hash-table) parameter-content))
			     (omega~error "Parameter-name ~A not allowed for refinement-algorithm ~A or used double"
					  parameter-name refinement-algorithm))))
		     parameter-pair-list)

	     (mapcar #'(lambda (param-type)
			 (when (null (arg~find-argtype param-type))
			   (omega~error "Parameter type ~A is not a specified omega parameter type!"
					param-type)))
		     parameter-types)
	     
	     (when old-value
	       (omega~message "There exists already a strategy with name ~A, it is replaced by the new definition."
			      (keim~name old-value)
			      ))
	     
	     (setf (gethash name strat*strategy-ks-hash-table) new-strategy-ks)

	     new-strategy-ks)))

(defmacro strat~define-strategy-ks (&rest attribs)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A written respresentation of a strategy-ks.")
	   (effect  "Creates the new strategy-ks and adds to the strat*strategy-ks-hash-table.")
	   (value   "The new strategy-ks."))

  (do* ((rest-attribs attribs (rest rest-attribs))
	(found-name nil)
	(found-refinement-algorithm nil)
	(found-parameters nil)
	(found-condition nil)
	(found-print nil)
	(found-parameter-types nil)
	(found-remark nil)
	)
      ((null rest-attribs)
       (if (and found-name
		found-refinement-algorithm
		found-condition
		)
	   `(strat~create-new-strategy-ks ',found-name ',found-condition ',found-refinement-algorithm
					  ',found-parameters ',found-print ',found-parameter-types ',found-remark)
	 (omega~error "Wrong specification of strategy-ks, at least name and refinement-algorithm have to be specified.")))
    (let* ((head-attrib (first rest-attribs)))
      (if (null (listp head-attrib))
	  (omega~error "Each attribute of the strategy-ks definition has to be a pair, ~A is unfortunately not.")

	(let* ((car-attrib (first head-attrib))
	       (second-attrib (second head-attrib)))

	  (cond ((string-equal car-attrib 'parameter-types)
		 (setq found-parameter-types second-attrib))
		((string-equal car-attrib 'name)
		 (setq found-name second-attrib))
		((string-equal car-attrib 'refinement-algorithm)
		 (setq found-refinement-algorithm second-attrib))
		((string-equal car-attrib 'condition)
		 (setq found-condition second-attrib))
		((string-equal car-attrib 'print)
		 (setq found-print second-attrib))
		((string-equal car-attrib 'remark)
		 (setq found-remark (rest head-attrib)))
		(t
		 (setq found-parameters (append found-parameters (list head-attrib))))))))))


(defun strat~all-strategy-ks-in-hash-table ()
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "A list of all strategy-ks in strat*strategy-ks-hash-table.")) 
  
  (let* ((values nil))
    (maphash #'(lambda (key value)
		 (setq values (cons value values)))
	     strat*strategy-ks-hash-table)
    values))

(defun strat~all-strategy-ks-names-in-hash-table ()
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "A list of all the names (symbols) of the strategy-ks in"
		    "strat*strategy-ks-hash-table.")) 
  (let* ((names nil))
    (maphash #'(lambda (key value)
		 (setq names (cons key names)))
	     strat*strategy-ks-hash-table)
    names))

#| ---------------------------------------------------- Triggering Strategy-KS -------------------------------------------------------- |#

(defgeneric strat~trigger-condition-against-task! (strategy-ks task)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A strategy-ks and a task.")
	   (effect  "If the condition part of the strategy-ks fires on the task (and on the current pds in general)"
		    "then a job-offer of this strategy is placed on the control blackboard.")
	   (value   "Undefined."))
  (:method ((strategy-ks symbol) task)
	   (let* ((strat-obj (strat~find-strategy-ks strategy-ks)))
	     (if strat-obj
		 (strat~trigger-condition-against-task! strat-obj task)
	       (omega~error "Unknown strategy ~A" strategy-ks))))
  (:method ((strategy-ks strat+strategy-ks) task)
	   (let* ((condition (strat~strategy-ks-condition strategy-ks))
		  (match (apply condition (list task))))
	     
	     (when match
	       (let* ((new-job-offer (job~create-strategy-ks-offer strategy-ks task nil)))
		 (store~add-element! (black~get-blackboard-object-content 'job-offers sod*control-blackboard)
				     new-job-offer))))))
						       

















