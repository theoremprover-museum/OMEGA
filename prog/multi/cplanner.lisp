;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ana-main.lisp; This file is part of the OMEGA system
;;
;; major updates: 30.04.2002
;; 
;;
;; Authors: Siggi Scholl
;; email: scholl@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; the main algorithm for analogy

(in-package "OMEGA")

(mod~defmod ANA 
            :uses (agenda beta cri data env infer just keim lam meth node omega pds pdsc pdsj pdsn plan prob subst)
            :documentation "The analogy algorithm"
            :exports (
                      ))

(defun ana~transfer (roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "Evaluates the transfer procedures and return a message from the result"))
  ;; complete last action
  (let ((current-action (roc~analogy-current-action roc)))
    (when current-action
      (ana~action-update-correspondence current-action roc)
      (decf ana*indent)
      (ana~message "*******************************************************************************************************")))
  ;; check termination condition
  (let ((superstep (roc~analogy-source-superstep roc)))
    (ana~message "Local tasks: ~A" (ana~local-tasks))
    (if (or (null (ana~local-tasks))
	    (and (not superstep) (funcall (ana=ref-parameter 'termination-condition roc))))
	(exmes~create-termination-message roc nil)
      ;; call cycle algorithms
      (let ((result (ana~try-cycle-algs roc)))
	(if (ana~matching-p result)
	    ;; matching found
	    ;; test for special matching
	    (if (and superstep (not (ana~step-special-matching-p superstep result)) (funcall (ana~step-end-condition superstep)))
		(exmes~create-termination-message roc nil)
	      (progn
		(ana~trace "Applicable matching: ~A" result)
		(ana~message "*******************************************************************************************************")
		(incf ana*indent)
		(let ((action (ana~matching-apply result roc)))
		  (setf (roc~analogy-current-action roc) action)
		  (roc~analogy-append-action roc action)
		  (if (ana~action-demand-p action)
		      ;; action is demand
		      (exmes~create-interruption-message roc (list (ana~action-demand action)) nil)
		    ;; action is pstep
		    (ana~transfer roc)))))
	  ;; failure
	  (if (or (and superstep (ana~step-failed-p superstep))
		  (and superstep (funcall (ana~step-end-condition superstep))))
	      (exmes~create-termination-message roc nil)
	    (progn
	      (ana~message "Analogy failed:")
	      (ana~failure-print result)
	      (setf (roc~analogy-current-action roc) nil)
	      (exmes~create-failure-message roc (list 'analogy result)))))))))

(defun ana~interactive (roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An ananlogy state description")
	   (effect  "Asks the user for the strategy parameters")
	   (value   "Undefined"))
  (let* ((source-plans (ana~source-plans))
	 (source-plan (nth (omega~query-choice "~%Choose the source plan:" (mapcar 'keim~name source-plans) 0) source-plans))
	 (poss (list "Flat source steps" "Hierarchical source steps"))
	 (poss-number (omega~query-choice "~%Choose the source steps:" poss 0))
	 (steps (cond ((= 0 poss-number)
		       (ana~flat-steps source-plan))
		      ((= 1 poss-number)
		       (ana~hierarchical-steps source-plan))
		      (t nil))))
    (setf (roc~parameters roc) (list (list (roc~start-task roc)) source-plan steps nil))))

(defun ana~invokation (strategy-ks task parameters)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy, a task and a list of parameters")
	   (effect  "Changes the proof plan")
	   (value   "Applies the strategy to task and returns the message"))
  (let* ((strat-parameter-hashtable (strat~strategy-ks-hash-table strategy-ks))
	 ;; neue Analogy ROC-State-Description 
	 (roc (roc~fresh-analogy-state-description strategy-ks task nil parameters))
	 ;; Neue Start-strategy-ks Justification
	 (new-start-strategy-ks-application-just (strat~start-strategy-ks-application-create-just (list roc)))
	 ;; Neuer Strategy-step aus dieser Justification
	 (start-step (scon~create-scon-step nil nil new-start-strategy-ks-application-just)))

    ;; Roc bekommt start-step als Eintrag
    (setf (roc~start-step roc) start-step)

    ;; ROC wird in ROCS-list eingetragen
    (store~add-element! (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard) roc)
    
    ;; Strategy-step wird in Strategy-steps eingefuegt
    (sod~introduce-new-strategy-ks-step! start-step)
    
    (when (and (ana~root-task-p task) (null (first parameters)))
      (setf ana*indent 0))
    ;;(ana~trace "~%Invoking ANALOGY WITH STRATEGY ~A on TASK ~A" strategy-ks task)
    
    (when (null (roc~analogy-source-plan roc)) (ana~interactive roc))
    (ana~trace "Source plan: ~A" (roc~analogy-source-plan roc))
    (ana~trace "Steps: ~A" (roc~analogy-source-steps roc))
        
    (roc~analogy-table-init roc (roc~analogy-tasks roc))
    
    (setf ana*roc-state-description roc)
    (setf pplan*roc-state-description roc)
    (ana~transfer roc)))

(defun ana~reinvokation (roc parameters)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "Changes the pds")
	   (value   "Reinvokes the strategy application and return the message"))
  (let* (;; New Reinvoke-strategy-ks Justification
	 (new-reinvoke-strategy-ks-application-just
	  (strat~reinvoke-strategy-ks-application-create-just (list roc (roc~copy-state-description roc))))
	 ;; New Strategy-step from this Justification
	 (reinvoke-step (scon~create-scon-step nil nil new-reinvoke-strategy-ks-application-just)))
    
    (when parameters
      (setf (roc~parameters roc) parameters))
    
    ;; Strategy-step is inserted in strategy-steps
    (sod~introduce-new-strategy-ks-step! reinvoke-step)
    
    ;;(ana~trace "~%ReInvoking ANALOGY WITH ROC STATE Description ~A" roc)
    ;;(ana~trace "~%DEMANDS IN ROC STATE Description are set to nil")
    
    (setf (roc~demands roc)
	  nil)
    
    (setf ana*roc-state-description roc)
    (setf pplan*roc-state-description roc)
    (ana~transfer roc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Evaluation of transfer procedures}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana~cycle-alg-select (roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "Evaluates the control rule of the strategy and returns the selected transfer procedures"))
  (let ((crules (ana=ref-parameter 'crules roc))
	(cycle-algs (ana=ref-parameter 'cycle-algs roc)))
    (cri~call cycle-algs :kind :analogy :crules crules)))

(defun ana~try-set (set &optional (choose-sub-choice #'(lambda (m) m)) &key ((:failures failures))
			((:classes classes)) ((:parameters parameters)))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A set, optional a function, a list of failures, a list of failure classes and a list of failure parameters")
	   (effect  "Indentation of output messages")
	   (value   "Calls the function on the members of the set until the result is not a failure or there are no furhter"
		    "elements in the set. Return the result or the accumulated failures."))
  (if set
      (let ((result (prog2
			(incf ana*indent)
			(funcall choose-sub-choice (first set))
		      (decf ana*indent))))
	(if (ana~failure-p result)
	    (ana~try-set (rest set) choose-sub-choice :failures (append failures (list result))
			 :classes classes :parameters parameters)
	  result))
    (ana~failure-create classes parameters failures)))

(defun ana~cycle-alg-pattern-name (cycle-alg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A transfer procedure")
	   (effect  "None")
	   (value   "The name of the pattern of the transfer procedure"))
  (second cycle-alg))

(defun ana~cycle-alg-pattern (cycle-alg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A transfer procedure")
	   (effect  "Outputs an error message if the pattern does not exist")
	   (value   "The pattern of the transfer procedure, if it exists, otherwise nil"))
  (let ((pattern (ana~find-pattern (ana~cycle-alg-pattern-name cycle-alg))))
    (if pattern pattern
      (omega~error "Pattern ~A not found" (ana~cycle-alg-pattern-name cycle-alg)))))

(defun ana~cycle-alg-search-alg (cycle-alg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A transfer procedure")
	   (effect  "None")
	   (value   "The search algorithm of the transfer procedure"))
  (first cycle-alg))

(defun ana~cycle-alg-choice-controls (cycle-alg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A transfer procedure")
	   (effect  "None")
	   (value   "The list of choice controls of the transfer procedure"))
  (third cycle-alg))

(defun ana~cycle-alg-call (cycle-alg roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A transfer procedure and an analogy state description")
	   (effect  "None")
	   (value   "Evaluates the transfer procedure and returns the result or a failure"))
  (let ((result (funcall (ana~cycle-alg-search-alg cycle-alg)
			 (ana~matching-create (ana~cycle-alg-pattern cycle-alg) roc)
			 (ana~cycle-alg-choice-controls cycle-alg) roc)))
    (if (ana~failure-p result)
	(ana~failure-create 'cycle-alg cycle-alg result)
      result)))

(defun ana~try-cycle-algs (roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "Evaluates the selected transfer procedures of the strategy until one return no failure or all failed. Returns the"
		    "result or the accumulation of all failures"))
  (ana~try-set (ana~cycle-alg-select roc)
	       #'(lambda (cycle-alg)
		   (decf ana*indent)
		   (ana~trace ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
		   (ana~trace "Trying cycle algorithm for pattern ~A" (ana~cycle-alg-pattern-name cycle-alg))
		   (incf ana*indent)
		   (ana~cycle-alg-call cycle-alg roc))
	       :classes 'no-analogy-matching :parameters roc))

(defun ana~cycle-alg-choice-control-name (choice-control)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A choice control")
	   (effect  "None")
	   (value   "The name of the choice control"))
  (if (listp choice-control) (first choice-control) choice-control))

(defun ana~cycle-alg-choice-control-args (choice-control)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A choice control")
	   (effect  "None")
	   (value   "The arguments of the choice control"))
  (when (listp choice-control) (rest choice-control)))

(defun search-dfs (matching choice-controls roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching, a list of choice controls and an analogy state description")
	   (effect  "None")
	   (value   "Performs a depth first search on the tree spanned by the choice controls. Returns the matching or a failure"))
  (if (null choice-controls) matching
    (let* ((cycle-alg-choice-control (first choice-controls))
	   (control-name (ana~cycle-alg-choice-control-name cycle-alg-choice-control))
	   (control-args (ana~cycle-alg-choice-control-args cycle-alg-choice-control))
	   (choice-control (ana~find-choice-control control-name))
	   (choice-point (ana~choice-control-point choice-control)))
      (ana~try-set
       (ana~matching-choose matching choice-control control-args roc)
       #'(lambda (new-matching)
	   (let ((choice (ana~matching-get-choice new-matching choice-point)))
	     (decf ana*indent)
	     (ana~trace "Trying ~A..." choice)
	     (incf ana*indent)
	     (ana~matching-test-all new-matching)
	     (let ((result (if (ana~failure-p (ana~matching-status new-matching))
			       (ana~matching-status new-matching)
			     (search-dfs new-matching (rest choice-controls) roc))))
	       (if (ana~failure-p result)
		   (ana~failure-create 'choice choice result)
		 result))))
       :classes 'choice-point :parameters (list choice-point choice-control)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Analogy Patterns}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ana*pattern-hashtable (make-hash-table :test #'equal))

(defclass ana+pattern (keim+name)
  ((choice-points                   :initarg :choice-points
				    :initform nil
				    :accessor ana~pattern-choice-points)
   (status-init-function            :initarg :status-init-function
				    :initform nil
				    :accessor ana~pattern-status-init-function)
   (status-copy-function            :initarg :status-copy-function
				    :initform nil
				    :accessor ana~pattern-status-copy-function)
   (applicability-tests             :initarg :applicability-tests
				    :initform nil
				    :accessor ana~pattern-applicability-tests)
   (application-function            :initarg :application-function
				    :initform nil
				    :accessor ana~pattern-application-function))
  (:documentation "Objects of this class represent a pattern for instantiiating and applying an action"))

(defmethod ana~pattern-find-choice-point ((pattern ana+pattern) choice-point)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern and a choice point")
	   (effect  "None")
	   (value   "True, iff the pattern contains the choice point"))
  (find choice-point (ana~pattern-choice-points pattern)))

(defmacro ana~defpattern (name &rest attribs)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A name, and a list of properties")
	   (effect  "Creates a pattern object given by the name and properties and adds it to the pattern hash table")
	   (value   "Undefined"))
  `(block defpattern
     (let ((choice-points)
	   (status-init-function)
	   (status-copy-function)
	   (applicability-tests)
	   (application-function))
       (dolist (attrib ',attribs)
	 (cond ((string-equal (first attrib) :choice-points) (setf choice-points (rest attrib)))
	       ((string-equal (first attrib) :status-init-function) (setf status-init-function (second attrib)))
	       ((string-equal (first attrib) :status-copy-function) (setf status-copy-function (second attrib)))
	       ((string-equal (first attrib) :applicability-tests) (setf applicability-tests (rest attrib)))
	       ((string-equal (first attrib) :application-function) (setf application-function (second attrib)))
	       (t (return-from defpattern (omega~error ";;;ANA~~DEFPATTERN: Not expecting ~A" (first attrib))))))
       (let ((pattern (make-instance 'ana+pattern :name ',name :choice-points choice-points 
				     :status-init-function status-init-function
				     :status-copy-function status-copy-function
				     :applicability-tests applicability-tests
				     :application-function application-function)))
	 (when (gethash (symbol-name ',name) ana*pattern-hashtable)
	   (omega~warn "Redeclaring pattern ~A" ',name))
	 (setf (gethash (symbol-name ',name) ana*pattern-hashtable) pattern)
	 ;;(omega~message "The following pattern was declared: ~%~A" pattern)))))
	 ))))
	 
(defun ana~find-pattern (name)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A name")
	   (effect  "None")
	   (value   "The pattern object looked up in the hash table"))
  (gethash (symbol-name name) ana*pattern-hashtable))

(defmethod ana~pattern-tests ((pattern ana+pattern))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern")
	   (effect  "None")
	   (value   "The names of the applicability tests of the pattern"))
  (mapcar 'first (ana~pattern-applicability-tests pattern)))

(defmethod ana~pattern-test-exists-p ((pattern ana+pattern) test)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern and a name of a test")
	   (effect  "None")
	   (value   "True, iff the pattern contains a test with that name"))
  (find test (ana~pattern-tests pattern)))

(defmethod ana~pattern-test-arguments ((pattern ana+pattern) test)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern and a name of a test")
	   (effect  "None")
	   (value   "The arguments of the test, if the pattern contains it, otherwise nil"))
  (second (find-if #'(lambda (decl) (eq (first decl) test)) (ana~pattern-applicability-tests pattern))))

(defmethod ana~pattern-test-applicability ((pattern ana+pattern) test mapp status)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern, a test, a mapping and a status")
	   (effect  "None")
	   (value   "The new status, if the test is successful with the mapping, otherwise a failure"))
  (ana~trace "Testing applicability by ~A" test)
  (let* ((arg-names (ana~pattern-test-arguments pattern test))
	 (args (mapcar #'(lambda (arg) (mapp~get-component arg mapp)) arg-names)))
    (ana~trace "Arguments: ~A" args)
    (let ((result (apply test (cons status (cons (mapp~restrict-mapping mapp arg-names) args)))))
      (if (not (ana~failure-p result))
	  (ana~trace "Test successful.")
	(ana~trace "Test failed: ~A" result))
      result)))

(defmethod ana~pattern-apply ((pattern ana+pattern) mapp status)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern, a mapping and a status")
	   (effect  "None")
	   (value   "Applies the pattern with the mapping and the status and returns the applied step"))
  (ana~message "Applying pattern ~A..." (keim~name pattern))
  ;;(ana~trace "Using matching status ~A." status)
  (let ((result (apply (ana~pattern-application-function pattern)
		       (cons status (cons mapp (mapcar #'(lambda (arg) (mapp~get-component arg mapp))
						       (ana~pattern-choice-points pattern)))))))
    (ana~message "..pattern ~A applied." (keim~name pattern))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Analogy Matching}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ana+matching (keim+object)
  ((pattern                           :initarg :pattern
				      :initform nil
				      :accessor ana~matching-pattern)
   (mapp                              :initarg :mapp
				      :initform (mapp~create nil nil)
				      :accessor ana~matching-mapp)
   (status                            :initarg :status
				      :initform nil
				      :accessor ana~matching-status)
   (tests-made                        :initarg :tests-made
				      :initform nil
				      :accessor ana~matching-tests-made)
   (choice-control-and-source         :initarg :choice-control-and-source
				      :initform nil
				      :accessor ana~matching-choice-control-and-source))
  (:documentation "Objects of this class represent a matching for a pattern"))

(defun ana~matching-p (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is a matching"))
  (typep obj 'ana+matching))

(defun ana~matching-mapp-create (roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A state description")
	   (effect  "None")
	   (value   "Creates a mapping with the parameters of the strategy"))
  (mapp~create (roc~analogy-parameter-names roc)
	       (roc~analogy-parameters roc)))

(defun ana~matching-create (pattern state-des)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pattern and a state description")
	   (effect  "None")
	   (value   "Creates a new matching and returns it"))
  (let ((matching (make-instance 'ana+matching
				 :mapp (ana~matching-mapp-create state-des)
				 :pattern pattern)))
    (setf (ana~matching-status matching) (funcall (ana~pattern-status-init-function pattern)))
    matching))

(defmethod keim~equal ((matching1 ana+matching) (matching2 ana+matching))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "The matchings")
	   (effect  "None")
	   (value   "True, iff the mapping of the matchings are equal"))
  (keim~equal (ana~matching-mapp matching1) (ana~matching-mapp matching2)))

(defmethod print-object ((matching ana+matching) stream)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching and a stram")
	   (effect  "Writes the matching to the stream")
	   (value   "Undefined"))
  (format stream "~%Matching for ~A" (ana~matching-pattern matching))
  ;;(format stream "~%Choices: ~A" (ana~matching-mapp matching))
  (format stream "~%Status: ~A" (ana~matching-status matching))
  ;;(format stream "Choice control: ~A~%" (ana~matching-choice-control-and-source matching))
  ;;(format stream "Correspondence control: ~A~%" (ana~matching-correspondence-control-and-source matching))
  )

(defmethod ana~matching-get-choice ((matching ana+matching) choice-point)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching and a choice point")
	   (effect  "None")
	   (value   "The instantiation of the choice point in the matching"))
  (mapp~get-component choice-point (ana~matching-mapp matching)))

(defmethod ana~matching-source ((matching ana+matching))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching")
	   (effect  "None")
	   (value   "The source of the matching"))
  (ana~matching-get-choice matching 'source))

(defmethod ana~matching-method ((matching ana+matching))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching")
	   (effect  "None")
	   (value   "The method of the matching"))
  (ana~matching-get-choice matching 'method))

(defmethod ana~matching-extended-copy ((matching ana+matching) choice-point choice)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching, a choice point and a choice")
	   (effect  "None")
	   (value   "Extends the matching with the choice for the choice point and returns the extended matching"))
  (make-instance 'ana+matching
		 :pattern (ana~matching-pattern matching)
		 :mapp (mapp~insert-component choice-point choice (ana~matching-mapp matching))
		 :status (funcall (ana~pattern-status-copy-function (ana~matching-pattern matching))
				  (ana~matching-status matching))
		 :tests-made (ana~matching-tests-made matching)
		 ))
  
(defmethod ana~matching-choose ((matching ana+matching) control control-args state-des)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching, a choice control, control arguments and an analogy state description")
	   (effect  "None")
	   (value   "Return a list of new matchings, each one extended with one alternative computed by the choice control"))
  (let ((choice-point (ana~choice-control-point control))
	(conflict-set (ana~choose control (ana~matching-mapp matching) state-des control-args)))
    (ana~trace "Choosing choice point ~A:" choice-point)
    (ana~trace "Conflict set: ~A" conflict-set)
    (mapcar #'(lambda (choice) (ana~matching-extended-copy matching choice-point choice))
	    conflict-set)))

(defmethod ana~matching-test ((matching ana+matching) test)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching and the name of an applicability test")
	   (effect  "Changes the status of the matching by applying the test")
	   (value   "Undefined"))
  (let ((pattern (ana~matching-pattern matching))
	(mapp (ana~matching-mapp matching)))
    (when (and (not (ana~failure-p (ana~matching-status matching)))
	       (not (find test (ana~matching-tests-made matching)))
	       (subsetp (ana~pattern-test-arguments pattern test) (mapp~domain mapp)))
      (setf (ana~matching-tests-made matching) (append (ana~matching-tests-made matching) (list test)))
      (setf (ana~matching-status matching) (ana~pattern-test-applicability pattern test mapp (ana~matching-status matching))))))

(defmethod ana~matching-test-all ((matching ana+matching))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching")
	   (effect  "Changes the status of the matching by applying all possible tests")
	   (value   ""))
  (mapc #'(lambda (test) (ana~matching-test matching test)) (ana~pattern-tests (ana~matching-pattern matching))))

(defmethod ana~matching-apply ((matching ana+matching) state-des)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching and an analogy state description")
	   (effect  "None")
	   (value   "Applies the matching and returns the created action"))
  (let ((steps (ana~pattern-apply (ana~matching-pattern matching) (ana~matching-mapp matching) (ana~matching-status matching))))
    (ana~action-create matching steps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Choice Control}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ana*choice-control-hashtable (make-hash-table :test #'equal))

(defclass ana+choice-control (keim+name)
  ((choice-point                        :initarg :choice-point
					:initform nil
					:accessor ana~choice-control-point)
   (dependencies                        :initarg :dependencies
					:initform nil
					:accessor ana~choice-dependencies)
   (conflict-set-computation-function   :initarg :conflict-set-computation-function
					:initform nil
					:accessor ana~choice-conflict-set-computation-function)
   (crules-computation-function         :initarg :crules-computation-function
					:initform nil
					:accessor ana~choice-crules-computation-function))
  (:documentation "Objects of this class represent a choice control for a pattern"))

(defmacro ana~defchoicecontrol (name choice-point &rest attribs)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A name, the name of a choice point and some properties")
	   (effect  "Creates a new choice control object and inserts it in a hash table")
	   (value   "Undefined"))
  `(block defchoicecontrol
     (let ((dependencies)
	   (conflict-set-computation-function)
	   (crules-computation-function))
       (dolist (attrib ',attribs)
	 (cond ((string-equal (first attrib) :dependencies) (setf dependencies (rest attrib)))
	       ((string-equal (first attrib) :conflict-set-computation-function) (setf conflict-set-computation-function (second attrib)))
	       ((string-equal (first attrib) :crules-computation-function) (setf crules-computation-function (second attrib)))
	       (t (return-from defchoicecontrol (omega~error ";;;ANA~~DEFCHOICECONTROL: Not expecting ~A" (first attrib))))))
       (let ((choice-control (make-instance 'ana+choice-control :name ',name :choice-point ',choice-point
					    :dependencies dependencies
					    :conflict-set-computation-function conflict-set-computation-function
					    :crules-computation-function crules-computation-function)))
	 (when (gethash (symbol-name ',name) ana*choice-control-hashtable)
	   (omega~warn "Redeclaring choice control ~A" ',name))
	 (setf (gethash (symbol-name ',name) ana*choice-control-hashtable) choice-control)
	 ;;(omega~message "The following choice control was declared: ~%~A" choice-control)))))
	 ))))

(defun ana~find-choice-control (name)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "The name of a choice control")
	   (effect  "None")
	   (value   "The choice control looked up in the hash table"))
  (gethash (symbol-name name) ana*choice-control-hashtable))

(defmethod ana~choice-get-dependencies ((cc ana+choice-control) mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A choice control and a mapping")
	   (effect  "None")
	   (value   "A list of the instantiations of the dependent choice points"))
  (mapcar #'(lambda (dep) (mapp~get-component dep mapp)) (ana~choice-dependencies cc)))

(defmethod ana~choose ((cc ana+choice-control) mapp state-des &optional control-args)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A choice control, a mapping, a state description and optionally a list of arguments")
	   (effect  "None")
	   (value   "The alternatives for the choice point given by the choice control"))
  (let* ((dependencies (ana~choice-get-dependencies cc mapp))
	 (args (append (list state-des mapp) control-args dependencies))
	 (conflict-set (apply (ana~choice-conflict-set-computation-function cc) args))
	 (crules (apply (ana~choice-crules-computation-function cc) args)))
    (if crules (cri~call conflict-set :kind :analogy :crules crules) conflict-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Analogy Action}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ana+action (keim+object)
  ((matching                            :initarg :matching
					:initform nil
					:accessor ana~action-matching)
   (result                              :initarg :result
					:initform nil
					:accessor ana~action-result))
  (:documentation "Objects of this class represent an action created by the application of a matching"))

(defun ana~action-p (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is an action"))
  (typep obj 'ana+action))

(defmethod print-object ((action ana+action) stream)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action and a stream")
	   (effect  "Writes the action to the stream")
	   (value   "Undefined"))
  (format stream "(Action ~A)" (ana~action-result action)))

(defun ana~action-create (matching result)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A matching and the application result")
	   (effect  "None")
	   (value   "A new instance of an action"))
  (make-instance 'ana+action
		 :matching matching
		 :result result))

(defmethod ana~action-psteps ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The step, if it is a mehtod action, otherwise nil"))
  (if (ana~action-demand-p action) nil
    (ana~action-result action)))

(defmethod ana~action-steps ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The first segment of the strategy application or the step of the method application"))
  (if (ana~action-demand-p action)
      (find-if #'(lambda (seg) (ana~action-fulfilled-by-p action seg))
	       (ana~segs omega*current-proof-plan))
    (ana~action-result action)))

(defmethod ana~action-source ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The source of the action"))
  (ana~matching-source (ana~action-matching action)))

(defmethod ana~action-demand-p ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "True, iff the action is a demand"))
  (demand~demand-p (ana~action-result action)))

(defmethod ana~action-demand ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An Action")
	   (effect  "None")
	   (value   "The demand of the action"))
  (ana~action-result action))

(defmethod ana~action-demand-strategy ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The strategy of the demand of the action"))
  (demand~strategy-task-demand-strategy-ks (ana~action-demand action)))

(defmethod ana~action-demand-task ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The task of the demand of the action"))
  (demand~strategy-task-demand-task (ana~action-demand action)))

(defmethod ana~action-demand-parameters ((action ana+action))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action")
	   (effect  "None")
	   (value   "The parameters of the demand of the action"))
  (demand~parameters (ana~action-demand action)))

(defmethod ana~action-fulfilled-by-p ((action ana+action) step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action and a step")
	   (effect  "None")
	   (value   "True, iff the action corresponds to the step"))
  (cond ((and (ana~action-demand-p action) (ana~seg-invoke-p step))
	 (when (and (keim~equal (ana~action-demand-strategy action) (ana~seg-strategy step))
		    (keim~equal (ana~action-demand-task action) (ana~seg-task step))
		    (keim~equal (ana~action-demand-parameters action) (ana~seg-parameters step)))
	   t))
	((and (ana~action-psteps action) (ana~pstep-p step))
	 (when (keim~equal (ana~action-psteps action) step)
	   t))
	(t nil)))

(defmethod ana~action-already-applied-p ((action ana+action) mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action and a mapping")
	   (effect  "None")
	   (value   "True, iff the mapping is a sub mapping of the mapping of the action"))
  (let ((action-mapp (ana~matching-mapp (ana~action-matching action))))
    (keim~equal mapp (mapp~restrict-mapping action-mapp (mapp~domain mapp)))))

(defmethod ana~action-update-correspondence ((action ana+action) roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An action and an analogy state description")
	   (effect  "Updates the correspondence table of the state description with the new correspondences given by the action")
	   (value   "Undefined"))
  (let ((source (ana~action-source action))
	(target (ana~action-steps action)))
    (ana~cor-add-pair (roc~analogy-table roc) (list (ana~step-add-premises source) (ana~step-add-premises target)))
    (ana~cor-add-pair (roc~analogy-table roc) (list (ana~step-add-conclusions source) (ana~step-add-conclusions target)))
    (ana~cor-add-pair (roc~analogy-table roc) (list (ana~step-add-hyps source) (ana~step-add-hyps target)))
    (ana~cor-add-pair (roc~analogy-table roc) (list (ana~step-new-metavars source) (ana~step-new-metavars target))))
  (ana~cor-print (roc~analogy-table roc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Failures}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ana+failure (keim+object)
  ((classes                   :initarg :classes
			      :initform nil
			      :accessor ana~failure-classes)
   (parameters                :initarg :parameters
			      :initform nil
			      :accessor ana~failure-parameters)
   (sub-failures              :initarg :sub-failures
			      :initform nil
			      :accessor ana~failure-sub-failures))
  (:documentation "Objects of this class represent a failure categorized by a list of classes and spezified by a list of parameters."
		  "A failure can contain a list of sub failures."))


(defun ana~failure-p (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is a failure"))
  (typep obj 'ana+failure))

(defun ana~failure-list-p (objs)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is a list of failures"))
  (and (listp objs) (some 'ana~failure-p objs)))

(defun ana~failure-create (classes parameters &optional sub-failures)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of failure categories, a list of parameters and optionally a list of sub failures")
	   (effect  "None")
	   (value   "A failure object created from the input"))
  (make-instance 'ana+failure
		 :classes (if (listp classes) classes (list classes))
		 :parameters parameters
		 :sub-failures (if (listp sub-failures) sub-failures (list sub-failures))))

(defmethod ana~failure-print ((failure ana+failure))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A failure")
	   (effect  "Outputs the failure")
	   (value   "Undefined"))
  (ana~message "~A" failure)
  (when (ana~failure-sub-failures failure)
    (incf ana*indent)
    (mapc #'(lambda (sub-failure) (ana~failure-print sub-failure)) (ana~failure-sub-failures failure))
    (decf ana*indent)))
  
(defmethod print-object ((failure ana+failure) stream)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A failure and a stream")
	   (effect  "Writes the failure to the stream")
	   (value   "Undefined"))
  (let ((classes (ana~failure-classes failure)))
    (format stream "~A: ~A" (if (second classes) classes (first classes)) (ana~failure-parameters failure))))

(defmethod ana~failure-of-class-p ((failure ana+failure) class)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A failure and a category")
	   (effect  "None")
	   (value   "True, iff the failure or a sub failure belongs to the category"))
  (or (find class (ana~failure-classes failure))
      (some #'(lambda (sub-failure) (ana~failure-of-class-p sub-failure class)) (ana~failure-sub-failures failure))))

(defmacro ana~failure-when-not (test statement)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A test and a statement")
	   (effect  "None")
	   (value   "Evaluates the test. If the evaluation returns a failure, the failure is returned, otherwise the statement is executed"))
  `(let ((result ,test))
     (if (ana~failure-p result)
	 result
       ,statement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Substeps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana~just-meth (justification)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A node justification")
	   (effect  "None")
	   (value   "The method of the justification"))
  (if (or (infer~supermethod-p (just~method justification))
	  (infer~method-p (just~method justification)))
      (if (and (eq (keim~name (just~method justification)) 'simplifyinequality-m) ;; wird auch in 4.1.10.a gebraucht
	       (string= (first (pdsj~outline-pattern justification)) "EXISTENT"))
	  (meth~find-method 'simplifyinequality-m-b)
	(pds~inference-application (just~method justification)
				   (mapcar #'(lambda (pattern) (if (stringp pattern) pattern "NONEXISTENT"))
					   (pdsj~outline-pattern justification))))))

(defun ana~pstep-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A steo")
	   (effect  "None")
	   (value   "True, iff it is a method step"))
  (typep step 'keim::pdsc=actual-node))

(defun ana~substep-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff it is a method step or a method action"))
  (or (ana~pstep-p step) (and (ana~action-p step) (ana~action-psteps step))))

(defgeneric ana~substep-node (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The node closed by the substep"))
  (:method ((substep keim::pdsc=actual-node))
	   (pdsc~an-node substep))
  (:method ((substep ana+action))
	   (ana~substep-node (ana~action-psteps substep))))

(defgeneric ana~substep-just (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The justification of the step"))
  (:method ((substep keim::pdsc=actual-node))
	   (pdsc~an-just substep))
  (:method ((substep ana+action))
	   (ana~substep-just (ana~action-psteps substep))))

(defun ana~substep-subst (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The substitution of the step"))
  (pdsj~subst (ana~substep-just substep)))

(defun ana~substep-parameters (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The justification parameters of the step"))
  (pdsj~parameters (ana~substep-just substep)))

(defun ana~substep-method (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The method of the step"))
  (ana~just-meth (ana~substep-just substep)))

(defun ana~substep-backward-method-p (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "True, iff the step is a backward method application"))
  (meth~goal (ana~substep-method substep)))

(defun ana~substep-forward-method-p (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "True, iff the step is a forward method application"))
  (not (ana~substep-backward-method-p substep)))

(defun ana=methvar-of-methnode (methnode)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A method node")
	   (effect  "None")
	   (value   "The method variables contained in the node"))
  (if (meth~meta-node-p methnode)
      (meth~meta-node-metavar methnode)
    (keim~name methnode)))

(defun ana=substep-methvar-instance (substep methvar)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action) and a method variable")
	   (effect  "None")
	   (value   "The instance of the method variable in the step"))
  (meth~mapp-get-component methvar (ana~substep-subst substep) :all))

(defun ana=substep-methvars-instance (substep methvars)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action) and a list of method variables")
	   (effect  "None")
	   (value   "A list of instances of the method variables in the step"))
  (when methvars
    (let ((inst (ana=substep-methvar-instance substep (first methvars))))
      (if (listp inst)
	  (append inst (ana=substep-methvars-instance substep (rest methvars)))
	(cons inst (ana=substep-methvars-instance substep (rest methvars)))))))
    
(defun ana=substep-methnodes-instance (substep methnodes)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action) and a list of method nodes")
	   (effect  "None")
	   (value   "A list of nodes, that are the instantiations of the method nodes"))
  (ana=substep-methvars-instance substep (mapcar 'ana=methvar-of-methnode methnodes)))

(defun ana~substep-conclusion (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The first conclusion of the step"))
  (first (ana=substep-methnodes-instance substep (meth~exist-conclusions (ana~substep-method substep)))))

(defun ana~substep-conclusions (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The conclusions of the step"))
  (ana=substep-methnodes-instance substep (meth~conclusions (ana~substep-method substep))))

(defun ana~substep-exist-conclusions (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The exist conclusions of the step"))
  (ana=substep-methnodes-instance substep (meth~exist-conclusions (ana~substep-method substep))))

(defun ana~substep-add-conclusions (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The add conclusions of the step"))
  (ana=substep-methnodes-instance substep (meth~add-conclusions (ana~substep-method substep))))

(defun ana~substep-premises (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The premises of the step"))
  (ana=substep-methnodes-instance substep (meth~premises (ana~substep-method substep))))

(defun ana~substep-exist-premises (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The existent premises of the step"))
  (ana=substep-methnodes-instance substep (meth~exist-premises (ana~substep-method substep))))

(defun ana~substep-add-premises (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The subgoals of the step"))
  (ana=substep-methnodes-instance substep (meth~add-premises (ana~substep-method substep))))

(defun ana~substep-add-hyps (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The new hypotheses introduced by the step"))
  (remove-duplicates (apply 'append (mapcar #'(lambda (methvar) (ana~substep-nodes-of-methvar substep methvar))
					    (ana=substep-methvars-of-new-nodes-in-mapp substep)))))

(defun ana=methvars-of-nodes (mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A method mapping")
	   (effect  "None")
	   (value   "The method variables describing nodes"))
  (remove-if-not #'(lambda (methvar)
		     (let ((pds-obj (mapp~get-component methvar mapp)))
		       (or (pdsn~p pds-obj)
			   (and (listp pds-obj) (every 'pdsn~p pds-obj)))))
		 (mapp~domain mapp)))

(defun ana~substep-all-nodes (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "All nodes involved in the step"))
  (let ((mapp (meth~mapp-mapp (ana~substep-subst substep))))
    (ana=substep-methvars-instance substep (ana=methvars-of-nodes mapp))))

(defun ana~substep-new-nodes (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The nodes introduced by the step"))
  (set-difference (ana~step-all-nodes substep) (ana~step-exist-nodes substep)))

(defun ana~substep-exist-nodes (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The nodes existing before the step"))
  (union (ana~substep-exist-conclusions substep) (ana~substep-exist-premises substep)))

(defun ana=substep-methvars-of-new-nodes-in-mapp (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The method variables that describe nodes introduced by the step"))
  (let ((mapp (meth~mapp-mapp (ana~substep-subst substep)))
	(method (ana~substep-method substep)))
    (set-difference (ana=methvars-of-nodes mapp) (mapcar 'ana=methvar-of-methnode (union (meth~conclusions method) (meth~premises method))))))

(defun ana~substep-nodes-of-methvar (substep methvar)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action) and a method variable")
	   (effect  "None")
	   (value   "The nodes described by the method variable in the step"))
  (ana=substep-methvars-instance substep (list methvar)))

(defun ana~substep-exist-metavars (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The existing meta variables before the step"))
  (remove-duplicates (apply 'append
			    (mapcar #'(lambda (new-node)
					(if (pdsn~schematic-p new-node)
					    (remove-if-not #'meta~p (data~free-variables (node~formula new-node)))
					  nil))
				    (ana~substep-exist-nodes substep)))))

(defun ana~substep-new-metavars (substep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action)")
	   (effect  "None")
	   (value   "The meta variables introduced by the step"))
  (set-difference
   (remove-duplicates (apply 'append
			     (mapcar #'(lambda (new-node)
					 (if (pdsn~schematic-p new-node)
					     (remove-if-not #'meta~p (data~free-variables (node~formula new-node)))
					   nil))
				     (ana~substep-new-nodes substep))))
   (ana~substep-exist-metavars substep)))

(defun ana~substep-compute-parameters (substep task)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A substep (method step or method action) and a task")
	   (effect  "None")
	   (value   "Invokes the cri, to compute method parameters for task analogue to the step"))
  (cri~set-used-control-rules! (mapcar 'keim~name (keim~get substep 'applied-crules)))
  (let ((selected-methods-with-parameters
	 (remove-if-not #'(lambda (selected-method) (when (listp selected-method)
						      (keim~equal (keim~name (ana~substep-method substep)) (first selected-method))))
			(cri~call (list (keim~name (ana~substep-method substep)))
				  :kind 'methods
				  :task task
				  :task-node ;;(when with-task
				  (agenda~task-node task)
				  :task-formula ;;(when with-task
				  (node~formula (agenda~task-node task))
				  :agenda (pds~agenda omega*current-proof-plan)
				  :pds omega*current-proof-plan))))
    (mapcar 'third selected-methods-with-parameters)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Strategic Step}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana=sstep-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff it is a strategy step"))
  (scon~strategy-step-p step))

(defun ana=sstep-roc (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The corresponding state description"))
  (first (pdsj~parameters (scon~strategy-step-just sstep))))

(defun ana=sstep-roc-at-creation (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The state description at creation time of the step"))
  (let ((roc (second (pdsj~parameters (scon~strategy-step-just sstep)))))
    (if roc roc
      (omega~error "Scon step ~A has no second parameter (roc at creation)" sstep))))

(defun ana=sstep-interrupt-exmes (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The interruption message, if the step is an interruption, otherwise nil"))
  (when (ana=sstep-interrupt-p sstep)
    (third (pdsj~parameters (scon~strategy-step-just sstep)))))

(defun ana=sstep-reinvoke-tasks (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The local tasks at the reinvokation, if the step is a reinvokation, otherwise nil"))
  (when (ana=sstep-reinvoke-p sstep)
    (let ((tasks (third (pdsj~parameters (scon~strategy-step-just sstep)))))
      (if tasks tasks
	(omega~error "Scon step ~A has no third parameter (tasks)" sstep)))))

(defun ana=sstep-strategy (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The strategy of the step"))
  (roc~strategy-ks (ana=sstep-roc sstep)))

(defun ana=sstep-task (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The start task of the step"))
  (roc~start-task (ana=sstep-roc sstep)))

(defun ana=sstep-refalg (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The refinement algorithm of the step"))
  (strat~strategy-ks-refinement-algorithm (ana=sstep-strategy sstep)))

(defun ana=sstep-of-refalg-p (step refalg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step and a refinement algorithm")
	   (effect  "None")
	   (value   "True, iff the step is a straetgy step with the given refinement algorithm"))
  (when (ana=sstep-p step)
    (keim~equal (keim~name (ana=sstep-refalg step)) refalg)))

(defun ana=sstep-pplanner-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff the step is a pplanner step"))
  (ana=sstep-of-refalg-p step 'pplanner))

(defun ana=sstep-analogy-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff the step is an analogy step"))
  (ana=sstep-of-refalg-p step 'analogynew))

(defun ana=sstep-instmeta-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff the step is an instmeta step"))
  (ana=sstep-of-refalg-p step 'instmeta))

(defun ana=sstep-backtrack-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff the step is a backtracking step"))
  (ana=sstep-of-refalg-p step 'backtrack))

(defun ana=sstep-of-inference-p (sstep infer)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step and an inference")
	   (effect  "None")
	   (value   "True, iff the step has the given inference"))
  (when (ana=sstep-p sstep)
    (keim~equal (just~method (scon~strategy-step-just sstep)) (infer~find-method infer))))

(defun ana=sstep-invoke-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is an invokation"))
  (ana=sstep-of-inference-p sstep 'start-strategy-ks-application))

(defun ana=sstep-terminate-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is a termination"))
  (ana=sstep-of-inference-p sstep 'end-strategy-ks-application))

(defun ana=sstep-interrupt-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is an interruption"))
  (ana=sstep-of-inference-p sstep 'interrupt-strategy-ks-application))

(defun ana=sstep-reinvoke-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is a reinvokation"))
  (ana=sstep-of-inference-p sstep 'reinvoke-strategy-ks-application))

(defun ana=sstep-start-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is an invokation or reinvokation"))
  (or (ana=sstep-invoke-p sstep) (ana=sstep-reinvoke-p sstep)))

(defun ana=sstep-end-p (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "True, iff the step is an interruption or termination"))
  (or (ana=sstep-interrupt-p sstep) (ana=sstep-terminate-p sstep)))

(defun ana=sstep-substeps-of-roc (sstep roc)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step and an analogy state descrition")
	   (effect  "None")
	   (value   "All substeps of the step wrt. the state description"))
  (cond ((or (ana=sstep-pplanner-p sstep) (ana=sstep-analogy-p sstep))
	 (reverse (roc~pplanner-steps roc)))
	(t nil)))

(defun ana=sstep-substeps (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "All substeps of the step"))
  (let ((roc (ana=sstep-roc sstep)))
    (ana=sstep-substeps-of-roc sstep roc)))

(defun ana=sstep-substeps-at-creation (sstep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A strategy step")
	   (effect  "None")
	   (value   "The substeps of the step at creation time"))
  (cond ((ana=sstep-invoke-p sstep) nil)
	((or (ana=sstep-interrupt-p sstep) (ana=sstep-reinvoke-p sstep))
	 (let ((roc (ana=sstep-roc-at-creation sstep)))
	   (ana=sstep-substeps-of-roc sstep roc)))
	((ana=sstep-terminate-p sstep) (ana=sstep-substeps sstep))))

(defun ana=sstep-instmeta-metavar (istep)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An instmeta step")
	   (effect  "None")
	   (value   "The meta variable of the instmeta step"))
  (agenda~inst-task-meta-var (roc~start-task (ana=sstep-roc istep))))

(defun ana=sstep-instmeta-instantiation (istep pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An instmeta step")
	   (effect  "None")
	   (value   "The instantiation of the instmeta step"))
  (subst~get-component (ana=sstep-instmeta-metavar istep) (pds~cstrpool-bindings (pds~constraint-pool pds))))

(defun ana=ssteps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "All strategy steps in the pds"))
  (when (and pds (keim~get pds :solution-bb))
    (store~elements (black~get-blackboard-object-content 'ssteps-list (keim~get pds :solution-bb)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Segment}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ana+segment (keim+object)
  ((start-step                      :initarg :start-step
				    :initform nil
				    :accessor ana~seg-start-step)
   (end-step                        :initarg :end-step
				    :initform nil
				    :accessor ana~seg-end-step))
  (:documentation "Objects of this class represent a segment containing two strategy steps: a start step and an end step."))

(defmethod print-object ((seg ana+segment) stream)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment and a stream")
	   (effect  "Writes the segment to the stream")
	   (value   "Undefined"))
  (let ((start (ana~seg-start-step seg))
	(end (ana~seg-end-step seg))
	(roc-parameters (roc~parameters (ana~seg-roc seg))))
    (format stream "Segment ~A ~A-~A ~A" (keim~name (ana~seg-strategy seg))
	    (if (ana=sstep-invoke-p start) "Inv" "Reinv")
	    (if (null end) ""
	      (if (ana=sstep-interrupt-p end) "Inter" "Term"))
	    (if roc-parameters roc-parameters "()"))))

(defun ana~seg-p (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is a segment"))
  (typep obj 'ana+segment))

(defun ana~seg-open-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment has no end step"))
  (and (ana~seg-p seg) (null (ana~seg-end-step seg))))

(defun ana~seg-invoke-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment start with an invokation"))
  (and (ana~seg-p seg) (ana=sstep-invoke-p (ana~seg-start-step seg))))

(defun ana~seg-reinvoke-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment start with a reinvokation"))
  (and (ana~seg-p seg) (ana=sstep-reinvoke-p (ana~seg-start-step seg))))

(defun ana~seg-terminate-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment ends with a termination"))
  (and (ana~seg-p seg) (ana=sstep-terminate-p (ana~seg-end-step seg))))

(defun ana~seg-interrupt-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment ends with an interruption"))
  (and (ana~seg-p seg) (ana=sstep-interrupt-p (ana~seg-end-step seg))))

(defun ana~seg-pplanner-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment is created by pplanner"))
  (and (ana~seg-p seg) (ana=sstep-pplanner-p (ana~seg-start-step seg))))

(defun ana~seg-instmeta-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment is created by instmeta"))
  (and (ana~seg-p seg) (ana=sstep-instmeta-p (ana~seg-start-step seg))))

(defun ana~seg-analogy-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment is created by cplanner"))
  (and (ana~seg-p seg) (ana=sstep-analogy-p (ana~seg-start-step seg))))

(defun ana~seg-backtrack-p (seg)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment is created by backtracking"))
  (and (ana~seg-p seg) (ana=sstep-backtrack-p (ana~seg-start-step seg))))

(defun ana~seg-create (start &optional end)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A start step and an end step")
	   (effect  "None")
	   (value   "A new segment from the steps"))
  (if (and (ana=sstep-start-p start)
	   (or (and (ana=sstep-end-p end) (keim~equal (ana=sstep-roc start) (ana=sstep-roc end)))
	       (null end)))
      (make-instance 'ana+segment :start-step start :end-step end)
    (omega~error "Illegal step constellation.")))

(defmethod ana~seg-strategy ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The strategy, that created the segment"))
  (ana=sstep-strategy (ana~seg-start-step seg)))

(defmethod ana~seg-task ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The task the strategy of the segment was invoked on"))
  (ana=sstep-task (ana~seg-start-step seg)))

(defmethod ana~seg-roc ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The state description of the strategy application, that created the segment"))
  (ana=sstep-roc (ana~seg-start-step seg)))

(defmethod ana~seg-parameters ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The parameters of the strategy, that created the segment"))
  (roc~parameters (ana~seg-roc seg)))

(defmethod ana~seg-interrupt-exmes ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The interruption message, if the segment was interrupted, otherwise nil"))
  (when (ana~seg-end-step seg)
    (ana=sstep-interrupt-exmes (ana~seg-end-step seg))))

(defmethod ana~seg-failed-p ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "True, iff the segment ends with a failure step"))
  (and (ana~seg-interrupt-p seg) (null (ana~seg-interrupt-exmes seg))))

(defmethod ana~seg-interrupt-crules ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The control rules, that interrupted the segment, if it was interrupted, otherwise nil"))
  (when (ana~seg-interrupt-exmes seg)
    (mapcar 'keim~name (exmes~interruption-message-applied-crules (ana~seg-interrupt-exmes seg)))))

(defmethod ana~seg-invoke-tasks ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The tasks, the segment was invoked on"))
  (list (ana~seg-task seg)))

(defmethod ana~seg-reinvoke-tasks ((seg ana+segment) pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment and a pds")
	   (effect  "None")
	   (value   "The tasks, the segment was reinvoked on"))
  (remove-if-not #'(lambda (task) (pds~label2node (keim~name (agenda~task-node task)) pds)) (ana=sstep-reinvoke-tasks (ana~seg-start-step seg))))

(defmethod ana~seg-tasks ((seg ana+segment) pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment and a pds")
	   (effect  "None")
	   (value   "The task, the segment was working on"))
  (if (ana~seg-invoke-p seg) (ana~seg-invoke-tasks seg) (ana~seg-reinvoke-tasks seg pds)))

(defun ana~task-to-obj (task)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A task")
	   (effect  "None")
	   (value   "The node or the meta variable of the task"))
  (cond ((agenda~goal-or-goal-schema-task-p task)
	 (agenda~task-node task))
	((agenda~inst-task-p task)
	 (agenda~inst-task-meta-var task))))

(defmethod ana~seg-goals ((seg ana+segment) pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment and a pds")
	   (effect  "None")
	   (value   "The goals closed by the segment"))
  ;;(mapcar 'ana~task-to-obj (ana~seg-tasks seg pds)))
  (ana~step-exist-conclusions seg))

(defun ana=without-equal-steps (steps)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps")
	   (effect  "None")
	   (value   "Removes duplicate steps from the list, coming from a forward application of a method"))
  (when steps
    (cond ((and (ana~pstep-p (first steps)) (ana~pstep-p (second steps))
		;; steps are a forward applications
		(ana~substep-forward-method-p (first steps))
		(ana~substep-forward-method-p (second steps))
		;; the justifications are equal
		(keim~equal (ana~substep-just (first steps)) (ana~substep-just (second steps))))
	   (cons (first steps) (ana=without-equal-steps (rest (rest steps)))))
	  (t (cons (first steps) (ana=without-equal-steps (rest steps)))))))
	   
(defmethod ana~seg-substeps ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A segment")
	   (effect  "None")
	   (value   "The steps between the start step and the end step of the segment"))
  (let ((start (ana~seg-start-step seg))
	(end (ana~seg-end-step seg)))
    (ana=without-equal-steps
     (if end
	 ;; closed segment
	 (let ((backtracked-steps (ana~set-difference (ana=sstep-substeps-at-creation end) (ana=sstep-substeps end))))
	   (ana~set-difference (ana~set-difference (ana=sstep-substeps-at-creation end) (ana=sstep-substeps-at-creation start))
			       backtracked-steps))
       ;; open segment
       (ana~set-difference (ana=sstep-substeps start) (ana=sstep-substeps-at-creation start))))))

(defun ana~seg-substep-roc (substep pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A method step and a pds")
	   (effect  "None")
	   (value   "The state description of the strategy, that introduced the method step"))
  (ana~seg-roc (find-if #'(lambda (seg) (find substep (ana~seg-substeps seg))) (ana~segs pds))))

(defmethod ana~seg-instmeta-metavar ((seg ana+segment))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An instmeta segment")
	   (effect  "None")
	   (value   "The metavar instantiated by the instmeta segment"))
  (ana=sstep-instmeta-metavar (ana~seg-start-step seg)))

(defmethod ana~seg-instmeta-instantiation ((seg ana+segment) pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An instmeta segment and a pds")
	   (effect  "None")
	   (value   "The instantion done by the instmeta segment"))
  (ana=sstep-instmeta-instantiation (ana~seg-start-step seg) pds))

(defun ana=segs-from-ssteps (ssteps)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of strategy steps")
	   (effect  "None")
	   (value   "A list of segments representing the strategy steps"))
  (when ssteps
    (cond ((and (ana=sstep-start-p (first ssteps)) (ana=sstep-end-p (second ssteps)))
	   (cons (ana~seg-create (first ssteps) (second ssteps))
		 (ana=segs-from-ssteps (rest (rest ssteps)))))
	  ((and (ana=sstep-start-p (first ssteps)) (null (second ssteps)))
	   (list (ana~seg-create (first ssteps))))
	  (t (ana=segs-from-ssteps (rest ssteps))))))
  
(defun ana~segs (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "All segments in the pds"))
  (ana=segs-from-ssteps (ana=ssteps pds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Step}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana~step-exist-conclusions (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The existent conclusions of the step"))
  (cond ((listp step)
	 (set-difference (remove-duplicates (apply 'append (mapcar 'ana~step-exist-conclusions step)))
			 (apply 'append (mapcar 'ana~step-new-nodes step))))
	((ana~seg-p step)
	 (ana~step-exist-conclusions (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-exist-conclusions step))))

(defun ana~step-add-conclusions (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The add conclusions of the step"))
  (cond ((listp step)
	 (set-difference (remove-duplicates (apply 'append (mapcar 'ana~step-add-conclusions step)))
			 (apply 'append (mapcar 'ana~step-exist-nodes step))))
	((ana~seg-p step)
	 (ana~step-add-conclusions (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-add-conclusions step))))

(defun ana~step-exist-premises (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The existent premises of the step"))
  (cond ((listp step)
	 (set-difference (remove-duplicates (apply 'append (mapcar 'ana~step-exist-premises step)))
			 (apply 'append (mapcar 'ana~step-new-nodes step))))
	((ana~seg-p step)
	 (ana~step-exist-premises (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-exist-premises step))))

(defun ana~step-add-premises (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The subgoals of the step"))
  (cond ((listp step)
	 (set-difference (remove-duplicates (apply 'append (mapcar 'ana~step-add-premises step)))
			 (apply 'append (mapcar 'ana~step-exist-nodes step))))
	((ana~seg-p step)
	 (ana~step-add-premises (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-add-premises step))))

(defun ana~step-exist-nodes (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The existing nodes, ie the existent premises and conclusions"))
  (union (ana~step-exist-conclusions step) (ana~step-exist-premises step)))

(defun ana~step-all-nodes (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The node involved in the step"))
  (cond ((listp step)
	 (remove-duplicates (apply 'append (mapcar 'ana~step-all-nodes step))))
	((ana~seg-p step)
	 (ana~step-all-nodes (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-all-nodes step))))

(defun ana~step-new-nodes (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The nodes created by the step"))
  (cond ((listp step)
	 (apply 'append (mapcar 'ana~step-new-nodes step)))
	((ana~seg-p step)
	 (ana~step-new-nodes (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-new-nodes step))))

(defun ana~step-add-hyps (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The new hypotheses created by the step"))
  (cond ((listp step)
	 (remove-duplicates (apply 'append (mapcar 'ana~step-add-hyps step))))
	((ana~seg-p step)
	 (ana~step-add-hyps (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-add-hyps step))))

(defun ana~step-exist-metavars (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The existing meta variables of the step"))
  (cond ((listp step)
	 (set-difference (remove-duplicates (apply 'append (mapcar 'ana~step-exist-metavars step)))
			 (ana~step-new-metavars step)))
	((ana~seg-p step)
	 (ana~step-exist-metavars (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-exist-metavars step))))

(defun ana~step-new-metavars (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The meta variables created by the step"))
  (cond ((listp step)
	 (remove-duplicates (apply 'append (mapcar 'ana~step-new-metavars step))))
	((ana~seg-p step)
	 (ana~step-new-metavars (ana~seg-substeps step)))
	((ana~substep-p step)
	 (ana~substep-new-metavars step))))

(defun ana~step-end-condition (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "The end condition of the step, for method steps a function that returns nil"))
  (if (ana~seg-p step)
    (cond ((ana~seg-terminate-p step)
	   ;; step terminated
	   (ana=ref-parameter 'termination-check (ana~seg-roc step)))
	  ((ana~seg-interrupt-p step)
	   (if (ana~seg-failed-p step)
	       ;; step failed
	       #'(lambda () nil)
	     ;; step interrupted
	     (let ((crules (ana~seg-interrupt-crules step)))
	       ;;(let ((crules (ana=ref-parameter 'control-rules (ana~seg-roc step))))
	       #'(lambda ()
		   (cri~remove-all-used-control-rules)
		   (cri~set-used-control-rules! crules)
		   (let* ((pds omega*current-proof-plan)
			  (agenda (pds~agenda pds)))
		     (multiple-value-bind (free-tasks unblocked-tasks)
			 (if (agenda~empty-p agenda) (values nil nil) (pds~first-tasks! pds agenda))
		       ;; Note: pds~first-tasks! does already update the current-formulas in the tasks!
		       ;;       Hence, no further extra up-to-date necessary for the tasks!
		       (multiple-value-bind (result applied-crules)
			   (cri~call '(interrupt)
				     :kind 'strategy-interruption
				     :tasks  (if (agenda~empty-p agenda) nil
					       (remove-if-not #'(lambda (task)
								  (find ana*roc-state-description (agenda~task-rocs task)))
							      (append free-tasks unblocked-tasks)))
				     :pds pds)
			   applied-crules))))))))
    #'(lambda () nil)))

(defun ana~step-failed-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True, iff the step is a segment that failed"))
  (if (ana~seg-p step)
      (ana~seg-failed-p step)
    nil))

(defun ana~step-special-matching-p (step matching)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step and a matching")
	   (effect  "None")
	   (value   "True, iff the matched method is a normalization or restriction method wrt. the strategy of the step"))
  (when (and (ana~seg-pplanner-p step) (ana~matching-method matching))
    (let ((special-methods (union (gethash 'normalization-methods (strat~strategy-ks-hash-table (ana~seg-strategy step)))
				  (gethash 'restriction-methods (strat~strategy-ks-hash-table (ana~seg-strategy step))))))
      (find (keim~name (ana~matching-method matching)) special-methods))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana~all-steps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "All method steps and segments in the pds"))
  (apply 'append (mapcar #'(lambda (seg) (cons seg (ana~seg-substeps seg))) (ana~segs pds))))

(defun ana~backtrack-steps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "The backtracked steps"))
  (apply 'append (mapcar #'(lambda (seg) (cond ((or (ana~seg-pplanner-p seg) (ana~seg-analogy-p seg))
						(ana~seg-substeps seg))
					       ((ana~seg-instmeta-p seg)
						(list (ana~seg-start-step seg)))
					       (t nil)))
			 (ana~segs pds))))
  
(defun ana~flat-steps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "Method steps and instmeta steps in chronological order"))
  (apply 'append (mapcar #'(lambda (seg) (cond ((or (ana~seg-pplanner-p seg) (ana~seg-analogy-p seg))
						(ana~seg-substeps seg))
					       ((ana~seg-instmeta-p seg)
						(list seg))
					       (t nil)))
			 (ana~segs pds))))

(defun ana~hierarchical-steps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "The relevant segments of the pds in chronological order"))
  ;; remove pplanner segments without substeps
  (remove-if #'(lambda (seg) (and (or (ana~seg-pplanner-p seg) (ana~seg-analogy-p seg)) (null (ana~seg-substeps seg))))
	     (remove-if-not #'(lambda (seg) (or (ana~seg-pplanner-p seg) (ana~seg-analogy-p seg) (ana~seg-instmeta-p seg))) (ana~segs pds))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Access}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana~all-p (step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step")
	   (effect  "None")
	   (value   "True"))
  t)

(defun ana~find-next-in (steps step &optional (test 'ana~all-p))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps, a step and optional a test function")
	   (effect  "None")
	   (value   "Tests the steps after the given step in the list of steps and returns the first, that fullfils the test"))
  (let ((next-step (nth (+ 1 (position step steps)) steps)))
    (when next-step
      (if (funcall test next-step) next-step (ana~find-next-in next-step test)))))

(defun ana~find-first-in (steps &optional (test 'ana~all-p))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps and optional a test function")
	   (effect  "None")
	   (value   "The first step in the list, that fullfils the test"))
  (let ((first-step (first steps)))
    (if (funcall test first-step) first-step (ana~find-next-in steps first-step test))))

(defun ana~find-previous-in (steps step &optional (test 'ana~all-p))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps, a step and optional a test function")
	   (effect  "None")
	   (value   "Tests the steps before the given step in the list of steps and returns the first, that fullfils the test"))
  (let ((previous-step (nth (- 1 (position step steps)) steps)))
    (when previous-step
      (if (funcall test previous-step) previous-step (ana~find-previous-in steps previous-step test)))))

(defun ana~find-last-in (steps &optional (test 'ana~all-p))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps and optional a test function")
	   (effect  "None")
	   (value   "The last step in the list, that fullfils the test"))
  (let ((last-step (first (last steps))))
    (if (funcall test last-step) last-step (ana~find-previous-in steps last-step test))))

(defun ana~steps-find-until-in (steps step test)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps, a step and a test function")
	   (effect  "None")
	   (value   "All steps in the list from the given step on, until the test is fulfilled"))
  (when (not (funcall test step))
    (cons step (ana~steps-find-until-in steps (ana~find-next-in steps step) test))))

(defun ana~steps-behind-until-in (steps step test)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps, a step and a test function")
	   (effect  "None")
	   (value   "All steps in the list from the given step back, until the test is fulfilled"))
  (let ((next-step (ana~find-next-in steps step)))
    (when next-step
      (ana~steps-find-until-in steps next-step test))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Print}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ana=print-step (step pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A step and pds")
	   (effect  "Prints the step to the standard output")
	   (value   "Undefined"))
  (cond ((ana~seg-p step)
	 (if (ana~seg-invoke-p step)
	     (omega~message "**************************************************************************")
	   (omega~message "--------------------------------------------------------------------------"))
	 (omega~message "~A" step)
	 (when (ana~seg-instmeta-p step)
	   (omega~message "Instantiating ~A with ~A" (ana~seg-instmeta-metavar step) (ana~seg-instmeta-instantiation step pds)))
	 (ana=print-steps (ana~seg-substeps step) pds)
	 (when (not (ana~seg-open-p step))
	   (if (ana~seg-terminate-p step)
	       (omega~message "**************************************************************************")
	     (progn
	       (if (ana~seg-failed-p step)
		   (omega~message "Failed")
		 (omega~message "Interrupted by control rules: ~A" (ana~seg-interrupt-crules step)))
	       (omega~message "--------------------------------------------------------------------------")))))
	((ana~substep-p step)
	 ;;(when (ana~action-p step)
	 ;;  (omega~message "Analogy step"))
	 (if (meth~goal (ana~substep-method step))
	     (omega~message "Backward step ~A" step)
	   (omega~message "Forward step ~A" step)))
	(t
	 (omega~message "Unknown step ~A" step))))

(defun ana=print-steps (steps pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of steps and a pds")
	   (effect  "Prints the steps to the standard output")
	   (value   "Undefined"))
  (mapc #'(lambda (step) (ana=print-step step pds)) steps))
  
(defun ana~print-steps (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "Prints the segments of the pds to the standard output")
	   (value   "Undefined"))
  (ana=print-steps (ana~segs pds) pds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Correspondence table}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ana*table-verbose nil "Verbose mode for corresponding-tables.")


(defclass ana+correspondence-table ()
  ((source-to-targets        :initform (make-hash-table)
			     :accessor ana=cor-source-to-targets)
   (target-to-sources        :initform (make-hash-table)
			     :accessor ana=cor-target-to-sources)
   (cor-list                 :initform nil
			     :accessor ana=cor-list))
  (:documentation "Objects of this class contain information about the correspondance of pds objects."))

(defun ana~cor-table-construct ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "An empty correspondence table object"))
  (make-instance 'ana+correspondence-table))

(defun ana~cor-initialize (cor-list)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of corresponding pairs")
	   (effect  "None")
	   (value   "The extended correspondence table"))
  ;; creates a node-table and adds the correspondences in the list
  (let ((new-table (ana~cor-table-construct)))
    (mapcar #'(lambda (cor-pair)
		(ana~cor-add-pair new-table cor-pair))
	    cor-list)
    (ana~cor-print new-table)
    new-table))

(defmethod ana~cor-print ((table ana+correspondence-table))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondenve table")
	   (effect  "Displays the correspondence table")
	   (value   "Undefined"))
  (when ana*table-verbose
    (omega~message "Actual correspondances:")
    (mapc #'(lambda (cor-pair) (omega~message "~40@<~A~> --> ~40@<~A~>" (get 'objects (first cor-pair)) (get 'objects (second cor-pair))))
	  (ana=cor-list table))
    table))

(defun ana~cor-obj-p (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "True, iff the thing is a pds node or a meta variable"))
  (or
   (pdsn~p obj)
   (meta~p obj)))

(defmethod ana=cor-of ((table ana+correspondence-table) obj func)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table, a simple object and a function")
	   (effect  "None")
	   (value   "The corresponding objects to the object, if the obejct is in the correspondence table, otherwise nil."
		    "The direction of the correspondence is given by the function"))
  (get 'objects (gethash obj (funcall func table))))

(defmethod ana~cor-of ((table ana+correspondence-table) obj func)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table, a simple object and a function")
	   (effect  "None")
	   (value   "The corresponding objects to the object, if the obejct is in the correspondence table, otherwise a failure."
		    "The direction of the correspondence is given by the function"))
  (if (not (ana~cor-obj-p obj)) (list obj)
    (let ((cors (ana=cor-of table obj func)))
      (if cors cors
	(list (ana~failure-create 'missing-object (list obj)))))))

(defmethod ana~cor-set-of ((table ana+correspondence-table) set func)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table, a set of simple objects and a function")
	   (effect  "None")
	   (value   "The corresponding objects or a list of failures"))
  (remove-duplicates (apply 'append (mapcar #'(lambda (obj) (ana~cor-of table obj func)) set))))

(defmethod ana~cor-struct-of ((table ana+correspondence-table) obj func)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table, an simple or complex objects and a function")
	   (effect  "None")
	   (value   "A list of all corresponding simple or complex objects or a list of failures"))
  (cond ((listp obj)
	 (if obj
	     (let ((first-structs (ana~cor-struct-of table (first obj) func))
		   (rest-structs (ana~cor-struct-of table (rest obj) func)))
	       (cond ((and (ana~failure-list-p first-structs) (ana~failure-list-p rest-structs))
		      (append first-structs rest-structs))
		     ((and (ana~failure-list-p first-structs) (not (ana~failure-list-p rest-structs)))
		      first-structs)
		     ((and (not (ana~failure-list-p first-structs)) (ana~failure-list-p rest-structs))
		      rest-structs)
		     (t
		      (apply 'append (mapcar #'(lambda (first-cor)
						 (mapcar #'(lambda (rest-cors) (cons first-cor rest-cors)) rest-structs))
					     first-structs)))))
	   (list nil)))
	(t (ana~cor-of table obj func))))

(defmethod ana=cor-add ((table ana+correspondence-table) obj new-sym func)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table, a simple object, a symbol and a function")
	   (effect  "None")
	   (value   "Associates the object with the symbol in the correspondence table"))
  (setf (gethash obj (funcall func table)) new-sym))

(defmethod ana~cor-add-pair ((table ana+correspondence-table) pair)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table and a pair of corresponding objects")
	   (effect  "Adds the pair to the table")
	   (value   "The extended table"))
  (when (first pair) ;; do not enter empty pairs
    (let ((target-sym (gensym))
	  (source-sym (gensym))
	  (sources (if (listp (first pair))
		       (first pair)
		     (list (first pair))))
	  (targets (second pair)))
      (mapcar #'(lambda (source) (ana=cor-add table source target-sym 'ana=cor-source-to-targets)) sources)
      (setf (get 'objects target-sym) targets)
      (mapcar #'(lambda (target) (ana=cor-add table target source-sym 'ana=cor-target-to-sources)) targets)
      (setf (get 'objects source-sym) sources)
      (setf (ana=cor-list table) (append (ana=cor-list table) (list (list source-sym target-sym))))))
  table)

(defmethod ana~cor-change ((table ana+correspondence-table) node-pair)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A correspondence table and a pair")
	   (effect  "Removes an eventually existing old pair and adds the new pair")
	   (value   "The updated table"))
  (let* ((source (first node-pair))
	 (targets (second node-pair))
	 (sym (gethash source (ana=cor-table table)))
	 (val (get 'objects sym)))
    ;; remove the old nodes
    (setf (get 'objects sym)
	  (set-difference val targets))
    ;; insert the new nodes
    (ana~cor-add-pair table (list source targets))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Helper funtions}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ana*active nil "Flag for activation of analogy.")
(defvar ana*trace nil "Flag for omega~trace outputs.")
(defvar ana*messages t "Flag for omega~message outputs.")
(defvar ana*indent 0 "A number specifying the indentation")
(defvar ana*strategies nil "A list containing all cplanner strategies")
(defvar ana*strategic-control nil "A list containing all strategic control rules for the cplanner strategies")

(defun ana~store-solution-bb (new-solution-blackboard pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A solution blackboard and a pds")
	   (effect  "Associates the pds with the blackboard")
	   (value   "Undefined"))
  (keim~put pds :solution-bb new-solution-blackboard))

(defun ana~trace (format-string &rest args)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A format string and some arguments")
	   (effect  "Outputs the formated string if tracing is enabled")
	   (value   "Undefined"))
  (when ana*trace
    (apply 'omega~trace (cons (ana~indent-wo format-string) args))))

(defun ana~indent-wo (string)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A string")
	   (effect  "None")
	   (value   "Puts some spaces before the string"))
  (when (< ana*indent 0) (setf ana*indent 0))
  (concatenate 'string (make-string (* ana*indent 2) :initial-element #\SPACE) string))

(defun ana~indent (string)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A string")
	   (effect  "None")
	   (value   "Puts some spaces before the string"))
  (when (< ana*indent 0) (setf ana*indent 0))
  (concatenate 'string "~%" (make-string (* ana*indent 2) :initial-element #\SPACE) string))

(defun ana~message (format-string &rest args)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A format string and some arguments")
	   (effect  "Outputs the formated string if messages are enabled")
	   (value   "Undefined"))
  (when ana*messages
    (apply 'omega~message (cons (ana~indent-wo format-string) args))))

(defun ana=ref-parameter (name state-des)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A parameter name and a state description")
	   (effect  "None")
	   (value   "Returns the parameter of the refinement with the given name"))
  (gethash name (strat~strategy-ks-hash-table (roc~strategy-ks state-des))))

(defmacro ana~when-not (test statement)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A test and a statement")
	   (effect  "None")
	   (value   "Returns the result of the test, if it is not nil, otherwise the statement is executed"))
  `(let ((result ,test))
     (if result result ,statement)))

(defun ana~root-task-p (task)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A task")
	   (effect  "None")
	   (value   "True, iff the task is the task for the root node of the current proof plan"))
  (and (not (agenda~inst-task-p task))
       (keim~equal (prob~proof-root omega*current-proof-plan) (agenda~task-node task))))

(defun ana~goal-tasks ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "All tasks for nodes"))
  (remove-if 'agenda~inst-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan))))

(defun ana~inst-tasks ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "All tasks for meta variables"))
  (remove-if-not 'agenda~inst-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan))))

(defun ana~local-tasks ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "All tasks within the current analogy state description"))
  (remove-if-not #'(lambda (task) (find ana*roc-state-description (agenda~task-rocs task))) (ana~goal-tasks)))

(defun ana~set-difference (set1 set2)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Two sets")
	   (effect  "None")
	   (value   "The difference between set1 and set2 maintaining the order of the elements in set1"))
  (remove-if #'(lambda (elem) (find elem set2)) set1))

(defun ana~subsets (set count)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A set and a number")
	   (effect  "None")
	   (value   "All subset with the given size"))
  (if (= count 0)
      (list nil)
    (when set
      (append
       (mapcar #'(lambda (subsets) (cons (first set) subsets)) (ana~subsets (rest set) (- count 1)))
       (ana~subsets (rest set) count)))))

(defun ana~activate (bool)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A boolean")
	   (effect  "If true, the analogy is activated by adding the analogy strategies to the current strategies"
		    "and the analogy control rules to the current strategic control rules, otherwise analogy is deactivated by removing")
	   (value   "Undefined"))
  (if bool
      (progn
	(omega~message "Analogy activated.")
	(omega~message "Adding the following strategies: ~A" ana*strategies)
	(setf sod*current-strategies (append ana*strategies sod*current-strategies))
	(omega~message "Adding the following strategic control: ~A" ana*strategic-control)
	(setf sod*current-strategic-control-rules (append sod*current-strategic-control-rules ana*strategic-control)))
    (progn
      (omega~message "Analogy deactivated.")
      (setf sod*current-strategies (ana~set-difference sod*current-strategies ana*strategies))
      (setf sod*current-strategic-control-rules (ana~set-difference sod*current-strategic-control-rules ana*strategic-control)))))

(defun ana~source-plans ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "All plans found in this session, that are completely solving a problem"))
  (remove-if 'pds~open-nodes (apply 'append (mapcar 'prob~proofs (oc=get-all-problems)))))

(defun ana~cstrpool-bindings (pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A pds")
	   (effect  "None")
	   (value   "The current constraint pool bindings in the form of a substitution"))
  (let ((pool (pds~constraint-pool pds)))
    (if pool
	(pds~cstrpool-bindings pool)
      (subst~create nil nil))))

