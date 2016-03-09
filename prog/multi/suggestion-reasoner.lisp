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


;; The suggestion reasoner occurs, if an execution-message was returned.
;; There exist three cases of execution messages:
;; 1.) The termination message: A strategy-ks signals, that it has finished successful
;; 2.) The interruption message: A strategy-ks has interrupted itself and now wants to get a demand fulfilled
;; 3.) The failure message: A strategy-ks has intterupred its computation, since it was stuck for some reasons
;;
;; The suggester now has the job to analyze the execution message and to generate proposals for demands from it and
;; to determine, whether a strategy-ks should be terminated.
;; Thereto it can generate two sorts of reactions:
;; 
;; 1.) termination-reaction
;;     This reaction leads thereto, that a strategy-ks gets terminated (for details see code)
;;     Especially this means, that it would not get stored at the store.
;;     This reaction also can contain demands, that are written to the CBB.
;; 
;; 2.) non-termination-reaction
;;     This reaction leads thereto, that a not-terminated strategy-ks is written to the store. Furthermore all generated demands
;;     are annotated to this strategy-ks, i.e. the strategy-ks can be applied not until all its demands have been fulfilled
;;
;; Let us take a look, what happens, if the planer can not go on, that happens for example if you have a task no method is applicable to.
;; In the case of an error, you have principally the following possibilities:
;; 1.) Simply terminate this strategy to go on with the present situation.
;;     This makes sense for example, if there is a task, that is not closable in a definit strategy, but in another absolutly.
;;     -> So terminate the present strategy application and do something other, perhaps selected by demands, perhaps not!
;; 2.) Uphold the strategy application, but make somthing different.
;;     For example tackle a non closable goal with another strategy, but after that go back to the original strategy.
;;     -> Create corresponding demands, that have to be annotated to the strategy, i.e. as soon as a demands are fulfilled, the strategy
::        can be reinvoked
;; 3.) Backtrack one or more steps or the whole strategy
;;     -> like 2.) Create a corresponding demand. Should the strategy disappear on backtracking, then its away!
;;
;;


(mod~defmod suggr
            :uses (cri)
            :documentation "The suggestion reasoner"
            :exports (
                      
                      suggr~reasoner
                      
                      suggr*current-suggestion-control-rules
		      suggr*current-execution-message
		      ))


(defvar suggr*current-suggestion-control-rules nil)

(defvar suggr*current-execution-message nil)

#| --------------------------------------------------------- possible reactions ------------------------------------------------------ |#

(eval-when (load compile eval)
  (defclass suggr+reaction (keim+object)
    ((demands :initform nil
	      :initarg :demands
	      :accessor suggr~reaction-demands)
     (execution-message :initform nil
			:initarg :execution-message
			:accessor suggr~reaction-execution-message))))

(defun suggr~reaction-p (obj)
  (typep obj 'suggr+reaction))

(defmethod print-object ((suggr suggr+reaction) stream)
  (format stream ":A reaction on message ~A with demands ~A:")
  (suggr~reaction-execution-message suggr)
  (suggr~reaction-demands suggr))

(defun suggr~create-failure-reaction (demands execution-message)
  (make-instance 'suggr+reaction
		 :demands demands
		 :execution-message execution-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; termination reaction

(eval-when (load compile eval)
  (defclass suggr+termination-reaction (suggr+reaction)
    ((success :initform nil
	      :initarg :success
	      :accessor suggr~termination-reaction-success))))

(defun suggr~termination-reaction-p (obj)
  (typep obj 'suggr+termination-reaction))

(defmethod print-object ((suggr suggr+termination-reaction) stream)
  (format stream ":A termination reaction on message ~A with demands ~A and success ~A:"
	  (suggr~reaction-execution-message suggr)
	  (suggr~reaction-demands suggr)
	  (suggr~termination-reaction-success suggr)))

(defun suggr~create-termination-reaction (demands execution-message success)
  (make-instance 'suggr+termination-reaction
		 :demands demands
		 :execution-message execution-message
		 :success success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non termination reaction

(eval-when (load compile eval)
  (defclass suggr+non-termination-reaction (suggr+reaction)
    ()))

(defun suggr~non-termination-reaction-p (obj)
  (typep obj 'suggr+non-termination-reaction))

(defmethod print-object ((suggr suggr+non-termination-reaction) stream)
  (format stream ":A non termination reaction on message ~A with demands ~A:"
	  (suggr~reaction-execution-message suggr)
	  (suggr~reaction-demands suggr)))

(defun suggr~create-non-termination-reaction (demands execution-message)
  (make-instance 'suggr+non-termination-reaction
		 :demands demands
		 :execution-message execution-message))


#| ----------------------------------------------------- Der suggestion reasoner ---------------------------------------------------- |#



(defun suggr~reasoner (message)

  ;; set the current usable control rules on the permitted failure!
  (cri~remove-all-used-control-rules)
  (cri~set-used-control-rules! suggr*current-suggestion-control-rules)

  (setf suggr*current-execution-message message)
  
  (multiple-value-bind
      (results applied-crules)
      (cri~call '(t)
		:kind 'suggestion
		:pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))

    (if (equal results '(t))
	;; No control rule has fired
	;; -> no suggestions, default treatment of the message
	(suggr=interpret-message message)
      ;; otherwise there should be a reaction in the results
      (suggr=interpret-reaction (first (first results))))))


#| -------------------------------------------------------- interpret reaction ------------------------------------------------------ |#


(defgeneric suggr=interpret-reaction (reaction)
  (declare (edited  "27-AUG-1999")
	   (authors Ameier)
	   (input   "A suggestion reaction.")
	   (effect  "Depends on the reaction.")
	   (value   "Undefined."))

  (:method ((reaction suggr+termination-reaction))

	   ;; Execution of a strategy-ks gets terminated
	   ;; 1. ROC has to be removed from TASKS (any other TASKS, exp ...)
	   ;; 2. New strategy-step is created and inserted in strategy-steps
	   ;; 3. activ flag in state-des is set to nil
	   ;; (the termination so far)
	   ;; 4. New demands are entried in store.
	   ;; 5. success (i.e. successful termination)
	   ;;    a) -> If succes: All fulfilled demands are deleted from the demands store
	   ;;    b) -> Otherwise: In all tasks, that have this ROC, it is annotated in the strategy control, that this strategy failed on them!
	   ;; 5.b) -> better we let it!
	   
	   (let* ((demands (suggr~reaction-demands reaction))
		  (exmes (suggr~reaction-execution-message reaction))
		  (success (suggr~termination-reaction-success reaction))
		  (state-des (exmes~state-description exmes)))

	     (when (null state-des)
	       (error "~%Can not terminate before start, see function suggr=interpret-reaction"))
	     
	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF REACTION**: TERMINATE EXECUTION of strategy-ks ~A on task ~A."
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF REACTION**: NEW DEMANDS ~A are stored, the state description is deleted from all tasks and gets inactive."
			      demands))

	     
	     ;; 1. + 5b) Remove the ROCS! + entry in control
	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		    (strategy-ks (roc~strategy-ks state-des))
		    (all-tasks (agenda~all-tasks agenda)))
	       (mapcar #'(lambda (task)
			   (let* ((rocs (agenda~task-rocs task)))
			     (when (find state-des rocs)
			       ;; zu 1.
			       (setf (agenda~task-rocs task) (remove state-des rocs))
			       ;; zu 5b) -> better we let it!
			       ;;(when (null success)
			       ;; (setf (agenda~task-already-applied-strategy-ks task)
			       ;;	       (remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
			       ;;					(agenda~task-already-applied-strategy-ks task))
			       ;;				  :test #'keim~equal)))
			       )))
		       all-tasks))
	     
	     ;; 2. New strategy-step is created and inserted in strategy-steps
	     (let* (;; an end-strategy-application-justification is created!
		    (new-justification (strat~end-strategy-ks-application-create-just (list state-des)))
		    ;; Thereto a new scon step is created:
		    (strategy-end-step (scon~create-scon-step nil nil new-justification)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! strategy-end-step))
	     
	     ;; 3. activ flag in state-des is set to nil
	     (setf (roc~activ state-des) nil)
	     
	     
	     ;; 4. If new demands -> add to demands-store
	     (when demands
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands))

	     ;; 5a) If success -> delete fulfilled demands
	     (when success
	       (multiple-value-bind
		   (store2 rem del)
		   (store~demands-store-remove-fulfilled-demands! (black~get-blackboard-object-content 'demands sod*control-blackboard)
								  exmes)
		 
		 ;; The delete demands are annotated to the last step (which should have a termination just)
		 (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			   'fulfilled-demands
			   del)
		 
		 (when sod*verbose
		   (omega~message "**SOD:: INTERPRETATION OF REACTION**: The following demands are fulfilled and thus removed: ~A" del))))))

  (:method ((reaction suggr+non-termination-reaction))

	   ;; Execution of a strategy-ks gets interrupted!
	   ;; 1. store demands in demands store
	   ;; 2. store demands in state-description (-> i.e. state-description depends from these demands)
	   ;; 3. store state-description in store (-> i.e. can be reinvoked)
	   ;; 4. New strategy interruption step
	   ;; 5. demands are also annotated to this strategy interruption step.
	   ;; Note: ROCS entries remain in TASKS!!!!!!!

	   
	   (let* ((demands (suggr~reaction-demands reaction))
		  (exmes (suggr~reaction-execution-message reaction))
		  (state-des (exmes~state-description exmes)))

	     (when (null state-des)
	       (error "~% Handling of start message non awaited ... in function suggr=interpret-reaction"))

	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF REACTION**: NON TERMINATE EXECUTION of strategy-ks ~A on task ~A."
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF REACTION**: NEW DEMANDS ~A are stored, the state description is stored and not deleted from tasks and stays active."
			      demands))
	     
	     
	     ;; 1. add demands in demands-store
	     (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				  demands)
	     
	     ;; 2. store demands in state-description
	     (setf (roc~demands state-des) demands)
	     
	     ;; 3. store state-description in store
	     (store~add-element! (black~get-blackboard-object-content 'store sod*solution-blackboard) state-des)
	     
	     ;; 4. New strategy interruption step
	     (let* (;; New interrupt-strategy-ks justification
		    (new-interrupt-strategy-ks-application-just
		     (strat~interrupt-strategy-ks-application-create-just (list state-des (roc~copy-state-description state-des)) ))
		    ;; New strategy-step from this justification
		    (interrupt-step (scon~create-scon-step nil nil new-interrupt-strategy-ks-application-just)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! interrupt-step))
	     
	     ;; 5. The posed demands are annotated to the last step (that should have an interruption just)
	     (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
		       'posed-demands
		       demands))))


#| -------------------------------------------------------- interpret messages ------------------------------------------------------- |#

(defgeneric suggr=interpret-message (exmes)
  (declare (edited  "27-AUG-1999")
	   (authors Ameier)
	   (input   "An execution message.")
	   (effect  "Depends on the message, and on the refinement operation which produced it.")
	   (value   "Undefined."))
  (:method ((exmes exmes+start-message))

	   ;; The following is the default treatment in case of a start message:
	   ;; Nothing

	   nil
	   )
  (:method ((exmes exmes+interruption-message))
	   
	   ;; The following is the default treatment in case of an interruption message:
	   ;; Execution of strategy-ks was interrupted ->
	   ;; 1. store demands in demands store
	   ;; 2. store demands in state-description (-> i.e. state-description depends on these demands)
	   ;; 3. store state-description in store (-> i.e. can be reinvoked)
	   ;; 4. New strategy interruption step
	   ;; 5. demands are also annotated to this strategy interruption step.
	   ;; Note: ROCS entries remain in TASKS!!!!!!! 

	   (let* ((state-des (exmes~state-description exmes)))
	   
	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF EXMES**: MESSAGE OF INTERRUPTED EXECUTION from strategy-ks ~A on start-task ~A"
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF EXMES**: Demands ~A and state-description are stored."
			      (exmes~interruption-message-demands exmes)))

	     (let* ((demands (exmes~interruption-message-demands exmes)))
	       
	       ;; 1. add demands to demands-store 
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands)
	       
	       ;; 2. store demands in state-description
	       (setf (roc~demands state-des) demands)
	       
	       ;; 3. store state-description in store
	       (store~add-element! (black~get-blackboard-object-content 'store sod*solution-blackboard) state-des)
	       
	       ;; 4. New strategy interruption step
	       (let* (;; New interrupt-strategy-ks justification
		      (new-interrupt-strategy-ks-application-just
		       (strat~interrupt-strategy-ks-application-create-just (list state-des (roc~copy-state-description state-des))))
		      ;; New strategy-step from this justification
		      (interrupt-step (scon~create-scon-step nil nil new-interrupt-strategy-ks-application-just)))
		 
		 ;; Strategy-step is inserted in strategy-steps
		 (sod~introduce-new-strategy-ks-step! interrupt-step))
	       
	       ;; 5. The posed demands are annotated to the last step (which should have an interruption just)
	       (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			 'posed-demands
			 demands))))
  (:method ((exmes exmes+termination-message))
	   
	   ;; The following is the default treatment in case of a termination message:
	   ;; Execution of a strategy-ks terminates proper ->
	   ;; 1. ROC has to be removed from TASKS (any other TASKS, exp ...)
	   ;; 2. New strategy-step is created and inserted in strategy-steps
	   ;; 3. activ flag in state-des is set to nil
	   ;; 4. All fulfilled demands are removed from the demands store
	   ;; 5. Evtl. new Demands are added

	   (let* ((state-des (exmes~state-description exmes))
		  (demands (exmes~termination-message-demands exmes)))

	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF EXMES**: MESSAGE OF TERMINATED EXECUTION from strategy-ks ~A on start-task ~A"
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF EXMES**: NEW Demands ~A are stored, the state description is deleted from all tasks and gets inactive."
			      demands))
	     
	     ;; 1. Remove the ROCS!
	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		    (all-tasks (agenda~all-tasks agenda)))
	       (mapcar #'(lambda (task)
			   (let* ((rocs (agenda~task-rocs task)))
			     (setf (agenda~task-rocs task) (remove state-des rocs))))
		       all-tasks))
	     
	     ;; 2. New strategy-step is created and inserted in strategy-steps
	     (let* (;; an end-strategy-application-justification is created!
		    (new-justification (strat~end-strategy-ks-application-create-just (list state-des)))
		    ;; Thereto a new scon step is created:
		    (strategy-end-step (scon~create-scon-step nil nil new-justification)))
	       
	       ;; Strategy-step is inserted in Strategy-steps
	       (sod~introduce-new-strategy-ks-step! strategy-end-step))
	     
	     ;; 3. activ flag in state-des is set to nil
	     (setf (roc~activ state-des) nil)
	     
	     
	     ;; 4. The fulfilled demands are removed!
	     (multiple-value-bind
		 (store2 rem del)
		 (store~demands-store-remove-fulfilled-demands! (black~get-blackboard-object-content 'demands sod*control-blackboard)
								exmes)
	       
	       ;; The removed demands are annotated to the last step (which should have ab termination just)
	       (keim~put (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)
			 'fulfilled-demands
			 del)
	       
	       (when sod*verbose
		 (omega~message "**SOD:: INTERPRETATION OF EXMES**: The following demands are fulfilled and thus removed: ~A" del)))

	     ;; 5. If new demands -> add to demands-store
	     (when demands
	       (store~add-elements! (black~get-blackboard-object-content 'demands sod*control-blackboard)
				    demands))))
  
  (:method ((exmes exmes+failure-message))
	   
	   ;; The following is the default treatment for a failure message
	   ;; Execution of a strategy-ks fails for some reasons!
	   ;; -> State-description gets terminated, i.e.
	   ;;    1. ROC has to be removed from TASKS (any other TASKS, exp ...)
	   ;;    2. New strategy-step is created and inserted in strategy-steps
	   ;;    3. activ flag in state-des is set to nil
	   ;;    State-des is NOT STORED!
	   ;;    No DEMANDS get fulfilled !
	   ;; 4. In all tasks, that have this ROC, it is annotated in the strategy control, that this strategy has failed on them!
	   ;; 4.-> -> better we let it!
	   
	   (let* ((state-des (exmes~state-description exmes))
		  (strategy-ks (roc~strategy-ks state-des)))
	     
	     (when sod*verbose
	       (omega~message "~%**SOD:: INTERPRETATION OF EXMES**: MESSAGE OF FAILED EXECUTION from strategy-ks ~A on start-task ~A"
			      (roc~strategy-ks state-des)
			      (roc~start-task state-des))
	       (omega~message "**SOD:: INTERPRETATION OF EXMES**: The state description is deleted from all tasks and gets inactive."
			      demands))

	     ;; 1. + 4. Remove the ROCS + Entries in the task control, that the strategy-ks has failed on them
	     (let* ((agenda (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard)))
		    (all-tasks (agenda~all-tasks agenda)))
	       (mapcar #'(lambda (task)
			   (let* ((rocs (agenda~task-rocs task)))
			     (when (find state-des rocs)
			       ;; to 1.
			       (setf (agenda~task-rocs task) (remove state-des rocs))
			       ;; to 4. -> better we let it!
			       ;;(setf (agenda~task-already-applied-strategy-ks task)
			       ;;	     (remove-duplicates (cons (list strategy-ks (roc~parameters state-des))
			       ;;				      (agenda~task-already-applied-strategy-ks task))
			       ;;				:test #'keim~equal))
			       )))
		       all-tasks))
	     
	     ;; 2. New strategy-step is created and inserted strategy-steps
	     (let* (;; an end-strategy-application-justification is created!
		    (new-justification (strat~end-strategy-ks-application-create-just (list state-des)))
		    ;; Thereto a new scon step is created:
		    (strategy-end-step (scon~create-scon-step nil nil new-justification)))
	       
	       ;; Strategy-step is inserted in strategy-steps
	       (sod~introduce-new-strategy-ks-step! strategy-end-step))
	     
	     ;; 3. activ flag in state-des is set to nil
	     (setf (roc~activ state-des) nil)
	     ))
  )
