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


(mod~defmod ROC 
            :uses (agenda black keim pds sod store)
            :documentation "Stuff and data-structures for state description of refinement operation calls (roc's)"
            :exports (
                      roc+backtrack-state-description
                      roc+expansion-state-description
                      roc+instmeta-state-description
                      roc+pplanner-state-description
                      roc+state-description
                      
                      roc~activ
                      roc~backtrack-removed-steps
                      roc~backtrack-state-description-p
                      roc~create-backtrack-state-description
                      roc~create-expansion-state-description
                      roc~create-instmeta-state-description
                      roc~create-pplanner-state-description
                      roc~demands
                      roc~demands-are-fulfilled-p
                      roc~expansion-new-open-nodes
                      roc~expansion-state-description-p
                      roc~first-task-in-agenda-carry-roc-p
                      roc~fresh-backtrack-state-description
                      roc~fresh-expansion-state-description
                      roc~fresh-instmeta-state-description
                      roc~fresh-pplanner-state-description
                      roc~instmeta-state-description-p
                      roc~parameters
                      roc~pplanner-new-lines
                      roc~pplanner-outline-lines
                      roc~pplanner-state-description-p
                      roc~pplanner-steps
                      roc~start-step
                      roc~start-task
                      roc~state-description-p
                      roc~strategy-ks
                      ))

;; in this module the data-structures for refinement operation call state-descriptions are given. State descriptions describe the current
;; state of the execution of a strategy-ks on the solution blackboard. If such an execution is interrupted this state has to be stored.
;; The idea is that an execution is interrupted since first other applications need to be done before this execution can be reinvoked.
;; Thus if a execution is interrupted it places so-called demands on the solution blackboard. If this demands are fulfilled the execution
;; can be continued.
;; A refinement operation call state-description should be direclty produced if a new strategy-ks is invoked. The task on which the
;; strategy-ks is called, is marked with this ROC, that's necessary because:
;; 
;; 1.) There is only one global agenda!
;; 2.) If a new strategy-ks is called, it creates a new refinement-operation-call (new name for state-description)
;;     and marks the task, on which it was called, with this new ROC (in the slot refinement-operation-calls).
;; 3.) PPlanner is only allowed to work on tasks, that are annotated with its own ROC!
;; 4.) If a method is applied, the ROC entry is inherited from the start task to all created tasks.
;; 5.) If an execution is interrupted, the ROC is put to the store.                                      
;; 6.) A ROC from the store is called again, if all its demands are fulfilled + one of its tasks (i.e. that have its ROC)
;;     is amoung the actual first-tasks.
;;
;; Nevertheless the main sense of such a roc-state-description is to be saved in the store and to be recalled again later.
;; Thus each ROC state-description consists of a strategy-ks, a start-task, and a list of demands (which was placed
;; on the control blackboard). Furthermore a state-description has slots depending on the refinement algorithm of the strategy-ks.


#| -------------------------------------------------- State-description (allgemein) --------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+state-description (keim+object)
    ((strategy-ks :initform nil
		  :initarg :strategy-ks
		  :accessor roc~strategy-ks)
     (start-task :initform nil
		 :initarg :start-task
		 :accessor roc~start-task)
     (parameters :initform nil
		 :initarg :parameters
		 :accessor roc~parameters)
     (demands :initform nil
	      :initarg :demands
	      :accessor roc~demands)
     (start-step :initform nil
		 :initarg :start-step
		 :accessor roc~start-step)
     (activ :initform nil
	    :initarg :activ
	    :accessor roc~activ))))

(defmethod print-object ((roc roc+state-description) stream)
  (format stream "<ROC-State-description for strategy-ks ~A on start-task ~A with demands ~A and start-step ~A and activ ~A and parameters ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~demands roc)
	  (roc~start-step roc)
	  (roc~activ roc)
	  (roc~parameters roc)))

(defun roc~state-description-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a state-description, nil otherwise."))
  (typep obj 'roc+state-description))


#| --------------------------------------------------- PPlanner State Description ---------------------------------------------------- |#

;; The following is the concrete state description for pplanner. The state of pplanner is additionally described by: all nodes,
;; that resulted from the strategy execution so far.

(eval-when (load compile eval)
  (defclass roc+pplanner-state-description (roc+state-description)
    ((new-lines :initform nil
		:initarg :new-lines
		:accessor roc~pplanner-new-lines)
     (outline-lines :initform nil
		    :initarg :outline-lines
		    :accessor roc~pplanner-outline-lines)
     (steps :initform nil
	    :initarg :steps
	    :accessor roc~pplanner-steps)
     (run-time :initform nil
	       :initarg :run-time
	       :accessor roc~pplanner-run-time)
     (random-ssed :initform nil
		  :initarg :random-seed
		  :accessor roc~pplanner-random-seed))))

(defmethod print-object ((roc roc+pplanner-state-description) stream)
  (format stream "<PPLANNER-STATE: STRATEGY-KS ~A on START-TASK ~A with ~%     NEW-LINES ~A~%     OUTLINE-LINES ~A~%     Start-Step ~A~%      Steps ~A~%     Activ ~A~%     Parameters ~A~%     RUN-Time ~A~%     Random-Seed ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~pplanner-new-lines roc)
	  (roc~pplanner-outline-lines roc)
	  (roc~start-step roc)
	  (roc~pplanner-steps roc)
	  (roc~activ roc)
	  (roc~parameters roc)
	  (roc~pplanner-run-time roc)
	  (if (roc~pplanner-random-seed roc)
	      'given
	    nil)
	  ))

(defun roc~pplanner-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+pplanner-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+pplanner-state-description))

(defun roc~create-pplanner-state-description (strategy-ks demands start-task new-lines outline-lines start-step steps activ parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, start-task, an agenda, a list of new lines, a list"
		    "of outline-lines, a start-step, activ flag, the parameters.")
	   (effect  "NOne.")
	   (value   "A new PPlanner-state-description."))
  (make-instance 'roc+pplanner-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :outline-lines outline-lines
		 :new-lines new-lines
		 :start-step start-step
		 :steps steps
		 :activ activ
		 :parameters parameters
		 :run-time 0
		 :random-seed nil
		 ))

(defmethod roc~copy-state-description ((stades roc+pplanner-state-description))
  (roc~create-pplanner-state-description (roc~strategy-ks stades)
					 (copy-list (roc~demands stades))
					 (roc~start-task stades)
					 (copy-list (roc~pplanner-new-lines stades))
					 (copy-list (roc~pplanner-outline-lines stades))
					 (roc~start-step stades)
					 (copy-list (roc~pplanner-steps stades))
					 (roc~activ stades)
					 (copy-list (roc~parameters stades))))

(defun roc~fresh-pplanner-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create a pplanner state-description"
		    "and the parameters.")
	   (effect  "None.")
	   (value   "A PPlanner state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-pplanner-state-description strategy-ks nil start-task
							 nil (list (agenda~task-node start-task))
							 start-step nil 't parameters)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))


#| --------------------------------------------------- LPlanner State Description ---------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+lplanner-state-description (roc+state-description)
    ()))

(defmethod print-object ((roc roc+lplanner-state-description) stream)
  (format stream "<LPLANNER-STATE: STRATEGY-KS ~A on START-TASK ~A"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  ))

(defun roc~lplanner-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+lplanner-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+lplanner-state-description))

(defun roc~create-lplanner-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks and astart-task.")
	   (effect  "None.")
	   (value   "A new LPlanner-state-description."))
  (make-instance 'roc+lplanner-state-description
		 :strategy-ks strategy-ks
		 :start-task start-task
		 :start-step start-step
		 :parameters parameters
		 ))

(defmethod roc~copy-state-description ((stades roc+lplanner-state-description))
  )

(defun roc~fresh-lplanner-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create a lplanner state-description"
		    "and the parameters.")
	   (effect  "None.")
	   (value   "A LPlanner state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-lplanner-state-description strategy-ks start-task start-step parameters)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))

#| ----------------------------------------------------------- Inst-Meta-ROC ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+instmeta-state-description (roc+state-description)
    ((last-plan-step :initform nil
		     :initarg :last-plan-step
		     :accessor roc~instmeta-last-plan-step
		     :documentation "The last plan-step-before a meta-variable instantiation.")
     (last-cpool :initform nil
		 :initarg :last-cpool
		 :accessor roc~instmeta-last-cpool
		 :documentation "The last cpool before a meta-variable instantiation."))))


(defmethod print-object ((roc roc+instmeta-state-description) stream)
  (format stream "<INSTMETA-STATE: STRATEGY-KS ~A on START-TASK ~A and Start-step ~A and activ ~A and parameters ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~start-step roc)
	  (roc~activ roc)
	  (roc~parameters roc)
	  ))

(defun roc~instmeta-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+instmeta-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+instmeta-state-description))

(defun roc~create-instmeta-state-description (strategy-ks demands start-task start-step activ parameters last-plan-step last-cpool)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, a start-task and a start-step, a activ flag and parameters.")
	   (effect  "NOne.")
	   (value   "A new InstMeta-state-description."))
  (make-instance 'roc+instmeta-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :start-step start-step
		 :activ activ
		 :parameters parameters
		 :last-plan-step last-plan-step
		 :last-cpool last-cpool
		 ))

(defmethod roc~copy-state-description ((stades roc+instmeta-state-description))
  )

(defun roc~fresh-instmeta-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create an InstMeta state-description."
		    "And parameters.")
	   (effect  "None.")
	   (value   "An InstMeta state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-instmeta-state-description strategy-ks nil start-task start-step nil parameters nil nil)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))

#| ----------------------------------------------------------- BACKTRACK-ROC ---------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+backtrack-state-description (roc+state-description)
    ((removed-steps :initform nil
		    :initarg :removed-steps
		    :accessor roc~backtrack-removed-steps))))

(defmethod print-object ((roc roc+backtrack-state-description) stream)
  (format stream "<BackTrack-STATE: STRATEGY-KS ~A on START-TASK ~A and Start-step ~A, removed-steps ~A, and activ ~A and parameters ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~start-step roc)
	  (roc~backtrack-removed-steps roc)
	  (roc~activ roc)
	  (roc~parameters roc)
	  ))

(defun roc~backtrack-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+backtrack-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+backtrack-state-description))

(defun roc~create-backtrack-state-description (strategy-ks demands start-task start-step removed-steps activ parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, a start-task, a start-step, removed-steps, a activ flag and parameters.")
	   (effect  "NOne.")
	   (value   "A new BackTrack-state-description."))
  (make-instance 'roc+backtrack-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :start-step start-step
		 :removed-steps removed-steps
		 :activ activ
		 :parameters parameters))

(defun roc~fresh-backtrack-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create a BackTrack state-description and parameters.")
	   (effect  "None.")
	   (value   "A BackTrack state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-backtrack-state-description strategy-ks nil start-task start-step nil nil parameters)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))


#| ----------------------------------------------------------- EXPANSION-ROC ---------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+expansion-state-description (roc+state-description)
    ((new-nodes :initform nil
		     :initarg :new-nodes
		     :accessor roc~expansion-new-nodes))))

(defmethod print-object ((roc roc+expansion-state-description) stream)
  (format stream "<EXP-STATE: STRATEGY-KS ~A on START-TASK ~A and Start-step ~A, new-nodes ~A, and activ ~A and parameters ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~start-step roc)
	  (roc~expansion-new-nodes roc)
	  (roc~activ roc)
	  (roc~parameters roc)
	  ))

(defun roc~expansion-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+expansion-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+expansion-state-description))

(defun roc~create-expansion-state-description (strategy-ks demands start-task start-step new-nodes activ parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, a start-task, a start-step, new-nodes, a activ flag and parameters.")
	   (effect  "NOne.")
	   (value   "A new Expansion-state-description."))
  (make-instance 'roc+expansion-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :start-step start-step
		 :new-nodes new-nodes
		 :activ activ
		 :parameters parameters))

(defun roc~fresh-expansion-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create an Expansion state-description and parameters.")
	   (effect  "None.")
	   (value   "An Expansion state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-expansion-state-description strategy-ks nil start-task start-step nil nil parameters)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))


#| ----------------------------------------------------------- ATP-ROC ---------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass roc+atp-state-description (roc+state-description)
    ((atp-out :initform nil
	      :initarg :atp-out
	      :accessor roc~atp-atp-out))))

(defmethod print-object ((roc roc+atp-state-description) stream)
  (format stream "<ATP-STATE: STRATEGY-KS ~A on START-TASK ~A and Start-step ~A, atp-out ~A, and activ ~A and parameters ~A>"
	  (roc~strategy-ks roc)
	  (roc~start-task roc)
	  (roc~start-step roc)
	  (roc~atp-atp-out roc)
	  (roc~activ roc)
	  (roc~parameters roc)
	  ))

(defun roc~atp-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+atp-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+atp-state-description))

(defun roc~create-atp-state-description (strategy-ks demands start-task start-step atp-out activ parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, a start-task, a start-step, atp-out, a activ flag and parameters.")
	   (effect  "NOne.")
	   (value   "A new atp-state-description."))
  (make-instance 'roc+atp-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :start-step start-step
		 :atp-out atp-out
		 :activ activ
		 :parameters parameters))

(defun roc~fresh-atp-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create an atp state-description and parameters.")
	   (effect  "None.")
	   (value   "An atp state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-atp-state-description strategy-ks nil start-task start-step nil nil parameters)))
    
    (setf (agenda~task-rocs start-task)
	  (cons new-roc (agenda~task-rocs start-task)))
    
    new-roc))





#| --------------------------------------------------------- ROC TEST FUNCTIONS ------------------------------------------------------- |#

(defun roc~demands-are-fulfilled-p (stades)
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state description (on the store).")
	   (effect  "None.")
	   (value   "T if there are no demands in the demands slot which are also in the demands entry"
		    "on the control blackboard, nil otherwise."))
  
  (let* ((demands-of-roc (roc~demands stades))
	 (demands-of-control-bb (store~elements (black~get-blackboard-object-content 'demands sod*control-blackboard)))
	 (demands-of-roc-also-on-control-bb (remove-if-not #'(lambda (roc-demand)
							       (find roc-demand demands-of-control-bb :test #'eq))
							   demands-of-roc)))

    ;;(format t "~%demands-of-roc ~A" demands-of-roc)
    ;;(format t "~%demands-of-control-bb ~A" demands-of-control-bb)
    ;;(format t "~%demands-of-roc-also-on-control-bb ~A" demands-of-roc-also-on-control-bb)
        
    (if demands-of-roc-also-on-control-bb
	nil
      't)))

(defun roc~first-task-in-agenda-carry-roc-p (stades)
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "A ROC state description (on the store).")
	   (effect  "None.")
	   (value   "Takes the agenda of the pds on the solution blackboard and computes the first tasks"
		    "on this agenda. T if one of the first tasks carries the ROC in its ROCS slot, nil otherwise."))
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (first-tasks (pds~first-tasks! pds agenda)))
    
    (if (remove-if-not #'(lambda (task)
			   (find stades (agenda~task-rocs task) :test #'eq))
		       first-tasks)
	't
      nil)))




#| --------------------------------------------------- Analogy State Description ---------------------------------------------------- |#

;; The following is the concrete state description for analogy.


(defvar ana*roc-state-description nil)

(eval-when (load compile eval)
  (defclass roc+analogy-state-description (roc+pplanner-state-description)
    (
     (subst           :initform nil
		      :initarg :subst
		      :accessor roc~analogy-subst)
     (table           :initform nil
		      :initarg  :table
		      :accessor roc~analogy-table)
     (current-action  :initform nil
		      :initarg current-action
		      :accessor roc~analogy-current-action)
     (actions         :initform nil
		      :initarg  :actions
		      :accessor roc~analogy-actions)
    )))

(defmethod print-object ((roc roc+analogy-state-description) stream)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and a stream")
	   (effect  "Writes the state description to the stream")
	   (value   "Undefined"))
  (format stream "<ANALOGY-STATE: STRATEGY-KS ~A on START-TASK ~A>"
	  (keim~name (roc~strategy-ks roc))
	  (roc~start-task roc)))

(defun roc~analogy-state-description-p (obj)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is of class roc+analogy-state-description, nil"
		    "otherwise."))
  (typep obj 'roc+analogy-state-description))

(defun roc~create-analogy-state-description (strategy-ks demands start-task new-lines outline-lines start-step steps actions
							 activ parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, demands, start-task, an agenda, a list"
		    "of outline-lines, a start-step, activ flag, the parameters.")
	   (effect  "NOne.")
	   (value   "A new ANALOGY-state-description."))
  (make-instance 'roc+analogy-state-description
		 :strategy-ks strategy-ks
		 :demands demands
		 :start-task start-task
		 :new-lines new-lines
		 :outline-lines outline-lines
		 :start-step start-step
		 :steps steps
		 :actions actions
		 :activ activ
		 :parameters parameters
		 ))

(defun roc~fresh-analogy-state-description (strategy-ks start-task start-step parameters)
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A strategy-ks, a start-task and a start-step to create an analogy state-description"
		    "and the parameters.")
	   (effect  "None.")
	   (value   "Am ANALOGY state-description with strategy-ks and start-task."
		    "The new roc is inserted into the rocs slot of the start-task."))
  (let* ((new-roc (roc~create-analogy-state-description strategy-ks nil start-task nil
							 (list (agenda~task-node start-task))
							 start-step nil nil 't parameters)))
    (mapc #'(lambda (task) (setf (agenda~task-rocs task)
				 (cons new-roc (agenda~task-rocs task))))
	  (union (list start-task) (first parameters)))
    new-roc))

(defmethod roc~copy-state-description ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "A copy of the state description"))
  (roc~create-analogy-state-description (roc~strategy-ks roc)
					 (copy-list (roc~demands roc))
					 (roc~start-task roc)
					 (copy-list (roc~pplanner-new-lines roc))
					 (copy-list (roc~pplanner-outline-lines roc))
					 (roc~start-step roc)
					 (copy-list (roc~pplanner-steps roc))
					 (copy-list (roc~analogy-actions roc))
					 (roc~activ roc)
					 (copy-list (roc~parameters roc))))

(defmethod roc~analogy-append-action ((roc roc+analogy-state-description) action)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and an action")
	   (effect  "None")
	   (value   "Adds the action to the state description"))
  (setf (roc~analogy-actions roc) (append (roc~analogy-actions roc) (list action))))

(defmethod roc~analogy-mapping-already-applied-p ((roc roc+analogy-state-description) mapping)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and a mapping")
	   (effect  "None")
	   (value   "True, iff the state description contains an action, that represents the same as the mapping"))
  (some #'(lambda (action) (ana~action-already-applied-p action mapping)) (roc~analogy-actions roc)))

(defmethod roc~analogy-find-action-of-step ((roc roc+analogy-state-description) step)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and a step")
	   (effect  "None")
	   (value   "The action corresponding to the step, if there is one, otherwise nil"))
  (find-if #'(lambda (action) (ana~action-fulfilled-by-p action step))
	   (roc~analogy-actions roc)))

(defmethod roc~analogy-applied-actions ((roc roc+analogy-state-description) pds)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and a pds")
	   (effect  "None")
	   (value   "All applied action in the state description"))
  (remove-if 'null (mapcar  #'(lambda (step) (roc~analogy-find-action-of-step roc step)) (ana~all-steps pds))))

(defmethod roc~analogy-last-source ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The source of the last introduced action within the strategy of the state description"))
  (let ((last-action (first (last (roc~analogy-applied-actions roc omega*current-proof-plan)))))
    (when last-action (ana~action-source last-action))))
  
(defmethod roc~analogy-tasks ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The tasks, the analogy strategy was invoked on"))
  (first (roc~parameters roc)))

(defmethod roc~analogy-source-plan ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The source plan for the analogy"))
  (second (roc~parameters roc)))

(defmethod roc~analogy-source-steps ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The source steps to transfer by analogy"))
  (third (roc~parameters roc)))

(defmethod roc~analogy-source-superstep ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The super step of the source steps, if there is one, otherwise nil"))
  (fourth (roc~parameters roc)))

(defmethod roc~analogy-parameters ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The parameters of the strategy invokation"))
  (rest (roc~parameters roc)))

(defmethod roc~analogy-parameter-names ((roc roc+analogy-state-description))
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description")
	   (effect  "None")
	   (value   "The names of the parameters of the strategy invokation"))
  (ana=ref-parameter 'parameters roc))

(defmethod roc~analogy-get-parameter ((roc roc+analogy-state-description) parameter-name)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and the name of a parameter")
	   (effect  "None")
	   (value   "The parameter of the strategy invokation with the given name"))
  (nth (position parameter-name (roc~analogy-parameter-names roc)) (roc~analogy-parameters roc)))

(defun roc~analogy-table-init (roc tasks)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description and a list of tasks")
	   (effect  "Initializes the corresponendence table")
	   (value   "Undefined"))
  (let ((source-plan (roc~analogy-source-plan roc))
	(steps (roc~analogy-source-steps roc)))
    (setf (roc~analogy-table roc)
	  (ana~cor-initialize (list (list (ana~step-exist-conclusions steps)
					  (mapcar 'agenda~task-node tasks))
				    (list (ana~step-exist-premises steps)
					  (remove-duplicates (apply 'append (mapcar #'(lambda (task) (pds~node-supports (agenda~task-node task))) tasks))))
				    (list (ana~step-exist-metavars steps)
					  (subst~domain (ana~cstrpool-bindings omega*current-proof-plan)))
				    )))))

