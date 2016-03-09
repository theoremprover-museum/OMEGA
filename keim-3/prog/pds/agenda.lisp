;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

(in-package "KEIM")


(mod~defmod AGENDA 
            :uses (data term keim meta node)
            :documentation "Data Structures and Functions for the pds agenda"
            :exports (agenda+agenda
                      agenda+goal
                      agenda+goal-schema
                      agenda+ordering
                      agenda+pseudo-goal
                      agenda+task
		      agenda+empty-agenda
                      agenda+inst-task
		      
                      agenda~create
		      agenda~generate
		      agenda~create-goal-schema
                      agenda~create-goal
		      agenda~create-task
		      agenda~create-open-task
		      agenda~goal-schema-formula
		      agenda~schematic-formula-p
                      agenda~create-pseudo-goal
                      agenda~first-task
                      agenda~first-tasks!
		      agenda~all-tasks
		      agenda~update
		      agenda~update-goal-schemas!
		      agenda~get-first-task
		      agenda~get-tasks
                      agenda~insert-tasks
                      agenda~next-tasks
                      agenda~ordering
                      agenda~ordering-create
                      agenda~ordering-first-tasks
                      agenda~ordering-next-tasks
                      agenda~orderings
		      agenda~task-p
                      agenda~pseudo-goal-p
		      agenda~goal-p
		      agenda~goal-schema-p
                      agenda~replace-task
		      agenda~for-loui
                      agenda~task-node
		      agenda~goal-schematic-p
		      agenda~task-blocked-p
		      agenda~task-formula
		      agenda~task-rocs
		      agenda~task-already-applied-strategy-ks
                      agenda~then-agenda
		      agenda~empty-p
		      agenda~switch
		      agenda~in-use
		      agenda*print
		      agenda~inst-task-meta-var
		      agenda~inst-task-plan-step
		      agenda~inst-task-p
		      agenda~inst-task-create
		      agenda~p
		      agenda~goal-or-goal-schema-task-p
		      ))

(defvar agenda*in-use t)

(defvar agenda*print nil)

(defun agenda~switch (&optional (status (not agenda*in-use)))
  (setf agenda*in-use status))

(defun agenda~in-use ()
  agenda*in-use)

#|(eval-when (compile eval load)
(defclass agenda+task ()
  ((node :initarg :node
	 :accessor agenda~task-node
	 :documentation "The node of the task.")
   (blocked-p :initarg :blocked-p
	      :initform nil
	      :accessor agenda~task-blocked-p
	      :documentation "Whether this task is blocked."))
  (:documentation "Data structure for an agenda task."))|#

;; New VErsion Ameier
(eval-when (compile eval load)
  (defclass agenda+task ()
    ((rocs :initarg :rocs
	   :initform nil
	   :accessor agenda~task-rocs
	   :documentation "The node of the task.")    ;;; What the f... are rocs?????  VS
     (already-applied-strategy-ks :initarg :already-applied-strategiy-ks
				  :initform nil
				  :accessor agenda~task-already-applied-strategy-ks
				  :documentation "A list of all strategies/parameter pairs, already applied on the task.")
     (node :initarg :node
	   :initform nil
	   :accessor agenda~task-node
	   :documentation "The node of the task.")
     (blocked-p :initarg :blocked-p
		:initform nil
		:accessor agenda~task-blocked-p
		:documentation "Whether this task is blocked."))
    (:documentation "Data structure for an agenda task."))
  )
 
(defclass agenda+goal (agenda+task)
  ()
  (:documentation "Data structure for a goal task."))

(defclass agenda+goal-schema (agenda+task)
  (
;   (formula :initarg :formula
;            :initform nil
;            :accessor agenda=goal-schema-formula
;            :documentation "The current updated formula schema of the corresponding node.")
   ;; Durch die Einfuehrung von pdsn+schematic-node, this slot corresponds to the pdsn~current-formula
   ;; Only the flag goal-schematic-p is needed to know whether the pdsn~current-formula contains
   ;; meta-variables or not.
   ;; How to proceed:
   ;; - Fetching some goal-schema SG: when goal-schematic-p(SG) is set to T, then consider the node(SG):
   ;; When the up-to-date of this node is NIL, then compute the current-formula; set up-to-date to T, and
   ;; set goal-schematic-p(SG) to NIL, when the resulted current-formula does not contain meta-variables
   ;; 
   (schematic :initarg :schematic
	      :initform T
	      :accessor agenda~goal-schematic-p
	      :documentation "A boolean stating whether the task formula is schematic."))
  (:documentation "Data structure for a goal-schema task."))


(defclass agenda+pseudo-goal (agenda+task)
  ()
  (:documentation "Data structure for a pseudo-goal task."))

;; INST TASK

(eval-when (compile eval load)
  (defclass agenda+inst-task (agenda+task)
    ((meta-var :initarg :meta-var
	       :initform nil
	       :accessor agenda~inst-task-meta-var
	       :documentation "The meta-variable of an instantiation task (meta variable to instantiate).")
     (plan-step :initarg :plan-step
		:initform nil
		:accessor agenda~inst-task-plan-step
		:documentation "The plan step in that the meta-variable was produced."))
    (:documentation "Data structure for agenda meta variable instantiation tasks."))
  ;; Ein Instantiierungs Task besteht quasi aus einer Meta-Variable die geschlossen werden soll. 
  )

(defun agenda~inst-task-p (obj)
  (declare (edited  "08-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type agenda+inst-task, nil otherwise."))

  (typep obj 'agenda+inst-task))

(defmethod print-object ((task agenda+inst-task) stream)
  (format stream "<INST-TASK for Meta-Variable ~A from STEP ~A>"
	  (agenda~inst-task-meta-var task)
	  (agenda~inst-task-plan-step task)))

(defun agenda~inst-task-create (meta-var plan-step)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "A meta-variable.")
	   (effect  "None.")
	   (value   "A new Inst-TASK for the meta-variable."))
  (make-instance 'agenda+inst-task
		 :meta-var meta-var
		 :plan-step plan-step))

;;;;;;;;;;;;;;;;;;



(defclass agenda+ordering ()
  ((first :initarg :first
	  :accessor agenda~ordering-first-tasks
	  :documentation "Tasks to be considered first.")
   (next :initarg :next
	 :accessor agenda~ordering-next-tasks
	 :documentation "Tasks to be considered next."))
  (:documentation "Data structure for a plan agenda ordering."))

(defclass agenda+agenda ()
  ((first :initarg :first
	  :initform nil
	  :accessor agenda~first-task
	  :documentation "Task to be considered first.")
   (next :initarg :next
	 :initform nil
	 :accessor agenda~next-tasks
	 :documentation "Tasks to be considered next.")
   (orderings :initarg :orderings
	      :initform nil
	      :accessor agenda~orderings
	      :documentation "Orderings on the next tasks.")
   (then :initarg :then
	 :initform nil
	 :accessor agenda~then-agenda 
	 :documentation "A pointer to an agenda containing other tasks."))
  (:documentation "Data structure for a plan agenda."))

(defclass agenda+empty-agenda ()
  ()
  (:documentation "Data structure for an empty plan agenda."))

(defun agenda~p (obj)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type agenda+agenda, otherwise nil.")) 
  (typep obj 'agenda+agenda))


(defmethod print-object ((agenda agenda+agenda) stream)
  (if agenda*print
      (let ((first-task (agenda~first-task agenda))
	    (next-tasks (agenda~next-tasks agenda))
	    (then-agenda (agenda~then-agenda agenda)))
	(if first-task
	    (progn (format stream "~%first task: ~A" first-task)
		   (when next-tasks
		     (format stream "~%next tasks: ~{~A ~}~% ordered by: ~{~A ~}"
			     next-tasks (agenda~orderings agenda)))
		   (unless (agenda~empty-p then-agenda)
		     (format stream "~%THEN:")
		     (print-object then-agenda stream)))
	  (if next-tasks
	      (progn (format stream "~%next tasks: ~{~A ~}~% ordered by: ~{~A ~}"
			     next-tasks (agenda~orderings agenda))
		     (unless (agenda~empty-p then-agenda) 
		       (format stream "~%THEN:")
		       (print-object then-agenda stream)))
	    (print-object then-agenda stream))))
    (format stream "<Agenda>")))
     
(defmethod print-object ((agenda agenda+empty-agenda) stream)
  (format stream "Empty Agenda"))

(defun agenda~empty-p (object)
  (typep object 'agenda+empty-agenda))

(defmethod print-object ((goal agenda+goal) stream)
  (format stream "?goal#~A" (keim~name (agenda~task-node goal))))

(defmethod print-object ((goal agenda+goal-schema) stream)
  (format stream "?goal-schema#~A" (keim~name (agenda~task-node goal))))

(defun agenda~task-p (object)
  (typep object 'agenda+task))

(defun agenda~pseudo-goal-p (object)
  (typep object 'agenda+pseudo-goal))

(defun agenda~goal-p (object)
  (typep object 'agenda+goal))

(defun agenda~goal-schema-p (object)
  (typep object 'agenda+goal-schema))

(defun agenda~goal-or-goal-schema-task-p (task)
  (declare (edited  "10-JUN-1999")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "T if the task is a goal oder a goal-schema-task."))
  (if (or (agenda~goal-p task)
	  (agenda~goal-schema-p task))
      't
    nil))

(defmethod print-object ((goal agenda+pseudo-goal) stream)
  (format stream "!pseudo-goal#~A" (keim~name (agenda~task-node goal))))

(defmethod print-object ((ordering agenda+ordering) stream)
  (let ((tasks1 (mapcar #'keim~name (mapcar #'agenda~task-node
					    (agenda~ordering-first-tasks ordering))))
	(tasks2 (mapcar #'keim~name (mapcar #'agenda~task-node
					    (agenda~ordering-next-tasks ordering)))))
    (format stream "(~{~A, ~}~A  <  ~{~A, ~}~A)" (butlast tasks1) (first (last tasks1))
	    (butlast tasks2) (first (last tasks2)))
    ))

(defun agenda~create-pseudo-goal (clsd-node)
  (make-instance 'agenda+pseudo-goal
		 :node clsd-node))

(defun agenda~create-goal-schema (open-node)
  (declare (edited  "21-APR-1999")
	   (authors Lassaad)
	   (input   "An open node whose formula contains meta-variables.")
	   (effect  "None.")
	   (value   "The goal schema of OPEN-NODE."))
  (make-instance 'agenda+goal-schema
		 :node open-node))

(defun agenda~create-task (node)
  (cond ((pdsn~open-node-p node)
	 (agenda~create-open-task node))
	(T
	 (agenda~create-pseudo-goal node))
	))

(defun agenda~create-goal (open-node)
  (make-instance 'agenda+goal
		 :node open-node))

(defun agenda~create-open-task (node)
  (if (pdsn~schematic-p node) 
      (agenda~create-goal-schema node)
    (agenda~create-goal node)))
	
(defmethod agenda~goal-schema-formula ((goal-schema agenda+goal-schema))
  (or ;;(agenda=goal-schema-formula goal-schema)
   (pdsn~current-formula (agenda~task-node goal-schema))
   (node~formula (agenda~task-node goal-schema))))

(defsetf agenda~goal-schema-formula (goal-schema) (formula)
  `(if (agenda~goal-schema-p ,goal-schema)
       ;;(setf (agenda=goal-schema-formula ,goal-schema) ,formula)
       (setf (pdsn~current-formula (agenda~task-node ,goal-schema)) ,formula)
     (error ";;;agenda~~goal-schema-formula: ~A is not of type AGENDA+GOAL-SCHEMA."
	    ,goal-schema)))

(defgeneric agenda~schematic-formula-p (formula)
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T, when FORMULA contains meta-variables."))
  (:method ((formula term+term))
	   (remove-if-not #'meta~p (data~free-variables formula)))
;  (:method ((appl term+appl))
;           (or (agenda~schematic-formula-p (data~appl-function appl))
;               (some #'agenda~schematic-formula-p (data~appl-arguments appl))))
;  (:method ((abstr term+abstr))
;           (agenda~schematic-formula-p (data~abstr-range abstr)))
;  (:method ((mvar meta+variable))
;           T)
;  (:method (other)
;           (declare (ignore other))
;           nil))
  )

;(defgeneric agenda~schematic-formula-p (formula)
;  (declare (edited  "30-MAR-1998")
;           (authors Lassaad)
;           (input   "A formula.")
;           (effect  "None.")
;           (value   "T, when FORMULA contains meta-variables."))
;  (:method ((appl term+appl))
;           (or (agenda~schematic-formula-p (data~appl-function appl))
;               (some #'agenda~schematic-formula-p (data~appl-arguments appl))))
;  (:method ((abstr term+abstr))
;           (agenda~schematic-formula-p (data~abstr-range abstr)))
;  (:method ((mvar meta+variable))
;           T)
;  (:method (other)
;           (declare (ignore other))
;           nil))

(defun agenda~ordering-create (first-tasks next-tasks)
  (make-instance 'agenda+ordering
		 :first first-tasks
		 :next next-tasks))

(defun agenda~create (first-task next-tasks orderings then-agenda)
  (make-instance 'agenda+agenda
		 :first first-task
		 :next next-tasks
		 :orderings orderings
		 :then then-agenda))

(defun agenda~generate (tasks-list)
  (declare (edited  "04-NOV-1998")
	   (authors Lassaad)
	   (input   "A list of task lists.")
	   (effect  "None.")
	   (value   "The agenda containing all the tasks in TASKS-LIST and"
		    "considering the partial order given within it."))
  (if tasks-list
      (let ((tasks1 (first tasks-list)))
	(if (rest tasks1)
	    (agenda~create nil tasks1 nil (agenda~generate (rest tasks-list)))
	  (agenda~create (first tasks1) (second tasks-list) nil (agenda~generate (rest (rest tasks-list))))))
    (make-instance 'agenda+empty-agenda)))
    

(defgeneric agenda~task-formula (task)
  (declare (edited  "16-APR-1998")
	   (authors Lassaad)
	   (input   "An agenda task.")
	   (effect  "None.")
	   (value   "The currennt formula of the associated node."))
  (:method ((goal agenda+goal))
	   (node~formula (agenda~task-node goal)))
  (:method ((sgoal agenda+goal-schema))
	   (agenda~goal-schema-formula sgoal))
  (:method ((pgoal agenda+pseudo-goal))
	   (node~formula (agenda~task-node pgoal)))
  (:method (object)
	   (error ";;;agenda~task-formula: Dont know how determine the formula of ~A instance of ~A!"
		  object (type-of object)))
  )
  
	   

;;; Operationen der Agenda:
; AGENDA: first-task, next-tasks, orderings, then-agenda
; - first-task: is a task on the agenda to be considered first, when
;              the orderings delivers a sole alternative.
; - next-tasks: are tasks on the agenda to be considered next, they
;               are partially sorted by orderings
; - orderings: defined on elements in next-tasks
; - then-agenda: An agenda to be considered after the current agenda.
;                We use such a pointer, when we have a first-task G
;                reduced say to G1, G2, and G3 with the ordering G1 < G3. 
;                Here the current agenda is set to
;                agenda(NIL,(G1 G2 G3),(<G1>.<G3>),agenda(NIL,(...),(...),...))
;
; * Getting the tasks to consider first on the agenda:
; if first-task then return first-task
; else determine a sublist of next-tasks using orderings as follows:
;     - if ORDERINGS
;       then: if (rest next-tasks)
;             then: ordering-after := after(first (ORDERINGS))
;		    recursive(next-tasks \ ordering-after, (rest ORDERINGS), agenda)
;             else: first-task(agenda) := (first next-tasks)
;                   next-tasks(agenda) := (remove first-task next-tasks)
;                   orderings(agenda) := (remove-task first-task orderings(agenda))
;;
;; When first-task and next-tasks are both NIL, then then-agenda becomes the
;; current agenda.
;; Remove a task from an ordering: If the task is selected as first task, then it
;; must be in the before(ordering). It cannot be in an after(ordering), otherwise
;; it was deleted from next-tasks. When the task is deleted by backtracking, then
;; it must be deleted from before(ordering) and after(ordering).
;; 
; * Inserting new tasks and new orderings instead of an old task:
; 1) When 'oldT is first-task(agenda) and new-tasks is not empty' then:
;    1.A: if (rest new-tasks)
;         then: agenda(NIL,new-tasks,new-orderings,old-agenda\{oldT})
;         else: old-agenda[oldT\first(new-tasks)]
; 2) new-tasks is empty and oldT is first-task(agenda):
;    - remove oldT
;    - when next-tasks(agenda) = NIL then return then-agenda(agenda)
; 3) new-tasks is not empty and oldT is in next-tasks:
;    - Substitute oldT by new-tasks in next-tasks and in old-orderings
;    - Insert new-orderings
; 4) orderings without first-of-all

(defun agenda~for-loui (agenda)
  (declare (edited  "04-NOV-1998")
	   (authors Lassaad)
	   (input   "An agenda.")
	   (effect  "None.")
	   (value   "A list of disjunctive task lists, where the union of these lists"
		    "contains all the tasks of the AGENDA and the partial order in"
		    "AGENDA is given by the order of these lists."))
  (unless (agenda~empty-p agenda)
    (let ((agenda-ft (agenda~first-task agenda))
	  (agenda-nts (agenda~next-tasks agenda))
	  (orderings (agenda~orderings agenda)))
      (if agenda-ft
	  (cons (list agenda-ft)
		(append (if orderings
			    (agenda=consider-orderings! agenda-nts orderings agenda T)
			  (list agenda-nts))
			(agenda~for-loui (agenda~then-agenda agenda))))
	(if agenda-nts
	    (append (if orderings
			(agenda=consider-orderings! agenda-nts orderings agenda T)
		      (list agenda-nts))
		    (agenda~for-loui (agenda~then-agenda agenda)))
	  (agenda~for-loui (agenda~then-agenda agenda)))))
    ))
	  
(defun agenda~first-tasks! (agenda &optional no-blockedp)
  (declare (edited  "22-JAN-1998")
	   (authors Lassaad)
	   (input   "An agenda.")
	   (effect  "Could modify AGENDA.")
	   (value   "A pair: the list of the existing unblocked tasks on AGENDA to"
		    "be considered first, and if this list is empty, the list of"
		    "unblocked task which have to be considered first."))
  (let ((agenda-ft (agenda~first-task agenda)))
    (if agenda-ft
	;;; AGENDA with first task:
	(if (agenda~task-blocked-p agenda-ft)
	    ;;; AGENDA-ft is blocked, consider next tasks:
	    (let* ((agenda-nts (agenda~next-tasks agenda))
		   (unblocked-nts (remove-if #'agenda~task-blocked-p agenda-nts))
		   (agenda<s (agenda~orderings agenda))
		   (current-nts (if agenda<s
				    (agenda=consider-orderings! unblocked-nts
								agenda<s agenda)
				  unblocked-nts)))
	      (if current-nts (values current-nts)
		;;; No unblocked next task:
		(let ((then-agenda (agenda~then-agenda agenda)))
		  (if (agenda~empty-p then-agenda)
		      ;;; AGENDA-ft has to be unblocked:
		      (unless no-blockedp
			(progn (setf (agenda~task-blocked-p agenda-ft) nil)
			       (values nil (list agenda-ft))))
		    ;;; Try to find an unblocked task on then-agenda:
		    (multiple-value-bind (unblocked)
			(agenda~first-tasks! then-agenda t)
		      (if unblocked (values unblocked)
			;;; No unblocked task on then-agenda, AGENDA-ft has to be unblocked:
			(unless no-blockedp
			  (progn (setf (agenda~task-blocked-p agenda-ft) nil)
				 (values nil (list agenda-ft))))))))))
	  ;;; AGENDA-ft is unblocked:
	  (values (list agenda-ft)))
      ;;; AGENDA without first task:
      (let ((agenda-nts (agenda~next-tasks agenda)))
	(if agenda-nts
	    ;;; AGENDA with next tasks:
	    (let* ((agenda-nts (agenda~next-tasks agenda))
		   (unblocked-nts (remove-if #'agenda~task-blocked-p agenda-nts))
		   (agenda<s (agenda~orderings agenda))
		   (current-nts (if agenda<s
				    (agenda=consider-orderings! unblocked-nts
								agenda<s agenda)
				  unblocked-nts)))
	      (if current-nts (values current-nts)
		;;; No unblocked next task:
		(let ((then-agenda (agenda~then-agenda agenda)))
		  (if (agenda~empty-p then-agenda)
		      ;;; AGENDA-ft has to be unblocked:
		      (unless no-blockedp
			(let ((blocked-nts (if agenda<s
					       (agenda=consider-orderings! agenda-nts
									   agenda<s agenda)
					     agenda-nts)))
			  (dolist (task blocked-nts (values nil blocked-nts))
			    (setf (agenda~task-blocked-p task) nil))))
		    ;;; Try to find an unblocked task on then-agenda:
		    (multiple-value-bind (unblocked)
			(agenda~first-tasks! then-agenda t)
		      (if unblocked (values unblocked)
			;;; No unblocked task on then-agenda, AGENDA-ft has to be unblocked:
			(let ((blocked-nts (if agenda<s
					       (agenda=consider-orderings! agenda-nts
									   agenda<s agenda)
					     agenda-nts)))
			  (dolist (task blocked-nts (values nil blocked-nts))
			    (setf (agenda~task-blocked-p task) nil)))))))))
	  (let ((then-agenda (agenda~then-agenda agenda)))
	    (unless (agenda~empty-p then-agenda)
	      (agenda~first-tasks! then-agenda))))))
    ))


(defun agenda~all-tasks (agenda)
  (declare (edited  "18-SEP-1998")
	   (authors Jzimmer)
	   (input   "An agenda")
	   (effect  )
	   (value   "All tasks in AGENDA or its next-agendas."))
  (unless (agenda~empty-p agenda)
    (let ((first-task (agenda~first-task agenda))
	  )
      (if first-task
	  (cons first-task
		(append (agenda~next-tasks agenda)
			(agenda~all-tasks
			 (agenda~then-agenda agenda))))
	(append (agenda~next-tasks agenda)
		(agenda~all-tasks
		 (agenda~then-agenda agenda)))))))


(defun agenda=consider-orderings! (tasks orderings agenda &optional all-tasks)
  (declare (edited  "22-JAN-1998")
	   (authors Lassaad)
	   (input   "A task list with more than one element , a non-empty list of agenda"
		    "orderings, an agenda, and a flag ALL-TASKS which means that we have"
		    "to sort all the TASKS. We will get a list of task lists as result.")
	   (effect  "Eventually modify AGENDA.")
	   (value   "If ALL-TASKS is set to t, then a list of sublists of tasks relevant"
		    "to ORDERINGS. Otherwise, a sublist of TASKS according to the ORDERINGS."))
  (if all-tasks
      (let* ((sorted-tasks (agenda=sort-tasks tasks orderings tasks orderings))
	     (first-tasks (first sorted-tasks)))
	(unless (rest first-tasks)
	  ;;; ORDERINGS lead to a sole task A-FIRST-TASK, AGENDA must be changed so that:
	  ;; first(AGENDA) < A-FIRST-TASK < rest(TASKS) < then(AGENDA)
	  (agenda=insert-afirst-task! agenda (first first-tasks)))
	sorted-tasks)
    (let ((first-tasks (agenda=sort-tasks tasks orderings)))
      (unless (rest first-tasks)
	;;; ORDERINGS lead to a sole task A-FIRST-TASK, AGENDA must be changed so that:
	;; first(AGENDA) < A-FIRST-TASK < rest(TASKS) < then(AGENDA)
	(agenda=insert-afirst-task! agenda (first first-tasks)))
      first-tasks)))

(defun agenda=sort-tasks (tasks orderings &optional all-tasks all-orderings)
  (declare (edited  "27-MAR-1998")
	   (authors Lassaad)
	   (input   "A task list with at least two elements, a list of agenda"
		    "orderings, and optionally a list of all tasks, and a list"
		    "of all orderings.")
	   (effect  "None.")
	   (value   "When ALL-TASKS is non-empty, then a list of sublists of"
		    "ALL-TASKS relevant to ALL-ORDERINGS; otherwise the sublist"
		    "of TASKS containing the tasks which dont occur as next tasks"
		    "in ORDERINGS."))
  (if orderings
      (let* ((next-tasks (agenda~ordering-next-tasks (first orderings)))
	     (rest-tasks (set-difference tasks next-tasks)))
	(if rest-tasks
	    (if (rest rest-tasks)
		(agenda=sort-tasks rest-tasks (rest orderings) all-tasks all-orderings)
	      ;;; Only one task remains
	      (if all-tasks
		  (let ((rest-all-tasks (set-difference all-tasks rest-tasks)))
		    (if rest-all-tasks
			(if (rest rest-all-tasks)
			    (cons rest-tasks
				  (agenda=sort-tasks rest-all-tasks all-orderings
						     rest-all-tasks all-orderings))
			  (list rest-tasks rest-all-tasks))
		      (list rest-tasks)))
		rest-tasks))
	  (error ";;;agenda=sort-tasks: Agenda with inconsistent orderings!")))
    (if all-tasks
	(let ((rest-all-tasks (set-difference all-tasks tasks)))
	  (if rest-all-tasks
	      (if (rest rest-all-tasks)
		  (cons tasks
			(agenda=sort-tasks rest-all-tasks all-orderings
					   rest-all-tasks all-orderings))
		(list tasks rest-all-tasks))
	    (list tasks)))
      tasks)))
    
(defun agenda=insert-afirst-task! (agenda agenda-task)
  (declare (edited  "22-JAN-1998")
	   (authors Lassaad)
	   (input   "An agenda, and a task from its next task list.")
	   (effect  "Changing the order of AGENDA-TASK on AGENDA so that it"
		    "will be considered before all other tasks within the"
		    "current next task list of AGENDA. The next task list,"
		    "the ordering list, and eventually the then-agenda of AGENDA"
		    "must be adapted.")
	   (value   "Unspecified."))
  (let ((first-task (agenda~first-task agenda)))
    (if first-task
	(setf (agenda~next-tasks agenda) nil
	      (agenda~orderings agenda) nil
	      (agenda~then-agenda agenda)
	      (agenda~create agenda-task
			     (remove agenda-task (agenda~next-tasks agenda))
			     (agenda=replace-task-in-orderings agenda-task
							       NIL
							       (agenda~orderings agenda))
			     (agenda~then-agenda agenda)))
      (setf (agenda~first-task agenda)
	    agenda-task
	    (agenda~next-tasks agenda)
	    (remove agenda-task
		    (agenda~next-tasks agenda))
	    (agenda~orderings agenda)
	    (agenda=replace-task-in-orderings agenda-task
					      NIL
					      (agenda~orderings agenda))))
    ))

(defun agenda=replay-substs (stask stask-formula substs)
  (declare (edited  "19-APR-1998")
	   (authors Lassaad)
	   (input   "A goal-schema task, its associated formula and a list"
		    "of substitutions for meta-variables.")
	   (effect  "Reset the current formula of STASK to the formula resulted"
		    "from applying the substitutons in SUBSTS to STASK-FORMULA.")
	   (value   "Unspecified."))
  (if substs
      (let ((formula stask-formula))
	(dolist (subst substs)
	  (setq formula (subst~apply subst formula)))
	(setq formula (beta~normalize formula))
	(setf (agenda~goal-schema-formula stask) formula) 
	(setf (agenda~goal-schematic-p stask) (agenda~schematic-formula-p formula)))
    (and (setf (agenda~goal-schematic-p stask) t)
	 (setf (agenda~goal-schema-formula stask) nil))
    ))

#|
(defun agenda~update (agenda pds-nodes)
  (declare (edited  "23-APR-1999")
	   (authors Lassaad)
	   (input   "An agenda, and a list of pds nodes.")
	   (effect  "None.")
	   (value   "The result of updating AGENDA by deleting the tasks which"
		    "have no associated nodes in PDS and by setting the schematic"
		    "flag of goal schemas to T."))
  ;(format T "~%Open nodes: ~A~%All nodes: ~A" (pds~open-nodes pds*current-proof-plan) pds-nodes)
  (let ((agenda-ft (agenda~first-task agenda))
	(agenda-nts (agenda~next-tasks agenda))
	(then-agenda (agenda~then-agenda agenda)))
    (if agenda-ft
	;;; AGENDA with first task:
	(let ((ft-node (agenda~task-node agenda-ft)))
	  (if (and (find ft-node pds-nodes)
		   (if (pdsn~open-node-p ft-node) (not (agenda~pseudo-goal-p agenda-ft))
		     (agenda~pseudo-goal-p agenda-ft)))
	      ;;; AGENDA-ft must remain:
	      (progn
		(when (agenda~goal-schema-p agenda-ft)
		  (setf (agenda~goal-schematic-p agenda-ft) T))
		;;; Consider the AGENDA-nts:
		(if agenda-nts
		    (let ((new-agenda agenda))
		      (dolist (ntask agenda-nts)
			(let ((ntask-node (agenda~task-node ntask)))
			  (if (and (find ntask-node pds-nodes)
				   (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
				     (agenda~pseudo-goal-p ntask)))
			      ;;; This next task must remain:
			      (when (agenda~goal-schema-p ntask)
				(setf (agenda~goal-schematic-p ntask) T))
			    ;;; This next task must be removed:
			    (setq new-agenda
				  (agenda~replace-task ntask nil nil nil new-agenda)))))
		      (if (agenda~empty-p then-agenda) new-agenda
			(agenda~create agenda-ft (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
				       (agenda~update then-agenda pds-nodes))))
		  (if (agenda~empty-p then-agenda) agenda
		    (agenda~create agenda-ft nil nil
				   (agenda~update then-agenda pds-nodes)))))
	    ;;; AGENDA-ft must be removed:
	    (if agenda-nts
		;;; Consider the AGENDA-nts:
		(let ((new-agenda agenda))
		  (setf (agenda~first-task new-agenda) NIL)
		  (dolist (ntask agenda-nts)
		    (let ((ntask-node (agenda~task-node ntask)))
		      (if (and (find ntask-node pds-nodes)
			       (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
				 (agenda~pseudo-goal-p ntask)))
			  ;;; This next task must remain:
			  (when (agenda~goal-schema-p ntask)
			    (setf (agenda~goal-schematic-p ntask) T))
			;;; This next task must be removed:
			(setq new-agenda
			      (agenda~replace-task ntask nil nil nil new-agenda)))))
		  (cond ((agenda~empty-p then-agenda) new-agenda)
			((eq new-agenda then-agenda)
		         ;;; All AGENDA-NTS are removed and since the AGENDA-FT is removed to we have to consider
			 ;; the THEN-AGENDA
			 (agenda~update then-agenda pds-nodes))
			(T
		         ;;; NEW-AGENDA has at least one next-task
			 (agenda~create nil (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
					(agenda~update then-agenda pds-nodes)))))
	      ;;; AGENDA-ft must be removed and AGENDA-nts is empty, take the updated then-AGENDA:
	      (if (agenda~empty-p then-agenda) then-agenda
		(agenda~update then-agenda pds-nodes)))))
      ;;; AGENDA without first task:
      (if agenda-nts
	  ;;; Consider the AGENDA-nts:
	  (let ((new-agenda agenda))
	    (dolist (ntask agenda-nts)
	      (let ((ntask-node (agenda~task-node ntask)))
		(if (and (find ntask-node pds-nodes)
			 (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
			   (agenda~pseudo-goal-p ntask)))
		    ;;; This next task must remain:
		    (when (agenda~goal-schema-p ntask)
		      (setf (agenda~goal-schematic-p ntask) T))
		  ;;; This next task must be removed:
		  (setq new-agenda
			(agenda~replace-task ntask nil nil nil new-agenda)))))
	    (cond ((agenda~empty-p then-agenda) new-agenda)
		  ((eq new-agenda then-agenda)
		   ;;; All AGENDA-NTS are removed and since there is no AGENDA-FT we have to consider
		   ;; the THEN-AGENDA
		   (agenda~update then-agenda pds-nodes))
		  (T
		   ;;; NEW-AGENDA has at least one next-task
		   (agenda~create nil (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
				  (agenda~update then-agenda pds-nodes)))))
	;;; AGENDA without first task and without next tasks, take the updated then-AGENDA:
	(if (agenda~empty-p then-agenda) then-agenda
	  (agenda~update then-agenda pds-nodes))))
    ))|#
;; NEW VERSION AMEIER
(defun agenda~update (agenda pds-nodes)
  (declare (edited  "23-APR-1999")
	   (authors Lassaad)
	   (input   "An agenda, and a list of pds nodes.")
	   (effect  "None.")
	   (value   "The result of updating AGENDA by deleting the tasks which"
		    "have no associated nodes in PDS and by setting the schematic"
		    "flag of goal schemas to T."))
  ;;;(format T "~%Open nodes: ~A~%All nodes: ~A" (pds~open-nodes pds*current-proof-plan) pds-nodes)
  (let ((agenda-ft (agenda~first-task agenda))
	(agenda-nts (agenda~next-tasks agenda))
	(then-agenda (agenda~then-agenda agenda))
	(all-own-reasons (remove-duplicates (apply #'append (mapcar #'(lambda (node)
									(pdsj~all-own-reasons (node~justification node)))
								    pds-nodes)))))
    (if agenda-ft
	;;; AGENDA with first task:
	(if (or (and (agenda~inst-task-p agenda-ft)
		     (find (agenda~inst-task-plan-step agenda-ft) all-own-reasons))
		(let ((ft-node (agenda~task-node agenda-ft)))
		  (and (find ft-node pds-nodes)
		       (if (pdsn~open-node-p ft-node) (not (agenda~pseudo-goal-p agenda-ft))
			 (agenda~pseudo-goal-p agenda-ft)))))
	    ;;; AGENDA-ft must remain:
	    (progn
	      (when (agenda~goal-schema-p agenda-ft)
		  (setf (agenda~goal-schematic-p agenda-ft) T))
		;;; Consider the AGENDA-nts:
		(if agenda-nts
		    (let ((new-agenda agenda))
		      (dolist (ntask agenda-nts)
			(if (or (and (agenda~inst-task-p ntask)
				     (find (agenda~inst-task-plan-step ntask) all-own-reasons))
				(let ((ntask-node (agenda~task-node ntask)))
				  (and (find ntask-node pds-nodes)
				       (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
					 (agenda~pseudo-goal-p ntask)))))
			    ;;; This next task must remain:
			    (when (agenda~goal-schema-p ntask)
			      (setf (agenda~goal-schematic-p ntask) T))
			  ;;; This next task must be removed:
			  (setq new-agenda
				(agenda~replace-task ntask nil nil nil new-agenda))))
		      (if (agenda~empty-p then-agenda) new-agenda
			(agenda~create agenda-ft (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
				       (agenda~update then-agenda pds-nodes))))
		  (if (agenda~empty-p then-agenda) agenda
		    (agenda~create agenda-ft nil nil
				   (agenda~update then-agenda pds-nodes)))))
	  ;;; AGENDA-ft must be removed:
	  (if agenda-nts
	      ;;; Consider the AGENDA-nts:
	      (let ((new-agenda agenda))
		(setf (agenda~first-task new-agenda) NIL)
		(dolist (ntask agenda-nts)
		  (if (or (and (agenda~inst-task-p ntask)
			       (find (agenda~inst-task-plan-step ntask) all-own-reasons))
			  (let ((ntask-node (agenda~task-node ntask)))
			    (and (find ntask-node pds-nodes)
				 (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
				   (agenda~pseudo-goal-p ntask)))))
		      ;;; This next task must remain:
		      (when (agenda~goal-schema-p ntask)
			(setf (agenda~goal-schematic-p ntask) T))
		    ;;; This next task must be removed:
		    (setq new-agenda
			  (agenda~replace-task ntask nil nil nil new-agenda))))
		(cond ((agenda~empty-p then-agenda) new-agenda)
		      ((eq new-agenda then-agenda)
		       ;;; All AGENDA-NTS are removed and since the AGENDA-FT is removed to we have to consider
		       ;; the THEN-AGENDA
		       (agenda~update then-agenda pds-nodes))
		      (T
		       ;;; NEW-AGENDA has at least one next-task
		       (agenda~create nil (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
				      (agenda~update then-agenda pds-nodes)))))
	      ;;; AGENDA-ft must be removed and AGENDA-nts is empty, take the updated then-AGENDA:
	    (if (agenda~empty-p then-agenda) then-agenda
	      (agenda~update then-agenda pds-nodes))))
      ;; AGENDA without first task:
      (if agenda-nts
	  ;;; Consider the AGENDA-nts:
	  (let ((new-agenda agenda))
	    (dolist (ntask agenda-nts)
	      (if (or (and (agenda~inst-task-p ntask)
			   (find (agenda~inst-task-plan-step ntask) all-own-reasons))
		      (let ((ntask-node (agenda~task-node ntask)))
			(and (find ntask-node pds-nodes)
			     (if (pdsn~open-node-p ntask-node) (not (agenda~pseudo-goal-p ntask))
			       (agenda~pseudo-goal-p ntask)))))
		  ;;; This next task must remain:
		  (when (agenda~goal-schema-p ntask)
		    (setf (agenda~goal-schematic-p ntask) T))
		;;; This next task must be removed:
		(setq new-agenda
		      (agenda~replace-task ntask nil nil nil new-agenda))))
	    (cond ((agenda~empty-p then-agenda) new-agenda)
		  ((eq new-agenda then-agenda)
		   ;;; All AGENDA-NTS are removed and since there is no AGENDA-FT we have to consider
		   ;; the THEN-AGENDA
		   (agenda~update then-agenda pds-nodes))
		  (T
		   ;; NEW-AGENDA has at least one next-task
		   (agenda~create nil (agenda~next-tasks new-agenda) (agenda~orderings new-agenda)
				  (agenda~update then-agenda pds-nodes)))))
	;;; AGENDA without first task and without next tasks, take the updated then-AGENDA:
	(if (agenda~empty-p then-agenda) then-agenda
	  (agenda~update then-agenda pds-nodes))))
    ))


(defun agenda~update-goal-schemas! (agenda subst &optional but-task)
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "An agenda, a substitution binding some meta-variables,"
		    "and optionally one of the agenda tasks.")
	   (effect  "Applies SUBST to the formulas of goal-schemas in TASK.")
	   (value   "The list of schematic tasks, but BUT-TASK, whose formula"
		    "does not have anymore free variables."))
  (let ((goal-schemas (agenda~get-tasks agenda #'agenda~goal-schema-p))
	(result))
    (dolist (goal-schema goal-schemas)
      (when (agenda~goal-schematic-p goal-schema)
	(let ((formula-schema (beta~normalize (subst~apply subst (agenda~goal-schema-formula goal-schema)))))
	  (setf (agenda~goal-schema-formula goal-schema) formula-schema)
	  (unless (agenda~schematic-formula-p formula-schema)
	    (setf (agenda~goal-schematic-p goal-schema) nil)
	    (setq result (cons goal-schema result))))))
    (remove but-task result)
    ))

(defun agenda~get-tasks (agenda &optional (predicate t) sortedp)
  (declare (edited  "27-MAR-1998")
	   (authors Lassaad)
	   (input   "an agenda, a predicate, and a flag SORTEDP.")
	   (effect  "None.")
	   (value   "The list of the AGENDA tasks which fullfill the"
		    "PREDICATE. This list is sorted if SORTEDP is not NIL."))
  (unless (agenda~empty-p agenda)
    (let ((first-task (agenda~first-task agenda))
	  (next-tasks (remove-if-not predicate
				     (agenda~next-tasks agenda)))
	  (then-agenda (agenda~then-agenda agenda))
	  ;;;LC: It is necessary to store THEN-AGENDA at this point, because
	  ;; below the function agenda=consider-orderings! possibly change this slot.
	  )
      (when (and sortedp (rest next-tasks))
	(setq next-tasks (apply #'append
				(agenda=consider-orderings! next-tasks
							    (agenda~orderings agenda)
							    agenda T))))
      (append (if (and first-task (funcall predicate first-task))
		  (cons first-task next-tasks)
		next-tasks)
	      (agenda~get-tasks then-agenda predicate sortedp)))
    ))

(defun agenda~get-first-task (agenda &optional predicate)
  (declare (edited  "27-MAR-1998")
	   (authors Lassaad)
	   (input   "an agenda, and optionally a predicate.")
	   (effect  "None.")
	   (value   "The first task on the AGENDA which fullfills"
		    "PREDICATE, when this is given. Otherwise the"
		    "first possible task on AGENDA."))
  (unless (agenda~empty-p agenda)
    (let ((first-task (agenda~first-task agenda)))
      (if first-task
	  (if predicate
	      (if (funcall predicate first-task)
		  first-task
		(let* ((next-tasks (agenda~next-tasks agenda))
		       (the-task (find-if predicate next-tasks)))
		  (if the-task the-task
		    (agenda~get-first-task (agenda~then-agenda agenda) predicate))))
	    first-task)
	(let ((next-tasks (agenda~next-tasks agenda)))
	  (if predicate
	      (let ((the-task (find-if predicate next-tasks)))
		(if the-task the-task
		  (agenda~get-first-task (agenda~then-agenda agenda) predicate)))
	    (if next-tasks
		(if (rest next-tasks)
		    (let ((the-first-tasks (agenda=consider-orderings! next-tasks
								       (agenda~orderings agenda)
								       agenda)))
		      (if the-first-tasks
			  (first the-first-tasks)
			(error ";;; AGENDA~~GET-FIRST-TASK: Inconsistent agenda orderings: ~A!~%"
				     (agenda~orderings agenda))))
		  (first next-tasks))
	      (error ";;; AGENDA~~GET-FIRST-TASK: Inconsistent agenda ~A!~%" agenda))))))
    ))

;; Remark by AMEIER: Added a keyword where (behind/before) that determines where new tasks should be inserted into the
;; next-tasks list: behind -> behind the other next-tasks (this was the standard until now, hence the default of the keyword is behind)
;;                  before -> before the other next-tasks
(defun agenda~insert-tasks (old-task first-task new-tasks orderings agenda &key (where 'behind))
  (declare (edited  "23-JAN-1998")
	   (authors Lassaad)
	   (input   "An old task, a task to be considered first (or NIL),"
		    "a list of new tasks, a list of orderings, and an agenda"
		    "which contains OLD-TASK. Note FIRST-TASK and OLD-TASK"
		    "can be identical. The OLD-TASK can be different from"
		    "AGENDA first task and not in the next tasks of AGENDA,"
		    "it have not belong to its then-agenda too. In such a"
		    "case OLD-TASK was deleted.")
	   (effect  "None.")
	   (value   "The passed agenda by inserting the FIRST-TASK, the"
		    "NEW-TASKS, and the ORDERINGS."))
  (if (or first-task new-tasks orderings)
      (let ((new-agenda (agenda=insert-tasks old-task first-task new-tasks orderings agenda :where where)))
	(if new-agenda 
            ;;; FIRST-TASK and NEW-TASKS together with ORDERINGS are inserted in AGENDA at the
	    ;; same level as OLD-TASK.
	    new-agenda
          ;;; OLD-TASK does not belong to AGENDA, then insert FIRST-TASK and NEW-TASKS
	  ;; at the top level of AGENDA:
	  (let ((agenda-ft (agenda~first-task agenda)))
	    (if agenda-ft
	        ;;; AGENDA has a first task, then use AGENDA as then-agenda
		(agenda~create first-task new-tasks orderings agenda)
	      ;;; Otherwise: FIRST-TASK < NEW-TASKS U AGENDA-NTS < then-AGENDA
	      (progn (setf (agenda~first-task agenda) first-task
			   (agenda~next-tasks agenda) (append new-tasks (agenda~next-tasks agenda))
			   (agenda~orderings agenda) (append orderings (agenda~orderings agenda)))
		     agenda)))))
    agenda))

(defun agenda=insert-tasks (old-task first-task new-tasks orderings agenda &key (where 'behind))
  (declare (edited  "23-JAN-1998")
	   (authors Lassaad)
	   (input   "An old task, a task to be considered first (or NIL),"
		    "a list of new tasks, a list of orderings, and an agenda"
		    "which contains OLD-TASK. Note FIRST-TASK and OLD-TASK"
		    "can be identical. The OLD-TASK can be different from"
		    "AGENDA first task and not in the next tasks of AGENDA,"
		    "it have not belong to its then-agenda too. In such a"
		    "case OLD-TASK was deleted.")
	   (effect  "None.")
	   (value   "The adapted agenda by inserting the FIRST-TASK, the"
		    "NEW-TASKS, and the ORDERINGS, when OLD-TASK belongs to"
		    "AGENDA and NIL otherwise."))
  (let ((agenda-ft (agenda~first-task agenda)))
    (if agenda-ft
	;;; AGENDA with a first task:
	(if (eq old-task agenda-ft)
	    (if first-task
		;;; FIRST-TASK < NEW-TASKS U {OLD-TASK} < AGENDA-NTS < then-AGENDA
		(if (agenda~next-tasks agenda)
		    (progn (setf (agenda~first-task agenda) NIL)
			   (agenda~create first-task (cons agenda-ft new-tasks) orderings agenda))
		  (progn (setf (agenda~first-task agenda) first-task
			       (agenda~next-tasks agenda) (cons agenda-ft new-tasks)
			       (agenda~orderings agenda) orderings)
			 agenda))
	      ;;; {OLD-TASK} U NEW-TASKS < AGENDA-NTS < then-AGENDA
	      (if (agenda~next-tasks agenda)
		  (progn (setf (agenda~first-task agenda) NIL)
			 (agenda~create NIL (cons agenda-ft new-tasks) orderings agenda))
		(progn (setf (agenda~first-task agenda) NIL
			     (agenda~next-tasks agenda) (cons agenda-ft new-tasks)
			     (agenda~orderings agenda) orderings)
		       agenda)))
	  (let ((agenda-nts (agenda~next-tasks agenda)))
	    (if (find old-task agenda-nts)
		(if first-task
		    ;;; AGENDA-ft < FIRST-TASK < NEW-TASKS U AGENDA-NTS < then-AGENDA
		    (progn (setf (agenda~first-task agenda) first-task)
			   (setf (agenda~next-tasks agenda) (if (string-equal where 'behind)
								(append agenda-nts new-tasks)
							      (append new-tasks agenda-nts)))
			   (setf (agenda~orderings agenda) (append (agenda~orderings agenda) orderings))
			   (agenda~create agenda-ft nil nil agenda))
		  ;;; AGENDA-ft < NEW-TASKS U AGENDA-nts < then-AGENDA
		  (progn (setf (agenda~next-tasks agenda) (if (string-equal where 'behind)
							      (append agenda-nts new-tasks)
							    (append new-tasks agenda-nts)))
			 (setf (agenda~orderings agenda) (append (agenda~orderings agenda) orderings))
			 agenda))
	      ;;; OLD-TASK is different from AGENDA-ft and does not belong to AGENDA-nts
	      (let ((then-agenda (agenda~then-agenda agenda)))
		(unless (agenda~empty-p then-agenda)
		  (let ((new-then-agenda
			 (agenda=insert-tasks old-task first-task new-tasks orderings then-agenda)))
		    (when new-then-agenda
		      (agenda~create agenda-ft agenda-nts (agenda~orderings agenda) new-then-agenda))))))))
      ;;; AGENDA without a first task:
      (let ((agenda-nts (agenda~next-tasks agenda)))
	(if agenda-nts
	    (if (find old-task agenda-nts)
		(if first-task
		    ;;; FIRST-TASK < NEW-TASKS U AGENDA-NTS < then-AGENDA
		    (progn (setf (agenda~first-task agenda) first-task)
			   (setf (agenda~next-tasks agenda) (if (string-equal where 'behind)
								(append agenda-nts new-tasks)
							      (append new-tasks agenda-nts)))
			   (setf (agenda~orderings agenda) (append (agenda~orderings agenda) orderings))
			   agenda)
		  ;;; AGENDA-ft < NEW-TASKS U AGENDA-nts < then-AGENDA
		  (progn (setf (agenda~next-tasks agenda) (if (string-equal where 'behind)
							      (append agenda-nts new-tasks)
							    (append new-tasks agenda-nts)))
			 (setf (agenda~orderings agenda) (append (agenda~orderings agenda) orderings))
			 agenda))
	      ;;; OLD-TASK does not belong to AGENDA-nts
	      (let ((then-agenda (agenda~then-agenda agenda)))
		(unless (agenda~empty-p then-agenda)
		  (let ((new-then-agenda
			 (agenda=insert-tasks old-task first-task new-tasks orderings then-agenda)))
		    (when new-then-agenda
		      (agenda~create nil agenda-nts (agenda~orderings agenda) new-then-agenda))))))
	  (let ((then-agenda (agenda~then-agenda agenda)))
	    (unless (agenda~empty-p then-agenda)
	      (agenda=insert-tasks old-task first-task new-tasks orderings then-agenda)))))
      )))

;; Remark by AMEIER: Added a keyword where (behind/before) that determines where new tasks should be inserted into the
;; next-tasks list: behind -> behind the other next-tasks 
;;                  before -> before the other next-tasks (this was the standard until now, hence the default of the keyword is before)
(defun agenda~replace-task (old-task new-first-task new-tasks orderings agenda &key (where 'before))
  (declare (edited  "20-JAN-1998")
	   (authors Lassaad)
	   (input   "An old task, a new task to be considered first (or NIL),"
		    "a list of new tasks, a list of orderings on elements of"
		    "NEW-TASKS, and an agenda which contains OLD-TASK.")
	   (effect  "None.")
	   (value   "The passed agenda by inserting NEW-FIRST-TASK when different"
		    "from NIL, replacing OLD-TASK by NEW-TASKS and inserting"
		    "the new ORDERINGS."))
  (let ((agenda-ft (agenda~first-task agenda)))
    (if agenda-ft
	;;; AGENDA has a first task, this can be OLD-TASK
	(if (eq old-task agenda-ft)
	    (let ((agenda-nts (agenda~next-tasks agenda)))
	      (if agenda-nts
		  (if new-tasks
		      ;;; NEW-FIRST-TASK < NEW-TASKS < AGENDA-NTS < then-AGENDA
		      (progn (setf (agenda~first-task agenda) NIL)
			     (agenda~create new-first-task
					    new-tasks
					    orderings
					    agenda))
		    ;;; NEW-FIRST-TASK < AGENDA-NTS < then-AGENDA
		    (progn (setf (agenda~first-task agenda) new-first-task)
			   agenda))
		(if (or new-first-task (rest new-tasks))
		    ;;; NEW-FIRST-TASK < NEW-TASKS < then-AGENDA
		    (progn (setf (agenda~first-task agenda) new-first-task
				 (agenda~next-tasks agenda) new-tasks
				 (agenda~orderings agenda) orderings)
			   agenda)
		  (if new-tasks
		      ;;; first(NEW-TASKS) < then-AGENDA
		      (progn (setf (agenda~first-task agenda) (first new-tasks))
			     agenda)
		    (agenda~then-agenda agenda)))))
	  ;;; AGENDA-ft and OLD-TASK are not the same: OLD-TASK can belongs to AGENDA-nts or it is
	  ;; in the AGENDA-then:
	  (let ((agenda-nts (agenda~next-tasks agenda)))
	    (if (find old-task agenda-nts)
		(if new-first-task
		    (let ((rest-tasks (if (string-equal where 'before)
					  (append new-tasks (remove old-task agenda-nts))
					(append (remove old-task agenda-nts) new-tasks))))
		      (if rest-tasks
			  ;;; AGENDA-fts < NEW-FIRST-TASK < REST-TASKS
			  (progn (setf (agenda~first-task agenda) new-first-task
				       (agenda~next-tasks agenda) rest-tasks
				       (agenda~orderings agenda)
				       (append (agenda=replace-task-in-orderings old-task
										 new-tasks
										 (agenda~orderings agenda))
					       orderings))
				 (agenda~create agenda-ft nil nil agenda))
			;;; AGENDA-fts < NEW-FIRST-TASK
			(progn (setf (agenda~next-tasks agenda) (list new-first-task)
				     (agenda~orderings agenda) nil)
			       agenda)))
		  ;;; No new first task:
		  (let ((rest-tasks (if (string-equal where 'before)
					(append new-tasks (remove old-task agenda-nts))
				      (append (remove old-task agenda-nts) new-tasks))))
		    (if rest-tasks
			;;; AGENDA-ft < REST-TASKS
			(progn (setf (agenda~next-tasks agenda) rest-tasks
				     (agenda~orderings agenda)
				     (append (agenda=replace-task-in-orderings old-task
									       new-tasks
									       (agenda~orderings agenda))
					     orderings))
			       agenda)
		      (progn (setf (agenda~next-tasks agenda) nil
				   (agenda~orderings agenda) nil)
			     agenda))))
	      (let* ((then-agenda (agenda~then-agenda agenda))
		     (new-agenda (unless (agenda~empty-p then-agenda)
				   (agenda~replace-task old-task new-first-task new-tasks 
							orderings then-agenda
							:where where))))
		(if new-agenda
		    (agenda~create agenda-ft agenda-nts (agenda~orderings agenda) new-agenda)
		  (error ";;;agenda~~replace-task: The task ~A does not occur in ~A"
			 old-task agenda))))))
      ;;; AGENDA has no first task, OLD-TASK must belong to AGENDA next tasks
      (let ((agenda-nts (agenda~next-tasks agenda)))
	(if (find old-task agenda-nts)
	    (let ((agenda-nts1 (if (string-equal where 'before)
				   (append new-tasks (remove old-task agenda-nts))
				 (append (remove old-task agenda-nts) new-tasks)))
		  (orderings1 (append (agenda=replace-task-in-orderings old-task
									new-tasks
									(agenda~orderings agenda))
				      orderings)))
	      (if (or agenda-nts1 new-first-task)
		  ;;; NEW-FIRST-TASK < AGENDA-NTS1 < then-AGENDA
		  (progn (setf (agenda~first-task agenda) new-first-task
			       (agenda~next-tasks agenda) agenda-nts1
			       (agenda~orderings agenda) orderings1)
			 agenda)
		(agenda~then-agenda agenda)))
	  (let* ((then-agenda (agenda~then-agenda agenda))
		 (new-agenda (unless (agenda~empty-p then-agenda)
			       (agenda~replace-task old-task new-first-task new-tasks 
						    orderings then-agenda
						    :where where))))
	    (if new-agenda
		(agenda~create nil agenda-nts (agenda~orderings agenda) new-agenda)
	      (error ";;;agenda~~replace-task: The task ~A does not occur in ~A"
		     old-task agenda)))))
      )))

(defun agenda=replace-task-in-orderings (old-task new-tasks orderings)
  (declare (edited  "22-JAN-1998")
	   (authors Lassaad)
	   (input   "A task, a task list, and a list of orderings.")
	   (effect  "Unspecified.")
	   (value   "The result of substituting OLD-TASK by NEW-TASKS in"
		    "ORDERINGS."))
  (when orderings
    (let* ((ordering (first orderings))
	   (first-tasks (agenda~ordering-first-tasks ordering)))
      (if (find old-task first-tasks)
	  (let ((new-first-tasks (append new-tasks
					 (remove old-task first-tasks))))
	    (if new-first-tasks
		(progn (setf (agenda~ordering-first-tasks ordering)
			     new-first-tasks)
		       (cons ordering
			     (agenda=replace-task-in-orderings old-task
							       new-tasks
							       (rest orderings))))
	      (agenda=replace-task-in-orderings old-task
						new-tasks
						(rest orderings))))
	(let ((next-tasks (agenda~ordering-next-tasks ordering)))
	  (if (find old-task next-tasks)
	      (let ((new-next-tasks (append new-tasks
					    (remove old-task next-tasks))))
		(if new-next-tasks
		    (progn (setf (agenda~ordering-next-tasks ordering)
				 new-next-tasks)
			   (cons ordering
				 (agenda=replace-task-in-orderings old-task
								   new-tasks
								   (rest orderings))))
		  (agenda=replace-task-in-orderings old-task
						    new-tasks
						    (rest orderings))))
	    (cons ordering
		  (agenda=replace-task-in-orderings old-task
						    new-tasks
						    (rest orderings)))))))
    ))
 
