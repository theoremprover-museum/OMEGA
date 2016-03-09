;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "KEIM")

(mod~defmod LAGENDA 
            :uses (data term keim meta node)
            :documentation "Data Structures and Functions for the pds agenda"
            :exports (lagenda+agenda
                      lagenda~tasks
                      lagenda~first-orderings
                      lagenda~last-orderings
                      lagenda~orderings
                      lagenda+task
                      lagenda~task-nodes
                      lagenda~task-control-link
                      lagenda+goal-schema
                      lagenda~goal-schematic-p
                      lagenda+goal
                      lagenda+pseudo-goal
                      lagenda~pseudo-goal-step
                      lagenda+ordering
                      lagenda~ordering-first
                      lagenda~ordering-then
                      lagenda~ordering-step
                      lagenda~goal-p
                      lagenda~goal-schema-p
                      lagenda~pseudo-goal-p
                      lagenda~ordering-p
                      lagenda~agenda-p
                      lagenda~ref-execution-task-p
                      lagenda~empty-p
                      lagenda~task-node
                      lagenda~create-pseudo-goal
                      lagenda~create-goal-schema
                      lagenda~create-task
                      lagenda~create-goal
                      lagenda~create-open-task
                      lagenda~create-ordering
                      lagenda~initial-agenda
                      lagenda~first-tasks
                      lagenda~tasks-to-consider
                      lagenda~tasks-of
		      lagenda~goal-schema-formula
                      lagenda~add-tasks!
                      lagenda~add-ordering!
                      lagenda~delete-task!
                      lagenda~delete-tasks!
                      lagenda~delete-ordering!
                      lagenda~for-loui
		      ))

(defvar lagenda*print NIL)

(eval-when (compile eval load)
  (defclass lagenda+agenda ()
  ((tasks :initarg :tasks
	  :initform nil
	  :accessor lagenda~tasks
	  :documentation "The agenda tasks.")
   (first-orderings :initarg :first-orderings
		    :initform nil
		    :accessor lagenda~first-orderings
		    :documentation "Orderings on the tasks that should be handled first.")
   (last-orderings :initarg :last-orderings
		   :initform nil
		   :accessor lagenda~last-orderings
		   :documentation "Orderings on the tasks that should be handled last.")
   (orderings :initarg :orderings
	      :initform nil
	      :accessor lagenda~orderings
	      :documentation "Orderings on other tasks."))
  (:documentation "Data structure for the PDS agenda."))

  
  (defclass lagenda+task ()
    ((nodes :initarg :nodes
	    :accessor lagenda~task-nodes
	    :documentation "The nodes referred to by the task.")
     (control-link :initarg :control-link
		   :initform nil
		   :accessor lagenda~task-control-link))
    (:documentation "Data structure for tasks."))

  (defclass lagenda+goal-schema (lagenda+task)
    ((schematic :initarg :schematic
	        :initform T
	        :accessor lagenda~goal-schematic-p
	        :documentation "A boolean stating whether the goal formula is schematic."))
    (:documentation "Data structure for goal-schema tasks."))

  (defclass lagenda+goal (lagenda+task)
    ()
    (:documentation "Data structure for goal tasks."))

  (defclass lagenda+pseudo-goal (lagenda+task)
    ((step           :initarg :step
		     :accessor lagenda~pseudo-goal-step
		     :initform :nil))
    (:documentation "Data structure for pseudo-goal tasks."))

  (defclass lagenda+ordering ()
    ((first :initarg :first
	    :accessor lagenda~ordering-first
	    :documentation "(Actual) nodes determining the tasks to prefer.")
     (then :initarg :then
	   :accessor lagenda~ordering-then
	   :documentation "(Actual) nodes determining the tasks to delay.")
     (step :initarg :step
	   :accessor lagenda~ordering-step
	   :documentation "The step that created this ordering."))
    (:documentation "Data structure for orderings."))
  )

(defmethod print-object ((agenda lagenda+agenda) stream)
  (if lagenda*print
      (format stream "~%tasks: ~{~A ~}~% orderings: ~{~A ~}"
	      (lagenda~tasks agenda)
	      (append (lagenda~first-orderings agenda)
		      (lagenda~orderings agenda)
		      (lagenda~last-orderings agenda)))
    (format stream "<Agenda>")))


(defmethod print-object ((pseudo-goal lagenda+pseudo-goal) stream)
  (format stream "Pseudo-Goal: ~A" (mapcar #'keim~name (lagenda~task-nodes pseudo-goal))))

(defmethod print-object ((goal lagenda+goal-schema) stream)
  (format stream "Goal-Schema: ~A" (keim~name (first (lagenda~task-nodes goal)))))

(defmethod print-object ((goal lagenda+goal) stream)
  (format stream "Goal: ~A" (keim~name (first (lagenda~task-nodes goal)))))

(defmethod print-object ((ordering lagenda+ordering) stream)
  (if (lagenda~ordering-first ordering)
      (if (lagenda~ordering-then ordering)
	  (format stream "(~{~A ~} < ~{~A ~})" 
		  (lagenda~ordering-first ordering) 
		  (lagenda~ordering-then ordering))
	(format stream "(~{~A ~} < *)" 
		(lagenda~ordering-first ordering)))
    (format stream "(* < ~{~A ~})"  
	    (lagenda~ordering-then ordering))))

(defun lagenda~goal-p (object)
  (typep object 'lagenda+goal))

(defun lagenda~pseudo-goal-p (object)
  (typep object 'lagenda+pseudo-goal))

(defun lagenda~goal-schema-p (object)
  (typep object 'lagenda+goal-schema))

(defun lagenda~ordering-p (object)
  (typep object 'lagenda+ordering))

(defun lagenda~agenda-p (object)
  (typep object 'lagenda+agenda))

(defun lagenda~ref-execution-task-p (task)
  (and (lagenda~pseudo-goal-p task) (lagenda~task-step task)))

(defun lagenda~empty-p (agenda)
  (null (lagenda~tasks agenda)))

(defun lagenda~task-node (task)
  (if (lagenda~ref-execution-task-p task)
      (error ";;;lagenda~~task-node: ~A can refer to several nodes." task)
    (first (lagenda~task-nodes task))))



;; Creation Functions
(defun lagenda~create-pseudo-goal (ref-nodes &optional step)
  (make-instance 'lagenda+pseudo-goal
		 :nodes ref-nodes
		 :step step))

(defun lagenda~create-goal-schema (open-node)
  (declare (edited  "21-APR-1999")
	   (authors Lassaad)
	   (input   "An open node whose formula contains meta-variables.")
	   (effect  "None.")
	   (value   "The goal schema of OPEN-NODE."))
  (make-instance 'lagenda+goal-schema
		 :nodes (list open-node)))

(defun lagenda~create-task (node)
  (cond ((pdsn~open-node-p node)
	 (lagenda~create-open-task node))
	(T
	 (lagenda~create-pseudo-goal (list node)))
	))

(defun lagenda~create-goal (open-node)
  (make-instance 'lagenda+goal
		  :nodes (list open-node)))

(defun lagenda~create-open-task (node)
  (if (pdsn~schematic-p node) 
      (lagenda~create-goal-schema node)
    (lagenda~create-goal node)))

(defun lagenda~create-ordering (first-nodes then-nodes &optional step)
  (make-instance 'lagenda+ordering
                 :first first-nodes
                 :then then-nodes
                 :step step))
  
(defun lagenda~initial-agenda (node)
  (make-instance 'lagenda+agenda
		 :tasks (list (lagenda~create-goal node))))



;; Functions determining tasks    
(defun lagenda~first-tasks (agenda)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An agenda.")
	   (effect  "Nothing.")
	   (value   "The list of the tasks on AGENDA that should be considered first."))
  (lagenda~tasks-to-consider (lagenda~tasks agenda)
			     (lagenda~first-orderings agenda)
			     (lagenda~last-orderings agenda)
			     (lagenda~orderings agenda)))

(defun lagenda~tasks-to-consider (tasks firsts lasts orderings)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "A task list, a list of first-orderings, a list of last-orderings"
		    "and an ordering list.")
	   (effect  "Nothing.")
	   (value   "The elements of TASKS that should be considered first according to"
		    "the given order constraints."))
  (if (or (null (rest tasks)) (and (null firsts) (null lasts) (null orderings)))
      tasks
    (cond (firsts
	   (let* ((ordering (first firsts))
		  (first-tasks (lagenda=tasks-referred tasks (lagenda~ordering-first ordering))))
	     (if first-tasks
		 first-tasks
	       (lagenda~tasks-to-consider tasks (rest firsts) lasts orderings))))
	  (lasts
	   (let* ((ordering (first lasts))
		  (last-tasks (lagenda=tasks-referred tasks (lagenda~ordering-then ordering)))
		  (before-tasks (remove-if #'(lambda (task) (find task last-tasks))
					   tasks)))
	     (if before-tasks
		 (lagenda~tasks-to-consider before-tasks firsts (rest lasts) orderings)
	       last-tasks)))
	  (T
	   (let* ((ordering (first orderings))
		  (first-task (lagenda=task-referred tasks (lagenda~ordering-first ordering))))
	     (if (null first-task)
		 (lagenda~tasks-to-consider tasks firsts lasts (rest orderings))
	       (let* ((after-tasks (remove first-task tasks))
		      (then-task (lagenda=task-referred after-tasks (lagenda~ordering-then ordering))))
		 (if (null then-task)
		     (lagenda~tasks-to-consider tasks firsts lasts (rest orderings))
		   (let ((other-then-tasks (lagenda=tasks-referred (remove then-task after-tasks)
								   (lagenda~ordering-then ordering))))
		     (lagenda~tasks-to-consider (remove-if #'(lambda (task) (find task other-then-tasks))
							   (remove then-task tasks))
						firsts lasts (rest orderings))))))))
	  )))
		       

(defun lagenda=task-referred (tasks ref-nodes)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "A task list and a list of (actual) nodes.")
	   (effect  "Nothing.")
	   (value   "An element of TASKS that is referred by an element of REF-NODES."))
  (when ref-nodes
    (let* ((ref-node (first ref-nodes))
	   (task (if (node~p ref-node)
		     (find-if #'(lambda (task) (and (not (lagenda~pseudo-goal-p task))
						    (eq (first (lagenda~task-nodes task)) ref-node)))
			      tasks)
		   (find-if #'(lambda (task) (and (lagenda~pseudo-goal-p task)
						  (find-if #'(lambda (node)
							       (and (eq node (pdsc~an-node ref-node))
								    (eq (node~justification node) (pdsc~an-just ref-node))))
							   (lagenda~task-nodes task))))
			    tasks))))
      (if task
	  task
	(lagenda=task-referred tasks (rest ref-nodes)))
      )))


(defun lagenda=tasks-referred (tasks ref-nodes)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "A task list and a list of (actual) nodes.")
	   (effect  "Nothing.")
	   (value   "The elements of TASKS that are referred by the elements of REF-NODES."))
  (when ref-nodes
    (let* ((ref-node (first ref-nodes))
	   (task (if (node~p ref-node)
		     (find-if #'(lambda (task) (and (not (lagenda~pseudo-goal-p task))
						    (eq (first (lagenda~task-nodes task)) ref-node)))
			      tasks)
		   (find-if #'(lambda (task) (and (lagenda~pseudo-goal-p task)
						  (find-if #'(lambda (node)
							       (and (eq node (pdsc~an-node ref-node))
								    (eq (node~justification node) (pdsc~an-just ref-node))))
							   (lagenda~task-nodes task))))
			    tasks))))
      (if task
	  (cons task (lagenda=tasks-referred (remove task tasks) (rest ref-nodes)))
	(lagenda=tasks-referred tasks (rest ref-nodes)))
      )))

(defun lagenda~tasks-of (agenda nodes)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA and a list of NODES.")
	   (effect  "None.")
	   (value   "The AGENDA tasks that refer to one of the NODES."))
  (remove-if-not #'(lambda (task) (find-if #'(lambda (task-node) (find task-node nodes))
					   (lagenda~task-nodes task)))
		 (lagenda~tasks agenda))
  )

(defmethod lagenda~goal-schema-formula ((goal-schema lagenda+goal-schema))
  (or ;;(agenda=goal-schema-formula goal-schema)
   (pdsn~current-formula (first (lagenda~task-nodes goal-schema)))
   (node~formula (first (lagenda~task-node goal-schema)))))



;; Update Functions 
(defun lagenda~add-tasks! (agenda task new-tasks)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, a TASK (or NIL) and a list of new tasks.")
	   (effect  "Extends the task list of AGENDA with the tasks in NEW-TASKS."
		    "When TASK is (not NIL and) referred by some ordering, then this"
		    "ordering is extended to refer, in the same way, the tasks in NEW-TASKS.")
	   (value   "Undefined."))
  (setf (lagenda~tasks agenda) (append new-tasks (lagenda~tasks agenda)))
  (when (and task
	     (or (lagenda~first-orderings agenda) (lagenda~last-orderings agenda) (lagenda~orderings agenda)))
    (lagenda=update-orderings! (lagenda~first-orderings agenda) (lagenda~last-orderings agenda)
			       (lagenda~orderings agenda) task new-tasks))
  )

(defun lagenda=update-orderings! (firsts lasts orderings task new-tasks &optional referring-nodes)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "A list of first-orderings, a list of last-orderings, a list of ORDERINGS, a TASK,"
		    "a list of new tasks and optionally a list (actual) nodes referring to the elements"
		    "of NEW-TASKS.")
	   (effect  "When TASK is referred by some ORDERING, then this ordering is extended"
		    "to refer, in the same way, the tasks in NEW-TASKS.")
	   (value   "Undefined."))
  (cond (firsts
	 (let ((ordering1 (first firsts)))
	   (if (lagenda=task-referred (list task) (lagenda~ordering-first ordering1))
	       (setf (lagenda~ordering-first ordering1)
		     (append (if referring-nodes referring-nodes
			       (lagenda=nodes-referring new-tasks))
			     (lagenda~ordering-first ordering1)))
	     (lagenda=update-orderings! (rest firsts) lasts orderings task new-tasks referring-nodes))))
	(lasts
	 (let ((ordering1 (first lasts)))
	   (if (lagenda=task-referred (list task) (lagenda~ordering-then ordering1))
	       (setf (lagenda~ordering-then ordering1)
		     (append (if referring-nodes referring-nodes
			       (lagenda=nodes-referring new-tasks))
			     (lagenda~ordering-then ordering1)))
	     (lagenda=update-orderings! firsts (rest lasts) orderings task new-tasks referring-nodes))))
	(orderings
	 (let ((ordering1 (first orderings)))
	   (cond ((lagenda=task-referred (list task) (lagenda~ordering-first ordering1))
		  (let ((new-nodes (if referring-nodes referring-nodes
				     (lagenda=nodes-referring new-tasks))))
		    (setf (lagenda~ordering-first ordering1) 
			  (append new-nodes (lagenda~ordering-first ordering1)))
		    (lagenda=update-orderings! firsts lasts (rest orderings) task new-tasks new-nodes)))
		 ((lagenda=task-referred (list task) (lagenda~ordering-then ordering1))
		  (let ((new-nodes (if referring-nodes referring-nodes
				     (lagenda=nodes-referring new-tasks))))
		    (setf (lagenda~ordering-then ordering1) 
			  (append new-nodes (lagenda~ordering-then ordering1)))
		    (lagenda=update-orderings! firsts lasts (rest orderings) task new-tasks new-nodes)))
		 (T
		  (lagenda=update-orderings! firsts lasts (rest orderings) task new-tasks referring-nodes))))
	 )))

(defun lagenda=nodes-referring (tasks)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "A list of TASKS.")
	   (effect  "None.")
	   (value   "A list of the (actual) nodes that should be used to refer to the TASKS."))
  (when tasks
    (append (if (not (lagenda~pseudo-goal-p (first tasks)))
		(lagenda~task-nodes (first tasks))
	      (mapcar #'pdsc~an-create (lagenda~task-nodes (first tasks))))
	    (lagenda=nodes-referring (rest tasks)))
    ))
		    

(defun lagenda~add-ordering! (agenda ordering)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, and a new ORDERING.")
	   (effect  "Extends orderings of AGENDA with ORDERING.")
	   (value   "Undefined."))
  (cond ((null (lagenda~ordering-then ordering))
	 (setf (lagenda~first-orderings agenda)
	       (cons ordering (lagenda~first-orderings agenda))))
	((null (lagenda~ordering-first ordering))
	 (setf (lagenda~last-orderings agenda)
	       (cons ordering (lagenda~last-orderings agenda))))
	(T
	 (setf (lagenda~orderings agenda)
	       (cons ordering (lagenda~orderings agenda))))
	))

(defun lagenda~delete-task! (agenda task)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, and a TASK.")
	   (effect  "Deletes TASK from AGENDA.")
	   (value   "Undefined."))
  (setf (lagenda~tasks agenda)
	(remove task (lagenda~tasks agenda))))

(defun lagenda~delete-tasks! (agenda tasks)
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, and a list of TASKS.")
	   (effect  "Deletes the element of TASKS from AGENDA.")
	   (value   "Undefined."))
  (setf (lagenda~tasks agenda)
	(remove-if #'(lambda (task) (find task tasks))
		   (lagenda~tasks agenda))))

(defmethod lagenda~delete-ordering! ((agenda lagenda+agenda) (ordering lagenda+ordering))
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, and an ORDERING.")
	   (effect  "Deletes ORDERING from AGENDA.")
	   (value   "Undefined."))
  (cond ((null (lagenda~ordering-then ordering))
	 (setf (lagenda~first-orderings agenda)
	       (remove ordering (lagenda~first-orderings agenda))))
	((null (lagenda~ordering-first ordering))
	 (setf (lagenda~last-orderings agenda)
	       (remove ordering (lagenda~last-orderings agenda))))
	(T
	 (setf (lagenda~orderings agenda)
	       (remove ordering (lagenda~orderings agenda))))
	))




;;  Function for LOUI
(defun lagenda~for-loui (agenda)
  (declare (edited  "04-NOV-1998")
	   (authors Lassaad)
	   (input   "An agenda.")
	   (effect  "None.")
	   (value   "A list consisting of a list of the first tasks on AGENDA and,"
                    "in case AGENDA contains other tasks, of a list of the rest"
                    "tasks."))
  (unless (lagenda~empty-p agenda)
    (let* ((first-tasks (lagenda~first-tasks agenda))
	   (rest-tasks (remove-if #'(lambda (task) (find task first-tasks))
				  (lagenda~tasks agenda))))
      (if rest-tasks
	  (list first-tasks rest-tasks)
	(list first-tasks)
	))))

    
;; Anpassung 
(defmethod pds~task-formula ((goal lagenda+goal) &optional pds)
  (declare (ignore pds))
  (node~formula (first (lagenda~task-nodes goal))))

(defmethod pds~task-formula ((sgoal lagenda+goal-schema) &optional (pds pds*current-proof-plan))
  (if (lagenda~goal-schematic-p sgoal)
      (pds~node-formula (first (lagenda~task-nodes sgoal)) pds)
    (pdsn~current-formula (first (lagenda~task-nodes sgoal)))))

(defmethod pds~task-formula ((pgoal lagenda+pseudo-goal) &optional (pds pds*current-proof-plan))
  (if (lagenda~pseudo-goal-step pgoal)
      (error ";;;pds~~task-formula: Not defined for the pseudo goal ~A (a refinement execution task)!"
	     pgoal)
    (pds~node-formula (first (lagenda~task-nodes pgoal)) pds)))
		      
  
(defmethod pdsh~metavars-of ((task lagenda+goal))
  (pdsh~metavars-of (first (lagenda~task-nodes task))))

(defmethod pdsh~metavars-of ((task lagenda+goal-schema))
  (pdsh~metavars-of (first (lagenda~task-nodes task))))
  
(defmethod pdsh~metavars-of ((task lagenda+pseudo-goal))
  (declare (ignore task))
  )












