;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "OMEGA")

(defun dummy-status-init ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "True"))
  t)

(defun dummy-status-copy (status)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status")
	   (effect  "None")
	   (value   "True"))
  t)

(defun matching-not-already-applied (status cp-mapp &rest choice-points)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status, a choice point mapping and choices")
	   (effect  "None")
	   (value   "The status, if an action with these choices was already applied within the current analogy strategy, otherwise a failure"))
  (if (roc~analogy-mapping-already-applied-p ana*roc-state-description cp-mapp)
      (ana~failure-create 'matching-already-applied choice-points)
    status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Abstract Analogy Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defpattern source-substep
		(choice-points source)
		(applicability-tests (source-check-substep (source))))

(defun source-check-substep (status cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "The status, if the source step is a substep, otherwise a failure"))
  (if (ana~substep-p source) status
    (ana~failure-create 'source-is-not-a-substep source)))

(defun source-check-seg-instmeta (status cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "The status, if the source step is an instmeta step, otherwise a failure"))
  (if (ana~seg-instmeta-p source) status
    (ana~failure-create 'source-is-not-an-instmeta-step source)))

(defun source-check-seg-pplanner (status cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "The status, if the source step is segment, that contains method steps, otherwise a failure"))
  (if (or (ana~seg-pplanner-p source) (ana~seg-analogy-p source)) status
    (ana~failure-create 'source-is-not-an-pplanner-step source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Abstract Demand Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defpattern demand
		(choice-points demand-strategy demand-tasks demand-parameters)
		(status-init-function dummy-status-init)
		(status-copy-function dummy-status-copy)
		(applicability-tests (matching-not-already-applied (demand-strategy demand-tasks demand-parameters)))
		(application-function demand-apply))

(defun demand-apply (status cp-mapp &rest choice-points)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A status, a choice point mapping and choices")
	   (effect  "None")
	   (value   "Creates a demand from the choices and returns it"))
  (let* ((strategy (mapp~get-component 'demand-strategy cp-mapp))
	 (tasks (mapp~get-component 'demand-tasks cp-mapp))
	 (parameters (mapp~get-component 'demand-parameters cp-mapp))
	 (ref-alg (keim~name (strat~strategy-ks-refinement-algorithm strategy))))
    ;; tasks can be nil, if the segment contains no steps
    (ana~message "Demand for ~A on tasks ~A" (keim~name strategy) tasks)
    (demand~create-strategy-task-demand strategy (if tasks (first tasks) (first (ana~goal-tasks)))
					(if (keim~equal ref-alg 'AnalogyNew) (cons tasks parameters) parameters))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Instmeta Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; derived from analogy and demand
(ana~defpattern instmeta
		(choice-points source demand-strategy demand-tasks metavar instantiation demand-parameters)
		(status-init-function dummy-status-init)
		(status-copy-function dummy-status-copy)
		(applicability-tests (source-check-seg-instmeta (source))
				     (matching-not-already-applied (demand-tasks demand-strategy demand-parameters source)))
		(application-function demand-apply))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{PPlanner Segment Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; derived from analogy and demand
(ana~defpattern pplanner-segment
		(choice-points source demand-strategy demand-tasks demand-parameters goal source-strategy start-condition substeps)
		(status-init-function dummy-status-init)
		(status-copy-function dummy-status-copy)
		(applicability-tests (source-check-seg-pplanner (source))
				     (pplanner-segment-check-goal-tasks (demand-tasks))
				     (pplanner-segment-check-start-condition (demand-tasks start-condition))
				     (matching-not-already-applied (demand-tasks demand-strategy demand-parameters source)))
		(application-function demand-apply))

(defun pplanner-segment-check-goal-tasks (dummy cp-mapp demand-tasks)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A dummy, a choice point mapping and a list of tasks")
	   (effect  "None")
	   (value   "True, if the given tasks, are tasks for nodes, not for meta variables, otherwise a failure"))
  (if (every 'agenda~goal-or-goal-schema-task-p demand-tasks) t
    (ana~failure-create 'demand-tasks-are-not-all-goal-task demand-tasks)))

(defun pplanner-segment-check-start-condition (dummy cp-mapp demand-tasks start-condition)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A dummy, a choice point mapping, a list of tasks and a condition")
	   (effect  "None")
	   (value   "True, if the condition is fullfiled on every task, otherwise a failure"))
  (if (every #'(lambda (task) (funcall start-condition task)) demand-tasks) t
    (ana~failure-create 'start-condition-not-fulfilled (list start-condition demand-tasks))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Method Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; derived from analogy
(ana~defpattern method
		(choice-points source method goal supports parameters)
		(status-init-function method-status-init)
		(status-copy-function method-status-copy)
		(applicability-tests (source-check-substep (source))
				     (method-match-goal (method goal))
				     (method-match-supports (method goal supports))
				     (method-match-parameters (method parameters))
				     (method-check-application-condition (method goal supports parameters))
				     (matching-not-already-applied (method goal supports parameters source)))
		(application-function method-apply))

(defun method-status-init ()
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "Nothing")
	   (effect  "None")
	   (value   "An empty method mapping"))
  (meth~mapping-create (subst~create nil nil)
		       (mapp~create nil nil)))

(defun method-status-copy (mmapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A method mapping")
	   (effect  "None")
	   (value   "A copy of the mapping"))
  (meth~mapping-copy mmapp))

(defun method-match-goal (mmapps cp-mapp method goal)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method and a node")
	   (effect  "None")
	   (value   "The new mappings, if the node matches the method goal, otherwise a failure"))
  (if (ana~failure-p goal) goal
    (if (ana~task-of-node goal nil)
	(if (meth~goal method)
	    (ana~when-not (meth~match-p (meth~goal method) goal mmapps :one2one)
			  (ana~failure-create 'goal-does-not-match (list method goal)))
	  mmapps)
      (ana~failure-create 'no-task-for-goal goal))))

(defun method-match-supports (mmapps cp-mapp method goal supports)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method, a node and a list of nodes")
	   (effect  "None")
	   (value   "The new mappings, if the list of nodes support the node and they match the premises of the method, otherwise a failure"))
  (if (ana~failure-p supports) supports
    (if (ana~task-of-node goal supports)
	(let ((premises-to-match (append (meth~existent-premises method) (meth~closed-premises method))))
	  (ana~when-not (meth~match-p premises-to-match supports mmapps :many2many)
			(ana~failure-create 'supports-do-not-match supports)))
      (ana~failure-create 'no-task-for-goal-with-supports (list method goal supports)))))

(defun method-match-parameters (mmapps cp-mapp method parameters)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method and a list of parameters")
	   (effect  "None")
	   (value   "The new mappings, if the parameters match the method parameters, otherwise a failure"))
  (if (ana~failure-p parameters) parameters
    (let ((meth-params (subseq (meth~parameters method) 0 (length parameters))))
      (ana~when-not (meth~match-p meth-params parameters mmapps :one2one)
		    (ana~failure-create 'parameters-do-not-match (list method parameters))))))

(defun method-check-application-condition (mmapps cp-mapp method goal supports parameters)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method, a node, a list of nodes and a list of parameters")
	   (effect  "None")
	   (value   "The new mappings, if the application condition of the method is fulfilled, otherwise a failure"))
  (let ((appl-cond (meth~application-condition method)))
    (if appl-cond
	(let ((result-mmapps (meth~check-condition appl-cond mmapps)))
	  (ana~when-not (remove-if-not #'(lambda (mmapp) (meth~mapp-constraint mmapp)) result-mmapps)
			(ana~failure-create 'application-condition-not-fulfilled mmapps)))
      mmapps)))

(defun ana~focus-goal (goal supports)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A goal or nil and a list of nodes")
	   (effect  "None")
	   (value   "The goal or the node, that is supported by the list of nodes"))
  (agenda~task-node (ana~task-of-node goal supports)))

(defun method-not-already-applied (mmapps cp-mapp method goal supports parameters source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method, a node, a list of nodes, a list of parameters and a source step")
	   (effect  "None")
	   (value   "The mappings, that lead to a new method step. If none exist, a failure is returned"))
  (let ((already-applied-mmatchings (keim::pdsc~already-applied-mmatchings (pdsj~control (node~justification (ana~focus-goal goal supports))))))
    (ana~when-not
     (remove-if #'(lambda (mmapp)
		    (let ((mmatching (ana~mmatching-create method goal parameters mmapp)))
		      (find mmatching already-applied-mmatchings :test #'pplan=matching-is-subsumed-p)))
		mmapps)
     (ana~failure-create 'method-already-applied (list method goal supports parameters)))))

(defun ana~task-of-node (goal supports)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A node and a list of nodes")
	   (effect  "None")
	   (value   "The task for the goal, for which the nodes are supports"))
  (find-if #'(lambda (task) (subsetp supports (pds~node-supports (agenda~task-node task))))
	   (reverse (remove-if-not #'(lambda (task) (if goal (keim~equal (agenda~task-node task) goal) t)) (ana~goal-tasks)))))

(defun ana~mmatching-create (method goal parameters mmapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A method, a node, a list of parameters and a method mapping")
	   (effect  "None")
	   (value   "Creates a method matching and returns it"))
  (let ((exist-prems (mapcar #'(lambda (meth-prem) (meth~pds-object meth-prem mmapp nil)) (meth~existent-premises method)))
	(closed-prems (mapcar #'(lambda (meth-prem) (meth~pds-object meth-prem mmapp nil)) (meth~closed-premises method))))
    (pplan=mmatching-create method parameters mmapp (pds~constraint-pool omega*current-proof-plan) ana*roc-state-description
			    (when (meth~goal method) goal) exist-prems closed-prems)))
  
(defun method-apply (mmapps cp-mapp source method goal supports parameters)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A list of method mappings, a choice point mapping, a method, a node, a list of nodes and a list of parameters")
	   (effect  "Creates a method step and introduces it into the pds")
	   (value   "The method step"))
  (let ((task (ana~task-of-node goal supports))
	(mmatching (ana~mmatching-create method goal parameters (first mmapps))))
    (ana~message "Application of ~A on ~A" method task)
    ;; set global variable to nil, otherwise the step is added to this state description
    ;;(let ((roc pplan*roc-state-description))
    ;;(setf pplan*roc-state-description nil)
    (multiple-value-bind (new-agenda new-opens new-supps)
	(pplan=apply-plan-step! task mmatching omega*current-proof-plan (pds~agenda omega*current-proof-plan))
      (setf (pds~agenda omega*current-proof-plan) new-agenda)
      ;;(setf pplan*roc-state-description roc)
      (pds~last-plan-step omega*current-proof-plan))))
