;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "OMEGA")

(defun ana~obj-to-task (obj)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "A thing")
	   (effect  "None")
	   (value   "The task, if the thing is a node or a meta variable, otherwise thing"))
  (cond ((pdsn~p obj)
	 (find-if #'(lambda (task) (keim~equal (agenda~task-node task) obj)) (ana~goal-tasks)))
	((meta~p obj)
	 (find-if #'(lambda (task) (keim~equal (agenda~inst-task-metavar task) obj)) (ana~inst-tasks)))
	(t obj)))

(defun crules-none (roc cp-mapp &rest dependencies)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a list of choices")
	   (effect  "None")
	   (value   "Nil"))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol source-next-from-steps
		      source
		      (dependencies steps)
		      (conflict-set-computation-function conflict-source-next-from-steps)
		      (crules-computation-function crules-none))

(defun conflict-source-next-from-steps (roc cp-mapp steps)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a list of steps")
	   (effect  "None")
	   (value   "Alternatives: The chronologically next source step in steps or nil"))
  (let* ((source (roc~analogy-last-source roc))
	 (next-source (if source (ana~find-next-in steps source) (ana~find-first-in steps))))
    (when next-source (list next-source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Choice Controls For Method Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol method-from-source
		      method
		      (dependencies source)
		      (conflict-set-computation-function conflict-method-from-source)
		      (crules-computation-function crules-none))

(defun conflict-method-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "Alternatives: The method, if the step is a method step, otherwise nil"))
  (when (ana~substep-p source)
    (list (ana~substep-method source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol goal-from-source
		      goal
		      (dependencies source)
		      (conflict-set-computation-function conflict-goal-from-source)
		      (crules-computation-function crules-none))

(defun conflict-goal-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "Alternatives: The corresponding goal nodes of the source step in the target plan"))
  (when (ana~substep-p source)
    (if (meth~goal (ana~substep-method source))
	(ana~cor-struct-of (roc~analogy-table roc) (ana~substep-conclusion source) 'ana=cor-source-to-targets)
      (list nil))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol source-from-goal
		      source
		      (dependencies steps goal source-plan)
		      (conflict-set-computation-function conflict-source-from-goal)
		      (crules-computation-function crules-none))

(defun conflict-source-from-goal (roc cp-mapp steps goal source-plan)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a list of source steps, a goal and a pds")
	   (effect  "None")
	   (value   "Alternatives: The source steps from the list, that correspond to the goal"))
  (let ((source-goals (ana~cor-of (roc~analogy-table roc) goal 'ana=cor-target-to-sources)))
    (when (not (ana~failure-list-p source-goals))
      (append
       ;; strategy applications
       (remove-if #'(lambda (seg) (null (intersection source-goals (ana~seg-goals seg source-plan)))) (remove-if-not 'ana~seg-pplanner-p steps))
       ;; backward method applications
       ;;(mapcar #'(lambda (node) (pdsc~an-create node)) source-goals)
       (remove-if-not #'(lambda (step) (and (ana~substep-p step) (find (ana~substep-node step) source-goals))) steps)
       ;; forward method applications
       (remove-if-not #'(lambda (substep)
			  (and (ana~substep-forward-method-p substep)
			       (find-if #'(lambda (node-list) (subsetp (ana~substep-exist-premises substep) node-list))
					(ana~cor-struct-of (roc~analogy-table roc) (pds~node-supports goal) 'ana=cor-target-to-sources))))
		      (remove-if-not 'ana~substep-p steps))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol goal-from-agenda
		      goal
		      (conflict-set-computation-function conflict-goal-from-agenda)
		      (crules-computation-function crules-none))

(defun conflict-goal-from-agenda (roc cp-mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping")
	   (effect  "None")
	   (value   "Alternatives: All goal tasks"))
  (mapcar 'agenda~task-node (ana~goal-tasks)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol supports-from-source
		      supports
		      (dependencies source)
		      (conflict-set-computation-function conflict-supports-from-source)
		      (crules-computation-function crules-none))

(defun conflict-supports-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "Alternatives: The corresponding supports of the source step in the target plan"))
  (when (ana~substep-p source)
    (ana~cor-struct-of (roc~analogy-table roc) (ana~substep-exist-premises source) 'ana=cor-source-to-targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol parameters-from-source
		      parameters
		      (dependencies source goal supports)
		      (conflict-set-computation-function conflict-parameters-from-source)
		      (crules-computation-function crules-none))

(defun conflict-parameters-from-source (roc cp-mapp source goal supports)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a source step, goal and supports")
	   (effect  "None")
	   (value   "Alternatives: The corresponding parameters and the computed parameters"))
  (when (ana~substep-p source) 
    (let ((cors (ana~cor-struct-of (roc~analogy-table roc) (ana~substep-parameters source) 'ana=cor-source-to-targets))
	  (computed-parameters (ana~substep-compute-parameters source (ana~task-of-node goal supports))))
      (if (not (null (first cors)))
	  ;; if the method computes the parameters itself
	  (append cors (list nil) computed-parameters)
	(append cors computed-parameters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Choice Controls For Demand Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(ana~defchoicecontrol demand-strategy-AnalogyInstMeta
;                      demand-strategy
;                      (conflict-set-computation-function conflict-demand-strategy-AnalogyInstMeta)
;                      (crules-computation-function crules-none))
;
;(defun conflict-demand-strategy-AnalogyInstMeta (roc cp-mapp)
;  (declare (edited  "10-JUN-2003")
;           (authors Scholl)
;           (input   "An analogy state description, a choice point mapping")
;           (effect  "None")
;           (value   "Alternatives: The strategy AnalogyInstMeta"))
;  (list (strat~find-strategy-ks 'AnalogyInstMeta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-strategy
		      demand-strategy
		      (conflict-set-computation-function conflict-demand-strategy)
		      (crules-computation-function crules-none))

(defun conflict-demand-strategy (roc cp-mapp strategy-name)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a strategy name")
	   (effect  "None")
	   (value   "Alternatives: The strategy with the name"))
  (list (strat~find-strategy-ks strategy-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-parameters-none
		      demand-parameters
		      (conflict-set-computation-function conflict-demand-parameters-none)
		      (crules-computation-function crules-none))

(defun conflict-demand-parameters-none (roc cp-mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping")
	   (effect  "None")
	   (value   "Alternatives: No parameters"))
  (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-parameters
		      demand-parameters
		      (conflict-set-computation-function conflict-demand-parameters)
		      (crules-computation-function crules-none))

(defun conflict-demand-parameters (roc cp-mapp choice-points)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a list of choice points")
	   (effect  "None")
	   (value   "Alternatives: The tupel of choice for the choice points"))
  (list (mapcar #'(lambda (choice-point) (mapp~get-component choice-point cp-mapp)) choice-points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Choice Controls For Instmeta Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-strategy-from-source
		      demand-strategy
		      (dependencies source)
		      (conflict-set-computation-function conflict-demand-strategy-from-source)
		      (crules-computation-function crules-none))

(defun conflict-demand-strategy-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "Alternatives: The strategy, if the source step is a segment, otherwise none"))
  (when (ana~seg-p source)
    (list (ana~seg-strategy source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol metavar-from-source
		      metavar
		      (dependencies source)
		      (conflict-set-computation-function conflict-metavar-from-source)
		      (crules-computation-function crules-none))

(defun conflict-metavar-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and ")
	   (effect  "None")
	   (value   "Alternatives: The corresponding meta variables from the source step in the target plan"))
  (when (ana~seg-instmeta-p source)
    (ana~cor-of (roc~analogy-table roc) (ana~seg-instmeta-metavar source) 'ana=cor-source-to-targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol metavar-from-agenda
		      metavar
		      (conflict-set-computation-function conflict-metavar-from-agenda)
		      (crules-computation-function crules-none))

(defun conflict-metavar-from-agenda (roc cp-mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping")
	   (effect  "None")
	   (value   "Alternatives: All instantiation tasks"))
  (mapcar 'agenda~inst-task-meta-var (ana~inst-tasks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol source-from-metavar
		      source
		      (dependencies steps metavar)
		      (conflict-set-computation-function conflict-source-from-metavar)
		      (crules-computation-function crules-none))

(defun conflict-source-from-metavar (roc cp-mapp steps metavar)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a list of source steps and a meta variable")
	   (effect  "None")
	   (value   "Alternatives: The source steps, that instantiated a corresponding meta variable"))
  (mapcar #'(lambda (source-metavar) (find-if #'(lambda (seg) (keim~equal (ana~seg-instmeta-metavar seg) source-metavar))
					      (remove-if-not 'ana~seg-instmeta-p steps)))
	  (ana~cor-of (roc~analogy-table roc) metavar 'ana=cor-target-to-sources)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-tasks-from-metavar
		      demand-tasks
		      (dependencies metavar)
		      (conflict-set-computation-function conflict-demand-tasks-from-metavar)
		      (crules-computation-function crules-none))

(defun conflict-demand-tasks-from-metavar (roc cp-mapp metavar)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a meta variable")
	   (effect  "None")
	   (value   "Alternatives: The instantiation task for the meta variable"))
  (mapcar 'list (remove-if-not #'(lambda (task) (keim~equal (agenda~inst-task-meta-var task) metavar)) (ana~inst-tasks))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(ana~defchoicecontrol instantiation-computed-from-metavar
;                      instantiation
;                      (dependencies source metavar)
;                      (conflict-set-computation-function conflict-instantiation-computed-from-metavar)
;                      (crules-computation-function crules-none))
;
;(defun conflict-instantiation-computed-from-metavar (roc cp-mapp source metavar)
;  (declare (edited  "10-JUN-2003")
;           (authors Scholl)
;           (input   "An analogy state description, a choice point mapping, a source step and a meta variable")
;           (effect  "None")
;           (value   "Alternatives: The computed instantiation by the computation function from the source step"))
;  (when (ana~seg-instmeta-p source)
;    (let* ((inst (apply (ana=ref-parameter 'compute-instantiation-function (ana~seg-roc source)) (list metavar))))
;      (when inst (list inst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol instantiation-do-nothing
		      instantiation
		      (conflict-set-computation-function conflict-instantiation-do-nothing)
		      (crules-computation-function crules-none))

(defun conflict-instantiation-do-nothing (roc cp-mapp)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping")
	   (effect  "None")
	   (value   "Alternatives: Empty list"))
  (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Choice Controls For PPlanner Segment Pattern}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol demand-tasks-from-source
		      demand-tasks
		      (dependencies source-plan source)
		      (conflict-set-computation-function conflict-demand-tasks-from-source)
		      (crules-computation-function crules-none))

(defun conflict-demand-tasks-from-source (roc cp-mapp source-plan source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a pds and a source step")
	   (effect  "None")
	   (value   "Alternatives: The task node tupels, that correspond to the source step"))
  (when (ana~seg-p source)
    (let ((source-goals (ana~seg-goals source source-plan)))
      (reverse (mapcar 'reverse
		       (ana~subsets (remove-if 'null
					       (mapcar 'ana~obj-to-task (ana~cor-set-of (roc~analogy-table roc) source-goals 'ana=cor-source-to-targets)))
		   (length source-goals)))))))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol source-strategy-from-source
		      source-strategy
		      (dependencies source)
		      (conflict-set-computation-function conflict-source-strategy-from-source)
		      (crules-computation-function crules-none))

(defun conflict-source-strategy-from-source (roc cp-mapp source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping and a source step")
	   (effect  "None")
	   (value   "Alternatives: The strategy, if the source step is a segment, otherwise none"))
  (when (ana~seg-p source)
    (list (ana~seg-strategy source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol start-condition-from-source
		      start-condition
		      (dependencies source source-strategy)
		      (conflict-set-computation-function conflict-start-condition-from-source)
		      (crules-computation-function crules-none))

(defun conflict-start-condition-from-source (roc cp-mapp source source-strategy)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a source step and a strategy")
	   (effect  "None")
	   (value   "Alternatives: The start condition of the source, if it is a segment, otherwise none"))
  (when (ana~seg-p source)
    (cond ((ana~seg-invoke-p source)
	   (list (strat~strategy-ks-condition source-strategy)))
	  ((ana~seg-reinvoke-p source)
	   (list #'(lambda (task) t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ana~defchoicecontrol substeps-from-source
		      substeps
		      (dependencies source-plan source)
		      (conflict-set-computation-function conflict-substeps-from-source)
		      (crules-computation-function crules-none))

(defun conflict-substeps-from-source (roc cp-mapp source-plan source)
  (declare (edited  "10-JUN-2003")
	   (authors Scholl)
	   (input   "An analogy state description, a choice point mapping, a pds and a source step")
	   (effect  "None")
	   (value   "Alternatives: The substeps, if the source is a segment, otherwise none"))
  (when (ana~seg-p source)
    (list (ana~seg-substeps source))))

