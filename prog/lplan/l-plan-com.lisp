;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "OMEGA")

(com~defcommand retract
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats planning)
  (function oc=retract)
  (help "Retracts the originator step of a line."))

(defun oc=retract (ndline)
  (let ((originator-step (pdsh~originator-step ndline)))
    (if originator-step
	(multiple-value-bind (inst-step-p add-mor-supports new-task-nodes-followed-by-inst new-task-nodes-other)
	    (back~retract-step originator-step 'retract nil omega*current-proof-plan)
	  (back~update-agenda nil
			      (union new-task-nodes-followed-by-inst new-task-nodes-other)
			      (pds~agenda omega*current-proof-plan)
			      omega*current-proof-plan))
      (omega~message "~A has no originator step" ndline))))

(com~defcommand backtrack
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats planning)
  (function oc=backtrack)
  (help "Backtracks the task of a line."))

(defun oc=backtrack (ndline)
  (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (pos (position ndline (mapcar 'agenda~task-node all-tasks)))
	 (task (when pos (nth pos all-tasks))))
    (if task
	(back~track-task task (pds~agenda omega*current-proof-plan) omega*current-proof-plan)
      (omega~message "There is no task with node ~A." ndline))))
