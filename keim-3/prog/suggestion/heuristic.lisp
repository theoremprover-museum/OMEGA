
(in-package :keim)



(mod~defmod HEUR 
            :uses (rsrc)
            :documentation ""
            :exports (
                      
		      heur~reset-resource-obj
                      heur~sort-p
		      defun heur~set-bb-agent-heur
                      defun heur~param-bb-agent-heur

		      heur*act-diff
                      heur*act-diff-help
                      heur*delta-activation-level
                      heur*delta-exec-time
                      heur*init-resource-value
                      heur*max-penalty
                      heur*wait-for-n-resets
		      heur*param-agent-eval-fun
		      heur*bb-agent-eval-fun
		      heur~bb-agent-evaluate-fun
		      heur~parameter-agent-evaluate-fun))


#{
\section{Heuristics}
#}

(defvar heur*init-resource-value 10 "A resource value initially assigned to each parameter
agent. This variable is used by the function heur~init")

(defvar heur*delta-exec-time 30
  "Time in seconds by which average exec-int must differ from prev. runs that activation
level is changed.")

(defvar heur*delta-activation-level 1
  "A number specifying by which amount activation level is changed.")

(defvar heur*max-penalty 10 "The max. number of penalties")

(defvar heur*wait-for-n-resets 3
  "The number of resets which have to occur bevor agents give themself a penalty")

(defvar heur*act-diff 1.0
  "The per cent difference by which the act user interval has to exceed the average
interval before activation level will be changed.")

(defvar heur*act-diff-help 1.0)



(defgeneric heur~reset-resource-obj (r-obj)
  (declare (edited  "27-APR-2000")
	   (authors Mth)
	   (input   "An agent resource object.")
	   (effect  "Sets all slots of the object to initial values. To be called before"
		    "updating resources or after evaluating parameter resources."
		    "This function also updates the paramter agents total values.")
	   (value   "None."))
   (:method ((r-obj rsrc+bb-agent-resource))
	    ;; store the resource information if necessary
	    (rsrc~print-res-info r-obj)
	    (setf (rsrc~last-entries r-obj) 0)
	    (setf (rsrc~last-used r-obj) 0)
	    (setf (rsrc~last-bad r-obj) 0)
	    (setf (rsrc~run-time-last r-obj) 0)
	    (setf (rsrc~agents-running r-obj) 0))
   (:method ((r-obj rsrc+param-agent-resource))
	    ;; if object was already evaluated then reset its data
	    ;; else mark object as evaluated
	    (if (rsrc~evaluated r-obj)
		(progn
		  ;; first update the total values:
		  (when (rsrc~running-state r-obj)
		    ;; but only if agent was running.
		    (progn 
		      (incf (rsrc~visited-entries r-obj) (rsrc~last-entries r-obj))
		      (incf (rsrc~useful-entries r-obj) (rsrc~last-used r-obj))
		      (incf (rsrc~bad-entries r-obj) (rsrc~last-bad r-obj))
		      (incf (rsrc~run-time-total r-obj) (rsrc~run-time-last r-obj))))
		  ;; store the resource information if necessary
		  (rsrc~print-res-info r-obj)
		  ;; now reset the data from last run
		  (setf (rsrc~run-time-last r-obj) 0)
		  (setf (rsrc~last-entries r-obj) 0)
		  (setf (rsrc~last-used r-obj) 0)
		  (setf (rsrc~last-bad r-obj) 0)
		  (setf (rsrc~evaluated r-obj) NIL))
	      (setf (rsrc~evaluated r-obj) T))))

#{
\subsection{Changing heuristics}
%Now an attempt to seperate the heuristics from the other
%code, making them easier accessible for changes:
In general, this mechanism works like the principle used
for the sorting criterias in the bb module.
Several heuristics for evaluating the agents performance
can be specified as methods.
These evaluation functions have to take an agents resource object
as argument and must return an integer which specifies the number
of given penalties !
;;
Each of these methods are specified by an indicator, e.g a
symbol.
A global variable can be bound to one of these symbols. The value
of this variable is then used to determine which function has to be
applied.
Different variables exists for every class of agents (e.g. blackboard
and command agents), which allows a flexibility in specifiyng
heuristics.
As a default all agents use the heuristic specified as :test.
#}



(defvar heur*param-agent-eval-fun :test
  "A variable specifying the evaluation function for parameter agents")

(defvar heur*bb-agent-eval-fun :test
  "A variable specifying the evaluation function for blackboard agents")

(defun heur~set-bb-agent-heur (indicator)
  (setf heur*bb-agent-eval-fun indicator))

(defun heur~set-param-agent-heur (indicator)
  (setf heur*param-agent-eval-fun indicator))


(defmethod heur~parameter-agent-evaluate-fun
  ((resource-obj rsrc+param-agent-resource) (indicator (eql :test)))
  (let* ((penalty 0)
	 (time-per-entry-last-run (/ (rsrc~run-time-last resource-obj)
						 (rsrc~last-entries resource-obj)))
	 (time-per-entry-all-runs (/ (rsrc~run-time-total resource-obj)
						 (rsrc~visited-entries resource-obj)))
	 (p (/ heur*max-penalty 2)))
    (when (rsrc~running-state resource-obj)
      ;; if agent was running
      (if (eql (rsrc~serious-reset resource-obj) 1)
	  ;; if agent could not finish computation ...
	  (progn
	    (setf penalty (round (* p (/ time-per-entry-last-run
						     time-per-entry-all-runs))))
	    (if (> penalty heur*max-penalty)
		(setf penalty (- heur*max-penalty))
	      (setf penalty (- penalty)))
	    (setf (rsrc~serious-reset resource-obj) 0))
	    	;; else ...
	(when (rsrc~last-used resource-obj)
	  (progn
	    (setf penalty (round (* p (/ time-per-entry-all-runs
						     time-per-entry-last-run))))
	    (when (> penalty heur*max-penalty)
	      (setf penalty heur*max-penalty))
	    ;; FOR DEMONSTRATION ONLY
	    (setf penalty 0))
	  penalty)))
    penalty))



(defmethod heur~bb-agent-evaluate-fun
  ((resource-obj rsrc+param-agent-resource) (indicator (eql :test)))
  0)













	   
	   


			   


		    
	   



