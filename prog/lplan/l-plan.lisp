;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; plan.lisp; This file is part of the OMEGA system
;;
;; major updates: 24.2.1999,
;; 
;;
;; Authors: Lassaad Cheikhrouhou, Juergen Zimmer, Carsten Ullrich
;; email: {lassaad,jzimmer, cullrich}@ags.uni-sb.de 
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

;; Planungs-Algorithmus
(in-package "OMEGA")

(mod~defmod PLAN 
            :uses (lagenda arg com comint cri env foci help infer inter just keim logic mapp meth cosint
			  node oc omega ot pds pdsc pdsj pdsn pos post prob socket subst sys term type)
            :documentation "The data structures and algorithm for agenda-based backward/forward planning. "
            :exports (
                      ))

(defvar plan*planning-methods nil "List of methods used by default for planning.")
(defvar plan*control-rules nil "List of control rules used for planning.")
(defvar plan*g-inapplicable-methods nil "List of collected g-inapplicable-methods.")
(defvar plan*s-inapplicable-methods nil "List of collected s-inapplicable-methods.")
(defvar plan*watch-ref nil "Method to watch for.")

(defun plan~trace (format-string &rest args)
  (declare (edited  "22-MAY-1997")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((ref (when (position :ref args) (nth (+ 1 (position :ref args)) args))))
    (when (or plan*trace (if (listp ref) (find plan*watch-ref ref) (eq plan*watch-ref ref)))
      (apply #'omega~trace (cons format-string args))
      nil)))
    
(defun plan~message (format-string &rest args)
  (declare (edited  "22-MAY-1997")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (when plan*messages
    ;; (apply #'omega~message (cons format-string args))
    (apply 'format (cons t (cons (concatenate 'string format-string "~%") args)))
    NIL))

(defgeneric pds~update-current-formulas (nodes cs)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A schematic node or a list of schematic nodes, a substitution")
	   (effect  "Updates the current formula by applying the substition to the current formula.")
	   (value   "First the changed nodes and second the unchanged nodes."))
  (:method ((nodes list) cs)
	   (when nodes
	     (multiple-value-bind (first-changed first-unchanged)
		 (pds~update-current-formulas (first nodes) cs)
	       (multiple-value-bind (rest-changed rest-unchanged)
		   (pds~update-current-formulas (rest nodes) cs)
		 (values (append first-changed rest-changed) (append first-unchanged rest-unchanged))))))
  (:method ((node pdsn+node) cs)
	   (let ((old-formula (pds~node-formula node))
		 (new-formula (if (pdsh~cs-new-bindings cs)
				  (multiple-value-bind (f new-inst add-inst)
				      (cstr~inst-apply (pds~node-formula node)
						       (cstr~inst-from-subst (pdsh~cs-new-bindings cs)))
				    (setf (pdsh~cs-new-bindings cs) (cstr~inst-to-subst new-inst))
				    (setf (pdsh~cs-constraint cs) (cstr~update (pdsh~cs-constraint cs)
									       add-inst))
				    (beta~normalize f))
				(pds~node-formula node))))
	     (setf (pdsn~current-formula node) new-formula
		   (pdsn~up-to-date node) T)
	     (if (data~equal old-formula new-formula)
		 ;; formula did not change
		 (values nil (list node))
	       ;; formula has changed
	       (values (list node) nil)))))

(defgeneric pds~reset-current-formulas (nodes subst &optional already-changed already-unchanged)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A schematic node or a list of schematic nodes, a substitution, a list of already changed nodes"
		    "and a list of already considered, but unchanged nodes.")
	   (effect  "Resets the current formula by applying the substition to the original formula.")
	   (value   "First the changed nodes and second the unchanged nodes."))
  (:method ((nodes list) subst &optional already-changed already-unchanged)
	   (when nodes
	     (multiple-value-bind (first-changed first-unchanged)
		 (pds~reset-current-formulas (first nodes) subst already-changed already-unchanged)
	       (multiple-value-bind (rest-changed rest-unchanged)
		   (pds~reset-current-formulas (rest nodes) subst already-changed already-unchanged)
		 (values (append first-changed rest-changed) (append first-unchanged rest-unchanged))))))
  (:method ((node pdsn+node) subst &optional already-changed already-unchanged)
	   (cond ((find node already-changed) (values (list node) nil))
		 ((find node already-unchanged) (values nil (list node)))
		 (t 
		  (let ((old-formula (pds~node-formula node))
			(new-formula (if subst 
					 (beta~normalize (cstr~inst-apply (node~formula node) (cstr~inst-from-subst subst)))
				       (node~formula node))))
		    (setf (pdsn~current-formula node) new-formula
			  (pdsn~up-to-date node) T)
		    (if (data~equal old-formula new-formula)
			;; formula did not change
			(values nil (list node))
		      ;; formula has changed
		      (values (list node) nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Planning Step Data Structure}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plan+step ()
  ((method            :initarg :method
		      :initform nil
		      :accessor plan~step-method
		      :documentation "The matched method.")
   (focus-goal        :initarg :focus-goal
		      :initform nil
		      :accessor plan~step-focus-goal
		      :documentation "The goal to be closed: just for OUTLINE-PAT.")
   (setting           :initarg :setting
		      :accessor plan~step-setting
		      :documentation "The binding of the method meta-variables to pds-objects.")
   (mor-scope         :initarg :mor-scope
		      :initform nil
		      :accessor plan~step-mor-scope
		      :documentation "The mor scope of the method application.")
   (new-pure-metavars :initarg :new-pure-metavars
		      :initform nil
		      :accessor plan~step-new-pure-metavars)
   (new-nodes        :initarg :new-nodes
		     :initform nil
		     :accessor plan~step-new-nodes)
   (conclusions      :initarg :conclusions
		     :initform nil
		     :accessor plan~step-conclusions)
   (premises         :initarg :premises
		     :initform nil
		     :accessor plan~step-premises))
  (:documentation "Some information about a planning step."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Helper Functions For Planning Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=create-mor-scope (new-constraint pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A constraint and a pds.")
	   (effect  "Creates a new mor scope by merging the new constraint and the old constraints from the pds.")
	   (value   "A tuple: First a boolean value indicating the success of the merging operation,"
		    "second the newly created mor scope."))
  (when new-constraint
    (if (cstr~constraint-p new-constraint)
	;; there are new constraints introduced by the application condition
	(let* ((new-variables-of-constraint (cstr~variables new-constraint))
	       (relevant-pds-scopes (remove-if-not #'(lambda (mor-scope)
						       (intersection new-variables-of-constraint
								     (pdsh~domain mor-scope)))
						   (pds~constraint-store pds)))
	       (relevant-pds-mor-scopes (remove-if-not 'pdsh~mor-scope-p relevant-pds-scopes))
	       (relevant-constraint-states (mapcar 'pdsh~ms-cstr-state relevant-pds-mor-scopes)))
	  (multiple-value-bind (constraint new-bindings)
	      (cstr~merge new-constraint (mapcar 'pdsh~cs-constraint relevant-constraint-states))
	    (when constraint
	      ;; the new constraints are consistent with the pds contraint store
	      (values t (pdsh~ms-create (pdsh~cs-create new-bindings constraint relevant-constraint-states)
					relevant-pds-scopes
					(cstr~variables constraint))))))
      ;; no new constraints are introduced by application condition
      (values t nil))))

(defun plan=new-metavars (new-nodes pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A list of nodes and a pds.")
	   (effect  "None.")
	   (value   "The meta variables which are newly introduced in these nodes."))
  (let ((node-variables (when new-nodes
			  (reduce 'union (mapcar 'pdsn~metavars new-nodes)))))
    (set-difference node-variables (pdsh~domain (pds~constraint-store pds)))))

(defun plan=new-pure-metavars (new-nodes new-mor-scope pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A list of nodes, a mor scope and a pds.")
	   (effect  "None.")
	   (value   "The meta variables which are newly introduced in these nodes"
		    "and which are not connected or bound by any constraint."))
  (set-difference (plan=new-metavars new-nodes pds) (when new-mor-scope (pdsh~ms-domain new-mor-scope))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Planning Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan~step-update-condition (step setting)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a setting.")
	   (effect  "Updates the setting slot of the planning step to the choosen setting.")
	   (value   "Undefined."))
  (setf (plan~step-setting step) setting))

(defun plan=step-apply-new-bindings (step new-bindings)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a list of bindings.")
	   (effect  "Applies every binding in the list to the setting of the planning step.")
 	   (value   "The changed setting."))
  ;; apply the new bindings on the codomain of the mapping
  (when (and new-bindings (not (subst~empty-p new-bindings)))
    (let* ((setting-subst (meth~mapp-subst (plan~step-setting step)))
	   (updated-codom (cstr~inst-apply (subst~codomain setting-subst) (cstr~inst-from-subst new-bindings)))
	   (updated-subst (meth~subst-create (subst~domain setting-subst) updated-codom)))
      (meth~mapp-new-subst (plan~step-setting step) updated-subst)
      (plan~step-setting step))))
  
(defun plan~step-cstr-state (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "None.")
 	   (value   "The mor scope of the planning step."))
  (when (plan~step-mor-scope step)
    (pdsh~ms-cstr-state (plan~step-mor-scope step))))

(defun plan~step-instantiating-p (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "None.")
 	   (value   "True iff the step introduced some new bindings."))
  (and (plan~step-cstr-state step) (pdsh~cs-new-bindings (plan~step-cstr-state step))
       (not (subst~empty-p (pdsh~cs-new-bindings (plan~step-cstr-state step))))))

(defun plan~step-update-mor-scope-reason (step reason)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a reason.")
	   (effect  "Updates the reason of the mor scope of the step.")
	   (value   "Undefined."))
  (when (plan~step-mor-scope step)
    (setf (pdsh~ms-reason (plan~step-mor-scope step)) reason)))

(defun plan~step-update-new-nodes (step new-nodes pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step, a list of nodes and a pds.")
	   (effect  "Updates the new nodes and new pure meta variables slots of the step.")
	   (value   "Undefined."))
  (setf (plan~step-new-nodes step) (union (plan~step-new-nodes step) new-nodes))
  (setf (plan~step-new-pure-metavars step) (union (plan~step-new-pure-metavars step)
						  (plan=new-pure-metavars new-nodes (plan~step-mor-scope step) pds))))

(defun plan~step-update-mor-scope (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Updates the mor scope of the step by evaluating the constraints of the setting of the step.")
	   (value   "True iff the new constraints are consistent with th old."))
  (let ((new-constraint (meth~mapp-constraint (plan~step-setting step))))
    (multiple-value-bind (consistent mor-scope)
	(plan=create-mor-scope new-constraint pds)
      (when consistent
	(setf (plan~step-mor-scope step) mor-scope)
	(when mor-scope
	  (let ((new-bindings (pdsh~cs-new-bindings (pdsh~ms-cstr-state mor-scope))))
	    (plan=step-apply-new-bindings step new-bindings)))
	t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Application Planning Step Data Structure}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plan+application (plan+step)
  ((parameters    :initarg :parameters
		  :initform nil
		  :accessor plan~appl-parameters
		  :documentation "The matched method parameters.")
   (matched-prems :initarg :matched-prems
		  :initform nil
		  :accessor plan~appl-matched-prems)
   (exist-prems   :initarg :exist-prems
		  :initform nil
		  :accessor plan~appl-exist-prems
		  :documentation "The unsigned premises: just for OUTLINE-PAT.")
   (clsed-prems   :initarg :clsed-prems
		  :initform nil
		  :accessor plan~appl-clsed-prems
		  :documentation "The premises to be deleted: just for OUTLINE-PAT."))
  (:documentation "Some information about an application planning step."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Application Planning Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan~appl-p (object)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An arbitrary thing.")
	   (effect  "None.")
	   (value   "True iff the thing is an application planning step."))
  (typep object 'plan+application))

(defmethod print-object ((object plan+application) stream)
  (format stream "~A matched with focus goal ~A, existent premises ~A, closed premises ~A and parameters ~A."
	  (plan~step-method object)
	  (plan~step-focus-goal object)
	  (plan~appl-exist-prems object)
	  (plan~appl-clsed-prems object)
	  (plan~appl-parameters object)))

(defun plan~appl-create (method focus-goal)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A method and the node of the focus goal.")
	   (effect  "Creates a new application planning step.")
	   (value   "The new application planning step."))
  (make-instance 'plan+application
		 :method method
		 :focus-goal focus-goal))

(defun plan~appl-add-concs (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The add conclusions of the application."))
  (set-difference (plan~step-conclusions step) (list (plan~step-focus-goal step))))

(defun plan~appl-subgoals (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The subgoals of the application."))
  (set-difference (plan~step-premises appl) (plan~appl-matched-prems appl)))

(defun plan~appl-update-parameters (appl parameters)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a list of parameters.")
	   (effect  "Updates the parameter slot of the application step object.")
	   (value   "Undefined."))
  (setf (plan~appl-parameters appl) parameters))

(defun plan~appl-update-supports (appl settings supports)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of possible settings and a list of support nodes.")
	   (effect  "Updates the slots concerning the premises of the application step."
		    "It is seen as given, that all settings match the premises to the same nodes.")
	   (value   "Undefined."))
  (let ((setting (first settings)))
    (setf (plan~appl-clsed-prems appl) (meth~compute-closed-premises (plan~step-method appl) setting))
    (setf (plan~appl-exist-prems appl) (meth~compute-existent-premises (plan~step-method appl) setting))
    (setf (plan~appl-matched-prems appl) supports)))

(defun plan~appl-update-condition (appl setting)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a setting.")
	   (effect  "Updates the setting slot of the application step to the choosen setting"
		    "and computes the output parameters from the setting.")
	   (value   "Undefined."))
  (plan~step-update-condition appl setting)
  (setf (plan~appl-parameters appl) (meth~compute-parameters (plan~step-method appl) setting)))

(defun plan~appl-parameters-complete-p (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "True iff the parameters are already completely specified."))
  (= (length (plan~appl-parameters appl))
     (length (meth~parameters (plan~step-method appl)))))

(defun plan~appl-focus-normal-supports (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The supports of the focus goal."))
  (pdsn~normal-supports (plan~step-focus-goal appl)))

(defun plan~appl-focus-extra-supports (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The supports of the focus goal."))
  (pdsn~extra-supports (plan~step-focus-goal appl)))

(defun plan~appl-inherited-normal-supports (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The inherited supports for the subgoals."))
  (set-difference (plan~appl-focus-normal-supports appl) (plan~appl-clsed-prems appl)))

(defun plan~appl-inherited-extra-supports (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The inherited extra supports for the subgoals."))
  (set-difference (plan~appl-focus-extra-supports appl) (plan~appl-clsed-prems appl)))

(defun plan~appl-inherited-supports (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The inherited supports for the subgoals."))
  (union (plan~appl-inherited-normal-supports appl) (plan~appl-inherited-extra-supports appl)))

(defun plan~appl-extra-hypotheses (appl subgoal)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a subgoal.")
	   (effect  "None.")
	   (value   "The extra hypotheses of the subgoal wrt. to the focus goal."))
  (set-difference (pdsn~hyps subgoal) (pdsn~hyps (plan~step-focus-goal appl))))

(defun plan~appl-backward-p (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "True iff the step is a backward application."))
  (meth~goal (plan~step-method appl)))

(defun plan~appl-forward-p (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "True iff the step is a forward application."))
  (not (plan~appl-backward-p appl)))

(defun plan~appl-secondary-goals (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The secondary goals of the application."))
  (when (plan~appl-forward-p appl) (list (plan~step-focus-goal appl))))

(defun plan~appl-primary-goals (appl)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step.")
	   (effect  "None.")
	   (value   "The primary goals of the application."))
  (when (plan~appl-backward-p appl) (list (plan~step-focus-goal appl))))

(defun plan~appl-create-outline (appl nodes)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A application planning step and a list of nodes.")
	   (effect  "None.")
 	   (value   "A list of outline patterns, representing every node."))
  (mapcar #'(lambda (node) (cond ((find node (plan~appl-primary-goals appl)) (infer~existent))
				 ((find node (plan~appl-clsed-prems appl)) (infer~deleted))
				 ((find node (plan~appl-exist-prems appl)) (infer~persistent))
				 (t (infer~added))))
	  nodes))

(defun plan~appl-redundant-step-p (appl pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A application planning step and a pds.")
	   (effect  "None.")
 	   (value   "The reason of the previous step, the application planning step is redundant to."
		    "Nil, if the planning step is not redundant"))
  (when (plan~appl-forward-p appl)
    (some #'(lambda (rsn) (pdsh~redundant-step-p rsn (meth~inference (plan~step-method appl))
						 (plan~appl-secondary-goals appl)
						 (plan~appl-matched-prems appl)
						 (plan~appl-inherited-supports appl)))
	  (pds~all-reasons pds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Expansion Planning Step Data Structure}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plan+expansion (plan+step)
  ()
  (:documentation "Some information for the expansion of a previous planning step."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Proper Refinement Step Data Structure}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plan+proper (plan+step)
  ((task               :initform nil
		       :accessor plan~proper-task
		       :initarg :task)
   (ref                :initform nil
		       :accessor plan~proper-ref
		       :initarg :ref)
   (params             :initform nil
		       :accessor plan~proper-params
		       :initarg :params)
   (exec-params        :initform nil
		       :accessor plan~proper-exec-params
		       :initarg :exec-params))
  (:documentation "Some information for the expansion of a previous planning step."))

(defun plan~proper-p (obj)
  (typep obj 'plan+proper))

(defun plan~proper-create (task ref)
  (make-instance 'plan+proper :task task :ref ref))

(defun plan~proper-update-params (proper params)
  (setf (plan~proper-params proper) params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Expansion Planning Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan~exp-create (goal)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "The node of a focus goal, must be a pseudo goal.")
	   (effect  "Creates a new expension planning step for this goal.")
	   (value   "The new expension planning step."))
  (let ((just (node~justification goal)))
    ;;  (reason (pdsh~originator-step goal)))
    (make-instance 'plan+expansion
		   :focus-goal goal
		   :method (meth~find-method (infer~find-arbitrary-application-name (just~method just)))
		   :setting (pdsj~subst just)
		   :premises (just~premises just))))

(defun plan=exp-new-bindings (exp)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An expansion planning step.")
	   (effect  "None.")
	   (value   "The bindings introduced between the application of a method and its expansion."))
  (let* ((reasons (pdsh~reasons-later-than (pdsh~originator-step (plan~step-focus-goal exp))))
	 (new-bindings-list (remove-if 'null (mapcar #'(lambda (rsn)
							 (when (pdsh~reason-mor-effect rsn)
							   (pdsh~cs-new-bindings (pdsh~ms-cstr-state (pdsh~reason-mor-effect rsn)))))
						     reasons))))
    (when new-bindings-list
      (reduce 'subst~disjoint-compose-substitution new-bindings-list))))
								 
(defun plan=exp-apply-new-bindings (exp)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An expansion planning step.")
	   (effect  "Applies the bindings between application and expansion to the setting of the expansion.")
	   (value   "The changed setting."))
  ;; applies new bindings since the originator step of exp to the method mapping
  (let ((setting (plan~step-setting exp))
	(new-bindings (plan=exp-new-bindings exp)))
    (when new-bindings
      (plan=step-apply-new-bindings exp new-bindings))
    setting))

(defun plan=exp-update-hyps (exp)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An expansion planning step.")
	   (effect  "Updates the hypotheses of the expansion nodes by adding the common hypotheses of the conclusions.")
	   (value   "Undefined."))
  (let* ((concs (plan~step-conclusions exp))
	 (common-hyps (when concs (reduce 'intersection (mapcar 'pdsn~hyps concs)))))
    (dolist (new-node (plan~step-new-nodes exp))
      (setf (pdsn~hyps new-node) (union (pdsn~hyps new-node) common-hyps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Both Application And Expansion Planning Steps}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric plan~step-all-nodes (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "None.")
	   (value   "All nodes involved in the planning step."))
  (:method ((appl plan+application))
	   (cons (plan~step-focus-goal appl) (append (plan~step-new-nodes appl) (plan~appl-matched-prems appl))))
  (:method ((exp plan+expansion))
	   (append (plan~step-premises exp) (list (plan~step-focus-goal exp)) (plan~step-new-nodes exp))))

(defgeneric plan~step-agenda-tasks (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "None.")
	   (value   "The nodes for which tasks have to be created."))
  (:method ((appl plan+application))
	   (if (meth~non-reliability (plan~step-method appl))
	       (append (plan~appl-subgoals appl) (list (plan~step-focus-goal appl)))
	     (plan~appl-subgoals appl)))
  (:method ((exp plan+expansion))
	   (remove-if-not #'(lambda (new-node) (pdsj~open-p (node~justification new-node))) (plan~step-new-nodes exp)))
  (:method ((ref plan+proper))
	   nil))

(defgeneric plan~step-create-reason (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Creates a new reason depending on the kind of the planning step.")
	   (value   "The newly created reason."))
  (:method ((appl plan+application) pds)
	   (let ((ia-reason (pds~ia-create (plan~step-conclusions appl) (plan~appl-secondary-goals appl)
					   (plan~step-method appl)
					   (plan~appl-create-outline appl (append (plan~step-conclusions appl)
										  (plan~step-premises appl)))
					   (plan~step-new-pure-metavars appl)
					   (plan~step-new-nodes appl) pds)))
	     (plan~step-update-mor-scope-reason appl ia-reason)
	     (setf (pdsh~reason-mor-effect ia-reason) (plan~step-mor-scope appl))
	     ia-reason))
  (:method ((exp plan+expansion) pds)
	   (let* ((focus-goal (plan~step-focus-goal exp))
		  (ie-reason (pds~ie-create (list focus-goal) (pdsj~above (node~justification focus-goal))
					    (plan~step-new-nodes exp) pds)))
	     (plan~step-update-mor-scope-reason exp ie-reason)
	     (setf (pdsh~reason-mor-effect ie-reason) (plan~step-mor-scope exp))
	     ie-reason)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Testing The Admissibility Of Refinements}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric plan=relevant-wrt-goal-p (appl failed-step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a valid failed step with fitting inference.")
	   (effect  "None.")
	   (value   "A tuple: First a boolean value indicating inadmissibility,"
		    "Second the step, if its still relevant, otherwise nil."))
  (:method (appl failed-step)
	   (let ((ref (plan~step-method appl)))
	     (if (and (plan~appl-backward-p appl)
		      (pdsn~schematic-p (plan~step-focus-goal appl)))
		 (when (data~equal-p (pds~node-formula (plan~step-focus-goal appl))
				     (pdsh~fs-goal-formula failed-step))
		   (if (and (not (meth~parameters ref)) (not (meth~premises-to-match ref)))
		       (progn
			 (plan~trace "    The refinement is inadmissible." :ref (plan~step-method appl))
			 (values t nil)
			 t)
		     (values nil failed-step)))
	       ;; the method has no goal or the goal is not schematic
	       (values nil failed-step)))))
  
(defgeneric plan=relevant-wrt-params-p (appl failed-step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a valid failed step with fitting inference.")
	   (effect  "None.")
	   (value   "A tuple: First a boolean value indicating inadmissibility,"
		    "Second the step, if its still relevant, otherwise nil."))
  (:method (appl failed-step)
	   (if (plan~appl-parameters-complete-p appl)
	       ;; parameters are completely specified
	       (when (and (= (length (plan~appl-parameters appl)) (length (pdsh~fs-parameters failed-step)))
			  (every 'keim~equal (plan~appl-parameters appl)
				 (pdsh~fs-parameters failed-step)))
		 ;; the parameters are the same as in the failed step
		 (if (meth~premises-to-match (plan~step-method appl))
		     (values nil failed-step)
		   (progn
		     (plan~trace "        The refinement is inadmissible." :ref (plan~step-method appl))
		     (values t nil))))
	     (values nil failed-step))))

(defgeneric plan=relevant-wrt-supps-p (appl failed-step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a valid failed step with fitting inference.")
	   (effect  "None.")
	   (value   "A tuple: First a boolean value indicating inadmissibility,"
		    "Second the step, if its still relevant, otherwise nil."))
  (:method (appl failed-step)
	   (when (every #'(lambda (prem used-support)
			    (or (eq prem used-support)
				(and (pdsn~schematic-p prem)
				     (keim~equal (pds~node-formula prem)
						 used-support))))
			(plan~appl-matched-prems appl) (pdsh~fs-used-supports failed-step))
	     ;; all premises are the same as in the failed step
	     (if (plan~appl-parameters-complete-p appl)
		 (progn
		   (plan~trace "          The refinement is inadmissible." :ref (plan~step-method appl))
		   (values t nil))
	       (values nil failed-step)))))

(defgeneric plan=relevant-wrt-setting-p (appl failed-step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step and a valid failed step with fitting inference.")
	   (effect  "None.")
	   (value   "True iff the parameters of the method are the same as those in the failed step."))
  (:method (appl failed-step)
	   (when (every 'keim~equal (plan~appl-parameters appl) (pdsh~fs-parameters failed-step))
	     (plan~trace "            The refinement is inadmissible." :ref (plan~step-method appl))
	     (values t nil))))

#{
The following function collects the results of the relevance tests for the failed steps.
Its an higher order function and should be called with one of the above functions as test.
#}

(defun plan=inadmissible-p (appl failed-steps test)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of valid failed steps with fitting inference"
		    "and a test function.")
	   (effect  "None.")
	   (value   "A tuple: First a boolean value indicating inadmissibility,"
		    "Second the still relevant failed steps."))
  (let ((relevant-failed-steps (mapcar #'(lambda (failed-step)
					   (multiple-value-bind (inadmissible failed-step)
					       (funcall test appl failed-step)
					     (list inadmissible failed-step)))
				       failed-steps)))
    (if (some #'first relevant-failed-steps)
	(values t nil)
      (values nil (remove-if 'null (mapcar #'second relevant-failed-steps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions That Extend A Given Matching}
The method matching is extended in four steps. In the first and the last step the permanently
inapplicable methods are updated as a side effect.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=match (appl settings meth-obj pds-obj)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of settings, a method object and a pds object.")
	   (effect  "Matches the pds object to the method object within all settings.")
	   (value   "The matchable new settings."))
  (declare (ignore appl))
  ;; (plan~trace "settings: ~A" settings)
  (reduce 'append (mapcar #'(lambda (setting)
			      ;; (plan~trace "setting: ~A" setting)
			      (let ((new-settings (meth~match-p meth-obj pds-obj (meth~mapping-copy setting) :one2one)))
				;; (plan~trace "setting: ~A" setting)
				(if (listp new-settings) new-settings (list new-settings))))
			  settings)))

(defgeneric plan=match-goal (appl settings failed-steps task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of settings, a list of failed steps and a task.")
	   (effect  "Updates the permanently inapplicable methods of the task node."
		    "Updates the application planning step.")
	   (value   "A tuple: First the application step, if the goal could be matched."
		    "Second the new settings, third the still relevant failed steps."))
  (:method (appl settings failed-steps (task lagenda+task))
	   (let* ((ref (plan~step-method appl))
		  (meth-goal (meth~goal ref))
		  (task-goal (lagenda~task-node task)))
	     (if meth-goal
		 ;; method is a backward method
		 (let ((new-settings (plan=match appl settings meth-goal task-goal)))
		   (if new-settings
		       ;; goal does match
		       (multiple-value-bind (inadmissible relevant-fs)
			   (plan=inadmissible-p appl failed-steps 'plan=relevant-wrt-goal-p)
			 (when (not inadmissible)
			   (values appl new-settings relevant-fs)))
		     ;; goal does not match
		     (progn
		       ;; now test for g-inapplicability
		       ;; this is only a side effect
		       (when (not (meth~parameters ref))
			 ;; there is no other way to apply this method
			 (plan~trace "    /// ~A is g-inapplicable to ~A ///" ref task-goal :ref ref)
			 (misc~append-to-slot plan*g-inapplicable-methods ref))
		       (values nil nil nil))))
	       ;; refinement is a forward method
	       (values appl settings failed-steps)))))

(defgeneric plan=bind-parameters (appl settings failed-steps param-tuple)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of settings, a list of failed steps and a parameter tuple.")
	   (effect  "Updates the application planning step.")
	   (value   "A tuple: First the application step, if the parameters could be matched."
		    "Second the new settings, third the still relevant failed steps."))
  (:method (appl settings failed-steps param-tuple)
	   (let ((ref (plan~step-method appl)))
	     (if (meth~parameters ref)
		 ;; method needs parameters
		 (let ((meth-params (subseq (meth~parameters ref) 0 (length param-tuple))))
		   (plan~trace "        Binding parameters ~A to ~A" param-tuple meth-params :ref ref)
		   (let ((new-settings (plan=match appl settings meth-params param-tuple)))
		     (when new-settings
		       ;; parameters could be matched
		       ;; there is only one possibility for mapping
		       (plan~appl-update-parameters appl param-tuple)
		       (multiple-value-bind (inadmissible relevant-fs)
			   (plan=inadmissible-p appl failed-steps 'plan=relevant-wrt-params-p)
			 (when (not inadmissible)
			   (values appl new-settings relevant-fs))))))
	       ;; method needs no parameters
	       (values appl settings failed-steps)))))

(defgeneric plan=match-supports (appl settings failed-steps supp-tuple)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a list of settings, a list of failed steps and a support tuple.")
	   (effect  "Updates the application planning step.")
	   (value   "A tuple: First the application step, if the supports could be matched."
		    "Second the new settings, third the still relevant failed steps."))
  (:method (appl settings failed-steps supp-tuple)
	   (let ((premises-to-match (meth~premises-to-match (plan~step-method appl))))
	     ;; (plan~trace "settings: ~A" settings)
	     (if premises-to-match
		 ;; method needs premises
		 (let ((new-settings (plan=match appl settings premises-to-match supp-tuple)))
		   ;; (plan~trace "settings: ~A" settings)
		   (when new-settings
		     ;; premises could be matched
		     ;; (plan~trace "supp-tuple: ~A" supp-tuple)
		     ;; (plan~trace "new settings: ~A" new-settings)
		     (plan~appl-update-supports appl new-settings supp-tuple)
		     (multiple-value-bind (inadmissible relevant-fs)
			 (plan=inadmissible-p appl failed-steps 'plan=relevant-wrt-supps-p)
		       (when (not inadmissible)
			 (values appl new-settings relevant-fs)))))
	       ;; method needs no premises
	       (values appl settings failed-steps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions For Updating Instantly Inapplicable Methods}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=collect-inapplicable-methods (ref task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement and a task.")
	   (effect  "Test the method for inapplicability"
		    "and evtl. add the method to the inapplicable methods lists.")
	   (value   "Undefined."))
  (when (and (meth~goal ref) (not (meth~parameters ref)))
    (plan~trace "        /// ~A is g-inapplicable to ~A ///" ref (lagenda~task-node task) :ref ref)
    (misc~append-to-slot plan*g-inapplicable-methods ref))
  (when (and (meth~premises-to-match ref) (not (meth~parameters ref)))
    (plan~trace "        /// ~A is s-inapplicable to ~A ///" ref (lagenda~task-node task) :ref ref)
    (misc~append-to-slot plan*s-inapplicable-methods ref)))

(defun plan=update-inapplicable-methods (goal)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A goal node.")
	   (effect  "Updates the inapplicable method slots of the goal node.")
	   (value   "Undefined."))
  (when (subsetp (pdsn~s-inapplicable-methods goal) plan*s-inapplicable-methods)
    (plan~trace "Uniting extra support with supports.")
    (misc~unite-with-slot (pdsn~normal-supports goal) (pdsn~extra-supports goal))
    (setf (pdsn~extra-supports goal) nil))
  (plan~trace "Collected g-inapplicable methods: ~A" plan*g-inapplicable-methods)
  (plan~trace "Collected s-inapplicable methods: ~A" plan*s-inapplicable-methods)
  (misc~unite-with-slot (pdsn~g-inapplicable-methods goal) plan*g-inapplicable-methods)
  (misc~unite-with-slot (pdsn~s-inapplicable-methods goal) plan*s-inapplicable-methods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions Dealing With Method Sets}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=failed-methods-with-sole-setting (task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "All methods in the failed steps of task node which can only applied with"
		    "a sole setting."))
  (let ((failed-steps (pdsn~failed-steps (lagenda~task-node task))))
    (mapcar 'pdsh~fs-inference
	    (remove-if-not 'pdsh~fs-sole-setting-p failed-steps))))

(defun plan=relevant-methods (task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "The relevant methods for solving the task."))
  (declare (ignore task))
  (if plan*planning-methods plan*planning-methods
    (let ((methods (remove-duplicates (append meth*planning-methods meth*MOReasoning-methods
					      meth*normalizing-methods meth*restricting-methods))))
      (setq plan*planning-methods methods)
      methods))
  )


  
(defun plan=alternative-methods (task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "The alternative methods for solving the task."))
  (let* ((task-node (lagenda~task-node task))
	 (inapplicable-methods (if (pdsn~extra-supports task-node)
				   (progn
				     (plan~trace "  There are extra supports: ~A" (pdsn~extra-supports task-node))
				     (plan~trace "  Trying s-inapplicable methods again with extra supports: ~A"
						 (pdsn~s-inapplicable-methods task-node))
				     (plan~trace "  The refinement ~A is s-inapplicable, but is tried again with extra supports."
						 plan*watch-ref :ref (pdsn~s-inapplicable-methods task-node))
				     (set-difference (pdsn~g-inapplicable-methods task-node)
						     (pdsn~s-inapplicable-methods task-node)))
				 (progn
				   (plan~trace "  There are no extra supports.")
				   (plan~trace "  S-inapplicable methods are not tried again: ~A"
					       (pdsn~s-inapplicable-methods task-node))
				   (plan~trace "  The refinement ~A is s-inapplicable and will not be tried again."
					       plan*watch-ref :ref (pdsn~s-inapplicable-methods task-node))
				   (union (pdsn~g-inapplicable-methods task-node)
					  (pdsn~s-inapplicable-methods task-node)))))
	 (failed-methods-with-sole-setting (plan=failed-methods-with-sole-setting task))
	 (relevant-methods (plan=relevant-methods task)))
    (plan~trace "  The relevant methods contain the refinement ~A." plan*watch-ref :ref relevant-methods)
    (plan~trace "  The inapplicable methods contain the refinement ~A." plan*watch-ref :ref inapplicable-methods)
    (plan~trace "  The failed methods contain the refinement ~A." plan*watch-ref :ref failed-methods-with-sole-setting)
    (sort (remove-if #'(lambda (method)
			 (or
			  (find method inapplicable-methods)
			  (find (meth~inference method) failed-methods-with-sole-setting)
			  (= 0 (meth~rating method))
			  (meth~supermethod-p method)))
		     relevant-methods)
	  '> :key 'meth~rating)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Functions Concerning The Control Rule Interpreter}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=update-support-choice (supp-choice-1 supp-choice-2)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "Two  support choices.")
	   (effect  "None.")
	   (value   "A new support choice which represents both input choices."))
  (cond ((not supp-choice-1) supp-choice-2)
	((not supp-choice-2) supp-choice-2)
	(t (cri~select supp-choice-1 supp-choice-2))))

(defun plan~limited-control-rule-p (rule)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A control rule.")
	   (effect  "None.")
	   (value   "True iff the control rule can also applied in limited mode."))
  (every #'(lambda (action) (and (not (eq (first action) 'select)) (not (eq (first action) 'insert)) (not (eq (first action) 'choose))))
	 (cri~to-do-part (cri~find-control-rule rule))))

(defun plan~cri-call (alternative-list &key ((:kind kind))
				       ((:pds pds) omega*current-proof-plan)
				       ((:tasks tasks))
				       ((:agenda agenda) (pds~agenda pds))
				       ((:task task))
				       ((:task-node task-node))
				       ((:task-formula task-formula))
				       ((:method method))
				       ((:crules crules))
				       ((:mode mode))
				       )
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "Arguments as seen at the cri, but also a mode key for the limited mode.")
	   (effect  "None.")
	   (value   "The result of the cri, evtl. invoked in limited mode."))
  (cond ((null mode)
					;	   (plan~trace "Using control rules: ~A" crules)
	 (cri~call alternative-list
		   :kind kind
		   :pds pds
		   :tasks tasks
		   :agenda agenda
		   :task task
		   :task-node task-node
		   :task-formula task-formula
		   :method method
		   :crules crules))
	((eq mode 'limited)
	 (let ((limited-crules (remove-if-not 'plan~limited-control-rule-p crules)))
					;	     (plan~trace "Using control rules: ~A" limited-crules)
	   (cri~call alternative-list
		     :kind kind
		     :pds pds
		     :tasks tasks
		     :agenda agenda
		     :task task
		     :task-node task-node
		     :task-formula task-formula
		     :method method
		     :crules limited-crules)))
	(t (omega~error "Unknown mode for calling cri."))))

(defun plan~strat-crules (task)
  (if (strat~task-default-strat-p task) plan*control-rules (strat~task-control-rules task)))

(defun plan~crules-for (task ref accessor)
  (let* ((method (if (strat~p ref) (strat~ref ref) ref))
	 (srules (strat~task-control-rules task))
	 (mrules (funcall accessor method)))
    (if (and srules (intersection mrules srules))
	(remove-if-not #'(lambda (rule) (find rule mrules)) srules)
      mrules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Planning Algorithm Itself}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Some Helper Functions}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#{
The planning algorithm performs a depth first search on the tree of all possible matchings
of all possible refinements of all possible tasks. The children of the nodes in this tree
are tried to order by invoking the control rule interpreter. The following function tries
the different children in this order until a consistent matching has been found. The function
is a higher order one and is used at every branching in the depth first search.
#}

(defun plan=try-set (set choose-sub-choice &key ((:accu-func accu-func)) ((:accu-init accu-init)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A list of choices, a function to be called with every choice,"
		    "until a choice succeeds.")
	   (effect  "Depends of the effects of the called function.")
	   (value   "The value of the first successful function call,"
		    "nil otherwise."))
  (if set
      (let ((first-choice (first set)))
	(multiple-value-bind (choosen-sub-setting val)
	    (funcall choose-sub-choice first-choice)
	  (let ((accu (when accu-func (funcall accu-func accu-init val))))
	    (if choosen-sub-setting
		(values choosen-sub-setting accu)
	      (plan=try-set (rest set) choose-sub-choice :accu-func accu-func :accu-init accu)))))
    (values nil accu-init)))

(defun plan=set (user-choice set-planner-func &rest args)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A user choices, a function to be called to determine the set to be searched and its required arguments.")
	   (effect  "None.")
	   (value   "A list of choices, containing the user choice, if it is given, or the conflict set determined by the planner."))
  (if user-choice
      (list user-choice)
    (apply set-planner-func args)))

(defun plan=update-loui (pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A pds.")
	   (effect  "Some function calls for loui.")
	   (value   "Undefined."))
  (when (plan~loui-interface-p)              ;; update loui-agenda stuff
    (socket~write "update-agenda")
    (parse~agenda (lagenda~for-loui (pds~agenda pds)))
    (socket~write "update-prooftree")
    (parse~proof pds)))

(defun plan=loui-ready ()
  (when (plan~loui-interface-p) (socket~write "ready")))

(defun plan=proof-planned (pds)
  ;; LOUI aus seiner Schleife befreien!
  (plan=loui-ready)
  (plan~message "The current pds ~A is planned!~%" pds)
  (plan~show-data)
  (plan~instantiate-metavars pds))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Search For A Planning Step}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
The commands.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan~plan (&optional (pds omega*current-proof-plan) (modus plan*current-modus) (agenda (pds~agenda pds)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "Optinal a pds a plan modus (not used) and an agenda.")
	   (effect  "Tries to plan the agenda.")
	   (value   "Undefined."))
  (if (lagenda~empty-p agenda)
      (plan=proof-planned pds)
    (let ((bounds (plan~modus-bounds modus)))
      (plan=automatically (first bounds) pds agenda)))
  (plan=loui-ready))

(defun plan=automatically (max-steps pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A maximal number of steps, a pds and optionally an agenda.")
	   (effect  "Tries to plan the agenda within the number of steps.")
	   (value   "Undefined."))
  (if (and (not (lagenda~empty-p agenda)) (or (null max-steps) (not (zerop max-steps))))
      (progn 
	(let ((success (plan~step-plan pds agenda)))
	  (if (lagenda~empty-p (pds~agenda pds))
	      (format t "~%AGENDA AFTER: Empty Agenda!")
	    (format t "~%AGENDA AFTER:~%~A~%~%" (oc=show-the-agenda (pds~agenda pds))))
	  (when success
	    (plan=automatically (- max-steps 1) pds))))
    (if (zerop max-steps)
	(plan~message ";;; The allowed planning steps are used up and the current pds ~A is not yet planned!"
		      pds))))
    
(defun plan~step-plan (&optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "Optional a pds and an agenda.")
	   (effect  "Does a planning step on the agenda."
		    "Updates loui.")
	   (value   "Undefined."))
  (if (lagenda~empty-p agenda)
      (progn
	(plan=loui-ready)
	(plan~message "The current agenda is empty!"))
    (let ((success (plan=do-step pds agenda)))
      (plan=update-loui pds)
      (if success
	  (if (lagenda~empty-p (pds~agenda pds))
	      (plan=proof-planned pds)
	    (plan=loui-ready))
	(progn
	  ;; LOUI aus seiner Schleife befreien!
	  (plan=loui-ready)
	  (plan~message "No proof plan can be found for the current pds ~A!" pds)))
      success)))

(defun plan~step-plan-with (refh goalh supportsh paramsh &optional (pds omega*current-proof-plan))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement symbol, a goal symbol, a list of support symbols, a list of parameter symbols and optional a pds.")
	   (effect  "Does a planning step with the given arguments."
		    "Updates loui.")
	   (value   "Undefined."))
  (let* ((ref (ref~find-ref refh))
	 (goal (when goalh (pds~label2node goalh)))
	 (supps (when supportsh (mapcar #'pds~label2node supportsh)))
	 (task (when goal (plan~find-task-for-goal-in-agenda goal (pds~agenda pds))))
	 (params (mapcar #'(lambda (par)
			     (cond ((and (symbolp par) (pds~label2node par)) (pds~label2node par))
				   ((and (listp par) (or (eq (first par) :term) (eq (first par) 'term)))
				    (post~read-object (second par) (pds~environment pds) :existing-term))
				   ((listp par) (pos~list-position par))
				   (t par)))
			 paramsh)))
    (plan~trace "================================================================================================")
    (plan~trace "=")
    (plan~trace "= Trying user selection...")
    (plan~trace "")
    (plan=choose-task pds (pds~agenda pds) :user-task task :user-ref ref :user-params params
		      :user-supps supps))
    (if (lagenda~empty-p (pds~agenda pds))
	(plan=proof-planned pds)
      (plan=loui-ready)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
First a task must be selected.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=do-step (pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A pds and optionally an agenda.")
	   (effect  "Does a planning step on the agenda.")
	   (value   "Undefined."))
  (plan~trace "================================================================================================")
  (plan~trace "=")
  (plan~trace "= Now planning a step...")
  (plan~trace "")
  (multiple-value-bind (success task-to-backtrack)
      (plan=choose-task pds agenda)
    (if success
	t
      ;; the tasks in the agenda could not be solved
      ;; backtrack the latest of the influencing tasks
      (progn
	(plan~trace "None of the tasks could be solved. Therefore...")
	(plan~trace "")
	(back~track-task task-to-backtrack agenda pds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
Then a task has to be solved depending on whether it is a pseudo or a goal task.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=task-set-planner (pds agenda)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A pds and an agenda.")
	   (effect  "None.")
	   (value   "Determines the conflict set for tasks."))
  (let* ((strat-tasks1 (strat~strategic-conflict-tasks (lagenda~tasks agenda) pds))
	 (first-tasks (lagenda~tasks-to-consider strat-tasks1 (lagenda~first-orderings agenda)
						 (lagenda~last-orderings agenda)
						 (lagenda~orderings agenda)))
	 (strat-tasks first-tasks))
    (if (strat~task-default-strat-p (first strat-tasks))
	(progn
	  (plan~trace "There are no strategic tasks: ~A" strat-tasks)
	  (plan~trace "Using the default control rules:"))
      (progn 
	(plan~trace "Latest strategic tasks: ~A" strat-tasks)
	(plan~trace "Using the control rules from the latest strategic refinement:")))
    (plan~trace "~A" (plan~strat-crules (first strat-tasks)))
    (let ((tasks (if (> (length first-tasks) 1)
		     (progn 
		       (plan~trace "Choice point: Task selection")
		       (plan~cri-call strat-tasks :kind 'tasks :tasks first-tasks
				      ;; all first tasks have the same control-link
				      :crules (plan~strat-crules (first strat-tasks))))
		   (progn
		     (plan~trace "There is only one task, invoking cri not neccessary")
		     strat-tasks))))
      (when tasks
	(cons (first tasks) (remove-if-not #'(lambda (task) (plan=interact-p task (first tasks) pds))
					   (rest tasks)))))))

(defun plan=choose-task (pds agenda &key ((:user-task user-task)) ((:user-ref user-ref))
			     ((:user-params user-params)) ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A pds, an agenda and the following key arguments: a task, a refinement, a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "Evtl. changes pds and agenda.")
	   (value   "A tuple: First a boolean value indicating success."
		    "Second the task to backtrack, if not successful."))
  (let ((interacting-tasks (plan=set user-task 'plan=task-set-planner pds agenda)))
    (plan~trace "Interacting tasks ~A" interacting-tasks)
    (let ((appl (plan=try-set interacting-tasks
				   #'(lambda (task-choice)
				       (plan=try-task task-choice agenda pds
						      :user-ref user-ref :user-params user-params
						      :user-supps user-supps)))))
      (if appl
	  (progn
	    (pds~agenda pds)
	    t)
	(values nil (cri-parser~task (first interacting-tasks)))))))

(defun plan=interact-p (task-choice1 task-choice2 pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "Two task choices and a pds.")
	   (effect  "None.")
	   (value   "True, iff the two tasks interact through meta variables."))
  (let ((task1 (cri-parser~task task-choice1))
	(task2 (cri-parser~task task-choice2)))
    (or (intersection (pdsh~metavars-of task1) (pdsh~metavars-of task2))
	(some #'(lambda (mor-scope) (and (intersection (pdsh~metavars-of task1) (pdsh~domain mor-scope))
					 (intersection (pdsh~metavars-of task2) (pdsh~domain mor-scope))))
	      (pds~constraint-store pds)))))

(defun plan=try-task (task-choice agenda pds &key ((:user-ref user-ref)) ((:user-params user-params))
				  ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice, an agenda, a pds and the following key arguments: a refinement, a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "Evtl. changes the pds and agenda.")
	   (value   "The planning step if task can be solved,"
		    "nil otherwise."))
  (declare (ignore agenda))
  (plan~trace "  Trying ~A..." (cri-parser~task task-choice) :ref plan*watch-ref)
  (let ((step (plan=task-solvable (cri-parser~task task-choice) task-choice pds :user-ref user-ref :user-params user-params
				  :user-supps user-supps)))
    (when step
      ;; a method matching was found to solve the task
      (let ((step-rsn (plan~execute-step step pds)))
	(unless (plan~proper-p step)
	  (plan=insert-new-tasks step-rsn step (cri-parser~task task-choice) pds)))
      step)))

(defun plan=insert-new-tasks (appl-rsn appl task pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application step, a task and a pds.")
	   (effect  "Inserts new tasks in the agenda.")
	   (value   "Undefined."))
  ;; insert new tasks in agenda
  (let* ((subgoals (plan~step-agenda-tasks appl))
	 (new-tasks (mapcar 'lagenda~create-task subgoals))
	 (orderings (plan=create-agenda-orderings (meth~outline-orderings (plan~step-method appl))
						  (plan~step-setting appl))))
    (if (plan~appl-forward-p appl)
	(progn 
	  (strat~update-control-link (list task) :reason appl-rsn)
	  (plan~trace "|| Inserting new tasks ~A at ~A" subgoals task)
	  (when new-tasks 
	    (lagenda~add-tasks! (pds~agenda pds) task new-tasks))
	  (dolist (ordering orderings)
		  (setf (lagenda~ordering-step ordering) appl-rsn)
		  (lagenda~add-ordering! (pds~agenda pds) ordering)))
      (progn
	(plan~trace "|| Replacing ~A with new tasks for ~A" task subgoals)
	(when new-tasks 
	  (lagenda~add-tasks! (pds~agenda pds) task new-tasks))
	(dolist (ordering orderings)
		(setf (lagenda~ordering-step ordering) appl-rsn)
		(lagenda~add-ordering! (pds~agenda pds) ordering))
	(lagenda~delete-task! (pds~agenda pds) task)))
    ;; update control link
    (strat~update-control-link new-tasks :super-task task :reason appl-rsn)
    ;; update current formulas
    (when (plan~step-instantiating-p appl)
      (let ((mor-goals (pds~mor-goals pds)))
	(multiple-value-bind
	 (changed)
	 (pds~update-current-formulas (pds~mor-nodes mor-goals) (plan~step-cstr-state appl))
	 (when (plan~step-mor-scope appl)
	   (pdsh~ms-update-domain (plan~step-mor-scope appl)
				  (cstr~variables (pdsh~cs-constraint (plan~step-cstr-state appl)))))
	 (pds~adapt-control-infos mor-goals changed))))
    ))

(defun plan=create-agenda-orderings (meth-orderings mmapp)
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "Method orderings, a binding of the method meta-variables,"
		    "and a list of tasks which can be referenced by the"
		    "METH-ORDERINGS.")
	   (effect  "Signals an error when the method orderings are inconsistent.")
	   (value   "A list of agenda orderings when"
		    "the method orderings are consistent."))
  (when meth-orderings
    (let* ((agenda-orderings (plan=create-agenda-orderings (rest meth-orderings) mmapp))
	   (meth-ordering1 (first meth-orderings))
	   (key-word (keim~name meth-ordering1))
	   (meth-nodes (meth~ordering-ordered-nodes meth-ordering1)))
      (cond ((string-equal key-word 'first)
	     (let* ((meth-var (if (meth~node-p (first meth-nodes)) (keim~name (first meth-nodes))
				(first meth-nodes)))
		    (pds-node (meth~mapp-get-component meth-var mmapp :mapp)))
	       (if pds-node
		   (cons (lagenda~create-ordering (if (listp pds-node) pds-node (list pds-node))
						  nil)
			 agenda-orderings)
		 (return-from plan=create-agenda-orderings
			      (omega~error ";;; No associated pds node for ~A in ~A" (first meth-nodes) mmapp)))))
	    ((string-equal key-word 'before)
	     (let* ((meth-vars (mapcar #'(lambda (mnode)
					   (if (meth~node-p mnode)
					       (keim~name mnode)
					     (if (listp mnode)
						 (mapcar #'(lambda (mn)
							     (if (meth~node-p mn) (keim~name mn) mn))
							 mnode)
					       mnode)))
				       meth-nodes))
		    (pds-nodes (mapcar #'(lambda (meth-var)
					   (meth~mapp-get-component meth-var mmapp :mapp))
				       meth-vars)))
	       (if (some #'null pds-nodes)
		   (return-from plan=create-agenda-orderings
				(omega~error ";;; No associated pds nodes for ~A in ~A" meth-nodes mmapp))
		 (cons (lagenda~create-ordering (if (listp (first pds-nodes)) (first pds-nodes) (list (first pds-nodes)))
						(if (listp (second pds-nodes)) (second pds-nodes) (list (second pds-nodes))))
		       agenda-orderings))))
	    (T
	     (return-from plan=create-agenda-orderings
			  (omega~error ";;; Unkown ordering key word ~A!" key-word))))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
A refinement must be selected for a goal task. 
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=ref-set-planner (task-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice.")
	   (effect  "None.")
	   (value   "Determines the conflict set for refinements."))
  (let* ((task-ref-set (cri-parser~task-ref-set task-choice))
	 (task (cri-parser~task task-choice)))
    (if task-ref-set
	;; task has additional control information about refinements
	(let* ((failed-methods-with-sole-setting (plan=failed-methods-with-sole-setting task))
	       ;; (inapplicable-methods (plan=inapplicable-methods task))
	       (task-ref-set-without-failed
		(remove-if #'(lambda (ref-choice)
			       (or
				(find (cri-parser~ref ref-choice) failed-methods-with-sole-setting)
				;; (find (cri-parser~ref ref-choice) inapplicable-methods)))
				))
			   task-ref-set))
	       (refs (mapcar 'cri-parser~ref task-ref-set)))
	  (plan~trace "  There are associated refinements with task" :ref refs)
	  (plan~trace "  The refinement ~A was proposed by a control rule of kind task." plan*watch-ref :ref refs)
	  ;; call cri in limited mode
	  (plan~cri-call task-ref-set-without-failed :kind 'methods :mode 'limited
			 :task-node (lagenda~task-node task)
			 :crules (plan~strat-crules task)))
      ;; task has no additional control information about refinements
      (let ((alternative-methods (plan=alternative-methods task)))
	(plan~trace "  Refinements are taken from alternative methods" :ref alternative-methods)
	(plan~trace "  The alternative methods contain the refinement ~A" plan*watch-ref :ref alternative-methods)
	(mapcar 'ref~find-ref (plan~cri-call (mapcar 'keim~name alternative-methods) :kind 'methods
						 :task-node (lagenda~task-node task)
						 :crules (plan~strat-crules task)))))))
  
(defgeneric plan=task-solvable (task task-choice pds &key ((:user-ref user-ref)) ((:user-params user-params))
				     ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task, a task choice, a pds and the following key arguments: a refinement, a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The planning step, if the task is solvable,"
		    "nil otherwise."))
  (:method ((task lagenda+task) task-choice pds &key ((:user-ref user-ref)) ((:user-params user-params))
	    ((:user-supps user-supps)))
	   (plan~trace "  Choice point: Refinement selection")
	   (setf plan*g-inapplicable-methods nil)
	   (setf plan*s-inapplicable-methods nil)
	   (let ((ref-set (plan=set user-ref 'plan=ref-set-planner task-choice)))
	     (plan~trace "  Alternative refinements: ~A" ref-set)
	     (plan~trace "  The alternative refinements contain the refinement ~A" plan*watch-ref
			 :ref (mapcar 'cri-parser~ref ref-set))
	     (let ((result (plan=try-set ref-set #'(lambda (ref-choice) (plan=apply-refinement task-choice ref-choice pds
											       :user-params user-params
											       :user-supps user-supps)))))
	       (plan=update-inapplicable-methods (lagenda~task-node task))
	       result)))
  (:method ((task lagenda+pseudo-goal) task-choice pds &key ((:user-ref user-ref)) ((:user-params user-params))
	    ((:user-supps user-supps)))
	   (declare (ignore user-supps user-params user-ref))
	   (let ((step (lagenda~pseudo-goal-step task)))
	     (if (and step (pdsh~pr-reason-p step))
		 ;; execution of a proper refinement
		 (let* ((ref (pdsh~pr-refinement step))
			(exec-params (ref~refinement-executable-p ref task)))
		   (when exec-params
		     (plan~proper-create task ref)))
	       ;; expansion of a method
	       (let* ((exp (plan~exp-create (lagenda~task-node (cri-parser~task task-choice))))
		      (ref (plan~step-method exp))
		      (setting (plan=exp-apply-new-bindings exp))
		      (exp-cond (meth~expansion-condition ref))
		      (settings (if exp-cond
				    (meth=check-condition exp-cond setting)
				  setting))
		      (new-setting (if (listp settings)
				       (progn
					 (when (second settings)				 
					   (omega~warning "Ignoring all mappings of the expansion condition of ~A but the first." ref))
					 (first settings))
				     settings)))
		 (plan~trace "  Checking expansion condition...")
		 (if (meth~mapp-constraint new-setting)
		     (progn
		       (plan~step-update-condition exp new-setting)
		       (plan~trace "  Subst:      ~A" (meth~mapp-subst new-setting))
		       (plan~trace "  Mapp:       ~A" (meth~mapp-mapp new-setting))
		       (plan~trace "  Constraint: ~A" (meth~mapp-constraint new-setting))
		       (if (plan~step-update-mor-scope exp pds)
			   ;; the new constraints are consistent with the pds constraint state
			   (progn
			     (plan~trace "  Refinement can definitly be expanded!")
			     (plan~trace "")
			     exp)
			 (progn
			   (plan~trace "  ...failed (new constraints are inconsistent with the pds constraint store)")
			   nil)))
		   (progn
		     (plan~trace "  ...failed")
		     nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
The selected refinement must be checked for application in 4 steps.
The goal of the refinement must be matched, if it exists.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=apply-refinement (task-choice ref-choice pds &key ((:user-params user-params))
					  ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A goal task choice, a refinement choice, a pds and the following key arguments: a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement,"
		    "nil otherwise."))
  (let* ((task (cri-parser~task task-choice))
	 (ref (cri-parser~ref ref-choice)))
    (if (ref~refinement-p ref)
	(plan=choose-proper-params (plan~proper-create task ref) ref-choice pds)
      (let* ((failed-steps-with-inference (pdsh~fs-with-inference (lagenda~task-node task) (meth~inference ref)))
	     (appl (plan~appl-create ref (lagenda~task-node task))))
	(plan~trace "    Trying ~A..." ref :ref ref)
	;; for supermethods condition check
	(setf meth*current-method ref)
	(plan~trace "    Matching focus goal..." :ref ref)
	(multiple-value-bind (success new-settings relevant-fs)
	    (plan=match-goal appl (list (meth~create-empty-mmapp)) failed-steps-with-inference task)
	  (if success
	      ;; goal matching successful, if nessessary
	      (progn 
		(plan~trace "    ...success" :ref ref)
		(plan=choose-params appl new-settings relevant-fs task-choice ref-choice pds :user-params user-params
				    :user-supps user-supps))
	    (progn
	      (plan~trace "    ...failed" :ref ref)
	      nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
Parameters for the refinement must be choosen.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=ref-param-set-planner (ref-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement choice.")
	   (effect  "None.")
	   (value   "Determines the conflict set for parameter tuples proposed by additional control information."))
  (let* ((param+supp-list (cri-parser~ref-param+supp-list ref-choice)))
    (if param+supp-list
	;; some parameters are proposed by the refinement choice and have to be used
	(progn 
	  (plan~trace "    There are parameters associated with the refinement" :ref (cri-parser~ref ref-choice))
	  param+supp-list)
      ;; no parameters are proposed by the refinement choice
      (list nil))))
    
(defun plan=choose-params (appl settings failed-steps task-choice ref-choice pds &key ((:user-params user-params))
				     ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task choice, a refinement choice, a pds and the following key arguments: a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement,"
		    "nil otherwise."))
  (let ((ref (cri-parser~ref ref-choice)))
    (if (not (meth~parameters ref))
	;; refinement needs no parameters
	(progn
	  (plan~trace "    Refinement needs no parameters" :ref ref)
	  (plan=try-params appl settings failed-steps task-choice ref-choice nil nil pds :user-supps user-supps))
      ;; refinement needs parameters
      (let ((param+supp-list (plan=ref-param-set-planner ref-choice)))
	(plan~trace "    Refinement needs parameters" :ref ref)
	(plan~trace "    Alternative refinement parameters: ~A" param+supp-list :ref ref)
	(plan=try-set param+supp-list
		      #'(lambda (ref-param+supp)
			  ;; note: appl has to be copied because this is a backtracking point
			  ;; and the failed steps depend on the branch in the dfs tree
			  (plan=try-ref-params appl settings failed-steps task-choice
					       ref-choice ref-param+supp pds :user-params user-params
					       :user-supps user-supps)))))))
  
(defun plan=param-set-planner (task method)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task node and a refinement.")
	   (effect  "None.")
	   (value   "Determines the conflict set for parameter tuples."))
  ;; es muss noch ein Teilmatching an cri uebergeben werden
  (let ((add-param-set (plan~cri-call nil :kind 'parameters :method method :task-node (lagenda~task-node task)
				      :crules (plan~crules-for task method 'cri~parameter-control-rules-for))))
    (if add-param-set
	;; additional parameters are proposed by the cri and have to be used
	(progn
	  (plan~trace "      Additional parameters are proposed by control rules." :ref method)
	  add-param-set)
      ;; no additional parameters are proposed, wait for application condition
      (progn
	(plan~trace "      No additional parameters are proposed by control rules." :ref method)
	(list nil)))))
	
(defun plan=try-ref-params (appl settings failed-steps task-choice ref-choice ref-param+supp pds &key ((:user-params user-params))
				      ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task choice, a refinement choice, parameters proposed by the refinement choice, a pds"
		    "and the following key arguments: a parameter tuple"
		    "and a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement and the refinement parameters,"
		    "nil otherwise."))
  (let ((ref (cri-parser~ref ref-choice)))
    (when ref-param+supp
      (plan~trace "      Trying (partial) parameters ~A..." (cri-parser~ref-param-tuple ref-param+supp) :ref ref))
    (if (< (cri-parser~ref-param-count ref-param+supp) (length (meth~parameters (cri-parser~ref ref-choice))))
	;; additional parameters are required
	(progn
	  (plan~trace "      Additional parameters are required" :ref ref)
	  (let ((add-param-set (plan=set user-params 'plan=param-set-planner (cri-parser~task task-choice) (plan~step-method appl))))
	    (plan~trace "      Alternative parameter tuples: ~A" add-param-set :ref ref)
	    (plan=try-set add-param-set
			  #'(lambda (param-choice)
			      ;; note: appl has to be copied because this is a backtracking point
			      ;; and the failed steps depend on the branch in the dfs tree
			      (plan=try-params appl settings failed-steps task-choice ref-choice
					       ref-param+supp param-choice pds :user-supps user-supps)))))
      ;; no additional parameters required
      (progn 
	(plan~trace "      No additional parameters are required" :ref ref)
	(plan=try-params appl settings failed-steps task-choice ref-choice ref-param+supp nil pds :user-supps user-supps)))))
  
(defun plan=try-params (appl settings failed-steps task-choice ref-choice ref-param+supp param-choice pds &key ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task choice, a refinement choice, parameters proposed by the refinement choice,"
		    "a parameter choice, a pds and the following key arguments: a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement, the refinement parameters"
		    "and the parameters, nil otherwise."))
  (let* ((param-tuple (append (cri-parser~ref-param-tuple ref-param+supp)
			      (cri-parser~param-tuple param-choice)))
	 (ref (cri-parser~ref ref-choice)))
    (plan~trace "        Trying (partial) parameters ~A..." param-tuple :ref ref)
    (multiple-value-bind (success new-settings relevant-fs)
	(plan=bind-parameters appl settings failed-steps param-tuple)
      (if success
	  ;; parameter binding is consistent
	  (progn
	    (plan~trace "        ...success" :ref ref)
	    (plan=choose-supports appl new-settings relevant-fs task-choice ref-choice ref-param+supp param-choice pds :user-supps user-supps))
	(progn
	  (plan~trace "        ...failed" :ref ref)
	  nil)))))

(defun plan=proper-param-set-planner (task ref)
  (plan~cri-call nil :kind 'parameters :method ref :task-node (lagenda~task-node task)
		 :crules (plan~crules-for task ref 'cri~parameter-control-rules-for)))
  
(defun plan=choose-proper-params (proper ref-choice pds)
  (let ((ref-param-tuple-list (cri-parser~ref-param-tuple-list ref-choice)))
    (if ref-param-tuple-list
	;; there are parameter tuples proposed by the refinement choice
	(plan=try-set ref-param-tuple-list
		      #'(lambda (ref-param-tuple)
			  (plan=try-proper-params proper ref-param-tuple pds)))
      (if (ref~input-parameters (plan~proper-ref proper))
	  (plan=try-set (plan=proper-param-set-planner (plan~proper-task proper) (plan~proper-ref proper))
			#'(lambda (param-tuple)
			    (plan=try-proper-params proper param-tuple pds)))
	(plan=try-proper-params proper nil pds)))))

(defun plan=try-proper-params (proper param-tuple pds)
  (let ((params (ref~refinement-applicable-p (plan~proper-ref proper) param-tuple)))
    (when params
      (plan~proper-update-params proper params)
      proper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
Supports must be choosen.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=supp-set-planner (task-choice ref-choice ref-param+supp param-choice method)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice, a refinement choice, parameters proposed by the refinement, a parameter choice and a refinement.")
	   (effect  "None.")
	   (value   "Determines the conflict set for support tuples."))
  (let* ((task (cri-parser~task task-choice))
	 (ref (cri-parser~ref ref-choice))
	 (task-supps (cri-parser~task-supps task-choice))
	 (ref-only-supp-choice (cri-parser~ref-supp-choice ref-choice))
	 (ref-param-supp-choice (cri-parser~ref-param-supp-choice ref-param+supp))
	 ;; ref-only-supp-choice and ref-param-supp-choice can not be both different from nil
	 (ref-supp-choice (if ref-only-supp-choice ref-only-supp-choice ref-param-supp-choice))
	 (param-supp-choice (cri-parser~param-supp-choice param-choice))
	 (supp-conflict-set
	  (if (and (not task-supps) (not ref-supp-choice) (not param-supp-choice))
	      ;; no additional control information about supports available, use node supports
	      ;; of task node
	      (pdsn~supports (lagenda~task-node task))
	    ;; additional control information about supports available, call cri in limited mode
	    (let* ((first-supps (if (and task-supps ref-supp-choice) (plan=update-support-choice task-supps ref-supp-choice)
				  (if task-supps task-supps ref-supp-choice))))
	      (if (and first-supps param-supp-choice) (plan=update-support-choice first-supps param-supp-choice)
		(if first-supps first-supps param-supp-choice)))))
	 (limited (or task-supps ref-supp-choice param-supp-choice)))
    (if limited
	(plan~trace "        Supports associated to task, refinement and parameters are used." :ref ref)
      (plan~trace "        No supports associated to task, refinement or parameters." :ref ref))
    (let* ((supp-choice (if limited
			    (plan~cri-call supp-conflict-set :kind 'supports :method method :task-node (lagenda~task-node task)
					   :task task
					   :mode 'limited :crules (plan~crules-for task method 'cri~support-control-rules-for))
			  (plan~cri-call supp-conflict-set :kind 'supports :method method :task-node (lagenda~task-node task)
					 :task task
					 :crules (plan~crules-for task method 'cri~support-control-rules-for))))
	   (tuples (if (not (cri-parser~supp-tuples supp-choice))
		       ;; supp-choice is a list of supps
		       (cri~n-tuples-from-sequence (length (meth~premises-to-match ref)) supp-choice)
		     ;; supp-choice is a set of supp-tuples
		     supp-choice)))
      (if (find ref (pdsn~s-inapplicable-methods (lagenda~task-node task)))
	  (progn 
	    (plan~trace "        Refinement ~A is s-inapplicable: Trying only tuples with at least one extra supports." ref :ref ref)
	    (remove-if-not #'(lambda (tuple) (some #'(lambda (support) (find support (pdsn~extra-supports (lagenda~task-node task))))
						   tuple))
			   tuples))
	tuples))))

(defun plan=choose-supports (appl settings failed-steps task-choice ref-choice ref-param+supp param-choice pds
				       &key ((:user-supps user-supps)))
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task choice, a refinement choice, parameters proposed by the refinement choice,"
		    "a parameter choice, a pds and the following key arguments: a support tuple as user selection.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement, the refinement parameters"
		    "and the parameters, nil otherwise."))
  (plan~trace "        Choice point: Support selection" :ref (cri-parser~ref ref-choice))
  (let* ((task (cri-parser~task task-choice))
	 (ref (cri-parser~ref ref-choice))
	 (supp-tuples (plan=set user-supps 'plan=supp-set-planner task-choice ref-choice ref-param+supp param-choice ref)))
    (plan~trace "        Alternative support tuples: ~A" supp-tuples :ref ref)
    (if supp-tuples
	;; there are possible support tuples
	(multiple-value-bind (try-result inapplicable)
	    (plan=try-set supp-tuples
			  #'(lambda (supp-tuple)
			      ;; note: appl has to be copied because this is a backtracking point
			      ;; and the failed steps depend on the branch in the dfs tree
			      (plan=try-supp-tuple appl settings failed-steps task supp-tuple pds))
			  :accu-func #'(lambda (a b) (and a b)) :accu-init t)
	  (when inapplicable
	    ;; the method is not applicable with any supp-tuple
	    ;; this is only a side effect
	    (plan=collect-inapplicable-methods ref task))
	  try-result)
      ;; there are no possible support tuples
      (progn 
	(plan~trace "        There are no possible support tuples." :ref ref)
	(plan=collect-inapplicable-methods ref task)
	nil))))

(defun plan=try-supp-tuple (appl settings failed-steps task supp-tuple pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task, a support tuple and a pds.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement, the refinement parameters,"
		    "the parameters and the support tuple, nil otherwise."))
  (plan~trace "          Trying support tuple ~A..." supp-tuple :ref (plan~step-method appl))
  (if (= (length (meth~premises-to-match (plan~step-method appl))) (length supp-tuple))
      (if (meth~premises-to-match (plan~step-method appl))
	  (multiple-value-bind (success new-settings relevant-fs)
	      (plan=match-supports appl settings failed-steps supp-tuple)
	    (if success
		;; supports could be matched to the used supports
		(progn
		  (plan~trace "          ...success" :ref (plan~step-method appl))
		  (plan=choose-setting appl new-settings relevant-fs task pds))
	      (progn
		(plan~trace "          ...failed" :ref (plan~step-method appl))
		(values nil t))))
	;; method needs no premises to match
	(progn
	  (plan~trace "          Refinement needs no premises to match" :ref (plan~step-method appl))
	  (plan=choose-setting appl settings failed-steps task pds)))
    (omega~error "The number of premises is not the same as the number of supports tried to match")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
A setting must be selected.
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plan=setting-set-planner (appl settings task)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application step and a list of settings.")
	   (effect  "None.")
	   (value   "Determines the conflict set for settings."))
  (let* ((ref (plan~step-method appl))
	 (appl-cond (meth~application-condition ref))
	 (mmapps (reduce 'append (mapcar #'(lambda (setting) (if appl-cond
								 (let ((new-settings (meth=check-condition appl-cond setting)))
								   (if (listp new-settings) new-settings (list new-settings)))
							       (list setting)))
					 settings)))
	 (setting-set (if (> (length mmapps) 1)
			  ;; if there are 2 or more settings, invoke cri
			  (plan~cri-call mmapps :kind 'setting :method ref :task-node (plan~step-focus-goal appl)
					 :crules (plan~crules-for task ref 'cri~setting-control-rules-for))
			mmapps)))
    ;; remove all settings which are inconsistent
    (remove-if-not 'meth~mapp-constraint setting-set)))
	   
(defun plan=choose-setting (appl settings failed-steps task pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps,"
		    "a task and a pds.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement, the refinement parameters,"
		    "the parameters and the support tuple, nil otherwise."))
  (plan~trace "          Checking application condition..." :ref (plan~step-method appl))
  (let* ((ref (plan~step-method appl))
	 ;;(goal (agenda~task-node task))
	 (setting-set (plan=setting-set-planner appl settings task)))
    (if setting-set
	;; application condition fullfilled
	(progn
	  (plan~trace "          ...the application condition is fullfilled." :ref ref)
	  (plan~trace "          Choice point: Setting selection" :ref ref)
	  (plan~trace "          Alternative settings: ~A" setting-set :ref ref)
	  (plan=try-set setting-set
			#'(lambda (setting) (plan=try-setting appl failed-steps setting pds))))
      ;; application condition not fulfilled, now test for permanently inapplicability
      (progn
	(plan~trace "          ...the application condition is not fullfilled." :ref ref)
	nil))))

(defun plan=try-setting (appl failed-steps setting pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An partial application step, a list of possible settings, a list of failed steps"
		    "and a pds.")
	   (effect  "None.")
	   (value   "The application planning step if task can be solved with the refinement, the refinement parameters,"
		    "the parameters, the support tuple and the setting, nil otherwise."))
  (plan~trace "            Trying setting..." :ref (plan~step-method appl))
  (plan~trace "            Subst:      ~A" (meth~mapp-subst setting) :ref (plan~step-method appl))
  (plan~trace "            Mapp:       ~A" (meth~mapp-mapp setting) :ref (plan~step-method appl))
  (plan~trace "            Constraint: ~A" (meth~mapp-constraint setting) :ref (plan~step-method appl))
  (plan~appl-update-condition appl setting)
  (multiple-value-bind (inadmissible)
      (plan=inadmissible-p appl failed-steps 'plan=relevant-wrt-setting-p)
    (if (not inadmissible)
	(progn
	  (plan~trace "            Refinement is admissible." :ref (plan~step-method appl))
	  (if (plan~step-update-mor-scope appl pds)
	      ;; the new constraints are consistent with the pds constraint state
	      (progn
		(plan~trace "            Refinement can definitly be applied!" :ref (plan~step-method appl))
		(plan~trace "" :ref (plan~step-method appl))
		appl)
	    (progn
	      (plan~trace "            ...failed (inconsistent new constraints)" :ref (plan~step-method appl))
	      nil)))
      (progn
	(plan~trace "            ...failed" :ref (plan~step-method appl))
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{The Execution Of A Planning Step}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric plan~execute-step (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Changes pds and agenda according to the planning step (redundant or not).")
	   (value   "The reason which represents this step."))
  (:method ((appl plan+application) pds)
	   (plan~message "----------------------------------------------------------------------------------------------------")
	   (plan~message "|")
	   (plan~message "| Applying ~A" (plan~step-method appl))
	   (plan~message "| with focus goal ~A, parameters ~A,"
			 (plan~step-focus-goal appl) (plan~appl-parameters appl))
	   (plan~message "| existent premises ~A and closed premises ~A..."
			 (plan~appl-exist-prems appl) (plan~appl-clsed-prems appl))
	   (plan~trace "")
	   (let* ((previous-step (plan~appl-redundant-step-p appl pds))
		  (appl-rsn (if previous-step
				;; method is a redundant forward method
				(progn (plan=process-redundant-application appl pds previous-step)
				       previous-step)
			      ;; method application is not redundant
			      (plan=process-new-step appl pds))))
	     (plan~trace "")
	     (plan~message "| ...application of ~A complete." (plan~step-method appl))
	     (plan~message "|")
	     (plan~message "----------------------------------------------------------------------------------------------------")
	     (plan~trace "||")
	     appl-rsn))
  (:method ((exp plan+expansion) pds)
	   (plan~message "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
	   (plan~message "+")
	   (plan~message "+ Expanding ~A justifying ~A" (plan~step-method exp) (plan~step-focus-goal exp))
	   (plan~trace "")
	   (let ((exp-rsn (plan=process-new-step exp pds)))
	     (plan~trace "")
	     (plan~message "+ ...expansion of ~A complete." (plan~step-method exp))
	     (plan~message "+")
	     (plan~message "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
	     (plan~trace "||")
	     exp-rsn))
  (:method ((proper plan+proper) pds)
	   (let ((ref (plan~proper-ref proper))
		 (task (plan~proper-task proper)))
	     (plan~message "####################################################################################################")
	     (plan~message "#")
	     (multiple-value-bind (new-nodes focus-task new-tasks agenda-orderings reason nodes)
		 (if (lagenda~pseudo-goal-p task)
		     ;; execution
		     (let ((params (plan~proper-exec-params proper)))
		       (plan~message "# Executing proper refinement ~A to ~A~%# with parameters ~A" (keim~name ref) task params)
		       (ref~refinement-execute ref task params))
		   ;; application
		   (let ((params (plan~proper-params proper)))
		     (plan~message "# Applying proper refinement ~A to ~A~%# with parameters ~A" (keim~name ref) task params)
		     (ref~refinement-apply ref task params)))
	       (let ((mor-scope (pdsh~reason-mor-effect reason)))
		 (setf (plan~step-mor-scope proper) mor-scope)
		 (plan~step-update-new-nodes proper new-nodes pds)
		 (dolist (new-node new-nodes)
		   (pds~insert-node! new-node pds))
                 ;;; Updating the pds-agenda
                 (when new-tasks 
		   (lagenda~add-tasks! (pds~agenda pds) task new-tasks))
                 (dolist (ordering agenda-orderings)
		   (lagenda~add-ordering! (pds~agenda pds) ordering))
                 (when focus-task 
		   (lagenda~delete-task! (pds~agenda pds) focus-task))
                 ;; mor-scope
                 (when mor-scope
		   (pds~insert-mor-scope mor-scope (plan~step-new-pure-metavars step) pds))
		 (when (strat~p ref)
		   ;; set control-link
		   (strat~update-control-link (lagenda~tasks-of (pds~agenda pds) nodes) :super-task focus-task :reason reason)))
	       (if (lagenda~pseudo-goal-p task)
		   (plan~message "# ...execution of ~A complete." (keim~name ref))
		 (plan~message "# ...application of ~A complete." (keim~name ref)))
	       (plan~message "#")
	       (plan~message "####################################################################################################")
	       reason)
	     )))

(defun plan=process-redundant-application (appl pds previous-step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "An application planning step, a pds and the previous redundant step.")
	   (effect  "Changes pds and agenda according to the planning step wrt. the redundant step.")
	   (value   "Undefined."))
  (plan~trace "The refinement application is a redundant step")
  (plan~trace "Redundant: ~A" previous-step)
  (let ((focus-goal (plan~step-focus-goal appl)))
    (setf (plan~step-conclusions appl) (pdsh~ia-conclusions previous-step))
    (setf (plan~step-premises appl) (pdsh~ia-premises previous-step))
    (plan~trace "Step 1: Update reason links")
    (plan~trace "        Adding ~A to secondary goals of redundant reason" focus-goal)
    (misc~append-to-slot (pdsh~ia-secondary-goals previous-step) focus-goal)
    (plan~trace "        Adding redundant reason to reason list of ~A" focus-goal)
    (misc~append-to-slot (pdsn~reasons focus-goal) (list previous-step))
    (plan~trace "Step 2: Change supports")
    (plan=change-supports appl pds)))

(defun plan=process-new-step (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Changes pds and agenda according to the planning step (not redundant).")
	   (value   "The reason which represents this step."))
  (plan~trace "Step 1: Evaluate computations")
  (plan=process-computations step)
  (plan~trace "Step 2: Create new nodes")
  (plan=create-new-nodes step pds)
  (plan~trace "Step 3: Create justifications for conclusions")
  (plan=create-justifications step)
  (plan~trace "Step 4: Insert the new nodes ~A into the pds" (plan~step-new-nodes step))
  (plan=insert-new-nodes-into-pds step pds)
  (plan~trace "Step 5: Change supports")
  (plan=change-supports step pds)
  (plan~trace "Step 6: Create reason")
  (let ((step-rsn (plan=create-reason step pds)))
    (plan~trace "Step 7: Evaluate outline actions")
    (plan=process-actions step pds)
    (plan~trace "Step 8: Insert MOR scope into the pds")
    (pds~insert-mor-scope (plan~step-mor-scope step) (plan~step-new-pure-metavars step) pds)
    step-rsn))

(defgeneric plan=process-computations (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "Executes the computations of the step.")
	   (value   "Undefined."))
  (:method ((appl plan+application))
	   (let ((constraint (meth~mapp-constraint (plan~step-setting appl))))
	     (meth~carry-out-computations (meth~outline-computations (plan~step-method appl))
					  (plan~step-setting appl))
	     ;; will be added to functionality later
	     ;; (plan~appl-update-mor-scope appl)
	     (let ((setting (plan~step-setting appl)))
	       (setf (meth~mapp-constraint setting) constraint)
	       (plan~appl-update-condition appl setting)
	       (plan~trace "        Subst:      ~A" (meth~mapp-subst setting))
	       (plan~trace "        Mapp:       ~A" (meth~mapp-mapp setting))
	       (plan~trace "        Constraint: ~A" (meth~mapp-constraint setting)))
	     ))
  (:method ((exp plan+expansion))
	   (let ((constraint (meth~mapp-constraint (plan~step-setting exp))))
	     (meth~carry-out-computations (meth~expansion-computations (plan~step-method exp))
					  (plan~step-setting exp))
	     (let ((setting (plan~step-setting exp)))
	       (setf (meth~mapp-constraint setting) constraint)
	       (plan~trace "        Subst:      ~A" (meth~mapp-subst setting))
	       (plan~trace "        Mapp:       ~A" (meth~mapp-mapp setting))
	       (plan~trace "        Constraint: ~A" (meth~mapp-constraint setting)))
	     )))

(defun plan=new-expansion-nodes (just existing-nodes)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A justification and a list of nodes.")
	   (effect  "None.")
	   (value   "All direct and indirect premises of the justification, that do not belong the the node list."))
  (let ((list-of-list 
	 (mapcar #'(lambda (prem)
		     (unless (find prem existing-nodes)
		       (union (list prem) (plan=new-expansion-nodes (node~justification prem) existing-nodes))))
		 (just~premises just))))
    (when list-of-list (reduce 'union list-of-list))))

(defgeneric plan=create-new-nodes (appl pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Creates the new nodes of the step and updates the step object.")
	   (value   "Undefined."))
  (:method ((appl plan+application) pds)
	   (let ((method (plan~step-method appl)))
	     (multiple-value-bind (concs prems new-nodes)
		 (meth~complete-outlines (meth~conclusions method) (meth~premises method)
					 (plan~step-setting appl) nil nil)
	       (setf (plan~step-conclusions appl) concs)
	       (setf (plan~step-premises appl) prems)
	       (plan~step-update-new-nodes appl new-nodes pds)))
	   (plan~trace "        add conclusions ~A created" (plan~appl-add-concs appl))
	   (plan~trace "        subgoals ~A created" (plan~appl-subgoals appl)))
  (:method ((exp plan+expansion) pds)
	   (let* ((meth-concs (meth~conclusions (plan~step-method exp)))
		  (concs))
	     (dolist (meth-conc meth-concs)
	       (let ((node (meth~object-instance meth-conc (plan~step-setting exp)))
		     (just (meth~object-instance (node~justification meth-conc) (plan~step-setting exp))))
		 (setf concs (union concs (list node)))
		 (setf (pdsj~status (node~justification node)) "expanded")
		 (setf (pdsj~control just) (pdsj~control (node~justification node)))
		 (setf (pdsj~above just) (node~justification node))
		 (setf (pdsj~below (node~justification node)) just)
		 (pdsj~replace-justification! (node~justification node) just)
		 (setf (node~justification node) just)
		 (plan~step-update-new-nodes exp
					     (plan=new-expansion-nodes just (plan~step-premises exp))
					     pds)
		 (plan~trace "        expansion nodes ~A created" (plan~step-new-nodes exp))))
	     (setf (plan~step-conclusions exp) concs)
	     (plan=exp-update-hyps exp))))

(defgeneric plan=create-justifications (step)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "Creates the justifications for the step.")
	   (value   "Undefined."))
  (:method ((appl plan+application))
	   (dolist (conc (plan~step-conclusions appl))
	     (let ((just (pdsj~closed-just-create (meth~inference (plan~step-method appl))
						  (plan~step-premises appl)
						  (plan~appl-parameters appl)
						  "unexpanded"
						  (plan~step-setting appl))))
	       ;; vererbe control information
	       ;; spaeter nicht mehr noetig
	       (plan~trace "        ~A" just)
	       (when (node~justification conc)
		 (setf (pdsj~control just) (pdsj~control (node~justification conc))))
	       (setf (node~justification conc) just))))
  (:method ((exp plan+expansion))
	   ))

(defun plan=insert-new-nodes-into-pds (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Inserts the new nodes of the step into the pds.")
	   (value   "Undefined."))
  (dolist (new-node (plan~step-new-nodes step))
    (pds~insert-node! new-node pds)))

(defgeneric plan=change-supports (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Changes the supports of the focus goal and the subgoals.")
	   (value   "Undefined."))
  (:method ((appl plan+application) pds)
	   (declare (ignore pds))
	   ;; the subgoals have to be processed before the secondary goal because the original supports of the secondary goal
	   ;; are needed to compute the inherited supports
	   (let ((inherited-methods (remove-if #'meth~goal (pdsn~s-inapplicable-methods (plan~step-focus-goal appl))))
		 (inherited-normal-supports (plan~appl-inherited-normal-supports appl))
		 (inherited-extra-supports (plan~appl-inherited-extra-supports appl)))
	     (dolist (subgoal (plan~appl-subgoals appl))
	       (plan~trace "        Updating the supports of the subgoal ~A:" subgoal)
	       (let ((subgoal-extra-supports (if inherited-methods
						 (union inherited-extra-supports
							(plan~appl-extra-hypotheses appl subgoal))
					       nil))
		     (subgoal-normal-supports (if inherited-methods
						  inherited-normal-supports
						(union
						 (plan~appl-extra-hypotheses appl subgoal)
						 (union inherited-normal-supports inherited-extra-supports)))))
		 (plan~trace "        Setting supports to ~A" subgoal-normal-supports)
		 (plan~trace "        Setting extra supports to ~A" subgoal-extra-supports)
		 (plan~trace "        Setting s-inapplicable methods to ~A" inherited-methods)
		 (setf (pdsn~normal-supports subgoal) subgoal-normal-supports)
		 (setf (pdsn~extra-supports subgoal) subgoal-extra-supports)
		 (setf (pdsn~s-inapplicable-methods subgoal) inherited-methods))))
	   (when (plan~appl-forward-p appl)
	     ;; there is a secondary goal
	     (plan~trace "        Updating the supports of the focus goal (forward method):")
	     (let ((secondary-goal (first (plan~appl-secondary-goals appl))))
	       (plan~trace "        Removing ~A" (plan~appl-clsed-prems appl))
	       (pdsn~remove-supports secondary-goal (plan~appl-clsed-prems appl))
	       (plan~trace "        Adding ~A" (plan~appl-add-concs appl))
	       (pdsn~add-new-supports secondary-goal (plan~appl-add-concs appl)))))
  (:method ((exp plan+expansion) pds)
	   (declare (ignore pds))
	   ))

(defun plan=create-reason (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step and a pds.")
	   (effect  "Creates a reason for the step and inserts it in the history and the nodes.")
	   (value   "The created reason."))
  (let ((new-rsn (plan~step-create-reason step pds)))
    (plan~trace "        New reason: ~A" new-rsn)
    (dolist (node (plan~step-all-nodes step)) 
      (plan~trace "        Adding reason to node ~A." node)
      (misc~append-to-slot (pdsn~reasons node) new-rsn))
    new-rsn))

(defun plan=process-actions (step pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A planning step.")
	   (effect  "Executes the actions of the step.")
	   (value   "Undefined."))
  ;; only sponsor and unsponsor actions are implemented
  ;; for other actions the sym2action table in l-method.lisp has to be extended
  ;; for node creating actions the reasons have to be given to meth~execute-action (5th argument)
  (let ((method (plan~step-method step)))
    (dolist (action (meth~outline-actions method))
      (meth~execute-action method action (plan~step-setting step) pds nil (pdsn~hyps (plan~step-focus-goal step))))))

;; Umdefinierte Funktionen

(defun oc=show-agenda ()
  (if (not (lagenda~empty-p (pds~agenda omega*current-proof-plan)))
      (omega~message (oc=show-the-agenda (pds~agenda omega*current-proof-plan)))
    (omega~message ";;;The current agenda is empty!")))

(defun oc=show-the-agenda (agenda)
  (format nil "~%tasks: ~{~A ~}~% orderings: ~{~A ~}"
	  (lagenda~tasks agenda)
	  (append (lagenda~first-orderings agenda)
		  (lagenda~orderings agenda)
		  (lagenda~last-orderings agenda))))


(defun cri~call (alternative-list &key ((:kind kind))
				  ((:pds pds) omega*current-proof-plan)
				  ((:tasks tasks))
				  ((:agenda agenda) (pds~agenda pds))
				  ((:task task))
				  ((:task-node task-node))
				  ((:task-formula task-formula))
				  ((:method method))
				  ((:crules crules))
				  ((:mmatchings mmatchings))
				  )
  (declare (edited  "07-FEB-1997")
	   (authors Cullrich)
	   (input   "An alternative list, a keyword kind, and optional a keyword pds")
	   (effect  "side-effects specified in the control-rules")
	   (value   "the changed alternative-list according to the contrul-rules of the"
		    "specified kind. The controle-rules use the pds given with the"
		    "'pds' keyword"))
;  (format t "~% task: ~S" task)
 ; (format t "~% tasks: ~S" tasks)

  (let* ((cri*current-original-alternative-list alternative-list)
	 (cri*used-control-rules-hashtable
	  (if crules (cri~set-used-control-rules crules)
	    cri*used-control-rules-hashtable))
	 (cri*current-tasks tasks)
	 (cri*current-agenda agenda)
	 (cri*current-pds pds)
;	 (cri*current-task (if task task
;                             (first tasks)))
	 (cri*current-task task)
	 (cri*current-task-node task-node)
	 (cri*current-task-formula task-formula)
	 (cri*current-method method)
	 (cri*applied-rules nil)
	 (cri*mmatchings mmatchings)
	 (cri*current-supports
	  (when cri*current-task
	    (pds~node-supports (lagenda~task-node cri*current-task))))
	 (rules (cri~used-control-rules-of-kind kind))
	 )
;    (format t "~% curren-supp :~S" cri*current-supports)
    (cri=mark-alternative-list alternative-list)
    (when cri*verbose (progn (omega~message "~% Rules: ~S" rules)
			     (omega~message "~% Incoming alternative list: ~S" alternative-list)))
    (let ((result 
	   (if rules
	       (cri=call rules alternative-list kind)
	     alternative-list)))
      (when cri*verbose (omega~message "~% Resulting alternative list: ~S" result))
      (values result
	      cri*applied-rules))
    )
  )


(defmethod meth~build-object (ordering (env env+environment) (indicator (eql :ordering)) &optional decl-cont)
  (let ((head (first ordering)))
    (if (find head meth*ordering-keywords :test #'string-equal)
	(make-instance 'meth+ordering
		       :name head
		       :ordered-nodes (mapcar #'(lambda (on)
						  (if (listp on)
						      (mapcar #'(lambda (on-elt)
								  (meth~build-object on-elt env :argument decl-cont))
							      on)
						    (meth~build-object on env :argument decl-cont)))
					      (rest ordering)))
      (omega~error "Unknown ordering with head symbol ~A" head))))

(defun show-goal (symbol)
  (let* ((node (if (pdsn~p symbol) symbol
		 (pds~label2node symbol omega*current-proof-plan)))
	 (task (find-if #'(lambda (ta)
			    (find node (lagenda~task-nodes ta)))
			(lagenda~tasks (pds~agenda omega*current-proof-plan)))))
    (if task
	(format t "~%Current formula of ~A: ~A" (keim~name node) (pds~task-formula task omega*current-proof-plan))
      (format t "~%No task for ~A." (keim~name node)))
    ))
