(in-package :omega)

;; Following Problem:
;; Up to now steps in the sense of plan steps have always been bound to nodes, i.e. the reason has been created with pds~change-last-plan-step!
;; respectively (pdsc~an-create node), the chaining of the steps has been done by the justifications!
;; Strategies get their own step mechanism -> see sod-control.lisp.
;; A strategy application should insert two steps in each these own strategy steps. One at the start of the strategy and one at the end!
;; 1.) We create a blackbox justification above start strategy at the start of a strategy and insert this step in the strategy steps.
;;     This justificationi contains the strategy-ks and the start-task as parameter.
;; 2.) We memorize the thereby created scon~strategy-step
;; 3.) An analogous blackboard justification is created at the end of a strategy and likewise inserted in the strategy steps.
;;     This justification contains the ROC and the memorized strat-step as parameter.

;; Fun things like the creation of own justifications for PPlanner applications for example (nice for abstraction and presentation)
;; should be done not until the very end! (See commandos -> make-strategic-justifications)

#| ------------------------------------------------------- New Parameter Types ------------------------------------------------------- |#

(eval-when (load compile eval)
  (arg~deftype strategy-ks
	       (read-function ddummy)
	       (predicate ot~strategy-ks-p)
	       (help "A strategy-ks."))
  )

(defun ot~strategy-ks-p (obj)
  (strat~strategy-ks-p obj))

(eval-when (load compile eval)
  (arg~deftype task
	       (read-function ddummy)
	       (predicate ot~task-p)
	       (help "A Task."))
  )

(defun ot~task-p (obj)
  (agenda~task-p obj))

(eval-when (load compile eval)
  (arg~deftype roc
	       (read-function ddummy)
	       (predicate ot~roc-p)
	       (help "A refinement operation call state description."))
  )

(defun ot~roc-p (obj)
  (roc~state-description-p obj))

(eval-when (load compile eval)
  (arg~deftype scon-step
	       (read-function ddummy)
	       (predicate ot~scon-step-p)
	       (help "A scon-step."))
  )

(defun ot~scon-step-p (obj)
  (scon~strategy-step-p obj))

#| ------------------------------------------------------ Starting a Strategy --------------------------------------------------------- |#

(infer~defbbox start-strategy-ks-application
	       (outline-function strat~start-strategy-ks-application-create-just)
	       (test-function )
	       (expansion-function )
	       (parameter-types roc)
	       (help "Start a strategy-ks-application"))


(defun strat~start-strategy-ks-application-create-just (parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "A list of parameters (indeed: the ROC of the strategy-application, the"
		    "new created just of this step should be used to create a step which is then"
		    "entered into the ROC as start step, see for example PPlanner or InstMeta).")
	   (effect  "None.")
	   (value   "A justification of the Blackb-box method start-strategy-ks-application."))
  (pdsj~closed-just-create (infer~find-method 'start-strategy-ks-application)
			   nil
			   parameters
			   "untested"))

#| --------------------------------------------------------------- Ending a strategy ------------------------------------------------- |#


(infer~defbbox end-strategy-ks-application
	       (outline-function strat~end-strategy-ks-application-create-just)
	       (test-function )
	       (expansion-function )
	       (parameter-types roc)
	       (help "End a strategy-ks-application"))

(defun strat~end-strategy-ks-application-create-just (parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "A list of parameters (indeed: the ROC of the strategy-application which contains as start-step"
		    "the scon-step of the start-justification).")
	   (effect  "None.")
	   (value   "A justification of the Blackb-box method end-strategy-ks-application."))
  (pdsj~closed-just-create (infer~find-method 'end-strategy-ks-application)
			   nil
			   parameters
			   "untested"))
#| -------------------------------------------------------- Interrupting  a strategy ------------------------------------------------- |#

(infer~defbbox interrupt-strategy-ks-application
	       (outline-function strat~interrupt-strategy-ks-application-create-just)
	       (test-function )
	       (expansion-function )
	       (parameter-types roc)
	       (help "Interrupt a strategy-ks-application"))

(defun strat~interrupt-strategy-ks-application-create-just (parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "A list of parameters (indeed: the ROC of the strategy-application which contains as start-step"
		    "the scon-step of the start-justification).")
	   (effect  "None.")
	   (value   "A justification of the Blackb-box method interrupt-strategy-ks-application."))
  (pdsj~closed-just-create (infer~find-method 'interrupt-strategy-ks-application)
			   nil
			   parameters
			   "untested"))

#| -------------------------------------------------------- Reinvoking a strategy ------------------------------------------------- |#

(infer~defbbox reinvoke-strategy-ks-application
	       (outline-function strat~reinvoke-strategy-ks-application-create-just)
	       (test-function )
	       (expansion-function )
	       (parameter-types roc)
	       (help "Reinvoke a strategy-ks-application"))

(defun strat~reinvoke-strategy-ks-application-create-just (parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "A list of parameters (indeed: the ROC of the strategy-application which contains as start-step"
		    "the scon-step of the start-justification).")
	   (effect  "None.")
	   (value   "A justification of the Blackb-box method reinvoke-strategy-ks-application."))
  (pdsj~closed-just-create (infer~find-method 'reinvoke-strategy-ks-application)
			   nil
			   parameters
			   "untested"))

#| ------------------------------------------------------ A PPLANNER STRATEGY JUSTIFICATION ------------------------------------------- |#

;; In addition to the above mentions general stuff made at strategy-ks applications, an additional justification is created at PPlanner,
;; that is annotated to the nodes of start-task to get a high level as possible justification!
;; compare to PPlanner

(infer~defbbox pplanner-strategy-ks-application
	       (outline-function strat~pplanner-strategy-ks-application-create-just)
	       (test-function )
	       (expansion-function strat~pplanner-strategy-ks-application-expansion)
	       (parameter-types symbol)
	       (help "End a strategy-ks-application"))

(defun strat~pplanner-strategy-ks-application-create-just (outline parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "An outline (the first is the closed node) and a list of parameters"
		    "(indeed: the name of the strategy-ks).")
	   (effect  "None.")
	   (value   "A justification of the Blackb-box method pplanner-strategy-ks-application."))
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'pplanner-strategy-ks-application)
					    (rest outline)
					    parameters
					    "unexpanded"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (strat=insert-just! conc-node new-just)))

(defun strat=insert-just! (node new-just)  
  (declare (edited  "05-AUG-1998")
	   (authors Ameier)
	   (input   "A node and a justification.")
	   (effect  "The justification of the node is set on new-just, and new-just contains in its PLIST the old just as the"
		    "entry old-just.")
	   (value   "Undefined."))
  (let* ((old-just (node~justification node)))
    (pdsj~insert-just-above old-just new-just)
    (setf (node~justification node) new-just)))

(defun strat~pplanner-strategy-ks-application-expansion (outline parameters)
  (declare (edited  "17-JUN-1999")
	   (authors Ameier)
	   (input   "The outline and the parameters of a pplanner-strategy-ks-application.")
	   (effect  "Set the justification below of the pplan-strategy as current strategy!")
	   (value   "Undefined."))

  (let* ((conc-node (first outline))
	 (pplan-strat-just (node~justification conc-node))
	 (below-just (pdsj~below pplan-strat-just)))

    (setf (node~justification conc-node)
	  below-just)))






