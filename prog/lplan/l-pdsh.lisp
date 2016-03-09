;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
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

(in-package "KEIM")

(mod~defmod PDSH
            :uses (pdsj pdsn)
            :documentation "The pds history module"
            :exports (
		      pdsh+canonical-reason
		      pdsh+constraint-state
		      pdsh+ia-reason
		      pdsh+ie-reason
		      pdsh+mor-scope
		      pdsh+pr-reason
		      pdsh+reason

		      pdsh~ancestor-mor-scopes
		      pdsh~cs-bindings
		      pdsh~cs-constraint
		      pdsh~cs-create
		      pdsh~cs-new-bindings
		      pdsh~cs-old-bindings
		      pdsh~domain
		      pdsh~followed-by-instantiating-step-p
		      pdsh~fs-add
		      pdsh~fs-focus-supports
		      pdsh~fs-goal-formula
		      pdsh~fs-inference
		      pdsh~fs-interacting-steps
		      pdsh~fs-parameters
		      pdsh~fs-sole-setting-p
		      pdsh~fs-used-supports
		      pdsh~fs-valid-p
		      pdsh~fs-with-inference
		      pdsh~fs-with-inference-p
		      pdsh~has-reason-p
		      pdsh~ia-add-conclusions
		      pdsh~ia-add-conclusion-node-p
		      pdsh~ia-conclusions
		      pdsh~ia-conclusion-outline
		      pdsh~ia-delete-supports
		      pdsh~ia-delete-support-p
		      pdsh~ia-expansion-reason
		      pdsh~ia-extra-hypotheses
		      pdsh~ia-extra-hypothese-p
		      pdsh~ia-focus-goals
		      pdsh~ia-focus-supports
		      pdsh~ia-inference
		      pdsh~ia-inherited-supports
		      pdsh~ia-most-abstract-justification
		      pdsh~ia-outline-patterns
		      pdsh~ia-premises
		      pdsh~ia-premise-outline
		      pdsh~ia-primary-goals
		      pdsh~ia-primary-goal-node-p
		      pdsh~ia-reason-p
		      pdsh~ia-ref
		      pdsh~ia-secondary-goals
		      pdsh~ia-subgoals
		      pdsh~ia-subgoal-node-p
		      pdsh~ia-used-supports
		      pdsh~ie-expanded-justification
		      pdsh~ie-expansion-leaves
		      pdsh~ie-expansion-nodes
		      pdsh~ie-expansion-origins
		      pdsh~ie-expansion-origin-nodes
		      pdsh~ie-further-expansion-reasons
		      pdsh~ie-reason-p
		      pdsh~influenced-by-p
		      pdsh~influenced-steps
		      pdsh~influencing-steps
		      pdsh~input-nodes
		      pdsh~instantiating-step-p
		      pdsh~latest-influencing-step
		      pdsh~latest-reason-in-list
		      pdsh~metavars-of
		      pdsh~metavars-of-current-node
		      pdsh~metavars-of-node
		      pdsh~ms-ancestor-scopes
		      pdsh~ms-create
		      pdsh~ms-cstr-state
		      pdsh~ms-domain
		      pdsh~ms-reason
		      pdsh~ms-update-domain
		      pdsh~mor-object-p
		      pdsh~mor-scope-p
		      pdsh~mor-step-p
		      pdsh~mor-task-p
		      pdsh~new-task-nodes
		      pdsh~occurs-in-ancestor-scope-p
		      pdsh~originator-step
		      pdsh~pr-constraint-state
		      pdsh~pr-input-nodes
		      pdsh~pr-parameters
		      pdsh~pr-reason-p
		      pdsh~pr-refinement
		      pdsh~reasons-eq-or-later
		      pdsh~reasons-later-in-list
		      pdsh~reasons-later-than
		      pdsh~reason-eq-or-later-p
		      pdsh~reason-later-p
		      pdsh~reason-mor-effect
		      pdsh~reason-new-metavars
		      pdsh~reason-new-nodes
		      pdsh~reason-of-existence
		      pdsh~reason-of-existence-p
		      pdsh~reason-p
		      pdsh~reason-predecessor
		      pdsh~reason-retracted
		      pdsh~reason-successor
		      pdsh~redundant-step-p
		      pdsh~sort-reasons
		      pdsh~super-goals))

#{
This module implements the pds history, which stores information, that is needed in the backtracking
of tasks.
The algorithms, data structures and definitions in this module are developed
in a paper by Lassaad Cheikhrouhou. 
\subsection{Data Structures Representing Refinement Steps}
Refinement steps represent the steps done in the proof developing process, i.e. automated planning steps
as well as interactive steps. These data structures are called reasons.
#}

(eval-when (load compile eval)
  (defclass pdsh+constraint-state ()
    ((old-bindings     :initform nil
		       :accessor pdsh~cs-old-bindings
		       :initarg :old-bindings)
     (new-bindings     :initform nil
		       :accessor pdsh~cs-new-bindings
		       :initarg :new-bindings)
     (bindings         :initform nil
		       :accessor pdsh=cs-bindings
		       :initarg :bindings)
     (constraint       :initform nil
		       :accessor pdsh~cs-constraint
		       :initarg :constraint))
    (:documentation "Data structure representing a constraint state.")))

(defmethod print-object ((cs pdsh+constraint-state) stream)
  (format stream 
	  (if *print-escape* "#<pdsh+constraint-state with old bindings: ~A, new bindings ~A and constraint: ~A>"
	    "#<Old bindings: ~A, new bindings ~A and constraint: ~A>")
	  (pdsh~cs-old-bindings cs) (pdsh~cs-new-bindings cs) (pdsh~cs-constraint cs)))
 
(defun pdsh~cs-create (new-bindings constraint old-constraint-states)
  (let ((new-constraint-state (make-instance 'pdsh+constraint-state
					     :new-bindings new-bindings
					     :constraint constraint)))
    (when old-constraint-states
      (setf (pdsh~cs-old-bindings new-constraint-state)
	    (when old-constraint-states
	      (reduce 'subst~disjoint-compose-substitution (mapcar 'pdsh~cs-bindings old-constraint-states)))))
    new-constraint-state))

(defmethod pdsh~cs-bindings ((cs pdsh+constraint-state))
  (if (pdsh=cs-bindings cs)
      (pdsh=cs-bindings cs)
    (let ((bindings (if (pdsh~cs-old-bindings cs)
			(subst~compose-substitution-nso (pdsh~cs-new-bindings cs) (pdsh~cs-old-bindings cs))
		      (pdsh~cs-new-bindings cs))))
      (setf (pdsh=cs-bindings cs) bindings)
      bindings)))

(eval-when (load compile eval)
  (defclass pdsh+mor-scope ()
    ((domain           :initform nil
		       :accessor pdsh~ms-domain
		       :initarg :domain)
     (cstr-state       :initform nil
		       :accessor pdsh~ms-cstr-state
		       :initarg :cstr-state)
     (ancestor-scopes  :initform nil
		       :accessor pdsh~ms-ancestor-scopes
		       :initarg :ancestor-scopes)
     (reason           :initform nil
		       :accessor pdsh~ms-reason
		       :initarg :reason))
    (:documentation "Data structure for keeping the history of the pds constaint store.")))
     
(eval-when (load compile eval)
  (defclass pdsh+reason ()
    ((predecessor  :initform nil
		   :accessor pdsh~reason-predecessor
		   :initarg :predecessor)
     (successor    :initform nil
		   :accessor pdsh~reason-successor
		   :initarg :successor)
     (mor-effect   :initform nil
		   :accessor pdsh~reason-mor-effect
		   :initarg :mor-effect)
     (new-metavars :initform nil
		   :accessor pdsh~reason-new-metavars
		   :initarg :new-metavars)
     (new-nodes    :initform nil
		   :accessor pdsh~reason-new-nodes
		   :initarg :new-nodes)
     (retracted    :initform nil
		   :accessor pdsh~reason-retracted
		   :initarg :retracted))
    (:documentation "Base data structure representing a refinement step.")))

(eval-when (load compile eval)
  (defclass pdsh+canonical-reason (pdsh+reason)
    ()
    (:documentation "Base data structure representing a canonical refinement step.")))

(eval-when (load compile eval)
  (defclass pdsh+pr-reason (pdsh+reason)
    ((refinement       :initform nil
		       :accessor pdsh~pr-refinement
		       :initarg :refinement)
     (input-nodes      :initform nil
		       :accessor pdsh~pr-input-nodes
		       :initarg :input-nodes)
     (parameters       :initform nil
		       :accessor pdsh~pr-parameters
		       :initarg :parameters)
     (constraint-state :initform nil
		       :accessor pdsh~pr-constraint-state
		       :initarg :constraint-state))
    (:documentation "Base data structure representing a proper refinement step.")))

(eval-when (load compile eval)
  (defclass pdsh+ia-reason (pdsh+canonical-reason)
    ((conclusions      :initform nil
		       :accessor pdsh~ia-conclusions
		       :initarg :conclusions)
     (ref              :initform nil
		       :accessor pdsh~ia-ref
		       :initarg :ref)
     (outline-patterns :initform nil
		       :accessor pdsh~ia-outline-patterns
		       :initarg :outline-patterns)
     (secondary-goals  :initform nil
		       :accessor pdsh~ia-secondary-goals
		       :initarg :secondary-goals))
    (:documentation "Data structure representing an inference application refinement step.")))

(eval-when (load compile eval)
  (defclass pdsh+ie-reason (pdsh+canonical-reason)
    ((expansion-origins :initform nil
			:accessor pdsh~ie-expansion-origins
			:initarg :expansion-origins))
    (:documentation "Data structure representing an inference application refinement step.")))

(eval-when (load compile eval)
  (defclass pdsh+failed-step ()
    ((inference         :initform nil
			:accessor pdsh~fs-inference
			:initarg :inference)
     (parameters        :initform nil
			:accessor pdsh~fs-parameters
			:initarg :parameters)
     (used-supports     :initform nil
			:accessor pdsh~fs-used-supports
			:initarg :used-supports)
     (goal-formula      :initform nil
			:accessor pdsh~fs-goal-formula
			:initarg :goal-formula)
     (interacting-steps :initform nil
			:accessor pdsh~fs-interacting-steps
			:initarg :interacting-steps)
     (focus-supports    :initform nil
			:accessor pdsh~fs-focus-supports
			:initarg :focus-supports))
    (:documentation "Data structure representing a failed inference step.")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions concerning reasons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{General Functions Concerning Reasons}
Most functions in this section deal with the chronological relation between reasons.
They are independent of a special subtype of reason.
#}

(defgeneric pdsh~reason-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An arbitrary parameter.")
	   (effect  "None.")
	   (value   "True iff rsn is a reason."))
  (:method ((rsn pdsh+reason))
	   t)
  (:method (rsn)
	   nil))

(defgeneric pdsh~pr-reason-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An arbitrary parameter.")
	   (effect  "None.")
	   (value   "True iff rsn is a proper reason."))
  (:method ((rsn pdsh+pr-reason))
	   t)
  (:method (rsn)
	   nil))

(defun pdsh~reasons-eq-or-later (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "A list containing rsn itself as first argument"
		    "and all reasons occuring later in the history than rsn as the rest of the list."
		    "The reasons remain in their chronological sequence (latest reason last)."))
  (when (not (null rsn))
    (cons rsn (pdsh~reasons-eq-or-later (pdsh~reason-successor rsn)))))

(defun pdsh~reasons-later-than (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "A list of all reasons occuring later in the history than rsn."
		    "The reasons remain in their chronological sequence (latest reason last)."))
  (rest (pdsh~reasons-eq-or-later rsn)))

(defun pdsh~reasons-later-in-list (reasons rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A list of reasons and a reason.")
	   (effect  "None.")
	   (value   "The reasons that occur after rsn in the list, if rsn is a member of reasons,"
		    "the whole list otherwise."))
  (if (not (find rsn reasons))
      reasons
    (let ((pos (position rsn reasons)))
      (nthcdr (+ pos 1) reasons))))

(defun pdsh~reason-eq-or-later-p (rsn1 rsn2)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "Two reasons.")
	   (effect  "None.")
	   (value   "True iff rsn2 occurs later than rsn1 or rsn1 eq rsn2."
		    "In case the reason are not chronologically comparable nil is returned."))
  (cond ((null rsn1) nil)
	((eq rsn1 rsn2) t)
	(t (pdsh~reason-eq-or-later-p (pdsh~reason-successor rsn1) rsn2))))

(defun pdsh~reason-later-p (rsn1 rsn2)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "Two reasons.")
	   (effect  "None.")
	   (value   "True iff rsn2 occurs later than rsn1."))
  (and (not (eq rsn1 rsn2)) (pdsh~reason-eq-or-later-p rsn1 rsn2)))

(defun pdsh~latest-reason-in-list (reasons)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason list.")
	   (effect  "None.")
	   (value   "The latest reason in the list or nil in case of an empty list."))
  (when reasons
    (if (not (second reasons))
	(first reasons)
      (let ((latest-reason (pdsh~latest-reason-in-list (rest reasons))))
	(if (pdsh~reason-later-p (first reasons) latest-reason)
	    latest-reason
	  (first reasons))))))

(defun pdsh~sort-reasons (reasons)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason list.")
	   (effect  "None.")
	   (value   "A chronologically sorted list with duplicates removed."
		    "The last element corresponds to the latest reason."))
  (when reasons
    (let ((latest-rsn (pdsh~latest-reason-in-list reasons)))
      (append (pdsh~sort-reasons (remove-if #'(lambda (rsn) (eq latest-rsn rsn)) reasons))
	      (list latest-rsn)))))

(defun pdsh~has-reason-p (node rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node and a reason.")
	   (effect  "None.")
	   (value   "True iff node is involved in rsn."))
  (find rsn (pdsn~reasons node)))

(defgeneric pdsh~input-nodes (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "The input nodes of rsn."))
  (:method ((rsn pdsh+ie-reason))
	   (append (pdsh~ie-expansion-origin-nodes rsn) (pdsh~ie-expansion-leaves rsn)))
  (:method ((rsn pdsh+ia-reason))
	   (append (pdsh~ia-primary-goals rsn) (pdsh~ia-used-supports rsn)))
  (:method ((rsn pdsh+pr-reason))
	   (pdsh~pr-input-nodes rsn)))

(defgeneric pdsh~new-task-nodes (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "The nodes of for which a new task has to be created."))
  (:method ((rsn pdsh+ie-reason))
	   (pdsh~ie-expansion-origin-nodes rsn))
  (:method ((rsn pdsh+ia-reason))
	   (pdsh~ia-primary-goals rsn))
  (:method ((rsn pdsh+pr-reason))
	   ;; has to be implemented
	   ))

(defgeneric pdsh~reason-inference (rsn)
  (:method ((rsn pdsh+ia-reason))
	   (just~method (pdsh~ia-most-abstract-justification rsn)))
  (:method ((rsn pdsh+ie-reason))
	   (just~method (pdsh~ie-expanded-justification rsn)))
  (:method ((rsn pdsh+pr-reason))
	   ;; has to be implemented
	   ))

(defgeneric pdsh~reason-of-existence-p (rsn node)
  (:method ((rsn pdsh+ia-reason) (node pdsn+node))
	   (or (find node (pdsh~ia-subgoals rsn))
	       (find node (pdsh~ia-extra-hypotheses rsn))
	       (find node (pdsh~ia-add-conclusions rsn))))
  (:method ((rsn pdsh+ie-reason) (node pdsn+node))
	   (find node (pdsh~ie-expansion-nodes rsn)))
  (:method ((rsn pdsh+pr-reason) (node pdsn+node))
	   (find node (pdsh~pr-input-nodes rsn))))

(defun pdsh~reason-of-existence (node)
  (find-if #'(lambda (rsn) (pdsh~reason-of-existence-p rsn node)) (pdsn~reasons node)))
  
(defgeneric pdsh~super-goals (node)
  (:method ((node pdsn+node))
	   (let ((reason-of-existence (pdsh~reason-of-existence node)))
	     (when (and reason-of-existence (find node (pdsh~ia-subgoals reason-of-existence)))
	       (pdsh~super-goals reason-of-existence))))
  (:method ((rsn pdsh+ia-reason))
	   (union (pdsh~ia-conclusions rsn) (pdsh~ia-secondary-goals rsn)))
  (:method ((rsn pdsh+ie-reason))
	   (pdsh~ie-expansion-origin-nodes rsn))
  (:method ((rsn pdsh+pr-reason))
	   (pdsh~pr-input-nodes)))

(defgeneric pdsh~redundant-step-p (rsn inference focus-goals supports inherited-supports)
  (:method ((rsn pdsh+ia-reason) inference focus-goals supports inherited-supports)
	   (when (and
		  (keim~equal (pdsh~reason-inference rsn) inference)
		  (every 'keim~equal (pdsh~input-nodes rsn) supports)
		  (not (some #'(lambda (later-rsn)
				 (some #'(lambda (support) (pdsh~node-influenced-by-p support later-rsn)) supports))
			     (pdsh~reasons-eq-or-later rsn)))
		  (or (not (pdsh~ia-subgoals rsn))
		      ;; the inference has subgoals
		      ;; now look at the inherited supports
		      (let ((ia-inherited-supports (pdsh~ia-inherited-supports rsn)))
			(and (= (length ia-inherited-supports)
				(length inherited-supports))
			     (every #'(lambda (ia-is) (find ia-is inherited-supports))
				    ia-inherited-supports)
			     ;; the inherited supports are identical
			     (not (some #'(lambda (later-rsn)
					    (some #'(lambda (inherited-support) (pdsh~node-influenced-by-p inherited-support later-rsn)) inherited-supports))
					(pdsh~reasons-eq-or-later rsn)))))))
	     rsn))
  (:method ((rsn pdsh+ie-reason) inference focus-goals supports inherited-supports)
	   nil)
  (:method ((rsn pdsh+pr-reason) inference focus-goals supports inherited-supports)
	   ;; has to be implemented
	   ))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions concerning failed steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{General Functions Concerning Failed Steps}
#}

(defmethod print-object ((object pdsh+failed-step) stream)
  (if (pdsh~fs-valid-p object)
      (format stream "#<valid ~_ failed step ~A ~A ~A ~A>"
	      (pdsh~fs-inference object)
	      (pdsh~fs-parameters object)
	      (pdsh~fs-used-supports object)
	      (pdsh~fs-goal-formula object))
    (format stream "#<invalid failed step>")))

(defun pdsh~fs-valid-p (failed-step)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A failed step.")
	   (effect  "None.")
	   (value   "True iff the failed step is valid."))
  (if (some 'pdsh~reason-retracted (pdsh~fs-interacting-steps failed-step))
      ;; failed step is not valid
      nil
    ;; failed step is valid
    t))

(defun pdsh~fs-sole-setting-p (failed-step)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A failed.")
	   (effect  "None.")
	   (value   "True iff the failed step is valid and the contained inference"
		    "can only be applied with a sole setting."))
  (and (pdsh~fs-valid-p failed-step)
       (not (or (pdsh~fs-parameters failed-step)
		(pdsh~fs-used-supports failed-step)
		(pdsh~fs-goal-formula failed-step)))))

(defun pdsh~fs-add (node rsn interacting-steps inference)
  (let ((used-supports (mapcar #'(lambda (support) (if (pdsn~schematic-p support)
						       (pds~node-formula support)
						     support))
			       (pdsh~ia-used-supports rsn))))
    (misc~append-to-slot (pdsn~failed-steps node)
			 (make-instance 'pdsh+failed-step
					:inference inference
					:parameters (pdsh~ia-parameters rsn)
					:used-supports used-supports
					:goal-formula (when (pdsn~schematic-p node) (pds~node-formula node))
					:interacting-steps interacting-steps
					:focus-supports (set-difference (pdsn~supports node) used-supports)))))

(defun pdsh~fs-with-inference-p (failed-step inference)
  (eq (pdsh~fs-inference failed-step) inference))

(defgeneric pdsh~fs-with-inference (obj inference)
  (:method ((node pdsn+node) inference)
	   (pdsh~fs-with-inference (pdsn~failed-steps node) inference))
  (:method ((failed-steps list) inference)
	   (remove-if-not #'(lambda (failed-step) (pdsh~fs-with-inference-p failed-step inference))
			  failed-steps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions concerning inference application reasons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{General Functions Concerning Inference Application Reasons}
The functions in this section provide information that can be obtained from an inference
application reason.
#}

(defmethod keim~equal ((rsn1 pdsh+ia-reason) (rsn2 pdsh+ia-reason))
  (and (keim~equal (pdsh~ia-conclusions rsn1) (pdsh~ia-conclusions rsn2))
       (every 'string-equal (pdsh~ia-outline-patterns rsn1) (pdsh~ia-outline-patterns rsn2))
       (keim~equal (pdsh~ia-secondary-goals rsn1) (pdsh~ia-secondary-goals rsn2))
;       (keim~equal (pdsh~reason-predecessor rsn1) (pdsh~reason-predecessor rsn2))
;       (keim~equal (pdsh~reason-successor rsn1) (pdsh~reason-successor rsn2))
       (keim~equal (pdsh~reason-mor-effect rsn1) (pdsh~reason-mor-effect rsn2))))

(defmethod keim~equal ((rsn1 pdsh+ia-reason) rsn2)
  nil)

(defmethod print-object ((object pdsh+ia-reason) stream)
  (format stream "#<application of ~A on focus goals ~A with premises ~A>"
	  (pdsh~ia-inference object)
	  (pdsh~ia-focus-goals object)
	  (pdsh~ia-premises object)))
  
(defmethod print-object ((object pdsh+ie-reason) stream)
  (format stream "#<expansion of ~A ~A>"
	  (pdsh~ie-expansion-origin-nodes object)
	  (pdsh~ie-expanded-justification object)))
  
(defun pdsh~ia-reason-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An arbitrary parameter.")
	   (effect  "None.")
	   (value   "True iff rsn is a inference application reason."))
  (typep rsn 'pdsh+ia-reason))

(defun pdsh~ia-expansion-reason (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason rsn.")
	   (effect  "None.")
	   (value   "The inference expansion reason, if the inference application"
		    "is expanded, nil otherwise."))
  (let ((expanded-just (pdsh~ia-most-abstract-justification rsn)))
    (when (pdsj~expanded-p expanded-just)
      (find-if #'(lambda (reason)
		   (when (pdsh~ie-reason-p reason)
		     (pdsj~justs-equal-at-this-level
		      (pdsh~ie-expanded-justification reason)
		      expanded-just))) (pdsn~reasons (first (pdsh~ia-conclusions rsn)))))))

(defun pdsh~ia-most-abstract-justification (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The most abstract justification, ie the justification representing the inference application."))
  (pdsj~most-abstract (node~justification (first (pdsh~ia-conclusions rsn)))))

;(defun pdsh~ia-ref (rsn)
;  (let* ((inference (pdsh~ia-inference rsn))
;	 (outline-pattern (pdsh~ia-outline-patterns rsn))
;	 (outline-pattern2 (substitute infer*non-existent infer*added outline-pattern :test 'string=))
;	 (outline-pattern3 (substitute infer*existent infer*persistent outline-pattern2 :test 'string=))
;	 (outline-pattern4 (substitute infer*closed infer*deleted outline-pattern3 :test 'string=)))
;    (pds~inference-application inference outline-pattern4)))
    
(defun pdsh~ia-inference (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The inference of the inference application."))
  (just~method (pdsh~ia-most-abstract-justification rsn)))

(defun pdsh~ia-premises (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The premises of the inference application."))
  (just~premises (pdsh~ia-most-abstract-justification rsn)))

(defun pdsh~ia-premise-outline (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "The outline of node wrt the inference application if node is"
		    "a premise node, nil otherwise."))
  (let* ((node-list (pdsh~ia-premises rsn))
	 (outline-pattern (pdsh~ia-outline-patterns rsn))
	 (node-pos (position node node-list)))
    (when node-pos
      (nth (+ node-pos
	      (- (length outline-pattern) (length node-list)))
	   outline-pattern))))

(defun pdsh~ia-subgoal-node-p (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "True iff node is a subgoal node of the inference application."))
  (infer~added-pattern-p (pdsh~ia-premise-outline rsn node)))

(defun pdsh~ia-conclusion-outline (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "The outline of node wrt the inference application if node is"
		    "a conclusion node, nil otherwise."))
  (let* ((node-list (pdsh~ia-conclusions rsn))
	 (outline-pattern (pdsh~ia-outline-patterns rsn))
	 (node-pos (position node node-list)))
    (when node-pos
      (nth node-pos outline-pattern))))

(defun pdsh~ia-add-conclusion-node-p (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "True iff node is a add conclusion node of the inference application."))
  (infer~added-pattern-p (pdsh~ia-conclusion-outline rsn node)))

(defun pdsh~ia-primary-goal-node-p (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "True iff node is a primary goal node of the inference application."))
  (infer~existent-pattern-p (pdsh~ia-conclusion-outline rsn node)))
 
(defun pdsh~ia-subgoals (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The subgoals of the inference application."))
  (remove-if-not #'(lambda (prem) (pdsh~ia-subgoal-node-p rsn prem))
		 (pdsh~ia-premises rsn)))

(defun pdsh~ia-used-supports (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The used supports of the inference application."))
  (remove-if #'(lambda (prem) (pdsh~ia-subgoal-node-p rsn prem))
	     (pdsh~ia-premises rsn)))
  
(defun pdsh~ia-delete-support-p (rsn node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a node.")
	   (effect  "None.")
	   (value   "True iff node is a delete support node of the inference application."))
  (infer~deleted-pattern-p (pdsh~ia-premise-outline rsn node)))
 
(defun pdsh~ia-delete-supports (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The delete supports of the inference application."))
  (remove-if-not #'(lambda (prem) (pdsh~ia-delete-support-p rsn prem))
		 (pdsh~ia-premises rsn)))

(defun pdsh~ia-add-conclusions (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The add conclusions of the inference application."))
  (remove-if-not #'(lambda (conc) (pdsh~ia-add-conclusion-node-p rsn conc))
		 (pdsh~ia-conclusions rsn)))
    
(defun pdsh~ia-primary-goals (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The primary goals of the inference application."))
  (remove-if-not #'(lambda (conc) (pdsh~ia-primary-goal-node-p rsn conc))
		 (pdsh~ia-conclusions rsn)))

(defun pdsh~ia-extra-hypothese-p (rsn hyp)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason and a hypothese node.")
	   (effect  "None.")
	   (value   "True iff hyp was introduced by rsn."))
  (not (some #'(lambda (conc) (find hyp (pdsn~hyps conc))) (pdsh~ia-conclusions rsn))))

(defun pdsh~ia-extra-hypotheses (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference application reason.")
	   (effect  "None.")
	   (value   "The hypotheses introduced by the inference application."))
  (let ((extra-hyps nil))
    (dolist (subgoal (pdsh~ia-subgoals rsn))
      (dolist (hyp (pdsn~hyps subgoal))
	(when (pdsh~ia-extra-hypothese-p rsn hyp)
	  (setf extra-hyps (cons hyp extra-hyps)))))
    extra-hyps))

(defun pdsh~ia-parameters (rsn)
  (pdsj~parameters (pdsh~ia-most-abstract-justification rsn)))

(defun pdsh~ia-focus-goals (rsn)
  (union (pdsh~ia-primary-goals rsn) (pdsh~ia-secondary-goals rsn)))

(defun pdsh~ia-focus-supports (rsn)
  (reduce 'union (mapcar 'pdsn~supports (pdsh~ia-focus-goals rsn))))

(defun pdsh~ia-inherited-supports (rsn)
  (set-difference (pdsh~ia-focus-supports rsn) (pdsh~ia-delete-supports rsn)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions concerning inference expansion reasons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{General Functions Concerning Inference Expansion Reasons}
The functions in this section provide information that can be obtained from an inference
expansion reason.
#}

(defmethod keim~equal ((rsn1 pdsh+ie-reason) (rsn2 pdsh+ie-reason))
  (and (keim~equal (pdsh~ie-expansion-origins rsn1) (pdsh~ie-expansion-origins rsn2))
;       (keim~equal (pdsh~reason-predecessor rsn1) (pdsh~reason-predecessor rsn2))
;       (keim~equal (pdsh~reason-successor rsn1) (pdsh~reason-successor rsn2))
       (keim~equal (pdsh~reason-mor-effect rsn1) (pdsh~reason-mor-effect rsn2))))

(defmethod keim~equal ((rsn1 pdsh+ie-reason) rsn2)
  nil)

(defun pdsh~ie-reason-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An arbitrary parameter.")
	   (effect  "None.")
	   (value   "True iff rsn is an inference expansion reason."))
  (typep rsn 'pdsh+ie-reason))

(defun pdsh~ie-further-expansion-reasons (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference expansion reason rsn.")
	   (effect  "None.")
	   (value   "A list of inference expansion reasons, if the expansion roots are"
		    "further expanded, nil otherwise."))
  (let ((expansion-reasons nil))
    (dolist (expansion-origin (pdsh~ie-expansion-origins rsn))
      (let ((expansion-just (pdsj~below (second expansion-origin)))
	    (expansion-root (first expansion-origin)))
	(when (pdsj~expanded-p expansion-just)
	  (dolist (reason (pdsn~reasons expansion-root))
	    (when (pdsh~ie-reason-p reason)
	      (when (pdsj~justs-equal-at-this-level
		     (pdsh~ie-expanded-justification reason)
		     expansion-just)
		(setf expansion-reasons (append expansion-reason (list reason)))))))))
    expansion-reasons))
       
(defun pdsh~ie-expanded-justification (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference expansion reason.")
	   (effect  "None.")
	   (value   "The expanded justification of rsn."))
  (second (first (pdsh~ie-expansion-origins rsn))))

(defun pdsh~ie-expansion-leaves (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference expansion reason.")
	   (effect  "None.")
	   (value   "The leaves of the inference expansion."))
  (just~premises (pdsh~ie-expanded-justification rsn)))
					    
(defun pdsh~ie-expansion-nodes (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference expansion reason.")
	   (effect  "None.")
	   (value   "The expansion nodes of rsn without origins and leaves."))
  (let ((leaves (pdsh~ie-expansion-leaves rsn))
	(exp-nodes nil))
    (dolist (expansion-origin (pdsh~ie-expansion-origins rsn))
      (let ((expansion-just (pdsj~below (second expansion-origin))))
	(setf exp-nodes (append exp-nodes
				(pdsh=iter-premises expansion-just #'(lambda (node) (pdsh~has-reason-p node rsn)))))))
    (remove-duplicates exp-nodes)
    (remove-if #'(lambda (node) (find node leaves)) exp-nodes)))

(defun pdsh~ie-expansion-origin-nodes (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An inference expansion reason.")
	   (effect  "None.")
	   (value   "A list of the expansion origin nodes of rsn."))
  (mapcar 'first (pdsh~ie-expansion-origins rsn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions concerning mor scopes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{General Functions Concerning Mor Scopes}
The mor scope of a reason describes the meta variables, its constraits and its binding relevant
for the reason. In this section the relation 'influenced' between tasks, nodes and steps is
defined.
#}

(defmethod keim~equal ((ms2 pdsh+mor-scope) (ms2 pdsh+mor-scope))
  (and (keim~equal (pdsh~ms-domain ms1) (pdsh~ms-domain ms2))
       (keim~equal (pdsh~ms-cstr-state ms1) (pdsh~ms-cstr-state ms2))
;       (keim~equal (pdsh~ms-ancestor-scopes ms1) (pdsh~ms-ancestor-scopes ms2))
       (keim~equal (pdsh~ms-reason ms1) (pdsh~ms-reason ms2))))

(defmethod print-object ((object pdsh+mor-scope) stream)
  (format stream "#<mor-scope with domain ~A and ~A"
	  (pdsh~ms-domain object) (pdsh~ms-cstr-state object)))

(defun pdsh~mor-scope-p (ms)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "An arbitrary parameter.")
	   (effect  "None.")
	   (value   "True iff ms is a mor scope."))
  (typep ms 'pdsh+mor-scope))

(defun pdsh~ms-update-domain (mor-scope cstr-vars)
  (let ((cstr-state (pdsh~ms-cstr-state mor-scope))
	(ancestor-scopes (pdsh~ms-ancestor-scopes mor-scope)))
    (setf (pdsh~ms-domain mor-scope) (union (union (subst~domain (pdsh~cs-new-bindings cstr-state))
						   cstr-vars)
					    (apply 'append (mapcar 'pdsh~domain ancestor-scopes))))
    mor-scope))
    
(defun pdsh~ms-create (cstr-state ancestor-scopes cstr-vars)
  (pdsh~ms-update-domain (make-instance 'pdsh+mor-scope
					:cstr-state cstr-state
					:ancestor-scopes ancestor-scopes)
			 cstr-vars))

(defgeneric pdsh~domain (scope)
  (:method ((scope list))
	   (when (not (null scope))
	     (union (pdsh~domain (first scope)) (pdsh~domain (rest scope)))))
  (:method ((scope pdsh+mor-scope))
	   (pdsh~ms-domain scope))
  (:method ((scope meta+variable))
	   (list scope)))

(defgeneric pdsh~occurs-in-ancestor-scope-p (mvar ancestor-scope)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "mvar: a list of meta variables or a single meta variable"
		    "ancestor-scope: a list of ancestor scopes or a single ancestor scope")
	   (effect  "None.")
	   (value   "True iff some meta variable occurs in some ancestor scope."))
  (:method ((mvar list) ancestor-scope)
	   (when (not (null mvar))
	     (or (pdsh~occurs-in-ancestor-scope-p (first mvar) ancestor-scope)
		 (pdsh~occurs-in-ancestor-scope-p (rest mvar) ancestor-scope))))
  (:method ((mvar meta+variable) ancestor-scope)
	   (find mvar (pdsh~domain ancestor-scope))))

(defgeneric pdsh~metavars-of-current-node (node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The meta variables in the current formula of node."))
  (:method ((nodes list))
	   (when (not (null nodes))
	     (union (pdsh~metavars-of-current-node (first nodes)) (pdsh~metavars-of-current-node (rest nodes)))))
  (:method ((node pdsn+schematic-node))
	   ;;; return the metavars of the current formula
	   (remove-if-not 'meta~p (term~variables (pds~node-formula node))))
  (:method ((node pdsn+node))
	   NIL))
  
(defgeneric pdsh~metavars-of-node (node)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The meta variables in the original formula of node."))
  (:method ((nodes list))
	   (when (not (null nodes))
	     (union (pdsh~metavars-of-node (first nodes)) (pdsh~metavars-of-node (rest nodes)))))
  (:method ((node pdsn+schematic-node))
	   ;;; return the metavars of the original formula
	   (remove-if-not 'meta~p (term~variables (node~formula node))))
  (:method ((node pdsn+node))
	   NIL))
  
(defun pdsh~node-influenced-by-p (node rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node and a reason.")
	   (effect  "None.")
	   (value   "True iff one of the meta variables of node occurs in an ancestor scope of rsn."))
  (when (and (pdsn~schematic-p node) (pdsh~reason-mor-effect rsn))
    (pdsh~occurs-in-ancestor-scope-p (pdsh~metavars-of-node node) (pdsh~ms-ancestor-scopes (pdsh~reason-mor-effect rsn)))))

(defun pdsh~influenced-steps (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "The steps influenced by rsn."))
  (let ((reasons (pdsh~reasons-later-than rsn)))
    (remove-if-not #'(lambda (reason)
		       (some #'(lambda (node) (pdsh~node-influenced-by-p node rsn))
			     (pdsh~input-nodes reason)))
		   reasons)))

(defgeneric pdsh~metavars-of (thing)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "One of the following: A formula, a node list, a node, a task.")
	   (effect  "None.")
	   (value   "The meta variables of thing according to the definition in LC's paper."))
  (:method ((node node+node))
	   (if (pdsn~open-node-p node)
	       (pdsh~metavars-of-node (cons node
					    (remove-duplicates (append (pdsn~hyps node)
								       (pdsn~supports node)))))
	     ;;; return metavars used in expansion condition
	     ))
  (:method ((task agenda+task))
	   (pdsh~metavars-of (agenda~task-node task))))

(defun pdsh~mor-task-p (task)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "True iff task is a mor task according to the definition in LC's paper."))
  (pdsh~mor-object-p task))

(defun pdsh~mor-step-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "True iff rsn has a mor effect."))
  (when (not (null (pdsh~reason-mor-effect rsn))) t))

(defgeneric pdsh~mor-object-p (obj)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node or a task.")
	   (effect  "None.")
	   (value   "True iff the node is a mor node or the task is a mor task."))
  (:method ((node node+node))
	   (if (pdsn~open-node-p node)
	       (or
		(pdsn~schematic-p node)
		(some 'pdsn~schematic-p (pdsn~hyps node))
		(some 'pdsn~schematic-p (pdsn~supports node)))
	     ;;; return true iff some metavars are used in expansion condition
	     ))
  (:method ((task agenda+task))
	   (pdsh~mor-object-p (agenda~task-node task))))

(defgeneric pdsh~originator-step (obj)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node or a task.")
	   (effect  "None.")
	   (value   "The originator step according to the definition in LC's paper."))
  (:method ((node node+node))
	   (let ((reasons (pdsh~sort-reasons (pdsn~reasons node))))
	     (if (pdsn~open-node-p node)
	         ;;; node open
		 (let ((last-updating-rsn
			(find-if #'(lambda (rsn) (or (pdsh~pr-reason-p rsn)
						     ;;; Im Fall proper refinement evtl. weitere
						     ;;; Bedingungen einfuegen
						     (when (pdsh~ia-reason-p rsn)
						       (find node (pdsh~ia-secondary-goals rsn)))))
				 (reverse reasons))))
		   (if last-updating-rsn
		       last-updating-rsn
		     (first reasons)))
	       ;;; node closed
	       (let ((last-closing-rsn
		      (find-if #'(lambda (rsn) (or (when (pdsh~ia-reason-p rsn)
						     (find node (pdsh~ia-conclusions rsn)))
						   (when (pdsh~ie-reason-p rsn)
						     (and
						      (find node (pdsh~ie-expansion-origin-nodes rsn))
						      (pdsj~justs-equal-at-this-level
						       (pdsh~ie-expanded-justification rsn)
						       (pdsj~above (pdsj~least-abstract (node~justification node))))))))
			       (reverse reasons))))
		 (if last-closing-rsn
		     last-closing-rsn
		   ;;; Vorsicht: kann im Fall von mehreren proper refinements nicht entscheiden
		   ;;; welches die node geschlossen hat.
		   (first reasons))))))
  (:method ((task lagenda+goal-schema))
	   (pdsh~originator-step (lagenda~task-node task)))
  (:method ((task lagenda+goal))
	   (pdsh~originator-step (lagenda~task-node task)))
  (:method ((task lagenda+pseudo-goal))
	   (if (lagenda~pseudo-goal-step task)
	       (lagenda~pseudo-goal-step task)
	     (pdsh~originator-step (lagenda~task-node task))))
  )

(defun pdsh~influenced-by-p (obj rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A task or a node and a reason.")
	   (effect  "None.")
	   (value   "True iff the node or the task are influenced by the reason."))
  (when (pdsh~mor-object-p obj)
    (when (pdsh~reason-later-p (pdsh~originator-step obj) rsn)
      (when (pdsh~reason-mor-effect rsn)
	(pdsh~occurs-in-ancestor-scope-p (pdsh~metavars-of obj) (pdsh~ms-ancestor-scopes (pdsh~reason-mor-effect rsn)))))))

(defgeneric pdsh~influencing-steps (obj)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A node or a task or a list with components consisting of nodes or tasks.")
	   (effect  "None.")
	   (value   "All steps that influence at least one of the given nodes/tasks."))
  (:method ((obj-list list))
	   (when obj-list
	     (union (pdsh~influencing-steps (first obj-list)) (pdsh~influencing-steps (rest obj-list)))))
  (:method (obj)
	   (let ((reasons (pdsh~reasons-later-than (pdsh~originator-step obj))))
	     (remove-if-not #'(lambda (reason)
				(pdsh~influenced-by-p obj reason))
			    reasons))))

(defun pdsh~latest-influencing-step (task)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "The last step which influences task."))
  (let ((influencing-steps (pdsh~influencing-steps task)))
    (when influencing-steps
      (first (last influencing-steps)))))

(defun pdsh~ancestor-mor-scopes (mor-scope)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A mor scope.")
	   (effect  "None.")
	   (value   "Returns all ancestor scopes that are real mor scopes, not just meta variables."))
  (let ((ams (pdsh~ms-ancestor-scopes mor-scope)))
    (remove-if #'(lambda (ancestor) (meta~p ancestor)) ams)
    ams))

(defun pdsh~instantiating-step-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "True iff the reason corresponds to an instantiating step."))
  (let ((mor-scope (pdsh~reason-mor-effect rsn)))
    (when mor-scope
      (not (subst~empty-p (pdsh~cs-new-bindings (pdsh~ms-cstr-state mor-scope)))))))

(defun pdsh~followed-by-instantiating-step-p (rsn)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A reason.")
	   (effect  "None.")
	   (value   "True iff the step is followed by an instantiating step."))
  (some 'pdsh~instantiating-step-p (pdsh~reasons-later-than rsn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General functions not concerning reasons directly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Some Helper Functions}
#}

(defun pdsh=iter-premises (just check)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "A justification and a test predicate.")
	   (effect  "None.")
	   (value   "A list of all from just reachable nodes that satisfy check."))
  (let ((nodes nil))
    (dolist (prem (just~premises just))
      (when (funcall check prem) (setf nodes (append nodes
			       (cons prem (pdsh=iter-premises (pdsj~most-abstract (node~justification prem)) check))))))
    nodes))


;;;

(defmethod lagenda~delete-ordering! ((agenda lagenda+agenda) (step pdsh+reason))
  (declare (edited  "11-JUN-2002")
	   (authors Lassaad)
	   (input   "An AGENDA, and a reason representing some refinement STEP.")
	   (effect  "Deletes the orderings from AGENDA that were added by STEP.")
	   (value   "Undefined."))
  (setf (lagenda~first-orderings agenda)
	(remove-if #'(lambda (ordering) (eq (lagenda~ordering-step ordering) step))
		   (lagenda~first-orderings agenda)))
  (setf (lagenda~last-orderings agenda)
	(remove-if #'(lambda (ordering) (eq (lagenda~ordering-step ordering) step))
		   (lagenda~last-orderings agenda)))
  (setf (lagenda~orderings agenda)
	(remove-if #'(lambda (ordering) (eq (lagenda~ordering-step ordering) step))
		   (lagenda~orderings agenda)))
  )
