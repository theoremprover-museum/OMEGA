;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
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

(mod~defmod rule :uses (env just keim meta mod pdsc pdsj pdsn pds node post sys term data)
	    :documentation "Natural deduction rule definition and use."
	    :exports (rule+meta-node
		      rule+rule
		      rule~premises
		      rule~conclusion
		      rule~extra-hyps
		      rule~parameters
		      rule~computations
		      rule~sideconds
		      rule~actions
		      rule~description
		      rule~theory
		      rule~inference
		      rule~env
		      rule~rule-p
		      rule~find-rule
		      rule~apply
		      rule~defrule
		      rule~describe
		      )
	    )

#{\section{Natural Deduction Rules} 
\label{mod:rule}

In this module, we define natural deduction rules.  Natural deduction rules
are like production rules.  They do a match on some set of lines in
a natural deduction proof, and if the match succeeds (and possibly some
other sideconditions are satisfied), carry out some action.  This action
could be to modify some lines in the proof or create new lines. 

Our rules can be thought of as having a ``left-hand'' and ``right-hand'' sides.
The left-hand side is a set of lines which must be present in the proof, and
must be matched in order for the rule to be carried out.  The right-hand
side describes new lines that will be added to the proof when the the rule
is carried out, or describes how the justifications of existing lines
(from the left-hand side) are changed.  Part of the matching procedure
may be in testing sideconditions (such as that a certain variable is
does not occur free in particular lines).

The left-hand side will be called the preconditions, the right-hand side
the postconditions, and the sideconditions, well, they're called the
sideconditions. 

We can also think of our rules as defining functions, which given certain
arguments of specified types, either produce the postconditions or return
an error.  For this reason, we include information about the arguments
(lines and terms) in the rule definition.

To round things out, we allow our rules to make changes in the
support structure of the proof, that is, to change the lines that
may be useful for proving a particular line.  For example, when a 
rule being applied means that a certain line is no longer useful, it
can then be removed as a support from other still-planned lines in the
proof.

Note: many of the ideas contained here are based on the natural deduction
rules of Andrews's TPS system.
#}

#|
rule~argtypes (rulename):  Given the symbol RULENAME, get the list of 
  argument types this rule expects.  As a theoretical example:
  (rule~argtypes 'mp) => (nd+line nd+line)

rule~describe (rulename): Describe the rule named by RULENAME.  No
  other effects.

rule~apply (rulename arg-list): RULENAME is a symbol naming the rule,
  ARG-LIST is a list of lines, formulas, whatever the rule expects, and
  types of the arguments must match what is returned by 
  (rule~argtypes RULENAME).
  Returns two values if rule was successfully carried out, T and
  a list of new and modified lines, or NIL if it failed for some
  reason (with informative output).  
|#

#{\subsection{Meta-lines}
Our rules, in order to be general, are schematic.  That is, they describe
patterns in the input lines.  For this reason, we need what we might
call ``meta-lines'', which have metavariables for their
sets of hypotheses, as well as meta-justifications. 
#}

(eval-when (load compile eval)

(defclass rule=parameter (help+help)
  ((type :accessor rule=parameter-type
	 :initform nil
	 :initarg :type
	 :documentation "The type of the rule parameter."))
  (:documentation "A class of parameters of a rule."))

;;;A meta-node has a list of extra hypotheses:
(defclass rule+meta-node (pdsn+node)
  ((hyps :initform nil)
   (formula :initform nil)
   )
  (:documentation "An pdsn+node with possibly some variables for attributes."))
)

(defmethod print-object ((arg rule=parameter) stream)
  (format stream "{~A ~A ~A}" (keim~name arg) (rule=parameter-type arg) (help~help-string arg)))

(defun rule~meta-node-create (representation hyp-list environment)
  (declare (edited  "20-FEB-1998")
	   (authors Sorge)
	   (input   "A list representation of a meta-node, i.e. (LABEL (HYP-LIST) FORMULA,"
		    "a association list mapping hyp-symbols to hyp-meta-nodes and an environment.")
	   (effect  "Creates a meta-node corresponding to the representation.")
	   (value   "The newly created meta-node."))
  (make-instance 'rule+meta-node
		 :name (first representation)
		 :hyps (mapcar #'(lambda (x) (rule~label2meta-node x hyp-list)) (second representation))
		 :formula (post~read-object (third representation)
					    environment
					    :existing-term)))

(defgeneric rule~label2meta-node (label meta-nodes)
  (declare (edited  "02-MAR-1998")
	   (authors Sorge)
	   (input   "A symbol and a list of meta-nodes.")
	   (effect  "None.")
	   (value   "The meta-node corresponding to the symbol."))
  (:method ((label string) (meta-nodes cons))
	   (cdr (assoc label meta-nodes :test #'string-equal)))
  (:method ((label symbol) (meta-nodes cons))
	   (cdr (assoc label meta-nodes :test #'string-equal)))
  (:method (label (meta-nodes cons))
	   (cdr (assoc label meta-nodes)))
  (:method ((label string) (meta-node rule+meta-node))
	   (when (string-equal (keim~name meta-node) label)
	     meta-node))
  (:method ((label symbol) (meta-node rule+meta-node))
	   (when (string-equal (keim~name meta-node) label)
	     meta-node))
  (:method (label (meta-node rule+meta-node))
	   (when (equal (keim~name meta-node) label)
	     meta-node))
  (:method (label meta-nodes)
	   (declare (ignore label))
	   (error "~A must be a list of meta-nodes or a meta-node." meta-nodes)))
	   


(defun rule=parameter-p (thing)
  (declare (edited  "16-JUN-1997")
	   (authors Lassaad)
	   (input   "Anything.")
	   (effect  "None.")
	   (value   "T, iff the argument is a rule=parameter."))
  (typep thing 'rule=parameter))

(defmethod print-object ((meta-node rule+meta-node) stream)
  (format stream 
	  (if *print-escape* "#<rule+meta-node ~A>" "~A")
	  (keim~name meta-node)))


#{\subsection{Rules}
Rules are instances of the class RULE+RULE, which has many slots, as seen
by the access functions below.  Each rule has a name (case insensitive). 
The parameters the rule expects, with their Lisp types (for error 
checking), and with a help string for each parameter are also stored.
Below we'll show the syntax of a rule definition.
#}

(eval-when (load compile eval) 
(defclass rule+rule (keim+name)
  ((premises :accessor rule~premises
	     :initarg :premises
	     :documentation "Premises of a rule.")
   (conclusion :accessor rule~conclusion
	       :initarg :conclusion
	       :documentation "Conclusion of a rule.")
   (extra-hyps :accessor rule~extra-hyps
	       :initarg :extra-hyps
	       :documentation "Additional hypotheses for a rule.")
   (parameters :accessor rule~parameters
	       :initarg :parameters
	       :documentation "Parameters of a rule.")
   (computations :accessor rule~computations
		 :initarg :computations
		 :documentation "Computations to be done when applying the rule.")
   (sideconds :accessor rule~sideconds
	      :initarg :sideconds
	      :documentation "Sideconditions of a rule (predicates that must be true).")
   (actions :accessor rule~actions
	    :initarg :actions
	    :documentation "Actions of a rule (changes in support structure).")
   (description :accessor rule~description
		:initarg :desc
		:documentation "Short description of the rule.")
   (theory :initarg :theory  
	   :accessor rule~theory
	   :documentation "The theory in which the rule is defined.")
   (inference :initarg :inference
	      :accessor rule~inference
	      :documentation "The inference-rule to which the rule is assigned.")
   (type-variables :initarg :type-variables
                   :accessor rule~type-variables
                   :documentation "A list of type-variables that is defined for the rule. (THIS IS SOME IMPLICIT KAPPA NOTATION FOR THE RULE!!!!")
   (env :accessor rule~env
	:initarg :env
	:documentation "Environment used at time of definition.")
   )
  (:documentation "A natural deduction rule.")))

(defun rule~rule-p (thing)
  (declare (authors nesmith)
	   (input "Anything")
	   (value "T if the argument is a rule+rule, otherwise NIL."))
  (typep thing 'rule+rule))

(defvar rule*rule-hash-table (make-hash-table :test #'equal)
  "A hash table to map rule names to the rule objects themselves.")

(defun rule~find-rule (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a rule (symbol or string).")
           (effect  "None.")
           (value   "The rule object with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   rule*rule-hash-table))


(eval-when (load compile eval)
(defvar rule*verbose nil "If nil, warnings will be suppressed, 
otherwise they will be printed.")
)

;;;;;;;; BEGIN
;;; rule~apply rule outlines parameters:
;; rule=match rule outlines parameters
;; rule=check-sideconds sideconds subst
;; rule=do-computations computations subst
;; rule=match-extra-hyps extra-hyps rule-premises subst
;;        - Ermittle die Formel von extra-hyp    
;;        - Ist extra-hyp in einer rule-prem, die gebunden ist 
;;           dann suche nach einem hyp von pdsn-node, deren Formel die Formel von extra-hyp gleicht.
;;           Sonst, wird eine neue Hyp erzeugt.
;; rule=check-and-complete-outlines
;;        - Ist Conc ungleich NIL:
;;             rule=check-and-complete-premises conc conc-hyps rule-prems prems subst
;;                   (i) prem = NIL, dann erzeuge einen offenen Knoten mit conc-hyps U extra-hyp
;;                                   und fuege den in subst
;;                   (ii) prem ungleich NIL, dann checke ab, dass hyps(prem) ohne extra-hyp eine
;;                                           Untermenge von conc-hyps und dass conc nicht in
;;                                           justifying of prem ist.
;;             Conc ist offen, dann rechtfertige den mit rule~inference params premises
;;        - Ist Conc = NIL:
;;             bestimme aus den existierenden Hyps eine conc-hyps
;;             rule=complete-premises conc-hyps rule-prems prems subst
;;             erzeuge die Konklusion und deren Justification
;; rule=carry-out-actions rule-actions subst

(defun rule~apply (rulename outlines parameters &optional verbose &key purpose)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A rule name,  a possibly incomplete list of nodes, i.e., can"
		    "contain NILs, a list of parameters, optionally VERBOSE argument"
		    "(defaults to T), and a key word designating the purpose of"
		    "interpreting the rule.")
	   (effect  "Applies the rule with the given arguments and changes the"
		    "current PDS according to the stated PURPOSE.")
	   (value   "If carried out, returns according to the key PURPOSE:"
		    "- :apply, three values: T, a list of all involved nodes"
		    "(Conc Prem1 .. Premn Hyp1 .. Hypm), and the list of new nodes."
		    "- :expand, two values: T, a list of all involved nodes:"
		    " (Conc Prem1 .. Premn Hyp1 .. Hypm)."
		    "- :test, T."
		    "If matching failed, returns nil."))
  (let ((rule (rule~find-rule rulename))
	(*print-gensym* nil))
    (unless rule
      (return-from rule~apply (omega~error ";;; No such rule: ~A~%" rulename)))
    (when (and (eql purpose :apply)
	       (pdsn~p (first outlines))
	       (let ((just (node~justification (first outlines))))
		 (not (pdsj~open-just-p just))))
      (return-from rule~apply (omega~error ";;; Already closed conclusion: ~A~%" (first outlines))))
    (multiple-value-bind (subst mapping)
	;;; First, match the rule outlines and the rule parameters to the given
	;; outlines and parameters respectively.
	(rule=match rule outlines parameters)
      (when (and subst
		 (rule=check-sideconds (rule~sideconds rule)
				       subst mapping purpose))
	;;; The sideconditions can be successfully evaluated, carry out the rule
	;; computations.
	(let ((subst2 (rule=do-computations (rule~computations rule)
					    subst
					    mapping
                                            (rule~type-variables rule))))
	  (when subst2
	    ;;; Match and create extra hypotheses, then check and complete the
	    ;; outlines.
	    (let* ((outline-conc (first outlines))
		   (rule-prems (rule~premises rule))
		   (rule-xtrahyps (rule~extra-hyps rule))
		   (mapping2 (rule=match-extra-hyps rule-xtrahyps rule-prems
						    outline-conc subst2 mapping)))
	      (when mapping2
		(let* ((rule-conc (rule~conclusion rule))
		       (rule-params (rule~parameters rule))
		       (outline-prems (rest outlines))
		       (mapping3 (rule=check-and-complete-outlines (rule~inference rule)
								   outline-conc rule-conc
								   outline-prems rule-prems
								   rule-params subst2 mapping2 purpose)))
		  (when mapping3
		    (cond ((eql purpose :apply)
			   (if outline-conc
			       ;;; Backward application of RULE
			       (rule=do-backward-application outline-conc
							     (remove-if #' null outline-prems)
							     (mapcar #'(lambda (rxhyp)
									 (mapp~get-component (keim~name rxhyp)
											     mapping3))
								     rule-xtrahyps)
							     (rule~actions rule)
							     mapping3 verbose)
			     ;;; Forward application of RULE
			     (rule=do-forward-application (mapp~get-component (keim~name rule-conc)
									      mapping3)
							  (remove-if #' null outline-prems)
							  (mapcar #'(lambda (rxhyp)
								      (mapp~get-component (keim~name rxhyp)
											  mapping3))
								  rule-xtrahyps)
							  (rule~actions rule)
							  mapping3 verbose)))
			  ((eql purpose :expand)
			   (if outline-conc
			       ;;; Backward application of RULE
			       (rule=do-backward-expansion outline-conc
							   (remove-if #' null outline-prems)
							   (mapcar #'(lambda (rxhyp)
								       (mapp~get-component (keim~name rxhyp)
											   mapping3))
								   rule-xtrahyps)
							   verbose)
			     ;;; Forward application of RULE
			     (rule=do-forward-expansion (mapp~get-component (keim~name rule-conc)
									    mapping3)
							(remove-if #' null outline-prems)
							(mapcar #'(lambda (rxhyp)
								    (mapp~get-component (keim~name rxhyp)
											mapping3))
								rule-xtrahyps)
							verbose)))
			  ((eql purpose :test)
			   t)
			  (t
			   (return-from rule~apply (omega~error ";;; Unknown purpose ~A" purpose))))))))))))
    ))

(defun rule=do-backward-application (conclusion old-prems extra-hyps rule-actions mapping verbose)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A conclusion, a list of given premises, a list of extra hypotheses, a"
		    "list of rule actions, a mapping, and a boolean stating whether to show"
		    "new inserted and changed nodes.")
	   (effect  "Inserting some control information in the PDS and applying the given"
		    "rule actions.")
	   (value   "A triple: T, a list of new and modified nodes, and the conclusion."))
  (let* ((conc-just (node~justification conclusion))
	 (conc-prems (just~premises conc-just))
	 (new-prems (set-difference conc-prems old-prems))
	 (new-hyps (remove-if #'(lambda (xtrahyp)
				  (some #'(lambda (prem)
					    (find xtrahyp (pdsn~hyps prem)))
					old-prems))
			      extra-hyps))
	 (sorted-hyps)
	 (new-nodes (append new-prems new-hyps))
	 (conc-sponsors (pdsj~sponsors conc-just))
	 (conc-unsponsors (pdsj~unsponsors conc-just)))
    ;;; Inheritance of the supports to the premises
    (dolist (subgoal new-prems)
      (let ((subgoal-just (node~justification subgoal)))
	(setf (pdsj~sponsors subgoal-just) conc-sponsors)
	(setf (pdsj~unsponsors subgoal-just) conc-unsponsors)))
    (dolist (subgoal (remove-if-not #'pdsn~open-node-p old-prems))
      (let ((subgoal-just (node~justification subgoal)))
	(setf (pdsj~sponsors subgoal-just)
	      (remove subgoal (union (pdsj~sponsors subgoal-just) conc-sponsors)))
	(setf (pdsj~unsponsors subgoal-just)
	      (union (pdsj~unsponsors subgoal-just) conc-unsponsors))))
    ;;; Changing the open node list of PDS
    (setf (pds~open-nodes omega*current-proof-plan)
	  (append new-prems
		  (remove conclusion (pds~open-nodes omega*current-proof-plan))))
    ;; Inserting this rule application as the last plan step in the PDS and
    ;; the corresponding reason into the involved nodes: 
    (let ((reason (pds~change-last-plan-step! conclusion)))
      (dolist (node (append conc-prems (cons conclusion extra-hyps)))
	(pdsn~insert-reason! node reason)))
    ;; Carry out the rule actions 
    (rule=carry-out-actions rule-actions mapping)
    ;; Add any new lines to the proof at appropriate places
    (rule=add-new-nodes-to-proof new-nodes (list conclusion))
    ;;; Sort the new hyps:
    (dolist (prem conc-prems)
      (let ((hyp (find-if #'(lambda (ph) (find ph extra-hyps))
			  (pdsn~hyps prem))))
	(when hyp
	  (setq sorted-hyps
		(append sorted-hyps (list hyp))))))
    ;; Show the new lines as well as modified lines we just created, if desired
    (when verbose
      (dolist (line (append new-nodes (list conclusion)))
	(pds~show-node line)))
    (values t (cons conclusion (append conc-prems sorted-hyps)) new-nodes)
    ))

(defun rule=do-forward-application (conclusion old-prems extra-hyps rule-actions mapping verbose)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A conclusion, a list of given premises, a list of extra hypotheses, a"
		    "list of rule actions, a mapping, and a boolean stating whether to show"
		    "new inserted and changed nodes.")
	   (effect  "Inserting some control information in the PDS and applying the given"
		    "rule actions.")
	   (value   "A triple: T, a list of new and modified nodes, and the conclusion."))
  (let* ((conc-just (node~justification conclusion))
	 (conc-prems (just~premises conc-just))
	 (new-prems (set-difference conc-prems old-prems))
	 (new-hyps (remove-if #'(lambda (xtrahyp)
				  (some #'(lambda (prem)
					    (find xtrahyp (pdsn~hyps prem)))
					old-prems))
			      extra-hyps))
	 (sorted-hyps)
	 (new-nodes (append new-prems new-hyps (list conclusion))))
    (if new-prems
	;;; with new subgoals
	(multiple-value-bind (prems-sponsors prems-unsponsors)
	    (rule=sponsors-unsponsors (remove-if-not #'(lambda (open)
							 (subsetp old-prems
								  (pds~node-supports open omega*current-proof-plan)))
						     (pds~open-nodes omega*current-proof-plan)))
	  
;	  ;;; Changing the open nodes                    
;          (setf (pds~open-nodes omega*current-proof-plan)
;                (append new-prems (pds~open-nodes omega*current-proof-plan)))
	  ;;; LC: delayed after applying the rule actions in order to prevent that
	  ;; open premises possibly get the deduced conclusion as support
	  ;;; Inheritance of the supports to the new premises
	  (dolist (subgoal new-prems)
	    (let ((subgoal-just (node~justification subgoal)))
	      (setf (pdsj~sponsors subgoal-just) prems-sponsors)
	      (setf (pdsj~unsponsors subgoal-just) prems-unsponsors))))
      ;;; without any new subgoal:
      (when (subsetp conc-prems (pds~support-nodes omega*current-proof-plan))
	;; Each goal premise belongs to the head state (the support nodes of
	;; the hole PDS):
	(setf (pds~support-nodes omega*current-proof-plan)
	      (cons conclusion (pds~support-nodes omega*current-proof-plan)))))
    ;; Inserting this rule application as the last plan step in the PDS and
    ;; the corresponding reason into the involved nodes: 
    (let ((reason (pds~change-last-plan-step! conclusion)))
      (dolist (node (append conc-prems (cons conclusion extra-hyps)))
	(pdsn~insert-reason! node reason)))
    ;; Carry out the rule actions 
    (rule=carry-out-actions rule-actions mapping)
    ;;; Changing the open nodes when there is open premises:
    (when new-prems
      (setf (pds~open-nodes omega*current-proof-plan)
	    (append new-prems (pds~open-nodes omega*current-proof-plan))))
    ;; Add any new lines to the proof at appropriate places
    (rule=add-new-nodes-to-proof new-nodes nil)
    ;;; Sort the new hyps:
    (dolist (prem conc-prems)
      (let ((hyp (find-if #'(lambda (ph) (find ph extra-hyps))
			  (pdsn~hyps prem))))
	(when hyp
	  (setq sorted-hyps
		(append sorted-hyps (list hyp))))))
    ;; Show the new lines as well as modified lines we just created, if desired
    (when verbose (dolist (line new-nodes)
		    (pds~show-node line)))
    (values t (cons conclusion (append conc-prems sorted-hyps)) new-nodes)
    ))

(defun rule=sponsors-unsponsors (nodes)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A node list.")
	   (effect  "None.")
	   (value   "A pair: the union of NODES sponsors, and the intersection"
		    "of NODES unsponsors."))
  (when nodes
    (let ((node-just (node~justification (first nodes))))
      (rule=sps-unsps (rest nodes)
		      (pdsj~sponsors node-just)
		      (pdsj~unsponsors node-just)))))

(defun rule=sps-unsps (nodes sps unsps)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "Three node lists.")
	   (effect  "None.")
	   (value   "A pair: the union of NODES sponsors with SPS, and the intersection"
		    "of NODES unsponsors with UNSPS."))
  (if nodes
      (let ((node-just (node~justification (first nodes))))
	(rule=sps-unsps (rest nodes)
			(union (pdsj~sponsors node-just) sps)
			(intersection (pdsj~unsponsors node-just) unsps)))
    (values sps unsps)))
	
(defun rule=do-backward-expansion (conclusion old-prems extra-hyps verbose)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A conclusion, a list of given premises, a list of extra"
		    "hypotheses, and a boolean stating whether to show new"
		    "inserted and changed nodes.")
	   (effect  "Inserting the new nodes in the PDS.")
	   (value   "A pair: T, a list of all involved nodes:"
		    "(Conc Prem1 .. Premn Hyp1 .. Hypm)."))
  (let* ((conc-just (node~justification conclusion))
	 (conc-prems (just~premises conc-just))
	 (new-prems (set-difference conc-prems old-prems))
	 (new-hyps (remove-if #'(lambda (xtrahyp)
				  (some #'(lambda (prem)
					    (find xtrahyp (pdsn~hyps prem)))
					old-prems))
			      extra-hyps))
	 (new-nodes (append new-prems new-hyps))
	 (sorted-hyps))
    ;;; Add any new lines to the proof at appropriate places
    (rule=add-new-nodes-to-proof new-nodes (list conclusion))
    ;;; Show the new lines as well as modified lines we just created, if desired
    (when verbose
      (dolist (line (append new-nodes (list conclusion)))
	(pds~show-node line)))
    ;;; Sort the extra hypotheses
    (dolist (prem conc-prems)
      (let ((hyp (find-if #'(lambda (ph) (find ph extra-hyps))
			  (pdsn~hyps prem))))
	(when hyp
	  (setq sorted-hyps
		(append sorted-hyps (list hyp))))))
    ;;; Return a pair
    (values t (cons conclusion (append conc-prems sorted-hyps)))
    ))

(defun rule=do-forward-expansion (conclusion old-prems extra-hyps verbose)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A conclusion, a list of given premises, a list of extra"
		    "hypotheses, and a boolean stating whether to show new"
		    "inserted and changed nodes.")
	   (effect  "Inserting the new nodes in the PDS.")
	   (value   "A pair: T, a list of all involved nodes:"
		    "(Conc Prem1 .. Premn Hyp1 .. Hypm)."))
  (let* ((conc-just (node~justification conclusion))
	 (conc-prems (just~premises conc-just))
	 (new-prems (set-difference conc-prems old-prems))
	 (new-hyps (remove-if #'(lambda (xtrahyp)
				  (some #'(lambda (prem)
					    (find xtrahyp (pdsn~hyps prem)))
					old-prems))
			      extra-hyps))
	 (new-nodes (append new-prems new-hyps (list conclusion)))
	 (sorted-hyps nil))
    ;;; Add any new lines to the proof at appropriate places
    (rule=add-new-nodes-to-proof new-nodes nil)
    ;;; Show the new lines as well as modified lines we just created, if desired
    (when verbose
      (dolist (line new-nodes) (pds~show-node line)))
    ;;; Sort the extra hypotheses
    (dolist (prem conc-prems)
      (let ((hyp (find-if #'(lambda (ph) (find ph extra-hyps))
			  (pdsn~hyps prem))))
	(when hyp
	  (setq sorted-hyps
		(append sorted-hyps (list hyp))))))
    ;;; Return a pair
    (values t (cons conclusion (append conc-prems sorted-hyps)))
    ))		      

(defun rule=match (rule outlines parameters)
  (declare (edited  "17-FEB-1998")
	   (authors Lassaad)
	   (input   "A rule, an outline list, and a parameter list. An outline is"
		    "either a node or NIL.")
	   (effect  "None.")
	   (value   "A pair consisting of a substitution (of some rule meta-variables)"
		    "and a mapping (of other rule objects), if RULE matches the OUTLINES"
		    "and the PARAMETERS. Otherwise, NIL."))
  (multiple-value-bind (subst mapping)
      (rule=bind-nodes (cons (rule~conclusion rule) (rule~premises rule))
		       outlines
                       (subst~create nil nil) ;; data*global-type-var-subst 
                       (mapp~create nil nil)
                       (rule~type-variables rule))
      ;;; Binding some rule outlines to the given non-NIL outlines and matching the formulas
    (when subst
      (rule=bind-parameters (rule~parameters rule) parameters subst mapping (rule~type-variables rule)))
    ;;; Binding the rule parameters to the given parameters and checking their types.
    ))
    

(defun rule=bind-nodes (meta-nodes outlines &optional (the-subst (subst~create nil nil))
				                      (mapping (mapp~create nil nil))
                                                      (add-bind nil))
  (declare (edited  "17-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of meta-nodes, an outline list, a substitution, and"
		    "a mapping. An outline is either a PDSN+NODE or NIL.")
	   (effect  "None.")
	   (value   "A pair: An extention of SUBST and an extension of MAPPING"
		    "resulted from matching the META-NODES to the OUTLINES, if"
		    "this is possible. Otherwise, NIL."))
  (if meta-nodes
      (if outlines
	  (let ((outline (first outlines)))
	    (if outline
		(let* ((meta-node (first meta-nodes))
		       (new-subst (term~alpha-match (node~formula meta-node)
						    (node~formula outline)
						    :subst the-subst)))
		    (if new-subst
			(rule=bind-nodes (rest meta-nodes)
					 (rest outlines)
					 new-subst
					 (mapp~insert-component! (keim~name meta-node)
								 outline
								 mapping)
                                         add-bind)
		      (omega~error ";;; Node ~A does not match the associated rule outline.~%" (keim~name outline))))
	      (rule=bind-nodes (rest meta-nodes) (rest outlines) the-subst mapping add-bind)))
	(omega~error ";;; Incorrect number of outlines received.~%"))
    (if outlines
	(omega~error ";;; Incorrect number of outlines received.~%")
      (values the-subst mapping))
    ))

;; Ameier:
;; Ich habe hier eine folgende Erweiterung reingebaut:
;; falls ein Rule-Parameter als Term spezifiziert ist und der input-paramter ein Schema-ist, dann wird versucht mittels
;; unification des Typs des Rule-Parameters und des Typs des input-parameters ein instantiieung dieser Kappas zu berechnen.
;; Ist dies moeglich, d.h. enthaelt der entstehende Term keine freien Typvariablen mehr, so wird dieser Term als
;; input parameter benutzt.
;; Beispiel: 1.) Foralle auf eine Zeile (forall (lam (x:i) (P x))) wird der parameter (Kappa aa t:aa) reingesteckt.
;;               Unifiziert man nun zwischen x:i und t:aa die Typen, so ergibt sich, dass aa -> i
;;               dementsprchend wird dann direkt t:i eingesetzt.
;;           2.) Foralle auf eine Zeile (forall (lam (x:o) (P x))) wird der Parameter
;;               (Kappa aa (=:(o (o aa aa) (o aa aa)) =:(o aa aa) =:(o aa aa)):o reingesteckt. Unifiziert man nun die Typen
;;               der Terme, so erhaelt man das Unifikations-Problem o=o, was bereits geloest ist. Man erhaelt also keine
;;               Instantiierung fuer aa!
;; Parameter die sich nicht ground-typisch instantiieren lassen werden zurueckgewiesen! (Also im Klartext die Regel wird nicht
;; angewandt).
;;
;; Alternative: (NICHT implementiert!)
;; Was in einem solchen Fall vielleicht auch moeglich waere ist, dass verbleibende freie Kappas nach aussen gezogen werden!
;; ALso in obigem BEispiel am Schluss (kappa aa (P (=:(o (o aa aa) (o aa aa)) =:(o aa aa) =:(o aa aa)):o)) herauskommt.


(defun rule=bind-parameters (rule-parameters parameters the-subst mapping add-bind)
  (declare (edited  "17-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of rule-parameters, a list of parameters, a"
		    "substitution, and a mapping.")
	   (effect  "None.")
	   (value   "A pair: An extension of SUBST and an extension of MAPPING"
		    "resulted from matching each RULE-PARAMETER to the associated"
		    "PARAMETER, if the latter is an instance of the class designating"
		    "the sort of the former and both have compatible types."
		    "Otherwise, NIL."))
  (if rule-parameters
      (if parameters
	  (let ((param (first parameters))
		(rule-param (first rule-parameters)))
	    (if (cond ((string-equal 'term+schematic (rule=parameter-type rule-param))
		       (typep param 'term+term))
		      ;; kleiner HAck von AMEIER!
		      ;; Problem, man kann Terme als Parameter haben, die gegrounded werden sollen/muessen-> term+term
		      ;; und solche die eben nicht gegrounded werden muessen/sollen
		      ;; diese bezeichnet man einfach bei der rule~defrule als term+schematic
		      ;; damit werden sie dann einfach ebenfalls auf term+term getestet, aber spaeter nicht mehr gegrounded!
		      (t
		       (typep param (rule=parameter-type rule-param))))
		(let ((param-var (keim~name rule-param)))
		  (if (meta~p param-var)
		      (cond ((and (string-equal 'term+term (rule=parameter-type rule-param))
				  (data~schema-p param))
			     ;; Parameter ist term Parameter
			     ;; -> Falls reinkommender Parameter ein Kappa-Parameter ist
			     ;;    dann ground-substituiere den reinkommenden Parameter
			     (let* ((rule-param-var-type (subst~apply the-subst (term~type param-var)))
				    (param-range (data~alpha-copy (data~schema-range param) nil))
				    (param-domain (data~schema-domain param))
				    (new-subst-pre (type~alpha-equal (subst~apply the-subst rule-param-var-type)
								     (term~type param-range)
								     (cons rule-param-var-type param-domain))))
			       
			       (if new-subst-pre
				   (let* ((grounded-check (every #'(lambda (var)
								     (let* ((substed (subst~apply new-subst-pre var)))
								       (null (type~type-variables substed))))
								 param-domain)))
				     (if grounded-check
					 (let* ((new-subst (term~alpha-match param-var (subst~apply new-subst-pre param-range)
									     :subst the-subst)))
					   (if new-subst
					       (rule=bind-parameters (rest rule-parameters)
								     (rest parameters)
								     new-subst
								     mapping
								     add-bind)
					     (omega~error ";;; Parameter ~A does not match the associated parameter.~%" param)))
				       (omega~error ";;; Not able to ground schematic input parameter ~A.~%" param)))
				 (omega~error ";;; Parameter ~A does not match the associated parameter.~%" param))))
			    (t
			     (let* ((new-subst (term~alpha-match param-var param
								 :subst the-subst)))
			       (if new-subst
				   (rule=bind-parameters (rest rule-parameters)
							 (rest parameters)
							 new-subst
							 mapping
							 add-bind)
				 (omega~error ";;; Parameter ~A does not match the associated parameter.~%" param)))))
		    (rule=bind-parameters (rest rule-parameters)
					  (rest parameters)
					  the-subst
					  (mapp~insert-component! param-var param mapping)
                                          add-bind)))
	      (omega~error ";;; Parameter ~A does not match the associated parameter.~%" param)))
	(omega~error ";;; Incorrect number of parameters received.~%"))
    (if parameters
	(omega~error ";;; Incorrect number of parameters received.~%")
      (values the-subst mapping))
    ))

(defun rule=check-sideconds (sideconds subst mapping purpose)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of side conditions, a substitution, a mapping,"
		    "and a purpose why evaluating the associated rule.")
	   (effect  "None.")
	   (value   "The result of evaluating SIDECONDS with the given SUBST"
		    "and MAPPING."))
  (let ((relevant-sideconds
	 (if (eql purpose :apply)
	     sideconds
	   (if (find purpose (list :expand :test) :test #'eql)
	       (remove-if #'(lambda (sidecond)
			      (equal (car sidecond) 'pds~node-supported-by-p))
			  sideconds)
	     (omega~error ";;;RULE=MATCH-SIDECONDS: unknown purpose key word ~A"
			 purpose)))))
    (rule=evaluate-sideconds relevant-sideconds subst mapping)
    ))

(defun rule=evaluate-sideconds (sideconds subst mapping)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of side conditions, a substitution, and a mapping.")
	   (effect  "None.")
	   (value   "T, when SIDECONDS can be successfully evaluated with the"
		    "settings in SUBST and MAPPING, otherwise NIL."))
  (if sideconds
      (let* ((sidecond (first sideconds))
	     (lisp-expr (cons (first sidecond)
			      (mapcar #'(lambda (arg)
					  (rule=associated-object arg subst mapping))
				      (rest sidecond)))))
	(if (apply (car lisp-expr) (cdr lisp-expr))
	    (rule=evaluate-sideconds (rest sideconds) subst mapping)
	  (omega~error ";;; Following sidecondition is not fulfilled: ~A~%"
		     lisp-expr)))
    T))

(defgeneric rule=associated-object (rule-object subst mapping)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A rule object, a substitution, and a mapping.")
	   (effect  "None.")
	   (value   "The associated object for RULE-OBJECT in SUBST and MAPPING."))
  (:method ((rule-param rule=parameter) (subst subst+substitution) (mapping mapp+mapping))
	   (let ((param-name (keim~name rule-param)))
	     (or (subst~get-component param-name subst)
		 (mapp~get-component param-name mapping))))
  (:method ((meta-node rule+meta-node) (subst subst+substitution) (mapping mapp+mapping))
	   (let ((node-name (keim~name meta-node)))
	     (or (subst~get-component node-name subst)
		 (mapp~get-component node-name mapping))))
  (:method ((rule-object T) (subst subst+substitution) (mapping mapp+mapping))
	   (or (subst~get-component rule-object subst)
	       (mapp~get-component rule-object mapping))))
		   
(defun rule=do-computations (computations the-subst mapping add-bind)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of computations, a substitution, and a mapping,"
		    "where a computation consists of a meta-variable and a lisp"
		    "expression whose arguments are meta-variables.")
	   (effect  "None.")
	   (value   "An extension of SUBST resulted from matching the computations"
		    "meta-variables to the results returned by the lisp expressions."))
  (setq comp computations)
  (setq subst the-subst)
  (setq map mapping)
  (if computations
      (let* ((computation (first computations))
	     (meta-expr (second computation))
	     (lisp-expr (cons (first meta-expr)
			      (mapcar #'(lambda (arg)
					  (rule=associated-object arg the-subst mapping))
				      (rest meta-expr))))
	     (new-subst (term~alpha-match (first computation) (apply (car lisp-expr) (cdr lisp-expr))
					  :subst the-subst)))
	  (if new-subst
	      (rule=do-computations (rest computations) new-subst mapping add-bind)
	    (omega~error ";;; Inconsistent binding of ~A to the result of ~A~%"
			 (first computation) lisp-expr)))
    the-subst))

(defun rule=match-extra-hyps (extra-hyps rule-premises the-conc subst mapping)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "Two lists of rule lines (extra hypotheses, and the rule"
		    "premises), the conclusion to which the rule is applied,"
		    "a substitution of the rule meta-variables, and a mapping"
		    "of rule-objects to pds-objects.")
	   (effect  "None.")
	   (value   "An extension of MAPPING resulted from determining the"
		    "object nodes associated to the given extra hypotheses."))
  (if extra-hyps
      (let* ((extra-hyp (first extra-hyps))
	     (some-rule-prems (remove-if-not #'(lambda (rule-prem)
						 (find extra-hyp (pdsn~hyps rule-prem)))
					     rule-premises)))
	(if some-rule-prems
	    (let ((xhyp-formula (rule=subst-apply subst (node~formula extra-hyp)))
		  (some-prems (apply #'append
				     (mapcar #'(lambda (rule-prem)
						 (let ((prem (mapp~get-component
							      (keim~name rule-prem)
							      mapping)))
						   (when prem (list prem))))
					     some-rule-prems))))
	      (if some-prems
		  ;;; EXTRA-HYP must be one hypothesis of SOME-PREM:
		  ;; Get the hypotheses of SOME-PREM whose formula corresponds to XHYP-FORMULA;
		  ;; Is there just one hypothesis, then bind EXTRA-HYP to it.
		  ;; Is there no hypothesis, then signal a OMEGA~ERROR
		  ;; Is there more than one hypothesis, then ignore those which are hypotheses of
		  ;; the conclusion too.
		  (let ((the-hyps
			 (funcall #'rule=intersection
				(mapcar #'(lambda (some-prem)
					    (remove-if-not #'(lambda (hyp)
							       (term~alpha-equal
								(node~formula hyp)
								xhyp-formula))
							   (pdsn~hyps some-prem)))
						   some-prems))))
		    (if the-hyps
			(if (rest the-hyps)
			    (if the-conc
				(let ((rest-hyps (set-difference the-hyps
								 (pdsn~hyps the-conc))))
				  (if (or (not rest-hyps) (rest rest-hyps))
				      (omega~error ";;; The hypotheses of ~A and ~A are not compatible.~%"
						  the-conc (first some-prems))
				    (rule=match-extra-hyps (rest extra-hyps)
							   rule-premises
							   the-conc
							   subst
							   (mapp~insert-component! (keim~name extra-hyp)
										   (first rest-hyps)
										   mapping))))
			      (omega~error ";;; Inconsistent rule application!~%"))
			  (rule=match-extra-hyps (rest extra-hyps)
						 rule-premises
						 the-conc
						 subst
						 (mapp~insert-component! (keim~name extra-hyp)
									 (first the-hyps)
									 mapping)))
		      (omega~error ";;; A node in ~A has no appropriate extra hypothesis.~%"
				  some-prems)))
		;;; EXTRA-HYP must be newly created.
		(rule=match-extra-hyps (rest extra-hyps)
				       rule-premises
				       the-conc
				       subst
				       (mapp~insert-component! (keim~name extra-hyp)
							       (pdsn~make-hypothesis
								xhyp-formula
								(rule=clever-labelling))
							       mapping))))
	  (omega~error ";;; Extra hypothesis ~A does not occur in any rule premise!~%"
		      extra-hyp)))
    mapping))

(defun rule=intersection (lists)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A list of lists.")
	   (effect  "none.")
	   (value   "The intersection of the LISTS elements."))
  (when lists
    (let ((list1 (first lists)))
      (if (rest lists)
	  (intersection list1
			(rule=intersection (rest lists)))
	list1))))

(defun rule=check-and-complete-outlines (inference outline-conc rule-conc outline-prems
					 rule-prems rule-params subst mapping purpose)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "An inference (ND rule), a conclusion outline, a rule conclusion,"
		    "a premise outline list, a list of rule premises, a list of rule"
		    "parameters, a substitution, a mapping, and a key word.")
	   (effect  "When PURPOSE is :APPLY or :EXPAND and OUTLINE-CONC is not NIL, the"
		    "open justification of OUTLINE-CONC is replaced by a closed justification"
		    "which corresponds to the application of the given INFERENCE.")
	   (value   "An extension of MAPPING resulted from completing the outlines, when"
		    "it is guaranteed that no one of the conclusion justifying nodes"
		    "corresponds to the conclusion itself and the non-extra hypotheses of"
		    "the premises are subsets of the conclusion hypotheses. Otherwise, NIL."))
  (if outline-conc
      (let ((given-prems (remove-if #'null outline-prems)))
	(if (find outline-conc 
		  (pdsn~justifying-nodes given-prems))
	    (omega~error ";;; Node ~A would directly or indirectly justify itself.~%" outline-conc)
	  (let ((new-mapping (rule=check-and-complete-premises (pdsn~hyps outline-conc)
							       outline-prems
							       rule-prems
							       subst mapping purpose)))
	    (when new-mapping
	      (when (or (eql purpose :apply) (eql purpose :expand))
		(let* ((conc-just (node~justification outline-conc))
		       (conc-just-ctrl (pdsj~control conc-just))
		       (new-conc-just (pdsj~closed-just-create
				       inference
				       (mapcar #'(lambda (rule-prem)
						   (mapp~get-component (keim~name rule-prem)
								       new-mapping))
					       rule-prems)
				       (mapcar #'(lambda (rule-param)
						   (rule=associated-object rule-param subst new-mapping))
					       rule-params)
				       "grounded")))
		  (setf (pdsj~control new-conc-just)
			conc-just-ctrl
			(node~justification outline-conc)
			(pdsj~replace-justification! conc-just new-conc-just))))
	      new-mapping))))
    (if (or (eql purpose :apply) (eql purpose :expand))
	(let* ((conc-hyps (rule=hyps-of-existent-premises outline-prems rule-prems mapping)) 
	       (new-mapping (rule=complete-premises conc-hyps outline-prems rule-prems subst mapping)))
	  (when new-mapping
	    (mapp~insert-component! (keim~name rule-conc)
				    (pdsn~create (rule=clever-labelling)
						 conc-hyps
						 (rule=subst-apply subst
							      (node~formula rule-conc))
						 (pdsj~closed-just-create
						  inference
						  (mapcar #'(lambda (rule-prem)
							      (mapp~get-component (keim~name rule-prem)
										  new-mapping))
							  rule-prems)
						  (mapcar #'(lambda (rule-param)
							      (rule=associated-object rule-param subst
										      new-mapping))
							  rule-params)
						  "grounded"))
				    new-mapping))
	  new-mapping)
      mapping)))

(defun rule=check-and-complete-premises (conc-hyps premises rule-prems subst mapping purpose)
  (declare (edited  "18-FEB-1998")
	   (authors Lassaad)
	   (input   "A hypothesis list (of the conclusion to which the rule is applied), a"
		    "possibly incomplete premise list, a list of rule premises, a substitution,"
		    "a mapping, and a key word indicating the PURPOSE of this function call.")
	   (effect  "None.")
	   (value   "An extension of MAPPING resulted from creating new premise nodes, when"
		    "the non-extra hypotheses of each existent premise is a subset of CONC-HYPS."
		    "Otherwise, NIL."))
  (if premises
      (let* ((premise (first premises))
	     (rule-prem (first rule-prems))
	     (extra-hyps (pdsn~hyps rule-prem))
             (obj-extrahs (mapcar #'(lambda (extra)
				      (mapp~get-component (keim~name extra) mapping))
                                  extra-hyps))) 
	(if premise
            ;;; Check that the hypotheses of PREMISE without OBJ-EXTRAHS is a subset 
            ;; of CONC-HYPS. 
            (if (subsetp (set-difference (pdsn~hyps premise) obj-extrahs) conc-hyps)
                (rule=check-and-complete-premises conc-hyps (rest premises) 
                                                  (rest rule-prems) subst mapping purpose)
              (omega~error ";;; The hypotheses of premise ~A are incompatible with those of the conclusion.~%" 
                         premise))
          ;;; Create new premise
	  (rule=check-and-complete-premises conc-hyps (rest premises)
					    (rest rule-prems) subst
					    (if (or (eql purpose :apply) (eql purpose :expand))
						(mapp~insert-component!
						 (keim~name rule-prem)
						 (pdsn~create (rule=clever-labelling)
							      (union conc-hyps obj-extrahs)
							      (rule=subst-apply subst
										(node~formula rule-prem))
							      (pdsj~open-just-create))
						 mapping)
					      mapping)
					    purpose))) 
    (if rule-prems
	(omega~error ";;; Incorrect number of premises received.~%") 
      mapping)))
										     
(defun rule=hyps-of-existent-premises (premises rule-prems mapping &optional hyps)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A possibly incomplete list of premises, the rule premises, a"
		    "mapping, and optionally a hypothesis list.")
	   (effect  "Signal an error, when the length of PREMISES does not equal the"
		    "length of RULE-PREMS.")
	   (value   "The union of the non-extra hypotheses of the existent premises."))
  (if premises
      (let ((premise (first premises)))
	(if premise
	    (let* ((rule-prem (first rule-prems))
		   (extra-hyps (pdsn~hyps rule-prem))
		   (obj-extrahs (mapcar #'(lambda (extra)
					    (mapp~get-component (keim~name extra) mapping))
					extra-hyps))) 
	      (rule=hyps-of-existent-premises (rest premises) (rest rule-prems) mapping
					      (union hyps 
                                                     (set-difference (pdsn~hyps premise)
                                                                     obj-extrahs))))
	  (rule=hyps-of-existent-premises (rest premises) (rest rule-prems) mapping hyps)))
    (if rule-prems
        (omega~error ";;; Incorrect number of premises received.~%")
      hyps)))

(defun rule=complete-premises (conc-hyps premises rule-prems subst mapping)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A hypothesis list (of the conclusion to which the rule is applied),"
		    "a possibly incomplete premise list, the rule premises, a substitution,"
		    "and a mapping.")
	   (effect  "None.")
	   (value   "An extension of MAPPING resulted from creating the new premises."))
  (if premises
      (if (first premises)
	  (rule=complete-premises conc-hyps (rest premises) (rest rule-prems) subst mapping)
	(let* ((rule-prem (first rule-prems))
	       (extra-hyps (pdsn~hyps rule-prem))
	       (obj-extrahs (mapcar #'(lambda (extra)
					(mapp~get-component (keim~name extra) mapping))
				    extra-hyps)))
	  (rule=complete-premises conc-hyps (rest premises) (rest rule-prems) subst
				  (mapp~insert-component! (keim~name rule-prem)
							  (pdsn~create (rule=clever-labelling)
								       (union conc-hyps obj-extrahs)
								       (rule=subst-apply 
									subst
									(node~formula rule-prem))
								       (pdsj~open-just-create))
							  mapping))))
    mapping))

(defgeneric rule=subst-apply (subst formula)
  (declare (edited  "19-JAN-1999")
	   (authors Lassaad)
	   (input   "A substitution and a formula.")
	   (effect  "None.")
	   (value   "The result of substituting bounded variables by SUBST"
		    "in FORMULA, lambda-bounded variable in FORMULA must be"
		    ", in contrast to the function subst~apply, substituted"
		    "too."))
  (:method ((subst subst+substitution) (nix null))
	   nix)
  (:method ((subst subst+substitution) (objects cons))
	   (cons (rule=subst-apply subst (first objects))
		 (rule=subst-apply subst (rest objects))))
  (:method ((subst subst+substitution) (primitive data+primitive))
	   (subst~apply subst primitive))
  (:method ((subst subst+substitution) (primitive meta+variable))
	   (let* ((domain (subst~domain subst))
		  (codomain (subst~codomain subst))
		  (assoc-list (mapcar #'list domain codomain)))
	     (second (assoc primitive assoc-list))))	     
  (:method ((subst subst+substitution) (appl data+appl))
	   (data~appl-create (rule=subst-apply subst (data~appl-function appl))
			     (rule=subst-apply subst (data~appl-arguments appl))))
  (:method ((subst subst+substitution) (abstr data+abstr))
	   (data~abstr-create (rule=subst-apply subst (data~abstr-domain abstr))
			      (rule=subst-apply subst (data~abstr-range abstr)))))
  
			     
  
          
(defun rule=carry-out-actions (rule-actions mapping)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "Rule actions, and a mapping.")
	   (effect  "Carrying out the rule actions to change the support"
		    "nodes of the involved nodes.")
	   (value   "Unspecified."))
  (dolist (rule-action rule-actions)
    (rule=carry-out-action rule-action mapping)))

(defun rule=carry-out-action (rule-action mapping)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A rule action (see documentation of rule~~def-rule), and"
		    "a mapping.")
	   (effect  "Carrying the ACTION out.")
	   (value   "Unspecified."))
  (let ((open-nodes (rule=action-open-nodes (first rule-action)
					    mapping))
	(action2function (list (cons 'sponsor 'pds~add-sponsors)
			       (cons 'unsponsor 'pds~delete-sponsors))))
    (dolist (open-node open-nodes)
      (dolist (an-action (rest rule-action))
	(let ((assoc-func (assoc (first an-action)
				 action2function
				 :test #'string-equal)))
	  (if assoc-func
	      (let ((supports (mapcar #'(lambda (node-label)
					  (mapp~get-component node-label
							      mapping))
				      (rest an-action))))
		(if (some #'null supports)
		    (omega~error ";;; Some of the rule lines~{ ~A~} could not be instantiated.~%"
				(rest an-action))
		  (eval (list (rest assoc-func)
			      open-node
			      (cons 'list supports)))))
	    (omega~error ";;; Unknown action ~A.~%" (first an-action))))))
    ))

(defgeneric rule=action-open-nodes (nodes-pattern mapping)
  (declare (edited  "19-FEB-1998")
	   (authors Lassaad)
	   (input   "A NODES-PATTERN of a rule action, and a mapping.")
	   (effect  "None.")
	   (value   "A list of open nodes that are represented by the given"
		    "pattern NODES-PATTERN."))
  (:method ((pattern null) (mapping mapp+mapping))
	   ;(declare (ignore mapping))
	   (pds~open-nodes omega*current-proof-plan))
  (:method ((pattern symbol) (mapping mapp+mapping))
	   (let ((open-node (mapp~get-component pattern mapping)))
	     (if open-node (list open-node)
	       (omega~error ";;; Rule line ~A could not be instantiated.~%"
			   pattern))))
  (:method ((pattern list) (mapping mapp+mapping)) 
	   (let ((key-word (first pattern))
		 (supportings (mapcar #'(lambda (node-label)
					  (mapp~get-component node-label mapping))
				      (rest pattern)))
		 (all-nodes (pds~open-nodes omega*current-proof-plan)))
	     (if (some #'null supportings)
		 (omega~error ";;; Some of the rule lines~{ ~A~} could not be instantiated.~%"
			     (rest pattern))
	       (cond ((string-equal key-word 'support)
		      (remove-if-not #'(lambda (node)
					 (subsetp supportings (pds~node-supports node)))
				     all-nodes))
		   (T
		    (omega~error ";;; Undefined key word (node selector): ~A" key-word))))))
  )
		
		
				    
		
;;;;;; END ;;;;;;;;;;;;;
  

(defmacro rule~defrule (name inference theory &rest attribs) 
  (declare
   (authors Sorge nesmith Lassaad)
   (effect "Parses the definition and creates a new rule (or redefines
 an existing rule with this name).")
   (value "The rule created."))
  `(block defrule
     (let* ((name (quote ,name))
	  (inference (let ((inf (infer~find-method (quote ,inference))))
		       (unless inf (omega~error ";;;RULE~~DEFRULE: The inference rule ~A does not exist" (quote ,inference)))
		       inf))
	  (th-ident (if (string-equal (car (quote ,theory)) :in)
		        (cadr (quote ,theory))
                      (omega~error ";;;RULE~~DEFRULE: A theory lacks for the definition of ~A" name)))
          (theory (if (th~require-only th-ident)
		      (th~find-theory th-ident)
		    (omega~error ";;;RULE~~DEFRULE: The theory ~A does not exist." th-ident)))
          (attribs (quote ,attribs))
	  (parameters) (declarations) (premises) (conclusion) (extra-hyps)
	  (sideconds) (actions) (computations) (desc "")
	  (environment (env~create (th~env theory))))
     (when (and inference th-ident theory)
       (do ((attribs (cdr attribs) (cdr attribs))
	    (attrib (car attribs) (car attribs)))
	   ((and (null attrib) (null attribs)))
	 (if (consp attrib)
	     (cond
	      ((string-equal (car attrib) :declarations)   (setq declarations (cdr attrib)))
	      ((string-equal (car attrib) :premises)       (setq premises (cdr attrib)))
	      ((string-equal (car attrib) :conclusion)     (setq conclusion (cadr attrib)))
	      ((string-equal (car attrib) :extra-hyps)     (setq extra-hyps (cdr attrib)))
	      ((string-equal (car attrib) :parameters)     (setq parameters (cdr attrib)))
	      ((string-equal (car attrib) :computations)   (setq computations (cdr attrib)))
	      ((string-equal (car attrib) :sideconditions) (setq sideconds (cdr attrib)))
	      ((string-equal (car attrib) :actions)        (setq actions (cdr attrib)))
	      ((string-equal (car attrib) :description)    (setq desc (cadr attrib)))
	      (t (return-from defrule (omega~error ";;;RULE~~DEFRULE: Not expecting ~A" (car attrib)))))
	   (return-from defrule (omega~error ";;;RULE~~DEFRULE: Not expecting ~A" attrib))))
       (post~read-object-list declarations environment)
       (let* ((type-vars (mapcar #'(lambda (x) (env~lookup-object x environment))
                                 (cdr (find-if #'(lambda (x) (string-equal (car x) :type-variables)) declarations))))
              (hyp-list (rule=read-extra-hyps extra-hyps environment))
	      (real-prems (mapcar #'(lambda (x) (rule~meta-node-create x hyp-list environment))
				  premises))
	      (real-conc (rule~meta-node-create conclusion hyp-list environment))
	      (real-params (mapcar #'(lambda (x) (rule=make-parameter x environment)) parameters))
              (newrule (make-instance 'rule+rule :name name
                                      :theory theory
                                      :inference inference
                                      :premises real-prems
                                      :conclusion real-conc
                                      :extra-hyps (mapcar #'cdr hyp-list)
                                      :parameters real-params
                                      :sideconds (rule=replace-env-symbols sideconds environment)
                                      :computations (rule=replace-env-symbols computations environment)
                                      :desc desc
                                      :actions actions
                                      :type-variables type-vars
                                      :env environment)))
	 (when (member 'tactics th*output)
	   (if (gethash (symbol-name name)  rule*rule-hash-table)
	       (omega~message ";;; Redefining rule ~A" name)
	     (omega~message ";;; Defining rule ~A" name)))
         (setf (gethash (symbol-name name)  rule*rule-hash-table)
               newrule))))))

(defun rule=read-extra-hyps (extra-hyps environment)
  (declare (edited  "20-FEB-1998")
	   (authors Sorge)
	   (input   "A list of extra hypotheses representations and an environement.")
	   (effect  "Creates meta-lines from the representations.")
	   (value   "A hashtable mapping a symbolic line name to the corresponding meta-line."))
  (let ((hyp-lines))
    (dolist (line extra-hyps)
      (setq hyp-lines
	    (acons (car line)
		   (rule~meta-node-create line hyp-lines environment)
		   hyp-lines)))
    hyp-lines))

(defun rule=replace-env-symbols (expression environment)
  (declare (edited  "02-MAR-1998")
	   (authors Sorge)
	   (input   "An expression consisting of lists of symbols and an environment.")
	   (effect  "None.")
	   (value   "The expression with those symbols replaced, which exist in the environment."))
  (cond ((symbolp expression)
	 (let ((envsymb (env~lookup-object expression environment)))
	   (if envsymb envsymb
	     expression)))
	((listp expression)
	 (cons (rule=replace-env-symbols (car expression) environment)
	       (rule=replace-env-symbols (cdr expression) environment)))
	(t expression)))

(defun rule=make-parameter (parameter environment)
  (if (or (not (listp parameter)) (< (length parameter) 2))
      (error "~A is not the right style for rule parameters." parameter)
    (let ((name (rule=replace-env-symbols (car parameter) environment))
	  (type (cadr parameter))
	  (rest (cddr parameter)))
      (make-instance 'rule=parameter
		     :name name
		     :type type
		     :help (when rest (car rest))))))

(defun rule~describe (rulename)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule name.")
           (effect  "Prints a description of the rule to standard output.")
           (value   "Undefined."))
  (flet ((print-node (line)
		     (omega~message (pds~node2string line))))
    (let ((rule*verbose t)
	  (*print-escape* nil)
	  (*print-gensym* nil)
	  (rule (rule~find-rule rulename)))
      (unless rule
	(omega~warn ";;; RULE~~DESCRIBE: no such rule ~A.~%" rulename)
	(return-from rule~describe nil))
      (with-slots (parameters conclusion premises extra-hyps parameters sideconds actions
			description) rule
	(omega~message "~A: ~A~%" rulename description)
	(when parameters
	  (omega~message "~&Parameters are:")
	  (mapc #'(lambda (x)
		    (omega~message "~A, ~(~A~) of type ~:@(~A~@:)~%" (keim~name x) (help~help-string x) (rule=parameter-type x)))
		parameters))
	(omega~message "~&Premises are:")
	(mapc #'print-node premises)
	(omega~message "~%Conclusion is:")
	(print-node conclusion)
	(when extra-hyps
	  (omega~message "~%Additional hypotheses are:")
	  (mapc #'print-node extra-hyps))
	(when sideconds
	  (omega~message "~%Restrictions are:~%~{~A ~}" sideconds))
	(when actions
	  (omega~message "~%Support structure changes are: ~A" actions))))))


(defun rule=clever-labelling (&optional line)
; want to give this line a label based on what purpose it's serving.
; but put it off for the time being
  (declare (ignore line))
  (pds~new-node-name)
)
 
(defun rule=add-new-nodes-to-proof (new-lines old-lines &optional modus)
  (declare (edited  "08-JUL-1997")
	   (authors nesmith Lassaad)
	   (input   "A list of the new lines created by the rule, as well as the"
		    "old lines involved in the rule application (which could perhaps"
		    "now be justified by one of the new lines. Optionally a boolean:"
		    "T, means new open nodes have to be inserted into the list of"
		    "the PDS open nodes.")
	   (effect  "Adds each of the new lines to the proof before any lines it"
		    "justifies, and after each of the lines which justifies it or"
		    "is a hypothesis of it.")
	   (value   "Unspecified."))
  (dolist (line new-lines)
    (let ((justified-by (pdsn~just-premises line))
	  (justifies (remove-if-not #'(lambda (old-line)
					(member line (pdsn~just-premises
						      old-line)))
				    old-lines))
	  (supports (pds~node-supports line))
	  (hyps (pdsn~hyps line)))
      (if modus
	  (pds~insert-node-after-before! line
					 (append supports justified-by hyps) 
					 justifies)
	(pds~only-insert-node-between! line
				       (append supports justified-by hyps) 
				       justifies)))
    ))


(defvar rule*print-entire-node t)

(defun rule=pprint-node (line)
  (if rule*print-entire-node
      (pprint-logical-block 
       (nil nil)
       (write (symbol-name (keim~name line)) :escape nil)
       (write-char #\space)
       (pprint-logical-block
	(nil (mapcar #'symbol-name
		     (mapcar #'keim~name (pdsn~hyps line)))
	     :prefix "(" :suffix ")")
	(pprint-exit-if-list-exhausted)
	(loop
	 (write (pprint-pop) :escape nil)
	 (pprint-exit-if-list-exhausted)
	 (write-char #\space)
	 (pprint-newline :fill)))
       (write-string " ! ")
       (write (node~formula line)))
      (write (symbol-name (keim~name line))))) 



(pp~modify-style pds-simple
  (rule+meta-node
   (lambda (s l)
     (let ((*standard-output* s)) 
       (rule=pprint-node l)))
   25)
  )

(pp~modify-style pds-pretty
  (rule+meta-node
   (lambda (s l)
     (let ((*standard-output* s)) 
       (rule=pprint-node l)))
   )
  )

(pp~modify-style pds-post
  (rule+meta-node
   (lambda (s l)
     (let ((*standard-output* s)) 
       (rule=pprint-node l)))
   )
  )

;;; inserted by Afiedler
(defmethod infer~theory ((rule infer+rule))
  (rule~theory (rule~find-rule (infer~find-arbitrary-application-name rule))))

(defmethod infer~compute-outline ((rule infer+rule) (outline list) (parameters list))
  (let* ((outline-pattern (infer~compute-outline-pattern outline))
	 (outline-rule (infer~outline-pattern2application rule outline-pattern)))
    (if (infer~closed-pattern-p (car outline-pattern))
	(omega~warn "The rule ~A cannot be applied with a closed conclusion." (keim~name rule))
      (if outline-rule
	  (multiple-value-bind (success complete-outline new-nodes)
	      (rule~apply outline-rule outline parameters rule*verbose :purpose :apply)
	    (declare (ignore new-nodes))
	    (if (and (not success)
		     (not (omega~error "INFER~~COMPUTE-OUTLINE: The application of the rule ~A was not successful."
				       (keim~name rule))))
		(values outline nil)
	      (let ((goal-just (node~justification (car complete-outline))))
		(setf (pdsj~outline-pattern goal-just) outline-pattern)
		(values complete-outline success))))
	(omega~warn ";;; No rule specified for pattern ~A" outline-pattern)))))

#| explanation for rule~defrule
   (input "A name and list of attributes.
Here's an example of the syntax.
\\begin{code}
(rule~def-rule forall-i (in base)
  (arguments D1 X)
  (argumenttypes pdsn+node sym+sym)
  (arghelps \"An universal node\" \"A parameter\")
  (declarations
   (type-variables CC)
   )
  (meta-variables (A (O CC)) (X CC))
  (preconditions
   (D1 (H) () (forall A)))
  (sideconditions (pds~not-free-in-nodes-or-hyps-p X D1))
  (postconditions
   (D2 (H) () (eval (hop~beta-contract (A X))))
   (D1 (H) () (forall A) (\"ForallI\" (X) (D2))))
  (actions)
  (description \"Backward application of the rule ForallI\"))
\\end{code}

Here's a description of the syntax:
\\begin{code}
(rule~def-rule {\\it name} (in {\\it theory})
 (arguments {\\it symbol}+)
 (argumenttypes {\\it type}+)
 (arghelps {\\it string}+)
 (declarations {\\it postdeclaration}*)
 (meta-variables {\\it symboldeclaration}*)
 (preconditions {\\it meta-node-precond}*)
 (sideconditions {\\it sidecondition}*)
 (postconditions {\\it meta-node-postcond}*)
 (actions {\\it action})
 (description {\\it string})
 )
where the number of arguments, argumenttypes and arghelps specified 
must be equal.

\\begin{postsyntax}
\syntax{
{\\nt meta-node-precond} ::= 
    ({\\nt name} ({\\nt hypset}*) ({\\nt extra-hyps*}) {\\nt formula}).

{\\nt meta-node-postcond} ::= 
    ({\\nt name} ({\\nt hypset}*) ({\\nt extra-hyps*}) \\{{\\nt formula} | {\\nt eval-formula}\\} {\\nt justification}+). 

{\\nt hypset} ::= {\\nt symbol}.
{\\nt extra-hyp} ::= {\\nt symbol}.
{\\nt eval-formula} ::= (eval ({\\nt lispfunctionname} {\\nt meta-obj}*)).
{\\nt meta-obj} ::= \\{{\nt term} | {\\nt rule-term-pos}\\}.
{\\nt sidecondition} ::= ({\\nt lispfunctionname} \\{{\\nt meta-obj} | {\\nt nodename}\\}*).

{\\nt action} ::= ({\\nt nodes-pattern} {\\nt action-pattern}*).
{\\nt nodes-pattern} ::= \\{NIL | {\\nt nodename} | (support {\\nt nodename}*)\\}.
{\\nt action-pattern} ::= \\{(sponsor {\\nt nodename}*) | (unsponsor {\\nt nodename}*)\\}
}
\\end{postsyntax}
\\end{code}

All the symbols and types used in the formulas and arguments of the rule
must be declared. Those which are to be interpreted as meta-variables 
must be declared in the {\\tt meta-variable} section.

Let's take a look at how the preconditions are specified.  Note that 
they have a name (which is how they can be referred to, and which should also
be the argument name used) and of course a formula.  For hypotheses, there
are two lists.  The first is a list of symbols, each of which represents
a {\\em set\\/} of hypotheses.  The symbols of the second list, the 
{\\em extra-hyps\\/}, each are the name of a single line, which must be
defined in the rule somewhere.

Let's take an example to show how the matching process works when a rule
is applied.
\\begin{code}
(rule~def-rule exists-e (in base)
  (arguments D1 X C1)
  (argumenttypes pdsn+node sym+sym pdsn+node)
  (arghelps \"An existential node\" \"A parameter\" \"Node to be proved\")
  (declarations
   (type-variables CC)
   )
  (meta-variables (A (O CC)) (B O) (X CC))
  (preconditions
   (D1 (H) () (exists A))
   (C1 (H) () B))
  (sideconditions
   (pds~support-of-p D1 C1)
   (pds~not-free-in-nodes-or-hyps-p X C1 D1))
  (postconditions
   (D2 () (D2) (eval (hop~beta-contract (A X))) (\"Choose\" (X)))
   (C2 (H) (D2) B)
   (C1 (H) ()   B (\"Exists-E\" () (D1 C2))))
  (actions (C2 (sponsor D2) (unsponsor D1)))
  (description \"Existential elimination\"))
\\end{code}

Suppose we had the following proof:
\\begin{code}
H1  (H1) ! (exists (lam (X I) (P X)))  Hyp
    ...
C   (H1) ! (exists (lam (Y I) (P Y)))  Open
\\end{code}

Suppose we apply this rule with the arguments {\\tt H1}, a new parameter
{\\tt Z} of type I, and the line {\\tt C}.

We get the following matches:
\\begin{itemize}
\\item {\\tt D1} \\(=\\) {\\tt H1} therefore
\\item {\\tt A} \\(=\\) {\\tt (lam (X I) (P X))} and
\\item the type-variable {\\tt AA} \\(=\\) {\\tt I}
\\item {\\tt X} \\(=\\) {\\tt Z} 
\\item {\\tt C1} \\(=\\) {\\tt C} and therefore
\\item {\\tt B} \\(=\\) {\\tt (lam (Y I) (P Y))}
\\item finally {\\tt H} \\(=\\) {\\tt (H1)}
\\end{itemize}

Because the matching succeeds, the sideconditions are evaluated.
We assume that the node {\tt D1} is a support node of {\tt C1}, the
first sidecondition {\\vb (pds~support-of-p D1 C1)} is, therefore,
fullfilled. The second sidecondition {\\vb (pds~not-free-in-nodes-or-hyps-p Z C H1)} 
succeeds too, because {\\tt Z} is not free in the nodes {\\tt C} or
{\\tt H1}. 

Now we actually carry out the rule.  To do so, we see that there are two
new nodes in the postconditions, {\\tt D2} and {\\tt C2}.  Say that these
new nodes will be named {\\tt L1} and {\\tt L2}. 
{\\tt L2} will
be created with the hypotheses {\\tt (H1 L1)} as well as, because
the symbol {\\tt D2} appears in the {\\tt extra-hyps} list of the
definition of {\\tt C2}.  The node {\\tt L1} will have only itself as a 
hypothesis, which corresponds to the definition for {\\tt D2}.

The formula for {\\tt L2} will be {\\tt (lam (Y I) (P Y))}, because that
is the value of {\\tt B}.  For that of {\\tt L1}, we must evaluate the
formula {\\tt (hop~beta-contract (A X))}.  The formula {\\tt (A X)} is
really {\\tt ((lam (X I) (P X)) Z)}, so when it it beta-contracted, we
get {\\tt (P Z)}.  {\\tt C2} is a new node, but it has no justification,
so we make its real-life counterpart {\\tt L2} an open node.

Note that by the rule, {\\tt C1} should get a justification, so we justify
{\\tt C} with that justification, instantiating the meta-variables.

Here's what the proof will look like after the rule is used.
\\begin{code}
H1  (H1)    ! (exists (lam (X I) (P X)))  Hyp
L1  (L1)    ! (P Z)                       Choose: (Z)
    ...
L2  (H1 L1) ! (exists (lam (Y I) (P Y)))  Open
C   (H1)    ! (exists (lam (Y I) (P Y)))  Exists-E: (H1 L2)
\\end{code}

Each open postcondition corresponds to a subgoal, i.e. an open node to
be closed later. This open node inherits the supports of the rule
conclusion, i.e. the node that is closed by the method application. Such
node occurs both in the precondition list and, closed, in the postcondition
list. The subgoal node is a premise of the conclusion node. Consequently,
the open node {\\tt L2} which corresponds to the open postcondition {\\tt C2}
inherits the support nodes of the node {\\tt C} that corresponds to the
rule conclusion {\\tt C1}.

Moreover, the support nodes of an open node can be influenced by applying rule 
{\\tt action}s.  These have an influence only on the
supports of an open node. The only action for this rule was
{\\tt (C2 (sponsor D2) (unsponsor D1))}.  The nodes-pattern, {\\tt C2}, states
that only the associated open node {\\tt L2} is concerned by the given
action-patterns. The first action-pattern {\\tt (sponsor D2)} adds the
corresponding node {\\tt L1} to the supports of {\\tt L2} and the second
action-pattern {\\tt (unsponsor D1)} deletes the associated node {\\tt H1} from
{\\tt L2} supports.  
")
|#
