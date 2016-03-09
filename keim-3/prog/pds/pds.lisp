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

(in-package "KEIM")

(mod~defmod pds
	    :uses (th mod env just keim node post 
		   pdsc pdsj pdsn agenda prob term type logic)
	    :documentation "Proof planning functions."
	    :exports (
		      pds+proof-plan
		      pds~environment
		      pds~open-nodes
		      pds~support-nodes
		      pds~lemmata
		      pds~proof-context
		      pds~first-plan-step
		      pds~last-plan-step
		      pds~label-node-hashtable
		      pds~constraint-pool
		      pds+constraint-pool
		      pds~cstrpool-bindings
		      pds~cstrpool-constraint
		      pds~cstrpool-otherwise
		      pds~cstrpool-previous
		      pds~cstrpool-plansteps
		      pds~agenda
		      pds~constraint-pool-p
		      pds~cstrpool-create
		      pds~cstrpool-last-bindings
		      pds~proof-plan-p
		      pds~new-node-name
		      pds~find-proof-plan
		      pds~proof-plan-create
		      pds~change-last-plan-step!
		      pds~def-proof-plan
		      pds~inference-application
		      pds~get-inference-env
		      pds~post-read-obj
		      pds~label2node
		      pds~existent-p
		      pds~problem-assumption-p
		      pds~open-node!
		      pds~delete-node!
		      pds~cleanup
		      pds~plan-steps
		      pds~insert-node!
		      pds~only-insert-node!
		      pds~insert-node-after-before! 
		      pds~only-insert-node-between!
		      pds~insert-justification
		      pds~insert-actual-justification
		      pds~proof-done-p
		      pds~add-sponsors
		      pds~add-sponsors-if-hyp
		      pds~delete-sponsors
		      pds~node-supports
		      pds~node-formula
		      pds~task-formula
		      pds~reset-schematic-nodes!
		      pds~update-schematic-nodes!
		      pds~first-tasks!
		      pds~agenda-goals-with-supports-in
		      pds~node-supported-by-p
		      pds~find-support
		      pds~find-open-node
		      pds~find-node-support
		      pds~find-in-subproof
		      pds~add-thy-assertion
		      pds~add-hypothesis
		      pds~start-proof-plan
		      pds~focus
		      pds~proof-plan-status
		      pds~not-free-in-nodes-or-hyps-p
		      pds~constants-not-free-p
		      pds~not-free-in-terms-p
		      pds~change-top-var-quantor
		      pds~term-equal-application-of-to-p
		      pds~term-equal-defn-expansion-p
		      pds~not-atom-p
		      pds~pushneg
		      pds~pushneg-rec
		      pds~pullneg
;		      pds~replace-and-contract Funktionalitaet wird von folgender Funktion uebernommen:
		      pds~replace-and-contract-at-position
		      pds~replace-without-contract-at-position
		      pds~ground-definiens
		      ;;; LC: Erstmal anpassen: pds~make-problem-from-node
		      pds*avoid-node-breaks
		      pds~avoid-node-breaks
		      pds+quantified-term
		      pds~pprint-proof-plan
		      pds~pprint-pds-node
		      pds~figure-margins
		      pds-simple
		      pds~show-node
		      pds~node2string
		      pds~show-proof-plan
		      pds-pretty
		      pds-post
		      pds~post-print
		      pds~linearize-plan
		      pds~post-read-meth-mapping
		      pds~post-read-proof-plan-after
		      
		      pds~remove-proof-plan
		      pds~new-proof-plan-name
		      pds~insert-hyps!

		      pds~ordered-premises

		      pds*current-proof-plan
		      )
	       )


#{\subsection{Natural Deduction Proofs}
We define our proofs with some extra slots.  In order to quickly find
the lines that not yet justified (on which we will of course concentrate
our efforts), we keep a list of them current.  We also keep track of what
the final line of the proof is, that is, just what the ultimate goal of
this proof is.  We also, in order to be able to quickly translate from
user inputs referring to the symbol names of lines to the lines themselves,
we keep a hashtable with this association.   We also keep a integer counter
for use in generating new line names (as noted above).
#}

#{We'll use this hash table to keep track of all the proofs that
exist at any given time. Note that the names of proofs are
case-insensitive.
#}

(defvar pds*proof-plan-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by proof plan name, that holds all existing proof plans.  This way we can
jump back and forth between proof plans.")

#{Since most of our functions deal with only one proof at a time, 
it is useful to keep this variable around to refer to the current proof.
#}

(defvar pds*current-proof-plan nil
  "The current proof plan.  Most proof planning functions will refer to this variable as a
default proof plan.")


(defvar pds*current-prfpln-env nil
  "The environment of the current proof plan, pds*current-proof-plan.  Some functions will refer
to this variable as a default environment.")


(eval-when (load compile eval) 
(defclass pds+proof-plan (prob+proof) 
  ((environment :initarg :environment
		:accessor pds~environment
		:documentation "The environment of the PDS, it inherits from the environment of the corresponding problem.")
   (open-nodes :initarg :open-nodes
	       :accessor pds~open-nodes
	       :documentation "The nodes which still are unjustified.")
   (support-nodes :initarg :support-nodes
		  :initform nil
	          :accessor pds~support-nodes
	          :documentation "The assumptions and their consequences.")
   (lemmata :initarg :lemmata
	    :initform nil
	    :accessor pds~lemmata
	    :documentation "An ordered list of lemmata in the pds.")
   (first-plan-step :initarg :first-plan-step
	            :initform nil
	            :accessor pds~first-plan-step
	            :documentation "The first plan step in the proof plan.")
   (last-plan-step :initarg :last-plan-step
	           :initform nil
	           :accessor pds~last-plan-step
	           :documentation "The last plan step in the proof plan.")
   (label-node-hashtable
    :accessor pds~label-node-hashtable
    :initarg :label-node-hashtable
    :initform (make-hash-table :test #'equal)
    :documentation "A mapping from labels to nodes.")
   (label-counter
    :accessor pds=label-counter
    :initarg :label-counter
    :initform 0)
   (agenda :initarg :agenda
	   :initform (make-instance 'agenda+empty-agenda)
	   :accessor pds~agenda
	   :documentation "holds the current agenda.")
   (cstr-pool :initarg :cstr-pool
	      :initform nil
	      :accessor pds~constraint-pool
	      :documentation "holds the consraints on the meta-variables in the PDS and their bindings history for backtracking.")
   (proof-context :initarg :proof-context
		  :initform nil
		  :accessor pds~proof-context
		  :documentation "The associated proof-context administering all the foci etc.")
   )
  (:documentation "A proof plan object."))

(defclass pds+constraint-pool ()
  ((bindings :initarg :bindings
	     :initform nil
	     :accessor pds~cstrpool-bindings
	     :documentation "Bindings of some meta-variables")
   (constraint :initarg :constraint
	       :initform nil
	       :accessor pds~cstrpool-constraint
	       :documentation "Non-binding constraint defined on meta-variables which are not bound in BINDINGS.")
   (otherwise :initarg :otherwise
 	      :initform nil
 	      :accessor pds~cstrpool-otherwise
 	      :documentation "Other constraint situation exclusive to this constraint pool.")
   (previous :initarg :previous
	     :accessor pds~cstrpool-previous
	     :initform nil
	     :documentation "Pointer to the previous constraint pool.")
   (plansteps :initarg :plansteps
	      :initform nil
	      :accessor pds~cstrpool-plansteps
	      :documentation "The plan steps which result in this constraint pool.")
   )
  (:documentation "A data structure for the PDS constraint pool."))
)


(defmethod print-object ((prfpln pds+proof-plan) stream)
  (format stream 
	  (if *print-escape* "#<pds+proof-plan ~A>" "~A")
	  (keim~name prfpln)))


(defun pds~proof-plan-p (thing)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type PDS+PROOF-PLAN, else NIL."))
  (typep thing 'pds+proof-plan))

(defmethod print-object ((cstrpool pds+constraint-pool) stream)
  (format stream 
	  (if *print-escape* "#<pds+constraint-pool with bindings: ~A and constraint: ~A>"
	    "#<Bindings: ~A and constraint: ~A>")
	  (pds~cstrpool-bindings cstrpool) (pds~cstrpool-constraint cstrpool)))
 
(defun pds~constraint-pool-p (thing)
  (declare (edited  "23-MAR-1998")
           (authors Lassaad)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type PDS+CONSTRAINT-POOL, else NIL."))
  (typep thing 'pds+constraint-pool))

(defun pds~cstrpool-create (bindings cstr previous &optional plsteps)
  (make-instance 'pds+constraint-pool
		 :previous previous
		 :bindings bindings
		 :constraint cstr
		 :plansteps plsteps))

(defmethod pds~cstrpool-last-bindings ((cstrpool pds+constraint-pool))
  (declare (edited  "31-MAR-1998")
	   (authors Lassaad)
	   (input   "A constraint pool.")
	   (effect  "None.")
	   (value   "The last bindings in CSTRPOOL that is not empty."))
  (let ((bindings (pds~cstrpool-bindings cstrpool)))
    (if bindings bindings
      (let ((previous (pds~cstrpool-previous cstrpool)))
	(when previous
	  (pds~cstrpool-last-bindings previous))))
    ))

#{
Every time we create a node, we need a name for it, and if one isn't supdsied,
we have to think of it on our own here.  We'll use the following function
to make new nodes, basically of the form ``N1'', ``N2''.  To keep track
of which names we've already used in this proof plan, there's a counter kept
in each proof plan.
#}

(defun pds~new-node-name (&optional (pds pds*current-proof-plan))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "None.")
           (effect  "None.")
           (value   "A new symbol."))
  (loop 
      (let ((count (incf (pds=label-counter pds))))
       (if (pds~label2node (format nil "L~A" count))
	   (incf (pds=label-counter pds))
         (return-from pds~new-node-name 
	   (make-symbol (format nil "L~A" count))))))
  )


#{
Given the name of a proof plan, we want to find the proof plan itself.  Note that
these names are case insensitive.
#}

(defgeneric pds~find-proof-plan (name)
  (declare (edited  "12-JUN-1997" "28-JUL-92 10:00")
           (authors Sorge NESMITH)
           (input   "A name of a proof plan (symbol or string).")
           (effect  "None.")
           (value   "The proof plan with this name, or NIL if none exists."))
  (:method ((name string))
	   (gethash (string-upcase name) pds*proof-plan-hashtable))
  (:method ((name symbol))
	   (gethash (symbol-name name) pds*proof-plan-hashtable))
  (:method ((proof pds+proof-plan))
	   (gethash (keim~name proof) pds*proof-plan-hashtable)))

(defun pds~proof-plan-create (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A symbol NAME to be used as name for the proof plan.")
           (effect  "A new proof plan is made, and put in hash table of proof plans.")
           (value   "The new proof plan with name NAME."))
  (let ((new-proof (make-instance 'pds+proof-plan :name name)))
    (setf (gethash (symbol-name name) pds*proof-plan-hashtable)
	  new-proof)
    new-proof))


(defun pds~change-last-plan-step! (node &optional (pds pds*current-proof-plan))
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A node, and optionally a PDS.")
	   (effect  "Inserts the plan step associated to NODE at the end of the plan"
		    "step sequence in PDS. If the last plan step of PDS was NIL, i.e."
		    "NODE corresponds to the first proven node in PDS, then sets the"
		    "first plan step of PDS to the plan step associated to NODE.")
	   (value   "The last plan step."))
  (let ((last-plstp (pds~last-plan-step pds))
	(plstp (pdsc~an-create node)))
    (if last-plstp
	(setf (pds~last-plan-step pds)
	      (pds=connect-plan-steps! last-plstp plstp))
      (setf (pds~first-plan-step pds) plstp
	    (pds~last-plan-step pds) plstp))
    plstp
    ))


(defun pds=connect-plan-steps! (plstp1 plstp2)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "Two plan steps.")
	   (effect  "Orders the plan step PLSTP2 just after the plan step PLSTP1.")
	   (value   "PLSTP2."))
  (setf (pdsj~predecessor (pdsc~an-just plstp2)) plstp1)
  (setf (pdsj~successor (pdsc~an-just plstp1)) plstp2)
  plstp2)


(defun pds=remove-plan-step! (pds plstp)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, and one of its plan steps.")
	   (effect  "Remove PLSTP from the PDS plan steps.")
	   (value   "Unspecified."))
  (let ((last-plstp (pds~last-plan-step pds))
	(first-plstp (pds~first-plan-step pds))
	(plstp-just (pdsc~an-just plstp)))
    ;(format T "~%STEPS BEFORE:~%~A~%" (pds~plan-steps pds))
    (when (keim~equal last-plstp plstp)
      (setf (pds~last-plan-step pds) (pdsj~predecessor plstp-just)))
    (when (keim~equal first-plstp plstp)
      (setf (pds~first-plan-step pds) (pdsj~successor plstp-just)))
    (pdsj~control-connect-pred&succ! (pdsj~control plstp-just))
    ;(format T "~%STEPS AFTER:~%~A~%" (pds~plan-steps pds))
    ))

 
#{\subsection{Reading a Proof Plan}
It is of course useful to have a way to store proof plans
and be able to read them back in. The macro pds~def-proof-plan allows you
to do that.

First let's give an example of this syntax.
\begin{code}
(PDS (problem example)
     (in base)
     (conclusion L100)
     (assumptions L1)
     (open-nodes L100)
     (support-nodes L1)
     (nodes
      (L1   (L1) (P A) (0 ("Hyp" () () "closed" () ())))
      (L100 (L1) (exists (lam (Y I) (P Y))) (0 ("Open" () () "open" () ())))
      )
     (controls
      )
     (plan-steps
      )
     )
   
  
\end{code}

Now here's a description of the syntax:

\begin{code}
(PDS (problem {\it problem-name})
  (in {\it thy-name})
  (declarations {\it post-declaration}*)
  (conclusion {\it node-name})
  (assumptions {\it node-name}*)
  (open-nodes {\it node-name}*)
  (support-nodes {\it node-name}*)
  (nodes {\it node}*)
  (lemmata {\it node-name}*)
  (controls {\it node-ctrls}*)
  (plan-steps {\it plan-step}*)
  ).

{\it node} ::== 
 ({\it node-name} ({\it hyp-name}*) {\it formula} ({\it just-number} {\it justification}*).

{\it justification} ::== ({\it method-name}              
			  ({\it just-param}*)         
			  ({\it node-name}*)
			  {\it status}
			  {\it pds-subst}
			  {\it outline-pattern}
			  ).
			   
{\it just-param} ::== term {\it term} |
                      position {\it position} | ...			   

{\it status} ::== open | closed | grounded | expanded | checked | ...

{\it pds-subst} ::== () | (({\it method-metavar}*) ({\it pds-object}*))

{\it pds-object} ::== node {\it node-name} |
                      term {\it term} |
                      just {\it node-just} | ... 

{\it outline-pattern} ::== ({\it conc-outline} {\it prem-outline}*)| ()

{\it conc-outline} ::== existent | nonexistent

{\it prem-outline} ::== existent | nonexistent | closed

{\it node-ctrls} ::== ({\it node-name} {\it just-ctrl}*).

{\it just-ctrl} ::== () |
                     (({\it node-name}*) ({\it node-name}*) ({\it alternative}*) ({\it why-method}))

{\it alternative} ::== Not yet specified

{\it why-method} ::== Not yet specified

{\it plan-step} ::== ({\it node-just} {\it node-just}*).

{\it node-just} ::== {\it node-name} {\it just-number}

{\it just-number} ::== 0 | 1 | 2 | ...

\end{code}

Above, {\it pds-name}, as well as {\it thy-name}, {\it node-name}, {\it hyp-name},
and {\it method-name} is a symbol.  {\it post-declaration} is any declaration
suitable for adding to the theory environment. {\it formula} is the POST
representation of a formula, suitable for reading.  Every {\it just-param}
consists of a key word and an associated POST representation, i.e. TERM with a
POST representation of a term, or POSITION with a POST representation of a
position. {\it pds-subst} is either an empty list or a pair of lists. The first
one contains symbols denoting metavariables of the justification method. The
second list contains the associated objects from the PDS represented by
{\it pds-object}s. Each {\it pds-object} consists of a key word and a suitable
representaion of the object itself, e.g. the associated object of the key word
JUST is represented by a node name succeeded by a number, we mean thereby the
${\rm number}^{th}$ less abstract justification of the node.

The {\it conc-outline} is either EXISTENT or NONEXISTENT saying respectively the
associated node was closed by a backward method or generated by a forward method.
Whereas the key words for {\it prem-outline} have a little bit moderate semantic:
EXISTENT means the associated premise existed before the method application and it
does not matter whether it was open or closed. CLOSED denotes that the associated
premise existed closed before applying the method. And NONEXISTENT corresponds to
a premise which did not exist before applying the method, i.e. this premise is
generated by the method application as a subgoal.

In {\it node-ctrls} we store the node name and the controls of the node
justification beginning with the most abstract one represented by {\it just-ctrl}s.
A {\it just-ctrl} is either empty or consists of sponsor list, unsponsor list,
{\it alternative}-list, and {\it why-method}-list.

The list with key word PLAN-STEPS contains the ordering of applying the methods when
constructing the PDS. This list is sorted from the first plan step to the last one.
Each plan step is represented by a {\it node-just} following with some
{\it node-just}. Each {\it node-just} consists of a node and a number to retrieve
the ${\rm number}^th$ justification of this node. The first {\it node-just}
corresponds to the node closed by the plan step and the rest {\it node-just}s are the
nodes involved in this plan step including the expansion nodes.

Here's basically what happens when you read something like this in.
A new proof plan is created with the name {\it pds-name}, and with an
environment that consists only of the symbols declared in the declaration
part of the form.  The nodes are created using the {\it nodes} given.  Their
order doesn't matter, because we make sure they are created in the proper
order. For each node, we read in the list of {\it hyp-name}s as the
hypotheses of the node, the {\it formula} as its (of course) formula, 
and we create a justification from the list of justifications which
are sorted from the justification at the most abstract level to the one at
the least abstract level. For each given {\it justification} a justification
is created with method {\it method-name}, premises from the list of {\it node-name}s,
the list of parameters {\it just-param}s (which we first parse), status {\it status}, 
substitution {\it pds-subst}, and outline-pattern {\it outline-pattern}.
The below and above justifications are respectively
set to the next and previous justification in the given {\it justification} list.
The current justification of the node corresponds to the justification on the
{\it just-number}-th abstraction level.

The control part of a justification is first created with sponsors and unsponsors
given in the first and respectively second list of {\it node-name}s.
The reasons, successor, and predecessor slots are then filled after processing the
given {\it plan-step}s. (The other slots are not yet considered!)
The plan steps are listed from the first to the last one in the PDS. We create an
actual node with node {\it node-name} and the ${\it just-number}^th$ justification of
this node. This actual node is then inserted as reason in the control of the own
justification and in the control of the rest justifications corresponding to the
remaining {\it node-just}s. The plan step sequence of the PDS is constructed by
joining the resulted actual nodes. 

There is some redundancy in this syntax and it is used to check for 
consistency.  The node name and formula given in the {\it conclusion} field must
match with those given in the node's description.  The assumptions are
the only nodes that may be used as hypotheses of the conclusion. 
The open nodes should be exactly those specified in the {\it open-nodes} 
field.
#}
;;
;; The formula in a pds must be currently nonpolymorph: 
;; (get-wff (wff) (term~read wff nd*current-proof-environment))
;; If we want to allow polymorph formula we should change the definition 
;; of get-wff using term~read-poly instead of term~read.   
;;
(defmacro pds~def-proof-plan (seek problem &rest attribs)
  (declare (edited  "11-SEP-2000" "28-MAY-1997"  "28-JUL-92 10:00")
           (authors Pollet Lassaad NESMITH)
           (input   "A written representation of an proof plan.")
           (effect  "Read the proof plan, construct a real proof plan.")
           (value   "The new proof plan."))
  (if (and (consp problem) (>= (length problem) 2) (string-equal (car problem) :problem))
      (let* ((name (cadr problem))
	     th-spec decl-list conc ass-list conc-p open-list supp-list
	     node-list lemma-list agenda node-list-p ctrl-list plstp-list
	     (help-str "")
	     (prob (pds=seek-problem name seek)))
	(if prob
	    (let* ((proof-name (do* ((x 0 (1+ x))
				     (y (pds~new-proof-plan-name prob)
					(pds~new-proof-plan-name prob x)))
				   ((not (pds~find-proof-plan y)) y)))
		   (prob-assum (prob~assumptions prob))
		   (prob-conc (prob~conclusion prob))
		   (proof-env (env~create (prob~environment prob) proof-name))
		   (type-var-subst (env~lookup-object 'type-var-subst (prob~environment prob))))
	      (env~enter 'type-var-subst
			 (if type-var-subst
			     (keim~copy type-var-subst :downto '(data+struct))
			   (subst~create nil nil))
			 proof-env)
	      (setf data*global-type-var-subst (env~lookup-object 'type-var-subst proof-env))
	      (do ((attribs (rest attribs) (rest attribs))
		   (attrib (first attribs) (first attribs)))
		  ((and (null attrib) (null attribs)))
		(if (consp attrib)
		    (cond
		     ((string-equal (first attrib) :in)             (setq th-spec (second attrib))) 
		     ((string-equal (car attrib) :declarations)     (setq decl-list (cdr attrib)))
		     ((string-equal (first attrib) :conclusion)     (setq conc (second attrib)
									  conc-p t))
		     ((string-equal (first attrib) :assumptions)    (setq ass-list (rest attrib)))
		     ((string-equal (first attrib) :open-nodes)     (setq open-list (rest attrib)))
		     ((string-equal (first attrib) :support-nodes)  (setq supp-list (rest attrib)))
		     ((string-equal (first attrib) :nodes)          (setq node-list (rest attrib)
									  node-list-p t))
		     ((string-equal (first attrib) :lemmata)        (setq lemma-list (rest attrib)))
		     ((string-equal (first attrib) :agenda)         (setq agenda attrib))
		     ((string-equal (first attrib) :controls)       (setq ctrl-list (rest attrib)))
		     ((string-equal (first attrib) :plan-steps)     (setq plstp-list (rest attrib)))
		     ((string-equal (first attrib) :help)           (setq help-str (second attrib)))
		   (t (error ";;;pds~~def-proof-plan: Not expecting ~S" attrib)))
		  (error ";;;pds~~def-proof-plan: Not expecting ~S" attrib)))
	      (unless conc-p      (error ";;;pds~~def-proof-plan: Must specify conclusion."))
	      (unless node-list-p (error ";;;pds~~def-proof-plan: Must specify node list."))
	      (unless (and (= (length ass-list) (length prob-assum))
			   (every #'string-equal (cons conc ass-list)
				  (mapcar #'keim~name (cons prob-conc prob-assum))))
		(error ";;;pds~~def-proof-plan: Assumptions and conclusion of problem and pds are not equal."))
;;; do some error-checking here before beginning.))
	      `(block nil
		 ;; in this when, we could ask if the user wants to write over
		 ;; a proof plan that already exists.  
		 (unless (th~find-theory ',th-spec) (error ";;;pds~~def-proof-plan: Theorie ~A does not exist." ',th-spec))
		 (let* ((theory (th~find-theory ',th-spec))
			(new-proof (make-instance 'pds+proof-plan 
						  :name ',proof-name
						  :theory theory
						  :steps nil 
						  :environment ',proof-env
						  :open-nodes nil
						  :help ',help-str
						  ))
			(pds*current-proof-plan new-proof)
			(pds*current-prfpln-env ',proof-env)
			(node-label-mapping nil))
		   (labels ((look-up-node (label)
				;;;Retrieves the node given its label:
					  (let ((node (cdr (assoc label
								  node-label-mapping
								  :test #'string-equal))))
					    (if node 
						node
					      (error "pds~~def-proof-plan: ~S is not a node." label))))
			    (get-just (pos&justs nodeformula)
			    ;;;Constructs a justification given the PDS-POST representation of
			    ;;;its above justifications and the justification itself:
				      (let ((just-list
					     (mapcar #'(lambda (just)
							 (let ((status (fourth just)))
							   (if (string= status "open")
							       (pdsj~open-just-create)
							     (let* ((meth-str (first just))
								    (outline-pat (sixth just))
								    (meth (infer~find-method meth-str))
								    (the-meth (if meth meth
										(error "The inference ~A has not been loaded." meth-str)))
								    (subst (get-subst (fifth just)
										      the-meth
										      outline-pat
										      outline-pat
										      (third just)
										      nodeformula))
								    (the-just
								     (pdsj~create
								      the-meth
								      (mapcar #'look-up-node (third just))
								      NIL NIL
								      status
								      (mapcar #'get-param (second just))
								      ;(get-subst (fifth just) mappl-id)
								      subst
								      outline-pat
								      NIL))
								    (ass-p (cdr (assoc-if
										 #'(lambda (ind)
										     (string-equal meth-str
												   (symbol-name ind)))
										 node-label-mapping))))
							       (when ass-p
								 (keim~put the-just :assertion ass-p))
							       the-just)
							     )))
						     (rest pos&justs))))
					(pdsj~nth-chain-justs! (first pos&justs) just-list)))
			    (set-node-ctrls (node&ctrls)
				  ;;;Sets the controls of the node justifications
					    (set-just-ctrl (pdsn~most-abstract-just (look-up-node (first node&ctrls)))
							   (rest node&ctrls)))
			    (set-just-ctrl (just ctrls)
				 ;;;Sets the control of a justification and of its below
				 ;;;justifications:
					   (when ctrls
					     (let ((ctrl (first ctrls)))
					       (when ctrl
						 (setf (pdsj~control just)
						       (pdsc~create NIL
								    (mapcar #'look-up-node (first ctrl))
								    (mapcar #'look-up-node (second ctrl)))))
					       (set-just-ctrl (pdsj~below just) (rest ctrls)))))
			    (get-wff (wff)
				     ;;;Creates a formula from its POST representation
				     (if (and (listp wff)
					      (symbolp (first wff))
					      (string-equal (first wff) :all-types))
					 (post~read-object wff pds*current-prfpln-env :existing-term-closed)
				       (post~read-object wff pds*current-prfpln-env :existing-term)))
			    (get-param (param)
			     ;;;Creates an object from its PDS-POST representation:
				       (pds~post-read-obj param pds*current-prfpln-env theory #'look-up-node NIL NIL))
			    (get-subst (subst inference outline-pat adapted-pat outline nodeformula)
			     ;;;Creates a substitution from its PDS-POST representation:
				       (when subst
					 (let ((label-pat (find-if #'(lambda (pat)
								       (not (or (infer~nonexistent-pattern-p pat)
										(infer~existent-pattern-p pat)
										(infer~closed-pattern-p pat)
										(infer~list-pattern-p pat))))
								   adapted-pat)))
					   (if label-pat
					       ;;;A method with many conclusions
					       (let* ((conc (look-up-node label-pat))
						      (conc-just (node~justification conc)))
						 (when conc-just
						   ;;; CONC is processed
						   (let* ((the-just (pdsj~get-twin-just conc-just
											inference
											outline-pat))
							  (the-pat (pdsj~conclusion-outline the-just))
							  (new-adapted-pat (substitute the-pat
										       label-pat
										       adapted-pat))
							  (the-subst (get-subst subst
										inference
										outline-pat
										new-adapted-pat
										outline
										nodeformula)))
						     (when the-subst
						       ;;; It is possible to compute the subst from the
						       ;;; new adapted outline pattern:
						       (setf (pdsj~subst the-just) the-subst)
						       the-subst)
						     ;;; It is not possible to compute the subst, so return NIL.
						     ))
						 ;;; CONC is not yet processed, it is therefore not yet possible
						 ;;; to compute the subst, so return NIL: subst will be computed
						 ;;; after processing CONC.
						 )
					     ;;; Either a method with only one conclusion, 
					     ;;; or it is now possible to compute the subst
					     ;;; For this the bound variables are temporarly
					     ;;; added to the proof environment.
					     (let* ((copy-of-pat (mapcar #'(lambda (x) x) adapted-pat))
						    ;;MP: pds~inference-application is desctructive!!
						    (infer-appl (pds~inference-application inference copy-of-pat))
						    (terms (cons nodeformula                         
								 (mapcar #'(lambda (node)
									     (node~formula
									      (look-up-node node)))
									 outline)))
						   (added-vars (pds=add-vars-temporary-to-env
								(mapcan #'term~bound-variables terms) pds*current-prfpln-env))
						   (mapp (pds~post-read-meth-mapping subst
										     pds*current-prfpln-env
										     theory
										     infer-appl
										     #'look-up-node)))
					       (pds=remove-names-from-env  added-vars pds*current-prfpln-env)
					       mapp)))))
			    (process-plstp (plstp)
				 ;;;Creates the actual node of the PLSTP and inserts it in
				 ;;;the specified justifications:
					   (let* ((node (look-up-node (first plstp)))
						  (just (pdsn~nth-just node (second plstp)))
						  (reason (pdsc~an-create node just)))
					     (pdsj~insert-reason! just reason)
					     (insert-reason reason (rest (rest plstp)))
					     reason))
			    (process-declarations (decl-list) 
						  (post~read-object-list decl-list 
									 pds*current-prfpln-env))
			    (insert-reason (reason node-justs)
				 ;;;Inserts REASON in the given node justifications:
					   (when node-justs
					     (let ((node (look-up-node (first node-justs))))
					       (pdsj~insert-reason! (pdsn~nth-just node (second node-justs))
								    reason)
					       (insert-reason reason (rest (rest node-justs))))))
			    (check-just (node just)
			      ;;;Prevents a node to have itself as premise
					(if (find node (pdsj~ass&premises just))
					    (error "pds~~def-proof-plan: Illegal justification: ~
                              Node ~A justifies itself." (keim~name node))
					  just))
			    (get-task (task)
				      (cond ((string-equal (string (car task)) "task")
					     (let ((task-node (look-up-node (second task))))
					       (if (pdsn~open-node-p task-node)
						   (agenda~create-open-task task-node)
						 (agenda~create-pseudo-goal task-node))))
					     ((string-equal (string (car task)) "task-inst")
					      (agenda~inst-task-create
					       (env~lookup-object (first (second task)) pds*current-prfpln-env)
					       (pds~post-read-obj (second (second task)) pds*current-prfpln-env
								  theory #'look-up-node NIL NIL)))
					     (T  (error "pds~~def-proof-plan: unknown task ~A." task))))
			    (get-agenda (theagenda)
					(if (> (length theagenda) 1)
					  (let* ((first (when (second theagenda)
							  (get-task (second theagenda))))
						 (next (mapcar #'get-task (third theagenda)))
						 (tasklist (pairlis (cons (second theagenda) (third theagenda))
								    (cons first next)))
						 (orderings
						  (mapcar #'(lambda (ord)
						     (apply #'agenda~ordering-create
							(mapcar #'(lambda (orderlist)
							  (mapcar #'(lambda (task)
							      (rest (assoc task tasklist :test #'equal)))
								  orderlist)) ord)))
							  (fourth theagenda)))
						 (thenagenda (get-agenda (fifth theagenda))))
					    (agenda~create first next orderings thenagenda))
					  (agenda~generate nil)))
			    )
		     (process-declarations ',decl-list)
		     (dolist (node ',node-list)
		       (let ((label (first node))
			     (wff   (get-wff (third node))))
			 (push (cons label (make-instance
					    (if (data~free-variables wff)
						'pdsn+schematic-node 'pdsn+node)
					    :name label :formula wff))
			       node-label-mapping)))
		     (dolist (node ',node-list
				   (let ((hashtable (pds~label-node-hashtable new-proof)))
				     (dolist (node (prob~proof-steps new-proof))
				       (setf (gethash (symbol-name (keim~name node)) 
						      hashtable)
					     node))))
		       (let ((new-node nil)
			     (label (first node)))
			 (setq new-node (look-up-node label))
			 (let* ((hyps (mapcar #'look-up-node (second node)))
				(wff (node~formula new-node)) ;(get-wff (third node)))
				(just (get-just (fourth node) wff)))
			   (setf (pdsn~hyps new-node) hyps)
			   ;(setf (node~formula new-node) wff)
			   (setf (node~justification new-node) 
				 (check-just new-node just))
			   (setf (prob~proof-steps new-proof)
				 (append (prob~proof-steps new-proof) (list new-node))))))
		     (let* ((root (look-up-node ',conc)))
		       (setf (prob~proof-problem new-proof) ',prob)
		       (setf (prob~proof-root new-proof) root))
		     (setf (pds~open-nodes new-proof) (mapcar #'look-up-node ',open-list))
		     (setf (pds~lemmata new-proof) (mapcar #'look-up-node ',lemma-list))
		     (setf (pds~support-nodes new-proof) (mapcar #'look-up-node ',supp-list))
		     (setf (pds~agenda new-proof) (get-agenda ',agenda))
		     (mapcar #'set-node-ctrls ',ctrl-list)
		     (let* ((plstps (mapcar #'process-plstp ',plstp-list))
			    (first-plstp (first plstps))
			    (last-plstp (first (last plstps))))
		       (setf (pds~first-plan-step new-proof) first-plstp)
		       (setf (pds~last-plan-step new-proof) last-plstp)
		       (mapcar #'(lambda (pred succ)
				   (pds=connect-plan-steps! pred succ))
			       (butlast plstps) (rest plstps))))
		   (pds~post-read-proof-plan-after new-proof)
		   (prob~add-proof ',prob new-proof)
		   (setf (gethash (symbol-name ',proof-name) pds*proof-plan-hashtable) new-proof))))
	  (error ";;;PDS~~DEF-PROOF-PLAN: no valid problem-name.")))
    (error ";;;PDS~~DEF-PROOF-PLAN: no valid problem-name.")))


(defgeneric pds~post-read-proof-plan-after (pds)
  (declare (edited  "13-SEP-2000")
	   (authors Pollet)
	   (input   "A PDS that has been read by pds~~def-proof-plan.")
	   (effect  "Do some post-processing: (this method is defined in method.lisp!)"
		    "-replace (:pds-anode ...) in the constraint-pools by the corresponding objects.")
	   (value   "-")))

(defun pds=add-vars-temporary-to-env (vars env)
  (declare (edited  "11-SEP-2000")
	   (authors Pollet)
	   (input   "A list of variables and an environment.")
	   (effect  "Adds the variables to env, when they is"
		    "no other object with this name.") 
	   (value   "A list with the names of the vars that have"
		    "been added."))
    (mapcan
     #'(lambda (var)
	 (let ((name (keim~name var)))
	   (if (env~lookup-object name env)
	       (warn ";;pds=add-vars-temporary-to-env: object with name ~A already exists in env ~A."
			name (keim~name env))
	     (progn (env~enter name var env) (list name)))))
	vars))

(defun pds=remove-names-from-env (varnames env)
  (declare (edited  "11-SEP-2000")
	   (authors Pollet)
	   (input   "A list of names and an environment.")
	   (effect  "Removes objects with the a name that appears in namelist"
		    "from the given environment.")
	   (value   "-"))
    (mapc
     #'(lambda (var)
	     (env~remove var env))
	varnames))

(defgeneric pds~inference-application (inference outline-pattern)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "An inference, and an outline pattern.")
	   (effect  "None.")
	   (value   "The application of INFERENCE that corresponds to the"
		    "given OUTLINE-PATTERN.")))
    
(defun pds=seek-problem(problem seek)
  (let ((prob (prob~find-problem problem)))
    (when (and seek (not prob)) (print "do something elaborate."))
    prob))
    
(defun pds~new-proof-plan-name (problem &optional (number nil))
  (declare (edited  "11-JUN-1997")
	   (authors Sorge)
	   (input   "A problem and a number.")
	   (value   "A new standard proof-plan name for the problem."))
  (let ((name (keim~name problem))
	(prooflist (prob~proofs problem)))
    (if (numberp number) (make-symbol (format nil "~A-~A" name number))
      (make-symbol (format nil "~A-~A" name (1+ (length prooflist)))))))
  
(defun pds~remove-proof-plan (&optional (pds pds*current-proof-plan))
  (declare (edited  "12-JUN-1997")
	   (authors Sorge)
	   (input   "A PDS.")
	   (effect  "Removes the PDS both from the proofs of the problem and from the"
		    "PDS hashtable.")
	   (value   "Undefined."))
  (when (pds~proof-plan-p pds)
    (let ((prob (prob~proof-problem pds)))
      (prob~remove-proof prob pds :completely t)
      (remhash (symbol-name (keim~name pds)) pds*proof-plan-hashtable))))

(defgeneric pds~post-read-meth-mapping (subst env theory method func)
  (declare (edited  "07-SEP-2000")
	   (authors Pollet Sorge)
	   (input   "A PDS-POST representation of a substitution, an environment,"
		    "a method, and a function. The function FUNC delivers for each"
		    "node label the associated node in the PDS.")
	   (effect  "None.")
	   (value   "The method mapping object which corresponds to the bindings of"
		    "the METHOD metavariables to PDS-objects."
		    "(This method is defined in method.lisp!)")))
  
(defgeneric pds~get-inference-env (inference)
  (declare (edited  "05-JUN-1997")
	   (authors Lassaad)
	   (input   "An inference application.")
	   (effect  "None.")
	   (value   "The environment of this INFERENCE application."))
  )

(defgeneric pds~post-read-obj (object env theory func meth-env indicator)
  (declare (edited  "29-MAY-1997")
	   (authors Lassaad)
	   (input   "A PDS-POST representation of a PDS object, the PDS environment,"
		    "a function which delivers for each node label its associated node"
		    "in the PDS, an environment of a method application, and an indicator.")
	   (effect  "None.")
	   (value   "The PDS object."))
  (:method ((object t) (env t) (theory t) (func t) meth-env (indicator t))
	   (declare (ignore meth-env))
	   (error "PDS~~POST-READ-OBJ: Don't know how to read ~A with indicator of ~A"
		  object indicator))
  (:method ((object cons) (env env+environment) (theory th+theory) (func function) meth-env (indicator null))
	   (let ((key (first object)))
	     (if (symbolp key)
		 (pds~post-read-obj (second object) env theory func meth-env key)
	       (let ((rest-obj (rest object)))
		 (if rest-obj
		     (cons (pds~post-read-obj key env theory func meth-env nil)
			   (pds~post-read-obj rest-obj env theory func meth-env nil))
		   (list (pds~post-read-obj key env theory func meth-env nil)))))))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-term)))
	   (declare (ignore meth-env))
	   (if (and (consp object)
		    (string-equal (string (car object)) "all-types"))
	       (if (symbolp (car (last object)))
		   (post~read-object (car (last object)) env :existing-term )
		 (post~read-object object env :existing-term-closed))
	   (let ((term  (post~read-object object env :existing-term))) ;;MP: post~read-object adds 'all-types' when there
	     (if (term~schema-p term)                           ;;are type-vars, this is not the original formula
		 (data~schema-range term) term))))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-type)))
	   (declare (ignore meth-env))
	   (post~read-object object env :existing-type))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-sort)))
	   (declare (ignore meth-env))
	   (sort~lookup-sort object (th~senv theory)))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-thdef)))
	   (declare (ignore meth-env))
	   (th~find-assumption object theory))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-number)))
	   (declare (ignore meth-env))
	   object)
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-subst)))
	   (let*  ((vars (mapcar #'(lambda (var) (env~lookup-object var env)) (car object)))
		   (terms (mapcan #'(lambda (term var)
				      (when var
					(list (pds~post-read-obj term env theory func meth-env nil))))
				    (second object) vars)))
	     (when (some #'null vars)
		 (warn "PDS~~POST-READ-OBJ: substitution ~A contains unknown variables" object))
	     (subst~create (remove nil vars) terms)))
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-anode)))
           (declare (ignore meth-env))
           (let* ((node (funcall func (car object)))
                  (just (pdsn~nth-just node (second object))))
             (pdsc~an-create node just)))
  (:method (object (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-nil)))
	   (declare (ignore object meth-env))
	   nil)
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-symbol)))
	   (declare (ignore meth-env))
	   object)
  (:method ((object t) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-var)))
	   (declare (ignore meth-env))
	   (let* ((var-name (first object))
		  (var-type (second object))
		  (var (env~lookup-object var-name env)))
	     (if var
		 (if (and (term~variable-p var)
			  (string-equal (keim~name var) var-name)
			  (keim~equal (term~type var)
				      (post~read-object var-type env :existing-type)))
		     var
		   (let ((new-var-name (pds=env-new-object-name var-name env)))
		     (term~create-primitive-in-environment new-var-name
					     (post~read-object var-type env :existing-type)
					     'term+variable
					     env)))
	       (term~create-primitive-in-environment var-name
				       (post~read-object var-type env :existing-type)
				       'term+variable
				       env))
	     ))
  (:method ((object symbol) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-node)))
	   (declare (ignore meth-env))
	   (apply func (list object)))
  (:method ((object cons) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-just)))
	   (declare (ignore meth-env))
	   (let ((node (apply func (list (first object)))))
	     (pdsn~nth-just node (second object))))
  (:method ((object cons) (env env+environment) (theory th+theory) (func function) meth-env (indicator (eql :pds-post-obj)))
	   (declare (ignore meth-env))
	   (post~read-object object env NIL))
  )

(defun pds=env-new-object-name (sym env)
  (declare (edited  "04-JUL-1997")
	   (authors Lassaad)
	   (input   )
	   (effect  )
	   (value   "A symbol to define an object that does not exist in ENV."))
  (let ((new-sym-name (concatenate 'string (symbol-name sym)
			      (princ-to-string 1))))
    (when (env~lookup-object new-sym-name env)
      (let ((n 2))
	(loop
	 (setq new-sym-name (concatenate 'string (symbol-name sym)
					 (princ-to-string n)))
	 (unless (env~lookup-object new-sym-name env)
	   (return))
	 (incf n))))
    (intern new-sym-name)))
    

#{
\subsection{Proof Plan Operations} 
#}

(defun pds~label2node (label &optional (pds pds*current-proof-plan))
  (declare (edited  "20-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A name of a node.")
           (effect  "None.")
           (value   "The node in pds*current-proof-plan with this name."))
  (gethash 
        (etypecase label
           (symbol (symbol-name label))
           (string label))
        (pds~label-node-hashtable pds)))


(defun pds~existent-p (label &optional proof-plan)
  (declare (edited  "20-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A name of a node.")
           (effect  "None.")
           (value   "Non-nil iff a node with this name exists in 
pds*current-proof-plan."))
  (pds~label2node label proof-plan))


(defun pds~problem-assumption-p (node &optional (pds pds*current-proof-plan))
  (declare (edited  "24-APR-1997 15:15")
	   (authors LASSAAD)
	   (input   "A node and optional a pds.")
	   (effect  "None.")
	   (value   "T, iff node a hypthesis NODE that corresponds to an"
		    "assumption in the problem of PDS."))
  (find (keim~name node) (mapcar #'keim~name
				 (prob~proof-assumptions pds)))
  )

#{\subsubsection{Backtracking}
While constructing a proof plan backtracking must be carried out, if some open node
cannot be closed. This is done by applying the function pds~delete-node! to remove
the open node and to perform all dependent changes. In this operation we distinguish
three reasons of having a node in the proof plan: A node can be inserted by the user
when loading the problem (theorem or assumption), or adding a hypothesis (assumption),
can be a precondition and/or
a postcondition of an applied method (method application reason), or can be a node
of the expansion of some method (method application reason).

Nodes belonging to the original problem description can only be deleted if these are
assumptions. Assumptions of the problem description which are not used to prove the
theorem, have to be removed when cleaning up the hole proof.

When deleting a node we have first to backtrack all the method expansions to which this
node belong and then all method applications to whose preconditions and/or postconditions
this node belongs. These reasons are deleted from the reason list of each other node
related to the node to be deleted and in the case where a reason list of some node becomes
empty the node itself has to be deleted too.

In addition to the removing of the reason from the reason list of the involved nodes a
method application reason is backtracked by: making open the justified nodes by the method
(preconditions occurring in the closed postconditions) and the nodes inserted as conclusions
(closed postconditions not occurring in the preconditions and not being hypotheses), for the
latter nodes support nodes must be determined from the current proof situation. The support
nodes of a node are (heuristically) its hypotheses and each logical consequence of them which
does not depend logically on this node.
#}

(defgeneric pds~open-node! (node &optional (pds pds*current-proof-plan))
  (declare (edited  "14-MAY-1997")
	   (authors Lassaad)
	   (input   "A closed node, and optional a pds.")
	   (effect  "The plan step, that corresponds to the last method application for"
		    "closing NODE, is backtracked.")
	   (value   "The undone plan steps."))
  (:method ((node pdsn+node) &optional (pds pds*current-proof-plan))
	   (let* ((node-just (node~justification node))
		  (own-reason (pdsj~own-reason node-just)))
	     (if own-reason
		 (pds=open-justification! pds node node-just own-reason)
	       (let ((above-reason (pdsj~above-own-reason node-just)))
		 (if above-reason
		     (pds=open-justification! pds node (pdsc~an-just above-reason) above-reason)
		   (let ((other-reasons (or (pdsj~other-reasons node-just)
					    (pdsj~above-other-reasons node-just))))
		     (if other-reasons
			 (warn "~%The node ~A was not directly closed by a method application.~%It is involved in the justifying of the nodes: ~{~A ~}" (keim~name node) (mapcar #'keim~name (mapcar #'pdsc~an-node other-reasons)))
		       (warn "~%The node ~A is not involved in any method application." (keim~name node)))))))
	     )))


(defun pds=open-justification! (pds node just 
				    &optional (reason (pdsj~own-reason just)))
  (declare (edited  "14-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications, and"
		    "optionally the own reason of the justification, i.e. the plan"
		    "step that justified the node by the justification."
		    "JUST must be generated by a method application, i.e. REASON"
		    "may not be NIL.")
	   (effect  "Open the justification JUST and undo each reason that becomes"
		    "inconsistent when JUST becomes open.")
	   (value   "The undone plan steps."))
  (multiple-value-bind (rest-reasons1 undone-reasons1)
      ;;First undo the below justifications of JUST:
      (pds=undo-below-just-of! pds node just)
    (multiple-value-bind (rest-reasons2 undone-reasons2)
	;;Then undo JUST itself:
	(pds=undo-justification! pds node just reason)
      (let ((new-reasons (union rest-reasons1 rest-reasons2)))
	;;Then after undoing the inconsistent reasons of JUST, adapt the justification of NODE:
	(if (or new-reasons
		(pdsj~above-reasons just))
	    ;;NODE has still reasons, it was therefore not deleted; Open its justification:
	    (let* ((old-ctrl (pdsj~control just))
		   (new-ctrl (pdsc~create new-reasons
					  (pdsc~sponsors old-ctrl)
					  (pdsc~unsponsors old-ctrl))) 
		   (new-just (pdsj~open-just-create new-ctrl))
		   (above-just (pdsj~above just)))
	      ;;Inherit alternative-mmatchings, alternative-methods, applied-critics, and failed-methods:
	      (setf (pdsc~alternative-mmatchings new-ctrl) (pdsc~alternative-mmatchings old-ctrl))
	      (setf (pdsc~alternative-methods new-ctrl) (pdsc~alternative-methods old-ctrl))
	      (setf (pdsc~applied-critics new-ctrl) (pdsc~applied-critics old-ctrl))
	      (setf (pdsc~failed-methods new-ctrl) (pdsc~failed-methods old-ctrl))
	      (setf (pdsc~excluded-methods new-ctrl) (pdsc~excluded-methods old-ctrl))
	      ;;Inherit the above justifications:
	      (when above-just
		(setf (pdsj~above new-just) above-just)
		(setf (pdsj~below above-just) new-just))
	      (setf (node~justification node) new-just)
	      ;;Insert NODE into the open nodes of PDS and remove it from the PDS support nodes
	      (setf (pds~open-nodes pds)
		    (union (list node) (pds~open-nodes pds)))
	      (setf (pds~support-nodes pds)
		    (remove node (pds~support-nodes pds))))
	  (when (equal node (prob~proof-root pds))
	    ;;NODE has no more reasons but it could not be deleted, because it corresponds to
	    ;;the conclusion of PDS; Open its justification:
	    (let* ((old-ctrl (pdsj~control (pdsn~most-abstract-just node)))
		   (new-ctrl (pdsc~create NIL (pdsc~sponsors old-ctrl) (pdsc~unsponsors old-ctrl)))
		   (new-just (pdsj~open-just-create new-ctrl)))
	      (setf (node~justification node) new-just)
	      ;;Inherit alternative-mmatchings, alternative-methods, applied-critics, and failed-methods:
	      (setf (pdsc~alternative-mmatchings new-ctrl) (pdsc~alternative-mmatchings old-ctrl))
	      (setf (pdsc~alternative-methods new-ctrl) (pdsc~alternative-methods old-ctrl))
	      (setf (pdsc~applied-critics new-ctrl) (pdsc~applied-critics old-ctrl))
	      (setf (pdsc~failed-methods new-ctrl) (pdsc~failed-methods old-ctrl))
	      (setf (pdsc~excluded-methods new-ctrl) (pdsc~excluded-methods old-ctrl))
	      ;;Insert NODE into the open nodes of PDS and remove it from the PDS support nodes
	      (setf (pds~open-nodes pds)
		    (union (list node) (pds~open-nodes pds)))
	      (setf (pds~support-nodes pds)
		    (remove node (pds~support-nodes pds))))))
	;;;Return undone steps
	(union undone-reasons1 undone-reasons2)))
    ))

(defun pds=undo-below-just-of! (pds node just)
  (declare (edited  "01-JUL-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications.")
	   (effect  "Undo the reasons of the below justifications of JUST that"
		    "get inconsistent when JUST becomes open.")
	   (value   "A pair of reason lists: the reasons of the below justifications"
		    "of JUST that remain consistent even when JUST becomes open, and"
		    "the undone reasons."))
  (let ((below-just (pdsj~below just)))
    (when below-just
      (multiple-value-bind (rest-reasons1 undone-reasons1)
	  (pds=undo-below-just-of! pds node below-just)
	(multiple-value-bind (rest-reasons2 undone-reasons2)
	    (pds=undo-justification! pds node below-just)
	  (values (union rest-reasons2 (set-difference rest-reasons1 undone-reasons2))
		  (union undone-reasons1 undone-reasons2)))))
    ))

(defun pds=undo-justification! (pds node just &optional reason)
  (declare (edited  "14-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications,"
		    "and optionally one of the justification reasons."
		    "When given, REASON must be the own reason of JUST.")
	   (effect  "Undo the reasons of JUST that get inconsistent when JUST"
		    "gets open.")
	   (value   "A pair of reason lists: the reasons of JUST that remain"
		    "consistent even when JUST becomes open, and the undone"
		    "reasons."))
  (let ((own-reason (if reason reason
		      (pdsj~own-reason just))))
    (multiple-value-bind (rest-reasons undone-reasons)
	(pds=undo-inconsistent-other-reasons! pds node just)
      (if own-reason
	  ;;JUST is created by a method application: Undo this method application by deleting
	  ;;the own reason of JUST from the involved nodes and removing the justifying of NODE
	  ;;with JUST from the PDS plan steps:
	  (let ((more-undone-rs (pds=delete-own-reason! pds node just own-reason)))
	    (pds=remove-plan-step! pds own-reason)
	    (values rest-reasons (union more-undone-rs undone-reasons)))
	(values rest-reasons undone-reasons)))
    ))


(defun pds=undo-inconsistent-other-reasons! (pds node just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, and one of the node justifications.")
	   (effect  "Undo the reasons of JUST that become inconsistent when JUST becomes"
		    "open.")
	   (value   "A pair of reason lists: the reasons of JUST that remain consistent"
		    "even when JUST is open, and the undone reasons."))
  (let* ((other-reasons (pdsj~other-reasons just))
	 (undo-reason (find-if #'(lambda (r)
				   (pdsj~clsd-premise-p (pdsc~an-just r) node))
			       other-reasons)))
    (if undo-reason
	(let ((undone-reasons1 (pds=open-justification! pds
							(pdsc~an-node undo-reason)
							(pdsc~an-just undo-reason)
							undo-reason)))
	  (multiple-value-bind (rest-reasons undone-reasons2)
	      (pds=undo-inconsistent-other-reasons! pds node just)
	    (values rest-reasons (union undone-reasons1 undone-reasons2))))
      ;;;Deletes the other-reasons of JUST
      ;(let ((own-reason (pdsj~own-reason just)))
	;(setf (pdsj~reasons just) (when own-reason (list own-reason)))
	;other-reasons))
      (values other-reasons))
    ))


(defun pds=delete-own-reason! (pds node just &optional (reason (pdsj~own-reason just)))
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications, and"
		    "optionally the justification own reason."
		    "JUST may not have below reasons.")
	   (effect  "Remove REASON from the reason list of JUST and from its sons in PDS.")
	   (value   "The deleted reasons."))
  (when reason
    (let ((the-new-hyps)
	  (undone-reasons))
      (if (pdsj~below-reasons just)
	  ;;;To prevent this error message, you must remove the remaining below reasons
	  ;;;while undoing below justifications of JUST.
	  (warn ";;;PDS=DELETE-OWN-REASON!: ~A may not have below reasons." just)
	(multiple-value-bind (nodes&justs new-hyps)
	    (pds=reason-nodes&justs node just reason)
	  (pds=remove-reason! pds node just reason)
	  (dolist (n&j nodes&justs)
	    (setq undone-reasons
 		  (append undone-reasons
 			  (pds=delete-reason! pds (first n&j) (rest n&j) reason))))
	  (setq the-new-hyps new-hyps)))
      ;;;Consider the twin reasons of REASON, i.e. the reasons that are created at the same
      ;;;tactic application:
      (let* ((outln-pat (pdsj~outline-pattern just))
	     (conc-labels (remove-if #'(lambda (pat) (or (infer~nonexistent-pattern-p pat)
							 (infer~existent-pattern-p pat)
							 (infer~closed-pattern-p pat)))
				     outln-pat)))
	(if conc-labels
	    (progn
	      (dolist (conc-label conc-labels)
		(let ((conc (pds~label2node conc-label pds)))
		  (when conc
	            ;;;The twin reason in CONC is not yet considered
		    (let* ((conc-just (node~justification conc))
			   (the-just (pdsj~get-twin-just conc-just (just~method just) outln-pat))
			   (the-own-reason (pdsj~own-reason the-just)))
		      (when the-own-reason
		        ;;;The twin reason is not yet deleted:
			(setq undone-reasons
			      (union undone-reasons
				     (pds=open-justification! pds conc the-just the-own-reason))))))))
	      (dolist (new-hyp the-new-hyps (cons reason undone-reasons))
		(when (find new-hyp (prob~proof-steps pds))
		  (setq undone-reasons
			(union undone-reasons
			       (pds=delete-node! new-hyp nil pds))))))
	  (dolist (new-hyp the-new-hyps (cons reason undone-reasons))
	    (when (find new-hyp (prob~proof-steps pds))
	      (setq undone-reasons
		    (union undone-reasons
			   (pds=delete-node! new-hyp nil pds))))))))
    ))

(defun pds=remove-reason! (pds node just reason)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications, and"
		    "one of the justification reasons.")
	   (effect  "Deletes REASON from the reason list of JUST. If the reason"
		    "list of NODE becomes empty, NODE has to be removed from PDS.")
	   (value   "Unspecified."))
  (let ((new-reasons (remove reason (pdsj~reasons just))))
    (setf (pdsj~reasons just) new-reasons)
    (unless (or new-reasons
		(pdsj~above-reasons just)
		(pdsj~below-reasons just))
      (let ((root-node (prob~proof-root pds)))
	(unless
	    (or (keim~equal node root-node)
		(find node (pdsn~hyps root-node)))
	  (if (equal just (pdsc~an-just reason))
	      ;;; REASON is the last own reason of NODE, then deletes the premises of
	      ;;; JUST everywhere they occur as unsponsors when deleting NODE.
	      (pds=remove-node! pds node (just~premises just))
	    (pds=remove-node! pds node)))))
    ))


(defun pds=remove-node! (pds node &optional node-prems verbose)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, and optional a node list (the NODE"
		    "premises), and a boolean.")
	   (effect  "Delete NODE from PDS by removing it from the proof steps of PDS,"
		    "from the sponsors and unsponsors of the remaining nodes, and from the"
		    "PDS open nodes. If NODE-PREMS is nonempty, then they will be"
		    "deleted everywhere they occur together as unsponsors.")
	   (value   "Unspecified."))
  (when (pds~label2node (keim~name node) pds)
    (let ((curr-steps (remove node (prob~proof-steps pds)))
	  (unsps-to-delete (if node-prems
			       (cons node node-prems)
			     (list node))))
      (setf (prob~proof-steps pds) curr-steps)
      (remhash (symbol-name (keim~name node)) (pds~label-node-hashtable pds))
      
      (dolist (step curr-steps)
	(dolist (just (pdsn~all-justs step))
	  (when (pdsj~sponsors just)
	    (setf (pdsj~sponsors just) (remove node (pdsj~sponsors just))))
	  (when (pdsj~unsponsors just)
	    (setf (pdsj~unsponsors just) (set-difference (pdsj~unsponsors just) unsps-to-delete)))))
      	  
      (setf (pds~open-nodes pds)
	    (remove node (pds~open-nodes pds)))
      (setf (pds~support-nodes pds)
	    (remove node (pds~support-nodes pds)))
      
      (when verbose
	(pp~pprint node 'pds-simple))
      )))


(defun pds=delete-reason! (pds node just reason)
  (declare (edited  "04-NOV-1999" "15-MAY-1997")
	   (authors Jzimmer Lassaad)
	   (input   "A pds, one of its nodes, one of the node justifications, and one"
		    "of the justification reasons.")
	   (effect  "Delete REASON from the reason list of JUST. If NODE has no more"
		    "reasons, then it is deleted too. When NODE has only own reasons,"
		    "then backtrack each of them which occurs after REASON.")
	   (value   "The undone reasons related to the own reason of NODE."))
  (when just     ;; added this line, jzimmer
    (pds=remove-reason! pds node just reason)
    ;;After deleting REASON from NODE: If all the remaining reasons of NODE are own
    ;;reasons, then undo each that occurs after REASON, i.e. belongs to a below
    ;;justification of JUST or belongs to JUST and is one of the JUST successors.
    (unless (or (pdsj~other-reasons just)
		(pdsj~above-other-reasons just)
		(pdsj~below-other-reasons just))
      (let ((own-reason (pdsj~own-reason just)))
	(if (and own-reason
		 (find own-reason
		       (pdsj~successors (pdsc~an-just reason))
		       :test #'keim~equal))
	    ;;The own reason of JUST belongs to the successors of REASON which means that this
	    ;;own reason is created after REASON.
	    (pds=open-justification! pds node just own-reason)
	  (let ((below-reason (pdsj~below-own-reason just)))
	    (when below-reason
	      (pds=open-justification! pds node (pdsc~an-just below-reason) below-reason))))
	))))

;;REMARK1:
;;sponsor and unsponsor
;;by method application
;;by method expansion: new open nodes get as default the method premises as sponsors
;;(and perhaps as well the new inserted hypotheses). Nodes which become open get 
;;as default the sponsors of its previous justification.
;;REMARK2:
;;When POST-reading a PDS file, a reason should be created only once. This can be reached
;;by creating only own reasons. Other reasons should be retrieved. Another possibility is
;;by POST-printing a PDS, only own reasons are considered and must be output with the
;;node justification pairs, that include it. These node, justification pairs can be computed
;;by the below function. 
(defun pds=reason-nodes&justs (node just reason)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A node, one of its justifications, and the justification own reason.")
	   (effect  "None.")
	   (value   "A pair:"
		    "- A node justification pair list of all nodes but the REASON root that"
		    "contain REASON."
		    "- The new hypotheses that are inserted while carrying out REASON."))
  (let* ((node-hyps (pdsn~hyps node))
	 (just-prems (just~premises just))
	 (new-hyps (apply #'append
			  (mapcar #'(lambda (n)
				      (set-difference (pdsn~hyps n) node-hyps))
				  just-prems)))
	 (leaf-nodes (union just-prems new-hyps))
	 (the-below-just (pdsj~least-below-just just))
	 (below-reason (pdsj~below-own-reason just)))
    (if the-below-just
	;;JUST was expanded:
	(if below-reason
	    ;;to an open justification which was closed by a method application. The closed
	    ;;justification corresponds to the BELOW-REASON justification: Use the sponsors
	    ;;and the premises of this justification as candidates to compute the REASON involved
	    ;;node, justification pairs.
	    (let* ((the-just (pdsc~an-just below-reason))
		   (cand-nodes (union (just~premises the-just)
				      (pdsj~sponsors the-just))))
	      (values (pds=get-reason-nodes&justs reason
						  (set-difference cand-nodes leaf-nodes)
						  leaf-nodes)
		      new-hyps))
	  (if (pdsj~open-just-p the-below-just)
	      ;;to an open justification which is still open: Use the sponsors of this justification
	      ;;and the premises of its above justification as candidates to compute the REASON involved
	      ;;node, justification pairs.
	      (let ((cand-nodes (union (pdsj~sponsors the-below-just)
				       (just~premises (pdsj~above the-below-just)))))
		(values (pds=get-reason-nodes&justs reason
						    (set-difference cand-nodes leaf-nodes)
						    leaf-nodes)
			new-hyps))
	    ;;to a closed justification: Use the premises of this justification as candidates to
	    ;;compute the REASON involved node, justification pairs.
	    (values (pds=get-reason-nodes&justs reason
						(set-difference (just~premises the-below-just) leaf-nodes)
						leaf-nodes)
		    new-hyps)))
      ;;; JUST was not expanded: Use the new hypotheses and the sponsors and unsponsors of the premises
      ;; JUST-PREMS as candidates to compute the REASON involved node, justification pairs:
      (let ((cand-nodes))
	(dolist (prem just-prems)
	  (let ((prem-just (node~justification prem)))
	    (setq cand-nodes (union cand-nodes (union (pdsj~sponsors prem-just)
						      (pdsj~unsponsors prem-just))))))
	(values (pds=get-reason-nodes&justs reason (set-difference cand-nodes leaf-nodes) leaf-nodes)
		new-hyps)))
    ))


(defun pds=get-reason-nodes&justs (reason roots leafs)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A reason, and two node lists.")
	   (effect  "None.")
	   (value   "All nodes of the trees with roots in ROOTS and with leafs in LEAFS"
		    "with their justifications that contain REASON."))
  (if roots
      (let* ((root (first roots))
	     (root-just (node~justification root)))
	(if (pdsn~hypothesis-node-p root)
	    (if (find reason (pdsj~reasons root-just))
		(cons (cons root root-just)
		      (pds=get-reason-nodes&justs reason (rest roots) leafs))
	      (pds=get-reason-nodes&justs reason (rest roots) leafs))
	  (let* ((reason-just (pdsj~with-reason-p root-just reason))
		 (the-just (if reason-just reason-just root-just))
		 (the-below-just (pdsj~least-below-just the-just))
		 (the-own-reason (or (pdsj~own-reason the-just)
				     (pdsj~below-own-reason the-just)))
		 (cand-roots
		  (if the-below-just
		      (if the-own-reason
			  (let ((just (pdsc~an-just the-own-reason)))
			    (union (just~premises just)
				   (pdsj~sponsors just)))
			(if (pdsj~open-just-p the-below-just)
			    (union (pdsj~sponsors the-below-just)
				   (just~premises (pdsj~above the-below-just)))
			  (just~premises the-below-just)))
		    (just~premises the-just)))
		 (new-roots (set-difference cand-roots leafs)))
	    (if reason-just
		(cons (cons root reason-just)
		      (pds=get-reason-nodes&justs reason
						  (remove-duplicates (append (rest roots) new-roots))
						  leafs))
	      (pds=get-reason-nodes&justs reason
					  (remove-duplicates (append (rest roots) new-roots))
					  leafs)))))
    (mapcar #'(lambda (leaf)
		(cons leaf (pdsj~with-reason-p (node~justification leaf) reason)))
	    leafs)
    ))


(defgeneric pds~delete-node! (node &optional verbose pds) 
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A node or a list of nodes, and optionally a boolean, and a pds.")
	   (effect  "Delete NODE from PDS.")
	   (value   "The undone plan steps."))
  (:method ((node pdsn+node) &optional (verbose nil) (pds pds*current-proof-plan))
	   (when (eq node (prob~proof-root pds))
	     (warn "PDS~~DELETE-NODE!: Won't delete root node of proof plan.")
             (return-from pds~delete-node!))
	   (when (find node (pdsn~hyps (prob~proof-root pds)))
	     (warn "PDS~~DELETE-NODE!: Won't delete hypothesis of root node of proof plan.")
             (return-from pds~delete-node!))
	   (when (pds~label2node (keim~name node) pds)
	     (pds=delete-node! node verbose pds)))
  (:method ((nodes list) &optional verbose (pds pds*current-proof-plan))
	   (let ((undone-steps))
	     (dolist (node nodes undone-steps)
	       (setq undone-steps
		     (union undone-steps (pds~delete-node! node verbose pds))))))
  (:method (node &optional verbose pds)
	   (declare (ignore verbose pds))
	   (error ";;;PDS~~DELETE-NODE!: ~A is not of type PDSN+NODE." node))
  )


(defun pds=delete-node! (node verbose pds)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A node, a boolean, and a pds.")
	   (effect  "Delete NODE from the PDS after backtracking its reasons.")
	   (value   "The undone plan steps."))
  ;;;First undo the below reasons, then the most abstract one, and
  ;;;then undo the rest reasons which remains consistent even when
  ;;;NODE gets open.
  (let ((just (pdsn~most-abstract-just node)))
    (multiple-value-bind (below-rs undone-rs1)
	(pds=undo-below-just-of! pds node just)
      (multiple-value-bind (self-rs undone-rs2)
	  (pds=undo-justification! pds node just)
	(let ((undone-steps (union undone-rs1 undone-rs2)))
	  (dolist (reason (append below-rs self-rs))
	    (let ((reason-node (pdsc~an-node reason)))
	      (when (pds~label2node (keim~name reason-node) pds)
		(setq undone-steps
		      (union undone-steps
			     (pds=open-justification! pds reason-node
						      (pdsc~an-just reason) reason))))))
	  ;;; Delete node:
	  (when (pds~label2node (keim~name node) pds)
	    (pds=remove-node! pds node nil verbose))
	  ;;; Return undone-steps
	  undone-steps)))
    ))


; Lassaad's Version:
;;pds~cleanup should be applied only if the proof plan is full expanded and does not have
;;open nodes. On more abstract levels you can apply the subprocedure pds=cleanup-same.
;(defun pds~cleanup (&optional (pds pds*current-proof-plan) verbose)
;  (declare (edited  "20-MAY-1997")
;           (authors Lassaad)
;           (input   "None.")
;           (effect  "Removes nodes not used in the PDS proof and removes redundant"
;                    "steps, where a node is justified by a node exactly like it. PDS"
;                    "must have no remaining open nodes and all nodes must be expanded,"
;                    "otherwise an error is signaled.")
;           (value   "Undefined."))
;  (if (pds~open-nodes pds)
;      (error "There are still open nodes. Cleanup not applicable.")
;    (if (some #'(lambda (n) (not (pdsn~grounded-p n)))
;              (prob~proof-steps pds))
;        (error "There are still non-grounded nodes. Cleanup not applicable.")
;      (progn
;        (pds=cleanup-same! pds)
;        (pds=cleanup-hyps! (prob~proof-root pds))
;        (pds=cleanup-justifying! pds verbose)
;        ))))


; DEF's Version:
(defun pds~cleanup (&optional (pds pds*current-proof-plan) verbose)
  (declare (edited  "24-MAR-1998")
	   (authors Fehrer Lassaad)
	   (input   "None.")
	   (effect  "Removes nodes not used in the PDS proof and removes redundant"
		    "steps, where a node is justified by a node exactly like it. PDS"
		    "must have no remaining open nodes and all nodes must be expanded,"
		    "otherwise an error is signaled.")
	   (value   "Undefined."))
  (if (pds~open-nodes pds)
      (error "There are still open nodes. Cleanup not applicable.")
;    (if (some #'(lambda (n) (not (pdsn~grounded-p n)))
;              (prob~proof-steps pds))
;        (error "There are still non-grounded nodes. Cleanup not applicable.")
      (progn
;        (pds=cleanup-hyps! (prob~proof-root pds)) ; nicht vor cleanup-same machen,
;        (format T "~%Hyps durch")                 ; man kann so mehr Varianten finden; DEF
        (pds=cleanup-justifying! pds verbose)
;        (format T "~%Justs durch")
	(pds=cleanup-same*! pds)
;	(format T "~%same durch")
        (pds=cleanup-hyps! (prob~proof-root pds))
;        (format T "~%Hyps durch")
;        (pds=cleanup-justifying! pds verbose)     ; geht nicht mehr hinter cleanup-same,
;        (format T "~%Justs durch")                ;wegen der reasons; ist aber auch nicht noetig,
	)))                                        ; da dies selbst remove-node! aufruft; DEF


; Lassaad's Version
;(defun pds=cleanup-same! (pds)
;  (declare (edited  "20-MAY-1997")
;           (authors Lassaad)
;           (input   "A pds.")
;           (effect  "Each node in PDS which is justified by the method WEAKEN"
;                    "is merged with its justifying node.")
;           (value   "Unspecified."))
;  (dolist (node (prob~proof-steps pds))
;    (when (equal (pdsn~just-method node) (infer~find-method 'weaken))
;      (pds=merge-nodes! pds node (first (pdsn~just-premises node))))
;    ))


(defun pds=cleanup-same*! (pds)
  (declare (edited  "24-MAR-1998") 
	   (authors Fehrer)
	   (input   "a pds")
	   (effect  "Nodes in the proof are checked one by one. If a node"
		    "is identical to another one whose"
		    "hypotheses are a subset of those of the first one, then the first node is"
		    "replaced by the node that justifies it in the justifying-nodes and hypotheses"
		    "of all other nodes in the proof.")
	   (value   "undefined"))
  (let* ((tested-nodes nil)
	 (root-node (prob~proof-root pds)))
    (dolist (node (remove root-node (pds~ordered-premises root-node)))
      (let ((jnodes (reverse tested-nodes)); important !!! DEF
	    (hyps (pdsn~hyps node))
	    (formula (node~formula node)))
	(dolist (jnode jnodes)
	  (when (and (subsetp (pdsn~hyps jnode) hyps)
		     (data~equal formula
				 (node~formula jnode)))
	    (pds=change-refs node jnode pds)
	    (return))))
      (push node tested-nodes))))

(defun pds=change-refs (from-node to-node pds)
  (declare (edited  "24-MAR-1998")
	   (authors Fehrer)
	   (input   "two nodes and an enclosing pds")
	   (effect  "The first node will be replaced by the second one everywhere it"
		    "occurs in the given pds as a hypothesis or justifying node.")
	   (value   "undefined"))
  (dolist (node (prob~proof-steps pds)
	    (pds=remove-node! pds from-node)
	    )
    (setf (pdsn~hyps node)
	  (nsubst to-node from-node (pdsn~hyps node)))
    (setf (just~premises (node~justification node))
	  (nsubst to-node from-node (pdsn~just-premises node)))))


(defun pds=merge-nodes! (pds node1 node2)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, and two of its nodes: NODE1 is justified by the method WEAKEN"
		    "from NODE2.")
	   (effect  "Replaces the WEAKEN justification of NODE1 by NODE2 justification and"
		    "changes some outline-patterns to prevent inconsistencies in the PDS"
		    "history. NODE2 is then deleted.")
	   (value   "Unspecified."))
  ;;;LC: If NODE2 is justified with WEAKEN too, then merge first NODE2 
  ;;; with its justification:
  (when (equal (pdsn~just-method node2) (infer~find-method 'weaken))
      (pds=merge-nodes! pds node2 (first (pdsn~just-premises node2))))
  ;;;LC: Here we assume that just1 and just2 have no above justs.
  (let ((just1 (node~justification node1))
	(just2 (node~justification node2)))
    (if (or (pdsj~above just1) (pdsj~above just2))
	(error ";;;PDS=MERGE-NODES: You must consider above justifications.")
      (let ((weaken-r (pdsj~own-reason just1))
	    (other-rs1 (pdsj~other-reasons just1)))
	;;;Delete the weaken reason:
        (pdsj~remove-reason! just1 weaken-r)
        (pdsj~remove-reason! just2 weaken-r)
        (pds=remove-plan-step! pds weaken-r)
	(if (pdsn~hypothesis-node-p node2)
	    ;;;NODE2 is a hypothesis: NODE1 had to be created after NODE2, otherwise
	    ;;;NODE1 might not use the hypothesis NODE2 as a premise:
	    (dolist (j (mapcar #'pdsc~an-just other-rs1))
	      (pds=adapt-prem-outline-of-just! node1 j))
	  ;;;NODE2 is justified by a forward method:
	  (let ((own-r2 (pdsj~own-reason just2))
		(r2-preds (pdsj~predecessors just2)))
	    (if (some #'(lambda (r) (find r r2-preds)) other-rs1)
		;;;NODE1 was created before NODE2:
		(pds=adapt-conc-outline-of-just! just2)
	      ;;; NODE2 was created before NODE1:
	      (dolist (j (mapcar #'pdsc~an-just other-rs1))
		(pds=adapt-prem-outline-of-just! node1 j)))
	    ;;;As NODE1 will get the justification of NODE2, we replace NODE2 with
	    ;;;NODE1 in the plan step OWN-R2:
	    (setf (pdsc~an-node own-r2) node1)))
	;;;Replace the justification of NODE1 with that of NODE2
	(setf (node~justification node1) just2)
	;;;Replace NODE2 by NODE1 everywhere it occurs as a premise (in the reasons of
	;;;NODE2) and as support node: 
	(dolist (r (pdsj~other-reasons just2))
	  (pdsj~substitute-premise! (pdsc~an-just r) node2 node1))
	(pds=substitute-in-supports! node2 node1 pds)
	;;;Insert the old other reasons of NODE1 into its new justification
	(dolist (r other-rs1)
	  (pdsj~insert-reason! just2 r))
	;;;Remove NODE2:
	(pds=remove-node! pds node2)))
    ))


(defun pds=adapt-prem-outline-of-just! (node just)
  (declare (edited  "23-JUN-1997")
	   (authors Lassaad)
	   (input   "A node and a justification: NODE can be a premise"
		    "of JUST.")
	   (effect  "Adapt the justification JUST so that, the outline"
		    "of its premise NODE is either EXISTENT or CLOSED.")
	   (value   "Unspecified."))
  (let ((prem-outline (pdsj~premise-outline just node)))
    (when prem-outline
      (unless (infer~nonexistent-pattern-p prem-outline)
	(let* ((just-outline (pdsj~outline-pattern just))
	       (just-prems (just~premises just))
	       (prem-oln-pos (+ (position node just-prems)
				(- (length just-outline)
				   (length just-prems))))
	       (new-oln-pat1 (substitute "EXISTENT"
					 prem-outline
					 just-outline
					 :start prem-oln-pos
					 :end (+ prem-oln-pos 1)))
	       (just-method (just~method just))
	       (exist-appl (infer~outline-pattern2application
			    just-method new-oln-pat1)))
	  (if exist-appl
	      (if (pdsj~subst just)
	          (error ";;;PDS=ADAPT-PREM-OUTLINE-OF-JUST: The corresponding method application must be replayed.")
		(setf (pdsj~outline-pattern just) new-oln-pat1))
	    (let* ((new-oln-pat2 (substitute "CLOSED" 
					     prem-outline
					     just-outline
					     :start prem-oln-pos
					     :end (+ prem-oln-pos 1)))
		   (exist-appl (infer~outline-pattern2application
			        just-method new-oln-pat2)))
	      (if exist-appl
		  (if (pdsj~subst just)
	              (error ";;;PDS=ADAPT-PREM-OUTLINE-OF-JUST: The corresponding method application must be replayed.")
		    (setf (pdsj~outline-pattern just) new-oln-pat2))
		(error ";;;PDS=ADAPT-PREM-OUTLINE-OF-JUST: You must define an application with the outline pattern ~A or ~A for the inference method ~A." new-oln-pat1 new-oln-pat2 just-method))))
	  )))
    ))


(defun pds=adapt-conc-outline-of-just! (just)
  (declare (edited  "23-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification, the outline of its conclusion"
		    "must be NONEXISTENT.")
	   (effect  "Adapt the justification JUST so that, the outline"
		    "of its conclusion NODE is EXISTENT.")
	   (value   "Unspecified."))
  (let ((conc-outline (pdsj~conclusion-outline just)))
    (if (infer~existent-pattern-p conc-outline)
	(error ";;;PDS=ADAPT-CONC-OUTLINE-OF-JUST!: Adapting an existent conclusion outline.")
      (let* ((just-outline (pdsj~outline-pattern just))
	     (conc-oln-pos (position conc-outline just-outline))
	     (new-outln-pat (substitute "EXISTENT"
					 conc-outline
					 just-outline
					 :start conc-oln-pos
					 :end (+ conc-oln-pos 1)))
	     (just-method (just~method just))
	     (exist-appl (infer~outline-pattern2application
			  just-method new-outln-pat)))
	(if exist-appl
	    (if (pdsj~subst just)
	        (error ";;;PDS=ADAPT-PREM-OUTLINE-OF-JUST: The corresponding method application must be replayed.")
	      (setf (pdsj~outline-pattern just) new-outln-pat))
	  (error ";;;PDS=ADAPT-CONC-OUTLINE-OF-JUST: You must define an application with the outline pattern ~A for the inference method ~A." new-outln-pat just-method))))
    ))

(defun pds=substitute-in-supports! (node1 node2 pds)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "Tow nodes, and a pds.")
	   (effect  "Replaces NODE1 by NODE2 everywhere it occurs as a support node.")
	   (value   "Unspecified."))
  (when (find node1 (pds~support-nodes pds))
    (nsubstitute node2 node1 (pds~support-nodes pds))
    (pds=substitute-in-unsponsors! node1 node2 pds))
  (pds=substitute-in-sponsors! node1 node2 pds))


(defun pds=substitute-in-unsponsors! (node1 node2 pds)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "Two nodes, and a pds.")
	   (effect  "Replaces NODE1 by NODE2 everywhere it occurs in the unsponsors"
		    "of a PDS node.")
	   (value   "Unspecified."))
  (let* ((first-plstp (pds~first-plan-step pds))
	 (pds-plstps (when first-plstp
			(cons first-plstp (pdsj~successors (pdsc~an-just first-plstp))))))
    (dolist (plstp pds-plstps)
      (pdsc~substitute-unsponsor! (pdsj~control (pdsc~an-just plstp)) node1 node2))
    (dolist (node (pds~open-nodes pds))
      (pdsc~substitute-unsponsor! (pdsj~control (node~justification node)) node1 node2))
    ))


(defun pds=substitute-in-sponsors! (node1 node2 pds)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "Two nodes, and a pds.")
	   (effect  "Replaces NODE1 by NODE2 everywhere it occurs in the sponsors"
		    "of a PDS node.")
	   (value   "Unspecified."))
  (let* ((first-plstp (pds~first-plan-step pds))
	 (pds-plstps (when first-plstp
			(cons first-plstp (pdsj~successors (pdsc~an-just first-plstp))))))
    (dolist (plstp pds-plstps)
      (pdsc~substitute-sponsor! (pdsj~control (pdsc~an-just plstp)) node1 node2))
    (dolist (node (pds~open-nodes pds))
      (pdsc~substitute-sponsor! (pdsj~control (node~justification node)) node1 node2))
    ))


(defun pds=cleanup-hyps! (node)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "Minimizes the hypotheses of NODE and recursively of its premises"
		    "to those actually used in its proof.")
	   (value   "Unspecified."))
  (if (pdsn~hypothesis-node-p node)
      (setf (pdsn~hyps node) (list node))
    (let ((prems (pdsn~just-ass&premises node)))
      (dolist (prem prems)
	(pds=cleanup-hyps! prem))
      (let ((prems-hyps (remove-duplicates (apply #'append
						  (mapcar #'pdsn~hyps prems)))))
	(setf (pdsn~hyps node)
	      (intersection (pdsn~hyps node) prems-hyps))))
    ))


(defun pds=cleanup-justifying! (pds verbose)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, and a boolean.")
	   (effect  "Removes nodes not used in PDS, i.e. are not used in justifying"
		    "the root node of PDS.")
	   (value   "Unspecified."))
  (let ((nodes (set-difference (prob~proof-steps pds)
			       (pdsn~justifying-nodes (list (prob~proof-root pds))))))
      (pds=cleanup-nodes! pds nodes verbose)
      ))


(defun pds=cleanup-nodes! (pds nodes verbose)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A PDS, some of its nodes, and a boolean.")
	   (effect  "Deletes each node in NODES from PDS after undoing"
		    "its inconsistent reasons.")
	   (value   "Unspecified."))
  (when nodes
    (let ((node (first nodes)))
      (when (pds~label2node (keim~name node) pds)
	  (pds=cleanup-node! pds node verbose))
      (pds=cleanup-nodes! pds (rest nodes) verbose))
    ))
	

(defun pds=cleanup-node! (pds node verbose)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, one of its nodes, and a boolean.")
	   (effect  "Deletes NODE from the PDS after backtracking its"
		    "inconsistent reasons.")
	   (value   "Undefined."))
  (when (pds~problem-assumption-p node pds)
    (format t "~%The assumption ~S has never been used. You should remove it from the
problem file.~%" node)
    (setf (prob~assumptions (prob~proof-problem pds))
	  (remove node (prob~proof-assumptions pds))))
  (let* ((just (pdsn~most-abstract-just node))
	 ;;;LC: Anpassen: see pds=delete-node!
	 (rest-reasons (pds=undo-justification! pds node just)))
    (dolist (reason rest-reasons)
      (when (find node (just~premises (pdsc~an-just reason)))
	(pds=open-justification! pds (pdsc~an-node reason)
				 (pdsc~an-just reason) reason)))
    (when (pds~label2node (keim~name node) pds)
      (pds=remove-node! pds node nil verbose))
    ))


(defun pds~plan-steps (&optional (pds pds*current-proof-plan))
  (declare (edited  "09-MAY-1997")
	   (authors Lassaad)
	   (input   "Optional a pds.")
	   (effect  "None.")
	   (value   "PDS plan steps from the first to the last one."))
  (let ((plstp (pds~first-plan-step pds)))
    (when plstp
      (cons plstp (pdsj~successors (pdsc~an-just plstp))))
    ))


(defun pds~ordered-premises (node)
  (declare (edited  "02-APR-1998")
	   (authors Fehrer)
	   (input   "a node")
	   (effect  "none")
	   (value   "a list of the node's premises (recursive!) in ascending order,"
		    "including node itself"))
  (if (pdsn~hypothesis-p node)
      (list node)
    (append (remove-duplicates (apply #'append (mapcar #'pds~ordered-premises (pdsn~just-premises node))) :from-end T)
	    (list node))))

;(defun pds=sort-nodes (nodes)
;  (declare (edited  "19-SEP-1996")
;           (authors Lassaad)
;           (input   "A node list.")
;           (effect  "Put nodes in order such that a node comes after its hypothesis"
;                    "and its justification premises on every abstract level." )
;           (value   "The sorted list NODES. Destructive."))
;  (stable-sort nodes
;               #'(lambda (x y)
;                   (or (member x (pdsn~hyps y))
;                       (member x (pdsn~just-all-premises y))))))


;; simplistic, can have better ones that take advantage of knowing
;; where this line comes from..
(defun pds~insert-node! (line &optional (proof pds*current-proof-plan))
  (declare (edited  "23-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A new node and a proof plan.")
           (effect  "Insert the node into the proof plan before nodes it supports
and after all its hypotheses.")
           (value  "Undefined."))
  (let ((hashtable (pds~label-node-hashtable proof)))
    (setf (gethash (symbol-name (keim~name line)) hashtable)
	  line))
  (when (pdsn~open-node-p line)
    (setf (pds~open-nodes proof)
	  (union (list line) (pds~open-nodes proof))))
  (let* ((supports (if (pdsn~open-node-p line)
		       (pds~node-supports line)
		     (pdsn~just-all-premises line)))
	 (hyps (pdsn~hyps line))
	 (hyps-and-supports (append (copy-list supports)
				    (copy-list hyps)))
	 (proof-lines* (prob~proof-steps proof)))
    ;; walk down proof-lines, just stick this guy in after all his
    ;; hyps and lines that justify him 
    (let ((next-line-pos 
	   (or (position-if
		#'(lambda (pline)
		    (null (intersection (member pline proof-lines*)
					hyps-and-supports)))
		proof-lines*)
	       (length proof-lines*))))
      (setf (prob~proof-steps proof)
	    (append (subseq proof-lines* 0 next-line-pos)
		    (cons line (subseq proof-lines* next-line-pos)))))
    ))

(defun pds~only-insert-node! (line &optional (proof pds*current-proof-plan))
  (declare (edited  "23-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A new node and a proof plan.")
           (effect  "Insert the node into the proof plan before nodes it supports
and after all its hypotheses.")
           (value  "Undefined."))
  (let ((hashtable (pds~label-node-hashtable proof)))
    (setf (gethash (symbol-name (keim~name line)) hashtable)
	  line))
  (let* ((supports (if (pdsn~open-node-p line)
		       (pds~node-supports line)
		     (pdsn~just-all-premises line)))
	 (hyps (pdsn~hyps line))
	 (hyps-and-supports (append (copy-list supports)
				    (copy-list hyps)))
	 (proof-lines* (prob~proof-steps proof)))
    ;; walk down proof-lines, just stick this guy in after all his
    ;; hyps and lines that justify him 
    (let ((next-line-pos 
	   (or (position-if
		#'(lambda (pline)
		    (null (intersection (member pline proof-lines*)
					hyps-and-supports)))
		proof-lines*)
	       (length proof-lines*))))
      (setf (prob~proof-steps proof)
	    (append (subseq proof-lines* 0 next-line-pos)
		    (cons line (subseq proof-lines* next-line-pos)))))
    ))

; insert new-line after the earlier lines but before the later lines
(defun pds~insert-node-after-before! (line earlier-lines later-lines
					   &optional (proof pds*current-proof-plan))
  (declare (edited  "23-SEP-1996" "09-FEB-1993 12:46")
	   (authors Lassaad nesmith)
	   (input   "A node, a list of earlier nodes, and a list of 
later nodes.  PROOF is an optional argument which defaults to 
PDS*CURRENT-PROOF-PLAN.")
	   (effect  "The LINE is inserted into the proof steps so that is
comes after all the EARLIER-LINES and before all the LATER-LINES.")
	   (value   "Undefined."))
  (let ((hashtable (pds~label-node-hashtable proof)))
    (setf (gethash (symbol-name (keim~name line)) hashtable)
	  line))
  (when (pdsn~open-node-p line)
    (setf (pds~open-nodes proof)
	  (union (list line) (pds~open-nodes proof))))
  (let* ((proof-lines* (prob~proof-steps proof))
	 (next-line-pos 
	  (or (if later-lines 
		  (apply #'min
			 (mapcar #'(lambda (x) (position x proof-lines*))
				 later-lines))
		(position-if
		 #'(lambda (pline)
		     (null (intersection (member pline proof-lines*)
					 earlier-lines)))
		 proof-lines*))
	      (length proof-lines*))))
    (setf (prob~proof-steps proof)
	  (append (subseq proof-lines* 0 next-line-pos)
		  (cons line (subseq proof-lines* next-line-pos)))))
  )

; insert new-line after the earlier lines but before the later lines
(defun pds~only-insert-node-between! (line earlier-lines later-lines
					   &optional (proof pds*current-proof-plan))
  (declare (edited  "21-JAN-1998" "08-JUL-1997")
	   (authors Jzimmer Lassaad)
	   (input   "A node, a list of earlier nodes, and a list of "
		    "later nodes.  PROOF is an optional argument which defaults to "
		    "PDS*CURRENT-PROOF-PLAN.")
	   (effect  "The LINE is inserted into the proof steps so that is"
		    "comes after all the EARLIER-LINES and before all the LATER-LINES.")
	   (value   "Undefined."))
  
  (let ((hashtable (pds~label-node-hashtable proof)))
    (setf (gethash (symbol-name (keim~name line)) hashtable)
	  line))
  (let* ((proof-lines* (prob~proof-steps proof))
	 (next-line-pos 
	  (or (if later-lines 
		  (let ((pos-list
			 (apply #'append
				(mapcar #'(lambda (x)
					    (let ((pos (position x proof-lines*)))
					      (if pos
						  (list pos)
						NIL)))
					later-lines))))
		    (if pos-list
			(apply #'min pos-list)
		      0))
		(position-if
		 #'(lambda (pline)
		     (null (intersection (member pline proof-lines*)
					 earlier-lines)))
		 proof-lines*))
	      (length proof-lines*))))
    (setf (prob~proof-steps proof)
	  (append (subseq proof-lines* 0 next-line-pos)
		  (cons line (subseq proof-lines* next-line-pos)))))
  )

(defun pds=not-on-path-to-hyps-p (roots tests)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "Two lists of pds nodes.")
	   (value   "NIL, iff each node in TESTS is on a path from a node in ROOTS to a"
		    "hypothesis, otherwise T. The path is via the premises, if there is"
		    "no open node on the path, otherwise it is via the support nodes of"
		    "the open node."))
  (cond ((null tests)
	 nil)
	((null roots)
	 t)
	((and roots tests)
	 (pds=not-on-path-to-hyps-p
	  (apply #'append (mapcar #'(lambda (r)
				      (let ((just (pdsn~least-abstract-just r)))
					(unless (pdsn~hypothesis-p r)
					  (if (pdsj~open-p just)
					      (pds~node-supports r)
					    (just~premises just)))))
				  roots))
	  (remove-if #'(lambda (test) (find test roots)) tests)))
	(t (error "Something is wrong in function PDS=NOT-ON-PATH-TO-HYPS-P"))))

(defun pds~insert-justification (node just)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "A pds node and a justification.")
	   (effect  "Inserts JUST in NODE's justification list w.r.t. its level of"
		    "abstraction.")
	   (value   "Undefined."))
  (labels ((insert-just (low new)
			(if (pds=not-on-path-to-hyps-p (just~premises low)
						       (just~premises new))
			    (pdsj~insert-just-below low new)
			  (let ((above (pdsj~above low)))
			    (if above
				(insert-just above new)
			      (pdsj~insert-just-above low new))))))
    (if (pdsj~open-just-p (node~justification node))
	(setf (node~justification node) just)
      (insert-just (pdsn~least-abstract-just node) just))))

(defun pds~insert-actual-justification (node just)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "A pds node and a justification.")
	   (effect  "Inserts JUST in NODE's justification list as the actual"
		    "justification w.r.t. its level of abstraction.")
	   (value   "Undefined."))
  (pds~insert-justification node just)
  (setf (node~justification node) just))


(defun pds~proof-done-p (&optional (pds pds*current-proof-plan))
  (declare (edited  "04-JUL-1997")
           (authors Lassaad)
           (input   "A pds (defaults to pds*current-proof-plan).")
           (effect  "none.")
           (value  "True if proof is complete, i.e. all nodes are grounded, nil otherwise."))
  (let* ((open-nodes (pds~open-nodes pds))
	 (all-nodes (prob~proof-steps pds))
	 (ng-nodes (remove-if #'pdsn~grounded-p all-nodes)) 
	 (uu-nodes (set-difference ng-nodes open-nodes))
	 (ue-nodes (remove-if #'pdsn~untested-p uu-nodes))
	 (ut-nodes (set-difference uu-nodes ue-nodes)))
    (cond (open-nodes
	   (inter~print-warning (comint~interface comint*current-comint)
				"There are still open nodes ~A ." (mapcar #'keim~name open-nodes))
	   (when ut-nodes
	     (inter~print-warning (comint~interface comint*current-comint)
				  "There are also nodes justified by black boxes which are not yet called ~A ." (mapcar #'keim~name ut-nodes)))
	   (when ue-nodes
	     (inter~print-warning (comint~interface comint*current-comint)
				  "There are also still unexpanded nodes ~A ." (mapcar #'keim~name ue-nodes)))
	   (return-from pds~proof-done-p nil))
	  (ut-nodes
	   (inter~print-warning (comint~interface comint*current-comint)
				"There are still nodes justified by black boxes which are not yet called ~A ." (mapcar #'keim~name ut-nodes))
	   (when ue-nodes
	     (inter~print-warning (comint~interface comint*current-comint)
				  "There are also still unexpanded nodes ~A ." (mapcar #'keim~name ue-nodes)))
	   (return-from pds~proof-done-p nil))
	  (ue-nodes
	   (inter~print-warning (comint~interface comint*current-comint)
				"There are still unexpanded nodes ~A ."
				(mapcar #'keim~name ue-nodes))
	   (return-from pds~proof-done-p nil))
	  ((not all-nodes)
	   (inter~print-warning (comint~interface comint*current-comint)
				"The proof doesn't contain any lines!")
	   (return-from pds~proof-done-p nil))
	  (t
	   (inter~output-object (comint~interface comint*current-comint)
		                "You are done! All nodes are grounded!")
	   t))))
	   

(defgeneric pds~add-sponsors (open-node newsupports &optional (pds pds*current-proof-plan))
  (declare (edited  "15-OCT-1999" "10-JUN-1997"  "28-JUL-92 10:00")
           (authors Sorge Lassaad NESMITH)
           (input   "An open node, a list of new support nodes, and optionally a pds.")
           (effect  "Adds new support nodes to the support nodes of"
		    "the open node.")
           (value  "The new list of support nodes."))
  (:method ((open-node pdsn+node) newsupports  &optional (pds pds*current-proof-plan))
	   (let* ((newsupports (if (every #'consp newsupports)
				   (apply #'append newsupports)
				 newsupports))
		  (pds-supports (pds~support-nodes pds))
		  (supp-supports (remove-if-not #'(lambda (n) (find n pds-supports))
						newsupports))
		  (other-supports (set-difference newsupports supp-supports)))
	     (setf (pdsn~just-sponsors open-node)
		   (union (pdsn~just-sponsors open-node) other-supports))
	     (setf (pdsn~just-unsponsors open-node)
		   (set-difference (pdsn~just-unsponsors open-node) newsupports))))
  (:method ((open-nodes list) newsupports  &optional (pds pds*current-proof-plan))
	   (remove-duplicates
	    (apply #'append
		   (mapcar #'(lambda (x)
			       (pds~add-sponsors x newsupports pds))
			   open-nodes)))))

(defgeneric pds~add-sponsors-if-hyp (open-node newsupports &optional (pds pds*current-proof-plan))
  (declare (edited  "07-MAR-2000")
	   (authors Sorge Ameier)
           (input   "An open node, a list of new support nodes, and optionally a pds.")
           (effect  "Adds new support nodes to the support nodes of the open node"
		    "if it is contained in the hypotheses of the open node, only.")
           (value  "The new list of support nodes."))
  (:method ((open-node pdsn+node) newsupports  &optional (pds pds*current-proof-plan))
	   ;;(format t "PDS~~ADD-SPONSORS-IF-HYP: open: ~A  new-supps: ~A~%" open-node newsupports)
	   (let* ((newsupports (if (every #'consp newsupports)
				   (apply #'append newsupports)
				 newsupports))
		  (pds-supports (pds~support-nodes pds))
		  (hyps (pdsn~hyps open-node))
		  (other-supports (remove-if #'(lambda (n) (or (find n pds-supports)
							       (not (find n hyps))))
					     newsupports)))
	     (setf (pdsn~just-sponsors open-node)
		   (union (pdsn~just-sponsors open-node) other-supports))
	     (setf (pdsn~just-unsponsors open-node)
		   (set-difference (pdsn~just-unsponsors open-node) other-supports))))
  (:method ((open-nodes list) newsupports  &optional (pds pds*current-proof-plan))
	   (remove-duplicates
	    (apply #'append
		   (mapcar #'(lambda (x)
			       (pds~add-sponsors-if-hyp x newsupports pds))
			   open-nodes)))))

;;;Remark: The supports of a node N correponds to:
;;  (pds-supports U sponsors(N)) \ unsponsors(N)
;;We can therefore realize pds~delete-sponsors by inserting the given OLDSUPPORTS
;;in the unsponsors(N) without special consideration of supports from the
;;sponsors(N). 
(defgeneric pds~delete-sponsors (open-node oldsupports &optional pds)
  (declare (edited  "15-OCT-1999" "19-SEP-1996" "28-JUL-92 10:00")
           (authors Sorge Lassaad NESMITH)
           (input   "An open node and a list of support nodes to be removed.")
           (effect  "Removes given support nodes from the support nodes of the open node.")
           (value   "The new list of supports."))
  (:method ((open-node pdsn+node) oldsupports  &optional (pds pds*current-proof-plan))
  ;;;LC: The optional argument PDS is added in order to call this function with same arguments
  ;; as the function pds~add-sponsors. Both functions are called to carry out method and rule
  ;; actions.
	   (declare (ignore pds))
	   (setf (pdsn~just-unsponsors open-node)
		 (union (pdsn~just-unsponsors open-node)
			(if (every #'consp oldsupports)
			    (apply #'append oldsupports)
			  oldsupports))))
  (:method ((open-nodes list) oldsupports  &optional (pds pds*current-proof-plan))
	   (remove-duplicates
	    (apply #'append
		   (mapcar #'(lambda (x)
			       (pds~delete-sponsors x oldsupports pds))
			   open-nodes)))))

(defun pds~node-supports (node &optional (pds pds*current-proof-plan))
  (declare (edited  "11-JUN-1997")
	   (authors Sorge Lassaad)
	   (input   "A node, and optionally a pds.")
	   (effect  "None.")
	   (value   "The support nodes of NODE."))
  (let ((just (node~justification node)))
    (set-difference (union (pds~support-nodes pds) (pdsj~sponsors just))
		    (pdsj~unsponsors just))
    ))


(defsetf pds~node-supports (node) (nodes)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge Lassaad)
	   (input   "A node and a node list.")
	   (effect  "Sets the supports of NODE to NODES.")
	   (value   "Unspecified."))
  `(let* ((pds-supports (pds~support-nodes pds*current-proof-plan))
	  (unsponsors (set-difference pds-supports ,nodes))
	  (sponsors (set-difference ,nodes pds-supports))
	  (node-just (node~justification ,node)))
     (setf (pdsj~sponsors node-just) sponsors)
     (setf (pdsj~unsponsors node-just) unsponsors)
     ))

(defgeneric pds~node-formula (node &optional pds)
  (declare (edited  "14-APR-1999")
	   (authors Lassaad)
	   (input   "A pds NODE, and optionally a PDS.")
	   (effect  "When NODE is schematic and its current formula is not up-to-date,"
		    "then updates the current formula of NODE using the current bindings"
		    "of the meta-variables in PDS.")
	   (value   "The up-to-date formula of node."))
  (:method ((schematic-node pdsn+schematic-node) &optional (pds pds*current-proof-plan))
	   (unless (pdsn~up-to-date schematic-node)
	     (let ((subst (when (pds~constraint-pool pds) (pds~cstrpool-bindings (pds~constraint-pool pds)))))
	       (setf (pdsn~current-formula schematic-node)
		     (if subst
			 (if (remove-if-not #'data~abstr-p (subst~codomain subst))
			     ;; there are abstractions in the codomain of the mvar-subst
			     ;; -> we have to beta-normalize
			     (beta~normalize (subst~apply subst (node~formula schematic-node)))
			   ;; otherwise we can spare to beta-normalize (whcih is a verry complex operation)
			   (subst~apply subst (node~formula schematic-node)))
		       (node~formula schematic-node)))
	       (setf (pdsn~up-to-date schematic-node) T)))
	   (pdsn~current-formula schematic-node))
  (:method ((pdsn pdsn+node) &optional pds)
	   (declare (ignore pds))
	   (node~formula pdsn))
  )

(defgeneric pds~task-formula (task &optional pds)
  (declare (edited  "16-APR-1998")
	   (authors Lassaad)
	   (input   "An agenda task, and a pds.")
	   (effect  "None.")
	   (value   "The currennt formula of the associated node."))
  (:method ((goal agenda+goal) &optional pds)
	   (declare (ignore pds))
	   (node~formula (agenda~task-node goal)))
  (:method ((sgoal agenda+goal-schema) &optional (pds pds*current-proof-plan))
	   (cond ((agenda~goal-schematic-p sgoal)
		  (pds~node-formula (agenda~task-node sgoal) pds))
		 (T
		  (pdsn~current-formula (agenda~task-node sgoal)))))
  (:method ((pgoal agenda+pseudo-goal) &optional (pds pds*current-proof-plan))
	   (pds~node-formula (agenda~task-node pgoal) pds))
  (:method (object &optional pds)
	   (declare (ignore pds))
	   (error ";;;pds~~task-formula: Dont know how determine the formula of ~A instance of ~A!"
		  object (type-of object)))
  )

;; to be called when backtracking
(defun pds~reset-schematic-nodes! (&optional (pds pds*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "14-APR-1999")
	   (authors Lassaad)
	   (input   "A pds, and an agenda.")
	   (effect  "Sets the up-to-date flag in the schematic nodes of PDS to NIL, and updates AGENDA"
		    "by removing the tasks which no more refer to existing PDS nodes and setting the slot"
		    "schematic-p of the remaining goal-schemas in AGENDA to T.")
	   (value   "The updated agenda."))
  (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
	(remove-if-not #'pdsn~schematic-p (prob~proof-steps pds)))
  (agenda~update agenda (prob~proof-steps pds))
  )

;; to be called after extending the PDS constraint state with meta-variable bindings:
#|(defun pds~update-schematic-nodes! (&optional (pds pds*current-proof-plan) solved-task new-opens)
  (declare (edited  "14-APR-1999")
	   (authors Lassaad)
	   (input   "A pds, and optionally a solved task, and a list of new open nodes.")
	   (effect  "Sets the up-to-date flag in the schematic support nodes to NIL, and"
		    "adapts the slot schematic-p of the goal-schemas in the PDS agenda.")
	   (value   "A pair: - A list of tasks whose formulas did not contain meta-variables"
		    "          before the updating"
		    "        - A list of schematic tasks whose formulas become closed by the"
		    "          updating"))
  (let* ((all-tasks (agenda~all-tasks (pds~agenda pds)))
	 (tasks (remove solved-task all-tasks))
	 (task-supports (apply #'append (mapcar #'(lambda (task) (pds~node-supports (agenda~task-node task) pds))
						(if new-opens all-tasks tasks)
						;; When there is new subgoals, i.e., NEW-OPENS is not NIL, then we have to reset
						;; the schematic supports of all tasks, since the supports of SOLVED-TASK are
						;; inherited to NEW-OPENS. Otherwise, only the schematic supports of the remaining
						;; tasks are reset
						))))
    ;; Sets the up-to-date flag in the schematic support nodes to NIL
    (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
	  (remove-if-not #'pdsn~schematic-p task-supports))
    ;; Fetch the closed tasks before applying the new meta-variable bindings:
    (let* ((goal-schemas (remove-if-not #'agenda~goal-schema-p tasks))
	   (before-closed (append (remove-if-not #'agenda~goal-p (set-difference tasks goal-schemas))
				  (remove-if #'agenda~goal-schematic-p goal-schemas)))
	   (after-closed-candidates (remove-if-not #'agenda~goal-schematic-p goal-schemas))
	   (mvar-subst (pds~cstrpool-bindings (pds~constraint-pool pds)))
	   after-closed)
      ;; Sets the up-to-date flag in the schematic task nodes to NIL
      (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
	    (mapcar #'agenda~task-node after-closed-candidates))
      
      (dolist (goal-schema after-closed-candidates)
	;; - Fetching some goal-schema SG: when goal-schematic-p(SG) is set to T, then consider the node(SG):
	;; When the up-to-date of this node is NIL, then compute the current-formula; set up-to-date to T, and
	;; set goal-schematic-p(SG) to NIL, when the resulted current-formula does not contain meta-variables
	(let ((gs-node (agenda~task-node goal-schema)))
	  (unless (pdsn~up-to-date gs-node)
	    (setf (pdsn~current-formula gs-node) (beta~normalize (subst~apply mvar-subst (node~formula gs-node)))
		  (pdsn~up-to-date gs-node) T)
	    (unless (remove-if-not #'meta~p (data~free-variables (pdsn~current-formula gs-node)))
	      (setf (agenda~goal-schematic-p goal-schema) NIL)
	      (push goal-schema after-closed)))))
      (values before-closed after-closed))
    ))|#
;; New Version by AMEIER
(defun pds~update-schematic-nodes! (&optional (pds pds*current-proof-plan) solved-task new-opens)
  (declare (edited  "14-APR-1999")
	   (authors Lassaad)
	   (input   "A pds, and optionally a solved task, and a list of new open nodes.")
	   (effect  "Sets the up-to-date flag in the schematic support nodes to NIL, and"
		    "adapts the slot schematic-p of the goal-schemas in the PDS agenda.")
	   (value   "A pair: - A list of tasks whose formulas did not contain meta-variables"
		    "          before the updating"
		    "        - A list of schematic tasks whose formulas become closed by the"
		    "          updating"))
  (let* ((all-tasks (agenda~all-tasks (pds~agenda pds)))
	 (all-node-tasks (remove-if #'agenda~inst-task-p all-tasks))
	 (tasks (remove-if #'agenda~inst-task-p 
			   (remove solved-task all-tasks)))
	 (task-supports (apply #'append (mapcar #'(lambda (task) (pds~node-supports (agenda~task-node task) pds))
						(if new-opens all-node-tasks tasks)
						;; When there is new subgoals, i.e., NEW-OPENS is not NIL, then we have to reset
						;; the schematic supports of all tasks, since the supports of SOLVED-TASK are
						;; inherited to NEW-OPENS. Otherwise, only the schematic supports of the remaining
						;; tasks are reset
						))))
    ;; Sets the up-to-date flag in the schematic support nodes to NIL
    (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
	  (remove-if-not #'pdsn~schematic-p task-supports))
    ;; Fetch the closed tasks before applying the new meta-variable bindings:
    (let* ((goal-schemas (remove-if-not #'agenda~goal-schema-p tasks))
	   (before-closed (append (remove-if-not #'agenda~goal-p (set-difference tasks goal-schemas))
				  (remove-if #'agenda~goal-schematic-p goal-schemas)))
	   (after-closed-candidates (remove-if-not #'agenda~goal-schematic-p goal-schemas))
	   (mvar-subst (pds~cstrpool-bindings (pds~constraint-pool pds)))
	   after-closed)
      ;; Sets the up-to-date flag in the schematic task nodes to NIL
      (mapc #'(lambda (n) (setf (pdsn~up-to-date n) NIL))
	    (mapcar #'agenda~task-node after-closed-candidates))
      
      (dolist (goal-schema after-closed-candidates)
	;; - Fetching some goal-schema SG: when goal-schematic-p(SG) is set to T, then consider the node(SG):
	;; When the up-to-date of this node is NIL, then compute the current-formula; set up-to-date to T, and
	;; set goal-schematic-p(SG) to NIL, when the resulted current-formula does not contain meta-variables
	(let ((gs-node (agenda~task-node goal-schema)))
	  (unless (pdsn~up-to-date gs-node)
	    (setf (pdsn~current-formula gs-node)
		  (if (remove-if-not #'data~abstr-p (subst~codomain mvar-subst))
		      ;; there are abstractions in the codomain of the mvar-subst
		      ;; -> we have to beta-normalize
		      (beta~normalize (subst~apply mvar-subst (node~formula gs-node)))
		    ;; otherwise we can spare to beta-normalize (whcih is a verry complex operation)
		    (subst~apply mvar-subst (node~formula gs-node))))
	    (setf (pdsn~up-to-date gs-node) T)
	    (unless (remove-if-not #'meta~p (data~free-variables (pdsn~current-formula gs-node)))
	      (setf (agenda~goal-schematic-p goal-schema) NIL)
	      (push goal-schema after-closed)))))
      (values before-closed after-closed))
    ))



(defun pds~first-tasks! (pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "20-APR-1999")
	   (authors Lassaad)
	   (input   "A pds.")
	   (effect  "Possibly updates some goal schemas on the PDS agenda.")
	   (value   "A pair: - First tasks on PDS agenda which were not blocked"
		    "        - First tasks on PDS agenda which were blocked and"
		    "        are unblocked by this function."))
  (multiple-value-bind (unblocked-tasks were-blocked-tasks)
      (agenda~first-tasks! agenda)
    (let ((mvar-subst (when (pds~constraint-pool pds) (pds~cstrpool-bindings (pds~constraint-pool pds)))))
      (when mvar-subst
	(dolist (task (remove-if-not #'(lambda (x) (and (agenda~goal-schema-p x) (agenda~goal-schematic-p x)))
				     (append unblocked-tasks were-blocked-tasks)))
	  (let ((task-node (agenda~task-node task)))
	    (unless (pdsn~up-to-date task-node)
	      (setf (pdsn~current-formula task-node) (subst~apply mvar-subst (node~formula task-node))
		    (pdsn~up-to-date task-node) T)
	      (unless (remove-if-not #'meta~p (data~free-variables (pdsn~current-formula task-node)))
		(setf (agenda~goal-schematic-p task) NIL)))))))
    (values unblocked-tasks were-blocked-tasks)
    ))

(defun pds~agenda-goals-with-supports-in (agenda supports &optional (pds pds*current-proof-plan) except)
  (declare (edited  "27-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of support nodes, an agenda, and optionally a pds, and"
		    "a list of agenda tasks.")
	   (effect  "None.")
	   (value   "A list of pairs where each pair consists of a agenda"
		    "task, when this is a goal or a schematic goal without"
		    "meta-variables in its current formula, and the associated"
		    "supports of this task are not disjoint with SUPPORTS,"
		    "and the intersection of the task supports and SUPPORTS."))
  (let ((tasks (agenda~get-tasks agenda
				 #'(lambda (task)
				     (unless (find task except)
				       (or (agenda~goal-p task)
					   (and (agenda~goal-schema-p task)
						(not (agenda~goal-schematic-p task))))))))
	(result))
    (dolist (task tasks)
      (let* ((task-node (agenda~task-node task))
	     (node-supps (pds~node-supports task-node pds))
	     (intersect (intersection supports node-supps)))
	(when intersect
	  (setq result
		(cons (list task intersect) result)))))
    result))

(defun pds~node-supported-by-p (node &rest supp-nodes)
  (declare (edited  "17-JUN-1997")
	   (authors Lassaad)
	   (input   "a node, and node list.")
	   (effect  "None.")
	   (value   "T, iff the element of SUPP-NODES are all supports of NODE."))
  (subsetp supp-nodes (pds~node-supports node)))

(defun pds~find-support (pred &optional (pds pds*current-proof-plan))
  (declare (edited  "18-JUL-1997")
	   (authors Lassaad)
	   (input   "A lisp function to be used as a unary predicate, and a pds.")
	   (effect  "None.")
	   (value   "A PDS node whose formula satisfies the predicate PRED."))
  (let ((supp-nodes (pds~support-nodes pds)))
    (or (find-if #'(lambda (node) (eval (list pred (node~formula node))))
		 supp-nodes)
	(find-if #'(lambda (node) (eval (list pred (node~formula node))))
		 (set-difference (prob~proof-steps pds) supp-nodes)))
    ))

(defun pds~find-open-node (pred &optional (pds pds*current-proof-plan))
  (declare (edited  "17-JUL-1997")
	   (authors Lassaad)
	   (input   "A lisp function to be used as a unary predicate, and a pds.")
	   (effect  "None.")
	   (value   "A PDS open node whose formula satisfies the predicate PRED."))
  (find-if #'(lambda (node) (eval (list pred (node~formula node))))
	   (pds~open-nodes pds)))

(defun pds~find-node-support (node pred &optional (pds pds*current-proof-plan))
  (declare (edited  "17-JUL-1997")
	   (authors Lassaad)
	   (input   "A node, a lisp function to be used as a unary predicate, and a pds.")
	   (effect  "None.")
	   (value   "A NODE support node whose formula satisfies the predicate PRED."))
  (when (and node (pdsn~open-node-p node))
    (find-if #'(lambda (x) (eval (list pred (node~formula x))))
	     (pds~node-supports node pds))))

(defun pds~find-in-subproof (node pred)
  (declare (edited  "07-DEC-2000")
	   (authors Sorge)
	   (input   "A node and a lisp function to be used as a unary predicate.")
	   (effect  "None.")
	   (value   "A node from the subproof justifying the given NODE, whose formula"
		    "satisfies the predicate PRED."))
  (when node
    (find-if #'(lambda (x) (eval (list pred (node~formula x))))
	     (reverse (cdr (pds~linearize-plan node))))))

(defun pds~add-thy-assertion (assumption &optional (pds pds*current-proof-plan))
  (declare (edited  "16-MAR-1999" "18-FEB-1997")
	   (authors Pollet Lassaad)
	   (input   "An assumption, and a proof plan. The given assumption must"
		    "be an assertion in the theory of the proof plan.")
	   (effect  "Inserts ASSUMPTION as a new hypothesis in PDS.")
	   (value   "The inserted assumption node."))
 (catch 'poly!
  (let* ((assertion (cond ((prob~p assumption)
			   (pds=make-new-node-from-conc-and-ass assumption))
			  ((th~definition-p assumption)
			   (return-from pds~add-thy-assertion
			     (warn ";;; Cannot insert a definition into a PDS!")))
			  (t (th~ass-node assumption))))
	 (not-insert (pds~label2node (keim~name assumption) pds))
	 (non-hyp-nodes (remove-if #'pdsn~hypothesis-node-p (prob~proof-steps pds)))
	 (new-hyp (if not-insert not-insert
		    (pds=node2pdsn assertion)))
	 (conc (prob~proof-root pds)))
    (setf (pds~support-nodes pds)
	  (union (list new-hyp) (pds~support-nodes pds)))
    (dolist (line non-hyp-nodes new-hyp)
      (setf (pdsn~hyps line) (union (list new-hyp) (pdsn~hyps line))))
    (unless not-insert
      (pds~only-insert-node-between! new-hyp
				     (pdsn~hyps conc)
				     (list conc)
				     pds))
    new-hyp)))

(defun pds=make-new-node-from-conc-and-ass (prob)
  (declare (edited  "16-MAR-1999")
	   (authors Pollet)
	   (input   "A problem.")
	   (effect  "nil")
	   (value   "A problem with assumptions A1...An, local constants c1...cn,"
		    "and conclusion THM is converted to a node, with node-formula"
		    "forall c1...cn . (A1 /\.../\ An ) => THM"))
  (let* ((env (prob~environment prob))
	 (and (logic~conjunction-constant))
	 (imp (logic~implication-constant))
	 (all (logic~universal-quantor :name :forall))
	 (ass-formulas (mapcar #'node~formula (prob~assumptions prob)))
	 (con-formula (node~formula (prob~conclusion prob)))
	 (constants
	  (mapcan #'(lambda (const)
		      (when (some #'(lambda (form) (data~substruct-p form const))
				  (cons con-formula ass-formulas)) (list const))) 
		  (mapcar #'(lambda (const) (env~lookup-object const env))
			  (env~class-keys env 'term+constant nil))))
	 (vars (mapcar #'(lambda (const) (term~variable-create (keim~name const) (term~type const))) constants))
;;; build the implication	 
         (formula  (if ass-formulas
		       (if (some #'data~schema-p (cons con-formula ass-formulas))
			   (throw 'poly! (error "Polymorphic assertion cannot be transformed to a theorem-node."))
			 (do* ((ass ass-formulas (rest ass))
			       (term (car ass)
				     (term~appl-create and (list (car ass) term))))
			     ((null (Rest ass)) (term~appl-create imp (list term con-formula)))))
		     con-formula))
;;; build the quantifiers
	 (formula (if vars
                      (progn (data~replace-structs formula constants vars :destructive t)
                             (do* ((var vars (rest var))
                                   (term (term~appl-create all
                                                           (list (term~abstr-create (list (car var))
                                                                                    formula)))
                                         (term~appl-create all
                                                           (list (term~abstr-create (list (car var))
                                                                                    term)))))
                                   ((null (rest var)) term)))
                    formula))
	 )
	(node~create (keim~name prob)
		     formula
		     (node~justification (car (th~ass-node prob))))
	))

(defun pds~add-hypothesis (formula &optional (pds pds*current-proof-plan) hyp-name)
  (declare (edited  "23-SEP-1996")
	   (authors Lassaad NESMITH)
	   (input   "A formula, a pds, and a name for the new node.")
	   (effect  "Creates a new hypothesis with the formula and adds it as a
general assumption to PDS.")
	   (value   "The new hypothesis. If HYP-NAME was not specified, a new name is generated."))
  (let ((non-hyp-lines (remove-if #'pdsn~hypothesis-node-p (prob~proof-steps pds)))
	(new-hyp
	 (pdsn~make-hypothesis formula
			       (or hyp-name (pds~new-node-name pds))))
	(assumps (prob~proof-assumptions pds))
	(conc (prob~proof-root pds))
	(pds-prob (prob~proof-problem pds)))
    (pds~only-insert-node-between! new-hyp
				   (pdsn~hyps conc)
				   (list conc)
				   pds)
    (setf (prob~assumptions pds-prob) (cons (pds=pdsn2node new-hyp) assumps))
    (setf (pds~support-nodes pds)
	  (cons new-hyp (pds~support-nodes pds)))
    (dolist (line non-hyp-lines new-hyp)
      (setf (pdsn~hyps line) (cons new-hyp (pdsn~hyps line))))
    ))

(defgeneric pds~start-proof-plan (problem name)              ;;; a very ugly function!!!!!
  (declare (edited  "11-JUN-1997" "23-SEP-1996" "28-JUL-92 10:00")
           (authors Sorge Lassaad NESMITH)
           (input   "A problem and a name.")
           (effect "Start a new proof plan with name NAME for PROBLEM. POBLEM should be of type"
		   "PROB+PROBLEM; NAME is a symbol. Sets pds*current-proof-plan to new proof plan"
		   "and returns it.")
           (value   "The new proof plan."))
  (:method ((problem prob+problem) name)
	   (let* ((new-proof (pds~proof-plan-create name))
		  (conc (prob~conclusion problem))
		  (assumptions (mapcar #'pds=node2pdsn
				       (prob~assumptions problem)))
		  (root (pdsn~create (keim~name conc)
				     assumptions
				     (node~formula conc)
				     (pdsj~open-just-create)))
		  (type-var-subst (env~lookup-object 'type-var-subst (prob~environment problem))))
	     (setf (pds~agenda new-proof) (agenda~create (agenda~create-goal root) nil nil
							 (pds~agenda new-proof)))
	     (setf pds*current-proof-plan new-proof)
	     (setf (prob~proof-problem new-proof) problem)
	     (setf (prob~proof-theory new-proof) (prob~theory problem))
	     (setf (prob~proof-root new-proof) root)
	     (setf (pds~open-nodes new-proof) (list root))
	     (setf (pds~environment new-proof) (env~create (prob~environment problem) name))
	     (env~enter 'type-var-subst
			(if type-var-subst
			    (keim~copy type-var-subst :downto '(data+struct))
			  (subst~create nil nil))
			(pds~environment new-proof))
	     (setf data*global-type-var-subst (env~lookup-object 'type-var-subst (pds~environment new-proof)))
	     (prob~add-proof problem new-proof)
	     (setf (prob~proof-steps new-proof) (append assumptions (list root)))
	     (setf (pds~support-nodes new-proof) assumptions)
	     (setf (pds~node-supports root) assumptions)
	     (dolist (x (cons root assumptions))
	       (setf (gethash (symbol-name (keim~name x)) (pds~label-node-hashtable new-proof)) x))
	     new-proof)))
      

(defun pds=node2pdsn (node)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge)
	   (input   "A node of type NODE+NODE.")
	   (effect  "An object of type PDSN+NODE is created and the information of NODE is copied.")
	   (value   "The new pds-node."))
  (let ((new-node (pdsn~create (etypecase (keim~name node)
				 (symbol (keim~name node))
				 (string (make-symbol (keim~name node))))
			       nil
			       (node~formula node)
			       (pdsj~closed-just-create (node~just-method node)
							nil
							nil
							"grounded"))))
    (setf (pdsn~hyps new-node) (list new-node))
    new-node))
	       
(defun pds=pdsn2node (pdsn)
  (declare (edited  "11-JUN-1997")
	   (authors Sorge)
	   (input   "A node of type PDSN+NODE.")
	   (effect  "An object of type NODE+NODE is created and the information of PDSN is copied.")
	   (value   "The new node."))
  (node~create (keim~name pdsn)
	       (node~formula pdsn)
	       (just~create (node~just-method pdsn) nil)))
	       
(defun pds~assumptions (&optional (proof-plan pds*current-proof-plan))
  (declare (edited  "12-JUN-1997")
	   (authors Sorge)
	   (input   "A proof-plan.")
	   (effect  "None.")
	   (value   "A list of assumptions of the proof-plan. This list differs from the"
		    "problem-assumptions as it contains the nodes in the pds-steps, that"
		    "is nodes of type PDSN+NODE."))
  (mapcar #'(lambda (x) (gethash (symbol-name x)
				 (pds~label-node-hashtable proof-plan)))
	  (mapcar #'keim~name (prob~proof-assumptions proof-plan))))

(defun pds~focus (line &optional (proof-plan pds*current-proof-plan))
  (declare (edited  "23-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A node, and an optional 
proof plan (defaults to PDS*CURRENT-PROOF-PLAN)")
           (effect "Make this node (if an open node) the current focus
of attention (i.e., the first open node).")
           (value   "T if carried out, NIL if not."))
  (when (and (pds~proof-plan-p proof-plan)
	     (pdsn~p line)
	     (pdsn~open-node-p line))
    (setf (pds~open-nodes proof-plan)
	  (cons line (delete line (pds~open-nodes proof-plan))))
    t))

	     
(defun pds~proof-plan-status (&optional (proof-plan pds*current-proof-plan))
  (declare (edited  "23-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "An optional proof plan (defaults to PDS*CURRENT-PROOF-PLAN).")
           (effect "For each open node, prints the node name and names
of its supporting nodes.")
           (value   "Undefined"))
  (let ((plines (pds~open-nodes proof-plan)))
    (if plines
	(progn
	  (format t "~%Open nodes    Supports")
	  (dolist (pline plines t)
	    (format t "~%~A~17T~{~A ~}~%" (keim~name pline)
		    (mapcar #'keim~name (pds~node-supports pline)))))
      (format t "~%No open nodes in proof plan ~A.~%" 
	      (keim~name proof-plan)))))



#{\subsection{Functions used in rule definitions}

We will want, in defining some rules, to describe some properties or
operations on ND proofs.  Here are a few of these operations.
#}

(defun pds~not-free-in-nodes-or-hyps-p (term &rest lines)
  (declare (edited  "23-SEP-1996" "09-FEB-1993 12:46")
	   (authors Lassaad nesmith)
	   (input   "A TERM and  as &rest argument a list of nodes.")
	   (effect  "None.")
	   (value   "T if TERM is not free in (occurs un-lambda-bound in)
LINES' formulas nor free in any of their hypotheses, otherwise nil."))
  (let ((hyps (delete-duplicates
	       (mapcan #'copy-list (mapcar #'pdsn~hyps lines)))))
    (dolist (line 
	     (remove-duplicates (append lines hyps)) 
	     t)
      (let ((unbound-symbols
	     (data~all-unbound-symbols (node~formula line))))
	(if (find term unbound-symbols :test #'data~equal-p)
	    (return-from pds~not-free-in-nodes-or-hyps-p nil))))))

(defun pds~constants-not-free-p (symlist &rest nodes)
  (declare (edited  "02-JUL-1997")
	   (authors Lassaad)
	   (input   "A list of symbol terms and as &rest argument a list of nodes.")
	   (effect  "None.")
	   (value   "T if the elements in SYMLIST are disjoint and each of them is"
		    "not free in (occurs un-lambda-bound in) NODES' formulas nor free"
		    "in any of their hypotheses, otherwise nil."))
  (when (equal symlist (remove-duplicates symlist))
    (let ((hyps (remove-duplicates
		 (apply #'append (mapcar #'pdsn~hyps nodes)))))
      (dolist (line (remove-duplicates (append nodes hyps)) t)
	(let ((unbound-symbols (data~all-unbound-symbols (node~formula line))))
	  (when (some #'(lambda (sym) (some #'(lambda (unbs) (data~equal-p sym unbs))
					    unbound-symbols))
		      symlist)
	    (return-from pds~constants-not-free-p nil)))))))

(defun pds~not-free-in-terms-p (term &rest others)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A TERM and as &rest argument a list of OTHERS terms.")
	   (effect  "None.")
	   (value   "T if TERM is not free in the given terms (occurs un-lambda-bound in)"
		    "in the OTHERS, otherwise nil."))
  (dolist (other others t)
    (when (member term (data~all-unbound-symbols other))
      (return-from pds~not-free-in-terms-p nil))))
 

(defun pds~change-top-var-quantor (var formula)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A variable VAR and a FORMULA, which must be a quantified
formula whose first argument is an abstraction.  VAR should not appear free
in FORMULA.")
	   (effect  "None.")
	   (value   "A new quantified formula just like FORMULA, but where
the bound variable is changed to VAR."))
  (let ((quantor (data~appl-function formula))
	(arg (car (data~appl-arguments formula))))
    (if (term~abstr-p arg)
	(logic~quantification-create 
	 quantor var 
	 (beta~normalize (term~appl-create arg (list var))))
      (error "Can't apply pds~~change-top-var-quantor because the first 
argument is not a lambda abstraction."))))

(defun pds~term-equal-application-of-to-p (term1 abstr term2)
  (declare (edited  "19-JUN-1997")
	   (authors Lassaad)
	   (input   "three terms: the second term must be a lambda abstraction.")
	   (effect  "None.")
	   (value   "T, iff TERM1 equals the application of ABSTR to TERM2."))
  (term~alpha-equal term1
		    (beta~contract (term~appl-create abstr (list term2)))))

(defun pds~term-equal-defn-expansion-p (term1 definiendum definiens term2 pos)
  (declare (edited  "03-JUL-1997")
	   (authors Fehrer)
	   (input   "four terms and a position")
	   (effect  "none")
	   (value   "T, iff TERM1 equals TERM2 with definiendum expanded by definiens."))
  (term~alpha-equal term2
		    (pds~replace-and-contract-at-position definiendum definiens
							  term1
							  pos)))
   
(defun pds~not-atom-p (formula)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A FORMULA.")
	   (effect  "None.")
	   (value   "T if formula is not an atom, otherwise NIL."))
  (not (logic~atom-p formula)))


(defun pds~pushneg (formula &optional (env (pds~environment pds*current-proof-plan)))			    
  (declare (edited  "17-FEB-1999" "09-FEB-1993 12:46")
	   (authors Pollet nesmith)
	   (input   "A FORMULA and an environment.")
	   (effect  "None.")
	   (value   "If formula is negated, tries to push the negation through"
		    "a quantifier or connective, returning a new formula."
		    "Otherwise returns the original formula. (War pds~pushneg!)"))
  (when (env~empty-p env)
    (warn "PDS~PUSHNEG is called with an empty environment! consider using the optional argument ENV")) 
  (unless (logic~negation-p formula)
    (return-from pds~pushneg formula))
  (let* ((the-not (data~appl-function formula))
	 (scope (car (data~appl-arguments formula))))
    (cond ((or (logic~existential-quantification-p scope)
	       (logic~universal-quantification-p scope))
	   (let ((quant (data~appl-function scope))
		 (quant-scope (car (data~appl-arguments scope)))
		 (sort (when (cdr (data~appl-arguments scope)) (cadr (data~appl-arguments scope)))))
	     (if (term~abstr-p quant-scope)
		 (term~appl-create
		  (logic~dual-quantifier quant)
		  (if sort
		      (list (term~abstr-create
			     (list (logic~quantification-bound-variable scope))
			     (term~appl-create the-not 
			      (list (logic~quantification-scope scope))))
			    sort)
		      (list (term~abstr-create
			 (list (logic~quantification-bound-variable scope))
			 (term~appl-create the-not 
			  (list (logic~quantification-scope scope)))))))
	       formula)))
	  ((logic~negation-p scope)
	   (car (data~appl-arguments scope)))
	  ((logic~conjunction-p scope)
	   (term~appl-create 
	    (env~lookup-object 'or env)
	    (list (term~appl-create the-not (list (car (data~appl-arguments scope))))
		  (term~appl-create the-not (list (cadr (data~appl-arguments scope)))))))
	  ((logic~disjunction-p scope)
	   (term~appl-create 
	    (env~lookup-object 'and env)
	    (list (term~appl-create the-not (list (car (data~appl-arguments scope))))
		  (term~appl-create the-not (list (cadr (data~appl-arguments scope)))))))
	  ((logic~implication-p scope)
	   (term~appl-create 
	    (env~lookup-object 'and env)
	    (list (car (data~appl-arguments scope))
		  (term~appl-create the-not (list (cadr (data~appl-arguments scope)))))))
	  ((logic~equivalence-p scope)
	   (let* ((implies (env~lookup-object 'implies env))
		  (or (env~lookup-object 'or env))
		  (not (env~lookup-object 'not env))
		  (arguments (data~appl-arguments scope)))
	     (term~appl-create or (list (term~appl-create not (list (term~appl-create implies arguments)))
				   (term~appl-create not (list (term~appl-create implies (reverse arguments))))))))
	  (t formula))))

(defun pds~pushneg-rec (formula &optional (env (pds~environment pds*current-proof-plan)) (formula-sign T))
  (declare (edited  "09-FEB-1999")
	   (authors Lassaad )
	   (input   "A FORMULA, an environment, and a boolean interpreted as an additional sign for FORMULA.")
	   (effect  "None.")
	   (value   "If formula is negated, tries to push the negation through
a quantifier or connective recursively up to atoms, returning a new formula. Otherwise returns the
original formula."))
  (when (env~empty-p env)
    (warn "pds~~pushneg is called with an empty environment! consider using the optional argument ENV"))
  (cond ((and formula-sign (not (logic~negation-p formula)))
	 ;;; +F -> F
	 (return-from pds~pushneg-rec formula))
	((and (null formula-sign) (logic~negation-p formula))
	 ;;; -notF -> rec(+F)
	 (pds~pushneg-rec (first (data~appl-arguments formula)) env T))
	((and formula-sign (logic~negation-p formula))
	 ;;; +notF -> consider F
	 (let ((scope (car (data~appl-arguments formula))))
	   (cond ((or (logic~existential-quantification-p scope)
		      (logic~universal-quantification-p scope))
		  (let* ((quantor (data~appl-function scope))
			 (quant-args (data~appl-arguments scope))
			 (quant-scope (first quant-args)))
		    (if (term~abstr-p quant-scope)
			(term~appl-create
			 (cond ((keim~equal quantor (data~schema-range (env~lookup-object 'forall env)))
				(env~lookup-object 'exists env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'exists env)))
				(env~lookup-object 'forall env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'forall-sort env)))
				(env~lookup-object 'exists-sort env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'exists-sort env)))
				(env~lookup-object 'forall-sort env)))
			 (append (list (term~abstr-create
					(list (logic~quantification-bound-variable scope))
					(pds~pushneg-rec (logic~quantification-scope scope) env NIL)))
				 (rest quant-args)))
		      formula)))
		 ((logic~negation-p scope)
		  (pds~pushneg-rec (car (data~appl-arguments scope)) env T))
		 ((logic~conjunction-p scope)
		  (term~appl-create 
		   (env~lookup-object 'or env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env NIL)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~disjunction-p scope)
		  (term~appl-create 
		   (env~lookup-object 'and env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env NIL)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~implication-p scope)
		  (term~appl-create 
		   (env~lookup-object 'and env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env T)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~equivalence-p scope)
		  (let ((implies (env~lookup-object 'implies env))
			(or (env~lookup-object 'or env))
			(arguments (data~appl-arguments scope)))
		    (term~appl-create or (list (pds~pushneg-rec (term~appl-create implies arguments) env NIL)
					       (pds~pushneg-rec (term~appl-create implies (reverse arguments)) env NIL)))))
		 (t formula))))
	((and (null formula-sign) (not (logic~negation-p formula)))
	 ;;; -F -> consider F
	 (let ((scope formula))
	   (cond ((or (logic~existential-quantification-p scope)
		      (logic~universal-quantification-p scope))
		  (let* ((quantor (data~appl-function scope))
			 (quant-args (data~appl-arguments scope))
			 (quant-scope (first quant-args)))
		    (if (term~abstr-p quant-scope)
			(term~appl-create
			 (cond ((keim~equal quantor (data~schema-range (env~lookup-object 'forall env)))
				(env~lookup-object 'exists env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'exists env)))
				(env~lookup-object 'forall env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'forall-sort env)))
				(env~lookup-object 'exists-sort env))
			       ((keim~equal quantor (data~schema-range (env~lookup-object 'exists-sort env)))
				(env~lookup-object 'forall-sort env)))
			 (append (list (term~abstr-create
					(list (logic~quantification-bound-variable scope))
					(pds~pushneg-rec (logic~quantification-scope scope) env NIL)))
				 (rest quant-args)))
		      (term~appl-create (env~lookup-object 'not env) (list formula)))))
		 ((logic~negation-p scope)
		  (pds~pushneg-rec (car (data~appl-arguments scope)) env T))
		 ((logic~conjunction-p scope)
		  (term~appl-create 
		   (env~lookup-object 'or env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env NIL)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~disjunction-p scope)
		  (term~appl-create 
		   (env~lookup-object 'and env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env NIL)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~implication-p scope)
		  (term~appl-create 
		   (env~lookup-object 'and env)
		   (list (pds~pushneg-rec (car (data~appl-arguments scope)) env T)
			 (pds~pushneg-rec (cadr (data~appl-arguments scope)) env NIL))))
		 ((logic~equivalence-p scope)
		  (let ((implies (env~lookup-object 'implies env))
			(or (env~lookup-object 'or env))
			(arguments (data~appl-arguments scope)))
		    (term~appl-create or (list (pds~pushneg-rec (term~appl-create implies arguments) env NIL)
					       (pds~pushneg-rec (term~appl-create implies (reverse arguments)) env NIL)))))
		 (t (term~appl-create (env~lookup-object 'not env) (list formula))))))
	))
	       
(defun pds~pullneg (formula &optional (env (pds~environment pds*current-proof-plan)))
  (declare (edited  "17-FEB-1999" "07-AUG-1997" "17-JUL-1997")
	   (authors Pollet Sorge Chris Lassaad)
	   (input   "A formula and an environment.")
	   (effect  "None.")
	   (value   "If FORMULA has a negated least nested subformula, then try to pull the negation"
		    "outside of the formula  (War frueher pds~pullneg!):"
		    "(and (not A) (not B)) to (not (or A B))"
		    "(and A (not B)) to (not (implies A B))"
		    "(and (not A) B) to (not (implies B A))"
		    "(or (not A) (not B)) (not (and A B))"
		    "(all x (not (phi x))) to (not (ex x (phi x)))"
		    "(ex x (not (phi x))) to (not (all x (phi x)))"
		    "(equiv A (not B)) to (not (equiv A B))"
		    "(equiv A (not B)) to (not (equiv A B)) and sorted qunators!"))
  (cond ((logic~conjunction-p formula)
	 (let ((conj1 (car (data~appl-arguments formula)))
	       (conj2 (cadr (data~appl-arguments formula))))
	   (if (logic~negation-p conj1)
	       (if (logic~negation-p conj2)
		   ;;; (and (not A) (not B))
		   (term~appl-create (env~lookup-object 'not env)
				(list (term~appl-create (env~lookup-object 'or env)
						   (append (data~appl-arguments conj1)
							   (data~appl-arguments conj2)))))
		 ;;; (and (not A) B)
		 (term~appl-create (env~lookup-object 'not env)
			      (list (term~appl-create (env~lookup-object 'implies env)
						 (cons conj2
						       (data~appl-arguments conj1))))))
	     (if (logic~negation-p conj2)
		 ;;; (and A (not B))
		 (term~appl-create (env~lookup-object 'not env)
			      (list (term~appl-create (env~lookup-object 'implies env)
						 (cons conj1
						       (data~appl-arguments conj2)))))
	       ;;; (and A B)
	       (error "PDS~~PULLNEG: A conjunction without any negated conjunct!")))))
	((logic~disjunction-p formula)
	 (let ((disj1 (car (data~appl-arguments formula)))
	       (disj2 (cadr (data~appl-arguments formula))))
	   (if (and (logic~negation-p disj1)
		    (logic~negation-p disj2))
	       ;;; (or (not A) (not B))
	       (term~appl-create (env~lookup-object 'not env)
			    (list (term~appl-create (env~lookup-object 'and env)
						   (append (data~appl-arguments disj1)
							   (data~appl-arguments disj2)))))
	     (error "PDS~~PULLNEG: A disjunction with unnegated disjunct!"))))
	((or (logic~existential-quantification-p formula)
	     (logic~universal-quantification-p formula))
	 (let ((quant (data~appl-function formula))
	       (quant-arg (car (data~appl-arguments formula)))
               (sort (when (cdr (data~appl-arguments formula)) (cadr (data~appl-arguments formula)))))
	   (if (term~abstr-p quant-arg)
	       (let ((abstr-scope (data~abstr-range quant-arg)))
		 (if (logic~negation-p abstr-scope)
		     (term~appl-create
		      (post~read-object 'not env :existing-term)
		      (list (term~appl-create
			     (logic~dual-quantifier quant)
			     (if sort
				 (list (term~abstr-create 
					(data~abstr-domain quant-arg)
					(car (data~appl-arguments abstr-scope)))
					sort)
			       (list (term~abstr-create 
				      (data~abstr-domain quant-arg)
				      (car (data~appl-arguments abstr-scope))))))))
		   (error "PDS~~PULLNEG: A quantified formula without negated scope!")))
	     (error "PDS~~PULLNEG: A quantifier applied to a non-abstraction!"))))
	((logic~equivalence-p formula)
	 (let ((equiv (data~appl-function formula))
	       (arg1 (car (data~appl-arguments formula)))
	       (arg2 (cadr (data~appl-arguments formula))))
	   (if (logic~negation-p arg1)
	       (term~appl-create (data~appl-function arg1)
			    (list (term~appl-create equiv
					       (append (data~appl-arguments arg1) (list arg2)))))
	     (term~appl-create (data~appl-function arg2)
			  (list (term~appl-create equiv
					     (cons arg1 (data~appl-arguments arg2))))))))
	(t
	 (error "PDS~~PULLNEG: An unconsidered type of formulae!"))
	))



(defun pds~replace-and-contract-at-position (term replacement in-term pos)
  (declare (edited  "19-AUG-1999" "01-MAR-1999" "14-APR-1998")
	   (authors Pollet Lassaad Fehrer)
	   (input   "a defined constant, its definition, a formula term and a position")
	   (effect  "none")
	   (value   "the formula with replaced definition occurrence"))
  (let ((orig (data~struct-at-position in-term pos)))
    (if (data~schema-equal orig term)
	(let* ((steppi (data~alpha-copy replacement nil))
	 ;; (term~alpha-copy replacement nil))
	 ;; (definiens1 (keim::term=copy-and-polytype-rename steppi nil))
	 ;;(definiens (if (data~schema-p definiens1)
	 ;;		(data~schema-range definiens1)
	 ;;	      definiens1))
	 ;; (definiens steppi)
	       (definiens (pds~ground-definiens steppi orig))
	       (orig-type (term~type orig))
	       (def-type (term~type definiens))
	       (subst (type~alpha-match def-type orig-type)))
	  (if subst
	      (let* ((inserted-term (subst~apply subst definiens))
		     (new-term (data~replace-at-position in-term pos inserted-term
							 :replacers-downto '(term+constant type+primitive) 
							 )))
		(beta~normalize new-term))
	    (error "PDS~~REPLACE-AND-CONTRACT-AT-POSITION: not matchable!")))
      (error "PDS~~REPLACE-AND-CONTRACT-AT-POSITION: cannot replace ~A by a definition for ~A." orig term))))


(defun pds~ground-definiens (definiens-pre term)
  (if (null (data~schema-p definiens-pre))
      definiens-pre
    (let* ((domain (data~schema-domain definiens-pre))
	   (range (data~schema-range definiens-pre))
	   (type-range (term~type range))
	   (type-term (term~type term))
	   (subst (type~alpha-equal type-range type-term domain)))
      (if subst
	  (let* ((grounded-check (every #'(lambda (var)
					    (let* ((substed (subst~apply subst var)))
					      (null (type~type-variables substed))))
					domain)))
	    (if grounded-check
		(subst~apply subst (data~alpha-copy range nil))
	      (error ";;; Not able to ground schematic input parameter ~A.~%" definiens-pre)))
	(error ";;; Definiens ~A does not match term ~A.~%" definiens-pre term)))))


(defun pds~replace-without-contract-at-position (term replacement in-term pos)
  (declare (edited  "19-AUG-1999" "01-MAR-1999" "14-APR-1998")
	   (authors Pollet Lassaad Fehrer)
	   (input   "a defined constant, its definition, a formula term and a position")
	   (effect  "none")
	   (value   "the formula with replaced definition occurrence"))
  (let* ((orig (data~struct-at-position in-term pos))
	 (definiens1   (data~copy replacement)) ;; Hier haben Aenderungen in
					       ;; term==copy-and-polytype-rename Probleme
					       ;; verursacht. Allerdings ist noch nicht
					       ;; klar ob data~copy die richtige
					       ;; Funktionalitaet hat (vorallem im defualt
					       ;; Aufruffall
	 ;(definiens1   (keim::term=copy-and-polytype-rename replacement nil))
	 ;(definiens (if (data~schema-p definiens1) (data~schema-range definiens1)
	 ;	      definiens1))
	 (definiens (pds~ground-definiens definiens1 orig))
	 (orig-type (term~type orig))
	 (def-type (term~type definiens))
	 (subst (type~alpha-match def-type orig-type)))
    (if subst
	(let* ((inserted-term (subst~apply subst definiens))
	       (new-term (data~replace-at-position in-term pos inserted-term
						   :replacers-downto '(term+constant) 
						   )))
	  new-term)
      (error "PDS~~REPLACE-WITHOUT-CONTRACT-AT-POSITION: not matchable!"))
    ))


(defmethod post~read-object ((obj list) (env env+environment) 
			     (key (eql :pds)))
  (if (consp (car obj))
      (eval (cons 'pds~def-proof-plan (cons nil obj)))
    (eval (cons 'pds~def-proof-plan obj))))

#{\subsection{Proof pretty-printing}
Here we define functions for pretty-printing of proof plans. This uses the
PP module to set up some new styles.
#}

(defvar pds*label-width 10 "default width for node labels in output")
(defvar pds*hyps-width 10 "default width for hypotheses in output")
(defvar pds*formula-width 40 "default width for formulas in output")
(defvar pds*just-width 15 "default width for justifications in output")
(defvar pds*print-entire-node t "True if we want to print an entire node, nil if we just want to print its label.")
(defvar pds*avoid-node-breaks nil "True if we want to avoid breaking nodes")
           

(defun pds~avoid-node-breaks (bool)
  (declare (edited  "24-SEP-1996")
	   (authors Lassaad nesmith)
	   (input   "T or NIL")
	   (effect  "The global value of pds*avoid-node-breaks is set to BOOL.
If true during pretty-printing of proof plans, all node breaks will be avoided,
so that a proof plan node will appear on a single output node.
Otherwise a prettier output will be attempted.")
	   (value   "BOOL"))
  (setq pds*avoid-node-breaks bool))


(defun pds=pprint-just (just)
  (let ((rule (keim~name (just~method just)))
	(terms (pdsj~parameters just))
	(lines (just~premises just))) 
    (pprint-logical-block
     (nil nil)
     (write rule :escape nil)
     (if (or lines terms) (write-char #\:))
     (unless pds*avoid-node-breaks
       (pprint-newline :miser))
     (pprint-logical-block
      (nil terms)
      (pprint-exit-if-list-exhausted)
      (write-char #\space)
      (write-char #\()
      (unwind-protect 
	  (loop (write (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\space)
		(unless pds*avoid-node-breaks
		  (pprint-newline :linear)))
	(write-char #\) )))
     (unless pds*avoid-node-breaks
       (pprint-newline :miser))
     (pprint-logical-block 
      (nil lines)
      (pprint-exit-if-list-exhausted)
      (write-char #\space)
      (write-char #\( )
      (let ((pds*print-entire-node nil))
	(unwind-protect 
	    (loop (write (pprint-pop) :gensym nil :escape nil)
		  (pprint-exit-if-list-exhausted)
		  (write-char #\space)
		  (unless pds*avoid-node-breaks
		    (pprint-newline :linear)))
	  (write-char #\) )))))
    ))


(defvar pds*print-ellipsis nil "True if an ellipsis should be printed before
an open node.")


(defun pds=pprint-node (line)
  (if pds*print-entire-node
      (flet ((str-sym-case (name)
			   (etypecase name
			     (symbol (symbol-name name))
			     (string name)))) 
	(when (and pds*print-ellipsis
		   (pdsn~open-node-p line))
	  (format t "               ...~%"))
	(if pds*avoid-node-breaks
	    (progn
	      (write (str-sym-case (keim~name line)))
	      (write-string " ")
	      (write (mapcar #'str-sym-case
			     (mapcar #'keim~name
				     (if (or (pdsn~th-assumption-p line)
					     (pdsn~local-definition-p line))
					 (pdsn~hyps line)
				       (remove-if ;;#'pdsn~th-assumption-p
					          #'(lambda (node)
						      (or (pdsn~th-assumption-p node)
							  (pdsn~local-definition-p node)))
						  (pdsn~hyps line))))))
	      (write-string " ! ")
	      (write (node~formula line))
	      (write-string " ")
	      (write (node~justification line))
	      (terpri))
	  (pp~pprint-table 
	   (str-sym-case (keim~name line))
	   (list pds*label-width :l "" " ")
	   (mapcar #'str-sym-case
		   (mapcar #'keim~name (if (or (pdsn~th-assumption-p line)
					       (pdsn~local-definition-p line))
					   (pdsn~hyps line)
					 (remove-if ;;#'pdsn~th-assumption-p
					            #'(lambda (node)
						    	(or (pdsn~th-assumption-p node)
						     	    (pdsn~local-definition-p node)))
						    (pdsn~hyps line)))))
	   (list pds*hyps-width :l nil nil  
		 #'(lambda (s o)
		     (let ((*print-escape* nil)
			   (*print-miser-width* nil))
		       (format s "~%~:/pprint-fill/" o)
		       )))
	   " ! " '(3 :c)
	   (node~formula line) 
	   (list pds*formula-width :l)
	   (node~justification line) 
	   (list pds*just-width :r " "))))
    (write (keim~name line))))


(defgeneric pds=pprint-type (type)
  (:method ((type type+primitive))
	   (write (keim~name type) :escape nil))
  (:method ((type type+func))
	   (write-char #\()
	   (write (data~abstr-n-range type))
	   (pprint-logical-block (nil (reverse (data~abstr-n-domain type)))
				 (loop (pprint-exit-if-list-exhausted)
				       (write-char #\space)
				       (write (pprint-pop))))
	   (write-char #\))
	   )
  (:method ((type type+appl))
	   (write-char #\()
	   (write (data~appl-function type))
	   (pprint-logical-block (nil (data~appl-arguments type))
				 (loop (pprint-exit-if-list-exhausted)
				       (write-char #\space)
				       (write (pprint-pop))))
	   (write-char #\))))


(defvar pds*printing-bound-var nil "True if we are printing a bound variable,
nil otherwise.")


(defgeneric pds=pprint-term (term)
  (:method ((term term+primitive))
	   (write (keim~name term) :escape nil)
	   (when pds*printing-bound-var
	     (write-char #\:)
	     (write (term~type term))))
  (:method ((term term+appl))
	   (unless pds*avoid-node-breaks
	     (pprint-indent :current 0))
	   (pprint-logical-block
	    (nil nil)
	    (if pds*avoid-node-breaks
		(write (cons (data~appl-function term)
			     (copy-list (data~appl-arguments 
					 term))))
	      (pprint-fill *standard-output*   
			   (cons (data~appl-function term)
				 (copy-list (data~appl-arguments 
					     term)))))))
  (:method ((term term+schema))
	   (write-string "KAP")
	   (write-char #\[)
	   (pprint-logical-block (nil (data~schema-domain term))
				 (write (pprint-pop))
				 (loop (pprint-exit-if-list-exhausted)
				       (write-string ", ")
				       (write (pprint-pop))))
	   (write-char #\])
	   (write-char #\.)
	   (unless pds*avoid-node-breaks
	     (pprint-newline :fill)
	     (pprint-indent :block 1))
	   (write (data~schema-range term)))
  (:method ((term term+abstr))
	   (pprint-logical-block (nil nil :prefix "(" :suffix ")")
				 (write-char #\[)
				 #+comment(pds=pprint-term (data~abstr-domain term))
				 (pprint-logical-block (nil (data~abstr-domain term))
						       (write (pprint-pop))
						       (loop (pprint-exit-if-list-exhausted)
							     (write-string ", ")
							     (write (pprint-pop))))
				 (write-char #\])
				 (write-char #\.)
				 (unless pds*avoid-node-breaks
				   (pprint-newline :fill)
				   (pprint-indent :block 1))
				 (write (data~abstr-range term)))))
    

(defvar pds*quantifier-names (list "FORALL" "EXISTS") "Names of quantifiers")


(defun pds=quantified-term-p (appl)
  (declare
   (authors nesmith)
   (input "An application.")
   (value "T if the application with one argument, which is an abstraction,
and the function's name is on the list pds*quantifier-names"))
  (when (term~appl-p appl)
    (and (term~primitive-p (data~appl-function appl))
	 (let ((args (data~appl-arguments appl)))
	   (and (= 1 (length args))
		(term~abstr-p (car args))))
	 (member (keim~name (data~appl-function appl))
		 pds*quantifier-names :test #'string-equal))))


(deftype pds+quantified-term ()
  '(and term+appl (satisfies pds=quantified-term-p)))


(defun pds=pprint-quantified-term (term)
  (let ((quantor (data~appl-function term))
	(args (data~appl-arguments term)))
    (let ((arg (car args)))
      (pprint-indent :current 2)
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
			    (write quantor)
			    (write-char #\space)
			    (write-char #\[)
			    (loop
			     (let ((var (first (data~abstr-domain arg)))
				   (scope (data~abstr-range arg)))
			       (let ((pds*printing-bound-var t))
				 ;(pds=pprint-term var)
				 (write var)
				 )
			       (if (and (pds=quantified-term-p scope)
					(data~equal quantor (data~appl-function scope)))
				   (progn
				     (write-char #\,)
				     (setq arg (car (data~appl-arguments scope))))
				 (progn
				   (write-char #\])
				   (write-char #\space)
				   (unless pds*avoid-node-breaks
				     (pprint-newline :fill))
				   ;(pds=pprint-term scope)
				   (write scope)
				   (return)))))))))


(defun pds=pprint-proof (proof)
  (let ((pds*print-entire-node t)
	;;(lines (remove-if #'pdsn~th-assumption-p (prob~proof-steps proof))))
	(lines (prob~proof-steps proof)))
    (multiple-value-bind (pds*label-width pds*hyps-width 
					  pds*formula-width pds*just-width)
	(pds~figure-margins lines)
      (pprint-logical-block (*standard-output* lines)
			    (pprint-exit-if-list-exhausted)
			    (loop (write (pprint-pop))
				  (pprint-exit-if-list-exhausted))))))

(defun pds~pprint-proof-plan (proof style)
  (declare
   (authors nesmith)
   (input "An pds+proof-plan PROOF, and a pp+style STYLE.")
   (effect "The proof will be printed in the style to *standard-output*."))
  (unless (pp~find-style style)
    (setq style 'pds-simple))
  (pp~pprint proof style)
  )


(defun pds~pprint-pds-node (node style)
  (declare (edited  "20-FEB-1997")
	   (authors Lassaad)
	   (input   "A node NODE, and a pp+style STYLE.")
	   (effect  "The node will be printed in the style to *standard-output*.")
	   (value   "Unspecified."))
  (unless (pp~find-style style)
    (setq style 'pds-simple))
  (pp~pprint node style)
  )


(defun pds~figure-margins (lines)
  (declare
   (authors sorge nesmith)
   (input "A list of proof plan nodes")
   (value "Tries to figure out the proper width of the various fields in printing."
	  "Four values: the width of the labels field, the width of the hypotheses field,"
	  "the width of the formula field and the width of the justification field."
	  "We always give the formulas at least 50% of the entire line."))
  (let ((total-width (- (or *print-right-margin* 79) 6))
	(max-label (apply #'max (mapcar #'(lambda (line) (length (symbol-name (keim~name line)))) lines)))
	(max-just (1+ (apply #'max 
			     (mapcar #'(lambda (line)
					 (length 
					  (format nil "~A: ~S" (pdsn~just-method line)
					     (mapcar #'keim~name (pdsn~just-premises line)))))
				     lines))))
	(max-hyps (apply #'max 
			 (mapcar #'(lambda (line)
				     (length 
				      (princ-to-string
				       (mapcar #'keim~name (remove-if #'(lambda (node)
									  (or (pdsn~th-assumption-p node)
									      (pdsn~local-definition-p node)))
								      (pdsn~hyps line))))))
				 lines))))
    (if (< (+ max-label max-just max-hyps) (floor (/ total-width 2)))
	(values max-label max-hyps (- total-width (+ max-label max-hyps
						     max-just ))
		max-just)
      (values max-label 
	      (max (- (floor (/ total-width 2)) 
		      (+ max-label max-just))
		   10)
	      (floor (/ total-width 2))
	      max-just))))


#{We set up three different styles for use with {\vb pp~pprint}.

The first is {\vb PDS-SIMPLE}, it merely prints the lines in a relatively
readable form (but better than {\vb post~print}).

The next is {\vb PDS-PRETTY}.  This style is like {\vb PDS-SIMPLE}, but
prints ellipses just before planned lines, and for quantified formulas
which are defined as those applications of the right type whose function
looks like a EXISTS or an FORALL, it prints no lambda and tries to
compress multiple quantifiers into a single one. 

The last style is {\vb PDS-POST}.  This corresponds to the style printed by
{\vb post~print}. 
#}

(pp~defstyle pds-simple
	     :help "A simple style for printing proof plans."
	     :pprint-methods
	     ((term+term (lambda (s l)
			   (let ((*standard-output* s))
			     (pds=pprint-term l)))
			 5)
              (just+justification
	       (lambda (s l)
		 (let ((*standard-output* s))
		   (pds=pprint-just l))))
	      (pds+proof-plan 
	       (lambda (s l)
		 (let ((*standard-output* s)) 
		   (pds=pprint-proof l))))
	      (pdsn+node
	       (lambda (s l)
		 (let ((*standard-output* s)) 
		   (pds=pprint-node l))))
	      (type+type
	       (lambda (s l)
		 (let ((*standard-output* s)) 
		   (pds=pprint-type l))))
	      ))


(defvar pds*default-output-style 'pds-simple "Current output style.")


(defun pds~show-node (line)
  (declare
   (authors nesmith)
   (input "A proof plan node")
   (effect "pprints the node to *standard-output* using 
pds*default-output-style"))
  (pp~pprint line pds*default-output-style))

(defun pds~node2string (line)
  (with-output-to-string (output)
			 (pp~pprint line pds*default-output-style output)))


(defun pds~show-proof-plan (proof)
  (declare
   (authors nesmith)
   (input "A proof plan")
   (effect "pprints the proof plan to *standard-output* using 
pds*default-output-style"))
  (pds~pprint-proof-plan proof pds*default-output-style))


(pp~defstyle pds-pretty :parent pds-simple
	     :help "A style that prints quantified formulas more intelligently."
	     :pprint-methods
	     ((pds+quantified-term
	       (lambda (s l)
		 (let ((*standard-output* s))
		   (pds=pprint-quantified-term l)))
	       6)
	      (pds+proof-plan 
	       (lambda (s l)
		 (let ((*standard-output* s)
		       (pds*print-ellipsis t)) 
		   (pds=pprint-proof l))))))


(pp~defstyle 
 pds-post :parent pp+top
 :help "A style which will print PDS stuff in POST format."
 :pprint-methods
 ((type+primitive 
   (lambda (stream type)
     (write-string (string (keim~name type)) stream)))
  (type+func
   (lambda (stream type)
     (write-char #\( stream)
     (write (data~abstr-n-range type))
     (pprint-logical-block (stream (reverse (data~abstr-n-domain type)))
			   (loop (pprint-exit-if-list-exhausted)
				 (write-char #\space)
				 (write (pprint-pop))))
     (write-char #\) stream)))
  (type+appl
   (lambda (stream type)
     (write-char #\( stream)
     (write (data~appl-function type))
     (pprint-logical-block (stream (data~appl-arguments type))
			   (loop (pprint-exit-if-list-exhausted)
				 (write-char #\space)
				 (write (pprint-pop))))
     (write-char #\) stream)))
  (term+term
   (lambda (s term) (post~print term s)))
  (subst+substitution
   (lambda (stream subst)
     (let ((*standard-output* stream))
       (if (subst~empty-p subst)
	   (write-string "()")
	 (progn
	   (write-string "(substitution ")
	   (pprint-logical-block
	    (nil (subst~domain subst) :prefix "(" :suffix ") ")
	    (write (pprint-pop))
	    (loop
	     (pprint-exit-if-list-exhausted)
	     (write-char #\space)
	     (write (pprint-pop))))
	   (pprint-newline :fill)
	   (pds=post-print-list (subst~codomain subst) stream)
	   (write-char #\))))
       )))
  (mapp+mapping
   (lambda (s mapp)
     (let ((*standard-output* s))
       (if (mapp~empty-p mapp)
	   (write-string "()")
	 (progn
	   (write-string "(mapping ")
	   (pprint-logical-block
	    (nil (mapp~domain mapp) :prefix "(" :suffix ") ")
	    (write (pprint-pop))
	    (loop
	     (pprint-exit-if-list-exhausted)
	     (write-char #\space)
	     (write (pprint-pop))))
	   (pprint-newline :fill)
	   (pds=post-print-list (mapp~codomain mapp) s)
	   (write-char #\))))
       )))
  (pds+constraint-pool
   (lambda (s cstr)
     (let ((*standard-output* s))
       (write-string "(constraint-pool ")
       (pds~post-print (pds~cstrpool-bindings cstr) s) 
       (pprint-logical-block
	(nil (list  (pds~cstrpool-constraint cstr)
		    (pds~cstrpool-otherwise cstr)
		    (pds~cstrpool-previous cstr)))
	(pprint-exit-if-list-exhausted)
	(loop
	 (write (pprint-pop))
	 (pprint-exit-if-list-exhausted)
	 (write-char #\space)
	 (pprint-newline :fill)))
       (pds=post-print-list (pds~cstrpool-plansteps cstr) s)
       (write-string ")"))))
  (agenda+empty-agenda
   (lambda (s agenda)
     (declare (ignore agenda))
     (format s "(agenda)")))
  (agenda+agenda
   (lambda (s agenda)
     (let ((*standard-output* s))
	 (pprint-logical-block
	  (nil (list  (agenda~first-task agenda)
		      (agenda~next-tasks agenda)
		      (agenda~orderings agenda)
		      (agenda~then-agenda agenda))
	       :prefix "(agenda " :suffix ")")
	  (pprint-exit-if-list-exhausted)
	  (loop
	   (let ((thing (pprint-pop)))
	     (if thing
		 (write thing)
	       (write-string "()")))
	   (pprint-exit-if-list-exhausted)
	   (write-char #\space)
	   (pprint-newline :fill))))))
  (agenda+inst-task
   (lambda (s task)
     (let ((*standard-output* s))
       (format s "(task-inst (~A "
	       (keim~name (agenda~inst-task-meta-var task)))
       (pds~post-print (agenda~inst-task-plan-step task) s)
       (format s "))"))))
  (agenda+ordering
   (lambda (s order)
     (let ((*standard-output* s))
	       (pprint-logical-block
	(nil (list  (agenda~ordering-first-tasks order)
		    (agenda~ordering-next-tasks order))
	:prefix "(" :suffix ")")
	(pprint-exit-if-list-exhausted)
	(loop
	 (write (pprint-pop))
	 (pprint-exit-if-list-exhausted)
	 (write-char #\space)
	 (pprint-newline :fill))))))
  (agenda+task
   (lambda (s task)
     (format s "(task ~A)" (keim~name (agenda~task-node task)))
     ))
  (pds+proof-plan 	   
   (lambda (stream proof)
     (let* ((*standard-output* stream)
	    (env (pds~environment proof))
	    (type-vars (env~class-keys env 'type+variable nil))
	    (type-constants (env~class-keys env 'type+constant nil))
	    (constants (env~class-keys env 'term+constant nil))
	    (meta-variables (env~class-keys env 'meta+variable nil))
	    (variables (set-difference (env~class-keys env 'term+variable nil)
				       meta-variables)))
       (pprint-logical-block 
	(nil nil :prefix "("
	     :suffix ")")
	(write-string "PDS ")
	(write-string "(problem ")
	(write-string (princ-to-string (keim~name (prob~proof-problem proof))))
	(write-string ")")
	(pprint-newline :mandatory)
	(write-string " (in ")
	(write-string (princ-to-string (keim~name (prob~proof-theory proof))))
	(write-string ")")
	(pprint-newline :mandatory)
	(pprint-indent :block 2)
	(pprint-logical-block 
	 (nil (list type-vars type-constants 
		    constants meta-variables variables) 
	      :prefix "("
	      :suffix ")")
	 (write-string "declarations ")
	 (pprint-indent :block 2)
	 (pprint-logical-block 
	  (nil (pprint-pop) :prefix "("
	       :suffix ")")
	  (pprint-indent :block 1)
	  (write-string "type-variables ")
	  (pprint-exit-if-list-exhausted)
	  (loop (write (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\space)
		(pprint-newline :linear)))
	 (pprint-newline :fill)
	 (pprint-logical-block 
	  (nil (pprint-pop) :prefix "("
	       :suffix ")")
	  (pprint-indent :block 1)
	  (write-string "type-constants ")
	  (pprint-exit-if-list-exhausted)
	  (loop (write (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\space)
		(pprint-newline :linear)))
	 (pprint-newline :fill)
	 (pprint-logical-block 
	  (nil (mapcar #'(lambda (sym) (post~read-object sym env :existing-term))
		       (pprint-pop))
	       :prefix "("
	       :suffix ")")
	  (pprint-indent :block 1)
	  (write-string "constants ")
	  (pprint-exit-if-list-exhausted)
	  (loop 
	   (let* ((term (pprint-pop)))
	     (when term
	       (write-char #\space)
	       (write-char #\()
	       (write term)
	       (write-char #\space)
	       (write (term~type term))
	       (write-char #\))))
	   (pprint-exit-if-list-exhausted)
	   (pprint-newline :linear)))
	 (pprint-newline :fill)
	 (pprint-logical-block 
	  (nil (mapcar #'(lambda (sym) (post~read-object sym env :existing-term))
		       (pprint-pop))
	       :prefix "("
	       :suffix ")")
	  (pprint-indent :block 1)
	  (write-string "meta-variables ")
	  (pprint-exit-if-list-exhausted)
	  (loop 
	   (let* ((term (pprint-pop)))
	     (when term
	       (write-char #\space)
	       (write-char #\()
	       (write term)
	       (write-char #\space)
	       (write (term~type term))
	       (write-char #\))))
	   (pprint-exit-if-list-exhausted)
	   (pprint-newline :linear)))
	 (pprint-newline :fill)
	 (pprint-logical-block 
	  (nil (mapcar #'(lambda (sym) (post~read-object sym env :existing-term))
		       (pprint-pop))
	       :prefix "("
	       :suffix ")")
	  (pprint-indent :block 1)
	  (write-string "variables ")
	  (pprint-exit-if-list-exhausted)
	  (loop 
	   (let* ((term (pprint-pop)))
	     (when term
	       (write-char #\space)
	       (write-char #\()
	       (write term)
	       (write-char #\space)
	       (write (term~type term))
	       (write-char #\))))
	   (pprint-exit-if-list-exhausted)
	   (pprint-newline :linear))))
	(pprint-newline :mandatory)
	(write-string "(conclusion ")
	(write-string (princ-to-string (keim~name (prob~proof-root proof))))
	(write-char #\))
	(pprint-newline :mandatory)
	(pprint-logical-block
	 (nil (prob~proof-assumptions proof)
	      :prefix "("
	      :suffix ")")
	 (write-string "assumptions")
	 (pprint-exit-if-list-exhausted)
	 (pprint-indent :block 1)
	 (loop
	  (write-char #\space)
	  (write-string (princ-to-string (keim~name (pprint-pop))))
	  (pprint-exit-if-list-exhausted)))
	(pprint-newline :mandatory)
	(pprint-logical-block 
	 (nil (pds~open-nodes proof)
	      :prefix "("
	      :suffix ")")
	 (pprint-indent :block 2)
	 (write-string "open-nodes")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write-char #\space)
	  (write (symbol-name (keim~name (pprint-pop))))
	  (pprint-exit-if-list-exhausted)))
	(pprint-newline :mandatory)
	(pprint-logical-block 
	 (nil (pds~support-nodes proof)
	      :prefix "("
	      :suffix ")")
	 (pprint-indent :block 2)
	 (write-string "support-nodes")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write-char #\space)
	  (write (symbol-name (keim~name (pprint-pop))))
	  (pprint-exit-if-list-exhausted)))
	(pprint-newline :mandatory)
	(pprint-logical-block
	 (nil				
	  (prob~proof-steps proof)
	  :prefix "("
	  :suffix ")")
	 (write-string "nodes ")
	 (pprint-indent :block 1)
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (pprint-newline :fill)
	  (write (pprint-pop))
	  (pprint-exit-if-list-exhausted)))
	(pprint-newline :mandatory)
	(pprint-logical-block 
	 (nil (pds~lemmata proof)
	      :prefix "("
	      :suffix ")")
	 (pprint-indent :block 2)
	 (write-string "lemmata")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write-char #\space)
	  (write (symbol-name (keim~name (pprint-pop))))
	  (pprint-exit-if-list-exhausted)))
	(pprint-newline :mandatory)
	(write (pds~agenda proof))
;        (pprint-logical-block 
;         (nil (remove-if #'null (agenda~for-loui (pds~agenda proof)))
;              :prefix "("
;              :suffix ")")
;         (pprint-indent :block 2)
;         (write-string "tasks")
;         (pprint-exit-if-list-exhausted)
;         (loop
;          (write-char #\space)
;          (pprint-logical-block
;           (nil (pprint-pop) :prefix "(" :suffix ")")
;           (loop
;            (write (pprint-pop))
;            (pprint-exit-if-list-exhausted)
;            (write-char #\space)
;            (pprint-newline :fill)))
;          (pprint-exit-if-list-exhausted)
;          (write-char #\space)
;          (pprint-newline :fill)))
	(pprint-newline :mandatory)
	(pprint-logical-block
	 (nil (pds=nodes&controls (prob~proof-steps proof))
	      :prefix "("
	      :suffix ")")
	 (write-string "controls")
	 (pprint-indent :block 1)
	 (loop
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :mandatory)
	  (let ((node&ctrls (pprint-pop)))
	    (write-char #\()
	    (write-string (symbol-name (keim~name (first node&ctrls))))
	    (pprint-logical-block
	     (nil (rest node&ctrls)
		  :prefix " " :suffix ")")
	     (loop
	      (let ((ctrl (pprint-pop)))
		(if ctrl
		    (write ctrl)
		  (write-string "()")))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)
	      (pprint-newline :fill))))))
	(pprint-newline :mandatory)
	(pprint-logical-block
	 (nil (pds=plan-steps&nodes (pds~plan-steps proof))
	      :prefix "("
	      :suffix ")")
	 (write-string "plan-steps ")
	 (pprint-indent :block 1)
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (pprint-newline :fill)
	  (pprint-logical-block
	   (nil (pprint-pop)
		:prefix "(" :suffix ")")
	   (write-string (symbol-name (keim~name (pprint-pop))))
	   (write-char #\space)
	   (write (pprint-pop))
	   (loop
	    (pprint-exit-if-list-exhausted)
	    (write-char #\space)
	    (write-string (symbol-name (keim~name (pprint-pop))))
	    (write-char #\space)
	    (write (pprint-pop))))
	  (write-char #\space)
	  (pprint-exit-if-list-exhausted)))
	))))
  (pdsn+node
   (lambda (stream line)
     (let ((*standard-output* stream))
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	(write-string (etypecase (keim~name line)
			(symbol (symbol-name (keim~name line)))
			(string (keim~name line))))
	(write-char #\space)
	(pprint-logical-block
	 (nil nil) 
	 (pprint-logical-block
	  (nil (mapcar #'keim~name (pdsn~hyps line))
	       :prefix "(" :suffix ")")
	  (pprint-exit-if-list-exhausted)
	  (loop
	   (let ((name (pprint-pop)))
	     (write-string (etypecase name
			(symbol (symbol-name name))
			(string name))))
	   (pprint-exit-if-list-exhausted)
	   (write-char #\space)))
	 (write-char #\space)
	 (write (node~formula line)))
	(pprint-newline :mandatory)
	(pprint-logical-block
	 (nil (pdsj~position-all-justs (node~justification line))
	      :prefix "(" :suffix ")")
	 (write (pprint-pop))
	 (write-char #\space)
	 (loop
	  (write (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :mandatory)))
	(pprint-newline :mandatory)))))
  (pdsj+justification
   (lambda (stream just)
     (let ((*standard-output* stream))
       (pprint-logical-block
	(nil nil :prefix "(" :suffix ")")
	;; from each justification we post-print 
	;; the method name, a list of post representation for the parameters,
	;; a premise list, a status, a post representation of a substitution,
	;; a post representation of an outline-pattern.
	;;first method name
	(write  (string (keim~name (just~method just))) :escape t :pretty nil)
	(write-char #\space)
	;;parameter list
	(pds=post-print-list (pdsj~parameters just) stream)
	(write-char #\space)
	;;premise list
	(pprint-logical-block
	 (nil (just~premises just)
	      :prefix "(" :suffix ")")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write-string (symbol-name (keim~name (pprint-pop))))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space)))
	(write-char #\space)
	;;status
	(write (string (pdsj~status just)) :escape t :pretty nil)
	(write-char #\space)
	(pprint-newline :fill)
	(let ((subst (pdsj~subst just)))
	  (if subst
	      (write subst)
	    (write-string "()")))
	(write-char #\space)
	(pprint-newline :fill)
	;;outline-pattern
	(pprint-logical-block
	 (nil (pdsj~outline-pattern just)
	      :prefix "(" :suffix ")")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write (string (pprint-pop)) :escape t :pretty nil)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space)))
	))))
  (pdsc+control
   (lambda (stream ctrl)
     (let ((*standard-output* stream))
       (if ctrl
	   (pprint-logical-block
	    (nil nil :prefix "(" :suffix ")")
	    (pprint-logical-block
	     (nil (pdsc~sponsors ctrl)
		  :prefix "(" :suffix ")")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (write-string (symbol-name (keim~name (pprint-pop))))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)))
	    (write-char #\space)
	    (pprint-logical-block
	     (nil (pdsc~unsponsors ctrl)
		  :prefix "(" :suffix ")")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (write-string (symbol-name (keim~name (pprint-pop))))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)))
	    (write-char #\space)
	    (pprint-logical-block
	     (nil (pdsc~alternatives ctrl)
		  :prefix "(" :suffix ")")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (write (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)))
	    (write-char #\space)
	    (pprint-logical-block
	     (nil (pdsc~why-method ctrl)
		  :prefix "(" :suffix ")")
	     (pprint-exit-if-list-exhausted)
	     (loop
	      (write (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\SPACE))))
	 (pprint-logical-block
	  (nil nil :prefix "(" :suffix ")")))
       )))
  ))


(defmethod post~print ((line pdsn+node) stream)
  (let ((*standard-output* stream))
    (pp~pprint line 'pds-post)))

(defmethod post~print ((proof pds+proof-plan) stream)
  (let ((*standard-output* stream))
    (pp~pprint proof 'pds-post)))

(defun pds=post-print-list (list stream)
  (declare (edited  "09-JUL-1997")
	   (authors Lassaad)
	   (input   "A list of pds objects, and a stream.")
	   (effect  "Prints on the stream a POST-similar representation of LIST.")
	   (value   "Unspecified."))
  (if list
      (progn 
	(format stream "(")
	(dolist (obj list)
	  (pds~post-print obj stream)
	  )
	(format stream ")"))
    (format stream "()")))
    
(defgeneric pds~post-print (pds-obj stream)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds object, and a stream.")
	   (effect  "Prints on the stream a POST-similar representation of PDS-OBJ.")
	   (value   "Unspecified."))
  (:method ((pds-obj-list cons) stream)
	   (pds=post-print-list pds-obj-list stream))
  (:method ((pds-obj th+def) stream)
	   (format stream "(:pds-thdef ~A)" (keim~name pds-obj)))
  (:method ((pds-obj pdsn+node) stream)
	   (format stream "(:pds-node ~A)" (keim~name pds-obj)))
  (:method ((pds-obj pdsj+justification) stream)
	   (let ((just-node (pds=get-just-node pds-obj)))
	     (if just-node
		 (format stream "(:pds-just (~A ~A))"
			 (keim~name just-node)
			 (position pds-obj (pdsj~all-justs pds-obj)))
	       (error ";;;PDS~~POST-PRINT: Dont know how to output the pds object ~A"
		      pds-obj))))
  (:method ((pds-obj pdsc=actual-node) stream)
	   (let* ((an-node (pdsc~an-node pds-obj))
		  (an-just (pdsc~an-just pds-obj))
		  (jpos (position an-just (pdsn~all-justs an-node))))
	     (format stream "(:pds-anode (~A ~A))" (keim~name an-node) jpos)))
  (:method ((pds-obj term+variable) stream)
	   (format stream "(:pds-var (~A ~A))"
		   (post~print pds-obj nil)
		   (post~print (term~type pds-obj) nil)))
  (:method ((pds-obj term+term) stream)
	   (format stream "(:pds-term ~A)" (post~print pds-obj nil)))
  (:method ((pds-obj null) stream)
	   (format stream "(:pds-nil)"))
  (:method ((pds-obj symbol) stream)
	   (format stream "(:pds-symbol ~A)" pds-obj))
  (:method ((pds-obj number) stream)
	   (format stream "(:pds-number ~A)" pds-obj))
  (:method ((pds-obj subst+substitution) stream)
	   (format stream "(:pds-subst (~A"
		   (mapcar #'keim~name (subst~domain pds-obj)))
	   (pds=post-print-list (subst~codomain pds-obj) stream)
	   (format stream "))"))
  (:method ((pds-obj type+type) stream)
	   (format stream "(:pds-type ~A)" (post~print pds-obj nil)))
  (:method ((pds-obj t) stream)
	   (format stream "(:pds-post-obj ~A)" (post~print pds-obj nil)))
  (:method ((pds-obj sort+sort) stream)
	   (format stream "(:pds-sort ~A)" (post~print pds-obj nil)))
  )

(defun pds=get-just-node (just)
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The node, JUST is one of its justifications, if there is"
		    "some direct or indirect referencing using the JUST reasons,"
		    "Otherwise, nil."))
  (let ((own-reason (or (pdsj~own-reason just)
			(pdsj~above-own-reason just)
			(pdsj~below-own-reason just))))
    (if own-reason
	(pdsc~an-node own-reason)
      (let ((other-reasons (pdsj~all-other-reasons just)))
	(pds=reasons-get-just-node other-reasons just)))
    ))

(defun pds=reasons-get-just-node (reasons just)
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A reason list, and a justification.")
	   (effect  "None.")
	   (value   "The node, JUST is one of its justifications, if this is a"
		    "premise in the justification of one reason from REASONS,"
		    "Otherwise, nil."))
  (when reasons
    (let* ((reason-just (pdsc~an-just (first reasons)))
	   (premises (just~premises reason-just))
	   (just-node (find-if #'(lambda (node)
				   (find just
					 (pdsn~all-justs node)))
			       premises)))
      (or just-node
	  (pds=reasons-get-just-node (rest reasons) just)))
    ))


(defun pds=nodes&controls (nodes)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A node list.")
	   (effect  "None.")
	   (value   "A list containing for each node which has at least a non-empty"
		    "control a list with the node itself as first element and the rest"
		    "elements are the node controls beginning with the one of the most"
		    "abstract justification."))
  (when nodes
    (let* ((node (first nodes))
	   (node-just (node~justification node))
	   (justs (append (pdsj~above-justs node-just)
			  (list node-just)))
	   (controls (mapcar #'pdsj~control justs)))
      (if (every #'null controls)
	  (pds=nodes&controls (rest nodes))
	(cons (cons node controls)
	      (pds=nodes&controls (rest nodes)))))
    ))


(defun pds=plan-steps&nodes (steps)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A plan step list.")
	   (effect  "None.")
	   (value   "A list consisting of an abstract representation of these STEPS."
		    "Each plan step is represented by a list of nodes"
		    "alternated by numbers. The first node corresponds to the node"
		    "closed by the plan step. The number thereafter is the number"
		    "of the corresponding justification. The rest of this list"
		    "delivers the involved nodes with the associated justifications."))
  (when steps
    (let ((step (first steps)))
      (cons (pds=step-nodes&just-numbers step)
	    (pds=plan-steps&nodes (rest steps))))
    ))
	
    
(defun pds=step-nodes&just-numbers (step)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A step.")
	   (value   "A list containing the STEP conclusion as first element then the"
		    "number of its justification generated by this STEP and the rest"
		    "consists of the involved nodes followed by the number of their"
		    "associated justifications, i.e. the justifications that contain"
		    "this STEP."))
  (let* ((step-just (pdsc~an-just step))
	 (step-node (pdsc~an-node step))
	 (just-number (position step-just
				(pdsj~all-justs step-just)))
	 (just-prems (just~premises step-just))
	 (the-below-just (pdsj~least-below-just step-just))
	 (below-reason (pdsj~below-own-reason step-just)))
    (if the-below-just
	;;JUST was expanded:
	(if below-reason
	    ;;to an open justification which was closed by a method application. The closed
	    ;;justification corresponds to the BELOW-REASON justification: Use the sponsors
	    ;;and the premises of this justification as candidates to compute the STEP involved
	    ;;node, justification number pairs.
	    (let* ((the-just (pdsc~an-just below-reason))
		   (cand-nodes (union (just~premises the-just)
				      (pdsj~sponsors the-just))))
	      (cons step-node
		    (cons just-number
			  (pds=get-step-nodes&just-numbers step
							   (set-difference cand-nodes just-prems)
							   just-prems))))
	  (if (pdsj~open-just-p the-below-just)
	      ;;to an open justification which is still open: Use the sponsors of this justification
	      ;;and the premises of its above justification as candidates to compute the STEP involved
	      ;;node, justification number pairs.
	      (let ((cand-nodes (union (pdsj~sponsors the-below-just)
				       (just~premises (pdsj~above the-below-just)))))
		(cons step-node
		      (cons just-number
			    (pds=get-step-nodes&just-numbers step
							     (set-difference cand-nodes just-prems)
							     just-prems))))
	    ;;to a closed justification: Use the premises of this justification as candidates to
	    ;;compute the STEP involved node, justification pairs.
	    (cons step-node
		  (cons just-number
			(pds=get-step-nodes&just-numbers step
							 (set-difference (just~premises the-below-just) just-prems)
							 just-prems)))))
      ;;JUST was not expanded: Use the new hypotheses as candidates to compute the STEP involved
      ;;node, justification number pairs.
      (let* ((node-hyps (pdsn~hyps step-node))
	     (new-hyps (apply #'append
			      (mapcar #'(lambda (n)
					  (set-difference (pdsn~hyps n) node-hyps))
				      just-prems))))
	(cons step-node
	      (cons just-number (pds=get-step-nodes&just-numbers step new-hyps just-prems)))))
    ))


(defun pds=get-step-nodes&just-numbers (step roots leafs)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A plan step, and two node lists.")
	   (effect  "None.")
	   (value   "All nodes of the trees with roots in ROOTS and with leafs in LEAFS"
		    "with the number of their justifications that contain STEP."))
  (if roots
      (let* ((root (first roots))
	     (root-just (node~justification root)))
	(if (pdsn~hypothesis-node-p root)
	    (if (find step (pdsj~reasons root-just))
		(cons root
		      (cons 0
			    (pds=get-step-nodes&just-numbers step (rest roots) leafs)))
	      (pds=get-step-nodes&just-numbers step (rest roots) leafs))
	  (let* ((step-just&num (pdsj~with-reason-level-p root-just step))
		 (the-just (if step-just&num (first step-just&num)
			     root-just))
		 (the-below-just (pdsj~least-below-just the-just))
		 (the-own-reason (or (pdsj~own-reason the-just)
				     (pdsj~below-own-reason the-just)))
		 (cand-roots
		  (if the-below-just
		      (if the-own-reason
			  (let ((just (pdsc~an-just the-own-reason)))
			    (union (just~premises just)
				   (pdsj~sponsors just)))
			(if (pdsj~open-just-p the-below-just)
			    (union (pdsj~sponsors the-below-just)
				   (just~premises (pdsj~above the-below-just)))
			  (just~premises the-below-just)))
		    (just~premises the-just)))
		 (new-roots (set-difference cand-roots leafs)))
	    (if step-just&num
		(cons root
		      (cons (rest step-just&num)
			    (pds=get-step-nodes&just-numbers step
							     (remove-duplicates (append (rest roots)
											new-roots))
							     leafs)))
	      (pds=get-step-nodes&just-numbers step
					       (remove-duplicates (append (rest roots) new-roots))
					       leafs)))))
    (apply #'append (mapcar #'(lambda (leaf)
				(list leaf
				      (rest (pdsj~with-reason-level-p (node~justification leaf)
								      step))))
			    leafs))
    ))

	     
;;; TO DO: Effizienter geht es wie in proof~linearize-proof-tree oder koennte vielleicht
;;; ersetzt durch proof~linearize-proof-tree. Aber erst nach der Einfuehrung von verschiedenen
;;; Justifications
(defgeneric pds~linearize-plan (node)
  (declare (edited  "15-JAN-1997")
	   (authors Sorge)
	   (input   "A node in a PDS structure")
	   (value   "Lineraizes the structure"))
  (:method (obj)
	   (error ";;; ~A is not a proof-node" obj))
  (:method ((node node+node))
	   (let* ((just (node~justification node))
		  (premises (pdsj~ass&premises just)))
	     (remove-duplicates (cons node (apply #'append (mapcar #'pds~linearize-plan premises))))))
  )


(defun pds~insert-hyps! (toaddline sponsorlines-list &optional (pds pds*current-proof-plan))
  (setf (pdsn~hyps toaddline)
	(remove-duplicates (append (pdsn~hyps toaddline)
				   (first sponsorlines-list)))))

