;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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

(mod~defmod prob :uses (env keim mod node post term type)
	    :documentation "Datastructures and basic functionality for deduction problems."
	    :exports (
		      prob+problem

		      prob~create
		      prob~find-problem
		      prob~print
		      ;;; prob~read  gone with the wind... VS
		      prob~read-problem-from-file ;; just come from the dusk... AM
		      prob~p
		      prob~proven-p
		      prob~set-proven
		      prob~status
		      prob~theory
		      prob~environment
		      prob~conclusion
		      prob~assumptions
		      prob~category
		      prob~proofs
		      prob~proof-names
		      prob~add-proof
		      prob~remove-proof
		      prob~proof

		      prob+proof

		      prob~proof-p
		      prob~proof-create
		      prob~find-proof
		      prob~proof-problem
		      prob~proof-theory
		      prob~proof-root
		      ;;; proof~steps --> prob~proof-steps
		      prob~proof-steps
		      prob~proof-conclusion
		      prob~proof-assumptions
		      prob~proof-environment
		      prob~linearize-proof-tree
		      )
	    )

#{\section{\keim-deduction Problems and Proofs}\label{mod:prob}#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attaching the proof module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#{\subsection{Proofs}\label{mod:proof}

All of the intended proof formats have in common, that they can be viewed to consist of steps of a special
class.  These steps can be simple proof steps like resolution steps, natural deduction steps etc. or proofs
themselves.  The proof itself can be viewed as a tree of proof-steps. Therefore the class {\vb PROB+PROOF}
contains the root of such a tree, which could be of type {\vb NODE+NODE}
(for example a proof-root for a resolution proof would simply consist of box). Furthermore all the single
steps of the proof are stored so we can access them even when the complete tree is not yet connected. 
The proof relates to both a problem and a theory, so one can refere to both environments to construct a proof-tree.#}

(eval-when (load compile eval)
(defclass prob+proof (help+help)
  ((problem :initarg :problem
	    :initform nil
	    :accessor prob=proof-problem
	    :documentation "The problem which is proved.")
   (theory :initarg :theory
	   :initform nil
	   :accessor prob=proof-theory
	   :documentation "The theory in which the proof is valid.")
   (root :initarg :root
	 :initform nil
	 :accessor prob~proof-root
	 :documentation "The root of the proof-tree.")
   (steps :initarg :steps
	  :initform nil
	  :reader prob~proof-steps
	  :writer prob=proof-steps
	  :documentation "Steps in the problem.")
  )
  (:documentation "This is the class of all KEIM-proofs.")))


(defvar prob*proof-hash-table (make-hash-table :test #'equal)
  "A hash table, that keeps track of all the existing proofs at any given time.")


(defun prob~proof-p (obj)
  (declare (edited  "28-AUG-1995 20:51")
	   (authors SORGE)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is a proof."))
  (typep obj 'prob+proof))


(defun prob~proof-create (name problem theory root steps help)
  (declare (edited  "28-AUG-1995 20:26")
	   (authors SORGE)
	   (input   "A name, a problem, a theory, a proof-root, a list of proof steps and a help-string.")
	   (effect  "None.")
	   (value   "A multi proof with SUBPROOFS as subproofs in the steps slot."))
  (make-instance 'prob+proof
		 :name (prob=read-string name)
		 :problem problem
		 :theory theory
		 :root root
		 :steps steps
		 :help help))
		 
  
(defgeneric prob~find-proof (name)
  (declare (edited  "29-AUG-1995 12:55")
	   (authors SORGE)
	   (input   "A name of a proof.")
	   (effect  "None.")
	   (value   "The Proof with this name or NIL if none exists."))
  (:method ((name string))
	   (gethash (prob=read-string name) prob*proof-hash-table))
  (:method ((name symbol))
	   (gethash (prob=read-string name) prob*proof-hash-table))
  (:method ((proof prob+proof))
	   (gethash (keim~name proof) prob*proof-hash-table)))


#{\subsection{Accessor functions}#}


(defsetf prob~proof-steps (proof) (steps)
  `(prob=proof-steps-medium ,proof ,steps))

(defgeneric prob=proof-steps-medium (proof steps)
  (:method (proof steps)
	   (error ";;;PROB~~PROOF-STEPS: wrong type of ~A." proof))
  (:method ((proof prob+proof) steps)
	   (prob=proof-steps steps proof)))

(defgeneric prob~proof-problem (proof)
  (declare (edited  "22-MAY-1997 10:28")
	   (authors SORGE)
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The problem for wich PROOF is a proof."))
  (:method (proof)
	   (error "~A must be of type PROB+PROOF" proof))
  (:method ((proof prob+proof))
	   (prob=proof-problem proof)))

(defsetf prob~proof-problem (proof) (problem)
  (declare (edited  "22-MAY-1997 10:32")
	   (authors SORGE)
	   (input   "A generic proof and a problem.")
	   (effect   "The problem of PROOF is set to PROBLEM.")
	   (value    "Undefined."))
  `(if (prob~proof-p ,proof)
       (if (prob~p ,problem)
	   (setf (prob=proof-problem ,proof) ,problem)
	 (error ";;;PROB~~PROOF-PROBLEM: ~A must be of type PROB+PROBLEM." ,problem))
     (error ";;;PROB~~PROOF-PROBLEM: ~A must be of type PROB+PROOF." ,proof)))

(defgeneric prob~proof-theory (proof)
  (declare (edited  "28-AUG-1995 20:02")
	   (authors SORGE)
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The theory in which the proof is valid."))
  (:method (proof)
	   (error "~A must be of type PROB+PROOF" proof))
  (:method ((proof prob+proof))
	   (prob=proof-theory proof)))

(defsetf prob~proof-theory (proof) (theory)
  (declare (edited  "21-MAY-1997 18:35")
	   (authors SORGE)
	   (input   "A generic proof and a theory.")
	   (effect   "The theory of PROOF is set to THEORY.")
	   (value    "Undefined."))
  `(if (prob~proof-p ,proof)
       (if (th~p ,theory)
	   (setf (prob=proof-theory ,proof) ,theory)
	 (error ";;;PROB~~PROOF-THEORY: ~A must be of type TH+THEORY." ,theory))
     (error ";;;PROB~~PROOF-THEORY: ~A must be of type PROB+PROOF." ,proof)))


(defmethod shared-initialize :after ((obj prob+proof) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (gethash (prob=read-string (keim~name obj)) prob*proof-hash-table)
	obj)
  obj)
  
#{\subsection{Reader Shortcuts}#}

(defgeneric prob~proof-conclusion (proof)
  (declare (edited  "06-JUN-1997 13:16")
	   (authors SORGE)
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The conclusion of the proofs problem."))
  (:method (proof)
	   (error "~A must be of type PROB+PROOF" proof))
  (:method ((proof prob+proof))
	   (prob~conclusion (prob~proof-problem proof))))

(defgeneric prob~proof-assumptions (proof)
  (declare (edited  "06-JUN-1997 13:16")
	   (authors SORGE)
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The assumptions of the proofs problem."))
  (:method (proof)
	   (error "~A must be of type PROB+PROOF" proof))
  (:method ((proof prob+proof))
	   (prob~assumptions (prob~proof-problem proof))))

(defgeneric prob~proof-environment (proof)
  (declare (edited  "06-JUN-1997 13:16")
	   (authors SORGE)
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The environment of the proofs problem."))
  (:method (proof)
	   (error "~A must be of type PROB+PROOF" proof))
  (:method ((proof prob+proof))
	   (prob~environment (prob~proof-problem proof))))

#{\subsection{Algorithms}
Here we provide a set of algorithms for \keim\ proofs. {\vb PROB~LINEARIZE-PROOF-TREE} is needed for instance
whenever a proof is written to a stream.#}



(defun prob~linearize-proof-tree (root-node)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A proof node.")
	   (effect  "None.")
	   (value   "A linear list of all proof nodes in the DAG obtained through the justification of ROOT-TREE in correct order."
		    "This function performs a depth-up-to-joins search by first counting the occurrences of all nodes in the tree"
		    "and then taking the first one with a 0 and decrementing is successors."))
  (let ((property-key (gentemp "prob=number")))
    (prob=count-nodes root-node property-key)
    (keim~put root-node property-key 0)
    (nreverse (prob=linearize (list root-node) property-key))))

(defun prob=count-nodes (node property-key)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A proof node and a symbol.")
	   (effect  "The property PROPERTY-KEY in all nodes in the tree is incremented by 1 or initialized with 1.")
	   (value   "Undefined."))
  (let ((number (keim~get node property-key)))
    (cond ((null number)
	   (keim~put node property-key 1)
	   (mapc #'(lambda (node) (prob=count-nodes node property-key))
		 (node~just-premises node)))
	  (t (keim~put node property-key (1+ number))))))

(defun prob=linearize (node-list property-key)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A list of proof nodes and a symbol.")
	   (effect  "The property PROPERTY-KEY in all nodes is decremented step by step and removed finally.")
	   (value   "A list of nodes."))
  (if (null node-list)
      nil
      (let* ((next-node (find-if #'(lambda (node)
				     (zerop (keim~get node property-key)))
				 node-list))
	     (counter (count next-node node-list))
	     (next-list (append (mapcan #'(lambda (node)
					    (let ((number (- (keim~get node property-key) counter)))
					      (keim~put node property-key number)
					      (if (zerop number) (list node) nil)))
					  (if next-node
					      (node~just-premises next-node)
					    nil))
				(delete next-node node-list))))
	(keim~remprop next-node property-key)
	(cons next-node (prob=linearize next-list property-key)))))


#{\subsection{Problems}\label{mod:proof}

\keim-deduction problems are the basic data structures for protocolling deductions based on \keim.  A
problem consists of the formulas of the theorem that is to be proved, divided into {\em assumptions} and a
{\em conclusion}. As each \keim-deduction problem can provide its own environment, i.e. {\em variables},
{\em constants}, {\em assumptions}, {\em conclusions} etc., it can use both its own and the global definitions
of the theory it belongs to. (See also section \ref{mod:th-ass} for more information about the environment of
				  a theorem.)#}


(eval-when (load compile eval)
(defclass prob+problem (help+help)
  ((theory :initarg :theory
	   :accessor prob=theory
	   :documentation "The theory in which the problem is defined.")
   (status :initarg :status
	   :initform 'conjectured
	   :accessor prob~status
	   :documentation "Status of the proof: conjectured, unchecked, proven, ...")
   (category :initarg :category
	      :initform 'theorem
	      :accessor prob~category
	      :documentation "Category of the problem: Theorem, Lemma, Proposition, ...")
   (conclusion :initarg :conclusion
	       :accessor prob=conclusion
	       :documentation "What the proof is allegedly proving.");doc
   (assumptions :initarg :assumptions
	 :accessor prob=assumptions
	 :documentation "Allowed assumptions (a list of proof nodes) for this proof.");doc
   (environment :initarg :environment
		:accessor prob~environment
		:documentation "An environment (constants, variables, etc.).")
   (proofs :initarg proofs
	   :initform nil
	   :accessor prob=proofs
	   :documentation "An association list of proofs."))
  (:documentation "A KEIM-deduction problem object.")))

;; We'll use this hash table to keep track of all the problems that
;; exist at any given time 
(defvar prob*problem-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by problem name, that holds all existing problems.
This way we can refer to problems by name.")


(defgeneric prob=read-string (name)
  (declare (edited  "20-JUL-1995 13:55")
	   (authors SORGE)
	   (input   "A name (symbol or string).")
	   (effect  "None.")
	   (value   "NAME as an uppercase string."))
  (:method (name)
	   (error "~A has to be of type symbol or string" name))
  (:method ((name symbol))
	   (symbol-name name))
  (:method ((name string))
	   (string-upcase name)))


;;; redo VS
(defgeneric prob~print (object &optional stream)
  (declare (edited  "11-NOV-1992 12:20")
	   (authors RICHTS sorge)
	   (input   "A problem (which can also be a proof) and a stream.")
	   (effect  "This around-method prints a readable form of PROBLEM (the assertions) on STREAM."
		    "Then before closing the final parenthesis it calls the next method, which then prints the proof-slots"
		    "if there are any or does nothing if PROBLEM is a simple problem.")
	   (value   "Undefined."))
  (:method :around ((problem  prob+problem) &optional (stream t))
   (format stream "~%(Problem ~S (~S) ~{~%  ~S~}~%  ~S" (keim~name problem) (keim~name (prob~theory problem))
	   (prob~assumptions problem) (prob~conclusion problem))
   (call-next-method)
   (format stream ")"))
  (:method ((problem  prob+problem) &optional (stream t))
   (declare (ignore stream)))
  (:method ((node  node+node) &optional (stream t))
   (format stream "~%~S:~%   ~S" (node~justification node) node)))



#{\subsection{Auxiliary functions}#}

(defun prob~p (thing)
  (declare
   (authors nesmith)
   (input  "A lisp object THING.")
   (effect "None.")
   (value  "T if THING is a prob+problem, nil otherwise."))
  (typep thing 'prob+problem)
  )

(defgeneric prob~proven-p (problem)
  (declare (edited  "02-JUN-1997 17:16")
	   (authors SORGE)
	   (input   "A problem.")
	   (value   "T if problem is proven, otherwise NIL."))
    (:method (problem)
           (error "~A must be of type PROB+PROBLEM" problem))
  (:method ((problem string))
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (prob~proven-p prob)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem symbol))
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (prob~proven-p prob)
	       (error "Problem ~A does not exist" problem))))
  (:method ((proof prob+proof))
	   (prob~proven-p (prob=proof-problem proof)))
  (:method ((problem prob+problem))
	   (string-equal (prob~status problem) :proven)))

(defgeneric prob~set-proven (problem)
  (declare (edited  "02-JUN-1997 17:16")
	   (authors SORGE)
	   (input   "A problem.")
	   (value   "Sets the status slot of PROBLEM to proven."))
  (:method (problem)
           (error "~A must be of type PROB+PROBLEM" problem))
  (:method ((problem string))
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (prob~set-proven prob)
	       (error "Problem ~A does not exist" problem))))
  (:method ((problem symbol))
	   (let ((prob (prob~find-problem problem)))
	     (if prob
		 (prob~set-proven prob)
	       (error "Problem ~A does not exist" problem))))
  (:method ((proof prob+proof))
	   (prob~set-proven (prob=proof-problem proof)))
  (:method ((problem prob+problem))
	   (setf (prob~status problem) :proven)))

(defgeneric prob~theory (problem)
  (declare (edited  " 5-JUN-1992 07:57" )
	   (authors KOHLHASE SORGE)
	   (input   "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "The theory to which the problem belongs."))
  (:method ((proof prob+proof))
	   (prob~theory (prob=proof-problem proof)))
  (:method ((problem prob+problem))
	   (prob=theory problem)))

(defsetf prob~theory (problem) (theory)
  (declare (edited  "28-NOV-1996")
	   (authors Lassaad)
	   (input   "A problem and a theory.")
	   (effect  "Sets the theory of PROBLEM to THEORY.")
	   (value   "Unspecified."))
  `(if (prob~p ,problem)
       (if (th~p ,theory)
	   (setf (prob=theory ,problem) ,theory)
	 (error ";;;PROB~~THEORY: ~A must be of type TH+THEORY." ,theory))
     (error ";;;PROB~~THEORY: ~A must be of type PROB+PROBLEM." ,problem)))

(defgeneric prob~conclusion (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "The conlusions to be proven in PROBLEM."))
  (:method ((proof prob+proof))
	   (prob~conclusion (prob=proof-problem proof)))
  (:method ((problem prob+problem))
	   (prob=conclusion problem)))


(defsetf prob~conclusion (problem) (conclusion)
  (declare (edited  "14-MAY-1997")
	   (authors Sorge)
	   (input    "A KEIM-deduction problem and a formula, that is to be the new conclusion of the problem.")
	   (effect   " If CONCLUSION is a formula of type O, then the conclusion of PROBLEM is changed to CONCLUSION.")
	   (value    "Undefined."))
  `(if (node~p ,conclusion)
       (if (type~o-p (term~type (node~formula ,conclusion)))
	   (setf (prob=conclusion ,problem) ,conclusion)
	 (error "The term ~A has type ~A and is therefore not a valid conclusion of the problem ~A"
		conclusion (term~type (node~formula ,conclusion)) ,problem))
     (error "~A must be a node." ,conclusion)))
  
(defgeneric prob~assumptions (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE Sorge)
	   (input    "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "The assumptions of PROBLEM."))
  (:method (problem)
	   (error "~A must be of type PROB+PROBLEM." problem))
  (:method ((proof prob+proof))
	   (prob~assumptions (prob=proof-problem proof)))
  (:method ((problem prob+problem))
	   (prob=assumptions problem)))

(defsetf prob~assumptions (problem) (assumptions)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE Sorge)
	   (input    "A KEIM-deduction problem and a list of terms.")
	   (effect   "If ASSUMPTIONS is a list of formulae of type O,"
		     "then the list of assumptions of PROBLEM is changed to ASSUMPTIONS.")
	   (value    "Undefined."))
  `(if (and (listp ,assumptions)
	    (every #'node~p ,assumptions)
	    (every #'(lambda (x) (type~o-p (term~type (node~formula x)))) ,assumptions))
       (setf (prob=assumptions ,problem) ,assumptions)
     (error "~A must be a list of nodes with formulas of type O." ,assumptions)))

(defgeneric prob~proofs (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors Sorge)
	   (input    "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "A list of proofs for PROBLEM."))
  (:method (problem)
	   (error "~A must be of type PROB+PROBLEM." problem))
  (:method ((problem prob+problem))
	   (mapcar #'rest (prob=proofs problem))))

(defsetf prob~proofs (problem) (proofs)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors Sorge)
	   (input    "A KEIM-deduction problem and a list of proofs.")
	   (effect   "The list of proofs of PROBLEM is changed to PROOFS.")
	   (value    "Undefined."))
  `(if (and (listp ,proofs)
	    (every #'prob~proof-p ,proofs))
       (setf (prob=proofs ,problem)
	     (pairlis (mapcar #'keim~name ,proofs) ,proofs))
     (error "~A must be a list of proofs." ,proofs)))

(defgeneric prob~proof-names (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors Sorge)
	   (input    "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "A list of the names of the proofs for PROBLEM."))
  (:method (problem)
	   (error "~A must be of type PROB+PROBLEM." problem))
  (:method ((problem prob+problem))
	   (mapcar #'car (prob=proofs problem))))

(defgeneric prob~add-proof (problem proof)
  (declare (edited  "28-AUG-1995 21:01")
	   (authors SORGE)
	   (input   "A PROBLEM and a PROOF or list of PROOFS.")
	   (effect  "Adds the proofs to the prooflist of PROBLEM.")
	   (value   "Undefined."))
  (:method (problem proof)
	   (error "~A must be of type PROB+PROBLEM and ~A must either be a list of proofs or of type PROB+PROOF" problem proof))
  (:method ((problem prob+problem) (proofs cons))
	   (if (every #'prob~proof-p proofs)
	       (setf (prob=proofs problem)
		     (pairlis (mapcar #'keim~name proofs)
			      proofs
			      (prob=proofs problem)))
	     (error "~A must be a list of proofs" proofs)))
  (:method ((problem prob+problem) (proof prob+proof))
	   (setf (prob=proofs problem)
		 (acons (keim~name proof) proof (prob=proofs problem)))))

(defgeneric prob~remove-proof (problem proofs &key (completely nil))
  (declare (edited  "15-MAY-1997")
	   (authors Sorge)
	   (input   "A PROBLEM and a PROOF or a list of PROOFS.")
	   (effect  "Removes the PROOFS from the prooflist of PROBLEM.")
	   (value   "Undefined."))
  (:method (problem proof &key completely)
	   (declare (ignore completely))
	   (error "~A must be of type PROB+PROBLEM and ~A must either be a list of proofs or of type PROB+PROOF" problem proof))
  (:method ((problem prob+problem) (proofs cons) &key (completely nil))
	   (if (every #'prob~proof-p proofs)
	       (dolist (x proofs)
		 (prob~remove-proof problem x :completely completely)) 
	     (error "~A must be a list of proofs" proofs)))
  (:method ((problem prob+problem) (proofs prob+proof) &key (completely nil))
	   (setf (prob=proofs problem)
		 (remove (assoc (keim~name proofs) (prob=proofs problem))
			 (prob=proofs problem)))
	   (when completely (remhash (prob=read-string (keim~name proofs)) prob*proof-hash-table))))


(defgeneric prob~proof (problem proof)
  (declare (edited  "29-AUG-1995 13:29")
	   (authors SORGE)
	   (input   "A proof or a name of a proof.")
	   (effect  "None.")
	   (value   "T, if the input is a proof and this proof is in the list of proofs for the problem."
		    "A proof, if the input is a name and a proof of this name is in the list of proofs."
		    "Nil in all other cases."))
  (:method (problem proof)
	   (declare (ignore proof))
	   (error "~A must be of type PROB+PROOF" problem))
  (:method ((problem prob+proof) name)
	   (let ((item (assoc name (prob=proofs problem) :test #'string-equal)))
	     (when item (cdr item))))
  (:method ((problem prob+problem) (proof prob+proof))
	   (when (rassoc proof (prob=proofs problem)))))

;; redo for checking of correctness for proofs VS
(defun prob~create (name theory env assumptions conclusion &optional (proofs nil) (category 'theorem) (help ""))
  (declare (edited  " 3-MAY-1993 10:45" )
	   (authors KOHLHASE SORGE)
	   (input   "A name, status, an environment, a list of assumptions, one conclusion"
		    "and optionally a list of proofs, the category of the problem and a help-string."
		    "The environment are the local declarations of the problem, with the environment of the"
		    "theory as a parent.")
	   (effect  "None.")
	   (value   "The respective problem."))
  (let ((newproblem 
	 (make-instance 'prob+problem :name name
			:theory theory
			:environment env
			:assumptions assumptions 
			:conclusion conclusion
			:category category
			:help help
			)))
    (when proofs (setf (prob~proofs newproblem) proofs))
    newproblem))


(defgeneric prob~find-problem (name)
  (declare (edited  "19-JUL-1995 22:40")
	   (authors SORGE)
           (input   "A name of a problem (symbol or string).")
           (effect  "None.")
           (value   "The problem with this name, or NIL if none exists."))
  (:method (name)
	   (gethash (prob=read-string name) prob*problem-hash-table))
  (:method ((name prob+problem))
	   (gethash (prob=read-string (keim~name name)) prob*problem-hash-table)))

;; make sure the new problem is in the hash table
(defmethod shared-initialize :after ((obj prob+problem) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (if (eq (find-class 'prob+problem) (class-of obj))
    (setf (gethash (prob=read-string (keim~name obj)) prob*problem-hash-table)
	obj)
    (remhash (prob=read-string (keim~name obj)) prob*problem-hash-table))
  obj)
 
#{\subsection{ Proved Problems}

If a \keim-deduction problem is proven, it can be converted to a proved problem of class {\vb PROB+PROOF}
({\vb TH+THM} or {\vb TH+LEMMA} respectivly as {\vb PROB+PROOF} has no own instances).
The class {\vb PROB+PROOF} is a \keim-deduction problem that can possibly contain several proofs. This 
is useful for formalizing examples that can have multiple proofs or to have proofs in various formats for a
single \keim-deduction problem.#}

;;;post~print + print-object stuff

;;; some nice styles for the follwing would be advisable....  VS

(defun prob=print-hashtable (stream table)
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       (format stream "~A " key))
	   table))

;;;changed to primary method. commented out old primary    DEF
;;;dont know what that ^^^ comment means... VS
(defmethod post~print ((problem prob+problem) stream)
  (declare (edited  "27-MAR-1993 13:04")
	   (authors RICHTS sorge)
	   (input   "A problem (which can also be a proof) and a stream.")
	   (effect  "This method prints the POST-representation of PROBLEM (the declarations) on STREAM.")
	   (value   "Undefined."))
  (cond ((null stream) (with-output-to-string (string-stream)
			 (post~print problem string-stream)))
	((eq stream t) (post~print problem *standard-output*))
	(t
	 (if (prob~proven-p problem)
	     (format stream "~%(Theorem ")
	   (format stream "~%(Problem "))
	 (format stream "~A ~%(in ~A) ~%" (keim~name problem)
		 (keim~name (prob~theory problem)))
	 (format stream "(category ~A) ~%" (prob~category problem)) 
	 (let* ((env (prob~environment problem))
		(type-vars  (env~class-keys env 'type+variable nil))
		(type-constants (env~class-keys env 'type+constant nil))
		(constants (env~class-keys env 'term+constant nil))
		(variables (env~class-keys env 'term+variable nil)))
	   (mapc #'(lambda (x) (env~post-print x 
					       (env~lookup-object x env)
					       stream))
		 (append type-vars type-constants constants variables))
	   (mapc #'(lambda (x) (post~print x stream) (terpri stream))
		 (prob~assumptions problem))
	   (let ((node (prob~conclusion problem)))
	     (format stream "(conclusion " )
	     (post~print (node~formula node) stream)
	     (format stream ")~%"))
	   (format stream "(help ~S)" (help~help-string problem))
	   (format stream ")")))))

(defmethod post~print ((proof prob+proof) stream)
  (declare (edited  "30-AUG-1995 05:54")
	   (authors SORGE)
	   (input   "A proof.")
	   (effect  "Prints a POST-representation of PROOF on STREAM."
		    "This primary-method should be specialized by more elaborated methods"
		    "for subclasses.")
	   (value   "Undefinded."))
  (format stream "(proof ")
  (format stream "~A " (keim~name proof))
  (format stream ")~%"))

(defmethod post~print ((node node+node) stream)
  (declare (edited  "24-MAY-1997 22:08")
	   (authors SORGE)
	   (input   "A node.")
	   (effect  "Prints the POST-representation of assumptions and conclusions as used"
		    "in problems. Any other nodes are not printed as there is no POST~~READ-OBJECT"
		    "method specified for them. Instead a warning is signalled.")
	   (value   "Undefined."))
  (let ((just (keim~name (just~method (node~justification node)))))
    (cond ((string-equal just 'hyp)
	   (format stream "(assumption ~A " (keim~name node))
	   (post~print (node~formula node) stream)
	   (format stream ")"))
	  ((string-equal just 'open)
	   (format stream "(conclusion ~A " (keim~name node))
	   (post~print (node~formula node) stream)
	   (format stream ")"))
	  (t (warn ";;;POST~~PRINT: The node ~A does not have a POST-representation." node)))))

;;; post-syntax

;; ===============================================================================
;;                        POST-anschluss und Environments
;; ===============================================================================


(defmethod post~read-object ((thing list) (env env+environment) 
			     (indicator (eql :conclusion)))
  (let ((name (post~read-symbol (car thing) env)))
    (when (rest thing)
      
      (when (null (post~all-types-checker (cadr thing)))
	(error "~%During reading term ~A, it is only allowed to use 'all-types' at the top of a term" (cadr thing)))
      
      (let* ((term (post~read-object (cadr thing) env :existing-term-closed))
	     (node (when (or (type~o-p (term~type term))
			     (and (term~schema-p term)
				  (type~o-p (term~type (data~schema-range term))))
			     (error ";;;post~~read-object: ~A is not of type O!" term))
		     (node~create name term (just~create (infer~find-method 'open) nil)))))
	node))))

(defmethod post~read-object ((thing list) (env env+environment) 
			     (indicator (eql :assumption)))
  (let ((name (post~read-symbol (car thing) env)))
    (when (rest thing)
      
      (when (null (post~all-types-checker (cadr thing)))
	(error "~%During reading term ~A, it is only allowed to use 'all-types' at the top of a term" (cadr thing)))

      (let* ((term (post~read-object (cadr thing) env :existing-term-closed))
	     (node (when (or (type~o-p (term~type term))
			     (and (term~schema-p term)
				  (type~o-p (term~type (data~schema-range term))))
			     (error ";;; 22 POST~~READ-OBJECT: ~A is not of type O!" term))
		     (node~create name term (just~create (infer~find-method 'hyp) nil)))))
	node))))

;;(defmethod post~read-object ((problem list) (env env+environment)
;;			     (indicator (eql :problem)))
;;  (let* ((name (first problem))
;;	 (theory (prob=find-theory (rest problem)))
;;	 (env (env~create (list (th~env theory))))
;;	 (constants (post~read-object (prob=find-constants (rest problem)) env :constants))
;;	 (variables (post~read-object (prob=find-variables (rest problem)) env :variables))
;;	 (assumptions (mapcar #'(lambda (assum)
;;				  (post~read-object assum env :assumption))
;;			      (prob=find-assumptions (rest problem)))) 
;;	 (conclusion (post~read-object (prob=find-conclusion (rest problem)) env :conclusion)))
;;   (declare (ignore variables constants))
;;    (prob~create name theory env assumptions conclusion)))

(defun prob~read-problem-from-file (file)
  (declare (edited  "22-JAN-1998")
	   (authors Ameier)
	   (input   "A file.")
	   (effect  "None.")
	   (value   "Tries to read the file as problem. If succeeds the problem, otherwise an"
		    "error."))
  (with-open-file (stream file :direction :input
			  :if-does-not-exist :error)
		  (let* ((problem-list (read stream)))
		    (if (and (listp problem-list)
			     (string= (string (first problem-list)) "PROBLEM"))
			(post~read-object problem-list (env~create) nil)
		      (error "~%Not able to read the file ~A as a post-problem." file)))))


(defun prob=find-name (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The name of the problem (the first thing that is not a list itself)."))
  (let ((pos-names (remove-if #'listp problem-rest)))
    (when (null pos-names)
      (error "~% While reading the problem-definition with ~A, no name for the problem was found." problem-rest))
    (when (>= (length pos-names) 2)
      (format t "~%Warning: While reading the problem-definition with ~A, more as one name was found: ~A ~% The first one was choosen."
	      problem-rest pos-names))
    (first pos-names)))

(defun prob=find-theory (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The theory of the problem."))
  (let ((pos-theories (remove-if-not #'(lambda (thing)
					 (string= (string (first thing)) "IN"))
				     problem-rest)))
    (when (null pos-theories)
      (error "~% While reading the problem-definition with ~A, no theory for the problem was found." problem-rest))
    (when (>= (length pos-theories) 2)
      (format t "~%Warning: While reading the problem-definition with ~A, more as one theory was found: ~A ~% The first one was choosen."
	      problem-rest pos-theories))
    (th~require-completely (second (first pos-theories))))) ;; hier muss vielleicht was
							       ;; anderes hin als
							       ;; th~require-only AMEIER

(defun prob=find-conclusion (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The conclusion of the problem."))
  (let* ((pos-conclusions (remove-if-not #'(lambda (thing)
					     (string= (string (first thing)) "CONCLUSION"))
					 problem-rest)))
    (when (null pos-conclusions)
      (error "~% While reading the problem-definition with ~A, no conclusion for the problem was found." problem-rest))
    (when (>= (length pos-conclusions) 2)
      (format t "~%Warning: While reading the problem-definition with ~A, more as one conclusion was found: ~A ~% The first one was choosen."
	      problem-rest pos-conclusions))
    (rest (first pos-conclusions))))

(defun prob=find-assumptions (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The assumptions of the problem."))
  (mapcar #'rest (remove-if-not #'(lambda (thing)
				    (string= (string (first thing)) "ASSUMPTION"))
				problem-rest)))

  
(defun prob=find-constants (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The constants of the problem."))
  (apply 'append (mapcar #'rest (remove-if-not #'(lambda (thing)
						   (string= (string (first thing)) "CONSTANTS"))
					       problem-rest))))

(defun prob=find-variables (problem-rest)
  (declare (edited  "07-JAN-1998")
	   (authors Ameier)
	   (input   "A list of things defining a problem.")
	   (effect  "None.")
	   (value   "The variables of the problem."))
  (apply 'append (mapcar #'rest (remove-if-not #'(lambda (thing)
						   (string= (string (first thing)) "VARIABLES"))
					       problem-rest))))



(defmethod print-object ((obj (eql prob*problem-hash-table)) stream)
  (format stream "#<PROBLEM ")
  (prob=print-hashtable stream obj)
  (format stream ">")
)

(defmethod print-object :before ((obj prob+problem) stream)
  (if (prob~proven-p obj)
      (format stream "Theorem:   ")
    (format stream "Problem:   ")))

(defmethod print-object ((problem prob+problem) stream)
  (format stream "~A (~A) ~A"
	  (keim~name problem)
	  (keim~name (prob~theory problem))
	  (help~help-string problem)))

(defmethod print-object ((obj prob+proof) stream)
  (format stream "PROOF ~A (for ~A) (in ~A) ~A ~%"
	  (keim~name obj)
	  (keim~name (prob~proof-problem obj))
	  (keim~name (prob~proof-theory obj))
	  (help~help-string obj)))

(defmethod print-object ((obj (eql prob*proof-hash-table)) stream)
  (format stream "#<PROOF ")
  (prob=print-hashtable stream obj)
  (format stream ">")
)
