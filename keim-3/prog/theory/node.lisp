;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(mod~defmod node :uses (just keim mod lab) 
	    :documentation "Datastructures and basic functionality for generic proof nodes."
	    :exports (
		      node+node
		      node~create
		      node~p
		      node~formula
		      ;;node~set-formula! -> (setf (node~formula node) ..)
		      node~justification
		      ;; node~set-justification! -> (setf (node~justification 
		      ;;node~just-nodes -> node~just-premises
		      node~just-premises
		      node~just-method
		      node~make-new-name
		      node~post-print-step
		      node~post-print-steps
		      )
	    )

#{\section{Proof Nodes}\label{mod:node}

Proof nodes are abstract objects which contain a name (a label), a sequent, and a justification. They could for instance
be used as lines in a natural deduction proof or as clauses in a resolution proof.#}

(eval-when (load compile eval)
  (defclass node+node (keim+name lab+mark)
    ((formula :accessor node~formula
	      :initarg :formula
	      :documentation "The formula for a proof node.")
     (justification :reader node~justification
		    :writer node=justification
		    :initarg :justification
		    :type just+justification
		    :documentation "The justification for a proof node."))
    (:documentation "A node in the plan data structure.")))


(defun node~create (label wff just)
  (declare (edited  "17-SEP-1996")
	   (authors Lassaad)
	   (input "A label, a formula and a justification")
	   (effect "None.")
	   (value  "An instance of class NODE+NODE with corresponding slot values"))
  (let ((node (make-instance 'node+node :name label :formula wff
                             :justification just)))
    (if (just~premises just)
	(setf (lab~mark node) (lab~compute-mark (just~premises just)))
      (setf (lab~mark node) (lab~new-label node)))
    (setf (get (caar (lab~mark node)) 'formula) node)
    node))



(defun node~p (obj)
  (declare (edited  "30-MAY-1996 09:56")
	   (authors SORGE MELIS)
	   (input   "An object.")
	   (value   "T iff obj is of type node+node."))
  (typep obj 'node+node))

(defsetf node~justification (node) (just)
  `(node=justification-medium ,node ,just))

(defgeneric node=justification-medium (node just)
  (:method (node just)
	   (error ";;;NODE~~JUSTIFICATION: wrong type of ~A." node))
  (:method ((node node+node) just)
	   (node=justification just node)))

(defgeneric node~just-premises (node)
  (declare (edited  "17-SEP-1996")
	   (authors Lassaad)
	   (input    "A proof node.")
	   (effect   "None.")
	   (value    "The premises of the justification of NODE."))
  (:method ((node node+node))
   (just~premises (node~justification node))))

(defgeneric node~just-method (node)
  (declare (edited  "17-SEP-1996")
	   (authors SORGE Lassaad)
	   (input    "A proof node.")
	   (effect   "None.")
	   (value    "The inference-method of the justification of NODE."))
  (:method ((node node+node))
   (just~method (node~justification node))))


#{For deduction system applications it is a recurring situation that the system needs a new (that does not
occur anywhere else) name for a proof node. The function {\vb node~make-new-name} dreams one up according
to the status of the system.#}

(defun node~make-new-name ()
  (declare
   (authors nesmith)
   (input "none")
   (value "A new symbol for use as a name"))
  (gensym "NODE")
  ;; fill in with some complicated way of naming nodes, including a way
  ;; to reset a counter when beginning a new proof, making sure node
  ;; names are distinct, etc.
  )

;#{\subsection{POST interface for resolution nodes} 
;
;The following functions form a peculiarity of \keim\ from the history of implementation, they may vanish in
;future versions. In the resolution system the nodes for resolution proofs and the clauses are equated
;therefore a clause has 2 \post\ representations, one as a clause, and anther as a proof step. The former can
;be obtained with the function {\vb POST~PRINT}, the latter is supplied by the following functions. #}

(defgeneric node~post-print-step (justification node stream)
  (declare (edited  "06-FEB-1993 12:45")
	   (authors RICHTS)
	   (input   "A justification, the proof node it is leading to and a stream.")
	   (effect  "The POST representation of the step leading to NODE"
                     "(consisting of JUSTIFICATION and NODEs formula)"
		    "is printed on STREAM"
                    "(e.g. (resolution step1 <resolvent> (<cl-name1> <pos1>) (<cl-name2> <pos2>)"
                                       "<subst> <renaming>)).")
	   (value   "Undefined.")))

;;; redo the following functions as soon as justifications are done..... (VS)

(defun node~post-print-steps (nodes stream)
  (declare (edited  "06-FEB-1993 14:34")
	   (authors RICHTS)
	   (input "a list of nodes and a stream"  )
	   (effect "the nodes are printed  in POST representation to the stream"))
  (when nodes
     (node~post-print-step (node~justification (first nodes)) (first nodes) stream)
     (mapc #'(lambda (node)
	       (format stream " ")
	       (node~post-print-step (node~justification node) node stream))
	   (rest nodes))))

(defmethod print-object ((node node+node) stream)
  (let ((just (node~justification node)))
    (format stream "#<~A: LABEL: ~A ~A with ~A from ~{~A~}>"
	    (type-of node)
	    (lab~mark node)
	    (node~formula node)
	    (keim~name (just~method just))
	    (keim~name (just~premises just))
	    )))

