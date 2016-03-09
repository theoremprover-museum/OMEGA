;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                           ;;
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

(mod~defmod pdsj
	    :uses (mod node just keim th pdsc)
	    :documentation "Basic datastructures and functionality of pds justifications."
	    :exports (
		      pdsj+justification
		      pdsj~above
		      pdsj~most-abstract
		      pdsj~least-abstract
		      pdsj~below
		      pdsj~status
		      pdsj~parameters
		      pdsj~subst
		      pdsj~constraints
		      pdsj~outline-pattern
		      pdsj~control
		      pdsj~create
		      pdsj~justification-p
		      pdsj~ass&premises
		      pdsj~all-premises
		      pdsj~set-below-justs
		      pdsj~set-above-justs
		      pdsj~nth-chain-justs!
		      pdsj~chain-justs!
                      pdsj~insert-just-above
                      pdsj~insert-just-below
                      pdsj~replace-justification-in-chain!
		      pdsj~replace-justification!
		      pdsj~above-justs
		      pdsj~below-justs
		      pdsj~least-below-just
		      pdsj~all-justs
		      pdsj~position-all-justs
		      pdsj~reasons
		      pdsj~sponsors   
		      pdsj~unsponsors
		      pdsj~own-reason
		      pdsj~above-own-reason
		      pdsj~below-own-reason
		      pdsj~other-reasons
		      pdsj~above-other-reasons
		      pdsj~below-other-reasons
		      pdsj~above-reasons
		      pdsj~below-reasons
		      pdsj~all-own-reasons
		      pdsj~all-other-reasons
		      pdsj~set-other-reasons!
		      pdsj~remove-reason!
		      pdsj~currently-insert-reason!
		      pdsj~insert-reason!
		      pdsj~successor
		      pdsj~predecessor
		      pdsj~successors
		      pdsj~predecessors
		      pdsj~control-connect-pred&succ!
		      pdsj~mix-ctrl!
		      pdsj~with-reason-p
		      pdsj~with-reason-level-p
		      pdsj~open-just-create
		      pdsj~closed-just-create
		      pdsj~open-just-p
		      pdsj~unexpanded-p
		      pdsj~expanded-p
		      pdsj~untested-p
		      pdsj~grounded-p
		      pdsj~open-p
		      pdsj~premise-p
		      pdsj~clsd-premise-p  
		      pdsj~substitute-premise!
		      pdsj~premise-outline
		      pdsj~conclusion-outline
		      pdsj~get-twin-just
		      pdsj~copy
		      pdsj~assertion-just-p
		      )
	    )

#{\section{Justifications}\label{mod:just}

Justifications are rather complex objects which encode the dependency of the current nodes in
the plan data structure upon eachother, as well as the inference method with which each line was grounded.
Furthermore they keep track of some necessary additional data and supplementary information.

A node is grounded by application of an inference method of type INFER+INFERENCE to the nodes supports and the
additional parameter list. Dependencies to other levels of abstraction are stored in pointers to the corresponding
justifications. To record the structure of the proof plan at the time the node is grounded, and thus know later in which
direction the inference method was applied, the provided outline pattern is stored as well.

In our example THM was grounded by DEF with the lines A1 and A2 as support. As all three lines already existed in
the proof tree, the outline pattern is the list (T T T).
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structure and functions for PDS-node justifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (load compile eval)

(defclass pdsj+justification (just+justification)
  ((method :initform (infer~find-method "Open")
	   :documentation "The inference method when the node is closed otherwise the Open-string.")
   (above :accessor pdsj~above
	  :initform nil
	  :initarg :above
	  :type pdsj+justification
	  :documentation "The justification of the node on a higher level of abstraction.")
   (below :accessor pdsj~below
	  :initform nil
	  :initarg :below
	  :type pdsj+justification
	  :documentation "The justification of the node on a lower level of abstraction.")
   (status :accessor pdsj~status
	   :initform nil
	   :initarg :status
	   :documentation "The status of the justification (open, closed, grounded, ..).")
   (parameters :accessor pdsj~parameters
	       :initform nil
	       :initarg :parameters
	       :documentation "A list of additional parameters for the method.")
   (substitutions :accessor pdsj~subst
		  :initform nil
		  :initarg :subst
		  :documentation "The substitutions of the metavariable in the method.")
   (constraints :accessor pdsj~constraints
		:initform nil
		:initarg :constraints
		:documentation "The constraint-state produced by the application of the method.")
   (outline-pattern :accessor pdsj~outline-pattern
		    :initform nil
		    :initarg :outline-pattern
		    :documentation "The outline-pattern for the method (when it was called).")
   (control :accessor pdsj~control
	    :initform nil
	    :initarg :control
	    :type pdsc+control
	    :documentation "An object storing all control informations for the justification.")
   )
  (:documentation "The justification for a proof node."))
)
 

#{\subsection{Accessor Functions}#}
(defun pdsj~most-abstract (just)
  (declare (edited  "18-NOV-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "JUST, if its above justification is NIL, otherwise the most abstract"
		    "justification of the above justification of JUST."))
  (let ((above-just (pdsj~above just)))
    (if above-just
	(pdsj~most-abstract above-just)
      just)
    ))

(defun pdsj~least-abstract (just)
  (declare (edited  "18-NOV-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "JUST, if its below justification is NIL, otherwise the least abstract"
		    "justification of the below justification of JUST."))
  (let ((below-just (pdsj~below just)))
    (if below-just
	(pdsj~least-abstract below-just)
      just)
    ))

(defun pdsj~ass&premises (just)
  (declare (edited  "24-FEB-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (value   "A list containing the premises nodes for the justification"
		    "and eventually the assertion node if JUST is an assertion"
		    "justification."))
  (let ((prems (just~premises just))
	(ass (keim~get just :assertion)))
    (if ass
	(cons ass prems)
      prems)
    ))

(defun pdsj~all-premises (just)
  (declare (edited  "17-SEP-1996" "13-MAY-1996 11:15")
	   (authors Lassaad SORGE)
	   (input   "A justification.")
	   (value   "A list of nodes containing the premises of JUST on every"
		    "abstract level."))
  (let ((prems (pdsj~ass&premises just))
	(below-prems (pdsj=all-below-premises just))
	(above-prems (pdsj=all-above-premises just)))
    (remove-duplicates (append above-prems prems below-prems))))


(defun pdsj=all-below-premises (just)
  (declare (edited  "20-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list of nodes containing the premises of its below justifications."))
  (let ((below-just (pdsj~below just)))
    (when below-just
      (remove-duplicates (append (pdsj~ass&premises below-just)
				 (pdsj=all-below-premises below-just))))))


(defun pdsj=all-above-premises (just)
  (declare (edited  "20-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list of nodes containing the premises of its above justifications."))
  (let ((above-just (pdsj~above just)))
    (when above-just
      (remove-duplicates (append (pdsj~ass&premises above-just)
				 (pdsj=all-above-premises above-just))))))



(defun pdsj~set-below-justs (most-abstr-just less-abstr-justs)
  (declare (edited  "20-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification, and a list of less abstract justifications"
		    "sorted from the most abstract one to the least abstract "
		    "justification. All these justifications should justify the"
		    "same node but at different abstraction levels.")
	   (effect  "Set the above and below slots of the MOST-ABSTR-JUST and"
		    "justifications in LESS-ABST-JUSTS.")
	   (value   "The least abstract justification."))
  (if less-abstr-justs
      (let ((abstr-just (first less-abstr-justs)))
	(setf (pdsj~below most-abstr-just) abstr-just)
	(setf (pdsj~above abstr-just) most-abstr-just)
	(pdsj~set-below-justs abstr-just (rest less-abstr-justs)))
    most-abstr-just))


(defun pdsj~set-above-justs (least-abstr-just more-abstr-justs)
  (declare (edited  "20-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification, and a list of more abstract justifications"
		    "sorted from the least abstract one to the most abstract "
		    "justification. All these justifications should justify the"
		    "same node but at different abstraction levels.")
	   (effect  "Set the above and below slots of the LEAST-ABSTR-JUST and"
		    "justifications in MORE-ABST-JUSTS.")
	   (value   "The least abstract justification."))
  (if more-abstr-justs
      (let ((abstr-just (first more-abstr-justs)))
	(setf (pdsj~below abstr-just) least-abstr-just)
	(setf (pdsj~above least-abstr-just) abstr-just)
	(pdsj~set-above-justs abstr-just (rest more-abstr-justs))
	least-abstr-just)
    least-abstr-just))


(defun pdsj~nth-chain-justs! (position justs)
  (declare (edited  "24-SEP-1997")
	   (authors Lassaad)
	   (input   "An integer, and a justification list sorted from the most"
		    "abstract one to the least abstract justification. All these"
		    "justifications should justify the same node but at different"
		    "abstraction levels.")
	   (effect  "Bind the given justifications by setting their above and below"
		    "slots.")
	   (value   "The justification on the POSITION-th abstraction level."))
  (nth position (pdsj~chain-justs! (first justs) (rest justs))))

(defun pdsj~chain-justs! (most-abstr-just less-abstr-justs)
  (declare (edited  "24-SEP-1997")
	   (authors Lassaad)
	   (input   "A justification, and a list of less abstract justifications"
		    "sorted from the most abstract one to the least abstract "
		    "justification. All these justifications should justify the"
		    "same node but at different abstraction levels.")
	   (effect  "Set the above and below slots of the MOST-ABSTR-JUST and"
		    "justifications in LESS-ABST-JUSTS.")
	   (value   "A list resulting from consing MOST-ABSTR-JUST to LESS-ABSTR-JUSTS."))
  (if less-abstr-justs
      (let* ((justs (pdsj~chain-justs! (first less-abstr-justs)
				       (rest less-abstr-justs)))
	     (next-just (first justs)))
	(setf (pdsj~above next-just) most-abstr-just)
	(setf (pdsj~below most-abstr-just) next-just)
	(cons most-abstr-just justs))
    (list most-abstr-just)))

(defun pdsj~insert-just-above (just new)
  (declare (edited  "01-DEC-1996")
	   (authors Afiedler)
	   (input   "Two justifications.")
	   (effect  "Inserts NEW above JUST in the justification chain represented by JUST.")
	   (value   "NEW."))
  (let ((orig-above (pdsj~above just)))
    (if orig-above
	(pdsj~insert-just-below orig-above new)
      (progn (setf (pdsj~above just) new)
	     (setf (pdsj~below new) just))))
  new)

(defun pdsj~insert-just-below (just new)
  (declare (edited  "01-DEC-1996")
	   (authors Afiedler)
	   (input   "Two justifications.")
	   (effect  "Inserts NEW below JUST in the justification chain represented by JUST.")
	   (value   "NEW."))
  (let ((orig-below (pdsj~below just)))
    (pdsj~set-below-justs just (if orig-below  
					(list new orig-below)
				      (list new))))
  new)


(defun pdsj~replace-justification-in-chain! (old new)
  (declare (edited  "03-DEC-1996")
	   (authors Afiedler)
	   (input   "Two justifications.")
	   (effect  "Replaces OLD by NEW in the chain of justifications represented by"
		    "OLD.")
	   (value   "NEW."))
  (let ((above (pdsj~above old))
	(below (pdsj~below old)))
    (when above
      (setf (pdsj~above new) above)
      (setf (pdsj~below above) new))
    (when below
      (setf (pdsj~below new) below)
      (setf (pdsj~above below) new)))
  new)


(defun pdsj~replace-justification! (old new)
  (declare (edited  "16-JUN-1997")
	   (authors Lassaad)
	   (input   "Two justifications.")
	   (effect  "Replaces OLD by NEW in the chain of justifications represented by"
		    "OLD as less abstract one.")
	   (value   "NEW."))
  (let ((above (pdsj~above old)))
    (when above
      (setf (pdsj~above new) above)
      (setf (pdsj~below above) new))
    new))


(defun pdsj~above-justs (just)
  (declare (edited  "24-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list of the above justifications of JUST."))
  (let ((above-just (pdsj~above just)))
    (when above-just
      (append (pdsj~above-justs above-just) (list above-just)))
    ))


(defun pdsj~below-justs (just)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list of the below justifications of JUST."))
  (let ((below-just (pdsj~below just)))
    (when below-just
	(cons below-just (pdsj~below-justs below-just)))
    ))

(defun pdsj~least-below-just (just)
  (declare (edited  "02-JUL-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The least below justification of JUST."))
  (let ((below-just (pdsj~below just)))
    (when below-just
	(or (pdsj~least-below-just below-just)
	    below-just))
    ))

(defun pdsj~all-justs (just)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list of all justifications associated with JUST (in order) from"
		    "the most to the least abstract one."))
  (append (pdsj~above-justs just)
	  (cons just (pdsj~below-justs just))))


(defun pdsj~position-all-justs (just)
  (declare (edited  "23-SEP-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A list, its first member is a positive integer, and its"
		    "rest consists of all justifications associated with JUST"
		    "(in order) from the most to the least abstract one. The"
		    "positive integer corresponds to the position of JUST in"
		    "the rest of the list."))
  (let ((all-justs (pdsj~all-justs just)))
    (cons (position just all-justs) all-justs)))


(defun pdsj~reasons (just)
  (declare (edited  "24-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The reasons of JUST."))
  (let ((just-ctrl (pdsj~control just)))
    (when just-ctrl
      (pdsc~reasons just-ctrl))
    ))

(defsetf pdsj~reasons (just) (reasons)
  (declare (edited  "10-JUN-1997")
	   (authors Sorge Lassaad)
	   (input   "A justification and a list of reasons.")
	   (effect  "The reason slot of the control-structure of JUST is set to the list.")
	   (value   "Undefined."))
  `(if (pdsj~justification-p ,just)
       (let ((ctrl (pdsj~control ,just)))
	 (if ctrl
	     (setf (pdsc~reasons ctrl) ,reasons)
	   (setf (pdsj~control ,just)
		 (pdsc~create ,reasons NIL NIL))))
     (error ";;;PDSJ~~REASONS: ~A is not of type PDSJ+JUSTIFICATION." ,just)))

(defun pdsj~sponsors (just)
  (declare (edited  "24-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The sponsors of JUST."))
  (let ((control (pdsj~control just)))
    (when control
      (pdsc~sponsors control))))

(defun pdsj~unsponsors (just)
  (declare (edited  "24-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The unsponsors of JUST."))
  (let ((control (pdsj~control just)))
    (when control
      (pdsc~unsponsors control))))

(defsetf pdsj~sponsors (just) (nodes)
  (declare (edited  "18-NOV-1996")
	   (authors Lassaad)
	   (input   "A justification, and a node list.")
	   (effect  "Sets the sponsors of JUST to NODES.")
	   (value   "Undefined."))
  `(if (pdsj~justification-p ,just)
       (let ((ctrl (pdsj~control ,just)))
	 (if ctrl
	     (setf (pdsc~sponsors ctrl) ,nodes)
	   (setf (pdsj~control ,just)
		 (pdsc~create NIL ,nodes NIL))))
     (error ";;;SETF PDSJ~~SPONSORS: ~A must be of type PDSJ+JUSTIFICATION." ,just)))


(defsetf pdsj~unsponsors (just) (nodes)
  (declare (edited  "18-NOV-1996")
	   (authors Lassaad)
	   (input   "A justification, and a node list.")
	   (effect  "Sets the unsponsors of JUST to NODES.")
	   (value   "Undefined."))
  `(if (pdsj~justification-p ,just)
       (let ((ctrl (pdsj~control ,just)))
	 (if ctrl
	     (setf (pdsc~unsponsors ctrl) ,nodes)
	   (setf (pdsj~control ,just)
		 (pdsc~create NIL NIL ,nodes))))
     (error ";;;SETF PDSJ~~SPONSORS: ~A must be of type PDSJ+JUSTIFICATION." ,just)))


(defun pdsj~own-reason (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The own reason of JUST, i.e. the reason of JUST with JUST"
		    "as justification."))
  (find-if #'(lambda (r)
	       (equal just (pdsc~an-just r)))
	   (pdsj~reasons just)))

(defun pdsj~above-own-reason (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next own reason of an above justification of JUST."))
  (let ((above-just (pdsj~above just)))
    (when above-just
      (or (pdsj~own-reason above-just)
	  (pdsj~above-own-reason above-just)))
    ))

(defun pdsj~below-own-reason (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next own reason of a below justification of JUST."))
  (let ((below-just (pdsj~below just)))
    (when below-just
      (or (pdsj~own-reason below-just)
	  (pdsj~below-own-reason below-just)))
    ))


(defun pdsj~other-reasons (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The non-own reasons of JUST, i.e. reasons of JUST whose justifications"
		    "dont correspond to JUST."))
  (remove-if #'(lambda (r)
	         (equal just (pdsc~an-just r)))
	     (pdsj~reasons just)))

(defun pdsj~above-other-reasons (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next nonempty non-own reason list of an above justification of JUST."))
  (let ((above-just (pdsj~above just)))
    (when above-just
      (or (pdsj~other-reasons above-just)
	  (pdsj~above-other-reasons above-just)))
    ))


(defun pdsj~below-other-reasons (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next nonempty non-own reason list of a below justification of JUST."))
  (let ((below-just (pdsj~below just)))
    (when below-just
      (or (pdsj~other-reasons below-just)
	  (pdsj~below-other-reasons below-just)))
    ))


(defun pdsj~above-reasons (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next nonempty reason list of an above justification of JUST."))
  (let ((above-just (pdsj~above just)))
    (when above-just
      (or (pdsj~reasons above-just)
	  (pdsj~above-reasons above-just)))
    ))

(defun pdsj~below-reasons (just)
  (declare (edited  "15-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The next nonempty reason list of a below justification of JUST."))
  (let ((below-just (pdsj~below just)))
    (when below-just
      (or (pdsj~reasons below-just)
	  (pdsj~below-reasons below-just)))
    ))


(defun pdsj~all-own-reasons (just)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The own reasons of JUST, its above justifications, and"
		    "of its below justifications."))
  (apply #'append (mapcar #'(lambda (j)
			      (let ((own-r (pdsj~own-reason j)))
				(when own-r (list own-r))))
			  (pdsj~all-justs just))
	 ))


(defun pdsj~all-other-reasons (just)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The non-own reasons of JUST, its above justifications, and"
		    "of its below justifications."))
  (apply #'append (mapcar #'pdsj~other-reasons (pdsj~all-justs just))
	 ))

(defun pdsj~set-other-reasons! (just reasons)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a reason list.")
	   (effect  "Sets the non-own reasons of JUST to REASONS.")
	   (value   "Unspecified."))
  (let ((own-reason (pdsj~own-reason just)))
    (setf (pdsj~reasons just)
	  (if own-reason
	      (cons own-reason reasons)
	    reasons))
    ))


(defun pdsj~remove-reason! (just reason)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a reason.")
	   (effect  "Deletes REASON from the reasons of JUST, its above justifications,"
		    "and its below justifications.")
	   (value   "Unspecified."))
  (dolist (j (pdsj~all-justs just))
    (setf (pdsj~reasons j) (remove reason (pdsj~reasons j)))
    ))


(defun pdsj~currently-insert-reason! (just reason)
  (declare (edited  "22-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a reason.")
	   (effect  "Inserts REASON in the reason list of JUST and removes"
		    "it from the reasons of JUST above and below justifications.")
	   (value   "Unspecified."))
  (pdsj~remove-reason! just reason)
  (pdsj~insert-reason! just reason))


(defun pdsj~insert-reason! (just reason)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a reason.")
	   (effect  "Inserts REASON in the JUST control.")
	   (value   "Unspecified."))
  (let ((control (pdsj~control just)))
    (if control
	(unless (find reason (pdsc~reasons control))
	  (setf (pdsc~reasons control) (cons reason (pdsc~reasons control))))
      (setf (pdsj~control just)
	    (pdsc~create (list reason) NIL NIL)))
    ))


(defun pdsj~successor (just)
  (declare (edited  "24-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The JUST successor."))
  (let ((just-ctrl (pdsj~control just)))
    (when just-ctrl
      (pdsc~successor just-ctrl))
    ))

(defsetf pdsj~successor (just) (anode)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and an actual node.")
	   (effect  "Set the successor of JUST to ANODE.")
	   (value   "Unspecified."))
  `(if (pdsj~justification-p ,just)
       (let ((ctrl (pdsj~control ,just)))
	 (if ctrl
	     (setf (pdsc~successor ctrl) ,anode)
	   (setf (pdsj~control ,just)
		 (pdsc~create NIL NIL NIL NIL NIL NIL ,anode)))
	 )
     (error ";;; SETF PDSJ~~SUCCESSOR: ~A must be of type PDSJ+JUSTIFICATION." ,just)
     ))


(defun pdsj~predecessor (just)
  (declare (edited  "24-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The JUST predecessor."))
  (let ((just-ctrl (pdsj~control just)))
    (when just-ctrl
      (pdsc~predecessor just-ctrl))
    ))

(defsetf pdsj~predecessor (just) (anode)
  (declare (edited  "20-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and an actual node.")
	   (effect  "Set the predecessor of JUST to ANODE.")
	   (value   "Unspecified."))
  `(if (pdsj~justification-p ,just)
       (let ((ctrl (pdsj~control ,just)))
	 (if ctrl
	     (setf (pdsc~predecessor ctrl) ,anode)
	   (setf (pdsj~control ,just)
		 (pdsc~create NIL NIL NIL NIL NIL NIL NIL ,anode)))
	 )
     (error ";;; SETF PDSJ~~PREDECESSOR: ~A must be of type PDSJ+JUSTIFICATION." ,just)
     ))


(defun pdsj~successors (just)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The successor list of JUST."))
  (let ((just-succ (pdsj~successor just)))
    (when just-succ
      (cons just-succ (pdsj~successors (pdsc~an-just just-succ))))
    ))

(defun pdsj~predecessors (just)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The predecessor list of JUST."))
  (let ((just-pred (pdsj~predecessor just)))
    (when just-pred
      (cons just-pred (pdsj~predecessors (pdsc~an-just just-pred))))
    ))

(defun pdsj~control-connect-pred&succ! (control)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A control.")
	   (effect  "Connect the predecessor and successor of CONTROL, i.e. the"
		    "successor of its predecessor becomes its successor and vice"
		    "versa. The predecessor and successor of CONTROL are finally"
		    "set to NIL.")
	   (value   "Unspecified."))
  (let ((ctrl-pred (pdsc~predecessor control))
	(ctrl-succ (pdsc~successor control)))
    (when ctrl-pred
      (setf (pdsj~successor (pdsc~an-just ctrl-pred)) ctrl-succ)
      (setf (pdsc~predecessor control) NIL))
    (when ctrl-succ
      (setf (pdsj~predecessor (pdsc~an-just ctrl-succ)) ctrl-pred)
      (setf (pdsc~successor control) NIL))
    ))


(defun pdsj~mix-ctrl! (just ctrl)
  (declare (edited  "03-NOV-1997")
	   (authors Lassaad)
	   (input   "A justification, and a control")
	   (effect  "Extends the control of JUST with the control information"
		    "in CTRL.")
	   (value   "Unspecified."))
  (when ctrl
    (let ((just-ctrl (pdsj~control just)))
      (if just-ctrl
	  (let ((ctrl-rs (pdsc~reasons ctrl))
		(ctrl-sps (pdsc~sponsors ctrl))
		(ctrl-unsps (pdsc~unsponsors ctrl)))
	    (when ctrl-rs
	      (setf (pdsc~reasons just-ctrl) (union (pdsc~reasons just-ctrl) ctrl-rs)))
	    (when ctrl-sps
	      (setf (pdsc~sponsors just-ctrl) (union (pdsc~sponsors just-ctrl) ctrl-sps)))
	    (when ctrl-unsps
	      (setf (pdsc~unsponsors just-ctrl) (union (pdsc~unsponsors just-ctrl) ctrl-unsps))))
	(setf (pdsj~control just) ctrl)))
    ))
    

(defun pdsj~with-reason-p (just reason &key ((:above-only above-only)) ((:below-only below-only)))
  (declare (edited  "16-MAY-1997" "6-OCT-1999")
	   (authors Lassaad cullrich)
	   (input   "A justification, a reason and two keywords that indicate in which"
		    "direction to search.")
	   (effect  "None.")
	   (value   "JUST, an above, or a below justification of it, iff REASON"
		    "belongs to the reason list of this justification. Otherwise NIL"))
  ;; the keywords are needed in order to terminate the function when the give reason is not in
  ;; the reasons of the node.
  (when just
    (if (find reason (pdsj~reasons just))
	just
      (or (and (not below-only)
	       (pdsj~with-reason-p (pdsj~above just) reason :above-only t))
	  (and (not above-only)
	       (pdsj~with-reason-p (pdsj~below just) reason :below-only t))))
    ))

(defun pdsj~with-reason-level-p (just reason)
  (declare (edited  "28-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "A dotted pair that consists of JUST, an above, or a below"
		    "justification of it together with a number denoting its"
		    "abstraction level, iff REASON belongs to the reason list"
		    "of this justification. Otherwise, NIL."))
  (let* ((justs (pdsj~all-justs just))
	 (pos (position-if #'(lambda (j) (find reason (pdsj~reasons j)))
			   justs)))
    (when pos
      (cons (nth pos justs) pos))
    ))

#{\subsection{Special Justifications}#}
#{\subsubsection{Constructors for Special Justifications}#}

(defun pdsj~open-just-create (&optional ctrl) 
  (declare (edited  "25-MAY-1997" "18-SEP-1996")
	   (authors Lassaad)
	   (input   "Optionally a control object.")
	   (effect  "A new justification with an open status and with the given"
		    "control is created.")
	   (value   "The new justification."))
  (make-instance 'pdsj+justification
		 :method (infer~find-method "open")
		 :status "open"
		 :control ctrl))

(defun pdsj~closed-just-create (method premises parameters &optional (status "unexpanded")
				       substitutions outline-pattern constraints)
  (declare (edited  "23-JAN-1998" "18-SEP-1996")
	   (authors Jzimmer Lassaad)
	   (input   "An inference method, a list of proof nodes, a list of parameters, a list"
		    "of substitutions, and a list for the outline-pattern.")
	   (effect  "A new justification with closed status is created.")
	   (value   "The new justification."))
  (let ((newobj (make-instance 'pdsj+justification
			       :method method
			       :status status
			       :parameters parameters
			       :subst substitutions
			       :constraints constraints
			       :outline-pattern outline-pattern)))
    (when premises
      (setf (just~premises newobj) premises))
    newobj))


#{\subsubsection{Query Functions for Special Justifications}#}

(defmethod pdsj~open-just-p ((just pdsj+justification))
  (infer~open-p (just~method just)))

(defmethod pdsj~unexpanded-p ((just pdsj+justification))
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "T, iff JUST is not yet expanded, i.e. with the status unexpanded."))
  (string-equal (pdsj~status just) "unexpanded"))

(defmethod pdsj~expanded-p ((just pdsj+justification))
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "T, iff JUST is not yet expanded, i.e. with the status expanded."))
  (string-equal (pdsj~status just) "expanded"))

(defmethod pdsj~untested-p ((just pdsj+justification))
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "T, iff JUST is not yet expanded, i.e. with the status untested."))
  (string-equal (pdsj~status just) "untested"))

(defmethod pdsj~grounded-p ((just pdsj+justification))
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "T, iff JUST is not yet expanded, i.e. with the status grounded."))
  (string-equal (pdsj~status just) "grounded"))

(defmethod pdsj~open-p ((just pdsj+justification))
  (declare (edited  "28-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "T, iff JUST is not yet expanded, i.e. with the status open."))
  (string-equal (pdsj~status just) "open"))

(defgeneric pdsj~assertion-just-p (obj)
  (declare (edited  "13-NOV-1997")
	   (authors Afiedler)
	   (input   "An object.")
	   (value   "T, iff OBJ is an assertion level justification."))
  (:method (obj) ; method for pdsj+justification in al-justification.lisp, where
	   (declare (ignore obj)) ; assertion level justifications are defined.
	   nil)) 

(defun pdsj~premise-p (just node)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a node.")
	   (effect  "None.")
	   (value   "NODE, iff it belongs to the premises of JUST."
		    "Otherwise, NIL."))
  (find node (just~premises just)))

(defun pdsj~clsd-premise-p (just node)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification ,and a node.")
	   (effect  "None.")
	   (value   "T, iff NODE is a premise of JUST that has to be closed."))
  (infer~closed-pattern-p (pdsj~premise-outline just node)))

(defun pdsj~substitute-premise! (just from-prem to-prem)
  (declare (edited  "23-MAY-1997" "23-SEP-1996")
	   (authors Lassaad)
	   (input   "A justification and two nodes.")
	   (effect  "Replaces in premises of JUST, of its above justification, and of its below"
		    "justifications the node FROM-PREM with the node TO-PREM.")
	   (value   "The resulted justification."))
  (let ((above-just (pdsj~above just))
	(below-just (pdsj~below just)))
    (pdsj=substitute-premise! just from-prem to-prem)
    (when above-just
      (pdsj=above-just-substitute-premise! above-just from-prem to-prem))
    (when below-just
      (pdsj=below-just-substitute-premise! below-just from-prem to-prem))
    just))

(defun pdsj=substitute-premise! (just from-prem to-prem)
  (declare (edited  "23-MAY-1997" "07-NOV-1996")
	   (authors Lassaad)
	   (input   "A justification, and two nodes.")
	   (effect  "Replaces in premises of JUST FROM-PREM with TO-PREM.")
	   (value   "Unspecified."))
  (nsubstitute to-prem from-prem (just~premises just))
  (let ((ass (keim~get just :assertion)))
    (when (and ass (equal ass from-prem))
      (keim~put just :assertion to-prem)
      (setf (just~method just) (string (keim~name to-prem))))
    ))
	       

(defun pdsj=above-just-substitute-premise! (just from-prem to-prem)
  (declare (edited  "23-MAY-1997" "07-NOV-1996")
	   (authors Lassaad )
	   (input   "A justification, and two nodes.")
	   (effect  "Replaces in premises of JUST, and of its above justifications"
		    "FROM-PREM with TO-PREM.")
	   (value   "Unspecified."))
  (pdsj=substitute-premise! just from-prem to-prem)
  (let ((above-just (pdsj~above just)))
    (when above-just
      (pdsj=above-just-substitute-premise! above-just from-prem to-prem))
    ))

(defun pdsj=below-just-substitute-premise! (just from-prem to-prem)
  (declare (edited  "23-MAY-1997" "07-NOV-1996")
	   (authors Lassaad)
	   (input   "A justification, and two nodes.")
	   (effect  "Replaces in premises of JUST, and of its below justifications"
		    "FROM-PREM with TO-PREM.")
	   (value   "Unspecified."))
  (pdsj=substitute-premise! just from-prem to-prem)
  (let ((below-just (pdsj~below just)))
    (when below-just
      (pdsj=below-just-substitute-premise! below-just from-prem to-prem))
    ))

;;;Semantic of outlines:
;;The outline of the conclusion:
;; - "NONEXISTENT" means the conclusion was justified by a forward method
;; - "EXISTENT"   means the conclusion was justified by a backward method
;; - NODE-LABEL means the conclusion was justified together with the node NODE-LABEL 
;;The outline of a premise:
;; - "NONEXISTENT" : The premise was generated by a method application as a subgoal
;; - "EXISTENT"   : The node was used by the method application as a premise and it
;;       does not matter if it is open. Therefore this node may be open.
;; - "CLOSED"   : The node was used by the method application as a premise that was
;;       and must remain closed after the method application.

(defun pdsj~premise-outline (just node)
  (declare (edited  "23-MAY-1997")
	   (authors Lassaad)
	   (input   "A justification, and a node.")
	   (effect  "None.")
	   (value   "The outline entry of NODE, if it is a premise of JUST;"
		    "Otherwise, NIL."))
  (let* ((node-list (just~premises just))
	 (outline-pattern (pdsj~outline-pattern just))
	 (node-pos (position node node-list)))
    (when node-pos
      (nth (+ node-pos
	      (- (length outline-pattern) (length node-list)))
	   outline-pattern))))


(defun pdsj~conclusion-outline (just)
  (declare (edited  "23-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification.")
	   (effect  "None.")
	   (value   "The outline entry of the conclusion in the"
		    "outline-pattern of JUST, i.e. the first EXISTENT,"
		    "NONEXISTENT in the list."))
  (find-if #'(lambda (oln) (or (infer~existent-pattern-p oln)
			       (infer~nonexistent-pattern-p oln)))
	   (pdsj~outline-pattern just)))

(defun pdsj~get-twin-just (just inference twin-outln-pat)
  (declare (edited  "29-JUN-1997")
	   (authors Lassaad)
	   (input   "A justification, an inference, and an outline pattern"
		    "of a twin justification of JUST, one of its above, or"
		    "one of its below justifications, i.e. created with the"
		    "same tactic or method application.")
	   (effect  "None.")
	   (value   "The twin justification: JUST, one of its below, or one"
		    "of its above justificatons."))
  (let* ((justs (pdsj~all-justs just))
	 (infer-justs (remove-if-not #'(lambda (j) (equal inference
							  (just~method j)))
				     justs)))
    (if infer-justs
	(if (rest infer-justs)
	    (find-if #'(lambda (j)
			 (let ((j-outln (pdsj~outline-pattern j)))
			   (and (= (length j-outln) (length twin-outln-pat))
				(let* ((pos1 (position-if #'(lambda (oln)
							      (or (infer~existent-pattern-p oln)
								  (infer~nonexistent-pattern-p oln)))
							  j-outln))
				       (pos2 (position-if #'(lambda (oln)
							      (or (infer~existent-pattern-p oln)
								  (infer~nonexistent-pattern-p oln)))
							  twin-outln-pat)))
				  (if (zerop pos1)
				      (if (zerop pos2)
					  (equal (rest twin-outln-pat)
						 (rest j-outln))
					(and (equal (subseq twin-outln-pat 1 pos2)
						    (subseq j-outln 1 pos2))
					     (equal (subseq twin-outln-pat (+ pos2 1))
						    (subseq j-outln (+ pos2 1)))))
				    (if (zerop pos2)
					(and (equal (subseq twin-outln-pat 1 pos1)
						    (subseq j-outln 1 pos1))
					     (equal (subseq twin-outln-pat (+ pos1 1))
						    (subseq j-outln (+ pos1 1))))
				      (if (< pos1 pos2)
					  (and (equal (subseq twin-outln-pat 0 pos1)
						      (subseq j-outln 0 pos1))
					       (equal (subseq twin-outln-pat (+ pos1 1) pos2)
						      (subseq j-outln (+ pos1 1) pos2))
					       (equal (subseq twin-outln-pat (+ pos2 1))
						      (subseq j-outln (+ pos2 1))))
					(if (< pos2 pos1)
					    (and (equal (subseq twin-outln-pat 0 pos2)
							(subseq j-outln 0 pos2))
						 (equal (subseq twin-outln-pat (+ pos2 1) pos1)
							(subseq j-outln (+ pos2 1) pos1))
						 (equal (subseq twin-outln-pat (+ pos1 1))
							(subseq j-outln (+ pos1 1))))
					  (and (equal (subseq twin-outln-pat 0 pos1)
						      (subseq j-outln 0 pos1))
					       (equal (subseq twin-outln-pat (+ pos1 1))
						      (subseq j-outln (+ pos1 1))))))))))))
		     infer-justs)
	  (first infer-justs))
      (error ";;;PDSJ~~GET-TWIN-JUST: The justification ~A does not use the inference method ~A"
	     just inference))
    ))

					






						      
	   
				


#{\subsection{Constructor and Query}#}


(defun pdsj~create (method premises above below status parameters substitutions
			   outline-pattern control)
  (declare (edited  "17-SEP-1996" "07-MAY-1996 10:30")
	   (authors Lassaad SORGE)
	   (input   "An inference method, a list of proof nodes, more and less abstract justifications,"
		    "a keyword string, a list of parameters, a list of substitutions, and a list for the"
		    "outline-pattern.")
	   (effect  "A new justification is created.")
	   (value   "The new justification."))
  (let ((newobj (make-instance 'pdsj+justification
			       :method method
			       :above above
			       :below below
			       :status status
			       :parameters parameters
			       :subst substitutions
			       :outline-pattern outline-pattern
			       :control control)))
    (when premises
	  (setf (just~premises newobj) premises))
    newobj)) 

(defun pdsj~justification-p (thing)
  (declare (edited  "18-SEP-1996" "12-APR-1996")
	   (authors Lassaad Lassaad)
	   (input   "Any THING.")
	   (effect  "None")
	   (value   "T if THING is of type PDSJ+JUSTIFICATION, else NIL."))
  (typep thing 'pdsj+justification))



(defmethod keim~equal ((just1 pdsj+justification) (just2 pdsj+justification))
  (and (keim~equal (just~method just1) (just~method just2))
       (keim~equal (just~premises just1) (just~premises just2))
       (keim~equal (pdsj~parameters just1) (pdsj~parameters just2))
       (string-equal (pdsj~status just1) (pdsj~status just2))))

(defmethod print-object ((just pdsj+justification) stream)
  (let ((prems (mapcar #'keim~name (just~premises just))))
    (if prems
	(format stream "#<Justified by ~A from ~A>" (keim~name (just~method just)) prems)
      (format stream "#<Justified by ~A>" (keim~name (just~method just))))))

(defun pdsj~copy (just)
  (declare (edited  "18-SEP-1996")
           (authors Lassaad)
           (input   "A justification.")
           (effect  "None.")
           (value   "A copy of the justification JUST."))
  (let ((new-just (with-slots (method premises above below status parameters
			       substitutions outline-pattern control)
			      just
			      (pdsj~create method
				           (copy-list premises)
					   above
					   below
					   status
					   (copy-list parameters)
					   substitutions
				           outline-pattern
					   (when control
					     (pdsc~copy control)))))
	(ass (keim~get just :assertion)))
    (when ass (keim~put new-just :assertion ass))
    new-just
    ))



