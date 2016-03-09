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


(mod~defmod PDSN 
            :uses (data infer just keim meta node pdsc pdsj term th)
            :documentation "Basic datastructures and functionality of proof plan nodes."
            :exports (
                      pdsn+node
                      pdsn+schematic-node
                      
                      pdsn~all-justs
                      pdsn~alternative-methods
                      pdsn~alternative-mmatchings
                      pdsn~applied-critics
                      pdsn~copy
                      pdsn~create
                      pdsn~current-formula
                      pdsn~depending-nodes
                      pdsn~equal-node
                      pdsn~excluded-methods
                      pdsn~failed-methods
                      pdsn~grounded-p
                      pdsn~hyp-in-but-not-in-p
                      pdsn~hypothesis-node-p
                      pdsn~hypothesis-p
                      pdsn~hyps
                      pdsn~insert-reason!
                      pdsn~just-all-premises
                      pdsn~just-ass&premises
                      pdsn~just-method
                      pdsn~just-parameters
                      pdsn~just-premises
                      pdsn~just-sponsors
                      pdsn~just-status
                      pdsn~just-unsponsors
                      pdsn~justifying-nodes
                      pdsn~least-abstract-just
                      pdsn~list-p
                      pdsn~local-definition-p
                      pdsn~make-hypothesis
                      pdsn~most-abstract-just
                      pdsn~nth-just
                      pdsn~open-node-create
                      pdsn~open-node-p
                      pdsn~p
                      pdsn~replace-just!
                      pdsn~schematic-p
                      pdsn~superset-of-without-hyp-p
                      pdsn~th-assumption-p
                      pdsn~unexpanded-p
                      pdsn~untested-p
                      pdsn~up-to-date
                      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structure and functions of a PDS node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(eval-when (load compile eval) 
(defclass pdsn+node (node+node)
  ((hyps :reader pdsn~hyps
	 :writer pdsn=hyps
	 :initarg :hyps
	 :initform nil
	 :documentation "Hypotheses upon which the node depends.")
   (justification :type pdsj+justification
		  :initform nil
	          :documentation "The justification of the node."))
  (:documentation "A node in a proof plan."))

(defclass pdsn+schematic-node (pdsn+node)
  ((current-formula :initform nil
		    :accessor pdsn~current-formula
		    :documentation "An instance for the formula of the schematic node.")
   (up-to-date :initform nil
	       :accessor pdsn~up-to-date
	       :documentation "A flag stating whether the content of CURRENT-FORMULA is up-to-date.")
   )
  (:documentation "A node in a proof plan, whose formula contains meta-variables."))
)

(defmethod pdsn~current-formula ((node pdsn+node)) (node~formula node))

(defmethod print-object ((node pdsn+schematic-node) stream)
  (format stream 
	  (if *print-escape* "#<pdsn+schematic-node ~S>" "~S")
	  (keim~name node)))

#{\subsection{Constructors and Query Functions}#}

(defun pdsn~create (label hyps wff just)
  (declare (edited  "14-APR-1999"  "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A label, a list of hypotheses (other nodes), a formula and a justification.")
           (effect  "None.")
           (value   "A new node created with input values."))
  (if (some #'meta~p (data~free-variables wff))
      (make-instance 'pdsn+schematic-node
		     :name label
		     :hyps hyps
		     :formula wff
		     :justification just)
    (make-instance 'pdsn+node
		   :name label
		   :hyps hyps
		   :formula wff
		   :justification just))
    )



(defun pdsn~copy (node)
  (declare (edited  "18-SEP-1996" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A node.")
           (effect  "None.")
           (value   "A copy of the node."))
  (with-slots (name hyps formula justification control) node
	      (pdsn~create name
			   hyps
			   (data~copy formula) 
			   (pdsj~copy justification)
			   )))


(defun pdsn~p (thing)
  (declare (edited  "18-SEP-96")
           (authors Lassaad)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type PDSN+NODE, else NIL."))
  (typep thing 'pdsn+node))

(defun pdsn~schematic-p (thing)
  (declare (edited  "14-APR-1999")
           (authors Lassaad)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type PDSN+SCHEMATIC-NODE, else NIL."))
  (typep thing 'pdsn+schematic-node))

(defun pdsn~list-p (node-list)
  (declare (edited  "22-OCT-1996" "15-OCT-1996")
	   (authors Lassaad Jzimmer)
	   (input   "An object.")
	   (effect  )
	   (value   "T, if NODE-LIST is a list of PDSN-NODEs, otherwise NIL."))
  (every #'pdsn~p node-list))

(defgeneric pdsn~equal-node (node pdsn)
  (declare (edited  "12-JUN-1997")
	   (authors Sorge)
	   (input   "A node and a pds-node.")
	   (effect  "None.")
	   (value   "T if label and formula of NODE is equal to PDSN."))
  (:method ((pdsn pdsn+node) (node node+node))
	   (pdsn~equal-node node pdsn))
  (:method ((node node+node) (pdsn pdsn+node))
	   (and (equal (keim~name node) (keim~name pdsn))
		(keim~equal (node~formula node) (node~formula pdsn)))))
  
#{\subsection{Accessor Functions}#}

(defsetf pdsn~hyps (node) (hyps)
  `(pdsn=hyps-medium ,node ,hyps))

(defgeneric pdsn=hyps-medium (node hyps)  ;;;; necessary for later....
  (:method ((node pdsn+node) hyps)
	   (pdsn=hyps hyps node))
  (:method (node hyps)
	   (declare (ignore hyps))
	   (error ";;;PDSN~~HYPS: Wrong type of ~A." node)))

(defun pdsn~replace-just! (node just)
  (declare (edited  "03-NOV-1997")
	   (authors Lassaad)
	   (input   "A node, and a justification.")
	   (effect  "Substituting the least abstract justification of NODE by JUST.")
	   (value   "NODE with the new justification JUST."))
  (let* ((node-just (node~justification node))
	 (above-just (pdsj~above node-just)))
    (when above-just
      (setf (pdsj~below above-just) just
	    (pdsj~above just) above-just))
    (pdsj~mix-ctrl! just (pdsj~control node-just))
    (setf (node~justification node) just)
    node))

(defun pdsn~most-abstract-just (node)
  (declare (edited  "13-FEB-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The justification of NODE on the most abstract level."))
  (pdsj~most-abstract (node~justification node)))

(defun pdsn~least-abstract-just (node)
  (declare (edited  "04-JUL-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The justification of NODE on the least abstract level."))
  (pdsj~least-abstract (node~justification node)))

(defun pdsn~nth-just (node number)
  (declare (edited  "30-MAY-1997")
	   (authors Lassaad)
	   (input   "A node, and an integer.")
	   (effect  "None.")
	   (value   "The NUMBER-th less abstract justification of NODE."))
  (nth number (pdsn~all-justs node)))


(defun pdsn~all-justs (node)
  (declare (edited  "19-MAY-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The justifications of NODE (in order) from the most to the"
		    "least abstract one."))
  (pdsj~all-justs (node~justification node)))

#{\subsubsection{Shortcuts for Justifications and Controls}#}

(defun pdsn~just-method (node)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "The justification method of NODE."))
  (let ((just (node~justification node)))
    (when just
      (just~method just))))


(defun pdsn~just-parameters (node)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "The parameters in the justification of NODE."))
  (let ((just (node~justification node)))
    (when just
      (pdsj~parameters just))))
  

(defun pdsn~just-premises (node)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "The premises which justifies NODE."))
  (let ((just (node~justification node)))
    (when just
      (just~premises just))))


(defun pdsn~just-ass&premises (node)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "The premises which justifies NODE and eventually the"
		    "assertion node, if the justification of NODE is an"
		    "assertion justification."))
  (let ((just (node~justification node)))
    (when just
      (pdsj~ass&premises just))))
  

(defun pdsn~just-all-premises (node) 
  (declare (edited  "20-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "A list of nodes containing the premises of NODE on every"
		    "abstract level."))
  (let ((just (node~justification node)))
    (when just
      (pdsj~all-premises just))))


(defun pdsn~just-status (node)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad)
	   (input   "A proof plan node.")
	   (effect  "None.")
	   (value   "The justification status of NODE."))
  (let ((just (node~justification node)))
    (when just
      (pdsj~status just))))


(defun pdsn~just-sponsors (node)
  (declare (edited  "10-OCT-1996")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None")
	   (value   "The sponsor nodes of NODE."))
  (pdsj~sponsors (node~justification node)))


(defun pdsn~just-unsponsors (node)
  (declare (edited  "10-OCT-1996")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None")
	   (value   "The unsponsor nodes of NODE."))
  (pdsj~unsponsors (node~justification node)))


(defsetf pdsn~just-sponsors (node) (nodes)
  (declare (edited  "18-NOV-1996")
	   (authors Lassaad)
	   (input   "A node, and a node list.")
	   (effect  "Sets the sponsors of NODE to NODES.")
	   (value   "Undefined."))
  `(if (pdsn~p ,node)
       (if (pdsn~list-p ,nodes)
	   (setf (pdsj~sponsors (node~justification ,node)) ,nodes)
	 (error ";;;SETF PDSN~~SPONSORS: Each element of ~A must be of type PDSN+NODE." ,nodes))
     (error ";;;SETF PDSN~~SPONSORS: ~A must be of type PDSN+NODE." ,node)))


(defsetf pdsn~just-unsponsors (node) (nodes)
  (declare (edited  "18-NOV-1996")
	   (authors Lassaad)
	   (input   "A node, and a node list.")
	   (effect  "Sets the unsponsors of NODE to NODES.")
	   (value   "Undefined."))
  `(if (pdsn~p ,node)
       (if (pdsn~list-p ,nodes)
	   (setf (pdsj~unsponsors (node~justification ,node)) ,nodes)
	 (error ";;;SETF PDSN~~UNSPONSORS: Each element of ~A must be of type PDSN+NODE." ,nodes))
     (error ";;;SETF PDSN~~UNSPONSORS: ~A must be of type PDSN+NODE." ,node)))

(defun pdsn~applied-critics (node)
  (pdsc~applied-critics (pdsj~control (node~justification node))))

(defsetf pdsn~applied-critics (node) (alters)
  `(if (pdsn~p ,node)
       (let* ((node-just (node~justification ,node))
	      (ctrl (pdsj~control node-just)))
	 (if ctrl
	     (setf (pdsc~applied-critics ctrl) ,alters)
	   (setf (pdsj~control node-just)
		 (pdsc~create nil nil nil ,alters))))
     ))

(defun pdsn~alternative-methods (node)
  (pdsc~alternative-methods (pdsj~control (node~justification node))))

(defsetf pdsn~alternative-methods (node) (alters)
  `(if (pdsn~p ,node)
       (let* ((node-just (node~justification ,node))
              (ctrl (pdsj~control node-just)))
         (if ctrl
             (setf (pdsc~alternative-methods ctrl) ,alters)
           (setf (pdsj~control node-just)
                 (pdsc~create nil nil nil ,alters))))
     ))

(defun pdsn~alternative-mmatchings (node)
  (pdsc~alternative-mmatchings (pdsj~control (node~justification node))))

(defsetf pdsn~alternative-mmatchings (node) (alters)
  `(if (pdsn~p ,node)
       (let* ((node-just (node~justification ,node))
              (ctrl (pdsj~control node-just)))
         (if ctrl
             (setf (pdsc~alternative-mmatchings ctrl) ,alters)
           (setf (pdsj~control node-just)
                 (pdsc~create nil nil nil ,alters))))
     ))

(defun pdsn~failed-methods (node)
  (pdsc~failed-methods (pdsj~control (node~justification node))))

(defsetf pdsn~failed-methods (node) (alters)
  `(if (pdsn~p ,node)
       (let* ((node-just (node~justification ,node))
              (ctrl (pdsj~control node-just)))
         (if ctrl
             (setf (pdsc~failed-methods ctrl) ,alters)
           (setf (pdsj~control node-just)
                 (pdsc~create nil nil nil ,alters))))
     ))

(defun pdsn~excluded-methods (node)
  (pdsc~excluded-methods (pdsj~control (node~justification node))))
  
(defsetf pdsn~excluded-methods (node) (alters)
  `(if (pdsn~p ,node)
       (let* ((node-just (node~justification ,node))
	      (ctrl (pdsj~control node-just)))
	 (if ctrl
	     (setf (pdsc~excluded-methods ctrl) ,alters)
	   (setf (pdsj~control node-just)
		 (pdsc~create nil nil nil ,alters))))
     ))



#{\subsection{Structural Functions for Nodes}#}

(defun pdsn~justifying-nodes (justified-nodes)
  (declare (edited  "18-SEP-1996")
	   (authors Lassaad NESMITH)
	   (input  "A list of proof plan nodes.")
	   (effect "None." )
	   (value  "The nodes which justify the input nodes, as well as the nodes which"
		   "justify them, ad infinitum, recursively.  That is, all nodes upon which the "
		   "input nodes depend." ))
  (let ((nodes-to-check justified-nodes)
	(curr-jnode nil)
	(just-premises nil)
	(justifying-nodes nil)
	(already-checked nil))
    (loop
     (unless nodes-to-check (return-from pdsn~justifying-nodes 
				     (remove-duplicates justifying-nodes)))
     (setq curr-jnode (car nodes-to-check)
	   nodes-to-check (cdr nodes-to-check))
     (unless (member curr-jnode already-checked)
       (setq just-premises (pdsn~just-ass&premises curr-jnode)
	     justifying-nodes (cons curr-jnode 
				    (append just-premises justifying-nodes))
	     nodes-to-check (append just-premises nodes-to-check)
	     already-checked (cons curr-jnode already-checked))))))

(defun pdsn~depending-nodes (node)
  (declare (edited  "01-FEB-1999")
	   (authors Lassaad)
 	   (input   "A node.")
 	   (effect  "None.")
 	   (value   "The nodes which depend on the input nodes, as well as the nodes which"
 		    "depend on them, ad infinitum, recursively.  That is, all nodes which"
 		    "are closed by methods having (directly or indirectly) the input node"
 		    "as subgoal."))
  (let* ((node-other-reasons (pdsj~other-reasons (node~justification node)))
 	 (depending-reasons (remove-if-not #'(lambda (reason)
 					       (find node (just~premises (pdsc~an-just reason))))
 					   node-other-reasons))
 	 (depending-nodes (mapcar #'pdsc~an-node depending-reasons)))
    (remove-duplicates (append depending-nodes
 			       (apply #'append (mapcar #'pdsn~depending-nodes depending-nodes))))
    ))
		  
#{\subsection{Special Nodes}#}
#{\subsubsection{Constructors for Special Nodes}#}


(defun pdsn~make-hypothesis (formula label)
  (declare (edited  "10-JUN-1997" "19-SEP-1996")
           (authors Sorge Lassaad)
           (input   "A formula, and a name.")
           (effect  "none")
           (value    "A new hypothesis node with name LABEL, and assertion FORMULA."))
  (let ((new-node
	 (make-instance 'pdsn+node
			:name label
			:formula formula
			:justification (pdsj~closed-just-create (infer~find-method "hyp")
								NIL NIL "grounded")
			)))
    (setf (pdsn~hyps new-node) (list new-node))
    new-node))


(defun pdsn~open-node-create (formula hyps label &optional reasons)
  (declare (edited  "10-JUN-1997" "19-SEP-1996" "28-JUL-92 10:00")
           (authors Sorge Lassaad NESMITH)
           (input   "A formula, a list of hypothesis nodes, a label,"
		    "and optionally a reason list.")
           (effect  "None.")
           (value   "A new open node with label LABEL, hypotheses HYPS,"
		    "and assertion FORMULA."))
  (let ((just (pdsj~open-just-create)))
    (when reasons
      (setf (pdsj~reasons just) reasons))
    (pdsn~create label hyps formula just)
    ))


#{\subsubsection{Queries for Special Nodes}#}

(defun pdsn~hypothesis-node-p (node)
  (declare (edited  "20-FEB-1997")
           (authors Lassaad)
           (input   "A node.")
           (effect  "none")
           (value   "True if NODE is a hypothesis, nil otherwise."))
  (find node (pdsn~hyps node)) ; Hypotheses are nodes that occur in their own hypotheses slot
  #+old(let* ((node-just (node~justification node))
	 (just-method (when node-just
			(just~method node-just))))
    (and (infer~dummy-p just-method)
	 (not (infer~open-p just-method)))))


(defun pdsn~hypothesis-p (node)
  (declare (edited  "20-FEB-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff NODE is a hypothesis with method \"Hyp\", that is NODE"
		    "corresponds to an assumption of the problem description or"
		    "is inserted by a method application."))
  (let ((method (node~just-method node)))
    (and (equal (pdsn~hyps node) (list node))
         (infer~dummy-p method)
	 (not (infer~open-p method))
	 (not (th~assumption-method-p method)))
    ))


(defun pdsn~th-assumption-p (node)
  (declare (edited  "20-FEB-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff NODE is a hypothesis justified by a method"
		    "used for theory-assumption."))
  (th~assumption-method-p (node~just-method node)))


(defun pdsn~local-definition-p (node)
  (declare (edited  "13-SEP-1999")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T iff Node is a hypothesis justified by a local-definition"
		    "justification."))
  (let* ((just (node~justification node))
	 (method (just~method just)))
    (string-equal (keim~name method)
		  (keim~name (infer~find-method 'local-def)))))


(defun pdsn~open-node-p (node)
  (declare (edited  "23-JUN-1998" "18-SEP-1996")
	   (authors Jzimmer Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff NODE has an open justification."))
  (and (pdsn~p node)
       (let ((just (node~justification node)))
	 (when just
	   (pdsj~open-just-p just)))))

(defun pdsn~unexpanded-p (node)
  (declare (edited  "25-MAY-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff the NODE least abstract justification has the status unexpanded."))
  (pdsj~unexpanded-p (pdsn~least-abstract-just node)))


(defun pdsn~grounded-p (node)
  (declare (edited  "25-MAY-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff the NODE least abstract justification has the status grounded."))
  (pdsj~grounded-p (pdsn~least-abstract-just node)))


(defun pdsn~untested-p (node)
  (declare (edited  "25-MAY-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "T, iff the NODE least abstract justification has the status untested."))
  (pdsj~untested-p (pdsn~least-abstract-just node)))

;;; print-object

(defmethod print-object ((node pdsn+node) stream)
  (format stream 
	  (if *print-escape* "#<pdsn+node ~S>" "~S")
	  (keim~name node)))


(defun pdsn~insert-reason! (node reason)
  (declare (edited  "25-MAY-1997" "08-NOV-1996")
	   (authors Lassaad)
	   (input   "A node, and a reason.")
	   (effect  "Inserts REASON in the reasons of NODE justification.")
	   (value   "Unspecified."))
  (pdsj~insert-reason! (node~justification node) reason))

#|
;; must be adapted if needed 
(defun pdsn~appl-hyp-in-but-not-in-p (abstr term nodes1 nodes2)
  (declare (edited  "19-JUN-1997")
	   (authors Lassaad)
	   (input   "A lambda abstraction, a term, and two node lists.")
	   (effect  "None.")
	   (value   "T, iff NODES1 contains a hypothesis whose formula corresponds"
		    "to the application of ABSTR to TERM and this hypothesis does"
		    "not occur in NODES2."))
  (pdsn~hyp-in-but-not-in-p (hop~beta-normform (data~appl-create abstr (list term)))
			    nodes1 nodes2))
|#
  

(defgeneric pdsn~hyp-in-but-not-in-p (object nodes1 nodes2)
  (declare (edited  "18-JUN-1997")
	   (authors Lassaad)
	   (input   "An object (a formula or a node), and two node lists.")
	   (effect  "None.")
	   (value   "T, iff OBJECT is (a, or a formula of a) hypothesis node"
		    "that belongs to NODES1 and does not occur in NODES2."))
  (:method ((formula term+term) (nodes1 list) (nodes2 list))
	   (find-if #'(lambda (node)
			(and (pdsn~hypothesis-p node)
			     (keim~equal (node~formula node) formula)))
		    (set-difference nodes1 nodes2)))
  (:method ((node pdsn+node) (nodes1 list) (nodes2 list))
	   (find node (set-difference nodes1 nodes2)))
  )


#|
;; must be adapted if needed
(defun pdsn~superset-of-without-appl-hyp-p (nodes1 nodes2 abstr term)
  (declare (edited  "19-JUN-1997")
	   (authors Lassaad)
	   (input   "Two node lists, a lambda abstraction, and a term.")
	   (effect  "None.")
	   (value   "T, iff NODES2 without the hypothesis whose formula corresponds"
		    "to the application of ABSTR to TERM is a subset of NODES1."))
  (pdsn~superset-of-without-hyp-p nodes1 nodes2
				  (hop~beta-normform (data~appl-create abstr (list term)))))
|#


(defgeneric pdsn~superset-of-without-hyp-p (nodes1 nodes2 object)
  (declare (edited  "18-JUN-1997")
	   (authors Lassaad)
	   (input   "Two node lists, and an object (a hypothesis formula, "
		    "or a hypothesis node).")
	   (effect  "None.")
	   (value   "T, iff the second node list without the hypothesis node"
		    "is a subset of the first node list."))
  (:method ((nodes1 list) (nodes2 list) (formula term+term))
	   (let ((formula-node (find-if #'(lambda (node)
					    (and (pdsn~hypothesis-p node)
						 (keim~equal (node~formula node) formula)))
					nodes2)))
	     (if formula-node
		 (subsetp (remove formula-node nodes2) nodes1)
	       (subsetp nodes2 nodes1))))
  (:method ((nodes1 list) (nodes2 list) (node pdsn+node))
	   (if (pdsn~hypothesis-p node)
	       (subsetp (remove node nodes2) nodes1)
	     (error "PDSN~~SUPERSET-OF-WITHOUT-HYP-P: ~A is not a hypothesis node." node)))
  )
	      
	   
 
