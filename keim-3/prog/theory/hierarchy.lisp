;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A data structure for hierarchies of definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "KEIM")


(mod~defmod HIER 
            :uses (data keim node prob term th)
            :documentation "A data structure for hierarchies of definitions."
            :exports (
                      hier+graph
                      hier+node
                      
                      hier~get-node
                      hier~get-node-rank
                      hier~graph-create
		      hier~graph-empty-p
                      hier~graph-nodes
                      hier~graph-roots
                      hier~hierarchy-for-problem
                      hier~insert-node
                      hier~make-hierarchy
                      hier~node-children
                      hier~node-create
                      hier~node-object
                      hier~node-rank
		      hier~pop-definition
		      hier~pop-definitions
                      hier~rank-node
                      hier~rank-nodes
                      hier~remove-node
                      hier~remove-nodes
                      hier~update-root-nodes
                      ))

(defclass hier+graph (keim+object)
  ((roots :initarg :roots
	  :initform NIL
	  :accessor hier~graph-roots
	  :documentation "The set of root nodes of the hierarchy graph.")
   (nodes :initarg :nodes
	  :initform NIL
	  :accessor hier~graph-nodes
	  :documentation "The set of nodes in the hierarchy graph."))
  (:documentation "The hierarchy graph."))
   

(defclass hier+node (keim+object)
  ((object :initarg :object
	     :initform NIL
	     :accessor hier~node-object
	     :documentation "The defined object the node represents.")
   (children :initarg :children
	     :initform NIL
	     :accessor hier~node-children
	     :documentation "The list of children of this definition.")
   (rank :initarg :rank
	 :initform NIL
	 :accessor hier~node-rank
	 :documentation "The rank of the defined object in the hierarchy. If that rank is NIL it is not properly ranked yet."))
  (:documentation "A node in the hierarchy data-structure."))
   

(defun hier~graph-create (nodes)
  (make-instance 'hier+graph :nodes nodes))

(defun hier~node-create (object)
  (make-instance 'hier+node :object object))


(defun hier~graph-empty-p (graph)
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A hierarchy graph.")
	   (effect  "None.")
	   (value   "T if the graph is empty. O/W NIL."))
  (and (null (hier~graph-roots graph))
       (null (hier~graph-nodes graph))))
  
(defgeneric hier~insert-node (node graph)
  (declare (edited  "25-APR-2006")
	   (authors Sorge)
	   (input   "A node and a graph.")
	   (effect  "Inserts the node into the graph, if it is new.")
	   (value   "Either the newly inserted node, or the old node in the graph."))
  (:method ((node hier+node) (graph hier+graph))
	   (let* ((nodes (hier~graph-nodes graph))
		  (old-node (or (find node nodes)
				(find (hier~node-object node) nodes :key #'hier~node-object :test #'keim~equal))))
	     (if old-node old-node
	       (progn
		 (setf (hier~graph-nodes graph) (cons node nodes))
		 node))))
  (:method ((node th+def) (graph hier+graph))
	   (let* ((nodes (hier~graph-nodes graph))
		  (old-node (find node nodes :key #'hier~node-object :test #'keim~equal)))
	     (if old-node old-node
	       (let ((new-node (hier~node-create node)))
		 (setf (hier~graph-nodes graph) (cons new-node nodes))
		 new-node))))
  )

  

(defun hier~rank-node (node)
  (declare (edited  "25-APR-2006")
	   (authors Sorge)
	   (input   "A list of definitions occurring the object of the node.")
	   (effect  "Computes and enters the rank if possible.")
	   (value   "The new rank."))
  (when (null (hier~node-rank node))
    (let ((children (hier~node-children node)))
      (if children
	  (progn (hier~rank-nodes children)
		 (let ((child-ranks (mapcar #'hier~node-rank children)))
		   (setf (hier~node-rank node) (1+ (apply #'max child-ranks)))))
	(setf (hier~node-rank node) 0)))))

(defun hier~rank-nodes (nodes)
  (dolist (node nodes)
    (hier~rank-node node)))

(defun hier=contained-definitions (formula defs)
  (declare (edited  "25-APR-2006" "17-APR-1998")
	   (authors Sorge Chris)
	   (input   "A formula and a list of definitions.")
	   (effect  "None")
	   (value   "All defined concept within term"))
  (let* ((poslist (data~positions formula #'(lambda (x) (term~constant-p x))))
	 (constants (mapcar #'(lambda (pos) (data~struct-at-position formula pos))
			    poslist))
	 (equal (logic~equality-constant))
	 (equiv (logic~equivalence-constant)))
    (remove-if #'(lambda (x) (or (keim~equal x equal) (keim~equal x equiv)))
	       (remove-duplicates
		(mapcan #'(lambda (def)
			    (when (find-if #'(lambda (const)
					       (keim~equal (keim~name const)
							   (keim~name def)))
					   constants)
			      (list def)))
			defs))
	       :key #'th~definition-constant)))

(defun hier~hierarchy-for-problem (problem)
  (hier~make-hierarchy
   (node~formula (prob~conclusion problem))
   (prob~theory problem)
   (mapcar #'node~formula (prob~assumptions problem))))


(defun hier~make-hierarchy (formula theory &optional (formulas nil))
  (declare (edited  "25-APR-2006")
	   (authors Sorge)
	   (input   "A formulas, a theory, and optionally a set of assumption formulas.")
	   (effect  "Constructs a hierarchy data structure for the given problem.")
	   (value   "The new hierarchy data structure."))
  (let* ((defs (th~definitions-recursed (th~find-theory theory)))
	 (top-defs (if formulas (mapcan #'(lambda (x)
					       (hier=contained-definitions x defs))
					   (cons formula formulas))
		     (hier=contained-definitions formula defs)))
	 (top-nodes (mapcar #'hier~node-create top-defs))
	 (graph (hier~graph-create top-nodes)))
    (do ((node (car top-defs)  (car top-defs)))
	((null node) graph)
      (let* ((def-formula (th~ass-node node))
	     (new-node (hier~insert-node node graph))
	     (child-defs (hier=contained-definitions def-formula defs))
	     (new-defs (set-difference child-defs top-defs))
	     (child-nodes (mapcar #'(lambda (x) (hier~insert-node x graph)) child-defs)))
	(setf (hier~node-children new-node) child-nodes)
	(setf top-defs (append (cdr top-defs) new-defs))
      ))
    (hier~rank-nodes (hier~graph-nodes graph))
    (hier~update-root-nodes graph)
    graph
    ))

(defun hier~update-root-nodes (graph)
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A hierarchy graph.")
	   (effect  "Sets the root nodes of the graph.")
	   (value   "Undefined."))
  (if (null (hier~graph-nodes graph))
      (setf (hier~graph-roots graph) nil)
    (let ((max-rank (apply #'max (mapcar #'hier~node-rank (hier~graph-nodes graph)))))
      (setf (hier~graph-roots graph)
	    (remove-if-not #'(lambda (x) (= (hier~node-rank x) max-rank)) (hier~graph-nodes graph))))
    ))

(defgeneric hier~remove-node (node graph)  ;;; This is simplistic and we assume that we always remove the top node!
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A node and a hierarchy graph.")
	   (effect  "Removes the node from the graph.")
	   (value   "The updated graph."))
  (:method ((node hier+node) (graph hier+graph))
	   (when (find node (hier~graph-nodes graph))
	     (setf (hier~graph-nodes graph) (remove node (hier~graph-nodes graph)))
	     (hier~update-root-nodes graph))
	   )
  (:method ((def th+def) (graph hier+graph))
	   (let* ((nodes (hier~graph-nodes graph))
		  (node (find def nodes :key #'hier~node-object)))
	   (when node
	     (setf (hier~graph-nodes graph) (remove node (hier~graph-nodes graph)))
	     (hier~update-root-nodes graph))
		 ))
  (:method ((term term+term) (graph hier+graph))
	   (let* ((nodes (hier~graph-nodes graph))
		  (node (find term nodes
			      :key #'(lambda (x) (th~definition-constant (hier~node-object x)))
			      :test #'data~schema-equal)))
	     (when node
	       (setf (hier~graph-nodes graph) (remove node (hier~graph-nodes graph)))
	       (hier~update-root-nodes graph))
	     ))
  )


;; (defun hier~remove-nodes  (nodes graph)
;;   (if nodes
;;       (hier~remove-nodes (cdr nodes) (hier~remove-node (car nodes) graph))
;;     graph))

(defun hier~remove-nodes  (nodes graph)
  (dolist (node nodes)
    (hier~remove-node node graph))
  graph)


(defgeneric hier~get-node (node graph)  
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A node (or term or definition) and a hierarchy graph.")
	   (effect  "None.")
	   (value   "The node if it is in the graph."))
  (:method ((node hier+node) (graph hier+graph))
	   (find node (hier~graph-nodes graph)))
  (:method ((def th+def) (graph hier+graph))
	   (find def (hier~graph-nodes graph) :key #'hier~node-object))
  (:method ((term term+term) (graph hier+graph))
	   (find term (hier~graph-nodes graph)
		 :key #'(lambda (x) (th~definition-constant (hier~node-object x)))
		 :test #'data~schema-equal))
  )

(defgeneric hier~get-node-rank (node graph)
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A node (or term or definition) and a hierarchy graph.")
	   (effect  "None.")
	   (value   "The node's rank if it is in the graph."))
  (:method ((node hier+node) (graph hier+graph))
	   (let ((found (find node (hier~graph-nodes graph))))
	     (when found (hier~node-rank found))))
  (:method ((def th+def) (graph hier+graph))
	   (let ((found (find def (hier~graph-nodes graph) :key #'hier~node-object)))
	     (when found (hier~node-rank found))))
  (:method ((term term+term) (graph hier+graph))
	   (let ((found (find term (hier~graph-nodes graph)
			      :key #'(lambda (x) (th~definition-constant (hier~node-object x)))
			      :test #'data~schema-equal)))
	     (when found (hier~node-rank found))))
  )


(defun hier~pop-definition (graph)
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A hierarchy graph.")
	   (effect  "Pops the topmost root node of the graph and deletes it from the graph.")
	   (value   "Two values: 1. The topmost node. 2. The modified graph structure."))
  (unless (hier~graph-empty-p graph)
    (let ((top-root (car (hier~graph-roots graph))))
      (hier~remove-node top-root graph)
      (values ; (th~definition-constant (hier~node-object top-root))
              (hier~node-object top-root)
	      graph))))

(defun hier~pop-definitions (graph)
  (declare (edited  "30-APR-2006")
	   (authors Chris)
	   (input   "A hierarchy graph.")
	   (effect  "Pops the root nodes of the graph and deletes them from the graph.")
	   (value   "Two values: 1. The topmost nodes. 2. The modified graph structure."))
  (unless (hier~graph-empty-p graph)
    (let ((roots (hier~graph-roots graph)))
      (hier~remove-nodes roots graph)
      (values ; (th~definition-constant (hier~node-object top-root))
              (mapcar #'hier~node-object roots)
	      graph))))


(defun hier~pop-definitions (graph)
  (declare (edited  "26-APR-2006")
	   (authors Sorge)
	   (input   "A hierarchy graph.")
	   (effect  "Pops all the root nodes of the graph and deletes them from the graph.")
	   (value   "Two values: 1. The list of root nodes. 2. The modified graph structure."))
  (unless (hier~graph-empty-p graph)
    (let ((roots (hier~graph-roots graph)))
      (dolist (top-root roots)
	(hier~remove-node top-root graph))
      (values (mapcar #'th~definition-constant roots)
	      graph))))


(defmethod print-object ((obj hier+node) stream)
  (format stream "~%~A over (~{~A~^,~}) rank: ~A" (hier~node-object obj)
	  (mapcar #'hier~node-object (hier~node-children obj))
	  (hier~node-rank obj)))

(defmethod print-object ((obj hier+graph) stream)
  (format stream "~%Roots: (~{~A~^,~}) ~%Nodes: (~{~A~^,~})"
	  (hier~graph-roots obj) (hier~graph-nodes obj)))


