;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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



(mod~defmod ALTR 
            :uses (alb alx data env just keim logic meta misc node pds pdsn pp sys
		       term)
            :documentation "Assertion level rule and premise trees."
            :exports (altr+node-error
                      altr+premise-node
                      altr+rule-node
                      altr+tree-building-error
                      altr+tree-error
                      altr+tree-node
                      
                      altr~add-preconditions
                      altr~create-premise-tree
                      altr~create-rule-tree
                      altr~delete-useless-rule-trees
                      altr~find-or-node-premise
                      altr~find-roots
                      altr~get-node
                      altr~get-premise-trees
                      altr~get-rule-tree
                      altr~leaf-p
                      altr~main-p
                      altr~make-premise-node
                      altr~next-node
                      altr~or-node-p
                      altr~preconditions
                      altr~premise-line
                      altr~premise-rule-node
                      altr~premises2lines
                      altr~remove-detours!
                      altr~root-p
                      altr~show
                      altr~show-all-rule-trees
                      altr~show-cond
                      altr~tree-size-without-leaves
                      altr~update-premise-trees!
                      altr~useless-p
                      altr~valuate-element
		      ))




;################################################
;##                                            ##
;##                  Errors                    ##
;##                                            ##
;################################################

(sys~define-condition
 altr+node-error (sys+error)
 ((content))
 (lambda (condition stream)
   (format stream "~S is neither a rule node, nor a premise node."
	   (altr+node-error-content condition))))

(sys~define-condition
 altr+tree-error (sys+error)
 ((content) (type))
 (lambda (condition stream)
   (format stream "~S doesn't refer to a ~(~S~) tree."
	   (altr+tree-error-content condition)
	   (altr+tree-error-type condition))))

(sys~define-condition
 altr+tree-building-error (altr+tree-error)
 ()
 (lambda (condition stream)
   (format stream "Can't continue building the ~(~S~) tree ~S."
	   (altr+tree-error-content condition)
	   (altr+tree-error-type condition))))





;#########################################
;##                                     ##
;##             Helpers                 ##
;##                                     ##
;#########################################

(defvar altr*visited-nodes-hash-table (misc~make-hash-table 'altr*visited-nodes-hash-table)
  "A hash table that holds already visited premise nodes.
   The keys are the names of visited nodes, and the data are the according premise
   nodes. The nodes should be stored here only during one traverse through the tree, such
   that the nodes are marked only during that traverse.")

(defun altr=visited-p (node)
  (declare (edited  "17-FEB-1993 15:46")
	   (authors AFIEDLER)
	   (input   "A rule node or a premise node")
	   (value   "The node, if it has been visited in the current traverse, else NIL."))
  (misc~gethash (keim~name node) altr*visited-nodes-hash-table))

(defun altr=set-node-visited! (node)
  (declare (edited  "17-FEB-1993 15:50")
	   (authors AFIEDLER)
	   (input   "A node")
	   (effect  #{Marks NODE visited by storing it in#}
		    #{{\vb altr*visited-nodes-hash-table} with its name as key.#})
	   (value   "NODE"))
  (setf (misc~gethash (keim~name node) altr*visited-nodes-hash-table) node))

(defun altr=set-all-nodes-unvisited! ()
  (declare (edited  "17-FEB-1993 16:00")
	   (authors AFIEDLER)
	   (input   "None")
	   (effect  #{Markes all nodes unvisited by clearing#}
		    #{{\vb altr*visited-nodes-hash-table}. This function should be called#}
		    #{after a traverse.#})
	   (value   "Undefined."))
  (misc~clrhash altr*visited-nodes-hash-table))



(defvar altr*main-p-hash-table (misc~make-hash-table 'altr*main-p-hash-table)
  "A hash table, that holds all rule nodes of the main conclusion paths.
   The keys are the names of rule nodes, and the data are T.")

(defgeneric altr~main-p (node)
  (declare (edited  "25-FEB-1993 17:53")
	   (authors AFIEDLER)
	   (input   "A node.")
	   (value   "True, if NODE is a main node, that means it is in the main conclusion"
		    "path of a rule tree."))
  )


(defun altr=mark-main-node (node)
  (declare (edited  "25-FEB-1993 17:54")
	   (authors AFIEDLER)
	   (input   "A rule node.")
	   (effect  #{Marks NODE as a main node, that means it is in the main conclusion#}
		    #{path of a rule tree, by storing T in {\vb altr*main-p-hash-table}#}
		    #{with the name of NODE as key.#})
	   (value   "T"))
  (setf (misc~gethash (keim~name node) altr*main-p-hash-table) t))



;#########################################
;##                                     ##
;##      Klassen mit Funktionen         ##
;##                                     ##
;#########################################

(defclass altr+tree-node (keim+name)
  ((preconditions :accessor altr~preconditions
		  :initarg :preconditions
		  :documentation
		  "This is the {preconditions} slot of {tree node}. It contains a list of
                   the sons of the tree node.") 
   (next-node :accessor altr~next-node
	      :initarg :next-node
	      :documentation
	      "This is the {next node} slot of {tree node}. It contains a list of the
               fathers of the node.
               Actually, this defines a graph and sometimes a graph will be built, but in
               most cases `next-node' contains a list of only one name, i.e. mostly a tree
               is defined."))   
  (:default-initargs :preconditions nil
		     :next-node nil)
  (:documentation "A node in a tree.
                   The slot `preconditions' contains a list of the sons of the node, and
                   the slot `next-node' contains a list of the fathers of the node.  
                   Actually, this defines a graph and sometimes a graph will be built,
                   but in most cases `next-node' contains a list of only one node, i.e.
                   mostly a tree is defined."))

(defclass altr+rule-node (pdsn+node altr+tree-node)
  ((no-flip :accessor altr=no-flip-p
	    :initarg :no-flip
	    :initform nil
	    :documentation "Boolean to indicate, whether this node may be flipped."))
  (:default-initargs :justification nil
		     :no-flip nil)
  (:documentation "A rule node, i. e. a node in a rule tree.
                   Actually, it is sometimes a graph, i. e. the tree has several roots.
                   Thus a set of rule trees is represented in a single graph. But
                   traversing the graph from a root only using precondition links leads to
                   the leaves of the tree with that root. Thus  several trees share some
                   branches and a part of the main conclusion path.
                   This happens, if in one node containing a conjunction distinct
                   elements are chosen by applying the and-elimination rule.
                   The slots `label', `hyp' and `supports' of {\\vb pdsn+nodes} are not
used."))

(defmethod print-object ((node altr+rule-node) stream)
  (format stream "#<altr+rule-node ~S>" (keim~name node)))


(defclass altr+premise-node (altr+rule-node)
  ((rule-node :accessor altr~premise-rule-node
	      :initarg :rule-node
	      :documentation
	      "This is the {corresponding rule node} of {premise tree node}.") 
   (line :accessor altr~premise-line
	 :initarg :line
	 :documentation
	 "This is the {proof line, that matches the formula} of {premise tree
          node}."))  
  (:default-initargs :rule-node nil
		     :line nil)
  (:documentation "A node in a premise tree.
                   A premise tree is a real rule tree (not a graph) having some more
                   slots.
                   The slot `formula' contains the formula of the according rule node or
                   its negation, dependent on the premise tree being created positively
                   or negatively (i. e. the rule tree or the associated rule tree, see
                   [Huang 91] for further information).
                   The slot `rule-node' contains the name of the according rule node.
                   The slot `line' contains the proof line, whose formula
                   matches to the `formula' slot, if it exists, otherwise nil."))

(defmethod print-object ((pre altr+premise-node) stream)
  (format stream "#<altr+premise-node ~S>" (keim~name pre)))

(defvar altr*rule-prefix "RT-"
  "The prefix for rule nodes.")

(defvar altr*premise-prefix "PT-"
  "The prefix for premise nodes.")

(defgeneric altr=set-new-name! (node)
  (declare (edited  "04-FEB-1993 14:04")
	   (authors AFIEDLER)
	   (input  "A tree node.")
	   (effect  #{Gives NODE a new name with the prefix in {\vb altr*rule-prefix} or#}
		    #{{\vb altr*premise-prefix} respectively.#})
	   (value   "Undefined."))
  (:method ((node altr+rule-node))
	   (keim~set-name! node (gensym altr*rule-prefix)))
  (:method ((node altr+premise-node))
	   (keim~set-name! node (gensym altr*premise-prefix))))


(defvar altr*tree-hash-table (misc~make-hash-table 'altr*tree-hash-table)
  "A hash table that holds all tree nodes.
   The keys are the names of the nodes, and the data are the according tree nodes.
   The nodes of rule trees have the prefix \"RT-\", those of premise trees \"PT-\".")

(defun altr=get-node (name)
  (declare (edited  "01-FEB-1994 10:27")
	   (authors AFIEDLER)
	   (input   "The name of an tree node.")
	   (value   "The tree node, if it exists, otherwise an error is signalled."))
  (let ((obj (misc~gethash name altr*tree-hash-table)))
    (if obj
	obj
      (error (sys~make-condition 'altr+node-error :content name)))))

;### noch notwendig?
(defgeneric altr~get-node (name)
  (declare (edited  "01-FEB-1994 10:26")
	   (authors AFIEDLER)
	   (input   "The name of an tree node.")
	   (value   "The tree node, if it exists, otherwise an error is signalled."))
  (:method ((name symbol))
	   (altr=get-node name))
  (:method ((name string))
	   (altr=get-node name)))

(defmethod misc~store ((node altr+tree-node))
  (setf (misc~gethash (keim~name node) altr*tree-hash-table) node)
  node)

(defun altr~add-preconditions (node preconditions)
  (declare (edited  "26-FEB-1993 09:13")
	   (authors AFIEDLER)
	   (input   "A tree node and a list of tree nodes that are instances of"
		    "the same subclass of altr+tree-node as NODE and direct precondition"
		    "nodes of NODE.")
	   (effect  "Adds PRECONDITIONS to the list in the `preconditions' slot of"
		    "NODE.")
	   (value   "Undefined."))
  (if preconditions
      (setf (altr~preconditions node) (append (altr~preconditions node) preconditions))))

(defun altr=not-visited-preconditions (node)
  (declare (edited  "26-FEB-1993 09:19")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (value   "A list of the not yet visited direct precondition nodes of NODE."))
  (let ((res (apply #'append
		    (mapcar #'(lambda (pre)
				(if (not (altr=visited-p pre))
				    pre))
			    (altr~preconditions node)))))
    (if res (list res))))

(defun altr=flippables (nodes)
  (declare (edited  "18-DEC-1997")
	   (authors Afiedler)
	   (input   "A list of rule tree nodes.")
	   (value   "NODES with non-flippables removed."))
  (remove-if #'altr=no-flip-p nodes)
  #+old(if nodes
      (apply #'append
	     (mapcar #'(lambda (node)
			 (if (not (altr=no-flip-p node))
			     (list node)))
		     nodes))))

(defun altr~premises2lines (premises)
  (declare (edited  "10-NOV-1993 13:43")
	   (authors AFIEDLER)
	   (input   "A list premise nodes.")
	   (value   "A list of the matching proof lines."))
  (mapcar #'altr~premise-line premises))


(defun altr=get-rule-tree (name)
  (declare (edited  "01-FEB-1994 10:34")
	   (authors AFIEDLER)
	   (input   "The reference to a tree (i.e. a label of a line).")
	   (value   "The node referencing a current proof's rule tree, if it exists,"
		    "otherwise an error is signalled."))
  (let ((obj (misc~gethash name (alb~rule-tree-hash-table
				 (alb~proof-hash-tables pds*current-proof-plan)))))
    (if obj
	obj
      (error (sys~make-condition 'altr+tree-error :content name :type 'rule)))))

(defgeneric altr~get-rule-tree (line)
  (declare (edited  "01-FEB-1994 10:38")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (value   "The node referencing a current proof's rule tree, if it exists,"
		    "otherwise an error is signalled."))
  (:method ((line pdsn+node))
	   (altr~get-rule-tree (keim~name line)))
  (:method ((line symbol))
	   (altr~get-node (altr=get-rule-tree line)))
  (:method ((line string))
	   (altr~get-node (altr=get-rule-tree line))))

(defun altr=get-premise-trees (name)
  (declare (edited  "03-NOV-1997")
	   (authors Afiedler)
	   (input   "The reference to trees (i.e. a label of a line).")
	   (value   "A list of names of nodes referencing current proof's premise"
		    "trees, if they exist, otherwise an error is signalled."))
  (cond ((alb~get-premise-trees name))
	(t (error (sys~make-condition 'altr+tree-error :content name :type 'premise)))))

(defgeneric altr~get-premise-trees (line)
  (declare (edited  "01-FEB-1994 10:47")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (value   "A list of nodes referencing current proof's premise trees,"
		    "if they exist, otherwise an error is signalled."))
  (:method ((line pdsn+node))
	   (altr~get-premise-trees (keim~name line)))
  (:method ((line symbol))
	   (mapcar #'altr~get-node (altr=get-premise-trees line)))
  (:method ((line string))
	   (mapcar #'altr~get-node (altr=get-premise-trees line))))



(defmethod alb~store-tree ((line symbol) (node altr+rule-node))
  (setf (misc~gethash line (alb~rule-tree-hash-table
			    (alb~proof-hash-tables pds*current-proof-plan)))
	(keim~name node)))

(defmethod alb~store-tree ((line symbol) (node altr+premise-node))
  (let ((tree (sys~handler-case
	       (alb~get-premise-trees line)
	       (altr+tree-error () nil))))
    (setf (misc~gethash line (alb~premise-tree-hash-table
			      (alb~proof-hash-tables pds*current-proof-plan)))
	  (append tree (list (keim~name node))))))

(defmethod altr~main-p ((node altr+rule-node))
	   (misc~gethash (keim~name node) altr*main-p-hash-table))

(defmethod altr~main-p ((node altr+premise-node))
	   (let ((rule-node (altr~premise-rule-node node)))
	     (cond ((altr~main-p rule-node)
		    (term~alpha-equal (node~formula node) (node~formula rule-node)))
		   (t (let ((prewff (node~formula node)))
			(and (logic~negation-p prewff)
			     (alx~match-p (node~formula rule-node)
					   (alx~negation-scope prewff))))))))





;#########################################
;##                                     ##
;##             Regelbaeume             ##
;##                                     ##
;#########################################

(defun altr=no-rule-applicable-p (formula)
  (declare (edited  "08-MAR-1993 13:24")
	   (authors AFIEDLER)
	   (input   "A formula.")
	   (value   "True, if FORMULA is an atom in the sense of first order predicate"
		    "logic, otherwise NIL."))
  (logic~atom-p formula))

(defun altr=make-rule-node (&key hyps formula justification supports preconditions
				 next-node no-flip)
  (declare (edited  "17-SEP-1992 14:41")
	   (authors AFIEDLER)
	   (input   "A list of hypothesis lines, a formula, a justification, a list of"
		    "support lines, a list of the direct precondition nodes, a list of"
		    "the direct conclusion nodes, and a boolean.")  
	   (effect  "A new rule node is made, and put in the hash table of rule nodes,"
		    "after getting a new name with prefix \"RT-\".")
	   (value   "The new rule node."))
  (omega~output ".")
  (let ((new-node (make-instance 'altr+rule-node
				 :hyps hyps
				 :formula formula
				 :justification justification
				 :supports supports
				 :preconditions preconditions
				 :next-node  next-node
				 :no-flip no-flip)))
    (altr=set-new-name! new-node)
    (misc~store new-node)))

;;; The following functions make the next node in a rule tree, not only building the main
;;; path down to the root, but also building the side branches up, dependend on being
;;; PRECONDITIONS or NEXT-NODE nil. If NEXT-NODE is nil, then the next node is in the main
;;; conclusion path, else PRECONDITIONS should be nil and the next node is in a side branch.

(defun altr=handle-branch-node! (new-node node)
  (declare (edited  "16-DEC-1992 10:28")
	   (authors AFIEDLER)
	   (input   "Two rule nodes.")
	   (effect  "If the `next-node' slot of NEW-NODE is not nil, that is NEW-NODE is"
		    "in a branch, then add NEW-NODE to the preconditions list of NODE.")
	   (value   "NEW-NODE."))
  (when (altr~next-node new-node)
    (altr~add-preconditions node (list new-node)))
  new-node)

(defun altr=handle-universal-quantification (node preconditions next-node)
  (declare (edited  "23-JUN-1997"  "24-FEB-1993 13:35")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is an universal quantification.")
	   (value   "A list with the new node, if it exists, otherwise NIL."))
; Actually, this function applies a forall-elimination to NODE using a new meta variable
; with prefix `META-' and writes the resulting formula in the `formula' slot of a new rule
; node that is inserted in the rule tree by setting the `preconditions' slot of the new
; node.
; This is only done building the main conclusion path downward. In a side branch, NIL is
; returned to break the building of the branch.
  (if preconditions
      (let* ((meta (gensym "META-"))
	     (env (alb~rule-environment pds*current-proof-plan))
	     (formula (progn (env~enter meta
					(meta~variable-create
					 meta
					 (term~type (logic~quantification-bound-variable
						     (node~formula node))))
					env)
      			     (beta~contract
			      (data~appl-create
			       (car (data~appl-arguments
				     (node~formula node)))
			       (list (post~read-object meta env :existing-term)))))))
	(list (altr=make-rule-node :formula formula
				   :preconditions preconditions
				   :next-node next-node)))))

(defun altr=handle-existential-quantification (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "24-FEB-1993 13:45")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is an existential quantification.")
	   (value   "A list with the new node, if it exists, otherwise NIL."))
; Actually, this function applies a exists-elimination to NODE using a new meta variable
; with prefix `META-' and writes the resulting formula in the `formula' slot of a new rule
; node that is inserted in the rule tree by setting the `next-node' of the new node.
; This is only done building a side branch upward.
  (if next-node
      (let* ((meta (gensym "META-"))
	     (env (alb~rule-environment pds*current-proof-plan))
	     (formula (progn (env~enter meta
					(meta~variable-create
					 meta
					 (term~type (logic~quantification-bound-variable
						     (node~formula node))))
					env)
			     (beta~contract
			      (data~appl-create
			       (car (data~appl-arguments
				     (node~formula node)))
			       (list (post~read-object meta env :existing-term))))))
	     (new-node (list (altr=make-rule-node :formula formula
						  :preconditions preconditions
						  :next-node next-node))))
	(altr~add-preconditions node new-node)
	new-node)))

(defun altr=handle-atom (node preconditions next-node)
  (declare (edited  "30-SEP-1992 11:28")
	   (authors AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Breaks building that branch or the tree.")
	   (value   "NIL"))
  (declare (ignore node preconditions next-node))
  nil)

(defun altr=handle-double-negation (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "16-DEC-1992 10:33")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, whose formula is a double negation, a preconditions"
		    "list, and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree.")
	   (value   "A list with the new node."))
; Actually, this function applies a not-not-elimination to NODE and writes the resulting
; formula in the `formula' slot of a new rule node that is inserted in the rule tree by
; setting the `preconditions' slot or the `next-node' slot of the new node.
  (list (altr=handle-branch-node!
	 (altr=make-rule-node :formula (alx~negation-scope
					(alx~negation-scope (node~formula node)))
			      :preconditions preconditions
			      :next-node next-node)
	 node)))

(defun altr=handle-single-negation (node preconditions next-node)
  (declare (edited  "16-DEC-1992 10:33")
	   (authors AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is a single negation.")
	   (value   "A list with the new node, if it exists, otherwise NIL."))
; Actually, this function tries to push the negation into the formula and writes the
; resulting formula in the `formula' slot of a new rule node that is inserted in the rule
; tree by setting the `preconditions' slot or the `next-node' slot of the new node. If
; pushing fails, NIL is returned. 
  (let ((formula (alx~negation-normal node)))
    (if formula
	(list (altr=handle-branch-node!
	       (altr=make-rule-node :formula formula
				    :preconditions preconditions
				    :next-node next-node)
	       node)))))
					
(defun altr=handle-negation (node preconditions next-node)
  (declare (edited  "25-NOV-1992 17:04")
	   (authors AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is a negation.")
	   (value   "A list with the new node, if it exists, else NIL."))
; Actually, this function delegates work to `altr=handle-double-negation' and
; `altr=handle-single-negation', if a rule is applicable, else it returns NIL. 
  (cond ((alx~double-negation-p (node~formula node))
	 (altr=handle-double-negation node preconditions next-node))
	((altr=no-rule-applicable-p (alx~negation-scope (node~formula node)))
	 nil)
	(T (altr=handle-single-negation node preconditions next-node))))

(defun altr=handle-disjunction (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "18-FEB-1994 13:28")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is a disjunction.")
	   (value   "A list with the new nodes, if they exist, else NIL."))
; Actually, this function applies an or-elimination to NODE, what results in two
; formulae. One is the conclusion, the other the lacking second precondition to apply that
; rule. Both formulae are written in the `formula' slot of two new rule nodes that are
; inserted in the rule tree by setting the `preconditions' slot or `next-node' slot of the
; new nodes. The subtree with the second precondition as root is also created.
; This happens only building the main conclusion path downward. In a side branch, two
; precondition nodes are created, where one of them must be justified. NODE is called an
; `Or-node.' 
  (cond (preconditions
	 (let* ((args (data~appl-arguments (node~formula node)))
		(new-node (altr=make-rule-node :formula (car args)
					       :preconditions preconditions)))
	   (altr~add-preconditions new-node
				   (altr=make-previous-node (alx~negate-formula
							     (cadr args))
							    new-node))
	   (list new-node)))
	(next-node
	 (mapcar #'(lambda (wff)
			   (altr=handle-branch-node!
			    (altr=make-rule-node :formula wff
						 :next-node next-node)
			    node))
		 (data~appl-arguments (node~formula node))))
	(t (error (sys~make-condition 'altr+tree-building-error)))))

(defun altr=handle-conjunction (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "22-MAR-1993 12:18")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule nodes in the rule tree, if the current formula"
		    "is a conjunction.")
	   (value   "A list with the new nodes."))
; Actually, this function applies an and-elimination to NODE and writes the resulting
; formula in the `formula' slot of a new rule node that is inserted in the rule tree by
; setting the `preconditions' slot or the `next-node' slot of the new node. 
  (mapcar #'(lambda (wff)
	      (altr=handle-branch-node! (altr=make-rule-node :formula wff
							     :preconditions preconditions
							     :next-node next-node)
					node))
	  (data~appl-arguments (node~formula node))))

(defun altr=handle-equivalence (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "16-DEC-1992 11:19")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current formula"
		    "is an equivalence.")
	   (value   "A list with the new node."))
; Actually, this function applies an equiv-elimination to NODE and writes the resulting
; formula in the `formula' slot of a new rule node that is inserted in the rule tree by
; setting the `preconditions' slot or the `next-node' slot of the new node. 
  (let* ((args (data~appl-arguments (node~formula node)))
	 (arg1 (car args))
	 (arg2 (cadr args))
	 (implies (env~lookup-object :implies
				     (alb~rule-environment pds*current-proof-plan))))
    (list (altr=handle-branch-node!
	   (altr=make-rule-node :formula
				(data~appl-create implies (list arg1 arg2))
				:preconditions preconditions
				:next-node next-node)
	   node)
	  (altr=handle-branch-node!
	   (altr=make-rule-node :formula
				(data~appl-create implies (list arg2 arg1))
				:preconditions preconditions
				:next-node next-node)
	   node))))

(defun altr=handle-implication (node preconditions next-node)
  (declare (edited  "23-JUN-1997" "08-MAR-1993 09:30")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node, a preconditions list and a next-node list.")
	   (effect  "Creates the next rule node in the rule tree, if the current"
		    "formula is an implication.")
	   (value   "A list with the new nodes."))
; Actually, this function applies an implies-elimination to NODE, what results in two
; formulae. One is the conclusion, the other the lacking second precondition to apply that
; rule. Both formulae are written in the `formula' slot of two new rule nodes that are
; inserted in the rule tree by setting the `preconditions' slot or `next-node' slot of the
; new nodes. The subtree with the second precondition as root is also created.
; This happens only building the main conclusion path downward, i.e. if PRECONDITIONS is
; not NIL. Otherwise, in the composition path, two precondition nodes are created, which
; hold the negation of the implication's precondition, and its conclusion.
  (let* ((env (alb~rule-environment pds*current-proof-plan))
	 (args (data~appl-arguments (node~formula node)))
	 (imp2or (data~appl-create (env~lookup-object :or env) 
				   (list (data~appl-create (env~lookup-object :not env)
							   (list (car args)))
					 (cadr args)))))
    (cond (preconditions
	   (let ((new-node (altr=make-rule-node :formula (cadr args) ; 'ImpE
						:preconditions preconditions)))
	     (altr~add-preconditions new-node
				     (altr=make-previous-node (car args)
							      new-node))
	     (list new-node
		   (altr=make-rule-node :formula imp2or ; 'Imp2Or
					:preconditions preconditions
					:no-flip t ; verhindert dritte Praemisse im
						   ; Praemissenbaum - aber was ist, wenn
						   ; ich imp2or im Praemissenbaum will?
					))))
	  (next-node
	   (altr=handle-branch-node! (altr=make-rule-node :formula imp2or
							  :next-node next-node)
				     node))
	  (t (error (sys~make-condition 'altr+tree-building-error))))))



(defun altr=make-next-rule-node (node &key preconditions next-node)
  (declare (edited  "11-NOV-1992 14:35")
	   (authors AFIEDLER)
	   (input   "A rule node and a preconditions list or a next-node list.")
	   (effect  "Creates the next rule nodes in the rule tree by applying the"
		    "according natural deduction rule if possible, and inserts them in"
		    "the rule tree by setting the `preconditions' slots and the"
		    "`next-nodes' slots of NODE and the new nodes.")
	   (value   "If NODE is not a list and if there are next nodes, then a list of"
		    "these nodes, otherwise NIL."))
  (if (not (listp node))
      (let ((formula (node~formula node)))
	(if (term~p formula)
	    (let ((pointer (cond ((altr=no-rule-applicable-p formula)
				  (altr=handle-atom node preconditions next-node))
				 ((logic~negation-p formula)
				  (altr=handle-negation node preconditions next-node))
				 ((logic~universal-quantification-p formula)
				  (altr=handle-universal-quantification
				   node preconditions next-node))
				 ((logic~existential-quantification-p formula)
				  (altr=handle-existential-quantification
				   node preconditions next-node))
				 ((logic~disjunction-p formula)
				  (altr=handle-disjunction node preconditions next-node))
				 ((logic~conjunction-p formula)
				  (altr=handle-conjunction node preconditions next-node))
				 ((logic~equivalence-p formula)
				  (altr=handle-equivalence node preconditions next-node))
				 ((logic~implication-p formula)
				  (altr=handle-implication node preconditions next-node)))))
	      (if preconditions
		  (setf (altr~next-node node) pointer)))
	  (error (sys~make-condition 'alb+term-error :term formula :type 'term)))
	(if preconditions
	    (altr~next-node node)
	  (altr~preconditions node)))))

(defun altr=build-rule-tree-dn (nodes)
  (declare (edited  "29-OCT-1992 09:31")
	   (authors AFIEDLER)
	   (input   "A list of rule nodes.")
	   (effect  "Builds the main conclusion paths of the rule trees, one for each"
		    "node. Each node on a main conclusion path is marked.")
	   (value   "NODES"))
  (if nodes
      (let ((actual-node (car nodes))
	    (node-list (cdr nodes)))
	(altr=mark-main-node actual-node)
	(unless (altr=no-rule-applicable-p (node~formula actual-node))
	  (altr=build-rule-tree-dn
	   (altr=make-next-rule-node actual-node
				     :preconditions (list actual-node))))
	(altr=build-rule-tree-dn node-list)
	nodes)))

(defun altr=build-rule-tree-up (nodes)
  (declare (edited  "02-NOV-1992 16:17")
	   (authors AFIEDLER)
	   (input   "A list of rule nodes.")
	   (effect  "Builds the composition paths, i. e. the side branches of the rule"
		    "tree. That means, for each node a subtree is created with that node"
		    "as the root.")
	   (value   "NODES"))
  (if nodes
      (let ((actual-node (car nodes))
	    (node-list (cdr nodes)))
	(append (unless (altr=no-rule-applicable-p (node~formula actual-node))
		  (altr=build-rule-tree-up
		   (altr=make-next-rule-node actual-node
					     :next-node (list actual-node))))
		(altr=build-rule-tree-up node-list)))))

(defun altr=make-previous-node (formula next-node)
  (declare (edited  "23-JUN-1997" "25-NOV-1992 17:02")
	   (authors Afiedler AFIEDLER)
	   (input   "A formula and a next-node.")
	   (effect  "Builds the next node in a side branch and the subtree of the rule"
		    "tree with that node as root.")
	   (value   "A list with the new node."))
  (let ((new-node (altr=make-rule-node :formula formula
				       :next-node (list next-node))))
    (altr~add-preconditions new-node (altr=build-rule-tree-up (list new-node)))
    (list new-node)))



(defgeneric altr~create-rule-tree (line)
  (declare (edited  "29-OCT-1992 09:32")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Creates the rule tree of the formula in LINE and stores it, if"
		    "necessary.")
	   (value   "The rule node that references the tree, i.e. that node that"
		    "contains the formula in LINE."))
  (:method ((line pdsn+node))
	   (omega~output "~%Creating rule tree ~S " (keim~name line))
	   (let ((rule-tree (altr~create-rule-tree (node~formula line))))
	     (alb~store-tree line rule-tree)
	     (altr~show-cond rule-tree)))
  (:method ((formula term+term))
	   (let ((new-node (altr=make-rule-node :formula formula)))
	     (altr=build-rule-tree-dn (list new-node))
	     new-node)))




(defun altr~useless-p (rule-tree)
  (declare (edited  "25-OCT-1993 11:39")
	   (authors AFIEDLER)
	   (input   "A rule tree.")
	   (effect  "None.")
	   (value   "T, iff the depth of RULE-TREE is not greater than 2."))
  (dolist (root (altr~find-roots rule-tree) t)
    (let ((pre-nodes (altr~preconditions root)))
      (if (when pre-nodes
	    (apply #'append (mapcar #'altr~preconditions pre-nodes)))
	  (return-from altr~useless-p nil)))))

(defun altr~delete-useless-rule-trees ()
  (declare (edited  "13-JUN-1994 16:20")
	   (authors AFIEDLER)
	   (input   "None.")
	   (effect  "Deletes the current proof's rule trees, that are useless,"
		    "and whose hypothesis lines are deletable.")
	   (value   "Undefined."))
  (omega~output "~%~%Deleting useless rule trees")
  (let ((hash-table (alb~rule-tree-hash-table (alb~proof-hash-tables
						pds*current-proof-plan)))) 
    (misc~maphash #'(lambda (key value)
		      (when (and ;(pres=not-deletable-hypothesis-p key)
				 (altr~useless-p (altr~get-node value)))
			(misc~remhash key hash-table)
			(omega~output "~% Deleting rule tree ~A" key)))
		  hash-table)))




;#########################################
;##                                     ##
;##         Praemissen-Baeume           ##
;##                                     ##
;#########################################

(defun altr~make-premise-node (&key formula justification preconditions next-node
				     rule-node line)
  (declare (edited  "18-APR-1994 10:19")
	   (authors AFIEDLER)
	   (input   "Optional the formula of the new node, its justification, its sons,"
		    "its father, an according rule node, and the label of a"
		    "matching proof line.")
	   (effect  "Creates a new premise node with a new name and stores it.")
	   (value   "The new node."))
  (let ((new (make-instance 'altr+premise-node
			    :formula formula
			    :justification justification
			    :preconditions preconditions
			    :next-node next-node
			    :rule-node rule-node
			    :line line)))
    (altr=set-new-name! new)
    (misc~store new)))

(defun altr=make-premise-node-from-rule-node (rule-node next-node &key line (pos t))
  (declare (edited  "13-APR-1994 14:03")
	   (authors AFIEDLER)
	   (input   "A rule node and the next node, and optional a proof line and a"
		    "flag.") 
	   (effect  "Creates a new premise node with a new name with prefix \"PT-\"."
                    "The input rule node is marked visited."
                    "POS should be T, if the formula in the rule node NAME is to be"
		    "written in the `formula' slot of the new premise node, and NIL, if"
		    "the formula in the rule node NAME is to be negated before writing it" 
		    "in the `formula' slot of the new premise node."
                    "LINE should match the formula of the new premise node.")
	   (value   "The new premise node."))
  (altr=set-node-visited! rule-node)
  (altr~make-premise-node :formula (if pos
				       (node~formula rule-node)
				     (alx~negate-formula (node~formula rule-node)))
			  :next-node next-node
			  :rule-node rule-node
			  :line line))

(defun altr=make-next-premise-nodes (premise preconditions &key pos)
  (declare (edited  "08-FEB-1993 10:17")
	   (authors AFIEDLER)
	   (input   "A premise node and a list of rule nodes and an optional flag.")
	   (effect  "Creates for each rule node in PRECONDITIONS a premise node, that"
		    "becomes a son of PREMISE."
                    "POS should be NIL, if the formulae in the new premise nodes should"
		    "be the negated formulae of the rule nodes in PRECONDITIONS."
		    "Otherwise the formulae in the new premise nodes are the same as in"
		    "the rule nodes.")
	   (value   "A list of the names of the new premise nodes."))
  (if premise (altr=set-node-visited! premise))
  (if (and premise preconditions)
      (let ((pre (car preconditions))
	    (pre-list (cdr preconditions)))
	(unless (or (altr=visited-p pre) (altr=no-flip-p pre))
	  (altr~add-preconditions premise
				  (list (altr=make-premise-node-from-rule-node
					 pre
					 premise
					 :pos pos))))
	(altr=make-next-premise-nodes premise pre-list :pos pos)))
  (altr~preconditions premise))

(defun altr=build-premise-tree (premises &key pos)
  (declare (edited  "17-FEB-1993 09:38")
	   (authors AFIEDLER)
	   (input   "A list of premise nodes and an optional flag.")
	   (effect  "For each premise node in the list, a subtree is created with that"
		    "node as the root."
                    "POS should be T, if the subtree corresponds directly to a subtree"
		    "of the according rule tree, and NIL, if it corresponds to a subtree"
		    "of the associated rule tree.")
	   (value   "Undefined."))
  (if premises
      (let* ((pre (car premises))
	     (pre-list (cdr premises))
	     (rule-node (altr~premise-rule-node pre)))
	(if pos
	    (progn
	      (altr=make-next-premise-nodes pre (altr~preconditions rule-node) :pos t)
	      (altr=build-premise-tree (altr~preconditions pre) :pos t)
	      (altr=build-premise-tree pre-list :pos t))
	  (dolist (next (altr~next-node rule-node))
	    (if (not (altr~or-node-p next))
		(altr=make-next-premise-nodes pre (altr~preconditions next) :pos t))
	    (altr=build-premise-tree (altr~preconditions pre) :pos t)
	    (altr=make-next-premise-nodes pre (list next) :pos nil)
	    (altr=build-premise-tree (altr=flippables (altr=not-visited-preconditions
						       pre))
				     :pos nil)
	    (altr=build-premise-tree pre-list :pos nil))))))


(defvar altr*dummy (gensym)
  "A dummy variable to store a premise tree before its line's label is known.")

(defun altr~create-premise-tree (rule-node &key line pos)
  (declare (edited  "10-FEB-1993 17:10")
	   (authors AFIEDLER)
	   (input   "A rule node and optional a proof line and a flag.")
	   (effect  "Creates a premise tree corresponding to the rule tree containing"
		    "RULE-NODE."
                    "POS should be T, if the formula RULE-NODE is to be written in the"
		    "`formula' slot of the root node of the premise tree, and NIL, if"
		    "the formula in RULE-NODE is to be negated before writing in the"
		    "`formula' slot of the root node of the premise tree (the latter"
		    "creates an associated tree)."
                    "LINE should match the formula of the root node of the premise tree.")
	   (value   "The root of the premise tree."))
  (altr=set-all-nodes-unvisited!)
  (let ((premise (altr=make-premise-node-from-rule-node
		  rule-node nil :line line :pos pos)))
    (altr=build-premise-tree (list premise) :pos pos)
    (let* ((premise-wff (node~formula premise))
	   (prems (altr~preconditions premise))
	   (positive-wff (if (alx~double-negation-p premise-wff)
			     (alx~negation-scope (alx~negation-scope premise-wff))))
	   (root (cond ((and (not pos)
			     (alx~double-negation-p premise-wff)
			     (not (data~equal positive-wff (node~formula (car prems)))))
			(setf (altr~premise-line premise) nil)
			(altr~make-premise-node :formula positive-wff
						:preconditions (list premise)
						:line line))
		       (t premise))))
      (alb~store-tree (if line line altr*dummy) root)
      root)))

(defun altr~remove-detours! (node)
  (declare (edited  "19-DEC-1997")
	   (authors Afiedler)
	   (input   "A premise tree root node.")
	   (effect  "Removes detours in the premise tree.")
	   (value   "Undefined."))
  (let ((wff (node~formula node))
	(line (altr~premise-line node)))
    (do ((current node (car pres))
	 (pres (altr~preconditions node) (altr~preconditions current)))
	((not (= (length pres) 1)))
      (let* ((pre (car pres))
	     (pre-line (altr~premise-line pre)))
	(when (and (data~equal (node~formula pre) wff)
		   (or (not pre-line) (not line) (eq pre-line line)))
	  (setf (altr~preconditions node)
		(altr~preconditions pre))
	  (setf (node~justification node)
		(node~justification pre))
	  (if (not line)
	      (setf (altr~premise-line node)
		    (altr~premise-line pre))))))
    (mapc #'altr~remove-detours! (altr~preconditions node))))

(defgeneric altr~update-premise-trees! (line)
  (declare (edited  "03-FEB-1994 09:19")
	   (authors AFIEDLER)
	   (input   "A proof line.")
	   (effect  "Changes the key that refers to the trees to LINE, and sets their"
		    "root nodes' line-slot to LINE.")
	   (value   "Undefined."))
  (:method ((line symbol))
	   (altr~update-premise-trees! (pds~label2node line)))
  (:method ((line pdsn+node))
	   (mapcar #'(lambda (tree)
		       (alb~store-tree line tree)
		       (setf (altr~premise-line tree) line))
		   (altr~get-premise-trees altr*dummy))
	   (alb~remove-tree! altr*dummy)))

(defun altr~valuate-element (nodes)
  (declare (edited  "22-APR-1993 13:33")
	   (authors AFIEDLER)
	   (input   "A list of premise nodes.")
	   (effect  "Valuates each premise node by counting the nodes in their subtrees.")
	   (value   "A list of a-lists, where each premise node is associated with its"
		    "value.")) 
  (append (list nodes)
	  (if (listp nodes)
	      (apply #'+ (mapcar #'(lambda (el)
				    (cdr (last (altr~valuate-element el))))
				nodes))
	    (alb~line-value (altr~premise-line nodes)))))



;#########################################
;##                                     ##
;##      Funktionen fuer Baeume         ##
;##                                     ##
;#########################################

(defun altr=find-roots-of-nodes (nodes)
  (declare (edited  "16-DEC-1992 16:01")
	   (authors AFIEDLER)
	   (input   "A list of rule nodes")
	   (value   "A list of the roots downward of the rule nodes in the according"
		    "rule trees."))
  (if nodes
      (let* ((actual-node (car nodes))
	     (node-list (cdr nodes))
	     (next (altr~next-node actual-node)))
	(append (if next
		    (altr=find-roots-of-nodes next)
		  (list actual-node))
		(if node-list
		    (altr=find-roots-of-nodes node-list))))))

(defgeneric altr~find-roots (node)
  (declare (edited  "27-OCT-1992 17:21")
	   (authors AFIEDLER)
	   (input   "A line label or a rule node refering to a rule tree.")
	   (value   "A list of the roots of the tree."))
  (:method ((node altr+rule-node))
	   (altr=find-roots-of-nodes (list node)))
  (:method ((node symbol))
	   (altr~find-roots (altr~get-node node)))
  (:documentation "Finds the roots of the rule tree referenced by NODE"))

(defgeneric altr~root-p (node)
  (declare (edited  "03-FEB-1994 15:32")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (value   "T, iff NODE is the root of a tree, otherwise NIL."))
  (:method ((node altr+tree-node))
	   (not (altr~next-node node))))

(defgeneric altr~leaf-p (node)
  (declare (edited  "03-FEB-1994 15:36")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (value   "T, iff node is a leaf of a tree, otherwise NIL."))
  (:method ((node altr+tree-node))
	   (not (altr~preconditions node))))

(defgeneric altr~or-node-p (node)
  (declare (edited  "04-JUL-1997"  "24-FEB-1994 15:47")
	   (authors Afiedler AFIEDLER)
	   (input   "A rule node.")
	   (value   "T, iff the formula in NODE is a disjunction in a composition path of"
		    "a rule tree and its preconditions are its subterms, or the formula in"
		    "NODE is an implication in a composition path of a rule tree and its"
		    "preconditions are its subterms or negations thereof."))
  (:method ((node altr+rule-node))
	   (let ((wff (node~formula node)))
	     (and (not (altr~main-p node))
		  (or (logic~disjunction-p wff) (logic~implication-p wff)))))
  (:method ((node altr+premise-node))
           (flet ((memb (a b) (member a b :test #'data~equal-p)))
	     (let* ((wff (node~formula node))
		    (preconditions (altr~preconditions node)))
	       (if (data~appl-p wff)
		   (let ((sub (data~appl-arguments wff)))
					; (or a b)
		     (or (and (logic~disjunction-p wff)
			      (every #'(lambda (pre) (memb (node~formula pre) sub))
				     preconditions))
					; (implies a b)
			 (and (logic~implication-p wff)
			      (every #'(lambda (pre)
					 (let ((formula (node~formula pre)))
					   (or ; 'ImpI
					    (data~equal-p formula (cadr sub))
					    (data~equal-p (alx~negate-formula formula)
							  (car sub))
					    (if (logic~negation-p formula)
						(data~equal-p (alx~negation-scope
							       formula)							      
							      (car sub)))
					; 'Or2Imp 
					    (and (logic~disjunction-p formula)
						 (let ((pre-sub (data~appl-arguments
								 formula)))
						   (or (and (memb (car pre-sub) sub)
							    (memb (alx~negate-formula
								   (cadr pre-sub))
								  sub))
						       (and (memb (cadr pre-sub) sub)
							    (memb (alx~negate-formula
								   (car pre-sub))
								  sub))))))))
				     preconditions))
			 (and (logic~negation-p wff)
					; (not (and a b))
			      (or (and (logic~conjunction-p (car sub))
				       (let ((conj-args (data~appl-arguments (car sub))))
					 (every
					  #'(lambda (pre)
					      (let ((formula (node~formula pre)))
						(and (logic~negation-p formula)
						     (member (alx~negation-scope formula)
							     conj-args
							     :test #'data~equal-p))))
					  preconditions)))
					; (not (equiv a b))
				  (and (logic~equivalence-p (car sub))
				       (let* ((equiv-args (data~appl-arguments (car sub)))
					      (e1 (car equiv-args))
					      (e2 (cadr equiv-args)))
					 (every
					  #'(lambda (pre)
					      (let ((formula (node~formula pre)))
						(and (logic~negation-p formula)
						     (let* ((imp (alx~negation-scope
								  formula))
							    (imp-args (data~appl-arguments
								       imp))
							    (i1 (car imp-args))
							    (i2 (cadr imp-args)))
						       (and (logic~implication-p imp)
							    (or (and (data~equal-p e1 i1)
								     (data~equal-p e2 i2))
								(and (data~equal-p e1 i2)
								     (data~equal-p e2 i1))))))))
					  preconditions))))))))))))

(defun altr=find-or-node-premise (node)
  (declare (edited  "14-JAN-1999")
	   (authors Afiedler)
	   (input   "An or-node or a premise tree.")
	   (value   "The actual premise node of NODE, if it exists, otherwise NIL."))
  (let ((premises (altr~preconditions node))
	(res nil))
    (dolist (pre premises)
      (setq res (cond ((altr~premise-line pre) pre)
		      ((altr=find-or-node-premise pre) pre)))
      (if res (return res))
      )
    #+old(if (and premises (< 1 (length premises)))
	(let ((eins (car premises))
	      (zwei (cadr premises)))
	  (cond ((altr~premise-line eins) eins)
		((altr~premise-line zwei) zwei)
		(t
		 (let ((result (altr=find-or-node-premise eins)))
		   (if result
		       eins
		     (let ((result (altr=find-or-node-premise zwei)))
		       (if result
			   zwei))))))))))

(defun altr~find-or-node-premise (node)
  (declare (edited  "28-MAY-1996")
	   (authors Afiedler)
	   (input   "An or-node of a premise tree.")
	   (value   "The actual premise node of NODE, if it exists, otherwise an error"
		    "is signalled."))
  (cond ((altr=find-or-node-premise node))
	(t (error "Can't find the actual premise of or-node ~A" node))))

#+old(defun altr~find-or-node-premise (node)
  (declare (edited  "28-MAY-1996")
	   (authors Afiedler)
	   (input   "An or-node of a premise tree.")
	   (value   "The actual premise node of NODE."))
  (let* ((premises (altr~preconditions node)))
    (if (and premises (< 1 (length premises)))
	(let ((eins (car premises))
	      (zwei (cadr premises)))
	  (cond ((altr~premise-line eins) eins)
		((altr~premise-line zwei) zwei)
		(t
		 (let ((result (altr~find-or-node-premise eins)))
		   (if result
		       eins
		     (let ((result (altr~find-or-node-premise zwei)))
		       (if result
			   zwei
			 (error "Can't find the actual premise of or-node ~A")))))))))))


(defun altr~tree-size-without-leaves (node)
  (declare (edited  "22-FEB-1994 14:55")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (value   "The number of nodes in the tree with root NAME without the leaves."))
  (if (altr~leaf-p node)
      '0
    (+ 1 (apply #'+ (mapcar #'(lambda (pre)
				(altr~tree-size-without-leaves pre))
			    (altr~preconditions node))))))





;#########################################
;##                                     ##
;##           Baeume zeigen             ##
;##                                     ##
;#########################################

(defun altr=show-branch (nodes)
  (declare (edited  "24-NOV-1992 18:15")
	   (authors AFIEDLER)
	   (input   "A list of rule nodes.")
	   (effect  "Prints the side branches of a rule tree.")
	   (value   "Undefined."))
  (if nodes
      (let* ((actual-node (car nodes))
	     (node-list (cdr nodes))
	     (just (node~justification actual-node)))
	(omega~output "~1&  |~S ~S ~S~%  |   pre: ~S~%  |   next: ~S~%"
		      (keim~name actual-node)
		      (node~formula actual-node)
		      (if just (keim~name (just~method just)))
		      (mapcar #'keim~name (altr~preconditions actual-node))
		      (mapcar #'keim~name (altr~next-node actual-node)))
	(altr=show-branch (altr~preconditions actual-node))
	(if node-list
	    (altr=show-branch node-list)))
    (omega~output "~1&  Leaf~%")))
	
(defun altr=show-main-conclusion-path (nodes)
  (declare (edited  "24-NOV-1992 18:15")
	   (authors AFIEDLER)
	   (input   "A list of rule nodes.")
	   (effect  "Prints the main conclusion path of a rule tree.")
	   (value   "Undefined."))
  (if nodes
      (let* ((actual-node (car nodes))
	     (node-list (cdr nodes))
	     (just (node~justification actual-node)))
	(omega~output "~1&*~S ~S ~S~%*    pre: ~S~%*    next: ~S~%"
		      (keim~name actual-node)
		      (node~formula actual-node)
		      (if just (keim~name (just~method just)))
		      (mapcar #'keim~name (altr~preconditions actual-node))
		      (mapcar #'keim~name (altr~next-node actual-node)))
        (let ((preconditions (cdr (altr~preconditions actual-node))))
	  (if preconditions (altr=show-branch preconditions)))
	(altr=show-main-conclusion-path (altr~next-node actual-node))
	(if node-list
	    (altr=show-main-conclusion-path node-list)))
    (omega~output "~1&Root~%")))

(defun altr=show-premise-tree (nodes)
  (declare (edited  "03-FEB-1993 17:37")
	   (authors AFIEDLER)
	   (input   "A list of premise nodes.")
	   (effect  "Shows the premise trees with the elements of NODES as roots.")
	   (value   "Undefined."))
  (if nodes
      (let* ((actual-node (car nodes))
	     (node-list (cdr nodes))
	     (just (node~justification actual-node))
	     (premise-line (altr~premise-line actual-node)))
	(omega~output "~1& name: ~S ~S ~S~%     Rule node: ~S~%     line: ~A ~S~%     ~
                       pre : ~S~%     next: ~S~%"
		      (keim~name actual-node)
		      (node~formula actual-node)
		      (if just (keim~name (just~method just)))
		      (altr~premise-rule-node actual-node)
		      (if premise-line premise-line)
		      (if premise-line (node~formula premise-line))
		      (mapcar #'keim~name (altr~preconditions actual-node))
		      (mapcar #'keim~name (altr~next-node actual-node)))
	(altr=show-premise-tree (altr~preconditions actual-node))
	(if node-list
	    (altr=show-premise-tree node-list)))
    (omega~output "~1& Leaf~%")))

(defgeneric altr=pprint-nice (obj blanks)
  (declare (edited  "01-FEB-1996")
	   (authors Afiedler)
	   (input   "An object and a string.")
	   (effect  "Pretty-prints OBJ indented by BLANKS.")
	   (value   "Undefined."))
  (:method (obj (blanks string))
	   (write obj))
  (:method ((obj altr+rule-node) (blanks string))
	   (altr=show-main-conclusion-path (list obj)))
  (:method ((obj altr+premise-node) (blanks string))
	   (let* ((name (symbol-name (keim~name obj)))
		  (kids (altr~preconditions obj))
		  (blanks (make-sequence 'string (+ 1 (length blanks))
					 :initial-element #\space)))
	     (write-string name)
	     (write-char #\space)
	     (altr=pprint-nice (node~formula obj) blanks)
	     (when kids
	       (mapc #'(lambda (kid)
			 (terpri)
			 (terpri)
			 (write-string blanks)
			 (altr=pprint-nice kid blanks))
		     kids))))
  )

(pp~modify-style misc-nice
		 (altr+tree-node (lambda (str obj)
			  (let ((*standard-output* str))
			    (altr=pprint-nice obj "")))
			20))

(defgeneric altr=pprint-nice-verbose (obj blks)
  (declare (edited  "01-FEB-1996")
	   (authors Afiedler)
	   (input   "An object and a string of blanks.")
	   (effect  "Writes OBJ verbosely with identation of subsequent lines according"
		    "to BLANKS.")
	   (value   "Undefined."))
  (:method (obj (blanks string))
	   (write obj))
  (:method ((obj just+justification) (blanks string))
	   (pp~pprint obj 'pds-simple))
  (:method ((obj altr+tree-node) (blanks string))
	   (altr=pprint-nice obj blanks))
  (:method ((obj altr+premise-node) (blanks string))
	   (let* ((name (symbol-name (keim~name obj)))
		  (kids (altr~preconditions obj))
		  (line (altr~premise-line obj))
		  (r-node (altr~premise-rule-node obj))
		  (blanks (make-sequence 'string (+ 3 (length blanks))
					 :initial-element #\space)))
	     (write-string (concatenate 'string name
					(if (altr~or-node-p obj)
					    " or-node "
					  " ")
					"(" (if line (symbol-name (keim~name line))) " "
					(if r-node (symbol-name (keim~name r-node))) ") "))
	     (altr=pprint-nice (node~formula obj) blanks)
	     (write-char #\space)
	     (altr=pprint-nice-verbose (node~justification obj) blanks)
	     (if kids
		 (mapc #'(lambda (kid)
			   (terpri)
			   (terpri)
			   (write-string blanks)
			   (altr=pprint-nice-verbose kid blanks))
		       kids)
	       (write-string " Leaf")))))

(pp~defstyle nice-verbose
	     :help "A nice style for printing verbosely"
	     :pprint-methods
	     ((altr+tree-node (lambda (str obj)
		       (let ((*standard-output* str))
			 (altr=pprint-nice-verbose obj "")))
		     20)))


(defun altr~show (tree)
  (declare (edited  "03-FEB-1993 17:53")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (effect  "Prints the tree with NODE as root.")
	   (value   "Undefined."))
  (misc~show tree 'nice-verbose))




(defgeneric altr~show-cond (node)
  (declare (edited  "03-FEB-1993 17:53")
	   (authors AFIEDLER)
	   (input   "A tree node.")
	   (effect  #{Prints the tree with NODE as root, iff {\vb alx*show-premise-tree}#}
                    #{or {\vb alx*show-rule-tree} respectively is T.#})
	   (value   "NODE."))
  (:method ((node altr+rule-node))
	   (when alx*show-rule-tree
	     (altr=show-main-conclusion-path (list node)))
	   node)
  (:method ((node altr+premise-node))
	   (when alx*show-premise-tree
	     (omega~output "~1&Root:")
	     (altr~show node))
	   node)
  (:method ((node symbol))
	   (when alx*show-rule-tree
	     (omega~output "~%~% Rule tree ~S~%" node)
	     (altr=show-main-conclusion-path (list (altr~get-node node))))
	   node)
  (:documentation "Prints the rule tree or the premise tree corresponding to NODE"))

(defun altr~show-all-rule-trees ()
  (declare (edited  "27-JAN-1994 11:47")
	   (authors AFIEDLER)
	   (input   "NONE.")
	   (effect  "Shows all rule trees of the current proof.")
	   (value   "Undefined."))
  (misc~maphash #'(lambda (key val)
		    (declare (ignore key))
		    (altr~show (altr~get-node val)))
		(alb~rule-tree-hash-table (alb~proof-hash-tables pds*current-proof-plan))))





