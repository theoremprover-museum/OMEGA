;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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



(mod~defmod prot :uses (atptop res cl env node just f2p p2f keim prob arg lit pos subst term type post hocnf)
	    :documentation "Calling PROTEIN in omega"
	    :exports (
		      ;; Von den Trees:
		      prot+tree
		      prot~tree-root-node
		      prot~tree-p
		      prot~tree-create

		      prot+tree-node
		      prot~tree-node-clause
		      prot~tree-node-predecessor-node
		      prot~tree-node-predecessor-position
		      prot~tree-node-position-successor-pairs
		      prot~tree-node-red-cuts-pairs
		      prot~tree-node-factor-cuts-triples
		      prot~tree-node-p
		      prot~tree-node-create

		      prot+position-successor-pair
		      prot~position-successor-pair-position
		      prot~position-successor-pair-successor-node
		      prot~position-successor-pair-p
		      prot~position-successor-pair-create

		      prot+red-cuts-pair
		      prot~red-cuts-pair-position
		      prot~red-cuts-pair-path-node
		      prot~red-cuts-pair-p
		      prot~red-cuts-pair-create

		      prot+factor-cuts-triple
		      prot~factor-cuts-triple-position
		      prot~factor-cuts-triple-factor-node
		      prot~factor-cuts-triple-factor-position
		      prot~factor-cuts-triple-p
		      prot~factor-cuts-triple-create
		      
		      prot~print-node


		      ;; Aufruf Protein
		      prot~program
		      prot~call-protein
		      prot~generate-protein-problem
		      prot~add-in-string!
		      prot~complete-protein-problem!
		      prot~generate-protein-problem-default!
		      prot~read-protein-output
		      prot~complete-protein-problem-from-file!
		      ))





#| The translation algorithm consists of two main steps.
   First the protein.tree is translated into a tree-object, that is an exact object-copy of the syntactical tree in the protein.tree
   file. The date structures needed for this tree are desribed below.
   Second this tree is translated into a resolution proof. If we look at the protein manual we see, that the tree is a refutation tree
   with three sort of links from one literal to an other. First simple successor links from a literal to a contradictinal literal,
   second factor links from one literal to a factorable literal and third reduction links from a literal to a contradictinal literal
   earlier in the path. The simple successor links are translated by resolution steps and the other two link-sorts are translated by
   factoring-steps (how and wy is described below), so that we get as result a resolution proof, consisting of binary factoring and
   binary resolution steps.
|#



#| -------------------------------------------------- Some global Variables -------------------------------------------------------- |#


(defvar prot*convert-counter nil)                      ;; zaehlt die converierten Objecte mit
(defvar prot*temporary-variables nil)                  ;; temporaere Variablen
(defvar prot*just-counter nil)                         ;; zaehlt die neuen Justifications (Resolution, Factoring) mit
(defvar prot*current-problem nil)                      ;; hier wird immer das momentane Problem gespeichert
(defvar prot*convert-list nil)                         ;; in dieser LIste werden die Converierten Namen und Object gespeichert
(defvar prot*number-clause-list nil)                   ;; enthaelt die geparsten Klauseln mit entsprechenden Nummern
(defvar prot*rest-initial-clauses nil)                 ;; enthalt waehrend des Pasrens die noch nicht aus dem Beweis geparsten (und dann
                                                       ;; mit entsprechenden Nummer versehenen) initial Klauseln
(defvar prot*local-clause-vars nil)                    ;; enthalt die Variablen, die Nur in einer kLausel actuell sind
(defvar prot*current-environment nil)                  ;; the current environment

(defvar prot*in-string nil)                            ;; the current in-string is stored

(defvar prot*name-symbols '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
			   #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
			   #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
			   #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
			   #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
			   #\8 #\9 #\_))               ;; Die Buchstaben, die in einem Protein Namen erlaubt sind


(defun prot~program ()
  (let* ((prog (sys~getenv "PROTEINHOME")))  ;; Binary des Protein Beweisers

    (when (or (null prog) (null (probe-file prog)))
      (error "There is no PROTEIN-executable at ~A, please check your path to the PROTEIN-executable." prog))
    
    prog))


#| ------------------------------------------------------- Trees -------------------------------------------------------------- |#

#| The following are data-structures to build Refutataion Tree, like produced from protein in the .tree Output file. Each node consists
   of a clause. One literal of the clause is linked with the predecessor (if the node is not the root of the tree) and each other literal
   is either itself linked with a successor node's clause, or it is linked by a factoring_cut with an other literal or it is linked
   by a reduction_cut with an other literal. |#

;; ------------------------------------------------ trees

(defclass prot+tree ()
  ((root-node :initarg :root-node
	      :initform nil
	      :accessor prot~tree-root-node
	      :documentation "the root-node of the tree.")))

(defun prot~tree-p (thing)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is a prot+tree, otherwise nil."))
  (typep thing 'prot+tree))

(defmethod print-object ((tree prot+tree) stream)
  (format stream "Tree with Root-node ~A" (prot~tree-root-node tree)))

(defun prot~tree-create (root-node)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A tree with this node as root-node."))
  (make-instance 'prot+tree
		 :root-node root-node))

;; ----------------------------------------------- Tree nodes

(defclass prot+tree-node ()
  ((clause :initarg :clause
	   :initform nil
	   :accessor prot~tree-node-clause
	   :documentation "The clause of an node.")
   (clause-position :initarg :clause-position
		    :initform nil
		    :accessor prot~tree-node-clause-position
		    :documentation "The position of the literal, that is connected with the predecessor node.")
   (predecessor-node :initarg :predecessor-node
		     :initform nil
		     :accessor prot~tree-node-predecessor-node
		     :documentation "The predecessor node.")
   (predecessor-position :initarg :predecessor-position
			 :initform nil
			 :accessor prot~tree-node-predecessor-position
			 :documentation "The position of the literal in the predecessor, that is connected with this node.")
   (position-successor-pairs :initarg :position-successor-pairs
			     :initform nil
			     :accessor prot~tree-node-position-successor-pairs
			     :documentation "The list of the position-successor-pairs. Each literal, except the literal that is connected with the predecessor, the reduction_cut literals and the factor_cut literals, are connected with a successor node. The list of the pairs of the positions of this literals and its successor nodes are stored here.")
   (red-cuts-pairs :initarg :red-cuts-pairs
		   :initform nil
		   :accessor prot~tree-node-red-cuts-pairs
		   :documentation "A list of red-cuts-pairs: For each literal, that is linked by reduction_cut this slot contains a red-cuts pairs: FIrst the position of the literal in this clause, second the tree-node of the corresponding contradictional literal.")
   (factor-cuts-triples :initarg :factor-cuts-triples
			:initform nil
			:accessor prot~tree-node-factor-cuts-triples
			:documentation "A list of Factor-cuts-triples: For each literal, that is linked by a factor_cut this slot contains a factor-cuts-triple: First the position of the literal, second the node , that contain the literal this literal is linked with and third the position of this literal."))) 

(defun prot~tree-node-p (thing)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type prot+tree-node, nil otherwise."))
  (typep thing 'prot+tree-node))


(defmethod print-object ((tree-node prot+tree-node) stream)
  (format stream "<Node with clause ~A>" (prot~tree-node-clause tree-node)))

(defun prot~tree-node-create (clause clause-position predecessor-node predecessor-position
				     position-successor-pairs red-cuts-pairs factor-cuts-triples)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, a position in this clause, a node a, position in the clause of this node,"
		    "a list of position-successor-pairs, a list of red-cuts-pairs and a list of factor-cuts-triples.")
	   (effect  "None.")
	   (value   "A new object of type prot+tree-node"))
  (make-instance 'prot+tree-node
		 :clause clause
		 :clause-position clause-position
		 :predecessor-node predecessor-node
		 :predecessor-position predecessor-position
		 :position-successor-pairs position-successor-pairs
		 :red-cuts-pairs red-cuts-pairs
		 :factor-cuts-triples factor-cuts-triples))


;; --------------------------------------- position successor pair

(defclass prot+position-successor-pair ()
  ((position :initarg :position
	     :initform nil
	     :accessor prot~position-successor-pair-position
	     :documentation "The position of a position successor pair. That is the position of the literal in the according clause, that is linked with an successor node.")
   (successor-node :initarg :successor-node
		   :initform nil
		   :accessor prot~position-successor-pair-successor-node
		   :documentation "The successor node of a position successor pair. That is the according successor node.")))

(defun prot~position-successor-pair-p (thing)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type prot+position-successor-pair, nil otherwise."))
  (typep thing 'prot+position-successor-pair))

(defmethod print-object ((thing prot+position-successor-pair) stream)
  (format stream "Position-Successor-Pair: Position ~A with Node ~A"
	  (prot~position-successor-pair-position thing)
	  (prot~position-successor-pair-successor-node thing)))

(defun prot~position-successor-pair-create (position successor-node)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A position and a node.")
	   (effect  "None.")
	   (value   "A new object of type prot+position-successor-pair."))
  (make-instance 'prot+position-successor-pair
		 :position position
		 :successor-node successor-node))

;; --------------------------------------- red-cuts-pair 
	  
(defclass prot+red-cuts-pair ()
  ((position :initarg :position
	     :initform nil
	     :accessor prot~red-cuts-pair-position
	     :documentation "The position of a red cuts pair. That is the position of the literal in the according clause, that is linkek by a reduction-cut with a contradictonal literal in the path.")
   (path-node :initarg :path-node
	      :initform nil
	      :accessor prot~red-cuts-pair-path-node
	      :documentation "The path node of a red cuts path node. The node that contains the contradictional path node.")))
#| Remark: Possible contradictional literals for reduction-cuts are only such literals, that are the connection nodes (the literals
           signed by the position of a position-successor-pair) in the path from root to the current node. |#

(defun prot~red-cuts-pair-p (thing)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type prot+red-cuts-pair, nil otherwise."))
  (typep thing 'prot+red-cuts-pair))

(defmethod print-object ((thing prot+red-cuts-pair) stream)
  (format stream "Red Cuts PAir: Position ~A with Node ~A"
	  (prot~red-cuts-pair-position thing)
	  (prot~red-cuts-pair-path-node thing)))

(defun prot~red-cuts-pair-create (position path-node)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A position and a node.")
	   (effect  "None.")
	   (value   "A new object of type prot+red-cuts-pair."))
  (make-instance 'prot+red-cuts-pair
		 :position position
		 :path-node path-node))

;; --------------------------------------- factor-cuts-triple

(defclass prot+factor-cuts-triple ()
  ((position :initarg :position
	     :initform nil
	     :accessor prot~factor-cuts-triple-position
	     :documentation "The position of a factor cuts triple. This is the position of a literal in the current node's clause.")
   (factor-node :initarg :factor-node
		:initform nil
		:accessor prot~factor-cuts-triple-factor-node
		:documentation "The factor node of a factor cuts triple. This is the node, that contains the according other literal")
   (factor-position :initarg :factor-position
		    :initform nil
		    :accessor prot~factor-cuts-triple-factor-position
		    :documentation "The factor position of a factor cuts triple. This is the position of the other factorable literal."
		    )))
#| Remark: As factor-cuts literals are only such literals possible, that stand 'before' the current literal. That means literals, that
           are, if we look at the tree with the root above, left and higher as the literal (compare with the protein manual). |#

(defun prot~factor-cuts-triple-p (thing)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type prot+factor-cuts-triple, nil otherwise."))
  (typep thing 'prot+factor-cuts-triple))

(defmethod print-object ((thing prot+factor-cuts-triple) stream)
  (format stream "Factor-Cuts Triple: Position ~A with Node ~A Position ~A"
	  (prot~factor-cuts-triple-position thing)
	  (prot~factor-cuts-triple-factor-node thing)
	  (prot~factor-cuts-triple-factor-position thing)))

(defun prot~factor-cuts-triple-create (position factor-node factor-position)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A position, a node and a position of the clause of this node.")
	   (effect  "None.")
	   (value   "A new object of type prot+factor-cuts-triple."))
  (make-instance 'prot+factor-cuts-triple
		 :position position
		 :factor-node factor-node
		 :factor-position factor-position))


;; ---------------------------------------------- Print

(defun prot~print-node (tree-node)
  (omega~message "<Node with Clause ~A, connected with its predecessor at position ~A."
	  (prot~tree-node-clause tree-node)
	  (prot~tree-node-clause-position tree-node))
  (omega~message "~%Successors at:") 
  (mapcar #'(lambda (pos-succ-pair)
	      (omega~message "~%At position ~A with node"
		      (prot~position-successor-pair-position pos-succ-pair))
	      (prot~print-node (prot~position-successor-pair-successor-node pos-succ-pair)))
	  (prot~tree-node-position-successor-pairs tree-node))
  (omega~message "~%Cuts at:")
  (mapcar #'(lambda (red-cuts-pair)
	      (omega~message "~%~A" red-cuts-pair))
	  (prot~tree-node-red-cuts-pairs tree-node))
  (omega~message "~%factor-cuts at:")
  (mapcar #'(lambda (factor-cuts-triple)
	      (omega~message "~%~A" factor-cuts-triple))
	  (prot~tree-node-factor-cuts-triples tree-node))
  (omega~message ">"))


#| ---------------------------------------------------------- BUILD TREES --------------------------------------------------------- |#


#| Remark: Build-tree-algorithm:
           First the protein.tree file is read. The query -clause is taken to construct the root-node.
           The output of the protein.tree file is always in the a left-to-right order. That means, that the next line in the protein.tree
           File is automaltically the link for the next left-most-unlinked literal. If a new clause is inserted, the remaining unlinked
           Literals of this clause get the next left-most-literals and so on. That means, that the hole tree is constructed from left to
           right.
           To repaet to remarks from above:
           As factor-cuts literals are only such literals possible, that stand 'before' the current literal. That means literals, that
           are, if we look at the tree with the root above, left and higher as the literal (compare with the protein manual).
           Possible contradictional literals for reduction-cuts are only such literals, that are the connection nodes (the literals
           signed by the position of a position-successor-pair) in the path from root to the current node.
           The according literals for a factor-cut or a reduction cut are signed by numbers. This numbers give the amount of literals
           you have to go back, to found the right literal.
           Additionally it should be remarked, that negativ and positiv literals are counted separately for the the factor-cuts and the
           reduction cuts. This means, that if you have for example a positive literal, that is linked by a reduction cut with
           justification count-number 2, you get the accoridng contradictional literal by counting only the negativ path literals back.
|#
           


(defun prot=build-tree (query-clause tree-lines)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "The query clause and a list of strings, representing the contents of the"
		    "protein.tree file.")
	   (effect  "None.")
	   (value   "The tree constructed from the protein.tree file."))
  (let* ((root-tree-node (prot~tree-node-create query-clause nil nil nil nil nil nil)))
    (do* ((rest-literals (cl~literals query-clause) (rest rest-literals))
	  (rest-tree-lines tree-lines)
	  (num 0 (+ num 1)))
	((null rest-literals)
	 nil)
      (let* ((position (pos~list-position (list num)))
	     (new-rest-tree-lines (prot=build-tree-under! root-tree-node position rest-tree-lines)))
	(setq rest-tree-lines new-rest-tree-lines)))
    
    (prot~tree-create root-tree-node)
    ;; (setq global-tree (prot~tree-create root-tree-node))
    ;; (omega~error "alskdalksdh")
    ))

(defun prot=build-tree-under! (predecessor-node predecessor-position tree-lines) 
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A node, a position of a literal in its clause and a set of tree-lines.")
	   (effect  "The tree under this literal is created and between this perhaps the"
		    "entries of the node are changed.")
	   (value   "The remaining rest-lines."))
  (multiple-value-bind
      (just-string number1 number2)
      (prot=split-tree-information (first tree-lines))
    (cond ((string= just-string "ext")
	   (let* ((next-clause (prot=number2clause number1))
		  (pos-number (- number2 1))
		  (clause-position (pos~list-position (list pos-number)))
		  (new-tree-node (prot~tree-node-create next-clause clause-position predecessor-node predecessor-position nil nil nil)))
	     ;; update predecessor-node 
	     (prot=update-tree-node-position-successor-pairs! predecessor-node predecessor-position new-tree-node)

	     (do* ((num 0 (+ num 1))
		   (rest-literals (cl~literals next-clause) (rest rest-literals))
		   (rest-tree-lines (rest tree-lines)))
		 ((null rest-literals)
		  rest-tree-lines)
	       (if (= num pos-number)
		   ;; literal bereits mit predecessor verknuepft
		   nil
		 (let* ((position (pos~list-position (list num)))
			(new-rest-tree-lines (prot=build-tree-under! new-tree-node position rest-tree-lines)))
		   (setq rest-tree-lines new-rest-tree-lines))))))
	  ((string= just-string "fact")
	   (let* ((next-clause (prot=number2clause number1))
		  (pos-number (- number2 1))
		  (clause-position (pos~list-position (list pos-number)))
		  (new-tree-node (prot~tree-node-create next-clause clause-position predecessor-node predecessor-position nil nil nil)))
	     ;; update predecessor-node 
	     (prot=update-tree-node-position-successor-pairs! predecessor-node predecessor-position new-tree-node)
	     (rest tree-lines)))
	  ((or (string= just-string "red_cut")
	       (string= just-string "red"))
	   (let* ((cut-node (prot=compute-red-cuts-node predecessor-node predecessor-position number1)))
	     (prot=update-tree-node-red-cuts-pairs! predecessor-node predecessor-position cut-node)
	     (rest tree-lines)))
	  ((string= just-string "factor_cut")
	   (multiple-value-bind
	       (fac-node fac-position)
	       (prot=compute-fac-node-and-position predecessor-node predecessor-position number1)
	     (prot=update-tree-node-factor-cuts-triples! predecessor-node predecessor-position fac-node fac-position)
	     (rest tree-lines)))
	  (t
	   (omega~error "Unknown just: ~A" just-string)))))

(defun prot=compute-red-cuts-node (node position sub-number)
  (declare (edited  "24-OCT-1997")
	   (authors Ameier)
	   (input   "A node, a position in that node and a number.")
	   (effect  "None.")
	   (value   "Goes back the path from the node as many numbers as in the input described,"
		    "but only such nodes are counted, that literal have a contradictional polarity"
		    "to the literal of the input clause at the input position."))
  (let* ((literal-at-pos (data~struct-at-position (cl~literals (prot~tree-node-clause node)) position))
	 (pol (lit~positive-p literal-at-pos)))
    (do* ((last-node node (prot~tree-node-predecessor-node last-node))
	  (current-node (prot~tree-node-predecessor-node node) (prot~tree-node-predecessor-node current-node))
	  (rest-sub-number sub-number))
	((and (= 1 rest-sub-number)
	      (not (equal pol (lit~positive-p (data~struct-at-position (prot~tree-node-clause current-node)
								(prot=position-successor-pair-to-node-position current-node last-node))))))
	 current-node)
      (let* ((path-literal (data~struct-at-position (prot~tree-node-clause current-node)
					     (prot=position-successor-pair-to-node-position current-node last-node)))
	     (contra (not (equal pol (lit~positive-p path-literal)))))
	(when contra
	  (setq rest-sub-number (- rest-sub-number 1)))
	))))



(defun prot=position-successor-pair-to-node-position (current-node successor-node)
  (declare (edited  "24-OCT-1997")
	   (authors Ameier)
	   (input   "A node and a node, who is among the successors of the first node.")
	   (effect  "None.")
	   (value   "The position of the position-successor-pair of the first-node, that"
		    "describes the path to the successor-node."))  
  (prot~position-successor-pair-position (find successor-node (prot~tree-node-position-successor-pairs current-node)
					       :test #'(lambda (item1 item2)
							 (eq item1 (prot~position-successor-pair-successor-node item2))))))

  
(defun prot=compute-fac-node-and-position (node position sub-num)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A node, a position and a number.")
	   (effect  "None.")
	   (value   "Multiple-Value:"
		    "First the node and second the position in the clause of the factoring."))
  (let* ((literal (data~struct-at-position (cl~literals (prot~tree-node-clause node)) position))
	 (pol (lit~positive-p literal)))
    (do* ((current-node node)
	  (current-start-position position)
	  (rest-sub-num sub-num)
	  (back-position nil))
	(back-position
	 (values current-node back-position))
      (let* ((current-start-num (pos~first current-start-position))
	     (current-clause (prot~tree-node-clause current-node))
	     (current-literals (cl~literals current-clause))
	     (current-connection-position (prot~tree-node-clause-position current-node))
	     (current-connection-literal (if current-connection-position
					     (data~struct-at-position current-literals current-connection-position)
					   nil))
	     (current-literals-smaller-as-current-start-position-and-without-connection-literal
	      (remove current-connection-literal (do* ((i 0 (+ i 1))
						       (rest-literals current-literals (rest rest-literals))
						       (back-literals nil))
						     ((= i current-start-num)
						      back-literals)
						   (setq back-literals (append back-literals (list (first rest-literals)))))))
	     (lits-of-eq-pol (remove-if-not #'(lambda (lit)
						(equal (lit~positive-p lit) pol))
					    current-literals-smaller-as-current-start-position-and-without-connection-literal)))
	
	(if (< (length lits-of-eq-pol) rest-sub-num)
	    (progn
	      (setq rest-sub-num (- rest-sub-num (length lits-of-eq-pol)))
	      (setq current-start-position (prot~position-successor-pair-position
					    (find current-node
						  (prot~tree-node-position-successor-pairs (prot~tree-node-predecessor-node current-node))
						  :test #'(lambda (item1 item2)
							    (eq item1 (prot~position-successor-pair-successor-node item2))))))
	      (setq current-node (prot~tree-node-predecessor-node current-node)))
	  (let* ((needed-literal (do* ((rest-subbi rest-sub-num (- rest-subbi 1))
				       (rest-literals (reverse lits-of-eq-pol)
						      (rest rest-literals)))
				     ((= rest-subbi 1)
				      (first rest-literals)))))
	    (setq back-position (pos~list-position (list (position needed-literal (cl~literals current-clause)))))))))))

;; res2ref=position-of-literal-in-clause needed-literal current-clause))))))))
			      
  
  
(defun prot=update-tree-node-factor-cuts-triples! (node position factor-node factor-position)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A node and a position an facto-node and the factor-position.")
	   (effect  "The factor-cuts-triples of the node are updated by the pair of this two positions.")
	   (value   "None."))
  (setf (prot~tree-node-factor-cuts-triples node)
	(cons (prot~factor-cuts-triple-create position factor-node factor-position)
	      (prot~tree-node-factor-cuts-triples node))))




(defun prot=update-tree-node-red-cuts-pairs! (node position cut-node)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A tree node, a position of a literal of its clause and a second tree-node.")
	   (effect  "The red-cuts-pairs slot of the node is updated by a pair"
		    "(position cut-node). That means, that the literal at the position is"
		    "connected with a literal of the clause of the cut-node (the position of this"
		    "literal is stored in the successor node as clause-position).")
	   (value   "Undefinded."))
  (setf (prot~tree-node-red-cuts-pairs node)
	(cons (prot~red-cuts-pair-create position cut-node)
	      (prot~tree-node-red-cuts-pairs node))))


(defun prot=update-tree-node-position-successor-pairs! (node position successor-node)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A tree node, a position of a literal of its clause and a second tree-node.")
	   (effect  "The position-sucessor-pairs slot of the node is updated by a pair"
		    "(position successor-node). That means, that the literal at the position is"
		    "connected with a literal of the clause of the sucessor node (the position of this"
		    "literal is stored in the successor node as clause-position).")
	   (value   "Undefinded."))
  (setf (prot~tree-node-position-successor-pairs node)
	(cons (prot~position-successor-pair-create position successor-node)
	      (prot~tree-node-position-successor-pairs node))))

(defun prot=split-tree-information (tree-line)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A string, that represents an entry of the protein.tree file.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: the just of the line (ext,red_cut,fact)"
		    "Second: The first number in th info string."
		    "Third: The second number in the info string."))
  (let* ((divided-string-list (atptop~divide-string tree-line #\, :ignore-char-list (list #\space #\))))
	 (after-info-string-list (member "info(" divided-string-list
					 :test #'(lambda (string1 string2)
						   (and (equal (char string1 0) (char string2 0))
							(equal (char string1 1) (char string2 1))
							(equal (char string1 2) (char string2 2))
							(equal (char string1 3) (char string2 3))
							(equal (char string1 4) (char string2 4)))))))
    (values
     (second after-info-string-list)
     (atptop~parse-number (third after-info-string-list))
     (atptop~parse-number (fourth after-info-string-list)))))

#| ----------------------------------------------------------- Build resolution proof ---------------------------------------------- |#

#| Remark: About the algorithm, that builds a resolution proof from a tree
           After the tree was created directly from the protein.tree file, this tree has to be translated into a resolution proof.
           Therefor each position-successor-pair-link is translated by an according resolution step, and each reduction cut and
           factor-cut link is translated by an factoring step.
           The translation of a position-successor-pair link as resolution and the translation of a factor-cut link as factoring step
           should be clear.
           A reduction cut link could be also translated by a factoring step, because, the linked literal can be factored with the
           literal, that is linked by a position-successor-pair with the contradictional path literal.
           The things become somewhat complicated, because the factor-cut and reduction-cut links can be not only to other literals in
           the same clause, but to (more or less) each literal that is above and left of the current literal (because the tree is build
           from left-to-right, look at comments to the build-tree-algorithm). Therefor we have to translate the tree into a
           resolution proof from right-to-left (exactly contrary), because otherwise it could be happens, that a literal is already
           deleted by resolution or factoring, but an other literal (right and below in the tree) has to be factored with it.
|#

(defun prot=resolve-under! (tree-node res-proof)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A tree-node and the current-resolution proof.")
	   (effect  "Computes the resolution proof under this node, so the resolution proof is changed by new steps.")
	   (value   "Multiple-Value-bind:"
		    "First: A clause, that is the result-clause of the resolution proof under this tree-node."
		    "Second: The position of the literal in the returnded clause, that was connected with"
		    "        the predecessor."
		    "Third: A list of pairs of positions and tree-nodes. The position stands for a literal"
		    "       in the result-clause that was connected by red_cut, and the tree-node is the"
		    "       node, in that this literal has to be resolved."
		    "Fourth: A list of the remaining factor-cuts-triples."))
  (let* ((position-successor-pairs (prot~tree-node-position-successor-pairs tree-node))
	 (red-cuts-pairs (prot~tree-node-red-cuts-pairs tree-node))
	 (factor-cuts-triples (prot~tree-node-factor-cuts-triples tree-node))
	 (clause (prot~tree-node-clause tree-node))
	 (connection-position (prot~tree-node-clause-position tree-node))
	 (all-steps (append position-successor-pairs red-cuts-pairs factor-cuts-triples))
	 (ordered-steps (do* ((num 0 (+ num 1))
			      (back-steps nil))
			    ((= num (length (cl~literals clause)))
			     back-steps)
			  (let* ((next-step (find (pos~list-position (list num)) all-steps
						  :test #'(lambda (item1 item2)
							    (cond ((prot~position-successor-pair-p item2)
								   (keim~equal item1 (prot~position-successor-pair-position item2)))
								  ((prot~red-cuts-pair-p item2)
								   (keim~equal item1 (prot~red-cuts-pair-position item2)))
								  ((prot~factor-cuts-triple-p item2)
								   (keim~equal item1 (prot~factor-cuts-triple-position item2))))))))
			    (setq back-steps (cons next-step back-steps))))))
    (multiple-value-bind
	(remaining-clause
	 remaining-connection-position
	 remaining-red-cuts-pairs
	 remaining-factor-cuts-triples)
	(prot=make-all-steps clause
			     tree-node
			     connection-position
			     ordered-steps        ;; RUECKWAERTS GEORDNET !!!
			     res-proof)
      (values remaining-clause
	      remaining-connection-position
	      remaining-red-cuts-pairs
	      remaining-factor-cuts-triples))))

					      
(defun prot=make-all-steps (clause
			    tree-node
			    connection-position
			    ordered-steps    ;; RUECKWAERTS GEORDNET !!!
			    res-proof)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, its according tree-node, the position of the literal the node is connected"
		    "with its predecessor, a list of all links (position-successor-pairs, reduction-cuts and"
		    "factor-triple) of the remaining literals (without the literal"
		    "that is linked with the predecessor-node) in the order from right to left (that means"
		    "the link with the highest position is the first and so on) and the current resolution proof.")
	   (effect  "Computes from left to right the resolution proof under this clause/node. The resolution proof"
		    "is updated by new inserted steps.")
	   (value   "Multiple-Value:"
		    "First: The clause, that is the result of the resolution steps under this node and the possible"
		    "       factor-cuts and reduction-cuts."
		    "Second: The position of the literal in the new clause (first), that is connected with the"
		    "        predecessor node."
		    "Third: The remaining reduction-cuts."
		    "Fourth: The remaining factor-cuts."
		    "Remark: The remaining reduction and factor cuts contain all (updated to positions of the new"
		    "        clause, look first) reduction and factor cuts, that refer to nodes, that are left"
		    "        and above of the current nodes (that means nodes, that are not already worked off)."))
  (do* ((rest-ordered-steps ordered-steps (rest rest-ordered-steps)) 
	;; man beachte: ist bereits in umgekehrter Reihenfolge
	(current-clause clause)
	(current-connection-position connection-position)
	(current-remaining-red-cuts-pairs nil)
	(current-remaining-factor-cuts-triples nil))
      ((null rest-ordered-steps)
       (values current-clause
	       current-connection-position
	       current-remaining-red-cuts-pairs
	       current-remaining-factor-cuts-triples))
    (let* ((next-step (first rest-ordered-steps)))
      (cond ((prot~position-successor-pair-p next-step)
	     (multiple-value-bind
		 (new-current-clause
		  new-current-connection-position
		  new-current-remaining-red-cuts-pairs
		  new-current-remaining-factor-cuts-triples)
		 (prot=work-on-position-successor-pair next-step
						       tree-node
						       current-clause
						       current-connection-position
						       current-remaining-red-cuts-pairs
						       current-remaining-factor-cuts-triples
						       res-proof)
	       (setq current-clause new-current-clause)
	       (setq current-connection-position new-current-connection-position)
	       (setq current-remaining-red-cuts-pairs new-current-remaining-red-cuts-pairs)
	       (setq current-remaining-factor-cuts-triples new-current-remaining-factor-cuts-triples)))
	    ((prot~red-cuts-pair-p next-step)
	     (multiple-value-bind
		 (new-current-clause
		  new-current-connection-position
		  new-current-remaining-red-cuts-pairs
		  new-current-remaining-factor-cuts-triples)
		 (prot=work-on-red-cuts-pair next-step
					     tree-node
					     current-clause
					     current-connection-position
					     current-remaining-red-cuts-pairs
					     current-remaining-factor-cuts-triples
					     res-proof)
	       (setq current-clause new-current-clause)
	       (setq current-connection-position new-current-connection-position)
	       (setq current-remaining-red-cuts-pairs new-current-remaining-red-cuts-pairs)
	       (setq current-remaining-factor-cuts-triples new-current-remaining-factor-cuts-triples)))
	    ((prot~factor-cuts-triple-p next-step)
	     (multiple-value-bind
		 (new-current-clause
		  new-current-connection-position
		  new-current-remaining-red-cuts-pairs
		  new-current-remaining-factor-cuts-triples)
		 (prot=work-on-factor-cuts-triple next-step
						  tree-node
						  current-clause
						  current-connection-position
						  current-remaining-red-cuts-pairs
						  current-remaining-factor-cuts-triples
						  res-proof)
	       (setq current-clause new-current-clause)
	       (setq current-connection-position new-current-connection-position)
	       (setq current-remaining-red-cuts-pairs new-current-remaining-red-cuts-pairs)
	       (setq current-remaining-factor-cuts-triples new-current-remaining-factor-cuts-triples)))))))


(defun prot=work-on-red-cuts-pair (red-cuts-pair
				   tree-node
				   clause
				   connection-position
				   remaining-red-cuts-pairs
				   remaining-factor-cuts-triples
				   res-proof)
  (declare (edited  "30-OCT-1997")
	   (authors Ameier)
	   (input   "A red-cuts-pair, the current node, the current clause (in general not identical to the clause"
		    "of the node, but the current result of working off the literals of this clause from right to"
		    "left), the position of the literal in this clause, that is connected with the predecessor node"
		    "(also in general not identical to the clause-position in the node, but the current result of"
		    "working off the literals from right to left and between this precess the position to the"
		    "predecessor node can be changed) and the remaining reduction-cuts and factor-cuts.")
	   (effect  "If possible the reduction-cut is translated by a factoring step (possible means, that the according"
		    "factorable literal has to be in the clause, or to say it in an other way, that the reduction-node"
		    "is the predecessor-node of the current node, because in this case the literal described by the"
		    "predecessor-connection position is factorable with the current looked at literal.")
	   (value   "Multiple-Value:"
		    "First: The resulting clause (the input clause if the reduction cut was not possible to translate"
		    "       the result of the factoring step otherwise)."
		    "Second: The position of the literal in the new clause (first), that is connected with the"
		    "        predecessor node."
		    "Third: The remaining reduction-cuts."
		    "Fourth: The remaining factor-cuts."
		    "Remark: The remaining reduction and factor cuts contain all (updated to positions of the new"
		    "        clause, look first) reduction and factor cuts, that refer to nodes, that are left"
		    "        and above of the current nodes (that means nodes, that are not already worked off)."))
  (if (prot=red-cuts-pair-usable-p red-cuts-pair tree-node)
      (multiple-value-bind
	  (clause-after-red-cuts-update
	   connection-position-after-red-cuts-update
	   remaining-red-cuts-pairs-after-red-cuts-update
	   remaining-factor-cuts-triples-after-red-cuts-update)
	  (prot=insert-red-cuts clause
				connection-position
				tree-node
				(cons red-cuts-pair remaining-red-cuts-pairs)
				remaining-factor-cuts-triples
				res-proof)
	(values clause-after-red-cuts-update
		connection-position-after-red-cuts-update
		remaining-red-cuts-pairs-after-red-cuts-update
		remaining-factor-cuts-triples-after-red-cuts-update))
    (values clause
	    connection-position
	    (cons red-cuts-pair remaining-red-cuts-pairs)
	    remaining-factor-cuts-triples)))

(defun prot=work-on-factor-cuts-triple (factor-cuts-triple
					tree-node
					clause
					connection-position
					remaining-red-cuts-pairs
					remaining-factor-cuts-triples
					res-proof)
  (if (prot=factor-cuts-triple-usable-p factor-cuts-triple tree-node)
      (multiple-value-bind
	  (clause-after-factor-triple-update
	   connection-position-after-factor-triple-update
	   remaining-red-cuts-pairs-after-factor-triple-update
	   remaining-factor-cuts-triples-after-factor-triple-update)
	  (prot=insert-factor-cuts clause
				   connection-position
				   tree-node
				   remaining-red-cuts-pairs
				   (cons factor-cuts-triple remaining-factor-cuts-triples)
				   res-proof)
	(values clause-after-factor-triple-update
		connection-position-after-factor-triple-update
		remaining-red-cuts-pairs-after-factor-triple-update
		remaining-factor-cuts-triples-after-factor-triple-update))
    (values clause
	    connection-position
	    remaining-red-cuts-pairs
	    (cons factor-cuts-triple remaining-factor-cuts-triples))))

(defun prot=red-cuts-pair-usable-p (red-cuts-pair tree-node)
  (declare (edited  "23-OCT-1997")
	   (authors Ameier)
	   (input   "A red-cuts-pair and a tree-node.")
	   (effect  "None.")
	   (value   "T if the path node of the red-cuts-pair is eq to the"
		    "predecessor node of the tree-node."))
  (eq (prot~red-cuts-pair-path-node red-cuts-pair)
      (prot~tree-node-predecessor-node tree-node)))

(defun prot=factor-cuts-triple-usable-p (factor-cuts-triple tree-node)
  (declare (edited  "23-OCT-1997")
	   (authors Ameier)
	   (input   "A factor-cuts-triple and a tree-node.")
	   (effect  "None.")
	   (value   "T if the factor node of the factor-cuts-triple is eq to the"
		    "tree-node."))
  (eq (prot~factor-cuts-triple-factor-node factor-cuts-triple)
      tree-node))

    
(defun prot=work-on-position-successor-pair (position-successor-pair
					     tree-node
					     clause
					     connection-position
					     remaining-red-cuts-pairs
					     remaining-factor-cuts-triples
					     res-proof)
  
  (let* ((position (prot~position-successor-pair-position position-successor-pair))
	 (sucessor (prot~position-successor-pair-successor-node position-successor-pair)))
    
    ;; mache resolution proof unter diesem Sucessor
    (multiple-value-bind
	(back-clause
	 back-position
	 back-red-cuts-pairs
	 back-factor-cuts-triples)
	(prot=resolve-under! sucessor res-proof)
      
      ;; Mache Resolutions Schritt mit clause an position und back-clause an back-position
      (let* ((new-clause (prot=make-resolution clause back-clause position back-position))
	     (new-connection-position (if connection-position
					  (prot=update-position connection-position (list position))
					nil)))
	(setf (res~proof-clauses res-proof) (cons new-clause (res~proof-clauses res-proof)))
	
	;; update die back-red-cuts-pairs und back-factor-cuts-triples
	(multiple-value-bind
	    (updated-back-red-cuts-pairs updated-back-factor-cuts-triples)
	    (prot=update-back-red-cuts-pairs-back-factor-cuts-triples back-red-cuts-pairs
								      back-factor-cuts-triples
								      clause
								      back-position)
	  ;; update dir remaining-red-cuts-pairs und remainig-factor-cuts-triples
	  (let* ((updated-remaining-factor-cuts-triples (prot=update-current-factor-cuts-triples remaining-factor-cuts-triples
												 (list position)))
		 (updated-remaining-red-cuts-pairs (prot=update-current-red-cuts-pairs remaining-red-cuts-pairs
										       (list position))))
	    
	    ;; -> mache noch die moeglichen back-red-cuts-pairs und back-factor-cuts-triples Schritte
	    ;; -> Remark: Dies hat keinen Einfluss mehr auf die updated-remaining-factor-cuts-triples + updated-remaining-red-cuts-pairs

	    (multiple-value-bind
		(clause-after-pos-factor-cuts
		 connection-position-after-pos-factor-cuts
		 back-red-cuts-pairs-after-pos-factor-cuts
		 back-factor-cuts-triples-after-pos-factor-cuts)
		(prot=insert-factor-cuts new-clause
					 new-connection-position
					 tree-node
					 updated-back-red-cuts-pairs
					 updated-back-factor-cuts-triples
					 res-proof)
	      (multiple-value-bind
		  (clause-after-pos-red-cuts
		   connection-position-after-pos-red-cuts
		   back-red-cuts-pairs-after-pos-red-cuts
		   back-factor-cuts-triples-after-pos-red-cuts)
		  (prot=insert-red-cuts clause-after-pos-factor-cuts
					connection-position-after-pos-factor-cuts
					tree-node
					back-red-cuts-pairs-after-pos-factor-cuts
					back-factor-cuts-triples-after-pos-factor-cuts
					res-proof)
		(values clause-after-pos-red-cuts
			connection-position-after-pos-red-cuts
			(append back-red-cuts-pairs-after-pos-red-cuts updated-remaining-red-cuts-pairs)
			(append back-factor-cuts-triples-after-pos-red-cuts updated-remaining-factor-cuts-triples))))))))))

(defun prot=update-back-red-cuts-pairs-back-factor-cuts-triples (red-cuts-pairs factor-cuts-triples clause position)
  (declare (edited  "23-OCT-1997")
	   (authors Ameier)
	   (input   "A set of red-cuts-pairs and a set of factor-cuts-triples the first clause of resolution"
		    "and the resolution position in the second clause.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "The updated red-cuts-pairs and factor-cuts-triples."))
  (let* ((rest-cl-length (- (length (cl~literals clause)) 1))
	 (pos-num (pos~first position)))
    (values
     (mapcar #'(lambda (red-cut-pair)
		 (let* ((position-num (pos~first (prot~red-cuts-pair-position red-cut-pair)))
			(node (prot~red-cuts-pair-path-node red-cut-pair)))
		   (prot~red-cuts-pair-create (pos~list-position (list (- (+ position-num rest-cl-length) (if (> position-num pos-num)
													      1
													    0))))
					      node)))
	     red-cuts-pairs)
     (mapcar #'(lambda (factor-cuts-triple)
		 (let* ((position-num (pos~first (prot~factor-cuts-triple-position factor-cuts-triple)))
			(node (prot~factor-cuts-triple-factor-node factor-cuts-triple))
			(pos2 (prot~factor-cuts-triple-factor-position factor-cuts-triple)))
		   (prot~factor-cuts-triple-create
		    (pos~list-position (list (- (+ position-num rest-cl-length) (if (> position-num pos-num)
										    1
										  0))))
		    node
		    pos2)))
	     factor-cuts-triples))))


(defun prot=insert-red-cuts (clause predecessor-position tree-node red-cuts-pairs factor-cuts-triples res-proof)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, the position in that clause, the clause is connected with its predecessor"
		    "the tree node, the red-cuts-pairs, remaining factor-cuts-triples and the current"
		    "resolution proof.")
	   (effect  "New steps are insert in the resolution proof.")
	   (value   "The literal of every red-cuts-pair, whose node is eq to the predecessor-node"
		    "is factored with the literal of the current-clause at the predecessor-position."
		    "Multiple-value:"
		    "First: The new clause after this factorings."
		    "Second: The updated predecessor position."
		    "Third: The remaining, unfactored red-cuts-pairs with updated positions."
		    "Fourth: The remaing factor-cuts-triples with updated positions."))
  (let* ((possible-red-cuts-pairs (remove-if-not #'(lambda (red-cuts-pair)
						     (prot=red-cuts-pair-usable-p red-cuts-pair tree-node))
						 red-cuts-pairs))
	 (remaining-red-cuts-pairs (remove-if #'(lambda (red-cuts-pair)
						  (prot=red-cuts-pair-usable-p red-cuts-pair tree-node))
					      red-cuts-pairs)))
    (do* ((current-clause clause)
	  (current-predecessor-position predecessor-position)
	  (rest-red-cuts-pairs possible-red-cuts-pairs)
	  (current-remaining-red-cuts-pairs remaining-red-cuts-pairs)
	  (current-factor-cuts-triples factor-cuts-triples))
	((null rest-red-cuts-pairs)
	 (values current-clause
		 current-predecessor-position
		 current-remaining-red-cuts-pairs
		 current-factor-cuts-triples))
      (let* ((head-red-cuts-pair (first rest-red-cuts-pairs))
	     (head-pos (prot~red-cuts-pair-position head-red-cuts-pair))
	     (greater-pos (if (< (pos~first head-pos) (pos~first current-predecessor-position))
			      current-predecessor-position
			    head-pos))
	     (lesser-pos (if (< (pos~first head-pos) (pos~first current-predecessor-position))
			     head-pos
			   current-predecessor-position)))
	
	
	(setq current-clause (prot=make-factoring current-clause current-predecessor-position head-pos))
	(setf (res~proof-clauses res-proof) (cons current-clause (res~proof-clauses res-proof)))
	
	(setq current-predecessor-position lesser-pos)
	;; predecessor position ist auf jedenfall nun die lesser position
	
	(setq rest-red-cuts-pairs (prot=update-current-red-cuts-pairs (rest rest-red-cuts-pairs) (list greater-pos)))
	(setq current-remaining-red-cuts-pairs (prot=update-current-red-cuts-pairs current-remaining-red-cuts-pairs (list greater-pos)))
	(setq current-factor-cuts-triples (prot=update-current-factor-cuts-triples current-factor-cuts-triples (list greater-pos)))))))


(defun prot=update-current-factor-cuts-triples (factor-cuts-triples positions)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A list of factor-cuts-triples and a list of positions.")
	   (effect  "None.")
	   (value   "A list of updated red-cuts-pairs. That means, that for every position in"
		    "the positions list, that is lesser as the position-number of the factor-cuts-triple"
		    "this position-number is reduced for 1."))
  (let* ((pos-numbers (mapcar #'pos~first positions)))
    (mapcar #'(lambda (factor-cuts-triple)
		(let* ((position1 (prot~factor-cuts-triple-position factor-cuts-triple))
		       (pos-num1 (pos~first position1))
		       (node (prot~factor-cuts-triple-factor-node factor-cuts-triple))
		       (position2 (prot~factor-cuts-triple-factor-position factor-cuts-triple))
		       (lesser-nums (remove-if-not #'(lambda (num)
						       (< num pos-num1))
						   pos-numbers)))
		  (prot~factor-cuts-triple-create (pos~list-position (list (- pos-num1 (length lesser-nums))))
						  node
						  position2)))
	    factor-cuts-triples)))


  
(defun prot=update-current-red-cuts-pairs (red-cuts-pairs positions)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A list of red-cuts-pairs and a list of positions.")
	   (effect  "None.")
	   (value   "A list of updated red-cuts-pairs. That means, that for every position in"
		    "the positions list, that is lesser as the position-number of the red-cut-pair"
		    "this position-number is reduced for 1."))
  (let* ((pos-numbers (mapcar #'pos~first positions)))
    (mapcar #'(lambda (red-cuts-pair)
		(let* ((position (prot~red-cuts-pair-position red-cuts-pair))
		       (pos-num (pos~first position))
		       (node (prot~red-cuts-pair-path-node red-cuts-pair))
		       (lesser-nums (remove-if-not #'(lambda (num)
						       (< num pos-num))
						   pos-numbers)))
		  (prot~red-cuts-pair-create (pos~list-position (list (- pos-num (length lesser-nums))))
					     node)))
	    red-cuts-pairs)))


    
(defun prot=insert-factor-cuts (clause connection-position tree-node red-cuts-pairs factor-cuts-triples res-proof)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, the node of the clause, the connection-position to the predecessor, a list"
		    "of red-cuts-pairs, a list of factor-cuts-triples and a resolution proof.")
	   (effect  "Makes the factoring steps.")
	   (value   "Multiple-value-bind:"
		    "First: The resulting clause, after inserting all possible factor-cuts."
		    "Second: The updated connection position."
		    "Third: The updated red-cuts-pairs"
		    "Fourth: The updated remaining factor-cuts-triples."))
  (let* ((possible-factor-cuts-triples (remove-if-not #'(lambda (factor-cuts-triple)
							  (prot=factor-cuts-triple-usable-p factor-cuts-triple tree-node))
						      factor-cuts-triples))
	 (remaining-factor-cuts-triples (remove-if #'(lambda (factor-cuts-triple)
						       (prot=factor-cuts-triple-usable-p factor-cuts-triple tree-node))
						   factor-cuts-triples)))
    (do* ((rest-fac-triples possible-factor-cuts-triples)
	  (current-clause clause)
	  (current-red-cuts-pairs red-cuts-pairs)
	  (current-connection-position connection-position)
	  (current-remaining-fac-triples remaining-factor-cuts-triples))
	((null rest-fac-triples)
	 (values current-clause
		 current-connection-position
		 current-red-cuts-pairs
		 current-remaining-fac-triples))
      (let* ((head-triple (prot=take-highest-triple rest-fac-triples))
	     (pos1 (prot~factor-cuts-triple-position head-triple))
	     (pos2 (prot~factor-cuts-triple-factor-position head-triple))
	     (greater-pos (if (< (pos~first pos1) (pos~first pos2))
			      pos2
			    pos1))
	     (lesser-pos (if (< (pos~first pos1) (pos~first pos2))
			     pos1
			   pos2)))
	
	(setq current-clause (prot=make-factoring current-clause lesser-pos greater-pos))
	(setf (res~proof-clauses res-proof) (cons current-clause (res~proof-clauses res-proof)))
	
	(setq rest-fac-triples
	      (remove head-triple rest-fac-triples :test 'keim~equal))
	(setq current-connection-position (prot=update-position current-connection-position (list greater-pos)))
	(setq current-red-cuts-pairs (prot=update-current-red-cuts-pairs current-red-cuts-pairs (list greater-pos)))
	(setq current-remaining-fac-triples (prot=update-current-factor-cuts-triples current-remaining-fac-triples (list greater-pos)))))))



(defun prot=take-highest-triple (factor-cuts-triples)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A list of factor-cuts-triples.")
	   (effect  "None.")
	   (value   "The factor-cuts-triple with the highest position number."))
  (do* ((rest-fac-triples factor-cuts-triples (rest rest-fac-triples))
	(current-high -1)
	(current-triple nil))
      ((null rest-fac-triples)
       current-triple)
    (let* ((head-triple (first rest-fac-triples))
	   (max-num (max (pos~first (prot~factor-cuts-triple-position head-triple))
			 (pos~first (prot~factor-cuts-triple-factor-position head-triple)))))
      (when (> max-num current-high)
	(setq current-high max-num)
	(setq current-triple head-triple)))))



(defun prot=update-position (con-position clause-positions)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A position and the list of the positions of the"
		    "resolved or factored literals of the clause of this node.")
	   (effect  "None.")
	   (value   "The updated predecessor connection position. That means, that for every position in"
		    "the positions list, that is lesser as the position-number of the red-cut-pair"
		    "this position-number is reduced for 1."))
  (if con-position
      (let* ((pos-num (pos~first con-position))
	     (pos-numbers (mapcar #'pos~first clause-positions))
	     (lesser-nums (remove-if-not #'(lambda (num)
					     (< num pos-num))
					 pos-numbers)))
	(pos~list-position (list (- pos-num (length lesser-nums)))))
    nil))
  
(defun prot=make-resolution (clause1 clause2 position1 position2)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "Two clauses and two positions.")
	   (effect  "None.")
	   (value   "The result of resolving these two clauses at these positions:"
		    "A clause if possible, otherwise nil."))
  (multiple-value-bind
      (sep-clause2 sep-renaming)
      (res~separate-clauses clause1 clause2)
    (let* ((literals1 (cl~literals clause1))
	   (literals2 (cl~literals sep-clause2))
	   (lit1 (data~struct-at-position literals1 position1))
	   (lit2 (data~struct-at-position literals2 position2))
	   (mgu (term~unify (lit~atom lit1) (lit~atom lit2))))
      (if (or (equal (lit~positive-p lit1) (lit~positive-p lit2))
	      (null mgu))
	  (omega~error "In prot=make-resolution: not possible.")
	(let* ((new-literals (mapcar #'(lambda (lit)
					 (subst~apply mgu
						      lit
						      :downto '(data+primitive)))
				     (append (remove lit1 literals1)
					     (remove lit2 literals2))))
	       (new-just (res~resolution-create (list clause1 clause2)
						(list position1 position2)
						(list (subst~create nil nil) sep-renaming)
						mgu
						(format nil "Prot-RES-~A" (incf prot*just-counter)))))
	  (cl~create new-literals :justification new-just))))))


    
(defun prot=make-factoring (clause position1 position2)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A clause and two positions.")
	   (effect  "None.")
	   (value   "The result of factoring these clause at these positions:"
		    "A clause if possible, otherwise nil."))
  (let* ((literals (cl~literals clause))
	 (lit1 (data~struct-at-position literals position1))
	 (lit2 (data~struct-at-position literals position2))
	 (mgu (term~unify (lit~atom lit1) (lit~atom lit2))))
    (if (or (not (equal (lit~positive-p lit1) (lit~positive-p lit2)))
	    (null mgu)
	    (keim~equal position1 position2))
	(omega~error "In prot=make-factoring: not possible.")
      (let* ((greater-pos (if (< (pos~first position1) (pos~first position2))
			      position2
			    position1))
	     (lesser-pos (if (< (pos~first position1) (pos~first position2))
			     position1
			   position2))
	     (greater-lit (data~struct-at-position literals greater-pos))
	     (new-literals (mapcar #'(lambda (lit)
				       (subst~apply mgu
						    lit
						    :downto '(data+primitive)))
				   (remove greater-lit literals)))
	     (new-just (res~factoring-create clause
					     (list lesser-pos greater-pos)
					     (subst~create nil nil)
					     mgu
					     (format nil "Prot-FACT-~A" (incf prot*just-counter)))))
	(cl~create new-literals :justification new-just)))))
   
			     
  
#| -------------------------------------------------------------- PROTEIN PRINT ---------------------------------------------------- |#


(defun prot=print (declarations clauses &key (docu 't))
  
  (setq prot*in-string "")
  
  (prot=write-options declarations)
  (when docu
    (prot=write-convert-list))
  (prot=print-clauses clauses))

(defun prot=add-string-to-in-string (string)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "Adds the string to the prot*in-string."))
  (setq prot*in-string (format nil "~A~A" prot*in-string string)))
	   

(defun prot=write-convert-list ()
  (declare (edited  "28-AUG-1997")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "Print the convert-list into the prot*in-string.")
	   (value   "Undefined."))
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string "%%%% THE NAME CONVERSIONS %%%%")
  (prot=add-string-to-in-string #\Newline)
  (mapcar #'(lambda (pair)
	      (prot=add-string-to-in-string (format nil "%%%%    ~A  -->  ~A"
						    (keim~name (if (term~schema-p (first pair))
								   (data~schema-range (first pair))
								 (first pair)))
						    (second pair)))
	      (prot=add-string-to-in-string #\Newline))
	  prot*convert-list)
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string "%%%% THE CLAUSE LISTS %%%%")
  (prot=add-string-to-in-string #\Newline))

(defun prot=write-options (declarations)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "The declarations for protein.tme")
	   (effect  "Print the declarations into the prot*in-string.")
	   (value   "Undefined.")) 
  (mapcar #'(lambda (string)
	      (prot=add-string-to-in-string string)
	      (prot=add-string-to-in-string #\Newline))
	  declarations))

(defun prot=print-clauses (clauses)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "Prints the clauses into the prot*in-string.")
	   (value   "Undefined."))
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string "% THE CLAUSES %")
  (prot=add-string-to-in-string #\Newline)
  
  (mapc #'(lambda (clause)
	    (prot=print-clause clause)
	    (prot=add-string-to-in-string #\Newline))
	clauses)
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string #\Newline)
  (prot=add-string-to-in-string "% END CLAUSES"))

(defun prot=print-clause (clause)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "Prints the clause into the prot*in-string.")
	   (value   "Undefined."))
  (prot=add-string-to-in-string #\Newline)
  (prot=print-term (first (cl~literals clause)))
  (mapc #'(lambda (lit)
	    (prot=add-string-to-in-string " ; ")
	    (prot=print-term lit))
	(rest (cl~literals clause)))
  (prot=add-string-to-in-string "."))

(defgeneric prot=print-term (object)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "An term object.")
	   (effect  "A PROTEIN-input representation of OBJECT is written to prot*in-string.")
	   (value   "Undefined."))
  (:method ((literal lit+literal))
	   (when (not (lit~positive-p literal))
	     (prot=add-string-to-in-string "-"))
	   (prot=print-term (lit~atom literal)))
  (:method ((var term+variable))
	   (prot=add-string-to-in-string (prot=get-checked-name-to-object var)))
  (:method ((const term+constant))
	   (prot=add-string-to-in-string (prot=get-checked-name-to-object const)))
  (:method ((term term+appl))
	   (prot=print-term (data~appl-function term))
	   (let ((args (data~appl-arguments term)))
	     (prot=add-string-to-in-string "(")
	     (prot=print-term (first args))
	     (mapc #'(lambda (term)
		       (prot=add-string-to-in-string ",")
		       (prot=print-term term))
		   (rest args))
	     (prot=add-string-to-in-string ")"))))

#| --------------------------------------------------------- Protein READ ------------------------------------------------------------ |#

(defun prot=read-without-parsing (res-proof protein-out-string)
  (declare (edited  "07-JUN-2000")
	   (authors Ameier)
	   (input   "The protein-problem-dir, the current resolution proof." )
	   (effect  "None.")
	   (value   "T if protein has found a proof, that is if the out-string contains the tree file, otherwise nil."))
  (let* ((line-strings (atptop~divide-string protein-out-string #\Newline)))
    (multiple-value-bind
	(trc-strings tree-strings)
	(do* ((rest-strings line-strings (rest rest-strings))
	      (trc-strings nil)
	      (tree-strings nil))
	    ((or trc-strings (null rest-strings))
	     (if (null rest-strings)
		 (error "~% Rest-strings are nil in function prot=read-with-parsing.")
	       (values (remove-if #'(lambda (string)
				      (string= string ""))
				  trc-strings)
		       (remove-if #'(lambda (string)
				      (String= string ""))
				  tree-strings))))
	  (let* ((head-string (first rest-strings)))
	    (cond ((string= (first rest-strings) "%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%")
		   (setq trc-strings (rest rest-strings)))
		  (t
		   (setq tree-strings (append tree-strings (list head-string)))))))

      (if tree-strings
	  't
	nil))))


(defun prot=read-with-parsing (res-proof protein-out-string)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "The protein-problem-dir, the current resolution proof." )
	   (effect  "During the read of the proof the resolution proof is updated by new steps.")
	   (value   "T if protein has found a proof, nil otherwise."))
  (let* ((line-strings (atptop~divide-string protein-out-string #\Newline)))
    (multiple-value-bind
	(trc-strings tree-strings)
	(do* ((rest-strings line-strings (rest rest-strings))
	      (trc-strings nil)
	      (tree-strings nil))
	    ((or trc-strings (null rest-strings))
	     (if (null rest-strings)
		 (error "~% Rest-strings are nil in function prot=read-with-parsing.")
	       (values (remove-if #'(lambda (string)
				      (string= string ""))
				  trc-strings)
		       (remove-if #'(lambda (string)
				      (String= string ""))
				  tree-strings))))
	  (let* ((head-string (first rest-strings)))
	    (cond ((string= (first rest-strings) "%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%")
		   (setq trc-strings (rest rest-strings)))
		  (t
		   (setq tree-strings (append tree-strings (list head-string)))))))
      
      (let* ((query-clause (prot=read-initial-clauses! res-proof trc-strings tree-strings))
	     ;; liest initial clauses ein aus protein.trc, stellt prot*number-clause-list auf und gibt query-clause zurueck
	     (new-tree (prot=build-tree query-clause (rest tree-strings)))  ;; erste Zeile gibt Query-CLause an -> bereits behandelt
	     ;; baut Beweisbaum auf aus protein.tree
	     (empty-clause (prot=resolve-under! (prot~tree-root-node new-tree) res-proof))
	     ;; baut resolution Proof auf
	     )
	(setf (res~proof-empty-clause res-proof)
	      empty-clause)))))


(defun prot=read-initial-clauses! (res-proof trc-strings tree-strings)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "The current-resolution proof, a list of strings, containing the contents of the"
		    "protein.trc file and a similar list of strings, containing the contents of the"
		    "protein.tree file.")
	   (effect  "The initial clauses are read from the protein.trc file and the"
		    "prot*number-clause-list is updated by them.")
	   (value   "The query clause."))
  
  (setq prot*rest-initial-clauses (res~proof-initial-clauses res-proof))
  (setq prot*number-clause-list nil)
  (setq prot*local-clause-vars nil)
  
  (let* ((initial-clauses-lines (do* ((rest-strings trc-strings (rest rest-strings))
				      (clauses-strings nil)
				      (control-flag 'start))
				    ((equal control-flag 'end) clauses-strings)
				  (let* ((head-string (first rest-strings)))
				    (cond ((equal control-flag 'start)
					   (when (string= head-string "--------------------    input clauses   --------------------")
					     (setq control-flag 'reading)))
					  ((equal control-flag 'reading)
					   (cond ((string= head-string "--------------------        flags       --------------------")
						  (setq control-flag 'end))
						 (t
						  (setq clauses-strings (append clauses-strings (list head-string)))))))))))
    
    ;; liest clauses und updated prot*number-clause-list
    (mapcar #'(lambda (input-line)
		(when (prot=initial-clause-check-p input-line)
		  (prot=parse-input-line input-line)))
	    initial-clauses-lines)
    
    ;; bestimmt query clause und gibt sie zurueck
    (prot=parse-query-clause tree-strings)))

(defun prot=initial-clause-check-p (input-line)
  (declare (edited  "22-OCT-1997")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "T if the string starts with clause (after 1)."))
  (if (and (>= (length input-line) 7)
	   (equal (char input-line 1) #\C)
	   (equal (char input-line 2) #\l)
	   (equal (char input-line 3) #\a)
	   (equal (char input-line 4) #\u)
	   (equal (char input-line 5) #\s)
	   (equal (char input-line 6) #\e))
      't
    nil))

(defun prot=parse-input-line (input-line)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A string, that represents an initial clause in the protein.trc file.")
	   (effect  "The string is interpreted as clause and between this process the"
		    "prot*number-clause-list is updated by an according new number-clause"
		    "pair.") 
	   (value   "Undefined."))
  (multiple-value-bind
      (prefix rest-word-I)
      (atptop~get-next-word input-line #\.)
    (declare (ignore prefix))
    (multiple-value-bind
	(number-string rest-word-II)
	(atptop~get-next-word rest-word-I #\: :ignore-char-list (list #\space))
      (let* ((clause (prot=parse-clause (atptop~cut-first-char
					  (atptop~cut-last-char rest-word-II)))))
	(setq prot*number-clause-list (cons (list (atptop~parse-number number-string) clause)
					     prot*number-clause-list))
	clause))))


	
(defun prot=parse-clause (clause-string)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A string, that represents the literals of a clause.")
	   (effect  "None.")
	   (value   "A initial clause of the current-resolution proof, that is equal till renaming,"
		    "to the clause, represented by this string."))
  (setq prot*local-clause-vars nil)
  
  (let* ((literal-strings (do* ((i 0 (+ i 1))
				(open-kl 0)
				(closed-kl 0)
				(literal-strings nil)
				(current-string ""))
			      ((= i (length clause-string))
			       (append literal-strings (list current-string)))
			    (let* ((current-char (char clause-string i)))
			      (cond ((eq current-char #\space)
				     nil)
				    ((eq current-char #\()
				     (setq open-kl (+ open-kl 1))
				     (setq current-string (format nil "~A~A" current-string current-char)))
				    ((eq current-char #\))
				     (setq closed-kl (+ closed-kl 1))
				     (setq current-string (format nil "~A~A" current-string current-char)))
				    ((eq current-char #\,)
				     (if (= open-kl closed-kl)
					 (progn
					   (setq literal-strings (append literal-strings (list current-string)))
					   (setq current-string ""))
				       (setq current-string (format nil "~A~A" current-string current-char))))
				    (t
				     (setq current-string (format nil "~A~A" current-string current-char)))))))
	 (literals (mapcar #'prot=parse-literal literal-strings))
	 (new-clause (cl~create literals))
	 (original-clause (find new-clause prot*rest-initial-clauses
				:test #'atptop~clauses-equal-till-renaming-p)))
    (setq prot*rest-initial-clauses (remove original-clause prot*rest-initial-clauses))
    original-clause))


(defun prot=parse-literal (literal-string)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A string, that represents a literal.")
	   (effect  "If the literal contains new renaming variables (the input clauses)"
		    "a renamed from the really inputed clauses) the prot*convert-list is"
		    "updated by pairs (new-var var-string).")
	   (value   "The according literal object."))  
  (let* ((polarity (if (string= (format nil "~A~A~A~A"
					(char literal-string 0)
					(char literal-string 1)
					(char literal-string 2)
					(char literal-string 3))
				"not_")
		       nil
		     't))
	 (atom-string (if polarity
			  literal-string
			(do* ((i 3 (+ i 1))
			      (new-string "" (format nil "~A~A" new-string (char literal-string i))))
			    ((= i (- (length literal-string) 1))
			     new-string))))
	 (atom (prot=parse-term atom-string)))
    (lit~literal-create atom polarity)))
			      


(defun prot=parse-term (term-string &key (type nil))
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A string representing a term and keyword type.")
	   (effect  "Perhaps the prot*local-clause-vars is changed (if new vars needed).")
	   (value   "A term instance of the input string."))
  (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
    ;; reads till a "(" is reached 
    ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
    (if (string= rest-string "")
	(prot=string2object functor-string :type type)
      (let* ((functor (prot=string2object functor-string))
	     (args (mapcar #'(lambda (term-string awaiting-type)
			       (prot=parse-term term-string :type awaiting-type))
			   (prot=parse-term-list (atptop~cut-last-char rest-string))
			   (data~n-domain (term~type (if (term~schema-p functor)
							 (data~schema-range functor)
						       functor))))))
	(term~appl-create functor args
			  )))))


(defun prot=parse-term-list (term-list-string)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A term-list-sting, this is something like s(a,b,c),p(d,e,f),r,q")
	   (effect  "None.")
	   (value   "This list is divided top-level by the commata:"
		    "returns a list of strings."
		    "( \"s(a,b,c)\"  \"p(d,e,f)\" \"r" \"q")"
		    "The function does not destroy the term-structur down in the terms."))
  (do ((rest-term-list-string term-list-string)
       (current-word "")
       (return-terms nil))
      ((string= rest-term-list-string "") return-terms)
    (multiple-value-bind 	      
	(next-word rest-string)
	(atptop~get-next-word rest-term-list-string #\, :handle-break-char 'pre)
      (setq current-word (format nil "~A~A" current-word next-word))
      (setq rest-term-list-string rest-string) 
      (if (= (atptop~number-of-char-in-string #\( current-word) (atptop~number-of-char-in-string #\) current-word)) 
	  (progn
	    (if (string= rest-string "")
		(setq return-terms (append return-terms (list current-word)))
	      (setq return-terms (append return-terms (list (atptop~cut-last-char current-word))))) 
	    (setq current-word "")))))) 



(defun prot=parse-query-clause (tree-strings)
  (declare (edited  "21-OCT-1997")
	   (authors Ameier)
	   (input   "A list of strings, representing the contents of the protein.tree file.")
	   (effect  "None.")
	   (value   "The Query clause."))
  (let* ((first-line (first tree-strings)))    
    (multiple-value-bind
	(just num1 num2)
	(prot=split-tree-information first-line)
      (declare (ignore num2 just))
      (prot=number2clause num1))))

							  
#| ---------------------------------------- Handling Convert lists and clause-number lists ------------------------------------------ |#


(defun prot=number2clause (number)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A number.")
	   (effect  "None.")
	   (value   "If the number corresponds to a clause and is therefor standing in"
		    "the prot*number-clause-list the corresponding clause is returned."
		    "otherwise nil."))
  (second (first (member number prot*number-clause-list
			 :test #'(lambda (number pair) (= number (first pair)))))))


#|
   You can't use all symbols in names for predicates, functions and symbols as input for protein. So the existing names
   has to be converted in a protein acceptable form.
   In the variable prot*convert-list a list of pairs (lists) of objects and their converted name-strings is stored
|#

(defun prot=compute-convert-list (clauses)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "All names of Constants and variables of the clauses are converted"
		    "into names, that are usable by PROTEIN."
		    "A list of these objects and their corresponding new names (strings)"
		    "is stored in prot*convert-list.")
	   (value   "Undifined."))
  (setq prot*convert-list nil)
  (mapcar #'prot=convert-object clauses))

(defgeneric prot=convert-object (object)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "The prot*convert-list is updated. by converting the parts of this"
		    "object.")
	   (value   "Undifined."))
  (:method ((object cl+clause))
	   (mapcar #'prot=convert-object (cl~literals object)))
  (:method ((object lit+literal))
	   (prot=convert-object (lit~atom object)))
  (:method ((object term+appl))
	   (mapcar #'prot=convert-object
		   (cons (data~appl-function object)
			 (data~appl-arguments object))))
  (:method ((object term+primitive))
	   (prot=convert-name object)))

(defun prot=convert-name (object)
  (declare (edited  "14-MAY-1996")
	   (authors Ameier)
	   (input   "An object, that can be of type term+variable, term+constant or term+number.")
	   (effect  "If a new name-string is produced, a pair of old-name-string"
		    "and new-name-string is added to the prot*convert-list.")
	   (value   "From the name is a protein-compatible name produced."
		    "That means all symbols till alphabetics,numbers and _ are"
		    "deleted from the name and a counter-number is added."
		    "If var is set the resulting string is upcased, otherwise"
		    "it is downcased. If the name was attached before the"
		    "before produced new-string is taken from the prot*convert-list"
		    "and is returned otherwise a new string is produced in the way"
		    "descibed before."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the prot*convert-list
	 ;;(partner-string (second (first (member object prot*convert-list
	 ;;					:test #'(lambda (thing pair)
	 ;;						  (let* ((thing2 (first pair)))
	 ;; 						    (if (term~schema-p thing2)
	 ;;							(data~equal-p thing (data~schema-range thing2))
	 ;;						      (keim~equal thing thing2)))))))))
	 (partner-string (second (first (member object prot*convert-list
						:test #'(lambda (thing pair)
							  (let* ((thing2 (first pair)))
							    (or (eq thing thing2)
								(and (data~equal thing thing2)
								     (data~equal (term~type thing) (term~type thing2)))))))))))
    ;; If element already in the prot*convert-list give back the ob-string already used in the prot*convert-list
    (if partner-string
	partner-string
      ;; If not a new string is computed and is together with the input object, or, if existing its representation in the
      ;; environment, added to the prot*convert-list (list object string).
      ;; If polymorphie is used, it is necessary to use the object from the environment.
      ;; For example set with Type (aa -> o), but in the application (set a) with a of type i, set is of type (i -> o)
      ;; If we would save in the prot*convert-list this set, we couldn't create a term (set 1) with 1 of type num.
      (let ((new-string ""))
	(do ((i 0 (+ i 1)))
	    ((>= i (length name-string)) nil)
	  (when (member (char name-string i) prot*name-symbols)
	    (setq new-string (format nil "~A~A" new-string (char name-string i)))))
	(setq new-string (format nil "ob_~A_~A" new-string (incf prot*convert-counter)))
	(let ((checked-new-string (if (term~variable-p object)
				      (string-upcase new-string)
				    (string-downcase new-string)))
	      (env-element (env~lookup-object (intern (string-upcase name-string) (find-package :omega))
					      (res~proof-environment prot*current-problem))))
	  (setq prot*convert-list (cons (list object
					      ;;(if (and env-element (not (typep object 'term+number)))
					      ;;	   env-element
					      ;;	 object)
					      checked-new-string) prot*convert-list))
	  checked-new-string)))))


(defun prot=get-checked-name-to-object (object)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "The checked name string to the object."
		    "If the object is in the prot*convert-list, this string is taken"
		    "otherwise the keim~name slot of the object is returned as string."))
  (let* ((name (keim~name object))
	 (name-string (if (stringp name)
			  name
			(format nil "~A" name)))
	 ;; Compute whether the element is already in the prot*convert-list
	 ;;(partner-string (second (first (member object prot*convert-list
	 ;;					:test #'(lambda (thing pair)
	 ;;						  (let* ((thing2 (first pair)))
	 ;;						    (if (term~schema-p thing2)
	 ;;							(data~equal-p thing (data~schema-range thing2))
	 ;;						      (keim~equal thing thing2)))))))))
	 (partner-string (second (first (member object prot*convert-list
						:test #'(lambda (thing pair)
							  (let* ((thing2 (first pair)))
							    (or (eq thing thing2)
								(and (data~equal thing thing2)
								     (data~equal (term~type thing) (term~type thing2)))))))))))
    (if partner-string
	partner-string
      name-string))) 


(defun prot=string2object (string &key (type nil))
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "A string")
	   (effect  "None.")
	   (value   "If the string corresponds to an object in the prot*convert-list"
		    "or the prot*local-clause-vars list the corresponding object is returned."
		    "otherwise the string corresponds to a new renaming var, so this"
		    "new renaming-var is created, the prot*local-clause-vars list is updated"
		    "by the pair new-var/string and the new-var is returned."))
  (let ((member-convert-list (first (first (member string prot*convert-list
						   :test #'(lambda (string pair) (string= string (second pair))))))))
    (if member-convert-list
	;; -> already in convert-list -> reconvert the string to an object
	;; member-convert-list

	(cond ((typep member-convert-list 'term+number)
	       member-convert-list)
	      ((string-equal (keim~name member-convert-list) "=")
	       (env~lookup-object '= prot*current-environment))
	      (t
	       member-convert-list))
      
      ;; not yet in convert-list -> look at prot*local-clause-vars
      (let ((member-local-clause (first (first (member string prot*local-clause-vars
						       :test #'(lambda (string pair) (string= string (second pair))))))))
	(if member-local-clause
	    member-local-clause   ;; -> already a new local clause var produced -> return it
	  ;; not yet a new var produced, produce it , add the pair (new-var "var-string") to  prot*local-clause-vars and
	  ;; return the new-var
	  (let* ((needed-type (cond ((null type)
				     (type~i))
				    ((null (type~variable-p type))
				     type)
				    (t ;; -> type ist type-variable
				     (if (env~lookup-object (keim~name type) prot*current-environment)
					 ;; ist im environment
					 type
				       ;; ist nicht im environment -> Kappa gebunden irgendwo her
				       (let* ((new-symbol (term~generate-new-name 'ntv prot*current-environment))
					      (new-type-var (type~variable-create new-symbol)))
					 (env~enter new-symbol new-type-var prot*current-environment)
					 new-type-var)))))
		 (new-var (term~generate-term-primitive-with-new-name 'prv- needed-type 'term+variable prot*current-environment)))

	    (setq prot*temporary-variables (cons new-var prot*temporary-variables))
	    (setq prot*local-clause-vars (cons (list new-var string) prot*local-clause-vars))
	    new-var))))))



#| ------------------------------------------------------- Hauptteil ---------------------------------------------------------------- |#

#|

Entfaellt ab Version 3.3

(defun prot=set-current-type-var-subst! ()
  (declare (edited  "27-MAR-1998")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "If the prot*current-environment contains a type-var-subst, the global"
		    "variable prot*current-type-var-subst is set to this value, otherwise"
		    "a new substitution is created, added with key type-var-subst in the"
		    "environment and prot*current-type-var-subst is set to this new substitution.")
	   (value   "Undefined."))
  (let* ((type-var-subst (env~lookup-object 'type-var-subst prot*current-environment)))
    (if type-var-subst
	(setq prot*current-type-var-subst type-var-subst)
      (let ((new-subst (subst~create nil nil)))
	(setq prot*current-type-var-subst new-subst)
	(env~enter 'type-var-subst new-subst prot*current-environment)))))

|#

(defun prot~generate-protein-problem (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "None.")
	   (value   "A atp-problem with type protein, a partial resolution proof and partial settet"
		    "global vars."
		    "Remark: atp-in-string is NOT-SET, and global vars are not set completly !!"))
  
  (setq prot*convert-counter 0)        ;; setzt counter fuer neue Namen auf 0
  (setq prot*temporary-variables nil)  ;; setzt die temporaeren Variablen auf nil
  (setq prot*just-counter 0)           ;; setzt justification counter fuer neue justification auf 0
  
  (let* ((res-proof (atptop~resolution-proof-create-from-nodes conclusion-node assumption-nodes ho-pds)))
    
    (setq prot*current-problem res-proof) ;; wichtig fuer convert-list
    (setq prot*current-environment (res~proof-environment res-proof))

    ;; Entfaellt ab Version 3.3
    ;; (prot=set-current-type-var-subst!)   ;; setzt variable prot*current-type-var-subst

    ;; translate the initial resolution proof res-proof to f.o. and normalize it
    (p2f~translate res-proof)
    
    (omega~message "~% Normalizing ...")
    (hocnf~normalize-res-proof! res-proof)

    ;; Remove clauses that contain abstractions
    ;; After p2f translation it can happen that there are clauses that contain lambda-abstraction
    ;; Such clauses can not be handled by OTTER (or any f.o. ATP and are therefore removed from the
    ;; clauses list
    (atptop~remove-clauses-with-abstractions! res-proof)
    
    ;; Compute the convertion of names
    (prot=compute-convert-list (res~proof-clauses res-proof))

    (atpprb~create-fo-problem (gensym "protein-problem-")
			      'protein
			      nil      ;; protein-in-file kommt erst spaeter dazu: -> protein~add-in-string! 
			      nil
			      res-proof
			      (list prot*convert-list)
			      (list 'p2f p2f*domain p2f*codomain))
    
    ;; in die atpprb~problem-translation-settings kommt eine Liste mit entweder:
    ;; 'p2f p2f*domain p2f*codomain, falls p2f~translate
    
    ;; in the global-var-list ist folgende Ordbubg:
    ;; 1.  prot*convert-list

    ;; nicht in global-var-list:
    ;; 1.  prot*current-problem           -> reconstruierbar aus res-proof environment
    ;; 2.  prot*current-environment       -> reconstruierbar aus res-proof environment
    ;; Entfaellt ab Version 3.3
    ;; 3.  prot*current-type-var-subst    -> reconstruierbar aus res-proof environment
    ;; 4.  prot*in-string                 -> nicht mehr benoetigt
    ;; 5.  prot*convert-counter           -> nicht mehr benoetigt
    ;; 6.  prot*name-symbols              -> unveraenderlich global
    ;; 7.  prot*number-clause-list        -> erst beim parsen benoetigt
    ;; 8.  prot*just-counter              -> erst beim parsen benoetigt
    ;; 9.  prot*temporary-variables       -> erst beim parsen benoetigt
    ;; 10. prot*rest-initial-clauses      -> erst beim parsen benoetigt
    ;; 11. prot*local-clause-vars         -> erst beim parsen benoetigt

    ))

(defun prot~add-in-string! (protein-problem declarations clauses &key (docu 't))
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "An protein-problem, the declarations of the flag settings for protein,"
		    "the clauses. The flag docu signs, whether the convert list should be added"
		    "to the protein.in string or not.")
	   (effect  "The prot*in-string is produced and added to the protein-problems atp-in-string slot.")
	   (value   "Undefined."))
  
  ;; konstruiert das protein in-file im prot*in-string
  (prot=print declarations clauses :docu docu)
  
  ;; setzt atp-in-string im protein-problem
  (setf (atpprb~problem-atp-in-string protein-problem) prot*in-string)
  )

(defun prot~complete-protein-problem! (protein-problem &key (parse-back 't))
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A protein problem and as keyword parse-back, a flag whether the may found proof"
		    "should be parsed back from the string.")
	   (effect  "The atp-out-string is read and then the resolution proof is parsed"
		    "from this string to complete the partial resolution proof.")
	   (value   "If there was a atp-out-string that represents a resolution proof (if protein has found a"
		    "proof) the complete resolution proof if flag parse-back was T or T if parse-bacl was"
		    "nil. If there is no atp-out-string (protein has failed to find a proof) nil."))
  (let* ((protein-out-string (atpprb~problem-atp-out-string protein-problem))
	 (tree-file-existent (not (atptop~string-is-prefix-of-string-p
				   (format nil "~A~A"
					   #\Newline
					   "%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%")
				   protein-out-string)))
	 (res-proof (atpprb~problem-part-res-proof protein-problem))
	 (global-vars (atpprb~problem-global-vars protein-problem))
	 (proof-object (third global-vars))
	 (translation-settings (atpprb~problem-translation-settings protein-problem)))
    
    (setq prot*convert-list (first global-vars))
    
    (setq prot*current-problem res-proof)
    (setq prot*current-environment (res~proof-environment res-proof))

    ;; Entfaellt ab Version 3.3
    ;; (prot=set-current-type-var-subst!)    ;; setzt prot*current-type-var-subst
    
    (setq prot*temporary-variables nil)  ;; setzt temporaere Variablen auf nil
    (setq prot*just-counter 0)           ;; setzt justification counter fuer neue justification auf 0
    
    (setq p2f*domain (second translation-settings))    ;; stellt Uebersetzungsinformation p2f wieder her
    (setq p2f*codomain (third translation-settings))
    
    ;; parse protein out-file (consisting of protein.trc and protein.tree)
    (let* ((proof-flag (if (not parse-back)
			   (if tree-file-existent
			       't
			     nil)  ;; -> ist protein.tree File da -> Beweis da, wenn nicht nil
			 (if tree-file-existent
			     (progn (omega~message "Parsing Protein Proof ... ~%")
				    (prot=read-with-parsing res-proof protein-out-string))
			   nil))))
      ;; output
      (if proof-flag
	  (omega~message "~% PROTEIN HAS FOUND A PROOF ~%")
	(omega~message "~% PROTEIN HAS FAILED TO FIND A PROOF ~%"))
      
      ;; remove temporary-variables
      (mapcar #'(lambda (var)
		  (env~remove (keim~name var) prot*current-environment))
	      prot*temporary-variables)
      
      
      ;; values
      (if proof-flag
	  (if parse-back
	      (progn
		(res~add-proof-in-hash-table res-proof)
		(setq omega*current-resolution-proof res-proof)
		(atptop~order-resolution-steps! res-proof)
		(omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
		res-proof)
	    't)
	nil))))

(defun prot~call-protein (open-node ho-pds dir ressource parse-back)
  (declare (edited  "27-OCT-1997")
	   (authors Ameier)
	   (input   "An Node, that is justified by the protein-method, the acording PDS, that"
		    "contains this node, a directory, in that temporaery needed files can be stored,"
		    "the time-ressource in seconds and a flag that signs whether a eventuelly found proof"
		    "should be parsed or not")
	   (effect  "In the directory a new file protein.tme is created, that contains the clauses"
		    "to the problem that is constructed from the input node."
		    "Protein is called on this file and will also create new Files (protein.tree ...)")
	   (value   "If a proof was found and parse-back was true: the according resolution proof."
		    "If a proof was found and parse-back was nil: true."
		    "Otherwise: NIl"))
  
  (let* ((problem-name (keim~name ho-pds))
	 (protein-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) dir))
	 (protein-problem (prot~generate-protein-problem open-node
							 (remove open-node (pds~node-supports open-node))
							 ho-pds
							 ))
	 (res-proof (atpprb~problem-part-res-proof protein-problem)))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file protein-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring protein-problem-dir)))))

    ;; erzeugt protein in-file in prot*in-string und fuegt es in protein-problem hinzu
    (prot~add-in-string! protein-problem
			 (append (list "protein_flag(trace,internal)."))
			 (res~proof-clauses res-proof))

    ;; call-protein vor Ort -> schreibt protein out-file in den out-string des protein-problems
    (prot=call-protein! protein-problem protein-problem-dir ressource)
    
    ;; parsen des protein-beweises
    (prot~complete-protein-problem! protein-problem :parse-back parse-back)))


(defun prot=call-protein! (protein-problem protein-problem-dir ressource)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A protein-problem, the directory for input, output and a time-ressource.")
	   (effect  "Writes the in-string of the protein-problem to the file protein.tme in the"
		    "directory, calls protein on it, reads the file protein.tree from the directory"
		    "and writes it into the out-string of the protein-problem.")
	   (value   "Undefined."))
  
  (let* ((in-file (merge-pathnames "protein.tme" protein-problem-dir))
	 (prot-out-file (merge-pathnames "protein.out" protein-problem-dir))
	 (prot-tree-file (merge-pathnames "protein.tree" protein-problem-dir))
	 (prot-trc-file (merge-pathnames "protein.trc" protein-problem-dir)))

    (atptop~print-string-in-file (atpprb~problem-atp-in-string protein-problem) in-file)
    
    ;; call protein
    (let* ((call-flag (atptop~call-with-time-ressource (format nil "cd ~A;~A protein >! temp.out;mv temp.out protein.out &"
							       protein-problem-dir (prot~program))
						       prot-out-file
						       "protein"
						       protein-problem-dir
						       ressource)))
      ;; der komplizierte Befehl bei atptop~call-with-time-ressource ist notwendig, da protein leider sofort sein out file zu schreiben
      ;; beginnt. Daher wird das eigentliche out-file erst nachdem protein fertig ist geschrieben.
      
      (if (null call-flag)
	  (progn
	    (omega~message "~% Protein was not able to find a proof in the given time ressource. ~%")
	    (setf (atpprb~problem-atp-out-string protein-problem)
		  (format nil "~A~A%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%~A~A"
			  ""
			  #\Newline
			  #\Newline
			  "")))
	
	
	;; if the proof was a success, the protein.tree (NICHT protein.OUT!!!!!!!) file will exists
	;; aber genauso wird auch das File protein.trc File benoetigt. 
	;; -> read protein.tree und protein.trc und setzte sie in folgender weise zusamman
	;; -> and set atp-out-string of the protein-problem

	(let* ((trc-string (if (probe-file prot-trc-file)
			       (atptop~read-file-as-string prot-trc-file)
			     ""))
	       (tree-string (if (probe-file prot-tree-file)
				(atptop~read-file-as-string prot-tree-file)
			      "")))
	  (setf (atpprb~problem-atp-out-string protein-problem)
		(format nil "~A~A%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%~A~A"
			tree-string
			#\Newline
			#\Newline
			trc-string)))
		   ;; Reihenfolge im PROTEIN-ATP-OUT-STRING:
		   ;; 1. protein.trc File
		   ;; 2. %%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%% Als Separator
		   ;; 3. protein.tree File
	))))
 
;; hiermit gilt:
;; atp-out-string beginnt mit #\Newline%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%
;;                                            -> kein protein.tree file da
;;                                            -> Beweis fehlgeschlagen
;; atp-out-string beginnt anders -> protein.tree file ist da -> Beweis geglueckt -> parsbar
      
(defun prot~generate-protein-problem-default! (conclusion-node assumption-nodes ho-pds)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node, a set of assumption-nodes and the pds the nodes are from.")
	   (effect  "The created protein-problem is inserted into the list in the plist entry"
		    "'atp-problems of the conclusion node.")
	   (value   "A atp-problem with type protein, a partial resolution proof."
		    "The global-vars are set."
		    "The atp-in-string is also already set."))
  (let* ((protein-problem (prot~generate-protein-problem conclusion-node assumption-nodes ho-pds)))
    
    (prot~add-in-string! protein-problem
			 (append (list "protein_flag(trace,internal)."))
			 (res~proof-clauses (atpprb~problem-part-res-proof protein-problem))
			 :docu nil)
    
    (keim~put conclusion-node 'atp-problems (cons protein-problem (keim~get conclusion-node 'atp-problems)))
    
    protein-problem))


#| -------------------------------------------------- Read free stuff for Protein --------------------------------------------------- |#

;; Notice:
;; The protein out-file has to consists of the following parts:
;; 1. The protein .tree file
;; 2. The string "%%%%% THIS WAS THE .TREE FILE, NEXT IS THE .TRC FILE %%%%%"
;; 3. The protein .trc file

(defun prot~read-protein-output (open-node file)
  (declare (edited  "02-JUN-2000")
	   (authors Ameier)
	   (input   "An open node and a file.")
	   (effect  "Mayby changes the protein global variables.")
	   (value   "1. For the open node a new resolution proof is created."
		    "2. The file is tried to read as a protein proof for this resolution proof"
		    "   (notice that the file has to contain both the protein.tree and the"
		    "    protein.trc file!). In particular, a mapping is computed from the"
		    "   initial clauses of the protein file and the initial clauses of the"
		    "   new reslution proof."
		    "If it was possible to read the file as a protein proof file, the resolution"
		    "proof is completes (an empty clauses is dreived according to the proof in"
		    "the file) and this complete resolution proof is returned."
		    "If it was not possible to read the file as a protein proof file nil is"
		    "returned."))
  (let* ((protein-problem (prot~generate-protein-problem open-node
							 (remove open-node (pds~node-supports open-node))
							 omega*current-proof-plan))
	 (res-proof (prot~complete-protein-problem-from-file! protein-problem file)))
    
    res-proof))

(defun prot~complete-protein-problem-from-file! (atp-problem file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (res-proof (atpprb~problem-part-res-proof atp-problem))
	 (global-vars (atpprb~problem-global-vars atp-problem)))
    
    ;; was ist mit translation settings? -> braucht man zumindest nicht fuer reine first order probleme

    (setf (atpprb~problem-atp-out-string atp-problem) out-string)
      
    ;; Note: in the atp-problem we have in the global-var-list:
    ;; 1.  prot*convert-list       -> we have to reconstruct by matching the clauses!
        
    (setq prot*convert-list (prot=reconstruct-convert-list! res-proof out-string))
       
    ;; Other necessary settings
    (setq prot*current-problem res-proof)
    (setq prot*current-environment (res~proof-environment res-proof))
    (setq prot*temporary-variables nil)                      ;; setzt temporaeri variablen auf nil
    (setq prot*just-counter 0)
    
    (if (null prot*convert-list)
	(progn
	  (omega~message "~% Could not match the out-file to the problem.")
	  nil)
      (let* ((proof-flag (prot=read-with-parsing res-proof out-string)))
	
	;; remove temporary-variables
	(mapcar #'(lambda (var)
		    (env~remove (keim~name var) prot*current-environment))
		prot*temporary-variables)
	
	(if proof-flag
	    (progn
	      (omega~message "~% Protein has found a proof.~%")
	      (setf (res~proof-empty-clause res-proof)
		    (otter=find-empty-clause (res~proof-step-clauses res-proof)))
	      (setq omega*current-resolution-proof res-proof)
	      (res~add-proof-in-hash-table res-proof)		  
	      (atptop~order-resolution-steps! res-proof)
	      (omega~message "~% OMEGA*CURRENT-RESOLUTION-PROOF IS SET TO THE FOUND RESOLUTION PROOF")
	      res-proof)
	  nil)))))

(defun prot=reconstruct-convert-list! (res-proof out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "The current resolution proof and the out-string of a protein call.")
	   (effect  "The content of prot*convert-list and prot*local-clause-vars can be changed.")
	   (value   "First the input clauses of the proof in the out-string are read. Thereby, the new"
		    "created constants have as names exactly the names they have in the out-file."
		    "Then these read clauses are matched against the input clauses in the resolution proof."
		    "This results (if successfull) in a mapping, that mapps each constant in the"
		    "read clauses to a constant in the input clauses. From this mapping we compute"
		    "a list of pairs of constants and strings (where the constant is taken from the"
		    "new resolutiomn proof whereas the string is from the out-file."))

  (let* ((res-proof-input-clauses (res~proof-clauses res-proof))
	 (out-file-input-clauses (prot=read-input-clauses out-string)))
    
    (multiple-value-bind
	(success mapping info-triples)
	(atptop~subset-by-equality-except-names-p out-file-input-clauses res-proof-input-clauses)
      ;; It can happen that protein uses directly some factors of input clauses -> complete the set by all possible
      ;; factors!
      
      (if success

	  (progn
	    (atptop~change-resolution-proof! res-proof info-triples)

	    (let* ((mapp-domain (mapp~domain mapping))
		   (mapp-codomain (mapp~codomain mapping))
		   (mapp-constant-domain (remove-if-not #'term~constant-p mapp-domain))
		   (mapp-constant-codomain (remove-if-not #'term~constant-p mapp-codomain)))
	      
	      (mapcar #'(lambda (dom codom)
			  (let* ((name (keim~name dom)))
			    (list codom (if (stringp name)
					    name
					  (string name)))))
		      mapp-constant-domain mapp-constant-codomain)))
	nil))))

(defun prot=read-input-clauses (out-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A protein outfile.")
	   (effect  "During the parsing of the input clauses (see value) the prot*convert-list is changed.")
	   (value   "Reads the input clauses, without a convertion setting."
		    "Thereby, for each name that starts with a capital letter a new variable of type i is produced, and"
		    "for each other name a constant of type (i <- ...) is produced."))
  
  (setf prot*convert-list nil)
  
  (let* ((line-strings (atptop~divide-string out-string #\Newline))
	 (initial-clause-lines (do* ((rest-string-lines line-strings (rest rest-string-lines))
				     (break-flag nil)
				     (read-flag nil)
				     (return-list nil))
				   ((or (null rest-string-lines)
					break-flag)
				    return-list)
				 (let* ((string-line (first rest-string-lines)))
				   (cond ((string= string-line "--------------------        flags       --------------------")
					  (setf return-list (butlast return-list)) ;; the last is ""
					  (setf break-flag 't))
					 ((string= string-line "--------------------    input clauses   --------------------")
					  (setf rest-string-lines (rest rest-string-lines)) ;; skip one line (containing "" only)
					  (setf read-flag 't))
					 (read-flag
					  (setf return-list (append return-list (list string-line))))))))
	 ;; Note:
	 ;; If a input clause is parsed, each name that starts with "_" is interpreted as variable (e.g., _7146, _7150)!
	 ;; -> a new local variable of type i is created (local = relevant only for the clause itself) 
	 ;; Each other letter is interpreted as a constant. Hence, e new constant is created. The type of the constant
	 ;; is (o <- i ...) or (i <- i ...) depending on whether the constant is the predicat of a literal or internal
	 ;; and on the number of the arguments on which it is applied.
	 ;; Since constants are not local for a clause, for each new created constant a new entry is made in the
	 ;; prot*convert-list
	 ;; Furthermore, each literal whose predicate-symbol starts with "not_" is interpreted as negative literal.
	 ;; All clause-lines that start with "Query" are ignored
	 (input-clauses (apply #'append (mapcar #'(lambda (line)
						    (prot=parse-free-input-clause-line line))
						initial-clause-lines))))
    input-clauses))

(defun prot=parse-free-input-clause-line (input-clause-line)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a input clause.")
	   (effect  "The variables prot*local-clause-vars and prot*convert-list can be changed"
		    "(prot*local-clause-vars is set to nil at the beginning, then new local variables"
		    " are added. For prot*convert-list see prot=read-input-clauses.")
	   (value   "A list containing the new created clause."))
  
  (setf prot*local-clause-vars nil)

  (multiple-value-bind
      (description rest-string)
      (atptop~get-next-word (atptop~cut-first-char input-clause-line) #\")
    
    (if (string= description "Query  ")
	nil
      (multiple-value-bind
	  (prefix literal-list-string)
	  (atptop~get-next-word rest-string #\: :ignore-char-list (list #\[ #\] #\space))

	(let* ((literal-list (do* ((rest-literal-strings (blik=separate-literals literal-list-string) (rest rest-literal-strings))
				   (literal-list nil))
				 ((null rest-literal-strings)
				  literal-list)
			       (let* ((head-literal (first rest-literal-strings)))
				 (setf literal-list (append literal-list
							    (list (prot=parse-free-literal head-literal))))))))
	  (list (cl~create literal-list)))))))

(defun prot=parse-free-literal (literal-string)
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string representing a literal.")
	   (effect  "See prot=parse-free-input-clause-line.")
	   (value   "A literal."))
  (if (atptop~string-is-prefix-of-string-p "not_" literal-string)
      (lit~literal-create (prot=parse-free-term (atptop~cut-x-first-chars literal-string 4)
						:predicat 't)
			  nil)
    (lit~literal-create (prot=parse-free-term literal-string :predicat 't) 't)))

(defun prot=parse-free-term (term-string &key (predicat nil))
   (multiple-value-bind
      (functor-string rest-string)
      (atptop~get-next-word term-string #\()
     ;; reads till a "(" is reached 
     ;; if rest-string = "" (empty) -> term isn't an application -> otherwise term is an application
     
     (if (string= rest-string "")
	 (prot=free-string2object functor-string :predicat predicat :number-of-args 0)
	(let* ((args (mapcar #'(lambda (term-string)
				 (prot=parse-free-term term-string))
			     (prot=parse-term-list (atptop~cut-last-char rest-string))))
	       (functor (prot=free-string2object functor-string :predicat predicat :number-of-args (length args))))
	  (term~appl-create functor args)))))

(defun prot=free-string2object (string &key (predicat nil) (number-of-args 0))
  (declare (edited  "29-MAY-2000")
	   (authors Ameier)
	   (input   "A string, and as keywords predicat (to sign whether the string should be intrpreted as"
		    "predicat or as function) and number-of-args (to sign how many arguments the premitive"
		    "corresponding to the string should have.")
	   (effect  "The prot*convert-list and prot*local-clause-vars can be changed:"
		    "1.) If the string starts with a "_" and it is not alsready conatined in an entry"
		    "    in the prot*local-clause-vars, a new variable with type i is created and a corresponding"
		    "    entry is made in the prot*local-clause-vars."
		    "2.) Othwewise: if the string is not contained in an entry in the prot*convert-list, a new"
		    "    constant (whose type depends on the keywords predicat and number-of-args) is created"
		    "    and a corresponding entry is added to prot*convert-list.")		    
	   (value   "The object corresponding wrt. prot*convert-list or prot*local-clause-vars to the string."))
  (let ((member-convert-list (first (find string prot*convert-list
					  :test #'(lambda (string pair)
						    (string= string (second pair))))))
	(member-local-clause (first (find string prot*local-clause-vars
					      :test #'(lambda (string pair) (string= string (second pair)))))))

    (cond ((string-equal string "=")
	   (env~lookup-object '= prot*current-environment))

	  (member-convert-list
	   ;; -> string is already in prot*convert-list -> give back the corresponding object
	   
	   member-convert-list)
	  
	  (member-local-clause
	   ;; -> string is already in prot*local-clause-vars -> return it

	   member-local-clause)

	  (;; string neither in prot*convert-list nor prot*local-clause-vars
	   ;; -> create new object and add entry to prot*convert-list or prot*local-clause-vars
	   
	   (if (equal (char string 0) #\_)
	       ;; -> first letter of string is _
	       ;; -> produce new variable and add it to prot*local-clause-vars
	       
	       (let* ((new-var (term~generate-term-primitive-with-new-name 'orv- (type~i) 'term+variable prot*current-environment)))
		 
		 (setq prot*temporary-variables (cons new-var prot*temporary-variables))
		 (setq prot*local-clause-vars (cons (list new-var string) prot*local-clause-vars))
		 
		 new-var)

	     ;; first letter of string is not "_"
	     ;; -> produce new constant and add it to prot*convert-list
	     
	     (let* ((type (if predicat 
			      (type~predicate-create number-of-args)
			    (type~function-create number-of-args)))
		    (new-constant (term~constant-create string type)))
	       
	       (setq prot*convert-list (cons (list new-constant string) prot*convert-list))
	       
	       new-constant))))))
