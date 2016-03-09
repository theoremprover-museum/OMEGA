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



(mod~defmod SPLIT 
            :uses (cl extdelta gsub keim lit node pdsn pos r2ntop ref res subst)
            :documentation "Splitting of a refutation graph."
            :exports (
                      
                      split~get-list-of-literal-lists-with-position
		      split~splitting-ref-graph-clauses!
		      split~splitting-ref-graph!
                      
                      split*clause-counter
                      split*just-counter
                      split*link-counter
                      split*literal-list
                      split*ref-graph-counter))







(defvar split*clause-counter 0)
(defvar split*just-counter 0)
(defvar split*ref-graph-counter 0)
(defvar split*link-counter 0)
(defvar split*literal-list nil)

#| -------------------------------------------------------- AUXILIARIES ----------------------------------------------- |#

(defun split=copy-relation-pair (pair)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A extdelta-relation-pair.")
	   (effect  "None.")
	   (value   "A new instance of a extdelta-relation-pair, with eq slots as"
		    "the input pair."))
  (extdelta~create-delta-pair (extdelta~formula pair) (extdelta~formula-position pair)
			      (extdelta~pdsnode pair) (extdelta~clause pair) (extdelta~clause-position pair)))

(defun split=check-delta-relation (refutation-graph delta-relation)
  (declare (edited  "06-APR-1998")
	   (authors Ameier)
	   (input   "A refutation graph and a delta-relation.")
	   (effect  "None.")
	   (value   "A new delta-relation (also the single pairs are copied, not shared), that contains"
		    "only that pairs of the input delta-relation, that accord to clauses of the"
		    "refutation graph."))
  (let* ((clause-nodes (ref~clause-nodes refutation-graph)))
    (make-instance 'extdelta+delta-relation
		   :relation-pairs
		   (mapcar #'split=copy-relation-pair
			   (remove-if-not #'(lambda (pair)
					      (find (extdelta~clause pair) clause-nodes))
					  (extdelta~relation-pairs delta-relation))))))
	 
(defun split=create-split-clause (old-clause literals &key (ignore nil))
  (declare (edited  "29-MAR-1996")
	   (authors Ameier)
	   (input   "A clause and a set of literals ,who should be a subset of the"
		    "literals of the clause and the keyword ignore (default nil) ,who"
		    "show's if the ref-graph according to this part of the split"
		    "is unnecassary.")
	   (effect  "The pairs of new-literal and old-literal are added to the split*literal-list"
		    "if ignore isn't nil otherwise they are ignored.")
	   (value   "Creates a new clause with the set of literals and take from the plist"
		    "of the initial clause the ground-substitution and add it to the plist"
		    "of the new clause. Remark the literals are created new."
		    "Returns a multiple-value,"
		    "First the new-clause, second a list of pairs. A pair is a list of"
		    "a literal from the old-clause and the corresponding literal from"
		    "the new clause."))
  (let* ((new-name (intern (string-upcase (format nil "~A-split-~A" (keim~name old-clause) (incf split*clause-counter)))
			   (find-package :omega)))
	 (new-literals (mapcar #'(lambda (literal)
	 			   (lit~literal-create (lit~atom literal)
	 					       (lit~positive-p literal)))
	 		       literals))
	 (new-just (res~initial-create (intern (string-upcase (format nil "Split-~A" (incf split*just-counter)))
					       (find-package :omega))))
	 (new-clause (r2ntop~create-trans-clause new-literals
						 :name new-name
						 :justification new-just
						 :renaming (r2ntop~trans-clause-renaming old-clause))))
    
    (setf (r2ntop~trans-clause-ground-subst new-clause)
	  (gsub~check-ground-subst new-clause (r2ntop~trans-clause-ground-subst old-clause)))
    
    (if (null ignore)
	(setq split*literal-list (append (mapcar #'list new-literals literals) split*literal-list)))
    (values
     new-clause
     (mapcar #'list literals new-literals))))

(defun split=replace-old-literals-by-news-in-links (refutation-graph literal-pair-list)
  (declare (edited  "29-MAR-1996")
	   (authors Ameier)
	   (input   "A refutation graph, and a list of literal-pairs.")
	   (effect  "The first literal of each pair is replaced by the second literal of the"
		    "pair in all links of the refutation-graph.")
	   (value   "Undefined."))
  (let ((links (ref~links refutation-graph)))
    (mapcar #'(lambda (literal-pair)
		(let ((old-literal (first literal-pair))
		      (new-literal (second literal-pair)))
		  (mapcar #'(lambda (link)
			      (if (lit~positive-p old-literal)
				  (let ((pos-literals (ref~positive-shore link)))
				    (if (member old-literal pos-literals)
					(setf (ref~positive-shore link)
					      (remove old-literal (cons new-literal pos-literals)))))
				(let ((neg-literals (ref~negative-shore link)))
				  (if (member old-literal neg-literals)
				      (setf (ref~negative-shore link)
					    (remove old-literal (cons new-literal neg-literals)))))))
			  links)))
	    literal-pair-list)))

(defun split~get-list-of-literal-lists-with-position (clauses delta-pairs position)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A set of clauses, a set of delta-relation-pairs and a position.")
	   (effect  "None.")
	   (value   "The set of literal from each delta-pairs, whose clause is element"
		    "of the set of clauses, and whose formula-position has position as"
		    "prefix."))
  (mapcar #'(lambda (clause)
	      (let ((literals (cl~literals clause))
		    (pairs-of-clause-with-pos (remove-if-not #'(lambda (pair)
								 (and 
								  (eq (extdelta~clause pair) clause)
								  (pos~prefix-p position (extdelta~formula-position pair))))
							     delta-pairs)))
		(mapcar #'(lambda (pair)
			    (let ((clause-position (extdelta~clause-position pair)))
			      (r2ntop~term-at-position literals clause-position)))
			pairs-of-clause-with-pos)))
	  clauses))						

(defun split=add-literal-to-link (literal-to-add link)
  (declare (edited  "29-MAR-1996")
	   (authors Ameier)
	   (input   "A literal and a link.")
	   (effect  "The literal is added to the correct shore of the link.")
	   (value   "Undefined."))
  (if (lit~positive-p literal-to-add)
      (setf (ref~positive-shore link) (cons literal-to-add (ref~positive-shore link)))
    (setf (ref~negative-shore link) (cons literal-to-add (ref~negative-shore link)))))

#| ----------------------------------------------------- MAIN ----------------------------------------------------------- |#

(defun split~splitting-ref-graph! (refutation-graph delta-relation node-to-split
						    split-node1 split-node2  
						    planned-node1 planned-node2)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph, the delta-relation, the node, that should be split"
		    "the two result-nodes of splitting and the two new planned-nodes.")
	   (effect  "Refutation-graph and delta-relation could be changed between the process."
		    "The refutation-graph is splittet into two new-refutation-graphs for"
		    "the new planned-nodes according to the node to split. Also the corresponding"
		    "delta-relations are created and updated.")
	   (value   "Multiple-value:"
		    "First: A list consisting of the planned-node1, the according refutation graph"
		    "       and the according delta-relation."
		    "Second: A similar list for the second planned-node."))
  
  (setq r2ntop*check-flag 't)
  (setq split*clause-counter 0)
  (setq split*just-counter 0)
  (setq split*ref-graph-counter 0)
  (setq split*link-counter 0)
  (setq split*literal-list nil)

  (let* ((pairs-of-node-to-split (remove-if-not #'(lambda (pair)
						    (eq (extdelta~pdsnode pair) node-to-split))
						(extdelta~relation-pairs delta-relation)))
	 (clauses-to-split (remove-duplicates (mapcar #'extdelta~clause pairs-of-node-to-split)))
	 (list-of-literal-lists1 (split~get-list-of-literal-lists-with-position clauses-to-split pairs-of-node-to-split
										(pos~list-position '(1))))
	 (list-of-literal-lists2 (split~get-list-of-literal-lists-with-position clauses-to-split pairs-of-node-to-split
										(pos~list-position '(2)))))
    (multiple-value-bind
	(new-ref-graph1 new-ref-graph2)
	(split=splitting-clause-list refutation-graph clauses-to-split list-of-literal-lists1 list-of-literal-lists2)
      (split=update-extdelta-relation delta-relation pairs-of-node-to-split split-node1 split-node2)
      
      (let* ((new-delta-relation1 (split=check-delta-relation new-ref-graph1 delta-relation))
	     (new-delta-relation2 (split=check-delta-relation new-ref-graph2 delta-relation)))
	
	;; NEW:
	
	(split=copy-clauses-in-ref-graph! new-ref-graph1 new-delta-relation1)
       	(split=copy-clauses-in-ref-graph! new-ref-graph2 new-delta-relation2)
	
	;; END NEW !
	
	(values
	 (list planned-node1 new-ref-graph1 new-delta-relation1)
	 (list planned-node2 new-ref-graph2 new-delta-relation2))))))



(defun split~splitting-ref-graph-clauses! (refutation-graph delta-relation clauses-to-split
							    list-of-literal-lists1
							    list-of-literal-lists2
							    planned-node1 planned-node2)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph, a delta-relation, the set of clauses-to-split,"
		    "a list of literal-lists from the according clauses-to-split who should"
		    "stand in the left split-clauses, a analog list for the rigth split-clauses."
		    "The new planned-nodes.")
	   (effect  "Refutation-graph and delta-relation could be changed between the process."
		    "The refutation-graph is splittet into two new-refutation-graphs for"
		    "the new planned-nodes according to the set of clauses-to-split."
		    "the delta-relations for this nodes are created updated and the justifications"
		    "for the planned nodes are created and set.")
	   (value   "Multiple-value:"
		    "First: A list consisting of the planned-node1, the according refutation graph"
		    "       and the according delta-relation."
		    "Second: A similar list for the second planned-node."))
  
  (setq r2ntop*check-flag 't)
  (setq split*clause-counter 0)
  (setq split*just-counter 0)
  (setq split*ref-graph-counter 0)
  (setq split*link-counter 0)
  (setq split*literal-list nil)
  
  (let ((pairs-to-split (remove-if-not #'(lambda (pair)
					   (member (extdelta~clause pair) clauses-to-split))
				       (extdelta~relation-pairs delta-relation))))
    (multiple-value-bind
	(new-ref-graph1 new-ref-graph2)
	(split=splitting-clause-list refutation-graph clauses-to-split list-of-literal-lists1 list-of-literal-lists2)
      (split=update-extdelta-relation delta-relation pairs-to-split planned-node1 planned-node2)

      (let* ((new-delta-relation1 (split=check-delta-relation new-ref-graph1 delta-relation))
	     (new-delta-relation2 (split=check-delta-relation new-ref-graph2 delta-relation)))

	;; NEW:
	
	(split=copy-clauses-in-ref-graph! new-ref-graph1 new-delta-relation1)
       	(split=copy-clauses-in-ref-graph! new-ref-graph2 new-delta-relation2)

	;; END NEW !
	
	(values
	 (list planned-node1 new-ref-graph1 new-delta-relation1)
	 (list planned-node2 new-ref-graph2 new-delta-relation2))))))


(defun split=copy-clauses-in-ref-graph! (ref-graph delta-relation)
  (declare (edited  "17-APR-1998")
	   (authors Ameier)
	   (input   "A refutation graph and a delta-relation.")
	   (effect  "All clauses in the refutation graph are copied (downto data+primitive)."
		    "The plist of the new clauses is set to the plist of the old clauses"
		    "(without copying it). The delta-relation is updated by replaceing the old"
		    "clauses by the new. Also in the links, the old clauses are replaced by the new.")
	   (value   "The upadted refutation Graph."))
  (let* ((clauses (ref~clause-nodes ref-graph))
	 (new-clauses (mapcar #'(lambda (clause)
				  (keim~copy clause :downto '(data+primitive))

				  
				  ;; wird alles automatisch ueber die keim~copy function kopiert RENAMING + GROUND-SUBST!
				  ;; paramod-position im Fall von paramod-clauses wird automatisch gesetzt
				  ;; keim~copy auf r2ntop+paramod-clause 's
				  
				  )
			      clauses)))
    
    (setf (ref~clause-nodes ref-graph) new-clauses)
    
    (mapcar #'(lambda (delta-pair)
		(let* ((pair-clause (delta~delta-clause delta-pair))
		       (according-new-clause (data~assoc pair-clause clauses new-clauses #'eq)))
		  (setf (delta~delta-clause delta-pair) according-new-clause)))
	    (delta~relation-pairs delta-relation))
    
    (mapcar #'(lambda (link)
		(let* ((pos-shore (ref~positive-shore link))
		       (neg-shore (ref~negative-shore link)))
		  (setf (ref~positive-shore link)
			(mapcar #'(lambda (literal)
				    (let* ((clause (lit~clause literal))
					   (pos (position literal (cl~literals clause) :test #'eq))
					   (according-new-clause (data~assoc clause clauses new-clauses #'eq)))
				      (nth pos (cl~literals according-new-clause))))
				pos-shore))
		  (setf (ref~negative-shore link)
			(mapcar #'(lambda (literal)
				    (let* ((clause (lit~clause literal))
					   (pos (position literal (cl~literals clause) :test #'eq))
					   (according-new-clause (data~assoc clause clauses new-clauses #'eq)))
				      (nth pos (cl~literals according-new-clause))))
				neg-shore))))
	    (ref~links ref-graph))
    
    
    ref-graph))
  
			      
      

(defun split=update-extdelta-relation (delta-relation pairs-of-node-to-split split-node1 split-node2)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A delta-relation ,the split-pairs from this delta-relation, who accords"
		    "to clauses or nodes,who has been splittet, and the new splitte-nodes.")
	   (effect  "The delta-relation is updated by replacing all of the split-pairs"
		    "by pairs for the new split-nodes.")
	   (value   "Undefined."))
  (terpri)
  
  (let ((new-pairs (apply 'append
			  (mapcar #'(lambda (pair)
				      (let* ((clause-position (extdelta~clause-position pair))
					     (clause (extdelta~clause pair))
					     (literals (cl~literals clause))
					     (literal (r2ntop~term-at-position literals clause-position))
					     (new-literal (first (first (member literal split*literal-list
										:test #'(lambda (item1 item2)
											  (eq item1 (second item2))))))))
					(if new-literal
					    (let* ((formula-position (extdelta~formula-position pair))
						   (new-clause (lit~clause new-literal))
						   (new-clause-position
						    (r2ntop~position-of-literal new-literal (cl~literals new-clause)))
						   (new-formula-position (pos~rest formula-position))
						   (new-pdsnode (if (= (pos~first formula-position) 1)
								   split-node1
								 split-node2))
						   (new-formula (r2ntop~new-named-term (node~formula new-pdsnode))))
					      (list (extdelta~create-delta-pair new-formula new-formula-position
										new-pdsnode new-clause new-clause-position)))
					  nil)))
				  pairs-of-node-to-split))))
    (setf (extdelta~relation-pairs delta-relation)
	  (append new-pairs (r2ntop~remove-list pairs-of-node-to-split
						(extdelta~relation-pairs delta-relation))))))


(defun split=splitting-clause-list (refutation-graph list-of-clauses list-of-literal-lists1 list-of-literal-lists2 &key (ignore nil))
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph a list of clauses to split, and implicit the new"
		    "split-clauses for each of the clauses to split, by giving for each"
		    "clause-to-split the literals for the new left split-clause (stand all"
		    "in the first list-of-literal-lists) and the new rigth split-clause"
		    "(stand all in the second list-of-literal-lists)."
		    "The keyword ignore show's if one of the created ref-graphs of this step"
		    "is unnecassary (compare algorithm in paper), this causes that the"
		    "literal pairs of this unnecassary ref-graph aren't added to the"
		    "split*literal-list.")
	   (effect  "None.")
	   (value   "As multiple-value:"
		    "The two refutation-graph resulting from splitting of all clauses."
		    "First the refutation-graph of the left-split-clauses, second"
		    "the refutation-graph of the rigth-split-clauses."))
  (if list-of-clauses
      (let ((clause (first list-of-clauses))
	    (literal-list1 (first list-of-literal-lists1))
	    (literal-list2 (first list-of-literal-lists2)))
	(multiple-value-bind
	    (ref-graph1 ref-graph2)
	    (split=splitting-clause refutation-graph clause literal-list1 literal-list2 :ignore ignore)
	  (let ((updated-ref-graph2 (if (null ref-graph2)
					refutation-graph ;; clause-to-split not element of ref-clause-set -> nil
				      (split=update-link-pair-list! ref-graph2 refutation-graph)))
		(updated-ref-graph1 (split=update-link-pair-list! ref-graph1 refutation-graph))) 
	    (cond ((equal ignore 'left)
		   (multiple-value-bind
		       (rigth-ref1 rigth-ref2)
		       (split=splitting-clause-list updated-ref-graph2 (rest list-of-clauses) (rest list-of-literal-lists1)
						    (rest list-of-literal-lists2) :ignore 'left)
		     (if (and (null rigth-ref1) (null rigth-ref2))
			 (values nil updated-ref-graph2)
		       (values nil rigth-ref2))))
		  ((equal ignore 'rigth)
		   (multiple-value-bind
		       (left-ref1 left-ref2)
		       (split=splitting-clause-list updated-ref-graph1 (rest list-of-clauses) (rest list-of-literal-lists1)
						    (rest list-of-literal-lists2) :ignore 'right) 
		     (if (and (null left-ref1) (null left-ref2))
			 (values updated-ref-graph1 nil)
		       (values left-ref1 nil))))
		  (t
		   (multiple-value-bind
		       (left-ref1 left-ref2)
		       (split=splitting-clause-list updated-ref-graph1 (rest list-of-clauses) (rest list-of-literal-lists1)
						    (rest list-of-literal-lists2) :ignore 'right) 
		     (multiple-value-bind
			 (rigth-ref1 rigth-ref2)
			 (split=splitting-clause-list updated-ref-graph2 (rest list-of-clauses) (rest list-of-literal-lists1)
						      (rest list-of-literal-lists2) :ignore 'left)
		       (if (and (null left-ref1) (null left-ref2) (null rigth-ref1) (null rigth-ref2))
			   (values updated-ref-graph1 updated-ref-graph2)
			 (values left-ref1 rigth-ref2)))))))))
    (values nil nil)))

(defun split=update-link-pair-list! (new-ref-graph old-ref-graph)
  (declare (edited  "08-MAY-1996")
	   (authors Ameier)
	   (input   "Two refutation-graphs, first is result of splitting the second.")
	   (effect  "If in the plist-slot link-pair-list of the old-ref-graph is set,"
		    "(that means this is also a result of a splitting)"
		    "the link-pair-list of new-ref-graph is updatet by making link-pairs"
		    "from old-links (in link-pairs of old-ref-graph) to new-links"
		    "(in link-pairs of new-ref-graph).")
	   (value   "New-ref-graph."))
  (let ((old-link-pair-list (keim~get old-ref-graph 'link-pair-list)))
    
    (if (and old-link-pair-list (not (eq old-ref-graph new-ref-graph)))
	;; this means,that old-ref-graph himself is already the result of a splitting.
	;; so it contains in plist-slot link-pair-list a mapping from links of the original ref-graph
	;; and its links. The new-ref-graph contains in plist-slot link-pair-list a mapping of
	;; links in old-ref-graph and its links, now create a mapping from links in original ref-graph
	;; and the links in the new-ref-graph.
	(let ((new-link-pair-list (keim~get new-ref-graph 'link-pair-list)))
	  (keim~put new-ref-graph 'link-pair-list
		    (mapcar #'(lambda (new-link-pair)
				(let* ((link-in-old-ref-graph (first new-link-pair))
				       (link-in-new-ref-graph (second new-link-pair))
				       (link-pair-of-old-link-pair-list-with-link-in-old-ref-graph-as-second
					(first (member link-in-old-ref-graph old-link-pair-list
						       :test #'(lambda (link link-pair)
								 (eq link (second link-pair)))))))
				  (list (first link-pair-of-old-link-pair-list-with-link-in-old-ref-graph-as-second)
					link-in-new-ref-graph)))
			    new-link-pair-list)))
      ;; if in old-ref-graph the plist-slot link-pair-list isn't set, the old-ref-graph was the original ref-graph
      ;; so the link-pair-list in new-ref-graph is already a mapping from the original ref-graph links to
      ;; new-ref-graph links -> nothing to do
      ))
  new-ref-graph)

(defun split=splitting-clause (refutation-graph clause literal-list1 literal-list2 &key (ignore nil))
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph, a clause to split and the literals of clause"
		    "for the new left split-clause, and the literals of clause for"
		    "the new rigth split-clause."
		    "The keyword ignore show's if one of the created ref-graphs of this step"
		    "is unnecassary (compare algorithm in paper), this causes that the"
		    "literal pairs of this unnecassary ref-graph aren't added to the"
		    "split*literal-list.")
	   (effect  "The old refutation-graph is destroyed.")
	   (value   "Multiple-value:"
		    "The both splittet refutation-graphs:"
		    "First: The refutation-graph who contains the left split-clause,"
		    "second: The refutation-graph who contains the rigth split-clause."
		    "If one of the graphs isn't needed (ignore) is returned instead"
		    "of the graph a nil. If no splitting is needed (clause to split"
		    "isn't in ref-graph) the first value is the input ref-graph himself"
		    "and the second is nil."))
  (if (member clause (ref~clause-nodes refutation-graph) :test 'eq)
      ;; 1. Splitt the clause and update the refutation-graph by new-clauses
      (multiple-value-bind
	  (split-clause1 literal-pair-list1)
	  (if (equal ignore 'left)
	      (split=create-split-clause clause literal-list1 :ignore ignore)
	    (split=create-split-clause clause literal-list1))
	(multiple-value-bind
	    (split-clause2 literal-pair-list2)
	    (if (equal ignore 'right)
		(split=create-split-clause clause literal-list2 :ignore ignore)
	      (split=create-split-clause clause literal-list2))
	  (setf (ref~clause-nodes refutation-graph)
		(cons split-clause1 (cons split-clause2 (remove clause (ref~clause-nodes refutation-graph)))))
	  (split=replace-old-literals-by-news-in-links refutation-graph (append literal-pair-list1 literal-pair-list2))
	  ;; 2. create the two new ref-graphs
	  (let ((ref-graph1 (split=create-new-ref-graph-from-clause split-clause1 refutation-graph))
		(ref-graph2 (split=create-new-ref-graph-from-clause split-clause2 refutation-graph)))
	    ;; (ref~print-object ref-graph1 t)
	    ;; (ref~print-object ref-graph2 t)
	    (values ref-graph1 ref-graph2))))
    (values refutation-graph nil)))

(defun split=create-new-ref-graph-from-clause (split-clause old-refutation-graph)
  (declare (edited  "29-MAR-1996")
	   (authors Ameier)
	   (input   "A clause and a old-refutation-graph.")
	   (effect  "None.")
	   (value   "Creates a minimal refutation graph, that contains clause and"
		    "accords to the link-system of the old-refutation-graph."))
  (let ((new-refutation-graph
	 (ref~refutation-graph-create (intern (string-upcase (format nil "~A-split-~A"
								     (keim~name old-refutation-graph)
								     (incf split*ref-graph-counter)))
					      (find-package :omega))
				      (list split-clause)
				      nil
				      (subst~create () ())
				      (ref~ground-substitution old-refutation-graph))))
    (split=get-minimal-ref-graph new-refutation-graph old-refutation-graph (cl~literals split-clause) nil)))

(defun split=prefer-existing-clauses (literal-list existing-clauses)
  (do* ((rest-literals literal-list (rest rest-literals)))
      ((or (null rest-literals) (member (lit~clause (first rest-literals)) existing-clauses))
       (if (null rest-literals)
	   (values (first literal-list) (lit~clause (first literal-list)))
	 (values (first rest-literals) (lit~clause (first rest-literals)))))))

(defun split=get-minimal-ref-graph (new-ref-graph old-ref-graph unresolved-literals link-pair-list)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "The new-refutation-graph (by starting from split=create-new-ref-graph-from-clause"
		    "the empty ref-graph), the old-refutation-graph, a set of unresolved literals"
		    "in the new-refutation-graph, and a list of pairs of a link from the"
		    "new-ref-graph and a link from the old ref-graph ,which are at the same clauses.")
	   (effect  "A unresolved literal from the list of unresolved literals is token,"
		    "the according link in the old ref-graph is searched and so on the paper...")
	   (value   "The new ref-graph."))
  (if unresolved-literals
      (let* ((links-of-old-ref-graph (ref~links old-ref-graph))
	     (clauses-of-new-ref-graph (ref~clause-nodes new-ref-graph))
	     (literal-to-resolve (first unresolved-literals))
	     (clause-to-resolve (lit~clause literal-to-resolve))
	     (link-with-this-literal (first (remove-if-not
					     #'(lambda (link)
						 (let ((shore-literals (append (ref~positive-shore link)
									       (ref~negative-shore link))))
						   (member literal-to-resolve shore-literals)))
					     links-of-old-ref-graph)))
	     (partner-literals (if (lit~positive-p literal-to-resolve)
				   (ref~negative-shore link-with-this-literal)
				 (ref~positive-shore link-with-this-literal))))
	(multiple-value-bind
	    (partner-literal partner-clause)
	    (split=prefer-existing-clauses partner-literals clauses-of-new-ref-graph)
	  (cond ((member partner-clause clauses-of-new-ref-graph)
		 ;; this is possability 2. from paper: partner clause is already in the new-ref-graph
		 ;; -> the link 'link-with-this-literal' has already a corresponding-link 'cos-link'
		 ;;    in the new-graph (for proof see paper)
		 ;; -> seek the cos-link in the link-pair-list and add literal to the cos-link
		 ;;    remove the literal-to-resolve from the unresolved-literals list
		 (let ((cos-link (second (first (member link-with-this-literal link-pair-list
							:test #'(lambda (link link-pair)
								  (eq link (first link-pair))))))))
		   (split=add-literal-to-link literal-to-resolve cos-link)
		   (split=get-minimal-ref-graph new-ref-graph old-ref-graph (rest unresolved-literals) link-pair-list)))
		(t
		 ;; this is possability 1. from paper: partner clause isn't in the new-ref-graph
		 ;; create a new link for the new-graph ,add the pair (link-with-this-literal new-link)
		 ;; to the link-pair-list ,add the partner-clause to new-ref-graph
		 ;; remove literal-to-resolve from unresolved-literals list and add the other literals
		 ;; from partner-clause to this list
		 (let ((new-link (ref~link-create (intern (string-upcase (format nil "link-~A" (incf split*link-counter)))
							  (find-package :omega))
						  (list (list clause-to-resolve
							      (r2ntop~position-of-literal literal-to-resolve
											   (cl~literals clause-to-resolve)))
							(list partner-clause
							      (r2ntop~position-of-literal partner-literal
											   (cl~literals partner-clause))))
						  (subst~create () ())))
		       (other-literals-from-partner-clause (remove partner-literal (cl~literals partner-clause))))
		   (setf (ref~clause-nodes new-ref-graph)
			 (cons partner-clause (ref~clause-nodes new-ref-graph)))
		   (ref~add-link-to-graph new-ref-graph new-link)
		   (split=get-minimal-ref-graph new-ref-graph old-ref-graph
						(append other-literals-from-partner-clause (rest unresolved-literals))
						(cons (list link-with-this-literal new-link) link-pair-list)))))))
    (progn
      (keim~put new-ref-graph 'link-pair-list link-pair-list)
      new-ref-graph)))


				      


