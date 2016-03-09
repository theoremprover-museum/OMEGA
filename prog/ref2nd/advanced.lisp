
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




(mod~defmod ADV 
            :uses (cl extdelta lit plco r2ntop ref)
            :documentation "Further decomposition to receive advanced styled proofs."
            :exports (
                      
                      adv~get-smallest-node-to-split!
                      adv~make-further-decompose-in-sspu!
                      adv~smaller-sspu!
                      ))


#| --------------------------------------------------- Auxiliaries ------------------------------------------------------ |#

(defun adv=get-most-member-element (list)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of all elements of list which are the most frequently occurring ones in list."
		    "Second: The number of occurrences."
		    "If list is nil, return values are nil, 0."))
  (do* ((check-list list)
	(current-top-length 0)
	(current-top-member-list nil))
      ((null check-list) (values current-top-member-list current-top-length))
    (let* ((head (first check-list))
	   (head-in-check-list (remove-if-not #'(lambda (list-element)
						  (eq list-element head))
					      check-list))
	   (rest-check-list (remove-if #'(lambda (list-element)
						  (eq list-element head))
					      check-list))
	   (length-head-in-check-list (length head-in-check-list)))
      (cond ((> length-head-in-check-list current-top-length)
	     (setq current-top-length length-head-in-check-list)
	     (setq current-top-member-list (list head)))
	    ((= length-head-in-check-list current-top-length)
	     (setq current-top-member-list (cons head current-top-member-list)))
	    (t
	     nil))
      (setq check-list rest-check-list))))

 #| -------------------------------------------------------- Against SSPU ---------------------------------------------- |#

(defun adv~smaller-sspu! (unit)
  (declare (edited  "18-JUL-1996")
	   (authors Ameier)
	   (input   "A decomposition unit.")
	   (effect  "This function looks for plco's who are complementary."
		    "If there are no more nodes to decompose there is a new node"
		    "insert: -A v A (LEMMA) and this new node is directly returned for"
		    "splitting. So you separate two contrary plco's and get nearer"
		    "to sspu.")
	   (value   "If unit contains nodes to decompose or unit is sspu: nil,"
		    "otherwise: the new node -A v A. (A stands for a ground formula."))
  
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Getting smaller sspu by TND adding.")) 
  
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (ref-graph (r2ntop~decompose-unit-ref-graph unit))
	 (plco-pairs (r2ntop~decompose-unit-plco-pairs unit))
	 (nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit)))
    (multiple-value-bind
	(flag contrary-plco)
	(plco~sspu-refutable-p plco-pairs)
      (declare (ignore contrary-plco))
      (if (or flag nodes-to-decompose)
	  nil
	(let* ((first-polylink (adv=prefer-often-used-and-double-poly-links plco-pairs)))
	  (cond ((typep first-polylink 'plco+d2s-link)
		 (let* ((source-link (plco~source-link first-polylink))
			(new-pdsnode (r2ntop~insert-lemma-in-proof! source-link ref-graph delta-relation)))
		   (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list new-pdsnode))

		   (when r2ntop*trans-debug
		     (omega~message "~%~%r2ntop*trans-debug: Choosing new inserted node ~A." new-pdsnode))
		   
		   new-pdsnode))
		(t
		 (let ((new-pdsnode (r2ntop~insert-lemma-in-proof! first-polylink ref-graph delta-relation)))
		   (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list new-pdsnode))

		   (when r2ntop*trans-debug
		     (omega~message "~%~%r2ntop*trans-debug: Choosing new inserted node ~A." new-pdsnode))
		   
		   new-pdsnode))))))))

(defun adv=prefer-often-used-and-double-poly-links (contrary-plcos)
  (declare (edited  "31-JUL-1996")
	   (authors Ameier)
	   (input   "A list of contrary-plco's.")
	   (effect  "None.")
	   (value   "A Polylink to expand by trivial lemma."
		    "The function finds the link which is contained in the most contrary"
		    "polylink comparison-pairs or one which is part of double-poly-link."))
  (let* ((all-links (apply 'append (mapcar #'(lambda (contrary-plco)
					       (list (plco~first-polylink contrary-plco)
						     (plco~second-polylink contrary-plco)))
					   contrary-plcos)))
	 ;; next step deletes all links, whose polyshore is only a unit clause
	 (hard-poly-links (remove-if #'(lambda (contrary-link)
					 (if (not (typep contrary-link 'plco+d2s-link))
					     (let* ((pos-shore (ref~positive-shore contrary-link))
						    (neg-shore (ref~negative-shore contrary-link))
						    (single-shore (if (= (length pos-shore) 1)
								      pos-shore
								    neg-shore)))
					       (if (= (length (cl~literals (lit~clause (first single-shore)))) 1)
						   't))))
				     all-links))
	 (most-often-links (adv=get-most-member-element hard-poly-links)))
    (if (= (length most-often-links) 1)
	(first most-often-links)
      (let ((parts-of-double-polylink-links (remove-if-not #'(lambda (link)
							       (typep link 'plco+d2s-link))
							   most-often-links)))
	(if parts-of-double-polylink-links
	    (first parts-of-double-polylink-links)
	  (first most-often-links))))))

#| -------------------------------------------------- Better SSPU ------------------------------------------------------- |#

(defun adv~make-further-decompose-in-sspu! (unit)
  (declare (edited  "31-JUL-1996")
	   (authors Ameier)
	   (input   "A unit whose ref-graph is sspu and ground."
		    "(-> No nodes to decompose anymore, ready to go to the basic algorithm.)")
	   (effect  "Accoding to the stylistic keyword sspu-style (in res2nd~my-main)"
		    "maybe an addtional decomposition is prepared, and so the"
		    "nodes-to-decompose are updated by a new node to split."
		    "This new node is possibly newly created as"
		    " (HYPS-OF-CONC-NODE) |- A v -A (Lemma).")
	   (value   "The next selected node to split or nil if no further decomposition."))

  ;; hier ist auf jedenfall die nodes-to-decompose der unit leer
  
  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Making further decomposition in unit: ~A" unit))
  
  (let* ((conc-node (r2ntop~decompose-unit-conclusion-node unit))
	 (ref-graph (r2ntop~decompose-unit-ref-graph unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (conc-clauses (remove-duplicates (mapcar #'extdelta~clause
						  (remove-if-not #'(lambda (pair)
								     (eq (extdelta~pdsnode pair) conc-node))
								 (extdelta~relation-pairs delta-relation)))))
	 (laenge (length conc-clauses)))
    (cond ((equal r2ntop*sspu-style 'sspu)

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: Choosing no further node, because sspu-style is 'sspu"))
	   
	   nil)
	  
	  ;; untill now style is dir or aut
	  ((or (and (equal r2ntop*sspu-style 'aut) (= laenge 2))
	       (and (equal r2ntop*sspu-style 'dir) (>= laenge 2)))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: sspu-style is aut or dir and conclusion clauses are (more as one) ~A" conc-clauses))
	   
	   (if (not (r2ntop~node-trivial-p conc-node))
	       (progn
		 (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list conc-node))
		 
		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing conc-node ~A for further decomposition, because it's not trivial."
				  conc-node))
		 
		 conc-node)
	     
	     (multiple-value-bind
		 (lit-clause1 link1 lit-clause2 link2)
		 (adv=get-trail-between-clauses (first conc-clauses) (second conc-clauses) ref-graph)
	       (declare (ignore link2 lit-clause2))
	       (if lit-clause1
		   (let ((new-pdsnode (r2ntop~insert-lemma-in-proof! link1 ref-graph
								     delta-relation)))
		     (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list new-pdsnode))
		     
		     (when r2ntop*trans-debug
		       (omega~message "~%~%r2ntop*trans-debug: Adding TND node ~A, and Choosing it, to try to separate the conc-clauses ~A"
				      new-pdsnode
				      conc-clauses))
		     
		     new-pdsnode)
		 (progn
		   
		   (when r2ntop*trans-debug
		     (omega~message "~%~%r2ntop*trans-debug: Choosing no further node, because of keine Ahnun why!"))
		   
		   nil)))))
	  ((and (= (length conc-clauses) 1) (> (length (cl~literals (first conc-clauses))) 1))
	   (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list conc-node))
	   
	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: Choosing conc-node ~A for further decomposition, because it accords to the exactly one and non-unit clause ~A, and sspu-style is aut or dir"
			    conc-node
			    (first conc-clauses)))
	   
	   conc-node)
	  ((and (= (length conc-clauses) 1) (= (length (cl~literals (first conc-clauses))) 1))

	   (when r2ntop*trans-debug
	     (omega~message "~%~%r2ntop*trans-debug: there is exactly one and unit clause ~A according to the conc-node ~A."
			    (first conc-clauses)
			    conc-node))
	   
	   (let* ((conc-clause-literal (first (cl~literals (first conc-clauses))))
		  (conc-link (r2ntop~get-link-with-literal conc-clause-literal (ref~links ref-graph)))
		  (contrary-polylinks (adv=compute-contrary-polylinks conc-link conc-clause-literal ref-graph)))
	     
	     (cond ((= (length contrary-polylinks) 0)
		    
		    (when r2ntop*trans-debug
		      (omega~message "~%~%r2ntop*trans-debug: Choosing no further node, because there are no contradictional polylinks"))
		    
		    nil)
		   ((and (equal r2ntop*sspu-style 'aut) (> (length contrary-polylinks) 1))
		    
		    (when r2ntop*trans-debug
		      (omega~message "~%~%r2ntop*trans-debug: Choosing no further node, because there are more as one contradictional polylink and sspu-style is aut."))
		    
		    nil)
		   ((and (equal r2ntop*sspu-style 'aut) (> (length (append (ref~positive-shore (first contrary-polylinks))
									   (ref~negative-shore (first contrary-polylinks))))
							   3))

		    (when r2ntop*trans-debug
		      (omega~message "~%~%r2ntop*trans-debug: Choosing no further node, because the polylink ~A contains to much contra-shore and the sspu-style is aut."
				     (first contrary-polylinks)))     
		    
		    nil)
		   (t
		    (let* ((contra-polylink (first contrary-polylinks))
			   (neg-shore (ref~negative-shore contra-polylink))
			   (pos-shore (ref~positive-shore contra-polylink))
			   (poly-shore (if (> (length neg-shore) 1)
					   neg-shore
					 pos-shore)))		      
		      (cond ((eq contra-polylink conc-link)
			     (let ((clause1 (lit~clause (first poly-shore)))
				   (clause2 (lit~clause (second poly-shore))))
			       (multiple-value-bind
				   (lit-clause1 link1 lit-clause2 link2)
				   (adv=get-trail-between-clauses clause1 clause2 ref-graph)
				 (declare (ignore link2 lit-clause2))
				 (cond ((eq clause1 clause2)
					;; conclusion clause is direkt linked with two literals of the same clause
					;; decompose the node of this clause to split the clause
					(let* ((pairs-to-clause (remove-if-not #'(lambda (pair)
										   (eq (extdelta~clause pair) clause1))
									       (extdelta~relation-pairs delta-relation)))
					       (node-to-clause (extdelta~pdsnode (first pairs-to-clause))))

					  (when r2ntop*trans-debug
					    (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A to eleminte contradictional polylinks."
							   node-to-clause))
					  
					  (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list node-to-clause))
					  node-to-clause))
				       (lit-clause1
					(let ((new-pdsnode (r2ntop~insert-lemma-in-proof! link1 ref-graph
											  delta-relation)))
					  (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list new-pdsnode))

					  (when r2ntop*trans-debug
					    (omega~message "~%~%r2ntop*trans-debug: Adding TND-node ~A and choosing it to eleminta contradictional polylinks."
							   new-pdsnode))
					  
					  new-pdsnode))
				       (t
					nil)))))
			    (t
			     (let ((new-pdsnode (r2ntop~insert-lemma-in-proof! contra-polylink ref-graph
									       delta-relation)))
			       (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list new-pdsnode))

			       (when r2ntop*trans-debug
				 (omega~message "~%~%r2ntop*trans-debug: Adding TND-node ~A and choosing it to eleminte contradictional polylinks."
						new-pdsnode))
			       
			       new-pdsnode)))))))))))

(defun adv=compute-contrary-polylinks (conc-link conc-lit ref-graph)
  (let* ((pos-shore (ref~positive-shore conc-link))
	 (neg-shore (ref~negative-shore conc-link))
	 (shore-of-lit (if (member conc-lit pos-shore)
			   't
			 nil))
	 (dummy-lit (lit~literal-create 'dummy 't)))
    
    (if shore-of-lit
	(setf (ref~positive-shore conc-link) (cons dummy-lit pos-shore))
      (setf (ref~negative-shore conc-link) (cons dummy-lit neg-shore)))
    ;; -> conc-link becomes polylink
    
    (let* ((compare-pairs (plco~compute-compare-pair-list ref-graph)))
      ;; -> computes plco-pairs with conc-link as polylink -> simulates conc-link not usable as unit-literal
      
      (if shore-of-lit
	  (setf (ref~positive-shore conc-link) pos-shore)
	(setf (ref~negative-shore conc-link) neg-shore))
      ;; removes dummy lit from conc-link -> normal conc-link
      
      (multiple-value-bind
	  (flag contra-comparison-pairs)
	  (plco~sspu-refutable-p compare-pairs)
	(if flag
	    nil
	  (mapcar #'(lambda (contra-plco)
		      (let* ((first-link (plco~first-polylink contra-plco))
			     (second-link (plco~second-polylink contra-plco)))
			(if (typep first-link 'plco+d2s-link)
			    (plco~source-link first-link)
			    ;; -> this d2s-link must be the conc-link himself because we had have sspu
			  (if (eq conc-link first-link)
			      second-link
			    first-link))))
		  (remove-if-not #'(lambda (plco)
				     (let ((first-link (plco~first-polylink plco))
					   (second-link (plco~second-polylink plco)))
				       (or (typep first-link 'plco+d2s-link) ;; expanded conc-link himself
					   (typep second-link 'plco+d2s-link)
					   (eq conc-link first-link)
					   (eq conc-link second-link))))
				 contra-comparison-pairs)))))))

#| ------------------------------------------------------ TRAILS between Clauses ---------------------------------------------------- |#

(defun adv=get-trail-between-clauses (clause1 clause2 ref-graph)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "Two clauses and a refutation-graph containing these clauses.")
	   (effect  "None.")
	   (value   "Multiple values:"
		    "First: A literal of clause1."
		    "Second: A link with this literal."
		    "Third: A literal of clause2."
		    "Fourth: A link with this literal."
		    "Clause1 and clause2 are trailed about these literals and these links."))
  (if (eq clause1 clause2)
      (values nil nil nil nil)
    (let* ((literals1 (cl~literals clause1))
	   (literals2 (cl~literals clause2))
	   (links (ref~links ref-graph)))
      (multiple-value-bind
	  (lit1 lit2)
	  (do* ((rest-lit1 literals1 (rest rest-lit1))
		(back-lit1 nil)
		(back-lit2 nil))
	      (back-lit1 (values back-lit1 back-lit2))
	    (let* ((head-lit (first rest-lit1))
		   (trail-to-literals2 (adv=get-trail-literals (list head-lit)
							       links
							       (list head-lit)
							       literals2)))
	      (if trail-to-literals2
		  (progn
		    (setq back-lit1 head-lit)
		    (setq back-lit2 (first trail-to-literals2))))))
	(values lit1 (r2ntop~get-link-with-literal lit1 links) lit2 (r2ntop~get-link-with-literal lit2 links))))))	
  
(defun adv=get-trail-literals (current-literal-list usable-links still-trailed-lits literals-to-reach)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A list of literals (called current),a list of links (called usables), a list of literals"
		    "(called still trailed), and a list of literals (called seek-after-literals).")
	   (effect  "None.")
	   (value   "If there exists a trail from one of the current-literals to one of the seek-after-literals"
		    "a list of literals from seek-after-literals, for which the algorithm has found a trail."
		    "(These are possibly not all existing ones)"))
  (if (and usable-links current-literal-list)
      (multiple-value-bind
	  (used-links contra-literals)
	  (adv=get-links-and-contra-literals current-literal-list usable-links)
	(let* ((clauses-of-contra-lits (remove-duplicates (mapcar #'lit~clause contra-literals)))
	       (new-current-literals (r2ntop~remove-list (append contra-literals still-trailed-lits)
							 (apply 'append (mapcar #'cl~literals clauses-of-contra-lits))))
	       (inter (intersection literals-to-reach contra-literals)))
	  (if inter
	      inter
	    (adv=get-trail-literals
	     new-current-literals
	     (r2ntop~remove-list used-links usable-links)
	     (append contra-literals new-current-literals still-trailed-lits)
	     literals-to-reach))))
    nil))

(defun adv=get-links-and-contra-literals (literals links)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A list of literals and a list of links.")
	   (effect  "None.")
	   (value   "Multiple values:"
		    "First: A list of links which have one of the literals in its shores,"
		    "       and are member in the list of links."
		    "Second: A list of all literals that are contrary to one of the"
		    "        literals from input on a link of the link input list."))
  (do* ((rest-literals literals (rest rest-literals))
	(rest-links links)
	(back-links nil)
	(back-literals nil))
      ((null rest-literals) (values back-links back-literals))
    (let* ((head-lit (first rest-literals)))
      (multiple-value-bind
	  (used-link contra-literals)
	  (r2ntop~get-link-and-contra-literals head-lit rest-links)
	(if used-link
	    (progn
	      (setq back-links (cons used-link back-links))
	      (setq back-literals (append contra-literals back-literals))
	      (setq rest-links (remove used-link rest-links))))))))


#| ---------------------------------------- SECOND ENGINE ARBIRARY-> SSPU: SEEK SMALLEST CLAUSES ------------------------------------ |#

(defun adv~get-smallest-node-to-split! (unit)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A decompose-unit whose nodes-to-decompose slot is nil"
		    "and who contain still contrary polylink comparison pairs.")
	   (effect  "A new node is chosen to split this node and added to the"
		    "nodes-to-decompose-slot.")
	   (value   "A pdsnode (nil if unit has still nodes-to-split or"
		    "is always sspu)."))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Getting smaller sspu by splitting clause."))
  
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (plco-pairs (r2ntop~decompose-unit-plco-pairs unit))
	 (nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit)))
    (multiple-value-bind
	(flag contrary-plco)
	(plco~sspu-refutable-p plco-pairs)
      (declare (ignore contrary-plco))
      (if (or flag nodes-to-decompose)

	  (progn
	    ;; Sollte eigentlich gar nicht vorkommen !
	    (omega~message "~%In FUnktion adv~get-smallest-node-to-split!, we have reach a situation, that shouldn't arise.")
	    nil)

	(let* ((polylink (adv=prefer-often-used-and-double-poly-links plco-pairs))
	       (literals-of-double-side (cond ((typep polylink 'plco+d2s-link)
					       (let* ((source-link (plco~source-link polylink)))
						 (append (ref~positive-shore source-link)
							 (ref~negative-shore source-link))))
					      (t
					       (let* ((neg-shore (ref~negative-shore polylink))
						      (pos-shore (ref~positive-shore polylink)))
						 (if (> (length neg-shore) 1)
						     neg-shore
						   pos-shore)))))
	       (clauses-of-this-literals (remove-duplicates (mapcar #'lit~clause literals-of-double-side)))
	       (delta-pairs-of-this-clauses (remove-if-not #'(lambda (delta-pair)
							       (member (extdelta~clause delta-pair) clauses-of-this-literals))
							   (extdelta~relation-pairs delta-relation)))
	       (pdsnode (adv=seek-smallest-node-to-split delta-pairs-of-this-clauses)))
	  (setf (r2ntop~decompose-unit-nodes-to-decompose unit) (list pdsnode))

	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A" pdsnode))
	  
	  pdsnode)))))



(defun adv=seek-smallest-node-to-split (delta-pairs-of-non-unit-clauses)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A list of delta-pairs.")
	   (effect  "None.")
	   (value   "A pdsnode."
		    "The function looks for a polylink contained in as many as possible"
		    "contrary comparison-pairs and preferrably a double polylink."
		    "Now the node corresponding to the smallest clause on the double shore of this"
		    "polylink (at double-polylinks both shores) is chosen and returned."
		    "The idea is to split clauses that are as small as possible to have still"
		    "big assertion-clauses."))
  (let ((node-clauses-lists (adv=get-lists-of-node-and-according-clauses delta-pairs-of-non-unit-clauses)))
    (do* ((rest-node-clauses-lists node-clauses-lists (rest rest-node-clauses-lists))
	  (smallest-node nil)
	  (length-of-biggest-clause-in-smallest-node nil))
	((null rest-node-clauses-lists) smallest-node)
      (let* ((first-node (first (first rest-node-clauses-lists)))
	     (clauses (second (first rest-node-clauses-lists)))
	     (length-of-biggest-clause (adv=length-of-biggest-clause clauses)))
	(if (or (null smallest-node)
		(< length-of-biggest-clause length-of-biggest-clause-in-smallest-node))
	    (progn
	      (setq smallest-node first-node)
	      (setq length-of-biggest-clause-in-smallest-node length-of-biggest-clause)))))))

(defun adv=length-of-biggest-clause (clause-list)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "The lenght of the biggest clause."))
  (do* ((rest-clauses clause-list (rest rest-clauses))
	(biggest-length nil))
      ((null rest-clauses) biggest-length)
    (let ((clause-length (length (cl~literals (first rest-clauses)))))
      (if (or (null biggest-length) (> clause-length biggest-length))
	  (setq biggest-length clause-length)))))

(defun adv=get-lists-of-node-and-according-clauses (delta-pairs-of-non-unit-clauses)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A list of delta-pairs.")
	   (effect  "None.")
	   (value   "A list of lists of pdsnode and a list of the clauses corresponding to this node"
		    "pdsnode and clauses have to be contained in the delta-pairs."))
  (if delta-pairs-of-non-unit-clauses
      (let* ((head-pair (first delta-pairs-of-non-unit-clauses))
	     (pdsnode (extdelta~pdsnode head-pair))
	     (pairs-with-this-node (remove-if-not #'(lambda (pair)
						      (eq (extdelta~pdsnode pair) pdsnode))
						  delta-pairs-of-non-unit-clauses)))
	(cons (list pdsnode (remove-duplicates (mapcar #'extdelta~clause pairs-with-this-node)))
	      (adv=get-lists-of-node-and-according-clauses (r2ntop~remove-list pairs-with-this-node
										  delta-pairs-of-non-unit-clauses))))
    nil))

#| --------------------------------------------------------------------------------------------------------------------------------- |#



