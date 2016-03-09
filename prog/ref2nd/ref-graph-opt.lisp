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



(mod~defmod REFOPT 
            :uses (cl data delta extdelta keim lit pos r2ntop ref res2ref subst term)
            :documentation "Optmization of refutation graphs."
            :exports (
                      
                      refopt~delete-flips!
                      refopt~optimize-ref-graph!
                      ))




#| ------------------------------------------------------ AUXILIARIES ------------------------------------------------ |#

(defun refopt=equal-history-and-ground-subst (clause1 clause2)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "T if the history clause of both clauses is same and the ground-substiutions"
		    "are keim~equal on all variables that are contained in both."
		    "Nil otherwise."))
  (if (or (typep clause1 'r2ntop+paramod-clause) (typep clause1 'r2ntop+flip-clause)
	  (typep clause2 'r2ntop+paramod-clause) (typep clause2 'r2ntop+flip-clause)
	  (typep clause1 'r2ntop+reflex-clause) (typep clause2 'r2ntop+reflex-clause))
      nil
    (let ((ground-subst1 (r2ntop~trans-clause-ground-subst clause1))
	  (ground-subst2 (r2ntop~trans-clause-ground-subst clause2)))
      (and (eq (r2ntop~trans-clause-history clause1) (r2ntop~trans-clause-history clause2))
	   (keim~equal (subst~remove-components (subst~domain ground-subst1)
						(subst~compose-substitution ground-subst1 (r2ntop~trans-clause-renaming clause1)))
		       (subst~remove-components (subst~domain ground-subst2)
						(subst~compose-substitution ground-subst2 (r2ntop~trans-clause-renaming clause2))))))))

(defun refopt=get-possible-union-clause-lists (clauses)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A set of clauses.")
	   (effect  "None.")
	   (value   "A list of list of clauses. Each list of clauses contains only clauses"
		    "with same history and sam ground-substitution and is bigger than one."))
  (let ((ordered-clauses (r2ntop~order-modulo-test clauses :test 'refopt=equal-history-and-ground-subst)))
    (remove-if #'(lambda (clause-list)
		   (= (length clause-list) 1))
	       ordered-clauses)))

(defun refopt=remove-literal-from-links! (literal ref-graph)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A literal and a refutation graph.")
	   (effect  "The literall is removed from the link-shore in wich it was standing." )
	   (value   "Undefined."))
  (let* ((links (ref~links ref-graph))
	 (link-with-literal (first (remove-if-not #'(lambda (link)
						      (member literal (append (ref~positive-shore link)
									      (ref~negative-shore link))))
						  links))))
    (if link-with-literal
	(progn
	  (setf (ref~positive-shore link-with-literal) (remove literal (ref~positive-shore link-with-literal))) 
	  (setf (ref~negative-shore link-with-literal) (remove literal (ref~negative-shore link-with-literal)))))))

(defun refopt=clauses-have-same-links-till-one (clause1 clause2 links)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses and a set of links.")
	   (effect  "None.")
	   (value   "Multiple value:"
		    "First: t if all literals same at the both clauses, have the same links"
		    "       till at least one. Nil otherwise."
		    "Second: A list of all literals-pairs (one from first clasue ,one from second)"
		    "        that are in different links."))
  (do* ((rest-literals1 (cl~literals clause1) (rest rest-literals1))
	(rest-literals2 (cl~literals clause2) (rest rest-literals2))
	(unequal-literals nil)
	(unequal 0))
      ((null rest-literals1) (if (>= unequal 2)
				 (values nil unequal-literals)
			       (values t unequal-literals)))
    (let* ((first-literal1 (first rest-literals1))
	   (first-literal2 (first rest-literals2))
	   (link-of-literal1 (first (remove-if-not #'(lambda (link)
						       (member first-literal1 (append (ref~positive-shore link)
										      (ref~negative-shore link))))
						   links)))
	   (literals-of-link-of-literal1 (append (ref~positive-shore link-of-literal1)
						 (ref~negative-shore link-of-literal1))))
      (if (not (member first-literal2 literals-of-link-of-literal1))
	  (progn
	    (setq unequal (+ unequal 1))
	    (setq unequal-literals (cons (list first-literal1 first-literal2) unequal-literals)))))))

#| ------------------------------------------------------ OPT-I ------------------------------------------------------ |#

(defun refopt~optimize-ref-graph! (ref-graph delta-relation)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A refutation graph and a delta-relation.")
	   (effect  "All clause-nodes who have same history and sam ground-substitution"
		    "and have all links till one in common are uned, that means, the"
		    "links on them are uned and one of the clauses is deleted.")
	   (value   "The ref-graph."))
  (let* ((clauses (ref~clause-nodes ref-graph))
	 (poss-union-clause-lists (refopt=get-possible-union-clause-lists clauses)))
    (refopt=seek-union-clauses poss-union-clause-lists ref-graph delta-relation)
    ref-graph))
	 
(defun refopt=seek-union-clauses (poss-union-clause-lists ref-graph delta-relation)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A list of clause-lists a refutation graph and a delta-relation.")
	   (effect  "Each sublist of clauses consists of clauses that have same history"
		    "and same ground-substitution. Now is seeked a pair oc clauses"
		    "from same sublist, that both clauses have same links till one."
		    "If such a clause pair is found it is uned, this causes"
		    "changes in links and clauses of the refutation graph."
		    "The search after unable pairs is continued recursively till"
		    "there is now further pair to une found, or there are no clauses"
		    "to check.")
	   (value   "Undefined."))
  (if poss-union-clause-lists
      (multiple-value-bind
	  (union-pair new-poss-union-clause-lists)
	  (refopt=get-next-union-pair poss-union-clause-lists ref-graph)
	(if union-pair
	    (progn
	      (refopt=union-of-clause-pair! (first union-pair) (second union-pair) ref-graph delta-relation)
	      (refopt=seek-union-clauses new-poss-union-clause-lists ref-graph delta-relation))))))

(defun refopt=union-of-clause-pair! (master-clause clause ref-graph delta-relation)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses , the refutation graph and the delta-relation.")
	   (effect  "The two clauses have same history and same links till one."
		    "The first is called master, the second slave. This both clauses"
		    "are uned in tha way, that all links at them are uned and then"
		    "slave is dleted in clause-nodes of refutation graph and in the"
		    "delta-relation. The old-links are replaced by new created"
		    "union-links.")
	   (value   "Undefined."))
  (let ((links (ref~links ref-graph))
	(literals2 (cl~literals clause)))
    (multiple-value-bind
	(flag unequal-literals)
	(refopt=clauses-have-same-links-till-one master-clause clause links)
      (declare (ignore flag))
      (let* ((literal1 (first (first unequal-literals)))
	     (literal2 (second (first unequal-literals)))
	     (remove-literals-from-links (remove literal2 literals2))
	     (link-with-literal1 (first (remove-if-not #'(lambda (link)
							   (member literal1 (append (ref~positive-shore link)
										    (ref~negative-shore link))))
						       links)))
	     (link-with-literal2 (first (remove-if-not #'(lambda (link)
							   (member literal2 (append (ref~positive-shore link)
										    (ref~negative-shore link))))
						       links))))
	;; removed alle literale from clause from other links
	(mapcar #'(lambda (literal)
		    (refopt=remove-literal-from-links! literal ref-graph))
		remove-literals-from-links)
	;; uned the two links of master-clause and clause
	(let* ((pos-shore-master (ref~positive-shore link-with-literal1))
	       (neg-shore-master (ref~negative-shore link-with-literal1))
	       (pos-shore-clause (ref~positive-shore link-with-literal2))
	       (neg-shore-clause (ref~negative-shore link-with-literal2))
	       (new-link (ref~link-create (intern (string-upcase (format nil "paramod-link-~A" (incf r2ntop*link-counter)))
						  (find-package :omega))
					  (mapcar #'r2ntop~get-literal-parent-and-position
						  (remove literal2 (append pos-shore-master
									   pos-shore-clause
									   neg-shore-master
									   neg-shore-clause)))
					  (subst~create () ()))))
	  ;; remove-the-second-link ,second clause ,the substitutions of the second-clause
	  ;; the delta-pairs of the second clause and add the new-link
	  (setf (ref~clause-nodes ref-graph) (remove clause (ref~clause-nodes ref-graph)))
	  (setf (delta~relation-pairs delta-relation)
		(r2ntop~remove-list (remove-if-not #'(lambda (pair)
						       (eq (extdelta~clause pair) clause))
						   (extdelta~relation-pairs delta-relation))
				    (extdelta~relation-pairs delta-relation)))
	  (setf (ref~ground-substitution ref-graph) (subst~remove-components (subst~domain (r2ntop~trans-clause-ground-subst clause))
									     (ref~ground-substitution ref-graph)))
	  (setf (ref~links ref-graph) (cons new-link (r2ntop~remove-list (list link-with-literal2 link-with-literal1)
									 (ref~links ref-graph)))))))))


(defun refopt=get-pair-to-une (clause-list links)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A set of clauses (all with same history clause) and a set of links.")
	   (effect  "None.")
	   (value   "If existing a list of two clauses from the list that have same links"
		    "till one, if no such clauses exists nil."))
  (if clause-list
      (let* ((head (first clause-list))
	     (rest-list (rest clause-list))
	     (uniables (remove-if-not #'(lambda (clause)
					  (refopt=clauses-have-same-links-till-one head clause links))
				      rest-list)))
	(if uniables
	    (list head (first uniables))
	  (refopt=get-pair-to-une rest-list links)))
    nil))

(defun refopt=get-next-union-pair (poss-union-clause-lists ref-graph)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A list of clause-lists and a refutaton graph.")
	   (effect  "None.")
	   (value   "All clauses of a sublist have same history. Now there are"
		    "all pairs of clauses computed that could be uned because"
		    "all links on them are equal till one."
		    "Returned is a multiple-value:"
		    "First: A pair of two clauses that could be uned."
		    "Second: The input list of clauses without this pair."))
  (let ((links (ref~links ref-graph)))
    (do* ((rest-check-tuples poss-union-clause-lists (rest rest-check-tuples))
	  (back-poss-union-clause-lists nil)
	  (back-pair nil))
	((or back-pair (null rest-check-tuples)) (values back-pair back-poss-union-clause-lists))
      (let* ((clause-list (first rest-check-tuples))
	     (pair-to-une (refopt=get-pair-to-une clause-list links)))
	(if pair-to-une
	    (progn
	      (setq back-pair pair-to-une)
	      (setq back-poss-union-clause-lists (append back-poss-union-clause-lists
							 (cons (remove (second pair-to-une) clause-list)
							       (rest rest-check-tuples)))))
	  (setq back-poss-union-clause-lists (cons clause-list back-poss-union-clause-lists)))))))


#| ------------------------------------------------ OPT-FLIP-CLAUSES ------------------------------------------------- |#

(defun refopt~delete-flips! (ref-graph)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A refutation graph.")
	   (effect  "The flip-clauses are checked wether they are necessary."
		    "A flip clause isn't necessary if it is only linked with"
		    "the equation-literal of paramod-clauses. In this case"
		    "the flip-clause is removed from the clause-list of the"
		    "ref-graph and the both links on it are uned.")
	   (value   "The refutation-graph."))
  (let* ((clauses (ref~clause-nodes ref-graph))
	 (flip-clauses (remove-if-not #'(lambda (clause)
					  (typep clause 'r2ntop+flip-clause))
				      clauses)))
    (mapcar #'(lambda (flip-clause)
		(if (refopt=delete-flip-clause-p flip-clause ref-graph)
		    (refopt=remove-flip-clause! flip-clause ref-graph)))
	    flip-clauses))
  ref-graph)
    
(defun refopt=remove-flip-clause! (flip-clause ref-graph)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A flip-clause and the ref-graph.")
	   (effect  "The flip-clauses is deleted from the ref-graph and the two links on it are"
		    "uned.")
	   (value   "The new created union link."))
  (let* ((current-links (ref~links ref-graph))
	 (literal1 (first (cl~literals flip-clause)))
	 (literal2 (second (cl~literals flip-clause)))
	 (link1 (refopt=get-link-to-literal literal1 current-links))
	 (link2 (refopt=get-link-to-literal literal2 current-links))
	 (new-link (ref~link-create (intern (string-upcase (format nil "link-~A" (incf r2ntop*link-counter)))
					    (find-package :omega))
				    (mapcar #'r2ntop~get-literal-parent-and-position
					    (remove literal1
						    (remove literal2
							    (append (ref~positive-shore link1)
								    (ref~positive-shore link2)
								    (ref~negative-shore link1)
								    (ref~negative-shore link2)))))
				    (subst~create () ()))))
    ;; flip possible wrong flipped paramod-equation literals
    (refopt=correct-flip-the-paramod-equation-literals! new-link)
    ;; update clause nodes of ref-graph
    (setf (ref~clause-nodes ref-graph) (remove flip-clause (ref~clause-nodes ref-graph)))
    ;; update links of ref-graph
    (setf (ref~links ref-graph) (cons new-link (remove link1 (remove link2 (ref~links ref-graph)))))
    new-link))

(defun refopt=correct-flip-the-paramod-equation-literals! (link)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A link.")
	   (effect  "The link is created by destroing a flip-clause, possibly the paramod"
		    "equation literals are not flipped correctly. Now they are flipped in the"
		    "correct direction.")
	   (value   "Undefined."))
  (let* ((shores (append (ref~positive-shore link) (ref~negative-shore link)))
	 (equation-paramod-literals (remove-if-not #'refopt=equation-paramod-literal-p shores))
	 (other-literals (remove-if #'refopt=equation-paramod-literal-p shores))
	 (head-atom (if other-literals
			(lit~atom (first other-literals))
		      (lit~atom (first equation-paramod-literals)))))
    (mapcar #'(lambda (equation-literal)
		(let ((atom (lit~atom equation-literal)))
		  (if (not (data~equal head-atom atom))
		      (setf (lit~atom equation-literal)
			    (term~appl-create (data~appl-function atom)
					      (list (cadr (data~appl-arguments atom))          
						    (car (data~appl-arguments atom))))))))      
	    equation-paramod-literals)))

;; The situation is:  [...,a=b]--L1--[-a=b,b=a]--L2--[-b=a, ...]
;; Remark: A flip-clause could be delted from the ref-graph if all literals without the literals from
;; the flip clause itself and possible paramod-equation-clauses have SAME atoms.
;; In this case all paramod-equation-clauses maybe flipped are reflipped and the links L! and L2 are uned.

(defun refopt=equation-paramod-literal-p (literal)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "T if the literal id the equation literal of a paramod-clause,"
		    "that means it is the second (position 1) literal of a paramod"
		    "clause, nil otherwise."))
  (and (typep (lit~clause literal) 'r2ntop+paramod-clause)
       (eq literal (r2ntop~term-at-position (cl~literals (lit~clause literal)) (pos~list-position (list '1))))))

(defun refopt=delete-flip-clause-p (flip-clause ref-graph)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A flip-clause and the refutation graph.")
	   (effect  "None.")
	   (value   "T if the flip clause could get deleted from the graph."
		    "Otherwise nil."))
  (let* ((links (ref~links ref-graph))
	 (literal1 (first (cl~literals flip-clause)))
	 (literal2 (second (cl~literals flip-clause)))
	 (link1 (refopt=get-link-to-literal literal1 links))
	 (link2 (refopt=get-link-to-literal literal2 links))
	 (other-not-equation-paramod-literals
	  (remove-if #'refopt=equation-paramod-literal-p
		     (remove literal1 (remove literal2 (append (ref~positive-shore link1)
							       (ref~positive-shore link2)
							       (ref~negative-shore link1)
							       (ref~negative-shore link2))))))
	 (head-literal (first other-not-equation-paramod-literals)))
    (if (or (not head-literal) (= (length other-not-equation-paramod-literals) 1))
	't
      (let* ((head-literal-atom (lit~atom head-literal)))
	(do* ((rest-literals (rest other-not-equation-paramod-literals) (rest rest-literals))
	      (flag 't))
	    ((or (null rest-literals) (null flag)) flag)
	  (let* ((atom (lit~atom (first rest-literals))))
	    (if (not (data~equal atom head-literal-atom))
		(setq flag nil))))))))

(defun refopt=get-link-to-literal (literal links)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A literal and a list of links.")
	   (effect  "None.")
	   (value   "The link in which literal is in if existing, otherwise"
		    "nil."))
  (first (remove-if-not #'(lambda (link)
			    (member literal (append (ref~positive-shore link)
						    (ref~negative-shore link))))
			links)))
					      

 


		 
		
		
