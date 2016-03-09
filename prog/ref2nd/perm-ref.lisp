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



(mod~defmod SSPU 
            :uses (cl data extdelta keim lit node r2ntop ref subst)
            :documentation "Computation of the eks-partition."
            :exports (
                      
                      sspu~get-adjacent-literals
                      sspu~ground-sspu-steps
                      ))







#| ------------------------------------------------- AUXILIARIES -------------------------------------------------- |#

(defgeneric sspu=unit-clause-p (clause)
  (declare (edited  "20-JAN-1995")
	   (authors Ameier)
	   (input "A CLAUSE." )
	   (effect "None." )
	   (value "T, iff CLAUSE is a unit clause." ))
  (:method ((clause cl+clause))
	   (let ((no-of-literals (length (cl~literals clause))))
	     (eql no-of-literals 1))))

(defun sspu=get-unit-resolvent (non-unit-clause resol-contras)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A non-unit-clause and the list of the resolution-partner"
		    "unit clauses for the literals of non-unit-clause"
		    "nil for the literal who hasn't a resolution-partner.")
	   (effect  "None.")
	   (value   "The literal who hasn't a resolution-partner is returned."))
  (do* ((rest-literals (cl~literals non-unit-clause) (rest rest-literals))
	(rest-contras resol-contras (rest rest-contras)))
      ((null (first rest-contras)) (first rest-literals))))

(defun sspu=member-clause-of-resol-contras-in-clause-list-p (resol-contra clause-list)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A set of clauses and nil, a set of clauses.")
	   (effect  "None.")
	   (value   "T if a clause from the first set is in the second set,"
		    "otherwise nil."))
  (do* ((resol-elements resol-contra (rest resol-elements))
	(flag nil))
      ((or (null resol-elements) (not (null flag))) flag)
    (let ((head (first resol-elements)))
      (if (and head (member head clause-list))
	  (setq flag 't)))))

(defun sspu=get-linked-literals (literal links)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A literal and a set of links.")
	   (effect  "None.")
	   (value   "Seeks the first link with literal in shore and"
		    "returns the literals of the contary shore."))
  (let* ((link-with-literal (first (member literal links
					   :test #'(lambda (lit link)
						     (member lit (append (ref~positive-shore link)
									 (ref~negative-shore link)))))))
	 (pos-shore (ref~positive-shore link-with-literal))
	 (neg-shore (ref~negative-shore link-with-literal)))
    (if (member literal pos-shore)
	neg-shore
      pos-shore)))

(defun sspu~get-adjacent-literals (literal links)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A literal and a set of links.")
	   (effect  "None.")
	   (value   "Seeks the first link with literal in shore and"
		    "returns the two shores without literal."))
  (let* ((link-with-literal (first (member literal links
					   :test #'(lambda (lit link)
						     (member lit (append (ref~positive-shore link)
									 (ref~negative-shore link)))))))
	 (pos-shore (ref~positive-shore link-with-literal))
	 (neg-shore (ref~negative-shore link-with-literal)))
    (remove literal (append pos-shore neg-shore))))

(defun sspu=check-clause-nodes (refutation-graph)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph.")
	   (effect  "Seek all clauses who stand in the links, and set the"
		    "clause-nodes-set to this list.")
	   (value   "Undefined."))
  (let* ((links (ref~links refutation-graph))
	 (clauses-at-links (remove-duplicates
			    (apply 'append (mapcar #'(lambda (link)
						       (let ((shores (append (ref~negative-shore link)
									     (ref~positive-shore link))))
							 (mapcar #'lit~clause shores)))
						   links)))))
    (setf (ref~clause-nodes refutation-graph) clauses-at-links)))

(defun sspu=non-unit-clause-sspu-p (clause-node refutation-graph)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A clause and a refutation graph.")
	   (effect  "None.")
	   (value   "The resol-contras if for every literal of the clause"
		    "exists a unit-clause resolution-partner, till one."
		    "otherwise nil."))
  (let* ((links (ref~links refutation-graph))
	 (resol-contras (mapcar #'(lambda (literal)
				    (let* ((linked-literals (sspu=get-linked-literals literal links))
					   (linked-clauses (mapcar #'lit~clause linked-literals)))
				      (if (= (length linked-literals) 1)
					  (if (sspu=unit-clause-p (first linked-clauses))
					      (first linked-clauses)
					    nil)
					nil)))
				(cl~literals clause-node))))
    (if (> (length (remove-if-not #'null resol-contras)) 1)
	nil
      resol-contras)))

(defun sspu=check-resol-contras (resol-contras conclusion-clauses)
  (if (member nil resol-contras)
      resol-contras                                              ;; -> real sspu resolution: one unit-resolvent left
    (let ((intersect (remove-if-not #'(lambda (resol-element)    ;; -> last step, the 'unit-resolvent' is empty-clause 
					(member resol-element conclusion-clauses))
				    resol-contras)))
      (if intersect
	  (let ((replace-element (first intersect)))
	    (if (= (length intersect) 1)
		(mapcar #'(lambda (resol-element)
			    (if (eq resol-element replace-element)
				nil
			      resol-element))
			resol-contras)
	      (cons nil (rest resol-contras))))
	(cons nil (rest resol-contras))))))

#| ------------------------------------------------ MAIN --------------------------------------------------------- |#


(defun sspu=get-next-non-unit-clause (non-unit-clauses ref-graph conclusion-clauses)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A set of non-unit-clauses a refutation-graph and the set of"
		    "conclusion-nodes.")
	   (effect  "None.")
	   (value   "Returns the next non-unit-clause for the next sspu-step,"
		    "that means, the clause could have atmost 1 literal,who"
		    "isn't linked with a unit-clause. The nodes, which have"
		    "partners from conclusion-node are tried to aufschieben"
		    "to get a direct proof."
		    "Returned is multiple-value:"
		    "First the choosen non-unit-clause, second the linked unit-clause"
		    "partners, nil for the literal of non-unit-clause who didn't"
		    "has a partner."))
  (let* ((non-unit-clauses-with-resol-contras (mapcar #'(lambda (non-unit-clause)
							  (list non-unit-clause
								(sspu=non-unit-clause-sspu-p non-unit-clause ref-graph)))
						      non-unit-clauses))
	 (non-unit-clause-canditats (remove-if-not #'(lambda (non-unit-clause-pair)
						       (let (;; (non-unit-clause (first non-unit-clause-pair))
							     (resol-contra (second non-unit-clause-pair)))
							 resol-contra))
						   non-unit-clauses-with-resol-contras))
	 (non-unit-clause-pairs-without-conclusion-clauses
	  (remove-if-not #'(lambda (non-unit-clause-pair)
			     (let (;; (non-unit-clause (first non-unit-clause-pair))
				   (resol-contra (second non-unit-clause-pair)))
			       (not (sspu=member-clause-of-resol-contras-in-clause-list-p
				     resol-contra conclusion-clauses))))
			 non-unit-clause-canditats)))
    (if non-unit-clause-pairs-without-conclusion-clauses
	(let ((non-unit-clause (first (first non-unit-clause-pairs-without-conclusion-clauses)))
	      (resol-contras (second (first non-unit-clause-pairs-without-conclusion-clauses))))
	  (values non-unit-clause (sspu=check-resol-contras resol-contras conclusion-clauses)))
      (let ((non-unit-clause (first (first non-unit-clause-canditats)))
	    (resol-contras (second (first non-unit-clause-canditats))))
	(values non-unit-clause (sspu=check-resol-contras resol-contras conclusion-clauses))))))

(defun sspu=update-refutation-graph (ref-graph non-unit-clause resol-contras)
  (declare (edited  "02-APR-1996")
	   (authors Ameier)
	   (input   "A refutation-graph a non-unit-clause and the resolution"
		    "partner clauses (unit-clauses) for the literals of the"
		    "the non-unit-clause.")
	   (effect  "Resolution is applied at the non-unit-clause, by replacing"
		    "the non-unit-clause through the unit-resolvent ofthis step."
		    "Between this the links has to be changed and the clause nodes"
		    "of the ref-graph.")
	   (value   "Undefined."))
  (let* ((ground-subst (r2ntop~trans-clause-ground-subst non-unit-clause))
	 (links (ref~links ref-graph))
	 (unit-resolvent (sspu=get-unit-resolvent non-unit-clause resol-contras))
	 (new-literal (lit~literal-create (subst~apply ground-subst (lit~atom unit-resolvent)
						       :downto '(data+primitive)
						       )
					  (lit~positive-p unit-resolvent)))
	 (new-clause (r2ntop~create-trans-clause (list new-literal))))
    (mapcar #'(lambda (literal resol-contra)
		(let* ((acc-link (first (member literal links
						    :test #'(lambda (lit link)
							      (member lit (append (ref~positive-shore link)
										  (ref~negative-shore link)))))))
		       (pos-shore (ref~positive-shore acc-link))
		       (neg-shore (ref~negative-shore acc-link))
		       (new-pos-shore (if (lit~positive-p literal)
					  (if resol-contra
					      (remove literal pos-shore)
					    (cons new-literal (remove literal pos-shore)))
					pos-shore))
		       (new-neg-shore (if (not (lit~positive-p literal))
					  (if resol-contra
					      (remove literal neg-shore)
					    (cons new-literal (remove literal neg-shore)))
					neg-shore)))
		  (if (or (null new-pos-shore) (null new-neg-shore))
		      (ref~remove-link-from-graph ref-graph acc-link)
		    (progn
		      (setf (ref~positive-shore acc-link) new-pos-shore)
		      (setf (ref~negative-shore acc-link) new-neg-shore)))))
	    (cl~literals non-unit-clause) resol-contras)
    (setf (ref~clause-nodes ref-graph)
	  (cons new-clause (remove non-unit-clause (ref~clause-nodes ref-graph))))
    (sspu=check-clause-nodes ref-graph)
    ;; (ref~print-object ref-graph 't)
    new-clause))

(defun sspu~ground-sspu-steps (unit) 
  (declare (edited  "27-FEB-1996")
	   (authors Ameier)
	   (input   "A decompose unit.") 
	   (effect  "If ref-graph is a sspu-resolution, its leaves are instantiated, the"
		    "instantiation is propagated through the whole proof.")
	   (value   "A list of triples <U, List-Leaves, Assertion> with"
		    " U: a unit-resolvent, the last may be an empty clause."
		    " List-Leaves: a list of unit clause-occurences."
		    " Assertion: an non-unit clause-occurrence serving as an assertion"
		    "            of the sspu-resolution step."))
  (let* ((ref-graph (r2ntop~decompose-unit-ref-graph unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	 (clauses-of-conc-node (remove-duplicates
				(mapcar #'extdelta~clause
					(remove-if-not #'(lambda (pair)
							   (eq (extdelta~pdsnode pair) conclusion-node))
						       (extdelta~relation-pairs delta-relation)))))
	 (clause-nodes (ref~clause-nodes ref-graph))
	 (non-unit-clauses (remove-if-not #'(lambda (clause)
					      (> (length (cl~literals clause)) 1))
					  clause-nodes))
	 ;; (unit-clauses (remove-if-not #'sspu=unit-clause-p clause-nodes))
	 )
    (do* ((current-non-unit-clauses non-unit-clauses)
	  (current-ref-graph ref-graph)
	  (sspu-steps nil))
	((null current-non-unit-clauses)
	 (cons (list (r2ntop~create-trans-clause ()) (ref~clause-nodes current-ref-graph) nil)
	       sspu-steps))
      (multiple-value-bind
	  (non-unit-clause resol-contras)
	  (sspu=get-next-non-unit-clause current-non-unit-clauses current-ref-graph clauses-of-conc-node)
	;; 1. update the current-non-uit-clauses
	(setq current-non-unit-clauses (remove non-unit-clause current-non-unit-clauses))
	;; 2. update the current refutation-graph by apply resolution on the non-unit-clause
	(let* ((checked-resol-contras (remove-if #'null resol-contras))
	       (new-unit-clause (sspu=update-refutation-graph current-ref-graph non-unit-clause resol-contras)))
	  ;; 3. build new sspu-step and add it to the sspu-steps
	  (setq sspu-steps (cons (list new-unit-clause checked-resol-contras non-unit-clause)
				 sspu-steps)))))))


