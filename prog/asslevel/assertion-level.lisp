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



(mod~defmod AL 
            :uses (alb altr alx data just keim logic misc mod node pds pdsn prob sys)
            :documentation "The assertion level algorithms."
            :exports (al+consistent-match-error
                      al+front-error
                      
                      al~applied-tree
                      al~find-best-premises
                      al~get-best-premises
                      al~match-conclusion
                      al~match-premises
                      al~preceding-lines
		      ))


;################################################
;##                                            ##
;##                  Errors                    ##
;##                                            ##
;################################################

(sys~define-condition
 al+consistent-match-error (sys+error)
 ((eins) (zwei))
 (lambda (condition stream)
   (format stream "Matching conflict between ~A and ~A."
	   (al+consistent-match-error-eins condition)
	   (al+consistent-match-error-zwei condition))))

(sys~define-condition
 al+front-error (sys+error)
 ()
 (lambda (condition stream)
   (declare (ignore condition))
   (format stream "Can't find matching front.")))



;################################################
;##                                            ##
;##                Matching                    ##
;##                                            ##
;################################################

;;; Conclusion

(defun al=match-conclusion (formula rule-node)
  (declare (edited  "25-FEB-1993 16:33")
	   (authors AFIEDLER)
	   (input   "A formula and a rule node.")
	   (effect  "Tries to match FORMULA with a formula in the main conclusion path"
		    "(`positive') or the negation of a formula in the composition path"
		    "(i.e. a side branch; `negative') of the subtree with RULE-NODE as"
		    "root.")
	   (value   "Returns a list of triples, each consisting consisting of:"
		    "the matching rule node, a boolean being T in the `positive' case and"
		    "NIL in the `negative' case, the matching (i.e. the binding),"
		    "iff matching is possible."))
  (let* ((pos (altr~main-p rule-node))
	 (matches (if pos
		      (alx~match-p
		       (node~formula rule-node)
		       (alx~formula-without-double-negation formula))
		    (alx~match-p
		     (let ((wff (node~formula rule-node)))
		       (if (logic~negation-p wff)
			   (alx~negation-scope wff)
			 (alx~negate-formula wff)))
		     (alx~formula-without-double-negation formula))))
	 (pre-list (altr~preconditions rule-node))
	 (res (if matches (list (list rule-node pos matches)))))
    (dolist (pre pre-list res)
      (setq res (append res (al=match-conclusion formula pre))))))

(defun al~match-conclusion (formula rule-node)
  (declare (edited  "25-FEB-1993 16:33")
	   (authors AFIEDLER)
	   (input   "A formula and a rule node.")
	   (effect  "Tries to match FORMULA with a formula in the main conclusion path"
		    "(`positive') or the negation of a formula in the composition path"
		    "(i.e. a side branch; `negative') of the subtree with RULE-NODE as"
		    "root.")
	   (value   "Returns a list of triples, each consisting consisting of:"
		    "the matching rule node, a boolean being T in the `positive' case and"
		    "NIL in the `negative' case, the matching (i.e. the binding),"
		    "iff matching is possible."))
  (when alx*tracer
    (omega~output "~1& Matching conclusion "))
  (let ((results (al=match-conclusion formula rule-node)))
    (when alx*tracer
      (if results
	  (omega~output " succeeded~%")
	(omega~output " failed~%")))
    results))


;;; Premises

(defun al=find-front (tree from-here)
  (declare (edited  "24-MAY-1996")
	   (authors Afiedler)
	   (input   "A premise tree and a list of lists, whose first elements are"
		    "premise tree nodes.")
	   (value   "A list of the elements of FROM-HERE that build a front in TREE."))
  (let ((preconds (altr~preconditions tree)))
    (if preconds
	(apply #'append
	       (let ((front (mapcar #'(lambda (pre)
					(let ((res (assoc pre from-here)))
					  (if res
					      (list res)
					    (sys~handler-case (al=find-front pre
									       from-here)
							      (al+front-error () nil)))))
				    preconds)))
		 (cond ((if (altr~or-node-p tree)
			    (some #'identity front)
			  (every #'identity front))
			front)
		       (t (error (sys~make-condition 'al+front-error))))))
      (error (sys~make-condition 'al+front-error)))))

(defun al=consistent-bindings (binding &rest bindings)
  (declare (edited  "16-APR-1998" "21-MAY-1996")
	   (authors Afiedler)
	   (input   "At least two bindings.")
	   (value   "The union of the bindings, iff they are consistent, otherwise NIL."))
  (sys~handler-case
   (progn (mapc #'(lambda (bind)
		    (let ((domain (subst~domain binding))
			  (codomain (subst~codomain binding))
			  (this-domain (subst~domain bind))
			  (this-codomain (subst~codomain bind)))
		      (mapc #'(lambda (var)
				(let ((co (data~assoc var domain codomain #'data~equal))
				      (this-co (data~assoc var this-domain this-codomain
							   #'eq)))
				  (if co
				      (unless (data~equal co this-co)
					(error (sys~make-condition
						'al+consistent-match-error
						:eins (subst~create (list var)
								    (list this-co))
						:zwei (subst~create (list var)
								    (list co)))))
				    (subst~add-component var this-co binding
							 :destructive t))))
			    this-domain)))
		bindings)
	  ; old: bindings waren noch a-lists
	  #+old(mapc #'(lambda (bind)
			 (let ((mem (member bind result
					    :test
					    #'(lambda (x y)
						(keim~equal (car x)
							    (car y))))))
			   (if (not mem)
			       (push bind result)
			     (if (and (car mem)
				      (not (data~equal-p (cdr bind)
							 (cdar mem))))
				 #+old(not (keim~equal (cdr bind) (cdar mem)))
				 (error (sys~make-condition
					 'al+consistent-match-error
					 :eins bind :zwei (car mem)))))))
		     (apply #'append bindings))
	  binding)
   (al+consistent-match-error () nil)))


(defun al=find-consistent-bindings (consistent matchings)
  (declare (edited  "21-MAY-1996")
	   (authors Afiedler)
	   (input   "A binding and a list of matchings, where each matching is a list"
		    "whose first element is a premise tree node and whose rest are lists"
		    "of possibly matching nd-lines and their respective bindings.")
	   (value   "A list of dotted pairs of premise tree nodes and their matching"
		    "nd-lines, iff a matching of the premise tree node with the according"
		    "nd-lines in MATCHINGS consistent to CONSISTENT is possible,"
		    "otherwise NIL."))
  (flet ((next (con pro re)
	       (let ((pro-rest (cddr pro)))
		 (if pro-rest
		     (al=find-consistent-bindings con
						    (cons (cons (car pro) pro-rest)
							  re))))))
    (let ((probe (car matchings))
	  (rest (cdr matchings))
	  (new-consistent (keim~copy consistent :downto '(term+term))))
      (if (< 1 (length probe))
	  (let ((new (al=consistent-bindings new-consistent (cdadr probe))))
	    (if new
		(if rest
		    (let ((result (al=find-consistent-bindings new rest)))
		      (if result
			  (cons (cons (car probe) (cadr probe)) result)
			(next consistent probe rest)))
		  (list (cons (car probe) (cadr probe))))
	      (next consistent probe rest)))))))


(defun al=find-all-consistent-bindings (node line-binding matchings)
  (declare (edited  "24-MAY-1996")
	   (authors Afiedler)
	   (input   "A premise tree node, a list of a line and its binding to match NODE,"
		    "and a list of matchings as in {\\vb al=find-consistent-bindings}.")
	   (value   "A list of dotted pairs of premise tree nodes and their matching"
		    "nd-lines, iff a matching of the premise tree node with the"
		    "corresponding nd-lines in MATCHINGS consistent to LINE-BINDING is"
		    "possible, otherwise NIL."))
  (let ((front (sys~handler-case (al=find-front node matchings)
				 (al+front-error () nil))))
    (if front
	(let ((consistent (al=find-consistent-bindings (cdr line-binding) front)))
	  (if consistent
	      (apply #'append
		     (mapcar #'(lambda (con)
				 (al=find-all-consistent-bindings (car con)
								    (cdr con)
								    matchings))
			     consistent))
	    (list (cons node (car line-binding)))))
      (list (cons node (car line-binding))))))

(defun al=match-premises-help (preconditions premise-tree binding)
  (declare (edited  "16-APR-1998" "21-MAY-1996")
	   (authors Afiedler Afiedler)
	   (input   "A list of lines, a premise-tree, and a binding.")
	   (effect  "Matches the lines against premise tree.")
	   (value   "A list of tree nodes and their matching lines with bindings, if"
		    "successful, otherwise nil"))
  (if premise-tree
      (let* ((tree-wff (node~formula premise-tree))
	     (matchings
	      (apply #'append
		     (mapcar #'(lambda (premise)
				 (let* ((line-wff (node~formula premise))
					(match (alx~match-p tree-wff line-wff binding
							     nil)))
				   (if match
				       (list (cons premise
						   (if (subst~empty-p match)
						       binding
						     match)))
				     (if (alx~double-negation-p tree-wff)
					 (let ((match (alx~match-p (alx~negation-scope
								     (alx~negation-scope
								      tree-wff))
								    line-wff
								    binding nil)))
					   (if match
					       (list (cons premise
							   (if (subst~empty-p match)
							       binding
							     match)))))))))
			     preconditions))))
	(append (if matchings (list (cons premise-tree matchings)))
		(apply #'append
		       (mapcar #'(lambda (match)
				   (let ((bind (cdr match)))
				     (apply #'append
					    (mapcar #'(lambda (son)
							(al=match-premises-help
							 preconditions
							 son
							 bind))
						    (altr~preconditions premise-tree)))))
			       (if matchings matchings (list (cons 'dummy binding)))))))))

(defun al~match-premises (preconditions premise-tree conclusion binding)
  (declare (edited  "23-AUG-1993 15:08")
	   (authors AFIEDLER)
	   (input   "A list of lines, a premise tree, a line, and a binding.")
	   (effect  "Matches PRECONDITIONS against PREMISE-TREE w.r.t. BINDING,"
		    "by which CONCLUSION matches the root of PREMISE-TREE.")
	   (value   "A list of matching precondition lines, iff successful,"
		    "otherwise NIL."))
  (let ((matchings (al=find-all-consistent-bindings
		    premise-tree
		    (cons conclusion binding)
		    (al=match-premises-help preconditions premise-tree binding))))
    (mapcar #'(lambda (node-and-line)
		(let ((line (cond ((alx~get-best-same-line conclusion
							   (cdr node-and-line)))
				  (t (cdr node-and-line)))))
		  (setf (altr~premise-line (car node-and-line)) line)
		  line))
	    matchings)))



;################################################
;##                                            ##
;##     Auswahl der besten Anwendung           ##
;##                                            ##
;################################################

(defun al=best-preconditions-as-list (nodes)
  (declare (edited  "22-FEB-1994 14:45")
	   (authors AFIEDLER)
	   (input   "A list of premise nodes of the current proof.")
	   (value   "A list of the best premise nodes."))
  (if (every #'alb~line-value (prob~proof-steps pds*current-proof-plan))
      (let ((new-list (mapcar #'altr~valuate-element nodes)))
	(list (car (rassoc (apply #'min
				  (mapcar #'(lambda (node) (cdr (assoc node new-list)))
					  nodes))
			   new-list))))
    (if (> (length nodes) 1)
	(let ((val-list (mapcar #'(lambda (pre)
				    (cons pre (altr~tree-size-without-leaves pre)))
				nodes)))
	  (list (car (rassoc (apply #'min
				    (mapcar #'(lambda (node) (cdr (assoc node val-list)))
					    nodes))
			     val-list))))
      nodes)))

(defun al=find-best-premises (node)
  (declare (edited  "25-FEB-1994 09:28")
	   (authors AFIEDLER)
	   (input   "A premise node.")
	   (value   "A list of the best premises, i.e. the nearest matched nodes to a"
		    "leaf, in the subtree with NODE as root.")) 
  (let* ((pre-list (mapcar #'al=find-best-premises (altr~preconditions node)))
	 (top-list (misc~toplist pre-list)))
    (if top-list
	(cond ((and (altr~or-node-p node) (< 1 (length (altr~preconditions node))))
	       (al=best-preconditions-as-list top-list))
	      ((misc~nil-in-list pre-list) nil)
	      (t top-list))
      (if (and (altr~premise-line node) (not (altr~root-p node)))
	  (list node)))))

(defun al~find-best-premises (node)
  (declare (edited  "25-FEB-1994 09:28")
	   (authors AFIEDLER)
	   (input   "A premise node.")
	   (value   "A list of the best premises, i.e. the nearest matched nodes to a"
		    "leaf, in the subtree with NODE as root.")) 
  (let ((premises (al=find-best-premises node)))
    (unless (misc~nil-in-list premises)
      (misc~toplist premises)))) ; ist dieses misc~toplist noetig, oder ist es schon
				 ; toplist wegen al=find-best-premises?


;### besser woanders hin?
(defun al=all-preceding-lines-with-subset-hypotheses (line hyps)
  (declare (edited  "18-MAR-1998")
	   (authors Afiedler)
	   (input   "A proof line and a list of hypotheses.")
	   (value   "A nested list of LINE's logical predecessor, whose hypotheses are"
		    "a subset of HYPS."))
  (let ((res (mapcar #'(lambda (el)
			 (al=all-preceding-lines-with-subset-hypotheses el hyps))
		     (pdsn~just-premises line))))
    (if (subsetp (pdsn~hyps line) hyps)
	(cons line res)
      res)))

(defun al=all-preceding-lines (line)
  (declare (edited  "01-OCT-1996")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (value   "A nested list of LINE's logical predecessors."))
  (cons line (mapcar #'al=all-preceding-lines (pdsn~just-premises line))))

(defun al=preceding-lines-until-structural (line)
  (declare (edited  "02-OCT-1996")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (value   "A nested list of LINE's logical predecessor until those lines"
		    "justified by structural rules."))
  (let ((just (node~justification line)))
    (cons line (unless (alx~structural-rule-p just)
		       (mapcar #'al=preceding-lines-until-structural
			       (just~premises just))))))

(defun al=preceding-lines-until-structural-or-atom (line)
  (declare (edited  "02-OCT-1996")
	   (authors Afiedler)
	   (input   "A proof line.")
	   (value   "A nested list of LINE's logical predecessor until those lines"
		    "that are atoms or that are justified by structural rules."))
  (let ((just (node~justification line))
	(wff (node~formula line)))
    (cons line (unless (or (alx~structural-rule-p just) (logic~atom-p wff))
		       (mapcar #'al=preceding-lines-until-structural-or-atom
			       (just~premises just))))))

(defun al~preceding-lines (line &optional (mode 'all))
  (declare (edited  "01-OCT-1996")
	   (authors Afiedler)
	   (input   "A proof line and and optional a mode. Possible modes are ALL,"
		    "STRUCT, STRUCT-OR-ATOM, and HYPS-SUBSET.") 
	   (value   "A list of LINE's logical predecessor."))
  (case mode
	(all (misc~toplist (mapcar #'al=all-preceding-lines
				   (pdsn~just-premises line)))); 'all
					; stimmt nicht, da es ueber case hinauslaeuft,
					; dann stimmen hypotheses nicht mehr ueberein,
					; weil Fallunterscheidungen unterschlagen werden
	(struct (let ((just (node~justification line)))
		  (unless (alx~structural-rule-p just)
			  (misc~toplist (mapcar #'al=preceding-lines-until-structural
						(just~premises just))))))
	(struct-or-atom
	 (let ((just (node~justification line))
	       (wff (node~formula line)))
	   (unless (or (alx~structural-rule-p just) (logic~atom-p wff))
		   (misc~toplist (mapcar #'al=preceding-lines-until-structural-or-atom
					 (just~premises just))))))
	(hyps-subset
	 (let ((hyps (pdsn~hyps line)))
	   (misc~toplist
	    (mapcar #'(lambda (l)
			(al=all-preceding-lines-with-subset-hypotheses l hyps))
		    (pdsn~just-premises line)))))
	(otherwise (error "~A is not a defined mode for al~preceding-lines." mode))))

(defgeneric al~get-best-premises (premise-node line binding)
  (declare (edited  "07-JAN-1994 09:34")
	   (authors AFIEDLER)
	   (input   "A premise node, a proof line and a binding as described in"
		    "{\\vb alx~match-p}.")
	   (effect  "If possible, this function writes the matching lines in the `line'"
		    "slots of the nodes in the premise tree with PREMISE-NODE as root,"
		    "and chooses the best premises, i. e. the nearest matched nodes to a"
		    "leaf.")
	   (value   "A list of the names of the chosen nodes."))
  (:method (premise-node line binding)
	   (when alx*tracer
	     (omega~output "~% Matching premises   "))
	   (let ((result (al~match-premises (al~preceding-lines line 'hyps-subset)
					      premise-node
					      line binding))) ;noch besser: nicht alle
					;preceding lines, sondern
					;nur solche, deren Hypothesen Teilmenge der
					;Hypothesen der aktuellen Line sind.
	     (if (and result (not (keim~equal result (list line))))
		 (progn (when alx*tracer
			  (omega~output " succeeded~%"))
			result)
	       (progn (when alx*tracer
			(omega~output " failed~%"))
		      nil))))
  (:method :after (premise-node line binding)
	   (declare (ignore line binding))
	   (altr~show-cond premise-node)))




;################################################
;##                                            ##
;##     Finden des ausgewaehlten Baums         ##
;##                                            ##
;################################################

(defun al~applied-tree (trees just-lines)
  (declare (edited  "01-FEB-1994 15:52")
	   (authors AFIEDLER)
	   (input   "A list of premise trees and a list of premise lines.")
	   (values  "Returns two values:"
		    "The premise tree, that results in JUST,"
		    "and the justifying premise nodes."))
  (let* ((premises-lists (mapcar #'al~find-best-premises trees))
	 (just-list (mapcar #'(lambda (premises)
				(if premises
				    (alx~sort-premises
				     (altr~premises2lines premises)
				     (car just-lines))))
			    premises-lists))
	 (pos (position just-lines
			(mapcar #'(lambda (x) (if x x (list x))) just-list)
			:test #'(lambda (a b)
				  (not (and (set-difference a b)
					    (set-difference b a))))))
	 
	 (result (nth pos trees)))
    
    (altr~show-cond result)    
    (values (nth pos trees) (nth pos premises-lists))))







