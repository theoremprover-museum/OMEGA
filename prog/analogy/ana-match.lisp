;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ana-match.lisp; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(in-package "OMEGA")

;; this file contains the matcher for analogy


(mod~defmod ANAMATCH
            :uses (beta bind data gb hou keim logic match node omega pds pdsn post subst term)
            :documentation "The matcher for analogy."
            :exports (ana+matcher
                      
                      ana~create-match-problem
                      ana~match
                      ana~matcher-costs
                      ana~matcher-depth
                      ana~matcher-dummy
                      ana~matcher-max-costs
                      ana~matcher-p
                      ana~matcher-repairs
                      ana~matcher-source-node
                      ana~matcher-subst
                      ana~matcher-target-node
                      ana~matcher-term-maps
                      ana~matcher-terms
                      ana~matcher-top-level-repair
		      ana~node-equal
                      ana~set-matcher-costs
                      ana~set-matcher-subst
                      ana~set-matcher-terms
                      
                      ana*add-condition-cost
                      ana*add-conjunct-cost
                      ana*add-conclusion-cost
                      ana*add-disjunct-cost
                      ana*add-equiv-cost
                      ana*add-existential-quantifier-cost
		      ana*add-not-cost
                      ana*add-universal-quantifier-cost
                      ana*counter
                      ana*del-condition-cost
                      ana*del-conjunct-cost
                      ana*del-conclusion-cost
                      ana*del-disjunct-cost
                      ana*del-equiv-cost
                      ana*del-existential-quantifier-cost
		      ana*del-not-cost
                      ana*del-universal-quantifier-cost
                      ana*match-verbose
                      ana*maximal-matching-depth-difference
                      ana*maximal-matching-solutions
                      ana*not-same-type
                      ana*swap-implies-cost
                      ana*term-map-costs))

(defvar ana*match-verbose nil "Verbose mode for the matcher.")


(defvar ana*maximal-matching-solutions 1
  "The maximal number of solutions for a matching problem.")

(defvar ana*counter 0 "The counter of the matching depth.")

(defvar ana*maximal-matching-depth-difference 200)

(defvar ana*matched-nodes 0 "The number of nodes we matched (needed for experiments)")

;; the costs are a vector:(term-abb add/del bindings)
(defvar ana*add-conjunct-cost '(1 0 0))
(defvar ana*add-disjunct-cost '(1 0 0))
(defvar ana*add-equiv-cost '(1 0 0))
(defvar ana*add-condition-cost '(1 0 0))
(defvar ana*add-conclusion-cost '(1 0 0))
(defvar ana*add-universal-quantifier-cost '(1 0 0))
(defvar ana*add-existential-quantifier-cost '(1 0 0))
(defvar ana*add-not-cost '(1 0 0))
(defvar ana*del-conjunct-cost '(1 0 0))
(defvar ana*del-disjunct-cost '(1 0 0))
(defvar ana*del-equiv-cost '(1 0 0))
(defvar ana*del-condition-cost '(1 0 0))
(defvar ana*del-conclusion-cost '(1 0 0))
(defvar ana*del-universal-quantifier-cost '(1 0 0))
(defvar ana*del-existential-quantifier-cost '(1 0 0))
(defvar ana*del-not-cost '(1 0 0))
(defvar ana*swap-implies-cost '(2 0 0))
(defvar ana*term-map-costs '(0 1 0))
(defvar ana*not-same-type '(0 0 0))

;---------------------------------------------------------
; match data-structure and constructor functions
;---------------------------------------------------------

(eval-when (compile eval load)
  (defclass ana+matcher (keim+name)
    ((terms :initform nil :initarg :terms
	    :accessor ana=matcher-terms
	    :documentation "The term to be matched.")
     (subst :initform (subst~create nil nil)
	    :initarg :subst
	    :accessor ana=matcher-subst
	    :documentation "The substitution.")
     (term-maps :initform nil :initarg :term-maps
		:accessor ana=matcher-term-maps
		:documentation "The terms that have been mapped on each other.")
     (costs :initform '(0 0 0)
	    :accessor ana=matcher-costs :initarg :costs
	    :documentation "The costs of the matcher.")
     (source-node :initform nil :initarg :source-node
		  :accessor ana=matcher-source-node
		  :documentation "The source-node the matching problem corresponds to.")
     (target-node :initform nil :initarg :target-node
		  :accessor ana=matcher-target-node
		  :documentation "The target-node the matching problem corresponds to.")
     (max-costs :initform nil :initarg :max-costs
		:accessor ana=matcher-max-costs
		:documentation "The maximum costs this problem may cost.")
     (repairs :initform nil :initarg :repairs
	      :accessor ana=matcher-repairs
	      :documentation "The structural changes made to make a match possible.")
     (top-flag :initform t  ;; internal use only!
	       :initarg :top-flag
	       :accessor ana=matcher-top-flag-h
	       :documentation "Needed to check if a repair is a top-level repair.")
     (top-level-repair :initform nil :initarg :top-level-repair
		       :accessor ana=matcher-top-level-repair
		       :documentation "The repair made on top level.")
     (dummy :initform nil :initarg :dummy
	    :accessor ana=matcher-dummy
	    :documentation "A dummy needed to use Karstens matcher.")
     (depth :initform 0 :initarg :depth
	    :accessor ana=matcher-depth
	    :documentation "The depth at which this problem was created.")
     )
    (:documentation "Data structure for extended matching.")))

(defgeneric ana~matcher-p (thing)
  (:method ((matcher ana+matcher))
	   t))
;  (:method (thing)
;           (declare (ignore thing))
;           nil))

(defgeneric ana~matcher-terms (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-terms matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defun ana~set-matcher-terms (matcher new-terms)
  (progn (setf (ana=matcher-terms matcher) new-terms)
	 matcher))

(defgeneric ana~matcher-subst (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-subst matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defun ana~set-matcher-subst (matcher new-subst)
  (progn (setf (ana=matcher-subst matcher) new-subst)
	 matcher))

(defgeneric ana~matcher-term-maps (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-term-maps matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana~matcher-costs (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-costs matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defun ana~set-matcher-costs (matcher new-costs)
  (progn (setf (ana=matcher-costs matcher) new-costs)
	 matcher))


(defgeneric ana~matcher-source-node (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-source-node matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana~matcher-target-node (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-target-node matcher)))
;  (:method (matcher)
;           ;(declare (ignore matcher))
;           (omega~warn "Problem !! ~s" matcher)
;           nil))

(defgeneric ana~matcher-max-costs (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-max-costs matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana~matcher-repairs (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-repairs matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana=matcher-top-flag (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-top-flag-h matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana~matcher-top-level-repair (matcher &optional old-subst)
  (:method ((matcher ana+matcher) &optional old-subst)
;           (when (eq (ana~matcher-subst matcher) 'not-matched)
;             ;; if the matcher was not really matched, we do it now in order
;             ;; to be able to return the top-level-repair
;             ;; we choose the first one, as it will be the cheapest and we have only one
;             ;; target node.
;             (let ((new-matcher (first (ana~match (list (ana~matcher-source-node matcher))
;                                                  (list (ana~matcher-target-node matcher))
;                                                  old-subst))))
;               ;; now we replace the new values in our matcher:
;               (setf (ana=matcher-subst matcher) (ana~matcher-subst new-matcher)
;                     (ana=matcher-term-maps matcher) (ana~matcher-term-maps new-matcher)
;                     (ana=matcher-costs matcher) (ana~matcher-costs new-matcher)
;                     (ana=matcher-repairs matcher) (ana~matcher-repairs new-matcher)
;                     (ana=matcher-top-level-repair matcher) (ana~matcher-top-level-repair new-matcher))
;               ))
;             ;; now return the top-level-repair:
	   (declare (ignore old-subst))
	   (ana=matcher-top-level-repair matcher)))
  
;  (:method (matcher &optional old-subst)
;           (declare (ignore matcher old-subst))
;           nil))

(defgeneric ana~matcher-dummy (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-dummy matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defgeneric ana~matcher-depth (matcher)
  (:method ((matcher ana+matcher))
	   (ana=matcher-depth matcher)))
;  (:method (matcher)
;           (declare (ignore matcher))
;           nil))

(defmethod print-object ((object ana+matcher) (stream stream))
  (format stream "(~a)" (list ;concatenate 'string
				     "ana+matcher: "
				     (format nil "~a" (keim~name object))
				     (if (ana~matcher-terms object)
					 (concatenate 'string
						      " terms: "
						      (format nil "~a" (ana~matcher-terms object)))
				       "")
				     (if (ana~matcher-subst object)
					 (concatenate 'string
						      " subst: "
						      (format nil "~a" (ana~matcher-subst object)))
				       "")
				     (if (ana~matcher-term-maps object)
					 (concatenate 'string
						      " term-maps: "
						      (format nil "~a" (ana~matcher-term-maps object)))
				     "")
				     (if (ana~matcher-repairs object)
					 (concatenate 'string
						      " repairs: "
						      (format nil "~a" (ana~matcher-repairs object)))
				       "")
				     " costs: "
				     (format nil "~a" (ana~matcher-costs object))
				     )))


(defun ana~create-match-problem (&optional &key ((:terms terms)) ((:subst subst))
					   ((:old-prob old-prob))
					   ((:term-maps term-maps)) ((:costs costs))
					   ((:source-node source-node))
					   ((:target-node target-node))
					   ((:max-costs old-max-costs))  ((:repairs repairs))
					   ((:top-flag top-flag)) ((:top-level-repair top-level-repair))
					   ((:dummy dummy)))
  (declare (ignore top-level-repair))
  ;; first check if the problem to be created does not exceed the cost-limit:
  (let ((max-costs (or old-max-costs (and old-prob (ana~matcher-max-costs old-prob)))))
;    (if (> ana*counter 20000) nil;(omega~message "Aborting matching! Limit exceded!")
    ;; we don't test for counter here, because it produces error messages later on
    ;; we test in ana=create-ref
    (when (or (not max-costs) (ana=costs-<=-p costs max-costs))
;      (unless (and repairs ;;we do not create refs which contain logical symbols
;                   (ana=contains-logical-symbol-p (second (first (last repairs)))))
	(make-instance 'ana+matcher :terms
		       terms
		       :subst (or subst (bind~local-substitution))
		       :term-maps (or term-maps (and old-prob (ana~matcher-term-maps old-prob)))
		       :costs (or (and costs
				       repairs
				       (ana=add-costs costs
						      (ana=repair-costs
						       (first (last repairs)))))
				  costs
				  (and old-prob (ana~matcher-costs old-prob)))
		       :source-node (or source-node (and old-prob (ana~matcher-source-node old-prob)))
		       :target-node (or target-node (and old-prob (ana~matcher-target-node old-prob)))
		       :max-costs (or max-costs (and old-prob (ana~matcher-max-costs old-prob)))
		       :repairs (or repairs (and old-prob (ana~matcher-repairs old-prob)))
		       :dummy (or dummy (and old-prob (ana~matcher-dummy old-prob)))
		       :top-flag top-flag
		       :top-level-repair (and old-prob
					      (or (ana~matcher-top-level-repair old-prob)
						  (when (ana=matcher-top-flag old-prob)
						    repairs)))
		       :depth ana*counter
		       :name (make-symbol (format nil "~As-~At-~A"
						  (or source-node (and old-prob (ana~matcher-source-node old-prob)))
						  (or target-node (and old-prob (ana~matcher-target-node old-prob)))
						  (incf ana*counter)))
		       )))
  );)

;---------------------------------------------------------
; the main match functions:
;---------------------------------------------------------

(defun ana~match (source-nodes-h target-nodes-h2 &optional old-subst max-costs)
  (when (and target-nodes-h2 source-nodes-h);;if there are no corresponding nodes, we cannot match...
    (let* ((css (some #'ana=constraint-state-p source-nodes-h))
	   (cst (some #'ana=constraint-state-p target-nodes-h2))
	   (source-nodes (or (and css (remove css source-nodes-h
					      :test #'ana~node-equal))
			     source-nodes-h))
	   (target-nodes-h (or (and cst (remove cst target-nodes-h2
						:test #'ana~node-equal))
			       target-nodes-h2))
	   )
      (if (and css cst) ;we are matching the constraint states!
	  (list (ana~create-match-problem :source-node css :target-node cst :subst
					  (subst~create nil nil) :costs '(0 0 0)))
	(let ((source-terms (if (and (listp source-nodes) (pdsn~p (first source-nodes)))
			    ;;if the source-node was not yet variablelized, do it now and
			    ;;save the variabelized term in the property list:
			    (mapcar
			     #'ana~variabelize
			     source-nodes)
			  source-nodes))
	  (target-terms (if (listp target-nodes-h)
			    (mapcar #'node~formula target-nodes-h)
			  (list target-nodes-h)))
	  (target-nodes (if (listp target-nodes-h) target-nodes-h
			  (list target-nodes-h)))
	  )
      (setf ana*counter 0)
      (when ana*match-verbose
	(omega~trace "Now matching the nodes ~S and ~S with formulas ~%~S~%and~%~S~%with substitution ~s"
		     source-nodes target-nodes source-terms target-terms old-subst))
      (when (listp target-nodes)
	(setf ana*matched-nodes (+ ana*matched-nodes (length target-nodes))))
      (let ((solution
	     (ana=solution-reduce
	      (reverse
	       (ana=match
		(mapcan
		 #'(lambda (source-node source-term)
		     (mapcar #'(lambda (target-term target-node)
				 (ana~create-match-problem :terms (list(list source-term target-term))
							   :subst old-subst
							   :costs '(0 0 0)
							   :source-node source-node
							   :target-node target-node
							   :top-flag t
							   :max-costs max-costs
							   :dummy (ana=create-dummy target-term)
							   ))
			     target-terms target-nodes))
		 source-nodes source-terms)
		nil)
	       ))))
	(when ana*match-verbose (omega~trace "These solutions were found:~%~S" solution))
	solution
	))))))

(defun ana~node-equal (node1 node2)
  (eq (keim~name node1)
      (keim~name node2)))

(defun ana=constraint-state-p (node)
  ;; determines wheter a node represents the constraint state
  (or (and (pdsn~p node)
	   (data~variable-p (node~formula node))
	   (eq (keim~name (node~formula node)) 'cosie-meta-cs)
	   node)
      (and (pdsn~p node)
	   (eq (keim~name (just~method (node~justification node)))
	       'prove-cs-answer)
	   node)))

      

(defun ana=create-dummy (term)
  ;; creates an dummy uni+analogy problem to be used with Karstens unifier
  (let* ((prob (make-instance 'uni+analogy))
	 (free-variables (data~free-variables term))
	 )
    (mapcar #'(lambda (var)
		(match~hash-match-only var (uni~match-only-table prob)))
	    free-variables)
    (setf (uni~match-only prob) free-variables)
    prob))
    
    
(defun ana=match (matching-problems solutions)
  (let ((cheapest-matching-problem
	 (ana=get-cheapest-matching-problem matching-problems solutions)))
    (if (or (not matching-problems)
	    (and (>= (length solutions) ana*maximal-matching-solutions)
		 ;; we want to return all matching problem with the same costs (do we??)
		 (ana=costs-<-p (ana~matcher-costs (first solutions))
				(ana~matcher-costs cheapest-matching-problem))))
	;; have we found enough solutions?
	solutions
      ;; else continue the match
      (let ((match-result (ana=solve-matching-problem
			   cheapest-matching-problem)))
	(if (ana=solved match-result)
	    (ana=match (ana=get-rest-of-matching-problem matching-problems)
		       (ana=add-solution match-result solutions))
	  (ana=match (ana=merge-matching-problems
		      match-result
		      (ana=get-rest-of-matching-problem matching-problems))
		     solutions))))))
  
(defun ana=solution-reduce (solutions)
  (dolist (i solutions solutions)
    (when (ana=matcher-subst i)
      (setf (ana=matcher-subst i)
	    (subst~reduce (ana=matcher-subst i) :restrict-vars
			  (data~free-variables
			   (or (ana~variabelize (ana~matcher-source-node i))
			       (ana~matcher-source-node i));; needed if we match terms not nodes
			   ))))
    (setf (ana=matcher-repairs i)
	  (mapcar #'(lambda (repair)
		      (list (first repair)
			    (ana=replace-nullvars-by-reference (second repair))
			    ))
		  (ana=matcher-repairs i)))
    (setf (ana=matcher-term-maps i)
	  (mapcar #'(lambda (map)
		      (list (ana=replace-nullvars-by-reference (first map))
			    (ana=replace-nullvars-by-reference (second map))
			    ))
		  (ana=matcher-term-maps i)))
    ))

(defgeneric ana=replace-nullvars-by-reference (term)
  (:method ((term hou+nullvariable))
	   (or (cdr (assoc 'keim::reference-variable (keim~plist term)))
	       term))
  (:method ((term term+appl))
	   (data~appl-create
	    (ana=replace-nullvars-by-reference (data~appl-function term))
	    (mapcar #'ana=replace-nullvars-by-reference
		    (data~appl-arguments term))))
  (:method (term)
	   term))
  

(defun ana=merge-matching-problems (new-probs old-probs)
  ;; important! the new-problems have to be in the right order as we merge
  (if new-probs
      (merge 'list new-probs old-probs #'ana=costs-<=-p :key #'ana~matcher-costs)
    old-probs))

(defun ana=solved (match-result)
  ;; a problem is solved if there are no terms left
  (and match-result (not (ana~matcher-terms (first match-result))))) ;;first as
								     ;;match-result is
								     ;;always a list


(defun ana=add-solution (match-result solutions)
  (cons (first match-result) solutions))

(defun ana=get-cheapest-matching-problem (uni-probs solutions)
  ;; z.b(div-var 1-var n) (div 1 (times n n)) kann auch mit div->(lam x y ) div x (times n n)
  ;; wollen aber lieber n-> n*n
  (when uni-probs
    (if (and solutions
	     (> (- (ana~matcher-depth (first uni-probs))
		   (ana~matcher-depth (first solutions)))
		ana*maximal-matching-depth-difference))
	(ana=get-cheapest-matching-problem (rest uni-probs) solutions)
      (first uni-probs))))

(defun ana=get-rest-of-matching-problem (uni-probs)
  (rest uni-probs))

(defun ana=solve-matching-problem (matching-problem)
  (when matching-problem
  (let ((bindings (ana~matcher-subst matching-problem)))
    (bind~with-bindings
     (; first restore the old binding-context
      (when bindings (mapcar #'bind~bind (subst~domain bindings) (subst~codomain bindings)))
					; then match
      (remove nil
	      (ana=match-terms (ana~matcher-terms matching-problem) ;; the terms to be matched
			       matching-problem)))
     :insert nil))
  ))

(defun ana=match-terms (terms problem)
  (if terms
      (let ((term1 (first (first terms)))
	    (term2 (second (first terms))))
	(if (and (data~appl-p term1)
		 (not (beta~redex-p term1))
		 (bind~binding (data~appl-function term1)))
	    ;;we have a function wich has a binding. In order to make term-mappings
	    ;;possible, we create two match-problems
	    (append
	     (list (ana~create-match-problem
		    :terms (append (list (list (beta~head-normalize term1)
					       (beta~head-normalize term2)))
				   (rest terms))
		    :old-prob problem))
	     (ana=create-ref 'term-map term1 term2
			     (rest terms) problem))
	  (if (and (data~variable-p term1)
		   (bind~binding term1))
	      (append
	       (list (ana~create-match-problem
		      :terms (append (list (list (beta~head-normalize term1)
						 (beta~head-normalize term2)))
				     (rest terms))
		      :old-prob problem))
	       (ana=create-ref 'term-map term1 term2
			       (rest terms) problem))
	    (ana=match-terms-h (if (or (beta~redex-p term1)
				       (data~variable-p term1))
				   (beta~head-normalize term1)
				 term1)
			       (if (beta~redex-p term2)
				   (beta~head-normalize term2)
				 term2)
			       (rest terms) problem))));)
    ; if there are no terms left, return the solved problem with the inserted subst
    (list (ana~set-matcher-subst (ana~set-matcher-terms problem nil)
				 (bind~local-substitution)))))

(defgeneric ana=match-terms-h (source-term target-term rest-terms prob)

  (:method ((source-term hou+nullvariable) (target-term hou+nullvariable)
            rest-terms prob)
           (when (hou~trivial-p source-term target-term (ana~matcher-dummy prob))
             (ana=match-terms rest-terms prob)))
  
  (:method ((source-term hou+nullvariable) target-term
            rest-terms prob)
	   ;; we want to match (forall (n num) (div 1 n)) with (forall (n num) (div 1 n*n))
	   ;; therefor we have to make mappings of nullvariables possible!
	   (ana=create-ref 'term-map source-term target-term rest-terms prob))
  
  (:method ((source-term T) (target-term hou+nullvariable)
            rest-terms prob)
           (when (hou~flex-p source-term (ana~matcher-dummy prob))
	     (ana=match-terms rest-terms prob)))
  
  (:method ((source-term term+variable) (target-term term+appl)
	    rest-terms prob)
	   ;; first check, if the source-term comes from the target:
	   (unless (match~match-only-p source-term (ana~matcher-dummy prob))
	     (let ((target-fct (data~appl-function target-term)))
	       (if (logic~connective-p target-fct)
					; if the target-fct is a logical symbol,
					; we bind normally, we create an add ref, and a term-map
		   (append
		    (let ((bound (uni~bind source-term target-term (ana~matcher-dummy prob))))
		      (when bound
			(list (ana~create-match-problem :terms rest-terms :old-prob prob))))
		    (ana=create-ref 'add source-term target-term rest-terms prob)
		    ;(ana=create-ref 'term-map source-term target-term rest-terms prob)
		    ; mal rausnehmen. Wird ja sowieso nicht gemacht.
		    )
		 ;; else bind the variable
		 (let ((bound (uni~bind source-term target-term (ana~matcher-dummy prob))))
		   (if bound
		     (list(ana~create-match-problem :terms rest-terms :old-prob prob
						    :costs (ana=add-costs ana*not-same-type
									  (ana~matcher-costs prob))))
		     (ana=create-ref 'term-map source-term target-term rest-terms prob)
		     ))))))
		      
  (:method ((source-term term+variable) target-term
	    rest-terms prob)
	   ;; first check if the source-term comes from the target:
	   (if (match~match-only-p source-term (ana~matcher-dummy prob))
	       (when (hou~trivial-p source-term target-term (ana~matcher-dummy prob))
		 (ana=match-terms rest-terms prob))
	     ;;  we prefer binding variables on terms with the same type:
	     (if (or (gb~helpvariable-p source-term)
		     (data~equal (term~type source-term)
				 (term~type target-term)))
		 (let ((bound (uni~bind source-term target-term (ana~matcher-dummy prob))))
		   (if bound
		       (ana=match-terms rest-terms prob)
		     ;; why this else case??
		     (ana=create-ref 'term-map source-term target-term rest-terms prob)
		     ))
	       (let ((bound (uni~bind source-term target-term (ana~matcher-dummy prob))))
		 (if bound
		     (list(ana~create-match-problem :terms rest-terms :old-prob prob
						    :costs (ana=add-costs ana*not-same-type
									  (ana~matcher-costs
									   prob))))
		   (ana=create-ref 'term-map source-term target-term rest-terms prob)
		   )))))

  (:method ((source-term term+constant) target-term
	    rest-terms prob)
	   (when (hou~trivial-p source-term target-term (ana~matcher-dummy prob))
	     (ana=match-terms rest-terms prob)))

  (:method ((source-term term+appl) (target-term term+variable)
	    rest-terms prob)
	   ;; treat target-vars as constants
	   (if (logic~connective-p (data~appl-function source-term))
					; if the source-fct is a logical symbol, we create a reformulation and
					; recurse on the two arguments to find the better term to delete
	       (append (ana=create-ref 'del source-term target-term rest-terms prob)
		       ;(ana=create-ref 'term-map source-term target-term rest-terms prob)
					; mal rausnehmen. Wird ja sowieso nicht gemacht.
		       )
	     (when (hou~flex-p source-term (ana~matcher-dummy prob))
	       ;; return the possible alternatives:
	       ;;  1. the mgbs
	       ;;  2. term-match
	       (append (ana=create-ref 'mgb source-term target-term rest-terms prob)
		       (ana=create-ref 'term-map source-term target-term rest-terms prob)
		       ))
	       ;; otherwise it comes from the source, therefore nil
	     ))

  
  (:method ((source-term term+appl) (target-term term+constant)
	    rest-terms prob)
	   (if (logic~connective-p (data~appl-function source-term))
		 ; if the source-fct is a logical symbol, we create a reformulation and
		 ; recurse on the two arguments to find the better term to delete
	       ;(append
		(ana=create-ref 'del source-term target-term rest-terms prob)
		       ;(ana=create-ref 'term-map source-term target-term rest-terms prob))
					; mal rausnehmen. Wird ja sowieso nicht gemacht.		       
;	   (if (ana~exists-reformulation-for-term source-term) ???????????????????
	     (when (keim::hou~flex-p source-term (ana~matcher-dummy prob))
	       ;; return the possible alternatives:
	       ;;  1. the mgbs
	       ;;  2. term-match
	       (append (ana=create-ref 'mgb source-term target-term rest-terms prob)
		       (ana=create-ref 'term-map source-term target-term rest-terms prob))
	       ;; sonst wenn kein flex, dann aus target, also nil
	       )))

  (:method ((source-term term+appl) (target-term term+abstr)
	    rest-terms prob)
	   (when (hou~flex-p source-term (ana~matcher-dummy prob))
	     ;; no flex, then from target!
	     (append (ana=create-ref 'mgb source-term target-term rest-terms prob)
		     (ana=create-ref 'term-map source-term target-term rest-terms prob)
		     )))

  (:method ((source-term term+appl) (target-term term+appl)
	    rest-terms prob)
	   (let ((source-fct (data~appl-function source-term))
		 (target-fct (data~appl-function target-term)))
	     (if (logic~connective-p source-fct)
		 (if (logic~connective-p target-fct)
		     (if (data~equal target-fct source-fct)
			 ;; if source and target are the same logical symbols
			 (append (ana=create-ref 'decompose source-term target-term
						 rest-terms prob)
				 (ana=create-ref 'add source-term target-term
						 rest-terms prob)
				 (ana=create-ref 'del source-term target-term
						 rest-terms prob)
				 ;(ana=create-ref 'term-map source-term target-term
					;	 rest-terms prob)
				 )
		       ;; if source and target are different logical symbols
		       (append ;(ana=create-ref 'replace source-term target-term
					;		     rest-terms term-abb costs refs node)
			(ana=create-ref 'add source-term target-term rest-terms prob)
			(ana=create-ref 'del source-term target-term rest-terms prob)
			;(ana=create-ref 'term-map source-term target-term rest-terms
			;prob)
			))
		   ;; if source is a logical symbol and target is no logical symbol
		   ;(append
		    (ana=create-ref 'del source-term target-term rest-terms prob)
		    ;(ana=create-ref 'term-map source-term target-term rest-terms prob))
		    )
	       (if (logic~connective-p target-fct)
		   ;; if  target is a logical symbol and source is no logical symbol
		   ;(append ;(ana=create-ref 'mgb source-term target-term
					;		 rest-terms term-abb costs refs node)
		    (ana=create-ref 'add source-term target-term rest-terms prob)
		    ;(ana=create-ref 'term-map source-term target-term rest-terms prob))
		 ;; if neither target nor source are logical symbols
		 (if (hou~flex-p source-term (ana~matcher-dummy prob))
		     ;; if source is flex
		     (let ((result
			    (if (ana~term-equal-p source-fct target-fct)
				;; if the source and target term have the 'same' function symbol, then
				;; first treat the source-function-symbol as a constant and try to
				;; match
				(append (ana=create-ref 'decompose source-term target-term
							rest-terms prob)
					(ana=create-ref 'mgb source-term target-term
							rest-terms prob)
					(ana=create-ref 'term-map source-term target-term
							rest-terms prob))
			      ;; else, the function symbols are different
			      (append (ana=create-ref 'mgb source-term target-term
						      rest-terms prob)
				      (ana=create-ref 'term-map source-term target-term
						      rest-terms prob)))))
		       (append (ana=special-case-match source-term target-term rest-terms
						       prob)
			       result))
		   ;; else the source-term is rigid, so it comes out of the source
		   (ana=create-ref 'decompose source-term target-term rest-terms prob)
;		   (let ((decompose (hou~decompose source-term target-term (ana~matcher-dummy prob))))
;					;		   (omega~message "hallo2 ~A" source-term)
;		     (when decompose
;		       (ana=match-terms (append decompose rest-terms) prob)))
		   )))))
  
  (:method (source-term target-term rest-terms prob)
	   (let ((termpair (hou~alpha-eta source-term target-term (ana~matcher-dummy prob))))
	     (ana=match-terms (cons (mapcar #'beta~contract
					    ;; beta contract because we do not normalize with bindings
					    termpair)
				    rest-terms) prob)))
  )

(defun ana=special-case-match (source-appl target-appl rest-terms prob)
  (declare (edited  "08-JUL-1999")
	   (authors Cullrich)
	   (input   "A source and a target term, more terms and a matching-problem.")
	   (effect  )
	   (value   "Depending on which special cases have been defined, we create new"
		    "matching problems."))
  (cond
   (;; if the source-applications has the form (times x x)
    (and (string= "TIMES-VAR" (keim~name (data~appl-function source-appl)))
	 (data~equal (first (data~appl-arguments source-appl))
		     (second (data~appl-arguments source-appl))))
    ;; then create a new matching problem with (power x 2)
    (list ;;list because we have to return a list of new problems!
     (ana~create-match-problem
      :terms (cons (list (data~appl-create
			  (ana=lookup-object 'power)
			  (list (first (data~appl-arguments source-appl))
				(ana~variabelize (ana=lookup-object 2))))
			 target-appl)
		   rest-terms)
      :old-prob prob)))))

(defun ana=lookup-object (thing)
  (post~read-object thing (pds~environment omega*current-proof-plan)
		    :existing-term))


(defun ana=create-ref (typ source-term target-term rest-terms prob)
  (when (< ana*counter 20000)
  (let ((costs (ana~matcher-costs prob)))
    (case typ
      ('add (let ((target-fct (data~appl-function target-term))
		  (args (data~appl-arguments target-term))
		  )
	      (case (keim~name target-fct)
		('and (list		; if we add a conjunction, try both arguments
		       (ana~create-match-problem
			:terms (cons (list source-term (first args)) rest-terms)
			:costs (ana=add-costs ana*add-conjunct-cost costs)
			:repairs (append (ana~matcher-repairs prob) (list (list 'add-conjunct (second args))))
			:old-prob prob)
		       (ana~create-match-problem
			:terms (cons (list source-term (second args)) rest-terms)
			:costs (ana=add-costs ana*add-conjunct-cost costs)
			:repairs (append (ana~matcher-repairs prob) (list (list 'add-conjunct (first args))))
			:old-prob prob)
			))
		('or (list		; if we add a disjunction, try both arguments
		      (ana~create-match-problem
		       :terms (cons (list source-term (first args)) rest-terms)
		       :costs (ana=add-costs ana*add-disjunct-cost costs)
		       :repairs (append (ana~matcher-repairs prob) (list (list 'add-disjunct (second args))))
		       :old-prob prob)
		      (ana~create-match-problem
		       :terms (cons (list source-term (second args)) rest-terms)
		       :costs (ana=add-costs ana*add-disjunct-cost costs)
		       :repairs (append (ana~matcher-repairs prob) (list (list 'add-disjunct (first args))))
		       :old-prob prob)))
		('equiv (list		; if we add an equivalence, try both arguments
			 (ana~create-match-problem
			  :terms (cons (list source-term (first args)) rest-terms)
			  :costs (ana=add-costs ana*add-equiv-cost costs)
			  :repairs (append (ana~matcher-repairs prob) (list (list 'add-equiv (second args))))
			  :old-prob prob)
			 (ana~create-match-problem
			  :terms (cons (list source-term (second args)) rest-terms)
			  :costs (ana=add-costs ana*add-equiv-cost costs)
			  :repairs (append (ana~matcher-repairs prob) (list (list 'add-equiv (first args))))
			  :old-prob prob)))
		('implies (list		; if we add an equivalence, try both arguments
			   (ana~create-match-problem
			    :terms (cons (list source-term (first args)) rest-terms)
			    :costs (ana=add-costs ana*add-condition-cost costs)
			    :repairs (append (ana~matcher-repairs prob) (list (list 'add-condition (second args))))
			    :old-prob prob)
			   (ana~create-match-problem
			    :terms (cons (list source-term (second args)) rest-terms)
			    :costs (ana=add-costs ana*add-conclusion-cost costs)
			    :repairs (append (ana~matcher-repairs prob) (list (list 'add-conclusion (first args))))
			    :old-prob prob)))
		('forall (list
			  (ana~create-match-problem
			   :terms (cons (list source-term (data~abstr-range (first args))) rest-terms)
			   :costs (ana=add-costs ana*add-universal-quantifier-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'add-universal-quantifier (data~abstr-domain 
										   (first args)))))
			   :old-prob prob)))
		('exists (list
			  (ana~create-match-problem
			   :terms (cons (list source-term (data~abstr-range (first args))) rest-terms)
			   :costs (ana=add-costs ana*add-existential-quantifier-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'add-existential-quantifier (data~abstr-domain
										     (first args)))))
			   :old-prob prob)))
		('not    (list
			  (ana~create-match-problem
			   :terms (cons (list source-term (first args)) rest-terms)
			   :costs (ana=add-costs ana*add-not-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'add-not)))
			   :old-prob prob)))
		)))
      ('del (let ((source-fct (data~appl-function source-term))
		  (args (data~appl-arguments source-term))
		  )
	      (case (keim~name source-fct)
		('and (list		; if we del a conjunction, try both arguments
		       (ana~create-match-problem
			:terms (cons (list (first args) target-term) rest-terms)
			:costs (ana=add-costs ana*del-conjunct-cost costs)
			:repairs (append (ana~matcher-repairs prob) (list (list 'del-conjunct (second args))))
			:old-prob prob)
		       (ana~create-match-problem
			:terms (cons (list (second args) target-term) rest-terms)
			:costs (ana=add-costs ana*del-conjunct-cost costs)
			:repairs (append (ana~matcher-repairs prob) (list (list 'del-conjunct (first args))))
			:old-prob prob)))
		('or (list		; if we del a disjunction, try both arguments
		      (ana~create-match-problem
		       :terms (cons (list (first args) target-term) rest-terms)
		       :costs (ana=add-costs ana*del-disjunct-cost costs)
		       :repairs (append (ana~matcher-repairs prob) (list (list 'del-disjunct (second args))))
		       :old-prob prob)
		      (ana~create-match-problem
		       :terms (cons (list (second args) target-term) rest-terms)
		       :costs (ana=add-costs ana*del-disjunct-cost costs)
		       :repairs (append (ana~matcher-repairs prob) (list (list 'del-disjunct (first args))))
		       :old-prob prob)))
		('equiv (list		; if we del an equivalence, try both arguments
			 (ana~create-match-problem
			  :terms (cons (list (first args) target-term) rest-terms)
			  :costs (ana=add-costs ana*del-equiv-cost costs)
			  :repairs (append (ana~matcher-repairs prob) (list (list 'del-equiv (second args))))
			  :old-prob prob)
			 (ana~create-match-problem
			  :terms (cons (list (second args) target-term) rest-terms)
			  :costs (ana=add-costs ana*del-equiv-cost costs)
			  :repairs (append (ana~matcher-repairs prob) (list (list 'del-equiv (first args))))
			  :old-prob prob)))
		('implies (list		; if we del an equivalence, try both arguments
			   (ana~create-match-problem
			    :terms (cons (list (first args) target-term) rest-terms)
			    :costs (ana=add-costs ana*del-condition-cost costs)
			    :repairs (append (ana~matcher-repairs prob) (list (list 'del-condition (second args))))
			    :old-prob prob)
			   (ana~create-match-problem
			    :terms (cons (list (second args) target-term) rest-terms)
			    :costs (ana=add-costs ana*del-conclusion-cost costs)
			    :repairs (append (ana~matcher-repairs prob) (list (list 'del-conclusion (first args))))
			    :old-prob prob)))
		('forall (list
			  (ana~create-match-problem
			   :terms (cons (list (data~abstr-range (first args)) target-term) rest-terms)
			   :costs (ana=add-costs ana*del-universal-quantifier-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'del-universal-quantifier (data~abstr-domain
										   (first
										    args)))))
			   :old-prob prob)))
		('exists (list
			  (ana~create-match-problem
			   :terms (cons (list (data~abstr-range (first args)) target-term) rest-terms)
			   :costs (ana=add-costs ana*del-existential-quantifier-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'del-existential-quantifier (data~abstr-domain
										     (first
										      args)))))
			   :old-prob prob)))
		('not    (list
			  (ana~create-match-problem
			   :terms (cons (list (first args) target-term) rest-terms)
			   :costs (ana=add-costs ana*del-not-cost costs)
			   :repairs (append (ana~matcher-repairs prob)
					    (list (list 'del-not)))
			   :old-prob prob)))
		)))
      ('decompose;; same fct oder source aus target
       (let* ((head1 (data~appl-function source-term))
	      (head2 (data~appl-function target-term))
	      (args1 (data~appl-arguments source-term))
	      (args2 (data~appl-arguments target-term)))
	 (if (member (keim~name head2) '(and or equiv not))
	     (append;; as these logical symbols are commutative, we will swap them around
	      (ana=decompose-match-problem head1 head2 args1 args2 rest-terms prob)
	      (ana=decompose-match-problem head1 head2 args1 (reverse args2) rest-terms prob))
	   (if (eql (keim~name head2) 'implies)
	       (append;; swapping the arguments of implies is expensive
		(ana=decompose-match-problem head1 head2 args1 args2 rest-terms prob)
		(ana=decompose-match-problem head1 head2 args1 (reverse args2) rest-terms
					     prob
					     (ana=add-costs
					      ana*swap-implies-cost
					      (ana~matcher-costs prob))
					     ))
	     ;; just decomposing costs nothing
	     (ana=decompose-match-problem head1 head2 args1 args2 rest-terms prob)))))
      ('mgb (ana=create-mgbs source-term target-term rest-terms prob))
      
      ('term-map (unless (or 
			  ;; we don't create term-maps on abstractions
			  ;; needed for lim-seq-3.2.4 on lim-seq-3.2.5
			  (data~abstr-p target-term)
			  (data~equal (beta~normalize source-term) target-term)
			     ;; we don't create term-maps if the source is already bound
			     ;; to the target:
			  (and (data~variable-p source-term)
			       (bind~binding source-term)
			       (or (data~constant-p (bind~binding source-term))
				   (data~variable-p (bind~binding source-term)))
			       (eq (keim~name (bind~binding source-term))
				   (if (or (data~constant-p target-term)
					   (data~variable-p target-term))
				       (keim~name target-term)
				     (if (data~abstr-p target-term)
					 (if (data~appl-p
					      (data~abstr-n-range
					       target-term))
					     (keim~name (data~appl-function
							 (data~abstr-n-range
							  target-term))))
				       ))))
			  (and (data~appl-p source-term)
			       (data~variable-p (data~appl-function source-term))
			       (data~appl-p target-term)
			       (bind~binding (data~appl-function
					      source-term))
			       (data~equal (bind~binding (data~appl-function
							  source-term))
					   (data~appl-function target-term)))
			       
;			       (data~equal (bind~binding source-term) ;; geht nicht da
                                   ;immer aus unterschiedlichen envs!
;					   target-term))
;			     (ana=contains-helpvariable-p source-term)
			  (ana=contains-logical-symbol-p target-term)
			  (ana=contains-logical-symbol-p source-term)
			  )
		   (let ((new (ana~create-match-problem :terms rest-terms
							:term-maps
							(append (list (list source-term target-term))
								(ana~matcher-term-maps prob))
							:costs (ana=add-term-map-costs
								source-term
								target-term
								costs)
							:old-prob prob)))
		     (when new (list new)))))
      
      ))))

(defun ana=decompose-match-problem (head1 head2 args1 args2 rest-terms prob &optional costs)
  (if (data~variable-p head1) ;; if the head of the source is a variable, bind it
      (let ((bound (uni~bind head1 head2 (ana~matcher-dummy prob))))
	(when bound
	  (let ((termlist nil))
	    ;; create new uni-problem
	    (list
	     (ana~create-match-problem
	      :terms (append (dolist (i args1 ;(cons (list head1 head2)
					(nreverse termlist))
			       (push (list i (pop args2)) termlist))
			     rest-terms)
	      :costs costs
	      :old-prob prob)
	     ))))
    (let ((termlist nil))
      ;; create new uni-problem
      (list
       (ana~create-match-problem
	:terms (append (dolist (i args1 (append
					 (unless ;we don't want to match them if they are equal
					     (data~equal head1 head2)
					   (list (list head1 head2)))
					 (nreverse termlist)))
			 (push (list i (pop args2)) termlist))
		       rest-terms)
	:costs costs
	:old-prob prob))
      )))

(defun ana=create-mgbs (source-term target-term rest-terms prob)
  (let* ((flex-head (data~top source-term))
	 (rigid-head (data~top target-term)))
    (sort   ;; the new problems should be sorted in order to merge properly!
     (mapcar #'(lambda (new-term)
		 (ana~create-match-problem
		  :terms (cons (list flex-head new-term)
			       (cons (list source-term target-term)
				     rest-terms))
		  :costs (ana=add-costs (ana~matcher-costs prob)
					(ana=costs flex-head new-term))
		  :old-prob prob
		  ))
	     (gb~construct (term~type flex-head) rigid-head))
     #'ana=costs-<=-p :key #'ana~matcher-costs)
     ))

;====================================================
;  costs
;====================================================

(defun ana=add-costs (x y)
  (map 'list #'+ x y))

(defun ana=costs-<=-p (x y)
  (if (or (not x) (< (car x) (car y))) t
    (unless (> (car x) (car y))
      (ana=costs-<=-p (cdr x) (cdr y)))))

(defun ana=costs-<-p (x y)
  (if (not x) nil
    (if (< (car x) (car y)) t
      (unless (> (car x) (car y))
	(ana=costs-<-p (cdr x) (cdr y))))))

(defun ana=add-term-map-costs (sterm tterm old-costs)
  ;; the term mapping cost is the added term depth, that means
  ;; the smaller the terms, the cheaper the mapping costs
  (let ((sterm-costs (ana=term-costs sterm))
	(tterm-costs (ana=term-costs tterm)))
;  (let ((mcosts (+ (ana=term-costs sterm) (ana=term-costs tterm))))
  ;; we take the maximum term-mapping cost:
;  (declare (ignore tterm))
  (list (first old-costs)
;        (max (ana=term-costs sterm) (second old-costs))
	(+ (* sterm-costs sterm-costs)(* tterm-costs tterm-costs))
        (third old-costs))))


(defun ana=term-costs (term)
  (if (listp term)
      (apply #'+ (mapcar #'ana=term-costs term))
    (1+ (apply #'+ 0 (mapcar #'ana=term-costs (data~substructs term))))))

(defun ana=repair-costs (repair)
  (if (second repair)
      (list (ana=repair-costs-h (second repair)) 0 0)
    '(0 0 0)))

(defun ana=repair-costs-h (term)
  (if (listp term)
      (apply #'+ (mapcar #'ana=repair-costs-h term))
    (if (logic~connective-p term)
	(1+ (apply #'+ 0 (mapcar #'ana=repair-costs-h (data~substructs term))))
      (apply #'+ 0 (mapcar #'ana=repair-costs-h (data~substructs term))))))

(defgeneric ana=costs (flex-head new-term)
  (declare (edited  "21-SEP-1998")
	   (authors Cullrich)
	   (input   "two terms.")
	   (effect  "calculates the matching cost of the two terms.")
	   (value   "the cost (an integer)"))
  (:method ((flex-head term+variable) (new-term term+constant))
	   (format t "~%ja~%") ;; gibt es diesen Fall ueberhaupt?? kann new-term constante sein?
	   '(0 0 0))

  (:method ((flex-head keim::gb+helpvariable) (new-term term+abstr))
	   ;; bevorzuge Projektion, oder Imitation auf einer Konstanten
	   (if (data~appl-p (data~abstr-range new-term))
	       ;; wenn Applikation, dann ueberpruefen ob Projektion:
	       (if (ana=is-projection new-term)
		   '(0 0 0) ;wieso eigentlich Kosten 1? test mit 0
		 ;; sonst ist es eine Imitation
		 '(0 0 3))
	     ;; aber Imitation einer Konstanten ist ok
	     ;; sollte billiger sein als Abb einer Fktvar auf Konst, die keine Fkt ist
	     ;; sollte aber teurer sein als Projektion wegen Bsp.II ??????? was war das??
	     (if (ana=is-projection new-term)
		 ;; um zu unterscheiden zwischen lam x.x und lam x.c!!!
		 '(0 0 0)
	       '(0 0 2))
	     ))

  (:method ((flex-head term+variable) (new-term term+abstr))
	   ;; wenn new-term Fkt ist:
	   (if (data~appl-p (data~abstr-range new-term))
	       ;; wenn Projektion: (noch Holger fragen, ob es eleganter geht!!!!!)
	       (if (ana=is-projection new-term)
		   '(0 0 4) ;; kann das eintreten?
		 ;; sonst Imitation
		 ;; wenn komplette Imitation, dann fast wie Termabbildung, also teurer
		 ;; machen
		 ;(let ((adder 0));(if (ana=complete-imitation-p new-term)
				  ;; funktioniert nicht, da es sich erst nach
				  ;; Instantierung der Hilfsvar ergibt...
				 ; 1
		                 ; 0)))
		 ;; wenn die Funktionen dieselbe Stelligkeit haben
		 (if (= (length (data~abstr-domain (data~annotation flex-head)))
			(length (data~appl-arguments (data~abstr-range new-term))))
		       
		     ;; und auch nicht permutiert wurden
		     ;; 3.3. aber!! die Permutation kann ich hier gar nicht pruefen!, da
		     ;; hier Hilfsvariablen eingefuehrt werden, ueber die dann die
		     ;; Projektion gemacht wird
		     ;; gluecklicherweise sind die vom mgb erzeugten Bindings schon
		     ;; richtig geordnet
;		     (if (ana=abstr-not-permutated new-term)
			 '(0 0 0)
;		       '(0 0 1)
		       
		     ;; else nicht selbe Stelligkeit
;		     (if (ana=abstr-not-permutated new-term)
			 '(0 0 2)
;		       '(0 0 3)
		     ))
	     ;; wenn new-term Konstante ist, dann teuer, da Fkt nicht unbedingt auf
	     ;; Konstanten gemacht werden sollen (Bsp 2)
	     ;; gibts das???
	     '(0 0 3))
	   )
  )

(defun ana=is-projection (term)
  ;; ich test hier, ob der top-term der range in der domain is
  (member (if (data~appl-p (data~abstr-range term))
	      (data~appl-function (data~abstr-range term))
	    (data~abstr-range term))
	  (data~abstr-domain term))
  )

(defun ana=abstr-not-permutated (abstr)
  (let ((lambda-terms (data~abstr-domain abstr))
	(counter 1)
	(result t))
    ;; first number the lambda terms
    (dolist (term lambda-terms (setf counter 1))
      (setf (data~plist term)
	    (acons :ana-val counter (data~plist term)))
      (omega~message "Term ~S, Counter:" term (cdr (assoc :ana-val (data~plist term))))
      (setf counter (1+ counter)))
    ;; then we map the arguments and remember their number. if we get a number lower than
    ;; the last one, we know that there must be a permutated
    (dolist (term
	     (when (data~appl-p (data~abstr-range abstr))
	       (data~appl-arguments (data~abstr-range abstr)))
	     result)
      ;; kann noch optimiert werden
      ; if the term has a corresponding number,
      (when (cdr (assoc :ana-val (data~plist term)))
	(progn
	  (omega~message "Vergleich: term ~A, number ~A, counter ~A"
			 term (cdr (assoc :ana-val (data~plist term))) counter)
	  ; we check if it is lower than counter. if so, than a preceding term had a
	  ; higher number, which means that in the lambda-list it occured before the
	  ; actual term, which means that they are swapped
	  (if (< (cdr (assoc :ana-val (data~plist term))) counter)
	      (setf result nil))
	  (setf counter (cdr (assoc :ana-val (data~plist term)))))))))

(defun ana=complete-imitation-p (abstr)
  (let ((lambda-terms (data~abstr-domain abstr))
	(range (data~abstr-range abstr)))
    (not (some #'(lambda (term)
		   (data~substruct-positions term range))
	       lambda-terms))))

(defgeneric ana=contains-logical-symbol-p (term)
  (:method ((term list))
	   (some #'ana=contains-logical-symbol-p term))
  (:method ((term keim::gb+helpvariable))
	   nil)
  (:method ((term hou+nullvariable))
	   nil)
  (:method ((term term+constant))
	   nil)
  (:method ((term term+variable))
	   nil)
  (:method ((term term+appl))
	   (or (logic~connective-p (data~appl-function term))
	       (dolist (subterm (data~appl-arguments term) nil)
		 (when (ana=contains-logical-symbol-p subterm)
		   (return T)))))
  (:method ((term term+abstr))
	   (ana=contains-logical-symbol-p (data~abstr-range term))))
