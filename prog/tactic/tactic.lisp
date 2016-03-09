;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

(in-package :omega)


(mod~defmod TAC 
            :uses (data infer just keim node omega pds pdsc pdsj pdsn agenda th)
            :documentation "Tactic definitions, interpretation and application"
            :exports (tac+tactic
		      tac~theory
		                            
                      tac~apply
                      tac~change-pds-structure
                      tac~change-reason-structure
                      tac~deftactic
                      tac~find-tactic
                      tac~forget&destroy-hyp
                      tac~p
                      tac~rewrite-outline-pattern
		      tac~set-manage-list
                      
                      tac*tactic-hash-table
                      tac*verbose))


(defclass tac+tactic (keim+name)
  ((inference :accessor tac=inference
	      :initarg :inference
	      :initform nil
	      :documentation "The inference rule the tactic belongs to.")
   (premises :accessor tac=premises
	      :initarg :premises
	      :initform nil
	      :documentation "A list of lines that are the premises of the tactic.")
   (conclusions :accessor tac=conclusions
		:initarg :conclusions
		:initform nil
		:documentation "A list of lines that are the conclusion of the tactic.")
   (hypotheses :accessor tac=hypotheses
	       :initform nil
	       :initarg :hypotheses
	       :documentation "A list of lines that are returned as new hypotheses of the tactics conclusions.")
   (parameters :accessor tac=parameters
	       :initarg :parameters
	       :initform nil
	       :documentation "Additional parameters needed for the tactic.")
   (computations :accessor tac=computations
		 :initform nil
		 :initarg :comps
		 :documentation "A list pairs of line-names and function-names.")
   (sideconds :accessor tac=sideconds
	      :initarg :sideconds
	      :initform nil
	      :documentation "Sideconditions of a tactic (predicates that must be true).") 
   (description :accessor tac=description
		:initarg :desc
		:initform nil
		:documentation "Short description of the tactic.")
   (theory :initarg :theory  
	   :accessor tac~theory
	   :initform nil
	   :documentation "The theory in which the rule is defined.")))


(defvar tac*tactic-hash-table (make-hash-table :test #'equal)
  "A hash table to map tactic names to the tactic objects themselves.")

(eval-when (load compile eval)
(defvar tac*verbose nil "If nil, warnings will be suppressed, otherwise they will be printed.")
)

(defun tac~p (obj)
  (typep obj 'tac+tactic))

(defmacro tac~deftactic (name inference theory &rest attribs)
  (declare (edited  "20-JUN-1997 21:08")
	   (authors SORGE)
	   (input   "A written expression of a tactic."
		    "Example:
 \\begin{code}
 (tac~deftactic ande-f ande (in base)
   (premises L1)
   (conclusions L2 L3)
   (hypotheses )
   (computations (L2 (tac~compute-left-conjunct L1))
                 (L3 (tac~compute-right-conjunct L1)))
   (sideconditions (tac~conjunction-line-p L1))
   (description \"Forwar application of AND-Elimination\"))
 \\end{code}")
	   (effect  "Creates an instance of tac+tactic.")
	   (value   "Returns the new tactic."))
  `(block deftactic
     (let* ((name (quote ,name))
	  (inference (let ((inf (infer~find-method (quote ,inference))))
		       (unless inf (omega~error ";;;TAC~~DEFTACTIC: The inference rule ~A does not exist" (quote ,inference)))
		       inf))
	  (th-ident (if (string-equal (car (quote ,theory)) :in)
		        (cadr (quote ,theory))
                      (omega~error ";;;TAC~~DEFTACTIC: A theory lacks for the definition of ~A" name)))
	  (theory (if (th~require-only th-ident)
		      (th~find-theory th-ident)
		    (omega~error ";;;TAC~~DEFTACTIC: The theory ~A does not exist." th-ident)))
	  (attribs (quote ,attribs))
	  (premises) (conclusions) (hypotheses) (parameters)
	  (sideconds) (desc "") (comps))
     (when (and inference th-ident theory)
       (do ((attribs (cdr attribs) (cdr attribs))
	    (attrib (car attribs) (car attribs)))
	   ((and (null attrib) (null attribs)))
	 (if (consp attrib)
	     (cond 
	      ((string-equal (car attrib) :premises)       (setq premises    (cdr attrib)))
	      ((string-equal (car attrib) :conclusions)    (setq conclusions (cdr attrib)))
	      ((string-equal (car attrib) :hypotheses)     (setq hypotheses  (cdr attrib)))
	      ((string-equal (car attrib) :parameters)     (setq parameters  (cdr attrib)))
	      ((string-equal (car attrib) :sideconditions) (setq sideconds   (cdr attrib)))
	      ((string-equal (car attrib) :computations)   (setq comps       (cdr attrib)))
	      ((string-equal (car attrib) :description)    (setq desc        (cadr attrib)))
	      (t (return-from deftactic (omega~error ";;;TAC~~DEFTACTIC: Not expecting ~A" (car attrib)))))
	   (return-from deftactic (omega~error ";;;TAC~~DEFTACTIC: Not expecting ~A" attrib))))
       (let ((newtactic (make-instance 'tac+tactic
				       :name name
				       :theory theory
				       :inference inference
				       :premises premises
				       :conclusions conclusions
				       :hypotheses hypotheses
				       :parameters parameters
				       :sideconds sideconds
				       :desc desc
				       :comps comps)))
	 (when (member 'tactics th*output)
	   (if (gethash (symbol-name name)  tac*tactic-hash-table)
	       (omega~message ";;; Redefining tactic ~A" name)
	     (omega~message ";;; Defining tactic ~A" name)))
	 (setf (gethash (symbol-name name)  tac*tactic-hash-table)
	       newtactic))))))
  

(defun tac~find-tactic (name)
  (declare (edited  "20-JUN-1997 21:39")
           (authors SOrge)
           (input   "A name of a tactic (symbol or string).")
           (effect  "None.")
           (value   "The tactic object with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string (string-upcase name)))
   tac*tactic-hash-table))

(defun tac~apply (tac-name outline parameters &optional (tac*verbose tac*verbose) &key ((:purpose purpose) :apply))
  (declare (edited  "20-JUN-1997 21:48")
	   (authors SORGE)
	   (input   "A tactic-name, the argument-list, optionally a flag"
		    "indicating whether the warnings should be issued or not"
		    "and a prupose key with possible values:"
		    ":apply, :test, and :expand.")
	   (effect  "Applys the tactic using the arguments. Computes and"
		    "inserts new nodes for purposes :apply and :expand.")
	   (value   "Returns according to the key PURPOSE:"
		    "- :apply, three values: T, a list of nodes forming the"
		    "outline of the tactic, and the list of conclusions."
		    "- :expand, three values: T, a list of nodes forming the"
		    "outline, and the list of conclusions."
		    "- :test, T."
		    "If matching failed, returns nil."))
  (let* ((apply (string-equal purpose :apply))
	 (expand (string-equal purpose :expand))
	 (test (string-equal purpose :test))
	 (tactic (if (or apply test expand)
		     (tac~find-tactic tac-name)
		   (return-from tac~apply (omega~error ";;;TAC~~APPLY: invalid purpose ~A." purpose))))
	 (arg-maplists (if tactic
			   (tac=match-arglist outline parameters tactic)
			 (return-from tac~apply (omega~warn ";;; ~A is not an existing tactic.~%" tac-name))))
	 (arg-mappings (if arg-maplists
			   (apply #'append arg-maplists)
			 (return-from tac~apply)))
	 (conc-mappings (first arg-maplists))
	 (prem-mappings (second arg-maplists))
	 (side-conds (mapcar
		      #'(lambda (x) (tac=extract-function&arguments x arg-mappings))
		      (tac=sideconds tactic)))
	 (side-test (if (notany #'null side-conds)
			(dolist (x side-conds)
			  (unless (apply (first x) (rest x))
			    (return-from tac~apply
			      (omega~warn ";;; Sidecondition ~A cannot be fulfilled with arguments ~A." (car x) (cdr x)))))
		      (return-from tac~apply)))
	 (hyp-list (mapcar #'(lambda (x) (if (listp (car x)) (caar x) (car x))) (tac=hypotheses tactic)))
	 (prem-hyps (tac~set-manage-list #'union
					 (mapcar #'(lambda (x) (pdsn~hyps (rest x))) prem-mappings)))
	 (concl-hyps (tac~set-manage-list #'union
					  (mapcar #'(lambda (x)
						      (unless (or test (pdsn~open-node-p (rest x)))
							(return-from tac~apply
							  (omega~warn ";;; ~A is not an open node.~%" (keim~name (rest x)))))
						      (pdsn~hyps (rest x)))
						  conc-mappings)))
	 (compute-labels (mapcar #'car (tac=computations tactic)))
	 (compute-functions (mapcar #'(lambda (x) (tac=extract-function&arguments (cadr x) arg-mappings))
				    (tac=computations tactic))))
    (declare (ignore side-test))
    (unless (notany #'null compute-functions)
      (return-from tac~apply (omega~warn ";;; New terms cannot be computed while applying Tactic ~A.~%" tac-name)))
    (unless (tac=test-conclusions-premises conc-mappings prem-mappings)
      (return-from tac~apply (omega~warn ";;; Tactic ~A cannot justify a node by its own derivative.~%" tac-name)))
    (unless (tac=test-hyps-existent-conclusions conc-mappings prem-hyps)
      (return-from tac~apply (omega~warn ";;; Tactic ~A cannot be applied to the given conclusions.~%" tac-name)))
    ;;; from now on NOTHING should go wrong anymore when verifying!!!
    (when test (return-from tac~apply t))
    ;;; the last three return-from's are not bad style.... but a necessity     
    ;;; as this code was implemented on a notebook with a linelength of
    ;;; 80 Characters and the rest of this function was very hard to read
    ;;; with all the intensions etc....    (June 28th 1997 VS)
    (let* ((formula-list (pairlis compute-labels
				  (mapcar #'(lambda (x) (apply (first x) (rest x))) compute-functions)))
	   (dummy (when (some #'(lambda (x) (term~free-type-variables (cdr x))) formula-list)
		    (return-from tac~apply
		      (omega~warn ";;; Some schematic terms could not be grounded while applying Tactic ~A!~%" tac-name))))
	   (hyp-nodes (mapcar #'(lambda (x) 
				  (let ((new-node
					 (pdsn~create (pds~new-node-name) nil
						      (cdr (assoc x formula-list))
						      (pdsj~closed-just-create (infer~find-method "HYP")
									       nil nil "grounded"))))
				    (setf (pdsn~hyps new-node) (list new-node))
				    (pds~only-insert-node! new-node)
				    (when tac*verbose (omega~message (pds~node2string new-node)))
				    (cons x new-node)))
			      hyp-list))
	   (prem-nodes (mapcar #'(lambda (x)
				   (let ((old-prem (assoc x prem-mappings)))
				     (if old-prem old-prem
				       (let ((new-node
					      (pdsn~create (pds~new-node-name)
							   concl-hyps
							   (cdr (assoc x formula-list))
							   (pdsj~open-just-create))))
					 (if apply
					     (pds~insert-node! new-node)
					   (pds~only-insert-node! new-node))
					 (when tac*verbose (omega~message (pds~node2string new-node)))
					 (cons x new-node)))))
			       (tac=prepare-line-list (tac=premises tactic))))
	   (conc-nodes (mapcar #'(lambda (x)
				   (let ((old-conc (assoc x conc-mappings)))
				     (if old-conc
					 (let* ((old-concl (cdr old-conc))
						(old-just (node~justification old-concl))
						(old-reasons (pdsj~reasons old-just))
						(old-sponsors (pdsj~sponsors old-just))
						(old-unsponsors (pdsj~unsponsors old-just))
						(new-control (pdsc~create old-reasons old-sponsors old-unsponsors)))
					   (when (pdsn~open-node-p old-concl)
					     (setf (node~justification old-concl)
						   (pdsj~replace-justification! (node~justification old-concl)
										(pdsj~closed-just-create
										 (tac=inference tactic)
										 (mapcar #'cdr prem-nodes)
										 parameters)))
					     (setf (pdsj~control (node~justification old-concl)) new-control)
					     (when tac*verbose (omega~message (pds~node2string old-concl))))
					   (cons x old-concl))
				       (let ((new-node
					      (pdsn~create (pds~new-node-name)
							   prem-hyps
							   (cdr (assoc x formula-list))
							   (pdsj~closed-just-create
							    (tac=inference tactic)
							    (mapcar #'cdr prem-nodes)
							    parameters))))
					 (pds~only-insert-node! new-node)
					 (when tac*verbose (omega~message (pds~node2string new-node)))
					 (cons x new-node))
				     )))
			       (tac=prepare-line-list (tac=conclusions tactic)))))
      (declare (ignore dummy))
      (mapc #'(lambda (hypmap nodecons)
		(when (string-equal (car hypmap) (car nodecons))
		  (mapc #'(lambda (x)
			    (let ((insnode (cdr (or (assoc x prem-nodes)
						    (assoc x conc-nodes)))))
			      (setf (pdsn~hyps insnode) (cons (cdr nodecons) (pdsn~hyps insnode)))))
			(cdr hypmap))))
	    (mapcar #'(lambda (x) (if (listp (car x)) (car x) x)) (tac=hypotheses tactic))
	    hyp-nodes)
      (let ((real-concs (mapcar #'cdr conc-nodes))
	    (real-prems (mapcar #'cdr prem-nodes))
	    (real-hyps (mapcar #'cdr hyp-nodes)))
	(when tac*verbose
	  (omega~trace "Conclusions: ~A  Premises: ~A  Hypotheses:  ~A" conc-nodes prem-nodes hyp-nodes))
	(unless expand (tac~change-pds-structure real-concs real-prems real-hyps))
	(values t (append real-concs real-prems real-hyps) real-concs)
      ))))
  
(defun tac=test-conclusions-premises (conc-mappings prem-mappings)
  (declare (edited  "02-JUL-1997")
	   (authors Sorge)
	   (input   "A tactic and association list for conclusions and premises.")
	   (effect  "None.")
	   (value   "Nil if one of the premises is deduced from one of the conclusions. Otherwise T."))
  (let* ((prems (mapcar #'cdr prem-mappings))
	 (concs (mapcar #'cdr conc-mappings))
	 (all-premises (pdsn~justifying-nodes prems)))
    (notany #'(lambda (conc) (find conc all-premises)) concs)))

(defun tac=test-hyps-existent-conclusions (conc-mappings prem-hyps)
  (declare (edited  "21-JUN-1997")
	   (authors Sorge)
	   (input   "A tactic, conclusion-mappings for that tactic and the union of all"
		    "the hypotheses of the given premises.")
	   (value   "T if all the existent conclusions have PREM-HYPS as subsets of their"
		    "hypotheses-sets. Otherwise NIL."))
  (let ((concl (mapcar #'cdr conc-mappings)))
    (every #'(lambda (x) (subsetp prem-hyps (pdsn~hyps x))) concl)))

(defun tac=match-arglist (outline parameters tactic)
  (declare (edited  "21-JUN-1997")
	   (authors Sorge)
	   (input   "An outline, a list of parameters and a tactic.")
	   (effect  "None.")
	   (value   "A list of association list relating a meta-argument to the real expression."
		    "Nil if something goes wrong."))
  (let ((concs (tac=prepare-line-list (tac=conclusions tactic)))
	(prems (tac=prepare-line-list (tac=premises tactic)))
	(params (tac=parameters tactic)))
    (if (= (length outline) (+ (length prems) (length concs)))
	(if (= (length parameters) (length params))
	    (list
	     (mapcan #'(lambda (metasymbol line)        ;;; matching the conclines and checking their types.
			 (when line
			   (if (pdsn~p line)
			       (list (cons metasymbol line))
			     (return-from tac=match-arglist
			       (omega~warn ";;; ~A should be of type pdsn+node~%" line)))))
		     concs outline)
	     (mapcan #'(lambda (metasymbol line)        ;;; matching the conclines and checking their types.
			 (when line
			   (if (pdsn~p line)
			       (list (cons metasymbol line))
			     (return-from tac=match-arglist
			       (omega~warn ";;; ~A should be of type pdsn+node~%" line)))))
		     prems (subseq outline (length concs)))
	     (mapcan #'(lambda (metaparam parameter)        ;;; matching the parameters and checking their types.
			 (if (typep parameter (second metaparam))
			     (list (cons (first metaparam) parameter))
			   (return-from tac=match-arglist
			     (omega~warn ";;; ~A should be of type ~A.~%" parameter (second metaparam)))))
		     params parameters))
	  (omega~error ";;; Not the right number of parameters specified for tactic ~A.~%" (keim~name tactic)))
      (omega~error ";;; Length of outline is not correct for tactic ~A.~%" (keim~name tactic)))))



(defun tac=extract-function&arguments (meta-function arg-mappings)
  (declare (edited  "21-JUN-1997")
	   (authors Sorge)
	   (input   "A cons describing a function and arguments and some argument-mappings.")
	   (value   "An evaluable expression of the real function and the real arguments."))
  (let* ((meta-func (car meta-function))
	 (meta-args (cdr meta-function))
	 (real-func (symbol-function meta-func)))
    (if real-func
	(cons meta-func
	      (mapcar #'(lambda (x)
			  (cond ((atom x) (tac=symbol2argument x arg-mappings))
				((string-equal (car x) :formula)
				 (node~formula (tac=symbol2argument (cadr x) arg-mappings)))
				((string-equal (car x) :justification)
				 (node~justification (tac=symbol2argument (cadr x) arg-mappings)))
				((string-equal (car x) :hyps)
				 (pdsn~hyps (tac=symbol2argument (cadr x) arg-mappings)))
				(t (return-from tac=extract-function&arguments
				     (omega~error ";;; Unknown keyword ~A.~%" (car x))))))
		      meta-args))
      (return-from tac=extract-function&arguments (omega~error ";;; ~A is not an existing function.~%" meta-func)))))
	
			  
(defun tac=symbol2argument (symbol mappings)
  (declare (edited  "21-JUN-1997")
	   (authors Sorge)
	   (input   "A symbol and some argument-mappings.")
	   (value   "The corresponding argument for symbol."))
  (let ((arg (assoc symbol mappings :test #'string-equal)))
    (unless arg (omega~warn ";;; Cannot map argument named ~A.~%" symbol))
    (rest arg)))
	       
(defun tac=prepare-line-list (line-list)
  (mapcar #'(lambda (x) (if (listp x) (car x) x)) line-list))

(defmethod infer~compute-outline ((tactic infer+tactic) (outline list) (parameters list))
  (let* ((outline-pattern (infer~compute-outline-pattern outline))
	 (application (infer~outline-pattern2application tactic outline-pattern)))
    (if application
	(multiple-value-bind
	    (success new-outline conclusions)
	    (tac~apply application outline parameters tac*verbose :purpose :apply)
	  (if (and (not success)
		   (not (omega~warn ";;; Something went wrong while applying the outline-function to ~A" outline)))
	      (values outline nil)
	    (let* ((hyp-list (subseq new-outline (length outline)))  ;;; here we deal with multiple conclusions
		   (prem-list (subseq 
			       (subseq new-outline
				       0
				       (- (length new-outline)
					  (length hyp-list)))
			       (length conclusions))))
	      (tac~rewrite-outline-pattern outline-pattern conclusions)
	      (tac~change-reason-structure conclusions prem-list hyp-list)
	      (values new-outline t))))
      (warn ";;; Cannot compute outline for pattern ~A" outline-pattern)
      )))  ;;; some of the stuff might need to be adjusted.... VS


(defun tac~rewrite-outline-pattern (pattern nodes)
  (declare (edited  "19-JUN-1997 19:00")
	   (authors SORGE)
	   (input   "An original OUTLINE-PATTERN and a list of NODES.")
	   (effect  "The outline-pattern of the nodes are set to a pattern that"
		    "includes references to conclusions that are sibling of each node.")
	   (value   "The updated NODES."))
  (labels ((rewrite-pattern (number)
			    (let* ((node-names (mapcar #'keim~name nodes)))
			      (setf (nth number node-names) (nth number pattern))
			      (append node-names (subseq pattern (length node-names))))))
    (do ((rest-nodes nodes (cdr rest-nodes))
	 (n 0 (1+ n)))
	((= n (length nodes)) nil)
      (setf (pdsj~outline-pattern (node~justification (car rest-nodes)))
	    (rewrite-pattern n))))
  nodes)

				      
(defun tac=get-all-premises (node-list)
  (declare (edited  "19-JUN-1997 18:39")
	   (authors SORGE)
	   (input   "A list of nodes.")
	   (value   "A list containing all the premises of the nodes."))
  (when node-list
    (append (node~just-premises (car node-list))
	    (tac=get-all-premises (cdr node-list)))))


(defgeneric tac~forget&destroy-hyp (nodes old-hyp new-hyp &key (test 'data~equal))
  (declare (edited  "06-MAR-2000" "02-JUL-1997")
	   (authors Sorge Sorge Chris Pollet)
	   (input   "A list of nodes, an old hypothesis and a new (identical) hypothesis.")
	   (effect  "Deletes the new hypothesis from the PDS, replaces all occurences of"
		    "it in the hyp-lists of the given nodes with the old  hypothesis.")
	   (value   "The changed node-list."))
  (:method ((nodes list) old-hyp new-hyp &key (test 'data~equal))
	   (if (funcall test (node~formula old-hyp) (node~formula new-hyp))
	       (let ((pds omega*current-proof-plan))
		 (mapcar #'(lambda (node)
			     (when (find new-hyp (pdsn~hyps node))
			       (setf (pdsn~hyps node)
				     (cons old-hyp (remove new-hyp (pdsn~hyps node))))))
			 nodes)
		 (pds~delete-node! new-hyp nil pds))
	     (omega~error ";;; Something really stupid happened when exchanging hypotheses....")))
  (:method ((node pdsn+node) old-hyp new-hyp &key (test 'data~equal))
	   (if (funcall test (node~formula old-hyp) (node~formula new-hyp))
	       (let ((pds omega*current-proof-plan))
		 (when (find new-hyp (pdsn~hyps node))
		   (setf (pdsn~hyps node)
			 (cons old-hyp (remove new-hyp (pdsn~hyps node)))))
		 (pds~delete-node! new-hyp nil pds))
	     (omega~error ";;; Something really stupid happened when exchaning a hypothesis...."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The nodes is result of a backward tactic. That means we have to
;; change things in the nodes which are logical decendents of it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tac~change-pds-structure (concl prem hyps)
  (declare (edited  "05-AUG-1999" "20-JUN-1997 18:02")
	   (authors Sorge)
	   (input   "Lists of conclusions, premises and hypotheses of a tactic.")
	   (effect  "Changes the support structure in the PDS for newly introduced nodes.")
	   (value   "Undefined."))
  (let* ((pds omega*current-proof-plan)
	 (old-concs (remove-if-not #'(lambda (x) (find x (pds~open-nodes pds))) concl))
	 #+weg(old-prems (if old-concs
			(tac~set-manage-list #'intersection
					     (cons prem
						   (mapcar #'(lambda (node)
							       (pds~node-supports node pds))
							   old-concs)))
		      (remove-if-not #'(lambda (x) (find x (pds~support-nodes pds))) prem))) ;;; not completely right ... consider local support lines	 
	 (old-supps (tac~set-manage-list #'union (cons (pds~support-nodes pds)
						       (mapcar #'pds~node-supports (pds~open-nodes pds)))))
	 (old-prems (remove-if-not #'(lambda (x) (find x old-supps)) prem))
	 (new-prems (set-difference prem old-prems))
	 (new-supps (set-difference concl old-concs)))  ;;; quickly debugged!!!
    ;; old-concs should still be supports (or not) of the open-node
    ;; therefore they do not have to be explicitely added!!!
    (when new-prems
      (multiple-value-bind (prems-sponsors prems-unsponsors)
	  (rule=sponsors-unsponsors (remove-if-not #'(lambda (open)
						       (subsetp old-prems
								(pds~node-supports open omega*current-proof-plan)))
						   (pds~open-nodes omega*current-proof-plan)))
	  
	(dolist (subgoal new-prems)
	  (let ((subgoal-just (node~justification subgoal)))
	    (setf (pdsj~sponsors subgoal-just) prems-sponsors)
	    (setf (pdsj~unsponsors subgoal-just) prems-unsponsors)))))
    (when (and old-prems (subsetp old-prems (pds~support-nodes pds)))
      (setf (pds~support-nodes pds) (union new-supps (pds~support-nodes pds))))
    (setf (pds~open-nodes pds) (set-difference (pds~open-nodes pds) concl))
    (dolist (onode (pds~open-nodes pds))
      (let ((tac-supps (tac~set-manage-list #'union (mapcar #'pds~node-supports (append old-concs old-prems))))
	    (o-supps (pds~node-supports onode pds)))
	(when (and old-prems
		   (subsetp old-prems o-supps)
		   (not (find onode new-prems)))
	  (pds~add-sponsors onode (append new-prems new-supps) pds))
	(when (subsetp prem (pds~node-supports onode pds))
	  (pds~add-sponsors onode new-supps pds))
	(when (and old-prems (find onode new-prems))
	  (pds~delete-sponsors onode new-supps pds))
	(when (and o-supps
		   (not (find onode new-prems))
		   (subsetp tac-supps o-supps)
		   (subsetp old-concs o-supps))
	  (pds~add-sponsors onode new-prems pds))
	(when (find onode new-prems)
	  (pds~delete-sponsors onode concl))
      ))
    (let ((old-sponsors (tac~set-manage-list #'union (mapcar #'pdsn~just-sponsors old-concs)))
	  (old-unsponsors (tac~set-manage-list #'intersection (mapcar #'pdsn~just-unsponsors old-concs))))
      (dolist (pnode prem)
	(when (and (pdsn~open-node-p pnode)
		   (not (find pnode old-prems)))
	  (pds~add-sponsors pnode old-sponsors)
	  (pds~delete-sponsors pnode old-unsponsors)
	  (dolist (hnode hyps)
	    (when (find hnode (pdsn~hyps pnode))
	      (pds~add-sponsors pnode (list hnode))
	      (dolist (onode (pds~open-nodes pds))
		(when (find pnode (pds~node-supports onode pds))
		  (pds~add-sponsors onode (list hnode)))))))))))

#+old(defun tac~change-pds-structure (concl prem hyps)
  (declare (edited  "20-JUN-1997 18:02")
	   (authors SORGE)
	   (input   "Lists of conclusions, premises and hypotheses of a tactic.")
	   (effect  "Changes the support structure in the PDS for newly introduced nodes.")
	   (value   "Undefined."))
  (let ((pds omega*current-proof-plan)
	(old-concs (remove-if #'(lambda (x)
				  (and (null (pdsn~just-sponsors x))
				       (null (pdsn~just-unsponsors x))))
			      concl)))
    (when (subsetp prem (pds~support-nodes pds))
      (setf (pds~support-nodes pds) (union concl (pds~support-nodes pds))))
    (setf (pds~open-nodes pds) (set-difference (pds~open-nodes pds) concl))
    (dolist (onode (pds~open-nodes pds))
      (when (subsetp prem (pds~node-supports onode))
	(pds~add-sponsors onode concl pds)))
    (let ((old-sponsors (tac~set-manage-list #'union (mapcar #'pdsn~just-sponsors old-concs)))
	  (old-unsponsors (tac~set-manage-list #'intersection (mapcar #'pdsn~just-unsponsors old-concs))))
      (dolist (pnode prem)
	(when (pdsn~open-node-p pnode)
	  (pds~delete-sponsors pnode old-unsponsors)
	  (pds~add-sponsors pnode old-sponsors)
	  (dolist (hnode hyps)
	    (when (find hnode (pdsn~hyps pnode))
	      (pds~add-sponsors pnode (list hnode)))))))))
      
(defun tac~set-manage-list (function list)
  (cond ((not (consp list)) list)
	((< (length list) 2) (car list))
	((= (length list) 2)
	 (funcall function (car list) (cadr list)))
	(t (funcall function (car list) (tac~set-manage-list function (cdr list))))))


(defun tac~change-reason-structure (conclusions premises &optional hyps)
  (declare (edited  "28-JAN-1998")
	   (authors Sorge)
	   (input   "A list of conclusions and a list of premises.")
	   (effect  "Changes the reasons in the premises and conclusion nodes.")
	   (value   "Undefined."))
  (let ((reasons (mapcar #'pds~change-last-plan-step! conclusions)))
    (mapc #'(lambda (node reason)
	      (pdsn~insert-reason! node reason)
	      (mapc #'(lambda (premise)
			(pdsn~insert-reason! premise reason))
		    (append premises hyps)))
	  conclusions
	  reasons)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some stuff for Black-Box Tactics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod infer~compute-outline ((tactic infer+black-box) (outline list) (parameters list))
  (let* ((outline-pattern (infer~compute-outline-pattern outline))
	 (new-outline (apply (infer~outline-function tactic) (list outline parameters))))
    (if (and (null new-outline)
	     (not (warn ";;; Something went wrong while applying the outline-function to ~A" outline)))
	(values outline nil)
      (let* ((conc (car new-outline))
	     (hyps (subseq new-outline (length outline)))
	     (prems (cdr (subseq new-outline 0 (- (length new-outline) (length hyps))))))
	(setf (pdsj~outline-pattern (node~justification conc)) outline-pattern)
	(tac~change-pds-structure (list conc) prems hyps)
	(tac~change-reason-structure (list conc) prems hyps)
	(values new-outline t)))))

;;; :around method for changing the agenda: 
(defmethod infer~compute-outline :around ((tactic infer+inference) (outline list) (parameters list))
  (let ((open-nodes (mapcan #'(lambda (x) (when (and (pdsn~p x)
                                                     (pdsn~open-node-p x))
                                            (list x)))
                            outline))
        (closed-nodes (mapcan #'(lambda (x) (when (and (pdsn~p x)
                                                       (not (pdsn~open-node-p x)))
                                              (list x)))
                              outline)))
    (multiple-value-bind (new-outline success)
        (call-next-method)
      (when (and success (foci~in-use))
	;;(foci~update-pcs open-nodes (remove-if-not #'pdsn~open-node-p new-outline)))
	(foci~compute-pcs)
	(when pds*current-proof-plan (setf (pds~proof-context pds*current-proof-plan) foci*pcs))
	(when (foci~active-pc) (sugg~reset)))
      (when (and success (agenda~in-use) (not (infer~method-p tactic)))
	;;; By planning method application the agenda is updated while carrying out the application:
        (let* ((new-opens (remove-if-not #'pdsn~open-node-p
					 (set-difference new-outline outline)))
	       (no-more-opens (remove-if #'pdsn~open-node-p open-nodes))
	       (new-agenda (pds~agenda omega*current-proof-plan))
	       (carried-tasks (agenda~get-tasks new-agenda #'(lambda (tk)
							       (find (agenda~task-node tk) no-more-opens)))))
	  (dolist (task (rest carried-tasks))
	    (setq new-agenda (agenda~replace-task task nil nil nil new-agenda)))
	  (if (first carried-tasks)
	      ;;; at least one open node was closed:
	      (setf (pds~agenda omega*current-proof-plan)
		    (agenda~replace-task (first carried-tasks) nil
					 (mapcar #'agenda~create-goal new-opens)
					 nil new-agenda))
	    (when new-opens
	      ;;; Only open nodes are generated, insert the associated tasks:
	      (setf (agenda~next-tasks new-agenda) (union (mapcar #'agenda~create-goal new-opens)
							  (agenda~next-tasks new-agenda)))
	      (setf (pds~agenda omega*current-proof-plan) new-agenda)))))
      (values new-outline success))))



;;; inserted by Afiedler
(defmethod infer~theory ((tactic infer+tactic))
  (tac~theory (tac~find-tactic (infer~find-arbitrary-application-name tactic))))
