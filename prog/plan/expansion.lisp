;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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


;; Expansion of Methods   ---  VS started Thursday April 2nd 1998  15:40

(in-package "OMEGA")

;;; brief disturbance by Chris and Lassaad ... problems with rule~apply

;;LC: A similar function must be given in the tactic system
(defun exp~actual-outline-pattern (node &optional (pds omega*current-proof-plan)
					 (just (node~justification node)))
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "A node, a PDS, and the node current justification.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and maybe other nodes, a list of"
		    "the nodes that are justified using this method."))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (prems (just~premises just))
	 (concs-l (- (length outln-pat) (length prems))))
    (if (> concs-l 1)
	;;More than one conclusion:
	(exp=adapt-outline-pattern outln-pat node (- concs-l 1) pds)
      (values outln-pat (list node)))
    ))

;;LC: A similar function must be given in the tactic system
(defun exp=adapt-outline-pattern (pattern conc concs-l pds)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "An outline-pattern, a node, a positive integer, and a PDS:"
		    "CONC is justified by the same method application together"
		    "with CONCS-L other nodes in PDS.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (if (zerop concs-l)
      (if conc
	  (values pattern (list conc))
	(values pattern nil))
    (let ((first-pat (first pattern)))
      (if (or (infer~existent-pattern-p first-pat)
	      (infer~nonexistent-pattern-p first-pat))
	  ;;;FIRST-PAT is the outline of CONC:
	  (multiple-value-bind (patt concs)
	      (exp=adapt-outline-pattern (rest pattern) nil concs-l pds)
	    (values (cons first-pat patt)
		    (cons conc concs)))
	;;;FIRST-PAT is a label of another conclusion:
	(multiple-value-bind (patt concs)
	    (exp=adapt-outline-pattern (rest pattern) conc (- concs-l 1) pds)
	  (let* ((node (pds~label2node first-pat pds))
		 (node-just (node~justification node))
		 (node-pat (pdsj~conclusion-outline node-just)))
	    (values (cons node-pat patt)
		    (cons node concs))))))
    ))


(defun exp~expand-method&insert-nodes (node &optional (pds omega*current-proof-plan))
  (let ((new-nodes (exp~expand-method node pds)))
    (when new-nodes
      (mapc #'(lambda (node)
		(unless (pds~label2node (keim~name node) pds)
		  (pds~insert-node! node pds)
		  (when (pdsn~open-node-p node)
		    (setf (pds~open-nodes pds)
			  (union (list node) (pds~open-nodes pds))))))
	    new-nodes))))


(defun exp~expand-method (node &optional (pds omega*current-proof-plan))
  (declare (edited  "28-MAY-1998" "02-APR-1998")
	   (authors Jzimmer Sorge)
	   (input   "A node that has a method as justification and a PDS.")
	   (effect  "Expands the node by changing its justfication and maybe inserting"
		    "new nodes into the PDS.")
	   (value   "A list of nodes that have been newly introduced in the PDS."))
  (let* ((just (node~justification node)))
    (multiple-value-bind (outline-pattern concs)
	(exp~actual-outline-pattern node pds just)
      (let* ((real-method (pds~inference-application (just~method just) outline-pattern))
	     (outline (append concs (just~premises just)))
	     (parameters (pdsj~parameters just))
	     (mapp (pdsj~subst just))
	     )
	(when real-method
	  (let ((exp-func (meth~expansion-function real-method))
		(env (meth~environment real-method))
		)
	    (if exp-func
		(meth~execute-expansion-function (first exp-func)
						 (meth~mapp-mapp mapp)
						 (meth~mapp-subst mapp)
						 env)
	      (progn (exp~init real-method outline parameters mapp pds concs)
		     (exp~computations)
		     (exp~end)))))))))


;;;; a longer interference by Chris, Andreas, Holger, Lassaad... problem with term~alpha-match



(eval-when (load compile eval)
  (let ((conclusions nil)
	(premises nil)
	(mapping nil)
	(method nil)
	(pds nil)
	(real-conclusions nil)
	(real-premises nil)
	(decl-cont nil)
	(new-nodes nil)
	(success t))

    (defun exp~init (meth outline parameters &optional (meth-map (meth~mapping-create
								    (subst~create nil nil)
								    (mapp~create nil nil)))
			    (proof-plan omega*current-proof-plan)
			    real-concs)
      (declare (edited  "03-APR-1998")
	       (authors Sorge)
	       (input   "A METHOD of type METH+METHOD, a list of nodes and a list of PARAMETERS."
			"Optionally a mapping for the method and a PDS.")
	       (effect  "Initializes the necessary expansion variables.")
	       (value   "Undefined."))
      (setf method meth)
      (setf real-conclusions real-concs)
      (setf real-premises (set-difference outline real-concs))
      (setf decl-cont (meth~declarative-content method))
      (setf conclusions (meth~conclusions method))
      (setf premises (meth~premises method))
      (setf mapping meth-map)
      (setf new-nodes nil)
      (meth~carry-out-computations (meth~expansion-computations method) meth-map)
      (setf pds proof-plan)
      (omega~trace "~%Premises: ~{~%~A~}~%Conclusions: ~{~%~A~}~%Declarative-Content: ~{~%~A~}" premises conclusions decl-cont)
      ;;;(omega~trace "~%Metavariable Substitution:~%~A" meth-map)
      (tacl~init outline)
      (omega~message "Expansion of Method ~A with outline ~{~A ~}." method outline)
      )


    (defun exp=insert-premises-recursive (just)
      (let* ((prems (just~premises just))
	     (hyps)
	     (rem-prem (remove-if #'(lambda (x)
				      (let ((y (find x real-premises)))
					(when y
					  (setf hyps (append (pdsn~hyps y) hyps)))
					y))
				  prems)))
	(mapc #'(lambda (x)
		  (unless (find x new-nodes :test #'keim~equal)
		    (when (infer~dummy-p (just~method (node~justification x)))
		      (setf (pdsn~hyps x) x))
		    (push x new-nodes)
		    (setf hyps (remove-duplicates (append hyps (exp=insert-premises-recursive (node~justification x)))))
		    (setf (pdsn~hyps x) hyps)))
	      rem-prem)
	hyps))
    
    (defun exp~expansion-step (conc-line)
      (declare (edited  "23-APR-1998")
	       (authors Sorge)
	       (input   "A single conclusion-line.")
	       (effect  "Inserts some new nodes.")
	       (value   "The newly inserted nodes."))
      (let ((node (meth~object-instance conc-line mapping)))
	(if node
	    (let ((just (meth~object-instance (node~justification conc-line) mapping)))
	      (cond (just
		     (omega~trace "~%Expanding node ~A with justification ~A" node just)
		     (exp=insert-premises-recursive just)
		     (exp=insert-just node just))
		    ((listp (just~premises (node~justification conc-line)))
		     (let ((prems (exp=object-instance-list (just~premises (node~justification conc-line)) mapping)))
		       (omega~trace "~%Expanding node ~A with justification ~A and premises:" node (node~justification node))
		       (omega~trace "~%~{~A~}" prems )
		       
		     ))
		    (t (omega~warn "Dont know how to expand an empty justification..."))))
	  (omega~error "Method is underspecified or some computations are missing.~% Node ~A cannot be expanded!" conc-line))))
    
    
    (defun exp~computations ()
      (declare (edited  "03-APR-1998")
	       (authors Sorge)
	       (input   "None.")
	       (effect  "Changes some justifications and might insert some new nodes into the PDS.")
	       (value   "Undefined."))
      (loop
       (when (or (null conclusions) (not success)) (return))
       (let ((conc (car conclusions)))
	 (setf success (exp~expansion-step conc)))
       (setf conclusions (cdr conclusions))
       (omega~trace "New Conclusions: ~{~%~A~}" conclusions)
       ))

    
    (defun exp~end ()            ;;;;; outline-pattern setzen!!!!!!!
      (if success
	  (progn
	    (omega~message "Method ~A successfully expanded!" method)
	    (tacl~end)
	    new-nodes)
	(progn
	  (omega~warn "Expansion of method ~A was not successful!!!" method)
	  (tacl~end :force t)
	  (setf success t)
	  new-nodes)))

    ))

(defgeneric exp~meth-node2node (meth-node &optional (pds omega*current-proof-plan))
  (declare (edited  "23-APR-1998")
	   (authors Sorge)
	   (input   "A method-node and optionally a proof-plan.")
	   (effect  "None.")
	   (value   "The real node in the PDS, corresponding to METH-NODE."))
  (:method ((meth-node meth+node) &optional (pds omega*current-proof-plan))
	   (exp~meth-node2node (keim~name meth-node) pds))
  (:method ((meth-node symbol) &optional (pds omega*current-proof-plan))
	   (pds~label2node meth-node pds))
  (:method ((meth-node string) &optional (pds omega*current-proof-plan))
	   (exp~meth-node2node meth-node pds))
  (:method ((meth-node term+syn-sorted-var) &optional (pds omega*current-proof-plan))
	   (when (keim~equal (ssterm~sort meth-node) (ssterm~get-ssort 'prln))
	     (exp~meth-node2node (keim~name meth-node) pds))))
  

(defun exp=object-instance-list (objects mmapp)
  (when objects
    (meth~object-instance objects mmapp)))
	     
(defgeneric exp=insert-just (line &optional just)
  (declare (edited  "23-APR-1998")
	   (authors Sorge)
	   (input   "A line or set of lines and optionally a justification.")
	   (effect  "Inserts the justification as the new actual justification of the node.")
	   (value   "The changed nodes."))
  (:method ((line pdsn+node) &optional (just (node~justification line)))
	   (pdsj~replace-justification! (node~justification line) just)
	   (setf (node~justification line) just)
	   line)
  (:method ((lines list) &optional just)
	   (if (consp just)
	       (let ((full-justs (if (< (length just) (length lines))
				     (append just (make-list (- (length lines) (length just))) :initial-element nil)
				   just)))
		 (mapcar #'exp=insert-just lines just))
	     (mapcar #'(lambda (x) (exp=insert-just x just)) lines))))

(defmethod infer~apply-expansion-function ((inference infer+method) (outlines list) parameters)
  (declare (ignore parameters))
  (exp~expand-method&insert-nodes (first outlines) omega*current-proof-plan))
  


;;
;;      (if (null red-decl-cont)       ;;; this case is for efficiency
;;          (let* ((full-concs (exp=object-instance-list concs meth-map))
;;                 (new-justs (mapcar #'(lambda (x)
;;                                        (meth~object-instance
;;                                         (node~justification x)
;;                                         meth-map))
;;                                    concs)))
;;            (exp=insert-just full-concs new-justs))
;;        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Expansion Stuff must be actually adapted with the old Stuff (above).
;;; REMARK: Method expansion as a planning step is done by the Stuff below,
;;;    whereas method expansion by a user command is done by the Stuff above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LC: Extension: The expansion of methods may include condition evaluation
;; and therefore new constraints may be generated.
(defun pexp~expand-method (node &optional (pds omega*current-proof-plan))
  (declare (edited  "14-APR-1999" "28-MAY-1998" "02-APR-1998")
	   (authors Lassaad Jzimmer Sorge)
	   (input   "A node that has a method as justification and a PDS.")
	   (effect  "Expands the node by changing its justfication and maybe inserting"
		    "new nodes into the PDS.")
	   (value   "A tuple:"
		    " - a list of nodes that have been newly introduced in the PDS,"
		    " - whether the method expansion was successful,"
		    " - and a possibly new constraint pool, when the method expansion"
		    "affects the PDS constraint pool."))
  (let ((just (node~justification node)))
    (multiple-value-bind (outline-pattern concs)
	(exp~actual-outline-pattern node pds just)
      (let* ((real-method (pds~inference-application (just~method just) outline-pattern))
	     (outline (append concs (just~premises just)))
	     (parameters (pdsj~parameters just))
	     (mapp (pdsj~subst just))
	     new-mapp
	     )
	;; Copying MAPP, since the expansion may fail and we have to delay it after changing
	;; the constraint state. For the next expansion we need the old MAPP unchanged!
	(cond ((and (pds~constraint-pool pds)
		    (intersection (pds~cstrpool-plansteps (pds~constraint-pool pds))
				  (pdsj~successors just)))
	       ;; The constraint state is changed after creating JUST, we have to apply the
	       ;; new variable bindings to MAPP
	       (setq new-mapp
		     (meth~mapping-create
		      (meth~subst-create (subst~domain (meth~mapp-subst mapp))
					 (meth~subst-apply (pds~cstrpool-bindings (pds~constraint-pool pds))
							   (subst~codomain (meth~mapp-subst mapp))))
		      (meth~mapping-copy (meth~mapp-mapp mapp)))))
	      (T
	       (setq new-mapp (meth~mapping-copy mapp))))
	       
	(when real-method
	  (let ((exp-func (meth~expansion-function real-method))
		(env (meth~environment real-method))
		)
	    (if exp-func
		(meth~execute-expansion-function (first exp-func)
						 (meth~mapp-mapp new-mapp)
						 (meth~mapp-subst new-mapp)
						 env)
	      (progn (pexp~init real-method outline parameters new-mapp pds concs)
		     (pexp~computations)
		     (pexp~end)))))))))


;;;; a longer interference by Chris, Andreas, Holger, Lassaad... problem with term~alpha-match



(eval-when (load compile eval)
  (let ((conclusions nil)
	(premises nil)
	(mapping nil)
	(method nil)
	(pds nil)
	(real-conclusions nil)
	(real-premises nil)
	(decl-cont nil)
	(new-nodes nil)
	(success t))

    (defun pexp~init (meth outline parameters &optional (meth-map (meth~mapping-create
								   (subst~create nil nil)
								   (mapp~create nil nil)))
			   (proof-plan omega*current-proof-plan)
			   real-concs)
      (declare (edited  "14-APR-1999" "03-APR-1998")
	       (authors Lassaad Sorge)
	       (input   "A METHOD of type METH+METHOD, a list of nodes and a list of PARAMETERS."
			"Optionally a mapping for the method and a PDS.")
	       (effect  "Initializes the necessary expansion variables.")
	       (value   "Undefined."))
      (setf method meth)
      (setf real-conclusions real-concs)
      (setf real-premises (set-difference outline real-concs))
      (setf decl-cont (meth~declarative-content method))
      (setf conclusions (meth~conclusions method))
      (setf premises (meth~premises method))
      (cond ((meth~expansion-condition method)
	     (let ((new-meth-map (meth~check-condition (meth~expansion-condition method)
						       (meth~mapp-new-constraint meth-map T)
						       proof-plan)))
	       (cond (new-meth-map
		      (setf mapping new-meth-map)
		      (meth~carry-out-computations (meth~expansion-computations method) new-meth-map))
		     (T
		      (setf success nil)))))
	    (T
	     (setf mapping meth-map)
	     (meth~carry-out-computations (meth~expansion-computations method) meth-map)))
      (setf new-nodes nil)
      (setf pds proof-plan)
      (omega~trace "~%Premises: ~{~%~A~}~%Conclusions: ~{~%~A~}~%Declarative-Content: ~{~%~A~}" premises conclusions decl-cont)
      (omega~trace "~%Metavariable Substitution:~%~A" meth-map)
      (tacl~init outline)
      (omega~message "Expansion of Method ~A with outline ~{~A ~}." method outline)
      )


    (defun pexp=insert-premises-recursive (just)
      (let* ((prems (just~premises just))
	     (hyps)
	     (rem-prem (remove-if #'(lambda (x)
				      (let ((y (find x real-premises)))
					(when y
					  (setf hyps (append (pdsn~hyps y) hyps)))
					y))
				  prems)))
	(mapc #'(lambda (x)
		  (unless (find x new-nodes :test #'keim~equal)
		    (when (infer~dummy-p (just~method (node~justification x)))
		      (setf (pdsn~hyps x) x))
		    (push x new-nodes)
		    (setf hyps (remove-duplicates (append hyps (pexp=insert-premises-recursive (node~justification x)))))
		    (setf (pdsn~hyps x) hyps)))
	      rem-prem)
	hyps))
    
    (defun pexp~expansion-step (conc-line)
      (declare (edited  "23-APR-1998")
	       (authors Sorge)
	       (input   "A single conclusion-line.")
	       (effect  "Inserts some new nodes.")
	       (value   "The newly inserted nodes."))
      (let ((node (meth~object-instance conc-line mapping)))
	(if node
	    (let* ((just (meth~object-instance (node~justification conc-line) mapping)))
	      (cond (just
		     (omega~trace "~%Expanding node ~A with justification ~A" node just)
		     (pexp=insert-premises-recursive just)
		     (pexp=insert-just node just))
		    ((listp (just~premises (node~justification conc-line)))
		     (let* ((prems (pexp=object-instance-list (just~premises (node~justification conc-line)) mapping)))
		       (omega~trace "~%Expanding node ~A with justification ~A and premises:" node (node~justification node))
		       (omega~trace "~%~{~A~}" prems )
		       
		     ))
		    (t (omega~warn "Dont know how to expand an empty justification..."))))
	  (omega~error "Method is underspecified or some computations are missing.~% Node ~A cannot be expanded!" conc-line))))
    
    
    (defun pexp~computations ()
      (declare (edited  "03-APR-1998")
	       (authors Sorge)
	       (input   "None.")
	       (effect  "Changes some justifications and might insert some new nodes into the PDS.")
	       (value   "Undefined."))
      (loop
       (when (or (null conclusions) (not success)) (return))
       (let ((conc (car conclusions)))
	 (setf success (pexp~expansion-step conc)))
       (setf conclusions (cdr conclusions))
       (omega~trace "New Conclusions: ~{~%~A~}" conclusions)
       ))

    
    (defun pexp~end ()            ;;;;; outline-pattern setzen!!!!!!!
      (if success
	  (progn
	    (omega~message "Method ~A successfully expanded!" method)
	    (tacl~end)
	    (values new-nodes T (when (pds~constraint-pool-p (meth~mapp-constraint mapping))
				  (meth~mapp-constraint mapping))))
	(progn
	  (omega~warn "Expansion of method ~A was not successful!!!" method)
	  (tacl~end :force t)
	  (setf success t)
	  (values new-nodes))))

    ))

  

(defun pexp=object-instance-list (objects mmapp)
  (when objects
    (meth~object-instance objects mmapp)))
	     
(defgeneric pexp=insert-just (line &optional just)
  (declare (edited  "23-APR-1998")
	   (authors Sorge)
	   (input   "A line or set of lines and optionally a justification.")
	   (effect  "Inserts the justification as the new actual justification of the node.")
	   (value   "The changed nodes."))
  (:method ((line pdsn+node) &optional (just (node~justification line)))
	   (pdsj~replace-justification! (node~justification line) just)
	   (setf (node~justification line) just)
	   line)
  (:method ((lines list) &optional just)
	   (if (consp just)
	       (let ((full-justs (if (< (length just) (length lines))
				     (append just (make-list (- (length lines) (length just))) :initial-element nil)
				   just)))
		 (mapcar #'pexp=insert-just lines just))
	     (mapcar #'(lambda (x) (pexp=insert-just x just)) lines))))




