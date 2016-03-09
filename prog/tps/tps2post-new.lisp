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


(mod~defmod TPS2POST
            :uses (env infer just keim node omega pds pdsj pdsn post prob tacl term tps-omega)
            :documentation "Translation of TPS proofs into OMEGAs PDS."
            :exports (
                      
                      t2p~translate-and-insert-tps-proof
                      t2p~translate-formula
                      t2p~translate-tps-proof
		      t2p~translate-tps-proofstring
                      ))


(defvar t2p*fixed-predicates '("EXISTS" "FORALL" "LAMBDA" "~" "FALSEHOOD" "TRUTH"))
(defvar t2p*tps-proof-object nil)
(defvar t2p*bound-variables nil)
(defvar t2p*term2type-table (make-hash-table :test #'equal))

(defun t2p~translate-formula (tps-formula &optional (plan omega*current-proof-plan))
  (declare (edited  "21-JAN-1998")
	   (authors Sorge)
	   (input   "A string representing a TPS formula.")
	   (value   "The same formula in \\POST syntax"))
  (let* ((env (pds~environment plan))
	 (formula (t2p=parse-formula tps-formula env))
	 (t2p*term2type-table (make-hash-table :test #'equal)))
    (post~read-object formula env :existing-term)))


(defclass t2p=proof-line (keim+object)
  ((label :initarg :label :accessor t2p=label)
   (hyplist :initarg :hyplist :accessor t2p=hyplist)
   (formula :initarg :formula :accessor t2p=formula)
   (just-name :initarg :just-name :accessor t2p=just-name)
   (just-params :initarg :just-params :accessor t2p=just-params)
   (just-premises :initarg :just-premises :accessor t2p=just-premises)
   (reference :initarg :reference :accessor t2p=reference)
   (pds-node :initarg :pds-node :accessor t2p=pds-node))
  (:Documentation "The (internal) class of TPS Proof Structures"))


(defun t2p=proof-line-create (label hyplist formula just-name just-params just-premises reference pds-node)
  (declare (edited  "22-JAN-1998")
	   (authors Chris Sorge)
	   (input   "TPS Proofline infos.")
	   (effect  "Creates a new internal instance of t2p=proof-line")
	   (value   "The new object."))
  (make-instance 't2p=proof-line
		 :label label
		 :hyplist hyplist
		 :formula formula
		 :just-name just-name
		 :just-params just-params
		 :just-premises just-premises
		 :reference reference
		 :pds-node pds-node))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the proof from a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun t2p~translate-and-insert-tps-proof (tps-node &optional (plan omega*current-proof-plan))
  (declare (edited  "26-JAN-1998")
	   (authors SorgeChris)
	   (input   "A node justified with a TPS proof.")
	   (effect  "Inserts a partial proof plan.")
	   (value   "Unknown."))
  (let* ((just (node~justification tps-node))
	 (hyp-nodes (just~premises just))
	 (tps-point (car (pdsj~parameters just)))
	 (t2p*tps-proof-object (cdr (assoc tps-point tps*problem-list)))
	 (tps-prf  (atpprb~tps-proof-string t2p*tps-proof-object)))
    (tacl~init (cons tps-node hyp-nodes))
    (let ((node-list (t2p~translate-tps-proof tps-prf)))
      (setf (prob~proof-steps plan) (union (prob~proof-steps plan) node-list))
      (dolist (x node-list)
	(setf (gethash (symbol-name (keim~name x)) (pds~label-node-hashtable plan)) x))
      )
    (tacl~end)
    ))


(defun t2p~translate-and-insert-partial-tps-proof (tps-node tps-proof-obj &optional (plan omega*current-proof-plan))
  (declare (edited  "05-AUG-1998")
	   (authors ChrisSorge)
	   (input   "A TPS Proofobject")
	   (effect  "Inserts a partial proof plan.")
	   (value   "Unknown."))
  (let* ((just (node~justification tps-node))
	 (hyp-nodes (just~premises just))
	 (t2p*tps-proof-object tps-proof-obj)
	 (tps-prf  (atpprb~tps-proof-string t2p*tps-proof-object)))
    (tacl~init (cons tps-node hyp-nodes))
    (let ((node-list (t2p~translate-tps-proof tps-prf)))
      (progn (setf (prob~proof-steps plan) (union (prob~proof-steps plan) node-list))
	     (dolist (x node-list)
	       (setf (gethash (symbol-name (keim~name x)) (pds~label-node-hashtable
							   plan)) x))))
    (tacl~end)
    ))




  
(defun t2p~translate-tps-proof (tps-proof)
  (declare (edited  "26-JAN-1998")
	   (authors Chris Sorge)
	   (input   "A TPS proof object.")
	   (effect  "Creates pds-nodes containing the TPS-proof.")
	   (value   "A list of newly created nodes or names of TPS justifications, that"
		    "cannot be translated as there are no corresponding tps*tactics in OMEGA."))
  (let* ((tps-lines (cdr (seventh tps-proof)))
	 (tps-node-hashtable (make-hash-table :test #'equal))
	 existing-lines)
    (flet ((check-node (tps-node)
		       (if (just~method
			    (t2p=translate-justification (t2p=just-name tps-node)
							 (t2p=translate-parameters
							  (t2p=just-name tps-node)
							  (t2p=just-params tps-node))
							 (t2p=just-premises tps-node)))
			   tps-node
			 (error "Sorry, but not all TPS justifications are known to OMEGA and therefore this partial can not be translated to OMEGA. The first problematic TPS justification is ~A. For further information contact chris|sorge@cs.uni-sb.de."
				(t2p=just-name tps-node))))
	   (make-pds-node (tps-node)
			  (let ((ref (t2p=reference tps-node)))
			    (cond ((and ref (string-equal (cdr ref) :OPEN))
				   (let ((node (pds~label2node (car ref) omega*current-proof-plan)))
				     (push node existing-lines)
				     (pdsn~replace-just! node
							 (t2p=translate-justification (t2p=just-name tps-node)
										      (t2p=translate-parameters
										       (t2p=just-name tps-node)
										       (t2p=just-params tps-node))
										      (t2p=just-premises tps-node)))
				     node))
				  (ref (let ((node (pds~label2node (car ref) omega*current-proof-plan)))
					 (push node existing-lines)
					 node))
				  (t (let ((label (pds~new-node-name))
					   hyps
					   (wff (t2p~translate-formula (t2p=formula tps-node)))
					   (just (t2p=translate-justification (t2p=just-name tps-node)
									      (t2p=translate-parameters
									       (t2p=just-name tps-node)
									       (t2p=just-params tps-node))
									      (t2p=just-premises tps-node))))
				       (pdsn~create label hyps wff just))))))
	   (update-node (tps-node)
			(let ((new-hyplist (mapcar #'(lambda (x) (gethash x tps-node-hashtable))
						   (t2p=hyplist tps-node)))
			      (new-premises (mapcar #'(lambda (x) (gethash x tps-node-hashtable))
						    (t2p=just-premises tps-node))))
			  (setf (pdsn~hyps (t2p=pds-node tps-node)) new-hyplist)
			  (setf (just~premises (node~justification (t2p=pds-node
								    tps-node)))
				new-premises)
			  (if (pdsn~just-method (t2p=pds-node tps-node))
			      (let ((node (t2p=pds-node tps-node)))
				(when view*on (view~update-step nil node))
				node)
			    (t2p=just-name tps-node))))
	   )
      (let ((new-node-list
	     (mapcar #'update-node
		     (mapcar #'check-node
			     (mapcar #'(lambda (x) (let* ((new-node (t2p=insert-node x))
							  (pds-node (make-pds-node new-node)))
						     (setf (t2p=pds-node new-node) pds-node)
						     (setf (gethash (t2p=label new-node) tps-node-hashtable) pds-node)
						     new-node))
				     tps-lines)))))
	(t2p=update-rulec-nodes new-node-list) ;;; for Armin and Proverb, adds parameter
	                                       ;;; to the tps*RuleC nodes
	(set-difference new-node-list existing-lines)))))


;;;  (trace t2p=set-hyps t2p=insert-node t2p=set-conclusion t2p=correct-hyps t2p=pds-node t2p=label t2p=pds-node )


(defun t2p=update-rulec-nodes  (nodelist) 
  ;;; for Armin and Proverb: Rulec gets a parameter
  (declare (edited  "13-11-98")
	   (authors Chris)
	   (input   "All nodes of a tps-proof")
	   (effect  "None.")
	   (value   "All nodes justified by tps*RuleC get a"
		    "parameter"))
  (mapcar #'(lambda (tps-node)
	      (when (t2p=RuleC-node-p tps-node)
		(setf (pdsj~parameters (node~justification tps-node))
		      (t2p=RuleC-node-param tps-node nodelist))))
	  nodelist))

(defun t2p=RuleC-node-param (pdsnode nodelist)
  (declare (edited  "13-11-98")
	   (authors Chris)
	   (input   "A pds node and a list of nodes belonging to the same tps-proof")
	   (effect  "None.")
	   (value   "If node is justified by tps*RuleC, then the parameter"
		    "of this node (computed from the other nodes) is returned"))
  (when (t2p=RuleC-node-p pdsnode)
    (let* ((just-rulec-node (node~justification pdsnode))
	   (premises (just~premises just-rulec-node))
	   (prem-node  (first premises))
	   (choose-node (find-if #'(lambda (node) 
				     (and (find prem-node (node~just-premises node))
					  (eq (node~just-method node) (infer~find-method "tps*choose"))))
				 nodelist)))
      (pdsj~parameters (node~justification choose-node)))))
	   
	 
(defun t2p=RuleC-node-p (node)
  (eq (node~just-method node) (infer~find-method "tps*rulec")))
	  
  

(defun t2p=insert-node (proof-line)
  (let* ((comment (seventh proof-line))
	 (real-com (when (and comment (stringp comment))
		     (let ((com-list (read-from-string comment)))
		       (when (and (listp com-list)
				  (listp (car com-list))
				  (listp (cadr com-list))
				  (string-equal (caar com-list) :OMEGA-LABEL)
				  (string-equal (caadr com-list) :OMEGA-JUSTIFICATION))
			 (cons (cadar com-list) (cadadr com-list)))))))    
    (t2p=proof-line-create (first proof-line) (second proof-line) (third proof-line)
			   (fourth proof-line) (fifth proof-line) (sixth proof-line)
			   real-com nil)))

(defun t2p=translate-parameters (rule parameters &optional (plan omega*current-proof-plan))
  (declare (edited  "26-JAN-1998")
	   (authors Sorge)
	   (input   "The name of a rule and a list of parameters.")
	   (effect  "None.")
	   (value   "Translates the parameters in a way specified by the rules and returns the"
		    "correct parameter-list corresponding to OMEGAs rules."))
  (cond ((or (string-equal rule "UI")
	     (string-equal rule "Choose")
	     (string-equal rule "UGen")
	     (string-equal rule "EGen"))	 
	 (mapcar #'(lambda (x) (t2p~translate-formula x plan)) parameters))
	((not (null parameters))
	 (error "Don't know how to translate the parameter list for TPS-rule ~A" rule))
	(t parameters)))
  
(defun t2p=translate-justification (name params premises)
  (let ((newname (if (stringp name) name (string name))))
    (if (and (string-equal (aref newname 0) #\P)
	     (string-equal (aref newname 1) #\L)
	     (string-equal (aref newname 2) #\A)
	     (string-equal (aref newname 3) #\N))
	(pdsj~open-just-create)
      (pdsj~closed-just-create
       (infer~find-method (t2p=rule-name newname))
       nil
       params
       "unexpanded"
       nil;; substitution
       (make-list (1+ (length premises)) :initial-element "existent")
       nil))))
	  


(defun t2p=rule-name (name)
  (declare (edited  "27-JAN-1998 18:37")
	   (authors SORGE)
	   (input   "A string representing a TPS rule-name.")
	   (value   "A string representing an OMEGA tactic-name."
		    "TPS rules are directly translated into tactic-names, with prefix TPS*."
		    "Any blanks in the TPS name are translated to dashes."))
  (when (stringp name)
    (flet ((check-char (char)
		       (if (string-equal char #\space)
			   #\-
			 char)))
      (do* ((count 0 (1+ count))
	    (char (aref name count) (aref name count))
	    (newstr (format nil "~A" (check-char char))
		    (format nil "~A~A" newstr (check-char char))))
	  ((= count (1- (length name)))
	   (format nil "tps*~A" newstr))))))
	  
;;; New formula parsing....

(defun t2p=parse-formula (formula env)
  (declare (edited  "10-MAR-1998 19:23")
	   (authors SORGE)
	   (input   "A string containing a tps-formula and an environment.")
	   (effect  "None.")
	   (value   "A string containing a omega-formula."))
  (let ((post-expr (read-from-string (format nil "(~A)" (t2p=parse-formula-rec formula)))))
    (if (> (length post-expr) 1)
	(t2p=second-parse post-expr env)
      (t2p=second-parse (car post-expr) env))))
      
(defun t2p=parse-formula-rec (formula)
  (if (equal formula "") ""
    (let ((char (aref formula 0))
	  (rest-formula (subseq formula 1)))
      (cond ((string-equal char #\[)
	     (concatenate 'string "(" (t2p=parse-formula-rec rest-formula)))
	    ((string-equal char #\])
	     (concatenate 'string ")" (t2p=parse-formula-rec rest-formula)))
	    ((string-equal char #\SPACE)
	     (concatenate 'string " " (t2p=parse-formula-rec rest-formula)))
	    (t 
	     (multiple-value-bind (term rest-formula)
		 (t2p=parse-term formula)
	       (concatenate 'string term (t2p=parse-formula-rec rest-formula))))))))

(defun t2p=parse-term (formula)
  (declare (edited  "11-MAR-1998")
	   (authors Sorge)
	   (input   "A partial formula.")
	   (value   "A term if formula starts with one and the remaining formula."))
  (let ((term (t2p=parse-term-rec formula)))
    (values (t2p=translate-term term) (string-left-trim term formula))))

(defun t2p=parse-term-rec (formula)
  (if (or (equal formula "")
	  (find (aref formula 0) '(#\[ #\] #\SPACE)))
      ""
    (concatenate 'string
		 (coerce (list (aref formula 0)) 'string)
		 (t2p=parse-term-rec (subseq formula 1)))))
	   
(defun t2p=translate-term (term)
  (declare (edited  "22-JAN-1998")
	   (authors Sorge)
	   (input   "A term.")
	   (value   "The term in POST syntax and a flag specifying a lambda scope."))
  (cond
   ((string-equal term "~") "NOT")
   ((string-equal (aref term 0) #\~) (format nil "(NOT ~A)" (t2p=translate-term (subseq term 1))))
   ((string-equal term "LAMBDA") "LAM")
   ((string-equal term "FALSEHOOD") "FALSE")
   ((string-equal term "TRUTH") "TRUE")
   ((find term '("=" "FORALL" "EXISTS" "IMPLIES" "AND" "OR" "EQUIV") :test #'string-equal) term)
   (t (t2p=untype term))))

(defun t2p=untype (term)
  (let ((pos (position #\( term)))
    (if pos
	(let ((real-term (concatenate 'string
				      (subseq term 0 pos)
				      "___"
				      (substitute #\^ #\)
				       (substitute #\_ #\( 
						   (subseq term pos))))))
	  (setf (gethash (string-upcase real-term) t2p*term2type-table)
		(cons (subseq term 0 pos) (subseq term pos)))
	  real-term)
      term)))
		   
(defun t2p=second-parse (formula env)
  (cond ((null formula) formula)
	((atom formula)
	 (let ((term-assoc (gethash (symbol-name formula) t2p*term2type-table)))
	     (if term-assoc
		 (let ((term (car term-assoc))
		       (type (cdr term-assoc)))
		   (unless (find formula t2p*bound-variables)
		     (t2p=env-entry term type env))
		   (read-from-string term))
	       formula)))
	((or (equal (car formula) 'forall)
	     (equal (car formula) 'exists))
	 (push (second formula) t2p*bound-variables)
	 (prog1 
	     (list (car formula)
		   (list 'lam
			 (t2p=bound-variable (second formula))
			 (t2p=second-parse (third formula) env)))
	   (pop t2p*bound-variables)))
	((equal (car formula) 'lam)
	 (push (second formula) t2p*bound-variables)
	 (prog1 
	     (list (car formula)
		   (t2p=bound-variable (second formula))
		   (t2p=second-parse (third formula) env))
	   (pop t2p*bound-variables)))
	(t (list (t2p=second-parse (car formula) env)
		 (t2p=second-parse (cadr formula) env)))))

(defun t2p=env-entry (term type env)
  (unless (env~lookup-object term env :test #'string=)
    (let ((real-types (read-from-string (t2p=get-types type)))
	  (real-term (read-from-string term)))
      (if (upper-case-p (aref term 0))
	  (post~read-object (list real-term real-types) env :constant)
	(post~read-object (list real-term real-types) env :variable)))))

  
(defun t2p=get-types (string)
  (declare (edited  "22-JAN-1998")
	   (authors Sorge)
	   (input   "A string containing TPS type-information.")
	   (value   "A string in POST type syntax."))
  (unless (equal string "")
    (let ((char (aref string 0)))
      (if (or (equal char #\() (equal char #\)))
	  (concatenate 'string
		       (format nil "~C" char)
		       (t2p=get-types (subseq string 1)))
	(concatenate 'string
		     (symbol-name (car (rassoc char
					       (if (atpprb~tps-problem-p
						    t2p*tps-proof-object)
						   (atpprb~tps-type-translation
						    t2p*tps-proof-object)
						 (tps~_typetable t2p*tps-proof-object)) :test #'string-equal)))
		     " "
		     (t2p=get-types (subseq string 1)))))))

(defun t2p=bound-variable (var)
  (let* ((var-assoc (gethash (symbol-name var) t2p*term2type-table))
	 (name (car var-assoc))
	 (type (cdr var-assoc)))
    (list (read-from-string name)
	  (read-from-string (t2p=get-types type)))))

