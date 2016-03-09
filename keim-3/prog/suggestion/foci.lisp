;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
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

(in-package :keim)
;(in-package :OMEGA) ;; ACHTUNG das hier ist noch nicht ok



(mod~defmod FOCI 
            :uses (data infer keim node pds pdsj pdsn prob sugg)
            :documentation "Focus structures for guiding interactive proofs."
            :exports (
                      foci+pc
                      
                      foci~active-pc
                      foci~add-pcs
                      foci~compute-pcs
		      foci~compute-prev-open-form-info
                      foci~del-pcs
                      foci~descendants
                      foci~find-open
                      foci~find-support
                      foci~focus-line
		      foci~free-vars
                      foci~in-use                      foci~initialize
		      foci~initial-pds-hashtable
		      foci~lineorder
                      foci~passive-pcs
                      foci~pc-create
                      foci~pc-p
                      foci~pc<
                      foci~pc>=
		      foci~pds-hashtable
		      foci~prev-open-forms
		      foci~set-active-focus
                      foci~status
                      foci~suggest-commands
                      foci~supports
                      foci~switch
                      foci~update-pcs
                      
		      
                      foci*in-use
                      foci*pcs))

	
#{
\section{Foci}
The module provides the functionality for focus structures for guiding interactive proofs.
#}

(eval-when (load eval compile)
  
  (defclass foci+pc (keim+object)
    ((focus-line :accessor foci~focus-line
		 :initarg :focus-line
		 :initform nil
		 :documentation "The focussed line of the proof context.")
     (supports :accessor foci~supports
	       :initarg :supports
	       :initform nil
	       :documentation "The support lines of the focus-line.")
     (descendants :accessor foci~descendants
		  :initarg :descendants
		  :initform nil
		  :documentation "Descendant lines of focus-line.")
     (lineorder :accessor foci~lineorder
		:initarg :lineorder
		:initform nil
		:documentation "The chronological orderings of the proof lines.")
     (prev-open-forms :accessor foci~prev-open-forms
		      :initarg :prev-open-forms
		      :initform nil
		      :documentation "Not needed yet.")
     (free-vars       :accessor foci~free-vars
		      :initarg :free-vars
		      :initform nil
		      :documentation "The free variables of the focus, i.e. of the focus
and the support lines.")
     (status :accessor foci~status
	     :initarg :status
	     :initform nil
	     :documentation "The status (regular or non-regular) od pc.")
     (pds-hashtable :accessor foci~pds-hashtable
		    :initarg :pds-hashtable
		    :initform nil
		    :documentation "A hashtable for storing automatically found proofs."))
    (:documentation "The class of proof contexts. Each proof context of a PDS"
		    "completely describes a single open subproblem of the PDS.")))

(defun foci~initial-pds-hashtable ()
  (make-hash-table :test #'equal))

(defmethod print-object ((obj foci+pc) stream)
  (format stream "~%[Open: ~A Supps:(~A) Descs:(~A) Lineorder: ~A Prev-Open-Forms: ~A
Free-Vars: ~A Stat: ~A Proofs: ~A]"
	  (foci~focus-line obj)
	  (foci~supports obj)
	  (foci~descendants obj)
	  (foci~lineorder obj)
	  (foci~prev-open-forms obj)
	  (foci~free-vars obj)
	  (foci~status obj)
	  (sugg~hash2list (foci~pds-hashtable obj) nil)
	  ))
  
(defvar foci*pcs nil
  "A global variable containing all (open) proof contexts.")

(defun foci~pc-create (focus-line supports descendants lineorder prev-open-forms free-vars status pds-hashtable)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "An (open) focus-line, a list of supports, a list of descendants of"
		    "focus-line, a lineorder, nodes that have been examined so far,"
		    "free variables, and a status (regular or non-regular).")
	   (effect  "Creates an instance of class foci+pc.")
	   (value   "An instance of class foci+context."))
  (make-instance 'foci+pc
		:focus-line focus-line
		:supports supports
		:descendants descendants
		:lineorder lineorder
		:prev-open-forms prev-open-forms
		:free-vars free-vars
		:status status
		:pds-hashtable pds-hashtable))


(defun foci~pc-p (obj)
  (declare (edited  "29-DEC-1999")
	   (input   "A object.")
	   (effect  "None")
	   (value   "T, iff obj is an instance of foci+pc"))
  (typep obj 'foci+pc))

(defun foci~active-pc ()
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The active proof context."))
  (when foci*pcs
    (first foci*pcs)))

(defun foci~passive-pcs ()
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The list of non-active proof contexts."))
  (when foci*pcs
    (rest foci*pcs)))

(defun foci~pc< (pc1 pc2)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "Two proof contexts")
	   (effect  "None.")
	   (value   "T iff pc1 is smaller (ad-hoc heuristics) than pc2."))
  (string< (keim~name (foci~focus-line pc1))
	   (keim~name (foci~focus-line pc2))))

(defun foci~pc>= (pc1 pc2)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "Two proof contexts")
	   (effect  "None.")
	   (value   "T iff pc1 is greater (ad-hoc heuristics) than pc2."))
  (not (foci~pc< pc1 pc2)))

(defgeneric foci~add-pcs (obj)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "A proof context or a list of proof contexts")
	   (effect  "Adds the argument proof context(s) to foci*pcs.")
	   (value   "The modified foci*pcs."))
  (:method ((obj foci+pc))
	   (setf foci*pcs
		 (sort (adjoin obj foci*pcs :test #'data~equal)
		       #'foci~pc>=)))
  (:method ((obj list))
	   (setf foci*pcs
		 (sort (union obj foci*pcs :test #'data~equal)
		       #'foci~pc>=)))
  (:method (obj)
	   (error "~A should be a proof context or a list." obj)))

(defgeneric foci~del-pcs (obj)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "A proof context or a list of proof contexts")
	   (effect  "Removes the argument proof context(s) from foci*pcs.")
	   (value   "The modified foci*pcs."))
  (:method ((obj foci+pc))
	   (setf foci*pcs
		 (sort (remove obj foci*pcs :test #'data~equal)
		       #'foci~pc>=)))
  (:method ((obj list))
	   (setf foci*pcs
		 (sort (set-difference foci*pcs obj :test #'data~equal)
		       #'foci~pc>=)))
  (:method (obj)
	   (error "~A should be a proof context or a list." obj)))

(defun foci~find-support (predicate)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "The list of all support-lines in the active proof context, which"
		    "fulfill the predicate."))
  (when (foci~active-pc)
    (mapcan #'(lambda (x)
		(multiple-value-bind (values error)
		    (ignore-errors (apply predicate (list x)))
		  (cond (error (sugg~output t 0 "A predicate could not be applied to find support nodes!~%>>>>  ~A" error))
			((and (not (consp values))
			      (not (null values))) (list (list x)))
			(values (list (cons x values)))
			(t nil))))
	    (let ((active-focus (foci~active-pc)))
	      (when active-focus (foci~supports active-focus))))))

(defun foci~find-supportlist (predicate)
  (declare (edited  "24-OCT-2002")
	   (authors Pollet)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "The list of all premise-lines in the active proof context, which"
		    "fulfill the predicate."))
  (when (foci~active-pc)
		(multiple-value-bind (values error)
		    (ignore-errors (apply predicate (list (foci~supports (foci~active-pc)))))
		  (cond (error (sugg~output t 0 "A predicate could not be applied to find support nodes!~%>>>>  ~A" error))
			((consp values) (list values))
			(t nil)))))

(defun foci~find-premise (predicate)
  (declare (edited  "24-OCT-2002")
	   (authors Pollet)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "The list of all premise-lines in the active proof context, which"
		    "fulfill the predicate."))
  (when (foci~active-pc)
    (mapcan #'(lambda (x)
		(multiple-value-bind (values error)
		    (ignore-errors (apply predicate (list x)))
		  (cond (error (sugg~output t 0 "A predicate could not be applied to find support nodes!~%>>>>  ~A" error))
			((and (not (consp values))
			      (not (null values))) (list (list x)))
			(values (list (cons x values)))
			(t nil))))
	    (let ((active-focus (foci~active-pc)))
	      (when active-focus (foci~premises active-focus))))))

(defun foci~find-line (predicate)
  (declare (edited  "24-OCT-2002")
	   (authors Pollet)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "The list of all lines in the active proof context, which"
		    "fulfill the predicate."))
  (when (foci~active-pc)
    (mapcan #'(lambda (x)
		(multiple-value-bind (values error)
		    (ignore-errors (apply predicate (list x)))
		  (cond (error (sugg~output t 0 "A predicate could not be applied to find support nodes!~%>>>>  ~A" error))
			((and (not (consp values))
			      (not (null values))) (list (list x)))
			(values (list (cons x values)))
			(t nil))))
	    (let ((active-focus (foci~active-pc)))
	      (when active-focus (foci~all-lines active-focus))))))


(defun foci~find-open (predicate &optional (flag NIL))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A predicate and optional a flag, indicating if open nodes, not in the foci"
		    "shall be regarded (not NIL)")
	   (effect  "None.")
	   (value   "1. A list with the focussed line of the open active focus in obj,"
		    "iff this line fulfills the predicate, nil otherwise"
		    "2. A list of all open focussed lines of other foci, which also"
		    "fulfill the predicate."))
  (when (foci~active-pc)
    (flet ((apply-pred (line)
		       (multiple-value-bind (values error)
			   (ignore-errors (apply predicate (list line)))
			 (cond (error (sugg~output t 0 "A predicate could not be applied to find open nodes!~%>>>>  ~A" error))
			       ((and (not (consp values))
				     (not (null values))) (list line))
			       ((car values) (cons line values))
			       (t nil)))))
      (let ((arg1 (apply-pred (foci~focus-line (foci~active-pc))))
	    (arg2 (if flag (mapcan #'(lambda (x) (apply-pred (foci~focus-line x)))
				   (foci~passive-pcs))
		    (list NIL))))
	(values arg1 arg2)))))

(defun foci~find-openlist (predicate)
  (declare (edited  "24-OCT-2002")
	   (authors Pollet)
	   (input   "A predicate.")
	   (effect  "None.")
	   (value   "1. A list with the focussed line of the open active focus in obj,"
		    "iff this line fulfills the predicate, nil otherwise"
		    "2. A list of all open focussed lines of other foci, which also"
		    "fulfill the predicate."))
  (when (foci~active-pc)
    (multiple-value-bind (values error)
	(ignore-errors (apply predicate (list (mapcar #'foci~focus-line
						      (cons (foci~active-pc)
							    (foci~passive-pcs))))))
      (let ((result (cond (error (sugg~output t 0 "A predicate could not be applied to find open nodes!~%>>>>  ~A" error))
			  ((consp values)  values)
			  (t nil))))
	(when result
	  (if (member (foci~focus-line (foci~active-pc)) result)
	      (values (list result) nil)
	    (values nil (list result))))))))

(defun foci~initialize (theorem-line assumption-lines)
   (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A theorem line and a list of assumption lines.")
	   (effect  "Binds the global variable foci*pcs to the"
		    "initial proof-context, defined by the theorem line and the"
		    "assumption lines.")
	   (value   "The modified foci*pcs."))
   (let* ((thm-line (if (listp theorem-line)
			(car theorem-line)
		      theorem-line))
	  (pc (foci~pc-create thm-line
			      assumption-lines
			      nil
			      (mapcar #'keim~name (cons thm-line assumption-lines))
			      nil
			      (mapcan #'(lambda (x) (term~free-variables (node~formula
									  x)))
				      (cons thm-line assumption-lines))
			      :regular
			      (foci~initial-pds-hashtable)
			      )))
     (setf foci*pcs (list pc))))

(defgeneric foci~suggest-commands (pds)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A proof plan.")
	   (effect  "None.")
	   (value   "some suggested commands. (This function is redefined in omega."))
  (:method (pds)
	   (declare (ignore pds))
	   nil)
  (:method (obj)
	   (error "~A should be a PDS." obj)))

(defun foci~compute-pcs (&key  (pds pds*current-proof-plan) delete-case (pds-hashtable (foci~initial-pds-hashtable)))
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "(Optionally) a PDS, and a flag delete-case signalling that"
		    "foci~comute-pcs is executed since a note has been deleted."
                    "In this case one probably wants to save the search"
		    "information stored in the proof contexts in foci*pcs.")
	   (effect  "Modifies foci*pcs and the proof context slots of the given PDS.")
	   (value   "The modified foci*pcs."))
  (declare (ignore delete-case)) ;;; not used yet
  (let* ((root (prob~proof-root pds))
	 (regular-pcs (remove-duplicates
		       (foci=compute-regular-pcs root pds (foci=sort-lines pds) :pds-hashtable pds-hashtable)
		       :test #'(lambda (x y) (string-equal
					      (keim~name (foci~focus-line x))
					      (keim~name (foci~focus-line y))))))
	 (non-regular-pcs (remove-duplicates
			   (foci=compute-non-regular-pcs regular-pcs pds (foci=sort-lines pds) :pds-hashtable pds-hashtable)
			   :test #'(lambda (x y) (string-equal
						  (keim~name (foci~focus-line x))
						  (keim~name (foci~focus-line y))))))
	 (new-foci-pcs (sort (append regular-pcs non-regular-pcs) #'foci~pc>=)))
    (setf foci*pcs (sort new-foci-pcs #'foci~pc>=))
    (setf (pds~proof-context pds) foci*pcs)
    ))

									     
(defun foci=compute-regular-pcs (root pds lineorder &key collected-descendants prev-open-forms pds-hashtable)
  (declare (edited  "30-DEC-1999")
	   (authors Chris)
	   (input   "A root node, a PDS, a lineorder for PDS, and (optionally) the"
		    "descendants of root and some previously open forms")
	   (effect  "None.")
	   (value   "A list of all regular proof contexts reachable from root."))
  (if (pdsn~open-node-p root)
      (list (foci~pc-create root
			    (pds~node-supports root)
			    collected-descendants
			    lineorder
			    prev-open-forms
			    (mapcan #'(lambda (x) (term~free-variables (node~formula x)))
				    (cons root (pds~node-supports root)))
			    :regular
			    pds-hashtable))
    (let ((prem-nodes (pdsj~all-premises (node~justification root))))
      (mapcan #'(lambda (new-root)
		  (foci=compute-regular-pcs new-root pds lineorder
					    :collected-descendants (cons root
									 collected-descendants)
					    :prev-open-forms prev-open-forms
					    :pds-hashtable pds-hashtable))
	      prem-nodes))))

(defun foci=sort-lines (obj)
  (declare (edited  "30-DEC-1999")
	   (authors Chris)
	   (input   "A PDS.")
	   (effect  "None.")
	   (value   "The sorted list of proof lines."))
  (let* ((orig-theo (keim~name (prob~proof-root obj)))
	 (orig-assus (mapcar #'keim~name (prob~proof-assumptions obj)))
	 (all-steps (mapcar #'keim~name (prob~proof-steps obj)))
	 (other-lines (remove orig-theo
			      (set-difference all-steps orig-assus))))
    (append (sort other-lines #'string>)
	    (cons orig-theo orig-assus))))
					     
(defun foci=compute-non-regular-pcs (regular-pcs pds lineorder &key prev-open-forms pds-hashtable)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "A list of regular pcs' (of pds), a PDS, a ordering of the lines"
		    "in PDS, and optionally some previously open formulas.")
	   (effect  "None.")
	   (value   "A list of all non regular proof contexts of pds (wrt regular-pcs)."))
  (let ((non-regular-open-nodes
	 (set-difference (pds~open-nodes pds)
			 (mapcar #'foci~focus-line regular-pcs))))
    (mapcan #'(lambda (new-root)
		(foci=comp-non-reg-pcs new-root pds lineorder
				       :collected-descendants nil
				       :prev-open-forms prev-open-forms
				       :pds-hashtable pds-hashtable))
	    non-regular-open-nodes)))

(defun foci=comp-non-reg-pcs (root pds lineorder &key collected-descendants prev-open-forms pds-hashtable)
  (declare (edited  "30-DEC-1999")
	   (authors Chris)
	   (input   "A root node, a PDS, a lineorder for PDS, and (optionally) the"
		    "descendants of root and some previously open forms")
	   (effect  "None.")
	   (value   "A list of non-regular proof contexts."))
  (if (pdsn~open-node-p root)
      (list (foci~pc-create root
			    (pds~node-supports root)
			    collected-descendants
			    lineorder
			    prev-open-forms
			    (mapcan #'(lambda (x) (term~free-variables (node~formula
									x)))
				    (cons root (pds~node-supports root)))
			    :non-regular
			    pds-hashtable))
    (let ((prem-nodes (pdsj~all-premises (node~justification root))))
      (mapcan #'(lambda (new-root)
		  (foci=comp-non-reg-pcs new-root
					 pds
					 lineorder
					 :collected-descendants (cons root
								      collected-descendants)
					 :prev-open-forms prev-open-forms
					 :pds-hashtable pds-hashtable))
	      prem-nodes))))

(defgeneric foci=update-pcs (previous-open-line new-open-lines &optional  (pds pds*current-proof-plan) pds-hashtable)
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "An previously open line (which is closed now) and the new open lines"
		    "(optionally a PDS)")
	   (effect  "Modifies foci*pcs and the proof context slots of the given PDS.")
	   (value   "The modified foci*pcs."))
  (:method ((previous-open-line pdsn+node) new-open-lines &optional (pds pds*current-proof-plan) pds-hashtable)
	   (let* ((pc-to-modify (find-if #'(lambda (x) (equal previous-open-line
							     (foci~focus-line x)))
					foci*pcs))
		  (new-lineorder (append (mapcar #'keim~name new-open-lines)
					 (when pc-to-modify (foci~lineorder pc-to-modify))))
		  (coll-descendants (when pc-to-modify (foci~descendants pc-to-modify)))
		  (prev-open-forms (when pc-to-modify (foci~prev-open-forms pc-to-modify)))
		  (new-pcs (foci=compute-regular-pcs previous-open-line
						     pds
						     new-lineorder
						     :collected-descendants coll-descendants
						     :prev-open-forms prev-open-forms
						     :pds-hashtable pds-hashtable)))
	     (setf foci*pcs (sort (append new-pcs (remove pc-to-modify foci*pcs)) #'foci~pc>=))))
  (:method ((previous-open-line null) new-open-lines &optional (pds pds*current-proof-plan) pds-hashtable)
	   (let ((new-pcs (mapcan #'(lambda (x)
				      (foci=comp-non-reg-pcs
				       x pds
				       (append (mapcar #'keim~name new-open-lines)
					       (foci~lineorder (foci~active-pc)))
				       :collected-descendants nil
				       :prev-open-forms nil
				       :pds-hashtable pds-hashtable))
				  new-open-lines)))
	     (setf foci*pcs (sort (append new-pcs foci*pcs) #'foci~pc>=))))
  (:method ((previous-open-line cons) new-open-lines &optional (pds pds*current-proof-plan) pds-hashtable)
	   (dolist (pol previous-open-line)
	     (foci=update-pcs pol new-open-lines pds pds-hashtable))))



(defun foci~update-pcs (previous-open-line new-open-lines &optional  (pds pds*current-proof-plan) (pds-hashtable (foci~initial-pds-hashtable)))
  (declare (edited  "29-DEC-1999")
	   (authors Chris)
	   (input   "An previously open line (which is closed now) and the new open lines"
		    "(optionally a PDS)")
	   (effect  "Modifies foci*pcs and the proof context slots of the given PDS.")
	   (value   "The modified foci*pcs."))
  (foci=update-pcs previous-open-line new-open-lines pds pds-hashtable)
  (when pds (setf (pds~proof-context pds) foci*pcs))
  (when (foci~active-pc) (sugg~reset)))


(defgeneric foci~set-active-focus (line &optional (pds pds*current-proof-plan))
  (declare (edited  "30-DEC-1999" "14-APR-1998")
	   (authors Sorge Chris)
	   (input   "An instance of foci+pc or a proofline (specifying the new active"
		    " focus), and a proof context,"
		    "i.e. an instance of foci+focus-context or"
		    "foci+proof-context.")
	   (effect  "Destructively changes obj, by setting obj2 as new active focus of it.")
	   (value   "The changed object."))
  (:method ((focus foci+pc) &optional (pds pds*current-proof-plan))
	   (let ((pc (if pds (pds~proof-context pds) foci*pcs)))
	     (if (find focus pc)
		 (let* ((sublist1 (member focus pc))
			(sublist2 (reverse (cdr (member focus (reverse foci*pcs)))))
			(new-pc (append sublist1 sublist2)))
		   (cond ((and pds (tree-equal pc foci*pcs))
			  (setf (pds~proof-context pds) new-pc)
			  (setf foci*pcs new-pc))
			 (pds (setf (pds~proof-context pds) new-pc))
			 (t (setf foci*pcs new-pc)))))))
  (:method ((focus pdsn+node) &optional (pds pds*current-proof-plan))
	   (let* ((pc (if pds (pds~proof-context pds) foci*pcs))
		  (new-active (find-if #'(lambda (x) (equal (foci~focus-line x) focus)) pc)))
	     (foci~set-active-focus new-active pds)))
  (:method (obj &optional (pds pds*current-proof-plan))
	   (declare (ignore obj))
	   (foci~compute-pcs :pds pds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional and modified methods for other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod pds~start-proof-plan :around ((problem prob+problem) name)
  (declare (ignore name))
  (let* ((proof-plan (call-next-method))
	 (assumptions (pds~support-nodes proof-plan))
	 (conc (pds~open-nodes proof-plan))
	 (proof-context (foci~initialize conc assumptions)))
    (when proof-plan
      (setf (pds~proof-context proof-plan) proof-context))
    proof-plan))

(defvar foci*in-use nil)

(defun foci~switch (&optional (status (not foci*in-use)))
  (setf foci*in-use status))

(defun foci~in-use ()
  foci*in-use)

(defmethod infer~compute-outline :around ((tactic infer+inference) (outline list) (parameters list))
  (let ((open-nodes (mapcan #'(lambda (x) (when (and (pdsn~p x)
						     (pdsn~open-node-p x))
					    (list x)))
			    outline))
	)
    (multiple-value-bind (new-outline success)
	(call-next-method)
      (when (and success (foci~in-use))
	;;(foci~update-pcs open-nodes (remove-if-not #'pdsn~open-node-p new-outline)))
	(foci~compute-pcs)
	(when pds*current-proof-plan (setf (pds~proof-context pds*current-proof-plan) foci*pcs))
	(when (foci~active-pc) (sugg~reset)))
      (values new-outline success))))


(defmethod infer~apply-expansion-function :around ((method infer+inference) (outline list)
						   (parameters list))
   (let ((result (call-next-method)))
     (when (foci~in-use) (foci~compute-pcs))
     result))



;;;;;;;;

(defun foci~premises (foc);MP
  (let ((hyps (pdsn~hyps (foci~focus-line foc)))
	(all (remove-if #'pdsn~open-node-p (mapcar #'pds~label2node (foci~lineorder foc)))))
    (remove-if-not #'(lambda (l) (subsetp  (pdsn~hyps l) hyps)) all)))

(defun foci~all-lines (foc);MP
  (remove (foci~focus-line foc) (mapcar #'pds~label2node (foci~lineorder foc))))
