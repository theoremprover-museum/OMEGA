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
            :uses (infer just keim node pds pdsj pdsn prob)
            :documentation "Focus structures for guiding interactive proofs."
            :exports (foci+context
                      foci+context
                      foci+focus
                      foci+focus-context
                      foci+proof-context
                      
                      foci~active-focus
                      foci~check
                      foci~compute-proof-context-from-pds
                      foci~context-create
                      foci~context-p
                      foci~descendants
                      foci~find-open
                      foci~find-support
                      foci~focus-context
                      foci~focus-context-p
                      foci~focus-create
                      foci~focus-line
                      foci~focus-p
                      foci~in-use
                      foci~initialize
                      foci~lineorder
                      foci~open-foci
                      foci~proof-context-create
                      foci~proof-context-p
                      foci~set-active-focus
                      foci~status
                      foci~subcontexts
                      foci~supports
                      foci~switch
                      foci~update
                      foci~suggest-commands
		      
                      foci*in-use
                      foci*proof-context))



;;#{
;;\subsection{Focus structures for guiding interactive proofs}
;;#}

(eval-when (load eval compile)
  
  (defclass foci+focus-context (keim+object)
    ((focus-line :accessor foci~focus-line
		 :initarg :focus-line
		 :initform nil
		 :documentation "The focussed line of the proof context.")
     (descendants :accessor foci~descendants
		  :initarg :descendants
		  :initform nil
		  :documentation "Descendant lines of focus-line.")
     (status :accessor foci~status
	     :initarg :status
	     :initform nil
	     :documentation "The status (open, closed) of the focus context."))
    (:documentation "The superclass of foci and contexts."))
  
  (defclass foci+context (foci+focus-context)
    ((subcontexts :accessor foci~subcontexts
		  :initarg :subcontexts
		  :initform nil
		  :documentation "The subcontexts of the focus context."))
    (:documentation "The class of contexts."))

  (defclass foci+focus (foci+focus-context)
    ((supports :accessor foci~supports
	       :initarg :supports
	       :initform nil
	       :documentation "The support lines of the focus context."))
    (:documentation "The class of foci."))
  
  (defclass foci+proof-context (keim+object)
    ((focus-context :accessor foci~focus-context
		    :initarg :focus-context
		    :initform nil
		    :documentation "The focus context of the proof context.")
     (open-foci :accessor foci~open-foci
		:initarg :open-foci
		:initform nil
		:documentation "The list of all open foci embedded in the focus-context.
This list is ordered, whereas the ordering defines the priority between the particular
foci.")
     (lineorder   :accessor foci~lineorder
		  :initarg :lineorder
		  :initform nil
		  :documentation "The lineorder of all nodes in the proof context. (An
odered list of all nodes. Note that all slots in the classes foci+focus or foci+context
which  contain lists of prooflines are ordered, and thus define a lineorder. As an
invariant we have that all these lineorders have to be consistent with
the overall lineorder in the proof context.)"))
       (:documentation "The class of proof contexts."))

  )

(defvar foci*proof-context nil
  "A global variable containing a proof context.")

(defun foci~context-create (subcontexts focus-line descendants)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A list of subcontexts, a focus-line and a list of descendants of"
		    "focus-line.")
	   (effect  "Creates an instance of class foci+context.")
	   (value   "An instance of class foci+context."))
  (make-instance 'foci+context
		:focus-line focus-line
		:status (if (and (pdsn~p focus-line)
				 (pdsn~open-node-p focus-line))
			    :open
			  :closed)
		:descendants descendants
		:subcontexts subcontexts))

(defun foci~focus-create (supports focus-line descendants)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A ordered list of supports, a focus-line and a ordered"
		    "list of descendants of focus-line.")
	   (effect  "Creates a instance of class foci+focus.")
	   (value   "An instance of class foci+focus."))
  (make-instance 'foci+focus
		:focus-line focus-line
		:status (if (and (pdsn~p focus-line)
				 (pdsn~open-node-p focus-line))
			    :open
			  :closed)
		:descendants descendants
		:supports supports))

(defun foci~proof-context-create (focus-context lineorder open-foci)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A focus-contexts and a ordered lists of prooflines. Furthermore (by"
		    "key) a list of open foci, specifying a priority-order on the open"
		    "of the object.")
	   (effect  "Creates an instance of class foci+proof-context and destructively"
		    "changes the focus-contexts in order to make them consistent with"
		    "the lineorder and the priority order on the"
		    "open foci defined by open-foci.")
	   (value   "Creates an instance of class foci+proof-context." ))
  (let ((obj (make-instance 'foci+proof-context
			    :focus-context focus-context
			    :open-foci open-foci
			    :lineorder lineorder)))
    obj))


(defun foci~proof-context-p (obj)
  (declare (edited  "16-APR-1998")
	   (authors Chris)
	   (input   "A object.")
	   (effect  "None")
	   (value   "T, iff obj is an instance of foci+proof-context"))
  (typep obj 'foci+proof-context))

(defun foci~focus-context-p (obj)
  (declare (edited  "16-APR-1998")
	   (authors Chris)
	   (input   "A object.")
	   (effect  "None")
	   (value   "T, iff obj is an instance of foci+focus-context"))
  (typep obj 'foci+focus-context))

(defun foci~context-p (obj)
  (declare (edited  "16-APR-1998")
	   (authors Chris)
	   (input   "A object.")
	   (effect  "None")
	   (value   "T, iff obj is an instance of foci+context"))
  (typep obj 'foci+context))


(defun foci~focus-p (obj)
  (declare (edited  "16-APR-1998")
	   (authors Chris)
	   (input   "A object.")
	   (effect  "None")
	   (value   "T, iff obj is an instance of foci+focus"))
  (typep obj 'foci+focus))


(defun foci~active-focus (&optional (obj foci*proof-context))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "Optionally a focus object, i.e. an instance of foci+focus-context or foci+proof-context.")
	   (effect  "None.")
	   (value   "The active focus of the object."))
  (if (foci~proof-context-p obj)
      (car (foci~open-foci obj))
    (error "Function foci~~active-focus only makes sense for instances of class foci+proof-context, but ~A is of type ~A" obj (type-of obj))))
  
(defgeneric foci~set-active-focus (obj2 &optional (obj foci*proof-context))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "An instance of focus+focus or a proofline (specifying the new active"
		    " focus) and a focus object,"
		    "i.e. an instance of foci+focus-context or"
		    "foci+proof-context.")
	   (effect  "Destructively changes obj, by setting obj2 as new active focus of it.")
	   (value   "The changed object."))
  (:method ((obj2 foci+focus) &optional (obj foci*proof-context))
	   (if (find obj2 (foci~open-foci obj))
		 (let ((sublist1 (member obj2 (foci~open-foci obj)))
		       (sublist2 (reverse
				  (cdr (member obj2 (reverse (foci~open-foci obj)))))))
		   (setf (foci~open-foci obj) (append sublist1 sublist2)))))
  (:method ((obj2 pdsn+NODE) &optional (obj foci*proof-context))
	   (let ((new-active
		  (find-if #'(lambda (x)
			       (and  (pdsn~p (foci~focus-line x))
				     (pdsn~open-node-p (foci~focus-line x))
				     (keim~equal (keim~name (foci~focus-line x))
						 (keim~name obj2))))
			   (foci~open-foci obj))))
	     (foci~set-active-focus new-active obj))) 
  (:method (obj2 &optional (obj foci*proof-context))
;	   (error "Function foci~~set-active-focus only makes sense for instances of
;class foci+proof-context and foci+focus, but ~A and ~A are of types ~A and ~A"
;                        obj obj2 (type-of obj) (type-of obj2))))
	   (foci~compute-proof-context-from-pds (foci~lineorder foci*proof-context)
						(foci~open-foci foci*proof-context)
						pds*current-proof-plan)))

(defun foci~find-support (predicate &optional (obj foci*proof-context))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "The list of all support-lines in the active focus of obj, which"
		    "fulfill the predicate."))
  (mapcan #'(lambda (x)
	      (when (ignore-errors (apply predicate (list x))) (list x)))
	  (let ((active-focus (foci~active-focus obj)))
	    (when active-focus (foci~supports active-focus)))))

(defun foci~find-open (predicate &optional (obj foci*proof-context))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A predicate and optionally a focus object, i.e. an instance of"
		    "foci+proof-context.")
	   (effect  "None.")
	   (value   "1. A list with the focussed line of the open active focus in obj,"
		    "iff this line fulfills the predicate, nil otherwise"
		    "2. A list of all open focussed lines of other foci, which also"
		    "fulfill the predicate."))
  (let ((arg1 (when (ignore-errors
		      (apply predicate
			     (list (foci~focus-line (foci~active-focus obj)))))
		(list (foci~focus-line (foci~active-focus obj)))))
	(arg2 (mapcan #'(lambda (x)
			  (when (ignore-errors (apply predicate (list (foci~focus-line x))) (list x))))
		      (rest (foci~open-foci obj)))))
    (values arg1 arg2)))


(defun foci=compute-application-type (previous-open previous-closed in-outline out-outline)
  (declare (edited  "15-APR-1998")
	   (authors Chris)
	   (input   "The previous open nodes, the previous closed nodes and two outlines"
		    "specifying the application of a rule or tactic.")
	   (effect  "None")
	   (value   "The type of the application (:one-focus-closed,"
		    ":one-focus-backward-linear, :one-focus-backward-split,"
		    ":one-focus-forward, :multi-focus-closed, :multi-focus-backward-linear,"
		    ":multi-focus-backward-split)"
		    "and a list of the newly introduced open nodes"
		    "and the newly introduced supports."))
;  (labels ((new-in-outline-with-respect-to
;           (outline1 outline2)
;           (if outline2
;               (multiple-value-bind (new-open new-supports new-hyps)
;                   (new-in-outline-with-respect-to (cdr outline1) (cdr outline2))
;                 (if (car outline2) ;; not new
;                     (values new-open new-supports new-hyps)
;                   (if (pdsn~open-node-p (car outline1)) ;; new
;                       (values (cons (car outline1) new-open) new-supports new-hyps)
;                     (values new-open (cons (car outline1) new-supports) new-hyps))))
;             (values nil nil outline1))))
;    (multiple-value-bind (later-open later-closed later-new-hyps)
;        (new-in-outline-with-respect-to out-outline in-outline)
  (declare (ignore previous-closed))  ;; vielleicht brauch ich das aber doch noch mal
  (let* ((prev-open (remove-duplicates previous-open :test #'keim~equal))
	 (later-open (set-difference
		     (mapcan #'(lambda (x) (when (and (pdsn~p x)
						      (pdsn~open-node-p x))
					     (list x)))
			     out-outline)
		     prev-open
		     :test #'(lambda (x y) (equal (keim~name x) (keim~name y)))))
	 (later-new-hyps (subseq out-outline (length in-outline))))
    (declare (ignore later-closed))
    (cond ((and (= (length prev-open) 1) 
		(not (mapcan #'(lambda (x) (when (and (pdsn~p x)
						      (pdsn~open-node-p x))
					     (list x)))
			     out-outline)))
	   (values :one-focus-closed nil later-new-hyps))
	  ((and (= (length prev-open) 1) (= (length later-open) 1))
	   (values :one-focus-backward-linear later-open later-new-hyps))
	  ((and (= (length prev-open) 1) (> (length later-open) 1))
	   (values :one-focus-backward-split later-open later-new-hyps))
	  ((and (= (length prev-open) 0))
	   (values :one-focus-forward nil later-new-hyps))
	  ((and (> (length prev-open) 1) (= (length later-open) 0))
	   (values :multi-focus-closed nil later-new-hyps))
	  ((and (> (length prev-open) 1) (= (length later-open) 1))
	   (values :multi-focus-backward-linear later-open later-new-hyps))
	  ((and (> (length prev-open) 1) (> (length later-open) 1))
	   (values :multi-focus-backward-split later-open later-new-hyps))
	  (t (values :mysterious later-open later-new-hyps)))))
	      
(defun foci~initialize (theorem-line assumption-lines)
   (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A theorem line and a list of assumption lines.")
	   (effect  "Binds the global variable foci*proof-context to the"
		    "initial proof-context, defined by the theorem line and the"
		    "assumption lines.")
	   (value   "The updated object."))
   (let* ((thm-line (if (listp theorem-line)
			(car theorem-line)
		      theorem-line))
	 (focus (foci~focus-create assumption-lines thm-line nil)))
     (setf foci*proof-context
	   (foci~proof-context-create focus (cons thm-line assumption-lines)
				      (list focus)))))


(defun foci=supports-of-node (node)
  (declare (edited  "16-APR-1998")
	   (authors Chris)
	   (input   "A proofline.")
	   (effect  "None")
	   (value   "Computes the supports of a node."))
  (pds~node-supports node))

(defun foci~update (previous-open previous-closed in-outline out-outline &optional (obj foci*proof-context))
   (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "a list of previously open nodes and a list of previously closed"
		    "nodes and two outlines, specifying the"
		    "application of a rule or tactic and a optional focus-object,"
		    "i.e. an instance of foci+proof-context and an ")
	   (effect  "Updates the active focus of OBJ, i.e. destructively changes"
		    "the object of type foci+focus in case of an forward application"
		    "and a linear backward application of a rule/tactic,"
		    "or replaces it by an object of type foci+context"
		    "in case of a splitting backward application.")
	   (value   "The updated object."))
   (multiple-value-bind (type new-open new-supports)
       (foci=compute-application-type previous-open previous-closed in-outline out-outline)
     (let ((new-obj (foci=update obj type new-open new-supports)))
       (unless (foci~check new-obj)
	 (setf new-obj (foci~compute-proof-context-from-pds
			(foci~lineorder obj)
			(foci~open-foci obj)
			pds*current-proof-plan)))
       (setf foci*proof-context new-obj)
       (setf (pds~proof-context pds*current-proof-plan) new-obj)
       (foci~suggest-commands pds*current-proof-plan)
       obj)))
     

(defgeneric foci~suggest-commands (pds)
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A proof plan.")
	   (effect  "None.")
	   (value   "some suggested commands. (This function is redefined in omega."))
  (:method (pds)
	   (declare (ignore pds))
	   nil))




;(defun foci~update (previous-open previous-closed in-outline out-outline &optional (obj foci*proof-context)) obj)

(defgeneric foci=update (obj type new-open new-supports &optional (lineorder (foci~lineorder
									      foci*proof-context)))
  (declare (edited  "14-APR-1998")
	   (authors Chris)
	   (input   "A focus-object, i.e. an instance of foci+focus-context or"
		    "foci+proof-context, a application type (:one-focus-closed,"
		    ":one-focus-backward-linear, :one-focus-backward-split,"
		    ":one-focus-forward, :multi-focus-closed, :multi-focus-backward-linear,"
		    ":multi-focus-backward-split),"
		    "a list of the newly introduced open nodes,"
		    "a list of new supports and optionally a lineorder")
	   (effect  "Updates the active focus of OBJ, i.e. destructively changes"
		    "the object of type foci+focus in case of an forward application"
		    "and a linear backward application of a rule/tactic,"
		    "or replaces it by an object of type foci+context"
		    "in case of a splitting backward application."
		    "Furthermore the new supports have are correctly inserted in"
		    "all foci they belong to.")
	   (value   "The updated object."))
  (:method ((obj foci+focus) type new-open new-supports &optional (lineorder (foci~lineorder
									      foci*proof-context)))
	   (cond ((equal type :update-supports)
		  (setf (foci~supports obj)
			(when new-open
			  (foci=order-with-respect-to (pds~node-supports (car new-open))
						      lineorder)))
		  obj)
		 ((equal type :one-focus-closed)
		  (setf (foci~status obj) :closed)
		  obj)
		 ((equal type :one-focus-backward-linear)
		  (setf (foci~descendants obj) (cons (foci~focus-line obj)
						     (foci~descendants obj)))
		  (setf (foci~focus-line obj) (car new-open))
		  (setf (foci~supports obj)
			(foci=order-with-respect-to (pds~node-supports (car new-open))
						    lineorder))
		  obj)
		 ((equal type :one-focus-backward-split)
		  (error "This foci update case should not occur."))
		 ((equal type :one-focus-forward)
		  (setf (foci~supports obj)
			(foci=order-with-respect-to (pds~node-supports (foci~focus-line obj))
						    lineorder)))
		 ((equal type :multi-focus-closed)
		  (foci=update obj :one-focus-closed new-open  new-supports))
		 ((equal type :multi-focus-backward-linear)
		  (foci=update obj :one-focus-backward-linear new-open  new-supports))
		 ((equal type :multi-focus-backward-split)
		  (foci=update obj :one-focus-backward-split new-open  new-supports))
		 (t (error "Unknown application type ~A in function foci=update"
			   type))))
  (:method ((obj foci+context) type new-open new-supports &optional (lineorder (foci~lineorder
										foci*proof-context)))
	   (declare (ignore type new-open new-supports lineorder))
	   obj)
  (:method ((obj foci+proof-context) type new-open new-supports &optional (lineorder
									   (foci~lineorder
									    foci*proof-context))) 
	   (setf (foci~lineorder obj) (append new-open new-supports lineorder))
	   (if (not (foci~active-focus obj))
	       obj
	     (cond ((equal type :one-focus-closed)
		    (foci=update (foci~active-focus obj) type new-open new-supports)
		    (mapcar #'(lambda (x) (foci=update x :update-supports nil nil))
			    (cdr (foci~open-foci obj)))
		    (setf (foci~open-foci obj) (remove (foci~active-focus obj) (foci~open-foci obj)))
		    obj)
		   ((equal type :one-focus-backward-linear)
		    (foci=update (foci~active-focus obj) type new-open new-supports)
		    (mapcar #'(lambda (x) (foci=update x :update-supports nil nil))
			    (cdr (foci~open-foci obj)))
		    obj)
		   ((equal type :one-focus-backward-split)
		    (foci~compute-proof-context-from-pds (foci~lineorder obj)
							 (foci~open-foci obj)
							 pds*current-proof-plan))
		   ((equal type :one-focus-forward)
		    (foci=update (foci~active-focus obj) type new-open new-supports)
		    (mapcar #'(lambda (x) (foci=update x :update-supports nil nil))
			    (cdr (foci~open-foci obj)))
		    obj)
		   ((equal type :multi-focus-closed)
		    (foci=update (foci~active-focus obj) type new-open new-supports)
		    (mapcar #'(lambda (x) (foci=update x :update-supports nil nil))
			    (cdr (foci~open-foci obj)))
		    (setf (foci~open-foci obj)
			  (remove-if #'(lambda (x) (not (and
							 (pdsn~p (foci~focus-line x))
							 (pdsn~open-node-p (foci~focus-line x)))))
				     (foci~open-foci obj)))
		    obj)
		   ((equal type :multi-focus-backward-linear);; Keine Ahnung ob das hier reicht
		    (foci=update (foci~active-focus obj) type new-open new-supports)
		    (mapcar #'(lambda (x) (foci=update x :update-supports nil nil))
			    (cdr (foci~open-foci obj)))
		    (setf (foci~open-foci obj)
			  (append
			   new-open
			   (remove-if #'(lambda (x) (not
						     (and
						      (pdsn~p (foci~focus-line x))
						      (pdsn~open-node-p (foci~focus-line
									 x)))))
				      (foci~open-foci obj))))
		    obj)
		   ((equal type :multi-focus-backward-split);; Keine Ahnung ob das hier reicht
		    (foci~compute-proof-context-from-pds (foci~lineorder obj)
							 (foci~open-foci obj)
							 pds*current-proof-plan))
		   ((equal type :mysterious)
		    (foci~compute-proof-context-from-pds (foci~lineorder obj)
							 (foci~open-foci obj)
							 pds*current-proof-plan))
		   (t (error "Unknown application type ~A in function foci~~update"
			     type))))))


(defun foci=order-with-respect-to (list1 list2 &optional new-in-list1 reverse-ordered)
  (declare (edited  "15-APR-1998")
	   (authors Chris)
	   (input   "Two lists and two optional lists.")
	   (effect  "None")
	   (value   "reordered list1, whereas list1 ist ordered with respect to"
		    "the ordering described by list2. Elements of list1 which do"
		    "not ocuur in list2 are put in front of the result list."))
  (if list2
      (if (find (car list2) list1)
	  (foci=order-with-respect-to list1 (cdr list2) new-in-list1
				      (cons (car list2) reverse-ordered))
	(foci=order-with-respect-to list1 (cdr list2) new-in-list1 reverse-ordered))
    (if list1
	(if (find (car list1) reverse-ordered)
	    (foci=order-with-respect-to (cdr list1) list2 new-in-list1 reverse-ordered)
	  (foci=order-with-respect-to (cdr list1) list2 (cons (car list1) new-in-list1)
				      reverse-ordered))
      (append (reverse new-in-list1) (reverse reverse-ordered)))))

(defun foci=is-ordered-with-respect-to (list1 list2)
  (declare (edited  "15-APR-1998")
	   (authors Chris)
	   (input   "Two lists and two optional lists.")
	   (effect  "None")
	   (value   "T, if list1 is ordered with respect to the ordering specified in"
		    "list2."))
  (equal list1 (foci=order-with-respect-to list1 list2)))


(defgeneric foci=embedded-nodes (obj)
  (declare (edited  "14-APR-1998")
           (authors Chris)
           (input   "A object of type pdsfoci+proof-context, foci+context or foci+focus")
           (effect  "None.")
           (value   "The list of the embedded prooflines"))
  (:method ((obj foci+proof-context))
	   (foci=embedded-nodes (foci~focus-context obj)))
  (:method ((obj foci+context))
	   (let ((focilist (mapcan #'(lambda (x) (copy-list (foci=embedded-nodes x)))
				   (foci~subcontexts obj))))
	     (remove-duplicates
	      (append
	       focilist
	       (append 
		(list (foci~focus-line obj))
		(foci~descendants obj))))))
  (:method ((obj foci+focus))
	   (append (foci~descendants obj) (list (foci~focus-line obj)) (foci~supports obj))))
	   

(defun foci~compute-proof-context-from-pds (partial-lineorder partial-foci-order pds
							      &optional suggest-commands)
  (declare (edited  "14-APR-1998")
           (authors Chris)
           (input   "A partial lineorder (list of lines), a partial foci order (list"
		    "of foci) and a proof data structure and optionally a flag suggest-commands.")
           (effect  "Destructively changes the proof-context of the pds by replacing"
		    "it by the newly computed one."
		    "Furthermore sets the global variable foci*proof-context.")
           (value   "The associated proof-context, whereas the line"))

	  (let ((orig-theorem (pds~label2node (keim~name (prob~conclusion pds)))))
	    (multiple-value-bind (new-focus-context embedded-lines embedded-foci)
				 (foci=compute-focus-context orig-theorem pds
							     partial-lineorder)
	      (let ((new-proof-context
		     (foci~proof-context-create
		      new-focus-context
		      (foci=order-with-respect-to embedded-lines partial-lineorder)
		      (foci=order-foci-with-respect-to embedded-foci
						       partial-foci-order))))

		(setf (pds~proof-context pds) new-proof-context)
		(setf foci*proof-context new-proof-context)
		(format t "~%  Check new proof context: ~A ~%" (foci~check
								new-proof-context))
		(when suggest-commands (foci~suggest-commands pds*current-proof-plan))
		new-proof-context))))


(defun foci=compute-focus-context (line pds lineorder &optional descendants)
  (cond ((foci=open-node line)
	 (let* ((supports (foci=order-with-respect-to
			   (foci=supports-of-node line)
			   lineorder))
		(focus (foci~focus-create supports line descendants)))
	   (setf (foci~status focus) :open)
	   (values focus (append supports (list line) descendants) (list focus))))
	((foci=split-node line)
	 (let* ((split-nodes (foci=split-node line))
		(subcontexts nil)
		(embed-lines nil)
		(embed-foci nil))
	   (dolist (node split-nodes nil)
	     (multiple-value-bind (new-focus-context embedded-lines embedded-foci)
		 (foci=compute-focus-context node pds lineorder)
	       (setf subcontexts (cons new-focus-context subcontexts))
	       (setf embed-lines (append embedded-lines embed-lines))
	       (setf embed-foci (append embedded-foci embed-foci))))
	   (setf embed-lines (remove-duplicates embed-lines))
	   (let ((new-focus (foci~context-create subcontexts line descendants)))
	     (setf (foci~status new-focus) :closed)
	     (values new-focus (append embed-lines (list line) descendants) embed-foci))))
	((foci=linear-node line)
	 (let ((father-node (foci=linear-node line)))
	   (foci=compute-focus-context father-node pds lineorder (cons line descendants))))
	((foci=hyp-assu-node line)
	 (let ((new-focus (foci~focus-create nil line descendants)))
	   (setf (foci~status new-focus) :closed)
	   (values new-focus (cons line descendants) nil)))
	(t (error "Something is wrong with line ~A with respect to the focus-mechanism." line))))


(defun foci=order-foci-with-respect-to (foci-list partial-foci-order &optional new-in-foci-list reverse-ordered)
    (declare (edited  "15-APR-1998")
	   (authors Chris)
	   (input   "Two foci-lists and two optional lists.")
	   (effect  "None")
	   (value   "reordered foci-list, whereas foci-list ist ordered with respect to"
		    "the ordering described by partial-foci-order. Elements of foci-list which do"
		    "not ocuur in partial-foci-order are put in front of the result list."))
  (if partial-foci-order 
      (if (find (car partial-foci-order ) foci-list :test #'(lambda (x y) (keim~equal (foci~focus-line x) (foci~focus-line y))))
	  (foci=order-foci-with-respect-to foci-list (cdr partial-foci-order ) new-in-foci-list
				      (cons (car foci-list ) reverse-ordered))
	(foci=order-foci-with-respect-to foci-list (cdr partial-foci-order ) new-in-foci-list reverse-ordered))
    (if foci-list
	(if (find (car foci-list) reverse-ordered :test #'(lambda (x y) (keim~equal (foci~focus-line x) (foci~focus-line y))))
	    (foci=order-foci-with-respect-to (cdr foci-list) partial-foci-order  new-in-foci-list reverse-ordered)
	  (foci=order-foci-with-respect-to (cdr foci-list) partial-foci-order (cons (car foci-list) new-in-foci-list)
				      reverse-ordered))
      (append (reverse new-in-foci-list) (reverse reverse-ordered)))))




(defun foci=open-node (node)
  (and (pdsn~p node) (pdsn~open-node-p node)))

(defun foci=linear-node (node)
  (when (and (pdsn~p node) (not (pdsn~open-node-p node)) (= 1 (length (just~premises (node~justification node)))))
    (car (just~premises (node~justification node)))))

(defun foci=split-node (node)
  (when (and (pdsn~p node) (not (pdsn~open-node-p node)) (> (length (just~premises
								     (node~justification
								      node)))
							    1))
    (just~premises (node~justification node))))

(defun foci=hyp-assu-node (node)
  (and (pdsn~p node) (not (pdsn~open-node-p node)) (= 0 (length (just~premises (node~justification node))))))
	



  
(defgeneric foci~check (obj &optional (lineorder (foci~lineorder foci*proof-context))  open-foci)
  (declare (edited  "14-APR-1998")
           (authors Chris)
           (input   "A object of type foci+proof-context, foci+context or foci+focus")
           (effect  "None.")
           (value   "T, if the focus structure is consistent, nil otherwise."))
  (:method ((obj foci+proof-context) &optional (lineorder (foci~lineorder foci*proof-context))  open-foci)
;	   (format t "~%Checking ~A" obj)
	   (and
	    (or (every #'(lambda (x) (and (pdsn~p (foci~focus-line x))
					  (pdsn~open-node-p (foci~focus-line x))))
		       (foci~open-foci obj))
		(progn (format t "~%Code 1") nil))
	    (or (subsetp (foci=order-with-respect-to (foci=embedded-nodes obj) lineorder) lineorder)
		(progn (format t "~%Code 19") nil))
	    (foci~check (foci~focus-context obj)
			(if lineorder lineorder
			  (foci~lineorder obj))
			(if open-foci open-foci
			  (foci~open-foci obj)))))
  (:method ((obj foci+context) &optional (lineorder (foci~lineorder foci*proof-context))  open-foci)
	   (format t "~%Checking ~A" obj)
	   (and
	    (or (not (and (foci~focus-p obj)
			  (pdsn~p (foci~focus-line obj))
			  (pdsn~open-node-p (foci~focus-line obj))))
		(progn (format t "~%Code 2") nil))
	    (or (equal (foci~status obj) :closed)
		(progn (format t "~%Code 3") nil))
	    (or (every #'(lambda (x) (not (and (pdsn~p x)
					       (pdsn~open-node-p x))))
		       (foci~descendants obj))
		(progn (format t "~%Code 4") nil))
	    (or (every #'(lambda (x) (foci~check x lineorder open-foci))
		       (foci~subcontexts obj))
		(progn (format t "~%Code 5") nil))))
  (:method ((obj foci+focus) &optional (lineorder (foci~lineorder foci*proof-context))  open-foci)
	   (format t "~%Checking ~A" obj)
	   (or
	    (and
	     (or (not (and (pdsn~p (foci~focus-line obj))
			   (pdsn~open-node-p (foci~focus-line obj))))
		 (progn (format t "~%Code 6") nil))
	     (or (equal (foci~status obj) :closed)
		 (progn (format t "~%Code 7") nil))
	     (or (not (find obj open-foci))
		 (progn (format t "~%Code 8") nil))
	     (or (every #'(lambda (x) (not (and (pdsn~p x)
						(pdsn~open-node-p x))))
			(foci~descendants obj))
		 (progn (format t "~%Code 9") nil))
	     (or (every #'(lambda (x) (not (and (pdsn~p x)
						(pdsn~open-node-p x))))
			(foci~supports obj))
		 (progn (format t "~%Code 10") nil))
	     (or (foci=is-ordered-with-respect-to (foci~supports obj) lineorder)
		 (progn (format t "~%Code 11") nil)))
	    (and
	     (or (pdsn~p (foci~focus-line obj))
		 (progn (format t "~%Code 12") nil))
	     (or (pdsn~open-node-p (foci~focus-line obj))
		 (progn (format t "~%Code 13") nil))
	     (or (equal (foci~status obj) :open)
		 (progn (format t "~%Code 14") nil))
	     (or (find obj open-foci)
		 (progn (format t "~%Code 15") nil))
	     (or (every #'(lambda (x) (not (and (pdsn~p x)
						(pdsn~open-node-p x))))
			(foci~descendants obj))
		 (progn (format t "~%Code 16") nil))
	     (or (every #'(lambda (x) (not (and (pdsn~p x)
						(pdsn~open-node-p x))))
			(foci~supports obj))
		 (progn (format t "~%Code 17") nil))
	     (or (foci=is-ordered-with-respect-to (foci~supports obj) lineorder)
		 (progn (format t "~%Code 18") nil))))))


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

(defmethod infer~compute-outline :around ((tactic infer+inference) (outline list)
					  (parameters list))
  (let ((line-order (when (foci~in-use) (foci~lineorder foci*proof-context)))
	(foci-order (when (foci~in-use) (foci~open-foci foci*proof-context))))
    (multiple-value-bind (new-outline success)
	(call-next-method)
      (when (and success (foci~in-use))
	(foci~compute-proof-context-from-pds line-order foci-order pds*current-proof-plan))
    (values new-outline success))))


(defmethod infer~apply-expansion-function :around ((method infer+inference) (outline list)
						   (parameters list))
  
  (let ((line-order (when (foci~in-use) (foci~lineorder foci*proof-context)))
	(foci-order (when (foci~in-use) (foci~open-foci foci*proof-context)))
        (result (call-next-method)))
    (when (foci~in-use)
      (foci~compute-proof-context-from-pds line-order foci-order pds*current-proof-plan))
    result))


