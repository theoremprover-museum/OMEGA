;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package "KEIM")

(mod~defmod bind :uses (keim mod)
            :documentation "Mechanism for handling binding contexts."
            :exports (
                      bind~bind
		      bind~binding
		      bind~with-bindings
		      bind~insert-bindings!

		      bind~local-substitution
		      bind~global-substitution
		      )
	    )

#{ \section{bind}\label{bind:bind}

This module provides a mechanism for handling binding contexts.
The macro {\tt bind$\sim$with-bindings} creates a new context in wich variables may be bound to
terms.
This new context can shadow some older context in the sense that the older
binding of a variable becomes invisible if the same variable is bound to another term
in the new context.
Nevertheless the older binding is not lost because after leaving the {\tt bind$\sim$with-bindings} macro
the former context is restored again.
Arbitrarily nested variable contexts are possible with {\tt bind$\sim$with-bindings}.
The macro returns the new-built variable context as a substituion.
The function {\tt bind$\sim$bind} binds a variable to a term in the actual context
while the function {\tt bind$\sim$binding} supplies the binding term of a variable in respect to the
correct context.

#}



;;;
;;; global variables
;;;

(defvar bind*binding-stack nil)         ; obsolete, remove it!
(defvar bind*enable-stack nil)          ; 
(defvar bind*label-tab nil)             ; In this table we have a mapping from labels to lists of contexts
					; labelled with the respective label.
(defvar bind*btp-tab nil)               ;
(defvar bind*key-table (make-hash-table)) ;

;;; bind*current-stack: see below, after definition of bind=context-stack-create



;;; A mixin class bind+context+mixin we use to build generalizated substitutions and mappings which
;;; serves as contexts.

(eval-when (load compile eval)
(defclass bind+context+mixin ()
  ((pred :initform nil :initarg :pred :accessor bind=pred-context))
  (:documentation "The mixin class for the construction of contexts.")))


;;;
;;; contexts are generalized substitutions or mappings:
;;;
;;;     mapp+mapping  bind+context-mixin
;;;            /\        /|
;;;           /  \      / |
;;;          /    \    /  |
;;;         /      \  /   |
;;;   subst+subst   \/    |
;;;        \        /\    |
;;;         \      /  \   |
;;;          \    /    \  |
;;;           \  /      \ |
;;;            \/        \|
;;;    bind+context   bind+mapping
;;;

(eval-when (load compile eval)
(defclass bind+context (subst+substitution bind+context+mixin)
  ((clabel :initform nil :initarg :clabel :accessor bind=clabel)
   (home-stack :initform nil :initarg :stack-of :accessor bind=stack-of))
  (:documentation "The class of substitutions.")))

(eval-when (load compile eval)
(defclass bind+mapping (mapp+mapping bind+context+mixin)
  ((home-stack :initform nil :initarg :stack-of :accessor bind=stack-of))
  (:documentation "The class of binding mappings.")))

(eval-when (load compile eval)
(defclass bind+context-stack ()
  ((stack :initform nil :initarg :stack :accessor bind=stack)
   (pred-stack :initform nil :initarg :pred-stack :accessor bind=pred-stack)
   (pred-ctx :initform nil :initarg :pred-ctx :accessor bind=pred-ctx)
   (key-table :initform nil :initarg :key-table :accessor bind=key-table))
  (:documentation "The class of context stacks.")))





(defun bind=context-create (domain codomain pred &optional (stack bind*current-stack))
  (declare (edited  "11-AUG-1995 12:12")
	   (authors GKLEIN)
	   (input   "A list of variables and a list of terms and a predecessor context.")
	   (effect  "None.")
	   (value   "The new context, i.e. a generalized substitution with the domain DOMAIN,"
		    "the codomain CODOMAIN  and the predecessor context PRED."))
  (let ((new-context (change-class (subst~create domain codomain) 'bind+context)))
    (setf (bind=pred-context new-context) pred)
    (setf (bind=stack-of new-context) stack)
    new-context))

(defun bind=context-stack-create (pred-context)
  (declare (edited  "11-AUG-1995 12:12")
	   (authors GKLEIN)
	   (input   "A context.")
	   (effect  "None.")
	   (value   "A new and empty stack of contexts. The new stack has a pointer to the CONTEXT."))
  (let ((newstack (make-instance 'bind+context-stack :stack nil :key-table nil
				 :pred-ctx pred-context :pred-stack (when pred-context
								      (bind=stack-of (car pred-context))))))
    (setf (bind=stack newstack) (list (bind=context-create nil nil nil newstack)))
    newstack))

(defvar bind*current-stack (bind=context-stack-create nil))        ; contains the actually active stack object 


(defun bind=mapping-create (domain codomain pred)
  (let ((new-mapping (change-class (mapp~create domain codomain) 'bind+mapping)))
    (setf (bind=pred-context new-mapping) pred)
    (setf (bind=stack-of new-mapping) bind*current-stack)
    new-mapping))




(defun bind=next-pred ()
  (declare (edited  "18-AUG-1995 11:33")
	   (authors GKLEIN)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The next predecessor list that would be needed when a new context"
		    "has to be added to the stack of contexts."))
;;; The new predecessor list is the concatenation of the top-context with the predecessor list of the top-context.
  (let ((top-context (bind=current-context)))
    (cond ((null top-context)
	   nil)
	  (t
	   (cons top-context (bind=pred-context top-context))))))

(defun bind=next-mapp-pred (key)
  (let ((top-mapping (first (bind=key key))))
    (when top-mapping
      (cons top-mapping (bind=pred-context top-mapping)))))
	  


(defun bind=current-mapping (key)
  (first (bind=key key)))



;;; lack of comments for BTP argument
(defmacro bind~with-bindings ((&body forms) &key (btp nil) (label nil) (enable nil)
			      (insert t) (build-subst t))
  (declare (edited  "10-AUG-1995 18:33")
	   (authors GKLEIN)
	   (input   "A body of forms, and optionally a label")
	   (effect  "This macro creates a new binding context in which FORMS will be evaluated."
		    "After the FORMS are evaluated the old binding context is present again."
		    "If a LABEL is given as argument to this macro, then the new context gets this LABEL.")
	   (value   #{\begin{enumerate}
		    \item If the last form in FORM evaluates to a data object, then the data object
		    with the inserted bindings is returned. If it does not evaluate to a data object,
		    then the object itself will be returned.\\
		    If the optional keyword paramater INSERT is NIL, then the data object will be returned
		    without inserted bindings.
		    \item The substitution induced by the binding context.
		    \item If a LABEL is given as argument, then a substitution is returned which
		    results from the concatenation of all older binding contexts with the same label
		    LABEL. If no label is given as argument, then NIL will be returned.
		    \end{enumerate}#}))
  `(unwind-protect
       (progn (unless bind*current-stack
		(setf bind*current-stack (bind=context-stack-create nil)))
	      (bind=init ,enable ,label ,btp)
	      (let* ((result (if ,insert
				 (bind~insert-bindings! (progn ,@forms))
			       (progn ,@forms)))
		     (subst (if ,build-subst
				(subst~create (subst~domain (bind=current-context))
					      (subst~codomain (bind=current-context)))
			      (subst~create nil nil)))
		     (label-subst (when ,label
				    (bind=label-subst ,label))))
		(values result subst label-subst)))
     (progn (bind=exit ,enable ,label ,btp)
	    (when (and (null (bind=pred-ctx bind*current-stack))
		       (null (bind=stack bind*current-stack)))
	      (setf bind*current-stack nil)))))


(defun bind=current-enable ()
  (first bind*enable-stack))



(defgeneric bind=init (enable label btp)
  (declare (edited  "02-OCT-1996")
	   (authors gklein Fehrer)
	   (input   "an enable-list, optionally a label and a backtrack-point")
	   (effect  "")
	   (value   "undefined"))
  (:method ((enable list) label btp)
	   (push enable bind*enable-stack)
	   (bind=init 'binding label btp)
	   (mapc #'(lambda (enable-elem)
		     (bind=init enable-elem label btp))
		 enable)
	   (bind=label label)
	   (if btp
	     (bind=btp btp)))
  (:method ((key (eql 'binding)) label btp)
	   (declare (ignore label btp))
	   (let ((new-context (bind=context-create nil nil (bind=next-pred))))
	     (push new-context (bind=stack bind*current-stack))))
  (:method (key label btp)
	   (declare (ignore label btp))
	   (declare (special bind*key-table))
	   (unless (bind=key-exists-p key)
	     (bind=key-create key))
	   (let ((new-mapping (bind=mapping-create nil nil (bind=next-mapp-pred key))))
	     (setf (gethash key bind*key-table) (cons new-mapping (gethash key bind*key-table))))))



(defgeneric bind=exit (enable label btp)
  (:method ((enable list) label btp)
	   (mapc #'(lambda (enable-elem)
		     (bind=exit enable-elem label btp))
		 enable)
	   (bind=exit 'binding label btp)
	   (pop bind*enable-stack)
	   (when label
	     (bind=pop-label-context label))
	   (when btp
	     (bind=pop-btp-context btp)))
  (:method ((key (eql 'binding)) label btp)
	   (declare (ignore label btp))
	   (pop (bind=stack bind*current-stack)))
  (:method (key label btp)
	   (declare (ignore label btp))
	   (declare (special bind*key-table))
	   (setf (gethash key bind*key-table) (cdr (gethash key bind*key-table)))))


;;;
;;;
;;;
;Diese Funktion wurde ersetzt durch nachstehende welche
;(defgeneric bind~bind (var thing &key key)
;  (declare (edited  "10-AUG-1995 18:57")
;           (authors GKLEIN)
;           (input   "A variable VAR and a THING.")
;           (effect  "The binding VAR -> THING will be inserted in the current binding context.")
;           (value   "undefined."))
;  (:method ((varlist list) (thinglist list) &key key)
;           (mapc #'(lambda (var thing)
;                     (bind~bind var thing :key key))
;                 varlist thinglist))
;  (:method ((var data+variable) thing &key (key nil))
;           (bind=bind var thing key)))

(defgeneric bind~bind (var thing &key key)
  (declare (edited  "10-AUG-1995 18:57")
	   (authors GKLEIN)
	   (input   "A primitive VAR and a THING.")
	   (effect  "The binding VAR -> THING will be inserted in the current binding context.")
	   (value   "undefined."))
  (:method ((varlist list) (thinglist list) &key key)
	   (mapc #'(lambda (var thing)
		     (bind~bind var thing :key key))
		 varlist thinglist))
  (:method ((var data+variable) thing &key (key nil))
	   (bind=bind var thing key))
  (:method ((const data+constant) thing &key (key nil))
	   (bind=bind const thing key)))


;auch diese Funktion wurde abgeaendert in nachstehende welche
;(defgeneric bind=bind (var thing key)
;  (:method ((var data+variable) (thing data+struct) (key (eql nil)))
;           (let ((context (bind=current-context)))
;             (if context
;                 (progn
;                   (subst~insert-component! var thing context)
;                   (setf (data~binding var) (list context thing (bind=current-stack)))
;                   nil)
;               (error "you can bind variables only in binding contexts"))))
;  (:method ((var data+variable) thing key)
;           (if (bind=key-stack-not-empty-p key)
;               (let ((mapping (bind=current-mapping key)))
;                 (mapp~insert-component! var thing mapping)
;                 (unless (data~binding-table var)
;                   (setf (data~binding-table var) (make-hash-table)))
;                 (setf (gethash key (data~binding-table var)) (list mapping thing)))
;             (cond ((bind=key-exists-p key)
;                    (error "Try to execute a (bind~~bind ~A ~A :key ~A) outside from a appropriate bind~~with-binding for the key ~A."
;                           var thing key key))
;                   (t
;                    (error "unknown key ~A." key))))))



(defgeneric bind=bind (var thing key)
  (:method ((var data+primitive) (thing data+struct) (key (eql nil)))
	   (let ((context (bind=current-context)))
	     (if context
		 (progn
		   (subst~insert-component! var thing context)
		   (setf (data~binding var) (list context thing (bind=current-stack)))
		   nil)
	       (error "you can bind variables only in binding contexts"))))
  (:method ((var data+primitive) thing key)
	   (if (bind=key-stack-not-empty-p key)
	       (let ((mapping (bind=current-mapping key)))
		 (mapp~insert-component! var thing mapping)
		 (unless (data~binding-table var)
		   (setf (data~binding-table var) (make-hash-table)))
		 (setf (gethash key (data~binding-table var)) (list mapping thing)))
	     (cond ((bind=key-exists-p key)
		    (error "Try to execute a (bind~~bind ~A ~A :key ~A) outside from a appropriate bind~~with-binding for the key ~A."
			   var thing key key))
		   (t
		    (error "unknown key ~A." key))))))


(defun bind~binding (var &key (key nil))
  (declare (edited  "10-AUG-1995 19:37")
	   (authors GKLEIN)
	   (input   "A primitive VAR.")
	   (effect  "None.")
	   (value   "The binding of the variable VAR in respect to the current binding context."))
  (bind=binding var key))

;Auch diese Funktion wurde abgeaendert in nachstehende welche
;(defgeneric bind=binding (var key)
;  (:method ((var data+variable) (key (eql nil)))
;           (declare (special bind*binding-stack))
;           (if (and bind*current-stack
;                      (bind=current-context))
;               (let ((current-context (bind=current-context))
;                     (binding-context (first (data~binding var)))
;                     (binding-term (second (data~binding var)))
;                     (trusting-stack (third (data~binding var))))
;                 (cond ((and binding-context
;                             (bind=trust-p binding-context current-context trusting-stack))
;                        binding-term)
;                       (t
;                        (multiple-value-bind (val context)
;                            (bind=lookup-binding var (bind=current-stack))
;                          (setf (data~binding var) (list (car context) val (bind=current-stack)))
;                                        ; this effect will reduce the search the next time
;                          val))))
;             (error "no current context present!")))
;  (:method ((var data+variable) key)
;           (let ((current-mapping (bind=current-mapping key)))
;             (when (data~binding-table var)
;               (let* ((entry (gethash key (data~binding-table var)))
;                      (binding-mapping (first entry))
;                      (binding-plist (second entry)))
;                 (cond ((eq current-mapping binding-mapping)
;                        binding-plist)
;                       ((member binding-mapping (bind=pred-context current-mapping))
;                        binding-plist)
;                       (t
;                        (multiple-value-bind (val context)
;                            (mapp~lookup-mappings var (cons current-mapping (bind=pred-context current-mapping)))
;                          (setf (gethash key (data~binding-table var)) (list context val))
;                                        ; reduce the search the next time
;                          val))))))))

(defgeneric bind=binding (var key)
  (:method ((var data+primitive) (key (eql nil)))
	   (declare (special bind*binding-stack))
	   (if (and bind*current-stack
		      (bind=current-context))
	       (let ((current-context (bind=current-context))
		     (binding-context (first (data~binding var)))
		     (binding-term (second (data~binding var)))
		     (trusting-stack (third (data~binding var))))
		 (cond ((and binding-context
			     (bind=trust-p binding-context current-context trusting-stack))
			binding-term)
		       (t
			(multiple-value-bind (val context)
			    (bind=lookup-binding var (bind=current-stack))
			  (setf (data~binding var) (list (car context) val (bind=current-stack)))
					; this effect will reduce the search the next time
			  val))))
	     (error "no current context present!")))
  (:method ((var data+primitive) key)
	   (let ((current-mapping (bind=current-mapping key)))
	     (when (data~binding-table var)
	       (let* ((entry (gethash key (data~binding-table var)))
		      (binding-mapping (first entry))
		      (binding-plist (second entry)))
		 (cond ((eq current-mapping binding-mapping)
			binding-plist)
		       ((member binding-mapping (bind=pred-context current-mapping))
			binding-plist)
		       (t
			(multiple-value-bind (val context)
			    (mapp~lookup-mappings var (cons current-mapping (bind=pred-context current-mapping)))
			  (setf (gethash key (data~binding-table var)) (list context val))
					; reduce the search the next time
			  val))))))))



#{
\noindent{\bf Example:} We nest two binding contexts in the following code:
\begin{verbatim}
(bind~with-bindings
 ((bind~bind x a)
  (bind~bind y b)
  (bind~with-bindings
   ((bind~bind x b)
    (format t "~%Inner binding: x = ~A" (bind~binding x))))
  (format t "~%Outer binding: x = ~A" (bind~binding x))))
\end{verbatim}

This gives us the output:
\begin{verbatim}
Inner binding: x = B
Outer binding: x = A
NIL
{(Y --> B) (X --> A)}
NIL
\end{verbatim}

If the last form in {\vb bind~with-bindings} is a data object, then the bindings will be inserted for
the variables in the object, and this modified object will be returned as the first value of the macro:

\begin{verbatim}
(bind~with-bindings
 ((bind~bind x a)
  (bind~bind y b)
  (bind~with-bindings
   ((bind~bind x b)
    (format t "~%Inner binding: x = ~A" (bind~binding x))))
  (format t "~%Outer binding: x = ~A" (bind~binding x))
  y))
\end{verbatim}

results in:

\begin{verbatim}
Inner binding: x = B
Outer binding: x = A
B
{(Y --> B) (X --> A)}
NIL
\end{verbatim}
#}


;;;
;;; labels
;;;

(defun bind=label (label)
  (declare (edited  "02-MAY-1996 15:23")
	   (authors GKLEIN)
	   (input   "A label.")
	   (effect  "The actual context will be labelled with LABEL.")
	   (value   "undefined."))
  (setf (bind=clabel (bind=current-context)) label)
  (bind=add-labeled-context label (bind=current-context)))


(defun bind=all-labeled-contexts (label)
  (declare (edited  "02-MAY-1996 13:13")
	   (authors GKLEIN)
	   (input   "A label.")
	   (effect  "None.")
	   (value   "The list of all contexts labeled with LABEL."))
  (when label
    (cdr (assoc label bind*label-tab))))


(defun bind=add-labeled-context (label context)
  (declare (edited  "02-MAY-1996 13:22")
	   (authors GKLEIN)
	   (input   "A label and a context.")
	   (effect  "The CONTEXT will be added to the list of all contexts labeled with LABEL.")
	   (value   "undefined."))
  (when label
    (let ((tab-entry (assoc label bind*label-tab)))
      (if tab-entry
	  (push context (cdr tab-entry))
	(setf bind*label-tab (acons label (list context) bind*label-tab))))))
	
(defun bind=label-subst (label)
  (declare (edited  "02-MAY-1996 13:41")
	   (authors GKLEIN)
	   (input   "A label.")
	   (effect  "None.")
	   (value   "A substitution obtained from all contexts which are labeled LABEL."))
  (let* ((contexts (bind=all-labeled-contexts label))
	 (new-domain (apply #'append (mapcar #'subst~domain contexts)))
	 (new-codomain (apply #'append (mapcar #'subst~codomain contexts))))
    (subst~create new-domain new-codomain)))


(defun bind=pop-label-context (label)
  (declare (edited  "02-MAY-1996 13:59")
	   (authors GKLEIN)
	   (input   "A label.")
	   (effect  "removes the first context from the list of all context labelled with LABEL.")
	   (value   "The removed context."))
  (let ((tab-entry (assoc label bind*label-tab)))
    (if tab-entry
	(let ((context (pop (cdr tab-entry))))
	  (acons label tab-entry (remove label bind*label-tab :key #'car))
	  context))))


;;;
;;; backtracking points btp
;;;


(defun bind=btp (btp)
  (declare (edited  "24-MAY-1996 14:46")
	   (authors GKLEIN)
	   (input   "A backtracking point (a Lisp symbol).")
	   (effect  "The current context becomes a backtracking point.")
	   (value   "undefined."))
  (when btp
    (let ((tab-entry (assoc btp bind*btp-tab))
	  (context (bind=current-context)))
      (if tab-entry
	  (push context (cdr tab-entry))
	(setf bind*btp-tab (acons btp (list context) bind*btp-tab))))))


(defun bind=pop-btp-context (btp)
  (declare (edited  "24-MAY-1996 14:53")
	   (authors GKLEIN)
	   (input   "A backtracking point (a Lisp symbol).")
	   (effect  "removes the first context from the list of all context associated with BTP.")
	   (value   "The removed context."))
  (let ((tab-entry (assoc btp bind*btp-tab)))
    (if tab-entry
	(let ((context (pop (cdr tab-entry))))
	  (acons btp tab-entry (remove btp bind*btp-tab :key #'car))
	  context))))


#+OhneCbktrkWarDasGutGenug(defun bind=btp-subst (btp)
  (declare (edited  "24-MAY-1996 14:56")
	   (authors GKLEIN)
	   (input   "A backtracking point (a Lisp symbol).")
	   (effect  "None.")
	   (value   "A substitution obtained from all contexts which are associated with BTP."))
  (let* ((btp-context (second (assoc btp bind*btp-tab)))
	 (contexts (subseq (bind=current-context) 0 (1+ (position btp-context (bind=current-context)))))
	 (new-domain (apply #'append (mapcar #'subst~domain contexts)))
	 (new-codomain (apply #'append (mapcar #'subst~codomain contexts))))
    (subst~create new-domain new-codomain)))
    

(defun bind=btp-subst (btp)
  (declare (edited  "21-JUN-1996 13:57")
	   (authors GKLEIN)
	   (input   "A backtracking point (a Lisp symbol).")
	   (effect  "None.")
	   (value   "A substitution obtained from all contexts which are associated with
BTP."))
  (let* ((btp-context (second (assoc btp bind*btp-tab)))
	 (btp-stack-obj (bind=stack-of btp-context))
	 (current-stack (bind=current-context-list))
	 (current-stack-obj (bind=current-stack))
	 (contexts (bind=btp-contexts btp-context btp-stack-obj current-stack current-stack-obj))
	 (new-domain (apply #'append (mapcar #'subst~domain contexts)))
	 (new-codomain (apply #'append (mapcar #'subst~codomain contexts))))
    (subst~create new-domain new-codomain)))



(defun bind=btp-contexts (context1 stack-obj1 stack2 stack-obj2)
  (if (eq stack-obj1 stack-obj2)
      (subseq stack2 0 (1+ (position context1 stack2)))
    (let ((pred-stack-obj (bind=pred-stack stack-obj2))
	  (pred-stack (bind=pred-ctx stack-obj2)))
      (when pred-stack-obj
	(append stack2 (bind=btp-contexts context1 stack-obj1 pred-stack pred-stack-obj))))))

	 
;;;
;;;
;;;


(defun bind~local-substitution ()
  (declare (edited  "03-APR-1996 14:44")
	   (authors GKLEIN)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The current bindings as substitution."))
  (declare (special bind*binding-stack))
  (let ((context (bind=current-context)))
    (subst~create (mapp~domain context) (mapp~codomain context))))



(defun bind=global-label-substitution ()
  (declare (edited  "02-MAY-1996 17:13")
	   (authors GKLEIN)
	   (input   "None")
	   (effect  "None.")
	   (value   "A substitution containing all binding pairs from the binding contexts which are labelled with"
		    "the same label as the current binding context."))
  (let ((label (bind=clabel (bind=current-context))))
    (if label
	(bind=label-subst label)
      (bind~local-substitution))))

	
(defun bind~global-substitution (btp)
  (declare (edited  "24-MAY-1996 16:39")
	   (authors GKLEIN)
	   (input   "a backtracking point (a Lisp symbol).")
	   (effect  "None.")
	   (value   "A substitution containing all binding pairs from the binding contexts which are younger"
		    "as the context where the backtracking point have been installed."))
  (bind=btp-subst btp))


(defgeneric bind=lookup-binding (var contexts)
  (declare (edited  "14-AUG-1995 17:05")
	   (authors GKLEIN)
	   (input   "A variable and a list of contexts.")
	   (effect  "Set up the binding slot to the context in which VAR occurs in the domain"
		    "and the corresponding binding value.")
	   (value   "The first binding of VAR in the list of CONTEXTS,"
		    "or NIL if no binding for VAR exists."))
  (:method (var (contexts list))
	   (multiple-value-bind (term sub-contexts)
	       (bind=lookup-binding-contexts var contexts)
	     (cond (term
		    (values term sub-contexts))
		   ((bind=pred-stack (bind=stack-of (car contexts)))
		    (bind=lookup-binding var (bind=pred-ctx (bind=stack-of (car contexts)))))
		   (t
		    nil))))
  (:method (var (context-stack bind+context-stack))
	   (bind=lookup-binding var (bind=stack context-stack))))

 
(defun bind=lookup-binding-contexts (var contexts)
  (declare (edited  "14-JUN-1996 16:56")
	   (authors GKLEIN)
	   (input   "A variable and a list of contexts.")
	   (effect  "None.")
	   (value   "The first binding of VAR in the list of CONTEXTS,"
		    "or NIL if no binding for VAR exists."))
  (let ((context (first contexts)))
    (when context
      (let ((binding-term (subst~get-component var context)))
	(cond ((null binding-term)
	       (bind=lookup-binding-contexts var (cdr contexts)))
	      (t
	       (values binding-term contexts)))))))

;;; the following functions return nil, if no binding context is in effect!!!	   

(defun bind=current-context ()
  (declare (edited  "02-OCT-1996")
	   (authors Fehrer)
	   (input   "nothing")
	   (effect  "none")
	   (value   "the actual binding context, or nil, if none in effect"))
  (if bind*current-stack
      (first (bind=stack bind*current-stack))))

(defun bind=current-context-list ()
  (declare (edited  "02-OCT-1996")
	   (authors Fehrer)
	   (input   "nothing")
	   (effect  "none")
	   (value   "the contents of the actual binding stack, or nil, if none in effect"))
  (if bind*current-stack
      (bind=stack bind*current-stack)))

;;;
;;;
;;;



;;;
;;;
;;;

(defun bind~insert-bindings! (datum &key (local nil))
  (declare (edited  "23-SEP-1998")
	   (authors Gebhard)
	   (input   "A (list of) data object, optionally a flag LOCAL.")
	   (effect  "None.")
	   (value   "See bind=insert-bindings."))
  (bind=insert-bindings datum local nil))


;;
;; Version, die verwaltet welche Varaiblen schon ersetzt worden sind.
;;
(defgeneric bind=insert-bindings (datum local cycles)
  (declare (edited  "15-APR-1996 16:04")
	   (authors GKLEIN)
	   (input   "A (list of) data object, optionally a flag LOCAL.")
	   (effect  "None.")
	   (value   "A new data object where the bindings of all substructs of DATUM"
		    "are inserted into the datum recursively."
		    "If LOCAL is T, then the recursion will occur only for these variables"
		    "which are bounded in the top level binding context."))
  (:method (datum local cycles)
	   (declare (ignore local))
	   datum)
  (:method ((data list) local cycles)
	    (mapcar #'(lambda (datum)
			(bind=insert-bindings datum local cycles))
		    data))
  (:method ((datum data+constant)  local cucles)
	   (declare (ignore local))
	   datum)
  (:method ((var data+variable) local cycles)
	   (cond ((or (null (bind~binding var))
		      (find var cycles))
		  var)
		 (local
		  (if (bind=local-p var)
		      (bind=insert-bindings (bind~binding var) local (cons var cycles))
		    var))
		 (t
		  (bind=insert-bindings (bind~binding var) local (cons var cycles)))))
  (:method ((appl data+appl) local cycles)
	   ;;(let ((list-of-all-subterms (data~substructs appl)))
	   ;;  (if (intersection cycles list-of-all-subterms)
	   ;;	 appl
	       (data~appl-create (bind=insert-bindings (data~appl-function appl) local cycles)
				 (bind=insert-bindings (data~appl-arguments appl) local cycles)))
  ;; ))
  (:method ((scheme data+schema) local cycles)
	   (let ((kappas (mapcan #'(lambda (x)
					   (unless (bind~binding x)
					     (list x)))
				       (data~schema-kappa scheme))))
	     (if kappas
		 (data~schema-create (bind=insert-bindings (data~schema-datum scheme) local cycles)
				     kappas)
	       (bind=insert-bindings (data~schema-datum scheme) local cycles))))
  (:method ((abstr data+abstr) local cycles)  
	   ;;(let ((list-of-all-subterms (data~substructs abstr)))
	   ;;  (if (intersection cycles list-of-all-subterms)
	   ;;	 abstr
	       (data~abstr-create (bind=insert-bindings (data~abstr-domain abstr) local cycles)
				  (bind=insert-bindings (data~abstr-range abstr) local cycles)
				  )))
  ;; ))
                                   

;;
;; Version, die in abstraktionen nur ide Freien Variablen ersetzt
;;
(defgeneric bind=insert-bindings (datum local cycles)
  (declare (edited  "15-APR-1996 16:04")
	   (authors GKLEIN)
	   (input   "A (list of) data object, optionally a flag LOCAL.")
	   (effect  "None.")
	   (value   "A new data object where the bindings of all substructs of DATUM"
		    "are inserted into the datum recursively."
		    "If LOCAL is T, then the recursion will occur only for these variables"
		    "which are bounded in the top level binding context."))
  (:method (datum local cycles)
	   (declare (ignore local))
	   datum)
  (:method ((data list) local cycles)
	    (mapcar #'(lambda (datum)
			(bind=insert-bindings datum local cycles))
		    data))
  (:method ((datum data+constant)  local cucles)
	   (declare (ignore local))
	   datum)
  (:method ((var data+variable) local cycles)
	   (cond ((or (null (bind~binding var))
		      (find var cycles))
		  var)
		 (local
		  (if (bind=local-p var)
		      (bind=insert-bindings (bind~binding var) local cycles)
		    var))
		 (t
		  (bind=insert-bindings (bind~binding var) local cycles))))
  (:method ((appl data+appl) local cycles)
	   ;;(let ((list-of-all-subterms (data~substructs appl)))
	   ;;  (if (intersection cycles list-of-all-subterms)
	   ;;	 appl
	       (data~appl-create (bind=insert-bindings (data~appl-function appl) local cycles)
				 (bind=insert-bindings (data~appl-arguments appl) local cycles)))
  ;; ))
  (:method ((abstr data+abstr) local cycles)  
	   ;;(let ((list-of-all-subterms (data~substructs abstr)))
	   ;;  (if (intersection cycles list-of-all-subterms)
	   ;;	 abstr
	       (data~abstr-create (data~abstr-domain abstr)
				  (bind=insert-bindings (data~abstr-range abstr) local
							(append (data~abstr-domain abstr) cycles)
				  ))))
  ;; ))


(defun bind=local-p (var)
  (declare (edited  "23-MAY-1996 10:54")
	   (authors GKLEIN)
	   (input   "A variable.")
	   (effect  "None.")
	   (value   "T iff var is bound in the current context."))
  (eq (bind=current-context) (first (data~binding var))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;


(defun bind=key-create (key)
  (declare (special bind*key-table))
  (multiple-value-bind (val found) (gethash key bind*key-table)
    (declare (ignore val))
    (if found
	(error "key ~A exists" key)
      (setf (gethash key bind*key-table) nil))))


(defun bind=key-exists-p (key)
  (declare (special bind*key-table))
  (multiple-value-bind (val found) (gethash key bind*key-table)
    (declare (ignore val))
    found))

(defun bind=key-empty-stack-p (key)
  (declare (edited  "17-MAY-1996 09:49")
	   (authors GKLEIN)
	   (input   "A key.")
	   (effect  "A key.")
	   (value   "T iff the existing stack for the key KEY is empty."
		    "If no stack exists already, then an error will be signaled."))
  (declare (special bind*key-table))
  (multiple-value-bind (val found) (gethash key bind*key-table)
    (if found
	(null val)
      (error "key ~A does not exist" key))))


(defun bind=key-stack-not-empty-p (key)
  (declare (edited  "17-MAY-1996 09:47")
	   (authors GKLEIN)
	   (input   "A key.")
	   (effect  "None.")
	   (value   "T iff a non empty stack exists for the key KEY."))
  (declare (special bind*key-table))
  (gethash key bind*key-table))
    
    

  
(defun bind=key (key)
  (declare (special bind*key-table))
  (gethash key bind*key-table))


;;;;;;;;;;;;;;;


(defun bind=current-stack ()
  bind*current-stack)

(defun bind=find-context (context cstack) ;;; weg damit
  (declare (edited  "07-JUN-1996 16:23")
	   (authors GKLEIN)
	   (input   "A context and a stack of contexts.")
	   (effect  "None.")
	   (value   "The (first) substack of CSTACK (inclusive) in which CONTEXT is a member."))
  (when cstack
    (if (member context (bind=stack cstack))
	cstack
      (bind=find-context context (bind=pred-stack cstack)))))

(defun bind=pred-stack-p (stack1 stack2)
  (declare (edited  "13-JUN-1996 10:34")
	   (authors GKLEIN)
	   (input   "Two stacks.")
	   (effect  "None.")
	   (value   "Two values:"
		    "2. T iff STACK1 is a predecessor (reflexive and transitive) stack of STACK2."
		    "1. If the second value is T then the sub-stack of stack1 is returned in which the chain"
		    "of son nodes from stack1 to stack2 ends. If stack1 is the same as stack2 then"
		    "this value is T."))
  (cond ((eq stack1 stack2)
	 (values t t))
	((null stack2)
	 (values nil nil))
	((eq stack1 (bind=pred-stack stack2))
	 (values (bind=pred-ctx stack2) t))
	(t
	 (bind=pred-stack-p stack1 (bind=pred-stack stack2)))))

  
(defun bind=trust-p (binding-context current-context last-access-context)
  (declare (edited  "07-JUN-1996 16:30")
	   (authors GKLEIN)
	   (input   "Two contexts and a stack.")
	   (effect  "None.")
	   (value   "T iff STACK is substack of the current stack."))
  (let ((binding-stack (bind=stack-of binding-context))
	(current-stack (bind=stack-of current-context)))
    (when (bind=pred-stack-p binding-stack current-stack)
      (multiple-value-bind (substack current-before-last)
	  (bind=pred-stack-p current-stack last-access-context)
	(when current-before-last
	  (if (eq substack t)
	      (member binding-context (bind=stack current-stack))
	    (and (eq current-context (car substack))
		 (member binding-context (bind=stack current-stack)))))))))


(defmacro bind~with-new-stack ((&body forms) &key (parent-context-list nil))
  `(progn
;     (when (null ,parent-context-list)
;       (warn "You have to specify a parent context in bind~~with-new-stack."))
     (let ((new-stack (bind=context-stack-create ,parent-context-list))
	   (old-stack bind*current-stack))
       (unwind-protect
	   (progn (setf bind*current-stack new-stack)
		  (progn ,@forms))
	 (setf bind*current-stack old-stack)))))


;;;;

(defun bindels ()
  (format t "~%bind*binding-stack: ~A" bind*binding-stack)
  (format t "~%bind*current-stack: ~A" bind*current-stack)
  ;(format t "~%bind*enable-stack: ~A" bind*enable-stack)
  ;(format t "~%bind*label-tab: ~A" bind*label-tab)
  ;(format t "~%bind*btp-tab: ~A" bind*btp-tab)
  ;(format t "~%bind*key-table: ~A" bind*key-table)
  )
