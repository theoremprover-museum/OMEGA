;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(mod~defmod meta :uses (term type data env keim mod post)
	    :documentation "Metavariables"
	    :exports (
		      meta+variable
		      meta~p
		      meta~read-term
		      meta~read-poly-term
		      meta~match
		      meta~match-bind
		      meta~meta-subst
		      meta+error
		      meta+mismatch
		      meta~mismatch-expected
		      meta~mismatch-found
		      meta+mismatch-type
		      meta~mismatch-type-expected
		      meta~mismatch-type-found
		      meta+illegal-match
		      meta~illegal-match-wffschema
		      meta~illegal-match-wffvalue
		      meta~illegal-match-gwff
		      meta~variable-create
		      )
	    )
		   
#{\section{Metavariables}\label{mod:meta}
\subsection{Introduction}
Metavariables are variables that stand in for other terms.  They are thus
used in {\vb META~MATCH-BIND} and {\vb META~META-SUBST}.  The class
META+VAR is used to distinguish these variables. It is a direct subclass
of SYM+VAR.
#}


(eval-when (load compile eval)
(defclass meta+variable (term+variable)
  ()
  (:documentation "A metavariable.")))

(defun meta~p (thing)
  (declare (authors nesmith)
	   (input "A lisp object THING.")
	   (effect "None")
	   (value "T if the thing is a meta-variable, nil otherwise.")
	   (example "(meta~read-term '(meta-x) env) --> T"))
  (typep thing 'meta+variable))

(defmethod post~read-object ((vars list) (env env+environment) 
			    (indicator (eql :meta-variables)))
  (mapcar #'(lambda (var)
	      (post~read-object var env :meta-variable))
	  vars))

(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :meta-variable)))
  (let* ((type (post~read-object (second var) env :existing-type))
	 (new-term (meta~create-variable-in-environment (first var)
							(data~n-normalize type :destructive t)
							env)))
    new-term))

(data~defgeneric meta~variable-create (symbol (type))
  (declare (edited  "12-FEB-1998")
	   (authors Ameier)
	   (input   "A symbol and a type.")
	   (effect  "None.")
	   (value   "An object of type meta+variable."))
  (:method (symbol (type type+type))
	   (let ((tvar (data~variable-create 'meta+variable :name symbol)))
	     (setf (data~annotation tvar) type)
	     tvar)))

(defun meta~create-variable-in-environment (symbol type env &key (allow-multi nil))
  (declare (edited  "12-FEB-1998")
	   (authors Ameier)
	   (input   "A symbol, a type and an environment.")
	   (effect  "If there is already an object in the environment with the key symbol"
		    ", but an other class, error."
		    "If there is already an object in the environment with the key symbol and the same class"
		    ", but an other type (annotation), error."
		    "If there is already an object in the environment with the same symbol and the same"
		    "class, this object is returned."
		    "Otherwise or if it is allow to define terms multiple in the environment, a term-object of"
		    "the class is created, added to the environment and returned.")
	   (value   "See effect."))
  (when (not (symbolp symbol))
    (error "~S is not a symbol." symbol))
  (when (not (type~p type))
    (error "~S is not a type." type))
  (when (not (env~p env))
    (error "~S is not an environment." env))
   (let* ((thing (env~lookup-object symbol env))
	  (class-thing (class-name (class-of thing))))
     (cond ((or allow-multi (null thing))
	    (let* ((real-type (if (type~schema-p type)
				  (data~schema-range type)
				type))
		   (type-kappas (if (type~schema-p type)
				    (data~schema-domain type)
				  nil))
		   (term-object (meta~variable-create symbol real-type))
		   (term-object-ii (if (type~schema-p type)
				       (term~schema-create term-object :kappas type-kappas)
				     term-object)))
	      (env~enter symbol term-object-ii env)
	      term-object-ii))
	   ((and thing (not (equal class-thing 'meta+variable)))
	    (error "Can't declare ~S as a term-object of class meta+variable, because it already exists in the environment as a ~A. "
		   symbol class-thing))
	   ((not (data~equal (data~annotation thing) type))
	    (error "Can't declare ~A as a term of type ~A, because it already exists in the environment with type ~S." 
		   symbol type (data~annotation thing)))
	   (t
	    thing))))


(defmethod term=alpha-equal ((term1 meta+variable) (term2 term+term) bdbl)
  (cond ((data~equal term1 term2)
	 t)
	((bind~binding term1)
	 (term=alpha-equal (bind~binding term1) term2
			   bdbl))
	((and (find term1 bdbl) (term~not-free-occuring term1 term2))
	 (let ((lev-res (type~alpha-equal (data~annotation term1)
					  (data~annotation term2)
					  bdbl)))
	   (when lev-res
	     (progn 
	       (bind~bind term1 term2)
	       (subst~bind-subst lev-res bdbl)))))
	(t nil)))

#{
\subsection{Matching meta-formulas}
META~MATCH matches a meta-term (one which may contain meta-variables)
against a normal term (in which all meta-variables will be ignored). 
It returns an association list binding meta-variables to terms and
type variables to constant types. 

META~SUBST will apply such a binding list to a meta-term to produce
an instantiated term.
#}

;; Mit dem Matchen sollten wir absprechen, ob es wirklich noetig ist, einen eigenen
;; Matcher zu haben, oder ob man nicht besser Holgers data~alpha-equal Jumbofunktion
;; nehmen kann.
;;
;; Wenn doch was neues sein muss, dann bitte hierher, und nicht weiter oben
;; a la Arthur einfach ueberdefinieren.
;;
;; Wenn moeglich, BINDUNGSMECHANISMUS BENUTZEN!!!
;;
;; Holger, bitte sorge dafuer, dass bind, beta, subst  usw. auch wirklich in das SYSTEM struct
;; eingebaut werden.
;;
;;                                 DEF
;;
;; PS: alle diese Kommentare sind nach Vollzug zu loeschen.

;; LC-Wunsch
;; meta~match wffschema gwff &optional (subst (subst~create nil nil))
;;                                     (additional-vars nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#{\subsection{Error handling}
We define the following conditions.
\begin{description}
\item[META+ERROR] A subclass of SYS+ERROR.
\item[META+MISMATCH] A subclass of META+ERROR, used for reporting general 
failures in matching two terms.
\item[META+MISMATCH-TYPE] A subclass of META+ERROR, used for reporting failures
in matching when two terms differ in type.
\end{description}

By handling the condition META+ERROR (with SYS~HANDLER-BIND, for 
example), you can detect and appropriately handle 
all matching failures.  This depends on the Lisp being able to do this.
#}



(sys~define-condition meta+error (sys+error)
		      ()
		      (lambda (condition stream)
			(declare (ignore condition))
			(format stream "~%Some kind of error in metavariable matching.")))


(sys~define-condition meta+mismatch (meta+error)
  ((expected) (found))
  (lambda (condition stream)
    (format stream "~%;;;Wff mismatch.  Meta-term is ")
    (post~print (meta+mismatch-expected condition) stream)
    (format stream ", but real term is ")
    (post~print (meta+mismatch-found condition) stream)))

(defmacro meta~mismatch-expected (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+mismatch, its EXPECTED slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch :expected 'A :found 'B) --> A"))
  `(meta+mismatch-expected ,err))

(defmacro meta~mismatch-found (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+mismatch, its FOUND slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch :expected 'A :found 'B) --> B"))
  `(meta+mismatch-found ,err))


(sys~define-condition meta+mismatch-type (meta+error)
  ((expected) (found))
  (lambda (condition out)
	     (let ((expect (meta+mismatch-type-expected condition))
		   (found (meta+mismatch-type-found condition)))
	     (princ "~%;;;Type mismatch.  Meta-term " out)
	     (post~print expect out)
	     (princ " must have type " out)
	     (post~print (term~type expect) out)
	     (princ " while real term " out)
	     (post~print found out)
	     (princ " has type " out)
	     (post~print (term~type found) out)
	     (princ "." out)
	     (terpri out))))
 

(defmacro meta~mismatch-type-expected (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+mismatch-type, its EXPECTED slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch-type :expected 'A :found 'B) --> A"))
  `(meta+mismatch-type-expected ,err))

(defmacro meta~mismatch-type-found (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+mismatch-type, its FOUND slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch-type :expected 'A :found 'B) --> B"))
  `(meta+mismatch-type-found ,err))


(sys~define-condition meta+illegal-match (meta+error)
  ((wffschema) (wffvalue)(gwff))
  (lambda (condition stream)
    (princ "~%;;; Illegal Match.  " stream)
    (post~print (meta+illegal-match-wffschema condition)
		stream)
    (princ " matched " stream)
    (post~print (meta+illegal-match-wffvalue condition) stream)
    (princ " but now matches " stream)
    (post~print (meta+illegal-match-gwff condition) stream)))

(defmacro meta~illegal-match-wffschema (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+illegal-match, its WFFSCHEMA slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> A"))
  `(meta+illegal-match-wffschema ,err))

(defmacro meta~illegal-match-wffvalue (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+illegal-match, its WFFVALUE slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> B"))
  `(meta+illegal-match-wffvalue ,err))

(defmacro meta~illegal-match-gwff (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+illegal-match, its GWFF slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> C"))
  `(meta+illegal-match-gwff ,err))


(defun meta=report-mismatch (expect found)
  (sys~signal (sys~make-condition 'meta+mismatch :expected expect :found found)))

(defun meta=report-mismatch-type (expect found)
  (sys~signal 
   (sys~make-condition 'meta+mismatch-type :expected expect :found found)))


(defun meta=wffeval (meta-var bindings)
  (or (cdr (assoc meta-var bindings
		  :test (if (poly~p meta-var) 
			    #'(lambda (x y) (and (poly~p y)
						 (eq (poly=instance-obj x)
						     (poly=instance-obj y))))
			  ;#'eql
			  #'(lambda (x y)
			      (and (term~p y) (term~equal x y))))))
      meta-var))

(defmethod term~create-primitive-in-environment ((symbol symbol) (type type+type) (class (eql 'meta+variable))
						 (env env+environment) &key (allow-multi nil))
  (let* ((thing (env~lookup-object symbol env))
	 (class-thing (class-name (class-of thing))))
    (cond ((or allow-multi (null thing))
	   (let ((term-object (meta~variable-create symbol type)))
	     (env~enter symbol term-object env)
	     term-object))
	  ((and thing (not (equal class-thing class)))
	   (error "Can't declare ~S as a term-object of class ~A, because it already exists in the environment as a ~A. "
		  symbol class class-thing))
	  ((not (keim~equal (data~annotation thing) type))
	   (error "Can't declare ~A as a term of type ~A, because it already exists in the environment with type ~S." 
		  symbol type (data~annotation thing)))
	  (t
	   thing))))

  




#{\subsection{Auxiliary functions}
#}

