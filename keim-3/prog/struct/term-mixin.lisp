;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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


;; KEIM-3 REPLACEENTS:
;; termix~set-term!              --> termix~term (accessor)

(mod~defmod termix :uses (env keim mod post term datix)
	    :documentation "Using terms in larger structures"
	    :exports (
		      termix+mixin
		      termix~term
		      termix+named-term
		      termix~create-named-term
		      termix~named-term-p
		      termix~read-named-term
		      termix~named-term-abbrev
		      )
	    )

#{
\section{Term mixins}\label{mod:termix}
 Often we would like to have objects which behave as terms, but also
 have other properties (such as names).  Here TERMIX+MIXIN is a new 
 KEIM+OBJECT
 that contains a slot TERM, where a term can be placed.
#}

; rename term+mixin -> termix+mixin

(eval-when (load compile eval)
  (defclass termix+mixin (datix+mixin)
    ((struct :initform nil
	     :initarg :term
	     :accessor termix~term))
    (:documentation "A mixin struct object which contains a term instead a struct.")
    )
  )



(defmethod print-object ((thing termix+mixin) stream)
  (print-object (termix~term thing) stream))

#{
\section{Named terms}
 Here we define TERMIX+NAMED-TERM.  Instances of this class will
 contain a name slot as well as a term slot, so we can associate
 names with terms.
#}

(eval-when (load compile eval)
  (defclass termix+named-term (datix+named-struct)
    ((struct :initform nil
	     :initarg :term
	     :accessor termix~term))
    (:documentation "This is the superclass to all terms in KEIM that have a name")))

(defun termix~named-term-p (thing)
  (declare 
   (authors nesmith)
   (input   "An object")
   (effect  "none")
   (value   "T if the object is a named-term, otherwise nil."))
  (typep thing 'termix+named-term))

(defun termix~create-named-term (name term)
  (declare (edited  "22-JAN-1998")
	   (authors Ameier)
	   (input   "A name and a term.")
	   (effect  "None.")
	   (value   "A named term."))
  (make-instance 'termix+named-term
		 :name name
		 :term term))

(defmethod print-object ((named-term termix+named-term) stream)
  (format stream "(~A ~A "
	  'namedterm
	  (keim~name named-term))
  (format stream " ~A" (termix~term named-term))
  (format stream ")"))

(defmethod post~read-object ((named-term list) (env env+environment) 
			     (indicator (eql :named-term)))
  (let* ((name (first named-term))
	 (term (post~read-object (second named-term) env :existing-term)))
    (termix~create-named-term name term)))
  
(defun termix~read-named-term (symbol env)
  (declare 
   (authors nesmith)
   (input   "A symbol and a environment")
   (effect  "none")
   (value   "If the symbol is associated with a named-term in the environment,
returns the named-term."))
  (let ((obj (env~lookup-object symbol env)))
    (unless (termix~named-term-p obj)
      (post~error "~A is not a named term in environment." symbol))
    obj))


(defgeneric termix~named-term-abbrev (named-term)
  (declare 
   (authors nesmith)
   (input   "A named-term")
   (effect  "none")
   (value   "A short string identifying the specific type of the named-term 
(used in printing)."))
  (:method ((thing termix+named-term))
	   "namedterm")
  (:documentation "A string abbreviation for this type of named of term. Used in printing."))

(defmethod env~post-print (key (term termix+named-term) stream)
  (declare (ignore key))
  (post~print term stream)
  (values))


(defmethod post~print ((term termix+named-term) stream)
  (format stream "(~A " (keim~name term))
  (post~print (termix~term term) stream)
  (format stream ")"))

(defmethod keim~copy ((named-term termix+named-term) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep named-term x)) downto))
      named-term
    (termix~create-named-term (keim~name named-term)
			      (data~copy (termix~term named-term)
					 :explode explode
					 :share share
					 :preserve preserve
					 :downto downto))))
					



(defmethod data~free-variables ((term termix+named-term))
  (remove-duplicates (data~free-variables (termix~term term))))
