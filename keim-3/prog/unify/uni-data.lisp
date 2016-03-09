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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package "KEIM")

(mod~defmod uni-data :uses ()
	    :documentation "Data Structures for Unification."
	    :exports  (uni+data
                       uni+terms
                       uni+hou
                       uni~terms
		       uni~daughters
                       uni~terms-push
                       uni~terms-pop
                       uni~flex-flex
                       uni~flex-rigid
                       uni~constraints
                       uni~substitution
		       keim~copy
                       uni~reduce
                       )
	    )

;;
;; main data structures
;;

(eval-when (load compile eval) 
  (defclass uni+data (data+top)
    ()
    (:documentation 
     "The class uni+data is the top class for
      unification problem/solution objects.")))


(eval-when (load compile eval) 
  (defclass uni+linked (uni+data)
    ((link :accessor uni~link :initarg :link :initform nil)
     (daughters :accessor uni~daughters :initarg :daughters :initform nil))
    (:documentation 
     "Uni structures with link to original/daughters problem.")))

(defmethod uni~origin ((uni uni+linked))
  (if (uni~link uni)
      (uni~origin (uni~link uni))
    ;; else
    uni))

(defmethod uni~origin ((uni cons))
  (when uni (mapcar #'uni~origin uni)))


(eval-when (load compile eval) 
  (defclass uni+terms (uni+linked)
    ((terms :accessor uni~terms :initarg :terms :initform nil))
    (:documentation 
     "Uni+terms are unification structures with termlists, e.g., for
      first order and higher order unification problems")))

(defgeneric uni~terms-push (termpair uni))

(defmethod uni~terms-push ((termpair cons)(uni uni+terms))
  (declare (edited  "11-DEC-1996")
	   (authors Konrad)
	   (input   "a termpair (cons) and a uni+terms structure.")
	   (effect  "push termpair into termlist of UNI.")
	   (value   "the new termlist."))
  (setf (uni~terms uni)
        (cons termpair (uni~terms uni))))

(defgeneric uni~terms-pop (uni))

(defmethod uni~terms-pop ((uni uni+terms))
  (declare (edited  "11-DEC-1996")
	   (authors Konrad)
	   (input   "a uni+terms structure.")
	   (effect  "pops first element from termstack.")
	   (value   "the first termpair."))
  (let ((pop (first (uni~terms uni))))
    (setf (uni~terms uni)
          (rest (uni~terms uni)))
    pop))


;;
;; Higher order unification
;;

(eval-when (load compile eval) 
  (defclass uni+hou (uni+terms)
    ((flex-flex :accessor uni~flex-flex :initarg :flex-flex :initform nil)
     (flex-rigid :accessor uni~flex-rigid :initarg :flex-rigid :initform nil)
     (subst :accessor uni~substitution :initarg :substitution
            :initform nil)
     (bill :accessor uni~bill :initarg :bill :initform 0))
    (:documentation 
     "Uni+hou are higher-order unification structures. Subst carries
       a single solution substitution.")))

(defmethod print-object ((object uni+hou) (stream stream))
  (format stream "~a" (list "HOU"
                            "terms:" (uni~terms object)
                            "subst:" (uni~substitution object)
                            "flex-flex:" (uni~flex-flex object)
                            "flex-rigid:" (uni~flex-rigid object)
			    "bill:" (uni~bill object))))

(eval-when (load compile eval) 
  (defclass uni+dsearch (uni+hou)
    ((bind-context :accessor uni~bind-context :initarg :bind-context
            :initform nil))
    (:documentation 
     "Uni+dsearch are HOU problems that are solved by lazy DFS.")))
     
;;
;; copy function
;;

(defmethod keim~copy ((object uni+hou)
                      &key (explode :all-classes) (share :all-classes)
                      preserve downto)
  (let ((newobj (uni~create object
                            :terms (uni~terms object))))
    (setf (uni~flex-rigid newobj) (uni~flex-rigid object))
    (setf (uni~flex-flex newobj) (uni~flex-flex object))
    (setf (uni~substitution newobj) (uni~substitution object))
    (setf (uni~bill newobj) (uni~bill object))
    (setf (uni~link newobj) (uni~link object))
    newobj))
