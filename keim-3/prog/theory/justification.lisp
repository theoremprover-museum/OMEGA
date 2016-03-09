;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(in-package "KEIM")

(mod~defmod just :uses ( keim mod )
	    :documentation  "Datastructures and basic functionality for generic justifications."
	    :exports (
		      just+justification ;; -> other slot names
		      just~create
		      ;;just~rule -> just~method
		      just~method
		      ;;just~set-rule! -> (setf (just~method ..) ..)
		      ;;just~nodes -> just~premises
		      just~premises
		      ;;just~set-nodes! -> setf just~premises
		      just~check-p
		      )
	    )


#{\section{Justifications}\label{mod:just}

All justifications represent a tree (or DAG) structure which encodes the dependency of the current nodes in
the calculus upon each other, as well as the rule with which each line was grounded. At this level of
abstraction the rule can be an arbitrary \lisp\ object where as the dependency is represented by the list of
nodes that the node to be justified depends on. #}

(eval-when (load compile eval)
(defclass just+justification (keim+object)
  ((method :accessor just~method
	   :initform "Unspecified"
	   :initarg :method
	   :documentation "The inference method that justifies this proof node.")
   (premises :accessor just~premises
	     ;;:reader just~premises
	     ;;:writer just=premises
	     :initform nil
	     :initarg :premises
	     :documentation "The proof-nodes from which this node is justified."))
  (:documentation "The justification for a proof node.")))

(defun just~create (method premises)
  (declare (edited  "17-SEP-1996")
	   (authors Lassaad )
	   (input   "A derivation-rule and a list of proof-nodes.")
	   (effect  "A new justification is created.")
	   (value   "The new justification."))
  (let ((new-obj (make-instance 'just+justification :method method)))
    (setf (just~premises new-obj) premises)
    new-obj)
  )
    
;;(defsetf just~premises (just) (prems)
;;  `(just=premises-medium ,just ,prems))
;;
;;(defgeneric just=premises-medium (just prems)
;;  (:method (just prems)
;;           (error ";;;JUST~~PREMISES: wrong type of ~A." just))
;;  (:method ((just just+justification) prems)
;;           (just=premises prems just)))


(defmethod print-object ((just just+justification) stream)
  (let ((node-list (mapcar #'keim~name (just~premises just))))
    (if node-list
	(format stream "#<Justified by ~A from ~A>" (keim~name (just~method just)) node-list)
    (format stream "#<Justified by ~A>" (keim~name (just~method just))))))

#| the followig is no onger in use
#{\subsection{Verifying Justifications}

The next function is intended to be a general function for objects
resulting from a justification verifying that the justification is
correct, i.e., if the justification is applied to the original objects
then the specified resulting object comes out.
#}

(defgeneric just~check-p (object justification)
  (declare (edited  "26-JUL-1995")
	   (authors Acsehn)
	   (input "An OBJECT and a JUSTIFICATION.")
	   (effect "None.")
	   (value "T, if OBJECT is a result from JUSTIFICATION, otherwise NIL. 
The justification rule is applied to the nodes of JUSTIFICATION and the resulting object 
must be equal to OBJECT.")))
|#
