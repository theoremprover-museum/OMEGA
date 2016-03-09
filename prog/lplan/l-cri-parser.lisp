;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; plan.lisp; This file is part of the OMEGA system
;;
;; major updates: 24.2.1999,
;; 
;;
;; Authors: Lassaad Cheikhrouhou, Juergen Zimmer, Carsten Ullrich
;; email: {lassaad,jzimmer, cullrich}@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Planungs-Algorithmus
(in-package "OMEGA")

(mod~defmod CRI-PARSER 
            :uses (node)
            :documentation "The selector functions for the cri language. "
            :exports (cri-parser~param-tuple
		      cri-parser~param-supp-choice
		      cri-parser~ref
		      cri-parser~ref-param+supp-list
		      cri-parser~ref-param-supp-choice
		      cri-parser~ref-param-tuple
		      cri-parser~ref-supp-choice
		      cri-parser~ref-param-tuple-list
		      cri-parser~supps
		      cri-parser~supp-tuples
		      cri-parser~task
		      cri-parser~task-ref-set
		      cri-parser~task-supps))

#{
This module implements selector functions for the result language of the control rule
interpreter.
\subsection{Selector Functions For Task Choice}
#}

(defun cri-parser~task (task-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice.")
	   (effect  "None.")
	   (value   "The task object of task choice."))
  (if (consp task-choice)
      (first task-choice)
    task-choice))

(defun cri-parser~task-ref-set (task-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice.")
	   (effect  "None.")
	   (value   "A list of refinement choices associated with task choice,"
		    "possibly nil."))
  (when (and (consp task-choice)
	     (consp (second task-choice)))
    (second task-choice)))

(defun cri-parser~task-supps (task-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A task choice.")
	   (effect  "None.")
	   (value   "A list of supports (node objects),"
		    "possibly nil."))
  (when (and (consp task-choice)
	     (node~p (second task-choice)))
    (rest task-choice)))

#{
\subsection{Selector Functions For Refinement Choice}
#}

(defun cri-parser~ref (ref-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement choice.")
	   (effect  "None.")
	   (value   "The refinement object of refinement choice."))
  (let ((ref (if (listp ref-choice)
		 (first ref-choice)
	       ref-choice)))
    (if (symbolp ref)
	(let ((method (ref~find-ref ref)))
	  (if method
	      method
	    (omega~error "Method named in control rule does not exist: ~A" ref)))
      ref)))

(defun cri-parser~ref-param+supp-list (ref-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement choice.")
	   (effect  "None.")
	   (value   "A list of parameter+support associated with refinement choice,"
		    "possibly nil."))
  (when (listp ref-choice)
    (let ((rest-choice (rest ref-choice)))
      (when (and (consp (first rest-choice))
		 (consp (first (first rest-choice))))
	(first (first rest-choice))))))

(defun cri-parser~ref-supp-choice (ref-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A refinement choice.")
	   (effect  "None.")
	   (value   "A support choice associated with refinement choice,"
		    "possibly nil."))
  (when (listp ref-choice)
    (let ((rest-choice (rest ref-choice)))
      (when (not (and (consp (first rest-choice))
		      (consp (first (first rest-choice)))))
	rest-choice))))

;;      (if (listp (first rest-choice))
	  ;; rest-choice is parameter+support-list or consists of supp-tuples
;;	  (when (not (listp (first (first rest-choice))))
	    ;; rest-choice can only consist of supp-tuples
;;	    (mapcar 'cri-parser~supps rest-choice))
	;; rest-choice consists of supports
;;	(cri-parser~supps rest-choice)))))

(defun cri-parser~ref-param-tuple (ref-param+supp)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A parameter+support substructure of refinement choice.")
	   (effect  "None.")
	   (value   "The corresponding parameter tuple."))
  (when ref-param+supp
    (first ref-param+supp)))

(defun cri-parser~ref-param-count (ref-param+supp)
  (if ref-param+supp
      (length (cri-parser~ref-param-tuple ref-param+supp))
    0))

(defun cri-parser~ref-param-supp-choice (ref-param+supp)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A parameter+support substructure of refinement choice.")
	   (effect  "None.")
	   (value   "The corresponding support choice, possibly nil."))
  (when ref-param+supp
    (rest ref-param+supp)))

(defun cri-parser~ref-param-tuple-list (ref-choice)
  (when (listp ref-choice)
    (rest ref-choice)))

#{
\subsection{Selector Functions For Parameter Choice}
#}

(defun cri-parser~param-create (tuple supp-choice)
  (cons tuple supp-choice))

(defun cri-parser~param-tuple (param-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A parameter choice.")
	   (effect  "None.")
	   (value   "The corresponding parameter tuple."))
  (first param-choice))
  ;;  (if (and (consp param-choice) (listp (first param-choice)))
;;      (first param-choice)
;;   param-choice))

(defun cri-parser~param-supp-choice (param-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A parameter choice.")
	   (effect  "None.")
	   (value   "The corresponding support choice, possibly nil."))
  (rest param-choice))
 
 ; (when (and (consp param-choice) (listp (first param-choice)))
  ;  (second param-choice)))

#{
\subsection{Selector Functions For Support Choice}
#}


(defun cri-parser~supps (supp-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A support choice.")
	   (effect  "None.")
	   (value   "The unordered list of support nodes, possibly nil."))
  (when (and (consp supp-choice)
	     (not (listp (first supp-choice))))
    supp-choice))

(defun cri-parser~supp-tuples (supp-choice)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A support choice.")
	   (effect  "None.")
	   (value   "A list of support tuples (ordered support nodes),"
		    "possibly nil."))
  (when (and (consp supp-choice)
	     (listp (first supp-choice)))
    supp-choice))
