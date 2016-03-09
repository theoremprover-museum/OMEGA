;;; -*- Syntax: Common-Lisp; Package: KEIM; Base: 10; Mode: LISP -*-
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





(mod~defmod TOSET 
            :uses (cl data hou keim lit pos stind term uni)
            :documentation "Datastructures and basic functionality of sets of term objects. This module provides the general interface for clause-graphs, indexed clause sets.... ."
            :exports (
                      toset+index
                      toset+list
                      toset+set
                      
                      toset~add-object
                      toset~all-general
                      toset~all-instances
                      toset~all-renamed
                      toset~all-same-top
                      toset~all-subterms
                      toset~all-unifiable
                      toset~create
                      toset~elements
                      toset~index
                      toset~member-p
                      toset~objects
                      toset~remove-object
                      toset~term-accessor
                      toset~top-terms
                      ))





#{\section{Term object sets}
\label{mod:toset}

An important problem in implemting automated theorem provers is the
handling of large sets of objects containing terms. The tasks in these
sets often are not simple; they have to do with unifying, matching or
renaming.

For instance during a resolution proof when a new clause is
constructed it must be checked if a renaming, an instance or a more
general clause already exists. Then to be able to perform all possible
resolution steps with this clause all all clauses with a literal which
is unifiable with one of the literals in the new clause must be found.

For implementing these tasks efficiently several techniques have been
developed (clause graphs, indexing etc.). This module tries to define
a general interface for these techniques so that different techniques
and different implementations can be exchanged more easily.

Furthermore a very simple way of handling sets,
using a list to store the objects, is defined in this module.

\subsection{Classes}


A term object set (toset) is a datastructure managing sets of objects
which contain terms. The purpose of a toset is to return all objects
which contain a term that is related with a given term.

For instance if we store all clauses of a resolution proof in a toset
and we get a new clause with a literal {\vb (+ (P x))} we want to get
all clauses which contain a literal that is unifiable with this one.
If we have implemented a clause graph there will be links pointing
to these clauses.

In this case we only need the toplevel terms of clauses (the atoms).
If we would find a new demodulator {\vb (f x a) = x} we would like to
find all clauses which contain terms {\vb (f x a)} can be matched to.
In this case we need terms at all levels of a clause. Therefor the
class {\vb toset+set} provides a term accessor slot that contains a
function which returns the terms of an \keim\ object. This function
specifies which terms of a \keim\ object can be accessed in a toset,
i.e. which terms shall be indexed.
#}

(eval-when (load compile eval)
(defclass toset+set (keim+name keim+object)
  ((term-accessor :initarg :term-accessor :accessor toset~term-accessor))
  (:documentation "This class is the basic interface to big sets of term objects, it is
                   intended to be specialized to inter alii clause graphs and indexing structures of
                   various kinds.")))

(eval-when (load compile eval)
(defclass toset+list (toset+set)
  ((objects :initarg :objects :accessor toset~objects))
  (:documentation "This class is the basic interface to large lists of term objects, it is
                   intended to be specialized to inter alii clause graphs and indexing structures of
                   various kinds.")))

(eval-when (load compile eval)
  (defclass toset+index (toset+set)
    ((index :initarg :index :accessor toset~index))
    (:documentation "This class represents an indexing structure for objects.")))
   


#{\subsection{Accessors} #}


(defgeneric toset~top-terms (object)
  (declare (edited  "05-JAN-1992 13:08")
	   (authors RICHTS)
	   (input   "A KEIM-object.")
	   (effect  "None.")
	   (value  "1. A list of all topterms in OBJECT (i.e. the term itself if OBJECT is a term"
		    "   or the atoms of all literals if it is a clause)."
		    "2. A list of the terms position sin OBJECT.")
	   (remark  "This function can be used as a term-accessor for termobject-sets."))
  (:method ((term term+term))
   (values (list term) (list (pos~empty))))
  (:method ((literal lit+literal))
   (values (list (lit~atom literal)) (list (pos~add-front 1))))
  (:method ((clause cl+clause))
	   (let ((i 1)
		 (terms nil)
		 (positions nil))
	     (mapc #'(lambda (literal)
		       (push (lit~atom literal) terms)
		       (push (pos~add-front i (pos~add-front 1)) positions)
		       (incf i))
		   (cl~literals clause))
	     (values terms positions)))
  (:method ((toset toset+list))
	   (let ((i 1)
		 (terms nil)
		 (positions nil))
	     (mapc #'(lambda (obj)
		       (multiple-value-bind (newterms newpositions)
			   (toset~top-terms obj)
			 (setf terms (append terms newterms))
			 (setf positions (append positions
						 (mapcar #'(lambda (pos) (pos~add-front i pos))
							 newpositions)))
			 (incf i)))
		   (toset~objects toset))
	     (values terms positions)))
  (:method ((index toset+index))
	   (values (toset~term-accessor index) (pos~empty))))
		 
			 
	       
			
			  
				  
				  


#{\subsection{Simple Functions on Sets} #}

(defgeneric toset~create (implementation name objects &key term-accessor)
  (declare (edited)
	   (authors RICHTS)
	   (input  "The implementation of a termobject-set, i.e. one of the keywords (:list,...) and a list of objects."
		   "Optional a function that maps from an object to the terms of the object."
		   "It must return two values: A list of all terms and a list of the position of the terms in their object."
		   "The default is TOSET~TOP-TERMS which returns the topterms of an object."
		   "(e.g. the term itself if the object is a term or the atoms
of the literals if it is a clause).")
	   (effect "None.")
	   (value  "A new set of the termobjects OBJECTS which can be accessed over the terms specified by the"
		   "TOSET~TERMS method for the termsets."))
  (:method ((implementation (eql :list)) name (objects list) &key (term-accessor #'toset~top-terms))
   (make-instance 'toset+list :objects objects :name (string name) :term-accessor term-accessor))
  (:method ((implementation (eql :index)) name (objects list) &key (term-accessor #'toset~top-terms))
	   (let* ((hashtable (stind~create-index))
		  (index (make-instance 'toset+index :name name :index hashtable :term-accessor term-accessor)))
	     (stind~insert-terms objects hashtable)
	     index)))


(defgeneric toset~elements (toset)
  (declare (edited)
	   (authors Chris)
	   (input  "A toset")
	   (effect "The elemenst of the toset.")
	   (value  "The changed TOSET."))
  (:method ((to-list toset+list))
	   (toset~objects to-list))
  (:method ((index toset+index))
	   (toset~index index)))


(defgeneric toset~add-object (toset object)
  (declare (edited)
	   (authors Chris)
	   (input  "A term object-set and an object.")
	   (effect "OBJECT is adjoined to TOSET.")
	   (value  "The changed TOSET."))
  (:method ((to-list toset+list) object)
	   (let ((new-objects (adjoin object (toset~objects to-list)
				      :test #'keim~equal)))
	     (setf (toset~objects to-list) new-objects))
	   to-list)
  (:method ((index toset+index) object)
	   (stind~insert-term object (toset~index index))
	   (let ((new-objects (cons object (toset~term-accessor index))))
	     (setf (toset~term-accessor index) new-objects))
	   index))


(defgeneric toset~remove-object (toset object)
  (declare (edited)
	   (authors Chris)
	   (input  "A term object-set and an object.")
	   (effect "OBJECT is removed from TOSET.")
	   (value  "The changed TOSET."))
  (:method ((to-list toset+list) object)
	   (setf (toset~objects to-list) (delete object (toset~objects to-list)))
	   to-list)
  (:method ((index toset+index) object)
	   (stind~remove-term object (toset~index index))
	   (setf (toset~term-accessor index) (delete object (toset~term-accessor index)))
	   index))
	   

(defgeneric toset~member-p (toset object) 
  (declare (edited)
	   (authors RICHTS)
	   (input  "A termobject-set and an object.")
	   (effect "None.")
	   (value  "True, iff OBJECT is a member in TOSET."))
  (:method ((to-list toset+set) object)
	   (find object (toset~objects to-list)))
  (:method ((index toset+index) object)
	   (find object (toset~term-accessor index))))



#{\subsection{Complex functions on sets} #}


(defgeneric toset~all-renamed (toset term) ;;; noch nicht angepasst
  (declare (edited)
	   (authors RICHTS)
	   (input  "A term object-set and a term.")
	   (effect "None.")
	   (value "1. A list of all objects in TOSET that contain a term which is a renaming of TERM."
		   "2. A list of the renamed terms."
		   "3. A list of the positions of the terms in their object."
		   "4. A list of the renaming substitutions."))
  (:method ((to-list toset+set) term)
   (labels ((all-renamed (object-list)
	      (if object-list
		  (multiple-value-bind (objects terms positions substitutions)
		      (all-renamed (cdr object-list))
		    (let* ((object (car object-list)))
		      (multiple-value-bind (subterms positions)
			  (funcall (toset~term-accessor to-list) object)
			(mapc #'(lambda (subterm position)
				  (let ((substitution (uni~renamed term subterm)))
				    (when substitution
				      (push object objects)
				      (push subterm terms)
				      (push position positions)
				      (push substitution substitutions))))
			      subterms positions)))
		    (values objects terms positions substitutions))
		  (values nil nil nil nil))))
     (all-renamed (toset~objects to-list)))))

(defgeneric toset~all-unifiable (toset term &key unify-function bindings)   ;;; noch nicht angepasst
  (declare (edited)
	   (authors RICHST)
	   (input  "A term object-set, a term.")
	   (effect "None.")
	   (value "1. A list of all objects in TOSET that contain a term which is unifiable with TERM."
		   "2. A list of the unifiable terms."
		   "3. A list of the positions of the terms in their object."
		   "4. A list of the unifying substitutions."))
  (:method ((to-list toset+set) term &key unify-function bindings)
	   (declare (ignore bindings))
   (labels ((all-unifiable (objects uni-func)
	      (if objects
		  (multiple-value-bind (objects terms positions substitutions)
		      (all-unifiable (cdr objects) uni-func)
		    (let* ((object (car objects)))
		      (multiple-value-bind (subterms positions)
			  (funcall (toset~term-accessor to-list) object)
			(mapc #'(lambda (subterm position)
				  (let ((substitution
					 (funcall uni-func term subterm)))
				    (when substitution
				      (push object objects)
				      (push subterm terms)
				      (push position positions)
				      (push substitution substitutions))))
			      subterms positions)))
		    (values objects terms positions substitutions))
		  (values nil nil nil nil))))
     (all-unifiable (toset~top-terms (toset~objects to-list))
		    (if unify-function unify-function
		      #'hou~pre-unify-terms))))
  (:method ((index toset+index) term &key unify-function bindings)
	   (declare (ignore unify-function))
	   (values (stind~retrieve-term term (toset~index index) :bindings bindings)
		   nil nil nil)))


(defgeneric toset~all-same-top (toset term)  
  (declare (edited)
	   (authors KOHLHASE CHRIS)
	   (input  "A term object-set, and a termT.")
	   (effect "None.")
	   (value "1. A list of all objects in TOSET that contain a term"
		  "which has the same topsymbol as TERM or a variable as"
		  "tobsymbol. If the topsymbol of term is itself a variable"
		  "we get all objects of toset."
		  "2. A list of lists of terms, such that for each object"
		  "there is a list that contains the terms"
		  " with the same topsymbol in that object."
		  "3. A list of lists of positions of the terms in their object."))
  (:method ((to-list toset+set) term)
	   (let ((objects nil)
		 (termlists nil)
		 (positionlists nil)
		 (top (data~top term)))
	     (mapc #'(lambda (obj)
		       (let* ((terms nil)
			      (positions nil))
			 (multiple-value-bind (auxterms auxpositions)
			     (funcall (toset~term-accessor to-list) obj)
			   (mapc #'(lambda (term2 position)
				     (when (or
					    (data~variable-p top)
					    (keim~equal (data~top term2) top)
					    (data~variable-p (data~top term2)))
				       (push term2 terms)
				       (push position positions)))
			      auxterms auxpositions))
			 (when terms
			   (progn
			     (push obj objects)
			     (push terms termlists)
			     (push positions positionlists)))))
		   (toset~objects to-list))
	     (values objects termlists positionlists))))

	   




(defgeneric toset~all-instances (toset term &key bindings) ;;; noch nicht angepasst
  (declare (edited)
	   (authors RICHTS)
	   (input  "A termobject-set, a term.")
	   (effect "None.")
	   (value "1. A list of all objects in TOSET that contain a subterm which is a instance of TERM,"
		   "   i.e. there exists a substitution sigma with sigma(TERM) = subterm."
		   "2. A list of the terms which are instances."
		   "3. A list of the positions of the terms in their object."
		   "4. A list of the substitutions."))
  (:method ((to-list toset+set) term &key bindings)
	   (declare (ignore bindings))
	   (labels ((all-instances (objects)
	      (if objects
		  (multiple-value-bind (objects terms positions substitutions)
		      (all-instances (cdr objects))
		    (let* ((object (car objects)))
		      (multiple-value-bind (subterms positions)
			  (funcall (toset~term-accessor to-list) object)
			(mapc #'(lambda (subterm position)
				  (let ((substitution (uni~match term subterm)))
				    (when substitution
				      (push object objects)
				      (push subterm terms)
				      (push position positions)
				      (push substitution substitutions))))
			      subterms positions)))
		    (values objects terms positions substitutions))
		  (values nil nil nil nil))))
     (all-instances (toset~objects to-list))))
  (:method ((index toset+index) term &key bindings)
	   (values (stind~retrieve-term term (toset~index index) :mode :inst :bindings bindings)
		   nil nil nil)))

(defgeneric toset~all-general (toset term &key bindings)  ;;; noch nicht angepasst
  (declare (edited)
	   (authors RICHTS)
	   (input  "A termobject-set, a term and a termset-keyword that is defined in TOSET.")
	   (effect "None.")
	   (value "1. A list of all objects in TOSET that contain a subterm which is more general than TERM,"
		   "   i.e. there exists a substitutions sigma with sigma(subterm) = TERM."
		   "2. A list of the general terms."
		   "3. A list of the positions of the terms in their object."
		   "4. A list of the substitutions."))
  (:method ((to-list toset+set) term &key bindings)
	   (declare (ignore bindings))
	   (labels ((all-general (objects)
	      (if objects
		  (multiple-value-bind (objects terms positions substitutions)
		      (all-general (cdr objects))
		    (let* ((object (car objects)))
		      (multiple-value-bind (subterms positions)
			  (funcall (toset~term-accessor to-list) object)
			(mapc #'(lambda (subterm position)
				  (let ((substitution (uni~match subterm term)))
				    (when substitution
				      (push object objects)
				      (push subterm terms)
				      (push position positions)
				      (push substitution substitutions))))
			      subterms positions)))
		    (values objects terms positions substitutions))
		  (values nil nil nil nil))))
     (all-general (toset~objects to-list))))
  (:method ((index toset+index) term &key bindings)
	   (values (stind~retrieve-term term (toset~index index) :mode :gen :bindings bindings)
		   nil nil nil)))



(defgeneric toset~all-subterms (toset object &key mode bindings)
  (declare (edited  "15-APR-1997")
	   (authors Lklein)
	   (input   "A term object-set and an object.")
	   (effect  "None.")
	   (value   "A list of all subterms."))
  (:method ((index toset+index) object &key mode bindings)
	   (if (eq :mode :of-index)
	       (stind~retrieve-subterms-of-index object index :bindings bindings)
	     (stind~retrieve-subterms-of-term object index :bindings bindings))))
	   







(defmethod print-object ((object toset+set) stream)
  (format stream "\{ ~A \}" (toset~elements object)))

(defmethod print-object ((object toset+index) stream)
  (format stream "\{ ~A \}" (keim~name object) (toset~elements object)))
