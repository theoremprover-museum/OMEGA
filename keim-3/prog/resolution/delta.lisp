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

(in-package "KEIM")


(mod~defmod DELTA 
            :uses (cl keim pos post termix)
            :documentation "Abstract datatype for the delta relation."
            :exports (
                      delta+relation
                      delta+relation-pair
                      
                      delta~add-pair!
                      delta~add-prefix-delta!
                      delta~create
                      delta~create-relation
                      delta~delta-clause
                      delta~delta-formula
                      delta~delta-position-in-clause
                      delta~delta-position-in-formula
                      delta~get-atom
                      delta~get-literals
                      delta~get-subrelations
                      delta~pair-create
                      delta~print-object
                      delta~read
                      delta~relation-p
                      delta~relation-pair-p
                      delta~relation-pairs
                      delta~remove-pair!
                      delta~replace!
                      ))





#{\section{Delta relations}\label{mod:delta}
 A delta relation keeps track of the relationships between literals in
in clauses and their origins in the input formulas.   For example, one may want
 to know where an atomic subterm of a formula occurs after it has been put in
 conjunctive normal form.
#}
(eval-when (load compile eval)
  (defclass delta+relation (keim+object)
    ((relation-pairs :initarg :relation-pairs
		     :accessor delta~relation-pairs
		     :initform nil
		     :documentation "This is a list of delta+relation-pairs."))
    (:documentation "This is the structure for the delta relation.")))


(defun delta~relation-p (object)
  (declare (edited  "06-MAR-1998")
	   (authors Ameier)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "T if the object is of class delta+relation, nil otherwise."))
  (typep object 'delta+relation))

  
(eval-when (load compile eval)
  (defclass delta+relation-pair (keim+object)
    ((delta-formula :initarg :delta-formula
		    :accessor delta~delta-formula 
		    :initform nil
		    :documentation "This is the formula of the delta pair.")
     (delta-position-in-formula :initarg :delta-position-in-formula
				:accessor delta~delta-position-in-formula
				:initform (pos~empty)
				:documentation "The position of the atomic occurrence in the formula.")
     (delta-clause :initarg :delta-clause
		   :accessor delta~delta-clause
		   :initform nil
		   :documentation "This is the clause of the relation pair.")
     (delta-position-in-clause :initarg :delta-position-in-clause
			       :accessor delta~delta-position-in-clause
			       :initform (pos~empty)
			       :documentation "The position of the literal in the clause."))
    (:documentation "This is the structure for the relation pair.")))

(defun delta~relation-pair-p (object)
  (declare (edited  "06-MAR-1998")
	   (authors Ameier)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "T if the object is of class delta+relation-pair, nil otherwise."))
  (typep object 'delta+relation-pair))


(defun delta~create (delta-pair-list)
  (declare (edited  "10-FEB-1993 12:17")
	   (authors ACSEHN)
	   (input "a list of delta+relation-pair pairs."  )
	   (effect "none." )
	   (value "a delta relation."  ))
  (if (delta=delta-pair-list-p delta-pair-list)
      (make-instance 'delta+relation :relation-pairs delta-pair-list)
    (error "The parameter ~A is not a list consisting of delta+relaton-pairs." delta-pair-list)))


(defun delta=delta-pair-list-p (delta-pair-list)
  (declare (edited  "10-FEB-1993 12:20")
	   (authors ACSEHN)
	   (input "a list of objects."  )
	   (effect "none." )
	   (value "T, iff all objects in the list are delta+relation-pair objects."  ))
  (every #'delta~relation-pair-p delta-pair-list))


(defgeneric delta~pair-create (delta-formula delta-position-in-formula delta-clause delta-position-in-clause)
  (declare (edited  "31-MAR-1993 16:42")
	   (authors ACSEHN)
	   (input "a named formula, a position of an atomic occurrence in formula, a literal"
		  ", specified by a clause and a position of the literal in the clause.")
	   (effect "None." )
	   (value "A delta relation pair."  ))
  (:method ((delta-formula termix+named-term)
	    (delta-position-in-formula pos+position)
	    (delta-clause cl+clause)
	    (delta-position-in-clause pos+position))
	   (make-instance 'delta+relation-pair 
			  :delta-formula delta-formula
			  :delta-position-in-formula delta-position-in-formula
			  :delta-clause delta-clause
			  :delta-position-in-clause delta-position-in-clause)))
  
(defgeneric delta~add-pair! (delta-relation formula position clause position-of-literal)
  (declare (edited  "31-MAR-1993 16:42")
	   (authors ACSEHN)
	   (input "a delta relation, a named formula, a position of an atomic occurrence"
		  "of this formula, and a position of the literal in the clause.")
	   (effect "a new relation pair is created and added to the delta-relation." )
	   (value "the altered delta relation."  ))
  (:method ((delta-relation delta+relation)
	    (formula termix+named-term)
	    (position pos+position)
	    (clause cl+clause)
	    (position-of-literal pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (new-pair (delta~pair-create formula position clause position-of-literal))
		  (new-relation-pairs (cons new-pair relation-pairs)))
	     (setf (delta~relation-pairs delta-relation) new-relation-pairs)
	     delta-relation)))

(defun delta~create-relation ()
  (declare (edited  "10-FEB-1993 19:48")
	   (authors ACSEHN)
	   (input "None."  )
	   (effect "None."  )
	   (value "An empty delta relation."  ))
  (delta~create nil))

;; hier noch genau checken
(defmethod keim~copy ((object delta+relation) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep object x)) downto))
      object
    (let* ((new-relation (delta~create-relation)))
      (setf (delta~relation-pairs new-relation)
	    (keim~copy (delta~relation-pairs object)
		       :explode explode
		       :share share
		       :preserve preserve
		       :downto downto)))))

(defmethod keim~copy ((object delta+relation-pair) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep object x)) downto))
      object
    (delta~pair-create (keim~copy (delta~delta-formula object)
				  :explode explode
				  :share share
				  :preserve preserve
				  :downto downto)
		       (keim~copy (delta~delta-position-in-formula object)
				  :explode explode
				  :share share
				  :preserve preserve
				  :downto downto)
		       (keim~copy (delta~delta-clause object)
				  :explode explode
				  :share share
				  :preserve preserve
				  :downto downto)
		       (keim~copy (delta~delta-position-in-clause object)
				  :explode explode
				  :share share
				  :preserve preserve
				  :downto downto))))

(defgeneric delta~remove-pair! (delta-relation formula position clause position-of-literal)
  (declare (edited  "31-MAR-1993 16:42")
	   (authors ACSEHN)
	   (input  "A delta relation."
		   "A named formula, a position of an atomar occurence in this formula,"
		   "a clause and a position of a literal in this clause.")
	   (effect "The relation pair with these contents is removed from the delta-relation." )
	   (value "The altered DELTA-RELATION."  ))
  (:method ((delta-relation delta+relation)
	    (formula termix+named-term)
	    (position pos+position)
	    (clause cl+clause)
	    (position-of-literal pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair)
				     (if (and (keim~equal (delta~delta-formula pair) formula)
					      (keim~equal (delta~delta-position-in-formula pair) position)
					      (keim~equal (delta~delta-clause pair) clause)
					      (keim~equal (delta~delta-position-in-clause pair) position-of-literal))
					 (list pair)
					 nil))
				 relation-pairs)))
	     (setf (delta~relation-pairs delta-relation) (set-difference relation-pairs pairs))
	     delta-relation)))

(defmethod keim~equal ((pair1 delta+relation-pair) (pair2 delta+relation-pair))
  (declare (edited  "31-MAR-1993 17:08")
	   (authors ACSEHN)
	   (input "two objects."  )
	   (effect "none." )
	   (value "T, iff the two objects are equal."  ))
  (let ((formula1 (delta~delta-formula pair1))
	(pos-of-formula1 (delta~delta-position-in-formula pair1))
	(clause1 (delta~delta-clause pair1))
	(pos-of-clause1 (delta~delta-position-in-clause pair1))
	(formula2 (delta~delta-formula pair2))
	(pos-of-formula2 (delta~delta-position-in-formula pair2))
	(clause2 (delta~delta-clause pair2))
	(pos-of-clause2 (delta~delta-position-in-clause pair2)))
    (and (keim~equal formula1 formula2)
	 (keim~equal pos-of-formula1 pos-of-formula2)
	 (keim~equal clause1 clause2)
	 (keim~equal pos-of-clause1 pos-of-clause2))))

(defgeneric delta~get-literals (delta-relation formula position)
  (declare (edited  "31-MAR-1993 16:45")
	   (authors ACSEHN)
	   (input   "Delta-relation, a named formula and a position.")
	   (effect "none.")
	   (value   "1. a list of all clauses that are in delta-relation to the position in formula, and"
		    "2. a list of all literals that are in delta-relation to the position in formula."))
  (:method ((delta-relation delta+relation)
	    (formula termix+named-term)
	    (position pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair) (if (and (keim~equal (delta~delta-formula pair) formula)
							   (keim~equal (delta~delta-position-in-formula pair) position))
						      (list pair)
						      nil))
				 relation-pairs))
		  (all-clauses (mapcar #'(lambda (pair) (delta~delta-clause pair)) pairs))
		  (all-positions (mapcar #'(lambda (pair) (delta~delta-position-in-clause pair)) pairs)))
	     (values all-clauses all-positions))))


(defgeneric delta~get-atom (delta-relation clause position)
  (declare (edited  "31-MAR-1993 17:18")
	   (authors ACSEHN)
	   (input   "A delta relation, a clause and a position.")
	   (effect  "None.")
	   (value   "The atomic subformular which with the literal specified by CLAUSE and POSITION"
		    "is linked in DELTA-RELATION1, i.e."
		    "1. the named formula and"
		    "2. the position of the atom."))
  (:method ((delta-relation delta+relation) (clause cl+clause) (position pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair) (if (and (keim~equal (delta~delta-clause pair) clause)
							   (keim~equal (delta~delta-position-in-clause pair) position))
						      (list pair)
						      nil))
				 relation-pairs))
		  (formula-in (delta~delta-formula (first pairs)))
		  (position-formula (delta~delta-position-in-formula (first pairs))))
	     (values formula-in position-formula))))


(defun delta=pairs-with-formula (formula delta)
  (declare (edited  "31-MAR-1993 17:42")
	   (authors ACSEHN)
	   (input "a formula and a delta-relation."  )
	   (effect "none." )
	   (value "a list of relation pairs where formula is the delta-formula."  ))
  (let* ((relation-pairs (delta~get-subrelations delta))
	 (pairs (mapcan #'(lambda (pair) (if (keim~equal (delta~delta-formula pair)
							 formula)
					     (list pair)
					     NIL))
			relation-pairs)))
    pairs))

(defun delta~add-prefix-delta! (delta new-theorem subformula pos-prefix)
  (declare (edited  "24-FEB-1993 11:36")
	   (authors ACSEHN)
	   (input "a delta-relation, a named new-theorem of the form `(not (implies (and ass-1 (and ass-2 ..."
		  "(and ass-n-2 ass-n-1))) th)' a subformula in new-theorem and a prefix of a position" )
	   (effect "the pos-prefix is appended to the front of the positions of all pairs where subformula is in. "  )
	   (value "the altered relation pairs."  ))
  (let ((pairs-with-formula (delta=pairs-with-formula subformula delta)))
    (mapcar #'(lambda (pair) (delta=add-prefix-to-delta-pair! pair new-theorem pos-prefix)) pairs-with-formula)
    pairs-with-formula))

	
(defun delta=add-prefix-to-delta-pair! (pair new-theorem pos-prefix)
  (declare (edited  "24-FEB-1993 18:20")
	   (authors ACSEHN)
	   (input "a pair, a new-theorem and a pos-prefix."  )
	   (effect "in the pair the delta formula is set to new-theorem and the pos-prefix is appended to the "
		   "front of the delta-position-in-formula of that pair.")
	   (value "the altered pair."  ))
  (let* ((pos-in-formula (delta~delta-position-in-formula pair))
	 (new-pos (pos~concatenate pos-prefix pos-in-formula)))
    (setf (delta~delta-formula pair) new-theorem)
    (setf (delta~delta-position-in-formula pair) new-pos)
    pair))


;--------

(defgeneric delta~get-subrelations (delta-relation)
  (declare (edited  "12-FEB-1993 15:13")
	   (authors ACSEHN)
	   (input   "A delta-relation.")
	   (effect  "None.")
	   (value   "A list of all sub-relations in delta-relation."))
  (:method ((delta-relation delta+relation))
	   (delta~relation-pairs delta-relation)))

; Input / Output

(defun delta~print-object (object &optional (stream t))
  (declare (edited  "10-FEB-1993 19:42")
	   (authors ACSEHN)
	   (input "an delta object and a stream, optionally the standard output."   )
	   (effect "the object is printed on the stream."  )
	   (value "nil." ))
  (delta=print-object object stream))

(defgeneric delta=print-object (object stream)
  (declare (edited  "12-AUG-1992 14:54")
	   (authors ACSEHN)
	   (input "an Delta-object OBJECT and a stream."  )
	   (effect "the object is written on the stream." )
	   (value "none."  ))
  (:method ((object delta+relation) stream)
	   (delta=print-object-relation object stream))
  (:method ((object delta+relation-pair) stream)
	   (delta=print-object-relation-pair object stream))
  (:method ((list list) stream)
	   (mapcar #'(lambda (x) (delta=print-object x stream)) list)
	   'ok))

(defun delta=print-object-relation-pair (object stream)
  (format stream "~%~% formula: ~A " (delta~delta-formula object))
  (format stream " position: ~A " (delta~delta-position-in-formula object))
  (format stream " clause: ~A " (delta~delta-clause object))
  (format stream " position: ~A " (delta~delta-position-in-clause object)))

(defun delta=print-object-relation (object stream)
  (let ((relation-pairs (delta~relation-pairs object)))
    (delta=print-object relation-pairs stream)))


#{\subsection{POST interface}#}

(defmethod print-object ((object delta+relation) stream)
  (let ((relation-pairs (delta~relation-pairs object)))
    (cond ((or (null *print-length*) 
	       (> *print-length* (length relation-pairs)))
	   (format stream "(delta~{ ~A~} ...)"
		   (subseq relation-pairs 0 3)))
	  (t (format stream "(delta~{ ~A~} ...)"
		     (subseq relation-pairs 0 (1- *print-length*)))))))

(defmethod print-object ((object delta+relation-pair) stream)
  (format stream "(~A , ~A) * (~A , ~A)"
	  (delta~delta-formula object)
	  (delta~delta-position-in-formula object)
	  (delta~delta-clause object)
	  (delta~delta-position-in-clause object)))


(defmethod post~print ((object delta+relation) stream)
  (let ((relation-pairs (delta~relation-pairs object)))
    (format stream "(delta-relation ")
    (mapc #'(lambda (relation-pair)
	      (format stream " ")
	      (post~print relation-pair stream))
	  relation-pairs)
    (format stream ")")))

(defmethod post~print ((object delta+relation-pair) stream)
  (format stream "(~S" (keim~name (delta~delta-formula object)))
  (format stream " (")
  (post~print (delta~delta-position-in-formula object) stream)
  (format stream " (~A" (keim~name (delta~delta-clause object)))
  (format stream " ")
  (post~print (delta~delta-position-in-clause object) stream)
  (format stream ")))"))


(defun delta~read (thing env)
  (unless (and (listp thing)
	       (symbolp (car thing))
	       (string= "DELTA-RELATION" (car thing)))
    (post~error "~A does not match specification for a DELTA-RELATION declaration" thing))
  (post~read-object (cdr thing) env :delta-relation))

#{\begin{postsyntax}
\syntax{\nt{delta-relation}  ::=  (delta-relation \nt{relation-pair}*).
\nt{relation-pair}  ::= (\nt{term-name} (\nt{position} (\nt{clause-name} \nt{position})* )* ) .
}
\end{postsyntax}
#}

(defmethod post~read-object ((cmd list) env (indicator (eql :delta-relation)))
  (let* ((dr (delta~create-relation))
	 (rels cmd))
    (dolist (rel rels dr)
      (let ((formula (termix~read-named-term (first rel) env)))
	(dolist (pos-clause-list (rest rel))
	  (let ((pos (pos~read (first pos-clause-list) env)))
	    (dolist (clause-pos (rest pos-clause-list))
	      (delta~add-pair! dr 
			       formula
			       pos
			       (cl~read (first clause-pos) env)
			       (pos~read (second clause-pos) env)))))))))


(defgeneric delta~replace! (object old-object new-object)
  (declare (edited  "03-JUN-1993 18:38")
	   (authors ACSEHN)
	   (input "an OBJECT, i.e. a delta relation and an OLD-OBJECT which can be a clause or a named term and"
		  "a NEW-OBJECT of the corresponding type.")
	   (effect "The OLD-OBJECT is replaced by the NEW-OBJECT in OBJECT."  )
	   (value "The altered OBJECT."  ))
  (:method ((relation delta+relation) (old-object cl+clause) (new-object cl+clause))
	   (let ((subrelations (delta~get-subrelations relation)))
	     (mapcar #'(lambda (pair)
			 (let ((old-clause (delta~delta-clause pair))
			       (old-pos-in-clause (delta~delta-position-in-clause pair))
			       (old-formula (delta~delta-formula pair))
			       (old-pos-in-formula (delta~delta-position-in-formula pair)))
			   (when (keim~equal old-clause old-object)
			     (progn (delta~remove-pair! relation old-formula
							old-pos-in-formula
							old-clause
							old-pos-in-clause)
				    (delta~add-pair! relation old-formula
						     old-pos-in-formula
						     new-object
						     old-pos-in-clause)))))
		     subrelations)
	     relation))
  (:method ((relation delta+relation) (old-object termix+named-term) (new-object termix+named-term))
	   (let ((subrelations (delta~get-subrelations relation)))
	     (mapcar #'(lambda (pair)
			 (let ((old-clause (delta~delta-clause pair))
			       (old-pos-in-clause (delta~delta-position-in-clause pair))
			       (old-formula (delta~delta-formula pair))
			       (old-pos-in-formula (delta~delta-position-in-formula pair)))
			   (when (keim~equal old-formula old-object)
			     (progn (delta~remove-pair! relation old-formula
							old-pos-in-formula
							old-clause
							old-pos-in-clause)
				    (delta~add-pair! relation new-object
						     old-pos-in-formula
						     old-clause
						     old-pos-in-clause)))))
		     subrelations)
	     relation)))

