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

(IN-PACKAGE "KEIM")



(mod~defmod LIT 
            :uses (data env keim logic pos post)
            :documentation "Abstract datatype for literals and constraints."
            :exports (
                      lit+constraint
                      lit+literal
                      
                      lit~add-clause-position!
                      lit~atom
                      lit~clause
                      lit~clause-position
                      lit~clauses
                      lit~constraint-create
                      lit~constraint-p
                      lit~literal-create
                      lit~literal-p
                      lit~negative-p
                      lit~polarity
                      lit~positive-p
                      lit~proper-literal-p
                      lit~read
                      lit~remove-clause-position!
                      lit~weight
                      
                      lit*literal-counter))

#{
\section{ Literals}
\label{mod:lit}

This module provides basic datastructures and algorithms for literals,
as they would be used in clauses.

A literal is an object with the following slots: the clause in which
it occurs, its atom and its polarity. To make available a mapping from literals
to their clauses literals have an additional slot CLAUSE-POSITION that
contains a list of Pairs (clause pos) specifying the clause and the position
where the literal occurs. This slot is set when
a clause is created (see module clause, section \ref{mod:cl}). Additionally
we provide a slot weight for storing the weight of literal.
#}
 
(defvar lit*literal-counter 0)


(eval-when (load compile eval)
(defclass lit+literal (data+top keim+name)
    ((clause-position :initarg :clause-position :accessor lit~clause-position)
     (atom :initarg :atom :accessor lit~atom)
     (polarity :initarg :polarity :accessor lit~polarity
	       :documentation "This slot is t if the literal is positive, nil if it is negated.") 
     (weight :initarg :weight :accessor lit~weight))
    (:documentation "The class of literals, i.e. atoms or negated atoms.")))



(eval-when (load compile eval)
(defclass lit+constraint (lit+literal)
    ()
    (:documentation "The class of constraints, i.e. a subclass of literals.")))


(defun lit~literal-create (term polarity &key name (name-prefix "L-") weight)
  (declare (edited  "1-7-1992")
	   (authors "RICHTS,Chris")
	   (input   "A term with type o, a flag, a string (name) or a name-prefix and a weight.")
	   (effect  "None.")
	   (value   "A newly created literal with TERM as its atom, the polarity POLARITY and"
		    "the name NAME if NAME was supplied or the name"
		    "'<NAME-PREFIX>-<number>' else;"
		    "clause-position is a slot linking the literal to clauses by storing a"
		    "list of pairs (cl pos) and weigth is"
		    "a number, specifying the weight of the literal."))
  (make-instance 'lit+literal
		 :atom term
		 :polarity polarity
		 :name (if name (string name)
			 (format nil "~A~A" name-prefix (incf lit*literal-counter)))
		 :clause-position nil
		 :weight weight))

(defun lit~constraint-create (term polarity &key name (name-prefix "L-") weight)
  (declare (edited  "22-FEB-1997")
	   (authors "RICHTS,Chris")
	   (input   "A term with type o, a flag, a string (name) or a name-prefix and a weight.")
	   (effect  "None.")
	   (value   "A newly created literal with TERM as its atom,"
		    "the polarity POLARITY and"
		    "the name NAME if NAME was supplied or the name"
		    "'<NAME-PREFIX>-<number>' else, and weigth is"
		    "a number, specifying the weight of the literal."))
  (make-instance 'lit+constraint
		 :atom term
		 :polarity polarity
		 :clause-position nil
		 :name (if name (string name)
			 (format nil "~A~A" name-prefix (incf lit*literal-counter)))
		 :weight weight))


(defmethod lit~add-clause-position! ((object lit+literal) cl pos)
    (declare (edited  "22-FEB-1997")
	   (authors Chris)
	   (input   "A literal, a clause and a position.")
	   (effect  "Destructively adds the pair (cl pos) to the clause-position slot")
	   (value   "The changed literal."))
  (setf (lit~clause-position object)
	(if (assoc cl (lit~clause-position object))
	    (warn "The literal ~A is already in clause ~A" object cl)
	  (cons (cons cl pos) (lit~clause-position object)))))


(defmethod lit~remove-clause-position! ((object lit+literal) cl)
    (declare (edited  "22-FEB-1997")
	   (authors Chris)
	   (input   "A literal, a clause cl")
	   (effect  "Destructively removes the pair (cl pos) from the clause-position slot.")
	   (value   "The changed literal."))
  (setf (lit~clause-position object)
	(remove (assoc cl (lit~clause-position object))
		(lit~clause-position object)
		:test #'keim~equal)))



 
(defmethod data~reader ((object lit+literal))
  (declare (edited  "22-FEB-1997")
	   (authors Chris)
	   (input   "a literal (lit+literal)")
	   (effect  "none")
	   (value   "the atom of the literal"))
  (lit~atom object))
	   

(defun lit~literal-p (object)
  (declare (edited  "13-NOV-1996")
	   (authors Chris)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if OBJECT is a literal, otherwise NIL."))
  (typep object 'lit+literal))

(defun lit~proper-literal-p (object)
  (declare (edited  "13-NOV-1996")
	   (authors Chris)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if OBJECT is a proper literal (no constraint), otherwise NIL."))
  (and (typep object 'lit+literal)
       (not (lit~constraint-p object))))


(defun lit~constraint-p (literal)
  (declare (edited  "13-NOV-1996")
	   (authors CHRIS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "T if  LITERAL is negative, otherwise NIL."))
  (typep literal 'lit+constraint))


(defgeneric lit~positive-p (literal)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "T if  LITERAL is positive (not negated), otherwise NIL."))
  (:method ((literal lit+literal))
	    (lit~polarity literal)))

(defgeneric lit~negative-p (literal)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "T if  LITERAL is negative, otherwise NIL."))
  (:method ((literal lit+literal))
	    (not (lit~polarity literal))))

(defun lit~clause (literal)
  (declare (edited  "19-FEB-1998")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "The clause of the first cons-pair in the clause-position slot of"
		    "LITERAL. If the clause-position sloy is nil, nil is returned."))
  (let* ((clause-position (lit~clause-position literal)))
    (if (> (length clause-position) 1)
	(error "~% Problem in Function lit\~clause, the clause-position ~A contains more than one entry." clause-position)
      (if clause-position
	  (car (car clause-position))
	nil))))

(defun lit~clauses (literal)
  (declare (edited  "19-FEB-1998")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A list of all clauses contained as first elements in the cons-pairs"
		    "of the clause-position slot of literal."))
  (mapcar #'car (lit~clause-position literal)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod data=linear-p ((literal lit+literal))
  (data=linear-p (lit~atom literal)))

(defmethod data~substructs ((literal lit+literal))
  (data~substructs (lit~atom literal)))

(defmethod data~positions ((literal lit+literal) test)
  (let ((subpositions (data~positions (lit~atom literal) test)))
    (mapl #'(lambda (positions-tail)
	      (setf (car positions-tail) (pos~add-front 1 (car positions-tail))))
	  subpositions)
    (if (funcall test literal)
	(cons (pos~empty) subpositions)
	subpositions)))

(defmethod data~position ((literal lit+literal) test)
  (if (funcall test literal)
      (pos~empty)
      (let ((position (data~position (lit~atom literal) test)))
	(if position (pos~add-front 1 position) nil))))

(defmethod data~struct-at-position ((literal lit+literal) position)
  (cond ((pos~empty-p position) literal)
	((= 1 (pos~first position)) (data~struct-at-position (lit~atom literal) (pos~rest position)))
	(t (error "Position ~A not in literal ~A." position literal))))


(defmethod data=replace-at-position ((literal lit+literal) position struct destructive downto)
  (cond ((pos~empty-p position) literal)
	((= 1 (pos~first position))
	 (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
	     (progn
	       (setf (lit~atom literal)
		     (data=replace-at-position (lit~atom literal) (pos~rest position) struct 't downto))
	       literal)
	   (lit~literal-create (data=replace-at-position (lit~atom literal) (pos~rest position) struct destructive downto)
			       (lit~positive-p literal))))
	(t (error "Position ~A not in literal ~A." position literal))))

(defmethod data=replace-structs ((literal lit+literal) old-structs new-structs destructive downto replacers-downto test) 
  (let* ((assoc-struct (data~assoc literal old-structs new-structs test)))
    (if assoc-struct
	(data~copy assoc-struct
		   :downto replacers-downto
		   :explode nil :preserve :all-classes) 
      (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
	  (progn (setf (lit~atom literal)
		       (data=replace-structs (lit~atom literal) old-structs new-structs 't downto replacers-downto test))
		 literal)
	(lit~literal-create (data=replace-structs (lit~atom literal) old-structs new-structs destructive downto replacers-downto test) 
			    (lit~positive-p literal))))))

(defmethod data=replace-fv ((literal lit+literal) bound domain codomain destructive downto test)
  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
      (setf (lit~atom literal)
	    (data=replace-fv (lit~atom literal) bound domain codomain 't downto test))
    (lit~literal-create (data=replace-fv (lit~atom literal) bound domain codomain destructive downto test)
			(lit~positive-p literal))))


(defmethod data=replace-at-position ((literal lit+constraint) position struct destructive downto)
  (cond ((pos~empty-p position) literal)
	((= 1 (pos~first position))
	 (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
	     (setf (lit~atom literal)
		   (data=replace-at-position (lit~atom literal) (pos~rest position) struct 't downto))
	   (lit~constraint-create (data=replace-at-position (lit~atom literal) (pos~rest position) struct destructive downto)
				  (lit~positive-p literal))))
	(t (error "Position ~A not in literal ~A." position literal))))

(defmethod data=replace-structs ((literal lit+constraint) old-structs new-structs destructive downto replacers-downto test) 
  (let* ((assoc-struct (data~assoc literal old-structs new-structs test)))
    (if assoc-struct
	(data~copy assoc-struct
		   :downto replacers-downto
		   :explode nil :preserve :all-classes) 
      (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
	  (setf (lit~atom literal)
		(data=replace-structs (lit~atom literal) old-structs new-structs 't downto replacers-downto test))
	(lit~constraint-create (data=replace-structs (lit~atom literal) old-structs new-structs destructive downto
						     replacers-downto test) 
			       (lit~positive-p literal))))))

(defmethod data=replace-fv ((literal lit+constraint) bound domain codomain destructive downto test)
  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep literal x)) downto))
      (setf (lit~atom literal)
	    (data=replace-fv (lit~atom literal) bound domain codomain 't downto test))
    (lit~constraint-create (data=replace-fv (lit~atom literal) bound domain codomain destructive downto test)
			   (lit~positive-p literal))))

      

(defmethod data~free-variables ((literal lit+literal))
  (remove-duplicates (data~free-variables (lit~atom literal))))

(defmethod data~bound-variables ((literal lit+literal))
  (data~bound-variables (lit~atom literal)))

(defmethod logic~all-bound-variables ((literal lit+literal))
  (logic~all-bound-variables (lit~atom literal)))

(defmethod logic~ground-p ((literal lit+literal))
  (logic~ground-p (lit~atom literal)))

(defmethod keim~copy ((literal lit+literal) &key (explode :all-classes) share preserve downto)
  (data~copy literal
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim~copy ((literal lit+constraint) &key (explode :all-classes) share preserve downto)
  (data~copy literal
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod data=copy ((literal lit+literal) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep literal x)) downto)) 
      literal
    (let* ((atom-copy (data=copy (lit~atom literal) renaming am explode share preserve downto))
	   (new-atom (first atom-copy))
	   (new-ren (second atom-copy)))
      (list (lit~literal-create new-atom (lit~positive-p literal))
	    new-ren))))

(defmethod data=copy ((literal lit+constraint) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep literal x)) downto)) 
      literal
    (let* ((atom-copy (data=copy (lit~atom literal) renaming am explode share preserve downto))
	   (new-atom (first atom-copy))
	   (new-ren (second atom-copy)))
      (list (lit~constraint-create new-atom (lit~positive-p literal))
	    new-ren))))
				   
(defmethod keim~equal ((literal1 lit+literal) (literal2 lit+literal))
  (data~equal literal1 literal2))

(defmethod data~equal ((literal1 lit+literal) (literal2 lit+literal))
  (and (or (and (lit~positive-p literal1) (lit~positive-p literal2))
	   (and (not (lit~positive-p literal1)) (not (lit~positive-p literal2))))
       (data~equal (lit~atom literal1) (lit~atom literal2))))

(defmethod data~equal-p ((literal1 lit+literal) (literal2 lit+literal)  &key mode destructive)
  (and (or (and (lit~positive-p literal1) (lit~positive-p literal2))
	   (and (not (lit~positive-p literal1)) (not (lit~positive-p literal2))))
       (data~equal-p (lit~atom literal1) (lit~atom literal2)
		     :mode mode
		     :destructive destructive)))

(defmethod data~alpha-equal ((literal1 lit+literal) (literal2 lit+literal) bound1 bound2 bindables)
  (and (or (and (lit~positive-p literal1) (lit~positive-p literal2))
	   (and (not (lit~positive-p literal1)) (not (lit~positive-p literal2))))
       (data~alpha-equal (lit~atom literal1) (lit~atom literal2) bound1 bound2 bindables)))

(defmethod print-object ((literal lit+literal) stream)
  (format stream "~A~A" (if (lit~positive-p literal) "+" "-") (lit~atom literal)))

(defmethod print-object ((literal lit+constraint) stream)
  (format stream "*~A" (lit~atom literal)))

(defmethod post~read-object (literal (env env+environment) (indicator (eql :literal)))
  (let* ((pol (first literal)))
    (if (not (or (eq pol '+) (eq pol '-) (eq pol '*)))
	(error "Literal ~A should begin with +, - or *." literal)
      (cond ((eq pol '+)
	     (lit~literal-create (post~read-object (second literal) env :existing-term) 't))
	    ((eq pol '-)
	     (lit~literal-create (post~read-object (second literal) env :existing-term) nil))
	    ((eq pol '*)
	     (lit~constraint-create (post~read-object (second literal) env :existing-term) nil))))))

(defmethod post~print ((literal lit+literal) stream)
  (format stream "(~A " (if (lit~positive-p literal) '+ '-))
  (post~print (lit~atom literal) stream)
  (format stream ")"))

(defmethod post~print ((literal lit+constraint) stream)
  (format stream "(* ")
  (post~print (lit~atom literal) stream)
  (format stream ")"))



(defun lit~read (lit env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A literal in \\POST format and an environment.")
	   (effect  "The literal is constructed.")
	   (value   "The new literal is returned.")
	   (example "\\vb (lit~read '(+ (P a)) env)."
		    "\\vb (lit~read '(- (Q y)) env)."))
  (post~read-object lit env :literal))



#| WAS IS MIT FOLGENDEN BEIDEN METHODEN:
   Braucht die wer ? AMEIER

   (defmethod hop~beta-normform ((object lit+literal))
  (lit~literal-create (hop~beta-normform (lit~atom object))
		      (lit~positive-p object)))

(defmethod uni=unify-free-terms-rec ((term1 lit+literal) (term2 lit+literal) termlist1 termlist2 &key eq-test)
  (uni=unify-free-terms-rec (lit~atom term1) (lit~atom term2) termlist1 termlist2 :eq-test eq-test))

|#

