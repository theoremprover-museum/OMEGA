;;; -*- Syntax: Common-Lisp; Package: KEIM; Base: 10; Mode: LISP -*-
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

(IN-PACKAGE "KEIM")




(mod~defmod CL 
            :uses (beta data env keim lit logic node pos post)
            :documentation "Abstract datatype for clauses, i.e. sets of literals."
            :exports (
                      cl+clause
                      cl+ordered-clause
                      
                      cl~beta-reduce!
                      cl~clause-p
                      cl~clause-set
                      cl~constraints
                      cl~create
                      cl~create-ordered
                      cl~empty-p
                      cl~env-lookup
                      cl~insert-literal!
                      cl~length
                      cl~literals
                      cl~neg-literals
                      cl~ordered-clause-p
                      cl~ordered-env-lookup
                      cl~ordered-read
                      cl~pos-literals
                      cl~proper-length
                      cl~proper-literals
                      cl~read
                      cl~remove-literal!
                      cl~tautology
                      cl~tautology-p
                      cl~unit-p
                      cl~weight
                      cl~with-context
                      cl~with-context-list
                      
                      cl*clause-counter))



#{\section{Clauses}
\label{mod:cl}

This module provides basic datastructures and algorithms for clauses,
as they would be used in resolution. It is their most important function 
to store a list of literals, which are logically connected conjunctive.

For a more efficent computation the clause class also contains two slots
for separated lists of the positive and negative literals. It also has
a slot for a clause set which can serve to divide the whole set of
clauses during the resolution process.

Because clauses should also be the nodes in a proof, they are a
subclass of {\vb NODE+NODE}.

\subsection{General Functions}

A clause is an object with the following slots:
a list of its positive literals, a list of its negative literals
and the clause set it belongs to.
Additionally clauses provide slots for constraints, which are a subclass of literals,
and weight information.
Because a clause is also a proof
node it also inherits all slots of {\vb NODE+NODE} and the formula slot of
of {\vb NODE+NODE} is identical with the literal slot of {\vb
CL+CLAUSE}.

For representing the true clause,
the literal slot of an clause can be T. The empty clause is represented
by an empty list of literals, i.e. the literal slot is {\vb NIL}.
#}

(defvar cl*clause-counter 0 "a counter for creating unique clause names")

(eval-when (load compile eval)
  (defclass cl+clause (node+node)
    ((literals :initarg :literals :accessor cl=literals)
     (pos-literals :initarg :pos-literals :accessor cl~pos-literals)
     (neg-literals :initarg :neg-literals :accessor cl~neg-literals) 
     (clause-set :initarg :clause-set :accessor cl~clause-set)
     (constraints :initarg :constraints :accessor cl~constraints)
     (weight :initarg :weight :accessor cl~weight))
    (:documentation "The class of clauses, i.e. sets of instances of CL+CLAUSE.")))

(eval-when (load compile eval)
  (defclass cl+ordered-clause (cl+clause)
    ()))


(defun cl~create (literals &key
			   clause-set
			   (weight 0)
			   justification
			   name
			   (name-prefix "C-"))
  (declare (edited  "17-MAR-1998")
	   (authors Ameier)
	   (input   "A set of literals.")
	   (effect  "The clause-position slots of the literals are set to the new clause.")
	   (value   "The newly created clause."))
  (multiple-value-bind
      (pos-lits neg-lits cons-lits)
      (cl=split-literals literals)
    (let* ((clause (make-instance 'cl+clause
				  :literals literals
				  :pos-literals pos-lits
				  :neg-literals neg-lits
				  :constraints cons-lits
				  :name (cond ((numberp name) (format nil "~A" name))
					      ((null name) (format nil "~A~A" name-prefix (incf cl*clause-counter)))
					      (t (string name)))
				  :weight weight
				  :justification justification
				  :clause-set clause-set
				  ))
	   (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defun cl~create-ordered (literals &key
				   pos-literals
				   neg-literals
				   clause-set
				   constraints
				   (weight 0)
				   justification
				   name
				   (name-prefix "C-"))  
  (declare (edited  "1-7-1988")
	   (authors ohlbach chris)
	   (input   "A list of literals. Optional additional paramaters.")
	   (effect  "The clause-position slots of the literals are set to the new clause.")
	   (value   "The newly created ordered-clause."))
  (multiple-value-bind
      (posliterals negliterals constraints-II)
      (cl=split-literals literals)
    (let* ((pos-lits (union (reverse pos-literals) posliterals :test #'keim~equal)) ;; reverse because by the union the order
	   (neg-lits (union (reverse neg-literals) negliterals :test #'keim~equal)) ;; in the first list is reversed
	   (cons-lits (union (reverse constraints) constraints-II :test #'keim~equal))
	   (clause (make-instance 'cl+ordered-clause
				  :literals (append neg-lits pos-lits cons-lits)
				  :pos-literals pos-lits
				  :neg-literals neg-lits
				  :name (cond ((numberp name) (format nil "~A" name))
					      ((null name) (format nil "~A~A" name-prefix (incf cl*clause-counter)))
					      (t (string name)))
				  :constraints cons-lits
				  :weight weight
				  :justification justification
				  :clause-set clause-set
				  ))
	   (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    (append neg-lits pos-lits cons-lits))
      clause)))

#{The justification for a clause can store the way the clause was
constructed and represent its place in the proof tree. For further information
about nodes and justifications see the corresponding modules (section
\ref{mod:node} and \ref{mod:just} in chapter \ref{sys:problem}). Justifications for resolution, factoring and
paramodulation are defined in the resolution module (section \ref{mod:res}).

All functions in this module update the clause-position slots in the clauses
literals if its appearance
in a clause changes.#}


(defun cl~clause-p (object)
  (declare (edited  "26-Jan-93")
	   (authors nesmith)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is a CL+CLAUSE, NIL otherwise."))
  (typep object 'cl+clause))

(defun cl~ordered-clause-p (object)
  (declare (edited  "26-Jan-93")
	   (authors nesmith)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is a CL+CLAUSE, NIL otherwise."))
  (typep object 'cl+ordered-clause))


(defgeneric cl~literals (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS CHRIS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The list of literals in CLAUSE."))
  (:method ((clause cl+ordered-clause))
	   (append (cl~neg-literals clause)
		   (cl~pos-literals clause)
		   (cl~constraints clause)))
  (:method ((clause cl+clause))
	   (cl=literals clause)))


(defsetf cl~literals (clause) (literals)
  `(cond ((or (typep ,clause 'cl+ordered-clause) (typep ,clause 'cl+clause))
	  (mapc #'(lambda (literal)
		    (lit~remove-clause-position! literal ,clause))
		(cl~literals ,clause))
	  (multiple-value-bind
	      (pos-literals neg-literals constraints)
	      (cl=split-literals ,literals)
	    (setf (cl=literals ,clause) ,literals)
	    (setf (cl~pos-literals ,clause) pos-literals)
	    (setf (cl~neg-literals ,clause) neg-literals)
	    (setf (cl~constraints ,clause) constraints)
	    (setf (cl=literals ,clause)
		  (if (typep ,clause 'cl+ordered-clause)
		      (append neg-literals pos-literals constraints)
		    ,literals))
	    (let ((pos (pos~add-front -1)))
	      (mapc #'(lambda (literal)
			(lit~add-clause-position! literal ,clause (keim~copy (pos~next! pos))))
		    (if (typep ,clause 'cl+ordered-clause)
			(append neg-literals pos-literals constraints)
		      ,literals)))))
	 (t
	  (error "In setf cl~literals: ~A is not a clause" ,clause))))



(defmethod keim~copy ((clause cl+clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod data=copy ((clause cl+clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      (list clause)
    (let* ((literals-copy (data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy)))
      (list (cl~create new-literals
		       :justification (keim~copy (node~justification clause)
						 :explode explode
						 :share share
						 :preserve preserve
						 :downto downto)	       
		       :clause-set nil
		       :weight (cl~weight clause))
	    new-ren))))

(defmethod data=copy ((clause cl+ordered-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy)))
      (list (cl~create-ordered new-literals
			       :justification (keim~copy (node~justification clause)
							 :explode explode
							 :share share
							 :preserve preserve
							 :downto downto)	       
			       :clause-set nil
			       :weight (cl~weight clause))
	    new-ren))))


(defmethod print-object ((clause cl+clause) stream)
  (declare (special *print-length*))
  (cond ((cl~empty-p clause) (format stream "[]"))
	; ((cl~true-p clause) (format stream "{T}"))
	(t (format stream "{[~A] ~A" (keim~name clause) (car (cl~literals clause)))
	   (do ((i (1- (or *print-length* 0)) (1- i))
		(literals (cdr (cl~literals clause)) (cdr literals)))
	       ((or (zerop i) (null literals))
		(if literals
		    (format stream " ...}")
		  (format stream "}")))
	     (format stream " ~A" (car literals))))))


(defun cl=split-literals (literals)
  (if literals
      (multiple-value-bind (pos-literals neg-literals constraints)
	  (cl=split-literals (cdr literals))
	(cond ((lit~constraint-p (car literals))
	       (values pos-literals neg-literals (cons (car literals) constraints)))
	      ((lit~positive-p (car literals))
	       (values (cons (car literals) pos-literals) neg-literals constraints))
	      (t
	       (values pos-literals (cons (car literals) neg-literals) constraints))))	  
    (values nil nil nil)))


#{\subsection{Simple operations and predicates}#}

(defun cl~empty-p (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS CHRIS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "True, iff CLAUSE is empty, i.e. it has zero literals."))
  (null (cl~literals clause)))

(defun cl~unit-p (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS CHRIS )
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "True, iff CLAUSE is a unit clause, i.e. it has exactly one literal."))
  (= 1 (length (cl~proper-literals clause))))


(defgeneric cl~proper-literals (clause)
  (declare (edited  "17-FEB-1998")
	   (authors Chris)
	   (input   "A clause")
	   (effect  "none")
	   (value   "The set of proper literals, i.e. all literals of CLAUSE"
		    "leaving out the constraints"))
  (:method ((clause cl+ordered-clause))
	   (append (cl~neg-literals clause) (cl~pos-literals clause)))
  (:method ((clause cl+clause))
	   (remove-if #'lit~constraint-p (cl~literals clause))))

(defun cl~length (clause)
  (declare (edited  "29-NOV-1991 12:47")
	   (authors RICHTS CHRIS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The number of the literals in CLAUSE."))
  (length (cl~literals clause)))

(defun cl~proper-length (clause)
  (declare (edited  "17-FEB-1998")
	   (authors CHRIS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The number of the proper literals (no constraints) in CLAUSE."))
  (length (cl~proper-literals clause)))


(defgeneric cl~insert-literal! (literal clause)
  (declare (edited  "28-NOV-1991 12:54")
	   (authors RICHTS CHRIS ohlbach)
	   (input   "A literal and a clause.")
	   (effect  "LITERAL is added to the literals of CLAUSE and the clause-position"
		    "informations of all literals is updated.")
	   (value   "CLAUSE."
		    "REMARK: It is tested wether LITERAL is already in CLAUSE (EQ-TEST)"
		    "        If this is the case the LITERAL isn't inserted again."))
  (:method ((literal lit+literal) (clause cl+ordered-clause))
	   (if (find literal (cl~literals clause) :test #'eq)
	       clause
	     (progn
	       (cond ((lit~constraint-p literal)
		      (setf (cl~constraints clause)
			    (cons literal (cl~constraints clause))))
		     ((lit~positive-p literal)
		      (setf (cl~pos-literals clause)
			    (cons literal (cl~pos-literals clause))))
		     (t
		      (setf (cl~neg-literals clause)
			    (cons literal (cl~neg-literals clause)))))
	       (setf (cl=literals clause) (append (cl~neg-literals clause) (cl~pos-literals clause) (cl~constraints clause)))
	       (let* ((pos (pos~add-front -1)))
		 (mapc #'(lambda (literal)
			   (lit~remove-clause-position! literal clause)
			   (lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
		       (append (cl~neg-literals clause) (cl~pos-literals clause) (cl~constraints clause))))
	       clause)))
  (:method ((literal lit+literal) (clause cl+clause))
	   (if (find literal (cl~literals clause) :test #'eq)
	       clause
	     (progn
	       (cond ((lit~constraint-p literal)
		      (setf (cl~constraints clause)
			    (cons literal (cl~constraints clause))))
		     ((lit~positive-p literal)
		      (setf (cl~pos-literals clause)
			    (cons literal (cl~pos-literals clause))))
		     (t
		      (setf (cl~neg-literals clause)
			    (cons literal (cl~neg-literals clause)))))
	       (setf (cl=literals clause) (append (cl~literals clause) (list literal)))
	       (let* ((pos (pos~add-front -1)))
		 (mapc #'(lambda (literal)
			   (lit~remove-clause-position! literal clause)
			   (lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
		       (cl~literals clause)))
	       clause))))


(defgeneric cl~beta-reduce! (obj)
  (declare (edited  "02-AUG-1996")
	   (authors Chris)
	   (input   "a clause")
	   (effect  "destructively changes the literals")
	   (value   "the reduced clause"))
  (:method ((obj cl+clause))
	   (mapcar #'cl~beta-reduce! (cl~literals obj))
	   obj)
  (:method ((obj lit+literal))
	   (setf (lit~atom obj) (beta~normalize (lit~atom obj)))))


(defgeneric cl~remove-literal! (literal clause &key (test #'eq))
  (declare (edited  "28-NOV-1991 12:59")
	   (authors RICHTS CHRIS ohlbach)
	   (input   "A literal and a clause and as keyword a test function.")
	   (effect  "All literals which do satisfy the test with literal are removed"
		    "from the clause and the clause-position informations of all"
		    "literals are updated.")
	   (value   "The clause."))
  (:method ((literal lit+literal) (clause cl+ordered-clause) &key (test #'eq))
	   (let* ((old-literals (cl~literals clause))
		  (old-cons-lits (cl~constraints clause))
		  (old-pos-lits (cl~pos-literals clause))
		  (old-neg-lits (cl~neg-literals clause)))
	     ;; loescht Literale aus den Listen
	     (setf (cl~constraints clause)
		   (remove literal old-cons-lits :test test))
	     (setf (cl~pos-literals clause)
		   (remove literal old-pos-lits :test test))
	     (setf (cl~neg-literals clause)
		   (remove literal old-neg-lits :test test))
	     (setf (cl=literals clause)
		   (append (cl~neg-literals clause) (cl~pos-literals clause) (cl~constraints clause)))
	     
	     ;; loescht clause-position Information aus den rausgeworfenen Literalen
	     (mapcar #'(lambda (lit)
			 (lit~remove-clause-position! lit clause))
		     (set-difference old-literals (cl~literals clause) :test #'eq))
	     
	     ;; updated bei den verbliebenen Literalen die clause-position Information
	     (let* ((pos (pos~add-front -1)))
	       (mapc #'(lambda (literal)
			 (lit~remove-clause-position! literal clause)
			 (lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
		     (append (cl~neg-literals clause) (cl~pos-literals clause) (cl~constraints clause))))
	     clause))
  (:method ((literal lit+literal) (clause cl+clause) &key (test #'eql))
	   (let* ((old-literals (cl~literals clause))
		  (old-cons-lits (cl~constraints clause))
		  (old-pos-lits (cl~pos-literals clause))
		  (old-neg-lits (cl~neg-literals clause)))
	     ;; loescht Literale aus den Listen
	     (setf (cl=literals clause)
		   (remove literal old-literals :test test))
	     (setf (cl~constraints clause)
		   (remove literal old-cons-lits :test test))
	     (setf (cl~pos-literals clause)
		   (remove literal old-pos-lits :test test))
	     (setf (cl~neg-literals clause)
		   (remove literal old-neg-lits :test test))

	     ;; loescht clause-position Information aus den rausgeworfenen Literalen
	     (mapcar #'(lambda (lit)
			 (lit~remove-clause-position! lit clause))
		     (set-difference old-literals (cl~literals clause) :test #'eq))
	     
	     ;; updated bei den verbliebenen Literalen die clause-position Information
	     (let* ((pos (pos~add-front -1)))
	       (mapc #'(lambda (literal)
			 (lit~remove-clause-position! literal clause)
			 (lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
		     (cl~literals clause)))
	     clause)))



(defgeneric cl~tautology-p (clause &key (test-function #'data~equal))
  (declare (edited  "12-NOV-1996")
	   (authors Chris)
	   (input   "a clause and a optionally test-function")
	   (effect  "none")
	   (value   "t if CLAUSE is an tautology, nil if not"))
  (:method ((obj cl+clause) &key (test-function #'data~equal))
	   (not (cl~tautology obj :test-function test-function))))



(defgeneric cl~tautology (clause &key test-function)
  (declare (edited  "07-NOV-1996")
	   (authors Chris)
	   (input   "a clause and a optionally test-function")
	   (effect  "none")
	   (value   "a list with CLAUSE if is not a tautology nil otherwise"))
  (:method ((obj cl+clause) &key test-function)
	   (if (some
		#'(lambda (lit1)
		    (some
		     #'(lambda (lit2)
			 (if test-function
			     (funcall test-function
				      (lit~atom lit1) (lit~atom lit2))
			              ;;; better data~renaming 
			   (data~equal (lit~atom lit1) (lit~atom lit2))))
		     (cl~pos-literals obj)))
		(append (cl~neg-literals obj) (cl~constraints obj)))
	       nil
	     (when (notany #'cl~tautology (cl~literals obj))
		   (list obj))))
  (:method ((obj lit+literal) &key test-function)
	   (or (and (lit~positive-p obj) (logic~truth-constant-p (lit~atom obj))) 
	       (and (lit~negative-p obj) (logic~falsity-constant-p (lit~atom obj)))
	       (and (lit~positive-p obj) (and (logic~equality-p (lit~atom obj))
					      (data~equal (first (data~appl-arguments obj))
							  (second (data~appl-arguments obj)))))))
  (:method ((obj list) &key test-function)
	   (mapcan #'(lambda (el) (cl~tautology el :test-function test-function))
		   obj)))


#{\subsection{Term operations on clauses}#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod data~free-variables ((clause cl+clause))
  (remove-duplicates (data~free-variables (cl~literals clause))))

;; (defmethod data~bound-variables ((clause cl+clause))
;;  (data~bound-variables (cl~literals clause)))

(defmethod logic~ground-p ((clause cl+clause))
  (every #'logic~ground-p (cl~literals clause)))

(defmethod logic~all-bound-variables ((clause cl+clause))
  (data~free-variables (cl~literals clause)))

(defmethod data=linear-p ((clause cl+clause))
  (data=linear-p (cl~literals clause)))

(defmethod data~positions ((clause cl+clause) test)
  (data~positions (cl~literals clause) test))

(defmethod data~position ((clause cl+clause) test)
  (data~position (cl~literals clause) test))

(defmethod data~struct-at-position ((clause cl+clause) position)
  (data~struct-at-position (cl~literals clause) position))

(defmethod data=replace-at-position ((clause cl+clause) position struct destructive downto)
  (cond ((pos~empty-p position) clause)
	(t
	 (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
	     (progn
	       (setf (cl~literals clause)
		     (data=replace-at-position (cl~literals clause) position struct 't downto))
	       clause)
	   (cl~create (data=replace-at-position (cl~literals clause) position struct destructive downto))))))

(defmethod data=replace-structs ((clause cl+clause) old-structs new-structs destructive downto replacers-downto test) 
  (let* ((assoc-struct (data~assoc clause old-structs new-structs test)))
    (if assoc-struct
	(data~copy assoc-struct
		   :downto replacers-downto
		   :explode nil :preserve :all-classes)
      (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
	  (progn (setf (cl~literals clause)
		       (data=replace-structs (cl~literals clause) old-structs new-structs 't downto replacers-downto test))
		 clause)
	(cl~create (data=replace-structs (cl~literals clause) old-structs new-structs destructive downto replacers-downto test))))))

(defmethod data=replace-fv ((clause cl+clause) bound domain codomain destructive downto test)
  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
      (setf (cl~literals clause)
	    (data=replace-fv (cl~literals clause) bound domain codomain 't downto test))
    (cl~create (data=replace-fv (cl~literals clause) bound domain codomain destructive downto test))))

(defmethod data=replace-at-position ((clause cl+ordered-clause) position struct destructive downto)
  (cond ((pos~empty-p position) clause)
	(t
	 (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
	     (setf (cl~literals clause)
		   (data=replace-at-position (cl~literals clause) position struct 't downto))
	   (cl~create-ordered (data=replace-at-position (cl~literals clause) position struct destructive downto))))))

(defmethod data=replace-structs ((clause cl+ordered-clause) old-structs new-structs destructive downto replacers-downto test) 
  (let* ((assoc-struct (data~assoc clause old-structs new-structs test)))
    (if assoc-struct
	(data~copy assoc-struct
		   :downto replacers-downto
		   :explode nil :preserve :all-classes)
      (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
	  (setf (cl~literals clause)
		(data=replace-structs (cl~literals clause) old-structs new-structs 't downto replacers-downto test))
	(cl~create-ordered (data=replace-structs (cl~literals clause) old-structs new-structs destructive downto
						 replacers-downto test))))))

(defmethod data=replace-fv ((clause cl+ordered-clause) bound domain codomain destructive downto test)
  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep clause x)) downto))
      (setf (cl~literals clause)
	    (data=replace-fv (cl~literals clause) bound domain codomain 't downto test))
    (cl~create-ordered (data=replace-fv (cl~literals clause) bound domain codomain destructive downto test))))


;  Chris: Braucht jemand das ?
; (defmethod uni=renamed-free-terms-rec ((clause1 cl+clause) (clause2 cl+clause) termlist1 termlist2)
;   (if (= (cl~length clause1) (cl~length clause2))
;       (let ((new-termlist1 (append (cl~literals clause1) termlist1))
; 	    (new-termlist2 (append (cl~literals clause2) termlist2)))
; 	(uni=renamed-free-terms-rec (car new-termlist1) (car new-termlist2) (cdr new-termlist1) (cdr new-termlist2)))
;       nil))


;  Chris: Braucht jemand das ?
; (defmethod term=unshared-p ((clause cl+clause))
;   (term=unshared-p (cl~literals clause))) 



#{\subsection{POST interface}


The \post\ syntax for literals is \\
\begin{postsyntax}
\syntax{ \nt{clause}  ::= (clause \nt{name} (\nt{var-dec}*) \nt{literal}*).}
\end{postsyntax}
The function {\vb POST~PRINT} prints this representation of an literal and {\vb CL~READ}
can read it.

Because the variables in a clause are implicitly universal quantified,
the declarations of the variables of a clause are printed in the \post\ syntax.
If a variable with a name occurring in the declaration already exists,
it is shadowed and inside the clause the new variable is used.
The variables declared in a clause are only known inside the clause; they
are removed from the environment after the clause has been created.

#}


(defmethod post~print ((clause cl+clause) stream)
  (format stream "(clause ~A (" (keim~name clause))
  (let* ((bound-variables (remove-duplicates (logic~all-bound-variables clause) :test #'keim~equal))  ;; data~bound-variables
	 (extra-variables (remove-if #'(lambda (var)
					 (member var bound-variables :test #'keim~equal))
				     (keim~get clause :cl=extra-variables)))
	 (variables (append bound-variables extra-variables))
	 (type-vars-of-variables (remove-duplicates (apply 'append (mapcar #'term~type-variables-rec variables)))))

    (mapcar #'(lambda (type-var)
		(post~print type-var stream)
		(format stream " "))
	    type-vars-of-variables)
    (post~print-declaration variables stream))
  (format stream ") ")
  (dolist (lit (cl~literals clause))
    (post~print lit stream) (princ " " stream))
  (format stream ")"))

(defmethod post~print ((clause cl+ordered-clause) stream)
  (format stream "(ordered-clause ~A (" (keim~name clause))
  (let* ((bound-variables (remove-duplicates (logic~all-bound-variables clause) :test #'keim~equal))  ;; data~bound-variables
	 (extra-variables (remove-if #'(lambda (var) (member var bound-variables :test #'keim~equal))
				     (keim~get clause :cl=extra-variables))))
    (post~print-declaration bound-variables stream)
    (post~print-declaration extra-variables stream))
  (format stream ") ")
  (dolist (lit (cl~literals clause))
    (post~print lit stream) (princ " " stream))
  (format stream ")"))



(defmethod env~post-print (key (clause cl+clause) stream)
  (declare (ignore key))
  (post~print clause stream))


;; Chris: Post~read functions eliminated, we need a new extended post-syntax for clauses
;;  with constraints ...


;; Chris: Nothing changed below
;; ameier: things below are a little bit shitti

#{\subsection{Operations for the context of a clause}

Because the variables defined by a clause are needed later
(for reading substitutions for this clause), the association
between the \post-Syntax of the variables and the \keim-objects
is saved in the clause.  If the \post\ syntax of an object, which needs
the variables of a clause, is read, the variables can be restored to
the environment.
#}

(defun cl=save-context (clause var-keys env)
  (declare (edited  "31-MAR-1993 15:41")
	   (authors RICHTS CHRIS)
	   (input   "A clause, a list of symbols (POST-keys for variables defined in the clause) and an environment.")
	   (effect  "The association between VAR-KEYS and their variables in ENV is saved in CLAUSE.")
	   (value   "Undefined."))
  (let ((variables (mapcar #'(lambda (key)
			       (env~lookup-object key env))
			   var-keys)))
    (keim~put clause 'cl=context (cons var-keys variables))))

(defun cl=restore-context (clause env)
  (declare (edited  "31-MAR-1993 15:45")
	   (authors RICHTS CHRIS)
	   (input   "A clause and an environment. The clause contains the association"
		    "between its variables and their POST-syntax.")
	   (effect  "The association between POST-syntax (some keys) and the variables is restored into ENV.")
	   (value   "Undefined."))
  (let* ((context (keim~get clause 'cl=context))
	 (var-keys (car context))
	 (variables (cdr context)))
    (mapc #'(lambda (key variable)
	      (env~enter key variable env))
	  var-keys variables))
  env)

(defun cl=remove-context (clause env)
  (declare (edited  "31-MAR-1993 15:45")
	   (authors RICHTS CHRIS)
	   (input   "A clause and an environment. The clause contains the association"
		    "between its variables and their POST-syntax.")
	   (effect  "The association between POST-syntax (some keys) and the variables is removed from ENV.")
	   (value   "Undefined."))
  (let* ((context (keim~get clause 'cl=context))
	 (var-keys (car context)))
    (env~remove-plural var-keys env)))

(defmacro cl~with-context (clause env &body body)
  (declare (edited  "18-AUG-1993 15:39")
	   (authors RICHTS CHRIS)
	   (input   "A clause, an environment and a body.")
	   (effect  "The context of CLAUSE, i.e. the declaration of the variables in it, are added to ENV."
		    "The variables are removed after the body has been executed.")
	   (value   "The value of BODY executed during ENV is changed."))
  `(prog1 (progn (cl=restore-context ,clause ,env)
		 ,@body)
     (cl=remove-context ,clause ,env)))

(defmacro cl~with-context-list (clauses env &body body)
  (declare (edited  "26-JUL-1995")
	   (authors Acsehn)
	   (input "A list of clauses CLAUSES, an environment ENV and a BODY.")
	   (effect "The context of all CLAUSES, i.e. all relations between variable symbols and variables"
		   "are added to ENV. All variables of CONTEXT are removed from ENV when BODY was executed.")
	   (value "The value of BODY executed with the augmented variable declarations."))
  `(prog1 (progn (mapc #'(lambda (clause) (cl=restore-context clause ,env))
		       ,clauses)
		 ,@body)
     (mapc #'(lambda (clause) (cl=remove-context clause ,env))
	   ,clauses)))


(defun cl~read (clause env)
  (declare (edited  "11-JUN-1993 12:46")
	   (authors nesmith)
	   (input   "A clause CLAUSE in \\post\\ format and an environment.")
	   (effect  "The clause is constructed.")
	   (value   "The new clause is returned."))
  (post~read-object (if (listp clause) 
			(cdr clause) 
		      clause) 
		    env
		    (if (listp clause) 
			(intern (string (car clause)) (find-package "KEYWORD"))
		      :clause)))

(defun cl~ordered-read (clause env)
  (declare (edited  "11-JUN-1993 12:46")
	   (authors nesmith)
	   (input   "A clause CLAUSE in \\post\\ format and an environment.")
	   (effect  "The clause is constructed as ordered-clause.")
	   (value   "The new ordered-clause is returned."))
  (post~read-object (if (listp clause) 
			(cdr clause) 
		      clause) 
		    env
		    (if (listp clause) 
			(intern (string (car clause)) (find-package "KEYWORD"))
		      :ordered-clause)))



(defmethod post~read-object ((clause symbol) (env env+environment) 
			     (indicator (eql :clause)))
  (cl~env-lookup clause env))

(defmethod post~read-object ((clause string) (env env+environment) 
			     (indicator (eql :clause)))
  (cl~env-lookup clause env))

;;(defmethod post~read-object ((clause list) (env env+environment) 
;;			     (indicator (eql :clause)))
;;  (let* ((name (first clause))
;;	 (var-decs (second clause))
;;	 (literals (if (car (cddr clause))
;;		       (cddr clause)
;;		       nil)))
;;    ;; leave the variables in the environment 
;;    (dolist (var var-decs)
;;      (post~read-object var env :variable-multiple))
;;    (let* ((processed-lits (mapcar #'(lambda (lit) (lit~read lit env))
;;				   literals))
;;	   (newclause (cl~create processed-lits
;;				 :name name))
;;	   (var-keys (mapcar #'car var-decs)))
;;     (env~enter name newclause env)
;;      (cl=save-context newclause var-keys env)
;;      (cl=remove-context newclause env)
;;      newclause)))

(defmethod post~read-object ((clause list) (env env+environment) 
			     (indicator (eql :clause)))
  (let* ((name (first clause))
	 (var-decs (second clause))
	 (literals (if (car (cddr clause))
		       (cddr clause)
		     nil)))

    ;; enter the variables in the environment 
    (mapcar #'(lambda (var-dec)
		(if (listp var-dec)
		    ;; -> variable declaration
		    (post~read-object var-dec env :variable-multiple)
		  ;; -> type-variable declaration
		  (post~read-object var-dec env :type-variable-multiple)))
	    var-decs)
    
    (let* ((processed-lits (mapcar #'(lambda (lit) (lit~read lit env))
				   literals))
	   (newclause (cl~create processed-lits
				 :name name))
	   (var-keys (mapcar #'(lambda (var-dec)
				 (if (listp var-dec)
				     ;; -> variable declaration
				     (car var-dec)
				   ;; -> type-variable declaration
				   var-dec))
			     var-decs)))
      (env~enter name newclause env)
      (cl=save-context newclause var-keys env)
      (cl=remove-context newclause env)
      newclause)))


(defmethod post~read-object ((clause symbol) (env env+environment) 
			     (indicator (eql :ordered-clause)))
  (cl~ordered-env-lookup clause env))

(defmethod post~read-object ((clause string) (env env+environment) 
			     (indicator (eql :ordered-clause)))
  (cl~ordered-env-lookup clause env))

(defmethod post~read-object ((clause list) (env env+environment) 
			     (indicator (eql :ordered-clause)))
  (let* ((name (first clause))
	 (var-decs (second clause))
	 (literals (if (car (cddr clause))
		       (cddr clause)
		       nil)))
    ;; leave the variables in the environment 
    (dolist (var var-decs)
      (post~read-object var env :variable-multiple))
    (let* ((processed-lits (mapcar #'(lambda (lit) (lit~read lit env))
				   literals))
	   (standart-literals (remove-if #'lit~constraint-p processed-lits))
	   (constraint-literals (remove-if-not #'lit~constraint-p processed-lits))
	   (newclause (cl~create-ordered standart-literals
					 :name name
					 :constraints constraint-literals))
	   (var-keys (mapcar #'car var-decs)))
      (env~enter name newclause env)
      (cl=save-context newclause var-keys env)
      (cl=remove-context newclause env)
      newclause)))





(defun cl~env-lookup (key env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A KEY and an environment ENV.")
	   (effect  "None.")
	   (value   "KEY is looked up in ENV.  If it is associated with a ordered-clause,"
		    "the clause will be returned, otherwise post~error is signaled."))
  (let ((obj (env~lookup-object key env)))
    (unless (cl~clause-p obj)
      (post~error "~A is a ~A in environment, not a clause" 
		  key (class-of obj)))
    obj))

(defun cl~ordered-env-lookup (key env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A KEY and an environment ENV.")
	   (effect  "None.")
	   (value   "KEY is looked up in ENV.  If it is associated with a clause,"
		    "the clause will be returned, otherwise post~error is signaled."))
  (let ((obj (env~lookup-object key env)))
    (unless (cl~ordered-clause-p obj)
      (post~error "~A is a ~A in environment, not a clause" 
		  key (class-of obj)))
    obj))




;;; braucht LEO

(defgeneric sksym=check (object varlist)
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "An object and a list (possibly empty) of variables")
	   (effect  "None.")
	   (value   "True if all conditions are ok, else nil."))
  (:method ((object sksym+sk-constant) varlist)
	   (declare (ignore varlist))
	     ;arity muss 0 sein, da es sich um eine SkolenKonstante handelt, die nicht im Rahmen
	     ;einer Applikation auftaucht. Es sind keine Variablenbedingungen zu beachten
	     ;Kein weiterer Abstieg
	   (if (= (sksym~arity object) 0)
	       t
	     nil))
  (:method ((object term+appl) varlist)
	     ;1. functionsymbol vom Typ sksym~sk-const: #argumente >= arity und Var-test
	     ;                                          anschliessend Argumente pruefen
	     ;2. functionssymbol ist nicht sksym... : rekursiver abstieg in fktsym und args
	   (if (typep (data~appl-function object) 'sksym+sk-constant)
	       (and (>= (length (data~appl-arguments object))
			(sksym~arity (data~appl-function object)))
		    (sksym=var-test (subseq (data~appl-arguments object)
					    0 (sksym~arity (data~appl-function object)))
				    varlist)
		    (sksym=check (data~appl-arguments object) varlist))
	       (and (sksym=check (data~appl-function object) varlist)
		    (sksym=check (data~appl-arguments object) varlist))))
  (:method ((object term+abstr) varlist)
	     ;1. varlist := app(varlist,domain)
	     ;   rekursiver abstieg in die domain
	     (sksym=check (data~abstr-range object)
			  (union (data~abstr-domain object) varlist)))
  (:method ((object term+primitive) varlist)
	   (declare (ignore varlist))
	   t)  ;immer true
  (:method ((object list) varlist)
	   (if (every #'(lambda (x) (sksym=check x varlist)) object)
	       t
	     nil))
	     ; mapping auf jedes Element mit Verundung
  (:method ((object cl+clause) varlist)
	    ; teste alle positiven und negativen literale
	    ; dies sind listen in den slots pos-lit und neg-lit
	   (if (and (every #'(lambda (x) (sksym=check x varlist)) (cl~pos-literals object))
		    (every #'(lambda (x) (sksym=check x varlist)) (cl~neg-literals object)))
	       object
	       nil))
;  (:method ((object leo+clause) varlist)
;            ; teste alle positiven und negativen literale
;            ; dies sind listen in den slots pos-lit, neg-lit und uni-literals
;           (if (and (every #'(lambda (x) (sksym=check x varlist)) (cl~pos-literals object))
;                    (every #'(lambda (x) (sksym=check x varlist)) (cl~neg-literals object))
;                    (every #'(lambda (x) (sksym=check x varlist)) (leo~cl-neguni-literals object))
;                    (every #'(lambda (x) (sksym=check x varlist)) (leo~cl-uni-literals object)))
;               object
;               nil))
  (:method ((object lit+literal) varlist)
	   (sksym=check (lit~atom object) varlist)))
	      



	      

	      
