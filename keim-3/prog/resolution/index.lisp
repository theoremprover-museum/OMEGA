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




(mod~defmod STIND 
            :uses (beta data keim lit pos subst term type)
            :documentation "Higher Order Termindexing."
            :exports (
                      stind+basic-subst
                      stind+binding-subst
                      stind+node
                      stind+term-subst
                      
                      stind~clear-index
                      stind~convert-term
                      stind~create-index
                      stind~insert-term
                      stind~insert-terms
                      stind~print-subst
                      stind~print-tree
                      stind~remove-term
                      stind~retrieve-subterms-of-index
                      stind~retrieve-subterms-of-term
                      stind~retrieve-term
                      
                      stind*free-vars
                      stind*substitution))






;;; stind+basic-subst is the basic class of abstract higher order substitutions.
;;; All other classes inherit from this class. It consists of slots for the head-symbol
;;; and the binder of a substitution and for the head-symbol's type.

(defclass stind+basic-subst ()
  ((head-symbol :initform nil :initarg :head-symbol :accessor head-symbol)
   (type :initform nil :initarg :type :accessor the-type)
   (binder :initform 0 :initarg :binder :accessor binder))
  (:documentation "The basic class of abstract higher order substitutions."))






;;; stind+term-subst is the class of abstract substitutions which comprise a query term.
;;; The class inherits from stind+basic-subst ans includes an additional slot for the
;;; number arguments.

(defclass stind+term-subst (stind+basic-subst)
  ((arguments :initform nil :initarg :arguments :accessor arguments))
  (:documentation "The class of abstract higher order substitutions for constituing a query term."))






;;; stind+binding-subst is the class of abstract higher order substitutions which comprise
;;; a binding. I.e. a binding is a list of instances of this class. The class inherits from
;;; stind+basic-subst and includes an additional slot for the termposition of a head-symbol.

(defclass stind+binding-subst (stind+basic-subst)
  ((pos :initform nil :initarg :pos :accessor pos))
  (:documentation "The class of abstract higher order substitutions for constituing a binding."))







;;; stind+node is the class of the nodes of a substitution tree, i.e. an index tree. An instance
;;; of the class is a node in the tree and represents a single substitution. The class inherits
;;; from stind+basic-subst and includes an additional slot for the number of descendants of a node.

(defclass stind+node (stind+binding-subst)
  ((maximum :initform nil :initarg :maximum :accessor maximum))
  (:documentation "The class of the nodes in a substitution tree."))







;;; stind~create-index creates an empty substitution tree. After creation the tree solely consists
;;; of the root. The root node contains no substitution and is used only for maintaining the tree.
;;; The tree is implemented by a hashtable. The treepositions of the nodes are used as keys for
;;; the table. 
;;; The treepositions of a tree represent a numbering of the nodes and are defined as follows:
;;; - The root has the treeposition ().
;;; - If the treeposition of a node is (a_1,..,a_n) and the node has m descendants, then the
;;;   treepositions of the sons are (1, a_1,..,a_n),..,(m, a_1,..,a_n). 

(defun stind~create-index ()
  (declare (edited  "06-MAR-1996")
	   (authors Lklein)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The a created substitution tree."))
  (let ((hashtable (make-hash-table :test #'equal)))
    (setf (gethash '() hashtable) (make-instance 'stind+node))
    hashtable))






;;; stind~clear-index resets a substitution tree, i.e. all entries are removed except the root node.

(defun stind~clear-index (index)
  (declare (edited  "11-MAR-1996")
	   (authors Lklein)
	   (input   "A substitution tree.")
	   (effect  "Resets the substitution tree.")
	   (value   "The changed tree."))
  (clrhash index)
  (setf (gethash '() index) (make-instance 'stind+node))
  index)






;;; stind~create-new-nodes is a generic function for inserting a term into a substitution tree.
;;; The TERM is stored in a new branch of the tree starting with node TREEPOS.
;;; The first substitution has the termposition TERMPOS. BINDER is a number indicating the number
;;; of variables bound by an abstraction and is used for recursive calls. FREE-VAR-COUNTER counts
;;; the free variables of TERM and is used to rename these variables. VARLIST is an alist and
;;; is composed of the existing renamings of the variables of TERM. INDEX is a subtitution tree.
;;; The functions returns four values: a treeposition, FREE-VAR-COUNTER and VARLIST (maybe modified),
;;; and nil. The values are used for recursive calls and for cooperation with the functions
;;; stind=copy-arguments and stind=compare-arguments. 

(defgeneric stind=create-new-nodes (term treepos binder termpos free-var-counter varlist index)
  (declare (edited  "11-MAR-1996")
	   (authors Lklein)
	   (input   "A term, a node, the name of variables bound by an abstraction, a termposition, a number counting the free variables, an alist for renaming variables and a substitution tree.")
	   (effect  "Inserts a new node or a new branch into the substitution tree.")
	   (value   "A treeposition, a modified counter and renaming list and nil."))
  (:method ((term data+constant) treepos binder termpos free-var-counter varlist index)
	   (setf (gethash treepos index)
		 (make-instance 'stind+node
				:head-symbol term
				:type (data~annotation term)
				:pos termpos
				:binder binder))
	   (when (listp (stind=max (cdr treepos) index))
	      (setf (maximum (gethash (cdr treepos) index)) 1))
	   (values (cons 1 treepos) free-var-counter varlist nil))
  (:method ((term data+variable) treepos binder termpos free-var-counter varlist index)
	   (let ((name (cdr (assoc term varlist))))
	     (setf (gethash treepos index)
		   (make-instance 'stind+node
				  :head-symbol (if (null name)
						   free-var-counter
						 name)
				  :type (data~annotation term)
				  :pos termpos
				  :binder binder))
	     (when (listp (stind=max (cdr treepos) index))
	       (setf (maximum (gethash (cdr treepos) index)) 1))
	     (if (null name)
		 (values (cons 1 treepos) (1+ free-var-counter) (acons term free-var-counter varlist))
	       (values (cons 1 treepos) free-var-counter varlist nil))))
  (:method ((term data+abstr) treepos binder termpos free-var-counter varlist index)
	   (declare (ignore binder))
	   (let ((domain (data~abstr-domain term)))
	     (multiple-value-bind (new-treepos new-free-var-counter new-varlist)
		 (stind=create-new-nodes (data~abstr-range term) treepos (length domain) termpos
					 free-var-counter (stind=process-domain domain 1 termpos varlist)
					 index)
	       (values new-treepos new-free-var-counter new-varlist nil))))
  (:method ((term data+appl) treepos binder termpos free-var-counter varlist index)
	   (let* ((appl-head (data~appl-function term))
		  (name (if (data~variable-p appl-head)
			    (cdr (assoc appl-head varlist))
			  appl-head)))
	     (setf (gethash treepos index)
		   (make-instance 'stind+node
				  :head-symbol (if (null name)
						   free-var-counter
						 name)
				  :type (data~annotation appl-head)
				  :pos termpos
				  :binder binder))
	     (when (listp (stind=max (cdr treepos) index))
	       (setf (maximum (gethash (cdr treepos) index)) 1))
	     (if (null name)
		 (stind=copy-arguments (data~appl-arguments term) (cons 1 treepos) termpos 1
				       (1+ free-var-counter) (acons appl-head free-var-counter varlist)
				       index)
	       (stind=copy-arguments (data~appl-arguments term) (cons 1 treepos) termpos 1
				     free-var-counter varlist index)))))








;;; stind=copy-arguments inserts the arguments of an application into a substitution tree.
;;; TERMS is a list of terms, the node TREEPOS is the first node created. TERMPOS is the
;;; temposition of the function symbol of the application. The first element of TERMS is
;;; the NUMBERth argument to the function. FREE-VAR-COUNTER counts the free variables of
;;; the whole TERM and is used to rename these variables. VARLIST is an alist and
;;; is composed of the existing renamings of the variables of TERM. INDEX is a subtitution tree.
;;; The functions returns four values: a treeposition, FREE-VAR-COUNTER and VARLIST (maybe modified),
;;; and nil.

(defun stind=copy-arguments (terms treepos termpos number free-var-counter varlist hashtable)
  (declare (edited  "11-MAR-1996")
	   (authors Lklein)
	   (input   "A list of terms, a node, a termposition, the number of the first argument, a number counting the free-variables, an alist for renaming variables and a substitution tree.")
	   (effect  "Inserts the terms into the tree.")
	   (value   "A treeposition, amodified counter and renaming list and nil."))
  (if (null terms)
      (values treepos free-var-counter varlist nil)
    (multiple-value-bind (treepos new-free-var-counter new-varlist)
	(stind=create-new-nodes (car terms) treepos 0 (cons number termpos)
				free-var-counter varlist hashtable)
      (stind=copy-arguments (cdr terms) treepos termpos (1+ number)
			    new-free-var-counter new-varlist hashtable))))









;;; stind~insert-term inserts the object OBJECT into the substitution tree INDEX.
;;; If the tree is empty, a new branch is created for the term of OBJECT. Otherwise
;;; the term is inserted at the suitable position. 

(defgeneric stind~insert-term (object index)
  (declare (edited  "11-MAR-1996")
	   (authors Lklein)
	   (input   "An object, i.e. a literal or a term, and a substitution tree.")
	   (effect  "Inserts the object into the substitution tree.")
	   (value   "None."))
  (:method ((object term+term) index)
	   (let ((treepos (if (listp (stind=max '() index))
			      (cdr (stind=create-new-nodes (beta~eta-longform (beta~normalize object))
							   '(1) 0 '(1) 1 nil index))
			    (cdr (stind=compare-nodes (beta~eta-longform (beta~normalize object))
							'(1) 0 '(1) 1 nil nil index)))))
	     (setf (maximum (gethash treepos index))
		   (cons object (stind=max treepos index)))))
  (:method ((object lit+literal) index)
	   (let* ((term (lit~atom object))
		  (treepos (if (listp (stind=max '() index))
			       (cdr (stind=create-new-nodes (beta~eta-longform (beta~normalize term))
							    '(1) 0 '(1) 1 nil index))
			     (cdr (stind=compare-nodes (beta~eta-longform (beta~normalize term))
							 '(1) 0 '(1) 1 nil nil index)))))
	     (setf (maximum (gethash treepos index))
		   (cons object (stind=max treepos index))))))







;;; stind~insert-terms inserts all objects of the list OBJECTLIST into the
;;; substitution tree INDEX.


(defun stind~insert-terms (objectlist index)
  (declare (edited  "20-FEB-1996")
	   (authors Lklein)
	   (input   "A list of objects, i.e. literals or terms, and a substitution tree.")
	   (effect  "All objects of the list are inserted into the substitution tree.")
	   (value   "None."))
  (mapcar #'(lambda (object)
	      (stind~insert-term object index))
	  objectlist))



	   




;;; stind=head performs an access to the head-symbol slot of the node POSITION
;;; of the substitution tree INDEX.

(defun stind=head (position index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns the contents of the head-symbol slot of the referred node."))
  (head-symbol (gethash position index)))







;;; stind=pos performs an access to the pos slot of the node POSITION
;;; of the substitution tree INDEX.

(defun stind=pos (position index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns the contents of the pos slot of the referred node."))
  (pos (gethash position index)))







;;; stind=max performs an access to the max slot of the node POSITION
;;; of the substitution tree INDEX.

(defun stind=max (position index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns the contents of the pos slot of the referred node."))
  (maximum (gethash position index)))







;;; stind=binder performs an access to the binder slot of the node POSITION
;;; of the substitution tree INDEX.

(defun stind=binder (position index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns the contents of the binder slot of the referred node."))
  (binder (gethash position index)))







;;; stind=type performs an access to the type slot of the node POSITION
;;; of the substitution tree INDEX

(defun stind=type (position index)
  (declare (edited  "17-OCT-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns the contents of the type slot of the referred node."))
  (the-type (gethash position index)))









;;; stind=compare-nodes is a generic function which searchs the substitution tree INDEX for
;;; a suitable branch to insert the term TERM. The function compares the nodes of the tree with TERM.
;;; TREEPOS is the first node compared. TERMPOS is the substitution variable for the term.
;;; BINDER is a number indicating the number of variables bound by an abstraction. FREE-VAR-COUNTER
;;; counts the free variables of TERM and is used to rename these variables. VARLIST is an alist and
;;; is composed of the existing renamings of the variables of TERM. INDEX is a subtitution tree.
;;; RESULT is true iff the comparison has succeeded up to now. 
;;; The functions returns four values: a treeposition, FREE-VAR-COUNTER and VARLIST (maybe modified),
;;; and the last value is the result of the comparison.
;;; If a comparison fails, the function stind=insertion-backtracking is called to compare the next
;;; brother of the current node with TERM. For comparing the arguments of an application the function
;;; stind=compare-arguments is called. 

(defgeneric stind=compare-nodes (term treepos binder termpos free-var-counter varlist result index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input  "A term, a node, the number of variables bound by an abstraction, a termposition, a number for counting the free variables, an alist for renaming variables, a flag indicating the result of former comparisons and a substitution tree.")
	   (effect  "The tree is changed iff it does not contain a term which is alpha-equal to the given term.")
	   (value   "The function compares the term with the nodes of the substitution tree and returns a treeposition, a modified counter and renaming list and the result of the comparison."))
  (:method ((term data+abstr) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore binder))
	   (let ((domain (data~abstr-domain term)))
	     (multiple-value-bind (treepos free-var-counter varlist result)
		 (stind=compare-nodes (data~abstr-range term) treepos (length domain) termpos free-var-counter
					(stind=process-domain domain 1 termpos varlist) result index)
	       (values treepos free-var-counter varlist result))))
  (:method ((term data+constant) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (if (and (keim~equal term (stind=head treepos index))
		    (= binder (stind=binder treepos index)))
	       (values (cons 1 treepos) free-var-counter varlist 't)
	     (stind=insertion-backtracking (car treepos) (cdr treepos) term binder
					   termpos free-var-counter varlist index)))
  (:method ((term data+variable) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (let* ((name (cdr (assoc term varlist))))
	     (if (and (equal (if (null name) free-var-counter name)
			     (stind=head treepos index))
		      (= binder (stind=binder treepos index))
		      (keim~equal (data~annotation term) (stind=type treepos index)))
		 (if (null name)
		     (values (cons 1 treepos) (1+ free-var-counter)
			     (acons term free-var-counter varlist) 't)
		   (values (cons 1 treepos) free-var-counter varlist 't))
	       (stind=insertion-backtracking (car treepos) (cdr treepos) term binder
					     termpos free-var-counter varlist index))))
  (:method ((term data+appl) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (let* ((appl-head (data~appl-function term))
		  (name (if (data~variable-p appl-head)
			    (cdr (assoc appl-head varlist))
			  appl-head)))
	     (if (and (keim~equal (if (null name) free-var-counter name)
				  (stind=head treepos index))
		      (= binder (stind=binder treepos index))
		      (keim~equal (data~annotation appl-head) (stind=type treepos index)))
		 (if (null name)
		     (stind=compare-arguments (data~appl-arguments term) (cons 1 treepos)
					      termpos 1 (1+ free-var-counter)
					      (acons appl-head free-var-counter varlist) 't
					      index)
		   (stind=compare-arguments (data~appl-arguments term) (cons 1 treepos)
					    termpos 1 free-var-counter varlist 't index))
	       (stind=insertion-backtracking (car treepos) (cdr treepos) term binder termpos
					     free-var-counter varlist index)))))







;;; stind=compare-arguments compares the arguments of an application with the nodes of a
;;; substitution tree. The returned values are built exactly like those of stind=compare-nodes.
;;; TERMS is a list of terms representing the arguments. TREEPOS is the first node compared.
;;; TERMPOS is the substitution variable of the application and NUMBER specifies the argument.
;;; FREE-VAR-COUNTER counts the free variables in TERMS and is used to rename these variables.
;;; VARLIST is an alist and is composed of the existing renamings of the variables of TERM. 
;;; RESULT is true iff the comparison has succeeded up to now. INDEX is a subtitution tree.
;;; If a comparison fails, the remaining substitutions are put into the substitution
;;; tree by calling stind=copy-arguments.


(defun stind=compare-arguments (terms treepos termpos number free-var-counter varlist result index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A list of terms, a node, a termposition, the number of the first argument in the term-list, the number of renamed free variables, an alist for renaming variables, a flag for the result of a previous call of the function and a substitution tree.")
	   (effect  "Compares the arguments of an application and inserts remaining arguments if the comparison fails.")
	   (value   "The function compares the term with the nodes of the substitution tree and returns a treeposition, a modified counter and renaming list and the result of the comparison."))
  (if (null terms)
      (values treepos free-var-counter varlist result)
    (multiple-value-bind (treepos free-var-counter varlist result)
	(stind=compare-nodes (car terms) treepos 0 (cons number termpos)
			       free-var-counter varlist 't index)
      (if result
	  (stind=compare-arguments (cdr terms) treepos termpos (1+ number)
				   free-var-counter varlist 't index)
	(stind=copy-arguments (cdr terms) treepos termpos (1+ number)
			      free-var-counter varlist index)))))







;;; stind=insertion-backtracking performs backtracking from the current node to the ancestor of the node
;;; and then selects the next son for further comparison. If the son doesn't exist, a new branch
;;; is created and the term is inserted into the substitution tree.
;;; NUMBER is the number of the son at which backtracking has been set off, TREEPOS is the node
;;; to which backtracking is performed. TERM is the term which has to be compared or inserted.
;;; BINDER is a number indicating the number of variables bound by an abstraction. FREE-VAR-COUNTER
;;; counts the free variables of TERM and is used to rename these variables. VARLIST is an alist and
;;; is composed of the existing renamings of the variables of TERM. INDEX is a subtitution tree.
;;; TERMPOS represents the substitution variable of the term.
;;; The result of this function is composed by the called function stind=compare-nodes or
;;; stind=create-nodes.

(defun stind=insertion-backtracking (number treepos term binder termpos free-var-counter varlist index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input  "The number of a son, the ancestor of this son, a term, the number of variables bound by an abstraction, a termposition, a number for counting the free variables, an alist for renaming variables and a substitution tree.")
	   (effect  "The tree is changed iff it does not contain a term which is alpha-equal to the given term.")
	   (value   "Returns a treeposition, a modified counter and renaming list and the result of the comparison."))
  (if (< number (stind=max treepos index))
      (stind=compare-nodes term (cons (1+ number) treepos) binder
			     termpos free-var-counter varlist 't index)
    (let ((new-max (1+ (stind=max treepos index))))
      (setf (maximum (gethash treepos index)) new-max)
      (stind=create-new-nodes term (cons new-max treepos) binder
			      termpos free-var-counter varlist index))))






;;; stind*substitution is a hashtable which contains the sequence of simple substitutions generated
;;; by transforming a term. The keys of the hashtable are termposition. Each termposition represents
;;; a simple abstract substitution of the class stind+term-subst.
;;; The hashtable comprises all substitutions constituing a queryterm.

(defvar stind*substitution (make-hash-table :test #'equal))









;;; stind=create-substitution is a generic function for creating abstract higher order substitutions.
;;; A term TERM is transformed in a sequence of abstract substitutions and this sequence is stored in 
;;; stind*substitution. TERMPOS is the termpositionof the first symbol in TERM.
;;; BINDER is a number indicating the number of variables bound by an abstraction. FREE-VAR-COUNTER
;;; counts the free variables of TERM and is used to rename these variables. VARLIST is an alist and
;;; is composed of the existing renamings of the variables of TERM. FREE-VAR-LIST contains for each
;;; renaming of a free variable the original name of the variable.
;;; The function returns three values: FREE-VAR-LIST, FREE-VAR-COUNTER and VARLIST (all may be modified).

(defgeneric stind=create-substitution (term termpos binder free-var-counter varlist free-var-list)
  (declare (edited  "06-FEB-1996")
	   (authors Lklein)
	   (input   "A term, a termposition, a number indicating the variables bound at termposition, a number for counting the free variables, an alist for renaming variables and an alist for re-renaming the free variables.")
	   (effect  "Inserts the term recursively into stind*substitution.")
	   (value   "The re-renaming list, the counter and the renaming list (all may be modified."))
  (:method ((term data+constant) termpos binder free-var-counter varlist free-var-list)
	   (setf (gethash termpos stind*substitution)
		 (make-instance 'stind+term-subst
				:head-symbol term
				:type (data~annotation term)
				:arguments nil
				:binder binder))
	   (values free-var-list free-var-counter varlist))
  (:method ((term data+variable) termpos binder free-var-counter varlist free-var-list)
	   (let ((name (cdr (assoc term varlist))))
	     (setf (gethash termpos stind*substitution)
		   (make-instance 'stind+term-subst
				  :head-symbol (if (null name)
						   free-var-counter
						 name)
				  :type (data~annotation term)
				  :arguments nil
				  :binder binder))
	     (if (null name)
		 (values (acons free-var-counter term free-var-list)
			 (1- free-var-counter)
			 (acons term free-var-counter varlist))
	       (values free-var-list
		       free-var-counter
		       varlist))))
  (:method ((term data+appl) termpos binder free-var-counter varlist free-var-list)
	   (let* ((appl-head (data~appl-function term))
		  (name (if (data~variable-p appl-head)
			    (cdr (assoc appl-head varlist))
			  appl-head)))
	     (setf (gethash termpos stind*substitution)
		   (make-instance 'stind+term-subst
				  :head-symbol (if (null name)
						   free-var-counter
						 name)
				  :type (data~annotation appl-head)
				  :arguments (length (data~appl-arguments term))
				  :binder binder))
	     (if (null name)
		 (stind=convert-arguments (data~appl-arguments term) termpos 1 (1- free-var-counter)
					  (acons appl-head free-var-counter varlist)
					  (acons free-var-counter appl-head free-var-list))
	       (stind=convert-arguments (data~appl-arguments term) termpos 1 free-var-counter
					varlist free-var-list))))
  (:method ((term data+abstr) termpos binder free-var-counter varlist free-var-list)
	   (declare (ignore binder))
	   (let ((domain (data~abstr-domain term)))
	     (multiple-value-bind (new-free-var-list new-free-var-counter new-varlist)
		 (stind=create-substitution (data~abstr-range term) termpos (length domain) free-var-counter
					    (stind=process-domain domain 1 termpos varlist)
					    free-var-list)
	       (values new-free-var-list new-free-var-counter new-varlist)))))








;;; stind=convert-argmuents transforms the arguments of an application in a sequence of
;;; simple abstract substitutions.
;;; The function is invoked by stind=create-substitution. For each term of the list TERMS,
;;; stind=create-substitution is used for the transformation. NUMBER specifies the argument.
;;; All other parameters and the result are equivalent to the parameters of stind=create-substitution.

(defun stind=convert-arguments (terms termpos number free-var-counter varlist free-var-list)
  (declare (edited  "20-JUN-1996")
	   (authors Lklein)
	   (input   "A list of terms, a termposition, the number for the first term in the list specifying the argument, a number indicating the variables bound at termposition, a number for counting the free variables, an alist for renaming variables and an alist for re-renaming the free variables.")
	   (effect  "Transforms the terms of the termlist in simple abstract substitutions.")
	   (value   "The re-renaming list, the counter and the renaming list (all may be modified."))
  (if (null terms)
      (values free-var-list free-var-counter varlist)
    (multiple-value-bind (new-free-var-list new-free-var-counter new-varlist)
	(stind=create-substitution (car terms) (cons number termpos) 0 free-var-counter
				   varlist free-var-list)
      (stind=convert-arguments (cdr terms) termpos (1+ number) new-free-var-counter
			       new-varlist new-free-var-list))))

  









;;; stind~convert-term converts a given term TERM into a sequence of simple abstract substitutions and stores 
;;; this sequence in stind*substitution. The term is returned.


(defun stind~convert-term (term)
  (declare (edited  "20-JUN-1996")
	   (authors Lklein)
	   (input   "A term.")
	   (effect  "Transforms the term into a sequence of simple substitutions and stores this sequence in stind*substitution.")
	   (value   "The term."))
  (stind=clear-substitution)
  (multiple-value-bind (free-vars foo1 foo2)
      (stind=create-substitution (beta~eta-longform (beta~normalize term)) '(1) 0 -1 nil nil)
    (declare (ignore foo1)
	     (ignore foo2))
    (setq stind*free-vars free-vars))
  term)





;;; stind*free-vars is an alist used for creating substitutions after a retrieval. Each pair consists of
;;; a renaming and the former name of the renamed variable refering to the last transformed term.

(defvar stind*free-vars)






;;; stind=clear-substitution resets the hashtable stind*substitution.

(defun stind=clear-substitution ()
  (declare (edited  "20-JUN-1996")
	   (authors Lklein)
	   (input   "None.")
	   (effect  "Resets stind*substitution.")
	   (value   "None."))
  (clrhash stind*substitution))






;;; stind~print-subst prints the sequence of abstract higher order substitutions
;;; comprising a queryterm.

(defun stind~print-subst ()
  (declare (edited  "07-OCT-1996")
	   (authors Lklein)
	   (input   "None.")
	   (effect  "Prints all substitutions belonging to a term.")
	   (value   "None."))
  (maphash #'(lambda (position value)
	       (declare (ignore value))
	       (let ((head (head-symbol (gethash position stind*substitution)))
		     (binder (binder (gethash position stind*substitution)))
		     (arguments (arguments (gethash position stind*substitution))))
		 (format t "~%Substitution: ~A~%Head: ~A~%Binder: ~A~%Arguments: ~A~%"
			 position head binder arguments)))
	   stind*substitution))
  





;;; stind~print-tree prints all nodes of the substitution tree INDEX.

(defun stind~print-tree (index)
  (declare (edited  "07-OCT-1996")
	   (authors Lklein)
	   (input   "A substitution tree.")
	   (effect  "Prints all nodes of the tree." )
	   (value   "None."))
  (maphash #'(lambda (position value)
	       (declare (ignore value))
	       (format t "~&Treeposition: ~A~%Substitution: ~A~%Head: ~A~%Binder:~A~%Number of Sons: ~A~%~%"
		       position
		       (stind=pos position index)
		       (stind=head position index)
		       (stind=binder position index)
		       (stind=max position index)))
	   index))








;;; stind~retrieve-term retrieves all terms of the substitution tree INDEX which are cosimplifiable with a given term or literal OBJECT. Depending on the keyword :MODE the function can also retrieve all instances or generalizations of OBJECT. The keyword :BINDINGS is either t or nil and specifies whether generated bindings for the free variables of OBJECT should be transformed into substitutions. The result is a list of all found terms. If :BINDINGS is t the list consists of pairs representing a term and a corresponding substitution.

(defgeneric stind~retrieve-term (object index &key mode bindings)
  (declare (edited  "26-AUG-1996")
	   (authors Lklein)
	   (input   "A Term or a literal, a substitution tree and two keyword arguments :mode and :binding. The keys for :mode are :inst to retireve all instances and :gen to retrieve all generalizations. For :bindins there are two keys t and nil specifying whether the generated bindings should be returned.")
	   (effect  "None.")
	   (value   "A list of all cosimplifiable objects if :bindings is nil. And a list of pairs consisting of a cosimplifiable object and a susbtituion if :bindings is t."))
  (:method ((object term+term) index &key mode bindings)
	   (let ((term object))
	     (stind~convert-term term)
	     (stind=retrieve-dac 1 mode index bindings)))
  (:method ((object lit+literal) index &key mode bindings)
	   (let ((term (lit~atom object)))
	     (stind~convert-term term)
	     (stind=retrieve-dac 1 mode index bindings))))









;;; stind=retrieve-dac starts the retrieval process for all descendants of the root and combines the results. For each result the unsolved problems are checked out with the generated bindings. NUMBER specifies the current descendant. INDEX is a substitution tree. MODE and LONG-OUTPUT are the values of the keyword arguments of stind~retrieve-term. The result is equivalent to stind~retrieve-term.

(defun stind=retrieve-dac (number mode index long-output)
  (declare (edited  "28-NOV-1996")
	   (authors Lklein)
	   (input   "A number specifying a descendant of the root, the retrieval mode, a substitution tree and a flag describing the output mode.")
	   (effect  "None.")
	   (value   "A list of all cosimplifiable objects if the last parameter is nil. And else a list of pairs consisting of a cosimplifiable object and a substitituion."))
  (let ((max (stind=max '() index)))
    (cond ((listp max)
	  ; (format t  "The index is empty.")
	   nil)
	  ((> number max)
	   (values nil
		   nil))
	  (t
	   (multiple-value-bind (pos-list1 stop-list1)
	       (stind=tree-term-compare-nodes (list number) nil mode index)
	     (declare (ignore pos-list1))
	     (let ((stop-list2 (stind=retrieve-dac (1+ number) mode index long-output)))
	       (nconc    
		(mapcan #'(lambda (entry)
			    (let ((term-list (copy-list (stind=max (car entry) index))))
			      (if term-list
				  (let ((entry-unsolved (second entry))
					(entry-bindings (third entry)))
				    (if (and (null entry-unsolved)
					     (null entry-bindings))
					(if long-output
					    (mapcar #'(lambda (single-term)
							(list single-term
							      (subst~create nil nil)))
						    term-list)
					  term-list)
				      (multiple-value-bind (unsolved-problems bindings)
					  (stind=test-unsolved-problems entry-unsolved entry-bindings nil)
					(if (and (null unsolved-problems) 
						 (null bindings))
					    nil
					  (if long-output
					      (mapcar #'(lambda (single-term)
							  (list single-term
								(stind=create-substitution-list
								 (stind=copy-bindings bindings)
								 stind*free-vars)))
						      term-list)
					    term-list)))))
				nil)))
			stop-list1)
		stop-list2)))))))







;;; stind=create-substitution transforms the generate bindings into substitutions for the free variables of the queryterm. BINDING-LIST is the list of all generated bindings and VAR-LIST is a list for re-renaming the free variables of the queryterm. The generated substitution is returned.

(defun stind=create-substitution-list (binding-list var-list)
  (declare (edited  "02-APR-1997")
	   (authors Lklein)
	   (input   "A list with bindings and a list for re-renaming the free variables of the queryterm.")
	   (effect  "None.")
	   (value   "Creates a substitution for the free variables and returns it."))
  (let ((variable-list (mapcan #'(lambda (binding)
				   (if (< (car binding) 0)
				       (list (cdr (assoc (car binding) (copy-list var-list))))
				     nil))
			       binding-list))
	(term-list (mapcan #'(lambda (binding)
			       (if (< (car binding) 0)
				   (list (stind=create-keim-term (cdr binding) (copy-list var-list)))
				 nil))
			     binding-list)))
    (subst~create variable-list term-list)))
		  









;;; stind=create-keim-term takes a binding BINDING and an alist RENAMING for renaming the variables and creates a keim-term.

(defun stind=create-keim-term (binding renaming)
  (declare (edited  "02-APR-1997")
	   (authors Lklein)
	   (input   "A binding and a renaming for the variables.")
	   (effect  "The binding is destructively used.")
	   (value   "A keim term equivalent to the binding in respect of the renaming, the rest of the binding and a modified renaming."))
  (let* ((first-element (car binding))
	 (head (head-symbol first-element))
	 (termpos (pos first-element))
	 (termpos-length (length termpos))
	 (element-type (the-type first-element))
	 (binder (binder first-element)))
    (cond ((> binder 0)
	   (multiple-value-bind (var-list alist)
	       (stind=abstr-var termpos binder)
	     (setf (binder (car binding)) 0)
	     (multiple-value-bind (term binding-rest new-renaming)
		 (stind=create-keim-term binding (append renaming alist))
	       (values (data~abstr-create var-list term)
		       binding-rest
		       new-renaming))))
	  ((type~complex-p element-type)
	   (if (or (numberp head)
		   (listp head))
	       (let* ((look-up (assoc head renaming :test #'equal))
		      (variable (if look-up
				    (cdr look-up)
				  (term~variable-create (gensym "dc") element-type))))
		 (multiple-value-bind (arguments binding-rest new-renaming)
		     (stind=create-keim-arguments (cdr binding) termpos-length
						  (if look-up
						      renaming
						    (acons head variable renaming)))
		   (values (data~appl-create variable arguments)
			   binding-rest
			   new-renaming)))
	     (multiple-value-bind (arguments binding-rest new-renaming)
		 (stind=create-keim-arguments (cdr binding) termpos-length renaming)
	       (values (data~appl-create head arguments)
		       binding-rest
		       new-renaming))))
	  ((or (numberp head)
	       (listp head))
	   (let* ((look-up (assoc head renaming :test #'equal))
		  (variable (if look-up
				(cdr look-up)
			      (term~variable-create (gensym "dc") element-type))))
	     (values variable
		     (cdr binding)
		     (if look-up
			 renaming
		       (acons head variable renaming)))))
	  (t
	   (values (data~constant-create 'data+constant :name head :annotation element-type)
		   (cdr binding)
		   renaming)))))

		 







;;; stind=create-keim-arguments generates the arguments of a keim term. BINDING is transformed to the keim term using the alist RENAMING specifying the re-renamings of the variables in binding. BREAK-LENGTH is the length of the termposition of the function symbol. The result is a list of keim-terms specifying the arguments, the remaining binding and a modified renaming.

(defun stind=create-keim-arguments (binding break-length renaming)
  (declare (edited  "02-APR-1997")
	   (authors Lklein)
	   (input   "A binding, a break-length and a renaming.")
	   (effect  "The binding is used destructively.")
	   (value   "A list of keim terms equivalent to the arguments in binding in respect of the renaming, the rest of the binding and a modified renaming."))
  (if binding
      (let* ((first-element (car binding))
	     (termpos (pos first-element))
	     (termpos-length (length termpos)))
	(if (<= termpos-length break-length)
	    (values nil
		    binding
		    renaming)
	  (multiple-value-bind (term binding-rest1 new-renaming1)
	      (stind=create-keim-term binding renaming)
	    (multiple-value-bind (term-list binding-rest2 new-renaming2)
		(stind=create-keim-arguments binding-rest1 break-length new-renaming1)
	      (values (if term-list
			  (cons term term-list)
			(list term))
		      binding-rest2
		      new-renaming2)))))
    (values nil
	    nil
	    renaming)))









;;; stind=abstr-var creates a variable list for generating an abstraction and an alist to transform the internal representation of these variables. TERMPOS is the termposition at which the abstraction take place and NUMBER is the number of lambda-bound variables. 

(defun stind=abstr-var (termpos number)
  (declare (edited  "02-APR-1997")
	   (authors Lklein)
	   (input   "A termposition and a number counting the lambda-bound variables at the termposition.")
	   (effect  "None.")
	   (value   "A list of new variables and an alist for renaming the internal respresentation of these variables."))
  (if (= number 0)
      (values nil
	      nil)
    (let ((variable (data~variable-create 'data+variable)))
      (multiple-value-bind (var-list alist)
	  (stind=abstr-var termpos (1- number))
	(values (nconc var-list (list variable))
		(acons (list number termpos) variable alist))))))













;;; stind=tree-make-binding and stind=tree-make-binding traverse a subtree with root TREEPOS of the substitution tree INDEX while creating bindings. VARIABLE is used for occur-check-tests. The generated bindings can be used to build unsolved problems. The functions return 4 values: the first and the second value is usable for generating unsolved problems, and each value is a list of pairs and each pair consists of a node and a binding. The third and fourth value only indicates that no occur-check has been found and therefore the values are lists of nodes. The second and fourth value contains the values for leaves and the first and third value the appropriate items for inner nodes. The function stind=tree-make-binding is used for calling the second function.

(defun stind=tree-make-binding (treepos variable index)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input  "A node, a variable for occur check-tests and a substitution tree.")
	   (effect "None." )
 	   (value  "Four values: the first and second value is a list of pairs of node and binding, and the third and fourth value is a list of nodes." ))
  (stind=tree-make-binding-dac treepos variable (length (stind=pos treepos index)) 1 nil index))

      

(defun stind=tree-make-binding-dac (treepos variable cut-length number flag index)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input   "A node, a variable for occur check-tests, a break-length, the number of the current son, a flag, and a substitution tree." )
	   (effect  "None.")
	   (value   "Four values: the first and second value is a list of pairs of node and binding, and the third and fourth value is a list of nodes."))
  (let* ((max (stind=max treepos index))
	 (current-termpos (stind=pos treepos index))
	 (symbol (stind=head treepos index))
	 (new-symbol (if (listp symbol) 
			 (list (car symbol)
			       (stind=fix-termpos (second symbol) cut-length))
		       symbol))
	 (binder (stind=binder treepos index))
	 (new-termpos (stind=fix-termpos current-termpos cut-length)))
    (cond ((and flag
		(<= (length current-termpos) cut-length))
	   (values (list (list treepos nil))
		   nil
		   nil
		   nil))
	  ((eq variable symbol)
	   (values nil
		   nil
		   nil
		   nil))
	  ((and (listp max)
		(or (not (listp symbol))
		    (>= (length (second symbol)) cut-length)))
	   (values nil
		   (list (list treepos
			       (list (make-instance 'stind+binding-subst
						    :head-symbol new-symbol
						    :pos new-termpos
						    :type (stind=type treepos index)
						    :binder binder))))
		   nil
		   nil))
	  ((listp max)
	   (values nil
		   nil
		   nil
		   (list treepos)))
	  ((> number max)
	   (values nil
		   nil
		   nil
		   nil))
	  (t
	   (multiple-value-bind (pos-list1 stop-list1 p-list1 s-list1)
	       (stind=tree-make-binding-dac (cons number treepos) variable cut-length 1 t index)
	     (multiple-value-bind (pos-list2 stop-list2 p-list2 s-list2)
		 (stind=tree-make-binding-dac treepos variable cut-length (1+ number) nil index)
	       (if (or (not (listp symbol))
		       (>= (length (second symbol)) cut-length))
		   (let ((new-pos-list1 (mapcar #'(lambda (element)
						    (list (car element)
							  (cons (make-instance 'stind+binding-subst
									       :head-symbol new-symbol
									       :type (stind=type treepos index) 
									       :pos new-termpos
									       :binder binder)
								(second element))))
					      pos-list1))
			 (new-stop-list1 (mapcar #'(lambda (element)
						     (list (car element)
							   (cons (make-instance 'stind+binding-subst
										:head-symbol new-symbol
										:type (stind=type treepos index)
										:pos new-termpos
										:binder binder)
								 (second element))))
						 stop-list1)))
		     (values (nconc new-pos-list1 pos-list2)
			     (nconc new-stop-list1 stop-list2)
			     (nconc p-list1 p-list2)
			     (nconc s-list1 s-list2)))
		 (values nil
			 nil
			 (nconc (mapcar #'(lambda (element)
					    (car element))
					pos-list1)
				p-list1 p-list2)
			 (nconc (mapcar #'(lambda (element)
					    (car element))
					stop-list1)
				s-list1 s-list2)))))))))
			 










;;; The functions stind=tree-elim and stind=tree-elim-dac create - analogously to stind=tree-make-binding - bindings which are used for the elimination rule of the simplification algorithm. Therefore an additional condition has to be tested. FIRST-BINDER is the value of the binder slot of the first instances of each generated binding. It represents the number of abstractions performed by applying the elimination rule. The ALIST comprises all renamings of variables. OFFSET and MAX are numbers used to correct the representation of lambda-bound variables. If the additional condition fails a binding for an unsolved problem is created. So the result of the two functions consists of four values, separated unto leaves and inner nodes and bindings for unsolved problems and the elimination rule.

(defun stind=tree-elim (treepos variable first-binder alist offset max index)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input   "A node, a variable for occur check-tests, a value for the binder slot of the first instance of each binding, an alist for renamings, two numbers for correcting the representation of bound variables and a substitution tree.")
	   (effect  "None.")
	   (value   "Four values: each value is a list consisting of node-binding-lists."))
  (stind=tree-elim-dac treepos variable first-binder alist offset max
		       (length (stind=pos treepos index)) 1 nil index))

      


(defun stind=tree-elim-dac (treepos variable first-binder alist offset max cut-length number flag index)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input   "A node, a variable for occur check-tests, a value for the binder slot of the first instance of each binding, an alist for renamings, two numbers for correcting the representation of bound variables, a break-length, the number of the current son, a flag and a substitution tree.")
	   (effect  "None.")
	   (value   "Four values: each value is a list consisting of node-binding-lists."))
  (let* ((nr-of-sons (stind=max treepos index))
	 (current-termpos (stind=pos treepos index))
	 (symbol (stind=head treepos index))
	 (symbol-termpos (if (listp symbol) (length (second symbol)) nil))
	 (symbol-number (if (listp symbol) (car symbol) nil))
	 (in-alist (assoc symbol alist :test #'equal))
	 (new-binder (if (null flag)
			 first-binder
		       (stind=binder treepos index)))
	 (new-symbol (if (listp symbol)
			 (if (= symbol-termpos cut-length)
			     (if in-alist
				 (list (cdr in-alist)
				       '())
			       (list (+ (car symbol)
					offset)
				     '()))
			   (list (car symbol)
				 (stind=fix-termpos (second symbol) cut-length)))
		       symbol))
	 (new-termpos (stind=fix-termpos current-termpos cut-length)))
    (cond ((and flag
		(<= (length current-termpos) cut-length))
	   (values (list (list treepos nil))
		   nil
		   nil
		   nil))
	  ((eql variable symbol)
	   (values nil
		   nil
		   nil
		   nil))
	  ((and (listp nr-of-sons)
		(listp symbol)
		(or (< symbol-termpos cut-length)
		    (and (= symbol-termpos cut-length)
			 (<= symbol-number max)
			 (not in-alist))))
	   (values nil
		   nil
		   nil
		   (list treepos)))
	  ((listp nr-of-sons)
	   (values nil
		   (list (list treepos
			       (list (make-instance 'stind+binding-subst
						    :head-symbol new-symbol
						    :pos new-termpos
						    :type (stind=type treepos index)
						    :binder new-binder))))
		   nil
		   nil))
	  ((> number nr-of-sons)
	   (values nil
		   nil
		   nil
		   nil))
	  (t
	   (multiple-value-bind (pos-list1 stop-list1 p-list1 s-list1)
	       (stind=tree-elim-dac (cons number treepos) variable first-binder
				    alist offset max cut-length 1 t index)
	     (multiple-value-bind (pos-list2 stop-list2 p-list2 s-list2)
		 (stind=tree-elim-dac treepos variable first-binder alist
				      offset max cut-length (1+ number) nil index)
	       (if (and (listp symbol)
			(or (< symbol-termpos cut-length)
			    (and (= symbol-termpos cut-length)
				 (<= symbol-number max)
				 (not in-alist))))
		   (values nil
			   nil
			   (nconc (mapcar #'(lambda (element)
					      (car element))
					  pos-list1)
				  p-list1 p-list2)
			   (nconc (mapcar #'(lambda (element)
					    (car element))
					stop-list1)
				s-list1 s-list2))
		 (let ((new-pos-list1 (mapcar #'(lambda (element)
						  (list (car element)
							(cons (make-instance 'stind+binding-subst
									     :head-symbol new-symbol
									     :type (stind=type treepos index) 
									     :pos new-termpos
									     :binder new-binder)
							      (second element))))
					      pos-list1))
		       (new-stop-list1 (mapcar #'(lambda (element)
						   (list (car element)
							 (cons (make-instance 'stind+binding-subst
									      :head-symbol new-symbol
									      :type (stind=type treepos index)
									      :pos new-termpos
									      :binder new-binder)
							       (second element))))
					       stop-list1)))

		   (values (nconc new-pos-list1 pos-list2)
			   (nconc new-stop-list1 stop-list2)
			   (nconc p-list1 p-list2)
			   (nconc s-list1 s-list2))))))))))











;;; stind=tree-test-args and stind=tree-test-args-dac check the condition of the elimination rule for the term of the left side of the equation, i.e. they check the arguments of a function variable. If the condition is fulfilled for a branch of the subtree with root TREEPOS of the substitution tree INDEX, a list is created with a binding and the list of the variables which will be bound by the elimination rule. If the condition fails for a branch the list only consists of a node and a binding. Therefore the result comprises four parts separated into leaves and inner nodes and bindings for unsolved problems and for the application of the elimination rule.

(defun stind=tree-test-args (treepos index)
  (declare (edited  "06-NOV-1996")
	   (authors Lklein)
	   (input   "A node and a substitution tree.")
	   (effect  "None.")
	   (value   "Four values: the first two values are lists of node-binding-list. Each of the next two values consists of a list of lists with three entries: node, binding and a list of bound variables."))
  (let ((termpos (stind=pos treepos index)))
    (stind=tree-test-args-dac treepos (length termpos) 1 nil index)))


(defun stind=tree-test-args-dac (treepos cut-length number flag index)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((max (stind=max treepos index))
	 (current-termpos (stind=pos treepos index))
	 (current-length (length current-termpos))
	 (symbol (stind=head treepos index))
	 (new-symbol (if (listp symbol)
			 (list (car symbol) (stind=fix-termpos (second symbol) cut-length))
		       symbol))
	 (symbol-length (if (listp symbol)
			    (length (second symbol))
			  nil))
	 (binder (stind=binder treepos index))
	 (correct (if (and (= binder 0)
			   (listp symbol)
			   (= symbol-length cut-length))
		      t
		    nil))
	 (new-termpos (stind=fix-termpos current-termpos cut-length)))
    (cond ((and flag
		(<= current-length cut-length))
	   (values nil
		   nil
		   (list (list treepos nil nil))
		   nil
		   nil
		   nil))
	  ((listp max)
	   (if (or (not (listp symbol))
		   (>= (length (second symbol)) cut-length))
	       (if (or correct
		       (null flag))
		   (values nil
			   nil
			   nil
			   (list (list treepos
				       (list (make-instance 'stind+binding-subst
							    :head-symbol new-symbol
							    :pos new-termpos
							    :type (stind=type treepos index)
							    :binder binder))
				       (if (null flag)
					   nil
					 (list symbol))))
			   nil
			   nil)
		 (values nil
			 (list (list treepos
				     (list (make-instance 'stind+binding-subst
							  :head-symbol new-symbol
							  :pos new-termpos
							  :type (stind=type treepos index)
							  :binder binder))))
			 nil
			 nil
			 nil
			 nil))
	     (values nil nil nil nil nil (list treepos))))
	  ((> number max)
	   (values nil nil nil nil nil nil))
	  (t
	   (multiple-value-bind (pos-list11 stop-list11 pos-list12 stop-list12 p-list1 s-list1)
	       (stind=tree-test-args-dac (cons number treepos) cut-length 1 t index)
	     (multiple-value-bind (pos-list21 stop-list21 pos-list22 stop-list22 p-list2 s-list2)
		   (stind=tree-test-args-dac treepos cut-length (1+ number) nil index)
	       (if (or (not (listp symbol))
		       (>= symbol-length cut-length))
		   (let* ((new-pos-list12 (mapcan #'(lambda (element)
						      (if (and (> current-length cut-length)
							       (or (not correct)
								   (find symbol (third element) :test #'equal)))
							  (progn
							    (push (list (car element)
									(second element))
								  pos-list11)
							    nil)
							(list (list (car element)
								    (cons (make-instance 'stind+binding-subst
											 :head-symbol new-symbol
											 :type (stind=type treepos index)
											 :pos new-termpos
											 :binder binder)
									  (second element))
								    (if (> current-length cut-length)
									(cons symbol (third element))
								      (third element))))))
						  pos-list12))
			  (new-stop-list12 (mapcan #'(lambda (element)
						       (if (and (> current-length cut-length)
								(or (not correct)
								    (find symbol (third element) :test #'equal)))
							   (progn
							     (push (list (car element)
									 (second element))
								   stop-list11)
							     nil)
							 (list (list (car element)
								     (cons (make-instance 'stind+binding-subst
											  :head-symbol new-symbol
											  :type (stind=type treepos index)
											  :pos new-termpos
											  :binder binder)
									   (second element))
								     (if (> current-length cut-length)
									 (cons symbol (third element))
								       (third element))))))
						   stop-list12))
			  (new-pos-list11 (mapcan #'(lambda (element)
						      (list (list (car element)
								  (cons (make-instance 'stind+binding-subst
										       :head-symbol new-symbol
										       :type (stind=type treepos index) 
										       :pos new-termpos
										       :binder binder)
									(second element)))))
						  pos-list11))
			  (new-stop-list11 (mapcan #'(lambda (element)
						       (list  (list (car element)
								    (cons (make-instance 'stind+binding-subst
											 :head-symbol new-symbol
											 :type (stind=type treepos index) 
											 :pos new-termpos
											 :binder binder)
									  (second element)))))
						   stop-list11)))
	       
		     (values (nconc new-pos-list11 pos-list21)
			     (nconc new-stop-list11 stop-list21)
			     (nconc new-pos-list12 pos-list22)
			     (nconc new-stop-list12 stop-list22)
			     (nconc p-list1 p-list2)
			     (nconc s-list1 s-list2)))
		 (values nil
			 nil
			 nil
			 nil
			 (nconc (mapcar #'(lambda (element)
					    (car element))
					pos-list11)
				(mapcar #'(lambda (element)
					    (car element))
					pos-list12)
				p-list1 p-list2)
			 (nconc (mapcar #'(lambda (element)
					    (car element))
					stop-list11)
				(mapcar #'(lambda (element)
					    (car element))
					stop-list12)
				s-list1 s-list2)))))))))








;;; stind=fix-termpos cuts off the last CUT-LENGTH elements of the list TERMPOSITION.

(defun stind=fix-termpos (termpos cut-length)
  (declare (edited  "04-SEP-1996")
	   (authors Lklein)
	   (input   "A list and a number.")
	   (effect  "None.")
	   (value   "Cuts off as many elements as specified by the number at the end of the list."))
  (butlast termpos cut-length))









;;; stind=term-make-binding and stind=term-make-binding-args create a binding of the substitutions of a queryterm beginning with the substitution specified by TERMPOS. VARIABLE is used for occur check-tests. The functions either return the created binding or a number if the creating process has failed. The number is 1 iff there has been an occur-check and 2 iff the binding cannot be used for unsolved problems.

(defun stind=term-make-binding (termpos variable)
  (declare (edited  "27-AUG-1996")
	   (authors Lklein)
	   (input   "A termposition and a variable for occur check-tests.")
	   (effect  "None.")
	   (value   "A binding or 1 iff an occur check arises or 2 iff the binding cannot be used for building unsolved problems."))
  (stind=term-make-binding-args termpos variable (length termpos) 1))
	

(defun stind=term-make-binding-args (termpos variable cut-length number)
  (declare (edited  "27-AUG-1996")
	   (authors Lklein)
	   (input   "A termposition, a variable for occur check-tests, a break-length and the number of the current argument.")
	   (effect  "None.")
	   (value   "A binding or 1 iff an occur check arises or 2 iff the binding cannot be used for building unsolved problems."))
  (let ((term (gethash termpos stind*substitution)))
    (if term
	(if (= number 1)
	    (let ((symbol (head-symbol term)))
	      (cond ((eql symbol variable)
		     1)
		    ((or (not (listp symbol))
			 (>= (length (second symbol)) cut-length))
		     (let ((depth (stind=term-make-binding-args (cons 1 termpos) variable
								cut-length 1))
			   (breadth (stind=term-make-binding-args termpos variable
								  cut-length (1+ number))))
		       (if (and (listp depth)
				(listp breadth))
			   (cons (make-instance 'stind+binding-subst
						:head-symbol (if (listp symbol)
								 (list (car symbol)
								       (stind=fix-termpos (second symbol)
											  cut-length))
							       symbol)
						:type (the-type term)
						:binder (binder term)
						:pos (stind=fix-termpos termpos cut-length))
				 (nconc depth
					breadth))
			 (if (or (eq depth 1)
				 (eq breadth 1))
			     1
			   2))))
		    (t
		     2)))
	  (if (gethash (cons number termpos) stind*substitution)
	      (let ((depth (stind=term-make-binding-args (cons number termpos) variable
							 cut-length 1))
		    (breadth (stind=term-make-binding-args termpos variable
							   cut-length (1+ number))))
		(if (and (listp depth)
			 (listp breadth))
		    (nconc depth breadth)
		  (if (or (eq depth 1)
			  (eq breadth 1))
		      1
		    2)))
	    nil))
      nil)))








;;; The functions stind=term-elim and stind=term-elim-dac create - analogously to stind=term-make-binding - a binding which is used for the elimination rule of the simplification algorithm. Therefore an additional condition has to be tested. FIRST-BINDER is the value of the binder slot of the first instances of the generated binding. It represents the number of abstractions performed by applying the elimination rule. The ALIST comprises all renamings of variables. OFFSET and MAX are numbers used to correct the representation of lambda-bound variables. If the additional condition fails a number is returned.

(defun stind=term-elim (termpos variable first-binder alist offset max)
  (declare (edited  "07-NOV-1996")
	   (authors Lklein)
	   (input   "A termposition, a variable for occur check-tests, a value for the binder slot of the first instance of each binding, an alist for renamings and two numbers for correcting the representation of bound variables.")
	   (effect  "None.")
	   (value   "A binding or 1 iff an occur check arises or 2 iff the binding cannot be used for building unsolved problems."))
  (stind=term-elim-dac termpos variable first-binder alist offset max t (length termpos) 1))
	

(defun stind=term-elim-dac (termpos variable first-binder alist offset max flag cut-length number)
  (declare (edited  "07-NOV-1996")
	   (authors Lklein)
	   (input   "A node, a variable for occur check-tests, a value for the binder slot of the first instance of each binding, an alist for renamings, two numbers for correcting the representation of bound variables, a flag, a break-length and the number of the current argument.")
	   (effect  "None.")
	   (value   "A binding or 1 iff an occur check arises or 2 iff the binding cannot be used for building unsolved problems."))
  (let ((term (gethash termpos stind*substitution)))
    (if term
	(let* ((symbol (head-symbol term))
	       (symbol-termpos (if (listp symbol) (length (second symbol)) nil))
	       (symbol-number (if (listp symbol) (car symbol) nil))
	       (in-alist (assoc symbol alist :test #'equal))
	       (new-symbol (if (listp symbol)
			       (if (= symbol-termpos cut-length)
				   (if in-alist
				       (list (cdr in-alist)
					     '())
				     (list (+ (car symbol)
					      offset)
					   '()))
				 (list (car symbol)
				       (stind=fix-termpos (second symbol) cut-length)))
			     symbol)))
	  (cond ((eql variable symbol)
		 1)
		((and (listp symbol)
		      (< symbol-termpos cut-length))
		 2)
		((and (listp symbol)
		      (= symbol-termpos cut-length)
		      (<= symbol-number max)
		      (not in-alist))
		 2)
		((= number 1)
		 (let ((depth (stind=term-elim-dac (cons 1 termpos) variable 0 alist
								    offset max nil cut-length 1))
		       (breadth (stind=term-elim-dac termpos variable 0 alist offset
								      max nil cut-length (1+ number))))
		    (if (and (listp depth)
			     (listp breadth))
			(cons (make-instance 'stind+binding-subst
					     :head-symbol new-symbol
					     :type (the-type term)
					     :binder (if flag
							 first-binder
						       (binder term))
					     :pos (stind=fix-termpos termpos cut-length))
			      (nconc depth
				     breadth))
		      (if (or (eq depth 1)
			      (eq breadth 1))
			  1
			2))))
		((gethash (cons number termpos) stind*substitution)
		 (let ((depth (stind=term-elim-dac (cons number termpos) variable 0 alist
								    offset max nil cut-length 1))
		       (breadth (stind=term-elim-dac termpos variable 0 alist offset max
								      nil cut-length (1+ number))))
		   (if (and (listp depth)
			    (listp breadth))
		       (nconc depth breadth)
		     (if (or (eq depth 1)
			     (eq breadth 1))
			 1
		       2))))
		(t
		 nil)))
      nil)))












;;; stind=term-test-args and stind=term-test-args-dac check the condition of the elimination rule for the term of the left side of the equation, i.e. they check the arguments of a function variable. If the condition is fulfilled for the substitution TERMPOS and its arguments of the current query term, a list is created with the variables which will be bound by the elimination rule. If the condition fails a number is returned analogously to stind=term-make-binding.

(defun stind=term-test-args (termpos)
  (declare (edited  "27-AUG-1996")
	   (authors Lklein)
	   (input   "A termposition.")
	   (effect  "None.")
	   (value   "Checks the conditions of the elimination rule for the term on the left side of the equation. If the conditions are fulfilled a list of all bound variables is returned. Else the function returns 1."))
  (stind=term-test-args-dac termpos (length termpos) 1 nil))
	

(defun stind=term-test-args-dac (termpos cut-length number flag)
  (declare (edited  "27-AUG-1996")
	   (authors Lklein)
	   (input   "A termposition, a break-length, the number of the current argument and a flag.")
	   (effect  "None.")
	   (value   "Checks the conditions of the elimination rule for the term on the left side of the equation. If the conditions are fulfilled a list of all bound variables is returned. Otherwise the function returns 1."))
  (let ((term (gethash termpos stind*substitution)))
    (if term
	(if (= number 1)
	    (let ((symbol (head-symbol term)))
	      (if (or (and flag
			   (= (binder term) 0)
			   (listp symbol)
			   (= (length (second symbol)) cut-length))
		      (not flag))
		  (let ((depth (stind=term-test-args-dac (cons 1 termpos) cut-length 1 t))
			(breadth (stind=term-test-args-dac termpos cut-length (1+ number) t)))
		    (if (and (listp depth)
			     (listp breadth)
			     (not (find symbol depth :test #'equal))
			     (not (find symbol breadth :test #'equal))
			     (not (intersection depth breadth :test #'equal)))
			(if flag
			    (cons symbol (nconc depth breadth))
			  (nconc depth breadth))
		      1))
		1))
	  (if (gethash (cons number termpos) stind*substitution)
	      (let ((depth (stind=term-test-args-dac (cons number termpos) cut-length 1 t))
		    (breadth (stind=term-test-args-dac termpos cut-length (1+ number) t)))
		(if (and (listp depth)
			 (listp breadth)
			 (not (intersection depth breadth :test #'equal)))
		    (nconc depth breadth)
		  1))
	    nil))
      nil)))



  
	    







;(defun stind=term-binder (termpos)
;  (declare (edited  "27-AUG-1996")
;	   (authors Lklein)
;	   (input   )
;	   (effect  )
;	   (value   ))
;  (binder (gethash termpos stind*substitution)))





	 

;(defun stind=term-head (termpos)
;  (declare (edited  "27-AUG-1996")
;	   (authors Lklein)
;	   (input   )
;	   (effect  )
;	   (value   ))
;  (head-symbol (gethash termpos stind*substitution)))





;(defun stind=term-args (termpos)
;  (declare (edited  "27-AUG-1996")
;	   (authors Lklein)
;	   (input   )
;	   (effect  )
;	   (value   ))
;  (arguments (gethash termpos stind*substitution)))






		




     
;;; stind=instantiate-free-var is used for replacing free variable occurences by appropriate bindings. The free variables of BINDING are replaced by the bindings in the alist BINDINGS. VARIABLE is used for occur check-tests. When WHOLE-TERM is true all occurences are checked, if it is nil only the instance of binding is checked for replacement. The altered binding is returned.

(defun stind=b-instantiate-free-var (binding variable bindings whole-term) 
  (declare (edited  "09-SEP-1996")
	   (authors Lklein)
	   (input   "A binding, a variable for occur check-tests, an alist with bindings and a flag.")
	   (effect  "None.")
	   (value   "If the flag is true the all variables of the binding which are bound in the alist are replaced by the appropriate binding. If the falg is nil, only the first instance of the binding is processed."))
  (if (null binding)
      nil
    (let* ((first-element (car binding))
	   (symbol (head-symbol first-element))
	   (termpos (pos first-element))
	   (old-binding (if (numberp symbol)
			    (cdr (assoc symbol bindings :test #'equal))
			  nil)))
      (if (and (numberp symbol)
	       old-binding)
	  (cond ((> (binder (car old-binding)) 0)    ;;; Abstraktion
		 (multiple-value-bind (alist binding-rest)
		     (stind=find-args (cdr binding) 1 (length termpos) nil nil)
		   (let ((new-binding (stind=adapt-old-binding old-binding (binder first-element)
							       termpos variable alist t)))
		     (if (numberp new-binding)
			 1
		       (let ((instance1 (stind=b-instantiate-free-var new-binding variable
								      bindings t)))
			 (if (listp instance1)
			     (if whole-term
				 (let ((instance2 (stind=b-instantiate-free-var binding-rest variable
										bindings whole-term)))
				   (if (listp instance2)
				       (nconc instance1
					      instance2)
				     1))
			       (nconc instance1
				      binding-rest))
			   1))))))
		(t
		 (let ((fixed-binding (stind=fix-old-binding old-binding termpos variable)))
		   (if (listp fixed-binding)
		       (if whole-term
			   (stind=b-instantiate-free-var (nconc fixed-binding
								(cdr binding))
							 variable bindings t)
			 
			 (nconc fixed-binding
				(cdr binding)))
		     1))))
	(if whole-term
	    (let ((reduce-cdr (stind=b-instantiate-free-var (cdr binding) variable bindings t)))
	      (if (numberp reduce-cdr)
		  1
		(cons (car binding)
		      reduce-cdr)))
	  binding)))))

			     









  



;;; stind=fix-old-binding is used for fitting the termpositions of the binding BINDING. The function is called when a free variable is replaced by a binding and this binding is an application or a symbol. VARIABLE is used for occur check-tests and the termpositions are enlarged with TERMPOSITION.

(defun stind=fix-old-binding (binding termpos variable)
  (declare (edited  "09-SEP-1996")
	   (authors Lklein)
	   (input   "A binding, a termposition and a variable for occur check-tests.")
	   (effect  "None.")
	   (value   "The function fits all termpositions of the binding. If the process fails 1 is returned."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element)))
	(if (equal variable
		   (head-symbol first-element))  ;;; Occur-Check
	    1
	  (let ((new-binding (stind=fix-old-binding (cdr binding) termpos
						    variable)))
	    (if (listp new-binding)
		(cons (make-instance 'stind+binding-subst
				     :head-symbol (if (listp symbol)
						      (list (car symbol)
							    (stind=enlarge-termpos termpos (second symbol)))
						    symbol)
				     :type (the-type first-element)
				     :pos (stind=enlarge-termpos termpos (pos first-element)) 
				     :binder (binder first-element))
		      new-binding)
	      1))))
    nil))
	    









;;; stind=enlarge-termpos appends two lists.

(defun stind=enlarge-termpos (base-termpos current-termpos)
  (declare (edited  "14-OCT-1996")
	   (authors Lklein)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "The first list appended to the second list."))
  (append current-termpos base-termpos))   ;;; append !!!!!











;;; stind=find-args finds the arguments of the application in the binding BINDING. NUMBER is the number of the current arguments. The instances of an argument are collected in the list COLLECT. BREAK-LENGTH is the length of the termposition of the functional symbol of the application and FLAG checks if it is the first call.

(defun stind=find-args (binding number break-length collect flag)
  (declare (edited  "11-SEP-1996")
	   (authors Lklein)
	   (input   "A binding, the number of the current argument, a break-length, a list for collecting the symbols of an argument and a flag.")
	   (effect  "The list is modified destructively.")
	   (value   "Returns an alist the the arguments of the application and the remaining binding."))
  (if binding
      (let* ((first-element (car binding))
	     (current-termpos (pos first-element))
	     (current-length (length current-termpos)))
	(if (<= current-length break-length)
	    (values (acons (list number '()) collect nil)
		    binding)
	  (if (and flag
		   (= current-length (1+ break-length)))
	      (multiple-value-bind (alist binding-rest)
		  (stind=find-args binding (1+ number)
				   break-length nil nil)
		    (values (acons (list number '()) collect alist)
			    binding-rest))
	    (let* ((symbol (head-symbol first-element))
		   (new-symbol (if (listp symbol)
				   (if (> (length (second symbol)) break-length)
				       (list (car symbol)
					     (stind=fix-termpos (second symbol) (1+ break-length)))
				     (cons t symbol))
				 symbol)))
	      (stind=find-args (cdr binding) number break-length
			       (nconc collect
				      (list (make-instance 'stind+binding-subst
							   :head-symbol new-symbol
							   :type (the-type first-element)
							   :pos (stind=fix-termpos current-termpos (1+ break-length))
							   :binder (binder first-element))))
			       t)))))
    (values (if collect
		(acons (list number '()) collect nil)
	      nil)
	    nil)))













;;; stind=adapt-old-binding fits the binding BINDING which should replace a free variable. The function is called if the binding is an abstraction. VARIABLE is used for occur check-tests. LAMBDA-BINDER is the value of the binder slot of the first instance of the adapted binding. TERMPOS is the termposition of the free variable. ALIST contains the replacing values for the lambda-bound variables of the abstraction. FLAG checks if it is the first call of the function. The function returns the adapted binding or 1 if somethings fails.

(defun stind=adapt-old-binding (binding lambda-binder termpos variable alist flag) 
  (declare (edited  "12-SEP-1996")
	   (authors Lklein)
	   (input   "A binding, a number counting abstractions, a termposition, a variable for occur check-tests, an alist for replacing lambda-bound variables and a flag.")
	   (effect  "None.")
	   (value   "Returns the adapted binding, i.e. all lambda-bound variables are replaced and all termpositions are adpated. Or it returns 1 iff an occur check arises."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element))
	     (current-termpos (stind=enlarge-termpos termpos (pos first-element)))
	     (current-binder (binder first-element))
	     (in-alist (cdr (assoc symbol alist :test #'equal))))
	(cond ((eql variable symbol)
	       1)
	      (in-alist
	       (let* ((adapted-instance (stind=adapt-instance in-alist lambda-binder
							      current-termpos flag))
		      (adapted-binding (stind=adapt-old-binding (cdr binding) nil termpos
								variable alist nil)))
		 (if (listp adapted-binding)
		     (nconc adapted-instance
			    adapted-binding)
		   1)))
	      (t
	       (let ((adapted-binding (stind=adapt-old-binding (cdr binding) nil termpos
							       variable alist nil)))
		 (if (listp adapted-binding)
		     (cons (make-instance 'stind+binding-subst
					  :head-symbol symbol
					  :type (the-type first-element)
					  :pos current-termpos
					  :binder (if flag
						      lambda-binder
						    current-binder))
			   adapted-binding)
		   1)))))
    nil))
	      









;;; stind=adapt-instance replaces an occurence of a lambda-bound variable with the binding BINDING. LAMBDA-BINDER specifies the value of the binder slot of the first instance. TERMPOS is the termposition of the lambda-bound variable and flag checks if it is the first call. The function returns the changed binding.
	  
(defun stind=adapt-instance (binding lambda-binder termpos flag)
  (declare (edited  "15-OCT-1996")
	   (authors Lklein)
	   (input   "A binding, a number for the binder slot of the first instance of the changed binding, a termposition and a flag.")
	   (effect  "None.")
	   (value   "Returns the chnaged binding."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element))
	     (adapted-instance (stind=adapt-instance (cdr binding) nil termpos nil)))
	(cons (make-instance 'stind+binding-subst
			     :head-symbol (if (listp symbol)
					      (if (numberp (car symbol))
						  (list (car symbol)
							(append (second symbol)
								termpos))
						(cdr symbol))
					    symbol)
			     :type (the-type first-element)
			     :pos (stind=enlarge-termpos termpos
							 (pos first-element))
			     :binder (if flag
					 lambda-binder
				       (binder first-element)))
	      adapted-instance))
    nil))
		















;;; stind=bb-compare-nodes sinplifies two bindings BINDING1 and BINDING2. VARIABLE1 and VARIABLE2 are the corresponding bound variables. BASE-TERMPOS1 and BASE-TERMPOS2 are the termpositions of these variables and therefore the base termpositions for the bindings. BINDINGS is an alist with bindings and mode specifies the simplification mode. The function returns five values: a flag which is true iff the simplification has suceeded, a list of unsolved problems, a list of new bindings and two lists of the remaining bindings.

(defun stind=bb-compare-nodes (binding1 variable1 base-termpos1 binding2 variable2 base-termpos2 bindings mode)
  (declare (edited  "30-SEP-1996")
	   (authors Lklein)
	   (input   "A binding, the bound variable and the termposition of the variable, another binding with variable and termposition, an alist of bindings and the simplification mode.")
	   (effect  "None.")
	   (value   "The function returns five values: a flag which is true iff the simplification has suceeded, a list of unsolved problems, a list of new bindings and two lists of the remaining bindings."))
  (let* ((first1 (car binding1))
	 (first2 (car binding2))
	 (termpos1 (pos first1))
	 (termpos2 (pos first2))
	 (termpos-length1 (length termpos1))
	 (termpos-length2 (length termpos2))
	 (symbol1 (head-symbol first1))
	 (symbol2 (head-symbol first2))
	 (binder1 (binder first1))
	 (binder2 (binder first2))
	 (type1 (the-type first1))
	 (type2 (the-type first2))
	 (function1 (type~complex-p type1)))
    (cond ((or (eql symbol1 variable2)
	       (eql symbol2 variable1))
	   nil)
	  ((and (numberp symbol1)
		(assoc symbol1 bindings :test #'equal))
	   (let ((instance (stind=b-instantiate-free-var binding1 variable2
							 bindings nil)))
	     (if (listp instance)
		 (stind=bb-compare-nodes instance variable1 base-termpos1 binding2
					 variable2 base-termpos2 bindings mode)
	       nil)))
	  ((and (numberp symbol2)
		(assoc symbol2 bindings :test #'equal)) 
	   (let ((instance (stind=b-instantiate-free-var binding2 variable1
							 bindings nil)))
	     (if (listp instance)
		 (stind=bb-compare-nodes binding1 variable1 base-termpos1 instance
					 variable2 base-termpos2 bindings mode)
	       nil)))
	  ((and (numberp symbol1)
		(not (numberp symbol2)))
	   (cond ((eq mode :inst)
		  nil)
		 ((= binder1 binder2)
		  (multiple-value-bind (lambda-list binding-rest1)
		      (stind=b-test-args (cdr binding1) termpos1 termpos-length1)
		    (if (listp lambda-list)
			(multiple-value-bind (new-binding2 binding-rest2)
			    (stind=b-elim binding2 symbol1 termpos-length2
					  (+ (length lambda-list)
					     (- binder2 binder1))
					  (stind=create-alist-with-termpos
					   lambda-list termpos2 1)
					  (- (length lambda-list) binder1)
					  binder1 nil)
			  (if (listp new-binding2)
			      (values t
				      nil
				      (acons symbol1 new-binding2 nil)
				      mode
				      binding-rest1
				      binding-rest2)
			    nil))
		      (multiple-value-bind (new-binding1 binding-rest1)
			  (stind=b-make-binding binding1 nil termpos-length1 nil)
			(if (listp new-binding1)
			    (multiple-value-bind (new-binding2 binding-rest2)
				(stind=b-make-binding binding2 nil termpos-length2 nil)
			      (if (listp new-binding2)
				  (values t
					  (list (list new-binding1 new-binding2))
					  nil
					  mode
					  binding-rest1
					  binding-rest2)
				nil))
			  nil)))))
		 (t
		  nil)))
	  ((and (numberp symbol2)
		(not (numberp symbol1)))
	   (cond ((eq mode :gen)
		  nil)
		 ((= binder2 binder1)
		  (multiple-value-bind (lambda-list binding-rest2)
		      (stind=b-test-args (cdr binding2) termpos2 termpos-length2)
		    (if (listp lambda-list)
			(multiple-value-bind (new-binding1 binding-rest1)
			    (stind=b-elim binding1 symbol2 termpos-length1
					  (+ (length lambda-list)
					     (- binder1 binder2))
					  (stind=create-alist-with-termpos
					   lambda-list termpos1 1)
					  (- (length lambda-list) binder2)
							   binder2 nil)
			  (if (listp new-binding1)
			      (values t
				      nil
				      (acons symbol2 new-binding1 nil)
				      mode
				      binding-rest1
				      binding-rest2)
			    nil))
		      (multiple-value-bind (new-binding1 binding-rest1)
			  (stind=b-make-binding binding1 nil termpos-length1 nil)
			(if (listp new-binding1)
			    (multiple-value-bind (new-binding2 binding-rest2)
				(stind=b-make-binding binding2 nil termpos-length2 nil)
			      (if (listp new-binding2)
				  (values t
					  (list (list new-binding1 new-binding2))
					  nil
					  mode
					  binding-rest1
					  binding-rest2)
				nil))
			  nil)))))
		 (t
		  nil)))
	  ((and (numberp symbol1)
		(numberp symbol2))
	   (if (= binder1 binder2)
	       (cond ((= binder1 binder2)
		      (if function1
			  (stind=bb-compare-arguments (cdr binding1) variable1 base-termpos1 (cdr binding2)
						      variable2 base-termpos2 bindings termpos-length1 mode)
			(values t
				nil
				nil
				mode
				(cdr binding1)
				(cdr binding2))))
		     ((null mode)
		      (multiple-value-bind (lambda-list binding-rest1)
			  (stind=b-test-args (cdr binding1) termpos1 termpos-length1)
			(if (listp lambda-list)
			    (multiple-value-bind (new-binding2 binding-rest2)
				(stind=b-elim binding2 symbol1 termpos-length2
					      (+ (length lambda-list)
						 (- binder2 binder1))
					      (stind=create-alist-with-termpos
					       lambda-list termpos2 1)
					      (- (length lambda-list) binder1)
					      binder1 nil)
			      (if (listp new-binding2)
				  (values t
					  nil
					  (acons symbol1 new-binding2 nil)
					  nil
					  binding-rest1
					  binding-rest2)
				nil))
			  (multiple-value-bind (lambda-list binding-rest2)
			      (stind=b-test-args (cdr binding2) termpos2 termpos-length2)
			    (if (listp lambda-list)
				(multiple-value-bind (new-binding1 binding-rest1)
				    (stind=b-elim binding1 symbol2 termpos-length1
						  (+ (length lambda-list)
						     (- binder1 binder2))
						  (stind=create-alist-with-termpos
						   lambda-list termpos1 1)
						  (- (length lambda-list) binder2)
						  binder2 nil)
				  (if (listp new-binding1)
				      (values t
					      nil
					      (acons symbol2 new-binding1 nil)
					      nil
					      binding-rest1
					      binding-rest2)
				    nil))
			      (multiple-value-bind (new-binding1 binding-rest1)
				  (stind=b-make-binding binding1 nil termpos-length1 nil)
				(if (listp new-binding1)
				    (multiple-value-bind (new-binding2 binding-rest2)
					(stind=b-make-binding binding2 nil termpos-length2 nil)
				      (if (listp new-binding2)
					  (values t
						  (list (list new-binding1 new-binding2))
						  nil
						  binding-rest1
						  binding-rest2)
					nil))
				  nil)))))))
		     ((eq mode :inst)
		      (multiple-value-bind (lambda-list binding-rest2)
			  (stind=b-test-args (cdr binding2) termpos2 termpos-length2)
			(if (listp lambda-list)
			    (multiple-value-bind (new-binding1 binding-rest1)
				(stind=b-elim binding1 symbol2 termpos-length1
					      (+ (length lambda-list)
						 (- binder1 binder2))
					      (stind=create-alist-with-termpos
					       lambda-list termpos1 1)
					      (- (length lambda-list) binder2)
					      binder2 nil)
			      (if (listp new-binding1)
				  (values t
					  nil
					  (acons symbol2 new-binding1 nil)
					  mode
					  binding-rest1
					  binding-rest2)
				nil))
			  (multiple-value-bind (new-binding1 binding-rest1)
			      (stind=b-make-binding binding1 nil termpos-length1 nil)
			    (if (listp new-binding1)
				(multiple-value-bind (new-binding2 binding-rest2)
				    (stind=b-make-binding binding2 nil termpos-length2 nil)
				  (if (listp new-binding2)
				      (values t
					      (list (list new-binding1 new-binding2))
					      nil
					      mode
					      binding-rest1
					      binding-rest2)
				    nil))
			      nil)))))
		     ((eq mode :gen)
		      (multiple-value-bind (lambda-list binding-rest1)
			  (stind=b-test-args (cdr binding1) termpos1 termpos-length1)
			(if (listp lambda-list)
			    (multiple-value-bind (new-binding2 binding-rest2)
				(stind=b-elim binding2 symbol1 termpos-length2
					      (+ (length lambda-list)
						 (- binder2 binder1))
					      (stind=create-alist-with-termpos
					       lambda-list termpos2 1)
					      (- (length lambda-list) binder1)
					      binder1 nil)
			      (if (listp new-binding2)
				  (values t
					  nil
					  (acons symbol1 new-binding2 nil)
					  mode
					  binding-rest1
					  binding-rest2)
				nil))
			  (multiple-value-bind (new-binding1 binding-rest1)
			      (stind=b-make-binding binding1 nil termpos-length1 nil)
			    (if (listp new-binding1)
				(multiple-value-bind (new-binding2 binding-rest2)
				    (stind=b-make-binding binding2 nil termpos-length2 nil)
				  (if (listp new-binding2)
				      (values t
					      (list (list new-binding1 new-binding2))
					      nil
					      mode
					      binding-rest1
					      binding-rest2)
				    nil))
			      nil)))))
		     (t
		      nil))
	     nil))
	  ((and (keim~equal (if (listp symbol1)
				(list (car symbol1)
				      (stind=enlarge-termpos base-termpos1 (second symbol1)))
			      symbol1)
			    (if (listp symbol2)
				(list (car symbol2)
				      (stind=enlarge-termpos base-termpos2 (second symbol2)))
			      symbol2))
		(= binder1 binder2)
		(keim~equal type1 type2))
	   (if function1
	       (stind=bb-compare-arguments (cdr binding1) variable1 base-termpos1 (cdr binding2)
					   variable2 base-termpos2 bindings termpos-length1 mode)
	     (values t
		     nil
		     nil
		     mode
		     (cdr binding1)
		     (cdr binding2))))
	  (t
	   nil))))













;;; stind=bb-compare-arguments simplifies the arguments of an application (decomposition). All parameters and values are analogous to stind=bb-compare-nodes.

(defun stind=bb-compare-arguments (binding1 variable1 termpos1 binding2 variable2 termpos2 bindings break-length mode)
  (declare (edited  "02-OCT-1996")
	   (authors Lklein)
	   (input   "A binding, the bound variable and the termposition of the variable, another binding with variable and termposition, an alist of bindings, the length of the termposition of the functional symbol and the simplification mode.")
	   (effect  "None.")
	   (value   "The function returns five values: a flag which is true iff the simplification has suceeded, a list of unsolved problems, a list of new bindings and two lists of the remaining bindings."))
  (do* ((new-binding nil)
	(rest1 binding1)
	(rest2 binding2)
	(new-bindings bindings (nconc new-binding new-bindings))
	(problem1 nil)
	(problem nil (nconc problem1 problem))
	(result t))
      ((or (null result)
	   (null rest1)
	   (<= (length (pos (car rest1))) break-length))
       (values result
	       problem
	       (if (equal new-bindings bindings)
		   nil
		 new-bindings)
	       mode
	       rest1
	       rest2))
    (multiple-value-setq (result problem1 new-binding mode rest1 rest2)
      (stind=bb-compare-nodes rest1 variable1 termpos1 rest2 variable2 termpos2 new-bindings mode))))
	



	      
				  






;;; stind=b-make-binding creates a binding of the first part of BINDING. VARIABLE is used for occur check-tests. BREAK-LENGTH is a break-length and flag checks if it is the first call of the function. The created binding and the remaining binding are returned or 1 if something fails.

(defun stind=b-make-binding (binding variable break-length flag)
  (declare (edited  "16-OCT-1996")
	   (authors Lklein)
	   (input   "A binding, a variable for occur check-tests, a break-length and a flag.")
	   (effect  "None.")
	   (value   "The function creates a binding of the first part of the given binding. It returns the created binding and the rest of the given binding. Or it returns 1 if something fails."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element)))
	(cond ((and flag
		    (<= (length (pos first-element)) break-length))
	       (values nil
		       binding))
	      ((eql symbol variable)
	       1)
	      (t
	       (multiple-value-bind (new-binding binding-rest)
		   (stind=b-make-binding (cdr binding) variable break-length t)
		 (if (listp new-binding)
		     (if (and (not (numberp binding-rest))
			      (or (not (listp symbol))
				  (>= (length (second symbol)) break-length)))
			 (values (cons (stind=make-instance first-element break-length t)
				       new-binding)
				 binding-rest)
		       (values binding-rest
			       2))
		   1)))))
    (values nil
	    nil)))
	
				   











;;; stind=b-elim creates - analogously to stind=b-make-binding - a binding which is used for the elimination rule of the simplification algorithm. Therefore an additional condition has to be tested. FIRST-BINDER is the value of the binder slot of the first instances of the generated binding. It represents the number of abstractions performed by applying the elimination rule. The ALIST comprises all renamings of variables. OFFSET and MAX are numbers used to correct the representation of lambda-bound variables. If the additional condition fails a number is returned.. 

(defun stind=b-elim (binding variable break-length first-binder alist offset max flag)
  (declare (edited  "05-NOV-1996")
	   (authors Lklein)
	   (input   "A binding, a variable for occur check-tests, a break-length, a value for the binder slot of the first instance of each binding, an alist for renamings, two numbers for correcting the representation of bound variables and a flag.")
	   (effect  "None.")
	   (value   "The created binding and the remaining binding or 1 iff an occur check arises."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element))
	     (symbol-termpos (if (listp symbol) (length (second symbol)) nil))
	     (symbol-number (if (listp symbol) (car symbol) nil))
	     (in-alist (if (listp symbol) (assoc symbol alist :test #'equal) nil)))
	(cond ((and flag
		    (<= (length (pos first-element)) break-length))
	       (values nil
		       binding))
	      ((eql symbol variable)
	       1)
	      (t
	       (multiple-value-bind (new-binding binding-rest)
		   (stind=b-elim (cdr binding) variable break-length 0 alist
				 offset max t)
		 (if (listp new-binding)
		     (if (or (numberp binding-rest)
			     (and (listp symbol)
				  (or (< symbol-termpos break-length)
				      (and (= symbol-termpos break-length)
					   (<= symbol-number max)
					   (not in-alist)))))
			 (values binding-rest
				 2)
		       (values (cons (make-instance 'stind+binding-subst
						    :head-symbol (if (and (listp symbol)
									  (= symbol-termpos break-length))
								     (if in-alist
									 (list (cdr in-alist)
									       '())
								       (list (+ (car symbol)
										offset)
									     '()))
								   symbol)
						    :binder (if (null flag)
								first-binder
							      (binder first-element))
						    :type (the-type first-element)
						    :pos (stind=fix-termpos (pos first-element) break-length))
				     new-binding)
			       binding-rest))
		   1)))))
    (values nil
	    nil)))
	








;;; stind=make-instance creates an instance of the class stind+binding-subst for the substitution SUBST. CUT-LENGTH is used for correcting the termpositions. If FLAG is true the binder slot is set by the substitution otherwise it is set to 0.

(defun stind=make-instance (subst cut-length flag)
  (declare (edited  "17-OCT-1996")
	   (authors Lklein)
	   (input   "A substitution, a number for correcting the termpositions and nil iff the binder slot should be set to zero.")
	   (effect  "None.")
	   (value   "Creates an instance of the class stind+binding-subst and returns it."))
  (let ((symbol (head-symbol subst)))
    (make-instance 'stind+binding-subst
		   :head-symbol (if (listp symbol)
				    (list (car symbol)
					  (stind=fix-termpos (second symbol)
							     cut-length))
				  symbol)
		   :type (the-type subst)
		   :pos (stind=fix-termpos (pos subst) cut-length)
		   :binder (if flag
			       (binder subst)
			     0))))













;;; stind=b-test-args checks the condition of the elimination rule for the term of the left side of the equation, i.e. it checks the arguments of a function variable. TERMPOS is the termposition of the function variable and BREAK-LENGTH is its length. If the condition is fulfilled for the substitutions in BINDING, a list is created with the variables which will be bound by the elimination rule. If the condition fails 1 is returned. 

(defun stind=b-test-args (binding termpos break-length)
  (declare (edited  "17-OCT-1996")
	   (authors Lklein)
	   (input   "A binding, a termposition and its length.")
	   (effect  "None.")
	   (value   "Checks the conditions of the elimination rule for the term on the left side of the equation. If the conditions are fulfilled a list of all bound variables and the remaining binding are returned. Otherwise the function returns 1."))
  (if binding
      (let* ((first-element (car binding))
	     (symbol (head-symbol first-element)))
	(if (<= (length (pos first-element)) break-length)
	    (values nil
		    binding)
	  (if (and (listp symbol)
		   (= (binder first-element) 0)
		   (= (length (second symbol)) break-length))
	      (multiple-value-bind (result binding-rest)
		  (stind=b-test-args (cdr binding) termpos break-length)
		(if (listp result)
		    (if (find symbol result :test #'equal)
			1
		      (values (cons (list (car symbol)
					  termpos)
				    result)
			      binding-rest))
		  1))
	    1)))
    (values nil
	    nil)))













;;; stind=tree-instantiate-free-var is used for replacing free variable occurences by appropriate bindings. The free variables of the subtree with node TREEPOS of the substitution tree INDEX are replaced by the bindings in the alist BINDINGS. VARIABLE is used for occur check-tests. The functions generates a binding for each path in the subtree and returns two values: one for inner nodes and one for leaves.

(defun stind=tree-instantiate-free-var (treepos variable bindings index)
  (declare (edited  "29-OCT-1996")
	   (authors Lklein)
	   (input   "A node, a variable for occur check-tests, an alist with bindings and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns a binding for each path of the subtree and occurences free variables are replaced by the appropriate binding if possible. The function returns two values, each value consists of pairs of node and binding."))
  (let* ((termpos (stind=pos treepos index))
	 (termpos-length (length termpos))
	 (pos-result nil)
	 (stop-result nil)
	 (old-binding (cdr (assoc (stind=head treepos index) bindings :test #'equal)))
	 (sons (stind=create-list-of-sons2 treepos 1 index)))
    (if (> (binder (car old-binding)) 0)  ;;; Abstraktion !
	(progn
	  (mapcar #'(lambda (nr-of-son)
		      (multiple-value-bind (pos-list stop-list)
			  (stind=tree-find-args nr-of-son 1 termpos-length
						nil nil 1 index)
			(setq pos-result
			      (nconc (mapcan #'(lambda (listelement)
						 (let ((new-binding
							(stind=adapt-old-binding old-binding
										 (stind=binder treepos index)
										 termpos variable
										 (car listelement)
										 t)))
						   (if (numberp new-binding)
						       nil
						     (list (list new-binding
								 (second listelement))))))
					     pos-list)
				     pos-result))
			(setq stop-result
			      (nconc (mapcan #'(lambda (listelement)
						 (let ((new-binding
							(stind=adapt-old-binding old-binding
										 (stind=binder treepos index)
										 termpos variable
										 (car listelement)
										 t)))
						   (if (numberp new-binding)
						       nil
						     (list (list new-binding
								 (second listelement))))))
					     stop-list)
				     stop-result))))
		  sons)
	  (values pos-result
		  stop-result))
      (let ((new-binding (stind=fix-old-binding old-binding termpos variable)))
	(if (listp new-binding)
	    (if (null sons)
		(values nil
			(list (list new-binding
				    treepos)))
	      (values (mapcar #'(lambda (current-treepos)
				  (list (stind=copy-substitutions new-binding)
					current-treepos))
			      sons)
		      nil))
	  (values nil
		  nil))))))









;;; stind=tree-find-args finds the arguments of the applications in the subtree with root TREEPOS of the substitution tree INDEX. NUMBER is the number of the current arguments. The instances of an argument are collected in the list COLLECT. BREAK-LENGTH is the length of the termposition of the functional symbol of the application and FLAG checks if it is the first call. BRANCH is the number of the current son of the node TREEPOS. The function returns two values: one for inner nodes and one for leaves. Each value is a list of pairs of arguments and node.

(defun stind=tree-find-args (treepos number break-length collect flag branch index)
  (declare (edited  "29-OCT-1996")
	   (authors Lklein)
	   (input   "A node, the number of the current argument, a break-length, a list for collecting the symbols of an argument, a flag, a number specifying the current son and a substitution tree.")
	   (effect  "None.")
	   (value   "The function returns two values: one for inner nodes and one for leaves. Each value is a list of pairs of arguments and node."))
  (let* ((symbol (stind=head treepos index))
	 (new-symbol (if (listp symbol)
			 (if (> (length (second symbol)) break-length)
			     (list (car symbol)
				   (stind=fix-termpos (second symbol) (1+ break-length)))
			   (cons t symbol))
		       symbol))
	 (max (stind=max treepos index))
	 (current-termpos (stind=pos treepos index))
	 (current-length (length current-termpos)))
    (cond ((and flag
		(<= current-length break-length))
	   (values (list (list (acons (list number '()) collect nil)
			       treepos))
		   nil))
	  ((listp max)
	   (let ((instance (list (make-instance 'stind+binding-subst
						:head-symbol new-symbol
						:type (stind=type treepos index)
						:pos (stind=fix-termpos current-termpos (1+ break-length))
						:binder (stind=binder treepos index)))))
	     (if (and (= current-length (1+ break-length))
		      collect)
		 (values nil
			 (list (list (acons (list number '()) collect
					    (acons (list (1+ number) '()) instance nil))
				     treepos)))
	       (values nil
		       (list (list (acons (list number '()) (nconc collect instance) nil)
				   treepos))))))
	  ((> branch max)
	   (values nil
		   nil))
	  (t
	   (let ((instance (list (make-instance 'stind+binding-subst
						:head-symbol new-symbol
						:type (stind=type treepos index)
						:pos (stind=fix-termpos current-termpos (1+ break-length))
						:binder (stind=binder treepos index)))))
	     (if (and flag
		      (= current-length (1+ break-length)))
		 (multiple-value-bind (pos-list1 stop-list1)
		     (stind=tree-find-args (cons branch treepos) (1+ number)
					   break-length instance t 1 index)
		   (multiple-value-bind (pos-list2 stop-list2)
		       (stind=tree-find-args treepos number break-length collect t (1+ branch) index)
		     (values (nconc (mapcar #'(lambda (listelement)
						(list (acons (list number '())
							     (stind=copy-substitutions collect)
							     (car listelement))
						      (second listelement)))
					    pos-list1)
				    pos-list2)
			     (nconc (mapcar #'(lambda (listelement)
						(list (acons (list number '())
							     (stind=copy-substitutions collect)
							     (car listelement))
						      (second listelement)))
					    stop-list1)
				    stop-list2))))
	       (multiple-value-bind (pos-list1 stop-list1)
		   (stind=tree-find-args (cons branch treepos) number break-length
					 (nconc (stind=copy-substitutions collect) instance) t 1 index)
		 (multiple-value-bind (pos-list2 stop-list2)
		     (stind=tree-find-args treepos number break-length collect t (1+ branch) index)
		   (values (nconc pos-list1 pos-list2)
			   (nconc stop-list1 stop-list2))))))))))
						 










;;; stind=create-list-of-sons2 creates a list of the treepositions the sons of the node TREEPOS. NUMBER specifies the current son and INDEX is a substitution tree.

(defun stind=create-list-of-sons2 (treepos number index)
  (declare (edited  "29-OCT-1996")
	   (authors Lklein)
	   (input   "A node, a number specifying the current son and a substitution tree.")
	   (effect  "None.")
	   (value   "Returns a list with the treepositions of all sons of the node."))
  (cond ((listp (stind=max treepos index))
	 nil)
	((> number (stind=max treepos index))
	 nil)
	(t
	 (cons (cons number treepos)
	       (stind=create-list-of-sons2 treepos (1+ number) index)))))









;;; stind=copy-subsitutions copies the substitutions SUBSTITUTIONS.

(defun stind=copy-substitutions (substitutions)
  (declare (edited  "29-OCT-1996")
	   (authors Lklein)
	   (input   "A list of substitutions.")
	   (effect  "None.")
	   (value   "A copy of the substitutions."))
  (if substitutions
      (let ((first-element (car substitutions)))
	(cons (make-instance 'stind+binding-subst
			     :head-symbol (head-symbol first-element)
			     :pos (pos first-element)
			     :binder (binder first-element)
			     :type (the-type first-element))
	      (stind=copy-substitutions (cdr substitutions))))
    nil))










;;; stind=copy-substitutions-and-enlarge-termpos copies a list of substitutions and changes the termpositions of these susbtitutions by enlarging them with BASE-TERMPOS.

(defun stind=copy-substitutions-and-enlarge-termpos (substitutions base-termpos)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   "A list of substitutions and a termposition.")
	   (effect  "None.")
	   (value   "Copies the substitutions and enlarges all termpositions with the given position."))
  (if substitutions
      (let* ((first-element (car substitutions))
	     (symbol (head-symbol first-element)))
	(cons (make-instance 'stind+binding-subst
			     :head-symbol (if (list symbol)
					      (list (car symbol)
						    (stind=enlarge-termpos base-termpos (second symbol)))
					    symbol)
			     :pos (stind=enlarge-termpos base-termpos (pos first-element))
			     :binder (binder first-element)
			     :type (the-type first-element))
	      (stind=copy-substitutions (cdr substitutions))))
    nil))


  







;;; stind=term-instantiate-free-var is used for replacing free variable occurences by appropriate bindings. The free variables of the subterm of the query term are replaced by the bindings in the alist BINDINGS. VARIABLE is used for occur check-tests. TERMPOS specifies the subterm. The function creates a binding with all replacements.

(defun stind=term-instantiate-free-var (termpos variable bindings)
  (declare (edited  "30-OCT-1996")
	   (authors Lklein)
	   (input   "A termposition, a variable for occur check-tests and an alist with bindings.")
	   (effect  "None.")
	   (value   "Returns a binding and free variables are replaced by an appropriate binding."))
  (let* ((term (gethash termpos stind*substitution))
	 (cut-length (length termpos))
	 (old-binding (cdr (assoc (head-symbol term) bindings :test #'equal))))
    (if (> (binder (car old-binding)) 0)
	(let ((alist nil))
	  (stind=adapt-old-binding old-binding (binder term) termpos variable
				   (dotimes (arg (arguments term) alist)
				     (setq alist
					   (acons (list (1+ arg) '())
						  (stind=term-find-args (cons (1+ arg) termpos)
									cut-length 1)
						  alist)))
				   t))
      (stind=fix-old-binding old-binding termpos variable))))







;;; stind=term-find-args finds the arguments of the application in the query term beginning with TERMPOS. NUMBER is the number of the current arguments. CUT-LENGTH is the length of the termposition of the functional symbol of the application.

(defun stind=term-find-args (termpos cut-length number)
  (declare (edited  "31-OCT-1996")
	   (authors Lklein)
	   (input   "A termposition, a break-length and the number of the current argument.")
	   (effect  "None.")
	   (value   "Returns a list with the the arguments of the application."))
  (let ((term (gethash termpos stind*substitution)))
    (if term
	(if (= number 1)
	    (let ((symbol (head-symbol term))
		  (depth (stind=term-find-args (cons 1 termpos) cut-length 1))
		  (breadth (stind=term-find-args termpos cut-length (1+ number))))
	      (cons (make-instance 'stind+binding-subst 
				   :head-symbol (if (listp symbol)
						    (if (> (length (second symbol)) cut-length)
							(list (car symbol)
							      (stind=fix-termpos (second symbol)
										 (1+ cut-length)))
						      (cons t symbol))
						  symbol)
				   :pos (stind=fix-termpos termpos (1+ cut-length))
				   :type (the-type term)
				   :binder (binder term))
		    (nconc depth
			   breadth)))
	  (if (gethash (cons number termpos) stind*substitution)
	      (nconc (stind=term-find-args (cons number termpos) cut-length 1)
		     (stind=term-find-args termpos cut-length (1+ number)))
	    nil))
      nil)))
	      

	    








;;; stind=tree-term-compare-nodes simplifies the subtree with root TREEPOS with the query term. BINDINGS is an alist of bindings, MODE  describes the simplification mode and INDEX is a substitution tree. The function returns two values, each a list of 4-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings and the simplification mode. The first value contains all inner nodes, the second all leaves of the subtree.

(defun stind=tree-term-compare-nodes (treepos bindings mode index)
  (declare (edited  "05-NOV-1996")
	   (authors Lklein)
	   (input   "A node, an alist with bindings, the simplification mode and a substitution tree.")
	   (effect  "None.")
	   (value   "Simplifies the specified subtree and returns two values. Each value is a list of 4-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings and the simplification mode. The first value contains all inner nodes, the second all leaves of the subtree."))
  (let* ((termpos (stind=pos treepos index))
	 (term (gethash termpos stind*substitution))
	 (termpos-length (length termpos))
	 (symbol1 (stind=head treepos index))
	 (symbol2 (head-symbol term))
	 (binder1 (stind=binder treepos index))
	 (binder2 (binder term))
	 (type1 (stind=type treepos index))
	 (type2 (the-type term))
	 (function1 (type~complex-p type1)))
    (cond ((and (numberp symbol1)
		(assoc symbol1 bindings :test #'equal))
	   (multiple-value-bind (pos-list stop-list)
	       (stind=tree-instantiate-free-var treepos nil bindings index)
	     (values (mapcan #'(lambda (element)
				 (multiple-value-bind (result unsolved new-binding new-mode binding-rest)
				     (stind=b-term-compare-nodes termpos (car element) symbol1
								 bindings '() mode)
				   (declare (ignore binding-rest))
				   (if result
				       (list (list (second element)
						   unsolved               ;;; mapcan verwenden wegen nil
						   new-binding
						   new-mode))
				     nil)))
			     pos-list)
		     (mapcan #'(lambda (element)
				 (multiple-value-bind (result unsolved new-binding new-mode binding-rest)
				     (stind=b-term-compare-nodes termpos (car element) symbol1
								 bindings '() mode)
				   (declare (ignore binding-rest))
				   (if result
				       (list (list (second element)
						   unsolved
						   new-binding
						   new-mode))
				     nil)))
			     stop-list))))
	  ((and (numberp symbol2)
		(assoc symbol2 bindings :test #'equal))
	   (let ((new-binding (stind=term-instantiate-free-var termpos nil bindings)))
	     (if (listp new-binding)
		 (stind=tree-b-compare-nodes treepos symbol2 new-binding bindings termpos mode index)
	       nil)))
	  ((and (numberp symbol1)  
		(or (not (numberp symbol2))
		    (eq mode :gen)))
	   (cond ((eq mode :inst)
		  (values nil
			  nil))
		 (t
		  (if (= binder1 binder2)
		      (let ((term-binding (stind=term-make-binding termpos nil)))
			(multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim pos-cut stop-cut)
			    (stind=tree-test-args treepos index)
			  (values (nconc
				   (mapcan #'(lambda (element)
					       (let ((new-term-binding
						      (stind=term-elim termpos symbol1
											(+ (length (third element))
											   (- binder2 binder1))
											(stind=create-alist
											 (third element) 1)
											(- (length (third element))
											   binder1) binder1)))
						 (if (listp new-term-binding)
						     (list (list (car element)
								 nil
								 (acons symbol1 new-term-binding nil)
								 mode))
						   (if (= new-term-binding 2)
						       (list (list (car element)
								   nil
								   nil
								   mode))
						   nil))))
					   pos-elim)
				   (if (listp term-binding)
				       (nconc (mapcar #'(lambda (element)
							  (list (car element)
								(list (list (second element)
									    (stind=copy-substitutions term-binding)))
								nil
								mode))
						      pos-unsolved)
					      (mapcar #'(lambda (element)
							  (list element
								nil
								nil
								mode))
						      pos-cut))
				     (if (= term-binding 2)
					 (nconc (mapcar #'(lambda (element)
							    (list (car element)
								  nil
								  nil
								  mode))
							pos-unsolved)
						(mapcar #'(lambda (element)
							    (list element
								  nil
								  nil
								  mode))
							pos-cut))
				       nil)))
				  (nconc
				   (mapcan #'(lambda (element)
					       (let ((new-term-binding
						      (stind=term-elim termpos symbol1
											(+ (length (third element))
											   (- binder2 binder1))
											(stind=create-alist
											 (third element) 1)
											(- (length (third element))
											   binder1) binder1)))
						 (if (listp new-term-binding)
						     (list (list (car element)
								 nil
								 (acons symbol1 new-term-binding nil)
								 mode))
						   (if (= new-term-binding 2)
						       (list (list (car element)
								   nil
								   nil
								   mode))
						   nil))))
					   stop-elim)
				   (if (listp term-binding)
				       (nconc (mapcar #'(lambda (element)
							  (list (car element)
								(list (list (second element)
									    (stind=copy-substitutions term-binding)))
								nil
								mode))
						      stop-unsolved)
					      (mapcar #'(lambda (element)
							  (list element
								nil
								nil
								mode))
						      stop-cut))
				     (if (= term-binding 2)
					 (nconc (mapcar #'(lambda (element)
							    (list (car element)
								  nil
								  nil
								  mode))
							stop-unsolved)
						(mapcar #'(lambda (element)
							    (list element
								  nil
								  nil
								  mode))
							stop-cut))
				       nil))))))
		    (values nil
			    nil)))))
	  ((and (numberp symbol2)
		(or (not (numberp symbol1))
		    (eq mode :inst)))
	   (cond ((eq mode :gen)
		  (values nil
			  nil))
		 (t
		  (if (= binder2 binder1)
		      (let ((lambda-list (stind=term-test-args termpos)))
			(if (listp lambda-list)
			    (multiple-value-bind (pos-list stop-list pos-cut stop-cut)
				(stind=tree-elim treepos symbol2
						 (+ (length lambda-list)
						    (- binder1 binder2))
						 (stind=create-alist lambda-list 1)
						 (- (length lambda-list) binder2)
						 binder2 index)
			      (values (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       (acons symbol2 (second element) nil)
							       mode))
						     pos-list)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     pos-cut))
				      (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       (acons symbol2 (second element) nil)
							       mode))
						     stop-list)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     stop-cut))))
			  (multiple-value-bind (pos-list stop-list pos-cut stop-cut)
			      (stind=tree-make-binding treepos symbol2 index)
			    (let ((term-binding (stind=term-make-binding termpos nil)))
			      (if (listp term-binding)
				  (values (nconc
					   (mapcar #'(lambda (element)
						       (list (car element)
							     (list (list (second element)
									 (stind=copy-substitutions term-binding)))
							     nil
							     mode))
						   pos-list)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   pos-cut))
					  (nconc
					   (mapcar #'(lambda (element)
						      (list (car element)
							    (list (list (second element)
									(stind=copy-substitutions term-binding)))
							    nil
							    mode))
						  stop-list)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   stop-cut)))
				(if (= term-binding 2)
				    (values (nconc
					   (mapcar #'(lambda (element)
						       (list (car element)
							     nil
							     nil
							     mode))
						   pos-list)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   pos-cut))
					  (nconc
					   (mapcar #'(lambda (element)
						      (list (car element)
							    nil
							    nil
							    mode))
						  stop-list)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   stop-cut)))
				  (values nil
					  nil)))))))
		    (values nil
			    nil)))))
	  ((and (numberp symbol1)
		(numberp symbol2))
	   (if (= binder1 binder2)
	       (cond ((= symbol1 symbol2)
		      (if function1
			  (stind=tree-term-compare-arguments treepos bindings termpos-length mode index)
			(if (listp (stind=max treepos index))
			    (values nil
				    (list (list treepos
						nil
						nil
						mode)))
			  (values (mapcar #'(lambda (treepos)
					      (list treepos
						    nil
						    nil
						    mode))
					  (stind=create-list-of-sons2 treepos 1 index))
				  nil))))
		     ((null mode)
		      (let ((lambda-list (stind=term-test-args termpos)))
			(if (listp lambda-list)
			    (multiple-value-bind (pos-list stop-list pos-cut stop-cut)
				(stind=tree-elim treepos symbol2
						 (+ (length lambda-list)
						    (- binder1 binder2))
						 (stind=create-alist lambda-list 1)
						 (- (length lambda-list) binder2)
						 binder2 index)
			      (values (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       (acons symbol2 (second element) nil)
							       nil))
						     pos-list)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     pos-cut))
				      (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       (acons symbol2 (second element) nil)
							       nil))
						     stop-list)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     stop-cut))))
			  (let ((term-binding (stind=term-make-binding termpos nil)))
			    (multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim pos-cut stop-cut)
				(stind=tree-test-args treepos index)
			      (values
			       (nconc
				(mapcan #'(lambda (element)
					    (let ((new-term-binding
						   (stind=term-elim termpos symbol1
										     (+ (length (third element))
											(- binder2 binder1))
										     (stind=create-alist
										      (third element) 1)
										     (- (length (third element))
											binder1)
										     binder1)))
					      (if (listp new-term-binding)
						  (list (list (car element)
							      nil
							      (acons symbol1 new-term-binding nil)
							      nil))
						(if (= new-term-binding 2)
						    (list (list (car element)
								nil
								nil
								mode))
						  nil))))
					pos-elim)
				(if (listp term-binding)
				    (nconc (mapcar #'(lambda (element)
						       (list (car element)
						      (list (list (second element)
								  (stind=copy-substitutions term-binding)))
						      nil
						      nil))
					    pos-unsolved)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   pos-cut))
				  (if (= term-binding 2)
				      (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       nil
							       mode))
						     pos-unsolved)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     pos-cut))   
				    nil)))
			       (nconc
				(mapcan #'(lambda (element)
					    (let ((new-term-binding
						   (stind=term-elim termpos symbol1
										     (+ (length (third element))
											(- binder2 binder1))
										     (stind=create-alist
										      (third element) 1)
										     (- (length (third element))
											binder1)
										     binder1)))
					      (if (listp new-term-binding)
						  (list (list (car element)
							      nil
							      (acons symbol1 new-term-binding nil)
							      nil))
						(if (= new-term-binding 2)
						    (list (list (car element)
								nil
								nil
								mode))
						  nil))))
					stop-elim)
				(if (listp term-binding)
				    (nconc (mapcar #'(lambda (element)
						       (list (car element)
							     (list (list (second element)
									 (stind=copy-substitutions term-binding)))
							     nil
							     nil))
						   stop-unsolved)
					   (mapcar #'(lambda (element)
						       (list element
							     nil
							     nil
							     mode))
						   stop-cut))
				  (if (= term-binding 2)
				      (nconc (mapcar #'(lambda (element)
							 (list (car element)
							       nil
							       nil
							       mode))
						     stop-unsolved)
					     (mapcar #'(lambda (element)
							 (list element
							       nil
							       nil
							       mode))
						     stop-cut))
				    nil))))))))))
;		     ((eq mode :inst)
;		      (let ((lambda-list (stind=term-test-args termpos)))
;			(if (listp lambda-list)
;			    (multiple-value-bind (pos-list stop-list)
;				(stind=tree-elim treepos symbol2
;								  (+ (length lambda-list)
;								     (- binder1 binder2))
;								  (stind=create-alist lambda-list 1)
;								  (- (length lambda-list) binder2)
;								  binder2 index)
;			      (values (mapcar #'(lambda (element)
;						  (list (car element)
;							nil
;							(acons symbol2 (second element) nil)
;							:inst))
;					      pos-list)
;				      (mapcar #'(lambda (element)
;						  (list (car element)
;							nil
;							(acons symbol2 (second element) nil)
;							:inst))
;					      stop-list)))
;			  (multiple-value-bind (pos-list stop-list)
;			      (stind=tree-make-binding treepos symbol2 index)
;			    (let ((term-binding (stind=term-make-binding termpos nil)))
;			      (if (listp term-binding)
;				  (values (mapcar #'(lambda (element)
;						      (list (car element)
;							    (list (list (second element)
;									(stind=copy-substitutions term-binding)))
;							    nil
;							    :inst))
;						  pos-list)
;					  (mapcar #'(lambda (element)
;						      (list (car element)
;							    (list (list (second element)
;									(stind=copy-substitutions term-binding)))
;							    nil
;							    :inst))
;						  stop-list))
;				(values nil
;					nil)))))))
;		     ((eq mode :gen)
;		      (let ((term-binding (stind=term-make-binding termpos nil)))
;			(multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim)
;			    (stind=tree-test-args treepos index)
;			  (values (nconc
;				   (mapcan #'(lambda (element)
;					       (let ((new-term-binding
;						      (stind=term-elim termpos symbol1
;											(+ (length (third element))
;											   (- binder2 binder1))
;											(stind=create-alist
;											 (third element) 1)
;											(- (length (third element))
;											   binder1) binder1)))
;						 (if (listp new-term-binding)
;						     (list (list (car element)
;								 nil
;								 (acons symbol1 new-term-binding nil)
;								 :gen))
;						   nil)))
;					   pos-elim)
;				   (if (listp term-binding)
;				       (mapcar #'(lambda (element)
;						   (list (car element)
;							 (list (list (second element)
;								     (stind=copy-substitutions term-binding)))
;							 nil
;							 :gen))
;					       pos-unsolved)
;				     nil))
;				  (nconc
;				   (mapcan #'(lambda (element)
;					       (let ((new-term-binding
;						      (stind=term-elim termpos symbol1
;											(+ (length (third element))
;											   (- binder2 binder1))
;											(stind=create-alist
;											 (third element) 1)
;											(- (length (third element))
;											   binder1) binder1)))
;						 (if (listp new-term-binding)
;						     (list (list (car element)
;								 nil
;								 (acons symbol1 new-term-binding nil)
;								 :gen))
;						   nil)))
;					   stop-elim)
;				   (if (listp term-binding)
;				       (mapcar #'(lambda (element)
;						   (list (car element)
;							 (list (list (second element)
;								     (stind=copy-substitutions term-binding)))
;							 nil
;							 :gen))
;					       stop-unsolved)
;				     nil)))))))
	     (values nil
		     nil)))
	  ((and (keim~equal symbol1 symbol2)
		(= binder1 binder2)
		(keim~equal type1 type2))
	   (if function1
	       (stind=tree-term-compare-arguments treepos bindings termpos-length mode index)
	     (if (listp (stind=max treepos index))
		 (values nil
			 (list (list treepos
				     nil
				     nil
				     mode)))
	       (values (mapcar #'(lambda (treepos)
				   (list treepos
					 nil
					 nil
					 mode))
			       (stind=create-list-of-sons2 treepos 1 index))
		       nil))))
	  (t
	   (values nil
		   nil)))))










;;; stind=tree-term-compare-arguments compares the arguments of the decomposition rule has been chosen in stind=tree-term-compare-nodes. Parameters and values are exactly like the corresponding parameters and values of stind=tree-term-compare-nodes.

(defun stind=tree-term-compare-arguments (treepos bindings break-length mode index)
  (declare (edited  "07-NOV-1996")
	   (authors Lklein)
	   (input   "A node, an alist with bindings, the length of the termposition of the functional symbol, the simplification mode and a substitution tree.")
	   (effect  "None.")
	   (value   "Simplifies the arguments of an application in the specified subtree and returns two values. Each value is a list of 4-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings and the simplification mode. The first value contains all inner nodes, the second all leaves of the subtree."))
  (do* ((queue (mapcar #'(lambda (treepos)
			   (list treepos nil nil mode))
		       (stind=create-list-of-sons2 treepos 1 index)))
	(queue-element (pop queue) (pop queue))
	(result-pos-list nil)
	(result-stop-list nil))
      ((null queue-element)
       (values result-pos-list
	       result-stop-list))
    (multiple-value-bind (pos-list stop-list)
	(stind=tree-term-compare-nodes (car queue-element) (append (third queue-element) bindings)
				       (fourth queue-element) index)
      (mapc #'(lambda (element)
		(let ((element1 (car element))
		      (element2 (second element))
		      (element3 (third element))
		      (element4 (fourth element)))
		  (if (<= (length (stind=pos element1 index)) break-length)
		      (push (list element1
				  (nconc element2 (second queue-element))
				  (nconc element3 (third queue-element))
				  element4)
			    result-pos-list)
		    (push (list element1
				(nconc element2 (second queue-element))
				(nconc element3 (third queue-element))
				element4)
			  queue))))
	    pos-list)
      (mapc #'(lambda (element)
		(push (list (car element)
			    (nconc (second element) (second queue-element))
			    (nconc (third element) (third queue-element))
			    (fourth element))
		      result-stop-list))
	    stop-list))))
      


		  
		  










;(defun stind=create-list-of-sons (treepos number index)
;  (declare (edited  "05-NOV-1996")
;	   (authors Lklein)
;	   (input   )
;	   (effect  )
;	   (value   ))
;  (let ((max (stind=max treepos index)))
;    (cond ((and (= number 1)
;		(listp max))
;	   (values nil
;		   (list treepos)))
;	  ((> number max)
;	   (values nil
;		   nil))
;	  (t
;	   (multiple-value-bind (pos-list stop-list)
;	       (stind-create-list-of-sons treepos (1+ number) index)
;	     (if (listp (stind=max (cons number treepos) index))
;		 (values pos-list
;			 (cons (cons number treepos) stop-list))
;	       (values (cons (cons number treepos) pos-list)
;		       stop-list)))))))








;;; stind=create-alist creates an alist. The elements of the list LAMBDA-LIST are the keys for an increasing sequence of numbers, starting with NUMBER. 
		 
(defun stind=create-alist (lambda-list number)
  (declare (edited  "07-NOV-1996")
	   (authors Lklein)
	   (input   "A list and a number.")
	   (effect  "None.")
	   (value   "An alist with the elements of the list as keys and numbers as values. The first element denotes the given number, the next element gets the successor of the number as value, and so on."))
  (if lambda-list
      (acons (car lambda-list)
	     number
	     (stind=create-alist (cdr lambda-list) (1+ number)))
    nil))














;;; stind=create-alist-with-termpos creates an alist. Each key is a list of an element of the list LAMBDA-LIST and the given termposition TERMPOS. This means that a key denotes a lambda-bound variable. NUMBER is the value of the first element of the alist. 

(defun stind=create-alist-with-termpos (lambda-list termpos number)
  (declare (edited  "15-NOV-1996")
	   (authors Lklein)
	   (input   "A list, a termposition and a number.")
	   (effect  "None.")
	   (value   "An alist whose keys are pairs of element of the list and the termposition and whose values are numbers. The first key gets the given number as value, the next key gets the successor of the number as value, and so on."))
  (if lambda-list
      (acons (list (caar lambda-list)
		   termpos)
	     number
	     (stind=create-alist-with-termpos (cdr lambda-list) termpos (1+ number)))
    nil))















;;; stind=tree-b-compare-nodes simplifies the subtree with root TREEPOS with the binding BINDING. BASE-TERMPOS is the termposition of the substitution in the node TREEPOS. BINDINGS is an alist of bindings, MODE  describes the simplification mode and INDEX is a substitution tree. The function returns two values, each a list of 5-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings, the simplification mode and the remaining binding. The first value contains all inner nodes, the second all leaves of the subtree.

(defun stind=tree-b-compare-nodes (treepos variable binding bindings base-termpos mode index)
  (declare (edited  "08-NOV-1996")
	   (authors Lklein)
	   (input   "A node, a variable, a binding, an alist with bindings, a termposition, the simplification mode and a substitution tree.")
	   (effect  "None.")
	   (value   "Simplifies the specified subtree and returns two values. Each value is a list of 5-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings, the simplification mode and the remaining binding. The first value contains all inner nodes, the second all leaves of the subtree."))
  (let* ((first (car binding))
	 (b-termpos (pos first))
	 (b-termpos-length (length b-termpos))
	 (tree-termpos (stind=pos treepos index))
	 (tree-termpos-length (length tree-termpos))
	 (symbol1 (stind=head treepos index))
	 (symbol2 (head-symbol first))
	 (binder1 (stind=binder treepos index))
	 (binder2 (binder first))
	 (type1 (stind=type treepos index))
	 (type2 (the-type first))
	 (function1 (type~complex-p type1)))
    (cond ((eql symbol1 variable)
	   (values nil
		   nil))
	  ((and (numberp symbol1)
		(assoc symbol1 bindings :test #'equal))
	   (multiple-value-bind (pos-list stop-list)
	       (stind=tree-instantiate-free-var treepos nil bindings index)
	     (values (mapcar #'(lambda (tree-binding treepos)
				 (multiple-value-bind (result unsolved new-binding new-mode binding-rest1 binding-rest2)
				     (stind=bb-compare-nodes tree-binding symbol1 tree-termpos
							     binding variable base-termpos bindings mode)
				   (declare (ignore binding-rest1))
				   (if result
				       (list treepos
					     unsolved
					     new-binding
					     new-mode
					     binding-rest2)
				     nil)))
			     pos-list)
		     (mapcar #'(lambda (tree-binding treepos)
				 (multiple-value-bind (result unsolved new-binding new-mode binding-rest1 binding-rest2)
				     (stind=bb-compare-nodes tree-binding symbol1 tree-termpos
							     binding variable base-termpos bindings mode)
				   (declare (ignore binding-rest1))
				   (if result
				       (list treepos
					     unsolved
					     new-binding
					     new-mode
					     binding-rest2)
				     nil)))
			     stop-list))))
	  ((and (numberp symbol2)
		(assoc symbol2 bindings :test #'equal))
	   (let ((instance (stind=b-instantiate-free-var binding variable bindings nil)))
	     (if (listp instance)
		 (stind=tree-b-compare-nodes treepos variable instance bindings base-termpos mode index)
	       (values nil
		       nil))))
	  ((and (numberp symbol1)  
		(not (numberp symbol2)))
	   (cond ((eq mode :inst)
		  (values nil
			  nil))
		 ((= binder1 binder2)
		  (multiple-value-bind (new-binding1 binding-rest1)
		      (stind=b-make-binding binding nil b-termpos-length nil)
		    (multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim)
			(stind=tree-test-args treepos index)
		      (values (nconc
			       (mapcan #'(lambda (element)
					   (multiple-value-bind (new-binding2 binding-rest2)
					       (stind=b-elim binding symbol1 b-termpos-length
									      (+ (length (third element))
										 (- binder2 binder1))
									      (stind=create-alist-with-termpos
									       (third element) b-termpos 1)
									      (- (length (third element)) binder1)
									      binder1 nil)
					     (if (listp new-binding2)
						 (list (list (car element)
							     nil
							     (acons symbol1 new-binding2 nil)
							     mode
							     binding-rest2))
					       nil)))
				       pos-elim)
			       (if (listp new-binding1)
				   (mapcar #'(lambda (element)
					       (list (car element)
						     (list (list (second element)
								 (stind=copy-substitutions new-binding1)))
						     nil
						     mode
						     binding-rest1))
					   pos-unsolved)
				 nil))
			      (nconc
			       (mapcan #'(lambda (element)
					   (multiple-value-bind (new-binding2 binding-rest2)
					       (stind=b-elim binding symbol1 b-termpos-length
									      (+ (length (third element))
										 (- binder2 binder1))
									      (stind=create-alist-with-termpos
									       (third element) b-termpos 1)
									      (- (length (third element)) binder1)
									      binder1 nil)
					     (if (listp new-binding2)
						 (list (list (car element)
							     nil
							     (acons symbol1 new-binding2 nil)
							     mode
							     binding-rest2))
					       nil)))
				       stop-elim)
			       (if (listp new-binding1)
				   (mapcar #'(lambda (element)
					       (list (car element)
						     (list (list (second element)
								 (stind=copy-substitutions new-binding1)))
						     nil
						     mode
						     binding-rest1))
					   stop-unsolved)
				 nil))))))
		 (t
		  (values nil
			  nil))))
	  ((and (numberp symbol2)
		(not (numberp symbol1)))
	   (cond ((eq mode :gen)
		  (values nil
			  nil))
		 ((= binder2 binder1)
		  (multiple-value-bind (lambda-list binding-rest)
		      (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
		    (if (listp lambda-list)
			(multiple-value-bind (pos-list stop-list)
			    (stind=tree-elim treepos symbol2
					     (+ (length lambda-list) (- binder1 binder2))
					     (stind=create-alist-with-termpos
					      lambda-list tree-termpos 1)
					     (- (length lambda-list) binder2) binder2 index)
			  (values (mapcar #'(lambda (element)
					      (list (car element)
						    nil
						    (acons symbol2 (second element) nil)
						    mode
						    binding-rest))
					  pos-list)
				  (mapcar #'(lambda (element)
					      (list (car element)
						    nil
						    (acons symbol2 (second element) nil)
						    mode
						    binding-rest))
					  stop-list)))
		      (multiple-value-bind (pos-list stop-list)
			  (stind=tree-make-binding treepos symbol2 index)
			(multiple-value-bind (new-binding binding-rest)
			    (stind=b-make-binding binding nil b-termpos-length nil)
			  (if (listp new-binding)
			      (values (mapcar #'(lambda (element)
						  (list (car element)
							(list (list (second element)
								    (stind=copy-substitutions binding)))
							nil
							mode
							binding-rest))
					      pos-list)
				      (mapcar #'(lambda (element)
						  (list (car element)
							(list (list (second element)
								    (stind=copy-substitutions new-binding)))
							nil
							mode
							binding-rest))
					      stop-list))
			    (values nil
				    nil)))))))
		 (t
		  (values nil
			  nil))))
	  ((and (numberp symbol1)
		(numberp symbol2))
	   (if (= binder1 binder2)
	       (cond ((= symbol1 symbol2)
		      (if function1
			  (stind=tree-b-compare-arguments treepos (cdr binding) variable bindings
							  tree-termpos-length base-termpos mode index)
			(if (listp (stind=max treepos index))
			    (values nil
				    (list (list treepos
						nil
						nil
						mode
						(cdr binding))))
			  (values (mapcar #'(lambda (treepos)
					      (list treepos
						    nil
						    nil
						    mode
						    (cdr binding)))
					  (stind=create-list-of-sons2 treepos 1 index))
				  nil))))
		     ((null mode)
		      (multiple-value-bind (lambda-list binding-rest)
			  (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
			(if (listp lambda-list) 
			    (multiple-value-bind (pos-list stop-list)
				(stind=tree-elim treepos symbol2
						 (+ (length lambda-list) (- binder1 binder2))
						 (stind=create-alist-with-termpos
						  lambda-list tree-termpos 1)
						 (- (length lambda-list) binder2) binder2 index)
			      (values (mapcar #'(lambda (element)
						  (list (car element)
							nil
							(acons symbol2 (second element) nil)
							nil
							binding-rest))
					      pos-list)
				      (mapcar #'(lambda (element)
						  (list (car element)
							nil
							(acons symbol2 (second element) nil)
							nil
							binding-rest))
					      stop-list)))
			  (multiple-value-bind (new-binding1 binding-rest1)
			      (stind=b-make-binding binding nil b-termpos-length nil)
			    (multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim)
				(stind=tree-test-args treepos index)
			      (values
			       (nconc
				(mapcan #'(lambda (element)
					    (multiple-value-bind (new-binding2 binding-rest2)
						(stind=b-elim binding symbol1 b-termpos-length
									       (+ (length (third element))
										  (- binder2 binder1))
									       (stind=create-alist-with-termpos
										(third element) b-termpos 1)
									       (- (length (third element)) binder1)
									       binder1 nil)
					      (if (listp new-binding2)
						  (list (list (car element)
							      nil
							      (acons symbol1 new-binding2 nil)
							      nil
							      binding-rest2))
						nil)))
					pos-elim)
				(if (listp new-binding1)
				    (mapcar #'(lambda (element)
						(list (car element)
						      (list (list (second element)
								  (stind=copy-substitutions new-binding1)))
						      nil
						      nil
						      binding-rest1))
					    pos-unsolved)
				  nil))
			       (nconc
				(mapcan #'(lambda (element)
					    (multiple-value-bind (new-binding2 binding-rest2)
						(stind=b-elim binding symbol1 b-termpos-length
									       (+ (length (third element))
										  (- binder2 binder1))
									       (stind=create-alist-with-termpos
										(third element) b-termpos 1)
									       (- (length (third element)) binder1)
									       binder1 nil)
					      (if (listp new-binding2)
						  (list (list (car element)
							      nil
							      (acons symbol1 new-binding2 nil)
							      nil
							      binding-rest2))
						nil)))
					stop-elim)
				(if (listp new-binding1)
				    (mapcar #'(lambda (element)
						(list (car element)
						      (list (list (second element)
								  (stind=copy-substitutions new-binding1)))
						      nil
						      nil
						      binding-rest1))
					    stop-unsolved)
				  nil))))))))
		     ((eq mode :inst)
		      (multiple-value-bind (lambda-list binding-rest)
			  (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
			(if (listp lambda-list)
			    (multiple-value-bind (pos-list stop-list)
				(stind=tree-elim treepos symbol2
						 (+ (length lambda-list) (- binder1 binder2))
						 (stind=create-alist-with-termpos
						  lambda-list tree-termpos 1)
						 (- (length lambda-list) binder2) binder2 index)
			      (values (mapcar #'(lambda (element)
						  (list (car element)
							nil
							(acons symbol2 (second element) nil)
							:inst
							binding-rest))
					      pos-list)
				      (mapcar #'(lambda (element)
						  (list (car element)
							nil
							(acons symbol2 (second element) nil)
							:inst
							binding-rest))
					      stop-list)))
			  (multiple-value-bind (pos-list stop-list)
			      (stind=tree-make-binding treepos symbol2 index)
			    (multiple-value-bind (new-binding binding-rest)
				(stind=b-make-binding binding nil b-termpos-length nil)
			      (if (listp new-binding)
				  (values (mapcar #'(lambda (element)
						      (list (car element)
							    (list (list (second element)
									(stind=copy-substitutions binding)))
							    nil
							    :inst
							    binding-rest))
						  pos-list)
					  (mapcar #'(lambda (element)
						      (list (car element)
							    (list (list (second element)
									(stind=copy-substitutions new-binding)))
							    nil
							    :inst
							    binding-rest))
						  stop-list))
				(values nil
					nil)))))))
		     ((eq mode :gen)
		      (multiple-value-bind (new-binding1 binding-rest1)
			  (stind=b-make-binding binding nil b-termpos-length nil)
			(multiple-value-bind (pos-unsolved stop-unsolved pos-elim stop-elim)
			    (stind=tree-test-args treepos index)
			  (values (nconc
				   (mapcan #'(lambda (element)
					       (multiple-value-bind (new-binding2 binding-rest2)
						   (stind=b-elim binding symbol1 b-termpos-length
										  (+ (length (third element))
										     (- binder2 binder1))
										  (stind=create-alist-with-termpos
										   (third element) b-termpos 1)
										  (- (length (third element)) binder1)
										  binder1 nil)
						 (if (listp new-binding2)
						     (list (list (car element)
								 nil
								 (acons symbol1 new-binding2 nil)
								 :gen
								 binding-rest2))
						   nil)))
					   pos-elim)
				   (if (listp new-binding1)
				       (mapcar #'(lambda (element)
						   (list (car element)
							 (list (list (second element)
								     (stind=copy-substitutions new-binding1)))
							 nil
							 :gen
							 binding-rest1))
					       pos-unsolved)
				     nil))
				  (nconc
				   (mapcan #'(lambda (element)
					       (multiple-value-bind (new-binding2 binding-rest2)
						   (stind=b-elim binding symbol1 b-termpos-length
										  (+ (length (third element))
										     (- binder2 binder1))
										  (stind=create-alist-with-termpos
										   (third element) b-termpos 1)
										  (- (length (third element)) binder1)
										  binder1 nil)
						 (if (listp new-binding2)
						     (list (list (car element)
								 nil
								 (acons symbol1 new-binding2 nil)
								 :gen
								 binding-rest2))
						   nil)))
					   stop-elim)
				   (if (listp new-binding1)
				       (mapcar #'(lambda (element)
						   (list (car element)
							 (list (list (second element)
								     (stind=copy-substitutions new-binding1)))
							 nil
							 :gen
							 binding-rest1))
					       stop-unsolved)
				     nil)))))))
	     (values nil
		     nil)))
	  ((and (keim~equal symbol1 (if (listp symbol2)
					(list (car symbol2)
					      (stind=enlarge-termpos base-termpos (second symbol2)))
				      symbol2))
		(= binder1 binder2)
		(keim~equal type1 type2))
	   (if function1
	       (stind=tree-b-compare-arguments treepos (cdr binding) variable bindings
					       tree-termpos-length base-termpos mode index)
	     (if (listp(stind=max treepos index))
		 (values nil
			 (list (list treepos
				     nil
				     nil
				     mode
				     (cdr binding))))
	       (values (mapcar #'(lambda (treepos)
				   (list treepos
					 nil
					 nil
					 mode
					 (cdr binding)))
			       (stind=create-list-of-sons2 treepos 1 index))
		       nil))))
	  (t
	   (values nil
		   nil)))))












;;; stind=tree-b-compare-arguments compares the arguments of the decomposition rule has been chosen in stind=tree-b-compare-nodes. Parameters and values are exactly like the corresponding parameters and values of stind=tree-b-compare-nodes.

(defun stind=tree-b-compare-arguments (treepos binding variable bindings break-length base-termpos mode index)
  (declare (edited  "12-NOV-1996")
	   (authors Lklein)
	   (input   "A node, a binding, a variable, an alist with bindings, the length of the termposition of the functional symbol, a termposition, the simplification mode and a substitution tree.")
	   (effect  "None.")
	   (value   "Simplifies the arguments of an application in the specified subtree and returns two values. Each value is a list of 4-tupels. A tupel consists of a node, a list of unsolved problems, a list of new bindings and the simplification mode. The first value contains all inner nodes, the second all leaves of the subtree."))
  (do* ((queue (mapcar #'(lambda (treepos)
			   (list treepos
				 nil
				 nil
				 mode
				 (stind=copy-substitutions binding)))
		       (stind=create-list-of-sons2 treepos 1 index)))
	(queue-element (pop queue) (pop queue))
	(result-pos-list nil)
	(result-stop-list nil))
      ((null queue-element)
       (values result-pos-list
	       result-stop-list))
    (multiple-value-bind (pos-list stop-list)
	(stind=tree-b-compare-nodes (car queue-element) variable (nth 4 queue-element)
				    (nconc (third queue-element) bindings) base-termpos
				    (fourth queue-element) index)
      (mapc #'(lambda (element)
		(if (<= (length (stind=pos (car element) index)) break-length)
		    (push (list (car element)
				(nconc (second element) (second queue-element))
				(nconc (third element) (third queue-element))
				(fourth element)
				(nth 4 element))
			  result-pos-list)
		  (push (list (car element)
			      (nconc (second element) (second queue-element))
			      (nconc (third element) (third queue-element))
			      (fourth element)
			      (nth 4 element))
			queue)))
	    pos-list)
      (mapc #'(lambda (element)
		(push (list (car element)
			    (nconc (second element) (second queue-element))
			    (nconc (third element) (third queue-element))
			    (fourth element)
			    (nth 4 element))
		      result-stop-list))
	    stop-list))))












;;; Rueckgabe: erfolgreich - unsolved - new-binding - binding-rest

;;; stind=b-term-compare-nodes sinplifies a binding BINDING and the query term at position TERM-TERMPOS. VARIABLE is the corresponding bound variable. BASE-TERMPOS1 and BASE-TERMPOS2 are the termpositions of these variables and therefore the base termpositions for the bindings. BINDINGS is an alist with bindings and mode specifies the simplification mode. The function returns five values: a flag which is true iff the simplification has suceeded, a list of unsolved problems, a list of new bindings and two lists of the remaining bindings.

(defun stind=b-term-compare-nodes (term-termpos binding variable bindings base-termpos mode)
  (declare (edited  "12-NOV-1996")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((first (car binding))
	 (b-termpos (pos first))
	 (b-termpos-length (length b-termpos))
	 (term (gethash term-termpos stind*substitution))
	 (symbol1 (head-symbol first))
	 (symbol2 (head-symbol term))
	 (binder1 (binder first))
	 (binder2 (binder term))
	 (type1 (the-type first))
	 (type2 (the-type term))
	 (function1 (type~complex-p type1)))
    (cond ((eq symbol2 variable)
	   nil)
	  ((and (numberp symbol1)
		(assoc symbol1 bindings :test #'equal))
	   (let ((instance (stind=b-instantiate-free-var binding nil bindings nil)))
	     (if (listp instance)
		 (stind=b-term-compare-nodes term-termpos instance variable bindings base-termpos mode)
	       nil)))
	  ((and (numberp symbol2)
		(assoc symbol2 bindings :test #'equal)) 
	   (let ((new-binding (stind=term-instantiate-free-var term-termpos symbol1 bindings))) 
	     (if (listp new-binding)                                                       
		 (stind=bb-compare-nodes binding variable base-termpos
					 new-binding symbol2 term-termpos bindings mode)
	       nil)))
	  ((and (numberp symbol1)  
		(not (numberp symbol2)))
	   (cond ((eq mode :inst)
		  nil)
		 ((= binder1 binder2)
		  (multiple-value-bind (lambda-list binding-rest)
		      (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
		    (if (listp lambda-list)
			(let ((new-term-binding
			       (stind=term-elim term-termpos symbol1
								 (+ (length lambda-list)
								    (- binder2 binder1))
								 (stind=create-alist-with-termpos
								  lambda-list term-termpos 1)
								 (- (length lambda-list) binder1)
								 binder1)))
			  (if (listp new-term-binding)
			      (values t
				      nil
				      (acons symbol1 new-term-binding nil)
				      mode
				      binding-rest)
			    nil))
		      (multiple-value-bind (new-binding binding-rest)
			  (stind=b-make-binding binding nil b-termpos-length nil)
			(if (listp new-binding)
			    (let ((new-term-binding (stind=term-make-binding term-termpos nil)))
			      (if (listp new-term-binding)
				  (values t
					  (list (list new-binding new-term-binding))
					  nil
					  mode
					  binding-rest)
				nil))
			  nil)))))
		 (t
		  nil)))
	  ((and (numberp symbol2)
		(not (numberp symbol1)))
	   (cond ((eq mode :gen)
		  nil)
		 ((= binder2 binder1)
		  (let ((lambda-list (stind=term-test-args term-termpos)))
		    (if (listp lambda-list)
			(multiple-value-bind (new-binding binding-rest)
			    (stind=b-elim binding symbol2 b-termpos-length
							   (+ (length lambda-list)
							      (- binder1 binder2))
							   (stind=create-alist-with-termpos
							    lambda-list b-termpos 1)
							   (- (length lambda-list) binder2)
							   binder2 nil)
			  (if (listp new-binding)
			      (values t
				      nil
				      (acons symbol2 new-binding nil)
				      mode
				      binding-rest)
			    nil))
		      (multiple-value-bind (new-binding binding-rest)
			  (stind=b-make-binding binding nil b-termpos-length nil)
			(if (listp new-binding)
			    (let ((new-term-binding (stind=term-make-binding term-termpos nil)))
			      (if (listp new-term-binding)
				  (values t
					  (list (list new-binding new-term-binding))
					  nil
					  mode
					  binding-rest)
				nil))
			  nil)))))
		 (t
		  nil)))
	  ((and (numberp symbol1)
		(numberp symbol2))
	   (if (= binder1 binder2)
	       (cond ((= symbol1 symbol2)
		      (if function1
			  (stind=b-term-compare-arguments (cdr binding) variable bindings
							  base-termpos term-termpos 1 mode)
			(values t
				nil
				nil
				mode
				(cdr binding))))
		     ((null mode)
		      (multiple-value-bind (lambda-list binding-rest)
			  (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
			(if (listp lambda-list)
			    (let ((new-term-binding
				   (stind=term-elim term-termpos symbol1
						    (+ (length lambda-list)
						       (- binder2 binder1))
						    (stind=create-alist-with-termpos
						     lambda-list term-termpos 1)
						    (- (length lambda-list) binder1)
						    binder1)))
			      (if (listp new-term-binding)
				  (values t
					  nil
					  (acons symbol1 new-term-binding nil)
					  nil
					  binding-rest)
				nil))
			  (let ((lambda-list (stind=term-test-args term-termpos)))
			    (if (listp lambda-list)
				(multiple-value-bind (new-binding binding-rest)
				    (stind=b-elim binding symbol2 b-termpos-length
						  (+ (length lambda-list)
						     (- binder1 binder2))
						  (stind=create-alist-with-termpos
						   lambda-list b-termpos 1)
						  (- (length lambda-list) binder2)
						  binder2 nil)
				  (if (listp new-binding)
				      (values t
					      nil
					      (acons symbol2 new-binding nil)
					      nil
					      binding-rest)
				    nil))
			      (multiple-value-bind (new-binding binding-rest)
				  (stind=b-make-binding binding nil b-termpos-length nil)
				(if (listp new-binding)
				    (let ((new-term-binding (stind=term-make-binding term-termpos symbol1)))
				      (if (listp new-term-binding)
					  (values t
						  (list (list new-binding new-term-binding))
						  nil
						  nil
						  binding-rest)
					nil))
				  nil)))))))
		     ((eq mode :inst)
		      (let ((lambda-list (stind=term-test-args term-termpos)))
			(if (listp lambda-list)
			    (multiple-value-bind (new-binding binding-rest)
				(stind=b-elim binding symbol2 b-termpos-length
					      (+ (length lambda-list)
						 (- binder1 binder2))
					      (stind=create-alist-with-termpos
					       lambda-list b-termpos 1)
					      (- (length lambda-list) binder2)
					      binder2 nil)
			      (if (listp new-binding)
				  (values t
					  nil
					  (acons symbol2 new-binding nil)
					  :inst
					  binding-rest)
				nil))
			  (multiple-value-bind (new-binding binding-rest)
			      (stind=b-make-binding binding nil b-termpos-length nil)
			    (if (listp new-binding)
				(let ((new-term-binding (stind=term-make-binding term-termpos nil)))
				  (if (listp new-term-binding)
				      (values t
					      (list (list new-binding new-term-binding))
					      nil
					      :inst
					      binding-rest)
				    nil))
			      nil)))))
		     ((eq mode :gen)
		      (multiple-value-bind (lambda-list binding-rest)
		      (stind=b-test-args (cdr binding) b-termpos b-termpos-length)
		    (if (listp lambda-list)
			(let ((new-term-binding
			       (stind=term-elim term-termpos symbol1
						(+ (length lambda-list)
						   (- binder2 binder1))
						(stind=create-alist-with-termpos
						 lambda-list term-termpos 1)
						(- (length lambda-list) binder1)
						binder1)))
			  (if (listp new-term-binding)
			      (values t
				      nil
				      (acons symbol1 new-term-binding nil)
				      :gen
				      binding-rest)
			    nil))
		      (multiple-value-bind (new-binding binding-rest)
			  (stind=b-make-binding binding nil b-termpos-length nil)
			(if (listp new-binding)
			    (let ((new-term-binding (stind=term-make-binding term-termpos nil)))
			      (if (listp new-term-binding)
				  (values t
					  (list (list new-binding new-term-binding))
					  nil
					  :gen
					  binding-rest)
				nil))
			  nil))))))
	     nil))
	  ((and (keim~equal symbol2 (if (listp symbol1)
					(list (car symbol1)
					      (stind=enlarge-termpos base-termpos (second symbol1)))
				      symbol1))
		(keim~equal type1 type2)
		(= binder1 binder2))
	   (if function1
	       (stind=b-term-compare-arguments (cdr binding) variable bindings base-termpos term-termpos 1 mode)
	     (values t
		     nil
		     nil
		     mode
		     (cdr binding))))
	  (t
	   nil))))





(defun stind=b-term-compare-arguments (binding variable bindings base-termpos termpos number mode)
  (declare (edited  "12-NOV-1996")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if (< number (arguments (gethash termpos stind*substitution)))
      (multiple-value-bind (result1 unsolved-problem1 new-binding1 mode1 binding-rest1)
	  (stind=b-term-compare-nodes (cons number termpos) binding variable bindings base-termpos mode)
	(if result1
	    (multiple-value-bind (result2 unsolved-problem2 new-binding2 mode2 binding-rest2)
		(stind=b-term-compare-arguments binding-rest1 variable (nconc new-binding1 bindings)
						base-termpos termpos (1+ number) mode1)
	      (if result2
		  (values t
			  (nconc unsolved-problem1 unsolved-problem2)
			  (nconc new-binding1 new-binding2)
			  mode2
			  binding-rest2)
		nil))
	  nil))
    (stind=b-term-compare-nodes (cons number termpos) binding variable bindings base-termpos mode)))
	      
		     

		     






;;; delete 

(defgeneric stind=remove-compare-heads (term treepos binder termpos free-var-counter varlist result index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input  "A term, a node, two binders and a substitution variable." )
	   (effect  "Compares the term with the nodes of the substitution tree.")
	   (value   "Four values: a node, a list, a number and true or nil."))
  (:method ((term data+abstr) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore binder))
	   (let ((domain (data~abstr-domain term)))
	     (multiple-value-bind (treepos free-var-counter varlist result)
		 (stind=remove-compare-heads (data~abstr-range term) treepos (length domain) termpos free-var-counter
					     (stind=process-domain domain 1 termpos varlist) result index)
	       (values treepos free-var-counter varlist result))))
  (:method ((term data+constant) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (if (and (keim~equal term (stind=head treepos index))
		    (= binder (stind=binder treepos index)))
	       (values (cons 1 treepos) free-var-counter varlist 't)
	     (stind=remove-backtrack (car treepos) (cdr treepos) term binder
				     termpos free-var-counter varlist index)))
  (:method ((term data+variable) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (let* ((name (cdr (assoc term varlist))))
	     (if (and (equal (if (null name) free-var-counter name)
			     (stind=head treepos index))
		      (= binder (stind=binder treepos index))
		      (keim~equal (data~annotation term) (stind=type treepos index)))
		 (if (null name)
		     (values (cons 1 treepos) (1+ free-var-counter)
			     (acons term free-var-counter varlist) 't)
		   (values (cons 1 treepos) free-var-counter varlist 't))
	       (stind=remove-backtrack (car treepos) (cdr treepos) term binder
				       termpos free-var-counter varlist index))))
  (:method ((term data+appl) treepos binder termpos free-var-counter varlist result index)
	   (declare (ignore result))
	   (let* ((appl-head (data~appl-function term))
		  (name (if (data~variable-p appl-head)
			    (cdr (assoc appl-head varlist))
			  appl-head)))
	     (if (and (keim~equal (if (null name) free-var-counter name)
				  (stind=head treepos index))
		      (= binder (stind=binder treepos index))
		      (keim~equal (data~annotation appl-head) (stind=type treepos index)))
		 (if (null name)
		     (stind=remove-compare-args (data~appl-arguments term) (cons 1 treepos)
						termpos 1 (1+ free-var-counter)
						(acons appl-head free-var-counter varlist)
						't index)
		   (stind=remove-compare-args (data~appl-arguments term) (cons 1 treepos)
					      termpos 1 free-var-counter varlist 't index))
	       (stind=remove-backtrack (car treepos) (cdr treepos) term binder termpos
				       free-var-counter varlist index)))))








;;; stind=remove-compare-args compares the arguments of an application with the nodes of a
;;; substitution tree. The returned values are built exactly like those of stind=remove-compare-heads.
;;; TERMS is a list of terms representing the arguments. TREEPOS is the first node at which
;;; a comparison is performed. BINDER1 is the binder of the head symbol of the previous node
;;; and BINDER2 is the binder of the first element of TERMS. TERMPOS is the substitution variable
;;; of the application function and the first element of TERMS is the NUMBERth argument to
;;; the application function. The RESULT describes the result of the comparison: it is true,
;;; if the previous comparisons are successful, otherwise nil.
;;; stind=remove-compare-args returns four values, exactly built like the returned values of
;;; stind=remove-compare-heads.
;;; If a comparison fails, the remaining substitutions are put into the substitution
;;; tree by calling stind=copy-arguments.


(defun stind=remove-compare-args (terms treepos termpos number free-var-counter varlist result index) 
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input   "A list of terms, a node, two binders, a substitution variable and true or nil.")
	   (effect  "Compares the arguments of an application and inserts remaining arguments if the comparison fails.")
	   (value   "A multiple value of a node, a list, a number and true or nil."))
  (if (null terms)
      (values treepos free-var-counter varlist result)
    (multiple-value-bind (treepos free-var-counter varlist result)
	(stind=remove-compare-heads (car terms) treepos 0 (cons number termpos)
				    free-var-counter varlist 't index)
      (if result
	  (stind=remove-compare-args (cdr terms) treepos termpos (1+ number)
				     free-var-counter varlist 't index)
	nil))))







;;; stind=remove-backtrack performs backtracking from one node to the father of the node
;;; and then selects the next son for further comparison. If the son doesn't exist, a new branch
;;; is created and the term is inserted into the substitution tree beginning at a new son.
;;; NUMBER is the number of the son at which backtracking has been set off, TREEPOS is the node
;;; to which backtracking is performed. TERM is the term which has to be compared or inserted.
;;; BINDER1 is the binder of the head symbol of node TREEPOS and BINDER2 is the binder of TERM.
;;; TERMPOS represents the substitution variable of the term.


(defun stind=remove-backtrack (number treepos term binder termpos free-var-counter varlist index)
  (declare (edited  "12-MAR-1996")
	   (authors Lklein)
	   (input  "A number, a node, a term, two binders and a substitution variable." )
	   (effect  "Performs backtracking and selects a new son for comparison or creates a new branch for inserting.")
	   (value   "None."))
  (if (< number (stind=max treepos index))
      (stind=remove-compare-heads term (cons (1+ number) treepos) binder
				  termpos free-var-counter varlist 't index)
    nil))





(defgeneric stind~remove-term (object index)
  (declare (edited  "11-MAR-1996")
	   (authors Lklein)
	   (input   "A term.")
	   (effect  "Removes the term from the index.")
	   (value   "True iff the term is removed."))
  (:method ((object term+term) index)
	   (let ((term object))
	     (if (listp (stind=max '() index))
		 (progn
		  ; (format t "The index is empty.")
		   nil)
	       (let ((treepos (cdr (stind=remove-compare-heads (beta~eta-longform (beta~normalize term))
							       '(1) 0 '(1) 1 nil nil index))))
		 (if treepos
		     (progn
		       (setf (maximum (gethash treepos index))
			     (delete term (maximum (gethash treepos index))))
		       t)
		   (progn
		     (format t "The term is not stored in the index.")
		     nil))))))
  (:method ((object lit+literal) index)
	   (let ((term (lit~atom object)))
	     (if (listp (stind=max '() index))
		 (progn
		  ; (format t "The index is empty.")
		   nil)
	       (let ((treepos (cdr (stind=remove-compare-heads (beta~eta-longform (beta~normalize term))
							       '(1) 0 '(1) 1 nil nil index))))
		 (if treepos
		     (progn
		       (setf (maximum (gethash treepos index))
			     (delete object (maximum (gethash treepos index))))
		       t)
		   (progn
		     (format t "The term is not stored in the index.")
		     nil)))))))









(defgeneric stind~retrieve-subterms-of-term (object index &key mode bindings) 
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (:method ((object term+term) index &key mode bindings)
	   (let ((term object))
	     (if (numberp (stind=max '() index))
		 (progn
		   (stind~convert-term term)
		   (stind=retrieve-subterms-dac (stind=term-make-binding '(1) nil) '() 1 index mode bindings))
	       (progn
		; (format t "The index is empty.")
		 nil))))
  (:method ((object lit+literal) index &key mode bindings)
	   (let ((term (lit~atom object)))
	     (if (numberp (stind=max '() index))
		 (progn
		   (stind~convert-term term)
		   (stind=retrieve-subterms-dac (stind=term-make-binding '(1) nil) '() 1 index mode bindings))
	       (progn
		 ; (format t "The index is empty.")
		 nil)))))





  



(defun stind=retrieve-subterms-dac (binding treepos number index mode long-output)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((current-treepos (cons number treepos)))
    (if (gethash current-treepos index)
	(multiple-value-bind (pos-list stop-list)
	    (stind=tree-b-compare-nodes current-treepos nil (stind=copy-substitutions binding)
					nil (stind=pos current-treepos index) mode index)
	  (let ((list2 (stind=retrieve-subterms-dac binding treepos (1+ number) index mode long-output))
		(list3 (if (listp (stind=max current-treepos index))
			   (stind=retrieve-subterms-dac binding current-treepos 1 index mode long-output)
			 nil)))
	    (nconc
	       (mapcan #'(lambda (entry)
			   (let ((term-list (stind=get-leaves (stind=max (car entry) index) index))
				 (keim-pos (stind=get-keim-position (stind=pos (car entry) index) nil index)))
			     (if term-list
				 (multiple-value-bind (unsolved-problems bindings)
				     (stind=test-unsolved-problems (second entry) (third entry) nil)
				   (declare (ignore unsolved-problems))
				   (mapcar #'(lambda (single-term)
					       (if long-output
						   (list single-term
							 (stind=create-substitution-list
							  (stind=copy-bindings bindings)
							  stind*free-vars)
							 keim-pos)
						 (list single-term keim-pos)))
					   term-list))
			       nil)))
		       pos-list)
	       (mapcan  #'(lambda (entry)
			    (let ((term-list (stind=max (car entry) index))
				  (keim-pos (pos~empty)))
			      (if term-list
				  (multiple-value-bind (unsolved-problems bindings)
				      (stind=test-unsolved-problems (second entry) (third entry) nil)
				    (declare (ignore unsolved-problems))
				    (mapcar #'(lambda (single-term)
						(if long-output
						    (list single-term
							  (stind=create-substitution-list
							   (stind=copy-bindings bindings)
							   stind*free-vars)
							  keim-pos)
						  (list single-term keim-pos)))
					    term-list))
				nil)))
			stop-list)
	       list2 list3)))
      nil)))












(defun stind=get-leaves (pos-list index)
  (declare (edited  "10-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (mapcar #'(lambda (entry)
	      (stind=get-leaves-dac (car entry) (second entry) (third entry) 1 index))
	  pos-list))










(defun stind=get-leaves-dac (treepos unsolved-problems bindings number index)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((current-treepos (cons number treepos)))
    (if (gethash current-treepos index)
	(if (listp (stind=max current-treepos index))
	    (cons (list current-treepos
			(stind=copy-unsolved-problems unsolved-problems)
			(stind=copy-bindings bindings))
		  (stind=get-leaves-dac treepos unsolved-problems bindings (1+ number) index))
	  (nconc (stind=get-leaves-dac current-treepos unsolved-problems bindings 1 index)
		 (stind=get-leaves-dac treepos unsolved-problems bindings (1+ number) index)))
      nil)))









(defun stind=copy-unsolved-problems (problems)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if problems
      (cons (list (stind=copy-substitutions (caar problems))
		  (stind=copy-substitutions (cadar problems)))
	    (stind=copy-unsolved-problems (cdr problems)))
    nil))








(defun stind=copy-bindings (bindings)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if bindings
      (acons (caar bindings)
	     (stind=copy-substitutions (cdar bindings))
	     (stind=copy-bindings (cdr bindings)))
    nil))









(defun stind=test-unsolved-problems (old-problems bindings new-problems)
  (declare (edited  "09-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if old-problems
      (let ((symbol1 (head-symbol (caaar old-problems)))
	    (symbol2 (head-symbol (caadar old-problems))))
	(if (or (and (numberp symbol1)
		     (assoc symbol1 bindings))
		(and (numberp symbol2)
		     (assoc symbol2 bindings)))
	    (multiple-value-bind (result new-unsolved-problems new-bindings)
		(stind=bb-compare-nodes (caar old-problems) nil '()
					(cadar old-problems) nil '()
					bindings nil)
	      (if result
		  (if new-bindings
		      (stind=test-unsolved-problems (nconc (cdr old-problems) new-unsolved-problems new-problems)
						    (nconc new-bindings bindings) nil)
		    (stind=test-unsolved-problems (cdr old-problems) bindings
						  (nconc new-unsolved-problems new-problems)))
		(values nil
			nil)))
	  (stind=test-unsolved-problems (cdr old-problems) bindings (nconc (list (car old-problems)) new-problems))))
    (values new-problems
	    bindings)))




;;; stind~retrieve-subterms-of-index returns 

(defgeneric stind~retrieve-subterms-of-index (object index &key mode bindings)
  (declare (edited  "15-JAN-1997")
	   (authors Lklein)
	   (input  "A term." )
	   (effect  "None.")
	   (value   "Returns all terms of an index which are contained as subterms in the query term, together with the keim-psoition, the remaining unsolved problems and the bindings made."))
  (:method ((object term+term) index &key mode bindings)
	   (let ((term object))
	     (if (numberp (stind=max '() index))
		 (stind=process-subterms (list term) term '() index mode bindings)
	       (progn
		 ; (format t "The index is empty.")
		 nil))))
  (:method ((object lit+literal) index &key mode bindings)
	   (let ((term (lit~atom object)))
	     (if (numberp (stind=max '() index))
		 (stind=process-subterms (list term) term '() index mode bindings)
	       (progn
		 ; (format t "The index is empty.")
		 nil)))))




(defun stind=process-subterms (term-list base-term processed-terms index mode long-output)
  (declare (edited  "15-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if term-list
      (let ((current-term (car term-list)))
	(if (not (find current-term processed-terms :test #'keim~equal))
	    (let ((position-list (pos~substructure-positions current-term base-term))
		  (found-subterms (stind~retrieve-term current-term index :mode mode :bindings long-output)))
	      (nconc
	       (mapcan #'(lambda (subterm)
			   (mapcar #'(lambda (position)
				       (if long-output
					   (list (car subterm)
						 (second subterm)
						 position)
					 (list subterm position)))
				   position-list))
		       found-subterms)
	       (stind=process-subterms (append (pos~substructures current-term) (cdr term-list))  ;; muss append sein
				       base-term (cons current-term processed-terms) index mode long-output)))
	  (stind=process-subterms (append (pos~substructures current-term) (cdr term-list))
				  base-term (cons current-term processed-terms) index mode long-output)))
    nil))






(defun stind=get-keim-position (treepos old-length index)
  (declare (edited  "16-JAN-1997")
	   (authors Lklein)
	   (input  "A node and the length of a termposition." )
	   (effect  )
	   (value   "The keim-position corresponding to the subterm at the node."))
  (let*  ((current-termpos (stind=pos treepos index))
	  (new-length (length current-termpos)))
    (if (= new-length 1)
	(let ((leading-zeros (do* ((i (stind=binder treepos index) (1- i))
				   (result (if (zerop i)
					       nil
					     (pos~empty))
					   (pos~add-front 0 result)))
				 ((zerop i) result)))
	      (last-value (if (type~complex-p (stind=type treepos index))
			      0
			    (car (stind=pos treepos index)))))
	  (if (null old-length)
	      (if leading-zeros
		  (pos~add-end last-value leading-zeros)
		(pos~add-end last-value (pos~empty)))
	    leading-zeros))
      (if (or (null old-length)
	      (< new-length old-length))
	  (let* ((leading-zeros (do* ((i (stind=binder treepos index) (1- i))
				     (result (if (zerop i)
						 nil
					       (pos~empty))
					     (pos~add-front 0 result)))
				   ((zerop i) result)))
		 (position-rest (stind=get-keim-position (cdr treepos) new-length index))
		 (rest (if position-rest position-rest (pos~empty)))
		 (position (if leading-zeros
			       (pos~concatenate (pos~add-end (car current-termpos) rest) leading-zeros)
			     (pos~add-end (car current-termpos) rest))))
	    (if (and (null old-length)
		     (type~complex-p (stind=type treepos index)))
		(pos~add-end 0 position)
	      position))
	(stind=get-keim-position (cdr treepos) old-length index)))))






(defun stind=process-domain (domain number termpos varlist)
  (declare (edited  "29-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (if domain
      (acons (car domain) (list number termpos) 
	     (stind=process-domain (cdr domain) (1+ number) termpos varlist))
    varlist))
       






(defgeneric beta~eta-longform (term)
  (declare (edited  "29-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (:method ((term data+abstr))
	   (beta~eta-expand (data~abstr-create (data~abstr-domain term)
					       (beta~eta-longform (data~abstr-range term)))))
  (:method ((term data+primitive))
	   (beta~eta-expand term))
  (:method ((term data+appl))
	   (let* ((args (data~appl-arguments term))
		  (new-args (mapcar #'beta~eta-longform
				    args)))
	     (if (every #'eq new-args args)
		 (beta~eta-expand term)
	       (beta~eta-expand (data~appl-create (data~appl-function term)
						  new-args))))))



(defgeneric beta~eta-expand (term)
  (declare (edited  "29-JAN-1997")
	   (authors Lklein)
	   (input   )
	   (effect  )
	   (value   ))
  (:method ((term data+abstr))
	   (let* ((domain (data~abstr-domain term))
		  (range (data~abstr-range term))
		  (type (data~annotation range)))
	     (if (type~primitive-p type)
		 term
	       (let* ((newvar (term~variable-create (gensym "dc") (data~abstr-c-domain type)))
										    ;;vorher type~c-domain
		      (newterm (data~abstr-create newvar (data~appl-create range (list newvar)))))
		 (beta~eta-expand
		  (data~abstr-create domain newterm))))))
  (:method ((term data+primitive))
	   (let ((type (data~annotation term)))
	     (if (type~primitive-p type)
		 term
	       (let ((newvar (term~variable-create (gensym "dc") (data~abstr-c-domain type)))) ;;vorher type~c-domain 
		 (beta~eta-expand (data~abstr-create newvar (data~appl-create term (list newvar))))))))
  (:method ((term data+appl))
	   (let ((type (data~annotation term)))
	     (if (type~primitive-p type)
		 term
	       (let ((newvar (term~variable-create (gensym "dc") (data~abstr-c-domain type))))  ;;vorher type~c-domain
		 (beta~eta-expand (data~abstr-create newvar (data~appl-create term (list newvar)))))))))



