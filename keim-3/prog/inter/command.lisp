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
 

(mod~defmod COM 
            :uses (arg asi dflt doc help inter keim man pp sys)
            :documentation "The basic command structures"
            :exports (
                      com+abort-command
                      com+abort-command-command
                      com+category
                      com+category.None.
                      com+command
                      com+fragment
                      com+latex-doc
                      com+latex-man
                      
                      comint~fragments
                      com~agent-computation
                      com~apply-defaults
                      com~apply-special-defaults
                      com~arghelps
                      com~argnames
                      com~argtypes
                      com~categories
                      com~category-hash-table
                      com~category-p
                      com~command-p
                      com~command-to-help-p
                      com~commands
                      com~default-fn
                      com~defcategory
                      com~defcommand
                      com~deffragment
                      com~find-category
                      com~find-command
                      com~find-fragment
                      com~frag-all-categories
                      com~frag-direct-categories
                      com~frag-direct-commands
                      com~frag-uses-comms-of
                      com~fragment
                      com~function
                      com~general-default-computation
                      com~help
                      com~level
                      com~log-p
                      com~read-command
                      com~read-command-to-help
                      com~set-agent-computation
                      com~specified-arg-p
                      com~unspecified
                      
                      com*agent-computation
                      com*category-hash-table
                      com*fragment-hash-table
                      com*unspecified
                      comint*current-comint
                      comint*help-on-comint))



#{
\section{Commands}
 A command is, conceptually, a function to be applied by a user,
 but provides an interface over the actual function. This interface
 allows one to specify the types the arguments should have and to
 describe the arguments to the user. 

 We also allow commands to be grouped in categories, so that they
 can be kept track of and/or appropriately displayed to the user.
#}

;; We'll use this hash table to keep track of all the categories that
;; exist at any given time 
(eval-when (load compile eval)
(defvar com*category-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by category name, that holds all existing command
categories.")
(defvar com*fragment-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by fragment name, that holds all existing fragments of commands.")
)

(defun com~category-hash-table ()
  (declare (authors nesmith)
	   (input "None")
	   (value "The hash table where all categories are stored,"
		  "keyed by command name (stored as strings, so packages makes no difference)."))
  com*category-hash-table)


#{\subsection{Categories}

 A command category is a way to group commands in a logical
 fashion, so that a hierarchy of commands can be defined.  One may
 want to define {\vb system} commands or commands pertaining to a specific
 prover, etc.  One may then break down this category into commands
 relating to natural deduction proving, or to commands that do
 printing, etc.  This information does not really affect how the
 command works, but is merely available if a program wishes to take
 advantage of it.
#}

(eval-when (load compile eval)
(defclass com+category (help+help keim+name)
  ((commands :initarg :commands
	     :initform nil
	     :accessor com~commands
	     :documentation "The list of commands of this category."))
  (:documentation "General class of command categories."))

(defclass com+fragment (help+help keim+name)
  ((commands :initarg :commands 
	     :accessor com~frag-direct-commands
	     :documentation "A hash table holding all commands, which belong to this fragment.
                             Note that all the fragments must be disjoint contrarily to the categories.")
   (uses-comms-of :initarg :uses-comms-of
	      :accessor com~frag-uses-comms-of
	      :documentation "Fragments, their direct commands are used from this one too.")
   (categories :initform nil
	       :initarg :categories
	       :accessor com~frag-direct-categories
	       :documentation "The list of categories that is relevant for this fragment. These are the
                               categories to which the direct commands belong."))
  (:documentation "The class of all categories that define a fragment of commands for command interpreters."))
)

(eval-when (load compile eval)
(defmacro mapcaap (function &rest list)
    (declare (edited  "24-MAR-1995")
	     (authors Acsehn)
	     (input "A FUNCTION and a LIST." )
	     (effect "None." )
	     (value  "Like the result of(mapcan FUNCTION LIST)."))
    `(apply #'append (mapcar ,function ,@list)))
)

(defmethod com~frag-all-categories ((frag com+fragment))
  (declare (edited  "20-JUL-1995")
	   (authors Lassaad)
	   (input   "FRAGMENT")
	   (effect  "None")
	   (value   "All categories to which in the FRAGMENT direct defined and"
		    "from other fragments imported commands belong."  ))
  (let ((cats (com~frag-direct-categories frag))
	(usedfrags (com~frag-uses-comms-of frag)))
    (if usedfrags
	(remove-duplicates (append cats (mapcaap #'com~frag-direct-categories usedfrags)))
      cats)))


(defgeneric com~commands (cat-or-frag)
  (declare (edited  "12-JUL-1995")
	   (authors Kohlhase)
	   (input   "A category or a fragment.")
	   (effect  "None.")
	   (value   "A list of all commands in {\vb CAT-OR-FRAG}."))
  (:method ((frag com+fragment))
	   (let ((coms nil)
		 (usedfrags (com~frag-uses-comms-of frag)))
	     (maphash #'(lambda (key val)
			  (declare (ignore key))
			  (push val coms))
		      (com~frag-direct-commands frag))
	     (if usedfrags
		 (let ((imported-coms nil))
		   (mapcar #'(lambda (frgt)
			       (maphash #'(lambda (key val)
					    (declare (ignore key))
					    (push val imported-coms))
					(com~frag-direct-commands frgt)))				   
			   usedfrags)
		   (append coms imported-coms))	       
	       coms))))

#{
 \subsection{Commands}
 Each {\vb com+command} object describes how a command is defined.
 The slot ARGNAMES is a list of names of the arguments that the
 command requires; these are used when interacting with the user.
 ARGTYPES are argument types (defined by {\vb arg~deftype}) corresponding
 to the ARGNAMES.  These are used in prompting the user for the
 arguments.  ARGHELPS is a list of strings which describe what each
 argument is.  FUNCTION is a function which will carry out the
 command when given the number and type of arguments specified.
 CATEGORY is a command category that this command belongs to.
 DEFAULT-FN is a symbol naming a function, which when called with a list of arguments (same number
 as the command takes), and some of which may be unspecified, computes defaults 
 for (some of) the unspecified arguments.  
 If DEFAULT-FN is NIL, then no defaulting  will be done.
 LOG-P is a boolean specifying whether the command is to be considered when writing in a log file.
#}

(eval-when (load compile eval)
(defclass com+command (help+help keim+name)
  ((argnames :initarg :argnames :initform nil :reader com=argnames
	     :documentation "Names of the arguments for this function. A list of symbols.")
   (argtypes :initarg :argtypes :initform nil :reader com=argtypes
	     :documentation "Types of the arguments. A list of symbols.")
   (arghelps :initarg :arghelps :initform nil :reader com=arghelps
	     :documentation "Help strings for the arguments.")
   (function :initarg :function :reader com=function
	     :documentation "A symbol naming the function that carries out the command.")
   (level    :initarg :level :reader com~level :initform 0
	     :documentation "A number denoting the level of the command.")
   (fragment :initarg :fragment :reader com~fragment
	     :documentation "The category (fragment) under which this command falls.")
   (categories :initarg :categories :reader com=categories
	     :documentation "The list of categories to which this command belongs.")
   (default-fn :initarg :default-fn :initform nil :reader com=default-fn
     :documentation
     "A function for computing argument defaults.")
   (log-p :initarg :log-p :initform nil :reader com=log-p
	  :documentation
	  "A boolean specifying whether this command should be written in the log file."))
  (:documentation "General class of commands."))
)

(defmethod print-object ((comm com+command) stream)
  (format stream
	  (if *print-escape* "#<com+command ~S>" "~S")
	  (keim~name comm)))

(defgeneric com~argnames (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument names for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=argnames com)))

(defgeneric com~argtypes (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument types for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=argtypes com)))

(defgeneric com~arghelps (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument help strings for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=arghelps com)))

(defgeneric com~function (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The function for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=function com)))

(defgeneric com~help (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The help string for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (help~help-string com)))

(defgeneric com~categories (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The list categories this command belongs to.")
   (effect "None"))
  (:method ((com com+command))
	   (com=categories com)))


(defgeneric com~default-fn (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "This command's default function.")
   (effect "None"))
  (:method ((command com+command))
    (com=default-fn command)))

(defgeneric com~log-p (command)
  (declare
   (authors Lassaad)
   (input "A command.")
   (value "Boolean.")
   (effect "None"))
  (:method ((command com+command))
    (com=log-p command)))


(eval-when (load compile eval)
(defun com~find-category (string-or-symbol)
  (declare
   (authors nesmith)
   (input "A string or symbol.")
   (value "The category with the given name or NIL if none exists.")
   (effect "None"))
  (gethash (etypecase string-or-symbol 
	     (string string-or-symbol)
	     (symbol (symbol-name string-or-symbol)))
	   com*category-hash-table))

(defun com~find-fragment (string-or-symbol)
  (declare
   (authors nesmith)
   (input "A string or symbol.")
   (value "The fragment with the given name or NIL if none exists.")
   (effect "None"))
  (gethash (etypecase string-or-symbol 
	     (string string-or-symbol)
	     (symbol (symbol-name string-or-symbol)))
	   com*fragment-hash-table))
)

(eval-when (load compile eval)
  
(defmethod com~find-command (str-or-sym (fragment com+fragment))
  (declare (edited  "24-JUL-1995")
	   (authors Lassaad)
	   (input  "A string or a symbol and a fragment")
	   (effect "None")
	   (value  "The command with the given name or NIL if no command of {\vb FRAGMENT} has this name." ))
  (let* ((thing (etypecase str-or-sym
		  (string str-or-sym)
		  (symbol (symbol-name str-or-sym))))
	 (com (gethash thing (com~frag-direct-commands fragment))))
    (if com
	com
      (some #'(lambda (frag)
		(gethash thing (com~frag-direct-commands frag)))
	    (com~frag-uses-comms-of fragment)))))
  

(defun com~command-p (thing)
  (declare
   (authors nesmith)
   (input "Anything.")
   (value "T if the argument is a command, otherwise NIL.")
   (effect "None"))
  (typep thing 'com+command))
)

(defmethod print-object ((argtype com+category) stream)
  (format stream "#<com+category ~A>" (keim~name argtype)))

(defun com~category-p (thing)
  (declare
   (authors nesmith)
   (input "Anything.")
   (value "T if the argument is a command category, otherwise NIL.")
   (effect "None"))
  (typep thing 'com+category))

(eval-when (load compile eval)
(defmacro com~defcategory (name &rest attribs)
  (declare (edited  "16-JUL-1997" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A written representation of an com+category."
		    "Here is an example of the syntax:"
		    \\begin{code}
		    (com~defcategory my-arithmetic
				     (subcats peano-num nat-arith)
				     (help \"My arithmetic commands.\"))
		    \\end{code})
           (effect  "Read the category, construct a real com+category.")
           (value   "None."))
  (when (com~find-category name)
    (inter~print-warning
     (asi~create)
     (format nil
	     ";;;com~~defcategory: Redefining the category ~A!" name))
    (remhash (symbol-name name) com*category-hash-table))
  (let ((help ""))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (let ((carattrib (if (symbolp (car attrib)) 
			       (symbol-name (car attrib)) 
			     (car attrib))))
	    (cond ((string-equal carattrib :help) (setq help (cadr attrib)))
		  (t (error ";;;com~~defcategory ~A: Not expecting ~S" name attrib))))
	(error ";;;com~~defcategory ~A: ~A is not a valid attribute specification" name attrib)))
    (let ((new-category (make-instance 'com+category :name name :help help)))
      (setf (gethash (symbol-name name) com*category-hash-table)
	    new-category)))))


(defmacro com~deffragment (name &rest attribs)
  (declare (edited  "16-JUL-1997" "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A written representation of an com+fragent."
		    "Here is an example of the syntax:"
		    \\begin{code}
		    (com~deffragment my-arithmetic
				     (usedfrags real-arithmetic)
				     (help \"My arithmetic commands.\"))
		    \\end{code})
           (effect  "Read the fragment, construct a real com+fragment and a com+category and registers both.")
           (value   "None."))
  (when (com~find-fragment name)
    (inter~print-warning
     (asi~create)
     (format nil
	     ";;;com~~deffragment: Redefining the fragment ~A and thereby deleting its commands!" name))
      (remhash (symbol-name name) com*category-hash-table)
      (remhash (symbol-name name) com*fragment-hash-table))
    (when (com~find-category name)
      (if (asi~yes-no-query
	   (asi~create)
	   (format nil
		   ";;;com~~deffragment: The fragment ~A exists already as a category. Redefine it as fragment and change the classification of its commands? " name))
	  (remhash (symbol-name name) com*category-hash-table)
	(return-from com~deffragment)))
    (let ((help "")
	  (usedfrags nil))
      (do ((attribs (cdr attribs) (cdr attribs))
	   (attrib (car attribs) (car attribs)))
	  ((and (null attrib) (null attribs)))
	(if (consp attrib)
	    (let ((carattrib (if (symbolp (car attrib)) 
				 (symbol-name (car attrib)) 
			       (car attrib))))
	      (cond ((string-equal carattrib :help) (setq help (cadr attrib)))
		    ((string-equal :uses-comms-of carattrib)
		     (setq usedfrags (cdr attrib)))
		    (t
		     (error ";;;com~~deffragment ~A: Not expecting ~S" name attrib))))
	  (error ";;;com~~deffragment ~A: ~A is not a valid attribute specification" name attrib)))
      (let ((badimport (find-if-not #'com~find-fragment usedfrags)))
	  (when badimport
	    (error ";;;com~~deffragment ~A: ~S does not refer to an existing fragment."
		   name badimport))
	  (let ((new-frag (make-instance 'com+fragment
					 :name name
					 :help help
					 :uses-comms-of (mapcar #'(lambda (x) (com~find-fragment x))
								usedfrags)
					 :commands (make-hash-table :test #'equal)))
		(new-cat (make-instance 'com+category :name name :help help)))
	    (setf (gethash (symbol-name name) com*fragment-hash-table)
		  new-frag)
	    (setf (gethash (symbol-name name) com*category-hash-table)
		  new-cat)))
      ))
	

(eval-when (load compile eval)
(defmacro com~defcommand (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an com+command."
		    "Here is an example of the syntax:"
 "\\begin{code}
 (com~defcommand divide
  (argnames dividend divisor)
  (argtypes number posinteger)
  (arghelps \"Number to divide\" \"Number by which to divide\")
  (function /)
  (defaults (0 1))
  (help \"Divide one number by a positive integer.\")
  (frag-cats my-arithmetic arithmetic)
  (log-p T))
 \\end{code}"
		    "Any category, fragment and argtypes used must already be defined."
		    "The defaults slot should have the following syntax:"
 "\\begin{code}
 (defaults {\\it fn-name}| {(lambda (arg1 \\ldots argn) . {\\it body})} 
                        |  ({\\it default1 \\ldots defaultn}) )
 \\end{code}"
		    "A single symbol will be interpreted as the name of a function to call."
		    "A lambda expression will be expanded into a function definition."
		    "A list of defaults will be interpreted as the defaults for the"
		    "arguments, which will be evaluated at run-time, not at definition time,"
		    "if they are not already specified."  
		    "In such a list, you can specify an ``unspecified'' argument"
		    "by a call to the function com~unspecified (with no arguments),"
		    "e.g.,  (defaults ((com~unspecified) 1))."
                    "The log-p slot should be either (log-p T) or (log-p nil)."
		    "The latter is similar to the case when this slot is not given.")
           (effect  "Read the command, construct a real com+command.")
           (value   "The new command."))
  (let ((argnames) (argtypes) (arghelps) (level 0)
	(function) (help "")  (fragment)
	(cats)     (log-p)    (default-fn))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
          (let ((carattrib (if (symbolp (car attrib))
                               (symbol-name (car attrib))
			     (car attrib))))
	    (cond 
	     ((string-equal carattrib :argnames) (setq argnames (cdr attrib)))
	     ((string-equal carattrib :argtypes) (setq argtypes (cdr attrib)))
	     ((string-equal carattrib :arghelps) (setq arghelps (cdr attrib)))
	     ((string-equal carattrib :function) (setq function (cadr attrib)))
	     ((string-equal carattrib :help)     (setq help     (cadr attrib)))
	     ((string-equal carattrib :level)    (setq level    (cadr attrib)))
	     ((string-equal carattrib :frag-cats)
	      (let ((frag-cats (cdr attrib)))
		(setq fragment (com~find-fragment (first frag-cats)))
		(setq cats (mapcar #'com~find-category frag-cats))))
	     ((string-equal carattrib :defaults) (setq default-fn (com=read-default-spec (cadr attrib) name)))
	     ((string-equal carattrib :log-p) (setq log-p (cadr attrib)))
	     (t (error ";;;com~~defcommand: Not expecting ~S" attrib))))
	(error ";;;com~~defcategory ~A: ~A is not a valid attribute specification" name attrib)))
    (unless (symbolp function)
      (error ";;;com~~defcommand: Function ~S must be a symbol." function))
    (unless (= (length argnames) (length argtypes))
      (error ";;;com~~defcommand ~A: Length of Argnames and Argtypes do not match." name))
    (let ((badname (find-if-not #'symbolp argnames)))
      (when badname
	(error ";;;com~~defcommand ~A: Argname ~S must be a symbol."
	       name badname)))
    (let ((badtype (find-if-not #'arg~find-argtype argtypes)))
      (when badtype
	(error ";;;com~~defcommand ~A: Argtype ~S does not refer to an argument type."
	       name badtype)))
    (let ((badhelp (find-if-not #'stringp arghelps)))
      (when badhelp
	(error ";;;com~~defcommand ~A: Help ~S must be a string." 
	       name badhelp)))
    (unless fragment
      (error ";;;com~~defcommand ~A: The given fragment is not known." name))
    (when (some #'not cats) 
      (error ";;;com~~defcommand ~A: Some symbol in the frag-cats list is not a known category" name))
    (unless (equal argnames (remove-duplicates argnames))
      (error ";;;com~~defcommand ~A: Argnames ~S must be unique." name argnames))
    `(block command-definition
       (when (com~find-command ',name ,fragment)
	     (inter~print-warning
	      (asi~create)
	      (format nil
		      ";;;com~~defcommand: Redefining command ~A!" ',name))
              (remhash ',(symbol-name name) ,(com~frag-direct-commands fragment)))
       ,(if (symbolp default-fn) nil default-fn)
       (let ((new-command
	      (make-instance 'com+command
			     :name ',name
			     :level ',level
			     :argtypes ',argtypes
			     :argnames ',argnames
			     :arghelps ',arghelps	       
			     :help ',help
			     :function ',function
			     :fragment ',fragment
			     :categories ',cats
			     :default-fn ',(if (symbolp default-fn) default-fn (second default-fn))
			     :log-p ',log-p)))
	 (setf (gethash ',(symbol-name name) ,(com~frag-direct-commands fragment))
	       new-command)
	 (mapcar #'(lambda (cat)
		     (unless (member new-command (com~commands cat))
		       (push new-command (com~commands cat)))
		     (unless (member cat (com~frag-direct-categories ',fragment))
		       (push cat (com~frag-direct-categories ',fragment))))
		 ',cats)))))
)

#|
OLD com~defcommand
(defmacro com~defcommand (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an com+command."
		    "Here is an example of the syntax:"
		    \\begin{code}
		    (com~defcommand divide
				    (argnames dividend divisor)
				    (argtypes number posinteger)
				    (arghelps \"Number to divide\" \"Number by which to divide\")
				    (function /)
				    (defaults (0 1))
				    (help \"Divide one number by a positive integer.\")
				    (frag-cats my-arithmetic arithmetic)
				    (log-p T))
		    \\end{code}
		    "Any category, fragment and argtypes used must already be defined."
		    "The defaults slot should have the following syntax:"
		    \\begin{code}
		    (defaults {\\it fn-name}| {(lambda (arg1 \\ldots argn) . {\\it body})} 
                    |  ({\\it default1 \\ldots defaultn}) )
		    \\end{code}
		    "A single symbol will be interpreted as the name of a function to call."
		    "A lambda expression will be expanded into a function definition."
		    "A list of defaults will be interpreted as the defaults for the"
		    "arguments, which will be evaluated at run-time, not at definition time,"
		    "if they are not already specified."  
		    "In such a list, you can specify an ``unspecified'' argument"
		    "by a call to the function com~unspecified (with no arguments),"
		    "e.g.,  (defaults ((com~unspecified) 1))."
		    "The log-p slot should be either (log-p T) or (log-p nil)."
		    "The latter is similar to the case when this slot is not given.")
           (effect  "Read the command, construct a real com+command.")
           (value   "The new command."))
  (let ((argnames nil)
	(argtypes nil)
	(arghelps nil)
	(function nil)
	(help "")
	(fragment nil)
        (cats nil)
        (default-fn nil)
	(log-p nil))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
          (let ((carattrib (if (symbolp (car attrib))
                               (symbol-name (car attrib))
			     (car attrib))))
	    (cond 
	     ((string-equal carattrib :argnames) (setq argnames (cdr attrib)))
	     ((string-equal carattrib :argtypes) (setq argtypes (cdr attrib)))
	     ((string-equal carattrib :arghelps) (setq arghelps (cdr attrib)))
	     ((string-equal carattrib :function) 
	      (setq function (cadr attrib)))
	     ((string-equal carattrib :help) (setq help (cadr attrib)))
	     ((string-equal carattrib :frag-cats)
	      (let ((frag-cats (cdr attrib)))
		(setq fragment (com~find-fragment (first frag-cats)))
		(setq cats (mapcar #'com~find-category frag-cats))))
	     ((string-equal carattrib :defaults) (setq default-fn (com=read-default-spec (cadr attrib) name)))
	     ((string-equal carattrib :log-p) (setq log-p (cadr attrib)))
	     (t (error ";;;com~~defcommand: Not expecting ~S" attrib))))
	(error ";;;com~~defcategory ~A: ~A is not a valid attribute specification" name attrib)))
    (unless (symbolp function)
      (error ";;;com~~defcommand: Function ~S must be a symbol." function))
    (unless (= (length argnames) (length argtypes))
      (error ";;;com~~defcommand ~A: Length of Argnames and Argtypes do not match." name))
    (let ((badname (find-if-not #'symbolp argnames)))
      (when badname
	(error ";;;com~~defcommand ~A: Argname ~S must be a symbol."
	       name badname)))
    (let ((badtype (find-if-not #'arg~find-argtype argtypes)))
      (when badtype
	(error ";;;com~~defcommand ~A: Argtype ~S does not refer to an argument type."
	       name badtype)))
    (let ((badhelp (find-if-not #'stringp arghelps)))
      (when badhelp
	(error ";;;com~~defcommand ~A: Help ~S must be a string." 
	       name badhelp)))
    (unless fragment
      (error ";;;com~~defcommand ~A: The given fragment is not known." name))
    (when (some #'not cats) 
      (error ";;;com~~defcommand ~A: Some symbol in the frag-cats list is not a known category" name))
    (unless (equal argnames (remove-duplicates argnames))
      (error ";;;com~~defcommand ~A: Argnames ~S must be unique." name argnames))
    `(block nil
       (when (com~find-command ',name ,fragment)
	 (error ";;;com~~defcommand ~A: command already defined, skipping definition!" ',name))
       ,(if (symbolp default-fn) nil default-fn)
       (let ((new-command
	      (make-instance 'com+command
			     :name ',name 
			     :argtypes ',argtypes
			     :argnames ',argnames
			     :arghelps ',arghelps	       
			     :help ',help
			     :function ',function
			     :fragment ',fragment
			     :categories ',cats
			     :default-fn ',(if (symbolp default-fn) default-fn (second default-fn))
			     :log-p ',log-p)))
	 (setf (gethash ',(symbol-name name) ,(com~frag-direct-commands fragment))
	       new-command)
	 (mapcar #'(lambda (cat)
		     (unless (member new-command (com~commands cat))
		       (push new-command (com~commands cat)))
		     (unless (member cat (com~frag-direct-categories ',fragment))
		       (push cat (com~frag-direct-categories ',fragment))))
		 ',cats)))))
|#
 



#{ {\vb com+abort-command} is a new condition type that is used when
 aborting a command.
#}

(sys~define-condition com+abort-command (sys+abort)
  (command)
  (lambda (condition stream)
	     (format stream "Aborting command ~A~%" 
		     (keim~name (com+abort-command-command condition)))))



#{ We define the {\vb command} argument type. #}

(eval-when (load compile eval)
(arg~deftype command
 (read-function com~read-command)
 (predicate com~command-p)
 (help "a command"))

(arg~deflisttype command-list command)
)

(defmethod com~read-command ((thing com+command) &rest others)
  (declare (ignore others))
  thing)


(defmethod com~read-command ((thing symbol) &rest others)
  (declare (ignore others))
  (let* ((frags (comint~fragments comint*current-comint))
         (com (some #'(lambda (frag)
			(com~find-command thing frag))
		    frags)))
    (if com 
	com
      (arg~signal-wrong-type 'command thing))))

(defmethod com~read-command ((thing string) &rest others)
  (declare (ignore others))
  (let* ((frags (comint~fragments comint*current-comint))
         (com (some #'(lambda (frag)
			(com~find-command thing frag))
		    frags)))
    (if com 
	com
      (arg~signal-wrong-type 'command thing))))

#{ We define the {\vb command-to-help} argument type. #}


(arg~deftype command-to-help
 (read-function com~read-command-to-help)
 (predicate com~command-to-help-p)
 (help "a command to help"))


(defmethod com~read-command-to-help ((thing symbol) &rest others)
  (declare (ignore others))
  (if comint*help-on-comint
      (let* ((frags (comint~fragments comint*help-on-comint))
	     (com (some #'(lambda (frag)
			    (com~find-command thing frag))
			frags)))
	(if com
	    com
	  (arg~signal-wrong-type 'command-to-help thing)))
    (arg~signal-wrong-type 'command-to-help thing)))


#{
\subsection{Using defaults for command arguments}
#}

(defvar com*agent-computation nil)

(defun com~agent-computation ()
  com*agent-computation)

(defun com~set-agent-computation (&optional (boolean t))
  (setf com*agent-computation boolean))

(defun com~apply-special-defaults (command arglist)
  (let* ((computation (and (com~agent-computation) (dflt~find-agents command)))
	 (defaults (when computation
		     (com~general-default-computation command
						      (mapcar #'(lambda (x)
								  (if (com~specified-arg-p x)
								      x (car x)))
							      arglist)))))
    (values defaults computation)))

(defun com~apply-defaults (command arglist)
  (declare
   (authors nesmith)
   (input "A command and a list of arguments to the command.")
   (value "The result of calling the command's default-fn on the given arguments.  If the command does
not have a default function, the list is returned unchanged."))
  (let ((default-fn (com~default-fn command)))
    (if default-fn
	(apply default-fn arglist)
      arglist)))

(defgeneric com~general-default-computation (command arglist))

(eval-when (compile load eval)
  (defvar com*unspecified (cons nil nil)))

(defun com~unspecified () 
  (declare
   (authors nesmith)
   (value "The designated undefined argument (guaranteed to be the only object that is not 
com~specified-arg-p)."))
  com*unspecified)

(defun com~specified-arg-p (arg)
  (declare
   (authors nesmith)
   (input "Any lisp object.")
   (value "True if this object is not the same as (com~unspecified). Used to tell specified arguments
to functions from unspecified."))
  (not (eq arg (com~unspecified))))

(defun com=read-default-spec (default-spec name)
  (declare
   (authors nesmith)
   (input "a specification of a default function, and symbol NAME")
   (effect "when DEFAULT-SPEC is a symbol, returns it.  If DEFAULT-SPEC is
a lambda-form, returns a {\\vb defun} using the NAME and the body of the
DEFAULT-SPEC.  Otherwise, it is assumed that DEFAULT-SPEC is a list of
forms, one for each argument, with the defaults for them.")
   (example "\\begin{code}
(com=read-default-spec 'foo 'read-fun) -> FOO
(com=read-default-spec '(lambda (x y) (body y x)) 'read-fun) ->
                 (DEFUN READ-FUN (X Y) (BODY Y X))
(com=read-default-spec '(1 2 3) 'read-fun) ->
                    (DEFUN READ-FUN (&REST GIVEN-ARGS)
                      (MAPCAR #'(LAMBDA (GIVEN DEFAULT)
           		        (IF (COM~SPECIFIED-ARG-P GIVEN) GIVEN DEFAULT))
                         GIVEN-ARGS (LIST 1 2 3)))
\\end{code}
"))
  (cond ((symbolp default-spec) default-spec)
	((listp default-spec)
	 (let ((new-name (gensym (format nil "~A-DEFAULT-FN" name))))
	   (cond ((eq (car default-spec) 'lambda)
		  `(defun ,new-name ,@(cdr default-spec)))
		 (t 
		  (com=build-default-fn default-spec new-name)))))
	(t (error "Can't read default-fn specifier ~S" default-spec))))

(defun com=build-default-fn (default-list name)
  `(defun ,name (&rest given-args)
     (mapcar #'(lambda (given default)
		 (if (com~specified-arg-p given) given default))
	     given-args (list ,@default-list))))








 
;(in-package :ags)


(eval-when (load compile eval)
  
(defmethod doc~parse-def ((type (eql 'com~defcategory)) obj)
    (values (second obj) "Command Category"
	    nil
	    (list
	     (list 'misc
		   (cadr (assoc :help (cddr obj) :test #'string=))))))

(defmethod doc~parse-def ((type (eql 'com~deffragment)) obj)
  (let ((usedfrags (cdr 
		     (assoc :uses-comms-of (cddr obj)
			    :test #'string=)))) 
    (values (second obj) "Command Fragment"
	    usedfrags
	    (list
	     (when usedfrags
	       (list 'remark 
		     (format nil "This fragment uses commands defined in ~
                    the fragment~:[~;s~] ~A~{, ~A~}." (cdr usedfrags)
		    (car usedfrags) (cdr usedfrags))))
	     (list 'misc
		   (cadr
		    (assoc :help (cddr obj) :test #'string=)))))))

(defmethod doc~parse-def ((type (eql 'com~defcommand)) obj)
  (let ((help (cadr (assoc :help (cddr obj)
		      :test #'string=)))
	(argtypes (cdr (assoc :argtypes (cddr obj)
		      :test #'string=)))
	(argnames (cdr (assoc :argnames (cddr obj)
		      :test #'string=)))
	(arghelps (cdr (assoc :arghelps (cddr obj)
		      :test #'string=)))
	(defaults (cdr (assoc :defaults (cddr obj)
			      :test #'string=)))
	(frag-cats (cdr (assoc :frag-cats (cddr obj)
			       :test #'string=))))
    (values (second obj) "Command" argnames
	  (list
	   (when argnames
	     (list 'input
                   (concatenate 'string
                                "\\begin{tabular}[t]{l@{ of type }l@{ is }l}"
				(apply #'concatenate
				       (cons 'string
					     (mapcar #'(lambda (x y z)
							 (format nil "~A \\taband ~A \\taband \\parbox[t]{5cm}{~A}\\\\~%" x y (if z z "not documented.")))
						     argnames
						     argtypes
						     arghelps)))
				"\\end{tabular}")))
	   #+old(when argnames
	     (let ((multiple-args (cdr argnames)))
	     (list 'arguments
		   (format nil "This command takes the argument~:[~;s~]: ~
                      ~A~{, ~A~} with respective argument type~:[~;s~] ~A~{, ~A~}. ~
                      The arguments have the following meanings:
                      ~%\\begin{itemize}~%~:{\\item[~A] ~A~%~}\\end{itemize}~%"
			   multiple-args (car argnames) (cdr argnames)
			   multiple-args (car argtypes) (cdr argtypes)
			   (mapcar #'list argnames 
				   (substitute "No documentation" nil arghelps))
			   ))))
	   #+old(list 'example
		 (format nil "\\begin{tabular}{r~{~*c~}}~%~
                    PROMPT: ~A ~{ & ~A ~} \\\\~%~{ & ~A ~}~:[~; \\\\~%~]\\end{tabular}~%"
			  argnames (second obj) argnames argtypes argtypes))
	   (list 'remark 
		   (format nil "This command belongs to the fragment: ~A~%~
                    and the categories ~A~{, ~A~}.~%~
                   ~:[~;Argument defaults are ~A.~]"
		    (first frag-cats) (car frag-cats) (cdr frag-cats) defaults defaults))
	   (list 'misc help)))))




(pp~modify-style doc+latex-interface
		 ((cons (member com~defcategory com~deffragment com~defcommand))
		  doc~latex-lispdef
		  15))

(pp~modify-style doc+latex-complete
		 ((cons (member com~defcategory com~deffragment com~defcommand))
		  doc~latex-lispdef
		  15))

(pp~modify-style doc+latex-documentation
		 ((cons (member com~defcategory com~deffragment com~defcommand))
		  doc~latex-lispdef
		  15))
)

(defun com=extract-doc-pathname (out inpath)
  (declare 
   (authors nesmith)
   (input "An output stream OUT, and a pathname INPATH")
   (value "NIL")
   (effect #{Writes the \LaTeX\ declarations for printing a manual to OUT, then
pretty-prints INPATH in the parent style to OUT (it is assumed that the
parent style knows how to extract the documentation from INPATH), then finishes
the output with an \\end\{document\}. #}))
  (man~write-tex-declarations out)
  (man~write-input-omega-macros out)
  (format out "\\begin{document}~%")
  (man~write-tex-special-hyphenations out)
  (man~write-tex-title out
		       (format nil "The ~A MODULE" 
			       (string-upcase (pathname-name inpath))))
  (pp~pprint-with-parent-style inpath nil out)
  (format out "\\end{document}~%"))

(pp~defstyle com+latex-doc :parent doc+doc
             :help "The style for \LaTeX\ output of command-structures."
             :pprint-methods
             (((cons (member com~defcategory com~deffragment com~defcommand))
	       doc~latex-lispdef
	       5)
	     ))

(pp~defstyle com+latex-man :parent com+latex-doc
	     :help "Same style as COM+LATEX-DOC, except that stand-alone \LaTeX\ files will be created."
	     :pprint-methods
	     ((pathname com=extract-doc-pathname 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make-Load-Form Methods   ---  by VS  sorge@ags.uni-sb.de
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following stuff could be compiler dependent...

#|
(defmethod make-load-form ((obj com+fragment) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots obj :slots '(keim::name keim::help keim::commands keim::uses-comms-of keim::categories)))

(defmethod make-load-form ((obj com+command) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots obj :slots '(keim::name keim::help keim::argnames keim::argtypes keim::arghelps
						       keim::function keim::fragment keim::categories
						       keim::default-fn keim::log-p))
  (print "hallo"))

(defmethod make-load-form ((obj com+category) &optional env)
  (declare (ignore env))
  (print (keim~name obj))
  (setf (gethash (symbol-name (keim~name obj)) keim::com*category-hash-table) obj)
  (make-load-form-saving-slots obj :slots '(keim::name keim::help keim::commands))
)|#
