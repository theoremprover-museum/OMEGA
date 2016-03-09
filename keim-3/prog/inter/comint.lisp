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

(mod~defmod comint :uses (mod sys inter com asi arg socket)
	    :documentation "Definition of an extended command interpreter."
	    :exports (
		      comint*current-comint
		      comint+leave-comint

		      comint~apply-command2arguments
		      comint~top
		      comint~create
		      comint~commands
		      comint~interface
		      comint~fragments
		      comint~logfile
		      comint~last-comm
		      comint~debugger-on-break
		      comint~find-command
		      
		      ))

(defvar comint*current-comint nil)

#{\section{Command Interpreters}

This module implements data structures and functionality for simple command interpreters.
These are basically read-eval loops for the \keim\ commands that provide a given interface
with algorithms that allow to query the user for commands (and their arguments) in a
controlled way. In particular, typing or type errors are caught and handled without falling
into the debugger.#}

;;; First we define a condition which will be signaled when we want to leave
;;; the current command interpreter, COMINT+LEAVE-COMINT, which inherits from SYS+ABORT.

(sys~define-condition comint+leave-comint (sys+abort)
  (comint-name)
  (lambda (condition stream)  
	  (format stream ";;;INFO: The command interpreter ~A is left."
		  (comint+leave-comint-comint-name condition)))
  (:documentation "Signals that we should leave the current command interpreter."))

#{All information of a command interpreter, such as local settings of switches or the set of
commands available to a command interpreter are stored in an object of the following class.#}

(defclass comint+comint (keim+name)
  ((fragments :initarg :fragments
	      :initform nil
	      :accessor comint~fragments
	      :documentation "A list of fragments that define the commands valid for this command interpreter.")
   (interface  :initarg :interface
	     :initform nil
	     :accessor comint~interface
	     :documentation "The interface of this command interpreter.")
  (logfile :initarg :logfile
	   :initform nil
	   :accessor comint~logfile
	   :documentation "If non-nil a log-file is kept.")
  (debugger-on-break :initform nil
		     :accessor comint~debugger-on-break
		     :documentation "If true, the command interpreter goes into the debugger on break.")
  (last-comm :initarg :last-comm
	   :initform nil
	   :accessor comint~last-comm
	   :documentation "The last command considered in the command interpreter (can not be HELP)."))
  (:documentation "The class of command interpreters"))
	     

#{In particular, command interpreters have a {\vb NAME} (which will be used as the
  of the command interpreter), an {\vb INTERFACE} to be used for communication and a
  set of commands, that are available. The following function runs the read-eval-print
  loop for an command interpreter.#}

(defun comint~commands (comint &optional (category))
  (declare (edited  "12-JUL-1995")
	   (authors Kohlhase)
	   (input   "A command interpreter and (optionally) a category that is relevant for it.")
	   (effect  "None.")
	   (value   "The list of commands of category {\vb CATEGORY} in {\vb COMINT}."))
  (let ((comint-commands (remove-duplicates
			  (mapcan #'com~commands (comint~fragments comint)))))
    (if category
	    (remove-if-not #'(lambda (com)
			       (member category (com~categories com)))
			   comint-commands)				   
	  comint-commands)))
(defmethod comint~find-command (comm-name (comint comint+comint))
  (declare (edited  "25-JUL-1995")
	   (authors Lassaad)
	   (input   "A string or a symbol and a comint")
	   (effect  "None")
	   (value   "The command with the given name or NIL if none exists in the corresponded fragments of comint."))
  (some #'(lambda (frag)
	    (com~find-command comm-name frag))
	(comint~fragments comint)))


#{Thus a specific command interpreter can be created using#}
(defun comint~create (name fragments interface &optional (logfile))
  (declare (edited  "03-NOV-1995" "05-JUL-1995")
	   (authors Lassaad Kohlhase)
	   (input   "A NAME, a list of FRAGMENTS, an INTERFACE and optional a log file"
		    "for the command interpreter.")
	   (effect  "None.")
	   (value   "The command interpreter."))
  (if logfile
      (make-instance 'comint+comint
		     :name name
		     :fragments fragments
		     :interface interface
		     :logfile logfile)
    (make-instance 'comint+comint
		     :name name
		     :fragments fragments
		     :interface interface)))

#{and then started with the function#}
      				   
	 
     
	

(defun comint~top (&optional (comint (comint~create "COMINT" (list (com~find-fragment 'comint)) (asi~create))))
  (declare 
   (authors kohlhase lassaad)
   (input "Optionally a command interpreter,"
	  "if not supplied one is created with interface ASI~CREATE and name COMINT by default.")
   (value "Undefined")
   (effect "After initialization, runs a read-eval-print loop,"
	   "reading a command, getting its arguments and applying the command."))
  (let* ((interface (comint~interface comint))
	 (name (keim~name comint))
	 (comint*current-comint comint))
    (with-simple-restart
     (abort "Exit ~A top level." name)
     (loop
      (with-simple-restart
       (abort "Return to ~A top level." name)
       (if (comint~debugger-on-break comint)
	   (sys~handler-case
	    (comint=main-loop comint)
	    (comint+leave-comint (c) 
				 (inter~output-object interface c)
				 (return-from comint~top nil)))
	 (sys~handler-case
	  (comint=main-loop comint)
	  (comint+leave-comint (c)
			       (inter~output-object interface c)
			       (return-from comint~top nil))
	  (com+abort-command (c) (inter~print-warning interface c))
	  (break (c) 
		 (inter~print-warning 
		  interface c))
	  (simple-error (c)
			(inter~print-error
			 interface c))
	  (error (c) (inter~terpri interface)
		     (inter~print-error interface c)
		     (inter~terpri interface))
	  (sys+error (c) (inter~terpri interface)
		     (inter~print-error interface c)
		     (inter~terpri interface)))
	 ))))))

(defun comint=loop-for-arg (name type help default comint)
  (declare (edited  "07-NOV-1995" "05-JUL-1995")
	   (authors Lassaad Kohlhase)
	   (input   "The specification of a desired argument of command,"
		    "specified  by its NAME, TYPE, HELP-string and the Default,"
		    "furthermore a command interpreter COMINT." )
	   (effect "Prints a dialog to the the interface of COMINT that determines the argument.")
	   (value  "The value of the argument and some informations specifying whether the Default"
		   "or another object is returned."
		   "("default". Default) or ((object-name). object)."))
  (let* ((interface (comint~interface comint))
	 (comint-name (keim~name comint))
	 (comint*current-comint comint)
	 (prompt (if name (format nil "~A (~A) ~A: " name type help)
		   (format nil "~A: " comint-name))))
    (loop
     (if (comint~debugger-on-break comint)
	 (progn
	   (inter~terpri interface)
	   ;; returns out of loop only if normal exit of prompt
	   ;; occurs. any error puts us in the handler below
	   (return-from 
	       comint=loop-for-arg
	     (if (com~specified-arg-p default)
		 (inter~prompt-for-input-with-default-info interface prompt type default)
                                                                       ;;; ((arg). value) oder
	                                                               ;;; ("DEFAULT". default-value)
	       (inter~prompt-for-input-info interface prompt type))))  ;;; ((arg). value)
       (sys~handler-case
	(progn
	  (inter~terpri interface)
	  ;; returns out of loop only if normal exit of prompt occurs. any error puts us in the
	  ;; handler below
	  (return-from 
	      comint=loop-for-arg
	    (if (com~specified-arg-p default)
		(inter~prompt-for-input-with-default-info interface prompt type 
						          default)
	      (inter~prompt-for-input-info interface prompt type))))
	(arg+input-error (c) (inter~print-error interface c)
			 (inter~terpri interface)))))))


(defun comint=loop-for-args (names types helps command input-args comint)
  (declare (edited  "08-DEC-1999" "07-NOV-1995" "05-JUL-1995")
	   (authors Sorge Lassaad Kohlhase)
	   (input  "The specification of the list of argument neede for command,"
		    "specified  by name NAME, TYPE, HELP-string and the Default,"
		    "furthermore a list of INPUT-ARGS already given with the command,"
		    "finally a command interpreter COMINT." )
	   (effect "Checks INPUT-ARGS for their well-typedness and"
		   "prints a dialog to the the interface of COMINT that determines the remaining arguments.")
	   (value  "The list of arguments determined with the corresponding informations."))
  (let* ((real-args&info nil)
	 (interface (comint~interface comint))
	 (comint*current-comint comint))
    (setq real-args&info
	  (mapcar #'(lambda (name type help arg)
		      (if (com~specified-arg-p arg)
			   (if (comint~debugger-on-break comint) 
			     (sys~handler-case 
			       ;;;(arg~read-type type arg) ;;; output ((arg). value)
			      (cons (list arg)
				    (arg~read-type type arg))
			      (arg+input-error (c)
					       (invoke-debugger c)))
			     (sys~handler-case
			     ;;;(arg~read-type type arg) ;;; output ((arg). value)
			      (cons (list arg)
				    (arg~read-type type arg))
			      (arg+input-error (c) (inter~print-error 
						    interface c)
					       (inter~terpri interface)
					       (comint=loop-for-arg name type help (com~unspecified) comint))))
			arg))
		  names types helps input-args))
    (loop
     (let* ((real-args (mapcar #'(lambda (arg&info) (if (com~specified-arg-p arg&info)
							(rest arg&info)
						      arg&info))
			       real-args&info))
	    (poss-args (com~apply-defaults command real-args))
	    (n (or (position-if-not #'com~specified-arg-p real-args)
		   (mismatch poss-args real-args
			     :test-not 
			     #'(lambda (x y) (and (com~specified-arg-p x)
						  (not (com~specified-arg-p y))))))))
       (if n
	   (setf (nth n real-args&info)
		 (comint=loop-for-arg (nth n names) (nth n types) (nth n helps) (nth n poss-args) comint))
	 (return-from comint=loop-for-args real-args&info))))))


(defun comint=main-loop (comint)
  (declare (edited  "08-DEC-1999" "05-JUL-1995" "30-JUL-1998")
	   (authors Sorge Kohlhase Hess)
	   (input   "A command interpreter.")
	   (effect  "A read-eval loop using the commands of COMINT is conduted with the interface of COMINT.")
	   (value   "Undefined."))
  (let* ((interface (comint~interface comint))
	 (comint-name (keim~name comint))
	 (comint*current-comint comint)
	 (input-list (inter~prompt-for-command interface (format nil "~A: " comint-name))))
    (when input-list
      (let* ((comm (sys~handler-case
			   (arg~read-type 'command (car input-list))
			   (arg+input-error (c) (inter~print-error interface c)
					    (inter~terpri interface)
					    (return-from comint=main-loop))))
		    (comm-name (keim~name comm))
		    (argnames (com~argnames comm))
		    (argtypes (com~argtypes comm))
		    (arghelps (com~arghelps comm))
		    (input-args 
		     (append (if (cdr input-list)
				 (subseq (cdr input-list) 0 
					 (min (length argtypes)
					      (1- (length input-list))))
			       nil)
			     (make-list (max 0 
					     (- (length argtypes) 
						(length (cdr input-list))))
					:initial-element (com~unspecified))))
		    (real-args&info nil)
		    (real-args nil))
	       (unless (string= comm-name "HELP")
		 (setf (comint~last-comm comint*current-comint) comm))
	       (setq real-args&info
		     (comint=loop-for-args argnames argtypes arghelps comm input-args comint))
	       (setq real-args (mapcar #'(lambda (arg-info) (rest arg-info))
				       real-args&info))
	       (comint~apply-command2arguments comm real-args)
	       (let ((logfile (comint~logfile comint)))
		 ;;Only commands whose log-p slot is T are written in the log file 
		 (when (and logfile (com~log-p comm))
		   (let ((logstream (open logfile
					  :direction :output
					  :if-exists :append))
			 (args-info (mapcar #'(lambda (arg-info) (first arg-info))
					    real-args&info)))
		     (format logstream
			     "~A ~A~{ ~A~}~%"
			     (keim~name (com~fragment comm)) 
			     comm-name
			     args-info)
		     (close logstream))))
	       ))))

(defun comint~commands (comint &optional (category))
  (declare (edited  "12-JUL-1995")
	   (authors Kohlhase)
	   (input   "A command interpreter and (optionally) a category that is relevant for it.")
	   (effect  "None.")
	   (value   "The list of commands of category {\vb CATEGORY} in {\vb COMINT}."))
  (let ((comint-commands (remove-duplicates
			  (mapcan #'com~commands (comint~fragments comint)))))
    (if category
	    (remove-if-not #'(lambda (com)
			       (member category (com~categories com)))
			   comint-commands)				   
	  comint-commands)))

(defgeneric comint~apply-command2arguments (command arguments)
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "A command and a list of arguments.")
	   (effect  "Applys the command to arguments.")
	   (value   "Undefined."))
  (:method ((command com+command) (arguments list))
	   (let ((fun (com~function command)))
	     (apply fun arguments))))
	

