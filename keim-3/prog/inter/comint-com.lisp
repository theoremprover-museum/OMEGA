;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1994 by AG Siekmann, Fachbereich Informatik,             ;;
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


(mod~defmod ccom :uses (mod sys com  inter keim help  asi arg)
	    :documentation "Definition of the basic commands."
	    :exports ( ccom~real-args-of
		       ccom~apply-command
		      ))

#{This module defines the basic commands for the commands {\vb comint} interpreters,
  and furthermore a command interpreter for help processing.

We start out by defining the basic fragments and categories
\begin{tabular}{ll}
{\vb exit} & for exiting the interpreter \\
{\vb help} & for help commands \\
{\vb backtracking} & for undoing and restoring old states of the interpreter \\
{\vb comint-state} & for changing the internal state of the interpreter \\
{\vb log-commands} & consists of commands for changing the logging-out mode \\
{\vb lisp} & for lisp-related commands like calling an inferior lisp, loading files or systems\ldots \\
{\vb change-comint} & for evoking a new command interpreter or leaving the current one \\
{\vb comint} & that includes all basic comint commands \\
{\vb file-io} & for commands that handle input and output to files \\
{\vb to-be-implemented} & for commands that are specified, but whose functionality is not yet implemented
\end{tabular}
Note that commands  can belong to more than one category.#}

(eval-when (load compile eval)
(com~deffragment exit
  (uses-comms-of)
  (help "Commands for exiting the command interpreter"))

(com~deffragment help
  (uses-comms-of exit)
  (help "Help commands"))

(com~deffragment backtracking
  (uses-comms-of)
  (help "Backtracking commands"))

(com~defcategory comint-state
  (help "Commands for manipulating the state of the command interpreter"))

(com~defcategory log-commands
  (help "Commands that are to be written in the log file, when the log mode is set"))

(com~defcategory lisp
  (help "Lisp-related commands"))

(com~defcategory change-comint 
  (help "Commands that evoke a new command interpreter or leave the current one."))

(com~deffragment comint
 (uses-comms-of exit backtracking)
 (help "Basic comint commands")) 

(com~defcategory file-io
  (help "Commands that concern input and output on files"))

#|
(com~defcategory to-be-implemented
  (help "These commands are not implemented yet, please help!!"))
|#
)


(com~defcommand exit-lisp
  (function sys~exit-from-lisp)
  (help "Exit from the Lisp completely.")
  (frag-cats exit lisp))


(com~defcommand exit
  (function ccom=leave)
  (argnames )
  (argtypes )
  (frag-cats exit change-comint)
  (help "Leave the current command interpreter top level."))

(defun ccom=leave ()
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))



;;; The command BREAK just goes into the debugger.

(com~defcommand break
  (function ccom=break)
  (argnames )
  (argtypes )
  (frag-cats exit change-comint)
  (help "Go directly into the debugger."))

#|(defun ccom=break ()
  (invoke-debugger (make-condition 'simple-break))) |#
(defun ccom=break ()
  (invoke-debugger (make-condition 'break)))
 
 
				       
(com~defcommand debug-mode
  (function ccom=debug-mode)
  (argnames debug-p)
  (argtypes boolean)
  (arghelps "T to go into debugger on errors, else nil")
  (frag-cats comint comint-state)
  (defaults ((ccom=debug-mode-default)))
  (help "If argument is T, then go into debugger on errors."))

(defun ccom=debug-mode-default ()
  (not (comint~debugger-on-break comint*current-comint)))

(defun ccom=debug-mode (bool)
  (setf (comint~debugger-on-break comint*current-comint) bool))

(com~defcommand verbose-mode
  (function ccom=verbose-mode)
  (argnames verbose-p)
  (argtypes boolean)
  (arghelps "T: turn on verbose mode, else nil")
  (frag-cats comint comint-state)
  (defaults ((ccom=verbose-mode-default)))
  (help "If argument is T, then turn on verbose mode for rule warnings."))

(defun ccom=verbose-mode-default ()
  (not rule*verbose))

(defun ccom=verbose-mode (bool)
  (setq rule*verbose bool))

;;; The command LOAD-SYS loads a system into omega

(com~defcommand load-sys
		(function ccom=load-sys)
		(argnames system force)
		(argtypes existing-system defsystem-force)
		(arghelps "Name of the system to be loaded"
			  "Which parts should be loaded"
			  "(:all :new-source, :new-source-and-dependents or a list of modules")
		(defaults ((com~unspecified) :source-newer-and-dependents))
		(frag-cats comint lisp file-io)
		(log-p T)
		(help "Load a system into the current command interpreter."))

(defun ccom=load-sys (system force)
  (mk::operate-on-system system :load :force force))

;;; The command LOAD-FILE loads a file into omega and assumes it to be a lisp file

(com~defcommand load-file
  (function ccom=load-file)
  (argnames file)
  (argtypes existing-file)
  (arghelps "Name of the file to be loaded") 
  (frag-cats comint lisp file-io)
  (log-p T)
  (help "Load a lisp file into the current command interpreter."))

(defun ccom=load-file (file)
  (load file))


;;; The command LISP gives you a new Lisp read-eval-print loop. 

(com~defcommand lisp
  (function ccom=lisp)
  (argnames )
  (argtypes )
  (frag-cats comint lisp change-comint)
  (help "Gives a lisp top level.  Exit by entering :escape."))

    
(defun ccom=lisp ()
  (let ((tag (gensym)))
    (catch tag
      (flet ((escape () (throw tag t)))
	(let (* ** *** + ++ +++ / // /// -)
	  (format t "~%Enter :escape to return to ~A top level.~%" (keim~name comint*current-comint))
	  (with-simple-restart (abort "Leave Lisp sublevel")
	    (loop
	     (with-simple-restart (abort "Return to Lisp sublevel")
	       (sys~handler-case 
	       (let ((form (progn (inter~fresh-line (comint~interface comint*current-comint)) 
				  (inter~prompt-for-input
				   (comint~interface comint*current-comint)
				   "Lisp: " 'anything)))
		     (vals nil))
		 (inter~fresh-line (comint~interface comint*current-comint))
		 (setq - form)

		     (setq vals (multiple-value-list (eval form)))
		 (psetq +++ ++ ++ + *** ** ** * /// // // /)
		 (setq / vals * (car vals) + form)
		 (if (eq * :escape) (escape))
		 (dolist (val vals)
		   (inter~output-object (comint~interface comint*current-comint) val)
		   (inter~terpri (comint~interface comint*current-comint))))
	       (error (c) (invoke-debugger c))
	       (simple-error (c) (invoke-debugger c))
	       (serious-condition (c) (invoke-debugger c))
	       (warning (c) 
		 (inter~print-warning (comint~interface comint*current-comint) c)))))))))))


#|
(defvar comint*help-on-comint nil "The command interpreter, the help interpreter speaks about")

(com~defcommand help
  (function ccom=help)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats comint change-comint help)
  (defaults)
  (help "Enters the command interpreter for help."))

(defun ccom=help ()
  (let ((comint*help-on-comint comint*current-comint))
    (ccom=help-commands)
    (comint~top (comint~create (format nil "HELP on ~A" (keim~name comint*help-on-comint))
			       (list (com~find-fragment 'help))
			       (comint~interface comint*current-comint)))))

(defun ccom=help-commands ()
  (declare (edited  "17-OCT-1995")
	   (authors Lassaad)
	   (input   "None")
	   (effect  "Prints all commands belonging to the HELP fragment"
		    "to the current interface.")
	   (value   "Undefined."))
  (pp~pprint (com~find-fragment 'help)
	     'ccom=help-comint-print
	     (asi~output (comint~interface comint*current-comint))))

|#

(pp~defstyle 
 ccom=help-comint-print
 :help "Style used for printing list of commands of a fragment/category"
 :pprint-methods
  ((com+fragment (lambda (s l) (let ((*standard-output* s)) (ccom=pprint-fragment l))))
   (com+category (lambda (s l) (let ((*standard-output* s)) (ccom=pprint-category l))))
   (com+command  (lambda (s l) (let ((*standard-output* s)) (ccom=pprint-command l))))
   (string (lambda (s l) (write-string l s)))
))

(defun ccom=pprint-fragment (fragment)
  (let ((commands nil))
    (maphash #'(lambda (key com) (push com commands))
	     (com~frag-direct-commands fragment))
    (pprint-logical-block (nil nil)
			  (write (help~help-string fragment))
			  (pprint-newline :mandatory)
			  (pprint-logical-block (nil commands)
						(loop (pprint-exit-if-list-exhausted)
						      (pprint-tab :section 4 1)
						      (write (pprint-pop))
						      (pprint-newline :mandatory))))))

(defun ccom=pprint-category (category)
  (let ((commands (comint~commands comint*help-on-comint category)))
    (pprint-logical-block (nil nil)
			  (write (help~help-string category))
			  (pprint-newline :mandatory)
			  (pprint-logical-block (nil commands)
						(loop (pprint-exit-if-list-exhausted)
						      (pprint-tab :section 4 1)
						      (write (pprint-pop))
						      (pprint-newline :mandatory))))))
			  

(defun ccom=pprint-command (command)
  (flet ((get-words (string)
		    (let ((newstring (substitute #\newline #\space 
						 string :test #'char=)))
		      (with-input-from-string (in newstring)
					      (do ((res nil)
						   (word (read-line in nil :eof) (read-line in nil :eof)))
						  ((eq word :eof) (nreverse res))
						(push word res))))))
    (let ((help-string (help~help-string command)))
      (pprint-logical-block (nil nil)
			    (write-string (string (keim~name command)))
			    (write-char #\:)
			    (write-char #\space)
			    (pprint-indent :current 0)
			    (pprint-logical-block (nil (get-words help-string))
						  (loop (pprint-exit-if-list-exhausted)
							(write (pprint-pop))
							(write-char #\space)
							(pprint-newline :fill)))))))

(com~defcommand start-log
  (function ccom=start-log)
  (argnames log-file)
  (argtypes pathname)
  (arghelps "The pathname of the log-file.")
  (frag-cats comint comint-state file-io log-commands)
  (defaults ((ccom=logfile-default)))
  (help "Further commands are recorded in the given file."))

(defun ccom=start-log (path)
  (let ((log-file (comint~logfile comint*current-comint)))
    (if log-file
	(let ((yes-no (inter~prompt-for-input
		       (comint~interface comint*current-comint)
		       (format nil "Current log file: ~A ~%Changing it? (y/n)" log-file)
		       (arg~find-argtype 'boolean))))
	  (when yes-no
	    (let ((logstream (open path
			           :direction :output 
			           :if-does-not-exist :create
			           :if-exists :rename)))
	      (if logstream
		  (progn (close logstream)
			 (setf (comint~logfile comint*current-comint) path))
		(error "File ~A cannot be opened for log output" path)))))
      (let ((logstream (open path
			     :direction :output 
			     :if-does-not-exist :create
			     :if-exists :rename)))
	(if logstream
	    (progn (close logstream)
		   (setf (comint~logfile comint*current-comint) path))
	  (error "File ~A cannot be opened for log output" path))))))

(defun ccom=logfile-default ()
  (format nil "~~/~A.rpy" (keim~name comint*current-comint)))

(com~defcommand stop-log
  (function ccom=stop-log) 
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats comint comint-state file-io log-commands)
  (defaults)
  (help "Stops logging the commands."))

(defun ccom=stop-log ()
  (when (comint~logfile comint*current-comint)
    (setf (comint~logfile comint*current-comint) nil)))

(com~defcommand log-mode
  (function ccom=log-mode)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats comint comint-state log-commands)
  (defaults)
  (help "Asking for the actual log mode."))

(defun ccom=log-mode ()
  (let ((interface (comint~interface comint*current-comint))
	(logfile (comint~logfile comint*current-comint)))
    (cond (logfile (inter~output-object interface "The log mode is set.")
		   (inter~terpri interface)
		   (inter~output-object interface "The actual log file: ")
		   (inter~output-object interface logfile))
	  (t (inter~output-object interface "The log mode is not set.")))))
	       

(com~defcommand execute-log
  (function ccom=execute-log)
  (argnames log-file)
  (argtypes pathname)
  (arghelps "The pathname of the log-file.")
  (frag-cats comint file-io log-commands)
  (defaults ((ccom=logfile-default))) 
  (help "Applies the commands specified in a log-file."))

(defun ccom=execute-log (path)
  (declare (edited  "26-JUL-1995")
	   (authors Lassaad)
	   (input  "A file name" ) 
	   (effect "Reads and applyies the commands from the log file PATH" )
	   (value  "None" ))
  (let ((current (sys~current-directory)))
    (sys~handler-case
     (with-open-file (in path :direction :input
			 :if-does-not-exist :error)
		     (sys~chdir (directory-namestring path))
		     (loop (let ((input-list
				  (sys~handler-case
				   (asi~linereadp in)
				   (asi+no-input-error () nil)))
				 (next-input (peek-char t in nil 'eof)))
			     (when input-list (ccom~apply-command input-list))
			     (cond ((equal next-input #\;) (read-line in))
				   ((equal next-input 'eof) (return-from ccom=execute-log)))))
		     (sys~chdir current))
     (file-error (c)
		 (sys~chdir current)
		 (inter~print-error (comint~interface comint*current-comint) c))
     (error (c)
	    (sys~chdir current)
	    (error c)))))


(defun ccom~apply-command (input-list)
  (declare (edited  "26-JUL-1995")
	   (authors Lassaad)
	   (input   "A list: its first element is a fragment, its second element a "
		    "command name and the rest contains specifiers for the arguments"
		    "of the command.")
	   (effect  "Applyies the command with the specified arguments.")
	   (value   "None"))
  (when input-list
    (let* ((frag (com~find-fragment (first input-list)))
	   (comm (com~find-command (second input-list) frag))
	   (args-info (rest (rest input-list)))
	   (real-args (ccom~real-args-of args-info comm))
	   (fun (com~function comm)))    
      (apply (symbol-function fun) real-args))))

(defun ccom~real-args-of (args-info command)
  (declare (edited  "10-NOV-1995")
	   (authors Lassaad)
	   (input  "ARGS-INFO a list of argument informations, each of them is a list"
		   "containing an argument name for not default argument or the key"
		   "word DEFAULT for a default argument, and a COMMAND.")
	   (effect "none" )
	   (value  "list of the objects to be used as arduments for COMMAND." ))
  (let ((real-args nil)
	(argtypes (com~argtypes command)))
    (setq real-args (mapcar #'(lambda (type arg-info)
				(if (listp arg-info)
				    (arg~read-type type (first arg-info))
				  (com~unspecified)))
			    argtypes args-info))

    (loop
     (let* ((poss-args (com~apply-defaults command real-args))
	    (n (or (position-if-not #'com~specified-arg-p real-args)
		   (mismatch poss-args real-args
			     :test-not 
			     #'(lambda (x y) (and (com~specified-arg-p x)
						  (not (com~specified-arg-p y))))))))
       (if n
	   (setf (nth n real-args)
		 (nth n poss-args))
	 (return-from ccom~real-args-of
	   (mapcar #'(lambda (type argument)  ;MP: just to get an argument check on defaults 
		       (funcall                    
			(arg~read-function (arg~find-argtype type))
			(inter~get-symbol argument)))
		   argtypes real-args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OLD Stuff....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; help commands for comint

#|
(com~defcommand command
  (function ccom=help-command)
  (argnames command)
  (argtypes command-to-help)
  (arghelps "Command for which help is desired")
  (frag-cats help)
  (defaults ((ccom=command-default)))
  (help "Show help for a command."))

(defun ccom=command-default ()
  (let ((last-comm (comint~last-comm comint*help-on-comint)))
    (if last-comm
	last-comm
      (com~unspecified))))
      
(defun ccom=help-command (command)
  (declare (edited  "12-JUL-1995") 
	   (authors Kohlhase)
	   (input   "A command.")
	   (effect  "The command is described to the current interface.")
	   (value   "Undefined."))
  (let* ((argnames (com~argnames command))
	 (argtypes (com~argtypes command))
	 (arghelps (com~arghelps command))
	 (help (com~help command))
	 (line1* nil)
	 (line2* nil)
	 (argnamehelppairs (mapcar #'(lambda (x y) (list x y)) 
				   argnames arghelps))
	 (fragment (keim~name (com~fragment command)))
	 (categories (mapcar #'keim~name (com~categories command)))
	 (str nil))
    (do* ((line1 (mapcar #'symbol-name argnames) (cdr line1))
	  (line2 (mapcar #'symbol-name argtypes) (cdr line2))
	  (line1-lengths (mapcar #'length line1) (cdr line1-lengths))
	  (line2-lengths (mapcar #'length line2) (cdr line2-lengths))
	  (tmp (format nil "~A: ~A" (keim~name comint*help-on-comint) (keim~name command)))
	  (ltmp (length tmp)))
	((null line1) (setq line1* (cons tmp (nreverse line1*)))
	 (setq line2* (cons (make-string ltmp :initial-element #\space)
			    (nreverse line2*))))
      (let ((l1 (car line1))(l1l (car line1-lengths))
	    (l2 (car line2))(l2l (car line2-lengths)))
	(if (> l1l l2l)
	    (progn
	      (let* ((diff (- l1l l2l)) (diff2 (floor diff 2)))
		(push l1 line1*)
		(push (format nil "~A~A~A" 
			      (make-string diff2 
					   :initial-element
					   #\space)
			      l2
			      (make-string (- diff diff2)
					   :initial-element
					   #\space))
		      line2*)))
	  (progn
	    (let* ((diff (- l2l l1l)) (diff2 (floor diff 2)))
	      (push l2 line2*)
	      (push (format nil "~A~A~A" 
			    (make-string diff2 
					 :initial-element
					 #\space)
			    l1
			    (make-string (- diff diff2)
					 :initial-element
					 #\space))
		    line1*))))))
    ;; a cheapo way to do it, not very good 
    (setq str
	  (format nil "~%~A is a command.~%~
                       ~A~%~%~%The command format for ~A is:~%~%~
                       ~{~A  ~}~%~{~A  ~}~%~
                       ~@[The arguments have the following meanings:~%~
                       ~:{~A : ~A~%~}~%~]~%~
                       It is defined in the fragment ~A~%and member of the categories~%~{~A ~}~%~%~%"
		  (keim~name command) help (keim~name command) 
		  line1* line2* argnamehelppairs
		  fragment categories))
    (inter~output-object (comint~interface comint*current-comint) str)
    (when (rule~find-rule (keim~name command))
      (inter~output-object 
       (comint~interface comint*current-comint)
       (with-output-to-string (*standard-output*)
			      (rule~describe (keim~name command))))
      (inter~terpri (comint~interface comint*current-comint)))
    ))

(com~defcommand help-commands
  (function ccom=help-commands)
  (argnames)
  (argtypes)
  (argtypes)
  (frag-cats help)
  (help "Show this info."))

(com~defcommand categories
  (function ccom=categories)
  (argnames)
  (argtypes)
  (argtypes)
  (frag-cats help)
  (help "Show all categories relevant to the command interpreter calling help."))

(defun ccom=categories ()
  (declare (edited  "19-OCT-1995")
	   (authors Lassaad)
	   (input   "None.")
	   (effect  "Prints all categories relevant to the current help on command"
		    "interpreter to the current interface.")
	   (value   "Undefined."))
  (pp~pprint comint*help-on-comint
	     'ccom=comint-categories-print
	     (asi~output (comint~interface comint*current-comint))))

(pp~defstyle 
 ccom=comint-categories-print
 :help "Style used for printing list of categories of a command interpreter"
 :pprint-methods
  ((comint+comint (lambda (s l) (let ((*standard-output* s)) (ccom=pprint-comint-categories l))))
   (com+category  (lambda (s l) (let ((*standard-output* s)) (ccom=pprint-only-category l))))
   (string (lambda (s l) (write-string l s)))
))


(defun ccom=pprint-comint-categories (comint) 
  (let ((categories (remove-duplicates (mapcan #'com~frag-all-categories
					       (comint~fragments comint)))))
    (pprint-logical-block (nil nil)
			  (write (format nil
					 "Categories in ~A:"
					 (keim~name comint)))
			  (pprint-newline :mandatory)
			  (pprint-logical-block (nil categories)
						(loop (pprint-exit-if-list-exhausted)
						      (pprint-tab :section 4 1)
						      (write (pprint-pop))
						      (pprint-newline :mandatory))))))
			  

(defun ccom=pprint-only-category (category)
  (flet ((get-words (string)
		    (let ((newstring (substitute #\newline #\space 
						 string :test #'char=)))
		      (with-input-from-string (in newstring)
					      (do ((res nil)
						   (word (read-line in nil :eof) (read-line in nil :eof)))
						  ((eq word :eof) (nreverse res))
						(push word res))))))
    (let ((help-string (help~help-string category)))
      (pprint-logical-block (nil nil)
			    (write-string (string (keim~name category)))
			    (write-char #\:)
			    (write-char #\space)
			    (pprint-indent :current 0)
			    (pprint-logical-block (nil (get-words help-string))
						  (loop (pprint-exit-if-list-exhausted)
							(write (pprint-pop))
							(write-char #\space)
							(pprint-newline :fill)))))))


(com~defcommand commands
  (function ccom=commands)
  (argnames cat-name)
  (argtypes existing-category-name)
  (arghelps "Symbol name of a category")
  (frag-cats help)
  (defaults ((ccom=default-category-name)))
  (help "Show all commands of the category that belongs to the help on command interpreter."))

(defun ccom=default-category-name ()
  (declare (edited  "20-OCT-1995")
	   (authors Lassaad)
	   (input   "None")
	   (effect  "None")
	   (value   "The category having the same name as the actual help on command interpreter."))
  (let ((comint-name (keim~name comint*help-on-comint)))
    comint-name))
  

(defun ccom=commands (cat-name)
  (declare (edited  "18-OCT-1995")
	   (authors Lassaad)
	   (input   "A category.")
	   (effect  "Prints all commands in CATEGORY to the current interface."
		    "The current interface must be an instance of ASI+INTER.")
	   (value   "Undefined."))
  (pp~pprint (com~find-category cat-name)
	     'ccom=help-comint-print
	     (asi~output (comint~interface comint*current-comint))))



(com~defcommand undo
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats backtracking to-be-implemented)
  (function ccom=undo)
  (help "Undo the last command."))


(defun ccom=undo ()
  (inter~print-warning (comint~interface comint*current-comint) "UNDO not implemented.")
)

(com~defcommand set-backtrack-label
  (argnames label)
  (argtypes symbol)
  (arghelps "Label to use")
  (frag-cats backtracking to-be-implemented)
  (function ccom=set-backtrack-label)
  (help "Establish LABEL to mark state for later backtracking."))


(defun ccom=set-backtrack-label ()
  (inter~print-warning (comint~interface comint*current-comint) "SET-BACKTRACK-LABEL not implemented.")
)

(com~defcommand backtrack
  (argnames label)
  (argtypes symbol) 
  (arghelps "Label to return to")
  (frag-cats backtracking to-be-implemented)
  (function ccom=backtrack)
  (help "Discard all changes to current proof since LABEL state was
established."))


(defun ccom=backtrack ()
  (inter~print-warning (comint~interface comint*current-comint) "BACKTRACK not implemented.")
)
|#




















