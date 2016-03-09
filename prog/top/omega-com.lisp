;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

(in-package :omega)
 
(mod~defmod oc :uses (mod sys com comint omega pdsn pds ot inter keim help
		      just node asi arg prob post env th heur rsrc agent)
	    :documentation "Definition of the basic OMEGA commands."
	    :exports (
		      oc~default-current-planline
                      oc=default-problem
		      oc~default-logfile
		      oc~create-file&evtl-rename-oldfile
                      oc~nil-argument
		      oc~falsity-p
		      oc~build-outline
		      ))

;;; This module defines the basic OMEGA commands.

;;; We start out by defining the basic fragments and categories

#{\section{Omega Command Fragments and Categories}#}

(eval-when (load compile eval)
(com~deffragment omega-basic
 (uses-comms-of)		
 (help "basic commands in OMEGA"))

(com~deffragment declaration
  (uses-comms-of)
  (help "Commands for declaration of new objects"))

(com~deffragment direct-display
  (uses-comms-of)
  (help "Displaying proof parts on the same interface"))

(com~deffragment mbase
  (uses-comms-of direct-display)
  (help "Commands for using and handling MBase"))

(com~deffragment omega
 (uses-comms-of omega-basic declaration direct-display)	 
 (help "OMEGA commands"))

(com~deffragment proof-view
  (uses-comms-of direct-display)
  (help "Commands for manipulation of the proof viewer"))

(com~deffragment atp
 (uses-comms-of direct-display)	 
 (help "ATP commands"))

(com~deffragment extern
 (uses-comms-of direct-display)	 
 (help "Commands for external systems"))

(com~deffragment leo
 (uses-comms-of)	 
 (help "Basic commands in LEO"))

(com~deffragment leo-interactive
 (uses-comms-of direct-display)	 
 (help "Interactive commands in LEO"))

(com~deffragment tps
 (uses-comms-of direct-display)	 
 (help "Commands for tps"))

(com~deffragment sapper
 (uses-comms-of direct-display)
 (help "Commands to invoke SAPPER"))

(com~deffragment mycas
 (uses-comms-of direct-display)
 (help "The basic myCAS commands"))

(com~deffragment no-gui
 (uses-comms-of )
 (help "Dummy fragment for the graphical user interface. Commands belonging to this fragment will not be included in the menu bar."))

(com~deffragment rules
 (uses-comms-of direct-display declaration)	 
 (help "RULES commands"))

(com~deffragment tactics
 (uses-comms-of direct-display declaration)	 
 (help "TACTICS commands"))

(com~deffragment presentation
 (uses-comms-of direct-display)	 
 (help "PRESENTATION commands"))

(com~deffragment proverb
 (uses-comms-of direct-display)	 
 (help "PROVERB commands"))

(com~deffragment verify
 (uses-comms-of direct-display)	 
 (help "Commands to invoke verifiers and checkers"))

(com~defcategory proof-manipulation
   (help "Commands for proof manipulation"))

(com~defcategory theory
   (help "Theory commands"))

(com~defcategory generic
   (help "All domain independent tactics."))

(com~deffragment analogy
                 (uses-comms-of direct-display)
                 (help "Commands for analogy."))

(com~deffragment examples
                 (uses-comms-of direct-display)
		 (help "Commands dealing with the examples and testbeds etc."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Categories for Nic Tactics and External Reasoners etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcategory false
		 (help "Commands dealing with the falsehood."))

(com~defcategory definition
		 (help "Commands dealing with expansion or contraction of definitions."))

(com~defcategory nic-special-tactic
		 (help "Commands dealing with special (additional) tactics for NIC, that
                        are not belonging to the core calculus."))

(com~defcategory dummy
		 (help "Commands dealing with dummy rules."))

(com~defcategory that
		 (help "Commands dealing with the description operator."))

)

#{\section{General Commands}#}

#|  Commented, since it doesn't work in multiprocessing mode anymore!!!

#{\subsection{Calling Subinterpreters}#}

;;; Changing to PROOF-VIEW command interpreter
(com~defcommand proof-view       
  (function oc=proof-view)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for PROOF VIEWer manipulation."))

(defun oc=proof-view ()
  (comint~top (comint~create "PROOF-VIEW"
			     (list (com~find-fragment 'comint) (com~find-fragment 'proof-view))
			     (comint~interface comint*current-comint))))


;;; Changing to MBASE command interpreter
(com~defcommand mbase       
  (function oc=mbase)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for the MBASE."))

(defun oc=mbase ()
  (comint~top (comint~create "MBASE"
			     (list (com~find-fragment 'comint) (com~find-fragment 'mbase))
			     (comint~interface comint*current-comint)
			     (comint~logfile comint*current-comint))))



;;; Changing into ATP command interpreter
(com~defcommand atp       
  (function oc=atp)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for ATP (OTTER, MKRP, ...)."))

(defun oc=atp ()
  (comint~top (comint~create "ATP"
			     (list (com~find-fragment 'comint) (com~find-fragment 'atp))
			     (comint~interface comint*current-comint)
			     (comint~logfile comint*current-comint))))



;;; Changing into RULES command interpreter
(com~defcommand rules       
  (function oc=rules)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for application of RULES."))

(defun oc=rules ()
  (comint~top (comint~create "RULES"
			     (list (com~find-fragment 'comint) (com~find-fragment 'rules))
			     (comint~interface comint*current-comint)
			     (comint~logfile comint*current-comint))))



;;; Changing to TACTICS command interpreter
(com~defcommand tactics       
  (function oc=tactics)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for application of TACTICS."))

(defun oc=tactics ()
  (comint~top (comint~create "TACTICS"
			     (list (com~find-fragment 'comint) (com~find-fragment 'tactics))
			     (comint~interface comint*current-comint)
			     (comint~logfile comint*current-comint))))



;;; Changing to PRESENTATION command interpreter
(com~defcommand presentation       
  (function oc=presentation)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint)
  (defaults)
  (help "Enters the command interpreter for PRESENTATION."))

(defun oc=presentation ()
  (comint~top (comint~create "PRESENTATION"
			     (list (com~find-fragment 'comint) (com~find-fragment 'presentation))
			     (comint~interface comint*current-comint)
			     (comint~logfile comint*current-comint))))
|#

#{\subsection{Handling OMEGA's Processes}#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New and adapted commands for OMEGA with muliple processes
;; The commands are adapted from KEIM and are therefore redefined.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Adapted from KEIM

(com~defcommand exit-lisp
  (function oc=exit-lisp)
  (help "Exit from the Lisp completely.")
  (frag-cats exit lisp))

(defun oc=exit-lisp ()
   (when (and (symbolp opr*calling-listener)
	      (string<
	       (string-downcase "httpclient")
	       (string-downcase (string opr*calling-listener)))) ;;RPC response for exit-lisp before exiting
	(http~send-response opr*calling-listener            
			    (rpc~compose-methodresponse (list "exit"))
			    :type "text/xml")                   
	(socket~close opr*calling-listener)                            
	(socket~delete opr*calling-listener))
  (sys~exit-from-lisp))

(com~defcommand exit
  (function oc=leave)
  (argnames )
  (argtypes )
  (frag-cats exit change-comint)
  (help "Leave the current command interpreter top level."))

(defun oc=leave ()
  (opr~quit)
  (setf (proc~resume-hook opr*lisp-listener) #'oc=leave-comint)
  (opr~release-listener)
  (proc~kill opr*omega-core-process)
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))

(defun oc=leave-comint ()
  (omega~release-top-level)
  (setf (proc~resume-hook opr*lisp-listener) nil)
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))

(com~defcommand break
  (function oc=break)
  (argnames )
  (argtypes )
  (frag-cats exit change-comint)
  (help "Go directly into the debugger."))

(defun oc=break ()
;;  (invoke-debugger (make-condition 'break)))
  (break "~%Use :continue to return to OMEGA top Level!~%Do NOT use :reset! This might be fatal for your LISP process!"))

;;; NEW
(com~defcommand single-process-omega
		(frag-cats exit)
		(function opr~quit)
		(help "Returns to old, conservative OMEGA, running in a single process."))

(com~defcommand interrupt
  (function oc=interrupt)
  (frag-cats exit)		
  (help "Interrupt a computation"))		

(defun oc=interrupt ())

(com~defcommand reset
  (function opr~reset-omega-processes)
  (argnames )
  (argtypes )
  (frag-cats exit)
  (help "Go reset OMEGAs main processes."))

#{\subsection{Miscellaneous}#}

(com~defcommand cd      
  (argnames dir)
  (argtypes existing-directory)
  (arghelps "A directory name")
  (frag-cats omega-basic file-io)
  (function oc=cd)
  (log-p T)
  (help "Change the Lisp's current working directory to the given directory."))

(com~defcommand cd-ex    
  (argnames dirname variablename)
  (argtypes pathname string)
  (arghelps "A directory name" "A name referring to an environment or global variable, e.g. EXAMPLESHOME")
  (frag-cats omega-basic file-io)
  (function oc=cd)
  (log-p T)
  (help "Change the Lisp's current working directory to the given directory appended to
the path specified by the additional variable."))

(defun oc=cd (dirname &optional variablename)
  (declare (edited  "27-NOV-1998")
	   (authors Lassaad Pollet)
	   (input "A directory name and optionally a name referring to a"
		  "environment or global variable, e.g. EXAMPLESHOME.")
	   (effect  "Change the Lisp's current working directory to DIRNAME."
		    "If variablename is given then the path specified by this"
		    "variable specified by the additional argument.")
	   (value   "Unspecified."))
  (if variablename
      (let* ((prefix-path (or (sys~getenv variablename) (symbol-value (intern variablename))))
	     (suffix-path (namestring dirname))
	     (path (if prefix-path
		       (concatenate 'string prefix-path "/" suffix-path)
		     suffix-path)))
	(sys~chdir path)
	(omega~trace "Changing the Lisp's current working directory to: ~A~%" 
		     path))
    (progn
      (sys~chdir dirname)
      (omega~trace "Changing the Lisp's current working directory to: ~A~%" 
		   dirname))))

(com~defcommand pwd      
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic file-io)
  (function oc=pwd)
  (help "Print the Lisp's current working directory."))

(defun oc=pwd ()
  (declare (edited  "13-FEB-1997")
	   (authors Lassaad)
	   (input   "None.")
	   (effect  "Print the Lisp's current working directory.")
	   (value   "Unspecified."))
  (omega~message "The Lisp's current working directory: ~A~%" 
		 (sys~current-directory)))

(com~defcommand ls      
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic file-io)
  (function oc=ls)
  (help "Print the Lisp's current working directory."))

(defun oc=ls ()
  (declare (edited  "11-MAR-1998" "13-FEB-1997")
	   (authors Sorge Lassaad)
	   (input   "None.")
	   (effect  "Print the content of Lisp's current working directory.")
	   (value   "Unspecified."))
  (sys~call-system "ls -xF"))

(com~defcommand execute-command
  (function oc=execute-commands)
  (argnames command)
  (argtypes anything-list)
  (arghelps "A list with the fragment, command and arguments.")
  (frag-cats comint file-io log-commands)
  (defaults )
  (help "Applies a command."))

(com~defcommand execute-command*
  (function oc=execute-commands)
  (argnames command)
  (argtypes anything-list)
  (arghelps "A list of lists with the fragment, command and arguments.")
  (frag-cats comint file-io log-commands)
  (defaults )
  (help "Applies a list of commands."))

(defun oc=execute-commands (comlist)
  (let ((comlist (if (atom (car comlist)) (list comlist) comlist))) 
    (dolist (com (butlast comlist) (ccom~apply-command (car (last comlist))))
      (ccom~apply-command com))))

(com~defcommand step-log
  (argnames log-file)
  (argtypes existing-rpy-file)
  (arghelps "Name of a file to execute stepwise.")
  (frag-cats omega-basic log-commands)
  (function oc=step-log)
  (help "Read a log file stepwise and optionally store some of its commands in
a new log file."))

(defun oc=step-log (logf)
  (let* ((interface (comint~interface comint*current-comint))
         (log-mode (omega~query "Storing some commands in a new log file? (y/n)"
				(arg~find-argtype 'boolean)))
	 (new-logf (when log-mode
		     (omega~query (format nil "The name of the new log file (different to ~A)" logf)
				  (arg~find-argtype 'pathname)
				  (oc~default-logfile)))))
    (if (and new-logf
	     (equal (translate-logical-pathname logf) (translate-logical-pathname new-logf)))
	(inter~print-error interface
			   (format nil "The new log file is identical to ~A" logf))
      (sys~handler-case
       (with-open-file (in logf :direction :input
		           :if-does-not-exist :error)
		       (loop (let ((input-list (asi~linereadp in)))
			   (if input-list
			       (oc=apply-command-after-return interface input-list)
			     (return-from oc=step-log))
			   (when new-logf
			     (oc=ask-to-store-command-in interface input-list new-logf)))))
       (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))))

(defun oc=apply-command-after-return (interface input-list)
  (let* ((frag (com~find-fragment (first input-list)))
	 (comm (com~find-command (second input-list) frag))
	 (args-info (rest (rest input-list)))
	 (real-args (ccom~real-args-of args-info comm))
	 (func (com~function comm))
	 (prompt (format nil "~%~%Command:~%  ~A ~{ ~A~}"
			 (keim~name comm) ;;args-info)))
			 (mapcar #'oc~input-form real-args))))
    (when (inter~prompt-for-return interface prompt)
      (apply (symbol-function func) real-args))))

(defgeneric oc~input-form (real-arg)
  (declare (edited  "20-NOV-1995")
	   (authors Lassaad)
	   (input   "A real ARGUMENT of a command.")
	   (effect  "None")
	   (value   "The name of REAL-ARG if this an instance of KEIM+NAME."
		    "Otherwise the print form of the REAL-ARG."))
  (:method ((real-arg KEIM+NAME))
	   (keim~name real-arg))
  (:method (real-arg)
	   real-arg)
  (:documentation "Return the name or the print form of an object."))
  



(defun oc=ask-to-store-command-in (interface input-list logfile)
  (when (inter~prompt-for-input interface
				(format nil "Storing the applyied command in ~A? (y/n)"
					logfile)
				(arg~find-argtype 'boolean))
    (let ((logstream (open logfile
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)))
      (format logstream "~{~A ~}~%" input-list)
      (close logstream))))
    
(defun keim::ccom=logfile-default ()
  (oc~default-logfile))
 
(defun oc~default-logfile ()
  (declare (edited  "15-NOV-1995")
	   (authors Lassaad)
	   (input   "none")
	   (effect  "none")
	   (value   "The default path name of the omega log file." ))
  (let ((tp-dir (oc~default-current-tp-dir)))
    (if (com~specified-arg-p tp-dir)
	(format nil "~Aomega.rpy" tp-dir)
      "~/omega.rpy")))

(defun oc~create-file&evtl-rename-oldfile (path)
  (declare (edited  "17-NOV-1995")
	   (authors Lassaad)
	   (input   "PATH name")
	   (effect  "When one file with the name PATH exists, then it is renamed and"
		    "a new file with the name PATH is created. Otherwise a new file"
		    "with the name PATH is created.")
	   (value   "PATH, if no error is happened."))
  (let ((stream (open path
			 :direction :output 
			 :if-does-not-exist :create
			 :if-exists :rename)))
    (if stream
	(close stream)
      (error "File ~A cannot be opened for log output" path)))
  path)


#|
(com~defcommand log-comment
  (argnames comment)
  (argtypes string)
  (arghelps "The comment to write in the log file")
  (frag-cats omega-basic log-commands)
  (function oc=log-comment)
  (help "WritRead a log file stepwise and eventuell store some of its commands in
a new log file."))

(defun oc=log-comment (str)
  (format t str))
|#

#{\section{Proof Handling}#}

#{\subsection{Loading Problems and Proofs}#}

(com~defcommand read-problem      
  (argnames in-file)
  (argtypes existing-post-file)
  (arghelps "Name of a file to read")
  (frag-cats omega-basic file-io)
  (function oc=read-problem)
  (log-p T)
  (help "Read the file, which should contain a POST representation of a
problem, and make the read problem the new current proof. A new
environment is made to read the problem into."))

(defun oc=read-problem (pname)
  (sys~handler-case
   (with-open-file (in pname :direction :input
                       :if-does-not-exist :error)
                   (let ((plist (read in)))
		     (sys~handler-case
		      (let ((problem (prob~find-problem (cadr plist)))
			    (newobj (post~read-object plist (env~create) nil)))
			(when problem
			  (omega~message "Redefining problem ~A~%" (keim~name problem))
			  (when (prob~proofs problem)
			    (dolist (x (prob~proofs problem))
			      (pds~remove-proof-plan x))))
			(oc=prove-pre (ot~read-proof-plan newobj))
			)
		      (error (c)
			     (omega~error c) 
			     (sys~signal
			      (sys~make-condition 'inter+error
						  :format-string "~A is not a problem file!"
						  :args (list pname)))))))
   (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))

 
(com~defcommand read-pds        
  (argnames in-file)
  (argtypes existing-pds-file) 
  (arghelps "Name of a file to read")
  (frag-cats omega-basic file-io)
  (function oc=read-pds)
  (log-p T)
  (help "Read the file, which should contain a POST representation of a
proof plan, and make the read problem the new current proof. A new
environment is made to read the problem into."))

(defun oc=read-pds (pname)
  (sys~handler-case
   (with-open-file (in pname :direction :input
		       :if-does-not-exist :error)
		   (let* ((list (read in))
			  (newobj (post~read-object (append (list (car list) nil) (cdr list))
						    (env~create)
						    nil))
			  (new-plan (ot~read-proof-plan newobj)))
		     (when (foci~in-use) (foci~compute-pcs :pds new-plan))
		     (oc=prove (ot~read-proof-plan newobj))
		     ;;; Reset the method lists:
		     (meth~current-theory-methods!)
		     (setq omega*current-tp-dir (directory-namestring  (pathname pname)))
		     ))
   (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))


(com~defcommand read-resolution-proof
  (argnames in-file)
  (argtypes existing-post-file)
  (arghelps "Name of a file to read")
  (frag-cats omega-basic file-io)
  (function oc=read-resolution-proof)
  (log-p T)
  (help "Read the file, which should contain a POST representation of a
resolution-proof, and make the read proof the new current resolution proof. A new
environment is made to read the problem into."))

(defun oc=read-resolution-proof (pname)
  (sys~handler-case
   (with-open-file (in pname :direction :input
		       :if-does-not-exist :error)
     (let ((newobj (res~proof-read (read in))))
       (setq omega*current-resolution-proof (ot~read-resolution-proof newobj))
       (res~add-proof-in-hash-table newobj)))
   (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))
 

#{\subsection{Saving Problems and Proofs}#}

(com~defcommand write-problem      
  (argnames outfile supersede?)
  (argtypes pathname boolean)
  (arghelps "Name of a file to write to" "Supersede OUTFILE if it exists?")
  (frag-cats omega-basic file-io)
  (function oc=write-problem)
  (log-p T)
  (help "Write the current proof in the form of a POST expression to the file
established. Second argument determines if the file, when it exists, will be
superseded or appended to."))

(defun oc=write-problem (path supersede?)
  (let ((pathname (ot~post-pathname path)))
    (if (prob~proof-p omega*current-proof-plan)
	(let ((only-problem (if (pds~proof-plan-p omega*current-proof-plan)
				(prob~proof-problem omega*current-proof-plan)
			      omega*current-proof-plan)))
	  (sys~handler-case
	   (with-open-file (out pathname :direction :output 
				:if-exists (if supersede? :supersede :append)
				:if-does-not-exist :create)
			   (post~print only-problem out)
			   (omega~message "Wrote file ~A." (truename out)))
	   (file-error (c)
		       (omega~error c))))
      (arg~signal-wrong-type 'problem omega*current-proof-plan))))


(com~defcommand write-pds          
  (argnames outfile supersede?)
  (argtypes pathname boolean)
  (arghelps "Name of a file to write to" "Supersede OUTFILE if it exists?")
  (frag-cats omega-basic file-io)
  (function oc=write-pds)
  (log-p T)
  (help "Write the current proof in the form of a POST expression to the file
established. Second argument determines if the file, when it exists, will be
superseded or appended to."))

(defun oc=write-pds (path supersede?)
  (let ((pathname (ot~pds-pathname path)))
    (if (pds~proof-plan-p omega*current-proof-plan)
	(sys~handler-case
	 (with-open-file (out pathname :direction :output 
			      :if-exists (if supersede? :supersede :append)
			      :if-does-not-exist :create)
                         (dolist (node (prob~proof-steps omega*current-proof-plan))
			   (dolist (var (term~bound-variables (node~formula node)))
                             (keim~set-name! var (gentemp 'var))))
			 (post~print omega*current-proof-plan out)
			 (omega~message "Wrote file ~A." (truename out)))
	 (file-error (c) (omega~error c)))
      (arg~signal-wrong-type 'proof-plan omega*current-proof-plan))))



(com~defcommand write-resolution-proof
  (argnames outfile supersede?)
  (argtypes pathname boolean)
  (arghelps "Name of a file to write to" "Supersede OUTFILE if it exists?")
  (frag-cats omega-basic file-io)
  (function oc=write-resolution-proof)
  (log-p T)
  (help "Write the current resolution proof in the form of a POST expression to the file
established. Second argument determines if the file, when it exists, will be
superseded or appended to."))

(defun oc=write-resolution-proof (path supersede?)
  (let ((pathname (ot~post-pathname path)))
    (if (res~proof-p omega*current-resolution-proof)
	(sys~handler-case
	 (with-open-file (out pathname :direction :output 
			      :if-exists (if supersede? :supersede :append)
			      :if-does-not-exist :create)
			 (post~print omega*current-resolution-proof out)
			 (omega~message "Wrote file ~A." (truename out)))
	 (file-error (c) (omega~error c)))
      (arg~signal-wrong-type 'resolution-proof omega*current-resolution-proof))))


#{\subsection{Miscellaneous}#}

(com~defcommand prove         
  (argnames proof-plan)
  (argtypes proof-plan)
  (arghelps "A natural deduction proof or a problem")
  (frag-cats omega-basic comint-state)
  (function oc=prove-pre)
  (help "Make the nd proof, given as argument, the current proof-plan. If a
problem is specified, a new proof for this problem will be created."))

(com~defcommand retry-prove         
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic comint-state)
  (function oc=prove-retry)
  (help "Make start a new proof, for the current problem"))

(defun oc=prove-retry ()
  (when omega*current-proof-plan
      (oc=prove-pre (prob~proof-problem omega*current-proof-plan))))

(defun oc=prove-pre (proof-plan)
  (if (prob~p proof-plan)
      (oc=prove
       (pds~start-proof-plan proof-plan (ot~new-proof-plan-name proof-plan)))
    (oc=prove proof-plan))
  ;;; Reset the method lists:
  (meth~current-theory-methods!)
  (keim~name omega*current-proof-plan)) ;return value for xmlrpc

;;; This function is redefined in view/proof-view-com.lisp --- make sure to apply all
;;; changes there!
(defun oc=prove (proof-plan)
  (omega~message "Changing to proof plan ~A" (keim~name proof-plan))
  (setq omega*current-proof-plan proof-plan
	omega*current-theory (prob~proof-theory proof-plan)
	logic*current-theory omega*current-theory
        keim::pds*current-proof-plan proof-plan)
  (setf foci*proof-context (pds~proof-context proof-plan))
  (alift~initialize proof-plan) 
;  (alift~initialize omega*current-proof-plan) ; wird das wegcompiliert?
  ;;; Nein, es wird nicht wegkompiliert! In prog/view/proof-view-com.lisp wird oc=prove
  ;;; ueberdefiniert und dort war diese Zeile auskommentiert! AF
  omega*current-proof-plan)

(defun oc~nil-argument ()   ;;; this thing is for command-defaults
  nil)     

(defun oc~falsity-p (formula)
  (let ((falsity (env~lookup-object :false (pds~environment omega*current-proof-plan))))
    (data~equal formula falsity)))

(defun oc~default-current-planline ()
  (declare
   (authors nesmith)
   (input "none")
   (value "the current planned line of the current proof, or the unspecified value if none such exists"))
  (if (and (pds~proof-plan-p omega*current-proof-plan)
	   (pds~open-nodes omega*current-proof-plan))
      (car (pds~open-nodes omega*current-proof-plan))
    (com~unspecified))) 

(com~defcommand set-focus      
  (argnames planline)
  (argtypes ndplanline)
  (arghelps "Plan line to focus on")
  (frag-cats omega-basic comint-state)
  (function oc=set-focus)
  (help "Makes the argument the current goal to be proven."))


(defun oc=set-focus (planline)
  (pds~focus planline omega*current-proof-plan)
  (oc=show-subproblem planline))

(com~defcommand proof-ground?
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic proof-manipulation)
  (function oc=check-proof)
  (help "Check current proof to see whether it is complete."))

(defun oc=check-proof ()
  (let ((yesno (pds~proof-done-p omega*current-proof-plan)))
    yesno))

(com~defcommand remove-proof         
  (argnames proof-plan)
  (argtypes proof-plan)
  (arghelps "A natural deduction proof")
  (frag-cats omega-basic comint-state)
  (function oc=remove-proof)
  (help "Remove the given proof form OMEGA."))

(defun oc=remove-proof (proof)
  (when (pds~proof-plan-p proof) 
    (let* ((current (eq proof omega*current-proof-plan))
	   (y-n (if current
		    (inter~prompt-for-input
		     (comint~interface comint*current-comint)
		     (format nil "~A is the currently active proof. Erase it? (y/n)" (keim~name proof))
		     (arg~find-argtype 'boolean))
		  t)))
      (when (and y-n (pds~remove-proof-plan proof))
	(when current
	  (setf omega*current-proof-plan nil)
	  (setf foci*proof-context nil))
	(omega~message "Proof successfully erased.")))))

(com~defcommand remove-proof*         
  (argnames proof-plan-list)
  (argtypes proof-plan-list)
  (arghelps "A list of natural deduction proofs")
  (frag-cats omega-basic comint-state)
  (function oc=remove-proof*)
  (help "Remove the list of given proofs from OMEGA."))

(defun oc=remove-proof* (proof-list)
  (mapc #'oc=remove-proof proof-list))

(com~defcommand remove-proofs         
  (argnames problem)
  (argtypes problem)
  (arghelps "A problem")
  (frag-cats omega-basic comint-state)
  (function oc=remove-proofs)
  (help "Removes the all the proofs of the given problem."))

(defun oc=remove-proofs (problem)
  (when
      (notany #'null
	      (mapcar #'pds~remove-proof-plan (prob~proofs problem)))
    (omega~message "All proofs successfully erased.")))



#{\section{Displaying Proofs and Related Stuff}#}
;;; Commands belonging to the direct-display fragment

(com~defcommand show-problem       
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats direct-display)
  (function oc=show-problem)
  (help "Display the current problem without proof steps."))


(defun oc=show-problem ()
  (if omega*current-proof-plan
      (if (pds~proof-plan-p omega*current-proof-plan)
	  (omega~message "~A" (prob~proof-problem omega*current-proof-plan))
	(omega~error "~A"
		     (sys~signal (sys~make-condition 'arg+wrong-type-error
					 :expected (arg~find-argtype 'problem)
					 :received omega*current-proof-plan))))
    (omega~message "No problem was loaded!")))

(com~defcommand show-problems       
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats direct-display)
  (function oc=show-problems)
  (help "Display all currently available problems."))

(defun oc=get-all-problems ()
  (let ((problem-list nil))
    (maphash #'(lambda (name prob)
		 (declare (ignore name))
		 (unless (prob~proven-p prob)
		   (setf problem-list (cons prob problem-list))))
	     keim::prob*problem-hash-table)
    problem-list))

(defun oc=show-problems ()
  (omega~message (ohlp~pprint-sorted-help-object-list (oc=get-all-problems))))

(com~defcommand show-pds        
  (frag-cats direct-display)
  (function oc=show-pds) 
  (help "Display the current proof plan."))


(defun oc=show-pds ()
  (if omega*current-proof-plan
      (if (pds~proof-plan-p omega*current-proof-plan)
	  (let ((string (with-output-to-string (*standard-output*)
					       (pds~pprint-proof-plan omega*current-proof-plan omega*current-ndstyle))))
	    (inter~output-object (comint~interface comint*current-comint) string))
	(arg~signal-wrong-type 'proof-plan omega*current-proof-plan))
    (inter~output-object (comint~interface comint*current-comint)
			 "There is no proof plan currently active!.")))

(com~defcommand show-agenda
  (frag-cats direct-display)
  (function oc=show-agenda)
  (help "Display the tasks on the current agenda.")) 

(defun oc=show-agenda ()
  (if (agenda~in-use)
      (if (not (agenda~empty-p (pds~agenda omega*current-proof-plan)))
	  (omega~message (oc=show-the-agenda (pds~agenda omega*current-proof-plan)))
	(omega~message ";;;The current agenda is empty!"))
    (omega~message ";;;No agenda is set!")))

(defun oc=show-the-agenda (agenda)
  (let ((first-task (agenda~first-task agenda))
        (next-tasks (agenda~next-tasks agenda))
        (then-agenda (agenda~then-agenda agenda)))
    (if first-task
        (concatenate 'string
                     (format nil "first task: ~A" first-task)
                     (if next-tasks
                         (format nil "~%next tasks: ~{~A ~}~% ordered by: ~{~A ~}"
                                 next-tasks (agenda~orderings agenda))
                       "")
                     (if (agenda~empty-p then-agenda) ""
                       (concatenate 'string
                                    (format nil "~%THEN:~%")
                                    (oc=show-the-agenda then-agenda))))
      (if next-tasks
          (concatenate 'string
                       (format nil "next tasks: ~{~A ~}~% ordered by: ~{~A ~}"
                               next-tasks (agenda~orderings agenda))
                       (if (agenda~empty-p then-agenda) ""
                         (concatenate 'string
                                      (format nil "~%THEN:~%")
                                      (oc=show-the-agenda then-agenda))))
        (oc=show-the-agenda then-agenda)))
    ))


(com~defcommand show-subproblem        
  (argnames plan)
  (argtypes ndplanline)
  (arghelps "Plan line to show")
  (frag-cats direct-display)
  (function oc=show-subproblem)
  (defaults ((oc~default-current-planline)))
  (help "Display the current subproblem: an open node and its supports."))


(defun oc=show-subproblem (plan)
  (let ((omega*current-proof-plan
	 (make-instance 'pds+proof-plan
			:steps
			(append (pds~node-supports plan) (list plan))))) 
    (oc=show-pds))
  )


(com~defcommand show-proof        
  (argnames node)
  (argtypes ndline)
  (arghelps "A node whose proof should be shown")
  (frag-cats direct-display)
  (function oc=show-proof)
  (defaults ((oc=default-root-node)))
  (help "Display a proof tree given its root node."))

(defun oc=default-root-node ()
  (declare (edited  "15-JAN-1997")
	   (authors Lassaad)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The root node of the current proof plan."))
  (if (pds~proof-plan-p omega*current-proof-plan)
      (prob~proof-root omega*current-proof-plan)
    (com~unspecified)))
  

(defun oc=show-proof (root-node)
  (declare (edited  "15-JAN-1997")
	   (authors Lassaad)
	   (input   "A node.")
	   (effect  "Shows the proof tree with root ROOT-NODE.")
	   (value   "Unspecified."))
  (let* ((subproof-plan
	  (make-instance 'pds+proof-plan
			 :steps (reverse (pds~linearize-plan root-node))))
	 (string (with-output-to-string (*standard-output*)
		  (pds~pprint-proof-plan subproof-plan omega*current-ndstyle))))
    (inter~output-object (comint~interface comint*current-comint) string))
  )


(com~defcommand show-line      
  (argnames line)
  (argtypes ndline)
  (arghelps "A line to be shown")
  (function oc=show-line)
  (frag-cats direct-display)
  (defaults ((oc~default-current-planline)))
  (help "Show a line."))

(defun oc=show-line (ndline)
  (let ((string (with-output-to-string (*standard-output*)
		 (pds~pprint-pds-node ndline omega*current-ndstyle))))
    (omega~message string)))

(com~defcommand show-line*      
  (argnames lines)
  (argtypes ndline-list)
  (arghelps "A list of lines to be shown")
  (function oc=show-line*)
  (frag-cats direct-display)
  (defaults ((com~unspecified)))
  (help "Show a line."))

(defun oc=show-line* (lines)
  (mapc #'oc=show-line lines))


(com~defcommand show-supports    
  (argnames plan)
  (argtypes ndplanline)
  (arghelps "Plan line")
  (frag-cats direct-display)
  (function oc=show-supports)
  (defaults ((oc~default-current-planline)))
  (help "Display the support lines of an open line."))

(defun oc=show-supports (planline)
  (mapc #'(lambda (line)
	    (omega~message "~A" (keim~name line)))
	(pds~node-supports planline)))

(com~defcommand show-hyps    
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-hyps)
  (help "Display the hypotheses of a line."))

(defun oc=show-hyps (planline)
  (mapc #'(lambda (line)
	    (omega~message "~A" (keim~name line)))
	(pdsn~hyps planline)))


(com~defcommand show-justification        
  (argnames line)
  (argtypes ndline)
  (arghelps "Line to show the justification of ")
  (function oc=show-justification)
  (frag-cats direct-display)
  (help "Show the justification of a line."))

(defun oc=show-justification (ndline)
  (let* ((just (node~justification ndline))
	 (prem (just~premises just))
	 (string (if prem
		     (format nil "justified by ~A from ~A"
			     (keim~name (just~method just))
			     (mapcar #'keim~name prem))
		   (format nil "justified by ~A" (keim~name (just~method just))))))
    (omega~message "~A is ~A" (keim~name ndline) string)))

(com~defcommand show-problem-proofs         
  (argnames problem)
  (argtypes problem)
  (arghelps "A natural deduction proof or a problem")
  (frag-cats omega-basic direct-display)
  (defaults ((oc=default-proof-plan)))
  (function oc=show-problem-proofs)
  (help "Show all currently existing proof plans for the problem."))

(defun oc=default-proof-plan ()
  (if (pds~proof-plan-p omega*current-proof-plan)
      (keim~name (prob~proof-problem omega*current-proof-plan))
    (com~unspecified)))

(defun oc=show-problem-proofs (problem)
  (let ((prob (prob~find-problem problem)))
    (if (prob~proofs prob)
	(progn
	  (omega~message "~%Proof plans for problem ~A:~%~%" (keim~name prob))
	  (mapc #'(lambda (x)
		    (cond ((and (pds~open-nodes x)
				(eq x omega*current-proof-plan))
			   (omega~message "active --> ~A (open)~%" (keim~name x)))
			  ((pds~open-nodes x)
			   (omega~message "           ~A (open)~%" (keim~name x)))
			  ((eq x omega*current-proof-plan)
			   (omega~message "active --> ~A~%" (keim~name x)))
			  (t
			   (omega~message "           ~A~%" (keim~name x)))))
		(prob~proofs prob)))
      (omega~message "~%No proof plans for problem ~A:~%" (keim~name prob)))))

(com~defcommand show-subterm        
  (argnames node position)
  (argtypes ndline position)
  (arghelps "A node containing a subterm to be shown" "The position of a subterm")
  (frag-cats direct-display)
  (function oc=show-subterm)
  (defaults )
  (help "Display the subterm at given position of some formula in the proof tree."))

(defun oc=show-subterm (line pos)
  (let ((term (data~struct-at-position (node~formula line) pos)))
    (when term (omega~message "~A" term))))
			 
(com~defcommand rules&tactics-output
  (argnames trace)
  (argtypes boolean)
  (arghelps "More information on rules and tactics applications")
  (frag-cats omega-basic comint-state)
  (function  oc=rules&tactics-output)
  (defaults (T))
  (log-p nil)
  (help "Starts/stops verbose output of rule- and tactic- interpreter."))

(defun oc=rules&tactics-output (trace)
  (setf tac*verbose trace)
  (setf rule*verbose trace))



#{\section{Proof Manipulation}#}

#{\subsection{Environment and Support Structure}#}

;;; Commands for the fragment Declaration
;;; DECLARE allows you to add new declarations to the current environment.

(com~defcommand declare      
  (argnames form)
  (argtypes anything)
  (arghelps "Post expression (a list) for environment declaration")
  (frag-cats declaration proof-manipulation)
  (function oc=declare)
  (log-p T) 
  (help "Read the form into the environment of the current proof-plan."))

(defun oc=declare (form)
  (cond ((and (consp form) (eq (car form) 'problem))
	 (let ((problem (prob~find-problem (cadr form)))
	       (newobj (post~read-object form (env~create) nil)))
	   (when problem
	     (omega~message "Redefining problem ~A~%" (keim~name problem))
	     (when (prob~proofs problem)
	       (dolist (x (prob~proofs problem))
		 (pds~remove-proof-plan x))))
	   (oc=prove-pre (ot~read-proof-plan newobj))))
	((pds~proof-plan-p omega*current-proof-plan)
	 (sys~handler-case
	  (post~read-object form (pds~environment omega*current-proof-plan) nil)
	  (error (c) (inter~print-error (comint~interface comint*current-comint) c)
		 (sys~signal (sys~make-condition
			      'inter+error :interface (comint~interface comint*current-comint)
			      :format-string "Received ~A, was expecting ~
                                            a POST declaration."
			      :args (list form))))))
	 (T (arg~signal-wrong-type 'proof-plan omega*current-proof-plan))))


(com~defcommand change-name      
  (argnames const new-name)
  (argtypes term symbol)
  (arghelps "A constant" "New name for constant")
  (frag-cats declaration proof-manipulation)
  (function oc=change-name)
  (log-p T) 
  (help "Change DESTRUCTIVELY the name of the constant!"))

(defun oc=change-name (constant new-name)
  (if (and (term~constant-p constant)
	   (symbolp new-name)
	   (not (term~number-p constant))
	   (keim::env=simple-lookup-pair (keim~name constant) (pds~environment omega*current-proof-plan)))
      (progn 
	(env~remove (keim~name constant) (pds~environment omega*current-proof-plan))
	(keim~set-name! constant new-name)
	(env~enter new-name constant (pds~environment omega*current-proof-plan)))
    (omega~error "~A has to be a constant in the local problem environment! Moreover, ~A has to be a symbol." constant new-name)))



(com~defcommand add-support
  (argnames planline new-support)
  (argtypes ndplanline ndline)
  (arghelps "A plan line" "A new supporting line")
  (frag-cats omega-basic proof-manipulation)
  (function oc=add-support)
  (defaults ((oc~default-current-planline) 
	     (com~unspecified)))
  (log-p T)
  (help "Add a new support line for a plan line."))

(defun oc=add-support (planline new-support)
  (pds~add-sponsors planline (list new-support))
  (oc=show-subproblem planline)
) 


(com~defcommand add-support*
  (argnames planline new-supports)
  (argtypes ndplanline ndline-list)
  (arghelps "A plan line" "A list of new supporting lines")
  (frag-cats omega-basic proof-manipulation)
  (function oc=add-support*)
  (defaults ((oc~default-current-planline) 
	     (com~unspecified)))
  (log-p T)
  (help "Add new support lines for a plan line."))

(defun oc=add-support* (planline new-supports)
  (pds~add-sponsors planline new-supports)
  (oc=show-subproblem planline)
)


(com~defcommand support
  (argnames planline new-supports)
  (argtypes ndplanline ndline-list)
  (arghelps "A plan line" "A list of new supporting lines")
  (frag-cats omega-basic proof-manipulation)
  (function oc=support)
  (defaults ((oc~default-current-planline) 
	     (com~unspecified)))
  (log-p T)
  (help "Set support lines for a plan line. Any current sponsoring lines 
not specified will be unsponsored."))
 
(defun oc=support (planline new-supports)
  (pds~delete-sponsors planline (pds~node-supports planline))
  (pds~add-sponsors planline new-supports)
  (oc=show-subproblem planline))


(com~defcommand delete-support
  (argnames planline oldsupport)
  (argtypes ndplanline ndline)
  (arghelps "A plan line" "An old support-line")
  (frag-cats omega-basic proof-manipulation)
  (function oc=delete-support)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Remove line as support of a plan line."))

(defun oc=delete-support (planline old-support)
  (pds~delete-sponsors planline (list old-support))
  (oc=show-subproblem planline)
  )


(com~defcommand delete-support*
  (argnames planline oldsupports)
  (argtypes ndplanline ndline-list)
  (arghelps "Plan line" "A list of old support-lines")
  (frag-cats omega-basic proof-manipulation)
  (function oc=delete-support*)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Remove lines as supports of a plan line."))

(defun oc=delete-support* (planline old-supports)
  (pds~delete-sponsors planline old-supports)
  (oc=show-subproblem planline)
  )


(com~defcommand insert-hyp          
  (argnames line hyp)
  (argtypes ndline ndline)
  (arghelps "A line whose hypotheses list is to be expanded" "A hypothesis to be inserted")
  (frag-cats omega-basic proof-manipulation)
  (function oc=insert-hyp)
  (log-p T)
  (help "Inserts a new hypothesis into the hypotheses list of a line."))

(defun oc=insert-hyp (line hyp)
  (let ((line-hyps (pdsn~hyps line))
	(line-supp (pds~node-supports line)))
    (when (not (member hyp line-hyps))
      (setf (pdsn~hyps line) (cons hyp line-hyps))
      (when (pdsn~open-node-p line)
	(setf (pds~node-supports line) (cons hyp line-supp))))))



(com~defcommand insert-hyp*          
  (argnames line line-list)
  (argtypes ndline ndline-list)
  (arghelps "A line whose hypotheses list is to be expanded" "A list of hypotheses to be inserted")
  (frag-cats omega-basic proof-manipulation)
  (function oc=insert-hyp*)
  (log-p T)
  (help "Inserts new hypotheses into the hypotheses list of a line."))

(defun oc=insert-hyp* (line line-list)
  (let ((line-hyps (pdsn~hyps line))
	(line-supp (pds~node-supports line)))
    (dolist (hyp line-list)
      (when (not (member hyp line-hyps))
	(setf line-hyps (cons hyp line-hyps))
	(when (pdsn~open-node-p line)
	  (setf line-supp (cons hyp line-supp)))))
    (setf (pdsn~hyps line) line-hyps)
    (setf (pds~node-supports line) line-supp)))


#{\subsection{Proof and Problem Structure}#}

(com~defcommand add-hyp            
  (argnames formula new-name)
  (argtypes formula symbol)
  (arghelps "Formula for new hypothesis" "Adding a hypotheses leads to a new problem. Please enter its name")
  (frag-cats omega-basic proof-manipulation)
  (function oc=add-hyp)
  (log-p T)
  (help "Add the formula as a hypothesis to the current proof."))

(defun oc=add-hyp (formula new-name)
  (let ((old-prob (prob~proof-problem omega*current-proof-plan)))
    (if (prob~find-problem new-name)
	(omega~warn "~%Problem ~A already exists. No changes made." new-name)
      (let ((new-prob (prob~create new-name
				   (prob~theory old-prob)
				   (prob~environment old-prob)
				   (prob~assumptions old-prob)
				   (prob~conclusion old-prob)
				   nil
				   (prob~category old-prob)
				   (help~help-string old-prob))))
	(oc=rename-current-pds (ot~new-proof-plan-name new-prob))
	(setf (prob~proof-problem omega*current-proof-plan) new-prob)
	(prob~add-proof new-prob omega*current-proof-plan)
	(pds~add-hypothesis formula omega*current-proof-plan)
	(oc=prove omega*current-proof-plan)))))
  
  
(defun oc=rename-current-pds (name &optional (proof-plan omega*current-proof-plan))   ;;; dirty hack!!!! VS
  (pds~remove-proof-plan proof-plan)
  (keim~set-name! omega*current-proof-plan name)
  (setf (gethash (symbol-name name) keim::prob*proof-hash-table) proof-plan)
  (setf (gethash (symbol-name name) keim::pds*proof-plan-hashtable) proof-plan))







(com~defcommand add-local-hyp            
  (argnames formula)
  (argtypes formula)
  (arghelps "Formula for new hypothesis")
  (frag-cats omega-basic proof-manipulation)
  (function oc=add-local-hyp)
  (log-p T)
  (help "Add the formula as a local hypothesis to the current proof."))

(defun oc=add-local-hyp (formula)
  (let* ((new-hyp (pdsn~make-hypothesis formula (pds~new-node-name omega*current-proof-plan))))
    (pds~only-insert-node-between! new-hyp
				   (pdsn~hyps (prob~proof-root omega*current-proof-plan))
				   (list (prob~proof-root omega*current-proof-plan))
				   omega*current-proof-plan)))








(com~defcommand lemma
  (argnames node formula)
  (argtypes ndplanline formula)
  (arghelps "An open node" "Formula to be proved as lemma")
  (function oc=lemma)
  (frag-cats omega-basic proof-manipulation)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Insert a lemma in order to prove an open node."))

(defun oc=lemma (node lemma-formula)
  (let* ((lemma-hyps (pdsn~hyps node))
	 (lemma-supps (pds~node-supports node omega*current-proof-plan))
	 (lemma-node (pdsn~open-node-create lemma-formula
					    lemma-hyps
					    (pds~new-node-name omega*current-proof-plan)))
	 (task (agenda~create-open-task lemma-node)))
    (pds~add-sponsors lemma-node lemma-supps)
    (pds~add-sponsors node (list lemma-node))
    (pds~insert-node! lemma-node omega*current-proof-plan)
    (setf (agenda~next-tasks (pds~agenda omega*current-proof-plan))
	  (cons task (agenda~next-tasks (pds~agenda omega*current-proof-plan))))
    ))

;;(com~defcommand island
;;  (argnames formula support-list supported-list)
;; (argtypes formula ndline-list ndline-list)
;;  (arghelps "A forumula for a new open node."
;;	    "A list of support-nodes for the new open-node."
;;	    "A list of open nodes which are supported by the new open-node.")
;;  (frag-cats omega-basic proof-manipulation)
;;  (function oc=insert-island!)
;;  (defaults ((com~unspecified)
;;	     (com~unspecified)
;;	     (com~unspecified)
;;	     ))
;;  (log-p T) 
;;  (help "Introduces a new open line as an island into the plan."))

		
(defun oc=insert-island! (formula supports-for-new-line lines-supported-by-new-line &optional (pds omega*current-proof-plan))
  (declare (edited  "29-JUL-1999")
	   (authors Ameier)
	   (input   "A formula, two lists of nodes and a pds.")
	   (effect  "A new open line is created"
		    "with the formula and introduced into the pds. The set of hyps of the new line"
		    "is the union of all hyps of the supports (the first input list of nodes) for the"
		    "new line. All lines in the seconmd input list of nodes get the new node as support.")
	   (value   "Undefined."))

  (let* ((hyps (remove-duplicates (apply 'append (mapcar #'pdsn~hyps supports-for-new-line))))  
	 (label (pds~new-node-name pds))
	 (new-node (pdsn~open-node-create formula hyps label)))
    ;; sollten da noch reasons rein in diesen neuen KNOTEN ?????

    ;; new-node bekommt genau supports als supports
    (pds~delete-sponsors new-node (pds~node-supports new-node) pds)
    (pds~add-sponsors new-node supports-for-new-line pds)

    ;; die anderen Knoten bekommen new-node as support
    (mapcar #'(lambda (node)
		(if (pdsn~open-node-p node)
		    (pds~add-sponsors node (list new-node) pds)
		  (omega~message "~%Warning: ~A should be an open-node!")))
	    lines-supported-by-new-line)

    ;; falls unter den supported-nodes solche sind, die eine kleinere Hypothesen-Menge als
    ;; der neue Knoten haben, so ergibt das eine Warning! (da dann der neue Knoten nicht
    ;; benutzt werden kann zum schliessen des Knotens)

    (mapcar #'(lambda (node)
		(let* ((hyps-of-node (pdsn~hyps node)))
		  (when (not (subsetp hyps hyps-of-node))
		    (omega~message "~%The hypothesis set of the new node ~A is ~A and thus not a subset of the hypothesis set ~A of the  supported node ~A." new-node hyps hyps-of-node node))))
	    lines-supported-by-new-line)
    
    ;; new-node wird in pds eingefuegt
    (pds~insert-node! new-node pds)))


(defun oc=local-def-intro! (term)
  (let* ((term-type (term~type term))
	 (new-const (term~generate-term-primitive-with-new-name 'ld term-type 'term+constant
								(pds~environment omega*current-proof-plan)))
	 (definition (term~appl-create (env~lookup-object '=def (pds~environment omega*current-proof-plan))
				       (list new-const term) 
				       ))
	 (node (node~create (keim~name new-const)
			    definition 
			    (just~create (infer~find-method 'local-def) nil)))
	 (non-hyp-nodes (remove-if #'pdsn~hypothesis-node-p (prob~proof-steps omega*current-proof-plan)))
	 (conc (prob~proof-root omega*current-proof-plan))
	 (new-pds-node (keim::pds=node2pdsn node)))

    (setf (pds~support-nodes omega*current-proof-plan)
	  (union (list new-pds-node) (pds~support-nodes omega*current-proof-plan)))

    (dolist (line non-hyp-nodes new-pds-node)
      (setf (pdsn~hyps line)
	    (union (list new-pds-node) (pdsn~hyps line))))

    (pds~only-insert-node-between! new-pds-node
				   (pdsn~hyps conc)
				   (list conc)
				   omega*current-proof-plan)

    new-pds-node))



;;; DELETE will delete a line or a group of lines from the current proof.

(com~defcommand delete
  (argnames line)
  (argtypes ndline)
  (arghelps "The line to delete.")
  (frag-cats omega-basic proof-manipulation)
  (function oc=delete)
  (log-p T)
  (help "Delete the given line from the current proof-plan.  All lines that
depend on it will have their justifications changed.  All other lines 
that use the specified line as hypothesis will also be deleted."))

(defun oc=delete (node &optional (pds omega*current-proof-plan))
  ;;; The delete node version for OMEGA-3 without PLAN-3, loading the
  ;; system PLAN-3 will redefine this function (see /plan/plan.lisp):
  (let ((undone-steps (pds~delete-node! node t pds)))
    (when (agenda~in-use)
      (let ((pds-nodes (prob~proof-steps pds))
	    (pds-agenda (pds~agenda pds))
	    (new-tasks))
	(dolist (step undone-steps)
	  (let ((step-node (pdsc~an-node step)))
	    (when (find step-node pds-nodes)
	      (setq new-tasks (cons (agenda~create-goal step-node) new-tasks)))))
	(when new-tasks
	  (if (agenda~empty-p pds-agenda)
	      (setf (pds~agenda pds)
		    (agenda~create nil new-tasks nil pds-agenda))
	    (setf (agenda~next-tasks pds-agenda) (append (agenda~next-tasks pds-agenda) new-tasks)
		  (pds~agenda pds) (agenda~update pds-agenda pds-nodes))))))
    (when (foci~in-use)  (foci~compute-pcs :pds pds :delete-case t))))

(com~defcommand delete*
  (argnames lines)
  (argtypes ndline-list)
  (arghelps "The lines to delete.")
  (frag-cats omega-basic proof-manipulation)
  (function oc=delete*)
  (log-p T)
  (help "Delete the given lines from the current proof-plan.  All lines that
depend on them will have their justifications changed.  All other lines 
that use one or more of the specified lines as hypotheses will also be
deleted."))

(defun oc=delete* (lines)
  (dolist (line lines)
    (when (pds~label2node (keim~name line) omega*current-proof-plan)
      (oc=delete line))))

(com~defcommand open
   (argnames line)
   (argtypes ndline)
   (arghelps "The line to be made an open line")
   (frag-cats omega-basic proof-manipulation backtracking)
   (function oc=open)
   (defaults )
   (log-p T)
   (help "Make a justified line open again."))

(defun oc=open (node &optional (pds omega*current-proof-plan))
  ;;; The open node version for OMEGA-3 without PLAN-3, loading the
  ;; system PLAN-3 will redefine this function (see /plan/plan-com.lisp):
  (let ((undone-steps (pds~open-node! node pds)))
    (when (agenda~in-use)
      (let ((pds-nodes (prob~proof-steps pds))
	    (pds-agenda (pds~agenda pds))
	    (new-tasks))
	(dolist (step undone-steps)
	  (let ((step-node (pdsc~an-node step)))
	    (when (find step-node pds-nodes)
	      (setq new-tasks (cons (agenda~create-goal step-node) new-tasks)))))
	(when new-tasks
	  (if (agenda~empty-p pds-agenda)
	      (setf (pds~agenda pds)
		    (agenda~create nil new-tasks nil pds-agenda))
	    (setf (agenda~next-tasks pds-agenda) (append (agenda~next-tasks pds-agenda) new-tasks)
		  (pds~agenda pds) pds-agenda)))))
    (when (foci~in-use)  (foci~compute-pcs :pds pds :delete-case t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion and Contraction of Nodes  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; expansion and stuff....

(com~defcommand expand-node          
   (argnames line)
   (argtypes ndline)
   (arghelps "The line to be expanded")
   (frag-cats omega-basic proof-manipulation)
   (function oc=expand-node)
   (defaults ((oc=default-node-to-expand)))
   (log-p T)
   (help "Expand a node."))

(defun oc=default-node-to-expand ()
  (first (oc=default-nodes-to-expand)))

(defun oc=expand-node (line)
  (when line
    (let ((just (node~justification line)))
      (if (pdsj~expanded-p just)
	  (let ((below (pdsj~below just)))
	    (if below
		(setf (node~justification line) below)
	      (omega~warn "An expanded current justification of ~A without below justification!." (keim~name line))))
	(when (or (pdsj~untested-p just) (pdsj~unexpanded-p just))
	  (let ((pattern (pdsj~outline-pattern just))
		(prem (just~premises just)))
	    (omega~message "~%Expanding the node ~A ..." (keim~name line))
	    (infer~apply-expansion-function (just~method just)
					    (if (infer~method-p (just~method just))
						(cons line prem)
					      (oc~build-outline line pattern prem))
					    (pdsj~parameters just))
	    ))))))
  
(defun oc~build-outline (line pattern prem)
  (declare (edited  "28-JUN-1997 05:53")
	   (authors SORGE)
	   (input   "A line to be expanded, the outline-pattern and premises"
		    "of its justification.")
	   (effect  "None.")
	   (value   "The complete outline-pattern needed for the inference"
		    "method. This is especially important when there is"
		    "more than one conclusion involved."))
  (if pattern
      (let* ((new-pattern (subseq pattern 0 (- (length pattern) (length prem))))
	     (concs (mapcar #'pds~label2node new-pattern))
	     (pos (position-if #'null concs))
	     )
	(when pos
	  (setf (nth pos concs) line))
	(append (mapcar #'oc=fully-expand-node concs) prem))
    (cons line prem)))

(com~defcommand expand-node*          
   (argnames lines)
   (argtypes ndline-list)
   (arghelps "A list of lines to be expanded")
   (frag-cats omega-basic proof-manipulation)
   (function oc=expand-node*)
   (defaults ((oc=default-nodes-to-expand)))
   (log-p T)
   (help "Expand some nodes."))

(defun oc=expand-node* (lines)
  (dolist (l lines)
    (opr~signal-interrupt l)
    (oc=expand-node l)))

(defun oc=default-nodes-to-expand ()
  (labels ((order-embedded-nodes (to-investigate &optional ordered)
				 (if to-investigate
				     (order-embedded-nodes
				      (mapcan #'(lambda (node)
						  (copy-list (pdsn~just-premises node)))
					      to-investigate)
				      (append ordered
					      (remove-if
					       #'(lambda (node)
						   (or (pdsn~open-node-p node)
						       (pdsj~grounded-p (node~justification node))))
					       to-investigate)))
				   ordered))
	   (remove-double-nodes (list &optional restlist)
				(if list 
				    (if (find (car list) restlist)
					(remove-double-nodes (cdr list) restlist)
				      (remove-double-nodes (cdr list)
							   (cons (car list) restlist)))
				  (reverse restlist))))
    (let* ((ng-nodes (order-embedded-nodes (list (prob~proof-root omega*current-proof-plan))))
	   (result (when ng-nodes
		     (let ((ue-nodes (remove-if-not #'(lambda (n) (pdsj~unexpanded-p (node~justification n)))
						    ng-nodes)))
		       (if ue-nodes
			   (flet ((num-just (node)
					    (let ((meth (keim~name (just~method (node~justification node)))))
					      (find meth '(simplify-num simplify-num* expand-num expand-num* arith-simplify)
						    :test #'string-equal))))
			     (let ((non-num-nodes (remove-if #'num-just ue-nodes)))
			       (if non-num-nodes
				   non-num-nodes								   
				 ue-nodes)))
			 (let ((ut-nodes (remove-if-not #'(lambda (n) (pdsj~untested-p (node~justification n)))
							ng-nodes)))
			   (if ut-nodes
			       ut-nodes
			     ng-nodes)))))))
      (remove-double-nodes result))))

(com~defcommand expand-one-level          
   (frag-cats omega-basic proof-manipulation)
   (function oc=expand-one-level)
   (log-p T)
   (help "Expand a complete level in the PDS."))

(defun oc=expand-one-level ()
  (oc=expand-node* (oc=default-nodes-to-expand)))
  

(com~defcommand expand-justification
   (argnames justification)
   (argtypes inference)
   (arghelps "An inference rule to be expanded")
   (frag-cats omega-basic proof-manipulation)
   (function oc=expand-justification)
;;   (defaults ((oc=default-nodes-to-expand)))
   (log-p T)
   (help "Expand all nodes containing the given justified by the given inference rule."))

(defun oc=expand-justification (inference)
  (let* ((all-nodes (prob~proof-steps omega*current-proof-plan))
	 (all-inference-nodes (remove-if-not #'(lambda (node)
						 (keim~equal inference (node~just-method node)))
					     all-nodes)))
    (mapcar #'oc=expand-node all-inference-nodes)))


(com~defcommand ground-node          
   (argnames line)
   (argtypes ndline)
   (arghelps "The line to be expanded until it is ground")
   (frag-cats omega-basic proof-manipulation)
   (function oc=ground-node)
   (defaults ((oc=default-node-to-expand)))
   (log-p T)
   (help "Expand a node."))

(defun oc=ground-node (line)
  (when line (oc=fully-expand-node line)))

(defun oc=fully-expand-node (node)
  (declare (edited  "29-JUN-1997 09:23")
	   (authors SORGE)
	   (input   "A node.")
	   (effect  "Sets the active justification of the node to the least"
		    "abstract justification.")
	   (value   "The node."))
  (let ((below-just (pdsj~below (node~justification node)))) 
    (unless (null below-just)
      (setf (node~justification node) below-just)
      (oc=fully-expand-node node))
    node))
    
(com~defcommand ground-pds          
   (frag-cats omega-basic proof-manipulation)
   (function oc=ground-pds)
   (log-p T)
   (help "Expand every node in the PDS until it is grounded on ND level."))

(defun oc=ground-pds ()
  (do ((nodes (oc=default-nodes-to-expand) (oc=default-nodes-to-expand)))
      ((null nodes))
    (opr~signal-interrupt nodes)
    (oc=expand-node* nodes)))

;;; contraction and stuff....

(com~defcommand unexpand-node          
   (argnames line)
   (argtypes ndline)
   (arghelps "The line to be unexpanded")
   (frag-cats omega-basic proof-manipulation)
   (function oc=unexpand-node)
   (defaults ((oc=default-node-to-unexpand)))
   (log-p T)
   (help "Contract a node."))

(defun oc=default-node-to-unexpand ()
  (first (oc=default-nodes-to-unexpand)))

(defun oc=unexpand-node (line)
  (when line
    (let* ((just (node~justification line))
	   (above (pdsj~above just)))
      (when above (setf (node~justification line) above)))))
	
(com~defcommand unexpand-node*          
   (argnames lines)
   (argtypes ndline-list)
   (arghelps "A list of lines to be unexpanded")
   (frag-cats omega-basic proof-manipulation)
   (function oc=unexpand-node*)
   (defaults ((oc=default-nodes-to-unexpand)))
   (log-p T)
   (help "Contract some nodes."))

(defun oc=unexpand-node* (lines)
  (mapc #'oc=unexpand-node lines))

(defun oc=default-nodes-to-unexpand ()
  (labels ((order-embedded-nodes (to-investigate &optional ordered)
				 (if to-investigate
				     (order-embedded-nodes
				      (mapcan #'(lambda (node)
						  (copy-list (pdsn~just-premises node)))
					      to-investigate)
				      (append ordered
					      (remove-if
					       #'(lambda (node)
						   (or (pdsn~open-node-p node)
						       (not (pdsj~above (node~justification node)))))
					       to-investigate)))
				   ordered))
	   (remove-double-nodes (list &optional restlist)
				(if list 
				    (if (find (car list) restlist)
					(remove-double-nodes (cdr list) restlist)
				      (remove-double-nodes (cdr list)
							   (cons (car list) restlist)))
				  (reverse restlist))))
    (let* ((nc-nodes (order-embedded-nodes (list (prob~proof-root omega*current-proof-plan))))
	   )
      (remove-double-nodes nc-nodes))))

(com~defcommand unexpand-one-level          
   (frag-cats omega-basic proof-manipulation)
   (function oc=unexpand-one-level)
   (log-p T)
   (help "Contract a complete level in the PDS."))

(defun oc=unexpand-one-level ()
  (oc=unexpand-node* (oc=default-nodes-to-unexpand)))
  
(com~defcommand abstract-node          
   (argnames line)
   (argtypes ndline)
   (arghelps "The line to be contracted until it is at its most abstract level")
   (frag-cats omega-basic proof-manipulation)
   (function oc=abstract-node)
   (defaults ((oc=default-node-to-unexpand)))
   (log-p T)
   (help "Expand a node."))

(defun oc=abstract-node (line)
  (when line (oc=fully-unexpand-node line)))

(defun oc=fully-unexpand-node (node)
  (declare (edited  "29-JUN-1997 09:23")
	   (authors SORGE)
	   (input   "A node.")
	   (effect  "Sets the active justification of the node to the most"
		    "abstract justification.")
	   (value   "The node."))
  (let ((above-just (pdsj~above (node~justification node)))) 
    (unless (null above-just)
      (setf (node~justification node) above-just)
      (oc=fully-unexpand-node node))
    node))

(com~defcommand abstract-pds          
   (argnames )
   (argtypes )
   (arghelps )
   (frag-cats omega-basic proof-manipulation)
   (function oc=abstract-pds)
   (defaults )
   (log-p T)
   (help "Contract every node in the PDS until it is on the most abstract level."))

(defun oc=abstract-pds ()
  (let ((nodes (oc=default-nodes-to-unexpand)))
    (mapc #'oc=fully-unexpand-node nodes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start of cleanup-proof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand cleanup-proof
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic proof-manipulation)
  (function oc=cleanup-proof)
  (log-p T)
  (help "Check current proof to see whether it is complete, if so, delete all proof lines that do not contribute to the proof of the conclusion."))


;;; This function is redefined in view/proof-view-com.lisp --- make sure to apply all
;;; changes there!
(defun oc=cleanup-proof ()
  (if (pds~proof-plan-p omega*current-proof-plan)
      (pds~cleanup omega*current-proof-plan)
    (arg~signal-wrong-type 'proof-plan omega*current-proof-plan)))


(defun oc~default-current-tp-dir ()
  (if omega*current-tp-dir
      omega*current-tp-dir
      (com~unspecified)))

(defun oc~default-current-proverb-dir ()
  (if omega*current-tp-dir
      omega*current-tp-dir
    "nil"))

#{\section{Theory Handling}#}

#{\subsection{Display Commands}#}

(com~defcommand show-theories         
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic theory)
  (function oc=show-theories)
  (help "Display all loaded theories."))

(defun oc=show-theories ()
  (omega~message (ohlp~pprint-sorted-help-object-list (th~all-theories))))

(com~defcommand available-theories
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic theory)
  (function oc=available-theories)
  (help "Display all loadable theories."))

(defun oc=available-theories ()
  (omega~message
   (ohlp~pprint-sorted-list2columns
    (mapcan #'(lambda (dir)
		(let ((dir-name (format nil "~A/" (namestring dir))))
		  (when (and (probe-file dir-name)
			     (not (find (pathname-name dir) '(CVS RCS SCCS) :test #'string-equal)))
		    (list (pathname-name dir)))))
	    (mapcan #'directory
		    *theory-registry*)))))

(com~defcommand show-theory         
  (argnames theory-name)
  (argtypes symbol)
  (arghelps "The name of a theory to be displayed")
  (frag-cats omega-basic theory)
  (function oc=show-theory)
  (defaults ((oc=default-theory)))
  (help "Fully display a theory when loaded."))

(defun oc=show-theory (th-name)
  (let ((theory (th~find-theory th-name)))
    (if theory
	(omega~message "~A~%" theory)
      (omega~error "Theory ~A is not loaded.~%" th-name))))
				    
      
(com~defcommand show-theory*         
  (argnames theory-names)
  (argtypes symbol-list)
  (arghelps "The name of theories to be displayed")
  (frag-cats omega-basic theory)
  (function oc=show-theory*)
  (help "Fully display a theory when loaded."))

(defun oc=show-theory* (th-names)
  (mapc #'oc=show-theory th-names))
				    

(com~defcommand problem-theory         
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic theory)
  (function oc=problem-theory)
  (help "Shows the theory of the current proof plan."))

(defun oc=problem-theory ()
  (if omega*current-proof-plan
      (omega~message "The theory of the current proof plan: ~%   ~A: ~A"
		     (oc=default-theory)
		     (help~help-string (prob~theory omega*current-proof-plan)))
    (omega~message "No problem has been loaded!")))

(com~defcommand current-theory         
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic theory)
  (function oc=current-theory)
  (help "Shows the OMEGA current theory."))

(defun oc=current-theory ()
  (if omega*current-theory
      (omega~message "The current theory: ~%   ~A: ~A"
		     (keim~name omega*current-theory)
		     (help~help-string omega*current-theory))
    (omega~message "The OMEGA current theory is not instantiated!")))


(com~defcommand import-theories         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to list its import theories")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=import-theories)
  (help "Lists the import theories of a given theory."))

(defun oc=import-theories (theory-name)
  (let ((import-thys (th~imports-recursed theory-name)))
    (if import-thys
	(omega~message "All import theories of ~A:~%~{   ~A: ~A~%~}"
		       theory-name
		       (apply #'append (mapcar #'(lambda (thy)
						   (list (keim~name thy) (help~help-string thy)))
					       import-thys)))
      (omega~message "~A does not import any theory!" theory-name))))

(com~defcommand theory-all-assumptions         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to list its assumptions")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-all-assumptions)
  (help "Lists all the theory assumptions."))

(defun oc=default-theory ()
  (if omega*current-proof-plan
      (keim~name (prob~proof-theory omega*current-proof-plan))
    (keim~name omega*current-theory)))

(defun oc=theory-all-assumptions (theory-name)
  (let ((assumptions (th~assumptions-recursed theory-name)))
    (omega~message
     (if assumptions
	 (ohlp~pprint-sorted-help-object-list assumptions)
       "No assumptions available!."))))
    
(com~defcommand theory-own-assumptions         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to list their own assumptions")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-own-assumptions)
  (help "Lists the own assumptions of a theory."))

(defun oc=theory-own-assumptions (theory-name)
  (let ((assumptions (th~assumptions theory-name)))
    (omega~message
     (if assumptions
	 (ohlp~pprint-sorted-help-object-list assumptions)
       "No assumption is available!."))))

(com~defcommand theory-all-theorems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "A name of a theory to list its assumptions")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-all-theorems)
  (help "Lists all the theory theorems."))

(defun oc=theory-all-theorems (theory-name)
  (let ((theorems (th~theorems-recursed theory-name)))
    (omega~message
     (if theorems
	 (ohlp~pprint-sorted-help-object-list theorems)
       "No theorems available!."))))
    
(com~defcommand theory-own-theorems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to list their own theorems")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-own-theorems)
  (help "Lists the own theorems of a theory."))

(defun oc=theory-own-theorems (theory-name)
  (let ((theorems (th~theorems theory-name)))
    (omega~message
     (if theorems
	 (ohlp~pprint-sorted-help-object-list theorems)
       "No theorems available!."))))

(com~defcommand theory-all-problems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "A name of a theory to list its assumptions")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-all-problems)
  (help "Lists all the theory problems."))

(defun oc=theory-all-problems (theory-name)
  (let ((problems (th~problems-recursed theory-name)))
    (omega~message
     (if problems
	 (ohlp~pprint-sorted-help-object-list problems)
       "No problems available!."))))
    
(com~defcommand theory-own-problems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to list their own problems")
  (defaults ((oc=default-theory)))
  (frag-cats omega-basic theory)
  (function oc=theory-own-problems)
  (help "Lists the own problems of a theory."))

(defun oc=theory-own-problems (theory-name)
  (let ((problems (th~problems theory-name)))
    (omega~message
     (if problems
	 (ohlp~pprint-sorted-help-object-list problems)
       "No problems available!."))))

(com~defcommand show-ass         
  (argnames ass-name)
  (argtypes thy-assumption)
  (arghelps "The name of an assumption to be shown")
  (function oc=show-ass)
  (frag-cats omega-basic theory)
  (help "Show an assumption of the problem theory"))

(defun oc=show-ass (assumption)
  (let ((ass (format nil "~A (~A) ~A " (keim~name assumption)
		     (keim~name (th~ass-theory assumption))
		     (th~ass-formula assumption))))
    (cond ((th~axiom-p assumption)      (omega~message "Axiom:      ~A" ass))
	  ((th~definition-p assumption) (omega~message "Definition: ~A" ass))
	  (t (omega~message "~A: ~A" (prob~category assumption) ass)))))
				     				    
(com~defcommand show-ass*         
  (argnames ass-list)
  (argtypes thy-ass-list)
  (arghelps "A list of assumptions to be shown")
  (function oc=show-ass*)
  (frag-cats omega-basic theory)
  (help "Show some assumptions of the problem theory"))

(defun oc=show-ass* (ass-list)
  (mapc #'oc=show-ass ass-list))

(com~defcommand show-theory-problem       
  (argnames prob)
  (argtypes thy-problem)
  (arghelps "A problem of the current theory to be displayed.")
  (frag-cats direct-display)
  (function oc=show-theory-prob)
  (help "Display a problem of the current theory."))


(defun oc=show-theory-prob (prob)
  (inter~output-object (comint~interface comint*current-comint)
			prob))
  
(com~defcommand show-theory-problem*       
  (argnames prob)
  (argtypes thy-prob-list)
  (arghelps "A list of problem of the current current theory to be displayed.")
  (frag-cats direct-display)
  (function oc=show-theory-prob*)
  (help "Display a problem of the current theory."))


(defun oc=show-theory-prob* (problem-list)
  (mapc #'oc=show-theory-prob problem-list))


#{\subsection{Loading from the Theory Database}#}

(com~defcommand require-theory         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to read")
  (frag-cats omega-basic theory file-io)
  (function oc=require-theory)
  (log-p T)
  (help "Make the required theory  the new current theory, if necessary load it from the theory registry."))

(defun oc=require-theory (name)
  (setq omega*current-theory (th~require name))
  (setq logic*current-theory omega*current-theory))

(com~defcommand require-pure-theory         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to read")
  (frag-cats omega-basic theory file-io)
  (function oc=require-pure-theory)
  (log-p T)
  (help "Make the required theory  the new current theory, if necessary load it from the theory registry."))

(defun oc=require-pure-theory (name)
  (setq omega*current-theory (th~require-only name))
  (setq logic*current-theory omega*current-theory))

(com~defcommand require-complete-theory         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "Name of a theory to read")
  (frag-cats omega-basic theory file-io)
  (function oc=require-complete-theory)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Make the required theory  the new current theory, if necessary load it completely (including theorems and methods) from the theory registry."))

(defun oc=require-complete-theory (name)
  (setq omega*current-theory (th~require-completely name))
  (setq logic*current-theory omega*current-theory)
  (keim~name logic*current-theory)) ;return value for xmlrpc

(com~defcommand load-theory-theorems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose theorems are to be loaded")
  (frag-cats omega-basic theory file-io)
  (function th~load-theorems)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the theorems of the given theory."))

(com~defcommand load-theory-methods         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose methods are to be loaded")
  (frag-cats omega-basic theory file-io)
  (function th~load-methods)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the methodss of the given theory."))

(com~defcommand load-theory-tactics         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose tactics ar to be loaded")
  (frag-cats omega-basic theory file-io)
  (function th~load-tactics)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the tactics of the given theory."))

(com~defcommand load-theory-rules         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose tactics ar to be loaded")
  (frag-cats omega-basic theory file-io)
  (function th~load-rules)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the tactics of the given theory."))

(com~defcommand load-theory-problems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose problems ar to be loaded")
  (frag-cats omega-basic theory file-io)
  (function oc=load-theory-problems)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the problems of the given theory."))

(defun oc=load-theory-problems (theo)  ;return value for xmlrpc
  (mapcar #'keim~name  (th~problems (th~load-problems theo))))

(com~defcommand load-theorem-proofs
  (argnames theorem)
  (argtypes theorem)
  (arghelps "The name of the theorem to be loaded")
  (frag-cats omega-basic theory file-io)
  (function th~load-theorem-proofs)
  (defaults)
  (log-p T)
  (help "Load the proof of the given theorem."))

(com~defcommand execute-theorem-log
  (argnames theorem)
  (argtypes problem)
  (arghelps "The name of a theorem of the current theory to be executed")
  (frag-cats omega-basic theory file-io)
  (function th~execute-theory-replay)
  (defaults)
  (log-p T)
  (help "Execute the log-file of the given theorem."))

(com~defcommand execute-theory-log
  (argnames theory)
  (argtypes existing-theory)
  (arghelps "The name of the theory")
  (frag-cats omega-basic theory file-io)
  (function oc=execute-theory-log)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Execute the log-files of the given theory."))

(defun oc=execute-theory-log (theo)
  (th~execute-all-theory-replay (list theo)))


(com~defcommand execute-theory-log*
  (argnames theories)
  (argtypes existing-theory-list)
  (arghelps "A list with the names of theories")
  (frag-cats omega-basic theory file-io)
  (function th~execute-all-theory-replay theories)
  (defaults)
  (log-p T)
  (help "Execute the log-files of the given theories."))

(com~defcommand execute-all-theory-log
  (argnames )
  (argtypes )
  (frag-cats omega-basic theory file-io)
  (function th~execute-all-theory-replay)
  (defaults)
  (log-p T)
  (help "Execute all log-files of all theories."))

#{\subsection{Writing into the Theory Database}#}

(defun oc=default-problem ()
  (keim~name (prob~proof-problem omega*current-proof-plan)))

(com~defcommand write-to-theory         ;; works KK, now MP
  (argnames )
  (argtypes )
  (frag-cats omega-basic theory file-io)
  (function oc=write2theory)
  (log-p T)
  (help "Write the current object as theorem and proof or problem into the theory."))

(defun oc=write2theory ()
  (let* ((proof  omega*current-proof-plan)
	 (prob  (prob~proof-problem proof))
	 (theo (prob~proof-theory proof))
	 (interface (comint~interface comint*current-comint)))
  (if (and (first (check~check proof ()))
	   (null (pds~open-nodes proof)))
      (progn (th~problem2theorem prob proof)
	     (th~write2theory prob)
	     (th~write-theorem-proofs proof)
	     (omega~message "Writing theorem ~A and proof ~A to theory ~A."
			    (keim~name prob) proof (keim~name theo)))
      (when (inter~prompt-for-input interface
				  (format nil "The proof is not complete. Write Problem ~A to
                                               the problem-file of theory ~A? (y/n)"
					       (keim~name prob) (keim~name theo))
				  'boolean)
            (th~write2theory prob)
	    (omega~message "Writing problem ~A to theory ~A." (keim~name prob) (keim~name theo))))))
			 
#{\subsection{Miscellaneous}#}

(com~defcommand remove-theories         
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega-basic theory)
  (function oc=remove-theories)
  (help "Remove all theories."))

#-logic-new(defun oc=remove-theories ()
  (let ((base (gethash "BASE" keim::th*theory-hash-table))
	(loaded-base))
    (with-hash-table-iterator (file keim::th*loaded)
			      (multiple-value-bind (a1 a2 a3)
				  (file)
				(declare (ignore a1 a3))
				(when (string-equal (file-namestring a2) "base.thy"))
				  (setf loaded-base a2)))
    (clrhash keim::th*theory-hash-table)
    (setf (gethash "BASE" keim::th*theory-hash-table) base)
    (clrhash keim::th*loaded)
    (setf (gethash loaded-base keim::th*loaded) t)
    (setq keim::pds*current-proof-plan nil)
    (setq omega*current-theory nil)
    (setq logic*current-theory omega*current-theory)
    (setq omega*current-proof-plan nil)
    (setf foci*proof-context nil)
    ))

#+logic-new(defun oc=remove-theories ()
	     (clrhash keim::th*theory-hash-table)
	     (clrhash keim::th*loaded)
	     (setq keim::pds*current-proof-plan nil)
	     (setq omega*current-theory nil)
	     (setq logic*current-theory omega*current-theory)
	     (setq omega*current-proof-plan nil)
	     (setf foci*proof-context nil)
	     )


;;;;; Expanding whole subproblems  

(defun oc=fully-expand-subproblem (concline)
  (oc=expand-subproblem concline (pds~node-supports concline) T))

;(com~defcommand expand-subproblem
;  (argnames concl supports equality)
;  (argtypes ndline ndline-list boolean)
;  (arghelps  "conclusion line" "A list of support-lines." "Expand Equality?")
;  (function oc=expand-subproblem)
;  (frag-cats atp) ; hier stand noch base
;  (defaults oc=expand-subproblem-defaults)
;  (log-p T)
;  (help "Expands all definitions in the conclusion line and in all of it's hypotheses."))
;
;(defun oc=expand-subproblem-defaults (concl supports equality)
;  (cond ((not (com~specified-arg-p concl))
;         (list (oc~default-current-planline) supports equality))
;        ((not (com~specified-arg-p supports))
;         (list concl
;               (when concl (pds~node-supports concl))
;               equality))
;        ((not (com~specified-arg-p equality))
;         (list concl supports T))
;        (t (list concl supports equality))))
;
;(defun oc=expand-subproblem (concline supports equality)
;  (let* ((defis (th~definitions-recursed
;                 (prob~theory omega*current-proof-plan)))
;         (line (when concline (gentac=defsi concline nil equality
;                                           (let ((formula (node~formula concline)))
;                           (remove-if-not #'(lambda (x) (if equality
;                                                            (data~position formula
;                                                                           #'(lambda (y)
;                                                                               (data~equal (th~definition-constant x)
;                                                                                           y)))
;                                                          (and (not (data~equal (th~definition-constant x)
;                                                                                (logic~equality-constant)))
;                                                               (data~position formula
;                                                                              #'(lambda (y)
;                                                                                  (data~equal (th~definition-constant x)
;                                                                                              y))))))
;                                                  defis)))))
;         (new-supps (mapcar #'(lambda (supportline)
;                                (gentac=defse nil supportline equality
;                                             (let ((formula (node~formula supportline)))
;                           (remove-if-not #'(lambda (x) (if equality
;                                                            (data~position formula
;                                                                           #'(lambda (y)
;                                                                               (data~equal (th~definition-constant x)
;                                                                                           y)))
;                                                          (and (not (data~equal (th~definition-constant x)
;                                                                                (logic~equality-constant)))
;                                                               (data~position formula
;                                                                              #'(lambda (y)
;                                                                                  (data~equal (th~definition-constant x)
;                                                                                              y))))))
;                                                  defis))))
;                            supports)))
;    (when line (oc=delete-support* (cadr line) supports))
;    (values line new-supps)))

(defun oc=expand-subproblem-defaults (concl supports prohib-defs)
  (cond ((not (com~specified-arg-p concl))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p supports))
	 (list concl
	       (when concl (pds~node-supports concl))
	       (com~unspecified)))
	((not (com~specified-arg-p prohib-defs))
	 (list concl supports (list (th~find-assumption '= (prob~theory
							    omega*current-proof-plan))
				    (th~find-assumption 'defined (prob~theory
								  omega*current-proof-plan))
				    (th~find-assumption 'equiv (prob~theory
								  omega*current-proof-plan)))))
	(t (list concl supports prohib-defs))))

(defun oc=expand-subproblem (concline supports prohib-defs &optional prohib-theory-list)
  (let* ((defs (orules=contained-definitions (node~formula concline)))
	 (prohib-theory-defs
	  (apply 'append (mapcar #'th~definitions prohib-theory-list)))
	 (all-prohib-defs (union prohib-defs prohib-theory-defs))
	 (new-concline
	  (if (term~schema-p (node~formula concline))
	      (let* ((domain (data~schema-domain (node~formula concline)))
		     (new-tvs (mapcar #'(lambda (var)
					  (declare (ignore var))
					  (type~generate-type-primitive-with-new-name 'tc 'type+constant
										      (pds~environment omega*current-proof-plan)))
				      domain)))
		     (second (tacl~apply 'kappai (list concline nil) (list new-tvs))))
	    concline))
	 (line (if (and defs
			(not (subsetp defs all-prohib-defs)))
		   (second (gentac=defsi new-concline nil all-prohib-defs))
		 new-concline))
	 (new-supps (mapcar #'(lambda (supportline)
				(let ((defs (orules=contained-definitions (node~formula supportline))))
				  (if (and defs (not (subsetp defs all-prohib-defs))) 
				      (first (gentac=defse nil supportline all-prohib-defs))
				    supportline)))
			    supports)))
;;;    (when line (oc=delete-support* (cadr line) supports))
    (values line new-supps)))

(defun oc=expand-default-subproblem ()
  ;;; jzimmer, 17.6.2005. Expands all definitions in a the current proof plan
  ;;; and returns the resulting open conjecture in OMDoc format.
  ;;; takes the default values without asking the user. For automatic use only!
  (let* ((concline (oc~default-current-planline))
         (supports (when concline (pds~node-supports concline)))
         (prohib-defs (list (th~find-assumption '= (prob~theory
                                                    omega*current-proof-plan))
                            (th~find-assumption 'defined (prob~theory
                                                          omega*current-proof-plan))
                            (th~find-assumption 'equiv (prob~theory
                                                        omega*current-proof-plan))))
         (prohib-theory-list nil)
         (defs (orules=contained-definitions (node~formula concline)))
	 (prohib-theory-defs
	  (apply 'append (mapcar #'th~definitions prohib-theory-list)))
	 (all-prohib-defs (union prohib-defs prohib-theory-defs))
	 (new-concline
	  (if (term~schema-p (node~formula concline))
	      (let* ((domain (data~schema-domain (node~formula concline)))
		     (new-tvs (mapcar #'(lambda (var)
					  (declare (ignore var))
					  (type~generate-type-primitive-with-new-name 'tc 'type+constant
										      (pds~environment omega*current-proof-plan)))
				      domain)))
		(second (tacl~apply 'kappai (list concline nil) (list new-tvs))))
	    concline))
	 (line (if (and defs
			(not (subsetp defs all-prohib-defs)))
		   (second (gentac=defsi new-concline nil all-prohib-defs))
		 new-concline))
	 (new-supps (mapcar #'(lambda (supportline)
				(let ((defs (orules=contained-definitions (node~formula supportline))))
				  (if (and defs (not (subsetp defs all-prohib-defs))) 
				      (first (gentac=defse nil supportline all-prohib-defs))
				    supportline)))
			    supports)))
    (let* ((open-nodes (pds~open-nodes omega*current-proof-plan))
           (theory (prob~proof-theory omega*current-proof-plan))
           (name (keim~name omega*current-proof-plan))
           ;;; hack because not all open nodes are really open
           (goals nil)
           (foobar 
            (mapcar #'(lambda (node)
                        (if (infer~open-p (just~method (node~justification
                                                        node)))
                            (setf goals (cons node goals))))
                    open-nodes))
           (goal (when goals (car goals)))
;;;           (res (when goal (tstp~prob2tstp goal)))
	   (adsf (format t "goal ~A" goal))
           (res (when goal (om~conjecture2omdoc goal theory name)))
           )
      (format t "~A" res)
      res)))


(defun oc=write-all-omdoc(theory-name)
  ;;; jzimmer, 17.6.2005. Writes all problems of a theory as OMDoc conjectures
  ;;; in files. 
  (let* ((theory (th~find-theory theory-name))
         (prob-names (mapcar #'keim~name  (th~problems (th~load-problems theory-name))))
         )
    (dolist (prob-name prob-names)
      (let* ((prob (th~find-problem prob-name theory-name))
             (foo (oc=prove-pre prob))
             (goal (oc~default-current-planline))
             (name (keim~name omega*current-proof-plan))
             (omdoc (when goal (om~conjecture2omdoc goal theory name)))
             (file (format nil "/home/jzimmer/import/MathServe/thirdParty/omega/xmp/set/~A.omdoc" name))
             )
        (with-open-file (stream file :direction :output :if-exists :supersede)
                        (format stream omdoc)
                        )))))
                                        ;(defun batac=expand-defsi (outline parameters)
;  (let* ((conc (first outline))
;         (conc-line (list nil conc))
;         (prec (second outline))
;         (defs (first parameters)))
;    (multiple-value-bind
;        (prec-eq tlist) (batac=substitute-definitions (node~formula conc) defs)
;      (declare (ignore prec-eq))
;      (tacl~init outline)
;      (let ((defsymbols (mapcar #'batac=get-defsymb tlist)))
;        (do* ((restlist defsymbols (cdr restlist))
;              (defsymb (car restlist) (car restlist)))
;            ((null restlist) t)
;          (let* ((def (th~find-assumption defsymb (prob~theory omega*current-proof-plan)))
;                 (definiendum (th~definition-constant def))
;                 (definiens (data~copy (th~ass-node def) :downto '(term+constant
;                                                                   type+primitive)))
;                 (poslist (data~substruct-positions definiendum (node~formula (cadr conc-line)))))
;            (format t "~% conc ~A" (node~formula (cadr conc-line)))
;            (format t "~% definiendum ~A" definiendum)
;            (format t "~% definiens ~A" definiens)
;            (format t "~% poslist ~A" poslist)
;            (setf conc-line
;                  (if (null (rest restlist))
;                      (let ((line
;                             (tacl~apply 'defni* (list (cadr conc-line) nil) (list
;                                                                             definiendum
;                                                                             definiens
;                                                                             poslist))))
;                        (tacl~apply 'lambda (list (cadr line) prec) nil))
;                    (tacl~apply 'defni* (list (cadr conc-line) nil) (list definiendum
;                                                                          definiens poslist)))))))
;      (tacl~end))))(defun batac=expand-defsi (outline parameters)
;  (let* ((conc (first outline))
;         (conc-line (list nil conc))
;         (prec (second outline))
;         (defs (first parameters)))
;    (multiple-value-bind
;        (prec-eq tlist) (batac=substitute-definitions (node~formula conc) defs)
;      (declare (ignore prec-eq))
;      (tacl~init outline)
;      (let ((defsymbols (mapcar #'batac=get-defsymb tlist)))
;        (do* ((restlist defsymbols (cdr restlist))
;              (defsymb (car restlist) (car restlist)))
;            ((null restlist) t)
;          (let* ((def (th~find-assumption defsymb (prob~theory omega*current-proof-plan)))
;                 (definiendum (th~definition-constant def))
;                 (definiens (data~copy (th~ass-node def) :downto '(term+constant
;                                                                   type+primitive)))
;                 (poslist (data~substruct-positions definiendum (node~formula (cadr conc-line)))))
;            (format t "~% conc ~A" (node~formula (cadr conc-line)))
;            (format t "~% definiendum ~A" definiendum)
;            (format t "~% definiens ~A" definiens)
;            (format t "~% poslist ~A" poslist)
;            (setf conc-line
;                  (if (null (rest restlist))
;                      (let ((line
;                             (tacl~apply 'defni* (list (cadr conc-line) nil) (list
;                                                                             definiendum
;                                                                             definiens
;                                                                             poslist))))
;                        (tacl~apply 'lambda (list (cadr line) prec) nil))
;                    (tacl~apply 'defni* (list (cadr conc-line) nil) (list definiendum
;                                                                          definiens poslist)))))))
;      (tacl~end))))

;;;;;;;;;;;;
;;; Default Agent stuff
;;;;;;;;;;;;




(com~defcommand activate-agents
  (function oc=set-agent-computation)
  (frag-cats omega)
  (help "Activates agent-based default computations."))

(defun oc=set-agent-computation ()
  (setf foci*in-use t)
  (unless (csm~active-p)
    (if (mixin~activep)
	(oc~start-agents-with-gui)
      (csm~start))))

(defun oc~start-agents-with-gui () 
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Initializes the suggestion mechanims directing the"
		    "output to the graphical user interface LOUI.")
	   (value   "Undefined."))
  (when (and (mixin~activep) (foci~in-use))
    (csm~start
     #'(lambda (x)
	 (omega~output "Suggestions: ~{       ~A~%~}~%" x)  ;;; temporary
	 (socket~write (format nil "updateSuggestions(~A)"
			       (parse~list-of-strings
				(mapcar #'(lambda (entry)
					    (keim~name (bb~entry-command entry)))
					x))) :inout)))))
;; MALTE
(com~defcommand deactivate-agents
  (function oc=deac-agents)
  (frag-cats omega)
  (help "Deactivates agent-based default computations."))

(defun oc=deac-agents ()
  (csm~quit)
  (mapcar #'(lambda (x) (when (agent~resources x)
			  (when (rsrc~log-flag (agent~resources x)) (agent~change-log-flag x))))
	      (agent~get-agents)))
 
(com~defcommand suspend-agents
  (function csm~suspend)
  (frag-cats omega)
  (help "Deactivates agent-based default computations."))

(com~defcommand resume-agents
  (function csm~resume)
  (frag-cats omega)
  (help "Deactivates agent-based default computations."))

(com~defcommand reset-agents
  (function sugg~reset)
  (frag-cats omega)
  (help "Resets agent-based default computations."))

(defmethod sugg~reset :before ()
  (when (and (mixin~activep) (csm~active-p) (not sod*vil))               ;;;MP: sod*vil from multi/sod-main.lisp
    (socket~write "updateSuggestions([\"\"])")))          ;;;I don't want to have this window with interactive methods

(com~defcommand set-command-agents
  (argnames commands)
  (argtypes command-list)
  (arghelps "A list of commands")
  (function csm~set-considered-commands)
  (defaults ((oc=set-command-agents-default)))
  (frag-cats omega)
  (help "Sets the list of command agents used for command suggestions."))

(defun oc=set-command-agents-default ()
  (mapcar #'bb~command
	  (remove-if-not #'bb~command-blackboard-p (bb~get-blackboards))))

(com~defcommand set-classifier-agents
  (argnames commands)
  (argtypes symbol-list)
  (arghelps "A list of classifier agents")
  (function csm~set-considered-commands)
  (defaults ((oc=set-classifier-agents-default)))
  (frag-cats omega)
  (help "Sets the list of classifier agents used for command suggestions."))

(defun oc=set-classifier-agents-default ()
  (mapcar #'agent~name (agent~get-classifiers)))

(com~defcommand set-command-heuristics
  (argnames heuristics)
  (argtypes symbol-list)
  (arghelps "A priority list of heuristics")
  (function oc=set-command-heuristics)
  (defaults ((oc=set-command-heuristics-default)))
  (frag-cats omega)
  (help "Sets the priority list of heuristics used for sorting command suggestions."))

(defun oc=set-command-heuristics (heuristics)
  (when (every #'symbolp heuristics)
    (setf bb*ordering-command-suggestion-leq
	  (mapcar #'oc=convert-symbol heuristics))))

(defun oc=set-command-heuristics-default ()
  (oc=heuristics-default 'bb~command-suggestion-leq-p))

(defun oc=convert-symbol (symbol)
  (when (symbolp symbol)
    (intern symbol (find-package :keyword))))

(defun oc=heuristics-default (predicate)
  (delete-duplicates
   (mapcar #'clos::eql-specializer-object
	   (remove-if-not #'clos::eql-specializer-p
			  (mapcar #'(lambda (method)
				      (third (sys~method-specializers method)))
				  (sys~generic-function-methods predicate))))))

(com~defcommand set-parameter-heuristics
  (argnames heuristics)
  (argtypes symbol-list)
  (arghelps "A priority list of heuristics")
  (function oc=set-parameter-heuristics)
  (defaults ((oc=set-parameter-heuristics-default)))
  (frag-cats omega)
  (help "Sets the priority list of heuristics used for sorting parameter suggestions."))

(defun oc=set-parameter-heuristics (heuristics)
  (when (every #'symbolp heuristics)
    (setf bb*ordering-parameter-suggestion-leq
	  (mapcar #'oc=convert-symbol heuristics))))

(defun oc=set-parameter-heuristics-default ()
  (oc=heuristics-default 'bb~parameter-suggestion-leq-p))


(com~defcommand set-agents-defaults
  (argnames theory)
  (argtypes existing-theory)
  (arghelps "A theory")
  (function th~agent-default)
  (defaults ((oc=default-theory)))
  (frag-cats omega)
  (help "Sets the priority list of heuristics used for sorting parameter suggestions."))

(defun oc=agents-restart (&key (theo-default nil))
  (let ((olevel sugg*output)
	(auto auto*in-use))
    ;(sugg~set-output 0)
    (oc=deac-agents)
    (when theo-default (th~agent-default theo-default))
    (oc~start-agents-with-gui)
    (sugg~set-output olevel)))
    
;; Malte
(com~defcommand use-standard-heuristics
  (function oc=use-standard-heuristics)
  (frag-cats omega)
  (help "Set the default standard heuristics."))


(defun oc=use-standard-heuristics ()
  (csm~set-default-heuristics)
  (auto~set-criteria 'standard)
  (agent~init-resources))

(com~defcommand agents-status
  (function csm~status)
  (frag-cats omega)
  (help "The status of the suggestion mechanism."))

(com~defcommand agent-output
  (argnames level)
  (argtypes integer)
  (arghelps "Use Default Agents in verbose mode")
  (function sugg~set-output)
  (frag-cats omega)
  (defaults (2))
  (help "Distributed computation of Command suggestions."))

(com~defcommand apply-suggestion
  (argnames number)
  (argtypes integer)
  (arghelps "Apply nth suggestion")
  (function oc=apply-suggestion)
  (frag-cats omega)
  (defaults (1))
  (help "Applys the nth suggestion of the current suggestion blackboard entries."))

(defun oc=apply-suggestion (&optional (n 1))
  (when (and (csm~active-p) (>= n 1))
    (let ((suggestion (nth (1- n) (bb~entries (agent~surveyed-bb csm*suggestion-agent)))))
      (when suggestion
	(multiple-value-bind (arguments command)
	    (bb~pop-entry (list suggestion))
	  (comint~apply-command2arguments command arguments))))))

;; MALTE
;; A new apply-suggestion-function, which is able to apply not only the
;; best possible argument instantiation to a command.
(com~defcommand apply-sugg 
  (argnames n m)
  (argtypes number number)
  (arghelps "Apply nth suggestion" "Apply mth argument instantiation")
  (function oc=apply-sugg)
  (frag-cats omega)
  (defaults (1 1))
  (help "Applys the nth suggestion of the current suggestion blackboard entries."))

(defun oc=apply-sugg (&optional (n 1) (m 1))
  (let ((cbb NIL))
  (when (and (csm~active-p) (>= n 1) (>= m 1))
    (let ((suggestion (nth (1- n) (bb~entries (agent~surveyed-bb csm*suggestion-agent)))))
      (when suggestion
	(multiple-value-bind (arguments command)
	    (bb~pop-entry (list suggestion))
	  (setf cbb (bb~find-blackboard command))
	  (let* ((as (nth (1- m) (bb~entries cbb)))
		 (sug (bb~create-command-suggestion command as NIL)))
	    (multiple-value-bind (args comm)
		(bb~pop-entry (list sug))
	      (comint~apply-command2arguments command args)))))))))



(com~defcommand recompute-defaults
  (argnames command arg-list)
  (argtypes command string-list)
  (arghelps "A command" "A list of strings")
  (function oc=recompute-defaults)
  (frag-cats omega)
  (help "Activates agent-based default computations."))

(defun oc=recompute-defaults (command arg-list)  ;;; has to be redone   VS
  (let* ((com-arg-types (com~argtypes command))
	 (new-arg-list (if (>= (length arg-list) (length com-arg-types))
			   arg-list
			 (append arg-list (make-list (- (length com-arg-types) (length arg-list)))
				 :initial-element "NIL")))
	 (real-args (mapcar #'(lambda (type arg)
				(multiple-value-bind (value success)
				    (ignore-errors (arg~read-type type
								  (if (null arg) arg
								    (read-from-string arg))))
				  (if success
				      (return-from oc=recompute-defaults
					(progn (omega~error "Wrong argument: ~A is not of type ~A" arg type)
					       (parse~symbol "false")))
				    value)))
			    com-arg-types
			    new-arg-list))
	 (method (format nil "newDefaults(~a)"
			 (parse~list (mapcar #'parse~list-of-atoms
					     (mapcar #'parse~arguments
						     (bb~reset command real-args)))))))
    (socket~write method :inout)))

(com~defcommand set-active-focus      
  (argnames planline)
  (argtypes ndline)
  (arghelps "A open line specifying a new open focus context.")
  (frag-cats omega)
  (defaults ((oc~default-current-planline)))
  (function oc=set-active-focus)
  (help "Sets the focus planline is belonging to to be the new active focus context."))

(defun oc=set-active-focus (line)
  (foci~set-active-focus line)
  (unless sugg*quit  (sugg~reset)))

#|  currently not in use  VS
(com~defcommand set-agent-complexity-level     
  (argnames int)
  (argtypes integer)
  (arghelps "An integer specifying the complexity for the agent based default suggestion mechansim,"
	    "i.e. only agents with a complexity level below this level can get active.")
  (function oc=set-agent-complexity-level)
  (frag-cats omega)
  (defaults (dflt*complexity-level))
  (help "An integer specifying the complexity for the default suggestion mechansim,"
	"i.e. only agents with a complexity level below this level can get active."))

(defun oc=set-agent-complexity-level (num)
  (setf dflt*complexity-level num)
  (oc=print-activated-deactivated-agents))

(defun oc=print-activated-deactivated-agents ()
  (let* ((agent-list (mapcar #'dflt~level (dflt~all-agents)))
	 (active (count-if #'(lambda (x) (<= x dflt*complexity-level)) agent-list))
	 (retired (- (length agent-list) active)))
    (omega~message "Active parameter agents:  ~A" active)
    (omega~message "Retired parameter agents: ~A" retired)))

|#

(com~defcommand load-agents      
  (argnames agent-type)
  (argtypes number)
  (arghelps "Which agents do you want to load? (1) OMEGA standard agents (2) NIC agents (3) External systems agents (4) Set demo (5) ZMZ (6) Homo")
  (frag-cats omega)
  (function oc=load-agents)
  (help "Loading the suggestion agents."))

(defun oc=load-agents (number)  ;;; needs to be changed, when we know where the
				;;; agents go. VS,CS
  (cond ((= number 1)
	 (omega~message "Loading OMEGA standard agents for theory base.")
	 (load (concatenate 'string (*user-top-dir*)
                            "keim-3/prog/suggestion/thy/test-agents.thy"))
	 (agent~init-resources))
	((= number 2)
	 (omega~message "Loading NIC agents.")
	 (load (concatenate 'string (*user-top-dir*)
			    "omega-3/prog/oants/nic-pl.thy"))
	 (load (concatenate 'string (*user-top-dir*)
			    "omega-3/prog/oants/nic-fo.thy"))
	 (agent~init-resources))
	((= number 3)
	 (omega~message "Loading External systems agents.")
	 (load (concatenate 'string (*user-top-dir*)
			    "omega-3/prog/oants/extern.thy"))
	 (agent~init-resources))
	((= number 4)
	 (omega~message "Loading External systems agents.")
	 ;; just in order to make sure that everything is well defined; should be removed later
	 ;; (oc=remove-theories)
	 ;; (csm~erase)
	 (oc=deac-agents)
	 (load "~/omega/omega-3/prog/oants/extern.thy")
	 (nic~use-nic-agents)
	 (oc=stop-use-resources)
	 (auto~set-auto-default-interval 10)
	 (nic~use-nic-heuristics)
	 (nic~add-command-agents '(solved-by-pl-atp counterexample-by-satchmo))
	 ;; (oc=set-agent-computation)
	 )
	((= number 5)
	 (omega~message "Loading ZMZ agents.")
	 (th~load-agents 'zmz)
	 (agent~init-resources))
	((= number 6)
	 (omega~message "Loading homo agents.")
	 (th~load-agents 'homo)
	 (agent~init-resources))
	))


(com~defcommand erase-agents
  (frag-cats omega)
  (function csm~erase)
  (help "Erase all agents."))

;;;
;;;  Tracing stuff for debugging
;;;

(com~defcommand trace-bb
  (argnames blackboard)
  (argtypes symbol)
  (arghelps "The blackboard to be traced")
  (function oc=trace-bb)
  (frag-cats omega)
  (help "Trace a blackboard."))

(defun oc=trace-bb (bb)
  (if (null bb)
      (apply #'sugg~trace (bb~get-blackboards))
    (sugg~trace (bb~find-blackboard bb))))
		
(com~defcommand untrace-bb
  (argnames blackboard)
  (argtypes symbol)
  (arghelps "The blackboard to be untraced")
  (function oc=untrace-bb)
  (frag-cats omega)
  (help "Trace a blackboard."))

(defun oc=untrace-bb (bb)
  (if (null bb)
      (apply #'sugg~untrace (bb~get-blackboards))
    (sugg~trace (bb~find-blackboard bb))))
		
(com~defcommand trace-agents
  (argnames command)
  (argtypes symbol)
  (arghelps "The command whose agents are to be traced")
  (function oc=trace-agents)
  (frag-cats omega)
  (help "Traceing the agents of a command."))

(defun oc=trace-agents (com)
  (apply #'sugg~trace (agent~command2parameter-agents com)))
		
(com~defcommand untrace-agents
  (argnames command)
  (argtypes symbol)
  (arghelps "The command whose agents are to be untraced")
  (function oc=untrace-agents)
  (frag-cats omega)
  (help "Untraceing the agents of a command."))

(defun oc=untrace-agents (com)
  (apply #'sugg~untrace (agent~command2parameter-agents com)))

;(com~defcommand use-foci 
;  (argnames flag)
;  (argtypes boolean)
;  (arghelps "Use Default- and Focus mechanism.")
;  (function oc=foci-activate)
;  (frag-cats omega)
;  (defaults (t))
;  (log-p T)
;  (help "Distributed computation of Command suggestions."))
;
;
;(defun oc=foci-activate (bool)
;  (declare (ignore bool))
;  (oc=load-agents))
;
;oc=load-agents needs 1 arg!  

(com~defcommand nic-initialize 
  (argnames )
  (argtypes )
  (arghelps )
  (function nic~initialiaze-substitution-hashtable)
  (frag-cats omega)
  (defaults )
  (log-p T)
  (help "Initialize substitution hashtable for NIC calculus."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OMEGA HELP commands 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand apropos
  (function ohlp~apropos)
  (argnames Something)
  (argtypes string)
  (arghelps "Apropos what?")
  (frag-cats omega direct-display help)
  (defaults)
  (help "Searches for commands related to a given string."))

(com~defcommand apropos*
  (function ohlp~apropos*)
  (argnames Something)
  (argtypes string)
  (arghelps "Apropos what?")
  (frag-cats omega direct-display help)
  (defaults)
  (help "Searches for subjects related to a given string."))

(com~defcommand help
  (function ohlp~help)
  (argnames Something)
  (argtypes string)
  (arghelps "Help on what?")
  (frag-cats omega direct-display help)
  (defaults)
  (help "Gives help on a specified subject."))

(com~defcommand help-index
  (function ohlp~index)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega direct-display help)
  (defaults)
  (help "Returns a list of all available help topics."))

(com~defcommand help-sections
  (function ohlp~sections)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega direct-display help)
  (defaults)
  (help "Returns a list of all available help sections."))

(com~defcommand help-topics
  (function ohlp~topics)
  (argnames Section)
  (argtypes string)
  (arghelps "An overview of which section?")
  (frag-cats omega direct-display help)
  (defaults)
  (help "Returns a list of all topics available in the help sections."))

;;; recover the old command functionality
(com~defcommand show-categories
  (function ohlp~show-categories)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega direct-display)
  (defaults)
  (help "Displays the categories available in the current command interpreter."))

(com~defcommand show-fragments
  (function ohlp~show-fragments)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega direct-display)
  (defaults)
  (help "Displays the fragments available in the current command interpreter."))

(com~defcommand show-commands
  (function ohlp~show-commands)
  (argnames Category)
  (argtypes existing-category-name)
  (arghelps "Name of a category")
  (frag-cats omega direct-display)
  (defaults ((oc=default-category)))
  (help "Displays the categories available in the current command interpreter."))

(defun oc=default-category ()
  (keim~name comint*current-comint))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Dynamic command loading facility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand show-command-files
  (function cld~display-directory)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega direct-display)
  (defaults )
  (help "Displays command files that can be loaded into the OMEGA command interpreter."))

(com~defcommand load-command-file
  (function cld~load-command-file)
  (argnames file)
  (argtypes string)
  (arghelps "A command file to be loaded.")
  (frag-cats omega-basic file-io)
  (defaults ((oc=load-command-file-default)))
  (help "Loads a command file into the OMEGA command interpreter."))

(defun oc=load-command-file-default ()
  (car (cld~command-files)))

(com~defcommand load-command-file*
  (function oc=load-command-file*)
  (argnames files)
  (argtypes string-list)
  (arghelps "A list of command files to be loaded.")
  (frag-cats omega-basic file-io)
  (defaults ((cld~command-files)))
  (help "Loads a list of command files into the OMEGA command interpreter."))

(defun oc=load-command-file* (list)
  (mapc #'cld~load-command-file list))

(com~defcommand load-command-registry
  (function cld~load-registry-files)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic file-io)
  (defaults ((cld~command-files)))
  (help "Loads the content of the command registry into the OMEGA command interpreter."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Testing OMEGA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(com~defcommand test-omega
  (function oc=omega~test)
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic)
  (defaults )
  (help "Loads the content of the command registry into the OMEGA command interpreter."))

(defun oc=omega~test ()
  (let* ((examples-dir  *omegahome*)
	 (examples-file (cond (examples-dir (concatenate 'string examples-dir
							  "omega-3/examples/test-omega.rpy"))
			      ((probe-file
				"/project/omega/omega-3/examples/test-omega.rpy")
			       "/project/omega/omega-3/examples/test-omega.rpy")
			      (t "test-omega.rpy"))))
    (if (probe-file examples-file)
	(multiple-value-bind (sec min hour day month year)
	    (decode-universal-time (get-universal-time))
	  (sys~chdir examples-dir)
	  (omega~message "*****************************************************************************************")
	  (omega~message " This is an automatic test generated by OMEGA at ~A:~A:~A on ~A/~A/~A."
			 hour min sec month day year) 
	  (omega~message " Executing (recursive) replay file ~A" examples-file)
	  (omega~message "*****************************************************************************************")
	  (keim::ccom=execute-log examples-file)
	  (omega~message "*****************************************************************************************")
	  (omega~message " End of automatic test output")
	  (omega~message
	   "*****************************************************************************************"))
      (progn
	(omega~warn "Replay file for automatic test of OMEGA (~A) not found" examples-file)
	(omega~warn "This might be caused by a wrong setting of the environmant variable EXAMPLESHOME.")))))


;;;; comment or dummy command 

(com~defcommand comment
  (argnames list)
  (argtypes string-list) 
  (arghelps "Anything to comment")
  (frag-cats comint)
  (function ccom=comment)
  (help "Does nothing but printing a message."))
    
(defun ccom=comment (comment)
  (let ((string ""))
    (dolist (comi comment)
      (setq string (concatenate 'string string " " comi)))
    (omega~message "~A" string)))

(defmethod th~prove (proof-plan)
  (oc=prove proof-plan))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions for commands in mbase.com   MP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=mbase-enter ()
  (when (mbase~enter)
    (setf th*current-db 'mbase)	
    (format t "Changed current theory database to MBase.")))	

(defun oc=mbase-leave ()
  (setf th*current-db nil)
  (format t "Changed current theory database to Omega.")
  (mbase~leave))

(defun oc=mbase-restart ()
  (when (mbase~restart)
    (setf th*current-db 'mbase)	
    (format t "Restarted MBase service.")))	

(defun oc=add-theory-path (path)
  (setf *theory-registry* (cons path *theory-registry*)))

(defun oc=change-theory-path (pathlist)
  (setf *theory-registry* pathlist))

(defun oc=show-theory-path (pathlist)
  (declare (ignore pathlist))
  (list (mapcar #'(lambda (path) (concatenate 'string "\"" (if (typep path 'pathname)
							       (namestring path)
							     path) "\"")) *theory-registry*)))

(defun oc=import-ass (assumption)
  (pds~add-thy-assertion assumption omega*current-proof-plan)
  (when (foci~in-use)
    (foci~compute-pcs)
    (unless sugg*quit  (sugg~reset))))


(defun oc=import-ass* (assumptions)
  (mapc #'oc=import-ass assumptions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions and commands for debugging.   MP (stolen from JZ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand trace-planner 
  (argnames trace)
  (argtypes boolean)
  (arghelps "T to trace the planner, else nil")
  (frag-cats omega-basic comint-state)
  (function  oc=trace-planner)
  (defaults ((not plan*trace)))
  (log-p nil)
  (help "Turn on/off the tracing of the planner."))

(defun oc=trace-planner (trace)
  (setf plan*trace trace))

(com~defcommand trace-controlrules
  (argnames trace)
  (argtypes boolean)
  (arghelps "T to trace the control-rules, else nil")
  (frag-cats omega-basic comint-state)
  (function  oc=trace-controlrules)
  (defaults ((not cri*verbose)))
  (log-p nil)
  (help "Starts/stops verbose output that shows set of methods that are selected by the control-rules."))

(defun oc=trace-controlrules-default ()
  (not cri*verbose))

(defun oc=trace-controlrules (trace)
  (setf cri*verbose trace))

(com~defcommand trace-methodconditions
  (argnames trace)
  (argtypes boolean)
  (arghelps "T to trace the check of the method-conditions, else nil")
  (frag-cats omega-basic comint-state)
  (function  oc=trace-methodconditions)
  (defaults ((oc=trace-methodconditions-defaults)))
  (log-p nil)
  (help "Turn on/off the tracing for the applicability of methods."))

(defun oc=trace-methodconditions-defaults ()
  (if (member 'meth=check-condition (trace))
      nil
    T))

(defun oc=trace-methodconditions (trace)
  (if trace
      (trace meth=check-condition)
    (untrace meth=check-condition)))


(com~defcommand trace-methodfunction
  (argnames function)
  (argtypes string)
  (arghelps "Give a method function that should be traced")
  (frag-cats omega-basic comint-state)
  (function  oc=trace-methodfunction)
  (log-p nil)
  (help "Turn on/off the tracing for particular method functions."))

(defun oc=trace-methodfunction (function)
  (let ((func (read-from-string (concatenate 'string "meth=defn-" function))))
    (if (find func (trace))
	(eval `(untrace ,func))
      (eval `(trace ,func)))))



;;; Maltes Omega Commands

(com~defcommand reset-resources
  		(function agent~reset-resources)
		(frag-cats omega)
		(help "Sets all the agents resource objects to init-values."))


(com~defcommand start-res-log
		(argnames arg)
		(argtypes symbol)
		(function oc=start-res-log)
		(frag-cats omega)
		(defaults ())
		(help "Begins recording the object resources for the given agents."
		      "COMMANDS --- start recording resources for command agents"
		      "ALL      --- start recording resources for all agents"
		      "<agent-name> start recording resources for given agent."))

(defun oc=start-res-log (&optional (a 'commands))
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "A symbol.")
	   (effect  "Begins recording the object resources for the given agents."
		      "COMMANDS --- start recording resources for command agents"
		      "PARAMETERS - start recording resources for parameter agents"
		      "ALL      --- start recording resources for all agents"
		      "<agent-name> start recording resources for given agent.")
	   (value   "None."))
  (cond ((string-equal a 'commands)
	 (mapcar #'(lambda (x) (unless (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-command-agents)))
	((string-equal a 'all)
	 (mapcar #'(lambda (x) (unless (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-agents)))
	((string-equal a 'parameters)
	 (mapcar #'(lambda (x) (unless (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-parameter-agents))
	(t (let ((agent (agent~get-agent a)))
	     (if agent (if (rsrc~log-flag agent)
			   (format t "Already recording resource info for agent ~A"
				   (agent~name agent))
			 (agent~change-log-flag agent))
	       (format t "agent does not exists.")))))))

(com~defcommand stop-res-log
		(argnames arg)
		(argtypes symbol)
		(function oc=stop-res-log)
		(frag-cats omega)
		(defaults ())
		(help "Stops recording the object resources for the given agent."))


(defun oc=stop-res-log (&optional (a 'commands))
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "A symbol.")
	   (effect  "Stops recording the object resources for the given agents."
		      "COMMANDS --- stops recording resources for command agents"
		      "PARAMETERS - stops recording resources for parameter agents"
		      "ALL      --- stops recording resources for all agents"
		      "<agent-name> stops recording resources for given agent.")
	   (value   "None."))
  (cond ((string-equal a 'commands)
	 (mapcar #'(lambda (x) (when (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-command-agents)))
	((string-equal a 'all)
	 (mapcar #'(lambda (x) (when (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-command-agents)))
	((string-equal a 'parameters)
	 (mapcar #'(lambda (x) (when (rsrc~log-flag (agent~resources x))
				 (agent~change-log-flag x))) (agent~get-parameter-agents)))
	(t (let ((agent (agent~get-agent a)))
	     (if agent (when (rsrc~log-flag agent) (agent~change-log-flag agent))
	       (format t "agent does not exists."))))))


(com~defcommand write-logs
 		(argnames arg)
		(argtypes symbol)
		(arghelps "For which agents? The options are: 'commands 'all 'single-agent")
		(function co=write-logs)
		(frag-cats omega)
		(defaults ('commands))
		(help "Writes resource information to file."))


;; das ist noch ein bisschen Scheisse !!
;; muss auch fuer einzelnen Agenten moeglich sein.
(defun co=write-logs (arg)
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "A symbol.")
	   (effect  "Writes the recorded resource informations from the specified"
		    "agents to a file."
		    "command -- treats command agents only."
		    "all     -- write resources of all agents to file.")
	   (value   "None."))
  (cond ((string-equal arg 'commands) (agent~write-logs 'commands))
	((string-equal arg 'all) (agent~write-logs 'all))))



;; to be combined with rite-logs !!!
(com~defcommand write-res-log
		(argnames arg)
		(argtypes symbol)
		(arghelps "For which agents? The options are: 'commands 'all 'single-agent")
		(function oc=write-res-log)
		(frag-cats omega)
		(defaults ('commands))
		(help "Writes resource information from agent to file."))

;; to be combined with rite-logs !!!
(defun oc=write-res-log (name)
  (let ((agent (agent~get-agent name)))
  (if agent (progn (agent~write-res-log agent)
		   (sugg~output agent 2 "Wrote resource history from agent ~A" (agent~name agent)))
    (format t "Agent does not exists."))))



(com~defcommand use-resources
  		(function oc=use-resources)
		(frag-cats omega)
		(help "Tells system to use resource adaption."))

(defun oc=use-resources ()
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Tell system to use the resource mechanism.")
	   (value   "None."))
  (setf rsrc*use-resources T)
  (print "Agents now resource adaptive !"))


(com~defcommand stop-use-resources
		(function oc=stop-use-resources)
		(frag-cats omega)
		(help "Tells system to stop using resource adaption."))

(defun oc=stop-use-resources ()
  (declare (edited  "21-MAY-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Stops using the resource mechanism.")
	   (value   "None."))
  (setf rsrc*use-resources NIL)
  (print "Agents now NOT resource adaptive !"))

;; ------------------------------------------
;; some functions which are useful for demos
;; ------------------------------------------


(com~defcommand show-resources
		(argnames arg)
		(argtypes symbol)
		(arghelps "For which agents ? The options are 'all 'commands <agent-name>")
		(function oc=show-resources)
		(frag-cats omega)
		(defaults ('all))
		(help "Prints the contents of the resource object."))

(defun oc=show-resources (arg)
  (cond ((string-equal arg 'all)
	 (mapcar #'(lambda (x) (format t "~A~%" (agent~resources x)))
		 (agent~get-agents)))
	((string-equal arg 'commands)
	 (mapcar #'(lambda (x) (format t "~A~%" (agent~resources x)))
		 (agent~get-command-agents)))
	(t (format t "~A~%" (agent~resources (agent~get-agent arg))))))
	
;;;;;MP Commands for Matejas learn-stuff

(com~defcommand pds2outline
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function lea=extract-tolearn)
  (defaults )
  (Log-p T) 
  (help "Shows the OUTLINE (in the sense of Mateja Learning project) of the
current pds."))

;;I use this for the demo and until I include the things in the system
(com~defcommand load-learnOmatic
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function oc=load-learnOmatic)
  (defaults )
  (Log-p T) 
  (help "Load (and compile) the files of Mateja's learnOmatic plus the given file."))

(defun oc=load-learnOmatic ()
  (unless (fboundp 'lea=next)
    (let ((dir (user::append-directories (*user-top-dir*) "omega-3/prog/learning")))
      (compile-file (make-pathname :directory dir :name "learn" :type "lisp"))
      (mapc #'(lambda (name)(load (make-pathname :directory dir :name name)))
	    (list "learn.fasl" "mateja.lisp" "setlearn.thy" "tryandlearn.thy")))))


(com~defcommand shutdown
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic)
  (function oc=shutdown)
  (log-p T)
  (help "Shutdown all servers and exit lisp"))

(defun oc=shutdown ()
  (prog1 T
    (proc~create :name "SHUTDOWN-PROCESS"
                 :priority 100
                 :function #'(lambda () (sleep 2) (sys~exit-from-lisp)))))


(com~defcommand clausify
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats omega-basic)
  (function oc=clausify)
  (log-p T)
  (help "Clausify the current proof problem."))

(defun oc=clausify ()
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
;; 	 (res-proof (atptop~resolution-proof-create-from-pds-open-node open-node
         ;;                                                                        omega*current-proof-plan))

         (res-proof (tstp~create-tstp-problem open-node omega*current-proof-plan))
         )
    (let (;; (tstpcnf (tstp~prob2xtstp res-proof))
	  (tstpcnf (tstp~prob2tstp res-proof))
	  (delta (with-output-to-string (delta-stream)
					(post~print (res~proof-delta-relation res-proof)
						    delta-stream)))
;; 	  (cnfproof (with-output-to-string (cnf-stream)
;; 					   (post~print res-proof
          ;; 						       cnf-stream)))
	  )
      (format t "~A" tstpcnf)
      ;;      (format t "~A" delta)
      ;;      (format t "cnf: ~A" cnfproof)
      (list tstpcnf delta));;jzimmer: re-introduced it because we can
    ;;handle multiple outputs now. 30.3.2005
    ;;      tstpcnf)  ;;jzimmer: don't need deltarlation anymore because Andreas can reconstruct
    ;;it, 15.3.2004
    ))

