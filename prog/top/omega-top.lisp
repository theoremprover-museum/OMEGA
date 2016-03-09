;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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
 
(mod~defmod omega :uses (mod sys inter com asi mixin comint arg)
	    :documentation "Definition of OMEGA top level."
	    :exports (omega~top
		      omega~loui
		      ))

;;;;; OMEGA top level

;;; We define a simple top level for OMEGA using the ASI+INTER interface.

;;; First we define a condition which will be signaled when we want to leave
;;; the OMEGA top level, OMEGA+LEAVE-OMEGA, which inherits from SYS+ABORT.
;;; While we are running OMEGA, we keep some global variables.

(defvar omega*current-databases nil)
(defvar omega*current-theory nil)
(defvar omega*current-db nil)
(defvar omega*current-command nil)
(defvar omega*current-proof-plan nil)
(defvar omega*current-ndstyle 'pds-pretty)
(defvar omega*current-tp-dir nil)
(defvar omega*current-resolution-proof nil)

(defvar omega*top-level nil "The OMEGA top level command interpreter.")

(defun omega~release-top-level ()
  (setf omega*top-level nil))

(defun omega~top (&optional (interface))
  (declare 
   (authors hess sorge nesmith)
   (input "An interface (creates one of type ASI+INTER by default)")
   (value "Undefined")
   (effect "After initialization, runs a read-eval-print loop,"
	   "reading a command, getting its arguments and applying the command."))
  (when omega*top-level (return-from omega~top (omega~warn "OMEGA top level already active.")))
  (let* ((omega*current-db nil)
	 (omega*current-databases nil)
	 (omega*current-theory nil)
	 (omega*current-ndstyle omega*current-ndstyle)
	 (omega*current-tp-dir nil)
	 (inter (if interface
		    interface
		  (asi~create)))
;         (inter (if interface
;                    interface
;                  (if (find :gui *features*)
;                      (ozi~create)
;                    (asi~create))))
	 ;;         (logfORnil (omega=ask-for-logfile inter)) 
	 ;;         (logfile (if logfORnil (oc~create-file&evtl-rename-oldfile logfORnil)))
	 (comint (comint~create "OMEGA"
				;;;(list (com~find-fragment 'comint) (com~find-fragment 'omega))
				;;;Extending the commands executable in the command interpreter
				;;;OMEGA with the commands of the new fragments below.
				(remove-if #'null
					   (list (com~find-fragment 'comint)
						 (com~find-fragment 'omega)
						 (com~find-fragment 'rules)
						 (com~find-fragment 'tactics)
						 (com~find-fragment 'planning)
						 (com~find-fragment 'atp)
						 (com~find-fragment 'extern)
						 (com~find-fragment 'tps)
						 (com~find-fragment 'no-gui )
						 (com~find-fragment 'presentation)
						 (com~find-fragment 'mbase)
						 (com~find-fragment 'proof-view)
						 (com~find-fragment 'mycas)
						 (com~find-fragment 'leo)
						 (com~find-fragment 'leo-interactive)
						 (com~find-fragment 'verify)
						 (com~find-fragment 'analogy)
						 (com~find-fragment 'examples)
						 ))
		                inter
				nil)))
    ;; logfile)))
    ;;; Setting important global variables.
    (setf omega*top-level comint)
    (setf tpl:*reset-hook* #'omega~release-top-level)
    (setf comint*current-comint comint)
    (setf omega*current-proof-plan (if omega*current-proof-plan
				       omega*current-proof-plan
				     keim::pds*current-proof-plan))
    ;;; The rest is the actual intialization phase of OMEGA.
    (let ((current-dir (sys~current-directory))
	  (examples (sys~current-directory))) ;;(sys~getenv 'EXAMPLESHOME)))
      (when examples (sys~chdir examples))
      (opr~initialize-omega)
      (opr~initialize-listeners)
      (cld~load-new-registry-files)
      (format t "~% rpc*java-server ~A~%" ags::rpc*java-server-host)
      (if ags::rpc*java-server-host
          (let* ((sender (gentemp 'temp))
                 (methodname "ready")
                 (message (rpc~compose-methodcall methodname  nil))
                 )
            (socket~define sender)
            (socket~connect ags::rpc*java-server-host ags::rpc*java-server-port sender)
            (format t "~%sending ready message to java now: ~A" message)
            (http~send-request sender
                               message 
                               :uri "/rpc2")))
      (comint~top comint)
      (sys~chdir current-dir)
      (values))))

#| Commented the following as it is definitly annoying. VS.
(defun omega=ask-for-logfile (interface)
  (declare (edited  "15-NOV-1995")
	   (authors Lassaad)
	   (input  "an INTERFACE" )
	   (effect "Asking the user for setting the default logfile."
		   "This can be set to NIL when the default logmode should be nil.")
	   (value  "The path name of the logfile or nil."))

  (inter~output-object interface "Starting the command interpreter for OMEGA ...")
  (inter~terpri interface)
  (when (inter~prompt-for-input interface
				"Setting logging mode? (y/n)"
				(arg~find-argtype 'boolean))
    (omega=get-default-logfile interface)))
   

(defun omega=get-default-logfile (interface)
  (declare (edited  "15-NOV-1995")
	   (authors Lassaad)
	   (input  "an INTERFACE")
	   (effect "Asking the user whether to use the default path name of the log"
		   "file or to choose a new path name.")
	   (value  "The path name of the logfile." ))

  (inter~prompt-for-input-with-default interface
				       "The default path name of the log file "
				       (arg~find-argtype 'pathname)
				       (oc~default-logfile)))

|#

(defun omega~loui ()
  (declare (edited  "12-AUG-1998")
	   (authors Hess)
	   (input  "Nothing.")
	   (effect "Creates an mixed interface.")
	   (value  "None." ))
  (omega~top))
  ;;;(omega~top (mixin~create)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefined function from the KEIM comint module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric comint~apply-command2arguments (command arguments)
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "A command and a list of arguments.")
	   (effect  "Applys the command to arguments.")
	   (value   "Undefined."))
  (:method ((command com+command) (arguments list))
	   (when (and (not (opr~active-p)) opr*not-killed)
	     (opr~initialize-omega))
	   (if (opr~active-p)
	       (progn 
		 (opr~enqueue-command
		  (opr~normalize-command command :args arguments :process opr*lisp-listener))
		 (opr~arrest-listener))
	     (apply (com~function command) arguments))))
	

;;; Revision testing!!!
