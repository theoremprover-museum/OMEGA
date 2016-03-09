;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
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

(mod~defmod loui
	    :uses (mod serv)
	    :documentation "LISP-functions using the loui service."
	    :exports  (loui~enter
                       loui~call
                       loui~leave
                       loui~restart

                       loui~error-number
                       loui~error-message

                       loui~signal-interface-errors-on  ; this is the default
                       loui~signal-interface-errors-off 
                       
                       loui~signal-errors-on 
                       loui~signal-errors-off ; this is the default

                       loui~load-url
		       loui~query
		       loui~casequery
                       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;    errors    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar loui*signal-interface-errors T)  ; corresponds to serv~signal-errors
(defvar loui*signal-errors nil)         

(defun loui~error? ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "T iff the last service-operation returned an error. "))
  (serv~service-error?))

(defun loui~error-number ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The original loui-service-error-number. "))
  (serv~service-error-number))

(defun loui~error-message ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The error message. "))
  (serv~error-message))

(defun loui~signal-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "loui*signal-errors is set to T.")
           (value   "T."))
  (setq loui*signal-errors T)
  T)

(defun loui~signal-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "loui*signal-errors is set to nil.")
           (value   "T."))
  (setq loui*signal-errors nil)
  T)

(defun loui~signal-interface-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "loui*signal-interface-errors is set to T.")
           (value   "T."))
  (setq loui*signal-interface-errors T)
  T)

(defun loui~signal-interface-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "loui*signal-interface-errors is set to nil.")
           (value   "T."))
  (setq loui*signal-interface-errors nil)
  T)

(defun loui=signal-interface-error (function-name)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "An interface error occured while executing function ~A.~%~A: ~A"
          function-name (serv~error-number) (serv~error-message)))

(defun loui=signal-error (function-name method)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "In Function ~A: The Loui-service could not apply method '~A' successfully.~%~A: ~A"
          function-name method
          (loui~error-number) (loui~error-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  enter   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~enter ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Enter the Loui service.")
           (value   "T iff successfull."))
  (cond ((serv~enter "MYLOUI" :signal-errors nil) T) 
        (loui*signal-interface-errors
         (loui=signal-interface-error "loui~enter"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  leave   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~leave ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Leave the Loui service.")
           (value   "T iff successfull."))
  (cond ((serv~leave "MYLOUI" :signal-errors nil) T) 
        (loui*signal-interface-errors
         (loui=signal-interface-error "loui~leave"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  restart  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~restart ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Restart the Loui service.")
           (value   "T iff successfull."))
  (cond ((serv~restart "MYLOUI" :signal-errors nil) T) 
        (loui*signal-interface-errors
         (loui=signal-interface-error "loui~restart"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   call   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~call (method &optional (function-name "loui~call"))
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "A string containing the Oz-method to be executed.")
           (effect  "Call the Loui-service to execute the method.")
           (value   "The result if everything went allright, otherwise nil."))
  (cond ((serv~apply "MYLOUI" method ;(format nil "exec(~A)" method)
                     :signal-errors nil
		     :timeout 600)
         (serv~error-message))
        ((and loui*signal-interface-errors (serv~interface-error?))
         (loui=signal-interface-error function-name))
        ((and loui*signal-errors (serv~service-error?))
         (loui=signal-error function-name method))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   load-url   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~load-url (url)
  (declare (edited "25-AUG-1999")
           (authors afranke)
           (input   "A string containing a URL.")
           (effect  "Call the Loui-service to localize it.")
           (value   "The content as a string."))
  (loui~enter)
  (loui~call (format nil "loadURL('~A')" url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   queries   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loui~casequery (text arglist &key (title "Click on one button") (default nil))
  (declare (edited  "03-MAY-2000")
	   (authors Pollet)
	   (input   "A TEXT with help, explanations, etc., a list ARGLIST that contains"
		    "the different cases from which the user can choose one, and optionally"
		    "a DEFAULT number, so that one of the cases will be highlighted, and"
		    "a TITLE for the window.")
	   (effect  "The query is sent to LOUI, where a query-dialog will pop up.")
	   (value   "The position of the element in the ARGLIST that was chosen."
		    "(The first element has position 0 ...)"))
  (loui~enter)
   (read-from-string (loui~call
		      (format nil "casequery(message:'~A~%' args:[~{'~A'~}] default:~A title:'~A' $)"
			      text arglist (if default default (1+ (length arglist))) title))))


(defun loui~query (prompt argtypes argdefaults &optional (name 'query))
  (declare (edited  "07-APR-2000")
	   (authors Pollet)
	   (input   "Three equally long lists containing strings for prompting, names of"
		    "the arg-types and arg-defaults; and optionally a name for the dialog.")
	   (effect  "The query is sent to LOUI, where a query-dialog will pop up.")
	   (value   "A list with the results."))
  (loui~enter)
  (read-from-string (loui~call
		     (format nil "query(command:'~A' args:[~{'~A'~}] defaults:[~{'~A'~}] help:[~{\"~A\"~}] $)"
			     name argtypes argdefaults prompt))))
