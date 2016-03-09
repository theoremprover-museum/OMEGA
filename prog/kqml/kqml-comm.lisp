;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 2001 by AG Siekmann, Fachbereich Informatik,             ;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file: kqml-comm.lisp
;; creation date: 02.02.2001
;;
;; author: Juergen Zimmer
;; e-mail: (jzimmer@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains function for 
;; KQML communication facilities for OMEGA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)

(defvar kqml*error '(0 "Dummy-Answer-String" "noMethodHasBeenSentYet"))
(defvar kqml*default-timeout-send 10)
(defvar kqml*default-timeout-send-and-wait 30)
(defvar kqml*signal-errors T)            
(defvar kqml*signal-message-errors nil)

(defun kqml~send (performative language content &key ((:timeout seconds) kqml*default-timeout-send))
  (declare (edited  "22-FEB-2001")
           (authors jzimmer)
           (input   "The symbolic name of the receiver agent and the content"
		    "which can be OpenMath or OMDoc in XML encoding.")
           (effect  "Sends the message to the KQML converation module of the"
		    "Oz-wrapper around OMEGA.")
           (value   "The result is T, iff the message was sent successfully to"
		    "the receiver agent (within :timeout seconds)."))

  (kqml=operation (format nil "sendKqml('~A' '~A' '~A' \~A)"
                          performative 
                          language 
                          content
			  (kqml=mkopt-timeout seconds)))
  (cond ((kqml~ok?) (kqml~error-message))
        (T nil)))

(defun kqml~send-and-wait(performative service cd language content 
                                       &key ((:timeout seconds) kqml*default-timeout-send-and-wait))
  (declare (edited  "18-APR-2001")
           (authors Jzimmer)
           (input   "The performative, the service, the content-language"
		    "which can be OpenMath or OMDoc in XML encoding.")
           (effect  "Sends the message to the KQML converation module of the"
		    "Oz-wrapper around OMEGA AND WAITS FOR THE RESULT")
           (value   "The result of the KQML query if the receiver agent"
                    "could answer the query (within :timeout seconds),"
                    "NIL, otherwise."))
  
  (kqml=operation (format nil "sendKqmlAndWait('~A' '~A' '~A' '~A' ~A \~A)" 
                          performative 
                          service
                          cd
                          language 
                          content
			  (kqml=mkopt-timeout seconds)))
  (cond ((kqml~ok?) 
         (kqml~error-message))
        (T nil)))

(defun kqml=operation (message)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "A message (string).")
           (effect  "Send the message to the :service-socket, read the "
                    "answer list '(err-number err-message) from the "
                    ":service-socket and assign it to kqml*error.")
           (value   "The err-message if the err-number is 0, otherwise nil."))
  (omega~message message)
  (socket~write message :service)
  (proc~socket-wait-for-input-available :service "Waiting for the service socket.")
  (setq kqml*error (cons message (read-from-string (socket~read :service))))
  (if (kqml~ok?) 
      (kqml~error-message)
    nil))

(defun kqml=mkopt-timeout (seconds)
  (cond ((null seconds) "")
        (T (format nil " timeout:~A" seconds))))

(defun kqml=signal-message-error (function-name receiver performative content)
  (omega~error  "~* '~A' could not send the performative ~A to ~% ~A."
		function-name performative receiver content)
  (omega~error  "Content of message: ~%~A " Content)
  (omega~error  "Error: ~A ~A"(kqml~error-number) (kqml~error-message)))
    
(defun kqml~error-number ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The error code of the last service-operation. "
                    "If the operation went allright, the error code is 0."))
  (cadr kqml*error))

(defun kqml~error-message ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The error message of the last service-operation. "
                    "If the operation went allright, the error message is "
                    "the answer string."))
  (caddr kqml*error))

(defun kqml~last-sent ()
  (declare (edited  "22-JUN-1999")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "A string containing the Oz-method that was sent during "
                    "the last service-operation. "
                    "If the result of that service-operation was an error, "
                    "this is the method that caused it. "))
  (car kqml*error))

(defun kqml~ok? ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "T iff the last service-operation went allright."))
  (zerop (kqml~error-number)))

				 
	 
	






