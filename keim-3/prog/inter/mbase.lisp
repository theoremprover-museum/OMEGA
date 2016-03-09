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

(mod~defmod mbase
	    :uses (mod serv)
	    :documentation "LISP-functions using the mbase service."
	    :exports  (mbase~enter
                       mbase~call
                       mbase~leave
                       mbase~restart

                       mbase~error-number
                       mbase~error-message

                       mbase~signal-interface-errors-on  ; this is the default
                       mbase~signal-interface-errors-off 
                       
                       mbase~signal-errors-on 
                       mbase~signal-errors-off ; this is the default
                       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;    errors    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mbase*signal-interface-errors T)  ; corresponds to serv~signal-errors
(defvar mbase*signal-errors nil)         

(defun mbase~error? ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "T iff the last service-operation returned an error. "))
  (serv~service-error?))

(defun mbase~error-number ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The original mbase-service-error-number. "))
  (serv~service-error-number))

(defun mbase~error-message ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "None.")
           (value   "The error message. "))
  (serv~error-message))

(defun mbase~signal-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "mbase*signal-errors is set to T.")
           (value   "T."))
  (setq mbase*signal-errors T)
  T)

(defun mbase~signal-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "mbase*signal-errors is set to nil.")
           (value   "T."))
  (setq mbase*signal-errors nil)
  T)

(defun mbase~signal-interface-errors-on ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "mbase*signal-interface-errors is set to T.")
           (value   "T."))
  (setq mbase*signal-interface-errors T)
  T)

(defun mbase~signal-interface-errors-off ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "mbase*signal-interface-errors is set to nil.")
           (value   "T."))
  (setq mbase*signal-interface-errors nil)
  T)

(defun mbase=signal-interface-error (function-name)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "An interface error occured while executing function ~A.~%~A: ~A"
          function-name (serv~error-number) (serv~error-message)))

(defun mbase=signal-error (function-name method)
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Signals an error.")
           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "In Function ~A: The MBASE-service could not apply method '~A' successfully.~%~A: ~A"
          function-name method
          (mbase~error-number) (mbase~error-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  enter   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbase~enter ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Enter the MBASE service.")
           (value   "T iff successfull."))
  (cond ((serv~enter "MBASE" :signal-errors nil) T) 
        (mbase*signal-interface-errors
         (mbase=signal-interface-error "mbase~enter"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  leave   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbase~leave ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Leave the MBASE service.")
           (value   "T iff successfull."))
  (cond ((serv~leave "MBASE" :signal-errors nil) T) 
        (mbase*signal-interface-errors
         (mbase=signal-interface-error "mbase~leave"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  restart  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbase~restart ()
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "None.")
           (effect  "Restart the MBASE service.")
           (value   "T iff successfull."))
  (cond ((serv~restart "MBASE" :signal-errors nil) T) 
        (mbase*signal-interface-errors
         (mbase=signal-interface-error "mbase~restart"))
        (T nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   call   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mbase~call (method &optional (function-name "mbase~call"))
  (declare (edited  "02-SEP-1998")
           (authors afranke)
           (input   "A string containing the Oz-method to be executed.")
           (effect  "Call the MBASE-service to execute the method.")
           (value   "The result if everything went allright, otherwise nil."))
  (cond ((serv~apply "MBASE" (concatenate 'string "exec(" method ")")
                     :signal-errors nil)
         (serv~error-message))
        ((and mbase*signal-interface-errors (serv~interface-error?))
         (mbase=signal-interface-error function-name))
        ((and mbase*signal-errors (serv~service-error?))
         (mbase=signal-error function-name method))
        (T nil)))











