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


(mod~defmod INAC 
            :uses (arg comint omega opr)
            :documentation "Interactive stuff"
            :exports (
                      
                      inac~interactive-questions
                      inac~stepper
                      
                      inac*interactive
                      inac*strategy-ks-execution-interactive))

;;; The following functions are internal in other modules and should not be used:
;;; (comint=loop-for-arg)

(defvar inac*interactive nil)
;; global variable, to set interactive

(defvar inac*strategy-ks-execution-interactive nil)
;; global variable, to set the single stategy-ks interactive

#| ----------------------------------------------------- Interface for Stepper ---------------------------------------------------- |#

(defun inac~stepper ()
  nil)

#|
  (declare (edited  "06-AUG-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "After initizlizing an interface, runs a read-eval-print loop,"
		    "reading a command, getting its arguments and applying the command.")
	   (value   "Undefined."))
  
  (opr~arrest-listener)
  
  (let* ((inter (asi~create))
	 (comint (comint~create "STEPPER"
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
						 (com~find-fragment 'interactive)
						 ))
		                inter
				nil)))

    (cld~load-new-registry-files)
    (let ((current-dir (sys~current-directory))
	  (examples (sys~current-directory))) 
      (when examples (sys~chdir examples))
      (comint~top comint)
      (sys~chdir current-dir)
      (values)))

  (opr~release-listener))

|#
#| ---------------------------------------------------------- Interactive ----------------------------------------------------------- |#

(defun inac~interactive-questions (arg-names ask-strings types defaults)
  
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega~error "Cannot query via LOUI"))
	(t
	 (let* (;;(interface (if (and (not (boundp 'comint*current-comint))  comint*current-comint)
		;;	       (comint~interface comint*current-comint)
		;;	     (asi~create)))
		(back nil))
	   
	   (opr~arrest-listener)
	   
	   (unwind-protect
	       (let* ((arg-types (mapcar #'arg~find-argtype types))
		      (read-things
		       (do* ((rest-names arg-names (rest rest-names))
			     (rest-strings ask-strings (rest rest-strings))
			     (rest-types arg-types (rest arg-types))
			     (rest-defaults defaults (rest defaults))
			     (back-things nil))
			   ((null rest-names)
			    back-things)
			 (let* ((backi (keim::comint=loop-for-arg (first rest-names)
								  (first rest-types)
								  (first rest-strings)
								  (first rest-defaults)
								  comint*current-comint)))
			   (setq back-things (append back-things (list backi)))))))
		 (setf back (mapcar #'cdr read-things)))
	     (opr~release-listener))
	   back))))





