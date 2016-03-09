
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic reimplementation of the suggestion mechanism
;; using concurrent lisp facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Suggestion Module
;;
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The file contains the very basic functionality for the
;; multi-agent based suggestion mechanism. These are functions and
;; global variables that are needed in the subsequent modules.
;; Furthermore, it provides a trace facility for debugging output
;; of the mechanism with different levels of importance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :keim)


(mod~defmod SUGG 
            :uses (proc)
            :documentation "The base file for the suggestion mechanism."
            :exports (
                      
                      sugg~hash2list
                      sugg~output
                      sugg~output-message
                      sugg~panic
                      sugg~quit
                      sugg~reset
		      sugg~reset-trace
		      sugg~reset-trace-object
                      sugg~set-output
                      sugg~start
                      sugg~trace
                      sugg~trace-output
                      sugg~untrace
                      
                      sugg*output
                      sugg*quit
                      sugg*reset
                      sugg*trace-objects
                      sugg*traced-objects))


#{
\section{Suggestion}
This module provides some elementary functions for the command suggestion mechanism including some trace
facilities for the objects involved.
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; steering
(defvar sugg*reset 0)
(defvar sugg*quit nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steering the mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric sugg~reset ()
  (:method ()
	   (agent~reset-ext-provers)
	   (incf sugg*reset)))
	   

(defun sugg~quit ()
  (setf sugg*quit t))

(defun sugg~start ()
  (setf sugg*quit nil))

(defun sugg~panic ()
  (sugg=kill-all-processes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sugg=kill-all-processes ()
  (declare (edited  "23-NOV-1999")
	   (authors Sorge)
	   (input   "Nothing.")
	   (effect  "Kills all running processes, except the LISP LISTENER.")
	   (value   "Undefined."))
  (dolist (proc (butlast (proc~all-processes)))
    (proc~kill proc)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functionality for other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sugg~hash2list (hash-table &optional (direction t))
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A hash-table and a flag.")
	   (effect  "None.")
	   (value   "A list containing the elements of the hash-table if DIRECTION is t"
		    "Otherwise a list of keys is returned."))
  (let (list)
    (maphash #'(lambda (x y) (if direction
				 (push y list)
			       (push x list)))
	     hash-table)
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manually built-in debugging system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The idea of this facility is to facilitate debugging in the
;; mechanism by being able to do this on different levels of
;; granularity.
;; Since the whole mechanism should be implemented as sound as
;; possible, i.e. there shouldn't be crashes because of wrong
;; function applications or wrong arguments supplied, one should wrap
;; critical steps into IGNORE-ERRORS wrappers. This limits the natural
;; debugging facilities and we have to introduce our own.
;; The function sugg~output can print messages of different importance
;; giving the working programmer more or less detailed information what
;; went on during the computations. It has a level argument specifying
;; the level of importance of a particular message. There are some
;; conventions on these levels:
;;
;; 0 - Errors which have been caught by IGNORE-ERRORS or HANDLER-CASES. (should never occur)
;; 1 - Warnings about things that might lead to errors somewhere later. (might occur but shouldn't)
;; 2 - General status messages of the system. (should only be suppressed when running completely silently)
;; 3 - Something interesting has been done.
;; ...
;; 10 - Something really boring has happened.
;; >10 - Yaaaawwwwwwwwwwwnnnnnnn.
;;
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; In case of implementing new functions for the mechanism,
;; please think about adding the appropriate debugging-output
;; IMMEDIATELY!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;

(defvar sugg*output 2)

(defgeneric sugg~output (obj level string &rest args)
  (declare (edited  "19-NOV-1999")
	   (authors Sorge)
	   (input   "An object, a number, a string and arguments.")
	   (effect  "Prints string to standard output, if the level is not greater than"
		    "the SUGG*OUTPUT threshold. The object serves to indicate who is talking.")
	   (value   "Undefined."))
  (:method (obj level string &rest args)
	   (declare (ignore obj))
	   (when (<= level sugg*output)
	     (format t ";;;CSM Arbitrary [~A]: " level)
	     (sugg~output-message level string args)))
  (:method ((obj null) level string &rest args)
	   (when (<= level sugg*output)
	     (format t ";;;CSM Creator [~A]: " level)
	     (sugg~output-message level string args))))
  
(defun sugg~output-message (level string args)
  (when (<= level sugg*output)
    (cond ((= level 0)
	   (format t "  ==>ERROR<==  ~A~%" (apply #'format (cons nil (cons string args)))))
	  ((= level 1)
	   (format t "  ==>WARNING<==  ~A~%" (apply #'format (cons nil (cons string args)))))
	  (t (format t "~A~%" (apply #'format (cons nil (cons string args))))))))

(defun sugg~set-output (&optional (level 2))
  (declare (edited  "03-DEC-1999")
	   (authors Sorge)
	   (input   "An integer.")
	   (effect  "Sets the output level of messages to the LEVEL.")
	   (value   "Undefined."))
  (setf sugg*output level))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manually built-in tracing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The idea of this facility is to enable the programmer of agents
;; to monitor the behaviour of certain components of the mechanism
;; only.
;; Certain objects, e.g. agents or blackboards, can be passed to the
;; tracing mechanism using SUGG~TRACE. Specifying calls to the
;; SUGG~TRACE-OUTPUT in certain important functions ensures that
;; the user is kept up-to-date with the behaviour of the specified
;; object.
;; The tracing facility is somewhat orthogonal to the debugging
;; facility. One could compare the SUGG~TRACE with the regular
;; LISP trace and SUGG~OUTPUT with LISP step (however with levels
;; of granularity).
;;
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; In case of implementing new functions for the mechanism,
;; please think about adding the appropriate tracing-output
;; IMMEDIATELY!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;

(defvar sugg*traced-objects nil
  "A list of objects to be traced during the run of the suggestion mechanism.")

(defun sugg~trace (&rest obj)
  (dolist (x obj)
    (when x
      (pushnew x sugg*traced-objects)))
  sugg*traced-objects)

(defun sugg~untrace (&rest obj)
  (if (null obj)
      (setf sugg*traced-objects nil)
    (setf sugg*traced-objects
	  (set-difference sugg*traced-objects obj :test #'eq))))

(defgeneric sugg~trace-output (obj string &rest args)
  (:method :around (obj string &rest args)
	   (declare (ignore string args))
	   (when (find obj sugg*traced-objects)
	     (call-next-method)))
  (:method :before (obj string &rest args)
	   (declare (ignore obj string args))
	   (format t ">> TRACE >>"))
  (:method (obj string &rest args)
	   (format t "~A: ~A~%" obj (apply #'format (cons nil (cons string args))))))

(defun sugg~reset-trace ()
  (setf sugg*traced-objects
	(mapcar #'sugg~reset-trace-object sugg*traced-objects)))

(defgeneric sugg~reset-trace-object (obj)
  (:method (obj)
	   obj))
