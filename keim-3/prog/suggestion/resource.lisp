
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22/04/00
;;;
;;; New class hierarchy
;;;
;;; 23/04/00
;;;
;;; Suggestion blackboard has now list of the resource objects from
;;; command-agents.
;;;
;;; 07/06/00
;;;
;;; The evaluate-last-run method resets the resource object. Therefore
;;; the surveying agents may not have the correct resource information
;;; this may has to be modified again. !!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :keim)



(mod~defmod RSRC 
            :uses (sys)
            :documentation "This module provides the basic data structure for the resource
objects."
            :exports (rsrc+agent-resource
                      rsrc+agent-resource
                      rsrc+bb-agent-resource
                      rsrc+bb-resource
                      rsrc+command-bb-resource
                      rsrc+param-agent-resource
                      rsrc+resources
                      rsrc+sugg-agent-resource
                      rsrc+sugg-bb-resource
                      
		      rsrc~act-resources
                      rsrc~add-data
                      rsrc~agent-data
                      rsrc~agents-running
                      rsrc~average-n-int
                      rsrc~average-prev-n-int
		      rsrc~bad-entries
                      rsrc~errors
                      rsrc~eval-fct
		      rsrc~evaluated
                      rsrc~get-time-stamp
                      rsrc~last-bad
                      rsrc~last-entries
                      rsrc~last-used
                      rsrc~log-flag
                      rsrc~name
		      rsrc~print-res-info
                      rsrc~res1
                      rsrc~res3
		      rsrc~reset-activation-level
		      rsrc~resets
		      rsrc~run-time-last
                      rsrc~run-time-total
                      rsrc~running-state
                      rsrc~serious-reset
                      rsrc~time-diff
                      rsrc~update-resources
                      rsrc~update-time
                      rsrc~useful-entries
                      rsrc~visited-entries
                      
                      rsrc*act-exec-intervals
                      rsrc*activation-level
		      rsrc*activation-level-old
                      rsrc*exec-intervals
                      rsrc*use-resources))

;;; The following functions are internal in other modules and should not be used:
;;; (ohlp=make-directory)

#{
\section{Resources}

This module provides the data structure for the resource objects.
It also defines functions for picking up and maintain information about the
agents performance.

#}


(defvar rsrc*use-resources T "A flag indicating whether the resource mechanism is used.")

(defvar rsrc*activation-level 0
  "The global activation level. All agents with a complexity-rating (act~resources) below this level will be turned off")

(defvar rsrc*activation-level-old 0
  "This variable is used only when the activation level is temporary changed.")
 
(defvar rsrc*exec-intervals 5
  "Specifies after how many resets the average user interaction interval gets updated.")

(defvar rsrc*act-exec-intervals 5
  "The number of resets occured after activation level was last updated.")

		      
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for the Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rsrc+resources ()
  ((name :accessor rsrc~name
	 :initarg :name
	 :initform ""
	 :documentation "The name of the resource object. If the Object is an
agent-resource, the name of the agent, otherwise the name of the blackboard.")
   (log-flag :accessor rsrc~log-flag
	     :initarg :flag
	     :initform NIL
	     :documentation "If flag is set, reource information will be written
to a file, each time the agent is resetting."))
      (:documentation "The superclass for all resource-objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes for the Blackboard resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rsrc+bb-resource (rsrc+resources)
  ((agent-data :accessor rsrc~agent-data
	       :initarg :agent-data
	       :initform nil
	       :documentation "A list of the resource objects of the agents working for
the blackboard."))
   (:documentation "The class of blackboard-resource objects."))

(defclass rsrc+command-bb-resource (rsrc+bb-resource) ())
(defclass rsrc+sugg-bb-resource (rsrc+bb-resource) ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes for the Agent resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rsrc+agent-resource (rsrc+resources)
  ((actual-resources :accessor rsrc~act-resources
		     :initarg actual-resources
		     :initform 1
		     :documentation "The agents complexity rating.")
   (res1 :accessor rsrc~res1
	 :initarg new-resources
	 :initform 0
	 :documentation "Resource adjustment from surveyor agent.")
   (res3 :accessor rsrc~res3
	 :initarg new-resources
	 :initform 0
	 :documentation "The given penalties.")
   (running-state :accessor rsrc~running-state
		  :initarg running-state
		  :initform T
		  :documentation "T if agent is currently working, NIL otherwise.")
   (run-time-last :accessor rsrc~run-time-last
		  :initarg :run-time-last
		  :initform 0
		  :documentation "The running time needed on the last visited entry.")
   (run-time-total :accessor rsrc~run-time-total
		   :initarg :run-time-total
		   :initform 0
		   :documentation "The total running time of the agent.")
   (visited-entries :accessor rsrc~visited-entries
		    :initarg :visited-entries
		    :initform 0
		    :documentation "The number of all visited entries on the backboard.")
   (useful-entries :accessor rsrc~useful-entries
		   :initarg :useful-entries
		   :initform 0
		   :documentation "The number of all used entries on the blackboard.")
   (bad-entries :accessor rsrc~bad-entries
		:initarg :bad-entries
		:initform 0
		:documentation "The number of entries the agent was applied on, but
couldn't make a new suggestion")
   (last-entries :accessor rsrc~last-entries
		 :initarg :last-entries
		 :initform 0
		 :documentation "The number of visited entries after the last reset.")
   (last-used-entries :accessor rsrc~last-used
		      :initarg :last-used
		      :initform 0
		      :documentation "The number of all used entries on the blackboard
after the last reset")
   (last-bad-entries :accessor rsrc~last-bad
		     :initarg :last-bad
		     :initform 0
		     :documentation "The number of all entries on the blackboard the
agent could not use.")
   (errors :accessor rsrc~errors
	   :initarg :errors
	   :initform 0
	   :documentation "For debbunging only. Remove later")
   (resets :accessor rsrc~resets
	   :initarg resets
	   :initform 0
	   :documentation "The number of resets occured.")
   (eval-function :accessor rsrc~eval-fct
		  :initarg eval-fct
		  :initform NIL
		  :documentation "A function to be applied on the resource object to evaluate the resources."))
  (:documentation : "The superclass for all agent-resource objects."))


(defclass rsrc+param-agent-resource (rsrc+agent-resource)
  ((serious-reset :accessor rsrc~serious-reset
		  :initarg resets
		  :initform 0
		  :documentation "A integer indicating wether there was a reset before the end of
a calculation (1) or not (0)")
   (evaluated :accessor rsrc~evaluated
	      :initarg evaluated
	      :initform NIL
	      :documentation "A flag indicating whether the resources have been evaluated
from either the agent itself or the blackboard agent."))
  (:documentation "The object containing all the data for evaluating the performance of a parameter agent."))


(defclass rsrc+bb-agent-resource (rsrc+agent-resource)
  ((agents-running :accessor rsrc~agents-running
		  :initarg agents-running
		  :initform 0
		  :documentation "The number of agents currently working for the blackboard.")))


(defclass rsrc+sugg-agent-resource (rsrc+bb-agent-resource) ())
   
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for  picking up and maintain information about the
;; agents performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; !!!! Do not forget to set rsrc~last-exec-time to zero in the
;      evaluate-resources method !!!!

;; FUNKTION WIRD ANSCHEINEND NICHT MEHR BENOETIGT. ENTFERNEN ?
(defmethod rsrc~update-time (time (res rsrc+sugg-agent-resource))
  (declare (edited  "30-MAY-2000")
	   (authors Mth)
	   (input   "A timestamp and a resource object from the suggestion"
		    "agent.")
	   (effect  "If the method is called for the first time after the last"
		    "reset then the last-exec-time slot will be set to the"
		    "time stamp given.")
	   (value   "None."))
  ;; is this the first call of the function since the last reset ?
  (when (eql (rsrc~last-exec-time res) 0)
    (setf (rsrc~last-exec-time res) time)))
     

(defun rsrc=new-average (old-av-value new-total-value nr-of-resets)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "An average value for the last runs, a new total value, the number"
		    "of resets occured.")
	   (effect  "None.")
	   (value   "The new overall average value."))
  (if (> nr-of-resets 1)
      (/ (+ (* old-av-value (- nr-of-resets 1))
	    new-total-value)      ;; the new total value
	 nr-of-resets)
    new-total-value))


(defgeneric rsrc~add-data (res-obj data-obj)
  (declare (edited  "27-APR-2000")
	   (authors Mth)
	   (input   "A blackboard resource object and a resource object from a an agent"
		    "working for the blackboard.")
	   (effect  "The agents resource object will be added to the list of reource"
		    "objects, which is maintained by the blackboard resource object.")
	   (value   "None."))					
  (:method ((res-obj rsrc+bb-resource) data-obj)
	   (when (not (find-if #'(lambda (obj) (equal (rsrc~name obj) (rsrc~name data-obj)))
			       (rsrc~agent-data res-obj)))
	     (let ((newlist (cons data-obj (rsrc~agent-data res-obj))))
	       (setf (rsrc~agent-data res-obj) newlist)))))


;; Methods for command and suggestion agent could probably be merged !
;; Computations are the same. Suggestion agent has to update user inter
;; action interval. (Use Befor / After -Methods ??

;; ACHTUNG: Was passiert hier, wenn alle Sub-Agenten inaktiv ? 
(defgeneric rsrc~update-resources (agent-resources bb-resources)  
  (declare (edited  "27-APR-2000")
	   (authors Mth)
	   (input   "An angent resource object and a resource object from the blackboard"
		    "the agent surveyes.")
	   (effect  "The data in the blackboard resource object gets updated and the new"
        	    "values will be stored in the agent resource object")
	   (value   "None."))
  (:method ((agent-resources rsrc+bb-agent-resource) (bb-resources
						      rsrc+bb-resource))
	   ;; method for command agent
	   (incf (rsrc~resets agent-resources) 1)
	   (heur~reset-resource-obj agent-resources)
	   (let ((used-resources 0))
	     (mapcar #'(lambda (res-obj)
			 (when (rsrc~running-state res-obj)
			   (incf (rsrc~agents-running agent-resources) 1)
			 ;; calculate total values
			   (incf (rsrc~run-time-last agent-resources) (rsrc~run-time-last res-obj))
			   (incf (rsrc~last-entries agent-resources) (rsrc~last-entries res-obj))
			   (incf (rsrc~last-used agent-resources)
				 (rsrc~last-used res-obj))
			   (incf (rsrc~last-bad agent-resources) (rsrc~last-bad	res-obj))
			   ;; get the resources after agent has itself given a penalty
			 (incf used-resources (rsrc~act-resources res-obj)))
			 ;; mark parameter agents resource object as evaluated
			 (heur~reset-resource-obj res-obj))
		     (rsrc~agent-data bb-resources))
	     ;;; hier langsam !!!!
	     (setf (rsrc~act-resources agent-resources) (/ used-resources (length
									     (rsrc~agent-data bb-resources))))
	     ;; calculate total values
	     (incf (rsrc~visited-entries agent-resources) (rsrc~last-entries
							   agent-resources))
	     (incf (rsrc~useful-entries agent-resources) (rsrc~last-used agent-resources))
	     (incf (rsrc~bad-entries agent-resources) (rsrc~last-bad agent-resources))
	     (incf (rsrc~run-time-total agent-resources) (rsrc~run-time-last
							  agent-resources))))
  (:method ((agent-resources rsrc+sugg-agent-resource) (bb-resources rsrc+bb-resource))
	   ;; method for suggestion agent
	   (incf (rsrc~resets agent-resources) 1)
	   (heur~reset-resource-obj agent-resources)
	   ;; begin recording the resource information of the suggestion
	   ;; agent
	   (let ((used-resources 0))
	     (mapcar #'(lambda (res-obj)
			 (when (rsrc~running-state res-obj)
			   (incf (rsrc~agents-running agent-resources)
				 (+ (rsrc~agents-running res-obj) 1))
			   ;; calculate total values
			   (incf (rsrc~run-time-last agent-resources) (rsrc~run-time-last res-obj))
			   (incf (rsrc~last-entries agent-resources) (rsrc~last-entries res-obj))
			   (incf (rsrc~last-used agent-resources)
				 (rsrc~last-used res-obj))
			   (incf (rsrc~last-bad agent-resources) (rsrc~last-bad	res-obj))
			   ;; get the resources after agent has itself given a penalty
			   (incf used-resources (rsrc~act-resources res-obj))))
		     (rsrc~agent-data bb-resources))
	     ;;; hier langsam !!!!
	     (setf (rsrc~act-resources agent-resources) (/ used-resources (length
									      (rsrc~agent-data bb-resources))))
	     ;; calculate total values
	     (incf (rsrc~visited-entries agent-resources) (rsrc~last-entries
							   agent-resources))
	     (incf (rsrc~useful-entries agent-resources) (rsrc~last-used agent-resources))
	     (incf (rsrc~bad-entries agent-resources) (rsrc~last-bad agent-resources))
	     (incf (rsrc~run-time-total agent-resources) (rsrc~run-time-last
							  agent-resources)))))
  
	     
	     




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for printing object data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-object ((res-obj rsrc+param-agent-resource) stream)
  (format stream "Data from param-agent: ~A~% Complexity Rating: ~A~% ~
 Res1: ~A~% Res3: ~A~%DATA from last run:~% Runtime in ms: ~A~% Visited entries: ~A~% ~
 Useful entries:~A~% Bad entries: ~A~% Errors:~A~%Total DATA:~% Runtime in ms: ~A~% Visited entries: ~A~% ~
 Useful entries (extended PAI's): ~A~% Bad entries: ~A~%" (rsrc~name res-obj) (rsrc~act-resources res-obj) (rsrc~res1 res-obj) (rsrc~res3 res-obj) (rsrc~run-time-last res-obj) (rsrc~last-entries res-obj)
(rsrc~last-used res-obj) (rsrc~last-bad res-obj) (rsrc~errors res-obj)
(rsrc~run-time-total res-obj) (rsrc~visited-entries res-obj) (rsrc~useful-entries res-obj)
(rsrc~bad-entries res-obj)))


(defmethod print-object ((res-obj rsrc+bb-resource) stream)
  (format stream "Blackboard: ~A: Listing data from param-agents:~%" (rsrc~name res-obj))
  (dolist (obj (rsrc~agent-data res-obj))
    (print-object obj stream)))


(defmethod print-object ((res-obj rsrc+bb-agent-resource) stream)
 (format stream "Data from command-agent (aggregated values form subagents): ~A~% Number of running subagents: ~A~% Complexity Rating: ~A~%~
Res1: ~A~% Res3: ~A~%DATA form last run:~% Runtime in ms: ~A~% Visited entries: ~A~% Useful~
entries: ~A~% Bad entries: ~A~% Errors:~A~%Total DATA:~% Runtime in ms: ~A~% Visited entries: ~A~% Useful entries (extended PAI's): ~A~% Bad~
entries: ~A~%"  (rsrc~name res-obj) (rsrc~agents-running res-obj) (rsrc~act-resources res-obj) (rsrc~res1 res-obj)
(rsrc~res3 res-obj) (rsrc~run-time-last res-obj) (rsrc~last-entries res-obj)
(rsrc~last-used res-obj) (rsrc~last-bad res-obj) (rsrc~errors res-obj)
(rsrc~run-time-total res-obj) (rsrc~visited-entries res-obj) (rsrc~useful-entries res-obj)
(rsrc~bad-entries res-obj)))

(defmethod print-object ((res-obj rsrc+sugg-agent-resource) stream)
  (format stream "Data from suggestion-agent: ~A~% Number of running agents: ~A~% Actual-resources: ~A~% 
~A~%DATA form last run:~% Runtime: ~A~% Visited entries: ~A~% Useful entries: ~A~% Bad~
entries: ~A~% Errors:~A~%Total DATA:~% Runtime: ~A~% Visited entries: ~A~% Useful entries: ~A~% Bad~
entries: ~A~%" (rsrc~name res-obj) (rsrc~agents-running res-obj)  (rsrc~act-resources res-obj)  (rsrc~run-time-last res-obj) (rsrc~last-entries res-obj)
(rsrc~last-used res-obj) (rsrc~last-bad res-obj) (rsrc~errors res-obj)
(rsrc~run-time-total res-obj) (rsrc~visited-entries res-obj) (rsrc~useful-entries res-obj)
(rsrc~bad-entries res-obj)))
 

(defgeneric rsrc~print-res-info (res)
  (:method ((res rsrc+resources))
	   ;; if log string not NIL, then append actual resource information
	   (when (rsrc~log-flag res)
	     (setf (rsrc~log-flag res)
		   (format nil "~A~%--------------------------~%~A" (rsrc~log-flag res) res)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rsrc~get-time-stamp ()
  (declare (edited  "27-APR-2000")
	   (authors Mth)
	   (input   "None.")
	   (effect  "Gets the internal time.")
	   (value   "None."))
  (get-internal-real-time))
  ;; could also use: get-internal-run-time, but this would be system-dependent 


;; returns the time between two timesteps in mili-seconds
(defun rsrc~time-diff (time-stamp-1 time-stamp-2)
  (declare (edited  "27-APR-2000")
	   (authors Mth)
	   (input   "Two time-stamps")
	   (effect  "None.")
	   (value   "The time difference between the time-stamps in miliseconds."))
  (- time-stamp-2 time-stamp-1))




(defgeneric ohlp=make-directory (pathname)
  (:method ((pathname pathname))
	   (unless (probe-file pathname)
	     (sys~call-system (format nil "mkdir ~A" (namestring pathname)))))
  (:method ((pathname string))
	   (let ((path (make-pathname :directory pathname)))
	     (unless (probe-file path)
	       (sys~call-system (format nil "mkdir ~A" (namestring path)))))))


(defun rsrc~reset-activation-level ()
  (setf rsrc*activation-level rsrc*activation-level-old)
  (setf heur*act-diff heur*act-diff-help))
  
