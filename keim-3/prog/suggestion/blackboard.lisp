

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic reimplementation of the suggestion mechanism
;; using concurrent lisp facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Blackboard Module
;;
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is all the basic functionality to define and work with
;; blackboards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :keim)


(mod~defmod BB 
            :uses (agent com comint foci keim node pdsn rsrc sugg sys)
            :documentation "The basic functionality for blackboards."
            :exports (bb+blackboard
                      bb+blackboard
                      bb+command-blackboard
                      bb+command-suggestion
                      bb+entry
                      bb+parameter-suggestion
                      bb+suggestion-blackboard
                      
                      bb~active-agents
                      bb~active-blackboards
                      bb~add-list-entries
                      bb~add-list-of-list-entries
                      bb~add-list-of-single-entries
                      bb~add-new-entry
                      bb~add-single-entry
                      bb~agent-visited-entry
                      bb~agents
                      bb~agents-done-p
                      bb~available-sorting-criteria
                      bb~classifier
                      bb~com-args
                      bb~command
                      bb~command-blackboard-p
                      bb~command-suggestion-equal-p
                      bb~command-suggestion-leq-p
                      bb~command-suggestion-p
                      bb~create-command-blackboard
                      bb~create-command-suggestion
                      bb~create-empty-parameter-suggestion
                      bb~create-parameter-suggestion
                      bb~create-suggestion-blackboard
                      bb~enter-agent
                      bb~enter-information
                      bb~entries
                      bb~entry-command
                      bb~entry-empty-p
                      bb~entry-equal-p
                      bb~entry-greater-p
                      bb~entry-mapping
                      bb~entry-p
                      bb~entry-parameters
                      bb~entry-status
                      bb~entry-visited
                      bb~entry-visited-p
                      bb~equality
                      bb~erase
                      bb~find-agent
                      bb~find-blackboard
                      bb~find-command
                      bb~get-blackboards
                      bb~get-sugg-blackboard
                      bb~get-sum-of-blackboard-resources
                      bb~information
                      bb~make-command-blackboard
                      bb~make-parameter-suggestion
                      bb~make-suggestion-blackboard
                      bb~merge-new-entries
                      bb~name
                      bb~new-entries
                      bb~ordering
                      bb~p
                      bb~parameter-suggestion-equal-p
                      bb~parameter-suggestion-leq-p
                      bb~parameter-suggestion-p
                      bb~pop-all-entries
                      bb~pop-entry
                      bb~register-blackboard
                      bb~remove-agent
                      bb~remove-all-agents
                      bb~remove-all-blackboards
                      bb~remove-blackboard
                      bb~remove-information
                      bb~reset
                      bb~reset-state
                      bb~resources
                      bb~set-command-suggestion-equality
                      bb~set-command-suggestion-ordering
                      bb~set-parameter-suggestion-equality
                      bb~set-parameter-suggestion-ordering
                      bb~suggestion-blackboard-p
                      bb~surveyor
                      
                      bb*blackboard-hash-table
                      bb*complexity-level
                      bb*dual-rules
                      bb*ordering-command-suggestion-equal
                      bb*ordering-command-suggestion-leq
                      bb*ordering-parameter-suggestion-equal
                      bb*ordering-parameter-suggestion-leq
                      bb*symmetric-rules))



#{
\section{Blackboards}
This module provides elementary functions for blackboards.
#}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for the Blackboards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

  (defclass bb+blackboard ()
    ((name :accessor bb~name
	   :initarg :name
	   :initform ""
	   :documentation "The name of the blackboard")
     (entries :accessor bb~entries
	      :initarg :entries
	      :initform nil
	      :documentation "A list of blackboard entries.")
     (new-entries :accessor bb~new-entries
		  :initarg :new-entries
		  :initform nil
		  :documentation "A list of entries that have not been inserted.")
     (agents :accessor bb~agents
	     :initarg :agents
	     :initform nil
	     :documentation "A list of agents associated with the blackboard.")
     (surveyor :accessor bb~surveyor
	       :initarg :surveyor
	       :initform nil
	       :documentation "An agent surveying the blackboard.")
     (reset :accessor bb~reset-state
	    :initarg :reset
	    :initform nil
	    :documentation "The reset state of the blackboard.")
     (ordering :accessor bb~ordering
	       :initarg :ordering
	       :initform nil
	       :documentation "A binary predicate specifying the ordering on the blackboard.")
     (equality :accessor bb~equality
	       :initarg :equality
	       :initform #'equal
	       :documentation "A binary predicate specifying equality for blackboard entries.")
     (resources :accessor bb~resources
		:initarg :resources
		:initform nil
		:documentation "The resource information of the blackboard.")
     (information :accessor bb~information
		  :initarg :information
		  :initform nil
		  :documentation "A slot for miscellanious information the blackboard and
                   the agents might want to communicate. This is generally a list of symbols."))
    (:documentation "The class of blackboards."))

  (defclass bb+command-blackboard (bb+blackboard)
    ((command :accessor bb~command
	      :initarg :command
	      :initform nil
	      :documentation "A backward-reference to the command the blackboard is initialized for.")
     (com-args :accessor bb~com-args
	       :initarg :com-args
	       :initform nil
	       :documentation "A list of argument names for the command."))
    (:documentation "The class of command blackboards."))

  (defclass bb+suggestion-blackboard (bb+blackboard)
    ((classifier :accessor bb~classifier
		 :initarg :classifier
		 :initform nil
		 :documentation "A suggestion blackboard also can have a punch of
                  classifying agents working for it."))
    (:documentation "The class of suggestion blackboards."))
  )

;;;
;;;  Blackboard entries
;;;
;;;  We do not want to allow every arbitrary entry on our blackboards.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for the Blackboard Entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

  (defclass bb+entry ()
    ((visited :accessor bb~entry-visited
	      :initarg :visited
	      :initform nil
	      :documentation "A list of agents that have already visited the bb-entry."))
    (:documentation "The mother of all blackboard entries."))

  (defclass bb+command-suggestion (bb+entry)
    ((command :accessor bb~entry-command
	      :initarg :command
	      :initform nil
	      :documentation "A command the suggestion is made for.")
     (parameters :accessor bb~entry-parameters
		 :initarg :parameters
		 :initform nil
		 :type bb+parameter-suggestion
		 :documentation "A parameter suggestion."))
    (:documentation "Blackboard entries for command suggestions."))

  (defclass bb+parameter-suggestion (bb+entry)
     ((mapping :accessor bb~entry-mapping
	       :initarg :mapping
	       :initform nil
	       :documentation "An association-list mapping a parameter to a value")
      (status :accessor bb~entry-status
	      :initarg :status
	      :initform nil
	      :documentation "The status of the entry, i.e. completed or not."))
     (:documentation "Blackboard entries for parameter suggestions."))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors for blackboards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bb~create-command-blackboard (name command entries new-entries agents ordering
					  equality rsc information com-args)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "Slot values for command blackboards.")
	   (effect  "Creates an instance of BB+COMMAND-BLACKBOARD.")
	   (value   "The newly created instance."))
  (make-instance 'bb+command-blackboard
		 :name name
		 :command command
		 :entries entries
		 :new-entries new-entries
		 :agents agents
		 :reset sugg*reset
		 :ordering ordering
		 :equality equality
		 :resources rsc
		 :information information
		 :com-args com-args))

(defgeneric bb~make-command-blackboard (command)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A command.")
	   (effect  "A new standard command blackboard is created.")
	   (value   "The newly created blackboard."))
  (:method ((command string))
	   (bb~make-command-blackboard (bb~find-command command)))
  (:method ((command symbol))
	   (bb~make-command-blackboard (bb~find-command command))))

(defun bb~create-suggestion-blackboard (name entries new-entries agents ordering equality resources information classifier)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "Slot values for suggestion blackboards.")
	   (effect  "Creates an instance of BB+SUGGESTION-BLACKBOARD.")
	   (value   "The newly created instance."))
  (make-instance 'bb+suggestion-blackboard
		 :name name
		 :entries entries
		 :new-entries new-entries
		 :agents agents
		 :reset sugg*reset
		 :ordering ordering
		 :equality equality
		 :resources resources
		 :information information
		 :classifier classifier))


(defun bb~make-suggestion-blackboard (&key (name nil) (agents nil) (classifiers nil))
  (declare (edited  "23-NOV-1999")
	   (authors Sorge)
	   (input   )
	   (effect  "A new standard suggestion blackboard is created.")
	   (value   "The newly created blackboard."))
  (let* ((name (if name
		   (etypecase name                          
		     (string (string-upcase name))
		     (symbol (symbol-name name)))
		 (symbol-name (gensym "BB"))))
	 ;; MALTE 22/04/00
	 (res-obj (make-instance 'rsrc+sugg-bb-resource :name name))
	 (bb (bb~create-suggestion-blackboard name nil nil agents 
					      #'bb~entry-greater-p
					      #'bb~entry-equal-p
					      res-obj nil classifiers)))
    (bb~register-blackboard bb)
    (mapcar #'(lambda (agent) (rsrc~add-data res-obj (agent~resources agent))) agents)
    (sugg~output nil 2 "Suggestion Blackboard ~A created." name)
    bb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates for blackboards  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bb~p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+BLACKBOARD."))
  (typep obj 'bb+blackboard))

(defun bb~suggestion-blackboard-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+SUGGESTION-BLACKBOARD."))
  (typep obj 'bb+suggestion-blackboard))

(defun bb~command-blackboard-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+COMMAND-BLACKBOARD."))
  (typep obj 'bb+command-blackboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bb*blackboard-hash-table (make-hash-table :test #'equal)
  "A hash-table keeping track of all existing blackboards.")

(defvar bb*complexity-level 100
  "The level of complexity for which agents are being invoked. (100 should be rather high.)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adminstrative functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bb~find-blackboard (bb)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A name of a blackboard.")
	   (effect  "None.")
	   (value   "The blackboard with that name if one exists."))
  (:method ((bb bb+blackboard))
	   (bb~find-blackboard (bb~name bb)))
  (:method ((bb symbol))
	   (bb~find-blackboard (symbol-name bb)))
  (:method ((bb string))
	   (gethash (string-upcase bb) bb*blackboard-hash-table)))  

(defun bb~register-blackboard (bb)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "The blackboard is registered in the hash-table.")
	   (value   "Undefined."))
  (setf (gethash (bb~name bb) bb*blackboard-hash-table) bb))

(defgeneric bb~remove-blackboard (bb)
  (declare (edited  "27-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "The blackboard is removed from the mechanism.")
	   (value   "T if successful, o/w NIL."))
  (:method ((bb bb+blackboard))
	   (bb~remove-blackboard (bb~name bb)))
  (:method ((key string))
	   (remhash (string-upcase key) bb*blackboard-hash-table))
  (:method ((key symbol))
	   (remhash (symbol-name key) bb*blackboard-hash-table)))

(defun bb~remove-all-blackboards ()
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Removes all blackboards from the mechanism.")
	   (value   "Undefined."))
  (clrhash bb*blackboard-hash-table)
  (sugg~output nil 2 "Blackboard hash-table cleared."))
  
(defun bb~get-blackboards ()
  (declare (edited  "06-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A list of all existing blackboards."))
  (sugg~hash2list bb*blackboard-hash-table))

;; MALTE
(defun bb~get-sugg-blackboard ()
 (find-if #'bb~suggestion-blackboard-p (bb~get-blackboards)))
    
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Miscellaneous



(defgeneric bb~reset (bb &optional entry)        ;;; VS: Here should go the new resource distribution!       <---- to be done
  (declare (edited  "19-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard and optionally a default entry.")
	   (effect  "The blackboard is reseted, i.e. all its entry slots are either"
		    "set to NIL or filled with empty blackboard entries."
		    "Moreover, a new resource allocation is computed.")
	   (value   "Undefined."))
  (:method :before ((bb bb+blackboard) &optional entry)
	   (declare (ignore entry))
	   (sugg~output bb 10 "I'm being reset."))
  (:method ((bb symbol) &optional entry)
	   (let ((real-bb (bb~find-blackboard bb)))
	     (when real-bb (bb~reset real-bb entry))))
  (:method ((bb string) &optional entry)
	   (let ((real-bb (bb~find-blackboard bb)))
	     (when real-bb (bb~reset real-bb entry))))
  (:method ((bb bb+blackboard) &optional (entry nil))
	   (setf (bb~entries bb) (when entry (list entry)))
	   (setf (bb~new-entries bb) nil))
  (:method :after ((bb bb+blackboard) &optional entry)
	   (declare (ignore entry))
	   (setf (bb~reset-state bb) sugg*reset)
	   (sugg~output bb 6 "I am reset.")
	   (sugg~trace-output bb "I am reset."))
  (:method ((bb bb+command-blackboard) &optional (entry nil))
	   (setf (bb~entries bb) (if entry
				     (let* ((argnames (bb~com-args bb))
					    (alist (ignore-errors (pairlis argnames entry))))
				       (if alist (list (bb~make-parameter-suggestion alist))
					 (progn 
					   (sugg~output bb 0 "Cannot reset with ~A as initial entry" entry)
					   (list (bb~create-empty-parameter-suggestion)))))
				   (list (bb~create-empty-parameter-suggestion))))
	   (setf (bb~new-entries bb) nil)))


(defun bb~erase (bb)
  (declare (edited  "23-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "The entries of the blackboard are erased. (Not agent structure, though).")
	   (value   "Undefined."))
  (sugg~output bb 10 "Erasing all entries.")
  (setf (bb~entries bb) nil)
  (setf (bb~new-entries bb) nil)
  (setf (bb~information bb) nil)
  (sugg~output bb 6 "All entries erased."))

;;; Handling entries

(defgeneric bb~merge-new-entries (bb)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "The new-entries of the blackboard are merged with the old entries.")
	   (value   "For suggestion blackboards the new list of entries. In case of command"
		    "blackboards: If the best rated entry has changed it is returned. O/w NIL."))
  (:method ((bb bb+suggestion-blackboard))
	   (sugg~output bb 5 "Merging new-entries ~A." (bb~new-entries bb))
	   (setf (bb~entries bb)
		 (bb=merge-with-order (bb~entries bb)
				      (bb~new-entries bb)
				      (bb~ordering bb)
				      (bb~equality bb)))
	   (setf (bb~new-entries bb) nil)
	   (sugg~output bb 10 "My entries are: ~A" (bb~entries bb))
	   (bb~entries bb))
  (:method ((bb bb+command-blackboard))
	   (let* ((entries (bb~entries bb))
		  (first (car entries))
		  (equl (bb~equality bb))
		  (new (bb=merge-with-order entries (bb~new-entries bb) (bb~ordering bb) equl)))
	     (sugg~output bb 5 "Merging new-entries ~A." (bb~new-entries bb))
	     (setf (bb~new-entries bb) nil)
	     (setf (bb~entries bb) new)
	     (sugg~output bb 10 "My entries are: ~A" new)
	     (unless (and first (funcall equl first (car new)))
	       (sugg~output bb 10 "There's a new best suggestion ~A!" (car new)) 
	       (car new))))
  (:method :after ((bb bb+blackboard))
	   (sugg~trace-output bb "Entries merged. Entries are: ~{~%~A~}" (bb~entries bb))))

(defgeneric bb~add-new-entry (bb entry)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard and a blackboard entry.")
	   (effect  "The entry is added in the new-entry list of the blackboard.")
	   (value   "Undefined."))
  (:method ((bb bb+blackboard) (entries cons))
	   (sugg~output bb 5 "Adding new entries ~A." entries)
	   (setf (bb~new-entries bb)
		 (remove-duplicates               
		  (append (bb~new-entries bb) entries)
		  :test (bb~equality bb)))
	   (sugg~output bb 10 "There are new-entries: ~A." (bb~new-entries bb)))
  (:method ((bb bb+suggestion-blackboard) (entries cons))
	   (sugg~output bb 7 "Adding new entries ~A." entries)
	   (dolist (x entries)
	     (bb~add-new-entry bb x))
	   (sugg~output bb 11 "There are new-entries: ~A." (bb~new-entries bb)))
  (:method ((bb bb+blackboard) (entry bb+entry))
	   (sugg~output bb 5 "Adding new entry ~A." entry)
	   (unless (find entry (bb~new-entries bb) :test (bb~equality bb))
	     (setf (bb~new-entries bb) (cons entry (bb~new-entries bb))))
	   (sugg~output bb 10 "My new-entries are: ~A." (bb~new-entries bb)))
  (:method ((bb bb+suggestion-blackboard) (entry bb+entry))
	   (sugg~output bb 5 "Adding new entry ~A." entry)
	   (setf (bb~new-entries bb)
		 (if (find entry (bb~new-entries bb) :test (bb~equality bb))
		     (substitute-if entry #'(lambda (x) (funcall (bb~equality bb) x entry))
				    (bb~new-entries bb))
		   (cons entry (bb~new-entries bb))))
	   (sugg~output bb 10 "My new-entries are: ~A." (bb~new-entries bb)))
  (:method ((bb bb+blackboard) (entry null))
	   (sugg~output bb 5 "Empty new entry is not passed on."))
  (:method ((bb bb+blackboard) entry)
	   (sugg~output bb 5 "Adding new entry ~A." entry )
	   (unless (find entry (bb~new-entries bb) :test (bb~equality bb))
	     (sugg~output bb 1 "A weird entry (~A) is being passed on!" entry)
	     (setf (bb~new-entries bb) (cons entry (bb~new-entries bb))))
	   (sugg~output bb 10 "My new-entries are: ~A." (bb~new-entries bb)))
  (:method :after ((bb bb+blackboard) entry)
	   (declare (ignore entry))
	   (sugg~trace-output bb "New entries added: ~{~%~A~}" (bb~new-entries bb))))


(defgeneric bb~pop-entry (bb)
  (declare (edited  "06-DEC-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "None.")
	   (value   "Returns its best entry as a list of arguments and the command applicable to them."))
  (:method ((list list))
	   (let ((best (car list)))
	     (when best
	       (if (bb~command-suggestion-p best)
		   (let* ((command (bb~entry-command best))
			  (mapping (bb~entry-mapping (bb~entry-parameters best)))
			  (com-args (bb~com-args (bb~find-blackboard command))))
		     (values (bb=pair-parameters&args mapping com-args) command))
		 (sugg~output t 1 "Don't know what to do with entry ~A!" best)))))
  (:method ((bb bb+suggestion-blackboard))
	   (when (car (bb~entries bb))
	     (let* ((best (car (bb~entries bb)))
		    (command (bb~entry-command best))
		    (mapping (bb~entry-mapping (bb~entry-parameters best)))
		    (com-args (bb~com-args (bb~find-blackboard command))))
	       (values (bb=pair-parameters&args mapping com-args)
		       command))))
  (:method ((bb bb+command-blackboard))
	   (when (car (bb~entries bb))
	     (let* ((best (car (bb~entries bb)))
		    (command (bb~command bb))
		    (mapping (bb~entry-mapping best))
		    (com-args (bb~com-args bb)))
	       (values (bb=pair-parameters&args mapping com-args)
		       command)))))

(defun bb~pop-all-entries (bb)
  (declare (edited  "06-DEC-1999")
	   (authors Sorge)
	   (input   "A command blackboard.")
	   (effect  "None.")
	   (value   "A list of lists of arguments and the command applicable to each of these lists."))
  (let* ((command (bb~command bb))
	 (com-args (bb~com-args bb)))
    (values
     (remove-duplicates 
      (mapcar #'(lambda (entry)
		  (bb=pair-parameters&args (bb~entry-mapping entry) com-args))
	      (bb~entries bb))
      :test #'(lambda (sugg1 sugg2) (every #'eq sugg1 sugg2))) ;;;MP remove duplicates from hidden agents
     command)))


;;; Handling agents

(defgeneric bb~enter-agent (bb agent)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard and an agent.")
	   (effect  "Registers the agent with the blackboard.")
	   (value   "Undefined."))
  (:method ((bb bb+blackboard) agent)
	   (let* ((old-agents (bb~agents bb))
		  (new-agents (cons agent old-agents)))
	     (sugg~output bb 8 "Registering agent ~A." agent)
	     (sugg~output bb 10 "Old agents: ~A." old-agents)
	     (setf (bb~agents bb) new-agents)
	     (sugg~output bb 10 "New agents: ~A." new-agents)))
  (:method ((bb bb+blackboard) (agents list))
	   (dolist (x agents)
	     (bb~enter-agent bb x))))

(defgeneric bb~remove-agent (bb agent)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard and an agent.")
	   (effect  "Removes the agent from the blackboard's agent-list.")
	   (value   "Undefined."))
  (:method ((bb null) agent)
	   (sugg~output agent 1 "Did not have a blackboard assigned!"))
  (:method :before ((bb bb+blackboard) agent)
	   (sugg~output bb 8 "Removing agent ~A." agent))
  (:method ((bb bb+blackboard) (agents list))
	   (dolist (agent agents) (bb~remove-agent bb agent)))
  (:method ((bb bb+blackboard) agent)
	   (sugg~output bb 0 "Don't know how to remove agent ~A!" agent)))

(defgeneric bb~remove-all-agents (bb)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard.")
	   (effect  "Removes all agents from the blackboard.")
	   (value   "Undefined."))
  (:method :after ((bb bb+suggestion-blackboard))
	   (setf (bb~classifier bb) nil))
  (:method ((bb bb+blackboard))
	   (sugg~output bb 2 "Removing all my agents!")
	   (setf (bb~agents bb) nil)
	   (setf (bb~surveyor bb) nil)))

(defgeneric bb~find-agent (bb agent)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard and an agent.")
	   (effect  "None.")
	   (value   "The agent if it is working for the blackboard. O/w NIL."))
  (:method ((bb bb+blackboard) agent)
	   (find agent (bb~agents bb))))

;;; Handling information and resources

(defgeneric bb~enter-information (bb info)
  (declare (edited  "01-DEC-1999")
	   (authors Sorge)
	   (input   "A blackboard and a piece of information.")
	   (effect  "Enters the information into the blackboards information slot.")
	   (value   "The information if it was not yet contained in the information"
		    "slot. Otherwise NIL."))
  (:method ((bb bb+blackboard) (info string))
	   (unless (find info (bb~information bb) :test #'string-equal)
	     (sugg~output bb 8 "Entering new information ~A." info)
	     (push info (bb~information bb))))
  (:method ((bb bb+blackboard) (info symbol))
	   (unless (find info (bb~information bb) :test #'string-equal)
	     (sugg~output bb 8 "Entering new information ~A." info)
	     (push info (bb~information bb))))
  (:method ((bb bb+blackboard) info)
	   (bb~enter-information bb (format nil "~A" info)))
  (:method ((bb bb+blackboard) (infos list))
	   (dolist (info infos)
	     (bb~enter-information bb info))))

(defgeneric bb~remove-information (bb info)
  (declare (edited  "01-DEC-1999")
	   (authors Sorge)
	   (input   "A blackboard and a piece of information.")
	   (effect  "Removes the information from the blackboards information slot.")
	   (value   "The remaining information if INFO was contained in the information"
		    "slot. Otherwise NIL."))
  (:method ((bb bb+blackboard) (info string))
	   (when (find info (bb~information bb) :test #'string-equal)
	     (sugg~output bb 8 "Removing information ~A." info)
	     (setf (bb~information bb)
		   (remove info (bb~information bb) :test #'string-equal))
	     (sugg~output bb 6 "My information is ~A" (bb~information bb))))
  (:method ((bb bb+blackboard) (info symbol))
	   (when (find info (bb~information bb) :test #'string-equal)
	     (sugg~output bb 8 "Removing information ~A." info)
	     (setf (bb~information bb)
		   (remove info (bb~information bb) :test #'string-equal))
	     (sugg~output bb 6 "My information is ~A" (bb~information bb))))
  (:method ((bb bb+blackboard) info)
	   (bb~remove-information bb (format nil "~A" info)))
  (:method ((bb bb+blackboard) (infos list))
	   (dolist (info infos)
	     (bb~remove-information bb info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bb=merge-with-order (list1 list2 order equality)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "Two lists of blackboard entries or single entries.")
	   (effect  "None.")
	   (value   "Ordered concatenation of both lists. In case of duplicate entries"
		    "those from the list1 are removed."))
  (:method ((list1 null) (list2 null) order equality)
	   (declare (ignore order equality)))
  (:method ((list1 null) (list2 cons) order equality)
	   (declare (ignore equality))
	   (sort list2 order))
  (:method ((list1 cons) (list2 null) order equality)
	   (declare (ignore order equality))
	   list1)
  (:method ((list1 null) list2 order equality)
	   (declare (ignore order equality))
	   (list list2))
  (:method (list1 (list2 null) order equality)
	   (declare (ignore order equality))
	   (list list1))
  (:method ((list1 cons) (list2 cons) order equality)
	   (sort (append (remove-if #'(lambda (x)
					(find x list2 :test equality))
				    list1)
			 list2) order))
  (:method (list1 (list2 cons) order equality)
	   (if (find list1 list2 :test equality)
	       list2
	     (sort (cons list1 list2) order)))
  (:method ((list1 cons) list2 order equality)
	   (let ((in1 (find list2 list1 :test equality)))
	     (if in1
		 (substitute list2 in1 list1 :test equality)
	       (sort (cons list2 list1) order))))
  (:method (list1 list2 order equality)
	   (cond ((funcall equality list1 list2) list2)
		 ((funcall order list1 list2) (list list1 list2))
		 (t (list list2 list1)))))

(defun bb=pair-parameters&args (params args)
  (declare (edited  "06-DEC-1999")
	   (authors Sorge)
	   (input   "A list associating argnames to actual parameters and a list argnames.")
	   (effect  "None.")
	   (value   "A list containing parameters and unspecified parameters in an order a command can be applied to."))
  (when args
    (let ((mapping (find-if #'(lambda (x) (string-equal (car args) (car x))) params)))
      (if mapping
	  (cons (cdr mapping) (bb=pair-parameters&args params (cdr args)))
	(cons nil (bb=pair-parameters&args params (cdr args)))))))	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors for blackboard entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bb~create-command-suggestion (command parameters visited)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A command, a parameter suggestion and a list of agent-names.")
	   (effect  "Creates an instance of BB+COMMAND-SUGGESTION.")
	   (value   "The newly created instance."))
  (make-instance 'bb+command-suggestion
		 :command command
		 :parameters parameters
		 :visited visited))

(defun bb~create-parameter-suggestion (mapping status visited)
  (declare (edited  "14-APR-1998")
	   (authors Sorge)
	   (input   "A association-list, a status indicator, and a list of agent-names.")
	   (effect  "Creates an instance of BB+PARAMETER-SUGGESTION.")
	   (value   "The newly created instance."))
  (make-instance 'bb+parameter-suggestion
		 :mapping mapping
		 :status status
		 :visited visited))

(defun bb~create-empty-parameter-suggestion ()
  (bb~create-parameter-suggestion nil nil nil))

(defgeneric bb~make-parameter-suggestion (mapping)
  (declare (edited  "06-DEC-1999")
	   (authors Sorge)
	   (input   "A mapping for a parameter suggestion.")
	   (effect  "Removes nil arguments from the mapping and creates a parameter suggestion.")
	   (value   "The newly create parameter suggestion."))
  (:method (mapping)
	   (sugg~output nil 0 "This: ~A could not be converted into a parameter suggestion!" mapping))
  (:method ((mapping null))
	   mapping)
  (:method ((mapping bb+parameter-suggestion))
	   mapping)
  (:method ((mapping list))
	   (if (every 'consp mapping)
	       (let ((new-mapping (remove-if #'(lambda (x) (null (cdr x))) mapping)))
		 (bb~create-parameter-suggestion new-mapping nil nil))
	     (sugg~output nil 0 "The list ~A could not be converted into a parameter suggestion!" mapping))))
	     
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates for blackboard entries  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bb~entry-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+ENTRY."))
  (typep obj 'bb+entry))

(defun bb~command-suggestion-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+COMMAND-SUGGESTION."))
  (typep obj 'bb+command-suggestion))

(defun bb~parameter-suggestion-p (obj)
  (declare (edited  "20-NOV-1999")
	   (authors Sorge)
	   (input   "A lisp object.")
	   (value   "T if the object is of type BB+PARAMETER-SUGGESTION."))
  (typep obj 'bb+parameter-suggestion))

(defgeneric bb~entry-empty-p (entry)
  (declare (edited  "05-JAN-2000")
	   (authors Sorge)
	   (input   "A blackboard entry.")
	   (effect  "None.")
	   (value   "T if the entry is the empty entry."))
  (:method ((entry bb+command-suggestion))
	   (null (bb~entry-parameters entry)))
  (:method ((entry bb+parameter-suggestion))
	   (null (bb~entry-mapping entry)))
  (:method ((entry list))
	   (null entry))
  (:method (entry)
	   (sugg~output t 0 "~A is not a blackboard entry!" entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulation of blackboard entries 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Adding entries to parameter suggestions

(defgeneric bb~add-single-entry (entry extension comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard entry, another element for the entry,"
		    "and a list of command-arguments.")
	   (effect  "None.")
	   (value   "A new blackboard entry where the old entry is extended by the new element."))
  (:method ((entry bb+parameter-suggestion) (extension cons) (comargs cons))
	   (let* ((mapping (bb~entry-mapping entry))
		  (new-mapping (bb=extend-entry-by-one extension mapping comargs)))
	     (bb~create-parameter-suggestion new-mapping
					     (= (length new-mapping) (length comargs))
					     nil))))

(defgeneric bb~add-list-of-single-entries (entry extension-list comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard entry, a list of new single element for the entry,"
		    "and a list of command-arguments.")
	   (effect  "None.")
	   (value   "A list of new blackboard entries where each consists of the old entry"
		    "extended by a single element of the list."))
  (:method ((entry bb+parameter-suggestion) (extension cons) (comargs cons))
	   (let* ((mapping (bb~entry-mapping entry))
		  (new-mappings (bb=extend-entries-by-one extension mapping comargs)))
	     (mapcar #'(lambda (new-mapping)
			 (bb~create-parameter-suggestion new-mapping
							 (= (length new-mapping) (length comargs))
							 nil))
		     new-mappings))))

(defgeneric bb~add-list-entries (entry extension-list comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard entry, a list of new elements for the entry,"
		    "and a list of command-arguments.")
	   (effect  "None.")
	   (value   "A new blackboard entry containing the old entry extended by"
		    "the list of new elements."))
  (:method ((entry bb+parameter-suggestion) (extension cons) (comargs cons))
	   (let* ((mapping (bb~entry-mapping entry))
		  (new-mapping (bb=extend-entry-by-list extension mapping comargs)))
	     (bb~create-parameter-suggestion new-mapping
					     (= (length new-mapping) (length comargs))
					     nil))))

(defgeneric bb~add-list-of-list-entries (entry extension-list comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard entry, a list of new element lists for the entry,"
		    "and a list of command-arguments.")
	   (effect  "None.")
	   (value   "A list of new blackboard entries where each consists of the old entry"
		    "extended by a list of elements of the extension-list."))
  (:method ((entry bb+parameter-suggestion) (extension cons) (comargs cons))
	   (let* ((mapping (bb~entry-mapping entry))
		  (new-mappings (bb=extend-entries-by-list extension mapping comargs)))
	     (mapcar #'(lambda (new-mapping)
			 (bb~create-parameter-suggestion new-mapping
							 (= (length new-mapping) (length comargs))
							 nil))
		     new-mappings))))

(defun bb=extend-entry-by-one (extension entry comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A acons cell, an alist, and a list command arguments.")
	   (effect  "None.")
	   (value   "The alist extended with the acons cell at the position"
		    "indicated by the comargs-list."))
  (cond ((not (find (car extension) comargs :test #'string-equal))
	 (sugg~output t 1 "The CAR of the new suggestion ~A doesn't make sense." extension)
	 (bb=extend-entry-by-one extension entry (cons (car extension) comargs))) ;;MP extension for hidden agents
	((null comargs) nil)
	((null entry) (list extension))
	((and (string-equal (car comargs) (caar entry))
	      (string-equal (car comargs) (car extension)))
	 (sugg~output t 3 "No destructive changes possible within blackboard entries.")
	 entry)
	((string-equal (car comargs) (caar entry))
	 (cons (car entry) (bb=extend-entry-by-one extension (cdr entry) (cdr comargs))))
	((string-equal (car comargs) (car extension)) 
	 (cons extension entry))
	(t (bb=extend-entry-by-one extension entry (cdr comargs)))))
  
(defun bb=extend-entries-by-one (extension entry comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "Two alists and a list of command arguments.")
	   (effect  "None.")
	   (value   "A list of alists where each element is extended by one"
		    "of the elements of the first alist."))
  (mapcar #'(lambda (new-entry)
	      (bb=extend-entry-by-one new-entry entry comargs))
	  extension))

(defun bb=extend-entry-by-list (extension entry comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "Two alists and a list of command arguments.")
	   (effect  "None.")
	   (value   "The second alist is extended by the first alist."))
  (if extension
      (bb=extend-entry-by-list
       (cdr extension)
       (bb=extend-entry-by-one (car extension) entry comargs)
       comargs)
    entry))

(defun bb=extend-entries-by-list (extension entry comargs)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A list of alist, an alist, and a list of command arguments.")
	   (effect  "None.")
	   (value   "The second alist is extended by each of the first alists."))
  (mapcar #'(lambda (new-entry)
	      (bb=extend-entry-by-list new-entry entry comargs))
	  extension))

;;; Handling Agent visits

(defgeneric bb~entry-visited-p (entry agent)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "A blackboard entry and an agent.")
	   (effect  "None.")
	   (value   "T if the entry was already visited by the agent. O/w NIL."))
  (:method ((entry bb+entry) (agent string))
	   (find agent (bb~entry-visited entry) :test #'string-equal))
  (:method ((entry bb+entry) (agent symbol))
	   (find agent (bb~entry-visited entry) :test #'string-equal)))
  
(defgeneric bb~agent-visited-entry (agent entry)
  (declare (edited  "22-NOV-1999")
	   (authors Sorge)
	   (input   "An agent and a blackboard entry.")
	   (effect  "Extends the list of visitors of the entry by agent.")
	   (value   "The extended visitor list."))
  (:method (agent (entries list))
	   (mapcar #'(lambda (entry)
		       (bb~agent-visited-entry agent entry))
		   entries))
  (:method ((agent string) (entry bb+entry))
	   (unless (find agent (bb~entry-visited entry) :test #'string-equal)
	     (sugg~output t 4 "Updating visitor list ~A with ~A." entry agent)
	     (pushnew (string-upcase agent) (bb~entry-visited entry)))
	   (bb~entry-visited entry))
  (:method ((agent symbol) (entry bb+entry))
	   (bb~agent-visited-entry (symbol-name agent) entry)))
	   
(defun bb~agents-done-p (bb)
  (declare (edited  "29-MAY-2000")
	   (authors Mth/Sorge)
	   (input   "A command blackboard.")
	   (effect  "None.")
	   (value   "A list of all active agents working for the blackboard."))
  (if rsrc*use-resources
      ;; if resource adaption active then use this function
      (not (remove-if-not #'(lambda (entry)
			      ;; is there an agent which has not visited entry ?
			      (find-if-not #'(lambda (agent) (bb~entry-visited-p entry agent))
					   (bb~active-agents bb)))
			  (bb~entries bb)))
    ;; else use the old code.
    ;; some problems may occur if flag is changed during one automate session
    (let ((agents-number (length (bb~agents bb)))
	  (entries-visited (mapcar #'bb~entry-visited (bb~entries bb))))
      (every #'(lambda (x)
		 (= (length x) agents-number))
	     entries-visited))))    
    
;; (defun bb~agents-done-p (bb)
;;  (declare (edited  "29-DEC-1999")
;;	   (authors Sorge)
;;	   (input   "A command blackboard.")
;;	   (effect  "None.")
;;	   (value   "T if all of the blackboard's entries have been visited by all"
;;		    "agents of the blackboard."))
;;  (let ((agents-number (length (bb~agents bb)))
;;	(entries-visited (mapcar #'bb~entry-visited (bb~entries bb))))
;;    (every #'(lambda (x)
;;	       (= (length x) agents-number))
;;	   entries-visited)))    

(defun bb~active-agents (bb)
  (declare (edited  "25-MAY-2000")
	   (authors Mth)
	   (input   "A command blackboard.")
	   (effect  "None.")
	   (value   "The list of all active agents of the blackboard."))
 (remove-if-not #'(lambda (x) (rsrc~running-state (agent~resources x))) (bb~agents bb)))


(defun bb~active-blackboards (blackboard-list)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "A list of blackboards.")
	   (effect  "None.")
	   (value   "A sublist of the input list containing the blackboard with a"
		    "working surveyor agent."))
  (remove-if-not #'(lambda (x) (rsrc~running-state (agent~resources (bb~surveyor x))))
		 blackboard-list))

;; (defun bb~agents-done-p (bb)
 ;; (declare (edited  "29-DEC-1999")
;;	   (authors Sorge)
;;	   (input   "A command blackboard.")
;;	   (effect  "None.")
;;	   (value   "T if all of the blackboard's entries have been visited by all"
;;		    "active agents of the blackboard."))
;;  (let ((agents-number (length (bb~active-agents bb)))
;;	(entries-visited (mapcar #'bb~entry-visited (bb~entries bb))))
;;    (every #'(lambda (x)
;;	       (= (length x) agents-number))
;;	   entries-visited)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting functions for blackboard entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here should go a nice little mechanism to dynamically specify
;; sorting criteria. Moreover information from the chronological
;; focus should be used.
;; The idea is something like:
;; (1) Criteria can be specified as binary predicates
;; (2) An order on these criteria can be defined (and also changed)
;; (3) The generic less and equal functions are then simply a 
;;     disjunction, respecitively conjunction, of these criteria
;;     in the specified order.
;;     (do we need dynamic equality??)   VS                   <------ to be done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bb*ordering-command-suggestion-equal nil
  "A priority queue with symbols specifying the ordering of methods
   to determine equality of command suggestions.")

(defvar bb*ordering-parameter-suggestion-equal nil
  "A priority queue with symbols specifying the ordering of methods
   to determine equality of parameter suggestions.")

(defvar bb*ordering-command-suggestion-leq nil
  "A priority queue with symbols specifying the ordering of methods
   to determine the leq relation between command suggestions.")

(defvar bb*ordering-parameter-suggestion-leq nil
  "A priority queue with symbols specifying the ordering of methods
   to determine the leq relation between command suggestions.")

(defun bb~set-parameter-suggestion-ordering (list)
  (setf bb*ordering-parameter-suggestion-leq list))
  
(defun bb~set-parameter-suggestion-equality (list)
  (setf bb*ordering-parameter-suggestion-equal list))
  
(defun bb~set-command-suggestion-ordering (list)
  (setf bb*ordering-command-suggestion-leq list))
  
(defun bb~set-command-suggestion-equality (list)
  (setf bb*ordering-command-suggestion-equal list))

(defgeneric bb~parameter-suggestion-leq-p (sugg1 sugg2 indicator)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "Two parameter suggestions and a symbol.")
	   (effect  "None.")
	   (value   "T if SUGG1 is leq than SUGG2.")
	   (remark  "The methods of this generic function are used to determine the relation"
		    "between two different parameter suggestions. Different heuristcs"
		    "can be encoded and refered to by the indicator. A priority list of"
		    "these indicators can then be specified to determine which of the single"
		    "methods are to be used and in which combination."))
  (:method (sugg1 sugg2 indicator)
	   (declare (ignore sugg1 sugg2 indicator))))
	   

(defgeneric bb~command-suggestion-leq-p (sugg1 sugg2 indicator)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "Two command suggestions and a symbol.")
	   (effect  "None.")
	   (value   "T if SUGG1 is leq than SUGG2.")
	   (remark  "This function is used analogously to BB~PARAMETER-SUGGESTION-LEQ-P."))
  (:method (sugg1 sugg2 indicator)
	   (declare (ignore sugg1 sugg2 indicator))))

(defgeneric bb~parameter-suggestion-equal-p (sugg1 sugg2 indicator)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "Two parameter suggestions and a symbol.")
	   (effect  "None.")
	   (value   "T if SUGG1 is equal to SUGG2.")
	   (remark  "This function is used analogously to BB~PARAMETER-SUGGESTION-LEQ-P."))
  (:method (sugg1 sugg2 indicator)
	   (declare (ignore sugg1 sugg2 indicator))
	   t))

(defgeneric bb~command-suggestion-equal-p (sugg1 sugg2 indicator)
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "Two command suggestions and a symbol.")
	   (effect  "None.")
	   (value   "T if SUGG1 is equal to SUGG2.")
	   (remark  "This function is used analogously to BB~PARAMETER-SUGGESTION-LEQ-P."))
  (:method (sugg1 sugg2 indicator)
	   (declare (ignore sugg1 sugg2 indicator))
	   t))

(defgeneric bb~entry-greater-p (entry1 entry2)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "Two blackboard entries.")
	   (effect  "None.")
	   (value   "T if entry1 is greater with respect to given criteria. NIL o/w."))
  (:method ((entry1 bb+parameter-suggestion) (entry2 bb+parameter-suggestion))
	   (dolist (indicator bb*ordering-parameter-suggestion-leq)
	     (unless (bb~parameter-suggestion-leq-p entry1 entry2 indicator)
	       (return-from bb~entry-greater-p t))))
  ;; sorting criteria with respect to the chronological focus
  (:method ((entry1 bb+command-suggestion) (entry2 bb+command-suggestion))
	   (dolist (indicator bb*ordering-command-suggestion-leq)
	     (unless (bb~command-suggestion-leq-p entry1 entry2 indicator)
	       (return-from bb~entry-greater-p t)))))

(defgeneric bb~entry-equal-p (entry1 entry2)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "Two blackboard entries.")
	   (effect  "None.")
	   (value   "T if the entries are considered equal, o/w NIL."))
  (:method ((entry1 bb+parameter-suggestion) (entry2 bb+parameter-suggestion))
	   (dolist (indicator bb*ordering-parameter-suggestion-equal)
	     (unless (bb~parameter-suggestion-equal-p entry1 entry2 indicator)
	       (return-from bb~entry-equal-p nil)))
	   t)
  (:method ((entry1 bb+command-suggestion) (entry2 bb+command-suggestion))
	   (dolist (indicator bb*ordering-command-suggestion-equal)
	     (unless (bb~command-suggestion-equal-p entry1 entry2 indicator)
	       (return-from bb~entry-equal-p nil)))
	   t))


(defun bb~available-sorting-criteria (&optional (stream t))
  (declare (edited  "30-NOV-1999")
	   (authors Sorge)
	   (input   "Optionally a stream.")
	   (effect  "Prints the available sorting criteria to stream.")
	   (value   "None."))
  (flet ((get-indicators (function)
	     (mapcar #'clos::eql-specializer-object
		     (remove-if-not #'clos::eql-specializer-p
				    (mapcar #'(lambda (method)
						(third (sys~method-specializers method)))
					    (sys~generic-function-methods function))))))
    (let ((plq (get-indicators 'bb~parameter-suggestion-leq-p))
	  (peq (get-indicators 'bb~parameter-suggestion-equal-p))
	  (clq (get-indicators 'bb~command-suggestion-leq-p))
	  (ceq (get-indicators 'bb~command-suggestion-equal-p)))
      (format stream "Possible Sorting Criteria are:~%")
      (format stream "Parameter Suggestion Equality:~{ ~S~}~%" peq)
      (format stream "Parameter Suggestion Ordering:~{ ~S~}~%" plq)
      (format stream "Command Suggestion Equality:  ~{ ~S~}~%" ceq)
      (format stream "Command Suggestion Ordering:  ~{ ~S~}~%" clq)
      (values))))


;;; Sorting Criteria for Parameter Suggestions

(defmethod bb~parameter-suggestion-leq-p
  ((sugg1 bb+parameter-suggestion) (sugg2 bb+parameter-suggestion) (indicator (eql :status)))
  (not (bb~entry-status sugg1)))

(defmethod bb~parameter-suggestion-leq-p
  ((sugg1 bb+parameter-suggestion) (sugg2 bb+parameter-suggestion) (indicator (eql :length)))
  (<= (length (bb~entry-mapping sugg1))
      (length (bb~entry-mapping sugg2))))

;;; Equality Criteria for Parameter Suggestions

(defmethod bb~parameter-suggestion-equal-p
  ((sugg1 bb+parameter-suggestion) (sugg2 bb+parameter-suggestion) (indicator (eql :mapping)))
  (let ((map1 (bb~entry-mapping sugg1))
	(map2 (bb~entry-mapping sugg2)))
    (bb=parameter-suggestion-equal map1 map2)))

;;; Sorting Criteria for Command Suggestions

(defmethod bb~command-suggestion-leq-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :rating)))
  (>= (bb=command-rating sugg1) (bb=command-rating sugg2)))

(defmethod bb~command-suggestion-leq-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :null-positions)))
  (>= (bb=null-positions-weighted sugg1) (bb=null-positions-weighted sugg2)))

(defmethod bb~command-suggestion-leq-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :position-weights)))
  (>= (bb=position-weights sugg1) (bb=position-weights sugg2)))

;;; Equality Criteria for Command Suggestions

(defmethod bb~command-suggestion-equal-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :command)))
  (equal (bb~entry-command sugg1) (bb~entry-command sugg2)))
  

;;;;;;vvvvvvvvvvvv BEGIN OMEGA SPECIFIC vvvvvvvvvvv  REDO !!! vvvvvvvvvvvvvvv;;;;;;;

(defun bb=null-positions-weighted (sugg)
  (let ((command (bb~entry-command sugg))
	(parameters (bb~entry-mapping (bb~entry-parameters sugg))))
    (- (length (com~argnames command))   ;;; VS: Keim-free with getting information via command blackboards. <---- to be done.
       (length parameters))))            ;;; CB: Weighting of null-positions possible?                       <---- to be done.


(defun bb=position-weights (sugg)
  (apply #'+ (mapcar #'(lambda (x)
			 (if (node~p x)     ;;;  VS: KEIM specific !!!!!!!!!!!!!!!!!!!!!!!    <---- to be done.
			     (bb=lineorder-pos x)
			   0))
		     (bb~entry-mapping (bb~entry-parameters sugg)))))
	 
(defun bb=lineorder-pos (x)
  (do* ((list (foci~lineorder foci*pcs) (cdr list))
	(el (car list) (car list))
	(num 0 (incf num)))
      ((or (eq x el) (null list)) num)))
	   


(defun bb=command-rating (sugg)
  (let ((command (bb~entry-command sugg)))
    (or
     (bb=dual-rule-p command)
     (bb=symmetric-rule-p command)
     (com~level command))))
  

(defvar bb*symmetric-rules '((ander . andel) (oril . orir) (equiver . equivel)))
(defun bb=symmetric-rule-p (x)
  (when comint*current-comint
    (let ((symmrule (or (cdr (assoc (keim~name x) bb*symmetric-rules :test #'string-equal))
			(car (rassoc (keim~name x) bb*symmetric-rules :test #'string-equal)))))
      (when (string-equal (keim~name (comint~last-comm comint*current-comint)) symmrule)
	100))))

(defvar bb*dual-rules '((or2imp . imp2or) (=2equiv . equiv2=) (pullneg . pushneg)))
(defun bb=dual-rule-p (x)
  (when comint*current-comint
    (let ((dualrule (or (cdr (assoc (keim~name x) bb*dual-rules :test #'string-equal))
			(car (rassoc (keim~name x) bb*dual-rules :test #'string-equal)))))
      (when (string-equal (keim~name (comint~last-comm comint*current-comint)) dualrule)
	100))))

(defun bb=parameter-suggestion-equal (map1 map2)
  (and (= (length map1) (length map2))
       (every #'(lambda (x y)
		  (or (equal x y)
		      (keim~equal x y)))
	      map1 map2)))

;;;;;;^^^^^^^^^^^^ END OMEGA SPECIFIC ^^^^^^^^^^^  REDO !!! ^^^^^^^^^^^^^^^;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spcializations for methods from other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sugg~output ((obj bb+blackboard) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Blackboard ~A [~A]: " (bb~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output ((obj bb+command-blackboard) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Command Blackboard ~A [~A]: " (bb~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~output ((obj bb+suggestion-blackboard) level string &rest args)
  (when (<= level sugg*output)
    (format t ";;;Suggestion Blackboard ~A [~A]: " (bb~name obj) level)
    (sugg~output-message level string args)))

(defmethod sugg~trace-output ((obj bb+blackboard) string &rest args)
  (format t "Blackboard ~A: ~A~%" (bb~name obj) (apply #'format (cons nil (cons string args)))))

(defmethod sugg~reset-trace-object ((obj bb+blackboard))
  (bb~find-blackboard (bb~name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keim and OMEGA specific functionality
;; (this will need to be replaced, if the mechanism is ported)
;; We try to do as much as possible without Keim specific functions.
;; Where this cannot be avoided, we try to define generic functions 
;; and implement specific methods for particular Keim objects.
;; Where all this fails, the functions will have to be reimplemented.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Methods specializing on KEIM objects


;;; MALTE
(defmethod bb~make-command-blackboard ((command com+command))  ;;; This function could go within one of the
  (let* ((com-name (keim~name command))                        ;;; other methods. However it is more efficient
	 (name (etypecase com-name                             ;;; to implement it like this,             VS
		 (string (string-upcase com-name))
		 (symbol (symbol-name com-name))))
	 (com-args (com~argnames command))
	 (resource (make-instance 'rsrc+command-bb-resource :name command))
	 (bb (bb~create-command-blackboard name command nil nil nil 
					   #'bb~entry-greater-p
					   #'bb~entry-equal-p
					   resource nil com-args)))
    (bb~register-blackboard bb)
    (sugg~output nil 2 "Blackboard for command ~A created." name)
    bb))

(defmethod bb~find-blackboard ((command com+command))
  (bb~find-blackboard (keim~name command)))

(defmethod bb~remove-blackboard ((com com+command))
  (bb~remove-blackboard (keim~name com)))

(defmethod bb~enter-information ((bb bb+blackboard) (info keim+name))
  (bb~enter-information bb (keim~name info)))

(defmethod bb~remove-information ((bb bb+blackboard) (info keim+name))
  (bb~remove-information bb (keim~name info)))

(defmethod bb~reset ((com com+command) &optional entry)
  (bb~reset (keim~name com) entry))

;;; Functions that need to be KEIM specific

(defgeneric bb~find-command (command)
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "A name of a command.")
	   (effect  "None.")
	   (value   "The command with this name or NIL if no command exists with this name."))
  (:method ((command string))
	   (find command
		 (mapcan #'(lambda (frag)
			     (sugg~hash2list (com~frag-direct-commands frag)))
			 (sugg~hash2list com*fragment-hash-table))
		 :test #'(lambda (x y) (string-equal x (keim~name y)))))
  (:method ((command symbol))
	   (bb~find-command (symbol-name command)))
  (:method ((command com+command))
	   (bb~find-command (keim~name command))))

;;;;;;;;;;;;;
;; Print Object methods (to be cleaned up!!!!)
;;;;;;;;;;;;;


(defmethod print-object ((obj bb+parameter-suggestion) stream)
  (let ((mapping (apply #'append (mapcar #'(lambda (x)
					     (list (car x)
						   (if (pdsn~p (cdr x))
						       (format nil "~A: ~A"
							       (keim~name (cdr x))
							       (node~formula (cdr x)))
						     (cdr x))))
					 (bb~entry-mapping obj)))))
    (format stream "<~{[~A: ~A]~}>" mapping)))

(defmethod print-object ((obj bb+command-suggestion) stream)
  (format stream "~A: ~A" (keim~name (bb~entry-command obj)) (bb~entry-parameters obj)))


(defmethod print-object ((obj bb+command-blackboard) stream)
  (format stream "<Command-BB ~A>" (bb~name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for distribute/adapting resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bb~get-sum-of-blackboard-resources (blackboard)
  (declare (edited  "03-MAY-2000")
	   (authors Mth)
	   (input   "A blackboard.")
	   (effect  "None.")
	   (value   "The sum of the resources of the agents working for the underlying blackboard."))
  (:method ((blackboard bb+blackboard))
	   (let ((sum 0))
	     (mapcar
	      #'(lambda (x) (incf sum (rsrc~act-resources x)))
	      (rsrc~agent-data (bb~resources))))))



