;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automate Module
;;                     (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the functionality for OMEGA-Ants.
;; OMEGA-Ants is an automation and backtracking wrapper for the
;; command suggestion mechansim used in OMEGA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :omega)


(mod~defmod AUTO 
            :uses (agent bb com csm foci just keim mixin nic omega opr parse pds pdsc proc rule socket sugg tac)
            :documentation "OMEGA-Ants: Automation and backtracking wrapper for the command suggestion mechanism."
            :exports (
                      
                      auto~prove
                      auto~resume
                      auto~set-criteria
                      auto~stop
                      auto~suspend
                      auto~trace
                      auto~untrace
                      
                      auto*backtracked
                      auto*command-bbs
                      auto*default-interval
                      auto*deleted
                      auto*dummies
                      auto*history
                      auto*in-use
                      auto*process
                      auto*suggestions
                      auto*verbose))

;;; The following functions are internal in other modules and should not be used:
;;; (oc=open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar auto*verbose nil
  "Verbose output of the automate process.")

(defvar auto*history nil
  "A variable for storing already computed suggestions.")

(defvar auto*default-interval 2
  "Time in second for a single step of the mechanism.")

(defvar auto*in-use nil
  "Automation in use or not.")

(defvar auto*process nil
  "The anchor for the automation process.")

(defvar auto*suggestions nil
  "The currently available best suggestions provided by some suggestion agent.")

(defvar auto*deleted nil
  "A temporary list of deleted nodes (for backtracking purposes).")

(defvar auto*backtracked nil
  "A flag indicating whether the automation can immediately proceed as we backtracked.")

(defvar auto*command-bbs nil)

(defvar auto*dummies '(:nicdummy-or-e-l :NICDUMMY-OR-E-r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto=message (&rest args)
  (when auto*verbose (apply #'omega~trace args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run function for the automate process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto~set-auto-default-interval (num)
  (setq auto*default-interval num))		

(defun auto=time-bound-p (time)
  (>= (- (get-universal-time) time) auto*default-interval))

(defun greq (a b)
  (>= a b))

(defun auto=run-function ()
  (declare (edited  "05-JAN-2000")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "The run function for the automation of the suggestion mechanism."
		    "Loops infinitely, until stopped or a proof is finished.")
	   (value   "Undefined."))
  (csm~quit)
  (proc~wait "Waiting for suggestion mechanism to stop" #'(lambda () (not agent*processes)))
  (csm~initialize #'auto=suggestion)         ;;; maybe intialize with different heuristics!!!!
  (proc~wait "Waiting for mechanism to reset." #'(lambda () (or (not (foci~active-pc))
								(and agent*processes (csm~reset-p)))))
  (sugg~output t 3 "Automation sequence commencing.")
  ;;;(meth~remove-methods)
  (setf meth*normalizing-methods nil)
  (setf meth*restricting-methods nil)
  ;; seems not to have any influence on automate-behaviour MH 29/05/00 
  (multiple-value-bind (coms blackboards agents)
      (csm~considered-commands)
    (declare (ignore coms agents))
    (let* ((auto*command-bbs blackboards)
	   (time (get-universal-time))
	   (reset sugg*reset))
      (loop
       (sugg~output t 4 "Reset: ~A  Sugg*reset: ~A  Auto*in-use: ~A  Interval: ~D"
		    reset sugg*reset auto*in-use (- (get-universal-time) time))
       (proc~wait "Waiting to execute command."
		  #'(lambda () (or (and (not (equal reset sugg*reset)) (progn (auto=message "Reset case: 1") t))
				   (and (not auto*in-use) (progn (auto=message "Reset case: 2") t))
				   (and (not (foci~active-pc)) (progn (auto=message "Reset
case: 3") t))
				   ;; if resources are used, check only blackboards
				   ;; with an active command agent. MH
				   (and (auto=check-bbs (if rsrc*use-resources
							    (bb~active-blackboards
							     blackboards)
							  blackboards)) (progn (auto=message "Reset case: 4") t))
				   (and (greq (- (get-universal-time) time)
					    auto*default-interval)
					(progn (auto=message "Reset case: 5") t)))))
       (cond ((not auto*in-use)              ;; automation stopped
	      (return (auto=message "Quitting automation process.")))

	     ((not (equal reset sugg*reset)) ;; mechanism externally reset
	      (omega~warn "The proof has been changed or the suggestion mechnism reseted despite automation running.")
	      (auto=push-history auto*command-bbs t)
	      (setf auto*suggestions nil))		
	     (auto*suggestions 
	      (multiple-value-bind (arguments command)
		  (bb~pop-entry auto*suggestions)
		(opr~enqueue-command
		 (opr~normalize-command command
					:args arguments
					:process (proc~actual-process)))
		(unless (find (keim~name command) auto*dummies :test #'string-equal)
		  (auto=push-history auto*command-bbs)))
	      (setf auto*suggestions nil))
	     ((foci~active-pc)
	      (unless (auto=backtracking auto*command-bbs)
		(return (auto=message "Automate could not find a proof.")))
	      (setf auto*backtracked t))
	     (t (return (auto=message "Proof successfully finished."))))
       (proc~wait "Waiting for mechanism to reset."
		  #'(lambda ()
		      (or auto*backtracked
			  (and (not (equal reset sugg*reset)) (csm~reset-p))
			  (not auto*in-use)
			  (not (foci~active-pc)))))
       ;;     (when (pds~label2node 'l17)
       ;;       (let* ((node (pds~label2node 'l17))
       ;;              (just (node~justification node)))
       ;;         (format t "Node L17: Supps:  ~A  Reasons:  ~A   Other Reasons: ~A"
       ;;                 (pds~node-supports node)
       ;;                 (pdsj~own-reason just)
       ;;                 (pdsj~other-reasons just))))
       ;;     (when (pds~label2node 'l18) (print (pds~node-supports (pds~label2node 'l18))))
       ;;     (when (pds~label2node 'l19) (print (pds~node-supports (pds~label2node 'l19))))
       ;;     (oc=show-pds)
       (when (not auto*in-use) 
	 (return (auto=message "Quitting automation process.")))
       (unless (foci~active-pc)
	 (return (auto=message "Proof successfully finished.")))
       (setf reset sugg*reset)
       ;; reset the external provers :
       (mapcar #'agent~reset-external-prover agent*external-prover-agents)
       (setf agent*external-prover-agents NIL)
       (sugg~output t 2 "Resetted external provers.")
       (setf auto*backtracked nil)
     ;;;(proc~wait "Waiting for manual reset" #'(lambda () (not (equal reset2 auto*reset))))
       (setf time (get-universal-time)))
      (auto~stop))))
	   

;;; Querying for the agents to have finished

(defun auto=test-1-p (bbs)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "A list of command blackboards.")
	   (effect  "None.")
	   (value   "T if all agents which are working for the blackboards have regarded"
		    "all entries on their command blackboard, NIL o/w."))
  (every #'bb~agents-done-p bbs))

(defun auto=test-2-p (bbs)
  (declare (edited  "27-MAY-2000")
	   (authors Mth)
	   (input   "A list of command blackboards.")
	   (effect  "None.")
	   (value   "T if all new-entries have been merged with the old entries, NIL o/w."))
  (notany #'bb~new-entries bbs))
   

(defun auto=test-3-p (best-entries)
  (= (length best-entries) (length auto*suggestions)))

(defun auto=test-4-p (best-entries)
  (every #'(lambda (x)
			 (let* ((sugg (find (car x) auto*suggestions
					    :test #'(lambda (y z) (string-equal y (keim~name (bb~entry-command z))))))
				(parameters (bb~entry-mapping (bb~entry-parameters sugg))))
			   (= (length parameters) (length (bb~entry-mapping (cdr x)))))) best-entries))

(defun auto=best-ents (bbs)
  (mapcan
   #'(lambda (bb)
       (let ((fentry (car (bb~entries bb))))
	 (unless (bb~entry-empty-p fentry)
	   (list (cons (bb~name bb) fentry))))) bbs))

(defun auto=check-bbs (bbs)
  (declare (edited  "05-JAN-2000")
	   (authors Sorge)
	   (input   "A list of blackboards.")
	   (effect  "None.")
	   (value   "T if the blackboard agents and the suggestion agent have"
		    "finished."))
  (and (auto=test-1-p bbs)   ;;; All parameter agents are done
       (auto=test-2-p bbs)   ;;; All new entries were propagated
       (let ((best-entries (auto=best-ents bbs)))
	 (and (auto=test-3-p best-entries)
	      (auto=test-4-p best-entries)))))

	       ;; this could be more elaborate

;;; Suggestion function for the suggestion agent

(defun auto=suggestion (sugg)
  (if (mixin~activep)
      (socket~write (format nil "updateSuggestions(~A)"
			    (parse~list-of-strings
			     (mapcar #'(lambda (entry)
					 (keim~name (bb~entry-command entry)))
				     sugg))) :inout)
    (auto=message "Suggestions: ~{       ~A~%~}~%" sugg))
  (setf auto*suggestions sugg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backtracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto=backtracking (&optional (cbbs auto*command-bbs))
  (declare (edited  "05-JAN-2000")
	   (authors Sorge)
	   (input   "A list of command blackboards.")
	   (effect  "Performs a backtracking step for the automation mechanism.")
	   (value   "T if backtracking was possible, o/w NIL."))
  (auto=message "Backtracking!")
  ;;;(print auto*history)
  (when auto*history
    (auto=remove-last-step omega*current-proof-plan)
    (let ((last-suggestion (auto=remove-deleted (pop auto*history))))
      (if cbbs
	  (mapc #'auto=reset-blackboard last-suggestion cbbs)
	(dolist (sugg last-suggestion)
	  (auto=reset-blackboard sugg (bb~find-blackboard (car sugg)))))
      (setf auto*deleted nil)
      t)))

(defun auto=remove-deleted (suggestion)
  (let ((deleted (auto=manage-deleted-list)))
    (mapcar #'(lambda (suggs)
		(cons (car suggs)
		      (mapcan #'(lambda (sugg)
				  (unless (some #'(lambda (x)
						    (find (cdr x) deleted
							  :test #'keim~equal))
						(bb~entry-mapping sugg))
				    (list sugg)))
			      (cdr suggs))))
	    suggestion)))

(defun auto=manage-deleted-list ()
  (remove-duplicates
   (apply #'append
	  (mapcar #'(lambda (x)
		      (just~premises (pdsc~an-just x)))
		  auto*deleted))
   :test #'keim~equal))

(defun auto=reset-blackboard (sugg bb)
  (cond
   ((and bb (string-equal (bb~name bb) 'nictac-or-e))
    (bb~reset bb))
   (bb
    (setf (bb~entries bb) nil)
    (bb~add-new-entry bb (cdr sugg)))))

(defun auto=remove-last-step (&optional (plan omega*current-proof-plan))
  (declare (edited  "08-JAN-2000")
	   (authors Sorge)
	   (input   "A PDS.")
	   (effect  "Removes the step introduced last into the PDS.")
	   (value   "Undefined."))
  (when (pds~last-plan-step plan)
    (let* ((last (pds~last-plan-step plan))
	   (open-node (pdsc~an-node last)))
      (auto=message "Opening nodes: ~{~A ~}" open-node)
      (oc=open open-node plan))))

;;     (old-pcs foci*pcs)
;; (proc~wait "Waiting for foci to be recomputed."
;;					    #'(lambda () (not (tree-equal old-pcs foci*pcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing proof history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun auto=push-history (command-bbs &optional (first nil))
  (declare (edited  "05-JAN-2000")
	   (authors Sorge)
	   (input   "A list of command blackboards and a boolean.")
	   (effect  "Pushes computed suggestions on the history stack for backtracking."
		    "If first is NIL it omits the very first suggestion as already computed.")
	   (value   "Undefined."))
  (let ((history (auto=get-full-history command-bbs)))
    (if first
	(push history auto*history)
      (push (auto=remove-first-suggestion history) auto*history))
    ;;;(print auto*history)
    ))
  
(defun auto=get-full-history (command-bbs)
  (auto=message "Getting full history.")
  (mapcar #'(lambda (cbb) (cons (bb~name cbb) (bb~entries cbb))) command-bbs))

(defun auto=remove-first-suggestion (history &optional (suggestions auto*suggestions))
  (auto=message "Removing first suggestion.")
  (let* ((best (car suggestions))
	 (command (keim~name (bb~entry-command best)))
	 (sugg (bb~entry-parameters best))
	 (bb-suggs (assoc command history :test #'string-equal))
	 (new-suggs (auto=remove-subsumed-suggestions sugg (cdr bb-suggs) (car bb-suggs))))
    (substitute-if (cons command new-suggs)
		   #'(lambda (x) (string-equal (car x) command))
		   history)))

(defun auto=remove-subsumed-suggestions (sugg suggs command)
  (declare (edited  "07-JAN-2000")
	   (authors Sorge)
	   (input   "A parameter suggestion and a list of suggestions and a command-name.")
	   (effect  "None.")
	   (value   "A list where the suggestion and all those it subsumes are remove from SUGGS."))
  (let ((agent-length (length (bb~agents (bb~find-blackboard command)))))
    (mapcan #'(lambda (x)
		(unless (and (auto=suggestion-fully-visited x agent-length)
			     (auto=suggestion-subsumes sugg x))
		  (list x)))
	    suggs)))

(defun auto=suggestion-fully-visited (sugg length)
  (= (length (bb~entry-visited sugg)) length))
	      
(defun auto=suggestion-subsumes (sugg1 sugg2)   
  (declare (edited  "08-JAN-2000")
	   (authors Sorge)
	   (input   "Two suggestions.")
	   (effect  "None.")
	   (value   "T if SUGG1 subsumes SUGG2, o/w NIL."))
  (let ((map1 (bb~entry-mapping sugg1))
	(map2 (bb~entry-mapping sugg2)))
    (every #'(lambda (x)
	       (let ((pair (assoc (car x) map1 :test #'string-equal)))
		 (and pair
		      (keim~equal (cdr pair) (cdr x)))))
	   map2)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordering criteria for automate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto~set-criteria (&optional standard-heuristics)
  (cond  (standard-heuristics
	  (csm~set-default-heuristics)
	  (agent~init-resources))
	 (t ;;; the nic-agents are the default case
	  (bb~set-command-suggestion-ordering '(:i-e-f-demo))
	  (bb~set-parameter-suggestion-ordering '(:status :length))
	  (bb~set-command-suggestion-equality '(:command))
	  (bb~set-parameter-suggestion-equality '(:mapping))
	  (values))))

(defmethod bb~command-suggestion-leq-p
  ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :i-e-f)))
  (let ((com1 (mapcar #'keim~name (com~categories (bb~entry-command sugg1))))
	(com2 (mapcar #'keim~name (com~categories (bb~entry-command sugg2)))))
    (flet ((in (cat list)
	       (find cat list :test #'string-equal)))
      (or (in :elimination com2)
	  (and (in :introduction com2) (not (in :elimination com1)))
	  (and (in :false com2) (not (or (in :elimination com1)
					 (in :introduction com1))))
	  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for commands of the automate mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto~prove ()
  (declare (edited  "01-NOV-1999")
	   (authors Sorge)
	   (input   "Nothing.")
	   (effect  "Might perform a subproof automatically.")
	   (value   "Undefined."))
  (unless auto*in-use
    ;; initialization
    (setf foci*in-use t)
    (setf auto*in-use t)
    (setf auto*suggestions nil)
    (setf auto*history nil)       ;;;; could be handled differently for anytime purposes.
    (setf rule*verbose nil)
    (setf tac*verbose nil)
    (when (and (boundp 'nic*or-e-info-hashtable)
	       (boundp 'nic*or-e-removed-info-hashtable))
      (clrhash nic*or-e-info-hashtable)
      (clrhash nic*or-e-removed-info-hashtable))
    (setf auto*process
	  (proc~create :name "Automate"
		       :function #'auto=run-function))
    (sugg~reset)
    ))

(defun auto~stop ()
  (csm~quit)
  (setf auto*in-use nil)
  (setf rule*verbose t)
  (setf tac*verbose t))

(defun auto~suspend ()
  (csm~suspend)
  (proc~add-arrest-reason auto*process :suspended))

(defun auto~resume ()
  (csm~resume)
  (when (proc~arrest-reasons auto*process)
    (proc~revoke-arrest-reason auto*process :suspended)))

(defun auto~trace ()
  (setf auto*verbose t))

(defun auto~untrace ()
  (setf auto*verbose nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods that needed to be refined....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pds~delete-node! :around (node &optional verbose pds)
  (declare (ignore node verbose pds))
  (let ((undone (call-next-method)))
    (setf auto*deleted (append undone auto*deleted))
    undone))

(defmethod pds~open-node! :around (node &optional pds)
  (declare (ignore node verbose pds))
  (let ((undone (call-next-method)))
    (setf auto*deleted (append undone auto*deleted))
    undone))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequential call to several Automate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto~prove&wait ()
  (auto~prove)
  (proc~wait "Waiting for automate process to finish" #'(lambda () (proc~is-active auto*process))))

