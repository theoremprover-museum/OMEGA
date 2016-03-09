;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-

(in-package "OMEGA")

(com~defcommand show-reasons    
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-reasons)
  (help "Display the reasons of a line."))

(defun oc=show-reasons (ndline)
  (mapc #'(lambda (line)
	    (omega~message "~A" line))
	(pdsn~reasons ndline)))

(com~defcommand show-failed-steps    
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-failed-steps)
  (help "Display the failed steps of a line."))

(defun oc=show-failed-steps (ndline)
  (mapc #'(lambda (line)
	    (omega~message "~A" line))
	(pdsn~failed-steps ndline)))

(com~defcommand show-g-inapp    
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-g-inapp)
  (help "Display the g-inapplicable methods of a line."))

(defun oc=show-g-inapp (ndline)
  (mapc #'(lambda (line)
	    (omega~message "~A" line))
	(pdsn~g-inapplicable-methods ndline)))

(com~defcommand show-s-inapp    
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-s-inapp)
  (help "Display the s-inapplicable methods of a line."))

(defun oc=show-s-inapp (ndline)
  (mapc #'(lambda (line)
	    (omega~message "~A" line))
	(pdsn~s-inapplicable-methods ndline)))

(com~defcommand show-originator-step
  (argnames plan)
  (argtypes ndline)
  (arghelps "A line")
  (frag-cats direct-display)
  (function oc=show-originator-step)
  (help "Display the originator step of a line."))

(defun oc=show-originator-step (ndline)
  (omega~message "~A" (pdsh~originator-step ndline)))

(com~defcommand show-history
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats direct-display)
  (function oc=show-history)
  (help "Display the proof history."))

(defun oc=show-history ()
  (mapc #'(lambda (x)
	    (omega~message "~A" x))
	(pds~all-reasons omega*current-proof-plan)))

(com~defcommand show-constr
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats direct-display)
  (function oc=show-constraint-store)
  (help "Display the constraint store of the current proof plan."))

(defun oc=show-constraint-store ()
  (omega~message "~A" (pds~constraint-store omega*current-proof-plan)))

(defun oc=show-supports (planline)
  (mapc #'(lambda (line)
	    (omega~message "~A" (keim~name line)))
	(pdsn~supports planline)))

(com~defcommand show-extra-supports    
  (argnames plan)
  (argtypes ndplanline)
  (arghelps "Plan line")
  (frag-cats direct-display)
  (function oc=show-extra-supports)
  (defaults ((oc~default-current-planline)))
  (help "Display the extra support lines of an open line."))

(defun oc=show-extra-supports (planline)
  (mapc #'(lambda (line)
	    (omega~message "~A" (keim~name line)))
	(pdsn~extra-supports planline)))

(com~defcommand trace-plan
  (function oc=trace-plan)
  (argnames trace-plan-p)
  (argtypes boolean)
  (arghelps "T to trace the planning process, else nil")
  (frag-cats direct-display)
  (defaults ((oc=trace-plan-default)))
  (help "If argument is T, then trace the planning process."))

(defun oc=trace-plan-default ()
  (not plan*trace))

(defun oc=trace-plan (bool)
  (setf plan*trace bool))

(com~defcommand trace-back
  (function oc=trace-back)
  (argnames trace-back-p)
  (argtypes boolean)
  (arghelps "T to trace the backtracking process, else nil")
  (frag-cats direct-display)
  (defaults ((oc=trace-back-default)))
  (help "If argument is T, then trace the backtracking process."))

(defun oc=trace-back-default ()
  (not back*trace))

(defun oc=trace-back (bool)
  (setf back*trace bool))

(com~defcommand watch-for
  (argnames meth-name)
  (argtypes anything)
  (arghelps "The method to watch for.")
  (frag-cats planning)
  (function oc=watch-for)
  (log-p T)
  (help "Selects a method to trace the planning process for."))

(defun oc=watch-for (refh)
  (let ((ref (meth~find-method refh)))
    (setf plan*watch-ref ref)))

(com~defcommand write-result
  (argnames outfile)
  (argtypes pathname)
  (arghelps "Name of a file to write to")
  (frag-cats omega-basic file-io)
  (function oc=write-result)
  (log-p T)
  (help "Write the current proof in the form of a POST expression to the file established."))

(defun oc=write-result (path)
  (let ((pathname (ot~post-pathname path))
	(tmpname (merge-pathnames path "*.tmp")))
    (if (pds~proof-plan-p omega*current-proof-plan)
	(progn
	  (sys~handler-case
	   (with-open-file (out tmpname :direction :output 
				:if-exists :supersede
				:if-does-not-exist :create)
			   (if (agenda~empty-p (pds~agenda omega*current-proof-plan))
			       (oc=write-result-steps out omega*current-proof-plan)
			     (format out "(no-proof-found)"))
			   (omega~message "Wrote file ~A." (truename out)))
	   (file-error (c) (omega~error c)))
	  (excl::run-shell-command (format nil "mv ~A ~A" tmpname pathname) :wait nil :output :stream))
      (arg~signal-wrong-type 'proof-plan omega*current-proof-plan))))

(defgeneric oc=write-step (stream rsn pds)
  (:method (stream (rsn pdsh+ia-reason) pds)
	   (let* ((focus-goal (first (pdsh~ia-focus-goals rsn)))
		  (just (pdsh~ia-most-abstract-justification rsn))
		  (method (pdsh~ia-ref rsn))
		  (mmapp (pdsj~subst just))
		  (parameters (meth~compute-parameters method mmapp))
		  (input-length (list-length (infer~parameter-types (meth~inference method))))
		  (input-parameters (subseq parameters 0 input-length))
		  (existent-prems (meth~compute-existent-premises method mmapp))
		  (closed-prems (meth~compute-closed-premises method mmapp))
		  (subgoals (pdsh~ia-subgoals rsn))
		  (add-concs (pdsh~ia-add-conclusions rsn))
		  (new-hyps (remove-if-not #'pdsn~hypothesis-node-p (pdsh~reason-new-nodes rsn))))
	     (format stream "(inference-application ~A ~A ~A ~A " (keim~name method) (keim~name focus-goal)
		     (mapcar 'keim~name existent-prems) (mapcar 'keim~name closed-prems))
	     (format stream "(")
	     (dolist (param input-parameters)
	       (pds~post-print param stream))
	     (format stream ")")
	     (let ((*standard-output* stream))
	       (pp~pprint mmapp 'pds-post))
	     (format stream "~%~A~%~A~%~A" (mapcar 'keim~name subgoals) (mapcar 'keim~name add-concs)
		     (mapcar 'keim~name new-hyps))
	     (format stream ")~%"))))

(defun oc=write-result-steps (stream pds)
  (let* ((root (prob~proof-root pds))
	 (problem (prob~proof-problem pds))
	 (assumptions (prob~assumptions problem)))
    (format stream "((conclusion ~A)~%(assumptions ~A)~%" (keim~name root) (mapcar 'keim~name assumptions))
    (format stream "(steps~%")
    (dolist (rsn (pds~all-reasons pds))
      (oc=write-step stream rsn pds))
    (format stream "))")))

(defun oc=write-result-nodes (stream pds)
  (let* ((root (prob~proof-root pds))
	 (problem (prob~proof-problem pds))
	 (env (prob~environment problem))
	 (pds-env (pds~environment pds))
	 (type-vars  (env~class-keys env 'type+variable nil))
	 (type-constants (env~class-keys env 'type+constant nil))
	 (constants (env~class-keys env 'term+constant nil))
	 (variables (env~class-keys env 'term+variable nil))
	 (pds-type-vars  (env~class-keys pds-env 'type+variable nil))
	 (pds-type-constants (env~class-keys pds-env 'type+constant nil))
	 (pds-constants (env~class-keys pds-env 'term+constant nil))
	 (pds-variables (env~class-keys pds-env 'term+variable nil)))
    (format stream "(pds ~A ~%(in ~A) ~%" (keim~name problem) (keim~name (prob~theory problem)))
    (format stream "(declarations ")
    (mapc #'(lambda (x) (env~post-print x (env~lookup-object x pds-env) stream))
	  (append type-vars pds-type-vars type-constants pds-type-constants
		  constants pds-constants variables pds-variables))
    (format stream ")~%")
    (format stream "(nodes ")
    (dolist (node (prob~proof-steps pds))
      (post~print node stream))
    (format stream ")~%")
    (format stream ")")))
