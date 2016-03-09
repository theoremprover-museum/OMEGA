;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
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

;; ----------------------------------------------------------> Starting MULTI

(in-package :omega)

(com~defcommand multi
		(argnames strategies strategic-crules interactive bound)
		(argtypes symbol-list symbol-list boolean integer)
		(arghelps "The usable strategy-ks."
			  "The usable strategic control-rules."
			  "Interactive"
			  "bound of applied strategies")
		(frag-cats planning)
		(function sod=system-work)
		(defaults (sod*current-strategies
			   sod*current-strategic-control-rules
			   nil
			   40)
		  )
		(log-p T)
		(help ""))

(defun sod=system-work (strategy-symbols strategic-crules-symbols interactive bound &key (start-strategy nil))
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "A list of usable strategy-symbols and a list of usable strategic crules symbols.")
	   (effect  "...")
	   (value   "..."))
  
  ;; initialize
  (sod~initialize! omega*current-proof-plan strategy-symbols strategic-crules-symbols interactive bound)

  (setf sod*VIL nil)
  (setf pplan*VIL-on nil)

  ;; restart maple
  (when (socket~find-socket :Service)
    (maple~leave)
    (maple~enter))
  
  ;; call meta-planer
  (sod~system-go :start-strategy start-strategy)
  
  )

(com~defcommand restart-multi
		(argnames interactive bound)
		(argtypes boolean integer)
		(arghelps "Interactive" "bound of applied strategies")
		(frag-cats planning)
		(function sod=system-restart)
		(defaults (nil 40)
		  )
		(log-p T)
		(help ""))

(defun sod=system-restart (interactive bound)
  
  ;; initialize
  (setf inac*interactive interactive)
  (setf inac*strategy-ks-execution-interactive interactive)
  (setf sod*startegy-application-bound bound)
  
  ;; call meta-planer
  (sod~system-go)
  
  )

;; ----------------------------------------------------------------> stepper

(com~deffragment interactive
		 (uses-comms-of)		
		 (help "interactive commands in OMEGA"))

(com~defcommand step
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats interactive)
		(function sod=step)
		(defaults )
		(log-p T)
		(help ""))


(defun sod=step ()
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))

(com~defcommand stop-interactive-in-ks
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats interactive)
		(function sod=stop-interactive-in-ks)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=stop-interactive-in-ks ()
  (setf inac*strategy-ks-execution-interactive nil)
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))

(com~defcommand stop-interactive-completly
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats interactive)
		(function sod=stop-interactive-complete)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=stop-interactive-complete ()
  (setf inac*strategy-ks-execution-interactive nil)
  (setf inac*interactive nil)
  (sys~signal (sys~make-condition 'comint+leave-comint :comint-name (keim~name comint*current-comint))))


;; ----------------------------------------------------------------> show ...

(com~defcommand show-all-strategies
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=show-all-strategies)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=show-all-strategies ()
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Shows all strategies, nil rturned."))

  (let* ((all-strat-symbols (sod=all-strategies)))

    (omega~message "~%~A" all-strat-symbols)

    nil))

(defun sod=all-strategies ()
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "A list with the names (symbols) of all strategies in strat*strategies-hash-table."))
  (strat~all-strategy-ks-names-in-hash-table))

(com~defcommand show-all-strategic-control-rules
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=show-all-strategic-control-rules)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=show-all-strategic-control-rules ()
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Shows all strategic control rules, nil rturned."))

  (let* ((all-strat-crules-symbols (sod=all-strategic-control-rules)))
    
    (omega~message "~%~A" all-strat-crules-symbols)

    nil))

(defun sod=all-strategic-control-rules ()
  (declare (edited  "23-MAR-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "The names (symbols) of all crules in cri*control-rules-hashtable with kind strategic and kind suggestion"))
  (let* ((names nil))

    (maphash #'(lambda (key value)
		 (when (or (equal (cri~kind value) 'strategic)
			   (equal (cri~kind value) 'suggestion))
		   (setq names (append names (list key)))))
	     cri*control-rules-hashtable)
    names))

(com~defcommand show-current-strategies
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=show-current-strategies)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=show-current-strategies ()
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Shows the current strategies, nil rturned."))

  (omega~message "~%~A" sod*current-strategies)
  
  nil)


(com~defcommand show-current-strategic-control-rules
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=show-current-strategic-control-rules)
		(defaults )
		(log-p T)
		(help ""))

(defun sod=show-current-strategic-control-rules ()
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Shows the current strategic control rules, nil rturned."))
  
  (omega~message "~%~A" sod*current-strategic-control-rules)
  
  nil)

;; ----------------------------------------------------------------> Set ...


(com~defcommand set-current-strategies
		(argnames strategies)
		(argtypes symbol-list)
		(arghelps "The usable strategy-ks.")
		(frag-cats planning)
		(function sod=set-current-strategies!)
		(defaults ((sod=all-strategies)))
		(log-p T)
		(help ""))

(defun sod=set-current-strategies! (symbol-list)
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "A list of symbols.")
	   (effect  "The sumbols are checked wether they correspond to strategies in the"
		    "strat*strategies-hash-table. Then the variable sod*current-strategies"
		    "is set to the remainings.")
	   (value   "Undefined."))

  (let* ((remainings (remove-if-not #'strat~find-strategy-ks symbol-list)))

    (setf sod*current-strategies remainings)))

(com~defcommand set-current-strategic-control-rules
		(argnames strategic-control-rules)
		(argtypes symbol-list)
		(arghelps "The usable strategic-control-rules.")
		(frag-cats planning)
		(function sod=set-current-strategic-control-rules!)
		(defaults ((sod=all-strategic-control-rules)))
		(log-p T)
		(help ""))

(defun sod=set-current-strategic-control-rules! (symbol-list)
  (declare (edited  "19-JUL-1999")
	   (authors Ameier)
	   (input   "A list of symbols.")
	   (effect  "The sumbols are checked wether they correspond to strategies in the"
		    " cri*control-rules-hashtable (kind strategic). Then the variable"
		    "sod*current-strategic-control-rules is set to the remainings.")
	   (value   "Undefined."))

  (let* ((strategic-control-rules (sod=all-strategic-control-rules))
	 (remainings (remove-if-not #'(lambda (sym)
					(find sym strategic-control-rules :test #'string-equal))
				    symbol-list)))
    
    (setf sod*current-strategic-control-rules remainings)))
  
;; ----------------------------------------------------------------> Setting Verbooses

(com~defcommand set-sod-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "Sod-verbose on/out?")
		(frag-cats planning)
		(function sod=set-sod-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-sod-verbose! (flag)
  (setq sod*verbose flag))

(com~defcommand set-pplanner-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "Pplanner-verbose on/out?")
		(frag-cats planning)
		(function sod=set-pplanner-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-pplanner-verbose! (flag)
  (setq pplan*verbose flag))

(com~defcommand set-instmeta-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "InstMeta-verbose on/out?")
		(frag-cats planning)
		(function sod=set-instmeta-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-instmeta-verbose! (flag)
  (setq instmeta*verbose flag))

(com~defcommand set-backtrack-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "Backtrack-verbose on/out?")
		(frag-cats planning)
		(function sod=set-backtrack-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-backtrack-verbose! (flag)
  (setq back*verbose flag))

(com~defcommand set-exp-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "Expand-verbose on/out?")
		(frag-cats planning)
		(function sod=set-expand-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-expand-verbose! (flag)
  (setq expexp*verbose flag))

(com~defcommand set-all-verbose
		(argnames flag)
		(argtypes boolean)
		(arghelps "All verbose on/out?")
		(frag-cats planning)
		(function sod=set-all-verbose!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-all-verbose! (flag)
  (setq sod*verbose flag)
  (setq pplan*verbose flag)
  (setq instmeta*verbose flag)
  (setq back*verbose flag)
  (setq expexp*verbose flag))


#| -------------------------------------------------------------- Show Instantiations ------------------------------------------------- |#

(com~defcommand show-instantiations
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=show-instantiations)
		(defaults )
		(log-p T)
		(help "Shows the binding content of the current constraint-pool."))

(defun sod=show-instantiations ()
  (let* ((pds omega*current-proof-plan)
	 (constraint-pool (pds~constraint-pool pds))
	 (bindings (if constraint-pool
		       (pds~cstrpool-bindings constraint-pool)
		     (subst~create () ()))))
    
    (omega~message "~% ~A" bindings)))

#| ------------------------------------------------------------- APPLY INSTANTIATIONS ------------------------------------------------ |#

(com~defcommand apply-instantiations
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=apply-instantiations)
		(defaults )
		(log-p T)
		(help "Applies the binding content of the current constraint-pool."))

(defun sod=apply-instantiations ()
  (let* ((pds omega*current-proof-plan)
	 (constraint-pool (pds~constraint-pool pds))
	 (bindings (sod=replace-floats
		    (subst~idem (if constraint-pool
				    (pds~cstrpool-bindings constraint-pool)
				  (subst~create () ()))))))

    
    ;;(when (null (subst~empty-p bindings))
    ;;  (let* ((proof-steps (prob~proof-steps omega*current-proof-plan)))
    ;;	(mapcar #'(lambda (line)
    ;; 		    (let* ((formula (node~formula line)))
    ;;		      (setf (node~formula line)
    ;;			    (subst~apply bindings formula))))
    ;;		proof-steps)))))
    
    (setf plan*already-done nil)
    (plan=instantiate-pds omega*current-proof-plan bindings)))

(defun sod=replace-floats (subst)
  (declare (edited  "31-MAY-2001")
	   (authors Ameier)
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "A substitution, where in all terms of the codomain occurrences of float numbers are"
		    "replaced by corresponding occurrences of rational numbers (as far as possible)."))
  (let* ((codo (subst~codomain subst))
	 (new-codo (mapcar #'(lambda (term)
			       (sod=replace-0.5 term))
			   codo)))
    (subst~create (subst~domain subst) new-codo)))

(defun sod=replace-0.5 (term)
  (let* ((positions-of-0.5 (data~positions term #'(lambda (sub)
						    (and (term~number-p sub)
							 (= (keim~name sub) 0.5)))))
	 (div (env~lookup-object 'div (th~env 'integer)))
	 (one (term~constant-create '1 (env~lookup-object 'num (th~env 'natural)))) 
	 (two (term~constant-create '2 (env~lookup-object 'num (th~env 'natural))))) 
    (do* ((rest-positions positions-of-0.5 (rest rest-positions))
	  (curr-term term))
	((null rest-positions)
	 curr-term)
      (setf curr-term (data~replace-at-position curr-term
						(first rest-positions)
						(term~appl-create div (list one two)))))))

			       
  
#| ------------------------------------------------------- Creating PPlanner Justifications ------------------------------------------ |#

(com~defcommand make-strategic-justifications
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function sod=make-strategic-justifications)
		(defaults )
		(log-p T)
		(help "Creates and inserts pplanner-justifications!."))


(defun sod=make-strategic-justifications ()
  (let* ((start-step (black~get-blackboard-object-content 'first-strategy-ks-step sod*control-blackboard)))
    (do* ((current-step start-step (scon~strategy-step-successor current-step)))
	((null current-step)
	 nil)
      (let* ((current-just (scon~strategy-step-just current-step))
	     (just-method (just~method current-just)))
	(when (string-equal (keim~name just-method) 'end-strategy-ks-application)
	  (let* ((roc (first (pdsj~parameters current-just))))
	    (sod=interpret-roc! roc)
	    ))))))

		 
(defgeneric sod=interpret-roc! (roc)
  (:method ((roc roc+instmeta-state-description))
	   ;;(omega~message "~%No justification for InstMeta-Strategy Application.")
	   )
  (:method ((roc roc+backtrack-state-description))
	   ;;(omega~message "~%No justification for BackTrack-Strategy Application.")
	   )
  (:method ((roc roc+expansion-state-description))
	   ;;(omega~message "~%No justification for Exp-Strategy Application.")
	   )
  (:method ((roc roc+pplanner-state-description))
	   
	   ;; New pplanner-strategy-ks-application is created and
	   ;; set as justification in the node of the start-task
	   
	   (let* ((start-task (roc~start-task roc))
		  (strategy-ks (roc~strategy-ks roc))
		  (goal-node (agenda~task-node start-task))
		  (current-just-of-goal-node (node~justification goal-node))
		  (new-lines (roc~pplanner-new-lines roc))
		  (outline-nodes (roc~pplanner-outline-lines roc))
		  (start-step (roc~start-step roc))
		  (new-justification (strat~pplanner-strategy-ks-application-create-just (cons goal-node outline-nodes)
											 (list (keim~name strategy-ks)
											       ;; start-step))))
											       ))))
	     
	     ;; Now there would be some other possibilities:
	     
	     ;; This new step is now put in the pds
	     ;; (new-reason (pds~change-last-plan-step! goal-node))
	     ;; This reason is entried in all new-lines etc. ....
	     
	     ;; Since this function is mainly for presentation purposes, this should suffice for now!
	     
	     ;;(omega~message "~%New PPlanner Justification for node ~A: ~A" goal-node new-justification)
	     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions and commands for Interactive Methods, only for LOUI. MP 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand multi-VIL
		(argnames strategy strategies strategic-crules)
		(argtypes symbol symbol-list symbol-list)
		(arghelps "Start Strategy"
			  "The usable strategy-ks."
			  "The usable strategic control-rules.")
		(frag-cats planning)
		(function sod=system-work-VIL)
		(defaults ('tryanderror
			   sod*current-strategies
			   sod*current-strategic-control-rules))
		(log-p T)
		(help "Starts Multi in VIL-MODE."))


(com~defcommand imulti
		(argnames strategy strategies auto-strats strategic-crules)
		(argtypes symbol symbol-list symbol-list symbol-list)
		(arghelps "Start Strategy"
			  "The usable strategy-ks."
			  "The usable strategic control-rules."
			  "Strategies that should run without user interaction."
			  )
		(frag-cats planning)
		(function sod=system-work-VIL)
		(defaults ('tryanderror
			   sod*current-strategies
			   sod*current-strategic-control-rules
			   nil))
		(log-p T)
		(help "Starts Multi in VIL-MODE with auto-strats."))

(defun sod=system-work-VIL (strategy-symbol strategy-symbols strategic-crules-symbols &optional auto-strats)
  (let ((opr*calling-listener opr*loui-listener));;MP: it doesn't make sense to call this without Loui so we redirect every output
    (pplan=interactive-set-autostrat auto-strats) ;;rember the interactive strats
    (sod~initialize-VIL! omega*current-proof-plan strategy-symbols strategic-crules-symbols)
    (catch 'break 
      (sod~system-go :start-strategy strategy-symbol))        
    (csm~resume)(oc=deac-agents));;resume and kill, just to be sure 
  (pairlis (list "ID" "openLines" "allLines")
	   (list (or rpc*callid 'unconnected);;MP: return-value for ActiveMath,
		 (length (pds~open-nodes omega*current-proof-plan));;ignored by anything else anyway
		 (length (prob~proof-steps omega*current-proof-plan))))
  )

  
(com~defcommand im.agents
  (function sod=dummy)
  (frag-cats  planning)
  (help "Stop MULTI, start interactive planning."))

(com~defcommand im.multi
  (function  sod=dummy)
  (frag-cats  planning)
  (help "Stop interactive planning, start MULTI."))

;(com~defcommand im.start ---> now multi-vil
;  (function sod=im-start)
;  (frag-cats  planning)
;  (help "Start the interactive planning dialog."))


(com~defcommand im.stop
  (function sod=dummy)
  (frag-cats  planning)
  (help "Stop the intactive planning dialog."))

(com~defcommand im.set-goal
  (argnames line)
  (argtypes ndline)
  (arghelps "The new goal")
  (function sod=im-set-goal)
  (frag-cats  planning)
  (help "Set the active-focus to current to a line."))

(defun sod=im-set-goal (&optional goal)
  (if (mixin~activep)
      (progn
	(when goal (foci~set-active-focus goal))
	(socket~write (format nil "updateInterActive(goals ~A)"
			(parse~list-of-strings 
			 (mapcar #'(lambda (foc)
				     (keim~name (foci~focus-line foc)))
				 (cons (foci~active-pc)
				       (foci~passive-pcs)))))
			:inout)

	(sugg~reset))
    (omega~warn "This function is only available with LOUI.")))

(com~defcommand im.set-metavar
  (argnames mv)
  (argtypes termsym)
  (arghelps "A metavar of the current proofplan to instantiate")
  (function sod=dummy)
  (frag-cats  planning)
  (help "Instantiates a metavariable by term."))


(defun sod=dummy (&rest bla) )


(com~defcommand im.backtrack
  (argnames mv)
  (argtypes anything)
  (arghelps "A metavar of the current proofplan to instantiate")
  (function sod=dummy)
  (frag-cats  planning)
  (help "Backtrack to ...")) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USER COMMENTS!

(com~defcommand set-user-comments
		(argnames flag)
		(argtypes boolean)
		(arghelps "User-Comments on/out?")
		(frag-cats planning)
		(function sod=set-user-comments!)
		(defaults (t))
		(log-p T)
		(help ""))

(defun sod=set-user-comments! (flag)
  (setq pplan*user-comments flag))

