;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; plan.lisp; This file is part of the OMEGA system
;;
;; major updates: 24.2.1999,
;; 
;;
;; Authors: Lassaad Cheikhrouhou, Juergen Zimmer, Carsten Ullrich
;; email: {lassaad,jzimmer, cullrich}@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Planungs-Algorithmus
(in-package "OMEGA")

(mod~defmod PLAN 
            :uses (agenda arg com comint cri env exp foci help ina infer inter just keim logic mapp meth cosint
			  node oc omega ot parse pds pdsc pdsj pdsn pos post prob socket subst sys term type)
            :documentation "The data structures and algorithm for agenda-based backward/forward planning. "
            :exports (plan+meth-matching
                      plan+meth-matching
                      plan+modus 
                      
                      plan~apply-mmatching
		      plan~apply-mmatching-with-node
                      plan~back-pseudo-task
                      plan~back-task
                      plan~block-pseudo-task
                      plan~block-task
                      plan~change-modus
                      plan~delete-node
                      plan~expand-supermethod
                      plan~find-modus
		      plan~find-task-for-goal-in-agenda
                      plan~initialize-agenda
		      plan~instantiate-metavars
                      plan~iterate-planner
                      plan~loui-interface-p
                      plan~loui-loaded-p
                      plan~match-method
                      plan~matched-method
                      plan~message
                      plan~meth-matching-p
                      plan~mmatch-clsed-prems
                      plan~mmatch-cstr-state
                      plan~mmatch-exist-prems
                      plan~mmatch-goal
                      plan~mmatch-mmapp
                      plan~mmatch-parameters
                      plan~modus-bound-names
                      plan~modus-bounds
		      plan~new-pds
                      plan~next-pseudo-task
                      plan~next-task
                      plan~open-node
                      plan~plan
                      plan~reactive-change-modus
                      plan~reset-planner
		      plan~show-data
                      plan~show-modus
                      plan~step-plan
		      plan~step-plan-with
                      plan~trace
		      plan~try-planning-step
                      plan~unblock-pseudo-task
                      plan~unblock-task
                      plan~use-method
                      
                      plan*already-done
                      plan*current-modus
		      plan*current-task
                      plan*matching-attempts
		      plan*messages
                      plan*meta-vars
                      plan*modi
                      plan*trace))

;;; ATTENTION:
;;; ==========
;;; The following functions are internal in other modules and are redefined here:
;;; (oc=delete oc=open oc=prove-pre oc=read-problem)

;;; Some global variables:
(defvar plan*meta-vars nil "The meta-variables created during the planning process.")
(defvar plan*current-task nil "The task currently planned (needed by plan~iterate-planner).")
(defvar plan*matching-attempts 0 "The task currently planned (needed by plan~iterate-planner).")
(defvar plan*number-of-applied-methods 0 "The task currently planned (needed by plan~iterate-planner).")

;; flags controling the output
(defvar plan*messages t "Flag for omega~messages.")
(defvar plan*trace nil "Flag for omega~trace outputs.")

;;; Some data structures for modi and matchings
(eval-when (compile eval load)
  (defclass plan+modus (help+help)
  ((bounds :initarg :bounds
	   :initform nil
	   :accessor plan~modus-bounds
	   :documentation "The modus bounds.")
   (bound-names :initarg :bound-names
		:initform nil
		:reader plan~modus-bound-names
		:documentation "The names of the modus bounds."))
  (:documentation "Data structure for planning modi."))
  
  
;;; The planning modi:
  (defconstant plan*modi
    (list (make-instance 'plan+modus
		       :name 'automatic
		       :help "Automatic planning modus"
		       :bounds '(100)
		       :bound-names '("maximal steps"))
	(make-instance 'plan+modus
		       :name 'interactive
		       :help "Interactive planning modus"
		       :bounds '(nil 1 30)
		       :bound-names '("maximal tasks" "maximal methods" "maximal steps"))
	(make-instance 'plan+modus
		       :name 'mixed-initiative
		       :help "Mixed-initiative planning modus"
		       :bounds '(30)
		       :bound-names '("maximal steps")))
  "The different planning modi.")


(defclass plan+meth-matching ()
  ((method :initarg :method
	   :initform nil
	   :accessor plan~matched-method
	   :documentation "The matched method.")
   (parameters :initarg :parameters
	       :initform nil
	       :accessor plan~mmatch-parameters
	       :documentation "The matched method parameters.")
   (mmapp :initarg :mmapp
	  :accessor plan~mmatch-mmapp
	  :documentation "The binding of the method meta-variables to pds-objects.")
   (goal :initarg :goal
	 :initform nil
	 :accessor plan~mmatch-goal
	 :documentation "The goal to be closed: just for OUTLINE-PAT.")
   (exist-prems :initarg :exist-prems
		:initform nil
		:accessor plan~mmatch-exist-prems
		:documentation "The unsigned premises: just for OUTLINE-PAT.")
   (clsed-prems :initarg :clsed-prems
		:initform nil
		:accessor plan~mmatch-clsed-prems
		:documentation "The premises to be deleted: just for OUTLINE-PAT.")
;   (cstr-state :initarg :cstr-state
;               :initform nil
;               :accessor plan~mmatch-cstr-state
;               :documentation "The constraint pool resulted after evaluating the method application conditions.")
   )
  (:documentation "Some information to the matching of a method."))
)

(defun plan~mmatch-cstr-state (mmatch)
  (let ((cstr-state (meth~mapp-constraint (plan~mmatch-mmapp mmatch))))
    (when (pds~constraint-pool-p cstr-state) cstr-state)
    ))

(defun plan~meth-matching-p (object)
  (typep object 'plan+meth-matching))

(defun plan=mmatching-create (method params mmapp &optional goal exist-prems clsed-prems)
  (make-instance 'plan+meth-matching
		 :method method
		 :parameters params
		 :mmapp mmapp
		 :goal goal
		 :exist-prems exist-prems
		 :clsed-prems clsed-prems))


(defun plan~find-modus (symbol)
  (find-if #'(lambda (modus) (string-equal symbol (keim~name modus)))
	   plan*modi))

(defvar plan*current-modus (plan~find-modus 'automatic)
					;(plan~find-modus 'mixed-initiative)
  "The planning current modus.")

(defun plan~change-modus (symbol bound-list)
  (if (plan~loui-loaded-p)
      (let ((modus (plan~find-modus symbol)))
	(if modus
	    (progn (setf (plan~modus-bounds modus) bound-list)
		   (setq plan*current-modus modus)
		   (plan~show-modus modus))
	  (omega~error ";;; Unknown planning modus: ~A" symbol)))
    (if (string-equal symbol "mixed-initiative")
	(omega~error ";;; There is no interface to run the planner in a mixed-initiative modus!")
      (let ((modus (plan~find-modus symbol)))
	(if modus
	    (progn (setf (plan~modus-bounds modus) bound-list)
		   (setq plan*current-modus modus)
		   (plan~show-modus modus))
	  (omega~error ";;; Unknown planning modus: ~A" symbol))))))

	

(defun plan~show-modus (&optional (modus plan*current-modus))
  (let* ((bounds (plan~modus-bounds modus))
	 (bound-names (plan~modus-bound-names modus))
	 (bound-strings (mapcar #'(lambda (value name)
				    (if value
					(concatenate 'string (princ-to-string value) " " name  "~%")
				      ""))
				bounds bound-names)))
    (if (every #'null bounds)
	(plan~message (concatenate 'string (format nil "The current planning modus: ~A~%" (keim~name modus))
				    (help~help-string modus)))
      (plan~message (concatenate 'string (format nil "The current planning modus: ~A with:~%" (keim~name modus))
				  (apply #'concatenate (cons 'string bound-strings)) (help~help-string modus))))
    ))


(defun plan~show-data ()
  (omega~message "Used ~S matching attempts." plan*matching-attempts)
  (omega~message "Applied ~S methods." plan*number-of-applied-methods)
  )

(defun plan=set-planning-methods (names)
  (let ((methods
	 (apply #'append
		(mapcar #'(lambda (name)
			    (let ((meth (meth~find-method name)))
			      (when meth
				(list meth))))
			names))))
    (setf meth*planning-methods methods)
    ))

(defun plan~reset-planner ()
  (setf plan*meta-vars nil)      
  (setf plan*matching-attempts 0)
  (setf plan*number-of-applied-methods 0)
  (cosint~reset-all-cs)       ;; reset the constraint solvers
  )

(defun plan~loui-loaded-p ()
  (find :gui *features*))

(defun plan~loui-interface-p ()
					;(and (plan~loui-loaded-p) (mixin~read-from-socketp)))
  nil)


;;;*******************************************************
;;; The planning interface functions
;;;*******************************************************
;;; Changed so that backtracking is considered as one step
(defun plan~step-plan(&optional (pds omega*current-proof-plan) (agenda (plan=prepare-pds! pds)))
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "An agenda, and a pds")
	   (effect  "Carry out a planning step including backtracking, when AGENDA is"
		    "not empty, otherwise signal that AGENDA is emty.")
	   (value   "Unspecified."))
  (if (agenda~empty-p agenda)
      (progn
	(when (plan~loui-interface-p) (socket~write "ready"))
	(plan~message "The current agenda is empty!"))
    (multiple-value-bind (root-taskp new-agenda success)
	(plan=do-step pds agenda)
      (when (plan~loui-interface-p)              ;; update loui-agenda stuff
	(socket~write "update-agenda")
	(parse~agenda (agenda~for-loui new-agenda))
	(socket~write "update-prooftree")
	(parse~proof pds)
	;;; LOUI, falls schnell
	)
      (if success
	  (if (agenda~empty-p new-agenda)
	      (progn
		(setf (pds~agenda pds) new-agenda)
		;;; LOUI aus seiner Schleife befreien!
		(when (plan~loui-interface-p) (socket~write "ready"))
		(plan~message "The current pds ~A is planned!~%" pds)
		(plan~show-data)
		(plan~instantiate-metavars pds)
		)
	    (progn
	      (when (plan~loui-interface-p) (socket~write "ready"))
	      (setf (pds~agenda pds) new-agenda)))
	;;; The done step in PLAN=DO-STEP was backtracking: 
	(if root-taskp
	    ;;; The PDS root node had to be deleted in the backtracking step: When there is no task
	    ;; on the NEW-AGENDA different from ROOT-TASKP then signal that no plan can be found for
	    ;; the current PDS. Otherwise, you have to consider another task before the ROOT-TASKP:
	    (let ((other-task (agenda~get-first-task new-agenda
						     #'(lambda (tk) (not (eq tk root-taskp))))))
	      (if other-task
		  (let* ((new-agenda2 (agenda~replace-task root-taskp nil nil nil new-agenda))
			 (new-agenda3 (agenda~insert-tasks other-task nil (list root-taskp)
							   (list (agenda~ordering-create (list other-task)
											 (list root-taskp)))
					 		   new-agenda2)))
		    ;(plan~step-plan pds new-agenda3))
		    (setf (pds~agenda pds) new-agenda3))
		(progn
		  (setf (pds~agenda pds) new-agenda)
		  ;;; LOUI aus seiner Schleife befreien!
		  (when (plan~loui-interface-p) (socket~write "ready"))
		  (plan~message "No proof plan can be found for the current pds ~A!~%"
				 pds))))
	  ;;; Try to apply a method:
	  ;(plan~step-plan pds new-agenda))))
	  (setf (pds~agenda pds) new-agenda))))
    ))

;;;****************************************************************
;;; The planner main function
;;;****************************************************************
(defun plan~plan (&optional (pds omega*current-proof-plan) (modus plan*current-modus) (agenda (pds~agenda pds)))
  (declare (edited  "17-MAR-1998")
	   (authors Lassaad)
	   (input   "A PDS, a modus and an agenda.")
	   (effect  "Constructs a complete PDS or fails.")
	   (value   "Unspecified."))
  (plan=plan pds modus agenda)
  (when (plan~loui-interface-p) (socket~write "ready")))


(defun plan=plan (&optional (pds omega*current-proof-plan) (modus plan*current-modus) (agenda (pds~agenda pds)))
  (declare (edited  "18-SEP-1998")
	   (authors Jzimmer)
	   (input   "A PDS, a modus and an agenda.")
	   (effect  "Constructs a complete PDS or fails.")
	   (value   "Unspecified."))
  (if (agenda~empty-p agenda)
      (progn (plan~message "The current pds ~A is planned!~%" pds)
	     (plan~show-data)
	     (plan~instantiate-metavars pds))        ;; jzimmer

    ;;; Normalize PDS and AGENDA:
    (let ((new-agenda (plan=prepare-pds! pds)))
      (cond ((eq (keim~name modus) 'automatic)
	     (let ((bounds (plan~modus-bounds modus)))
	       (plan=automatically (first bounds) pds new-agenda)))
	    ((eq (keim~name modus) 'interactive)
	     (let ((bounds (plan~modus-bounds modus)))
	       (plan=interactively (first bounds) (second bounds) (third bounds) pds new-agenda)))
	    ((eq (keim~name modus) 'mixed-initiative)
	     (if (plan~loui-interface-p)
		 (let ((bounds (plan~modus-bounds modus)))
		   (plan=mixed-initiative (first bounds) pds new-agenda))
	       (plan~message ";;; You have to switch on LOUI interface!")))
	    (T
	     (omega~error ";;; Unkown planning modus: ~A" modus))))
    ))



;;******************************************************
;;   step planner for analogy                          *
;;******************************************************
(defun plan~step-plan-with (methodh goalh supportsh parameters &optional (pds omega*current-proof-plan))
  (declare (edited  "18-SEP-1998")
	   (authors Cullrich)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((method (if (meth~p methodh) methodh (meth~find-method methodh)))
	 (new-goal (when goalh (if (pdsn~p goalh) goalh
				 (pds~label2node goalh))))
		     ;; sonst????
		     ;(agenda~task-node (agenda~get-first-task (pds~agenda pds)))))
	 (new-supports (if supportsh
			   (if (pdsn~p (first supportsh))
			       supportsh
			     (mapcar #'pds~label2node supportsh))
			 (pdsn~hyps new-goal)))
	 (old-agenda (pds~agenda pds))
;	 (egal1 (omega~trace "1.AGENDA: ~S" old-agenda))
	 (tasks (reverse (agenda~first-tasks! old-agenda)))
;	 (egal2 (omega~trace "2.AGENDA: ~S, tasks: ~s" old-agenda tasks))
	 (non-schematic-task (first (member-if-not #'agenda~goal-schema-p tasks)))
	 (task (if new-goal (plan~find-task-for-goal-in-agenda new-goal old-agenda)
		 (if non-schematic-task non-schematic-task
		   (first tasks))))
	 (plan*current-task task)
;	 (egal3 (progn (omega~trace "1.AGENDA: ~S" old-agenda)
;		       (omega~trace "non sch-t ~S, task ~s " non-schematic-task task)))
	 (ac-goal (or new-goal (agenda~task-node task)))
	 (parameter-types (infer~parameter-types (meth~inference method)))
	 (new-parameters (and (or (= (length parameter-types) (length parameters))
				  (return-from plan~step-plan-with (omega~error "Too many or not enough parameters supplied!")))
			      (mapcar #'(lambda (x y) (arg~read-type x y)) parameter-types parameters)))
	 (new-agenda 
					;(progn (omega~message "task: ~A plan-current-task: ~A goal ~S" task plan*current-task
					;		ac-goal)
	  (plan=step-planner method pds task ac-goal new-supports
			     new-parameters)
	      )
	 ;)
	 )
    (if (agenda~empty-p new-agenda)
	(progn
	  (setf (pds~agenda pds) new-agenda)
		;;; LOUI aus seiner Schleife befreien!
	  (when (plan~loui-interface-p) (socket~write "ready"))
	  (plan~message "The current pds ~A is planned!~%" pds)
	  (plan~show-data)
	  (plan~instantiate-metavars pds)
	  new-agenda
	  )
      (progn
	(when (plan~loui-interface-p) (socket~write "ready"))
	;; new agenda can be nil if the method was not applicable, so check it:
	(setf (pds~agenda pds) (or new-agenda old-agenda))
	(when (foci~in-use) (foci~compute-pcs :pds pds)(sugg~reset))  ;;Hi VS: is this the right place?
	                                                              ;;foci~update-pcs would be better MP
	new-agenda
	))))


;;*******************************************************
;;*   supermethod stuff                                 *
;;*******************************************************
(defun plan~iterate-planner (goal support method-names control-rules)
  (declare (edited  "26-JUN-1997")
	   (authors Jzimmer)
	   (input   "The goal the supermethod matched with. An additional goal,"
		    "a list of the methods that should be applied iteratively and "
		    "a list of the local control rules.")
	   (effect  "Calls the planner with the specified methods until the "
		    "planning state is not changed anymore.")
	   (value   "Two values: 1) T iff the supermethod is applicable."
		    "      2) a list with the new-pds,"
		    "       the preconditions, the deleted assumptions, the new subgoals,"
		    "       the new assumptions"
		    "       and the goal, the supermethod is applied to."))
  
  "Note: A supermethod is applicable iff
     1) a GOAL is given and it has been closed by the application of one of the
        given methods, or
     2) no GOAL is given and at least one method has been applied."
  
  ;; remark: (list new-pds prec add-hyps add-prems add-concs act-goal T)
  (let* ((changed T)
	 (one-method-applied nil)
	 (old-pds omega*current-proof-plan)   ;; nur bei level 1 richtig!!!!!!!
	 (old-agenda (pds~agenda old-pds))
	 (first-task (unless (agenda~empty-p old-agenda)
		       (agenda~first-task old-agenda)))
	 (task (if first-task first-task
		 ;;		 (progn
		 ;;		 (omega~message "HIER!!!!! Es gibt keinen first-task! ~S ~S" goal plan*current-task)
		 plan*current-task))  ;(first (agenda~next-tasks old-agenda)))) stimmt nicht immer!

	 (act-goal (if goal goal
		     (agenda~task-node task)))
	 (act-supp (if support (list support)
		     (pds~node-supports act-goal old-pds)))
	 (new-pds (plan~new-pds act-goal
				act-supp
				old-pds
				))
	 (cri*used-control-rules-hashtable
	  (cri~set-used-control-rules control-rules))
	 )
;    (omega~message "first-task: ~S first-tasks ~S plan-current-task: ~S goal: ~S" first-task
;		   (agenda~first-tasks! old-agenda) plan*current-task act-goal)

    (when method-names
      (do nil
	  ((not changed)
	   ;; nothin happened; so return from iteration 
	   (let* ((root (prob~proof-root new-pds))
		  (steps (union (list root)
				(prob~proof-steps new-pds)))
		  (open-prems (pds~open-nodes new-pds))
		  (add-prems (remove root open-prems))
		  (add-hyps
		   (plan=set-difference (plan=union-of-hyps steps)
				   (pdsn~hyps root)))         ;;; NOCH FALSCH!!!!?????
		  (internal-prems
		   (plan=union-of-prems steps))
		  
		  (add-concs
		   (remove-duplicates
		    (plan=set-difference
		     (remove-if #'pdsn~open-node-p steps)
		     (cons root
			   (union internal-prems
				  (union add-hyps              ;;richtig???
					 (pdsn~just-unsponsors root)))))))
		  (prec (intersection internal-prems
				      act-supp))
		  )
	     #|
             (plan~message "~% act-supp: ~S" act-supp)
	     (plan~message "~% steps: ~S" steps)
	     (plan~message "~% add-prems: ~S" add-prems)
	     (plan~message "~% preconds: ~S" prec)
	     (plan~message "~% add-hyps: ~S" add-hyps)
	     (plan~message "~% add-concs: ~S" add-concs)
	     |#

	     (mapcar #'(lambda (node)
			 (let* ((just (node~justification node))
				)
			   (when just
			     (setf (pdsj~reasons just) nil))))
		     (append add-hyps add-prems add-concs))
	     
	     ;;return the appropriate values
	     (if goal
		 (if (member root open-prems)
		     (values NIL (list NIL NIL NIL NIL NIL T))
		   (values T (list new-pds prec add-hyps add-prems add-concs root T)))
	       (if (or add-concs one-method-applied)
		   (values T (list new-pds
				   prec
				   add-hyps
				   (remove act-goal add-prems)
				   add-concs
				   root
				   NIL))
		 (values NIL (list NIL NIL NIL NIL NIL NIL))))))
	
	;; try to apply all methods of the supermethod:
	(let* ((applied (mapcar #'(lambda (meth-name)
				    (plan~try-planning-step meth-name new-pds goal))
				method-names))
	       )
	  (setq changed (some #'(lambda (item)  ;; could at least one method be applied?
				  item)
			      applied))
	  (when changed (setq one-method-applied t)) 
	  )))))

(defun plan=union-of-hyps (nodes)
  (when nodes
    (union (pdsn~hyps (first nodes))
	   (plan=union-of-hyps (rest nodes)))))

(defun plan=union-of-prems (nodes)
  (when nodes
    (let* ((node (first nodes))
	   (just (node~justification node))
	   (infer (just~method just))
	   )
      (if (or (infer~dummy-p infer)
	      (pdsn~open-node-p node)
	      (pdsn~hypothesis-node-p node))
	  (plan=union-of-prems (rest nodes))
	(union (just~premises just)
	       (plan=union-of-prems (rest nodes)))))))

(defun plan~new-pds (goal supports old-pds)
  (let* ((old-just (node~justification goal))
	 (new-control (pdsc~create nil
				   supports
				   nil))
	 (new-just (pdsj~open-just-create new-control))
	 
	 (new-goal (pdsn~create (intern (format nil "LOCAL-~A" (keim~name goal)))
				(pdsn~hyps goal)
				(node~formula goal)
				new-just))
	 (new-agenda (agenda~create
		      (agenda~create-open-task new-goal)
		      nil
		      nil
		      (make-instance 'agenda+empty-agenda)))
	 
	 (new-proof
	  (make-instance 'pds+proof-plan
			 :name (keim~name new-goal)
			 :theory (prob~proof-theory old-pds)
			 :environment (pds~environment old-pds)
			 :root new-goal
			 :agenda new-agenda
			 :open-nodes (list new-goal)
			 :support-nodes supports
			 :problem (prob~proof-problem old-pds)
			 :label-node-hashtable (make-hash-table :test #'equal);(pds~label-node-hashtable old-pds)
			 :label-counter (keim::pds=label-counter old-pds)
			 ))
	 )
    (setf (gethash (symbol-name (keim~name new-goal))
		   keim::pds*proof-plan-hashtable)
	  new-proof)
    new-proof))



(defun plan~expand-supermethod (conc result)
  (let ((new-pds (first result))
	(add-hyps (third result))
	(add-prems (fourth result))
	(add-concs (fifth result))
	(new-goal (sixth result))
	(back (seventh result))
	)
    (omega~message "~& data: ~S " (prob~proof-steps new-pds))
    (plan~message "~% back :~S" back)
    (if back
	(let* ((old-just (node~justification conc))
	       (conc-ctrl (pdsc~create (pdsj~reasons old-just)
				       (pdsj~sponsors old-just)
				       (pdsj~unsponsors old-just)))
	       (new-just (node~justification new-goal))
	       (conc-just (pdsj~insert-just-below old-just new-just))
	       (new-reason (pdsc~an-create conc new-just))
	       (codomain (mapp~codomain (meth~mapp-mapp (pdsj~subst conc-just))))
	       )
	  (plan~message "new-goal: ~S ~S" new-goal new-just)
	  
	  (setf (pdsj~control conc-just) conc-ctrl)
	  (pdsj~insert-reason! new-just new-reason)
	  (setf (node~justification conc) conc-just)
	  (setf (mapp~codomain (meth~mapp-mapp (pdsj~subst conc-just)))
		(substitute conc new-goal codomain))
	  (setf p (pdsj~subst conc-just))
	  )
      (progn
	(dolist (conc add-concs)
	  (let* ((old-just (node~justification conc))
		 (new-just (pdsj~above old-just))
		 )
	    (setf (node~justification conc)
		  new-just)
	    (setf (pdsj~above new-just)
		  old-just)
	    (setf (pdsj~below new-just)
		  nil)
	    )
	  )
	))
    (plan=set-difference (prob~proof-steps new-pds)  ;; insert all nodes exept of...
			 (append add-prems
				 add-hyps
				 add-concs))
    ))

(defun plan~try-planning-step (method-name pds &optional (backwards nil))
  (let* ((agenda (pds~agenda pds))
	 (tasks (if backwards
		    (agenda~all-tasks agenda)
		  (list (first (agenda~all-tasks agenda)))))
	 ;; plane zunaechst nur den first-task wenn forward-planning!!!!!!!
	 (method (meth~find-method method-name))
	 )
    (when (and method tasks)
      (dolist (task tasks)
	(let* ((goal (agenda~task-node task)))
	  (when goal
	    (let* ((supports (pds~node-supports goal pds))
		   (selected-sequents (cri~call (list (list goal)
						      supports)
						:kind 'sequents
						:task task
						:tasks (list task)
						:pds pds))
		   (selected-method (plan=process-cri-method-list
				     (cri~call (list (keim~name method))
					       :kind 'methods
					       :task task
					       :tasks (list task)
					       :pds pds))
				    ))
					;		(plan~message "~% sel-meth:~S" selected-method)
					;		(plan~message "~% sel-seq:~S" selected-sequents)
	      (if (consp (first selected-method))   ; selected-method is a combined method
		  (let* ((select (first selected-method))
			 (supp (cond ((third select)
				      (third select))
				     ((second selected-sequents)
				      (second selected-sequents))
				     (T supports)))
			 (new-goal goal)
			 (params (second select))
			 (result
			  (when (eql (first (first selected-method))
				     method)
			    (plan=step-planner method
					       pds
					       task
					       new-goal
					       supp
					       params))
			  )
			 )
		    (when result
		      (return-from plan~try-planning-step T)))
		(let* ((new-goal (if (first selected-sequents)
				     (first selected-sequents)
				   goal))
		       (supp (if (second selected-sequents)
				 (second selected-sequents)
			       supports))
		       (result (plan=step-planner method
						  pds
						  task
						  goal
						  supp
						  )))
		  (when result
		    (return-from plan~try-planning-step T)
		    )))))))
      nil)))


;;******************************************************
;;   step planner                                      *
;;******************************************************

(defun plan=step-planner (method pds task goal supports &optional (parameters nil))
  (declare (edited  "24-FEB-1999")
	   (authors Cullrich Jzimmer)
	   (input   "A method, a PDS, the task to plan, a goal, the corresponding"
		    "support nodes and optional the method-parameters.")
	   (effect  "Tries to do a planning step with the given method "
		    "arguments.")
	   (value   "The new agenda, if the method could be applied."
		    "NIL otherwise."))
  ;;LC: angepasst
  (let ((node-formula (pds~task-formula task pds))
	(agenda (pds~agenda pds)))
    (multiple-value-bind (mmatching new-opens new-supps rest-mmatchings rest-methods)
	(plan=apply-method pds goal node-formula supports (list method) () task parameters)
      (when mmatching
	      ;;; One method was successfully applied to the TASK-NODE:
	(let ((applied-method (plan~matched-method mmatching))
	      (resulted-cstrpool (plan~mmatch-cstr-state mmatching))
		    ;;; Remark to the change of cstr+state while applying and expanding methods:
	      ;; It must be dinstinguished between adding new meta-variable bindings and
	      ;; changing the constraint pool. When a method application (expansion) only
	      ;; changes the cstr+constraint of CSTR+STATE and does not add meta-variable
	      ;; bindings, then the current cstr+state gets an empty pds~cstrpool-bindings.
	      ;; This is needed to prevent unneeded updating schematic tasks, namely when
	      ;; the meta-variable bindings is not changed:
	      )
	  (if (eq goal (plan~mmatch-goal mmatching))
		    ;;; TASK was solved by a backward method:
	      ;; 1- Carry out the method actions first, because these can make some
	      ;; changes to the supports of the involved nodes. For instance, by applying
	      ;; a method M we get a new hypothesis (H1 (and a b)) which occurs in a subgoal
	      ;; Li, hence during the application of M H1 is added to the supports of Li.
	      ;; Suppose M has the outline actions [Li (unsponsor H1) (sponsor Lj)] where
	      ;; Lj corresponds to (Lj (H1) a ("AndEL" () (H1))). To prevent that H1 will
	      ;; be splitted into (Ln a) and (Ln+1 b) and these are added as supports to
	      ;; Li, we have to carry out the actions of M before normalizing the new
	      ;; closed nodes and we must prohibit to normalize new closed nodes which does 
	      ;; not support any node.
	      ;; 2- Update the goal-schemas on the agenda but not TASK, because it was carried
	      ;; out, determine hereby the new-bound-tasks on the agenda.
	      ;; 3- Try to restrict some tasks and normalize the new supports.
	      ;; 4- Set the alternative mmatchings of TASK-NODE to REST-MMATCHINGS and
	      ;; alternative methods to REST-METHODS union the methods eliminated by the CRI.
	      ;; 5- Interpret the matched method orderings using the remaining tasks.
	      ;; 6- Replace TASK by the NEW-TASKS and eventually the associated pseudo-task
	      ;; when the applied method is non-reliable.
	      (let* ((cstr-bindings (when resulted-cstrpool
				      (pds~cstrpool-bindings resulted-cstrpool)))
		     (new-bound-tasks (when cstr-bindings
					(agenda~update-goal-schemas! agenda cstr-bindings task)))
		     (mmatching-mmapp (plan~mmatch-mmapp mmatching)))
		;; Because the execution of method actions can create new nodes, we have
		;; to insert the reasons of this nodes, which are the same as those of NEW-OPENS:
		(plan=execute-outline-actions goal new-opens mmatching pds)
		(multiple-value-bind (new-agenda remaining-tasks)
		    (plan=not-restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
		  (let ((new-tasks (if (meth~non-reliability applied-method)
                                             ;;; METHOD is non-reliable, we have to consider
                                       ;; a new pseudo task for TASK-NODE:
                                       (cons (agenda~create-pseudo-goal goal) remaining-tasks)
                                     remaining-tasks)))
                    (setf (pdsn~alternative-mmatchings goal) rest-mmatchings                  ;; (4)
                          (pdsn~alternative-methods goal) rest-methods)
                    (multiple-value-bind (first-task orderings)
                        (plan=create-agenda-orderings (meth~outline-orderings applied-method) ;; (5)
                                                      mmatching-mmapp new-tasks)
		      (agenda~replace-task task first-task (remove first-task new-tasks)
					   orderings new-agenda)))))
	    
		  ;;; A forward method was applied to contribute to the solving of TASK which is
	    ;; currently unsolved:
	    ;; 1- Carry out the matched method actions.
	    ;; 2- Update the goal-schemas on the agenda including TASK and determine hereby
	    ;; the new-bound-tasks on the agenda.
	    ;; 3- Try to restrict some tasks and normalize the new supports.
	    ;; 4- Dont change the alternative mmatchings and the alternative method slots
	    ;; of TASK-NODE.
	    ;; 5- Interpret the matched method orderings using the remaining tasks.
	    ;; 6- Insert the NEW-TASKS, including eventually pseudo-tasks when the applied
	    ;; method is non-reliable, into AGENDA:
	    (let* ((cstr-bindings (when resulted-cstrpool
				    (pds~cstrpool-bindings resulted-cstrpool)))
		   (new-bound-tasks (when cstr-bindings
				      (agenda~update-goal-schemas! agenda cstr-bindings)))
		   (mmatching-mmapp (plan~mmatch-mmapp mmatching))
		   (one-conc (find-if-not #'pdsn~hypothesis-p new-supps)))
	      (plan=execute-outline-actions one-conc new-opens mmatching pds)
	      (multiple-value-bind (new-agenda remaining-tasks)
                  (plan=not-restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
                (let ((new-tasks (if (meth~non-reliability applied-method)
                                           ;;; METHOD is non-reliable, we have to consider
                                     ;; a new pseudo task for TASK-NODE:
                                     (append (mapcar #'agenda~create-pseudo-goal
                                                     (remove-if #'pdsn~hypothesis-p new-supps))
                                             remaining-tasks)
                                   remaining-tasks)))
                  (multiple-value-bind (first-task orderings)
                      (plan=create-agenda-orderings (meth~outline-orderings applied-method)
                                                    mmatching-mmapp new-tasks)
                    (agenda~insert-tasks task first-task new-tasks orderings new-agenda)
		    ))))))))))
	      

(defun plan=not-restrict-tasks (new-opens new-supps new-bound-tasks agenda pds)
  (declare (edited  "26-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of open nodes, a list of closed nodes, a list of"
		    "bound AGENDA+GOAL-SCHEMAs, an agenda, and a PDS. The three"
		    "first inputs are obtained by a method application: NEW-OPENS"
		    "are subgoals of this method, NEW-SUPPS are new closed nodes"
		    "added by the method as supports, and NEW-BOUND-TASKS are"
		    "schematic goals whose formulas become closed because of the"
		    "constraints engendered by this method.")
	   (effect  "Eventually closes some open nodes in the PDS and splits"
		    "the conjunctions within the NEW-SUPPS.")
	   (value   "A pair: the new agenda and a list of agenda tasks."))
  (let* ((names (mapcar #'keim~name new-opens))
	 (old-opens (remove-if #'(lambda (node)
				 (member (keim~name node)
					 names))
			     (pds~open-nodes pds))))
;    (setf open-test (list old-opens new-opens))
					;    (format t "Test: ~s ~s" old-opens new-opens)
    (when (and (boundp 'ana*ina-flag) ana*ina-flag)
      (setf new-opens (ana~check-for-internal-analogy new-opens old-opens)))
    (let ((new-agenda agenda)
	  (goal-schema-tasks)
	  (remaining-opens))
    ;;; 2: Try to restrict each new open node whose formula does not contain
      ;; meta-variables by applying TASK-RESTRICTING-METHODS. For each new open 
      ;; node with schematic formula, a goal-schema task is created.
      (dolist (open-node new-opens)
	(if (agenda~schematic-formula-p (node~formula open-node))
	  (setq goal-schema-tasks (cons (agenda~create-goal-schema open-node)
					goal-schema-tasks))
					;(multiple-value-bind (mmatching)
					;    (plan=apply-first-method pds open-node (node~formula open-node)
					;			     (pds~node-supports open-node pds) nil nil 
					;			     meth*restricting-methods)
					;  (unless mmatching
	(setq remaining-opens (cons open-node remaining-opens))))
    ;;; 4: Try to normalize the new supports by applying SUPP-NORMALIZING-METHODS
    ;; and try simultenously to restrict some open nodes. Applying a normalization
    ;; method delivers new supports and their supported nodes. These supported
    ;; nodes are considered to be closed by applying TASK-RESTRICTING-METHODS using
    ;; the new delivered supports. When a supported node can be closed, then this can
    ;; either belong to the REMAINING-OPENS or its associated task belongs to the
    ;; agenda. This process should deliver the elements of REMAINING-OPENS which 
    ;; could not be closed:
					;(when new-supps
					;  (multiple-value-bind (an-agenda opens)
					;	  (plan=normalize-supports new-supps remaining-opens new-agenda pds)
					;	(setq new-agenda an-agenda
					;	      remaining-opens opens)))
    (values new-agenda
	    (append goal-schema-tasks
		    (mapcar #'agenda~create-goal remaining-opens)))
    ))
  )

(defun plan=apply-method (pds goal goal-formula supports method alternatives task
			      &optional (parameters nil))
  (declare (edited  "18-SEP-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
					; was plan=apply-first-method
  (if alternatives
      (multiple-value-bind (mmatching new-opens new-supps additional-return)
	  (plan~apply-mmatching task (first alternatives) pds)
	;;; This function should carry out the method matching and returns it together
	;; with NEW-OPENS, NEW-SUPPS, and possibly ADDITIONAL-RETURN depending on whether
	;; the key word :EXTRA-RETURN is set. NEW-OPENS are new open nodes coming from
	;; add-premises of the applied method. NEW-SUPPS are new closed nodes corresponding
	;; to add-conclusions of the applied methods and new generated hypothesis and/or
	;; supports. When :EXTRA-RETURN is set to "PREMS-SUPPORTED-NODES" then return the
	;; of pds-open nodes supported by the method existent premises.
	(if mmatching
	    (values  mmatching new-opens new-supps (rest alternatives)
		     (plan=sorted-rest-methods (plan~matched-method mmatching) () method)
		     ())
	  (plan=apply-method pds goal goal-formula supports method (rest alternatives) task)))
    (when method
      (let ((alternative-matchings (plan~match-method (first method) goal goal-formula
						      supports pds :parameters parameters)))
	(if alternative-matchings
	    (multiple-value-bind (mmatching new-opens new-supps additional-return)
		(plan~apply-mmatching task (first alternative-matchings) pds)
	      (if mmatching
		  (values mmatching new-opens new-supps (rest alternative-matchings) (rest method)
			  additional-return)
		(plan=apply-method pds goal goal-formula supports nil (rest alternative-matchings)
				   task)
		)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; DIFFERENT MODI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plan=automatically (max-steps pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "17-MAR-1998")
	   (authors Lassaad)
	   (input   "An agenda, and a PDS.")
	   (effect  "Constructs a complete PDS or fails.")
	   (value   "Unspecified."))
  (cond ((agenda~empty-p agenda)
	 ;;; No more tasks on the agenda
	 (setf (pds~agenda pds) agenda)
	 (plan~message "The current pds ~A is planned!~%" pds)
	 (plan~show-data)
	 (plan~instantiate-metavars pds))
	((or (null max-steps) (not (zerop max-steps)))
	 ;;; There is some tasks on the agenda and the maximal number of allowed planning steps
	 ;; is not reached: Try to carry out one planning step
	 (plan=show-agenda "~%AGENDA BEFORE:~%~A" agenda)
	 (multiple-value-bind (root-taskp new-agenda success)
	     (plan=do-step pds agenda)
	   (when (plan~loui-interface-p)
	     (socket~write "update-agenda")
	     (parse~agenda (agenda~for-loui new-agenda))
	     (socket~write "update-prooftree")
	     (parse~proof pds)
	     ;;; LOUI: Falls planer zu schnell ist, dann planer soll auf eine Nachricht warten:
	     )
	   (cond (success
		  (plan=show-agenda "~%AGENDA AFTER PLANNING:~%~A" new-agenda)
		  ;;; The planning step was successfull
		  (plan=automatically (when max-steps (- max-steps 1)) pds new-agenda))
		 ((and (not success) root-taskp)
		  (plan=show-agenda "~%AGENDA AFTER BACKTRACKING:~%~A" new-agenda)
		  ;;; The planning step was backtracking, where the PDS root node had to be deleted:
		  ;; When there is no task on the NEW-AGENDA which may be considered before the ROOT-TASKP,
		  ;; then signal that no plan can be found for the current PDS. Otherwise, you have to consider
		  ;; another task before the ROOT-TASKP:
		  (multiple-value-bind (unblocked-tasks were-blocked-tasks)
		      (pds~first-tasks! pds new-agenda)
		    (let* ((first-tasks (if unblocked-tasks unblocked-tasks were-blocked-tasks))
			   (other-tasks (remove-if #'(lambda (tk) (eq tk root-taskp)) first-tasks)))
		      (cond (other-tasks
			     (let ((new-agenda2 (agenda~insert-tasks root-taskp nil other-tasks 
								     (list (agenda~ordering-create other-tasks
												   (list root-taskp)))
								     new-agenda)))
			       (plan=automatically (when max-steps (- max-steps 1)) pds new-agenda2)))
			    ((not other-tasks)
			     (setf (pds~agenda pds) new-agenda)
			     (plan~message "No proof plan can be found for the current pds ~A!~%"
					   pds))))))
		 ((and (not success) (not root-taskp))
		  (plan=show-agenda "~%AGENDA AFTER BACKTRACKING:~%~A" new-agenda)
		  ;;; Try to apply a method:
		  (plan=automatically (when max-steps (- max-steps 1)) pds new-agenda)))))
	((zerop max-steps)
	 (setf (pds~agenda pds) agenda)
	 (plan~message ";;; The allowed planning steps are used up and the current pds ~A is not yet planned!"
		       pds))
	))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plan=interactively (max-tasks max-meths max-steps pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "24-APR-1998")
	   (authors Lassaad)
	   (input   "...")
	   (effect  )
	   (value   ))
  (if (agenda~empty-p agenda)
      (progn
	(setf (pds~agenda pds) agenda)
	(plan~message "The current pds ~A is planned!~%" pds)
	(plan~show-data)
	(plan~instantiate-metavars pds)
	)
    (if (or (null max-steps) (not (zerop max-steps)))
	(multiple-value-bind (root-taskp new-agenda success)
	    (plan=step-interactively max-tasks max-meths pds agenda)
	  (when (plan~loui-interface-p)
	    (socket~write "update-agenda")
	    (parse~agenda (agenda~for-loui new-agenda))
	    (socket~write "update-prooftree")
	    (parse~proof pds)
	    ;;; LOUI: Falls planer zu schnell ist, dann planer soll auf eine Nachricht warten:
	    )
	  (if success
	      (plan=interactively max-tasks max-meths (when max-steps (- max-steps 1)) pds new-agenda)
	    ;;; The done step in PLAN=STEP-INTERACTIVELY was backtracking: 
	    (if root-taskp
	        ;;; The PDS root node had to be deleted in the backtracking step: When there is no task
		;; on the NEW-AGENDA different from ROOT-TASKP then signal that no plan can be found for
		;; the current PDS. Otherwise, you have to consider another task before the ROOT-TASKP:
		(let ((other-task (agenda~get-first-task new-agenda
							 #'(lambda (tk) (not (eq tk root-taskp))))))
		  (if other-task
		      (let* ((new-agenda2 (agenda~replace-task root-taskp nil nil nil new-agenda))
			     (new-agenda3 (agenda~insert-tasks other-task nil (list root-taskp)
							       (list (agenda~ordering-create (list other-task)
											     (list root-taskp)))
							       new-agenda2)))
			(plan=interactively max-tasks max-meths (when max-steps (- max-steps 1)) pds new-agenda3))
		    (progn
		      (setf (pds~agenda pds) new-agenda)
		      (plan~message "No proof plan can be found for the current pds ~A!~%"
				     pds))))
	      ;;; Try to apply a method:
	      (plan=interactively max-tasks max-meths (when max-steps (- max-steps 1)) pds new-agenda))))
      (when (zerop max-steps)
	(progn
	  (setf (pds~agenda pds) agenda)
	  (plan~message ";;; The allowed planning steps are used up and the current pds ~A is not yet planned!"
			 pds))))
    ))


(defun plan=step-interactively (max-tasks max-meths pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "A non-empty agenda, and a pds")
	   (effect  "Try to plan a task by a method application or expansion, when"
		    "this is possible, otherwise take this task back or anther task to"
		    "go on.")
	   (value   "A tuple:"
		    "- NIL if the considered task can be planned or the PDS root node"
		    "is not involved in the backtracking step, and the task associated"
		    "to the PDS root node, if this is involved in the backtracking step."
		    "- the resulted agenda."
		    "- T, when this step corresponds to a method application or expansion,"
		    "and NIL when it corresponds to backtracking some tasks."))
  (multiple-value-bind (first-tasks)
      (agenda~first-tasks! agenda)
    (let ((tasks-to-consider (remove-if #'(lambda (task)
					    (and (agenda~goal-schema-p task)
						 (agenda~goal-schematic-p task)))
					first-tasks)))
      (if tasks-to-consider
	  ;;; Control heuristic: goal-schemas are considered later, i.e., goals,
	  ;; goal-schemas which are no more schematic, and pseudo-goals are considered first:
	  (if (rest tasks-to-consider)
	      ;;; More than one task to be considered, then invoke the CRI:
	      (let* ((cri-alternatives (cri~call tasks-to-consider
						 :kind 'tasks
						 :tasks tasks-to-consider
						 :pds pds))
		     (user-task (when (and max-tasks (> (length cri-alternatives) max-tasks))
				  (socket~write "select-query")
				  (parse~task-list (mapcar #'(lambda (alter)
							       (if (consp alter)
								   (first alter)
								 alter))
							   cri-alternatives))
				  (nth (read-from-string (socket~read)) cri-alternatives))))
		;;; CRI returns a list of alternatives, where an alternative is either a task,
		;; a list (task [methods] [params] [supps]), or a list (task [method] [params] [supps]).
		;; This list must be merged with the task failed-methods, with the alternative methods:
		(multiple-value-bind (task first-methods then-methods)
		    (if (eq cri-alternatives tasks-to-consider)
			(if user-task
			    (values user-task nil meth*planning-methods)
			  (values (first tasks-to-consider) nil meth*planning-methods))
		      (progn ;(omega~trace "Intervention of control rules ...")
			(plan=merge-task-alternatives
			      (if user-task
				  (remove-if-not #'(lambda (alter) (eq user-task (first alter)))
						 cri-alternatives)
				cri-alternatives)
			      meth*planning-methods)))
		  ;;; FIRST-METHODS should be a list of methods and methods with control informations
		  (if (agenda~pseudo-goal-p task)
		      (plan=refine-task task agenda pds)
		    ;;; Filter the relevant methods and call plan=task-interactively:
		    (let* ((task-node (agenda~task-node task))
			   (node-mmatchings (pdsn~alternative-mmatchings task-node))
			   (node-methods (pdsn~alternative-methods task-node))
			   (mmatchings-method (when node-mmatchings
						(let ((mmatching (first node-mmatchings)))
						  (unless (find (plan~matched-method mmatching)
								node-methods)
						    (if (plan~mmatch-parameters mmatching)
							(cons (plan~matched-method mmatching)
							      (plan~mmatch-parameters mmatching))
						      (plan~matched-method mmatching))))))
			   (considered-methods (if mmatchings-method
						   (cons mmatchings-method
							 (pdsn~failed-methods task-node))
						  (pdsn~failed-methods task-node)))
			   (the-first-methods (if (or node-methods considered-methods)
						  ;;; TASK was closed and it is now open because of backtracking: 
						  (remove-if-not #'(lambda (alter)
								     (or (and (meth~p alter)
									      (find alter node-methods))
									 (find (first alter) node-methods)
									 (second alter)))
								 first-methods)
						first-methods))
			   (the-then-methods (if (or node-methods considered-methods)
						  ;;; TASK was closed and it is now open because of backtracking:
						 (plan=set-difference node-methods then-methods)
					       then-methods))
			   (non-failed-first-methods (plan=set-difference the-first-methods considered-methods))
			   (non-failed-then-methods (plan=set-difference the-then-methods considered-methods)))
		      (multiple-value-bind (meth&ctrls rest-methods)
			  (plan=merge-method-alternatives non-failed-first-methods
							  non-failed-then-methods
							  (remove-if #'meth~p considered-methods))
			(let ((new-agenda (plan=task-interactively max-meths task task-node
								   node-mmatchings meth&ctrls rest-methods
								   pds agenda :invoke-cri
								   (not first-methods))))
			  (if new-agenda (values nil new-agenda t)
			    (plan=backtrack-task! task agenda pds))))))))
	    ;;; Only one task to be considered first:
	    (if (agenda~pseudo-goal-p (first tasks-to-consider))
		(plan=refine-task (first tasks-to-consider) agenda pds)
	      ;;; Filter the relevant methods and call plan=task-interactively:
	      (let* ((task (first tasks-to-consider))
		     (task-node (agenda~task-node task))
		     (node-mmatchings (pdsn~alternative-mmatchings task-node))
		     (node-methods (pdsn~alternative-methods task-node))
		     (failed-methods (pdsn~failed-methods task-node))
		     (methods (if (or node-methods node-mmatchings failed-methods)
				  node-methods
				meth*planning-methods))
		     (new-agenda (plan=task-interactively max-meths task task-node node-mmatchings nil
							  methods pds agenda :invoke-cri t)))
		(if new-agenda (values nil new-agenda t)
		  (plan=backtrack-task! task agenda pds)))))
	;;; The tasks to be considered first are all schematic, invoke the CRI:
	(let* ((cri-alternatives (cri~call first-tasks
					   :kind 'schematic-tasks
					   :tasks first-tasks
					  ; :agenda agenda   LC: the agenda may not be considered
					   :pds pds))
	       (user-task (when (and max-tasks (> (length cri-alternatives) max-tasks))
			    (socket~write "select-query")
			    (parse~task-list (mapcar #'first cri-alternatives))
			    (nth (read-from-string (socket~read)) cri-alternatives))))
	  (multiple-value-bind (task first-methods then-methods)
	      (if (eq cri-alternatives first-tasks)
		  (if user-task (values user-task nil meth*MOReasoning-methods)
		    (values (first first-tasks) nil meth*MOReasoning-methods))
		(progn ;(omega~trace "Intervention of control rules ...")
		       (plan=merge-task-alternatives
			(if user-task
			    (remove-if-not #'(lambda (alter) (eq user-task (first alter)))
					   cri-alternatives)
			  cri-alternatives)
			meth*MOReasoning-methods)))
	    
	    	    ;;; Filter the relevant methods and call plan=task-interactively:
	    (let* ((task-node (agenda~task-node task))
		   (node-mmatchings (pdsn~alternative-mmatchings task-node))
		   (node-methods (pdsn~alternative-methods task-node))
		   (mmatchings-method (when node-mmatchings
					(let ((mmatching (first node-mmatchings)))
					  (unless (find (plan~matched-method mmatching)
							node-methods)
					    (if (plan~mmatch-parameters mmatching)
						(cons (plan~matched-method mmatching)
						      (plan~mmatch-parameters mmatching))
					      (plan~matched-method mmatching))))))
		   (considered-methods (if mmatchings-method
					   (cons mmatchings-method
						 (pdsn~failed-methods task-node))
					 (pdsn~failed-methods task-node)))
		   (the-first-methods (if (or node-methods considered-methods)
					  ;;; TASK was closed and it is now open because of backtracking: 
					  (remove-if-not #'(lambda (alter)
							     (or (and (meth~p alter)
								      (find alter node-methods))
								 (find (first alter) node-methods)
								 (second alter)))
							 first-methods)
					first-methods))
		   (the-then-methods (if (or node-methods considered-methods)
					 ;;; TASK was closed and it is now open because of backtracking:
					 (plan=set-difference node-methods then-methods)
				       then-methods))
		   (non-failed-first-methods (plan=set-difference the-first-methods considered-methods))
		   (non-failed-then-methods (plan=set-difference the-then-methods considered-methods)))
	      (multiple-value-bind (meth&ctrls rest-methods)
		  (plan=merge-method-alternatives non-failed-first-methods
						  non-failed-then-methods
						  (remove-if #'meth~p considered-methods))
		(let ((new-agenda (plan=task-interactively max-meths task task-node
							   node-mmatchings meth&ctrls rest-methods
							   pds agenda :invoke-cri (not first-methods))))
		  (if new-agenda (values nil new-agenda t)
		    (plan=backtrack-task! task agenda pds)))))))))
    ))

(defun plan=task-interactively (max-meths task task-node mmatchings meth&ctrls methods pds
					  &optional (agenda (pds~agenda pds))
					  &key ((:invoke-cri invoke-cri)))			    
  (declare (edited  "28-MAR-1998")
	   (authors Lassaad)
	   (input   "A (non-pseudo) task, an agenda, a pds, and some key words: :CONTEXT"
		    "specifies in which context this function is called, for instance, the"
		    "context TASK-RESTRICTION means that this task was called to apply"
		    "task-resricting methods to TASK, in such context the CRI may not be"
		    "invoked to eventually restrict the available methods or sort them."
		    ":SUPPORTS, when stated it delivers the support nodes to be considered."
		    ":METHODS, delivers the methods to be considered and :FIRST-METHODS, when"
		    "given, it is considered as the list of alternative methods to be considered"
		    "first. When :FIRST-METHODS is not NIL, then the rest relevant methods"
		    "for TASK must be given too in :METHODS, then this function will try to"
		    "apply the first alternative method and then store the rest of FIRST-METHODS"
		    "and METHODS in the remaining alternative-methods of TASK for eventually"
		    "backtracking. Moreover, in such situation, the CRI may not be invoked."
		    "When :FIRST-METHODS and :METHODS are not specified, then the alternative"
		    "methods are determined by considering the context and the task current"
		    "alternative methods.")
	   (effect  "Changes AGENDA and PDS.")
	   (value   "The resulted AGENDA after planning this task, if this task can be planned,"
		    "otherwise nil."))
  ;;; When there is no way either to solve the given TASK or to apply a method in order
  ;; to contribute to solving this TASK, then this method returns NIL and backtracking
  ;; must be carried out in the caller of this function.
  (let ((node-formula (if (agenda~goal-schema-p task)
			  (agenda~goal-schema-formula task)
			(node~formula task-node)))
	(node-supports (pds~node-supports task-node pds))
	(all-meth&ctrls meth&ctrls)
	(all-methods methods)
	(all-mmatchings mmatchings))
    (omega~trace "")
    (omega~trace "Planning task: ~A  ...." task)
    ;;; Invoking CRI when stated
    (when (and invoke-cri (not meth&ctrls))
      (let ((cri-methods (plan=process-cri-method-list
			  (cri~call (mapcar #'keim~name methods) :kind 'methods :task task :task-node task-node
				    :task-formula node-formula :agenda agenda :pds pds))))
	(when (and methods (not cri-methods))
	  (omega~warn ";;;plan=task-interactively: An empty method alternative list returned by CRI!"))
	(multiple-value-bind (new-meth&ctrls rest-methods)
	    (if (eq cri-methods methods) (values nil methods)
	      (let* ((mmatchings-method (when mmatchings
					  (let ((mmatching (first mmatchings)))
					    (unless (find (plan~matched-method mmatching) methods)
					      (if (plan~mmatch-parameters mmatching)
						  (cons (plan~matched-method mmatching)
							(plan~mmatch-parameters mmatching))
						(plan~matched-method mmatching))))))
		     (considered-methods (if mmatchings-method
					     (cons mmatchings-method
						   (pdsn~failed-methods task-node))
					   (pdsn~failed-methods task-node))))
					;(omega~trace "Intervention of control rules ...")
		(plan=merge-method-alternatives cri-methods methods
						(remove-if #'meth~p considered-methods))))
	  (setq all-meth&ctrls new-meth&ctrls all-methods rest-methods))))
    ;;; Match all methods and collect the mmatchings:
    ;; First ALL-METH&CTRLS
    (dolist (meth&ctrl all-meth&ctrls)
      (let ((alternatives (if (listp meth&ctrl)
			      (if (third meth&ctrl)
				  ;;; METH&CTRL: <meth, params, supps>:
				  (plan~match-method (first meth&ctrl) task-node node-formula
						     (third meth&ctrl) pds :parameters (second meth&ctrl))
				;;; METH&CTRL: <meth, params>:
				(plan~match-method (first meth&ctrl) task-node node-formula node-supports pds
						   :parameters (second meth&ctrl) :invoke-cri t))
			    ;;; METH&CTRL: meth:
			    (plan~match-method meth&ctrl task-node node-formula node-supports pds :invoke-cri t))))
	(when alternatives
	  (setq all-mmatchings (append all-mmatchings alternatives)))))
    ;; then ALL-METHODS
    (dolist (method all-methods)
      (unless (= 0 (meth~rating method)) ;; choose only methods with rating not zero
	(let ((alternatives (plan~match-method method task-node node-formula node-supports pds :invoke-cri t)))
	  (when alternatives
	    (setq all-mmatchings (append all-mmatchings alternatives))))))
    ;;; Invoke user if necessary
    (when all-mmatchings
      (let ((mmatching (if (and max-meths (> (length all-mmatchings) max-meths))
			   (progn
			     (socket~write "select-query")
			     (parse~mmatching-list all-mmatchings)
			     (nth (read-from-string (socket~read)) all-mmatchings))
			 (first all-mmatchings))))
	(multiple-value-bind (mmatching new-opens new-supps)
	    (plan~apply-mmatching task mmatching pds)
	  ;;; One method was successfully applied to the TASK-NODE:
	  (let ((applied-method (plan~matched-method mmatching))
		(resulted-cstrpool (plan~mmatch-cstr-state mmatching))
		;;; Remark to the change of cstr+state while applying and expanding methods:
		;; It must be dinstinguished between adding new meta-variable bindings and
		;; changing the constraint pool. When a method application (expansion) only
		;; changes the cstr+constraint of CSTR+STATE and does not add meta-variable
		;; bindings, then the current cstr+state gets an empty pds~cstrpool-bindings.
		;; This is needed to prevent unneeded updating schematic tasks, namely when
		;; the meta-variable bindings is not changed:
		)
	    (if (eq task-node (plan~mmatch-goal mmatching))
		;;; TASK was solved by a backward method:
		;; 1- Carry out the method actions first, because these can make some
		;; changes to the supports of the involved nodes. For instance, by applying
		;; a method M we get a new hypothesis (H1 (and a b)) which occurs in a subgoal
		;; Li, hence during the application of M H1 is added to the supports of Li.
		;; Suppose M has the outline actions [Li (unsponsor H1) (sponsor Lj)] where
		;; Lj corresponds to (Lj (H1) a ("AndEL" () (H1))). To prevent that H1 will
		;; be splitted into (Ln a) and (Ln+1 b) and these are added as supports to
		;; Li, we have to carry out the actions of M before normalizing the new
		;; closed nodes and we must prohibit to normalize new closed nodes which does 
		;; not support any node.
		;; 2- Update the goal-schemas on the agenda but not TASK, because it was carried
		;; out, determine hereby the new-bound-tasks on the agenda.
		;; 3- Try to restrict some tasks and normalize the new supports.
		;; 4- Set the alternative mmatchings of TASK-NODE to REST-MMATCHINGS and
		;; alternative methods to REST-METHODS union the methods eliminated by the CRI.
		;; 5- Interpret the matched method orderings using the remaining tasks.
		;; 6- Replace TASK by the NEW-TASKS and eventually the associated pseudo-task
		;; when the applied method is non-reliable.
		(let* ((cstr-bindings (when resulted-cstrpool
					(pds~cstrpool-bindings resulted-cstrpool)))
		       (new-bound-tasks (when cstr-bindings
					  (agenda~update-goal-schemas! agenda cstr-bindings task)))
		       (mmatching-mmapp (plan~mmatch-mmapp mmatching)))
		  (plan=execute-outline-actions task-node new-opens mmatching pds)
		  (multiple-value-bind (new-agenda remaining-tasks)
		      (plan=restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
		    (let ((new-tasks (if (meth~non-reliability applied-method)
					 ;;; METHOD is non-reliable, we have to consider
					 ;; a new pseudo task for TASK-NODE:
					 (cons (agenda~create-pseudo-goal task-node) remaining-tasks)
				       remaining-tasks)))
		      (let ((matching-params (plan~mmatch-parameters mmatching))
			    (remain-matchings)
			    (remain-methods))
			(dolist (mmatch (remove mmatching all-mmatchings))
			  (let ((mmatch-meth (plan~matched-method mmatch))
				(mmatch-para (plan~mmatch-parameters mmatch)))
			    (if (eq applied-method mmatch-meth)
				;;; Same method as APPLIED-METHOD:
				(if matching-params
				    (when (equal matching-params mmatch-para)
				      ;;; Same parameters as those of APPLIED-METHOD:
				      (push mmatch remain-matchings))
				  (push mmatch remain-matchings))
			      (unless mmatch-para
				(unless (find mmatch-meth remain-methods)
				  (push mmatch-meth remain-methods))))))
			(setf (pdsn~alternative-mmatchings task-node) remain-matchings
			      (pdsn~alternative-methods task-node) remain-methods))
		      (multiple-value-bind (first-task orderings)
			  (plan=create-agenda-orderings (meth~outline-orderings applied-method)
							mmatching-mmapp new-tasks)
			(agenda~replace-task task first-task (remove first-task new-tasks)
					     orderings new-agenda)))))
	      ;;; A forward method was applied to contribute to the solving of TASK which is
	      ;; currently unsolved:
	      ;; 1- Carry out the matched method actions.
	      ;; 2- Update the goal-schemas on the agenda including TASK and determine hereby
	      ;; the new-bound-tasks on the agenda.
	      ;; 3- Try to restrict some tasks and normalize the new supports.
	      ;; 4- Dont change the alternative mmatchings and the alternative method slots
	      ;; of TASK-NODE.
	      ;; 5- Interpret the matched method orderings using the remaining tasks.
	      ;; 6- Insert the NEW-TASKS, including eventually pseudo-tasks when the applied
	      ;; method is non-reliable, into AGENDA:
	      (let* ((cstr-bindings (when resulted-cstrpool
				      (pds~cstrpool-bindings resulted-cstrpool)))
		     (new-bound-tasks (when cstr-bindings
					(agenda~update-goal-schemas! agenda cstr-bindings)))
		     (mmatching-mmapp (plan~mmatch-mmapp mmatching))
		     (one-conc (find-if-not #'pdsn~hypothesis-p new-supps)))
		(plan=execute-outline-actions one-conc new-opens mmatching pds)
		(multiple-value-bind (new-agenda remaining-tasks)
		    (plan=restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
		  (let ((new-tasks (if (meth~non-reliability applied-method)
				       ;;; METHOD is non-reliable, we have to consider
				       ;; a new pseudo task for TASK-NODE:
				       (append (mapcar #'agenda~create-pseudo-goal
						       (remove-if #'pdsn~hypothesis-p new-supps))
					       remaining-tasks)
				     remaining-tasks)))
		    (multiple-value-bind (first-task orderings)
			(plan=create-agenda-orderings (meth~outline-orderings applied-method)
						      mmatching-mmapp new-tasks)
		      (agenda~insert-tasks task first-task new-tasks orderings new-agenda))))))))
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixed-initiative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plan=mixed-initiative (max-steps pds &optional (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A non-empty agenda, and a pds.")
	   (effect  "Completes PDS in a reactive modus.")
	   (value   "T, if the mixed-initiative modus is changed, otherwise NIL."))
  (if (or (null max-steps) (not (zerop max-steps)))
      (let ((root-taskp)
	    (new-agenda)
	    (new-modusp))
        ;;; Send request message to LOUI:
	(socket~write "command?")
        ;;; Receive a message from LOUI:
	(let ((loui-message (socket~read)))
	  (if (string-equal loui-message "nothing")
	      ;;; No user suggestion is available
	      (multiple-value-bind (root-task the-agenda)
		  (plan=do-step pds agenda)
		(setq root-taskp root-task new-agenda the-agenda))
	    ;;; A user suggestion is delivered by LOUI, carry it out, and go on:
            (let* ((command&args (read-from-string loui-message))
                   (command (com~find-command (first command&args)
                                              (com~find-fragment 'suggestions))))
              (if command
                  ;;; A known COMMAND for a reactive planning step:
                  (let ((arg-types (com~argtypes command))
                        (given-args (rest command&args))
                        (parameters))
		    (if arg-types
                        ;;; COMMAND with arguments
                        (if given-args
                            ;;; Try to read the given arguments:
                            (progn 
                              (loop
                               (when (null arg-types) (return))
                               (if (comint~debugger-on-break comint*current-comint)
                                   (sys~handler-case
                                    (let ((arg-type (first arg-types))
                                          (given-arg (first given-args)))
                                      (setq arg-types (rest arg-types) given-args (rest given-args))
                                      (setq parameters
                                            (cons (arg~read-type arg-type given-arg) parameters)))
                                    (arg+input-error (c) (invoke-debugger c)))
                                 (sys~handler-case
                                  (let ((arg-type (first arg-types))
                                        (given-arg (first given-args)))
                                    (setq arg-types (rest arg-types) given-args (rest given-args))
                                    (setq parameters (cons (arg~read-type arg-type given-arg) parameters)))
                                  (arg+input-error (c)
                                                   (plan~message "The suggestion ~A cannot be carried out!~%~A"
                                                                  loui-message c)
                                                   (return (setq parameters nil))))))
			      (if parameters
                                  ;;; The given COMMAND arguments could be read successfully, then carry out
                                  ;; the associated reaction and go on:
                                  (multiple-value-bind (root-task the-agenda modusp)
                                      (apply (symbol-function (com~function command))
                                             (append (reverse parameters) (list pds agenda)))
                                    (setq root-taskp root-task new-agenda the-agenda new-modusp modusp))
                                ;;; The given COMMAND arguments could not be read, go on after signaling an
                                ;; information message during the argument reading loop.
                                (multiple-value-bind (root-task the-agenda)
                                    (plan=do-step pds agenda)
				  (setq root-taskp root-task new-agenda the-agenda))))
                          (progn
                            (plan~message "The suggestion ~A cannot be carried out because of missed arguments!"
                                           loui-message)
                            (multiple-value-bind (root-task the-agenda)
                                (plan=do-step pds agenda)
                              (setq root-taskp root-task new-agenda the-agenda))))
                      ;;; COMMAND without arguments, carry out the associated reaction and go on:
                      (multiple-value-bind (root-task the-agenda modusp)
                          (apply (symbol-function (com~function command)) (list pds agenda))
                        (setq root-taskp root-task new-agenda the-agenda new-modusp modusp))))
                ;;; Unknown COMMAND for reactive planning step, signal a message and go on:
                (progn
                  (plan~message "The unknown suggestion ~A cannot be carried out!" loui-message)
                  (multiple-value-bind (root-task the-agenda)
                      (plan=do-step pds agenda)
                    (setq root-taskp root-task new-agenda the-agenda)))))))
        ;;; Decide whether to go on or to end the planner loop:
	(socket~write "update-agenda")
	(parse~agenda (agenda~for-loui new-agenda))
	;(plan~message "~A" (agenda~for-loui new-agenda))
	(socket~write "update-prooftree")
	(parse~proof pds)
        (if (agenda~empty-p new-agenda)
            ;;; Signal success of planning the PDS:
            (progn
              (setf (pds~agenda pds) new-agenda)
              (plan~message "The current pds ~A is planned!~%" pds)
	      (plan~show-data)
	      (plan~instantiate-metavars pds)
	      )
          ;;; The last step was backtracking which had to delete the PDS root, try to find another task:
          (if root-taskp
              (let ((other-task (agenda~get-first-task new-agenda
                                                       #'(lambda (tk) (not (eq tk root-taskp))))))
                (if other-task
                    ;;; If another task is available then go on and consider this task before PDS root task:
                    (let* ((new-agenda2 (agenda~replace-task root-taskp nil nil nil new-agenda))
                           (new-agenda3 (agenda~insert-tasks other-task nil (list root-taskp)
                                                             (list (agenda~ordering-create (list other-task)
                                                                                           (list root-taskp)))
                                                             new-agenda2)))
		      (if new-modusp
			  (plan=plan pds plan*current-modus new-agenda2)
			(plan=mixed-initiative (when max-steps (- max-steps 1)) pds new-agenda3)))
                  ;;; PDS root task is the sole task on the agenda and cannot be solved, signal that
                  ;; no plan for PDS can be found:
                  (progn
                    (setf (pds~agenda pds) new-agenda)
                    (plan~message "No proof plan can be found for the current pds ~A!~%" pds))))
            ;;; The last step was backtracking without influencing the PDS root or a task restriction, go on:
	    (if new-modusp
		(plan=plan pds plan*current-modus new-agenda)
	      (plan=mixed-initiative (when max-steps (- max-steps 1)) pds new-agenda)))))
    (when (zerop max-steps)
      (progn
        (setf (pds~agenda pds) agenda)
        (plan~message ";;; The allowed planning steps are used up and the current pds ~A is not yet planned!"
                       pds)))))


    
;;;; Functions for the reactive modus:
(defun plan~reactive-change-modus (symbol bound-list &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   )
	   (effect  )
	   (value   "A tuple:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."
		    "- boolean whether planning modus is changed."))
  (let ((modus (plan~find-modus symbol)))
    (if modus
	(progn (setf (plan~modus-bounds modus) bound-list)
	       (setq plan*current-modus modus)
	       (plan~show-modus modus)
	       (values nil agenda T))
      (progn 
	(plan~message ";;; Unknown planning modus: ~A! Your suggestion is not applied!" symbol)
	(values nil agenda)))
    ))

(defun plan~back-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or nil.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is"
		    "obsolete. Otherwise, when there is an associated non-"
		    "pseudo task for NODE, then this task is taken back, and"
		    "if there is an associated pseudo task for NODE or there is"
		    "no associated task for NODE, then nothing is done.")
	   (value   "A tuple:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."
		    "- boolean whether planning modus is changed."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								  (eq (agenda~task-node tk) node))))))
	(if node-task
	    (plan=backtrack-task! node-task agenda pds)
	  (values nil agenda)))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~back-pseudo-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or nil.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is"
		    "obsolete. Otherwise, when there is an associated pseudo"
		    "task for NODE, then this task is taken back, and if there"
		    "is an associated non-pseudo task for NODE or there is no"
		    "associated task for NODE, then nothing is done.")
	   (value   "A pair:"
		    "- NIL or PDS root node."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (agenda~pseudo-goal-p tk)
								  (eq (agenda~task-node tk) node))))))
	(if node-task
	    (plan=backtrack-task! node-task agenda pds)
	  (values nil agenda)))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~next-task (node select-methods-p &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL, and a boolean.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated non-pseudo task for NODE, then this task is considered in"
		    "the next planning step, moreover when SELECT-METHODS-P is T, then we have to deliver"
		    "a list of non-failed methods for this NODE, a subset of them must be selected by the"
		    "user to be considered first.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  ;;; The list of methods to be selected by the user consists of methods and methods with parameters:
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								  (eq (agenda~task-node tk) node))))))
	(if node-task
	    (if select-methods-p
		;;; The user wants to select some methods:
		(let* ((node-failed-methods (pdsn~failed-methods node))
		       (node-methods (if (pdsn~alternative-methods node)
					 (pdsn~alternative-methods node)
				       (plan=set-difference (if (agenda~goal-p node-task) 
								meth*planning-methods  
							      meth*MOReasoning-methods)
							    node-failed-methods))))

		  ;;; Send LOUI a list of methods from which some methods has to be selected:
		  ;; TO-DO: parametrisierte Methoden rausnehmen:
		  (socket~write "choose-query")
		  (parse~plan-methods meth-to-select)
		  ;;; Wait for the LOUI message which corresponds to the selected methods:
		  (let ((selected-meths (read-from-string (socket~read)))
			;; TO-DO
			;;; (1 3 5) Positionen der ausgewaehlen Methoden:
			(meth-type (arg~find-argtype 'method))
			(taken-methods)
			(rejected-methods))
		    (loop
		     (when (null selected-meths) (return))
		     (sys~handler-case
		      (setq taken-methods (cons (arg~read-type meth-type (first selected-meths))
						taken-methods))
		      (arg+input-error ()
				       (setq rejected-methods
					     (cons (first selected-meths) rejected-methods))))
		     (setq selected-meths (rest selected-meths)))
		    (let* ((finally-taken-methods (remove-if #'(lambda (m)
								 (find m node-failed-methods
								       :test #'plan=parametrized-method-eq))
							     (reverse taken-methods)))
			   (failed-given-methods (mapcar #'(lambda (pm) (keim~name (first pm)))
							 (plan=set-difference taken-methods
									 finally-taken-methods)))
			   (rejected-methods (reverse rejected-methods)))
		      ;;; Signals some messages:
		      (if (and rejected-methods failed-given-methods)
			  (plan~message "The following methods are not taken into account:~%~{~A, ~}~A cannot be read, and ~%~{~A, ~}~A failed to close the node ~A."
					 (butlast rejected-methods) (first (last rejected-methods))
					 (butlast failed-given-methods) (first (last failed-given-methods))
					 (keim~name node))
			(if rejected-methods
			    (if (rest rejected-methods)
				(plan~message "The methods ~{~A, ~}~A cannot be read."
					       (butlast rejected-methods) (first (last rejected-methods)))
			      (plan~message "The method ~A cannot be read."
					     (first rejected-methods)))
			  (if (rest failed-given-methods)
			      (plan~message "The methods ~{~A, ~}~A are not taken into account.~% They failed to close the node ~A."
					     (butlast failed-given-methods) (first (last failed-given-methods))
					     (keim~name node))
			    (when failed-given-methods
			      (plan~message "The method ~A is not taken into account.~% It failed to close the node ~A."
					     (first failed-given-methods) (keim~name node))))))
		      ;;; LC: Bei dem Vorschlag von methods, wird z.B. folgendes erwartet:
		      ;; [meth1 (meth2 [param1 param2]) ..] 
		      ;;; Try to close NODE-TASK using the selected methods first:
		      (multiple-value-bind (root-taskp new-agenda)
			  (plan=do-step pds agenda :task node-task
					:first-methods finally-taken-methods
					:rest-methods (plan=set-difference node-methods finally-taken-methods))
			(values root-taskp new-agenda))
		      )))
	      ;;; The user does not want to select methods
	      (multiple-value-bind (root-taskp new-agenda)
		  (plan=do-step pds agenda :task node-task)
		(values root-taskp new-agenda)))
	  ;;; No associated open task for NODE on the agenda:
	  (values nil agenda)))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))
 


(defun plan~next-pseudo-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated pseudo task for NODE, then this task is considered in"
		    "the next planning step.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (agenda~pseudo-goal-p tk)
								  (eq (agenda~task-node tk) node))))))
	(if node-task
	    (multiple-value-bind (root-taskp new-agenda)
		(plan=do-step pds agenda :task node-task)
	      (values root-taskp new-agenda))
	  (values nil agenda)))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~block-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated non-pseudo task for NODE, then this task is blocked.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								  (eq (agenda~task-node tk) node))))))
	(when node-task
	  (setf (agenda~task-blocked-p node-task) T))
	(values nil agenda))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~block-pseudo-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated pseudo task for NODE, then this task is blocked.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (agenda~pseudo-goal-p tk)
								  (eq (agenda~task-node tk) node))))))
	(when node-task
	  (setf (agenda~task-blocked-p node-task) T))
	(values nil agenda))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~unblock-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated non-pseudo task for NODE, then this task is unblocked.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								  (eq (agenda~task-node tk) node))))))
	(when node-task
	  (setf (agenda~task-blocked-p node-task) nil))
	(values nil agenda))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~unblock-pseudo-task (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated pseudo task for NODE, then this task is unblocked.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (agenda~pseudo-goal-p tk)
								  (eq (agenda~task-node tk) node))))))
	(when node-task
	  (setf (agenda~task-blocked-p node-task) nil))
	(values nil agenda))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

(defun plan~open-node (node select-methods-p &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   )
	   (effect  )
	   (value   ))
  ;;; similar to plan~next-task
  (if node
      (multiple-value-bind (root-taskp new-agenda)
	  (if (pdsn~open-node-p node)
	      (values nil agenda)
	    (oc=open node))
	(let ((node-task (agenda~get-first-task new-agenda
						#'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								    (eq (agenda~task-node tk) node))))))
	  (if node-task
	      (if select-methods-p
		  ;;; The user wants to select some methods:
		  (let* ((node-failed-methods (pdsn~failed-methods node))
			 (node-methods (if (pdsn~alternative-methods node)
					   (pdsn~alternative-methods node)
					 (plan=set-difference (if (agenda~goal-p node-task) 
							     meth*planning-methods
							   meth*MOReasoning-methods)
							 node-failed-methods))))
		    ;;; Send LOUI a list of methods from which some methods has to be selected:
		    (socket~write "choose-query")
		    (parse~plan-methods (mapcar #'keim~name node-methods))
		    ;;; Wait for the LOUI message which corresponds to the selected methods:
		    (let* ((selected-meths (read-from-string (socket~read)))
			   ;;; (0 3 5) Positionen der ausgewaehlen Methoden:
			   (taken-methods (mapcar #'(lambda (n) (nth n node-methods))
						  selected-meths)))
		      ;;; Try to close NODE-TASK using the selected methods first:
		      (multiple-value-bind (root-taskp new-agenda)
			  (plan=do-step pds new-agenda :task node-task
					:first-methods taken-methods
					:rest-methods (plan=set-difference node-methods taken-methods))
			(values root-taskp new-agenda))))
		;;; The user does not want to select methods
		(multiple-value-bind (root-taskp new-agenda)
		    (plan=do-step pds new-agenda :task node-task)
		  (values root-taskp new-agenda)))
	    ;;; No associated open task for NODE on the agenda, i.e. NODE was perhaps closed by
	    ;; a forward method and is now deleted:
	    (values root-taskp new-agenda))))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))


(defun plan~delete-node (node &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   )
	   (effect  )
	   (value   ))
  ;;; similar to back-task
  (if node
      (oc=delete node pds)
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))

;;;; Planning functionalities:

(defun plan~initialize-agenda (&optional (pds omega*current-proof-plan))
  (declare (edited  "25-MAR-1998")
	   (authors Lassaad)
	   (input   "A pds created by loading a problem.")
	   (effect  "None.")
	   (value   "The created agenda containing a goal task for the"
		    "theorem node of PDS, i.e., the sole open node of PDS."))
  (let ((agenda (plan=prepare-pds! pds)))
    (when agenda (setf (pds~agenda pds) agenda))))

	
;;; A planning step consists of a trivial phase and a reduction phase:
;; - In the trivial phase we try to apply some methods to restrict
;; the open nodes (task-restricting-methods: Same and ForallE*). We call
;; these methods application-methods and they are applied to new open nodes.
;; In this phase we try also to apply the splitting-method AndE to new
;; hypotheses and to nodes closed by forward methods. After every application
;; of the splitting-method, we try to apply the task-restricting-methods to
;; the open nodes having the splitted conjunction as a support.
;; - In the reduction phase, we try to apply reducing methods, i.e., backward
;; methods and forward methods which make use of an open node supports to
;; deduce a new support. These reduction-methods are forallI*, andI, impI, equivI,
;; and assertion application methods and some domain- or strategy- specific
;; methods:

(defun plan=restrict-tasks4 (new-opens new-supps agenda pds)
  (declare (edited  "21-APR-1999" )
	   (authors Lassaad)
	   (input   "A list of open nodes, a list of closed nodes, an agenda, and a PDS."
		    "The two first inputs are obtained by a method application: NEW-OPENS"
		    "are subgoals of this method, and NEW-SUPPS are new closed nodes"
		    "added by the method as supports.")
	   (effect  "Eventually closes some open nodes in the PDS and splits"
		    "the conjunctions within the NEW-SUPPS.")
	   (value   "A pair: the new agenda and a list of agenda tasks."))
  (let ((new-agenda agenda)
	(goal-schema-tasks)
	(remaining-opens))
    ;; 1: Consider the agenda tasks whose formulas dont contain meta-variables.
    ;; Try to restrict these tasks by applying TASK-RESTRICTING-METHODS using
    ;; the supports belonging to NEW-SUPPS:
    (when new-supps
      (let ((agenda-tasks (pds~agenda-goals-with-supports-in agenda new-supps pds)
			  ;; Returns a list of pairs (task restricted-supps)
			  ))
	(dolist (task&supps agenda-tasks)
	  (let ((result-agenda (plan=plan-task (first task&supps) new-agenda pds
					       :context "TASK-RESTRICTION"
					       :supports (second task&supps)
					       :methods meth*restricting-methods)))
	    (when result-agenda
	      (setq new-agenda result-agenda))))))
    
    ;; 2: Create for each new open node of type pdsn+schematic-node a goal schema task
    ;; and try to restrict each new open node of type pdsn+node by using TASK-RESTRICTING-METHODS.
    (dolist (open-node new-opens)
      (cond ((pdsn~schematic-p open-node)
	     (setq goal-schema-tasks
		   (cons (agenda~create-goal-schema open-node)
			 goal-schema-tasks)))
	    (T
	     (multiple-value-bind (mmatching)
		 (plan=apply-first-method pds open-node (node~formula open-node)
					  (pds~node-supports open-node pds) nil nil 
					  meth*restricting-methods)
	       (unless mmatching
		 (setq remaining-opens (cons open-node remaining-opens)))))))

    ;; 4: Try to normalize the new supports by applying SUPP-NORMALIZING-METHODS
    ;; and try simultenously to restrict some open nodes. Applying a normalization
    ;; method delivers new supports and their supported nodes. These supported
    ;; nodes are considered to be closed by applying TASK-RESTRICTING-METHODS using
    ;; the new delivered supports. When a supported node can be closed, then this can
    ;; either belong to the REMAINING-OPENS or its associated task belongs to the
    ;; agenda. This process should deliver the elements of REMAINING-OPENS which 
    ;; could not be closed:
    (when new-supps
      (multiple-value-bind (an-agenda opens)
	  (plan=normalize-supports new-supps remaining-opens new-agenda pds)
	(setq new-agenda an-agenda
	      remaining-opens opens)))
    (values new-agenda
	    (append goal-schema-tasks
		    (mapcar #'agenda~create-goal remaining-opens)))
    ))


;; Main extension: we have to consider the schematic supports
(defun plan=restrict-tasks6 (previously-bound-tasks newly-bound-tasks new-opens new-supps agenda pds)
  (declare (edited  "21-APR-1999" )
	   (authors Lassaad)
	   (input   "Two lists of tasks whose formulas dont contain meta-variables: (PREVIOUSLY BOUND"
		    "TASKS did not contain meta-variables before the current planning step, for them"
		    "we consider only the NEW-SUPPS and their schematic supports. NEWLY BOUND TASKS"
		    "become fully instantiated due to this planning step, all their supports are"
		    "relevant.), a list of open nodes, a list of closed nodes, an agenda, and a PDS."
		    "The third input and the fourth input are obtained by a method application:"
		    "NEW-OPENS are subgoals of this method, and NEW-SUPPS are new closed nodes added"
		    "by the method as supports.")
	   (effect  "Eventually closes some open nodes in the PDS and splits the conjunctions within"
		    "the NEW-SUPPS and the schematic supports.")
	   (value   "A pair: the new agenda and a list of agenda tasks."))
  (let ((new-agenda agenda)
	goal-schema-tasks 
	remaining-opens
	old-schematic-supps)
    ;; 1: Try to restrict tasks from PREVIOUSLY-BOUND-TASKS with NEW-SUPPS and with OLD-SCHEMATIC-SUPPS:
    (dolist (pb-task previously-bound-tasks)
      (let* ((pb-task-supps (pds~node-supports (agenda~task-node pb-task) pds))
	     (relevant-supps (remove-if-not #'(lambda (node) (or (find node new-supps)
								 (pdsn~schematic-p node)))
					    pb-task-supps)))
	(when relevant-supps
	  (let ((result-agenda (plan=plan-task pb-task new-agenda pds
					       :context "TASK-RESTRICTION"
					       :supports relevant-supps
					       :methods meth*restricting-methods)))
	    (when result-agenda (setq new-agenda result-agenda))
	    (setq old-schematic-supps (union old-schematic-supps
					     (remove-if-not #'pdsn~schematic-p relevant-supps)))))))
    
    ;; 2: Try to restrict tasks from NEWLY-BOUND-TASKS with their supports:
    (dolist (nb-task newly-bound-tasks)
      (let ((relevant-supps (pds~node-supports (agenda~task-node nb-task) pds)))
	(when relevant-supps
	  (let ((result-agenda (plan=plan-task nb-task new-agenda pds
					       :context "TASK-RESTRICTION"
					       :supports relevant-supps
					       :methods meth*restricting-methods)))
	    (when result-agenda (setq new-agenda result-agenda))
	    (setq old-schematic-supps (union old-schematic-supps
					     (remove-if-not #'pdsn~schematic-p relevant-supps)))))))

    ;; 3: Create for each new open node of type pdsn+schematic-node a goal schema task
    ;; and try to restrict each new open node of type pdsn+node by using TASK-RESTRICTING-METHODS.
    (dolist (open-node new-opens)
      (cond ((pdsn~schematic-p open-node)
	     (setq goal-schema-tasks
		   (cons (agenda~create-goal-schema open-node)
			 goal-schema-tasks)))
	    (T
	     (multiple-value-bind (mmatching)
		 (plan=apply-first-method pds open-node (node~formula open-node)
					  (pds~node-supports open-node pds) nil nil 
					  meth*restricting-methods)
	       (unless mmatching
		 (setq remaining-opens (cons open-node remaining-opens)))))))

    ;; 4: Try to normalize the new supports by applying SUPP-NORMALIZING-METHODS
    ;; and try simultenously to restrict some open nodes. Applying a normalization
    ;; method delivers new supports and their supported nodes. These supported
    ;; nodes are considered to be closed by applying TASK-RESTRICTING-METHODS using
    ;; the new delivered supports. When a supported node can be closed, then this can
    ;; either belong to the REMAINING-OPENS or its associated task belongs to the
    ;; agenda. This process should deliver the elements of REMAINING-OPENS which 
    ;; could not be closed:
    (when (or new-supps old-schematic-supps)
      (multiple-value-bind (an-agenda opens)
	  (plan=normalize-supports (remove-duplicates (union new-supps old-schematic-supps))
				   remaining-opens new-agenda pds)
	(setq new-agenda an-agenda
	      remaining-opens opens)))
    (values new-agenda
	    (append goal-schema-tasks
		    (mapcar #'agenda~create-goal remaining-opens)))
    ))

(defun plan=restrict-tasks (new-opens new-supps new-bound-tasks agenda pds)
  (declare (edited  "26-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of open nodes, a list of closed nodes, a list of"
		    "bound AGENDA+GOAL-SCHEMAs, an agenda, and a PDS. The three"
		    "first inputs are obtained by a method application: NEW-OPENS"
		    "are subgoals of this method, NEW-SUPPS are new closed nodes"
		    "added by the method as supports, and NEW-BOUND-TASKS are"
		    "schematic goals whose formulas become closed because of the"
		    "constraints engendered by this method.")
	   (effect  "Eventually closes some open nodes in the PDS and splits"
		    "the conjunctions within the NEW-SUPPS.")
	   (value   "A pair: the new agenda and a list of agenda tasks."))
  (let ((new-agenda agenda)
	(goal-schema-tasks)
	(remaining-opens))
    ;;; 1: Consider the agenda tasks whose formulas dont contain meta-variables
    ;; and which dont belong to NEW-BOUND-TASKS. Try to restrict these tasks
    ;; by applying TASK-RESTRICTING-METHODS using the supports belonging to
    ;; NEW-SUPPS:
    (when new-supps
      (let ((agenda-tasks (pds~agenda-goals-with-supports-in agenda new-supps pds new-bound-tasks)
			;;; Returns a list of pairs (task restricted-supps)
			;; where TASK does not occur in NEW-BOUND-TASKS
			  ))
	(dolist (task&supps agenda-tasks)
	  (let ((result-agenda (plan=plan-task (first task&supps) new-agenda pds
					       :context "TASK-RESTRICTION"
					       :supports (second task&supps)
					       :methods meth*restricting-methods)))
	    (when result-agenda
	      (setq new-agenda result-agenda))))))
    ;;; 2: Try to restrict each new open node whose formula does not contain
    ;; meta-variables by applying TASK-RESTRICTING-METHODS. For each new open 
    ;; node with schematic formula, a goal-schema task is created.
    (dolist (open-node new-opens)
      (if (agenda~schematic-formula-p (node~formula open-node))
	  (setq goal-schema-tasks
		(cons (agenda~create-goal-schema open-node)
		      goal-schema-tasks))
	(multiple-value-bind (mmatching)
	    (plan=apply-first-method pds open-node (node~formula open-node)
				     (pds~node-supports open-node pds) nil nil 
				     meth*restricting-methods)
	  (unless mmatching
	    (setq remaining-opens (cons open-node remaining-opens))))))
    ;;; 3: Try to restrict each goal-schema task whose formula becomes closed
    ;; by the last method, i.e., each element in NEW-BOUND-TASKS, by applying
    ;; TASK-RESTRICTING-METHODS:
    (dolist (agenda-task new-bound-tasks)
      (let ((result-agenda (plan=plan-task agenda-task new-agenda pds :context "TASK-RESTRICTION"
					   :methods meth*restricting-methods)))
	(when result-agenda
	  ;;; The new bound task could be closed by a task restricting method:
	  ;; We have to reset NEW-AGENDA and
	  ;; adding the corresponding planning step to the steps of the current constraint pool,
	  ;; because the instantiation of these AGENDA-TASK depends on the last changement in
	  ;; the constraint pool
	  (setq new-agenda result-agenda)
	  )))
;	  (setf (pds~cstrpool-plansteps (pds~constraint-pool pds))
;                (cons (pdsj~own-reason (node~justification (agenda~task-node agenda-task)))
;                      (pds~cstrpool-plansteps (pds~constraint-pool pds)))))))
    ;;; 4: Try to normalize the new supports by applying SUPP-NORMALIZING-METHODS
    ;; and try simultenously to restrict some open nodes. Applying a normalization
    ;; method delivers new supports and their supported nodes. These supported
    ;; nodes are considered to be closed by applying TASK-RESTRICTING-METHODS using
    ;; the new delivered supports. When a supported node can be closed, then this can
    ;; either belong to the REMAINING-OPENS or its associated task belongs to the
    ;; agenda. This process should deliver the elements of REMAINING-OPENS which 
    ;; could not be closed:
    (when new-supps
      (multiple-value-bind (an-agenda opens)
	  (plan=normalize-supports new-supps remaining-opens new-agenda pds)
	(setq new-agenda an-agenda
	      remaining-opens opens)))
    (values new-agenda
	    (append goal-schema-tasks
		    (mapcar #'agenda~create-goal remaining-opens)))
    ))

(defun plan=normalize-supports (new-supps open-nodes agenda pds)
  (declare (edited  "27-MAR-1998")
	   (authors Lassaad)
	   (input   "A non-empty list of support nodes, a list of open nodes,"
		    "an agenda, and a pds. For each element in OPEN-NODES there"
		    "is no associated task on AGENDA.")
	   (effect  "Applies normalizing methods, i.e., AndE, to the elements"
		    "of NEW-SUPPS and eventually restricts some open nodes in PDS.")
	   (value   "A pair: a new agenda and the list of remaining open nodes in"
		    "OPEN-NODES."))
  ;;; This function should normalize the elements of NEW-SUPPS and simultenously restrict
  ;; some open nodes which either belong to OPEN-NODES or are associated tasks on the
  ;; agenda. It should then return the elements of OPEN-NODES which finally still remains open:
  (let ((new-supp1 (first new-supps))
	(pds-opens (pds~open-nodes pds)))
    (if (find-if #'(lambda (open) (find new-supp1 (pds~node-supports open pds)))
		 pds-opens)
	;;; NEW-SUPP1 supported at least one open node in PDS, it should then be normalized:
	(multiple-value-bind (mmatching new-opens new-supps2 rest-mmatchings rest-methods supported-nodes)
	    (plan=apply-first-method pds nil nil (list new-supp1) nil nil meth*normalizing-methods
				     :extra-return "PREMS-SUPPORTED-NODES")
	  (declare (ignore new-opens rest-mmatchings rest-methods))
	  (if mmatching
	      ;;; Which means new supports are generated by the normalization of the first
	      ;; element in NEW-SUPPS. Try to restrict some open nodes using these supports
	      ;; and try to normalize these supports recursively:
	      (let ((remaining-open-nodes open-nodes)
		    (new-agenda agenda))
		(dolist (supported-node supported-nodes)
		  (if (find supported-node remaining-open-nodes)
		      ;;; When this node can be closed by a task-restricting method, it is
		      ;; then removed from the remaining open nodes:
		      (multiple-value-bind (a-mmatching)
			  (plan=apply-first-method pds supported-node (node~formula supported-node)
						   new-supps2 nil nil meth*restricting-methods)
			(when a-mmatching
			  (setq remaining-open-nodes (remove supported-node remaining-open-nodes))))
	            ;;; The supported node must have an associated node on AGENDA, when this
		    ;; is a goal or a schematic goal without meta-variables, then we try to
		    ;; solve it by applying a task-restricting method:
		    (let ((agenda-task (agenda~get-first-task new-agenda #'(lambda (task)
									     (eq (agenda~task-node task)
										 supported-node)))))
		      (when (or (agenda~goal-p agenda-task)
				(and (agenda~goal-schema-p agenda-task)
				     (not (agenda~goal-schematic-p agenda-task))))
			(let ((result-agenda (plan=plan-task agenda-task new-agenda pds
							     :context "TASK-RESTRICTION"
							     :supports new-supps2
							     :methods meth*restricting-methods)))
			  (when result-agenda (setq new-agenda result-agenda)))))))
		(plan=normalize-supports (append (rest new-supps) new-supps2)
					 remaining-open-nodes new-agenda pds))
            ;;; The first element in NEW-SUPPS cannot be further normalized, then consider the
	    ;; rest supports. When this is NIL then return the list of OPEN-NODES:
	    (if (rest new-supps)
		(plan=normalize-supports (rest new-supps) open-nodes agenda pds)
	      (values agenda open-nodes))))
      ;;; NEW-SUPP1 does not support any node in PDS (It was unsponsored by a method action). It
      ;; may not be normalized:
      (if (rest new-supps)
	  (plan=normalize-supports (rest new-supps) open-nodes agenda pds)
	(values agenda open-nodes)))
    ))

;;LC: A similar function must be given in the tactic system
(defun plan=actual-outline-pattern (node &optional (pds omega*current-proof-plan)
					 (just (node~justification node)) undone-steps)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "A node, a PDS, and the node current justification.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and maybe other nodes, a list of"
		    "the nodes that are justified using this method."))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (prems (just~premises just))
	 (concs-l (- (length outln-pat) (length prems))))
    (if (> concs-l 1)
	;;More than one conclusion:
	(plan=adapt-outline-pattern outln-pat node (- concs-l 1) pds undone-steps)
      (values outln-pat (list node)))
    ))

;;LC: A similar function must be given in the tactic system
(defun plan=adapt-outline-pattern (pattern conc concs-l pds &optional undone-steps)
  (declare (edited  "27-JUN-1997")
	   (authors Lassaad)
	   (input   "An outline-pattern, a node, a positive integer, and a PDS:"
		    "CONC is justified by the same method application together"
		    "with CONCS-L other nodes in PDS.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (if (zerop concs-l)
      (if conc
	  (values pattern (list conc))
	(values pattern nil))
    (let ((first-pat (first pattern)))
      (if (or (infer~existent-pattern-p first-pat)
	      (infer~nonexistent-pattern-p first-pat))
	  ;;;FIRST-PAT is the outline of CONC:
	  (multiple-value-bind (patt concs)
	      (plan=adapt-outline-pattern (rest pattern) nil concs-l pds undone-steps)
	    (values (cons first-pat patt)
		    (cons conc concs)))
	;;;FIRST-PAT is a label of another conclusion:
	(multiple-value-bind (patt concs)
	    (plan=adapt-outline-pattern (rest pattern) conc (- concs-l 1) pds undone-steps)
	  (let* ((node (or (pds~label2node first-pat pds)
			   (let ((assoc-stp (find-if #'(lambda (stp)
							 (string-equal first-pat
								       (keim~name (pdsc~an-node stp))))
						     undone-steps)))
			     (when assoc-stp (pdsc~an-node assoc-stp)))))
		 (node-just (node~justification node))
		 (node-pat (pdsj~conclusion-outline node-just)))
	    (values (cons node-pat patt)
		    (cons node concs))))))
    ))


;;; This function tries to apply the first possible method-matching. It returns a tuple
;; <MMATCHING NEW-OPENS NEW-SUPPS REST-MMATCHINGS REST-METHODS>, these are useful for further
;; restricting tasks, normalizing supports and storing backtacking alternatives.
(defun plan=apply-first-method (pds open-node node-formula supports mmatchings meth&ctrls methods &key
				    ((:task task)) ((:invoke-cri invoke-cri)) ((:agenda agenda))
				    ((:extra-return extra-return)))
  (declare (edited  "28-MAR-1998")
	   (authors Lassaad)
	   (input   "A pds, an open node (or NIL), its current formula (or NIL), a list of support nodes, a"
		    "list of method matchings, a list of methods and methods with control informations, e.g.,"
		    "parameters and support nodes, to be considered after the MMATCHINGS, a list of methods"
		    "to be considered after METH&CTRLS, and key words: :TASK for an agenda task, :INVOKE-CRI"
		    "for whether to invoke the CRI for sorting the given methods, :AGENDA for an agenda,"
		    "which is useful while invoking the CRI for sorting the rest methods, and a key :EXTRA-RETURN"
		    "which specifies what should be additionally returned.")
	   (effect  "Applies the first applicable method to OPEN-NODE.")
	   (value   "A tuple: applied method matching, new open nodes, new closed nodes which correspond to"
		    "new supports, remaining method matchings, remaining methods, and some extra-returns"
		    "according to the key EXTRA-RETURN, when one method is applicable, otherwise NIL."))
  (if mmatchings
      ;;; The mmatchings are first tried, when INVOKE-CRI is set, we invoke the CRI to reorder/ restrict and
      ;; reject some methods in METHODS, the rejected methods are then, for completness, appended to the
      ;; returned alternative list. 
      (multiple-value-bind (mmatching new-opens new-supps additional-return)
	  (unless (or (plan=applied-critic-mmatching-p (first mmatchings)  open-node)
		      (plan=excluded-mmatching-p (first mmatchings) open-node)
		      (plan=failed-mmatching-p (first mmatchings) open-node))
	    ;;; LC: We check whether MMATCHING1 corresponds to some critic application that is previously
	    ;; considered. In such a case, we reject this alternative.
	    (plan~apply-mmatching task (first mmatchings) pds :extra-return extra-return))
	;;; This function should carry out the method matching and returns it together
	;; with NEW-OPENS, NEW-SUPPS, and possibly ADDITIONAL-RETURN depending on whether
	;; the key word :EXTRA-RETURN is set. NEW-OPENS are new open nodes coming from
	;; add-premises of the applied method. NEW-SUPPS are new closed nodes corresponding
	;; to add-conclusions of the applied methods and new generated hypothesis and/or
	;; supports. When :EXTRA-RETURN is set to "PREMS-SUPPORTED-NODES" then return the
	;; of pds-open nodes supported by the method existent premises.
	(if mmatching
	    (values  mmatching new-opens new-supps (rest mmatchings)
		     (plan=sorted-rest-methods (plan~matched-method mmatching) meth&ctrls methods)
		     additional-return)
	  (plan=apply-first-method pds open-node node-formula supports (rest mmatchings) meth&ctrls methods
				   :task task :invoke-cri invoke-cri :agenda agenda :extra-return extra-return)))
    (if invoke-cri
	;;; Invoking CRI, means you have to sort/filter out the methods in METH&CTRLS and in METHODS:
	(let* ((old-meth&ctrls (remove-if #'meth~p meth&ctrls))
	       (cri-methods (append (remove-if-not #'meth~p meth&ctrls) methods))
	       (cri-method-names (mapcar #'keim~name cri-methods))
;                                 cri-methods)
	       (sorted-methods
		(plan=process-cri-method-list
		 (cri~call cri-method-names :kind 'methods :task task
			   :task-node open-node :task-formula node-formula :agenda agenda :pds pds))))
	  ;(format t "~%Input: ~A Output: ~A~%" cri-methods sorted-methods)
	  (multiple-value-bind (new-meth&ctrls rest-methods)
	      (if (eq sorted-methods cri-methods) (values nil cri-methods)
		(plan=merge-method-alternatives sorted-methods cri-methods
						(remove-if #'meth~p (pdsn~failed-methods open-node))))
	    (plan=apply-first-method pds open-node node-formula supports nil
				     (append old-meth&ctrls new-meth&ctrls) rest-methods
				     :task task :extra-return extra-return)))
      (if meth&ctrls
	  (let* ((meth&ctrl (first meth&ctrls))
		 (method-to-match (if (listp meth&ctrl) (first meth&ctrl) meth&ctrl))
		 (alternatives (if (listp meth&ctrl)
				   (if (third meth&ctrl)
				       ;;; METH&CTRL: <meth, params, supps>:
				       (plan~match-method method-to-match open-node node-formula
							  (third meth&ctrl) pds :parameters (second meth&ctrl))
				     ;;; METH&CTRL: <meth, params>:
				     (plan~match-method method-to-match open-node node-formula supports pds
							:parameters (second meth&ctrl) :invoke-cri t))
			         ;;; METH&CTRL: meth:
				 (plan~match-method method-to-match open-node node-formula supports pds :invoke-cri t))))
	    ;;; LC: critics are checked whether they are soon applied:
	    (setq alternatives (remove-if #'(lambda (mm) (or (plan=applied-critic-mmatching-p mm open-node)
							     (plan=excluded-mmatching-p mm open-node)
							     (plan=failed-mmatching-p mm  open-node)))
					  alternatives))
	    (if alternatives
		(multiple-value-bind (mmatching new-opens new-supps additional-return)
		    (plan~apply-mmatching task (first alternatives) pds :extra-return extra-return)
		  (if mmatching
		      (values mmatching new-opens new-supps (rest alternatives)
			      (plan=sorted-rest-methods method-to-match meth&ctrls methods)
			      additional-return)
		    (plan=apply-first-method pds open-node node-formula supports (rest alternatives)
					     (rest meth&ctrls) methods :task task :extra-return extra-return)))
	      (plan=apply-first-method pds open-node node-formula supports nil (rest meth&ctrls) methods
				       :task task :extra-return extra-return)))
	(when methods
	  (let* ((method-to-match (first methods)))
	    (if (= 0 (meth~rating method-to-match))
		(plan=apply-first-method pds open-node node-formula supports nil nil (rest methods)
				       :task task :extra-return extra-return)
	      (let ((alternatives (plan~match-method method-to-match open-node node-formula supports pds :invoke-cri t)))
	    ;;; LC: critics are checked whether they are soon applied:
	    ;; Remove all critics in the applied-critics of OPEN-NODE
		(setq alternatives (remove-if #'(lambda (mm) (or (plan=applied-critic-mmatching-p mm open-node)
								 (plan=excluded-mmatching-p mm open-node)
								 (plan=failed-mmatching-p mm  open-node)))
					      alternatives))
		(if alternatives
		    (multiple-value-bind (mmatching new-opens new-supps additional-return)
			(plan~apply-mmatching task (first alternatives) pds :extra-return extra-return)
		      (if mmatching
			  (values mmatching new-opens new-supps (rest alternatives)
				  (rest methods) additional-return)
			(plan=apply-first-method pds open-node node-formula supports (rest alternatives)
						 nil (rest methods) :task task :extra-return extra-return)))
		  (plan=apply-first-method pds open-node node-formula supports nil nil (rest methods)
				       :task task :extra-return extra-return))))))))
    ))


(defun plan=applied-critic-mmatching-p (mmatching  node)
  (declare (edited  "11-FEB-1999")
	   (authors Lassaad)
	   (input   "A method matching, and a node.")
	   (effect  "None.")
	   (value   "T, when MMATCHING corresponds to a critic application"
		    "occurring in the applied critic slot of NODE."))
  (labels ((samep (obj1 obj2)
		  (cond ((term~p obj1)
			 (and (term~p obj2) (term~alpha-equal obj1 obj2)))
			((consp obj1)
			 (and (consp obj2) (samep (first obj1) (first obj2))
			      (samep (rest obj1) (rest obj2))))
			((null obj1) (null obj2))
			(T (keim~equal obj1 obj2))))
	   )
    (let ((method (plan~matched-method mmatching)))
      (when (meth~critic-p method)
	(find-if #'(lambda (ca) (and (eq method (first ca))
				     (samep (rest ca) (plan~mmatch-parameters mmatching))))
		 (pdsn~applied-critics node))))
    ))

(defun plan=excluded-mmatching-p (mmatching  node)
  (declare (edited  "11-FEB-1999")
	   (authors Lassaad)
	   (input   "A method matching, and a node.")
	   (effect  "None.")
	   (value   "T, when MMATCHING corresponds to a method application with parameters"
		    "occurring in the failed method slot of NODE."))
  (labels ((samep (obj1 obj2)
		  (cond ((term~p obj1)
			 (and (term~p obj2) (term~alpha-equal obj1 obj2)))
			((consp obj1)
			 (and (consp obj2) (samep (first obj1) (first obj2))
			      (samep (rest obj1) (rest obj2))))
			((null obj1) (null obj2))
			(T (keim~equal obj1 obj2))))
	   )
    (let ((method (plan~matched-method mmatching)))
      (when (meth~parameters method)
	(find-if #'(lambda (pm) (and (consp pm)
				     (eq method (first pm))
				     (samep (rest pm) (plan~mmatch-parameters mmatching))))
		 (pdsn~excluded-methods node))))
    ))

(defun plan=failed-mmatching-p (mmatching  node)
  (declare (edited  "11-FEB-1999")
	   (authors Lassaad)
	   (input   "A method matching, and a node.")
	   (effect  "None.")
	   (value   "T, when MMATCHING corresponds to a method application with parameters"
		    "occurring in the failed method slot of NODE."))
  (labels ((samep (obj1 obj2)
		  (cond ((term~p obj1)
			 (and (term~p obj2) (term~alpha-equal obj1 obj2)))
			((consp obj1)
			 (and (consp obj2) (samep (first obj1) (first obj2))
			      (samep (rest obj1) (rest obj2))))
			((null obj1) (null obj2))
			(T (keim~equal obj1 obj2))))
	   )
    (let ((method (plan~matched-method mmatching)))
      (when (meth~parameters method)
	(find-if #'(lambda (pm) (and (consp pm)
				     (eq method (first pm))
				     (samep (rest pm) (plan~mmatch-parameters mmatching))))
		 (pdsn~failed-methods node))))
    ))


;; OLD:  
;(defun plan=apply-first-method (pds open-node node-formula supports mmatchings meth&ctrls methods &key
;                                    ((:task task)) ((:invoke-cri invoke-cri)) ((:agenda agenda))
;                                    ((:extra-return extra-return)))
;  (declare (edited  "28-MAR-1998")
;           (authors Lassaad)
;           (input   "A pds, an open node (or NIL), its current formula (or NIL), a list of support nodes, a"
;                    "list of method matchings, a list of methods and methods with control informations, e.g.,"
;                    "parameters and support nodes, to be considered after the MMATCHINGS, a list of methods"
;                    "to be considered after METH&CTRLS, and key words: :TASK for an agenda task, :INVOKE-CRI"
;                    "for whether to invoke the CRI for sorting the given methods, :AGENDA for an agenda,"
;                    "which is useful while invoking the CRI for sorting the rest methods, and a key :EXTRA-RETURN"
;                    "which specifies what should be additionally returned.")
;           (effect  "Applies the first applicable method to OPEN-NODE.")
;           (value   "A tuple: applied method matching, new open nodes, new closed nodes which correspond to"
;                    "new supports, remaining method matchings, remaining methods, and some extra-returns"
;                    "according to the key EXTRA-RETURN, when one method is applicable, otherwise NIL."))
;  (if mmatchings
;      ;;; The mmatchings are first tried, when INVOKE-CRI is set, we invoke the CRI to reorder/ restrict and
;      ;; reject some methods in METHODS, the rejected methods are then, for completness, appended to the
;      ;; returned alternative list. 
;      (multiple-value-bind (mmatching new-opens new-supps additional-return)
;          (plan~apply-mmatching task (first mmatchings) pds :extra-return extra-return)
;        ;;; This function should carry out the method matching and returns it together
;        ;; with NEW-OPENS, NEW-SUPPS, and possibly ADDITIONAL-RETURN depending on whether
;        ;; the key word :EXTRA-RETURN is set. NEW-OPENS are new open nodes coming from
;        ;; add-premises of the applied method. NEW-SUPPS are new closed nodes corresponding
;        ;; to add-conclusions of the applied methods and new generated hypothesis and/or
;        ;; supports. When :EXTRA-RETURN is set to "PREMS-SUPPORTED-NODES" then return the
;        ;; of pds-open nodes supported by the method existent premises.
;        (if mmatching
;            (values  mmatching new-opens new-supps (rest mmatchings)
;                     (plan=sorted-rest-methods (plan~matched-method mmatching) meth&ctrls methods)
;                     additional-return)
;          (plan=apply-first-method pds open-node node-formula supports (rest mmatchings) meth&ctrls methods
;                                   :task task :invoke-cri invoke-cri :agenda agenda :extra-return extra-return)))
;    (if (and invoke-cri methods)
;        ;;; Invoking CRI, means consider only the given METHODS, METH&CTRLS are no more considered:
;        (let ((sorted-methods (cri~call methods :kind 'methods :task task :task-node open-node :agenda agenda :pds pds)))
;          (unless sorted-methods
;            (setf (pdsn~alternative-methods open-node) NIL)
;            (return-from plan=apply-first-method))
;          (multiple-value-bind (new-meth&ctrls rest-methods)
;              (if (eq sorted-methods methods) (values nil methods)
;                (progn ;(omega~trace "Intervention of control rules ...")
;                  (plan=merge-method-alternatives sorted-methods methods
;                                                  (remove-if #'meth~p (pdsn~failed-methods open-node)))))
;            (when (or new-meth&ctrls rest-methods)
;              (plan=apply-first-method pds open-node node-formula supports nil new-meth&ctrls rest-methods
;                                       :task task :extra-return extra-return))))
;      (if meth&ctrls
;          (let* ((meth&ctrl (first meth&ctrls))
;                 (alternatives (if (listp meth&ctrl)
;                                   (if (third meth&ctrl)
;                                       ;;; METH&CTRL: <meth, params, supps>:
;                                       (plan~match-method (first meth&ctrl) open-node node-formula
;                                                          (third meth&ctrl) pds :parameters (second meth&ctrl))
;                                     ;;; METH&CTRL: <meth, params>:
;                                     (plan~match-method (first meth&ctrl) open-node node-formula supports pds
;                                                        :parameters (second meth&ctrl) :invoke-cri t))
;                                 ;;; METH&CTRL: meth:
;                                 (plan~match-method meth&ctrl open-node node-formula supports pds :invoke-cri t))))
;            (if alternatives
;                (multiple-value-bind (mmatching new-opens new-supps additional-return)
;                    (plan~apply-mmatching task (first alternatives) pds :extra-return extra-return)
;                  (if mmatching
;                      (values mmatching new-opens new-supps (rest alternatives)
;                              (plan=sorted-rest-methods (plan~matched-method mmatching) meth&ctrls methods)
;                              additional-return)
;                    (plan=apply-first-method pds open-node node-formula supports (rest alternatives)
;                                             (rest meth&ctrls) methods :task task :extra-return extra-return)))
;              (plan=apply-first-method pds open-node node-formula supports nil (rest meth&ctrls) methods
;                                       :task task :extra-return extra-return)))
;        (when methods
;          (let ((alternatives (plan~match-method (first methods) open-node node-formula supports pds :invoke-cri t)))
;            (if alternatives
;                (multiple-value-bind (mmatching new-opens new-supps additional-return)
;                    (plan~apply-mmatching task (first alternatives) pds :extra-return extra-return)
;                  (if mmatching
;                      (values mmatching new-opens new-supps (rest alternatives) (rest methods)
;                              additional-return)
;                    (plan=apply-first-method pds open-node node-formula supports (rest alternatives)
;                                             nil (rest methods) :task task :extra-return extra-return)))
;              (plan=apply-first-method pds open-node node-formula supports nil nil (rest methods)
;                                       :task task :extra-return extra-return))))))
;    ))

  

(defun plan=merge-method-alternatives (method-alternatives methods failed-meth&params)
  (declare (edited  "15-APR-1998")
	   (authors Lassaad)
	   (input   "A list of method alternatives, a list of methods, and a list"
		    "of methods with parameters of the form (METH P1 .. Pn), where"
		    "a method alternative is either a method, or a tuple consisting"
		    "of a method list or a method, a parameter list, and a list of"
		    "support nodes.")
	   (effect  "None.")
	   (value   "A pair:"
		    "- a list of methods and method with control informations,"
		    "as parameters and/or supports, that occur in METHOD-ALTERNATIVES"
		    "and does not occur in FAILED-METH&PARAMS, and "
		    "- a list of the remaining methods in METHODS."))
  (if method-alternatives
      (let ((meth-alter1 (first method-alternatives)))
	(if (listp meth-alter1)
	    ;;; Method(s) with control information
	    (let ((meth-sym (first meth-alter1))
		  (meths1)
		  (param1 (second meth-alter1))
		  (supps1 (third meth-alter1)))
	      (if (or (meth~p meth-sym) (consp meth-sym)) 
		  (setq meths1 meth-sym)
		(if (and meth-sym (symbolp meth-sym))
		    (let ((some-meth (meth~find-method meth-sym)))
		      (if some-meth (setq meths1 some-meth)
			(let ((meths (eval meth-sym))
			      ;;;LC: EVAL soll erstmal bleiben, weil CRI liefert symbole die nicht
			      ;; ausgewertet sind.
			      )
			  (if (and meths (every #'meth~p meths))
			      (setq meths1 meths)
			    (return-from plan=merge-method-alternatives
			      (omega~error ";;;plan=merge-method-alternatives: Unknown method alternative ~A!"
					   meth-sym))))))
		  (return-from plan=merge-method-alternatives
		    (omega~error ";;;plan=merge-method-alternatives: Unknown method alternative ~A!"
				 meth-sym))))
	      (if (listp meths1)
		  ;;; METHS1 is a method list with control infomation:
		  (plan=merge-method-alternatives (append (mapcar #'(lambda (meth)
								      (list meth param1 supps1))
								  meths1)
							  (rest method-alternatives))
						  methods failed-meth&params)
		;;; METHS1 is a method with control information:
		(multiple-value-bind (first-meths)
		    (plan=merge-method-alternatives (rest method-alternatives) methods failed-meth&params)
		  (if param1
	              ;;; with parameters too, then take it only iff does not occur in FAILED-METH&PARAMS:
		      (if (find-if #'(lambda (meth&param)
				       (plan=parametrized-method-eq (cons meths1 param1) meth&param))
				   failed-meth&params)
			  ;;(values first-meths methods)
			  (values first-meths)
			;;(values (cons (list meths1 param1 supps1) first-meths) methods))
			(values (cons (list meths1 param1 supps1) first-meths)))
		    (if supps1
		        ;;; without parameters and with supports, to be taken it must occur in METHODS:
			(if (find meths1 methods)
			    ;;(values (cons (list meths1 nil supps1) first-meths) methods)
			    (values (cons (list meths1 nil supps1) first-meths))
			  ;;(values first-meths methods))
			  (values first-meths))
		      (progn
			(omega~warn ";;;plan=merge-method-alternatives: strange CRI method alternative: ~A!"
				    meth-alter1)
			;;; Treat METH-ALTER1 as the method METHS1, it must be deleted from REST-METHS: 
			(if (find meths1 methods)
			    ;;(values (cons meths1 first-meths) (remove meths1 methods))
			    (values (cons meths1 first-meths))
			  ;;(values first-meths methods))))))))
			  (values first-meths))))))))
	  ;;; METH-ALTER1 is method(s) identifier without control information:
	  (let ((meths1))
	    (if (meth~p meth-alter1)
		(setq meths1 meth-alter1)
	      (if (and meth-alter1 (symbolp meth-alter1))
		  (let ((some-meth (meth~find-method meth-alter1)))
		    (if some-meth (setq meths1 some-meth)
		      (let ((meths (eval meth-alter1))
			    ;;;LC: EVAL soll erstmal bleiben, weil CRI liefert symbole die nicht
			    ;; ausgewertet sind.
			    )
			(if (and meths (every #'meth~p meths))
			    (setq meths1 meths)
			  (return-from plan=merge-method-alternatives
			    (omega~error ";;;plan=merge-method-alternatives: Unknown method alternative ~A!"
					 meth-alter1))))))
		(return-from plan=merge-method-alternatives
		  (omega~error ";;;plan=merge-method-alternatives: Unknown method alternative ~A!"
			       meth-alter1))))
	    (if (listp meths1)
		(plan=merge-method-alternatives (append meths1 (rest method-alternatives))
						methods failed-meth&params)
	      (multiple-value-bind (first-meths)
		  (plan=merge-method-alternatives (rest method-alternatives) methods failed-meth&params)
		(if (find meths1 methods)
		    ;;(values (cons meths1 first-meths) (remove meths1 methods))
		    (values (cons meths1 first-meths))
		  ;;(values first-meths methods)))))))
		  (values first-meths)))))))
    ;;; No more alternative methods:
    ;;(values nil methods)))
    (values nil)))


(defun plan=sorted-rest-methods (method meth&ctrls methods &optional sorted-methods)
  (declare (edited  "16-APR-1998")
	   (authors Lassaad)
	   (input   "A method (successfully applied), a list of methods and methods with"
		    "control informations, a list of methods, and optionally a list of"
		    "methods as a collector.")
	   (effect  "None.")
	   (value   "The list of methods which occur in METH&CTRLS and in METHODS without"
		    "parameters and which are different from METHOD, preserving the order"
		    "in METH&CTRLS."))
  (if meth&ctrls
      (let ((meth&ctrl (first meth&ctrls)))
	(if (listp meth&ctrl)
	    (if (or (find (first meth&ctrl) sorted-methods) (eq (first meth&ctrl) method))
		;;; The method in METH&CTRL is either considered yet or may not be further considered:
		(plan=sorted-rest-methods method (rest meth&ctrls) methods sorted-methods)
	      ;;; The method in METH&CTRL must be inserted at the end of SORTED-METHODS and removed
	      ;; from METHODS:
	      (plan=sorted-rest-methods method (rest meth&ctrls) (remove (first meth&ctrl) methods) 
					(append sorted-methods (list (first meth&ctrl)))))
	  ;;; METH&CTRL must be a method
	  (if (or (find meth&ctrl sorted-methods) (eq meth&ctrl method))
	      ;;; The method METH&CTRL is either considered yet or may not be further considered:
	      (plan=sorted-rest-methods method (rest meth&ctrls) methods sorted-methods)
	    ;;; The method METH&CTRL must be inserted at the end of SORTED-METHODS and removed from
	    ;; METHODS:
	    (plan=sorted-rest-methods method (rest meth&ctrls) (remove meth&ctrl methods)
				      (append sorted-methods (list meth&ctrl))))))
    (append sorted-methods (remove method methods))
    ))



;;; Remarks for Backtracking:
;; - Another slot is needed: alternative-mmatchings
;; - Each pdsn+node has two slots: failed-methods 
;; and alternative-methods. Failed methods contain methods which are applied
;; to this node and then backtracked, because they dont lead to a solution.
;; Alternative-methods are methods and mmatchings which are not yet applied
;; to this node. These slots are changed twice: when the node is considered 
;; to be planned the first time in the context PLANNING, here the alternative
;; methods is set to the planning-methods which are not yet tested together
;; with the method matchings which are not yet carried out. The second time
;; these slots are changed when the associated node is reopened. In such
;; situation, we add the last applied method to failed-methods and consider
;; alternative-methods as the planning-methods for this node. 
;; - Both slots are NIL, means that the associated node is either not yet
;; considered during the planning process or no planning-method is applicable
;; to this node.
;; - Both AGENDA+GOALs and AGENDA+GOAL-SCHEMAs are similarly treated, because
;; the backtracking is chronological wrt. meta-variable instantiations and
;; dependency-directed wrt. non-schematic goals. Thus whenever we backtrack to
;; a AGENDA+GOAL-SCHEMA, the associated formula of this task will have the same
;; meta-variables and therefore the applicability of mor-methods to this task
;; is not changed.

(defun plan=plan-task (task agenda pds &key ((:context context)) ((:supports supports))
			    ((:methods methods)) ((:first-methods first-methods)))
			    
  (declare (edited  "28-MAR-1998")
	   (authors Lassaad)
	   (input   "A (non-pseudo) task, an agenda, a pds, and some key words: :CONTEXT"
		    "specifies in which context this function is called, for instance, the"
		    "context TASK-RESTRICTION means that this task was called to apply"
		    "task-resricting methods to TASK, in such context the CRI may not be"
		    "invoked to eventually restrict the available methods or sort them."
		    ":SUPPORTS, when stated it delivers the support nodes to be considered."
		    ":METHODS, delivers the methods to be considered and :FIRST-METHODS, when"
		    "given, it is considered as the list of alternative methods to be considered"
		    "first. When :FIRST-METHODS is not NIL, then the rest relevant methods"
		    "for TASK must be given too in :METHODS, then this function will try to"
		    "apply the first alternative method and then store the rest of FIRST-METHODS"
		    "and METHODS in the remaining alternative-methods of TASK for eventually"
		    "backtracking. Moreover, in such situation, the CRI may not be invoked."
		    "When :FIRST-METHODS and :METHODS are not specified, then the alternative"
		    "methods are determined by considering the context and the task current"
		    "alternative methods.")
	   (effect  "Changes AGENDA and PDS.")
	   (value   "The resulted AGENDA after planning this task, if this task can be planned,"
		    "otherwise nil."))
  ;;; When there is no way either to solve the given TASK or to apply a method in order
  ;; to contribute to solving this TASK, then this method returns NIL and backtracking
  ;; must be carried out in the caller of this function.
  (let* ((task-node (agenda~task-node task))
	 (node-formula (pds~task-formula task pds))
	 (node-supports (if supports supports
			  (pds~node-supports task-node pds))))
    (if (or (string-equal context "PLANNING") (string-equal context "MOR-PLANNING"))
	(let* ((exist-mmatchings (pdsn~alternative-mmatchings task-node))
	       (node-methods (pdsn~alternative-methods task-node))
	       (rest-methods (if (or first-methods methods) methods
			       (if (or exist-mmatchings node-methods (pdsn~failed-methods task-node))
				   node-methods
				 ;; Either TASK-NODE was not yet planned or no method is applicable to this
				 ;; node. The latter case is possible when TASK-NODE was closed in a planning step
				 ;; which affected the constraint pool, this step is taken back and no other alternative
				 ;; is available to close this node. 
				 (plan=set-difference (if (string-equal context "PLANNING")
							  meth*planning-methods
							meth*MOReasoning-methods)
						      (pdsn~failed-methods task-node))))))
	  (omega~trace "")
	  (omega~trace "Planning task: ~A  ..." task)
	  (setf plan*current-task task) ;;;JZ: fuer Supermethods
	  (multiple-value-bind (mmatching new-opens new-supps rest-mmatchings new-rest-methods)
	      (if first-methods
		  (plan=apply-first-method pds task-node node-formula node-supports exist-mmatchings
					   first-methods
					   rest-methods
					   :invoke-cri T ;;;(not first-methods)
					   :task task) ;;changed jzimmer
		(plan=apply-first-method pds task-node node-formula node-supports exist-mmatchings
					 nil rest-methods :task task :invoke-cri t :agenda agenda))
	    (when mmatching
	      ;;; One method was successfully applied to the TASK-NODE:
	      (let ((applied-method (plan~matched-method mmatching))
		    (resulted-cstrpool (plan~mmatch-cstr-state mmatching))
		    ;;; Remark to the change of cstr+state while applying and expanding methods:
		    ;; It must be dinstinguished between adding new meta-variable bindings and
		    ;; changing the constraint pool. When a method application (expansion) only
		    ;; changes the cstr+constraint of CSTR+STATE and does not add meta-variable
		    ;; bindings, then the current cstr+state gets an empty pds~cstrpool-bindings.
		    ;; This is needed to prevent unneeded updating schematic tasks, namely when
		    ;; the meta-variable bindings is not changed:
		    )
		(if (eq task-node (plan~mmatch-goal mmatching))
		    ;;; TASK was solved by a backward method:
		    ;; 1- Carry out the method actions first, because these can make some
		    ;; changes to the supports of the involved nodes. For instance, by applying
		    ;; a method M we get a new hypothesis (H1 (and a b)) which occurs in a subgoal
		    ;; Li, hence during the application of M H1 is added to the supports of Li.
		    ;; Suppose M has the outline actions [Li (unsponsor H1) (sponsor Lj)] where
		    ;; Lj corresponds to (Lj (H1) a ("AndEL" () (H1))). To prevent that H1 will
		    ;; be splitted into (Ln a) and (Ln+1 b) and these are added as supports to
		    ;; Li, we have to carry out the actions of M before normalizing the new
		    ;; closed nodes and we must prohibit to normalize new closed nodes which does 
		    ;; not support any node.
		    ;; 2- Update the goal-schemas on the agenda but not TASK, because it was carried
		    ;; out, determine hereby the new-bound-tasks on the agenda.
		    ;; 3- Try to restrict some tasks and normalize the new supports.
		    ;; 4- Set the alternative mmatchings of TASK-NODE to REST-MMATCHINGS and
		    ;; alternative methods to NEW-REST-METHODS union the methods eliminated by the CRI.
		    ;; 5- Interpret the matched method orderings using the remaining tasks.
		    ;; 6- Replace TASK by the NEW-TASKS and eventually the associated pseudo-task
		    ;; when the applied method is non-reliable.
		    (cond ((and resulted-cstrpool (pds~cstrpool-bindings resulted-cstrpool)
				(or (null (pds~cstrpool-previous resulted-cstrpool))
				    (not (eq (pds~cstrpool-bindings resulted-cstrpool)
					     (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))))
			   ;; Some meta-variables are bound in this planning step:
			   (multiple-value-bind (previously-clsd-goals newly-clsd-goals)
			       (pds~update-schematic-nodes! pds task new-opens)
			     (plan=execute-outline-actions task-node new-opens mmatching pds)
			     (multiple-value-bind (new-agenda remaining-tasks)
				 (plan=restrict-tasks6 previously-clsd-goals newly-clsd-goals
						       new-opens new-supps agenda pds)
			       (let ((new-tasks (if (meth~non-reliability applied-method)
						    ;; METHOD is non-reliable, we have to consider
						    ;; a new pseudo task for TASK-NODE:
						    (cons (agenda~create-pseudo-goal task-node) remaining-tasks)
						  remaining-tasks)))
				 (setf (pdsn~alternative-mmatchings task-node) rest-mmatchings
				       (pdsn~alternative-methods task-node) new-rest-methods)
				 (multiple-value-bind (first-task orderings)
				     (plan=create-agenda-orderings (meth~outline-orderings applied-method)
								   (plan~mmatch-mmapp mmatching) new-tasks)
				   (agenda~replace-task task first-task (remove first-task new-tasks)
							orderings new-agenda))))))
			  (T ;; No meta-variable is instantiated during this planning step:
			   (plan=execute-outline-actions task-node new-opens mmatching pds)
			   (multiple-value-bind (new-agenda remaining-tasks)
			       (plan=restrict-tasks4 new-opens new-supps agenda pds)
			     (let ((new-tasks (if (meth~non-reliability applied-method)
						  ;; METHOD is non-reliable, we have to consider
						  ;; a new pseudo task for TASK-NODE:
						  (cons (agenda~create-pseudo-goal task-node) remaining-tasks)
						remaining-tasks)))
			       (setf (pdsn~alternative-mmatchings task-node) rest-mmatchings
				     (pdsn~alternative-methods task-node) new-rest-methods)
			       (multiple-value-bind (first-task orderings)
				   (plan=create-agenda-orderings (meth~outline-orderings applied-method)
								 (plan~mmatch-mmapp mmatching) new-tasks)
				 (agenda~replace-task task first-task (remove first-task new-tasks)
						      orderings new-agenda))))))

		  ;;; A forward method was applied to contribute to the solving of TASK which is
		  ;; currently unsolved:
		  ;; 1- Carry out the matched method actions.
		  ;; 2- Update the goal-schemas on the agenda including TASK and determine hereby
		  ;; the new-bound-tasks on the agenda.
		  ;; 3- Try to restrict some tasks and normalize the new supports.
		  ;; 4- Dont change the alternative mmatchings and the alternative method slots
		  ;; of TASK-NODE.
		  ;; 5- Interpret the matched method orderings using the remaining tasks.
		  ;; 6- Insert the NEW-TASKS, including eventually pseudo-tasks when the applied
		  ;; method is non-reliable, into AGENDA:
		  (cond ((and resulted-cstrpool (pds~cstrpool-bindings resulted-cstrpool)
			      (or (null (pds~cstrpool-previous resulted-cstrpool))
				  (not (eq (pds~cstrpool-bindings resulted-cstrpool)
					   (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))))
			 (format T "~%SUBST1: ~A~%SUBST0: ~A~%EQ?: ~A"
				 (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))
				 (pds~cstrpool-bindings resulted-cstrpool)
				 (eq (pds~cstrpool-bindings resulted-cstrpool)
				     (pds~cstrpool-bindings (pds~cstrpool-previous resulted-cstrpool))))
				 
				 
				
			 ;; Some meta-variables are bound:
			 (multiple-value-bind (previously-clsd-goals newly-clsd-goals)
			     (pds~update-schematic-nodes! pds)
			   (plan=execute-outline-actions (find-if-not #'pdsn~hypothesis-p new-supps) new-opens mmatching pds)
			   (multiple-value-bind (new-agenda remaining-tasks)
			       (plan=restrict-tasks6 previously-clsd-goals newly-clsd-goals
						     new-opens new-supps agenda pds)
			     (let ((new-tasks (if (meth~non-reliability applied-method)
						  ;; METHOD is non-reliable, we have to consider
						  ;; a new pseudo task for TASK-NODE:
						  (append (mapcar #'agenda~create-pseudo-goal
								  (remove-if #'pdsn~hypothesis-p new-supps))
							  remaining-tasks)
						remaining-tasks)))
			       (multiple-value-bind (first-task orderings)
				   (plan=create-agenda-orderings (meth~outline-orderings applied-method)
								 (plan~mmatch-mmapp mmatching) new-tasks)
				 (agenda~insert-tasks task first-task new-tasks orderings new-agenda))))))
			(T ;; No meta-variable is instantiated during this planning step:
			 (plan=execute-outline-actions (find-if-not #'pdsn~hypothesis-p new-supps) new-opens mmatching pds)
			 (multiple-value-bind (new-agenda remaining-tasks)
			     (plan=restrict-tasks4 new-opens new-supps agenda pds)
			   (let ((new-tasks (if (meth~non-reliability applied-method)
						;; METHOD is non-reliable, we have to consider
						;; a new pseudo task for TASK-NODE:
						(append (mapcar #'agenda~create-pseudo-goal
								(remove-if #'pdsn~hypothesis-p new-supps))
							remaining-tasks)
					      remaining-tasks)))
			     (multiple-value-bind (first-task orderings)
				 (plan=create-agenda-orderings (meth~outline-orderings applied-method)
							       (plan~mmatch-mmapp mmatching) new-tasks)
			       (agenda~insert-tasks task first-task new-tasks orderings new-agenda))))))
		  )))))
      (if (string-equal context "TASK-RESTRICTION")
	  ;;; In this context the relevant methods are given by the caller of this function and
	  ;; there is no exist-mmatchings:
	  (multiple-value-bind (mmatching new-opens new-supps)
	      (plan=apply-first-method pds task-node node-formula node-supports nil nil methods :task task)
	    (when mmatching
	      ;;; TASK was solved: In this context NEW-OPENS and NEW-SUPPS must be empty and TASK-NODE
	      ;; must be eq to the goal of MMATCHING. The task is then deleted from the agenda which is
	      ;; returned. 
	      (if (and (eq task-node (plan~mmatch-goal mmatching))
		       (null new-opens) (null new-supps))
		  (agenda~replace-task task nil nil nil agenda)
		(omega~error ";;;PLAN=PLAN-TASK: Inkonsistent task-restricting method application!~%"))))
	(omega~error ";;;PLAN=PLAN-TASK: Illegal application context: ~S!~%" context))
      )))




;;; Remarks to backtracking within MOR:
; If an open node with meta-variables was inserted, we have to consider
; this open node as a goal-schema during the hole planning process. The
; information of having meta-variables in this node is important for the
; backtracking process: If we get a lock, i.e., a goal-schema that cannot be closed,
; then we consider the last applied method that affected the constraint
; pool and process as follows: We do as this method was not applied, i.e.
; we consider the previous constraint pool: When the lock can be solved, we
; are done; otherwise we consider the next previous constraint pool and so
; on until we get a constraint pool that allows to solve the lock or we
; reach the method that inserted the lock-node. We try then alternative
; methods.

(defun plan=create-agenda-orderings (meth-orderings mmapp tasks)
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "Method orderings, a binding of the method meta-variables,"
		    "and a list of tasks which can be referenced by the"
		    "METH-ORDERINGS.")
	   (effect  "Signals an error when the method orderings are inconsistent.")
	   (value   "A pair: the first task, and a list of agenda orderings when"
		    "the method orderings are consistent."))
  (when meth-orderings
    (multiple-value-bind (first-task agenda-orderings)
	(plan=create-agenda-orderings (rest meth-orderings) mmapp tasks)
      (let* ((meth-ordering1 (first meth-orderings))
	     (key-word (keim~name meth-ordering1))
	     (meth-nodes (meth~ordering-ordered-nodes meth-ordering1)))
	(cond ((string-equal key-word 'first)
	       (let* ((meth-var (if (meth~node-p (first meth-nodes)) (keim~name (first meth-nodes))
				  (first meth-nodes)))
		      (pds-node (meth~mapp-get-component meth-var mmapp :mapp)))
		 (if pds-node
		     (let ((the-task (find-if #'(lambda (task) (eq (agenda~task-node task) pds-node))
					      tasks)))
		       (if the-task
			   (if (or (null first-task) (eq the-task first-task))
			       (values the-task agenda-orderings)
			     (return-from plan=create-agenda-orderings
			       (omega~error ";;; Specifying two different tasks ~A and ~A to be considered first!"
					    the-task first-task)))
			 (values first-task agenda-orderings)))
		   (return-from plan=create-agenda-orderings
		     (omega~error ";;; No associated pds node for ~A in ~A" (first meth-nodes) mmapp)))))
	      ((string-equal key-word 'before)
	       (let* ((meth-vars (mapcar #'(lambda (mnode) (if (meth~node-p mnode) (keim~name mnode)
							     mnode))
					 meth-nodes))
		      (pds-nodes (meth~mapp-get-component meth-vars mmapp :mapp)))
		 (if (some #'null pds-nodes)
		     (return-from plan=create-agenda-orderings
		       (omega~error ";;; No associated pds nodes for ~A in ~A" meth-nodes mmapp))
		   (let ((the-tasks
			  (mapcar #'(lambda (x)
				      (if (listp x)
					  (remove-if
					   #'null
					   (mapcar #'(lambda (y)
						       (find-if #'(lambda (task)
								    (eq (agenda~task-node task) y))
							       tasks))
						   x))
					(find-if #'(lambda (task) (eq (agenda~task-node task) x))
						 tasks)))
				  pds-nodes)))
		     (if (some #'null the-tasks)
			 (values first-task agenda-orderings)
		       (values first-task
			       (let ((tasks1 (first the-tasks))
				     (tasks2 (second the-tasks)))
				 (cons (agenda~ordering-create
					(if (listp tasks1) tasks1 (list tasks1))
					(if (listp tasks2) tasks2 (list tasks2)))
				       agenda-orderings))))))))
	      (T
	       (return-from plan=create-agenda-orderings
		 (omega~error ";;; Unkown ordering key word ~A!" key-word))))
	))))
	       
		     
	      

;;; plan=prepare-pds!:
;; This function should adapt the pds and the agenda before applying any method to
;; be sure that all supports are normalized, by applying SUPP-NORMALIZING-METHODS,
;; and there is no open node which can directly be closed using one of its supports
;; by TASK-RESTRICTING-METHODS:
;; Because the pds can be constructed interactively by applying ND-rules, tactis and
;; methods. We assume that the application of methods always leads to a standard pds.
;; For tactics and rules there is two possibilities: 1) If we treat tactic and rule
;; application similar to a planning step and involve them in the agenda, then we
;; only once check that the pds is in a standard form namely after loading the problem.
;; We call the function plan=prepare-pds! in plan~initialize-agenda!
;; 2) If we use tactic and rule without taking care that they are applied to and they
;; produce a pds in a standard form, we have then to implement a little bit complex
;; plan=prepare-pds! which make use of the plan-steps order in the pds:
;; Here, we implement the simple version, that of 1):
(defun plan=prepare-pds! (pds)
  (declare (edited  "04-APR-1998")
	   (authors Lassaad)
	   (input   "A pds.")
	   (effect  "Applies SUPP-NORMALIZING-METHODS to the hypothesis of PDS"
		    "and TASK-RESTRICTING-METHODS to the open nodes of PDS before"
		    "creating the task agenda.")
	   (value   "An agenda containing goal tasks for the remaining open nodes"
		    "of PDS."))
  (let ((last-step (pds~last-plan-step pds))
	(pds-agenda (pds~agenda pds)))
    (if last-step
	;;; At least one proof step in PDS: 
	(let* ((step-just (pdsc~an-just last-step))
	       (stp-inference (just~method step-just)))
	  (if (infer~method-p stp-inference)
	      ;;; The last proof step corresponds to a method application, return pds-agenda
	      pds-agenda
	    ;;; The last proof step corresponds to the application of a tactic or a bbox, consider 
	    ;; all last steps occurring after a method step and replay them:
	    (let ((last-steps (list last-step)))
	      (loop
	       (let ((previous-step (pdsj~predecessor step-just)))
		 (if previous-step
		     (let ((pstep-just (pdsc~an-just previous-step)))
		       (if (infer~method-p (just~method pstep-just))
			   (return)
			 (setq last-step previous-step
			       step-just pstep-just
			       last-steps (cons previous-step last-steps))))
		   (return))))
	      ;;; Replay all these last steps by applying task restricting support normalizing methods:
	      (let ((conc-collector))
		(dolist (step last-steps (pds~agenda pds))
		  (let* ((node (pdsc~an-node step))
			 (just (pdsc~an-just step))
			 (conc-outline (pdsj~conclusion-outline just))
			 (prems (just~premises just))
			 (outline-pat (pdsj~outline-pattern just)))
		    (if (> (length outline-pat) (length (cons node (append conc-collector prems))))
			(setq conc-collector
			      (cons (when (infer~nonexistent-pattern-p conc-outline)
				      node)
				    conc-collector))
		      (let ((open-prems (remove-if-not #'pdsn~open-node-p prems)))
			;;; Try to close the open premises with task restricting methods:
			(when open-prems
			  (dolist (open open-prems)
			    (let ((new-agenda
				   (plan=plan-task
				    (agenda~get-first-task (pds~agenda pds)
							   #'(lambda (tk) (eq (agenda~task-node tk) open)))
				    (pds~agenda pds) pds :context "TASK-RESTRICTION"
				    :methods meth*restricting-methods)))
			      (when new-agenda (setf (pds~agenda pds) new-agenda)))))
			;;; Try to normalize the resulted supports of this plan-step:
			(let ((supp-collector (remove-if #'null conc-collector))
			      (node-hyps (pdsn~hyps node)))
			  (dolist (prem prems)
			    (setq supp-collector
				  (union supp-collector
					 (plan=set-difference (pdsn~hyps prem) node-hyps))))
			  (when supp-collector
			    (multiple-value-bind (new-agenda)
				(plan=normalize-supports supp-collector nil (pds~agenda pds) pds)
			      (setf (pds~agenda pds) new-agenda))))
			;;; Reset CONC-COLLECTOR
			(setq conc-collector nil)))))))))
      ;;; No proof step in PDS:
      (multiple-value-bind (new-agenda)
	  ;(plan=normalize-supports (pds~support-nodes pds) nil (pds~agenda pds) pds)
	  ;;LC: A generalization for the case when THM can be directly proven from the problem hyps
	  ;; by task-restricting methods:
	  (plan=restrict-tasks6 (pds~first-tasks! pds) NIL NIL (pds~support-nodes pds) (pds~agenda pds) pds)
	(setf (pds~agenda pds) new-agenda)))
    ))


;;; Backtracking algorithm:
;; We distinguish two cases: backtracking goal-tasks (GT) and schematic-tasks (ST).
;; A pseudo-task is considered in GT when its formula does not contain any meta-variable
;; and in ST otherwise:
;; [GT case]: Its associated node TN cannot be closed, then take back the plan steps which
;; depend on this node (St1, ... Stn). These steps are backtracked as follows:
;; 1) Sti is closed by a method:
;;         - If node(Sti) has mmatchings, then make this node open.
;;         - Otherwise, if node(Sti) has alternative-methods, then:
;;           A: take method(Sti) into failed-methods; and
;;           B: make node(Sti) open.
;;         - No alternative methods is available, then collect Sti to be recursively
;;           backtracked!
;; 2) Sti is closed by a tactic, a rule, a bbox: then make node(Sti) open.
;; 3) The open nodes resulted in 1) and those resulted in 2) are collected;
;; You have to apply the task-restricting-methods and supp-normalizing-methods to the
;; open nodes resulted in 2) and to their supports; Thereafter you have to create
;; new tasks for the all the resulted open nodes and insert them into AGENDA instead
;; of TASK.
;; [ST case]: Consider the last steps which modified the constraint pool (St1 ... Stn)
;; which are all closed by methods. These plan steps could affect the agenda by the
;; application or by the expansion of some method (When the expansion of a method affects
;; the cstr-pool then we register a pair list (N.M) and not the plan step represented by
;; an ac=node associated to the expanded justification of node N).
;; 1) Sti is a (N.M) then unexpand the associated justification, eventually create a
;; new pseudo task that has to be inserted at the same niveau as TASK but must be
;; considered after this (TASK < pseudo-T).
;; ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION:
;; When really the expansion of methods affect the constraint pool, then we have to
;; renounce the heuristic of considering pseudo-tasks before schematic tasks, because
;; this would be a source of loops.
;; ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION ATTENTION:
;; After unexpanding the associated node, we have then to reset the constraint pool
;; to the previous one, reset the current formulas of all current schematic-goals.
;; 2) Sti is a plan step, i.e., it corresponds to a method application, we have then
;; in addition of resetting the constraint pool process as in 1) and then 3) of GT-case,
;; without deleting any task in the agenda
;; [in the two cases]: When taking a plan step back in 1) it is possible to have an
;; associated pseudo task on the agenda, this must be deleted too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [GT-case]: - deleted-steps := pds~delete-node!(node(GT),pds)
;; - Consider each St in deleted-steps with node(St) is in open-nodes(pds):
;; A: meth(St) is a planning method, then 1)
;; B: 3) where, before generating tasks for remaining open nodes which were closed by
;; tactics, try to close them by task restricting methods.
;; [ST-case]: - last steps updating the cstr-pool
;; A: (N.M) delay this case
;; B: St then open the node and
;;;; WEITER:
;; When an open node that corresponds to the pds theorem must be deleted during the
;; backtracking process (i.e. the node associated to the task to be unlocked, for instance,
;; can be the pds theorem). This node may not be deleted, thus we have to signal a
;; message that the planner could not find a solution with the current settings and
;; return the agenda containing this node.
;;;;;;;;;;;
;; Ersetzen von agenda~update by pds~reset-schematic-nodes!
;; Extension with EXP-STP: The principle of taking EXP-STPs back:
;;  - EXP-STP changed the constraint state --> take back the applied method which led to this
;;    expansion ---> We obtain a goal (schema) 
;;  - EXP-STP did not change the constraint state --> take back only the expansion ---> We obtain
;;    a pseudo goal
(defun plan=backtrack-task! (task agenda pds)
  (declare (edited  "17-APR-1998")
	   (authors Lassaad)
	   (input   "A task, an agenda, and a pds, where task cannot be solved.")
	   (effect  "Modifies AGENDA and PDS, in order to go on in the planning"
		    "process.")
	   (value   "A pair:"
		    " - the pds root task to signal that its associated node had to"
		    "be deleted in this backtacking step, and NIL otherwise."
		    " - the resulted agenda."))
  (omega~trace "Backtracking task ~A ..." task)
;  (print "Vor Backtacking:")
;  (omega~message (oc=show-the-agenda agenda))
;  (format T "~%All steps before Backtracking:~%~A" (pds~plan-steps pds))
  
  ;; LC: NOTE eigentlich muesste das alles in PDS.LISP passieren, beim Loeschen und
  ;; oeffnen von Knoten muss man entsprechend die CSTR-POOL anpassen.
  ;;
  ;; When some of the UNDONE-STEPS affected the constraint pool, we need to take back not
  ;; only these steps but all their successors too. The pointer to the successor of such a
  ;; node is set to NIL by pds~delete-node!. An association of constraint steps to their
  ;; successors  CSTRSTPS2SUCCS must be precomputed and delivered to the function plan=consistent-opens!
  (let ((task-node (agenda~task-node task))
	task-formula
	root-task
	(cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	undone-steps
	redo-steps
	keep-taskp
	passed-nodes
	expansion-stpp)
    (cond ((agenda~goal-p task)
	   ;; TASK could not be closed, its associated node must be deleted and with it the
	   ;; plan steps which depend on it are undone. When TASK-NODE corresponds to the
	   ;; PDS root node, then return its associated node to signal that this task cannot be backtracked:
	   (if (eq task-node (prob~proof-root pds))
	       (setq root-task task keep-taskp T)
	     (setq undone-steps (pds~delete-node! task-node nil pds))))
	  ((agenda~goal-schema-p task)
	   ;; TASK had or has a schematic formula which cannot be closed, we consider then the
	   ;; plan step which lastly affected the constraint pool (CSTR-STP):
	   (let* ((cstr-pool (pds~constraint-pool pds))
		  (cstr-stps (when (pds~constraint-pool-p cstr-pool)
			       (pds~cstrpool-plansteps cstr-pool))))
	     (when (fboundp 'cosie~backtrack)
	       (cosie~backtrack (node~name task-node)))
	     (cond ((and (pds~constraint-pool-p cstr-pool) (null cstr-stps))
		    (omega~error ";;;plan=backtrack-task! inconsistent constraint pool ~A!" cstr-pool))
		   ((null cstr-pool)
		    ;; Case A: No planning step has affected the PDS constraint pool, then process similar
		    ;; to the case of the goal-task:
		    (setq undone-steps (pds~delete-node! task-node nil pds)))
		   ((rest cstr-stps)
		    (omega~error ";;;plan=backtrack-task! integrate this case with the next case!"))
		   (T
		    (let* ((cstr-stp (first cstr-stps))
			   (cstr-stp-just (pdsc~an-just cstr-stp))
			   (cstr-stp-succs (pdsj~successors cstr-stp-just))
			   (task-stps (pdsj~reasons (node~justification task-node))))
		      (cond ((some #'(lambda (tstp) (or (eq tstp cstr-stp)
							(find tstp cstr-stp-succs))) task-stps)
			     ;; Case B: At least one step that depends from TASK occurs after or corresponds to
			     ;; CSTR-STP, then process similar to the case of the goal-task.
			     (setq undone-steps (pds~delete-node! task-node nil pds)))
			    (T 
			     ;; O/w: CSTR-STP was carried out after all steps that depend on TASK (TASK-STPS),
			     ;; then we want to backtrack wrt. the constraint pool changement, i.e., we take CSTR-STP
			     ;; back and all steps which were carried out thereafter [successors(CSTR-STP)]. Hereby,
			     ;; we distinguish two cases: method expansion (Case C:) and method application (Case D:)
			     (let (node-to-open
				   (just-to-open cstr-stp-just)
				   (undo-stps (reverse cstr-stp-succs)))
			       (cond ((null (pdsj~own-reason cstr-stp-just))
				      ;; Case C: CSTR-STP corresponds to a method expansion: Take the expansion back:
				      (setq node-to-open (plan=remove-expansion! cstr-stp pds))
				      ;; Take the expansion step into the undone-steps
				      (setq undone-steps (list cstr-stp))
				      (setq expansion-stpp T)
				      ;; plan=remove-expansion! may be return other planning steps
				      (setq just-to-open (node~justification node-to-open)))
				     (T
				      ;; Case D: CSTR-STP corresponds to a method expansion:
				      (setq node-to-open (pdsc~an-node cstr-stp))))
			       ;; 1) Get the task formula before changing the constraint pool. TASK-FORMULA is needed
			       ;; to compare the TASK formula after taking back CSTR-STP with that before doing this.
			       ;; When TASK formula is changed, then the control information of TASK are reset, o/w,
			       ;; the control information remain the same to force another backtracking step in the
			       ;; next consideration of TASK. This approach is not complete, since not every constraint
			       ;; changement is taken into account, but only changements in the bindings of meta-variables
			       ;; that occur in the formula of TASK.
			       (setq task-formula (pds~node-formula task-node pds))
			       ;; 3) Changing the control info of the NODE-TO-OPEN:
			       ;; Failed methods with parameters are stored as excluded methods, in order to prevent 
			       ;; applying them in a similar situation. This is needed, because of exclusive situations.
			       ;; In order to prevent loops caused by such situations, some methods must have typical
			       ;; parameters for these situations. For instance:
			       ;;   - G is reduced to SG1 and SG2 in stp1,
			       ;;   - SG1 to SG11 in stp2 and SG2 to SG21 in stp3
			       ;;   - Now SG21 is closed by M1 in stp4 which affects the CSTR-POOL
			       ;;   - Assume SG11 cannot be closed: We have to take stp4 back. Two possibilities:
			       ;;     A: excluded(SG21) = NIL and SG11 < SG21
			       ;;      * closing SG11 by a method M2 (stp5) which has no subgoals will invalidate the
			       ;;        ordering SG11 < SG21
			       ;;      * Considering SG21, say this goal cannot be closed. We have then to take stp5 back
			       ;;      failed(SG11) = NIL SG21 < SG11
			       ;;      * The method M1 can now be applied to SG21 as in stp4 and we get the same situation
			       ;;      which would result in a LOOP
			       ;;
			       ;;     B: excluded(SG21) = (<M1 p11 .. p1n>) and SG11 < SG21
			       ;;      * closing SG11 by a method M2 (stp5) which has no subgoals will invalidate the
			       ;;      ordering SG11 < SG21
			       ;;      * Considering SG21, say this goal cannot be closed. We have then to take stp5 back
			       ;;      failed(SG11) = NIL SG21 < SG11
			       ;;      * The method M1 cannot be applied to SG21 only with other parameters p11 .. p1n.
			       ;;      When no other parameters are available and no other methods are applicable, we have
			       ;;      to take stp3 back which would prevent the LOOP
			       (when (pdsj~parameters just-to-open)
				 ;; The applied method have some parameters which (partially but hopefully sufficiently)
				 ;; specify the situation, in which the method was applied. To exclude applying the same
				 ;; method in the same situation, we store it into the slot excluded-methods:
				 (multiple-value-bind (outln-pat)
				     (plan=actual-outline-pattern node-to-open pds just-to-open)
				   (let ((the-method (pds~inference-application (just~method just-to-open) outln-pat)))
				     (setf (pdsn~excluded-methods node-to-open)
					   (cons (cons the-method (pdsj~parameters just-to-open))
						 (pdsn~excluded-methods node-to-open))))))
			       (setf (pdsn~failed-methods node-to-open) NIL
				     (pdsn~alternative-methods node-to-open) NIL
				     (pdsn~alternative-mmatchings node-to-open) NIL)
			       ;; Take CSTR-STP back and all steps which were carried out thereafter [successors(CSTR-STP)].
			       ;; For the successors(CSTR-STP), the control informations of the associated remaining
			       ;; nodes are reset, i.e., set to NIL.
			       (setq redo-steps cstr-stp-succs)
			       (when undo-stps
				 (loop
				  (let* ((undo-stp1 (first undo-stps))
					 (tback-stps (if (plan=expansion-step-p undo-stp1)
							 (plan=take-back-expansion! undo-stp1 pds)
						       (pds~open-node! (pdsc~an-node undo-stp1) pds))))
				    (setq undone-steps (union (plan=set-difference tback-stps undo-stps)
							      undone-steps))
				    (setq undo-stps (plan=set-difference undo-stps tback-stps))
				    (unless undo-stps (return)))))
;			       (let ((stp-to-open (pdsj~own-reason just-to-open)))
;                                 (setq undone-steps (union undone-steps
;                                                           (remove stp-to-open
;                                                           (pds~open-node! node-to-open pds)))
;                                       keep-taskp T passed-nodes (list node-to-open)))
;                               (setf (pds~constraint-pool pds) (pds~cstrpool-previous cstr-pool))
			       (let ((stp-to-open (pdsj~own-reason just-to-open)))
				 (setq undone-steps (union undone-steps (pds~open-node! node-to-open pds)))
				 (setq keep-taskp T passed-nodes (list node-to-open))
				 (when expansion-stpp
				   ;; In case of CSTR-STP is an expansion step, the reasons of TASK TASK-STPS can be
				   ;; ordered between the application of the method which is expanded (or led to the
				   ;; expansion) in CSTR-STP. Since this method is taken back too the reasons of TASK
				   ;; are taken back and TASK must be removed from the AGENDA:
				   (let ((opened-stp-succs (pdsj~successors just-to-open)))
				     (cond ((some #'(lambda (tstp) (or (eq tstp stp-to-open)
								       (find tstp opened-stp-succs))) task-stps)
					    ;; At least one step that depends from TASK occurs after or corresponds to
					    ;; the STP-TO-OPEN, which dont need to be the last CSTR-STP (see Exple below),
					    ;; then the steps of TASK (TASK-STPS) will be redone in PLAN=CONSISTENT-OPENS!
					    ;; and TASK should be removed from the AGENDA. 
					    ;; Exple: Close N by BH-AndE2FA in step s1; close a node N' by Ass delivers task
					    ;; SG in step s2; expand BH-AndE2FA(N) in step s3; In the backtracking wrt SG, we
					    ;; have the situation: s1: BH-AndE2FA(N) < s2: N'2SG < s3: Weaken-AndE*(N)
					    (setq keep-taskp NIL))))))
			       ))))))))
	  (T
	   ;; TASK is a pseudo task which could not be expanded, either because the expansion condition
	   ;; of the method could not be applied (when such a condition exists) or because the expansion
	   ;; constraint is not consistent to the current cstr-pool. In such situation we have to handle
	   ;; similar to the case of schematic task: If the last plan-step affecting the cstr-pool is
	   ;; after the own-plstp(TASK), then backtrack this plan-step, otherwise backtrack the
	   ;; own-plstp(TASK) and then if there is other alternatives to close TASK then create a new task
	   ;; and return the agenda; otherwise treat this TASK either as a GT, when the task formula does
	   ;; not contain meta-variables, or as a ST when its formula contains meta-variables:
	   (omega~error ";;;plan=backtrack-task! must be implemented for ~A!"
			task)))
    ;;; Determine remaining consistent open nodes: METH-NODES were closed by a methods and OTHER-NODES
    ;; were closed by tactics:
    (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
	(plan=consistent-opens! cstrsteps2succs
				(plan=set-difference undone-steps redo-steps) redo-steps passed-nodes NIL pds
				(prob~proof-steps pds))
      (let* ((the-meth-nodes (remove-duplicates (union meth-nodes pseudog-nodes)))
	     (the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
	(if the-other-nodes
	    ;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
	    (let ((extra-supps))
	      (dolist (each-node the-other-nodes)
		(setq extra-supps (union extra-supps
					 (plan=set-difference (pdsn~just-sponsors each-node)
							      (pdsn~just-unsponsors each-node)))))
	      (multiple-value-bind (the-agenda rest-opens)
		  (plan=normalize-supports extra-supps the-other-nodes agenda pds)
		(let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		       (new-tasks (if pds-rootp
				      (cons root-new-task
					    (append (mapcar #'agenda~create-task the-meth-nodes)
						    (mapcar #'agenda~create-goal rest-opens)))
				    (append (mapcar #'agenda~create-task the-meth-nodes)
					    (mapcar #'agenda~create-goal rest-opens))))
		       (new-agenda (pds~reset-schematic-nodes! pds
							       (if keep-taskp
								   (if new-tasks
								       (agenda~insert-tasks task nil new-tasks nil the-agenda)
								     the-agenda)
								 (agenda~replace-task task nil new-tasks nil the-agenda)))))
		  
		  (when task-formula
		    ;; We have Case A of schematic task: CSTR-STP > TASK-STPS
		    ;; After taking back the last constraint, i.e., that of CSTR-STP, we have to:
		    ;; 1- reset the control informations of the new schematic tasks in NEW-TASKS
		    ;; 2- reset the control information of TASK, when its formula is changed
		    ;; 3- add the ordering constraint TASK < NEW-TASKS to force that TASK has to
		    ;; be considered before the new tasks. This is important for two reasons: this
		    ;; ordering constraint prevent to get the same deadlock ater applying the same
		    ;; methods to NEW-TASKS, and it leads to the next backtracking wrt. TASK in
		    ;; case the control information of TASK is not reset.
		    ;; REMARK: It is possible to take back the last CSTR-STP instead of the task steps TASK-STPS
		    ;; because CSTR-STP > TASK-STPS. Even in such situation we have to reset the control
		    ;; information (set them to NIL) of the CSTR-STP, because some relevant methods might
		    ;; become applicable to CSTR-STP-NODE after further backtracking and changing the constraint
		    ;; pool. To avoid getting the same deadlock, we force that TASK be considered before the
		    ;; new tasks and among them CSTR-STP-NODE task. Though, the step 2) in case A above is
		    ;; not needed.
		    ;; This should be done in plan=consistent-opens!
		  
		    (unless (term~alpha-equal task-formula (pds~node-formula task-node pds))
		      (setf (pdsn~failed-methods task-node) NIL
					;(remove-if #'meth~p (pdsn~failed-methods task-node))
			    (pdsn~alternative-methods task-node) NIL
			    (pdsn~alternative-mmatchings task-node) NIL))
					;)
		    (when (pds~label2node (keim~name task-node) pds)
		      (setq new-agenda (agenda~insert-tasks task nil nil
							    (list (agenda~ordering-create (list task) new-tasks))
							    new-agenda)))) ;;; No orderings
		  (setf (pds~agenda pds) new-agenda)
		  ;(format T "~%All steps after Backtracking:~%~A" (pds~plan-steps pds))
		  (values
		   (or root-task root-new-task)
		   new-agenda))))
	  (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		 (new-tasks (if pds-rootp
				(cons root-new-task
				      (mapcar #'agenda~create-task the-meth-nodes))
			      (mapcar #'agenda~create-task the-meth-nodes)))
		 (new-agenda (pds~reset-schematic-nodes! pds
							 (if keep-taskp
							     (if new-tasks
								 (agenda~insert-tasks task nil new-tasks nil agenda)
							       agenda)
							   (agenda~replace-task task nil new-tasks nil agenda)))))
	    
	    (when task-formula
	      ;; We have Case A of schematic task: CSTR-STP > TASK-STPS
	      ;; After taking back the last constraint, i.e., that of CSTR-STP, we have to:
	      ;; 1- reset the control informations of the new schematic tasks in NEW-TASKS
	      ;; 2- reset the control information of TASK, when its formula is changed
	      ;; 3- add the ordering constraint TASK < NEW-TASKS to force that TASK has to
	      ;; be considered before the new tasks. This is important for two reasons: this
	      ;; ordering constraint prevent to get the same deadlock ater applying the same
	      ;; methods to NEW-TASKS, and it leads to the next backtracking wrt. TASK in
	      ;; case the control information of TASK is not reset.
	      ;; This should be done in plan=consistent-opens!
	      
	      (unless (term~alpha-equal task-formula (pds~node-formula task-node pds))
		(setf (pdsn~failed-methods task-node) NIL
					;(remove-if #'meth~p (pdsn~failed-methods task-node))
		      (pdsn~alternative-methods task-node) NIL
		      (pdsn~alternative-mmatchings task-node) NIL))
					;)
	      (when (pds~label2node (keim~name task-node) pds)
		(setq new-agenda (agenda~insert-tasks task nil nil
						      (list (agenda~ordering-create (list task) new-tasks))
						      new-agenda)))) ;;; No orderings
	    (setf (pds~agenda pds) new-agenda)
	    ;(format T "~%All steps after Backtracking:~%~A" (pds~plan-steps pds))
	    (values
	     (or root-task root-new-task)
	     new-agenda)))))
    ))

(defun stp~cstrpool-steps-and-successors (pds)
  ;;; Determines in case the constraint pool of PDS is not empty an association list
  ;; of the constraint pool plan steps to their successors just before the next constraint
  ;; plan step. The order of these steps is from the latest to the next previous one.
  ;; for instance, given a constraint pool with the following steps (<s1> <s4> <s5> <s7>),
  ;; where the last planning step in PDS is s9, this function must deliver
  ;; (<s7 s8 s9> <s5 s6 s7> <s4 s5> <s1 s2 s3 s4>)
  (labels ((successors-till (stp last-stp)
			    (let ((stp-succ (pdsj~successor (pdsc~an-just stp))))
			      (if (eq last-stp stp-succ)
				  (list last-stp)
				(cons stp-succ
				      (successors-till stp-succ last-stp)))))
	   (cstrsteps-and-succs (cstrpool last-cstrstp)
				(when cstrpool
				  (let* ((cstrstp (first (pds~cstrpool-plansteps cstrpool)))
					 (cstrstp-succs (successors-till cstrstp last-cstrstp)))
				    (cons (cons cstrstp cstrstp-succs)
					  (cstrsteps-and-succs (pds~cstrpool-previous cstrpool) cstrstp)))))
	   )
    (let ((cstrpool (pds~constraint-pool pds)))
      (when cstrpool
	(let ((last-cstrpool-stp (first (pds~cstrpool-plansteps cstrpool))))
	  (cons (cons last-cstrpool-stp (pdsj~successors (pdsc~an-just last-cstrpool-stp)))
		(cstrsteps-and-succs (pds~cstrpool-previous cstrpool) last-cstrpool-stp)))))
    ))


(defun oc=read-problem (pname)
  (sys~handler-case
   (with-open-file (in pname :direction :input
                       :if-does-not-exist :error)
                   (let ((plist (read in)))
		     (sys~handler-case
		      (let ((problem (prob~find-problem (cadr plist)))
			    (newobj (post~read-object plist (env~create) nil)))
			(when problem
			  (omega~message "Redefining problem ~A~%" (keim~name problem))
			  (when (prob~proofs problem)
			    (dolist (x (prob~proofs problem))
			      (pds~remove-proof-plan x))))
			(oc=prove-pre (ot~read-proof-plan newobj))
			)
		      (error (c)
			     (omega~error (format nil "~A" c))
			     (sys~signal
			      (sys~make-condition 'inter+error
						  :format-string "~A is not a problem file!"
						  :args (list pname)))))))
   (file-error (c) (inter~print-error (comint~interface comint*current-comint) c))))

(defun oc=prove (proof-plan)
  (when (mixin~activep)
    (let* ((name (keim~name proof-plan))
	   (method (format nil "setProofName(~a)" (parse~atom name))))
      (socket~write method :inout)))
  (when (and view*on (pds~proof-plan-p omega*current-proof-plan))
    (view~hide-proof omega*current-proof-plan))
; (when (and (not (mixin~activep))
;	     view*on (pds~proof-plan-p omega*current-proof-plan))
;    (view~hide-proof omega*current-proof-plan))
  (omega~message "Changing to proof plan ~A" (keim~name proof-plan))
  (let ((oldtheory (when omega*current-proof-plan (prob~proof-theory omega*current-proof-plan))))
    (setq omega*current-proof-plan proof-plan
	  omega*current-theory (prob~proof-theory proof-plan)
	  logic*current-theory omega*current-theory
	  keim::pds*current-proof-plan proof-plan)
    (alift~initialize omega*current-proof-plan) ;; KEIM-3 comment VS
    (setf foci*proof-context (pds~proof-context proof-plan))
    (setf foci*in-use t)
    (when (and (csm~active-p) (not auto*in-use))
      (oc=agents-restart :theo-default (unless (eq omega*current-theory oldtheory) omega*current-theory))))
  (when view*on 
;    (unless (mixin~activep) (view~unhide-proof omega*current-proof-plan))
    (view~unhide-proof omega*current-proof-plan)
    (view~clean-proof view*view)
    (view~display-proof omega*current-proof-plan))
  (plan~reset-planner)
  (leo~special-output
   '(concatenate 'string
		 (atptop~cut-x-last-chars (format nil "~%~A" (keim~name (prob~proof-problem omega*current-proof-plan))) 2)
		 " "))
  omega*current-proof-plan) 


(defun oc=delete (node &optional (pds omega*current-proof-plan))
  (declare (edited  "23-APR-1998")
	   (authors Lassaad)
	   (input   "A node and a pds.")
	   (effect  "deletes NODE and update PDS.")
	   (value   "see plan=backtrack-node!."))
  ;;; Redefinition of oc=delete in /top/omega-com.lisp:
  (if (eq node (prob~proof-root pds))
      (progn
	(plan~message ";;; Node ~A corresponds to the root of ~A, it may not be deleted!"
		      node pds)
	(values nil (pds~agenda)))
    (or (omega~trace ";;; Deleting node ~A ..." node)
	(let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	      (undone-steps (pds~delete-node! node nil pds))
	      (agenda (pds~agenda pds)))
	  ;; Foci stuff:
	  (when (foci~in-use) (foci~compute-pcs :pds pds :delete-case t))
	  (multiple-value-bind (meth-nodes other-nodes pseudo pds-rootp) ;;4 values MP
	      (plan=consistent-opens! cstrsteps2succs undone-steps nil nil nil pds)
	    (if other-nodes
	        ;;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
		(let ((extra-supps))
		  (dolist (each-node other-nodes)
		    (setq extra-supps (union extra-supps
					     (plan=set-difference (pdsn~just-sponsors each-node)
								  (pdsn~just-unsponsors each-node)))))
		  (multiple-value-bind (the-agenda rest-opens)
		      (plan=normalize-supports extra-supps other-nodes agenda pds)
		    ;;; Creates new tasks
		    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
			   (new-tasks (if pds-rootp
					  (cons root-new-task
						(append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
							(mapcar #'agenda~create-goal rest-opens)))
					(append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
						(mapcar #'agenda~create-goal rest-opens)))))
		      (when new-tasks
			(if (or (agenda~empty-p the-agenda) (agenda~first-task the-agenda))
			    (setq the-agenda (agenda~create nil new-tasks nil the-agenda))
			  (let ((next-tasks (agenda~next-tasks the-agenda)))
			    (setf (agenda~next-tasks the-agenda) (append new-tasks next-tasks))
			    (when next-tasks
			      (setf (agenda~orderings the-agenda)
				    (cons (agenda~ordering-create new-tasks next-tasks)
					  (agenda~orderings the-agenda)))))))
		      ;;; Adapt the PDS agenda:
		      (setf (pds~agenda pds)
			    (agenda~update the-agenda (prob~proof-steps pds)))
		      ;;; Return root task and new agenda:
		      (values root-new-task (pds~agenda pds)))))
	      (if (or meth-nodes pseudo pds-rootp)
		  (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
			 (new-tasks (if pds-rootp
					(cons root-new-task
					      (mapcar #'agenda~create-open-task (append meth-nodes pseudo)))
				      (mapcar #'agenda~create-open-task (append meth-nodes pseudo)))))
		    (if (or (agenda~empty-p agenda) (agenda~first-task agenda))
			(setq agenda (agenda~create nil new-tasks nil agenda))
		      (let ((next-tasks (agenda~next-tasks agenda)))
			(setf (agenda~next-tasks agenda) (append new-tasks next-tasks))
			(when next-tasks
			  (setf (agenda~orderings agenda)
				(cons (agenda~ordering-create new-tasks next-tasks)
				      (agenda~orderings agenda))))))
		    ;;; Adapt the PDS agenda:
		    (setf (pds~agenda pds)
			  (agenda~update agenda (prob~proof-steps pds)))
		    ;;; Return root task and new agenda:
		    (values root-new-task (pds~agenda pds)))
		(progn
		  ;;; Adapt the PDS agenda:
		  (setf (pds~agenda pds)
			(agenda~update agenda (prob~proof-steps pds)))
		  ;; Return root task and new agenda:
		  (values nil (pds~agenda pds))))))))))


(defun oc=open (node &optional (pds omega*current-proof-plan))
  (declare (edited  "23-APR-1998")
	   (authors Lassaad)
	   (input   "A node and a pds.")
	   (effect  "Opens NODE and update PDS.")
	   (value   "see plan=backtrack-node!."))
  ;;; Redefinition of oc=open in /top/omega-com.lisp:
  (omega~trace ";;; Opening node ~A ..." node)
  (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds))
	(undone-steps (pds~open-node! node pds))
	(agenda (pds~agenda pds)))
    ;; Foci stuff:
    (when (foci~in-use) (foci~compute-pcs :pds pds :delete-case t))
    (multiple-value-bind (meth-nodes other-nodes pseudo pds-rootp) ;; 4 values MP
	(plan=consistent-opens! cstrsteps2succs undone-steps nil nil nil pds)
      (if other-nodes
	  ;;; Normalize the extra-supports [sponsors \ unsponsors] before creating new tasks for them
	  (let ((extra-supps))
	    (dolist (each-node other-nodes)
	      (setq extra-supps (union extra-supps
				       (plan=set-difference (pdsn~just-sponsors each-node)
						       (pdsn~just-unsponsors each-node)))))
	    (multiple-value-bind (the-agenda rest-opens)
		(plan=normalize-supports extra-supps other-nodes agenda pds)
	      ;;; Creates new tasks
	      (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		     (new-tasks (if pds-rootp
				    (cons root-new-task
					  (append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
						  (mapcar #'agenda~create-goal rest-opens)))
				  (append (mapcar #'agenda~create-open-task (append meth-nodes pseudo))
					  (mapcar #'agenda~create-goal rest-opens)))))
		(when new-tasks
		  (if (or (agenda~empty-p the-agenda) (agenda~first-task the-agenda))
		      (setq the-agenda (agenda~create nil new-tasks nil the-agenda))
		    (let ((next-tasks (agenda~next-tasks the-agenda)))
		      (setf (agenda~next-tasks the-agenda) (append new-tasks next-tasks))
		      (when next-tasks
			(setf (agenda~orderings the-agenda)
			      (cons (agenda~ordering-create new-tasks next-tasks)
				    (agenda~orderings the-agenda)))))))
	        ;;; Adapt the PDS agenda:
		(setf (pds~agenda pds)
		      (agenda~update the-agenda (prob~proof-steps pds)))
	        ;;; Return root task and new agenda:
		(values root-new-task (pds~agenda pds)))))
	(if (or meth-nodes pseudo pds-rootp)
	    (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
		   (new-tasks (if pds-rootp
				  (cons root-new-task
					(mapcar #'agenda~create-open-task (append meth-nodes pseudo)))
				(mapcar #'agenda~create-open-task (append meth-nodes pseudo)))))
	      (if (or (agenda~empty-p agenda) (agenda~first-task agenda))
		  (setq agenda (agenda~create nil new-tasks nil agenda))
		(let ((next-tasks (agenda~next-tasks agenda)))
		  (setf (agenda~next-tasks agenda) (append new-tasks next-tasks))
		  (when next-tasks
		    (setf (agenda~orderings agenda)
			  (cons (agenda~ordering-create new-tasks next-tasks)
				(agenda~orderings agenda))))))
	      ;;; Adapt the PDS agenda:
	      (setf (pds~agenda pds)
		    (agenda~update agenda (prob~proof-steps pds)))
	      ;;; Return root task and new agenda:
	      (values root-new-task (pds~agenda pds)))
	  (progn
	    ;;; Adapt the PDS agenda:
	    (setf (pds~agenda pds)
		  (agenda~update agenda (prob~proof-steps pds)))
	    ;;; Return root task and new agenda:
	    (values nil (pds~agenda pds))))))
    ))

(defun plan=all-cstrpool-plansteps (cstrpool)
  (declare (edited  "19-APR-1998")
	   (authors Lassaad)
	   (input   "A constraint pool.")
	   (effect  "None.")
	   (value   "The list of the plan steps on which depends CSTRPOOL"
		    "beginning with the last."))
  (let ((steps (pds~cstrpool-plansteps cstrpool))
	(previous-pool (pds~cstrpool-previous cstrpool)))
    (append steps
	    (when previous-pool
	      (plan=all-cstrpool-plansteps previous-pool)))
    ))

(defun plan=all-cstrpool-bindings (cstrpool)
  (declare (edited  "19-APR-1998")
	   (authors Lassaad)
	   (input   "A constraint pool.")
	   (effect  "None.")
	   (value   "The list of the all CSTRPOOL bindings beginning with the first."))
  (let ((bindings (pds~cstrpool-bindings cstrpool))
	(previous-pool (pds~cstrpool-previous cstrpool)))
    (if previous-pool
	(let ((first-bindings (plan=all-cstrpool-bindings previous-pool)))
	  (if bindings
	      (append first-bindings (list bindings))
	    first-bindings))
      (when bindings (list bindings)))
    ))


(defun plan=consistent-opens! (cstrstps2succs undone-stps redo-stps passed-onodes passed-cnodes pds
					      &optional (pds-nodes (prob~proof-steps pds)) 
					      (cstrpool (pds~constraint-pool pds)))
  ;;; The additional argument cstrstp2succ is needed because the successors of undone constraint steps are
  ;; lost during the deletion of the associated justifications and nodes.
  ;; For instance, when before opening/deleting some nodes the constraint pool had the following steps
  ;; (<s1> <s4> <s5> <s7>), where the last planning step in PDS was s9, cstrstps2succs must be 
  ;; (<s7 s8 s9> <s5 s6 s7> <s4 s5> <s1 s2 s3 s4>)
  (declare (edited  "18-APR-1998")
	   (authors Lassaad)
	   (input   "Two lists of backtracked plan steps, open nodes whose control informations were adapted"
		    "by the caller of this function, a pds, its current node list, and its current constraint"
		    "pool.")
	   (effect  "Changes the current CSTR-POOL of PDS when elements of UNDONE-STPS belong to CSTRPOOL-STPS,"
		    "updates the alternative lists of some open nodes in PDS, (for the associated nodes of"
		    "REDO-STPS, the control information list are set to NIL) and deletes some of the remaining"
		    "open nodes which may not have an empty alternative method list. A node may not have an empty"
		    "alternative list iff it was closed by a planning method and the associated step belongs to"
		    "UNDONE-STPS.")
	   (value   "A tuple: two list of open nodes which always belong to PDS and which have consistent control"
		    "informations. The first list contains the nodes which were closed by planning methods, and"
		    "the second list contains the nodes which were closed by tactics, rules, or bboxes."
		    "A third of list of nodes closed by unreliable methods."
		    "Moreover the pds root node as a third value when it is involved in the backtracking step."))
  (if undone-stps
      (if cstrpool
	  ;;; We have to check that no plan step on which CSTRPOOL depends belongs to UNDONE-STPS:
	  (let ((cstrstp&succs (find-if #'(lambda (stp-succs) (find (first stp-succs) undone-stps))
					cstrstps2succs)))
	    (if cstrstp&succs
		;;; At least one undone step corresponds to a constraint pool step, e.g., s5
		;; (cstrstp&succs is <s5 s6 s7>): This step must be undone with its successors <s9 s8 s7 s6 s5>
		;; and the rest of the cstrstps2succs (<s4 s5> <s1 s2 s3 s4>)
		(let* ((cstrstp&succs-pos (position cstrstp&succs cstrstps2succs :test #'equal))
		       (cstrstp&other-succs (subseq cstrstps2succs 0 cstrstp&succs-pos))
		       (all-to-undo-stps (append
					  (apply #'append (mapcar #'(lambda (stp-succs) (reverse (rest stp-succs)))
								  cstrstp&other-succs))
					  (reverse cstrstp&succs)))
		       (to-redo-stps (butlast all-to-undo-stps))
		       (to-pass-stp (first (last all-to-undo-stps)))
		       (rest-cstrstps2succs (subseq cstrstps2succs (+ 1 cstrstp&succs-pos))))
		  ;;; For the example, we must get:
		  ;; cstrstp&succs-pos = 1; cstrstp&other-succs = (<s7 s8 s9>)
		  ;; all-to-undo-stps = (s9 s8 s7 s6 s5); rest-cstrstps2succs = (<s4 s5> <s1 s2 s3 s4>)
		  ;; to-redo-stps = (s9 s8 s7 s6); to-pass-stp = s5
		  ;; The steps s9, s8, s7, and s6 must be taken back and in the recursive call of
		  ;; plan=consistent-opens! must be considered as redo-stps, which has to be NIL as
		  ;; control information. The step s5 must be taken back and must be considered as a
		  ;; passed node, the control information of s5 must be preserved.
		  (let ((new-redo-stps redo-stps)
			(new-undone-stps (remove to-pass-stp undone-stps)))
		    (dolist (to-redo-stp to-redo-stps)
		      (let ((to-redo-stp-node (pdsc~an-node to-redo-stp)))
			(when (find to-redo-stp-node (prob~proof-steps pds))
			  (cond ((pdsn~open-node-p to-redo-stp-node)
				 (push to-redo-stp new-redo-stps))
				((plan=expansion-step-p to-redo-stp)
				 ;; Method expansion: take back this expansion
				 (let ((back-stps (if (eq (node~justification (pdsc~an-node to-redo-stp))
							  (pdsc~an-just to-redo-stp))
						      (plan=take-back-expansion! to-redo-stp pds)
						    (list to-redo-stp))))
				   (push to-redo-stp new-redo-stps)
				   (setq new-undone-stps (plan=set-difference
							  (union new-undone-stps (remove to-redo-stp back-stps))
							  new-redo-stps))))
				(T ;; Method application: take back this application
				 (let ((back-stps (pds~open-node! to-redo-stp-node pds)))
				   (push to-redo-stp new-redo-stps)
				   (setq new-undone-stps (plan=set-difference
							  (union new-undone-stps (remove to-redo-stp back-stps))
							  new-redo-stps))))))
			(when (find to-redo-stp (pds~cstrpool-plansteps (pds~constraint-pool pds)))
			  (setf (pds~constraint-pool pds) (pds~cstrpool-previous (pds~constraint-pool pds))))))
		    ;; Change the constraint pool of PDS
		    (setf (pds~constraint-pool pds) (pds~cstrpool-previous (pds~constraint-pool pds)))
		    ;; Possibly change the control information of to-pass-stp, and recursively call
		    ;; plan=consistent-opens! after taking CSTR-STP-NODE to the PASSED-ONODES or PASSED-CNODES
		    (cond ((plan=expansion-step-p to-pass-stp)
			   (plan=consistent-opens! rest-cstrstps2succs
						   (remove-duplicates new-undone-stps)
						   (remove-duplicates new-redo-stps)
						   passed-onodes
						   (cons (pdsc~an-node to-pass-stp) passed-cnodes)
						   pds))
			  (T
			   (let ((cstr-stp-node (pdsc~an-node to-pass-stp)))
			     (cond ((find cstr-stp-node passed-onodes)
				    (plan=consistent-opens! rest-cstrstps2succs
							    (remove-duplicates new-undone-stps)
							    (remove-duplicates new-redo-stps)
							    passed-onodes passed-cnodes pds))
				   (T
				    (let ((cstr-stp-just (pdsc~an-just to-pass-stp)))
				      ;; LC: Changing the control of CSTR-STP-NODE (a node closed in a planning step 
				      ;; which affected the constraint pool and this planning step belongs to the undone
				      ;; steps, therefore we have to change the control of this node)
				      ;; 1) When alter-mmatchs is not empty:
				      ;;  The control remains the same 
				      ;; 2) When alter-mmatchs is NIL:
				      ;;  failed-methods(CSTR-STP-NODE) := failed-methods U {now-method}
				      ;;  alter-methods  and alter-mmatchs remain the same
				      ;; LC-REMARK1: There is no matter, if CSTR-STP-NODE cannot be closed, i.e, neither
				      ;; alternative mmatchings nor alternative methods are available. This situation is considered
				      ;; in the recursive call of the planner, where considering the associated task of CSTR-STP-NODE
				      ;; will invoke a backtracking step. There, the conflict is solved by either deleting the node
				      ;; CSTR-STP-NODE or taking another step which affected the constraint pool.
				      (unless (pdsn~alternative-mmatchings cstr-stp-node)
					(multiple-value-bind (outln-pat)
					    (plan=actual-outline-pattern cstr-stp-node pds cstr-stp-just)
					  (let ((the-method (pds~inference-application (just~method cstr-stp-just) outln-pat)))
					    (setf (pdsn~failed-methods cstr-stp-node)
						  (cons (if (pdsj~parameters cstr-stp-just)
							    (cons the-method (pdsj~parameters cstr-stp-just))
							  the-method)
							(pdsn~failed-methods cstr-stp-node))))))
				      (plan=consistent-opens! rest-cstrstps2succs
							      (remove-duplicates new-undone-stps)
							      (remove-duplicates new-redo-stps)
							      (cons (pdsc~an-node to-pass-stp) passed-onodes)
							      passed-cnodes pds)))))))))
	      ;;; No one of the backtracked steps affected the constraint pool, then make a recursive call
	      ;; with a nil as constraint pool
	      (plan=consistent-opens! cstrstps2succs undone-stps redo-stps passed-onodes passed-cnodes pds pds-nodes nil)))
	;;; No plan step on which CSTRPOOL depends, belongs to UNDONE-STPS:
	(let* ((stp (first undone-stps))
	       (stp-node (pdsc~an-node stp)))
	  (if (find stp-node pds-nodes)
	      (cond ((pdsn~open-node-p stp-node) 
		     ;; LC-REMARK2: The planning step which closed STP-NODE and which is now taken back, did not affect
		     ;; the constraint pool. Therefore, in case the node STP-NODE cannot be closed, i.e, neither
		     ;; alternative mmatchings nor alternative methods are available, this node must be deleted,
		     ;; in contrast to the case of a planning step affecting the constraint pool (see LC-REMARK1 above). 
		     (if (or (pdsn~alternative-mmatchings stp-node) (pdsn~alternative-methods stp-node))
			 ;; STP-NODE was closed by a method, it is now open and has some alternative methods:
			 (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
			     (plan=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps
						     passed-onodes passed-cnodes pds pds-nodes nil)
			   (if (pdsn~alternative-mmatchings stp-node)
			       ;; Because of alternative mmatchings, we dont have to adapt the control info
			       (values (cons stp-node meth-nodes) other-nodes pseudog-nodes pds-rootp)
			     (let ((stp-inference (just~method (pdsc~an-just stp))))
			       (multiple-value-bind (outln-pat)
				   (plan=actual-outline-pattern stp-node pds (pdsc~an-just stp))
				 (let ((the-method (pds~inference-application stp-inference outln-pat)))
				   (setf (pdsn~failed-methods stp-node)
					 (cons (if (pdsj~parameters (pdsc~an-just stp))
						   (cons the-method (pdsj~parameters (pdsc~an-just stp)))
						 the-method)
					       (pdsn~failed-methods stp-node))))
				 (values (cons stp-node meth-nodes) other-nodes pseudog-nodes pds-rootp)))))
		       ;; STP-NODE has no alternative method:
		       (let ((stp-inference (just~method (pdsc~an-just stp))))
			 (if (infer~method-p stp-inference)
			     ;; STP-NODE was closed by a method, it is now open and has no alternative method,
			     ;; this must be deleted. When this node corresponds to the PDS root node then this
			     ;; node cannot be deleted, but we have to signal that the PDS root node was involved
			     ;; in this backtracking process by returning it as a third value of this function.
			     ;; Otherwise, after deleting this node, we have to consider the constraint pool in
			     ;; the recursive call, because of new undone steps:
			     (if (eq stp-node (prob~proof-root pds))
				 (multiple-value-bind (meth-nodes other-nodes pseudog-nodes)
				     (plan=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps
							     passed-onodes passed-cnodes pds pds-nodes nil)
				   ;; Before returning the pds root, we add the method taken back to its
				   ;; failed method list:
				   (let ((stp-inference (just~method (pdsc~an-just stp))))
				     (multiple-value-bind (outln-pat)
					 (plan=actual-outline-pattern stp-node pds (pdsc~an-just stp))
				       (let ((the-method (pds~inference-application stp-inference outln-pat)))
					 (setf (pdsn~failed-methods stp-node)
					       (cons (if (pdsj~parameters (pdsc~an-just stp))
							 (cons the-method (pdsj~parameters (pdsc~an-just stp)))
						       the-method)
						     (pdsn~failed-methods stp-node))))))
				   (values meth-nodes other-nodes pseudog-nodes stp-node))
			       (let ((additional-stps (pds~delete-node! stp-node nil pds)))
				 (plan=consistent-opens! cstrstps2succs (remove stp (append undone-stps additional-stps))
							 redo-stps passed-onodes passed-cnodes pds (prob~proof-steps pds))))
			   ;; STP was the application of a rule, a tactic, or a bbox: First try to apply
			   ;; task-restricting methods to STP-NODE with the supports: pds-supports and
			   ;; sponsors(STP-NODE). When STP-NODE cannot be closed, then return it:
			   (multiple-value-bind (a-mmatching)
			       (plan=apply-first-method pds stp-node (node~formula stp-node)
							(append (pds~support-nodes pds) (pdsn~just-sponsors stp-node))
							nil nil meth*restricting-methods)
			     (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
				 (plan=consistent-opens! cstrstps2succs (rest undone-stps)
							 redo-stps passed-onodes passed-cnodes pds pds-nodes nil)
			       (if a-mmatching
				   ;; STP-NODE is closed, no need to return it
				   (values meth-nodes other-nodes pseudog-nodes pds-rootp)
				 (values meth-nodes (cons stp-node other-nodes) pseudog-nodes pds-rootp))))))))
		    (T ;; STP corresponds to a method expansion: return STP-NODE with the meth-nodes, when the method
		       ;; justifying STP-NODE is non-reliable.
		     (let ((exp-inference (just~method (node~justification stp-node))))
		       (cond ((infer~method-p exp-inference)
			      (multiple-value-bind (outln-pat)
				  (plan=actual-outline-pattern stp-node pds (node~justification stp-node))
				(let ((the-method (pds~inference-application exp-inference outln-pat)))
				  (if (meth~non-reliability the-method)
				      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					  (plan=consistent-opens! cstrstps2succs (rest undone-stps)
								  redo-stps passed-onodes passed-cnodes pds pds-nodes nil)
					(values meth-nodes other-nodes (cons stp-node pseudog-nodes) pds-rootp))
				    (plan=consistent-opens! cstrstps2succs (rest undone-stps)
							    redo-stps passed-onodes passed-cnodes pds pds-nodes nil)))))
			     (T
			      (plan=consistent-opens! cstrstps2succs (rest undone-stps)
						      redo-stps passed-onodes passed-cnodes pds pds-nodes nil))))))
	    ;;; STP-NODE occurs no more in PDS: 
	    (plan=consistent-opens! cstrstps2succs (rest undone-stps) redo-stps passed-onodes passed-cnodes pds pds-nodes nil))))
    ;;; The first UNDONE-STPS are processed, now we have to process the second undone REDO-STPS:
    (if redo-stps
	(let ((stp-node (pdsc~an-node (first redo-stps))))
	  (if (find stp-node pds-nodes)
	      (cond ((pdsn~open-node-p stp-node)
		     (let ((stp-inference (just~method (pdsc~an-just (first redo-stps)))))
		       ;; Reset the control information of STP-NODE
		       (setf (pdsn~failed-methods stp-node) nil)
		       (setf (pdsn~alternative-mmatchings stp-node) nil)
		       (setf (pdsn~alternative-methods stp-node) nil)
		       (multiple-value-bind (meth-nodes other-nodes pseudog-nodes)
			   (plan=consistent-opens! cstrstps2succs nil (rest redo-stps) passed-onodes passed-cnodes pds pds-nodes nil)
			 (if (infer~method-p stp-inference)
			     (values (cons stp-node meth-nodes) other-nodes pseudog-nodes)
			   (values meth-nodes (cons stp-node other-nodes) pseudog-nodes)))))
		    (T ;; STP corresponds to a method expansion: return STP-NODE with the meth-nodes, when the method
		       ;; justifying STP-NODE is non-reliable.
		     (let ((exp-inference (just~method (node~justification stp-node))))
		       (cond ((infer~method-p exp-inference)
			      (multiple-value-bind (outln-pat)
				  (plan=actual-outline-pattern stp-node pds (node~justification stp-node))
				(let ((the-method (pds~inference-application exp-inference outln-pat)))
				  (if (meth~non-reliability the-method)
				      (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					  (plan=consistent-opens! cstrstps2succs nil (rest redo-stps)
								  passed-onodes passed-cnodes pds pds-nodes nil)
					(values meth-nodes other-nodes (cons stp-node pseudog-nodes) pds-rootp))
				    (plan=consistent-opens! cstrstps2succs nil (rest redo-stps)
							    passed-onodes passed-cnodes pds pds-nodes nil)))))
			     (T
			      (plan=consistent-opens! cstrstps2succs nil (rest redo-stps)
						      passed-onodes passed-cnodes pds pds-nodes nil))))))
	    (plan=consistent-opens! cstrstps2succs nil (rest redo-stps) passed-onodes passed-cnodes pds pds-nodes nil)))
      ;;; Now consider the passed-nodes:
      (values (intersection passed-onodes pds-nodes)
	      NIL
	      (set-difference (intersection passed-cnodes pds-nodes) passed-onodes)))
    ))



(defun plan=do-step (pds &optional (agenda (pds~agenda pds)) &key ((:task task))
			 ((:first-methods first-methods)) ((:rest-methods rest-methods)))  
  (declare (edited  "30-MAR-1998")
	   (authors Lassaad)
	   (input   "A non-empty agenda, and a pds")
	   (effect  "Try to plan a task by a method application or expansion, when"
		    "this is possible, otherwise take this task back or anther task to"
		    "go on.")
	   (value   "A tuple:"
		    "- NIL if the considered task can be planned or the PDS root node"
		    "is not involved in the backtracking step, and the task associated"
		    "to the PDS root node, if this is involved in the backtracking step."
		    "- the resulted agenda."
		    "- T, when this step corresponds to a method application or expansion,"
		    "and NIL when it corresponds to backtracking some tasks."))
  (when (or first-methods rest-methods)
    (omega~message ";;;plan=do-step should be extended to consider first-methods ~A and rest-methods ~A"
		   first-methods rest-methods))
  (multiple-value-bind (unblocked-tasks were-blocked-tasks)
      (if task
	  (if (agenda~task-blocked-p task)
	      (progn
		(setf (agenda~task-blocked-p task) nil)
		(values nil (list task)))
	    (values (list task)))
	(pds~first-tasks! pds agenda))
    (dolist (was-blocked were-blocked-tasks)
      (plan~message ";;; ~A is no more blocked." was-blocked))
    (let* ((first-tasks (if unblocked-tasks unblocked-tasks
			  were-blocked-tasks))
	   (sorted-alternatives (when (rest first-tasks)
				  (cri~call first-tasks :kind 'tasks :tasks first-tasks :pds pds))))
      ;; More than one task to be considered, then invoke the CRI:
      ;; CRI returns a list of alternatives, where an alternative is either a task,
      ;; a list (task [methods] [params] [supps]), or a list (task [method] [params] [supps]).
      ;; This list must be merged with the task failed-methods, with the alternative methods:
      ;; FIRST-METHODS should be a list of methods and methods with control informations
      (multiple-value-bind (task first-methods rest-methods)
	  (if (or (not sorted-alternatives)
		  (eq sorted-alternatives first-tasks))
	      ;; No control rule fires, then follow the default control: Goals < Pseudo-goals < Schematic-goals
	      (let ((goal-task (find-if #'(lambda (task)
					    (or (agenda~goal-p task)
						(and (agenda~goal-schema-p task)
						     (not (agenda~goal-schematic-p task)))))
					first-tasks)))
		(if goal-task
		    (cond ((or (pdsn~alternative-mmatchings (agenda~task-node goal-task))
			       (pdsn~failed-methods (agenda~task-node goal-task))
			       (pdsn~alternative-methods (agenda~task-node goal-task)))
			   (values goal-task nil (plan=intersection meth*planning-methods
								    (pdsn~alternative-methods (agenda~task-node goal-task)))))
			  (T
			   (values goal-task nil meth*planning-methods)))
		  (let ((pseudo-goal-task (find-if #'agenda~pseudo-goal-p first-tasks)))
		    (if pseudo-goal-task
			(values pseudo-goal-task)
		      (cond ((or (pdsn~alternative-mmatchings (agenda~task-node (first first-tasks)))
				 (pdsn~failed-methods (agenda~task-node (first first-tasks)))
				 (pdsn~alternative-methods (agenda~task-node (first first-tasks))))
			     (values (first first-tasks) nil
				     (plan=intersection meth*MOReasoning-methods
							(pdsn~alternative-methods (agenda~task-node (first first-tasks))))))
			    (T
			     (values (first first-tasks) nil meth*MOReasoning-methods)))))))
	    ;; Merge the control information
	    (plan=merge-task-alternatives sorted-alternatives))
	
	(cond ((agenda~pseudo-goal-p task)
	       (let ((new-agenda (plan=refine-task task agenda pds)))
		 (if new-agenda
		     (values NIL new-agenda T)
		   (omega~error ";;;plan=do-step: Expansion of ~A leads to a failure." task)))
	       ;; This function tries to expand the stated method and eventually
	       ;; changes the agenda. When the stated method cannot be expanded, 
	       ;; there are many possibilities for how to go on: - Backtracking, 
	       ;; i.e., when the node was open, than reopen it and consider the
	       ;; remaining alternatives, when the node was inserted by a method
	       ;; expansion, then backtrack this method. (- Opening and trying to
	       ;; plan this node, here we must have another kind of method
	       ;; expansions)
	       )
	      ((or (agenda~goal-p task) (and (agenda~goal-schema-p task)
					     (not (agenda~goal-schematic-p task))))
	       (let ((new-agenda (plan=plan-task task agenda pds :context "PLANNING" 
						 :methods rest-methods :first-methods first-methods)))
		 (plan=show-agenda "~%AGENDA AFTER A PLANNING STEP: ~A" new-agenda)
		 (if new-agenda (values nil new-agenda t)
		   (plan=backtrack-task! task agenda pds))))
	      (T
	       (let ((new-agenda (if first-methods
				     (plan=plan-task task agenda pds :context "MOR-PLANNING" 
						     :methods NIL :first-methods first-methods)
				   (plan=plan-task task agenda pds :context "MOR-PLANNING" 
						   :methods rest-methods :first-methods NIL))))
		 (plan=show-agenda "~%AGENDA AFTER A PLANNING STEP: ~A" new-agenda)
		 (if new-agenda (values nil new-agenda t)
		   (plan=backtrack-task! task agenda pds))))))
      )))

(defun plan=back-last-step! (&optional (pds omega*current-proof-plan) open-nodes)
  (declare (edited  "06-MAY-1999")
	   (authors Lassaad)
	   (input   "A pds with an empty agenda.")
	   (effect  "Takes back the last planning step of PDS.")
	   (value   "Unspecified."))
  (cond ((agenda~empty-p (pds~agenda pds))
	 (let ((last-stp (pds~last-plan-step pds)))
	   (cond ((plan=expansion-step-p last-stp)
		  (omega~error ";;;plan=back-last-step! must be extended: 1")
		  (error "BACK-LAST-STEP"))
		 (T
		  (let ((last-stp-node (pdsc~an-node last-stp))
			(last-stp-just (pdsc~an-just last-stp)))
		    (multiple-value-bind (outln-pat)
			(plan=actual-outline-pattern last-stp-node pds last-stp-just)
		      (let ((the-method (pds~inference-application (just~method last-stp-just) outln-pat)))
			(cond ((find the-method meth*restricting-methods)
			       (pds~open-node! last-stp-node pds)
			       (plan=back-last-step! pds (cons last-stp-node open-nodes)))
			      ((null (meth~goal the-method)) ;; A forward method
			       (pds~open-node! last-stp-node pds)
			       (plan=back-last-step! pds open-nodes))
			      (T ;;LAST-STP corresponds to a planning step:
			       (when (pdsj~parameters last-stp-just)
				 (setf (pdsn~excluded-methods last-stp-node)
				       (cons (cons the-method (pdsj~parameters last-stp-just))
					     (pdsn~excluded-methods last-stp-node))))
			       (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
				   (plan=consistent-opens! (stp~cstrpool-steps-and-successors pds)
							   (pds~open-node! last-stp-node pds)
							   NIL NIL NIL pds (prob~proof-steps pds))
				 (let* ((the-meth-nodes (remove-duplicates (append (union meth-nodes pseudog-nodes))))
					(restricted-nodes (intersection (set-difference open-nodes the-meth-nodes)
									(prob~proof-steps pds)))
					(the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
				   (if the-other-nodes
				       (progn (omega~error ";;;plan=back-last-step! must be extended: 2")
					      (error "BACK-LAST-STEP"))
				     (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
					    (mnodes-tasks (if pds-rootp
							      (cons root-new-task
								    (mapcar #'agenda~create-task the-meth-nodes))
							    (mapcar #'agenda~create-task the-meth-nodes)))
					    (rnodes-tasks (mapcar #'agenda~create-task restricted-nodes))
					    (new-agenda (cond ((rest mnodes-tasks)
							       (agenda~create NIL (union mnodes-tasks rnodes-tasks)
									      (when rnodes-tasks
										(list (agenda~ordering-create mnodes-tasks
													      rnodes-tasks)))
									      (pds~agenda pds)))
							      (T
							       (agenda~create (first mnodes-tasks) rnodes-tasks
									      NIL (pds~agenda pds))))))
				       (setf (pds~agenda pds) new-agenda)
				       (pds~reset-schematic-nodes! pds new-agenda))))))))))))))
	(T
	 (omega~error ";;;plan=back-last-step! must be extended: 3")
	 (error "BACK-LAST-STEP"))
	))


(defun plan=merge-task-alternatives (task-alternatives)
  (declare (edited  "15-APR-1998")
	   (authors Lassaad)
	   (input   "A list of task alternatives, and a list of methods, where"
		    "a task alternative is either a task, or a tuple consisting"
		    "of a task, a method list or a method, a parameter list, and"
		    "a list of support nodes.")
	   (effect  "None.")
	   (value   "A tuple: A task, a list of methods and method with control"
		    "informations as parameters and/or supports to be considered"
		    "first to carry out this task, and a list of methods to be"
		    "considered next."))
  ;(plan~trace "Tasks: ~S  ~%methods:~S " task-alternatives methods)
  (if task-alternatives
      (let ((task-alternative1 (first task-alternatives)))
	(if (agenda~pseudo-goal-p task-alternative1)
	    ;;; The first chosen alternative by CRI is a pseudo task to be refined:
	    (values task-alternative1)
	  ;;; The first chosen alternative by CRI is a task to be closed. This can be
	  ;; either a task or a task with additional control informations:
	  (let* ((task (if (listp task-alternative1) (first task-alternative1)
			 task-alternative1))
		 (task-node (agenda~task-node task))
		 (failed-methods (pdsn~failed-methods task-node))
		 (excluded-methods (pdsn~excluded-methods task-node))
		 ;(rest-methods (cond ((or (pdsn~alternative-methods task-node)
		 (relevant-methods (cond ((or (pdsn~alternative-methods task-node)
					      (pdsn~alternative-mmatchings task-node)
					      failed-methods)
					  ;; When TASK-NODE was considered, i.e., it has alternative
					  ;; methods, or alternative mmatchings, or failed methods, then
					  ;; we have to consider only the alternative methods.
					  (pdsn~alternative-methods task-node))
					 ((or (agenda~goal-p task)
					      (and (agenda~goal-schema-p task)
						   (not (agenda~goal-schematic-p task))))
					  meth*planning-methods)
					 (T ;; TASK is schematic
					  meth*MOReasoning-methods)))
		 
				 
				; methods))
		 (task-methods))
	    ;;; We have to merge the control informations in TASK-ALTERNATIVES for achieving TASK: 
	    (dolist (alternative (reverse task-alternatives))
	      ;; LC all rest methods which are not considered in the
	      ;; TASK-ALTERNATIVES are rejected
	      (when (and (listp alternative)
			 (eq task (first alternative)))
		(let ((meth-sym (second alternative))
		      (alter-meths)
		      (alter-param (third alternative))
		      (alter-supps (fourth alternative)))
		  (when meth-sym
		    (if (symbolp meth-sym)
			(let ((some-meth (meth~find-method meth-sym)))
			  (if some-meth (setq alter-meths some-meth)
			    (let ((methods (eval meth-sym))
			          ;;;LC: EVAL soll erstmal bleiben, weil CRI liefert symbole die nicht
				  ;; ausgewertet sind.
				  )
			      (if (and methods (every #'meth~p methods))
				  (setq alter-meths methods)
				(return-from plan=merge-task-alternatives
				  (omega~error ";;;plan=merge-task-alternatives: Unknown method alternative ~A!"
					       meth-sym))))))
		      (if (meth~p meth-sym) (setq alter-meths meth-sym)
			(if (listp meth-sym)
			    (let ((methods (mapcar #'meth~find-method meth-sym)))
			      (if (some #'null methods)
				  (return-from plan=merge-task-alternatives
				    (omega~error ";;;plan=merge-task-alternatives: Unknown method alternative ~A!"
						 meth-sym))
				(setq alter-meths methods)))
			  (return-from plan=merge-task-alternatives
			    (omega~error ";;;plan=merge-task-alternatives: Unknown method alternative ~A!"
					 meth-sym))))))
		  (cond ((consp alter-meths)
			 ;; ALTER-METHS is a non-empty list of methods:
			 (cond (alter-param
				;; Methods with parameters:
				(dolist (meth (reverse alter-meths))
				  (unless (find (cons meth alter-param) (union failed-methods excluded-methods)
						:test #'plan=parametrized-method-eq)
				    ;; No method with similar parameters belongs to the failed methods:
				    (when (find meth relevant-methods)
				      (push (list meth alter-param alter-supps) task-methods)))))
			       (T ;; Methods without parameters:
				(dolist (meth (reverse alter-meths))
				  (unless (find meth (union failed-methods excluded-methods))
				    (when (find meth relevant-methods)
				      ;; METH does not belong to failed-methods and is in relevant-methods:
				      (if alter-supps
					  ;; The supports for METH are restricted, for completness we have to
					  ;; consider METH in rest-methods with all possible supports. 
					  (push (list meth nil alter-supps) task-methods)
					;; METH has to be considered with all possible supports, it is therefore
					;; deleted from the rest-methods:
					(and (push meth task-methods)
					     (setq relevant-methods (remove meth relevant-methods))))))))))
			((and (meth~p alter-meths) (find alter-meths relevant-methods))
			 ;; ALTER-METHS is a method:
			 (cond (alter-param
				;; Method with parameters:
				(unless (find (cons alter-meths alter-param) (union failed-methods excluded-methods)
					      :test #'plan=parametrized-method-eq)
				  (push (list alter-meths alter-param alter-supps) task-methods)))
			       (T ;; Method without parameters:
				(unless (find alter-meths (union failed-methods excluded-methods))
				  (if alter-supps
				      (push (list alter-meths nil alter-supps) task-methods)
				    (and (push alter-meths task-methods)
					 (setq relevant-methods (remove alter-meths relevant-methods))))))))
			
			(alter-supps ;; ALTER-METHS is nil and ALTER-PARAM is nil, only ALTER-SUPPS has to be not nil:
			 (dolist (meth (reverse relevant-methods))
			   ;; The supports for TASK are restricted, the relevant-methods must be considered
			   ;; first with restricted supports and then with all supports:
			   (push (list meth nil alter-supps) task-methods)))))))
	    (if task-methods
		(values task task-methods)
	      (values task nil relevant-methods)))))
    ;;; Ausklammern von Warning, weil cri~call liefert zur Zeit ((task)) als Alternative:
;		      (if alter-supps
;                          (dolist (meth rest-methods)
;                            ;;; The supports for TASK are restricted, the rest-methods must be considered
;                            ;; first with restricted supports and then with all supports:
;                            (push (list meth nil alter-supps) task-methods))
;                        (omega~warn ";;;plan=merge-task-alternatives: strange task alternative ~A returned by CRI!"
;                                    alternative))))))))))
    (omega~warn ";;;plan=merge-task-alternatives: An empty task alternative list returned by CRI!")
    ))

(defun plan=intersection (list1 list2)
  (remove-if-not #'(lambda (elt) (find elt list2)) list1))


(defun plan=parametrized-method-eq (meth&param1 meth&param2)
  (declare (edited  "16-APR-1998")
	   (authors Lassaad)
	   (input   "Two parametrized methods, where a parametrized method corresponds"
		    "to a list, its first element is a method and the rest elements are"
		    "the parameters for this method.")
	   (effect  "None.")
	   (value   "T, iff both methods and both parameter lists are equal."))
  (and (listp meth&param1) (listp meth&param2)
       (eq (first meth&param1) (first meth&param2))
       (keim~equal (rest meth&param1) (rest meth&param2))))
				
		    
;;; Backtracking: TASK AGENDA PDS:
;; TASK ist open und kann nicht geschlossen werden:
;; Betrachte den Knoten N, der TASK-node eingefuehrt hat. Nehme die method(N)
;; zurueck und wende den naechsten alternative-mmatching an, falls welche
;; gibt. Sonst, falls es noch alternative-methods gibt, dann trage die entsprechende
;; Methode method(N) in den failed-methods ein und erzeuge eine neue Agenda mit neuem
;; task(N).
;; Bei Methoden mit parametern, wird die Methode mit den Parametern in die
;; failed-methods eingetragen.
;; Wenn ein Task wieder in einem Planning step betrachtet wird, dann muesste die
;; alternative-mmatchings leer sein und die alternative-methods was enthalten.
;; Falls die alternative-methods leer ist, dann heisst das dieser Task wurde
;; noch nicht betrachtet. Hier soll man alle Methoden einbeziehen.
						    

;;;;LC-change:
;; Constraint evaluation is done within the evaluation of the application conditions:
;; Let Cm be the constraint resulted from evaluating the method application condition,
;; let CSpds = (Cpds,Bpds) the PDS constraint state before applying the method, and let
;; CMS = (MC,MB) the constraint state resulted from merging Cm with the constraint of
;; CSpds, i.e, CSpds.Cpds: CMS.MB is not NIL means that this method application leads
;; to the binding of some meta-variables. The binding of these meta-variables is considered
;; when carrying out the proof step:
;;   - in the creation of new pds objects, by applying the binding to the codomain of mmapp-subst
;;   - to update the schematic goals and possibly restrict them
;;   - to create the new pds constraint state CSpds' = (MC, compose(Bpds,MB))
;;; meth~check-condition (c,mmapp) returns a mmapp (or mmapp-s) with constraint state as constraint
;; It calls meth=check-condition(c,mmapp), the returned results can have constraints.
;; These constraints are merged with the constraint of the pds constraint state and the 
;; result of this function call should a pair of binding-constraints and other constraints.
;; These are then used to create a constraint state. 

(defun plan~match-method (method open-node open-formula supports pds &key
				 ((:parameters parameters)) ((:invoke-cri invoke-cri)))
  (declare (edited  "09-MAR-1998")
	   (authors Lassaad)
	   (input   "A method, an open node, its current formula, a list of support nodes,"
		    "a pds whom belongs the objet nodes, and optionally a list of parameters.")
	   (effect  "None.")
	   (value   "A list of PLAN+METH-MATCHINGs repsenting the alternatives of matching"
		    "METHOD to OPEN-NODE and SUPPORTS under the constraints of PDS using the"
		    "given parameters."))
  ;;;NEW: 1) methods may have parameters which dont have to be provided by the CRI. Such parameters
  ;; must be being bound just before applying the method. Parameters are needed for control
  ;; purposes. For instance, we want to see how a schematic goal was just before being closed
  ;; by some method. Formulas for schematic goals could not correspond to the formula of the
  ;; associated node, for insance N: MV(t) and the formula for the associated task T_N is
  ;; p(t). The information that the head of the formula of T_N was p is sometimes needed for
  ;; control purposes, therefore we have to store this as a parameter for the applied method
  ;; Aseertion2RA (see the control rule reject-loop-support-for-Assertion2RA which prevents to
  ;; apply the same assertion in similar contexts (same predicate) on the same proof tree path.
  ;;      2) Parameters may be partly given by the caller of plan~match-method, or by the CRI  
  ;; during the selection of the supports.
  (plan~trace "~%~%... Trying to match ~S on ~S and ~S" method open-node supports)
  (setf plan*matching-attempts
	(+ plan*matching-attempts 1))
  
  (let ((meth-parameters (meth~parameters method))
	(mmapps1))
    ;;; Consider first the method parameters:
    (if meth-parameters
	;;; Match the given PARAMETERS to the first associated METH-PARAMETERS, the rest METH-PARAMETERS
	;; will be bound later
	(let ((mmapps (meth~match-p (subseq meth-parameters 0 (length parameters))
				    parameters
				    (meth~mapping-create (subst~create nil nil)
							 (mapp~create nil nil))
				    :one2one)))
	  (if mmapps
	      (setq mmapps1 mmapps)
	    (return-from plan~match-method
	      (plan~trace ";;;Delivering inconsistent parameters ~A!~%" parameters))))
      (if parameters
	  (return-from plan~match-method
	    (plan~trace ";;;Delivering parameters ~A for a method without parameters!~%"
			parameters))
	(setq mmapps1 (meth~mapping-create (subst~create nil nil)
					   (mapp~create nil nil)))))
    ;;; Trying to match the outlines, and to check the application conditions:
    (setf meth*current-method method)
    (let ((meth-appl-cond (meth~application-condition method))
	  (meth-goal (meth~goal method)))
      (if meth-goal
	  ;;; A backward METHOD:
	  (let ((mmapps2 (meth~match-p meth-goal open-node mmapps1 :one2one open-formula)))
	    (when mmapps2
	      (let ((exist-prems (meth~existent-premises method))
		    (clsed-prems (meth~closed-premises method)))
		(if (or exist-prems clsed-prems)
		    (let ((cri-supports (when invoke-cri (cri~call supports 
								   :kind 'supports
								   :task-node open-node
								   :task-formula open-formula
								   :method method
								   :pds pds)))
			  mmapps&prems result)
		      ;; CRI-SUPPS ::= '(' {PARAM-SUPPS} ')'
		      ;; PARAM-SUPPS ::= '(' [PARAMS] {NODE} ')' | {NODE}
		      ;; PARAMS ::= '(' {PARAM} ')'
		      (cond ((or (not invoke-cri) (eq cri-supports supports))
			     ;; The CRI is not invoked or no control information updates SUPPORTS
			     (let ((mmapps3 (meth~match-p (append exist-prems clsed-prems)
							  supports mmapps2 :many2many)))
			       (setq mmapps&prems (when mmapps3
						    (plan=consistent-premises open-node mmapps3
									      (mapcar #'keim~name exist-prems)
									      (mapcar #'keim~name clsed-prems))))))
			    (T ;; work off the alternative supports
			     (let (all-mmapps)
			       (cond ((every #'pdsn~p cri-supports)
				      ;; supports without parameters
				      (setq all-mmapps
					    (meth~match-p (append exist-prems clsed-prems)
							  cri-supports mmapps2 :many2many)))
				     (T ;; support combinations possibly with parameters:
				      (dolist (param-supps cri-supports)
				        (let ((params-or-supp (first param-supps)))
					  (if (consp params-or-supp)
					      (let ((mmapps3 (meth~match-p (subseq meth-parameters 0 (length params-or-supp))
									   params-or-supp mmapps2 :one2one)))
						(setq all-mmapps
						      (append all-mmapps
							      (meth~match-p (append exist-prems clsed-prems)
									    (rest param-supps) mmapps3 :many2many))))
					    (setq all-mmapps
						  (append all-mmapps
							  (meth~match-p (append exist-prems clsed-prems)
									param-supps mmapps2 :many2many))))))))
			       (setq mmapps&prems (when all-mmapps
						    (plan=consistent-premises open-node all-mmapps
									      (mapcar #'keim~name exist-prems)
									      (mapcar #'keim~name clsed-prems)))))))
		      (dolist (mmapp&prems mmapps&prems result)
			(let ((mmapp (meth~check-condition meth-appl-cond (first mmapp&prems) pds)))
			  (when mmapp
			    (if (consp mmapp)
				(setq result
				      (append result
					      (mapcar #'(lambda (mm)
							  (plan=mmatching-create
							   method (meth~compute-parameters meth-parameters mm)
							   mm open-node (second mmapp&prems) (third mmapp&prems)))
						      mmapp)))
			      (setq result
				    (cons (plan=mmatching-create
					   method (meth~compute-parameters meth-parameters mmapp) mmapp
					   open-node (second mmapp&prems) (third mmapp&prems))
					  result)))))))
		  ;;; Backward METHOD without existent and closed premises:
		  (let ((mmapps3 (meth~check-condition meth-appl-cond mmapps2 pds)))
		    (mapcar #'(lambda (mmapp)
				(plan=mmatching-create method (meth~compute-parameters meth-parameters mmapp)
						       mmapp open-node))
			    mmapps3))))))
	;;; A forward METHOD
	(let ((exist-prems (meth~existent-premises method))
	      (clsed-prems (meth~closed-premises method)))
	  (when (or exist-prems clsed-prems)
	    ;;; With existent and/or closed premises:
	    (let* ((the-supports (if invoke-cri
				     (cri~call supports
					       :kind 'supports
					       :task-node open-node
					       :task-formula open-formula
					       :method method
					       :pds pds)
				   supports))
		   (mmapps2 (meth~match-p (append exist-prems clsed-prems)
					  the-supports mmapps1 :many2many))
		   (mmapps3 (when mmapps2 (meth~check-condition meth-appl-cond mmapps2 pds))))
	      (mapcar #'(lambda (mmapp)
			  (plan=mmatching-create method (meth~compute-parameters meth-parameters mmapp) mmapp nil
						 (mapcar #'(lambda (meth-node)
							     (meth~mapp-get-component (keim~name meth-node)
										      mmapp :mapp))
							 exist-prems)
						 (mapcar #'(lambda (meth-node)
							     (meth~mapp-get-component (keim~name meth-node)
										      mmapp :mapp))
							 clsed-prems)))
		      mmapps3))))))
    ))


(defun plan=consistent-premises (open-node mmapps existp-labels clsedp-labels)
  (declare (edited  "16-MAR-1998")
	   (authors Lassaad)
	   (input   "A pds open node, a mmappitution list, and two lists of"
		    "method node labels: of existent nodes and of closed"
		    "nodes. These labels must be bound within the elements"
		    "of MMAPPS.")
	   (effect  "None.")
	   (value   "A list of triples: for each mmappitution, where the"
		    "associated premises are consistent with the OPEN-NODE,"
		    "i.e., each premise does not contain the goal OPEN-NODE"
		    "in its justifyings and has a hypothesis list which is"
		    "a subset of the goal hypotheses, this mmappitution, the"
		    "list of existent premises, and the list of closed premises"
		    "are returned together in a triple."))
  (when mmapps
    (let* ((mmapp (first mmapps))
	   (exist-prems (meth~mapp-get-component existp-labels mmapp :mapp))
	   (clsed-prems (meth~mapp-get-component clsedp-labels mmapp :mapp))
	   (goal-prems (append exist-prems clsed-prems))
	   (goal-hyps (pdsn~hyps open-node)))
      (if (and (every #'(lambda (prem) (subsetp (pdsn~hyps prem) goal-hyps))
		      goal-prems)
	       (not (find open-node (pdsn~justifying-nodes goal-prems))))
	  (cons (list mmapp exist-prems clsed-prems)
		(plan=consistent-premises open-node (rest mmapps) existp-labels clsedp-labels))
	(plan=consistent-premises open-node (rest mmapps) existp-labels clsedp-labels)))
    ))

;;;LC: This function corresponds to the expansion function of a planning method
;; Remark: In this function you have to use the current meta-variable bindings
;; while creating new-nodes:
(defun plan=refine-task (task agenda pds)
  (declare (edited  "17-MAR-1998")
	   (authors Lassaad)
	   (input   "A pseudo task, an agenda, and a pds.")
	   (effect  "Changes PDS by expanding the method in TASK.")
	   (value   "The changed agenda."))
  (let* ((task-node (agenda~task-node task))
	 (node-just (node~justification task-node))
	 (just-meth (just~method node-just)))
    (if (infer~method-p just-meth)
	(multiple-value-bind (new-nodes success new-cstr-state)
	    (pexp~expand-method task-node pds)
	  ;; - Check whether new-nodes was inserted in PDS
	  ;; - Check whether the new-nodes have the own reason of TASK, i.e., the reason
	  ;; which corresponds to the expanded method.
	  ;; - Check whether a new planning step is inserted 
	  ;; - Replace TASK with the NEW-TASKS which correspond to the nodes
	  ;; in NEW-NODES that are open or that are justified by a unreliable
	  ;; method.
	  ;; - In case of new open nodes, inheritance of supports?
	  ;; - In case of orderings, the orderings must be interpreted too
	  ;; - In case of NEW-CSTR-STATE with SUBST, i.e., bindings for
	  ;; meta-variables, the following should be done:
	  ;; A: SUBST should be composed with the previous SUBST
	  ;; B: SUBST should be applied to the pseudo-goals on AGENDA and
	  ;; the schematic supports of the goals on AGENDA must be considered
	  ;; as new supports, normalizing methods can be applied to the current
	  ;; instance of these supports. 
	  ;; IMPLEMENT the needed stuff for the expansion of BH-AndE
	  (cond ((not success)
		 ;; TASK-NODE cannot be expanded, since the expansion condition is evaluated to NIL.
		 ;; This is the case when the expansion condition is not compatible with the current
		 ;; constraint state. Therefore we must proceed as follows:
		 ;; CASE A: JUST-METH applied to TASK-NODE in a planning step which preceeds the last
		 ;; planning step that affected the constraint state, then:
		 ;; - take back the last planning step that affected the constraint state
		 ;; - call plan=refine-task recursively with the new agenda and new PDS
		 ;; CASE B: The last planning step that affected the constraint pool is before the
		 ;; application of JUST-METH, then
		 ;; - take back JUST-METH
		 (when plan*trace
		   (print "Expansion result:")
		   (plan=show-agenda "~%Agenda:~%~A" (pds~agenda pds))
		   (format T "~%The node ~A ~%The steps ~A" task-node (pds~plan-steps pds)))
		 (let ((own-reason (pdsj~own-reason node-just)))
		   (cond ((not own-reason)
			  (omega~error ";;;plan=refine-task Something went wrong in the expansion of ~A closed in a previous expansion."
				       task-node)
			  (error "Extend plan=refine-task"))
			 (T ;; The method to be expanded was applied to close TASK-NODE:
			  (let* ((pds-cstr (pds~constraint-pool pds))
				 (cstr-steps (when pds-cstr (pds~cstrpool-plansteps pds-cstr)))
				 (task-node-succs (pdsj~successors node-just)))
			    (cond ((and cstr-steps (intersection cstr-steps task-node-succs))
				   ;; CASE A:
				   (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds)))
				     (cond ((plan=expansion-step-p (first cstr-steps))
					    ;; CSTR-STP is an expansion step:
					    ;; - Remove this expansion --> expanded-node
					    ;; i) expanded-node is closed after TASK-NODE, then open this node, reset its
					    ;; control and order it after TASK
					    ;; ii) O/w: open TASK-NODE and exclude the applied method.
					    (let* ((expanded-node (plan=remove-expansion! (first cstr-steps) pds))
						   (exp-node-stp (pdsj~own-reason (node~justification expanded-node))))
					      (cond ((not exp-node-stp)
						     (omega~error
						      ";;;plan=refine-task must be extended to handle recursive expansions!")
						     ;; Principle: One applied method must be taken back either that of TASK-NODE
						     ;; or that of EXPANDED-NODE
						     (error "Extend plan=refine-task"))
						    ((find exp-node-stp task-node-succs)
						     (let ((exp-node (pdsc~an-node exp-node-stp))
							   (exp-just (pdsc~an-just exp-node-stp)))
						       ;; Pass the control information of EXP-NODE
						       (unless (pdsn~alternative-mmatchings exp-node)
							 (multiple-value-bind (outln-pat)
							     (plan=actual-outline-pattern exp-node pds exp-just)
							   (let ((the-method (pds~inference-application
									      (just~method exp-just) outln-pat)))
							     (if (pdsj~parameters exp-just)
								 (setf
;								       (pdsn~excluded-methods exp-node)
;                                                                       (cons (cons the-method (pdsj~parameters exp-just))
;                                                                             (pdsn~excluded-methods exp-node))
								       (pdsn~failed-methods exp-node) NIL
								       (pdsn~alternative-methods exp-node) NIL)
;								       (cons (cons the-method (pdsj~parameters exp-just))
;                                                                             (pdsn~failed-methods exp-node)))
							       (setf
;								(pdsn~excluded-methods exp-node)
;                                                                (cons the-method (pdsn~excluded-methods exp-node))
								     (pdsn~failed-methods exp-node) NIL
								     (pdsn~alternative-methods exp-node) NIL
;								     (cons the-method (pdsn~failed-methods exp-node))
								     )))))
						       ;; Open EXP-NODE and make PDS and AGENDA consistent
						       (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
							   (plan=consistent-opens! cstrsteps2succs
										   (cons (first cstr-steps)
											 (pds~open-node! exp-node pds))
										   NIL (list exp-node) NIL
										   pds (prob~proof-steps pds))
							 (let* ((the-meth-nodes
								 (remove-duplicates (union meth-nodes pseudog-nodes)))
								(the-other-nodes
								 (remove-duplicates (set-difference other-nodes the-meth-nodes))))
							   (if the-other-nodes
							       (let ((extra-supps))
								 (dolist (each-node the-other-nodes)
								   (setq extra-supps
									 (union extra-supps
										(plan=set-difference
										 (pdsn~just-sponsors each-node)
										 (pdsn~just-unsponsors each-node)))))
								 (multiple-value-bind (the-agenda rest-opens)
								     (plan=normalize-supports extra-supps the-other-nodes agenda pds)
								   (let* ((root-new-task
									   (when pds-rootp (agenda~create-goal pds-rootp)))
									  (new-tasks
									   (if pds-rootp
									       (cons
										root-new-task
										(append (mapcar #'agenda~create-task the-meth-nodes)
											(mapcar #'agenda~create-goal rest-opens)))
									     (append (mapcar #'agenda~create-task the-meth-nodes)
										     (mapcar #'agenda~create-goal rest-opens))))
									  (new-agenda
									   (pds~reset-schematic-nodes!
									    pds
									    (if new-tasks
										(agenda~insert-tasks
										 task nil new-tasks
										 (list (agenda~ordering-create
											(list task) new-tasks))
										 the-agenda)
									      the-agenda))))
								     (setf (pds~agenda pds) new-agenda)
								     new-agenda)))
							     
							     (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
								    (new-tasks (if pds-rootp
										   (cons root-new-task
											 (mapcar #'agenda~create-task the-meth-nodes))
										 (mapcar #'agenda~create-task the-meth-nodes)))
								    (new-agenda (pds~reset-schematic-nodes!
										 pds
										 (if new-tasks
										     (agenda~insert-tasks
										      task nil new-tasks
										      (list (agenda~ordering-create
											(list task) new-tasks)) agenda)
										   agenda))))
							       (setf (pds~agenda pds) new-agenda)
							       new-agenda))))))
						    (T
						     (omega~error
						      ";;;plan=refine-task must be extended!")
						     ;; Principle: Open TASK-NODE
						     (error "Extend plan=refine-task")))))
					   (T ;; CSTR-STP is a method application:
					    (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
						(let ((to-open (pdsc~an-node (first cstr-steps))))
						  ;; Reset the control of TO-OPEN:
						  (setf (pdsn~alternative-mmatchings to-open) NIL
							(pdsn~alternative-methods to-open) NIL)
						  (plan=consistent-opens! cstrsteps2succs (pds~open-node! to-open pds)
									  NIL (list to-open) NIL pds (prob~proof-steps pds)))
					      (let* ((the-meth-nodes (remove-duplicates (union meth-nodes pseudog-nodes)))
						     (the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
						(if the-other-nodes
						    (let ((extra-supps))
						      (dolist (each-node the-other-nodes)
							(setq extra-supps
							      (union extra-supps
								     (plan=set-difference (pdsn~just-sponsors each-node)
											  (pdsn~just-unsponsors each-node)))))
						      (multiple-value-bind (the-agenda rest-opens)
							  (plan=normalize-supports extra-supps the-other-nodes agenda pds)
							(let* ((root-new-task
								(when pds-rootp (agenda~create-goal pds-rootp)))
							       (new-tasks
								(if pds-rootp
								    (cons root-new-task
									  (append (mapcar #'agenda~create-task the-meth-nodes)
										  (mapcar #'agenda~create-goal rest-opens)))
								  (append (mapcar #'agenda~create-task the-meth-nodes)
									  (mapcar #'agenda~create-goal rest-opens))))
							       (new-agenda
								(pds~reset-schematic-nodes!
								 pds
								 (if new-tasks
								     (agenda~insert-tasks
								      task nil new-tasks
								      (list (agenda~ordering-create (list task) new-tasks))
								      the-agenda)
								   the-agenda))))
							  (setf (pds~agenda pds) new-agenda)
							  new-agenda)))
;						  
;                                                          (plan=refine-task task new-agenda pds))))
						  (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
							 (new-tasks (if pds-rootp
									(cons root-new-task
									      (mapcar #'agenda~create-task the-meth-nodes))
								      (mapcar #'agenda~create-task the-meth-nodes)))
							 (new-agenda (pds~reset-schematic-nodes!
								      pds
								      (if new-tasks
									  (agenda~insert-tasks
									   task nil new-tasks
									   (list (agenda~ordering-create (list task)
													 new-tasks))
									   agenda)
									agenda))))
						    (setf (pds~agenda pds) new-agenda)
						    new-agenda))))))))
				  
;					            (plan=refine-task task new-agenda pds)))))))))
				  (T ;; CASE B:
				   (let ((cstrsteps2succs (stp~cstrpool-steps-and-successors pds)))
				     (multiple-value-bind (meth-nodes other-nodes pseudog-nodes pds-rootp)
					 (plan=consistent-opens! cstrsteps2succs
								 (pds~open-node! task-node pds)
								 NIL NIL NIL pds (prob~proof-steps pds))
				       (let* ((the-meth-nodes (remove-duplicates (union meth-nodes pseudog-nodes)))
					      (the-other-nodes (remove-duplicates (set-difference other-nodes the-meth-nodes))))
					 (if the-other-nodes
					     (let ((extra-supps))
					       (dolist (each-node the-other-nodes)
						 (setq extra-supps
						       (union extra-supps
							      (plan=set-difference (pdsn~just-sponsors each-node)
										   (pdsn~just-unsponsors each-node)))))
					       (multiple-value-bind (the-agenda rest-opens)
						   (plan=normalize-supports extra-supps the-other-nodes agenda pds)
						 (let* ((root-new-task
							 (when pds-rootp (agenda~create-goal pds-rootp)))
							(new-tasks
							 (if pds-rootp
							     (cons root-new-task
								   (append (mapcar #'agenda~create-task the-meth-nodes)
									   (mapcar #'agenda~create-goal rest-opens)))
							   (append (mapcar #'agenda~create-task the-meth-nodes)
								   (mapcar #'agenda~create-goal rest-opens))))
							(new-agenda
							 (pds~reset-schematic-nodes!
							  pds (agenda~replace-task task nil new-tasks nil the-agenda))))
						   (setf (pds~agenda pds) new-agenda)
						   new-agenda)))
					   (let* ((root-new-task (when pds-rootp (agenda~create-goal pds-rootp)))
						  (new-tasks (if pds-rootp
								 (cons root-new-task
								       (mapcar #'agenda~create-task the-meth-nodes))
							       (mapcar #'agenda~create-task the-meth-nodes)))
						  (new-agenda (pds~reset-schematic-nodes!
							       pds (agenda~replace-task task nil new-tasks nil agenda))))
					     (setf (pds~agenda pds) new-agenda)
					     new-agenda))))))))))))
		
		(T ;; Successful expansion:
		 (let ((expansion-step (pds~change-last-plan-step! task-node pds))
		       (an-agenda agenda)
		       the-new-tasks)
		   ;; EXPANSION-STEP is created only to note that the expansion is carried out during
		   ;; the planning phase. When the expansion step changes the PDS constraint state, then
		   ;; we use EXPANSION-STEP as the reason for this changement.
		   ;; The reson which we insert into the NEW-NODES corresponds to the step of the method
		   ;; application which led to this expansion. 
		   
		   (when new-nodes
		     (omega~error ";;;plan=refine-task must be extended to handle new nodes."))
		   
		   (when new-cstr-state
		     (setf (pds~cstrpool-plansteps new-cstr-state) (list expansion-step)
			   (pds~constraint-pool pds) new-cstr-state)
		     (let ((new-mvar-subst (pds~cstrpool-bindings new-cstr-state)))
		       (cond ((not (subst~empty-p new-mvar-subst))
			      ;; - possibly composing its mvar-subst with the previous mvar-subst
			      ;; - associating the changement of the cstr-state to this expansion step
			      (let ((previous (pds~cstrpool-previous new-cstr-state)))
				;; Application of new-subst to the old-subst:
				;; - Is there a non-empty old-susbt, then compose new-subst with old-subst
				;; - Is there no non-empty old-susbt, then take old-subst
				(when (and (pds~constraint-pool-p previous)
					   (not (subst~empty-p (pds~cstrpool-bindings previous))))
				  (setf (pds~cstrpool-bindings new-cstr-state)
					(subst~compose-substitution (pds~cstrpool-bindings new-cstr-state)
								    (pds~cstrpool-bindings previous)))))
			      
			      ;; Reset the UP-TO-DATE flags of relevant nodes, since some meta-variables
			      ;; are bound during the expansion. The called function returns tasks which
			      ;; were bound before the expansion, and tasks which are bound due to the
			      ;; meta-variable bindings caused by the expansion:
			      (multiple-value-bind (previously-bound-tasks newly-bound-tasks)
				  (pds~update-schematic-nodes! pds)
				;; B: Seeing whether the new instantiations allow to close some goals
				;; by task-restricting methods, or impose to normalize some schematic
				;; supports:
				(multiple-value-bind (new-agenda new-tasks)
				    (plan=restrict-tasks6 previously-bound-tasks newly-bound-tasks NIL NIL agenda pds)
				  (setq an-agenda new-agenda
					the-new-tasks new-tasks))))
			     ((pds~cstrpool-previous new-cstr-state)
			      ;; No meta-variables are bound by this method expansion: take the previous MVAR-SUBST
			      (setf (pds~cstrpool-bindings new-cstr-state)
				    (pds~cstrpool-bindings (pds~cstrpool-previous new-cstr-state)))))))
		   (agenda~replace-task task NIL the-new-tasks NIL an-agenda)
		   ))))
	  
      (if (infer~bbox-p just-meth)
	  (let ((new-nodes (infer~apply-expansion-function
			    just-meth
			    (oc~build-outline task-node
					      (pdsj~outline-pattern node-just)
					      (just~premises node-just))
			    (pdsj~parameters node-just)))
		(new-pseudos))
	    (dolist (node new-nodes)
	      (when (plan=pseudo-closed-p node pds)
		(setq new-pseudos (cons (funcall #'agenda~create-pseudo-goal node)
					new-pseudos))))
	    (when (plan=pseudo-closed-p task-node pds)
	      (setq new-pseudos (cons (funcall #'agenda~create-pseudo-goal task-node)
				      new-pseudos)))
	    (agenda~replace-task task nil new-pseudos nil agenda))
	(error "~%Node ~A justified by ~A may not be considered as a pseudo task!" task-node just-meth)))
    ))

(defun plan=remove-expansion! (exp-stp pds)
  (declare (edited  "27-APR-1999")
	   (authors Lassaad)
	   (input   "A method expansion step, and a PDS.")
	   (effect  "Removes the nodes and justifications inserted by EXP-STP and"
		    "recursively all the expansions which led to the EXP-STP.")
	   (value   "The node, which was closed by the application of some method"
		    "whose expansion corresponds to EXP-STP or lead to EXP-STP."))
  (let ((exp-just (pdsc~an-just exp-stp))
	(exp-node (pdsc~an-node exp-stp)))
    (cond ((pdsj~own-reason exp-just)
	   (omega~error ";;;plan=remove-expansion! ~A does not correspond to a method expansion!" exp-stp))
	  (T
	   (let ((above-own-reason (pdsj~above-own-reason exp-just)))
	     (cond (above-own-reason
		    (let ((aor-just (pdsc~an-just above-own-reason)))
		      (cond ((and (subsetp (just~premises aor-just) (just~premises exp-just))
				  (subsetp (just~premises exp-just) (just~premises aor-just)))
			     (setf (pdsj~above (pdsj~below aor-just)) NIL)
			     (setf (pdsj~below aor-just) NIL)
			     (setf (node~justification exp-node) aor-just)
			     (plan=remove-plan-step! pds exp-stp) 
			     exp-node)
			    (T
			     (omega~error ";;;plan=remove-expansion! must be extended!")))))
		   (T
		    (omega~error ";;;plan=remove-expansion! must be extended!"))))))
    ))

(defun plan=expansion-step-p (plan-stp)
  (declare (edited  "27-APR-1999")
	   (authors Lassaad)
	   (input   "A plan step.")
	   (effect  "None.")
	   (value   "T, iff PLAN-STP corresponds to a method expansion."))
  (not (pdsj~own-reason (pdsc~an-just plan-stp)))
  )

(defun plan=take-back-expansion! (exp-stp pds)
  (declare (edited  "27-APR-1999")
	   (authors Lassaad)
	   (input   "An expansion step, and a PDS.")
	   (effect  "Takes back the expansion.")
	   (value   "A list containing EXP-STP."))
  (let* ((exp-just (pdsc~an-just exp-stp))
	 (exp-node (pdsc~an-node exp-stp))
	 (above-just (pdsj~above exp-just)))
    (cond ((and (subsetp (just~premises above-just) (just~premises exp-just))
		(subsetp (just~premises exp-just) (just~premises above-just)))
	   (setf (pdsj~above exp-just) NIL)
	   (setf (pdsj~below above-just) NIL)
	   (setf (node~justification exp-node) above-just)
	   (plan=remove-plan-step! pds exp-stp)
	   (list exp-stp))
	  (T
	   (omega~error ";;;plan=take-back-expansion! must be extended!")))
    ))


(defun plan=remove-plan-step! (pds plstp)
  (declare (edited  "16-MAY-1997")
	   (authors Lassaad)
	   (input   "A pds, and one of its plan steps.")
	   (effect  "Remove PLSTP from the PDS plan steps.")
	   (value   "Unspecified."))
  (let ((last-plstp (pds~last-plan-step pds))
	(first-plstp (pds~first-plan-step pds))
	(plstp-just (pdsc~an-just plstp)))
    ;(format T "~%STEPS BEFORE:~%~A~%" (pds~plan-steps pds))
    (when (keim~equal last-plstp plstp)
      (setf (pds~last-plan-step pds) (pdsj~predecessor plstp-just)))
    (when (keim~equal first-plstp plstp)
      (setf (pds~first-plan-step pds) (pdsj~successor plstp-just)))
    (pdsj~control-connect-pred&succ! (pdsj~control plstp-just))
    ;(format T "~%STEPS AFTER:~%~A~%" (pds~plan-steps pds))
    ))



(defmethod infer~apply-expansion-function ((infer infer+supermethod) (outline list)
					   (parameters list))
  (exp~expand-method&insert-nodes (first outline) omega*current-proof-plan))

;;;LC: This function executes a method matching by inserting the new nodes, and changing
;;;  the del-conclusions and some control informations of the PDS.
(defun plan~apply-mmatching (task mmatching pds &key ((:extra-return extra-return)))
  (declare (edited  "01-APR-1998")
	   (authors Lassaad)
	   (input   "An agenda task, a method matching, a pds, and a key word which specifies"
		    "additional returns of this function.")
	   (effect  "Applies the method of MMATCHING to PDS.")
	   (value   "A tuple: MMATCHING, a list of new open nodes (method subgoals),"
		    "a list of new deduced nodes including new hypotheses, and"
		    "eventually additional returns, when this MMATCHING can be"
		    "applied, otherwise NIL."))
  (let* ((method (plan~matched-method mmatching))
	 (mmapp (plan~mmatch-mmapp mmatching))
	 (goal (plan~mmatch-goal mmatching))
	 (just-parameters (plan~mmatch-parameters mmatching))
	 (exist-prems (plan=recreate-objects (meth~real-existent-premises method)
					     mmapp))
	 (clsed-prems (plan=recreate-objects (meth~closed-premises method)
					     mmapp))
	 (cstr-state (plan~mmatch-cstr-state mmatching))
	 (meth-prems (meth~premises method))
;	 (pre-parameters (mapcar #'(lambda (mparam)
;                                     (meth~mapp-get-component mparam mmapp :both))
;                                 (meth~parameters method)))
	 ;;; The parameters, before applying meta-variable bindings to the MMAPP and
	 ;; before carrying out the computations.
	 ;; NOTE: Parameters which correspond to the state of input objects, e.g., a
	 ;; schematic goal or a meta-variable within such a goal, they must be bound
	 ;; at the latest during the evaluation of the application conditions. They
	 ;; may not be bound by an application computation.
	 )
    ;;; First: when CSTR-STATE is not NIL and its binding empty (substitution), then
    ;; this means that the constraint evaluation of METHOD engenders a binding of some pds
    ;; meta-variables. Such meta-variables may occur in the associated pds objects to the method
    ;; variables stated in MMAPP. We have therefore to apply this BINDING to this terms in order
    ;; to update them, before to create new pds objects. Determining the PARAMETERS before taking
    ;; this changement of MMAPP ensures that this PARAMETERS remain unchanged even when they contain
    ;; bounded meta-variables:
    (plan~message " The ~S is applied!!!!!!!!!!" method)
    (plan~trace "==================================")
    (incf plan*number-of-applied-methods)
    (when cstr-state
      (let ((binding (pds~cstrpool-bindings cstr-state)))
	(when (and binding (not (subst~empty-p binding)))
	  (let* ((mmapp-subst (meth~mapp-subst mmapp))
		 (updated-codom (meth~subst-apply binding (subst~codomain mmapp-subst)))
		 (updated-subst (meth~subst-create (subst~domain mmapp-subst) updated-codom)))
	    (meth~mapp-new-subst mmapp updated-subst)))))
    ;;; Second, carry out the outline-computations
    (meth~carry-out-computations (meth~outline-computations method) (meth~mapp-new-constraint mmapp T))
    (meth~mapp-new-constraint mmapp (if cstr-state cstr-state T))
    
    ;;; Then, create the non-existent outlines
    (multiple-value-bind (concs prems new-nodes)
	(meth~complete-outlines (meth~conclusions method) meth-prems mmapp)
      ;;; LC: Reparation
	(plan~trace "deletes ...")
	(dolist (prem (append clsed-prems (when goal (list goal))))
	  (plan~trace "   ~S : ~S" (keim~name prem)
		      (node~formula prem)))
	(plan~trace "and adds ...")
	(dolist (new-node new-nodes)
	  (let ((new-just (node~justification new-node)))
	    (plan~trace "   ~S : ~S |- ~S           ~S"
			(keim~name new-node)
			(mapcar #'keim~name (pdsn~hyps new-node))
			(node~formula new-node)
			(if (and (node~justification new-node)
				 (pdsn~open-node-p new-node))
			    'OPEN
			  '!))
	  (when (and new-just
		     (infer~dummy-p (just~method new-just))
		     (not (infer~open-p (just~method new-just))))
	    (setf (pdsn~hyps new-node) (list new-node)))))
	(let* ((concs-outline-pattern (mapcar #'keim~name concs))
	     
	     (new-hyps (remove-if-not #'pdsn~hypothesis-node-p
				      new-nodes))
	     (old-prems (append exist-prems clsed-prems))
	     (new-prems (plan=set-difference prems old-prems))
	     (open-exist-prems (remove-if-not #'pdsn~open-node-p exist-prems))
	     (open-prems (append new-prems open-exist-prems))
	     (new-concs (remove goal concs))
	     (supps&prems (append old-prems (plan=set-difference new-nodes concs)))
	     (additional-returns)
	     (parameters (if (some #'null just-parameters)
			     (mapcar #'(lambda (pre-param mparam)
					 (if pre-param pre-param
					   (meth~mapp-get-component mparam mmapp :both)))
				     just-parameters (meth~parameters method))
			   just-parameters))
	     (proc-cont (car (meth~procedural-content method))))
	;;; In case METHOD is a critic, we have to insert it together with the parameters into the applied-critics
	  ;; slot of all open nodes
	(when (meth~critic-p method)
	  (dolist (openn (pds~open-nodes pds))
	    (setf (pdsn~applied-critics openn)
		  (cons (cons method parameters) (pdsn~applied-critics openn)))))

	;;; Setting the justifications of the conclusions; extenting the plan steps
	;; and inserting the correponding reasons
	(dolist (conc (plan=set-difference concs new-hyps)) ;; only non-hyps get a new
	  ;; justification
	  (let ((prems-outline-pattern (meth~prems-outline-pattern meth-prems mmapp)))
	    (cond
	     ((and (plan=tactic-application? proc-cont)
		   (eq conc goal))
	      ;;; CONC was open and is now closed by METHOD:
	      ;; CONC is deduced by a forward application of the method tactic:
	      (let* ((old-just (node~justification conc))
		     (new-just (pdsj~closed-just-create (meth~inference method)
							prems parameters "expanded" mmapp
							(append (substitute "EXISTENT" (keim~name conc)
									    concs-outline-pattern)
								prems-outline-pattern))))
		(setf (node~justification conc) (pdsj~insert-just-above old-just new-just)
		      (pds~open-nodes pds) (remove conc (pds~open-nodes pds)))
		(setf (pdsj~reasons new-just) (append (pdsj~reasons new-just) (pdsj~reasons old-just)))
		(setf (pdsj~reasons old-just) nil)
 		(setf (pdsj~sponsors new-just) (pdsj~sponsors old-just))       ;; NEW VOLKER
		(setf (pdsj~unsponsors new-just) (pdsj~unsponsors old-just))   ;; NEW VOLKER
		))
	     ((plan=tactic-application? proc-cont)
	      ;; CONC is deduced by a forward application of the method tactic:
	      (let* ((old-just (node~justification conc))
		     (new-just (pdsj~closed-just-create (meth~inference method)
							prems parameters "expanded" mmapp
							(append (substitute "NONEXISTENT" (keim~name conc)
									    concs-outline-pattern)
								prems-outline-pattern))))
		(setf (node~justification conc) (pdsj~insert-just-above old-just new-just))
		(setf (pdsj~reasons new-just) (append (pdsj~reasons new-just) (pdsj~reasons old-just)))
		(setf (pdsj~reasons old-just) nil)
		(setf (pdsj~sponsors new-just) (pdsj~sponsors old-just))       ;; NEW VOLKER
		(setf (pdsj~unsponsors new-just) (pdsj~unsponsors old-just)))) ;; NEW VOLKER 
	     ((eq conc goal)
	      ;;; CONC was open and is now closed by METHOD:
	      (let* ((conc-outline-pattern (append (substitute "EXISTENT" (keim~name conc)
							       concs-outline-pattern)
						     prems-outline-pattern))
		       (new-just (pdsj~closed-just-create (meth~inference method)
							  prems parameters "unexpanded" mmapp
							  conc-outline-pattern))
		       (old-just (node~justification conc)))
		  (setf (pdsj~control new-just) (pdsj~control old-just)
			(node~justification conc) (pdsj~replace-justification! old-just new-just)
			(pds~open-nodes pds) (remove conc (pds~open-nodes pds )))))
	     (t
	    ;;; CONC is deduced by a forward application of the method tactic:
	      (let* ((conc-outline-pattern (append (substitute "NONEXISTENT" (keim~name conc)
								 concs-outline-pattern)
						     prems-outline-pattern))
		     (old-just (node~justification conc))
		     (new-just (pdsj~closed-just-create (meth~inference method)
							prems parameters "unexpanded" mmapp
							conc-outline-pattern))
		     (conc-just (if old-just
				    (pdsj~insert-just-below old-just new-just)
				  new-just))
		     )
		(setf (node~justification conc) conc-just)))))	
	  (let ((conc-reason (pds~change-last-plan-step! conc pds)))
	    (dolist (pds-node (cons conc supps&prems))
	      (pdsn~insert-reason! pds-node conc-reason))
	    (when cstr-state
	      ;;; The cstrpool of pds is changed by this method application, therefore
	      ;; we mark this by registering the corresponding reasons.
	      (setf (pds~cstrpool-plansteps cstr-state)
		    (cons conc-reason (pds~cstrpool-plansteps cstr-state))))
	    ))

	;;; Inserting the new nodes into PDS:
	;;(unless (plan=tactic-application? proc-cont)

	(dolist (node new-nodes)
	  (when (null (find node (prob~proof-steps pds)))
	    (pds~insert-node! node pds)))
	
	;; Reset the PDS current constraint pool
	(when cstr-state
	  (cond ((not (subst~empty-p (pds~cstrpool-bindings cstr-state)))
		 ;; new bindings: Compose old and new bindings, when old bindings exist
		 ;; o/w. take old bindings
		 (let ((previous (pds~cstrpool-previous cstr-state)))
		   (when (and (pds~constraint-pool-p previous)
			      (not (subst~empty-p (pds~cstrpool-bindings previous))))
		     (setf (pds~cstrpool-bindings cstr-state)
			   (subst~compose-substitution (pds~cstrpool-bindings cstr-state)
						       (pds~cstrpool-bindings previous))))))
		((pds~constraint-pool-p (pds~cstrpool-previous cstr-state))
		 (setf (pds~cstrpool-bindings cstr-state)
		       (pds~cstrpool-bindings (pds~cstrpool-previous cstr-state)))))
	  ;; Change the PDS constraint state
	  (setf (pds~constraint-pool pds) cstr-state))
	;;; Inheritance of the supports:
	;; Insert CONCS in the PDS supports and REMOVE them from the supports
	;; of the open nodes which are not supported by OLD-PREMS:
	(unless (or goal open-prems)
	  (when (and old-prems (subsetp prems (pds~support-nodes pds)))
	    ;; If all premises are closed and they occur in the pds supports, then the
	    ;; conclusions should be inserted into this node list:
	    (setf (pds~support-nodes pds) (append new-concs (pds~support-nodes pds)))
	    (dolist (on (pds~open-nodes pds))
	      (let ((on-usps (pdsj~unsponsors (node~justification on))))
		(when (and on-usps
			   (some #'(lambda (n) (find n on-usps)) old-prems))
		  (setf (pdsj~unsponsors (node~justification on))
			(union on-usps new-concs)))))
	    ))

	(cond ((plan=tactic-application? proc-cont)      ;;;  VS new stuff ;; NEW VOLKER ;; war vorher if
	       (let ((old-prems (just~premises (pdsj~below (node~justification (car concs)))))
		     (hyps (set-difference (tac~set-manage-list #'union (mapcar #'pdsn~hyps prems))
					   (tac~set-manage-list #'union (mapcar #'pdsn~hyps concs)))))
		 (tac~change-pds-structure concs old-prems hyps)
		 ;;(tac~change-reason-structure concs old-prems hyps)
		 (mapcar #'(lambda (conc)
			     (mapcar #'(lambda (hyp)
					 (pdsn~insert-reason! hyp (pdsj~own-reason (node~justification conc))))
				     hyps))
			 concs)
		 ;;(mapcar #'(lambda (conc)        ;;; that could be a source of confussion!!!!!  VS
		 ;;		(setf (pdsj~predecessor (node~justification conc)) nil)
		 ;;		(setf (pdsj~successor (node~justification conc)) nil))
		 ;;	    concs)))
		 ))
	      ((and task open-prems);; A planning step wrt. TASK
	       ;; A backward method or a forward method with open nodes:
	       ;; 1) Inheritance of the sponsors and unsponsors of the GOAL resp. the
	       ;; TASK node to the add-premises (NEW-PREMS) and then to the other open
	       ;; premises and deletion of the (-) premises from the supports of these nodes:
	       ;; 2) In the case of forward method, delete the (-) premises from the supports
	       ;; of the TASK node and add conclusions NEW-CONCS to these supports:
	       (let* ((goal-just (if goal (node~justification goal)
				   (node~justification (agenda~task-node task))))
		      (sponsors (pdsj~sponsors goal-just))
		      (unsponsors (pdsj~unsponsors goal-just)))
		 ;; 1)
		 (dolist (prem new-prems)
		   (let ((prem-just (node~justification prem)))
		     (setf (pdsj~sponsors prem-just) sponsors)
		     (setf (pdsj~unsponsors prem-just) (union unsponsors clsed-prems))))
		 (dolist (prem open-exist-prems)
		   (let ((prem-just (node~justification prem)))
		     (setf (pdsj~sponsors prem-just)
			   (union sponsors (pdsj~sponsors prem-just)))
		     (setf (pdsj~unsponsors prem-just)
			   (union (intersection unsponsors (pdsj~unsponsors prem-just)) clsed-prems))))
		 ;; 2)
		 (unless goal
		   (pds~delete-sponsors (agenda~task-node task) clsed-prems pds)
		   (pds~add-sponsors (agenda~task-node task) new-concs pds))))
	      ((and task clsed-prems)
	       ;; A forward method wrt. TASK without subgoals but with deletions:
	       ;; Delete the (-) premises from the supports of the TASK node and add
	       ;; conclusions NEW-CONCS to these supports:
	       (progn (pds~delete-sponsors (agenda~task-node task) clsed-prems pds)
		      (pds~add-sponsors (agenda~task-node task) new-concs pds))
	       ;; A forward method wrt. TASK without subgoals and without deletions:
	       ;; add the conclusions NEW-CONCS to the supports of all open nodes which are
	       ;; supported from the premises of this method:
	       (let (prems-supported-nodes)
		 (dolist (node (pds~open-nodes pds))
		   (when (subsetp old-prems (pds~node-supports node pds))
		     (setq prems-supported-nodes
			   (cons node prems-supported-nodes))))
		 (when (string-equal extra-return "PREMS-SUPPORTED-NODES")
		   (setq additional-returns prems-supported-nodes))
		 (dolist (psn prems-supported-nodes)
		   (pds~add-sponsors psn new-concs pds))))
	      ;; A planning step wrt. all tasks
	      ((and goal new-prems)
	       (omega~error ";;;plan~~apply-mmatching: You have to inherit the supports of ~A to ~A."
			    goal new-prems))
	      ;; We have to consider the open nodes different from the open premises of the
	      ;; applied method and which are supported from the old premises of this method:
	      (t (let ((nodes (plan=set-difference (pds~open-nodes pds) open-prems))
		       (prems-supported-nodes))
		   (dolist (node nodes)
		     (when (subsetp old-prems (pds~node-supports node pds))
		       (setq prems-supported-nodes
			     (cons node prems-supported-nodes))))
		   (when (string-equal extra-return "PREMS-SUPPORTED-NODES")
		     (setq additional-returns prems-supported-nodes))
		   (when prems-supported-nodes
		     ;; Inherit the sponsors and unsponsors of the PREMS-SUPPORTED-NODES to the new (open) premises:
		     (let ((sps (pdsj~sponsors (node~justification (first prems-supported-nodes))))
			   (usps (pdsj~unsponsors (node~justification (first prems-supported-nodes)))))
		       (dolist (psn (rest prems-supported-nodes))
			 (setq sps (union sps (pdsj~sponsors (node~justification psn)))
			       usps (intersection usps (pdsj~unsponsors (node~justification psn)))))
		       (dolist (np new-prems)
			 (setf (pdsj~sponsors (node~justification np)) sps
			       (pdsj~unsponsors (node~justification np)) usps)))
		     ;; Delete the (-) premises CLSED-PREMS from the supports of the PREMS-SUPPORTED-NODES
		     ;; and add the (+) conclusions NEW-CONCS to their supports:
		     (dolist (psn prems-supported-nodes)
		       (pds~delete-sponsors psn clsed-prems pds)
		       (pds~add-sponsors psn new-concs pds))))))
	;;; LC: Each new hypothesis must sponsor each add-premise that depends on it.
	;; IMPORTANT: the insertion of new hypotheses must occur after the inheritance
	;; of the goal supports to the subgoals:
	(dolist (prem new-prems)
	  (let ((sps (intersection new-hyps (pdsn~hyps prem))))
	    (when sps
	      (pds~add-sponsors prem sps pds))))

	;;; Return:
	(cond ((string-equal extra-return "PREMS-SUPPORTED-NODES")
	       (values mmatching new-prems (append new-concs new-hyps) additional-returns))
	      (T
	       (values mmatching new-prems (append new-concs new-hyps) additional-returns)))
	))))

(defun plan~apply-mmatching-with-node (conclusion mmatching pds)
  ;; needed for analogy
  (declare (edited  "04-OCT-1999")
	   (authors Cullrich)
	   (input   "An open node, a methode matching and a pds.")
	   (effect  "Applies the method of MMATCHING to the pds.")
	   (value   "The new agenda."))
  (let* ((agenda (pds~agenda pds))
	 (tasks (reverse (agenda~first-tasks! agenda)))
	 (non-schematic-task (first (member-if-not #'agenda~goal-schema-p tasks)))
	 (task (if conclusion
		   (plan~find-task-for-goal-in-agenda conclusion agenda)
		(if non-schematic-task non-schematic-task
		  (first tasks))))
	 (goal (or conclusion (agenda~task-node task)))
	 )
    (multiple-value-bind (mmatching new-opens new-supps additional-return)
	(plan~apply-mmatching task mmatching pds)
      (let ((rest-mmatchings nil)
	    (rest-methods nil))
	(let ((applied-method (plan~matched-method mmatching))
	      (resulted-cstrpool (plan~mmatch-cstr-state mmatching))
	      )
	  (if (eq goal (plan~mmatch-goal mmatching))
	      (let* ((cstr-bindings (when resulted-cstrpool
				      (pds~cstrpool-bindings resulted-cstrpool)))
		     (new-bound-tasks (when cstr-bindings
					(agenda~update-goal-schemas! agenda cstr-bindings task)))
		     (mmatching-mmapp (plan~mmatch-mmapp mmatching)))
		(plan=execute-outline-actions goal new-opens mmatching pds)
		(multiple-value-bind (new-agenda remaining-tasks)
		    (plan=not-restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
		  (let ((new-tasks (if (meth~non-reliability applied-method)
                                       (cons (agenda~create-pseudo-goal goal) remaining-tasks)
                                     remaining-tasks)))
                    (setf (pdsn~alternative-mmatchings goal) rest-mmatchings                  ;; (4)
                          (pdsn~alternative-methods goal) rest-methods)
                    (multiple-value-bind (first-task orderings)
                        (plan=create-agenda-orderings (meth~outline-orderings applied-method) ;; (5)
                                                      mmatching-mmapp new-tasks)
		      (agenda~replace-task task first-task (remove first-task new-tasks)
					   orderings new-agenda)))))
	    (let* ((cstr-bindings (when resulted-cstrpool
				    (pds~cstrpool-bindings resulted-cstrpool)))
		   (new-bound-tasks (when cstr-bindings
				      (agenda~update-goal-schemas! agenda cstr-bindings)))
		   (mmatching-mmapp (plan~mmatch-mmapp mmatching))
		   (one-conc (find-if-not #'pdsn~hypothesis-p new-supps))) ;;;LC-change
	      (plan=execute-outline-actions one-conc new-opens mmatching pds)
	      (multiple-value-bind (new-agenda remaining-tasks)
                  (plan=not-restrict-tasks new-opens new-supps new-bound-tasks agenda pds)
                (let ((new-tasks (if (meth~non-reliability applied-method)
                                     (append (mapcar #'agenda~create-pseudo-goal
                                                     (remove-if #'pdsn~hypothesis-p new-supps))
                                             remaining-tasks)
                                   remaining-tasks)))
                  (multiple-value-bind (first-task orderings)
                      (plan=create-agenda-orderings (meth~outline-orderings applied-method)
                                                    mmatching-mmapp new-tasks)
                    (agenda~insert-tasks task first-task new-tasks orderings new-agenda)
		    ))))))))))


(defun plan=recreate-objects (objects mmapp)
  (cond ((listp objects)
	 (apply #'append
		(mapcar #'(lambda (obj)
			    (plan=recreate-objects obj mmapp))
			objects)))
	((meth~meta-node-p objects)
	 (let ((inst (meth~object-instance objects mmapp)))
	   (when inst inst)))
	((meth~node-p objects)
	 (let ((inst (meth~object-instance objects mmapp)))
	   (when inst
	     (list inst))))
	(T NIL)))

#|
;;; LC: wird nirgendwo aufgerufen
(defun plan=node-ancestors (node pds)
  (cond ((null node)
	 nil)
	((listp node)
	 (apply #'append
		(mapcar #'(lambda (n)
			    (plan=node-ancestors n pds))
			node)))
	((pdsn~open-node-p node)
	 (cons node (plan=node-ancestors (pds~node-supports node pds) pds)))
	((pdsn~p node)
	 (let* ((just (node~justification node))
		(prems (when just
			 (just~premises just)))
		)
	   (if prems
	       (cons node (plan=node-ancestors prems pds))
	     (list node))))
	(T NIL)))
|#

(defun plan=remove-supp-cycle (node &optional (accu nil))
  (unless (member node accu)
    (if (pdsn~open-node-p node)
	(let* ((old-supp (pds~node-supports node))
	       (new-supp (apply #'append
				(mapcar #'(lambda (supp)
					    (plan=remove-supp-cycle supp
								    (cons node accu)))
					old-supp)))
	       (unsponsors (plan=set-difference old-supp new-supp))
	       )
	  (when unsponsors
	    (pds~delete-sponsors node unsponsors)))
      (let* ((just (node~justification node))
	     (prems (just~premises just))
	     )
	(mapcar #'(lambda (prem)
		    (plan=remove-supp-cycle prem
					    (cons node accu)))
		prems)
	(list node)))))

#|
;;; LC: wird nirgendwo aufgerufen
(defun plan=node-ancestors (node pds acc)
  (cond ((null node)
	 nil)
	((listp node)
	 (apply #'append
		(mapcar #'(lambda (n)
			    (plan=node-ancestors n pds acc))
			node)))
	((pdsn~open-node-p node)
	 (if (member node acc)
	     nil
	   (cons node (plan=node-ancestors (pds~node-supports node pds) pds (cons node acc)))))
	((pdsn~p node)
	 (if (member node acc)
	     nil
	   (let* ((just (node~justification node))
		  (prems (when just
			   (just~premises just)))
		  )
	     (if prems
		 (cons node (plan=node-ancestors prems pds (cons node acc)))
	       (list node)))))
	 (T NIL)))
|#


(defun plan=pseudo-closed-p (node &optional (pds omega*current-proof-plan))
  ;;; Sollte ueberpruefen ob der Knoten mit einer non-reliable Methode
  ;; geschlossen waere.
  (let* ((node-just (node~justification node))
	 (inference (just~method node-just)))
    (when (infer~method-p inference)
      (multiple-value-bind (outln-pat)
	  (plan=actual-outline-pattern node pds node-just)
	(meth~non-reliability (pds~inference-application inference outln-pat))))
    ))


(defun plan~find-task-for-goal-in-agenda (goal agenda)
  (when agenda
    (if (and (not (agenda~empty-p agenda))
	     (agenda~first-task agenda)
	     (keim~equal goal (agenda~task-node (agenda~first-task agenda))))
	(agenda~first-task agenda)
      (let ((result (plan=find-task-for-goal-in-next-tasks goal (agenda~next-tasks agenda)))
	    )
	(if result result
	  (plan~find-task-for-goal-in-agenda goal (agenda~then-agenda agenda)))))))


(defun plan=find-task-for-goal-in-next-tasks (goal tasks)
  (when tasks
    (if (keim~equal goal (agenda~task-node (first tasks)))
	(first tasks)
      (plan=find-task-for-goal-in-next-tasks goal (rest tasks)))))


(defun plan=process-cri-method-list (alternative-list)
  ;; removes those methods that were not selected by CRI and have the rating 0
  ;; and replace the names by the methods
  (mapcar #'(lambda (name)
	      (if (consp name)
		  (cons (meth~find-method (first name))
			(rest name))
		(meth~find-method name)))
	  (remove-if #'(lambda (method-name)
			 (let ((method (meth~find-method method-name)))
			   (and (meth~p method)
				(= 0 (meth~rating method))
				(not (cri~is-marked-p method-name)))))
		     alternative-list)))

;;**************************************************************
;; stuff for the instantiation of                              *
;;  metavars at the end of the planning process                *
;;**************************************************************
(defun plan~instantiate-metavars (pds)
  (when plan*meta-vars
    (let ((subst (plan=create-meta-subst pds))
	  )
      (when (and subst (not (subst~empty-p subst)))
	(plan~message "~%Instantiating meta-variable(s) ~S " (subst~domain subst))
	(setf plan*already-done nil)
	(plan=instantiate-pds pds
			      subst)
	))))


(defun plan=create-meta-subst (pds)
  (let* ((env (pds~environment pds))
	 (cs-subst (cosint~reflect-all-cs))
	 )
    cs-subst))


#|
(defun plan=create-meta-subst (pds)
  (let* ((env (pds~environment pds))
	 (consts (mapcar #'plan=create-new-constant
			 plan*meta-vars))
	 (cs-subst (cosint~reflect-all-cs))
	 )
    (if consts
	(let* ((const-subst (subst~create plan*meta-vars consts))
	       (new-subst (subst~compose-substitution const-subst cs-subst))
	       )
	  new-subst)
      cs-subst)))
|#
(defun plan=create-new-constant (meta)
  (let* ((meta-name (symbol-name (keim~name meta)))
	 (name (if (> (length meta-name)
		      2)
		   (subseq meta-name
			   2 (length meta-name))
		 meta-name))
	 (count 0)
	 (env (pds~environment omega*current-proof-plan))
	 )
    (if (not (env~lookup-object name env :test #'string-equal))
	(let* ((sym-name (intern name))
	       (new-const (term~constant-create sym-name
						(term~type meta)))
	       )
	  (env~enter sym-name new-const env)
	  new-const)
      (loop 
       (incf count)
       (let ((new-name (format nil "~A~A" name count)))
	 (unless (env~lookup-object new-name env :test #'string-equal)
	   (let* ((sym-name (intern new-name))
		  (new-const (term~constant-create sym-name
						   (term~type meta))))
	     (env~enter sym-name new-const env)
	     (return-from plan=create-new-constant new-const))))))))



(defun plan=create-new-variable (meta)
  (let* ((meta-name (symbol-name (keim~name meta)))
	 (name (if (> (length meta-name)
		      2)
		   (subseq meta-name
			   2 (length meta-name))
		 meta-name))
	 (count 0)
	 (env (pds~environment omega*current-proof-plan))
	 )
    (if (not (env~lookup-object name env :test #'string-equal))
	(let* ((sym-name (intern name))
	       (new-var (term~variable-create sym-name
					      (term~type meta)))
	       ) 
	  (env~enter sym-name new-var env)
	  new-var)
      (loop 
       (incf count)
       (let ((new-name (format nil "~A~A" name count)))
	 (unless (env~lookup-object new-name env :test #'string-equal)
	   (let* ((sym-name (intern new-name))
		  (new-var (term~variable-create sym-name
						 (term~type meta))))
	     (env~enter sym-name new-var env)
	     (return-from plan=create-new-variable new-var))))))))

(defgeneric plan=instantiate-pds (plan subst)
  (declare (edited  "01-APR-1998")
	   (authors Jzimmer)
	   (input   "A PDS and a substitution." )
	   (effect  "Applies the substitution to the whole PDS.")
	   (value   "The new PDS." ))
  
  (:method ((liste list) (subst subst+substitution))
	   (mapcar #'(lambda (elem)
		       (plan=instantiate-pds elem subst))
		   liste))
  (:method ((liste null) (subst subst+substitution) )
	   nil)
  (:method ((pair cons) (subst subst+substitution))
	   (cons (plan=instantiate-pds (car pair)
				       subst)
		 (plan=instantiate-pds (cdr pair)
				       subst)
		 ))
  (:method ((pds pds+proof-plan) (subst subst+substitution))
	   (setf (prob~proof-steps pds)
		 (plan=instantiate-pds
		  (prob~proof-steps pds)
		  subst))
	   (setf (prob~proof-root pds)
		 (plan=instantiate-pds
		  (prob~proof-root pds)
		  subst))
	   pds)
  (:method ((term term+term) (subst subst+substitution))
	   (subst~apply subst term))
  
  (:method ((mapp meth+mapping) (subst subst+substitution))
	   (setf (meth~mapp-subst mapp)
		 (plan=instantiate-pds
		  (meth~mapp-subst mapp)
		  subst))
	   (setf (meth~mapp-mapp mapp)
		 (plan=instantiate-pds
		  (meth~mapp-mapp mapp)
		  subst))
	   mapp)
  
  (:method ((mapp mapp+mapping) (subst subst+substitution))
	   (let* ((new-codomain (plan=instantiate-pds
				 (mapp~codomain mapp)
				 subst))
		  )
	     (mapp~create (mapp~domain mapp)
			  new-codomain)))
  
  (:method ((sub subst+substitution) (subst subst+substitution))
	   (let* ((new-codomain (plan=instantiate-pds
				 (subst~codomain sub)
				 subst))
		  )
	     (setf (subst~codomain sub)
		   new-codomain)
	     sub))
  
  (:method ((node pdsn+node) (subst subst+substitution))
	   (unless (member node plan*already-done)
	     ;(omega~message ".")
	     (setf plan*already-done
		   (cons node plan*already-done))
	     (setf (node~formula node)
		   (plan=instantiate-pds (node~formula node)
					  subst))
	     (setf (node~justification node)
		   (plan=instantiate-pds (node~justification node)
					  subst)))
	   node)
  (:method ((just pdsj+justification) (subst subst+substitution))
	   (setf (pdsj~parameters just)
		 (plan=instantiate-pds (pdsj~parameters just)
					subst))
	   (setf (pdsj~subst just)
		 (plan=instantiate-pds (pdsj~subst just)
					subst))
	   just)
  (:method ((var term+variable) (subst subst+substitution))
	   (subst~apply subst var))
  (:method ((const term+constant) (subst subst+substitution))
	   const)
  (:method ((symbol symbol) (subst subst+substitution))
	   (let* ((pos (position symbol (subst~domain subst)
				 :key #'keim~name
				 :test #'string-equal))
		  )
	     (if pos
		 (intern (keim~name (nth pos (subst~codomain subst))))
	       symbol)))

  (:method ((str string) (subst subst+substitution))
	   str)
  
  (:method ((type type+type) (subst subst+substitution))
	   type)
  (:method ((rat ratio) (subst subst+substitution))
	   rat)
  (:method ((num fixnum) (subst subst+substitution))
	   num)
  (:method ((pos pos+position) (subst subst+substitution))
	   pos)
  (:method ((var term+syn-sorted-var) (subst subst+substitution))
	   var)
  (:method (obj subst)
	   (omega~warn "~%Cannot instantiate on object of type ~S." (type-of obj))
	   obj))


;;; TO IMPLEMENT:
(defun plan=expand-node! (task-node pds node-just just-meth)
  (declare (ignore task-node pds node-just just-meth))
  (omega~error ";;; plan=expand-node! must be implemented."))

;;********************************************************
;;*        some useful functions                         *
;;********************************************************

(defun plan~trace (format-string &rest args)
  (declare (edited  "22-MAY-1997")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  
  (when plan*trace
    (apply #'omega~trace
	   (cons format-string args))
    NIL))

(defun plan~message (format-string &rest args)
  (declare (edited  "22-MAY-1997")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))

  (when plan*messages
    (apply #'omega~message
	   (cons format-string args))
    NIL))

(defun plan=show-agenda (format-string agenda)
  (when plan*trace
    (omega~trace format-string 
		 (if (agenda~empty-p agenda)
		     "Empty Agenda!" 
		   (oc=show-the-agenda agenda)))))
    
(defun plan=set-difference (set1 set2)
  (declare (edited  "28-JAN-1999")
	   (authors Sorge)
	   (input   "Two list representing sets.")
	   (effect  "None.")
	   (value   "A list containing the members of set1 which are not in set2."
		    "The order of set1 is respected."))
  (remove-if #'(lambda (x) (find x set2)) set1))

;;;;;;  VS: trying to share code!!!!!!!!!!!!!!!!!!!

(defun plan=execute-outline-actions (task new-opens mmatching &optional (pds omega*current-proof-plan))
  (declare (edited  "12-MAY-2000")
	   (authors Vxs)
	   (input   "A list of nodes, a matching, the applied method, a task and a PDS.")
	   (effect  "Executes the actions and thereby can change reasons.")
	   (value   "Undefined."))
  (let* ((applied-method (plan~matched-method mmatching))
	 (actions (meth~outline-actions applied-method))
	 (proc-content (car (meth~procedural-content applied-method))))
    (when (and actions (not (plan=tactic-application? proc-content)))
      (let ((reasons (when new-opens
		       (pdsj~reasons (node~justification (first new-opens))))))
	(dolist (action actions) 
	  (meth~execute-action applied-method action (plan~mmatch-mmapp mmatching)
			       pds reasons (when task (pdsn~hyps task))))))))

(defun plan=tactic-application? (proc-content)
  (declare (edited  "19-MAY-2000")
	   (authors Vxs)
	   (input   "A procedural content.")
	   (effect  "None.")
	   (value   "T if the procedural content indicates that the method is a tactic application. O/w NIL."))
  (and (or (stringp proc-content)
	   (symbolp proc-content))
       (string-equal proc-content 'apply-tactic)))
  


;;;; LC rechanged
;(defun plan=set-methods-defaults ()
;  (let ((method-names nil))
;    (with-hash-table-iterator (get-method meth*method-hashtable)
;                              (labels ((try (got-one &optional key value) 
;                                            (when got-one     
;                                              (push (intern key) method-names)
;                                              (multiple-value-call #'try (get-method)))))
;                                (multiple-value-call #'try (get-method))))
;    method-names))



;; Ohne parametrisierte Methoden:
#|
(defun plan~open-node (node select-methods-p &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   )
	   (effect  )
	   (value   ))
  ;;; similar to plan~next-task
  (if node
      (multiple-value-bind (root-taskp new-agenda)
	  (if (pdsn~open-node-p node)
	      (values nil agenda)
	    (oc=open node))
	(let ((node-task (agenda~get-first-task new-agenda
						#'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								    (eq (agenda~task-node tk) node))))))
	  (if node-task
	      (if select-methods-p
		  ;;; The user wants to select some methods:
		  (let* ((node-failed-methods (pdsn~failed-methods node))
			 (node-methods (if (pdsn~alternative-methods node)
					   (pdsn~alternative-methods node)
					 (plan=set-difference (if (agenda~goal-p node-task) 
							     meth*planning-methods
							   meth*MOReasoning-methods)
							 node-failed-methods)))
			 (param-meths (mapcar #'(lambda (m)
						  (cons (keim~name m)
							(mapcar #'keim~name
								(mapcar #'ssterm~sort (meth~parameters m)))))
					      (if (agenda~goal-p node-task)
						  meth*planning-parammeths
						meth*MOReasoning-parammeths)))
			 (meth-to-select (append (mapcar #'keim~name node-methods) param-meths)))
		    ;;; Send LOUI a list of methods from which some methods has to be selected:
		    ;; TO-DO similar to back-task
		    (socket~write (format nil "~A" meth-to-select))
		    ;;; Wait for the LOUI message which corresponds to the selected methods:
		    (let ((selected-meths (read-from-string (socket~read)))
			  (meth-type (arg~find-argtype 'method))
			  (taken-methods)
			  (rejected-methods))
		      (loop
		       (when (null selected-meths) (return))
		       (sys~handler-case
			(setq taken-methods (cons (arg~read-type meth-type (first selected-meths))
						  taken-methods))
			(arg+input-error ()
					 (setq rejected-methods
					       (cons (first selected-meths) rejected-methods))))
		       (setq selected-meths (rest selected-meths)))
		      (let* ((finally-taken-methods (remove-if #'(lambda (m)
								   (find m node-failed-methods
									 :test #'plan=parametrized-method-eq))
							       (reverse taken-methods)))
			     (failed-given-methods (mapcar #'(lambda (pm) (keim~name (first pm)))
							   (plan=set-difference taken-methods
									   finally-taken-methods)))
			     (rejected-methods (reverse rejected-methods)))
		        ;;; Signals some messages:
			(if (and rejected-methods failed-given-methods)
			    (plan~message "The following methods are not taken into account:~%~{~A, ~}~A cannot be read, and ~%~{~A, ~}~A failed to close the node ~A."
					   (butlast rejected-methods) (first (last rejected-methods))
					   (butlast failed-given-methods) (first (last failed-given-methods))
					   (keim~name node))
			  (if rejected-methods
			      (if (rest rejected-methods)
				  (plan~message "The methods ~{~A, ~}~A cannot be read."
						 (butlast rejected-methods) (first (last rejected-methods)))
				(plan~message "The method ~A cannot be read."
					       (first rejected-methods)))
			    (if (rest failed-given-methods)
				(plan~message "The methods ~{~A, ~}~A are not taken into account.~% They failed to close the node ~A."
					       (butlast failed-given-methods) (first (last failed-given-methods))
					       (keim~name node))
			      (when failed-given-methods
				(plan~message "The method ~A is not taken into account.~% It failed to close the node ~A."
					       (first failed-given-methods) (keim~name node))))))
		        ;;; Try to close NODE-TASK using the selected methods first:
			(multiple-value-bind (root-taskp new-agenda)
			    (plan=do-step pds new-agenda :task node-task
					  :first-methods finally-taken-methods
					  :rest-methods (plan=set-difference node-methods finally-taken-methods))
			  (values root-taskp new-agenda))
			)))
	        ;;; The user does not want to select methods
		(multiple-value-bind (root-taskp new-agenda)
		    (plan=do-step pds new-agenda :task node-task)
		  (values root-taskp new-agenda)))
	    ;;; No associated open task for NODE on the agenda, i.e. NODE was perhaps closed by
	    ;; a forward method and is now deleted:
	    (values root-taskp new-agenda))))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))
;;; Uebergabe von parametrisierten Methoden auch!
(defun plan~next-task (node select-methods-p &optional (pds omega*current-proof-plan) (agenda (pds~agenda pds)))
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A pds node or NIL, and a boolean.")
	   (effect  "If NODE is NIL, then signal that the user suggestion is obsolete. Otherwise, when"
		    "there is an associated non-pseudo task for NODE, then this task is considered in"
		    "the next planning step, moreover when SELECT-METHODS-P is T, then we have to deliver"
		    "a list of non-failed methods for this NODE, a subset of them must be selected by the"
		    "user to be considered first.")
	   (value   "A pair:"
		    "- NIL or PDS root task."
		    "- the resulted agenda."))
  ;;; The list of methods to be selected by the user consists of methods and methods with parameters:
  (if node
      (let ((node-task (agenda~get-first-task agenda
					      #'(lambda (tk) (and (not (agenda~pseudo-goal-p tk))
								  (eq (agenda~task-node tk) node))))))
	(if node-task
	    (if select-methods-p
		;;; The user wants to select some methods:
		(let* ((node-failed-methods (pdsn~failed-methods node))
		       (node-methods (if (pdsn~alternative-methods node)
					 (pdsn~alternative-methods node)
				       (plan=set-difference (if (agenda~goal-p node-task) 
							   meth*planning-methods
							 meth*MOReasoning-methods)
						       node-failed-methods)))
		       (param-meths (mapcar #'(lambda (m)
						(cons (keim~name m)
						      (mapcar #'keim~name
							      (mapcar #'ssterm~sort (meth~parameters m)))))
					    (if (agenda~goal-p node-task)
						meth*planning-parammeths
					      meth*MOReasoning-parammeths)))
		       (meth-to-select (append (mapcar #'keim~name node-methods) param-meths)))
		  ;;; Send LOUI a list of methods from which some methods has to be selected:
		  ;; TO-DO: parametrisierte Methoden rausnehmen:
		  (socket~write "choose-query")
		  (parse~plan-methods meth-to-select)
		  ;;; Wait for the LOUI message which corresponds to the selected methods:
		  (let ((selected-meths (read-from-string (socket~read)))
			;; TO-DO
			;;; (1 3 5) Positionen der ausgewaehlen Methoden:
			(meth-type (arg~find-argtype 'method))
			(taken-methods)
			(rejected-methods))
		    (loop
		     (when (null selected-meths) (return))
		     (sys~handler-case
		      (setq taken-methods (cons (arg~read-type meth-type (first selected-meths))
						taken-methods))
		      (arg+input-error ()
				       (setq rejected-methods
					     (cons (first selected-meths) rejected-methods))))
		     (setq selected-meths (rest selected-meths)))
		    (let* ((finally-taken-methods (remove-if #'(lambda (m)
								 (find m node-failed-methods
								       :test #'plan=parametrized-method-eq))
							     (reverse taken-methods)))
			   (failed-given-methods (mapcar #'(lambda (pm) (keim~name (first pm)))
							 (plan=set-difference taken-methods
									 finally-taken-methods)))
			   (rejected-methods (reverse rejected-methods)))
		      ;;; Signals some messages:
		      (if (and rejected-methods failed-given-methods)
			  (plan~message "The following methods are not taken into account:~%~{~A, ~}~A cannot be read, and ~%~{~A, ~}~A failed to close the node ~A."
					 (butlast rejected-methods) (first (last rejected-methods))
					 (butlast failed-given-methods) (first (last failed-given-methods))
					 (keim~name node))
			(if rejected-methods
			    (if (rest rejected-methods)
				(plan~message "The methods ~{~A, ~}~A cannot be read."
					       (butlast rejected-methods) (first (last rejected-methods)))
			      (plan~message "The method ~A cannot be read."
					     (first rejected-methods)))
			  (if (rest failed-given-methods)
			      (plan~message "The methods ~{~A, ~}~A are not taken into account.~% They failed to close the node ~A."
					     (butlast failed-given-methods) (first (last failed-given-methods))
					     (keim~name node))
			    (when failed-given-methods
			      (plan~message "The method ~A is not taken into account.~% It failed to close the node ~A."
					     (first failed-given-methods) (keim~name node))))))
		      ;;; LC: Bei dem Vorschlag von methods, wird z.B. folgendes erwartet:
		      ;; [meth1 (meth2 [param1 param2]) ..] 
		      ;;; Try to close NODE-TASK using the selected methods first:
		      (multiple-value-bind (root-taskp new-agenda)
			  (plan=do-step pds agenda :task node-task
					:first-methods finally-taken-methods
					:rest-methods (plan=set-difference node-methods finally-taken-methods))
			(values root-taskp new-agenda))
		      )))
	      ;;; The user does not want to select methods
	      (multiple-value-bind (root-taskp new-agenda)
		  (plan=do-step pds agenda :task node-task)
		(values root-taskp new-agenda)))
	  ;;; No associated open task for NODE on the agenda:
	  (values nil agenda)))
    (progn 
      (plan~message ";;; Your suggestion is obsolete!")
      (values nil agenda))))


(defgeneric plan~copy (object)
  (declare (edited  "20-JUN-1997" "10-JAN-1995")
	   (authors Lassaad Acsehn)
	   (input "An OBJECT."  )
	   (effect "None." )
	   (value "A copy of OBJECT." ))
  #+gcl(:method ((table t))
		(when (typep table 'hash-table)
 		  (let ((new-table (make-hash-table :test #'equal)))
		    (maphash #'(lambda (key val)
				 (setf (gethash key new-table)
				       val))
			     table)
		    new-table)))
  #-gcl(:method ((table hash-table))
		(let ((new-table (make-hash-table :test #'equal)))
		  (maphash #'(lambda (key val)
			       (setf (gethash key new-table)
				     val))
			   table)
		  new-table))
  (:method ((ndproof pds+proof-plan))
	   (let ((new-proof (pds~proof-plan-create (gentemp (keim~name
							     (prob~proof-problem ndproof))))))
	     (setf (prob~proof-problem new-proof) (prob~proof-problem ndproof))
	     (setf (prob~proof-theory new-proof) (prob~proof-theory ndproof))
	     (setf (prob~proof-steps new-proof) (copy-list (prob~proof-steps ndproof)))
	     (setf (pds~environment new-proof) (plan~copy (pds~environment ndproof)))
	     (setf (prob~proof-root new-proof) (prob~proof-root ndproof))
	     (setf (pds~open-nodes new-proof) (copy-list (pds~open-nodes ndproof)))
	     (setf (pds~support-nodes new-proof) (plan~copy (pds~support-nodes ndproof)))
	     (setf (pds~label-node-hashtable new-proof) (plan~copy (pds~label-node-hashtable ndproof)))
	     new-proof))
  (:method ((object env+environment))
	   (env~copy object))
  (:method ((objectlist list))
	   (mapcar #'(lambda (object) (plan~copy object))
		   objectlist))
  (:method ((ndline pdsn+node))
	   (pdsn~copy ndline)))


|#

