;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

;; Commands for handling the planning system.

(in-package "OMEGA")

;;;; The commando fragments for planning
(com~deffragment planning
 (uses-comms-of direct-display)	 
 (help "PLANNING commands"))

(com~deffragment suggestions
 (help "Commands for planning user suggestions."))

;;;; Some new argument types for the planner commands:
;; METHOD and METHOD-LIST
(eval-when (load compile eval)
(arg~deftype method
 (predicate meth~method-p)
 (read-function plcom=read-method)
 (help "a method or a list, its first element is a method and its rest elements are
parameters for method."))

)

(defun plcom=get-method (obj)
  (if (symbolp obj)
      (let ((method (meth~find-method obj)))
	(if method method
	  (arg~signal-wrong-type 'method obj)))
    (if (listp obj)
	(let ((elt1 (first obj)))
	  (if (symbolp elt1)
	      (let ((method (meth~find-method elt1)))
		(if method
		    (let ((meth-params (meth~parameters method)))
		      (if meth-params
			  (meth~read-parameters method (rest obj) meth-params)
			(if (rest obj)
			    (arg~signal-wrong-type 'method obj)
			  method)))
		  (arg~signal-wrong-type 'method obj)))
	    (if (meth~p elt1)
		(let ((meth-params (meth~parameters elt1)))
		  (if meth-params
		      (meth~read-parameters elt1 (rest obj) meth-params)
		    (if (rest obj)
			(arg~signal-wrong-type 'method obj)
		      elt1)))
	      (arg~signal-wrong-type 'method obj))))
      (if (meth~p obj) obj
	(arg~signal-wrong-type 'method obj)))
    ))

(defmethod plcom=read-method ((obj meth+method) &rest others)
  (declare (ignore others))
  obj)

(defmethod plcom=read-method ((obj list) &rest others)
  (declare (ignore others))
  (plcom=get-method obj))

(defmethod plcom=read-method ((obj t) &rest others)
  (declare (ignore others))
  (plcom=get-method obj))

;;; METHOD-LIST
(arg~deflisttype method-list method)

;;; PRLN-NIL: for a pds node or nil
(eval-when (load compile eval)
(arg~deftype prln-nil
 (predicate plcom=prln-nil-p)
 (read-function plcom=read-prln-nil)
 (help "A PDS node or NIL, when the considered object is a (name of a) node that no more belongs to PDS."))
)

(defun plcom=prln-nil-p (object)
  (or (and (pdsn~p object)
	   (pds~label2node (keim~name object) omega*current-proof-plan))
      (null object)))

(defun plcom=get-prln-nil (obj)
  (if (pdsn~p obj)
      (pds~label2node (keim~name obj) omega*current-proof-plan)
    (if (symbolp obj)
	(pds~label2node obj omega*current-proof-plan)
      (arg~signal-wrong-type 'prln-nil obj))
    ))
      
(defmethod plcom=read-prln-nil ((obj pdsn+node) &rest others)
  (declare (ignore others))
  (plcom=get-prln-nil obj))

(defmethod plcom=read-prln-nil ((obj t) &rest others)
  (declare (ignore others))
  (plcom=get-prln-nil obj))




; ;;;; Some commands:
; ;;; Changing to PLANNING command interpreter   <== commented due to multiprocessing VS!
;; (com~defcommand planning       
;;   (function oc=planning)
;;   (argnames)
;;   (argtypes)
;;   (arghelps)
;;   (frag-cats omega change-comint)
;;   (defaults)
;;   (help "Enters the command interpreter for proof PLANNING."))
;
;; (defun oc=planning ()
;;   (comint~top (comint~create "PLANNING"
;;                            (list (com~find-fragment 'comint) (com~find-fragment 'planning))
;;                            (comint~interface comint*current-comint)
;;                            (comint~logfile comint*current-comint))))
;
;; ------------------------------------------------------------
;;OMEGA-COM.LISP
(com~defcommand reset-constr        
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats direct-display)
  (function oc=reset-constr) 
  (help "Display the current constraint."))

(defun oc=reset-constr ()
  (setf (pds~constraint-pool omega*current-proof-plan) nil))

(com~defcommand show-constr        
  (frag-cats direct-display)
  (function oc=show-constr) 
  (help "Display the current constraint."))

(com~defcommand show-nth-ctr        
  (argnames nth)
  (argtypes integer)
  (arghelps "The position of the constraint state to be shown relative to the current constraint state.")
  (frag-cats direct-display)
  (function oc=show-nth-ctr) 
  (help "Display the nth-previous constraint."))

(defun oc=show-nth-ctr (nth)
  (let ((ctr (pds~constraint-pool omega*current-proof-plan)))
    (if (zerop nth)
	(omega~output (oc=pprint-ctr-state ctr))
      (if (null ctr)
	  (omega~output "Empty constraint state")
	(do ((pos (- nth 1) (- pos 1))
	     (pos-ctr (pds~cstrpool-previous ctr) (pds~cstrpool-previous pos-ctr)))
	    ((or (zerop pos)
		 (null pos-ctr))
	     (cond ((zerop pos)
		    (omega~output (oc=pprint-ctr-state pos-ctr)))
		   (T (omega~output "Empty constraint state")))))))
    ))

(defun oc=show-constr ()
  (if (pds~proof-plan-p omega*current-proof-plan)
      (omega~output (oc=pprint-ctr-state (pds~constraint-pool omega*current-proof-plan)))
    (omega~error "No proof plan is currently active!.")))

(defun oc=pprint-ctr-state (cstrpool)  
  (if cstrpool
      (let ((ctr (pds~cstrpool-constraint cstrpool))
	    (subst (pds~cstrpool-bindings cstrpool)))
	(cond ((and ctr subst)
	       (concatenate 'string
			    (format nil "Constraint:~%  ")
			    (print-object ctr nil)
			    (format nil "~%Bindings:~%  ~A" subst)))
	      (ctr
	       (concatenate 'string
			    (format nil "Constraint:~%  ")
			    (print-object ctr nil)
			    (format nil "~%Empty bindings")))
	      (subst
	       (format nil "Empty constraint:~%Bindings:~%  ~A" subst))
	      (t
	       (format nil "Empty constraint and bindings"))))
    (format nil "Empty constraint state")))


;;; PLAN-COM.LISP
(com~defcommand initialize-agenda
  (frag-cats planning)
  (function lccom=initialize-agenda)
  (log-p T)
  (help "Initialize the agenda."))

(defun lccom=initialize-agenda ()
  ;;; should be called after loading a problem and/or
  ;; initializing a PDS
  (plan~initialize-agenda))

(com~defcommand set-methods
  (argnames meth-names)
  (argtypes symbol-list)
  (arghelps "The methods used for planning.")
  (frag-cats planning)
  (function plan=set-planning-methods)
  (defaults ((plan=set-methods-defaults)))
  (log-p T)
  (help "Set the list of methods used for planning."))



(com~defcommand step-plan
  (frag-cats planning)
  (function plan~step-plan)
  (log-p T)
  (help "Do a planning step."))

(com~defcommand n-step-plan
  (argnames num)
  (argtypes integer)
  (arghelps "The number of steps")
  (frag-cats planning direct-display)
  (function plcom=n-step-plan)
  (log-p T)
  (help "Do a certain number of planning steps."))

(com~defcommand nsp
  (argnames num)
  (argtypes integer)
  (arghelps "The number of steps")
  (frag-cats no-gui)
  (function plcom=n-step-plan)
  (log-p T)
  (help "Do a certain number of planning steps."))



(defun plcom=n-step-plan (numb)
  (let ((scounter 1))
    (loop
     (if (> scounter numb)
	 (return-from plcom=n-step-plan t)
       (progn (incf scounter)
	      (plan~step-plan))))))

(com~defcommand step-plan-with
  (argnames meth-name goal supports parameters)
  (argtypes anything anything anything anything)
  (arghelps "Method" "Goal" "Supports" "Parameters")
  (frag-cats planning)
  (function plan~step-plan-with)
  (log-p T)
  (help "Do a planning step with given goal and supports and parameters."))

(com~defcommand plan
  (frag-cats planning)
  (function plcom=plan)
  (log-p T)
  (help "Do a planning step."))

(defun plcom=plan ()
  (if omega*current-proof-plan
      (if (agenda~empty-p (pds~agenda omega*current-proof-plan))
	  (omega~message "The current agenda is empty!")
					;     (format t "The current agenda is empty!")
	(plan~plan omega*current-proof-plan))
    (omega~message "There is no current proof plan")
    ))
  
(com~defcommand change-modus
  (argnames modus bounds)
  (argtypes symbol anything)
  (arghelps "the modus name" "modus bounds")
  (frag-cats planning)
  (function plan~change-modus)
  (log-p T)
  (help "Do a planning step."))

(com~defcommand plan-modus
  (frag-cats planning)
  (function plan~show-modus)
  (log-p T)
  (help "Do a planning step."))


;;;; Commands for the reactive modus
;; Reactive commands for the agenda:
(com~defcommand change-modus
  (argnames modus bounds)
  (argtypes symbol anything)
  (arghelps "the modus name" "modus bounds")
  (frag-cats suggestions)
  (function plan~reactive-change-modus)
  (help "Do a planning step."))

(com~defcommand back
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to take back its associated task.")
  (frag-cats suggestions)
  (function plan~back-task)
  (help "Take back an agenda task."))

(com~defcommand next
  (argnames line methods?)
  (argtypes prln-nil boolean)
  (arghelps "A line to consider its associated task next."
	    "Methods to select by the user?")
  (frag-cats suggestions)
  (function plan~next-task)
  (help "Consider the given task next with eventually proposed methods first."))

(com~defcommand block
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to block its associated task.")
  (frag-cats suggestions)
  (function plan~block-task)
  (help "Block an agenda task."))

(com~defcommand unblock
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to unblock its associated task.")
  (frag-cats suggestions)
  (function plan~unblock-task)
  (help "Unblock an agenda task."))

(com~defcommand back-pseudo
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to take back its associated pseudo task.")
  (frag-cats suggestions)
  (function plan~back-pseudo-task)
  (help "Take back an agenda pseudo task."))

(com~defcommand next-pseudo
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to consider its associated pseudo task next.")
  (frag-cats suggestions)
  (function plan~next-pseudo-task)
  (help "Consider the given pseudo task next."))

(com~defcommand block-pseudo
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to block its associated pseudo task.")
  (frag-cats suggestions)
  (function plan~block-pseudo-task)
  (help "Block an agenda pseudo task."))

(com~defcommand unblock-pseudo
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to unblock its associated pseudo task.")
  (frag-cats suggestions)
  (function plan~unblock-pseudo-task)
  (help "Unblock an agenda pseudo task."))

;; Reactive commands for the PDS nodes:
(com~defcommand open
  (argnames line methods?)
  (argtypes prln-nil boolean)
  (arghelps "A line to open."
	    "Methods to select by the user?")
  (frag-cats suggestions)
  (function plan~open-node)
  (help "Reopen a node and eventually suggest a list of methods to be applied first on this node."))

(com~defcommand delete
  (argnames line)
  (argtypes prln-nil)
  (arghelps "A line to delete.")
  (frag-cats suggestions)
  (function plan~delete-node)
  (help "Delete a node and backtrack all plan steps depending on it."))


;;; Show Methods
(com~defcommand show-methods
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats planning direct-display)
  (function lccom=show-all-methods)
  (help "Show the loaded methods."))

(defun lccom=show-all-methods ()
  (meth~show-method-hashtable)
  (values))

(com~defcommand show-planning-methods
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats planning direct-display)
  (function lccom=plan-methods)
  (help "Show the regular planning methods."))

(defun lccom=plan-methods ()
  (cond ((and  meth*planning-methods meth*planning-parammeths)
	 (omega~message "Planning methods: ~{~%~A ~}" meth*planning-methods)
	 (omega~message "~%Parameterized planning methods: ~{~%~A ~}" meth*planning-parammeths))
	(meth*planning-methods
	 (omega~message "Planning methods: ~{~%~A ~}" meth*planning-methods))
	(meth*planning-parammeths
	 (omega~message "Parameterized planning methods: ~{~%~A ~}" meth*planning-parammeths))
	(t (omega~message "No planning methods defined!"))))

(com~defcommand show-normalizing-methods
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats planning direct-display)
  (function lccom=normalizing-methods)
  (help "Show the regular normalizing methods."))

(defun lccom=normalizing-methods ()
  (format t "Normalizing methods: ~{~%~A ~}" meth*normalizing-methods))

(com~defcommand show-restricting-methods
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats planning direct-display)
  (function lccom=restricting-methods)
  (help "Show the regular restricting methods."))

(defun lccom=plan-methods ()
  (format t "Task restricting methods: ~{~%~A ~}" meth*restricting-methods))


;(com~defcommand add-method
;  (argnames method-name)
;  (argtypes symbol)
;  (arghelps "the method name")
;  (frag-cats planning)
;  (function lccom=add-method)
;  (help "Add a method to plan with."))
;
;(defun lccom=add-method (meth-name)
;  (let ((meth (meth~find-method meth-name)))
;    (if meth (plan=add-method meth)
;      (format t "~%~A is not loaded!" meth-name))))
;
;(com~defcommand del-method
;  (argnames method-name)
;  (argtypes symbol)
;  (arghelps "the method name")
;  (frag-cats planning)
;  (function lccom=del-method)
;  (help "Delete a method to plan with."))
;
;(defun lccom=del-method (meth-name)
;  (let ((meth (meth~find-method meth-name)))
;    (if meth (plan=del-method meth)
;      (format t "~%~A is not loaded!" meth-name))))
;
;
;(com~defcommand add-mormethod
;  (argnames method-name)
;  (argtypes symbol)
;  (arghelps "the method name")
;  (frag-cats planning)
;  (function lccom=add-mor-method)
;  (help "Add a MOR method."))
;
;(defun lccom=add-mor-method (meth-name)
;  (let ((meth (meth~find-method meth-name)))
;    (if meth (plan=add-mor-method meth)
;      (format t "~%~A is not loaded!" meth-name))))
;
;
;(com~defcommand del-mormethod
;  (argnames method-name)
;  (argtypes symbol)
;  (arghelps "the method name")
;  (frag-cats planning)
;  (function lccom=del-mor-method)
;  (help "Add a MOR method."))
;
;(defun lccom=del-mor-method (meth-name)
;  (let ((meth (meth~find-method meth-name)))
;    (if meth (plan=del-mor-method meth)
;      (format t "~%~A is not loaded!" meth-name))))
;
;(com~defcommand show-method
;  (argnames method-name)
;  (argtypes symbol)
;  (arghelps "the method name")
;  (frag-cats planning)
;  (function plcom=show-method)
;  (defaults ((com~unspecified)))
;  (help "Shows the slots of a method."))
;
;(defun plcom=show-method (method-name)
;  (let ((method (meth~find-method method-name)))
;    (if method
;        (meth~show-method method)
;      (format T "~%~%There is no method with name ~A~%" method-name))
;    (values)))
;        
;(com~defcommand apply-method
;  (argnames method-name open-nodes support-nodes)
;  (argtypes symbol ndline-list ndline-list)
;  (arghelps "the name of the method to apply" "open nodes" "support nodes")
;  (frag-cats planning)
;  (function plcom=apply-method)
;  (defaults plcom=apply-method-defaults)
;  (log-p T)
;  (help "Changes a proof plan by applying the specified method on the given proof nodes."))


;;;; DEBUGGING commands
(com~defcommand reset-control      
  (argnames line)
  (argtypes ndline)
  (arghelps "A line its control to be reset")
  (function reset-control)
  (frag-cats planning)
  (help "Reset the control of a line."))

(defun reset-control (symbol)
  (let* ((node (if (pdsn~p symbol) symbol
		 (pds~label2node symbol)))
	 (control (pdsj~control (node~justification node))))
    (setf (pdsc~failed-methods control) NIL)
    (setf (pdsc~applied-critics control) NIL)
    (setf (pdsc~alternative-methods control) NIL)
    (setf (pdsc~alternative-mmatchings control) NIL)))

(com~defcommand remove-order
  (argnames)
  (argtypes)
  (arghelps)
  (function remove-order)
  (frag-cats planning)
  (help "Remove the orderings in the current agenda."))

(defun remove-order ()
  (let ((agenda (pds~agenda omega*current-proof-plan)))
    (setf (agenda~orderings agenda) NIL)
    (format t "~%The orderings in the current agenda are reset.")
    ))

(com~defcommand show-fathers      
  (argnames line)
  (argtypes ndline)
  (arghelps "A line its depending nodes to be shown")
  (function show-fathers)
  (frag-cats direct-display)
  (help "Show the depending nodes of a line."))

(defun show-fathers (node)
  (mapc #'oc=show-line (pdsn~depending-nodes node)))

(com~defcommand show-control      
  (argnames line)
  (argtypes ndline)
  (arghelps "A line its control to be shown")
  (function show-control)
  (frag-cats direct-display)
  (help "Show the control of a line."))

(defun show-control (symbol)
  (let* ((node (if (pdsn~p symbol) symbol
		 (pds~label2node symbol)))
	 (control (pdsj~control (node~justification node))))
    (if (or (pdsc~failed-methods control)
	    (pdsc~excluded-methods control)
	    (pdsc~alternative-methods control)
	    (pdsc~applied-critics control)
	    (pdsc~alternative-mmatchings control))
	(progn
	  (when (pdsc~failed-methods control)
	    (format t "Failed methods: ~A~%" (pdsc~failed-methods control)))
	  (when (pdsc~excluded-methods control)
	    (format t "Excluded methods: ~A~%" (pdsc~excluded-methods control)))
	  (when (pdsc~applied-critics control) 
	    (format t "Applied critics: ~A~%" (pdsc~applied-critics control)))
	  (when (pdsc~alternative-methods control) 
	    (format t "Alternative methods: ~A~%" (pdsc~alternative-methods control)))
	  (when (pdsc~alternative-mmatchings control)
	    (format t "~A alternative mmatchings~%" (length (pdsc~alternative-mmatchings control)))))
      (format t "Empty control information for ~A" (keim~name node)))
    ))

(com~defcommand show-goal      
  (argnames line)
  (argtypes ndline)
  (arghelps "An open line its current formula to be shown")
  (function show-goal)
  (frag-cats direct-display)
  (help "Show the current formula of an open line."))

(defun show-goal (symbol)
  (let* ((node (if (pdsn~p symbol) symbol
		 (pds~label2node symbol omega*current-proof-plan)))
	 (task (agenda~get-first-task (pds~agenda omega*current-proof-plan)
				      #'(lambda (task) (eq (agenda~task-node task) node)))))
    (if task
	(format t "~%Current formula of ~A: ~A" (keim~name node) (pds~task-formula task omega*current-proof-plan))
      (format t "~%No task for ~A." (keim~name node)))
    ))

(com~defcommand back-last-step
  (function plan=back-last-step!)
  (frag-cats planning)
  (log-p T)
  (help "Take back the last planning step."))		







