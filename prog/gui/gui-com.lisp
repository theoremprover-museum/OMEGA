;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
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
 
(mod~defmod guic :uses (mod sys inter arg mixin socket)
	    :documentation "Definition of the GUI commands."
	    :exports ())

(eval-when (compile)
  (error "The file GUI-COM.LISP should not be compiled!"))

(com~defcommand loui
  (function guic=reset)
  (argnames )
  (argtypes )
  (frag-cats comint)
  (help "Read input from socket instead of *standard-input*."))

(defun guic=reset ()
  (opr~initialize-omega)
  (opr~initialize-listeners))

(com~defcommand acl
  (function from=standard)
  (argnames )
  (argtypes )
  (frag-cats comint)
  (help "Read input from *standard-input* instead of socket."))

(defun from=standard()
  (mixin~read-from-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand request-command
  (argnames com)
  (argtypes command)
  (arghelps "A command.")
  (frag-cats comint)
  (function guic=request-command)
  (help ""))

(defun guic=get-conventional-defaults (com)
  (let* ((command (arg~read-type 'command com))
	 (argtypes (com~argtypes command))
	 (arglist (make-list (length argtypes)
			     :initial-element (com~unspecified))))
    (dotimes (x (length argtypes))
      (setf arglist (csm~apply-conventional-defaults command arglist)))
    arglist))

(defun guic=request-command (&optional (com nil))
  (when com
    (let* ((command (arg~read-type 'command com))
	   (name (keim~name command))
	   (argtypes (com~argtypes command))
	   (arghelps (com~arghelps command))
	   (bb (when (csm~active-p) (bb~find-blackboard command)))
	   (defaults (if bb (mapcar #'parse~arguments (bb~pop-all-entries bb))
		       (parse~arguments (guic=get-conventional-defaults command))))
	   (method (format nil "popupCommand(~a)"
			   (parse~command name argtypes arghelps defaults (if bb t nil)))))
      (socket~write method))))

(com~defcommand request-command-for-interpreter
  (argnames com)
  (argtypes command)
  (arghelps "A command.")
  (frag-cats comint)
  (function guic=request-command-for-interpreter)
  (help ""))

(defun guic=request-command-for-interpreter (&optional (com nil))
  (when com
    (let* ((command (arg~read-type 'command com))
	   (name (keim~name command))
	   (argtypes (com~argtypes command))
	   (arghelps (com~arghelps command))
	   (bb (when (csm~active-p) (bb~find-blackboard command)))
	   (defaults (if bb (mapcar #'parse~arguments (bb~pop-all-entries bb))
		       (parse~arguments (guic=get-conventional-defaults command))))
	   (method (if argtypes
		       (format nil "expandInterpreter(~a)"
			       (parse~command name argtypes arghelps defaults (if bb t nil)))
		     (format nil "executeInterpreter"))))
      (socket~write method))))

(com~defcommand get-proof
  (function guic=get-proof)
  (argnames )
  (argtypes )
  (frag-cats comint)
  (help "Returns the current proof in loui input language."))

(defun guic=get-proof ()
  (ozi~error-handler
   (parse~proof omega::omega*current-proof-plan)  ; otto
   t)
  )

(com~defcommand get-tasks
  (function guic=get-tasks)
  (argnames )
  (argtypes )
  (frag-cats comint)
  (help "Returns the current agenda in loui input language."))

(defun guic=get-tasks ()
  (ozi~error-handler
   (parse~agenda
    (agenda~for-loui (pds~agenda omega::omega*current-proof-plan)))
   t)
  )

(com~defcommand get-t
  (argnames node)
  (argtypes ndline)
  (arghelps "A node")
  (function guic=get-t)
  (frag-cats comint)
  (help "The term of the node."))

(defun guic=get-t (node)
  (parse~t (node~formula node))
  t)

(com~defcommand get-term
  (argnames node)
  (argtypes ndline)
  (arghelps "A node")
  (function guic=get-term)
  (frag-cats comint)
  (help "The term of the node."))

(defun guic=get-term (node)
  (parse~term (node~formula node))
  t)

(com~defcommand request-problems
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=request-problems)
  (help ""))

(defun guic=request-problems ()
  (let* ((problems (mapcar #'(lambda (x) (keim~name x))
			   (oc=get-all-problems)))
	 (method (format nil "popupProblems(~a)" (parse~list-of-strings problems))))
    (socket~write method :inout)))

(com~defcommand request-prob-proofs
  (argnames problem)
  (argtypes problem)
  (arghelps "A problem.")
  (frag-cats comint)
  (function guic=request-prob-proofs)
  (help ""))

(defun guic=request-prob-proofs (problem)
  (let* ((prob (prob~find-problem problem))
	 (proofs (mapcar #'keim~name (prob~proofs prob)))
	 (method (format nil "insertProbProofs(~a)" (parse~list-of-strings proofs))))
    (socket~write method :inout)))

;;
;;

(com~defcommand get-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=get-problem)
  (help "Get the name of the current problem."))

(defun guic=get-problem ()
  (let ((problem (if omega::omega*current-proof-plan
		     (if (pds~proof-plan-p omega::omega*current-proof-plan)
			 (keim~name (prob~proof-problem omega::omega*current-proof-plan))
		       "None")
		   "None")))
    (parse~name problem)
    t))

(com~defcommand get-problems
  (argnames )
  (argtypes )
  (function guic=get-problems)
  (frag-cats comint)
  (help "The list of all problems."))

(defun guic=get-problems()
  (let ((problems (mapcar #'(lambda (x) (keim~name x))
			  (omega::oc=get-all-problems))))
    (parse~list problems)
    t))

(com~defcommand get-prob-proofs
  (argnames problem)
  (argtypes problem)
  (arghelps "A natural deduction proof or a problem")
  (frag-cats comint)
  (function guic=get-prob-proofs)
  (help "Show all currently existing proof plans for the problem."))

(defun guic=get-prob-proofs (problem)
  (let* ((prob (prob~find-problem problem))
	 (proofs (mapcar #'keim~name (prob~proofs prob))))
    (parse~list proofs)
    t))

(com~defcommand get-pds-name
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=get-pds-name) 
  (help "Returns the name of the current proof plan."))

(defun guic=get-pds-name ()
  (let ((pds-name (keim~name omega::omega*current-proof-plan)))
    (parse~name pds-name)
    t))

(com~defcommand get-theories
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=get-theories)
  (help "Returns the list of all loaded theories."))

(defun guic=get-theories ()
  (let ((theories (mapcar #'keim~name (th~all-theories))))
    (parse~list theories)
    t))

;(com~defcommand get-theory-paths
;  (argnames )
;  (argtypes )
;  (arghelps )
;  (frag-cats comint)
;  (function guic=get-theory-paths)
;  (help "Returns the list of paths of all currently available theories."))

;(defun guic=get-theory-paths ()
;  (let ((paths (if (listp *theory-registry*)
;                   *theory-registry*
;                 (list *theory-registry*))))
;    (parse~list paths)
;    t))

(com~defcommand get-theory-paths
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=get-theory-paths)
  (help "Returns the list of paths of all currently available theories."))

(defun guic=get-theory-paths ()
  (let* ((paths (parse~list-of-atoms (if (listp *theory-registry*)
					 *theory-registry*
				       (list *theory-registry*))))
	 (method (format nil "selectTheory(~a)" paths)))
    (socket~write method)))

(com~defcommand get-rules
  (argnames cat)	
  (argtypes symbol)
  (arghelps "A category belonging to rules.")
  (frag-cats comint)
  (function guic=get-rules)
  (help "Returns the list of all rules in the given category"))

(defun guic=get-rules (cat)
  (let* ((category (com~find-category cat))
	 (rules (if category (comint~commands comint*current-comint category)
		  nil)))
    (parse~commands rules)
    t))

(com~defcommand get-tactics
  (argnames cat)
  (argtypes symbol)
  (arghelps "A category belonging to tactics.")
  (frag-cats comint)
  (function guic=get-tactics)
  (help "Returns the list of all tactics in the given category."))

(defun guic=get-tactics (cat)
  (let* ((category (com~find-category cat))
	 (taccat (com~find-category "TACTICS"))
	 (alltactics (if taccat (comint~commands comint*current-comint taccat)
		       nil))
	 (tactics (remove-if-not #'(lambda (x)
				     (member category (com~categories x)))
				 alltactics)))
    (parse~commands tactics)
    t))

(com~defcommand select-methods
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-methods)
  (help ""))

(defun guic=select-methods ()
  (let ((methods nil)
	(hashtable omega::meth*method-hashtable)
	(defaults (mapcar #'keim~name omega::meth*planning-methods)))
    (maphash #'(lambda (x y) (declare (ignore y))
		 (setq methods (cons x methods))) hashtable)
    (socket~write (format nil "popupMethods(~a ~a)"
			  (parse~list-of-strings methods)
			  (parse~list-of-strings defaults)))))

(com~defcommand set-default-methods
  (argnames method-list)
  (argtypes symbol-list)
  (arghelps  "Current methods to be used from the planner")
  (frag-cats comint)
  (function guic=set-default-methods)
  (help "Resets the list of current methods to the given list."))

(defun guic=set-default-methods (method-list)
  (unless (equal method-list
		 (mapcar #'keim~name omega::meth*planning-methods))
    (let ((methods (remove-if #'null
			      (mapcar #'(lambda (x)
					  (let ((meth
						 (omega::meth~find-method x)))
					    (unless meth
					      (format T "~%~%There is no method with name: ~A~%" x))
					    meth))
				      method-list))))
      (setf omega::meth*planning-methods methods)))
  t)

(com~defcommand select-strategies
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-strategies)
  (help ""))

(defun guic=select-strategies ()
  (let ((strategies (sod=all-strategies))
	(defaults sod*current-strategies))
    (socket~write (format nil "popupStrategies(~a ~a)"
			  (parse~list-of-strings strategies)
			  (parse~list-of-strings defaults)))))


(com~defcommand set-default-strategies
  (argnames strategy-list)
  (argtypes symbol-list)
  (arghelps "Current Strategies")
  (frag-cats comint)
  (function guic=set-default-strategies)
  (help "Sets the current list of strategies"))

(defun guic=set-default-strategies (strategy-list)
  (sod=set-current-strategies! strategy-list)
  t)

(com~defcommand multi-plan
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=multi-plan)
  (help ""))

(defun guic=multi-plan ()
  (sod=system-work sod*current-strategies sod*current-strategic-control-rules))

(com~defcommand get-command
  (argnames name frag)
  (argtypes symbol symbol)
  (arghelps "A name of a command" "A name of a fragment.")
  (frag-cats comint)
  (function guic=get-command)
  (help "Returns the command."))

(defun guic=get-command (name frag)
  (let* ((fragment (com~find-fragment frag))
	 (command (com~find-command name fragment)))
    (parse~command command)
    t))

(com~defcommand get-command-defaults
  (argnames name frag)
  (argtypes symbol symbol)
  (arghelps "A name of a command" "A name of a fragment.")
  (frag-cats comint)
  (function guic=get-command-defaults)
  (help "Returns the default arguments for the command."))

(defun guic=get-command-defaults (name frag)
  (let* ((fragment (com~find-fragment frag))
	 (command (com~find-command name fragment))
	 (bb (when (csm~active-p) (bb~find-blackboard command))))
    (if bb
	(parse~list-of-lists (mapcar #'parse~arguments (bb~pop-all-entries bb)))
      (parse~list (parse~arguments (guic=get-conventional-defaults command))))))

(com~defcommand get-all-cas
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=get-all-cas)
  (help "Returns the list of all CAS."))

(defun guic=get-all-cas ()
  (let ((caslist (mapcar #'(lambda (x) (symbol-name (keim~name x)))
			 (omega::ca~get-all-systems))))
    (parse~list caslist)
    t))

(com~defcommand check-arg-types
  (argnames comargs)
  (argtypes anything-list)
  (arghelps "A list with a command and its arguments.")
  (frag-cats comint)
  (function guic=check-arg-types)
  (help "Checks wether the given arguments match the command argtypes."))

(defun guic=check-arg-types (comargs)
  (sys~handler-case
   (let* ((command (arg~read-type 'command (car comargs)))
	  (arguments (cdr comargs))
	  (argtypes (com~argtypes command))
	  (argchecks (mapcar #'(lambda (x y)
				 (guic=check-arg-type x y))
			     argtypes arguments))
	  (index 0))
     (if (notany #'(lambda (x) (setq index (1+ index))
		     (and (or (stringp x)
			      (symbolp x))
			  (string-equal
			   x
			   "stephan-hess-special-false-value")))
		 argchecks)
	 (socket~write "closeCommand")
       (socket~write (format nil "errorCommand(~a)" index))))
   (arg+wrong-type-error () (parse~symbol "false"))))

(defun guic=check-arg-type (argtype arg)
  (sys~handler-case
   (progn
     (let ((read-function (arg~read-function
			   (arg~find-argtype argtype))))
       (funcall read-function arg)))
   (arg+wrong-type-error () "stephan-hess-special-false-value")
   (arg+cannot-read-error () "stephan-hess-special-false-value")))

(com~defcommand check-interpreter-arg-types
  (argnames comargs)
  (argtypes anything-list)
  (arghelps "A list with a command and its arguments.")
  (frag-cats comint)
  (function guic=check-interpreter-arg-types)
  (help "Checks wether the given arguments match the command argtypes."))

(defun guic=check-interpreter-arg-types (comargs)
  (sys~handler-case
   (let* ((command (arg~read-type 'command (car comargs)))
	  (arguments (cdr comargs))
	  (argtypes (com~argtypes command))
	  (argchecks (mapcar #'(lambda (x y)
				 (guic=check-arg-type x y))
			     argtypes arguments))
	  (index 0))
     (if (notany #'(lambda (x) (setq index (1+ index))
		     (and (or (stringp x)
			      (symbolp x))
			  (string-equal
			   x
			   "stephan-hess-special-false-value")))
		 argchecks)
	 (socket~write "executeInterpreter")
       (socket~write (format nil "errorCommand(~a)" index))))
   (arg+wrong-type-error () (parse~symbol "false"))))

;(com~defcommand check-com-and-arg-types
;  (argnames comargs)
;  (argtypes anything-list)
;  (arghelps "A list with a command and its arguments.")
;  (frag-cats comint)
;  (function guic=check-com-and-arg-types)
;  (help "Checks wether the given command is legal and the given arguments match the
;         command argtypes."))
;
;(defun guic=check-com-and-arg-types (comargs)
;  (sys~handler-case
;   (let* ((command (arg~read-type 'command (car comargs)))
;          (arguments (cdr comargs))
;          
;          (argtypes (com~argtypes command))
;          (arghelps (com~arghelps command))
;
;          (arglist (make-list (length argtypes)
;                              :initial-element (com~unspecified)))
;          (argchecks (mapcar #'(lambda (x y)
;                                 (guic=check-arg-type x y))
;                             argtypes arguments)))
;     (multiple-value-bind (default-list agents?)
;         (com~apply-special-defaults command arglist)
;       (unless agents?
;        (dolist (x argtypes)
;          (declare (ignore x))
;          (setq arglist (com~apply-defaults command arglist))))
;       (let ((defaults (if agents? (mapcar #'parse~arguments default-list)
;                         (parse~arguments arglist))))
;          (if (notany #'(lambda (x) (and (or (stringp x)
;                                             (symbolp x))
;                                         (string-equal
;                                          x
;                                          "stephan-hess-special-false-value")))
;                      argchecks)
;              (if (<= (length argtypes) (length arguments))
;                  (parse~symbol "allargs")
;                (let ((mixin (if agents? defaults
;                               (append (if arguments            ;;; hier noch den Agentenmumpitz einbauen....
;                                           (subseq arguments 0  ;;; habe keine Ahnung was das soll  VS
;                                                   (min (length argtypes)
;                                                        (length arguments)))
;                                         nil)
;                                       (subseq defaults (length arguments)
;                                               (length defaults))))))
;                  (parse~loui-command (keim~name command) argtypes arghelps mixin agents?)))
;            (parse~symbol "illegalarg")))))
;   (arg+wrong-type-error () (parse~symbol "illegalcom"))))

;;;  Agent stuff

(com~defcommand select-command-agents
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-command-agents)
  (help ""))

(defun guic=select-command-agents ()
  (multiple-value-bind (coms bbs agents)
      (csm~considered-commands)
    (declare (ignore coms agents))
    (let ((command-agents (mapcar #'agent~name (agent~get-command-agents)))
	  (defaults (mapcar #'bb~name bbs)))
      (socket~write (format nil "popupCommandAgents(~a ~a)"
			    (parse~list-of-strings command-agents)
			    (parse~list-of-strings defaults))))))

(com~defcommand select-classifier-agents
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-classifier-agents)
  (help ""))

(defun guic=select-classifier-agents ()
  (let ((classifier-agents (mapcar #'agent~name (agent~get-classifiers)))
	(defaults (mapcar #'agent~name (csm~considered-classifiers))))
    (socket~write (format nil "popupClassifierAgents(~a ~a)"
			  (parse~list-of-strings classifier-agents)
			  (parse~list-of-strings defaults)))))

(com~defcommand select-command-heuristics
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-command-heuristics)
  (help ""))

(defun guic=select-command-heuristics ()
  (socket~write (format nil "popupCommandHeuristics(~a ~a)"
			(parse~list-of-strings (oc=heuristics-default 'bb~command-suggestion-leq-p))
			(parse~list-of-strings bb*ordering-command-suggestion-leq))))

(com~defcommand select-parameter-heuristics
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats comint)
  (function guic=select-parameter-heuristics)
  (help ""))

(defun guic=select-parameter-heuristics ()
  (socket~write (format nil "popupParameterHeuristics(~a ~a)"
			(parse~list-of-strings (oc=heuristics-default 'bb~parameter-suggestion-leq-p))
			(parse~list-of-strings bb*ordering-parameter-suggestion-leq))))


