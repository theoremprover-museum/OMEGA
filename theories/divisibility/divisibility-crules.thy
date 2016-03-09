;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-

;;; When all current tasks are schematic tasks, we use the following control rules.
;; The CRI is called even when we are concerned with only one schematic task, because 
;; a control rule can select a task from the remaining agenda.
;; The CRI operates on a list of CURRENT-TASKS, a list of REMAINING-AGENDA-TASKS, the
;; PDS:

;;; This rule selects the first unblocked current task, where blocked(G) iff
;; - G = MV(t1, .., tn) where MV is a meta-variable
;; - or G = ~ MV(t1, .., tn)
;; - or G = A -> B and blocked(A) or blocked(B)
;; - or G = A | B and blocked(A) or blocked(B)
(cri~def-control-rule choose-unblocked-current-task
		      (kind schematic-tasks)
		      (if (first-unblocked-current-task "task"))
		      (then 
		       (choose ("task"))))


;;; When the crule select-unblocked-current-task is not applicable, then all current
;; tasks are blocked. With the help of the next crule we want to select a remaining agenda
;; task which contains one of the blocking meta-variables as subterm and which is unblocked:
(cri~def-control-rule choose-unblocked-agenda-task
		      (kind schematic-tasks)
		      (if (and (blocked-current-task "task1" "metavars")
			       (and (agenda-schematic-task-with-subterm-in "task2" "metavars")
				    (unblocked-task-p "task2"))))
		      (then
		       (choose ("task2"))))

;;; When the crule select-unblocked-agenda-task is not applicable, then each current
;; tasks is blocked wrt. some meta-variables and each remaining agenda schematic task
;; which contains one of these meta-variables is also blocked. With the help of the
;; next crule we want to check whether it is possible to suggest an instantiation of
;; a blocking meta-variable using the Bledsoe heuristic:
;;; WITH-BLEDSOEH-UNBLOCKED-TASKS binds "tasks" to a list of all blocked schematic tasks
;; wrt. a meta-variable "metavar" (current tasks first) to which the bledsoe heuristic
;; can be applied to suggest an instantiation of "metavar". This meta-predicate succeeds
;; when the list "tasks" is not empty.
 
(cri~def-control-rule  choose-bledsoe-methods
		       (kind schematic-tasks)
		       (if (and (with-bledsoeh-unblocked-tasks "tasks" "metavar")
				(and (first-element  "task" "tasks")
				     (tasks-formulas "tasks" "formulas"))))
		       (then
			(choose ("task" (BHImp1) ("formulas")))))


#+logic-new(when (fboundp 'cri~set-used-control-rules!)
	     (cri~set-used-control-rules! '(
					    choose-unblocked-current-task
					    choose-unblocked-agenda-task
					    choose-bledsoe-methods
					    )
					  ))




;;;;;;;;; META-PREDICATES ;;;;;;;;;;;;;
(in-package "OMEGA")

(defun first-unblocked-current-task (task)
  ;;; When TASK is bound checks whether it is unblocked and whether it belongs to
  ;; cri*current-tasks, then returns T
  ;; Otherwise binds TASK to the first unblocked task in cri*current-tasks, if there is
  ;; such a task, otherwise return NIL.
  (if (stringp task)
      ;;; TASK is not yet bound:
      (let ((unblocked-task (find-if #'unblocked-task-p cri*current-tasks)))
	(when unblocked-task
	  (list ;;A list of alternative bindings, where bindings is a list of cons pairs: 
	   (list (cons task unblocked-task)))))
    (and (find task cri*current-tasks) (unblocked-task-p task))
    ))

(defun unblocked-task-p (task)
  ;;; TASK may not be a string. This meta-predicate only checks that TASK is unblocked:
  (unless (stringp task)
    (if (agenda~goal-schema-p task)
	(if (agenda~goal-schematic-p task)
	    (let ((task-formula (agenda~goal-schema-formula task)))
	      (null (blocking-meta-variables task-formula)))
	  T)
      (or (agenda~goal-p task) (agenda~pseudo-goal-p task)))
    ))


(defun blocked-current-task (task metavars)
  ;;; TASK belongs to cri*current-tasks, it is blocked wrt. the meta-variables in METAVARS:
  (if (stringp task)
      (let ((blocked-tasks (remove-if-not #'blocked-task-p cri*current-tasks)))
	(when blocked-tasks
	  (if (stringp metavars)
	      (mapcar #'(lambda (blocked-task)
			  (list (cons task blocked-task)
				(cons metavars (blocking-meta-variables
						(agenda~goal-schema-formula blocked-task)))))
		      blocked-tasks)
	    (let ((alternative-bindings))
	      (dolist (blocked-task blocked-tasks (reverse alternative-bindings))
		(let ((blocking-mvars (blocking-meta-variables (agenda~goal-schema-formula blocked-task))))
		  (when (and (subsetp metavars blocking-mvars) (subsetp blocking-mvars metavars))
		    (setq alternative-bindings
			  (cons (list (cons task blocked-task)) alternative-bindings)))))))))
    (when (find task cri*current-task)
      (let ((blocking-mvars (blocked-task-p task)))
	(when blocking-mvars
	  (if (stringp metavars)
	      (list (list (cons metavars blocking-mvars)))
	    (and (subsetp metavars blocking-mvars) (subsetp blocking-mvars metavars))))))
    ))

(defun blocked-task-p (task)
  ;;; TASK may not be a string. This meta-predicate only checks that TASK is blocked:
  ;; It returns the list of meta-variables that blocks TASK:
  (unless (stringp task)
    (when (and (agenda~goal-schema-p task) (agenda~goal-schematic-p task))
      (let ((task-formula (agenda~goal-schema-formula task)))
	(blocking-meta-variables task-formula)))
    ))


(defun agenda-schematic-task-with-subterm-in (task termlist)
  ;;; TERMLIST may not be a string and may not be NIL.
  ;; TASK must be bound to a schematic task cri*current-agenda which does not belong
  ;; to cri*current-tasks and have at least a subterm in TERMLIST:
  (unless (or (stringp termlist) (null termlist))
    (let ((agenda-rest-tasks (agenda~get-tasks cri*current-agenda
					       #'(lambda (task)
						   (unless (find task cri*current-tasks)
						     (and (agenda~goal-schema-p task)
							  (agenda~goal-schematic-p task)))))))
      (when agenda-rest-tasks
	(if (stringp task)
	    (let ((the-tasks (remove-if-not #'(lambda (tk)
						(some #'(lambda (tm)
							  (data~substruct-positions
							   tm (agenda~goal-schema-formula tk)))
						      termlist))
					    agenda-rest-tasks)))
	      (when the-tasks
		(mapcar #'(lambda (tk) (list (cons task tk)))
			the-tasks)))
	  (and (find task agenda-rest-tasks)
	       (some #'(lambda (tm) (data~substruct-positions tm (agenda~goal-schema-formula task)))
		     termlist)))))
    ))

;;; Muss angepasst werden, um alle Alternativen zu liefern:
(defun with-bledsoeh-unblocked-tasks (tasks metavar)
  ;;; Binds all TASKS in cri*current-agenda blocked wrt. METAVAR and which can be
  ;; unblocked by suggesting an instantiation of METAVAR via a heuristic 
  ;; used by Bldsoe.
  (let ((all-tasks (agenda~get-tasks cri*current-agenda
				     #'(lambda (task)
					 (and (agenda~goal-schema-p task)
					      (agenda~goal-schematic-p task))))))
    (if (stringp tasks)
	(if (stringp metavar)
	    (multiple-value-bind (the-tasks mvar)
		(get-with-BH-unblocked all-tasks)
	      (when the-tasks
		(list (list (cons tasks the-tasks) (cons metavar mvar)))))
	  (multiple-value-bind (the-tasks)
	      (get-with-BH-unblocked all-tasks metavar)
	    (list (list (cons tasks the-tasks)))))
      (if (stringp metavar)
	  (multiple-value-bind (the-tasks mvar)
	      (get-with-BH-unblocked all-tasks)
	    (when (and (subsetp tasks the-tasks) (subsetp the-tasks tasks))
	      (list (list (cons metavar mvar)))))
	(multiple-value-bind (the-tasks)
	    (get-with-BH-unblocked all-tasks metavar)
	  (and (subsetp tasks the-tasks) (subsetp the-tasks tasks))
	  )))
    ))



(defun first-element (object alist)
  ;;; ALIST must be a non-empty list, and OBJECT is bound to its first element:
  (when (consp alist)
    (if (stringp object) (list (list (cons object (first alist))))
      (eq object (first alist)))
    ))

(defun tasks-formulas (tasks formulas)
  ;;; TASKS is a non-empty list of tasks and formulas must be bound to the current formulas of
  ;; these TASKS
  (unless (stringp tasks)
    (when tasks
      (if (stringp formulas)
	  (list (list (cons formulas
			    (mapcar #'agenda~task-formula tasks))))
	(when (= (length tasks) (length formulas))
	  (let ((the-tasks tasks)
		(the-formulas formulas))
	    (loop
	     (if the-tasks
		 (if (data~equal-p (first the-formulas) 
				   (agenda~task-formula (first the-tasks)))
		     (setq the-tasks (rest the-tasks)
			   the-formulas (rest the-formulas))
		   (return-from tasks-formulas))
	       (return)))
	    T))))
    ))
 
;;;;;;;; HELP FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;
(defun blocking-meta-variables (formula)
  ;;; Returns the meta-variables which blocks FORMULA:
  ;; blocked(G) iff - flexible(G)
  ;;                - G = A -> B and flexible(A) or flexible(B)
  ;;                - G = A | B and flexible(A) or flexible(B)
  ;; flexible(G) iff - G = MV(t1, .., tn) where MV is a meta-variable
  ;;                 - G = ~ MV(t1, .., tn)
  (let ((metavar (flexible-wrt-metavar formula)))
    (if metavar (list metavar)
      (cond ((logic~implication-p formula)
	     (let* ((args (data~appl-arguments formula))
		    (hyp-metavar (flexible-wrt-metavar (first args)))
		    (conc-metavar (flexible-wrt-metavar (second args))))
	       (if hyp-metavar (if conc-metavar (remove-duplicates (list hyp-metavar conc-metavar))
				 (list hyp-metavar))
		 (when conc-metavar (list conc-metavar)))))
	    ((logic~disjunction-p formula)
	     (let* ((disjs (data~appl-arguments formula))
		    (ldisj-metavar (flexible-wrt-metavar (first disjs)))
		    (rdisj-metavar (flexible-wrt-metavar (second disjs))))
	       (if ldisj-metavar (if rdisj-metavar (remove-duplicates (list ldisj-metavar rdisj-metavar))
				   (list ldisj-metavar))
		 (when rdisj-metavar (list rdisj-metavar)))))
	    (t nil)))
    ))

(defun flexible-wrt-metavar (formula &optional metavar)
  (let ((func (data~appl-function formula)))
    (if (meta~p func) (if metavar (when (eq func metavar) metavar)
			func)
      (when (eq func (logic~negation-constant :name 'not))
	(flexible-wrt-metavar (first (data~appl-arguments formula)) metavar)))
    ))

(defun get-with-BH-unblocked (tasks &optional mvar)
  ;;; Determines all elements in TASKS which are blocked wrt. MVAR and can become
  ;; unblocked by suggesting an instantiation of MVAR via a heuristic borrowed 
  ;; from a work of Bledsoe. When MVAR is not specified, then it is stated to the
  ;; first blocking meta-variable.
  ;; with-BH-unblocked(G) iff - G = A -> B and flexible(A)
  ;;                          - G = A | B and flexible(A) or flexible(B)
  (when tasks
    (let ((formula (agenda~goal-schema-formula (first tasks))))
      (cond ((logic~implication-p formula)
	     (let ((hyp-metavar (flexible-wrt-metavar (first (data~appl-arguments formula)) mvar)))
	       (if hyp-metavar
		   (multiple-value-bind (the-tasks)
		       (get-with-BH-unblocked (rest tasks) hyp-metavar)
		     (values (cons (first tasks) the-tasks) hyp-metavar))
		 (get-with-BH-unblocked (rest tasks) mvar))))
	    ((logic~disjunction-p formula)
	     (let* ((disjs (data~appl-arguments formula))
		    (ldisj-metavar (flexible-wrt-metavar (first disjs) mvar)))
	       (if ldisj-metavar
		   (multiple-value-bind (the-tasks)
		       (get-with-BH-unblocked (rest tasks) ldisj-metavar)
		     (values (cons (first tasks) the-tasks) ldisj-metavar))
		 (let ((rdisj-metavar (flexible-wrt-metavar (second disjs) mvar)))
		   (if rdisj-metavar
		       (multiple-value-bind (the-tasks)
			   (get-with-BH-unblocked (rest tasks) rdisj-metavar)
			 (values (cons (first tasks) the-tasks) rdisj-metavar))
		     (get-with-BH-unblocked (rest tasks) mvar))))))
	    (T (get-with-BH-unblocked (rest tasks) mvar))))
    ))













