(in-package :omega)

(mod~defmod CRIHELP 
            :uses (agenda beta bind class data env infer just keim logic meta meth node omega pds pdsc pdsj pdsn pos post pplan rcl roc subst term type)
            :documentation ""
            :exports (
                      crihelp*current-hint
                      crihelp*multiplication-table))

;;; The following functions are internal in other modules and should not be used:
;;; (methhelp=convert-hom-rescl-to-num methhelp=get-primitives methhelp=msolve-with-maple rcl=dissect-maple-hint strathelp=order-of-element-goal-p term=alpha-equal zmz=or-set-of-resclass-p zmz=resclass-set-formula-p zmz=resclass-singleton-p zmztac=class-factor zmztac=convert-resclass-operation-to-num-operation zmztac=count-first-n-nats)

;; content of this file
;; - hint system (used by InstFromCAS)
;; - cases
;; - special cases (with old hint system, used by InstFromParam)
;; - helper functions

;; The difference between the old and the new hint system is that the new system can compute the hint from
;; an instantiation task only. The old hint system additionally needed the start task of the whole problem.
;; The new hint system automatically computes this start task and so it is independent of the calling strategy.
;; The instatiation strategy can be treated as a stand-alone strategy and has no longer be called as a demand
;; with parameters. The strategy InstFromCAS implements this stand-alone strategy.

;; Another aspect of the new hint system is that the cases have been modularized and so new cases can easily be added.
;; A case is a class derived from crihelp+case. It has to implement the following methods:
;; - crihelp=case-test: this tests whether the case corresponds to the node
;; - crihelp=case-process: this is normally implemented by deriving from crihelp+case-pos or crihelp+case-neg
;; - crihelp=case-compute-multtable: this computes the multiplication table(s)
;; - crihelp=case-compute-hint: this computes the hint
;; - crihelp=case-compute-output: this computes the instantiation for an instantiation task
;; Methods can also be implemented by deriving from special base classes.
;; If a new case class has been implemented, it can be added to the list in crihelp*case-classes. Then it will be
;; taken into consideration every time a hint is demanded.
;; The implemented cases can be looked at as examples.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the hint system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar crihelp*case-classes
  '(crihelp+case-order
    crihelp+case-iso
    crihelp+case-closure-not
    crihelp+case-commut-not
    crihelp+case-assoc-not
    crihelp+case-distrib-not
    crihelp+case-unit
    crihelp+case-unit-not
    crihelp+case-inverse
    crihelp+case-inverse-not
    crihelp+case-divisor
    crihelp+case-divisor-not
    )
  "All cases to test for")

(defvar crihelp*processed-cases nil "An associations list which maps a node to a case object")

(defun crihelp=create-case (node class)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node and a case class.")
	   (effect  "If case test succeeds, the case is added to the association list crihelp*processed-cases.")
	   (value   "A case object of the specified class, if the case test was successful, nil otherwise."))
  (let ((case (make-instance class :node node)))
    (sys~handler-case
     (when (crihelp=case-test case)
       (progn
	 (setf crihelp*processed-cases (acons node case crihelp*processed-cases))
	 case))
     (T (c) (progn (omega~trace "Error in crihelp=case-test: ~A,~% but we continue ..." class) nil)))))
      
(defun crihelp=match-cases (node classes)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node and a list of case classes.")
	   (effect  "See crihelp=create-case.")
	   (value   "A case object if the formula of node belongs to one of the case classes."))
  (when classes
    (let ((case (crihelp=create-case node (first classes))))
      (if case
	  case
	(crihelp=match-cases node (rest classes))))))


(defun crihelp=find-case (node)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "If the case does not exist, it is added to the association list crihelp*processed-cases.")
	   (value   "A case object if node corresponds to a case class in crihelp*case-classes, otherwise nil."
		    "Note: If the node was analysed earlier, the stored object is taken from crihelp*processed-cases"))
  (let ((processed-case (cdr (assoc node crihelp*processed-cases))))
    (if processed-case
	processed-case
      (crihelp=match-cases node crihelp*case-classes))))
	  
(defun crihelp=param-to-inst-task-by-node (insttask node)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "An instantiation task and a node.")
	   (effect  "See crihelp=find-case.")
	   (value   "The instantiation for insttask wrt. the case object for node or - if that does not exists - a supergoal of node."))
  (let ((case (crihelp=find-case node)))
    (if case
	(progn
	  (format t "~A" case)
	  (crihelp=case-process case insttask))
      (when (not (keim~equal node (prob~proof-root omega*current-proof-plan)))
	(crihelp=param-to-inst-task-by-node insttask (crihelp=supergoal-of-node node))))))

(defun crihelp=param-to-inst-task (insttask)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "An instantiation task.")
	   (effect  "See crihelp=find-case.")
	   (value   "The instantiation for insttask if exists, otherwise nil."
		    "Note: The computation is accelerated by stored hints."))
  (crihelp=param-to-inst-task-by-node insttask (pdsc~an-node (agenda~inst-task-plan-step insttask))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find polynom function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=flat-list (struct)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A tree.")
	   (effect  "None.")
	   (value   "A flat list."))
  (if (listp struct)
      (apply #'append (mapcar #'crihelp=flat-list struct))
    (list struct)))
 		       
(defun crihelp=remove-power-functions (functions)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A tree of functions.")
	   (effect  "None.")
	   (value   "The flat list of functions without power functions."))
  (remove-if #'(lambda (funci)
 		 (find 'power (crihelp=flat-list funci)))
 	     functions))

(defun crihelp=get-unexpanded-iso-task (iso-task)
  (let* ((node (agenda~task-node iso-task))
	 (just (node~justification node))
	 (reason (first (pdsj~other-reasons just))))
    (if reason
	(let* ((step-just (pdsc~an-just reason))
	       (step-just-method (just~method step-just))
	       (step-just-method-name (keim~name step-just-method)))
	  (if (string-equal step-just-method-name 'Expand-Pair-Operation-m)
	      (agenda~create-goal (pdsc~an-node reason))
	    iso-task))
      iso-task)))

(defun crihelp=combine-functions (set functions)
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "A residue class set or a cartesian product set of residue class sets and"
		    "a list of fucntions, one for each position of the cartsian product.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: One function, where all input function are combined according to the"
		    "       structure of the set."
		    "       For example: (cartesian-product set1 set2) (f1 f2) -> (pair f1 f2)"
		    "                    (cartesian-product (cartesian-product set11 set12) set2) (f11 f12 f2)"
		    "                             -> (pair (pair f11 f12) f2)"
		    "Second: The remaining, not used functions list."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(multiple-value-bind
	    (back1 rest-functions1)
	    (crihelp=combine-functions (first (data~appl-arguments set)) functions)
	  (multiple-value-bind
	      (back2 rest-functions2)
	      (crihelp=combine-functions (second (data~appl-arguments set)) rest-functions1)
	    (values (term~appl-create (env~lookup-object 'pair env)
				      (list back1 back2))
		    rest-functions2)))
      (values (first functions)
	      (rest functions)))))				      

(defun crihelp=pairs-var (set new-var)
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "A residue class set or a cartesian product set of residue class sets and a new varieble x.")
	   (effect  "None.")
	   (value   "A list of the single position vars of the pairs with according class-factors:"
		    "For example: (cartesian-product set1 set2)  ->  '(((first-or-pair x) fac1)  ((second-of-pair x) fac2))"
		    "             (cartesian-product (cartesian-product set11 set12) set2)"
		    "                        -> '(((first-of-pair (first-of-pair x)) fac11) ((second-of-pair (first-of-pair x)) fac12)"
		    "                             ((second-of-pair x) fac2))"))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(let* ((back-list1 (crihelp=pairs-var (first (data~appl-arguments set)) new-var))
	       (back-list2 (crihelp=pairs-var (second (data~appl-arguments set)) new-var))
	       (first-of-pair (env~lookup-object 'first-of-pair env))
	       (second-of-pair (env~lookup-object 'second-of-pair env)))
	  (append (mapcar #'(lambda (back1)
			      (list (term~appl-create first-of-pair (list (first back1)))
				    (second back1)))
			  back-list1)
		  (mapcar #'(lambda (back2)
			      (list (term~appl-create second-of-pair (list (first back2)))
				    (second back2)))
			  back-list2)))
      (list (list new-var (zmztac=class-factor set))))))

(defun crihelp=replace-in-pol (assoc-list polynom class-factor)
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "A assoc list, a list representing a polynom, and a class-factor.")
	   (effect  "None.")
	   (value   "Transforms the polynom into a post-expression on restclasses."))
  (if (listp polynom)
      (let* ((parts (mapcar #'(lambda (polynom-item)
				(cond ((listp polynom-item)
				       (crihelp=replace-in-pol assoc-list polynom-item class-factor))
				      (t
				       (crihelp=translate assoc-list polynom-item class-factor))))					    
			    polynom)))
	(term~appl-create (first parts) (rest parts)))
    (crihelp=translate assoc-list polynom class-factor)))

(defun crihelp=translate (assoc-list polynom-item class-factor)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (cond ((numberp polynom-item)
	   (term~appl-create (env~lookup-object 'resclass env)
			     (list class-factor (term~constant-create polynom-item
								      (env~lookup-object 'num env)))))
	  ((string-equal polynom-item 'plus)
	   (env~lookup-object 'plus-resclass env))
	  ((string-equal polynom-item 'minus)
	   (env~lookup-object 'minus-resclass env))
	  ((string-equal polynom-item 'times)
	   (env~lookup-object 'times-resclass env))
	  ((assoc polynom-item assoc-list)
	   (let* ((trans (rest (assoc polynom-item assoc-list))))
	     (term~appl-create (env~lookup-object 'resclass env)
			       (list class-factor
				     (term~appl-create (env~lookup-object 'mod env)
						       (list (term~appl-create (env~lookup-object 'class-residue env)
									       trans)
							     class-factor))))))
	  (t
	   (omega~error "~%In function crihelp=translate can not translate ~A" polynom-item)))))

(defun crihelp=pairs-facs (set)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   "A residue class set or a cartesian product set of residue class sets.")
	   (effect  "None.")
	   (value   "A list of natiral numbers which are the modulo factors of the residue-class-sets."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(let* ((first-facs (crihelp=pairs-facs (first (data~appl-arguments set))))
	       (second-facs (crihelp=pairs-facs (second (data~appl-arguments set)))))
	  (append first-facs second-facs))
      (list (zmztac=class-factor set)))))

(defun crihelp=parse-polynom-functions (functions iso-task)
  (let* ((node (agenda~task-node iso-task))
	 (formula (node~formula node))
	 (set1 (first (data~appl-arguments formula)))
	 (set2 (third (data~appl-arguments formula)))
	 (class-factors (crihelp=pairs-facs set2))
	 (new-var (term~variable-create 'x (first (data~abstr-domain (term~type set1)))))
	 (pair-vars (crihelp=pairs-var set1 new-var))
	 (function-parts (mapcar #'(lambda (function class-factor)
				     (crihelp=parse-polynom-function function class-factor pair-vars))
				 functions class-factors))
	 (polynom (crihelp=combine-functions set2 function-parts)))
    (term~abstr-create (list new-var)
		       polynom)))

(defun crihelp=get-bound-vars-and-polynom (function)
  (if (string-equal (first function) 'lam)
      (multiple-value-bind
	  (bound-vars polynom)
	  (crihelp=get-bound-vars-and-polynom (third function))
	(values (cons (first (second function)) bound-vars)
		polynom))
    (values nil
	    (second function))))
	    
(defun crihelp=parse-polynom-function (function class-factor pair-vars)
  (declare (edited  "22-AUG-2000")
	   (authors Ameier)
	   (input   "A polynom-function from the rcl-module, a class-factor, and the list"
		    "of vars replacements.")
	   (effect  "None.")
	   (value   "A new term for this polynom function."))

  (multiple-value-bind
      (bound-vars polynom)
      (crihelp=get-bound-vars-and-polynom function)
    
    ;;(let* ((bound-vars (mapcar #'first (rest (butlast function))))
    ;;	 (polynom (first (rest (butlast (first (last function)))))))
    
    (crihelp=replace-in-pol (mapcar #'cons bound-vars pair-vars)
			    polynom
			    class-factor)))

(defun crihelp=find-polynom-function (task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "The polynom function for task."
		    "Note: The computation is accelerated by stored hints."))
  (let* ((iso-task (crihelp=get-unexpanded-iso-task task))
	 (node (agenda~task-node iso-task))
	 (processed-case (cdr (assoc node crihelp*processed-cases)))
	 (case (if processed-case processed-case (crihelp=create-case node 'crihelp+case-iso))))
    (crihelp=case-process-hint case)
    (when (crihelp~case-success case)
      (let* ((function-pairs (crihelp~case-hint case))
	     (functions (crihelp=remove-power-functions (rcl~check-closed-homomorphism (crihelp~case-2-table1 case)
										       (crihelp~case-2-table2 case) function-pairs))))
	(if functions
	    (progn
	      ;;(setq glo*poly functions)
	      ;;(error "ASAS")
	      (crihelp=parse-polynom-functions functions iso-task))
	  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the base case class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case ()
  ((node                 :initform nil
			 :accessor crihelp~case-node
			 :initarg :node
			 :documentation "The node the case is created from.")
   (mv                   :initform nil
			 :accessor crihelp~case-mv
			 :initarg :mv
			 :documentation "The MV to be instantiated.") ;; see below MP
   (processed            :initform nil
			 :accessor crihelp~case-processed
			 :initarg :processed
			 :documentation "A flag whether the hint has already be computed.")
   (success              :initform nil
			 :accessor crihelp~case-success
			 :initarg :success
			 :documentation "The success flag.")
   (hint                 :initform nil
			 :accessor crihelp~case-hint
			 :initarg :hint
			 :documentation "The computed hint.")
   (output               :initform nil
			 :accessor crihelp~case-output
			 :initarg :output
			 :documentation "An association list mapping instatiation tasks to there instantiations.")
   )
  (:documentation "The base class for all cases."))

(defmethod print-object ((object crihelp+case) stream)
  (if (crihelp~case-processed object)
      (progn
	(format stream "The case for the formula ~A has already been processed with result ~A.~%"
		(crihelp~case-formula object) (crihelp~case-success object))
	(format stream "Hint: ~A~%" (crihelp~case-hint object)))
    (format stream "The case for the formula ~A has to be processed.~%" (crihelp~case-formula object))))

(defmethod crihelp~case-formula ((case crihelp+case))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The formula of the case node."))
  (pds~node-formula (crihelp~case-node case)))

(defmethod crihelp=case-output-for-insttask ((case crihelp+case) insttask)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "Adds a newly found instantiation to the output slot of case.")
	   (value   "The instantiation for the instantiation task."))
  (let ((out (cdr (assoc (agenda~inst-task-meta-var insttask) (crihelp~case-output case)))))
    (if out
	(progn
	  (format t "The parameters have already been computed for the metavar ~A: ~A" (agenda~inst-task-meta-var insttask) out)
	  out)
      (let ((computed-out (crihelp=case-compute-output case insttask)))
	(format t "The parameters are now computed for the metavar ~A: ~A~%" (agenda~inst-task-meta-var insttask) computed-out)
	(setf (crihelp~case-output case) (acons (agenda~inst-task-meta-var insttask) computed-out (crihelp~case-output case)))
	computed-out))))

(defmethod crihelp=case-process-hint ((case crihelp+case))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "Computes the multiplication table and the hint and stores them in the slots of case.")
	   (value   "Undefined."))
  (when (not (crihelp~case-processed case))
    (crihelp=case-compute-multtable case)
    (multiple-value-bind (success hint)
	(crihelp=case-compute-hint case)
      (setf (crihelp~case-success case) success
	    (crihelp~case-hint case) hint
	    (crihelp~case-processed case) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the positive case class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-pos (crihelp+case)
  ()
  (:documentation "The base class for all positive cases."))

(defmethod crihelp=case-process ((case crihelp+case-pos) insttask)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "See crihelp=case-process-hint.")
	   (value   "The instantiation for insttask."))
  (crihelp=case-process-hint case)
  (when (crihelp~case-success case)
    (crihelp=case-output-for-insttask case insttask)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the negative case class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-neg (crihelp+case)
  ()
  (:documentation "The base class for all negative cases."))

(defmethod crihelp=case-process ((case crihelp+case-neg) insttask)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "See crihelp=case-process-hint.")
	   (value   "The instantiation for insttask."))
  (crihelp=case-process-hint case)
  (unless (crihelp~case-success case)
    (crihelp=case-output-for-insttask case insttask)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the base case class for one multiplication table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-1 (crihelp+case)
  ((table                :initform nil
			 :accessor crihelp~case-1-table
			 :initarg :table)
   )
  (:documentation "The base class for all cases with one multiplication table."))

(defmethod crihelp=case-compute-multtable ((case crihelp+case-1))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "Computes the multiplication table and stores it in the slot of the case.")
	   (value   "Undefined."))
  (let* ((formula (crihelp~case-formula case)))
    (multiple-value-bind
	(sets ops)
	(crihelp=get-sets-and-operations-from-card-prod formula)
      (let* ((class-factors (mapcar #'zmztac=class-factor sets))
	     (class-factor-numbers (mapcar #'keim~name class-factors))
	     (number-lists (mapcar #'crihelp=number-set-to-resclass-set sets))
	     (operations-on-nums (mapcar #'(lambda (op class-factor)
					     (crihelp=convert-operation op class-factor nil))
					 ops class-factors))
	     (table (if (= (length sets) 1)
			(rcl~multiplication-table (first number-lists) (first class-factor-numbers)
						  :operation (first operations-on-nums))
		      (rcl~product-multiplication-table number-lists
							:modulo class-factor-numbers
							:operation operations-on-nums))))
	(setf (crihelp~case-1-table case) table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the base case class for hints in form of contra pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-contrapair (crihelp+case)
  ()
  (:documentation "The base class for all cases where the hint is a contra pair."))

(defmethod crihelp=case-compute-output ((case crihelp+case-contrapair) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (contra-pair (crihelp~case-hint case))
	 (params (if (null (listp (first contra-pair)))
		     ;; -> first element of contra-pair is not a list
		     ;; -> we have no cartesian products!
		     (mapcar #'(lambda (num)
				 (term~constant-create num (env~lookup-object 'num env)))
			     contra-pair)
		   ;; We have cartesian products
		   (apply #'append (mapcar #'(lambda (tupel)
					       (mapcar #'(lambda (num)
							   (term~constant-create num (env~lookup-object 'num env)))
						       tupel))
					   contra-pair))))
	 (following-exists (crihelp=following-exists (crihelp=formula-of-insttask inst-task))))
    (nth (- (length params) (+ following-exists 1)) params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the base case class for hints in form of numi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-numi (crihelp+case)
  ()
  (:documentation "The base class for all cases where the hint is numi."))

(defmethod crihelp=case-compute-output ((case crihelp+case-numi) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (numi (crihelp~case-hint case))
	 (params (if (listp numi)
		     ;; numi is list -> we have a cartesian product
		     ;; -> we have no cartesian products!
		     (mapcar #'(lambda (num)
				 (term~constant-create num (env~lookup-object 'num env)))
			     numi)
		   ;; We have not a cartesian product
		   (list  (term~constant-create numi (env~lookup-object 'num env)))))
	 (following-exists (crihelp=following-exists (crihelp=formula-of-insttask inst-task))))
    (nth (- (length params) (+ following-exists 1)) params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-closed-under case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-closure-not (crihelp+case-1 crihelp+case-neg crihelp+case-contrapair)
  ()
  (:documentation "The not-closed-under case."))

(defmethod crihelp=case-test ((case crihelp+case-closure-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated closed-under formula."))
  (crihelp=negated-p 'crihelp=closure-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-closure-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-closure (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-commutative case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-commut-not (crihelp+case-1 crihelp+case-neg crihelp+case-contrapair)
  ()
  (:documentation "The not-commutative case."))

(defmethod crihelp=case-test ((case crihelp+case-commut-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated commutative formula."))
  (crihelp=negated-p 'crihelp=commut-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-commut-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-commutativity (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-associative case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-assoc-not (crihelp+case-1 crihelp+case-neg crihelp+case-contrapair)
  ()
  (:documentation "The not-associative case."))

(defmethod crihelp=case-test ((case crihelp+case-assoc-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated associative formula."))
  (crihelp=negated-p 'crihelp=assoc-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-assoc-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-associativity (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the unit case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-unit (crihelp+case-1 crihelp+case-pos crihelp+case-numi)
  ()
  (:documentation "The unit case."))

(defmethod crihelp=case-test ((case crihelp+case-unit))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a unit formula."))
  (crihelp=unit-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-unit))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-unit-element (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-unit case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-unit-not (crihelp+case-1 crihelp+case-neg)
  ()
  (:documentation "The not-unit case."))

(defmethod crihelp=case-test ((case crihelp+case-unit-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated unit formula."))
  (crihelp=negated-p 'crihelp=unit-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-unit-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-unit-element (crihelp~case-1-table case)))

(defmethod crihelp=case-compute-output ((case crihelp+case-unit-not) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let* ((formula (crihelp~case-formula case))
	 (env (pds~environment omega*current-proof-plan))
	 (pair-list (crihelp~case-hint case))
	 (param-pair-list (if (listp (first (first pair-list)))
			      ;; we have cartesian products
			      (mapcar #'(lambda (pair)
					  (list (mapcar #'(lambda (num)
							    (term~constant-create num (env~lookup-object 'num env)))
							(first pair))
						(mapcar #'(lambda (num)
							    (term~constant-create num (env~lookup-object 'num env)))
							(second pair))))
				      pair-list)
			    ;; we have no cartesian products!
			    (mapcar #'(lambda (pair)
					(list (list (term~constant-create (first pair) (env~lookup-object 'num env)))
					      (list (term~constant-create (second pair) (env~lookup-object 'num env)))))
				    pair-list))))
    (multiple-value-bind (following-exists form)
	(crihelp=following-exists (crihelp=formula-of-insttask inst-task))
      
      ;;(format t "~%~%PARAM-PAIR-LIST: ~A" param-pair-list)
      ;;(format t "~%~%FORM: ~A" form)
      
      (when (and (logic~disjunction-p form)
		 (every #'(lambda (arg)					   
			    (and (logic~negation-p arg)
					      (data~appl-p (first (data~appl-arguments arg)))
					      (keim~equal (data~appl-function (first (data~appl-arguments arg)))
							  (data~schema-range (env~lookup-object '= env)))))
			(data~appl-arguments form)))
	;; form is a disjunction of unequations
	(let* ((equations (mapcar #'(lambda (arg)
				      (first (data~appl-arguments arg)))
					       (data~appl-arguments form)))
	       (1eq (first equations))
	       (2eq (second equations))
	       (op (second (data~appl-arguments
			    (data~abstr-range
			     (first (data~appl-arguments
				     (first (data~appl-arguments formula))))))))
	       (extract-arguments-of-1eq (crihelp=extract-arguments (first (data~appl-arguments 1eq)) op))
	       (extract-arguments-of-2eq (crihelp=extract-arguments (first (data~appl-arguments 2eq)) op))
	       ;; Since we write in our theory the equations as: (= (op a e) a) and  (= (op e a) a)
	       ;; the first argument is always the composed one whereas the second one is only a constant
	       (notunitarg (cond ((second extract-arguments-of-1eq)
					       ;; the first equation is (= (op a e) a), hence a is the first argument of the
				  ;; composed term (op a e)
				  (crihelp=extract-tupel (second extract-arguments-of-1eq))
				  ;; (second (data~appl-arguments (first extract-arguments-of-1eq)))
				  ;; a has the form (resclass n m), but we need only the m to instantiate it
				  ;; hence, the (second (data~appl-arguments ...))
				  )
				 ((first extract-arguments-of-2eq)
				  ;; the second equation is (= (op e a) a), hence a is the second argument of the
				  ;; composed term (op e a)
				  (crihelp=extract-tupel (first extract-arguments-of-2eq))
				  ;;(second (data~appl-arguments (second extract-arguments-of-2eq)))
				  )
				 (t
				  ;; if a cannot be reconstructed -> no advice possible
				  nil))))
	  
	  (when (and notunitarg
		     (every #'term~number-p notunitarg))
	    (let* ((params (second (assoc notunitarg param-pair-list :test #'keim~equal))))
	      
	      ;;(format t "~%~%PARAMS: ~A" params)
	      (nth (- (length params) (+ following-exists 1)) params))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the inverse-exists case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-inverse (crihelp+case-1 crihelp+case-pos)
  ()
  (:documentation "The inverse-exists case."))

(defmethod crihelp=case-test ((case crihelp+case-inverse))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is an inverse-exists formula."))
  (crihelp=inverse-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-inverse))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-inverses (crihelp~case-1-table case)))

(defmethod crihelp=case-compute-output ((case crihelp+case-inverse) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let* ((formula (crihelp~case-formula case))
	 (env (pds~environment omega*current-proof-plan))
	 (pair-list (crihelp~case-hint case))
	 (param-pair-list (if (listp (first (first pair-list)))
			      ;; we have cartesian products
			      (mapcar #'(lambda (pair)
					  (list (mapcar #'(lambda (num)
							    (term~constant-create num (env~lookup-object 'num env)))
							(first pair))
						(mapcar #'(lambda (num)
							    (term~constant-create num (env~lookup-object 'num env)))
							(second pair))))
				      pair-list)
			    ;; we have no cartesian products!
			    (mapcar #'(lambda (pair)
					(list (list (term~constant-create (first pair) (env~lookup-object 'num env)))
					      (list (term~constant-create (second pair) (env~lookup-object 'num env)))))
				    pair-list))))
    (multiple-value-bind (following-exists form)
	(crihelp=following-exists (crihelp=formula-of-insttask inst-task))
      
      ;;(format t "~%~%PARAM-PAIR-LIST: ~A" param-pair-list)
      ;;(format t "~%~%FORM: ~A" form)
      
      (when (and (logic~conjunction-p form)
		 (every #'(lambda (arg)
			    (and (data~appl-p arg)
				 (keim~equal (data~appl-function arg)
					     (data~schema-range (env~lookup-object '= env)))))
			(data~appl-arguments form)))
	;; form is a conjunction of equations
	(let* ((equations (data~appl-arguments form))
	       (1eq (first equations))
	       (2eq (second equations))
	       (op (second (data~appl-arguments formula)))
	       (extract-arguments-of-1eq (crihelp=extract-arguments (first (data~appl-arguments 1eq)) op))
	       (extract-arguments-of-2eq (crihelp=extract-arguments (first (data~appl-arguments 2eq)) op))
	       ;; Since we write in our theory the equations as: (= (op a x) e) and  (= (op x a) e)
	       ;; the first argument is always the composed one whereas the second one is only a constant
	       (inversearg (cond ((first extract-arguments-of-1eq)
				  ;; the first equation is (= (op a x) e), hence a is the first argument of the
				  ;; composed term (op a x)
				  (crihelp=extract-tupel (first extract-arguments-of-1eq))
				  ;; (second (data~appl-arguments (first extract-arguments-of-1eq)))
				  ;; a has the form (resclass n m), but we need only the m to instantiate it
				  ;; hence, the (second (data~appl-arguments ...))
				  )
				 ((second extract-arguments-of-2eq)
				  ;; the second equation is (= (op x a) e), hence a is the second argument of the
				  ;; composed term (op x a)
				  (crihelp=extract-tupel (second extract-arguments-of-2eq))
				  ;;(second (data~appl-arguments (second extract-arguments-of-2eq)))
				  )
				 (t
				  ;; if a cannot be reconstructed -> no advice possible
				  nil))))
	  (when (every #'term~number-p inversearg)
	    (let* ((params (second (assoc inversearg param-pair-list :test #'keim~equal))))
	      
	      ;;(format t "~%~%PARAMS: ~A" params)
	      (nth (- (length params) (+ following-exists 1)) params))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-inverse-exists case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-inverse-not (crihelp+case-1 crihelp+case-neg crihelp+case-numi)
  ()
  (:documentation "The not-inverse-exists case."))

(defmethod crihelp=case-test ((case crihelp+case-inverse-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated inverse-exists formula."))
  (crihelp=negated-p 'crihelp=inverse-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-inverse-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-inverses (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the divisor-exists case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-divisor (crihelp+case-1 crihelp+case-pos)
  ()
  (:documentation "The divisor-exists case."))

(defmethod crihelp=case-test ((case crihelp+case-divisor))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a divisor-exists formula."))
  (crihelp=divisor-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-divisor))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-divisors (crihelp~case-1-table case)))

(defmethod crihelp=case-compute-output ((case crihelp+case-divisor) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let ((formula (crihelp~case-formula case))
	(env (pds~environment omega*current-proof-plan))
	(param-pair (crihelp~case-hint case)))
    (multiple-value-bind (following-exists form)
	(crihelp=following-exists (crihelp=formula-of-insttask inst-task))
      (if (not (and (data~appl-p form)
		    (keim~equal (data~appl-function form)
				(data~schema-range (env~lookup-object '= env)))))
	  nil
	;; form is equation
	(let* ((args (data~appl-arguments form))
	       (composed-arg (first args))
	       (b-arg (crihelp=extract-tupel (second args)))
	       ;; in the equations (= (op a x) b) respectively (= (op y a) b), b the form (resclass n m),
	       ;; but we need only the m to instantiate it,  hence, the (second (data~appl-arguments ...)) ...
	       (op (second (data~appl-arguments formula)))
	       (extract-arguments-of-eq (crihelp=extract-arguments composed-arg op))
	       ;; Since we write in our theory the equations as (= (op a x) b) respectively (= (op y a) b)
	       ;; the composed argument is always the first argument of the equation
	       )
	  
	  ;;(format t "~%FORM: ~A" form)
	  ;;(format t "~%COMPOSED-ARG: ~A" composed-arg)
	  ;;(format t "~%b-arg: ~A" b-arg)
	  ;;(format t "~%op: ~A" op)
	  ;;(format t "~%extract-arguments-of-eq: ~A" extract-arguments-of-eq)
	  
	  (when (and (first extract-arguments-of-eq)
		     (second extract-arguments-of-eq))
	    ;; we need to be able to reconstruct both a and y,x ...
	    (let* ((mv (agenda~inst-task-meta-var inst-task))
		   (arg1 (first extract-arguments-of-eq))
		   (arg2 (second extract-arguments-of-eq))
		   (pre-a-arg (if (find mv (data~all-substructs arg1))
				  ;; mv is in arg1 -> then a is the second arg (we have the case (= (op y a) b))
				  arg2
				;; otherwise, a is the first argument (we have the case (= (op a x) b))
				arg1))
		   (a-arg (crihelp=extract-tupel pre-a-arg))
		   (side (if (find mv (data~all-substructs arg1))
			     'left ;; mv is left argument -> left divisor
			   'right ;; mv is right argument -> right divisor
			   ))
		   ;; (arg1 (crihelp=extract-tupel (first extract-arguments-of-eq)))
		   ;;(arg2 (crihelp=extract-tupel (second extract-arguments-of-eq)))
				;;;; in the equations (= (op a x) b) respectively (= (op y a) b), a and x have the form (resclass n m?),
				;;;; but we need only the m? to instantiate it,  hence, the (second (data~appl-arguments ...)) ...
		   ;;(a-arg (if (remove-if-not #'meta~p arg1)
		   ;;	   ;; arg1 contains meta-variable, then a is the second arg (we have the case (= (op y a) b))
		   ;;	   arg2
		   ;;	 ;; otherwise, a is the first argument (we have the case (= (op a x) b))
		   ;;	 arg1))
		   ;;(side (if (remove-if-not #'meta~p arg1)
		   ;;	  'left ;; mv is left argument -> left divisor
		   ;;	'right ;; mv is right argument -> right divisor
		   ;;	))
		   (divisors (rcl~table-pair2divisors (crihelp~case-1-table case)
						      (if (= (length a-arg) 1)
							  ;; -> keine Tupel
							  (list (keim~name (first a-arg))
								(keim~name (first b-arg)))
							(list (mapcar #'keim~name a-arg)
							      (mapcar #'keim~name b-arg)))))
		   (divisor-params (if (listp (first divisors))
				       ;; -> tupels
				       (list (mapcar #'(lambda (num)
							 (term~constant-create num (env~lookup-object 'num env)))
						     (first divisors))
					     (mapcar #'(lambda (num)
							 (term~constant-create num (env~lookup-object 'num env)))
						     (second divisors)))
				     (mapcar #'(lambda (num)
						 (list (term~constant-create num (env~lookup-object 'num env))))
					     divisors))))
	      
	      ;;(format t "~%a-arg: ~A" a-arg)
	      ;;(format t "~%side: ~A" side)
	      ;;(format t "~%divisors: ~A" divisors)
	      ;;(format t "~%divisor-params: ~A" divisor-params)
	      
	      (cond ((null divisor-params)
		     nil)
		    ((equal side 'left)
		     (nth (- (length (second divisor-params)) (+ following-exists 1)) (second divisor-params)))
		    ((equal side 'right)
		     (nth (- (length (first divisor-params)) (+ following-exists 1)) (first divisor-params)))))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-divisor-exists case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-divisor-not (crihelp+case-1 crihelp+case-neg crihelp+case-contrapair)
  ()
  (:documentation "The not-divisor-exists case."))

(defmethod crihelp=case-test ((case crihelp+case-divisor-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated divisor-exists formula."))
  (crihelp=negated-p 'crihelp=divisor-p (crihelp~case-formula case)))

(defmethod crihelp=case-compute-hint ((case crihelp+case-divisor-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-divisors (crihelp~case-1-table case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the order case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-order (crihelp+case-pos)
  ((tables               :initform nil
			 :accessor crihelp~case-order-tables
			 :initarg :tables)
   (set1                 :initform nil
			 :accessor crihelp~case-order-set1
			 :initarg :set1)
   (op1                  :initform nil
			 :accessor crihelp~case-order-op1
			 :initarg :op1)
   )
  (:documentation "The order case."))

(defmethod crihelp=case-test ((case crihelp+case-order))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is an order formula."))
  (strathelp=order-of-element-goal-p (crihelp~case-formula case)))

(defun crihelp=extract-sets-and-ops (formula)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "A order-formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "Set1, op1, set2, op2."))
  (let* ((envi (env~create (pds~environment omega*current-proof-plan))))
    (post~read-object 'type-var envi :type-variable)
    (post~read-object '(;;(set1 (o (o num))) (set2 (o (o num)))
			;;(op1 ((o num) (o num) (o num))) (op2 ((o num) (o num) (o num)))
			(set1 (o type-var)) (set2 (o type-var))
			(op1 (type-var type-var type-var)) (op2 (type-var type-var type-var))
			(ibound num))
		      envi :variables-multiple)
    (let* ((matcher (term~alpha-match (post~read-object '(exists-sort (lam (n num)
									   (and (exists-sort (lam (el1 type-var)
												  (order-of-element set1 op1 el1 n))
											     set1)
										(not (exists-sort (lam (el2 type-var)
												       (order-of-element set2 op2 el2 n))
												  set2))))
								      (integer-intervall 1 ibound))
							envi
							:existing-term)
				      formula)))
      (values (subst~apply matcher (env~lookup-object 'set1 envi))
	      (subst~apply matcher (env~lookup-object 'op1 envi))
	      (subst~apply matcher (env~lookup-object 'set2 envi))
	      (subst~apply matcher (env~lookup-object 'op2 envi))))))

(defmethod crihelp=case-compute-multtable ((case crihelp+case-order))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "Computes the multiplication tables and stores it in the slot of the case.")
	   (value   "Undefined."))
  (let ((formula (crihelp~case-formula case)))
    (multiple-value-bind
	(set1 op1 set2 op2)
	(crihelp=extract-sets-and-ops formula)
      (setf (crihelp~case-order-set1 case) set1)
      (setf (crihelp~case-order-op1 case) op1)
      (setf (crihelp~case-order-tables case) (crihelp=multiplication-tables-from-sets-and-ops set1 op1 set2 op2)))))
      
(defun crihelp=find-order-pair (tables)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "Two multiplication tables.")
	   (effect  "None.")
	   (value   "Checks whether in the first table is an element x with an order n, such that in the"
		    "second table there is no such element. Return multiple-value:"
		    "First: Success flag"
		    "Second: (if success) list of x and n"))
  (let* ((order-list1 (rcl~element-orders (first tables)))
	 (order-list2 (rcl~element-orders (second tables)))
	 (orders2 (remove-duplicates (apply #'append (mapcar #'(lambda (pair)
								 (if (null (second pair))
								     nil
								   (list (second pair))))
							     order-list2))))
	 (pairs1 (remove-if #'(lambda (pair)
				(let* ((el (first pair))
				       (order (second pair)))
				  (if (or (null order)
					  (find order orders2))
				      't
				    nil)))
			    order-list1)))
    (if pairs1
	(values 't
		(first pairs1))
      (values nil nil))))

(defmethod crihelp=case-compute-hint ((case crihelp+case-order))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (crihelp=find-order-pair (crihelp~case-order-tables case)))

(defun crihelp=first-order-of-element-p (form set1 op1 n)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "An existentially quantified formula, the set and the operation of the first structure of"
		    "a order-problem, and the order of the element we are looking for.")
	   (effect  "None.")
	   (value   "T if the input formula has the body (after eliminating all leading quantifiers)"
		    "(order-of-element set1 op1 X n)."))
  (multiple-value-bind
      (leading-ex rest-formula)
      (crihelp=following-exists form)

    (and (data~appl-p rest-formula)  
	 (keim~equal (data~appl-function rest-formula)
	   	     (data~schema-range (env~lookup-object 'order-of-element (pds~environment omega*current-proof-plan))))
	 (keim~equal set1 (first (data~appl-arguments rest-formula)))
	 (keim~equal op1 (second (data~appl-arguments rest-formula)))
	 (keim~equal n (fourth (data~appl-arguments rest-formula))))))
  
(defmethod crihelp=case-compute-output ((case crihelp+case-order) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (set1 (crihelp~case-order-set1 case))
	 (op1 (crihelp~case-order-op1 case))
	 (pair (crihelp~case-hint case))
	 (n-param (term~constant-create (second pair) (env~lookup-object 'num env)))
	 (x-param (if (null (listp (first pair)))
		      ;; -> second elemengt of pair is not a list
		      ;; -> we have no cartesian products!
		      (list (term~constant-create (first pair) (env~lookup-object 'num env)))
		    ;; We have cartesian products
		    (mapcar #'(lambda (num)
				(term~constant-create num (env~lookup-object 'num env)))
			    (first pair)))))
    (if (eq (crihelp=just-of-insttask inst-task) (node~justification (crihelp~case-node case)))
	n-param
      (let* ((form (crihelp=update-formula! (crihelp=mainprem-of-insttask inst-task)))
	     (following-exists (crihelp=following-exists form)))
	(if (crihelp=first-order-of-element-p form set1 op1 n-param)
	    (nth (- (length x-param) (+ following-exists 1)) x-param)
	  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the base case class for all cases with two tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-2 (crihelp+case)
  ((table1               :initform nil
			 :accessor crihelp~case-2-table1
			 :initarg :table1)
   (table2               :initform nil
			 :accessor crihelp~case-2-table2
			 :initarg :table2)
   )
  (:documentation "The base class for all cases with two tables."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the isomorphic case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-iso (crihelp+case-2 crihelp+case-pos)
  ()
  (:documentation "The isomorphic case."))

(defmethod crihelp=case-test ((case crihelp+case-iso))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is an isomorphic formula."))
  (crihelp=iso-p (crihelp~case-formula case)))

(defun crihelp=multiplication-tables-from-sets-and-ops (set1 op1 set2 op2)
  (multiple-value-bind
      (sets1 ops1)
      (crihelp=decompose-cartproduct-sets-and-ops set1 op1)
    (multiple-value-bind
	(sets2 ops2)
	(crihelp=decompose-cartproduct-sets-and-ops set2 op2)
      
      (let* ((class-factors1 (mapcar #'zmztac=class-factor sets1))
	     (class-factor-numbers1 (mapcar #'keim~name class-factors1))
	     (number-lists1 (mapcar #'crihelp=number-set-to-resclass-set sets1))
	     (operations-on-nums1 (mapcar #'(lambda (op class-factor)
					      (crihelp=convert-operation op class-factor nil))
					  ops1 class-factors1))
	     (table1 (if (= (length sets1) 1)
			 (rcl~multiplication-table (first number-lists1)
						   (first class-factor-numbers1)
						   :operation (first operations-on-nums1))
		       (rcl~product-multiplication-table number-lists1
							 :modulo class-factor-numbers1
							 :operation operations-on-nums1)))
	     
	     (class-factors2 (mapcar #'zmztac=class-factor sets2))
	     (class-factor-numbers2 (mapcar #'keim~name class-factors2))
	     (number-lists2 (mapcar #'crihelp=number-set-to-resclass-set sets2))
	     (operations-on-nums2 (mapcar #'(lambda (op class-factor)
					      (crihelp=convert-operation op class-factor nil))
					  ops2 class-factors2))
	     (table2 (if (= (length sets2) 1)
			 (rcl~multiplication-table (first number-lists2)
						   (first class-factor-numbers2)
						   :operation (first operations-on-nums2))
		       (rcl~product-multiplication-table number-lists2
							 :modulo class-factor-numbers2
							 :operation operations-on-nums2))))
	
	(list table1 table2)))))


(defun crihelp=scalar-multiplication-tables-from-sets-and-ops (ring set1 scop1 set2 scop2)
  (multiple-value-bind
      (sets1 ops1)
      (crihelp=decompose-cartproduct-sets-and-ops set1 scop1 :operation-composer 'scalar-operation)
    (multiple-value-bind
	(sets2 ops2)
	(crihelp=decompose-cartproduct-sets-and-ops set2 scop2 :operation-composer 'scalar-operation)

      (multiple-value-bind
	  (success1 table1)
	  (crihelp=create-scalar-multtable ring sets1 ops1)
	(multiple-value-bind
	    (success2 table2)
	    (crihelp=create-scalar-multtable ring sets2 ops2)
	  (if (and success1
		   success2)
	      (list table1 table2)
	    nil))))))

(defun crihelp=create-scalar-multtable (ring sets ops)
  (let* ((ring-class-factor (zmztac=class-factor ring))
	 (ring-class-factor-number (keim~name ring-class-factor))
	 (class-factors (mapcar #'zmztac=class-factor sets))
	 (class-factor-numbers (mapcar #'keim~name class-factors)))
    (if (null (every #'(lambda (fac-num)
			 ;;(or (= (mod fac-num ring-class-factor-number) 0)
			 ;; the modulo factor of the ring is divided by each of the modulo factors of the
			 ;; scalar elements
			 (= (mod ring-class-factor-number fac-num) 0))
		     class-factor-numbers))
	
	(progn
	  (omega~error "In function crihelp=create-scalar-multtable: only well-defined if the modulo factor of the ring devides the modulo factors of the modules or viceversa!")
	  (values nil nil))
      
      (let* ((checked-class-factors (mapcar #'(lambda (class-factor class-factor-number)
						(if (= (mod class-factor-number ring-class-factor-number) 0)
						    ring-class-factor
						  class-factor))
					    class-factors class-factor-numbers))
	     (checked-class-factor-numbers (mapcar #'keim~name checked-class-factors))
	     (number-lists (mapcar #'crihelp=number-set-to-resclass-set sets))
	     (ring-number-list (crihelp=number-set-to-resclass-set ring))
	     (operations-on-nums (mapcar #'(lambda (op class-factor)
					     (crihelp=convert-operation op class-factor nil))
					 ops checked-class-factors))
	     (table (if (= (length sets) 1)
			(rcl~scalar-multiplication-table ring-number-list
							 (first number-lists)
							 (first checked-class-factor-numbers)
							 :operation (first operations-on-nums))
		      (rcl~product-scalar-multiplication-table ring-number-list
							       number-lists
							       :modulo checked-class-factor-numbers
							       :operation operations-on-nums))))
	(values t table)))))


(defun crihelp=compute-isomorphism-multiplication-table! (node)
  (let* ((pre-formula (node~formula node))
	 (formula (if (logic~negation-p pre-formula)
		      (first (data~appl-arguments pre-formula))
		    pre-formula))
	 (args (data~appl-arguments formula))
	 (set1 (first args))
	 (op1 (second args))
	 (set2 (third args))
	 (op2 (fourth args)))
    (crihelp=multiplication-tables-from-sets-and-ops set1 op1 set2 op2)))



(defgeneric crihelp=compute-module-isomorphism-multiplication-table! (node)
  (:method ((node node+node))
	   (crihelp=compute-module-isomorphism-multiplication-table! (node~formula node)))
  (:method ((pre-formula term+term))	   
	   (let* ((formula (if (logic~negation-p pre-formula)
			       (first (data~appl-arguments pre-formula))
			     pre-formula))
		  (args (data~appl-arguments formula))
		  (set1 (first args))
		  (op1 (second args))
		  (set2 (third args))
		  (op2 (fourth args))
		  (ring (fifth args))
		  (scop1 (sixth args))
		  (scop2 (seventh args)))
	     
	     (append (crihelp=multiplication-tables-from-sets-and-ops set1 op1 set2 op2)
		     (crihelp=scalar-multiplication-tables-from-sets-and-ops ring set1 scop1 set2 scop2)))))
  


(defmethod crihelp=case-compute-multtable ((case crihelp+case-iso))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "Computes the multiplication tables and stores it in the slot of the case.")
	   (value   "Undefined."))
  (let* ((tables (crihelp=compute-isomorphism-multiplication-table! (crihelp~case-node case))))
    (setf (crihelp~case-2-table1 case) (first tables)
	  (crihelp~case-2-table2 case) (second tables))))

(defun crihelp=check-isomorphism (table1 table2)
  (multiple-value-bind
      (success1 hints1)
      (rcl~sem-isomorphism table1 table2)
    (if success1
 	(values success1
 		hints1)
      (multiple-value-bind
 	  (success2 hints2)
 	  (rcl~check-isomorphism table1 table2)
 	(if success2
 	    (values success2
 		    hints2)
 	  nil)))))

(defun crihelp=check-module-isomorphism (module1-table module2-table ring-module1-table ring-module2-table)
  (multiple-value-bind
      (success1 hints1)
      (rcl~sem-module-isomorphism module1-table module2-table ring-module1-table ring-module2-table)
    (if success1
 	(values success1
 		hints1)
      nil)))
;; so far no check with Maple was realized!

(defmethod crihelp=case-compute-hint ((case crihelp+case-iso))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (let* ((formula (crihelp~case-formula case)))
    (cond ((crihelp=applfunc-equals 'isomorphic formula)
	   (crihelp=check-isomorphism (crihelp~case-2-table1 case) (crihelp~case-2-table2 case)))
	  ((crihelp=applfunc-equals 'module-isomorphic formula)
	   (let* ((tables (crihelp=compute-module-isomorphism-multiplication-table! formula))
		  (module1-table (first tables))
		  (module2-table (second tables))
		  (ring-module1-table (third tables))
		  (ring-module2-table (fourth tables)))
	     (crihelp=check-module-isomorphism module1-table module2-table ring-module1-table ring-module2-table)))
	  (T
	   (omega~error "~%Something went wrong in function crihelp=case-compute-hint for case crihelp+case-iso.")))))	   
(defun crihelp=extract-isomorphic-function (just)
  (let* ((subst (meth~mapp-subst (pdsj~subst just)))
	 (assoc-list (mapcar #'list (subst~domain subst) (subst~codomain subst)))
	 (equation (second (assoc 'func-equation assoc-list :test #'(lambda (it1 it2)
								      (string-equal it1 (keim~name it2)))))))
    (second (data~appl-arguments equation))))

(defun crihelp=break-pairs (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (do* ((rest-list (list formula))
	  (back-list nil))
	((null rest-list)
	 back-list)
      (let* ((head (first rest-list)))
	(if (and (data~appl-p head)
		 (keim~equal (data~appl-function head)
			     (data~schema-range (env~lookup-object 'pair env))))
	    (setf rest-list (append (data~appl-arguments head) (rest rest-list)))
	  (progn
	    (setf back-list (append back-list (list head)))
	    (setf rest-list (rest rest-list))))))))

(defun crihelp=find-equation-to-mv (conjunction-of-implications mv)
  (do* ((current-formulas (list conjunction-of-implications))
	(x-back-value nil)
	(y-back-value nil))
      ((or x-back-value
	   (null current-formulas))
       (if x-back-value
	   (values x-back-value
		   y-back-value)
	 nil))
    (let* ((head-formula (first current-formulas)))
      (if (logic~conjunction-p head-formula)
	  (setf current-formulas (append (data~appl-arguments head-formula) (rest current-formulas)))
	(let* ((implication-args (data~appl-arguments head-formula))
	       (y-equation (second implication-args))
	       (y-side-stuff (data~all-substructs (second (data~appl-arguments y-equation)))))
	  
	  ;;(y-mv (if (data~appl-p (second (data~appl-arguments y-equation)))
	  ;;	  (second (data~appl-arguments (second (data~appl-arguments y-equation))))
	  ;;	nil)))
	  ;;(when (eq y-mv mv)
	  
	  (when (find mv y-side-stuff)
	    (setf x-back-value (first implication-args))
	    (setf y-back-value y-equation))
	  
	  (setf current-formulas (rest current-formulas)))))))

(defun crihelp=extract-isomorphic-value-for-mv (mv function pairs)
  (declare (edited  "29-JUN-2000")
	   (authors Ameier)
	   (input   "A meta-variable, a keim-function expression, and a function expressed as set of pairs"
		    "of numbers."
		    "Thereby, the function-expression has the form"
		    "(lam x (that (lam y (and ((x = (RCL n1)) => (y = (RCL mv_1)))"
		    "                          ...")
	   (effect  "None.")
	   (value   "First the implication of the function expression corresponding to the input meta-variable"
		    "is computed, for instance ((x = (RCL n_n)) => (y = (RCL mv))). Then the corresponding x value"
		    "that means n_n is extracted and converted into a natural number N. Then we comupte from the"
		    "pair-list-function the corresponding function value to N, which is f(N). Finally we convert"
		    "the natural number f(N) into a keim-object and return this keim-object."))
  (let* ((that-expr (data~abstr-range function))
	 (lam-y-expr (first (data~appl-arguments that-expr)))
	 (conjunction-of-implications (data~abstr-range lam-y-expr)))

    (multiple-value-bind
	(x-equation-of-mv y-equation-of-mv)
	(crihelp=find-equation-to-mv conjunction-of-implications mv)

      (let* ((x-value (crihelp=extract-tupel (second (data~appl-arguments x-equation-of-mv))))
	     (x-numbers (mapcar #'keim~name x-value))
	     (y-values (mapcar #'data~all-substructs
			       (crihelp=break-pairs (second (data~appl-arguments y-equation-of-mv)))))
	     (mv-pos (position mv y-values :test #'(lambda (mvit substruct-list)
						     (find mvit substruct-list))))
	     (mv-tupel (if (= (length x-numbers) 1)
			   (second (assoc (first x-numbers) pairs))
			 (second (find x-numbers pairs :test #'(lambda (it1 pair)
								 (equal it1 (first pair)))))))
	     (mv-number (nth mv-pos (if (listp mv-tupel)
					mv-tupel
				      (list mv-tupel)))))
	(term~constant-create mv-number (env~lookup-object 'num (pds~environment omega*current-proof-plan)))))))

(defun crihelp=all-conjuncts (formula)
  (declare (edited  "26-APR-2001")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A list of all conjuncts of a formula. That is, if the formula is not"
		    "a conjunction a list containing the formula itself only. If the formula"
		    "is a conjunction the conjuncts are also recursively decomposed."))
  (if (logic~conjunction-p formula)
      (let* ((args (data~appl-arguments formula)))
	(append (crihelp=all-conjuncts (first args))
		(crihelp=all-conjuncts (second args))))
    (list formula)))

(defun crihelp=isomorphism-mv-test-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (multiple-value-bind
	(leading-ex rest-formula)
	(crihelp=following-exists formula)
      (let* ((all-conjuncts (crihelp=all-conjuncts rest-formula)))
	(and (remove-if-not #'(lambda (formula)
				(and (data~appl-p formula)
				     (keim~equal (data~appl-function formula)
						 (data~schema-range (env~lookup-object 'homomorphism env)))))
			    all-conjuncts)
	     (remove-if-not #'(lambda (formula)
				(and (data~appl-p formula)
				     (keim~equal (data~appl-function formula)
						 (data~schema-range (env~lookup-object 'surjective env)))))
			    all-conjuncts)
	     (remove-if-not #'(lambda (formula)
				(and (data~appl-p formula)
				     (keim~equal (data~appl-function formula)
						 (data~schema-range (env~lookup-object 'injective env)))))
			    all-conjuncts))))))

(defmethod crihelp=case-compute-output ((case crihelp+case-iso) inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object and an instantiation task.")
	   (effect  "None.")
	   (value   "The instantiation for the instantiation task."))
  (let ((function-pairs (crihelp~case-hint case)))
    (if (crihelp=isomorphism-mv-test-p (crihelp=formula-of-insttask inst-task))
	;; -> We have a meta-variable that stands for a homormorphism
	;; -> in all other cases we have other meta-variables for which we can provide
	;;    no hints!!
	(let* ((isomorphic-function (crihelp=extract-isomorphic-function (crihelp=just-of-insttask inst-task)))
	       (value-for-mv (crihelp=extract-isomorphic-value-for-mv (agenda~inst-task-meta-var inst-task)
								      isomorphic-function
								      function-pairs)))
	  value-for-mv)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the not-distributive case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crihelp+case-distrib-not (crihelp+case-2 crihelp+case-neg crihelp+case-contrapair)
  ()
  (:documentation "The not-distributive case."))

(defmethod crihelp=case-test ((case crihelp+case-distrib-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "True iff the formula is a negated distributive formula."))
  (crihelp=negated-p 'crihelp=distrib-p (crihelp~case-formula case)))

(defun crihelp=compute-multiplication-tables-not-distrib (node)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "Two multiplication tables."))
  (let* ((formula (node~formula node))
	 (distrib (first (data~appl-arguments formula)))
	 (args (data~appl-arguments distrib))
	 (set (first args))
	 (op1 (second args))
	 (op2 (third args)))
    (multiple-value-bind
	(sets1 ops1)
	(crihelp=decompose-cartproduct-sets-and-ops set op1)
      (multiple-value-bind
	  (sets ops2)
	  (crihelp=decompose-cartproduct-sets-and-ops set op2)
	(let* ((class-factors (mapcar #'zmztac=class-factor sets))
	       (class-factor-numbers (mapcar #'keim~name class-factors))
	       (number-lists (mapcar #'crihelp=number-set-to-resclass-set sets))
	       (operations1-on-nums (mapcar #'(lambda (op1 class-factor)
						(crihelp=convert-operation op1 class-factor nil))
					    ops1 class-factors))
	       (operations2-on-nums (mapcar #'(lambda (op2 class-factor)
						(crihelp=convert-operation op2 class-factor nil))
					    ops2 class-factors)))
	  (if (= (length sets) 1)
	      (values (rcl~multiplication-table (first number-lists) (first class-factor-numbers)
						:operation (first operations1-on-nums))
		      (rcl~multiplication-table (first number-lists) (first class-factor-numbers)
						:operation (first operations2-on-nums)))
	    (values (rcl~product-multiplication-table number-lists
						      :modulo class-factor-numbers
						      :operation operations1-on-nums)
		    (rcl~product-multiplication-table number-lists
						      :modulo class-factor-numbers
						      :operation operations2-on-nums))))))))
  
(defmethod crihelp=case-compute-multtable ((case crihelp+case-distrib-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "Computes the multiplication tables and stores it in the slots of the case.")
	   (value   "Undefined."))
  (multiple-value-bind (table1 table2)
      (crihelp=compute-multiplication-tables-not-distrib (crihelp~case-node case))
    (setf (crihelp~case-2-table1 case) table1
	  (crihelp~case-2-table2 case) table2)))
    
(defmethod crihelp=case-compute-hint ((case crihelp+case-distrib-not))
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A case object.")
	   (effect  "None.")
	   (value   "The hint for this case class."))
  (rcl~check-distributivity (crihelp~case-2-table1 case) (crihelp~case-2-table2 case)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special cases (with old hint system)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; old hint system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the variable crihelp*current-hint computed hints are stored, crihelp*current-hint will contain a list with:
;; 1.) The current task
;; 2.) The success flag
;; 3.) The hint list

(defvar crihelp*current-hint nil "only used for special case in inst-from-paramII")

(defun crihelp=current-hint-is-actual-p (start-task)
  (declare (edited  "08-JUN-2000")
 	   (authors Ameier)
 	   (input   "The current start-task.")
 	   (effect  "None.")
 	   (value   "T if the information stored in crihelp*current-hint is the information computed"
 		    "for the current start-task (this is if the first element of crihelp*current-hint is the"
 		    "current start-task."))
  (if (listp crihelp*current-hint)
      (eq (first crihelp*current-hint) start-task)
    nil))

(defun crihelp=get-stored-hints ()
  (declare (edited  "08-JUN-2000")
 	   (authors Ameier)
 	   (input   "None.")
 	   (effect  "None.")
 	   (value   "Multiple-value"
 		    "First: The success-flag information of the crihelp*current-hint."
 		    "Second: The parameter information of the crihelp*current-hint."))
  (values (second crihelp*current-hint)
 	  (third crihelp*current-hint)))

(defun crihelp=store-current-hints! (start-task success-flag parameter-hint)
  (declare (edited  "08-JUN-2000")
 	   (authors Ameier)
 	   (input   "The current start task and the hint information computed for it.")
 	   (effect  "Stores this information in crihelp*current-hint.")
 	   (value   "Undefined."))
  (setf crihelp*current-hint (list start-task success-flag parameter-hint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; param-to-inst-task-from-equ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=param-to-inst-task-from-equ (inst-task start-task goal-tasks)
  
  (let* ((formula (node~formula (agenda~task-node start-task)))
	 (env (pds~environment omega*current-proof-plan))
	 (mv (agenda~inst-task-meta-var inst-task))
	 (equation-task (first (remove-if-not #'(lambda (task)
						  (let* ((task-formula (node~formula (agenda~task-node task))))
						    (if (and (data~appl-p task-formula)
							     (keim~equal (data~appl-function task-formula)
									 (data~schema-range (env~lookup-object '= env))))
							;; so far: formula is equation
							(let* ((args (data~appl-arguments task-formula))
							       (arg1 (first args))
							       (arg2 (second args)))
							  (if (or (and (data~appl-p arg1)
								       (keim~equal (data~appl-function arg1)
										   (env~lookup-object 'mod env)))
								  (and (data~appl-p arg2)
								       (keim~equal (data~appl-function arg2)
										   (env~lookup-object 'mod env))))
							      ;; so far: formula is equation and at least one of the arguments
							      ;; is a mod term
							      (if (find mv (data~all-substructs task-formula))
								  ;; formula is equation, at least one of the arguments is a mod term
								  ;; and formula contains mv
								  't
								nil)
							    nil))
						      nil)))						      
					      goal-tasks)))
	 (equation-args (if equation-task
			    (data~appl-arguments (node~formula (agenda~task-node equation-task)))
			  nil))
	 (mod-fac (if equation-task
		      (if (and (data~appl-p (first equation-args))
			       (keim~equal (data~appl-function (first equation-args))
					   (env~lookup-object 'mod env)))
			  (second (data~appl-arguments (first equation-args)))
			(second (data~appl-arguments (second equation-args))))
		    nil))			  
	 (maple-solve (if equation-task
			  (rcl~solve-equation-with-maple (node~formula (agenda~task-node equation-task)) mv)
			nil))
	 (foralli-consts (crihelp=foralli-vars-in-current-roc)))
    
    (if (null maple-solve)

	nil
      
      (cond ((and (data~appl-p formula)
		  (keim~equal (data~appl-function formula)
			      (data~schema-range (env~lookup-object 'associative env))))
	     
	     ;; Start Task is a 'associative' case!
	     ;; We can provide no information
	     
	     nil)
	    
	    ((and (logic~existential-quantification-p formula)
		  (data~appl-p (logic~quantification-scope formula))
		  (keim~equal (data~appl-function (logic~quantification-scope formula))
			      (data~schema-range (env~lookup-object 'unit env))))
	     
	     ;; Start Task is a 'unit' case!
	     ;; We can provide as information the maple-solve only if the solution of mv
	     ;; depends on nothing
	     
	     (if (crihelp=compatible-p maple-solve nil)
		 maple-solve
	       nil))
	    
	    ((and (data~appl-p formula)
		  (or (keim~equal (data~appl-function formula)
				  (data~schema-range (env~lookup-object 'inverse-exist env)))
		      (keim~equal (data~appl-function formula)
				  (data~schema-range (env~lookup-object 'divisors-exist env)))))
	     
	     ;; Start Task is a 'inverse-exist' or 'divisors-exists' case!
	     ;; We can provide as information the maple-solve only if the solution of mv
	     ;; depends only on the foralli-consts
 
	     (if (crihelp=compatible-p maple-solve foralli-consts)
		 (term~appl-create (env~lookup-object 'mod env)
				   (list maple-solve mod-fac))
	       nil))))))

(defgeneric crihelp=compatible-p (term consts)
  (declare (edited  "22-MAR-2000")
	   (authors Ameier)
	   (input   "A formula and a set of constants.")
	   (effect  "None.")
	   (value   "T if the formula consists only of the functions plus,minus,times, and does not"
		    "contain the constants in the second argument."))
  (:method ((term term+appl) consts)
	   (let* ((env (pds~environment omega*current-proof-plan)))
	     (if (find (data~appl-function term) (list (env~lookup-object 'plus env)
						       (env~lookup-object 'minus env)
						       (env~lookup-object 'times env)))
		 (every #'(lambda (arg)
			    (crihelp=compatible-p arg consts))
			(data~appl-arguments term))
	       nil)))
  (:method ((term term+primitive) consts)
	   (cond ((find term consts :test #'data~equal)
		  't)
		 ((term~number-p term)
		  't)
		 (t
		  nil)))
  (:method ((term term+term) consts)
	   nil))

(defun crihelp=foralli-vars-in-current-roc ()
  (mapcar #'(lambda (step)
	      (let* ((subst (meth~mapp-subst (pdsj~subst (pdsc~an-just step))))
		     (domain (subst~domain subst))
		     (codomain (subst~codomain subst))
		     (pairs (mapcar #'list domain codomain))
		     (c-const (find 'c domain :test #'(lambda (letter struct)
							(string-equal letter (keim~name struct))))))
		
		(second (assoc c-const pairs))))			    
	  (remove-if-not #'(lambda (step)
			     (eq (just~method (pdsc~an-just step))
				 (infer~find-method 'ForallI-Sort-Resclass-m)))
			 (roc~pplanner-steps pplan*roc-state-description))))
			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTINJNOTISO CRIHELPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=param-to-inst-taskII (inst-task start-task)
  (let* ((node (agenda~task-node start-task))
	 (just (node~justification node))
	 (just-method-name (keim~name (just~method just)))
	 (flip (if (or (string-equal just-method-name 'NotIsoSym-m-b)
		       (string-equal just-method-name 'NotIsoSym-m))
		   't
		 nil))
	 (formula (if flip
		      (node~formula (first (just~premises just)))
		    (node~formula node)))
	 (task (if flip
		   (agenda~create-task (first (just~premises just)))
		 start-task))
	 (env (pds~environment omega*current-proof-plan)))
    
    ;;(format t "~%~%NODE: ~A ~A" (node~formula node) (node~justification node))
    ;;(format t "~%~%JUST-NAME: ~A" just-method-name)
    ;;(format t "~%~%FLIP: ~A" flip)
    ;;(format t "~%~%FORMULA: ~A" formula)

    (cond ((and (logic~negation-p formula)
		(data~appl-p (first (data~appl-arguments formula)))
		(keim~equal (data~appl-function (first (data~appl-arguments formula)))
			    (data~schema-range (env~lookup-object 'isomorphic env))))

	    (let* ((tables (crihelp=compute-isomorphism-multiplication-table! (agenda~task-node task))))

	      ;;(format t "~%~%TABLES: ~A" tables)
	      
	      (multiple-value-bind
		  (success non-injectives)
		  (if (crihelp=current-hint-is-actual-p task)
		      (crihelp=get-stored-hints)
		    (crihelp=check-non-isomorphism (first tables) (second tables)))

		(setq class*hint (list success non-injectives rcl*check-line))
		
		(if (null success)
		    nil
		  (let* ((set1 (first (data~appl-arguments (first (data~appl-arguments formula)))))
			 (class-factor1 (zmztac=class-factor set1))
			 (plan-step (agenda~inst-task-plan-step inst-task))
			 (node (pdsc~an-node plan-step))
			 (step-formula (node~formula node))
			 (leading-exists-number (crihelp=following-exists step-formula))
			 (hint (nth (- (length non-injectives) leading-exists-number) non-injectives)))
		    (term~appl-create (env~lookup-object 'resclass env)
				      (list class-factor1 (term~constant-create hint (env~lookup-object 'num env))))))))))))

(defun crihelp=check-non-isomorphism (table1 table2)
  (multiple-value-bind
      (success1 hints1)
      (rcl~sem-non-isomorphism table1 table2)
    (if success1
 	(values success1
 		hints1)
      (multiple-value-bind
 	  (success2 hints2)
	  (rcl~check-non-isomorphism table1 table2)
 	(if success2
 	    (values success2
 		    hints2)
 	  nil)))))

(defun crihelp=matching-with-maple (formula1 formula2 h class-factor)
  (multiple-value-bind
      (converted-formula1 converts1)
      (methhelp=convert-hom-rescl-to-num formula1 h nil)
    (multiple-value-bind
	(converted-formula2 converts2)
	(methhelp=convert-hom-rescl-to-num formula2 h converts1)
      
      (let* ((converted-string1 (string-downcase (post~string converted-formula1)))
	     (converted-string2 (string-downcase (post~string converted-formula2)))
	     (equation (concatenate 'string
				    "(= "
				    (string-downcase converted-string1)
				    " "
				    (string-downcase converted-string2) ")"))
	     (primitives (remove-duplicates (append (methhelp=get-primitives converted-formula1)
						    (methhelp=get-primitives converted-formula2))))
	     (result (rcl~call-maple (list "msolve" equation (format nil "~A" class-factor)) :syntax 'post2mapleallsolutions)))
	(if (or (null result) (string-equal result "Error"))
	    nil
 	  (let* ((sols (rcl=split2single-solutions (rcl=dissect-maple-hint result)))) 
 	    (if (or (> (length sols) 1)
 		    (= (length sols) 0))
 		;; no solution or more than one (which means it is not a unique solution
 		nil
 	      ;; exactly one solution
 	      (let* ((eq-list (first sols))
 		     (const-list (remove-if-not #'term~constant-p primitives))
 		     (const-string-list (mapcar #'(lambda (const)
 						    (string-downcase (string (keim~name const))))
 						const-list)))
 		(if (every #'(lambda (eq-pair)
 			       (and (string= (first eq-pair) (second eq-pair))
 				    (find (first eq-pair) const-string-list :test #'string=)))
 			   eq-list)
 		    ;; only solution has the form {c1=c1,c2=c2, ...}
 		    ;; 't
 		    (let* ((check (crihelp=check-equation converted-string1 converted-string2 primitives class-factor)))
 		      (if check
 			  't
 			(progn 
 			  (format t "~%~%WARNING: IN FUNCTION crihelp=matching-with-maple")
 			  (format t "~%WE HAVE AN EXAMPLE WHERE MAPLE SAYS YES BUT CHECK SAYS NO!")
 			  (format t "~%~A ~A ~A ~A"
 				  converted-string1 converted-string2 primitives class-factor)
 			  nil)))
 		  nil)))))))))
 
;; The following 3 functions perform an additional check whether the equation term-string1 = term-string2 realy
;; holds with respect to the modulo factor given.
;; This test is performed only, if MAPLE already says that the two terms are equal with respect to the modulo factor.
;; However, it seems to be the case that MAPLE response is sometimes false. Therefore, we build in this additional check.

(defun crihelp=check-equation (term-string1 term-string2 primitives class-factor)
  (let ((check-function (crihelp=build-check-function term-string1 term-string2 primitives class-factor)))
    
    (every #'(lambda (tupel)
 	       (eval `(funcall ,check-function ,@tupel)))
 	   (crihelp=all-tupels primitives class-factor))))

(defun crihelp=all-tupels (primitives class-factor)
  (let* ((modf (keim~name class-factor))
 	 (new-tupels (do* ((counter 0 (incf counter))
 			   (tupels nil))
 			 ((= counter modf)
 			  tupels)
 		       (setf tupels (append tupels (list (list counter)))))))
    
    (do* ((rest-primitives primitives (rest rest-primitives))
 	  (current-tupels nil))
 	((null rest-primitives)
 	 current-tupels)
      (if (null current-tupels)
 	  (setf current-tupels new-tupels)
 	(setf current-tupels (apply #'append (mapcar #'(lambda (curr-tup)
 							 (mapcar #'(lambda (new-tup)
 								     (append curr-tup new-tup))
 								 new-tupels))
 						     current-tupels)))))))

(defun crihelp=build-check-function (term-string1 term-string2 primitives class-factor)
  (let* ((term1-lisp (read-from-string term-string1))
 	 (term2-lisp (read-from-string term-string2)))
    (eval `(lambda ,(mapcar #'(lambda (prim)
 				(read-from-string (string (keim~name prim))))
 			    primitives)
 	     (= (mod ,(if (listp term1-lisp)
 			  (subst '* 'times (subst '+ 'plus (subst '- 'minus term1-lisp)))
 			term1-lisp)
		     ,(keim~name class-factor))
 		(mod ,(if (listp term2-lisp)
 			  (subst '* 'times (subst '+ 'plus (subst '- 'minus term2-lisp)))
 			term2-lisp)
 		     ,(keim~name class-factor)))))))

(defun crihelp=compute-chain (node)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A node whose formula is an equation.")
	   (effect  "None.")
	   (value   "The list of all predecessors of the node whose formulas are also equations and"
		    "which are justified by =subst-m"))
  (let* ((other-reasons (pdsj~other-reasons (node~justification node)))
	 (=subst-reason (first (remove-if-not #'(lambda (reason)
						  (let* ((just (pdsc~an-just reason))
							 (method (just~method just))
							 (name (keim~name method)))
						    (string-equal name '=subst-m)))
					      other-reasons))))
    (if (null =subst-reason)
	nil
      (let* ((reason-node (pdsc~an-node =subst-reason)))
	(cons reason-node (crihelp=compute-chain reason-node))))))

(defun crihelp=order-tupels-II (tupels)
  ;; each Tupel is a list consisting of (("goal".task) ("assumption".ass-node) ("position".pos))
  ;; The tupels are ordered in the following way:
  ;; 1.) The list of all h(x_i) expressions is computed wich have the smallest amount of occurrences in the goal
  ;; 2.) Therewith the tupels list is devided into two lists: L!=tupels which affect one of these expressions
  ;;     and L2=others
  ;; 3.) For each tupel in L1 and L2 is computed whether its application would
  ;;     a) reduce the number of used expressions of homomorphism h
  ;;     b) equals the number of used expressions of homomorphism h
  ;;     c) adds one new expression of homomorphism h
  ;;     d) adds two new expression of homomorphism h
  ;;     
  ;; In the final order the tupels of L1 are before L2, intern of L1 and L2 the tupels are ordered in the following
  ;; way: a) Tupels, then b) Tupels, then c) Tupels, then d) Tupels
  ;;
  ;; In a first try we do not add some ordering with respect to the question whether the equation is applied on the
  ;; smaller or the larger side of the equation!

  
  (if (null tupels)
      nil
    (let* ((task (cdr (first (first tupels))))
	   (goal-node (agenda~task-node task))
	   (goal-formula (crihelp=update-formula! goal-node))
	   (ass-formula (crihelp=update-formula! (cdr (second (first tupels)))))
	   (h (data~appl-function (first (data~appl-arguments ass-formula))))
	   (goal-h-expressions (crihelp=h-expressions-in-term goal-formula h))
	   (smallest-occurrences (crihelp=check-for-smallest-number-of-occurrences goal-h-expressions))
	   (tupels-to-smallest-occurrences (remove-if-not #'(lambda (tupel)
							      (crihelp=tupel-affects-h-exprs-p tupel smallest-occurrences))
							  tupels))
	   (other-tupels (remove-if #'(lambda (tupel)
					(crihelp=tupel-affects-h-exprs-p tupel smallest-occurrences))
				    tupels)))
      (append (crihelp=split-tupel-list-by-effect-II goal-h-expressions h tupels-to-smallest-occurrences)
	      (crihelp=split-tupel-list-by-effect-II goal-h-expressions h other-tupels)))))

(defun crihelp=tupel-affects-h-exprs-p (tupel smallest-occurrences)
  (let* ((equation (crihelp=update-formula! (cdr (second tupel))))
	 (left-side (first (data~appl-arguments equation))))
    (find left-side smallest-occurrences :test #'keim~equal)))
	   
(defun crihelp=check-for-smallest-number-of-occurrences (goal-h-expressions)
  (do* ((rest-goal-h-expressions goal-h-expressions)
	(current-min nil)
	(current-min-h-exprs nil))
      ((null rest-goal-h-expressions)
       current-min-h-exprs)
    (let* ((head-h-expr (first rest-goal-h-expressions))
	   (head-struct (first head-h-expr))
	   (this-h-exprs (remove-if-not #'(lambda (h-expr)
					    (keim~equal (first h-expr) head-struct))
					rest-goal-h-expressions))
	   (other-h-exprs (remove-if #'(lambda (h-expr)
					 (keim~equal (first h-expr) head-struct))
				     rest-goal-h-expressions)))
      
      (setf rest-goal-h-expressions other-h-exprs)
      
      (cond ((or (null current-min)
		 (< (length this-h-exprs) current-min))
	     (setf current-min (length this-h-exprs))
	     (setf current-min-h-exprs (list head-struct)))
	    ((= (length this-h-exprs) current-min)
	     (setf current-min-h-exprs (cons head-struct current-min-h-exprs)))
	    ))))

(defun crihelp=split-tupel-list-by-effect-II (goal-h-expressions h tupels)
  (do* ((rest-tupels tupels (rest rest-tupels))
	(reduce-one-tupels nil)
	(0-effect-tupels nil)
	(add-one-tupels nil)
	(add-two-tupels nil))
      ((null rest-tupels)
       (append reduce-one-tupels
	       0-effect-tupels
	       add-one-tupels
	       add-two-tupels
	       ))  
    (let* ((head-tupel (first rest-tupels))
	   (equation (crihelp=update-formula! (cdr (second head-tupel))))
	   (position (cdr (third head-tupel)))
	   (pos-head (pos~first position))
	   (effect (crihelp=effect-of-equation goal-h-expressions equation h position)))
      (cond ((= effect -1)
	     (setf reduce-one-tupels (cons head-tupel reduce-one-tupels)))
	    ((= effect 0)
	     (setf 0-effect-tupels (cons head-tupel 0-effect-tupels)))
	    ((= effect 1)
	     (setf add-one-tupels (cons head-tupel add-one-tupels)))
	    ((= effect 2)
	     (setf add-two-tupels (cons head-tupel add-two-tupels)))
	    (t
	     (omega~error "~%Unawaited failure in function crihelp=split-tupel-list-by-effect."))))))

(defun crihelp=order-tupels (tupels)
  ;; each Tupel is a list consisting of (("goal".task) ("assumption".ass-node) ("position".pos))
  ;; for each Tupel is checked, whether its application would:
  ;; a) reduce the number of used expressions of homomorphism h
  ;; b) equals the number of used expressions of homomorphism h
  ;; c) adds one new expression of homomorphism h
  ;; d) adds two new expression of homomorphism h
  ;;
  ;; The Tupels are then ordered in this way (first a), then b) ...)
  ;;
  ;; Within these classes we order the tupels again by ordering first such tupels that work on the smaller
  ;; side of goal equation.

  (if (null tupels)
      nil
    (let* ((task (cdr (first (first tupels))))
	   (goal-node (agenda~task-node task))
	   (goal-formula (crihelp=update-formula! goal-node))
	   (ass-formula (crihelp=update-formula! (cdr (second (first tupels)))))
	   (h (data~appl-function (first (data~appl-arguments ass-formula))))
	   (goal-h-expressions (crihelp=h-expressions-in-term goal-formula h))
	   (goal-h-expressions-left (remove-if-not #'(lambda (goal-h-expr)
						       (= (pos~first (second goal-h-expr)) 1))
						   goal-h-expressions))
	   (goal-h-expressions-right (remove-if-not #'(lambda (goal-h-expr)
							(= (pos~first (second goal-h-expr)) 2))
						    goal-h-expressions))
	   (less-side (cond ((< (length goal-h-expressions-left)
				(length goal-h-expressions-right))
			     'left)
			    ((< (length goal-h-expressions-right)
				(length goal-h-expressions-left))
			     'right)
			    (t
			     nil))))
      (crihelp=split-tupel-list-by-effect goal-h-expressions h tupels less-side))))

(defun crihelp=split-tupel-list-by-effect (goal-h-expressions h tupels less-side)
  (do* ((rest-tupels tupels (rest rest-tupels))
	(reduce-one-tupels-less-side nil)
	(reduce-one-tupels-other-side nil)
	(0-effect-tupels-less-side nil)
	(0-effect-tupels-other-side nil)
	(add-one-tupels-less-side nil)
	(add-one-tupels-other-side nil)
	(add-two-tupels-less-side nil)
	(add-two-tupels-other-side nil))
      ((null rest-tupels)
       (append reduce-one-tupels-less-side reduce-one-tupels-other-side
	       0-effect-tupels-less-side 0-effect-tupels-other-side
	       add-one-tupels-less-side add-one-tupels-other-side
	       add-two-tupels-less-side add-two-tupels-other-side
	       ;; Die die zwei Addieren lassen wir mal ganz raus!
	       ))  
    (let* ((head-tupel (first rest-tupels))
	   (equation (crihelp=update-formula! (cdr (second head-tupel))))
	   (position (cdr (third head-tupel)))
	   (pos-head (pos~first position))
	   (less-sidep (if (or (and (= pos-head 1)
				    (string-equal 'left less-side))
			       (and (= pos-head 2)
				    (string-equal 'right less-side)))
			   't
			 nil))
	   (effect (crihelp=effect-of-equation goal-h-expressions equation h position)))
      (cond ((= effect -1)
	     (if less-sidep
		 (setf reduce-one-tupels-less-side (cons head-tupel reduce-one-tupels-less-side))
	       (setf reduce-one-tupels-other-side (cons head-tupel reduce-one-tupels-other-side))))
	    ((= effect 0)
	     (if less-sidep
		 (setf 0-effect-tupels-less-side (cons head-tupel 0-effect-tupels-less-side))
	       (setf 0-effect-tupels-other-side (cons head-tupel 0-effect-tupels-other-side))))
	    ((= effect 1)
	     (if less-sidep
		 (setf add-one-tupels-less-side (cons head-tupel add-one-tupels-less-side))
	       (setf add-one-tupels-other-side (cons head-tupel add-one-tupels-other-side))))
	    ((= effect 2)
	     (if less-sidep
		 (setf add-two-tupels-less-side (cons head-tupel add-two-tupels-less-side))
	       (setf add-two-tupels-other-side (cons head-tupel add-two-tupels-other-side))))
	    (t
	     (omega~error "~%Unawaited failure in function crihelp=split-tupel-list-by-effect."))))))

(defun crihelp=effect-of-equation (goal-h-expressions equation h position)
  (let* ((h-left (first (data~appl-arguments equation)))
	 (h-rights (mapcar #'first (crihelp=h-expressions-in-term (second (data~appl-arguments equation)) h)))
	 (h-right1 (first h-rights))
	 (h-right2 (second h-rights))
	 (goal-h-reduced-expressions (remove-if #'(lambda (goal-h-expr)
						    (and (keim~equal h-left (first goal-h-expr))
							 (keim~equal position (second goal-h-expr))))
						goal-h-expressions))
	 ;; removes the h-expression that would be replaced in this step
	 (h-left-in-rest-goal (remove-if-not #'(lambda (goal-h-expr)
						 (keim~equal h-left (first goal-h-expr)))
					     goal-h-reduced-expressions))
	 ;; other occurrences in the remaining goal
	 (h-right1-in-rest-goal (remove-if-not #'(lambda (goal-h-expr)
						   (keim~equal h-right1 (first goal-h-expr)))
					       goal-h-reduced-expressions))
	 (h-right2-in-rest-goal (remove-if-not #'(lambda (goal-h-expr)
						   (keim~equal h-right2 (first goal-h-expr)))
					       goal-h-reduced-expressions))
	 ;; occurences of hright1 and hright2 in the rest goal
	 )

    ;;(format t "~%goal-h-reduced-expressions: ~A" goal-h-reduced-expressions)
    ;;(format t "~%h-left-in-rest-goal: ~A" h-left-in-rest-goal)
    ;;(format t "~%h-right1-in-rest-goal: ~A" h-right1-in-rest-goal)
    ;;(format t "~%h-right2-in-rest-goal: ~A" h-right2-in-rest-goal)
    
    (- (+ (if h-right1-in-rest-goal 0 1)
	  (if h-right2-in-rest-goal 0 1)) 
       (+ (if h-left-in-rest-goal 0 1)
	  (if (and (null h-right1-in-rest-goal)
		   (null h-right2-in-rest-goal)
		   (keim~equal h-right1 h-right2))
	      1
	    0)))))
		   
(defun crihelp=h-expressions-in-term (term h)
  (let* ((positions (data~positions term #'(lambda (item)
					     (and (data~appl-p item)
						  (keim~equal (data~appl-function item) h)))))
	 (structs (mapcar #'(lambda (pos)
			      (data~struct-at-position term pos))
			  positions)))
    (mapcar #'(lambda (struct position)
		(list struct position))
	    structs positions)))

(defun crihelp=update-formula! (node)
  (if (null (pdsn~schematic-p node))
      (node~formula node)
    (progn
      (when (and (null (pdsn~up-to-date node))
		 (pds~constraint-pool omega*current-proof-plan))
	(setf (pdsn~current-formula node)
	      (beta~normalize (subst~apply (pds~cstrpool-bindings (pds~constraint-pool omega*current-proof-plan))
					   (node~formula node))))
	(setf (pdsn~up-to-date node) 't))
      (pdsn~current-formula node))))

(defun crihelp=positions-merging-left (task equation-supp)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A task and a node with a equation as formula.")
	   (effect  "None.")
	   (value   "The list of all positions in the node of task which merge with the left hand"
		    "side of the equation."))
  (let* ((node (agenda~task-node task))
	 (formula (crihelp=update-formula! node))
	 (left-side (first (data~appl-arguments (node~formula equation-supp)))))
    (data~substruct-positions left-side formula :test #'keim~equal)))
	 
(defun crihelp=equation-supports-of-task (task)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "The list of all support nodes of the node of the task which have equations as formulas."))
  (let* ((node (agenda~task-node task))
	 (supports (pds~node-supports node)))
    (remove-if-not #'(lambda (supp)
		       (let* ((formula (node~formula supp)))
			 (if (equation-p formula)
			     't
			   nil)))
		   supports)))

(defun crihelp=largest-mod-factor (non-iso-formula) 
  (declare (edited  "20-FEB-2001")
	   (authors Ameier)
	   (input   "A formula of the form '(not (isomorphic set1 op1 set2 op2)' where seti and opi are"
		    "sets and operations about residue classes.")
	   (effect  "None.")
	   (value   "The function computes the two modulo factors of set1 and set2 and returns the"
		    "larger one."))
  (let* ((iso-args (data~appl-arguments (first (data~appl-arguments non-iso-formula))))
	 (set1 (first iso-args))
	 (set2 (third iso-args))
	 (fac1 (keim~name (zmztac=class-factor set1)))
	 (fac2 (keim~name (zmztac=class-factor set2))))
    (max fac1 fac2)))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=just-of-insttask (inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "An instantiation task.")
	   (effect  "None.")
	   (value   "The justification of the plan step, that created the task."))
  (pdsc~an-just (agenda~inst-task-plan-step inst-task)))

(defun crihelp=mainprem-of-insttask (inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "An instantiation task.")
	   (effect  "None.")
	   (value   "The second premise of the plan step, that created the task."))
  (second (just~premises (crihelp=just-of-insttask inst-task))))

(defun crihelp=formula-of-insttask (inst-task)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "An instantiation task.")
	   (effect  "None.")
	   (value   "The formula of the plan step, that created the task."))
  (node~formula (crihelp=mainprem-of-insttask inst-task)))

(defun crihelp=supergoal-of-node (node)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The supergoals of the node, i.e. the nodes this node is a subgoal to."))
  (let ((reasons (pdsj~reasons (node~justification node))))
    (find-if-not #'(lambda (anode) (keim~equal node anode)) (mapcar 'pdsc~an-node reasons))))

(defun crihelp=applfunc-equals (prop formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A symbol and a formula.")
	   (effect  "None.")
	   (value   "Checks whether the formula is an application with head symbol prop."))
  (and (data~appl-p formula)
       (keim~equal (data~appl-function formula)
		   (data~schema-range (env~lookup-object prop (pds~environment omega*current-proof-plan))))))
  
(defun crihelp=negated-p (test formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A test function and a formula.")
	   (effect  "None.")
	   (value   "Checks whether the formula is a negation and the body satisfies test."))
  (and (logic~negation-p formula) (funcall test (first (data~appl-arguments formula)))))

(defun crihelp=closure-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol closed-under."))
  (crihelp=applfunc-equals 'closed-under formula))

(defun crihelp=commut-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol commutative."))
  (crihelp=applfunc-equals 'commutative formula))

(defun crihelp=assoc-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol associative."))
  (crihelp=applfunc-equals 'associative formula))

(defun crihelp=distrib-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol distributive."))
  (crihelp=applfunc-equals 'distributive formula))

(defun crihelp=unit-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an existential quantification with head symbol unit."))
  (and (logic~existential-quantification-p formula)
       (crihelp=applfunc-equals 'unit (logic~quantification-scope formula))))

(defun crihelp=inverse-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol inverse-exists."))
  (crihelp=applfunc-equals 'inverse-exist formula))

(defun crihelp=divisor-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol divisor-exists."))
  (crihelp=applfunc-equals 'divisors-exist formula))

(defun crihelp=iso-p (formula)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True iff the formula is an application with head symbol isomorphic or module-isomorphic."))
  (or (crihelp=applfunc-equals 'isomorphic formula)
      (crihelp=applfunc-equals 'module-isomorphic formula)))


(defun crihelp=closable-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (cond ((logic~disjunction-p formula)
	   (or (crihelp=closable-p (first (data~appl-arguments formula)))
	       (crihelp=closable-p (second (data~appl-arguments formula)))))
	  ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object '= env))))
	   (keim~equal (first (data~appl-arguments formula))
		       (second (data~appl-arguments formula))))
	  (t
	   nil))))
  
(defun crihelp=metaor-formula-p (formula)
  (cond ((logic~disjunction-p formula)
	 (and (crihelp=metaor-formula-p (first (data~appl-arguments formula)))
	      (crihelp=metaor-formula-p (Second (data~appl-arguments formula)))))
	((and (data~appl-p formula)
	      (keim~equal (data~appl-function formula)
			  (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan)))))
	 (or (meta~p (first (data~appl-arguments formula)))
	     (meta~p (second (data~appl-arguments formula)))))
	(t
	 nil)))

(defun crihelp=following-exists (formula)
  (declare (edited  "20-JUL-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A number: the amount of existsI that have to be made on this formula."
		    "       Thereby each leading exists-sort subformula of the formula is counted:"
		    "       A exists-sort on sort residue-class as 1 and a exists-sort on sort"
		    "       cartesian-product as n if the cartesian-product consists of n sets."
		    "Second: The rest formulas."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (cart-obj (data~schema-range (env~lookup-object 'cartesian-product env))))

    (do* ((rest-formula formula)
	  (back-number 0))
	((null (logic~existential-quantification-p rest-formula))
	 (values back-number
		 rest-formula))
      
      (let* ((sort (second (data~appl-arguments rest-formula))))
	(setf rest-formula (data~abstr-range (first (data~appl-arguments rest-formula))))
	(setf back-number (+ back-number (+ (length (data~substruct-positions cart-obj sort :test #'keim~equal)) 1)))))))
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for computation of multiplication tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=convert-operation (term class-factor assoc-list)
  (cond ((term~constant-p term)
	 (zmztac=convert-resclass-operation-to-num-operation term class-factor))
	((term~variable-p term)
	 (second (assoc term assoc-list)))
	((and (term~appl-p term)
	      (keim~equal (data~appl-function term) (env~lookup-object 'resclass (pds~environment omega*current-proof-plan)))
	      (= (length (data~appl-arguments term)) 2))
	 (let* ((args (data~appl-arguments term)))
	   (if (keim~equal (first args) class-factor)
	       (second args)
	     (omega~error "~%In function crihelp=convert-operation operation ~A not compatible with class-factor ~A" term class-factor))))
	((term~appl-p term)
	 (beta~normalize (term~appl-create (crihelp=convert-operation (data~appl-function term) class-factor assoc-list)
					   (mapcar #'(lambda (arg)
						       (crihelp=convert-operation arg class-factor assoc-list))
						   (data~appl-arguments term)))))
	((term~abstr-p term)
	 (let* ((domain (data~abstr-domain term))
		(range (data~abstr-range term))
		(nvar1 (term~variable-create 'n1 (env~lookup-object 'num (pds~environment omega*current-proof-plan))))
		(nvar2 (term~variable-create 'n2 (env~lookup-object 'num (pds~environment omega*current-proof-plan))))
		(converted-range (crihelp=convert-operation range class-factor (list (list (first domain) nvar1)
										       (list (second domain) nvar2)))))
	   (term~abstr-create (list nvar1 nvar2) converted-range)))))

(defun crihelp=number-set-to-resclass-set (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula, representing a set of resclasses with a common class factor.")
	   (effect  "None.")
	   (value   "A list of numbers."))
  (multiple-value-bind (flag class-factor number-list)
      (zmztac=resclass-set-p formula)
    (declare (ignore class-factor))
    (when flag (mapcar #'keim~name number-list))))

(defun crihelp=get-set-and-operation (formula)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (func-list (mapcar #'(lambda (func) (data~schema-range (env~lookup-object func env)))
			    '(closed-under unit divisors-exist inverse-exist associative commutative distributive))))
    (cond ((and (data~appl-p formula)
		(find (data~appl-function formula) func-list :test #'keim~equal))
	   (let* ((args (data~appl-arguments formula)))
	     (values (first args)
		     (second args))))
	  ((and (logic~negation-p formula)
		(data~appl-p (first (data~appl-arguments formula)))
		(find (data~appl-function (first (data~appl-arguments formula))) func-list :test #'keim~equal))
	   (let* ((args (data~appl-arguments (first (data~appl-arguments formula)))))
	     (values (first args)
		     (second args))))
	  ((and (logic~existential-quantification-p formula)
		(data~appl-p (logic~quantification-scope formula))
		(keim~equal (data~appl-function (logic~quantification-scope formula))
			    (data~schema-range (env~lookup-object 'unit env))))
	   (let* ((args (data~appl-arguments (logic~quantification-scope formula))))
	     (values (first args)
		     (second args))))
	  ((and (logic~negation-p formula)
		(logic~existential-quantification-p (first (data~appl-arguments formula)))
		(data~appl-p (logic~quantification-scope (first (data~appl-arguments formula))))
		(keim~equal (data~appl-function (logic~quantification-scope (first (data~appl-arguments formula))))
			    (data~schema-range (env~lookup-object 'unit env))))
	   (let* ((args (data~appl-arguments (logic~quantification-scope (first (data~appl-arguments formula))))))
	     (values (first args)
		     (second args)))))))

(defun crihelp=get-sets-and-operations-from-card-prod (formula)
  (declare (edited  "20-JUL-2000")
	   (authors Ameier)
	   (input   "A formula of a start task of a residue-class problem.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of sets."
		    "Second: A list of operations."
		    "Example: (associative (resclass-set 2) plus-resclass) -> ((resclass-set 2)) and (plus-resclass)"
		    "         (closed-under (cartesian-product (resclass-set 2) (resclass-set 2))"
		    "                                          (pair-operation plus-resclass plus-resclass))"
		    "         -> ((resclass-set 2) (resclass-set 2)) and (plus-resclass plus-resclass)"))
  (multiple-value-bind
      (direct-set direct-operation)
      (crihelp=get-set-and-operation formula)
    ;; This function returns, e.g. (cartesian-product (resclass-set 2) (resclass-set 2)) and
    ;; (pair-operation plus-resclass plus-resclass))
    ;; -> we have to decompose as next the cartesian product parts
    
    (multiple-value-bind
	(sets operations)
	(crihelp=decompose-cartproduct-sets-and-ops direct-set direct-operation)
      
      (values sets
	      operations))))

(defun crihelp=decompose-cartproduct-sets-and-ops (direct-set direct-operation &key (operation-composer 'pair-operation))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (cart-prod-obj (data~schema-range (env~lookup-object 'cartesian-product env)))
	 (comp-op-obj (data~schema-range (env~lookup-object operation-composer env))))
    
    (do* ((rest-sets (list direct-set))
	  (rest-ops (list direct-operation))
	  (back-sets nil)
	  (back-operations nil))
	((null rest-sets)
	 (values back-sets
		 back-operations))
      (let* ((head-set (first rest-sets))
	     (head-op (first rest-ops)))
	(cond ((and (data~appl-p head-set)
		    (keim~equal (data~appl-function head-set) cart-prod-obj))
	       ;; next set begins is cartesian-product
	       ;; -> next operation should be a pair-operation
	       ;;    otherwise -> failure
	       (when (null (and (data~appl-p head-op)
				(keim~equal (data~appl-function head-op) comp-op-obj)))
		 (omega~error "In function crihelp=get-sets-and-operations-from-card-prod: Unconsistent sets and operations."))
	       
	       (setf rest-sets (append (data~appl-arguments head-set) (rest rest-sets)))
	       (setf rest-ops (append (data~appl-arguments head-op) (rest rest-ops))))
	      (t
	       (setf back-sets (append back-sets (list head-set)))
	       (setf back-operations (append back-operations (list head-op)))
	       (setf rest-sets (rest rest-sets))
	       (setf rest-ops (rest rest-ops))))))))		 
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for computation of instantiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crihelp=extract-tupel (formula)
  (declare (edited  "23-JUL-2000")
	   (authors Ameier)
	   (input   "A formula of the form (resclass 2 n) or (pair (resclass 2 n1) ...")
	   (effect  "NOne.")
	   (value   "A (mayby unary) tupel (n1 n2 ...)."))
  (if (null formula)
      nil
    (let* ((env (pds~environment omega*current-proof-plan)))
      
      (do* ((rest-formulas (list formula))
	    (back-tupel nil))
	  ((null rest-formulas)
	   back-tupel)
	(let* ((head-formula (first rest-formulas)))
	  (if (and (data~appl-p head-formula)
		   (keim~equal (data~appl-function head-formula)
			       (data~schema-range (env~lookup-object 'pair env))))
	      ;; pair case ->
	      (setf rest-formulas (append (data~appl-arguments head-formula) (rest rest-formulas)))
	    ;; head-formula has the form (resclass m n)
	    (progn
	      (setf back-tupel (append back-tupel (list (second (data~appl-arguments head-formula)))))
	      (setf rest-formulas (rest rest-formulas)))))))))
  
(defun crihelp=extract-arguments (composed-term op)
  (declare (edited  "13-APR-2000")
	   (authors Ameier)
	   (input   "A composed term t and a  operation op, such that t = (op a b).")
	   (effect  "None.")
	   (value   "A list with a and b."))
  ;; Note, that it is possible that a and b cannot be computed!
  ;; e.g., op = lam x.y. x , and t = (op 0 1) = 0 ,
  ;; then this function can reconstrcut that a = 0 but it cannot reconstruct that b = 1!

  (let* ((env (pds~environment omega*current-proof-plan))
	 (type-var (type~variable-create 'aa))
	 (nvar1 (term~variable-create 'nvar1 type-var)) ;;(post~read-object '(o num) env :existing-type)))
	 (nvar2 (term~variable-create 'nvar2 type-var)) ;;(post~read-object '(o num) env :existing-type)))
	 (match-term (beta~normalize (term~appl-create op (list nvar1 nvar2)))))
    
    (multiple-value-bind
	(success subst)
	(bind~with-bindings ((keim::term=alpha-equal match-term composed-term (list nvar1 nvar2))))
      
      (if success
	  (list (when (find nvar1 (subst~domain subst))
		  (subst~apply subst nvar1))
		(when (find nvar2 (subst~domain subst))
		  (subst~apply subst nvar2)))
	(list nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; I've rewritten the hint mechanism:
; The usual way to compute a hint was to take the formula that is responsible for the inst-task 
; (a formula that does not contain any mvs, eg. an ex. quantified formula) and compute the hint for it.
; That means the hint mechanism has to know what the method has with the formula.
; What seems to better to me is to take the RESULTING formula containing the mv, or allow to give any node
; in the justification.
; In the case of ex-I-methods it is not hard to imagine what happened to the formula, but think of complex methods
; or methods which introduce than one mv. MP
; for examples see in permutation-strategies.thy

(defun crihelp=param-to-inst-task-by-nodes-mp (insttask nodes)
  (declare (edited  "22-NOV-2002")
	   (authors Pollet)
	   (input   "An instantiation task and a list of nodes.")
	   (effect  "See crihelp=find-case.")
	   (value   "The instantiation for insttask wrt. the case object for node."))
  (when (car nodes)
    (let* ((node (car nodes))
	   (mv (agenda~inst-task-meta-var insttask))
	   (case (and (data~substruct-p (node~formula node) mv) ;;only consider nodes that actually contain the MV
		      (crihelp=find-case-mp node mv))))
      (if case
	  (progn ;(omega~trace "~A:~A" (class-of case) case)
		 (crihelp=case-process case insttask))
	(crihelp=param-to-inst-task-by-nodes-mp insttask (rest nodes))))))

(defun crihelp=param-to-inst-task-mp (inst-task)
  (or (crihelp=param-to-inst-task-by-node-mp inst-task
					     (pdsc~an-node (agenda~inst-task-plan-step inst-task)))
      (crihelp=param-to-inst-task-by-nodes-mp inst-task
					      (pdsj~ass&premises (pdsc~an-just (agenda~inst-task-plan-step inst-task))))))


(defun crihelp=param-to-inst-task-by-node-mp (insttask node)
  (declare (edited  "23-NOV-2002" "13-MAY-2002")
	   (authors Pollet Scholl)
	   (input   "An instantiation task and a node.")
	   (effect  "See crihelp=find-case.")
	   (value   "The instantiation for insttask wrt. the case object for node or - if that does not exists - a supergoal of node."))
  (let ((case (crihelp=find-case-mp node (agenda~inst-task-meta-var insttask))))
    (if case
	(progn
	  (format t "~A" case)
	  (crihelp=case-process case insttask))
      (when (and (not (keim~equal node (prob~proof-root omega*current-proof-plan)))
		 (crihelp=supergoal-of-node node))
	(crihelp=param-to-inst-task-by-node-mp insttask (crihelp=supergoal-of-node node))))))

(defun crihelp=find-case-mp (node mv)
  (declare (edited  "13-MAY-2002")
	   (authors Scholl)
	   (input   "A node.")
	   (effect  "If the case does not exist, it is added to the association list crihelp*processed-cases.")
	   (value   "A case object if node corresponds to a case class in crihelp*case-classes, otherwise nil."
		    "Note: If the node was analysed earlier, the stored object is taken from crihelp*processed-cases"))
  (let ((processed-case (cdr (assoc node crihelp*processed-cases))))
    (if (and processed-case
	     (eq (crihelp~case-mv processed-case) mv))
	processed-case
      (let ((new-case (crihelp=match-cases node crihelp*case-classes)))
	(when new-case (setf (crihelp~case-mv new-case) mv) new-case)))))
