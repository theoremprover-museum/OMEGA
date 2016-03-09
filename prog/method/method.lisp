;; -*- Syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                      ;;
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
 
(in-package "OMEGA")


(mod~defmod METH 
            :uses (arg beta gb cstr data env help infer just keim logic mapp meta node omega pds pdsj pdsn pos post ssterm subst sys term th type)
            :documentation "The method module"
            :exports (meth+action
                      meth+action-pattern
                      meth+computation
                      meth+condition
                      ;;LC weg: meth+constraint
                      meth+funcall
                      meth+just
                      meth+key-expression
                      meth+mapping
                      meth+meta-node
                      meth+method
                      meth+node
                      meth+ordering
                      meth+supermethod
                      meth+constrained-mapping ;; LC meth+truth-mapping
                      
                      meth~action-p
                      meth~action-pattern-p
                      meth~add&expansion-nodes
                      meth~add-conclusions
                      meth~add-nodes
                      meth~add-premises
                      meth~application-condition
                      ;;LC weg: meth~application-constraint
                      meth~build-object
                      meth~build-with-keyword
                      meth~carry-out-computations
		      meth~compute-parameters
                      meth~check-condition
                      meth~closed-premises
                      meth~complete-outlines
                      meth~computation-p
                      meth~conclusions
                      meth~condition-p
                      meth~connective-constraint-p
                      meth~connective-function-p
                      ;; LC: weg meth~constraint-evaluate
                      meth~create-key-expression
                      meth~create-node
		      meth~critical-p
                      meth~declarative-content
                      meth~defcond
                      meth~deffun
		      meth~parameters
		      meth~non-reliability
		      meth~reasoning
		      meth~outline-orderings
		      meth~outline-actions
		      meth~define
                      meth~defmethod
                      meth~defsupermethod
                      meth~disjoint-compose-substitution
                      meth~enter-sorted-metavar
                      meth~env-enter-metavarlist
                      meth~environment
                      meth~execute-action
                      meth~execute-expansion-function
                      meth~exist-conclusions
                      meth~exist-premises
                      meth~existent-premises
                      meth~expansion-computations
		      meth~expansion-condition
                      meth~expansion-function
                      meth~expansion-nodes
                      meth~find-method
                      meth~funcall-p
                      meth~given-premises
                      meth~goal
                      meth~hypothesis-node-p
                      meth~inference
		      meth~current-theory-methods!
                      meth~insert-method
                      meth~just-match-status
                      meth~just-p
                      meth~just-parameters
                      meth~key-expression-p
                      meth~keyword-p
                      meth~label2node
		      meth~manual
                      meth~mapp-extend-mapp
                      meth~mapp-extend-subst
                      meth~mapp-get-component
                      meth~mapp-mapp
                      meth~mapp-new-mapp
                      meth~mapp-new-subst
                      meth~mapp-subst
                      meth~mapping-copy
                      meth~mapping-create
                      meth~mapping-extend
                      meth~mapping-p
                      meth~match-p
                      meth~meta-node-metavar
                      meth~meta-node-p
                      meth~method-p
                      meth~minus-node-p
                      meth~mor-method-p
                      meth~new-condition-connective
                      meth~new-constraint-connective
                      meth~new-keyword
                      meth~new-method-condition
                      meth~new-method-function
		      meth~new-critic
		      meth~critic-p
                      meth~new-relational-function
                      meth~node-p
                      meth~node-sign
                      meth~non-reliability
                      meth~object-instance
                      meth~open-just-p
                      meth~ordering-ordered-nodes
                      meth~ordering-p
                      meth~outline-actions
                      meth~outline-computations
                      meth~outline-orderings
                      meth~p
                      meth~parameters
                      meth~pds-object
                      meth~plus-node-p
                      meth~preconditions
                      meth~premises
                      meth~prems-outline-pattern
                      meth~procedural-content
                      meth~proved-concs
                      meth~rating
                      meth~read-parameters
                      meth~real-existent-premises
                      meth~relational-function-p
                      meth~remark
                      meth~remove-method
                      meth~remove-methods
                      meth~select-pds-nodes
                      meth~show-method-hashtable
                      meth~show-nodes
                      meth~subst-apply
                      meth~subst-create
                      meth~supermethod-p
                      meth~sym2action
                      meth~sym2condition
                      meth~sym2function
                      meth~theory
                      meth~tm-create
                      meth~tm-new-value
                      meth~tm-value
                      meth~truth-mapping-p
                      meth~zero-node-p
                      
                      meth*connectives
                      meth*relational-functions
                      meth*cstr-connectives
                      meth*current-method
                      meth*indent
                      meth*keywords
                      meth*method-hashtable
                      meth*ordering-keywords
		      
                      meth*planning-methods
                      meth*normalizing-methods
                      meth*restricting-methods

                      meth*sort2argtype
                      meth*sym2action
                      meth*sym2condition
                      meth*sym2function
		      meth*help4condfunc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All kinds of global variables....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Some lists for classifying and sorting methods wrt. their ratings:
(defvar meth*MOReasoning-methods NIL
  "This variable maintains the method list used for Middle-Out-Reasoning (:middle-out) , i.e., applicable to schematic goals.")

(defvar meth*planning-methods NIL
  "This variable maintains the method list used for planning (:planning)  goals.")

;(defvar meth*MOReasoning-parammeths NIL
;  "This variable maintains the list of methods with parameters used for Middle-Out-Reasoning (:middle-out), i.e., applicable to schematic goals.")

;(defvar meth*planning-parammeths NIL
;  "This variable maintains the list of methods with parameters used for planning (:planning) goals.")

(defvar meth*normalizing-methods NIL
  "This variable maintains the method list used for normalizing (:normalizing) supports.")
;;; LCh: In order to assume a certain supports propreties, these methods are
;; applied whenever new supports are generated. For instance, in order to
;; consider the conjuncts instead of conjunctured supports, we have to apply
;; AndE whenever a new hypothesis is generated.

(defvar meth*restricting-methods NIL
  "This variable maintains the method list used for restricting (:restricting) some goals.")
;;; This methods are applied in every plan step, they are all obvious methods
;; because they only restrict some open nodes by for instance Weaken, and
;; ForallE* ... They dont have subgoals.

(defvar meth*method-hashtable (make-hash-table :test #'equal)
  "A hash table to map ;method names to the methods themselves.")

(defvar meth*relational-functions
  '(alpha-matcher th-definition meval mbind subterm-matcher match-subterm termoccs termmgu)
  "List of the symbols denoting relational functions.")

(defvar meth*current-method nil
  "The method currently matched.")

(defvar meth*quantifiers
  '(mforall mexists)
  "List of the symbols denoting quantifiers.")

(defvar meth*connectives
  '(mand mor mnot mxor mif)
  "List of the symbols denoting connectives.")

(defvar meth*cstr-connectives
  '(cand )
  "List of the symbols denoting constraint connectives.")

(defvar meth*keywords
  '(:term :position :type)
  "List of symbols that refer to a keyword in a method declaration.")

(defvar meth*ordering-keywords
  '(before first)
  "List of symbols that refer to a keyword in a method ordering.")

(defvar meth*sym2action
  (list (cons 'sponsor #'pds~add-sponsors)
	(cons 'unsponsor #'pds~delete-sponsors)
	(cons 'sponsor-if-hyp #'pds~add-sponsors-if-hyp)
	(cons 'increasehyp #'pds~insert-hyps!))
  "An association list of symbols to lisp functions which carry out the associated action.")

(defvar meth*sym2function
  (make-hash-table :test #'equal)
  "A hash table which associates each symbol the corresponding method function.")

(defvar meth*sym2condition
  (make-hash-table :test #'equal)
  "A hash table which associates each symbol the corresponding method predicate.")

(defvar meth*help4condfunc
  (make-hash-table :test #'equal)
  "A hash table which associates each symbol the corresponding help for this condition/function.")

(defvar meth*sort2argtype
  (list (cons (ssterm~get-ssort :prln) (arg~find-argtype 'ndline))
	(cons (ssterm~get-ssort :term) (arg~find-argtype 'term))
	(cons (ssterm~get-ssort :termlist) (arg~find-argtype 'term-list))
	)
  "A mapping of sorts to argument types for parameter reading, the argument types must be defined before loading this file.")

(defvar meth*critics NIL
  "List of the methods which are seen as critics.")

(defun meth~new-critic (symbol)
  (when (or (symbolp symbol)
	    (omega~error "Critic indicator ~A has to be a symbol." symbol))
    (let ((critic-method (meth~find-method symbol)))
      (when (or critic-method
		(omega~error "Critic method ~A does not exist." symbol))
	(unless (find critic-method meth*critics)
	  (setq meth*critics (cons critic-method meth*critics)))))))

(defun meth~critic-p (object)
  (find object meth*critics))

;;; Just for printing methods:
(defvar meth*indent 3 "The number of spaces for indenting the output of the method.")

;;; ... and functions to dymanically update them

(defun meth~new-relational-function (symbol)
  (when (or (symbolp symbol)
	    (omega~error "Relational function indicator ~A has to be a symbol." symbol))
    (unless (find symbol meth*relational-functions)
      (setq meth*relational-functions (cons symbol
					    meth*relational-functions)))))

(defun meth~new-condition-connective (symbol)
  (when (or (symbolp symbol)
	    (omega~error "Connectives for application condition ~A has to be a symbol." symbol))
    (unless (find symbol meth*connectives)
      (setf meth*connectives (cons symbol
				   meth*connectives)))))

(defun meth~new-constraint-connective (symbol)
  (when (or (symbolp symbol)
	    (omega~error "Connectives for application constraint ~A has to be a symbol." symbol))
    (unless (find symbol meth*cstr-connectives)
      (setf meth*cstr-connectives (cons symbol
					meth*cstr-connectives)))))

(defun meth~new-keyword (symbol)
  (when (or (symbolp symbol)
	    (omega~error "Keyword ~A has to be a symbol." symbol))
    (unless (find symbol meth*keywords)
      (setf meth*keywords (cons symbol
				meth*keywords)))))

(defun meth~new-method-function (symbol function)
  (when (or (and (symbolp symbol) (functionp function))	    
	    (omega~error "Wrong method function mapping: ~A has to be a symbol and ~A a function." symbol function))
    (setf (gethash (symbol-name symbol) meth*sym2function) function)))

(defun meth~new-method-condition (symbol function)
  (when (or (and (symbolp symbol) (functionp function))	    
	    (omega~error "Wrong method function mapping: ~A has to be a symbol and ~A a predicate." symbol function))
    (setf (gethash (symbol-name symbol) meth*sym2condition) function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The basic datastructure for methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (defclass meth+method (help+help)
    ((theory :initarg :theory
	     :accessor meth~theory 
	     :documentation "This is the theory of a method.") ;;;THEORY
     (inference :initarg :inference
		:accessor meth~inference
		:documentation "The inference rule the method is associated with.")
     (rating :initarg :rating
	     :accessor meth~rating
	     :documentation "This is the rating-slot of a mthod. In this slot stands the rating used by the planner.")
     (non-reliability :initarg :non-reliability
		      :initform nil
		      :accessor meth~non-reliability
		      :documentation "This slot specifies whether this method is reliable.")
     (reasoning :initarg :reasoning
		:initform (list :planning)
		:accessor meth~reasoning
		:documentation "This slot specifies what kind of reasoning this method is used for.")
     (environment :initarg :environment
		  :accessor meth~environment
		  :documentation "This is the declarations-slot of a mthod. In this slot stands the
definition of the meta-variables with its sorts.")
     (parameters :initarg :parameters
		 :accessor meth~parameters
		 :documentation "A list of parameters to be given when applying this method.")
     (just-parameters :initarg :just-parameters
		      :initform nil
		      :accessor meth~just-parameters
		      :documentation "In this slot the parameters for the justification of
a method are stored.")
     (premises :initarg :premises
	       :accessor meth~premises
	       :documentation "In this slot the proof nodes are stored.")
     (application-condition :initarg :application-condition
			    :accessor meth~application-condition
			    :documentation "In this slot the constaints for the applicability of a method is stored.")
;LC weg     (application-constraint :initarg :application-constraint
;                             :accessor meth~application-constraint
;                             :documentation "In this slot the constraint for the applicability of a method is stored.")
     (outline-computations :initarg :outline-computations
			   :initform nil
			   :accessor meth~outline-computations
			   :documentation "The outline computations of the method.")
     (outline-actions :initarg :outline-actions
		      :initform nil
		      :accessor meth~outline-actions
		      :documentation "The outline actions of the method.")
     (outline-orderings :initarg :outline-orderings
			:initform nil
			:accessor meth~outline-orderings
			:documentation "The outline orderings of the method which keeps a special ordering of the method subgoals.")
     (expansion-condition :initarg :expansion-condition
			  :accessor meth~expansion-condition
			  :documentation "The expansion condition of a method.")
     (expansion-computations :initarg :expansion-computations
			     :initform nil
			     :accessor meth~expansion-computations
			     :documentation "The expansion computations of the method.")
     (expansion-function :initarg :expansion-function
			 :accessor meth~expansion-function
			 :documentation "The expansion function of a method.")
     (conclusions :initarg :conclusions
		  :accessor meth~conclusions
                  :documentation "The nodes to be (inserted) closed by the method.")
     (declarative-content :initarg :declarative-content
			  :accessor meth~declarative-content
			  :documentation "This is the declarative part of the tactic of a
method.")
     (procedural-content :initarg :procedural-content
			 :initform nil
			 :accessor meth~procedural-content
			 :documentation "The procedural content.")
     (help   :initarg :remark
	     :initform nil
	     :accessor meth~remark
	     :documentation "This is a slot where remarks can be stored, e.g. the purpose of this method, ...")
     (manual    :initarg :manual
		:initform nil
		:accessor meth~manual
		:documentation "This is a slot where the documentation should be stored, e.g. author, examples and documentation.")
)
    (:documentation "The basic datastructure for a method.")))


(defun meth~p (object)
  (typep object 'meth+method))

(defmethod print-object ((object meth+method) stream)
  (format stream "(method ~A)" (keim~name object)))

(eval-when (load compile eval)
  (defclass meth+supermethod (meth+method)
    ()
    (:documentation "The basic datastructure for supermethods.")))

(defun meth~supermethod-p (object)
  (typep object 'meth+supermethod))

(defmethod print-object ((object meth+supermethod) stream)
  (format stream "(supermethod ~A)" (keim~name object)))

(eval-when (load compile eval)
  (defclass meth+just (pdsj+justification)
    ((match-status :initform nil
		   :accessor meth~just-match-status
		   :documentation "The matching status of the justification."))
    (:documentation "The class of justifications of a method node."))
)

(defun meth=just-create (method premises params &optional status)
  (make-instance 'meth+just
		 :method method
		 :premises premises
		 :parameters params
		 :status status))

(defun meth~just-p (obj)
  (typep obj 'meth+just))

(defmethod meth~open-just-p ((just meth+just))
  (infer~open-p (just~method just)))

(eval-when (load compile eval)
  (defclass meth+node (pdsn+node)
    ((justification :initform nil
		    :documentation "The justification of the method node.")
     (sign :initform nil
	   :initarg :sign
	   :accessor meth~node-sign
	   :documentation "A sign specifying whether the node is to be deleted from or added to the planning state. It is either nil, + or -."))    
    (:documentation "This is the class of the meta nodes in the declarative content of a method."))
  )

(defun meth=node-create (label hyps formula just &optional sign)
  (make-instance 'meth+node
		 :name label
		 :hyps hyps
		 :formula formula
		 :justification just
		 :sign sign))

(defun meth~node-p (object)
  (declare (edited  "20-JUN-1997" "02-NOV-1994")
	   (authors Lassaad Acsehn)
	   (input "An OBJECT."  )
	   (effect "None." )  
	   (value "T, iff OBJECT is a planning node." ))
  (typep object 'meth+node))

(defun meth~plus-node-p (node)
  (when (meth~node-p node)
    (equal '+ (meth~node-sign node))))

(defun meth~zero-node-p (node)
  (when (meth~node-p node)
    (equal '0 (meth~node-sign node))))

(defun meth~minus-node-p (node)
  (when (meth~node-p node)
    (equal '- (meth~node-sign node))))

(defmethod print-object ((node meth+node) stream)
  (let* ((hyps-var (pdsn~hyps node))
	 (hyps (if (consp hyps-var)
		   (mapcar #'keim~name hyps-var)
		 hyps-var)))
    (if (meth~node-sign node)
	(format stream "{~A < ~A. ~A |- ~A (~A)>}"
		(meth~node-sign node) (keim~name node)
		hyps (node~formula node) (node~justification node))
    (format stream "< ~A. ~A |- ~A (~A)>" (keim~name node)
	    hyps (node~formula node) (node~justification node)))))

(defmethod print-object ((just meth+just) stream)
  (let ((prems (just~premises just))
	(method (just~method just)))
    (cond ((or (infer~dummy-p method)
	       (and (infer~p method) (consp prems)))
	   (call-next-method))
	  ((infer~p method)
	   (format stream "#<Justified by ~A from ~A>" (keim~name method) prems))
	  ((consp prems)
	   (format stream "#<Justified by ~A from ~A>" method (mapcar #'keim~name prems)))
	  (t (format stream "#<Justified by ~A from ~A>" method prems)))))

(eval-when (load compile eval)
  (defclass meth+meta-node (meth+node)
    ((metavar :initarg :metavar
	      :initform NIL
	      :accessor meth~meta-node-metavar
	      :documentation "A meta-variable of sort 'prlnlist' "))
    (:documentation "This is the class of the meta nodes in the declarative content of a
method."))
  )

(defmethod print-object ((node meth+meta-node) stream)
  (format stream "(Meta-node ~A)" (keim~name node)))

(defun meth=meta-node-create (label metavar &optional sign)
  (make-instance 'meth+meta-node
		 :name label
		 :metavar metavar
		 :sign sign))


(defun meth~meta-node-p (object)
  (declare (edited  "20-JUN-1997" "02-NOV-1994")
	   (authors Lassaad Acsehn)
	   (input "An OBJECT."  )
	   (effect "None." )  
	   (value "T, iff OBJECT is a planning node." ))
  (typep object 'meth+meta-node))

(eval-when (load compile eval)
  (defclass meth+condition (keim+name)
    ((args :initarg :args
	   :accessor meth=condition-args
	   :documentation "The arguments of the method condition."))
    (:documentation "A data structure for representing method conditions."))
  
  (defclass meth+constraint (keim+name)
  ((args :initarg :args
	 :accessor meth=constraint-args
	 :documentation "The arguments of the method constraint."))
  (:documentation "A data structure for representing method constraints."))

  (defclass meth+funcall (keim+name)
    ((args :initarg :args
	   :accessor meth=funcall-args
	   :documentation "The arguments of the method function call."))
    (:documentation "A data structure for representing method function calls."))

  (defclass meth+action (keim+name)
    ((support-nodes :initarg :support-nodes
		    :accessor meth=action-support-nodes
		    :documentation "Nodes to be deleted/added as supports by this action."))
    (:documentation "A data structure for representing actions."))

  (defclass meth+action-pattern ()
    ((supported-nodes :initarg :supported-nodes
		      :accessor meth=action-pattern-supported-nodes
		      :documentation "List of nodes (or a key word :all-opens) whose supports are affected by the actions of this action pattern.")
     (actions :initarg :actions
	      :accessor meth=action-pattern-actions
	      :documentation "List of actions which affect the supports of the supported nodes of this action pattern.")
     )
    (:documentation "A data structure for representing method actions."))

  (defclass meth+ordering (keim+name)
    ((ordered-nodes :initarg :ordered-nodes
		    :accessor meth~ordering-ordered-nodes
		    :documentation "The nodes to be ordered by this method ordering."))
    (:documentation "A data structure for representing method orderings."))
  
  (defclass meth+computation (keim+name)
    ((funcall :initarg :funcall
	      :type meth+funcall
	      :accessor meth=computation-funcall
	      :documentation "The function call of the method computation."))
    (:documentation "A data structure for representing method compuatations."))

  (defclass meth+key-expression (keim+name)
  ((args :initarg :args
	 :accessor meth=key-expression-args
	 :documentation "The arguments of the key-expression."))
  (:documentation "A data structure for representing expression with keywords."))
  )

(defun meth~connective-constraint-p (symbol)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "T, when the SYMBOL associated function is a logical"
		    "connective, otherwise NIL."))
  (find symbol meth*cstr-connectives :test #'string-equal))

(defmethod print-object ((constr meth+constraint) stream)
  (let ((constr-name (keim~name constr))
	(constr-args (meth=constraint-args constr)))
    (if (meth~connective-constraint-p constr-name)
	(format stream "~A[~{~A, ~}~A]" constr-name (butlast constr-args) (first (last constr-args)))
      (format stream "~A(~{~A, ~}~A)" constr-name (butlast constr-args) (first (last constr-args))))))
      

(defun meth~condition-p (object)
  (typep object 'meth+condition))

(defmethod print-object ((condition meth+condition) stream)
  (let ((cond-name (keim~name condition))
	(cond-args (meth=condition-args condition)))
    (if cond-args
	(if (meth~connective-function-p cond-name)
	    (format stream "~A[~{~A, ~}~A]" cond-name (butlast cond-args) (first (last cond-args)))
	  (format stream "~A(~{~A, ~}~A)" cond-name (butlast cond-args) (first (last cond-args))))
      (format stream "~A" cond-name))
    ))
      

(defun meth~funcall-p (object)
  (typep object 'meth+funcall))

(defmethod print-object ((funcall meth+funcall) stream)
  (if (meth=funcall-args funcall)
      (format stream "~A(~{~A, ~}~A)"
	      (keim~name funcall)
	      (butlast (meth=funcall-args funcall))
	      (first (last (meth=funcall-args funcall))))
    (format stream "~A" (keim~name funcall))))
    
		     

(defun meth~ordering-p (object)
  (typep object 'meth+ordering))

(defmethod print-object ((ordering meth+ordering) stream)
  (format stream "~A(~{~A, ~}~A)"
	  (keim~name ordering)
	  (butlast (meth~ordering-ordered-nodes ordering))
	  (first (last (meth~ordering-ordered-nodes ordering)))))

(defun meth~action-p (object)
  (typep object 'meth+action))

(defmethod print-object ((action meth+action) stream)
  (format stream "~A(~{~A, ~}~A)"
	  (keim~name action)
	  (butlast (meth=action-support-nodes action))
	  (first (last (meth=action-support-nodes action)))))

(defun meth~action-pattern-p (object)
  (typep object 'meth+action-pattern))

(defmethod print-object ((action-pat meth+action-pattern) stream)
  (format stream "<Apply ~{~A, ~}~A to: ~A>"
	  (butlast (meth=action-pattern-actions action-pat))
	  (first (last (meth=action-pattern-actions action-pat)))
	  (meth=action-pattern-supported-nodes action-pat)))

(defun meth~computation-p (object)
  (typep object 'meth+computation))

(defmethod print-object ((computation meth+computation) stream)
  (format stream "[~A <- ~A]"
	  (keim~name computation)
	  (meth=computation-funcall computation)))

(defun meth~key-expression-p (object)
  (typep object 'meth+key-expression))

(defmethod print-object ((key-expr meth+key-expression) stream)
  (format stream "[~A]" (meth=key-expression-args key-expr)))

(defun meth~create-key-expression (name expr)
  (make-instance 'meth+key-expression
		 :name name
		 :args expr))

(defun meth~connective-function-p (symbol)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "T, when the SYMBOL associated function is a logical"
		    "connective, otherwise NIL."))
  (find symbol meth*connectives :test #'string-equal))


(defun meth~quantifier-function-p (symbol)
  (declare (edited  "09-DEC-1998")
	   (authors  Lassaad)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "T, when the SYMBOL associated function is a logical"
		    "quantifier, otherwise NIL."))
  (find symbol meth*quantifiers :test #'string-equal))

(defun meth~relational-function-p (symbol)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "T, when the SYMBOL associated function is a relational"
		    "function, i.e., a predicate whose first arguments are"
		    "seen as input and whose last argument is seen as output,"
		    "otherwise NIL."))
  (find symbol meth*relational-functions :test  #'string-equal))

(defun meth~keyword-p (symbol)
  (declare (edited  "23-MAR-1998 09:22")
	   (authors SORGE)
	   (input   "A symbol.")
	   (value   "T if the SYMBOL is a keyword used in a method-declaration."))
  (find symbol meth*keywords))


(defmacro meth~defmethod (meth-name infer-name theory &rest attribs) 
  (declare (edited  "13-MAR-1998" "22-JUN-1997" "22-NOV-1996")
	   (authors Sorge Lassaad)
	   (input   "A method specification: name of the method to be defined, name of the"
		    "corresponding inference, a theory specification and the rest.")
	   (effect  "Defines a method using this specification.")
	   (value   "Unspecified."))
  `(block defmethod
     (let* ((name (quote ,meth-name))
	    (inference (let ((inf (infer~find-method (quote ,infer-name))))
			 (unless inf
			   (omega~error ";;;METH~~DEFMETHOD: The inference method ~A does not exist" (quote ,infer-name)))
			 inf))
	    (th-ident (if (string-equal (car (quote ,theory)) :in)
			  (cadr (quote ,theory))
			(omega~error ";;;METH~~DEFMETHOD: A theory is lacking for the definition of ~A" name)))
	    (theory (if (th~require-only th-ident)
			(th~find-theory th-ident)
		      (omega~error ";;;METH~~DEFMETHOD: The theory ~A does not exist." th-ident)))
	    (attribs (quote ,attribs))
	    (rating 0) (decls) (params) (just-params)
	    (prems) (concs) (outln-comps) (outln-acts)
	    (outln-orders)
	    (appl-cond) (exp-cond) (exp-comps) (decl-cont)
	    (proc-cont) (remark "") (manual) (non-reliable)
	    (meth-reasoning (list :planning)) (exp-func) 
	    (env (env~create (th~env theory))))
       (when (and inference th-ident theory)
	 (do ((attribs (cdr attribs) (cdr attribs))
	      (attrib (car attribs) (car attribs)))
	     ((and (null attrib) (null attribs)))
	   (if (consp attrib)
	       (cond 
		((string-equal (car attrib) :rating)                 (setq rating      (cadr attrib)))
		((string-equal (car attrib) :non-reliability)        (setq non-reliable (cadr attrib)))
		((string-equal (car attrib) :reasoning)              (setq meth-reasoning (cdr attrib)))
		((string-equal (car attrib) :declarations)           (setq decls       (cdr  attrib)))
		((string-equal (car attrib) :parameters)             (setq params      (cdr  attrib)))
		((string-equal (car attrib) :just-parameters)        (setq just-params (second  attrib)))
		((string-equal (car attrib) :premises)               (setq prems       (cdr  attrib)))
		((string-equal (car attrib) :application-condition)  (setq appl-cond   (cadr  attrib)))
		((string-equal (car attrib) :outline-computations)   (setq outln-comps (cdr  attrib)))
		((string-equal (car attrib) :outline-actions)        (setq outln-acts  (cdr  attrib)))
		((string-equal (car attrib) :outline-orderings)      (setq outln-orders (cdr  attrib)))
		((string-equal (car attrib) :expansion-computations) (setq exp-comps   (cdr  attrib)))
		((string-equal (car attrib) :expansion-condition)    (setq exp-cond   (cadr  attrib)))
		((string-equal (car attrib) :expansion-function)     (setq exp-func    (cdr attrib)))
		((string-equal (car attrib) :conclusions)            (setq concs       (cdr  attrib)))
		((string-equal (car attrib) :decl-content)           (setq decl-cont   (cdr  attrib)))
		((string-equal (car attrib) :proc-content)           (setq proc-cont   (rest  attrib)))
		((string-equal (car attrib) :remark)	             (setq remark      (rest attrib)))
		((string-equal (car attrib) :manual)	             (setq manual      (rest attrib)))
		(t (return-from defmethod (omega~error ";;;METH~~DEFMETHOD: Not expecting ~A" (car attrib)))))
	     (return-from defmethod (omega~error ";;;METH~~DEFMETHOD: Not expecting ~A" attrib))))
	 (meth~env-enter-metavarlist decls env)
	 (let* ((real-decl-cont (meth=complete-lines (meth=parse-lines decl-cont env) env))
		(real-params (mapcar #'(lambda (param)
					 (meth~build-object param env :parameter real-decl-cont))
				     params))
		(just-parameters (when just-params
				   (env~lookup-object just-params env
						      :test #'string-equal)))
		(real-concs (mapcar #'meth=set-match-status-open
				    (meth=prem&conc-lines concs real-decl-cont env)))
                (real-prems (meth=prem&conc-lines prems real-decl-cont env))
		(real-exp-comps (mapcar #'(lambda (computation)
					    (meth~build-object computation env :computation real-decl-cont))
					exp-comps))
		(real-outln-comps (mapcar #'(lambda (computation)
					     (meth~build-object computation env :computation real-decl-cont))
					 outln-comps))
		(real-outln-acts (mapcar #'(lambda (action)
					     (meth~build-object action env :action-pattern real-decl-cont))
					 outln-acts))
		(real-outln-orders (mapcar #'(lambda (order)
					       (meth~build-object order env :ordering real-decl-cont))
					   outln-orders))
		(real-appl-cond (meth~build-object appl-cond env :condition real-decl-cont))
		(real-exp-cond (meth~build-object exp-cond env :condition))
				
		(new-object (make-instance 'meth+method
					   :name name
					   :rating rating
					   :inference inference
					   :theory theory
					   :non-reliability non-reliable
					   :reasoning meth-reasoning
					   :parameters real-params
					   :premises real-prems
					   :just-parameters just-parameters
					   :conclusions real-concs
					   :application-condition real-appl-cond
					   :expansion-computations real-exp-comps
					   :expansion-condition real-exp-cond
					   :expansion-function exp-func
					   :outline-computations real-outln-comps
					   :outline-actions real-outln-acts
					   :outline-orderings real-outln-orders
					   :environment env
					   :declarative-content real-decl-cont
					   :procedural-content proc-cont
					   :remark remark
					   :manual manual)))
	   (meth~insert-method new-object)
	   new-object)))))


(defmacro meth~defsupermethod (meth-name infer-name theory &rest attribs) 
  (declare (edited  "18-SEP-1998")
	   (authors Jzimmer)
	   (input   "A supermethod specification: name of the supermethod to be defined, name of the"
		    "corresponding inference, a theory specification and the rest.")
	   (effect  "Defines a supermethod using this specification.")
	   (value   "Unspecified."))
  `(block defmethod
     (let* ((name (quote ,meth-name))
	    (inference (let ((inf (infer~find-method (quote ,infer-name))))
			 (unless inf
			   (omega~error ";;;METH~~DEFSUPERMETHOD: The inference method ~A does not exist" (quote ,infer-name)))
			 inf))
	    (th-ident (if (string-equal (car (quote ,theory)) :in)
			  (cadr (quote ,theory))
			(omega~error ";;;METH~~DEFSUPERMETHOD: A theory is lacking for the definition of ~A" name)))
	    (theory (if (th~require-only th-ident)
			(th~find-theory th-ident)
		      (omega~error ";;;METH~~DEFSUPERMETHOD: The theory ~A does not exist." th-ident)))
	    (attribs (quote ,attribs))
	    (rating 0) (decls) (params) (prems) (just-params)
	    (concs) (outln-comps) (outln-acts) (outln-orders) 
	    (appl-cond) (exp-comps) (exp-func) (exp-cond) (decl-cont) (proc-cont) (remark "") (non-reliable)
	    (meth-reasoning (list :planning)) 
	    (env (env~create (th~env theory))))
       (when (and inference th-ident theory)
	 (do ((attribs (cdr attribs) (cdr attribs))
	      (attrib (car attribs) (car attribs)))
	     ((and (null attrib) (null attribs)))
	   (if (consp attrib)
	       (cond 
		((string-equal (car attrib) :rating)                 (setq rating      (cadr attrib)))
		((string-equal (car attrib) :non-reliability)        (setq non-reliable (cadr attrib)))
		((string-equal (car attrib) :reasoning)              (setq meth-reasoning (cdr attrib)))
		((string-equal (car attrib) :declarations)           (setq decls       (cdr  attrib)))
		((string-equal (car attrib) :parameters)             (setq params      (cdr  attrib)))
		((string-equal (car attrib) :just-parameters)        (setq just-params (second attrib)))
		((string-equal (car attrib) :premises)               (setq prems       (cdr  attrib)))
		((string-equal (car attrib) :application-condition)  (setq appl-cond   (cadr  attrib)))
		((string-equal (car attrib) :outline-computations)   (setq outln-comps (cdr  attrib)))
		((string-equal (car attrib) :outline-actions)        (setq outln-acts  (cdr  attrib)))
		((string-equal (car attrib) :outline-orderings)      (setq outln-orders (cdr  attrib)))
		((string-equal (car attrib) :expansion-computations) (setq exp-comps   (cdr  attrib)))
		((string-equal (car attrib) :expansion-function)     (setq exp-func    (cdr  attrib)))
		((string-equal (car attrib) :conclusions)            (setq concs       (cdr  attrib)))
		((string-equal (car attrib) :decl-content)           (setq decl-cont   (cdr  attrib)))
		((string-equal (car attrib) :proc-content)           (setq proc-cont   (rest  attrib)))
		((string-equal (car attrib) :remark)	             (setq remark      (rest attrib)))
		((string-equal (car attrib) :manual)	             (setq manual      (rest attrib)))
		(t (return-from defmethod (omega~error ";;;METH~~DEFSUPERMETHOD: Not expecting ~A" (car attrib)))))
	     (return-from defmethod (omega~error ";;;METH~~DEFSUPERMETHOD: Not expecting ~A" attrib))))
	 (meth~env-enter-metavarlist decls env)
	 (let* ((real-decl-cont (meth=complete-lines (meth=parse-lines decl-cont env) env))
		(real-params (mapcar #'(lambda (param)
					 (meth~build-object param env :parameter real-decl-cont))
				     params))
                (just-parameters (when just-params
				   (env~lookup-object just-params env
						      :test #'string-equal)))
		(real-concs (mapcar #'meth=set-match-status-open
				    (meth=prem&conc-lines concs real-decl-cont env)))
                (real-prems (meth=prem&conc-lines prems real-decl-cont env))
		(real-exp-comps (mapcar #'(lambda (computation)
					    (meth~build-object computation env :computation real-decl-cont))
					exp-comps))
		(real-outln-comps (mapcar #'(lambda (computation)
					     (meth~build-object computation env :computation real-decl-cont))
					  outln-comps))
		(real-outln-acts (mapcar #'(lambda (action)
					     (meth~build-object action env :action-pattern real-decl-cont))
					 outln-acts))
		(real-outln-orders (mapcar #'(lambda (order)
					       (meth~build-object order env :ordering real-decl-cont))
					   outln-orders))
		(real-appl-cond (meth~build-object appl-cond env :condition real-decl-cont))
		(new-object (make-instance 'meth+supermethod
					   :name name
					   :rating rating
					   :inference inference
					   :theory theory
					   :non-reliability non-reliable
					   :reasoning meth-reasoning 
					   :parameters real-params
					   :just-parameters just-parameters
					   :premises real-prems
					   :conclusions real-concs
					   :application-condition real-appl-cond
					   :expansion-computations real-exp-comps
					   :expansion-function exp-func
					   :outline-computations real-outln-comps
					   :outline-actions real-outln-acts
					   :outline-orderings real-outln-orders
					   :environment env
					   :declarative-content real-decl-cont
					   :procedural-content proc-cont
					   :remark remark
					   :manual manual)))
	   
	   (meth~insert-method new-object)
	   new-object)))))

(defmacro meth~defstrategicmethod (methname control)
  `nil)

(defgeneric meth~label2node (decl-content node)
  (declare (edited  "18-MAR-1998")
	   (authors Sorge)
	   (input   "A method or a declarative content and a node or a label.")
	   (effect  "None.")
	   (value   "The node if it exists, o/w NIL."))
  (:method (decl-content node)
	   (error "~A should be a method or a list of nodes and ~A of type symbol or string or a metavariable"
		  decl-content node))
  (:method ((method meth+method) node)
	   (meth~label2node (meth~declarative-content method) node))
  (:method ((decl-content list) (node symbol))
	   (meth~label2node decl-content (symbol-name node)))
  (:method ((decl-content list) (node meth+node))
	   (when (find node decl-content) node))
  (:method ((decl-content list) (node term+syn-sorted-var))
	   (meth~label2node decl-content (keim~name node)))
  (:method ((decl-content list) (node string))
	   (find-if #'(lambda (x)
			(when (meth~node-p x)
			  (let ((label (keim~name x)))
			    (if (ssterm~var-p label)
				(string-equal node (keim~name label))
			      (string-equal node label)))))
		    decl-content)))
  
(defun meth=set-match-status-open (node)
  (when (meth~node-p node)
    (let ((just (node~justification node)))
      (when (meth~just-p just)
	(setf (meth~just-match-status just) :open))))
  node)

(defun meth=prem&conc-lines (descr line-list env)
  (declare (edited  "26-MAY-1998" )
	   (authors Jzimmer)
	   (input   "A description of premises or conclusions, a list of lines"
		    " and an environment.")
	   (effect  "Inserts plus or minus specifiers in some nodes.")
	   (value   "A list of real-nodes."))

  (when descr
    (let* ((node-descr (car descr))
	   (label (if (atom node-descr)
		      node-descr
		    (cadr node-descr)))
	   (sign (unless (atom node-descr)
		   (car node-descr)))
	   (real-node (meth~label2node line-list label)))
      (if real-node
	  (progn
	    (setf (meth~node-sign real-node) sign)
	    (cons real-node (meth=prem&conc-lines (cdr descr) line-list env)))
	(let* ((meta-var (env~lookup-object label env))
	       )
	  (if meta-var
	      (cons (meth=meta-node-create label meta-var sign)
		    (meth=prem&conc-lines (cdr descr) line-list env))
	    (omega~error ";;;METH~~DEFMETHOD: Line ~A does not have a counterpart in the
		      ;;;declarative content." label)))))))


(defun meth=complete-lines (assoc-list env)
  (declare (edited  "13-MAR-1998")
	   (authors Sorge)
	   (input   "An association-list with (labels . METH+NODEs).")
	   (effect  "Fills the gaps in the hyp-list and just-premises of the lines.")
	   (value   "A list of completed nodes."))
  (flet ((fill-node-list (list)
			 (mapcar #'(lambda (node)
				     (let ((real-node (assoc node assoc-list)))
				       (if real-node (cdr real-node)
					 (meth~enter-sorted-metavar node nil 'prln env))))
				 list)))
    (mapcar #'(lambda (x)
		(let* ((node (cdr x))
		       (hyps (pdsn~hyps node))
		       (just (node~justification node)))
		  (when hyps
		    (setf (pdsn~hyps node)
			  (if (consp hyps) (fill-node-list hyps)
			    (meth~enter-sorted-metavar hyps nil 'prln-list env))))
		  (when (and just (meth~just-p just) (just~premises just))
		    (let ((premises (just~premises just)))
		    (setf (just~premises just)
			  (if (consp premises) (fill-node-list premises)
			    (meth~enter-sorted-metavar premises nil 'prln-list env)))))
		  node))
	    assoc-list)))
			    

(defun meth=parse-lines (decl-content env)
  (declare (edited  "13-MAR-1998")
	   (authors Sorge)
	   (input   "A method representation of a declarative content and an environment.")
	   (effect  "Creates METH+NODEs for the content.")
	   (value   "A association list associating line labels to meta-nodes."))
  (when decl-content
    (let* ((line (car decl-content))
	   (label (first line))
	   (hyps (second line))
	   (formula (third line))
	   (just (fourth line)))
      (acons label
	     (meth=node-create
	      (meth~enter-sorted-metavar label nil 'prln env)
	      hyps
	      (post~read-object formula env :existing-term)
	      (meth=make-meta-just just env))
	     (meth=parse-lines (cdr decl-content) env)))))

(defun meth=make-meta-just (just env)
  (declare (edited  "23-MAR-1998 09:48")
	   (authors SORGE)
	   (input   "A string or symbol or a list as representation of a meta-justification and an environment.")
	   (effect  "Creates an object of type METH+JUST and enters not yet existing meta-variables into the environment.")
	   (value   "The newly created meta-justification."))
  (cond ((null just) nil)
	((consp just)
	 (let ((method (first just))
	       (params (second just))
	       (prems  (third just)))
	   (meth=just-create
	    (cond ((null method)
		   (omega~warn "NIL as justification method is rather weird!")
		   nil)
		  ((infer~find-method method) (infer~find-method method))
		  (t (meth~enter-sorted-metavar method nil 'inference env)))
	    prems
	    (cond ((null params) nil)
		  ((consp params)
		   (mapcar #'(lambda (x) (meth~enter-sorted-metavar x nil 'parameter env))
			   params))
		  (t (meth~enter-sorted-metavar params nil 'parameter-list env))))))
	(t (meth~enter-sorted-metavar just nil 'just env))))
  

(defun meth~env-enter-metavarlist (decls env)
  (declare (edited  "13-MAR-1998")
	   (authors Sorge)
	   (input   "A list of declarations and an environment.")
	   (effect  "Enters the declarations into the environment.")
	   (value   "Undefined."))
  (mapc #'(lambda (decl)
	    (if (string-equal (car decl) :sorted-meta-variables)
		(mapc #'(lambda (var)
			  (if (third var)
			      (meth~enter-sorted-metavar (first var) (second var) (third var) env)
			    (meth~enter-sorted-metavar (first var) (second var) 'term env)))
		      (cdr decl))
	      (post~read-object-list (list decl) env)))
	decls))


(defun meth~enter-sorted-metavar (variable type sort env)
  (declare (edited  "13-MAR-1998")
	   (authors Sorge)
	   (input   "The name of a sorted metavariable, its type and sort and an envrionment.")
	   (effect  "Creates the metavariable and enters it into the environment. If the sort"
		    "does not yet exist it is entered in the global envionrment for syntactic sorts.")
	   (value   "The newly created variable."))
  (let ((var (env~lookup-object variable env)))
    (if var var
      (let ((real-type (when type (post~read-object type env :existing-type)))
	    (real-sort (ssterm~get-ssort sort)))
	(when (and sort (not real-sort))
	  (ssterm~add-ssort sort)
	  (setf real-sort (ssterm~get-ssort sort)))
	(let ((new-var (ssterm~var-create variable real-sort real-type)))
	  (env~enter variable new-var env)
	  new-var)))))


(defgeneric meth~build-object (expression env indicator &optional decl-cont)
  (declare (edited  "23-MAR-1998")
	   (authors Sorge)
	   (input   "A representation of a certain object, an environment, and an indicator."
		    "(Optionally a declarative content.)")
	   (effect  "Creates an object according to the indicator and enters not yet existing"
		    "meta-variables into the environment.")
	   (value   "The newly created object."))
  (:method ((expression null) (env env+environment) indicator &optional decl-cont)
	   (declare (ignore indicator decl-cont)))
  (:method (expression (env env+environment) indicator &optional decl-cont)
	   (declare (ignore expression decl-cont))
	   (error "No method defined for indicator ~A does not exist." indicator))
  (:method (expression env indicator &optional decl-cont)
	   (declare (ignore expression indicator decl-cont))
	   (error "~A is not a valid environment." env))
  (:method (constraint (env env+environment) (indicator (eql :constraint)) &optional decl-cont)
	   (let* ((head (first constraint))
		  (args (if (meth~connective-constraint-p head)
			    (mapcar #'(lambda (arg) (meth~build-object arg env :constraint decl-cont))
				    (rest constraint))
			  (mapcar #'(lambda (arg) (meth~build-object arg env :argument decl-cont))
				  (rest constraint)))))
	     (make-instance 'meth+constraint
			    :name head
			    :args args)))
  (:method (condition (env env+environment) (indicator (eql :condition)) &optional decl-cont)
	   (let* ((connective (car condition))
		  (args (cond ((meth~connective-function-p connective)
			       (mapcar #'(lambda (cond) (meth~build-object cond env :condition decl-cont))
				       (cdr condition)))
			      ((meth~quantifier-function-p connective)
			       (list (meth~build-object (second condition) env :argument)
				     (meth~build-object (third condition) env :argument)
				     (meth~build-object (fourth condition) env :condition)))
			      ((meth~sym2condition connective)
			       (when (cdr condition)
				 (mapcar #'(lambda (cond) (meth~build-object cond env :argument))
					 (cdr condition))))
                              (t (return-from meth~build-object
                                   (omega~error "Unknown condition with head symbol ~A" connective))))))
	     (make-instance 'meth+condition
			    :name connective
			    :args args)))
  (:method (comp (env env+environment) (indicator (eql :computation)) &optional decl-cont)
	   (let* ((result (car comp))
		  (var (if (consp result)
			   (mapcar #'(lambda (v)
				       (meth~enter-sorted-metavar v nil nil env))
				   result)
			 (meth~enter-sorted-metavar result nil nil env)))
		  (function (meth~build-object (cadr comp) env :argument decl-cont)))
	     (make-instance 'meth+computation
			    :name var
			    :funcall function)))
  (:method (action-pat (env env+environment) (indicator (eql :action-pattern)) &optional decl-cont)
	   (let ((supported-nodes (remove-if #'listp action-pat)))
	     (make-instance 'meth+action-pattern
			    :supported-nodes (if (string-equal (first supported-nodes) :all-opens)
						 :all-opens
					       (mapcar #'(lambda (in)
							   (meth~build-object in env :argument decl-cont))
						       supported-nodes))
			    :actions (mapcar #'(lambda (act) (meth~build-object act env :action decl-cont))
					     (remove-if-not #'listp action-pat)))))
  (:method (action (env env+environment) (indicator (eql :action)) &optional decl-cont)
	   (let ((head (first action)))
	     (if (meth~sym2action head)
		 (make-instance 'meth+action
				:name head
				:support-nodes (mapcar #'(lambda (sn)
							   (meth~build-object sn env :argument decl-cont))
						       (rest action)))
	       (omega~error "Unknown action with head symbol ~A" head))))
  (:method (ordering (env env+environment) (indicator (eql :ordering)) &optional decl-cont)
	   (let ((head (first ordering)))
	     (if (find head meth*ordering-keywords :test #'string-equal)
		 (make-instance 'meth+ordering
				:name head
				:ordered-nodes (mapcar #'(lambda (on)
							   (meth~build-object on env :argument decl-cont))
						       (rest ordering)))
	       (omega~error "Unknown ordering with head symbol ~A" head))))
  (:method ((param symbol) (env env+environment) (indicator (eql :parameter)) &optional decl-cont)
	   (let ((the-param (or (meth~label2node decl-cont param)
				(env~lookup-object param env))))
	     (if the-param
		 (if (or (meth~node-p the-param) (ssterm~sort the-param))
		     the-param
		   (omega~error "Parameter ~A declared without a sort!" param))
	       (omega~error "Parameter ~A must be declared!" param))))
  (:method ((param cons) (env env+environment) (indicator (eql :parameter)) &optional decl-cont)
	   (declare (ignore decl-cont))
	   (cond (;;; for sorted parameters
		  (third param)
		  (meth~enter-sorted-metavar (first param) (second param) (third param) env))
		 (;;;LC: for the use of composed parameters
		  (meth~keyword-p (car param))
		  (meth~build-with-keyword (cadr param) env (car param)))
		 (T
		  (omega~error "Parameter ~A declared without a sort!~%OR Unknown keyword ~A!"
			       (first param) (first param)))))
  (:method ((argument symbol) (env env+environment) (indicator (eql :argument)) &optional decl-cont)
	   (or
	    (meth~label2node decl-cont argument)
	    (meth~enter-sorted-metavar argument nil nil env)))
  (:method ((argument string) (env env+environment) (indicator (eql :argument)) &optional decl-cont)
	   (declare (ignore decl-cont))
	   argument)
  (:method ((argument number) (env env+environment) (indicator (eql :argument)) &optional decl-cont)
	   (declare (ignore decl-cont))
	   (term~constant-create argument (env~lookup-object :num env)))
  (:method ((argument cons) (env env+environment) (indicator (eql :argument)) &optional decl-cont)
	   (if (meth~keyword-p (car argument))
	       (meth~build-with-keyword (cadr argument) env (car argument))
	     (meth~build-object argument env :function decl-cont)))
  (:method (function (env env+environment) (indicator (eql :function)) &optional decl-cont)
	   (let ((head (car function)))
	     (if (meth~sym2function head)
		 (let ((args (when (cdr function)
			       (mapcar #'(lambda (arg)
					   (meth~build-object arg env :argument decl-cont))
				       (cdr function)))))
		   (make-instance 'meth+funcall
				  :name head
				  :args args))
	       (if (string-equal head 'if)
		   (let ((arg1 (meth~build-object (second function) env :condition decl-cont))
			 (rest-args (mapcar #'(lambda (arg)
						(meth~build-object arg env :argument decl-cont))
					    (cddr function))))
		     (make-instance 'meth+funcall
				    :name head
				    :args (cons arg1 rest-args)))
		 (omega~error "Unknown function with head symbol ~A" head))))))


(defgeneric meth~build-with-keyword (expression env keyword)
  (declare (edited  "23-MAR-1998 10:58")
	   (authors SORGE)
	   (input   "An expression, an envrionment, and a keyword indicating the parsing of the expression.")
	   (effect  "Can create objects and enter meta-variables into the environment.")
	   (value   "An object created with the expression."))
  (:method (expression env keyword)
	   (declare (ignore expression env))
	   (omega~error "No method implemented for keyword ~A." keyword))
  (:method (expression env (keyword (eql :term)))
	   (meth~create-key-expression
	    :term
	    (meth=build-expression expression env keyword)))
  (:method (expression env (keyword (eql :type)))
	   (meth~create-key-expression
	    :type
	    (meth=build-expression expression env keyword)))
  (:method ((expression list) env (keyword (eql :position)))
	   (meth~create-key-expression
	    :position
	    (mapcar #'(lambda (x)
			(cond ((integerp x) x)
			      ((numberp x) (omega~error "~A is not an integer!" x))
			      (t (meth~enter-sorted-metavar x nil 'integer env))))
		    expression)))
  (:method ((expr symbol) env (keyword (eql :symbol)))
	   (meth~create-key-expression :symbol expr))
  (:method (expression env (keyword (eql :position)))
	   (meth~create-key-expression
	    :position
	    (meth~enter-sorted-metavar expression nil 'position env)))
  )

(defgeneric meth=build-expression (expr env indicator &optional bvars)
  (declare (edited  "09-APR-1998")
	   (authors Lassaad) 
	   (input   "An expression, an environment, a symbol indicating a"
		    "default sort, and a list of symbols representing unsorted"
		    "bound variables within EXPR.")
	   (effect  "Can create meta-variables and enter them into ENV.")
	   (value   "The expression with symbols substituted by objects"
		    "of the environment and, if EXPR contains an abstraction"
		    "then additionally to the ENV-objects the symbol :lam and"
		    "eventually symbols staying for unsorted bound variables,"
		    "i.e., occurring in BVARS."))
  (:method ((nix null) (env env+environment) indicator &optional bvars)
	   (declare (ignore indicator bvars))
	   nil)
  (:method ((expr cons) (env env+environment) (indicator (eql :term)) &optional bvars)
	   (let ((head (first expr)))
	     (if (and (symbolp head) (string-equal head :lam))
                 ;;; we have to parse a lambda abstraction:
		 ;; (lam (x1 t1) ... (xn tn) phi[x1, ... xn]) 
		 (let* ((rest-expr (rest expr))
			(matrix (butlast rest-expr))
			(the-matrix (meth=build-expression matrix env :lam-domain bvars)))
		   (cons head 
			 (append the-matrix
				 (meth=build-expression (last expr) env :term
							(remove-if-not #'symbolp
								       (mapcar #'first the-matrix))))))
	       (cons (meth=build-expression head env :term bvars)
		     (meth=build-expression (rest expr) env :term bvars)))))
  (:method ((sym symbol) (env env+environment) (indicator (eql :term)) &optional bvars)
	   (if (find sym bvars)
	       sym
	     (meth~enter-sorted-metavar sym nil 'term env)))
  (:method ((sym number) (env env+environment) (indicator (eql :term)) &optional bvars)
	   (declare (ignore bvars))
	   sym)
  (:method ((vars cons) (env env+environment) (indicator (eql :lam-domain)) &optional bvars)
	   (let* ((var1 (first vars))
		  (var1-sort (third var1)))
	     (if var1-sort
		 (cons (meth~enter-sorted-metavar (first var1) (second var1) var1-sort env)
		       (meth=build-expression (rest vars) env :lam-domain bvars))
	       (let ((var1-name (first var1)))
		 (if (find var1-name bvars)
		     (omega~error ";;;meth=build-expression: ~A denotes at least two different bound variables!"
				  var1-name)
		   (cons (list var1-name
			       (meth=build-expression (second var1) env :type))
			 (meth=build-expression (rest vars) env :lam-domain (cons var1-name bvars))))))))
  (:method (type-expr (env env+environment) (indicator (eql :type)) &optional bvars)
	   (declare (ignore bvars))
	   (post~read-object type-expr env :existing-type))
  (:method (expr (env env+environment) indicator &optional bvars)
	   (declare (ignore bvars))
	   (omega~error ";;; Dont know how to determine expression ~A instance of ~A indicating by ~A"
			expr (type-of expr) indicator))
  )
 


;----------------------------------- Accessors for Method Datastructure -----------

(defgeneric meth~exist-conclusions (method)
  (declare (edited  "27-MAR-1995")
	   (authors Lassaad)
	   (input "a method"  )
	   (effect "None." )
	   (value  "The lines which must be open in the planning state. (the conclusions lines whithout plus lines)" ))
  (:method ((method meth+method))
	   (remove-if #'(lambda (line)
			  (meth~plus-node-p line))
		      (meth~conclusions method))))

(defgeneric meth~exist-premises (method)
  (declare (edited  "27-MAR-1995")
	   (authors Lassaad)
	   (input "a method"  )
	   (effect "None." )
	   (value  "The premises which must exist in the planning state. (the premises lines whithout plus lines)" ))
  (:method ((method meth+method))
	   (remove-if #'(lambda (line)
			  (meth~plus-node-p line))
		      (meth~premises method))))

(defun meth~closed-premises (method)
  ;;; Heuristic: Minus premises must be closed, because they
  ;; will be deleted.
  (remove-if-not #'meth~minus-node-p
                 (meth~premises method)))

(defun meth~existent-premises (method)
;  ;;; Heuristic: Unsigned premises correspond to existent
;  ;; Diese Ueberlegungen gelten nicht mehr ganz:
;  ;; Sowohl EXISTENT als auch CLOSED premises koennen geloecht
;  ;; werden (geloecht von den Supports anderer offenen Knoten).
;  ;; Das koennte man durch die Actions realisieren.
;  ;; Auf der anderen Seite man koennte eine zusaetzliche Markierung
;  ;; fuer die eine Art von existent Praemissen, die geschlossen sein
;  ;; muessen. Diese zusaetzliche Bedingung muss aber deklarativ durch
;  ;; eine Spezialisierung der Praemisse representiert werden und nicht
;  ;; ueber application conditions.
;  ;;; Fazit: Man koennte folgende Markierungen fuer die Praemissen haben:
;  ;; (+ P): P wird nicht gematcht und wird als Unterziel erzeugt -> NONEXISTENT
;  ;; (! P): P wird gematcht zu eiem geschlossenen Knoten -> CLOSED
;  ;; P: P wird gematcht zu einem bliebigen Knoten -> EXISTENT
;  ;; ---------- Moegliche Erweiterungen -----------
;  ;; (? P): Es wird versucht P zu matchen zu einem beliebigen Knoten, falls
;  ;;      unmoeglich, wird fuer P ein neues Ziel erzeugt. Dies ermoeglicht
;  ;;      eine flexiblere Methodenanwendung und die Zusammenfassung von mehreren
;  ;;      Methoden in einer einzigen Methode.
;  ;; z.B: AndI-m
;  ;; - conclusions: [and A B]
;  ;; - premises: (? A) (? B)
  (remove-if #'(lambda (node)
		 (or (meth~minus-node-p node)
		     (meth~meta-node-p node)))
	     (meth~exist-premises method)))


(defun meth~real-existent-premises (method)
  (remove-if #'(lambda (node)
		 (meth~minus-node-p node))
	     (meth~exist-premises method)))

(defgeneric meth~add-premises (method)
  (declare (edited  "27-MAR-1995")
	   (authors Lassaad)
	   (input "a method"  )
	   (effect "None." )
	   (value  "The lines which must be added open into the planning state if they does not exist. (the plus lines of the premises)" ))
  (:method ((method meth+method))
	   (remove-if-not #'(lambda (line)
                              (meth~plus-node-p line))
			  (meth~premises method))))

(defgeneric meth~add-conclusions (method)
  (declare (edited  "27-MAR-1995")
	   (authors Lassaad)
	   (input "a method"  )
	   (effect "None." )
	   (value  "The support lines which must be added into the planning state. (the plus lines of the conclusions)" ))
  (:method ((method meth+method))
	   (remove-if-not #'(lambda (line)
			      (meth~plus-node-p line)
			      )
			  (meth~conclusions method))))

;---------------------------- Functions and Predicates ------------------------------------
(defmethod meth=show-node((node meth+node))
  (let ((*print-pretty* nil)
	(*print-length* 100)
	(*print-miser-width* nil)
	)
    (dotimes (i meth*indent)
      (format t " "))
    (format t "~A ~A ! ~A ~A"
	    (keim~name node)
	    (pdsn~hyps node)
	    (node~formula node)
	    (node~justification node))))

(defun meth~show-nodes (nodelist &optional (stream t))
  (mapcar #'(lambda (node)
	      (if (typep node 'meth+node)
		  (meth=show-node node)
		(format t "~A" node))
	      (terpri stream))
	  nodelist)
  nil)

(defgeneric meth~expansion-nodes (method)
  (declare (edited  "21-JUN-1997" "20-APR-1995")
	   (authors Lassaad Acsehn)
	   (input "A METHOD." )
	   (effect "None." )
	   (value "The list of all nodes in the declarative content of METHOD which are
created new by METHOD." ))
  (:method ((method meth+method))
	   (let ((nodes (union (meth~premises method)
			       (meth~conclusions method))))
	     (remove-if #'(lambda (node) (member node nodes)) (meth~declarative-content method)))))
  
(defgeneric meth~preconditions (method)
  (declare (edited  "21-JUN-1997" "02-NOV-1994")
	   (authors Lassaad Acsehn)
	   (input "A METHOD."  )
	   (effect "None." )
	   (value "A list of all nodes that must be present in the current proof state,
~i.e., all nodes not marked with a plus sign." ))
  (:method ((method meth+method))
	   (union (meth~exist-conclusions method)
		  (meth~exist-premises method))))

(defgeneric meth~add&expansion-nodes (method)
  (declare (edited  "21-JUN-1997" "02-NOV-1994")
	   (authors Lassaad Acsehn)
	   (input "A METHOD."  )
	   (effect "None." )
	   (value "A list of all nodes are created by the application of METHOD."))
  (:method ((method meth+method))
	   (append (meth~add-premises method)
		   (meth~add-conclusions method)
		   (meth~expansion-nodes method))))

(defun meth~goal (method)
  (declare (edited  "02-APR-1998")
	   (authors Lassaad)
	   (input   "a METHOD.")
	   (effect  "None.")
	   (value   "The method node representing the goal of the"
		    "METHOD, i.e., closed by a backward application"
		    "of METHOD, if METHOD is a backward method, otherwise NIL."))
  (let* ((concs (meth~conclusions method))
         (minus-concs (remove-if-not #'meth~minus-node-p concs)))
    (when minus-concs
      (if (rest minus-concs)
	  (omega~error "METH~~THEOREM: Method with more than one goal.")
	(first minus-concs))
      )
    ))

(defun meth=set-difference (set1 set2)
  (remove-if #'(lambda (x) (find x set2)) set1))

(defun meth~proved-concs (method &optional subst)
  (declare (edited  "12-APR-1996")
	   (authors Lassaad)
	   (input   "a METHOD")
	   (effect  "None")
	   (value   "The nodes proven by METHOD: the conclusions of method wich are"
		    "not hypothesis."))
  (let* ((concs (meth~conclusions method))
         (hyps (remove-if-not #'(lambda (conc) 
                                   (and (meth~plus-node-p conc)
                                        (meth~hypothesis-node-p conc subst)))
                              concs)))
     (meth=set-difference concs hyps)
    ))

(defun meth~hypothesis-node-p (meta-node &optional subst)
  (if subst
      (let* ((var (keim~name meta-node))
	     (node (subst~get-component var subst)))
	(if node
	    (pdsn~hypothesis-node-p  node)
	  (meth=hyp-node-p meta-node)))
    (meth=hyp-node-p meta-node)))

(defmethod meth=hyp-node-p ((node meth+node))
  (let ((node-just (node~justification node)))
    (if (meth~just-p node-just)             
	(let ((method (just~method node-just)))
	  (and (infer~dummy-p method)
	       (not (infer~open-p method))))
      (find-if #'(lambda (hyp)
		   (or (equal hyp (keim~name node))
		       (and (meth~node-p hyp) 
			    (equal (keim~name hyp) (keim~name node)))))
	       (pdsn~hyps node)))
    ))

(defun meth~given-premises (method)
  (declare (edited  "12-APR-1996")
	   (authors Lassaad)
	   (input   "a METHOD")
	   (effect  "None")
	   (value   "the unmarked nodes within the METHOD premises."))
  (remove-if #'(lambda (ln) (or (meth~plus-node-p ln)
				(meth~minus-node-p ln)))
	     (meth~premises method))
  )

(defgeneric meth~add-nodes (method)
  (declare (edited  "28-MAR-1995")
	   (authors Lassaad)
	   (input "A method"  )
	   (effect "None" )
	   (value "A list of all nodes that must be added in a plan state"  ))
  (:method ((method meth+method))
	   (let ((decl-content (meth~declarative-content method)))
	     (remove-if-not #'meth~plus-node-p decl-content))))


  
;----------------------- Defining new methods (provisorisch) -----------------------


(defgeneric meth~show-method-hashtable (&optional stream)
  (declare (edited  "22-JUN-1997"  "20-APR-1995")
	   (authors Lassaad Acsehn)
	   (input "Optionally a STREAM, default is *standard-output*."  )
	   (effect "Prints the hashtable of methods to STREAM in a readable way." )
	   (value "None." ))
  (:method (&optional (stream t))
	   (maphash #'(lambda (key inh)
			(format stream "~A:~A" key inh)
			(terpri stream))
		    meth*method-hashtable)
	   nil))

(defgeneric meth~find-method (name)
  (declare (edited  "22-JUN-1997"  "20-APR-1995")
	   (authors Lassaad Acsehn)
	   (input "A NAME of a method (symbol or string)."  )
	   (effect "None." )
	   (value "1. The method with NAME, or nil if none exists."
		  "2. T, if the method is found, otherwise nil. "))
  (:method (method)
	   (declare (ignore method)))
  (:method ((name symbol))
	   (gethash (symbol-name name) meth*method-hashtable))
  (:method ((name string))
	   (gethash (string-upcase name) meth*method-hashtable))
  (:method ((method meth+method))
	   (let ((name (keim~name method)))
	   (gethash (etypecase name
			(symbol (symbol-name name))
			(string (string-upcase name)))
		    meth*method-hashtable))))


(defun meth~current-theory-methods! (&optional (theory (prob~proof-theory omega*current-proof-plan)))
  (declare (edited  "18-NOV-1998")
	   (authors Lassaad)
	   (input   "A theory which corresponds to the current problem theory.")
	   (effect  "Resets the method lists which are considered from the planner so that they only"
		    "contain methods visible in THEORY.")
	   (value   "Undefined."))
  (let ((theories (cons theory (th~imports-recursed theory))))
    (setf meth*MOReasoning-methods NIL)
    (setf meth*planning-methods NIL)
    ;(setf meth*MOReasoning-parammeths NIL)
    ;(setf meth*planning-parammeths NIL)
    (setf meth*normalizing-methods NIL)
    (setf meth*restricting-methods NIL)
    (maphash #'(lambda (key method)
		 (declare (ignore key))
		 (when (find (meth~theory method) theories)
		   (meth=classify-method! method)))
	     meth*method-hashtable)))
		 
    
(defgeneric meth~insert-method (method)
  (declare (edited  "24-JUN-1998" "22-JUN-1997")
	   (authors Lassaad)
	   (input "A METHOD." )
	   (effect "Inserts METHOD in the hashtable of methods." )
	   (value "The method." ))
  (:method ((method meth+method))
	   (when (member 'methods th*output)
	     (omega~message ";;; Defining method ~A" (keim~name method)))*
	   ;;; Insert METHOD in the hash-table:
	   (when (gethash (symbol-name (keim~name method)) meth*method-hashtable)
	     (meth~remove-method (gethash (symbol-name (keim~name method)) meth*method-hashtable)))
	   (setf (gethash (symbol-name (keim~name method)) meth*method-hashtable)
		 method)
	   ;;; Classify METHOD:
	   (meth=classify-method! method)
	   method)
  (:method ((methodlist list))
	   (dolist (method methodlist)
	     (meth~insert-method method))))


(defun meth=classify-method! (method)
  (declare (edited  "18-NOV-1998")
	   (authors Lassaad)
	   (input   "A method.")
	   (effect  "Inserts METHOD in the corresponding method list to be"
		    "considered from the planner.")
	   (value   "Unspecified."))
  (let ((meth-reasoning (meth~reasoning method)))
    (when (find :middle-out meth-reasoning)
;      (if (meth~parameters method)
;          (setf meth*MOReasoning-parammeths
;                (meth=add-method-sorted method (meth~rating method)
;                                        meth*MOReasoning-parammeths))
      (setf meth*MOReasoning-methods
	    (meth=add-method-sorted method (meth~rating method)
				    meth*MOReasoning-methods)))
    (when (find :planning meth-reasoning)
      (if (meth~parameters method)
	  (setf meth*planning-methods
		(meth=add-method-sorted method (meth~rating method) ;; Used to be meth*parammeths...
					meth*planning-methods))
	(setf meth*planning-methods
	      (meth=add-method-sorted method (meth~rating method)
				      meth*planning-methods))))
    (when (find :normalizing meth-reasoning)
      (setf meth*normalizing-methods
	    (meth=add-method-sorted method (meth~rating method)
				    meth*normalizing-methods)))
    (when (find :restricting meth-reasoning)
      (setf meth*restricting-methods
	    (meth=add-method-sorted method (meth~rating method)
				    meth*restricting-methods)))
    ))


(defun meth=add-method-sorted (method rating meth-list)
  (declare (edited  "23-APR-1998")
	   (authors Lassaad)
	   (input   "A method, a number, and a method list that does not"
		    "contain METHOD.")
	   (effect  "None.")
	   (value   "The list resulted by inserting METHOD into METH-LIST"
		    "according to RATING."))
  (if meth-list
      (let ((first-meth (first meth-list)))
        (if (> (meth~rating first-meth) rating)
            (cons first-meth
                  (meth=add-method-sorted method rating (rest meth-list)))
          (cons method meth-list)))
    (list method)))


(defun meth~critical-p (method)
  (declare (edited  "05-JUN-2003")
	   (authors Vxs)
	   (input   "A method")
	   (effect  "None.")
	   (value   "T if the method is critical according to its reasoning slot. O/w NIL."))
  (when (find :critical (meth~reasoning method)) t))

(defun meth~remove-methods ()
  (declare (edited  "09-JAN-2000")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Removes all known methods.")
	   (value   "Undefined."))
  (setf meth*planning-methods nil
	meth*MOReasoning-methods nil
	meth*normalizing-methods nil
	meth*restricting-methods nil)
  (clrhash meth*method-hashtable)
  (values))
  

(defgeneric meth~remove-method (method)
  (declare (edited  "26-APR-1998" "23-APR-1998")
	   (authors Sorge Lassaad)
	   (input   "A method.")
	   (effect  "Removes the method form the lists of sorted planning methods.")
	   (value   "Undefined."))
  (:method ((method meth+method))
	   (setf meth*planning-methods (remove method meth*planning-methods))
	   ;(setf meth*planning-parammeths (remove method meth*planning-parammeths))
	   (setf meth*MOReasoning-methods (remove method meth*MOReasoning-methods))
	   ;(setf meth*MOReasoning-parammeths (remove method meth*MOReasoning-parammeths))
	   (setf meth*normalizing-methods (remove method  meth*normalizing-methods))
	   (setf meth*restricting-methods (remove method  meth*restricting-methods)))
  (:method ((method symbol))
	   (meth~remove-method (gethash (symbol-name method) meth*method-hashtable)))
  (:method ((method string))
	   (meth~remove-method (gethash (string-upcase method) meth*method-hashtable))))


(defmethod pds~get-inference-env ((method meth+method))
  (meth~environment method))

(defmethod pds~inference-application ((inference infer+method) (outline-pat list))
  (let* ((an-appl (meth~find-method (infer~find-arbitrary-application-name inference)))
	 (tacappl? (and an-appl (string-equal (car (meth~procedural-content an-appl)) 'apply-tactic)))
	 (new-outline (if tacappl? (remove-if #'infer~nonexistent-pattern-p outline-pat) outline-pat)))
    (or (meth~find-method (infer~outline-pattern2application inference new-outline))
	(meth~find-method (infer~outline-pattern2application inference (remove-if #'infer~nonexistent-pattern-p outline-pat)))  ;;; caution (for methods with variable premises, will only work sometimes) VS
	(meth~find-method (infer~outline-pattern2application inference outline-pat))  ;;; for methods that apply a tactic MP
	(when (= (length (infer~find-outline-methodnames (keim~name inference))) 1)   ;;maybe we are lucky
	  (meth~find-method (car (infer~find-outline-methodnames (keim~name inference)))))
	(meth~find-method (keim~name inference))
	(omega~error ";;;pds~~inference-application: Inconsistent mapping of some method to the inference ~A"
		       (keim~name inference)))))

(defmethod pds~inference-application ((inference infer+supermethod) (outline-pat list))
  (meth~find-method (infer~supermethod inference)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Condition checking....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (load compile eval)
  (defclass meth+mapping ()
    ((subst :initarg :subst
	    :initform (subst~create nil nil)
	    :type subst+substitution
	    :accessor meth~mapp-subst
	    :documentation "A mapping of method objects which correspond to terms and types.")
     (mapp :initarg :mapp
	   :initform (mapp~create nil nil)
	   :type mapp+mapping
	   :accessor meth~mapp-mapp
	   :documentation "A mapping of method objects which correspond neither to terms nor to types.")
     (extension :initarg :extension
 	        :initform nil
 	        :accessor meth~mapp-extension
 	        :documentation "A mapping of method objects to be extended during the evaluation of the method application condition.")
     (constraint :initarg :constraint
 		 :initform T
 		 :accessor meth~mapp-constraint
 		 :documentation "A constraint, where the empty constraint is T and the unsatisfiable constraint is NIL."))
    (:documentation "A data structure for the result of method applicability, it contains bindings of method objects and constraints on meta-variables."))
  )


;;; LC: HACK to test critics:
;(defun meth~mapping-p (object)
;  (typep object 'meth+mapping))
(defun meth~mapping-p (object)
  (or (typep object 'meth+mapping)
      (pds~constraint-pool-p object)))


(defmethod print-object ((object meth+mapping) stream)
  (let ((ext (meth~mapp-extension object))
	(cstr (meth~mapp-constraint object)))
    (cond (ext
	   (cond ((or (cstr~constraint-p cstr)
		      (pds~constraint-pool-p cstr))
		  (format stream "~%<~A~% ~A~% ~A~% ~A>" 
			  (meth~mapp-subst object)
			  (meth~mapp-mapp object)
			  ext cstr))
		 (T
		  (format stream "~%<~A~% ~A~% ~A~% ~A>" 
			  (meth~mapp-subst object)
			  (meth~mapp-mapp object)
			  ext cstr))))
	  ((or (cstr~constraint-p cstr)
	       (pds~constraint-pool-p cstr))
	   (format stream "~%<~A~% ~A~% ~A>" 
		   (meth~mapp-subst object)
		   (meth~mapp-mapp object)
		   cstr))
	  (T
	   (format stream "~%<~A~% ~A~% ~A>" 
		   (meth~mapp-subst object)
		   (meth~mapp-mapp object)
		   cstr))
	  )))
			      

(defmethod meth~mapping-create ((subst subst+substitution) (mapp mapp+mapping)
				&optional extension (constraint T))
  (declare (edited  "10-MAR-1998")
	   (authors Lassaad)
	   (input "A truth VALUE, and a SUBSTitution.")
	   (effect "None." )
	   (value "A new instance of METH=TRUTH-BINDING."))
  (make-instance 'meth+mapping
		 :subst subst
		 :mapp mapp
		 :extension extension
		 :constraint constraint))


(defgeneric meth~mapping-copy (mmapps)
  ;;; Copying mmapp-s and subst-s.
  ;; Note that for mmapp-s only the substitution and the mapping
  ;; are copied, because there is no need for copying the extension
  ;; and the constraint when calling this function in meth~match-p.
  (:method ((mmapps list))
	   (mapcar #'meth~mapping-copy mmapps))
  (:method ((subst subst+substitution))
 	   (meth~subst-create (subst~domain subst)
 			      (subst~codomain subst)))
  (:method ((mapp mapp+mapping))
 	   (mapp~create (mapp~domain mapp)
			(mapp~codomain mapp)))
  (:method ((mmapp meth+mapping))
	   (with-slots (subst mapp extension constraint) mmapp
		       (meth~mapping-create
			(meth~subst-create (subst~domain subst)
					   (subst~codomain subst))
			(mapp~create (mapp~domain mapp)
				     (mapp~codomain mapp))
			extension
			constraint)
		       ))
  )

(defun meth~subst-create (domain codomain)
  ;;;Without checking the class of the domain andd codomain elements:
  (let ((mapping (mapp~create domain codomain)))
;    (cond
;     ((notevery #'(lambda (item) (typep item 'data+variable)) domain)
;      (error "the domain of a substitution has to consist of variables!"))
;;     ((notevery #'(lambda (item) (typep item 'data+struct)) codomain)
;;      (error "the codomain of a substitution may contain only data structures!"))
;     (t
      (change-class mapping 'subst+substitution)
      ))

(defgeneric meth~subst-apply (subst object)
  (declare (edited  "22-APR-1998")
	   (authors Lassaad)
	   (input   "A substitution.")
	   (effect  )
	   (value   ))
  (:method ((subst subst+substitution) (nix null))
	   nil)
  (:method ((subst subst+substitution) (alist list))
	   (cons (meth~subst-apply subst (first alist))
		 (meth~subst-apply subst (rest alist))))
  (:method ((subst subst+substitution) (type type+type))
	   (subst~apply subst type))
  (:method ((subst subst+substitution) (term term+term))
	   (beta~normalize (subst~apply subst term)))
  (:method ((subst1 subst+substitution) (subst2 subst+substitution))
	   (subst~apply subst1 subst2))
  )


(defgeneric meth~mapp-get-component (meth-thing mmapp indicator)
  (declare (edited  "27-MAY-1998" "22-APR-1998")
	   (authors Jzimmer Lassaad)
	   (input   "A method variable, a method mapping, and an indicator.")
	   (effect  "None.")
	   (value   "The associated pds object for METH-THING in MMAPP."))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :subst)))
	   (subst~get-component meth-var (meth~mapp-subst mmapp)))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :mapp)))
	   (mapp~get-component meth-var (meth~mapp-mapp mmapp)))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :ext)))
	   (when (meth~mapp-extension mmapp)
	     (mapp~get-component meth-var (meth~mapp-extension mmapp))))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :both)))
	   (or (subst~get-component meth-var (meth~mapp-subst mmapp))
	       (mapp~get-component meth-var (meth~mapp-mapp mmapp))))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :all)))
	   (or (subst~get-component meth-var (meth~mapp-subst mmapp))
	       (mapp~get-component meth-var (meth~mapp-mapp mmapp))
	       (and (meth~mapp-extension mmapp)
		    (mapp~get-component meth-var (meth~mapp-extension mmapp)))))
  (:method ((sym symbol) (mmapp meth+mapping) (indicator (eql :mapp)))
	   (mapp~get-component sym (meth~mapp-mapp mmapp)))
  (:method ((meth-vars cons) (mmapp meth+mapping) (indicator (eql :subst)))
	   (let ((subst (meth~mapp-subst mmapp)))
	     (mapcar #'(lambda (meth-var)
			 (subst~get-component meth-var subst))
		     meth-vars)))
  (:method ((nix null) (mmapp meth+mapping) (indicator (eql :subst)))
	   )
  (:method ((meth-vars cons) (mmapp meth+mapping) (indicator (eql :mapp)))
	   (let ((mapp (meth~mapp-mapp mmapp)))
	     (mapcar #'(lambda (meth-var)
			 (mapp~get-component meth-var mapp))
		     meth-vars)))
  (:method ((nix null) (mmapp meth+mapping) (indicator (eql :mapp)))
	   )
  (:method ((meth-vars cons) (mmapp meth+mapping) (indicator (eql :ext)))
	   (let ((ext (meth~mapp-extension mmapp)))
	     (and ext
		  (mapcar #'(lambda (meth-var)
			      (mapp~get-component meth-var ext))
			  meth-vars))))
  (:method ((nix null) (mmapp meth+mapping) (indicator (eql :ext)))
	   )
  (:method ((meth-vars cons) (mmapp meth+mapping) (indicator (eql :both)))
	   (let ((mapp (meth~mapp-mapp mmapp))
		 (subst (meth~mapp-subst mmapp)))
	     (mapcar #'(lambda (meth-var)
			 (or (subst~get-component meth-var subst)
			     (mapp~get-component meth-var mapp)))
		     meth-vars)))
  (:method ((nix null) (mmapp meth+mapping) (indicator (eql :both)))
	   )
  (:method ((meth-vars cons) (mmapp meth+mapping) (indicator (eql :all)))
	   (let ((mapp (meth~mapp-mapp mmapp))
		 (subst (meth~mapp-subst mmapp))
		 (ext (meth~mapp-extension mmapp)))
	     (if ext
		 (mapcar #'(lambda (meth-var)
			     (or (subst~get-component meth-var subst)
				 (mapp~get-component meth-var mapp)
				 (mapp~get-component meth-var ext)))
			 meth-vars)
	       (mapcar #'(lambda (meth-var)
			     (or (subst~get-component meth-var subst)
				 (mapp~get-component meth-var mapp)))
		       meth-vars))))
  (:method ((nix null) (mmapp meth+mapping) (indicator (eql :all)))
	   )
  (:method (egal (mmapp meth+mapping) schnurz)
	   )
  )


(defgeneric meth~mapp-new-subst (mmapp new-subst)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, and a substitution.")
	   (effect  "Sets the substitution of MMAPP to NEW-SUBST.")
	   (value   "The changed MMAPP."))
  (:method ((mmapp meth+mapping) (new-subst subst+substitution))
	   (setf (meth~mapp-subst mmapp) new-subst)
	   mmapp)
  (:method ((mmapp meth+mapping) (new-substs list))
	   (when new-substs
	     (cons (meth~mapp-new-subst mmapp (first new-substs))
		   (when (rest new-substs)
		     (let ((mmapp-mapp (meth~mapp-mapp mmapp))
			   (mmapp-ext (meth~mapp-extension mmapp))
			   (mmapp-cstr (meth~mapp-constraint mmapp)))
		       (mapcar #'(lambda (subst)
				   (meth~mapping-create subst mmapp-mapp mmapp-ext mmapp-cstr))
			       (rest new-substs)))))))
  )

(defmethod meth~mapp-new-mapp ((mmapp meth+mapping) (new-mapp mapp+mapping))
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, and a substitution.")
	   (effect  "Sets the substitution of MMAPP to NEW-SUBST.")
	   (value   "The changed MMAPP."))
  (setf (meth~mapp-mapp mmapp) new-mapp)
  mmapp)

(defmethod meth~mapp-new-extension ((mmapp meth+mapping) new-ext)
  (declare (edited  "27-NOV-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, and a substitution.")
	   (effect  "Sets the substitution of MMAPP to NEW-SUBST.")
	   (value   "The changed MMAPP."))
  (setf (meth~mapp-extension mmapp) new-ext)
  mmapp)

(defmethod meth~mapp-new-constraint ((mmapp meth+mapping) new-cstr)
  (declare (edited  "27-NOV-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, and a substitution.")
	   (effect  "Sets the substitution of MMAPP to NEW-SUBST.")
	   (value   "The changed MMAPP."))
  (setf (meth~mapp-constraint mmapp) new-cstr)
  mmapp)

(defmethod meth~mapp-extend-subst ((mmapp meth+mapping) (meth-var term+syn-sorted-var) pds-obj)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, a method variable, and a pds object.")
	   (effect  "Extends the substitution of MMAPP with the binding of"
		    "METH-VAR to PDS-OBJ.")
	   (value   "The changed MMAPP."))
  (subst~insert-component! meth-var pds-obj (meth~mapp-subst mmapp))
  mmapp)

(defmethod meth~mapp-extend-mapp ((mmapp meth+mapping) (meth-var term+syn-sorted-var) pds-obj)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, a method variable, and a pds object.")
	   (effect  "Extends the substitution of MMAPP with the binding of"
		    "METH-VAR to PDS-OBJ.")
	   (value   "The changed MMAPP."))
  (mapp~insert-component! meth-var pds-obj (meth~mapp-mapp mmapp))
  mmapp)

(defgeneric meth~mapping-extend ((mmapp meth+mapping) (meth-var term+syn-sorted-var) pds-obj)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A METH=TRUTH-BINDING, a method variable, and a pds object.")
	   (effect  "Extends the substitution of MMAPP with the binding of"
		    "METH-VAR to PDS-OBJ.")
	   (value   "The changed MMAPP."))
  (:method ((mmapp meth+mapping) (meth-var term+syn-sorted-var) pds-obj)
	   (if (or (term~p pds-obj)
		   (type~p pds-obj)
		   (and (listp pds-obj) (every #'(lambda (elt) (or (term~p elt) (type~p elt))) pds-obj)))
	       (meth~mapp-extend-subst mmapp meth-var pds-obj)
	     (meth~mapp-extend-mapp mmapp meth-var pds-obj)))
  (:method ((mmapp meth+mapping) (nix1 null) (nix2 null))
	   mmapp)
  (:method ((mmapp meth+mapping) (meth-vars cons) (pds-objs cons))
	   (meth~mapping-extend (meth~mapping-extend mmapp (first meth-vars) (first pds-objs))
				(rest meth-vars) (rest pds-objs)))
  )

(defun meth~mapp-insert-extension (mmapp)
  (declare (edited  "27-NOV-1998")
	   (authors Lassaad)
	   (input   "A method mapping.")
	   (effect  "Inserts the extensions of MMAPP in the substitution and the mapping.")
	   (value   "The changed MMAPP."))
  (if (meth~mapp-extension mmapp)
      (meth~mapp-new-extension (meth~mapping-extend mmapp
						    (mapp~domain (meth~mapp-extension mmapp))
						    (mapp~codomain (meth~mapp-extension mmapp)))
			       NIL)
    mmapp))
	   
(defmethod meth=associated-node ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (method meth+method)
				 reasons pds default-hyps &optional later-nodes)
  (declare (edited  "10-APR-1998")
	   (authors Lassaad)
	   (input   "Method variable, method mapping, a method, where METH-VAR occurs as a label of"
		    "method node in the declarative content of METHOD, a list of REASONS to be inserted in"
		    "new created nodes, a PDS, and a list of default hypothesis from which new created open"
		    "nodes must depend, and optionally a pds node list, before which new nodes are inserted"
		    "in PDS.")
	   (effect  "Might creates new nodes and inserts them into PDS before LATER-NODES.")
	   (value   "The associated pds node to METH-VAR when it was created or can be created using"
		    "the bindings in MMAPP, otherwise NIL."))
;  (let ((pds-node (meth~mapp-get-component meth-var mmapp :mapp)))
;    (format t "hallo1: ~A~% late-nodes: ~A" pds-node later-nodes)
;    (if pds-node pds-node
;      (let ((pds-nodes (meth~create-node (meth~label2node method meth-var) mmapp :hyps T)))
  (let* ((pds-node (meth~mapp-get-component meth-var mmapp :mapp))
	 (pds-nodes ;;;(remove-if #'(lambda (node) (find node (prob~proof-steps pds)))
			       (cond ((pdsn~p pds-node) (list pds-node))
				     ((null pds-node) (meth~create-node (meth~label2node method meth-var) mmapp :hyps T))
				     (t pds-node))))
	;;; LC: PDS-NODES consists of hypothesis nodes, open nodes and justified nodes.
	;; All these nodes and their premises must be added to the PDS nodes after getting REASONS
	;;; This function must returns, whether one of the new nodes is open (oN). This is needed to change
	;; not only the hypothesis list of the open node (oN) but that of new nodes which depend on it too
	;; (<dN1 (M1 dN2 dN3)> <dN2 (M2 oN)>). 
	;; - Hypotheses are added first
	;; - Then the remaining nodes are considered next:
	;;   A) N is an open node: Then the hypothesis list of N must be extended on the hypotheses of the
	;;      method conclusion, and N must be inserted in a special PDS open nodes pds~hided-open-nodes, 
	;;      where this list contains open nodes of the PDS for which no agenda task exists.
	;;   B) N is justified by P1,..,Pn: then:
	;;     B1: remove each Pi which belongs to the PDS nodes.
	;;     B2: Insert the remaining Pi-s recursively
	;;     B3: Change the hypothesis list of N, when needed (step B2 returns (P1',..,Pm'))
	;;     B4: Insert B4 after P1,..,Pn and before LATER-NODES
	(if pds-nodes
	    (labels ((insert-nodes (nnodes lnodes pds default-hs reasons)
				   (when nnodes
				     (let ((node (first nnodes)))
				       (if (pdsn~open-node-p node)
					   (cond ((pds~label2node (keim~name node) pds)
						  (insert-nodes (rest nnodes) lnodes pds default-hs reasons))
						 (T ;;; node must be inserted:
						  (setf (pdsn~hyps node) (union (pdsn~hyps node) default-hs))
						  (dolist (reason reasons) (pdsn~insert-reason! node reason))
						  (when (null (find node (prob~proof-steps pds)))
						    (pds~insert-node-after-before! node NIL later-nodes pds))
						  (omega~error ";;; You have to implement pds~~hided-open-nodes")
						  ;;; Among others change the function pds~insert-node-after-before!
						  (insert-nodes (rest nnodes) lnodes pds default-hs reasons)
						  default-hs))
					 ;;; NODE is closed
					 (cond ((pds~label2node (keim~name node) pds)
						(insert-nodes (rest nnodes) lnodes pds default-hs reasons))
					       (T ;;; node must be inserted but after inserting its premises
						(let ((node-hyps (insert-nodes (pdsn~just-premises node)
									       lnodes pds default-hs reasons)))
						  (setf (pdsn~hyps node) (union (pdsn~hyps node) node-hyps))
						  (dolist (reason reasons) (pdsn~insert-reason! node reason))
						  (when (null (find node (prob~proof-steps pds)))
						    (pds~insert-node-after-before! node (pdsn~just-premises node) later-nodes pds))
						  (or (insert-nodes (rest nnodes) lnodes pds default-hs reasons)
						      node-hyps))))))))
		     )
	      (let ((hyp-nodes (remove-if-not #'pdsn~hypothesis-p pds-nodes)))
		(dolist (node hyp-nodes)
		  (dolist (reason reasons)
		    (pdsn~insert-reason! node reason))
		  (when (null (find node (prob~proof-steps pds)))
		    (pds~insert-node-after-before! node nil later-nodes pds)))
		(insert-nodes (meth=set-difference pds-nodes hyp-nodes) later-nodes pds default-hyps reasons)
		pds-nodes))
	  (omega~error ";;;~A does not allow to create a pds node for ~A!"
		       mmapp (meth~label2node method meth-var)))))
    

(defmethod meth=associated-node ((meth-node meth+node) (mmapp meth+mapping) method
				 reasons pds default-hyps &optional later-nodes)
  (declare (edited  "07-JUL-1999")
	   (authors Jzimmer)
	   (input   "Method node, method mapping,  where METH-NODE occurs as a label of"
		    "method node in the declarative content of METHOD, a list of REASONS to be inserted in"
		    "new created nodes, a PDS, and a list of default hypothesis from which new created open"
		    "nodes must depend, and optionally a pds node list, before which new nodes are inserted"
		    "in PDS.")
	   (effect  "Might creates new nodes and inserts them into PDS before LATER-NODES.")
	   (value   "The associated pds node to METH-NODE when it was created or can be created using"
		    "the bindings in MMAPP, otherwise NIL."))
  (labels ((insert-nodes (nnodes lnodes pds default-hs reasons)
			 (when nnodes
			   (let ((node (first nnodes)))
			     (if (pdsn~open-node-p node)
				 (cond ((pds~label2node (keim~name node) pds)
					(dolist (reason reasons) (pdsn~insert-reason! node reason))
					(insert-nodes (rest nnodes) lnodes pds default-hs reasons))
				       (T ;;; node must be inserted:
					(setf (pdsn~hyps node) (union (pdsn~hyps node) default-hs))
					(dolist (reason reasons) (pdsn~insert-reason! node reason))
					(when (null (find node (prob~proof-steps pds)))
					  (pds~insert-node-after-before! node NIL later-nodes pds))
					(omega~error ";;; You have to implement pds~~hided-open-nodes")
					;;; Among others change the function pds~insert-node-after-before!
					(insert-nodes (rest nnodes) lnodes pds default-hs reasons)
					default-hs))
			       ;;; NODE is closed
			       (cond ((pds~label2node (keim~name node) pds)
				      (dolist (reason reasons) (pdsn~insert-reason! node reason))
				      (insert-nodes (rest nnodes) lnodes pds default-hs reasons))
				     (T ;;; node must be inserted but after inserting its premises
				      (let ((node-hyps (insert-nodes (pdsn~just-premises node)
								     lnodes pds default-hs reasons)))
					(setf (pdsn~hyps node) (union (pdsn~hyps node) node-hyps))
					(dolist (reason reasons) (pdsn~insert-reason! node reason))
					(when (null (find node (prob~proof-steps pds)))
					  (pds~insert-node-after-before! node (pdsn~just-premises node) later-nodes pds))
					(or (insert-nodes (rest nnodes) lnodes pds default-hs reasons)
					    node-hyps))))))))
	   )
    (let ((pds-node (meth~mapp-get-component (keim~name meth-node) mmapp :mapp)))
      (if pds-node
	  (progn
	    (cond ((pdsn~hypothesis-p pds-node)
		   (dolist (reason reasons) (pdsn~insert-reason! pds-node reason))
		   (unless (pds~label2node (keim~name pds-node) pds)
		     (when (null (find node (prob~proof-steps pds)))
		       (pds~insert-node-after-before! pds-node nil later-nodes pds))))
		  (T
		   (insert-nodes (list pds-node) later-nodes pds default-hyps reasons)))
	    pds-node)
	(let ((pds-nodes (meth~create-node meth-node mmapp :hyps T)))
	  ;;; LC: PDS-NODES consists of hypothesis nodes, open nodes and justified nodes.
	  ;; All these nodes and their premises must be added to the PDS nodes after getting REASONS
	  ;;; This function must returns, whether one of the new nodes is open (oN). This is needed to change
	  ;; not only the hypothesis list of the open node (oN) but that of new nodes which depend on it too
	  ;; (<dN1 (M1 dN2 dN3)> <dN2 (M2 oN)>). 
	  ;; - Hypotheses are added first
	  ;; - Then the remaining nodes are considered next:
	  ;;   A) N is an open node: Then the hypothesis list of N must be extended on the hypotheses of the
	  ;;      method conclusion, and N must be inserted in a special PDS open nodes pds~hided-open-nodes,
	  ;;   B) N is justified by P1,..,Pn: then:
	  ;;     B1: remove each Pi which belongs to the PDS nodes.
	  ;;     B2: Insert the remaining Pi-s recursively
	  ;;     B3: Change the hypothesis list of N, when needed (step B2 returns (P1',..,Pm'))
	  ;;     B4: Insert B4 after P1,..,Pn and before LATER-NODES
	  (if pds-nodes
	      (let ((hyp-nodes (remove-if-not #'pdsn~hypothesis-p pds-nodes)))
		(dolist (node hyp-nodes)
		  (dolist (reason reasons) (pdsn~insert-reason! node reason))
		  (unless (pds~label2node (keim~name node) pds)
		    (when (null (find node (prob~proof-steps pds)))
		      (pds~insert-node-after-before! node nil later-nodes pds))))
		(insert-nodes (set-difference pds-nodes hyp-nodes) later-nodes pds default-hyps reasons)
		(first pds-nodes))
	    (omega~error ";;;~A does not allow to create a pds node for ~A!"
			 mmapp meth-node)))))
    ))

(defun meth~compute-parameters (params mmapp)
  (declare (edited  "15-FEB-1999")
	   (authors Lassaad)
	   (input   "Method parameters, and a method mapping.")
	   (effect  "None.")
	   (value   "The associated PDS objects for the elements of PARAMS."
		    "When MMAPP does not allow to compute the associated PDS"
		    "object for some element of PARAMS, then a NIL is returned"
		    "for this PDS object."))
  (mapcar #'(lambda (param) (meth~pds-object param mmapp nil))
	  params))
  


;;; The formal language of method conditions: A first order language with
;; quantifications on finite domains: We have
;; - connectors: MOR, MAND, ...
;; - predicates: SUBTERM, ...
;; - relational functions: ALPHA-MATCHER, BOUND, ...
;; - functions: TERMTYPE, ...
;;; Functions can occur in the method computations.
;;; How to evaluate a condition, where a condition is either atomic (predicate,    
;; or relational function), or compound (with a connector as function symbol):
;; - When condition is compound, then the evaluation of the arguments is delegated
;; to the associated function
;; - When condition is a relational function, then the last argument must be a method
;; variable, and the first remaining arguments are evaluated before calling the
;; associated function. When the last argument is bound, then its binding should be
;; consistent with the result of the function.
;; For instance, (alpha-matcher t1 t2 sub) is evaluated to either <T old-binding>
;; when sub is bound in old-binding and sub(t1) == t2, to
;; <T old-binding U {sub <- matcher(t1 t2)}> when t1 matches t2, or to <nil old-binding>.
;; For instance, (bound phi) is evaluated to either <T old-binding> when phi is
;; bound in old-binding.
;; - Otherwise, condition is a predicate, then the arguments are evaluated before
;; calling the associated function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LC: NEW: Extending Condition Checking to deal with constraints
;;; LC: Umbenennen von meth=eval-all zu meth=eval-arguments


#| New Version
(defun meth=check-condition (condition tmapp)
  (declare (edited  "10-MAR-1998")
	   (authors Lassaad)
	   (input   "A condition, and a truth-binding pair.")
	   (effect  "None.")
	   (value   "TMAPP extended by the evaluation of the given"
		    "CONDITION."))
  (let ((cond-func (keim~name condition))
	(cond-args (meth=condition-args condition)))
    (cond ((meth~connective-function-p cond-func)
	   ;;; connective functions evaluate their arguments. 
;	   (let ((result
	   (funcall (meth~sym2condition cond-func) cond-args tmapp))
;	     (when (not (meth~tm-value result))
;	       (setf ana*cause-of-failure
;		     (or ana*cause-of-failure (list cond-func cond-args tmapp))))
;	     result))
	  ((meth~quantifier-function-p cond-func)
	   (let* ((mvar (first cond-args))
		  (pds-obj (meth~pds-object mvar tmapp nil)))
	     ;;; Using the asociated binding for MVAR, if some binding is available
	     ;; in TMAPP, o/w using MVAR to evaluate the quantification function:
;	     (let ((result
	     (funcall (meth~sym2condition cond-func)
		      (list (if pds-obj pds-obj mvar)
			    (meth~pds-object (second cond-args) tmapp nil T)
			    (third cond-args))
		      tmapp)))
;	       (when (not (meth~tm-value result))
;		 (setf ana*cause-of-failure
;		       (or ana*cause-of-failure (list cond-func cond-args tmapp))))
;	       result)))
	  ((meth~relational-function-p cond-func)
	   ;;; The last argument of a relational function must be a
	   ;; method variable and the rest must be evaluable to pds objects.
;	   (let ((result
	   (funcall (meth~sym2condition cond-func)
		    (meth=eval-arguments cond-args tmapp T)
		    tmapp))
;	     (when (not (meth~tm-value result))
;	      (setf ana*cause-of-failure
;		    (or ana*cause-of-failure (list cond-func
;						   cond-args
;						   (meth=eval-all cond-args tmapp T)
;						   tmapp))))
;	     result))
	  (T
	   ;;; COND-FUNC denotes a function which expects pds objects as arguments
;	   (let ((result
	   (funcall (meth~sym2condition cond-func)
		    (meth=eval-arguments cond-args tmapp)
		    tmapp))
;	     (when (not (meth~tm-value result))
;	       (setf ana*cause-of-failure
;		     (or ana*cause-of-failure (list cond-func
;						    cond-args
;						    (meth=eval-all cond-args tmapp)
;						    tmapp))))
;	     result)))
	  
	  )))
|#

(defvar meth*cause-of-failure nil)
;; A variable to store the last cause of failure in  meth=check-condition
;; AMeier

(defun meth=check-condition (condition tmapp)
  (declare (edited  "10-MAR-1998")
	   (authors Lassaad)
	   (input   "A condition, and a truth-binding pair.")
	   (effect  "None.")
	   (value   "TMAPP extended by the evaluation of the given"
		    "CONDITION."))
  (let ((cond-func (keim~name condition))
	(cond-args (meth=condition-args condition)))
    (cond ((meth~connective-function-p cond-func)
	   ;;; connective functions evaluate their arguments. 
	   (let ((result (funcall (meth~sym2condition cond-func) cond-args tmapp)))
	     
	     (cond ((and (meth~mapping-p result) (null (meth~mapp-constraint result)))
		    (setf meth*cause-of-failure
			  (or meth*cause-of-failure (list cond-func cond-args tmapp)))
		    ;;(format t "~%CASE1: SET meth*cause-of-failure: ~A" meth*cause-of-failure)
		    )
		   (t
		    (setf meth*cause-of-failure nil)
		    ;;(format t "~%CASE1: RESET meth*cause-of-failure")
		    ))

	     result))
	  
	  ((meth~quantifier-function-p cond-func)
	   (let* ((mvar (first cond-args))
		  (pds-obj (meth~pds-object mvar tmapp nil)))
	     ;;; Using the asociated binding for MVAR, if some binding is available
	     ;; in TMAPP, o/w using MVAR to evaluate the quantification function:
	     (let ((result (funcall (meth~sym2condition cond-func)
				    (list (if pds-obj pds-obj mvar)
					  (meth~pds-object (second cond-args) tmapp nil T)
					  (third cond-args))
				    tmapp)))
	       
	       (cond ((and (meth~mapping-p result) (null (meth~mapp-constraint result)))
		      (setf meth*cause-of-failure
			    (or meth*cause-of-failure (list cond-func cond-args tmapp)))
		      ;;(format t "~%CASE2: SET meth*cause-of-failure: ~A" meth*cause-of-failure)
		      )
		     (t
		      (setf meth*cause-of-failure nil)
		      ;;(format t "~%CASE2: RESET meth*cause-of-failure")
		      ))
	       
	       result)))
	  
	  ((meth~relational-function-p cond-func)
	   ;;; The last argument of a relational function must be a
	   ;; method variable and the rest must be evaluable to pds objects.
	   (let ((result
		  (funcall (meth~sym2condition cond-func)
			   (meth=eval-arguments cond-args tmapp T)
			   tmapp)))
	     
	     (cond ((and (meth~mapping-p result) (null (meth~mapp-constraint result)))
		    (setf meth*cause-of-failure
			  (or meth*cause-of-failure (list cond-func cond-args tmapp)))
		    ;;(format t "~%CASE3: SET meth*cause-of-failure: ~A" meth*cause-of-failure)
		    )
		   (t
		    (setf meth*cause-of-failure nil)
		    ;;(format t "~%CASE3: RESET meth*cause-of-failure")
		    ))
	     
	     result))
	  
	  (T
	   ;;; COND-FUNC denotes a function which expects pds objects as arguments
	   (let ((result (funcall (meth~sym2condition cond-func)
				  (meth=eval-arguments cond-args tmapp)
				  tmapp)))
	     
	     (cond ((and (meth~mapping-p result) (null (meth~mapp-constraint result)))
		    (setf meth*cause-of-failure
			  (or meth*cause-of-failure (list cond-func cond-args tmapp)))
		    ;;(format t "~%CASE2: SET meth*cause-of-failure: ~A" meth*cause-of-failure)
		    )
		   (t
		    (setf meth*cause-of-failure nil)
		    ;;(format t "~%CASE3: RESET meth*cause-of-failure")
		    ))
	     
	     result))    
	  )))


(defgeneric meth~check-condition (condition mmapp &optional pds)
  (declare (edited  "10-MAR-1998")
	   (authors Lassaad)
	   (input   "A method condition, and a method mapping or a non-empty"
		    "list of method mappings.")
	   (effect  "None.")
	   (value   "- When MMAPP is a substitution and can be used to successfully"
		    "evaluate CONDITION, then MMAPP with possibly some extensions."
		    "- When MMAPP is a substitution list, then the extended elements"
		    "of MMAPP which enable a positive evaluation of CONDITION."
		    "- Otherwise NIL."))
  (:method ((condition meth+condition) (mmapp meth+mapping) &optional (pds omega*current-proof-plan))
	   (let ((check-res (meth=check-condition condition mmapp)))
	     (if (meth~mapping-p check-res)
		 (when (meth~mapp-constraint check-res)
		   (cond ((cstr~constraint-p (meth~mapp-constraint check-res))
			  (let ((cstr-pool (cstr~merge (meth~mapp-constraint check-res)
						       (pds~constraint-pool pds))))
			    ;;;CSTR~MERGE merges C1 with the constraints of the pds constraint pool
			    ;; and returns a constraint pool with resulted bindings, resulted remaining   
			    ;; constraints, and previous constraint pool (the given one), if it is
			    ;; possible. O/w it returns NIL.
			    (when cstr-pool
			      (meth~mapp-new-constraint (meth~mapp-insert-extension check-res)
							cstr-pool))))
			 (T ;;;No new constraints result from the evaluation of the condition:
			  (meth~mapp-insert-extension check-res))))
	       ;;; A list of alternatives:
	       (let (result)
		 (dolist (res check-res)
		   (cond ((cstr~constraint-p (meth~mapp-constraint res))
			  (let ((cstr-pool (cstr~merge (meth~mapp-constraint res)
						       (pds~constraint-pool pds))))
			    (when cstr-pool
			      (setq result
				    (append result
					    (list (meth~mapp-new-constraint
						   (meth~mapp-insert-extension
						    (meth~mapp-new-subst
						     (meth~mapp-new-mapp res
									 (meth~mapping-copy (meth~mapp-mapp mmapp)))
						     (meth~mapping-copy (meth~mapp-subst mmapp))))
						   cstr-pool)))))))
			 ((pds~constraint-pool-p (meth~mapp-constraint res))
			  ;;; The result of BINDABLE
			  (setq result (append result (list res))))
			 (T ;;;No new constraints result from the evaluation of the condition:
			  (setq result
				(append result
					(list (meth~mapp-insert-extension
					       (meth~mapp-new-subst
						(meth~mapp-new-mapp res
								    (meth~mapping-copy (meth~mapp-mapp mmapp)))
						(meth~mapping-copy (meth~mapp-subst mmapp))))))))))
		 (if (rest result) result (first result)))
	       )))
  (:method ((condition meth+condition) (mmapps T) &optional (pds omega*current-proof-plan))
	   (let ((mmapps1 (meth~check-condition condition (first mmapps) pds)))
	     (if mmapps1
		 (if (meth~mapping-p mmapps1) 
		     (cons mmapps1
			   (when (rest mmapps)
			     (meth~check-condition condition (rest mmapps) pds)))
		   (append mmapps1
			   (when (rest mmapps)
			     (meth~check-condition condition (rest mmapps) pds))))
	       (when (rest mmapps)
		 (meth~check-condition condition (rest mmapps) pds)))))
  (:method ((condition null) (mmapp meth+mapping) &optional pds)
	   (declare (ignore pds))
	   mmapp)
  (:method ((condition null) (mmapps T) &optional pds)
	   (declare (ignore))
	   mmapps)
  )

(defun meth~carry-out-computations (meth-computations mmapp)
  (declare (edited  "01-APR-1998")
	   (authors Lassaad)
	   (input   "..")
	   (effect  "Extends MMAPP by binding the names of METH-COMPUTATIONS"
		    "with the values resulted from evaluating the associated"
		    "funcalls.")
	   (value   "Unspecified."))
  (dolist (computation meth-computations)
    (let ((meth-funcall (meth=computation-funcall computation)))
      (cond ((meth~funcall-p meth-funcall)
	     (let ((func-name (keim~name meth-funcall))
		   (func-args (meth=funcall-args meth-funcall)))
	       (if (string-equal func-name 'if)
		   (let ((result (meth~check-condition (first func-args) mmapp)))
		     (meth~mapping-extend mmapp (keim~name computation)
					  (if result
					      (car (meth=eval-arguments (list (second func-args)) mmapp))
					    (car (meth=eval-arguments (list (third func-args)) mmapp)))))
		 (meth~mapping-extend mmapp (keim~name computation)
				      (apply (meth~sym2function func-name)
					     (meth=eval-arguments func-args mmapp))))))
	    ((meth~key-expression-p meth-funcall)
	     (meth~mapping-extend mmapp (keim~name computation)
				  (meth~pds-object meth-funcall mmapp nil)))
	    (t (omega~error "unknown computational object...."))))))

(defgeneric meth=bind-computations (var result mmapp)
  (:method ((var cons) (result cons) (mmapp meth+mapping))
	   (mapc #'(lambda (x y) (meth=bind-computations x y mmapp))
		 var result))
  (:method ((var cons) result (mmapp meth+mapping))
	   (meth~mapping-extend mmapp (car var) result))
  (:method (var result (mmapp meth+mapping))
	   (meth~mapping-extend mmapp var result))
  (:method (var (result null) (mmapp meth+mapping))
	   (declare (ignore var)))
  (:method ((var null) result (mmapp meth+mapping))
	   (declare (ignore result))))


(defmethod meth~execute-action ((method meth+method) (action-pat meth+action-pattern)
				(mmapp meth+mapping) (pds pds+proof-plan) reasons default-hyps)
  (declare (edited  "10-APR-1998")
	   (authors Lassaad)
	   (input   "A method, one of its action patterns, a method mapping, a PDS,"
		    "a list of reasons to be inserted into new created nodes, and a"
		    "list of default hypotheses to be used for creating new open nodes.")
	   (effect  "Carry out the actions of ACTION-PAT by updating the supports of the"
		    "involved nodes.")
	   (value   "Unspecified."))
  (let* ((aps-nodes (meth=action-pattern-supported-nodes action-pat))
	 (supp-nodes (meth=flat-list
		      (cond ((listp aps-nodes)
			     (mapcar #'(lambda (n)
					 (meth=associated-node n mmapp method reasons pds default-hyps))
				     aps-nodes))
			    ((string-equal aps-nodes :all-opens) (pds~open-nodes pds)))))
	 (supported-nodes (if (every #'consp supp-nodes) (apply #'append supp-nodes) supp-nodes)))
    (if supported-nodes
	(let ((the-actions (meth=action-pattern-actions action-pat)))
	  (dolist (action the-actions)
	    (let ((supp-nodes (mapcar #'(lambda (n)
					  (meth=associated-node n mmapp method reasons pds default-hyps supported-nodes))
				      (meth=action-support-nodes action)))
		  (action-func (meth~sym2action (keim~name action))))
	      (dolist (supported-node supported-nodes)
		(funcall action-func supported-node supp-nodes pds)))))
      (omega~error ";;; Action pattern ~A without supported nodes!" action-pat))
    ))



(defun meth~execute-expansion-function (exp-func mapp subst env)
  (declare (edited  "28-MAY-1998")
	   (authors Jzimmer)
	   (input "An expansion-function, a mapping and a substitution.")
	   (effect "The expansion-function is executed.")
	   (value "The result of applying the expansion-function." ))
  
  (let* ((symbol-list (mapcar #'keim~name
                              (append (subst~domain mapp)
				      (subst~domain subst))))
         (value-list (append (subst~codomain mapp)
			     (subst~codomain subst)))
	 (sym-val-list (mapcar #'cons symbol-list value-list))
	 (new-sym-val-list
	  (mapcar #'(lambda (pair)
		      (let* ((sym (first pair))
			     (term (cdr pair))
			     (new-term
			      (if term
				  term
				(env~lookup-object sym env))))
			(cons sym new-term)))
		  sym-val-list))
	 (value-list (mapcar #'cdr new-sym-val-list)))
    (omega~trace "exp-func: ~S" exp-func)
    (when (fboundp (first exp-func))
      (progv symbol-list value-list
	(eval exp-func)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LC Delete this function
;(defun meth=eval-butlast (args tmapp)
;  (declare (edited  "11-MAR-1998")
;           (authors Lassaad)
;           (input   "A non-empty list, and a method truth mapping.")
;           (effect  "Signals a method error, when either the last element of"
;                    "ARGS is a method function call or a method condition, or"
;                    "the remaining first elements cannot be evaluated using"
;                    "SUBST.")
;           (value   "When successful, the last element in ARGS and the evaluation"
;                    "result of the remaining elements."))
;  (let ((argn (first (last args))))
;    (if (or (meth~funcall-p argn) (meth~condition-p argn))
;        (omega~error "~A is not a method variable.~%" argn)
;      (append (meth=eval-all (butlast args) tmapp) (list argn)))
;    ))


;(defun meth=eval-all (args mmapp &optional unboundp)
;  (declare (edited  "11-MAR-1998")
;           (authors Lassaad)
;           (input   "A list, and a method mapping.")
;           (effect  "Signals a method error, when the elements of ARGS"
;                    "cannot be evaluated using SUBST.")
;           (value   "When successful, the evaluation of the elements of ARGS."))
;  (when args
;    (let ((the-arg1 (meth~pds-object (first args) mmapp nil)))
;      (if (or the-arg1 (meth~funcall-p (first args)))
;          (cons the-arg1 (meth=eval-all (rest args) mmapp unboundp))
;        (if (and unboundp (ssterm~var-p (first args)))
;            (cons (first args) (meth=eval-all (rest args) mmapp unboundp))
;          (omega~error "~A does not correspond to a pds object.~%" (first args)))))
;    ))
;;; LC: reimplemented
(defun meth=eval-arguments (args mmapp &optional unboundp)
  (declare (edited  "05-NOV-1998" "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of method objects, a method mapping, and a key word"
		    "UNBOUNDP stating whether some elements of ARGS are allowed to"
		    "be method variables without associated pds object in MMAPP.")
	   (effect  "Signals a method error, when UNBOUNDP is set to NIL and MMAPP"
		    "cannot be used or extended to evaluate each element of ARGS."
		    "Possibly extends MMAPP.")
	   (value   "When successful, the evaluation of the elements of ARGS."))
  ;;; For each argument in ARGS try to determine the associated PDS object. When 
  ;; this is not possible, then: if UNBOUNDP is set and the element of ARGS is a
  ;; method variable, then it is allowed to return the element self; otherwise
  ;; MMAPP may be extended by creating new PDS objects for unbound method variables
  ;; with the sorts CONST and METAVAR, when the type of these method variables
  ;; corresponds wrt. the substitution of MMAPP to a variable free type. 
  (when args
    (let ((arg1 (first args)))
      (cond ((ssterm~var-p arg1)
	     (if unboundp
		 ;;; Dont extend MMAPP:
		 (let ((the-arg1 (meth~pds-object arg1 mmapp nil)))
		   (if the-arg1
		       (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))
		     (cons arg1 (meth=eval-arguments (rest args) mmapp unboundp))))
	       ;;; Possibly extend MMAPP: 
	       (let ((the-arg1 (meth~pds-object arg1 mmapp nil T)))
		 (if the-arg1
		     (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))
		   (omega~error "~A does not correspond to a pds object.~%" arg1)))))
	    (T ;;; Possibly extend MMAPP
	     (let ((the-arg1 (meth~pds-object arg1 mmapp nil T)))
	       (cons the-arg1 (meth=eval-arguments (rest args) mmapp unboundp))))))
    ))

(defun meth=compute-all (args mmapp)
  (declare (edited  "04-FEB-1999" "11-MAR-1998")
	   (authors Sorge Lassaad)
	   (input   "A list, and a method mapping.")
	   (effect  "Signals a method error, when the elements of ARGS"
		    "cannot be evaluated using SUBST.")
	   (value   "When successful, the evaluation of the elements of ARGS."))
  (when args
    (let ((arg1 (first args)))
      (if (ssterm~var-p arg1)
	  (cond ((find arg1 (append (subst~domain (meth~mapp-subst mmapp))
				    (mapp~domain (meth~mapp-mapp mmapp))))
		 (cons (meth~mapp-get-component arg1 mmapp :both)
		       (meth=compute-all (rest args) mmapp)))
		(T
		 (let ((the-arg1 (or (meth~pds-object (first args) mmapp nil)
				     (meth~object-instance (first args) mmapp t))))
		   (when the-arg1
		     (cons the-arg1 (meth=compute-all (rest args) mmapp))))))
	(let ((the-arg1 (or (meth~pds-object (first args) mmapp nil)
			    (meth~object-instance (first args) mmapp nil))))
	  (when the-arg1
	    (cons the-arg1 (meth=compute-all (rest args) mmapp))))))))
	
		 
	
;;      (if (or the-arg1 (meth~funcall-p (first args)))
;;          (cons the-arg1 (meth=compute-all (rest args) mmapp))
;;        (if (ssterm~var-p (first args))
;;            (cons (first args) (meth=compute-all (rest args) mmapp))
;;          (omega~error "~A does not correspond to an instantiated object.~%" (first args)))))
;;    ))

(defun meth=sort2argtype (sort)
  (rest (assoc sort meth*sort2argtype)))

(defun meth~read-parameters (method pds-objects &optional (meth-params (meth~parameters method)))
  (declare (edited  "21-APR-1998")
	   (authors Lassaad)
	   (input   "A method, a list of (representation of) pds objects to be read as parameter"
		    "for METHOD, and optionally the list of METHOD parameters.")
	   (effect  "None.")
	   (value   "The list of associated pds objects, when this objects are consistent with"
		    "parameter sorts, otherwise NIL."))
  (let ((parameters))
    (do ((mparams meth-params (rest mparams))
	 (pdsobjs pds-objects (rest pdsobjs)))
	((or (null mparams) (null pdsobjs))
	 (if (or mparams pdsobjs)
	     (arg~signal-wrong-type 'method (cons (keim~name method) pds-objects))
	   (cons method (reverse parameters))))
      (let* ((mparam (first mparams))
	     (pdsobj (first pdsobjs))
	     (argtype (meth=sort2argtype (ssterm~sort mparam))))
	(if argtype
	    (sys~handler-case
	     (setq parameters
		   (cons (arg~read-type argtype pdsobj) parameters))
	     (error () (arg~signal-wrong-type (keim~name argtype) pdsobj)))
	  (or (omega~error ";;;No argument type associated to the sort ~A."
			   (keim~name (ssterm~sort mparam)))
	      (arg~signal-wrong-type 'method (cons (keim~name method) pds-objects))))))
    ))

(defun meth~method-p (object)
  (declare (edited  "21-APR-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T, iff object is a method or it is a list, its"
		    "first element is a method and its rest elements"
		    "can be parameters for this method."))
  (or (meth~p object)
      (and (consp object)
	   (meth~p (first object))
	   (sys~handler-case
	    (meth~read-parameters (first object) (rest object))
	    (arg+wrong-type-error)))
      ))


(defgeneric meth~pds-object (meth-obj mmapp indicator &optional extend-subst bvar-mapp)
  (declare (edited  "06-NOV-1998" "11-MAR-1998")
	   (authors Lassaad)
	   (input   "A method object, a method mapping, an indicator stating when given to which the"
		    "method object corresponds, and optionally a key EXTENDP which states whether it"
		    "is allowed to extend MMAPP by bindings to default objects (new constants and new"
		    "meta-variables), and an additional mapping BVAR-MAPP which stores a mapping of"
		    "symbols belonging to an abstraction domain to variables (this mapping is needed"
		    "to determine the range of this abstraction).")
	   (effect  "Possibly extends MMAPP when EXTENDP is set.")
	   (value   "The correponding pds object to METH-OBJ."))
  (:method ((key-expr meth+key-expression) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator bvar-mapp))
	   (let ((pds-obj (meth~pds-object (meth=key-expression-args key-expr) mmapp (keim~name key-expr) extendp)))
	     (if (term~p pds-obj)
		 (beta~normalize pds-obj)
	       pds-obj)))
  (:method ((alist cons) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (let ((elt1 (first alist)))
	     (if (and (symbolp elt1) (string-equal elt1 'lam))
		 (let ((matrix (butlast (rest alist))))
		   (multiple-value-bind (bvars new-bvar-mapp)
		       (meth~pds-object matrix mmapp :lam-domain extendp bvar-mapp)
		     (when bvars
		       (let ((scope (meth~pds-object (first (last alist)) mmapp :term extendp new-bvar-mapp)))
			 (when scope
			   (term~abstr-create (meth=flat-list bvars) scope))))))
	       (let ((head (meth~pds-object elt1 mmapp :term extendp bvar-mapp)))
		 (when head
		   (let ((args (meth~pds-object (rest alist) mmapp :appl-args extendp bvar-mapp)))
		     (when args
		       (term~appl-create head (meth=flat-list args)))))))))
  (:method ((bvars cons) (mmapp meth+mapping) (indicator (eql :lam-domain)) &optional extendp bvar-mapp)
	   (let ((bvar1 (first bvars))
		 (the-bvar-mapp (if bvar-mapp bvar-mapp (mapp~create nil nil))))
	     (if (consp bvar1)
		 (let ((bvar1-name (first bvar1))
		       (bvar1-type (data~replace-free-variables (second bvar1)
								(remove-if-not #'type~p (subst~domain (meth~mapp-subst mmapp)))
								(remove-if-not #'type~p (subst~codomain (meth~mapp-subst mmapp))))))
		   (when (meth~mapp-extension mmapp)
		     (setq bvar1-type
			   (data~replace-free-variables bvar1-type
							(remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp)))
							(remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))))
		   (let* ((the-bvar1 (term~variable-create bvar1-name bvar1-type))
			  (new-bvar-mapp1 (mapp~insert-component bvar1-name the-bvar1 the-bvar-mapp)))
		     (if (rest bvars)
			 (multiple-value-bind (rest-bvars new-bvar-mapp2)
			     (meth~pds-object (rest bvars) mmapp :lam-domain extendp new-bvar-mapp1)
			   (when rest-bvars
			     (values (cons the-bvar1 rest-bvars) new-bvar-mapp2)))
		       (values (list the-bvar1) new-bvar-mapp1))))
	       (if (ssterm~var-p bvar1)
		   (let ((assoc-var (meth~pds-object bvar1 mmapp :term extendp)))
		     (when assoc-var
		       (if (rest bvars)
			   (multiple-value-bind (rest-bvars new-bvar-mapp)
			       (meth~pds-object (rest bvars) mmapp :lam-domain extendp the-bvar-mapp)
			     (when rest-bvars
			       (values (cons assoc-var rest-bvars) new-bvar-mapp)))
			 (values (list assoc-var) the-bvar-mapp))))
		 (omega~error ";;; Illegal bound variable ~A, an intance of ~A"
			      bvar1 (type-of bvar1))))))
  (:method ((args cons) (mmapp meth+mapping) (indicator (eql :appl-args)) &optional extendp bvar-mapp)
	   (let ((arg1 (meth~pds-object (first args) mmapp :term extendp bvar-mapp)))
	     (when arg1
	       (if (rest args)
		   (let ((rest-args (meth~pds-object (rest args) mmapp :appl-args extendp bvar-mapp)))
		     (when rest-args (cons arg1 rest-args)))
		 (list arg1)))))
  (:method ((schema term+schema) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   ;;LC(meth~pds-object (data~schema-range schema) mmapp indicator extendp bvar-mapp))
	   (meth~pds-object (data~schema-range (data~copy schema :downto nil)) mmapp indicator extendp bvar-mapp))
  (:method ((const term+constant) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   (meth~object-instance const mmapp))
  (:method ((var term+variable) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   var)
  (:method ((meth-obj symbol) (mmapp meth+mapping) (indicator (eql :symbol)) &optional extendp bvar-mapp)
	   (declare (ignore bvar-mapp))
	   meth-obj)
  (:method ((meth-obj symbol) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (when (mapp~p bvar-mapp)
	     (mapp~get-component meth-obj bvar-mapp)))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (declare (ignore bvar-mapp))
	   (let ((pds-obj (or (meth~mapp-get-component meth-var mmapp :subst)
			      (meth~mapp-get-component meth-var mmapp :ext))))
	     (if pds-obj pds-obj
	       (when extendp
		 (let ((sort (ssterm~sort meth-var))
		       (subst (meth~mapp-subst mmapp))
		       (ext (meth~mapp-extension mmapp)))
		   (cond ((eq sort (ssterm~get-ssort :const))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((const (term~generate-term-primitive-with-new-name
					      (keim~name meth-var)
					      (data~replace-free-variables type ext-type-vars
									   (remove-if-not #'type~p (mapp~codomain ext)))
					      'term+constant
					      (pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var const ext)
				  const)
			      (let ((const (term~generate-term-primitive-with-new-name
					    (keim~name meth-var) type 'term+constant
					    (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var const)
				const))))
			 ((eq sort (ssterm~get-ssort :metavar))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((metavar (term~generate-term-primitive-with-new-name
						(keim~name meth-var)
						(data~replace-free-variables type ext-type-vars
									     (remove-if-not #'type~p (mapp~codomain ext)))
						'meta+variable
						(pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var metavar ext)
				  metavar)
			      (let ((metavar (term~generate-term-primitive-with-new-name
					      (keim~name meth-var) type 'meta+variable
					      (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var metavar)
				metavar))))
			 ))))))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator bvar-mapp))
	   (let ((pds-obj (meth~mapp-get-component meth-var mmapp :all)))
	     (if pds-obj pds-obj
	       (when extendp
		 (let ((sort (ssterm~sort meth-var))
		       (subst (meth~mapp-subst mmapp))
		       (ext (meth~mapp-extension mmapp)))
		   (cond ((eq sort (ssterm~get-ssort :const))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((const (term~generate-term-primitive-with-new-name
					      (keim~name meth-var)
					      (data~replace-free-variables type ext-type-vars
									   (remove-if-not #'type~p (mapp~codomain ext)))
					      'term+constant
					      (pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var const ext)
				  const)
			      (let ((const (term~generate-term-primitive-with-new-name
					    (keim~name meth-var) type 'term+constant
					    (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var const)
				const))))
			 ((eq sort (ssterm~get-ssort :metavar))
			  (let* ((type-subst (subst~create (remove-if-not #'type~p (subst~domain subst))
							   (remove-if-not #'type~p (subst~codomain subst))))
				 (type (meth~subst-apply type-subst (term~type meth-var)))
				 (type-free-vars (type~free-variables type))
				 (ext-type-vars (when ext
						  (remove-if-not #'type~p (mapp~domain ext)))))
			    (if (intersection type-free-vars ext-type-vars)
				(let ((metavar (term~generate-term-primitive-with-new-name
						(keim~name meth-var)
						(data~replace-free-variables type ext-type-vars
									     (remove-if-not #'type~p (mapp~codomain ext)))
						'meta+variable
						(pds~environment omega*current-proof-plan))))
				  (mapp~insert-component! meth-var metavar ext)
				  metavar)
			      (let ((metavar (term~generate-term-primitive-with-new-name
					      (keim~name meth-var) type 'meta+variable
					      (pds~environment omega*current-proof-plan))))
				(meth~mapp-extend-subst mmapp meth-var metavar)
				metavar))))
			 ))))))
  (:method ((meth-funcall meth+funcall) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   (let ((funcall-args (meth=funcall-args meth-funcall)))
	     (if funcall-args
		 (apply (meth~sym2function (keim~name meth-funcall))
			(meth=eval-arguments funcall-args mmapp))
	       (funcall (meth~sym2function (keim~name meth-funcall)) nil))))
  (:method ((meth-obj string) (mmapp meth+mapping) indicator &optional extendp bvar-mapp)
	   (declare (ignore indicator extendp bvar-mapp))
	   meth-obj)
  (:method (meth-obj (mmapp meth+mapping) (indicator t) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp meth-obj))
	   (omega~error ";;; meth~~pds-object must be implemented for key ~A!"
			indicator)
	   (error "IMPLEMENT IT."))
  (:method (meth-obj (mmapp meth+mapping) (indicator null) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (meth~object-instance meth-obj mmapp))
  (:method ((meth-obj type+type) (mmapp meth+mapping) (indicator (eql :type)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (meth~object-instance meth-obj mmapp))
  (:method ((meth-obj pos+position) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   meth-obj)
  (:method ((meth-obj list) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (if (null meth-obj)
	       (pos~empty)
	     (let ((first-pos (meth~pds-object (car meth-obj) mmapp indicator extendp bvar-mapp)))
	       (when first-pos
		 (pos~concatenate first-pos
				  (meth~pds-object (cdr meth-obj) mmapp indicator extendp bvar-mapp))))))
  (:method ((meth-obj integer) (mmapp meth+mapping) (indicator (eql :position)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (pos~list-position (list meth-obj)))
  (:method ((sym number) (mmapp meth+mapping) (indicator (eql :term)) &optional extendp bvar-mapp)
	   (declare (ignore extendp bvar-mapp))
	   (term~constant-create sym (env~lookup-object :num (pds~environment omega*current-proof-plan))))
  )

(defun meth~sym2function (symbol)
  (gethash (symbol-name symbol) meth*sym2function))

(defun meth~sym2action (symbol)
  (let ((assoc-func (assoc symbol meth*sym2action :test #'string-equal)))
    (when assoc-func (rest assoc-func))))

(defmacro meth~deffun (name args comment body)
  (declare (edited  "11-MAR-1998")
	   (authors Lassaad)
	   (input "A symbol list specifying a function.")
	   (effect "The function is defined in the current package as a new"
		   "method function. The first part of the function name is"
		   "identical for all method functions, i.e., special functions"
		   "used while interpreting a method. The first part of the name"
		   "(METH=DEFN-) is a convension for getting related names of"
		   "method functions and preventing using functions defined in"
		   "other modules. Moreover, method functions are represented"
		   "in the method data structure for simpilicity by their short"
		   "names, without (METH=DEFN-). The hash-table METH*SYM2FUNCTION"
		   "consists of a mapping from a method function short name to"
		   "the method function itself.")
	   (value "Unspecified."  )
	   ;(ignore (comment))
	   )
  (let ((new-name (read-from-string (concatenate 'string "meth=defn-"
						 (format nil "~A" name)))))
    `(progn
       (defun ,new-name ,args ,body)
       (let ((old-function (meth~sym2function ',name)))
	 (when old-function
	   (omega~warn "Redefining method function ~A.~%" ',name)
	   (remhash ',name meth*help4condfunc)
	   (remhash (symbol-name ',name) meth*sym2function))
	 (setf (gethash ',name meth*help4condfunc)
	       (cons "Function" ',comment))
	 (setf (gethash (symbol-name ',name) meth*sym2function)
	       (function ,new-name))))
    ))

(defun meth~sym2condition (symbol)
  (gethash (symbol-name symbol) meth*sym2condition))

(defmacro meth~defcond (name args comment body)
  (declare (edited  "03-APR-1998" "11-MAR-1998")
	   (authors Sorge Lassaad)
	   (input "A symbol list specifying a condition.")
	   (effect "The condition is defined in the current package as a new"
		   "method condition. The first part of the condition name is"
		   "identical for all method conditions, i.e., special conditions"
		   "used while interpreting a method. The first part of the name"
		   "(METH=DEFCOND-) is a convension for getting related names of"
		   "method conditions and preventing using conditions defined in"
		   "other modules. Moreover, method conditions are represented"
		   "in the method data structure for simpilicity by their short"
		   "names, without (METH=DEFCOND-). The hash-table METH*SYM2CONDITION"
		   "consists of a mapping from a method condition short name to"
		   "the method condition itself.")
	   (value "Unspecified."  )
	   ;(ignore (comment))
	   )
  (let ((new-name (read-from-string (concatenate 'string "meth=defcond-"
						 (format nil "~A" name)))))
    `(progn
       (defun ,new-name ,args ,body)
       (let ((old-function (meth~sym2condition ',name)))
	 (when old-function
	   (omega~warn "Redefining method condition ~A.~%" ',name)
	   (remhash ',name meth*help4condfunc)
	   (remhash (symbol-name ',name) meth*sym2condition))
	 (setf (gethash ',name meth*help4condfunc)
	       (cons "Condition" ',comment))
	 (setf (gethash (symbol-name ',name) meth*sym2condition)
	       (function ,new-name))))
    ))


(defun meth~complete-outlines (meth-concs meth-prems mmapp &optional pds-nodes hyps)
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "Two lists of method nodes (the method conclusions, and the method"
		    "premises), a method mapping containing bindings of method objects,"
		    "and optionally a list of pds nodes, and a hypothesis list. PDS-NODES"
		    "is a collector for the new nodes created while evaluating this function."
		    "HYPS is also a collector for the hypotheses of the new nodes.")
	   (effect  "Extends MMAPP with new bindings when new nodes are created.")
	   (value   "A triple consisting of three lists of pds nodes: conclusions, premises,"
		    "and new created nodes while evaluating this function."))

  (if meth-concs
      (let* ((meth-conc (first meth-concs))
	     )
	(if (meth~meta-node-p meth-conc)
	    (let* ((metavar (meth~meta-node-metavar meth-conc))
		   (aux-concs (meth~mapp-get-component metavar mmapp :both))
		   (concs (if (listp aux-concs) aux-concs (list aux-concs)))
		   )
	      (multiple-value-bind (pds-concs pds-prems new-nodes)
		  (meth~complete-outlines (rest meth-concs)
					  meth-prems
					  mmapp
					  (union concs pds-nodes)
					  hyps)
		(values (union concs
			       pds-concs)
			pds-prems
			new-nodes)))
	  (let ((pds-conc (meth~mapp-get-component (keim~name meth-conc) mmapp :mapp)))
	    (if pds-conc
	    ;;; PDS-CONC is existent
		(if hyps
		;;; A hypothesis list HYPS is specified to be used for new created
		    ;; nodes. The hyps of PDS-CONC must include the elements of HYPS. 
		    (if (subsetp hyps (pdsn~hyps pds-conc))
			(multiple-value-bind (pds-concs pds-prems new-nodes)
			    (meth~complete-outlines (rest meth-concs) meth-prems mmapp pds-nodes hyps)
			  (values (cons pds-conc
					pds-concs)
				  pds-prems
				  new-nodes))
		      (omega~error "Inconsistent hypothesis list ~A.~%" hyps))
		  (multiple-value-bind (pds-concs pds-prems new-nodes)
		      (meth~complete-outlines (rest meth-concs) meth-prems mmapp pds-nodes (pdsn~hyps pds-conc))
		    (values (cons pds-conc
				  pds-concs)
			    pds-prems
			    new-nodes)))
	  ;;; The associated node to METH-CONC must be created, without justification
	      ;; and normal hypotheses. The associated conclusion is the first element in
	      ;; the returned list CREATED-NODES which contains the new created nodes.
	      ;; Moreover SUBST is extended with new bindings for the new nodes. 
	      (let ((created-nodes (meth~create-node meth-conc mmapp :null-just T)))
		(multiple-value-bind (pds-concs pds-prems new-nodes)
		    (meth~complete-outlines (rest meth-concs) meth-prems mmapp 
					    (append pds-nodes created-nodes) hyps)
		  (values (cons (first created-nodes) pds-concs) pds-prems new-nodes)))))))
    ;;; Now, consider the premises:
    (if meth-prems
	(let* ((meth-prem (first meth-prems)))
	  (if (meth~meta-node-p meth-prem)
	      (let* ((metavar (meth~meta-node-metavar meth-prem))
		     (aux-prems (meth~mapp-get-component metavar mmapp :both))
		     (prems (if (listp aux-prems) aux-prems (list aux-prems)))
		     )
		(multiple-value-bind (pds-concs pds-prems new-nodes)
		    (meth~complete-outlines nil
					    (rest meth-prems)
					    mmapp
					    (if (meth~zero-node-p meth-prem)
						pds-nodes
					      (union prems pds-nodes))
					    hyps)
		  (values pds-concs (union prems pds-prems) new-nodes)))
	    
	    (let ((pds-prem (meth~mapp-get-component (keim~name meth-prem) mmapp :mapp)))
	      (if pds-prem
	      ;;; PDS-PREM is existent, its eventually specified extra hyps are
		  ;; considered as normal hyps for the new nodes. 
		  (multiple-value-bind (pds-concs pds-prems new-nodes)
		      (meth~complete-outlines nil (rest meth-prems) mmapp
					      pds-nodes
					      (union (pdsn~hyps pds-prem) hyps))
		    (values pds-concs (cons pds-prem pds-prems) new-nodes))
	    ;;; The associated node to METH-PREM must be created with the justification
		;; of METH-PREM and without normal hyps.
		(let ((created-nodes (meth~create-node meth-prem mmapp)))
		  (multiple-value-bind (pds-concs pds-prems new-nodes)
		      (meth~complete-outlines nil (rest meth-prems) mmapp 
					      (append created-nodes pds-nodes) hyps)
		    (values pds-concs (cons (first created-nodes) pds-prems) new-nodes)))))))
      ;;; Now, we have to set the normal hyps of the new-nodes
      (dolist (new-node pds-nodes (values nil nil pds-nodes))
	(when (or (null (node~justification new-node))
		  (not (pdsn~hypothesis-p new-node)))
	  (setf (pdsn~hyps new-node)
		(union (pdsn~hyps new-node) hyps)))))
    ))


(defmethod meth~create-node ((meth-node meth+node) (mmapp meth+mapping)
			     &key ((:null-just null-just)) ((:hyps hyps)))
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "A method node, a method mapping, which does not contain a binding for METH-NODE,"
		    "but it is suffisant to create a pds node to be associated to METH-NODE. Moreover"
		    "two key words stating whether the created node should have a justification"  
		    "(:NULL-JUST), and which are their normal hypotheses (:HYPS). When :NULL-JUST and"
		    ":HYPS are together NIL the hypothesis list of the new created node is determined"
		    "from its premises if this node is not open and not a hypothesis.")
	   (effect  "Extending MMAPP with bindings of new created nodes.")
	   (value   "A list of new created nodes, its first element is the pds node associated to"
		    "METH-NODE, if this is possible using the bindings in MMAPP, otherwise NIL."))
  (let ((node-formula (meth~object-instance (node~formula meth-node) mmapp)))
    (if node-formula
	(let ((extra-hyps nil)
	      (new-hyps nil))
	  ;;; Determining the new and extra hypotheses
	  (dolist (xtra-hyp (pdsn~hyps meth-node))
	    (let ((pds-hyp (meth~mapp-get-component (keim~name xtra-hyp) mmapp :mapp)))
	      (if pds-hyp
		  (setq extra-hyps (cons pds-hyp extra-hyps))
		(if (ssterm~var-p xtra-hyp)
		    (let ((add-hyps (meth~mapp-get-component xtra-hyp mmapp :mapp))
			  )
		      (if add-hyps
			  (setq extra-hyps (union add-hyps extra-hyps))
			(return-from meth~create-node
			  (omega~error "Not possible to create ~A with the binding ~A.~%"
				       xtra-hyp mmapp))))
		  (let ((new-nodes (meth~create-node xtra-hyp mmapp)))
		    (if new-nodes
			(setq extra-hyps (append extra-hyps new-nodes)
			      new-hyps (append new-hyps new-nodes))
		      (return-from meth~create-node
			(omega~error "Not possible to create ~A with the binding ~A.~%" xtra-hyp mmapp))))))))
	  (if null-just
	      (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
					   NIL node-formula NIL)))
		(setf (pdsn~hyps new-node) (append extra-hyps hyps))
		(meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
		(cons new-node new-hyps))
	    (let ((node-just)
		  (node-hyps extra-hyps))
	      (if (listp hyps)
		  ;;; The hypotheses of the new created node is given by the caller of this function.
		  (setq node-hyps (append extra-hyps hyps)
			node-just (meth~object-instance (node~justification meth-node) mmapp hyps))
		;;; The hypotheses of the new created node are the union of the normal hypotheses
		;; of its premises together with its extra-hyps:
		(multiple-value-bind (the-just the-hyps)
		    (meth~object-instance (node~justification meth-node) mmapp hyps)
		  (setq node-hyps (union extra-hyps the-hyps)
			node-just the-just)))
	      (if node-just
		  (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
					       NIL node-formula node-just))
			(just-method (just~method node-just)))
		    (if (and (infer~dummy-p just-method)
			     (not (infer~open-p just-method)))
		        ;;; The created node is a hypothesis node:
			(setf (pdsn~hyps new-node) (list new-node))
		      (setf (pdsn~hyps new-node) node-hyps))
		    (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
		    (cons new-node new-hyps))
		(omega~error "Not possible to create ~A with the binding ~A.~%"
			     (node~justification meth-node) mmapp)))))
      (omega~error "Not possible to create ~A with the binding ~A.~%"
		   (node~formula meth-node) mmapp))
    ))


(defmethod meth~create-node ((meta-node meth+meta-node) (mmapp meth+mapping)
			     &key ((:null-just null-just))
			     ((:hyps hyps)))
  (declare (edited  "27-MAY-1998" "12-MAR-1998")
	   (authors Jzimmer Lassaad)
	   (input   "A method node, a method mapping, which does not contain a binding for METH-NODE,"
		    "but it is suffisant to create a pds node to be associated to METH-NODE. Moreover"
		    "two key words stating whether the created node should have a justification"  
		    "(:NULL-JUST), and which are their normal hypotheses (:HYPS). When :NULL-JUST and"
		    ":HYPS are together NIL the hypothesis list of the new created node is determined"
		    "from its premises if this node is not open and not a hypothesis.")
	   (effect  "Extending MMAPP with bindings of new created nodes.")
	   (value   "A list of new created nodes, its first element is the pds node associated to"
		    "METH-NODE, if this is possible using the bindings in MMAPP, otherwise
NIL."))
  
  (let* ((var (meth~meta-node-metavar meta-node))
	 (val (meth~mapp-get-component var mmapp :mapp))
	 )
    val))

(defgeneric meth~object-instance (meth-obj mmapp &optional hyps)
  (declare (edited  "12-MAR-1998")
	   (authors Lassaad)
	   (input   "A method object, a method mapping, and optionally HYPS."
		    "When specified, this argument is either a non-empty list"
		    "of nodes to be used as normal premises for new created"
		    "nodes, or T signaling that hypothesis list of new created"
		    "nodes must be determined from its premises.")
	   (effect  "Might extend MMAPP with new created pds-objects.")
	   (value   "The associated pds object of METH-OBJ when MMAPP is"
		    "sufficient to create this object, if the pds object is"
		    "a justification and HYPS is set to T, then this pds object"
		    "is returned in a pair together with the union of the normal"
		    "hypotheses of the premises, otherwise NIL."))
  (:method ((meth-obj-list cons) (mmapp meth+mapping) &optional hyps)
	   ;;; METH-OBJ-LIST may not be empty
	   (let ((flatten? (every #'(lambda (x) (or (meth~node-p x) (pdsn~p x)
						    (eq (ssterm~sort x) (ssterm~get-ssort :prlnlist))
						    (eq (ssterm~sort x) (ssterm~get-ssort :prln))))
				  meth-obj-list))
		 (pds-obj (meth~object-instance (first meth-obj-list) mmapp hyps)))
	     (when pds-obj
	       (if (rest meth-obj-list)
		   (let ((pds-obj-list (meth~object-instance (rest meth-obj-list) mmapp hyps)))
		     (cond ((and pds-obj-list flatten?)
			    (meth=flat-list (cons pds-obj pds-obj-list)))
			   (pds-obj-list (cons pds-obj pds-obj-list))))
		 (list pds-obj)))))
  (:method ((meth-node meth+node) (mmapp meth+mapping) &optional hyps)
	   (let ((pds-node (or (meth~mapp-get-component (keim~name meth-node) mmapp :mapp)
			       (meth~mapp-get-component (keim~name meth-node) mmapp :ext))))
	     (if pds-node pds-node
	       (let ((node-formula (meth~object-instance (node~formula meth-node) mmapp)))
		 ;;; LCh: hier muss man die Typ-variablen, die nicht in der pds-env vorkommen mit einer
		 ;; Kappa binden und auch bei Just-parametern.
		 (when node-formula
		   (let ((extra-hyps (pdsn~hyps meth-node)))
		     (if extra-hyps
			 ;;; Node with extra hyps:
			 (let ((pds-hyps (meth~object-instance extra-hyps mmapp)))
			   (when pds-hyps
			     (let ((node-just)
				   (node-hyps pds-hyps))
			       (if (listp hyps)
				   ;;; HYPS is either NIL or a list of pds nodes to be used as normal hypotheses
				   (setq node-hyps (append pds-hyps hyps)
					 node-just (meth~object-instance (node~justification meth-node) mmapp))
				 ;;; HYPS is T, thus the normal hypotheses must be determined from the premises 
				 (multiple-value-bind (pds-just hyp-list)
				     (meth~object-instance (node~justification meth-node) mmapp hyps)
				   (setq node-hyps (union pds-hyps hyp-list)
					 node-just pds-just)))
			       (when node-just
				 (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
							      NIL node-formula node-just))
				       (just-method (just~method node-just)))
				   (if (and (infer~dummy-p just-method)
					    (not (infer~open-p just-method)))
		                       ;;; The created node is a hypothesis node:
				       (setf (pdsn~hyps new-node) (list new-node))
				     (setf (pdsn~hyps new-node) node-hyps))
				   (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
				   new-node)))))
		       ;;; Node without extra hyps:
		       (let ((node-just)
			     (node-hyps))
			 (if (listp hyps)
			     ;;; HYPS is either NIL or a list of pds nodes to be used as normal hypotheses
			     (setq node-hyps hyps
				   node-just (meth~object-instance (node~justification meth-node) mmapp))
			   ;;; HYPS is T, thus the normal hypotheses must be determined from the premises 
			   (multiple-value-bind (pds-just hyp-list)
			       (meth~object-instance (node~justification meth-node) mmapp hyps)
			     (setq node-hyps hyp-list 
				   node-just pds-just)))
			 (when node-just
			   (let ((new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
							NIL node-formula node-just))
				 (just-method (just~method node-just)))
			     (if (and (infer~dummy-p just-method)
				      (not (infer~open-p just-method)))
		                 ;;; The created node is a hypothesis node:
				 (setf (pdsn~hyps new-node) (list new-node))
			       (setf (pdsn~hyps new-node) node-hyps))
			     (meth~mapp-extend-mapp mmapp (keim~name meth-node) new-node)
			     new-node)))))))
	       )))
  (:method ((meth-obj meth+meta-node) (mmapp meth+mapping) &optional hyps)
	   (meth~create-node meth-obj mmapp :hyps hyps))
  
  (:method ((meth-just meth+just) (mmapp meth+mapping) &optional hyps)
	   (let ((just-method (meth~object-instance (just~method meth-just)
						    mmapp)))
             (unless just-method
               (return-from meth~object-instance))
             (if (infer~open-p just-method)
		 (if (listp hyps) (pdsj~open-just-create)
		   (values (pdsj~open-just-create) nil))
               (let* ((meth-prems (just~premises meth-just))
                      (just-prems (when meth-prems
                                    (meth~object-instance meth-prems mmapp hyps)))
                      (meth-params (if (and meth-prems (not just-prems))
                                       (return-from meth~object-instance)
                                     (pdsj~parameters meth-just)))
                      (just-params (when meth-params
                                     (meth~object-instance meth-params mmapp)))
                      (meth-status (if (and meth-params (not just-params))
                                       (return-from meth~object-instance)
                                     (pdsj~status meth-just)))
                      (just-status (when meth-status
                                     (meth~object-instance meth-status mmapp))))
                 (when (and meth-status (not just-status))
                   (return-from meth~object-instance))
		 (if (listp hyps)
		     (pdsj~closed-just-create just-method just-prems just-params
					      (if just-status just-status
						(if (or (infer~dummy-p just-method)
							(infer~rule-p just-method))
						    "grounded" "unexpanded")))
		   (let ((prems-hyps))
		     (do ((rest-meth-prems meth-prems (rest rest-meth-prems))
			  (rest-just-prems just-prems (rest rest-just-prems)))
			 ((null rest-meth-prems)
			  (values (pdsj~closed-just-create just-method just-prems just-params
							   (if just-status just-status
							     (if (or (infer~dummy-p just-method)
								     (infer~rule-p just-method))
								 "grounded" "unexpanded")))
				  prems-hyps))
		       (let* ((meth-prem (first rest-meth-prems))
			      (meth-prem-xhyps (pdsn~hyps meth-prem))
			      (just-prem-xhyps (meth~mapp-get-component (mapcar #'keim~name meth-prem-xhyps)
									mmapp :all)))
			 (setq prems-hyps (union prems-hyps
						 (meth=set-difference (pdsn~hyps (first rest-just-prems))
								 just-prem-xhyps)))))))
		 ))))
  (:method ((meth-appl term+appl) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   (let ((func (meth~object-instance (data~appl-function meth-appl) mmapp)))
	     (when func
	       (let ((args (meth~object-instance (data~appl-arguments meth-appl) mmapp)))
		 (when args
		   (if (term~abstr-p func)
		       (beta~normalize (term~appl-create func (meth=flat-list args)))
		     (if (and (or (logic~existential-quantor-p func)
				  (logic~universal-quantor-p func))
			      (rest (data~abstr-domain (car (meth=flat-list args)))))
			 (let* ((vars2bind (data~abstr-domain (car (meth=flat-list args))))
				(arg-range (data~abstr-range (car (meth=flat-list args))))
				(result (dolist (var (reverse vars2bind) arg-range)
					  (setq arg-range
						(term~appl-create
						 ;;LC: unneeded
						 ;;AM: Doch Needed
						 (if (data~constant-p func)
						     (data~copy func)
						   func)
						 (list (term~abstr-create (list var)
									  arg-range)))))))
			   result)
		       (term~appl-create
			;;LC: unneeded
			;;AM: DOCH NEEDED!
			(if (data~constant-p func)
			    (data~copy func)
			  func)
			(meth=flat-list args)))))))))
  (:method ((meth-abstr term+abstr) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   (let ((abstr-domain (meth~object-instance (data~abstr-domain meth-abstr) mmapp)))
	     (when abstr-domain
	       (let ((abstr-range (meth~object-instance (data~abstr-range meth-abstr) mmapp)))
		 (when abstr-range
		   (term~abstr-create (if (and (consp abstr-domain)
					       (every #'consp abstr-domain))
					       (apply #'append abstr-domain)
					       abstr-domain)
					  abstr-range))))))
  (:method ((meth-schema term+schema) (mmapp meth+mapping) &optional hyps)
	   ;;LC(meth~object-instance (data~schema-range meth-schema) mmapp hyps))
	   (meth~object-instance (data~schema-range (data~copy meth-schema :downto nil)) mmapp hyps))
  (:method ((meth-var term+syn-sorted-var) (mmapp meth+mapping) &optional hyps)
	   (let* ((subst (meth~mapp-subst mmapp))
		  (pds-obj (subst~get-component meth-var subst)))
	     (if pds-obj pds-obj
	       (let ((pds-obj (or (mapp~get-component meth-var (meth~mapp-mapp mmapp))
				  (meth~mapp-get-component meth-var mmapp :ext))))
		 (if (or (listp hyps) (not (pdsj~justification-p pds-obj))) pds-obj
		   (let ((prems-hyps))
		     (dolist (prem (just~premises pds-obj) (values pds-obj prems-hyps))
		       (setq prems-hyps (union prems-hyps (pdsn~hyps prem))))))))))
  (:method ((const term+number) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   const)
  (:method ((const term+primitive) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   ;;(omega~trace "~%meth~~object-instance of const (vorher) ~A~%" const)
	   ;;; Because of free type-variables
	   (let* ((mapp (meth~mapp-subst mmapp))
		  (map-dom (remove-if-not #'type~p (subst~domain mapp)))
		  (the-const (subst~apply (subst~restrict-substitution mapp map-dom)
					  const))
		  (const-type-vars (type~free-variables (term~type the-const)))
		  (ext-type-vars (when (meth~mapp-extension mmapp)
				   (remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp))))))
	     
	     (if (intersection const-type-vars ext-type-vars)
		 (data~replace-free-variables the-const ext-type-vars
					      (remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))
	       the-const)))
  (:method ((type type+type) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   ;;; Because of free type-variables
	   (let* ((mapp (meth~mapp-subst mmapp))
		  (map-dom (remove-if-not #'type~p (subst~domain mapp)))
		  (the-type (subst~apply (subst~restrict-substitution mapp map-dom) type))
		  (type-vars (type~free-variables the-type))
		  (ext-type-vars (when (meth~mapp-extension mmapp)
				   (remove-if-not #'type~p (mapp~domain (meth~mapp-extension mmapp))))))
	     (if (intersection type-vars ext-type-vars)
		 (data~replace-free-variables the-type ext-type-vars
					      (remove-if-not #'type~p (mapp~codomain (meth~mapp-extension mmapp))))
	       the-type)))
  (:method ((inference infer+inference) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   inference)
  (:method ((expr meth+key-expression) (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   (meth~pds-object expr mmapp nil))
  (:method (meth-obj (mmapp meth+mapping) &optional hyps)
	   (declare (ignore hyps))
	   (omega~error "Dont know how to determine ~A an instance of ~A using ~A.~%"
			meth-obj (type-of meth-obj) mmapp)))

(defun meth=flat-list (alist)
  (declare (edited  "03-JUL-1997")
	   (authors Lassaad)
	   (input   "A list, that possibly contains lists as elements.")
	   (effect  "None.")
	   (value   "The flatted list."))
  (when alist
    (let ((elt (first alist)))
      (if (listp elt)
	  (append (meth=flat-list elt)
		  (meth=flat-list (rest alist)))
	(cons elt (meth=flat-list (rest alist)))))
    ))

;; Old Version:
;; Problem: Not List sensitive.
;; Assume the input is a list with two meta-nodes N1, N2: (N1 N2)
;; In the mapping N1 is bound to a list of NonExistent nodes (n1 n2 n3)
;; and N2 is bound to a closed node n4
;; However, the output is (NONEXISTENT NONEXISTENT NONEXISTENT NONEXISTENT CLOSED)
;; So this output does not sign that there is a list of nodes corresponding to
;; NONEXISTENT NONEXISTENT NONEXISTENT NONEXISTENT
;;
;;
;;(defun meth~prems-outline-pattern (meth-prems &optional (mmapp nil))
;;  (declare (edited  "12-JUN-1998" "13-MAR-1998")
;;	   (authors Jzimmer Lassaad)
;;	   (input   "A list of method nodes.")
;;	   (effect  "None.")
;;	   (value   "A list of outline patterns according to the"
;;		    "class of METH-PREMS elements: Plus, minus,"
;;		    "and unsigned premises are respectively associated"
;;		    "NONEXISTENT, CLOSED, and EXISTENT."))
;;  (when meth-prems
;;    (let ((meth-prem (first meth-prems)))
;;      (if (meth~meta-node-p meth-prem)
;;	  (when mmapp
;;	    (let* ((metavar (meth~meta-node-metavar meth-prem))
;;		   (aux-prems (meth~mapp-get-component metavar mmapp :both))
;;		   (prems (if (listp aux-prems) aux-prems (list aux-prems)))
;;		   )
;;	      (if (meth~plus-node-p meth-prem)
;;		  (append (substitute-if "NONEXISTENT"
;;					 #'(lambda (node) T)
;;					 prems)
;;			  (meth~prems-outline-pattern (rest meth-prems) mmapp))
;;		(append (substitute-if "CLOSED"
;;				       #'(lambda (node) T)
;;				       prems)
;;			(meth~prems-outline-pattern (rest meth-prems) mmapp)))))
;;	
;;	(cons (if (meth~plus-node-p meth-prem) "NONEXISTENT"
;;		(if (meth~minus-node-p meth-prem) "CLOSED"
;;		  "EXISTENT"))
;;	      (meth~prems-outline-pattern (rest meth-prems) mmapp)))
;;     )))
;;
;;
;; New Version (changed by Ameier)
;; Lists of NONExistent/CLOSED nodes will be represented by one NONEXISTENT or one CLOSED
;; That is, the example from above will produce the output (NONEXISTENT CLOSED)
;; where the NONEXISTENT stands for the list of 4 NONEXISTENT nodes.
;; A list of closed nodes is represented by LIST

(defun meth~prems-outline-pattern (meth-prems &optional (mmapp nil))
  (declare (edited  "12-JUN-1998" "13-MAR-1998")
	   (authors Jzimmer Lassaad Ameier)
	   (input   "A list of method nodes.")
	   (effect  "None.")
	   (value   "A list of outline patterns according to the"
		    "class of METH-PREMS elements: Plus, minus,"
		    "and unsigned premises are respectively associated"
		    "NONEXISTENT, CLOSED, and EXISTENT."))
  (when meth-prems
    (let ((meth-prem (first meth-prems)))
      (if (meth~meta-node-p meth-prem)
	  (when mmapp
	    (let* ((metavar (meth~meta-node-metavar meth-prem))
		   (aux-prems (meth~mapp-get-component metavar mmapp :both))
		   (prems (if (listp aux-prems) aux-prems (list aux-prems)))
		   )
	      (if (meth~plus-node-p meth-prem)
		  (append (list (first (substitute-if infer*non-existent
						      #'(lambda (node) T)
						      prems)))
			  (meth~prems-outline-pattern (rest meth-prems) mmapp))
		(append (if (null prems)
			    (list infer*list)
			  (list (first (substitute-if infer*list
						      #'(lambda (node) T)
						      prems))))
			(meth~prems-outline-pattern (rest meth-prems) mmapp)))))
	(cons (if (meth~plus-node-p meth-prem) infer*non-existent
		(if (meth~minus-node-p meth-prem)
		    infer*closed
		  infer*existent))
	      (meth~prems-outline-pattern (rest meth-prems) mmapp)))
      )))



(defun meth~select-pds-nodes (meth-nodes pds-nodes test-func)
  (declare (edited  "13-MAR-1998")
	   (authors Lassaad)
	   (input   "A list of method nodes, the associated list of pds-nodes,"
		    "and a test function.")
	   (effect  "None.")
	   (value   "The associated sublist of PDS-NODES for the elements of"
		    "METH-NODES that fullfill the TEST-FUNC."))
  (if meth-nodes
      (if pds-nodes
	  (if (funcall test-func (first meth-nodes))
	      (cons (first pds-nodes)
		    (meth~select-pds-nodes (rest meth-nodes) (rest pds-nodes) test-func))
	    (meth~select-pds-nodes (rest meth-nodes) (rest pds-nodes) test-func))
	(omega~error " ..."))
    (when pds-nodes
      (omega~error " ..."))
    ))
	    
 
;; matching function
(defgeneric meth~match-p (meth-obj pds-obj mmapps indicator &optional node-formula) 
  (declare (edited  "09-MAR-1998")
	   (authors Lassaad)
	   (input   "A method object, a pds object, either a substitution or a"
		    "non-empty list of substitutions, an INDICATOR stating how"
		    "to match METH-OBJ to PDS-OBJ, and optionally a node formula"
		    "as a parameter stating for the current formula of a pds node,"
		    "i.e., the formula resulted by considering the current bindings"
		    "of the pds-metavariables.")
	   (effect  "None.")
	   (value   "A list of substitutions resulted from matching METH-OBJ to"
		    "PDS-OBJ according to INDICATOR using the given MMAPPS, Where:"
		    "- :ONE2ONE states that METH-OBJ must match PDS-OBJ,"
		    "- :ONE2MANY states that METH-OBJ must match one of the elements"
		    "in the list PDS-OBJ, and"
		    "- :MANY2MANY states that each element in the list METH-OBJ must"
		    "match one of the elements in the list PDS-OBJ."))
  ;;;LCh: If pds meta-variables occur not only in the formulas of open nodes but in
  ;; other pds objects as support nodes, justification parameters too, then you have
  ;; to change NODE-FORMULA by MVAR-INFO which can be either a formula or a substitution
  ;; for pds meta-variables.
  (:method (meth-obj (pds-list list) (mmapps T) (indicator (eql :one2many)) &optional node-formula)
	   (declare (ignore node-formula))
	   (when pds-list
	     (if (rest pds-list)
		 (let ((mmapps-copy (meth~mapping-copy mmapps)))
		   (append (meth~match-p meth-obj (first pds-list) mmapps :one2one)
			   (meth~match-p meth-obj (rest pds-list) mmapps-copy :one2many)))
	       (meth~match-p meth-obj (first pds-list) mmapps :one2one))))
  (:method ((meth-list list) (pds-list list) (mmapps T) (indicator (eql :many2many)) &optional node-formula)
	   (declare (ignore node-formula))
	   (if meth-list
	       (let ((new-mmapps (meth~match-p (first meth-list) pds-list mmapps :one2many)))
		 (when new-mmapps
		   (if (rest meth-list)
		       (meth~match-p (rest meth-list) pds-list new-mmapps :many2many)
		     new-mmapps)))
	     mmapps))
  (:method ((meth-list list) (pds-list list) (mmapps T) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (if meth-list
	       (when pds-list
		 (let ((new-mmapps (meth~match-p (first meth-list) (first pds-list) mmapps :one2one)))
		   (when new-mmapps
		     (meth~match-p (rest meth-list) (rest pds-list) new-mmapps :one2one))))
	     (unless pds-list mmapps)))
  (:method ((meth-node meth+node) (pds-node pdsn+node) (mmapps cons) (indicator (eql :one2one)) &optional node-formula)
	   (if (rest mmapps)
	       (append (meth~match-p meth-node pds-node (first mmapps) :one2one node-formula)
		       (meth~match-p meth-node pds-node (rest mmapps) :one2one node-formula))
	     (meth~match-p meth-node pds-node (first mmapps) :one2one node-formula)))

  (:method ((meth-node meth+node) (pds-node pdsn+node) (mmapp meth+mapping) (indicator (eql :one2one)) &optional node-formula)
	   (let ((meth-node-name (keim~name meth-node)))
	     (if (meth~mapp-get-component meth-node-name mmapp :mapp)
		 ;;; METH-NODE was bound
		 (list mmapp)
	       (let ((mmapps1 (meth~match-p (node~formula meth-node)
					    (if node-formula node-formula
					      (node~formula pds-node))
					    (list mmapp) :one2one)))
		 (if mmapps1
		   (let ((extra-hyps (pdsn~hyps meth-node)))
		     (if extra-hyps
			 (let ((mmapps2 (meth~match-p extra-hyps
						      (pdsn~hyps pds-node)
						      mmapps1 :many2many)))
			   (when mmapps2
			     (let* ((meth-node-just (node~justification meth-node))
				    (mmapps3 (if meth-node-just
						 (meth~match-p meth-node-just
							       (node~justification pds-node)
							       mmapps2 :one2one)
					       mmapps2)))
			       (when mmapps3
				 (meth~mapp-extend-mapp (first mmapps3) meth-node-name pds-node)
				 ;;; Because this function is destructive, it is sufficient to call
				 ;; it once in order to enter the binding of METH-NODE-NAME to the
				 ;; PDS-NODE in all elements of MMAPPS3.
				 mmapps3))))
		       (let* ((meth-node-just (node~justification meth-node))
			      (mmapps2 (if meth-node-just
					   (meth~match-p meth-node-just
							 (node~justification pds-node)
							 mmapps1 :one2one)
					 mmapps1)))
			 (when mmapps2
			   (meth~mapp-extend-mapp (first mmapps2) meth-node-name pds-node)
			   ;;; Because this function is destructive, it is sufficient to call
			   ;; it once in order to enter the binding of METH-NODE-NAME to the
			   ;; PDS-NODE in all elements of MMAPPS2.
			   mmapps2))))
		   (progn (setf ana*cause-of-failure (list pds-node meth-node)) nil))
		 ))))
  (:method ((meth-node meth+node) (pds-node pdsn+schematic-node) (mmapps cons) (indicator (eql :one2one))
	    &optional node-formula)
	   (if (rest mmapps)
	       (cond ((null node-formula)
		      (append (meth~match-p meth-node pds-node (first mmapps) :one2one
					    (pds~node-formula pds-node omega*current-proof-plan))
			      (meth~match-p meth-node pds-node (rest mmapps) :one2one
					    (pds~node-formula pds-node omega*current-proof-plan))))
		     (T
		      (append (meth~match-p meth-node pds-node (first mmapps) :one2one node-formula)
			      (meth~match-p meth-node pds-node (rest mmapps) :one2one node-formula))))
	     (cond ((null node-formula)
		    (meth~match-p meth-node pds-node (first mmapps) :one2one
				  (pds~node-formula pds-node omega*current-proof-plan)))
		   (T
		    (meth~match-p meth-node pds-node (first mmapps) :one2one node-formula)))
	     ))
  (:method ((meth-node meth+node) (pds-node pdsn+schematic-node) (mmapp meth+mapping) (indicator (eql :one2one))
	    &optional node-formula)
	   (let ((meth-node-name (keim~name meth-node)))
	     (if (meth~mapp-get-component meth-node-name mmapp :mapp)
		 ;; METH-NODE was bound
		 (list mmapp)
	       (let ((mmapps1 (meth~match-p (node~formula meth-node)
					    (if node-formula node-formula
					      (pds~node-formula pds-node omega*current-proof-plan))
					    (list mmapp) :one2one)))
		 (when mmapps1
		   (let ((extra-hyps (pdsn~hyps meth-node)))
		     (if extra-hyps
			 (let ((mmapps2 (meth~match-p extra-hyps
						      (pdsn~hyps pds-node)
						      mmapps1 :many2many)))
			   (when mmapps2
			     (let* ((meth-node-just (node~justification meth-node))
				    (mmapps3 (if meth-node-just
						 (meth~match-p meth-node-just
							       (node~justification pds-node)
							       mmapps2 :one2one)
					       mmapps2)))
			       (when mmapps3
				 (meth~mapp-extend-mapp (first mmapps3) meth-node-name pds-node)
				 ;; Because this function is destructive, it is sufficient to call
				 ;; it once in order to enter the binding of METH-NODE-NAME to the
				 ;; PDS-NODE in all elements of MMAPPS3.
				 mmapps3))))
		       (let* ((meth-node-just (node~justification meth-node))
			      (mmapps2 (if meth-node-just
					   (meth~match-p meth-node-just
							 (node~justification pds-node)
							 mmapps1 :one2one)
					 mmapps1)))
			 (when mmapps2
			   (meth~mapp-extend-mapp (first mmapps2) meth-node-name pds-node)
			   ;; Because this function is destructive, it is sufficient to call
			   ;; it once in order to enter the binding of METH-NODE-NAME to the
			   ;; PDS-NODE in all elements of MMAPPS2.
			   mmapps2)))))
		 ))))
  (:method ((meth-term term+term) (pds-term term+term) (mmapps T) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (if (listp mmapps)
	       (let* ((mmapp1 (first mmapps))
		      (match-res (ssterm~alpha-match meth-term pds-term
                                                     :assocs (logic~quantifiers)
                                                     :subst (meth~mapp-subst mmapp1))))
		 (append (meth~mapp-new-subst mmapp1 match-res)
			 (when (rest mmapps)
			   (meth~match-p meth-term pds-term (rest mmapps) :one2one))))
	     (let ((match-res (ssterm~alpha-match meth-term pds-term
						  :assocs (logic~quantifiers)
						  :subst (meth~mapp-subst mmapps))))
	       (when match-res
		 (meth~mapp-new-subst mmapps match-res)))))
;  
;                                      (ssterm~alpha-match meth-term pds-term
;                                                      :assocs (logic~quantifiers)
;                                                      :add-bind (meth~mapp-subst mmapps)))))
  (:method ((meth-obj term+syn-sorted-var) pds-obj (mmapp meth+mapping) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (let ((assoc-obj (meth~mapp-get-component meth-obj mmapp :both)))
	     (if assoc-obj
		 (when (keim~equal assoc-obj pds-obj)
		   mmapp)
	       (when (meth=syntactic-sort-p pds-obj (ssterm~sort meth-obj))
		 (meth~mapping-extend mmapp meth-obj pds-obj)))))
  (:method ((meth-obj term+syn-sorted-var) pds-obj (mmapps list) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (when mmapps
	     (let ((mmapp1 (meth~match-p meth-obj pds-obj (first mmapps) :one2one)))
	       (if mmapp1
		   (cons mmapp1
			 (meth~match-p meth-obj pds-obj (rest mmapps) :one2one))
		 (meth~match-p meth-obj pds-obj (rest mmapps) :one2one)))))
  (:method ((meth-just meth+just) (pds-just pdsj+justification) (mmapps T) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (let ((match-status (meth~just-match-status meth-just)))                                    
             (if (eq match-status :open)
                 (when (pdsj~open-just-p pds-just)
                   mmapps)
               (let ((mmapps1 (meth~match-p (just~premises meth-just)
                                            (just~premises pds-just)
                                            mmapps :one2one)))
                 (when mmapps1
                   (let ((mmapps2 (meth~match-p (pdsj~parameters meth-just)
						(pdsj~parameters pds-just)
						mmapps1 :one2one)))
                     (when mmapps2 
                       (let ((mmapps3 (meth~match-p (pdsj~status meth-just)
                                                    (pdsj~status pds-just)
                                                    mmapps2 :one2one)))
                         (when mmapps3
                           (meth~match-p (just~method meth-just)
                                         (just~method pds-just)
                                         mmapps3 :one2one))))))))))
  (:method ((null-just null) (pds-just pdsj+justification) (mmapps T) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   mmapps)
  (:method ((infer1 infer+inference) (infer2 infer+inference) (mmapps T) (indicator (eql :one2one)) &optional node-formula)
	   (declare (ignore node-formula))
	   (when (eq infer1 infer2)
	     mmapps))

  )

(defun meth~disjoint-compose-substitution (old-subst new-subst)
  (setf (subst~domain new-subst)
	(append (subst~domain new-subst) (subst~domain old-subst))
	(subst~codomain new-subst)
	(append (subst~codomain new-subst) (subst~codomain old-subst)))
  new-subst)
	
(defgeneric meth=syntactic-sort-p (pds-obj ssort)
  (declare (edited  "02-APR-1998")
	   (authors Lassaad)
	   (input   "A pds object, and a syntactic sort.")
	   (effect  "None.")
	   (value   "T, iff PDS-OBJ corresponds to the sort SSORT."))
  (:method (pds-obj (ssort null))
	   (declare (ignore pds-obj))
	   T)
  (:method ((problem prob+problem) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :problem) ssort))
  (:method ((pds-node pdsn+node) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :prln) ssort))
  (:method ((pds-just pdsj+justification) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :just) ssort))
  (:method ((pds-metavar meta+variable) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :metavar) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-var term+variable) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :var) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-const term+constant) (ssort ssterm+syntactic-sort))
	   (or (eq (ssterm~get-ssort :const) ssort)
	       (eq (ssterm~get-ssort :term) ssort)))
  (:method ((pds-const term+term) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :term) ssort))
  (:method ((pos pos+position) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :pos) ssort))
  (:method ((pds-list list) (ssort ssterm+syntactic-sort))
	   (cond ((every #'term~p pds-list)
		  (eq (ssterm~get-ssort :termlist) ssort))
		 ((every #'pdsn~p pds-list)
		  (eq (ssterm~get-ssort :prlnlist) ssort))
		 ((every #'pos~p pds-list)
		  (eq (ssterm~get-ssort :poslist) ssort))
		 (T
		  (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
			       (keim~name ssort) pds-list (type-of pds-list)))))
  (:method ((list string) (ssort ssterm+syntactic-sort))
	   T)
  (:method ((sym symbol) (ssort ssterm+syntactic-sort))
	   (eq (ssterm~get-ssort :symbol) ssort))
  (:method ((pds-obj T) (ssort ssterm+syntactic-sort))
	   (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
			(keim~name ssort) pds-obj (type-of pds-obj)))
  )

;(defgeneric meth=syntactic-sort-p (pds-obj ssort)
;  (declare (edited  "02-APR-1998")
;           (authors Lassaad)
;           (input   "A pds object, and a syntactic sort.")
;           (effect  "None.")
;           (value   "T, iff PDS-OBJ corresponds to the sort SSORT."))
;  (:method (pds-obj (ssort null))
;           (declare (ignore pds-obj))
;           T)
;  (:method ((pds-node pdsn+node) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :prln) ssort))
;  (:method ((pds-just pdsj+justification) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :just) ssort))
;  (:method ((pds-metavar meta+variable) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :metavar) ssort))
;  (:method ((pds-var term+variable) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :var) ssort))
;  (:method ((pds-const term+constant) (ssort ssterm+syntactic-sort))
;           (or (eq (ssterm~get-ssort :const) ssort)
;               (eq (ssterm~get-ssort :term) ssort)))
;  (:method ((pds-const term+term) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :term) ssort))
;  (:method ((pos pos+position) (ssort ssterm+syntactic-sort))
;           (eq (ssterm~get-ssort :pos) ssort))
;  (:method ((pds-list list) (ssort ssterm+syntactic-sort))
;           (cond ((every #'term~p pds-list)
;                  (eq (ssterm~get-ssort :termlist) ssort))
;                 ((every #'pdsn~p pds-list)
;                  (eq (ssterm~get-ssort :prlnlist) ssort))
;                 (T
;                  (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
;                               (keim~name ssort) pds-list (type-of pds-list)))))
;  (:method ((pds-obj T) (ssort ssterm+syntactic-sort))
;           (omega~error ";;;meth=syntactic-sort-p: Dont know how to compare ~S with the sort of ~S an instance of class ~S ~%"
;                        (keim~name ssort) pds-obj (type-of pds-obj)))
;  )

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to be ajusted .....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defgeneric meth~show-method (method &optional stream)
;  (declare (edited  "20-JUN-1997" "20-APR-1995")
;           (authors Lassaad Acsehn)
;           (input "A METHOD and a STREAM." )
;           (effect "prints METHOD on STREAM in a readable way." )
;           (value  "None." ))
;  (:method ((method meth+method) &optional (stream t))
;           (let ((name (keim~name method))
;                 (rating (meth~rating method))
;                 (params (meth~parameters method))
;                 (premises (meth~premises method))
;                 (conclusions (meth~conclusions method))
;                 (outline-constr (meth~outline-constraint method))
;                 (outln-function (meth~outline-function method))
;                 (exp-constr (meth~expansion-constraint method))
;                 (exp-function (meth~expansion-function method))
;                 (procedural-content (meth~procedural-content method))
;                 (declarative-content (meth~declarative-content method))
;                 (remark (meth~remark method)))
;             (format stream "(method ~A" name)
;             (format stream "~% (environment ... )")
;             (format stream "~% (rating ~S)" rating)
;             (format stream "~% (parameters ~S)" params)
;             (format stream "~% (premises ")
;             (meth=show-schematic-node-list premises stream)
;             (format stream ")")
;             (format stream "~% (conclusions ")
;             (meth=show-schematic-node-list conclusions stream)
;             (format stream ")")
;             (format stream "~% (outline-constraint ~S)" outline-constr)
;             (format stream "~% (outline-function ~S)" outln-function)
;             (format stream "~% (expansion-constraint ~S)" exp-constr)
;             (format stream "~% (expansion-function ~S)" exp-function)
;             (format stream "~% (declarative-content~%")
;             (meth~show-nodes declarative-content stream)
;             (dotimes (i meth*indent)
;               (format stream " "))
;             (format stream ")")
;             (format stream "~% (procedural-content~%")
;             (dotimes (i meth*indent)
;               (format stream " "))
;             (format stream "~S" procedural-content)
;             (format stream ")")
;             (format stream "~% (remark ~A))" remark)))
;  
;  (:method ((method meth+supermethod) &optional (stream t))
;           (let ((name (keim~name method))
;                 (rating (meth~rating method))
;                 (params (meth~parameters method))
;                 (premises (meth~premises method))
;                 (conclusions (meth~conclusions method))
;                 (outline-constr (meth~outline-constraint method))
;                 (outln-function (meth~outline-function method))
;                 (exp-constr (meth~expansion-constraint method))
;                 (exp-function (meth~expansion-function method))
;                 (procedural-content (meth~procedural-content method))
;                 (declarative-content (meth~declarative-content method))
;                 (remark (meth~remark method)))
;             (format stream "(supermethod ~A" name)
;             (format stream "~% (environment ... )")
;             (format stream "~% (rating ~S)" rating)
;             (format stream "~% (parameters ~S)" params)
;             (format stream "~% (premises ")
;             (meth=show-schematic-node-list premises stream)
;             (format stream ")")
;             (format stream "~% (conclusions ")
;             (meth=show-schematic-node-list conclusions stream)
;             (format stream ")")
;             (format stream "~% (outline-constraint ~S)" outline-constr)
;             (format stream "~% (outline-function ~S)" outln-function)
;             (format stream "~% (expansion-constraint ~S)" exp-constr)
;             (format stream "~% (expansion-function ~S)" exp-function)
;             (format stream "~% (declarative-content~%")
;             (meth~show-nodes declarative-content stream)
;             (dotimes (i meth*indent)
;               (format stream " "))
;             (format stream ")")
;             (format stream "~% (procedural-content~%")
;             (dotimes (i meth*indent)
;               (format stream " "))
;             (format stream "~S" procedural-content)
;             (format stream ")")
;             (format stream "~% (remark ~A))" remark)))
;  )
;    
;(defun meth=show-schematic-node (node &optional (stream t))
;  (cond ((meth~minus-node-p node)
;         (format stream "(- ~A)" (keim~name node)))
;        ((meth~plus-node-p node)
;         (format stream "(+ ~A)" (keim~name node)))
;        (t
;         (format stream "~A" (keim~name node)))))
;
;(defun meth=show-schematic-node-list (nodelist &optional (stream t))
;  (dolist (node nodelist)
;    (meth=show-schematic-node node stream)
;    (format stream " ")))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post interface (not yet complete!)  VS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pp~modify-style pds-post
   (meth+mapping
    (lambda (s mapp)
      (let ((*standard-output* s))
	(write-string "(method-mapping ")
	(pprint-logical-block
	 (nil (list (meth~mapp-subst mapp) (meth~mapp-mapp mapp) (meth~mapp-constraint mapp))
	      :suffix ")")
	 (pprint-exit-if-list-exhausted)
	 (loop
	  (write (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space)
	  (pprint-newline :fill)))
	))))




(defmethod pds~post-read-proof-plan-after (pds)
  (labels
      ((read-anodes (pool)
		    (when (and (pds~constraint-pool-p pool)
			       (pds~cstrpool-plansteps pool))
		      (setf (pds~cstrpool-plansteps pool)
			    (pds~post-read-obj (pds~cstrpool-plansteps pool)
					       (pds~environment pds)
					       (prob~proof-theory pds)
					       #'pds~label2node
					       nil
					       nil))
		      (read-anodes (pds~cstrpool-previous pool)))))
    (let ((nodes (prob~proof-steps pds)))
      (dolist (node nodes)
	(let ((subst (pdsj~subst (node~justification node))))
	  (when subst
	    (read-anodes (meth~mapp-constraint subst))))))))

(defmethod pds~post-read-meth-mapping (mapping env theory method func)
  (let ((subst-stuff (second mapping))
	(mapp-stuff (third mapping))
	(cstr-stuff (fourth mapping)))
    (meth~mapping-create
     (meth=post-read-meth-subst subst-stuff env theory method func #'meth~subst-create)
     (meth=post-read-meth-subst mapp-stuff env theory method func #'mapp~create)
     nil
     (meth=post-read-meth-cstr-pool cstr-stuff env theory method func))))

(defun meth=post-read-meth-cstr-pool (cstr-pool env theory method func)
  (cond 
    ((null cstr-pool) nil)
    ((consp cstr-pool)
     (pds~cstrpool-create
      (pds~post-read-obj (second cstr-pool) env theory  func (pds~get-inference-env method) NIL)
      (meth=post-read-meth-cstr (third cstr-pool) env theory method func)
      (meth=post-read-meth-cstr-pool (fifth cstr-pool) env theory method func)
      (sixth cstr-pool)
      ))
    ((eq cstr-pool T) T)))

  
(defun meth=post-read-meth-cstr (cstr env theory method func)
  (when cstr 
    (cstr~create (second cstr)
		 (if (eql (car cstr) 'cstr-comp)
		     (mapcar #'(lambda (cs) (meth=post-read-meth-cstr cs env theory method func))
			     (third cstr))
		   (mapcar #'(lambda (pds-obj)
			       (pds~post-read-obj pds-obj env theory func (pds~get-inference-env method) NIL))
			   (third cstr)))
		 (mapcar #'(lambda (mv) (env~lookup-object mv env))
			 (fourth cstr))
		 (fifth cstr))))

(defun meth=post-read-meth-subst (subst env theory method func1 func2)
  (declare (edited  "29-MAY-1997")
	   (authors Lassaad)
	   (input   "A PDS-POST representation of a substitution, an environment,"
		    "a method, and a function. The function FUNC delivers for each"
		    "node label the associated node in the PDS.")
	   (effect  "None.")
	   (value   "The substitution object which corresponds to the bindings of"
		    "the METHOD metavariables to PDS-objects."))
  (if subst
      (let* ((domain (second subst))
	     (codomain (third subst))
	     (meth-env (pds~get-inference-env method))
	     (meta-vars (mapcar #'(lambda (sym)
				    (env~lookup-object sym meth-env))
				domain))
	     (thecodomain  (mapcar #'(lambda (pds-obj)
				       (pds~post-read-obj pds-obj env theory func1 meth-env NIL))
				   codomain)))
	(funcall func2 (remove nil meta-vars) thecodomain))
    (funcall func2 nil nil)));the empty subst/mapp

;;; inserted by Afiedler
(defmethod infer~theory ((method infer+method))
  (meth~theory (meth~find-method (infer~find-arbitrary-application-name method))))

(defmethod infer~theory ((method infer+supermethod))
  (meth~theory (meth~find-method (infer~find-arbitrary-application-name method))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help stuff for methods 
;; author: MP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ohlp~defhelp methods
	      (object-list ohlp=methods)
	      (help "Help for commands."))

(defmethod help~help-string ((method meth+method))
  (let ((help-string (call-next-method))
	(remark (meth~remark method))
	(manual (meth~manual method)))
    (flet ((not-empty-string (obj)
			     (and (stringp obj) (not (equal obj "")))))
      (labels ((detag (expr)
		      (let ((start (position #\< expr))
			    (end (position #\> expr)))
			(cond ((not (and start end)) expr)
			      ((< end start) (concatenate 'string (subseq expr 0 start) (detag (subseq expr start))))
			      (t (concatenate 'string (subseq expr 0 start) (detag (subseq expr (1+ end)))))))))
	(cond ((not-empty-string help-string) help-string)
	      ((not-empty-string remark) (detag remark))
	      ((and (listp remark) (not-empty-string (car remark)))
	       (detag (car remark)))
	      ((not-empty-string manual) (detag manual))
	      ((listp manual)
	       (let ((docu (assoc 'documentation manual :test #'string-equal)))
		 (if (and (listp docu) (not-empty-string (second docu)))
		     (detag (second docu))
		   "")))
	      (t ""))))))

(defun ohlp=methods ()
  (ohlp~hash2list meth*method-hashtable t))

(defmethod ohlp~pprint-object ((method meth+method))
  (format nil "~%~A: ~A~%~%For more details a look in the online-manual for method  ~A." 
	      (keim~name method) (help~help-string method) (keim~name method)))

(defmethod ohlp~html-object ((method meth+method))
  (let* ((theory (keim~name (meth~theory method)))
	   (docu     (mapcan #'(lambda (man) (when (equal (car man) 'documentation) (cdr man)))
			     (meth~manual method)))
	   (author   (mapcan #'(lambda (man) (when (equal (car man) 'author) (cdr man)))
			     (meth~manual method)))
	   (examples (mapcan #'(lambda (man) (when (equal (car man) 'examples) (cdr man)))
			     (meth~manual method)))
	   (prems  (mapcar #'(lambda (conc)  (if (meth~node-sign conc)
						(if (eq (meth~node-sign conc) '+)
						    (list "#ff0000"  (meth~node-sign conc) (keim~name conc))
						  (list "#0000ff"  (meth~node-sign conc) (keim~name conc)))
					      (list "#0000ff" (keim~name conc))))
			   (meth~premises method)))
	   (concs  (mapcar #'(lambda (conc)   (if (meth~node-sign conc)
						(if (eq (meth~node-sign conc) '+)
						    (list "#ff0000"  (meth~node-sign conc) (keim~name conc))
						  (list "#0000ff"  (meth~node-sign conc) (keim~name conc)))
					      (list "#0000ff" (keim~name conc))))
			   (meth~conclusions method)))
	   (decl-cnt  (mapcar #'(lambda (dec) (list
					       (keim~name dec)
					       (mapcar #'keim~name (pdsn~hyps dec))
					       (node~formula dec)
					       (if (node~justification dec)
						   (cons 
						    (ohlp=parse-html-string (string (keim~name (just~method (node~justification dec)))))
						    (if (listp (just~premises (node~justification dec)))
							(mapcan #'(lambda (line) (when line (list (keim~name line))))
								(just~premises (node~justification dec)))
						      (just~premises (node~justification dec))))
						 "")))
			      (meth~declarative-content method)))
	   (out-action (mapcar #'(lambda (act) (list (mapcar #'(lambda (ac) (list
									     (keim~name ac)
									     (mapcar #'keim~name (meth=action-support-nodes ac))))
							     (meth=action-pattern-actions act))
						     (mapcar #'keim~name 
							     (meth=action-pattern-supported-nodes act))))
							      (meth~outline-actions method)))
	   (out-comp   (meth~outline-computations method))
	   (param      (meth~parameters method))      
	   (appl-cond  (ohlp=applcond2list (meth~application-condition method))))      
      (with-output-to-string (str)
       (ohlp=write-subtitle str "Method")
       (format str "<H3>It is defined in the theory: <a href=../theories/~(~A~).lml>~:(~A~)</A></H3><P>~%" theory theory)
;;every progn/when prints a row of the table 
       (format str "<TABLE BORDER>~%")
       (when  prems
	      (format str "<TR>~%")
	      (format str "<TD>Premises:</TD>")
	      (format str "<TD>") (dolist (prem prems) (format str "<FONT COLOR=\"~A\">~A</FONT> " (first prem)(rest prem)))
					               (format str "</TD>~%")
	      (format str "</TR>~%")) 
       (when  concs
	      (format str "<TR>~%")
	      (format str "<TD>Conclusions:</TD>")
	      (format str "<TD>") (dolist (conc concs) (format str "<FONT COLOR=\"~A\">~A</FONT> " (first conc)(rest conc)))
	                                               (format str "</TD>~%")
	      (format str "</TR>~%"))
       (when  param
	      (format str "<TR>~%")
	      (format str "<TD>Parameters:</TD>")
	      (format str "<TD>") (dolist (para param) (format str "~A " para))
	                                               (format str "</TD>~%")
	      (format str "</TR>~%"))
       (when  appl-cond
	      (format str "<TR>~%")
	      (format str "<TD>Application-Condition:</TD>")
	      (format str "<TD>")
	      (format str "~A" (ohlp=list2html-tabs appl-cond))
              (format str "</TD>~%")
	      (format str "</TR>~%"))
       (progn (format str "<TR>~%")
	      (format str "<TD>Declarative Content:</TD>")
	      (format str "<TD>") (format str "<TABLE>~%")
	                          (dolist (dec decl-cnt)
				    (format str "<TR> <TD><FONT COLOR=\"~A\">(~A)</FONT></TD>
                                                      <TD>~A</TD> <TD>|-</TD> <TD>~A</TD>
                                                      <TD>~A</TD> </TR>~%"
					    (or (Car (mapcan #'(lambda (line) (when
										  (eq (first dec) (car (last line)))
										(list (car line))))
							     (union prems concs)))
						"#000000")
					    (first dec)
					    (cons "<FONT FACE=\"Symbol\"> &#68;</FONT>"  (second dec)) 
					    (ohlp~html-formula (third dec))
					    (fourth dec)))
				  (format str "</TABLE>~%")	  
	      (format str "</TD>~%")
	      (format str "</TR>~%"))        
       (when  out-action
	      (format str "<TR>~%")
	      (format str "<TD>Outline-Action:</TD>")
	      (format str "<TD>")
	                (do* ((actions out-action (rest actions))
			      (br "" "<BR>")
			      (action (car actions) (car actions)))
			    ((null actions))
			  (format str "~A Apply " br)
			  (do* ((act (car action) (rest act))
				(and "" "and")
				(ac (car act) (car act)))
                              ((null act))
			    (format str " ~A ~A ~A " and (car ac) (cadr ac)))
                          (format str "to ")
                          (do* ((act (cadr action) (rest act))
                               (and "" "and")
			       (ac  (car act) (car act)))
                              ((null act))
                            (format str " ~A ~A" and  ac ))
			  (format str "."))
              (format str "</TD>~%")
	      (format str "</TR>~%"))
       (when  out-comp
	      (format str "<TR>~%")
	      (format str "<TD>Outline-Computation:</TD>")
	      (format str "<TD>")
	      (do* ((comps out-comp (rest comps))
		    (br "" "<BR>")
		    (comp (car comps) (car comps)))
		  ((null comps))
		(format str "~A ~A" br comp))
              (format str "</TD>~%")
	      (format str "</TR>~%"))
       (format str "</TABLE>~%")
;; table ends here
       (when docu
	  (format str "<P><H3>Description:</H3>")
	  (format str "~A~%" (car docu)))
       (when examples
	  (format str "<P><H3>The methods is used for the following problems:</H3>")
	  (format str "~A~%" (car examples)))
       (when author
	  (format str "<P><DIV ALIGN=right><FONT SIZE=-2>~A</FONT></DIV>" (car author))))))

(defgeneric ohlp=applcond2list (appl-cond)
  (:method ((appl-cond meth+condition))
	   (cons (keim~name appl-cond) (ohlp=applcond2list (meth=condition-args appl-cond))))
  (:method ((appl-cond meth+key-expression))
	   (cons (keim~name appl-cond)
		 (if (consp (ohlp=applcond2list (meth=key-expression-args appl-cond)))
			    (ohlp=applcond2list (meth=key-expression-args appl-cond))
						(list  (ohlp=applcond2list (meth=key-expression-args appl-cond))))))
  (:method ((appl-cond meth+funcall))
	   (cons (keim~name appl-cond) (ohlp=applcond2list (meth=funcall-args appl-cond))))
  (:method ((appl-cond cons))
	   (mapcar #'(lambda (cond) (ohlp=applcond2list cond)) appl-cond)) 
  (:method ((appl-cond keim+name))
	   (ohlp=applcond2list (keim~name appl-cond)))
  (:method ((appl-cond term+syn-sorted-term))
	   (ohlp=applcond2list (keim~name appl-cond)))
  (:method ((appl-cond symbol))
	   appl-cond)
  (:method ((appl-content T))))
	
(defun ohlp=list2html-tabs (list)
       (cond  ((null list) list)
	      ((and (consp list) (>= (length list) 2))
	       (concatenate 'string "<TABLE CELLSPACING=0 CELLPADDING=0>~%"
			    (do* ((restlist (rest list) (rest restlist))
				  (tableentry (concatenate 'string "<TR VALIGN=BASELINE><TD>" (string (car list))
							   "</TD><TD>" (ohlp=list2html-tabs (second list))
							   "</TD></TR>~%")
					      (concatenate 'string tableentry
							   "<TR VALIGN=TOP><TD></TD><TD>" (ohlp=list2html-tabs (car restlist)) "</TD></TR>~%")))
				((null (rest restlist)) tableentry))
			    "</TABLE>~%"))
	      ((consp list) (concatenate 'string "("(ohlp=list2html-tabs (car list)) (ohlp=list2html-tabs (rest list))")"))
	      (T (string list))))

